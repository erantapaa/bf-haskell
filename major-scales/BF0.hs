{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module BF0 where

import Data.DList hiding (replicate)
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.Reader
import Debug.Trace
import Control.Monad
import Data.List.Split (chunksOf)

data Instr = Move Int | Inc Int | Read | Print | Open | Close | PrByte
              | Debug String
  deriving (Read, Show, Eq)

data BFState = BFState { sp :: !Int, maxsp :: !Int }
  deriving (Read, Show) 

initialState = BFState 0 0

type BF = ReaderT Pair (StateT BFState (Writer (DList Instr)))

data R = R Int
  deriving (Read, Show)

data Pair = Pair { unPair :: Int }
  deriving (Read, Show)

class Register a where
  offset :: a -> Int

class IsPair a where
   offset1 :: a -> Int

class Translatable a where
  trans :: Int -> a -> a

instance Register R where
  offset (R x)    = x

instance Translatable R where
  trans a (R x) = R (x+a)

instance Register Pair where
  offset (Pair x) = x

instance IsPair Pair where
  offset1 (Pair x) = x+1

instance Translatable Pair where
  trans a (Pair x) = Pair (x+a)

first_cell x = R (offset x)
second_cell x = R (offset1 x)

movep :: Int -> BF ()
movep dx = tell (singleton (Move dx))

move :: Register a => a -> BF ()
move x   = movep (offset x)
moveneg x = movep (- (offset x))

inc a       = tell (singleton (Inc a))
readchar    = tell (singleton Read)
printch     = tell (singleton Print)
open        = tell (singleton Open)
close       = tell (singleton Close)
dec1        = inc (-1)
prbyte      = tell (singleton PrByte)
debug x msg = at x $ tell (singleton (Debug msg))

-- perform an action at offset x
at x body = do move x; body; moveneg x

clear x = at x (do open; dec1; close)

incr x = at x (inc 1)
decr x = at x (inc (-1))

-- execute body while x is non-zero
while x body = do
  move x
  open
  moveneg x
  body
  move x
  close
  moveneg x

-- destructive dotimes
dotimes' x body = 
  while x $ do { body; decr x }

-- non-destructive dotimes
dotimes x body = do
  alloc $ \t -> do
  clear t
  dotimes' x $ do
    incr t
    body
  dotimes' t $ incr x

-- non-destructive copy x to y using temporary t
copy' x y t = do
  clear y
  clear t
  dotimes' x $ do incr t; incr y
  dotimes' t $ incr x

-- destructive copy x to y
copy'' x y = do
  clear y
  dotimes' x (incr y)

-- non-destructive copy using a temporary
copy x y = do
  alloc $ \t -> copy' x y t

getch x = at x readchar
putch x = at x printch

-- allocate n cells of storage
nalloc n body = do
  st <- get
  let t = sp st
  put st { maxsp = max (maxsp st) (t+n), sp = t+n }
  r <- body (R t)
  modify $ \st -> st { sp = t }
  return r

alloc body = nalloc 1 body
allocPair body = nalloc 2 $ \x -> body (Pair (offset x))

-- a peephole optimizer
simplify :: [Instr] -> [Instr]
simplify [] = []
simplify (Move 0 : xs) = simplify xs
simplify (Move a : Move b : xs) = simplify (Move (a+b) : xs)
simplify (Inc 0 : xs) = simplify xs
simplify (Inc a : Inc b : xs) = simplify (Inc (a+b) : xs)
simplify (x : xs) = x : simplify xs

toOpcodes :: [Instr] -> String
toOpcodes xs = concatMap go xs
  where go (Move x)
          | x > 0 = replicate x '>'
          | x < 0 = replicate (-x) '<'
          | otherwise = ""
        go (Inc x)
          | x > 0 = replicate x '+'
          | x < 0 = replicate (-x) '-'
          | otherwise = ""
        go Open = "["
        go Close = "]"
        go Read = ","
        go Print = "."
        go PrByte = "@"
        go (Debug x)
            | ok x = "!" ++ x ++ "!"
            | otherwise = error $ "bad message string: " ++ x
           where ok x = all (\c -> notElem c x) "![]<>+-.,"

compile prog = compile' (allocPair $ \pzero -> runReaderT prog pzero)

compile' prog = 
  let ((_, st), ops) = runWriter (runStateT prog initialState)
  in toOpcodes (simplify (toList ops))

pass = return ()

-- execute body once if x is not zero
ifCellNotZero' x body = dotimes x $ body >> clear x

-- execute body once if x is zero
ifCellZero' x body = do
  alloc $ \t -> do
  clear t
  incr t
  ifCellNotZero' x (decr t)
  dotimes' t body

-- increment cell x by a constant a; can accept negative increments
incr_by x a = at x (inc a)
decr_by x a = incr_by x (-a)

assign a x = do clear a; incr_by a x

-- if px is non-zero execute thenClause, else execute elseClause
-- pzero is a pair of 0s
-- px+1 must be non-zero if px is zero
ifPairZeroElse' px thenClause elseClause = do
  pzero <- ask
  move px
  open
  moveneg px
  elseClause
  move pzero
  close
  movep 1
  open
  movep (-(offset1 px))
  thenClause
  movep (offset1 pzero)
  close
  movep (-(offset1 pzero))

-- initializes px+1 to 1 before calling ifPairZeroElse
ifPairZeroElse px thenClause elseClause = do
  assign (second_cell px) 1
  ifPairZeroElse' px thenClause elseClause

ifPairZero px thenClause    = ifPairZeroElse px thenClause (return ())
ifPairNonZero px thenClause = ifPairZeroElse px (return ()) thenClause

