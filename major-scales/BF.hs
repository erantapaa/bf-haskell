{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module BF where

import Data.DList hiding (replicate)
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Debug.Trace
import Control.Monad
import Data.List.Split (chunksOf)

data Instr = Move Int | Inc Int | Read | Print | Open | Close | PrByte
              | Debug String
  deriving (Read, Show, Eq)

data BFState = BFState { sp :: !Int, maxsp :: !Int }
  deriving (Read, Show) 

initialState = BFState 0 0

type BF = StateT BFState (Writer (DList Instr))

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

getch x = do move x; readchar; moveneg x
putch x = do move x; printch; moveneg x

alloc body = do
  st <- get
  let t = sp st
  put st{ maxsp = max (maxsp st) (t+1), sp = t+1 }
  r <- body (R t)
  st <- get
  put st{ sp = t }
  return r

allocPair body = do
  alloc $ \x -> do
  alloc $ \_ -> do
    let p = Pair (offset x)
    body p

-- a peephole optimizer
simplify :: [Instr] -> [Instr]
simplify [] = []
simplify (Move 0 : xs) = simplify xs
simplify (Move a : Move b : xs) = simplify (Move (a+b) : xs)
simplify (Inc 0 : xs) = simplify xs
simplify (Inc a : Inc b : xs) = simplify (Inc (a+b) : xs)
simplify (x : xs) = x : simplify xs

compile' :: BF a -> [Instr]
compile' prog = simplify $ toList $ execWriter (execStateT prog initialState)

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

compile prog = 
  let ((_, st), ops) = runWriter (runStateT prog initialState)
  in toOpcodes (simplify (toList ops))

comp1 prog =
  let ((_, st), ops) = runWriter (runStateT prog initialState)
  in toList ops

fmt40 x = unlines (chunksOf 40 x) 

test4 = do
  allocPair $ \px -> do
  allocPair $ \pz -> do
  alloc     $ \ch -> do
    incr px
    ifPairZeroElse px pz
      ( do incr_by ch 48)
      ( do incr_by ch 49)
    putch ch

-- test1 = compile $ getch 3 >> getch 4

pass = return ()

-- execute body once if x is not zero
ifNotZero' x body = dotimes x $ body >> clear x

-- execute body once if x is zero
ifZero' x body = do
  alloc $ \t -> do
  clear t
  incr t
  ifNotZero' x (decr t)
  dotimes' t body

incr_by x a = at x (inc a)

assign a x = do clear a; incr_by a x

-- if px is non-zero execute thenClause, else execute elseClause
-- pzero is a pair of 0s
-- px+1 must be non-zero if px is zero
ifPairZeroElse' px pzero thenClause elseClause = do
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
ifPairZeroElse px pzero thenClause elseClause = do
  assign (second_cell px) 1
  ifPairZeroElse' px pzero thenClause elseClause

