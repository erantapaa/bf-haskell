{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module BF where

import Data.DList hiding (replicate)
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Debug.Trace
import Control.Monad
import Data.List.Split (chunksOf)

data Instr = Move Int | Inc Int | Read | Print | Open | Close
  deriving (Read, Show, Eq)

data BFState = BFState { sp :: !Int, maxsp :: !Int }
  deriving (Read, Show) 

initialState = BFState 0 0

type BF a = StateT BFState (Writer (DList Instr)) a

class Register a where
  offset :: a -> Int

class IsPair a where
   offset1 :: a -> Int

data R = R Int
  deriving (Read, Show)

instance Register R where
  offset (R x)    = x

data Pair = Pair { unPair :: Int }
  deriving (Read, Show)

instance Register Pair where
  offset (Pair x) = x

instance IsPair Pair where
  offset1 (Pair x) = x+1

movep :: Int -> BF ()
movep dx = tell (singleton (Move dx))

move :: Register a => a -> BF ()
move x   = movep (offset x)
moveneg x = movep (- (offset x))

inc a    = tell (singleton (Inc a))
readchar = tell (singleton Read)
printch  = tell (singleton Print)
open     = tell (singleton Open)
close    = tell (singleton Close)
dec1     = inc (-1)

-- perform an action at offset x
at x body = do move x; body; moveneg x

clear x = do at x (do open; dec1; close)

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

compile prog = 
  let ((_, st), ops) = runWriter (runStateT prog initialState)
  in toOpcodes (simplify (toList ops))

comp1 prog =
  let ((_, st), ops) = runWriter (runStateT prog initialState)
  in toList ops

-- test1 = compile $ getch 3 >> getch 4

-- non-destructive copy
copy x y = do
  alloc $ \t -> copy' x y t

prog1 = do
  alloc $ \x -> do
  alloc $ \y -> do
  incr x
  copy x y

halt = undefined

ifNotZero' x body = dotimes x $ body >> clear x

ifZero' x body = do
  alloc $ \t -> do
  clear t
  incr t
  ifNotZero' x (decr t)
  dotimes' t body

incr_by x a = at x (inc a)

print_None = do
  -- N 78, o 111, n 110, 101
  alloc $ \a -> do
  alloc $ \b -> do
  clear a
  clear b
  incr_by b 13
  dotimes' b $ incr_by a 6
  putch a                   -- 'N'
  incr_by b 4
  dotimes' b $ incr_by a 8
  incr a
  putch a                   -- 'o'
  decr a
  putch a                   -- 'n'
  incr_by a (-9)
  putch a                   -- 'e'

program frameSize = do
  let prev x  = R (offset x-frameSize)
  let advance = movep frameSize
  let backup  = movep (-frameSize)

  alloc $ \f_ch -> do     -- the character
    alloc $ \f_id -> do   -- index associated with the character

    let rewind = do backup; while f_id backup

    advance
    getch f_ch
    while f_ch $ do
      copy (prev f_id) f_id
      incr f_id
      advance
      getch f_ch
    rewind
    advance

    alloc $ \f_x  -> do   -- char we are looking for
    alloc $ \f_z  -> do   -- used to compare f_ch and f_x

    while f_ch $ do
      copy f_ch f_x       -- initialize f_x with current character

      advance
      while f_ch $ do
        copy (prev f_x) f_x
        copy f_x f_z
        dotimes f_ch (decr f_z)
        ifZero' f_z $ do
          -- found a match
          putch f_ch
          halt
        advance

      rewind
      advance
      clear f_ch
      advance
  print_None

assign a x = do clear a; incr_by a x

