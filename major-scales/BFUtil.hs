module BFUtil where

import Control.Monad
import Data.List.Split
import BF0
import System.IO

data Frame = Frame { fr_size :: Int }
  deriving (Read, Show)

advance frame = movep (fr_size frame)
backup frame  = movep (-(fr_size frame))
next frame x  = trans (fr_size frame) x
prev frame x  = trans (-(fr_size frame)) x

clearPair p = do
  clear p
  clear (second_cell p)

allZero [] thenClause = thenClause
allZero (p:ps) thenClause =
  ifPairZero p (allZero ps thenClause)

-- increment a chain of pairs representing a multi-byte number
-- least significant pair occurs first in the list
incrPairs [] = return ()
incrPairs (p:ps) = do
  incr p
  ifPairZero p (incrPairs ps)

-- decrement a chain of pairs representing a multi-byte number
-- least significant pair occurs first in the list
decrPairs [] = return ()
decrPairs (p:ps) = do
  ifPairZero p (decrPairs ps )
  decr p

-- divide x by pr+pr'; quotient in q, remainder in pr'
divide x pr q = do
  let pr' = second_cell pr
  dotimes' pr' $ incr pr
  -- pr' = 0
  while x $ do
    decr x
    incr pr'
    decr pr
    ifPairZeroElse' pr
      (do incr q; dotimes' pr' (incr pr))
      pass

-- set r to 1 if px >= c
isGE c px result = do
  alloc $ \t -> do
  alloc $ \s -> do
  assign result 1
  assign t c
  while t $ do
    ifPairZeroElse px 
      (do clear result; clear t)
      pass
    decr px
    decr t
    incr s
  dotimes' s (incr px)

printNibble px = do
  alloc $ \r -> do
  isGE 10 px r
  incr_by px 48
  dotimes' r $ incr_by px 7
  putch px

printHexByte px = do
  allocPair $ \pq -> do
  allocPair $ \pr -> do
  let pr' = second_cell pr
  assign pr 16
  clear pr'
  clear pq
  divide px pr pq
  printNibble pq
  copy'' pr' pq
  printNibble pq

writeFoo program = do
  let out = unlines (chunksOf 60 (compile program))
      path = "interp/foo.bf"
  writeFile path out
  putStrLn $ show(length out) ++ " bytes written to " ++ path
-- divide the 4-byte number x0..x3 by 85
-- quotient returned in q0..q3, remainder in pr'

u32_div85 x0 x1 x2 x3 q0 q1 q2 q3 q r0 r1 pr = do
  let pr' = second_cell pr
  let three x = do { x; x; x }

  forM_ [r0, r1, q0, q1, q2, q3] clear
  clear pr'
  assign pr 85

  divide x3 pr q3 
  dotimes' pr' $ do
    three (incr q2)
    three (incr q1)
    three (incr q0)
    incr r0
    incr pr

  divide x2 pr q 
  dotimes' q $ incrPairs [q2, q3] 
  dotimes' pr' $ do
    three $ incrPairs [q1, q2, q3] 
    three $ incrPairs [q0, q1, q2, q3]
    incr r0
    incr pr

  divide x1 pr q
  dotimes' q $ incrPairs [q1, q2, q3]
  dotimes' pr' $ do
    three $ incrPairs [q0, q1, q2, q3]
    incr r0
    incr pr

  divide x0 pr q
  dotimes' q $ incrPairs [q0, q1, q2, q3]
  dotimes' pr' $ do incrPairs [r0, r1]; incr pr

  divide r1 pr q
  dotimes' q $ incrPairs [q0, q1, q2, q3]
  dotimes' pr' $ do incr r0; incr pr

  divide r0 pr q
  dotimes' q $ incrPairs [q0, q1, q2, q3]

-- read4 -- read four bytes and set notDone if any are non-zero
read4 x0 x1 x2 x3 notDone = do
  getch x3
  getch x2
  getch x1
  getch x0
  assign notDone 1
  allZero [x0, x1, x2, x3] $ clear notDone

-- copy p to x using the second cell of p as a temporary
copyp p x = copy' p x (second_cell p)

