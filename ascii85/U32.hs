{-# LANGUAGE FlexibleContexts #-}

module U32 where

import BF
import Control.Monad

data U32 = U32 (Pair,Pair,Pair,Pair)
  deriving (Read, Show)

instance Translatable U32 where
  trans x (U32 (a,b,c,d)) = U32 (trans x a, trans x b, trans x c, trans x d)

instance Register U32 where
  offset (U32 (a,_,_,_)) = offset a

u32_alloc body = do
  allocPair $ \p0 -> do
  allocPair $ \p1 -> do
  allocPair $ \p2 -> do
  allocPair $ \p3 -> do
  let u = U32 (p0,p1,p2,p3)
  body u

second x = R (offset1 x)

-- divide x by pr; pr' must be 0
-- q is incremented by quotient
-- remainder is in pr'

divide' x pr q pzero = do
  let pr' = R (offset1 pr)
  dotimes' pr' $ incr pr
  -- pr' = 0
  while x $ do
    decr x
    incr pr'
    decr pr
    ifPairZeroElse' pr pzero
      (do incr q; dotimes' pr' (incr pr))
      pass

-- destructive copy of a to b
copy'' a b = do
  clear b
  dotimes a (incr b)

-- destructive copy of ux to uy
u32_copy' ux uy = do
  let U32 (x0,x1,x2,x3) = ux
      U32 (y0,y1,y2,y3) = uy
  copy'' x0 y0
  copy'' x1 y1
  copy'' x2 y2
  copy'' x3 y3

-- set result to 1 if px >= c (a constant)
isGE c px result pzero = do
  alloc $ \t -> do
  alloc $ \s -> do
  assign result 1
  assign t c
  while t $ do
    ifPairZeroElse px pzero
      (do clear result; clear t)
      pass
    decr px
    decr t
    incr s
  dotimes' s (incr px)

printNibble px pzero = do
  alloc $ \r -> do
  isGE 10 px r pzero
  incr_by px 48
  dotimes' r $ incr_by px 7
  putch px

printHexByte px pzero = do
  allocPair $ \pq -> do
  allocPair $ \pr -> do
  let pr' = second pr
  assign pr 16
  clear pr'
  clear pq
  divide' px pr pq pzero
  printNibble pq pzero
  copy'' pr' pq
  printNibble pq pzero

-- increment a chain of pairs representing a multi-byte number
-- least significant pair occurs first in the list
incrPairs [] pzero = return ()
incrPairs (p:ps) pzero = do
  incr p
  ifPairZeroElse p pzero
    (incrPairs ps pzero)
    pass

-- decrement a chain of pairs representing a multi-byte number
-- least significant pair occurs first in the list
decrPairs [] pzero = return ()
decrPairs (p:ps) pzero = do
  ifPairZeroElse p pzero
    (do decrPairs ps pzero)
    pass
  decr p

allZero [] pzero thenClause = thenClause
allZero (p:ps) pzero thenClause =
  ifPairZeroElse p pzero (allZero ps pzero thenClause) (return ())

-- read 4 characters; set notDone if any are non-zero
u32_read ux notDone pzero = do
  let U32 (x0,x1,x2,x3) = ux
  getch x3
  getch x2
  getch x1
  getch x0
  assign notDone 1
  allZero [x0,x1,x2,x3] pzero $ clear notDone

three action = do action; action; action

-- divide ux by 85 leaving quotient in uq and remainder in pr'
u32_div85 ux uq pr pzero = do
  let U32 (x0,x1,x2,x3) = ux
      U32 (q0,q1,q2,q3) = uq
  alloc $ \q -> do
  allocPair $ \r0 -> do
  allocPair $ \r1 -> do
  u32_div85' x0 x1 x2 x3 q0 q1 q2 q3 q r0 r1 pr pzero

-- divide the 4-byte number x0..x3 by 85
-- quotient returned in q0..q3, remainder in pr'
u32_div85' x0 x1 x2 x3 q0 q1 q2 q3 q r0 r1 pr pzero = do
  let pr' = second pr
  forM_ [r0, r1, q0, q1, q2, q3] clear
  clear pr'
  assign pr 85

  divide' x3 pr q3 pzero
  dotimes' pr' $ do
    three (incr q2)
    three (incr q1)
    three (incr q0)
    incr r0
    incr pr

  divide' x2 pr q pzero
  dotimes' q $ incrPairs [q2, q3] pzero
  dotimes' pr' $ do
    three $ incrPairs [q1, q2, q3] pzero
    three $ incrPairs [q0, q1, q2, q3] pzero
    incr r0
    incr pr

  divide' x1 pr q pzero
  dotimes' q $ incrPairs [q1, q2, q3] pzero
  dotimes' pr' $ do
    three $ incrPairs [q0, q1, q2, q3] pzero
    incr r0
    incr pr

  divide' x0 pr q pzero
  dotimes' q $ incrPairs [q0, q1, q2, q3] pzero
  dotimes' pr' $ do incrPairs [r0, r1] pzero; incr pr

  divide' r1 pr q pzero
  dotimes' q $ incrPairs [q0, q1, q2, q3] pzero
  dotimes' pr' $ do incr r0; incr pr

  divide' r0 pr q pzero
  dotimes' q $ incrPairs [q0, q1, q2, q3] pzero

