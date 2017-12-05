
module Signed where

import Control.Monad.Reader
import BF0
import BFUtil

-- increment (sa,pa) by x
inc' (sa,pa) x = do
  ifPairZeroElse sa
    -- a is positive
    (dotimes' x (incr pa))
    -- a is negative
    (dotimes' x $
      do decr pa
         ifPairZero pa
           (do clear sa; dotimes' x (incr pa)))

-- decrement (sa,pa) by x
dec' (sa,pa) x = do
  ifPairZeroElse sa
    -- a is positive
    (dotimes' x $
      do decr pa
         ifPairZero pa
           (do assign sa 1; dotimes' x (incr pa)))
    -- a is negative
    (dotimes' x $ do incr pa)

-- add a to b; clears pa
add' (sa,pa) (sb,pb) = do
  ifPairZeroElse sa
    -- count up
    (inc' (sb,pb) pa)
    (dec' (sb,pb) pa)

xor sa sb sc = do
  ifPairZeroElse sa
    (copy sb sc)
    (ifPairZeroElse sb (assign sc 1) (clear sc))

mul' (sa,pa) (sb,pb) (sc,pc) = do
  xor sa sb sc
  -- compute pc = pa*pb
  let pb' = second_cell pb
  clear pb'
  clear pc
  dotimes' pa $ do
    dotimes' pb $ do
      incr pb'
      incr pc
    dotimes' pb' (incr pb)

-- set r if ch is not a digit
-- ch contains digit value otherwise
isNotDigit ch r = do
  decr_by ch 48
  isGE 10 ch r

-- set b to 10*a
mult10' a b = do
  clear b
  dotimes' a $ incr_by b 10

readNum px ch = do
  clear px
  let px' = second_cell px
  alloc $ \notDone -> do
  assign notDone 1
  while notDone $ do
    allocPair $ \r -> do
      isNotDigit ch r
      ifPairZeroElse r
        (do mult10' px px'
            dotimes' ch (incr px')
            dotimes' px' (incr px)
            getch ch)
        (do incr_by ch 48; clear notDone)

readSign sx ch = do
  decr_by ch 45
  ifPairZeroElse ch
    (do assign sx 1; getch ch)
    (do clear sx; incr_by ch 45)

readSignedNum (sx,px) ch = do
  readSign sx ch
  readNum px ch

-- print a cell as a decimal number
printNum = do
  let pzero    = Pair 0
      hasDigit = Pair 2
      x        = Pair 4 
      pr       = Pair 6 
  let pr'      = second_cell pr
  let frameSize = 10
      next = trans frameSize
      advance = movep frameSize
      backup = movep (-frameSize)

  local (\_ -> pzero) $ do
    clearPair pzero
    clear hasDigit
    advance
    while x $ do
      assign hasDigit 1
      assign pr 10
      clear pr'
      divide x pr (next x)
      advance
    backup
    while hasDigit $ do
      incr_by pr' 48
      putch pr'
      backup

-- assumes ch is populated with current input char
readNums frame ch hasNum (sx,px) = do
  clear hasNum
  advance frame
  copy'' (prev frame ch) ch
  assign hasNum 1
  while hasNum $ do
    readSignedNum (sx,px) ch
    decr_by ch 32
    ifPairZeroElse ch
      (do advance frame; assign hasNum 1; getch ch)
      (do clear hasNum
          incr_by ch 32)
-- ends in frame of last number read

