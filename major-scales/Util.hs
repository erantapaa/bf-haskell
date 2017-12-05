
module Util where

import BF0
import BFUtil

-- multiply a by 10 using b as a tmp
mult10' a b = do
  clear b
  dotimes' a $ incr_by b 10

-- read a decimal number into a; ch needs to be populated
readNum a ch = do
  let a' = second_cell a
  clear a
  getch ch
  while ch $ do
    decr_by ch 48
    mult10' a a'
    dotimes' ch (incr a')
    dotimes' a' (incr a)
    getch ch

