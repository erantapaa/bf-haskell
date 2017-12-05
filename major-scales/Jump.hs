module Jump where

import BF0
import Control.Monad

-- adjust a variable from value a to value b
adjust v a b
  | a == b    = return ()
  | a > b     = incr_by v (b-a)
  | otherwise = decr_by v (a-b)

adjustVars [] _ _ =  return ()
adjustVars (v:vs) (a:as) (b:bs) = do
  adjust v a b
  adjustVars vs as bs

jumpTable pa vars values = do
  let zeros = map (const 0) vars
      pairs = zip (zeros:values) values
      decrTest body = do { decr pa; ifPairZeroElse pa pass body }
      go [] = return ()
      go ((as,bs):rest) = do adjustVars vars as bs; decrTest (go rest)
  forM_ vars clear
  incr pa
  go pairs

-- special case jump table for a single output result 
jumpTable1 pa var values =
  jumpTable pa [var] [ [v] | v <- values ]

