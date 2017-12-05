{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts #-}

import BF
import U32
import Data.Char
import BFUtil
import Control.Monad

u32_printHex ux pzero = do
  let U32 (x0,x1,x2,x3) = ux
  alloc $ \t -> do
  assign t 32
  printHexByte x0 pzero
  putch t
  printHexByte x1 pzero
  putch t
  printHexByte x2 pzero
  putch t
  printHexByte x3 pzero
  putch t

printCR = do
  alloc $ \t -> do
    assign t 10
    putch t

printSPC = do
  alloc $ \sp -> do
    assign sp 32
    putch sp

decoder = do
  allocPair $ \pzero -> do
  alloc $ \notDone -> do
  allocPair $ \x0 -> do
  allocPair $ \x1 -> do
  allocPair $ \x2 -> do
  allocPair $ \x3 -> do
  allocPair $ \x4 -> do
  u32_alloc $ \us -> do
  alloc $ \t -> do
  let U32 (s0,s1,s2,s3) = us

  let readx = do
      forM_ [x4,x3,x2,x1,x0] getch
      assign notDone 1
      allZero [x0,x1,x2,x3,x4] pzero (clear notDone)

  let decode_bytes = do
      forM_ [s0,s1,s2,s3] clear
      -- load x0..x4; x0 is least significant character

      assign t 33
      dotimes' t $ do
        forM_ [x0,x1,x2,x3,x4] decr

      -- add x0
      dotimes' x0 $ incr s0

      -- add 85*x1
      dotimes' x1 $ do
        assign t 85
        dotimes' t $ incrPairs [s0, s1, s2, s3] pzero

      -- add 85*85*x2,  85*85 = 256*28 + 57
      dotimes' x2 $ do
        assign t 28
        dotimes' t $ incrPairs [s1,s2,s3] pzero
        assign t 57
        dotimes' t $ incrPairs [s0,s1,s2,s3] pzero

      -- add 85*85*85*x3,  85^3 = 9, 94, 237
      dotimes' x3 $ do
        assign t 9
        dotimes' t $ incrPairs [s2,s3] pzero
        assign t 94
        dotimes' t $ incrPairs [s1,s2,s3] pzero
        assign t 237
        dotimes' t $ incrPairs [s0,s1,s2,s3] pzero

      -- add 85^4*x4,   85^4 = 3, 28, 132, 177
      dotimes' x4 $ do
        assign t 3
        dotimes' t $ incrPairs [s3] pzero
        assign t 28
        dotimes' t $ incrPairs [s2, s3] pzero
        assign t 132
        dotimes' t $ incrPairs [s1, s2, s3] pzero
        assign t 177
        dotimes' t $ incrPairs [s0, s1, s2, s3] pzero

      -- forM_ [s3,s2,s1,s0] (\x -> printHexByte x pzero >> printSPC)
      -- printCR
      forM_ [s3, s2, s1, s0] putch

  let prog = do
      readx
      while notDone $ do
        decode_bytes
        readx

  prog

base m n
  | n < m    = [n]
  | otherwise = let (q,r) = divMod n m in base m q ++ [r]

encoder = do
  allocPair $ \pzero -> do
  alloc     $ \notDone -> do
  u32_alloc $ \ux -> do
  alloc     $ \ndigits -> do
  alloc     $ \hasDigit -> do
  u32_alloc $ \uq -> do
  allocPair $ \pr -> do

  let frameSize = 30
      advance = movep frameSize
      backup = movep (-frameSize)
      next x = trans frameSize x
      prev x = trans (-frameSize) x

  let read_ux = u32_read ux notDone pzero

  let printNL = do
      alloc $ \t -> do
      assign t 10
      putch t

  -- ux contains number to print
  let printBase85 = do
        let pr' = second pr
        assign ndigits 5
        clear (prev hasDigit)
        while ndigits $ do
          -- debug ndigits "ndigits"
          assign hasDigit 1
          u32_div85 ux uq pr pzero
          u32_copy' uq (next ux)
          copy ndigits (next ndigits)
          advance
          decr ndigits
        backup
        while hasDigit $ do
          copy'' pr' pr
          -- printHexByte pr pzero; printSPC  -- XXX change this
          incr_by pr 33; putch pr
          backup
        advance
  
  let prog = do
        advance
        read_ux
        while notDone $ do
          printBase85
          read_ux

  let test1 = do
        read_ux
        while notDone $ do
          u32_printHex ux pzero
          printNL
          read_ux

  {-
  let test2 = do
        u32_clear ux
        assign (u3 ux) 12
        u32_print ux

  let test3 = do
        u32_clear ux
        assign (u0 ux) 87
        u32_print ux
        u32_div85 ux pr uq pzero
        u32_print ux
        u32_print uq
        at (R (offset1 pr)) prbyte

  let test4 = do
        u32_clear ux
        assign (u3 ux) 87
        u32_print ux
        u32_div85 ux pr uq pzero
        u32_print ux
        u32_print uq
        at (R (offset1 pr)) prbyte
-}

  prog

main = undefined

