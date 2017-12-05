
module BFUtil where

import BF
import System.IO
import Data.List.Split

writeFoo program = do
  let out = unlines $ chunksOf 60 (compile program)
      path = "foo.bf"
  writeFile path out
  putStrLn $ show(length out) ++ " bytes written to " ++ path

