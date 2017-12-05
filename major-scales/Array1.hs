{-# LANGUAGE FlexibleContexts #-}

module Array1 where

import BF0

{-
 - frame 0 structure:
 -   offset 0: skip
 -   offset 1: index
 -   offset 2: datum
 - 
 - @{x+(n+1)*width+0}  = skip cell
 - @{x+(n+1)*width+field} = field being manipulated
 -
 -}

data BFArray = BFArray { astart_ :: R, awidth_ :: Int }

allocArray n width body
  | width < 3  = error ("invalid array width: " ++ show width)
  | n < 1      = error ("invalid array size: " ++ show n)
  | otherwise  = do let size = (n+1)*width
                    nalloc size $ \r -> do
                      let arr = BFArray { astart_ = r, awidth_ = width }
                      body arr

-- make an array at a specific memory location
mkArray width r
  | width < 3 = error ("invalid array width: " ++ show width)
  | otherwise = let arr = BFArray { astart_ = r, awidth_ = width }
                in arr

instance Register BFArray where
  offset a = offset (astart_ a)

arrayAdvance arr = movep (awidth_ arr)
arrayBackup arr = movep (-(awidth_ arr))

offset_skip = R 0
offset_index = R 1
offset_datum = R 2

arrayIndex arr = R (offset arr + 1)
arrayDatum arr = R (offset arr + 2)

arraySetup arr body = do
  let skip  = offset_skip
  let i     = offset_index
      advance = arrayAdvance arr
      backup  = arrayBackup arr
  let forward = do { advance; while skip advance }
      rewind  = do { backup; while skip backup }
  -- set up skip links
  at arr $ do
    while i $ do
      forward
      incr skip
      while skip backup
      decr i
    -- back at frame 0
    -- execute body
    body forward rewind
    -- back at frame 0, clear skip cells
    forward
    backup
    while skip $ do { clear skip; backup }
    -- back at frame 0

-- set array element to datum
arraySet arr field = do
  let datum = offset_datum
  arraySetup arr $ \forward rewind -> do
    forward
    clear field
    rewind
    while datum $ do
      forward
      incr field
      rewind
      decr datum

-- destructive get array element into datum
arrayGet' arr field = do
  let datum = offset_datum
  at arr $ clear datum
  arraySetup arr $ \forward rewind -> do
    forward
    while field $ do
      decr field
      rewind
      incr datum
      forward
    rewind

-- non destructive get; requires a tmp field
arrayGet arr field tmp = do
  let datum = offset_datum
  at arr $ clear datum
  arraySetup arr $ \forward rewind -> do
    forward
    clear tmp
    while field $ do
      decr field
      incr tmp
      rewind
      incr datum
      forward
    dotimes' tmp (incr field)
    rewind

-- increment array element by datum
arrayIncr arr field = do
  let datum = offset_datum
  arraySetup arr $ \forward rewind -> do
    while datum $ do
      forward
      incr field
      rewind
      decr datum

