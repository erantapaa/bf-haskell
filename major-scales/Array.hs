module Array where

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

arrayDatum x = R (offset x + 2)
arrayIndex x = R (offset x + 1)

arraySetup x width body = do
  let skip  = R 0
  let i     = R 1
      advance = movep width
      backup  = movep (-width)
  -- set up skip links
  at x $ do
    while i $ do
      advance; while skip advance
      incr skip
      while skip backup
      decr i
    -- back at frame 0
    let forward = do { advance; while skip advance }
        rewind  = do { backup; while skip backup }
    -- execute body
    body forward rewind
    -- back at frame 0, clear skip cells
    forward
    backup
    while skip $ do { clear skip; backup }
    -- back at frame 0

-- set array element to datum
arraySet x width field = do
  let datum = R 2
  arraySetup x width $ \forward rewind -> do
    forward
    clear field
    rewind
    while datum $ do
      forward
      incr field
      rewind
      decr datum

-- destructive fetch array element into datum
arrayFetch' x width field = do
  let datum = R 2
  at x $ clear datum
  arraySetup x width $ \forward rewind -> do
    forward
    -- debug field "array fetch"
    while field $ do
      decr field
      rewind
      incr datum
      forward
    rewind

-- non destructive fetch
arrayFetch x width field tmp = do
  let datum = R 2
  at x $ clear datum
  arraySetup x width $ \forward rewind -> do
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
arrayIncr x width field = do
  let datum = R 2
  arraySetup x width $ \forward rewind -> do
    while datum $ do
      forward
      incr field
      rewind
      decr datum

