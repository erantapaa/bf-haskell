
-- A solution to Daily Programm Challenge #343 Easy
-- https://www.reddit.com/r/dailyprogrammer/comments/7hhyin/20171204_challenge_343_easy_major_scales/

import BF0
import BFUtil
import Jump
import Data.Char

transfer pa = do
  clear pa
  dotimes' (second_cell pa) (incr pa)

program = do
  allocPair $ \ch -> do
  allocPair $ \error -> do
  allocPair $ \semitone -> do
  allocPair $ \key -> do
  allocPair $ \sharp -> do
  allocPair $ \x -> do
  allocPair $ \pr -> do
  allocPair $ \q -> do
  alloc     $ \ch_sharp -> do

  assign ch_sharp (ord '#')

  getch ch

  clear key
  clear sharp

  readKey ch key error

  ifPairZero error $ readSharp ch sharp error

  skipSpaces ch
  ifPairZero error $ readTone ch semitone error

  ifPairZeroElse error
    (do keyToSemi key x
        dotimes' sharp (incr x)
        dotimes' semitone (incr x)
        assign pr 12
        clear (second_cell pr)
        divide x pr q
        transfer pr
        semiToNote pr key sharp
        printNote key sharp ch_sharp)
    (printError)
  alloc $ \ch_nl -> do
    assign ch_nl (ord '\n')
    putch ch_nl

printError = do
  alloc $ \ch_e -> do
  alloc $ \ch_r -> do
  alloc $ \ch_o -> do
  alloc $ \x -> do
  clear ch_e
  clear ch_r
  clear ch_o
  assign x (ord 'A')
  dotimes' x $ do incr ch_e; incr ch_r; incr ch_o
  assign x 4
  -- e is A+4
  -- o is A+14
  -- r is A+17
  dotimes' x $ do
    incr ch_e
    incr ch_o; incr ch_o; incr ch_o; incr ch_o
    incr ch_r; incr ch_r; incr ch_r; incr ch_r
  decr ch_o
  decr ch_o
  incr ch_r
  putch ch_e; putch ch_r; putch ch_r; putch ch_o; putch ch_r

printNote key sharp ch_sharp = do
  incr_by key (ord 'A')
  putch key 
  dotimes' sharp $ putch ch_sharp

-- convert a semitone to a key and sharp
semiToNote pa key sharp =
  jumpTable pa [key,sharp]
    [ [ 0, 0 ]  --  0 A
    , [ 0, 1 ]  --  1 A#
    , [ 1, 0 ]  --  2 B
    , [ 2, 0 ]  --  3 C
    , [ 2, 1 ]  --  4 C#
    , [ 3, 0 ]  --  5 D
    , [ 3, 1 ]  --  6 D#
    , [ 4, 0 ]  --  7 E
    , [ 5, 0 ]  --  8 F
    , [ 5, 1 ]  --  9 F#
    , [ 6, 0 ]  -- 10 G
    , [ 6, 1 ]  -- 11 G#
    ]

-- skip spaces
skipSpaces ch = do
  alloc $ \notDone -> do
  assign notDone 1
  while notDone $ do
    decr_by ch 32
    ifPairZeroElse ch
      (getch ch)
      (do incr_by ch 32; clear notDone)

-- convert a key (0..6) to a semitone
keyToSemi pa key = do
  jumpTable1 pa key [0, 2, 3, 5, 6, 8, 10]

-- set r if ch is not 'A'..'G'
isNotKey ch r = do
    decr_by ch (ord 'A')
    isGE (ord 'G' - ord 'A' + 1) ch r

-- read a key ('A'..'G')
readKey ch key error = do
  -- ch is already populated
  -- skipSpaces ch
  -- expect a letter
  allocPair $ \r -> do
  isNotKey ch r
  ifPairZeroElse r
    (do copy'' ch key; getch ch)
    (do incr_by ch (ord 'A'); incr error)

readSharp ch sharp error = do
  clear sharp
  decr_by ch (ord '#')
  ifPairZeroElse ch
    (do incr sharp; getch ch)
    (do incr_by ch (ord '#'))

readKeySharp ch key sharp error = do
  clear key
  clear sharp
  readKey ch key error
  ifPairZero error $ readSharp ch sharp error

-- sets r if ch is not a lower or uppercase letter
-- ch is decremented by 'A' or 'a' respectively
isNotLetter ch r = do
  decr_by ch (ord 'A')
  isGE (ord 'Z' - ord 'A' + 1) ch r
  ifPairZeroElse r
    pass
    (do decr_by ch (ord 'a' - ord 'A')
        isGE (ord 'z' - ord 'a' + 1) ch r)

-- read two characters and convert it into a semitone
readTone ch semitone error = do
  alloc     $ \pa -> do
  allocPair $ \r -> do
  allocPair $ \pr -> do
  let ifLetter body = do
        isNotLetter ch r
        ifPairZeroElse r body (incr error)
  ifLetter $ do
  copy'' ch pa
  getch ch
  ifLetter $ do
  dotimes' ch (incr pa)
  assign pr 13
  clear (second_cell pr)
  alloc $ \q -> do
    divide pa pr q
  clear pr
  dotimes' (second_cell pr) (incr pr)
  jumpTable pr [semitone,error]
         [ [  0, 1]   -- hash 0
         , [ 11, 0]   -- hash 1
         , [ 11, 1]   -- hash 2
         , [ 11, 1]   -- hash 3
         , [  0, 0]   -- hash 4
         , [  5, 0]   -- hash 5
         , [  7, 0]   -- hash 6
         , [  3, 0]   -- hash 7
         , [  2, 0]   -- hash 8
         , [  2, 1]   -- hash 9
         , [  2, 1]   -- hash 10
         , [  9, 0]   -- hash 11
         , [  9, 1]   -- hash 12
         ]

main = undefined

