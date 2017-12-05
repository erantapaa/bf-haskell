import BF

frameSize = 10
advance = movep frameSize
backup  = movep (-frameSize)
next x = R (offset x+frameSize)
prev x = R (offset x-frameSize)

-- fields:
f_char     = Pair 0
f_cmpchar  = Pair 2
f_skipup   = R 4
f_skipdn   = R 5
f_zero     = Pair 6
f_notdup   = R 8
f_length   = R 9

-- read all of the input
readInput = do
  advance
  getch f_char
  while f_char $ advance >> getch f_char

-- rewind to beginning of string
rewind = do while f_char backup; advance

set1 x = do
  while x $ decr x
  incr x

main = undefined

-- x and x+1 are complementary cells 
-- zero and zero+1 are zero
ifZeroElse' px pzero thenClause elseClause = do
  move px
  open
  moveneg px
  thenClause
  move pzero
  close
  movep 1
  open
  movep (-(offset1 px))
  elseClause
  movep (offset1 pzero)
  close
  movep (-(offset1 pzero))

pass = return ()

ifNotZero px thenClause elseClause = do
  set1 px
  ifZeroElse' px f_zero
    elseClause
    thenClause

ifZero px thenClause elseClause = ifNotZero px elseClause thenClause

-- set y = 0 if x != 0
zero x y = do
  alloc $ \t -> do
    copy x t
    dotimes t $ clear y

-- determine which characters are duplicates
-- sets f_dup for those characters which are dups
-- 
duploop1 = do
  let advance_to_end  = while f_skipup advance
  let rewind_to_start = while f_skipdn backup

  -- f_skipup == 1 means advance to get to end
  -- f_skipdn == 1 means 
  rewind
  while f_char $ do set1 f_skipdn; set1 f_notdup
  rewind

  -- while start char is not zero
  while f_char $ do
    clear f_skipdn

    set1 f_skipup
    advance
    set1 f_skipdn
    clear f_skipup
    -- while end char is not zero
    while f_char $ do
      copy f_char f_cmpchar
      while f_skipdn $ do copy f_cmpchar (prev f_cmpchar); backup
      dotimes f_char (decr f_cmpchar)

      ifZero f_cmpchar
        (do clear f_notdup
            advance_to_end
            clear f_notdup)
        pass
      advance_to_end
      set1 f_skipup
      advance
      set1 f_skipdn
      clear f_skipup
    rewind_to_start
    advance

compareLoop = do

  -- f_notdup markers are set
  -- clear f_keep flags
  rewind
  backup
  clear f_notdup
  advance
  while f_char $ advance
  clear f_notdup

  let advance_to_next_dup = do advance; while f_notdup advance
  let backup_to_prev_dup  = do backup; while f_notdup backup
  let rewind_to_start = while f_skipdn backup_to_prev_dup
  let advance_to_end  = while f_skipup advance_to_next_dup

  rewind; while f_notdup advance
  while f_char $ do
    clear f_skipdn
    set1 f_skipup
    advance_to_next_dup
    set1  f_skipdn
    clear f_skipup

    -- while end character is not zero
    while f_char $ do

  -- while start char is not zero
  while f_char $ do
    clear f_skipdn

    set1 f_skipup
    advance
    set1 f_skipdn
    clear f_skipup
    -- while end char is not zero
    while f_char $ do
      copy f_char f_cmpchar
      while f_skipdn $ do copy f_cmpchar (prev f_cmpchar); backup
      dotimes f_char (decr f_cmpchar)

      ifZero f_cmpchar
        (do clear f_notdup
            advance_to_end
            clear f_notdup)
        pass
      advance_to_end
      set1 f_skipup
      advance
      set1 f_skipdn
      clear f_skipup
    rewind_to_start
    advance

-- truncate prefix of string which does not have a dup
init1 = do
  rewind
  while f_notdup $ clear f_char
  rewind

-- compute length
-- set f_length to the length of the string
computeLength = do
  rewind
  clear f_length
  while f_char $ do
    incr f_length
    copy f_length (next f_length)
    advance
  backup
  while f_char $ do
    copy (next f_length) f_length
    backup
  advance

