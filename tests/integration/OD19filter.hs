import ReWire
import ReWire.Bits

-------
-- Tests 1-4.
-------

-- Each of the following is implemented in VHDL in vhdl/filterprims.vhd.

test1 :: W32 -> W32 -> Bit
-- test1 i s = let tot = w32sTodouble s i in bool2bit $ ckrange 0 86401 tot
test1 = nativeVhdl "test1" test1

test2 :: W32 -> W32 -> Bit
-- test2 i _ = let (doyb,utcy1) = w32Tow16s i in bool2bit $ ckrange 1 366 doyb && ckrange 1993 9999 utcy1
test2 = nativeVhdl "test2" test2

test3 :: W32 -> W32 -> Bit
-- test3 i _ = let (pn,pacsrc) = w32Tow16s i in
--             let (pactyp,_)  = w16Tow8s pn in
--              bool2bit $ ckrange 0 8 pactyp && ckrange 0 101 pacsrc
test3 = nativeVhdl "test3" test3

test4 :: W32 -> W32 -> Bit
-- test4 i _ = let (_,mtver) = w32Tow16s i  in
--             let (mt,ver)  = w16Tow8s mtver in
--              bool2bit $ mt==1 && ver==6
test4 = nativeVhdl "test4" test4

data YN a = Yes a | No

start :: ReT (YN W32) (YN Bit) I (YN W32)
start = extrude filterOD19 zeroW32

main = undefined

filterOD19 :: ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
filterOD19 = word0 No
  
-- store
word0 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word0 (Yes i) = do
                  lift (put i)
                  i <- signal No
                  word1 i
word0 No      = do
                  i <- signal No
                  word0 i

-- check test1
word1 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word1 (Yes i) = do
                  s <- lift get
                  i <- signal (Yes (test1 i s))
                  word2 i
word1 No      = do
                  i <- signal No
                  word1 i

-- store
word2 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word2 (Yes i) = do
                  lift (put i)
                  i <- signal No
                  word3 i
word2 No      = do
                  i <- signal No
                  word2 i

-- check test1
word3 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word3 (Yes i) = do
                  s <- lift get
                  i <- signal (Yes (test1 i s))
                  word4 i
word3 No      = do
                  i <- signal No
                  word3 i

-- check test2
word4 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word4 (Yes i) = do
                  s <- lift get
                  i <- signal (Yes (test2 i s))
                  word5 i
word4 No      = do
                  i <- signal No
                  word4 i

-- check test3
word5 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word5 (Yes i) = do
                  s <- lift get
                  i <- signal (Yes (test3 i s))
                  word6 i
word5 No      = do
                  i <- signal No
                  word5 i

-- check test4
word6 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word6 (Yes i) = do
                  s <- lift get
                  i <- signal (Yes (test4 i s))
                  word7 i
word6 No       = do
                  i <- signal No
                  word6 i

word7 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word7 (Yes i)     = do
                      i <- signal No
                      word8 i
word7 No          = do
                      i <- signal No
                      word7 i

word8 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word8 (Yes i)     = do
                      i <- signal No
                      word9 i
word8 No          = do
                      i <- signal No
                      word8 i

word9 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word9 (Yes i)     = do
                      i <- signal No
                      word10 i
word9 No          = do
                      i <- signal No
                      word9 i


word10 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word10 (Yes i)     = do
                      i <- signal No
                      word11 i
word10 No          = do
                      i <- signal No
                      word10 i

word11 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word11 (Yes i)     = do
                      i <- signal No
                      word12 i
word11 No          = do
                      i <- signal No
                      word11 i

word12 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word12 (Yes i)     = do
                      i <- signal No
                      word13 i
word12 No          = do
                      i <- signal No
                      word12 i

word13 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word13 (Yes i)     = do
                      i <- signal No
                      word14 i
word13 No          = do
                      i <- signal No
                      word13 i

word14 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word14 (Yes i)     = do
                      i <- signal No
                      word15 i
word14 No          = do
                      i <- signal No
                      word14 i

word15 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word15 (Yes i)     = do
                      i <- signal No
                      word16 i
word15 No          = do
                      i <- signal No
                      word15 i

word16 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word16 (Yes i)     = do
                      i <- signal No
                      word17 i
word16 No          = do
                      i <- signal No
                      word16 i

word17 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word17 (Yes i)     = do
                      i <- signal No
                      word18 i
word17 No          = do
                      i <- signal No
                      word17 i

word18 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word18 (Yes i)     = do
                      i <- signal No
                      word19 i
word18 No          = do
                      i <- signal No
                      word18 i

word19 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word19 (Yes i)     = do
                      i <- signal No
                      word20 i
word19 No          = do
                      i <- signal No
                      word19 i

word20 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word20 (Yes i)     = do
                      i <- signal No
                      word21 i
word20 No          = do
                      i <- signal No
                      word20 i

word21 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word21 (Yes i)     = do
                      i <- signal No
                      word22 i
word21 No          = do
                      i <- signal No
                      word21 i

word22 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word22 (Yes i)     = do
                      i <- signal No
                      word23 i
word22 No          = do
                      i <- signal No
                      word22 i

word23 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word23 (Yes i)     = do
                      i <- signal No
                      word24 i
word23 No          = do
                      i <- signal No
                      word23 i

word24 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word24 (Yes i)     = do
                      i <- signal No
                      word25 i
word24 No          = do
                      i <- signal No
                      word24 i

word25 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word25 (Yes i)     = do
                      i <- signal No
                      word26 i
word25 No          = do
                      i <- signal No
                      word25 i

word26 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word26 (Yes i)     = do
                      i <- signal No
                      word27 i
word26 No          = do
                      i <- signal No
                      word26 i

word27 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word27 (Yes i)     = do
                      i <- signal No
                      word28 i
word27 No          = do
                      i <- signal No
                      word27 i

word28 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word28 (Yes i)     = do
                      i <- signal No
                      word29 i
word28 No          = do
                      i <- signal No
                      word28 i

word29 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word29 (Yes i)     = do
                      i <- signal No
                      word30 i
word29 No          = do
                      i <- signal No
                      word29 i

word30 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word30 (Yes i)     = do
                      i <- signal No
                      word31 i
word30 No          = do
                      i <- signal No
                      word30 i

word31 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word31 (Yes i)     = do
                      i <- signal No
                      word32 i
word31 No          = do
                      i <- signal No
                      word31 i

word32 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word32 (Yes i)     = do
                      i <- signal No
                      word33 i
word32 No          = do
                      i <- signal No
                      word32 i

word33 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word33 (Yes i)     = do
                      i <- signal No
                      word34 i
word33 No          = do
                      i <- signal No
                      word33 i

word34 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word34 (Yes i)     = do
                      i <- signal No
                      word35 i
word34 No          = do
                      i <- signal No
                      word34 i

word35 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word35 (Yes i)     = do
                      i <- signal No
                      word36 i
word35 No          = do
                      i <- signal No
                      word35 i

word36 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word36 (Yes i)     = do
                      i <- signal No
                      word37 i
word36 No          = do
                      i <- signal No
                      word36 i

word37 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word37 (Yes i)     = do
                      i <- signal No
                      word38 i
word37 No          = do
                      i <- signal No
                      word37 i

word38 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word38 (Yes i)     = do
                      i <- signal No
                      word39 i
word38 No          = do
                      i <- signal No
                      word38 i

word39 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word39 (Yes i)     = do
                      i <- signal No
                      word40 i
word39 No          = do
                      i <- signal No
                      word39 i

word40 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word40 (Yes i)     = do
                      i <- signal No
                      word41 i
word40 No          = do
                      i <- signal No
                      word40 i

word41 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word41 (Yes i)     = do
                      i <- signal No
                      word42 i
word41 No          = do
                      i <- signal No
                      word41 i

word42 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word42 (Yes i)     = do
                      i <- signal No
                      word43 i
word42 No          = do
                      i <- signal No
                      word42 i

word43 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word43 (Yes i)     = do
                      i <- signal No
                      word44 i
word43 No          = do
                      i <- signal No
                      word43 i

word44 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word44 (Yes i)     = do
                      i <- signal No
                      word45 i
word44 No          = do
                      i <- signal No
                      word44 i

word45 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word45 (Yes i)     = do
                      i <- signal No
                      word46 i
word45 No          = do
                      i <- signal No
                      word45 i

word46 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word46 (Yes i)     = do
                      i <- signal No
                      word47 i
word46 No          = do
                      i <- signal No
                      word46 i

word47 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word47 (Yes i)     = do
                      i <- signal No
                      word48 i
word47 No          = do
                      i <- signal No
                      word47 i

word48 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word48 (Yes i)     = do
                      i <- signal No
                      word49 i
word48 No          = do
                      i <- signal No
                      word48 i

word49 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word49 (Yes i)     = do
                      i <- signal No
                      word50 i
word49 No          = do
                      i <- signal No
                      word49 i

word50 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word50 (Yes i)     = do
                      i <- signal No
                      word51 i
word50 No          = do
                      i <- signal No
                      word50 i

word51 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word51 (Yes i)     = do
                      i <- signal No
                      word52 i
word51 No          = do
                      i <- signal No
                      word51 i

word52 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word52 (Yes i)     = do
                      i <- signal No
                      word53 i
word52 No          = do
                      i <- signal No
                      word52 i

word53 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word53 (Yes i)     = do
                      i <- signal No
                      word54 i
word53 No          = do
                      i <- signal No
                      word53 i

word54 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word54 (Yes i)     = do
                      i <- signal No
                      word55 i
word54 No          = do
                      i <- signal No
                      word54 i

word55 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word55 (Yes i)     = do
                      i <- signal No
                      word56 i
word55 No          = do
                      i <- signal No
                      word55 i

word56 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word56 (Yes i)     = do
                      i <- signal No
                      word57 i
word56 No          = do
                      i <- signal No
                      word56 i

word57 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word57 (Yes i)     = do
                      i <- signal No
                      word58 i
word57 No          = do
                      i <- signal No
                      word57 i

word58 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word58 (Yes i)     = do
                      i <- signal No
                      word59 i
word58 No          = do
                      i <- signal No
                      word58 i

word59 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word59 (Yes i)     = do
                      i <- signal No
                      word60 i
word59 No          = do
                      i <- signal No
                      word59 i

word60 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word60 (Yes i)     = do
                      i <- signal No
                      word61 i
word60 No          = do
                      i <- signal No
                      word60 i

word61 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word61 (Yes i)     = do
                      i <- signal No
                      word62 i
word61 No          = do
                      i <- signal No
                      word61 i

word62 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word62 (Yes i)     = do
                      i <- signal No
                      word63 i
word62 No          = do
                      i <- signal No
                      word62 i

word63 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word63 (Yes i)     = do
                      i <- signal No
                      word64 i
word63 No          = do
                      i <- signal No
                      word63 i

word64 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word64 (Yes i)     = do
                      i <- signal No
                      word65 i
word64 No          = do
                      i <- signal No
                      word64 i

word65 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word65 (Yes i)     = do
                      i <- signal No
                      word66 i
word65 No          = do
                      i <- signal No
                      word65 i

word66 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word66 (Yes i)     = do
                      i <- signal No
                      word67 i
word66 No          = do
                      i <- signal No
                      word66 i

word67 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word67 (Yes i)     = do
                      i <- signal No
                      word68 i
word67 No          = do
                      i <- signal No
                      word67 i

word68 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word68 (Yes i)     = do
                      i <- signal No
                      word69 i
word68 No          = do
                      i <- signal No
                      word68 i

word69 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word69 (Yes i)     = do
                      i <- signal No
                      word70 i
word69 No          = do
                      i <- signal No
                      word69 i

word70 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word70 (Yes i)     = do
                      i <- signal No
                      word71 i
word70 No          = do
                      i <- signal No
                      word70 i

word71 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word71 (Yes i)     = do
                      i <- signal No
                      word72 i
word71 No          = do
                      i <- signal No
                      word71 i

word72 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word72 (Yes i)     = do
                      i <- signal No
                      word73 i
word72 No          = do
                      i <- signal No
                      word72 i

word73 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word73 (Yes i)     = do
                      i <- signal No
                      word74 i
word73 No          = do
                      i <- signal No
                      word73 i

word74 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word74 (Yes i)     = do
                      i <- signal No
                      word75 i
word74 No          = do
                      i <- signal No
                      word74 i

word75 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word75 (Yes i)     = do
                      i <- signal No
                      word76 i
word75 No          = do
                      i <- signal No
                      word75 i

word76 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word76 (Yes i)     = do
                      i <- signal No
                      word77 i
word76 No          = do
                      i <- signal No
                      word76 i

word77 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word77 (Yes i)     = do
                      i <- signal No
                      word78 i
word77 No          = do
                      i <- signal No
                      word77 i

word78 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word78 (Yes i)     = do
                      i <- signal No
                      word79 i
word78 No          = do
                      i <- signal No
                      word78 i

word79 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word79 (Yes i)     = do
                      i <- signal No
                      word80 i
word79 No          = do
                      i <- signal No
                      word79 i

word80 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word80 (Yes i)     = do
                      i <- signal No
                      word81 i
word80 No          = do
                      i <- signal No
                      word80 i

word81 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word81 (Yes i)     = do
                      i <- signal No
                      word82 i
word81 No          = do
                      i <- signal No
                      word81 i

word82 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word82 (Yes i)     = do
                      i <- signal No
                      word83 i
word82 No          = do
                      i <- signal No
                      word82 i

word83 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word83 (Yes i)     = do
                      i <- signal No
                      word84 i
word83 No          = do
                      i <- signal No
                      word83 i

word84 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word84 (Yes i)     = do
                      i <- signal No
                      word85 i
word84 No          = do
                      i <- signal No
                      word84 i

word85 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word85 (Yes i)     = do
                      i <- signal No
                      word86 i
word85 No          = do
                      i <- signal No
                      word85 i

word86 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word86 (Yes i)     = do
                      i <- signal No
                      word87 i
word86 No          = do
                      i <- signal No
                      word86 i

word87 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word87 (Yes i)     = do
                      i <- signal No
                      word88 i
word87 No          = do
                      i <- signal No
                      word87 i

word88 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word88 (Yes i)     = do
                      i <- signal No
                      word89 i
word88 No          = do
                      i <- signal No
                      word88 i

word89 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word89 (Yes i)     = do
                      i <- signal No
                      word90 i
word89 No          = do
                      i <- signal No
                      word89 i

word90 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word90 (Yes i)     = do
                      i <- signal No
                      word91 i
word90 No          = do
                      i <- signal No
                      word90 i

word91 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word91 (Yes i)     = do
                      i <- signal No
                      word92 i
word91 No          = do
                      i <- signal No
                      word91 i

word92 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word92 (Yes i)     = do
                      i <- signal No
                      word93 i
word92 No          = do
                      i <- signal No
                      word92 i

word93 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word93 (Yes i)     = do
                      i <- signal No
                      word94 i
word93 No          = do
                      i <- signal No
                      word93 i

word94 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word94 (Yes i)     = do
                      i <- signal No
                      word95 i
word94 No          = do
                      i <- signal No
                      word94 i

word95 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word95 (Yes i)     = do
                      i <- signal No
                      word96 i
word95 No          = do
                      i <- signal No
                      word95 i

word96 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word96 (Yes i)     = do
                      i <- signal No
                      word97 i
word96 No          = do
                      i <- signal No
                      word96 i

word97 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word97 (Yes i)     = do
                      i <- signal No
                      word98 i
word97 No          = do
                      i <- signal No
                      word97 i

word98 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word98 (Yes i)     = do
                      i <- signal No
                      word99 i
word98 No          = do
                      i <- signal No
                      word98 i

word99 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word99 (Yes i)     = do
                      i <- signal No
                      word100 i
word99 No          = do
                      i <- signal No
                      word99 i

word100 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word100 (Yes i)     = do
                      i <- signal No
                      word101 i
word100 No          = do
                      i <- signal No
                      word100 i

word101 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word101 (Yes i)     = do
                      i <- signal No
                      word102 i
word101 No          = do
                      i <- signal No
                      word101 i

word102 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word102 (Yes i)     = do
                      i <- signal No
                      word103 i
word102 No          = do
                      i <- signal No
                      word102 i

word103 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word103 (Yes i)     = do
                      i <- signal No
                      word104 i
word103 No          = do
                      i <- signal No
                      word103 i

word104 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word104 (Yes i)     = do
                      i <- signal No
                      word105 i
word104 No          = do
                      i <- signal No
                      word104 i

word105 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word105 (Yes i)     = do
                      i <- signal No
                      word106 i
word105 No          = do
                      i <- signal No
                      word105 i

word106 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word106 (Yes i)     = do
                      i <- signal No
                      word107 i
word106 No          = do
                      i <- signal No
                      word106 i

word107 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word107 (Yes i)     = do
                      i <- signal No
                      word108 i
word107 No          = do
                      i <- signal No
                      word107 i

word108 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word108 (Yes i)     = do
                      i <- signal No
                      word109 i
word108 No          = do
                      i <- signal No
                      word108 i

word109 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word109 (Yes i)     = do
                      i <- signal No
                      word110 i
word109 No          = do
                      i <- signal No
                      word109 i

word110 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word110 (Yes i)     = do
                      i <- signal No
                      word111 i
word110 No          = do
                      i <- signal No
                      word110 i

word111 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word111 (Yes i)     = do
                      i <- signal No
                      word112 i
word111 No          = do
                      i <- signal No
                      word111 i

word112 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word112 (Yes i)     = do
                      i <- signal No
                      word113 i
word112 No          = do
                      i <- signal No
                      word112 i

word113 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word113 (Yes i)     = do
                      i <- signal No
                      word114 i
word113 No          = do
                      i <- signal No
                      word113 i

word114 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word114 (Yes i)     = do
                      i <- signal No
                      word115 i
word114 No          = do
                      i <- signal No
                      word114 i

word115 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word115 (Yes i)     = do
                      i <- signal No
                      word116 i
word115 No          = do
                      i <- signal No
                      word115 i

word116 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word116 (Yes i)     = do
                      i <- signal No
                      word117 i
word116 No          = do
                      i <- signal No
                      word116 i

word117 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word117 (Yes i)     = do
                      i <- signal No
                      word118 i
word117 No          = do
                      i <- signal No
                      word117 i

word118 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word118 (Yes i)     = do
                      i <- signal No
                      word119 i
word118 No          = do
                      i <- signal No
                      word118 i

word119 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word119 (Yes i)     = do
                      i <- signal No
                      word120 i
word119 No          = do
                      i <- signal No
                      word119 i

word120 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word120 (Yes i)     = do
                      i <- signal No
                      word121 i
word120 No          = do
                      i <- signal No
                      word120 i

word121 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word121 (Yes i)     = do
                      i <- signal No
                      word122 i
word121 No          = do
                      i <- signal No
                      word121 i

word122 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word122 (Yes i)     = do
                      i <- signal No
                      word123 i
word122 No          = do
                      i <- signal No
                      word122 i

word123 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word123 (Yes i)     = do
                      i <- signal No
                      word124 i
word123 No          = do
                      i <- signal No
                      word123 i

word124 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word124 (Yes i)     = do
                      i <- signal No
                      word125 i
word124 No          = do
                      i <- signal No
                      word124 i

word125 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word125 (Yes i)     = do
                      i <- signal No
                      word126 i
word125 No          = do
                      i <- signal No
                      word125 i

word126 :: YN W32 -> ReT (YN W32) (YN Bit) (StT W32 I) (YN W32)
word126 (Yes i)     = do
                      i <- signal No
                      word0 i
word126 No          = do
                      i <- signal No
                      word126 i

