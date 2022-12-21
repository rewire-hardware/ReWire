module PreWire.Bit where


type Bit = Bool

zero :: Bit
zero = False

one :: Bit
one = True


toInt :: Bit -> Int
toInt True = 1
toInt False = 0

notBit :: Bit -> Bit
notBit = not

-- AND
(>&&<) :: Bit -> Bit -> Bit
True >&&< True = True
_ >&&< _ = False

-- OR
(>||<) :: Bit -> Bit -> Bit
False >||< False = False
_ >||< _ = True

-- XOR
(>^<) :: Bit -> Bit -> Bit
False >^< True = True
True >^< False = True
_ >^< _ = False

-- Eq
(>==<) :: Bit -> Bit -> Bit
False >==< False = True
True >==< True = True
_ >==< _ = False

-- NAND
(>~&<) :: Bit -> Bit -> Bit
True >~&< True = False
_ >~&< _ = True

-- NOR
(>~|<) :: Bit -> Bit -> Bit
False >~|< False = True
_ >~|< _ = False

-- XNOR
(>~^<) :: Bit -> Bit -> Bit
False >~^< True = False
True >~^< False = False
_ >~^< _ = True


-- a b c ~> (a+b+c,carry_out)
rca :: Bit -> Bit -> Bit -> (Bit,Bit)
rca False False False = (False,False)
rca False False True = (True,False)
rca False True False = (True,False)
rca False True True = (False,True)
rca True False False = (True,False)
rca True False True = (False,True)
rca True True False = (False,True)
rca True True True = (True,True)



int2bin :: Int -> [Bit]
int2bin i | i==0      = []
          | otherwise = b : int2bin (i `div` 2)
              where b = case i `mod` 2 of
                      0 -> False
                      1 -> True
                      _ -> Prelude.error "can't happen"