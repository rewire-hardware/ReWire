{-# LANGUAGE DataKinds #-}
import Prelude hiding (round, (++), (^), replicate, map, 
                       take, drop, head, tail, (-),
                       init, last, (+), (*), (&&), (/=), (==), mod,
                       (<), (>=), (>), reverse, div, (/))
import ReWire
import ReWire.Vectors
import ReWire.Bits
import ReWire.Monad ( iter, Dev )

-- ** Types and Parameters

-- | Constants
type DataWord = W 128
primitive_polynomial :: DataWord
primitive_polynomial = lit 0xE1000000000000000000000000000000

-- | Interface types
type Multiplicand_i = DataWord
type Multiplier_i   = DataWord
type Product_o      = DataWord

initInput :: (Multiplicand_i,Multiplier_i)
initInput = (lit 0, lit 0)

-- | Note that our indexing will be backwards because in ReWire, msbit is at index 0 of a word
multiply :: Multiplicand_i -> Multiplier_i -> Product_o
multiply input_a input_b =
    let accumulation_vector :: DataWord
        accumulation_vector = lit 0 -- let Z_0 = 0 ^ 128
        temp_vector :: DataWord
        temp_vector = input_a -- let V_0 = Y
        -- i = 0; i<128; i++
        -- accumulation_vector_i = if input_b ! (Proxy :: Proxy i) then accumulation_vector_{i-1} ^ temp_vector_{i-1} else accumulation_vector_{i-1}
        -- temp_vector_i = if not (temp_vector_{i-1} ! (Proxy :: Proxy 127)) then temp_vector_{i-1} >>. lit 1 else (temp_vector_{i-1} >>. lit 1) ^ primitive_polynomial
        accumulation_vector_0 = if input_b ! (Proxy :: Proxy 0) then accumulation_vector ^ temp_vector else accumulation_vector -- Z_1 = if x_0 is 1 then Z_0 ^ V_0 else Z_0
        temp_vector_0 = if not (temp_vector ! (Proxy :: Proxy 127)) then temp_vector >>. lit 1 else (temp_vector >>. lit 1) ^ primitive_polynomial -- V_1 = if LSB(V_0) is 1 then V_0 >>. 1 ^  else 

        accumulation_vector_1 = if input_b ! (Proxy :: Proxy 1) then accumulation_vector_0 ^ temp_vector_0 else accumulation_vector_0
        temp_vector_1 = if not (temp_vector_0 ! (Proxy :: Proxy 127)) then temp_vector_0 >>. lit 1 else (temp_vector_0 >>. lit 1) ^ primitive_polynomial

        accumulation_vector_2 = if input_b ! (Proxy :: Proxy 2) then accumulation_vector_1 ^ temp_vector_1 else accumulation_vector_1
        temp_vector_2 = if not (temp_vector_1 ! (Proxy :: Proxy 127)) then temp_vector_1 >>. lit 1 else (temp_vector_1 >>. lit 1) ^ primitive_polynomial

        accumulation_vector_3 = if input_b ! (Proxy :: Proxy 3) then accumulation_vector_2 ^ temp_vector_2 else accumulation_vector_2
        temp_vector_3 = if not (temp_vector_2 ! (Proxy :: Proxy 127)) then temp_vector_2 >>. lit 1 else (temp_vector_2 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_4 = if input_b ! (Proxy :: Proxy 4) then accumulation_vector_3 ^ temp_vector_3 else accumulation_vector_3
        temp_vector_4 = if not (temp_vector_3 ! (Proxy :: Proxy 127)) then temp_vector_3 >>. lit 1 else (temp_vector_3 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_5 = if input_b ! (Proxy :: Proxy 5) then accumulation_vector_4 ^ temp_vector_4 else accumulation_vector_4
        temp_vector_5 = if not (temp_vector_4 ! (Proxy :: Proxy 127)) then temp_vector_4 >>. lit 1 else (temp_vector_4 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_6 = if input_b ! (Proxy :: Proxy 6) then accumulation_vector_5 ^ temp_vector_5 else accumulation_vector_5
        temp_vector_6 = if not (temp_vector_5 ! (Proxy :: Proxy 127)) then temp_vector_5 >>. lit 1 else (temp_vector_5 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_7 = if input_b ! (Proxy :: Proxy 7) then accumulation_vector_6 ^ temp_vector_6 else accumulation_vector_6
        temp_vector_7 = if not (temp_vector_6 ! (Proxy :: Proxy 127)) then temp_vector_6 >>. lit 1 else (temp_vector_6 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_8 = if input_b ! (Proxy :: Proxy 8) then accumulation_vector_7 ^ temp_vector_7 else accumulation_vector_7
        temp_vector_8 = if not (temp_vector_7 ! (Proxy :: Proxy 127)) then temp_vector_7 >>. lit 1 else (temp_vector_7 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_9 = if input_b ! (Proxy :: Proxy 9) then accumulation_vector_8 ^ temp_vector_8 else accumulation_vector_8
        temp_vector_9 = if not (temp_vector_8 ! (Proxy :: Proxy 127)) then temp_vector_8 >>. lit 1 else (temp_vector_8 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_10 = if input_b ! (Proxy :: Proxy 10) then accumulation_vector_9 ^ temp_vector_9 else accumulation_vector_9
        temp_vector_10 = if not (temp_vector_9 ! (Proxy :: Proxy 127)) then temp_vector_9 >>. lit 1 else (temp_vector_9 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_11 = if input_b ! (Proxy :: Proxy 11) then accumulation_vector_10 ^ temp_vector_10 else accumulation_vector_10
        temp_vector_11 = if not (temp_vector_10 ! (Proxy :: Proxy 127)) then temp_vector_10 >>. lit 1 else (temp_vector_10 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_12 = if input_b ! (Proxy :: Proxy 12) then accumulation_vector_11 ^ temp_vector_11 else accumulation_vector_11
        temp_vector_12 = if not (temp_vector_11 ! (Proxy :: Proxy 127)) then temp_vector_11 >>. lit 1 else (temp_vector_11 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_13 = if input_b ! (Proxy :: Proxy 13) then accumulation_vector_12 ^ temp_vector_12 else accumulation_vector_12
        temp_vector_13 = if not (temp_vector_12 ! (Proxy :: Proxy 127)) then temp_vector_12 >>. lit 1 else (temp_vector_12 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_14 = if input_b ! (Proxy :: Proxy 14) then accumulation_vector_13 ^ temp_vector_13 else accumulation_vector_13
        temp_vector_14 = if not (temp_vector_13 ! (Proxy :: Proxy 127)) then temp_vector_13 >>. lit 1 else (temp_vector_13 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_15 = if input_b ! (Proxy :: Proxy 15) then accumulation_vector_14 ^ temp_vector_14 else accumulation_vector_14
        temp_vector_15 = if not (temp_vector_14 ! (Proxy :: Proxy 127)) then temp_vector_14 >>. lit 1 else (temp_vector_14 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_16 = if input_b ! (Proxy :: Proxy 16) then accumulation_vector_15 ^ temp_vector_15 else accumulation_vector_15
        temp_vector_16 = if not (temp_vector_15 ! (Proxy :: Proxy 127)) then temp_vector_15 >>. lit 1 else (temp_vector_15 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_17 = if input_b ! (Proxy :: Proxy 17) then accumulation_vector_16 ^ temp_vector_16 else accumulation_vector_16
        temp_vector_17 = if not (temp_vector_16 ! (Proxy :: Proxy 127)) then temp_vector_16 >>. lit 1 else (temp_vector_16 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_18 = if input_b ! (Proxy :: Proxy 18) then accumulation_vector_17 ^ temp_vector_17 else accumulation_vector_17
        temp_vector_18 = if not (temp_vector_17 ! (Proxy :: Proxy 127)) then temp_vector_17 >>. lit 1 else (temp_vector_17 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_19 = if input_b ! (Proxy :: Proxy 19) then accumulation_vector_18 ^ temp_vector_18 else accumulation_vector_18
        temp_vector_19 = if not (temp_vector_18 ! (Proxy :: Proxy 127)) then temp_vector_18 >>. lit 1 else (temp_vector_18 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_20 = if input_b ! (Proxy :: Proxy 20) then accumulation_vector_19 ^ temp_vector_19 else accumulation_vector_19
        temp_vector_20 = if not (temp_vector_19 ! (Proxy :: Proxy 127)) then temp_vector_19 >>. lit 1 else (temp_vector_19 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_21 = if input_b ! (Proxy :: Proxy 21) then accumulation_vector_20 ^ temp_vector_20 else accumulation_vector_20
        temp_vector_21 = if not (temp_vector_20 ! (Proxy :: Proxy 127)) then temp_vector_20 >>. lit 1 else (temp_vector_20 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_22 = if input_b ! (Proxy :: Proxy 22) then accumulation_vector_21 ^ temp_vector_21 else accumulation_vector_21
        temp_vector_22 = if not (temp_vector_21 ! (Proxy :: Proxy 127)) then temp_vector_21 >>. lit 1 else (temp_vector_21 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_23 = if input_b ! (Proxy :: Proxy 23) then accumulation_vector_22 ^ temp_vector_22 else accumulation_vector_22
        temp_vector_23 = if not (temp_vector_22 ! (Proxy :: Proxy 127)) then temp_vector_22 >>. lit 1 else (temp_vector_22 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_24 = if input_b ! (Proxy :: Proxy 24) then accumulation_vector_23 ^ temp_vector_23 else accumulation_vector_23
        temp_vector_24 = if not (temp_vector_23 ! (Proxy :: Proxy 127)) then temp_vector_23 >>. lit 1 else (temp_vector_23 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_25 = if input_b ! (Proxy :: Proxy 25) then accumulation_vector_24 ^ temp_vector_24 else accumulation_vector_24
        temp_vector_25 = if not (temp_vector_24 ! (Proxy :: Proxy 127)) then temp_vector_24 >>. lit 1 else (temp_vector_24 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_26 = if input_b ! (Proxy :: Proxy 26) then accumulation_vector_25 ^ temp_vector_25 else accumulation_vector_25
        temp_vector_26 = if not (temp_vector_25 ! (Proxy :: Proxy 127)) then temp_vector_25 >>. lit 1 else (temp_vector_25 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_27 = if input_b ! (Proxy :: Proxy 27) then accumulation_vector_26 ^ temp_vector_26 else accumulation_vector_26
        temp_vector_27 = if not (temp_vector_26 ! (Proxy :: Proxy 127)) then temp_vector_26 >>. lit 1 else (temp_vector_26 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_28 = if input_b ! (Proxy :: Proxy 28) then accumulation_vector_27 ^ temp_vector_27 else accumulation_vector_27
        temp_vector_28 = if not (temp_vector_27 ! (Proxy :: Proxy 127)) then temp_vector_27 >>. lit 1 else (temp_vector_27 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_29 = if input_b ! (Proxy :: Proxy 29) then accumulation_vector_28 ^ temp_vector_28 else accumulation_vector_28
        temp_vector_29 = if not (temp_vector_28 ! (Proxy :: Proxy 127)) then temp_vector_28 >>. lit 1 else (temp_vector_28 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_30 = if input_b ! (Proxy :: Proxy 30) then accumulation_vector_29 ^ temp_vector_29 else accumulation_vector_29
        temp_vector_30 = if not (temp_vector_29 ! (Proxy :: Proxy 127)) then temp_vector_29 >>. lit 1 else (temp_vector_29 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_31 = if input_b ! (Proxy :: Proxy 31) then accumulation_vector_30 ^ temp_vector_30 else accumulation_vector_30
        temp_vector_31 = if not (temp_vector_30 ! (Proxy :: Proxy 127)) then temp_vector_30 >>. lit 1 else (temp_vector_30 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_32 = if input_b ! (Proxy :: Proxy 32) then accumulation_vector_31 ^ temp_vector_31 else accumulation_vector_31
        temp_vector_32 = if not (temp_vector_31 ! (Proxy :: Proxy 127)) then temp_vector_31 >>. lit 1 else (temp_vector_31 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_33 = if input_b ! (Proxy :: Proxy 33) then accumulation_vector_32 ^ temp_vector_32 else accumulation_vector_32
        temp_vector_33 = if not (temp_vector_32 ! (Proxy :: Proxy 127)) then temp_vector_32 >>. lit 1 else (temp_vector_32 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_34 = if input_b ! (Proxy :: Proxy 34) then accumulation_vector_33 ^ temp_vector_33 else accumulation_vector_33
        temp_vector_34 = if not (temp_vector_33 ! (Proxy :: Proxy 127)) then temp_vector_33 >>. lit 1 else (temp_vector_33 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_35 = if input_b ! (Proxy :: Proxy 35) then accumulation_vector_34 ^ temp_vector_34 else accumulation_vector_34
        temp_vector_35 = if not (temp_vector_34 ! (Proxy :: Proxy 127)) then temp_vector_34 >>. lit 1 else (temp_vector_34 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_36 = if input_b ! (Proxy :: Proxy 36) then accumulation_vector_35 ^ temp_vector_35 else accumulation_vector_35
        temp_vector_36 = if not (temp_vector_35 ! (Proxy :: Proxy 127)) then temp_vector_35 >>. lit 1 else (temp_vector_35 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_37 = if input_b ! (Proxy :: Proxy 37) then accumulation_vector_36 ^ temp_vector_36 else accumulation_vector_36
        temp_vector_37 = if not (temp_vector_36 ! (Proxy :: Proxy 127)) then temp_vector_36 >>. lit 1 else (temp_vector_36 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_38 = if input_b ! (Proxy :: Proxy 38) then accumulation_vector_37 ^ temp_vector_37 else accumulation_vector_37
        temp_vector_38 = if not (temp_vector_37 ! (Proxy :: Proxy 127)) then temp_vector_37 >>. lit 1 else (temp_vector_37 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_39 = if input_b ! (Proxy :: Proxy 39) then accumulation_vector_38 ^ temp_vector_38 else accumulation_vector_38
        temp_vector_39 = if not (temp_vector_38 ! (Proxy :: Proxy 127)) then temp_vector_38 >>. lit 1 else (temp_vector_38 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_40 = if input_b ! (Proxy :: Proxy 40) then accumulation_vector_39 ^ temp_vector_39 else accumulation_vector_39
        temp_vector_40 = if not (temp_vector_39 ! (Proxy :: Proxy 127)) then temp_vector_39 >>. lit 1 else (temp_vector_39 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_41 = if input_b ! (Proxy :: Proxy 41) then accumulation_vector_40 ^ temp_vector_40 else accumulation_vector_40
        temp_vector_41 = if not (temp_vector_40 ! (Proxy :: Proxy 127)) then temp_vector_40 >>. lit 1 else (temp_vector_40 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_42 = if input_b ! (Proxy :: Proxy 42) then accumulation_vector_41 ^ temp_vector_41 else accumulation_vector_41
        temp_vector_42 = if not (temp_vector_41 ! (Proxy :: Proxy 127)) then temp_vector_41 >>. lit 1 else (temp_vector_41 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_43 = if input_b ! (Proxy :: Proxy 43) then accumulation_vector_42 ^ temp_vector_42 else accumulation_vector_42
        temp_vector_43 = if not (temp_vector_42 ! (Proxy :: Proxy 127)) then temp_vector_42 >>. lit 1 else (temp_vector_42 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_44 = if input_b ! (Proxy :: Proxy 44) then accumulation_vector_43 ^ temp_vector_43 else accumulation_vector_43
        temp_vector_44 = if not (temp_vector_43 ! (Proxy :: Proxy 127)) then temp_vector_43 >>. lit 1 else (temp_vector_43 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_45 = if input_b ! (Proxy :: Proxy 45) then accumulation_vector_44 ^ temp_vector_44 else accumulation_vector_44
        temp_vector_45 = if not (temp_vector_44 ! (Proxy :: Proxy 127)) then temp_vector_44 >>. lit 1 else (temp_vector_44 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_46 = if input_b ! (Proxy :: Proxy 46) then accumulation_vector_45 ^ temp_vector_45 else accumulation_vector_45
        temp_vector_46 = if not (temp_vector_45 ! (Proxy :: Proxy 127)) then temp_vector_45 >>. lit 1 else (temp_vector_45 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_47 = if input_b ! (Proxy :: Proxy 47) then accumulation_vector_46 ^ temp_vector_46 else accumulation_vector_46
        temp_vector_47 = if not (temp_vector_46 ! (Proxy :: Proxy 127)) then temp_vector_46 >>. lit 1 else (temp_vector_46 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_48 = if input_b ! (Proxy :: Proxy 48) then accumulation_vector_47 ^ temp_vector_47 else accumulation_vector_47
        temp_vector_48 = if not (temp_vector_47 ! (Proxy :: Proxy 127)) then temp_vector_47 >>. lit 1 else (temp_vector_47 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_49 = if input_b ! (Proxy :: Proxy 49) then accumulation_vector_48 ^ temp_vector_48 else accumulation_vector_48
        temp_vector_49 = if not (temp_vector_48 ! (Proxy :: Proxy 127)) then temp_vector_48 >>. lit 1 else (temp_vector_48 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_50 = if input_b ! (Proxy :: Proxy 50) then accumulation_vector_49 ^ temp_vector_49 else accumulation_vector_49
        temp_vector_50 = if not (temp_vector_49 ! (Proxy :: Proxy 127)) then temp_vector_49 >>. lit 1 else (temp_vector_49 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_51 = if input_b ! (Proxy :: Proxy 51) then accumulation_vector_50 ^ temp_vector_50 else accumulation_vector_50
        temp_vector_51 = if not (temp_vector_50 ! (Proxy :: Proxy 127)) then temp_vector_50 >>. lit 1 else (temp_vector_50 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_52 = if input_b ! (Proxy :: Proxy 52) then accumulation_vector_51 ^ temp_vector_51 else accumulation_vector_51
        temp_vector_52 = if not (temp_vector_51 ! (Proxy :: Proxy 127)) then temp_vector_51 >>. lit 1 else (temp_vector_51 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_53 = if input_b ! (Proxy :: Proxy 53) then accumulation_vector_52 ^ temp_vector_52 else accumulation_vector_52
        temp_vector_53 = if not (temp_vector_52 ! (Proxy :: Proxy 127)) then temp_vector_52 >>. lit 1 else (temp_vector_52 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_54 = if input_b ! (Proxy :: Proxy 54) then accumulation_vector_53 ^ temp_vector_53 else accumulation_vector_53
        temp_vector_54 = if not (temp_vector_53 ! (Proxy :: Proxy 127)) then temp_vector_53 >>. lit 1 else (temp_vector_53 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_55 = if input_b ! (Proxy :: Proxy 55) then accumulation_vector_54 ^ temp_vector_54 else accumulation_vector_54
        temp_vector_55 = if not (temp_vector_54 ! (Proxy :: Proxy 127)) then temp_vector_54 >>. lit 1 else (temp_vector_54 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_56 = if input_b ! (Proxy :: Proxy 56) then accumulation_vector_55 ^ temp_vector_55 else accumulation_vector_55
        temp_vector_56 = if not (temp_vector_55 ! (Proxy :: Proxy 127)) then temp_vector_55 >>. lit 1 else (temp_vector_55 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_57 = if input_b ! (Proxy :: Proxy 57) then accumulation_vector_56 ^ temp_vector_56 else accumulation_vector_56
        temp_vector_57 = if not (temp_vector_56 ! (Proxy :: Proxy 127)) then temp_vector_56 >>. lit 1 else (temp_vector_56 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_58 = if input_b ! (Proxy :: Proxy 58) then accumulation_vector_57 ^ temp_vector_57 else accumulation_vector_57
        temp_vector_58 = if not (temp_vector_57 ! (Proxy :: Proxy 127)) then temp_vector_57 >>. lit 1 else (temp_vector_57 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_59 = if input_b ! (Proxy :: Proxy 59) then accumulation_vector_58 ^ temp_vector_58 else accumulation_vector_58
        temp_vector_59 = if not (temp_vector_58 ! (Proxy :: Proxy 127)) then temp_vector_58 >>. lit 1 else (temp_vector_58 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_60 = if input_b ! (Proxy :: Proxy 60) then accumulation_vector_59 ^ temp_vector_59 else accumulation_vector_59
        temp_vector_60 = if not (temp_vector_59 ! (Proxy :: Proxy 127)) then temp_vector_59 >>. lit 1 else (temp_vector_59 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_61 = if input_b ! (Proxy :: Proxy 61) then accumulation_vector_60 ^ temp_vector_60 else accumulation_vector_60
        temp_vector_61 = if not (temp_vector_60 ! (Proxy :: Proxy 127)) then temp_vector_60 >>. lit 1 else (temp_vector_60 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_62 = if input_b ! (Proxy :: Proxy 62) then accumulation_vector_61 ^ temp_vector_61 else accumulation_vector_61
        temp_vector_62 = if not (temp_vector_61 ! (Proxy :: Proxy 127)) then temp_vector_61 >>. lit 1 else (temp_vector_61 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_63 = if input_b ! (Proxy :: Proxy 63) then accumulation_vector_62 ^ temp_vector_62 else accumulation_vector_62
        temp_vector_63 = if not (temp_vector_62 ! (Proxy :: Proxy 127)) then temp_vector_62 >>. lit 1 else (temp_vector_62 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_64 = if input_b ! (Proxy :: Proxy 64) then accumulation_vector_63 ^ temp_vector_63 else accumulation_vector_63
        temp_vector_64 = if not (temp_vector_63 ! (Proxy :: Proxy 127)) then temp_vector_63 >>. lit 1 else (temp_vector_63 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_65 = if input_b ! (Proxy :: Proxy 65) then accumulation_vector_64 ^ temp_vector_64 else accumulation_vector_64
        temp_vector_65 = if not (temp_vector_64 ! (Proxy :: Proxy 127)) then temp_vector_64 >>. lit 1 else (temp_vector_64 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_66 = if input_b ! (Proxy :: Proxy 66) then accumulation_vector_65 ^ temp_vector_65 else accumulation_vector_65
        temp_vector_66 = if not (temp_vector_65 ! (Proxy :: Proxy 127)) then temp_vector_65 >>. lit 1 else (temp_vector_65 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_67 = if input_b ! (Proxy :: Proxy 67) then accumulation_vector_66 ^ temp_vector_66 else accumulation_vector_66
        temp_vector_67 = if not (temp_vector_66 ! (Proxy :: Proxy 127)) then temp_vector_66 >>. lit 1 else (temp_vector_66 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_68 = if input_b ! (Proxy :: Proxy 68) then accumulation_vector_67 ^ temp_vector_67 else accumulation_vector_67
        temp_vector_68 = if not (temp_vector_67 ! (Proxy :: Proxy 127)) then temp_vector_67 >>. lit 1 else (temp_vector_67 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_69 = if input_b ! (Proxy :: Proxy 69) then accumulation_vector_68 ^ temp_vector_68 else accumulation_vector_68
        temp_vector_69 = if not (temp_vector_68 ! (Proxy :: Proxy 127)) then temp_vector_68 >>. lit 1 else (temp_vector_68 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_70 = if input_b ! (Proxy :: Proxy 70) then accumulation_vector_69 ^ temp_vector_69 else accumulation_vector_69
        temp_vector_70 = if not (temp_vector_69 ! (Proxy :: Proxy 127)) then temp_vector_69 >>. lit 1 else (temp_vector_69 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_71 = if input_b ! (Proxy :: Proxy 71) then accumulation_vector_70 ^ temp_vector_70 else accumulation_vector_70
        temp_vector_71 = if not (temp_vector_70 ! (Proxy :: Proxy 127)) then temp_vector_70 >>. lit 1 else (temp_vector_70 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_72 = if input_b ! (Proxy :: Proxy 72) then accumulation_vector_71 ^ temp_vector_71 else accumulation_vector_71
        temp_vector_72 = if not (temp_vector_71 ! (Proxy :: Proxy 127)) then temp_vector_71 >>. lit 1 else (temp_vector_71 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_73 = if input_b ! (Proxy :: Proxy 73) then accumulation_vector_72 ^ temp_vector_72 else accumulation_vector_72
        temp_vector_73 = if not (temp_vector_72 ! (Proxy :: Proxy 127)) then temp_vector_72 >>. lit 1 else (temp_vector_72 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_74 = if input_b ! (Proxy :: Proxy 74) then accumulation_vector_73 ^ temp_vector_73 else accumulation_vector_73
        temp_vector_74 = if not (temp_vector_73 ! (Proxy :: Proxy 127)) then temp_vector_73 >>. lit 1 else (temp_vector_73 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_75 = if input_b ! (Proxy :: Proxy 75) then accumulation_vector_74 ^ temp_vector_74 else accumulation_vector_74
        temp_vector_75 = if not (temp_vector_74 ! (Proxy :: Proxy 127)) then temp_vector_74 >>. lit 1 else (temp_vector_74 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_76 = if input_b ! (Proxy :: Proxy 76) then accumulation_vector_75 ^ temp_vector_75 else accumulation_vector_75
        temp_vector_76 = if not (temp_vector_75 ! (Proxy :: Proxy 127)) then temp_vector_75 >>. lit 1 else (temp_vector_75 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_77 = if input_b ! (Proxy :: Proxy 77) then accumulation_vector_76 ^ temp_vector_76 else accumulation_vector_76
        temp_vector_77 = if not (temp_vector_76 ! (Proxy :: Proxy 127)) then temp_vector_76 >>. lit 1 else (temp_vector_76 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_78 = if input_b ! (Proxy :: Proxy 78) then accumulation_vector_77 ^ temp_vector_77 else accumulation_vector_77
        temp_vector_78 = if not (temp_vector_77 ! (Proxy :: Proxy 127)) then temp_vector_77 >>. lit 1 else (temp_vector_77 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_79 = if input_b ! (Proxy :: Proxy 79) then accumulation_vector_78 ^ temp_vector_78 else accumulation_vector_78
        temp_vector_79 = if not (temp_vector_78 ! (Proxy :: Proxy 127)) then temp_vector_78 >>. lit 1 else (temp_vector_78 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_80 = if input_b ! (Proxy :: Proxy 80) then accumulation_vector_79 ^ temp_vector_79 else accumulation_vector_79
        temp_vector_80 = if not (temp_vector_79 ! (Proxy :: Proxy 127)) then temp_vector_79 >>. lit 1 else (temp_vector_79 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_81 = if input_b ! (Proxy :: Proxy 81) then accumulation_vector_80 ^ temp_vector_80 else accumulation_vector_80
        temp_vector_81 = if not (temp_vector_80 ! (Proxy :: Proxy 127)) then temp_vector_80 >>. lit 1 else (temp_vector_80 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_82 = if input_b ! (Proxy :: Proxy 82) then accumulation_vector_81 ^ temp_vector_81 else accumulation_vector_81
        temp_vector_82 = if not (temp_vector_81 ! (Proxy :: Proxy 127)) then temp_vector_81 >>. lit 1 else (temp_vector_81 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_83 = if input_b ! (Proxy :: Proxy 83) then accumulation_vector_82 ^ temp_vector_82 else accumulation_vector_82
        temp_vector_83 = if not (temp_vector_82 ! (Proxy :: Proxy 127)) then temp_vector_82 >>. lit 1 else (temp_vector_82 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_84 = if input_b ! (Proxy :: Proxy 84) then accumulation_vector_83 ^ temp_vector_83 else accumulation_vector_83
        temp_vector_84 = if not (temp_vector_83 ! (Proxy :: Proxy 127)) then temp_vector_83 >>. lit 1 else (temp_vector_83 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_85 = if input_b ! (Proxy :: Proxy 85) then accumulation_vector_84 ^ temp_vector_84 else accumulation_vector_84
        temp_vector_85 = if not (temp_vector_84 ! (Proxy :: Proxy 127)) then temp_vector_84 >>. lit 1 else (temp_vector_84 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_86 = if input_b ! (Proxy :: Proxy 86) then accumulation_vector_85 ^ temp_vector_85 else accumulation_vector_85
        temp_vector_86 = if not (temp_vector_85 ! (Proxy :: Proxy 127)) then temp_vector_85 >>. lit 1 else (temp_vector_85 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_87 = if input_b ! (Proxy :: Proxy 87) then accumulation_vector_86 ^ temp_vector_86 else accumulation_vector_86
        temp_vector_87 = if not (temp_vector_86 ! (Proxy :: Proxy 127)) then temp_vector_86 >>. lit 1 else (temp_vector_86 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_88 = if input_b ! (Proxy :: Proxy 88) then accumulation_vector_87 ^ temp_vector_87 else accumulation_vector_87
        temp_vector_88 = if not (temp_vector_87 ! (Proxy :: Proxy 127)) then temp_vector_87 >>. lit 1 else (temp_vector_87 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_89 = if input_b ! (Proxy :: Proxy 89) then accumulation_vector_88 ^ temp_vector_88 else accumulation_vector_88
        temp_vector_89 = if not (temp_vector_88 ! (Proxy :: Proxy 127)) then temp_vector_88 >>. lit 1 else (temp_vector_88 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_90 = if input_b ! (Proxy :: Proxy 90) then accumulation_vector_89 ^ temp_vector_89 else accumulation_vector_89
        temp_vector_90 = if not (temp_vector_89 ! (Proxy :: Proxy 127)) then temp_vector_89 >>. lit 1 else (temp_vector_89 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_91 = if input_b ! (Proxy :: Proxy 91) then accumulation_vector_90 ^ temp_vector_90 else accumulation_vector_90
        temp_vector_91 = if not (temp_vector_90 ! (Proxy :: Proxy 127)) then temp_vector_90 >>. lit 1 else (temp_vector_90 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_92 = if input_b ! (Proxy :: Proxy 92) then accumulation_vector_91 ^ temp_vector_91 else accumulation_vector_91
        temp_vector_92 = if not (temp_vector_91 ! (Proxy :: Proxy 127)) then temp_vector_91 >>. lit 1 else (temp_vector_91 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_93 = if input_b ! (Proxy :: Proxy 93) then accumulation_vector_92 ^ temp_vector_92 else accumulation_vector_92
        temp_vector_93 = if not (temp_vector_92 ! (Proxy :: Proxy 127)) then temp_vector_92 >>. lit 1 else (temp_vector_92 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_94 = if input_b ! (Proxy :: Proxy 94) then accumulation_vector_93 ^ temp_vector_93 else accumulation_vector_93
        temp_vector_94 = if not (temp_vector_93 ! (Proxy :: Proxy 127)) then temp_vector_93 >>. lit 1 else (temp_vector_93 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_95 = if input_b ! (Proxy :: Proxy 95) then accumulation_vector_94 ^ temp_vector_94 else accumulation_vector_94
        temp_vector_95 = if not (temp_vector_94 ! (Proxy :: Proxy 127)) then temp_vector_94 >>. lit 1 else (temp_vector_94 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_96 = if input_b ! (Proxy :: Proxy 96) then accumulation_vector_95 ^ temp_vector_95 else accumulation_vector_95
        temp_vector_96 = if not (temp_vector_95 ! (Proxy :: Proxy 127)) then temp_vector_95 >>. lit 1 else (temp_vector_95 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_97 = if input_b ! (Proxy :: Proxy 97) then accumulation_vector_96 ^ temp_vector_96 else accumulation_vector_96
        temp_vector_97 = if not (temp_vector_96 ! (Proxy :: Proxy 127)) then temp_vector_96 >>. lit 1 else (temp_vector_96 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_98 = if input_b ! (Proxy :: Proxy 98) then accumulation_vector_97 ^ temp_vector_97 else accumulation_vector_97
        temp_vector_98 = if not (temp_vector_97 ! (Proxy :: Proxy 127)) then temp_vector_97 >>. lit 1 else (temp_vector_97 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_99 = if input_b ! (Proxy :: Proxy 99) then accumulation_vector_98 ^ temp_vector_98 else accumulation_vector_98
        temp_vector_99 = if not (temp_vector_98 ! (Proxy :: Proxy 127)) then temp_vector_98 >>. lit 1 else (temp_vector_98 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_100 = if input_b ! (Proxy :: Proxy 100) then accumulation_vector_99 ^ temp_vector_99 else accumulation_vector_99
        temp_vector_100 = if not (temp_vector_99 ! (Proxy :: Proxy 127)) then temp_vector_99 >>. lit 1 else (temp_vector_99 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_101 = if input_b ! (Proxy :: Proxy 101) then accumulation_vector_100 ^ temp_vector_100 else accumulation_vector_100
        temp_vector_101 = if not (temp_vector_100 ! (Proxy :: Proxy 127)) then temp_vector_100 >>. lit 1 else (temp_vector_100 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_102 = if input_b ! (Proxy :: Proxy 102) then accumulation_vector_101 ^ temp_vector_101 else accumulation_vector_101
        temp_vector_102 = if not (temp_vector_101 ! (Proxy :: Proxy 127)) then temp_vector_101 >>. lit 1 else (temp_vector_101 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_103 = if input_b ! (Proxy :: Proxy 103) then accumulation_vector_102 ^ temp_vector_102 else accumulation_vector_102
        temp_vector_103 = if not (temp_vector_102 ! (Proxy :: Proxy 127)) then temp_vector_102 >>. lit 1 else (temp_vector_102 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_104 = if input_b ! (Proxy :: Proxy 104) then accumulation_vector_103 ^ temp_vector_103 else accumulation_vector_103
        temp_vector_104 = if not (temp_vector_103 ! (Proxy :: Proxy 127)) then temp_vector_103 >>. lit 1 else (temp_vector_103 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_105 = if input_b ! (Proxy :: Proxy 105) then accumulation_vector_104 ^ temp_vector_104 else accumulation_vector_104
        temp_vector_105 = if not (temp_vector_104 ! (Proxy :: Proxy 127)) then temp_vector_104 >>. lit 1 else (temp_vector_104 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_106 = if input_b ! (Proxy :: Proxy 106) then accumulation_vector_105 ^ temp_vector_105 else accumulation_vector_105
        temp_vector_106 = if not (temp_vector_105 ! (Proxy :: Proxy 127)) then temp_vector_105 >>. lit 1 else (temp_vector_105 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_107 = if input_b ! (Proxy :: Proxy 107) then accumulation_vector_106 ^ temp_vector_106 else accumulation_vector_106
        temp_vector_107 = if not (temp_vector_106 ! (Proxy :: Proxy 127)) then temp_vector_106 >>. lit 1 else (temp_vector_106 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_108 = if input_b ! (Proxy :: Proxy 108) then accumulation_vector_107 ^ temp_vector_107 else accumulation_vector_107
        temp_vector_108 = if not (temp_vector_107 ! (Proxy :: Proxy 127)) then temp_vector_107 >>. lit 1 else (temp_vector_107 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_109 = if input_b ! (Proxy :: Proxy 109) then accumulation_vector_108 ^ temp_vector_108 else accumulation_vector_108
        temp_vector_109 = if not (temp_vector_108 ! (Proxy :: Proxy 127)) then temp_vector_108 >>. lit 1 else (temp_vector_108 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_110 = if input_b ! (Proxy :: Proxy 110) then accumulation_vector_109 ^ temp_vector_109 else accumulation_vector_109
        temp_vector_110 = if not (temp_vector_109 ! (Proxy :: Proxy 127)) then temp_vector_109 >>. lit 1 else (temp_vector_109 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_111 = if input_b ! (Proxy :: Proxy 111) then accumulation_vector_110 ^ temp_vector_110 else accumulation_vector_110
        temp_vector_111 = if not (temp_vector_110 ! (Proxy :: Proxy 127)) then temp_vector_110 >>. lit 1 else (temp_vector_110 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_112 = if input_b ! (Proxy :: Proxy 112) then accumulation_vector_111 ^ temp_vector_111 else accumulation_vector_111
        temp_vector_112 = if not (temp_vector_111 ! (Proxy :: Proxy 127)) then temp_vector_111 >>. lit 1 else (temp_vector_111 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_113 = if input_b ! (Proxy :: Proxy 113) then accumulation_vector_112 ^ temp_vector_112 else accumulation_vector_112
        temp_vector_113 = if not (temp_vector_112 ! (Proxy :: Proxy 127)) then temp_vector_112 >>. lit 1 else (temp_vector_112 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_114 = if input_b ! (Proxy :: Proxy 114) then accumulation_vector_113 ^ temp_vector_113 else accumulation_vector_113
        temp_vector_114 = if not (temp_vector_113 ! (Proxy :: Proxy 127)) then temp_vector_113 >>. lit 1 else (temp_vector_113 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_115 = if input_b ! (Proxy :: Proxy 115) then accumulation_vector_114 ^ temp_vector_114 else accumulation_vector_114
        temp_vector_115 = if not (temp_vector_114 ! (Proxy :: Proxy 127)) then temp_vector_114 >>. lit 1 else (temp_vector_114 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_116 = if input_b ! (Proxy :: Proxy 116) then accumulation_vector_115 ^ temp_vector_115 else accumulation_vector_115
        temp_vector_116 = if not (temp_vector_115 ! (Proxy :: Proxy 127)) then temp_vector_115 >>. lit 1 else (temp_vector_115 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_117 = if input_b ! (Proxy :: Proxy 117) then accumulation_vector_116 ^ temp_vector_116 else accumulation_vector_116
        temp_vector_117 = if not (temp_vector_116 ! (Proxy :: Proxy 127)) then temp_vector_116 >>. lit 1 else (temp_vector_116 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_118 = if input_b ! (Proxy :: Proxy 118) then accumulation_vector_117 ^ temp_vector_117 else accumulation_vector_117
        temp_vector_118 = if not (temp_vector_117 ! (Proxy :: Proxy 127)) then temp_vector_117 >>. lit 1 else (temp_vector_117 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_119 = if input_b ! (Proxy :: Proxy 119) then accumulation_vector_118 ^ temp_vector_118 else accumulation_vector_118
        temp_vector_119 = if not (temp_vector_118 ! (Proxy :: Proxy 127)) then temp_vector_118 >>. lit 1 else (temp_vector_118 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_120 = if input_b ! (Proxy :: Proxy 120) then accumulation_vector_119 ^ temp_vector_119 else accumulation_vector_119
        temp_vector_120 = if not (temp_vector_119 ! (Proxy :: Proxy 127)) then temp_vector_119 >>. lit 1 else (temp_vector_119 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_121 = if input_b ! (Proxy :: Proxy 121) then accumulation_vector_120 ^ temp_vector_120 else accumulation_vector_120
        temp_vector_121 = if not (temp_vector_120 ! (Proxy :: Proxy 127)) then temp_vector_120 >>. lit 1 else (temp_vector_120 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_122 = if input_b ! (Proxy :: Proxy 122) then accumulation_vector_121 ^ temp_vector_121 else accumulation_vector_121
        temp_vector_122 = if not (temp_vector_121 ! (Proxy :: Proxy 127)) then temp_vector_121 >>. lit 1 else (temp_vector_121 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_123 = if input_b ! (Proxy :: Proxy 123) then accumulation_vector_122 ^ temp_vector_122 else accumulation_vector_122
        temp_vector_123 = if not (temp_vector_122 ! (Proxy :: Proxy 127)) then temp_vector_122 >>. lit 1 else (temp_vector_122 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_124 = if input_b ! (Proxy :: Proxy 124) then accumulation_vector_123 ^ temp_vector_123 else accumulation_vector_123
        temp_vector_124 = if not (temp_vector_123 ! (Proxy :: Proxy 127)) then temp_vector_123 >>. lit 1 else (temp_vector_123 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_125 = if input_b ! (Proxy :: Proxy 125) then accumulation_vector_124 ^ temp_vector_124 else accumulation_vector_124
        temp_vector_125 = if not (temp_vector_124 ! (Proxy :: Proxy 127)) then temp_vector_124 >>. lit 1 else (temp_vector_124 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_126 = if input_b ! (Proxy :: Proxy 126) then accumulation_vector_125 ^ temp_vector_125 else accumulation_vector_125
        temp_vector_126 = if not (temp_vector_125 ! (Proxy :: Proxy 127)) then temp_vector_125 >>. lit 1 else (temp_vector_125 >>. lit 1) ^ primitive_polynomial
        accumulation_vector_127 = if input_b ! (Proxy :: Proxy 127) then accumulation_vector_126 ^ temp_vector_126 else accumulation_vector_126
        -- final temp_vector line unneeded
    in
        accumulation_vector_127



multiplyFun :: (Multiplicand_i,Multiplier_i) -> Product_o
multiplyFun (a,b) = multiply a b

start :: Dev (Multiplicand_i,Multiplier_i) Product_o
start = iter multiplyFun initInput

main = undefined
