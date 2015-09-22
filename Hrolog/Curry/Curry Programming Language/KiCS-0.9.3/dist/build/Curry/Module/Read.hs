{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Read (module Curry.Module.Read) where

import Curry.RunTimeSystem
import Curry.Module.Char
import Curry.Module.Prelude



-- begin included



-- end included

c_readNat :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_readNat x1 st = Curry.Module.Read.c_readNat'46readNatPrefix'463(Curry.Module.Prelude.c_dropWhile(Curry.Module.Prelude.pf(Curry.Module.Read.c_readNat'46_'35lambda2))(x1)(st))(Curry.Module.Prelude.C_Zero)(st)



c_readNat'46readNatPrefix'463 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_readNat'46readNatPrefix'463 x1@Curry.Module.Prelude.List x2 st = x2
c_readNat'46readNatPrefix'463 x1@((Curry.Module.Prelude.:<) x3 x4) x2 st = let {x5 = Curry.Module.Prelude.c_ord(x3)(st)} in Curry.Module.Read.c_readNat'46readNatPrefix'463_case_4(x2)(x4)(x5)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_62_61(x5)(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('0'))(st))(st))(Curry.Module.Prelude.op_60_61(x5)(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('9'))(st))(st))(st))(st)
c_readNat'46readNatPrefix'463 (Curry.Module.Prelude.ListOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Read.c_readNat'46readNatPrefix'463(x)(x2)(st))(i)(xs)(st)
c_readNat'46readNatPrefix'463 x x2 st = Curry.RunTimeSystem.patternFail("Read.readNat.readNatPrefix.3")(x)



c_readNat'46_'35lambda2 :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_readNat'46_'35lambda2 x1 st = Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Char(' '))(st)



c_readInt :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_readInt x1 st = Curry.Module.Read.c_readInt'46readIntPrefix'4614(Curry.Module.Prelude.c_dropWhile(Curry.Module.Prelude.pf(Curry.Module.Read.c_readInt'46_'35lambda3))(x1)(st))(st)



c_readInt'46readIntPrefix'4614 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_readInt'46readIntPrefix'4614 x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Zero
c_readInt'46readIntPrefix'4614 x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.Read.c_readInt'46readIntPrefix'4614_case_3(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('-'))(st))(st)
c_readInt'46readIntPrefix'4614 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Read.c_readInt'46readIntPrefix'4614(x)(st))(i)(xs)(st)
c_readInt'46readIntPrefix'4614 x st = Curry.RunTimeSystem.patternFail("Read.readInt.readIntPrefix.14")(x)



c_readInt'46_'35lambda3 :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_readInt'46_'35lambda3 x1 st = Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Char(' '))(st)



c_readHex :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_readHex x1 st = Curry.Module.Read.c_readHex'46readHexPrefix'4622(Curry.Module.Prelude.c_dropWhile(Curry.Module.Prelude.pf(Curry.Module.Read.c_readHex'46_'35lambda4))(x1)(st))(Curry.Module.Prelude.C_Zero)(st)



c_readHex'46hex2int'4622 :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_readHex'46hex2int'4622 x1 st = Curry.Module.Read.c_readHex'46hex2int'4622_case_2(x1)(Curry.Module.Char.c_isDigit(x1)(st))(st)



c_readHex'46readHexPrefix'4622 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_readHex'46readHexPrefix'4622 x1@Curry.Module.Prelude.List x2 st = x2
c_readHex'46readHexPrefix'4622 x1@((Curry.Module.Prelude.:<) x3 x4) x2 st = let {x5 = Curry.Module.Read.c_readHex'46hex2int'4622(x3)(st)} in Curry.Module.Read.c_readHex'46readHexPrefix'4622_case_0(x2)(x4)(x5)(Curry.Module.Prelude.op_62_61(x5)(Curry.Module.Prelude.C_Zero)(st))(st)
c_readHex'46readHexPrefix'4622 (Curry.Module.Prelude.ListOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Read.c_readHex'46readHexPrefix'4622(x)(x2)(st))(i)(xs)(st)
c_readHex'46readHexPrefix'4622 x x2 st = Curry.RunTimeSystem.patternFail("Read.readHex.readHexPrefix.22")(x)



c_readHex'46_'35lambda4 :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_readHex'46_'35lambda4 x1 st = Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Char(' '))(st)



c_readHex'46readHexPrefix'4622_case_0 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.Read.c_readHex'46readHexPrefix'4622(x4)(Curry.Module.Prelude.op_43(Curry.Module.Prelude.op_42(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(st))(x5)(st))(st)
c_readHex'46readHexPrefix'4622_case_0 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = x2
c_readHex'46readHexPrefix'4622_case_0 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Read.c_readHex'46readHexPrefix'4622_case_0(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c_readHex'46readHexPrefix'4622_case_0 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("Read.readHex.readHexPrefix.22_case_0")(x)



c_readHex'46hex2int'4622_case_2 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_45(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('0'))(st))(st)
c_readHex'46hex2int'4622_case_2 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Read.c_readHex'46hex2int'4622_case_1(x1)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_62_61(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('A'))(st))(st))(Curry.Module.Prelude.op_60_61(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('F'))(st))(st))(st))(st)
c_readHex'46hex2int'4622_case_2 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Read.c_readHex'46hex2int'4622_case_2(x1)(x)(st))(i)(xs)(st)
c_readHex'46hex2int'4622_case_2 x1 x st = Curry.RunTimeSystem.patternFail("Read.readHex.hex2int.22_case_2")(x)



c_readHex'46hex2int'4622_case_1 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43(Curry.Module.Prelude.op_45(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('A'))(st))(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(st)
c_readHex'46hex2int'4622_case_1 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_negate(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st)
c_readHex'46hex2int'4622_case_1 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Read.c_readHex'46hex2int'4622_case_1(x1)(x)(st))(i)(xs)(st)
c_readHex'46hex2int'4622_case_1 x1 x st = Curry.RunTimeSystem.patternFail("Read.readHex.hex2int.22_case_1")(x)



c_readInt'46readIntPrefix'4614_case_3 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_negate(Curry.Module.Read.c_readNat(x3)(st))(st)
c_readInt'46readIntPrefix'4614_case_3 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.Read.c_readNat((Curry.Module.Prelude.:<)(x2)(x3))(st)
c_readInt'46readIntPrefix'4614_case_3 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Read.c_readInt'46readIntPrefix'4614_case_3(x2)(x3)(x)(st))(i)(xs)(st)
c_readInt'46readIntPrefix'4614_case_3 x2 x3 x st = Curry.RunTimeSystem.patternFail("Read.readInt.readIntPrefix.14_case_3")(x)



c_readNat'46readNatPrefix'463_case_4 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.Read.c_readNat'46readNatPrefix'463(x4)(Curry.Module.Prelude.op_45(Curry.Module.Prelude.op_43(Curry.Module.Prelude.op_42(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(st))(x5)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('0'))(st))(st))(st)
c_readNat'46readNatPrefix'463_case_4 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = x2
c_readNat'46readNatPrefix'463_case_4 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Read.c_readNat'46readNatPrefix'463_case_4(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c_readNat'46readNatPrefix'463_case_4 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("Read.readNat.readNatPrefix.3_case_4")(x)


