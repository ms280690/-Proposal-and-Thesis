{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleRead (module Curry.Module.OracleRead) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.Read
import Curry.Module.Char
import Curry.Module.Prelude
import Curry.Module.OracleChar
import Curry.Module.OraclePrelude



-- begin included



-- end included

c_readNat :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_readNat x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OracleRead.c_readNat'46readNatPrefix'463(Curry.Module.OraclePrelude.c_dropWhile(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleRead.c_readNat'46_'35lambda2))))(x2)(x1)(st))(Curry.Module.Prelude.C_Zero)(x3)(st))(st)



c_readNat'46readNatPrefix'463 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_readNat'46readNatPrefix'463 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRead.c__case_7(x3)(x2)(x1)(st))(st)



c_readNat'46_'35lambda2 :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_readNat'46_'35lambda2 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Char(' '))(x1)(st))(st)



c_readInt :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_readInt x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OracleRead.c_readInt'46readIntPrefix'4614(Curry.Module.OraclePrelude.c_dropWhile(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleRead.c_readInt'46_'35lambda3))))(x2)(x1)(st))(x3)(st))(st)



c_readInt'46readIntPrefix'4614 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_readInt'46readIntPrefix'4614 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRead.c__case_5(x2)(x1)(st))(st)



c_readInt'46_'35lambda3 :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_readInt'46_'35lambda3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Char(' '))(x1)(st))(st)



c_readHex :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_readHex x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OracleRead.c_readHex'46readHexPrefix'4622(Curry.Module.OraclePrelude.c_dropWhile(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleRead.c_readHex'46_'35lambda4))))(x2)(x1)(st))(Curry.Module.Prelude.C_Zero)(x3)(st))(st)



c_readHex'46hex2int'4622 :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_readHex'46hex2int'4622 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OracleRead.c__case_3(x2)(Curry.Module.OracleChar.c_isDigit(x2)(x1)(st))(x3)(st))(st)



c_readHex'46readHexPrefix'4622 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_readHex'46readHexPrefix'4622 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRead.c__case_1(x3)(x2)(x1)(st))(st)



c_readHex'46_'35lambda4 :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_readHex'46_'35lambda4 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Char(' '))(x1)(st))(st)



c__case_1 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRead.c__case_1_case__7(x1)(x3)(x2)(st))(st)



c__case_0 x3 x5 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRead.c__case_0_case__6(x1)(x3)(x5)(x6)(x7)(st))(st)



c__case_3 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRead.c__case_3_case__5(x1)(x2)(x3)(st))(st)



c__case_2 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRead.c__case_2_case__4(x1)(x2)(x3)(st))(st)



c__case_5 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRead.c__case_5_case__3(x1)(x2)(st))(st)



c__case_4 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRead.c__case_4_case__2(x1)(x3)(x4)(x5)(st))(st)



c__case_7 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRead.c__case_7_case__1(x1)(x3)(x2)(st))(st)



c__case_6 x3 x5 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRead.c__case_6_case__0(x1)(x3)(x5)(x6)(x7)(st))(st)



c__case_6_case__0 x1 x3 x5 x6 x7@Curry.Module.Prelude.C_True st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))))(Curry.Module.OracleRead.c_readNat'46readNatPrefix'463(x5)(Curry.Module.OraclePrelude.op_45(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_42(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(x1)(st))(x6)(x8)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('0'))(x9)(st))(x10)(st))(x11)(st))(st)
c__case_6_case__0 x1 x3 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_6_case__0 x1 x3 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRead.c__case_6_case__0(x1)(x3)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_6_case__0 x1 x3 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleRead._case_6_case__0")(x)



c__case_7_case__1 x1 x3 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_7_case__1 x1 x3 x2@((Curry.Module.Prelude.:<) x4 x5) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(let {x6 = Curry.Module.OraclePrelude.c_ord(x4)(x1)(st)} in let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))))(Curry.Module.OracleRead.c__case_6(x3)(x5)(x6)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_62_61(x6)(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('0'))(x7)(st))(x8)(st))(Curry.Module.OraclePrelude.op_60_61(x6)(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('9'))(x9)(st))(x10)(st))(x11)(st))(x12)(st))(st))(st)
c__case_7_case__1 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRead.c__case_7_case__1(x1)(x3)(x)(st))(i)(xs)(st)
c__case_7_case__1 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleRead._case_7_case__1")(x)



c__case_4_case__2 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_negate(Curry.Module.OracleRead.c_readNat(x4)(x1)(st))(x6)(st))(st)
c__case_4_case__2 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRead.c_readNat((Curry.Module.Prelude.:<)(x3)(x4))(x1)(st))(st)
c__case_4_case__2 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRead.c__case_4_case__2(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_4_case__2 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleRead._case_4_case__2")(x)



c__case_5_case__3 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Zero)(st)
c__case_5_case__3 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleRead.c__case_4(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('-'))(x1)(st))(x5)(st))(st)
c__case_5_case__3 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRead.c__case_5_case__3(x1)(x)(st))(i)(xs)(st)
c__case_5_case__3 x1 x st = Curry.RunTimeSystem.patternFail("OracleRead._case_5_case__3")(x)



c__case_2_case__4 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_45(Curry.Module.OraclePrelude.c_ord(x2)(x1)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('A'))(x4)(st))(x5)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(x6)(st))(st)
c__case_2_case__4 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_negate(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(st)
c__case_2_case__4 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRead.c__case_2_case__4(x1)(x2)(x)(st))(i)(xs)(st)
c__case_2_case__4 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleRead._case_2_case__4")(x)



c__case_3_case__5 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_45(Curry.Module.OraclePrelude.c_ord(x2)(x1)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('0'))(x4)(st))(x5)(st))(st)
c__case_3_case__5 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))))))(Curry.Module.OracleRead.c__case_2(x2)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_62_61(Curry.Module.OraclePrelude.c_ord(x2)(x1)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('A'))(x6)(st))(x7)(st))(Curry.Module.OraclePrelude.op_60_61(Curry.Module.OraclePrelude.c_ord(x2)(x8)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('F'))(x9)(st))(x10)(st))(x11)(st))(x12)(st))(st)
c__case_3_case__5 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRead.c__case_3_case__5(x1)(x2)(x)(st))(i)(xs)(st)
c__case_3_case__5 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleRead._case_3_case__5")(x)



c__case_0_case__6 x1 x3 x5 x6 x7@Curry.Module.Prelude.C_True st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OracleRead.c_readHex'46readHexPrefix'4622(x5)(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_42(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(x1)(st))(x6)(x8)(st))(x9)(st))(st)
c__case_0_case__6 x1 x3 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_0_case__6 x1 x3 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRead.c__case_0_case__6(x1)(x3)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_0_case__6 x1 x3 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleRead._case_0_case__6")(x)



c__case_1_case__7 x1 x3 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_1_case__7 x1 x3 x2@((Curry.Module.Prelude.:<) x4 x5) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(let {x6 = Curry.Module.OracleRead.c_readHex'46hex2int'4622(x4)(x1)(st)} in let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleRead.c__case_0(x3)(x5)(x6)(Curry.Module.OraclePrelude.op_62_61(x6)(Curry.Module.Prelude.C_Zero)(x7)(st))(x8)(st))(st))(st)
c__case_1_case__7 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRead.c__case_1_case__7(x1)(x3)(x)(st))(i)(xs)(st)
c__case_1_case__7 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleRead._case_1_case__7")(x)


