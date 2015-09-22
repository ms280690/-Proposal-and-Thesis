{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleChar (module Curry.Module.OracleChar) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.Char
import Curry.Module.Prelude
import Curry.Module.OraclePrelude



-- begin included



-- end included

c_isUpper :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isUpper x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_62_61(Curry.Module.OraclePrelude.c_ord(x2)(x1)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('A'))(x3)(st))(x4)(st))(Curry.Module.OraclePrelude.op_60_61(Curry.Module.OraclePrelude.c_ord(x2)(x5)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('Z'))(x6)(st))(x7)(st))(x8)(st))(st)



c_isLower :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isLower x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_62_61(Curry.Module.OraclePrelude.c_ord(x2)(x1)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('a'))(x3)(st))(x4)(st))(Curry.Module.OraclePrelude.op_60_61(Curry.Module.OraclePrelude.c_ord(x2)(x5)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('z'))(x6)(st))(x7)(st))(x8)(st))(st)



c_isAlpha :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isAlpha x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OracleChar.c_isUpper(x2)(x1)(st))(Curry.Module.OracleChar.c_isLower(x2)(x3)(st))(x4)(st))(st)



c_isDigit :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isDigit x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_60_61(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('0'))(x1)(st))(Curry.Module.OraclePrelude.c_ord(x2)(x3)(st))(x4)(st))(Curry.Module.OraclePrelude.op_60_61(Curry.Module.OraclePrelude.c_ord(x2)(x5)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('9'))(x6)(st))(x7)(st))(x8)(st))(st)



c_isAlphaNum :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isAlphaNum x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OracleChar.c_isAlpha(x2)(x1)(st))(Curry.Module.OracleChar.c_isDigit(x2)(x3)(st))(x4)(st))(st)



c_isOctDigit :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isOctDigit x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_62_61(Curry.Module.OraclePrelude.c_ord(x2)(x1)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('0'))(x3)(st))(x4)(st))(Curry.Module.OraclePrelude.op_60_61(Curry.Module.OraclePrelude.c_ord(x2)(x5)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('7'))(x6)(st))(x7)(st))(x8)(st))(st)



c_isHexDigit :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isHexDigit x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List)))))))))))))))))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OracleChar.c_isDigit(x2)(x1)(st))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_62_61(Curry.Module.OraclePrelude.c_ord(x2)(x3)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('A'))(x4)(st))(x5)(st))(Curry.Module.OraclePrelude.op_60_61(Curry.Module.OraclePrelude.c_ord(x2)(x6)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('F'))(x7)(st))(x8)(st))(x9)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_62_61(Curry.Module.OraclePrelude.c_ord(x2)(x10)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('a'))(x11)(st))(x12)(st))(Curry.Module.OraclePrelude.op_60_61(Curry.Module.OraclePrelude.c_ord(x2)(x13)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('f'))(x14)(st))(x15)(st))(x16)(st))(x17)(st))(x18)(st))(st)



c_isSpace :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isSpace x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))))))))))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Char(' '))(x1)(st))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\t'))(x3)(st))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\n'))(x4)(st))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\r'))(x5)(st))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_ord(x2)(x6)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))(x7)(st))(x8)(st))(x9)(st))(x10)(st))(x11)(st))(st)



c_toUpper :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_toUpper x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OracleChar.c__case_10(x2)(Curry.Module.OracleChar.c_isLower(x2)(x1)(st))(x3)(st))(st)



c_toLower :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_toLower x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OracleChar.c__case_8(x2)(Curry.Module.OracleChar.c_isUpper(x2)(x1)(st))(x3)(st))(st)



c_digitToInt :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_digitToInt x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OracleChar.c__case_6(x2)(Curry.Module.OracleChar.c_isDigit(x2)(x1)(st))(x3)(st))(st)



c_intToDigit :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_intToDigit x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))))(Curry.Module.OracleChar.c__case_2(x2)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_62_61(x2)(Curry.Module.Prelude.C_Zero)(x1)(st))(Curry.Module.OraclePrelude.op_60_61(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(x3)(st))(x4)(st))(x5)(st))(st)



c__case_2 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleChar.c__case_2_case__10(x1)(x2)(x3)(st))(st)



c__case_1 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleChar.c__case_1_case__9(x1)(x2)(x3)(st))(st)



c__case_0 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleChar.c__case_0_case__8(x1)(x2)(st))(st)



c__case_6 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleChar.c__case_6_case__7(x1)(x2)(x3)(st))(st)



c__case_5 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleChar.c__case_5_case__6(x1)(x2)(x3)(st))(st)



c__case_4 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleChar.c__case_4_case__5(x1)(x2)(x3)(st))(st)



c__case_3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleChar.c__case_3_case__4(x1)(x2)(st))(st)



c__case_8 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleChar.c__case_8_case__3(x1)(x2)(x3)(st))(st)



c__case_7 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleChar.c__case_7_case__2(x1)(x2)(x3)(st))(st)



c__case_10 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleChar.c__case_10_case__1(x1)(x2)(x3)(st))(st)



c__case_9 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleChar.c__case_9_case__0(x1)(x2)(x3)(st))(st)



c__case_9_case__0 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_9_case__0 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_9_case__0 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleChar.c__case_9_case__0(x1)(x2)(x)(st))(i)(xs)(st)
c__case_9_case__0 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleChar._case_9_case__0")(x)



c__case_10_case__1 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.c_chr(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_45(Curry.Module.OraclePrelude.c_ord(x2)(x1)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('a'))(x4)(st))(x5)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('A'))(x6)(st))(x7)(st))(x8)(st))(st)
c__case_10_case__1 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OracleChar.c__case_9(x2)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x9)(st))(st)
c__case_10_case__1 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleChar.c__case_10_case__1(x1)(x2)(x)(st))(i)(xs)(st)
c__case_10_case__1 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleChar._case_10_case__1")(x)



c__case_7_case__2 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_7_case__2 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_7_case__2 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleChar.c__case_7_case__2(x1)(x2)(x)(st))(i)(xs)(st)
c__case_7_case__2 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleChar._case_7_case__2")(x)



c__case_8_case__3 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.c_chr(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_45(Curry.Module.OraclePrelude.c_ord(x2)(x1)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('A'))(x4)(st))(x5)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('a'))(x6)(st))(x7)(st))(x8)(st))(st)
c__case_8_case__3 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OracleChar.c__case_7(x2)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x9)(st))(st)
c__case_8_case__3 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleChar.c__case_8_case__3(x1)(x2)(x)(st))(i)(xs)(st)
c__case_8_case__3 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleChar._case_8_case__3")(x)



c__case_3_case__4 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(x1)(st))(st)
c__case_3_case__4 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_3_case__4 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleChar.c__case_3_case__4(x1)(x)(st))(i)(xs)(st)
c__case_3_case__4 x1 x st = Curry.RunTimeSystem.patternFail("OracleChar._case_3_case__4")(x)



c__case_4_case__5 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_45(Curry.Module.OraclePrelude.c_ord(x2)(x1)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('a'))(x4)(st))(x5)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(x6)(st))(st)
c__case_4_case__5 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleChar.c__case_3(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x7)(st))(st)
c__case_4_case__5 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleChar.c__case_4_case__5(x1)(x2)(x)(st))(i)(xs)(st)
c__case_4_case__5 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleChar._case_4_case__5")(x)



c__case_5_case__6 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_45(Curry.Module.OraclePrelude.c_ord(x2)(x1)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('A'))(x4)(st))(x5)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(x6)(st))(st)
c__case_5_case__6 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))))))))(Curry.Module.OracleChar.c__case_4(x2)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_62_61(Curry.Module.OraclePrelude.c_ord(x2)(x1)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('a'))(x7)(st))(x8)(st))(Curry.Module.OraclePrelude.op_60_61(Curry.Module.OraclePrelude.c_ord(x2)(x9)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('f'))(x10)(st))(x11)(st))(x12)(st))(x13)(st))(st)
c__case_5_case__6 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleChar.c__case_5_case__6(x1)(x2)(x)(st))(i)(xs)(st)
c__case_5_case__6 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleChar._case_5_case__6")(x)



c__case_6_case__7 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_45(Curry.Module.OraclePrelude.c_ord(x2)(x1)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('0'))(x4)(st))(x5)(st))(st)
c__case_6_case__7 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))))))(Curry.Module.OracleChar.c__case_5(x2)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_62_61(Curry.Module.OraclePrelude.c_ord(x2)(x1)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('A'))(x6)(st))(x7)(st))(Curry.Module.OraclePrelude.op_60_61(Curry.Module.OraclePrelude.c_ord(x2)(x8)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('F'))(x9)(st))(x10)(st))(x11)(st))(x12)(st))(st)
c__case_6_case__7 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleChar.c__case_6_case__7(x1)(x2)(x)(st))(i)(xs)(st)
c__case_6_case__7 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleChar._case_6_case__7")(x)



c__case_0_case__8 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))))))))))))(x1)(st))(st)
c__case_0_case__8 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_0_case__8 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleChar.c__case_0_case__8(x1)(x)(st))(i)(xs)(st)
c__case_0_case__8 x1 x st = Curry.RunTimeSystem.patternFail("OracleChar._case_0_case__8")(x)



c__case_1_case__9 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.c_chr(Curry.Module.OraclePrelude.op_45(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('A'))(x1)(st))(x2)(x4)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(x5)(st))(x6)(st))(st)
c__case_1_case__9 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleChar.c__case_0(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x7)(st))(st)
c__case_1_case__9 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleChar.c__case_1_case__9(x1)(x2)(x)(st))(i)(xs)(st)
c__case_1_case__9 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleChar._case_1_case__9")(x)



c__case_2_case__10 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_chr(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('0'))(x1)(st))(x2)(x4)(st))(x5)(st))(st)
c__case_2_case__10 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))))(Curry.Module.OracleChar.c__case_1(x2)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_62_61(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(x1)(st))(Curry.Module.OraclePrelude.op_60_61(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))(x6)(st))(x7)(st))(x8)(st))(st)
c__case_2_case__10 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleChar.c__case_2_case__10(x1)(x2)(x)(st))(i)(xs)(st)
c__case_2_case__10 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleChar._case_2_case__10")(x)


