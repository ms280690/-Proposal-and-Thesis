{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Char (module Curry.Module.Char) where

import Curry.RunTimeSystem
import Curry.Module.Prelude



-- begin included



-- end included

c_isUpper :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isUpper x1 st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_62_61(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('A'))(st))(st))(Curry.Module.Prelude.op_60_61(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('Z'))(st))(st))(st)



c_isLower :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isLower x1 st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_62_61(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('a'))(st))(st))(Curry.Module.Prelude.op_60_61(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('z'))(st))(st))(st)



c_isAlpha :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isAlpha x1 st = Curry.Module.Prelude.op_124_124(Curry.Module.Char.c_isUpper(x1)(st))(Curry.Module.Char.c_isLower(x1)(st))(st)



c_isDigit :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isDigit x1 st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_60_61(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('0'))(st))(Curry.Module.Prelude.c_ord(x1)(st))(st))(Curry.Module.Prelude.op_60_61(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('9'))(st))(st))(st)



c_isAlphaNum :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isAlphaNum x1 st = Curry.Module.Prelude.op_124_124(Curry.Module.Char.c_isAlpha(x1)(st))(Curry.Module.Char.c_isDigit(x1)(st))(st)



c_isOctDigit :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isOctDigit x1 st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_62_61(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('0'))(st))(st))(Curry.Module.Prelude.op_60_61(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('7'))(st))(st))(st)



c_isHexDigit :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isHexDigit x1 st = Curry.Module.Prelude.op_124_124(Curry.Module.Char.c_isDigit(x1)(st))(Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_62_61(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('A'))(st))(st))(Curry.Module.Prelude.op_60_61(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('F'))(st))(st))(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_62_61(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('a'))(st))(st))(Curry.Module.Prelude.op_60_61(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('f'))(st))(st))(st))(st))(st)



c_isSpace :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isSpace x1 st = Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Char(' '))(st))(Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Char('\t'))(st))(Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Char('\n'))(st))(Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Char('\r'))(st))(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))(st))(st))(st))(st))(st)



c_toUpper :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_toUpper x1 st = Curry.Module.Char.c_toUpper_case_10(x1)(Curry.Module.Char.c_isLower(x1)(st))(st)



c_toLower :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_toLower x1 st = Curry.Module.Char.c_toLower_case_8(x1)(Curry.Module.Char.c_isUpper(x1)(st))(st)



c_digitToInt :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_digitToInt x1 st = Curry.Module.Char.c_digitToInt_case_6(x1)(Curry.Module.Char.c_isDigit(x1)(st))(st)



c_intToDigit :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_intToDigit x1 st = Curry.Module.Char.c_intToDigit_case_2(x1)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_62_61(x1)(Curry.Module.Prelude.C_Zero)(st))(Curry.Module.Prelude.op_60_61(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(st))(st))(st)



c_intToDigit_case_2 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.op_43(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('0'))(st))(x1)(st))(st)
c_intToDigit_case_2 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Char.c_intToDigit_case_1(x1)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_62_61(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(st))(Curry.Module.Prelude.op_60_61(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))(st))(st))(st)
c_intToDigit_case_2 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Char.c_intToDigit_case_2(x1)(x)(st))(i)(xs)(st)
c_intToDigit_case_2 x1 x st = Curry.RunTimeSystem.patternFail("Char.intToDigit_case_2")(x)



c_intToDigit_case_1 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.op_45(Curry.Module.Prelude.op_43(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('A'))(st))(x1)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(st))(st)
c_intToDigit_case_1 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Char.c_intToDigit_case_0(Curry.Module.Prelude.c_otherwise(st))(st)
c_intToDigit_case_1 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Char.c_intToDigit_case_1(x1)(x)(st))(i)(xs)(st)
c_intToDigit_case_1 x1 x st = Curry.RunTimeSystem.patternFail("Char.intToDigit_case_1")(x)



c_intToDigit_case_0 x1@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))))))))))))(st)
c_intToDigit_case_0 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Char.c_intToDigit_case_0(x)(st))(i)(xs)(st)
c_intToDigit_case_0 x st = Curry.RunTimeSystem.patternFail("Char.intToDigit_case_0")(x)



c_digitToInt_case_6 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_45(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('0'))(st))(st)
c_digitToInt_case_6 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Char.c_digitToInt_case_5(x1)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_62_61(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('A'))(st))(st))(Curry.Module.Prelude.op_60_61(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('F'))(st))(st))(st))(st)
c_digitToInt_case_6 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Char.c_digitToInt_case_6(x1)(x)(st))(i)(xs)(st)
c_digitToInt_case_6 x1 x st = Curry.RunTimeSystem.patternFail("Char.digitToInt_case_6")(x)



c_digitToInt_case_5 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43(Curry.Module.Prelude.op_45(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('A'))(st))(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(st)
c_digitToInt_case_5 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Char.c_digitToInt_case_4(x1)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_62_61(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('a'))(st))(st))(Curry.Module.Prelude.op_60_61(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('f'))(st))(st))(st))(st)
c_digitToInt_case_5 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Char.c_digitToInt_case_5(x1)(x)(st))(i)(xs)(st)
c_digitToInt_case_5 x1 x st = Curry.RunTimeSystem.patternFail("Char.digitToInt_case_5")(x)



c_digitToInt_case_4 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43(Curry.Module.Prelude.op_45(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('a'))(st))(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(st)
c_digitToInt_case_4 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Char.c_digitToInt_case_3(Curry.Module.Prelude.c_otherwise(st))(st)
c_digitToInt_case_4 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Char.c_digitToInt_case_4(x1)(x)(st))(i)(xs)(st)
c_digitToInt_case_4 x1 x st = Curry.RunTimeSystem.patternFail("Char.digitToInt_case_4")(x)



c_digitToInt_case_3 x1@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(st)
c_digitToInt_case_3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Char.c_digitToInt_case_3(x)(st))(i)(xs)(st)
c_digitToInt_case_3 x st = Curry.RunTimeSystem.patternFail("Char.digitToInt_case_3")(x)



c_toLower_case_8 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.op_43(Curry.Module.Prelude.op_45(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('A'))(st))(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('a'))(st))(st))(st)
c_toLower_case_8 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Char.c_toLower_case_7(x1)(Curry.Module.Prelude.c_otherwise(st))(st)
c_toLower_case_8 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Char.c_toLower_case_8(x1)(x)(st))(i)(xs)(st)
c_toLower_case_8 x1 x st = Curry.RunTimeSystem.patternFail("Char.toLower_case_8")(x)



c_toLower_case_7 x1 x2@Curry.Module.Prelude.C_True st = x1
c_toLower_case_7 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Char.c_toLower_case_7(x1)(x)(st))(i)(xs)(st)
c_toLower_case_7 x1 x st = Curry.RunTimeSystem.patternFail("Char.toLower_case_7")(x)



c_toUpper_case_10 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.op_43(Curry.Module.Prelude.op_45(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('a'))(st))(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('A'))(st))(st))(st)
c_toUpper_case_10 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Char.c_toUpper_case_9(x1)(Curry.Module.Prelude.c_otherwise(st))(st)
c_toUpper_case_10 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Char.c_toUpper_case_10(x1)(x)(st))(i)(xs)(st)
c_toUpper_case_10 x1 x st = Curry.RunTimeSystem.patternFail("Char.toUpper_case_10")(x)



c_toUpper_case_9 x1 x2@Curry.Module.Prelude.C_True st = x1
c_toUpper_case_9 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Char.c_toUpper_case_9(x1)(x)(st))(i)(xs)(st)
c_toUpper_case_9 x1 x st = Curry.RunTimeSystem.patternFail("Char.toUpper_case_9")(x)


