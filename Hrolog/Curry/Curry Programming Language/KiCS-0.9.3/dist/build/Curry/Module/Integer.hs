{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Integer (module Curry.Module.Integer) where

import Curry.RunTimeSystem
import Curry.Module.Prelude



-- begin included



-- end included

c_pow :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_pow x1 x2 st = Curry.Module.Integer.c_pow_case_25(x1)(x2)(Curry.Module.Prelude.op_62_61(x2)(Curry.Module.Prelude.C_Zero)(st))(st)



c_pow'46powaux'463 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_pow'46powaux'463 x1 x2 x3 st = Curry.Module.Integer.c_pow'46powaux'463_case_24(x1)(x2)(x3)(Curry.Module.Prelude.op_61_61(x3)(Curry.Module.Prelude.C_Zero)(st))(st)



c_ilog :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ilog x1 st = Curry.Module.Integer.c_ilog_case_22(x1)(Curry.Module.Prelude.op_62(x1)(Curry.Module.Prelude.C_Zero)(st))(st)



c_isqrt :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_isqrt x1 st = Curry.Module.Integer.c_isqrt_case_20(x1)(Curry.Module.Prelude.op_62_61(x1)(Curry.Module.Prelude.C_Zero)(st))(st)



c_isqrt'46aux'4621 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_isqrt'46aux'4621 x1 x2 x3 st = Curry.Module.Integer.c_isqrt'46aux'4621_case_17(x1)(x2)(x3)(Curry.Module.Prelude.op_61_61(x3)(Curry.Module.Prelude.op_43(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st)



c_factorial :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_factorial x1 st = Curry.Module.Integer.c_factorial_case_15(x1)(Curry.Module.Prelude.op_62_61(x1)(Curry.Module.Prelude.C_Zero)(st))(st)



c_binomial :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_binomial x1 x2 st = Curry.Module.Integer.c_binomial_case_13(x1)(x2)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_62(x2)(Curry.Module.Prelude.C_Zero)(st))(Curry.Module.Prelude.op_62_61(x1)(x2)(st))(st))(st)



c_binomial'46aux'4641 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_binomial'46aux'4641 x1 x2 st = Curry.Module.Integer.c_binomial'46aux'4641_case_12(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Zero)(st))(st)



c_abs :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_abs x1 st = Curry.Module.Integer.c_abs_case_11(x1)(Curry.Module.Prelude.op_60(x1)(Curry.Module.Prelude.C_Zero)(st))(st)



c_max :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_max x1 x2 st = Curry.Module.Integer.c_max_case_10(x1)(x2)(Curry.Module.Prelude.op_60(x1)(x2)(st))(st)



c_min :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_min x1 x2 st = Curry.Module.Integer.c_min_case_9(x1)(x2)(Curry.Module.Prelude.op_60(x1)(x2)(st))(st)



c_max3 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_max3 x1 x2 x3 st = Curry.Module.Integer.c_max(x1)(Curry.Module.Integer.c_max(x2)(x3)(st))(st)



c_min3 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_min3 x1 x2 x3 st = Curry.Module.Integer.c_min(x1)(Curry.Module.Integer.c_min(x2)(x3)(st))(st)



c_maxlist :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_maxlist x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.Integer.c_maxlist_case_8(x2)(x3)(st)
c_maxlist (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_maxlist(x)(st))(i)(xs)(st)
c_maxlist x st = Curry.RunTimeSystem.patternFail("Integer.maxlist")(x)



c_minlist :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_minlist x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.Integer.c_minlist_case_7(x2)(x3)(st)
c_minlist (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_minlist(x)(st))(i)(xs)(st)
c_minlist x st = Curry.RunTimeSystem.patternFail("Integer.minlist")(x)



c_bitTrunc :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_bitTrunc x1 x2 st = Curry.Module.Integer.c_bitAnd(Curry.Module.Prelude.op_45(Curry.Module.Integer.c_pow(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x2)(st)



c_bitAnd :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_bitAnd x1 x2 st = Curry.Module.Integer.c_bitAnd_case_6(x1)(x2)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Zero)(st))(st)



c_bitOr :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_bitOr x1 x2 st = Curry.Module.Integer.c_bitOr_case_4(x1)(x2)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Zero)(st))(st)



c_bitNot :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_bitNot x1 st = Curry.Module.Integer.c_bitNot'46aux'46100(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))(x1)(st)



c_bitNot'46aux'46100 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_bitNot'46aux'46100 x1 x2 st = Curry.Module.Integer.c_bitNot'46aux'46100_case_2(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Zero)(st))(st)



c_bitXor :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_bitXor x1 x2 st = Curry.Module.Integer.c_bitXor_case_1(x1)(x2)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Zero)(st))(st)



c_even :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_even x1 st = Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_mod(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(Curry.Module.Prelude.C_Zero)(st)



c_odd :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_odd x1 st = Curry.Module.Prelude.op_47_61(Curry.Module.Prelude.c_mod(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(Curry.Module.Prelude.C_Zero)(st)



c_bitXor_case_1 x1 x2 x3@Curry.Module.Prelude.C_True st = x1
c_bitXor_case_1 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_43(Curry.Module.Prelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(Curry.Module.Integer.c_bitXor(Curry.Module.Prelude.c_div(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(Curry.Module.Prelude.c_div(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(st))(st))(Curry.Module.Integer.c_bitXor_case_0(x1)(x2)(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_mod(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(Curry.Module.Prelude.c_mod(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(st))(st))(st)
c_bitXor_case_1 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_bitXor_case_1(x1)(x2)(x)(st))(i)(xs)(st)
c_bitXor_case_1 x1 x2 x st = Curry.RunTimeSystem.patternFail("Integer.bitXor_case_1")(x)



c_bitXor_case_0 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_Zero
c_bitXor_case_0 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi)
c_bitXor_case_0 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_bitXor_case_0(x1)(x2)(x)(st))(i)(xs)(st)
c_bitXor_case_0 x1 x2 x st = Curry.RunTimeSystem.patternFail("Integer.bitXor_case_0")(x)



c_bitNot'46aux'46100_case_2 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_Zero
c_bitNot'46aux'46100_case_2 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_43(Curry.Module.Prelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(Curry.Module.Integer.c_bitNot'46aux'46100(Curry.Module.Prelude.op_45(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.Prelude.c_div(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(st))(st))(Curry.Module.Prelude.op_45(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(Curry.Module.Prelude.c_mod(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(st))(st)
c_bitNot'46aux'46100_case_2 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_bitNot'46aux'46100_case_2(x1)(x2)(x)(st))(i)(xs)(st)
c_bitNot'46aux'46100_case_2 x1 x2 x st = Curry.RunTimeSystem.patternFail("Integer.bitNot.aux.100_case_2")(x)



c_bitOr_case_4 x1 x2 x3@Curry.Module.Prelude.C_True st = x1
c_bitOr_case_4 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_43(Curry.Module.Prelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(Curry.Module.Integer.c_bitOr(Curry.Module.Prelude.c_div(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(Curry.Module.Prelude.c_div(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(st))(st))(Curry.Module.Integer.c_bitOr_case_3(x1)(x2)(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_mod(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st)
c_bitOr_case_4 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_bitOr_case_4(x1)(x2)(x)(st))(i)(xs)(st)
c_bitOr_case_4 x1 x2 x st = Curry.RunTimeSystem.patternFail("Integer.bitOr_case_4")(x)



c_bitOr_case_3 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi)
c_bitOr_case_3 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_mod(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st)
c_bitOr_case_3 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_bitOr_case_3(x1)(x2)(x)(st))(i)(xs)(st)
c_bitOr_case_3 x1 x2 x st = Curry.RunTimeSystem.patternFail("Integer.bitOr_case_3")(x)



c_bitAnd_case_6 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_Zero
c_bitAnd_case_6 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_43(Curry.Module.Prelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(Curry.Module.Integer.c_bitAnd(Curry.Module.Prelude.c_div(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(Curry.Module.Prelude.c_div(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(st))(st))(Curry.Module.Integer.c_bitAnd_case_5(x1)(x2)(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_mod(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(Curry.Module.Prelude.C_Zero)(st))(st))(st)
c_bitAnd_case_6 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_bitAnd_case_6(x1)(x2)(x)(st))(i)(xs)(st)
c_bitAnd_case_6 x1 x2 x st = Curry.RunTimeSystem.patternFail("Integer.bitAnd_case_6")(x)



c_bitAnd_case_5 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_Zero
c_bitAnd_case_5 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_mod(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st)
c_bitAnd_case_5 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_bitAnd_case_5(x1)(x2)(x)(st))(i)(xs)(st)
c_bitAnd_case_5 x1 x2 x st = Curry.RunTimeSystem.patternFail("Integer.bitAnd_case_5")(x)



c_minlist_case_7 x2 x3@Curry.Module.Prelude.List st = x2
c_minlist_case_7 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.Integer.c_min(x2)(Curry.Module.Integer.c_minlist((Curry.Module.Prelude.:<)(x4)(x5))(st))(st)
c_minlist_case_7 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_minlist_case_7(x2)(x)(st))(i)(xs)(st)
c_minlist_case_7 x2 x st = Curry.RunTimeSystem.patternFail("Integer.minlist_case_7")(x)



c_maxlist_case_8 x2 x3@Curry.Module.Prelude.List st = x2
c_maxlist_case_8 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.Integer.c_max(x2)(Curry.Module.Integer.c_maxlist((Curry.Module.Prelude.:<)(x4)(x5))(st))(st)
c_maxlist_case_8 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_maxlist_case_8(x2)(x)(st))(i)(xs)(st)
c_maxlist_case_8 x2 x st = Curry.RunTimeSystem.patternFail("Integer.maxlist_case_8")(x)



c_min_case_9 x1 x2 x3@Curry.Module.Prelude.C_True st = x1
c_min_case_9 x1 x2 x3@Curry.Module.Prelude.C_False st = x2
c_min_case_9 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_min_case_9(x1)(x2)(x)(st))(i)(xs)(st)
c_min_case_9 x1 x2 x st = Curry.RunTimeSystem.patternFail("Integer.min_case_9")(x)



c_max_case_10 x1 x2 x3@Curry.Module.Prelude.C_True st = x2
c_max_case_10 x1 x2 x3@Curry.Module.Prelude.C_False st = x1
c_max_case_10 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_max_case_10(x1)(x2)(x)(st))(i)(xs)(st)
c_max_case_10 x1 x2 x st = Curry.RunTimeSystem.patternFail("Integer.max_case_10")(x)



c_abs_case_11 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_negate(x1)(st)
c_abs_case_11 x1 x2@Curry.Module.Prelude.C_False st = x1
c_abs_case_11 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_abs_case_11(x1)(x)(st))(i)(xs)(st)
c_abs_case_11 x1 x st = Curry.RunTimeSystem.patternFail("Integer.abs_case_11")(x)



c_binomial'46aux'4641_case_12 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi)
c_binomial'46aux'4641_case_12 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_42(x2)(Curry.Module.Integer.c_binomial'46aux'4641(Curry.Module.Prelude.op_45(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.Prelude.op_45(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st)
c_binomial'46aux'4641_case_12 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_binomial'46aux'4641_case_12(x1)(x2)(x)(st))(i)(xs)(st)
c_binomial'46aux'4641_case_12 x1 x2 x st = Curry.RunTimeSystem.patternFail("Integer.binomial.aux.41_case_12")(x)



c_binomial_case_13 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_div(Curry.Module.Integer.c_binomial'46aux'4641(x2)(x1)(st))(Curry.Module.Integer.c_factorial(x2)(st))(st)
c_binomial_case_13 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_binomial_case_13(x1)(x2)(x)(st))(i)(xs)(st)
c_binomial_case_13 x1 x2 x st = Curry.RunTimeSystem.patternFail("Integer.binomial_case_13")(x)



c_factorial_case_15 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Integer.c_factorial_case_14(x1)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Zero)(st))(st)
c_factorial_case_15 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_factorial_case_15(x1)(x)(st))(i)(xs)(st)
c_factorial_case_15 x1 x st = Curry.RunTimeSystem.patternFail("Integer.factorial_case_15")(x)



c_factorial_case_14 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi)
c_factorial_case_14 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_42(x1)(Curry.Module.Integer.c_factorial(Curry.Module.Prelude.op_45(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st)
c_factorial_case_14 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_factorial_case_14(x1)(x)(st))(i)(xs)(st)
c_factorial_case_14 x1 x st = Curry.RunTimeSystem.patternFail("Integer.factorial_case_14")(x)



c_isqrt'46aux'4621_case_17 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = x2
c_isqrt'46aux'4621_case_17 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x4 = Curry.Module.Prelude.c_div(Curry.Module.Prelude.op_43(x3)(x2)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st)} in Curry.Module.Integer.c_isqrt'46aux'4621_case_16(x1)(x2)(x3)(x4)(Curry.Module.Prelude.op_62(Curry.Module.Prelude.op_42(x4)(x4)(st))(x1)(st))(st)
c_isqrt'46aux'4621_case_17 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_isqrt'46aux'4621_case_17(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_isqrt'46aux'4621_case_17 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("Integer.isqrt.aux.21_case_17")(x)



c_isqrt'46aux'4621_case_16 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Integer.c_isqrt'46aux'4621(x1)(x2)(x4)(st)
c_isqrt'46aux'4621_case_16 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Integer.c_isqrt'46aux'4621(x1)(x4)(x3)(st)
c_isqrt'46aux'4621_case_16 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_isqrt'46aux'4621_case_16(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_isqrt'46aux'4621_case_16 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("Integer.isqrt.aux.21_case_16")(x)



c_isqrt_case_20 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Integer.c_isqrt_case_19(x1)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Zero)(st))(st)
c_isqrt_case_20 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_isqrt_case_20(x1)(x)(st))(i)(xs)(st)
c_isqrt_case_20 x1 x st = Curry.RunTimeSystem.patternFail("Integer.isqrt_case_20")(x)



c_isqrt_case_19 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_Zero
c_isqrt_case_19 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Integer.c_isqrt_case_18(x1)(Curry.Module.Prelude.op_60(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))(st))(st)
c_isqrt_case_19 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_isqrt_case_19(x1)(x)(st))(i)(xs)(st)
c_isqrt_case_19 x1 x st = Curry.RunTimeSystem.patternFail("Integer.isqrt_case_19")(x)



c_isqrt_case_18 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi)
c_isqrt_case_18 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Integer.c_isqrt'46aux'4621(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st)
c_isqrt_case_18 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_isqrt_case_18(x1)(x)(st))(i)(xs)(st)
c_isqrt_case_18 x1 x st = Curry.RunTimeSystem.patternFail("Integer.isqrt_case_18")(x)



c_ilog_case_22 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Integer.c_ilog_case_21(x1)(Curry.Module.Prelude.op_60(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(st))(st)
c_ilog_case_22 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_ilog_case_22(x1)(x)(st))(i)(xs)(st)
c_ilog_case_22 x1 x st = Curry.RunTimeSystem.patternFail("Integer.ilog_case_22")(x)



c_ilog_case_21 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_Zero
c_ilog_case_21 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_43(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(Curry.Module.Integer.c_ilog(Curry.Module.Prelude.c_div(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(st))(st))(st)
c_ilog_case_21 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_ilog_case_21(x1)(x)(st))(i)(xs)(st)
c_ilog_case_21 x1 x st = Curry.RunTimeSystem.patternFail("Integer.ilog_case_21")(x)



c_pow'46powaux'463_case_24 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = x1
c_pow'46powaux'463_case_24 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.Integer.c_pow'46powaux'463(Curry.Module.Prelude.op_42(x1)(Curry.Module.Integer.c_pow'46powaux'463_case_23(x2)(x3)(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_mod(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st))(Curry.Module.Prelude.op_42(x2)(x2)(st))(Curry.Module.Prelude.c_div(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(st)
c_pow'46powaux'463_case_24 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_pow'46powaux'463_case_24(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_pow'46powaux'463_case_24 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("Integer.pow.powaux.3_case_24")(x)



c_pow'46powaux'463_case_23 x2 x3 x4@Curry.Module.Prelude.C_True st = x2
c_pow'46powaux'463_case_23 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi)
c_pow'46powaux'463_case_23 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_pow'46powaux'463_case_23(x2)(x3)(x)(st))(i)(xs)(st)
c_pow'46powaux'463_case_23 x2 x3 x st = Curry.RunTimeSystem.patternFail("Integer.pow.powaux.3_case_23")(x)



c_pow_case_25 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Integer.c_pow'46powaux'463(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(x2)(st)
c_pow_case_25 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Integer.c_pow_case_25(x1)(x2)(x)(st))(i)(xs)(st)
c_pow_case_25 x1 x2 x st = Curry.RunTimeSystem.patternFail("Integer.pow_case_25")(x)


