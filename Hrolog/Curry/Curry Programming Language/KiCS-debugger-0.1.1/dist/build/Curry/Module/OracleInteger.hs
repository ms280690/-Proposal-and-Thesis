{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleInteger (module Curry.Module.OracleInteger) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.Integer
import Curry.Module.Prelude
import Curry.Module.OraclePrelude



-- begin included



-- end included

c_pow :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_pow x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c__case_27(x2)(x3)(Curry.Module.OraclePrelude.op_62_61(x3)(Curry.Module.Prelude.C_Zero)(x1)(st))(x4)(st))(st)



c_pow'46powaux'463 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_pow'46powaux'463 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c__case_26(x2)(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.Prelude.C_Zero)(x1)(st))(x5)(st))(st)



c_ilog :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ilog x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c__case_24(x2)(Curry.Module.OraclePrelude.op_62(x2)(Curry.Module.Prelude.C_Zero)(x1)(st))(x3)(st))(st)



c_isqrt :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_isqrt x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c__case_22(x2)(Curry.Module.OraclePrelude.op_62_61(x2)(Curry.Module.Prelude.C_Zero)(x1)(st))(x3)(st))(st)



c_isqrt'46aux'4621 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_isqrt'46aux'4621 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OracleInteger.c__case_19(x2)(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.OraclePrelude.op_43(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x5)(st))(x6)(st))(st)



c_factorial :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_factorial x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c__case_17(x2)(Curry.Module.OraclePrelude.op_62_61(x2)(Curry.Module.Prelude.C_Zero)(x1)(st))(x3)(st))(st)



c_binomial :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_binomial x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))))(Curry.Module.OracleInteger.c__case_15(x2)(x3)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_62(x3)(Curry.Module.Prelude.C_Zero)(x1)(st))(Curry.Module.OraclePrelude.op_62_61(x2)(x3)(x4)(st))(x5)(st))(x6)(st))(st)



c_binomial'46aux'4641 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_binomial'46aux'4641 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c__case_14(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Zero)(x1)(st))(x4)(st))(st)



c_abs :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_abs x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c__case_13(x2)(Curry.Module.OraclePrelude.op_60(x2)(Curry.Module.Prelude.C_Zero)(x1)(st))(x3)(st))(st)



c_max :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_max x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c__case_12(x2)(x3)(Curry.Module.OraclePrelude.op_60(x2)(x3)(x1)(st))(x4)(st))(st)



c_min :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_min x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c__case_11(x2)(x3)(Curry.Module.OraclePrelude.op_60(x2)(x3)(x1)(st))(x4)(st))(st)



c_max3 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_max3 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c_max(x2)(Curry.Module.OracleInteger.c_max(x3)(x4)(x1)(st))(x5)(st))(st)



c_min3 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_min3 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c_min(x2)(Curry.Module.OracleInteger.c_min(x3)(x4)(x1)(st))(x5)(st))(st)



c_maxlist :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_maxlist x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_10(x2)(x1)(st))(st)



c_minlist :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_minlist x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_8(x2)(x1)(st))(st)



c_bitTrunc :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_bitTrunc x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OracleInteger.c_bitAnd(Curry.Module.OraclePrelude.op_45(Curry.Module.OracleInteger.c_pow(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x2)(x1)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x4)(st))(x3)(x5)(st))(st)



c_bitAnd :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_bitAnd x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c__case_6(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Zero)(x1)(st))(x4)(st))(st)



c_bitOr :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_bitOr x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c__case_4(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Zero)(x1)(st))(x4)(st))(st)



c_bitNot :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_bitNot x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c_bitNot'46aux'46100(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))(x2)(x1)(st))(st)



c_bitNot'46aux'46100 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_bitNot'46aux'46100 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c__case_2(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Zero)(x1)(st))(x4)(st))(st)



c_bitXor :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_bitXor x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c__case_1(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Zero)(x1)(st))(x4)(st))(st)



c_even :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_even x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_mod(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(Curry.Module.Prelude.C_Zero)(x3)(st))(st)



c_odd :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_odd x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_47_61(Curry.Module.OraclePrelude.c_mod(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(Curry.Module.Prelude.C_Zero)(x3)(st))(st)



c__case_1 x2 x3 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_1_case__27(x1)(x2)(x3)(x6)(st))(st)



c__case_0 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_0_case__26(x1)(x4)(st))(st)



c__case_2 x2 x3 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_2_case__25(x1)(x2)(x3)(x6)(st))(st)



c__case_4 x2 x3 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_4_case__24(x1)(x2)(x3)(x6)(st))(st)



c__case_3 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_3_case__23(x1)(x2)(x4)(st))(st)



c__case_6 x2 x3 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_6_case__22(x1)(x2)(x3)(x6)(st))(st)



c__case_5 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_5_case__21(x1)(x2)(x4)(st))(st)



c__case_8 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_8_case__20(x1)(x2)(st))(st)



c__case_7 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_7_case__19(x1)(x3)(x4)(st))(st)



c__case_10 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_10_case__18(x1)(x2)(st))(st)



c__case_9 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_9_case__17(x1)(x3)(x4)(st))(st)



c__case_11 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_11_case__16(x1)(x2)(x3)(x4)(st))(st)



c__case_12 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_12_case__15(x1)(x2)(x3)(x4)(st))(st)



c__case_13 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_13_case__14(x1)(x2)(x3)(st))(st)



c__case_14 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_14_case__13(x1)(x2)(x3)(x4)(st))(st)



c__case_15 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_15_case__12(x1)(x2)(x3)(x4)(st))(st)



c__case_17 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_17_case__11(x1)(x2)(x3)(st))(st)



c__case_16 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_16_case__10(x1)(x2)(x3)(st))(st)



c__case_19 x2 x3 x4 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_19_case__9(x1)(x2)(x3)(x4)(x6)(st))(st)



c__case_18 x2 x3 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_18_case__8(x1)(x2)(x3)(x4)(x5)(x6)(st))(st)



c__case_22 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_22_case__7(x1)(x2)(x3)(st))(st)



c__case_21 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_21_case__6(x1)(x2)(x3)(st))(st)



c__case_20 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_20_case__5(x1)(x2)(x3)(st))(st)



c__case_24 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_24_case__4(x1)(x2)(x3)(st))(st)



c__case_23 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_23_case__3(x1)(x2)(x3)(st))(st)



c__case_26 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_26_case__2(x1)(x2)(x3)(x4)(x5)(st))(st)



c__case_25 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_25_case__1(x1)(x3)(x5)(st))(st)



c__case_27 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_27_case__0(x1)(x2)(x3)(x4)(st))(st)



c__case_27_case__0 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c_pow'46powaux'463(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x2)(x3)(x1)(st))(st)
c__case_27_case__0 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_27_case__0 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_27_case__0(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_27_case__0 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_27_case__0")(x)



c__case_25_case__1 x1 x3 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_25_case__1 x1 x3 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st)
c__case_25_case__1 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_25_case__1(x1)(x3)(x)(st))(i)(xs)(st)
c__case_25_case__1 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_25_case__1")(x)



c__case_26_case__2 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_26_case__2 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))))))(Curry.Module.OracleInteger.c_pow'46powaux'463(Curry.Module.OraclePrelude.op_42(x2)(Curry.Module.OracleInteger.c__case_25(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_mod(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x6)(st))(x7)(st))(x8)(st))(Curry.Module.OraclePrelude.op_42(x3)(x3)(x9)(st))(Curry.Module.OraclePrelude.c_div(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x10)(st))(x11)(st))(st)
c__case_26_case__2 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_26_case__2(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_26_case__2 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_26_case__2")(x)



c__case_23_case__3 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Zero)(st)
c__case_23_case__3 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(Curry.Module.OracleInteger.c_ilog(Curry.Module.OraclePrelude.c_div(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(x1)(st))(x4)(st))(x5)(st))(st)
c__case_23_case__3 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_23_case__3(x1)(x2)(x)(st))(i)(xs)(st)
c__case_23_case__3 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_23_case__3")(x)



c__case_24_case__4 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c__case_23(x2)(Curry.Module.OraclePrelude.op_60(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(x1)(st))(x4)(st))(st)
c__case_24_case__4 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_24_case__4 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_24_case__4(x1)(x2)(x)(st))(i)(xs)(st)
c__case_24_case__4 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_24_case__4")(x)



c__case_20_case__5 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st)
c__case_20_case__5 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c_isqrt'46aux'4621(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x2)(x1)(st))(st)
c__case_20_case__5 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_20_case__5(x1)(x2)(x)(st))(i)(xs)(st)
c__case_20_case__5 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_20_case__5")(x)



c__case_21_case__6 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Zero)(st)
c__case_21_case__6 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c__case_20(x2)(Curry.Module.OraclePrelude.op_60(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))(x1)(st))(x4)(st))(st)
c__case_21_case__6 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_21_case__6(x1)(x2)(x)(st))(i)(xs)(st)
c__case_21_case__6 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_21_case__6")(x)



c__case_22_case__7 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c__case_21(x2)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Zero)(x1)(st))(x4)(st))(st)
c__case_22_case__7 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_22_case__7 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_22_case__7(x1)(x2)(x)(st))(i)(xs)(st)
c__case_22_case__7 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_22_case__7")(x)



c__case_18_case__8 x1 x2 x3 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c_isqrt'46aux'4621(x2)(x3)(x5)(x1)(st))(st)
c__case_18_case__8 x1 x2 x3 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c_isqrt'46aux'4621(x2)(x5)(x4)(x1)(st))(st)
c__case_18_case__8 x1 x2 x3 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_18_case__8(x1)(x2)(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_18_case__8 x1 x2 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_18_case__8")(x)



c__case_19_case__9 x1 x2 x3 x4 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_19_case__9 x1 x2 x3 x4 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(let {x5 = Curry.Module.OraclePrelude.c_div(Curry.Module.OraclePrelude.op_43(x4)(x3)(x1)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x7)(st)} in let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))(Curry.Module.OracleInteger.c__case_18(x2)(x3)(x4)(x5)(Curry.Module.OraclePrelude.op_62(Curry.Module.OraclePrelude.op_42(x5)(x5)(x8)(st))(x2)(x9)(st))(x10)(st))(st))(st)
c__case_19_case__9 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_19_case__9(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_19_case__9 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_19_case__9")(x)



c__case_16_case__10 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st)
c__case_16_case__10 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_42(x2)(Curry.Module.OracleInteger.c_factorial(Curry.Module.OraclePrelude.op_45(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x4)(st))(x5)(st))(st)
c__case_16_case__10 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_16_case__10(x1)(x2)(x)(st))(i)(xs)(st)
c__case_16_case__10 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_16_case__10")(x)



c__case_17_case__11 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c__case_16(x2)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Zero)(x1)(st))(x4)(st))(st)
c__case_17_case__11 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_17_case__11 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_17_case__11(x1)(x2)(x)(st))(i)(xs)(st)
c__case_17_case__11 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_17_case__11")(x)



c__case_15_case__12 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_div(Curry.Module.OracleInteger.c_binomial'46aux'4641(x3)(x2)(x1)(st))(Curry.Module.OracleInteger.c_factorial(x3)(x5)(st))(x6)(st))(st)
c__case_15_case__12 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_15_case__12 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_15_case__12(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_15_case__12 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_15_case__12")(x)



c__case_14_case__13 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st)
c__case_14_case__13 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_42(x3)(Curry.Module.OracleInteger.c_binomial'46aux'4641(Curry.Module.OraclePrelude.op_45(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(Curry.Module.OraclePrelude.op_45(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x5)(st))(x6)(st))(x7)(st))(st)
c__case_14_case__13 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_14_case__13(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_14_case__13 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_14_case__13")(x)



c__case_13_case__14 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_negate(x2)(x1)(st))(st)
c__case_13_case__14 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_13_case__14 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_13_case__14(x1)(x2)(x)(st))(i)(xs)(st)
c__case_13_case__14 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_13_case__14")(x)



c__case_12_case__15 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_12_case__15 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_12_case__15 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_12_case__15(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_12_case__15 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_12_case__15")(x)



c__case_11_case__16 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_11_case__16 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_11_case__16 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_11_case__16(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_11_case__16 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_11_case__16")(x)



c__case_9_case__17 x1 x3 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_9_case__17 x1 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c_max(x3)(Curry.Module.OracleInteger.c_maxlist((Curry.Module.Prelude.:<)(x5)(x6))(x1)(st))(x7)(st))(st)
c__case_9_case__17 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_9_case__17(x1)(x3)(x)(st))(i)(xs)(st)
c__case_9_case__17 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_9_case__17")(x)



c__case_10_case__18 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_9(x3)(x4)(x1)(st))(st)
c__case_10_case__18 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_10_case__18(x1)(x)(st))(i)(xs)(st)
c__case_10_case__18 x1 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_10_case__18")(x)



c__case_7_case__19 x1 x3 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_7_case__19 x1 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleInteger.c_min(x3)(Curry.Module.OracleInteger.c_minlist((Curry.Module.Prelude.:<)(x5)(x6))(x1)(st))(x7)(st))(st)
c__case_7_case__19 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_7_case__19(x1)(x3)(x)(st))(i)(xs)(st)
c__case_7_case__19 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_7_case__19")(x)



c__case_8_case__20 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteger.c__case_7(x3)(x4)(x1)(st))(st)
c__case_8_case__20 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_8_case__20(x1)(x)(st))(i)(xs)(st)
c__case_8_case__20 x1 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_8_case__20")(x)



c__case_5_case__21 x1 x2 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Zero)(st)
c__case_5_case__21 x1 x2 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_mod(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(st)
c__case_5_case__21 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_5_case__21(x1)(x2)(x)(st))(i)(xs)(st)
c__case_5_case__21 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_5_case__21")(x)



c__case_6_case__22 x1 x2 x3 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Zero)(st)
c__case_6_case__22 x1 x2 x3 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))))(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))))(Curry.Module.CEventOracle.c_replace(x13)(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(Curry.Module.OracleInteger.c_bitAnd(Curry.Module.OraclePrelude.c_div(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(Curry.Module.OraclePrelude.c_div(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x7)(st))(x8)(st))(x9)(st))(Curry.Module.OracleInteger.c__case_5(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_mod(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x10)(st))(Curry.Module.Prelude.C_Zero)(x11)(st))(x12)(st))(x13)(st))(st))(st))(st)
c__case_6_case__22 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_6_case__22(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_6_case__22 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_6_case__22")(x)



c__case_3_case__23 x1 x2 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st)
c__case_3_case__23 x1 x2 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_mod(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(st)
c__case_3_case__23 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_3_case__23(x1)(x2)(x)(st))(i)(xs)(st)
c__case_3_case__23 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_3_case__23")(x)



c__case_4_case__24 x1 x2 x3 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_4_case__24 x1 x2 x3 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))))(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))))(Curry.Module.CEventOracle.c_replace(x13)(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(Curry.Module.OracleInteger.c_bitOr(Curry.Module.OraclePrelude.c_div(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(Curry.Module.OraclePrelude.c_div(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x7)(st))(x8)(st))(x9)(st))(Curry.Module.OracleInteger.c__case_3(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_mod(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x10)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x11)(st))(x12)(st))(x13)(st))(st))(st))(st)
c__case_4_case__24 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_4_case__24(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_4_case__24 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_4_case__24")(x)



c__case_2_case__25 x1 x2 x3 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Zero)(st)
c__case_2_case__25 x1 x2 x3 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))))(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List)))(Curry.Module.CEventOracle.c_replace(x12)(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(Curry.Module.OracleInteger.c_bitNot'46aux'46100(Curry.Module.OraclePrelude.op_45(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(Curry.Module.OraclePrelude.c_div(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x7)(st))(x8)(st))(x9)(st))(Curry.Module.OraclePrelude.op_45(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(Curry.Module.OraclePrelude.c_mod(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x10)(st))(x11)(st))(x12)(st))(st))(st))(st)
c__case_2_case__25 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_2_case__25(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_2_case__25 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_2_case__25")(x)



c__case_0_case__26 x1 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Zero)(st)
c__case_0_case__26 x1 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st)
c__case_0_case__26 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_0_case__26(x1)(x)(st))(i)(xs)(st)
c__case_0_case__26 x1 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_0_case__26")(x)



c__case_1_case__27 x1 x2 x3 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_1_case__27 x1 x2 x3 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))))(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))))(Curry.Module.CEventOracle.c_replace(x14)(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(Curry.Module.OracleInteger.c_bitXor(Curry.Module.OraclePrelude.c_div(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(Curry.Module.OraclePrelude.c_div(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x7)(st))(x8)(st))(x9)(st))(Curry.Module.OracleInteger.c__case_0(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_mod(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x10)(st))(Curry.Module.OraclePrelude.c_mod(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x11)(st))(x12)(st))(x13)(st))(x14)(st))(st))(st))(st)
c__case_1_case__27 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteger.c__case_1_case__27(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_1_case__27 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleInteger._case_1_case__27")(x)


