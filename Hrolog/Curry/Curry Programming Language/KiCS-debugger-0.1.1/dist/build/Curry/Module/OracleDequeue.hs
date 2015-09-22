{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleDequeue (module Curry.Module.OracleDequeue) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.Dequeue
import Curry.Module.Prelude
import Curry.Module.OraclePrelude



-- begin included



-- end included

c_empty :: (Curry t0) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Dequeue.C_Queue t0
c_empty x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Dequeue.C_S(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.List)(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.List))(st)



c_isEmpty :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isEmpty x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_25(x2)(x1)(st))(st)



c_deqHead :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_deqHead x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_24(x2)(x1)(st))(st)



c_deqLast :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_deqLast x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_22(x2)(x1)(st))(st)



c_cons :: (Curry t0) => t0 -> (Curry.Module.Dequeue.C_Queue t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Dequeue.C_Queue t0
c_cons x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_20(x2)(x3)(x1)(st))(st)



c_deqTail :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Dequeue.C_Queue t0
c_deqTail x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_19(x2)(x1)(st))(st)



c_snoc :: (Curry t0) => t0 -> (Curry.Module.Dequeue.C_Queue t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Dequeue.C_Queue t0
c_snoc x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_17(x2)(x3)(x1)(st))(st)



c_deqInit :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Dequeue.C_Queue t0
c_deqInit x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_16(x2)(x1)(st))(st)



c_deqReverse :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Dequeue.C_Queue t0
c_deqReverse x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_14(x2)(x1)(st))(st)



c_check :: (Curry t0) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List t0) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Dequeue.C_Queue t0
c_check x2 x3 x4 x5 x1 st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(let {x6 = Curry.Module.OraclePrelude.op_43(x2)(x4)(x1)(st)} in let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(let {x7 = Curry.Module.OraclePrelude.c_div(x6)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x13)(st)} in let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(let {x9 = Curry.Module.OraclePrelude.c_splitAt(x7)(x3)(x15)(st)} in let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))(let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))(let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))))(let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List))))(Curry.Module.OracleDequeue.c__case_13(x2)(x3)(x4)(x5)(x7)(Curry.Module.OraclePrelude.op_45(x6)(x7)(x14)(st))(Curry.Module.OracleDequeue.c_check'46_'35selFP3'35f'39(x9)(x16)(st))(Curry.Module.OraclePrelude.op_43_43(x5)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_reverse(x18)(st))(Curry.Module.OracleDequeue.c_check'46_'35selFP4'35rf'39(x9)(x17)(st))(x19)(st))(x20)(st))(Curry.Module.OraclePrelude.op_60_61(x2)(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))(x4)(x21)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x22)(st))(x23)(st))(x24)(st))(st))(st))(st))(st))(st))(st))(st))(st)



c_check'46_'35selFP3'35f'39 :: (Curry t45) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t45) (Curry.Module.Prelude.List t45)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t45
c_check'46_'35selFP3'35f'39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_11(x2)(x1)(st))(st)



c_check'46_'35selFP4'35rf'39 :: (Curry t45) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t45) (Curry.Module.Prelude.List t45)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t45
c_check'46_'35selFP4'35rf'39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_10(x2)(x1)(st))(st)



c_listToDeq :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Dequeue.C_Queue t0
c_listToDeq x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OracleDequeue.c_check(Curry.Module.OraclePrelude.c_length(x2)(x1)(st))(x2)(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.List)(x3)(st))(st)



c_deqToList :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_deqToList x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_9(x2)(x1)(st))(st)



c_deqLength :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_deqLength x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_8(x2)(x1)(st))(st)



c_rotate :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Dequeue.C_Queue t0
c_rotate x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OracleDequeue.c_snoc(Curry.Module.OracleDequeue.c_deqHead(x2)(x1)(st))(Curry.Module.OracleDequeue.c_deqTail(x2)(x3)(st))(x4)(st))(st)



c_matchHead :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 t0 (Curry.Module.Dequeue.C_Queue t0))
c_matchHead x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_7(x2)(x1)(st))(st)



c_matchLast :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 t0 (Curry.Module.Dequeue.C_Queue t0))
c_matchLast x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_3(x2)(x1)(st))(st)



c__case_3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_3_case__25(x1)(x2)(st))(st)



c__case_2 x3 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_2_case__24(x1)(x3)(x4)(x5)(x6)(st))(st)



c__case_1 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_1_case__23(x1)(x4)(st))(st)



c__case_0 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_0_case__22(x1)(x7)(x8)(st))(st)



c__case_7 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_7_case__21(x1)(x2)(st))(st)



c__case_6 x3 x5 x6 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_6_case__20(x1)(x3)(x5)(x6)(x4)(st))(st)



c__case_5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_5_case__19(x1)(x6)(st))(st)



c__case_4 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_4_case__18(x1)(x7)(x8)(st))(st)



c__case_8 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_8_case__17(x1)(x2)(st))(st)



c__case_9 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_9_case__16(x1)(x2)(st))(st)



c__case_10 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_10_case__15(x1)(x2)(st))(st)



c__case_11 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_11_case__14(x1)(x2)(st))(st)



c__case_13 x2 x3 x4 x5 x7 x8 x10 x12 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_13_case__13(x1)(x2)(x3)(x4)(x5)(x7)(x8)(x10)(x12)(x13)(st))(st)



c__case_12 x7 x8 x10 x12 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_12_case__12(x1)(x7)(x8)(x10)(x12)(x13)(st))(st)



c__case_14 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_14_case__11(x1)(x2)(st))(st)



c__case_16 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_16_case__10(x1)(x2)(st))(st)



c__case_15 x3 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_15_case__9(x1)(x3)(x4)(x5)(x6)(st))(st)



c__case_17 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_17_case__8(x1)(x2)(x3)(st))(st)



c__case_19 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_19_case__7(x1)(x2)(st))(st)



c__case_18 x3 x5 x6 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_18_case__6(x1)(x3)(x5)(x6)(x4)(st))(st)



c__case_20 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_20_case__5(x1)(x2)(x3)(st))(st)



c__case_22 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_22_case__4(x1)(x2)(st))(st)



c__case_21 x4 x5 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_21_case__3(x1)(x4)(x6)(x7)(st))(st)



c__case_24 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_24_case__2(x1)(x2)(st))(st)



c__case_23 x3 x4 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_23_case__1(x1)(x4)(x6)(x7)(st))(st)



c__case_25 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_25_case__0(x1)(x2)(st))(st)



c__case_25_case__0 x1 x2@(Curry.Module.Dequeue.C_S x3 x4 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.op_43(x3)(x5)(x1)(st))(Curry.Module.Prelude.C_Zero)(x7)(st))(st)
c__case_25_case__0 x1 (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_25_case__0(x1)(x)(st))(i)(xs)(st)
c__case_25_case__0 x1 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_25_case__0")(x)



c__case_23_case__1 x1 x4 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_23_case__1 x1 x4 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_23_case__1 x1 x4 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_23_case__1(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_23_case__1 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_23_case__1")(x)



c__case_24_case__2 x1 x2@(Curry.Module.Dequeue.C_S x3 x4 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_head(Curry.Module.OracleDequeue.c__case_23(x3)(x4)(x6)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Zero)(x1)(st))(x7)(st))(x8)(st))(st)
c__case_24_case__2 x1 (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_24_case__2(x1)(x)(st))(i)(xs)(st)
c__case_24_case__2 x1 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_24_case__2")(x)



c__case_21_case__3 x1 x4 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_21_case__3 x1 x4 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_21_case__3 x1 x4 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_21_case__3(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_21_case__3 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_21_case__3")(x)



c__case_22_case__4 x1 x2@(Curry.Module.Dequeue.C_S x3 x4 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_head(Curry.Module.OracleDequeue.c__case_21(x4)(x5)(x6)(Curry.Module.OraclePrelude.op_61_61(x5)(Curry.Module.Prelude.C_Zero)(x1)(st))(x7)(st))(x8)(st))(st)
c__case_22_case__4 x1 (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_22_case__4(x1)(x)(st))(i)(xs)(st)
c__case_22_case__4 x1 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_22_case__4")(x)



c__case_20_case__5 x1 x2 x3@(Curry.Module.Dequeue.C_S x4 x5 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleDequeue.c_check(Curry.Module.OraclePrelude.op_43(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))((Curry.Module.Prelude.:<)(x2)(x5))(x6)(x7)(x8)(st))(st)
c__case_20_case__5 x1 x2 (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_20_case__5(x1)(x2)(x)(st))(i)(xs)(st)
c__case_20_case__5 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_20_case__5")(x)



c__case_18_case__6 x1 x3 x5 x6 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c_empty(x1)(st))(st)
c__case_18_case__6 x1 x3 x5 x6 x4@((Curry.Module.Prelude.:<) x7 x8) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))(Curry.Module.OracleDequeue.c_deqReverse(Curry.Module.OracleDequeue.c_check(x5)(x6)(Curry.Module.OraclePrelude.op_45(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x8)(x9)(st))(x10)(st))(st)
c__case_18_case__6 x1 x3 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_18_case__6(x1)(x3)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_18_case__6 x1 x3 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_18_case__6")(x)



c__case_19_case__7 x1 x2@(Curry.Module.Dequeue.C_S x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_18(x3)(x5)(x6)(x4)(x1)(st))(st)
c__case_19_case__7 x1 (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_19_case__7(x1)(x)(st))(i)(xs)(st)
c__case_19_case__7 x1 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_19_case__7")(x)



c__case_17_case__8 x1 x2 x3@(Curry.Module.Dequeue.C_S x4 x5 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OracleDequeue.c_deqReverse(Curry.Module.OracleDequeue.c_check(Curry.Module.OraclePrelude.op_43(x6)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))((Curry.Module.Prelude.:<)(x2)(x7))(x4)(x5)(x8)(st))(x9)(st))(st)
c__case_17_case__8 x1 x2 (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_17_case__8(x1)(x2)(x)(st))(i)(xs)(st)
c__case_17_case__8 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_17_case__8")(x)



c__case_15_case__9 x1 x3 x4 x5 x6@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c_empty(x1)(st))(st)
c__case_15_case__9 x1 x3 x4 x5 x6@((Curry.Module.Prelude.:<) x7 x8) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OracleDequeue.c_check(x3)(x4)(Curry.Module.OraclePrelude.op_45(x5)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x8)(x9)(st))(st)
c__case_15_case__9 x1 x3 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_15_case__9(x1)(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_15_case__9 x1 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_15_case__9")(x)



c__case_16_case__10 x1 x2@(Curry.Module.Dequeue.C_S x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_15(x3)(x4)(x5)(x6)(x1)(st))(st)
c__case_16_case__10 x1 (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_16_case__10(x1)(x)(st))(i)(xs)(st)
c__case_16_case__10 x1 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_16_case__10")(x)



c__case_14_case__11 x1 x2@(Curry.Module.Dequeue.C_S x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Dequeue.C_S(x5)(x6)(x3)(x4))(st)
c__case_14_case__11 x1 (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_14_case__11(x1)(x)(st))(i)(xs)(st)
c__case_14_case__11 x1 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_14_case__11")(x)



c__case_12_case__12 x1 x7 x8 x10 x12 x13@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Dequeue.C_S(x7)(x10)(x8)(x12))(st)
c__case_12_case__12 x1 x7 x8 x10 x12 x13@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_12_case__12 x1 x7 x8 x10 x12 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_12_case__12(x1)(x7)(x8)(x10)(x12)(x)(st))(i)(xs)(st)
c__case_12_case__12 x1 x7 x8 x10 x12 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_12_case__12")(x)



c__case_13_case__13 x1 x2 x3 x4 x5 x7 x8 x10 x12 x13@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Dequeue.C_S(x2)(x3)(x4)(x5))(st)
c__case_13_case__13 x1 x2 x3 x4 x5 x7 x8 x10 x12 x13@Curry.Module.Prelude.C_False st = let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(Curry.Module.OracleDequeue.c__case_12(x7)(x8)(x10)(x12)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x14)(st))(st)
c__case_13_case__13 x1 x2 x3 x4 x5 x7 x8 x10 x12 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_13_case__13(x1)(x2)(x3)(x4)(x5)(x7)(x8)(x10)(x12)(x)(st))(i)(xs)(st)
c__case_13_case__13 x1 x2 x3 x4 x5 x7 x8 x10 x12 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_13_case__13")(x)



c__case_11_case__14 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_11_case__14 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_11_case__14(x1)(x)(st))(i)(xs)(st)
c__case_11_case__14 x1 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_11_case__14")(x)



c__case_10_case__15 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_10_case__15 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_10_case__15(x1)(x)(st))(i)(xs)(st)
c__case_10_case__15 x1 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_10_case__15")(x)



c__case_9_case__16 x1 x2@(Curry.Module.Dequeue.C_S x3 x4 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(x4)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_reverse(x1)(st))(x6)(x7)(st))(x8)(st))(st)
c__case_9_case__16 x1 (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_9_case__16(x1)(x)(st))(i)(xs)(st)
c__case_9_case__16 x1 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_9_case__16")(x)



c__case_8_case__17 x1 x2@(Curry.Module.Dequeue.C_S x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_43(x3)(x5)(x1)(st))(st)
c__case_8_case__17 x1 (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_8_case__17(x1)(x)(st))(i)(xs)(st)
c__case_8_case__17 x1 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_8_case__17")(x)



c__case_4_case__18 x1 x7 x8@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_Just(Curry.Module.Prelude.T2(x7)(Curry.Module.OracleDequeue.c_empty(x1)(st))))(st)
c__case_4_case__18 x1 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_4_case__18(x1)(x7)(x)(st))(i)(xs)(st)
c__case_4_case__18 x1 x7 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_4_case__18")(x)



c__case_5_case__19 x1 x6@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_5_case__19 x1 x6@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_4(x7)(x8)(x1)(st))(st)
c__case_5_case__19 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_5_case__19(x1)(x)(st))(i)(xs)(st)
c__case_5_case__19 x1 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_5_case__19")(x)



c__case_6_case__20 x1 x3 x5 x6 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_5(x6)(x1)(st))(st)
c__case_6_case__20 x1 x3 x5 x6 x4@((Curry.Module.Prelude.:<) x9 x10) st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List)))(Curry.Module.Prelude.C_Just(Curry.Module.Prelude.T2(x9)(Curry.Module.OracleDequeue.c_deqReverse(Curry.Module.OracleDequeue.c_check(x5)(x6)(Curry.Module.OraclePrelude.op_45(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x10)(x11)(st))(x12)(st))))(st)
c__case_6_case__20 x1 x3 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_6_case__20(x1)(x3)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_6_case__20 x1 x3 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_6_case__20")(x)



c__case_7_case__21 x1 x2@(Curry.Module.Dequeue.C_S x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_6(x3)(x5)(x6)(x4)(x1)(st))(st)
c__case_7_case__21 x1 (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_7_case__21(x1)(x)(st))(i)(xs)(st)
c__case_7_case__21 x1 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_7_case__21")(x)



c__case_0_case__22 x1 x7 x8@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_Just(Curry.Module.Prelude.T2(x7)(Curry.Module.OracleDequeue.c_empty(x1)(st))))(st)
c__case_0_case__22 x1 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_0_case__22(x1)(x7)(x)(st))(i)(xs)(st)
c__case_0_case__22 x1 x7 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_0_case__22")(x)



c__case_1_case__23 x1 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_1_case__23 x1 x4@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_0(x7)(x8)(x1)(st))(st)
c__case_1_case__23 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_1_case__23(x1)(x)(st))(i)(xs)(st)
c__case_1_case__23 x1 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_1_case__23")(x)



c__case_2_case__24 x1 x3 x4 x5 x6@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_1(x4)(x1)(st))(st)
c__case_2_case__24 x1 x3 x4 x5 x6@((Curry.Module.Prelude.:<) x9 x10) st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.Prelude.C_Just(Curry.Module.Prelude.T2(x9)(Curry.Module.OracleDequeue.c_check(x3)(x4)(Curry.Module.OraclePrelude.op_45(x5)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x10)(x11)(st))))(st)
c__case_2_case__24 x1 x3 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_2_case__24(x1)(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_2_case__24 x1 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_2_case__24")(x)



c__case_3_case__25 x1 x2@(Curry.Module.Dequeue.C_S x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDequeue.c__case_2(x3)(x4)(x5)(x6)(x1)(st))(st)
c__case_3_case__25 x1 (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDequeue.c__case_3_case__25(x1)(x)(st))(i)(xs)(st)
c__case_3_case__25 x1 x st = Curry.RunTimeSystem.patternFail("OracleDequeue._case_3_case__25")(x)


