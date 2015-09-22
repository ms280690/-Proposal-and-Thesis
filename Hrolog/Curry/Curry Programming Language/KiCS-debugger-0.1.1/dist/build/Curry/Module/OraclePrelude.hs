{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OraclePrelude (module Curry.Module.OraclePrelude) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.Prelude



-- begin included



-- end included

op_46 :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t2 -> Curry.RunTimeSystem.State -> t0))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t2 -> Curry.RunTimeSystem.State -> t1))
op_46 x2 x3 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_'46'46_'35lambda2(x2)(x3)))))(st)



c_'46'46_'35lambda2 :: (Curry t9,Curry t11,Curry t7) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t9 -> Curry.RunTimeSystem.State -> t11))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t7 -> Curry.RunTimeSystem.State -> t9))) -> t7 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t11
c_'46'46_'35lambda2 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(x2)(Curry.Module.Oracle.c_apply(x3)(x4)(x1)(st))(x5)(st))(st)



c_id :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_id x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)



c_const :: (Curry t0,Curry t1) => t0 -> t1 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_const x2 x3 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)



c_curry :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 t0 t1) -> Curry.RunTimeSystem.State -> t2))) -> t0 -> t1 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t2
c_curry x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.c_apply(x2)(Curry.Module.Prelude.T2(x3)(x4))(x1)(st))(st)



c_uncurry :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t2))))) -> (Curry.Module.Prelude.T2 t0 t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t2
c_uncurry x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_155(x2)(x3)(x1)(st))(st)



c_flip :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t2))))) -> t1 -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t2
c_flip x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x4)(x1)(st))(x3)(x5)(st))(st)



c_until :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0))) -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_until x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_154(x2)(x3)(x4)(Curry.Module.Oracle.c_apply(x2)(x4)(x1)(st))(x5)(st))(st)



op_36 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1))) -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t1
op_36 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.c_apply(x2)(x3)(x1)(st))(st)



c_ensureSpine :: (Curry t0) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0))
c_ensureSpine x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.op_36_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_ensureSpine'46ensureList'4621))))))))(st)



c_ensureSpine'46ensureList'4621 :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_ensureSpine'46ensureList'4621 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_153(x2)(x1)(st))(st)



c_seq :: (Curry t0,Curry t1) => t0 -> t1 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t1
c_seq x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.op_36_33(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_const(x3)))))(x2)(x1)(st))(st)



c_error :: (Curry t0) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_error x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_prim_error))))(x2)(x1)(st))(st)



op_38_38 :: Curry.Module.Prelude.C_Bool -> Curry.Module.Prelude.C_Bool -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_38_38 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_152(x3)(x2)(x1)(st))(st)



op_124_124 :: Curry.Module.Prelude.C_Bool -> Curry.Module.Prelude.C_Bool -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_124_124 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_151(x3)(x2)(x1)(st))(st)



c_not :: Curry.Module.Prelude.C_Bool -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_not x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_150(x2)(x1)(st))(st)



c_otherwise :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_otherwise x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)



c_if_then_else :: (Curry t0) => Curry.Module.Prelude.C_Bool -> t0 -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_if_then_else x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_149(x3)(x4)(x2)(x1)(st))(st)



c_isLT :: Curry.Module.Prelude.C_Ordering -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isLT x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_148(x2)(x1)(st))(st)



c_isGT :: Curry.Module.Prelude.C_Ordering -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isGT x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_147(x2)(x1)(st))(st)



c_isEQ :: Curry.Module.Prelude.C_Ordering -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isEQ x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_146(x2)(x1)(st))(st)



c_compare :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_compare x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_145(x3)(x2)(x1)(st))(st)



op_60 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_60 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_compare(x2)(x3)(x1)(st))(Curry.Module.Prelude.C_LT)(x4)(st))(st)



op_62 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_62 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_compare(x2)(x3)(x1)(st))(Curry.Module.Prelude.C_GT)(x4)(st))(st)



op_60_61 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_60_61 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_47_61(Curry.Module.OraclePrelude.c_compare(x2)(x3)(x1)(st))(Curry.Module.Prelude.C_GT)(x4)(st))(st)



op_62_61 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_62_61 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_47_61(Curry.Module.OraclePrelude.c_compare(x2)(x3)(x1)(st))(Curry.Module.Prelude.C_LT)(x4)(st))(st)



c_max :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_max x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_141(x2)(x3)(Curry.Module.OraclePrelude.c_compare(x2)(x3)(x1)(st))(x4)(st))(st)



c_min :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_min x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_140(x2)(x3)(Curry.Module.OraclePrelude.c_compare(x2)(x3)(x1)(st))(x4)(st))(st)



op_47_61 :: (Curry t0) => t0 -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_47_61 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_not(Curry.Module.OraclePrelude.op_61_61(x2)(x3)(x1)(st))(x4)(st))(st)



c_fst :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T2 t0 t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_fst x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_139(x2)(x1)(st))(st)



c_snd :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T2 t0 t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t1
c_snd x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_138(x2)(x1)(st))(st)



c_head :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_head x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_137(x2)(x1)(st))(st)



c_tail :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_tail x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_136(x2)(x1)(st))(st)



c_null :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_null x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_135(x2)(x1)(st))(st)



op_43_43 :: (Curry t0) => (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
op_43_43 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_134(x3)(x2)(x1)(st))(st)



c_length :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_length x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_133(x2)(x1)(st))(st)



op_33_33 :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
op_33_33 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_132(x3)(x2)(x1)(st))(st)



c_map :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1))) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1
c_map x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_129(x2)(x3)(x1)(st))(st)



c_foldl :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t0))))) -> t0 -> (Curry.Module.Prelude.List t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_foldl x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_128(x2)(x3)(x4)(x1)(st))(st)



c_foldl1 :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0))))) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_foldl1 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_127(x2)(x3)(x1)(st))(st)



c_foldr :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t1))))) -> t1 -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t1
c_foldr x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_126(x2)(x3)(x4)(x1)(st))(st)



c_foldr1 :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0))))) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_foldr1 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_125(x2)(x3)(x1)(st))(st)



c_filter :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_filter x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_123(x2)(x3)(x1)(st))(st)



c_zip :: (Curry t0,Curry t1) => (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 t1)
c_zip x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_121(x3)(x2)(x1)(st))(st)



c_zip3 :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t1) -> (Curry.Module.Prelude.List t2) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T3 t0 t1 t2)
c_zip3 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_119(x3)(x4)(x2)(x1)(st))(st)



c_zipWith :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t2))))) -> (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t2
c_zipWith x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_116(x2)(x4)(x3)(x1)(st))(st)



c_zipWith3 :: (Curry t0,Curry t1,Curry t2,Curry t3) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t2 -> Curry.RunTimeSystem.State -> t3))))))) -> (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t1) -> (Curry.Module.Prelude.List t2) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t3
c_zipWith3 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_114(x2)(x4)(x5)(x3)(x1)(st))(st)



c_unzip :: (Curry t0,Curry t1) => (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 t1)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t1)
c_unzip x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_111(x2)(x1)(st))(st)



c_unzip'46_'35selFP3'35xs :: (Curry t476,Curry t477) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t476) (Curry.Module.Prelude.List t477)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t476
c_unzip'46_'35selFP3'35xs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_109(x2)(x1)(st))(st)



c_unzip'46_'35selFP4'35ys :: (Curry t476,Curry t477) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t476) (Curry.Module.Prelude.List t477)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t477
c_unzip'46_'35selFP4'35ys x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_108(x2)(x1)(st))(st)



c_unzip3 :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.List (Curry.Module.Prelude.T3 t0 t1 t2)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.List t2)
c_unzip3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_107(x2)(x1)(st))(st)



c_unzip3'46_'35selFP6'35xs :: (Curry t493,Curry t494,Curry t495) => (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List t493) (Curry.Module.Prelude.List t494) (Curry.Module.Prelude.List t495)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t493
c_unzip3'46_'35selFP6'35xs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_105(x2)(x1)(st))(st)



c_unzip3'46_'35selFP7'35ys :: (Curry t493,Curry t494,Curry t495) => (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List t493) (Curry.Module.Prelude.List t494) (Curry.Module.Prelude.List t495)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t494
c_unzip3'46_'35selFP7'35ys x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_104(x2)(x1)(st))(st)



c_unzip3'46_'35selFP8'35zs :: (Curry t493,Curry t494,Curry t495) => (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List t493) (Curry.Module.Prelude.List t494) (Curry.Module.Prelude.List t495)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t495
c_unzip3'46_'35selFP8'35zs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_103(x2)(x1)(st))(st)



c_concat :: (Curry t0) => (Curry.Module.Prelude.List (Curry.Module.Prelude.List t0)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_concat x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_43_43))(st))(Curry.Module.Prelude.List)(x2)(x1)(st))(st)



c_concatMap :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1))
c_concatMap x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_concat))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(x2)))))(x1)(st))(st)



c_iterate :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0))) -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_iterate x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(x3)(Curry.Module.OraclePrelude.c_iterate(x2)(Curry.Module.Oracle.c_apply(x2)(x3)(x1)(st))(x4)(st)))(st)



c_repeat :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_repeat x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.OraclePrelude.c_repeat(x2)(x1)(st)))(st)



c_replicate :: (Curry t0) => Curry.Module.Prelude.C_Int -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_replicate x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_take(x2)(Curry.Module.OraclePrelude.c_repeat(x3)(x1)(st))(x4)(st))(st)



c_take :: (Curry t0) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_take x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_102(x3)(x2)(x1)(st))(st)



c_drop :: (Curry t0) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_drop x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_100(x2)(x3)(Curry.Module.OraclePrelude.op_60_61(x2)(Curry.Module.Prelude.C_Zero)(x1)(st))(x4)(st))(st)



c_drop'46dropp'46272 :: (Curry t0) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_drop'46dropp'46272 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_99(x2)(x3)(x1)(st))(st)



c_splitAt :: (Curry t0) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t0)
c_splitAt x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_98(x2)(x3)(Curry.Module.OraclePrelude.op_60_61(x2)(Curry.Module.Prelude.C_Zero)(x1)(st))(x4)(st))(st)



c_splitAt'46splitAtp'46282 :: (Curry t0) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t0)
c_splitAt'46splitAtp'46282 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_97(x2)(x3)(x1)(st))(st)



c_splitAt'46splitAtp'46282'46_'35selFP10'35ys :: (Curry t576) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t576) (Curry.Module.Prelude.List t576)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t576
c_splitAt'46splitAtp'46282'46_'35selFP10'35ys x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_96(x2)(x1)(st))(st)



c_splitAt'46splitAtp'46282'46_'35selFP11'35zs :: (Curry t576) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t576) (Curry.Module.Prelude.List t576)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t576
c_splitAt'46splitAtp'46282'46_'35selFP11'35zs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_95(x2)(x1)(st))(st)



c_takeWhile :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_takeWhile x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_94(x2)(x3)(x1)(st))(st)



c_dropWhile :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_dropWhile x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_92(x2)(x3)(x1)(st))(st)



c_span :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t0)
c_span x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_90(x2)(x3)(x1)(st))(st)



c_span'46_'35selFP13'35ys :: (Curry t627) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t627) (Curry.Module.Prelude.List t627)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t627
c_span'46_'35selFP13'35ys x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_87(x2)(x1)(st))(st)



c_span'46_'35selFP14'35zs :: (Curry t627) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t627) (Curry.Module.Prelude.List t627)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t627
c_span'46_'35selFP14'35zs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_86(x2)(x1)(st))(st)



c_break :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t0)))
c_break x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_span(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_not))))(x2)(x1)(st))))))(st)



c_lines :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_lines x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_85(x2)(x1)(st))(st)



c_lines'46splitline'46314 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_lines'46splitline'46314 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_84(x2)(x1)(st))(st)



c_lines'46splitline'46314'46_'35selFP16'35ds :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_lines'46splitline'46314'46_'35selFP16'35ds x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_82(x2)(x1)(st))(st)



c_lines'46splitline'46314'46_'35selFP17'35es :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_lines'46splitline'46314'46_'35selFP17'35es x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_81(x2)(x1)(st))(st)



c_lines'46_'35selFP19'35l :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_lines'46_'35selFP19'35l x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_80(x2)(x1)(st))(st)



c_lines'46_'35selFP20'35xs_l :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_lines'46_'35selFP20'35xs_l x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_79(x2)(x1)(st))(st)



c_unlines :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_unlines x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_43_43))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))))))(x1)(st))(x2)(x3)(st))(st)



c_words :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_words x2 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(let {x3 = Curry.Module.OraclePrelude.c_dropWhile(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_words'46isSpace'46326))))(x2)(x1)(st)} in let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_78(x3)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.List)(x4)(st))(x5)(st))(st))(st)



c_words'46isSpace'46326 :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_words'46isSpace'46326 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Char(' '))(x1)(st))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\t'))(x3)(st))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\n'))(x4)(st))(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\r'))(x5)(st))(x6)(st))(x7)(st))(x8)(st))(st)



c_words'46_'35selFP22'35w :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_words'46_'35selFP22'35w x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_77(x2)(x1)(st))(st)



c_words'46_'35selFP23'35s2 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_words'46_'35selFP23'35s2 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_76(x2)(x1)(st))(st)



c_unwords :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_unwords x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_75(x2)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.List)(x1)(st))(x3)(st))(st)



c_unwords'46_'35lambda6 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_unwords'46_'35lambda6 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(x3))(x1)(st))(st)



c_reverse :: (Curry t0) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0))
c_reverse x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_foldl(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partCons))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pc))((Curry.Module.Prelude.:<)))(st))))(st))(Curry.Module.Prelude.List)))))(st)



c_and :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Bool) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))
c_and x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_38_38))(st))(Curry.Module.Prelude.C_True)))))(st)



c_or :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Bool) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))
c_or x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_124_124))(st))(Curry.Module.Prelude.C_False)))))(st)



c_any :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))
c_any x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_46(Curry.Module.OraclePrelude.c_or(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(x2)))))(x3)(st))(st)



c_all :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))
c_all x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_46(Curry.Module.OraclePrelude.c_and(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(x2)))))(x3)(st))(st)



c_elem :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))
c_elem x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_any(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.op_61_61(x2)))))(x1)(st))(st)



c_notElem :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))
c_notElem x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_all(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.op_47_61(x2)))))(x1)(st))(st)



c_lookup :: (Curry t0,Curry t1) => t0 -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 t1)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t1
c_lookup x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_74(x2)(x3)(x1)(st))(st)



c_enumFrom :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_enumFrom x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(x2)(Curry.Module.OraclePrelude.c_enumFrom(Curry.Module.OraclePrelude.op_43(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x3)(st)))(st)



c_enumFromThen :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_enumFromThen x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_iterate(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_45(x3)(x2)(x1)(st))))))(x2)(x4)(st))(st)



c_enumFromTo :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_enumFromTo x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_70(x2)(x3)(Curry.Module.OraclePrelude.op_62(x2)(x3)(x1)(st))(x4)(st))(st)



c_enumFromThenTo :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_enumFromThenTo x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_takeWhile(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_enumFromThenTo'46p'46364(x4)(x2)(x3)))))(Curry.Module.OraclePrelude.c_enumFromThen(x2)(x3)(x1)(st))(x5)(st))(st)



c_enumFromThenTo'46p'46364 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_enumFromThenTo'46p'46364 x2 x3 x4 x5 x1 st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_69(x2)(x3)(x4)(x5)(Curry.Module.OraclePrelude.op_62_61(x4)(x3)(x1)(st))(x6)(st))(st)



c_ord :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ord x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_prim_ord))))(x2)(x1)(st))(st)



c_chr :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_chr x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_prim_chr))))(x2)(x1)(st))(st)



c_succ :: Curry.Module.Prelude.C_Nat -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Nat
c_succ x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_67(x2)(x1)(st))(st)



op_43_94 :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Nat
op_43_94 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_66(x3)(x2)(x1)(st))(st)



c_cmpNat :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_cmpNat x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_63(x3)(x2)(x1)(st))(st)



c_cmpNatLT :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_cmpNatLT x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_59(x3)(x2)(x1)(st))(st)



c_cmpNatGT :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_cmpNatGT x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_56(x2)(x3)(x1)(st))(st)



op_60_94 :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_60_94 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_isLT(Curry.Module.OraclePrelude.c_cmpNat(x2)(x3)(x1)(st))(x4)(st))(st)



op_62_94 :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_62_94 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_isGT(Curry.Module.OraclePrelude.c_cmpNat(x2)(x3)(x1)(st))(x4)(st))(st)



op_60_61_94 :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_60_61_94 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_not(Curry.Module.OraclePrelude.c_isGT(Curry.Module.OraclePrelude.c_cmpNat(x2)(x3)(x1)(st))(x4)(st))(x5)(st))(st)



op_62_61_94 :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_62_61_94 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_not(Curry.Module.OraclePrelude.c_isLT(Curry.Module.OraclePrelude.c_cmpNat(x2)(x3)(x1)(st))(x4)(st))(x5)(st))(st)



op_42_94 :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Nat
op_42_94 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_53(x3)(x2)(x1)(st))(st)



c_pred :: Curry.Module.Prelude.C_Nat -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Nat
c_pred x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_52(x2)(x1)(st))(st)



c_inc :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_inc x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_50(x2)(x1)(st))(st)



c_dec :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_dec x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_48(x2)(x1)(st))(st)



c_mult2 :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_mult2 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_46(x2)(x1)(st))(st)



op_45_94 :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
op_45_94 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_45(x3)(x2)(x1)(st))(st)



c_div2 :: Curry.Module.Prelude.C_Nat -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Nat
c_div2 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_42(x2)(x1)(st))(st)



c_mod2 :: Curry.Module.Prelude.C_Nat -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_mod2 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_41(x2)(x1)(st))(st)



c_divmodNat :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int
c_divmodNat x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_40(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_IHi)(x1)(st))(x4)(st))(st)



c_divmodNat'46shift'46523 :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Nat
c_divmodNat'46shift'46523 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_32(x3)(x2)(x1)(st))(st)



op_43 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
op_43 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_31(x3)(x2)(x1)(st))(st)



op_45 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
op_45 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_28(x2)(x3)(x1)(st))(st)



op_42 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
op_42 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_27(x3)(x2)(x1)(st))(st)



c_divmod :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int
c_divmod x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_24(x3)(x2)(x1)(st))(st)



c_divmod'46_'35selFP25'35d :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_divmod'46_'35selFP25'35d x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_21(x2)(x1)(st))(st)



c_divmod'46_'35selFP26'35m :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_divmod'46_'35selFP26'35m x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_20(x2)(x1)(st))(st)



c_divmod'46_'35selFP28'35d :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_divmod'46_'35selFP28'35d x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_19(x2)(x1)(st))(st)



c_divmod'46_'35selFP29'35m :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_divmod'46_'35selFP29'35m x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_18(x2)(x1)(st))(st)



c_divmod'46_'35selFP31'35d :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_divmod'46_'35selFP31'35d x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_17(x2)(x1)(st))(st)



c_divmod'46_'35selFP32'35m :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_divmod'46_'35selFP32'35m x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_16(x2)(x1)(st))(st)



c_div :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_div x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_fst(Curry.Module.OraclePrelude.c_divmod(x2)(x3)(x1)(st))(x4)(st))(st)



c_mod :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_mod x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_snd(Curry.Module.OraclePrelude.c_divmod(x2)(x3)(x1)(st))(x4)(st))(st)



c_negate :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_negate x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_15(x2)(x1)(st))(st)



c_success :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success
c_success x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Success)(st)



op_61_58_61 :: (Curry t0) => t0 -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success
op_61_58_61 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_14(x2)(x3)(Curry.Module.OraclePrelude.op_61_61_61(x2)(x3)(x1)(st))(x4)(st))(st)



op_38_62 :: (Curry t0) => Curry.Module.Prelude.C_Success -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
op_38_62 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_cond(x2)(x3)(x1)(st))(st)



c_maybe :: (Curry t0,Curry t1) => t0 -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t0))) -> (Curry.Module.Prelude.C_Maybe t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_maybe x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_13(x2)(x3)(x4)(x1)(st))(st)



c_either :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t2 -> Curry.RunTimeSystem.State -> t1))) -> (Curry.Module.Prelude.C_Either t0 t2) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t1
c_either x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_12(x2)(x3)(x4)(x1)(st))(st)



op_62_62 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1))
op_62_62 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.op_62_62_61(x2)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_const(x3)))))(x1)(st))(st)



c_done :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_done x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_return(Curry.Module.Prelude.T0)(x1)(st))(st)



c_putChar :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_putChar x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_prim_putChar))))(x2)(x1)(st))(st)



c_readFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_readFile x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_prim_readFile))))(x2)(x1)(st))(st)



c_writeFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_writeFile x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_36_35_35(Curry.Module.Oracle.op_36_35_35(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.c_prim_writeFile))(st))(x2)(x1)(st))(x3)(x4)(st))(st)



c_appendFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_appendFile x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_36_35_35(Curry.Module.Oracle.op_36_35_35(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.c_prim_appendFile))(st))(x2)(x1)(st))(x3)(x4)(st))(st)



c_putStr :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_putStr x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_11(x2)(x1)(st))(st)



c_putStrLn :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_putStrLn x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_62_62(Curry.Module.OraclePrelude.c_putStr(x2)(x1)(st))(Curry.Module.OraclePrelude.c_putChar(Curry.Module.Prelude.C_Char('\n'))(x3)(st))(x4)(st))(st)



c_getLine :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_getLine x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OraclePrelude.c_getChar(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_getLine'46_'35lambda10))))(x2)(st))(st)



c_getLine'46_'35lambda10 :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_getLine'46_'35lambda10 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_10(x2)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\n'))(x1)(st))(x3)(st))(st)



c_getLine'46_'35lambda10'46_'35lambda11 :: Curry.Module.Prelude.C_Char -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_getLine'46_'35lambda10'46_'35lambda11 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_return((Curry.Module.Prelude.:<)(x2)(x3))(x1)(st))(st)



c_show :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_show x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_prim_show))))(x2)(x1)(st))(st)



c_print :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_print x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_putStrLn(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))(x3)(st))(st)



c_doSolve :: Curry.Module.Prelude.C_Success -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_doSolve x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_cond(x2)(Curry.Module.OraclePrelude.c_done(x1)(st))(x3)(st))(st)



c_sequenceIO :: (Curry t0) => (Curry.Module.Prelude.List (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0)))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List t0)))
c_sequenceIO x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_9(x2)(x1)(st))(st)



c_sequenceIO'46_'35lambda12 :: (Curry t937) => (Curry.Module.Prelude.List (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t937)))) -> t937 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List t937)))
c_sequenceIO'46_'35lambda12 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OraclePrelude.c_sequenceIO(x2)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_sequenceIO'46_'35lambda12'46_'35lambda13(x3)))))(x4)(st))(st)



c_sequenceIO'46_'35lambda12'46_'35lambda13 :: (Curry t937) => t937 -> (Curry.Module.Prelude.List t937) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List t937)))
c_sequenceIO'46_'35lambda12'46_'35lambda13 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_return((Curry.Module.Prelude.:<)(x2)(x3))(x1)(st))(st)



c_sequenceIO_ :: (Curry t0) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))))
c_sequenceIO_ x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_62_62))(st))(Curry.Module.OraclePrelude.c_done(x1)(st))))))(st)



c_mapIO :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List t1)))))
c_mapIO x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_sequenceIO))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(x2)))))(x1)(st))(st)



c_mapIO_ :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))))
c_mapIO_ x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_46(Curry.Module.OraclePrelude.c_sequenceIO_(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(x2)))))(x3)(st))(st)



op_63 :: (Curry t0) => t0 -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
op_63 x2 x3 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.RunTimeSystem.orF(x2)(x3))(st)



c_allValuesD :: (Curry t0) => (Curry.Module.Prelude.C_SearchTree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_allValuesD x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_8(x2)(x1)(st))(st)



c_allValuesB :: (Curry t0) => (Curry.Module.Prelude.C_SearchTree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_allValuesB x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_allValuesB'46unfoldOrs'46692((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(x1)(st))(st)



c_allValuesB'46partition'46692 :: (Curry t0) => (Curry.Module.Prelude.C_SearchTree t0) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t0))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t0))
c_allValuesB'46partition'46692 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_7(x3)(x2)(x1)(st))(st)



c_allValuesB'46partition'46692'46_'35selFP34'35vs :: (Curry t1001) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1001) (Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t1001))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1001
c_allValuesB'46partition'46692'46_'35selFP34'35vs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_6(x2)(x1)(st))(st)



c_allValuesB'46partition'46692'46_'35selFP35'35ors :: (Curry t1001) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1001) (Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t1001))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t1001)
c_allValuesB'46partition'46692'46_'35selFP35'35ors x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_5(x2)(x1)(st))(st)



c_allValuesB'46partition'46692'46_'35selFP37'35vs :: (Curry t1001) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1001) (Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t1001))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1001
c_allValuesB'46partition'46692'46_'35selFP37'35vs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_4(x2)(x1)(st))(st)



c_allValuesB'46partition'46692'46_'35selFP38'35ors :: (Curry t1001) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1001) (Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t1001))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t1001)
c_allValuesB'46partition'46692'46_'35selFP38'35ors x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_3(x2)(x1)(st))(st)



c_allValuesB'46unfoldOrs'46692 :: (Curry t0) => (Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t0)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_allValuesB'46unfoldOrs'46692 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_2(x2)(x1)(st))(st)



c_allValuesB'46unfoldOrs'46692'46_'35selFP40'35vals :: (Curry t1014) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1014) (Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t1014))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1014
c_allValuesB'46unfoldOrs'46692'46_'35selFP40'35vals x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_1(x2)(x1)(st))(st)



c_allValuesB'46unfoldOrs'46692'46_'35selFP41'35ors :: (Curry t1014) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1014) (Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t1014))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t1014)
c_allValuesB'46unfoldOrs'46692'46_'35selFP41'35ors x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_0(x2)(x1)(st))(st)



c_inject :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success))
c_inject x2 x3 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_inject'46_'35lambda14(x2)(x3)))))(st)



c_inject'46_'35lambda14 :: (Curry t1025) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1025 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1025 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success))) -> t1025 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success
c_inject'46_'35lambda14 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_38(Curry.Module.Oracle.c_apply(x3)(x4)(x1)(st))(Curry.Module.Oracle.c_apply(x2)(x4)(x5)(st))(x6)(st))(st)



c_PEVAL :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_PEVAL x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)



c_unknown :: (Curry t0) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_unknown x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.c_unknown(x1)(st))(st)



c__case_0 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_0_case__155(x1)(x2)(st))(st)



c__case_1 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_1_case__154(x1)(x2)(st))(st)



c__case_2 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_2_case__153(x1)(x2)(st))(st)



c__case_3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_3_case__152(x1)(x2)(st))(st)



c__case_4 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_4_case__151(x1)(x2)(st))(st)



c__case_5 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_5_case__150(x1)(x2)(st))(st)



c__case_6 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_6_case__149(x1)(x2)(st))(st)



c__case_7 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_7_case__148(x1)(x3)(x2)(st))(st)



c__case_8 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_8_case__147(x1)(x2)(st))(st)



c__case_9 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_9_case__146(x1)(x2)(st))(st)



c__case_10 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_10_case__145(x1)(x2)(x3)(st))(st)



c__case_11 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_11_case__144(x1)(x2)(st))(st)



c__case_12 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_12_case__143(x1)(x2)(x3)(x4)(st))(st)



c__case_13 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_13_case__142(x1)(x2)(x3)(x4)(st))(st)



c__case_14 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_14_case__141(x1)(x4)(st))(st)



c__case_15 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_15_case__140(x1)(x2)(st))(st)



c__case_16 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_16_case__139(x1)(x2)(st))(st)



c__case_17 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_17_case__138(x1)(x2)(st))(st)



c__case_18 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_18_case__137(x1)(x2)(st))(st)



c__case_19 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_19_case__136(x1)(x2)(st))(st)



c__case_20 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_20_case__135(x1)(x2)(st))(st)



c__case_21 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_21_case__134(x1)(x2)(st))(st)



c__case_24 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_24_case__133(x1)(x3)(x2)(st))(st)



c__case_22 x10 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_22_case__132(x1)(x10)(x3)(st))(st)



c__case_23 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_23_case__131(x1)(x4)(x3)(st))(st)



c__case_27 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_27_case__130(x1)(x3)(x2)(st))(st)



c__case_25 x7 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_25_case__129(x1)(x7)(x3)(st))(st)



c__case_26 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_26_case__128(x1)(x4)(x3)(st))(st)



c__case_28 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_28_case__127(x1)(x2)(x3)(st))(st)



c__case_31 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_31_case__126(x1)(x3)(x2)(st))(st)



c__case_29 x2 x7 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_29_case__125(x1)(x2)(x7)(x3)(st))(st)



c__case_30 x2 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_30_case__124(x1)(x2)(x4)(x3)(st))(st)



c__case_32 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_32_case__123(x1)(x3)(x2)(st))(st)



c__case_40 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_40_case__122(x1)(x2)(x3)(x4)(st))(st)



c__case_39 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_39_case__121(x1)(x2)(x3)(x4)(st))(st)



c__case_38 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_38_case__120(x1)(x2)(x3)(x4)(st))(st)



c__case_37 x2 x3 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_37_case__119(x1)(x2)(x3)(x6)(st))(st)



c__case_36 x2 x3 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_36_case__118(x1)(x2)(x3)(x5)(x4)(st))(st)



c__case_35 x2 x3 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_35_case__117(x1)(x2)(x3)(x6)(x5)(st))(st)



c__case_34 x2 x3 x6 x7 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_34_case__116(x1)(x6)(x10)(st))(st)



c__case_33 x6 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_33_case__115(x1)(x6)(x9)(x8)(st))(st)



c__case_41 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_41_case__114(x1)(x2)(st))(st)



c__case_42 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_42_case__113(x1)(x2)(st))(st)



c__case_45 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_45_case__112(x1)(x3)(x2)(st))(st)



c__case_43 x7 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_43_case__111(x1)(x7)(x3)(st))(st)



c__case_44 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_44_case__110(x1)(x4)(x3)(st))(st)



c__case_46 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_46_case__109(x1)(x2)(st))(st)



c__case_48 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_48_case__108(x1)(x2)(st))(st)



c__case_47 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_47_case__107(x1)(x4)(st))(st)



c__case_50 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_50_case__106(x1)(x2)(st))(st)



c__case_49 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_49_case__105(x1)(x4)(st))(st)



c__case_52 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_52_case__104(x1)(x2)(st))(st)



c__case_51 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_51_case__103(x1)(x3)(st))(st)



c__case_53 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_53_case__102(x1)(x3)(x2)(st))(st)



c__case_56 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_56_case__101(x1)(x2)(x3)(st))(st)



c__case_54 x7 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_54_case__100(x1)(x7)(x2)(st))(st)



c__case_55 x4 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_55_case__99(x1)(x4)(x2)(st))(st)



c__case_59 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_59_case__98(x1)(x3)(x2)(st))(st)



c__case_57 x7 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_57_case__97(x1)(x7)(x3)(st))(st)



c__case_58 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_58_case__96(x1)(x4)(x3)(st))(st)



c__case_63 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_63_case__95(x1)(x3)(x2)(st))(st)



c__case_60 x9 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_60_case__94(x1)(x9)(x3)(st))(st)



c__case_61 x6 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_61_case__93(x1)(x6)(x3)(st))(st)



c__case_62 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_62_case__92(x1)(x3)(st))(st)



c__case_66 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_66_case__91(x1)(x3)(x2)(st))(st)



c__case_64 x7 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_64_case__90(x1)(x7)(x3)(st))(st)



c__case_65 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_65_case__89(x1)(x4)(x3)(st))(st)



c__case_67 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_67_case__88(x1)(x2)(st))(st)



c__case_69 x2 x3 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_69_case__87(x1)(x2)(x5)(x6)(st))(st)



c__case_68 x2 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_68_case__86(x1)(x2)(x5)(x6)(st))(st)



c__case_70 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_70_case__85(x1)(x2)(x3)(x4)(st))(st)



c__case_74 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_74_case__84(x1)(x2)(x3)(st))(st)



c__case_73 x2 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_73_case__83(x1)(x2)(x5)(x4)(st))(st)



c__case_72 x2 x5 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_72_case__82(x1)(x2)(x5)(x7)(x8)(st))(st)



c__case_71 x2 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_71_case__81(x1)(x2)(x5)(x6)(st))(st)



c__case_75 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_75_case__80(x1)(x2)(x3)(st))(st)



c__case_76 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_76_case__79(x1)(x2)(st))(st)



c__case_77 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_77_case__78(x1)(x2)(st))(st)



c__case_78 x3 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_78_case__77(x1)(x3)(x7)(st))(st)



c__case_79 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_79_case__76(x1)(x2)(st))(st)



c__case_80 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_80_case__75(x1)(x2)(st))(st)



c__case_81 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_81_case__74(x1)(x2)(st))(st)



c__case_82 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_82_case__73(x1)(x2)(st))(st)



c__case_84 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_84_case__72(x1)(x2)(st))(st)



c__case_83 x3 x4 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_83_case__71(x1)(x3)(x4)(x8)(st))(st)



c__case_85 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_85_case__70(x1)(x2)(st))(st)



c__case_86 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_86_case__69(x1)(x2)(st))(st)



c__case_87 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_87_case__68(x1)(x2)(st))(st)



c__case_90 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_90_case__67(x1)(x2)(x3)(st))(st)



c__case_89 x2 x4 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_89_case__66(x1)(x2)(x4)(x5)(x9)(st))(st)



c__case_88 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_88_case__65(x1)(x4)(x5)(x6)(st))(st)



c__case_92 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_92_case__64(x1)(x2)(x3)(st))(st)



c__case_91 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_91_case__63(x1)(x2)(x4)(x5)(x6)(st))(st)



c__case_94 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_94_case__62(x1)(x2)(x3)(st))(st)



c__case_93 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_93_case__61(x1)(x2)(x4)(x5)(x6)(st))(st)



c__case_95 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_95_case__60(x1)(x2)(st))(st)



c__case_96 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_96_case__59(x1)(x2)(st))(st)



c__case_97 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_97_case__58(x1)(x2)(x3)(st))(st)



c__case_98 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_98_case__57(x1)(x2)(x3)(x4)(st))(st)



c__case_99 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_99_case__56(x1)(x2)(x3)(st))(st)



c__case_100 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_100_case__55(x1)(x2)(x3)(x4)(st))(st)



c__case_102 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_102_case__54(x1)(x3)(x2)(st))(st)



c__case_101 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_101_case__53(x1)(x5)(x3)(st))(st)



c__case_103 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_103_case__52(x1)(x2)(st))(st)



c__case_104 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_104_case__51(x1)(x2)(st))(st)



c__case_105 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_105_case__50(x1)(x2)(st))(st)



c__case_107 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_107_case__49(x1)(x2)(st))(st)



c__case_106 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_106_case__48(x1)(x4)(x3)(st))(st)



c__case_108 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_108_case__47(x1)(x2)(st))(st)



c__case_109 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_109_case__46(x1)(x2)(st))(st)



c__case_111 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_111_case__45(x1)(x2)(st))(st)



c__case_110 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_110_case__44(x1)(x4)(x3)(st))(st)



c__case_114 x2 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_114_case__43(x1)(x2)(x4)(x5)(x3)(st))(st)



c__case_113 x2 x5 x6 x7 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_113_case__42(x1)(x2)(x5)(x6)(x7)(x4)(st))(st)



c__case_112 x2 x6 x7 x8 x9 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_112_case__41(x1)(x2)(x6)(x7)(x8)(x9)(x5)(st))(st)



c__case_116 x2 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_116_case__40(x1)(x2)(x4)(x3)(st))(st)



c__case_115 x2 x5 x6 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_115_case__39(x1)(x2)(x5)(x6)(x4)(st))(st)



c__case_119 x3 x4 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_119_case__38(x1)(x3)(x4)(x2)(st))(st)



c__case_118 x4 x5 x6 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_118_case__37(x1)(x4)(x5)(x6)(x3)(st))(st)



c__case_117 x5 x6 x7 x8 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_117_case__36(x1)(x5)(x6)(x7)(x8)(x4)(st))(st)



c__case_121 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_121_case__35(x1)(x3)(x2)(st))(st)



c__case_120 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_120_case__34(x1)(x4)(x5)(x3)(st))(st)



c__case_123 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_123_case__33(x1)(x2)(x3)(st))(st)



c__case_122 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_122_case__32(x1)(x2)(x4)(x5)(x6)(st))(st)



c__case_125 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_125_case__31(x1)(x2)(x3)(st))(st)



c__case_124 x2 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_124_case__30(x1)(x2)(x4)(x5)(st))(st)



c__case_126 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_126_case__29(x1)(x2)(x3)(x4)(st))(st)



c__case_127 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_127_case__28(x1)(x2)(x3)(st))(st)



c__case_128 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_128_case__27(x1)(x2)(x3)(x4)(st))(st)



c__case_129 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_129_case__26(x1)(x2)(x3)(st))(st)



c__case_132 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_132_case__25(x1)(x3)(x2)(st))(st)



c__case_131 x3 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_131_case__24(x1)(x3)(x4)(x5)(x6)(st))(st)



c__case_130 x3 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_130_case__23(x1)(x3)(x5)(x6)(st))(st)



c__case_133 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_133_case__22(x1)(x2)(st))(st)



c__case_134 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_134_case__21(x1)(x3)(x2)(st))(st)



c__case_135 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_135_case__20(x1)(x2)(st))(st)



c__case_136 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_136_case__19(x1)(x2)(st))(st)



c__case_137 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_137_case__18(x1)(x2)(st))(st)



c__case_138 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_138_case__17(x1)(x2)(st))(st)



c__case_139 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_139_case__16(x1)(x2)(st))(st)



c__case_140 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_140_case__15(x1)(x2)(x3)(x4)(st))(st)



c__case_141 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_141_case__14(x1)(x2)(x3)(x4)(st))(st)



c__case_145 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_145_case__13(x1)(x3)(x2)(st))(st)



c__case_142 x9 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_142_case__12(x1)(x9)(x3)(st))(st)



c__case_143 x6 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_143_case__11(x1)(x6)(x3)(st))(st)



c__case_144 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_144_case__10(x1)(x3)(st))(st)



c__case_146 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_146_case__9(x1)(x2)(st))(st)



c__case_147 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_147_case__8(x1)(x2)(st))(st)



c__case_148 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_148_case__7(x1)(x2)(st))(st)



c__case_149 x3 x4 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_149_case__6(x1)(x3)(x4)(x2)(st))(st)



c__case_150 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_150_case__5(x1)(x2)(st))(st)



c__case_151 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_151_case__4(x1)(x3)(x2)(st))(st)



c__case_152 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_152_case__3(x1)(x3)(x2)(st))(st)



c__case_153 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_153_case__2(x1)(x2)(st))(st)



c__case_154 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_154_case__1(x1)(x2)(x3)(x4)(x5)(st))(st)



c__case_155 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_155_case__0(x1)(x2)(x3)(st))(st)



c_prim_error :: (Curry t0) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_prim_error x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.c_prim_error(x2)(st))(st)



c_failed :: (Curry t0) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_failed x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.c_failed(st))(st)



op_61_61 :: (Curry t0) => t0 -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_61_61 x3 x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.op_61_61(x3)(x2)(st))(st)



c_prim_ord :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_prim_ord x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.c_prim_ord(x2)(st))(st)



c_prim_chr :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_prim_chr x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.c_prim_chr(x2)(st))(st)



op_61_61_61 :: (Curry t0) => t0 -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_61_61_61 x3 x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.op_61_61_61(x3)(x2)(st))(st)



op_38 :: Curry.Module.Prelude.C_Success -> Curry.Module.Prelude.C_Success -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success
op_38 x3 x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.op_38(x3)(x2)(st))(st)



c_return :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))
c_return x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Prelude.c_return(x2)(st))))(st)



c_prim_putChar :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_prim_putChar x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Prelude.c_prim_putChar(x2)(st))))(st)



c_getChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Char))
c_getChar x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Oracle.c_safeIOResult(Curry.Module.Prelude.c_getChar(st))(st))))(st)



c_prim_readFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_prim_readFile x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Oracle.c_safeIOResult(Curry.Module.Prelude.c_prim_readFile(x2)(st))(st))))(st)



c_prim_writeFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_prim_writeFile x3 x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Prelude.c_prim_writeFile(x3)(x2)(st))))(st)



c_prim_appendFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_prim_appendFile x3 x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Prelude.c_prim_appendFile(x3)(x2)(st))))(st)



c_prim_show :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_prim_show x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.c_prim_show(x2)(st))(st)



c_getSearchTree :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_SearchTree t0)))
c_getSearchTree x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Prelude.c_getSearchTree(x2)(st))))(st)



c_cond :: (Curry t0) => Curry.Module.Prelude.C_Success -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_cond x3 x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.c_cond(x3)(x2)(st))(st)



op_61_58_60_61 :: (Curry t0) => t0 -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success
op_61_58_60_61 x3 x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.op_61_58_60_61(x3)(x2)(st))(st)



c__case_155_case__0 x1 x2 x3@(Curry.Module.Prelude.T2 x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x4)(x1)(st))(x5)(x6)(st))(st)
c__case_155_case__0 x1 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_155_case__0(x1)(x2)(x)(st))(i)(xs)(st)
c__case_155_case__0 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_155_case__0")(x)



c__case_154_case__1 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_154_case__1 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_until(x2)(x3)(Curry.Module.Oracle.c_apply(x3)(x4)(x1)(st))(x6)(st))(st)
c__case_154_case__1 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_154_case__1(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_154_case__1 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_154_case__1")(x)



c__case_153_case__2 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_153_case__2 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(x3)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_ensureSpine(x1)(st))(x4)(x5)(st)))(st)
c__case_153_case__2 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_153_case__2(x1)(x)(st))(i)(xs)(st)
c__case_153_case__2 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_153_case__2")(x)



c__case_152_case__3 x1 x3 x2@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_152_case__3 x1 x3 x2@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_152_case__3 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_152_case__3(x1)(x3)(x)(st))(i)(xs)(st)
c__case_152_case__3 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_152_case__3")(x)



c__case_151_case__4 x1 x3 x2@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_151_case__4 x1 x3 x2@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_151_case__4 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_151_case__4(x1)(x3)(x)(st))(i)(xs)(st)
c__case_151_case__4 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_151_case__4")(x)



c__case_150_case__5 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_150_case__5 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_150_case__5 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_150_case__5(x1)(x)(st))(i)(xs)(st)
c__case_150_case__5 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_150_case__5")(x)



c__case_149_case__6 x1 x3 x4 x2@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_149_case__6 x1 x3 x4 x2@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_149_case__6 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_149_case__6(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_149_case__6 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_149_case__6")(x)



c__case_148_case__7 x1 x2@Curry.Module.Prelude.C_LT st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_148_case__7 x1 x2@Curry.Module.Prelude.C_GT st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_148_case__7 x1 x2@Curry.Module.Prelude.C_EQ st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_148_case__7 x1 (Curry.Module.Prelude.C_OrderingOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_148_case__7(x1)(x)(st))(i)(xs)(st)
c__case_148_case__7 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_148_case__7")(x)



c__case_147_case__8 x1 x2@Curry.Module.Prelude.C_LT st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_147_case__8 x1 x2@Curry.Module.Prelude.C_GT st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_147_case__8 x1 x2@Curry.Module.Prelude.C_EQ st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_147_case__8 x1 (Curry.Module.Prelude.C_OrderingOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_147_case__8(x1)(x)(st))(i)(xs)(st)
c__case_147_case__8 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_147_case__8")(x)



c__case_146_case__9 x1 x2@Curry.Module.Prelude.C_LT st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_146_case__9 x1 x2@Curry.Module.Prelude.C_GT st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_146_case__9 x1 x2@Curry.Module.Prelude.C_EQ st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_146_case__9 x1 (Curry.Module.Prelude.C_OrderingOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_146_case__9(x1)(x)(st))(i)(xs)(st)
c__case_146_case__9 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_146_case__9")(x)



c__case_144_case__10 x1 x3@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_EQ)(st)
c__case_144_case__10 x1 x3@(Curry.Module.Prelude.C_Pos x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_LT)(st)
c__case_144_case__10 x1 x3@(Curry.Module.Prelude.C_Neg x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_GT)(st)
c__case_144_case__10 x1 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_144_case__10(x1)(x)(st))(i)(xs)(st)
c__case_144_case__10 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_144_case__10")(x)



c__case_143_case__11 x1 x6 x3@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_GT)(st)
c__case_143_case__11 x1 x6 x3@(Curry.Module.Prelude.C_Pos x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_cmpNat(x6)(x7)(x1)(st))(st)
c__case_143_case__11 x1 x6 x3@(Curry.Module.Prelude.C_Neg x8) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_GT)(st)
c__case_143_case__11 x1 x6 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_143_case__11(x1)(x6)(x)(st))(i)(xs)(st)
c__case_143_case__11 x1 x6 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_143_case__11")(x)



c__case_142_case__12 x1 x9 x3@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_LT)(st)
c__case_142_case__12 x1 x9 x3@(Curry.Module.Prelude.C_Pos x10) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_LT)(st)
c__case_142_case__12 x1 x9 x3@(Curry.Module.Prelude.C_Neg x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_cmpNat(x11)(x9)(x1)(st))(st)
c__case_142_case__12 x1 x9 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_142_case__12(x1)(x9)(x)(st))(i)(xs)(st)
c__case_142_case__12 x1 x9 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_142_case__12")(x)



c__case_145_case__13 x1 x3 x2@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_144(x3)(x1)(st))(st)
c__case_145_case__13 x1 x3 x2@(Curry.Module.Prelude.C_Pos x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_143(x6)(x3)(x1)(st))(st)
c__case_145_case__13 x1 x3 x2@(Curry.Module.Prelude.C_Neg x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_142(x9)(x3)(x1)(st))(st)
c__case_145_case__13 x1 x3 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_145_case__13(x1)(x3)(x)(st))(i)(xs)(st)
c__case_145_case__13 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_145_case__13")(x)



c__case_141_case__14 x1 x2 x3 x4@Curry.Module.Prelude.C_LT st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_141_case__14 x1 x2 x3 x4@Curry.Module.Prelude.C_EQ st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_141_case__14 x1 x2 x3 x4@Curry.Module.Prelude.C_GT st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_141_case__14 x1 x2 x3 (Curry.Module.Prelude.C_OrderingOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_141_case__14(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_141_case__14 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_141_case__14")(x)



c__case_140_case__15 x1 x2 x3 x4@Curry.Module.Prelude.C_GT st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_140_case__15 x1 x2 x3 x4@Curry.Module.Prelude.C_LT st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_140_case__15 x1 x2 x3 x4@Curry.Module.Prelude.C_EQ st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_140_case__15 x1 x2 x3 (Curry.Module.Prelude.C_OrderingOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_140_case__15(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_140_case__15 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_140_case__15")(x)



c__case_139_case__16 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_139_case__16 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_139_case__16(x1)(x)(st))(i)(xs)(st)
c__case_139_case__16 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_139_case__16")(x)



c__case_138_case__17 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_138_case__17 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_138_case__17(x1)(x)(st))(i)(xs)(st)
c__case_138_case__17 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_138_case__17")(x)



c__case_137_case__18 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_137_case__18 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_137_case__18(x1)(x)(st))(i)(xs)(st)
c__case_137_case__18 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_137_case__18")(x)



c__case_136_case__19 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_136_case__19 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_136_case__19(x1)(x)(st))(i)(xs)(st)
c__case_136_case__19 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_136_case__19")(x)



c__case_135_case__20 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_135_case__20 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_135_case__20 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_135_case__20(x1)(x)(st))(i)(xs)(st)
c__case_135_case__20 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_135_case__20")(x)



c__case_134_case__21 x1 x3 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_134_case__21 x1 x3 x2@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.OraclePrelude.op_43_43(x5)(x3)(x1)(st)))(st)
c__case_134_case__21 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_134_case__21(x1)(x3)(x)(st))(i)(xs)(st)
c__case_134_case__21 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_134_case__21")(x)



c__case_133_case__22 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Zero)(st)
c__case_133_case__22 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(Curry.Module.OraclePrelude.c_length(x4)(x1)(st))(x5)(st))(st)
c__case_133_case__22 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_133_case__22(x1)(x)(st))(i)(xs)(st)
c__case_133_case__22 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_133_case__22")(x)



c__case_130_case__23 x1 x3 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_33_33(x5)(Curry.Module.OraclePrelude.op_45(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x7)(st))(st)
c__case_130_case__23 x1 x3 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_130_case__23 x1 x3 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_130_case__23(x1)(x3)(x5)(x)(st))(i)(xs)(st)
c__case_130_case__23 x1 x3 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_130_case__23")(x)



c__case_131_case__24 x1 x3 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_131_case__24 x1 x3 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_130(x3)(x5)(Curry.Module.OraclePrelude.op_62(x3)(Curry.Module.Prelude.C_Zero)(x1)(st))(x7)(st))(st)
c__case_131_case__24 x1 x3 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_131_case__24(x1)(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_131_case__24 x1 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_131_case__24")(x)



c__case_132_case__25 x1 x3 x2@((Curry.Module.Prelude.:<) x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_131(x3)(x4)(x5)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Zero)(x1)(st))(x6)(st))(st)
c__case_132_case__25 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_132_case__25(x1)(x3)(x)(st))(i)(xs)(st)
c__case_132_case__25 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_132_case__25")(x)



c__case_129_case__26 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_129_case__26 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.Oracle.c_apply(x2)(x4)(x1)(st))(Curry.Module.OraclePrelude.c_map(x2)(x5)(x6)(st)))(st)
c__case_129_case__26 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_129_case__26(x1)(x2)(x)(st))(i)(xs)(st)
c__case_129_case__26 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_129_case__26")(x)



c__case_128_case__27 x1 x2 x3 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_128_case__27 x1 x2 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_foldl(x2)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x3)(x1)(st))(x5)(x7)(st))(x6)(x8)(st))(st)
c__case_128_case__27 x1 x2 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_128_case__27(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_128_case__27 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_128_case__27")(x)



c__case_127_case__28 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_foldl(x2)(x4)(x5)(x1)(st))(st)
c__case_127_case__28 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_127_case__28(x1)(x2)(x)(st))(i)(xs)(st)
c__case_127_case__28 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_127_case__28")(x)



c__case_126_case__29 x1 x2 x3 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_126_case__29 x1 x2 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x5)(x1)(st))(Curry.Module.OraclePrelude.c_foldr(x2)(x3)(x6)(x7)(st))(x8)(st))(st)
c__case_126_case__29 x1 x2 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_126_case__29(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_126_case__29 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_126_case__29")(x)



c__case_124_case__30 x1 x2 x4 x5@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_124_case__30 x1 x2 x4 x5@((Curry.Module.Prelude.:<) x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x4)(x1)(st))(Curry.Module.OraclePrelude.c_foldr1(x2)((Curry.Module.Prelude.:<)(x6)(x7))(x8)(st))(x9)(st))(st)
c__case_124_case__30 x1 x2 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_124_case__30(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_124_case__30 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_124_case__30")(x)



c__case_125_case__31 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_124(x2)(x4)(x5)(x1)(st))(st)
c__case_125_case__31 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_125_case__31(x1)(x2)(x)(st))(i)(xs)(st)
c__case_125_case__31 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_125_case__31")(x)



c__case_122_case__32 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.OraclePrelude.c_filter(x2)(x5)(x1)(st)))(st)
c__case_122_case__32 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_filter(x2)(x5)(x1)(st))(st)
c__case_122_case__32 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_122_case__32(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_122_case__32 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_122_case__32")(x)



c__case_123_case__33 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_123_case__33 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_122(x2)(x4)(x5)(Curry.Module.Oracle.c_apply(x2)(x4)(x1)(st))(x6)(st))(st)
c__case_123_case__33 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_123_case__33(x1)(x2)(x)(st))(i)(xs)(st)
c__case_123_case__33 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_123_case__33")(x)



c__case_120_case__34 x1 x4 x5 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_120_case__34 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(x4)(x6))(Curry.Module.OraclePrelude.c_zip(x5)(x7)(x1)(st)))(st)
c__case_120_case__34 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_120_case__34(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_120_case__34 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_120_case__34")(x)



c__case_121_case__35 x1 x3 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_121_case__35 x1 x3 x2@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_120(x4)(x5)(x3)(x1)(st))(st)
c__case_121_case__35 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_121_case__35(x1)(x3)(x)(st))(i)(xs)(st)
c__case_121_case__35 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_121_case__35")(x)



c__case_117_case__36 x1 x5 x6 x7 x8 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_117_case__36 x1 x5 x6 x7 x8 x4@((Curry.Module.Prelude.:<) x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T3(x5)(x7)(x9))(Curry.Module.OraclePrelude.c_zip3(x6)(x8)(x10)(x1)(st)))(st)
c__case_117_case__36 x1 x5 x6 x7 x8 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_117_case__36(x1)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_117_case__36 x1 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_117_case__36")(x)



c__case_118_case__37 x1 x4 x5 x6 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_118_case__37 x1 x4 x5 x6 x3@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_117(x5)(x6)(x7)(x8)(x4)(x1)(st))(st)
c__case_118_case__37 x1 x4 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_118_case__37(x1)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_118_case__37 x1 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_118_case__37")(x)



c__case_119_case__38 x1 x3 x4 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_119_case__38 x1 x3 x4 x2@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_118(x4)(x5)(x6)(x3)(x1)(st))(st)
c__case_119_case__38 x1 x3 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_119_case__38(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_119_case__38 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_119_case__38")(x)



c__case_115_case__39 x1 x2 x5 x6 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_115_case__39 x1 x2 x5 x6 x4@((Curry.Module.Prelude.:<) x7 x8) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))((Curry.Module.Prelude.:<)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x5)(x1)(st))(x7)(x9)(st))(Curry.Module.OraclePrelude.c_zipWith(x2)(x6)(x8)(x10)(st)))(st)
c__case_115_case__39 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_115_case__39(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_115_case__39 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_115_case__39")(x)



c__case_116_case__40 x1 x2 x4 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_116_case__40 x1 x2 x4 x3@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_115(x2)(x5)(x6)(x4)(x1)(st))(st)
c__case_116_case__40 x1 x2 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_116_case__40(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_116_case__40 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_116_case__40")(x)



c__case_112_case__41 x1 x2 x6 x7 x8 x9 x5@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_112_case__41 x1 x2 x6 x7 x8 x9 x5@((Curry.Module.Prelude.:<) x10 x11) st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x6)(x1)(st))(x8)(x12)(st))(x10)(x13)(st))(Curry.Module.OraclePrelude.c_zipWith3(x2)(x7)(x9)(x11)(x14)(st)))(st)
c__case_112_case__41 x1 x2 x6 x7 x8 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_112_case__41(x1)(x2)(x6)(x7)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_112_case__41 x1 x2 x6 x7 x8 x9 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_112_case__41")(x)



c__case_113_case__42 x1 x2 x5 x6 x7 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_113_case__42 x1 x2 x5 x6 x7 x4@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_112(x2)(x6)(x7)(x8)(x9)(x5)(x1)(st))(st)
c__case_113_case__42 x1 x2 x5 x6 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_113_case__42(x1)(x2)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_113_case__42 x1 x2 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_113_case__42")(x)



c__case_114_case__43 x1 x2 x4 x5 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_114_case__43 x1 x2 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_113(x2)(x5)(x6)(x7)(x4)(x1)(st))(st)
c__case_114_case__43 x1 x2 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_114_case__43(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_114_case__43 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_114_case__43")(x)



c__case_110_case__44 x1 x4 x3@(Curry.Module.Prelude.T2 x5 x6) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(let {x7 = Curry.Module.OraclePrelude.c_unzip(x4)(x1)(st)} in let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x12)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x5)(Curry.Module.OraclePrelude.c_unzip'46_'35selFP3'35xs(x7)(x10)(st)))((Curry.Module.Prelude.:<)(x6)(Curry.Module.OraclePrelude.c_unzip'46_'35selFP4'35ys(x7)(x11)(st))))(st))(st))(st))(st)
c__case_110_case__44 x1 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_110_case__44(x1)(x4)(x)(st))(i)(xs)(st)
c__case_110_case__44 x1 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_110_case__44")(x)



c__case_111_case__45 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(st)
c__case_111_case__45 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_110(x4)(x3)(x1)(st))(st)
c__case_111_case__45 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_111_case__45(x1)(x)(st))(i)(xs)(st)
c__case_111_case__45 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_111_case__45")(x)



c__case_109_case__46 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_109_case__46 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_109_case__46(x1)(x)(st))(i)(xs)(st)
c__case_109_case__46 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_109_case__46")(x)



c__case_108_case__47 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_108_case__47 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_108_case__47(x1)(x)(st))(i)(xs)(st)
c__case_108_case__47 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_108_case__47")(x)



c__case_106_case__48 x1 x4 x3@(Curry.Module.Prelude.T3 x5 x6 x7) st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(let {x8 = Curry.Module.OraclePrelude.c_unzip3(x4)(x1)(st)} in let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x15)(Curry.Module.Prelude.T3((Curry.Module.Prelude.:<)(x5)(Curry.Module.OraclePrelude.c_unzip3'46_'35selFP6'35xs(x8)(x12)(st)))((Curry.Module.Prelude.:<)(x6)(Curry.Module.OraclePrelude.c_unzip3'46_'35selFP7'35ys(x8)(x13)(st)))((Curry.Module.Prelude.:<)(x7)(Curry.Module.OraclePrelude.c_unzip3'46_'35selFP8'35zs(x8)(x14)(st))))(st))(st))(st))(st))(st)
c__case_106_case__48 x1 x4 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_106_case__48(x1)(x4)(x)(st))(i)(xs)(st)
c__case_106_case__48 x1 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_106_case__48")(x)



c__case_107_case__49 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T3(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(st)
c__case_107_case__49 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_106(x4)(x3)(x1)(st))(st)
c__case_107_case__49 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_107_case__49(x1)(x)(st))(i)(xs)(st)
c__case_107_case__49 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_107_case__49")(x)



c__case_105_case__50 x1 x2@(Curry.Module.Prelude.T3 x3 x4 x5) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_105_case__50 x1 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_105_case__50(x1)(x)(st))(i)(xs)(st)
c__case_105_case__50 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_105_case__50")(x)



c__case_104_case__51 x1 x2@(Curry.Module.Prelude.T3 x3 x4 x5) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_104_case__51 x1 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_104_case__51(x1)(x)(st))(i)(xs)(st)
c__case_104_case__51 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_104_case__51")(x)



c__case_103_case__52 x1 x2@(Curry.Module.Prelude.T3 x3 x4 x5) st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_103_case__52 x1 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_103_case__52(x1)(x)(st))(i)(xs)(st)
c__case_103_case__52 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_103_case__52")(x)



c__case_101_case__53 x1 x5 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_101_case__53 x1 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(x6)(Curry.Module.OraclePrelude.c_take(Curry.Module.OraclePrelude.op_45(Curry.Module.Prelude.C_Pos(x5))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x7)(x8)(st)))(st)
c__case_101_case__53 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_101_case__53(x1)(x5)(x)(st))(i)(xs)(st)
c__case_101_case__53 x1 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_101_case__53")(x)



c__case_102_case__54 x1 x3 x2@(Curry.Module.Prelude.C_Neg x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_102_case__54 x1 x3 x2@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_102_case__54 x1 x3 x2@(Curry.Module.Prelude.C_Pos x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_101(x5)(x3)(x1)(st))(st)
c__case_102_case__54 x1 x3 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_102_case__54(x1)(x3)(x)(st))(i)(xs)(st)
c__case_102_case__54 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_102_case__54")(x)



c__case_100_case__55 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_100_case__55 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_drop'46dropp'46272(x2)(x3)(x1)(st))(st)
c__case_100_case__55 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_100_case__55(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_100_case__55 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_100_case__55")(x)



c__case_99_case__56 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_99_case__56 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_drop(Curry.Module.OraclePrelude.op_45(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x5)(x6)(st))(st)
c__case_99_case__56 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_99_case__56(x1)(x2)(x)(st))(i)(xs)(st)
c__case_99_case__56 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_99_case__56")(x)



c__case_98_case__57 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(x3))(st)
c__case_98_case__57 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_splitAt'46splitAtp'46282(x2)(x3)(x1)(st))(st)
c__case_98_case__57 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_98_case__57(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_98_case__57 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_98_case__57")(x)



c__case_97_case__58 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(st)
c__case_97_case__58 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))(let {x6 = Curry.Module.OraclePrelude.c_splitAt(Curry.Module.OraclePrelude.op_45(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x5)(x9)(st)} in let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x12)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x4)(Curry.Module.OraclePrelude.c_splitAt'46splitAtp'46282'46_'35selFP10'35ys(x6)(x10)(st)))(Curry.Module.OraclePrelude.c_splitAt'46splitAtp'46282'46_'35selFP11'35zs(x6)(x11)(st)))(st))(st))(st))(st)
c__case_97_case__58 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_97_case__58(x1)(x2)(x)(st))(i)(xs)(st)
c__case_97_case__58 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_97_case__58")(x)



c__case_96_case__59 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_96_case__59 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_96_case__59(x1)(x)(st))(i)(xs)(st)
c__case_96_case__59 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_96_case__59")(x)



c__case_95_case__60 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_95_case__60 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_95_case__60(x1)(x)(st))(i)(xs)(st)
c__case_95_case__60 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_95_case__60")(x)



c__case_93_case__61 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.OraclePrelude.c_takeWhile(x2)(x5)(x1)(st)))(st)
c__case_93_case__61 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_93_case__61 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_93_case__61(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_93_case__61 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_93_case__61")(x)



c__case_94_case__62 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_94_case__62 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_93(x2)(x4)(x5)(Curry.Module.Oracle.c_apply(x2)(x4)(x1)(st))(x6)(st))(st)
c__case_94_case__62 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_94_case__62(x1)(x2)(x)(st))(i)(xs)(st)
c__case_94_case__62 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_94_case__62")(x)



c__case_91_case__63 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_dropWhile(x2)(x5)(x1)(st))(st)
c__case_91_case__63 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x4)(x5))(st)
c__case_91_case__63 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_91_case__63(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_91_case__63 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_91_case__63")(x)



c__case_92_case__64 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_92_case__64 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_91(x2)(x4)(x5)(Curry.Module.Oracle.c_apply(x2)(x4)(x1)(st))(x6)(st))(st)
c__case_92_case__64 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_92_case__64(x1)(x2)(x)(st))(i)(xs)(st)
c__case_92_case__64 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_92_case__64")(x)



c__case_88_case__65 x1 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)((Curry.Module.Prelude.:<)(x4)(x5)))(st)
c__case_88_case__65 x1 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_88_case__65 x1 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_88_case__65(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_88_case__65 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_88_case__65")(x)



c__case_89_case__66 x1 x2 x4 x5 x9@Curry.Module.Prelude.C_True st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(let {x6 = Curry.Module.OraclePrelude.c_span(x2)(x5)(x1)(st)} in let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x12)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x4)(Curry.Module.OraclePrelude.c_span'46_'35selFP13'35ys(x6)(x10)(st)))(Curry.Module.OraclePrelude.c_span'46_'35selFP14'35zs(x6)(x11)(st)))(st))(st))(st))(st)
c__case_89_case__66 x1 x2 x4 x5 x9@Curry.Module.Prelude.C_False st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_88(x4)(x5)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x13)(st))(st)
c__case_89_case__66 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_89_case__66(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_89_case__66 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_89_case__66")(x)



c__case_90_case__67 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(st)
c__case_90_case__67 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_89(x2)(x4)(x5)(Curry.Module.Oracle.c_apply(x2)(x4)(x1)(st))(x6)(st))(st)
c__case_90_case__67 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_90_case__67(x1)(x2)(x)(st))(i)(xs)(st)
c__case_90_case__67 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_90_case__67")(x)



c__case_87_case__68 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_87_case__68 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_87_case__68(x1)(x)(st))(i)(xs)(st)
c__case_87_case__68 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_87_case__68")(x)



c__case_86_case__69 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_86_case__69 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_86_case__69(x1)(x)(st))(i)(xs)(st)
c__case_86_case__69 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_86_case__69")(x)



c__case_85_case__70 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_85_case__70 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(let {x5 = Curry.Module.OraclePrelude.c_lines'46splitline'46314((Curry.Module.Prelude.:<)(x3)(x4))(x1)(st)} in let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x10)((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.c_lines'46_'35selFP19'35l(x5)(x8)(st))(Curry.Module.OraclePrelude.c_lines(Curry.Module.OraclePrelude.c_lines'46_'35selFP20'35xs_l(x5)(x9)(st))(x10)(st)))(st))(st))(st))(st)
c__case_85_case__70 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_85_case__70(x1)(x)(st))(i)(xs)(st)
c__case_85_case__70 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_85_case__70")(x)



c__case_83_case__71 x1 x3 x4 x8@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(x4))(st)
c__case_83_case__71 x1 x3 x4 x8@Curry.Module.Prelude.C_False st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(let {x5 = Curry.Module.OraclePrelude.c_lines'46splitline'46314(x4)(x1)(st)} in let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x11)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x3)(Curry.Module.OraclePrelude.c_lines'46splitline'46314'46_'35selFP16'35ds(x5)(x9)(st)))(Curry.Module.OraclePrelude.c_lines'46splitline'46314'46_'35selFP17'35es(x5)(x10)(st)))(st))(st))(st))(st)
c__case_83_case__71 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_83_case__71(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_83_case__71 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_83_case__71")(x)



c__case_84_case__72 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(st)
c__case_84_case__72 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_83(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('\n'))(x1)(st))(x5)(st))(st)
c__case_84_case__72 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_84_case__72(x1)(x)(st))(i)(xs)(st)
c__case_84_case__72 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_84_case__72")(x)



c__case_82_case__73 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_82_case__73 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_82_case__73(x1)(x)(st))(i)(xs)(st)
c__case_82_case__73 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_82_case__73")(x)



c__case_81_case__74 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_81_case__74 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_81_case__74(x1)(x)(st))(i)(xs)(st)
c__case_81_case__74 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_81_case__74")(x)



c__case_80_case__75 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_80_case__75 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_80_case__75(x1)(x)(st))(i)(xs)(st)
c__case_80_case__75 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_80_case__75")(x)



c__case_79_case__76 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_79_case__76 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_79_case__76(x1)(x)(st))(i)(xs)(st)
c__case_79_case__76 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_79_case__76")(x)



c__case_78_case__77 x1 x3 x7@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_78_case__77 x1 x3 x7@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(let {x4 = Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_break(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_words'46isSpace'46326))))(x1)(st))(x3)(x8)(st)} in let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x11)((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.c_words'46_'35selFP22'35w(x4)(x9)(st))(Curry.Module.OraclePrelude.c_words(Curry.Module.OraclePrelude.c_words'46_'35selFP23'35s2(x4)(x10)(st))(x11)(st)))(st))(st))(st))(st)
c__case_78_case__77 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_78_case__77(x1)(x3)(x)(st))(i)(xs)(st)
c__case_78_case__77 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_78_case__77")(x)



c__case_77_case__78 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_77_case__78 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_77_case__78(x1)(x)(st))(i)(xs)(st)
c__case_77_case__78 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_77_case__78")(x)



c__case_76_case__79 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_76_case__79 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_76_case__79(x1)(x)(st))(i)(xs)(st)
c__case_76_case__79 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_76_case__79")(x)



c__case_75_case__80 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_75_case__80 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_foldr1(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.c_unwords'46_'35lambda6))(st))(x2)(x1)(st))(st)
c__case_75_case__80 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_75_case__80(x1)(x2)(x)(st))(i)(xs)(st)
c__case_75_case__80 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_75_case__80")(x)



c__case_71_case__81 x1 x2 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_lookup(x2)(x5)(x1)(st))(st)
c__case_71_case__81 x1 x2 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_71_case__81 x1 x2 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_71_case__81(x1)(x2)(x5)(x)(st))(i)(xs)(st)
c__case_71_case__81 x1 x2 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_71_case__81")(x)



c__case_72_case__82 x1 x2 x5 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Just(x7))(st)
c__case_72_case__82 x1 x2 x5 x7 x8@Curry.Module.Prelude.C_False st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_71(x2)(x5)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x9)(st))(st)
c__case_72_case__82 x1 x2 x5 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_72_case__82(x1)(x2)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_72_case__82 x1 x2 x5 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_72_case__82")(x)



c__case_73_case__83 x1 x2 x5 x4@(Curry.Module.Prelude.T2 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_72(x2)(x5)(x6)(x7)(Curry.Module.OraclePrelude.op_61_61(x2)(x6)(x1)(st))(x8)(st))(st)
c__case_73_case__83 x1 x2 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_73_case__83(x1)(x2)(x5)(x)(st))(i)(xs)(st)
c__case_73_case__83 x1 x2 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_73_case__83")(x)



c__case_74_case__84 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_74_case__84 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_73(x2)(x5)(x4)(x1)(st))(st)
c__case_74_case__84 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_74_case__84(x1)(x2)(x)(st))(i)(xs)(st)
c__case_74_case__84 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_74_case__84")(x)



c__case_70_case__85 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_70_case__85 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(x2)(Curry.Module.OraclePrelude.c_enumFromTo(Curry.Module.OraclePrelude.op_43(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x3)(x5)(st)))(st)
c__case_70_case__85 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_70_case__85(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_70_case__85 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_70_case__85")(x)



c__case_68_case__86 x1 x2 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_62_61(x5)(x2)(x1)(st))(st)
c__case_68_case__86 x1 x2 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_68_case__86 x1 x2 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_68_case__86(x1)(x2)(x5)(x)(st))(i)(xs)(st)
c__case_68_case__86 x1 x2 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_68_case__86")(x)



c__case_69_case__87 x1 x2 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_60_61(x5)(x2)(x1)(st))(st)
c__case_69_case__87 x1 x2 x5 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_68(x2)(x5)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x7)(st))(st)
c__case_69_case__87 x1 x2 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_69_case__87(x1)(x2)(x5)(x)(st))(i)(xs)(st)
c__case_69_case__87 x1 x2 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_69_case__87")(x)



c__case_67_case__88 x1 x2@(Curry.Module.Prelude.C_O x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_I(x3))(st)
c__case_67_case__88 x1 x2@(Curry.Module.Prelude.C_I x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_O(Curry.Module.OraclePrelude.c_succ(x4)(x1)(st)))(st)
c__case_67_case__88 x1 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))(st)
c__case_67_case__88 x1 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_67_case__88(x1)(x)(st))(i)(xs)(st)
c__case_67_case__88 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_67_case__88")(x)



c__case_65_case__89 x1 x4 x3@(Curry.Module.Prelude.C_O x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_O(Curry.Module.OraclePrelude.op_43_94(x4)(x5)(x1)(st)))(st)
c__case_65_case__89 x1 x4 x3@(Curry.Module.Prelude.C_I x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_I(Curry.Module.OraclePrelude.op_43_94(x4)(x6)(x1)(st)))(st)
c__case_65_case__89 x1 x4 x3@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_I(x4))(st)
c__case_65_case__89 x1 x4 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_65_case__89(x1)(x4)(x)(st))(i)(xs)(st)
c__case_65_case__89 x1 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_65_case__89")(x)



c__case_64_case__90 x1 x7 x3@(Curry.Module.Prelude.C_O x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_I(Curry.Module.OraclePrelude.op_43_94(x7)(x8)(x1)(st)))(st)
c__case_64_case__90 x1 x7 x3@(Curry.Module.Prelude.C_I x9) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.Prelude.C_O(Curry.Module.OraclePrelude.op_43_94(Curry.Module.OraclePrelude.c_succ(x7)(x1)(st))(x9)(x10)(st)))(st)
c__case_64_case__90 x1 x7 x3@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_O(Curry.Module.OraclePrelude.c_succ(x7)(x1)(st)))(st)
c__case_64_case__90 x1 x7 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_64_case__90(x1)(x7)(x)(st))(i)(xs)(st)
c__case_64_case__90 x1 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_64_case__90")(x)



c__case_66_case__91 x1 x3 x2@(Curry.Module.Prelude.C_O x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_65(x4)(x3)(x1)(st))(st)
c__case_66_case__91 x1 x3 x2@(Curry.Module.Prelude.C_I x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_64(x7)(x3)(x1)(st))(st)
c__case_66_case__91 x1 x3 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_succ(x3)(x1)(st))(st)
c__case_66_case__91 x1 x3 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_66_case__91(x1)(x3)(x)(st))(i)(xs)(st)
c__case_66_case__91 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_66_case__91")(x)



c__case_62_case__92 x1 x3@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_EQ)(st)
c__case_62_case__92 x1 x3@(Curry.Module.Prelude.C_O x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_LT)(st)
c__case_62_case__92 x1 x3@(Curry.Module.Prelude.C_I x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_LT)(st)
c__case_62_case__92 x1 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_62_case__92(x1)(x)(st))(i)(xs)(st)
c__case_62_case__92 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_62_case__92")(x)



c__case_61_case__93 x1 x6 x3@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_GT)(st)
c__case_61_case__93 x1 x6 x3@(Curry.Module.Prelude.C_O x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_cmpNat(x6)(x7)(x1)(st))(st)
c__case_61_case__93 x1 x6 x3@(Curry.Module.Prelude.C_I x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_cmpNatLT(x6)(x8)(x1)(st))(st)
c__case_61_case__93 x1 x6 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_61_case__93(x1)(x6)(x)(st))(i)(xs)(st)
c__case_61_case__93 x1 x6 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_61_case__93")(x)



c__case_60_case__94 x1 x9 x3@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_GT)(st)
c__case_60_case__94 x1 x9 x3@(Curry.Module.Prelude.C_I x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_cmpNat(x9)(x10)(x1)(st))(st)
c__case_60_case__94 x1 x9 x3@(Curry.Module.Prelude.C_O x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_cmpNatGT(x9)(x11)(x1)(st))(st)
c__case_60_case__94 x1 x9 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_60_case__94(x1)(x9)(x)(st))(i)(xs)(st)
c__case_60_case__94 x1 x9 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_60_case__94")(x)



c__case_63_case__95 x1 x3 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_62(x3)(x1)(st))(st)
c__case_63_case__95 x1 x3 x2@(Curry.Module.Prelude.C_O x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_61(x6)(x3)(x1)(st))(st)
c__case_63_case__95 x1 x3 x2@(Curry.Module.Prelude.C_I x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_60(x9)(x3)(x1)(st))(st)
c__case_63_case__95 x1 x3 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_63_case__95(x1)(x3)(x)(st))(i)(xs)(st)
c__case_63_case__95 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_63_case__95")(x)



c__case_58_case__96 x1 x4 x3@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_GT)(st)
c__case_58_case__96 x1 x4 x3@(Curry.Module.Prelude.C_O x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_cmpNatLT(x4)(x5)(x1)(st))(st)
c__case_58_case__96 x1 x4 x3@(Curry.Module.Prelude.C_I x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_cmpNatLT(x4)(x6)(x1)(st))(st)
c__case_58_case__96 x1 x4 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_58_case__96(x1)(x4)(x)(st))(i)(xs)(st)
c__case_58_case__96 x1 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_58_case__96")(x)



c__case_57_case__97 x1 x7 x3@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_GT)(st)
c__case_57_case__97 x1 x7 x3@(Curry.Module.Prelude.C_I x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_cmpNatLT(x7)(x8)(x1)(st))(st)
c__case_57_case__97 x1 x7 x3@(Curry.Module.Prelude.C_O x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_cmpNatGT(x7)(x9)(x1)(st))(st)
c__case_57_case__97 x1 x7 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_57_case__97(x1)(x7)(x)(st))(i)(xs)(st)
c__case_57_case__97 x1 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_57_case__97")(x)



c__case_59_case__98 x1 x3 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_LT)(st)
c__case_59_case__98 x1 x3 x2@(Curry.Module.Prelude.C_O x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_58(x4)(x3)(x1)(st))(st)
c__case_59_case__98 x1 x3 x2@(Curry.Module.Prelude.C_I x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_57(x7)(x3)(x1)(st))(st)
c__case_59_case__98 x1 x3 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_59_case__98(x1)(x3)(x)(st))(i)(xs)(st)
c__case_59_case__98 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_59_case__98")(x)



c__case_55_case__99 x1 x4 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_LT)(st)
c__case_55_case__99 x1 x4 x2@(Curry.Module.Prelude.C_O x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_cmpNatGT(x5)(x4)(x1)(st))(st)
c__case_55_case__99 x1 x4 x2@(Curry.Module.Prelude.C_I x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_cmpNatGT(x6)(x4)(x1)(st))(st)
c__case_55_case__99 x1 x4 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_55_case__99(x1)(x4)(x)(st))(i)(xs)(st)
c__case_55_case__99 x1 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_55_case__99")(x)



c__case_54_case__100 x1 x7 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_LT)(st)
c__case_54_case__100 x1 x7 x2@(Curry.Module.Prelude.C_I x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_cmpNatGT(x8)(x7)(x1)(st))(st)
c__case_54_case__100 x1 x7 x2@(Curry.Module.Prelude.C_O x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_cmpNatLT(x9)(x7)(x1)(st))(st)
c__case_54_case__100 x1 x7 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_54_case__100(x1)(x7)(x)(st))(i)(xs)(st)
c__case_54_case__100 x1 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_54_case__100")(x)



c__case_56_case__101 x1 x2 x3@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_GT)(st)
c__case_56_case__101 x1 x2 x3@(Curry.Module.Prelude.C_O x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_55(x4)(x2)(x1)(st))(st)
c__case_56_case__101 x1 x2 x3@(Curry.Module.Prelude.C_I x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_54(x7)(x2)(x1)(st))(st)
c__case_56_case__101 x1 x2 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_56_case__101(x1)(x2)(x)(st))(i)(xs)(st)
c__case_56_case__101 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_56_case__101")(x)



c__case_53_case__102 x1 x3 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_53_case__102 x1 x3 x2@(Curry.Module.Prelude.C_I x4) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_94(Curry.Module.Prelude.C_O(Curry.Module.OraclePrelude.op_42_94(x3)(x4)(x1)(st)))(x3)(x6)(st))(st)
c__case_53_case__102 x1 x3 x2@(Curry.Module.Prelude.C_O x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_O(Curry.Module.OraclePrelude.op_42_94(x5)(x3)(x1)(st)))(st)
c__case_53_case__102 x1 x3 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_53_case__102(x1)(x3)(x)(st))(i)(xs)(st)
c__case_53_case__102 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_53_case__102")(x)



c__case_51_case__103 x1 x3@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_IHi)(st)
c__case_51_case__103 x1 x3@(Curry.Module.Prelude.C_O x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_I(Curry.Module.OraclePrelude.c_pred(x3)(x1)(st)))(st)
c__case_51_case__103 x1 x3@(Curry.Module.Prelude.C_I x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(x5)))(st)
c__case_51_case__103 x1 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_51_case__103(x1)(x)(st))(i)(xs)(st)
c__case_51_case__103 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_51_case__103")(x)



c__case_52_case__104 x1 x2@(Curry.Module.Prelude.C_O x3) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_51(x3)(x1)(st))(st)
c__case_52_case__104 x1 x2@(Curry.Module.Prelude.C_I x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_O(x6))(st)
c__case_52_case__104 x1 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_52_case__104(x1)(x)(st))(i)(xs)(st)
c__case_52_case__104 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_52_case__104")(x)



c__case_49_case__105 x1 x4@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Zero)(st)
c__case_49_case__105 x1 x4@(Curry.Module.Prelude.C_O x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_Neg(Curry.Module.OraclePrelude.c_pred(Curry.Module.Prelude.C_O(x5))(x1)(st)))(st)
c__case_49_case__105 x1 x4@(Curry.Module.Prelude.C_I x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Neg(Curry.Module.Prelude.C_O(x6)))(st)
c__case_49_case__105 x1 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_49_case__105(x1)(x)(st))(i)(xs)(st)
c__case_49_case__105 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_49_case__105")(x)



c__case_50_case__106 x1 x2@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st)
c__case_50_case__106 x1 x2@(Curry.Module.Prelude.C_Pos x3) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.OraclePrelude.c_succ(x3)(x1)(st)))(st)
c__case_50_case__106 x1 x2@(Curry.Module.Prelude.C_Neg x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_49(x4)(x1)(st))(st)
c__case_50_case__106 x1 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_50_case__106(x1)(x)(st))(i)(xs)(st)
c__case_50_case__106 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_50_case__106")(x)



c__case_47_case__107 x1 x4@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Zero)(st)
c__case_47_case__107 x1 x4@(Curry.Module.Prelude.C_O x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.OraclePrelude.c_pred(Curry.Module.Prelude.C_O(x5))(x1)(st)))(st)
c__case_47_case__107 x1 x4@(Curry.Module.Prelude.C_I x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(x6)))(st)
c__case_47_case__107 x1 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_47_case__107(x1)(x)(st))(i)(xs)(st)
c__case_47_case__107 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_47_case__107")(x)



c__case_48_case__108 x1 x2@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Neg(Curry.Module.Prelude.C_IHi))(st)
c__case_48_case__108 x1 x2@(Curry.Module.Prelude.C_Neg x3) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_Neg(Curry.Module.OraclePrelude.c_succ(x3)(x1)(st)))(st)
c__case_48_case__108 x1 x2@(Curry.Module.Prelude.C_Pos x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_47(x4)(x1)(st))(st)
c__case_48_case__108 x1 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_48_case__108(x1)(x)(st))(i)(xs)(st)
c__case_48_case__108 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_48_case__108")(x)



c__case_46_case__109 x1 x2@(Curry.Module.Prelude.C_Pos x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(x3)))(st)
c__case_46_case__109 x1 x2@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Zero)(st)
c__case_46_case__109 x1 x2@(Curry.Module.Prelude.C_Neg x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Neg(Curry.Module.Prelude.C_O(x4)))(st)
c__case_46_case__109 x1 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_46_case__109(x1)(x)(st))(i)(xs)(st)
c__case_46_case__109 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_46_case__109")(x)



c__case_44_case__110 x1 x4 x3@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.OraclePrelude.c_pred(Curry.Module.Prelude.C_O(x4))(x1)(st)))(st)
c__case_44_case__110 x1 x4 x3@(Curry.Module.Prelude.C_O x5) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_mult2(Curry.Module.OraclePrelude.op_45_94(x4)(x5)(x1)(st))(x7)(st))(st)
c__case_44_case__110 x1 x4 x3@(Curry.Module.Prelude.C_I x6) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_dec(Curry.Module.OraclePrelude.c_mult2(Curry.Module.OraclePrelude.op_45_94(x4)(x6)(x1)(st))(x8)(st))(x9)(st))(st)
c__case_44_case__110 x1 x4 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_44_case__110(x1)(x4)(x)(st))(i)(xs)(st)
c__case_44_case__110 x1 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_44_case__110")(x)



c__case_43_case__111 x1 x7 x3@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(x7)))(st)
c__case_43_case__111 x1 x7 x3@(Curry.Module.Prelude.C_O x8) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_inc(Curry.Module.OraclePrelude.c_mult2(Curry.Module.OraclePrelude.op_45_94(x7)(x8)(x1)(st))(x10)(st))(x11)(st))(st)
c__case_43_case__111 x1 x7 x3@(Curry.Module.Prelude.C_I x9) st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_mult2(Curry.Module.OraclePrelude.op_45_94(x7)(x9)(x1)(st))(x12)(st))(st)
c__case_43_case__111 x1 x7 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_43_case__111(x1)(x7)(x)(st))(i)(xs)(st)
c__case_43_case__111 x1 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_43_case__111")(x)



c__case_45_case__112 x1 x3 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_inc(Curry.Module.Prelude.C_Neg(x3))(x1)(st))(st)
c__case_45_case__112 x1 x3 x2@(Curry.Module.Prelude.C_O x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_44(x4)(x3)(x1)(st))(st)
c__case_45_case__112 x1 x3 x2@(Curry.Module.Prelude.C_I x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_43(x7)(x3)(x1)(st))(st)
c__case_45_case__112 x1 x3 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_45_case__112(x1)(x3)(x)(st))(i)(xs)(st)
c__case_45_case__112 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_45_case__112")(x)



c__case_42_case__113 x1 x2@(Curry.Module.Prelude.C_O x3) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_42_case__113 x1 x2@(Curry.Module.Prelude.C_I x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_42_case__113 x1 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_42_case__113(x1)(x)(st))(i)(xs)(st)
c__case_42_case__113 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_42_case__113")(x)



c__case_41_case__114 x1 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st)
c__case_41_case__114 x1 x2@(Curry.Module.Prelude.C_O x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Zero)(st)
c__case_41_case__114 x1 x2@(Curry.Module.Prelude.C_I x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st)
c__case_41_case__114 x1 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_41_case__114(x1)(x)(st))(i)(xs)(st)
c__case_41_case__114 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_41_case__114")(x)



c__case_33_case__115 x1 x6 x9 x8@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(x6)))(x9))(st)
c__case_33_case__115 x1 x6 x9 x8@(Curry.Module.Prelude.C_Pos x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Pos(Curry.Module.OraclePrelude.op_43_94(Curry.Module.Prelude.C_O(x6))(x10)(x1)(st)))(x9))(st)
c__case_33_case__115 x1 x6 x9 x8@(Curry.Module.Prelude.C_Neg x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_33_case__115 x1 x6 x9 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_33_case__115(x1)(x6)(x9)(x)(st))(i)(xs)(st)
c__case_33_case__115 x1 x6 x9 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_33_case__115")(x)



c__case_34_case__116 x1 x6 x10@(Curry.Module.Prelude.T2 x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_33(x6)(x9)(x8)(x1)(st))(st)
c__case_34_case__116 x1 x6 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_34_case__116(x1)(x6)(x)(st))(i)(xs)(st)
c__case_34_case__116 x1 x6 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_34_case__116")(x)



c__case_35_case__117 x1 x2 x3 x6 x5@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(x6)))(Curry.Module.OraclePrelude.c_mod2(x2)(x1)(st)))(st)
c__case_35_case__117 x1 x2 x3 x6 x5@(Curry.Module.Prelude.C_Pos x7) st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c__case_34(x2)(x3)(x6)(x7)(Curry.Module.OraclePrelude.c_divmodNat(Curry.Module.OraclePrelude.c_divmodNat'46shift'46523(x2)(x7)(x1)(st))(x3)(x13)(st))(x14)(st))(st)
c__case_35_case__117 x1 x2 x3 x6 x5@(Curry.Module.Prelude.C_Neg x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_35_case__117 x1 x2 x3 x6 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_35_case__117(x1)(x2)(x3)(x6)(x)(st))(i)(xs)(st)
c__case_35_case__117 x1 x2 x3 x6 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_35_case__117")(x)



c__case_36_case__118 x1 x2 x3 x5 x4@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(Curry.Module.OraclePrelude.op_45_94(x2)(x3)(x1)(st)))(st)
c__case_36_case__118 x1 x2 x3 x5 x4@(Curry.Module.Prelude.C_Pos x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_35(x2)(x3)(x6)(x5)(x1)(st))(st)
c__case_36_case__118 x1 x2 x3 x5 x4@(Curry.Module.Prelude.C_Neg x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_36_case__118 x1 x2 x3 x5 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_36_case__118(x1)(x2)(x3)(x5)(x)(st))(i)(xs)(st)
c__case_36_case__118 x1 x2 x3 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_36_case__118")(x)



c__case_37_case__119 x1 x2 x3 x6@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_36(x2)(x3)(x5)(x4)(x1)(st))(st)
c__case_37_case__119 x1 x2 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_37_case__119(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_37_case__119 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_37_case__119")(x)



c__case_38_case__120 x1 x2 x3 x4@Curry.Module.Prelude.C_EQ st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(Curry.Module.Prelude.C_Zero))(st)
c__case_38_case__120 x1 x2 x3 x4@Curry.Module.Prelude.C_LT st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.C_Pos(x2)))(st)
c__case_38_case__120 x1 x2 x3 x4@Curry.Module.Prelude.C_GT st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c__case_37(x2)(x3)(Curry.Module.OraclePrelude.c_divmodNat(Curry.Module.OraclePrelude.c_div2(x2)(x1)(st))(x3)(x5)(st))(x6)(st))(st)
c__case_38_case__120 x1 x2 x3 (Curry.Module.Prelude.C_OrderingOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_38_case__120(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_38_case__120 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_38_case__120")(x)



c__case_39_case__121 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_38(x2)(x3)(Curry.Module.OraclePrelude.c_cmpNat(x2)(x3)(x1)(st))(x5)(st))(st)
c__case_39_case__121 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_39_case__121 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_39_case__121(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_39_case__121 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_39_case__121")(x)



c__case_40_case__122 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Pos(x2))(Curry.Module.Prelude.C_Zero))(st)
c__case_40_case__122 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c__case_39(x2)(x3)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x5)(st))(st)
c__case_40_case__122 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_40_case__122(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_40_case__122 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_40_case__122")(x)



c__case_32_case__123 x1 x3 x2@(Curry.Module.Prelude.C_O x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_O(x3))(st)
c__case_32_case__123 x1 x3 x2@(Curry.Module.Prelude.C_I x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_I(x3))(st)
c__case_32_case__123 x1 x3 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_32_case__123(x1)(x3)(x)(st))(i)(xs)(st)
c__case_32_case__123 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_32_case__123")(x)



c__case_30_case__124 x1 x2 x4 x3@(Curry.Module.Prelude.C_Pos x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.OraclePrelude.op_43_94(x4)(x5)(x1)(st)))(st)
c__case_30_case__124 x1 x2 x4 x3@(Curry.Module.Prelude.C_Neg x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_45_94(x4)(x6)(x1)(st))(st)
c__case_30_case__124 x1 x2 x4 x3@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_30_case__124 x1 x2 x4 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_30_case__124(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_30_case__124 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_30_case__124")(x)



c__case_29_case__125 x1 x2 x7 x3@(Curry.Module.Prelude.C_Neg x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_Neg(Curry.Module.OraclePrelude.op_43_94(x7)(x8)(x1)(st)))(st)
c__case_29_case__125 x1 x2 x7 x3@(Curry.Module.Prelude.C_Pos x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_45_94(x9)(x7)(x1)(st))(st)
c__case_29_case__125 x1 x2 x7 x3@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_29_case__125 x1 x2 x7 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_29_case__125(x1)(x2)(x7)(x)(st))(i)(xs)(st)
c__case_29_case__125 x1 x2 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_29_case__125")(x)



c__case_31_case__126 x1 x3 x2@(Curry.Module.Prelude.C_Pos x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_30(x2)(x4)(x3)(x1)(st))(st)
c__case_31_case__126 x1 x3 x2@(Curry.Module.Prelude.C_Neg x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_29(x2)(x7)(x3)(x1)(st))(st)
c__case_31_case__126 x1 x3 x2@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_31_case__126 x1 x3 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_31_case__126(x1)(x3)(x)(st))(i)(xs)(st)
c__case_31_case__126 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_31_case__126")(x)



c__case_28_case__127 x1 x2 x3@(Curry.Module.Prelude.C_Neg x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_43(x2)(Curry.Module.Prelude.C_Pos(x4))(x1)(st))(st)
c__case_28_case__127 x1 x2 x3@(Curry.Module.Prelude.C_Pos x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_43(x2)(Curry.Module.Prelude.C_Neg(x5))(x1)(st))(st)
c__case_28_case__127 x1 x2 x3@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_28_case__127 x1 x2 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_28_case__127(x1)(x2)(x)(st))(i)(xs)(st)
c__case_28_case__127 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_28_case__127")(x)



c__case_26_case__128 x1 x4 x3@(Curry.Module.Prelude.C_Pos x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.OraclePrelude.op_42_94(x4)(x5)(x1)(st)))(st)
c__case_26_case__128 x1 x4 x3@(Curry.Module.Prelude.C_Neg x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_Neg(Curry.Module.OraclePrelude.op_42_94(x4)(x6)(x1)(st)))(st)
c__case_26_case__128 x1 x4 x3@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Zero)(st)
c__case_26_case__128 x1 x4 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_26_case__128(x1)(x4)(x)(st))(i)(xs)(st)
c__case_26_case__128 x1 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_26_case__128")(x)



c__case_25_case__129 x1 x7 x3@(Curry.Module.Prelude.C_Neg x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.OraclePrelude.op_42_94(x7)(x8)(x1)(st)))(st)
c__case_25_case__129 x1 x7 x3@(Curry.Module.Prelude.C_Pos x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.C_Neg(Curry.Module.OraclePrelude.op_42_94(x7)(x9)(x1)(st)))(st)
c__case_25_case__129 x1 x7 x3@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Zero)(st)
c__case_25_case__129 x1 x7 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_25_case__129(x1)(x7)(x)(st))(i)(xs)(st)
c__case_25_case__129 x1 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_25_case__129")(x)



c__case_27_case__130 x1 x3 x2@(Curry.Module.Prelude.C_Pos x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_26(x4)(x3)(x1)(st))(st)
c__case_27_case__130 x1 x3 x2@(Curry.Module.Prelude.C_Neg x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_25(x7)(x3)(x1)(st))(st)
c__case_27_case__130 x1 x3 x2@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Zero)(st)
c__case_27_case__130 x1 x3 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_27_case__130(x1)(x3)(x)(st))(i)(xs)(st)
c__case_27_case__130 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_27_case__130")(x)



c__case_23_case__131 x1 x4 x3@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('0'))(Curry.Module.Prelude.List))))))))))))))(x1)(st))(st)
c__case_23_case__131 x1 x4 x3@(Curry.Module.Prelude.C_Pos x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_divmodNat(x4)(x5)(x1)(st))(st)
c__case_23_case__131 x1 x4 x3@(Curry.Module.Prelude.C_Neg x6) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(let {x7 = Curry.Module.OraclePrelude.c_divmodNat(x4)(x6)(x1)(st)} in let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x12)(Curry.Module.Prelude.T2(Curry.Module.OraclePrelude.c_negate(Curry.Module.OraclePrelude.c_divmod'46_'35selFP25'35d(x7)(x10)(st))(x12)(st))(Curry.Module.OraclePrelude.c_divmod'46_'35selFP26'35m(x7)(x11)(st)))(st))(st))(st))(st)
c__case_23_case__131 x1 x4 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_23_case__131(x1)(x4)(x)(st))(i)(xs)(st)
c__case_23_case__131 x1 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_23_case__131")(x)



c__case_22_case__132 x1 x10 x3@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('0'))(Curry.Module.Prelude.List))))))))))))))(x1)(st))(st)
c__case_22_case__132 x1 x10 x3@(Curry.Module.Prelude.C_Pos x11) st = let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))(let {x12 = Curry.Module.OraclePrelude.c_divmodNat(x10)(x11)(x1)(st)} in let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))(let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))(let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x21)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))(Curry.Module.Prelude.T2(Curry.Module.OraclePrelude.c_negate(Curry.Module.OraclePrelude.c_divmod'46_'35selFP28'35d(x12)(x19)(st))(x21)(st))(Curry.Module.OraclePrelude.c_negate(Curry.Module.OraclePrelude.c_divmod'46_'35selFP29'35m(x12)(x20)(st))(x22)(st)))(st))(st))(st))(st)
c__case_22_case__132 x1 x10 x3@(Curry.Module.Prelude.C_Neg x15) st = let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List))(let {x16 = Curry.Module.OraclePrelude.c_divmodNat(x10)(x15)(x1)(st)} in let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x23)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List))(let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x24)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x25)(Curry.Module.Prelude.T2(Curry.Module.OraclePrelude.c_divmod'46_'35selFP31'35d(x16)(x23)(st))(Curry.Module.OraclePrelude.c_negate(Curry.Module.OraclePrelude.c_divmod'46_'35selFP32'35m(x16)(x24)(st))(x25)(st)))(st))(st))(st))(st)
c__case_22_case__132 x1 x10 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_22_case__132(x1)(x10)(x)(st))(i)(xs)(st)
c__case_22_case__132 x1 x10 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_22_case__132")(x)



c__case_24_case__133 x1 x3 x2@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.C_Zero))(st)
c__case_24_case__133 x1 x3 x2@(Curry.Module.Prelude.C_Pos x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_23(x4)(x3)(x1)(st))(st)
c__case_24_case__133 x1 x3 x2@(Curry.Module.Prelude.C_Neg x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c__case_22(x10)(x3)(x1)(st))(st)
c__case_24_case__133 x1 x3 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_24_case__133(x1)(x3)(x)(st))(i)(xs)(st)
c__case_24_case__133 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_24_case__133")(x)



c__case_21_case__134 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_21_case__134 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_21_case__134(x1)(x)(st))(i)(xs)(st)
c__case_21_case__134 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_21_case__134")(x)



c__case_20_case__135 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_20_case__135 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_20_case__135(x1)(x)(st))(i)(xs)(st)
c__case_20_case__135 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_20_case__135")(x)



c__case_19_case__136 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_19_case__136 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_19_case__136(x1)(x)(st))(i)(xs)(st)
c__case_19_case__136 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_19_case__136")(x)



c__case_18_case__137 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_18_case__137 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_18_case__137(x1)(x)(st))(i)(xs)(st)
c__case_18_case__137 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_18_case__137")(x)



c__case_17_case__138 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_17_case__138 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_17_case__138(x1)(x)(st))(i)(xs)(st)
c__case_17_case__138 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_17_case__138")(x)



c__case_16_case__139 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_16_case__139 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_16_case__139(x1)(x)(st))(i)(xs)(st)
c__case_16_case__139 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_16_case__139")(x)



c__case_15_case__140 x1 x2@Curry.Module.Prelude.C_Zero st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Zero)(st)
c__case_15_case__140 x1 x2@(Curry.Module.Prelude.C_Pos x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Neg(x3))(st)
c__case_15_case__140 x1 x2@(Curry.Module.Prelude.C_Neg x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(x4))(st)
c__case_15_case__140 x1 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_15_case__140(x1)(x)(st))(i)(xs)(st)
c__case_15_case__140 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_15_case__140")(x)



c__case_14_case__141 x1 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_success(x1)(st))(st)
c__case_14_case__141 x1 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_14_case__141 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_14_case__141(x1)(x)(st))(i)(xs)(st)
c__case_14_case__141 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_14_case__141")(x)



c__case_13_case__142 x1 x2 x3 x4@Curry.Module.Prelude.C_Nothing st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_13_case__142 x1 x2 x3 x4@(Curry.Module.Prelude.C_Just x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.c_apply(x3)(x5)(x1)(st))(st)
c__case_13_case__142 x1 x2 x3 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_13_case__142(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_13_case__142 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_13_case__142")(x)



c__case_12_case__143 x1 x2 x3 x4@(Curry.Module.Prelude.C_Left x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.c_apply(x2)(x5)(x1)(st))(st)
c__case_12_case__143 x1 x2 x3 x4@(Curry.Module.Prelude.C_Right x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.c_apply(x3)(x6)(x1)(st))(st)
c__case_12_case__143 x1 x2 x3 (Curry.Module.Prelude.C_EitherOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_12_case__143(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_12_case__143 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_12_case__143")(x)



c__case_11_case__144 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_done(x1)(st))(st)
c__case_11_case__144 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_62_62(Curry.Module.OraclePrelude.c_putChar(x3)(x1)(st))(Curry.Module.OraclePrelude.c_putStr(x4)(x5)(st))(x6)(st))(st)
c__case_11_case__144 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_11_case__144(x1)(x)(st))(i)(xs)(st)
c__case_11_case__144 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_11_case__144")(x)



c__case_10_case__145 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_return(Curry.Module.Prelude.List)(x1)(st))(st)
c__case_10_case__145 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OraclePrelude.c_getLine(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_getLine'46_'35lambda10'46_'35lambda11(x2)))))(x4)(st))(st)
c__case_10_case__145 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_10_case__145(x1)(x2)(x)(st))(i)(xs)(st)
c__case_10_case__145 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_10_case__145")(x)



c__case_9_case__146 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_return(Curry.Module.Prelude.List)(x1)(st))(st)
c__case_9_case__146 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.op_62_62_61(x3)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_sequenceIO'46_'35lambda12(x4)))))(x1)(st))(st)
c__case_9_case__146 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_9_case__146(x1)(x)(st))(i)(xs)(st)
c__case_9_case__146 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_9_case__146")(x)



c__case_8_case__147 x1 x2@(Curry.Module.Prelude.C_Value x3) st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(st)
c__case_8_case__147 x1 x2@Curry.Module.Prelude.C_Fail st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_8_case__147 x1 x2@Curry.Module.Prelude.C_Suspend st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_8_case__147 x1 x2@(Curry.Module.Prelude.C_Choice x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_allValuesD))))(x1)(st))(x4)(x5)(st))(st)
c__case_8_case__147 x1 (Curry.Module.Prelude.C_SearchTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_8_case__147(x1)(x)(st))(i)(xs)(st)
c__case_8_case__147 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_8_case__147")(x)



c__case_7_case__148 x1 x3 x2@(Curry.Module.Prelude.C_Value x4) st = Curry.Module.CEventOracle.c_replace(x1)(let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x13)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x4)(Curry.Module.OraclePrelude.c_allValuesB'46partition'46692'46_'35selFP34'35vs(x3)(x1)(st)))(Curry.Module.OraclePrelude.c_allValuesB'46partition'46692'46_'35selFP35'35ors(x3)(x12)(st)))(st))(st))(st))(st)
c__case_7_case__148 x1 x3 x2@(Curry.Module.Prelude.C_Choice x8) st = Curry.Module.CEventOracle.c_replace(x1)(let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x15)(Curry.Module.Prelude.T2(Curry.Module.OraclePrelude.c_allValuesB'46partition'46692'46_'35selFP37'35vs(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43(x8)(Curry.Module.OraclePrelude.c_allValuesB'46partition'46692'46_'35selFP38'35ors(x3)(x14)(st))(x15)(st)))(st))(st))(st))(st)
c__case_7_case__148 x1 x3 x2@Curry.Module.Prelude.C_Fail st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_7_case__148 x1 x3 x2@Curry.Module.Prelude.C_Suspend st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_7_case__148 x1 x3 (Curry.Module.Prelude.C_SearchTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_7_case__148(x1)(x3)(x)(st))(i)(xs)(st)
c__case_7_case__148 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_7_case__148")(x)



c__case_6_case__149 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_6_case__149 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_6_case__149(x1)(x)(st))(i)(xs)(st)
c__case_6_case__149 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_6_case__149")(x)



c__case_5_case__150 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_5_case__150 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_5_case__150(x1)(x)(st))(i)(xs)(st)
c__case_5_case__150 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_5_case__150")(x)



c__case_4_case__151 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_4_case__151 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_4_case__151(x1)(x)(st))(i)(xs)(st)
c__case_4_case__151 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_4_case__151")(x)



c__case_3_case__152 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_3_case__152 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_3_case__152(x1)(x)(st))(i)(xs)(st)
c__case_3_case__152 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_3_case__152")(x)



c__case_2_case__153 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_2_case__153 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(let {x5 = Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.c_allValuesB'46partition'46692))(st))(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(x3)(x4))(x1)(st)} in let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_allValuesB'46unfoldOrs'46692'46_'35selFP40'35vals(x5)(x8)(st))(Curry.Module.OraclePrelude.c_allValuesB'46unfoldOrs'46692(Curry.Module.OraclePrelude.c_allValuesB'46unfoldOrs'46692'46_'35selFP41'35ors(x5)(x9)(st))(x10)(st))(x11)(st))(st))(st))(st))(st)
c__case_2_case__153 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_2_case__153(x1)(x)(st))(i)(xs)(st)
c__case_2_case__153 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_2_case__153")(x)



c__case_1_case__154 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_1_case__154 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_1_case__154(x1)(x)(st))(i)(xs)(st)
c__case_1_case__154 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_1_case__154")(x)



c__case_0_case__155 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_0_case__155 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrelude.c__case_0_case__155(x1)(x)(st))(i)(xs)(st)
c__case_0_case__155 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrelude._case_0_case__155")(x)


