{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleList (module Curry.Module.OracleList) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.List
import Curry.Module.Maybe
import Curry.Module.Prelude
import Curry.Module.OracleMaybe
import Curry.Module.OraclePrelude



-- begin included



-- end included

c_elemIndex :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe Curry.Module.Prelude.C_Int))
c_elemIndex x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c_findIndex(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.op_61_61(x2)))))(x1)(st))(st)



c_elemIndices :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))
c_elemIndices x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleList.c_findIndices(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.op_61_61(x2)))))))))(st)



c_find :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t0))
c_find x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleMaybe.c_listToMaybe))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_filter(x2)))))(x1)(st))(st)



c_findIndex :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe Curry.Module.Prelude.C_Int))
c_findIndex x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleMaybe.c_listToMaybe))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleList.c_findIndices(x2)))))(x1)(st))(st)



c_findIndices :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_findIndices x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleList.c_findIndices'46_'35lambda4(x2)))(st))(Curry.Module.Prelude.List)(Curry.Module.OraclePrelude.c_zip(x3)(Curry.Module.OraclePrelude.c_enumFrom(Curry.Module.Prelude.C_Zero)(x1)(st))(x4)(st))(x5)(st))(st)



c_findIndices'46_'35lambda4 :: (Curry t7) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t7 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.T2 t7 Curry.Module.Prelude.C_Int) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_findIndices'46_'35lambda4 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_26(x2)(x4)(x3)(x1)(st))(st)



c_nub :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_nub x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c_nubBy(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_61_61))(st))(x2)(x1)(st))(st)



c_nubBy :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_nubBy x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_24(x2)(x3)(x1)(st))(st)



c_nubBy'46_'35lambda6 :: (Curry t45) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t45 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t45 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> t45 -> t45 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_nubBy'46_'35lambda6 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_not(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x3)(x1)(st))(x4)(x5)(st))(x6)(st))(st)



c_delete :: (Curry t0) => t0 -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_delete x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_23(x2)(x3)(x1)(st))(st)



op_92_92 :: (Curry t0) => (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
op_92_92 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_foldl(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleList.c_delete))(st))))(st))(x2)(x3)(x1)(st))(st)



c_union :: (Curry t0) => (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_union x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_21(x3)(x2)(x1)(st))(st)



c_intersect :: (Curry t0) => (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_intersect x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_19(x3)(x2)(x1)(st))(st)



c_intersperse :: (Curry t0) => t0 -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_intersperse x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_17(x2)(x3)(x1)(st))(st)



c_transpose :: (Curry t0) => (Curry.Module.Prelude.List (Curry.Module.Prelude.List t0)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List t0)
c_transpose x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_15(x2)(x1)(st))(st)



c_partition :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t0)
c_partition x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleList.c_partition'46select'4653(x2)))(st))(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(x3)(x1)(st))(st)



c_partition'46select'4653 :: (Curry t146) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t146 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> t146 -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t146) (Curry.Module.Prelude.List t146)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t146) (Curry.Module.Prelude.List t146)
c_partition'46select'4653 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_13(x2)(x3)(x4)(x1)(st))(st)



c_group :: (Curry t0) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List t0)))
c_group x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleList.c_groupBy(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_61_61))(st))))))(st)



c_groupBy :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List t0)
c_groupBy x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_11(x2)(x3)(x1)(st))(st)



c_groupBy'46_'35selFP3'35ys :: (Curry t154) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t154) (Curry.Module.Prelude.List t154)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t154
c_groupBy'46_'35selFP3'35ys x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_10(x2)(x1)(st))(st)



c_groupBy'46_'35selFP4'35zs :: (Curry t154) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t154) (Curry.Module.Prelude.List t154)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t154
c_groupBy'46_'35selFP4'35zs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_9(x2)(x1)(st))(st)



c_replace :: (Curry t0) => t0 -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_replace x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_8(x2)(x3)(x4)(x1)(st))(st)



c_isPrefixOf :: (Curry t0) => (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isPrefixOf x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_5(x3)(x2)(x1)(st))(st)



c_sortBy :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0))
c_sortBy x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleList.c_insertBy(x2)))(st))(Curry.Module.Prelude.List)))))(st)



c_insertBy :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> t0 -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_insertBy x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_3(x2)(x3)(x4)(x1)(st))(st)



c_last :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_last x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_1(x2)(x1)(st))(st)



c__case_1 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_1_case__26(x1)(x2)(st))(st)



c__case_0 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_0_case__25(x1)(x3)(x4)(st))(st)



c__case_3 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_3_case__24(x1)(x2)(x3)(x4)(st))(st)



c__case_2 x2 x3 x5 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_2_case__23(x1)(x2)(x3)(x5)(x6)(x7)(st))(st)



c__case_5 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_5_case__22(x1)(x3)(x2)(st))(st)



c__case_4 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_4_case__21(x1)(x4)(x5)(x3)(st))(st)



c__case_8 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_8_case__20(x1)(x2)(x3)(x4)(st))(st)



c__case_7 x2 x3 x5 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_7_case__19(x1)(x2)(x3)(x5)(x6)(x7)(st))(st)



c__case_6 x2 x3 x5 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_6_case__18(x1)(x2)(x3)(x5)(x6)(x7)(st))(st)



c__case_9 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_9_case__17(x1)(x2)(st))(st)



c__case_10 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_10_case__16(x1)(x2)(st))(st)



c__case_11 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_11_case__15(x1)(x2)(x3)(st))(st)



c__case_13 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_13_case__14(x1)(x2)(x3)(x4)(st))(st)



c__case_12 x2 x3 x5 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_12_case__13(x1)(x3)(x5)(x6)(x7)(st))(st)



c__case_15 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_15_case__12(x1)(x2)(st))(st)



c__case_14 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_14_case__11(x1)(x4)(x3)(st))(st)



c__case_17 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_17_case__10(x1)(x2)(x3)(st))(st)



c__case_16 x2 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_16_case__9(x1)(x2)(x4)(x5)(st))(st)



c__case_19 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_19_case__8(x1)(x3)(x2)(st))(st)



c__case_18 x3 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_18_case__7(x1)(x3)(x4)(x5)(x6)(st))(st)



c__case_21 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_21_case__6(x1)(x3)(x2)(st))(st)



c__case_20 x3 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_20_case__5(x1)(x3)(x4)(x5)(x6)(st))(st)



c__case_23 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_23_case__4(x1)(x2)(x3)(st))(st)



c__case_22 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_22_case__3(x1)(x2)(x4)(x5)(x6)(st))(st)



c__case_24 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_24_case__2(x1)(x2)(x3)(st))(st)



c__case_26 x2 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_26_case__1(x1)(x2)(x4)(x3)(st))(st)



c__case_25 x2 x5 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_25_case__0(x1)(x6)(x7)(st))(st)



c__case_25_case__0 x1 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(st)
c__case_25_case__0 x1 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_25_case__0 x1 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_25_case__0(x1)(x6)(x)(st))(i)(xs)(st)
c__case_25_case__0 x1 x6 x st = Curry.RunTimeSystem.patternFail("OracleList._case_25_case__0")(x)



c__case_26_case__1 x1 x2 x4 x3@(Curry.Module.Prelude.T2 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleList.c__case_25(x2)(x5)(x6)(Curry.Module.Oracle.c_apply(x2)(x5)(x1)(st))(x7)(st))(x4)(x8)(st))(st)
c__case_26_case__1 x1 x2 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_26_case__1(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_26_case__1 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleList._case_26_case__1")(x)



c__case_24_case__2 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_24_case__2 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(x4)(Curry.Module.OracleList.c_nubBy(x2)(Curry.Module.OraclePrelude.c_filter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleList.c_nubBy'46_'35lambda6(x2)(x4)))))(x5)(x1)(st))(x6)(st)))(st)
c__case_24_case__2 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_24_case__2(x1)(x2)(x)(st))(i)(xs)(st)
c__case_24_case__2 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleList._case_24_case__2")(x)



c__case_22_case__3 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_22_case__3 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.OracleList.c_delete(x2)(x5)(x1)(st)))(st)
c__case_22_case__3 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_22_case__3(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_22_case__3 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleList._case_22_case__3")(x)



c__case_23_case__4 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_23_case__4 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleList.c__case_22(x2)(x4)(x5)(Curry.Module.OraclePrelude.op_61_61(x2)(x4)(x1)(st))(x6)(st))(st)
c__case_23_case__4 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_23_case__4(x1)(x2)(x)(st))(i)(xs)(st)
c__case_23_case__4 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleList._case_23_case__4")(x)



c__case_20_case__5 x1 x3 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c_union(x5)(x3)(x1)(st))(st)
c__case_20_case__5 x1 x3 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.OracleList.c_union(x5)(x3)(x1)(st)))(st)
c__case_20_case__5 x1 x3 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_20_case__5(x1)(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_20_case__5 x1 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleList._case_20_case__5")(x)



c__case_21_case__6 x1 x3 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_21_case__6 x1 x3 x2@((Curry.Module.Prelude.:<) x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.OracleList.c__case_20(x3)(x4)(x5)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_elem(x4)(x1)(st))(x3)(x6)(st))(x7)(st))(st)
c__case_21_case__6 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_21_case__6(x1)(x3)(x)(st))(i)(xs)(st)
c__case_21_case__6 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleList._case_21_case__6")(x)



c__case_18_case__7 x1 x3 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.OracleList.c_intersect(x5)(x3)(x1)(st)))(st)
c__case_18_case__7 x1 x3 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c_intersect(x5)(x3)(x1)(st))(st)
c__case_18_case__7 x1 x3 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_18_case__7(x1)(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_18_case__7 x1 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleList._case_18_case__7")(x)



c__case_19_case__8 x1 x3 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_19_case__8 x1 x3 x2@((Curry.Module.Prelude.:<) x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.OracleList.c__case_18(x3)(x4)(x5)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_elem(x4)(x1)(st))(x3)(x6)(st))(x7)(st))(st)
c__case_19_case__8 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_19_case__8(x1)(x3)(x)(st))(i)(xs)(st)
c__case_19_case__8 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleList._case_19_case__8")(x)



c__case_16_case__9 x1 x2 x4 x5@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(st)
c__case_16_case__9 x1 x2 x4 x5@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x2)(Curry.Module.OracleList.c_intersperse(x2)((Curry.Module.Prelude.:<)(x6)(x7))(x1)(st))))(st)
c__case_16_case__9 x1 x2 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_16_case__9(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_16_case__9 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleList._case_16_case__9")(x)



c__case_17_case__10 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_17_case__10 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_16(x2)(x4)(x5)(x1)(st))(st)
c__case_17_case__10 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_17_case__10(x1)(x2)(x)(st))(i)(xs)(st)
c__case_17_case__10 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleList._case_17_case__10")(x)



c__case_14_case__11 x1 x4 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c_transpose(x4)(x1)(st))(st)
c__case_14_case__11 x1 x4 x3@((Curry.Module.Prelude.:<) x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(x5)(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_head))))(x4)(x1)(st)))(Curry.Module.OracleList.c_transpose((Curry.Module.Prelude.:<)(x6)(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_tail))))(x4)(x7)(st)))(x8)(st)))(st)
c__case_14_case__11 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_14_case__11(x1)(x4)(x)(st))(i)(xs)(st)
c__case_14_case__11 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleList._case_14_case__11")(x)



c__case_15_case__12 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_15_case__12 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_14(x4)(x3)(x1)(st))(st)
c__case_15_case__12 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_15_case__12(x1)(x)(st))(i)(xs)(st)
c__case_15_case__12 x1 x st = Curry.RunTimeSystem.patternFail("OracleList._case_15_case__12")(x)



c__case_12_case__13 x1 x3 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x3)(x5))(x6))(st)
c__case_12_case__13 x1 x3 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(x5)((Curry.Module.Prelude.:<)(x3)(x6)))(st)
c__case_12_case__13 x1 x3 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_12_case__13(x1)(x3)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_12_case__13 x1 x3 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleList._case_12_case__13")(x)



c__case_13_case__14 x1 x2 x3 x4@(Curry.Module.Prelude.T2 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleList.c__case_12(x2)(x3)(x5)(x6)(Curry.Module.Oracle.c_apply(x2)(x3)(x1)(st))(x7)(st))(st)
c__case_13_case__14 x1 x2 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_13_case__14(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_13_case__14 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleList._case_13_case__14")(x)



c__case_11_case__15 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_11_case__15 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))(let {x6 = Curry.Module.OraclePrelude.c_span(Curry.Module.Oracle.c_apply(x2)(x4)(x1)(st))(x5)(x9)(st)} in let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x12)((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(x4)(Curry.Module.OracleList.c_groupBy'46_'35selFP3'35ys(x6)(x10)(st)))(Curry.Module.OracleList.c_groupBy(x2)(Curry.Module.OracleList.c_groupBy'46_'35selFP4'35zs(x6)(x11)(st))(x12)(st)))(st))(st))(st))(st)
c__case_11_case__15 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_11_case__15(x1)(x2)(x)(st))(i)(xs)(st)
c__case_11_case__15 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleList._case_11_case__15")(x)



c__case_10_case__16 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_10_case__16 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_10_case__16(x1)(x)(st))(i)(xs)(st)
c__case_10_case__16 x1 x st = Curry.RunTimeSystem.patternFail("OracleList._case_10_case__16")(x)



c__case_9_case__17 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_9_case__17 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_9_case__17(x1)(x)(st))(i)(xs)(st)
c__case_9_case__17 x1 x st = Curry.RunTimeSystem.patternFail("OracleList._case_9_case__17")(x)



c__case_6_case__18 x1 x2 x3 x5 x6 x7@Curry.Module.Prelude.C_True st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(x5)(Curry.Module.OracleList.c_replace(x2)(Curry.Module.OraclePrelude.op_45(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x6)(x8)(st)))(st)
c__case_6_case__18 x1 x2 x3 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_6_case__18 x1 x2 x3 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_6_case__18(x1)(x2)(x3)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_6_case__18 x1 x2 x3 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleList._case_6_case__18")(x)



c__case_7_case__19 x1 x2 x3 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x2)(x6))(st)
c__case_7_case__19 x1 x2 x3 x5 x6 x7@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleList.c__case_6(x2)(x3)(x5)(x6)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x8)(st))(st)
c__case_7_case__19 x1 x2 x3 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_7_case__19(x1)(x2)(x3)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_7_case__19 x1 x2 x3 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleList._case_7_case__19")(x)



c__case_8_case__20 x1 x2 x3 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_8_case__20 x1 x2 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleList.c__case_7(x2)(x3)(x5)(x6)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Zero)(x1)(st))(x7)(st))(st)
c__case_8_case__20 x1 x2 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_8_case__20(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_8_case__20 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleList._case_8_case__20")(x)



c__case_4_case__21 x1 x4 x5 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_4_case__21 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x4)(x6)(x1)(st))(Curry.Module.OracleList.c_isPrefixOf(x5)(x7)(x8)(st))(x9)(st))(st)
c__case_4_case__21 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_4_case__21(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_4_case__21 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleList._case_4_case__21")(x)



c__case_5_case__22 x1 x3 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_5_case__22 x1 x3 x2@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_4(x4)(x5)(x3)(x1)(st))(st)
c__case_5_case__22 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_5_case__22(x1)(x3)(x)(st))(i)(xs)(st)
c__case_5_case__22 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleList._case_5_case__22")(x)



c__case_2_case__23 x1 x2 x3 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x5)(x6)))(st)
c__case_2_case__23 x1 x2 x3 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.OracleList.c_insertBy(x2)(x3)(x6)(x1)(st)))(st)
c__case_2_case__23 x1 x2 x3 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_2_case__23(x1)(x2)(x3)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_2_case__23 x1 x2 x3 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleList._case_2_case__23")(x)



c__case_3_case__24 x1 x2 x3 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(st)
c__case_3_case__24 x1 x2 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.OracleList.c__case_2(x2)(x3)(x5)(x6)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x3)(x1)(st))(x5)(x7)(st))(x8)(st))(st)
c__case_3_case__24 x1 x2 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_3_case__24(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_3_case__24 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleList._case_3_case__24")(x)



c__case_0_case__25 x1 x3 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_0_case__25 x1 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c_last((Curry.Module.Prelude.:<)(x5)(x6))(x1)(st))(st)
c__case_0_case__25 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_0_case__25(x1)(x3)(x)(st))(i)(xs)(st)
c__case_0_case__25 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleList._case_0_case__25")(x)



c__case_1_case__26 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleList.c__case_0(x3)(x4)(x1)(st))(st)
c__case_1_case__26 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleList.c__case_1_case__26(x1)(x)(st))(i)(xs)(st)
c__case_1_case__26 x1 x st = Curry.RunTimeSystem.patternFail("OracleList._case_1_case__26")(x)


