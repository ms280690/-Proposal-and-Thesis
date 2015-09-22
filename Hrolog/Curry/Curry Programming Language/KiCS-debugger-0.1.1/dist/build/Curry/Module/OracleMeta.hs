{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleMeta (module Curry.Module.OracleMeta) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.Meta
import Curry.Module.Prelude
import Curry.Module.OraclePrelude



-- begin included



-- end included

c_isFree :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Either t0 t0)))
c_isFree x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.c_headNormalFormIO(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleMeta.c_prim_isFree))))(x2)(x1)(st))(st)



c_throw :: (Curry t0) => Curry.Module.Meta.C_Exception -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_throw x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleMeta.c_prim_throw))))(x2)(x1)(st))(st)



c_list :: (Curry t0) => (Curry.Module.Meta.C_Seq t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_list x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMeta.c__case_4(x2)(x1)(st))(st)



c_interleave :: (Curry t0) => (Curry.Module.Meta.C_Seq t0) -> (Curry.Module.Meta.C_Seq t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Meta.C_Seq t0
c_interleave x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMeta.c__case_3(x3)(x2)(x1)(st))(st)



c_seq :: (Curry t0) => (Curry.Module.Prelude.C_SearchTree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Meta.C_Seq t0
c_seq x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMeta.c__case_1(x2)(x1)(st))(st)



c_isValOrChoice :: (Curry t0) => (Curry.Module.Prelude.C_SearchTree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isValOrChoice x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMeta.c__case_0(x2)(x1)(st))(st)



c_allValuesI :: (Curry t0) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.C_SearchTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0))
c_allValuesI x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleMeta.c_list))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleMeta.c_seq))))(x1)(st))(st)



c__case_0 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMeta.c__case_0_case__4(x1)(x2)(st))(st)



c__case_1 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMeta.c__case_1_case__3(x1)(x2)(st))(st)



c__case_3 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMeta.c__case_3_case__2(x1)(x3)(x2)(st))(st)



c__case_2 x2 x6 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMeta.c__case_2_case__1(x1)(x2)(x6)(x3)(st))(st)



c__case_4 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMeta.c__case_4_case__0(x1)(x2)(st))(st)



c_prim_isFree :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Either t0 t0)))
c_prim_isFree x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Oracle.c_safeIOResult(Curry.Module.Meta.c_prim_isFree(x2)(st))(st))))(st)



c_searchTree :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_SearchTree t0
c_searchTree x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Meta.c_searchTree(x2)(st))(st)



c_gnfIO :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))
c_gnfIO x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Oracle.c_safeIOResult(Curry.Module.Meta.c_gnfIO(x2)(st))(st))))(st)



c_ghnfIO :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))
c_ghnfIO x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Oracle.c_safeIOResult(Curry.Module.Meta.c_ghnfIO(x2)(st))(st))))(st)



c_nfIO :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))
c_nfIO x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Oracle.c_safeIOResult(Curry.Module.Meta.c_nfIO(x2)(st))(st))))(st)



c_hnfIO :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))
c_hnfIO x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Oracle.c_safeIOResult(Curry.Module.Meta.c_hnfIO(x2)(st))(st))))(st)



c_getRichSearchTree :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Meta.C_RichSearchTree t0)))
c_getRichSearchTree x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Oracle.c_safeIOResult(Curry.Module.Meta.c_getRichSearchTree(x2)(st))(st))))(st)



c_richSearchTree :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Meta.C_RichSearchTree t0
c_richSearchTree x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Meta.c_richSearchTree(x2)(st))(st)



c_parallelSearch :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List t0)))
c_parallelSearch x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Oracle.c_safeIOResult(Curry.Module.Meta.c_parallelSearch(x2)(st))(st))))(st)



c_cover :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_cover x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Meta.c_cover(x2)(st))(st)



c_st :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_SearchTree t0
c_st x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Meta.c_st(x2)(st))(st)



c_richST :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Meta.C_RichSearchTree t0
c_richST x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Meta.c_richST(x2)(st))(st)



c_ors :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_ors x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Meta.c_ors(x2)(st))(st)



c_prim_throw :: (Curry t0) => Curry.Module.Meta.C_Exception -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_prim_throw x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Meta.c_prim_throw(x2)(st))(st)



c__case_4_case__0 x1 x2@Curry.Module.Meta.C_Nil st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_4_case__0 x1 x2@(Curry.Module.Meta.C_Cons x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.OracleMeta.c_list(x4)(x1)(st)))(st)
c__case_4_case__0 x1 x2@(Curry.Module.Meta.C_Continued x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMeta.c_list(x5)(x1)(st))(st)
c__case_4_case__0 x1 (Curry.Module.Meta.C_SeqOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleMeta.c__case_4_case__0(x1)(x)(st))(i)(xs)(st)
c__case_4_case__0 x1 x st = Curry.RunTimeSystem.patternFail("OracleMeta._case_4_case__0")(x)



c__case_2_case__1 x1 x2 x6 x3@Curry.Module.Meta.C_Nil st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_2_case__1 x1 x2 x6 x3@(Curry.Module.Meta.C_Cons x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Meta.C_Cons(x7)(Curry.Module.OracleMeta.c_interleave(x6)(x8)(x1)(st)))(st)
c__case_2_case__1 x1 x2 x6 x3@(Curry.Module.Meta.C_Continued x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Meta.C_Continued(Curry.Module.OracleMeta.c_interleave(x6)(x9)(x1)(st)))(st)
c__case_2_case__1 x1 x2 x6 (Curry.Module.Meta.C_SeqOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleMeta.c__case_2_case__1(x1)(x2)(x6)(x)(st))(i)(xs)(st)
c__case_2_case__1 x1 x2 x6 x st = Curry.RunTimeSystem.patternFail("OracleMeta._case_2_case__1")(x)



c__case_3_case__2 x1 x3 x2@Curry.Module.Meta.C_Nil st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Meta.C_Continued(x3))(st)
c__case_3_case__2 x1 x3 x2@(Curry.Module.Meta.C_Cons x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Meta.C_Cons(x4)(Curry.Module.OracleMeta.c_interleave(x3)(x5)(x1)(st)))(st)
c__case_3_case__2 x1 x3 x2@(Curry.Module.Meta.C_Continued x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMeta.c__case_2(x2)(x6)(x3)(x1)(st))(st)
c__case_3_case__2 x1 x3 (Curry.Module.Meta.C_SeqOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleMeta.c__case_3_case__2(x1)(x3)(x)(st))(i)(xs)(st)
c__case_3_case__2 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleMeta._case_3_case__2")(x)



c__case_1_case__3 x1 x2@Curry.Module.Prelude.C_Fail st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Meta.C_Nil)(st)
c__case_1_case__3 x1 x2@(Curry.Module.Prelude.C_Value x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Meta.C_Cons(x3)(Curry.Module.Meta.C_Nil))(st)
c__case_1_case__3 x1 x2@(Curry.Module.Prelude.C_Choice x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_foldr1(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleMeta.c_interleave))(st))((Curry.Module.Prelude.:<)(Curry.Module.Meta.C_Nil)(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleMeta.c_seq))))(Curry.Module.OraclePrelude.c_filter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleMeta.c_isValOrChoice))))(x4)(x1)(st))(x5)(st)))(x6)(st))(st)
c__case_1_case__3 x1 x2@Curry.Module.Prelude.C_Suspend st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Meta.C_Nil)(st)
c__case_1_case__3 x1 (Curry.Module.Prelude.C_SearchTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleMeta.c__case_1_case__3(x1)(x)(st))(i)(xs)(st)
c__case_1_case__3 x1 x st = Curry.RunTimeSystem.patternFail("OracleMeta._case_1_case__3")(x)



c__case_0_case__4 x1 x2@Curry.Module.Prelude.C_Fail st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_0_case__4 x1 x2@(Curry.Module.Prelude.C_Value x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_0_case__4 x1 x2@(Curry.Module.Prelude.C_Choice x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_0_case__4 x1 x2@Curry.Module.Prelude.C_Suspend st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_0_case__4 x1 (Curry.Module.Prelude.C_SearchTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleMeta.c__case_0_case__4(x1)(x)(st))(i)(xs)(st)
c__case_0_case__4 x1 x st = Curry.RunTimeSystem.patternFail("OracleMeta._case_0_case__4")(x)


