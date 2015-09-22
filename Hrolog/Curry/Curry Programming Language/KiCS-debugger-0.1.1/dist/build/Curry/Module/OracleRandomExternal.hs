{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleRandomExternal (module Curry.Module.OracleRandomExternal) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.RandomExternal
import Curry.Module.Prelude
import Curry.Module.OraclePrelude



-- begin included



-- end included

c_split :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_split x2 x1 st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(let {x3 = Curry.Module.Oracle.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleRandomExternal.c_prim_split))))(x2)(x1)(st)} in let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x8)((Curry.Module.Prelude.:<)(Curry.Module.OracleRandomExternal.c_split'46_'35selFP3'35s1(x3)(x6)(st))(Curry.Module.OracleRandomExternal.c_split(Curry.Module.OracleRandomExternal.c_split'46_'35selFP4'35s2(x3)(x7)(st))(x8)(st)))(st))(st))(st))(st)



c_split'46_'35selFP3'35s1 :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_split'46_'35selFP3'35s1 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandomExternal.c__case_1(x2)(x1)(st))(st)



c_split'46_'35selFP4'35s2 :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_split'46_'35selFP4'35s2 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandomExternal.c__case_0(x2)(x1)(st))(st)



c_nextInt :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_nextInt x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleRandomExternal.c_prim_nextInt))))(x2)(x1)(st))(st)



c_nextIntRange :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_nextIntRange x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_36_35_35(Curry.Module.Oracle.op_36_35_35(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleRandomExternal.c_prim_nextIntRange))(st))(x2)(x1)(st))(x3)(x4)(st))(st)



c_nextBoolean :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Bool
c_nextBoolean x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.Prelude.C_Zero)))))(Curry.Module.OracleRandomExternal.c_nextIntRange(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x3)(st))(st)



c__case_0 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandomExternal.c__case_0_case__1(x1)(x2)(st))(st)



c__case_1 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRandomExternal.c__case_1_case__0(x1)(x2)(st))(st)



c_prim_split :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int
c_prim_split x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.RandomExternal.c_prim_split(x2)(st))(st)



c_prim_nextInt :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_prim_nextInt x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.RandomExternal.c_prim_nextInt(x2)(st))(st)



c_prim_nextIntRange :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_prim_nextIntRange x3 x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.RandomExternal.c_prim_nextIntRange(x3)(x2)(st))(st)



c_getRandomSeed :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Int))
c_getRandomSeed x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Oracle.c_safeIOResult(Curry.Module.RandomExternal.c_getRandomSeed(st))(st))))(st)



c__case_1_case__0 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_1_case__0 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRandomExternal.c__case_1_case__0(x1)(x)(st))(i)(xs)(st)
c__case_1_case__0 x1 x st = Curry.RunTimeSystem.patternFail("OracleRandomExternal._case_1_case__0")(x)



c__case_0_case__1 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_0_case__1 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRandomExternal.c__case_0_case__1(x1)(x)(st))(i)(xs)(st)
c__case_0_case__1 x1 x st = Curry.RunTimeSystem.patternFail("OracleRandomExternal._case_0_case__1")(x)


