{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleMaybe (module Curry.Module.OracleMaybe) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.Maybe
import Curry.Module.Prelude
import Curry.Module.OraclePrelude



-- begin included



-- end included

c_isJust :: (Curry t0) => (Curry.Module.Prelude.C_Maybe t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isJust x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMaybe.c__case_8(x2)(x1)(st))(st)



c_isNothing :: (Curry t0) => (Curry.Module.Prelude.C_Maybe t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isNothing x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMaybe.c__case_7(x2)(x1)(st))(st)



c_fromJust :: (Curry t0) => (Curry.Module.Prelude.C_Maybe t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_fromJust x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMaybe.c__case_6(x2)(x1)(st))(st)



c_fromMaybe :: (Curry t0) => t0 -> (Curry.Module.Prelude.C_Maybe t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_fromMaybe x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMaybe.c__case_5(x2)(x3)(x1)(st))(st)



c_maybeToList :: (Curry t0) => (Curry.Module.Prelude.C_Maybe t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_maybeToList x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMaybe.c__case_4(x2)(x1)(st))(st)



c_listToMaybe :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t0
c_listToMaybe x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMaybe.c__case_3(x2)(x1)(st))(st)



c_catMaybes :: (Curry t0) => (Curry.Module.Prelude.List (Curry.Module.Prelude.C_Maybe t0)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_catMaybes x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleMaybe.c_catMaybes'46_'35lambda4))(st))(Curry.Module.Prelude.List)(x2)(x1)(st))(st)



c_catMaybes'46_'35lambda4 :: (Curry t37) => (Curry.Module.Prelude.C_Maybe t37) -> (Curry.Module.Prelude.List t37) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t37
c_catMaybes'46_'35lambda4 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMaybe.c__case_2(x3)(x2)(x1)(st))(st)



c_mapMaybe :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t1))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1))
c_mapMaybe x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleMaybe.c_catMaybes))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(x2)))))(x1)(st))(st)



op_62_62_45 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.C_Maybe t0) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t1))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t1
op_62_62_45 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMaybe.c__case_1(x3)(x2)(x1)(st))(st)



c_sequenceMaybe :: (Curry t0) => (Curry.Module.Prelude.List (Curry.Module.Prelude.C_Maybe t0)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List t0)
c_sequenceMaybe x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMaybe.c__case_0(x2)(x1)(st))(st)



c_sequenceMaybe'46_'35lambda6 :: (Curry t66) => (Curry.Module.Prelude.List (Curry.Module.Prelude.C_Maybe t66)) -> t66 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List t66)
c_sequenceMaybe'46_'35lambda6 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleMaybe.op_62_62_45(Curry.Module.OracleMaybe.c_sequenceMaybe(x2)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleMaybe.c_sequenceMaybe'46_'35lambda6'46_'35lambda7(x3)))))(x4)(st))(st)



c_sequenceMaybe'46_'35lambda6'46_'35lambda7 :: (Curry t66) => t66 -> (Curry.Module.Prelude.List t66) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List t66)
c_sequenceMaybe'46_'35lambda6'46_'35lambda7 x2 x3 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Just((Curry.Module.Prelude.:<)(x2)(x3)))(st)



c_mapMMaybe :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t1))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List t1)))
c_mapMMaybe x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleMaybe.c_sequenceMaybe))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(x2)))))(x1)(st))(st)



c__case_0 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMaybe.c__case_0_case__8(x1)(x2)(st))(st)



c__case_1 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMaybe.c__case_1_case__7(x1)(x3)(x2)(st))(st)



c__case_2 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMaybe.c__case_2_case__6(x1)(x3)(x2)(st))(st)



c__case_3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMaybe.c__case_3_case__5(x1)(x2)(st))(st)



c__case_4 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMaybe.c__case_4_case__4(x1)(x2)(st))(st)



c__case_5 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMaybe.c__case_5_case__3(x1)(x2)(x3)(st))(st)



c__case_6 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMaybe.c__case_6_case__2(x1)(x2)(st))(st)



c__case_7 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMaybe.c__case_7_case__1(x1)(x2)(st))(st)



c__case_8 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMaybe.c__case_8_case__0(x1)(x2)(st))(st)



c__case_8_case__0 x1 x2@(Curry.Module.Prelude.C_Just x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_8_case__0 x1 x2@Curry.Module.Prelude.C_Nothing st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_8_case__0 x1 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleMaybe.c__case_8_case__0(x1)(x)(st))(i)(xs)(st)
c__case_8_case__0 x1 x st = Curry.RunTimeSystem.patternFail("OracleMaybe._case_8_case__0")(x)



c__case_7_case__1 x1 x2@Curry.Module.Prelude.C_Nothing st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_7_case__1 x1 x2@(Curry.Module.Prelude.C_Just x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_7_case__1 x1 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleMaybe.c__case_7_case__1(x1)(x)(st))(i)(xs)(st)
c__case_7_case__1 x1 x st = Curry.RunTimeSystem.patternFail("OracleMaybe._case_7_case__1")(x)



c__case_6_case__2 x1 x2@(Curry.Module.Prelude.C_Just x3) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_6_case__2 x1 x2@Curry.Module.Prelude.C_Nothing st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('J'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))(Curry.Module.Prelude.List))))))))))))))))))))))))(x1)(st))(st)
c__case_6_case__2 x1 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleMaybe.c__case_6_case__2(x1)(x)(st))(i)(xs)(st)
c__case_6_case__2 x1 x st = Curry.RunTimeSystem.patternFail("OracleMaybe._case_6_case__2")(x)



c__case_5_case__3 x1 x2 x3@Curry.Module.Prelude.C_Nothing st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_5_case__3 x1 x2 x3@(Curry.Module.Prelude.C_Just x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_5_case__3 x1 x2 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleMaybe.c__case_5_case__3(x1)(x2)(x)(st))(i)(xs)(st)
c__case_5_case__3 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleMaybe._case_5_case__3")(x)



c__case_4_case__4 x1 x2@Curry.Module.Prelude.C_Nothing st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_4_case__4 x1 x2@(Curry.Module.Prelude.C_Just x3) st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(st)
c__case_4_case__4 x1 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleMaybe.c__case_4_case__4(x1)(x)(st))(i)(xs)(st)
c__case_4_case__4 x1 x st = Curry.RunTimeSystem.patternFail("OracleMaybe._case_4_case__4")(x)



c__case_3_case__5 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_3_case__5 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Just(x3))(st)
c__case_3_case__5 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleMaybe.c__case_3_case__5(x1)(x)(st))(i)(xs)(st)
c__case_3_case__5 x1 x st = Curry.RunTimeSystem.patternFail("OracleMaybe._case_3_case__5")(x)



c__case_2_case__6 x1 x3 x2@(Curry.Module.Prelude.C_Just x4) st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x4)(x3))(st)
c__case_2_case__6 x1 x3 x2@Curry.Module.Prelude.C_Nothing st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_2_case__6 x1 x3 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleMaybe.c__case_2_case__6(x1)(x3)(x)(st))(i)(xs)(st)
c__case_2_case__6 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleMaybe._case_2_case__6")(x)



c__case_1_case__7 x1 x3 x2@Curry.Module.Prelude.C_Nothing st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_1_case__7 x1 x3 x2@(Curry.Module.Prelude.C_Just x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.c_apply(x3)(x4)(x1)(st))(st)
c__case_1_case__7 x1 x3 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleMaybe.c__case_1_case__7(x1)(x3)(x)(st))(i)(xs)(st)
c__case_1_case__7 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleMaybe._case_1_case__7")(x)



c__case_0_case__8 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Just(Curry.Module.Prelude.List))(st)
c__case_0_case__8 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleMaybe.op_62_62_45(x3)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleMaybe.c_sequenceMaybe'46_'35lambda6(x4)))))(x1)(st))(st)
c__case_0_case__8 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleMaybe.c__case_0_case__8(x1)(x)(st))(i)(xs)(st)
c__case_0_case__8 x1 x st = Curry.RunTimeSystem.patternFail("OracleMaybe._case_0_case__8")(x)


