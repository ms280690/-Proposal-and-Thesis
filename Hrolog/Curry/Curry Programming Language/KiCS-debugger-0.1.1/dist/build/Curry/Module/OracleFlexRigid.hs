{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleFlexRigid (module Curry.Module.OracleFlexRigid) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.FlexRigid
import Curry.Module.FlatCurry
import Curry.Module.Prelude
import Curry.Module.OracleFlatCurry
import Curry.Module.OraclePrelude



-- begin included



-- end included

c_getFlexRigid :: Curry.Module.FlatCurry.C_Expr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlexRigid.C_FlexRigidResult
c_getFlexRigid x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlexRigid.c__case_7(x2)(x1)(st))(st)



c_getFlexRigid'46_'35lambda2 :: Curry.Module.FlatCurry.C_BranchExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_getFlexRigid'46_'35lambda2 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlexRigid.c__case_5(x2)(x1)(st))(st)



c_joinCaseTypes :: Curry.Module.FlexRigid.C_FlexRigidResult -> Curry.Module.FlexRigid.C_FlexRigidResult -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlexRigid.C_FlexRigidResult
c_joinCaseTypes x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlexRigid.c__case_4(x3)(x2)(x1)(st))(st)



c__case_4 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlexRigid.c__case_4_case__7(x1)(x3)(x2)(st))(st)



c__case_0 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlexRigid.c__case_0_case__6(x1)(x3)(st))(st)



c__case_1 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlexRigid.c__case_1_case__5(x1)(x3)(st))(st)



c__case_2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlexRigid.c__case_2_case__4(x1)(x3)(st))(st)



c__case_3 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlexRigid.c__case_3_case__3(x1)(x3)(st))(st)



c__case_5 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlexRigid.c__case_5_case__2(x1)(x2)(st))(st)



c__case_7 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlexRigid.c__case_7_case__1(x1)(x2)(st))(st)



c__case_6 x14 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlexRigid.c__case_6_case__0(x1)(x15)(st))(st)



c__case_6_case__0 x1 x15@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_KnownFlex)(st)
c__case_6_case__0 x1 x15@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_KnownRigid)(st)
c__case_6_case__0 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlexRigid.c__case_6_case__0(x1)(x)(st))(i)(xs)(st)
c__case_6_case__0 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlexRigid._case_6_case__0")(x)



c__case_7_case__1 x1 x2@(Curry.Module.FlatCurry.C_Var x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_UnknownFR)(st)
c__case_7_case__1 x1 x2@(Curry.Module.FlatCurry.C_Lit x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_UnknownFR)(st)
c__case_7_case__1 x1 x2@(Curry.Module.FlatCurry.C_Comb x5 x6 x7) st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleFlexRigid.c_joinCaseTypes))(st))(Curry.Module.FlexRigid.C_UnknownFR)(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlexRigid.c_getFlexRigid))))(x7)(x1)(st))(x17)(st))(st)
c__case_7_case__1 x1 x2@(Curry.Module.FlatCurry.C_Let x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlexRigid.c_getFlexRigid(x9)(x1)(st))(st)
c__case_7_case__1 x1 x2@(Curry.Module.FlatCurry.C_Free x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlexRigid.c_getFlexRigid(x11)(x1)(st))(st)
c__case_7_case__1 x1 x2@(Curry.Module.FlatCurry.C_Or x12 x13) st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List)))(Curry.Module.OracleFlexRigid.c_joinCaseTypes(Curry.Module.OracleFlexRigid.c_getFlexRigid(x12)(x1)(st))(Curry.Module.OracleFlexRigid.c_getFlexRigid(x13)(x18)(st))(x19)(st))(st)
c__case_7_case__1 x1 x2@(Curry.Module.FlatCurry.C_Case x14 x15 x16) st = let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleFlexRigid.c_joinCaseTypes))(st))(Curry.Module.OracleFlexRigid.c__case_6(x14)(Curry.Module.OraclePrelude.op_61_61(x14)(Curry.Module.FlatCurry.C_Flex)(x1)(st))(x20)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlexRigid.c_getFlexRigid))))((Curry.Module.Prelude.:<)(x15)(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlexRigid.c_getFlexRigid'46_'35lambda2))))(x16)(x21)(st)))(x22)(st))(x23)(st))(st)
c__case_7_case__1 x1 (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlexRigid.c__case_7_case__1(x1)(x)(st))(i)(xs)(st)
c__case_7_case__1 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlexRigid._case_7_case__1")(x)



c__case_5_case__2 x1 x2@(Curry.Module.FlatCurry.C_Branch x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_5_case__2 x1 (Curry.Module.FlatCurry.C_BranchExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlexRigid.c__case_5_case__2(x1)(x)(st))(i)(xs)(st)
c__case_5_case__2 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlexRigid._case_5_case__2")(x)



c__case_3_case__3 x1 x3@Curry.Module.FlexRigid.C_ConflictFR st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_ConflictFR)(st)
c__case_3_case__3 x1 x3@Curry.Module.FlexRigid.C_UnknownFR st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_ConflictFR)(st)
c__case_3_case__3 x1 x3@Curry.Module.FlexRigid.C_KnownFlex st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_ConflictFR)(st)
c__case_3_case__3 x1 x3@Curry.Module.FlexRigid.C_KnownRigid st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_ConflictFR)(st)
c__case_3_case__3 x1 (Curry.Module.FlexRigid.C_FlexRigidResultOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlexRigid.c__case_3_case__3(x1)(x)(st))(i)(xs)(st)
c__case_3_case__3 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlexRigid._case_3_case__3")(x)



c__case_2_case__4 x1 x3@Curry.Module.FlexRigid.C_ConflictFR st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_ConflictFR)(st)
c__case_2_case__4 x1 x3@Curry.Module.FlexRigid.C_UnknownFR st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_UnknownFR)(st)
c__case_2_case__4 x1 x3@Curry.Module.FlexRigid.C_KnownFlex st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_KnownFlex)(st)
c__case_2_case__4 x1 x3@Curry.Module.FlexRigid.C_KnownRigid st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_KnownRigid)(st)
c__case_2_case__4 x1 (Curry.Module.FlexRigid.C_FlexRigidResultOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlexRigid.c__case_2_case__4(x1)(x)(st))(i)(xs)(st)
c__case_2_case__4 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlexRigid._case_2_case__4")(x)



c__case_1_case__5 x1 x3@Curry.Module.FlexRigid.C_ConflictFR st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_ConflictFR)(st)
c__case_1_case__5 x1 x3@Curry.Module.FlexRigid.C_UnknownFR st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_KnownFlex)(st)
c__case_1_case__5 x1 x3@Curry.Module.FlexRigid.C_KnownFlex st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_KnownFlex)(st)
c__case_1_case__5 x1 x3@Curry.Module.FlexRigid.C_KnownRigid st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_ConflictFR)(st)
c__case_1_case__5 x1 (Curry.Module.FlexRigid.C_FlexRigidResultOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlexRigid.c__case_1_case__5(x1)(x)(st))(i)(xs)(st)
c__case_1_case__5 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlexRigid._case_1_case__5")(x)



c__case_0_case__6 x1 x3@Curry.Module.FlexRigid.C_ConflictFR st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_ConflictFR)(st)
c__case_0_case__6 x1 x3@Curry.Module.FlexRigid.C_UnknownFR st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_KnownRigid)(st)
c__case_0_case__6 x1 x3@Curry.Module.FlexRigid.C_KnownFlex st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_ConflictFR)(st)
c__case_0_case__6 x1 x3@Curry.Module.FlexRigid.C_KnownRigid st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlexRigid.C_KnownRigid)(st)
c__case_0_case__6 x1 (Curry.Module.FlexRigid.C_FlexRigidResultOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlexRigid.c__case_0_case__6(x1)(x)(st))(i)(xs)(st)
c__case_0_case__6 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlexRigid._case_0_case__6")(x)



c__case_4_case__7 x1 x3 x2@Curry.Module.FlexRigid.C_ConflictFR st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlexRigid.c__case_3(x3)(x1)(st))(st)
c__case_4_case__7 x1 x3 x2@Curry.Module.FlexRigid.C_UnknownFR st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlexRigid.c__case_2(x3)(x1)(st))(st)
c__case_4_case__7 x1 x3 x2@Curry.Module.FlexRigid.C_KnownFlex st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlexRigid.c__case_1(x3)(x1)(st))(st)
c__case_4_case__7 x1 x3 x2@Curry.Module.FlexRigid.C_KnownRigid st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlexRigid.c__case_0(x3)(x1)(st))(st)
c__case_4_case__7 x1 x3 (Curry.Module.FlexRigid.C_FlexRigidResultOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlexRigid.c__case_4_case__7(x1)(x3)(x)(st))(i)(xs)(st)
c__case_4_case__7 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleFlexRigid._case_4_case__7")(x)


