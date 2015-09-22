{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.TransformationComb (module Curry.Module.TransformationComb) where

import Curry.RunTimeSystem
import Curry.Module.AbstractCurry
import Curry.Module.FlatCurry
import Curry.Module.FlatToAbstractCurry
import Curry.Module.Prelude
import Curry.Module.SrcRef
import Curry.Module.TransformationDebugInfo
import Curry.Module.TransformationMonad
import Curry.Module.TransformationPartCalls
import Curry.Module.AbstractHaskell



-- begin included



-- end included

c_transformComb :: Curry.Module.FlatCurry.C_CombType -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CExpr) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) Curry.Module.AbstractCurry.C_CExpr)
c_transformComb x1 x2 x3 x4 x5 st = Curry.Module.Prelude.pf(Curry.Module.TransformationMonad.op_62_62_61_46(Curry.Module.Prelude.pf(Curry.Module.TransformationMonad.c_freshVars(Curry.Module.Prelude.c_length(x3)(st))))(Curry.Module.Prelude.pf(Curry.Module.TransformationComb.c_transformComb'46_'35lambda2(x3)(x1)(x4)(x2)(x5))))



c_transformComb'46_'35lambda2 :: (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CExpr) -> Curry.Module.FlatCurry.C_CombType -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) Curry.Module.AbstractCurry.C_CExpr)
c_transformComb'46_'35lambda2 x1 x2 x3 x4 x5 x6 st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.TransformationMonad.c_ret))(Curry.Module.TransformationComb.c_transformComb'39(x2)(x4)(x1)(x6)(Curry.Module.TransformationDebugInfo.c_debugInfo(Curry.Module.TransformationDebugInfo.c_createStaticInfo(x3)(x5)(st))(Curry.Module.TransformationDebugInfo.c_dynamicInfo(Curry.Module.FlatToAbstractCurry.c_presym((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List)))(st))(Curry.Module.FlatToAbstractCurry.c_list(Curry.Module.Prelude.c_map(Curry.Module.TransformationDebugInfo.c_genTermCallVar(st))(x6)(st))(st))(st))(st))(st))(st)



c_transformComb'39 :: Curry.Module.FlatCurry.C_CombType -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CExpr) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_transformComb'39 x1@Curry.Module.FlatCurry.C_FuncCall x2 x3 x4 x5 st = Curry.Module.TransformationComb.c_transformComb'39_case_5(x3)(x4)(x5)(x2)(st)
c_transformComb'39 x1@(Curry.Module.FlatCurry.C_FuncPartCall x8) x2 x3 x4 x5 st = Curry.Module.TransformationComb.c_transformComb'39_case_4(x3)(x4)(x8)(x2)(st)
c_transformComb'39 x1@Curry.Module.FlatCurry.C_ConsCall x2 x3 x4 x5 st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.TransformationComb.c_call(Curry.Module.TransformationDebugInfo.c_renameCons(x2)(st))(x3)(x4)))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.TransformationDebugInfo.c_insHook((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))))))))))(x5)))(Curry.Module.TransformationDebugInfo.c_wrapReturn(st))(st))(st)
c_transformComb'39 x1@(Curry.Module.FlatCurry.C_ConsPartCall x15) x2 x3 x4 x5 st = Curry.Module.TransformationComb.c_transformComb'39_case_2(x3)(x4)(x15)(x2)(st)
c_transformComb'39 (Curry.Module.FlatCurry.C_CombTypeOr i xs) x2 x3 x4 x5 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationComb.c_transformComb'39(x)(x2)(x3)(x4)(x5)(st))(i)(xs)(st)
c_transformComb'39 x x2 x3 x4 x5 st = Curry.RunTimeSystem.patternFail("TransformationComb.transformComb'")(x)



c_transformComb'39'46insertHook'469 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_transformComb'39'46insertHook'469 x1 x2 x3 st = Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_comb(Curry.Module.Prelude.T2(Curry.Module.TransformationDebugInfo.c_debugMonadAs(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('H'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))(Curry.Module.Prelude.List))))))))))))))(st))((Curry.Module.Prelude.:<)(Curry.Module.FlatToAbstractCurry.c_acyStr(x1)(st))((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))))(st)



c_call :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CExpr) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> (Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr)) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_call x1 x2 x3 x4 st = Curry.Module.TransformationComb.c_callExpr(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_comb(x1)(st))(Curry.Module.Prelude.c_map(Curry.Module.FlatToAbstractCurry.c_xx(st))(x3)(st))(st))(x2)(x3)(x4)(st)



c_callExpr :: Curry.Module.AbstractCurry.C_CExpr -> (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CExpr) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> (Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr)) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_callExpr x1 x2 x3 x4 st = Curry.Module.TransformationComb.c_callExpr_case_0(x1)(x2)(x3)(x4)(Curry.Module.Prelude.c_null(x2)(st))(st)



c_funcRep :: Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_funcRep x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_comb(Curry.Module.TransformationDebugInfo.c_funcRepCons(st))(st))((Curry.Module.Prelude.:<)(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)))(st)



c_callExpr_case_0 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_36(x4)(x1)(st)
c_callExpr_case_0 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurry.C_CDoExpr(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_zipWith(Curry.Module.Prelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pc))(Curry.Module.AbstractCurry.C_CSPat))(Curry.Module.FlatToAbstractCurry.c_px(st))(st))(x3)(x2)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CSExpr))(Curry.Module.Prelude.c_apply(x4)(x1)(st))(st))(Curry.Module.Prelude.List))(st))
c_callExpr_case_0 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationComb.c_callExpr_case_0(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_callExpr_case_0 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("TransformationComb.callExpr_case_0")(x)



c_transformComb'39_case_2 x3 x4 x15 x2@(Curry.Module.Prelude.T2 x16 x17) st = let {x18 = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_43))(Curry.Module.Prelude.c_length(x4)(st))))(Curry.Module.Prelude.c_enumFromTo(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.op_45(x15)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st)} in Curry.Module.TransformationComb.c_callExpr(Curry.Module.AbstractCurry.C_CLambda(Curry.Module.Prelude.c_map(Curry.Module.FlatToAbstractCurry.c_px(st))(x18)(st))(Curry.Module.Prelude.op_36(Curry.Module.TransformationDebugInfo.c_wrapReturn(st))(Curry.Module.Prelude.op_36(Curry.Module.FlatToAbstractCurry.c_comb(Curry.Module.TransformationDebugInfo.c_renameCons(x2)(st))(st))(Curry.Module.Prelude.c_map(Curry.Module.FlatToAbstractCurry.c_xx(st))(Curry.Module.Prelude.op_43_43(x4)(x18)(st))(st))(st))(st)))(x3)(x4)(Curry.Module.Prelude.op_46(Curry.Module.TransformationDebugInfo.c_wrapReturn(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.TransformationComb.c_transformComb'39_case_1(x15)(x16)(Curry.Module.Prelude.op_60_61(x15)(Curry.Module.TransformationPartCalls.c_arityThreshold(st))(st))(st))(x15)(st))(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_comb(Curry.Module.TransformationDebugInfo.c_pcConsName(x2)(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.FlatToAbstractCurry.c_list(Curry.Module.Prelude.c_map(Curry.Module.TransformationDebugInfo.c_genTermCallVar(st))(x4)(st))(st))(Curry.Module.Prelude.List))(st))(st))(st))(st)
c_transformComb'39_case_2 x3 x4 x15 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationComb.c_transformComb'39_case_2(x3)(x4)(x15)(x)(st))(i)(xs)(st)
c_transformComb'39_case_2 x3 x4 x15 x st = Curry.RunTimeSystem.patternFail("TransformationComb.transformComb'_case_2")(x)



c_transformComb'39_case_1 x15 x16 x17@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.TransformationPartCalls.c_combDefaultPC)
c_transformComb'39_case_1 x15 x16 x17@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.TransformationPartCalls.c_combPC(x16))
c_transformComb'39_case_1 x15 x16 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationComb.c_transformComb'39_case_1(x15)(x16)(x)(st))(i)(xs)(st)
c_transformComb'39_case_1 x15 x16 x st = Curry.RunTimeSystem.patternFail("TransformationComb.transformComb'_case_1")(x)



c_transformComb'39_case_4 x3 x4 x8 x2@(Curry.Module.Prelude.T2 x9 x10) st = Curry.Module.TransformationComb.c_call(Curry.Module.TransformationDebugInfo.c_renameFunc(x2)(st))(x3)(x4)(Curry.Module.Prelude.op_46(Curry.Module.TransformationDebugInfo.c_wrapReturn(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.TransformationComb.c_transformComb'39_case_3(x8)(x9)(Curry.Module.Prelude.op_60_61(x8)(Curry.Module.TransformationPartCalls.c_arityThreshold(st))(st))(st))(x8)(st))(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_comb(Curry.Module.TransformationDebugInfo.c_pcTermName(x2)(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.FlatToAbstractCurry.c_list(Curry.Module.Prelude.c_map(Curry.Module.TransformationDebugInfo.c_genTermCallVar(st))(x4)(st))(st))(Curry.Module.Prelude.List))(st))(st))(st))(st)
c_transformComb'39_case_4 x3 x4 x8 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationComb.c_transformComb'39_case_4(x3)(x4)(x8)(x)(st))(i)(xs)(st)
c_transformComb'39_case_4 x3 x4 x8 x st = Curry.RunTimeSystem.patternFail("TransformationComb.transformComb'_case_4")(x)



c_transformComb'39_case_3 x8 x9 x10@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.TransformationPartCalls.c_combDefaultPC)
c_transformComb'39_case_3 x8 x9 x10@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.TransformationPartCalls.c_combPC(x9))
c_transformComb'39_case_3 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationComb.c_transformComb'39_case_3(x8)(x9)(x)(st))(i)(xs)(st)
c_transformComb'39_case_3 x8 x9 x st = Curry.RunTimeSystem.patternFail("TransformationComb.transformComb'_case_3")(x)



c_transformComb'39_case_5 x3 x4 x5 x2@(Curry.Module.Prelude.T2 x6 x7) st = Curry.Module.TransformationComb.c_call(Curry.Module.TransformationDebugInfo.c_renameFunc(x2)(st))(x3)(x4)(Curry.Module.Prelude.pf(Curry.Module.TransformationComb.c_transformComb'39'46insertHook'469(x7)(x5)))(st)
c_transformComb'39_case_5 x3 x4 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationComb.c_transformComb'39_case_5(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c_transformComb'39_case_5 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("TransformationComb.transformComb'_case_5")(x)


