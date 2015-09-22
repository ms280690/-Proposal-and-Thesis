{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.TransformationSignatures (module Curry.Module.TransformationSignatures) where

import Curry.RunTimeSystem
import Curry.Module.AbstractCurry
import Curry.Module.FlatCurry
import Curry.Module.FlatToAbstractCurry
import Curry.Module.Prelude
import Curry.Module.TransformationDebugInfo
import Curry.Module.System



-- begin included



-- end included

c_transformType :: Curry.Module.Prelude.C_Int -> Curry.Module.FlatCurry.C_TypeExpr -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CTypeExpr
c_transformType x1 x2@(Curry.Module.FlatCurry.C_TVar x4) x3 st = Curry.Module.Prelude.op_36(Curry.Module.TransformationSignatures.c_debugTVarWrapper(x1)(st))(Curry.Module.FlatToAbstractCurry.c_convertType(x2)(st))(st)
c_transformType x1 x2@(Curry.Module.FlatCurry.C_TCons x5 x6) x3 st = let {x7 = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.TransformationSignatures.c_transformType'46_'35lambda2(x3)))(x6)(st)} in Curry.Module.Prelude.op_36(Curry.Module.TransformationSignatures.c_debugTVarWrapper(x1)(st))(Curry.Module.AbstractCurry.C_CTCons(Curry.Module.TransformationDebugInfo.c_renameType(x5)(st))(Curry.Module.TransformationSignatures.c_transformType_case_3(x3)(x5)(x7)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(x5)(st))(x3)(st))(st)))(st)
c_transformType x1 x2@(Curry.Module.FlatCurry.C_FuncType x9 x10) x3 st = Curry.Module.TransformationSignatures.c_transformType_case_2(x1)(x3)(x9)(x10)(Curry.Module.Prelude.op_62(x1)(Curry.Module.Prelude.C_Zero)(st))(st)
c_transformType x1 (Curry.Module.FlatCurry.C_TypeExprOr i xs) x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationSignatures.c_transformType(x1)(x)(x3)(st))(i)(xs)(st)
c_transformType x1 x x3 st = Curry.RunTimeSystem.patternFail("TransformationSignatures.transformType")(x)



c_transformType'46_'35lambda2 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.FlatCurry.C_TypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CTypeExpr
c_transformType'46_'35lambda2 x1 x2 st = Curry.Module.TransformationSignatures.c_transformType(Curry.Module.Prelude.c_negate(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x2)(x1)(st)



c_transformType'46funcRepr'4614 :: Curry.Module.AbstractCurry.C_CTypeExpr -> Curry.Module.AbstractCurry.C_CTypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CTypeExpr
c_transformType'46funcRepr'4614 x1 x2 st = Curry.Module.AbstractCurry.C_CTCons(Curry.Module.TransformationDebugInfo.c_funcRepType(st))((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CTCons(Curry.Module.TransformationDebugInfo.c_debugTVar(st))(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))))



c_transformType'46transformParamType'4614 :: Curry.Module.FlatCurry.C_TypeExpr -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CTypeExpr
c_transformType'46transformParamType'4614 x1@(Curry.Module.FlatCurry.C_TVar x3) x2 st = Curry.Module.FlatToAbstractCurry.c_convertType(x1)(st)
c_transformType'46transformParamType'4614 x1@(Curry.Module.FlatCurry.C_FuncType x4 x5) x2 st = Curry.Module.TransformationSignatures.c_transformType(Curry.Module.Prelude.c_negate(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x1)(x2)(st)
c_transformType'46transformParamType'4614 x1@(Curry.Module.FlatCurry.C_TCons x6 x7) x2 st = Curry.Module.TransformationSignatures.c_transformType(Curry.Module.Prelude.c_negate(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x1)(x2)(st)
c_transformType'46transformParamType'4614 (Curry.Module.FlatCurry.C_TypeExprOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationSignatures.c_transformType'46transformParamType'4614(x)(x2)(st))(i)(xs)(st)
c_transformType'46transformParamType'4614 x x2 st = Curry.RunTimeSystem.patternFail("TransformationSignatures.transformType.transformParamType.14")(x)



c_debugTVarWrapper :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CTypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CTypeExpr)
c_debugTVarWrapper x1 st = Curry.Module.TransformationSignatures.c_debugTVarWrapper_case_0(x1)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Zero)(st))(st)



c_debugTVarWrapper_case_0 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.TransformationDebugInfo.c_wrapDebugTVar(st)
c_debugTVarWrapper_case_0 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id)
c_debugTVarWrapper_case_0 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationSignatures.c_debugTVarWrapper_case_0(x1)(x)(st))(i)(xs)(st)
c_debugTVarWrapper_case_0 x1 x st = Curry.RunTimeSystem.patternFail("TransformationSignatures.debugTVarWrapper_case_0")(x)



c_transformType_case_2 x1 x3 x9 x10 x11@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurry.C_CFuncType(Curry.Module.TransformationSignatures.c_transformType'46transformParamType'4614(x9)(x3)(st))(Curry.Module.TransformationSignatures.c_transformType(Curry.Module.Prelude.op_45(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x10)(x3)(st))
c_transformType_case_2 x1 x3 x9 x10 x11@Curry.Module.Prelude.C_False st = Curry.Module.TransformationSignatures.c_transformType_case_1(x1)(x3)(x9)(x10)(Curry.Module.Prelude.op_60_61(x1)(Curry.Module.Prelude.C_Zero)(st))(st)
c_transformType_case_2 x1 x3 x9 x10 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationSignatures.c_transformType_case_2(x1)(x3)(x9)(x10)(x)(st))(i)(xs)(st)
c_transformType_case_2 x1 x3 x9 x10 x st = Curry.RunTimeSystem.patternFail("TransformationSignatures.transformType_case_2")(x)



c_transformType_case_1 x1 x3 x9 x10 x11@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_36(Curry.Module.TransformationSignatures.c_debugTVarWrapper(x1)(st))(Curry.Module.TransformationSignatures.c_transformType'46funcRepr'4614(Curry.Module.TransformationSignatures.c_transformType'46transformParamType'4614(x9)(x3)(st))(Curry.Module.TransformationSignatures.c_transformType(Curry.Module.Prelude.c_negate(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x10)(x3)(st))(st))(st)
c_transformType_case_1 x1 x3 x9 x10 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationSignatures.c_transformType_case_1(x1)(x3)(x9)(x10)(x)(st))(i)(xs)(st)
c_transformType_case_1 x1 x3 x9 x10 x st = Curry.RunTimeSystem.patternFail("TransformationSignatures.transformType_case_1")(x)



c_transformType_case_3 x3 x5 x7 x8@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CTVar(Curry.Module.Prelude.T2(Curry.Module.Prelude.c_negate(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.TransformationDebugInfo.c_debugTVarName(st))))(x7)
c_transformType_case_3 x3 x5 x7 x8@Curry.Module.Prelude.C_False st = x7
c_transformType_case_3 x3 x5 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationSignatures.c_transformType_case_3(x3)(x5)(x7)(x)(st))(i)(xs)(st)
c_transformType_case_3 x3 x5 x7 x st = Curry.RunTimeSystem.patternFail("TransformationSignatures.transformType_case_3")(x)


