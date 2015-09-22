{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.AddWorld (module Curry.Module.AddWorld) where

import Curry.RunTimeSystem
import Curry.Module.FlatCurry
import Curry.Module.FlatCurryGoodies
import Curry.Module.Prelude



-- begin included



-- end included

c_main :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_main st = Curry.Module.Prelude.op_62_62_61(Curry.Module.FlatCurry.c_readFlatCurry((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_writeFile((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_show))(Curry.Module.AddWorld.c_addWorld(st))(st))(st))(st)



c_addWorld :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Prog)
c_addWorld st = Curry.Module.Prelude.op_46(Curry.Module.FlatCurryGoodies.c_updProgTypes(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.FlatCurryGoodies.c_updTypeConsDecls(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.c_apply(Curry.Module.FlatCurryGoodies.c_updConsArgs(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.AddWorld.c_addWorldTE(st))))(st))))(st))))(st))(Curry.Module.Prelude.op_46(Curry.Module.FlatCurryGoodies.c_updProgTypes(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.c_apply(Curry.Module.FlatCurryGoodies.c_updTypeSynonym(st))(Curry.Module.AddWorld.c_addWorldTE(st))(st))))(st))(Curry.Module.FlatCurryGoodies.c_updProgFuncs(Curry.Module.AddWorld.c_addWorldFuncs(st))(st))(st))(st)



c_addWorldTE :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_TypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_TypeExpr)
c_addWorldTE st = Curry.Module.Prelude.pf(Curry.Module.FlatCurryGoodies.c_trTypeExpr(Curry.Module.Prelude.pc(Curry.Module.FlatCurry.C_TVar))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.AddWorld.c_addWorldTE'46addWorldToIO'467))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pc))(Curry.Module.FlatCurry.C_FuncType)))



c_addWorldTE'46addWorldToIO'467 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_TypeExpr) -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_TypeExpr
c_addWorldTE'46addWorldToIO'467 x1 x2 st = Curry.Module.AddWorld.c_addWorldTE'46addWorldToIO'467_case_3(x1)(x2)(Curry.Module.AddWorld.c_isIO(x1)(st))(st)



c_prelude :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_prelude st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))



c_isIO :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isIO x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))(Curry.Module.Prelude.List)))(st))(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.AddWorld.c_prelude(st))(st))(st)
c_isIO (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AddWorld.c_isIO(x)(st))(i)(xs)(st)
c_isIO x st = Curry.RunTimeSystem.patternFail("AddWorld.isIO")(x)



c_unit :: Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_TypeExpr
c_unit st = Curry.Module.FlatCurry.C_TCons(Curry.Module.Prelude.T2(Curry.Module.AddWorld.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.List)



c_addWorldFuncs :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl)
c_addWorldFuncs st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.FlatCurryGoodies.c_updFuncType(Curry.Module.AddWorld.c_addWorldTE(st))(st))))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.AddWorld.c_changeExtIOFunc))))(st)



c_changeExtIOFunc :: Curry.Module.FlatCurry.C_FuncDecl -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_FuncDecl
c_changeExtIOFunc x1 st = let {x2 = Curry.Module.FlatCurryGoodies.c_resultType(Curry.Module.Prelude.c_apply(Curry.Module.FlatCurryGoodies.c_funcType(st))(x1)(st))(st)} in Curry.Module.AddWorld.c_changeExtIOFunc_case_1(x1)(x2)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.c_apply(Curry.Module.FlatCurryGoodies.c_isExternal(st))(x1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.c_apply(Curry.Module.FlatCurryGoodies.c_isTCons(st))(x2)(st))(Curry.Module.AddWorld.c_isIO(Curry.Module.FlatCurryGoodies.c_tConsName(x2)(st))(st))(st))(st))(st)



c_changeExtIOFunc_case_1 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_apply(Curry.Module.FlatCurryGoodies.c_updFuncArity(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_43))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))))(st))(x1)(st)
c_changeExtIOFunc_case_1 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.AddWorld.c_changeExtIOFunc_case_0(x1)(Curry.Module.Prelude.c_otherwise(st))(st)
c_changeExtIOFunc_case_1 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AddWorld.c_changeExtIOFunc_case_1(x1)(x2)(x)(st))(i)(xs)(st)
c_changeExtIOFunc_case_1 x1 x2 x st = Curry.RunTimeSystem.patternFail("AddWorld.changeExtIOFunc_case_1")(x)



c_changeExtIOFunc_case_0 x1 x2@Curry.Module.Prelude.C_True st = x1
c_changeExtIOFunc_case_0 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AddWorld.c_changeExtIOFunc_case_0(x1)(x)(st))(i)(xs)(st)
c_changeExtIOFunc_case_0 x1 x st = Curry.RunTimeSystem.patternFail("AddWorld.changeExtIOFunc_case_0")(x)



c_addWorldTE'46addWorldToIO'467_case_3 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.FlatCurry.C_FuncType(Curry.Module.AddWorld.c_unit(st))(Curry.Module.FlatCurry.C_TCons(x1)(x2))
c_addWorldTE'46addWorldToIO'467_case_3 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.AddWorld.c_addWorldTE'46addWorldToIO'467_case_2(x1)(x2)(Curry.Module.Prelude.c_otherwise(st))(st)
c_addWorldTE'46addWorldToIO'467_case_3 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AddWorld.c_addWorldTE'46addWorldToIO'467_case_3(x1)(x2)(x)(st))(i)(xs)(st)
c_addWorldTE'46addWorldToIO'467_case_3 x1 x2 x st = Curry.RunTimeSystem.patternFail("AddWorld.addWorldTE.addWorldToIO.7_case_3")(x)



c_addWorldTE'46addWorldToIO'467_case_2 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.FlatCurry.C_TCons(x1)(x2)
c_addWorldTE'46addWorldToIO'467_case_2 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AddWorld.c_addWorldTE'46addWorldToIO'467_case_2(x1)(x2)(x)(st))(i)(xs)(st)
c_addWorldTE'46addWorldToIO'467_case_2 x1 x2 x st = Curry.RunTimeSystem.patternFail("AddWorld.addWorldTE.addWorldToIO.7_case_2")(x)


