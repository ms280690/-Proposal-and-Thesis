{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.ExternalStubs (module Curry.Module.ExternalStubs) where

import Curry.RunTimeSystem
import Curry.Module.AbstractCurry
import Curry.Module.AbstractHaskell
import Curry.Module.FlatCurry
import Curry.Module.FlatCurryGoodies
import Curry.Module.FlatToAbstractCurry
import Curry.Module.IO
import Curry.Module.Prelude
import Curry.Module.TransformationDebugInfo
import Curry.Module.TransformationPrint
import Curry.Module.TransformationSignatures
import Curry.Module.System



-- begin included



-- end included

c_stubsInfo :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_stubsInfo st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('{'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('}'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))



c_containsExt :: Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_containsExt x1@(Curry.Module.FlatCurry.C_Prog x2 x3 x4 x5 x6) st = Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_any(Curry.Module.FlatCurryGoodies.c_isExternal(st))(st))(x5)(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_any(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_null))(Curry.Module.FlatCurryGoodies.c_typeConsDecls(st))(st))(st))(x4)(st))(st)
c_containsExt (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.ExternalStubs.c_containsExt(x)(st))(i)(xs)(st)
c_containsExt x st = Curry.RunTimeSystem.patternFail("ExternalStubs.containsExt")(x)



c_mkStubs :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.AbstractHaskell.C_HaskellProg)
c_mkStubs st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.ExternalStubs.c_mkStubs'39))))(Curry.Module.TransformationDebugInfo.c_higherOrderTypes(st))(st)



c_writeStubs :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_writeStubs x1 x2 st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.TransformationPrint.c_prettyacy(x1)))(Curry.Module.Prelude.op_36(Curry.Module.ExternalStubs.c_mkStubs(st))(Curry.Module.Prelude.c_apply(Curry.Module.FlatCurryGoodies.c_updQNamesInProg(Curry.Module.Prelude.pf(Curry.Module.TransformationDebugInfo.c_renameModule))(st))(x2)(st))(st))(st))(Curry.Module.ExternalStubs.c_clean(x1)(st))(st)



c_mkStubs'39 :: Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractHaskell.C_HaskellProg
c_mkStubs'39 x1@(Curry.Module.FlatCurry.C_Prog x3 x4 x5 x6 x7) x2 st = let {x10 = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_unzip))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.ExternalStubs.c_handleType))(st))(x5)(st))(st)} in Curry.Module.AbstractHaskell.C_HaskellProg(Curry.Module.Prelude.op_43_43(Curry.Module.TransformationDebugInfo.c_modulePrefix(st))(x3)(st))(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)(Curry.Module.ExternalStubs.c_mkStubs'39'46_'35selFP3'35types'39(x10)(st))(Curry.Module.ExternalStubs.c_mkStubs'39'46_'35selFP4'35insts(x10)(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.ExternalStubs.c_mkStubs'39'46_'35lambda2(x2)))(st))(x6)(st))(Curry.Module.Prelude.List)
c_mkStubs'39 (Curry.Module.FlatCurry.C_ProgOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.ExternalStubs.c_mkStubs'39(x)(x2)(st))(i)(xs)(st)
c_mkStubs'39 x x2 st = Curry.RunTimeSystem.patternFail("ExternalStubs.mkStubs'")(x)



c_mkStubs'39'46_'35lambda2 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.FlatCurry.C_FuncDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HFuncDecl
c_mkStubs'39'46_'35lambda2 x1 x2 st = Curry.Module.ExternalStubs.c_mkStub(x2)(x1)(st)



c_mkStubs'39'46_'35selFP3'35types'39 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HTypeDecl) (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_InstanceDecl)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HTypeDecl
c_mkStubs'39'46_'35selFP3'35types'39 x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_mkStubs'39'46_'35selFP3'35types'39 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.ExternalStubs.c_mkStubs'39'46_'35selFP3'35types'39(x)(st))(i)(xs)(st)
c_mkStubs'39'46_'35selFP3'35types'39 x st = Curry.RunTimeSystem.patternFail("ExternalStubs.mkStubs'._#selFP3#types'")(x)



c_mkStubs'39'46_'35selFP4'35insts :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HTypeDecl) (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_InstanceDecl)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_InstanceDecl
c_mkStubs'39'46_'35selFP4'35insts x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_mkStubs'39'46_'35selFP4'35insts (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.ExternalStubs.c_mkStubs'39'46_'35selFP4'35insts(x)(st))(i)(xs)(st)
c_mkStubs'39'46_'35selFP4'35insts x st = Curry.RunTimeSystem.patternFail("ExternalStubs.mkStubs'._#selFP4#insts")(x)



c_mkStub :: Curry.Module.FlatCurry.C_FuncDecl -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HFuncDecl
c_mkStub x1@(Curry.Module.FlatCurry.C_Func x3 x4 x5 x6 x7) x2 st = Curry.Module.ExternalStubs.c_mkStub_case_4(x1)(x2)(x3)(x4)(x6)(x7)(st)
c_mkStub (Curry.Module.FlatCurry.C_FuncDeclOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.ExternalStubs.c_mkStub(x)(x2)(st))(i)(xs)(st)
c_mkStub x x2 st = Curry.RunTimeSystem.patternFail("ExternalStubs.mkStub")(x)



c_handleType :: Curry.Module.FlatCurry.C_TypeDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.AbstractHaskell.C_HTypeDecl Curry.Module.AbstractHaskell.C_InstanceDecl)
c_handleType x1@(Curry.Module.FlatCurry.C_TypeSyn x2 x3 x4 x5) st = Curry.Module.Prelude.List
c_handleType x1@(Curry.Module.FlatCurry.C_Type x6 x7 x8 x9) st = Curry.Module.ExternalStubs.c_handleType_case_2(x1)(x9)(st)
c_handleType (Curry.Module.FlatCurry.C_TypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.ExternalStubs.c_handleType(x)(st))(i)(xs)(st)
c_handleType x st = Curry.RunTimeSystem.patternFail("ExternalStubs.handleType")(x)



c_mkType :: Curry.Module.FlatCurry.C_TypeDecl -> Curry.RunTimeSystem.State -> Curry.Module.AbstractHaskell.C_HTypeDecl
c_mkType x1@(Curry.Module.FlatCurry.C_Type x2 x3 x4 x5) st = let {x6 = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_convertTypeVariable))(x4)(st)} in let {x7 = Curry.Module.Prelude.T2(Curry.Module.Prelude.c_negate(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.TransformationDebugInfo.c_debugTVarName(st))} in let {x9 = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.TransformationDebugInfo.c_renameType))(x2)(st)} in Curry.Module.AbstractHaskell.C_HTypeDecl((Curry.Module.Prelude.:<)(Curry.Module.FlatToAbstractCurry.c_constraint(Curry.Module.TransformationDebugInfo.c_debugMonadClass(st))(x7)(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_constraint(Curry.Module.TransformationDebugInfo.c_staticInfoClass(st))))(x6)(st)))(Curry.Module.AbstractCurry.C_CType(x9)(Curry.Module.AbstractCurry.C_Private)((Curry.Module.Prelude.:<)(x7)(x6))((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CCons(x9)(Curry.Module.Prelude.c_length(x4)(st))(Curry.Module.AbstractCurry.C_Private)(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CTVar))(x6)(st)))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.List)
c_mkType (Curry.Module.FlatCurry.C_TypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.ExternalStubs.c_mkType(x)(st))(i)(xs)(st)
c_mkType x st = Curry.RunTimeSystem.patternFail("ExternalStubs.mkType")(x)



c_mkInst :: Curry.Module.AbstractHaskell.C_HTypeDecl -> Curry.RunTimeSystem.State -> Curry.Module.AbstractHaskell.C_InstanceDecl
c_mkInst x1@(Curry.Module.AbstractHaskell.C_HTypeDecl x2 x3 x4) st = Curry.Module.ExternalStubs.c_mkInst_case_0(x3)(st)
c_mkInst (Curry.Module.AbstractHaskell.C_HTypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.ExternalStubs.c_mkInst(x)(st))(i)(xs)(st)
c_mkInst x st = Curry.RunTimeSystem.patternFail("ExternalStubs.mkInst")(x)



c_noImpl :: Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_noImpl st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36(st))(Curry.Module.FlatToAbstractCurry.c_presym((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))))(st))(st))(Curry.Module.FlatToAbstractCurry.c_acyStr((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))(Curry.Module.Prelude.List))))))))))))))))(st))(st)



c_clean :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_clean x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.IO.c_openFile(x1)(Curry.Module.IO.C_ReadMode)(st))(Curry.Module.Prelude.pf(Curry.Module.ExternalStubs.c_clean'46_'35lambda3(x1)))(st)



c_clean'46_'35lambda3 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.IO.C_Handle -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_clean'46_'35lambda3 x1 x2 st = Curry.Module.Prelude.op_62_62(Curry.Module.IO.c_hGetLine(x2)(st))(Curry.Module.Prelude.op_62_62_61(Curry.Module.IO.c_hGetContents(x2)(st))(Curry.Module.Prelude.pf(Curry.Module.ExternalStubs.c_clean'46_'35lambda3'46_'35lambda4(x1)(x2)))(st))(st)



c_clean'46_'35lambda3'46_'35lambda4 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.IO.C_Handle -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_clean'46_'35lambda3'46_'35lambda4 x1 x2 x3 st = Curry.Module.Prelude.op_62_62(Curry.Module.IO.c_hClose(x2)(st))(Curry.Module.Prelude.c_writeFile(x1)(Curry.Module.Prelude.op_43_43(Curry.Module.ExternalStubs.c_stubsInfo(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x3)(st))(st))(st))(st)



c_mkInst_case_0 x3@(Curry.Module.AbstractCurry.C_CType x5 x6 x7 x8) st = Curry.Module.AbstractHaskell.C_Instance(Curry.Module.Prelude.List)(Curry.Module.AbstractHaskell.C_TypeClass(Curry.Module.TransformationDebugInfo.c_staticInfoClass(st))((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CTCons(x5)(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CTVar))(x7)(st)))(Curry.Module.Prelude.List)))((Curry.Module.Prelude.:<)(Curry.Module.AbstractHaskell.C_HFunc(Curry.Module.TransformationDebugInfo.c_staticInfoFunc(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(Curry.Module.AbstractCurry.C_Private)(Curry.Module.Prelude.List)(Curry.Module.FlatToAbstractCurry.c_untyped(st))(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_rules(st))((Curry.Module.Prelude.:<)(Curry.Module.FlatToAbstractCurry.c_simpleRule((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_px(st))(Curry.Module.Prelude.C_Zero)(st))(Curry.Module.Prelude.List))(Curry.Module.ExternalStubs.c_noImpl(st))(st))(Curry.Module.Prelude.List))(st)))(Curry.Module.Prelude.List))
c_mkInst_case_0 (Curry.Module.AbstractCurry.C_CTypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.ExternalStubs.c_mkInst_case_0(x)(st))(i)(xs)(st)
c_mkInst_case_0 x st = Curry.RunTimeSystem.patternFail("ExternalStubs.mkInst_case_0")(x)



c_handleType_case_2 x1 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.Prelude.List
c_handleType_case_2 x1 x9@Curry.Module.Prelude.List st = let {x12 = Curry.Module.ExternalStubs.c_mkType(x1)(st)} in Curry.Module.ExternalStubs.c_handleType_case_1(x1)(x12)(Curry.Module.TransformationDebugInfo.c_skippedType(x1)(st))(st)
c_handleType_case_2 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.ExternalStubs.c_handleType_case_2(x1)(x)(st))(i)(xs)(st)
c_handleType_case_2 x1 x st = Curry.RunTimeSystem.patternFail("ExternalStubs.handleType_case_2")(x)



c_handleType_case_1 x1 x12 x13@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.List
c_handleType_case_1 x1 x12 x13@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(x12)(Curry.Module.ExternalStubs.c_mkInst(x12)(st)))(Curry.Module.Prelude.List)
c_handleType_case_1 x1 x12 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.ExternalStubs.c_handleType_case_1(x1)(x12)(x)(st))(i)(xs)(st)
c_handleType_case_1 x1 x12 x st = Curry.RunTimeSystem.patternFail("ExternalStubs.handleType_case_1")(x)



c_mkStub_case_4 x1 x2 x3 x4 x6 x7@(Curry.Module.FlatCurry.C_Rule x8 x9) st = Curry.Module.Prelude.List
c_mkStub_case_4 x1 x2 x3 x4 x6 x7@(Curry.Module.FlatCurry.C_External x10) st = Curry.Module.ExternalStubs.c_mkStub_case_3(x1)(x2)(x3)(x4)(x6)(Curry.Module.TransformationDebugInfo.c_skippedFunc(x1)(st))(st)
c_mkStub_case_4 x1 x2 x3 x4 x6 (Curry.Module.FlatCurry.C_RuleOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.ExternalStubs.c_mkStub_case_4(x1)(x2)(x3)(x4)(x6)(x)(st))(i)(xs)(st)
c_mkStub_case_4 x1 x2 x3 x4 x6 x st = Curry.RunTimeSystem.patternFail("ExternalStubs.mkStub_case_4")(x)



c_mkStub_case_3 x1 x2 x3 x4 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.List
c_mkStub_case_3 x1 x2 x3 x4 x6 x7@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pc(Curry.Module.AbstractHaskell.C_HFunc(Curry.Module.TransformationDebugInfo.c_renameFunc(x3)(st))(x4)(Curry.Module.AbstractCurry.C_Private)((Curry.Module.Prelude.:<)(Curry.Module.FlatToAbstractCurry.c_constraint(Curry.Module.TransformationDebugInfo.c_debugMonadClass(st))(Curry.Module.Prelude.T2(Curry.Module.Prelude.c_negate(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.TransformationDebugInfo.c_debugTVarName(st)))(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_constraint(Curry.Module.TransformationDebugInfo.c_staticInfoClass(st))))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_convertTypeVariable))(Curry.Module.FlatToAbstractCurry.c_extractTVars(x6)(st))(st))(st)))(Curry.Module.TransformationSignatures.c_transformType(x4)(x6)(x2)(st))))(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_rules(st))((Curry.Module.Prelude.:<)(Curry.Module.FlatToAbstractCurry.c_simpleRule(Curry.Module.Prelude.c_map(Curry.Module.FlatToAbstractCurry.c_px(st))(Curry.Module.Prelude.c_enumFromTo(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.op_45(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36(st))(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_comb(Curry.Module.TransformationDebugInfo.c_hookHelperName(x3)(st))(st))(Curry.Module.Prelude.c_map(Curry.Module.FlatToAbstractCurry.c_xx(st))(Curry.Module.Prelude.c_enumFromTo(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.op_45(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st))(st))(st))(Curry.Module.ExternalStubs.c_noImpl(st))(st))(st))(Curry.Module.Prelude.List))(st))(st))(Curry.Module.Prelude.List)
c_mkStub_case_3 x1 x2 x3 x4 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.ExternalStubs.c_mkStub_case_3(x1)(x2)(x3)(x4)(x6)(x)(st))(i)(xs)(st)
c_mkStub_case_3 x1 x2 x3 x4 x6 x st = Curry.RunTimeSystem.patternFail("ExternalStubs.mkStub_case_3")(x)


