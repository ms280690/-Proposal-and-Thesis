{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.TransformationPartCalls (module Curry.Module.TransformationPartCalls) where

import Curry.RunTimeSystem
import Curry.Module.AbstractCurry
import Curry.Module.AbstractHaskell
import Curry.Module.FlatToAbstractCurry
import Curry.Module.Prelude
import Curry.Module.System
import Curry.Module.TransformationDebugInfo
import Curry.Module.TransformationPrint



-- begin included



-- end included

c_arityThreshold :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_arityThreshold st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))



c_partCallPrefix :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_partCallPrefix st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List))))))))



c_partCallsFile :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_partCallsFile st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))))))



c_partCallsModule :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_partCallsModule st = Curry.Module.Prelude.op_43_43(Curry.Module.TransformationDebugInfo.c_debugPackage(st))(Curry.Module.TransformationPartCalls.c_partCallsFile(st))(st)



c_partCallsAs :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_partCallsAs st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))(Curry.Module.Prelude.List))



c_partCallsImport :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_partCallsImport st = Curry.Module.Prelude.op_43_43(Curry.Module.TransformationPartCalls.c_partCallsModule(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.TransformationPartCalls.c_partCallsAs(st))(st))(st)



c_main :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_main st = Curry.Module.Prelude.op_62_62_61(Curry.Module.System.c_getArgs(st))(Curry.Module.Prelude.pf(Curry.Module.TransformationPartCalls.c_main'46_'35lambda2))(st)



c_main'46_'35lambda2 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_main'46_'35lambda2 x1 st = let {x2 = Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_head(x1)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.TransformationPartCalls.c_partCallsFile(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))(st))(st))(st)} in Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_putStr((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('`'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))(st))(Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_putStrLn))(Curry.Module.Prelude.op_43_43(Curry.Module.TransformationPartCalls.c_partCallPrefix(st))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_show(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('`'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List))(st))(st))(st))(st))(st))(Curry.Module.TransformationPrint.c_prettyacy(x2)(Curry.Module.TransformationPartCalls.c_mkPartCallsModule(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(st))(st))(st))(st)



c_mkPartCallsModule :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.AbstractHaskell.C_HaskellProg
c_mkPartCallsModule x1 st = Curry.Module.AbstractHaskell.C_HaskellProg(Curry.Module.TransformationPartCalls.c_partCallsModule(st))(Curry.Module.Prelude.List)((Curry.Module.Prelude.:<)(Curry.Module.TransformationDebugInfo.c_debugMonadImport(st))((Curry.Module.Prelude.:<)(Curry.Module.TransformationDebugInfo.c_debugInfoImport(st))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.TransformationPartCalls.c_mkPartCallHelper(Curry.Module.TransformationPartCalls.c_partCallsAs(st))))(Curry.Module.Prelude.c_enumFromTo(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(st))(Curry.Module.Prelude.List)



c_mkPartCallHelper :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.AbstractHaskell.C_HFuncDecl
c_mkPartCallHelper x1 x2 st = Curry.Module.AbstractHaskell.C_HFunc(Curry.Module.Prelude.T2(x1)(Curry.Module.Prelude.op_43_43(Curry.Module.TransformationPartCalls.c_partCallPrefix(st))(Curry.Module.Prelude.c_show(x2)(st))(st)))(x2)(Curry.Module.AbstractCurry.C_Public)((Curry.Module.Prelude.:<)(Curry.Module.TransformationDebugInfo.c_debugMonadConstraint(st))(Curry.Module.TransformationPartCalls.c_termConstraints(x2)(st)))(Curry.Module.TransformationPartCalls.c_mkType(x2)(st))(Curry.Module.AbstractCurry.C_CRules(Curry.Module.AbstractCurry.C_CFlex)((Curry.Module.Prelude.:<)(Curry.Module.TransformationPartCalls.c_createRule(x2)(st))(Curry.Module.Prelude.List)))



c_combDefaultPC :: Curry.Module.Prelude.C_Int -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_combDefaultPC x1 x2 x3 st = Curry.Module.TransformationPartCalls.c_combDefaultPC_case_3(x1)(x2)(x3)(Curry.Module.Prelude.op_62(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(st))(st)



c_combPC :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Int -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_combPC x1 x2 x3 x4 st = Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_comb(Curry.Module.Prelude.T2(x1)(Curry.Module.Prelude.op_43_43(Curry.Module.TransformationPartCalls.c_partCallPrefix(st))(Curry.Module.Prelude.c_show(x2)(st))(st)))(st))((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(st)



c_termConstraints :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_TypeClass
c_termConstraints x1 st = Curry.Module.Prelude.c_map(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_constraint(Curry.Module.TransformationDebugInfo.c_staticInfoClass(st))))(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_convertTypeVariable))(st))(Curry.Module.Prelude.c_enumFromTo(Curry.Module.Prelude.C_Zero)(x1)(st))(st)



c_createRule :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CRule
c_createRule x1 st = Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_noGuardRule((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CPComb(Curry.Module.TransformationDebugInfo.c_staticInfoCons(st))((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CPVar(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Zero)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CPVar(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CPVar(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))))(Curry.Module.Prelude.List)))))((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CPVar(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.List)))(Curry.Module.TransformationPartCalls.c_mkBody(x1)(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CLocalFunc(Curry.Module.AbstractCurry.C_CFunc(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.C_Zero)(Curry.Module.AbstractCurry.C_Public)(Curry.Module.FlatToAbstractCurry.c_untyped(st))(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_rules(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_constantRule(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36(st))(Curry.Module.FlatToAbstractCurry.c_point(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36(st))(Curry.Module.AbstractCurry.C_CSymbol(Curry.Module.TransformationDebugInfo.c_staticInfoCons(st)))(st))(Curry.Module.AbstractCurry.C_CVar(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Zero)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))(st))(st))(Curry.Module.AbstractCurry.C_CVar(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))(Curry.Module.Prelude.List)))))))(st))(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36(st))(Curry.Module.FlatToAbstractCurry.c_presym((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('+'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('+'))(Curry.Module.Prelude.List)))(st))(st))(Curry.Module.AbstractCurry.C_CVar(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))))(st))(st))(st))(Curry.Module.Prelude.List))(st))))(Curry.Module.Prelude.List))(st)



c_mkBody :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_mkBody x1 st = Curry.Module.TransformationPartCalls.c_mkLambdas(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st)



c_mkLambdas :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_mkLambdas x1 x2 st = let {x5 = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36(st))(Curry.Module.AbstractCurry.C_CSymbol(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))(Curry.Module.Prelude.List)))))))(st))(Curry.Module.FlatToAbstractCurry.c_list(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.TransformationPartCalls.c_mkLambdas'46_'35lambda3(x1)))(Curry.Module.Prelude.c_enumFromTo(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(Curry.Module.Prelude.op_45(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st))(st))(st)} in let {x6 = Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CLambda((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_px(st))(x2)(st))(Curry.Module.Prelude.List)))} in Curry.Module.TransformationPartCalls.c_mkLambdas_case_1(x1)(x2)(x5)(x6)(Curry.Module.Prelude.op_61_61(x1)(x2)(st))(st)



c_mkLambdas'46_'35lambda3 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_mkLambdas'46_'35lambda3 x1 x2 st = Curry.Module.AbstractCurry.C_CVar(Curry.Module.Prelude.T2(Curry.Module.Prelude.op_43(x1)(x2)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.c_show(x2)(st))))



c_mkLambdas'46funcRep'4661 :: Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_mkLambdas'46funcRep'4661 x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36(st))(Curry.Module.AbstractCurry.C_CSymbol(Curry.Module.TransformationDebugInfo.c_funcRepCons(st)))(st))(x1)(st))(st))(x2)(st)



c_mkDo :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_mkDo x1 x2 st = Curry.Module.AbstractCurry.C_CDoExpr((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CSLet((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CLocalPat(Curry.Module.AbstractCurry.C_CPVar(Curry.Module.Prelude.T2(Curry.Module.Prelude.op_43(x1)(x2)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.c_show(x2)(st)))))(Curry.Module.Prelude.c_apply(Curry.Module.TransformationDebugInfo.c_genTermCallVar(st))(x2)(st))(Curry.Module.Prelude.List))(Curry.Module.Prelude.List)))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id))(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CSExpr))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36(st))(Curry.Module.FlatToAbstractCurry.c_presym((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List)))))))(st))(st))(Curry.Module.TransformationPartCalls.c_mkLambdas(x1)(Curry.Module.Prelude.op_43(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st))(st))(st))(Curry.Module.Prelude.List)))



c_mkCall :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_mkCall x1 st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36_36(st))(Curry.Module.AbstractCurry.C_CSymbol(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))(Curry.Module.Prelude.List))))(st))(Curry.Module.Prelude.c_map(Curry.Module.FlatToAbstractCurry.c_xx(st))(Curry.Module.Prelude.c_enumFromTo(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(st))(st))(st)



op_126_126_62 :: Curry.Module.AbstractCurry.C_CTypeExpr -> Curry.Module.AbstractCurry.C_CTypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CTypeExpr
op_126_126_62 x1 x2 st = Curry.Module.AbstractCurry.C_CTCons(Curry.Module.TransformationDebugInfo.c_funcRepType(st))((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CTVar(Curry.Module.Prelude.T2(Curry.Module.Prelude.c_negate(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.TransformationDebugInfo.c_debugTVarName(st))))((Curry.Module.Prelude.:<)(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))))



op_126_62 :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CTypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CTypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CTypeExpr))
op_126_62 st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pc))(Curry.Module.AbstractCurry.C_CFuncType)



c_mkType :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CTypeExpr
c_mkType x1 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.TransformationPartCalls.op_126_62(st))(Curry.Module.AbstractCurry.C_CTCons(Curry.Module.TransformationDebugInfo.c_staticInfoCons(st))(Curry.Module.Prelude.List))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.TransformationPartCalls.op_126_62(st))(Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.pf(Curry.Module.TransformationPartCalls.c_mkType'46_'35lambda4))(Curry.Module.AbstractCurry.C_CTCons(Curry.Module.TransformationDebugInfo.c_debugTVar(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_tx(st))(x1)(st))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.c_enumFromTo(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.op_45(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st))(st))(Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.TransformationPartCalls.c_mkType'46_'35lambda5))(Curry.Module.Prelude.op_36(Curry.Module.FlatToAbstractCurry.c_tx(st))(x1)(st))(Curry.Module.Prelude.c_enumFromTo(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.op_45(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st))(st))(st)



c_mkType'46_'35lambda4 :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CTypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CTypeExpr)
c_mkType'46_'35lambda4 x1 st = Curry.Module.Prelude.c_apply(Curry.Module.TransformationPartCalls.op_126_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_tx(st))(x1)(st))(st)



c_mkType'46_'35lambda5 :: Curry.Module.Prelude.C_Int -> Curry.Module.AbstractCurry.C_CTypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CTypeExpr
c_mkType'46_'35lambda5 x1 x2 st = Curry.Module.TransformationPartCalls.op_126_126_62(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_tx(st))(x1)(st))(x2)(st)



c_mkLambdas_case_1 x1 x2 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.TransformationPartCalls.c_mkLambdas'46funcRep'4661(x5)))(Curry.Module.Prelude.op_36(x6)(Curry.Module.TransformationPartCalls.c_mkCall(x1)(st))(st))(st)
c_mkLambdas_case_1 x1 x2 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.TransformationPartCalls.c_mkLambdas_case_0(x1)(x2)(x5)(x6)(Curry.Module.Prelude.c_otherwise(st))(st)
c_mkLambdas_case_1 x1 x2 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationPartCalls.c_mkLambdas_case_1(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c_mkLambdas_case_1 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("TransformationPartCalls.mkLambdas_case_1")(x)



c_mkLambdas_case_0 x1 x2 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.TransformationPartCalls.c_mkLambdas'46funcRep'4661(x5)))(Curry.Module.Prelude.op_36(x6)(Curry.Module.TransformationPartCalls.c_mkDo(x1)(x2)(st))(st))(st)
c_mkLambdas_case_0 x1 x2 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationPartCalls.c_mkLambdas_case_0(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c_mkLambdas_case_0 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("TransformationPartCalls.mkLambdas_case_0")(x)



c_combDefaultPC_case_3 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))(st)
c_combDefaultPC_case_3 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.TransformationPartCalls.c_combDefaultPC_case_2(x1)(x2)(x3)(Curry.Module.Prelude.c_otherwise(st))(st)
c_combDefaultPC_case_3 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationPartCalls.c_combDefaultPC_case_3(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_combDefaultPC_case_3 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("TransformationPartCalls.combDefaultPC_case_3")(x)



c_combDefaultPC_case_2 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.TransformationPartCalls.c_combPC(Curry.Module.TransformationPartCalls.c_partCallsAs(st))(x1)(x2)(x3)(st)
c_combDefaultPC_case_2 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationPartCalls.c_combDefaultPC_case_2(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_combDefaultPC_case_2 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("TransformationPartCalls.combDefaultPC_case_2")(x)


