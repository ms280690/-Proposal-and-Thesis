{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.FlatToAbstractCurry (module Curry.Module.FlatToAbstractCurry) where

import Curry.RunTimeSystem
import Curry.Module.AbstractCurry
import Curry.Module.AbstractCurryPrinter
import Curry.Module.AbstractHaskell
import Curry.Module.Assertion
import Curry.Module.FlatCurry
import Curry.Module.FlatCurryGoodies
import Curry.Module.List
import Curry.Module.Prelude



-- begin included



-- end included

c_convertVariable :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_convertVariable x1 st = Curry.Module.FlatToAbstractCurry.c_convertVariable_case_10(x1)(Curry.Module.Prelude.op_60(x1)(Curry.Module.Prelude.C_Zero)(st))(st)



c_convertTypeVariable :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_convertTypeVariable x1 st = Curry.Module.FlatToAbstractCurry.c_convertTypeVariable_case_8(x1)(Curry.Module.Prelude.op_60(x1)(Curry.Module.Prelude.C_Zero)(st))(st)



c_xx :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr)
c_xx st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CVar))(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_convertVariable))(st)



c_px :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CPattern)
c_px st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CPVar))(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_convertVariable))(st)



c_tx :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CTypeExpr)
c_tx st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CTVar))(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_convertTypeVariable))(st)



c_freshVariable :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int)
c_freshVariable st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_43))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_freshVariable'46maximum'4615))(Curry.Module.Prelude.pf(Curry.Module.FlatCurryGoodies.c_allVars))(st))(st)



c_freshVariable'46maximum'4615 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_freshVariable'46maximum'4615 x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.c_negate(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st)
c_freshVariable'46maximum'4615 x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.FlatToAbstractCurry.c_freshVariable'46maximum'4615_case_5(x2)(x3)(st)
c_freshVariable'46maximum'4615 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_freshVariable'46maximum'4615(x)(st))(i)(xs)(st)
c_freshVariable'46maximum'4615 x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.freshVariable.maximum.15")(x)



c_extractTVars :: Curry.Module.FlatCurry.C_TypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_extractTVars x1@(Curry.Module.FlatCurry.C_TVar x2) st = (Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)
c_extractTVars x1@(Curry.Module.FlatCurry.C_TCons x3 x4) st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.List.c_nub))(Curry.Module.Prelude.c_concat(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_extractTVars))(x4)(st))(st))(st)
c_extractTVars x1@(Curry.Module.FlatCurry.C_FuncType x5 x6) st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.List.c_nub))(Curry.Module.Prelude.op_43_43(Curry.Module.FlatToAbstractCurry.c_extractTVars(x5)(st))(Curry.Module.FlatToAbstractCurry.c_extractTVars(x6)(st))(st))(st)
c_extractTVars (Curry.Module.FlatCurry.C_TypeExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_extractTVars(x)(st))(i)(xs)(st)
c_extractTVars x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.extractTVars")(x)



c_convertVisibility :: Curry.Module.FlatCurry.C_Visibility -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CVisibility
c_convertVisibility x1@Curry.Module.FlatCurry.C_Public st = Curry.Module.AbstractCurry.C_Public
c_convertVisibility x1@Curry.Module.FlatCurry.C_Private st = Curry.Module.AbstractCurry.C_Private
c_convertVisibility (Curry.Module.FlatCurry.C_VisibilityOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_convertVisibility(x)(st))(i)(xs)(st)
c_convertVisibility x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.convertVisibility")(x)



c_convertOperator :: Curry.Module.FlatCurry.C_OpDecl -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_COpDecl
c_convertOperator x1@(Curry.Module.FlatCurry.C_Op x2 x3 x4) st = Curry.Module.AbstractCurry.C_COp(x2)(Curry.Module.FlatToAbstractCurry.c_convertFixity(x3)(st))(x4)
c_convertOperator (Curry.Module.FlatCurry.C_OpDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_convertOperator(x)(st))(i)(xs)(st)
c_convertOperator x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.convertOperator")(x)



c_convertFixity :: Curry.Module.FlatCurry.C_Fixity -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CFixity
c_convertFixity x1@Curry.Module.FlatCurry.C_InfixOp st = Curry.Module.AbstractCurry.C_CInfixOp
c_convertFixity x1@Curry.Module.FlatCurry.C_InfixlOp st = Curry.Module.AbstractCurry.C_CInfixlOp
c_convertFixity x1@Curry.Module.FlatCurry.C_InfixrOp st = Curry.Module.AbstractCurry.C_CInfixrOp
c_convertFixity (Curry.Module.FlatCurry.C_FixityOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_convertFixity(x)(st))(i)(xs)(st)
c_convertFixity x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.convertFixity")(x)



c_convertTypeDecl :: Curry.Module.FlatCurry.C_TypeDecl -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CTypeDecl
c_convertTypeDecl x1@(Curry.Module.FlatCurry.C_Type x2 x3 x4 x5) st = Curry.Module.AbstractCurry.C_CType(x2)(Curry.Module.FlatToAbstractCurry.c_convertVisibility(x3)(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_convertVariable))(x4)(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_convertConsDecl))(x5)(st))
c_convertTypeDecl x1@(Curry.Module.FlatCurry.C_TypeSyn x6 x7 x8 x9) st = Curry.Module.AbstractCurry.C_CTypeSyn(x6)(Curry.Module.FlatToAbstractCurry.c_convertVisibility(x7)(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_convertVariable))(x8)(st))(Curry.Module.FlatToAbstractCurry.c_convertType(x9)(st))
c_convertTypeDecl (Curry.Module.FlatCurry.C_TypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_convertTypeDecl(x)(st))(i)(xs)(st)
c_convertTypeDecl x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.convertTypeDecl")(x)



c_convertType :: Curry.Module.FlatCurry.C_TypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CTypeExpr
c_convertType x1@(Curry.Module.FlatCurry.C_TVar x2) st = Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_tx(st))(x2)(st)
c_convertType x1@(Curry.Module.FlatCurry.C_FuncType x3 x4) st = Curry.Module.AbstractCurry.C_CFuncType(Curry.Module.FlatToAbstractCurry.c_convertType(x3)(st))(Curry.Module.FlatToAbstractCurry.c_convertType(x4)(st))
c_convertType x1@(Curry.Module.FlatCurry.C_TCons x5 x6) st = Curry.Module.AbstractCurry.C_CTCons(x5)(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_convertType))(x6)(st))
c_convertType (Curry.Module.FlatCurry.C_TypeExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_convertType(x)(st))(i)(xs)(st)
c_convertType x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.convertType")(x)



c_convertConsDecl :: Curry.Module.FlatCurry.C_ConsDecl -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CConsDecl
c_convertConsDecl x1@(Curry.Module.FlatCurry.C_Cons x2 x3 x4 x5) st = Curry.Module.AbstractCurry.C_CCons(x2)(x3)(Curry.Module.FlatToAbstractCurry.c_convertVisibility(x4)(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_convertType))(x5)(st))
c_convertConsDecl (Curry.Module.FlatCurry.C_ConsDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_convertConsDecl(x)(st))(i)(xs)(st)
c_convertConsDecl x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.convertConsDecl")(x)



c_convertLiteral :: Curry.Module.FlatCurry.C_Literal -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CLiteral
c_convertLiteral x1@(Curry.Module.FlatCurry.C_Intc x2) st = Curry.Module.AbstractCurry.C_CIntc(x2)
c_convertLiteral x1@(Curry.Module.FlatCurry.C_Floatc x3) st = Curry.Module.AbstractCurry.C_CFloatc(x3)
c_convertLiteral x1@(Curry.Module.FlatCurry.C_Charc x4) st = Curry.Module.AbstractCurry.C_CCharc(x4)
c_convertLiteral (Curry.Module.FlatCurry.C_LiteralOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_convertLiteral(x)(st))(i)(xs)(st)
c_convertLiteral x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.convertLiteral")(x)



c_convertPattern :: Curry.Module.FlatCurry.C_Pattern -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CPattern
c_convertPattern x1@(Curry.Module.FlatCurry.C_Pattern x2 x3) st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CPComb(x2)))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CPVar))(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_convertVariable))(st))(x3)(st))(st)
c_convertPattern x1@(Curry.Module.FlatCurry.C_LPattern x4) st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CPLit))(Curry.Module.FlatToAbstractCurry.c_convertLiteral(x4)(st))(st)
c_convertPattern (Curry.Module.FlatCurry.C_PatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_convertPattern(x)(st))(i)(xs)(st)
c_convertPattern x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.convertPattern")(x)



c_csuccess :: Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_csuccess st = Curry.Module.FlatToAbstractCurry.c_presym((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))))(st)



c_noGuardRule :: (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CPattern) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CLocalDecl) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CRule)
c_noGuardRule x1 x2 st = Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CRule(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.FlatToAbstractCurry.c_csuccess(st))(x2))(Curry.Module.Prelude.List)))



c_simpleRule :: (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CPattern) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CRule
c_simpleRule x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_noGuardRule(x1)(x2)(st))(Curry.Module.Prelude.List)(st)



c_constantRule :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CRule)
c_constantRule st = Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_simpleRule(Curry.Module.Prelude.List))



c_rules :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CRule) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CRules)
c_rules st = Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CRules(Curry.Module.AbstractCurry.C_CFlex))



c_funcDecl :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CVisibility -> Curry.Module.AbstractCurry.C_CTypeExpr -> (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CRule) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractHaskell.C_HFuncDecl
c_funcDecl x1 x2 x3 x4@Curry.Module.Prelude.List st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))(st)
c_funcDecl x1 x2 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.FlatToAbstractCurry.c_funcDecl_case_4(x1)(x2)(x3)(x4)(x5)(x6)(Curry.Module.Prelude.c_not(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_all(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_61_61))(Curry.Module.FlatToAbstractCurry.c_funcDecl'46arity'4683(x5)(st))))(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_funcDecl'46arity'4683))(x6)(st))(st))(st))(st)
c_funcDecl x1 x2 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_funcDecl(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_funcDecl x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.funcDecl")(x)



c_funcDecl'46arity'4683 :: Curry.Module.AbstractCurry.C_CRule -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_funcDecl'46arity'4683 x1@(Curry.Module.AbstractCurry.C_CRule x2 x3 x4) st = Curry.Module.Prelude.c_length(x2)(st)
c_funcDecl'46arity'4683 (Curry.Module.AbstractCurry.C_CRuleOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_funcDecl'46arity'4683(x)(st))(i)(xs)(st)
c_funcDecl'46arity'4683 x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.funcDecl.arity.83")(x)



c_pubFunc :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CTypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CRule) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractHaskell.C_HFuncDecl))
c_pubFunc x1 st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.FlatToAbstractCurry.c_funcDecl(x1)(Curry.Module.AbstractCurry.C_Public))



c_untypedFunc :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CRule) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractHaskell.C_HFuncDecl)
c_untypedFunc x1 st = Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_pubFunc(x1)(st))(Curry.Module.FlatToAbstractCurry.c_untyped(st))(st)



c_constantFunc :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractHaskell.C_HFuncDecl
c_constantFunc x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_untypedFunc(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_constantRule(st))(x2)(st))(Curry.Module.Prelude.List))(st)



c_prelude :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_prelude st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))



c_prename :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_prename x1 st = Curry.Module.Prelude.T2(Curry.Module.FlatToAbstractCurry.c_prelude(st))(x1)



c_presym :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_presym x1 st = Curry.Module.AbstractCurry.C_CSymbol(Curry.Module.FlatToAbstractCurry.c_prename(x1)(st))



op_36_36 :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr))
op_36_36 st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pc))(Curry.Module.AbstractCurry.C_CApply)



op_36_36_36 :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CExpr) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr))
op_36_36_36 st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.c_foldl(Curry.Module.FlatToAbstractCurry.op_36_36(st)))



c_comb :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CExpr) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr)
c_comb x1 st = Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36_36(st))(Curry.Module.AbstractCurry.C_CSymbol(x1))(st)



op_58_33_58 :: Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
op_58_33_58 x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36_36(st))(Curry.Module.FlatToAbstractCurry.c_presym((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(st))(st))((Curry.Module.Prelude.:<)(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)))(st)



c_list :: (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CExpr) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_list x1@Curry.Module.Prelude.List st = Curry.Module.FlatToAbstractCurry.c_presym((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List)))(st)
c_list x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.FlatToAbstractCurry.op_58_33_58(x2)(Curry.Module.FlatToAbstractCurry.c_list(x3)(st))(st)
c_list (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_list(x)(st))(i)(xs)(st)
c_list x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.list")(x)



c_acyStr :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_acyStr x1@Curry.Module.Prelude.List st = Curry.Module.FlatToAbstractCurry.c_presym((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List)))(st)
c_acyStr x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36_36(st))(Curry.Module.FlatToAbstractCurry.c_presym((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CLit(Curry.Module.AbstractCurry.C_CCharc(x2)))((Curry.Module.Prelude.:<)(Curry.Module.FlatToAbstractCurry.c_acyStr(x3)(st))(Curry.Module.Prelude.List)))(st)
c_acyStr (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_acyStr(x)(st))(i)(xs)(st)
c_acyStr x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.acyStr")(x)



op_45_62_62 :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CPattern) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr))
op_45_62_62 st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pc))(Curry.Module.AbstractCurry.C_CLambda)



op_45_62_45 :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CPattern -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr))
op_45_62_45 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pc))(Curry.Module.AbstractCurry.C_CLambda))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pc))((Curry.Module.Prelude.:<)))(Curry.Module.Prelude.List)))(st)



c_higherOrder :: Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_higherOrder x1 x2 st = Curry.Module.FlatToAbstractCurry.c_higherOrder'46construct'46121(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_higherOrder'46wrap'46121(x1)))(Curry.Module.Prelude.c_enumFromTo(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x2)(st))(st))(st)



c_higherOrder'46construct'46121 :: (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CExpr) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_higherOrder'46construct'46121 x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.FlatToAbstractCurry.c_higherOrder'46construct'46121_case_2(x2)(x3)(st)
c_higherOrder'46construct'46121 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_higherOrder'46construct'46121(x)(st))(i)(xs)(st)
c_higherOrder'46construct'46121 x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.higherOrder.construct.121")(x)



c_higherOrder'46wrap'46121 :: Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_higherOrder'46wrap'46121 x1 x2 st = Curry.Module.FlatToAbstractCurry.c_higherOrder'46wrap'46121_case_1(x1)(x2)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st)



c_monadWrap :: Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_monadWrap st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36(st))(Curry.Module.FlatToAbstractCurry.c_point(st))(st))(Curry.Module.FlatToAbstractCurry.c_presym((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List)))))))(st))(st)



c_point :: Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_point st = Curry.Module.FlatToAbstractCurry.c_presym((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List))(st)



c_untyped :: Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CTypeExpr
c_untyped st = Curry.Module.AbstractCurry.C_CTCons(Curry.Module.FlatToAbstractCurry.c_prename((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))(Curry.Module.Prelude.List))))))))(st))(Curry.Module.Prelude.List)



c_constraint :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractHaskell.C_TypeClass
c_constraint x1 x2 st = Curry.Module.AbstractHaskell.C_TypeClass(x1)((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CTVar(x2))(Curry.Module.Prelude.List))



c_test1 :: Curry.RunTimeSystem.State -> Curry.Module.Assertion.C_Assertion Curry.Module.AbstractCurry.C_CRule
c_test1 st = Curry.Module.Assertion.C_AssertEqual((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))(Curry.Module.Prelude.List))))))))))))))))))))(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_noGuardRule(Curry.Module.Prelude.List)(Curry.Module.FlatToAbstractCurry.c_csuccess(st))(st))(Curry.Module.Prelude.List)(st))(Curry.Module.FlatToAbstractCurry.c_simpleRule(Curry.Module.Prelude.List)(Curry.Module.FlatToAbstractCurry.c_csuccess(st))(st))



c_test3 :: Curry.RunTimeSystem.State -> Curry.Module.Assertion.C_Assertion (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_test3 st = Curry.Module.Assertion.C_AssertEqual((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))(Curry.Module.Prelude.List)))))))))))))))))))(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_showExpr(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36_36(st))(Curry.Module.FlatToAbstractCurry.c_presym((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))(Curry.Module.Prelude.List))(st))(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CVar))(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_convertVariable))(st))(Curry.Module.Prelude.c_enumFromTo(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))(st))(st))(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('1'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('2'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('3'))(Curry.Module.Prelude.List)))))))))))



c_test4 :: Curry.RunTimeSystem.State -> Curry.Module.Assertion.C_Assertion (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_test4 st = Curry.Module.Assertion.C_AssertEqual((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))(Curry.Module.Prelude.List))))))))))))))(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_showExpr(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_45_62_45(st))(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_px(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_xx(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('1'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('1'))(Curry.Module.Prelude.List))))))))))



c_test5 :: Curry.RunTimeSystem.State -> Curry.Module.Assertion.C_Assertion (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_test5 st = Curry.Module.Assertion.C_AssertEqual((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('1'))(Curry.Module.Prelude.List)))))))))))))))(Curry.Module.Prelude.op_36(Curry.Module.AbstractCurryPrinter.c_showExpr(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36(st))(Curry.Module.FlatToAbstractCurry.c_higherOrder(Curry.Module.FlatToAbstractCurry.c_monadWrap(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_45_62_45(st))(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_px(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_xx(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('1'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('1'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List)))))))))))))))))))))))))



c_test6 :: Curry.RunTimeSystem.State -> Curry.Module.Assertion.C_Assertion (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_test6 st = Curry.Module.Assertion.C_AssertEqual((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('2'))(Curry.Module.Prelude.List)))))))))))))))(Curry.Module.Prelude.op_36(Curry.Module.AbstractCurryPrinter.c_showExpr(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36(st))(Curry.Module.FlatToAbstractCurry.c_higherOrder(Curry.Module.FlatToAbstractCurry.c_monadWrap(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_45_62_62(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_px(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_px(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(Curry.Module.Prelude.List)))(st))(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_xx(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('1'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('2'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('1'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))



c_test7 :: Curry.RunTimeSystem.State -> Curry.Module.Assertion.C_Assertion (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_test7 st = Curry.Module.Assertion.C_AssertEqual((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('3'))(Curry.Module.Prelude.List)))))))))))))))(Curry.Module.Prelude.op_36(Curry.Module.AbstractCurryPrinter.c_showExpr(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36(st))(Curry.Module.FlatToAbstractCurry.c_higherOrder(Curry.Module.FlatToAbstractCurry.c_monadWrap(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_45_62_62(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_px(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_px(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_px(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))(st))(Curry.Module.Prelude.List))))(st))(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_xx(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('1'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('2'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('3'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('1'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))



c_higherOrder'46wrap'46121_case_1 x1 x2 x3@Curry.Module.Prelude.C_True st = x1
c_higherOrder'46wrap'46121_case_1 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.FlatToAbstractCurry.c_higherOrder'46wrap'46121_case_0(x1)(x2)(Curry.Module.Prelude.c_otherwise(st))(st)
c_higherOrder'46wrap'46121_case_1 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_higherOrder'46wrap'46121_case_1(x1)(x2)(x)(st))(i)(xs)(st)
c_higherOrder'46wrap'46121_case_1 x1 x2 x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.higherOrder.wrap.121_case_1")(x)



c_higherOrder'46wrap'46121_case_0 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36(st))(Curry.Module.FlatToAbstractCurry.c_point(st))(st))(Curry.Module.FlatToAbstractCurry.c_higherOrder'46wrap'46121(x1)(Curry.Module.Prelude.op_45(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st)
c_higherOrder'46wrap'46121_case_0 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_higherOrder'46wrap'46121_case_0(x1)(x2)(x)(st))(i)(xs)(st)
c_higherOrder'46wrap'46121_case_0 x1 x2 x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.higherOrder.wrap.121_case_0")(x)



c_higherOrder'46construct'46121_case_2 x2 x3@Curry.Module.Prelude.List st = x2
c_higherOrder'46construct'46121_case_2 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36_36(st))(Curry.Module.FlatToAbstractCurry.c_point(st))(st))((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(Curry.Module.FlatToAbstractCurry.c_higherOrder'46construct'46121(x3)(st))(Curry.Module.Prelude.List)))(st)
c_higherOrder'46construct'46121_case_2 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_higherOrder'46construct'46121_case_2(x2)(x)(st))(i)(xs)(st)
c_higherOrder'46construct'46121_case_2 x2 x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.higherOrder.construct.121_case_2")(x)



c_funcDecl_case_4 x1 x2 x3 x4 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))(st)
c_funcDecl_case_4 x1 x2 x3 x4 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.FlatToAbstractCurry.c_funcDecl_case_3(x1)(x2)(x3)(x4)(x5)(Curry.Module.Prelude.c_otherwise(st))(st)
c_funcDecl_case_4 x1 x2 x3 x4 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_funcDecl_case_4(x1)(x2)(x3)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_funcDecl_case_4 x1 x2 x3 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.funcDecl_case_4")(x)



c_funcDecl_case_3 x1 x2 x3 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.AbstractHaskell.C_HFunc(x1)(Curry.Module.FlatToAbstractCurry.c_funcDecl'46arity'4683(x5)(st))(x2)(Curry.Module.Prelude.List)(x3)(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_rules(st))(x4)(st))
c_funcDecl_case_3 x1 x2 x3 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_funcDecl_case_3(x1)(x2)(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c_funcDecl_case_3 x1 x2 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.funcDecl_case_3")(x)



c_freshVariable'46maximum'4615_case_5 x2 x3@Curry.Module.Prelude.List st = x2
c_freshVariable'46maximum'4615_case_5 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.Prelude.c_max(x2)(Curry.Module.FlatToAbstractCurry.c_freshVariable'46maximum'4615(x3)(st))(st)
c_freshVariable'46maximum'4615_case_5 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_freshVariable'46maximum'4615_case_5(x2)(x)(st))(i)(xs)(st)
c_freshVariable'46maximum'4615_case_5 x2 x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.freshVariable.maximum.15_case_5")(x)



c_convertTypeVariable_case_8 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.c_show(Curry.Module.Prelude.c_negate(x1)(st))(st)))
c_convertTypeVariable_case_8 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.FlatToAbstractCurry.c_convertTypeVariable_case_7(x1)(Curry.Module.Prelude.op_60_61(x1)(Curry.Module.Prelude.op_45(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('z'))(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('a'))(st))(st))(st))(st)
c_convertTypeVariable_case_8 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_convertTypeVariable_case_8(x1)(x)(st))(i)(xs)(st)
c_convertTypeVariable_case_8 x1 x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.convertTypeVariable_case_8")(x)



c_convertTypeVariable_case_7 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_chr(Curry.Module.Prelude.op_43(x1)(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('a'))(st))(st))(st))(Curry.Module.Prelude.List))
c_convertTypeVariable_case_7 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.FlatToAbstractCurry.c_convertTypeVariable_case_6(x1)(Curry.Module.Prelude.c_otherwise(st))(st)
c_convertTypeVariable_case_7 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_convertTypeVariable_case_7(x1)(x)(st))(i)(xs)(st)
c_convertTypeVariable_case_7 x1 x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.convertTypeVariable_case_7")(x)



c_convertTypeVariable_case_6 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))(Curry.Module.Prelude.c_show(x1)(st)))
c_convertTypeVariable_case_6 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_convertTypeVariable_case_6(x1)(x)(st))(i)(xs)(st)
c_convertTypeVariable_case_6 x1 x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.convertTypeVariable_case_6")(x)



c_convertVariable_case_10 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.c_show(Curry.Module.Prelude.c_negate(x1)(st))(st)))
c_convertVariable_case_10 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.FlatToAbstractCurry.c_convertVariable_case_9(x1)(Curry.Module.Prelude.c_otherwise(st))(st)
c_convertVariable_case_10 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_convertVariable_case_10(x1)(x)(st))(i)(xs)(st)
c_convertVariable_case_10 x1 x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.convertVariable_case_10")(x)



c_convertVariable_case_9 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))(Curry.Module.Prelude.c_show(x1)(st)))
c_convertVariable_case_9 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatToAbstractCurry.c_convertVariable_case_9(x1)(x)(st))(i)(xs)(st)
c_convertVariable_case_9 x1 x st = Curry.RunTimeSystem.patternFail("FlatToAbstractCurry.convertVariable_case_9")(x)


