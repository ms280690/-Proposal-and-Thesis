{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.AbstractCurry (module Curry.Module.AbstractCurry) where

import Curry.RunTimeSystem
import Curry.Module.Directory
import Curry.Module.Distribution
import Curry.Module.FileGoodies
import Curry.Module.Prelude
import Curry.Module.ReadShowTerm



-- begin included



-- end included

type C_QName = Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)

type C_CTVarIName = Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)

type C_CVarIName = Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)

data C_CurryProg = C_CurryProg (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeDecl) (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CFuncDecl) (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_COpDecl)
  | C_CurryProgFail Curry.RunTimeSystem.C_Exceptions
  | C_CurryProgOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractCurry.C_CurryProg)

data C_CVisibility = C_Public
  | C_Private
  | C_CVisibilityFail Curry.RunTimeSystem.C_Exceptions
  | C_CVisibilityOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractCurry.C_CVisibility)

data C_CTypeDecl = C_CType (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.AbstractCurry.C_CVisibility (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CConsDecl)
  | C_CTypeSyn (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.AbstractCurry.C_CVisibility (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) Curry.Module.AbstractCurry.C_CTypeExpr
  | C_CTypeDeclFail Curry.RunTimeSystem.C_Exceptions
  | C_CTypeDeclOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractCurry.C_CTypeDecl)

data C_CConsDecl = C_CCons (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int Curry.Module.AbstractCurry.C_CVisibility (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeExpr)
  | C_CConsDeclFail Curry.RunTimeSystem.C_Exceptions
  | C_CConsDeclOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractCurry.C_CConsDecl)

data C_CTypeExpr = C_CTVar (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
  | C_CFuncType Curry.Module.AbstractCurry.C_CTypeExpr Curry.Module.AbstractCurry.C_CTypeExpr
  | C_CTCons (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeExpr)
  | C_CTypeExprFail Curry.RunTimeSystem.C_Exceptions
  | C_CTypeExprOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractCurry.C_CTypeExpr)

data C_COpDecl = C_COp (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.AbstractCurry.C_CFixity Curry.Module.Prelude.C_Int
  | C_COpDeclFail Curry.RunTimeSystem.C_Exceptions
  | C_COpDeclOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractCurry.C_COpDecl)

data C_CFixity = C_CInfixOp
  | C_CInfixlOp
  | C_CInfixrOp
  | C_CFixityFail Curry.RunTimeSystem.C_Exceptions
  | C_CFixityOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractCurry.C_CFixity)

data C_CFuncDecl = C_CFunc (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int Curry.Module.AbstractCurry.C_CVisibility Curry.Module.AbstractCurry.C_CTypeExpr Curry.Module.AbstractCurry.C_CRules
  | C_CmtFunc (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int Curry.Module.AbstractCurry.C_CVisibility Curry.Module.AbstractCurry.C_CTypeExpr Curry.Module.AbstractCurry.C_CRules
  | C_CFuncDeclFail Curry.RunTimeSystem.C_Exceptions
  | C_CFuncDeclOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractCurry.C_CFuncDecl)

data C_CRules = C_CRules Curry.Module.AbstractCurry.C_CEvalAnnot (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CRule)
  | C_CExternal (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_CRulesFail Curry.RunTimeSystem.C_Exceptions
  | C_CRulesOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractCurry.C_CRules)

data C_CEvalAnnot = C_CFlex
  | C_CRigid
  | C_CChoice
  | C_CEvalAnnotFail Curry.RunTimeSystem.C_Exceptions
  | C_CEvalAnnotOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractCurry.C_CEvalAnnot)

data C_CRule = C_CRule (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CPattern) (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.AbstractCurry.C_CExpr Curry.Module.AbstractCurry.C_CExpr)) (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CLocalDecl)
  | C_CRuleFail Curry.RunTimeSystem.C_Exceptions
  | C_CRuleOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractCurry.C_CRule)

data C_CLocalDecl = C_CLocalFunc Curry.Module.AbstractCurry.C_CFuncDecl
  | C_CLocalPat Curry.Module.AbstractCurry.C_CPattern Curry.Module.AbstractCurry.C_CExpr (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CLocalDecl)
  | C_CLocalVar (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
  | C_CLocalDeclFail Curry.RunTimeSystem.C_Exceptions
  | C_CLocalDeclOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractCurry.C_CLocalDecl)

data C_CExpr = C_CVar (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
  | C_CLit Curry.Module.AbstractCurry.C_CLiteral
  | C_CSymbol (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
  | C_CApply Curry.Module.AbstractCurry.C_CExpr Curry.Module.AbstractCurry.C_CExpr
  | C_CLambda (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CPattern) Curry.Module.AbstractCurry.C_CExpr
  | C_CLetDecl (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CLocalDecl) Curry.Module.AbstractCurry.C_CExpr
  | C_CDoExpr (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CStatement)
  | C_CListComp Curry.Module.AbstractCurry.C_CExpr (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CStatement)
  | C_CCase Curry.Module.AbstractCurry.C_CExpr (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CBranchExpr)
  | C_CExprFail Curry.RunTimeSystem.C_Exceptions
  | C_CExprOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractCurry.C_CExpr)

data C_CStatement = C_CSExpr Curry.Module.AbstractCurry.C_CExpr
  | C_CSPat Curry.Module.AbstractCurry.C_CPattern Curry.Module.AbstractCurry.C_CExpr
  | C_CSLet (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CLocalDecl)
  | C_CStatementFail Curry.RunTimeSystem.C_Exceptions
  | C_CStatementOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractCurry.C_CStatement)

data C_CPattern = C_CPVar (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
  | C_CPLit Curry.Module.AbstractCurry.C_CLiteral
  | C_CPComb (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CPattern)
  | C_CPAs (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.AbstractCurry.C_CPattern
  | C_CPFuncComb (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CPattern)
  | C_CPatternFail Curry.RunTimeSystem.C_Exceptions
  | C_CPatternOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractCurry.C_CPattern)

data C_CBranchExpr = C_CBranch Curry.Module.AbstractCurry.C_CPattern Curry.Module.AbstractCurry.C_CExpr
  | C_CBranchExprFail Curry.RunTimeSystem.C_Exceptions
  | C_CBranchExprOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractCurry.C_CBranchExpr)

data C_CLiteral = C_CIntc Curry.Module.Prelude.C_Int
  | C_CFloatc Curry.Module.Prelude.C_Float
  | C_CCharc Curry.Module.Prelude.C_Char
  | C_CLiteralFail Curry.RunTimeSystem.C_Exceptions
  | C_CLiteralOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractCurry.C_CLiteral)

instance BaseCurry Curry.Module.AbstractCurry.C_CurryProg where
  nf f (Curry.Module.AbstractCurry.C_CurryProg x1 x2 x3 x4 x5) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> f(Curry.Module.AbstractCurry.C_CurryProg(v1)(v2)(v3)(v4)(v5))(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.AbstractCurry.C_CurryProg x1 x2 x3 x4 x5) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> f(Curry.Module.AbstractCurry.C_CurryProg(v1)(v2)(v3)(v4)(v5))(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractCurry.C_CurryProgOr(Curry.RunTimeSystem.mkRef(r)(5)(i))([Curry.Module.AbstractCurry.C_CurryProg(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(5)

  failed  = Curry.Module.AbstractCurry.C_CurryProgFail

  branching  = Curry.Module.AbstractCurry.C_CurryProgOr

  consKind (Curry.Module.AbstractCurry.C_CurryProgOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractCurry.C_CurryProgFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractCurry.C_CurryProgFail x) = x

  orRef (Curry.Module.AbstractCurry.C_CurryProgOr x _) = x

  branches (Curry.Module.AbstractCurry.C_CurryProgOr _ x) = x





instance BaseCurry Curry.Module.AbstractCurry.C_CVisibility where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractCurry.C_CVisibilityOr(Curry.RunTimeSystem.mkRef(r)(0)(i))([Curry.Module.AbstractCurry.C_Public,Curry.Module.AbstractCurry.C_Private]))(0)

  failed  = Curry.Module.AbstractCurry.C_CVisibilityFail

  branching  = Curry.Module.AbstractCurry.C_CVisibilityOr

  consKind (Curry.Module.AbstractCurry.C_CVisibilityOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractCurry.C_CVisibilityFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractCurry.C_CVisibilityFail x) = x

  orRef (Curry.Module.AbstractCurry.C_CVisibilityOr x _) = x

  branches (Curry.Module.AbstractCurry.C_CVisibilityOr _ x) = x





instance BaseCurry Curry.Module.AbstractCurry.C_CTypeDecl where
  nf f (Curry.Module.AbstractCurry.C_CType x1 x2 x3 x4) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> f(Curry.Module.AbstractCurry.C_CType(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CTypeSyn x1 x2 x3 x4) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> f(Curry.Module.AbstractCurry.C_CTypeSyn(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.AbstractCurry.C_CType x1 x2 x3 x4) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> f(Curry.Module.AbstractCurry.C_CType(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CTypeSyn x1 x2 x3 x4) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> f(Curry.Module.AbstractCurry.C_CTypeSyn(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractCurry.C_CTypeDeclOr(Curry.RunTimeSystem.mkRef(r)(4)(i))([Curry.Module.AbstractCurry.C_CType(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CTypeSyn(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(4)

  failed  = Curry.Module.AbstractCurry.C_CTypeDeclFail

  branching  = Curry.Module.AbstractCurry.C_CTypeDeclOr

  consKind (Curry.Module.AbstractCurry.C_CTypeDeclOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractCurry.C_CTypeDeclFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractCurry.C_CTypeDeclFail x) = x

  orRef (Curry.Module.AbstractCurry.C_CTypeDeclOr x _) = x

  branches (Curry.Module.AbstractCurry.C_CTypeDeclOr _ x) = x





instance BaseCurry Curry.Module.AbstractCurry.C_CConsDecl where
  nf f (Curry.Module.AbstractCurry.C_CCons x1 x2 x3 x4) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> f(Curry.Module.AbstractCurry.C_CCons(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.AbstractCurry.C_CCons x1 x2 x3 x4) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> f(Curry.Module.AbstractCurry.C_CCons(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractCurry.C_CConsDeclOr(Curry.RunTimeSystem.mkRef(r)(4)(i))([Curry.Module.AbstractCurry.C_CCons(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(4)

  failed  = Curry.Module.AbstractCurry.C_CConsDeclFail

  branching  = Curry.Module.AbstractCurry.C_CConsDeclOr

  consKind (Curry.Module.AbstractCurry.C_CConsDeclOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractCurry.C_CConsDeclFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractCurry.C_CConsDeclFail x) = x

  orRef (Curry.Module.AbstractCurry.C_CConsDeclOr x _) = x

  branches (Curry.Module.AbstractCurry.C_CConsDeclOr _ x) = x





instance BaseCurry Curry.Module.AbstractCurry.C_CTypeExpr where
  nf f (Curry.Module.AbstractCurry.C_CTVar x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CTVar(v1))(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CFuncType x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CFuncType(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CTCons x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CTCons(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.AbstractCurry.C_CTVar x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CTVar(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CFuncType x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CFuncType(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CTCons x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CTCons(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractCurry.C_CTypeExprOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.AbstractCurry.C_CTVar(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CFuncType(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CTCons(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.AbstractCurry.C_CTypeExprFail

  branching  = Curry.Module.AbstractCurry.C_CTypeExprOr

  consKind (Curry.Module.AbstractCurry.C_CTypeExprOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractCurry.C_CTypeExprFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractCurry.C_CTypeExprFail x) = x

  orRef (Curry.Module.AbstractCurry.C_CTypeExprOr x _) = x

  branches (Curry.Module.AbstractCurry.C_CTypeExprOr _ x) = x





instance BaseCurry Curry.Module.AbstractCurry.C_COpDecl where
  nf f (Curry.Module.AbstractCurry.C_COp x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.AbstractCurry.C_COp(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.AbstractCurry.C_COp x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.AbstractCurry.C_COp(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractCurry.C_COpDeclOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.AbstractCurry.C_COp(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(3)

  failed  = Curry.Module.AbstractCurry.C_COpDeclFail

  branching  = Curry.Module.AbstractCurry.C_COpDeclOr

  consKind (Curry.Module.AbstractCurry.C_COpDeclOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractCurry.C_COpDeclFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractCurry.C_COpDeclFail x) = x

  orRef (Curry.Module.AbstractCurry.C_COpDeclOr x _) = x

  branches (Curry.Module.AbstractCurry.C_COpDeclOr _ x) = x





instance BaseCurry Curry.Module.AbstractCurry.C_CFixity where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractCurry.C_CFixityOr(Curry.RunTimeSystem.mkRef(r)(0)(i))([Curry.Module.AbstractCurry.C_CInfixOp,Curry.Module.AbstractCurry.C_CInfixlOp,Curry.Module.AbstractCurry.C_CInfixrOp]))(0)

  failed  = Curry.Module.AbstractCurry.C_CFixityFail

  branching  = Curry.Module.AbstractCurry.C_CFixityOr

  consKind (Curry.Module.AbstractCurry.C_CFixityOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractCurry.C_CFixityFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractCurry.C_CFixityFail x) = x

  orRef (Curry.Module.AbstractCurry.C_CFixityOr x _) = x

  branches (Curry.Module.AbstractCurry.C_CFixityOr _ x) = x





instance BaseCurry Curry.Module.AbstractCurry.C_CFuncDecl where
  nf f (Curry.Module.AbstractCurry.C_CFunc x1 x2 x3 x4 x5) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> f(Curry.Module.AbstractCurry.C_CFunc(v1)(v2)(v3)(v4)(v5))(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CmtFunc x1 x2 x3 x4 x5 x6) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> Curry.RunTimeSystem.nfCTC(\ v6 state6 -> f(Curry.Module.AbstractCurry.C_CmtFunc(v1)(v2)(v3)(v4)(v5)(v6))(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.AbstractCurry.C_CFunc x1 x2 x3 x4 x5) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> f(Curry.Module.AbstractCurry.C_CFunc(v1)(v2)(v3)(v4)(v5))(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CmtFunc x1 x2 x3 x4 x5 x6) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> Curry.RunTimeSystem.gnfCTC(\ v6 state6 -> f(Curry.Module.AbstractCurry.C_CmtFunc(v1)(v2)(v3)(v4)(v5)(v6))(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractCurry.C_CFuncDeclOr(Curry.RunTimeSystem.mkRef(r)(6)(i))([Curry.Module.AbstractCurry.C_CFunc(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CmtFunc(Curry.RunTimeSystem.generator((Prelude.+)(r)((5::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(6)

  failed  = Curry.Module.AbstractCurry.C_CFuncDeclFail

  branching  = Curry.Module.AbstractCurry.C_CFuncDeclOr

  consKind (Curry.Module.AbstractCurry.C_CFuncDeclOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractCurry.C_CFuncDeclFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractCurry.C_CFuncDeclFail x) = x

  orRef (Curry.Module.AbstractCurry.C_CFuncDeclOr x _) = x

  branches (Curry.Module.AbstractCurry.C_CFuncDeclOr _ x) = x





instance BaseCurry Curry.Module.AbstractCurry.C_CRules where
  nf f (Curry.Module.AbstractCurry.C_CRules x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CRules(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CExternal x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CExternal(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.AbstractCurry.C_CRules x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CRules(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CExternal x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CExternal(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractCurry.C_CRulesOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.AbstractCurry.C_CRules(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CExternal(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.AbstractCurry.C_CRulesFail

  branching  = Curry.Module.AbstractCurry.C_CRulesOr

  consKind (Curry.Module.AbstractCurry.C_CRulesOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractCurry.C_CRulesFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractCurry.C_CRulesFail x) = x

  orRef (Curry.Module.AbstractCurry.C_CRulesOr x _) = x

  branches (Curry.Module.AbstractCurry.C_CRulesOr _ x) = x





instance BaseCurry Curry.Module.AbstractCurry.C_CEvalAnnot where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractCurry.C_CEvalAnnotOr(Curry.RunTimeSystem.mkRef(r)(0)(i))([Curry.Module.AbstractCurry.C_CFlex,Curry.Module.AbstractCurry.C_CRigid,Curry.Module.AbstractCurry.C_CChoice]))(0)

  failed  = Curry.Module.AbstractCurry.C_CEvalAnnotFail

  branching  = Curry.Module.AbstractCurry.C_CEvalAnnotOr

  consKind (Curry.Module.AbstractCurry.C_CEvalAnnotOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractCurry.C_CEvalAnnotFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractCurry.C_CEvalAnnotFail x) = x

  orRef (Curry.Module.AbstractCurry.C_CEvalAnnotOr x _) = x

  branches (Curry.Module.AbstractCurry.C_CEvalAnnotOr _ x) = x





instance BaseCurry Curry.Module.AbstractCurry.C_CRule where
  nf f (Curry.Module.AbstractCurry.C_CRule x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.AbstractCurry.C_CRule(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.AbstractCurry.C_CRule x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.AbstractCurry.C_CRule(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractCurry.C_CRuleOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.AbstractCurry.C_CRule(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(3)

  failed  = Curry.Module.AbstractCurry.C_CRuleFail

  branching  = Curry.Module.AbstractCurry.C_CRuleOr

  consKind (Curry.Module.AbstractCurry.C_CRuleOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractCurry.C_CRuleFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractCurry.C_CRuleFail x) = x

  orRef (Curry.Module.AbstractCurry.C_CRuleOr x _) = x

  branches (Curry.Module.AbstractCurry.C_CRuleOr _ x) = x





instance BaseCurry Curry.Module.AbstractCurry.C_CLocalDecl where
  nf f (Curry.Module.AbstractCurry.C_CLocalFunc x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CLocalFunc(v1))(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CLocalPat x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.AbstractCurry.C_CLocalPat(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CLocalVar x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CLocalVar(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.AbstractCurry.C_CLocalFunc x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CLocalFunc(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CLocalPat x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.AbstractCurry.C_CLocalPat(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CLocalVar x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CLocalVar(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractCurry.C_CLocalDeclOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.AbstractCurry.C_CLocalFunc(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CLocalPat(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CLocalVar(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(3)

  failed  = Curry.Module.AbstractCurry.C_CLocalDeclFail

  branching  = Curry.Module.AbstractCurry.C_CLocalDeclOr

  consKind (Curry.Module.AbstractCurry.C_CLocalDeclOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractCurry.C_CLocalDeclFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractCurry.C_CLocalDeclFail x) = x

  orRef (Curry.Module.AbstractCurry.C_CLocalDeclOr x _) = x

  branches (Curry.Module.AbstractCurry.C_CLocalDeclOr _ x) = x





instance BaseCurry Curry.Module.AbstractCurry.C_CExpr where
  nf f (Curry.Module.AbstractCurry.C_CVar x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CVar(v1))(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CLit x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CLit(v1))(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CSymbol x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CSymbol(v1))(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CApply x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CApply(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CLambda x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CLambda(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CLetDecl x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CLetDecl(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CDoExpr x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CDoExpr(v1))(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CListComp x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CListComp(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CCase x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CCase(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.AbstractCurry.C_CVar x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CVar(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CLit x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CLit(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CSymbol x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CSymbol(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CApply x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CApply(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CLambda x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CLambda(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CLetDecl x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CLetDecl(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CDoExpr x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CDoExpr(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CListComp x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CListComp(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CCase x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CCase(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractCurry.C_CExprOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.AbstractCurry.C_CVar(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CLit(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CSymbol(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CApply(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CLambda(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CLetDecl(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CDoExpr(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CListComp(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CCase(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.AbstractCurry.C_CExprFail

  branching  = Curry.Module.AbstractCurry.C_CExprOr

  consKind (Curry.Module.AbstractCurry.C_CExprOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractCurry.C_CExprFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractCurry.C_CExprFail x) = x

  orRef (Curry.Module.AbstractCurry.C_CExprOr x _) = x

  branches (Curry.Module.AbstractCurry.C_CExprOr _ x) = x





instance BaseCurry Curry.Module.AbstractCurry.C_CStatement where
  nf f (Curry.Module.AbstractCurry.C_CSExpr x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CSExpr(v1))(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CSPat x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CSPat(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CSLet x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CSLet(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.AbstractCurry.C_CSExpr x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CSExpr(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CSPat x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CSPat(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CSLet x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CSLet(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractCurry.C_CStatementOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.AbstractCurry.C_CSExpr(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CSPat(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CSLet(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.AbstractCurry.C_CStatementFail

  branching  = Curry.Module.AbstractCurry.C_CStatementOr

  consKind (Curry.Module.AbstractCurry.C_CStatementOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractCurry.C_CStatementFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractCurry.C_CStatementFail x) = x

  orRef (Curry.Module.AbstractCurry.C_CStatementOr x _) = x

  branches (Curry.Module.AbstractCurry.C_CStatementOr _ x) = x





instance BaseCurry Curry.Module.AbstractCurry.C_CPattern where
  nf f (Curry.Module.AbstractCurry.C_CPVar x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CPVar(v1))(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CPLit x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CPLit(v1))(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CPComb x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CPComb(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CPAs x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CPAs(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CPFuncComb x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CPFuncComb(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.AbstractCurry.C_CPVar x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CPVar(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CPLit x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CPLit(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CPComb x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CPComb(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CPAs x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CPAs(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CPFuncComb x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CPFuncComb(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractCurry.C_CPatternOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.AbstractCurry.C_CPVar(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CPLit(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CPComb(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CPAs(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CPFuncComb(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.AbstractCurry.C_CPatternFail

  branching  = Curry.Module.AbstractCurry.C_CPatternOr

  consKind (Curry.Module.AbstractCurry.C_CPatternOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractCurry.C_CPatternFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractCurry.C_CPatternFail x) = x

  orRef (Curry.Module.AbstractCurry.C_CPatternOr x _) = x

  branches (Curry.Module.AbstractCurry.C_CPatternOr _ x) = x





instance BaseCurry Curry.Module.AbstractCurry.C_CBranchExpr where
  nf f (Curry.Module.AbstractCurry.C_CBranch x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CBranch(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.AbstractCurry.C_CBranch x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.AbstractCurry.C_CBranch(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractCurry.C_CBranchExprOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.AbstractCurry.C_CBranch(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.AbstractCurry.C_CBranchExprFail

  branching  = Curry.Module.AbstractCurry.C_CBranchExprOr

  consKind (Curry.Module.AbstractCurry.C_CBranchExprOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractCurry.C_CBranchExprFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractCurry.C_CBranchExprFail x) = x

  orRef (Curry.Module.AbstractCurry.C_CBranchExprOr x _) = x

  branches (Curry.Module.AbstractCurry.C_CBranchExprOr _ x) = x





instance BaseCurry Curry.Module.AbstractCurry.C_CLiteral where
  nf f (Curry.Module.AbstractCurry.C_CIntc x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CIntc(v1))(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CFloatc x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CFloatc(v1))(state1))(x1)(state0)
  nf f (Curry.Module.AbstractCurry.C_CCharc x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CCharc(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.AbstractCurry.C_CIntc x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CIntc(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CFloatc x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CFloatc(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.AbstractCurry.C_CCharc x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.AbstractCurry.C_CCharc(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractCurry.C_CLiteralOr(Curry.RunTimeSystem.mkRef(r)(1)(i))([Curry.Module.AbstractCurry.C_CIntc(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CFloatc(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.AbstractCurry.C_CCharc(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(1)

  failed  = Curry.Module.AbstractCurry.C_CLiteralFail

  branching  = Curry.Module.AbstractCurry.C_CLiteralOr

  consKind (Curry.Module.AbstractCurry.C_CLiteralOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractCurry.C_CLiteralFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractCurry.C_CLiteralFail x) = x

  orRef (Curry.Module.AbstractCurry.C_CLiteralOr x _) = x

  branches (Curry.Module.AbstractCurry.C_CLiteralOr _ x) = x





instance Curry Curry.Module.AbstractCurry.C_CurryProg where
  strEq (Curry.Module.AbstractCurry.C_CurryProg x1 x2 x3 x4 x5) (Curry.Module.AbstractCurry.C_CurryProg y1 y2 y3 y4 y5) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.AbstractCurry.C_CurryProg x1 x2 x3 x4 x5) (Curry.Module.AbstractCurry.C_CurryProg y1 y2 y3 y4 y5) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.genEq(x5)(y5)(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.AbstractCurry.C_CurryProg x1 x2 x3 x4 x5) st = Curry.Module.AbstractCurry.C_CurryProg(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))

  foldCurry f c (Curry.Module.AbstractCurry.C_CurryProg x1 x2 x3 x4 x5) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(c)(st))(st))(st))(st))(st)

  typeName _ = "CurryProg"

  showQ d (Curry.Module.AbstractCurry.C_CurryProg x1 x2 x3 x4 x5) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CurryProg "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x5))))))))))


  showQ _ (Curry.Module.AbstractCurry.C_CurryProgOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractCurry.C_CVisibility where
  strEq Curry.Module.AbstractCurry.C_Public Curry.Module.AbstractCurry.C_Public st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.AbstractCurry.C_Private Curry.Module.AbstractCurry.C_Private st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.AbstractCurry.C_Public Curry.Module.AbstractCurry.C_Public st = Curry.Module.Prelude.C_True
  eq Curry.Module.AbstractCurry.C_Private Curry.Module.AbstractCurry.C_Private st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.AbstractCurry.C_Public st = Curry.Module.AbstractCurry.C_Public
  propagate f Curry.Module.AbstractCurry.C_Private st = Curry.Module.AbstractCurry.C_Private

  foldCurry f c Curry.Module.AbstractCurry.C_Public st = c
  foldCurry f c Curry.Module.AbstractCurry.C_Private st = c

  typeName _ = "CVisibility"

  showQ _ Curry.Module.AbstractCurry.C_Public = Prelude.showString("AbstractCurry.Public")
  showQ _ Curry.Module.AbstractCurry.C_Private = Prelude.showString("AbstractCurry.Private")
  showQ _ (Curry.Module.AbstractCurry.C_CVisibilityOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractCurry.C_CTypeDecl where
  strEq (Curry.Module.AbstractCurry.C_CType x1 x2 x3 x4) (Curry.Module.AbstractCurry.C_CType y1 y2 y3 y4) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(st))(st))(st)
  strEq (Curry.Module.AbstractCurry.C_CTypeSyn x1 x2 x3 x4) (Curry.Module.AbstractCurry.C_CTypeSyn y1 y2 y3 y4) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.AbstractCurry.C_CType x1 x2 x3 x4) (Curry.Module.AbstractCurry.C_CType y1 y2 y3 y4) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.genEq(x4)(y4)(st))(st))(st))(st)
  eq (Curry.Module.AbstractCurry.C_CTypeSyn x1 x2 x3 x4) (Curry.Module.AbstractCurry.C_CTypeSyn y1 y2 y3 y4) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.genEq(x4)(y4)(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.AbstractCurry.C_CType x1 x2 x3 x4) st = Curry.Module.AbstractCurry.C_CType(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))
  propagate f (Curry.Module.AbstractCurry.C_CTypeSyn x1 x2 x3 x4) st = Curry.Module.AbstractCurry.C_CTypeSyn(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))

  foldCurry f c (Curry.Module.AbstractCurry.C_CType x1 x2 x3 x4) st = f(x1)(f(x2)(f(x3)(f(x4)(c)(st))(st))(st))(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CTypeSyn x1 x2 x3 x4) st = f(x1)(f(x2)(f(x3)(f(x4)(c)(st))(st))(st))(st)

  typeName _ = "CTypeDecl"

  showQ d (Curry.Module.AbstractCurry.C_CType x1 x2 x3 x4) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CType "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))))))))


  showQ d (Curry.Module.AbstractCurry.C_CTypeSyn x1 x2 x3 x4) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CTypeSyn "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))))))))


  showQ _ (Curry.Module.AbstractCurry.C_CTypeDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractCurry.C_CConsDecl where
  strEq (Curry.Module.AbstractCurry.C_CCons x1 x2 x3 x4) (Curry.Module.AbstractCurry.C_CCons y1 y2 y3 y4) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.AbstractCurry.C_CCons x1 x2 x3 x4) (Curry.Module.AbstractCurry.C_CCons y1 y2 y3 y4) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.genEq(x4)(y4)(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.AbstractCurry.C_CCons x1 x2 x3 x4) st = Curry.Module.AbstractCurry.C_CCons(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))

  foldCurry f c (Curry.Module.AbstractCurry.C_CCons x1 x2 x3 x4) st = f(x1)(f(x2)(f(x3)(f(x4)(c)(st))(st))(st))(st)

  typeName _ = "CConsDecl"

  showQ d (Curry.Module.AbstractCurry.C_CCons x1 x2 x3 x4) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CCons "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))))))))


  showQ _ (Curry.Module.AbstractCurry.C_CConsDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractCurry.C_CTypeExpr where
  strEq (Curry.Module.AbstractCurry.C_CTVar x1) (Curry.Module.AbstractCurry.C_CTVar y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.AbstractCurry.C_CFuncType x1 x2) (Curry.Module.AbstractCurry.C_CFuncType y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.AbstractCurry.C_CTCons x1 x2) (Curry.Module.AbstractCurry.C_CTCons y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.AbstractCurry.C_CTVar x1) (Curry.Module.AbstractCurry.C_CTVar y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.AbstractCurry.C_CFuncType x1 x2) (Curry.Module.AbstractCurry.C_CFuncType y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.AbstractCurry.C_CTCons x1 x2) (Curry.Module.AbstractCurry.C_CTCons y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.AbstractCurry.C_CTVar x1) st = Curry.Module.AbstractCurry.C_CTVar(f((0::Int))(x1)(st))
  propagate f (Curry.Module.AbstractCurry.C_CFuncType x1 x2) st = Curry.Module.AbstractCurry.C_CFuncType(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.AbstractCurry.C_CTCons x1 x2) st = Curry.Module.AbstractCurry.C_CTCons(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.AbstractCurry.C_CTVar x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CFuncType x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CTCons x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "CTypeExpr"

  showQ d (Curry.Module.AbstractCurry.C_CTVar x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CTVar "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.AbstractCurry.C_CFuncType x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CFuncType "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.AbstractCurry.C_CTCons x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CTCons "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.AbstractCurry.C_CTypeExprOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractCurry.C_COpDecl where
  strEq (Curry.Module.AbstractCurry.C_COp x1 x2 x3) (Curry.Module.AbstractCurry.C_COp y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.AbstractCurry.C_COp x1 x2 x3) (Curry.Module.AbstractCurry.C_COp y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.AbstractCurry.C_COp x1 x2 x3) st = Curry.Module.AbstractCurry.C_COp(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))

  foldCurry f c (Curry.Module.AbstractCurry.C_COp x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)

  typeName _ = "COpDecl"

  showQ d (Curry.Module.AbstractCurry.C_COp x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.COp "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ _ (Curry.Module.AbstractCurry.C_COpDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractCurry.C_CFixity where
  strEq Curry.Module.AbstractCurry.C_CInfixOp Curry.Module.AbstractCurry.C_CInfixOp st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.AbstractCurry.C_CInfixlOp Curry.Module.AbstractCurry.C_CInfixlOp st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.AbstractCurry.C_CInfixrOp Curry.Module.AbstractCurry.C_CInfixrOp st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.AbstractCurry.C_CInfixOp Curry.Module.AbstractCurry.C_CInfixOp st = Curry.Module.Prelude.C_True
  eq Curry.Module.AbstractCurry.C_CInfixlOp Curry.Module.AbstractCurry.C_CInfixlOp st = Curry.Module.Prelude.C_True
  eq Curry.Module.AbstractCurry.C_CInfixrOp Curry.Module.AbstractCurry.C_CInfixrOp st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.AbstractCurry.C_CInfixOp st = Curry.Module.AbstractCurry.C_CInfixOp
  propagate f Curry.Module.AbstractCurry.C_CInfixlOp st = Curry.Module.AbstractCurry.C_CInfixlOp
  propagate f Curry.Module.AbstractCurry.C_CInfixrOp st = Curry.Module.AbstractCurry.C_CInfixrOp

  foldCurry f c Curry.Module.AbstractCurry.C_CInfixOp st = c
  foldCurry f c Curry.Module.AbstractCurry.C_CInfixlOp st = c
  foldCurry f c Curry.Module.AbstractCurry.C_CInfixrOp st = c

  typeName _ = "CFixity"

  showQ _ Curry.Module.AbstractCurry.C_CInfixOp = Prelude.showString("AbstractCurry.CInfixOp")
  showQ _ Curry.Module.AbstractCurry.C_CInfixlOp = Prelude.showString("AbstractCurry.CInfixlOp")
  showQ _ Curry.Module.AbstractCurry.C_CInfixrOp = Prelude.showString("AbstractCurry.CInfixrOp")
  showQ _ (Curry.Module.AbstractCurry.C_CFixityOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractCurry.C_CFuncDecl where
  strEq (Curry.Module.AbstractCurry.C_CFunc x1 x2 x3 x4 x5) (Curry.Module.AbstractCurry.C_CFunc y1 y2 y3 y4 y5) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(st))(st))(st))(st)
  strEq (Curry.Module.AbstractCurry.C_CmtFunc x1 x2 x3 x4 x5 x6) (Curry.Module.AbstractCurry.C_CmtFunc y1 y2 y3 y4 y5 y6) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(Curry.Module.Prelude.genStrEq(x6)(y6)(st))(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.AbstractCurry.C_CFunc x1 x2 x3 x4 x5) (Curry.Module.AbstractCurry.C_CFunc y1 y2 y3 y4 y5) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.genEq(x5)(y5)(st))(st))(st))(st))(st)
  eq (Curry.Module.AbstractCurry.C_CmtFunc x1 x2 x3 x4 x5 x6) (Curry.Module.AbstractCurry.C_CmtFunc y1 y2 y3 y4 y5 y6) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x5)(y5)(st))(Curry.Module.Prelude.genEq(x6)(y6)(st))(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.AbstractCurry.C_CFunc x1 x2 x3 x4 x5) st = Curry.Module.AbstractCurry.C_CFunc(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))
  propagate f (Curry.Module.AbstractCurry.C_CmtFunc x1 x2 x3 x4 x5 x6) st = Curry.Module.AbstractCurry.C_CmtFunc(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))(f((5::Int))(x6)(st))

  foldCurry f c (Curry.Module.AbstractCurry.C_CFunc x1 x2 x3 x4 x5) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(c)(st))(st))(st))(st))(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CmtFunc x1 x2 x3 x4 x5 x6) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(f(x6)(c)(st))(st))(st))(st))(st))(st)

  typeName _ = "CFuncDecl"

  showQ d (Curry.Module.AbstractCurry.C_CFunc x1 x2 x3 x4 x5) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CFunc "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x5))))))))))


  showQ d (Curry.Module.AbstractCurry.C_CmtFunc x1 x2 x3 x4 x5 x6) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CmtFunc "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x5))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x6))))))))))))


  showQ _ (Curry.Module.AbstractCurry.C_CFuncDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractCurry.C_CRules where
  strEq (Curry.Module.AbstractCurry.C_CRules x1 x2) (Curry.Module.AbstractCurry.C_CRules y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.AbstractCurry.C_CExternal x1) (Curry.Module.AbstractCurry.C_CExternal y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.AbstractCurry.C_CRules x1 x2) (Curry.Module.AbstractCurry.C_CRules y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.AbstractCurry.C_CExternal x1) (Curry.Module.AbstractCurry.C_CExternal y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.AbstractCurry.C_CRules x1 x2) st = Curry.Module.AbstractCurry.C_CRules(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.AbstractCurry.C_CExternal x1) st = Curry.Module.AbstractCurry.C_CExternal(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.AbstractCurry.C_CRules x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CExternal x1) st = f(x1)(c)(st)

  typeName _ = "CRules"

  showQ d (Curry.Module.AbstractCurry.C_CRules x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CRules "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.AbstractCurry.C_CExternal x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CExternal "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.AbstractCurry.C_CRulesOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractCurry.C_CEvalAnnot where
  strEq Curry.Module.AbstractCurry.C_CFlex Curry.Module.AbstractCurry.C_CFlex st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.AbstractCurry.C_CRigid Curry.Module.AbstractCurry.C_CRigid st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.AbstractCurry.C_CChoice Curry.Module.AbstractCurry.C_CChoice st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.AbstractCurry.C_CFlex Curry.Module.AbstractCurry.C_CFlex st = Curry.Module.Prelude.C_True
  eq Curry.Module.AbstractCurry.C_CRigid Curry.Module.AbstractCurry.C_CRigid st = Curry.Module.Prelude.C_True
  eq Curry.Module.AbstractCurry.C_CChoice Curry.Module.AbstractCurry.C_CChoice st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.AbstractCurry.C_CFlex st = Curry.Module.AbstractCurry.C_CFlex
  propagate f Curry.Module.AbstractCurry.C_CRigid st = Curry.Module.AbstractCurry.C_CRigid
  propagate f Curry.Module.AbstractCurry.C_CChoice st = Curry.Module.AbstractCurry.C_CChoice

  foldCurry f c Curry.Module.AbstractCurry.C_CFlex st = c
  foldCurry f c Curry.Module.AbstractCurry.C_CRigid st = c
  foldCurry f c Curry.Module.AbstractCurry.C_CChoice st = c

  typeName _ = "CEvalAnnot"

  showQ _ Curry.Module.AbstractCurry.C_CFlex = Prelude.showString("AbstractCurry.CFlex")
  showQ _ Curry.Module.AbstractCurry.C_CRigid = Prelude.showString("AbstractCurry.CRigid")
  showQ _ Curry.Module.AbstractCurry.C_CChoice = Prelude.showString("AbstractCurry.CChoice")
  showQ _ (Curry.Module.AbstractCurry.C_CEvalAnnotOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractCurry.C_CRule where
  strEq (Curry.Module.AbstractCurry.C_CRule x1 x2 x3) (Curry.Module.AbstractCurry.C_CRule y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.AbstractCurry.C_CRule x1 x2 x3) (Curry.Module.AbstractCurry.C_CRule y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.AbstractCurry.C_CRule x1 x2 x3) st = Curry.Module.AbstractCurry.C_CRule(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))

  foldCurry f c (Curry.Module.AbstractCurry.C_CRule x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)

  typeName _ = "CRule"

  showQ d (Curry.Module.AbstractCurry.C_CRule x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CRule "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ _ (Curry.Module.AbstractCurry.C_CRuleOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractCurry.C_CLocalDecl where
  strEq (Curry.Module.AbstractCurry.C_CLocalFunc x1) (Curry.Module.AbstractCurry.C_CLocalFunc y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.AbstractCurry.C_CLocalPat x1 x2 x3) (Curry.Module.AbstractCurry.C_CLocalPat y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq (Curry.Module.AbstractCurry.C_CLocalVar x1) (Curry.Module.AbstractCurry.C_CLocalVar y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.AbstractCurry.C_CLocalFunc x1) (Curry.Module.AbstractCurry.C_CLocalFunc y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.AbstractCurry.C_CLocalPat x1 x2 x3) (Curry.Module.AbstractCurry.C_CLocalPat y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq (Curry.Module.AbstractCurry.C_CLocalVar x1) (Curry.Module.AbstractCurry.C_CLocalVar y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.AbstractCurry.C_CLocalFunc x1) st = Curry.Module.AbstractCurry.C_CLocalFunc(f((0::Int))(x1)(st))
  propagate f (Curry.Module.AbstractCurry.C_CLocalPat x1 x2 x3) st = Curry.Module.AbstractCurry.C_CLocalPat(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))
  propagate f (Curry.Module.AbstractCurry.C_CLocalVar x1) st = Curry.Module.AbstractCurry.C_CLocalVar(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.AbstractCurry.C_CLocalFunc x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CLocalPat x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CLocalVar x1) st = f(x1)(c)(st)

  typeName _ = "CLocalDecl"

  showQ d (Curry.Module.AbstractCurry.C_CLocalFunc x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CLocalFunc "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.AbstractCurry.C_CLocalPat x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CLocalPat "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ d (Curry.Module.AbstractCurry.C_CLocalVar x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CLocalVar "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.AbstractCurry.C_CLocalDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractCurry.C_CExpr where
  strEq (Curry.Module.AbstractCurry.C_CVar x1) (Curry.Module.AbstractCurry.C_CVar y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.AbstractCurry.C_CLit x1) (Curry.Module.AbstractCurry.C_CLit y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.AbstractCurry.C_CSymbol x1) (Curry.Module.AbstractCurry.C_CSymbol y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.AbstractCurry.C_CApply x1 x2) (Curry.Module.AbstractCurry.C_CApply y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.AbstractCurry.C_CLambda x1 x2) (Curry.Module.AbstractCurry.C_CLambda y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.AbstractCurry.C_CLetDecl x1 x2) (Curry.Module.AbstractCurry.C_CLetDecl y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.AbstractCurry.C_CDoExpr x1) (Curry.Module.AbstractCurry.C_CDoExpr y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.AbstractCurry.C_CListComp x1 x2) (Curry.Module.AbstractCurry.C_CListComp y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.AbstractCurry.C_CCase x1 x2) (Curry.Module.AbstractCurry.C_CCase y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.AbstractCurry.C_CVar x1) (Curry.Module.AbstractCurry.C_CVar y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.AbstractCurry.C_CLit x1) (Curry.Module.AbstractCurry.C_CLit y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.AbstractCurry.C_CSymbol x1) (Curry.Module.AbstractCurry.C_CSymbol y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.AbstractCurry.C_CApply x1 x2) (Curry.Module.AbstractCurry.C_CApply y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.AbstractCurry.C_CLambda x1 x2) (Curry.Module.AbstractCurry.C_CLambda y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.AbstractCurry.C_CLetDecl x1 x2) (Curry.Module.AbstractCurry.C_CLetDecl y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.AbstractCurry.C_CDoExpr x1) (Curry.Module.AbstractCurry.C_CDoExpr y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.AbstractCurry.C_CListComp x1 x2) (Curry.Module.AbstractCurry.C_CListComp y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.AbstractCurry.C_CCase x1 x2) (Curry.Module.AbstractCurry.C_CCase y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.AbstractCurry.C_CVar x1) st = Curry.Module.AbstractCurry.C_CVar(f((0::Int))(x1)(st))
  propagate f (Curry.Module.AbstractCurry.C_CLit x1) st = Curry.Module.AbstractCurry.C_CLit(f((0::Int))(x1)(st))
  propagate f (Curry.Module.AbstractCurry.C_CSymbol x1) st = Curry.Module.AbstractCurry.C_CSymbol(f((0::Int))(x1)(st))
  propagate f (Curry.Module.AbstractCurry.C_CApply x1 x2) st = Curry.Module.AbstractCurry.C_CApply(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.AbstractCurry.C_CLambda x1 x2) st = Curry.Module.AbstractCurry.C_CLambda(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.AbstractCurry.C_CLetDecl x1 x2) st = Curry.Module.AbstractCurry.C_CLetDecl(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.AbstractCurry.C_CDoExpr x1) st = Curry.Module.AbstractCurry.C_CDoExpr(f((0::Int))(x1)(st))
  propagate f (Curry.Module.AbstractCurry.C_CListComp x1 x2) st = Curry.Module.AbstractCurry.C_CListComp(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.AbstractCurry.C_CCase x1 x2) st = Curry.Module.AbstractCurry.C_CCase(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.AbstractCurry.C_CVar x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CLit x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CSymbol x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CApply x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CLambda x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CLetDecl x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CDoExpr x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CListComp x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CCase x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "CExpr"

  showQ d (Curry.Module.AbstractCurry.C_CVar x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CVar "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.AbstractCurry.C_CLit x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CLit "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.AbstractCurry.C_CSymbol x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CSymbol "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.AbstractCurry.C_CApply x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CApply "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.AbstractCurry.C_CLambda x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CLambda "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.AbstractCurry.C_CLetDecl x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CLetDecl "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.AbstractCurry.C_CDoExpr x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CDoExpr "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.AbstractCurry.C_CListComp x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CListComp "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.AbstractCurry.C_CCase x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CCase "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.AbstractCurry.C_CExprOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractCurry.C_CStatement where
  strEq (Curry.Module.AbstractCurry.C_CSExpr x1) (Curry.Module.AbstractCurry.C_CSExpr y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.AbstractCurry.C_CSPat x1 x2) (Curry.Module.AbstractCurry.C_CSPat y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.AbstractCurry.C_CSLet x1) (Curry.Module.AbstractCurry.C_CSLet y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.AbstractCurry.C_CSExpr x1) (Curry.Module.AbstractCurry.C_CSExpr y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.AbstractCurry.C_CSPat x1 x2) (Curry.Module.AbstractCurry.C_CSPat y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.AbstractCurry.C_CSLet x1) (Curry.Module.AbstractCurry.C_CSLet y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.AbstractCurry.C_CSExpr x1) st = Curry.Module.AbstractCurry.C_CSExpr(f((0::Int))(x1)(st))
  propagate f (Curry.Module.AbstractCurry.C_CSPat x1 x2) st = Curry.Module.AbstractCurry.C_CSPat(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.AbstractCurry.C_CSLet x1) st = Curry.Module.AbstractCurry.C_CSLet(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.AbstractCurry.C_CSExpr x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CSPat x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CSLet x1) st = f(x1)(c)(st)

  typeName _ = "CStatement"

  showQ d (Curry.Module.AbstractCurry.C_CSExpr x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CSExpr "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.AbstractCurry.C_CSPat x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CSPat "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.AbstractCurry.C_CSLet x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CSLet "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.AbstractCurry.C_CStatementOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractCurry.C_CPattern where
  strEq (Curry.Module.AbstractCurry.C_CPVar x1) (Curry.Module.AbstractCurry.C_CPVar y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.AbstractCurry.C_CPLit x1) (Curry.Module.AbstractCurry.C_CPLit y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.AbstractCurry.C_CPComb x1 x2) (Curry.Module.AbstractCurry.C_CPComb y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.AbstractCurry.C_CPAs x1 x2) (Curry.Module.AbstractCurry.C_CPAs y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.AbstractCurry.C_CPFuncComb x1 x2) (Curry.Module.AbstractCurry.C_CPFuncComb y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.AbstractCurry.C_CPVar x1) (Curry.Module.AbstractCurry.C_CPVar y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.AbstractCurry.C_CPLit x1) (Curry.Module.AbstractCurry.C_CPLit y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.AbstractCurry.C_CPComb x1 x2) (Curry.Module.AbstractCurry.C_CPComb y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.AbstractCurry.C_CPAs x1 x2) (Curry.Module.AbstractCurry.C_CPAs y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.AbstractCurry.C_CPFuncComb x1 x2) (Curry.Module.AbstractCurry.C_CPFuncComb y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.AbstractCurry.C_CPVar x1) st = Curry.Module.AbstractCurry.C_CPVar(f((0::Int))(x1)(st))
  propagate f (Curry.Module.AbstractCurry.C_CPLit x1) st = Curry.Module.AbstractCurry.C_CPLit(f((0::Int))(x1)(st))
  propagate f (Curry.Module.AbstractCurry.C_CPComb x1 x2) st = Curry.Module.AbstractCurry.C_CPComb(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.AbstractCurry.C_CPAs x1 x2) st = Curry.Module.AbstractCurry.C_CPAs(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.AbstractCurry.C_CPFuncComb x1 x2) st = Curry.Module.AbstractCurry.C_CPFuncComb(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.AbstractCurry.C_CPVar x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CPLit x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CPComb x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CPAs x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CPFuncComb x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "CPattern"

  showQ d (Curry.Module.AbstractCurry.C_CPVar x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CPVar "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.AbstractCurry.C_CPLit x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CPLit "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.AbstractCurry.C_CPComb x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CPComb "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.AbstractCurry.C_CPAs x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CPAs "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.AbstractCurry.C_CPFuncComb x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CPFuncComb "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.AbstractCurry.C_CPatternOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractCurry.C_CBranchExpr where
  strEq (Curry.Module.AbstractCurry.C_CBranch x1 x2) (Curry.Module.AbstractCurry.C_CBranch y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.AbstractCurry.C_CBranch x1 x2) (Curry.Module.AbstractCurry.C_CBranch y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.AbstractCurry.C_CBranch x1 x2) st = Curry.Module.AbstractCurry.C_CBranch(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.AbstractCurry.C_CBranch x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "CBranchExpr"

  showQ d (Curry.Module.AbstractCurry.C_CBranch x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CBranch "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.AbstractCurry.C_CBranchExprOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractCurry.C_CLiteral where
  strEq (Curry.Module.AbstractCurry.C_CIntc x1) (Curry.Module.AbstractCurry.C_CIntc y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.AbstractCurry.C_CFloatc x1) (Curry.Module.AbstractCurry.C_CFloatc y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.AbstractCurry.C_CCharc x1) (Curry.Module.AbstractCurry.C_CCharc y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.AbstractCurry.C_CIntc x1) (Curry.Module.AbstractCurry.C_CIntc y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.AbstractCurry.C_CFloatc x1) (Curry.Module.AbstractCurry.C_CFloatc y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.AbstractCurry.C_CCharc x1) (Curry.Module.AbstractCurry.C_CCharc y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.AbstractCurry.C_CIntc x1) st = Curry.Module.AbstractCurry.C_CIntc(f((0::Int))(x1)(st))
  propagate f (Curry.Module.AbstractCurry.C_CFloatc x1) st = Curry.Module.AbstractCurry.C_CFloatc(f((0::Int))(x1)(st))
  propagate f (Curry.Module.AbstractCurry.C_CCharc x1) st = Curry.Module.AbstractCurry.C_CCharc(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.AbstractCurry.C_CIntc x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CFloatc x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.AbstractCurry.C_CCharc x1) st = f(x1)(c)(st)

  typeName _ = "CLiteral"

  showQ d (Curry.Module.AbstractCurry.C_CIntc x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CIntc "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.AbstractCurry.C_CFloatc x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CFloatc "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.AbstractCurry.C_CCharc x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractCurry.CCharc "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.AbstractCurry.C_CLiteralOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractCurry.C_CurryProg where
  showsPrec d (Curry.Module.AbstractCurry.C_CurryProg x1 x2 x3 x4 x5) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurryProg "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x5))))))))))


  showsPrec _ (Curry.Module.AbstractCurry.C_CurryProgOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractCurry.C_CVisibility where
  showsPrec _ Curry.Module.AbstractCurry.C_Public = Prelude.showString("Public")
  showsPrec _ Curry.Module.AbstractCurry.C_Private = Prelude.showString("Private")
  showsPrec _ (Curry.Module.AbstractCurry.C_CVisibilityOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractCurry.C_CTypeDecl where
  showsPrec d (Curry.Module.AbstractCurry.C_CType x1 x2 x3 x4) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CType "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))))))))


  showsPrec d (Curry.Module.AbstractCurry.C_CTypeSyn x1 x2 x3 x4) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CTypeSyn "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))))))))


  showsPrec _ (Curry.Module.AbstractCurry.C_CTypeDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractCurry.C_CConsDecl where
  showsPrec d (Curry.Module.AbstractCurry.C_CCons x1 x2 x3 x4) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CCons "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))))))))


  showsPrec _ (Curry.Module.AbstractCurry.C_CConsDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractCurry.C_CTypeExpr where
  showsPrec d (Curry.Module.AbstractCurry.C_CTVar x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CTVar "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.AbstractCurry.C_CFuncType x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CFuncType "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.AbstractCurry.C_CTCons x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CTCons "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.AbstractCurry.C_CTypeExprOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractCurry.C_COpDecl where
  showsPrec d (Curry.Module.AbstractCurry.C_COp x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("COp "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec _ (Curry.Module.AbstractCurry.C_COpDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractCurry.C_CFixity where
  showsPrec _ Curry.Module.AbstractCurry.C_CInfixOp = Prelude.showString("CInfixOp")
  showsPrec _ Curry.Module.AbstractCurry.C_CInfixlOp = Prelude.showString("CInfixlOp")
  showsPrec _ Curry.Module.AbstractCurry.C_CInfixrOp = Prelude.showString("CInfixrOp")
  showsPrec _ (Curry.Module.AbstractCurry.C_CFixityOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractCurry.C_CFuncDecl where
  showsPrec d (Curry.Module.AbstractCurry.C_CFunc x1 x2 x3 x4 x5) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CFunc "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x5))))))))))


  showsPrec d (Curry.Module.AbstractCurry.C_CmtFunc x1 x2 x3 x4 x5 x6) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CmtFunc "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x5))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x6))))))))))))


  showsPrec _ (Curry.Module.AbstractCurry.C_CFuncDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractCurry.C_CRules where
  showsPrec d (Curry.Module.AbstractCurry.C_CRules x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CRules "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.AbstractCurry.C_CExternal x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CExternal "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.AbstractCurry.C_CRulesOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractCurry.C_CEvalAnnot where
  showsPrec _ Curry.Module.AbstractCurry.C_CFlex = Prelude.showString("CFlex")
  showsPrec _ Curry.Module.AbstractCurry.C_CRigid = Prelude.showString("CRigid")
  showsPrec _ Curry.Module.AbstractCurry.C_CChoice = Prelude.showString("CChoice")
  showsPrec _ (Curry.Module.AbstractCurry.C_CEvalAnnotOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractCurry.C_CRule where
  showsPrec d (Curry.Module.AbstractCurry.C_CRule x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CRule "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec _ (Curry.Module.AbstractCurry.C_CRuleOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractCurry.C_CLocalDecl where
  showsPrec d (Curry.Module.AbstractCurry.C_CLocalFunc x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CLocalFunc "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.AbstractCurry.C_CLocalPat x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CLocalPat "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec d (Curry.Module.AbstractCurry.C_CLocalVar x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CLocalVar "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.AbstractCurry.C_CLocalDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractCurry.C_CExpr where
  showsPrec d (Curry.Module.AbstractCurry.C_CVar x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CVar "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.AbstractCurry.C_CLit x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CLit "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.AbstractCurry.C_CSymbol x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CSymbol "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.AbstractCurry.C_CApply x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CApply "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.AbstractCurry.C_CLambda x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CLambda "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.AbstractCurry.C_CLetDecl x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CLetDecl "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.AbstractCurry.C_CDoExpr x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CDoExpr "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.AbstractCurry.C_CListComp x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CListComp "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.AbstractCurry.C_CCase x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CCase "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.AbstractCurry.C_CExprOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractCurry.C_CStatement where
  showsPrec d (Curry.Module.AbstractCurry.C_CSExpr x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CSExpr "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.AbstractCurry.C_CSPat x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CSPat "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.AbstractCurry.C_CSLet x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CSLet "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.AbstractCurry.C_CStatementOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractCurry.C_CPattern where
  showsPrec d (Curry.Module.AbstractCurry.C_CPVar x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CPVar "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.AbstractCurry.C_CPLit x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CPLit "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.AbstractCurry.C_CPComb x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CPComb "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.AbstractCurry.C_CPAs x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CPAs "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.AbstractCurry.C_CPFuncComb x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CPFuncComb "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.AbstractCurry.C_CPatternOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractCurry.C_CBranchExpr where
  showsPrec d (Curry.Module.AbstractCurry.C_CBranch x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CBranch "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.AbstractCurry.C_CBranchExprOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractCurry.C_CLiteral where
  showsPrec d (Curry.Module.AbstractCurry.C_CIntc x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CIntc "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.AbstractCurry.C_CFloatc x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CFloatc "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.AbstractCurry.C_CCharc x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CCharc "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.AbstractCurry.C_CLiteralOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Read Curry.Module.AbstractCurry.C_CurryProg where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CurryProg(x1)(x2)(x3)(x4)(x5))(r5) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CurryProg")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3), ((,) x5 r5) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r4)])(r)





instance Read Curry.Module.AbstractCurry.C_CVisibility where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.AbstractCurry.C_Public)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("Public")(r)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.AbstractCurry.C_Private)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("Private")(r)])(r))





instance Read Curry.Module.AbstractCurry.C_CTypeDecl where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CType(x1)(x2)(x3)(x4))(r4) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CType")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CTypeSyn(x1)(x2)(x3)(x4))(r4) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CTypeSyn")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3)])(r))





instance Read Curry.Module.AbstractCurry.C_CConsDecl where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CCons(x1)(x2)(x3)(x4))(r4) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CCons")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3)])(r)





instance Read Curry.Module.AbstractCurry.C_CTypeExpr where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CTVar(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CTVar")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CFuncType(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CFuncType")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CTCons(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CTCons")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r)))





instance Read Curry.Module.AbstractCurry.C_COpDecl where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_COp(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("COp")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r)





instance Read Curry.Module.AbstractCurry.C_CFixity where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.AbstractCurry.C_CInfixOp)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CInfixOp")(r)])(r))((Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.AbstractCurry.C_CInfixlOp)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CInfixlOp")(r)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.AbstractCurry.C_CInfixrOp)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CInfixrOp")(r)])(r)))





instance Read Curry.Module.AbstractCurry.C_CFuncDecl where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CFunc(x1)(x2)(x3)(x4)(x5))(r5) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CFunc")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3), ((,) x5 r5) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r4)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CmtFunc(x1)(x2)(x3)(x4)(x5)(x6))(r6) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CmtFunc")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3), ((,) x5 r5) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r4), ((,) x6 r6) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r5)])(r))





instance Read Curry.Module.AbstractCurry.C_CRules where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CRules(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CRules")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CExternal(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CExternal")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))





instance Read Curry.Module.AbstractCurry.C_CEvalAnnot where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.AbstractCurry.C_CFlex)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CFlex")(r)])(r))((Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.AbstractCurry.C_CRigid)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CRigid")(r)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.AbstractCurry.C_CChoice)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CChoice")(r)])(r)))





instance Read Curry.Module.AbstractCurry.C_CRule where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CRule(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CRule")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r)





instance Read Curry.Module.AbstractCurry.C_CLocalDecl where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CLocalFunc(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CLocalFunc")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CLocalPat(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CLocalPat")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CLocalVar(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CLocalVar")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r)))





instance Read Curry.Module.AbstractCurry.C_CExpr where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CVar(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CVar")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CLit(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CLit")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CSymbol(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CSymbol")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CApply(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CApply")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CLambda(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CLambda")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CLetDecl(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CLetDecl")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CDoExpr(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CDoExpr")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CListComp(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CListComp")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CCase(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CCase")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r)))))))))





instance Read Curry.Module.AbstractCurry.C_CStatement where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CSExpr(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CSExpr")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CSPat(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CSPat")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CSLet(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CSLet")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r)))





instance Read Curry.Module.AbstractCurry.C_CPattern where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CPVar(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CPVar")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CPLit(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CPLit")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CPComb(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CPComb")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CPAs(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CPAs")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CPFuncComb(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CPFuncComb")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r)))))





instance Read Curry.Module.AbstractCurry.C_CBranchExpr where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CBranch(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CBranch")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r)





instance Read Curry.Module.AbstractCurry.C_CLiteral where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CIntc(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CIntc")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CFloatc(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CFloatc")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractCurry.C_CCharc(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractCurry")("CCharc")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r)))





c_readCurry :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.AbstractCurry.C_CurryProg
c_readCurry x1 st = Curry.Module.AbstractCurry.c_readCurryWithParseOptions(x1)(Curry.Module.Distribution.c_setQuiet(Curry.Module.Prelude.C_True)(Curry.Module.Distribution.c_defaultParams(st))(st))(st)



c_readUntypedCurry :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.AbstractCurry.C_CurryProg
c_readUntypedCurry x1 st = Curry.Module.AbstractCurry.c_readUntypedCurryWithParseOptions(x1)(Curry.Module.Distribution.c_setQuiet(Curry.Module.Prelude.C_True)(Curry.Module.Distribution.c_defaultParams(st))(st))(st)



c_readCurryWithParseOptions :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Distribution.C_FrontendParams -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.AbstractCurry.C_CurryProg
c_readCurryWithParseOptions x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesFileExist(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.AbstractCurry.c_readCurryWithParseOptions'46_'35lambda2(x2)(x1)))(st)



c_readCurryWithParseOptions'46_'35lambda2 :: Curry.Module.Distribution.C_FrontendParams -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.AbstractCurry.C_CurryProg
c_readCurryWithParseOptions'46_'35lambda2 x1 x2 x3 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesFileExist(Curry.Module.Prelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.AbstractCurry.c_readCurryWithParseOptions'46_'35lambda2'46_'35lambda3(x3)(x1)(x2)))(st)



c_readCurryWithParseOptions'46_'35lambda2'46_'35lambda3 :: Curry.Module.Prelude.C_Bool -> Curry.Module.Distribution.C_FrontendParams -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.AbstractCurry.C_CurryProg
c_readCurryWithParseOptions'46_'35lambda2'46_'35lambda3 x1 x2 x3 x4 st = Curry.Module.Prelude.op_62_62(Curry.Module.AbstractCurry.c_readCurryWithParseOptions'46_'35lambda2'46_'35lambda3_case_1(x1)(x2)(x3)(x4)(Curry.Module.Prelude.op_124_124(x1)(x4)(st))(st))(Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_findFileInLoadPath(Curry.Module.Prelude.op_43_43(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.AbstractCurry.c_readCurryWithParseOptions'46_'35lambda2'46_'35lambda3'46_'35lambda4))(st))(st)



c_readCurryWithParseOptions'46_'35lambda2'46_'35lambda3'46_'35lambda4 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.AbstractCurry.C_CurryProg
c_readCurryWithParseOptions'46_'35lambda2'46_'35lambda3'46_'35lambda4 x1 st = Curry.Module.AbstractCurry.c_readAbstractCurryFile(x1)(st)



c_readUntypedCurryWithParseOptions :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Distribution.C_FrontendParams -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.AbstractCurry.C_CurryProg
c_readUntypedCurryWithParseOptions x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesFileExist(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.AbstractCurry.c_readUntypedCurryWithParseOptions'46_'35lambda5(x2)(x1)))(st)



c_readUntypedCurryWithParseOptions'46_'35lambda5 :: Curry.Module.Distribution.C_FrontendParams -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.AbstractCurry.C_CurryProg
c_readUntypedCurryWithParseOptions'46_'35lambda5 x1 x2 x3 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesFileExist(Curry.Module.Prelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.AbstractCurry.c_readUntypedCurryWithParseOptions'46_'35lambda5'46_'35lambda6(x3)(x1)(x2)))(st)



c_readUntypedCurryWithParseOptions'46_'35lambda5'46_'35lambda6 :: Curry.Module.Prelude.C_Bool -> Curry.Module.Distribution.C_FrontendParams -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.AbstractCurry.C_CurryProg
c_readUntypedCurryWithParseOptions'46_'35lambda5'46_'35lambda6 x1 x2 x3 x4 st = Curry.Module.Prelude.op_62_62(Curry.Module.AbstractCurry.c_readUntypedCurryWithParseOptions'46_'35lambda5'46_'35lambda6_case_0(x1)(x2)(x3)(x4)(Curry.Module.Prelude.op_124_124(x1)(x4)(st))(st))(Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_findFileInLoadPath(Curry.Module.Prelude.op_43_43(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.AbstractCurry.c_readUntypedCurryWithParseOptions'46_'35lambda5'46_'35lambda6'46_'35lambda7))(st))(st)



c_readUntypedCurryWithParseOptions'46_'35lambda5'46_'35lambda6'46_'35lambda7 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.AbstractCurry.C_CurryProg
c_readUntypedCurryWithParseOptions'46_'35lambda5'46_'35lambda6'46_'35lambda7 x1 st = Curry.Module.AbstractCurry.c_readAbstractCurryFile(x1)(st)



c_abstractCurryFileName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_abstractCurryFileName x1 st = Curry.Module.Distribution.c_inCurrySubdir(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_apply(Curry.Module.FileGoodies.c_stripSuffix(st))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))(st))(st)



c_untypedAbstractCurryFileName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_untypedAbstractCurryFileName x1 st = Curry.Module.Distribution.c_inCurrySubdir(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_apply(Curry.Module.FileGoodies.c_stripSuffix(st))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))(st))(st)



c_readAbstractCurryFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.AbstractCurry.C_CurryProg
c_readAbstractCurryFile x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesFileExist(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.AbstractCurry.c_readAbstractCurryFile'46_'35lambda9(x1)))(st)



c_readAbstractCurryFile'46readExistingACY'4621 :: (Curry t0) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_readAbstractCurryFile'46readExistingACY'4621 x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_readFile(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.AbstractCurry.c_readAbstractCurryFile'46readExistingACY'4621'46_'35lambda8))(st)



c_readAbstractCurryFile'46readExistingACY'4621'46_'35lambda8 :: (Curry t1) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1
c_readAbstractCurryFile'46readExistingACY'4621'46_'35lambda8 x1 st = Curry.Module.Prelude.c_return(Curry.Module.ReadShowTerm.c_readUnqualifiedTerm((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(Curry.Module.Prelude.List)))(x1)(st))(st)



c_readAbstractCurryFile'46_'35lambda9 :: (Curry t2) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t2
c_readAbstractCurryFile'46_'35lambda9 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurry.c_readAbstractCurryFile'46readExistingACY'4621(x1)(st)
c_readAbstractCurryFile'46_'35lambda9 x1 x2@Curry.Module.Prelude.C_False st = let {x3 = Curry.Module.Distribution.c_inCurrySubdir(x1)(st)} in Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesFileExist(x3)(st))(Curry.Module.Prelude.pf(Curry.Module.AbstractCurry.c_readAbstractCurryFile'46_'35lambda9'46_'35lambda10(x1)(x3)))(st)
c_readAbstractCurryFile'46_'35lambda9 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurry.c_readAbstractCurryFile'46_'35lambda9(x1)(x)(st))(i)(xs)(st)
c_readAbstractCurryFile'46_'35lambda9 x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurry.readAbstractCurryFile._#lambda9")(x)



c_readAbstractCurryFile'46_'35lambda9'46_'35lambda10 :: (Curry t3) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t3
c_readAbstractCurryFile'46_'35lambda9'46_'35lambda10 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurry.c_readAbstractCurryFile'46readExistingACY'4621(x2)(st)
c_readAbstractCurryFile'46_'35lambda9'46_'35lambda10 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_error(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('X'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))))))(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))))))))))))))(st))(st))(st)
c_readAbstractCurryFile'46_'35lambda9'46_'35lambda10 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurry.c_readAbstractCurryFile'46_'35lambda9'46_'35lambda10(x1)(x2)(x)(st))(i)(xs)(st)
c_readAbstractCurryFile'46_'35lambda9'46_'35lambda10 x1 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurry.readAbstractCurryFile._#lambda9._#lambda10")(x)



c_writeAbstractCurryFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.AbstractCurry.C_CurryProg -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_writeAbstractCurryFile x1 x2 st = Curry.Module.Prelude.c_writeFile(x1)(Curry.Module.ReadShowTerm.c_showTerm(x2)(st))(st)



c_readUntypedCurryWithParseOptions'46_'35lambda5'46_'35lambda6_case_0 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Distribution.c_callFrontendWithParams(Curry.Module.Distribution.C_UACY)(x2)(x3)(st)
c_readUntypedCurryWithParseOptions'46_'35lambda5'46_'35lambda6_case_0 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_done(st)
c_readUntypedCurryWithParseOptions'46_'35lambda5'46_'35lambda6_case_0 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurry.c_readUntypedCurryWithParseOptions'46_'35lambda5'46_'35lambda6_case_0(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_readUntypedCurryWithParseOptions'46_'35lambda5'46_'35lambda6_case_0 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("AbstractCurry.readUntypedCurryWithParseOptions._#lambda5._#lambda6_case_0")(x)



c_readCurryWithParseOptions'46_'35lambda2'46_'35lambda3_case_1 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Distribution.c_callFrontendWithParams(Curry.Module.Distribution.C_ACY)(x2)(x3)(st)
c_readCurryWithParseOptions'46_'35lambda2'46_'35lambda3_case_1 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_done(st)
c_readCurryWithParseOptions'46_'35lambda2'46_'35lambda3_case_1 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurry.c_readCurryWithParseOptions'46_'35lambda2'46_'35lambda3_case_1(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_readCurryWithParseOptions'46_'35lambda2'46_'35lambda3_case_1 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("AbstractCurry.readCurryWithParseOptions._#lambda2._#lambda3_case_1")(x)


