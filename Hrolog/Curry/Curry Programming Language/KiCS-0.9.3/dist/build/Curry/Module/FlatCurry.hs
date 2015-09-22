{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.FlatCurry (module Curry.Module.FlatCurry) where

import Curry.RunTimeSystem
import Curry.Module.Directory
import Curry.Module.Distribution
import Curry.Module.FileGoodies
import Curry.Module.Prelude
import Curry.Module.ReadShowTerm



-- begin included



-- end included

type C_QName = Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)

type C_TVarIndex = Curry.Module.Prelude.C_Int

type C_VarIndex = Curry.Module.Prelude.C_Int

data C_Prog = C_Prog (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_TypeDecl) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_OpDecl)
  | C_ProgFail Curry.RunTimeSystem.C_Exceptions
  | C_ProgOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.FlatCurry.C_Prog)

data C_Visibility = C_Public
  | C_Private
  | C_VisibilityFail Curry.RunTimeSystem.C_Exceptions
  | C_VisibilityOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.FlatCurry.C_Visibility)

data C_TypeDecl = C_Type (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.FlatCurry.C_Visibility (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_ConsDecl)
  | C_TypeSyn (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.FlatCurry.C_Visibility (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) Curry.Module.FlatCurry.C_TypeExpr
  | C_TypeDeclFail Curry.RunTimeSystem.C_Exceptions
  | C_TypeDeclOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.FlatCurry.C_TypeDecl)

data C_ConsDecl = C_Cons (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Visibility (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_TypeExpr)
  | C_ConsDeclFail Curry.RunTimeSystem.C_Exceptions
  | C_ConsDeclOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.FlatCurry.C_ConsDecl)

data C_TypeExpr = C_TVar Curry.Module.Prelude.C_Int
  | C_FuncType Curry.Module.FlatCurry.C_TypeExpr Curry.Module.FlatCurry.C_TypeExpr
  | C_TCons (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_TypeExpr)
  | C_TypeExprFail Curry.RunTimeSystem.C_Exceptions
  | C_TypeExprOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.FlatCurry.C_TypeExpr)

data C_OpDecl = C_Op (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.FlatCurry.C_Fixity Curry.Module.Prelude.C_Int
  | C_OpDeclFail Curry.RunTimeSystem.C_Exceptions
  | C_OpDeclOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.FlatCurry.C_OpDecl)

data C_Fixity = C_InfixOp
  | C_InfixlOp
  | C_InfixrOp
  | C_FixityFail Curry.RunTimeSystem.C_Exceptions
  | C_FixityOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.FlatCurry.C_Fixity)

data C_FuncDecl = C_Func (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Visibility Curry.Module.FlatCurry.C_TypeExpr Curry.Module.FlatCurry.C_Rule
  | C_FuncDeclFail Curry.RunTimeSystem.C_Exceptions
  | C_FuncDeclOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.FlatCurry.C_FuncDecl)

data C_Rule = C_Rule (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) Curry.Module.FlatCurry.C_Expr
  | C_External (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_RuleFail Curry.RunTimeSystem.C_Exceptions
  | C_RuleOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.FlatCurry.C_Rule)

data C_CaseType = C_Rigid
  | C_Flex
  | C_CaseTypeFail Curry.RunTimeSystem.C_Exceptions
  | C_CaseTypeOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.FlatCurry.C_CaseType)

data C_CombType = C_FuncCall
  | C_ConsCall
  | C_FuncPartCall Curry.Module.Prelude.C_Int
  | C_ConsPartCall Curry.Module.Prelude.C_Int
  | C_CombTypeFail Curry.RunTimeSystem.C_Exceptions
  | C_CombTypeOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.FlatCurry.C_CombType)

data C_Expr = C_Var Curry.Module.Prelude.C_Int
  | C_Lit Curry.Module.FlatCurry.C_Literal
  | C_Comb Curry.Module.FlatCurry.C_CombType (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr)
  | C_Let (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Expr)) Curry.Module.FlatCurry.C_Expr
  | C_Free (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) Curry.Module.FlatCurry.C_Expr
  | C_Or Curry.Module.FlatCurry.C_Expr Curry.Module.FlatCurry.C_Expr
  | C_Case Curry.Module.FlatCurry.C_CaseType Curry.Module.FlatCurry.C_Expr (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_BranchExpr)
  | C_ExprFail Curry.RunTimeSystem.C_Exceptions
  | C_ExprOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.FlatCurry.C_Expr)

data C_BranchExpr = C_Branch Curry.Module.FlatCurry.C_Pattern Curry.Module.FlatCurry.C_Expr
  | C_BranchExprFail Curry.RunTimeSystem.C_Exceptions
  | C_BranchExprOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.FlatCurry.C_BranchExpr)

data C_Pattern = C_Pattern (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)
  | C_LPattern Curry.Module.FlatCurry.C_Literal
  | C_PatternFail Curry.RunTimeSystem.C_Exceptions
  | C_PatternOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.FlatCurry.C_Pattern)

data C_Literal = C_Intc Curry.Module.Prelude.C_Int
  | C_Floatc Curry.Module.Prelude.C_Float
  | C_Charc Curry.Module.Prelude.C_Char
  | C_LiteralFail Curry.RunTimeSystem.C_Exceptions
  | C_LiteralOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.FlatCurry.C_Literal)

instance BaseCurry Curry.Module.FlatCurry.C_Prog where
  nf f (Curry.Module.FlatCurry.C_Prog x1 x2 x3 x4 x5) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> f(Curry.Module.FlatCurry.C_Prog(v1)(v2)(v3)(v4)(v5))(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.FlatCurry.C_Prog x1 x2 x3 x4 x5) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> f(Curry.Module.FlatCurry.C_Prog(v1)(v2)(v3)(v4)(v5))(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.FlatCurry.C_ProgOr(Curry.RunTimeSystem.mkRef(r)(5)(i))([Curry.Module.FlatCurry.C_Prog(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(5)

  failed  = Curry.Module.FlatCurry.C_ProgFail

  branching  = Curry.Module.FlatCurry.C_ProgOr

  consKind (Curry.Module.FlatCurry.C_ProgOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.FlatCurry.C_ProgFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.FlatCurry.C_ProgFail x) = x

  orRef (Curry.Module.FlatCurry.C_ProgOr x _) = x

  branches (Curry.Module.FlatCurry.C_ProgOr _ x) = x





instance BaseCurry Curry.Module.FlatCurry.C_Visibility where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.FlatCurry.C_VisibilityOr(Curry.RunTimeSystem.mkRef(r)(0)(i))([Curry.Module.FlatCurry.C_Public,Curry.Module.FlatCurry.C_Private]))(0)

  failed  = Curry.Module.FlatCurry.C_VisibilityFail

  branching  = Curry.Module.FlatCurry.C_VisibilityOr

  consKind (Curry.Module.FlatCurry.C_VisibilityOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.FlatCurry.C_VisibilityFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.FlatCurry.C_VisibilityFail x) = x

  orRef (Curry.Module.FlatCurry.C_VisibilityOr x _) = x

  branches (Curry.Module.FlatCurry.C_VisibilityOr _ x) = x





instance BaseCurry Curry.Module.FlatCurry.C_TypeDecl where
  nf f (Curry.Module.FlatCurry.C_Type x1 x2 x3 x4) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> f(Curry.Module.FlatCurry.C_Type(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.FlatCurry.C_TypeSyn x1 x2 x3 x4) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> f(Curry.Module.FlatCurry.C_TypeSyn(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.FlatCurry.C_Type x1 x2 x3 x4) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> f(Curry.Module.FlatCurry.C_Type(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.FlatCurry.C_TypeSyn x1 x2 x3 x4) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> f(Curry.Module.FlatCurry.C_TypeSyn(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.FlatCurry.C_TypeDeclOr(Curry.RunTimeSystem.mkRef(r)(4)(i))([Curry.Module.FlatCurry.C_Type(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.FlatCurry.C_TypeSyn(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(4)

  failed  = Curry.Module.FlatCurry.C_TypeDeclFail

  branching  = Curry.Module.FlatCurry.C_TypeDeclOr

  consKind (Curry.Module.FlatCurry.C_TypeDeclOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.FlatCurry.C_TypeDeclFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.FlatCurry.C_TypeDeclFail x) = x

  orRef (Curry.Module.FlatCurry.C_TypeDeclOr x _) = x

  branches (Curry.Module.FlatCurry.C_TypeDeclOr _ x) = x





instance BaseCurry Curry.Module.FlatCurry.C_ConsDecl where
  nf f (Curry.Module.FlatCurry.C_Cons x1 x2 x3 x4) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> f(Curry.Module.FlatCurry.C_Cons(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.FlatCurry.C_Cons x1 x2 x3 x4) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> f(Curry.Module.FlatCurry.C_Cons(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.FlatCurry.C_ConsDeclOr(Curry.RunTimeSystem.mkRef(r)(4)(i))([Curry.Module.FlatCurry.C_Cons(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(4)

  failed  = Curry.Module.FlatCurry.C_ConsDeclFail

  branching  = Curry.Module.FlatCurry.C_ConsDeclOr

  consKind (Curry.Module.FlatCurry.C_ConsDeclOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.FlatCurry.C_ConsDeclFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.FlatCurry.C_ConsDeclFail x) = x

  orRef (Curry.Module.FlatCurry.C_ConsDeclOr x _) = x

  branches (Curry.Module.FlatCurry.C_ConsDeclOr _ x) = x





instance BaseCurry Curry.Module.FlatCurry.C_TypeExpr where
  nf f (Curry.Module.FlatCurry.C_TVar x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_TVar(v1))(state1))(x1)(state0)
  nf f (Curry.Module.FlatCurry.C_FuncType x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.FlatCurry.C_FuncType(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.FlatCurry.C_TCons x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.FlatCurry.C_TCons(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.FlatCurry.C_TVar x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_TVar(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.FlatCurry.C_FuncType x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.FlatCurry.C_FuncType(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.FlatCurry.C_TCons x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.FlatCurry.C_TCons(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.FlatCurry.C_TypeExprOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.FlatCurry.C_TVar(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.FlatCurry.C_FuncType(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.FlatCurry.C_TCons(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.FlatCurry.C_TypeExprFail

  branching  = Curry.Module.FlatCurry.C_TypeExprOr

  consKind (Curry.Module.FlatCurry.C_TypeExprOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.FlatCurry.C_TypeExprFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.FlatCurry.C_TypeExprFail x) = x

  orRef (Curry.Module.FlatCurry.C_TypeExprOr x _) = x

  branches (Curry.Module.FlatCurry.C_TypeExprOr _ x) = x





instance BaseCurry Curry.Module.FlatCurry.C_OpDecl where
  nf f (Curry.Module.FlatCurry.C_Op x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.FlatCurry.C_Op(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.FlatCurry.C_Op x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.FlatCurry.C_Op(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.FlatCurry.C_OpDeclOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.FlatCurry.C_Op(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(3)

  failed  = Curry.Module.FlatCurry.C_OpDeclFail

  branching  = Curry.Module.FlatCurry.C_OpDeclOr

  consKind (Curry.Module.FlatCurry.C_OpDeclOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.FlatCurry.C_OpDeclFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.FlatCurry.C_OpDeclFail x) = x

  orRef (Curry.Module.FlatCurry.C_OpDeclOr x _) = x

  branches (Curry.Module.FlatCurry.C_OpDeclOr _ x) = x





instance BaseCurry Curry.Module.FlatCurry.C_Fixity where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.FlatCurry.C_FixityOr(Curry.RunTimeSystem.mkRef(r)(0)(i))([Curry.Module.FlatCurry.C_InfixOp,Curry.Module.FlatCurry.C_InfixlOp,Curry.Module.FlatCurry.C_InfixrOp]))(0)

  failed  = Curry.Module.FlatCurry.C_FixityFail

  branching  = Curry.Module.FlatCurry.C_FixityOr

  consKind (Curry.Module.FlatCurry.C_FixityOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.FlatCurry.C_FixityFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.FlatCurry.C_FixityFail x) = x

  orRef (Curry.Module.FlatCurry.C_FixityOr x _) = x

  branches (Curry.Module.FlatCurry.C_FixityOr _ x) = x





instance BaseCurry Curry.Module.FlatCurry.C_FuncDecl where
  nf f (Curry.Module.FlatCurry.C_Func x1 x2 x3 x4 x5) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> f(Curry.Module.FlatCurry.C_Func(v1)(v2)(v3)(v4)(v5))(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.FlatCurry.C_Func x1 x2 x3 x4 x5) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> f(Curry.Module.FlatCurry.C_Func(v1)(v2)(v3)(v4)(v5))(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.FlatCurry.C_FuncDeclOr(Curry.RunTimeSystem.mkRef(r)(5)(i))([Curry.Module.FlatCurry.C_Func(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(5)

  failed  = Curry.Module.FlatCurry.C_FuncDeclFail

  branching  = Curry.Module.FlatCurry.C_FuncDeclOr

  consKind (Curry.Module.FlatCurry.C_FuncDeclOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.FlatCurry.C_FuncDeclFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.FlatCurry.C_FuncDeclFail x) = x

  orRef (Curry.Module.FlatCurry.C_FuncDeclOr x _) = x

  branches (Curry.Module.FlatCurry.C_FuncDeclOr _ x) = x





instance BaseCurry Curry.Module.FlatCurry.C_Rule where
  nf f (Curry.Module.FlatCurry.C_Rule x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.FlatCurry.C_Rule(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.FlatCurry.C_External x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_External(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.FlatCurry.C_Rule x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.FlatCurry.C_Rule(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.FlatCurry.C_External x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_External(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.FlatCurry.C_RuleOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.FlatCurry.C_Rule(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.FlatCurry.C_External(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.FlatCurry.C_RuleFail

  branching  = Curry.Module.FlatCurry.C_RuleOr

  consKind (Curry.Module.FlatCurry.C_RuleOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.FlatCurry.C_RuleFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.FlatCurry.C_RuleFail x) = x

  orRef (Curry.Module.FlatCurry.C_RuleOr x _) = x

  branches (Curry.Module.FlatCurry.C_RuleOr _ x) = x





instance BaseCurry Curry.Module.FlatCurry.C_CaseType where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.FlatCurry.C_CaseTypeOr(Curry.RunTimeSystem.mkRef(r)(0)(i))([Curry.Module.FlatCurry.C_Rigid,Curry.Module.FlatCurry.C_Flex]))(0)

  failed  = Curry.Module.FlatCurry.C_CaseTypeFail

  branching  = Curry.Module.FlatCurry.C_CaseTypeOr

  consKind (Curry.Module.FlatCurry.C_CaseTypeOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.FlatCurry.C_CaseTypeFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.FlatCurry.C_CaseTypeFail x) = x

  orRef (Curry.Module.FlatCurry.C_CaseTypeOr x _) = x

  branches (Curry.Module.FlatCurry.C_CaseTypeOr _ x) = x





instance BaseCurry Curry.Module.FlatCurry.C_CombType where
  nf f (Curry.Module.FlatCurry.C_FuncPartCall x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_FuncPartCall(v1))(state1))(x1)(state0)
  nf f (Curry.Module.FlatCurry.C_ConsPartCall x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_ConsPartCall(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.FlatCurry.C_FuncPartCall x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_FuncPartCall(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.FlatCurry.C_ConsPartCall x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_ConsPartCall(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.FlatCurry.C_CombTypeOr(Curry.RunTimeSystem.mkRef(r)(1)(i))([Curry.Module.FlatCurry.C_FuncCall,Curry.Module.FlatCurry.C_ConsCall,Curry.Module.FlatCurry.C_FuncPartCall(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.FlatCurry.C_ConsPartCall(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(1)

  failed  = Curry.Module.FlatCurry.C_CombTypeFail

  branching  = Curry.Module.FlatCurry.C_CombTypeOr

  consKind (Curry.Module.FlatCurry.C_CombTypeOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.FlatCurry.C_CombTypeFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.FlatCurry.C_CombTypeFail x) = x

  orRef (Curry.Module.FlatCurry.C_CombTypeOr x _) = x

  branches (Curry.Module.FlatCurry.C_CombTypeOr _ x) = x





instance BaseCurry Curry.Module.FlatCurry.C_Expr where
  nf f (Curry.Module.FlatCurry.C_Var x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_Var(v1))(state1))(x1)(state0)
  nf f (Curry.Module.FlatCurry.C_Lit x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_Lit(v1))(state1))(x1)(state0)
  nf f (Curry.Module.FlatCurry.C_Comb x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.FlatCurry.C_Comb(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.FlatCurry.C_Let x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.FlatCurry.C_Let(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.FlatCurry.C_Free x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.FlatCurry.C_Free(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.FlatCurry.C_Or x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.FlatCurry.C_Or(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.FlatCurry.C_Case x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.FlatCurry.C_Case(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.FlatCurry.C_Var x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_Var(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.FlatCurry.C_Lit x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_Lit(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.FlatCurry.C_Comb x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.FlatCurry.C_Comb(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.FlatCurry.C_Let x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.FlatCurry.C_Let(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.FlatCurry.C_Free x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.FlatCurry.C_Free(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.FlatCurry.C_Or x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.FlatCurry.C_Or(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.FlatCurry.C_Case x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.FlatCurry.C_Case(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.FlatCurry.C_ExprOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.FlatCurry.C_Var(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.FlatCurry.C_Lit(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.FlatCurry.C_Comb(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.FlatCurry.C_Let(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.FlatCurry.C_Free(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.FlatCurry.C_Or(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.FlatCurry.C_Case(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(3)

  failed  = Curry.Module.FlatCurry.C_ExprFail

  branching  = Curry.Module.FlatCurry.C_ExprOr

  consKind (Curry.Module.FlatCurry.C_ExprOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.FlatCurry.C_ExprFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.FlatCurry.C_ExprFail x) = x

  orRef (Curry.Module.FlatCurry.C_ExprOr x _) = x

  branches (Curry.Module.FlatCurry.C_ExprOr _ x) = x





instance BaseCurry Curry.Module.FlatCurry.C_BranchExpr where
  nf f (Curry.Module.FlatCurry.C_Branch x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.FlatCurry.C_Branch(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.FlatCurry.C_Branch x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.FlatCurry.C_Branch(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.FlatCurry.C_BranchExprOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.FlatCurry.C_Branch(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.FlatCurry.C_BranchExprFail

  branching  = Curry.Module.FlatCurry.C_BranchExprOr

  consKind (Curry.Module.FlatCurry.C_BranchExprOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.FlatCurry.C_BranchExprFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.FlatCurry.C_BranchExprFail x) = x

  orRef (Curry.Module.FlatCurry.C_BranchExprOr x _) = x

  branches (Curry.Module.FlatCurry.C_BranchExprOr _ x) = x





instance BaseCurry Curry.Module.FlatCurry.C_Pattern where
  nf f (Curry.Module.FlatCurry.C_Pattern x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.FlatCurry.C_Pattern(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.FlatCurry.C_LPattern x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_LPattern(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.FlatCurry.C_Pattern x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.FlatCurry.C_Pattern(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.FlatCurry.C_LPattern x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_LPattern(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.FlatCurry.C_PatternOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.FlatCurry.C_Pattern(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.FlatCurry.C_LPattern(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.FlatCurry.C_PatternFail

  branching  = Curry.Module.FlatCurry.C_PatternOr

  consKind (Curry.Module.FlatCurry.C_PatternOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.FlatCurry.C_PatternFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.FlatCurry.C_PatternFail x) = x

  orRef (Curry.Module.FlatCurry.C_PatternOr x _) = x

  branches (Curry.Module.FlatCurry.C_PatternOr _ x) = x





instance BaseCurry Curry.Module.FlatCurry.C_Literal where
  nf f (Curry.Module.FlatCurry.C_Intc x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_Intc(v1))(state1))(x1)(state0)
  nf f (Curry.Module.FlatCurry.C_Floatc x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_Floatc(v1))(state1))(x1)(state0)
  nf f (Curry.Module.FlatCurry.C_Charc x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_Charc(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.FlatCurry.C_Intc x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_Intc(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.FlatCurry.C_Floatc x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_Floatc(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.FlatCurry.C_Charc x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.FlatCurry.C_Charc(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.FlatCurry.C_LiteralOr(Curry.RunTimeSystem.mkRef(r)(1)(i))([Curry.Module.FlatCurry.C_Intc(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.FlatCurry.C_Floatc(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.FlatCurry.C_Charc(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(1)

  failed  = Curry.Module.FlatCurry.C_LiteralFail

  branching  = Curry.Module.FlatCurry.C_LiteralOr

  consKind (Curry.Module.FlatCurry.C_LiteralOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.FlatCurry.C_LiteralFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.FlatCurry.C_LiteralFail x) = x

  orRef (Curry.Module.FlatCurry.C_LiteralOr x _) = x

  branches (Curry.Module.FlatCurry.C_LiteralOr _ x) = x





instance Curry Curry.Module.FlatCurry.C_Prog where
  strEq (Curry.Module.FlatCurry.C_Prog x1 x2 x3 x4 x5) (Curry.Module.FlatCurry.C_Prog y1 y2 y3 y4 y5) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.FlatCurry.C_Prog x1 x2 x3 x4 x5) (Curry.Module.FlatCurry.C_Prog y1 y2 y3 y4 y5) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.genEq(x5)(y5)(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.FlatCurry.C_Prog x1 x2 x3 x4 x5) st = Curry.Module.FlatCurry.C_Prog(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))

  foldCurry f c (Curry.Module.FlatCurry.C_Prog x1 x2 x3 x4 x5) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(c)(st))(st))(st))(st))(st)

  typeName _ = "Prog"

  showQ d (Curry.Module.FlatCurry.C_Prog x1 x2 x3 x4 x5) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.Prog "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x5))))))))))


  showQ _ (Curry.Module.FlatCurry.C_ProgOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.FlatCurry.C_Visibility where
  strEq Curry.Module.FlatCurry.C_Public Curry.Module.FlatCurry.C_Public st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.FlatCurry.C_Private Curry.Module.FlatCurry.C_Private st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.FlatCurry.C_Public Curry.Module.FlatCurry.C_Public st = Curry.Module.Prelude.C_True
  eq Curry.Module.FlatCurry.C_Private Curry.Module.FlatCurry.C_Private st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.FlatCurry.C_Public st = Curry.Module.FlatCurry.C_Public
  propagate f Curry.Module.FlatCurry.C_Private st = Curry.Module.FlatCurry.C_Private

  foldCurry f c Curry.Module.FlatCurry.C_Public st = c
  foldCurry f c Curry.Module.FlatCurry.C_Private st = c

  typeName _ = "Visibility"

  showQ _ Curry.Module.FlatCurry.C_Public = Prelude.showString("FlatCurry.Public")
  showQ _ Curry.Module.FlatCurry.C_Private = Prelude.showString("FlatCurry.Private")
  showQ _ (Curry.Module.FlatCurry.C_VisibilityOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.FlatCurry.C_TypeDecl where
  strEq (Curry.Module.FlatCurry.C_Type x1 x2 x3 x4) (Curry.Module.FlatCurry.C_Type y1 y2 y3 y4) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(st))(st))(st)
  strEq (Curry.Module.FlatCurry.C_TypeSyn x1 x2 x3 x4) (Curry.Module.FlatCurry.C_TypeSyn y1 y2 y3 y4) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.FlatCurry.C_Type x1 x2 x3 x4) (Curry.Module.FlatCurry.C_Type y1 y2 y3 y4) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.genEq(x4)(y4)(st))(st))(st))(st)
  eq (Curry.Module.FlatCurry.C_TypeSyn x1 x2 x3 x4) (Curry.Module.FlatCurry.C_TypeSyn y1 y2 y3 y4) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.genEq(x4)(y4)(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.FlatCurry.C_Type x1 x2 x3 x4) st = Curry.Module.FlatCurry.C_Type(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))
  propagate f (Curry.Module.FlatCurry.C_TypeSyn x1 x2 x3 x4) st = Curry.Module.FlatCurry.C_TypeSyn(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))

  foldCurry f c (Curry.Module.FlatCurry.C_Type x1 x2 x3 x4) st = f(x1)(f(x2)(f(x3)(f(x4)(c)(st))(st))(st))(st)
  foldCurry f c (Curry.Module.FlatCurry.C_TypeSyn x1 x2 x3 x4) st = f(x1)(f(x2)(f(x3)(f(x4)(c)(st))(st))(st))(st)

  typeName _ = "TypeDecl"

  showQ d (Curry.Module.FlatCurry.C_Type x1 x2 x3 x4) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.Type "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))))))))


  showQ d (Curry.Module.FlatCurry.C_TypeSyn x1 x2 x3 x4) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.TypeSyn "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))))))))


  showQ _ (Curry.Module.FlatCurry.C_TypeDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.FlatCurry.C_ConsDecl where
  strEq (Curry.Module.FlatCurry.C_Cons x1 x2 x3 x4) (Curry.Module.FlatCurry.C_Cons y1 y2 y3 y4) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.FlatCurry.C_Cons x1 x2 x3 x4) (Curry.Module.FlatCurry.C_Cons y1 y2 y3 y4) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.genEq(x4)(y4)(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.FlatCurry.C_Cons x1 x2 x3 x4) st = Curry.Module.FlatCurry.C_Cons(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))

  foldCurry f c (Curry.Module.FlatCurry.C_Cons x1 x2 x3 x4) st = f(x1)(f(x2)(f(x3)(f(x4)(c)(st))(st))(st))(st)

  typeName _ = "ConsDecl"

  showQ d (Curry.Module.FlatCurry.C_Cons x1 x2 x3 x4) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.Cons "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))))))))


  showQ _ (Curry.Module.FlatCurry.C_ConsDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.FlatCurry.C_TypeExpr where
  strEq (Curry.Module.FlatCurry.C_TVar x1) (Curry.Module.FlatCurry.C_TVar y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.FlatCurry.C_FuncType x1 x2) (Curry.Module.FlatCurry.C_FuncType y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.FlatCurry.C_TCons x1 x2) (Curry.Module.FlatCurry.C_TCons y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.FlatCurry.C_TVar x1) (Curry.Module.FlatCurry.C_TVar y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.FlatCurry.C_FuncType x1 x2) (Curry.Module.FlatCurry.C_FuncType y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.FlatCurry.C_TCons x1 x2) (Curry.Module.FlatCurry.C_TCons y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.FlatCurry.C_TVar x1) st = Curry.Module.FlatCurry.C_TVar(f((0::Int))(x1)(st))
  propagate f (Curry.Module.FlatCurry.C_FuncType x1 x2) st = Curry.Module.FlatCurry.C_FuncType(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.FlatCurry.C_TCons x1 x2) st = Curry.Module.FlatCurry.C_TCons(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.FlatCurry.C_TVar x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.FlatCurry.C_FuncType x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.FlatCurry.C_TCons x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "TypeExpr"

  showQ d (Curry.Module.FlatCurry.C_TVar x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.TVar "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.FlatCurry.C_FuncType x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.FuncType "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.FlatCurry.C_TCons x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.TCons "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.FlatCurry.C_TypeExprOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.FlatCurry.C_OpDecl where
  strEq (Curry.Module.FlatCurry.C_Op x1 x2 x3) (Curry.Module.FlatCurry.C_Op y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.FlatCurry.C_Op x1 x2 x3) (Curry.Module.FlatCurry.C_Op y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.FlatCurry.C_Op x1 x2 x3) st = Curry.Module.FlatCurry.C_Op(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))

  foldCurry f c (Curry.Module.FlatCurry.C_Op x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)

  typeName _ = "OpDecl"

  showQ d (Curry.Module.FlatCurry.C_Op x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.Op "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ _ (Curry.Module.FlatCurry.C_OpDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.FlatCurry.C_Fixity where
  strEq Curry.Module.FlatCurry.C_InfixOp Curry.Module.FlatCurry.C_InfixOp st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.FlatCurry.C_InfixlOp Curry.Module.FlatCurry.C_InfixlOp st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.FlatCurry.C_InfixrOp Curry.Module.FlatCurry.C_InfixrOp st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.FlatCurry.C_InfixOp Curry.Module.FlatCurry.C_InfixOp st = Curry.Module.Prelude.C_True
  eq Curry.Module.FlatCurry.C_InfixlOp Curry.Module.FlatCurry.C_InfixlOp st = Curry.Module.Prelude.C_True
  eq Curry.Module.FlatCurry.C_InfixrOp Curry.Module.FlatCurry.C_InfixrOp st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.FlatCurry.C_InfixOp st = Curry.Module.FlatCurry.C_InfixOp
  propagate f Curry.Module.FlatCurry.C_InfixlOp st = Curry.Module.FlatCurry.C_InfixlOp
  propagate f Curry.Module.FlatCurry.C_InfixrOp st = Curry.Module.FlatCurry.C_InfixrOp

  foldCurry f c Curry.Module.FlatCurry.C_InfixOp st = c
  foldCurry f c Curry.Module.FlatCurry.C_InfixlOp st = c
  foldCurry f c Curry.Module.FlatCurry.C_InfixrOp st = c

  typeName _ = "Fixity"

  showQ _ Curry.Module.FlatCurry.C_InfixOp = Prelude.showString("FlatCurry.InfixOp")
  showQ _ Curry.Module.FlatCurry.C_InfixlOp = Prelude.showString("FlatCurry.InfixlOp")
  showQ _ Curry.Module.FlatCurry.C_InfixrOp = Prelude.showString("FlatCurry.InfixrOp")
  showQ _ (Curry.Module.FlatCurry.C_FixityOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.FlatCurry.C_FuncDecl where
  strEq (Curry.Module.FlatCurry.C_Func x1 x2 x3 x4 x5) (Curry.Module.FlatCurry.C_Func y1 y2 y3 y4 y5) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.FlatCurry.C_Func x1 x2 x3 x4 x5) (Curry.Module.FlatCurry.C_Func y1 y2 y3 y4 y5) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.genEq(x5)(y5)(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.FlatCurry.C_Func x1 x2 x3 x4 x5) st = Curry.Module.FlatCurry.C_Func(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))

  foldCurry f c (Curry.Module.FlatCurry.C_Func x1 x2 x3 x4 x5) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(c)(st))(st))(st))(st))(st)

  typeName _ = "FuncDecl"

  showQ d (Curry.Module.FlatCurry.C_Func x1 x2 x3 x4 x5) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.Func "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x5))))))))))


  showQ _ (Curry.Module.FlatCurry.C_FuncDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.FlatCurry.C_Rule where
  strEq (Curry.Module.FlatCurry.C_Rule x1 x2) (Curry.Module.FlatCurry.C_Rule y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.FlatCurry.C_External x1) (Curry.Module.FlatCurry.C_External y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.FlatCurry.C_Rule x1 x2) (Curry.Module.FlatCurry.C_Rule y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.FlatCurry.C_External x1) (Curry.Module.FlatCurry.C_External y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.FlatCurry.C_Rule x1 x2) st = Curry.Module.FlatCurry.C_Rule(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.FlatCurry.C_External x1) st = Curry.Module.FlatCurry.C_External(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.FlatCurry.C_Rule x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.FlatCurry.C_External x1) st = f(x1)(c)(st)

  typeName _ = "Rule"

  showQ d (Curry.Module.FlatCurry.C_Rule x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.Rule "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.FlatCurry.C_External x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.External "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.FlatCurry.C_RuleOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.FlatCurry.C_CaseType where
  strEq Curry.Module.FlatCurry.C_Rigid Curry.Module.FlatCurry.C_Rigid st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.FlatCurry.C_Flex Curry.Module.FlatCurry.C_Flex st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.FlatCurry.C_Rigid Curry.Module.FlatCurry.C_Rigid st = Curry.Module.Prelude.C_True
  eq Curry.Module.FlatCurry.C_Flex Curry.Module.FlatCurry.C_Flex st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.FlatCurry.C_Rigid st = Curry.Module.FlatCurry.C_Rigid
  propagate f Curry.Module.FlatCurry.C_Flex st = Curry.Module.FlatCurry.C_Flex

  foldCurry f c Curry.Module.FlatCurry.C_Rigid st = c
  foldCurry f c Curry.Module.FlatCurry.C_Flex st = c

  typeName _ = "CaseType"

  showQ _ Curry.Module.FlatCurry.C_Rigid = Prelude.showString("FlatCurry.Rigid")
  showQ _ Curry.Module.FlatCurry.C_Flex = Prelude.showString("FlatCurry.Flex")
  showQ _ (Curry.Module.FlatCurry.C_CaseTypeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.FlatCurry.C_CombType where
  strEq Curry.Module.FlatCurry.C_FuncCall Curry.Module.FlatCurry.C_FuncCall st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.FlatCurry.C_ConsCall Curry.Module.FlatCurry.C_ConsCall st = Curry.Module.Prelude.strEqSuccess
  strEq (Curry.Module.FlatCurry.C_FuncPartCall x1) (Curry.Module.FlatCurry.C_FuncPartCall y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.FlatCurry.C_ConsPartCall x1) (Curry.Module.FlatCurry.C_ConsPartCall y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.FlatCurry.C_FuncCall Curry.Module.FlatCurry.C_FuncCall st = Curry.Module.Prelude.C_True
  eq Curry.Module.FlatCurry.C_ConsCall Curry.Module.FlatCurry.C_ConsCall st = Curry.Module.Prelude.C_True
  eq (Curry.Module.FlatCurry.C_FuncPartCall x1) (Curry.Module.FlatCurry.C_FuncPartCall y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.FlatCurry.C_ConsPartCall x1) (Curry.Module.FlatCurry.C_ConsPartCall y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.FlatCurry.C_FuncCall st = Curry.Module.FlatCurry.C_FuncCall
  propagate f Curry.Module.FlatCurry.C_ConsCall st = Curry.Module.FlatCurry.C_ConsCall
  propagate f (Curry.Module.FlatCurry.C_FuncPartCall x1) st = Curry.Module.FlatCurry.C_FuncPartCall(f((0::Int))(x1)(st))
  propagate f (Curry.Module.FlatCurry.C_ConsPartCall x1) st = Curry.Module.FlatCurry.C_ConsPartCall(f((0::Int))(x1)(st))

  foldCurry f c Curry.Module.FlatCurry.C_FuncCall st = c
  foldCurry f c Curry.Module.FlatCurry.C_ConsCall st = c
  foldCurry f c (Curry.Module.FlatCurry.C_FuncPartCall x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.FlatCurry.C_ConsPartCall x1) st = f(x1)(c)(st)

  typeName _ = "CombType"

  showQ _ Curry.Module.FlatCurry.C_FuncCall = Prelude.showString("FlatCurry.FuncCall")
  showQ _ Curry.Module.FlatCurry.C_ConsCall = Prelude.showString("FlatCurry.ConsCall")
  showQ d (Curry.Module.FlatCurry.C_FuncPartCall x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.FuncPartCall "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.FlatCurry.C_ConsPartCall x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.ConsPartCall "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.FlatCurry.C_CombTypeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.FlatCurry.C_Expr where
  strEq (Curry.Module.FlatCurry.C_Var x1) (Curry.Module.FlatCurry.C_Var y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.FlatCurry.C_Lit x1) (Curry.Module.FlatCurry.C_Lit y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.FlatCurry.C_Comb x1 x2 x3) (Curry.Module.FlatCurry.C_Comb y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq (Curry.Module.FlatCurry.C_Let x1 x2) (Curry.Module.FlatCurry.C_Let y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.FlatCurry.C_Free x1 x2) (Curry.Module.FlatCurry.C_Free y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.FlatCurry.C_Or x1 x2) (Curry.Module.FlatCurry.C_Or y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.FlatCurry.C_Case x1 x2 x3) (Curry.Module.FlatCurry.C_Case y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.FlatCurry.C_Var x1) (Curry.Module.FlatCurry.C_Var y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.FlatCurry.C_Lit x1) (Curry.Module.FlatCurry.C_Lit y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.FlatCurry.C_Comb x1 x2 x3) (Curry.Module.FlatCurry.C_Comb y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq (Curry.Module.FlatCurry.C_Let x1 x2) (Curry.Module.FlatCurry.C_Let y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.FlatCurry.C_Free x1 x2) (Curry.Module.FlatCurry.C_Free y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.FlatCurry.C_Or x1 x2) (Curry.Module.FlatCurry.C_Or y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.FlatCurry.C_Case x1 x2 x3) (Curry.Module.FlatCurry.C_Case y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.FlatCurry.C_Var x1) st = Curry.Module.FlatCurry.C_Var(f((0::Int))(x1)(st))
  propagate f (Curry.Module.FlatCurry.C_Lit x1) st = Curry.Module.FlatCurry.C_Lit(f((0::Int))(x1)(st))
  propagate f (Curry.Module.FlatCurry.C_Comb x1 x2 x3) st = Curry.Module.FlatCurry.C_Comb(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))
  propagate f (Curry.Module.FlatCurry.C_Let x1 x2) st = Curry.Module.FlatCurry.C_Let(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.FlatCurry.C_Free x1 x2) st = Curry.Module.FlatCurry.C_Free(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.FlatCurry.C_Or x1 x2) st = Curry.Module.FlatCurry.C_Or(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.FlatCurry.C_Case x1 x2 x3) st = Curry.Module.FlatCurry.C_Case(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))

  foldCurry f c (Curry.Module.FlatCurry.C_Var x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.FlatCurry.C_Lit x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.FlatCurry.C_Comb x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)
  foldCurry f c (Curry.Module.FlatCurry.C_Let x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.FlatCurry.C_Free x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.FlatCurry.C_Or x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.FlatCurry.C_Case x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)

  typeName _ = "Expr"

  showQ d (Curry.Module.FlatCurry.C_Var x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.Var "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.FlatCurry.C_Lit x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.Lit "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.FlatCurry.C_Comb x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.Comb "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ d (Curry.Module.FlatCurry.C_Let x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.Let "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.FlatCurry.C_Free x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.Free "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.FlatCurry.C_Or x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.Or "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.FlatCurry.C_Case x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.Case "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ _ (Curry.Module.FlatCurry.C_ExprOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.FlatCurry.C_BranchExpr where
  strEq (Curry.Module.FlatCurry.C_Branch x1 x2) (Curry.Module.FlatCurry.C_Branch y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.FlatCurry.C_Branch x1 x2) (Curry.Module.FlatCurry.C_Branch y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.FlatCurry.C_Branch x1 x2) st = Curry.Module.FlatCurry.C_Branch(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.FlatCurry.C_Branch x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "BranchExpr"

  showQ d (Curry.Module.FlatCurry.C_Branch x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.Branch "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.FlatCurry.C_BranchExprOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.FlatCurry.C_Pattern where
  strEq (Curry.Module.FlatCurry.C_Pattern x1 x2) (Curry.Module.FlatCurry.C_Pattern y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.FlatCurry.C_LPattern x1) (Curry.Module.FlatCurry.C_LPattern y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.FlatCurry.C_Pattern x1 x2) (Curry.Module.FlatCurry.C_Pattern y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.FlatCurry.C_LPattern x1) (Curry.Module.FlatCurry.C_LPattern y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.FlatCurry.C_Pattern x1 x2) st = Curry.Module.FlatCurry.C_Pattern(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.FlatCurry.C_LPattern x1) st = Curry.Module.FlatCurry.C_LPattern(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.FlatCurry.C_Pattern x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.FlatCurry.C_LPattern x1) st = f(x1)(c)(st)

  typeName _ = "Pattern"

  showQ d (Curry.Module.FlatCurry.C_Pattern x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.Pattern "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.FlatCurry.C_LPattern x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.LPattern "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.FlatCurry.C_PatternOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.FlatCurry.C_Literal where
  strEq (Curry.Module.FlatCurry.C_Intc x1) (Curry.Module.FlatCurry.C_Intc y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.FlatCurry.C_Floatc x1) (Curry.Module.FlatCurry.C_Floatc y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.FlatCurry.C_Charc x1) (Curry.Module.FlatCurry.C_Charc y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.FlatCurry.C_Intc x1) (Curry.Module.FlatCurry.C_Intc y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.FlatCurry.C_Floatc x1) (Curry.Module.FlatCurry.C_Floatc y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.FlatCurry.C_Charc x1) (Curry.Module.FlatCurry.C_Charc y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.FlatCurry.C_Intc x1) st = Curry.Module.FlatCurry.C_Intc(f((0::Int))(x1)(st))
  propagate f (Curry.Module.FlatCurry.C_Floatc x1) st = Curry.Module.FlatCurry.C_Floatc(f((0::Int))(x1)(st))
  propagate f (Curry.Module.FlatCurry.C_Charc x1) st = Curry.Module.FlatCurry.C_Charc(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.FlatCurry.C_Intc x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.FlatCurry.C_Floatc x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.FlatCurry.C_Charc x1) st = f(x1)(c)(st)

  typeName _ = "Literal"

  showQ d (Curry.Module.FlatCurry.C_Intc x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.Intc "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.FlatCurry.C_Floatc x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.Floatc "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.FlatCurry.C_Charc x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatCurry.Charc "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.FlatCurry.C_LiteralOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.FlatCurry.C_Prog where
  showsPrec d (Curry.Module.FlatCurry.C_Prog x1 x2 x3 x4 x5) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Prog "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x5))))))))))


  showsPrec _ (Curry.Module.FlatCurry.C_ProgOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.FlatCurry.C_Visibility where
  showsPrec _ Curry.Module.FlatCurry.C_Public = Prelude.showString("Public")
  showsPrec _ Curry.Module.FlatCurry.C_Private = Prelude.showString("Private")
  showsPrec _ (Curry.Module.FlatCurry.C_VisibilityOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.FlatCurry.C_TypeDecl where
  showsPrec d (Curry.Module.FlatCurry.C_Type x1 x2 x3 x4) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Type "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))))))))


  showsPrec d (Curry.Module.FlatCurry.C_TypeSyn x1 x2 x3 x4) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("TypeSyn "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))))))))


  showsPrec _ (Curry.Module.FlatCurry.C_TypeDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.FlatCurry.C_ConsDecl where
  showsPrec d (Curry.Module.FlatCurry.C_Cons x1 x2 x3 x4) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Cons "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))))))))


  showsPrec _ (Curry.Module.FlatCurry.C_ConsDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.FlatCurry.C_TypeExpr where
  showsPrec d (Curry.Module.FlatCurry.C_TVar x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("TVar "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.FlatCurry.C_FuncType x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FuncType "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.FlatCurry.C_TCons x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("TCons "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.FlatCurry.C_TypeExprOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.FlatCurry.C_OpDecl where
  showsPrec d (Curry.Module.FlatCurry.C_Op x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Op "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec _ (Curry.Module.FlatCurry.C_OpDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.FlatCurry.C_Fixity where
  showsPrec _ Curry.Module.FlatCurry.C_InfixOp = Prelude.showString("InfixOp")
  showsPrec _ Curry.Module.FlatCurry.C_InfixlOp = Prelude.showString("InfixlOp")
  showsPrec _ Curry.Module.FlatCurry.C_InfixrOp = Prelude.showString("InfixrOp")
  showsPrec _ (Curry.Module.FlatCurry.C_FixityOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.FlatCurry.C_FuncDecl where
  showsPrec d (Curry.Module.FlatCurry.C_Func x1 x2 x3 x4 x5) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Func "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x5))))))))))


  showsPrec _ (Curry.Module.FlatCurry.C_FuncDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.FlatCurry.C_Rule where
  showsPrec d (Curry.Module.FlatCurry.C_Rule x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Rule "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.FlatCurry.C_External x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("External "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.FlatCurry.C_RuleOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.FlatCurry.C_CaseType where
  showsPrec _ Curry.Module.FlatCurry.C_Rigid = Prelude.showString("Rigid")
  showsPrec _ Curry.Module.FlatCurry.C_Flex = Prelude.showString("Flex")
  showsPrec _ (Curry.Module.FlatCurry.C_CaseTypeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.FlatCurry.C_CombType where
  showsPrec _ Curry.Module.FlatCurry.C_FuncCall = Prelude.showString("FuncCall")
  showsPrec _ Curry.Module.FlatCurry.C_ConsCall = Prelude.showString("ConsCall")
  showsPrec d (Curry.Module.FlatCurry.C_FuncPartCall x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FuncPartCall "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.FlatCurry.C_ConsPartCall x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ConsPartCall "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.FlatCurry.C_CombTypeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.FlatCurry.C_Expr where
  showsPrec d (Curry.Module.FlatCurry.C_Var x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Var "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.FlatCurry.C_Lit x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Lit "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.FlatCurry.C_Comb x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Comb "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec d (Curry.Module.FlatCurry.C_Let x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Let "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.FlatCurry.C_Free x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Free "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.FlatCurry.C_Or x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Or "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.FlatCurry.C_Case x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Case "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec _ (Curry.Module.FlatCurry.C_ExprOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.FlatCurry.C_BranchExpr where
  showsPrec d (Curry.Module.FlatCurry.C_Branch x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Branch "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.FlatCurry.C_BranchExprOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.FlatCurry.C_Pattern where
  showsPrec d (Curry.Module.FlatCurry.C_Pattern x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Pattern "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.FlatCurry.C_LPattern x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("LPattern "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.FlatCurry.C_PatternOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.FlatCurry.C_Literal where
  showsPrec d (Curry.Module.FlatCurry.C_Intc x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Intc "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.FlatCurry.C_Floatc x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Floatc "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.FlatCurry.C_Charc x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Charc "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.FlatCurry.C_LiteralOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Read Curry.Module.FlatCurry.C_Prog where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_Prog(x1)(x2)(x3)(x4)(x5))(r5) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Prog")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3), ((,) x5 r5) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r4)])(r)





instance Read Curry.Module.FlatCurry.C_Visibility where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.FlatCurry.C_Public)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Public")(r)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.FlatCurry.C_Private)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Private")(r)])(r))





instance Read Curry.Module.FlatCurry.C_TypeDecl where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_Type(x1)(x2)(x3)(x4))(r4) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Type")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_TypeSyn(x1)(x2)(x3)(x4))(r4) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("TypeSyn")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3)])(r))





instance Read Curry.Module.FlatCurry.C_ConsDecl where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_Cons(x1)(x2)(x3)(x4))(r4) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Cons")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3)])(r)





instance Read Curry.Module.FlatCurry.C_TypeExpr where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_TVar(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("TVar")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_FuncType(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("FuncType")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_TCons(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("TCons")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r)))





instance Read Curry.Module.FlatCurry.C_OpDecl where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_Op(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Op")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r)





instance Read Curry.Module.FlatCurry.C_Fixity where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.FlatCurry.C_InfixOp)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("InfixOp")(r)])(r))((Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.FlatCurry.C_InfixlOp)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("InfixlOp")(r)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.FlatCurry.C_InfixrOp)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("InfixrOp")(r)])(r)))





instance Read Curry.Module.FlatCurry.C_FuncDecl where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_Func(x1)(x2)(x3)(x4)(x5))(r5) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Func")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3), ((,) x5 r5) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r4)])(r)





instance Read Curry.Module.FlatCurry.C_Rule where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_Rule(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Rule")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_External(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("External")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))





instance Read Curry.Module.FlatCurry.C_CaseType where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.FlatCurry.C_Rigid)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Rigid")(r)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.FlatCurry.C_Flex)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Flex")(r)])(r))





instance Read Curry.Module.FlatCurry.C_CombType where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.FlatCurry.C_FuncCall)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("FuncCall")(r)])(r))((Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.FlatCurry.C_ConsCall)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("ConsCall")(r)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_FuncPartCall(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("FuncPartCall")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_ConsPartCall(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("ConsPartCall")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))))





instance Read Curry.Module.FlatCurry.C_Expr where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_Var(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Var")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_Lit(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Lit")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_Comb(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Comb")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_Let(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Let")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_Free(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Free")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_Or(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Or")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_Case(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Case")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r)))))))





instance Read Curry.Module.FlatCurry.C_BranchExpr where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_Branch(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Branch")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r)





instance Read Curry.Module.FlatCurry.C_Pattern where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_Pattern(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Pattern")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_LPattern(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("LPattern")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))





instance Read Curry.Module.FlatCurry.C_Literal where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_Intc(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Intc")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_Floatc(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Floatc")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.FlatCurry.C_Charc(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlatCurry")("Charc")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r)))





c_readFlatCurry :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_readFlatCurry x1 st = Curry.Module.FlatCurry.c_readFlatCurryWithParseOptions(x1)(Curry.Module.Distribution.c_setQuiet(Curry.Module.Prelude.C_True)(Curry.Module.Distribution.c_defaultParams(st))(st))(st)



c_readFlatCurryWithParseOptions :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Distribution.C_FrontendParams -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_readFlatCurryWithParseOptions x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_lookupFileInLoadPath(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurry.c_readFlatCurryWithParseOptions'46_'35lambda2(x2)(x1)))(st)



c_readFlatCurryWithParseOptions'46_'35lambda2 :: Curry.Module.Distribution.C_FrontendParams -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_readFlatCurryWithParseOptions'46_'35lambda2 x1 x2 x3 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_lookupFileInLoadPath(Curry.Module.Prelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurry.c_readFlatCurryWithParseOptions'46_'35lambda2'46_'35lambda3(x3)(x1)(x2)))(st)



c_readFlatCurryWithParseOptions'46_'35lambda2'46_'35lambda3 :: (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.Distribution.C_FrontendParams -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_readFlatCurryWithParseOptions'46_'35lambda2'46_'35lambda3 x1 x2 x3 x4 st = Curry.Module.Prelude.op_62_62(Curry.Module.FlatCurry.c_readFlatCurryWithParseOptions'46_'35lambda2'46_'35lambda3_case_2(x1)(x2)(x3)(x4)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Nothing)(st))(Curry.Module.Prelude.op_61_61(x4)(Curry.Module.Prelude.C_Nothing)(st))(st))(st))(Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_findFileInLoadPath(Curry.Module.Prelude.op_43_43(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurry.c_readFlatCurryWithParseOptions'46_'35lambda2'46_'35lambda3'46_'35lambda4))(st))(st)



c_readFlatCurryWithParseOptions'46_'35lambda2'46_'35lambda3'46_'35lambda4 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_readFlatCurryWithParseOptions'46_'35lambda2'46_'35lambda3'46_'35lambda4 x1 st = Curry.Module.FlatCurry.c_readFlatCurryFile(x1)(st)



c_flatCurryFileName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_flatCurryFileName x1 st = Curry.Module.Distribution.c_inCurrySubdir(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_apply(Curry.Module.FileGoodies.c_stripSuffix(st))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))(st))(st)



c_flatCurryIntName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_flatCurryIntName x1 st = Curry.Module.Distribution.c_inCurrySubdir(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_apply(Curry.Module.FileGoodies.c_stripSuffix(st))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))))(st))(st)



c_readFlatCurryFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_readFlatCurryFile x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesFileExist(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurry.c_readFlatCurryFile'46_'35lambda6(x1)))(st)



c_readFlatCurryFile'46readExistingFCY'4614 :: (Curry t0) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_readFlatCurryFile'46readExistingFCY'4614 x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_readFile(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurry.c_readFlatCurryFile'46readExistingFCY'4614'46_'35lambda5))(st)



c_readFlatCurryFile'46readExistingFCY'4614'46_'35lambda5 :: (Curry t1) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1
c_readFlatCurryFile'46readExistingFCY'4614'46_'35lambda5 x1 st = Curry.Module.Prelude.c_return(Curry.Module.ReadShowTerm.c_readTerm(x1)(st))(st)



c_readFlatCurryFile'46_'35lambda6 :: (Curry t2) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t2
c_readFlatCurryFile'46_'35lambda6 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.FlatCurry.c_readFlatCurryFile'46readExistingFCY'4614(x1)(st)
c_readFlatCurryFile'46_'35lambda6 x1 x2@Curry.Module.Prelude.C_False st = let {x3 = Curry.Module.Distribution.c_inCurrySubdir(x1)(st)} in Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesFileExist(x3)(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurry.c_readFlatCurryFile'46_'35lambda6'46_'35lambda7(x1)(x3)))(st)
c_readFlatCurryFile'46_'35lambda6 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatCurry.c_readFlatCurryFile'46_'35lambda6(x1)(x)(st))(i)(xs)(st)
c_readFlatCurryFile'46_'35lambda6 x1 x st = Curry.RunTimeSystem.patternFail("FlatCurry.readFlatCurryFile._#lambda6")(x)



c_readFlatCurryFile'46_'35lambda6'46_'35lambda7 :: (Curry t3) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t3
c_readFlatCurryFile'46_'35lambda6'46_'35lambda7 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.FlatCurry.c_readFlatCurryFile'46readExistingFCY'4614(x2)(st)
c_readFlatCurryFile'46_'35lambda6'46_'35lambda7 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_error(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('X'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))))))))))))))(st))(st))(st)
c_readFlatCurryFile'46_'35lambda6'46_'35lambda7 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatCurry.c_readFlatCurryFile'46_'35lambda6'46_'35lambda7(x1)(x2)(x)(st))(i)(xs)(st)
c_readFlatCurryFile'46_'35lambda6'46_'35lambda7 x1 x2 x st = Curry.RunTimeSystem.patternFail("FlatCurry.readFlatCurryFile._#lambda6._#lambda7")(x)



c_readFlatCurryInt :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_readFlatCurryInt x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesFileExist(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurry.c_readFlatCurryInt'46_'35lambda8(x1)))(st)



c_readFlatCurryInt'46_'35lambda8 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_readFlatCurryInt'46_'35lambda8 x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesFileExist(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurry.c_readFlatCurryInt'46_'35lambda8'46_'35lambda9(x2)(x1)))(st)



c_readFlatCurryInt'46_'35lambda8'46_'35lambda9 :: Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_readFlatCurryInt'46_'35lambda8'46_'35lambda9 x1 x2 x3 st = Curry.Module.Prelude.op_62_62(Curry.Module.FlatCurry.c_readFlatCurryInt'46_'35lambda8'46_'35lambda9_case_1(x1)(x2)(x3)(Curry.Module.Prelude.op_124_124(x1)(x3)(st))(st))(Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_findFileInLoadPath(Curry.Module.Prelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurry.c_readFlatCurryInt'46_'35lambda8'46_'35lambda9'46_'35lambda10))(st))(st)



c_readFlatCurryInt'46_'35lambda8'46_'35lambda9'46_'35lambda10 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_readFlatCurryInt'46_'35lambda8'46_'35lambda9'46_'35lambda10 x1 st = Curry.Module.FlatCurry.c_readFlatCurryFile(x1)(st)



c_writeFCY :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_writeFCY x1 x2 st = Curry.Module.Prelude.c_writeFile(x1)(Curry.Module.ReadShowTerm.c_showTerm(x2)(st))(st)



c_showQNameInModule :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showQNameInModule x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.FlatCurry.c_showQNameInModule_case_0(x1)(x3)(x4)(Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_61_61(x3)(x1)(st))(Curry.Module.Prelude.op_61_61(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(st))(st))(st)
c_showQNameInModule x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatCurry.c_showQNameInModule(x1)(x)(st))(i)(xs)(st)
c_showQNameInModule x1 x st = Curry.RunTimeSystem.patternFail("FlatCurry.showQNameInModule")(x)



c_showQNameInModule_case_0 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = x4
c_showQNameInModule_case_0 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_43_43(x3)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List))(x4)(st))(st)
c_showQNameInModule_case_0 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatCurry.c_showQNameInModule_case_0(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_showQNameInModule_case_0 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("FlatCurry.showQNameInModule_case_0")(x)



c_readFlatCurryInt'46_'35lambda8'46_'35lambda9_case_1 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_apply(Curry.Module.Distribution.c_callFrontend(Curry.Module.Distribution.C_FINT)(st))(x2)(st)
c_readFlatCurryInt'46_'35lambda8'46_'35lambda9_case_1 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_done(st)
c_readFlatCurryInt'46_'35lambda8'46_'35lambda9_case_1 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatCurry.c_readFlatCurryInt'46_'35lambda8'46_'35lambda9_case_1(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_readFlatCurryInt'46_'35lambda8'46_'35lambda9_case_1 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("FlatCurry.readFlatCurryInt._#lambda8._#lambda9_case_1")(x)



c_readFlatCurryWithParseOptions'46_'35lambda2'46_'35lambda3_case_2 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_done(st)
c_readFlatCurryWithParseOptions'46_'35lambda2'46_'35lambda3_case_2 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Distribution.c_callFrontendWithParams(Curry.Module.Distribution.C_FCY)(x2)(x3)(st)
c_readFlatCurryWithParseOptions'46_'35lambda2'46_'35lambda3_case_2 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatCurry.c_readFlatCurryWithParseOptions'46_'35lambda2'46_'35lambda3_case_2(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_readFlatCurryWithParseOptions'46_'35lambda2'46_'35lambda3_case_2 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("FlatCurry.readFlatCurryWithParseOptions._#lambda2._#lambda3_case_2")(x)


