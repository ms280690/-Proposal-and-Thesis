{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.CurrySyntax (module Curry.Module.CurrySyntax) where

import Curry.RunTimeSystem
import Curry.Module.Directory
import Curry.Module.Distribution
import Curry.Module.FileGoodies
import Curry.Module.Prelude
import Curry.Module.ReadShowTerm



-- begin included



-- end included

type C_Pos = Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int

type C_ModuleIdent = Curry.Module.Prelude.List Curry.Module.Prelude.C_Char

data C_Position = C_Position (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int
  | C_PositionFail Curry.RunTimeSystem.C_Exceptions
  | C_PositionOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.CurrySyntax.C_Position)

data C_Ident = C_Ident (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.C_Int
  | C_IdentFail Curry.RunTimeSystem.C_Exceptions
  | C_IdentOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.CurrySyntax.C_Ident)

data C_QualIdent = C_UnqualIdent Curry.Module.CurrySyntax.C_Ident
  | C_QualIdent (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.CurrySyntax.C_Ident
  | C_QualIdentFail Curry.RunTimeSystem.C_Exceptions
  | C_QualIdentOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.CurrySyntax.C_QualIdent)

data C_Module t0 = C_Module (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.C_Maybe (Curry.Module.CurrySyntax.C_ExportSpec t0)) (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_Decl t0))
  | C_ModuleFail Curry.RunTimeSystem.C_Exceptions
  | C_ModuleOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.CurrySyntax.C_Module t0))

data C_ExportSpec t0 = C_Exporting t0 (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_Export)
  | C_ExportSpecFail Curry.RunTimeSystem.C_Exceptions
  | C_ExportSpecOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.CurrySyntax.C_ExportSpec t0))

data C_Export = C_Export Curry.Module.CurrySyntax.C_QualIdent
  | C_ExportTypeWith Curry.Module.CurrySyntax.C_QualIdent (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_Ident)
  | C_ExportTypeAll Curry.Module.CurrySyntax.C_QualIdent
  | C_ExportModule (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_ExportFail Curry.RunTimeSystem.C_Exceptions
  | C_ExportOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.CurrySyntax.C_Export)

data C_ImportSpec t0 = C_Importing t0 (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_Import)
  | C_Hiding t0 (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_Import)
  | C_ImportSpecFail Curry.RunTimeSystem.C_Exceptions
  | C_ImportSpecOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.CurrySyntax.C_ImportSpec t0))

data C_Import = C_Import Curry.Module.CurrySyntax.C_Ident
  | C_ImportTypeWith Curry.Module.CurrySyntax.C_Ident (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_Ident)
  | C_ImportTypeAll Curry.Module.CurrySyntax.C_Ident
  | C_ImportFail Curry.RunTimeSystem.C_Exceptions
  | C_ImportOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.CurrySyntax.C_Import)

data C_Decl t0 = C_ImportDecl t0 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.C_Bool (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.C_Maybe (Curry.Module.CurrySyntax.C_ImportSpec t0))
  | C_InfixDecl t0 Curry.Module.CurrySyntax.C_Infix Curry.Module.Prelude.C_Int (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_Ident)
  | C_DataDecl t0 Curry.Module.CurrySyntax.C_Ident (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_Ident) (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_ConstrDecl t0))
  | C_NewtypeDecl t0 Curry.Module.CurrySyntax.C_Ident (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_Ident) (Curry.Module.CurrySyntax.C_NewConstrDecl t0)
  | C_TypeDecl t0 Curry.Module.CurrySyntax.C_Ident (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_Ident) Curry.Module.CurrySyntax.C_TypeExpr
  | C_TypeSig t0 (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_Ident) Curry.Module.CurrySyntax.C_TypeExpr
  | C_EvalAnnot t0 (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_Ident) Curry.Module.CurrySyntax.C_EvalAnnotation
  | C_FunctionDecl t0 Curry.Module.CurrySyntax.C_Ident (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_Equation t0))
  | C_ExternalDecl t0 Curry.Module.CurrySyntax.C_CallConv (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.CurrySyntax.C_Ident Curry.Module.CurrySyntax.C_TypeExpr
  | C_FlatExternalDecl t0 (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_Ident)
  | C_PatternDecl t0 (Curry.Module.CurrySyntax.C_ConstrTerm t0) (Curry.Module.CurrySyntax.C_Rhs t0)
  | C_ExtraVariables t0 (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_Ident)
  | C_DeclFail Curry.RunTimeSystem.C_Exceptions
  | C_DeclOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.CurrySyntax.C_Decl t0))

data C_ConstrDecl t0 = C_ConstrDecl t0 (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_Ident) Curry.Module.CurrySyntax.C_Ident (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_TypeExpr)
  | C_ConOpDecl t0 (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_Ident) Curry.Module.CurrySyntax.C_TypeExpr Curry.Module.CurrySyntax.C_Ident Curry.Module.CurrySyntax.C_TypeExpr
  | C_ConstrDeclFail Curry.RunTimeSystem.C_Exceptions
  | C_ConstrDeclOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.CurrySyntax.C_ConstrDecl t0))

data C_NewConstrDecl t0 = C_NewConstrDecl t0 (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_Ident) Curry.Module.CurrySyntax.C_Ident Curry.Module.CurrySyntax.C_TypeExpr
  | C_NewConstrDeclFail Curry.RunTimeSystem.C_Exceptions
  | C_NewConstrDeclOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.CurrySyntax.C_NewConstrDecl t0))

data C_Infix = C_InfixL
  | C_InfixR
  | C_Infix
  | C_InfixFail Curry.RunTimeSystem.C_Exceptions
  | C_InfixOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.CurrySyntax.C_Infix)

data C_EvalAnnotation = C_EvalRigid
  | C_EvalChoice
  | C_EvalAnnotationFail Curry.RunTimeSystem.C_Exceptions
  | C_EvalAnnotationOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.CurrySyntax.C_EvalAnnotation)

data C_CallConv = C_CallConvPrimitive
  | C_CallConvCCall
  | C_CallConvFail Curry.RunTimeSystem.C_Exceptions
  | C_CallConvOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.CurrySyntax.C_CallConv)

data C_TypeExpr = C_ConstructorType Curry.Module.CurrySyntax.C_QualIdent (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_TypeExpr)
  | C_VariableType Curry.Module.CurrySyntax.C_Ident
  | C_TupleType (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_TypeExpr)
  | C_ListType Curry.Module.CurrySyntax.C_TypeExpr
  | C_ArrowType Curry.Module.CurrySyntax.C_TypeExpr Curry.Module.CurrySyntax.C_TypeExpr
  | C_RecordType (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.CurrySyntax.C_Ident) Curry.Module.CurrySyntax.C_TypeExpr)) (Curry.Module.Prelude.C_Maybe Curry.Module.CurrySyntax.C_TypeExpr)
  | C_TypeExprFail Curry.RunTimeSystem.C_Exceptions
  | C_TypeExprOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.CurrySyntax.C_TypeExpr)

data C_Equation t0 = C_Equation t0 (Curry.Module.CurrySyntax.C_Lhs t0) (Curry.Module.CurrySyntax.C_Rhs t0)
  | C_EquationFail Curry.RunTimeSystem.C_Exceptions
  | C_EquationOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.CurrySyntax.C_Equation t0))

data C_Lhs t0 = C_FunLhs Curry.Module.CurrySyntax.C_Ident (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_ConstrTerm t0))
  | C_OpLhs (Curry.Module.CurrySyntax.C_ConstrTerm t0) Curry.Module.CurrySyntax.C_Ident (Curry.Module.CurrySyntax.C_ConstrTerm t0)
  | C_ApLhs (Curry.Module.CurrySyntax.C_Lhs t0) (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_ConstrTerm t0))
  | C_LhsFail Curry.RunTimeSystem.C_Exceptions
  | C_LhsOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.CurrySyntax.C_Lhs t0))

data C_Rhs t0 = C_SimpleRhs t0 (Curry.Module.CurrySyntax.C_Expression t0) (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_Decl t0))
  | C_GuardedRhs (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_CondExpr t0)) (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_Decl t0))
  | C_RhsFail Curry.RunTimeSystem.C_Exceptions
  | C_RhsOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.CurrySyntax.C_Rhs t0))

data C_CondExpr t0 = C_CondExpr t0 (Curry.Module.CurrySyntax.C_Expression t0) (Curry.Module.CurrySyntax.C_Expression t0)
  | C_CondExprFail Curry.RunTimeSystem.C_Exceptions
  | C_CondExprOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.CurrySyntax.C_CondExpr t0))

data C_Literal = C_Char Curry.Module.Prelude.C_Char
  | C_Int Curry.Module.CurrySyntax.C_Ident Curry.Module.Prelude.C_Int
  | C_Float Curry.Module.Prelude.C_Float
  | C_String (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_LiteralFail Curry.RunTimeSystem.C_Exceptions
  | C_LiteralOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.CurrySyntax.C_Literal)

data C_ConstrTerm t0 = C_LiteralPattern Curry.Module.CurrySyntax.C_Literal
  | C_NegativePattern Curry.Module.CurrySyntax.C_Ident Curry.Module.CurrySyntax.C_Literal
  | C_VariablePattern Curry.Module.CurrySyntax.C_Ident
  | C_ConstructorPattern Curry.Module.CurrySyntax.C_QualIdent (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_ConstrTerm t0))
  | C_InfixPattern (Curry.Module.CurrySyntax.C_ConstrTerm t0) Curry.Module.CurrySyntax.C_QualIdent (Curry.Module.CurrySyntax.C_ConstrTerm t0)
  | C_ParenPattern (Curry.Module.CurrySyntax.C_ConstrTerm t0)
  | C_TuplePattern (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_ConstrTerm t0))
  | C_ListPattern (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_ConstrTerm t0))
  | C_AsPattern Curry.Module.CurrySyntax.C_Ident (Curry.Module.CurrySyntax.C_ConstrTerm t0)
  | C_LazyPattern (Curry.Module.CurrySyntax.C_ConstrTerm t0)
  | C_FunctionPattern Curry.Module.CurrySyntax.C_QualIdent (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_ConstrTerm t0))
  | C_InfixFuncPattern (Curry.Module.CurrySyntax.C_ConstrTerm t0) Curry.Module.CurrySyntax.C_QualIdent (Curry.Module.CurrySyntax.C_ConstrTerm t0)
  | C_RecordPattern (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_Field t0 (Curry.Module.CurrySyntax.C_ConstrTerm t0))) (Curry.Module.Prelude.C_Maybe (Curry.Module.CurrySyntax.C_ConstrTerm t0))
  | C_ConstrTermFail Curry.RunTimeSystem.C_Exceptions
  | C_ConstrTermOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.CurrySyntax.C_ConstrTerm t0))

data C_Expression t0 = C_Literal Curry.Module.CurrySyntax.C_Literal
  | C_Variable Curry.Module.CurrySyntax.C_QualIdent
  | C_Constructor Curry.Module.CurrySyntax.C_QualIdent
  | C_Paren (Curry.Module.CurrySyntax.C_Expression t0)
  | C_Typed (Curry.Module.CurrySyntax.C_Expression t0) Curry.Module.CurrySyntax.C_TypeExpr
  | C_Tuple (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_Expression t0))
  | C_List (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_Expression t0))
  | C_ListCompr (Curry.Module.CurrySyntax.C_Expression t0) (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_Statement t0))
  | C_EnumFrom (Curry.Module.CurrySyntax.C_Expression t0)
  | C_EnumFromThen (Curry.Module.CurrySyntax.C_Expression t0) (Curry.Module.CurrySyntax.C_Expression t0)
  | C_EnumFromTo (Curry.Module.CurrySyntax.C_Expression t0) (Curry.Module.CurrySyntax.C_Expression t0)
  | C_EnumFromThenTo (Curry.Module.CurrySyntax.C_Expression t0) (Curry.Module.CurrySyntax.C_Expression t0) (Curry.Module.CurrySyntax.C_Expression t0)
  | C_UnaryMinus Curry.Module.CurrySyntax.C_Ident (Curry.Module.CurrySyntax.C_Expression t0)
  | C_Apply (Curry.Module.CurrySyntax.C_Expression t0) (Curry.Module.CurrySyntax.C_Expression t0)
  | C_InfixApply (Curry.Module.CurrySyntax.C_Expression t0) Curry.Module.CurrySyntax.C_InfixOp (Curry.Module.CurrySyntax.C_Expression t0)
  | C_LeftSection (Curry.Module.CurrySyntax.C_Expression t0) Curry.Module.CurrySyntax.C_InfixOp
  | C_RightSection Curry.Module.CurrySyntax.C_InfixOp (Curry.Module.CurrySyntax.C_Expression t0)
  | C_Lambda (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_ConstrTerm t0)) (Curry.Module.CurrySyntax.C_Expression t0)
  | C_Let (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_Decl t0)) (Curry.Module.CurrySyntax.C_Expression t0)
  | C_Do (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_Statement t0)) (Curry.Module.CurrySyntax.C_Expression t0)
  | C_IfThenElse (Curry.Module.CurrySyntax.C_Expression t0) (Curry.Module.CurrySyntax.C_Expression t0) (Curry.Module.CurrySyntax.C_Expression t0)
  | C_Case (Curry.Module.CurrySyntax.C_Expression t0) (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_Alt t0))
  | C_RecordConstr (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_Field t0 (Curry.Module.CurrySyntax.C_Expression t0)))
  | C_RecordSelection (Curry.Module.CurrySyntax.C_Expression t0) Curry.Module.CurrySyntax.C_Ident
  | C_RecordUpdate (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_Field t0 (Curry.Module.CurrySyntax.C_Expression t0))) (Curry.Module.CurrySyntax.C_Expression t0)
  | C_ExpressionFail Curry.RunTimeSystem.C_Exceptions
  | C_ExpressionOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.CurrySyntax.C_Expression t0))

data C_InfixOp = C_InfixOp Curry.Module.CurrySyntax.C_QualIdent
  | C_InfixConstr Curry.Module.CurrySyntax.C_QualIdent
  | C_InfixOpFail Curry.RunTimeSystem.C_Exceptions
  | C_InfixOpOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.CurrySyntax.C_InfixOp)

data C_Statement t0 = C_StmtExpr (Curry.Module.CurrySyntax.C_Expression t0)
  | C_StmtDecl (Curry.Module.Prelude.List (Curry.Module.CurrySyntax.C_Decl t0))
  | C_StmtBind (Curry.Module.CurrySyntax.C_ConstrTerm t0) (Curry.Module.CurrySyntax.C_Expression t0)
  | C_StatementFail Curry.RunTimeSystem.C_Exceptions
  | C_StatementOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.CurrySyntax.C_Statement t0))

data C_Alt t0 = C_Alt t0 (Curry.Module.CurrySyntax.C_ConstrTerm t0) (Curry.Module.CurrySyntax.C_Rhs t0)
  | C_AltFail Curry.RunTimeSystem.C_Exceptions
  | C_AltOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.CurrySyntax.C_Alt t0))

data C_Field t0 t1 = C_Field t0 Curry.Module.CurrySyntax.C_Ident t1
  | C_FieldFail Curry.RunTimeSystem.C_Exceptions
  | C_FieldOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.CurrySyntax.C_Field t0 t1))

instance BaseCurry Curry.Module.CurrySyntax.C_Position where
  nf f (Curry.Module.CurrySyntax.C_Position x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_Position(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_Position x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_Position(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_PositionOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.CurrySyntax.C_Position(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(3)

  failed  = Curry.Module.CurrySyntax.C_PositionFail

  branching  = Curry.Module.CurrySyntax.C_PositionOr

  consKind (Curry.Module.CurrySyntax.C_PositionOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_PositionFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_PositionFail x) = x

  orRef (Curry.Module.CurrySyntax.C_PositionOr x _) = x

  branches (Curry.Module.CurrySyntax.C_PositionOr _ x) = x





instance BaseCurry Curry.Module.CurrySyntax.C_Ident where
  nf f (Curry.Module.CurrySyntax.C_Ident x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Ident(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_Ident x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Ident(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_IdentOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.CurrySyntax.C_Ident(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.CurrySyntax.C_IdentFail

  branching  = Curry.Module.CurrySyntax.C_IdentOr

  consKind (Curry.Module.CurrySyntax.C_IdentOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_IdentFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_IdentFail x) = x

  orRef (Curry.Module.CurrySyntax.C_IdentOr x _) = x

  branches (Curry.Module.CurrySyntax.C_IdentOr _ x) = x





instance BaseCurry Curry.Module.CurrySyntax.C_QualIdent where
  nf f (Curry.Module.CurrySyntax.C_UnqualIdent x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_UnqualIdent(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_QualIdent x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_QualIdent(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_UnqualIdent x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_UnqualIdent(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_QualIdent x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_QualIdent(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_QualIdentOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.CurrySyntax.C_UnqualIdent(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_QualIdent(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.CurrySyntax.C_QualIdentFail

  branching  = Curry.Module.CurrySyntax.C_QualIdentOr

  consKind (Curry.Module.CurrySyntax.C_QualIdentOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_QualIdentFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_QualIdentFail x) = x

  orRef (Curry.Module.CurrySyntax.C_QualIdentOr x _) = x

  branches (Curry.Module.CurrySyntax.C_QualIdentOr _ x) = x





instance (BaseCurry t0) => BaseCurry (Curry.Module.CurrySyntax.C_Module t0) where
  nf f (Curry.Module.CurrySyntax.C_Module x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_Module(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_Module x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_Module(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_ModuleOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.CurrySyntax.C_Module(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(3)

  failed  = Curry.Module.CurrySyntax.C_ModuleFail

  branching  = Curry.Module.CurrySyntax.C_ModuleOr

  consKind (Curry.Module.CurrySyntax.C_ModuleOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_ModuleFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_ModuleFail x) = x

  orRef (Curry.Module.CurrySyntax.C_ModuleOr x _) = x

  branches (Curry.Module.CurrySyntax.C_ModuleOr _ x) = x





instance (BaseCurry t0) => BaseCurry (Curry.Module.CurrySyntax.C_ExportSpec t0) where
  nf f (Curry.Module.CurrySyntax.C_Exporting x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Exporting(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_Exporting x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Exporting(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_ExportSpecOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.CurrySyntax.C_Exporting(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.CurrySyntax.C_ExportSpecFail

  branching  = Curry.Module.CurrySyntax.C_ExportSpecOr

  consKind (Curry.Module.CurrySyntax.C_ExportSpecOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_ExportSpecFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_ExportSpecFail x) = x

  orRef (Curry.Module.CurrySyntax.C_ExportSpecOr x _) = x

  branches (Curry.Module.CurrySyntax.C_ExportSpecOr _ x) = x





instance BaseCurry Curry.Module.CurrySyntax.C_Export where
  nf f (Curry.Module.CurrySyntax.C_Export x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_Export(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_ExportTypeWith x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_ExportTypeWith(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_ExportTypeAll x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_ExportTypeAll(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_ExportModule x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_ExportModule(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_Export x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_Export(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_ExportTypeWith x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_ExportTypeWith(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_ExportTypeAll x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_ExportTypeAll(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_ExportModule x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_ExportModule(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_ExportOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.CurrySyntax.C_Export(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_ExportTypeWith(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_ExportTypeAll(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_ExportModule(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.CurrySyntax.C_ExportFail

  branching  = Curry.Module.CurrySyntax.C_ExportOr

  consKind (Curry.Module.CurrySyntax.C_ExportOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_ExportFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_ExportFail x) = x

  orRef (Curry.Module.CurrySyntax.C_ExportOr x _) = x

  branches (Curry.Module.CurrySyntax.C_ExportOr _ x) = x





instance (BaseCurry t0) => BaseCurry (Curry.Module.CurrySyntax.C_ImportSpec t0) where
  nf f (Curry.Module.CurrySyntax.C_Importing x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Importing(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_Hiding x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Hiding(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_Importing x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Importing(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_Hiding x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Hiding(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_ImportSpecOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.CurrySyntax.C_Importing(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_Hiding(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.CurrySyntax.C_ImportSpecFail

  branching  = Curry.Module.CurrySyntax.C_ImportSpecOr

  consKind (Curry.Module.CurrySyntax.C_ImportSpecOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_ImportSpecFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_ImportSpecFail x) = x

  orRef (Curry.Module.CurrySyntax.C_ImportSpecOr x _) = x

  branches (Curry.Module.CurrySyntax.C_ImportSpecOr _ x) = x





instance BaseCurry Curry.Module.CurrySyntax.C_Import where
  nf f (Curry.Module.CurrySyntax.C_Import x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_Import(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_ImportTypeWith x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_ImportTypeWith(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_ImportTypeAll x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_ImportTypeAll(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_Import x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_Import(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_ImportTypeWith x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_ImportTypeWith(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_ImportTypeAll x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_ImportTypeAll(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_ImportOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.CurrySyntax.C_Import(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_ImportTypeWith(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_ImportTypeAll(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.CurrySyntax.C_ImportFail

  branching  = Curry.Module.CurrySyntax.C_ImportOr

  consKind (Curry.Module.CurrySyntax.C_ImportOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_ImportFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_ImportFail x) = x

  orRef (Curry.Module.CurrySyntax.C_ImportOr x _) = x

  branches (Curry.Module.CurrySyntax.C_ImportOr _ x) = x





instance (BaseCurry t0) => BaseCurry (Curry.Module.CurrySyntax.C_Decl t0) where
  nf f (Curry.Module.CurrySyntax.C_ImportDecl x1 x2 x3 x4 x5) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> f(Curry.Module.CurrySyntax.C_ImportDecl(v1)(v2)(v3)(v4)(v5))(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_InfixDecl x1 x2 x3 x4) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> f(Curry.Module.CurrySyntax.C_InfixDecl(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_DataDecl x1 x2 x3 x4) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> f(Curry.Module.CurrySyntax.C_DataDecl(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_NewtypeDecl x1 x2 x3 x4) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> f(Curry.Module.CurrySyntax.C_NewtypeDecl(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_TypeDecl x1 x2 x3 x4) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> f(Curry.Module.CurrySyntax.C_TypeDecl(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_TypeSig x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_TypeSig(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_EvalAnnot x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_EvalAnnot(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_FunctionDecl x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_FunctionDecl(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_ExternalDecl x1 x2 x3 x4 x5) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> f(Curry.Module.CurrySyntax.C_ExternalDecl(v1)(v2)(v3)(v4)(v5))(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_FlatExternalDecl x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_FlatExternalDecl(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_PatternDecl x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_PatternDecl(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_ExtraVariables x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_ExtraVariables(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_ImportDecl x1 x2 x3 x4 x5) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> f(Curry.Module.CurrySyntax.C_ImportDecl(v1)(v2)(v3)(v4)(v5))(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_InfixDecl x1 x2 x3 x4) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> f(Curry.Module.CurrySyntax.C_InfixDecl(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_DataDecl x1 x2 x3 x4) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> f(Curry.Module.CurrySyntax.C_DataDecl(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_NewtypeDecl x1 x2 x3 x4) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> f(Curry.Module.CurrySyntax.C_NewtypeDecl(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_TypeDecl x1 x2 x3 x4) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> f(Curry.Module.CurrySyntax.C_TypeDecl(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_TypeSig x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_TypeSig(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_EvalAnnot x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_EvalAnnot(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_FunctionDecl x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_FunctionDecl(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_ExternalDecl x1 x2 x3 x4 x5) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> f(Curry.Module.CurrySyntax.C_ExternalDecl(v1)(v2)(v3)(v4)(v5))(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_FlatExternalDecl x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_FlatExternalDecl(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_PatternDecl x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_PatternDecl(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_ExtraVariables x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_ExtraVariables(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_DeclOr(Curry.RunTimeSystem.mkRef(r)(5)(i))([Curry.Module.CurrySyntax.C_ImportDecl(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_InfixDecl(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_DataDecl(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_NewtypeDecl(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_TypeDecl(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_TypeSig(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_EvalAnnot(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_FunctionDecl(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_ExternalDecl(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_FlatExternalDecl(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_PatternDecl(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_ExtraVariables(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(5)

  failed  = Curry.Module.CurrySyntax.C_DeclFail

  branching  = Curry.Module.CurrySyntax.C_DeclOr

  consKind (Curry.Module.CurrySyntax.C_DeclOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_DeclFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_DeclFail x) = x

  orRef (Curry.Module.CurrySyntax.C_DeclOr x _) = x

  branches (Curry.Module.CurrySyntax.C_DeclOr _ x) = x





instance (BaseCurry t0) => BaseCurry (Curry.Module.CurrySyntax.C_ConstrDecl t0) where
  nf f (Curry.Module.CurrySyntax.C_ConstrDecl x1 x2 x3 x4) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> f(Curry.Module.CurrySyntax.C_ConstrDecl(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_ConOpDecl x1 x2 x3 x4 x5) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> f(Curry.Module.CurrySyntax.C_ConOpDecl(v1)(v2)(v3)(v4)(v5))(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_ConstrDecl x1 x2 x3 x4) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> f(Curry.Module.CurrySyntax.C_ConstrDecl(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_ConOpDecl x1 x2 x3 x4 x5) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> f(Curry.Module.CurrySyntax.C_ConOpDecl(v1)(v2)(v3)(v4)(v5))(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_ConstrDeclOr(Curry.RunTimeSystem.mkRef(r)(5)(i))([Curry.Module.CurrySyntax.C_ConstrDecl(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_ConOpDecl(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(5)

  failed  = Curry.Module.CurrySyntax.C_ConstrDeclFail

  branching  = Curry.Module.CurrySyntax.C_ConstrDeclOr

  consKind (Curry.Module.CurrySyntax.C_ConstrDeclOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_ConstrDeclFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_ConstrDeclFail x) = x

  orRef (Curry.Module.CurrySyntax.C_ConstrDeclOr x _) = x

  branches (Curry.Module.CurrySyntax.C_ConstrDeclOr _ x) = x





instance (BaseCurry t0) => BaseCurry (Curry.Module.CurrySyntax.C_NewConstrDecl t0) where
  nf f (Curry.Module.CurrySyntax.C_NewConstrDecl x1 x2 x3 x4) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> f(Curry.Module.CurrySyntax.C_NewConstrDecl(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_NewConstrDecl x1 x2 x3 x4) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> f(Curry.Module.CurrySyntax.C_NewConstrDecl(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_NewConstrDeclOr(Curry.RunTimeSystem.mkRef(r)(4)(i))([Curry.Module.CurrySyntax.C_NewConstrDecl(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(4)

  failed  = Curry.Module.CurrySyntax.C_NewConstrDeclFail

  branching  = Curry.Module.CurrySyntax.C_NewConstrDeclOr

  consKind (Curry.Module.CurrySyntax.C_NewConstrDeclOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_NewConstrDeclFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_NewConstrDeclFail x) = x

  orRef (Curry.Module.CurrySyntax.C_NewConstrDeclOr x _) = x

  branches (Curry.Module.CurrySyntax.C_NewConstrDeclOr _ x) = x





instance BaseCurry Curry.Module.CurrySyntax.C_Infix where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_InfixOr(Curry.RunTimeSystem.mkRef(r)(0)(i))([Curry.Module.CurrySyntax.C_InfixL,Curry.Module.CurrySyntax.C_InfixR,Curry.Module.CurrySyntax.C_Infix]))(0)

  failed  = Curry.Module.CurrySyntax.C_InfixFail

  branching  = Curry.Module.CurrySyntax.C_InfixOr

  consKind (Curry.Module.CurrySyntax.C_InfixOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_InfixFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_InfixFail x) = x

  orRef (Curry.Module.CurrySyntax.C_InfixOr x _) = x

  branches (Curry.Module.CurrySyntax.C_InfixOr _ x) = x





instance BaseCurry Curry.Module.CurrySyntax.C_EvalAnnotation where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_EvalAnnotationOr(Curry.RunTimeSystem.mkRef(r)(0)(i))([Curry.Module.CurrySyntax.C_EvalRigid,Curry.Module.CurrySyntax.C_EvalChoice]))(0)

  failed  = Curry.Module.CurrySyntax.C_EvalAnnotationFail

  branching  = Curry.Module.CurrySyntax.C_EvalAnnotationOr

  consKind (Curry.Module.CurrySyntax.C_EvalAnnotationOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_EvalAnnotationFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_EvalAnnotationFail x) = x

  orRef (Curry.Module.CurrySyntax.C_EvalAnnotationOr x _) = x

  branches (Curry.Module.CurrySyntax.C_EvalAnnotationOr _ x) = x





instance BaseCurry Curry.Module.CurrySyntax.C_CallConv where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_CallConvOr(Curry.RunTimeSystem.mkRef(r)(0)(i))([Curry.Module.CurrySyntax.C_CallConvPrimitive,Curry.Module.CurrySyntax.C_CallConvCCall]))(0)

  failed  = Curry.Module.CurrySyntax.C_CallConvFail

  branching  = Curry.Module.CurrySyntax.C_CallConvOr

  consKind (Curry.Module.CurrySyntax.C_CallConvOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_CallConvFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_CallConvFail x) = x

  orRef (Curry.Module.CurrySyntax.C_CallConvOr x _) = x

  branches (Curry.Module.CurrySyntax.C_CallConvOr _ x) = x





instance BaseCurry Curry.Module.CurrySyntax.C_TypeExpr where
  nf f (Curry.Module.CurrySyntax.C_ConstructorType x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_ConstructorType(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_VariableType x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_VariableType(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_TupleType x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_TupleType(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_ListType x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_ListType(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_ArrowType x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_ArrowType(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_RecordType x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_RecordType(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_ConstructorType x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_ConstructorType(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_VariableType x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_VariableType(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_TupleType x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_TupleType(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_ListType x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_ListType(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_ArrowType x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_ArrowType(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_RecordType x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_RecordType(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_TypeExprOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.CurrySyntax.C_ConstructorType(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_VariableType(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_TupleType(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_ListType(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_ArrowType(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_RecordType(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.CurrySyntax.C_TypeExprFail

  branching  = Curry.Module.CurrySyntax.C_TypeExprOr

  consKind (Curry.Module.CurrySyntax.C_TypeExprOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_TypeExprFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_TypeExprFail x) = x

  orRef (Curry.Module.CurrySyntax.C_TypeExprOr x _) = x

  branches (Curry.Module.CurrySyntax.C_TypeExprOr _ x) = x





instance (BaseCurry t0) => BaseCurry (Curry.Module.CurrySyntax.C_Equation t0) where
  nf f (Curry.Module.CurrySyntax.C_Equation x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_Equation(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_Equation x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_Equation(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_EquationOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.CurrySyntax.C_Equation(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(3)

  failed  = Curry.Module.CurrySyntax.C_EquationFail

  branching  = Curry.Module.CurrySyntax.C_EquationOr

  consKind (Curry.Module.CurrySyntax.C_EquationOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_EquationFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_EquationFail x) = x

  orRef (Curry.Module.CurrySyntax.C_EquationOr x _) = x

  branches (Curry.Module.CurrySyntax.C_EquationOr _ x) = x





instance (BaseCurry t0) => BaseCurry (Curry.Module.CurrySyntax.C_Lhs t0) where
  nf f (Curry.Module.CurrySyntax.C_FunLhs x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_FunLhs(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_OpLhs x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_OpLhs(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_ApLhs x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_ApLhs(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_FunLhs x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_FunLhs(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_OpLhs x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_OpLhs(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_ApLhs x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_ApLhs(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_LhsOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.CurrySyntax.C_FunLhs(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_OpLhs(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_ApLhs(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(3)

  failed  = Curry.Module.CurrySyntax.C_LhsFail

  branching  = Curry.Module.CurrySyntax.C_LhsOr

  consKind (Curry.Module.CurrySyntax.C_LhsOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_LhsFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_LhsFail x) = x

  orRef (Curry.Module.CurrySyntax.C_LhsOr x _) = x

  branches (Curry.Module.CurrySyntax.C_LhsOr _ x) = x





instance (BaseCurry t0) => BaseCurry (Curry.Module.CurrySyntax.C_Rhs t0) where
  nf f (Curry.Module.CurrySyntax.C_SimpleRhs x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_SimpleRhs(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_GuardedRhs x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_GuardedRhs(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_SimpleRhs x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_SimpleRhs(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_GuardedRhs x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_GuardedRhs(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_RhsOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.CurrySyntax.C_SimpleRhs(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_GuardedRhs(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(3)

  failed  = Curry.Module.CurrySyntax.C_RhsFail

  branching  = Curry.Module.CurrySyntax.C_RhsOr

  consKind (Curry.Module.CurrySyntax.C_RhsOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_RhsFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_RhsFail x) = x

  orRef (Curry.Module.CurrySyntax.C_RhsOr x _) = x

  branches (Curry.Module.CurrySyntax.C_RhsOr _ x) = x





instance (BaseCurry t0) => BaseCurry (Curry.Module.CurrySyntax.C_CondExpr t0) where
  nf f (Curry.Module.CurrySyntax.C_CondExpr x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_CondExpr(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_CondExpr x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_CondExpr(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_CondExprOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.CurrySyntax.C_CondExpr(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(3)

  failed  = Curry.Module.CurrySyntax.C_CondExprFail

  branching  = Curry.Module.CurrySyntax.C_CondExprOr

  consKind (Curry.Module.CurrySyntax.C_CondExprOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_CondExprFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_CondExprFail x) = x

  orRef (Curry.Module.CurrySyntax.C_CondExprOr x _) = x

  branches (Curry.Module.CurrySyntax.C_CondExprOr _ x) = x





instance BaseCurry Curry.Module.CurrySyntax.C_Literal where
  nf f (Curry.Module.CurrySyntax.C_Char x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_Char(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_Int x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Int(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_Float x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_Float(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_String x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_String(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_Char x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_Char(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_Int x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Int(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_Float x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_Float(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_String x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_String(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_LiteralOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.CurrySyntax.C_Char(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_Int(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_Float(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_String(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.CurrySyntax.C_LiteralFail

  branching  = Curry.Module.CurrySyntax.C_LiteralOr

  consKind (Curry.Module.CurrySyntax.C_LiteralOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_LiteralFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_LiteralFail x) = x

  orRef (Curry.Module.CurrySyntax.C_LiteralOr x _) = x

  branches (Curry.Module.CurrySyntax.C_LiteralOr _ x) = x





instance (BaseCurry t0) => BaseCurry (Curry.Module.CurrySyntax.C_ConstrTerm t0) where
  nf f (Curry.Module.CurrySyntax.C_LiteralPattern x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_LiteralPattern(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_NegativePattern x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_NegativePattern(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_VariablePattern x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_VariablePattern(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_ConstructorPattern x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_ConstructorPattern(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_InfixPattern x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_InfixPattern(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_ParenPattern x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_ParenPattern(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_TuplePattern x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_TuplePattern(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_ListPattern x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_ListPattern(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_AsPattern x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_AsPattern(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_LazyPattern x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_LazyPattern(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_FunctionPattern x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_FunctionPattern(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_InfixFuncPattern x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_InfixFuncPattern(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_RecordPattern x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_RecordPattern(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_LiteralPattern x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_LiteralPattern(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_NegativePattern x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_NegativePattern(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_VariablePattern x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_VariablePattern(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_ConstructorPattern x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_ConstructorPattern(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_InfixPattern x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_InfixPattern(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_ParenPattern x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_ParenPattern(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_TuplePattern x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_TuplePattern(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_ListPattern x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_ListPattern(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_AsPattern x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_AsPattern(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_LazyPattern x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_LazyPattern(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_FunctionPattern x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_FunctionPattern(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_InfixFuncPattern x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_InfixFuncPattern(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_RecordPattern x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_RecordPattern(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_ConstrTermOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.CurrySyntax.C_LiteralPattern(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_NegativePattern(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_VariablePattern(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_ConstructorPattern(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_InfixPattern(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_ParenPattern(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_TuplePattern(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_ListPattern(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_AsPattern(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_LazyPattern(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_FunctionPattern(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_InfixFuncPattern(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_RecordPattern(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(3)

  failed  = Curry.Module.CurrySyntax.C_ConstrTermFail

  branching  = Curry.Module.CurrySyntax.C_ConstrTermOr

  consKind (Curry.Module.CurrySyntax.C_ConstrTermOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_ConstrTermFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_ConstrTermFail x) = x

  orRef (Curry.Module.CurrySyntax.C_ConstrTermOr x _) = x

  branches (Curry.Module.CurrySyntax.C_ConstrTermOr _ x) = x





instance (BaseCurry t0) => BaseCurry (Curry.Module.CurrySyntax.C_Expression t0) where
  nf f (Curry.Module.CurrySyntax.C_Literal x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_Literal(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_Variable x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_Variable(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_Constructor x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_Constructor(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_Paren x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_Paren(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_Typed x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Typed(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_Tuple x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_Tuple(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_List x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_List(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_ListCompr x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_ListCompr(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_EnumFrom x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_EnumFrom(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_EnumFromThen x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_EnumFromThen(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_EnumFromTo x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_EnumFromTo(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_EnumFromThenTo x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_EnumFromThenTo(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_UnaryMinus x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_UnaryMinus(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_Apply x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Apply(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_InfixApply x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_InfixApply(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_LeftSection x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_LeftSection(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_RightSection x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_RightSection(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_Lambda x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Lambda(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_Let x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Let(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_Do x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Do(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_IfThenElse x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_IfThenElse(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_Case x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Case(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_RecordConstr x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_RecordConstr(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_RecordSelection x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_RecordSelection(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_RecordUpdate x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_RecordUpdate(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_Literal x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_Literal(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_Variable x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_Variable(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_Constructor x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_Constructor(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_Paren x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_Paren(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_Typed x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Typed(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_Tuple x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_Tuple(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_List x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_List(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_ListCompr x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_ListCompr(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_EnumFrom x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_EnumFrom(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_EnumFromThen x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_EnumFromThen(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_EnumFromTo x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_EnumFromTo(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_EnumFromThenTo x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_EnumFromThenTo(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_UnaryMinus x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_UnaryMinus(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_Apply x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Apply(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_InfixApply x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_InfixApply(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_LeftSection x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_LeftSection(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_RightSection x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_RightSection(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_Lambda x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Lambda(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_Let x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Let(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_Do x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Do(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_IfThenElse x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_IfThenElse(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_Case x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_Case(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_RecordConstr x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_RecordConstr(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_RecordSelection x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_RecordSelection(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_RecordUpdate x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_RecordUpdate(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_ExpressionOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.CurrySyntax.C_Literal(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_Variable(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_Constructor(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_Paren(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_Typed(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_Tuple(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_List(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_ListCompr(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_EnumFrom(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_EnumFromThen(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_EnumFromTo(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_EnumFromThenTo(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_UnaryMinus(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_Apply(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_InfixApply(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_LeftSection(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_RightSection(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_Lambda(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_Let(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_Do(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_IfThenElse(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_Case(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_RecordConstr(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_RecordSelection(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_RecordUpdate(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(3)

  failed  = Curry.Module.CurrySyntax.C_ExpressionFail

  branching  = Curry.Module.CurrySyntax.C_ExpressionOr

  consKind (Curry.Module.CurrySyntax.C_ExpressionOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_ExpressionFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_ExpressionFail x) = x

  orRef (Curry.Module.CurrySyntax.C_ExpressionOr x _) = x

  branches (Curry.Module.CurrySyntax.C_ExpressionOr _ x) = x





instance BaseCurry Curry.Module.CurrySyntax.C_InfixOp where
  nf f (Curry.Module.CurrySyntax.C_InfixOp x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_InfixOp(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_InfixConstr x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_InfixConstr(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_InfixOp x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_InfixOp(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_InfixConstr x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_InfixConstr(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_InfixOpOr(Curry.RunTimeSystem.mkRef(r)(1)(i))([Curry.Module.CurrySyntax.C_InfixOp(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_InfixConstr(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(1)

  failed  = Curry.Module.CurrySyntax.C_InfixOpFail

  branching  = Curry.Module.CurrySyntax.C_InfixOpOr

  consKind (Curry.Module.CurrySyntax.C_InfixOpOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_InfixOpFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_InfixOpFail x) = x

  orRef (Curry.Module.CurrySyntax.C_InfixOpOr x _) = x

  branches (Curry.Module.CurrySyntax.C_InfixOpOr _ x) = x





instance (BaseCurry t0) => BaseCurry (Curry.Module.CurrySyntax.C_Statement t0) where
  nf f (Curry.Module.CurrySyntax.C_StmtExpr x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_StmtExpr(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_StmtDecl x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_StmtDecl(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurrySyntax.C_StmtBind x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_StmtBind(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_StmtExpr x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_StmtExpr(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_StmtDecl x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurrySyntax.C_StmtDecl(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurrySyntax.C_StmtBind x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CurrySyntax.C_StmtBind(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_StatementOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.CurrySyntax.C_StmtExpr(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_StmtDecl(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurrySyntax.C_StmtBind(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.CurrySyntax.C_StatementFail

  branching  = Curry.Module.CurrySyntax.C_StatementOr

  consKind (Curry.Module.CurrySyntax.C_StatementOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_StatementFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_StatementFail x) = x

  orRef (Curry.Module.CurrySyntax.C_StatementOr x _) = x

  branches (Curry.Module.CurrySyntax.C_StatementOr _ x) = x





instance (BaseCurry t0) => BaseCurry (Curry.Module.CurrySyntax.C_Alt t0) where
  nf f (Curry.Module.CurrySyntax.C_Alt x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_Alt(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_Alt x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_Alt(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_AltOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.CurrySyntax.C_Alt(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(3)

  failed  = Curry.Module.CurrySyntax.C_AltFail

  branching  = Curry.Module.CurrySyntax.C_AltOr

  consKind (Curry.Module.CurrySyntax.C_AltOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_AltFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_AltFail x) = x

  orRef (Curry.Module.CurrySyntax.C_AltOr x _) = x

  branches (Curry.Module.CurrySyntax.C_AltOr _ x) = x





instance (BaseCurry t0,BaseCurry t1) => BaseCurry (Curry.Module.CurrySyntax.C_Field t0 t1) where
  nf f (Curry.Module.CurrySyntax.C_Field x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_Field(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurrySyntax.C_Field x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.CurrySyntax.C_Field(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurrySyntax.C_FieldOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.CurrySyntax.C_Field(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(3)

  failed  = Curry.Module.CurrySyntax.C_FieldFail

  branching  = Curry.Module.CurrySyntax.C_FieldOr

  consKind (Curry.Module.CurrySyntax.C_FieldOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurrySyntax.C_FieldFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurrySyntax.C_FieldFail x) = x

  orRef (Curry.Module.CurrySyntax.C_FieldOr x _) = x

  branches (Curry.Module.CurrySyntax.C_FieldOr _ x) = x





instance Curry Curry.Module.CurrySyntax.C_Position where
  strEq (Curry.Module.CurrySyntax.C_Position x1 x2 x3) (Curry.Module.CurrySyntax.C_Position y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_Position x1 x2 x3) (Curry.Module.CurrySyntax.C_Position y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_Position x1 x2 x3) st = Curry.Module.CurrySyntax.C_Position(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_Position x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)

  typeName _ = "Position"

  showQ d (Curry.Module.CurrySyntax.C_Position x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Position "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ _ (Curry.Module.CurrySyntax.C_PositionOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.CurrySyntax.C_Ident where
  strEq (Curry.Module.CurrySyntax.C_Ident x1 x2) (Curry.Module.CurrySyntax.C_Ident y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_Ident x1 x2) (Curry.Module.CurrySyntax.C_Ident y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_Ident x1 x2) st = Curry.Module.CurrySyntax.C_Ident(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_Ident x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "Ident"

  showQ d (Curry.Module.CurrySyntax.C_Ident x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Ident "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.CurrySyntax.C_IdentOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.CurrySyntax.C_QualIdent where
  strEq (Curry.Module.CurrySyntax.C_UnqualIdent x1) (Curry.Module.CurrySyntax.C_UnqualIdent y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_QualIdent x1 x2) (Curry.Module.CurrySyntax.C_QualIdent y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_UnqualIdent x1) (Curry.Module.CurrySyntax.C_UnqualIdent y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_QualIdent x1 x2) (Curry.Module.CurrySyntax.C_QualIdent y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_UnqualIdent x1) st = Curry.Module.CurrySyntax.C_UnqualIdent(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_QualIdent x1 x2) st = Curry.Module.CurrySyntax.C_QualIdent(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_UnqualIdent x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_QualIdent x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "QualIdent"

  showQ d (Curry.Module.CurrySyntax.C_UnqualIdent x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.UnqualIdent "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_QualIdent x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.QualIdent "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.CurrySyntax.C_QualIdentOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0) => Curry (Curry.Module.CurrySyntax.C_Module t0) where
  strEq (Curry.Module.CurrySyntax.C_Module x1 x2 x3) (Curry.Module.CurrySyntax.C_Module y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_Module x1 x2 x3) (Curry.Module.CurrySyntax.C_Module y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_Module x1 x2 x3) st = Curry.Module.CurrySyntax.C_Module(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_Module x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)

  typeName _ = "Module"

  showQ d (Curry.Module.CurrySyntax.C_Module x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Module "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ _ (Curry.Module.CurrySyntax.C_ModuleOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0) => Curry (Curry.Module.CurrySyntax.C_ExportSpec t0) where
  strEq (Curry.Module.CurrySyntax.C_Exporting x1 x2) (Curry.Module.CurrySyntax.C_Exporting y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_Exporting x1 x2) (Curry.Module.CurrySyntax.C_Exporting y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_Exporting x1 x2) st = Curry.Module.CurrySyntax.C_Exporting(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_Exporting x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "ExportSpec"

  showQ d (Curry.Module.CurrySyntax.C_Exporting x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Exporting "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.CurrySyntax.C_ExportSpecOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.CurrySyntax.C_Export where
  strEq (Curry.Module.CurrySyntax.C_Export x1) (Curry.Module.CurrySyntax.C_Export y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_ExportTypeWith x1 x2) (Curry.Module.CurrySyntax.C_ExportTypeWith y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_ExportTypeAll x1) (Curry.Module.CurrySyntax.C_ExportTypeAll y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_ExportModule x1) (Curry.Module.CurrySyntax.C_ExportModule y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_Export x1) (Curry.Module.CurrySyntax.C_Export y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_ExportTypeWith x1 x2) (Curry.Module.CurrySyntax.C_ExportTypeWith y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_ExportTypeAll x1) (Curry.Module.CurrySyntax.C_ExportTypeAll y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_ExportModule x1) (Curry.Module.CurrySyntax.C_ExportModule y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_Export x1) st = Curry.Module.CurrySyntax.C_Export(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_ExportTypeWith x1 x2) st = Curry.Module.CurrySyntax.C_ExportTypeWith(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_ExportTypeAll x1) st = Curry.Module.CurrySyntax.C_ExportTypeAll(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_ExportModule x1) st = Curry.Module.CurrySyntax.C_ExportModule(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_Export x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_ExportTypeWith x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_ExportTypeAll x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_ExportModule x1) st = f(x1)(c)(st)

  typeName _ = "Export"

  showQ d (Curry.Module.CurrySyntax.C_Export x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Export "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_ExportTypeWith x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.ExportTypeWith "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_ExportTypeAll x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.ExportTypeAll "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_ExportModule x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.ExportModule "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.CurrySyntax.C_ExportOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0) => Curry (Curry.Module.CurrySyntax.C_ImportSpec t0) where
  strEq (Curry.Module.CurrySyntax.C_Importing x1 x2) (Curry.Module.CurrySyntax.C_Importing y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_Hiding x1 x2) (Curry.Module.CurrySyntax.C_Hiding y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_Importing x1 x2) (Curry.Module.CurrySyntax.C_Importing y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_Hiding x1 x2) (Curry.Module.CurrySyntax.C_Hiding y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_Importing x1 x2) st = Curry.Module.CurrySyntax.C_Importing(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_Hiding x1 x2) st = Curry.Module.CurrySyntax.C_Hiding(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_Importing x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_Hiding x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "ImportSpec"

  showQ d (Curry.Module.CurrySyntax.C_Importing x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Importing "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_Hiding x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Hiding "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.CurrySyntax.C_ImportSpecOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.CurrySyntax.C_Import where
  strEq (Curry.Module.CurrySyntax.C_Import x1) (Curry.Module.CurrySyntax.C_Import y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_ImportTypeWith x1 x2) (Curry.Module.CurrySyntax.C_ImportTypeWith y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_ImportTypeAll x1) (Curry.Module.CurrySyntax.C_ImportTypeAll y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_Import x1) (Curry.Module.CurrySyntax.C_Import y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_ImportTypeWith x1 x2) (Curry.Module.CurrySyntax.C_ImportTypeWith y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_ImportTypeAll x1) (Curry.Module.CurrySyntax.C_ImportTypeAll y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_Import x1) st = Curry.Module.CurrySyntax.C_Import(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_ImportTypeWith x1 x2) st = Curry.Module.CurrySyntax.C_ImportTypeWith(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_ImportTypeAll x1) st = Curry.Module.CurrySyntax.C_ImportTypeAll(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_Import x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_ImportTypeWith x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_ImportTypeAll x1) st = f(x1)(c)(st)

  typeName _ = "Import"

  showQ d (Curry.Module.CurrySyntax.C_Import x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Import "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_ImportTypeWith x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.ImportTypeWith "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_ImportTypeAll x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.ImportTypeAll "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.CurrySyntax.C_ImportOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0) => Curry (Curry.Module.CurrySyntax.C_Decl t0) where
  strEq (Curry.Module.CurrySyntax.C_ImportDecl x1 x2 x3 x4 x5) (Curry.Module.CurrySyntax.C_ImportDecl y1 y2 y3 y4 y5) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(st))(st))(st))(st)
  strEq (Curry.Module.CurrySyntax.C_InfixDecl x1 x2 x3 x4) (Curry.Module.CurrySyntax.C_InfixDecl y1 y2 y3 y4) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(st))(st))(st)
  strEq (Curry.Module.CurrySyntax.C_DataDecl x1 x2 x3 x4) (Curry.Module.CurrySyntax.C_DataDecl y1 y2 y3 y4) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(st))(st))(st)
  strEq (Curry.Module.CurrySyntax.C_NewtypeDecl x1 x2 x3 x4) (Curry.Module.CurrySyntax.C_NewtypeDecl y1 y2 y3 y4) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(st))(st))(st)
  strEq (Curry.Module.CurrySyntax.C_TypeDecl x1 x2 x3 x4) (Curry.Module.CurrySyntax.C_TypeDecl y1 y2 y3 y4) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(st))(st))(st)
  strEq (Curry.Module.CurrySyntax.C_TypeSig x1 x2 x3) (Curry.Module.CurrySyntax.C_TypeSig y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq (Curry.Module.CurrySyntax.C_EvalAnnot x1 x2 x3) (Curry.Module.CurrySyntax.C_EvalAnnot y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq (Curry.Module.CurrySyntax.C_FunctionDecl x1 x2 x3) (Curry.Module.CurrySyntax.C_FunctionDecl y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq (Curry.Module.CurrySyntax.C_ExternalDecl x1 x2 x3 x4 x5) (Curry.Module.CurrySyntax.C_ExternalDecl y1 y2 y3 y4 y5) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(st))(st))(st))(st)
  strEq (Curry.Module.CurrySyntax.C_FlatExternalDecl x1 x2) (Curry.Module.CurrySyntax.C_FlatExternalDecl y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_PatternDecl x1 x2 x3) (Curry.Module.CurrySyntax.C_PatternDecl y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq (Curry.Module.CurrySyntax.C_ExtraVariables x1 x2) (Curry.Module.CurrySyntax.C_ExtraVariables y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_ImportDecl x1 x2 x3 x4 x5) (Curry.Module.CurrySyntax.C_ImportDecl y1 y2 y3 y4 y5) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.genEq(x5)(y5)(st))(st))(st))(st))(st)
  eq (Curry.Module.CurrySyntax.C_InfixDecl x1 x2 x3 x4) (Curry.Module.CurrySyntax.C_InfixDecl y1 y2 y3 y4) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.genEq(x4)(y4)(st))(st))(st))(st)
  eq (Curry.Module.CurrySyntax.C_DataDecl x1 x2 x3 x4) (Curry.Module.CurrySyntax.C_DataDecl y1 y2 y3 y4) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.genEq(x4)(y4)(st))(st))(st))(st)
  eq (Curry.Module.CurrySyntax.C_NewtypeDecl x1 x2 x3 x4) (Curry.Module.CurrySyntax.C_NewtypeDecl y1 y2 y3 y4) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.genEq(x4)(y4)(st))(st))(st))(st)
  eq (Curry.Module.CurrySyntax.C_TypeDecl x1 x2 x3 x4) (Curry.Module.CurrySyntax.C_TypeDecl y1 y2 y3 y4) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.genEq(x4)(y4)(st))(st))(st))(st)
  eq (Curry.Module.CurrySyntax.C_TypeSig x1 x2 x3) (Curry.Module.CurrySyntax.C_TypeSig y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq (Curry.Module.CurrySyntax.C_EvalAnnot x1 x2 x3) (Curry.Module.CurrySyntax.C_EvalAnnot y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq (Curry.Module.CurrySyntax.C_FunctionDecl x1 x2 x3) (Curry.Module.CurrySyntax.C_FunctionDecl y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq (Curry.Module.CurrySyntax.C_ExternalDecl x1 x2 x3 x4 x5) (Curry.Module.CurrySyntax.C_ExternalDecl y1 y2 y3 y4 y5) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.genEq(x5)(y5)(st))(st))(st))(st))(st)
  eq (Curry.Module.CurrySyntax.C_FlatExternalDecl x1 x2) (Curry.Module.CurrySyntax.C_FlatExternalDecl y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_PatternDecl x1 x2 x3) (Curry.Module.CurrySyntax.C_PatternDecl y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq (Curry.Module.CurrySyntax.C_ExtraVariables x1 x2) (Curry.Module.CurrySyntax.C_ExtraVariables y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_ImportDecl x1 x2 x3 x4 x5) st = Curry.Module.CurrySyntax.C_ImportDecl(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))
  propagate f (Curry.Module.CurrySyntax.C_InfixDecl x1 x2 x3 x4) st = Curry.Module.CurrySyntax.C_InfixDecl(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))
  propagate f (Curry.Module.CurrySyntax.C_DataDecl x1 x2 x3 x4) st = Curry.Module.CurrySyntax.C_DataDecl(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))
  propagate f (Curry.Module.CurrySyntax.C_NewtypeDecl x1 x2 x3 x4) st = Curry.Module.CurrySyntax.C_NewtypeDecl(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))
  propagate f (Curry.Module.CurrySyntax.C_TypeDecl x1 x2 x3 x4) st = Curry.Module.CurrySyntax.C_TypeDecl(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))
  propagate f (Curry.Module.CurrySyntax.C_TypeSig x1 x2 x3) st = Curry.Module.CurrySyntax.C_TypeSig(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))
  propagate f (Curry.Module.CurrySyntax.C_EvalAnnot x1 x2 x3) st = Curry.Module.CurrySyntax.C_EvalAnnot(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))
  propagate f (Curry.Module.CurrySyntax.C_FunctionDecl x1 x2 x3) st = Curry.Module.CurrySyntax.C_FunctionDecl(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))
  propagate f (Curry.Module.CurrySyntax.C_ExternalDecl x1 x2 x3 x4 x5) st = Curry.Module.CurrySyntax.C_ExternalDecl(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))
  propagate f (Curry.Module.CurrySyntax.C_FlatExternalDecl x1 x2) st = Curry.Module.CurrySyntax.C_FlatExternalDecl(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_PatternDecl x1 x2 x3) st = Curry.Module.CurrySyntax.C_PatternDecl(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))
  propagate f (Curry.Module.CurrySyntax.C_ExtraVariables x1 x2) st = Curry.Module.CurrySyntax.C_ExtraVariables(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_ImportDecl x1 x2 x3 x4 x5) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(c)(st))(st))(st))(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_InfixDecl x1 x2 x3 x4) st = f(x1)(f(x2)(f(x3)(f(x4)(c)(st))(st))(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_DataDecl x1 x2 x3 x4) st = f(x1)(f(x2)(f(x3)(f(x4)(c)(st))(st))(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_NewtypeDecl x1 x2 x3 x4) st = f(x1)(f(x2)(f(x3)(f(x4)(c)(st))(st))(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_TypeDecl x1 x2 x3 x4) st = f(x1)(f(x2)(f(x3)(f(x4)(c)(st))(st))(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_TypeSig x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_EvalAnnot x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_FunctionDecl x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_ExternalDecl x1 x2 x3 x4 x5) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(c)(st))(st))(st))(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_FlatExternalDecl x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_PatternDecl x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_ExtraVariables x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "Decl"

  showQ d (Curry.Module.CurrySyntax.C_ImportDecl x1 x2 x3 x4 x5) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.ImportDecl "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x5))))))))))


  showQ d (Curry.Module.CurrySyntax.C_InfixDecl x1 x2 x3 x4) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.InfixDecl "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))))))))


  showQ d (Curry.Module.CurrySyntax.C_DataDecl x1 x2 x3 x4) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.DataDecl "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))))))))


  showQ d (Curry.Module.CurrySyntax.C_NewtypeDecl x1 x2 x3 x4) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.NewtypeDecl "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))))))))


  showQ d (Curry.Module.CurrySyntax.C_TypeDecl x1 x2 x3 x4) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.TypeDecl "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))))))))


  showQ d (Curry.Module.CurrySyntax.C_TypeSig x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.TypeSig "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ d (Curry.Module.CurrySyntax.C_EvalAnnot x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.EvalAnnot "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ d (Curry.Module.CurrySyntax.C_FunctionDecl x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.FunctionDecl "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ d (Curry.Module.CurrySyntax.C_ExternalDecl x1 x2 x3 x4 x5) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.ExternalDecl "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x5))))))))))


  showQ d (Curry.Module.CurrySyntax.C_FlatExternalDecl x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.FlatExternalDecl "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_PatternDecl x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.PatternDecl "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ d (Curry.Module.CurrySyntax.C_ExtraVariables x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.ExtraVariables "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.CurrySyntax.C_DeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0) => Curry (Curry.Module.CurrySyntax.C_ConstrDecl t0) where
  strEq (Curry.Module.CurrySyntax.C_ConstrDecl x1 x2 x3 x4) (Curry.Module.CurrySyntax.C_ConstrDecl y1 y2 y3 y4) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(st))(st))(st)
  strEq (Curry.Module.CurrySyntax.C_ConOpDecl x1 x2 x3 x4 x5) (Curry.Module.CurrySyntax.C_ConOpDecl y1 y2 y3 y4 y5) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_ConstrDecl x1 x2 x3 x4) (Curry.Module.CurrySyntax.C_ConstrDecl y1 y2 y3 y4) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.genEq(x4)(y4)(st))(st))(st))(st)
  eq (Curry.Module.CurrySyntax.C_ConOpDecl x1 x2 x3 x4 x5) (Curry.Module.CurrySyntax.C_ConOpDecl y1 y2 y3 y4 y5) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.genEq(x5)(y5)(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_ConstrDecl x1 x2 x3 x4) st = Curry.Module.CurrySyntax.C_ConstrDecl(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))
  propagate f (Curry.Module.CurrySyntax.C_ConOpDecl x1 x2 x3 x4 x5) st = Curry.Module.CurrySyntax.C_ConOpDecl(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_ConstrDecl x1 x2 x3 x4) st = f(x1)(f(x2)(f(x3)(f(x4)(c)(st))(st))(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_ConOpDecl x1 x2 x3 x4 x5) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(c)(st))(st))(st))(st))(st)

  typeName _ = "ConstrDecl"

  showQ d (Curry.Module.CurrySyntax.C_ConstrDecl x1 x2 x3 x4) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.ConstrDecl "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))))))))


  showQ d (Curry.Module.CurrySyntax.C_ConOpDecl x1 x2 x3 x4 x5) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.ConOpDecl "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x5))))))))))


  showQ _ (Curry.Module.CurrySyntax.C_ConstrDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0) => Curry (Curry.Module.CurrySyntax.C_NewConstrDecl t0) where
  strEq (Curry.Module.CurrySyntax.C_NewConstrDecl x1 x2 x3 x4) (Curry.Module.CurrySyntax.C_NewConstrDecl y1 y2 y3 y4) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_NewConstrDecl x1 x2 x3 x4) (Curry.Module.CurrySyntax.C_NewConstrDecl y1 y2 y3 y4) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.genEq(x4)(y4)(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_NewConstrDecl x1 x2 x3 x4) st = Curry.Module.CurrySyntax.C_NewConstrDecl(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_NewConstrDecl x1 x2 x3 x4) st = f(x1)(f(x2)(f(x3)(f(x4)(c)(st))(st))(st))(st)

  typeName _ = "NewConstrDecl"

  showQ d (Curry.Module.CurrySyntax.C_NewConstrDecl x1 x2 x3 x4) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.NewConstrDecl "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))))))))


  showQ _ (Curry.Module.CurrySyntax.C_NewConstrDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.CurrySyntax.C_Infix where
  strEq Curry.Module.CurrySyntax.C_InfixL Curry.Module.CurrySyntax.C_InfixL st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.CurrySyntax.C_InfixR Curry.Module.CurrySyntax.C_InfixR st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.CurrySyntax.C_Infix Curry.Module.CurrySyntax.C_Infix st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.CurrySyntax.C_InfixL Curry.Module.CurrySyntax.C_InfixL st = Curry.Module.Prelude.C_True
  eq Curry.Module.CurrySyntax.C_InfixR Curry.Module.CurrySyntax.C_InfixR st = Curry.Module.Prelude.C_True
  eq Curry.Module.CurrySyntax.C_Infix Curry.Module.CurrySyntax.C_Infix st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.CurrySyntax.C_InfixL st = Curry.Module.CurrySyntax.C_InfixL
  propagate f Curry.Module.CurrySyntax.C_InfixR st = Curry.Module.CurrySyntax.C_InfixR
  propagate f Curry.Module.CurrySyntax.C_Infix st = Curry.Module.CurrySyntax.C_Infix

  foldCurry f c Curry.Module.CurrySyntax.C_InfixL st = c
  foldCurry f c Curry.Module.CurrySyntax.C_InfixR st = c
  foldCurry f c Curry.Module.CurrySyntax.C_Infix st = c

  typeName _ = "Infix"

  showQ _ Curry.Module.CurrySyntax.C_InfixL = Prelude.showString("CurrySyntax.InfixL")
  showQ _ Curry.Module.CurrySyntax.C_InfixR = Prelude.showString("CurrySyntax.InfixR")
  showQ _ Curry.Module.CurrySyntax.C_Infix = Prelude.showString("CurrySyntax.Infix")
  showQ _ (Curry.Module.CurrySyntax.C_InfixOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.CurrySyntax.C_EvalAnnotation where
  strEq Curry.Module.CurrySyntax.C_EvalRigid Curry.Module.CurrySyntax.C_EvalRigid st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.CurrySyntax.C_EvalChoice Curry.Module.CurrySyntax.C_EvalChoice st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.CurrySyntax.C_EvalRigid Curry.Module.CurrySyntax.C_EvalRigid st = Curry.Module.Prelude.C_True
  eq Curry.Module.CurrySyntax.C_EvalChoice Curry.Module.CurrySyntax.C_EvalChoice st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.CurrySyntax.C_EvalRigid st = Curry.Module.CurrySyntax.C_EvalRigid
  propagate f Curry.Module.CurrySyntax.C_EvalChoice st = Curry.Module.CurrySyntax.C_EvalChoice

  foldCurry f c Curry.Module.CurrySyntax.C_EvalRigid st = c
  foldCurry f c Curry.Module.CurrySyntax.C_EvalChoice st = c

  typeName _ = "EvalAnnotation"

  showQ _ Curry.Module.CurrySyntax.C_EvalRigid = Prelude.showString("CurrySyntax.EvalRigid")
  showQ _ Curry.Module.CurrySyntax.C_EvalChoice = Prelude.showString("CurrySyntax.EvalChoice")
  showQ _ (Curry.Module.CurrySyntax.C_EvalAnnotationOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.CurrySyntax.C_CallConv where
  strEq Curry.Module.CurrySyntax.C_CallConvPrimitive Curry.Module.CurrySyntax.C_CallConvPrimitive st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.CurrySyntax.C_CallConvCCall Curry.Module.CurrySyntax.C_CallConvCCall st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.CurrySyntax.C_CallConvPrimitive Curry.Module.CurrySyntax.C_CallConvPrimitive st = Curry.Module.Prelude.C_True
  eq Curry.Module.CurrySyntax.C_CallConvCCall Curry.Module.CurrySyntax.C_CallConvCCall st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.CurrySyntax.C_CallConvPrimitive st = Curry.Module.CurrySyntax.C_CallConvPrimitive
  propagate f Curry.Module.CurrySyntax.C_CallConvCCall st = Curry.Module.CurrySyntax.C_CallConvCCall

  foldCurry f c Curry.Module.CurrySyntax.C_CallConvPrimitive st = c
  foldCurry f c Curry.Module.CurrySyntax.C_CallConvCCall st = c

  typeName _ = "CallConv"

  showQ _ Curry.Module.CurrySyntax.C_CallConvPrimitive = Prelude.showString("CurrySyntax.CallConvPrimitive")
  showQ _ Curry.Module.CurrySyntax.C_CallConvCCall = Prelude.showString("CurrySyntax.CallConvCCall")
  showQ _ (Curry.Module.CurrySyntax.C_CallConvOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.CurrySyntax.C_TypeExpr where
  strEq (Curry.Module.CurrySyntax.C_ConstructorType x1 x2) (Curry.Module.CurrySyntax.C_ConstructorType y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_VariableType x1) (Curry.Module.CurrySyntax.C_VariableType y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_TupleType x1) (Curry.Module.CurrySyntax.C_TupleType y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_ListType x1) (Curry.Module.CurrySyntax.C_ListType y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_ArrowType x1 x2) (Curry.Module.CurrySyntax.C_ArrowType y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_RecordType x1 x2) (Curry.Module.CurrySyntax.C_RecordType y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_ConstructorType x1 x2) (Curry.Module.CurrySyntax.C_ConstructorType y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_VariableType x1) (Curry.Module.CurrySyntax.C_VariableType y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_TupleType x1) (Curry.Module.CurrySyntax.C_TupleType y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_ListType x1) (Curry.Module.CurrySyntax.C_ListType y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_ArrowType x1 x2) (Curry.Module.CurrySyntax.C_ArrowType y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_RecordType x1 x2) (Curry.Module.CurrySyntax.C_RecordType y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_ConstructorType x1 x2) st = Curry.Module.CurrySyntax.C_ConstructorType(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_VariableType x1) st = Curry.Module.CurrySyntax.C_VariableType(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_TupleType x1) st = Curry.Module.CurrySyntax.C_TupleType(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_ListType x1) st = Curry.Module.CurrySyntax.C_ListType(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_ArrowType x1 x2) st = Curry.Module.CurrySyntax.C_ArrowType(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_RecordType x1 x2) st = Curry.Module.CurrySyntax.C_RecordType(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_ConstructorType x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_VariableType x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_TupleType x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_ListType x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_ArrowType x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_RecordType x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "TypeExpr"

  showQ d (Curry.Module.CurrySyntax.C_ConstructorType x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.ConstructorType "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_VariableType x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.VariableType "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_TupleType x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.TupleType "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_ListType x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.ListType "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_ArrowType x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.ArrowType "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_RecordType x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.RecordType "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.CurrySyntax.C_TypeExprOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0) => Curry (Curry.Module.CurrySyntax.C_Equation t0) where
  strEq (Curry.Module.CurrySyntax.C_Equation x1 x2 x3) (Curry.Module.CurrySyntax.C_Equation y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_Equation x1 x2 x3) (Curry.Module.CurrySyntax.C_Equation y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_Equation x1 x2 x3) st = Curry.Module.CurrySyntax.C_Equation(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_Equation x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)

  typeName _ = "Equation"

  showQ d (Curry.Module.CurrySyntax.C_Equation x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Equation "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ _ (Curry.Module.CurrySyntax.C_EquationOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0) => Curry (Curry.Module.CurrySyntax.C_Lhs t0) where
  strEq (Curry.Module.CurrySyntax.C_FunLhs x1 x2) (Curry.Module.CurrySyntax.C_FunLhs y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_OpLhs x1 x2 x3) (Curry.Module.CurrySyntax.C_OpLhs y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq (Curry.Module.CurrySyntax.C_ApLhs x1 x2) (Curry.Module.CurrySyntax.C_ApLhs y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_FunLhs x1 x2) (Curry.Module.CurrySyntax.C_FunLhs y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_OpLhs x1 x2 x3) (Curry.Module.CurrySyntax.C_OpLhs y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq (Curry.Module.CurrySyntax.C_ApLhs x1 x2) (Curry.Module.CurrySyntax.C_ApLhs y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_FunLhs x1 x2) st = Curry.Module.CurrySyntax.C_FunLhs(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_OpLhs x1 x2 x3) st = Curry.Module.CurrySyntax.C_OpLhs(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))
  propagate f (Curry.Module.CurrySyntax.C_ApLhs x1 x2) st = Curry.Module.CurrySyntax.C_ApLhs(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_FunLhs x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_OpLhs x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_ApLhs x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "Lhs"

  showQ d (Curry.Module.CurrySyntax.C_FunLhs x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.FunLhs "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_OpLhs x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.OpLhs "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ d (Curry.Module.CurrySyntax.C_ApLhs x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.ApLhs "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.CurrySyntax.C_LhsOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0) => Curry (Curry.Module.CurrySyntax.C_Rhs t0) where
  strEq (Curry.Module.CurrySyntax.C_SimpleRhs x1 x2 x3) (Curry.Module.CurrySyntax.C_SimpleRhs y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq (Curry.Module.CurrySyntax.C_GuardedRhs x1 x2) (Curry.Module.CurrySyntax.C_GuardedRhs y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_SimpleRhs x1 x2 x3) (Curry.Module.CurrySyntax.C_SimpleRhs y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq (Curry.Module.CurrySyntax.C_GuardedRhs x1 x2) (Curry.Module.CurrySyntax.C_GuardedRhs y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_SimpleRhs x1 x2 x3) st = Curry.Module.CurrySyntax.C_SimpleRhs(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))
  propagate f (Curry.Module.CurrySyntax.C_GuardedRhs x1 x2) st = Curry.Module.CurrySyntax.C_GuardedRhs(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_SimpleRhs x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_GuardedRhs x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "Rhs"

  showQ d (Curry.Module.CurrySyntax.C_SimpleRhs x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.SimpleRhs "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ d (Curry.Module.CurrySyntax.C_GuardedRhs x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.GuardedRhs "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.CurrySyntax.C_RhsOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0) => Curry (Curry.Module.CurrySyntax.C_CondExpr t0) where
  strEq (Curry.Module.CurrySyntax.C_CondExpr x1 x2 x3) (Curry.Module.CurrySyntax.C_CondExpr y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_CondExpr x1 x2 x3) (Curry.Module.CurrySyntax.C_CondExpr y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_CondExpr x1 x2 x3) st = Curry.Module.CurrySyntax.C_CondExpr(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_CondExpr x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)

  typeName _ = "CondExpr"

  showQ d (Curry.Module.CurrySyntax.C_CondExpr x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.CondExpr "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ _ (Curry.Module.CurrySyntax.C_CondExprOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.CurrySyntax.C_Literal where
  strEq (Curry.Module.CurrySyntax.C_Char x1) (Curry.Module.CurrySyntax.C_Char y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_Int x1 x2) (Curry.Module.CurrySyntax.C_Int y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_Float x1) (Curry.Module.CurrySyntax.C_Float y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_String x1) (Curry.Module.CurrySyntax.C_String y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_Char x1) (Curry.Module.CurrySyntax.C_Char y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_Int x1 x2) (Curry.Module.CurrySyntax.C_Int y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_Float x1) (Curry.Module.CurrySyntax.C_Float y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_String x1) (Curry.Module.CurrySyntax.C_String y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_Char x1) st = Curry.Module.CurrySyntax.C_Char(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_Int x1 x2) st = Curry.Module.CurrySyntax.C_Int(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_Float x1) st = Curry.Module.CurrySyntax.C_Float(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_String x1) st = Curry.Module.CurrySyntax.C_String(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_Char x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_Int x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_Float x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_String x1) st = f(x1)(c)(st)

  typeName _ = "Literal"

  showQ d (Curry.Module.CurrySyntax.C_Char x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Char "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_Int x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Int "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_Float x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Float "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_String x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.String "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.CurrySyntax.C_LiteralOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0) => Curry (Curry.Module.CurrySyntax.C_ConstrTerm t0) where
  strEq (Curry.Module.CurrySyntax.C_LiteralPattern x1) (Curry.Module.CurrySyntax.C_LiteralPattern y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_NegativePattern x1 x2) (Curry.Module.CurrySyntax.C_NegativePattern y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_VariablePattern x1) (Curry.Module.CurrySyntax.C_VariablePattern y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_ConstructorPattern x1 x2) (Curry.Module.CurrySyntax.C_ConstructorPattern y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_InfixPattern x1 x2 x3) (Curry.Module.CurrySyntax.C_InfixPattern y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq (Curry.Module.CurrySyntax.C_ParenPattern x1) (Curry.Module.CurrySyntax.C_ParenPattern y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_TuplePattern x1) (Curry.Module.CurrySyntax.C_TuplePattern y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_ListPattern x1) (Curry.Module.CurrySyntax.C_ListPattern y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_AsPattern x1 x2) (Curry.Module.CurrySyntax.C_AsPattern y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_LazyPattern x1) (Curry.Module.CurrySyntax.C_LazyPattern y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_FunctionPattern x1 x2) (Curry.Module.CurrySyntax.C_FunctionPattern y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_InfixFuncPattern x1 x2 x3) (Curry.Module.CurrySyntax.C_InfixFuncPattern y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq (Curry.Module.CurrySyntax.C_RecordPattern x1 x2) (Curry.Module.CurrySyntax.C_RecordPattern y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_LiteralPattern x1) (Curry.Module.CurrySyntax.C_LiteralPattern y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_NegativePattern x1 x2) (Curry.Module.CurrySyntax.C_NegativePattern y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_VariablePattern x1) (Curry.Module.CurrySyntax.C_VariablePattern y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_ConstructorPattern x1 x2) (Curry.Module.CurrySyntax.C_ConstructorPattern y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_InfixPattern x1 x2 x3) (Curry.Module.CurrySyntax.C_InfixPattern y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq (Curry.Module.CurrySyntax.C_ParenPattern x1) (Curry.Module.CurrySyntax.C_ParenPattern y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_TuplePattern x1) (Curry.Module.CurrySyntax.C_TuplePattern y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_ListPattern x1) (Curry.Module.CurrySyntax.C_ListPattern y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_AsPattern x1 x2) (Curry.Module.CurrySyntax.C_AsPattern y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_LazyPattern x1) (Curry.Module.CurrySyntax.C_LazyPattern y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_FunctionPattern x1 x2) (Curry.Module.CurrySyntax.C_FunctionPattern y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_InfixFuncPattern x1 x2 x3) (Curry.Module.CurrySyntax.C_InfixFuncPattern y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq (Curry.Module.CurrySyntax.C_RecordPattern x1 x2) (Curry.Module.CurrySyntax.C_RecordPattern y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_LiteralPattern x1) st = Curry.Module.CurrySyntax.C_LiteralPattern(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_NegativePattern x1 x2) st = Curry.Module.CurrySyntax.C_NegativePattern(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_VariablePattern x1) st = Curry.Module.CurrySyntax.C_VariablePattern(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_ConstructorPattern x1 x2) st = Curry.Module.CurrySyntax.C_ConstructorPattern(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_InfixPattern x1 x2 x3) st = Curry.Module.CurrySyntax.C_InfixPattern(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))
  propagate f (Curry.Module.CurrySyntax.C_ParenPattern x1) st = Curry.Module.CurrySyntax.C_ParenPattern(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_TuplePattern x1) st = Curry.Module.CurrySyntax.C_TuplePattern(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_ListPattern x1) st = Curry.Module.CurrySyntax.C_ListPattern(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_AsPattern x1 x2) st = Curry.Module.CurrySyntax.C_AsPattern(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_LazyPattern x1) st = Curry.Module.CurrySyntax.C_LazyPattern(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_FunctionPattern x1 x2) st = Curry.Module.CurrySyntax.C_FunctionPattern(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_InfixFuncPattern x1 x2 x3) st = Curry.Module.CurrySyntax.C_InfixFuncPattern(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))
  propagate f (Curry.Module.CurrySyntax.C_RecordPattern x1 x2) st = Curry.Module.CurrySyntax.C_RecordPattern(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_LiteralPattern x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_NegativePattern x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_VariablePattern x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_ConstructorPattern x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_InfixPattern x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_ParenPattern x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_TuplePattern x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_ListPattern x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_AsPattern x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_LazyPattern x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_FunctionPattern x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_InfixFuncPattern x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_RecordPattern x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "ConstrTerm"

  showQ d (Curry.Module.CurrySyntax.C_LiteralPattern x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.LiteralPattern "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_NegativePattern x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.NegativePattern "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_VariablePattern x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.VariablePattern "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_ConstructorPattern x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.ConstructorPattern "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_InfixPattern x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.InfixPattern "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ d (Curry.Module.CurrySyntax.C_ParenPattern x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.ParenPattern "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_TuplePattern x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.TuplePattern "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_ListPattern x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.ListPattern "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_AsPattern x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.AsPattern "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_LazyPattern x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.LazyPattern "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_FunctionPattern x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.FunctionPattern "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_InfixFuncPattern x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.InfixFuncPattern "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ d (Curry.Module.CurrySyntax.C_RecordPattern x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.RecordPattern "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.CurrySyntax.C_ConstrTermOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0) => Curry (Curry.Module.CurrySyntax.C_Expression t0) where
  strEq (Curry.Module.CurrySyntax.C_Literal x1) (Curry.Module.CurrySyntax.C_Literal y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_Variable x1) (Curry.Module.CurrySyntax.C_Variable y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_Constructor x1) (Curry.Module.CurrySyntax.C_Constructor y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_Paren x1) (Curry.Module.CurrySyntax.C_Paren y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_Typed x1 x2) (Curry.Module.CurrySyntax.C_Typed y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_Tuple x1) (Curry.Module.CurrySyntax.C_Tuple y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_List x1) (Curry.Module.CurrySyntax.C_List y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_ListCompr x1 x2) (Curry.Module.CurrySyntax.C_ListCompr y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_EnumFrom x1) (Curry.Module.CurrySyntax.C_EnumFrom y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_EnumFromThen x1 x2) (Curry.Module.CurrySyntax.C_EnumFromThen y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_EnumFromTo x1 x2) (Curry.Module.CurrySyntax.C_EnumFromTo y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_EnumFromThenTo x1 x2 x3) (Curry.Module.CurrySyntax.C_EnumFromThenTo y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq (Curry.Module.CurrySyntax.C_UnaryMinus x1 x2) (Curry.Module.CurrySyntax.C_UnaryMinus y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_Apply x1 x2) (Curry.Module.CurrySyntax.C_Apply y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_InfixApply x1 x2 x3) (Curry.Module.CurrySyntax.C_InfixApply y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq (Curry.Module.CurrySyntax.C_LeftSection x1 x2) (Curry.Module.CurrySyntax.C_LeftSection y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_RightSection x1 x2) (Curry.Module.CurrySyntax.C_RightSection y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_Lambda x1 x2) (Curry.Module.CurrySyntax.C_Lambda y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_Let x1 x2) (Curry.Module.CurrySyntax.C_Let y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_Do x1 x2) (Curry.Module.CurrySyntax.C_Do y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_IfThenElse x1 x2 x3) (Curry.Module.CurrySyntax.C_IfThenElse y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq (Curry.Module.CurrySyntax.C_Case x1 x2) (Curry.Module.CurrySyntax.C_Case y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_RecordConstr x1) (Curry.Module.CurrySyntax.C_RecordConstr y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_RecordSelection x1 x2) (Curry.Module.CurrySyntax.C_RecordSelection y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.CurrySyntax.C_RecordUpdate x1 x2) (Curry.Module.CurrySyntax.C_RecordUpdate y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_Literal x1) (Curry.Module.CurrySyntax.C_Literal y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_Variable x1) (Curry.Module.CurrySyntax.C_Variable y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_Constructor x1) (Curry.Module.CurrySyntax.C_Constructor y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_Paren x1) (Curry.Module.CurrySyntax.C_Paren y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_Typed x1 x2) (Curry.Module.CurrySyntax.C_Typed y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_Tuple x1) (Curry.Module.CurrySyntax.C_Tuple y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_List x1) (Curry.Module.CurrySyntax.C_List y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_ListCompr x1 x2) (Curry.Module.CurrySyntax.C_ListCompr y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_EnumFrom x1) (Curry.Module.CurrySyntax.C_EnumFrom y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_EnumFromThen x1 x2) (Curry.Module.CurrySyntax.C_EnumFromThen y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_EnumFromTo x1 x2) (Curry.Module.CurrySyntax.C_EnumFromTo y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_EnumFromThenTo x1 x2 x3) (Curry.Module.CurrySyntax.C_EnumFromThenTo y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq (Curry.Module.CurrySyntax.C_UnaryMinus x1 x2) (Curry.Module.CurrySyntax.C_UnaryMinus y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_Apply x1 x2) (Curry.Module.CurrySyntax.C_Apply y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_InfixApply x1 x2 x3) (Curry.Module.CurrySyntax.C_InfixApply y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq (Curry.Module.CurrySyntax.C_LeftSection x1 x2) (Curry.Module.CurrySyntax.C_LeftSection y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_RightSection x1 x2) (Curry.Module.CurrySyntax.C_RightSection y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_Lambda x1 x2) (Curry.Module.CurrySyntax.C_Lambda y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_Let x1 x2) (Curry.Module.CurrySyntax.C_Let y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_Do x1 x2) (Curry.Module.CurrySyntax.C_Do y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_IfThenElse x1 x2 x3) (Curry.Module.CurrySyntax.C_IfThenElse y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq (Curry.Module.CurrySyntax.C_Case x1 x2) (Curry.Module.CurrySyntax.C_Case y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_RecordConstr x1) (Curry.Module.CurrySyntax.C_RecordConstr y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_RecordSelection x1 x2) (Curry.Module.CurrySyntax.C_RecordSelection y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.CurrySyntax.C_RecordUpdate x1 x2) (Curry.Module.CurrySyntax.C_RecordUpdate y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_Literal x1) st = Curry.Module.CurrySyntax.C_Literal(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_Variable x1) st = Curry.Module.CurrySyntax.C_Variable(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_Constructor x1) st = Curry.Module.CurrySyntax.C_Constructor(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_Paren x1) st = Curry.Module.CurrySyntax.C_Paren(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_Typed x1 x2) st = Curry.Module.CurrySyntax.C_Typed(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_Tuple x1) st = Curry.Module.CurrySyntax.C_Tuple(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_List x1) st = Curry.Module.CurrySyntax.C_List(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_ListCompr x1 x2) st = Curry.Module.CurrySyntax.C_ListCompr(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_EnumFrom x1) st = Curry.Module.CurrySyntax.C_EnumFrom(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_EnumFromThen x1 x2) st = Curry.Module.CurrySyntax.C_EnumFromThen(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_EnumFromTo x1 x2) st = Curry.Module.CurrySyntax.C_EnumFromTo(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_EnumFromThenTo x1 x2 x3) st = Curry.Module.CurrySyntax.C_EnumFromThenTo(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))
  propagate f (Curry.Module.CurrySyntax.C_UnaryMinus x1 x2) st = Curry.Module.CurrySyntax.C_UnaryMinus(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_Apply x1 x2) st = Curry.Module.CurrySyntax.C_Apply(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_InfixApply x1 x2 x3) st = Curry.Module.CurrySyntax.C_InfixApply(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))
  propagate f (Curry.Module.CurrySyntax.C_LeftSection x1 x2) st = Curry.Module.CurrySyntax.C_LeftSection(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_RightSection x1 x2) st = Curry.Module.CurrySyntax.C_RightSection(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_Lambda x1 x2) st = Curry.Module.CurrySyntax.C_Lambda(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_Let x1 x2) st = Curry.Module.CurrySyntax.C_Let(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_Do x1 x2) st = Curry.Module.CurrySyntax.C_Do(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_IfThenElse x1 x2 x3) st = Curry.Module.CurrySyntax.C_IfThenElse(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))
  propagate f (Curry.Module.CurrySyntax.C_Case x1 x2) st = Curry.Module.CurrySyntax.C_Case(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_RecordConstr x1) st = Curry.Module.CurrySyntax.C_RecordConstr(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_RecordSelection x1 x2) st = Curry.Module.CurrySyntax.C_RecordSelection(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.CurrySyntax.C_RecordUpdate x1 x2) st = Curry.Module.CurrySyntax.C_RecordUpdate(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_Literal x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_Variable x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_Constructor x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_Paren x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_Typed x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_Tuple x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_List x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_ListCompr x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_EnumFrom x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_EnumFromThen x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_EnumFromTo x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_EnumFromThenTo x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_UnaryMinus x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_Apply x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_InfixApply x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_LeftSection x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_RightSection x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_Lambda x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_Let x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_Do x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_IfThenElse x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_Case x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_RecordConstr x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_RecordSelection x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_RecordUpdate x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "Expression"

  showQ d (Curry.Module.CurrySyntax.C_Literal x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Literal "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_Variable x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Variable "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_Constructor x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Constructor "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_Paren x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Paren "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_Typed x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Typed "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_Tuple x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Tuple "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_List x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.List "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_ListCompr x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.ListCompr "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_EnumFrom x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.EnumFrom "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_EnumFromThen x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.EnumFromThen "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_EnumFromTo x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.EnumFromTo "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_EnumFromThenTo x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.EnumFromThenTo "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ d (Curry.Module.CurrySyntax.C_UnaryMinus x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.UnaryMinus "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_Apply x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Apply "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_InfixApply x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.InfixApply "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ d (Curry.Module.CurrySyntax.C_LeftSection x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.LeftSection "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_RightSection x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.RightSection "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_Lambda x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Lambda "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_Let x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Let "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_Do x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Do "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_IfThenElse x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.IfThenElse "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ d (Curry.Module.CurrySyntax.C_Case x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Case "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_RecordConstr x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.RecordConstr "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_RecordSelection x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.RecordSelection "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.CurrySyntax.C_RecordUpdate x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.RecordUpdate "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.CurrySyntax.C_ExpressionOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.CurrySyntax.C_InfixOp where
  strEq (Curry.Module.CurrySyntax.C_InfixOp x1) (Curry.Module.CurrySyntax.C_InfixOp y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_InfixConstr x1) (Curry.Module.CurrySyntax.C_InfixConstr y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_InfixOp x1) (Curry.Module.CurrySyntax.C_InfixOp y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_InfixConstr x1) (Curry.Module.CurrySyntax.C_InfixConstr y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_InfixOp x1) st = Curry.Module.CurrySyntax.C_InfixOp(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_InfixConstr x1) st = Curry.Module.CurrySyntax.C_InfixConstr(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_InfixOp x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_InfixConstr x1) st = f(x1)(c)(st)

  typeName _ = "InfixOp"

  showQ d (Curry.Module.CurrySyntax.C_InfixOp x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.InfixOp "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_InfixConstr x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.InfixConstr "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.CurrySyntax.C_InfixOpOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0) => Curry (Curry.Module.CurrySyntax.C_Statement t0) where
  strEq (Curry.Module.CurrySyntax.C_StmtExpr x1) (Curry.Module.CurrySyntax.C_StmtExpr y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_StmtDecl x1) (Curry.Module.CurrySyntax.C_StmtDecl y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurrySyntax.C_StmtBind x1 x2) (Curry.Module.CurrySyntax.C_StmtBind y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_StmtExpr x1) (Curry.Module.CurrySyntax.C_StmtExpr y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_StmtDecl x1) (Curry.Module.CurrySyntax.C_StmtDecl y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurrySyntax.C_StmtBind x1 x2) (Curry.Module.CurrySyntax.C_StmtBind y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_StmtExpr x1) st = Curry.Module.CurrySyntax.C_StmtExpr(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_StmtDecl x1) st = Curry.Module.CurrySyntax.C_StmtDecl(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurrySyntax.C_StmtBind x1 x2) st = Curry.Module.CurrySyntax.C_StmtBind(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_StmtExpr x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_StmtDecl x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurrySyntax.C_StmtBind x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "Statement"

  showQ d (Curry.Module.CurrySyntax.C_StmtExpr x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.StmtExpr "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_StmtDecl x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.StmtDecl "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurrySyntax.C_StmtBind x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.StmtBind "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.CurrySyntax.C_StatementOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0) => Curry (Curry.Module.CurrySyntax.C_Alt t0) where
  strEq (Curry.Module.CurrySyntax.C_Alt x1 x2 x3) (Curry.Module.CurrySyntax.C_Alt y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_Alt x1 x2 x3) (Curry.Module.CurrySyntax.C_Alt y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_Alt x1 x2 x3) st = Curry.Module.CurrySyntax.C_Alt(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_Alt x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)

  typeName _ = "Alt"

  showQ d (Curry.Module.CurrySyntax.C_Alt x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Alt "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ _ (Curry.Module.CurrySyntax.C_AltOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0,Curry t1) => Curry (Curry.Module.CurrySyntax.C_Field t0 t1) where
  strEq (Curry.Module.CurrySyntax.C_Field x1 x2 x3) (Curry.Module.CurrySyntax.C_Field y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurrySyntax.C_Field x1 x2 x3) (Curry.Module.CurrySyntax.C_Field y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurrySyntax.C_Field x1 x2 x3) st = Curry.Module.CurrySyntax.C_Field(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))

  foldCurry f c (Curry.Module.CurrySyntax.C_Field x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)

  typeName _ = "Field"

  showQ d (Curry.Module.CurrySyntax.C_Field x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurrySyntax.Field "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ _ (Curry.Module.CurrySyntax.C_FieldOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.CurrySyntax.C_Position where
  showsPrec d (Curry.Module.CurrySyntax.C_Position x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Position "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec _ (Curry.Module.CurrySyntax.C_PositionOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.CurrySyntax.C_Ident where
  showsPrec d (Curry.Module.CurrySyntax.C_Ident x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Ident "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.CurrySyntax.C_IdentOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.CurrySyntax.C_QualIdent where
  showsPrec d (Curry.Module.CurrySyntax.C_UnqualIdent x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("UnqualIdent "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_QualIdent x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("QualIdent "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.CurrySyntax.C_QualIdentOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.CurrySyntax.C_Module t0) where
  showsPrec d (Curry.Module.CurrySyntax.C_Module x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Module "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec _ (Curry.Module.CurrySyntax.C_ModuleOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.CurrySyntax.C_ExportSpec t0) where
  showsPrec d (Curry.Module.CurrySyntax.C_Exporting x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Exporting "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.CurrySyntax.C_ExportSpecOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.CurrySyntax.C_Export where
  showsPrec d (Curry.Module.CurrySyntax.C_Export x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Export "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_ExportTypeWith x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ExportTypeWith "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_ExportTypeAll x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ExportTypeAll "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_ExportModule x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ExportModule "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.CurrySyntax.C_ExportOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.CurrySyntax.C_ImportSpec t0) where
  showsPrec d (Curry.Module.CurrySyntax.C_Importing x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Importing "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_Hiding x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Hiding "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.CurrySyntax.C_ImportSpecOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.CurrySyntax.C_Import where
  showsPrec d (Curry.Module.CurrySyntax.C_Import x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Import "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_ImportTypeWith x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ImportTypeWith "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_ImportTypeAll x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ImportTypeAll "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.CurrySyntax.C_ImportOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.CurrySyntax.C_Decl t0) where
  showsPrec d (Curry.Module.CurrySyntax.C_ImportDecl x1 x2 x3 x4 x5) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ImportDecl "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x5))))))))))


  showsPrec d (Curry.Module.CurrySyntax.C_InfixDecl x1 x2 x3 x4) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("InfixDecl "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))))))))


  showsPrec d (Curry.Module.CurrySyntax.C_DataDecl x1 x2 x3 x4) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("DataDecl "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))))))))


  showsPrec d (Curry.Module.CurrySyntax.C_NewtypeDecl x1 x2 x3 x4) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("NewtypeDecl "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))))))))


  showsPrec d (Curry.Module.CurrySyntax.C_TypeDecl x1 x2 x3 x4) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("TypeDecl "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))))))))


  showsPrec d (Curry.Module.CurrySyntax.C_TypeSig x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("TypeSig "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec d (Curry.Module.CurrySyntax.C_EvalAnnot x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("EvalAnnot "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec d (Curry.Module.CurrySyntax.C_FunctionDecl x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FunctionDecl "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec d (Curry.Module.CurrySyntax.C_ExternalDecl x1 x2 x3 x4 x5) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ExternalDecl "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x5))))))))))


  showsPrec d (Curry.Module.CurrySyntax.C_FlatExternalDecl x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FlatExternalDecl "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_PatternDecl x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("PatternDecl "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec d (Curry.Module.CurrySyntax.C_ExtraVariables x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ExtraVariables "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.CurrySyntax.C_DeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.CurrySyntax.C_ConstrDecl t0) where
  showsPrec d (Curry.Module.CurrySyntax.C_ConstrDecl x1 x2 x3 x4) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ConstrDecl "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))))))))


  showsPrec d (Curry.Module.CurrySyntax.C_ConOpDecl x1 x2 x3 x4 x5) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ConOpDecl "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x5))))))))))


  showsPrec _ (Curry.Module.CurrySyntax.C_ConstrDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.CurrySyntax.C_NewConstrDecl t0) where
  showsPrec d (Curry.Module.CurrySyntax.C_NewConstrDecl x1 x2 x3 x4) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("NewConstrDecl "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))))))))


  showsPrec _ (Curry.Module.CurrySyntax.C_NewConstrDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.CurrySyntax.C_Infix where
  showsPrec _ Curry.Module.CurrySyntax.C_InfixL = Prelude.showString("InfixL")
  showsPrec _ Curry.Module.CurrySyntax.C_InfixR = Prelude.showString("InfixR")
  showsPrec _ Curry.Module.CurrySyntax.C_Infix = Prelude.showString("Infix")
  showsPrec _ (Curry.Module.CurrySyntax.C_InfixOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.CurrySyntax.C_EvalAnnotation where
  showsPrec _ Curry.Module.CurrySyntax.C_EvalRigid = Prelude.showString("EvalRigid")
  showsPrec _ Curry.Module.CurrySyntax.C_EvalChoice = Prelude.showString("EvalChoice")
  showsPrec _ (Curry.Module.CurrySyntax.C_EvalAnnotationOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.CurrySyntax.C_CallConv where
  showsPrec _ Curry.Module.CurrySyntax.C_CallConvPrimitive = Prelude.showString("CallConvPrimitive")
  showsPrec _ Curry.Module.CurrySyntax.C_CallConvCCall = Prelude.showString("CallConvCCall")
  showsPrec _ (Curry.Module.CurrySyntax.C_CallConvOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.CurrySyntax.C_TypeExpr where
  showsPrec d (Curry.Module.CurrySyntax.C_ConstructorType x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ConstructorType "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_VariableType x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("VariableType "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_TupleType x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("TupleType "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_ListType x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ListType "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_ArrowType x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ArrowType "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_RecordType x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("RecordType "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.CurrySyntax.C_TypeExprOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.CurrySyntax.C_Equation t0) where
  showsPrec d (Curry.Module.CurrySyntax.C_Equation x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Equation "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec _ (Curry.Module.CurrySyntax.C_EquationOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.CurrySyntax.C_Lhs t0) where
  showsPrec d (Curry.Module.CurrySyntax.C_FunLhs x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FunLhs "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_OpLhs x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("OpLhs "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec d (Curry.Module.CurrySyntax.C_ApLhs x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ApLhs "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.CurrySyntax.C_LhsOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.CurrySyntax.C_Rhs t0) where
  showsPrec d (Curry.Module.CurrySyntax.C_SimpleRhs x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("SimpleRhs "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec d (Curry.Module.CurrySyntax.C_GuardedRhs x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("GuardedRhs "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.CurrySyntax.C_RhsOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.CurrySyntax.C_CondExpr t0) where
  showsPrec d (Curry.Module.CurrySyntax.C_CondExpr x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CondExpr "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec _ (Curry.Module.CurrySyntax.C_CondExprOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.CurrySyntax.C_Literal where
  showsPrec d (Curry.Module.CurrySyntax.C_Char x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Char "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_Int x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Int "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_Float x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Float "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_String x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("String "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.CurrySyntax.C_LiteralOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.CurrySyntax.C_ConstrTerm t0) where
  showsPrec d (Curry.Module.CurrySyntax.C_LiteralPattern x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("LiteralPattern "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_NegativePattern x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("NegativePattern "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_VariablePattern x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("VariablePattern "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_ConstructorPattern x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ConstructorPattern "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_InfixPattern x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("InfixPattern "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec d (Curry.Module.CurrySyntax.C_ParenPattern x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ParenPattern "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_TuplePattern x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("TuplePattern "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_ListPattern x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ListPattern "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_AsPattern x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AsPattern "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_LazyPattern x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("LazyPattern "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_FunctionPattern x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FunctionPattern "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_InfixFuncPattern x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("InfixFuncPattern "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec d (Curry.Module.CurrySyntax.C_RecordPattern x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("RecordPattern "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.CurrySyntax.C_ConstrTermOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.CurrySyntax.C_Expression t0) where
  showsPrec d (Curry.Module.CurrySyntax.C_Literal x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Literal "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_Variable x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Variable "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_Constructor x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Constructor "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_Paren x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Paren "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_Typed x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Typed "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_Tuple x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Tuple "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_List x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("List "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_ListCompr x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ListCompr "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_EnumFrom x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("EnumFrom "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_EnumFromThen x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("EnumFromThen "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_EnumFromTo x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("EnumFromTo "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_EnumFromThenTo x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("EnumFromThenTo "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec d (Curry.Module.CurrySyntax.C_UnaryMinus x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("UnaryMinus "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_Apply x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Apply "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_InfixApply x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("InfixApply "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec d (Curry.Module.CurrySyntax.C_LeftSection x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("LeftSection "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_RightSection x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("RightSection "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_Lambda x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Lambda "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_Let x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Let "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_Do x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Do "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_IfThenElse x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("IfThenElse "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec d (Curry.Module.CurrySyntax.C_Case x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Case "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_RecordConstr x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("RecordConstr "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_RecordSelection x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("RecordSelection "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.CurrySyntax.C_RecordUpdate x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("RecordUpdate "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.CurrySyntax.C_ExpressionOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.CurrySyntax.C_InfixOp where
  showsPrec d (Curry.Module.CurrySyntax.C_InfixOp x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("InfixOp "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_InfixConstr x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("InfixConstr "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.CurrySyntax.C_InfixOpOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.CurrySyntax.C_Statement t0) where
  showsPrec d (Curry.Module.CurrySyntax.C_StmtExpr x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("StmtExpr "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_StmtDecl x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("StmtDecl "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurrySyntax.C_StmtBind x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("StmtBind "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.CurrySyntax.C_StatementOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.CurrySyntax.C_Alt t0) where
  showsPrec d (Curry.Module.CurrySyntax.C_Alt x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Alt "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec _ (Curry.Module.CurrySyntax.C_AltOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0,Show t1) => Show (Curry.Module.CurrySyntax.C_Field t0 t1) where
  showsPrec d (Curry.Module.CurrySyntax.C_Field x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Field "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec _ (Curry.Module.CurrySyntax.C_FieldOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Read Curry.Module.CurrySyntax.C_Position where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Position(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Position")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r)





instance Read Curry.Module.CurrySyntax.C_Ident where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Ident(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Ident")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r)





instance Read Curry.Module.CurrySyntax.C_QualIdent where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_UnqualIdent(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("UnqualIdent")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_QualIdent(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("QualIdent")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))





instance (Read t0) => Read (Curry.Module.CurrySyntax.C_Module t0) where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Module(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Module")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r)





instance (Read t0) => Read (Curry.Module.CurrySyntax.C_ExportSpec t0) where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Exporting(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Exporting")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r)





instance Read Curry.Module.CurrySyntax.C_Export where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Export(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Export")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_ExportTypeWith(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("ExportTypeWith")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_ExportTypeAll(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("ExportTypeAll")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_ExportModule(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("ExportModule")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))))





instance (Read t0) => Read (Curry.Module.CurrySyntax.C_ImportSpec t0) where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Importing(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Importing")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Hiding(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Hiding")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))





instance Read Curry.Module.CurrySyntax.C_Import where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Import(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Import")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_ImportTypeWith(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("ImportTypeWith")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_ImportTypeAll(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("ImportTypeAll")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r)))





instance (Read t0) => Read (Curry.Module.CurrySyntax.C_Decl t0) where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_ImportDecl(x1)(x2)(x3)(x4)(x5))(r5) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("ImportDecl")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3), ((,) x5 r5) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r4)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_InfixDecl(x1)(x2)(x3)(x4))(r4) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("InfixDecl")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_DataDecl(x1)(x2)(x3)(x4))(r4) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("DataDecl")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_NewtypeDecl(x1)(x2)(x3)(x4))(r4) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("NewtypeDecl")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_TypeDecl(x1)(x2)(x3)(x4))(r4) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("TypeDecl")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_TypeSig(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("TypeSig")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_EvalAnnot(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("EvalAnnot")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_FunctionDecl(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("FunctionDecl")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_ExternalDecl(x1)(x2)(x3)(x4)(x5))(r5) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("ExternalDecl")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3), ((,) x5 r5) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r4)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_FlatExternalDecl(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("FlatExternalDecl")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_PatternDecl(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("PatternDecl")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_ExtraVariables(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("ExtraVariables")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))))))))))))





instance (Read t0) => Read (Curry.Module.CurrySyntax.C_ConstrDecl t0) where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_ConstrDecl(x1)(x2)(x3)(x4))(r4) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("ConstrDecl")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_ConOpDecl(x1)(x2)(x3)(x4)(x5))(r5) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("ConOpDecl")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3), ((,) x5 r5) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r4)])(r))





instance (Read t0) => Read (Curry.Module.CurrySyntax.C_NewConstrDecl t0) where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_NewConstrDecl(x1)(x2)(x3)(x4))(r4) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("NewConstrDecl")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3)])(r)





instance Read Curry.Module.CurrySyntax.C_Infix where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.CurrySyntax.C_InfixL)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("InfixL")(r)])(r))((Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.CurrySyntax.C_InfixR)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("InfixR")(r)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.CurrySyntax.C_Infix)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Infix")(r)])(r)))





instance Read Curry.Module.CurrySyntax.C_EvalAnnotation where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.CurrySyntax.C_EvalRigid)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("EvalRigid")(r)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.CurrySyntax.C_EvalChoice)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("EvalChoice")(r)])(r))





instance Read Curry.Module.CurrySyntax.C_CallConv where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.CurrySyntax.C_CallConvPrimitive)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("CallConvPrimitive")(r)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.CurrySyntax.C_CallConvCCall)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("CallConvCCall")(r)])(r))





instance Read Curry.Module.CurrySyntax.C_TypeExpr where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_ConstructorType(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("ConstructorType")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_VariableType(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("VariableType")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_TupleType(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("TupleType")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_ListType(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("ListType")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_ArrowType(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("ArrowType")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_RecordType(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("RecordType")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))))))





instance (Read t0) => Read (Curry.Module.CurrySyntax.C_Equation t0) where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Equation(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Equation")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r)





instance (Read t0) => Read (Curry.Module.CurrySyntax.C_Lhs t0) where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_FunLhs(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("FunLhs")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_OpLhs(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("OpLhs")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_ApLhs(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("ApLhs")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r)))





instance (Read t0) => Read (Curry.Module.CurrySyntax.C_Rhs t0) where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_SimpleRhs(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("SimpleRhs")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_GuardedRhs(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("GuardedRhs")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))





instance (Read t0) => Read (Curry.Module.CurrySyntax.C_CondExpr t0) where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_CondExpr(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("CondExpr")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r)





instance Read Curry.Module.CurrySyntax.C_Literal where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Char(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Char")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Int(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Int")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Float(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Float")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_String(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("String")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))))





instance (Read t0) => Read (Curry.Module.CurrySyntax.C_ConstrTerm t0) where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_LiteralPattern(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("LiteralPattern")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_NegativePattern(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("NegativePattern")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_VariablePattern(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("VariablePattern")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_ConstructorPattern(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("ConstructorPattern")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_InfixPattern(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("InfixPattern")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_ParenPattern(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("ParenPattern")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_TuplePattern(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("TuplePattern")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_ListPattern(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("ListPattern")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_AsPattern(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("AsPattern")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_LazyPattern(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("LazyPattern")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_FunctionPattern(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("FunctionPattern")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_InfixFuncPattern(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("InfixFuncPattern")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_RecordPattern(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("RecordPattern")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r)))))))))))))





instance (Read t0) => Read (Curry.Module.CurrySyntax.C_Expression t0) where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Literal(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Literal")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Variable(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Variable")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Constructor(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Constructor")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Paren(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Paren")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Typed(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Typed")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Tuple(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Tuple")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_List(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("List")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_ListCompr(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("ListCompr")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_EnumFrom(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("EnumFrom")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_EnumFromThen(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("EnumFromThen")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_EnumFromTo(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("EnumFromTo")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_EnumFromThenTo(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("EnumFromThenTo")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_UnaryMinus(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("UnaryMinus")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Apply(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Apply")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_InfixApply(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("InfixApply")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_LeftSection(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("LeftSection")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_RightSection(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("RightSection")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Lambda(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Lambda")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Let(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Let")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Do(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Do")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_IfThenElse(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("IfThenElse")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Case(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Case")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_RecordConstr(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("RecordConstr")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_RecordSelection(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("RecordSelection")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_RecordUpdate(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("RecordUpdate")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r)))))))))))))))))))))))))





instance Read Curry.Module.CurrySyntax.C_InfixOp where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_InfixOp(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("InfixOp")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_InfixConstr(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("InfixConstr")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))





instance (Read t0) => Read (Curry.Module.CurrySyntax.C_Statement t0) where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_StmtExpr(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("StmtExpr")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_StmtDecl(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("StmtDecl")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_StmtBind(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("StmtBind")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r)))





instance (Read t0) => Read (Curry.Module.CurrySyntax.C_Alt t0) where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Alt(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Alt")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r)





instance (Read t0,Read t1) => Read (Curry.Module.CurrySyntax.C_Field t0 t1) where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurrySyntax.C_Field(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurrySyntax")("Field")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r)





c_readCurry :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.CurrySyntax.C_Module (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int))
c_readCurry x1 st = Curry.Module.CurrySyntax.c_readCurryWithParseOptions(x1)(Curry.Module.Distribution.c_setQuiet(Curry.Module.Prelude.C_True)(Curry.Module.Distribution.c_defaultParams(st))(st))(st)



c_readCurryWithParseOptions :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Distribution.C_FrontendParams -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.CurrySyntax.C_Module (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int))
c_readCurryWithParseOptions x1 x2 st = let {x3 = Curry.Module.Prelude.c_apply(Curry.Module.CurrySyntax.c_stripSuffix(st))(x1)(st)} in Curry.Module.Prelude.op_62_62(Curry.Module.CurrySyntax.c_orElseDo(Curry.Module.CurrySyntax.c_readCurryWithParseOptions'46parseCurry'465(x2)(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st))(Curry.Module.CurrySyntax.c_readCurryWithParseOptions'46parseCurry'465(x2)(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))(st))(st))(Curry.Module.CurrySyntax.c_readCurryFile(x3)(st))(st)



c_readCurryWithParseOptions'46parse'465 :: (Curry t0) => Curry.Module.Distribution.C_FrontendParams -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe Curry.Module.Prelude.T0)
c_readCurryWithParseOptions'46parse'465 x1 x2 x3 st = Curry.Module.Prelude.op_62_62(Curry.Module.Distribution.c_callFrontendWithParams(Curry.Module.Distribution.C_CY)(x1)(x2)(st))(Curry.Module.Prelude.c_return(Curry.Module.Prelude.C_Just(Curry.Module.Prelude.T0))(st))(st)



c_readCurryWithParseOptions'46parseCurry'465 :: Curry.Module.Distribution.C_FrontendParams -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe Curry.Module.Prelude.T0)
c_readCurryWithParseOptions'46parseCurry'465 x1 x2 x3 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.CurrySyntax.c_absoluteFileName(Curry.Module.Prelude.op_43_43(x2)(x3)(st))(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.c_return(Curry.Module.Prelude.C_Nothing)(st))(Curry.Module.Prelude.pf(Curry.Module.CurrySyntax.c_readCurryWithParseOptions'46parse'465(x1)(x2)))))(st)



c_readCurryFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.CurrySyntax.C_Module (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int))
c_readCurryFile x1 st = let {x2 = Curry.Module.Prelude.c_apply(Curry.Module.CurrySyntax.c_stripSuffix(st))(x1)(st)} in Curry.Module.Prelude.op_62_62_61(Curry.Module.CurrySyntax.c_absoluteFileName(Curry.Module.Prelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.CurrySyntax.c_readCurryFile'46_'35lambda3(x2)))(st)



c_readCurryFile'46read'4613 :: (Curry t0) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_readCurryFile'46read'4613 x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_readFile(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.CurrySyntax.c_readCurryFile'46read'4613'46_'35lambda2))(st)



c_readCurryFile'46read'4613'46_'35lambda2 :: (Curry t48) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t48
c_readCurryFile'46read'4613'46_'35lambda2 x1 st = Curry.Module.Prelude.c_return(Curry.Module.ReadShowTerm.c_readUnqualifiedTerm((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))(Curry.Module.Prelude.List))))))))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(Curry.Module.Prelude.List)))(x1)(st))(st)



c_readCurryFile'46_'35lambda3 :: (Curry t49) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t49
c_readCurryFile'46_'35lambda3 x1 x2 st = Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_error))(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))(Curry.Module.Prelude.List))))))))))))))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.CurrySyntax.c_readCurryFile'46read'4613))(x2)(st)



c_writeCurryModule :: (Curry.Module.CurrySyntax.C_Module (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_writeCurryModule x1@(Curry.Module.CurrySyntax.C_Module x2 x3 x4) st = Curry.Module.CurrySyntax.c_writeCurryFile(Curry.Module.Prelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))(st))(x1)(st)
c_writeCurryModule (Curry.Module.CurrySyntax.C_ModuleOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurrySyntax.c_writeCurryModule(x)(st))(i)(xs)(st)
c_writeCurryModule x st = Curry.RunTimeSystem.patternFail("CurrySyntax.writeCurryModule")(x)



c_writeCurryFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.CurrySyntax.C_Module (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_writeCurryFile x1 x2 st = Curry.Module.Prelude.c_writeFile(x1)(Curry.Module.ReadShowTerm.c_showTerm(x2)(st))(st)



c_absoluteFileName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_absoluteFileName x1 st = Curry.Module.CurrySyntax.c_absoluteFileName_case_2(x1)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.FileGoodies.c_baseName(x1)(st))(st))(st)



c_absoluteFileName'46_'35lambda4 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_absoluteFileName'46_'35lambda4 x1 x2 st = Curry.Module.Prelude.c_return(Curry.Module.CurrySyntax.c_absoluteFileName'46_'35lambda4_case_0(x1)(x2)(st))(st)



c_orElseDo :: (Curry t0) => (Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t0)) -> (Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t0)
c_orElseDo x1 x2 st = Curry.Module.Prelude.op_62_62_61(x1)(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_maybe(x2)(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Prelude.pc(Curry.Module.Prelude.C_Just))(st))))(st)



c_stripSuffix :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_stripSuffix st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_takeWhile(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_47_61))(Curry.Module.Prelude.C_Char('.')))))



c_absoluteFileName'46_'35lambda4_case_0 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_Just(x1)
c_absoluteFileName'46_'35lambda4_case_0 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Nothing
c_absoluteFileName'46_'35lambda4_case_0 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurrySyntax.c_absoluteFileName'46_'35lambda4_case_0(x1)(x)(st))(i)(xs)(st)
c_absoluteFileName'46_'35lambda4_case_0 x1 x st = Curry.RunTimeSystem.patternFail("CurrySyntax.absoluteFileName._#lambda4_case_0")(x)



c_absoluteFileName_case_2 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Distribution.c_lookupFileInLoadPath(x1)(st)
c_absoluteFileName_case_2 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.CurrySyntax.c_absoluteFileName_case_1(x1)(Curry.Module.Prelude.c_otherwise(st))(st)
c_absoluteFileName_case_2 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurrySyntax.c_absoluteFileName_case_2(x1)(x)(st))(i)(xs)(st)
c_absoluteFileName_case_2 x1 x st = Curry.RunTimeSystem.patternFail("CurrySyntax.absoluteFileName_case_2")(x)



c_absoluteFileName_case_1 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesFileExist(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.CurrySyntax.c_absoluteFileName'46_'35lambda4(x1)))(st)
c_absoluteFileName_case_1 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurrySyntax.c_absoluteFileName_case_1(x1)(x)(st))(i)(xs)(st)
c_absoluteFileName_case_1 x1 x st = Curry.RunTimeSystem.patternFail("CurrySyntax.absoluteFileName_case_1")(x)


