{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.AbstractHaskell (module Curry.Module.AbstractHaskell) where

import Curry.RunTimeSystem
import Curry.Module.AbstractCurry
import Curry.Module.Prelude
import Curry.Module.AbstractCurryPrinter



-- begin included



-- end included

data C_TypeClass = C_TypeClass (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeExpr)
  | C_TypeClassFail Curry.RunTimeSystem.C_Exceptions
  | C_TypeClassOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractHaskell.C_TypeClass)

data C_InstanceDecl = C_Instance (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_TypeClass) Curry.Module.AbstractHaskell.C_TypeClass (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HFuncDecl)
  | C_InstanceDeclFail Curry.RunTimeSystem.C_Exceptions
  | C_InstanceDeclOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractHaskell.C_InstanceDecl)

data C_HFuncDecl = C_HFunc (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int Curry.Module.AbstractCurry.C_CVisibility (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_TypeClass) Curry.Module.AbstractCurry.C_CTypeExpr Curry.Module.AbstractCurry.C_CRules
  | C_HFuncDeclFail Curry.RunTimeSystem.C_Exceptions
  | C_HFuncDeclOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractHaskell.C_HFuncDecl)

data C_HaskellProg = C_HaskellProg (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HTypeDecl) (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_InstanceDecl) (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HFuncDecl) (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_COpDecl)
  | C_HaskellProgFail Curry.RunTimeSystem.C_Exceptions
  | C_HaskellProgOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractHaskell.C_HaskellProg)

data C_HTypeDecl = C_HTypeDecl (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_TypeClass) Curry.Module.AbstractCurry.C_CTypeDecl (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
  | C_HTypeDeclFail Curry.RunTimeSystem.C_Exceptions
  | C_HTypeDeclOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.AbstractHaskell.C_HTypeDecl)

instance BaseCurry Curry.Module.AbstractHaskell.C_TypeClass where
  nf f (Curry.Module.AbstractHaskell.C_TypeClass x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.AbstractHaskell.C_TypeClass(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.AbstractHaskell.C_TypeClass x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.AbstractHaskell.C_TypeClass(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractHaskell.C_TypeClassOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.AbstractHaskell.C_TypeClass(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.AbstractHaskell.C_TypeClassFail

  branching  = Curry.Module.AbstractHaskell.C_TypeClassOr

  consKind (Curry.Module.AbstractHaskell.C_TypeClassOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractHaskell.C_TypeClassFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractHaskell.C_TypeClassFail x) = x

  orRef (Curry.Module.AbstractHaskell.C_TypeClassOr x _) = x

  branches (Curry.Module.AbstractHaskell.C_TypeClassOr _ x) = x





instance BaseCurry Curry.Module.AbstractHaskell.C_InstanceDecl where
  nf f (Curry.Module.AbstractHaskell.C_Instance x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.AbstractHaskell.C_Instance(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.AbstractHaskell.C_Instance x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.AbstractHaskell.C_Instance(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractHaskell.C_InstanceDeclOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.AbstractHaskell.C_Instance(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(3)

  failed  = Curry.Module.AbstractHaskell.C_InstanceDeclFail

  branching  = Curry.Module.AbstractHaskell.C_InstanceDeclOr

  consKind (Curry.Module.AbstractHaskell.C_InstanceDeclOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractHaskell.C_InstanceDeclFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractHaskell.C_InstanceDeclFail x) = x

  orRef (Curry.Module.AbstractHaskell.C_InstanceDeclOr x _) = x

  branches (Curry.Module.AbstractHaskell.C_InstanceDeclOr _ x) = x





instance BaseCurry Curry.Module.AbstractHaskell.C_HFuncDecl where
  nf f (Curry.Module.AbstractHaskell.C_HFunc x1 x2 x3 x4 x5 x6) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> Curry.RunTimeSystem.nfCTC(\ v6 state6 -> f(Curry.Module.AbstractHaskell.C_HFunc(v1)(v2)(v3)(v4)(v5)(v6))(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.AbstractHaskell.C_HFunc x1 x2 x3 x4 x5 x6) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> Curry.RunTimeSystem.gnfCTC(\ v6 state6 -> f(Curry.Module.AbstractHaskell.C_HFunc(v1)(v2)(v3)(v4)(v5)(v6))(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractHaskell.C_HFuncDeclOr(Curry.RunTimeSystem.mkRef(r)(6)(i))([Curry.Module.AbstractHaskell.C_HFunc(Curry.RunTimeSystem.generator((Prelude.+)(r)((5::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(6)

  failed  = Curry.Module.AbstractHaskell.C_HFuncDeclFail

  branching  = Curry.Module.AbstractHaskell.C_HFuncDeclOr

  consKind (Curry.Module.AbstractHaskell.C_HFuncDeclOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractHaskell.C_HFuncDeclFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractHaskell.C_HFuncDeclFail x) = x

  orRef (Curry.Module.AbstractHaskell.C_HFuncDeclOr x _) = x

  branches (Curry.Module.AbstractHaskell.C_HFuncDeclOr _ x) = x





instance BaseCurry Curry.Module.AbstractHaskell.C_HaskellProg where
  nf f (Curry.Module.AbstractHaskell.C_HaskellProg x1 x2 x3 x4 x5 x6 x7 x8) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> Curry.RunTimeSystem.nfCTC(\ v6 state6 -> Curry.RunTimeSystem.nfCTC(\ v7 state7 -> Curry.RunTimeSystem.nfCTC(\ v8 state8 -> f(Curry.Module.AbstractHaskell.C_HaskellProg(v1)(v2)(v3)(v4)(v5)(v6)(v7)(v8))(state8))(x8)(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.AbstractHaskell.C_HaskellProg x1 x2 x3 x4 x5 x6 x7 x8) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> Curry.RunTimeSystem.gnfCTC(\ v6 state6 -> Curry.RunTimeSystem.gnfCTC(\ v7 state7 -> Curry.RunTimeSystem.gnfCTC(\ v8 state8 -> f(Curry.Module.AbstractHaskell.C_HaskellProg(v1)(v2)(v3)(v4)(v5)(v6)(v7)(v8))(state8))(x8)(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractHaskell.C_HaskellProgOr(Curry.RunTimeSystem.mkRef(r)(8)(i))([Curry.Module.AbstractHaskell.C_HaskellProg(Curry.RunTimeSystem.generator((Prelude.+)(r)((7::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((6::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((5::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(8)

  failed  = Curry.Module.AbstractHaskell.C_HaskellProgFail

  branching  = Curry.Module.AbstractHaskell.C_HaskellProgOr

  consKind (Curry.Module.AbstractHaskell.C_HaskellProgOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractHaskell.C_HaskellProgFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractHaskell.C_HaskellProgFail x) = x

  orRef (Curry.Module.AbstractHaskell.C_HaskellProgOr x _) = x

  branches (Curry.Module.AbstractHaskell.C_HaskellProgOr _ x) = x





instance BaseCurry Curry.Module.AbstractHaskell.C_HTypeDecl where
  nf f (Curry.Module.AbstractHaskell.C_HTypeDecl x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.AbstractHaskell.C_HTypeDecl(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.AbstractHaskell.C_HTypeDecl x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.AbstractHaskell.C_HTypeDecl(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.AbstractHaskell.C_HTypeDeclOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.AbstractHaskell.C_HTypeDecl(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(3)

  failed  = Curry.Module.AbstractHaskell.C_HTypeDeclFail

  branching  = Curry.Module.AbstractHaskell.C_HTypeDeclOr

  consKind (Curry.Module.AbstractHaskell.C_HTypeDeclOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.AbstractHaskell.C_HTypeDeclFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.AbstractHaskell.C_HTypeDeclFail x) = x

  orRef (Curry.Module.AbstractHaskell.C_HTypeDeclOr x _) = x

  branches (Curry.Module.AbstractHaskell.C_HTypeDeclOr _ x) = x





instance Curry Curry.Module.AbstractHaskell.C_TypeClass where
  strEq (Curry.Module.AbstractHaskell.C_TypeClass x1 x2) (Curry.Module.AbstractHaskell.C_TypeClass y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.AbstractHaskell.C_TypeClass x1 x2) (Curry.Module.AbstractHaskell.C_TypeClass y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.AbstractHaskell.C_TypeClass x1 x2) st = Curry.Module.AbstractHaskell.C_TypeClass(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.AbstractHaskell.C_TypeClass x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "TypeClass"

  showQ d (Curry.Module.AbstractHaskell.C_TypeClass x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractHaskell.TypeClass "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.AbstractHaskell.C_TypeClassOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractHaskell.C_InstanceDecl where
  strEq (Curry.Module.AbstractHaskell.C_Instance x1 x2 x3) (Curry.Module.AbstractHaskell.C_Instance y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.AbstractHaskell.C_Instance x1 x2 x3) (Curry.Module.AbstractHaskell.C_Instance y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.AbstractHaskell.C_Instance x1 x2 x3) st = Curry.Module.AbstractHaskell.C_Instance(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))

  foldCurry f c (Curry.Module.AbstractHaskell.C_Instance x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)

  typeName _ = "InstanceDecl"

  showQ d (Curry.Module.AbstractHaskell.C_Instance x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractHaskell.Instance "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ _ (Curry.Module.AbstractHaskell.C_InstanceDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractHaskell.C_HFuncDecl where
  strEq (Curry.Module.AbstractHaskell.C_HFunc x1 x2 x3 x4 x5 x6) (Curry.Module.AbstractHaskell.C_HFunc y1 y2 y3 y4 y5 y6) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(Curry.Module.Prelude.genStrEq(x6)(y6)(st))(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.AbstractHaskell.C_HFunc x1 x2 x3 x4 x5 x6) (Curry.Module.AbstractHaskell.C_HFunc y1 y2 y3 y4 y5 y6) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x5)(y5)(st))(Curry.Module.Prelude.genEq(x6)(y6)(st))(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.AbstractHaskell.C_HFunc x1 x2 x3 x4 x5 x6) st = Curry.Module.AbstractHaskell.C_HFunc(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))(f((5::Int))(x6)(st))

  foldCurry f c (Curry.Module.AbstractHaskell.C_HFunc x1 x2 x3 x4 x5 x6) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(f(x6)(c)(st))(st))(st))(st))(st))(st)

  typeName _ = "HFuncDecl"

  showQ d (Curry.Module.AbstractHaskell.C_HFunc x1 x2 x3 x4 x5 x6) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractHaskell.HFunc "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x5))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x6))))))))))))


  showQ _ (Curry.Module.AbstractHaskell.C_HFuncDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractHaskell.C_HaskellProg where
  strEq (Curry.Module.AbstractHaskell.C_HaskellProg x1 x2 x3 x4 x5 x6 x7 x8) (Curry.Module.AbstractHaskell.C_HaskellProg y1 y2 y3 y4 y5 y6 y7 y8) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x6)(y6)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x7)(y7)(st))(Curry.Module.Prelude.genStrEq(x8)(y8)(st))(st))(st))(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.AbstractHaskell.C_HaskellProg x1 x2 x3 x4 x5 x6 x7 x8) (Curry.Module.AbstractHaskell.C_HaskellProg y1 y2 y3 y4 y5 y6 y7 y8) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x5)(y5)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x6)(y6)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x7)(y7)(st))(Curry.Module.Prelude.genEq(x8)(y8)(st))(st))(st))(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.AbstractHaskell.C_HaskellProg x1 x2 x3 x4 x5 x6 x7 x8) st = Curry.Module.AbstractHaskell.C_HaskellProg(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))(f((5::Int))(x6)(st))(f((6::Int))(x7)(st))(f((7::Int))(x8)(st))

  foldCurry f c (Curry.Module.AbstractHaskell.C_HaskellProg x1 x2 x3 x4 x5 x6 x7 x8) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(f(x6)(f(x7)(f(x8)(c)(st))(st))(st))(st))(st))(st))(st))(st)

  typeName _ = "HaskellProg"

  showQ d (Curry.Module.AbstractHaskell.C_HaskellProg x1 x2 x3 x4 x5 x6 x7 x8) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractHaskell.HaskellProg "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x5))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x6))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x7))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x8))))))))))))))))


  showQ _ (Curry.Module.AbstractHaskell.C_HaskellProgOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.AbstractHaskell.C_HTypeDecl where
  strEq (Curry.Module.AbstractHaskell.C_HTypeDecl x1 x2 x3) (Curry.Module.AbstractHaskell.C_HTypeDecl y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.AbstractHaskell.C_HTypeDecl x1 x2 x3) (Curry.Module.AbstractHaskell.C_HTypeDecl y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.AbstractHaskell.C_HTypeDecl x1 x2 x3) st = Curry.Module.AbstractHaskell.C_HTypeDecl(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))

  foldCurry f c (Curry.Module.AbstractHaskell.C_HTypeDecl x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)

  typeName _ = "HTypeDecl"

  showQ d (Curry.Module.AbstractHaskell.C_HTypeDecl x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AbstractHaskell.HTypeDecl "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ _ (Curry.Module.AbstractHaskell.C_HTypeDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractHaskell.C_TypeClass where
  showsPrec d (Curry.Module.AbstractHaskell.C_TypeClass x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("TypeClass "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.AbstractHaskell.C_TypeClassOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractHaskell.C_InstanceDecl where
  showsPrec d (Curry.Module.AbstractHaskell.C_Instance x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Instance "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec _ (Curry.Module.AbstractHaskell.C_InstanceDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractHaskell.C_HFuncDecl where
  showsPrec d (Curry.Module.AbstractHaskell.C_HFunc x1 x2 x3 x4 x5 x6) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("HFunc "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x5))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x6))))))))))))


  showsPrec _ (Curry.Module.AbstractHaskell.C_HFuncDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractHaskell.C_HaskellProg where
  showsPrec d (Curry.Module.AbstractHaskell.C_HaskellProg x1 x2 x3 x4 x5 x6 x7 x8) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("HaskellProg "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x5))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x6))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x7))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x8))))))))))))))))


  showsPrec _ (Curry.Module.AbstractHaskell.C_HaskellProgOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.AbstractHaskell.C_HTypeDecl where
  showsPrec d (Curry.Module.AbstractHaskell.C_HTypeDecl x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("HTypeDecl "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec _ (Curry.Module.AbstractHaskell.C_HTypeDeclOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Read Curry.Module.AbstractHaskell.C_TypeClass where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractHaskell.C_TypeClass(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractHaskell")("TypeClass")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r)





instance Read Curry.Module.AbstractHaskell.C_InstanceDecl where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractHaskell.C_Instance(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractHaskell")("Instance")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r)





instance Read Curry.Module.AbstractHaskell.C_HFuncDecl where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractHaskell.C_HFunc(x1)(x2)(x3)(x4)(x5)(x6))(r6) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractHaskell")("HFunc")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3), ((,) x5 r5) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r4), ((,) x6 r6) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r5)])(r)





instance Read Curry.Module.AbstractHaskell.C_HaskellProg where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractHaskell.C_HaskellProg(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8))(r8) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractHaskell")("HaskellProg")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3), ((,) x5 r5) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r4), ((,) x6 r6) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r5), ((,) x7 r7) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r6), ((,) x8 r8) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r7)])(r)





instance Read Curry.Module.AbstractHaskell.C_HTypeDecl where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.AbstractHaskell.C_HTypeDecl(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("AbstractHaskell")("HTypeDecl")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r)






