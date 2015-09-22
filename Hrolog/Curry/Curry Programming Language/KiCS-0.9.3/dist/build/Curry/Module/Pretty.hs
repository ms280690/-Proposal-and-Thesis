{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Pretty (module Curry.Module.Pretty) where

import Curry.RunTimeSystem
import Curry.Module.Dequeue
import Curry.Module.Prelude



-- begin included



-- end included

type C_Layout = Curry.Module.Prelude.List Curry.Module.Prelude.C_Char

type C_Horizontal = Curry.Module.Prelude.C_Bool

type C_Remaining = Curry.Module.Prelude.C_Int

type C_Width = Curry.Module.Prelude.C_Int

type C_Position = Curry.Module.Prelude.C_Int

type C_StartPosition = Curry.Module.Prelude.C_Int

type C_EndPosition = Curry.Module.Prelude.C_Int

type C_Out = Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char

type C_OutGroupPrefix = Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char

type C_Margins = Curry.Module.Prelude.List Curry.Module.Prelude.C_Int

data C_Doc = C_Doc (Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Tokens -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Tokens))
  | C_DocFail Curry.RunTimeSystem.C_Exceptions
  | C_DocOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.Pretty.C_Doc)

data C_Tokens = C_Text (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Pretty.C_Tokens
  | C_Line (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Pretty.C_Tokens
  | C_Open Curry.Module.Pretty.C_Tokens
  | C_Close Curry.Module.Pretty.C_Tokens
  | C_Empty
  | C_OpenNest (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)))) Curry.Module.Pretty.C_Tokens
  | C_CloseNest Curry.Module.Pretty.C_Tokens
  | C_TokensFail Curry.RunTimeSystem.C_Exceptions
  | C_TokensOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.Pretty.C_Tokens)

instance BaseCurry Curry.Module.Pretty.C_Doc where
  nf f (Curry.Module.Pretty.C_Doc x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Pretty.C_Doc(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Pretty.C_Doc x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Pretty.C_Doc(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Pretty.C_DocOr(Curry.RunTimeSystem.mkRef(r)(1)(i))([Curry.Module.Pretty.C_Doc(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(1)

  failed  = Curry.Module.Pretty.C_DocFail

  branching  = Curry.Module.Pretty.C_DocOr

  consKind (Curry.Module.Pretty.C_DocOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Pretty.C_DocFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Pretty.C_DocFail x) = x

  orRef (Curry.Module.Pretty.C_DocOr x _) = x

  branches (Curry.Module.Pretty.C_DocOr _ x) = x





instance BaseCurry Curry.Module.Pretty.C_Tokens where
  nf f (Curry.Module.Pretty.C_Text x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.Pretty.C_Text(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.Pretty.C_Line x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.Pretty.C_Line(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.Pretty.C_Open x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Pretty.C_Open(v1))(state1))(x1)(state0)
  nf f (Curry.Module.Pretty.C_Close x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Pretty.C_Close(v1))(state1))(x1)(state0)
  nf f (Curry.Module.Pretty.C_OpenNest x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.Pretty.C_OpenNest(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.Pretty.C_CloseNest x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Pretty.C_CloseNest(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Pretty.C_Text x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.Pretty.C_Text(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.Pretty.C_Line x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.Pretty.C_Line(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.Pretty.C_Open x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Pretty.C_Open(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.Pretty.C_Close x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Pretty.C_Close(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.Pretty.C_OpenNest x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.Pretty.C_OpenNest(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.Pretty.C_CloseNest x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Pretty.C_CloseNest(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Pretty.C_TokensOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.Pretty.C_Text(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.Pretty.C_Line(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.Pretty.C_Open(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.Pretty.C_Close(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.Pretty.C_Empty,Curry.Module.Pretty.C_OpenNest(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.Pretty.C_CloseNest(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.Pretty.C_TokensFail

  branching  = Curry.Module.Pretty.C_TokensOr

  consKind (Curry.Module.Pretty.C_TokensOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Pretty.C_TokensFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Pretty.C_TokensFail x) = x

  orRef (Curry.Module.Pretty.C_TokensOr x _) = x

  branches (Curry.Module.Pretty.C_TokensOr _ x) = x





instance Curry Curry.Module.Pretty.C_Doc where
  strEq (Curry.Module.Pretty.C_Doc x1) (Curry.Module.Pretty.C_Doc y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Pretty.C_Doc x1) (Curry.Module.Pretty.C_Doc y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Pretty.C_Doc x1) st = Curry.Module.Pretty.C_Doc(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.Pretty.C_Doc x1) st = f(x1)(c)(st)

  typeName _ = "Doc"

  showQ d (Curry.Module.Pretty.C_Doc x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Pretty.Doc "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.Pretty.C_DocOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.Pretty.C_Tokens where
  strEq (Curry.Module.Pretty.C_Text x1 x2) (Curry.Module.Pretty.C_Text y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.Pretty.C_Line x1 x2) (Curry.Module.Pretty.C_Line y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.Pretty.C_Open x1) (Curry.Module.Pretty.C_Open y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.Pretty.C_Close x1) (Curry.Module.Pretty.C_Close y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq Curry.Module.Pretty.C_Empty Curry.Module.Pretty.C_Empty st = Curry.Module.Prelude.strEqSuccess
  strEq (Curry.Module.Pretty.C_OpenNest x1 x2) (Curry.Module.Pretty.C_OpenNest y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.Pretty.C_CloseNest x1) (Curry.Module.Pretty.C_CloseNest y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Pretty.C_Text x1 x2) (Curry.Module.Pretty.C_Text y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.Pretty.C_Line x1 x2) (Curry.Module.Pretty.C_Line y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.Pretty.C_Open x1) (Curry.Module.Pretty.C_Open y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.Pretty.C_Close x1) (Curry.Module.Pretty.C_Close y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq Curry.Module.Pretty.C_Empty Curry.Module.Pretty.C_Empty st = Curry.Module.Prelude.C_True
  eq (Curry.Module.Pretty.C_OpenNest x1 x2) (Curry.Module.Pretty.C_OpenNest y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.Pretty.C_CloseNest x1) (Curry.Module.Pretty.C_CloseNest y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Pretty.C_Text x1 x2) st = Curry.Module.Pretty.C_Text(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.Pretty.C_Line x1 x2) st = Curry.Module.Pretty.C_Line(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.Pretty.C_Open x1) st = Curry.Module.Pretty.C_Open(f((0::Int))(x1)(st))
  propagate f (Curry.Module.Pretty.C_Close x1) st = Curry.Module.Pretty.C_Close(f((0::Int))(x1)(st))
  propagate f Curry.Module.Pretty.C_Empty st = Curry.Module.Pretty.C_Empty
  propagate f (Curry.Module.Pretty.C_OpenNest x1 x2) st = Curry.Module.Pretty.C_OpenNest(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.Pretty.C_CloseNest x1) st = Curry.Module.Pretty.C_CloseNest(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.Pretty.C_Text x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.Pretty.C_Line x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.Pretty.C_Open x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.Pretty.C_Close x1) st = f(x1)(c)(st)
  foldCurry f c Curry.Module.Pretty.C_Empty st = c
  foldCurry f c (Curry.Module.Pretty.C_OpenNest x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.Pretty.C_CloseNest x1) st = f(x1)(c)(st)

  typeName _ = "Tokens"

  showQ d (Curry.Module.Pretty.C_Text x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Pretty.Text "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.Pretty.C_Line x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Pretty.Line "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.Pretty.C_Open x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Pretty.Open "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.Pretty.C_Close x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Pretty.Close "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ Curry.Module.Pretty.C_Empty = Prelude.showString("Pretty.Empty")
  showQ d (Curry.Module.Pretty.C_OpenNest x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Pretty.OpenNest "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.Pretty.C_CloseNest x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Pretty.CloseNest "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.Pretty.C_TokensOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.Pretty.C_Doc where
  showsPrec d (Curry.Module.Pretty.C_Doc x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Doc "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.Pretty.C_DocOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.Pretty.C_Tokens where
  showsPrec d (Curry.Module.Pretty.C_Text x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Text "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.Pretty.C_Line x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Line "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.Pretty.C_Open x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Open "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.Pretty.C_Close x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Close "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ Curry.Module.Pretty.C_Empty = Prelude.showString("Empty")
  showsPrec d (Curry.Module.Pretty.C_OpenNest x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("OpenNest "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.Pretty.C_CloseNest x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CloseNest "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.Pretty.C_TokensOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Read Curry.Module.Pretty.C_Doc where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Pretty.C_Doc(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Pretty")("Doc")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r)





instance Read Curry.Module.Pretty.C_Tokens where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Pretty.C_Text(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Pretty")("Text")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Pretty.C_Line(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Pretty")("Line")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Pretty.C_Open(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Pretty")("Open")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Pretty.C_Close(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Pretty")("Close")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Pretty.C_Empty)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Pretty")("Empty")(r)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Pretty.C_OpenNest(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Pretty")("OpenNest")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Pretty.C_CloseNest(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Pretty")("CloseNest")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r)))))))





c_deDoc :: Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Tokens -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Tokens)
c_deDoc x1@(Curry.Module.Pretty.C_Doc x2) st = x2
c_deDoc (Curry.Module.Pretty.C_DocOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_deDoc(x)(st))(i)(xs)(st)
c_deDoc x st = Curry.RunTimeSystem.patternFail("Pretty.deDoc")(x)



c_empty :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_empty st = Curry.Module.Pretty.c_text(Curry.Module.Prelude.List)(st)



c_text :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_text x1 st = Curry.Module.Pretty.C_Doc(Curry.Module.Prelude.pc(Curry.Module.Pretty.C_Text(x1)))



c_linesep :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_linesep st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc(Curry.Module.Pretty.C_Doc))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pc))(Curry.Module.Pretty.C_Line))(st)



c_line :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_line st = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_linesep(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(st)



c_linebreak :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_linebreak st = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_linesep(st))(Curry.Module.Prelude.List)(st)



c_softline :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_softline st = Curry.Module.Pretty.c_group(Curry.Module.Pretty.c_line(st))(st)



c_softbreak :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_softbreak st = Curry.Module.Pretty.c_group(Curry.Module.Pretty.c_linebreak(st))(st)



c_group :: Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_group x1 st = Curry.Module.Pretty.C_Doc(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc(Curry.Module.Pretty.C_Open))(Curry.Module.Prelude.op_46(Curry.Module.Pretty.c_deDoc(x1)(st))(Curry.Module.Prelude.pc(Curry.Module.Pretty.C_Close))(st))(st))



c_nest :: Curry.Module.Prelude.C_Int -> Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_nest x1 x2 st = Curry.Module.Pretty.C_Doc(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc(Curry.Module.Pretty.C_OpenNest(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Pretty.c_nest'46_'35lambda2(x1)))))(Curry.Module.Prelude.op_46(Curry.Module.Pretty.c_deDoc(x2)(st))(Curry.Module.Prelude.pc(Curry.Module.Pretty.C_CloseNest))(st))(st))



c_nest'46_'35lambda2 :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_nest'46_'35lambda2 x1 x2@((Curry.Module.Prelude.:<) x5 x6) x3 x4 st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.op_43(x5)(x1)(st))(x2)
c_nest'46_'35lambda2 x1 (Curry.Module.Prelude.ListOr i xs) x3 x4 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_nest'46_'35lambda2(x1)(x)(x3)(x4)(st))(i)(xs)(st)
c_nest'46_'35lambda2 x1 x x3 x4 st = Curry.RunTimeSystem.patternFail("Pretty.nest._#lambda2")(x)



c_hang :: Curry.Module.Prelude.C_Int -> Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_hang x1 x2 st = Curry.Module.Pretty.C_Doc(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc(Curry.Module.Pretty.C_OpenNest(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Pretty.c_hang'46_'35lambda3(x1)))))(Curry.Module.Prelude.op_46(Curry.Module.Pretty.c_deDoc(x2)(st))(Curry.Module.Prelude.pc(Curry.Module.Pretty.C_CloseNest))(st))(st))



c_hang'46_'35lambda3 :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_hang'46_'35lambda3 x1 x2 x3 x4 st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.op_43(Curry.Module.Prelude.op_45(x4)(x3)(st))(x1)(st))(x2)



c_align :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_align st = Curry.Module.Prelude.pf(Curry.Module.Pretty.c_hang(Curry.Module.Prelude.C_Zero))



c_combine :: Curry.Module.Pretty.C_Doc -> Curry.Module.Pretty.C_Doc -> Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_combine x1 x2 x3 st = Curry.Module.Pretty.c_enclose(x2)(x3)(x1)(st)



op_60_62 :: Curry.Module.Pretty.C_Doc -> Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
op_60_62 x1 x2 st = Curry.Module.Pretty.C_Doc(Curry.Module.Prelude.op_46(Curry.Module.Pretty.c_deDoc(x1)(st))(Curry.Module.Pretty.c_deDoc(x2)(st))(st))



op_60_43_62 :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc))
op_60_43_62 st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_combine(Curry.Module.Pretty.c_space(st)))



op_60_36_62 :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc))
op_60_36_62 st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_combine(Curry.Module.Pretty.c_line(st)))



op_60_47_62 :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc))
op_60_47_62 st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_combine(Curry.Module.Pretty.c_softline(st)))



op_60_36_36_62 :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc))
op_60_36_36_62 st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_combine(Curry.Module.Pretty.c_linebreak(st)))



op_60_47_47_62 :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc))
op_60_47_47_62 st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_combine(Curry.Module.Pretty.c_softbreak(st)))



c_compose :: (Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc))) -> (Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_compose x1 x2@Curry.Module.Prelude.List st = Curry.Module.Pretty.c_empty(st)
c_compose x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.Prelude.c_foldr1(x1)(x2)(st)
c_compose x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_compose(x1)(x)(st))(i)(xs)(st)
c_compose x1 x st = Curry.RunTimeSystem.patternFail("Pretty.compose")(x)



c_hsep :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_hsep st = Curry.Module.Prelude.pf(Curry.Module.Pretty.c_compose(Curry.Module.Pretty.op_60_43_62(st)))



c_vsep :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_vsep st = Curry.Module.Prelude.pf(Curry.Module.Pretty.c_compose(Curry.Module.Pretty.op_60_36_62(st)))



c_fillSep :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_fillSep st = Curry.Module.Prelude.pf(Curry.Module.Pretty.c_compose(Curry.Module.Pretty.op_60_47_62(st)))



c_sep :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_sep st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_group))(Curry.Module.Pretty.c_vsep(st))(st)



c_hcat :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_hcat st = Curry.Module.Prelude.pf(Curry.Module.Pretty.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.op_60_62)))



c_vcat :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_vcat st = Curry.Module.Prelude.pf(Curry.Module.Pretty.c_compose(Curry.Module.Pretty.op_60_36_36_62(st)))



c_fillCat :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_fillCat st = Curry.Module.Prelude.pf(Curry.Module.Pretty.c_compose(Curry.Module.Pretty.op_60_47_47_62(st)))



c_cat :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_cat st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_group))(Curry.Module.Pretty.c_vcat(st))(st)



c_punctuate :: Curry.Module.Pretty.C_Doc -> (Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc
c_punctuate x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_punctuate x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.Pretty.c_punctuate'46go'4675(x1)(x2)(st)
c_punctuate x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_punctuate(x1)(x)(st))(i)(xs)(st)
c_punctuate x1 x st = Curry.RunTimeSystem.patternFail("Pretty.punctuate")(x)



c_punctuate'46go'4675 :: Curry.Module.Pretty.C_Doc -> (Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc
c_punctuate'46go'4675 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.Pretty.c_punctuate'46go'4675_case_13(x1)(x3)(x4)(st)
c_punctuate'46go'4675 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_punctuate'46go'4675(x1)(x)(st))(i)(xs)(st)
c_punctuate'46go'4675 x1 x st = Curry.RunTimeSystem.patternFail("Pretty.punctuate.go.75")(x)



c_encloseSep :: Curry.Module.Pretty.C_Doc -> Curry.Module.Pretty.C_Doc -> Curry.Module.Pretty.C_Doc -> (Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_encloseSep x1 x2 x3 x4@Curry.Module.Prelude.List st = Curry.Module.Pretty.op_60_62(x1)(x2)(st)
c_encloseSep x1 x2 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_align(st))(Curry.Module.Pretty.c_enclose(x1)(x2)(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_cat(st))((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Pretty.op_60_62(x3)))(x6)(st)))(st))(st))(st)
c_encloseSep x1 x2 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_encloseSep(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_encloseSep x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("Pretty.encloseSep")(x)



c_hEncloseSep :: Curry.Module.Pretty.C_Doc -> Curry.Module.Pretty.C_Doc -> Curry.Module.Pretty.C_Doc -> (Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_hEncloseSep x1 x2 x3 x4@Curry.Module.Prelude.List st = Curry.Module.Pretty.op_60_62(x1)(x2)(st)
c_hEncloseSep x1 x2 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_align(st))(Curry.Module.Pretty.c_enclose(x1)(x2)(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_hcat(st))((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Pretty.op_60_62(x3)))(x6)(st)))(st))(st))(st)
c_hEncloseSep x1 x2 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_hEncloseSep(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_hEncloseSep x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("Pretty.hEncloseSep")(x)



c_fillEncloseSep :: Curry.Module.Pretty.C_Doc -> Curry.Module.Pretty.C_Doc -> Curry.Module.Pretty.C_Doc -> (Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_fillEncloseSep x1 x2 x3 x4@Curry.Module.Prelude.List st = Curry.Module.Pretty.op_60_62(x1)(x2)(st)
c_fillEncloseSep x1 x2 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_align(st))(Curry.Module.Pretty.c_enclose(x1)(x2)(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_hcat(st))((Curry.Module.Prelude.:<)(x5)(Curry.Module.Pretty.c_fillEncloseSep'46withSoftBreaks'4696(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Pretty.op_60_62(x3)))(x6)(st))(st)))(st))(st))(st)
c_fillEncloseSep x1 x2 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_fillEncloseSep(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_fillEncloseSep x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("Pretty.fillEncloseSep")(x)



c_fillEncloseSep'46withSoftBreaks'4696 :: (Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc
c_fillEncloseSep'46withSoftBreaks'4696 x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_fillEncloseSep'46withSoftBreaks'4696 x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.Pretty.c_fillEncloseSep'46withSoftBreaks'4696_case_12(x2)(x3)(st)
c_fillEncloseSep'46withSoftBreaks'4696 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_fillEncloseSep'46withSoftBreaks'4696(x)(st))(i)(xs)(st)
c_fillEncloseSep'46withSoftBreaks'4696 x st = Curry.RunTimeSystem.patternFail("Pretty.fillEncloseSep.withSoftBreaks.96")(x)



c_list :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_list st = Curry.Module.Prelude.pf(Curry.Module.Pretty.c_fillEncloseSep(Curry.Module.Pretty.c_lbracket(st))(Curry.Module.Pretty.c_rbracket(st))(Curry.Module.Pretty.c_comma(st)))



c_tupled :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_tupled st = Curry.Module.Prelude.pf(Curry.Module.Pretty.c_fillEncloseSep(Curry.Module.Pretty.c_lparen(st))(Curry.Module.Pretty.c_rparen(st))(Curry.Module.Pretty.c_comma(st)))



c_semiBraces :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_semiBraces st = Curry.Module.Prelude.pf(Curry.Module.Pretty.c_fillEncloseSep(Curry.Module.Pretty.c_lbrace(st))(Curry.Module.Pretty.c_rbrace(st))(Curry.Module.Pretty.c_semi(st)))



c_enclose :: Curry.Module.Pretty.C_Doc -> Curry.Module.Pretty.C_Doc -> Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_enclose x1 x2 x3 st = Curry.Module.Pretty.op_60_62(Curry.Module.Pretty.op_60_62(x1)(x3)(st))(x2)(st)



c_squotes :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_squotes st = Curry.Module.Prelude.pf(Curry.Module.Pretty.c_enclose(Curry.Module.Pretty.c_squote(st))(Curry.Module.Pretty.c_squote(st)))



c_dquotes :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_dquotes st = Curry.Module.Prelude.pf(Curry.Module.Pretty.c_enclose(Curry.Module.Pretty.c_dquote(st))(Curry.Module.Pretty.c_dquote(st)))



c_bquotes :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_bquotes st = Curry.Module.Prelude.pf(Curry.Module.Pretty.c_enclose(Curry.Module.Pretty.c_bquote(st))(Curry.Module.Pretty.c_bquote(st)))



c_parens :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_parens st = Curry.Module.Prelude.pf(Curry.Module.Pretty.c_enclose(Curry.Module.Pretty.c_lparen(st))(Curry.Module.Pretty.c_rparen(st)))



c_angles :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_angles st = Curry.Module.Prelude.pf(Curry.Module.Pretty.c_enclose(Curry.Module.Pretty.c_langle(st))(Curry.Module.Pretty.c_rangle(st)))



c_braces :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_braces st = Curry.Module.Prelude.pf(Curry.Module.Pretty.c_enclose(Curry.Module.Pretty.c_lbrace(st))(Curry.Module.Pretty.c_rbrace(st)))



c_brackets :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_brackets st = Curry.Module.Prelude.pf(Curry.Module.Pretty.c_enclose(Curry.Module.Pretty.c_lbracket(st))(Curry.Module.Pretty.c_rbracket(st)))



c_char :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_char x1 st = Curry.Module.Pretty.c_text((Curry.Module.Prelude.:<)(x1)(Curry.Module.Prelude.List))(st)



c_string :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_string st = Curry.Module.Prelude.op_46(Curry.Module.Pretty.c_hcat(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_string'46_'35lambda4))))(st)



c_string'46_'35lambda4 :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_string'46_'35lambda4 x1 st = Curry.Module.Pretty.c_string'46_'35lambda4_case_11(x1)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\r'))(Curry.Module.Prelude.List)))(st))(st)



c_int :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_int x1 st = Curry.Module.Pretty.c_text(Curry.Module.Prelude.c_show(x1)(st))(st)



c_float :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_float x1 st = Curry.Module.Pretty.c_text(Curry.Module.Prelude.c_show(x1)(st))(st)



c_lparen :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_lparen st = Curry.Module.Pretty.c_char(Curry.Module.Prelude.C_Char('('))(st)



c_rparen :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_rparen st = Curry.Module.Pretty.c_char(Curry.Module.Prelude.C_Char(')'))(st)



c_langle :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_langle st = Curry.Module.Pretty.c_char(Curry.Module.Prelude.C_Char('<'))(st)



c_rangle :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_rangle st = Curry.Module.Pretty.c_char(Curry.Module.Prelude.C_Char('>'))(st)



c_lbrace :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_lbrace st = Curry.Module.Pretty.c_char(Curry.Module.Prelude.C_Char('{'))(st)



c_rbrace :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_rbrace st = Curry.Module.Pretty.c_char(Curry.Module.Prelude.C_Char('}'))(st)



c_lbracket :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_lbracket st = Curry.Module.Pretty.c_char(Curry.Module.Prelude.C_Char('['))(st)



c_rbracket :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_rbracket st = Curry.Module.Pretty.c_char(Curry.Module.Prelude.C_Char(']'))(st)



c_squote :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_squote st = Curry.Module.Pretty.c_char(Curry.Module.Prelude.C_Char('\''))(st)



c_dquote :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_dquote st = Curry.Module.Pretty.c_char(Curry.Module.Prelude.C_Char('\"'))(st)



c_bquote :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_bquote st = Curry.Module.Pretty.c_char(Curry.Module.Prelude.C_Char('`'))(st)



c_semi :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_semi st = Curry.Module.Pretty.c_char(Curry.Module.Prelude.C_Char(';'))(st)



c_colon :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_colon st = Curry.Module.Pretty.c_char(Curry.Module.Prelude.C_Char(':'))(st)



c_comma :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_comma st = Curry.Module.Pretty.c_char(Curry.Module.Prelude.C_Char(','))(st)



c_space :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_space st = Curry.Module.Pretty.c_char(Curry.Module.Prelude.C_Char(' '))(st)



c_dot :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_dot st = Curry.Module.Pretty.c_char(Curry.Module.Prelude.C_Char('.'))(st)



c_backslash :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_backslash st = Curry.Module.Pretty.c_char(Curry.Module.Prelude.C_Char('\\'))(st)



c_equals :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_equals st = Curry.Module.Pretty.c_char(Curry.Module.Prelude.C_Char('='))(st)



c_normalise :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Tokens -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Tokens)
c_normalise st = Curry.Module.Prelude.pf(Curry.Module.Pretty.c_normalise'46go'46173(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id)))



c_normalise'46open'46173 :: Curry.Module.Pretty.C_Tokens -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Tokens
c_normalise'46open'46173 x1@(Curry.Module.Pretty.C_Close x2) st = x2
c_normalise'46open'46173 x1@(Curry.Module.Pretty.C_Text x3 x4) st = Curry.Module.Pretty.C_Open(x1)
c_normalise'46open'46173 x1@(Curry.Module.Pretty.C_Line x5 x6) st = Curry.Module.Pretty.C_Open(x1)
c_normalise'46open'46173 x1@(Curry.Module.Pretty.C_Open x7) st = Curry.Module.Pretty.C_Open(x1)
c_normalise'46open'46173 x1@Curry.Module.Pretty.C_Empty st = Curry.Module.Pretty.C_Open(x1)
c_normalise'46open'46173 x1@(Curry.Module.Pretty.C_OpenNest x8 x9) st = Curry.Module.Pretty.C_Open(x1)
c_normalise'46open'46173 x1@(Curry.Module.Pretty.C_CloseNest x10) st = Curry.Module.Pretty.C_Open(x1)
c_normalise'46open'46173 (Curry.Module.Pretty.C_TokensOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_normalise'46open'46173(x)(st))(i)(xs)(st)
c_normalise'46open'46173 x st = Curry.RunTimeSystem.patternFail("Pretty.normalise.open.173")(x)



c_normalise'46go'46173 :: (Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Tokens -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Tokens)) -> Curry.Module.Pretty.C_Tokens -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Tokens
c_normalise'46go'46173 x1 x2@Curry.Module.Pretty.C_Empty st = Curry.Module.Prelude.c_apply(x1)(Curry.Module.Pretty.C_Empty)(st)
c_normalise'46go'46173 x1 x2@(Curry.Module.Pretty.C_Open x3) st = Curry.Module.Pretty.c_normalise'46go'46173(Curry.Module.Prelude.op_46(x1)(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_normalise'46open'46173))(st))(x3)(st)
c_normalise'46go'46173 x1 x2@(Curry.Module.Pretty.C_Close x4) st = Curry.Module.Pretty.c_normalise'46go'46173(Curry.Module.Prelude.op_46(x1)(Curry.Module.Prelude.pc(Curry.Module.Pretty.C_Close))(st))(x4)(st)
c_normalise'46go'46173 x1 x2@(Curry.Module.Pretty.C_Line x5 x6) st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.op_46(x1)(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc(Curry.Module.Pretty.C_Line(x5)))(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_normalise'46go'46173(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id))))(st))(st))(x6)(st)
c_normalise'46go'46173 x1 x2@(Curry.Module.Pretty.C_Text x7 x8) st = Curry.Module.Pretty.C_Text(x7)(Curry.Module.Pretty.c_normalise'46go'46173(x1)(x8)(st))
c_normalise'46go'46173 x1 x2@(Curry.Module.Pretty.C_OpenNest x9 x10) st = Curry.Module.Pretty.C_OpenNest(x9)(Curry.Module.Pretty.c_normalise'46go'46173(x1)(x10)(st))
c_normalise'46go'46173 x1 x2@(Curry.Module.Pretty.C_CloseNest x11) st = Curry.Module.Pretty.C_CloseNest(Curry.Module.Pretty.c_normalise'46go'46173(x1)(x11)(st))
c_normalise'46go'46173 x1 (Curry.Module.Pretty.C_TokensOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_normalise'46go'46173(x1)(x)(st))(i)(xs)(st)
c_normalise'46go'46173 x1 x st = Curry.RunTimeSystem.patternFail("Pretty.normalise.go.173")(x)



c_doc2Tokens :: Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Tokens
c_doc2Tokens x1@(Curry.Module.Pretty.C_Doc x2) st = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_normalise(st))(Curry.Module.Prelude.c_apply(x2)(Curry.Module.Pretty.C_Empty)(st))(st)
c_doc2Tokens (Curry.Module.Pretty.C_DocOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_doc2Tokens(x)(st))(i)(xs)(st)
c_doc2Tokens x st = Curry.RunTimeSystem.patternFail("Pretty.doc2Tokens")(x)



c_pretty :: Curry.Module.Prelude.C_Int -> Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_pretty x1 x2 st = Curry.Module.Pretty.c_noGroup(Curry.Module.Pretty.c_doc2Tokens(x2)(st))(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.List))(st)



c_length :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int)
c_length st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_length))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_not))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_elem))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.c_enumFromTo(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(st))(st))))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_ord))(st))(st))))(st)



c_noGroup :: Curry.Module.Pretty.C_Tokens -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_noGroup x1@Curry.Module.Pretty.C_Empty x2 x3 x4 x5 st = Curry.Module.Prelude.List
c_noGroup x1@(Curry.Module.Pretty.C_Text x6 x7) x2 x3 x4 x5 st = let {x8 = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_length(st))(x6)(st)} in Curry.Module.Prelude.op_43_43(x6)(Curry.Module.Pretty.c_noGroup(x7)(x2)(Curry.Module.Prelude.op_43(x3)(x8)(st))(Curry.Module.Prelude.op_45(x4)(x8)(st))(x5)(st))(st)
c_noGroup x1@(Curry.Module.Pretty.C_Line x9 x10) x2 x3 x4 x5 st = Curry.Module.Pretty.c_noGroup_case_10(x2)(x3)(x10)(x5)(st)
c_noGroup x1@(Curry.Module.Pretty.C_Open x13) x2 x3 x4 x5 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_oneGroup(x13)(x2)(x3)(Curry.Module.Prelude.op_43(x3)(x4)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_noGroup'46_'35lambda6))(st))(x4)(st))(x5)(st)
c_noGroup x1@(Curry.Module.Pretty.C_Close x14) x2 x3 x4 x5 st = Curry.Module.Pretty.c_noGroup(x14)(x2)(x3)(x4)(x5)(st)
c_noGroup x1@(Curry.Module.Pretty.C_OpenNest x15 x16) x2 x3 x4 x5 st = Curry.Module.Pretty.c_noGroup(x16)(x2)(x3)(x4)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x15)(x5)(st))(x4)(st))(x2)(st))(st)
c_noGroup x1@(Curry.Module.Pretty.C_CloseNest x17) x2 x3 x4 x5 st = Curry.Module.Pretty.c_noGroup(x17)(x2)(x3)(x4)(Curry.Module.Prelude.c_tail(x5)(st))(st)
c_noGroup (Curry.Module.Pretty.C_TokensOr i xs) x2 x3 x4 x5 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_noGroup(x)(x2)(x3)(x4)(x5)(st))(i)(xs)(st)
c_noGroup x x2 x3 x4 x5 st = Curry.RunTimeSystem.patternFail("Pretty.noGroup")(x)



c_noGroup'46_'35lambda6 :: Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_noGroup'46_'35lambda6 x1 x2 st = x2



c_oneGroup :: Curry.Module.Pretty.C_Tokens -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_oneGroup x1@(Curry.Module.Pretty.C_Text x6 x7) x2 x3 x4 x5 st = let {x8 = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_length(st))(x6)(st)} in Curry.Module.Pretty.c_pruneOne(x7)(x2)(Curry.Module.Prelude.op_43(x3)(x8)(st))(x4)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_oneGroup'46_'35lambda7(x8)(x5)(x6)))(st)
c_oneGroup x1@(Curry.Module.Pretty.C_Line x9 x10) x2 x3 x4 x5 st = let {x11 = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_length(st))(x9)(st)} in Curry.Module.Pretty.c_pruneOne(x10)(x2)(Curry.Module.Prelude.op_43(x3)(x11)(st))(x4)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_oneGroup'46_'35lambda8(x11)(x5)(x9)(x2)))(st)
c_oneGroup x1@(Curry.Module.Pretty.C_Open x12) x2 x3 x4 x5 st = Curry.Module.Pretty.c_multiGroup(x12)(x2)(x3)(x4)(x5)(Curry.Module.Dequeue.c_empty(st))(x3)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_oneGroup'46_'35lambda9))(st)
c_oneGroup x1@(Curry.Module.Pretty.C_Close x13) x2 x3 x4 x5 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x5)(Curry.Module.Prelude.op_60_61(x3)(x4)(st))(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_noGroup(x13)(x2)(x3)))(st)
c_oneGroup x1@(Curry.Module.Pretty.C_OpenNest x14 x15) x2 x3 x4 x5 st = Curry.Module.Pretty.c_oneGroup(x15)(x2)(x3)(x4)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_oneGroup'46_'35lambda10(x14)(x5)(x2)))(st)
c_oneGroup x1@(Curry.Module.Pretty.C_CloseNest x16) x2 x3 x4 x5 st = Curry.Module.Pretty.c_oneGroup(x16)(x2)(x3)(x4)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_oneGroup'46_'35lambda12(x5)))(st)
c_oneGroup (Curry.Module.Pretty.C_TokensOr i xs) x2 x3 x4 x5 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_oneGroup(x)(x2)(x3)(x4)(x5)(st))(i)(xs)(st)
c_oneGroup x x2 x3 x4 x5 st = Curry.RunTimeSystem.patternFail("Pretty.oneGroup")(x)



c_oneGroup'46outText'46234 :: (Curry t0) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.Prelude.C_Int -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_oneGroup'46outText'46234 x1 x2 x3 x4 x5 st = Curry.Module.Prelude.op_43_43(x2)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x3)(Curry.Module.Prelude.op_45(x4)(x1)(st))(st))(x5)(st))(st)



c_oneGroup'46_'35lambda7 :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_oneGroup'46_'35lambda7 x1 x2 x3 x4 x5 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x2)(x4)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_oneGroup'46outText'46234(x1)(x3)(x5)))(st)



c_oneGroup'46outLine'46240 :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_oneGroup'46outLine'46240 x1 x2 x3 x4 x5 x6 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.Pretty.c_oneGroup'46outLine'46240_case_9(x1)(x2)(x3)(x5)(x6)(x7)(x8)(x4)(st)
c_oneGroup'46outLine'46240 x1 x2 x3 x4 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_oneGroup'46outLine'46240(x1)(x2)(x3)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_oneGroup'46outLine'46240 x1 x2 x3 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("Pretty.oneGroup.outLine.240")(x)



c_oneGroup'46_'35lambda8 :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_oneGroup'46_'35lambda8 x1 x2 x3 x4 x5 x6 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x2)(x5)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_oneGroup'46outLine'46240(x1)(x3)(x4)(x5)(x6)))(st)



c_oneGroup'46_'35lambda9 :: Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_oneGroup'46_'35lambda9 x1 x2 st = x2



c_oneGroup'46_'35lambda10 :: (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)))) -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_oneGroup'46_'35lambda10 x1 x2 x3 x4 x5 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x2)(x4)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_oneGroup'46_'35lambda10'46_'35lambda11(x5)(x1)(x3)))(st)



c_oneGroup'46_'35lambda10'46_'35lambda11 :: (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)))) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_oneGroup'46_'35lambda10'46_'35lambda11 x1 x2 x3 x4 x5 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x4)(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x2)(x5)(st))(x4)(st))(x3)(st))(st)



c_oneGroup'46_'35lambda12 :: (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_oneGroup'46_'35lambda12 x1 x2 x3 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x2)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_oneGroup'46_'35lambda12'46_'35lambda13(x3)))(st)



c_oneGroup'46_'35lambda12'46_'35lambda13 :: (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_oneGroup'46_'35lambda12'46_'35lambda13 x1 x2 x3 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x2)(st))(Curry.Module.Prelude.c_tail(x3)(st))(st)



c_multiGroup :: Curry.Module.Pretty.C_Tokens -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> (Curry.Module.Dequeue.C_Queue (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_multiGroup x1@(Curry.Module.Pretty.C_Text x9 x10) x2 x3 x4 x5 x6 x7 x8 st = let {x11 = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_length(st))(x9)(st)} in Curry.Module.Pretty.c_pruneMulti(x10)(x2)(Curry.Module.Prelude.op_43(x3)(x11)(st))(x4)(x5)(x6)(x7)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_multiGroup'46_'35lambda14(x11)(x8)(x9)))(st)
c_multiGroup x1@(Curry.Module.Pretty.C_Line x12 x13) x2 x3 x4 x5 x6 x7 x8 st = let {x14 = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_length(st))(x12)(st)} in Curry.Module.Pretty.c_pruneMulti(x13)(x2)(Curry.Module.Prelude.op_43(x3)(x14)(st))(x4)(x5)(x6)(x7)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_multiGroup'46_'35lambda15(x14)(x8)(x12)(x2)))(st)
c_multiGroup x1@(Curry.Module.Pretty.C_Open x15) x2 x3 x4 x5 x6 x7 x8 st = Curry.Module.Pretty.c_multiGroup(x15)(x2)(x3)(x4)(x5)(Curry.Module.Dequeue.c_cons(Curry.Module.Prelude.T2(x7)(x8))(x6)(st))(x3)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_multiGroup'46_'35lambda16))(st)
c_multiGroup x1@(Curry.Module.Pretty.C_Close x16) x2 x3 x4 x5 x6 x7 x8 st = Curry.Module.Pretty.c_multiGroup_case_8(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x16)(Curry.Module.Dequeue.c_matchHead(x6)(st))(st)
c_multiGroup x1@(Curry.Module.Pretty.C_OpenNest x22 x23) x2 x3 x4 x5 x6 x7 x8 st = Curry.Module.Pretty.c_multiGroup(x23)(x2)(x3)(x4)(x5)(x6)(x7)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_multiGroup'46_'35lambda22(x22)(x8)(x2)))(st)
c_multiGroup x1@(Curry.Module.Pretty.C_CloseNest x24) x2 x3 x4 x5 x6 x7 x8 st = Curry.Module.Pretty.c_multiGroup(x24)(x2)(x3)(x4)(x5)(x6)(x7)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_multiGroup'46_'35lambda24(x8)))(st)
c_multiGroup (Curry.Module.Pretty.C_TokensOr i xs) x2 x3 x4 x5 x6 x7 x8 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_multiGroup(x)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(st))(i)(xs)(st)
c_multiGroup x x2 x3 x4 x5 x6 x7 x8 st = Curry.RunTimeSystem.patternFail("Pretty.multiGroup")(x)



c_multiGroup'46outText'46261 :: (Curry t0) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.Prelude.C_Int -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_multiGroup'46outText'46261 x1 x2 x3 x4 x5 st = Curry.Module.Prelude.op_43_43(x2)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x3)(Curry.Module.Prelude.op_45(x4)(x1)(st))(st))(x5)(st))(st)



c_multiGroup'46_'35lambda14 :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_multiGroup'46_'35lambda14 x1 x2 x3 x4 x5 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x2)(x4)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_multiGroup'46outText'46261(x1)(x3)(x5)))(st)



c_multiGroup'46outLine'46267 :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_multiGroup'46outLine'46267 x1 x2 x3 x4 x5 x6 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.Pretty.c_multiGroup'46outLine'46267_case_5(x1)(x2)(x3)(x5)(x6)(x7)(x8)(x4)(st)
c_multiGroup'46outLine'46267 x1 x2 x3 x4 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_multiGroup'46outLine'46267(x1)(x2)(x3)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_multiGroup'46outLine'46267 x1 x2 x3 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("Pretty.multiGroup.outLine.267")(x)



c_multiGroup'46_'35lambda15 :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_multiGroup'46_'35lambda15 x1 x2 x3 x4 x5 x6 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x2)(x5)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_multiGroup'46outLine'46267(x1)(x3)(x4)(x5)(x6)))(st)



c_multiGroup'46_'35lambda16 :: Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_multiGroup'46_'35lambda16 x1 x2 st = x2



c_multiGroup'46_'35lambda18 :: (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_multiGroup'46_'35lambda18 x1 x2 x3 x4 x5 x6 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x2)(x5)(st))(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_multiGroup'46_'35lambda18'46_'35lambda19(x6)(x1)(x3)(x4)))(st)



c_multiGroup'46_'35lambda18'46_'35lambda19 :: (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_multiGroup'46_'35lambda18'46_'35lambda19 x1 x2 x3 x4 x5 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x2)(Curry.Module.Prelude.op_60_61(x3)(Curry.Module.Prelude.op_43(x4)(x5)(st))(st))(st))(x1)(st))(x5)(st)



c_multiGroup'46_'35lambda20 :: (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_multiGroup'46_'35lambda20 x1 x2 x3 x4 x5 x6 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x5)(st))(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_multiGroup'46_'35lambda20'46_'35lambda21(x6)(x2)(x3)(x4)))(st)



c_multiGroup'46_'35lambda20'46_'35lambda21 :: (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_multiGroup'46_'35lambda20'46_'35lambda21 x1 x2 x3 x4 x5 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x2)(Curry.Module.Prelude.op_60_61(x3)(Curry.Module.Prelude.op_43(x4)(x5)(st))(st))(st))(x1)(st))(x5)(st)



c_multiGroup'46_'35lambda22 :: (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)))) -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_multiGroup'46_'35lambda22 x1 x2 x3 x4 x5 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x2)(x4)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_multiGroup'46_'35lambda22'46_'35lambda23(x5)(x1)(x3)))(st)



c_multiGroup'46_'35lambda22'46_'35lambda23 :: (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)))) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_multiGroup'46_'35lambda22'46_'35lambda23 x1 x2 x3 x4 x5 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x4)(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x2)(x5)(st))(x4)(st))(x3)(st))(st)



c_multiGroup'46_'35lambda24 :: (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_multiGroup'46_'35lambda24 x1 x2 x3 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x2)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_multiGroup'46_'35lambda24'46_'35lambda25(x3)))(st)



c_multiGroup'46_'35lambda24'46_'35lambda25 :: (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_multiGroup'46_'35lambda24'46_'35lambda25 x1 x2 x3 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x2)(st))(Curry.Module.Prelude.c_tail(x3)(st))(st)



c_pruneOne :: Curry.Module.Pretty.C_Tokens -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_pruneOne x1 x2 x3 x4 x5 st = Curry.Module.Pretty.c_pruneOne_case_4(x1)(x2)(x3)(x4)(x5)(Curry.Module.Prelude.op_60_61(x3)(x4)(st))(st)



c_pruneMulti :: Curry.Module.Pretty.C_Tokens -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> (Curry.Module.Dequeue.C_Queue (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_pruneMulti x1 x2 x3 x4 x5 x6 x7 x8 st = Curry.Module.Pretty.c_pruneMulti_case_3(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(Curry.Module.Prelude.op_60_61(x3)(x4)(st))(st)



c_pruneMulti'46_'35lambda26 :: (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Dequeue.C_Queue (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))) -> Curry.Module.Prelude.C_Int -> Curry.Module.Pretty.C_Tokens -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_pruneMulti'46_'35lambda26 x1 x2 x3 x4 x5 x6 x7 st = Curry.Module.Pretty.c_pruneMulti'46_'35lambda26_case_2(x1)(x2)(x3)(x4)(x5)(x6)(x7)(Curry.Module.Dequeue.c_matchLast(x3)(st))(st)



c_pruneMulti'46_'35lambda26_case_2 x1 x2 x3 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_Nothing st = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_pruneOne(x5)(x6)(x2)(Curry.Module.Prelude.op_43(x4)(x7)(st))(x1)(st))(x7)(st)
c_pruneMulti'46_'35lambda26_case_2 x1 x2 x3 x4 x5 x6 x7 (Curry.Module.Prelude.C_Just x8) st = Curry.Module.Pretty.c_pruneMulti'46_'35lambda26_case_1(x1)(x2)(x4)(x5)(x6)(x7)(x8)(st)
c_pruneMulti'46_'35lambda26_case_2 x1 x2 x3 x4 x5 x6 x7 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_pruneMulti'46_'35lambda26_case_2(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c_pruneMulti'46_'35lambda26_case_2 x1 x2 x3 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("Pretty.pruneMulti._#lambda26_case_2")(x)



c_pruneMulti'46_'35lambda26_case_1 x1 x2 x4 x5 x6 x7 x8@(Curry.Module.Prelude.T2 x9 x10) st = Curry.Module.Pretty.c_pruneMulti'46_'35lambda26_case_0(x1)(x2)(x4)(x5)(x6)(x7)(x10)(x9)(st)
c_pruneMulti'46_'35lambda26_case_1 x1 x2 x4 x5 x6 x7 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_pruneMulti'46_'35lambda26_case_1(x1)(x2)(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c_pruneMulti'46_'35lambda26_case_1 x1 x2 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("Pretty.pruneMulti._#lambda26_case_1")(x)



c_pruneMulti'46_'35lambda26_case_0 x1 x2 x4 x5 x6 x7 x10 x9@(Curry.Module.Prelude.T2 x11 x12) st = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_pruneMulti(x5)(x6)(x2)(Curry.Module.Prelude.op_43(x11)(x7)(st))(x12)(x10)(x4)(x1)(st))(x7)(st)
c_pruneMulti'46_'35lambda26_case_0 x1 x2 x4 x5 x6 x7 x10 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_pruneMulti'46_'35lambda26_case_0(x1)(x2)(x4)(x5)(x6)(x7)(x10)(x)(st))(i)(xs)(st)
c_pruneMulti'46_'35lambda26_case_0 x1 x2 x4 x5 x6 x7 x10 x st = Curry.RunTimeSystem.patternFail("Pretty.pruneMulti._#lambda26_case_0")(x)



c_pruneMulti_case_3 x1 x2 x3 x4 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.Pretty.c_multiGroup(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(st)
c_pruneMulti_case_3 x1 x2 x3 x4 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x5)(Curry.Module.Prelude.C_False)(st))(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_pruneMulti'46_'35lambda26(x8)(x3)(x6)(x7)(x1)(x2)))(st)
c_pruneMulti_case_3 x1 x2 x3 x4 x5 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_pruneMulti_case_3(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c_pruneMulti_case_3 x1 x2 x3 x4 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("Pretty.pruneMulti_case_3")(x)



c_pruneOne_case_4 x1 x2 x3 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.Pretty.c_oneGroup(x1)(x2)(x3)(x4)(x5)(st)
c_pruneOne_case_4 x1 x2 x3 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x5)(Curry.Module.Prelude.C_False)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_noGroup(x1)(x2)(x3)))(st)
c_pruneOne_case_4 x1 x2 x3 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_pruneOne_case_4(x1)(x2)(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c_pruneOne_case_4 x1 x2 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("Pretty.pruneOne_case_4")(x)



c_multiGroup'46outLine'46267_case_5 x1 x2 x3 x5 x6 x7 x8 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43(x2)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x5)(Curry.Module.Prelude.op_45(x6)(x1)(st))(st))(x7)(st))(st)
c_multiGroup'46outLine'46267_case_5 x1 x2 x3 x5 x6 x7 x8 x4@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_replicate(x8)(Curry.Module.Prelude.C_Char(' '))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x5)(Curry.Module.Prelude.op_45(x3)(x8)(st))(st))(x7)(st))(st))
c_multiGroup'46outLine'46267_case_5 x1 x2 x3 x5 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_multiGroup'46outLine'46267_case_5(x1)(x2)(x3)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c_multiGroup'46outLine'46267_case_5 x1 x2 x3 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("Pretty.multiGroup.outLine.267_case_5")(x)



c_multiGroup_case_8 x2 x3 x4 x5 x6 x7 x8 x16 x17@Curry.Module.Prelude.C_Nothing st = Curry.Module.Pretty.c_oneGroup(x16)(x2)(x3)(x4)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_multiGroup'46_'35lambda18(x8)(x5)(x3)(x7)))(st)
c_multiGroup_case_8 x2 x3 x4 x5 x6 x7 x8 x16 (Curry.Module.Prelude.C_Just x17) st = Curry.Module.Pretty.c_multiGroup_case_7(x2)(x3)(x4)(x5)(x7)(x8)(x16)(x17)(st)
c_multiGroup_case_8 x2 x3 x4 x5 x6 x7 x8 x16 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_multiGroup_case_8(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x16)(x)(st))(i)(xs)(st)
c_multiGroup_case_8 x2 x3 x4 x5 x6 x7 x8 x16 x st = Curry.RunTimeSystem.patternFail("Pretty.multiGroup_case_8")(x)



c_multiGroup_case_7 x2 x3 x4 x5 x7 x8 x16 x17@(Curry.Module.Prelude.T2 x18 x19) st = Curry.Module.Pretty.c_multiGroup_case_6(x2)(x3)(x4)(x5)(x7)(x8)(x16)(x19)(x18)(st)
c_multiGroup_case_7 x2 x3 x4 x5 x7 x8 x16 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_multiGroup_case_7(x2)(x3)(x4)(x5)(x7)(x8)(x16)(x)(st))(i)(xs)(st)
c_multiGroup_case_7 x2 x3 x4 x5 x7 x8 x16 x st = Curry.RunTimeSystem.patternFail("Pretty.multiGroup_case_7")(x)



c_multiGroup_case_6 x2 x3 x4 x5 x7 x8 x16 x19 x18@(Curry.Module.Prelude.T2 x20 x21) st = Curry.Module.Pretty.c_multiGroup(x16)(x2)(x3)(x4)(x5)(x19)(x20)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_multiGroup'46_'35lambda20(x21)(x8)(x3)(x7)))(st)
c_multiGroup_case_6 x2 x3 x4 x5 x7 x8 x16 x19 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_multiGroup_case_6(x2)(x3)(x4)(x5)(x7)(x8)(x16)(x19)(x)(st))(i)(xs)(st)
c_multiGroup_case_6 x2 x3 x4 x5 x7 x8 x16 x19 x st = Curry.RunTimeSystem.patternFail("Pretty.multiGroup_case_6")(x)



c_oneGroup'46outLine'46240_case_9 x1 x2 x3 x5 x6 x7 x8 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43(x2)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x5)(Curry.Module.Prelude.op_45(x6)(x1)(st))(st))(x7)(st))(st)
c_oneGroup'46outLine'46240_case_9 x1 x2 x3 x5 x6 x7 x8 x4@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_replicate(x8)(Curry.Module.Prelude.C_Char(' '))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x5)(Curry.Module.Prelude.op_45(x3)(x8)(st))(st))(x7)(st))(st))
c_oneGroup'46outLine'46240_case_9 x1 x2 x3 x5 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_oneGroup'46outLine'46240_case_9(x1)(x2)(x3)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c_oneGroup'46outLine'46240_case_9 x1 x2 x3 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("Pretty.oneGroup.outLine.240_case_9")(x)



c_noGroup_case_10 x2 x3 x10 x5@((Curry.Module.Prelude.:<) x11 x12) st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_replicate(x11)(Curry.Module.Prelude.C_Char(' '))(st))(Curry.Module.Pretty.c_noGroup(x10)(x2)(Curry.Module.Prelude.op_43(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.Prelude.op_45(x2)(x11)(st))(x5)(st))(st))
c_noGroup_case_10 x2 x3 x10 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_noGroup_case_10(x2)(x3)(x10)(x)(st))(i)(xs)(st)
c_noGroup_case_10 x2 x3 x10 x st = Curry.RunTimeSystem.patternFail("Pretty.noGroup_case_10")(x)



c_string'46_'35lambda4_case_11 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Pretty.c_line(st)
c_string'46_'35lambda4_case_11 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Pretty.c_char(x1)(st)
c_string'46_'35lambda4_case_11 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_string'46_'35lambda4_case_11(x1)(x)(st))(i)(xs)(st)
c_string'46_'35lambda4_case_11 x1 x st = Curry.RunTimeSystem.patternFail("Pretty.string._#lambda4_case_11")(x)



c_fillEncloseSep'46withSoftBreaks'4696_case_12 x2 x3@Curry.Module.Prelude.List st = (Curry.Module.Prelude.:<)(Curry.Module.Pretty.c_group(Curry.Module.Pretty.op_60_62(Curry.Module.Pretty.c_linebreak(st))(x2)(st))(st))(Curry.Module.Prelude.List)
c_fillEncloseSep'46withSoftBreaks'4696_case_12 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = (Curry.Module.Prelude.:<)(Curry.Module.Pretty.c_group(Curry.Module.Pretty.op_60_62(Curry.Module.Pretty.c_linebreak(st))(Curry.Module.Pretty.c_group(Curry.Module.Pretty.op_60_62(x2)(Curry.Module.Pretty.c_linebreak(st))(st))(st))(st))(st))(Curry.Module.Pretty.c_fillEncloseSep'46withSoftBreaks'4696(x3)(st))
c_fillEncloseSep'46withSoftBreaks'4696_case_12 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_fillEncloseSep'46withSoftBreaks'4696_case_12(x2)(x)(st))(i)(xs)(st)
c_fillEncloseSep'46withSoftBreaks'4696_case_12 x2 x st = Curry.RunTimeSystem.patternFail("Pretty.fillEncloseSep.withSoftBreaks.96_case_12")(x)



c_punctuate'46go'4675_case_13 x1 x3 x4@Curry.Module.Prelude.List st = (Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List)
c_punctuate'46go'4675_case_13 x1 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = (Curry.Module.Prelude.:<)(Curry.Module.Pretty.op_60_62(x3)(x1)(st))(Curry.Module.Pretty.c_punctuate'46go'4675(x1)(x4)(st))
c_punctuate'46go'4675_case_13 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Pretty.c_punctuate'46go'4675_case_13(x1)(x3)(x)(st))(i)(xs)(st)
c_punctuate'46go'4675_case_13 x1 x3 x st = Curry.RunTimeSystem.patternFail("Pretty.punctuate.go.75_case_13")(x)


