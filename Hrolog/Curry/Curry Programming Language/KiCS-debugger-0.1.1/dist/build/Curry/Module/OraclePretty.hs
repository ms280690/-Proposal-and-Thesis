{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OraclePretty (module Curry.Module.OraclePretty) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.Pretty
import Curry.Module.Dequeue
import Curry.Module.Prelude
import Curry.Module.OracleDequeue
import Curry.Module.OraclePrelude



-- begin included



-- end included

data C_Doc = C_Doc (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Tokens -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Tokens)))
  | C_DocFail Curry.RunTimeSystem.C_Exceptions
  | C_DocOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.OraclePretty.C_Doc)

data C_Tokens = C_Text (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.OraclePretty.C_Tokens
  | C_Line (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.OraclePretty.C_Tokens
  | C_Open Curry.Module.OraclePretty.C_Tokens
  | C_Close Curry.Module.OraclePretty.C_Tokens
  | C_Empty
  | C_OpenNest (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))))))) Curry.Module.OraclePretty.C_Tokens
  | C_CloseNest Curry.Module.OraclePretty.C_Tokens
  | C_TokensFail Curry.RunTimeSystem.C_Exceptions
  | C_TokensOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.OraclePretty.C_Tokens)

instance BaseCurry Curry.Module.OraclePretty.C_Doc where
  nf f (Curry.Module.OraclePretty.C_Doc x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.OraclePretty.C_Doc(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.OraclePretty.C_Doc x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.OraclePretty.C_Doc(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.OraclePretty.C_DocOr(Curry.RunTimeSystem.mkRef(r)(1)(i))([Curry.Module.OraclePretty.C_Doc(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(1)

  failed  = Curry.Module.OraclePretty.C_DocFail

  branching  = Curry.Module.OraclePretty.C_DocOr

  consKind (Curry.Module.OraclePretty.C_DocOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.OraclePretty.C_DocFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.OraclePretty.C_DocFail x) = x

  orRef (Curry.Module.OraclePretty.C_DocOr x _) = x

  branches (Curry.Module.OraclePretty.C_DocOr _ x) = x





instance BaseCurry Curry.Module.OraclePretty.C_Tokens where
  nf f (Curry.Module.OraclePretty.C_Text x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.OraclePretty.C_Text(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.OraclePretty.C_Line x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.OraclePretty.C_Line(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.OraclePretty.C_Open x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.OraclePretty.C_Open(v1))(state1))(x1)(state0)
  nf f (Curry.Module.OraclePretty.C_Close x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.OraclePretty.C_Close(v1))(state1))(x1)(state0)
  nf f (Curry.Module.OraclePretty.C_OpenNest x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.OraclePretty.C_OpenNest(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.OraclePretty.C_CloseNest x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.OraclePretty.C_CloseNest(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.OraclePretty.C_Text x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.OraclePretty.C_Text(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.OraclePretty.C_Line x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.OraclePretty.C_Line(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.OraclePretty.C_Open x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.OraclePretty.C_Open(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.OraclePretty.C_Close x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.OraclePretty.C_Close(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.OraclePretty.C_OpenNest x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.OraclePretty.C_OpenNest(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.OraclePretty.C_CloseNest x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.OraclePretty.C_CloseNest(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.OraclePretty.C_TokensOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.OraclePretty.C_Text(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.OraclePretty.C_Line(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.OraclePretty.C_Open(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.OraclePretty.C_Close(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.OraclePretty.C_Empty,Curry.Module.OraclePretty.C_OpenNest(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.OraclePretty.C_CloseNest(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.OraclePretty.C_TokensFail

  branching  = Curry.Module.OraclePretty.C_TokensOr

  consKind (Curry.Module.OraclePretty.C_TokensOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.OraclePretty.C_TokensFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.OraclePretty.C_TokensFail x) = x

  orRef (Curry.Module.OraclePretty.C_TokensOr x _) = x

  branches (Curry.Module.OraclePretty.C_TokensOr _ x) = x





instance Curry Curry.Module.OraclePretty.C_Doc where
  strEq (Curry.Module.OraclePretty.C_Doc x1) (Curry.Module.OraclePretty.C_Doc y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.OraclePretty.C_Doc x1) (Curry.Module.OraclePretty.C_Doc y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.OraclePretty.C_Doc x1) st = Curry.Module.OraclePretty.C_Doc(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.OraclePretty.C_Doc x1) st = f(x1)(c)(st)

  typeName _ = "Doc"

  showQ d (Curry.Module.OraclePretty.C_Doc x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("OraclePretty.Doc "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.OraclePretty.C_DocOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.OraclePretty.C_Tokens where
  strEq (Curry.Module.OraclePretty.C_Text x1 x2) (Curry.Module.OraclePretty.C_Text y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.OraclePretty.C_Line x1 x2) (Curry.Module.OraclePretty.C_Line y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.OraclePretty.C_Open x1) (Curry.Module.OraclePretty.C_Open y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.OraclePretty.C_Close x1) (Curry.Module.OraclePretty.C_Close y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq Curry.Module.OraclePretty.C_Empty Curry.Module.OraclePretty.C_Empty st = Curry.Module.Prelude.strEqSuccess
  strEq (Curry.Module.OraclePretty.C_OpenNest x1 x2) (Curry.Module.OraclePretty.C_OpenNest y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.OraclePretty.C_CloseNest x1) (Curry.Module.OraclePretty.C_CloseNest y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.OraclePretty.C_Text x1 x2) (Curry.Module.OraclePretty.C_Text y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.OraclePretty.C_Line x1 x2) (Curry.Module.OraclePretty.C_Line y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.OraclePretty.C_Open x1) (Curry.Module.OraclePretty.C_Open y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.OraclePretty.C_Close x1) (Curry.Module.OraclePretty.C_Close y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq Curry.Module.OraclePretty.C_Empty Curry.Module.OraclePretty.C_Empty st = Curry.Module.Prelude.C_True
  eq (Curry.Module.OraclePretty.C_OpenNest x1 x2) (Curry.Module.OraclePretty.C_OpenNest y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.OraclePretty.C_CloseNest x1) (Curry.Module.OraclePretty.C_CloseNest y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.OraclePretty.C_Text x1 x2) st = Curry.Module.OraclePretty.C_Text(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.OraclePretty.C_Line x1 x2) st = Curry.Module.OraclePretty.C_Line(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.OraclePretty.C_Open x1) st = Curry.Module.OraclePretty.C_Open(f((0::Int))(x1)(st))
  propagate f (Curry.Module.OraclePretty.C_Close x1) st = Curry.Module.OraclePretty.C_Close(f((0::Int))(x1)(st))
  propagate f Curry.Module.OraclePretty.C_Empty st = Curry.Module.OraclePretty.C_Empty
  propagate f (Curry.Module.OraclePretty.C_OpenNest x1 x2) st = Curry.Module.OraclePretty.C_OpenNest(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.OraclePretty.C_CloseNest x1) st = Curry.Module.OraclePretty.C_CloseNest(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.OraclePretty.C_Text x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.OraclePretty.C_Line x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.OraclePretty.C_Open x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.OraclePretty.C_Close x1) st = f(x1)(c)(st)
  foldCurry f c Curry.Module.OraclePretty.C_Empty st = c
  foldCurry f c (Curry.Module.OraclePretty.C_OpenNest x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.OraclePretty.C_CloseNest x1) st = f(x1)(c)(st)

  typeName _ = "Tokens"

  showQ d (Curry.Module.OraclePretty.C_Text x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("OraclePretty.Text "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.OraclePretty.C_Line x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("OraclePretty.Line "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.OraclePretty.C_Open x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("OraclePretty.Open "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.OraclePretty.C_Close x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("OraclePretty.Close "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ Curry.Module.OraclePretty.C_Empty = Prelude.showString("OraclePretty.Empty")
  showQ d (Curry.Module.OraclePretty.C_OpenNest x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("OraclePretty.OpenNest "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.OraclePretty.C_CloseNest x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("OraclePretty.CloseNest "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.OraclePretty.C_TokensOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.OraclePretty.C_Doc where
  showsPrec d (Curry.Module.OraclePretty.C_Doc x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Doc "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.OraclePretty.C_DocOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.OraclePretty.C_Tokens where
  showsPrec d (Curry.Module.OraclePretty.C_Text x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Text "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.OraclePretty.C_Line x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Line "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.OraclePretty.C_Open x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Open "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.OraclePretty.C_Close x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Close "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ Curry.Module.OraclePretty.C_Empty = Prelude.showString("Empty")
  showsPrec d (Curry.Module.OraclePretty.C_OpenNest x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("OpenNest "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.OraclePretty.C_CloseNest x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CloseNest "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.OraclePretty.C_TokensOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Read Curry.Module.OraclePretty.C_Doc where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.OraclePretty.C_Doc(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("OraclePretty")("Doc")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r)





instance Read Curry.Module.OraclePretty.C_Tokens where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.OraclePretty.C_Text(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("OraclePretty")("Text")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.OraclePretty.C_Line(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("OraclePretty")("Line")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.OraclePretty.C_Open(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("OraclePretty")("Open")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.OraclePretty.C_Close(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("OraclePretty")("Close")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.OraclePretty.C_Empty)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("OraclePretty")("Empty")(r)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.OraclePretty.C_OpenNest(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("OraclePretty")("OpenNest")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.OraclePretty.C_CloseNest(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("OraclePretty")("CloseNest")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r)))))))





c_deDoc :: Curry.Module.OraclePretty.C_Doc -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Tokens -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Tokens))
c_deDoc x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_30(x2)(x1)(st))(st)



c_empty :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_empty x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_text(Curry.Module.Prelude.List)(x1)(st))(st)



c_text :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_text x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.OraclePretty.C_Doc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc(Curry.Module.OraclePretty.C_Text(x2))))))(st)



c_linesep :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_linesep x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc(Curry.Module.OraclePretty.C_Doc))))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partCons))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pc))(Curry.Module.OraclePretty.C_Line))(st))(x1)(st))(st)



c_line :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_line x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_linesep(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(x2)(st))(st)



c_linebreak :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_linebreak x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_linesep(x1)(st))(Curry.Module.Prelude.List)(x2)(st))(st)



c_softline :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_softline x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OraclePretty.c_group(Curry.Module.OraclePretty.c_line(x1)(st))(x2)(st))(st)



c_softbreak :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_softbreak x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OraclePretty.c_group(Curry.Module.OraclePretty.c_linebreak(x1)(st))(x2)(st))(st)



c_group :: Curry.Module.OraclePretty.C_Doc -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_group x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OraclePretty.C_Doc(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc(Curry.Module.OraclePretty.C_Open))))(Curry.Module.OraclePrelude.op_46(Curry.Module.OraclePretty.c_deDoc(x2)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc(Curry.Module.OraclePretty.C_Close))))(x3)(st))(x4)(st)))(st)



c_nest :: Curry.Module.Prelude.C_Int -> Curry.Module.OraclePretty.C_Doc -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_nest x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePretty.C_Doc(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc(Curry.Module.OraclePretty.C_OpenNest(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.pf(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))))(Curry.Module.OraclePretty.c_nest'46_'35lambda2(x2)))(st))))))(Curry.Module.OraclePrelude.op_46(Curry.Module.OraclePretty.c_deDoc(x3)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc(Curry.Module.OraclePretty.C_CloseNest))))(x4)(st))(x5)(st)))(st)



c_nest'46_'35lambda2 :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_nest'46_'35lambda2 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_29(x2)(x3)(x1)(st))(st)



c_hang :: Curry.Module.Prelude.C_Int -> Curry.Module.OraclePretty.C_Doc -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_hang x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePretty.C_Doc(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc(Curry.Module.OraclePretty.C_OpenNest(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.pf(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))))(Curry.Module.OraclePretty.c_hang'46_'35lambda3(x2)))(st))))))(Curry.Module.OraclePrelude.op_46(Curry.Module.OraclePretty.c_deDoc(x3)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc(Curry.Module.OraclePretty.C_CloseNest))))(x4)(st))(x5)(st)))(st)



c_hang'46_'35lambda3 :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_hang'46_'35lambda3 x2 x3 x4 x5 x1 st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_45(x5)(x4)(x1)(st))(x2)(x6)(st))(x3))(st)



c_align :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_align x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_hang(Curry.Module.Prelude.C_Zero)))))(st)



c_combine :: Curry.Module.OraclePretty.C_Doc -> Curry.Module.OraclePretty.C_Doc -> Curry.Module.OraclePretty.C_Doc -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_combine x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_enclose(x3)(x4)(x2)(x1)(st))(st)



op_60_62 :: Curry.Module.OraclePretty.C_Doc -> Curry.Module.OraclePretty.C_Doc -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
op_60_62 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePretty.C_Doc(Curry.Module.OraclePrelude.op_46(Curry.Module.OraclePretty.c_deDoc(x2)(x1)(st))(Curry.Module.OraclePretty.c_deDoc(x3)(x4)(st))(x5)(st)))(st)



op_60_43_62 :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))))
op_60_43_62 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_combine(Curry.Module.OraclePretty.c_space(x1)(st))))(st))(st)



op_60_36_62 :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))))
op_60_36_62 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_combine(Curry.Module.OraclePretty.c_line(x1)(st))))(st))(st)



op_60_47_62 :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))))
op_60_47_62 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_combine(Curry.Module.OraclePretty.c_softline(x1)(st))))(st))(st)



op_60_36_36_62 :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))))
op_60_36_36_62 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_combine(Curry.Module.OraclePretty.c_linebreak(x1)(st))))(st))(st)



op_60_47_47_62 :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))))
op_60_47_47_62 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_combine(Curry.Module.OraclePretty.c_softbreak(x1)(st))))(st))(st)



c_compose :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))))) -> (Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_compose x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_28(x2)(x3)(x1)(st))(st)



c_hsep :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_hsep x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_compose(Curry.Module.OraclePretty.op_60_43_62(x1)(st))))))(st)



c_vsep :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_vsep x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_compose(Curry.Module.OraclePretty.op_60_36_62(x1)(st))))))(st)



c_fillSep :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_fillSep x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_compose(Curry.Module.OraclePretty.op_60_47_62(x1)(st))))))(st)



c_sep :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_sep x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_group))))(Curry.Module.OraclePretty.c_vsep(x1)(st))(x2)(st))(st)



c_hcat :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_hcat x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_compose(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.op_60_62))(st))))))(st)



c_vcat :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_vcat x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_compose(Curry.Module.OraclePretty.op_60_36_36_62(x1)(st))))))(st)



c_fillCat :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_fillCat x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_compose(Curry.Module.OraclePretty.op_60_47_47_62(x1)(st))))))(st)



c_cat :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_cat x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_group))))(Curry.Module.OraclePretty.c_vcat(x1)(st))(x2)(st))(st)



c_punctuate :: Curry.Module.OraclePretty.C_Doc -> (Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc
c_punctuate x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_27(x2)(x3)(x1)(st))(st)



c_punctuate'46go'4675 :: Curry.Module.OraclePretty.C_Doc -> (Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc
c_punctuate'46go'4675 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_26(x2)(x3)(x1)(st))(st)



c_encloseSep :: Curry.Module.OraclePretty.C_Doc -> Curry.Module.OraclePretty.C_Doc -> Curry.Module.OraclePretty.C_Doc -> (Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_encloseSep x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_24(x2)(x3)(x4)(x5)(x1)(st))(st)



c_hEncloseSep :: Curry.Module.OraclePretty.C_Doc -> Curry.Module.OraclePretty.C_Doc -> Curry.Module.OraclePretty.C_Doc -> (Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_hEncloseSep x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_23(x2)(x3)(x4)(x5)(x1)(st))(st)



c_fillEncloseSep :: Curry.Module.OraclePretty.C_Doc -> Curry.Module.OraclePretty.C_Doc -> Curry.Module.OraclePretty.C_Doc -> (Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_fillEncloseSep x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_22(x2)(x3)(x4)(x5)(x1)(st))(st)



c_fillEncloseSep'46withSoftBreaks'4696 :: (Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc
c_fillEncloseSep'46withSoftBreaks'4696 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_21(x2)(x1)(st))(st)



c_list :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_list x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List)))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_fillEncloseSep(Curry.Module.OraclePretty.c_lbracket(x1)(st))(Curry.Module.OraclePretty.c_rbracket(x2)(st))(Curry.Module.OraclePretty.c_comma(x3)(st))))))(st)



c_tupled :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_tupled x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List)))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_fillEncloseSep(Curry.Module.OraclePretty.c_lparen(x1)(st))(Curry.Module.OraclePretty.c_rparen(x2)(st))(Curry.Module.OraclePretty.c_comma(x3)(st))))))(st)



c_semiBraces :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_semiBraces x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List)))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_fillEncloseSep(Curry.Module.OraclePretty.c_lbrace(x1)(st))(Curry.Module.OraclePretty.c_rbrace(x2)(st))(Curry.Module.OraclePretty.c_semi(x3)(st))))))(st)



c_enclose :: Curry.Module.OraclePretty.C_Doc -> Curry.Module.OraclePretty.C_Doc -> Curry.Module.OraclePretty.C_Doc -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_enclose x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OraclePretty.op_60_62(Curry.Module.OraclePretty.op_60_62(x2)(x4)(x1)(st))(x3)(x5)(st))(st)



c_squotes :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_squotes x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_enclose(Curry.Module.OraclePretty.c_squote(x1)(st))(Curry.Module.OraclePretty.c_squote(x2)(st))))))(st)



c_dquotes :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_dquotes x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_enclose(Curry.Module.OraclePretty.c_dquote(x1)(st))(Curry.Module.OraclePretty.c_dquote(x2)(st))))))(st)



c_bquotes :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_bquotes x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_enclose(Curry.Module.OraclePretty.c_bquote(x1)(st))(Curry.Module.OraclePretty.c_bquote(x2)(st))))))(st)



c_parens :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_parens x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_enclose(Curry.Module.OraclePretty.c_lparen(x1)(st))(Curry.Module.OraclePretty.c_rparen(x2)(st))))))(st)



c_angles :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_angles x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_enclose(Curry.Module.OraclePretty.c_langle(x1)(st))(Curry.Module.OraclePretty.c_rangle(x2)(st))))))(st)



c_braces :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_braces x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_enclose(Curry.Module.OraclePretty.c_lbrace(x1)(st))(Curry.Module.OraclePretty.c_rbrace(x2)(st))))))(st)



c_brackets :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_brackets x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_enclose(Curry.Module.OraclePretty.c_lbracket(x1)(st))(Curry.Module.OraclePretty.c_rbracket(x2)(st))))))(st)



c_char :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_char x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_text((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(x1)(st))(st)



c_string :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_string x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_46(Curry.Module.OraclePretty.c_hcat(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_string'46_'35lambda4))))))))(x2)(st))(st)



c_string'46_'35lambda4 :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_string'46_'35lambda4 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OraclePretty.c__case_19(x2)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_elem(x2)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\r'))(Curry.Module.Prelude.List)))(x3)(st))(x4)(st))(st)



c_int :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_int x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePretty.c_text(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))(x3)(st))(st)



c_float :: Curry.Module.Prelude.C_Float -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_float x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePretty.c_text(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))(x3)(st))(st)



c_lparen :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_lparen x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_char(Curry.Module.Prelude.C_Char('('))(x1)(st))(st)



c_rparen :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_rparen x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_char(Curry.Module.Prelude.C_Char(')'))(x1)(st))(st)



c_langle :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_langle x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_char(Curry.Module.Prelude.C_Char('<'))(x1)(st))(st)



c_rangle :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_rangle x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_char(Curry.Module.Prelude.C_Char('>'))(x1)(st))(st)



c_lbrace :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_lbrace x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_char(Curry.Module.Prelude.C_Char('{'))(x1)(st))(st)



c_rbrace :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_rbrace x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_char(Curry.Module.Prelude.C_Char('}'))(x1)(st))(st)



c_lbracket :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_lbracket x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_char(Curry.Module.Prelude.C_Char('['))(x1)(st))(st)



c_rbracket :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_rbracket x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_char(Curry.Module.Prelude.C_Char(']'))(x1)(st))(st)



c_squote :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_squote x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_char(Curry.Module.Prelude.C_Char('\''))(x1)(st))(st)



c_dquote :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_dquote x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_char(Curry.Module.Prelude.C_Char('\"'))(x1)(st))(st)



c_bquote :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_bquote x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_char(Curry.Module.Prelude.C_Char('`'))(x1)(st))(st)



c_semi :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_semi x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_char(Curry.Module.Prelude.C_Char(';'))(x1)(st))(st)



c_colon :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_colon x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_char(Curry.Module.Prelude.C_Char(':'))(x1)(st))(st)



c_comma :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_comma x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_char(Curry.Module.Prelude.C_Char(','))(x1)(st))(st)



c_space :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_space x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_char(Curry.Module.Prelude.C_Char(' '))(x1)(st))(st)



c_dot :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_dot x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_char(Curry.Module.Prelude.C_Char('.'))(x1)(st))(st)



c_backslash :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_backslash x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_char(Curry.Module.Prelude.C_Char('\\'))(x1)(st))(st)



c_equals :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_equals x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_char(Curry.Module.Prelude.C_Char('='))(x1)(st))(st)



c_normalise :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Tokens -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Tokens))
c_normalise x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_normalise'46go'46173(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_id))))))))(st)



c_normalise'46open'46173 :: Curry.Module.OraclePretty.C_Tokens -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Tokens
c_normalise'46open'46173 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_18(x2)(x1)(st))(st)



c_normalise'46go'46173 :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Tokens -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Tokens))) -> Curry.Module.OraclePretty.C_Tokens -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Tokens
c_normalise'46go'46173 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_17(x2)(x3)(x1)(st))(st)



c_doc2Tokens :: Curry.Module.OraclePretty.C_Doc -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Tokens
c_doc2Tokens x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_16(x2)(x1)(st))(st)



c_pretty :: Curry.Module.Prelude.C_Int -> Curry.Module.OraclePretty.C_Doc -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_pretty x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePretty.c_noGroup(Curry.Module.OraclePretty.c_doc2Tokens(x3)(x1)(st))(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.List))(x4)(st))(st)



c_length :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int))
c_length x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_length))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_filter(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_not))))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_elem))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.c_enumFromTo(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(x1)(st))(x2)(st))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_ord))))(x3)(st))(x4)(st))))))(x5)(st))(st)



c_noGroup :: Curry.Module.OraclePretty.C_Tokens -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_noGroup x2 x3 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_15(x3)(x4)(x5)(x6)(x2)(x1)(st))(st)



c_noGroup'46_'35lambda6 :: Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_noGroup'46_'35lambda6 x2 x3 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)



c_oneGroup :: Curry.Module.OraclePretty.C_Tokens -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_oneGroup x2 x3 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_13(x3)(x4)(x5)(x6)(x2)(x1)(st))(st)



c_oneGroup'46outText'46234 :: (Curry t0) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.Prelude.C_Int -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_oneGroup'46outText'46234 x2 x3 x4 x5 x6 x1 st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(x3)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x4)(Curry.Module.OraclePrelude.op_45(x5)(x2)(x1)(st))(x7)(st))(x6)(x8)(st))(x9)(st))(st)



c_oneGroup'46_'35lambda7 :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_oneGroup'46_'35lambda7 x2 x3 x4 x5 x6 x1 st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x3)(x5)(x1)(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_oneGroup'46outText'46234(x2)(x4)(x6)))(st))(x7)(st))(st)



c_oneGroup'46outLine'46240 :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_oneGroup'46outLine'46240 x2 x3 x4 x5 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_12(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x1)(st))(st)



c_oneGroup'46_'35lambda8 :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_oneGroup'46_'35lambda8 x2 x3 x4 x5 x6 x7 x1 st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x3)(x6)(x1)(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_oneGroup'46outLine'46240(x2)(x4)(x5)(x6)(x7)))(st))(x8)(st))(st)



c_oneGroup'46_'35lambda9 :: Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_oneGroup'46_'35lambda9 x2 x3 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)



c_oneGroup'46_'35lambda10 :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))))))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_oneGroup'46_'35lambda10 x2 x3 x4 x5 x6 x1 st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x3)(x5)(x1)(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_oneGroup'46_'35lambda10'46_'35lambda11(x6)(x2)(x4)))(st))(x7)(st))(st)



c_oneGroup'46_'35lambda10'46_'35lambda11 :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))))))) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_oneGroup'46_'35lambda10'46_'35lambda11 x2 x3 x4 x5 x6 x1 st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x5)(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x3)(x6)(x7)(st))(x5)(x8)(st))(x4)(x9)(st))(x10)(st))(st)



c_oneGroup'46_'35lambda12 :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_oneGroup'46_'35lambda12 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x3)(x1)(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_oneGroup'46_'35lambda12'46_'35lambda13(x4)))(st))(x5)(st))(st)



c_oneGroup'46_'35lambda12'46_'35lambda13 :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_oneGroup'46_'35lambda12'46_'35lambda13 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x3)(x1)(st))(Curry.Module.OraclePrelude.c_tail(x4)(x5)(st))(x6)(st))(st)



c_multiGroup :: Curry.Module.OraclePretty.C_Tokens -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> (Curry.Module.Dequeue.C_Queue (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))))) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_multiGroup x2 x3 x4 x5 x6 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_10(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x2)(x1)(st))(st)



c_multiGroup'46outText'46261 :: (Curry t0) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.Prelude.C_Int -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_multiGroup'46outText'46261 x2 x3 x4 x5 x6 x1 st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(x3)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x4)(Curry.Module.OraclePrelude.op_45(x5)(x2)(x1)(st))(x7)(st))(x6)(x8)(st))(x9)(st))(st)



c_multiGroup'46_'35lambda14 :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_multiGroup'46_'35lambda14 x2 x3 x4 x5 x6 x1 st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x3)(x5)(x1)(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_multiGroup'46outText'46261(x2)(x4)(x6)))(st))(x7)(st))(st)



c_multiGroup'46outLine'46267 :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_multiGroup'46outLine'46267 x2 x3 x4 x5 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_6(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x1)(st))(st)



c_multiGroup'46_'35lambda15 :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_multiGroup'46_'35lambda15 x2 x3 x4 x5 x6 x7 x1 st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x3)(x6)(x1)(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_multiGroup'46outLine'46267(x2)(x4)(x5)(x6)(x7)))(st))(x8)(st))(st)



c_multiGroup'46_'35lambda16 :: Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_multiGroup'46_'35lambda16 x2 x3 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)



c_multiGroup'46_'35lambda18 :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_multiGroup'46_'35lambda18 x2 x3 x4 x5 x6 x7 x1 st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x3)(x6)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_multiGroup'46_'35lambda18'46_'35lambda19(x7)(x2)(x4)(x5)))))(x8)(st))(st)



c_multiGroup'46_'35lambda18'46_'35lambda19 :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_multiGroup'46_'35lambda18'46_'35lambda19 x2 x3 x4 x5 x6 x1 st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x3)(Curry.Module.OraclePrelude.op_60_61(x4)(Curry.Module.OraclePrelude.op_43(x5)(x6)(x1)(st))(x7)(st))(x8)(st))(x2)(x9)(st))(x6)(x10)(st))(st)



c_multiGroup'46_'35lambda20 :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_multiGroup'46_'35lambda20 x2 x3 x4 x5 x6 x7 x1 st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x6)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_multiGroup'46_'35lambda20'46_'35lambda21(x7)(x3)(x4)(x5)))))(x8)(st))(st)



c_multiGroup'46_'35lambda20'46_'35lambda21 :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_multiGroup'46_'35lambda20'46_'35lambda21 x2 x3 x4 x5 x6 x1 st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x3)(Curry.Module.OraclePrelude.op_60_61(x4)(Curry.Module.OraclePrelude.op_43(x5)(x6)(x1)(st))(x7)(st))(x8)(st))(x2)(x9)(st))(x6)(x10)(st))(st)



c_multiGroup'46_'35lambda22 :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))))))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_multiGroup'46_'35lambda22 x2 x3 x4 x5 x6 x1 st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x3)(x5)(x1)(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_multiGroup'46_'35lambda22'46_'35lambda23(x6)(x2)(x4)))(st))(x7)(st))(st)



c_multiGroup'46_'35lambda22'46_'35lambda23 :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))))))) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_multiGroup'46_'35lambda22'46_'35lambda23 x2 x3 x4 x5 x6 x1 st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x5)(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x3)(x6)(x7)(st))(x5)(x8)(st))(x4)(x9)(st))(x10)(st))(st)



c_multiGroup'46_'35lambda24 :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_multiGroup'46_'35lambda24 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x3)(x1)(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_multiGroup'46_'35lambda24'46_'35lambda25(x4)))(st))(x5)(st))(st)



c_multiGroup'46_'35lambda24'46_'35lambda25 :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_multiGroup'46_'35lambda24'46_'35lambda25 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x3)(x1)(st))(Curry.Module.OraclePrelude.c_tail(x4)(x5)(st))(x6)(st))(st)



c_pruneOne :: Curry.Module.OraclePretty.C_Tokens -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_pruneOne x2 x3 x4 x5 x6 x1 st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OraclePretty.c__case_4(x2)(x3)(x4)(x5)(x6)(Curry.Module.OraclePrelude.op_60_61(x4)(x5)(x1)(st))(x7)(st))(st)



c_pruneMulti :: Curry.Module.OraclePretty.C_Tokens -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> (Curry.Module.Dequeue.C_Queue (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))))) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_pruneMulti x2 x3 x4 x5 x6 x7 x8 x9 x1 st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.OraclePretty.c__case_3(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(Curry.Module.OraclePrelude.op_60_61(x4)(x5)(x1)(st))(x10)(st))(st)



c_pruneMulti'46_'35lambda26 :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Dequeue.C_Queue (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))))) -> Curry.Module.Prelude.C_Int -> Curry.Module.OraclePretty.C_Tokens -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_pruneMulti'46_'35lambda26 x2 x3 x4 x5 x6 x7 x8 x1 st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OraclePretty.c__case_2(x2)(x3)(x4)(x5)(x6)(x7)(x8)(Curry.Module.OracleDequeue.c_matchLast(x4)(x1)(st))(x9)(st))(st)



c__case_2 x2 x3 x4 x5 x6 x7 x8 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_2_case__30(x1)(x2)(x3)(x5)(x6)(x7)(x8)(x10)(st))(st)



c__case_1 x2 x3 x5 x6 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_1_case__29(x1)(x2)(x3)(x5)(x6)(x7)(x8)(x9)(st))(st)



c__case_0 x2 x3 x5 x6 x7 x8 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_0_case__28(x1)(x2)(x3)(x5)(x6)(x7)(x8)(x11)(x10)(st))(st)



c__case_3 x2 x3 x4 x5 x6 x7 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_3_case__27(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x10)(st))(st)



c__case_4 x2 x3 x4 x5 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_4_case__26(x1)(x2)(x3)(x4)(x5)(x6)(x7)(st))(st)



c__case_6 x2 x3 x4 x5 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_6_case__25(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(st))(st)



c__case_5 x2 x3 x4 x6 x7 x8 x9 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_5_case__24(x1)(x2)(x3)(x4)(x6)(x7)(x8)(x9)(x5)(st))(st)



c__case_10 x3 x4 x5 x6 x7 x8 x9 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_10_case__23(x1)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x2)(st))(st)



c__case_9 x3 x4 x5 x6 x7 x8 x9 x17 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_9_case__22(x1)(x3)(x4)(x5)(x6)(x8)(x9)(x17)(x19)(st))(st)



c__case_8 x3 x4 x5 x6 x8 x9 x17 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_8_case__21(x1)(x3)(x4)(x5)(x6)(x8)(x9)(x17)(x18)(st))(st)



c__case_7 x3 x4 x5 x6 x8 x9 x17 x20 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_7_case__20(x1)(x3)(x4)(x5)(x6)(x8)(x9)(x17)(x20)(x19)(st))(st)



c__case_12 x2 x3 x4 x5 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_12_case__19(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(st))(st)



c__case_11 x2 x3 x4 x6 x7 x8 x9 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_11_case__18(x1)(x2)(x3)(x4)(x6)(x7)(x8)(x9)(x5)(st))(st)



c__case_13 x3 x4 x5 x6 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_13_case__17(x1)(x3)(x4)(x5)(x6)(x2)(st))(st)



c__case_15 x3 x4 x5 x6 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_15_case__16(x1)(x3)(x4)(x5)(x6)(x2)(st))(st)



c__case_14 x3 x4 x11 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_14_case__15(x1)(x3)(x4)(x11)(x6)(st))(st)



c__case_16 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_16_case__14(x1)(x2)(st))(st)



c__case_17 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_17_case__13(x1)(x2)(x3)(st))(st)



c__case_18 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_18_case__12(x1)(x2)(st))(st)



c__case_19 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_19_case__11(x1)(x2)(x3)(st))(st)



c__case_21 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_21_case__10(x1)(x2)(st))(st)



c__case_20 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_20_case__9(x1)(x3)(x4)(st))(st)



c__case_22 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_22_case__8(x1)(x2)(x3)(x4)(x5)(st))(st)



c__case_23 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_23_case__7(x1)(x2)(x3)(x4)(x5)(st))(st)



c__case_24 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_24_case__6(x1)(x2)(x3)(x4)(x5)(st))(st)



c__case_26 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_26_case__5(x1)(x2)(x3)(st))(st)



c__case_25 x2 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_25_case__4(x1)(x2)(x4)(x5)(st))(st)



c__case_27 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_27_case__3(x1)(x2)(x3)(st))(st)



c__case_28 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_28_case__2(x1)(x2)(x3)(st))(st)



c__case_29 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_29_case__1(x1)(x2)(x3)(st))(st)



c__case_30 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_30_case__0(x1)(x2)(st))(st)



c__case_30_case__0 x1 x2@(Curry.Module.OraclePretty.C_Doc x3) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_30_case__0 x1 (Curry.Module.OraclePretty.C_DocOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_30_case__0(x1)(x)(st))(i)(xs)(st)
c__case_30_case__0 x1 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_30_case__0")(x)



c__case_29_case__1 x1 x2 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.op_43(x6)(x2)(x1)(st))(x3))(st)
c__case_29_case__1 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_29_case__1(x1)(x2)(x)(st))(i)(xs)(st)
c__case_29_case__1 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_29_case__1")(x)



c__case_28_case__2 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_empty(x1)(st))(st)
c__case_28_case__2 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_foldr1(x2)(x3)(x1)(st))(st)
c__case_28_case__2 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_28_case__2(x1)(x2)(x)(st))(i)(xs)(st)
c__case_28_case__2 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_28_case__2")(x)



c__case_27_case__3 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_27_case__3 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_punctuate'46go'4675(x2)(x3)(x1)(st))(st)
c__case_27_case__3 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_27_case__3(x1)(x2)(x)(st))(i)(xs)(st)
c__case_27_case__3 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_27_case__3")(x)



c__case_25_case__4 x1 x2 x4 x5@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(st)
c__case_25_case__4 x1 x2 x4 x5@((Curry.Module.Prelude.:<) x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.OraclePretty.op_60_62(x4)(x2)(x1)(st))(Curry.Module.OraclePretty.c_punctuate'46go'4675(x2)(x5)(x8)(st)))(st)
c__case_25_case__4 x1 x2 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_25_case__4(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_25_case__4 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_25_case__4")(x)



c__case_26_case__5 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_25(x2)(x4)(x5)(x1)(st))(st)
c__case_26_case__5 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_26_case__5(x1)(x2)(x)(st))(i)(xs)(st)
c__case_26_case__5 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_26_case__5")(x)



c__case_24_case__6 x1 x2 x3 x4 x5@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.op_60_62(x2)(x3)(x1)(st))(st)
c__case_24_case__6 x1 x2 x3 x4 x5@((Curry.Module.Prelude.:<) x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_align(x1)(st))(Curry.Module.OraclePretty.c_enclose(x2)(x3)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_cat(x8)(st))((Curry.Module.Prelude.:<)(x6)(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.op_60_62(x4)))))(x7)(x9)(st)))(x10)(st))(x11)(st))(x12)(st))(st)
c__case_24_case__6 x1 x2 x3 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_24_case__6(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_24_case__6 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_24_case__6")(x)



c__case_23_case__7 x1 x2 x3 x4 x5@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.op_60_62(x2)(x3)(x1)(st))(st)
c__case_23_case__7 x1 x2 x3 x4 x5@((Curry.Module.Prelude.:<) x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_align(x1)(st))(Curry.Module.OraclePretty.c_enclose(x2)(x3)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_hcat(x8)(st))((Curry.Module.Prelude.:<)(x6)(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.op_60_62(x4)))))(x7)(x9)(st)))(x10)(st))(x11)(st))(x12)(st))(st)
c__case_23_case__7 x1 x2 x3 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_23_case__7(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_23_case__7 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_23_case__7")(x)



c__case_22_case__8 x1 x2 x3 x4 x5@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.op_60_62(x2)(x3)(x1)(st))(st)
c__case_22_case__8 x1 x2 x3 x4 x5@((Curry.Module.Prelude.:<) x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List)))))))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_align(x1)(st))(Curry.Module.OraclePretty.c_enclose(x2)(x3)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_hcat(x8)(st))((Curry.Module.Prelude.:<)(x6)(Curry.Module.OraclePretty.c_fillEncloseSep'46withSoftBreaks'4696(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.op_60_62(x4)))))(x7)(x9)(st))(x10)(st)))(x11)(st))(x12)(st))(x13)(st))(st)
c__case_22_case__8 x1 x2 x3 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_22_case__8(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_22_case__8 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_22_case__8")(x)



c__case_20_case__9 x1 x3 x4@Curry.Module.Prelude.List st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))((Curry.Module.Prelude.:<)(Curry.Module.OraclePretty.c_group(Curry.Module.OraclePretty.op_60_62(Curry.Module.OraclePretty.c_linebreak(x1)(st))(x3)(x7)(st))(x8)(st))(Curry.Module.Prelude.List))(st)
c__case_20_case__9 x1 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)(Curry.Module.OraclePretty.c_group(Curry.Module.OraclePretty.op_60_62(Curry.Module.OraclePretty.c_linebreak(x1)(st))(Curry.Module.OraclePretty.c_group(Curry.Module.OraclePretty.op_60_62(x3)(Curry.Module.OraclePretty.c_linebreak(x9)(st))(x10)(st))(x11)(st))(x12)(st))(x13)(st))(Curry.Module.OraclePretty.c_fillEncloseSep'46withSoftBreaks'4696(x4)(x14)(st)))(st)
c__case_20_case__9 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_20_case__9(x1)(x3)(x)(st))(i)(xs)(st)
c__case_20_case__9 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_20_case__9")(x)



c__case_21_case__10 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_21_case__10 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_20(x3)(x4)(x1)(st))(st)
c__case_21_case__10 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_21_case__10(x1)(x)(st))(i)(xs)(st)
c__case_21_case__10 x1 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_21_case__10")(x)



c__case_19_case__11 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_line(x1)(st))(st)
c__case_19_case__11 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_char(x2)(x1)(st))(st)
c__case_19_case__11 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_19_case__11(x1)(x2)(x)(st))(i)(xs)(st)
c__case_19_case__11 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_19_case__11")(x)



c__case_18_case__12 x1 x2@(Curry.Module.OraclePretty.C_Close x3) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_18_case__12 x1 x2@(Curry.Module.OraclePretty.C_Text x4 x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.OraclePretty.C_Open(x2))(st)
c__case_18_case__12 x1 x2@(Curry.Module.OraclePretty.C_Line x6 x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.OraclePretty.C_Open(x2))(st)
c__case_18_case__12 x1 x2@(Curry.Module.OraclePretty.C_Open x8) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.OraclePretty.C_Open(x2))(st)
c__case_18_case__12 x1 x2@Curry.Module.OraclePretty.C_Empty st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.OraclePretty.C_Open(x2))(st)
c__case_18_case__12 x1 x2@(Curry.Module.OraclePretty.C_OpenNest x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.OraclePretty.C_Open(x2))(st)
c__case_18_case__12 x1 x2@(Curry.Module.OraclePretty.C_CloseNest x11) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.OraclePretty.C_Open(x2))(st)
c__case_18_case__12 x1 (Curry.Module.OraclePretty.C_TokensOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_18_case__12(x1)(x)(st))(i)(xs)(st)
c__case_18_case__12 x1 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_18_case__12")(x)



c__case_17_case__13 x1 x2 x3@Curry.Module.OraclePretty.C_Empty st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.c_apply(x2)(Curry.Module.OraclePretty.C_Empty)(x1)(st))(st)
c__case_17_case__13 x1 x2 x3@(Curry.Module.OraclePretty.C_Open x4) st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(Curry.Module.OraclePretty.c_normalise'46go'46173(Curry.Module.OraclePrelude.op_46(x2)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_normalise'46open'46173))))(x1)(st))(x4)(x13)(st))(st)
c__case_17_case__13 x1 x2 x3@(Curry.Module.OraclePretty.C_Close x5) st = let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(Curry.Module.OraclePretty.c_normalise'46go'46173(Curry.Module.OraclePrelude.op_46(x2)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc(Curry.Module.OraclePretty.C_Close))))(x1)(st))(x5)(x14)(st))(st)
c__case_17_case__13 x1 x2 x3@(Curry.Module.OraclePretty.C_Line x6 x7) st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_36(Curry.Module.OraclePrelude.op_46(x2)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc(Curry.Module.OraclePretty.C_Line(x6)))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_normalise'46go'46173(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_id))))))))(x1)(st))(x15)(st))(x7)(x16)(st))(st)
c__case_17_case__13 x1 x2 x3@(Curry.Module.OraclePretty.C_Text x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.C_Text(x8)(Curry.Module.OraclePretty.c_normalise'46go'46173(x2)(x9)(x1)(st)))(st)
c__case_17_case__13 x1 x2 x3@(Curry.Module.OraclePretty.C_OpenNest x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.C_OpenNest(x10)(Curry.Module.OraclePretty.c_normalise'46go'46173(x2)(x11)(x1)(st)))(st)
c__case_17_case__13 x1 x2 x3@(Curry.Module.OraclePretty.C_CloseNest x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.C_CloseNest(Curry.Module.OraclePretty.c_normalise'46go'46173(x2)(x12)(x1)(st)))(st)
c__case_17_case__13 x1 x2 (Curry.Module.OraclePretty.C_TokensOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_17_case__13(x1)(x2)(x)(st))(i)(xs)(st)
c__case_17_case__13 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_17_case__13")(x)



c__case_16_case__14 x1 x2@(Curry.Module.OraclePretty.C_Doc x3) st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_normalise(x1)(st))(Curry.Module.Oracle.c_apply(x3)(Curry.Module.OraclePretty.C_Empty)(x4)(st))(x5)(st))(st)
c__case_16_case__14 x1 (Curry.Module.OraclePretty.C_DocOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_16_case__14(x1)(x)(st))(i)(xs)(st)
c__case_16_case__14 x1 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_16_case__14")(x)



c__case_14_case__15 x1 x3 x4 x11 x6@((Curry.Module.Prelude.:<) x12 x13) st = let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List)))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_replicate(x12)(Curry.Module.Prelude.C_Char(' '))(x1)(st))(Curry.Module.OraclePretty.c_noGroup(x11)(x3)(Curry.Module.OraclePrelude.op_43(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x14)(st))(Curry.Module.OraclePrelude.op_45(x3)(x12)(x15)(st))(x6)(x16)(st))(x17)(st)))(st)
c__case_14_case__15 x1 x3 x4 x11 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_14_case__15(x1)(x3)(x4)(x11)(x)(st))(i)(xs)(st)
c__case_14_case__15 x1 x3 x4 x11 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_14_case__15")(x)



c__case_15_case__16 x1 x3 x4 x5 x6 x2@Curry.Module.OraclePretty.C_Empty st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_15_case__16 x1 x3 x4 x5 x6 x2@(Curry.Module.OraclePretty.C_Text x7 x8) st = let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List)))(let {x9 = Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_length(x1)(st))(x7)(x19)(st)} in let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(x7)(Curry.Module.OraclePretty.c_noGroup(x8)(x3)(Curry.Module.OraclePrelude.op_43(x4)(x9)(x20)(st))(Curry.Module.OraclePrelude.op_45(x5)(x9)(x21)(st))(x6)(x22)(st))(x23)(st))(st))(st)
c__case_15_case__16 x1 x3 x4 x5 x6 x2@(Curry.Module.OraclePretty.C_Line x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_14(x3)(x4)(x11)(x6)(x1)(st))(st)
c__case_15_case__16 x1 x3 x4 x5 x6 x2@(Curry.Module.OraclePretty.C_Open x14) st = let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_oneGroup(x14)(x3)(x4)(Curry.Module.OraclePrelude.op_43(x4)(x5)(x1)(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_noGroup'46_'35lambda6))(st))(x24)(st))(x5)(x25)(st))(x6)(x26)(st))(st)
c__case_15_case__16 x1 x3 x4 x5 x6 x2@(Curry.Module.OraclePretty.C_Close x15) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_noGroup(x15)(x3)(x4)(x5)(x6)(x1)(st))(st)
c__case_15_case__16 x1 x3 x4 x5 x6 x2@(Curry.Module.OraclePretty.C_OpenNest x16 x17) st = let {x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x27)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)(Curry.Module.Prelude.List))))(Curry.Module.OraclePretty.c_noGroup(x17)(x3)(x4)(x5)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x16)(x6)(x1)(st))(x5)(x27)(st))(x3)(x28)(st))(x29)(st))(st)
c__case_15_case__16 x1 x3 x4 x5 x6 x2@(Curry.Module.OraclePretty.C_CloseNest x18) st = let {x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x30)(Curry.Module.Prelude.List))(Curry.Module.OraclePretty.c_noGroup(x18)(x3)(x4)(x5)(Curry.Module.OraclePrelude.c_tail(x6)(x1)(st))(x30)(st))(st)
c__case_15_case__16 x1 x3 x4 x5 x6 (Curry.Module.OraclePretty.C_TokensOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_15_case__16(x1)(x3)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_15_case__16 x1 x3 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_15_case__16")(x)



c__case_13_case__17 x1 x3 x4 x5 x6 x2@(Curry.Module.OraclePretty.C_Text x7 x8) st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List)))(let {x9 = Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_length(x1)(st))(x7)(x18)(st)} in let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))(Curry.Module.OraclePretty.c_pruneOne(x8)(x3)(Curry.Module.OraclePrelude.op_43(x4)(x9)(x19)(st))(x5)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_oneGroup'46_'35lambda7(x9)(x6)(x7)))(st))(x20)(st))(st))(st)
c__case_13_case__17 x1 x3 x4 x5 x6 x2@(Curry.Module.OraclePretty.C_Line x10 x11) st = let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List)))(let {x12 = Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_length(x1)(st))(x10)(x21)(st)} in let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x22)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List))(Curry.Module.OraclePretty.c_pruneOne(x11)(x3)(Curry.Module.OraclePrelude.op_43(x4)(x12)(x22)(st))(x5)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_oneGroup'46_'35lambda8(x12)(x6)(x10)(x3)))(st))(x23)(st))(st))(st)
c__case_13_case__17 x1 x3 x4 x5 x6 x2@(Curry.Module.OraclePretty.C_Open x13) st = let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List))(Curry.Module.OraclePretty.c_multiGroup(x13)(x3)(x4)(x5)(x6)(Curry.Module.OracleDequeue.c_empty(x1)(st))(x4)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_oneGroup'46_'35lambda9))(st))(x24)(st))(st)
c__case_13_case__17 x1 x3 x4 x5 x6 x2@(Curry.Module.OraclePretty.C_Close x14) st = let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x6)(Curry.Module.OraclePrelude.op_60_61(x4)(x5)(x1)(st))(x25)(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_noGroup(x14)(x3)(x4)))(st))(x26)(st))(st)
c__case_13_case__17 x1 x3 x4 x5 x6 x2@(Curry.Module.OraclePretty.C_OpenNest x15 x16) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_oneGroup(x16)(x3)(x4)(x5)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_oneGroup'46_'35lambda10(x15)(x6)(x3)))(st))(x1)(st))(st)
c__case_13_case__17 x1 x3 x4 x5 x6 x2@(Curry.Module.OraclePretty.C_CloseNest x17) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_oneGroup(x17)(x3)(x4)(x5)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_oneGroup'46_'35lambda12(x6)))(st))(x1)(st))(st)
c__case_13_case__17 x1 x3 x4 x5 x6 (Curry.Module.OraclePretty.C_TokensOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_13_case__17(x1)(x3)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_13_case__17 x1 x3 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_13_case__17")(x)



c__case_11_case__18 x1 x2 x3 x4 x6 x7 x8 x9 x5@Curry.Module.Prelude.C_True st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(x3)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x6)(Curry.Module.OraclePrelude.op_45(x7)(x2)(x1)(st))(x10)(st))(x8)(x11)(st))(x12)(st))(st)
c__case_11_case__18 x1 x2 x3 x4 x6 x7 x8 x9 x5@Curry.Module.Prelude.C_False st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List)))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_replicate(x9)(Curry.Module.Prelude.C_Char(' '))(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x6)(Curry.Module.OraclePrelude.op_45(x4)(x9)(x13)(st))(x14)(st))(x8)(x15)(st))(x16)(st)))(st)
c__case_11_case__18 x1 x2 x3 x4 x6 x7 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_11_case__18(x1)(x2)(x3)(x4)(x6)(x7)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_11_case__18 x1 x2 x3 x4 x6 x7 x8 x9 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_11_case__18")(x)



c__case_12_case__19 x1 x2 x3 x4 x5 x6 x7 x8@((Curry.Module.Prelude.:<) x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_11(x2)(x3)(x4)(x6)(x7)(x8)(x9)(x5)(x1)(st))(st)
c__case_12_case__19 x1 x2 x3 x4 x5 x6 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_12_case__19(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_12_case__19 x1 x2 x3 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_12_case__19")(x)



c__case_7_case__20 x1 x3 x4 x5 x6 x8 x9 x17 x20 x19@(Curry.Module.Prelude.T2 x21 x22) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_multiGroup(x17)(x3)(x4)(x5)(x6)(x20)(x21)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_multiGroup'46_'35lambda20(x22)(x9)(x4)(x8)))(st))(x1)(st))(st)
c__case_7_case__20 x1 x3 x4 x5 x6 x8 x9 x17 x20 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_7_case__20(x1)(x3)(x4)(x5)(x6)(x8)(x9)(x17)(x20)(x)(st))(i)(xs)(st)
c__case_7_case__20 x1 x3 x4 x5 x6 x8 x9 x17 x20 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_7_case__20")(x)



c__case_8_case__21 x1 x3 x4 x5 x6 x8 x9 x17 x18@(Curry.Module.Prelude.T2 x19 x20) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_7(x3)(x4)(x5)(x6)(x8)(x9)(x17)(x20)(x19)(x1)(st))(st)
c__case_8_case__21 x1 x3 x4 x5 x6 x8 x9 x17 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_8_case__21(x1)(x3)(x4)(x5)(x6)(x8)(x9)(x17)(x)(st))(i)(xs)(st)
c__case_8_case__21 x1 x3 x4 x5 x6 x8 x9 x17 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_8_case__21")(x)



c__case_9_case__22 x1 x3 x4 x5 x6 x8 x9 x17 x19@Curry.Module.Prelude.C_Nothing st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_oneGroup(x17)(x3)(x4)(x5)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_multiGroup'46_'35lambda18(x9)(x6)(x4)(x8)))(st))(x1)(st))(st)
c__case_9_case__22 x1 x3 x4 x5 x6 x8 x9 x17 x19@(Curry.Module.Prelude.C_Just x18) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_8(x3)(x4)(x5)(x6)(x8)(x9)(x17)(x18)(x1)(st))(st)
c__case_9_case__22 x1 x3 x4 x5 x6 x8 x9 x17 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_9_case__22(x1)(x3)(x4)(x5)(x6)(x8)(x9)(x17)(x)(st))(i)(xs)(st)
c__case_9_case__22 x1 x3 x4 x5 x6 x8 x9 x17 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_9_case__22")(x)



c__case_10_case__23 x1 x3 x4 x5 x6 x7 x8 x9 x2@(Curry.Module.OraclePretty.C_Text x10 x11) st = let {x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List)))(let {x12 = Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_length(x1)(st))(x10)(x26)(st)} in let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x27)((Curry.Module.Prelude.:<)(x28)(Curry.Module.Prelude.List))(Curry.Module.OraclePretty.c_pruneMulti(x11)(x3)(Curry.Module.OraclePrelude.op_43(x4)(x12)(x27)(st))(x5)(x6)(x7)(x8)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_multiGroup'46_'35lambda14(x12)(x9)(x10)))(st))(x28)(st))(st))(st)
c__case_10_case__23 x1 x3 x4 x5 x6 x7 x8 x9 x2@(Curry.Module.OraclePretty.C_Line x13 x14) st = let {x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x29)((Curry.Module.Prelude.:<)(x30)(Curry.Module.Prelude.List)))(let {x15 = Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_length(x1)(st))(x13)(x29)(st)} in let {x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x30)((Curry.Module.Prelude.:<)(x31)(Curry.Module.Prelude.List))(Curry.Module.OraclePretty.c_pruneMulti(x14)(x3)(Curry.Module.OraclePrelude.op_43(x4)(x15)(x30)(st))(x5)(x6)(x7)(x8)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_multiGroup'46_'35lambda15(x15)(x9)(x13)(x3)))(st))(x31)(st))(st))(st)
c__case_10_case__23 x1 x3 x4 x5 x6 x7 x8 x9 x2@(Curry.Module.OraclePretty.C_Open x16) st = let {x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x32)(Curry.Module.Prelude.List))(Curry.Module.OraclePretty.c_multiGroup(x16)(x3)(x4)(x5)(x6)(Curry.Module.OracleDequeue.c_cons(Curry.Module.Prelude.T2(x8)(x9))(x7)(x1)(st))(x4)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_multiGroup'46_'35lambda16))(st))(x32)(st))(st)
c__case_10_case__23 x1 x3 x4 x5 x6 x7 x8 x9 x2@(Curry.Module.OraclePretty.C_Close x17) st = let {x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x33)(Curry.Module.Prelude.List))(Curry.Module.OraclePretty.c__case_9(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x17)(Curry.Module.OracleDequeue.c_matchHead(x7)(x1)(st))(x33)(st))(st)
c__case_10_case__23 x1 x3 x4 x5 x6 x7 x8 x9 x2@(Curry.Module.OraclePretty.C_OpenNest x23 x24) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_multiGroup(x24)(x3)(x4)(x5)(x6)(x7)(x8)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_multiGroup'46_'35lambda22(x23)(x9)(x3)))(st))(x1)(st))(st)
c__case_10_case__23 x1 x3 x4 x5 x6 x7 x8 x9 x2@(Curry.Module.OraclePretty.C_CloseNest x25) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_multiGroup(x25)(x3)(x4)(x5)(x6)(x7)(x8)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_multiGroup'46_'35lambda24(x9)))(st))(x1)(st))(st)
c__case_10_case__23 x1 x3 x4 x5 x6 x7 x8 x9 (Curry.Module.OraclePretty.C_TokensOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_10_case__23(x1)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_10_case__23 x1 x3 x4 x5 x6 x7 x8 x9 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_10_case__23")(x)



c__case_5_case__24 x1 x2 x3 x4 x6 x7 x8 x9 x5@Curry.Module.Prelude.C_True st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(x3)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x6)(Curry.Module.OraclePrelude.op_45(x7)(x2)(x1)(st))(x10)(st))(x8)(x11)(st))(x12)(st))(st)
c__case_5_case__24 x1 x2 x3 x4 x6 x7 x8 x9 x5@Curry.Module.Prelude.C_False st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List)))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_replicate(x9)(Curry.Module.Prelude.C_Char(' '))(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x6)(Curry.Module.OraclePrelude.op_45(x4)(x9)(x13)(st))(x14)(st))(x8)(x15)(st))(x16)(st)))(st)
c__case_5_case__24 x1 x2 x3 x4 x6 x7 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_5_case__24(x1)(x2)(x3)(x4)(x6)(x7)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_5_case__24 x1 x2 x3 x4 x6 x7 x8 x9 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_5_case__24")(x)



c__case_6_case__25 x1 x2 x3 x4 x5 x6 x7 x8@((Curry.Module.Prelude.:<) x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_5(x2)(x3)(x4)(x6)(x7)(x8)(x9)(x5)(x1)(st))(st)
c__case_6_case__25 x1 x2 x3 x4 x5 x6 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_6_case__25(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_6_case__25 x1 x2 x3 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_6_case__25")(x)



c__case_4_case__26 x1 x2 x3 x4 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_oneGroup(x2)(x3)(x4)(x5)(x6)(x1)(st))(st)
c__case_4_case__26 x1 x2 x3 x4 x5 x6 x7@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x6)(Curry.Module.Prelude.C_False)(x1)(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_noGroup(x2)(x3)(x4)))(st))(x8)(st))(st)
c__case_4_case__26 x1 x2 x3 x4 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_4_case__26(x1)(x2)(x3)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_4_case__26 x1 x2 x3 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_4_case__26")(x)



c__case_3_case__27 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_multiGroup(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x1)(st))(st)
c__case_3_case__27 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x6)(Curry.Module.Prelude.C_False)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_pruneMulti'46_'35lambda26(x9)(x4)(x7)(x8)(x2)(x3)))))(x11)(st))(st)
c__case_3_case__27 x1 x2 x3 x4 x5 x6 x7 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_3_case__27(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_3_case__27 x1 x2 x3 x4 x5 x6 x7 x8 x9 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_3_case__27")(x)



c__case_0_case__28 x1 x2 x3 x5 x6 x7 x8 x11 x10@(Curry.Module.Prelude.T2 x12 x13) st = let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_pruneMulti(x6)(x7)(x3)(Curry.Module.OraclePrelude.op_43(x12)(x8)(x1)(st))(x13)(x11)(x5)(x2)(x14)(st))(x8)(x15)(st))(st)
c__case_0_case__28 x1 x2 x3 x5 x6 x7 x8 x11 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_0_case__28(x1)(x2)(x3)(x5)(x6)(x7)(x8)(x11)(x)(st))(i)(xs)(st)
c__case_0_case__28 x1 x2 x3 x5 x6 x7 x8 x11 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_0_case__28")(x)



c__case_1_case__29 x1 x2 x3 x5 x6 x7 x8 x9@(Curry.Module.Prelude.T2 x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_0(x2)(x3)(x5)(x6)(x7)(x8)(x11)(x10)(x1)(st))(st)
c__case_1_case__29 x1 x2 x3 x5 x6 x7 x8 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_1_case__29(x1)(x2)(x3)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_1_case__29 x1 x2 x3 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_1_case__29")(x)



c__case_2_case__30 x1 x2 x3 x5 x6 x7 x8 x10@Curry.Module.Prelude.C_Nothing st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_pruneOne(x6)(x7)(x3)(Curry.Module.OraclePrelude.op_43(x5)(x8)(x1)(st))(x2)(x11)(st))(x8)(x12)(st))(st)
c__case_2_case__30 x1 x2 x3 x5 x6 x7 x8 x10@(Curry.Module.Prelude.C_Just x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c__case_1(x2)(x3)(x5)(x6)(x7)(x8)(x9)(x1)(st))(st)
c__case_2_case__30 x1 x2 x3 x5 x6 x7 x8 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePretty.c__case_2_case__30(x1)(x2)(x3)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_2_case__30 x1 x2 x3 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("OraclePretty._case_2_case__30")(x)


