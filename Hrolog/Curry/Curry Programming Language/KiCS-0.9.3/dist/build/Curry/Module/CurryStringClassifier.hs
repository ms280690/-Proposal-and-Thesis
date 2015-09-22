{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.CurryStringClassifier (module Curry.Module.CurryStringClassifier) where

import Curry.RunTimeSystem
import Curry.Module.Char
import Curry.Module.Prelude



-- begin included



-- end included

type C_Tokens = Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token

data C_Token = C_SmallComment (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_BigComment (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_Text (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_Letter (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_Code (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_ModuleHead (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_Meta (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_TokenFail Curry.RunTimeSystem.C_Exceptions
  | C_TokenOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.CurryStringClassifier.C_Token)

instance BaseCurry Curry.Module.CurryStringClassifier.C_Token where
  nf f (Curry.Module.CurryStringClassifier.C_SmallComment x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurryStringClassifier.C_SmallComment(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurryStringClassifier.C_BigComment x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurryStringClassifier.C_BigComment(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurryStringClassifier.C_Text x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurryStringClassifier.C_Text(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurryStringClassifier.C_Letter x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurryStringClassifier.C_Letter(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurryStringClassifier.C_Code x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurryStringClassifier.C_Code(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurryStringClassifier.C_ModuleHead x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurryStringClassifier.C_ModuleHead(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CurryStringClassifier.C_Meta x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CurryStringClassifier.C_Meta(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CurryStringClassifier.C_SmallComment x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurryStringClassifier.C_SmallComment(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurryStringClassifier.C_BigComment x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurryStringClassifier.C_BigComment(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurryStringClassifier.C_Text x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurryStringClassifier.C_Text(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurryStringClassifier.C_Letter x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurryStringClassifier.C_Letter(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurryStringClassifier.C_Code x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurryStringClassifier.C_Code(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurryStringClassifier.C_ModuleHead x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurryStringClassifier.C_ModuleHead(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CurryStringClassifier.C_Meta x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CurryStringClassifier.C_Meta(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CurryStringClassifier.C_TokenOr(Curry.RunTimeSystem.mkRef(r)(1)(i))([Curry.Module.CurryStringClassifier.C_SmallComment(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurryStringClassifier.C_BigComment(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurryStringClassifier.C_Text(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurryStringClassifier.C_Letter(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurryStringClassifier.C_Code(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurryStringClassifier.C_ModuleHead(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CurryStringClassifier.C_Meta(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(1)

  failed  = Curry.Module.CurryStringClassifier.C_TokenFail

  branching  = Curry.Module.CurryStringClassifier.C_TokenOr

  consKind (Curry.Module.CurryStringClassifier.C_TokenOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CurryStringClassifier.C_TokenFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CurryStringClassifier.C_TokenFail x) = x

  orRef (Curry.Module.CurryStringClassifier.C_TokenOr x _) = x

  branches (Curry.Module.CurryStringClassifier.C_TokenOr _ x) = x





instance Curry Curry.Module.CurryStringClassifier.C_Token where
  strEq (Curry.Module.CurryStringClassifier.C_SmallComment x1) (Curry.Module.CurryStringClassifier.C_SmallComment y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurryStringClassifier.C_BigComment x1) (Curry.Module.CurryStringClassifier.C_BigComment y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurryStringClassifier.C_Text x1) (Curry.Module.CurryStringClassifier.C_Text y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurryStringClassifier.C_Letter x1) (Curry.Module.CurryStringClassifier.C_Letter y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurryStringClassifier.C_Code x1) (Curry.Module.CurryStringClassifier.C_Code y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurryStringClassifier.C_ModuleHead x1) (Curry.Module.CurryStringClassifier.C_ModuleHead y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CurryStringClassifier.C_Meta x1) (Curry.Module.CurryStringClassifier.C_Meta y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CurryStringClassifier.C_SmallComment x1) (Curry.Module.CurryStringClassifier.C_SmallComment y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurryStringClassifier.C_BigComment x1) (Curry.Module.CurryStringClassifier.C_BigComment y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurryStringClassifier.C_Text x1) (Curry.Module.CurryStringClassifier.C_Text y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurryStringClassifier.C_Letter x1) (Curry.Module.CurryStringClassifier.C_Letter y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurryStringClassifier.C_Code x1) (Curry.Module.CurryStringClassifier.C_Code y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurryStringClassifier.C_ModuleHead x1) (Curry.Module.CurryStringClassifier.C_ModuleHead y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CurryStringClassifier.C_Meta x1) (Curry.Module.CurryStringClassifier.C_Meta y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CurryStringClassifier.C_SmallComment x1) st = Curry.Module.CurryStringClassifier.C_SmallComment(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurryStringClassifier.C_BigComment x1) st = Curry.Module.CurryStringClassifier.C_BigComment(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurryStringClassifier.C_Text x1) st = Curry.Module.CurryStringClassifier.C_Text(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurryStringClassifier.C_Letter x1) st = Curry.Module.CurryStringClassifier.C_Letter(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurryStringClassifier.C_Code x1) st = Curry.Module.CurryStringClassifier.C_Code(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurryStringClassifier.C_ModuleHead x1) st = Curry.Module.CurryStringClassifier.C_ModuleHead(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CurryStringClassifier.C_Meta x1) st = Curry.Module.CurryStringClassifier.C_Meta(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.CurryStringClassifier.C_SmallComment x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurryStringClassifier.C_BigComment x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurryStringClassifier.C_Text x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurryStringClassifier.C_Letter x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurryStringClassifier.C_Code x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurryStringClassifier.C_ModuleHead x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CurryStringClassifier.C_Meta x1) st = f(x1)(c)(st)

  typeName _ = "Token"

  showQ d (Curry.Module.CurryStringClassifier.C_SmallComment x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurryStringClassifier.SmallComment "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurryStringClassifier.C_BigComment x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurryStringClassifier.BigComment "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurryStringClassifier.C_Text x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurryStringClassifier.Text "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurryStringClassifier.C_Letter x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurryStringClassifier.Letter "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurryStringClassifier.C_Code x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurryStringClassifier.Code "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurryStringClassifier.C_ModuleHead x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurryStringClassifier.ModuleHead "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CurryStringClassifier.C_Meta x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CurryStringClassifier.Meta "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.CurryStringClassifier.C_TokenOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.CurryStringClassifier.C_Token where
  showsPrec d (Curry.Module.CurryStringClassifier.C_SmallComment x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("SmallComment "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurryStringClassifier.C_BigComment x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("BigComment "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurryStringClassifier.C_Text x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Text "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurryStringClassifier.C_Letter x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Letter "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurryStringClassifier.C_Code x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Code "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurryStringClassifier.C_ModuleHead x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ModuleHead "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CurryStringClassifier.C_Meta x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Meta "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.CurryStringClassifier.C_TokenOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Read Curry.Module.CurryStringClassifier.C_Token where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurryStringClassifier.C_SmallComment(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurryStringClassifier")("SmallComment")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurryStringClassifier.C_BigComment(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurryStringClassifier")("BigComment")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurryStringClassifier.C_Text(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurryStringClassifier")("Text")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurryStringClassifier.C_Letter(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurryStringClassifier")("Letter")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurryStringClassifier.C_Code(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurryStringClassifier")("Code")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurryStringClassifier.C_ModuleHead(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurryStringClassifier")("ModuleHead")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CurryStringClassifier.C_Meta(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CurryStringClassifier")("Meta")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r)))))))





c_isSmallComment :: Curry.Module.CurryStringClassifier.C_Token -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isSmallComment x1@(Curry.Module.CurryStringClassifier.C_SmallComment x2) st = Curry.Module.Prelude.C_True
c_isSmallComment x1@(Curry.Module.CurryStringClassifier.C_BigComment x3) st = Curry.Module.Prelude.C_False
c_isSmallComment x1@(Curry.Module.CurryStringClassifier.C_Text x4) st = Curry.Module.Prelude.C_False
c_isSmallComment x1@(Curry.Module.CurryStringClassifier.C_Letter x5) st = Curry.Module.Prelude.C_False
c_isSmallComment x1@(Curry.Module.CurryStringClassifier.C_Code x6) st = Curry.Module.Prelude.C_False
c_isSmallComment x1@(Curry.Module.CurryStringClassifier.C_ModuleHead x7) st = Curry.Module.Prelude.C_False
c_isSmallComment x1@(Curry.Module.CurryStringClassifier.C_Meta x8) st = Curry.Module.Prelude.C_False
c_isSmallComment (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_isSmallComment(x)(st))(i)(xs)(st)
c_isSmallComment x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.isSmallComment")(x)



c_isBigComment :: Curry.Module.CurryStringClassifier.C_Token -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isBigComment x1@(Curry.Module.CurryStringClassifier.C_BigComment x2) st = Curry.Module.Prelude.C_True
c_isBigComment x1@(Curry.Module.CurryStringClassifier.C_SmallComment x3) st = Curry.Module.Prelude.C_False
c_isBigComment x1@(Curry.Module.CurryStringClassifier.C_Text x4) st = Curry.Module.Prelude.C_False
c_isBigComment x1@(Curry.Module.CurryStringClassifier.C_Letter x5) st = Curry.Module.Prelude.C_False
c_isBigComment x1@(Curry.Module.CurryStringClassifier.C_Code x6) st = Curry.Module.Prelude.C_False
c_isBigComment x1@(Curry.Module.CurryStringClassifier.C_ModuleHead x7) st = Curry.Module.Prelude.C_False
c_isBigComment x1@(Curry.Module.CurryStringClassifier.C_Meta x8) st = Curry.Module.Prelude.C_False
c_isBigComment (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_isBigComment(x)(st))(i)(xs)(st)
c_isBigComment x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.isBigComment")(x)



c_isComment :: Curry.Module.CurryStringClassifier.C_Token -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isComment x1 st = Curry.Module.Prelude.op_124_124(Curry.Module.CurryStringClassifier.c_isSmallComment(x1)(st))(Curry.Module.CurryStringClassifier.c_isBigComment(x1)(st))(st)



c_isText :: Curry.Module.CurryStringClassifier.C_Token -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isText x1@(Curry.Module.CurryStringClassifier.C_Text x2) st = Curry.Module.Prelude.C_True
c_isText x1@(Curry.Module.CurryStringClassifier.C_SmallComment x3) st = Curry.Module.Prelude.C_False
c_isText x1@(Curry.Module.CurryStringClassifier.C_BigComment x4) st = Curry.Module.Prelude.C_False
c_isText x1@(Curry.Module.CurryStringClassifier.C_Letter x5) st = Curry.Module.Prelude.C_False
c_isText x1@(Curry.Module.CurryStringClassifier.C_Code x6) st = Curry.Module.Prelude.C_False
c_isText x1@(Curry.Module.CurryStringClassifier.C_ModuleHead x7) st = Curry.Module.Prelude.C_False
c_isText x1@(Curry.Module.CurryStringClassifier.C_Meta x8) st = Curry.Module.Prelude.C_False
c_isText (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_isText(x)(st))(i)(xs)(st)
c_isText x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.isText")(x)



c_isLetter :: Curry.Module.CurryStringClassifier.C_Token -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isLetter x1@(Curry.Module.CurryStringClassifier.C_Letter x2) st = Curry.Module.Prelude.C_True
c_isLetter x1@(Curry.Module.CurryStringClassifier.C_SmallComment x3) st = Curry.Module.Prelude.C_False
c_isLetter x1@(Curry.Module.CurryStringClassifier.C_BigComment x4) st = Curry.Module.Prelude.C_False
c_isLetter x1@(Curry.Module.CurryStringClassifier.C_Text x5) st = Curry.Module.Prelude.C_False
c_isLetter x1@(Curry.Module.CurryStringClassifier.C_Code x6) st = Curry.Module.Prelude.C_False
c_isLetter x1@(Curry.Module.CurryStringClassifier.C_ModuleHead x7) st = Curry.Module.Prelude.C_False
c_isLetter x1@(Curry.Module.CurryStringClassifier.C_Meta x8) st = Curry.Module.Prelude.C_False
c_isLetter (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_isLetter(x)(st))(i)(xs)(st)
c_isLetter x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.isLetter")(x)



c_isCode :: Curry.Module.CurryStringClassifier.C_Token -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isCode x1@(Curry.Module.CurryStringClassifier.C_Code x2) st = Curry.Module.Prelude.C_True
c_isCode x1@(Curry.Module.CurryStringClassifier.C_SmallComment x3) st = Curry.Module.Prelude.C_False
c_isCode x1@(Curry.Module.CurryStringClassifier.C_BigComment x4) st = Curry.Module.Prelude.C_False
c_isCode x1@(Curry.Module.CurryStringClassifier.C_Text x5) st = Curry.Module.Prelude.C_False
c_isCode x1@(Curry.Module.CurryStringClassifier.C_Letter x6) st = Curry.Module.Prelude.C_False
c_isCode x1@(Curry.Module.CurryStringClassifier.C_ModuleHead x7) st = Curry.Module.Prelude.C_False
c_isCode x1@(Curry.Module.CurryStringClassifier.C_Meta x8) st = Curry.Module.Prelude.C_False
c_isCode (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_isCode(x)(st))(i)(xs)(st)
c_isCode x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.isCode")(x)



c_isModuleHead :: Curry.Module.CurryStringClassifier.C_Token -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isModuleHead x1@(Curry.Module.CurryStringClassifier.C_ModuleHead x2) st = Curry.Module.Prelude.C_True
c_isModuleHead x1@(Curry.Module.CurryStringClassifier.C_SmallComment x3) st = Curry.Module.Prelude.C_False
c_isModuleHead x1@(Curry.Module.CurryStringClassifier.C_BigComment x4) st = Curry.Module.Prelude.C_False
c_isModuleHead x1@(Curry.Module.CurryStringClassifier.C_Text x5) st = Curry.Module.Prelude.C_False
c_isModuleHead x1@(Curry.Module.CurryStringClassifier.C_Letter x6) st = Curry.Module.Prelude.C_False
c_isModuleHead x1@(Curry.Module.CurryStringClassifier.C_Code x7) st = Curry.Module.Prelude.C_False
c_isModuleHead x1@(Curry.Module.CurryStringClassifier.C_Meta x8) st = Curry.Module.Prelude.C_False
c_isModuleHead (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_isModuleHead(x)(st))(i)(xs)(st)
c_isModuleHead x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.isModuleHead")(x)



c_isMeta :: Curry.Module.CurryStringClassifier.C_Token -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isMeta x1@(Curry.Module.CurryStringClassifier.C_Meta x2) st = Curry.Module.Prelude.C_True
c_isMeta x1@(Curry.Module.CurryStringClassifier.C_SmallComment x3) st = Curry.Module.Prelude.C_False
c_isMeta x1@(Curry.Module.CurryStringClassifier.C_BigComment x4) st = Curry.Module.Prelude.C_False
c_isMeta x1@(Curry.Module.CurryStringClassifier.C_Text x5) st = Curry.Module.Prelude.C_False
c_isMeta x1@(Curry.Module.CurryStringClassifier.C_Letter x6) st = Curry.Module.Prelude.C_False
c_isMeta x1@(Curry.Module.CurryStringClassifier.C_Code x7) st = Curry.Module.Prelude.C_False
c_isMeta x1@(Curry.Module.CurryStringClassifier.C_ModuleHead x8) st = Curry.Module.Prelude.C_False
c_isMeta (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_isMeta(x)(st))(i)(xs)(st)
c_isMeta x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.isMeta")(x)



c_weaveIntoCode :: (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token)) -> (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_weaveIntoCode x1 x2 st = let {x3 = Curry.Module.CurryStringClassifier.c_unweaveCode(x2)(st)} in Curry.Module.CurryStringClassifier.c_weave(Curry.Module.Prelude.T2(Curry.Module.Prelude.c_apply(x1)(Curry.Module.CurryStringClassifier.c_weaveIntoCode'46_'35selFP3'35cs(x3)(st))(st))(Curry.Module.CurryStringClassifier.c_weaveIntoCode'46_'35selFP4'35ncs(x3)(st)))(st)



c_weaveIntoCode'46_'35selFP3'35cs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_weaveIntoCode'46_'35selFP3'35cs x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_weaveIntoCode'46_'35selFP3'35cs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_weaveIntoCode'46_'35selFP3'35cs(x)(st))(i)(xs)(st)
c_weaveIntoCode'46_'35selFP3'35cs x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.weaveIntoCode._#selFP3#cs")(x)



c_weaveIntoCode'46_'35selFP4'35ncs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_weaveIntoCode'46_'35selFP4'35ncs x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_weaveIntoCode'46_'35selFP4'35ncs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_weaveIntoCode'46_'35selFP4'35ncs(x)(st))(i)(xs)(st)
c_weaveIntoCode'46_'35selFP4'35ncs x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.weaveIntoCode._#selFP4#ncs")(x)



c_unweaveCode :: (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token)
c_unweaveCode x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)
c_unweaveCode x1@((Curry.Module.Prelude.:<) x2 x3) st = let {x4 = Curry.Module.CurryStringClassifier.c_unweaveCode(x3)(st)} in let {x5 = Curry.Module.CurryStringClassifier.c_unweaveCode'46_'35selFP6'35cs(x4)(st)} in let {x6 = Curry.Module.CurryStringClassifier.c_unweaveCode'46_'35selFP7'35ncs(x4)(st)} in Curry.Module.CurryStringClassifier.c_unweaveCode_case_66(x2)(x5)(x6)(Curry.Module.CurryStringClassifier.c_isCode(x2)(st))(st)
c_unweaveCode (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_unweaveCode(x)(st))(i)(xs)(st)
c_unweaveCode x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.unweaveCode")(x)



c_unweaveCode'46_'35selFP6'35cs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_unweaveCode'46_'35selFP6'35cs x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_unweaveCode'46_'35selFP6'35cs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_unweaveCode'46_'35selFP6'35cs(x)(st))(i)(xs)(st)
c_unweaveCode'46_'35selFP6'35cs x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.unweaveCode._#selFP6#cs")(x)



c_unweaveCode'46_'35selFP7'35ncs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_unweaveCode'46_'35selFP7'35ncs x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_unweaveCode'46_'35selFP7'35ncs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_unweaveCode'46_'35selFP7'35ncs(x)(st))(i)(xs)(st)
c_unweaveCode'46_'35selFP7'35ncs x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.unweaveCode._#selFP7#ncs")(x)



c_weave :: (Curry t0) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_weave x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.CurryStringClassifier.c_weave_case_65(x3)(x2)(st)
c_weave (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_weave(x)(st))(i)(xs)(st)
c_weave x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.weave")(x)



c_scan :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_scan x1 st = Curry.RunTimeSystem.freeF(\ x2 -> Curry.Module.CurryStringClassifier.c_modHead(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id))(Curry.Module.CurryStringClassifier.c_stateScan(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(Curry.Module.CurryStringClassifier.C_Code(x2))(x2)(x1)(st))(st))



c_stateScan :: Curry.Module.Prelude.C_Int -> Curry.Module.CurryStringClassifier.C_Token -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_stateScan x1 x2 x3 x4@Curry.Module.Prelude.List st = Curry.Module.Prelude.c_cond(Curry.Module.Prelude.op_61_58_61(x3)(Curry.Module.Prelude.List)(st))((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(st)
c_stateScan x1 x2 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.CurryStringClassifier.c_stateScan_case_59(x1)(x3)(x5)(x6)(x2)(st)
c_stateScan x1 x2 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_stateScan x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan")(x)



c_stateScan'46_'35selFP9'35comment :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_stateScan'46_'35selFP9'35comment x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_stateScan'46_'35selFP9'35comment (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan'46_'35selFP9'35comment(x)(st))(i)(xs)(st)
c_stateScan'46_'35selFP9'35comment x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan._#selFP9#comment")(x)



c_stateScan'46_'35selFP10'35rest :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_stateScan'46_'35selFP10'35rest x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_stateScan'46_'35selFP10'35rest (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan'46_'35selFP10'35rest(x)(st))(i)(xs)(st)
c_stateScan'46_'35selFP10'35rest x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan._#selFP10#rest")(x)



c_modHead :: (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_modHead x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CurryStringClassifier.c_modHead_case_21(x1)(x4)(x3)(st)
c_modHead x1 x2@Curry.Module.Prelude.List st = Curry.Module.CurryStringClassifier.c_maybeMo(Curry.Module.Prelude.c_apply(x1)(Curry.Module.Prelude.List)(st))(Curry.Module.Prelude.List)(st)
c_modHead x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_modHead(x1)(x)(st))(i)(xs)(st)
c_modHead x1 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.modHead")(x)



c_modHead'46_'35lambda11 :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_modHead'46_'35lambda11 x1 st = Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Char('\n'))(st))(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Char('\r'))(st))(st)



c_modHeadInLine :: (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_modHeadInLine x1 x2@Curry.Module.Prelude.List st = Curry.Module.CurryStringClassifier.c_maybeMo(Curry.Module.Prelude.c_apply(x1)(Curry.Module.Prelude.List)(st))(Curry.Module.Prelude.List)(st)
c_modHeadInLine x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CurryStringClassifier.c_modHeadInLine_case_14(x1)(x4)(x3)(st)
c_modHeadInLine x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_modHeadInLine(x1)(x)(st))(i)(xs)(st)
c_modHeadInLine x1 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.modHeadInLine")(x)



c_modHeadInLine'46_'35lambda13 :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_modHeadInLine'46_'35lambda13 x1 st = Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Char('\n'))(st))(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Char('\r'))(st))(st)



c_headers :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_headers st = (Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))(Curry.Module.Prelude.List))))))



c_lineBeginsWith :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_lineBeginsWith x1 x2 st = Curry.Module.CurryStringClassifier.c_lineBeginsWith_case_11(x1)(x2)(Curry.Module.Prelude.op_60(Curry.Module.Prelude.c_length(x1)(st))(Curry.Module.Prelude.c_length(x2)(st))(st))(st)



c_lineBeginsWith'46_'35selFP12'35s'39 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_lineBeginsWith'46_'35selFP12'35s'39 x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_lineBeginsWith'46_'35selFP12'35s'39 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_lineBeginsWith'46_'35selFP12'35s'39(x)(st))(i)(xs)(st)
c_lineBeginsWith'46_'35selFP12'35s'39 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.lineBeginsWith._#selFP12#s'")(x)



c_lineBeginsWith'46_'35selFP13'35rest :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_lineBeginsWith'46_'35selFP13'35rest x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_lineBeginsWith'46_'35selFP13'35rest (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_lineBeginsWith'46_'35selFP13'35rest(x)(st))(i)(xs)(st)
c_lineBeginsWith'46_'35selFP13'35rest x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.lineBeginsWith._#selFP13#rest")(x)



c_isSep :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isSep x1 st = Curry.Module.Prelude.op_124_124(Curry.Module.Char.c_isSpace(x1)(st))(Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(x1)(st))(Curry.Module.CurryStringClassifier.c_infixIDs(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('{'))(Curry.Module.Prelude.List))))(st))(st))(st)



c_infixIDs :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_infixIDs st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('~'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('@'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('#'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('$'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('%'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('^'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('*'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('+'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('?'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('|'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))))))))))))))))))))



c_delimiters :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_delimiters st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('{'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('}'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))))))))



c_toBeEscaped :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_toBeEscaped st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List)))))



c_maybeCode :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_maybeCode x1 x2 st = (Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Code(x1))(x2)



c_maybeMo :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_maybeMo x1 x2 st = Curry.Module.CurryStringClassifier.c_maybeMo_case_9(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.List)(st))(st)



c_plainCode :: (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_plainCode x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.CurryStringClassifier.c_plainCode_case_6(x3)(x2)(st)
c_plainCode x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_plainCode (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_plainCode(x)(st))(i)(xs)(st)
c_plainCode x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.plainCode")(x)



c_unscan :: (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_unscan x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.CurryStringClassifier.c_unscan_case_3(x3)(x2)(st)
c_unscan x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_unscan (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_unscan(x)(st))(i)(xs)(st)
c_unscan x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.unscan")(x)



c_readScan :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token)
c_readScan x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_readFile(x1)(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Prelude.pf(Curry.Module.CurryStringClassifier.c_scan))(st))(st)



c_testScan :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_testScan x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_readFile(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.CurryStringClassifier.c_testScan'46_'35lambda18))(st)



c_testScan'46_'35lambda18 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_testScan'46_'35lambda18 x1 st = Curry.Module.Prelude.c_print(Curry.Module.Prelude.op_61_61(Curry.Module.CurryStringClassifier.c_unscan(Curry.Module.CurryStringClassifier.c_scan(x1)(st))(st))(x1)(st))(st)



c_testWeave :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_testWeave x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_readFile(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.CurryStringClassifier.c_testWeave'46_'35lambda19))(st)



c_testWeave'46_'35lambda19 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_testWeave'46_'35lambda19 x1 st = Curry.Module.Prelude.c_print(Curry.Module.Prelude.op_61_61(Curry.Module.CurryStringClassifier.c_unscan(Curry.Module.CurryStringClassifier.c_weave(Curry.Module.CurryStringClassifier.c_unweaveCode(Curry.Module.CurryStringClassifier.c_scan(x1)(st))(st))(st))(st))(x1)(st))(st)



c_unscan_case_3 x3 x2@(Curry.Module.CurryStringClassifier.C_ModuleHead x4) st = Curry.Module.Prelude.op_43_43(x4)(Curry.Module.CurryStringClassifier.c_unscan_case_2(x3)(st))(st)
c_unscan_case_3 x3 x2@(Curry.Module.CurryStringClassifier.C_Code x16) st = Curry.Module.Prelude.op_43_43(x16)(Curry.Module.CurryStringClassifier.c_unscan(x3)(st))(st)
c_unscan_case_3 x3 x2@(Curry.Module.CurryStringClassifier.C_Text x17) st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.op_43_43(x17)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.CurryStringClassifier.c_unscan(x3)(st)))(st))
c_unscan_case_3 x3 x2@(Curry.Module.CurryStringClassifier.C_Letter x18) st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))(Curry.Module.Prelude.op_43_43(x18)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))(Curry.Module.CurryStringClassifier.c_unscan(x3)(st)))(st))
c_unscan_case_3 x3 x2@(Curry.Module.CurryStringClassifier.C_BigComment x19) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('{'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.op_43_43(x19)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('}'))(Curry.Module.Prelude.List)))(Curry.Module.CurryStringClassifier.c_unscan(x3)(st))(st))(st))(st)
c_unscan_case_3 x3 x2@(Curry.Module.CurryStringClassifier.C_SmallComment x20) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.op_43_43(x20)(Curry.Module.CurryStringClassifier.c_unscan(x3)(st))(st))(st)
c_unscan_case_3 x3 x2@(Curry.Module.CurryStringClassifier.C_Meta x21) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('{'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('+'))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.op_43_43(x21)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('+'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('}'))(Curry.Module.Prelude.List)))(Curry.Module.CurryStringClassifier.c_unscan(x3)(st))(st))(st))(st)
c_unscan_case_3 x3 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_unscan_case_3(x3)(x)(st))(i)(xs)(st)
c_unscan_case_3 x3 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.unscan_case_3")(x)



c_unscan_case_2 x3@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.CurryStringClassifier.c_unscan_case_1(x3)(x6)(x5)(st)
c_unscan_case_2 x3@Curry.Module.Prelude.List st = Curry.Module.CurryStringClassifier.c_unscan(x3)(st)
c_unscan_case_2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_unscan_case_2(x)(st))(i)(xs)(st)
c_unscan_case_2 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.unscan_case_2")(x)



c_unscan_case_1 x3 x6 x5@(Curry.Module.CurryStringClassifier.C_Code x7) st = Curry.Module.CurryStringClassifier.c_unscan_case_0(x3)(x6)(x7)(st)
c_unscan_case_1 x3 x6 x5@(Curry.Module.CurryStringClassifier.C_SmallComment x10) st = Curry.Module.CurryStringClassifier.c_unscan(x3)(st)
c_unscan_case_1 x3 x6 x5@(Curry.Module.CurryStringClassifier.C_BigComment x11) st = Curry.Module.CurryStringClassifier.c_unscan(x3)(st)
c_unscan_case_1 x3 x6 x5@(Curry.Module.CurryStringClassifier.C_Text x12) st = Curry.Module.CurryStringClassifier.c_unscan(x3)(st)
c_unscan_case_1 x3 x6 x5@(Curry.Module.CurryStringClassifier.C_Letter x13) st = Curry.Module.CurryStringClassifier.c_unscan(x3)(st)
c_unscan_case_1 x3 x6 x5@(Curry.Module.CurryStringClassifier.C_ModuleHead x14) st = Curry.Module.CurryStringClassifier.c_unscan(x3)(st)
c_unscan_case_1 x3 x6 x5@(Curry.Module.CurryStringClassifier.C_Meta x15) st = Curry.Module.CurryStringClassifier.c_unscan(x3)(st)
c_unscan_case_1 x3 x6 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_unscan_case_1(x3)(x6)(x)(st))(i)(xs)(st)
c_unscan_case_1 x3 x6 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.unscan_case_1")(x)



c_unscan_case_0 x3 x6 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CurryStringClassifier.c_unscan((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Code(x9))(x6))(st)
c_unscan_case_0 x3 x6 x7@Curry.Module.Prelude.List st = Curry.Module.CurryStringClassifier.c_unscan(x3)(st)
c_unscan_case_0 x3 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_unscan_case_0(x3)(x6)(x)(st))(i)(xs)(st)
c_unscan_case_0 x3 x6 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.unscan_case_0")(x)



c_plainCode_case_6 x3 x2@(Curry.Module.CurryStringClassifier.C_ModuleHead x4) st = Curry.Module.CurryStringClassifier.c_plainCode_case_5(x4)(x3)(st)
c_plainCode_case_6 x3 x2@(Curry.Module.CurryStringClassifier.C_Code x14) st = Curry.Module.Prelude.op_43_43(x14)(Curry.Module.CurryStringClassifier.c_plainCode(x3)(st))(st)
c_plainCode_case_6 x3 x2@(Curry.Module.CurryStringClassifier.C_Text x15) st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.op_43_43(x15)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.CurryStringClassifier.c_plainCode(x3)(st)))(st))
c_plainCode_case_6 x3 x2@(Curry.Module.CurryStringClassifier.C_Letter x16) st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))(Curry.Module.Prelude.op_43_43(x16)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))(Curry.Module.CurryStringClassifier.c_plainCode(x3)(st)))(st))
c_plainCode_case_6 x3 x2@(Curry.Module.CurryStringClassifier.C_BigComment x17) st = Curry.Module.CurryStringClassifier.c_plainCode(x3)(st)
c_plainCode_case_6 x3 x2@(Curry.Module.CurryStringClassifier.C_SmallComment x18) st = Curry.Module.CurryStringClassifier.c_plainCode(x3)(st)
c_plainCode_case_6 x3 x2@(Curry.Module.CurryStringClassifier.C_Meta x19) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('{'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('+'))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.op_43_43(x19)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('+'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('}'))(Curry.Module.Prelude.List)))(Curry.Module.CurryStringClassifier.c_plainCode(x3)(st))(st))(st))(st)
c_plainCode_case_6 x3 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_plainCode_case_6(x3)(x)(st))(i)(xs)(st)
c_plainCode_case_6 x3 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.plainCode_case_6")(x)



c_plainCode_case_5 x4 x3@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.CurryStringClassifier.c_plainCode_case_4(x3)(x4)(x6)(x5)(st)
c_plainCode_case_5 x4 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.op_43_43(x4)(Curry.Module.CurryStringClassifier.c_plainCode(x3)(st))(st)
c_plainCode_case_5 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_plainCode_case_5(x4)(x)(st))(i)(xs)(st)
c_plainCode_case_5 x4 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.plainCode_case_5")(x)



c_plainCode_case_4 x3 x4 x6 x5@(Curry.Module.CurryStringClassifier.C_Code x7) st = Curry.Module.Prelude.op_43_43(x4)(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_drop(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x7)(st))(Curry.Module.CurryStringClassifier.c_plainCode(x6)(st))(st))(st)
c_plainCode_case_4 x3 x4 x6 x5@(Curry.Module.CurryStringClassifier.C_SmallComment x8) st = Curry.Module.Prelude.op_43_43(x4)(Curry.Module.CurryStringClassifier.c_plainCode(x3)(st))(st)
c_plainCode_case_4 x3 x4 x6 x5@(Curry.Module.CurryStringClassifier.C_BigComment x9) st = Curry.Module.Prelude.op_43_43(x4)(Curry.Module.CurryStringClassifier.c_plainCode(x3)(st))(st)
c_plainCode_case_4 x3 x4 x6 x5@(Curry.Module.CurryStringClassifier.C_Text x10) st = Curry.Module.Prelude.op_43_43(x4)(Curry.Module.CurryStringClassifier.c_plainCode(x3)(st))(st)
c_plainCode_case_4 x3 x4 x6 x5@(Curry.Module.CurryStringClassifier.C_Letter x11) st = Curry.Module.Prelude.op_43_43(x4)(Curry.Module.CurryStringClassifier.c_plainCode(x3)(st))(st)
c_plainCode_case_4 x3 x4 x6 x5@(Curry.Module.CurryStringClassifier.C_ModuleHead x12) st = Curry.Module.Prelude.op_43_43(x4)(Curry.Module.CurryStringClassifier.c_plainCode(x3)(st))(st)
c_plainCode_case_4 x3 x4 x6 x5@(Curry.Module.CurryStringClassifier.C_Meta x13) st = Curry.Module.Prelude.op_43_43(x4)(Curry.Module.CurryStringClassifier.c_plainCode(x3)(st))(st)
c_plainCode_case_4 x3 x4 x6 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_plainCode_case_4(x3)(x4)(x6)(x)(st))(i)(xs)(st)
c_plainCode_case_4 x3 x4 x6 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.plainCode_case_4")(x)



c_maybeMo_case_9 x1 x2 x3@Curry.Module.Prelude.C_True st = x2
c_maybeMo_case_9 x1 x2 x3@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_ModuleHead(x1))(Curry.Module.CurryStringClassifier.c_maybeMo_case_8(x2)(st))
c_maybeMo_case_9 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_maybeMo_case_9(x1)(x2)(x)(st))(i)(xs)(st)
c_maybeMo_case_9 x1 x2 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.maybeMo_case_9")(x)



c_maybeMo_case_8 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CurryStringClassifier.c_maybeMo_case_7(x2)(x4)(x3)(st)
c_maybeMo_case_8 x2@Curry.Module.Prelude.List st = x2
c_maybeMo_case_8 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_maybeMo_case_8(x)(st))(i)(xs)(st)
c_maybeMo_case_8 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.maybeMo_case_8")(x)



c_maybeMo_case_7 x2 x4 x3@(Curry.Module.CurryStringClassifier.C_Code x5) st = (Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Code((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(x5)))(x4)
c_maybeMo_case_7 x2 x4 x3@(Curry.Module.CurryStringClassifier.C_SmallComment x6) st = x2
c_maybeMo_case_7 x2 x4 x3@(Curry.Module.CurryStringClassifier.C_BigComment x7) st = x2
c_maybeMo_case_7 x2 x4 x3@(Curry.Module.CurryStringClassifier.C_Text x8) st = x2
c_maybeMo_case_7 x2 x4 x3@(Curry.Module.CurryStringClassifier.C_Letter x9) st = x2
c_maybeMo_case_7 x2 x4 x3@(Curry.Module.CurryStringClassifier.C_ModuleHead x10) st = x2
c_maybeMo_case_7 x2 x4 x3@(Curry.Module.CurryStringClassifier.C_Meta x11) st = x2
c_maybeMo_case_7 x2 x4 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_maybeMo_case_7(x2)(x4)(x)(st))(i)(xs)(st)
c_maybeMo_case_7 x2 x4 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.maybeMo_case_7")(x)



c_lineBeginsWith_case_11 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_False
c_lineBeginsWith_case_11 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CurryStringClassifier.c_lineBeginsWith_case_10(x1)(x2)(Curry.Module.Prelude.c_otherwise(st))(st)
c_lineBeginsWith_case_11 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_lineBeginsWith_case_11(x1)(x2)(x)(st))(i)(xs)(st)
c_lineBeginsWith_case_11 x1 x2 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.lineBeginsWith_case_11")(x)



c_lineBeginsWith_case_10 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_61_61(x1)(x2)(st))(let {x4 = Curry.Module.Prelude.c_splitAt(Curry.Module.Prelude.c_length(x2)(st))(x1)(st)} in let {x6 = Curry.Module.CurryStringClassifier.c_lineBeginsWith'46_'35selFP13'35rest(x4)(st)} in Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.CurryStringClassifier.c_lineBeginsWith'46_'35selFP12'35s'39(x4)(st))(st))(Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.c_null(x6)(st))(Curry.Module.CurryStringClassifier.c_isSep(Curry.Module.Prelude.c_head(x6)(st))(st))(st))(st))(st)
c_lineBeginsWith_case_10 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_lineBeginsWith_case_10(x1)(x2)(x)(st))(i)(xs)(st)
c_lineBeginsWith_case_10 x1 x2 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.lineBeginsWith_case_10")(x)



c_modHeadInLine_case_14 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_Code x5) st = Curry.Module.CurryStringClassifier.c_modHeadInLine_case_13(x1)(x4)(x5)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_break(Curry.Module.Prelude.pf(Curry.Module.CurryStringClassifier.c_modHeadInLine'46_'35lambda13))(st))(x5)(st))(st)
c_modHeadInLine_case_14 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_BigComment x10) st = Curry.Module.CurryStringClassifier.c_maybeMo(Curry.Module.Prelude.c_apply(x1)(Curry.Module.Prelude.List)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_BigComment(x10))(Curry.Module.CurryStringClassifier.c_modHeadInLine(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id))(x4)(st)))(st)
c_modHeadInLine_case_14 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_SmallComment x11) st = Curry.Module.CurryStringClassifier.c_maybeMo(Curry.Module.Prelude.c_apply(x1)(Curry.Module.Prelude.List)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_SmallComment(x11))(Curry.Module.CurryStringClassifier.c_modHeadInLine(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id))(x4)(st)))(st)
c_modHeadInLine_case_14 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_Meta x12) st = Curry.Module.CurryStringClassifier.c_maybeMo(Curry.Module.Prelude.c_apply(x1)(Curry.Module.Prelude.List)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Meta(x12))(x4))(st)
c_modHeadInLine_case_14 x1 x4 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_modHeadInLine_case_14(x1)(x4)(x)(st))(i)(xs)(st)
c_modHeadInLine_case_14 x1 x4 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.modHeadInLine_case_14")(x)



c_modHeadInLine_case_13 x1 x4 x5 (Curry.Module.Prelude.T2 x6 x7) st = Curry.Module.CurryStringClassifier.c_modHeadInLine_case_12(x1)(x4)(x5)(x6)(x7)(st)
c_modHeadInLine_case_13 x1 x4 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_modHeadInLine_case_13(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_modHeadInLine_case_13 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.modHeadInLine_case_13")(x)



c_modHeadInLine_case_12 x1 x4 x5 x6 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CurryStringClassifier.c_modHead(Curry.Module.Prelude.op_46(x1)(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_43_43(x6)))(Curry.Module.Prelude.pc((Curry.Module.Prelude.:<)(x8)))(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Code(x9))(x4))(st)
c_modHeadInLine_case_12 x1 x4 x5 x6 x7@Curry.Module.Prelude.List st = Curry.Module.CurryStringClassifier.c_modHead(x1)((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Code(x5))(x4))(st)
c_modHeadInLine_case_12 x1 x4 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_modHeadInLine_case_12(x1)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_modHeadInLine_case_12 x1 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.modHeadInLine_case_12")(x)



c_modHead_case_21 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_Code x5) st = Curry.Module.CurryStringClassifier.c_modHead_case_20(x1)(x4)(x5)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_break(Curry.Module.Prelude.pf(Curry.Module.CurryStringClassifier.c_modHead'46_'35lambda11))(st))(x5)(st))(st)
c_modHead_case_21 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_BigComment x14) st = Curry.Module.CurryStringClassifier.c_maybeMo(Curry.Module.Prelude.c_apply(x1)(Curry.Module.Prelude.List)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_BigComment(x14))(Curry.Module.CurryStringClassifier.c_modHead(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id))(x4)(st)))(st)
c_modHead_case_21 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_SmallComment x15) st = Curry.Module.CurryStringClassifier.c_maybeMo(Curry.Module.Prelude.c_apply(x1)(Curry.Module.Prelude.List)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_SmallComment(x15))(Curry.Module.CurryStringClassifier.c_modHead(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id))(x4)(st)))(st)
c_modHead_case_21 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_Meta x16) st = Curry.Module.CurryStringClassifier.c_maybeMo(Curry.Module.Prelude.c_apply(x1)(Curry.Module.Prelude.List)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Meta(x16))(x4))(st)
c_modHead_case_21 x1 x4 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_modHead_case_21(x1)(x4)(x)(st))(i)(xs)(st)
c_modHead_case_21 x1 x4 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.modHead_case_21")(x)



c_modHead_case_20 x1 x4 x5 (Curry.Module.Prelude.T2 x6 x7) st = Curry.Module.CurryStringClassifier.c_modHead_case_19(x1)(x4)(x5)(x7)(x6)(st)
c_modHead_case_20 x1 x4 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_modHead_case_20(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_modHead_case_20 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.modHead_case_20")(x)



c_modHead_case_19 x1 x4 x5 x7 x6@Curry.Module.Prelude.List st = Curry.Module.CurryStringClassifier.c_modHead_case_18(x1)(x4)(x7)(st)
c_modHead_case_19 x1 x4 x5 x7 x6@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CurryStringClassifier.c_modHead_case_17(x1)(x4)(x5)(x10)(x11)(x7)(st)
c_modHead_case_19 x1 x4 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_modHead_case_19(x1)(x4)(x5)(x7)(x)(st))(i)(xs)(st)
c_modHead_case_19 x1 x4 x5 x7 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.modHead_case_19")(x)



c_modHead_case_17 x1 x4 x5 x10 x11 x7@Curry.Module.Prelude.List st = Curry.Module.CurryStringClassifier.c_modHead_case_16(x1)(x4)(x5)(x10)(x11)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_any(Curry.Module.Prelude.pf(Curry.Module.CurryStringClassifier.c_lineBeginsWith((Curry.Module.Prelude.:<)(x10)(x11))))(st))(Curry.Module.CurryStringClassifier.c_headers(st))(st))(st)
c_modHead_case_17 x1 x4 x5 x10 x11 x7@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CurryStringClassifier.c_modHead_case_15(x1)(x4)(x5)(x10)(x11)(x12)(x13)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_any(Curry.Module.Prelude.pf(Curry.Module.CurryStringClassifier.c_lineBeginsWith((Curry.Module.Prelude.:<)(x10)(x11))))(st))(Curry.Module.CurryStringClassifier.c_headers(st))(st))(st)
c_modHead_case_17 x1 x4 x5 x10 x11 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_modHead_case_17(x1)(x4)(x5)(x10)(x11)(x)(st))(i)(xs)(st)
c_modHead_case_17 x1 x4 x5 x10 x11 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.modHead_case_17")(x)



c_modHead_case_15 x1 x4 x5 x10 x11 x12 x13 x14@Curry.Module.Prelude.C_True st = Curry.Module.CurryStringClassifier.c_modHead(Curry.Module.Prelude.op_46(x1)(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(x10)(x11))))(Curry.Module.Prelude.pc((Curry.Module.Prelude.:<)(x12)))(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Code(x13))(x4))(st)
c_modHead_case_15 x1 x4 x5 x10 x11 x12 x13 x14@Curry.Module.Prelude.C_False st = Curry.Module.CurryStringClassifier.c_maybeMo(Curry.Module.Prelude.c_apply(x1)(Curry.Module.Prelude.List)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Code(x5))(x4))(st)
c_modHead_case_15 x1 x4 x5 x10 x11 x12 x13 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_modHead_case_15(x1)(x4)(x5)(x10)(x11)(x12)(x13)(x)(st))(i)(xs)(st)
c_modHead_case_15 x1 x4 x5 x10 x11 x12 x13 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.modHead_case_15")(x)



c_modHead_case_16 x1 x4 x5 x10 x11 x12@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_ModuleHead(Curry.Module.Prelude.c_apply(x1)((Curry.Module.Prelude.:<)(x10)(x11))(st)))(Curry.Module.CurryStringClassifier.c_modHeadInLine(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id))(x4)(st))
c_modHead_case_16 x1 x4 x5 x10 x11 x12@Curry.Module.Prelude.C_False st = Curry.Module.CurryStringClassifier.c_maybeMo(Curry.Module.Prelude.c_apply(x1)(Curry.Module.Prelude.List)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Code(x5))(x4))(st)
c_modHead_case_16 x1 x4 x5 x10 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_modHead_case_16(x1)(x4)(x5)(x10)(x11)(x)(st))(i)(xs)(st)
c_modHead_case_16 x1 x4 x5 x10 x11 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.modHead_case_16")(x)



c_modHead_case_18 x1 x4 x7@Curry.Module.Prelude.List st = Curry.Module.CurryStringClassifier.c_modHead(x1)(x4)(st)
c_modHead_case_18 x1 x4 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CurryStringClassifier.c_modHead(Curry.Module.Prelude.op_46(x1)(Curry.Module.Prelude.pc((Curry.Module.Prelude.:<)(x8)))(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Code(x9))(x4))(st)
c_modHead_case_18 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_modHead_case_18(x1)(x4)(x)(st))(i)(xs)(st)
c_modHead_case_18 x1 x4 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.modHead_case_18")(x)



c_stateScan_case_59 x1 x3 x5 x6 x2@(Curry.Module.CurryStringClassifier.C_Code x7) st = Curry.Module.CurryStringClassifier.c_stateScan_case_58(x1)(x3)(x5)(x7)(x6)(st)
c_stateScan_case_59 x1 x3 x5 x6 x2@(Curry.Module.CurryStringClassifier.C_Text x26) st = Curry.Module.CurryStringClassifier.c_stateScan_case_35(x1)(x3)(x5)(x26)(x6)(st)
c_stateScan_case_59 x1 x3 x5 x6 x2@(Curry.Module.CurryStringClassifier.C_BigComment x30) st = Curry.Module.CurryStringClassifier.c_stateScan_case_29(x1)(x3)(x5)(x30)(x6)(st)
c_stateScan_case_59 x1 x3 x5 x6 x2@(Curry.Module.CurryStringClassifier.C_Meta x34) st = Curry.Module.CurryStringClassifier.c_stateScan_case_25(x1)(x3)(x5)(x34)(x6)(st)
c_stateScan_case_59 x1 x3 x5 x6 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_59(x1)(x3)(x5)(x6)(x)(st))(i)(xs)(st)
c_stateScan_case_59 x1 x3 x5 x6 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_59")(x)



c_stateScan_case_25 x1 x3 x5 x34 x6@Curry.Module.Prelude.List st = Curry.Module.Prelude.c_cond(Curry.Module.Prelude.op_61_58_61(x3)(Curry.Module.Prelude.List)(st))(Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('+'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('}'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))(st))(st)
c_stateScan_case_25 x1 x3 x5 x34 x6@((Curry.Module.Prelude.:<) x35 x36) st = Curry.RunTimeSystem.freeF(\ x37 -> Curry.Module.CurryStringClassifier.c_stateScan_case_24(x1)(x3)(x5)(x34)(x35)(x36)(x37)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x5)(Curry.Module.Prelude.C_Char('+'))(st))(Curry.Module.Prelude.op_61_61(x35)(Curry.Module.Prelude.C_Char('}'))(st))(st))(st))
c_stateScan_case_25 x1 x3 x5 x34 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_25(x1)(x3)(x5)(x34)(x)(st))(i)(xs)(st)
c_stateScan_case_25 x1 x3 x5 x34 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_25")(x)



c_stateScan_case_24 x1 x3 x5 x34 x35 x36 x37 x38@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_38_62(Curry.Module.Prelude.op_61_58_61(x3)(Curry.Module.Prelude.List)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Meta(x34))(Curry.Module.CurryStringClassifier.c_stateScan(x1)(Curry.Module.CurryStringClassifier.C_Code(x37))(x37)(x36)(st)))(st)
c_stateScan_case_24 x1 x3 x5 x34 x35 x36 x37 x38@Curry.Module.Prelude.C_False st = Curry.Module.CurryStringClassifier.c_stateScan_case_23(x1)(x3)(x5)(x34)(x35)(x36)(x37)(Curry.Module.Prelude.op_61_61(x5)(Curry.Module.Prelude.C_Char('\n'))(st))(st)
c_stateScan_case_24 x1 x3 x5 x34 x35 x36 x37 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_24(x1)(x3)(x5)(x34)(x35)(x36)(x37)(x)(st))(i)(xs)(st)
c_stateScan_case_24 x1 x3 x5 x34 x35 x36 x37 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_24")(x)



c_stateScan_case_23 x1 x3 x5 x34 x35 x36 x37 x38@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_38_62(Curry.Module.Prelude.op_61_58_61(x3)((Curry.Module.Prelude.:<)(x5)(x37))(st))(Curry.Module.CurryStringClassifier.c_stateScan(Curry.Module.Prelude.op_43(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.CurryStringClassifier.C_Meta(x34))(x37)((Curry.Module.Prelude.:<)(x35)(x36))(st))(st)
c_stateScan_case_23 x1 x3 x5 x34 x35 x36 x37 x38@Curry.Module.Prelude.C_False st = Curry.Module.CurryStringClassifier.c_stateScan_case_22(x1)(x3)(x5)(x34)(x35)(x36)(x37)(Curry.Module.Prelude.c_otherwise(st))(st)
c_stateScan_case_23 x1 x3 x5 x34 x35 x36 x37 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_23(x1)(x3)(x5)(x34)(x35)(x36)(x37)(x)(st))(i)(xs)(st)
c_stateScan_case_23 x1 x3 x5 x34 x35 x36 x37 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_23")(x)



c_stateScan_case_22 x1 x3 x5 x34 x35 x36 x37 x38@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_38_62(Curry.Module.Prelude.op_61_58_61(x3)((Curry.Module.Prelude.:<)(x5)(x37))(st))(Curry.Module.CurryStringClassifier.c_stateScan(x1)(Curry.Module.CurryStringClassifier.C_Meta(x34))(x37)((Curry.Module.Prelude.:<)(x35)(x36))(st))(st)
c_stateScan_case_22 x1 x3 x5 x34 x35 x36 x37 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_22(x1)(x3)(x5)(x34)(x35)(x36)(x37)(x)(st))(i)(xs)(st)
c_stateScan_case_22 x1 x3 x5 x34 x35 x36 x37 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_22")(x)



c_stateScan_case_29 x1 x3 x5 x30 x6@Curry.Module.Prelude.List st = Curry.Module.Prelude.c_cond(Curry.Module.Prelude.op_61_58_61(x3)(Curry.Module.Prelude.List)(st))(Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('}'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))(st))(st)
c_stateScan_case_29 x1 x3 x5 x30 x6@((Curry.Module.Prelude.:<) x31 x32) st = Curry.RunTimeSystem.freeF(\ x33 -> Curry.Module.CurryStringClassifier.c_stateScan_case_28(x1)(x3)(x5)(x30)(x31)(x32)(x33)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x5)(Curry.Module.Prelude.C_Char('-'))(st))(Curry.Module.Prelude.op_61_61(x31)(Curry.Module.Prelude.C_Char('}'))(st))(st))(st))
c_stateScan_case_29 x1 x3 x5 x30 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_29(x1)(x3)(x5)(x30)(x)(st))(i)(xs)(st)
c_stateScan_case_29 x1 x3 x5 x30 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_29")(x)



c_stateScan_case_28 x1 x3 x5 x30 x31 x32 x33 x34@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_38_62(Curry.Module.Prelude.op_61_58_61(x3)(Curry.Module.Prelude.List)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_BigComment(x30))(Curry.Module.CurryStringClassifier.c_stateScan(x1)(Curry.Module.CurryStringClassifier.C_Code(x33))(x33)(x32)(st)))(st)
c_stateScan_case_28 x1 x3 x5 x30 x31 x32 x33 x34@Curry.Module.Prelude.C_False st = Curry.Module.CurryStringClassifier.c_stateScan_case_27(x1)(x3)(x5)(x30)(x31)(x32)(x33)(Curry.Module.Prelude.op_61_61(x5)(Curry.Module.Prelude.C_Char('\n'))(st))(st)
c_stateScan_case_28 x1 x3 x5 x30 x31 x32 x33 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_28(x1)(x3)(x5)(x30)(x31)(x32)(x33)(x)(st))(i)(xs)(st)
c_stateScan_case_28 x1 x3 x5 x30 x31 x32 x33 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_28")(x)



c_stateScan_case_27 x1 x3 x5 x30 x31 x32 x33 x34@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_38_62(Curry.Module.Prelude.op_61_58_61(x3)((Curry.Module.Prelude.:<)(x5)(x33))(st))(Curry.Module.CurryStringClassifier.c_stateScan(Curry.Module.Prelude.op_43(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.CurryStringClassifier.C_BigComment(x30))(x33)((Curry.Module.Prelude.:<)(x31)(x32))(st))(st)
c_stateScan_case_27 x1 x3 x5 x30 x31 x32 x33 x34@Curry.Module.Prelude.C_False st = Curry.Module.CurryStringClassifier.c_stateScan_case_26(x1)(x3)(x5)(x30)(x31)(x32)(x33)(Curry.Module.Prelude.c_otherwise(st))(st)
c_stateScan_case_27 x1 x3 x5 x30 x31 x32 x33 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_27(x1)(x3)(x5)(x30)(x31)(x32)(x33)(x)(st))(i)(xs)(st)
c_stateScan_case_27 x1 x3 x5 x30 x31 x32 x33 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_27")(x)



c_stateScan_case_26 x1 x3 x5 x30 x31 x32 x33 x34@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_38_62(Curry.Module.Prelude.op_61_58_61(x3)((Curry.Module.Prelude.:<)(x5)(x33))(st))(Curry.Module.CurryStringClassifier.c_stateScan(x1)(Curry.Module.CurryStringClassifier.C_BigComment(x30))(x33)((Curry.Module.Prelude.:<)(x31)(x32))(st))(st)
c_stateScan_case_26 x1 x3 x5 x30 x31 x32 x33 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_26(x1)(x3)(x5)(x30)(x31)(x32)(x33)(x)(st))(i)(xs)(st)
c_stateScan_case_26 x1 x3 x5 x30 x31 x32 x33 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_26")(x)



c_stateScan_case_35 x1 x3 x5 x26 x6@Curry.Module.Prelude.List st = Curry.Module.Prelude.c_cond(Curry.Module.Prelude.op_61_58_61(x3)(Curry.Module.Prelude.List)(st))(Curry.Module.CurryStringClassifier.c_stateScan_case_34(x5)(x26)(Curry.Module.Prelude.op_61_61(x5)(Curry.Module.Prelude.C_Char('\"'))(st))(st))(st)
c_stateScan_case_35 x1 x3 x5 x26 x6@((Curry.Module.Prelude.:<) x27 x28) st = Curry.RunTimeSystem.freeF(\ x29 -> Curry.Module.CurryStringClassifier.c_stateScan_case_33(x1)(x3)(x5)(x26)(x27)(x28)(x29)(Curry.Module.Prelude.op_61_61(x5)(Curry.Module.Prelude.C_Char('\"'))(st))(st))
c_stateScan_case_35 x1 x3 x5 x26 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_35(x1)(x3)(x5)(x26)(x)(st))(i)(xs)(st)
c_stateScan_case_35 x1 x3 x5 x26 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_35")(x)



c_stateScan_case_33 x1 x3 x5 x26 x27 x28 x29 x30@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_38_62(Curry.Module.Prelude.op_61_58_61(x3)(Curry.Module.Prelude.List)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Text(x26))(Curry.Module.CurryStringClassifier.c_stateScan(x1)(Curry.Module.CurryStringClassifier.C_Code(x29))(x29)((Curry.Module.Prelude.:<)(x27)(x28))(st)))(st)
c_stateScan_case_33 x1 x3 x5 x26 x27 x28 x29 x30@Curry.Module.Prelude.C_False st = Curry.Module.CurryStringClassifier.c_stateScan_case_32(x1)(x3)(x5)(x26)(x27)(x28)(x29)(Curry.Module.Prelude.op_61_61(x5)(Curry.Module.Prelude.C_Char('\\'))(st))(st)
c_stateScan_case_33 x1 x3 x5 x26 x27 x28 x29 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_33(x1)(x3)(x5)(x26)(x27)(x28)(x29)(x)(st))(i)(xs)(st)
c_stateScan_case_33 x1 x3 x5 x26 x27 x28 x29 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_33")(x)



c_stateScan_case_32 x1 x3 x5 x26 x27 x28 x29 x30@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_38_62(Curry.Module.Prelude.op_61_58_61(x3)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x27)(x29)))(st))(Curry.Module.CurryStringClassifier.c_stateScan(x1)(Curry.Module.CurryStringClassifier.C_Text(x26))(x29)(x28)(st))(st)
c_stateScan_case_32 x1 x3 x5 x26 x27 x28 x29 x30@Curry.Module.Prelude.C_False st = Curry.Module.CurryStringClassifier.c_stateScan_case_31(x1)(x3)(x5)(x26)(x27)(x28)(x29)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(x5)(st))(Curry.Module.CurryStringClassifier.c_toBeEscaped(st))(st))(st)
c_stateScan_case_32 x1 x3 x5 x26 x27 x28 x29 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_32(x1)(x3)(x5)(x26)(x27)(x28)(x29)(x)(st))(i)(xs)(st)
c_stateScan_case_32 x1 x3 x5 x26 x27 x28 x29 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_32")(x)



c_stateScan_case_31 x1 x3 x5 x26 x27 x28 x29 x30@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_error))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))))))))))))))(Curry.Module.Prelude.c_show(x1)(st))(st)))(st))(st)
c_stateScan_case_31 x1 x3 x5 x26 x27 x28 x29 x30@Curry.Module.Prelude.C_False st = Curry.Module.CurryStringClassifier.c_stateScan_case_30(x1)(x3)(x5)(x26)(x27)(x28)(x29)(Curry.Module.Prelude.c_otherwise(st))(st)
c_stateScan_case_31 x1 x3 x5 x26 x27 x28 x29 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_31(x1)(x3)(x5)(x26)(x27)(x28)(x29)(x)(st))(i)(xs)(st)
c_stateScan_case_31 x1 x3 x5 x26 x27 x28 x29 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_31")(x)



c_stateScan_case_30 x1 x3 x5 x26 x27 x28 x29 x30@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_38_62(Curry.Module.Prelude.op_61_58_61(x3)((Curry.Module.Prelude.:<)(x5)(x29))(st))(Curry.Module.CurryStringClassifier.c_stateScan(x1)(Curry.Module.CurryStringClassifier.C_Text(x26))(x29)((Curry.Module.Prelude.:<)(x27)(x28))(st))(st)
c_stateScan_case_30 x1 x3 x5 x26 x27 x28 x29 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_30(x1)(x3)(x5)(x26)(x27)(x28)(x29)(x)(st))(i)(xs)(st)
c_stateScan_case_30 x1 x3 x5 x26 x27 x28 x29 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_30")(x)



c_stateScan_case_34 x5 x26 x27@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Text(x26))(Curry.Module.Prelude.List)
c_stateScan_case_34 x5 x26 x27@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))(st)
c_stateScan_case_34 x5 x26 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_34(x5)(x26)(x)(st))(i)(xs)(st)
c_stateScan_case_34 x5 x26 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_34")(x)



c_stateScan_case_58 x1 x3 x5 x7 x6@Curry.Module.Prelude.List st = Curry.Module.Prelude.c_cond(Curry.Module.Prelude.op_61_58_61(x3)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(st))(Curry.Module.CurryStringClassifier.c_maybeCode(x7)(Curry.Module.Prelude.List)(st))(st)
c_stateScan_case_58 x1 x3 x5 x7 x6@((Curry.Module.Prelude.:<) x8 x9) st = Curry.RunTimeSystem.freeF(\ x10 -> Curry.Module.CurryStringClassifier.c_stateScan_case_57(x1)(x3)(x5)(x7)(x8)(x9)(x10)(Curry.Module.Prelude.op_61_61(x5)(Curry.Module.Prelude.C_Char('\"'))(st))(st))
c_stateScan_case_58 x1 x3 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_58(x1)(x3)(x5)(x7)(x)(st))(i)(xs)(st)
c_stateScan_case_58 x1 x3 x5 x7 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_58")(x)



c_stateScan_case_57 x1 x3 x5 x7 x8 x9 x10 x11@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_38_62(Curry.Module.Prelude.op_61_58_61(x3)(Curry.Module.Prelude.List)(st))(Curry.Module.CurryStringClassifier.c_maybeCode(x7)(Curry.Module.CurryStringClassifier.c_stateScan(x1)(Curry.Module.CurryStringClassifier.C_Text(x10))(x10)((Curry.Module.Prelude.:<)(x8)(x9))(st))(st))(st)
c_stateScan_case_57 x1 x3 x5 x7 x8 x9 x10 x11@Curry.Module.Prelude.C_False st = Curry.Module.CurryStringClassifier.c_stateScan_case_56(x1)(x3)(x5)(x7)(x8)(x9)(x10)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x5)(Curry.Module.Prelude.C_Char('-'))(st))(Curry.Module.Prelude.op_61_61(x8)(Curry.Module.Prelude.C_Char('-'))(st))(st))(st)
c_stateScan_case_57 x1 x3 x5 x7 x8 x9 x10 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_57(x1)(x3)(x5)(x7)(x8)(x9)(x10)(x)(st))(i)(xs)(st)
c_stateScan_case_57 x1 x3 x5 x7 x8 x9 x10 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_57")(x)



c_stateScan_case_56 x1 x3 x5 x7 x8 x9 x10 x11@Curry.Module.Prelude.C_True st = let {x11 = Curry.Module.Prelude.c_span(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_47_61))(Curry.Module.Prelude.C_Char('\n'))))(x9)(st)} in Curry.Module.Prelude.op_38_62(Curry.Module.Prelude.op_61_58_61(x3)(Curry.Module.Prelude.List)(st))(Curry.Module.CurryStringClassifier.c_maybeCode(x7)((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_SmallComment(Curry.Module.CurryStringClassifier.c_stateScan'46_'35selFP9'35comment(x11)(st)))(Curry.Module.CurryStringClassifier.c_stateScan(x1)(Curry.Module.CurryStringClassifier.C_Code(x10))(x10)(Curry.Module.CurryStringClassifier.c_stateScan'46_'35selFP10'35rest(x11)(st))(st)))(st))(st)
c_stateScan_case_56 x1 x3 x5 x7 x8 x9 x10 x11@Curry.Module.Prelude.C_False st = Curry.Module.CurryStringClassifier.c_stateScan_case_55(x1)(x3)(x5)(x7)(x8)(x9)(x10)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x5)(Curry.Module.Prelude.C_Char('{'))(st))(Curry.Module.Prelude.op_61_61(x8)(Curry.Module.Prelude.C_Char('-'))(st))(st))(st)
c_stateScan_case_56 x1 x3 x5 x7 x8 x9 x10 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_56(x1)(x3)(x5)(x7)(x8)(x9)(x10)(x)(st))(i)(xs)(st)
c_stateScan_case_56 x1 x3 x5 x7 x8 x9 x10 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_56")(x)



c_stateScan_case_55 x1 x3 x5 x7 x8 x9 x10 x11@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_38_62(Curry.Module.Prelude.op_61_58_61(x3)(Curry.Module.Prelude.List)(st))(Curry.Module.CurryStringClassifier.c_maybeCode(x7)(Curry.Module.CurryStringClassifier.c_stateScan(x1)(Curry.Module.CurryStringClassifier.C_BigComment(x10))(x10)(x9)(st))(st))(st)
c_stateScan_case_55 x1 x3 x5 x7 x8 x9 x10 x11@Curry.Module.Prelude.C_False st = Curry.Module.CurryStringClassifier.c_stateScan_case_54(x1)(x3)(x5)(x7)(x8)(x9)(x10)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x5)(Curry.Module.Prelude.C_Char('{'))(st))(Curry.Module.Prelude.op_61_61(x8)(Curry.Module.Prelude.C_Char('+'))(st))(st))(st)
c_stateScan_case_55 x1 x3 x5 x7 x8 x9 x10 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_55(x1)(x3)(x5)(x7)(x8)(x9)(x10)(x)(st))(i)(xs)(st)
c_stateScan_case_55 x1 x3 x5 x7 x8 x9 x10 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_55")(x)



c_stateScan_case_54 x1 x3 x5 x7 x8 x9 x10 x11@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_38_62(Curry.Module.Prelude.op_61_58_61(x3)(Curry.Module.Prelude.List)(st))(Curry.Module.CurryStringClassifier.c_maybeCode(x7)(Curry.Module.CurryStringClassifier.c_stateScan(x1)(Curry.Module.CurryStringClassifier.C_Meta(x10))(x10)(x9)(st))(st))(st)
c_stateScan_case_54 x1 x3 x5 x7 x8 x9 x10 x11@Curry.Module.Prelude.C_False st = Curry.Module.CurryStringClassifier.c_stateScan_case_53(x1)(x3)(x5)(x7)(x8)(x9)(x10)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x8)(Curry.Module.Prelude.C_Char('\''))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(x5)(st))(Curry.Module.Prelude.op_43_43(Curry.Module.CurryStringClassifier.c_infixIDs(st))(Curry.Module.CurryStringClassifier.c_delimiters(st))(st))(st))(st))(st)
c_stateScan_case_54 x1 x3 x5 x7 x8 x9 x10 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_54(x1)(x3)(x5)(x7)(x8)(x9)(x10)(x)(st))(i)(xs)(st)
c_stateScan_case_54 x1 x3 x5 x7 x8 x9 x10 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_54")(x)



c_stateScan_case_53 x1 x3 x5 x7 x8 x9 x10 x11@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_38_62(Curry.Module.Prelude.op_61_58_61(x3)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(st))(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.CurryStringClassifier.c_maybeCode(x7)))(Curry.Module.CurryStringClassifier.c_stateScan_case_52(x1)(x10)(x9)(st))(st))(st)
c_stateScan_case_53 x1 x3 x5 x7 x8 x9 x10 x11@Curry.Module.Prelude.C_False st = Curry.Module.CurryStringClassifier.c_stateScan_case_37(x1)(x3)(x5)(x7)(x8)(x9)(x10)(Curry.Module.Prelude.op_61_61(x5)(Curry.Module.Prelude.C_Char('\n'))(st))(st)
c_stateScan_case_53 x1 x3 x5 x7 x8 x9 x10 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_53(x1)(x3)(x5)(x7)(x8)(x9)(x10)(x)(st))(i)(xs)(st)
c_stateScan_case_53 x1 x3 x5 x7 x8 x9 x10 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_53")(x)



c_stateScan_case_37 x1 x3 x5 x7 x8 x9 x10 x11@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_38_62(Curry.Module.Prelude.op_61_58_61(x3)((Curry.Module.Prelude.:<)(x5)(x10))(st))(Curry.Module.CurryStringClassifier.c_stateScan(Curry.Module.Prelude.op_43(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.CurryStringClassifier.C_Code(x7))(x10)((Curry.Module.Prelude.:<)(x8)(x9))(st))(st)
c_stateScan_case_37 x1 x3 x5 x7 x8 x9 x10 x11@Curry.Module.Prelude.C_False st = Curry.Module.CurryStringClassifier.c_stateScan_case_36(x1)(x3)(x5)(x7)(x8)(x9)(x10)(Curry.Module.Prelude.c_otherwise(st))(st)
c_stateScan_case_37 x1 x3 x5 x7 x8 x9 x10 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_37(x1)(x3)(x5)(x7)(x8)(x9)(x10)(x)(st))(i)(xs)(st)
c_stateScan_case_37 x1 x3 x5 x7 x8 x9 x10 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_37")(x)



c_stateScan_case_36 x1 x3 x5 x7 x8 x9 x10 x11@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_38_62(Curry.Module.Prelude.op_61_58_61(x3)((Curry.Module.Prelude.:<)(x5)(x10))(st))(Curry.Module.CurryStringClassifier.c_stateScan(x1)(Curry.Module.CurryStringClassifier.C_Code(x7))(x10)((Curry.Module.Prelude.:<)(x8)(x9))(st))(st)
c_stateScan_case_36 x1 x3 x5 x7 x8 x9 x10 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_36(x1)(x3)(x5)(x7)(x8)(x9)(x10)(x)(st))(i)(xs)(st)
c_stateScan_case_36 x1 x3 x5 x7 x8 x9 x10 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_36")(x)



c_stateScan_case_52 x1 x10 x9@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.CurryStringClassifier.c_stateScan_case_51(x1)(x10)(x14)(x15)(Curry.Module.Prelude.op_61_61(x14)(Curry.Module.Prelude.C_Char('\\'))(st))(st)
c_stateScan_case_52 x1 x10 x9@Curry.Module.Prelude.List st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_error))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(Curry.Module.Prelude.c_show(x1)(st))(st))(st)
c_stateScan_case_52 x1 x10 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_52(x1)(x10)(x)(st))(i)(xs)(st)
c_stateScan_case_52 x1 x10 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_52")(x)



c_stateScan_case_51 x1 x10 x14 x15 x16@Curry.Module.Prelude.C_True st = Curry.Module.CurryStringClassifier.c_stateScan_case_50(x1)(x10)(x14)(x15)(st)
c_stateScan_case_51 x1 x10 x14 x15 x16@Curry.Module.Prelude.C_False st = Curry.Module.CurryStringClassifier.c_stateScan_case_39(x1)(x10)(x14)(x15)(st)
c_stateScan_case_51 x1 x10 x14 x15 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_51(x1)(x10)(x14)(x15)(x)(st))(i)(xs)(st)
c_stateScan_case_51 x1 x10 x14 x15 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_51")(x)



c_stateScan_case_39 x1 x10 x14 x15@((Curry.Module.Prelude.:<) x24 x25) st = Curry.Module.CurryStringClassifier.c_stateScan_case_38(x1)(x10)(x14)(x24)(x25)(Curry.Module.Prelude.op_61_61(x24)(Curry.Module.Prelude.C_Char('\''))(st))(st)
c_stateScan_case_39 x1 x10 x14 x15@Curry.Module.Prelude.List st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_error))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(Curry.Module.Prelude.c_show(x1)(st))(st))(st)
c_stateScan_case_39 x1 x10 x14 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_39(x1)(x10)(x14)(x)(st))(i)(xs)(st)
c_stateScan_case_39 x1 x10 x14 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_39")(x)



c_stateScan_case_38 x1 x10 x14 x24 x25 x26@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Letter((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))(Curry.Module.CurryStringClassifier.c_stateScan(x1)(Curry.Module.CurryStringClassifier.C_Code(x10))(x10)(x25)(st))
c_stateScan_case_38 x1 x10 x14 x24 x25 x26@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_error))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(Curry.Module.Prelude.c_show(x1)(st))(st))(st)
c_stateScan_case_38 x1 x10 x14 x24 x25 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_38(x1)(x10)(x14)(x24)(x25)(x)(st))(i)(xs)(st)
c_stateScan_case_38 x1 x10 x14 x24 x25 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_38")(x)



c_stateScan_case_50 x1 x10 x14 x15@((Curry.Module.Prelude.:<) x16 x17) st = Curry.Module.CurryStringClassifier.c_stateScan_case_49(x1)(x10)(x14)(x16)(x17)(st)
c_stateScan_case_50 x1 x10 x14 x15@Curry.Module.Prelude.List st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_error))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(Curry.Module.Prelude.c_show(x1)(st))(st))(st)
c_stateScan_case_50 x1 x10 x14 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_50(x1)(x10)(x14)(x)(st))(i)(xs)(st)
c_stateScan_case_50 x1 x10 x14 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_50")(x)



c_stateScan_case_49 x1 x10 x14 x16 x17@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.CurryStringClassifier.c_stateScan_case_48(x1)(x10)(x14)(x16)(x17)(x18)(x19)(Curry.Module.Prelude.op_61_61(x18)(Curry.Module.Prelude.C_Char('\''))(st))(st)
c_stateScan_case_49 x1 x10 x14 x16 x17@Curry.Module.Prelude.List st = Curry.Module.CurryStringClassifier.c_stateScan_case_40(x1)(x10)(x14)(x16)(Curry.Module.Prelude.op_61_61(x16)(Curry.Module.Prelude.C_Char('\''))(st))(st)
c_stateScan_case_49 x1 x10 x14 x16 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_49(x1)(x10)(x14)(x16)(x)(st))(i)(xs)(st)
c_stateScan_case_49 x1 x10 x14 x16 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_49")(x)



c_stateScan_case_40 x1 x10 x14 x16 x17@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Letter((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))(Curry.Module.CurryStringClassifier.c_stateScan(x1)(Curry.Module.CurryStringClassifier.C_Code(x10))(x10)(Curry.Module.Prelude.List)(st))
c_stateScan_case_40 x1 x10 x14 x16 x17@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_error))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(Curry.Module.Prelude.c_show(x1)(st))(st))(st)
c_stateScan_case_40 x1 x10 x14 x16 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_40(x1)(x10)(x14)(x16)(x)(st))(i)(xs)(st)
c_stateScan_case_40 x1 x10 x14 x16 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_40")(x)



c_stateScan_case_48 x1 x10 x14 x16 x17 x18 x19 x20@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Letter((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))))(Curry.Module.CurryStringClassifier.c_stateScan(x1)(Curry.Module.CurryStringClassifier.C_Code(x10))(x10)(x19)(st))
c_stateScan_case_48 x1 x10 x14 x16 x17 x18 x19 x20@Curry.Module.Prelude.C_False st = Curry.Module.CurryStringClassifier.c_stateScan_case_47(x1)(x10)(x14)(x16)(x17)(x18)(x19)(st)
c_stateScan_case_48 x1 x10 x14 x16 x17 x18 x19 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_48(x1)(x10)(x14)(x16)(x17)(x18)(x19)(x)(st))(i)(xs)(st)
c_stateScan_case_48 x1 x10 x14 x16 x17 x18 x19 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_48")(x)



c_stateScan_case_47 x1 x10 x14 x16 x17 x18 x19@((Curry.Module.Prelude.:<) x20 x21) st = Curry.Module.CurryStringClassifier.c_stateScan_case_46(x1)(x10)(x14)(x16)(x17)(x18)(x20)(x21)(st)
c_stateScan_case_47 x1 x10 x14 x16 x17 x18 x19@Curry.Module.Prelude.List st = Curry.Module.CurryStringClassifier.c_stateScan_case_41(x1)(x10)(x14)(x16)(x17)(Curry.Module.Prelude.op_61_61(x16)(Curry.Module.Prelude.C_Char('\''))(st))(st)
c_stateScan_case_47 x1 x10 x14 x16 x17 x18 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_47(x1)(x10)(x14)(x16)(x17)(x18)(x)(st))(i)(xs)(st)
c_stateScan_case_47 x1 x10 x14 x16 x17 x18 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_47")(x)



c_stateScan_case_41 x1 x10 x14 x16 x17 x18@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Letter((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))(Curry.Module.CurryStringClassifier.c_stateScan(x1)(Curry.Module.CurryStringClassifier.C_Code(x10))(x10)(x17)(st))
c_stateScan_case_41 x1 x10 x14 x16 x17 x18@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_error))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(Curry.Module.Prelude.c_show(x1)(st))(st))(st)
c_stateScan_case_41 x1 x10 x14 x16 x17 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_41(x1)(x10)(x14)(x16)(x17)(x)(st))(i)(xs)(st)
c_stateScan_case_41 x1 x10 x14 x16 x17 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_41")(x)



c_stateScan_case_46 x1 x10 x14 x16 x17 x18 x20 x21@((Curry.Module.Prelude.:<) x22 x23) st = Curry.Module.CurryStringClassifier.c_stateScan_case_45(x1)(x10)(x14)(x16)(x17)(x18)(x20)(x22)(x23)(Curry.Module.Prelude.op_61_61(x22)(Curry.Module.Prelude.C_Char('\''))(st))(st)
c_stateScan_case_46 x1 x10 x14 x16 x17 x18 x20 x21@Curry.Module.Prelude.List st = Curry.Module.CurryStringClassifier.c_stateScan_case_42(x1)(x10)(x14)(x16)(x17)(Curry.Module.Prelude.op_61_61(x16)(Curry.Module.Prelude.C_Char('\''))(st))(st)
c_stateScan_case_46 x1 x10 x14 x16 x17 x18 x20 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_46(x1)(x10)(x14)(x16)(x17)(x18)(x20)(x)(st))(i)(xs)(st)
c_stateScan_case_46 x1 x10 x14 x16 x17 x18 x20 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_46")(x)



c_stateScan_case_42 x1 x10 x14 x16 x17 x18@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Letter((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))(Curry.Module.CurryStringClassifier.c_stateScan(x1)(Curry.Module.CurryStringClassifier.C_Code(x10))(x10)(x17)(st))
c_stateScan_case_42 x1 x10 x14 x16 x17 x18@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_error))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(Curry.Module.Prelude.c_show(x1)(st))(st))(st)
c_stateScan_case_42 x1 x10 x14 x16 x17 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_42(x1)(x10)(x14)(x16)(x17)(x)(st))(i)(xs)(st)
c_stateScan_case_42 x1 x10 x14 x16 x17 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_42")(x)



c_stateScan_case_45 x1 x10 x14 x16 x17 x18 x20 x22 x23 x24@Curry.Module.Prelude.C_True st = Curry.Module.CurryStringClassifier.c_stateScan_case_44(x1)(x10)(x16)(x18)(x20)(x23)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_all(Curry.Module.Prelude.pf(Curry.Module.Char.c_isDigit))(st))((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))))(st))(st)
c_stateScan_case_45 x1 x10 x14 x16 x17 x18 x20 x22 x23 x24@Curry.Module.Prelude.C_False st = Curry.Module.CurryStringClassifier.c_stateScan_case_43(x1)(x10)(x14)(x16)(x17)(Curry.Module.Prelude.op_61_61(x16)(Curry.Module.Prelude.C_Char('\''))(st))(st)
c_stateScan_case_45 x1 x10 x14 x16 x17 x18 x20 x22 x23 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_45(x1)(x10)(x14)(x16)(x17)(x18)(x20)(x22)(x23)(x)(st))(i)(xs)(st)
c_stateScan_case_45 x1 x10 x14 x16 x17 x18 x20 x22 x23 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_45")(x)



c_stateScan_case_43 x1 x10 x14 x16 x17 x18@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Letter((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))(Curry.Module.CurryStringClassifier.c_stateScan(x1)(Curry.Module.CurryStringClassifier.C_Code(x10))(x10)(x17)(st))
c_stateScan_case_43 x1 x10 x14 x16 x17 x18@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_error))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(Curry.Module.Prelude.c_show(x1)(st))(st))(st)
c_stateScan_case_43 x1 x10 x14 x16 x17 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_43(x1)(x10)(x14)(x16)(x17)(x)(st))(i)(xs)(st)
c_stateScan_case_43 x1 x10 x14 x16 x17 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_43")(x)



c_stateScan_case_44 x1 x10 x16 x18 x20 x23 x24@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Letter((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))))))(Curry.Module.CurryStringClassifier.c_stateScan(x1)(Curry.Module.CurryStringClassifier.C_Code(x10))(x10)(x23)(st))
c_stateScan_case_44 x1 x10 x16 x18 x20 x23 x24@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_error))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))(Curry.Module.Prelude.c_show(x1)(st))(st))(st)
c_stateScan_case_44 x1 x10 x16 x18 x20 x23 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_stateScan_case_44(x1)(x10)(x16)(x18)(x20)(x23)(x)(st))(i)(xs)(st)
c_stateScan_case_44 x1 x10 x16 x18 x20 x23 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.stateScan_case_44")(x)



c_weave_case_65 x3 x2@Curry.Module.Prelude.List st = Curry.Module.CurryStringClassifier.c_weave_case_64(x3)(st)
c_weave_case_65 x3 x2@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CurryStringClassifier.c_weave_case_62(x3)(x8)(x9)(st)
c_weave_case_65 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_weave_case_65(x3)(x)(st))(i)(xs)(st)
c_weave_case_65 x3 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.weave_case_65")(x)



c_weave_case_62 x3 x8 x9@Curry.Module.Prelude.List st = Curry.Module.CurryStringClassifier.c_weave_case_61(x8)(x9)(x3)(st)
c_weave_case_62 x3 x8 x9@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CurryStringClassifier.c_weave_case_60(x8)(x12)(x13)(x3)(st)
c_weave_case_62 x3 x8 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_weave_case_62(x3)(x8)(x)(st))(i)(xs)(st)
c_weave_case_62 x3 x8 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.weave_case_62")(x)



c_weave_case_60 x8 x12 x13 x3@((Curry.Module.Prelude.:<) x14 x15) st = (Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x14)(Curry.Module.CurryStringClassifier.c_weave(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x12)(x13))(x15))(st)))
c_weave_case_60 x8 x12 x13 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_weave_case_60(x8)(x12)(x13)(x)(st))(i)(xs)(st)
c_weave_case_60 x8 x12 x13 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.weave_case_60")(x)



c_weave_case_61 x8 x9 x3@Curry.Module.Prelude.List st = (Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)
c_weave_case_61 x8 x9 x3@((Curry.Module.Prelude.:<) x10 x11) st = (Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x10)(Curry.Module.CurryStringClassifier.c_weave(Curry.Module.Prelude.T2(x9)(x11))(st)))
c_weave_case_61 x8 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_weave_case_61(x8)(x9)(x)(st))(i)(xs)(st)
c_weave_case_61 x8 x9 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.weave_case_61")(x)



c_weave_case_64 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_weave_case_64 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CurryStringClassifier.c_weave_case_63(x4)(x5)(st)
c_weave_case_64 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_weave_case_64(x)(st))(i)(xs)(st)
c_weave_case_64 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.weave_case_64")(x)



c_weave_case_63 x4 x5@Curry.Module.Prelude.List st = (Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)
c_weave_case_63 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_weave_case_63(x4)(x)(st))(i)(xs)(st)
c_weave_case_63 x4 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.weave_case_63")(x)



c_unweaveCode_case_66 x2 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x2)(x5))(x6)
c_unweaveCode_case_66 x2 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.T2(x5)((Curry.Module.Prelude.:<)(x2)(x6))
c_unweaveCode_case_66 x2 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CurryStringClassifier.c_unweaveCode_case_66(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c_unweaveCode_case_66 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("CurryStringClassifier.unweaveCode_case_66")(x)


