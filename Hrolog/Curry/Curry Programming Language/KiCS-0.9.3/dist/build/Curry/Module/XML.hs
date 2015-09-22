{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.XML (module Curry.Module.XML) where

import Curry.RunTimeSystem
import Curry.Module.Char
import Curry.Module.List
import Curry.Module.Prelude
import Curry.Module.Read



-- begin included



-- end included

data C_XmlExp = C_XText (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_XElem (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)
  | C_XmlExpFail Curry.RunTimeSystem.C_Exceptions
  | C_XmlExpOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.XML.C_XmlExp)

data C_Encoding = C_StandardEnc
  | C_Iso88591Enc
  | C_EncodingFail Curry.RunTimeSystem.C_Exceptions
  | C_EncodingOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.XML.C_Encoding)

data C_XmlDocParams = C_Enc Curry.Module.XML.C_Encoding
  | C_DtdUrl (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_XmlDocParamsFail Curry.RunTimeSystem.C_Exceptions
  | C_XmlDocParamsOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.XML.C_XmlDocParams)

instance BaseCurry Curry.Module.XML.C_XmlExp where
  nf f (Curry.Module.XML.C_XText x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.XML.C_XText(v1))(state1))(x1)(state0)
  nf f (Curry.Module.XML.C_XElem x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.XML.C_XElem(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.XML.C_XText x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.XML.C_XText(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.XML.C_XElem x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.XML.C_XElem(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.XML.C_XmlExpOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.XML.C_XText(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.XML.C_XElem(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(3)

  failed  = Curry.Module.XML.C_XmlExpFail

  branching  = Curry.Module.XML.C_XmlExpOr

  consKind (Curry.Module.XML.C_XmlExpOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.XML.C_XmlExpFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.XML.C_XmlExpFail x) = x

  orRef (Curry.Module.XML.C_XmlExpOr x _) = x

  branches (Curry.Module.XML.C_XmlExpOr _ x) = x





instance BaseCurry Curry.Module.XML.C_Encoding where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.XML.C_EncodingOr(Curry.RunTimeSystem.mkRef(r)(0)(i))([Curry.Module.XML.C_StandardEnc,Curry.Module.XML.C_Iso88591Enc]))(0)

  failed  = Curry.Module.XML.C_EncodingFail

  branching  = Curry.Module.XML.C_EncodingOr

  consKind (Curry.Module.XML.C_EncodingOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.XML.C_EncodingFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.XML.C_EncodingFail x) = x

  orRef (Curry.Module.XML.C_EncodingOr x _) = x

  branches (Curry.Module.XML.C_EncodingOr _ x) = x





instance BaseCurry Curry.Module.XML.C_XmlDocParams where
  nf f (Curry.Module.XML.C_Enc x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.XML.C_Enc(v1))(state1))(x1)(state0)
  nf f (Curry.Module.XML.C_DtdUrl x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.XML.C_DtdUrl(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.XML.C_Enc x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.XML.C_Enc(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.XML.C_DtdUrl x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.XML.C_DtdUrl(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.XML.C_XmlDocParamsOr(Curry.RunTimeSystem.mkRef(r)(1)(i))([Curry.Module.XML.C_Enc(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.XML.C_DtdUrl(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(1)

  failed  = Curry.Module.XML.C_XmlDocParamsFail

  branching  = Curry.Module.XML.C_XmlDocParamsOr

  consKind (Curry.Module.XML.C_XmlDocParamsOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.XML.C_XmlDocParamsFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.XML.C_XmlDocParamsFail x) = x

  orRef (Curry.Module.XML.C_XmlDocParamsOr x _) = x

  branches (Curry.Module.XML.C_XmlDocParamsOr _ x) = x





instance Curry Curry.Module.XML.C_XmlExp where
  strEq (Curry.Module.XML.C_XText x1) (Curry.Module.XML.C_XText y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.XML.C_XElem x1 x2 x3) (Curry.Module.XML.C_XElem y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.XML.C_XText x1) (Curry.Module.XML.C_XText y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.XML.C_XElem x1 x2 x3) (Curry.Module.XML.C_XElem y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.XML.C_XText x1) st = Curry.Module.XML.C_XText(f((0::Int))(x1)(st))
  propagate f (Curry.Module.XML.C_XElem x1 x2 x3) st = Curry.Module.XML.C_XElem(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))

  foldCurry f c (Curry.Module.XML.C_XText x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.XML.C_XElem x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)

  typeName _ = "XmlExp"

  showQ d (Curry.Module.XML.C_XText x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("XML.XText "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.XML.C_XElem x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("XML.XElem "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ _ (Curry.Module.XML.C_XmlExpOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.XML.C_Encoding where
  strEq Curry.Module.XML.C_StandardEnc Curry.Module.XML.C_StandardEnc st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.XML.C_Iso88591Enc Curry.Module.XML.C_Iso88591Enc st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.XML.C_StandardEnc Curry.Module.XML.C_StandardEnc st = Curry.Module.Prelude.C_True
  eq Curry.Module.XML.C_Iso88591Enc Curry.Module.XML.C_Iso88591Enc st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.XML.C_StandardEnc st = Curry.Module.XML.C_StandardEnc
  propagate f Curry.Module.XML.C_Iso88591Enc st = Curry.Module.XML.C_Iso88591Enc

  foldCurry f c Curry.Module.XML.C_StandardEnc st = c
  foldCurry f c Curry.Module.XML.C_Iso88591Enc st = c

  typeName _ = "Encoding"

  showQ _ Curry.Module.XML.C_StandardEnc = Prelude.showString("XML.StandardEnc")
  showQ _ Curry.Module.XML.C_Iso88591Enc = Prelude.showString("XML.Iso88591Enc")
  showQ _ (Curry.Module.XML.C_EncodingOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.XML.C_XmlDocParams where
  strEq (Curry.Module.XML.C_Enc x1) (Curry.Module.XML.C_Enc y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.XML.C_DtdUrl x1) (Curry.Module.XML.C_DtdUrl y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.XML.C_Enc x1) (Curry.Module.XML.C_Enc y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.XML.C_DtdUrl x1) (Curry.Module.XML.C_DtdUrl y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.XML.C_Enc x1) st = Curry.Module.XML.C_Enc(f((0::Int))(x1)(st))
  propagate f (Curry.Module.XML.C_DtdUrl x1) st = Curry.Module.XML.C_DtdUrl(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.XML.C_Enc x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.XML.C_DtdUrl x1) st = f(x1)(c)(st)

  typeName _ = "XmlDocParams"

  showQ d (Curry.Module.XML.C_Enc x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("XML.Enc "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.XML.C_DtdUrl x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("XML.DtdUrl "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.XML.C_XmlDocParamsOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.XML.C_XmlExp where
  showsPrec d (Curry.Module.XML.C_XText x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("XText "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.XML.C_XElem x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("XElem "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec _ (Curry.Module.XML.C_XmlExpOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.XML.C_Encoding where
  showsPrec _ Curry.Module.XML.C_StandardEnc = Prelude.showString("StandardEnc")
  showsPrec _ Curry.Module.XML.C_Iso88591Enc = Prelude.showString("Iso88591Enc")
  showsPrec _ (Curry.Module.XML.C_EncodingOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.XML.C_XmlDocParams where
  showsPrec d (Curry.Module.XML.C_Enc x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Enc "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.XML.C_DtdUrl x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("DtdUrl "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.XML.C_XmlDocParamsOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Read Curry.Module.XML.C_XmlExp where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.XML.C_XText(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("XML")("XText")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.XML.C_XElem(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("XML")("XElem")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r))





instance Read Curry.Module.XML.C_Encoding where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.XML.C_StandardEnc)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("XML")("StandardEnc")(r)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.XML.C_Iso88591Enc)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("XML")("Iso88591Enc")(r)])(r))





instance Read Curry.Module.XML.C_XmlDocParams where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.XML.C_Enc(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("XML")("Enc")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.XML.C_DtdUrl(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("XML")("DtdUrl")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))





c_encoding2Attribute :: Curry.Module.XML.C_Encoding -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_encoding2Attribute x1@Curry.Module.XML.C_StandardEnc st = Curry.Module.Prelude.List
c_encoding2Attribute x1@Curry.Module.XML.C_Iso88591Enc st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('8'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('8'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('5'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('9'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('1'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))))))))))))))
c_encoding2Attribute (Curry.Module.XML.C_EncodingOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_encoding2Attribute(x)(st))(i)(xs)(st)
c_encoding2Attribute x st = Curry.RunTimeSystem.patternFail("XML.encoding2Attribute")(x)



c_encoding2EncFunc :: Curry.Module.XML.C_Encoding -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_encoding2EncFunc x1@Curry.Module.XML.C_StandardEnc st = Curry.Module.Prelude.pf(Curry.Module.XML.c_standardEncoding)
c_encoding2EncFunc x1@Curry.Module.XML.C_Iso88591Enc st = Curry.Module.Prelude.pf(Curry.Module.XML.c_iso88591Encoding)
c_encoding2EncFunc (Curry.Module.XML.C_EncodingOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_encoding2EncFunc(x)(st))(i)(xs)(st)
c_encoding2EncFunc x st = Curry.RunTimeSystem.patternFail("XML.encoding2EncFunc")(x)



c_standardEncoding :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_standardEncoding x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_standardEncoding x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.XML.c_standardEncoding_case_68(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('<'))(st))(st)
c_standardEncoding (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_standardEncoding(x)(st))(i)(xs)(st)
c_standardEncoding x st = Curry.RunTimeSystem.patternFail("XML.standardEncoding")(x)



c_iso88591Encoding :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_iso88591Encoding x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_iso88591Encoding x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.XML.c_iso88591Encoding_case_60(x2)(x3)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(Curry.Module.Prelude.c_ord(x2)(st))(st))(Curry.Module.XML.c_iso88591list(st))(st))(st)
c_iso88591Encoding (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_iso88591Encoding(x)(st))(i)(xs)(st)
c_iso88591Encoding x st = Curry.RunTimeSystem.patternFail("XML.iso88591Encoding")(x)



c_iso88591list :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_iso88591list st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))))))))))))))))))))))))



c_lookupEncoding :: (Curry.Module.Prelude.List Curry.Module.XML.C_XmlDocParams) -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_Encoding
c_lookupEncoding x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.XML.c_lookupEncoding_case_59(x3)(x2)(st)
c_lookupEncoding x1@Curry.Module.Prelude.List st = Curry.Module.XML.C_StandardEnc
c_lookupEncoding (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_lookupEncoding(x)(st))(i)(xs)(st)
c_lookupEncoding x st = Curry.RunTimeSystem.patternFail("XML.lookupEncoding")(x)



c_lookupDtdUrl :: (Curry.Module.Prelude.List Curry.Module.XML.C_XmlDocParams) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_lookupDtdUrl x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.XML.c_lookupDtdUrl_case_58(x3)(x2)(st)
c_lookupDtdUrl (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_lookupDtdUrl(x)(st))(i)(xs)(st)
c_lookupDtdUrl x st = Curry.RunTimeSystem.patternFail("XML.lookupDtdUrl")(x)



c_hasDtdUrl :: (Curry.Module.Prelude.List Curry.Module.XML.C_XmlDocParams) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_hasDtdUrl x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_False
c_hasDtdUrl x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.XML.c_hasDtdUrl_case_57(x3)(x2)(st)
c_hasDtdUrl (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_hasDtdUrl(x)(st))(i)(xs)(st)
c_hasDtdUrl x st = Curry.RunTimeSystem.patternFail("XML.hasDtdUrl")(x)



c_xtxt :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_XmlExp
c_xtxt x1 st = Curry.Module.XML.C_XText(x1)



c_xml :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_XmlExp
c_xml x1 x2 st = Curry.Module.XML.C_XElem(x1)(Curry.Module.Prelude.List)(x2)



c_writeXmlFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.XML.C_XmlExp -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_writeXmlFile x1 x2 st = Curry.Module.XML.c_writeXmlFileWithParams(x1)((Curry.Module.Prelude.:<)(Curry.Module.XML.C_Enc(Curry.Module.XML.C_StandardEnc))(Curry.Module.Prelude.List))(x2)(st)



c_writeXmlFileWithParams :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.XML.C_XmlDocParams) -> Curry.Module.XML.C_XmlExp -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_writeXmlFileWithParams x1 x2 x3 st = Curry.Module.Prelude.c_writeFile(x1)(Curry.Module.XML.c_showXmlDocWithParams(x2)(x3)(st))(st)



c_showXmlDoc :: Curry.Module.XML.C_XmlExp -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showXmlDoc x1 st = Curry.Module.XML.c_showXmlDocWithParams(Curry.Module.Prelude.List)(x1)(st)



c_showXmlDocWithParams :: (Curry.Module.Prelude.List Curry.Module.XML.C_XmlDocParams) -> Curry.Module.XML.C_XmlExp -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showXmlDocWithParams x1 x2@(Curry.Module.XML.C_XElem x3 x4 x5) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('?'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('1'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('0'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))(Curry.Module.Prelude.op_43_43(Curry.Module.XML.c_encoding2Attribute(Curry.Module.XML.c_lookupEncoding(x1)(st))(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List)))))))))))))(Curry.Module.Prelude.op_43_43(Curry.Module.XML.c_showXmlDocWithParams_case_56(x1)(Curry.Module.XML.c_hasDtdUrl(x1)(st))(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('?'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.op_43_43(Curry.Module.XML.c_showXmlDocWithParams_case_55(x1)(x3)(Curry.Module.XML.c_hasDtdUrl(x1)(st))(st))(Curry.Module.XML.c_showXmlExp(Curry.Module.Prelude.C_Zero)(Curry.Module.XML.c_encoding2EncFunc(Curry.Module.XML.c_lookupEncoding(x1)(st))(st))(Curry.Module.XML.C_XElem(x3)(x4)(x5))(st))(st))(st))(st))(st))(st))(st)
c_showXmlDocWithParams x1 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_showXmlDocWithParams(x1)(x)(st))(i)(xs)(st)
c_showXmlDocWithParams x1 x st = Curry.RunTimeSystem.patternFail("XML.showXmlDocWithParams")(x)



c_showXmlExp :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.XML.C_XmlExp -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showXmlExp x1 x2 x3@(Curry.Module.XML.C_XText x4) st = Curry.Module.Prelude.op_43_43(Curry.Module.XML.c_xtab(x1)(st))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_apply(x2)(x4)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(st))(st)
c_showXmlExp x1 x2 x3@(Curry.Module.XML.C_XElem x5 x6 x7) st = Curry.Module.Prelude.op_43_43(Curry.Module.XML.c_xtab(x1)(st))(Curry.Module.Prelude.op_43_43(Curry.Module.XML.c_showXmlOpenTag(x5)(x6)(x2)(st))(Curry.Module.XML.c_showXmlExp_case_54(x1)(x2)(x5)(x7)(Curry.Module.Prelude.op_61_61(x7)(Curry.Module.Prelude.List)(st))(st))(st))(st)
c_showXmlExp x1 x2 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_showXmlExp(x1)(x2)(x)(st))(i)(xs)(st)
c_showXmlExp x1 x2 x st = Curry.RunTimeSystem.patternFail("XML.showXmlExp")(x)



c_showXmlExp'46_'35selFP3'35s :: (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showXmlExp'46_'35selFP3'35s x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.XML.c_showXmlExp'46_'35selFP3'35s_case_52(x3)(x2)(st)
c_showXmlExp'46_'35selFP3'35s (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_showXmlExp'46_'35selFP3'35s(x)(st))(i)(xs)(st)
c_showXmlExp'46_'35selFP3'35s x st = Curry.RunTimeSystem.patternFail("XML.showXmlExp._#selFP3#s")(x)



c_xtab :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_xtab x1 st = Curry.Module.Prelude.c_take(x1)(Curry.Module.Prelude.c_repeat(Curry.Module.Prelude.C_Char(' '))(st))(st)



c_showXmlOpenTag :: (Curry t0) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) t0)) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showXmlOpenTag x1 x2 x3 st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x1)(Curry.Module.Prelude.c_concat(Curry.Module.Prelude.c_map(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.pf(Curry.Module.XML.c_showXmlOpenTag'46attr2string'46125(x3)))(st))(x2)(st))(st))(st))(st)



c_showXmlOpenTag'46attr2string'46125 :: (Curry t130) => (Curry.Module.Prelude.Prim (t130 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) t130) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showXmlOpenTag'46attr2string'46125 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.Prelude.op_43_43(x3)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_apply(x1)(x4)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List))(st))(st))(st)
c_showXmlOpenTag'46attr2string'46125 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_showXmlOpenTag'46attr2string'46125(x1)(x)(st))(i)(xs)(st)
c_showXmlOpenTag'46attr2string'46125 x1 x st = Curry.RunTimeSystem.patternFail("XML.showXmlOpenTag.attr2string.125")(x)



c_showXmlExps :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showXmlExps x1 x2 x3 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.XML.c_showXmlExp(x1)(x3)))(st))(x2)(st)



c_isXText :: Curry.Module.XML.C_XmlExp -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isXText x1@(Curry.Module.XML.C_XText x2) st = Curry.Module.Prelude.C_True
c_isXText x1@(Curry.Module.XML.C_XElem x3 x4 x5) st = Curry.Module.Prelude.C_False
c_isXText (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_isXText(x)(st))(i)(xs)(st)
c_isXText x st = Curry.RunTimeSystem.patternFail("XML.isXText")(x)



c_xmlUnquoteSpecials :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_xmlUnquoteSpecials x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_xmlUnquoteSpecials x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.XML.c_xmlUnquoteSpecials_case_50(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('&'))(st))(st)
c_xmlUnquoteSpecials (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_xmlUnquoteSpecials(x)(st))(i)(xs)(st)
c_xmlUnquoteSpecials x st = Curry.RunTimeSystem.patternFail("XML.xmlUnquoteSpecials")(x)



c_xmlUnquoteSpecials'46_'35selFP5'35special :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_xmlUnquoteSpecials'46_'35selFP5'35special x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_xmlUnquoteSpecials'46_'35selFP5'35special (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_xmlUnquoteSpecials'46_'35selFP5'35special(x)(st))(i)(xs)(st)
c_xmlUnquoteSpecials'46_'35selFP5'35special x st = Curry.RunTimeSystem.patternFail("XML.xmlUnquoteSpecials._#selFP5#special")(x)



c_xmlUnquoteSpecials'46_'35selFP6'35rest :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_xmlUnquoteSpecials'46_'35selFP6'35rest x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_xmlUnquoteSpecials'46_'35selFP6'35rest (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_xmlUnquoteSpecials'46_'35selFP6'35rest(x)(st))(i)(xs)(st)
c_xmlUnquoteSpecials'46_'35selFP6'35rest x st = Curry.RunTimeSystem.patternFail("XML.xmlUnquoteSpecials._#selFP6#rest")(x)



c_xmlUnquoteSpecial :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_xmlUnquoteSpecial x1 x2 st = Curry.Module.XML.c_xmlUnquoteSpecial_case_48(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))(st))(st)



c_unquoteUnicode :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_unquoteUnicode x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_unquoteUnicode x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.XML.c_unquoteUnicode_case_35(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('#'))(st))(st)
c_unquoteUnicode (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_unquoteUnicode(x)(st))(i)(xs)(st)
c_unquoteUnicode x st = Curry.RunTimeSystem.patternFail("XML.unquoteUnicode")(x)



c_readXmlFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.XML.C_XmlExp
c_readXmlFile x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_readFile(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.XML.c_readXmlFile'46_'35lambda3(x1)))(st)



c_readXmlFile'46_'35lambda3 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.XML.C_XmlExp
c_readXmlFile'46_'35lambda3 x1 x2 st = let {x3 = Curry.Module.XML.c_parseXmlString(x2)(st)} in Curry.Module.XML.c_readXmlFile'46_'35lambda3_case_31(x1)(x3)(Curry.Module.Prelude.op_61_61(x3)(Curry.Module.Prelude.List)(st))(st)



c_readUnsafeXmlFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe Curry.Module.XML.C_XmlExp)
c_readUnsafeXmlFile x1 st = Curry.Module.Prelude.c_catchFail(Curry.Module.Prelude.op_62_62_61(Curry.Module.XML.c_readXmlFile(x1)(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Prelude.pc(Curry.Module.Prelude.C_Just))(st))(st))(Curry.Module.Prelude.c_return(Curry.Module.Prelude.C_Nothing)(st))(st)



c_showXmlFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_showXmlFile x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.XML.c_readXmlFile(x1)(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_putStr))(Curry.Module.Prelude.pf(Curry.Module.XML.c_showXmlDoc))(st))(st)



c_readFileWithXmlDocs :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)
c_readFileWithXmlDocs x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_readFile(x1)(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Prelude.pf(Curry.Module.XML.c_parseXmlString))(st))(st)



c_parseXmlString :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlString x1 st = Curry.Module.Prelude.c_fst(Curry.Module.XML.c_parseXmlTokens(Curry.Module.XML.c_scanXmlString(x1)(st))(Curry.Module.Prelude.C_Nothing)(st))(st)



c_parseXmlTokens :: (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)
c_parseXmlTokens x1@Curry.Module.Prelude.List x2 st = Curry.Module.XML.c_parseXmlTokens_case_29(x2)(st)
c_parseXmlTokens x1@((Curry.Module.Prelude.:<) x3 x4) x2 st = Curry.Module.XML.c_parseXmlTokens_case_28(x2)(x4)(x3)(st)
c_parseXmlTokens (Curry.Module.Prelude.ListOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseXmlTokens(x)(x2)(st))(i)(xs)(st)
c_parseXmlTokens x x2 st = Curry.RunTimeSystem.patternFail("XML.parseXmlTokens")(x)



c_parseXmlTokens'46_'35selFP8'35xexps :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP8'35xexps x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_parseXmlTokens'46_'35selFP8'35xexps (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseXmlTokens'46_'35selFP8'35xexps(x)(st))(i)(xs)(st)
c_parseXmlTokens'46_'35selFP8'35xexps x st = Curry.RunTimeSystem.patternFail("XML.parseXmlTokens._#selFP8#xexps")(x)



c_parseXmlTokens'46_'35selFP9'35rem_xtokens :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP9'35rem_xtokens x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_parseXmlTokens'46_'35selFP9'35rem_xtokens (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseXmlTokens'46_'35selFP9'35rem_xtokens(x)(st))(i)(xs)(st)
c_parseXmlTokens'46_'35selFP9'35rem_xtokens x st = Curry.RunTimeSystem.patternFail("XML.parseXmlTokens._#selFP9#rem_xtokens")(x)



c_parseXmlTokens'46_'35selFP14'35xexps1 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP14'35xexps1 x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_parseXmlTokens'46_'35selFP14'35xexps1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseXmlTokens'46_'35selFP14'35xexps1(x)(st))(i)(xs)(st)
c_parseXmlTokens'46_'35selFP14'35xexps1 x st = Curry.RunTimeSystem.patternFail("XML.parseXmlTokens._#selFP14#xexps1")(x)



c_parseXmlTokens'46_'35selFP15'35xtokens1 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP15'35xtokens1 x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_parseXmlTokens'46_'35selFP15'35xtokens1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseXmlTokens'46_'35selFP15'35xtokens1(x)(st))(i)(xs)(st)
c_parseXmlTokens'46_'35selFP15'35xtokens1 x st = Curry.RunTimeSystem.patternFail("XML.parseXmlTokens._#selFP15#xtokens1")(x)



c_parseXmlTokens'46_'35selFP12'35xexps :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP12'35xexps x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_parseXmlTokens'46_'35selFP12'35xexps (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseXmlTokens'46_'35selFP12'35xexps(x)(st))(i)(xs)(st)
c_parseXmlTokens'46_'35selFP12'35xexps x st = Curry.RunTimeSystem.patternFail("XML.parseXmlTokens._#selFP12#xexps")(x)



c_parseXmlTokens'46_'35selFP13'35rem_xtokens :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP13'35rem_xtokens x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_parseXmlTokens'46_'35selFP13'35rem_xtokens (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseXmlTokens'46_'35selFP13'35rem_xtokens(x)(st))(i)(xs)(st)
c_parseXmlTokens'46_'35selFP13'35rem_xtokens x st = Curry.RunTimeSystem.patternFail("XML.parseXmlTokens._#selFP13#rem_xtokens")(x)



c_parseXmlTokens'46_'35selFP17'35xexps :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP17'35xexps x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_parseXmlTokens'46_'35selFP17'35xexps (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseXmlTokens'46_'35selFP17'35xexps(x)(st))(i)(xs)(st)
c_parseXmlTokens'46_'35selFP17'35xexps x st = Curry.RunTimeSystem.patternFail("XML.parseXmlTokens._#selFP17#xexps")(x)



c_parseXmlTokens'46_'35selFP18'35rem_xtokens :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP18'35rem_xtokens x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_parseXmlTokens'46_'35selFP18'35rem_xtokens (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseXmlTokens'46_'35selFP18'35rem_xtokens(x)(st))(i)(xs)(st)
c_parseXmlTokens'46_'35selFP18'35rem_xtokens x st = Curry.RunTimeSystem.patternFail("XML.parseXmlTokens._#selFP18#rem_xtokens")(x)



c_parseXmlTokens'46_'35selFP20'35xexps :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP20'35xexps x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_parseXmlTokens'46_'35selFP20'35xexps (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseXmlTokens'46_'35selFP20'35xexps(x)(st))(i)(xs)(st)
c_parseXmlTokens'46_'35selFP20'35xexps x st = Curry.RunTimeSystem.patternFail("XML.parseXmlTokens._#selFP20#xexps")(x)



c_parseXmlTokens'46_'35selFP21'35rem_xtokens :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_parseXmlTokens'46_'35selFP21'35rem_xtokens x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_parseXmlTokens'46_'35selFP21'35rem_xtokens (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseXmlTokens'46_'35selFP21'35rem_xtokens(x)(st))(i)(xs)(st)
c_parseXmlTokens'46_'35selFP21'35rem_xtokens x st = Curry.RunTimeSystem.patternFail("XML.parseXmlTokens._#selFP21#rem_xtokens")(x)



c_scanXmlString :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_scanXmlString x1 st = Curry.Module.XML.c_scanXmlString'46scanXml'46191(Curry.Module.Prelude.c_apply(Curry.Module.XML.c_dropBlanks(st))(x1)(st))(st)



c_scanXmlString'46scanXml'46191 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_scanXmlString'46scanXml'46191 x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_scanXmlString'46scanXml'46191 x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.XML.c_scanXmlString'46scanXml'46191_case_22(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('<'))(st))(st)
c_scanXmlString'46scanXml'46191 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlString'46scanXml'46191(x)(st))(i)(xs)(st)
c_scanXmlString'46scanXml'46191 x st = Curry.RunTimeSystem.patternFail("XML.scanXmlString.scanXml.191")(x)



c_scanXmlString'46scanXml'46191'46_'35selFP23'35initxt :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_scanXmlString'46scanXml'46191'46_'35selFP23'35initxt x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_scanXmlString'46scanXml'46191'46_'35selFP23'35initxt (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlString'46scanXml'46191'46_'35selFP23'35initxt(x)(st))(i)(xs)(st)
c_scanXmlString'46scanXml'46191'46_'35selFP23'35initxt x st = Curry.RunTimeSystem.patternFail("XML.scanXmlString.scanXml.191._#selFP23#initxt")(x)



c_scanXmlString'46scanXml'46191'46_'35selFP24'35remtag :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_scanXmlString'46scanXml'46191'46_'35selFP24'35remtag x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_scanXmlString'46scanXml'46191'46_'35selFP24'35remtag (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlString'46scanXml'46191'46_'35selFP24'35remtag(x)(st))(i)(xs)(st)
c_scanXmlString'46scanXml'46191'46_'35selFP24'35remtag x st = Curry.RunTimeSystem.patternFail("XML.scanXmlString.scanXml.191._#selFP24#remtag")(x)



c_scanXmlText :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_scanXmlText x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)
c_scanXmlText x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.XML.c_scanXmlText_case_21(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('<'))(st))(st)
c_scanXmlText (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlText(x)(st))(i)(xs)(st)
c_scanXmlText x st = Curry.RunTimeSystem.patternFail("XML.scanXmlText")(x)



c_scanXmlText'46_'35selFP26'35txt :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_scanXmlText'46_'35selFP26'35txt x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_scanXmlText'46_'35selFP26'35txt (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlText'46_'35selFP26'35txt(x)(st))(i)(xs)(st)
c_scanXmlText'46_'35selFP26'35txt x st = Curry.RunTimeSystem.patternFail("XML.scanXmlText._#selFP26#txt")(x)



c_scanXmlText'46_'35selFP27'35rem :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_scanXmlText'46_'35selFP27'35rem x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_scanXmlText'46_'35selFP27'35rem (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlText'46_'35selFP27'35rem(x)(st))(i)(xs)(st)
c_scanXmlText'46_'35selFP27'35rem x st = Curry.RunTimeSystem.patternFail("XML.scanXmlText._#selFP27#rem")(x)



c_scanXmlText'46_'35selFP29'35txt :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_scanXmlText'46_'35selFP29'35txt x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_scanXmlText'46_'35selFP29'35txt (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlText'46_'35selFP29'35txt(x)(st))(i)(xs)(st)
c_scanXmlText'46_'35selFP29'35txt x st = Curry.RunTimeSystem.patternFail("XML.scanXmlText._#selFP29#txt")(x)



c_scanXmlText'46_'35selFP30'35rem :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_scanXmlText'46_'35selFP30'35rem x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_scanXmlText'46_'35selFP30'35rem (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlText'46_'35selFP30'35rem(x)(st))(i)(xs)(st)
c_scanXmlText'46_'35selFP30'35rem x st = Curry.RunTimeSystem.patternFail("XML.scanXmlText._#selFP30#rem")(x)



c_scanXmlElem :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_scanXmlElem x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_scanXmlElem x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.XML.c_scanXmlElem_case_17(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('!'))(st))(st)
c_scanXmlElem (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlElem(x)(st))(i)(xs)(st)
c_scanXmlElem x st = Curry.RunTimeSystem.patternFail("XML.scanXmlElem")(x)



c_scanXmlElemName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_scanXmlElemName x1 x2@Curry.Module.Prelude.List st = (Curry.Module.Prelude.:<)(Curry.Module.XML.C_XElem((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))(x1))(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(Curry.Module.Prelude.List)
c_scanXmlElemName x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.XML.c_scanXmlElemName_case_13(x1)(x3)(x4)(Curry.Module.Prelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('>'))(st))(st)
c_scanXmlElemName x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlElemName(x1)(x)(st))(i)(xs)(st)
c_scanXmlElemName x1 x st = Curry.RunTimeSystem.patternFail("XML.scanXmlElemName")(x)



c_scanXmlElemName'46_'35selFP32'35attrs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_scanXmlElemName'46_'35selFP32'35attrs x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_scanXmlElemName'46_'35selFP32'35attrs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlElemName'46_'35selFP32'35attrs(x)(st))(i)(xs)(st)
c_scanXmlElemName'46_'35selFP32'35attrs x st = Curry.RunTimeSystem.patternFail("XML.scanXmlElemName._#selFP32#attrs")(x)



c_scanXmlElemName'46_'35selFP33'35rest :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_scanXmlElemName'46_'35selFP33'35rest x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_scanXmlElemName'46_'35selFP33'35rest (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlElemName'46_'35selFP33'35rest(x)(st))(i)(xs)(st)
c_scanXmlElemName'46_'35selFP33'35rest x st = Curry.RunTimeSystem.patternFail("XML.scanXmlElemName._#selFP33#rest")(x)



c_scanXmlComment :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_scanXmlComment x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_scanXmlComment x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.XML.c_scanXmlComment_case_8(x2)(x3)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('-'))(st))(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_take(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x3)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))(Curry.Module.Prelude.List)))(st))(st))(st)
c_scanXmlComment (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlComment(x)(st))(i)(xs)(st)
c_scanXmlComment x st = Curry.RunTimeSystem.patternFail("XML.scanXmlComment")(x)



c_scanXmlCData :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_scanXmlCData x1 st = let {x2 = Curry.Module.XML.c_dropCData(x1)(st)} in Curry.Module.XML.c_scanXmlCData_case_7(x2)(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_head(x2)(st))(Curry.Module.Prelude.C_Char('>'))(st))(st)



c_dropCData :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_dropCData x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_dropCData x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.XML.c_dropCData_case_6(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('['))(st))(st)
c_dropCData (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_dropCData(x)(st))(i)(xs)(st)
c_dropCData x st = Curry.RunTimeSystem.patternFail("XML.dropCData")(x)



c_scanXmlProcInstr :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp
c_scanXmlProcInstr x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_scanXmlProcInstr x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.XML.c_scanXmlProcInstr_case_3(x2)(x3)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('?'))(st))(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_head(x3)(st))(Curry.Module.Prelude.C_Char('>'))(st))(st))(st)
c_scanXmlProcInstr (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlProcInstr(x)(st))(i)(xs)(st)
c_scanXmlProcInstr x st = Curry.RunTimeSystem.patternFail("XML.scanXmlProcInstr")(x)



c_parseAttrs :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_parseAttrs x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)
c_parseAttrs x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.XML.c_parseAttrs_case_2(x2)(x3)(Curry.Module.Char.c_isAlpha(x2)(st))(st)
c_parseAttrs (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseAttrs(x)(st))(i)(xs)(st)
c_parseAttrs x st = Curry.RunTimeSystem.patternFail("XML.parseAttrs")(x)



c_parseAttrs'46_'35selFP41'35name :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_parseAttrs'46_'35selFP41'35name x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_parseAttrs'46_'35selFP41'35name (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseAttrs'46_'35selFP41'35name(x)(st))(i)(xs)(st)
c_parseAttrs'46_'35selFP41'35name x st = Curry.RunTimeSystem.patternFail("XML.parseAttrs._#selFP41#name")(x)



c_parseAttrs'46_'35selFP42'35rest1 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_parseAttrs'46_'35selFP42'35rest1 x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_parseAttrs'46_'35selFP42'35rest1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseAttrs'46_'35selFP42'35rest1(x)(st))(i)(xs)(st)
c_parseAttrs'46_'35selFP42'35rest1 x st = Curry.RunTimeSystem.patternFail("XML.parseAttrs._#selFP42#rest1")(x)



c_parseAttrs'46_'35selFP39'35value :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_parseAttrs'46_'35selFP39'35value x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_parseAttrs'46_'35selFP39'35value (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseAttrs'46_'35selFP39'35value(x)(st))(i)(xs)(st)
c_parseAttrs'46_'35selFP39'35value x st = Curry.RunTimeSystem.patternFail("XML.parseAttrs._#selFP39#value")(x)



c_parseAttrs'46_'35selFP40'35rest2 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_parseAttrs'46_'35selFP40'35rest2 x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_parseAttrs'46_'35selFP40'35rest2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseAttrs'46_'35selFP40'35rest2(x)(st))(i)(xs)(st)
c_parseAttrs'46_'35selFP40'35rest2 x st = Curry.RunTimeSystem.patternFail("XML.parseAttrs._#selFP40#rest2")(x)



c_parseAttrs'46_'35selFP37'35rem_attrs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_parseAttrs'46_'35selFP37'35rem_attrs x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_parseAttrs'46_'35selFP37'35rem_attrs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseAttrs'46_'35selFP37'35rem_attrs(x)(st))(i)(xs)(st)
c_parseAttrs'46_'35selFP37'35rem_attrs x st = Curry.RunTimeSystem.patternFail("XML.parseAttrs._#selFP37#rem_attrs")(x)



c_parseAttrs'46_'35selFP38'35rem_inp :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_parseAttrs'46_'35selFP38'35rem_inp x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_parseAttrs'46_'35selFP38'35rem_inp (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseAttrs'46_'35selFP38'35rem_inp(x)(st))(i)(xs)(st)
c_parseAttrs'46_'35selFP38'35rem_inp x st = Curry.RunTimeSystem.patternFail("XML.parseAttrs._#selFP38#rem_inp")(x)



c_dropBlanks :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_dropBlanks st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_dropWhile(Curry.Module.Prelude.pf(Curry.Module.Char.c_isSpace)))



c_splitAtChar :: (Curry t0) => t0 -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t0)
c_splitAtChar x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)
c_splitAtChar x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.XML.c_splitAtChar_case_0(x1)(x3)(x4)(Curry.Module.Prelude.op_61_61(x3)(x1)(st))(st)
c_splitAtChar x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_splitAtChar(x1)(x)(st))(i)(xs)(st)
c_splitAtChar x1 x st = Curry.RunTimeSystem.patternFail("XML.splitAtChar")(x)



c_splitAtChar'46_'35selFP44'35first :: (Curry t231) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t231) (Curry.Module.Prelude.List t231)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t231
c_splitAtChar'46_'35selFP44'35first x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_splitAtChar'46_'35selFP44'35first (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_splitAtChar'46_'35selFP44'35first(x)(st))(i)(xs)(st)
c_splitAtChar'46_'35selFP44'35first x st = Curry.RunTimeSystem.patternFail("XML.splitAtChar._#selFP44#first")(x)



c_splitAtChar'46_'35selFP45'35rest :: (Curry t231) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t231) (Curry.Module.Prelude.List t231)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t231
c_splitAtChar'46_'35selFP45'35rest x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_splitAtChar'46_'35selFP45'35rest (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_splitAtChar'46_'35selFP45'35rest(x)(st))(i)(xs)(st)
c_splitAtChar'46_'35selFP45'35rest x st = Curry.RunTimeSystem.patternFail("XML.splitAtChar._#selFP45#rest")(x)



c_textOfXml :: (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_textOfXml x1 st = Curry.Module.Prelude.c_concat(Curry.Module.List.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_not))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_null))(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.XML.c_textOfXml'46textOfXmlItem'46255))(x1)(st))(st))(st))(st)



c_textOfXml'46textOfXmlItem'46255 :: Curry.Module.XML.C_XmlExp -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_textOfXml'46textOfXmlItem'46255 x1@(Curry.Module.XML.C_XText x2) st = x2
c_textOfXml'46textOfXmlItem'46255 x1@(Curry.Module.XML.C_XElem x3 x4 x5) st = Curry.Module.XML.c_textOfXml(x5)(st)
c_textOfXml'46textOfXmlItem'46255 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_textOfXml'46textOfXmlItem'46255(x)(st))(i)(xs)(st)
c_textOfXml'46textOfXmlItem'46255 x st = Curry.RunTimeSystem.patternFail("XML.textOfXml.textOfXmlItem.255")(x)



c_splitAtChar_case_0 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(x4)
c_splitAtChar_case_0 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.XML.c_splitAtChar(x1)(x4)(st)} in Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x3)(Curry.Module.XML.c_splitAtChar'46_'35selFP44'35first(x5)(st)))(Curry.Module.XML.c_splitAtChar'46_'35selFP45'35rest(x5)(st))
c_splitAtChar_case_0 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_splitAtChar_case_0(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_splitAtChar_case_0 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("XML.splitAtChar_case_0")(x)



c_parseAttrs_case_2 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.XML.c_splitAtChar(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(x2)(x3))(st)} in let {x7 = Curry.Module.XML.c_splitAtChar(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.c_tail(Curry.Module.XML.c_parseAttrs'46_'35selFP42'35rest1(x4)(st))(st))(st)} in let {x10 = Curry.Module.XML.c_parseAttrs(Curry.Module.Prelude.c_apply(Curry.Module.XML.c_dropBlanks(st))(Curry.Module.XML.c_parseAttrs'46_'35selFP40'35rest2(x7)(st))(st))(st)} in Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.XML.c_parseAttrs'46_'35selFP41'35name(x4)(st))(Curry.Module.XML.c_xmlUnquoteSpecials(Curry.Module.XML.c_parseAttrs'46_'35selFP39'35value(x7)(st))(st)))(Curry.Module.XML.c_parseAttrs'46_'35selFP37'35rem_attrs(x10)(st)))(Curry.Module.XML.c_parseAttrs'46_'35selFP38'35rem_inp(x10)(st))
c_parseAttrs_case_2 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_parseAttrs_case_1(x2)(x3)(Curry.Module.Prelude.c_otherwise(st))(st)
c_parseAttrs_case_2 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseAttrs_case_2(x2)(x3)(x)(st))(i)(xs)(st)
c_parseAttrs_case_2 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.parseAttrs_case_2")(x)



c_parseAttrs_case_1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)((Curry.Module.Prelude.:<)(x2)(x3))
c_parseAttrs_case_1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseAttrs_case_1(x2)(x3)(x)(st))(i)(xs)(st)
c_parseAttrs_case_1 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.parseAttrs_case_1")(x)



c_scanXmlProcInstr_case_3 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.XML.c_scanXmlString(Curry.Module.Prelude.c_tail(x3)(st))(st)
c_scanXmlProcInstr_case_3 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_scanXmlProcInstr(x3)(st)
c_scanXmlProcInstr_case_3 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlProcInstr_case_3(x2)(x3)(x)(st))(i)(xs)(st)
c_scanXmlProcInstr_case_3 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.scanXmlProcInstr_case_3")(x)



c_dropCData_case_6 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_tail(Curry.Module.Prelude.c_dropWhile(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_47_61))(Curry.Module.Prelude.C_Char(']'))))(x3)(st))(st)
c_dropCData_case_6 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_dropCData_case_5(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('>'))(st))(st)
c_dropCData_case_6 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_dropCData_case_6(x2)(x3)(x)(st))(i)(xs)(st)
c_dropCData_case_6 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.dropCData_case_6")(x)



c_dropCData_case_5 x2 x3 x4@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x2)(x3)
c_dropCData_case_5 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_dropCData_case_4(x3)(Curry.Module.Prelude.c_otherwise(st))(st)
c_dropCData_case_5 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_dropCData_case_5(x2)(x3)(x)(st))(i)(xs)(st)
c_dropCData_case_5 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.dropCData_case_5")(x)



c_dropCData_case_4 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.XML.c_dropCData(x3)(st)
c_dropCData_case_4 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_dropCData_case_4(x3)(x)(st))(i)(xs)(st)
c_dropCData_case_4 x3 x st = Curry.RunTimeSystem.patternFail("XML.dropCData_case_4")(x)



c_scanXmlCData_case_7 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.XML.c_scanXmlString(Curry.Module.Prelude.c_tail(x2)(st))(st)
c_scanXmlCData_case_7 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_scanXmlCData(x2)(st)
c_scanXmlCData_case_7 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlCData_case_7(x2)(x)(st))(i)(xs)(st)
c_scanXmlCData_case_7 x2 x st = Curry.RunTimeSystem.patternFail("XML.scanXmlCData_case_7")(x)



c_scanXmlComment_case_8 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.XML.c_scanXmlString(Curry.Module.Prelude.c_drop(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x3)(st))(st)
c_scanXmlComment_case_8 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_scanXmlComment(x3)(st)
c_scanXmlComment_case_8 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlComment_case_8(x2)(x3)(x)(st))(i)(xs)(st)
c_scanXmlComment_case_8 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.scanXmlComment_case_8")(x)



c_scanXmlElemName_case_13 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.XML.C_XElem((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))(x1))(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(Curry.Module.XML.c_scanXmlString(x4)(st))
c_scanXmlElemName_case_13 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_scanXmlElemName_case_12(x1)(x3)(x4)(Curry.Module.Char.c_isSpace(x3)(st))(st)
c_scanXmlElemName_case_13 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlElemName_case_13(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_scanXmlElemName_case_13 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("XML.scanXmlElemName_case_13")(x)



c_scanXmlElemName_case_12 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.XML.c_parseAttrs(Curry.Module.Prelude.c_apply(Curry.Module.XML.c_dropBlanks(st))(x4)(st))(st)} in let {x6 = Curry.Module.XML.c_scanXmlElemName'46_'35selFP32'35attrs(x5)(st)} in let {x7 = Curry.Module.XML.c_scanXmlElemName'46_'35selFP33'35rest(x5)(st)} in Curry.Module.XML.c_scanXmlElemName_case_11(x1)(x6)(x7)(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_head(x7)(st))(Curry.Module.Prelude.C_Char('/'))(st))(st)
c_scanXmlElemName_case_12 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_scanXmlElemName_case_10(x1)(x3)(x4)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('/'))(st))(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_head(x4)(st))(Curry.Module.Prelude.C_Char('>'))(st))(st))(st)
c_scanXmlElemName_case_12 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlElemName_case_12(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_scanXmlElemName_case_12 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("XML.scanXmlElemName_case_12")(x)



c_scanXmlElemName_case_10 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.XML.C_XElem(x1)(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(Curry.Module.XML.c_scanXmlString(Curry.Module.Prelude.c_tail(x4)(st))(st))
c_scanXmlElemName_case_10 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_scanXmlElemName_case_9(x1)(x3)(x4)(Curry.Module.Prelude.c_otherwise(st))(st)
c_scanXmlElemName_case_10 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlElemName_case_10(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_scanXmlElemName_case_10 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("XML.scanXmlElemName_case_10")(x)



c_scanXmlElemName_case_9 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.XML.c_scanXmlElemName(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(st))(x4)(st)
c_scanXmlElemName_case_9 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlElemName_case_9(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_scanXmlElemName_case_9 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("XML.scanXmlElemName_case_9")(x)



c_scanXmlElemName_case_11 x1 x6 x7 x8@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.XML.C_XElem(x1)(x6)(Curry.Module.Prelude.List))(Curry.Module.XML.c_scanXmlString(Curry.Module.Prelude.c_drop(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x7)(st))(st))
c_scanXmlElemName_case_11 x1 x6 x7 x8@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(Curry.Module.XML.C_XElem((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))(x1))(x6)(Curry.Module.Prelude.List))(Curry.Module.XML.c_scanXmlString(Curry.Module.Prelude.c_tail(x7)(st))(st))
c_scanXmlElemName_case_11 x1 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlElemName_case_11(x1)(x6)(x7)(x)(st))(i)(xs)(st)
c_scanXmlElemName_case_11 x1 x6 x7 x st = Curry.RunTimeSystem.patternFail("XML.scanXmlElemName_case_11")(x)



c_scanXmlElem_case_17 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.XML.c_scanXmlElem_case_16(x3)(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_take(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x3)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))(Curry.Module.Prelude.List)))(st))(st)
c_scanXmlElem_case_17 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_scanXmlElem_case_15(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('?'))(st))(st)
c_scanXmlElem_case_17 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlElem_case_17(x2)(x3)(x)(st))(i)(xs)(st)
c_scanXmlElem_case_17 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.scanXmlElem_case_17")(x)



c_scanXmlElem_case_15 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.XML.c_scanXmlProcInstr(x3)(st)
c_scanXmlElem_case_15 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_scanXmlElem_case_14(x2)(x3)(Curry.Module.Prelude.c_otherwise(st))(st)
c_scanXmlElem_case_15 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlElem_case_15(x2)(x3)(x)(st))(i)(xs)(st)
c_scanXmlElem_case_15 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.scanXmlElem_case_15")(x)



c_scanXmlElem_case_14 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.XML.c_scanXmlElemName((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(x3)(st)
c_scanXmlElem_case_14 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlElem_case_14(x2)(x3)(x)(st))(i)(xs)(st)
c_scanXmlElem_case_14 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.scanXmlElem_case_14")(x)



c_scanXmlElem_case_16 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.XML.c_scanXmlComment(Curry.Module.Prelude.c_drop(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x3)(st))(st)
c_scanXmlElem_case_16 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_scanXmlCData(x3)(st)
c_scanXmlElem_case_16 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlElem_case_16(x3)(x)(st))(i)(xs)(st)
c_scanXmlElem_case_16 x3 x st = Curry.RunTimeSystem.patternFail("XML.scanXmlElem_case_16")(x)



c_scanXmlText_case_21 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)((Curry.Module.Prelude.:<)(x2)(x3))
c_scanXmlText_case_21 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_scanXmlText_case_20(x2)(x3)(Curry.Module.Char.c_isSpace(x2)(st))(st)
c_scanXmlText_case_21 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlText_case_21(x2)(x3)(x)(st))(i)(xs)(st)
c_scanXmlText_case_21 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.scanXmlText_case_21")(x)



c_scanXmlText_case_20 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.XML.c_scanXmlText(Curry.Module.Prelude.c_apply(Curry.Module.XML.c_dropBlanks(st))(x3)(st))(st)} in let {x5 = Curry.Module.XML.c_scanXmlText'46_'35selFP26'35txt(x4)(st)} in Curry.Module.Prelude.T2(Curry.Module.XML.c_scanXmlText_case_19(x5)(Curry.Module.Prelude.c_null(x5)(st))(st))(Curry.Module.XML.c_scanXmlText'46_'35selFP27'35rem(x4)(st))
c_scanXmlText_case_20 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_scanXmlText_case_18(x2)(x3)(Curry.Module.Prelude.c_otherwise(st))(st)
c_scanXmlText_case_20 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlText_case_20(x2)(x3)(x)(st))(i)(xs)(st)
c_scanXmlText_case_20 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.scanXmlText_case_20")(x)



c_scanXmlText_case_18 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.XML.c_scanXmlText(x3)(st)} in Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x2)(Curry.Module.XML.c_scanXmlText'46_'35selFP29'35txt(x7)(st)))(Curry.Module.XML.c_scanXmlText'46_'35selFP30'35rem(x7)(st))
c_scanXmlText_case_18 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlText_case_18(x2)(x3)(x)(st))(i)(xs)(st)
c_scanXmlText_case_18 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.scanXmlText_case_18")(x)



c_scanXmlText_case_19 x5 x6@Curry.Module.Prelude.C_True st = x5
c_scanXmlText_case_19 x5 x6@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(x5)
c_scanXmlText_case_19 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlText_case_19(x5)(x)(st))(i)(xs)(st)
c_scanXmlText_case_19 x5 x st = Curry.RunTimeSystem.patternFail("XML.scanXmlText_case_19")(x)



c_scanXmlString'46scanXml'46191_case_22 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.XML.c_scanXmlElem(x3)(st)
c_scanXmlString'46scanXml'46191_case_22 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x4 = Curry.Module.XML.c_scanXmlText((Curry.Module.Prelude.:<)(x2)(x3))(st)} in (Curry.Module.Prelude.:<)(Curry.Module.XML.C_XText(Curry.Module.XML.c_scanXmlString'46scanXml'46191'46_'35selFP23'35initxt(x4)(st)))(Curry.Module.XML.c_scanXmlString'46scanXml'46191(Curry.Module.XML.c_scanXmlString'46scanXml'46191'46_'35selFP24'35remtag(x4)(st))(st))
c_scanXmlString'46scanXml'46191_case_22 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_scanXmlString'46scanXml'46191_case_22(x2)(x3)(x)(st))(i)(xs)(st)
c_scanXmlString'46scanXml'46191_case_22 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.scanXmlString.scanXml.191_case_22")(x)



c_parseXmlTokens_case_28 x2 x4 x3@(Curry.Module.XML.C_XText x5) st = let {x6 = Curry.Module.XML.c_parseXmlTokens(x4)(x2)(st)} in Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.XML.C_XText(Curry.Module.XML.c_xmlUnquoteSpecials(x5)(st)))(Curry.Module.XML.c_parseXmlTokens'46_'35selFP8'35xexps(x6)(st)))(Curry.Module.XML.c_parseXmlTokens'46_'35selFP9'35rem_xtokens(x6)(st))
c_parseXmlTokens_case_28 x2 x4 x3@(Curry.Module.XML.C_XElem x9 x10 x11) st = Curry.Module.XML.c_parseXmlTokens_case_27(x2)(x4)(x10)(x11)(x9)(st)
c_parseXmlTokens_case_28 x2 x4 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseXmlTokens_case_28(x2)(x4)(x)(st))(i)(xs)(st)
c_parseXmlTokens_case_28 x2 x4 x st = Curry.RunTimeSystem.patternFail("XML.parseXmlTokens_case_28")(x)



c_parseXmlTokens_case_27 x2 x4 x10 x11 x9@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.XML.c_parseXmlTokens_case_26(x2)(x4)(x10)(x11)(x12)(x13)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x12)(Curry.Module.Prelude.C_Char('<'))(st))(Curry.Module.Prelude.op_47_61(Curry.Module.Prelude.c_head(x13)(st))(Curry.Module.Prelude.C_Char('/'))(st))(st))(st)
c_parseXmlTokens_case_27 x2 x4 x10 x11 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseXmlTokens_case_27(x2)(x4)(x10)(x11)(x)(st))(i)(xs)(st)
c_parseXmlTokens_case_27 x2 x4 x10 x11 x st = Curry.RunTimeSystem.patternFail("XML.parseXmlTokens_case_27")(x)



c_parseXmlTokens_case_26 x2 x4 x10 x11 x12 x13 x14@Curry.Module.Prelude.C_True st = let {x14 = Curry.Module.XML.c_parseXmlTokens(x4)(Curry.Module.Prelude.C_Just(x13))(st)} in let {x17 = Curry.Module.XML.c_parseXmlTokens(Curry.Module.XML.c_parseXmlTokens'46_'35selFP15'35xtokens1(x14)(st))(x2)(st)} in Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.XML.C_XElem(x13)(x10)(Curry.Module.XML.c_parseXmlTokens'46_'35selFP14'35xexps1(x14)(st)))(Curry.Module.XML.c_parseXmlTokens'46_'35selFP12'35xexps(x17)(st)))(Curry.Module.XML.c_parseXmlTokens'46_'35selFP13'35rem_xtokens(x17)(st))
c_parseXmlTokens_case_26 x2 x4 x10 x11 x12 x13 x14@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_parseXmlTokens_case_25(x2)(x4)(x10)(x11)(x12)(x13)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x12)(Curry.Module.Prelude.C_Char('<'))(st))(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_head(x13)(st))(Curry.Module.Prelude.C_Char('/'))(st))(st))(st)
c_parseXmlTokens_case_26 x2 x4 x10 x11 x12 x13 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseXmlTokens_case_26(x2)(x4)(x10)(x11)(x12)(x13)(x)(st))(i)(xs)(st)
c_parseXmlTokens_case_26 x2 x4 x10 x11 x12 x13 x st = Curry.RunTimeSystem.patternFail("XML.parseXmlTokens_case_26")(x)



c_parseXmlTokens_case_25 x2 x4 x10 x11 x12 x13 x14@Curry.Module.Prelude.C_True st = Curry.Module.XML.c_parseXmlTokens_case_24(x2)(x4)(x10)(x11)(x13)(Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.C_False)(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_61_61))(Curry.Module.Prelude.c_tail(x13)(st))))(x2)(st))(st)
c_parseXmlTokens_case_25 x2 x4 x10 x11 x12 x13 x14@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_parseXmlTokens_case_23(x2)(x4)(x10)(x11)(x12)(x13)(Curry.Module.Prelude.c_otherwise(st))(st)
c_parseXmlTokens_case_25 x2 x4 x10 x11 x12 x13 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseXmlTokens_case_25(x2)(x4)(x10)(x11)(x12)(x13)(x)(st))(i)(xs)(st)
c_parseXmlTokens_case_25 x2 x4 x10 x11 x12 x13 x st = Curry.RunTimeSystem.patternFail("XML.parseXmlTokens_case_25")(x)



c_parseXmlTokens_case_23 x2 x4 x10 x11 x12 x13 x14@Curry.Module.Prelude.C_True st = let {x23 = Curry.Module.XML.c_parseXmlTokens(x4)(x2)(st)} in Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.XML.C_XElem((Curry.Module.Prelude.:<)(x12)(x13))(x10)(x11))(Curry.Module.XML.c_parseXmlTokens'46_'35selFP20'35xexps(x23)(st)))(Curry.Module.XML.c_parseXmlTokens'46_'35selFP21'35rem_xtokens(x23)(st))
c_parseXmlTokens_case_23 x2 x4 x10 x11 x12 x13 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseXmlTokens_case_23(x2)(x4)(x10)(x11)(x12)(x13)(x)(st))(i)(xs)(st)
c_parseXmlTokens_case_23 x2 x4 x10 x11 x12 x13 x st = Curry.RunTimeSystem.patternFail("XML.parseXmlTokens_case_23")(x)



c_parseXmlTokens_case_24 x2 x4 x10 x11 x13 x14@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(x4)
c_parseXmlTokens_case_24 x2 x4 x10 x11 x13 x14@Curry.Module.Prelude.C_False st = let {x20 = Curry.Module.XML.c_parseXmlTokens(x4)(x2)(st)} in Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.XML.C_XElem(x13)(x10)(x11))(Curry.Module.XML.c_parseXmlTokens'46_'35selFP17'35xexps(x20)(st)))(Curry.Module.XML.c_parseXmlTokens'46_'35selFP18'35rem_xtokens(x20)(st))
c_parseXmlTokens_case_24 x2 x4 x10 x11 x13 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseXmlTokens_case_24(x2)(x4)(x10)(x11)(x13)(x)(st))(i)(xs)(st)
c_parseXmlTokens_case_24 x2 x4 x10 x11 x13 x st = Curry.RunTimeSystem.patternFail("XML.parseXmlTokens_case_24")(x)



c_parseXmlTokens_case_29 x2@Curry.Module.Prelude.C_Nothing st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)
c_parseXmlTokens_case_29 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_parseXmlTokens_case_29(x)(st))(i)(xs)(st)
c_parseXmlTokens_case_29 x st = Curry.RunTimeSystem.patternFail("XML.parseXmlTokens_case_29")(x)



c_readXmlFile'46_'35lambda3_case_31 x1 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_error(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('X'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))(st))(st))(st)
c_readXmlFile'46_'35lambda3_case_31 x1 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_readXmlFile'46_'35lambda3_case_30(x1)(x3)(Curry.Module.Prelude.op_47_61(Curry.Module.Prelude.c_tail(x3)(st))(Curry.Module.Prelude.List)(st))(st)
c_readXmlFile'46_'35lambda3_case_31 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_readXmlFile'46_'35lambda3_case_31(x1)(x3)(x)(st))(i)(xs)(st)
c_readXmlFile'46_'35lambda3_case_31 x1 x3 x st = Curry.RunTimeSystem.patternFail("XML.readXmlFile._#lambda3_case_31")(x)



c_readXmlFile'46_'35lambda3_case_30 x1 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_error(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('X'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))))))(st))(st))(st)
c_readXmlFile'46_'35lambda3_case_30 x1 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.c_head(x3)(st))(st)
c_readXmlFile'46_'35lambda3_case_30 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_readXmlFile'46_'35lambda3_case_30(x1)(x3)(x)(st))(i)(xs)(st)
c_readXmlFile'46_'35lambda3_case_30 x1 x3 x st = Curry.RunTimeSystem.patternFail("XML.readXmlFile._#lambda3_case_30")(x)



c_unquoteUnicode_case_35 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.XML.c_unquoteUnicode_case_34(x3)(st)
c_unquoteUnicode_case_35 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_unquoteUnicode_case_32(x2)(x3)(Curry.Module.Prelude.c_otherwise(st))(st)
c_unquoteUnicode_case_35 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_unquoteUnicode_case_35(x2)(x3)(x)(st))(i)(xs)(st)
c_unquoteUnicode_case_35 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.unquoteUnicode_case_35")(x)



c_unquoteUnicode_case_32 x2 x3 x4@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(x2)(x3))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))(Curry.Module.Prelude.List))(st))
c_unquoteUnicode_case_32 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_unquoteUnicode_case_32(x2)(x3)(x)(st))(i)(xs)(st)
c_unquoteUnicode_case_32 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.unquoteUnicode_case_32")(x)



c_unquoteUnicode_case_34 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.XML.c_unquoteUnicode_case_33(x3)(x4)(x5)(Curry.Module.Prelude.op_61_61(x4)(Curry.Module.Prelude.C_Char('x'))(st))(st)
c_unquoteUnicode_case_34 x3@Curry.Module.Prelude.List st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_chr(Curry.Module.Read.c_readInt(x3)(st))(st))(Curry.Module.Prelude.List)
c_unquoteUnicode_case_34 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_unquoteUnicode_case_34(x)(st))(i)(xs)(st)
c_unquoteUnicode_case_34 x st = Curry.RunTimeSystem.patternFail("XML.unquoteUnicode_case_34")(x)



c_unquoteUnicode_case_33 x3 x4 x5 x6@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_chr(Curry.Module.Read.c_readHex(x5)(st))(st))(Curry.Module.Prelude.List)
c_unquoteUnicode_case_33 x3 x4 x5 x6@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_chr(Curry.Module.Read.c_readInt(x3)(st))(st))(Curry.Module.Prelude.List)
c_unquoteUnicode_case_33 x3 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_unquoteUnicode_case_33(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c_unquoteUnicode_case_33 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("XML.unquoteUnicode_case_33")(x)



c_xmlUnquoteSpecial_case_48 x1 x2 x3@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))(Curry.Module.XML.c_xmlUnquoteSpecials(x2)(st))
c_xmlUnquoteSpecial_case_48 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_xmlUnquoteSpecial_case_47(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))(st))(st)
c_xmlUnquoteSpecial_case_48 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_xmlUnquoteSpecial_case_48(x1)(x2)(x)(st))(i)(xs)(st)
c_xmlUnquoteSpecial_case_48 x1 x2 x st = Curry.RunTimeSystem.patternFail("XML.xmlUnquoteSpecial_case_48")(x)



c_xmlUnquoteSpecial_case_47 x1 x2 x3@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))(Curry.Module.XML.c_xmlUnquoteSpecials(x2)(st))
c_xmlUnquoteSpecial_case_47 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_xmlUnquoteSpecial_case_46(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))(Curry.Module.Prelude.List))))(st))(st)
c_xmlUnquoteSpecial_case_47 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_xmlUnquoteSpecial_case_47(x1)(x2)(x)(st))(i)(xs)(st)
c_xmlUnquoteSpecial_case_47 x1 x2 x st = Curry.RunTimeSystem.patternFail("XML.xmlUnquoteSpecial_case_47")(x)



c_xmlUnquoteSpecial_case_46 x1 x2 x3@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))(Curry.Module.XML.c_xmlUnquoteSpecials(x2)(st))
c_xmlUnquoteSpecial_case_46 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_xmlUnquoteSpecial_case_45(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('q'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))(st))(st)
c_xmlUnquoteSpecial_case_46 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_xmlUnquoteSpecial_case_46(x1)(x2)(x)(st))(i)(xs)(st)
c_xmlUnquoteSpecial_case_46 x1 x2 x st = Curry.RunTimeSystem.patternFail("XML.xmlUnquoteSpecial_case_46")(x)



c_xmlUnquoteSpecial_case_45 x1 x2 x3@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.XML.c_xmlUnquoteSpecials(x2)(st))
c_xmlUnquoteSpecial_case_45 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_xmlUnquoteSpecial_case_44(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))(st))(st)
c_xmlUnquoteSpecial_case_45 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_xmlUnquoteSpecial_case_45(x1)(x2)(x)(st))(i)(xs)(st)
c_xmlUnquoteSpecial_case_45 x1 x2 x st = Curry.RunTimeSystem.patternFail("XML.xmlUnquoteSpecial_case_45")(x)



c_xmlUnquoteSpecial_case_44 x1 x2 x3@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))(Curry.Module.XML.c_xmlUnquoteSpecials(x2)(st))
c_xmlUnquoteSpecial_case_44 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_xmlUnquoteSpecial_case_43(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))(st))(st)
c_xmlUnquoteSpecial_case_44 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_xmlUnquoteSpecial_case_44(x1)(x2)(x)(st))(i)(xs)(st)
c_xmlUnquoteSpecial_case_44 x1 x2 x st = Curry.RunTimeSystem.patternFail("XML.xmlUnquoteSpecial_case_44")(x)



c_xmlUnquoteSpecial_case_43 x1 x2 x3@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(st))(Curry.Module.XML.c_xmlUnquoteSpecials(x2)(st))
c_xmlUnquoteSpecial_case_43 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_xmlUnquoteSpecial_case_42(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))(st))(st)
c_xmlUnquoteSpecial_case_43 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_xmlUnquoteSpecial_case_43(x1)(x2)(x)(st))(i)(xs)(st)
c_xmlUnquoteSpecial_case_43 x1 x2 x st = Curry.RunTimeSystem.patternFail("XML.xmlUnquoteSpecial_case_43")(x)



c_xmlUnquoteSpecial_case_42 x1 x2 x3@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(st))(Curry.Module.XML.c_xmlUnquoteSpecials(x2)(st))
c_xmlUnquoteSpecial_case_42 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_xmlUnquoteSpecial_case_41(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))(st))(st)
c_xmlUnquoteSpecial_case_42 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_xmlUnquoteSpecial_case_42(x1)(x2)(x)(st))(i)(xs)(st)
c_xmlUnquoteSpecial_case_42 x1 x2 x st = Curry.RunTimeSystem.patternFail("XML.xmlUnquoteSpecial_case_42")(x)



c_xmlUnquoteSpecial_case_41 x1 x2 x3@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(st))(Curry.Module.XML.c_xmlUnquoteSpecials(x2)(st))
c_xmlUnquoteSpecial_case_41 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_xmlUnquoteSpecial_case_40(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))(st))(st)
c_xmlUnquoteSpecial_case_41 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_xmlUnquoteSpecial_case_41(x1)(x2)(x)(st))(i)(xs)(st)
c_xmlUnquoteSpecial_case_41 x1 x2 x st = Curry.RunTimeSystem.patternFail("XML.xmlUnquoteSpecial_case_41")(x)



c_xmlUnquoteSpecial_case_40 x1 x2 x3@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(st))(Curry.Module.XML.c_xmlUnquoteSpecials(x2)(st))
c_xmlUnquoteSpecial_case_40 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_xmlUnquoteSpecial_case_39(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))(st))(st)
c_xmlUnquoteSpecial_case_40 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_xmlUnquoteSpecial_case_40(x1)(x2)(x)(st))(i)(xs)(st)
c_xmlUnquoteSpecial_case_40 x1 x2 x st = Curry.RunTimeSystem.patternFail("XML.xmlUnquoteSpecial_case_40")(x)



c_xmlUnquoteSpecial_case_39 x1 x2 x3@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(st))(Curry.Module.XML.c_xmlUnquoteSpecials(x2)(st))
c_xmlUnquoteSpecial_case_39 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_xmlUnquoteSpecial_case_38(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('U'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))(st))(st)
c_xmlUnquoteSpecial_case_39 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_xmlUnquoteSpecial_case_39(x1)(x2)(x)(st))(i)(xs)(st)
c_xmlUnquoteSpecial_case_39 x1 x2 x st = Curry.RunTimeSystem.patternFail("XML.xmlUnquoteSpecial_case_39")(x)



c_xmlUnquoteSpecial_case_38 x1 x2 x3@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(st))(Curry.Module.XML.c_xmlUnquoteSpecials(x2)(st))
c_xmlUnquoteSpecial_case_38 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_xmlUnquoteSpecial_case_37(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('z'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))(Curry.Module.Prelude.List))))))(st))(st)
c_xmlUnquoteSpecial_case_38 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_xmlUnquoteSpecial_case_38(x1)(x2)(x)(st))(i)(xs)(st)
c_xmlUnquoteSpecial_case_38 x1 x2 x st = Curry.RunTimeSystem.patternFail("XML.xmlUnquoteSpecial_case_38")(x)



c_xmlUnquoteSpecial_case_37 x1 x2 x3@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(st))(Curry.Module.XML.c_xmlUnquoteSpecials(x2)(st))
c_xmlUnquoteSpecial_case_37 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_xmlUnquoteSpecial_case_36(x1)(x2)(Curry.Module.Prelude.c_otherwise(st))(st)
c_xmlUnquoteSpecial_case_37 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_xmlUnquoteSpecial_case_37(x1)(x2)(x)(st))(i)(xs)(st)
c_xmlUnquoteSpecial_case_37 x1 x2 x st = Curry.RunTimeSystem.patternFail("XML.xmlUnquoteSpecial_case_37")(x)



c_xmlUnquoteSpecial_case_36 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43(Curry.Module.XML.c_unquoteUnicode(x1)(st))(Curry.Module.XML.c_xmlUnquoteSpecials(x2)(st))(st)
c_xmlUnquoteSpecial_case_36 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_xmlUnquoteSpecial_case_36(x1)(x2)(x)(st))(i)(xs)(st)
c_xmlUnquoteSpecial_case_36 x1 x2 x st = Curry.RunTimeSystem.patternFail("XML.xmlUnquoteSpecial_case_36")(x)



c_xmlUnquoteSpecials_case_50 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.XML.c_splitAtChar(Curry.Module.Prelude.C_Char(';'))(x3)(st)} in Curry.Module.XML.c_xmlUnquoteSpecial(Curry.Module.XML.c_xmlUnquoteSpecials'46_'35selFP5'35special(x4)(st))(Curry.Module.XML.c_xmlUnquoteSpecials'46_'35selFP6'35rest(x4)(st))(st)
c_xmlUnquoteSpecials_case_50 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_xmlUnquoteSpecials_case_49(x2)(x3)(Curry.Module.Prelude.c_otherwise(st))(st)
c_xmlUnquoteSpecials_case_50 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_xmlUnquoteSpecials_case_50(x2)(x3)(x)(st))(i)(xs)(st)
c_xmlUnquoteSpecials_case_50 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.xmlUnquoteSpecials_case_50")(x)



c_xmlUnquoteSpecials_case_49 x2 x3 x4@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x2)(Curry.Module.XML.c_xmlUnquoteSpecials(x3)(st))
c_xmlUnquoteSpecials_case_49 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_xmlUnquoteSpecials_case_49(x2)(x3)(x)(st))(i)(xs)(st)
c_xmlUnquoteSpecials_case_49 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.xmlUnquoteSpecials_case_49")(x)



c_showXmlExp'46_'35selFP3'35s_case_52 x3 x2@(Curry.Module.XML.C_XText x4) st = Curry.Module.XML.c_showXmlExp'46_'35selFP3'35s_case_51(x4)(x3)(st)
c_showXmlExp'46_'35selFP3'35s_case_52 x3 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_showXmlExp'46_'35selFP3'35s_case_52(x3)(x)(st))(i)(xs)(st)
c_showXmlExp'46_'35selFP3'35s_case_52 x3 x st = Curry.RunTimeSystem.patternFail("XML.showXmlExp._#selFP3#s_case_52")(x)



c_showXmlExp'46_'35selFP3'35s_case_51 x4 x3@Curry.Module.Prelude.List st = x4
c_showXmlExp'46_'35selFP3'35s_case_51 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_showXmlExp'46_'35selFP3'35s_case_51(x4)(x)(st))(i)(xs)(st)
c_showXmlExp'46_'35selFP3'35s_case_51 x4 x st = Curry.RunTimeSystem.patternFail("XML.showXmlExp._#selFP3#s_case_51")(x)



c_showXmlExp_case_54 x1 x2 x5 x7 x8@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))))
c_showXmlExp_case_54 x1 x2 x5 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_showXmlExp_case_53(x1)(x2)(x5)(x7)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_length(x7)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.XML.c_isXText(Curry.Module.Prelude.c_head(x7)(st))(st))(st))(st)
c_showXmlExp_case_54 x1 x2 x5 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_showXmlExp_case_54(x1)(x2)(x5)(x7)(x)(st))(i)(xs)(st)
c_showXmlExp_case_54 x1 x2 x5 x7 x st = Curry.RunTimeSystem.patternFail("XML.showXmlExp_case_54")(x)



c_showXmlExp_case_53 x1 x2 x5 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_apply(x2)(Curry.Module.XML.c_showXmlExp'46_'35selFP3'35s(x7)(st))(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.op_43_43(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))(st))(st))(st))(st)
c_showXmlExp_case_53 x1 x2 x5 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.op_43_43(Curry.Module.XML.c_showXmlExps(Curry.Module.Prelude.op_43(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(x7)(x2)(st))(Curry.Module.Prelude.op_43_43(Curry.Module.XML.c_xtab(x1)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.op_43_43(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))(st))(st))(st))(st))(st)
c_showXmlExp_case_53 x1 x2 x5 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_showXmlExp_case_53(x1)(x2)(x5)(x7)(x)(st))(i)(xs)(st)
c_showXmlExp_case_53 x1 x2 x5 x7 x st = Curry.RunTimeSystem.patternFail("XML.showXmlExp_case_53")(x)



c_showXmlDocWithParams_case_55 x1 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('Y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))(Curry.Module.Prelude.op_43_43(x3)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('Y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List))))))))))(Curry.Module.Prelude.op_43_43(Curry.Module.XML.c_lookupDtdUrl(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))(st))(st))(st))(st)
c_showXmlDocWithParams_case_55 x1 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.List
c_showXmlDocWithParams_case_55 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_showXmlDocWithParams_case_55(x1)(x3)(x)(st))(i)(xs)(st)
c_showXmlDocWithParams_case_55 x1 x3 x st = Curry.RunTimeSystem.patternFail("XML.showXmlDocWithParams_case_55")(x)



c_showXmlDocWithParams_case_56 x1 x2@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))(Curry.Module.Prelude.List))
c_showXmlDocWithParams_case_56 x1 x2@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))
c_showXmlDocWithParams_case_56 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_showXmlDocWithParams_case_56(x1)(x)(st))(i)(xs)(st)
c_showXmlDocWithParams_case_56 x1 x st = Curry.RunTimeSystem.patternFail("XML.showXmlDocWithParams_case_56")(x)



c_hasDtdUrl_case_57 x3 x2@(Curry.Module.XML.C_DtdUrl x4) st = Curry.Module.Prelude.C_True
c_hasDtdUrl_case_57 x3 x2@(Curry.Module.XML.C_Enc x5) st = Curry.Module.XML.c_hasDtdUrl(x3)(st)
c_hasDtdUrl_case_57 x3 (Curry.Module.XML.C_XmlDocParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_hasDtdUrl_case_57(x3)(x)(st))(i)(xs)(st)
c_hasDtdUrl_case_57 x3 x st = Curry.RunTimeSystem.patternFail("XML.hasDtdUrl_case_57")(x)



c_lookupDtdUrl_case_58 x3 x2@(Curry.Module.XML.C_Enc x4) st = Curry.Module.XML.c_lookupDtdUrl(x3)(st)
c_lookupDtdUrl_case_58 x3 x2@(Curry.Module.XML.C_DtdUrl x5) st = x5
c_lookupDtdUrl_case_58 x3 (Curry.Module.XML.C_XmlDocParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_lookupDtdUrl_case_58(x3)(x)(st))(i)(xs)(st)
c_lookupDtdUrl_case_58 x3 x st = Curry.RunTimeSystem.patternFail("XML.lookupDtdUrl_case_58")(x)



c_lookupEncoding_case_59 x3 x2@(Curry.Module.XML.C_Enc x4) st = x4
c_lookupEncoding_case_59 x3 x2@(Curry.Module.XML.C_DtdUrl x5) st = Curry.Module.XML.c_lookupEncoding(x3)(st)
c_lookupEncoding_case_59 x3 (Curry.Module.XML.C_XmlDocParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_lookupEncoding_case_59(x3)(x)(st))(i)(xs)(st)
c_lookupEncoding_case_59 x3 x st = Curry.RunTimeSystem.patternFail("XML.lookupEncoding_case_59")(x)



c_iso88591Encoding_case_60 x2 x3 x4@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x2)(Curry.Module.XML.c_iso88591Encoding(x3)(st))
c_iso88591Encoding_case_60 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_43_43(Curry.Module.XML.c_standardEncoding((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(st))(Curry.Module.XML.c_iso88591Encoding(x3)(st))(st)
c_iso88591Encoding_case_60 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_iso88591Encoding_case_60(x2)(x3)(x)(st))(i)(xs)(st)
c_iso88591Encoding_case_60 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.iso88591Encoding_case_60")(x)



c_standardEncoding_case_68 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))(Curry.Module.Prelude.List)))))(Curry.Module.XML.c_standardEncoding(x3)(st))(st)
c_standardEncoding_case_68 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_standardEncoding_case_67(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('>'))(st))(st)
c_standardEncoding_case_68 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_standardEncoding_case_68(x2)(x3)(x)(st))(i)(xs)(st)
c_standardEncoding_case_68 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.standardEncoding_case_68")(x)



c_standardEncoding_case_67 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))(Curry.Module.Prelude.List)))))(Curry.Module.XML.c_standardEncoding(x3)(st))(st)
c_standardEncoding_case_67 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_standardEncoding_case_66(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('&'))(st))(st)
c_standardEncoding_case_67 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_standardEncoding_case_67(x2)(x3)(x)(st))(i)(xs)(st)
c_standardEncoding_case_67 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.standardEncoding_case_67")(x)



c_standardEncoding_case_66 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))(Curry.Module.Prelude.List))))))(Curry.Module.XML.c_standardEncoding(x3)(st))(st)
c_standardEncoding_case_66 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_standardEncoding_case_65(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\"'))(st))(st)
c_standardEncoding_case_66 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_standardEncoding_case_66(x2)(x3)(x)(st))(i)(xs)(st)
c_standardEncoding_case_66 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.standardEncoding_case_66")(x)



c_standardEncoding_case_65 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('q'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))(Curry.Module.Prelude.List)))))))(Curry.Module.XML.c_standardEncoding(x3)(st))(st)
c_standardEncoding_case_65 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_standardEncoding_case_64(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\''))(st))(st)
c_standardEncoding_case_65 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_standardEncoding_case_65(x2)(x3)(x)(st))(i)(xs)(st)
c_standardEncoding_case_65 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.standardEncoding_case_65")(x)



c_standardEncoding_case_64 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))(Curry.Module.Prelude.List)))))))(Curry.Module.XML.c_standardEncoding(x3)(st))(st)
c_standardEncoding_case_64 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_standardEncoding_case_63(x2)(x3)(Curry.Module.Prelude.op_60(Curry.Module.Prelude.c_ord(x2)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))(st))(st)
c_standardEncoding_case_64 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_standardEncoding_case_64(x2)(x3)(x)(st))(i)(xs)(st)
c_standardEncoding_case_64 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.standardEncoding_case_64")(x)



c_standardEncoding_case_63 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('#'))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_show(Curry.Module.Prelude.c_ord(x2)(st))(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))(Curry.Module.Prelude.List))(Curry.Module.XML.c_standardEncoding(x3)(st))(st))(st))(st)
c_standardEncoding_case_63 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_standardEncoding_case_62(x2)(x3)(Curry.Module.Prelude.op_62(Curry.Module.Prelude.c_ord(x2)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))))(st))(st)
c_standardEncoding_case_63 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_standardEncoding_case_63(x2)(x3)(x)(st))(i)(xs)(st)
c_standardEncoding_case_63 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.standardEncoding_case_63")(x)



c_standardEncoding_case_62 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('#'))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_show(Curry.Module.Prelude.c_ord(x2)(st))(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))(Curry.Module.Prelude.List))(Curry.Module.XML.c_standardEncoding(x3)(st))(st))(st))(st)
c_standardEncoding_case_62 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.XML.c_standardEncoding_case_61(x2)(x3)(Curry.Module.Prelude.c_otherwise(st))(st)
c_standardEncoding_case_62 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_standardEncoding_case_62(x2)(x3)(x)(st))(i)(xs)(st)
c_standardEncoding_case_62 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.standardEncoding_case_62")(x)



c_standardEncoding_case_61 x2 x3 x4@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x2)(Curry.Module.XML.c_standardEncoding(x3)(st))
c_standardEncoding_case_61 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.XML.c_standardEncoding_case_61(x2)(x3)(x)(st))(i)(xs)(st)
c_standardEncoding_case_61 x2 x3 x st = Curry.RunTimeSystem.patternFail("XML.standardEncoding_case_61")(x)


