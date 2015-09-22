{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.CompactFlatCurry (module Curry.Module.CompactFlatCurry) where

import Curry.RunTimeSystem
import Curry.Module.Directory
import Curry.Module.Distribution
import Curry.Module.FileGoodies
import Curry.Module.FlatCurry
import Curry.Module.List
import Curry.Module.Maybe
import Curry.Module.Prelude
import Curry.Module.RedBlackTree
import Curry.Module.SetRBT
import Curry.Module.Sort
import Curry.Module.TableRBT
import Curry.Module.Time
import Curry.Module.XML



-- begin included



-- end included

data C_Option = C_Verbose
  | C_Main (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_Exports
  | C_InitFuncs (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
  | C_Required (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_RequiredSpec)
  | C_Import (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_OptionFail Curry.RunTimeSystem.C_Exceptions
  | C_OptionOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.CompactFlatCurry.C_Option)

data C_RequiredSpec = C_AlwaysReq (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
  | C_Requires (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
  | C_RequiredSpecFail Curry.RunTimeSystem.C_Exceptions
  | C_RequiredSpecOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.CompactFlatCurry.C_RequiredSpec)

instance BaseCurry Curry.Module.CompactFlatCurry.C_Option where
  nf f (Curry.Module.CompactFlatCurry.C_Main x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CompactFlatCurry.C_Main(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CompactFlatCurry.C_InitFuncs x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CompactFlatCurry.C_InitFuncs(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CompactFlatCurry.C_Required x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CompactFlatCurry.C_Required(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CompactFlatCurry.C_Import x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CompactFlatCurry.C_Import(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CompactFlatCurry.C_Main x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CompactFlatCurry.C_Main(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CompactFlatCurry.C_InitFuncs x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CompactFlatCurry.C_InitFuncs(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CompactFlatCurry.C_Required x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CompactFlatCurry.C_Required(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CompactFlatCurry.C_Import x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CompactFlatCurry.C_Import(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CompactFlatCurry.C_OptionOr(Curry.RunTimeSystem.mkRef(r)(1)(i))([Curry.Module.CompactFlatCurry.C_Verbose,Curry.Module.CompactFlatCurry.C_Main(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CompactFlatCurry.C_Exports,Curry.Module.CompactFlatCurry.C_InitFuncs(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CompactFlatCurry.C_Required(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CompactFlatCurry.C_Import(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(1)

  failed  = Curry.Module.CompactFlatCurry.C_OptionFail

  branching  = Curry.Module.CompactFlatCurry.C_OptionOr

  consKind (Curry.Module.CompactFlatCurry.C_OptionOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CompactFlatCurry.C_OptionFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CompactFlatCurry.C_OptionFail x) = x

  orRef (Curry.Module.CompactFlatCurry.C_OptionOr x _) = x

  branches (Curry.Module.CompactFlatCurry.C_OptionOr _ x) = x





instance BaseCurry Curry.Module.CompactFlatCurry.C_RequiredSpec where
  nf f (Curry.Module.CompactFlatCurry.C_AlwaysReq x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.CompactFlatCurry.C_AlwaysReq(v1))(state1))(x1)(state0)
  nf f (Curry.Module.CompactFlatCurry.C_Requires x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.CompactFlatCurry.C_Requires(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.CompactFlatCurry.C_AlwaysReq x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.CompactFlatCurry.C_AlwaysReq(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.CompactFlatCurry.C_Requires x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.CompactFlatCurry.C_Requires(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.CompactFlatCurry.C_RequiredSpecOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.CompactFlatCurry.C_AlwaysReq(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.CompactFlatCurry.C_Requires(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.CompactFlatCurry.C_RequiredSpecFail

  branching  = Curry.Module.CompactFlatCurry.C_RequiredSpecOr

  consKind (Curry.Module.CompactFlatCurry.C_RequiredSpecOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.CompactFlatCurry.C_RequiredSpecFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.CompactFlatCurry.C_RequiredSpecFail x) = x

  orRef (Curry.Module.CompactFlatCurry.C_RequiredSpecOr x _) = x

  branches (Curry.Module.CompactFlatCurry.C_RequiredSpecOr _ x) = x





instance Curry Curry.Module.CompactFlatCurry.C_Option where
  strEq Curry.Module.CompactFlatCurry.C_Verbose Curry.Module.CompactFlatCurry.C_Verbose st = Curry.Module.Prelude.strEqSuccess
  strEq (Curry.Module.CompactFlatCurry.C_Main x1) (Curry.Module.CompactFlatCurry.C_Main y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq Curry.Module.CompactFlatCurry.C_Exports Curry.Module.CompactFlatCurry.C_Exports st = Curry.Module.Prelude.strEqSuccess
  strEq (Curry.Module.CompactFlatCurry.C_InitFuncs x1) (Curry.Module.CompactFlatCurry.C_InitFuncs y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CompactFlatCurry.C_Required x1) (Curry.Module.CompactFlatCurry.C_Required y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CompactFlatCurry.C_Import x1) (Curry.Module.CompactFlatCurry.C_Import y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.CompactFlatCurry.C_Verbose Curry.Module.CompactFlatCurry.C_Verbose st = Curry.Module.Prelude.C_True
  eq (Curry.Module.CompactFlatCurry.C_Main x1) (Curry.Module.CompactFlatCurry.C_Main y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq Curry.Module.CompactFlatCurry.C_Exports Curry.Module.CompactFlatCurry.C_Exports st = Curry.Module.Prelude.C_True
  eq (Curry.Module.CompactFlatCurry.C_InitFuncs x1) (Curry.Module.CompactFlatCurry.C_InitFuncs y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CompactFlatCurry.C_Required x1) (Curry.Module.CompactFlatCurry.C_Required y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CompactFlatCurry.C_Import x1) (Curry.Module.CompactFlatCurry.C_Import y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.CompactFlatCurry.C_Verbose st = Curry.Module.CompactFlatCurry.C_Verbose
  propagate f (Curry.Module.CompactFlatCurry.C_Main x1) st = Curry.Module.CompactFlatCurry.C_Main(f((0::Int))(x1)(st))
  propagate f Curry.Module.CompactFlatCurry.C_Exports st = Curry.Module.CompactFlatCurry.C_Exports
  propagate f (Curry.Module.CompactFlatCurry.C_InitFuncs x1) st = Curry.Module.CompactFlatCurry.C_InitFuncs(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CompactFlatCurry.C_Required x1) st = Curry.Module.CompactFlatCurry.C_Required(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CompactFlatCurry.C_Import x1) st = Curry.Module.CompactFlatCurry.C_Import(f((0::Int))(x1)(st))

  foldCurry f c Curry.Module.CompactFlatCurry.C_Verbose st = c
  foldCurry f c (Curry.Module.CompactFlatCurry.C_Main x1) st = f(x1)(c)(st)
  foldCurry f c Curry.Module.CompactFlatCurry.C_Exports st = c
  foldCurry f c (Curry.Module.CompactFlatCurry.C_InitFuncs x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CompactFlatCurry.C_Required x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CompactFlatCurry.C_Import x1) st = f(x1)(c)(st)

  typeName _ = "Option"

  showQ _ Curry.Module.CompactFlatCurry.C_Verbose = Prelude.showString("CompactFlatCurry.Verbose")
  showQ d (Curry.Module.CompactFlatCurry.C_Main x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CompactFlatCurry.Main "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ Curry.Module.CompactFlatCurry.C_Exports = Prelude.showString("CompactFlatCurry.Exports")
  showQ d (Curry.Module.CompactFlatCurry.C_InitFuncs x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CompactFlatCurry.InitFuncs "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CompactFlatCurry.C_Required x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CompactFlatCurry.Required "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CompactFlatCurry.C_Import x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CompactFlatCurry.Import "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.CompactFlatCurry.C_OptionOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.CompactFlatCurry.C_RequiredSpec where
  strEq (Curry.Module.CompactFlatCurry.C_AlwaysReq x1) (Curry.Module.CompactFlatCurry.C_AlwaysReq y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.CompactFlatCurry.C_Requires x1 x2) (Curry.Module.CompactFlatCurry.C_Requires y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.CompactFlatCurry.C_AlwaysReq x1) (Curry.Module.CompactFlatCurry.C_AlwaysReq y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.CompactFlatCurry.C_Requires x1 x2) (Curry.Module.CompactFlatCurry.C_Requires y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.CompactFlatCurry.C_AlwaysReq x1) st = Curry.Module.CompactFlatCurry.C_AlwaysReq(f((0::Int))(x1)(st))
  propagate f (Curry.Module.CompactFlatCurry.C_Requires x1 x2) st = Curry.Module.CompactFlatCurry.C_Requires(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.CompactFlatCurry.C_AlwaysReq x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.CompactFlatCurry.C_Requires x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "RequiredSpec"

  showQ d (Curry.Module.CompactFlatCurry.C_AlwaysReq x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CompactFlatCurry.AlwaysReq "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.CompactFlatCurry.C_Requires x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CompactFlatCurry.Requires "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.CompactFlatCurry.C_RequiredSpecOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.CompactFlatCurry.C_Option where
  showsPrec _ Curry.Module.CompactFlatCurry.C_Verbose = Prelude.showString("Verbose")
  showsPrec d (Curry.Module.CompactFlatCurry.C_Main x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Main "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ Curry.Module.CompactFlatCurry.C_Exports = Prelude.showString("Exports")
  showsPrec d (Curry.Module.CompactFlatCurry.C_InitFuncs x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("InitFuncs "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CompactFlatCurry.C_Required x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Required "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CompactFlatCurry.C_Import x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Import "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.CompactFlatCurry.C_OptionOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.CompactFlatCurry.C_RequiredSpec where
  showsPrec d (Curry.Module.CompactFlatCurry.C_AlwaysReq x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AlwaysReq "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.CompactFlatCurry.C_Requires x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Requires "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.CompactFlatCurry.C_RequiredSpecOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Read Curry.Module.CompactFlatCurry.C_Option where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.CompactFlatCurry.C_Verbose)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CompactFlatCurry")("Verbose")(r)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CompactFlatCurry.C_Main(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CompactFlatCurry")("Main")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.CompactFlatCurry.C_Exports)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CompactFlatCurry")("Exports")(r)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CompactFlatCurry.C_InitFuncs(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CompactFlatCurry")("InitFuncs")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CompactFlatCurry.C_Required(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CompactFlatCurry")("Required")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CompactFlatCurry.C_Import(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CompactFlatCurry")("Import")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))))))





instance Read Curry.Module.CompactFlatCurry.C_RequiredSpec where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CompactFlatCurry.C_AlwaysReq(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CompactFlatCurry")("AlwaysReq")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.CompactFlatCurry.C_Requires(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("CompactFlatCurry")("Requires")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))





c_isMainOption :: Curry.Module.CompactFlatCurry.C_Option -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isMainOption x1@(Curry.Module.CompactFlatCurry.C_Main x2) st = Curry.Module.Prelude.C_True
c_isMainOption x1@Curry.Module.CompactFlatCurry.C_Verbose st = Curry.Module.Prelude.C_False
c_isMainOption x1@Curry.Module.CompactFlatCurry.C_Exports st = Curry.Module.Prelude.C_False
c_isMainOption x1@(Curry.Module.CompactFlatCurry.C_InitFuncs x3) st = Curry.Module.Prelude.C_False
c_isMainOption x1@(Curry.Module.CompactFlatCurry.C_Required x4) st = Curry.Module.Prelude.C_False
c_isMainOption x1@(Curry.Module.CompactFlatCurry.C_Import x5) st = Curry.Module.Prelude.C_False
c_isMainOption (Curry.Module.CompactFlatCurry.C_OptionOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_isMainOption(x)(st))(i)(xs)(st)
c_isMainOption x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.isMainOption")(x)



c_getMainFuncFromOptions :: (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_getMainFuncFromOptions x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.CompactFlatCurry.c_getMainFuncFromOptions_case_139(x3)(x2)(st)
c_getMainFuncFromOptions (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_getMainFuncFromOptions(x)(st))(i)(xs)(st)
c_getMainFuncFromOptions x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.getMainFuncFromOptions")(x)



c_getRequiredFromOptions :: (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_RequiredSpec
c_getRequiredFromOptions x1 st = Curry.Module.Prelude.c_concat(Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.CompactFlatCurry.c_getRequiredFromOptions'46_'35lambda6))(Curry.Module.Prelude.List)(x1)(st))(st)



c_getRequiredFromOptions'46_'35lambda6 :: Curry.Module.CompactFlatCurry.C_Option -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_RequiredSpec)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_RequiredSpec)
c_getRequiredFromOptions'46_'35lambda6 x1@(Curry.Module.CompactFlatCurry.C_Required x3) x2 st = (Curry.Module.Prelude.:<)(x3)(x2)
c_getRequiredFromOptions'46_'35lambda6 x1@Curry.Module.CompactFlatCurry.C_Verbose x2 st = x2
c_getRequiredFromOptions'46_'35lambda6 x1@(Curry.Module.CompactFlatCurry.C_Main x4) x2 st = x2
c_getRequiredFromOptions'46_'35lambda6 x1@Curry.Module.CompactFlatCurry.C_Exports x2 st = x2
c_getRequiredFromOptions'46_'35lambda6 x1@(Curry.Module.CompactFlatCurry.C_InitFuncs x5) x2 st = x2
c_getRequiredFromOptions'46_'35lambda6 x1@(Curry.Module.CompactFlatCurry.C_Import x6) x2 st = x2
c_getRequiredFromOptions'46_'35lambda6 (Curry.Module.CompactFlatCurry.C_OptionOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_getRequiredFromOptions'46_'35lambda6(x)(x2)(st))(i)(xs)(st)
c_getRequiredFromOptions'46_'35lambda6 x x2 st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.getRequiredFromOptions._#lambda6")(x)



c_addImport2Options :: (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option
c_addImport2Options x1 st = Curry.Module.Prelude.op_43_43(x1)(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pc(Curry.Module.CompactFlatCurry.C_Import))(Curry.Module.List.c_nub(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_addImport2Options'46alwaysReqMod'4621))(st))(Curry.Module.CompactFlatCurry.c_getRequiredFromOptions(x1)(st))(st))(st))(st))(st)



c_addImport2Options'46alwaysReqMod'4621 :: Curry.Module.CompactFlatCurry.C_RequiredSpec -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_addImport2Options'46alwaysReqMod'4621 x1@(Curry.Module.CompactFlatCurry.C_AlwaysReq x2) st = Curry.Module.CompactFlatCurry.c_addImport2Options'46alwaysReqMod'4621_case_138(x2)(st)
c_addImport2Options'46alwaysReqMod'4621 x1@(Curry.Module.CompactFlatCurry.C_Requires x5 x6) st = Curry.Module.Prelude.List
c_addImport2Options'46alwaysReqMod'4621 (Curry.Module.CompactFlatCurry.C_RequiredSpecOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_addImport2Options'46alwaysReqMod'4621(x)(st))(i)(xs)(st)
c_addImport2Options'46alwaysReqMod'4621 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.addImport2Options.alwaysReqMod.21")(x)



c_requires :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.CompactFlatCurry.C_RequiredSpec
c_requires x1 x2 st = Curry.Module.CompactFlatCurry.C_Requires(x1)(x2)



c_alwaysRequired :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.CompactFlatCurry.C_RequiredSpec
c_alwaysRequired x1 st = Curry.Module.CompactFlatCurry.C_AlwaysReq(x1)



c_defaultRequired :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_RequiredSpec
c_defaultRequired st = (Curry.Module.Prelude.:<)(Curry.Module.CompactFlatCurry.c_alwaysRequired(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st))((Curry.Module.Prelude.:<)(Curry.Module.CompactFlatCurry.c_alwaysRequired(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))(Curry.Module.Prelude.List))))))))(st))((Curry.Module.Prelude.:<)(Curry.Module.CompactFlatCurry.c_alwaysRequired(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))(Curry.Module.Prelude.List))))))(st))((Curry.Module.Prelude.:<)(Curry.Module.CompactFlatCurry.c_alwaysRequired(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))))(st))((Curry.Module.Prelude.:<)(Curry.Module.CompactFlatCurry.c_requires(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))(Curry.Module.Prelude.List))))(st))((Curry.Module.Prelude.:<)(Curry.Module.CompactFlatCurry.c_requires(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))(Curry.Module.Prelude.List)))))(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))(Curry.Module.Prelude.List)))(st))((Curry.Module.Prelude.:<)(Curry.Module.CompactFlatCurry.c_requires(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('V'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List)))))))(st))((Curry.Module.Prelude.:<)(Curry.Module.CompactFlatCurry.c_requires(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))(Curry.Module.Prelude.List)))))(st))((Curry.Module.Prelude.:<)(Curry.Module.CompactFlatCurry.c_requires(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))(Curry.Module.Prelude.List))))(st))((Curry.Module.Prelude.:<)(Curry.Module.CompactFlatCurry.c_requires(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))(Curry.Module.Prelude.List)))))))(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))(Curry.Module.Prelude.List)))(st))((Curry.Module.Prelude.:<)(Curry.Module.CompactFlatCurry.c_requires(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('$'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('#'))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))))))))))(st))((Curry.Module.Prelude.:<)(Curry.Module.CompactFlatCurry.c_requires(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))))(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))))))))))))))))))))(st))((Curry.Module.Prelude.:<)(Curry.Module.CompactFlatCurry.c_requires(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))))))))))))))))))))(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))(Curry.Module.Prelude.List)))))))))))))))))(st))((Curry.Module.Prelude.:<)(Curry.Module.CompactFlatCurry.c_requires(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))(Curry.Module.Prelude.List))))))))))))))))))))))(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))(Curry.Module.Prelude.List)))))))))))))))))(st))((Curry.Module.Prelude.:<)(Curry.Module.CompactFlatCurry.c_requires(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))(Curry.Module.Prelude.List)))))))))))))))))(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))(Curry.Module.Prelude.List)))))))))))))))))(st))((Curry.Module.Prelude.:<)(Curry.Module.CompactFlatCurry.c_requires(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('K'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))))))))))))))))(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('K'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))))))))))(st))(Curry.Module.Prelude.List))))))))))))))))



c_prelude :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_prelude st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))



c_getRequiredInModule :: (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_RequiredSpec) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getRequiredInModule x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_getRequiredInModule'46getImpReq'4638(x2)))(st))(x1)(st)



c_getRequiredInModule'46getImpReq'4638 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CompactFlatCurry.C_RequiredSpec -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getRequiredInModule'46getImpReq'4638 x1 x2@(Curry.Module.CompactFlatCurry.C_AlwaysReq x3) st = Curry.Module.CompactFlatCurry.c_getRequiredInModule'46getImpReq'4638_case_137(x1)(x3)(st)
c_getRequiredInModule'46getImpReq'4638 x1 x2@(Curry.Module.CompactFlatCurry.C_Requires x6 x7) st = Curry.Module.Prelude.List
c_getRequiredInModule'46getImpReq'4638 x1 (Curry.Module.CompactFlatCurry.C_RequiredSpecOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_getRequiredInModule'46getImpReq'4638(x1)(x)(st))(i)(xs)(st)
c_getRequiredInModule'46getImpReq'4638 x1 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.getRequiredInModule.getImpReq.38")(x)



c_getImplicitlyRequired :: (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_RequiredSpec) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getImplicitlyRequired x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_getImplicitlyRequired'46getImpReq'4646(x2)))(st))(x1)(st)



c_getImplicitlyRequired'46getImpReq'4646 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CompactFlatCurry.C_RequiredSpec -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getImplicitlyRequired'46getImpReq'4646 x1 x2@(Curry.Module.CompactFlatCurry.C_AlwaysReq x3) st = Curry.Module.Prelude.List
c_getImplicitlyRequired'46getImpReq'4646 x1 x2@(Curry.Module.CompactFlatCurry.C_Requires x4 x5) st = Curry.Module.CompactFlatCurry.c_getImplicitlyRequired'46getImpReq'4646_case_135(x1)(x4)(x5)(Curry.Module.Prelude.op_61_61(x4)(x1)(st))(st)
c_getImplicitlyRequired'46getImpReq'4646 x1 (Curry.Module.CompactFlatCurry.C_RequiredSpecOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_getImplicitlyRequired'46getImpReq'4646(x1)(x)(st))(i)(xs)(st)
c_getImplicitlyRequired'46getImpReq'4646 x1 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.getImplicitlyRequired.getImpReq.46")(x)



c_defaultRequiredTypes :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_defaultRequiredTypes st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.CompactFlatCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.List))))))



c_generateCompactFlatCurryFile :: (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_generateCompactFlatCurryFile x1 x2 x3 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.CompactFlatCurry.c_computeCompactFlatCurry(x1)(x2)(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_generateCompactFlatCurryFile'46_'35lambda8(x3)))(st)



c_generateCompactFlatCurryFile'46_'35lambda8 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_generateCompactFlatCurryFile'46_'35lambda8 x1 x2 st = Curry.Module.Prelude.op_62_62(Curry.Module.FlatCurry.c_writeFCY(x1)(x2)(st))(Curry.Module.Prelude.c_done(st))(st)



c_computeCompactFlatCurry :: (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_computeCompactFlatCurry x1 x2 st = let {x3 = Curry.Module.CompactFlatCurry.c_addImport2Options(x1)(st)} in Curry.Module.CompactFlatCurry.c_computeCompactFlatCurry_case_134(x2)(x3)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(Curry.Module.CompactFlatCurry.C_Exports)(st))(x3)(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_any(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_isMainOption))(st))(x3)(st))(st))(st)



c_computeCompactFlatCurry'46_'35lambda9 :: (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option) -> Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_computeCompactFlatCurry'46_'35lambda9 x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.CompactFlatCurry.c_makeCompactFlatCurry(x2)(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_computeCompactFlatCurry'46_'35lambda9'46_'35lambda10))(st)



c_computeCompactFlatCurry'46_'35lambda9'46_'35lambda10 :: Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_computeCompactFlatCurry'46_'35lambda9'46_'35lambda10 x1 st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_putStrLn(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('z'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))))))))))))))))))))))(Curry.Module.Prelude.c_show(Curry.Module.Prelude.c_length(Curry.Module.CompactFlatCurry.c_moduleFuns(x1)(st))(st))(st))(st))(st))(Curry.Module.Prelude.c_return(x1)(st))(st)



c_makeCompactFlatCurry :: Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_makeCompactFlatCurry x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.CompactFlatCurry.c_requiredInCompactProg(x1)(x2)(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_makeCompactFlatCurry'46_'35lambda11(x1)(x2)))(st)



c_makeCompactFlatCurry'46_'35lambda11 :: Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option) -> (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_makeCompactFlatCurry'46_'35lambda11 x1 x2 x3@(Curry.Module.Prelude.T3 x4 x5 x6) st = let {x8 = Curry.Module.CompactFlatCurry.c_getRequiredFromOptions(x2)(st)} in let {x10 = Curry.Module.Prelude.op_43_43(x4)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_getRequiredInModule(x8)))(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_moduleName))(x6)(st))(st))(st)} in Curry.Module.Prelude.op_62_62_61(Curry.Module.CompactFlatCurry.c_getCalledFuncs(x8)(x5)(x6)(Curry.Module.CompactFlatCurry.c_extendFuncTable(Curry.Module.TableRBT.c_emptyTableRBT(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.CompactFlatCurry.c_leqQName))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_moduleFuns))(st))(x6)(st))(st))(Curry.Module.Prelude.c_foldr(Curry.Module.SetRBT.c_insertRBT(st))(Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_emptySetRBT(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.CompactFlatCurry.c_leqQName))(st))(x10)(st))(Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_emptySetRBT(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.CompactFlatCurry.c_leqQName))(st))(Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_emptySetRBT(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.CompactFlatCurry.c_leqQName))(st))(x10)(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12(x1)))(st)
c_makeCompactFlatCurry'46_'35lambda11 x1 x2 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_makeCompactFlatCurry'46_'35lambda11(x1)(x2)(x)(st))(i)(xs)(st)
c_makeCompactFlatCurry'46_'35lambda11 x1 x2 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.makeCompactFlatCurry._#lambda11")(x)



c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12 :: Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_putStrLn(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))(Curry.Module.Prelude.c_show(Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_43))(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.c_map(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_length))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_moduleFuns))(st))(x3)(st))(st))(st))(st))(st))(Curry.Module.Prelude.c_return(Curry.Module.FlatCurry.C_Prog(Curry.Module.CompactFlatCurry.c_moduleName(x1)(st))(Curry.Module.Prelude.List)(let {x8 = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_moduleTypes))(st))(x3)(st)} in Curry.Module.Prelude.c_filter(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12'46_'35lambda13(Curry.Module.CompactFlatCurry.c_requiredDatatypes(Curry.Module.CompactFlatCurry.c_extendTConsWithConsType(x5)(x6)(x8)(st))(x8)(st))))(x8)(st))(x4)(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12'46_'35lambda14(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_functionName))(x4)(st))))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_moduleOps))(st))(x3)(st))(st)))(st))(st)
c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12(x1)(x)(st))(i)(xs)(st)
c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12 x1 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.makeCompactFlatCurry._#lambda11._#lambda12")(x)



c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12'46_'35lambda13 :: (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.FlatCurry.C_TypeDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12'46_'35lambda13 x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_elemRBT(Curry.Module.CompactFlatCurry.c_tconsName(x2)(st))(st))(x1)(st)



c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12'46_'35lambda14 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.FlatCurry.C_OpDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12'46_'35lambda14 x1 x2@(Curry.Module.FlatCurry.C_Op x3 x4 x5) st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(x3)(st))(x1)(st)
c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12'46_'35lambda14 x1 (Curry.Module.FlatCurry.C_OpDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12'46_'35lambda14(x1)(x)(st))(i)(xs)(st)
c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12'46_'35lambda14 x1 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.makeCompactFlatCurry._#lambda11._#lambda12._#lambda14")(x)



c_requiredDatatypes :: (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_TypeDecl) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_requiredDatatypes x1 x2 st = let {x3 = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_newTypeConsOfTDecl(x1)))(st))(x2)(st)} in Curry.Module.CompactFlatCurry.c_requiredDatatypes_case_133(x1)(x2)(x3)(Curry.Module.Prelude.c_null(x3)(st))(st)



c_newTypeConsOfTDecl :: (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.FlatCurry.C_TypeDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_newTypeConsOfTDecl x1 x2@(Curry.Module.FlatCurry.C_TypeSyn x3 x4 x5 x6) st = Curry.Module.CompactFlatCurry.c_newTypeConsOfTDecl_case_132(x1)(x3)(x6)(Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_elemRBT(x3)(st))(x1)(st))(st)
c_newTypeConsOfTDecl x1 x2@(Curry.Module.FlatCurry.C_Type x7 x8 x9 x10) st = Curry.Module.CompactFlatCurry.c_newTypeConsOfTDecl_case_131(x1)(x7)(x10)(Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_elemRBT(x7)(st))(x1)(st))(st)
c_newTypeConsOfTDecl x1 (Curry.Module.FlatCurry.C_TypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_newTypeConsOfTDecl(x1)(x)(st))(i)(xs)(st)
c_newTypeConsOfTDecl x1 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.newTypeConsOfTDecl")(x)



c_newTypeConsOfTDecl'46_'35lambda15 :: (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_newTypeConsOfTDecl'46_'35lambda15 x1 x2 st = Curry.Module.Prelude.c_not(Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_elemRBT(x2)(st))(x1)(st))(st)



c_newTypeConsOfTDecl'46_'35lambda16 :: (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_newTypeConsOfTDecl'46_'35lambda16 x1 x2 st = Curry.Module.Prelude.c_not(Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_elemRBT(x2)(st))(x1)(st))(st)



c_newTypeConsOfTDecl'46_'35lambda17 :: Curry.Module.FlatCurry.C_ConsDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_newTypeConsOfTDecl'46_'35lambda17 x1@(Curry.Module.FlatCurry.C_Cons x2 x3 x4 x5) st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_allTypesOfTExpr))(st))(x5)(st)
c_newTypeConsOfTDecl'46_'35lambda17 (Curry.Module.FlatCurry.C_ConsDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_newTypeConsOfTDecl'46_'35lambda17(x)(st))(i)(xs)(st)
c_newTypeConsOfTDecl'46_'35lambda17 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.newTypeConsOfTDecl._#lambda17")(x)



c_extendTConsWithConsType :: (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_TypeDecl) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_extendTConsWithConsType x1 x2 x3@Curry.Module.Prelude.List st = x2
c_extendTConsWithConsType x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CompactFlatCurry.c_extendTConsWithConsType_case_130(x1)(x2)(x5)(x4)(st)
c_extendTConsWithConsType x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_extendTConsWithConsType(x1)(x2)(x)(st))(i)(xs)(st)
c_extendTConsWithConsType x1 x2 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.extendTConsWithConsType")(x)



c_extendTConsWithConsType'46_'35lambda18 :: (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.FlatCurry.C_ConsDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_extendTConsWithConsType'46_'35lambda18 x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_elemRBT(Curry.Module.CompactFlatCurry.c_consName(x2)(st))(st))(x1)(st)



c_extendFuncTable :: (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.FlatCurry.C_FuncDecl)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.FlatCurry.C_FuncDecl)
c_extendFuncTable x1 x2 st = Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.CompactFlatCurry.c_extendFuncTable'46_'35lambda19))(x1)(x2)(st)



c_extendFuncTable'46_'35lambda19 :: Curry.Module.FlatCurry.C_FuncDecl -> (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.FlatCurry.C_FuncDecl)) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.FlatCurry.C_FuncDecl)
c_extendFuncTable'46_'35lambda19 x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.TableRBT.c_updateRBT(Curry.Module.CompactFlatCurry.c_functionName(x1)(st))(x1)(st))(x2)(st)



c_requiredInCompactProg :: Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog))
c_requiredInCompactProg x1 x2 st = let {x3 = Curry.Module.List.c_nub(Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.CompactFlatCurry.c_requiredInCompactProg'46_'35lambda22))(Curry.Module.Prelude.List)(x2)(st))(st)} in let {x4 = Curry.Module.CompactFlatCurry.c_moduleName(x1)(st)} in let {x5 = Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.CompactFlatCurry.c_requiredInCompactProg'46_'35lambda26))(Curry.Module.Prelude.List)(x2)(st)} in let {x6 = Curry.Module.CompactFlatCurry.c_exportedFuncNames(Curry.Module.CompactFlatCurry.c_moduleFuns(x1)(st))(st)} in let {x7 = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_insertRBT(st))(x4)(st))(Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_emptySetRBT(st))(Curry.Module.Sort.c_leqString(st))(st))(st)} in Curry.Module.CompactFlatCurry.c_requiredInCompactProg_case_128(x1)(x2)(x3)(x4)(x5)(x6)(x7)(Curry.Module.Prelude.c_not(Curry.Module.Prelude.c_null(x5)(st))(st))(st)



c_requiredInCompactProg'46_'35lambda22 :: Curry.Module.CompactFlatCurry.C_Option -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_requiredInCompactProg'46_'35lambda22 x1@(Curry.Module.CompactFlatCurry.C_Import x3) x2 st = (Curry.Module.Prelude.:<)(x3)(x2)
c_requiredInCompactProg'46_'35lambda22 x1@Curry.Module.CompactFlatCurry.C_Verbose x2 st = x2
c_requiredInCompactProg'46_'35lambda22 x1@(Curry.Module.CompactFlatCurry.C_Main x4) x2 st = x2
c_requiredInCompactProg'46_'35lambda22 x1@Curry.Module.CompactFlatCurry.C_Exports x2 st = x2
c_requiredInCompactProg'46_'35lambda22 x1@(Curry.Module.CompactFlatCurry.C_InitFuncs x5) x2 st = x2
c_requiredInCompactProg'46_'35lambda22 x1@(Curry.Module.CompactFlatCurry.C_Required x6) x2 st = x2
c_requiredInCompactProg'46_'35lambda22 (Curry.Module.CompactFlatCurry.C_OptionOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_requiredInCompactProg'46_'35lambda22(x)(x2)(st))(i)(xs)(st)
c_requiredInCompactProg'46_'35lambda22 x x2 st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.requiredInCompactProg._#lambda22")(x)



c_requiredInCompactProg'46_'35lambda26 :: Curry.Module.CompactFlatCurry.C_Option -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_requiredInCompactProg'46_'35lambda26 x1@(Curry.Module.CompactFlatCurry.C_InitFuncs x3) x2 st = (Curry.Module.Prelude.:<)(x3)(x2)
c_requiredInCompactProg'46_'35lambda26 x1@Curry.Module.CompactFlatCurry.C_Verbose x2 st = x2
c_requiredInCompactProg'46_'35lambda26 x1@(Curry.Module.CompactFlatCurry.C_Main x4) x2 st = x2
c_requiredInCompactProg'46_'35lambda26 x1@Curry.Module.CompactFlatCurry.C_Exports x2 st = x2
c_requiredInCompactProg'46_'35lambda26 x1@(Curry.Module.CompactFlatCurry.C_Required x5) x2 st = x2
c_requiredInCompactProg'46_'35lambda26 x1@(Curry.Module.CompactFlatCurry.C_Import x6) x2 st = x2
c_requiredInCompactProg'46_'35lambda26 (Curry.Module.CompactFlatCurry.C_OptionOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_requiredInCompactProg'46_'35lambda26(x)(x2)(st))(i)(xs)(st)
c_requiredInCompactProg'46_'35lambda26 x x2 st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.requiredInCompactProg._#lambda26")(x)



c_requiredInCompactProg'46add2mainmodset'46118 :: (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_requiredInCompactProg'46add2mainmodset'46118 x1 x2 st = Curry.Module.Prelude.c_foldr(Curry.Module.SetRBT.c_insertRBT(st))(x1)(x2)(st)



c_requiredInCompactProg'46_'35lambda28 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))) -> Curry.Module.FlatCurry.C_Prog -> (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog))
c_requiredInCompactProg'46_'35lambda28 x1 x2 x3 x4 x5 st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.T3(Curry.Module.Prelude.c_concat(x2)(st))(Curry.Module.CompactFlatCurry.c_requiredInCompactProg'46add2mainmodset'46118(x4)(x1)(st))((Curry.Module.Prelude.:<)(x3)(x5)))(st)



c_requiredInCompactProg'46_'35lambda29 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.FlatCurry.C_Prog -> (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog))
c_requiredInCompactProg'46_'35lambda29 x1 x2 x3 x4 x5 st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.T3(Curry.Module.List.c_nub(x2)(st))(Curry.Module.CompactFlatCurry.c_requiredInCompactProg'46add2mainmodset'46118(x4)(x1)(st))((Curry.Module.Prelude.:<)(x3)(x5)))(st)



c_requiredInCompactProg'46_'35lambda30 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog))
c_requiredInCompactProg'46_'35lambda30 x1 x2 x3 x4 x5 x6 st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.T3((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(x4)(x1))(Curry.Module.Prelude.List))(Curry.Module.CompactFlatCurry.c_requiredInCompactProg'46add2mainmodset'46118(x5)(x2)(st))((Curry.Module.Prelude.:<)(x3)(x6)))(st)



c_requiredInCompactProg'46_'35lambda31 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.FlatCurry.C_Prog -> (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog))
c_requiredInCompactProg'46_'35lambda31 x1 x2 x3 x4 st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.T3(Curry.Module.List.c_nub(Curry.Module.Prelude.op_43_43(x1)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_exportedFuncNames))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_moduleFuns))(st))(st))(x4)(st))(st))(st))(Curry.Module.CompactFlatCurry.c_requiredInCompactProg'46add2mainmodset'46118(x3)(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_moduleName))(x4)(st))(st))((Curry.Module.Prelude.:<)(x2)(x4)))(st)



c_exportedFuncNames :: (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_exportedFuncNames x1 st = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_exportedFuncNames'46_'35lambda32))(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_exportedFuncNames'46_'35lambda33))(x1)(st))(st)



c_exportedFuncNames'46_'35lambda32 :: Curry.Module.FlatCurry.C_FuncDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_exportedFuncNames'46_'35lambda32 x1@(Curry.Module.FlatCurry.C_Func x2 x3 x4 x5 x6) st = x2
c_exportedFuncNames'46_'35lambda32 (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_exportedFuncNames'46_'35lambda32(x)(st))(i)(xs)(st)
c_exportedFuncNames'46_'35lambda32 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.exportedFuncNames._#lambda32")(x)



c_exportedFuncNames'46_'35lambda33 :: Curry.Module.FlatCurry.C_FuncDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_exportedFuncNames'46_'35lambda33 x1@(Curry.Module.FlatCurry.C_Func x2 x3 x4 x5 x6) st = Curry.Module.Prelude.op_61_61(x4)(Curry.Module.FlatCurry.C_Public)(st)
c_exportedFuncNames'46_'35lambda33 (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_exportedFuncNames'46_'35lambda33(x)(st))(i)(xs)(st)
c_exportedFuncNames'46_'35lambda33 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.exportedFuncNames._#lambda33")(x)



c_getCalledFuncs :: (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_RequiredSpec) -> (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) -> (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.FlatCurry.C_FuncDecl)) -> (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_getCalledFuncs x1 x2 x3 x4 x5 x6 x7 x8@Curry.Module.Prelude.List st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.T4(x3)(Curry.Module.Prelude.List)(x6)(x7))(st)
c_getCalledFuncs x1 x2 x3 x4 x5 x6 x7 x8@((Curry.Module.Prelude.:<) x9 x10) st = Curry.Module.CompactFlatCurry.c_getCalledFuncs_case_123(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x10)(x9)(st)
c_getCalledFuncs x1 x2 x3 x4 x5 x6 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_getCalledFuncs(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c_getCalledFuncs x1 x2 x3 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.getCalledFuncs")(x)



c_getCalledFuncs'46_'35lambda34 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.FlatCurry.C_FuncDecl)) -> (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) -> (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_RequiredSpec) -> Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_getCalledFuncs'46_'35lambda34 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 st = let {x12 = Curry.Module.CompactFlatCurry.c_getRequiredInModule(x10)(x8)(st)} in Curry.Module.CompactFlatCurry.c_getCalledFuncs(x10)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_insertRBT(st))(x8)(st))(x6)(st))((Curry.Module.Prelude.:<)(x11)(x9))(Curry.Module.CompactFlatCurry.c_extendFuncTable(x3)(Curry.Module.CompactFlatCurry.c_moduleFuns(x11)(st))(st))(Curry.Module.Prelude.c_foldr(Curry.Module.SetRBT.c_insertRBT(st))(x5)(x12)(st))(x4)(x7)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(x8)(x1))(Curry.Module.Prelude.op_43_43(x2)(x12)(st)))(st)



c_getCalledFuncs'46_'35lambda35 :: (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_getCalledFuncs'46_'35lambda35 x1 x2 st = Curry.Module.Prelude.c_not(Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_elemRBT(x2)(st))(x1)(st))(st)



c_getCalledFuncs'46_'35lambda36 :: (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_getCalledFuncs'46_'35lambda36 x1 x2 st = Curry.Module.Prelude.c_not(Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_elemRBT(x2)(st))(x1)(st))(st)



c_getCalledFuncs'46_'35lambda37 :: Curry.Module.FlatCurry.C_FuncDecl -> (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_getCalledFuncs'46_'35lambda37 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.T4(x3)((Curry.Module.Prelude.:<)(x1)(x4))(x5)(x6))(st)
c_getCalledFuncs'46_'35lambda37 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_getCalledFuncs'46_'35lambda37(x1)(x)(st))(i)(xs)(st)
c_getCalledFuncs'46_'35lambda37 x1 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.getCalledFuncs._#lambda37")(x)



c_allFuncCalls :: Curry.Module.FlatCurry.C_FuncDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_allFuncCalls x1@(Curry.Module.FlatCurry.C_Func x2 x3 x4 x5 x6) st = Curry.Module.CompactFlatCurry.c_allFuncCalls_case_119(x6)(st)
c_allFuncCalls (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_allFuncCalls(x)(st))(i)(xs)(st)
c_allFuncCalls x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.allFuncCalls")(x)



c_allFuncCallsOfExpr :: Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_allFuncCallsOfExpr x1@(Curry.Module.FlatCurry.C_Var x2) st = Curry.Module.Prelude.List
c_allFuncCallsOfExpr x1@(Curry.Module.FlatCurry.C_Lit x3) st = Curry.Module.Prelude.List
c_allFuncCallsOfExpr x1@(Curry.Module.FlatCurry.C_Comb x4 x5 x6) st = let {x7 = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_allFuncCallsOfExpr))(st))(x6)(st)} in Curry.Module.CompactFlatCurry.c_allFuncCallsOfExpr_case_118(x5)(x7)(x4)(st)
c_allFuncCallsOfExpr x1@(Curry.Module.FlatCurry.C_Free x10 x11) st = Curry.Module.CompactFlatCurry.c_allFuncCallsOfExpr(x11)(st)
c_allFuncCallsOfExpr x1@(Curry.Module.FlatCurry.C_Let x12 x13) st = Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_allFuncCallsOfExpr))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))(st))(st))(x12)(st))(Curry.Module.CompactFlatCurry.c_allFuncCallsOfExpr(x13)(st))(st)
c_allFuncCallsOfExpr x1@(Curry.Module.FlatCurry.C_Or x14 x15) st = Curry.Module.Prelude.op_43_43(Curry.Module.CompactFlatCurry.c_allFuncCallsOfExpr(x14)(st))(Curry.Module.CompactFlatCurry.c_allFuncCallsOfExpr(x15)(st))(st)
c_allFuncCallsOfExpr x1@(Curry.Module.FlatCurry.C_Case x16 x17 x18) st = Curry.Module.Prelude.op_43_43(Curry.Module.CompactFlatCurry.c_allFuncCallsOfExpr(x17)(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_allFuncCallsOfBranchExpr))(st))(x18)(st))(st)
c_allFuncCallsOfExpr (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_allFuncCallsOfExpr(x)(st))(i)(xs)(st)
c_allFuncCallsOfExpr x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.allFuncCallsOfExpr")(x)



c_allFuncCallsOfBranchExpr :: Curry.Module.FlatCurry.C_BranchExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_allFuncCallsOfBranchExpr x1@(Curry.Module.FlatCurry.C_Branch x2 x3) st = Curry.Module.CompactFlatCurry.c_allFuncCallsOfExpr(x3)(st)
c_allFuncCallsOfBranchExpr (Curry.Module.FlatCurry.C_BranchExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_allFuncCallsOfBranchExpr(x)(st))(i)(xs)(st)
c_allFuncCallsOfBranchExpr x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.allFuncCallsOfBranchExpr")(x)



c_allConstructorsOfFunc :: Curry.Module.FlatCurry.C_FuncDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_allConstructorsOfFunc x1@(Curry.Module.FlatCurry.C_Func x2 x3 x4 x5 x6) st = Curry.Module.CompactFlatCurry.c_allConstructorsOfFunc_case_117(x6)(st)
c_allConstructorsOfFunc (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_allConstructorsOfFunc(x)(st))(i)(xs)(st)
c_allConstructorsOfFunc x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.allConstructorsOfFunc")(x)



c_allConsOfExpr :: Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_allConsOfExpr x1@(Curry.Module.FlatCurry.C_Var x2) st = Curry.Module.Prelude.List
c_allConsOfExpr x1@(Curry.Module.FlatCurry.C_Lit x3) st = Curry.Module.Prelude.List
c_allConsOfExpr x1@(Curry.Module.FlatCurry.C_Comb x4 x5 x6) st = let {x7 = Curry.Module.Prelude.c_apply(Curry.Module.CompactFlatCurry.c_unionMap(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_allConsOfExpr))(st))(x6)(st)} in Curry.Module.CompactFlatCurry.c_allConsOfExpr_case_116(x5)(x7)(x4)(st)
c_allConsOfExpr x1@(Curry.Module.FlatCurry.C_Free x10 x11) st = Curry.Module.CompactFlatCurry.c_allConsOfExpr(x11)(st)
c_allConsOfExpr x1@(Curry.Module.FlatCurry.C_Let x12 x13) st = Curry.Module.List.c_union(Curry.Module.Prelude.c_apply(Curry.Module.CompactFlatCurry.c_unionMap(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_allConsOfExpr))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))(st))(st))(x12)(st))(Curry.Module.CompactFlatCurry.c_allConsOfExpr(x13)(st))(st)
c_allConsOfExpr x1@(Curry.Module.FlatCurry.C_Or x14 x15) st = Curry.Module.List.c_union(Curry.Module.CompactFlatCurry.c_allConsOfExpr(x14)(st))(Curry.Module.CompactFlatCurry.c_allConsOfExpr(x15)(st))(st)
c_allConsOfExpr x1@(Curry.Module.FlatCurry.C_Case x16 x17 x18) st = Curry.Module.List.c_union(Curry.Module.CompactFlatCurry.c_allConsOfExpr(x17)(st))(Curry.Module.Prelude.c_apply(Curry.Module.CompactFlatCurry.c_unionMap(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_allConsOfExpr'46consOfBranch'46252))(st))(x18)(st))(st)
c_allConsOfExpr (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_allConsOfExpr(x)(st))(i)(xs)(st)
c_allConsOfExpr x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.allConsOfExpr")(x)



c_allConsOfExpr'46consOfBranch'46252 :: Curry.Module.FlatCurry.C_BranchExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_allConsOfExpr'46consOfBranch'46252 x1@(Curry.Module.FlatCurry.C_Branch x2 x3) st = Curry.Module.CompactFlatCurry.c_allConsOfExpr'46consOfBranch'46252_case_115(x3)(x2)(st)
c_allConsOfExpr'46consOfBranch'46252 (Curry.Module.FlatCurry.C_BranchExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_allConsOfExpr'46consOfBranch'46252(x)(st))(i)(xs)(st)
c_allConsOfExpr'46consOfBranch'46252 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.allConsOfExpr.consOfBranch.252")(x)



c_allTypesOfFunc :: Curry.Module.FlatCurry.C_FuncDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_allTypesOfFunc x1@(Curry.Module.FlatCurry.C_Func x2 x3 x4 x5 x6) st = Curry.Module.CompactFlatCurry.c_allTypesOfTExpr(x5)(st)
c_allTypesOfFunc (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_allTypesOfFunc(x)(st))(i)(xs)(st)
c_allTypesOfFunc x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.allTypesOfFunc")(x)



c_allTypesOfTExpr :: Curry.Module.FlatCurry.C_TypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_allTypesOfTExpr x1@(Curry.Module.FlatCurry.C_TVar x2) st = Curry.Module.Prelude.List
c_allTypesOfTExpr x1@(Curry.Module.FlatCurry.C_FuncType x3 x4) st = Curry.Module.List.c_union(Curry.Module.CompactFlatCurry.c_allTypesOfTExpr(x3)(st))(Curry.Module.CompactFlatCurry.c_allTypesOfTExpr(x4)(st))(st)
c_allTypesOfTExpr x1@(Curry.Module.FlatCurry.C_TCons x5 x6) st = Curry.Module.List.c_union((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.Prelude.c_apply(Curry.Module.CompactFlatCurry.c_unionMap(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_allTypesOfTExpr))(st))(x6)(st))(st)
c_allTypesOfTExpr (Curry.Module.FlatCurry.C_TypeExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_allTypesOfTExpr(x)(st))(i)(xs)(st)
c_allTypesOfTExpr x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.allTypesOfTExpr")(x)



c_unionMap :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1)
c_unionMap x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.List.c_union))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(x1)))(st)



c_functionName :: Curry.Module.FlatCurry.C_FuncDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_functionName x1@(Curry.Module.FlatCurry.C_Func x2 x3 x4 x5 x6) st = x2
c_functionName (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_functionName(x)(st))(i)(xs)(st)
c_functionName x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.functionName")(x)



c_consName :: Curry.Module.FlatCurry.C_ConsDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_consName x1@(Curry.Module.FlatCurry.C_Cons x2 x3 x4 x5) st = x2
c_consName (Curry.Module.FlatCurry.C_ConsDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_consName(x)(st))(i)(xs)(st)
c_consName x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.consName")(x)



c_tconsName :: Curry.Module.FlatCurry.C_TypeDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_tconsName x1@(Curry.Module.FlatCurry.C_Type x2 x3 x4 x5) st = x2
c_tconsName x1@(Curry.Module.FlatCurry.C_TypeSyn x6 x7 x8 x9) st = x6
c_tconsName (Curry.Module.FlatCurry.C_TypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_tconsName(x)(st))(i)(xs)(st)
c_tconsName x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.tconsName")(x)



c_moduleImports :: Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_moduleImports x1@(Curry.Module.FlatCurry.C_Prog x2 x3 x4 x5 x6) st = x3
c_moduleImports (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_moduleImports(x)(st))(i)(xs)(st)
c_moduleImports x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.moduleImports")(x)



c_moduleTypes :: Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.FlatCurry.C_TypeDecl
c_moduleTypes x1@(Curry.Module.FlatCurry.C_Prog x2 x3 x4 x5 x6) st = x4
c_moduleTypes (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_moduleTypes(x)(st))(i)(xs)(st)
c_moduleTypes x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.moduleTypes")(x)



c_moduleOps :: Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.FlatCurry.C_OpDecl
c_moduleOps x1@(Curry.Module.FlatCurry.C_Prog x2 x3 x4 x5 x6) st = x6
c_moduleOps (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_moduleOps(x)(st))(i)(xs)(st)
c_moduleOps x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.moduleOps")(x)



c_moduleName :: Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_moduleName x1@(Curry.Module.FlatCurry.C_Prog x2 x3 x4 x5 x6) st = x2
c_moduleName (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_moduleName(x)(st))(i)(xs)(st)
c_moduleName x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.moduleName")(x)



c_moduleFuns :: Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl
c_moduleFuns x1@(Curry.Module.FlatCurry.C_Prog x2 x3 x4 x5 x6) st = x5
c_moduleFuns (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_moduleFuns(x)(st))(i)(xs)(st)
c_moduleFuns x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.moduleFuns")(x)



c_leqQName :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_leqQName x1@(Curry.Module.Prelude.T2 x3 x4) x2 st = Curry.Module.CompactFlatCurry.c_leqQName_case_114(x3)(x4)(x2)(st)
c_leqQName (Curry.Module.Prelude.T2Or i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_leqQName(x)(x2)(st))(i)(xs)(st)
c_leqQName x x2 st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.leqQName")(x)



c_readCurrentFlatCurry :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_readCurrentFlatCurry x1 st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_putStr(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List))))(st))(st))(Curry.Module.Prelude.op_62_62_61(Curry.Module.CompactFlatCurry.c_findSourceFileInLoadPath(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_readCurrentFlatCurry'46_'35lambda40))(st))(st)



c_readCurrentFlatCurry'46_'35lambda40 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_readCurrentFlatCurry'46_'35lambda40 x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesFileExist(Curry.Module.FlatCurry.c_flatCurryFileName(x1)(st))(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41(x1)))(st)



c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41 x1 x2 st = Curry.Module.CompactFlatCurry.c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41_case_113(x1)(x2)(Curry.Module.Prelude.c_not(x2)(st))(st)



c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41'46_'35lambda42 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Time.C_ClockTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41'46_'35lambda42 x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_getModificationTime(Curry.Module.FlatCurry.c_flatCurryFileName(x1)(st))(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41'46_'35lambda42'46_'35lambda43(x2)(x1)))(st)



c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41'46_'35lambda42'46_'35lambda43 :: Curry.Module.Time.C_ClockTime -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Time.C_ClockTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41'46_'35lambda42'46_'35lambda43 x1 x2 x3 st = Curry.Module.CompactFlatCurry.c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41'46_'35lambda42'46_'35lambda43_case_112(x1)(x2)(x3)(Curry.Module.Prelude.op_62(Curry.Module.Time.c_clockTimeToInt(x1)(st))(Curry.Module.Time.c_clockTimeToInt(x3)(st))(st))(st)



c_getSourceModificationTime :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Time.C_ClockTime
c_getSourceModificationTime x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesFileExist(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_getSourceModificationTime'46_'35lambda44(x1)))(st)



c_getSourceModificationTime'46_'35lambda44 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Time.C_ClockTime
c_getSourceModificationTime'46_'35lambda44 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Directory.c_getModificationTime(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))(st))(st)
c_getSourceModificationTime'46_'35lambda44 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Directory.c_getModificationTime(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st))(st)
c_getSourceModificationTime'46_'35lambda44 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_getSourceModificationTime'46_'35lambda44(x1)(x)(st))(i)(xs)(st)
c_getSourceModificationTime'46_'35lambda44 x1 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.getSourceModificationTime._#lambda44")(x)



c_findSourceFileInLoadPath :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_findSourceFileInLoadPath x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_getLoadPathForFile(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_findSourceFileInLoadPath'46_'35lambda45(x1)))(st)



c_findSourceFileInLoadPath'46_'35lambda45 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_findSourceFileInLoadPath'46_'35lambda45 x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.FileGoodies.c_lookupFileInPath(Curry.Module.FileGoodies.c_baseName(x1)(st))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(Curry.Module.Prelude.List)))(x2)(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_findSourceFileInLoadPath'46_'35lambda45'46_'35lambda46(x1)))(st)



c_findSourceFileInLoadPath'46_'35lambda45'46_'35lambda46 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_findSourceFileInLoadPath'46_'35lambda45'46_'35lambda46 x1 x2 st = Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.c_error(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List))))))))))))))))))))))))(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))(Curry.Module.Prelude.List)))))))))))))(st))(st))(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.FileGoodies.c_stripSuffix(st))(st))(x2)(st)



c_processPrimitives :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_processPrimitives x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.CompactFlatCurry.c_readPrimSpec(Curry.Module.CompactFlatCurry.c_moduleName(x2)(st))(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('2'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))(Curry.Module.Prelude.List))))))))))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_processPrimitives'46_'35lambda47(x2)))(st)



c_processPrimitives'46_'35lambda47 :: Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog
c_processPrimitives'46_'35lambda47 x1 x2 st = Curry.Module.Prelude.c_return(Curry.Module.CompactFlatCurry.c_mergePrimSpecIntoModule(x2)(x1)(st))(st)



c_mergePrimSpecIntoModule :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))) -> Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Prog
c_mergePrimSpecIntoModule x1 x2@(Curry.Module.FlatCurry.C_Prog x3 x4 x5 x6 x7) st = Curry.Module.FlatCurry.C_Prog(x3)(x4)(x5)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_mergePrimSpecIntoFunc(x1)))(st))(x6)(st))(x7)
c_mergePrimSpecIntoModule x1 (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_mergePrimSpecIntoModule(x1)(x)(st))(i)(xs)(st)
c_mergePrimSpecIntoModule x1 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.mergePrimSpecIntoModule")(x)



c_mergePrimSpecIntoFunc :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))) -> Curry.Module.FlatCurry.C_FuncDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl
c_mergePrimSpecIntoFunc x1 x2@(Curry.Module.FlatCurry.C_Func x3 x4 x5 x6 x7) st = let {x8 = Curry.Module.Prelude.c_lookup(x3)(x1)(st)} in Curry.Module.CompactFlatCurry.c_mergePrimSpecIntoFunc_case_111(x3)(x4)(x5)(x6)(x7)(x8)(Curry.Module.Prelude.op_61_61(x8)(Curry.Module.Prelude.C_Nothing)(st))(st)
c_mergePrimSpecIntoFunc x1 (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_mergePrimSpecIntoFunc(x1)(x)(st))(i)(xs)(st)
c_mergePrimSpecIntoFunc x1 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.mergePrimSpecIntoFunc")(x)



c_mergePrimSpecIntoFunc'46_'35selFP3'35lib :: (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_mergePrimSpecIntoFunc'46_'35selFP3'35lib x1@(Curry.Module.Prelude.C_Just x2) st = Curry.Module.CompactFlatCurry.c_mergePrimSpecIntoFunc'46_'35selFP3'35lib_case_109(x2)(st)
c_mergePrimSpecIntoFunc'46_'35selFP3'35lib (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_mergePrimSpecIntoFunc'46_'35selFP3'35lib(x)(st))(i)(xs)(st)
c_mergePrimSpecIntoFunc'46_'35selFP3'35lib x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.mergePrimSpecIntoFunc._#selFP3#lib")(x)



c_mergePrimSpecIntoFunc'46_'35selFP4'35entry :: (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_mergePrimSpecIntoFunc'46_'35selFP4'35entry x1@(Curry.Module.Prelude.C_Just x2) st = Curry.Module.CompactFlatCurry.c_mergePrimSpecIntoFunc'46_'35selFP4'35entry_case_108(x2)(st)
c_mergePrimSpecIntoFunc'46_'35selFP4'35entry (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_mergePrimSpecIntoFunc'46_'35selFP4'35entry(x)(st))(i)(xs)(st)
c_mergePrimSpecIntoFunc'46_'35selFP4'35entry x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.mergePrimSpecIntoFunc._#selFP4#entry")(x)



c_readPrimSpec :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_readPrimSpec x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesFileExist(x2)(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_readPrimSpec'46_'35lambda48(x1)(x2)))(st)



c_readPrimSpec'46_'35lambda48 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_readPrimSpec'46_'35lambda48 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_62_62_61(Curry.Module.XML.c_readXmlFile(x2)(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_readPrimSpec'46_'35lambda48'46_'35lambda49(x1)))(st)
c_readPrimSpec'46_'35lambda48 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.List)(st)
c_readPrimSpec'46_'35lambda48 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_readPrimSpec'46_'35lambda48(x1)(x2)(x)(st))(i)(xs)(st)
c_readPrimSpec'46_'35lambda48 x1 x2 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.readPrimSpec._#lambda48")(x)



c_readPrimSpec'46_'35lambda48'46_'35lambda49 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.XML.C_XmlExp -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_readPrimSpec'46_'35lambda48'46_'35lambda49 x1 x2 st = Curry.Module.Prelude.c_return(Curry.Module.CompactFlatCurry.c_xml2primtrans(x1)(x2)(st))(st)



c_xml2primtrans :: (Curry t0) => t0 -> Curry.Module.XML.C_XmlExp -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 t0 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_xml2primtrans x1 x2@(Curry.Module.XML.C_XElem x3 x4 x5) st = Curry.Module.CompactFlatCurry.c_xml2primtrans_case_107(x1)(x4)(x5)(x3)(st)
c_xml2primtrans x1 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans(x1)(x)(st))(i)(xs)(st)
c_xml2primtrans x1 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans")(x)



c_xml2primtrans'46xml2prim'46358 :: (Curry t248) => t248 -> Curry.Module.XML.C_XmlExp -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 t248 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_xml2primtrans'46xml2prim'46358 x1 x2@(Curry.Module.XML.C_XElem x3 x4 x5) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_85(x1)(x4)(x5)(x3)(st)
c_xml2primtrans'46xml2prim'46358 x1 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358(x1)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358 x1 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358")(x)



c_xml2primtrans'46xml2prim'46358_case_85 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_84(x1)(x4)(x5)(x7)(x6)(st)
c_xml2primtrans'46xml2prim'46358_case_85 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_85(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_85 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_85")(x)



c_xml2primtrans'46xml2prim'46358_case_84 x1 x4 x5 x7 x6 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_83(x1)(x4)(x5)(x7)(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_22(x1)(x4)(x5)(x7)(st))(Curry.Module.Prelude.c_failed(st))(st))(st)



c_xml2primtrans'46xml2prim'46358_case_22 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x70 x71) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_21(x1)(x4)(x5)(x71)(x70)(st)
c_xml2primtrans'46xml2prim'46358_case_22 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_22(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_22 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_22")(x)



c_xml2primtrans'46xml2prim'46358_case_21 x1 x4 x5 x71 x70 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x70)(Curry.Module.Prelude.C_Char('g'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_20(x1)(x4)(x5)(x71)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_20 x1 x4 x5 x71@((Curry.Module.Prelude.:<) x72 x73) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_19(x1)(x4)(x5)(x73)(x72)(st)
c_xml2primtrans'46xml2prim'46358_case_20 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_20(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_20 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_20")(x)



c_xml2primtrans'46xml2prim'46358_case_19 x1 x4 x5 x73 x72 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x72)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_18(x1)(x4)(x5)(x73)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_18 x1 x4 x5 x73@((Curry.Module.Prelude.:<) x74 x75) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_17(x1)(x4)(x5)(x75)(x74)(st)
c_xml2primtrans'46xml2prim'46358_case_18 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_18(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_18 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_18")(x)



c_xml2primtrans'46xml2prim'46358_case_17 x1 x4 x5 x75 x74 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x74)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_16(x1)(x4)(x5)(x75)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_16 x1 x4 x5 x75@((Curry.Module.Prelude.:<) x76 x77) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_15(x1)(x4)(x5)(x77)(x76)(st)
c_xml2primtrans'46xml2prim'46358_case_16 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_16(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_16 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_16")(x)



c_xml2primtrans'46xml2prim'46358_case_15 x1 x4 x5 x77 x76 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x76)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_14(x1)(x4)(x5)(x77)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_14 x1 x4 x5 x77@((Curry.Module.Prelude.:<) x78 x79) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_13(x1)(x4)(x5)(x79)(x78)(st)
c_xml2primtrans'46xml2prim'46358_case_14 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_14(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_14 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_14")(x)



c_xml2primtrans'46xml2prim'46358_case_13 x1 x4 x5 x79 x78 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x78)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_12(x1)(x4)(x5)(x79)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_12 x1 x4 x5 x79@Curry.Module.Prelude.List st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_11(x1)(x5)(x4)(st)
c_xml2primtrans'46xml2prim'46358_case_12 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_12(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_12 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_12")(x)



c_xml2primtrans'46xml2prim'46358_case_11 x1 x5 x4@((Curry.Module.Prelude.:<) x80 x81) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_10(x1)(x5)(x80)(st)
c_xml2primtrans'46xml2prim'46358_case_11 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_11(x1)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_11 x1 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_11")(x)



c_xml2primtrans'46xml2prim'46358_case_10 x1 x5 x80@(Curry.Module.Prelude.T2 x82 x83) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_9(x1)(x5)(x83)(x82)(st)
c_xml2primtrans'46xml2prim'46358_case_10 x1 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_10(x1)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_10 x1 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_10")(x)



c_xml2primtrans'46xml2prim'46358_case_9 x1 x5 x83 x82@((Curry.Module.Prelude.:<) x84 x85) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_8(x1)(x5)(x83)(x85)(x84)(st)
c_xml2primtrans'46xml2prim'46358_case_9 x1 x5 x83 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_9(x1)(x5)(x83)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_9 x1 x5 x83 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_9")(x)



c_xml2primtrans'46xml2prim'46358_case_8 x1 x5 x83 x85 x84 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x84)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_7(x1)(x5)(x83)(x85)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_7 x1 x5 x83 x85@((Curry.Module.Prelude.:<) x86 x87) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_6(x1)(x5)(x83)(x87)(x86)(st)
c_xml2primtrans'46xml2prim'46358_case_7 x1 x5 x83 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_7(x1)(x5)(x83)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_7 x1 x5 x83 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_7")(x)



c_xml2primtrans'46xml2prim'46358_case_6 x1 x5 x83 x87 x86 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x86)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_5(x1)(x5)(x83)(x87)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_5 x1 x5 x83 x87@((Curry.Module.Prelude.:<) x88 x89) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_4(x1)(x5)(x83)(x89)(x88)(st)
c_xml2primtrans'46xml2prim'46358_case_5 x1 x5 x83 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_5(x1)(x5)(x83)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_5 x1 x5 x83 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_5")(x)



c_xml2primtrans'46xml2prim'46358_case_4 x1 x5 x83 x89 x88 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x88)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_3(x1)(x5)(x83)(x89)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_3 x1 x5 x83 x89@((Curry.Module.Prelude.:<) x90 x91) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_2(x1)(x5)(x83)(x91)(x90)(st)
c_xml2primtrans'46xml2prim'46358_case_3 x1 x5 x83 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_3(x1)(x5)(x83)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_3 x1 x5 x83 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_3")(x)



c_xml2primtrans'46xml2prim'46358_case_2 x1 x5 x83 x91 x90 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x90)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_1(x1)(x5)(x83)(x91)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_1 x1 x5 x83 x91@Curry.Module.Prelude.List st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_0(x1)(x83)(x5)(st)
c_xml2primtrans'46xml2prim'46358_case_1 x1 x5 x83 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_1(x1)(x5)(x83)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_1 x1 x5 x83 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_1")(x)



c_xml2primtrans'46xml2prim'46358_case_0 x1 x83 x5@Curry.Module.Prelude.List st = Curry.Module.Prelude.T2(Curry.Module.Prelude.T2(x1)(x83))(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))
c_xml2primtrans'46xml2prim'46358_case_0 x1 x83 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_0(x1)(x83)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_0 x1 x83 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_0")(x)



c_xml2primtrans'46xml2prim'46358_case_83 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_82(x1)(x4)(x5)(x9)(x8)(st)
c_xml2primtrans'46xml2prim'46358_case_83 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_83(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_83 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_83")(x)



c_xml2primtrans'46xml2prim'46358_case_82 x1 x4 x5 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_81(x1)(x4)(x5)(x9)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_81 x1 x4 x5 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_80(x1)(x4)(x5)(x11)(x10)(st)
c_xml2primtrans'46xml2prim'46358_case_81 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_81(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_81 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_81")(x)



c_xml2primtrans'46xml2prim'46358_case_80 x1 x4 x5 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_79(x1)(x4)(x5)(x11)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_79 x1 x4 x5 x11@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_78(x1)(x4)(x5)(x13)(x12)(st)
c_xml2primtrans'46xml2prim'46358_case_79 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_79(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_79 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_79")(x)



c_xml2primtrans'46xml2prim'46358_case_78 x1 x4 x5 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_77(x1)(x4)(x5)(x13)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_77 x1 x4 x5 x13@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_76(x1)(x4)(x5)(x15)(x14)(st)
c_xml2primtrans'46xml2prim'46358_case_77 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_77(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_77 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_77")(x)



c_xml2primtrans'46xml2prim'46358_case_76 x1 x4 x5 x15 x14 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x14)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_75(x1)(x4)(x5)(x15)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_75 x1 x4 x5 x15@((Curry.Module.Prelude.:<) x16 x17) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_74(x1)(x4)(x5)(x17)(x16)(st)
c_xml2primtrans'46xml2prim'46358_case_75 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_75(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_75 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_75")(x)



c_xml2primtrans'46xml2prim'46358_case_74 x1 x4 x5 x17 x16 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x16)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_73(x1)(x4)(x5)(x17)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_73 x1 x4 x5 x17@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_72(x1)(x4)(x5)(x19)(x18)(st)
c_xml2primtrans'46xml2prim'46358_case_73 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_73(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_73 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_73")(x)



c_xml2primtrans'46xml2prim'46358_case_72 x1 x4 x5 x19 x18 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x18)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_71(x1)(x4)(x5)(x19)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_71 x1 x4 x5 x19@((Curry.Module.Prelude.:<) x20 x21) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_70(x1)(x4)(x5)(x21)(x20)(st)
c_xml2primtrans'46xml2prim'46358_case_71 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_71(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_71 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_71")(x)



c_xml2primtrans'46xml2prim'46358_case_70 x1 x4 x5 x21 x20 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x20)(Curry.Module.Prelude.C_Char('v'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_69(x1)(x4)(x5)(x21)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_69 x1 x4 x5 x21@((Curry.Module.Prelude.:<) x22 x23) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_68(x1)(x4)(x5)(x23)(x22)(st)
c_xml2primtrans'46xml2prim'46358_case_69 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_69(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_69 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_69")(x)



c_xml2primtrans'46xml2prim'46358_case_68 x1 x4 x5 x23 x22 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x22)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_67(x1)(x4)(x5)(x23)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_67 x1 x4 x5 x23@Curry.Module.Prelude.List st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_66(x1)(x5)(x4)(st)
c_xml2primtrans'46xml2prim'46358_case_67 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_67(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_67 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_67")(x)



c_xml2primtrans'46xml2prim'46358_case_66 x1 x5 x4@((Curry.Module.Prelude.:<) x24 x25) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_65(x1)(x5)(x24)(st)
c_xml2primtrans'46xml2prim'46358_case_66 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_66(x1)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_66 x1 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_66")(x)



c_xml2primtrans'46xml2prim'46358_case_65 x1 x5 x24@(Curry.Module.Prelude.T2 x26 x27) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_64(x1)(x5)(x27)(x26)(st)
c_xml2primtrans'46xml2prim'46358_case_65 x1 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_65(x1)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_65 x1 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_65")(x)



c_xml2primtrans'46xml2prim'46358_case_64 x1 x5 x27 x26@((Curry.Module.Prelude.:<) x28 x29) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_63(x1)(x5)(x27)(x29)(x28)(st)
c_xml2primtrans'46xml2prim'46358_case_64 x1 x5 x27 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_64(x1)(x5)(x27)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_64 x1 x5 x27 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_64")(x)



c_xml2primtrans'46xml2prim'46358_case_63 x1 x5 x27 x29 x28 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x28)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_62(x1)(x5)(x27)(x29)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_62 x1 x5 x27 x29@((Curry.Module.Prelude.:<) x30 x31) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_61(x1)(x5)(x27)(x31)(x30)(st)
c_xml2primtrans'46xml2prim'46358_case_62 x1 x5 x27 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_62(x1)(x5)(x27)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_62 x1 x5 x27 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_62")(x)



c_xml2primtrans'46xml2prim'46358_case_61 x1 x5 x27 x31 x30 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x30)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_60(x1)(x5)(x27)(x31)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_60 x1 x5 x27 x31@((Curry.Module.Prelude.:<) x32 x33) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_59(x1)(x5)(x27)(x33)(x32)(st)
c_xml2primtrans'46xml2prim'46358_case_60 x1 x5 x27 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_60(x1)(x5)(x27)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_60 x1 x5 x27 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_60")(x)



c_xml2primtrans'46xml2prim'46358_case_59 x1 x5 x27 x33 x32 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x32)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_58(x1)(x5)(x27)(x33)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_58 x1 x5 x27 x33@((Curry.Module.Prelude.:<) x34 x35) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_57(x1)(x5)(x27)(x35)(x34)(st)
c_xml2primtrans'46xml2prim'46358_case_58 x1 x5 x27 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_58(x1)(x5)(x27)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_58 x1 x5 x27 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_58")(x)



c_xml2primtrans'46xml2prim'46358_case_57 x1 x5 x27 x35 x34 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x34)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_56(x1)(x5)(x27)(x35)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_56 x1 x5 x27 x35@Curry.Module.Prelude.List st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_55(x1)(x27)(x5)(st)
c_xml2primtrans'46xml2prim'46358_case_56 x1 x5 x27 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_56(x1)(x5)(x27)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_56 x1 x5 x27 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_56")(x)



c_xml2primtrans'46xml2prim'46358_case_55 x1 x27 x5@((Curry.Module.Prelude.:<) x36 x37) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_54(x1)(x27)(x37)(x36)(st)
c_xml2primtrans'46xml2prim'46358_case_55 x1 x27 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_55(x1)(x27)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_55 x1 x27 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_55")(x)



c_xml2primtrans'46xml2prim'46358_case_54 x1 x27 x37 x36@(Curry.Module.XML.C_XElem x38 x39 x40) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_53(x1)(x27)(x37)(x39)(x40)(x38)(st)
c_xml2primtrans'46xml2prim'46358_case_54 x1 x27 x37 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_54(x1)(x27)(x37)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_54 x1 x27 x37 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_54")(x)



c_xml2primtrans'46xml2prim'46358_case_53 x1 x27 x37 x39 x40 x38@((Curry.Module.Prelude.:<) x41 x42) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_52(x1)(x27)(x37)(x39)(x40)(x42)(x41)(st)
c_xml2primtrans'46xml2prim'46358_case_53 x1 x27 x37 x39 x40 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_53(x1)(x27)(x37)(x39)(x40)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_53 x1 x27 x37 x39 x40 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_53")(x)



c_xml2primtrans'46xml2prim'46358_case_52 x1 x27 x37 x39 x40 x42 x41 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x41)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_51(x1)(x27)(x37)(x39)(x40)(x42)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_51 x1 x27 x37 x39 x40 x42@((Curry.Module.Prelude.:<) x43 x44) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_50(x1)(x27)(x37)(x39)(x40)(x44)(x43)(st)
c_xml2primtrans'46xml2prim'46358_case_51 x1 x27 x37 x39 x40 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_51(x1)(x27)(x37)(x39)(x40)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_51 x1 x27 x37 x39 x40 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_51")(x)



c_xml2primtrans'46xml2prim'46358_case_50 x1 x27 x37 x39 x40 x44 x43 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x43)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_49(x1)(x27)(x37)(x39)(x40)(x44)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_49 x1 x27 x37 x39 x40 x44@((Curry.Module.Prelude.:<) x45 x46) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_48(x1)(x27)(x37)(x39)(x40)(x46)(x45)(st)
c_xml2primtrans'46xml2prim'46358_case_49 x1 x27 x37 x39 x40 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_49(x1)(x27)(x37)(x39)(x40)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_49 x1 x27 x37 x39 x40 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_49")(x)



c_xml2primtrans'46xml2prim'46358_case_48 x1 x27 x37 x39 x40 x46 x45 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x45)(Curry.Module.Prelude.C_Char('b'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_47(x1)(x27)(x37)(x39)(x40)(x46)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_47 x1 x27 x37 x39 x40 x46@((Curry.Module.Prelude.:<) x47 x48) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_46(x1)(x27)(x37)(x39)(x40)(x48)(x47)(st)
c_xml2primtrans'46xml2prim'46358_case_47 x1 x27 x37 x39 x40 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_47(x1)(x27)(x37)(x39)(x40)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_47 x1 x27 x37 x39 x40 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_47")(x)



c_xml2primtrans'46xml2prim'46358_case_46 x1 x27 x37 x39 x40 x48 x47 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x47)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_45(x1)(x27)(x37)(x39)(x40)(x48)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_45 x1 x27 x37 x39 x40 x48@((Curry.Module.Prelude.:<) x49 x50) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_44(x1)(x27)(x37)(x39)(x40)(x50)(x49)(st)
c_xml2primtrans'46xml2prim'46358_case_45 x1 x27 x37 x39 x40 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_45(x1)(x27)(x37)(x39)(x40)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_45 x1 x27 x37 x39 x40 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_45")(x)



c_xml2primtrans'46xml2prim'46358_case_44 x1 x27 x37 x39 x40 x50 x49 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x49)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_43(x1)(x27)(x37)(x39)(x40)(x50)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_43 x1 x27 x37 x39 x40 x50@((Curry.Module.Prelude.:<) x51 x52) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_42(x1)(x27)(x37)(x39)(x40)(x52)(x51)(st)
c_xml2primtrans'46xml2prim'46358_case_43 x1 x27 x37 x39 x40 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_43(x1)(x27)(x37)(x39)(x40)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_43 x1 x27 x37 x39 x40 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_43")(x)



c_xml2primtrans'46xml2prim'46358_case_42 x1 x27 x37 x39 x40 x52 x51 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x51)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_41(x1)(x27)(x37)(x39)(x40)(x52)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_41 x1 x27 x37 x39 x40 x52@((Curry.Module.Prelude.:<) x53 x54) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_40(x1)(x27)(x37)(x39)(x40)(x54)(x53)(st)
c_xml2primtrans'46xml2prim'46358_case_41 x1 x27 x37 x39 x40 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_41(x1)(x27)(x37)(x39)(x40)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_41 x1 x27 x37 x39 x40 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_41")(x)



c_xml2primtrans'46xml2prim'46358_case_40 x1 x27 x37 x39 x40 x54 x53 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x53)(Curry.Module.Prelude.C_Char('y'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_39(x1)(x27)(x37)(x39)(x40)(x54)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_39 x1 x27 x37 x39 x40 x54@Curry.Module.Prelude.List st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_38(x1)(x27)(x37)(x40)(x39)(st)
c_xml2primtrans'46xml2prim'46358_case_39 x1 x27 x37 x39 x40 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_39(x1)(x27)(x37)(x39)(x40)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_39 x1 x27 x37 x39 x40 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_39")(x)



c_xml2primtrans'46xml2prim'46358_case_38 x1 x27 x37 x40 x39@Curry.Module.Prelude.List st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_37(x1)(x27)(x40)(x37)(st)
c_xml2primtrans'46xml2prim'46358_case_38 x1 x27 x37 x40 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_38(x1)(x27)(x37)(x40)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_38 x1 x27 x37 x40 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_38")(x)



c_xml2primtrans'46xml2prim'46358_case_37 x1 x27 x40 x37@((Curry.Module.Prelude.:<) x55 x56) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_36(x1)(x27)(x40)(x56)(x55)(st)
c_xml2primtrans'46xml2prim'46358_case_37 x1 x27 x40 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_37(x1)(x27)(x40)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_37 x1 x27 x40 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_37")(x)



c_xml2primtrans'46xml2prim'46358_case_36 x1 x27 x40 x56 x55@(Curry.Module.XML.C_XElem x57 x58 x59) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_35(x1)(x27)(x40)(x56)(x58)(x59)(x57)(st)
c_xml2primtrans'46xml2prim'46358_case_36 x1 x27 x40 x56 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_36(x1)(x27)(x40)(x56)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_36 x1 x27 x40 x56 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_36")(x)



c_xml2primtrans'46xml2prim'46358_case_35 x1 x27 x40 x56 x58 x59 x57@((Curry.Module.Prelude.:<) x60 x61) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_34(x1)(x27)(x40)(x56)(x58)(x59)(x61)(x60)(st)
c_xml2primtrans'46xml2prim'46358_case_35 x1 x27 x40 x56 x58 x59 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_35(x1)(x27)(x40)(x56)(x58)(x59)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_35 x1 x27 x40 x56 x58 x59 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_35")(x)



c_xml2primtrans'46xml2prim'46358_case_34 x1 x27 x40 x56 x58 x59 x61 x60 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x60)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_33(x1)(x27)(x40)(x56)(x58)(x59)(x61)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_33 x1 x27 x40 x56 x58 x59 x61@((Curry.Module.Prelude.:<) x62 x63) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_32(x1)(x27)(x40)(x56)(x58)(x59)(x63)(x62)(st)
c_xml2primtrans'46xml2prim'46358_case_33 x1 x27 x40 x56 x58 x59 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_33(x1)(x27)(x40)(x56)(x58)(x59)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_33 x1 x27 x40 x56 x58 x59 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_33")(x)



c_xml2primtrans'46xml2prim'46358_case_32 x1 x27 x40 x56 x58 x59 x63 x62 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x62)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_31(x1)(x27)(x40)(x56)(x58)(x59)(x63)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_31 x1 x27 x40 x56 x58 x59 x63@((Curry.Module.Prelude.:<) x64 x65) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_30(x1)(x27)(x40)(x56)(x58)(x59)(x65)(x64)(st)
c_xml2primtrans'46xml2prim'46358_case_31 x1 x27 x40 x56 x58 x59 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_31(x1)(x27)(x40)(x56)(x58)(x59)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_31 x1 x27 x40 x56 x58 x59 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_31")(x)



c_xml2primtrans'46xml2prim'46358_case_30 x1 x27 x40 x56 x58 x59 x65 x64 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x64)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_29(x1)(x27)(x40)(x56)(x58)(x59)(x65)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_29 x1 x27 x40 x56 x58 x59 x65@((Curry.Module.Prelude.:<) x66 x67) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_28(x1)(x27)(x40)(x56)(x58)(x59)(x67)(x66)(st)
c_xml2primtrans'46xml2prim'46358_case_29 x1 x27 x40 x56 x58 x59 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_29(x1)(x27)(x40)(x56)(x58)(x59)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_29 x1 x27 x40 x56 x58 x59 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_29")(x)



c_xml2primtrans'46xml2prim'46358_case_28 x1 x27 x40 x56 x58 x59 x67 x66 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x66)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_27(x1)(x27)(x40)(x56)(x58)(x59)(x67)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_27 x1 x27 x40 x56 x58 x59 x67@((Curry.Module.Prelude.:<) x68 x69) st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_26(x1)(x27)(x40)(x56)(x58)(x59)(x69)(x68)(st)
c_xml2primtrans'46xml2prim'46358_case_27 x1 x27 x40 x56 x58 x59 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_27(x1)(x27)(x40)(x56)(x58)(x59)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_27 x1 x27 x40 x56 x58 x59 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_27")(x)



c_xml2primtrans'46xml2prim'46358_case_26 x1 x27 x40 x56 x58 x59 x69 x68 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x68)(Curry.Module.Prelude.C_Char('y'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_25(x1)(x27)(x40)(x56)(x58)(x59)(x69)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans'46xml2prim'46358_case_25 x1 x27 x40 x56 x58 x59 x69@Curry.Module.Prelude.List st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_24(x1)(x27)(x40)(x56)(x59)(x58)(st)
c_xml2primtrans'46xml2prim'46358_case_25 x1 x27 x40 x56 x58 x59 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_25(x1)(x27)(x40)(x56)(x58)(x59)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_25 x1 x27 x40 x56 x58 x59 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_25")(x)



c_xml2primtrans'46xml2prim'46358_case_24 x1 x27 x40 x56 x59 x58@Curry.Module.Prelude.List st = Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_23(x1)(x27)(x40)(x59)(x56)(st)
c_xml2primtrans'46xml2prim'46358_case_24 x1 x27 x40 x56 x59 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_24(x1)(x27)(x40)(x56)(x59)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_24 x1 x27 x40 x56 x59 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_24")(x)



c_xml2primtrans'46xml2prim'46358_case_23 x1 x27 x40 x59 x56@Curry.Module.Prelude.List st = Curry.Module.Prelude.T2(Curry.Module.Prelude.T2(x1)(x27))(Curry.Module.Prelude.T2(Curry.Module.XML.c_textOfXml(x40)(st))(Curry.Module.XML.c_textOfXml(x59)(st)))
c_xml2primtrans'46xml2prim'46358_case_23 x1 x27 x40 x59 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358_case_23(x1)(x27)(x40)(x59)(x)(st))(i)(xs)(st)
c_xml2primtrans'46xml2prim'46358_case_23 x1 x27 x40 x59 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans.xml2prim.358_case_23")(x)



c_xml2primtrans_case_107 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CompactFlatCurry.c_xml2primtrans_case_106(x1)(x4)(x5)(x7)(x6)(st)
c_xml2primtrans_case_107 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans_case_107(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans_case_107 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans_case_107")(x)



c_xml2primtrans_case_106 x1 x4 x5 x7 x6 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans_case_105(x1)(x4)(x5)(x7)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans_case_105 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CompactFlatCurry.c_xml2primtrans_case_104(x1)(x4)(x5)(x9)(x8)(st)
c_xml2primtrans_case_105 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans_case_105(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans_case_105 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans_case_105")(x)



c_xml2primtrans_case_104 x1 x4 x5 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans_case_103(x1)(x4)(x5)(x9)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans_case_103 x1 x4 x5 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CompactFlatCurry.c_xml2primtrans_case_102(x1)(x4)(x5)(x11)(x10)(st)
c_xml2primtrans_case_103 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans_case_103(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans_case_103 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans_case_103")(x)



c_xml2primtrans_case_102 x1 x4 x5 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans_case_101(x1)(x4)(x5)(x11)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans_case_101 x1 x4 x5 x11@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CompactFlatCurry.c_xml2primtrans_case_100(x1)(x4)(x5)(x13)(x12)(st)
c_xml2primtrans_case_101 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans_case_101(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans_case_101 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans_case_101")(x)



c_xml2primtrans_case_100 x1 x4 x5 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans_case_99(x1)(x4)(x5)(x13)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans_case_99 x1 x4 x5 x13@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.CompactFlatCurry.c_xml2primtrans_case_98(x1)(x4)(x5)(x15)(x14)(st)
c_xml2primtrans_case_99 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans_case_99(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans_case_99 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans_case_99")(x)



c_xml2primtrans_case_98 x1 x4 x5 x15 x14 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x14)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans_case_97(x1)(x4)(x5)(x15)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans_case_97 x1 x4 x5 x15@((Curry.Module.Prelude.:<) x16 x17) st = Curry.Module.CompactFlatCurry.c_xml2primtrans_case_96(x1)(x4)(x5)(x17)(x16)(st)
c_xml2primtrans_case_97 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans_case_97(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans_case_97 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans_case_97")(x)



c_xml2primtrans_case_96 x1 x4 x5 x17 x16 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x16)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans_case_95(x1)(x4)(x5)(x17)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans_case_95 x1 x4 x5 x17@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.CompactFlatCurry.c_xml2primtrans_case_94(x1)(x4)(x5)(x19)(x18)(st)
c_xml2primtrans_case_95 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans_case_95(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans_case_95 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans_case_95")(x)



c_xml2primtrans_case_94 x1 x4 x5 x19 x18 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x18)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans_case_93(x1)(x4)(x5)(x19)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans_case_93 x1 x4 x5 x19@((Curry.Module.Prelude.:<) x20 x21) st = Curry.Module.CompactFlatCurry.c_xml2primtrans_case_92(x1)(x4)(x5)(x21)(x20)(st)
c_xml2primtrans_case_93 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans_case_93(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans_case_93 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans_case_93")(x)



c_xml2primtrans_case_92 x1 x4 x5 x21 x20 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x20)(Curry.Module.Prelude.C_Char('v'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans_case_91(x1)(x4)(x5)(x21)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans_case_91 x1 x4 x5 x21@((Curry.Module.Prelude.:<) x22 x23) st = Curry.Module.CompactFlatCurry.c_xml2primtrans_case_90(x1)(x4)(x5)(x23)(x22)(st)
c_xml2primtrans_case_91 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans_case_91(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans_case_91 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans_case_91")(x)



c_xml2primtrans_case_90 x1 x4 x5 x23 x22 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x22)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans_case_89(x1)(x4)(x5)(x23)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans_case_89 x1 x4 x5 x23@((Curry.Module.Prelude.:<) x24 x25) st = Curry.Module.CompactFlatCurry.c_xml2primtrans_case_88(x1)(x4)(x5)(x25)(x24)(st)
c_xml2primtrans_case_89 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans_case_89(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans_case_89 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans_case_89")(x)



c_xml2primtrans_case_88 x1 x4 x5 x25 x24 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x24)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CompactFlatCurry.c_xml2primtrans_case_87(x1)(x4)(x5)(x25)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_xml2primtrans_case_87 x1 x4 x5 x25@Curry.Module.Prelude.List st = Curry.Module.CompactFlatCurry.c_xml2primtrans_case_86(x1)(x5)(x4)(st)
c_xml2primtrans_case_87 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans_case_87(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans_case_87 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans_case_87")(x)



c_xml2primtrans_case_86 x1 x5 x4@Curry.Module.Prelude.List st = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_xml2primtrans'46xml2prim'46358(x1)))(x5)(st)
c_xml2primtrans_case_86 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_xml2primtrans_case_86(x1)(x5)(x)(st))(i)(xs)(st)
c_xml2primtrans_case_86 x1 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.xml2primtrans_case_86")(x)



c_mergePrimSpecIntoFunc'46_'35selFP4'35entry_case_108 x2@(Curry.Module.Prelude.T2 x3 x4) st = x4
c_mergePrimSpecIntoFunc'46_'35selFP4'35entry_case_108 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_mergePrimSpecIntoFunc'46_'35selFP4'35entry_case_108(x)(st))(i)(xs)(st)
c_mergePrimSpecIntoFunc'46_'35selFP4'35entry_case_108 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.mergePrimSpecIntoFunc._#selFP4#entry_case_108")(x)



c_mergePrimSpecIntoFunc'46_'35selFP3'35lib_case_109 x2@(Curry.Module.Prelude.T2 x3 x4) st = x3
c_mergePrimSpecIntoFunc'46_'35selFP3'35lib_case_109 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_mergePrimSpecIntoFunc'46_'35selFP3'35lib_case_109(x)(st))(i)(xs)(st)
c_mergePrimSpecIntoFunc'46_'35selFP3'35lib_case_109 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.mergePrimSpecIntoFunc._#selFP3#lib_case_109")(x)



c_mergePrimSpecIntoFunc_case_111 x3 x4 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.FlatCurry.C_Func(x3)(x4)(x5)(x6)(x7))(Curry.Module.Prelude.List)
c_mergePrimSpecIntoFunc_case_111 x3 x4 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CompactFlatCurry.c_mergePrimSpecIntoFunc'46_'35selFP4'35entry(x8)(st)} in Curry.Module.CompactFlatCurry.c_mergePrimSpecIntoFunc_case_110(x3)(x4)(x5)(x6)(x8)(x11)(Curry.Module.Prelude.c_null(x11)(st))(st)
c_mergePrimSpecIntoFunc_case_111 x3 x4 x5 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_mergePrimSpecIntoFunc_case_111(x3)(x4)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c_mergePrimSpecIntoFunc_case_111 x3 x4 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.mergePrimSpecIntoFunc_case_111")(x)



c_mergePrimSpecIntoFunc_case_110 x3 x4 x5 x6 x8 x11 x12@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.List
c_mergePrimSpecIntoFunc_case_110 x3 x4 x5 x6 x8 x11 x12@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(Curry.Module.FlatCurry.C_Func(x3)(x4)(x5)(x6)(Curry.Module.FlatCurry.C_External(Curry.Module.Prelude.op_43_43(Curry.Module.CompactFlatCurry.c_mergePrimSpecIntoFunc'46_'35selFP3'35lib(x8)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(x11))(st))))(Curry.Module.Prelude.List)
c_mergePrimSpecIntoFunc_case_110 x3 x4 x5 x6 x8 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_mergePrimSpecIntoFunc_case_110(x3)(x4)(x5)(x6)(x8)(x11)(x)(st))(i)(xs)(st)
c_mergePrimSpecIntoFunc_case_110 x3 x4 x5 x6 x8 x11 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.mergePrimSpecIntoFunc_case_110")(x)



c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41'46_'35lambda42'46_'35lambda43_case_112 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_62_62_61(Curry.Module.FlatCurry.c_readFlatCurry(x2)(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_processPrimitives(x2)))(st)
c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41'46_'35lambda42'46_'35lambda43_case_112 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_62_62_61(Curry.Module.FlatCurry.c_readFlatCurryFile(Curry.Module.FlatCurry.c_flatCurryFileName(x2)(st))(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_processPrimitives(x2)))(st)
c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41'46_'35lambda42'46_'35lambda43_case_112 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41'46_'35lambda42'46_'35lambda43_case_112(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41'46_'35lambda42'46_'35lambda43_case_112 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.readCurrentFlatCurry._#lambda40._#lambda41._#lambda42._#lambda43_case_112")(x)



c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41_case_113 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_62_62_61(Curry.Module.FlatCurry.c_readFlatCurry(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_processPrimitives(x1)))(st)
c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41_case_113 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_62_62_61(Curry.Module.CompactFlatCurry.c_getSourceModificationTime(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41'46_'35lambda42(x1)))(st)
c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41_case_113 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41_case_113(x1)(x2)(x)(st))(i)(xs)(st)
c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41_case_113 x1 x2 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.readCurrentFlatCurry._#lambda40._#lambda41_case_113")(x)



c_leqQName_case_114 x3 x4 x2@(Curry.Module.Prelude.T2 x5 x6) st = let {x7 = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Sort.c_cmpString(st))(x3)(st))(x5)(st)} in Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_61_61(x7)(Curry.Module.Prelude.C_LT)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x7)(Curry.Module.Prelude.C_EQ)(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Sort.c_leqString(st))(x4)(st))(x6)(st))(st))(st)
c_leqQName_case_114 x3 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_leqQName_case_114(x3)(x4)(x)(st))(i)(xs)(st)
c_leqQName_case_114 x3 x4 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.leqQName_case_114")(x)



c_allConsOfExpr'46consOfBranch'46252_case_115 x3 x2@(Curry.Module.FlatCurry.C_LPattern x4) st = Curry.Module.CompactFlatCurry.c_allConsOfExpr(x3)(st)
c_allConsOfExpr'46consOfBranch'46252_case_115 x3 x2@(Curry.Module.FlatCurry.C_Pattern x5 x6) st = Curry.Module.List.c_union((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.CompactFlatCurry.c_allConsOfExpr(x3)(st))(st)
c_allConsOfExpr'46consOfBranch'46252_case_115 x3 (Curry.Module.FlatCurry.C_PatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_allConsOfExpr'46consOfBranch'46252_case_115(x3)(x)(st))(i)(xs)(st)
c_allConsOfExpr'46consOfBranch'46252_case_115 x3 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.allConsOfExpr.consOfBranch.252_case_115")(x)



c_allConsOfExpr_case_116 x5 x7 x4@Curry.Module.FlatCurry.C_ConsCall st = (Curry.Module.Prelude.:<)(x5)(x7)
c_allConsOfExpr_case_116 x5 x7 x4@(Curry.Module.FlatCurry.C_ConsPartCall x8) st = (Curry.Module.Prelude.:<)(x5)(x7)
c_allConsOfExpr_case_116 x5 x7 x4@Curry.Module.FlatCurry.C_FuncCall st = x7
c_allConsOfExpr_case_116 x5 x7 x4@(Curry.Module.FlatCurry.C_FuncPartCall x9) st = x7
c_allConsOfExpr_case_116 x5 x7 (Curry.Module.FlatCurry.C_CombTypeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_allConsOfExpr_case_116(x5)(x7)(x)(st))(i)(xs)(st)
c_allConsOfExpr_case_116 x5 x7 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.allConsOfExpr_case_116")(x)



c_allConstructorsOfFunc_case_117 x6@(Curry.Module.FlatCurry.C_External x7) st = Curry.Module.Prelude.List
c_allConstructorsOfFunc_case_117 x6@(Curry.Module.FlatCurry.C_Rule x8 x9) st = Curry.Module.CompactFlatCurry.c_allConsOfExpr(x9)(st)
c_allConstructorsOfFunc_case_117 (Curry.Module.FlatCurry.C_RuleOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_allConstructorsOfFunc_case_117(x)(st))(i)(xs)(st)
c_allConstructorsOfFunc_case_117 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.allConstructorsOfFunc_case_117")(x)



c_allFuncCallsOfExpr_case_118 x5 x7 x4@Curry.Module.FlatCurry.C_FuncCall st = (Curry.Module.Prelude.:<)(x5)(x7)
c_allFuncCallsOfExpr_case_118 x5 x7 x4@(Curry.Module.FlatCurry.C_FuncPartCall x8) st = (Curry.Module.Prelude.:<)(x5)(x7)
c_allFuncCallsOfExpr_case_118 x5 x7 x4@Curry.Module.FlatCurry.C_ConsCall st = x7
c_allFuncCallsOfExpr_case_118 x5 x7 x4@(Curry.Module.FlatCurry.C_ConsPartCall x9) st = x7
c_allFuncCallsOfExpr_case_118 x5 x7 (Curry.Module.FlatCurry.C_CombTypeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_allFuncCallsOfExpr_case_118(x5)(x7)(x)(st))(i)(xs)(st)
c_allFuncCallsOfExpr_case_118 x5 x7 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.allFuncCallsOfExpr_case_118")(x)



c_allFuncCalls_case_119 x6@(Curry.Module.FlatCurry.C_External x7) st = Curry.Module.Prelude.List
c_allFuncCalls_case_119 x6@(Curry.Module.FlatCurry.C_Rule x8 x9) st = Curry.Module.List.c_nub(Curry.Module.CompactFlatCurry.c_allFuncCallsOfExpr(x9)(st))(st)
c_allFuncCalls_case_119 (Curry.Module.FlatCurry.C_RuleOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_allFuncCalls_case_119(x)(st))(i)(xs)(st)
c_allFuncCalls_case_119 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.allFuncCalls_case_119")(x)



c_getCalledFuncs_case_123 x1 x2 x3 x4 x5 x6 x7 x10 x9@(Curry.Module.Prelude.T2 x11 x12) st = Curry.Module.CompactFlatCurry.c_getCalledFuncs_case_122(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x10)(x11)(x12)(Curry.Module.Prelude.c_not(Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_elemRBT(x11)(st))(x2)(st))(st))(st)
c_getCalledFuncs_case_123 x1 x2 x3 x4 x5 x6 x7 x10 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_getCalledFuncs_case_123(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x10)(x)(st))(i)(xs)(st)
c_getCalledFuncs_case_123 x1 x2 x3 x4 x5 x6 x7 x10 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.getCalledFuncs_case_123")(x)



c_getCalledFuncs_case_122 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x13@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_62_62_61(Curry.Module.CompactFlatCurry.c_readCurrentFlatCurry(x11)(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_getCalledFuncs'46_'35lambda34(x12)(x10)(x4)(x6)(x5)(x2)(x7)(x11)(x3)(x1)))(st)
c_getCalledFuncs_case_122 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x13@Curry.Module.Prelude.C_False st = Curry.Module.CompactFlatCurry.c_getCalledFuncs_case_121(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x10)(x11)(x12)(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_apply(Curry.Module.TableRBT.c_lookupRBT(Curry.Module.Prelude.T2(x11)(x12))(st))(x4)(st))(Curry.Module.Prelude.C_Nothing)(st))(st)
c_getCalledFuncs_case_122 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_getCalledFuncs_case_122(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x10)(x11)(x12)(x)(st))(i)(xs)(st)
c_getCalledFuncs_case_122 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.getCalledFuncs_case_122")(x)



c_getCalledFuncs_case_121 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x13@Curry.Module.Prelude.C_True st = Curry.Module.CompactFlatCurry.c_getCalledFuncs(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x10)(st)
c_getCalledFuncs_case_121 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x13@Curry.Module.Prelude.C_False st = Curry.Module.CompactFlatCurry.c_getCalledFuncs_case_120(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x10)(x11)(x12)(Curry.Module.Prelude.c_otherwise(st))(st)
c_getCalledFuncs_case_121 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_getCalledFuncs_case_121(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x10)(x11)(x12)(x)(st))(i)(xs)(st)
c_getCalledFuncs_case_121 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.getCalledFuncs_case_121")(x)



c_getCalledFuncs_case_120 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x13@Curry.Module.Prelude.C_True st = let {x13 = Curry.Module.Maybe.c_fromJust(Curry.Module.Prelude.c_apply(Curry.Module.TableRBT.c_lookupRBT(Curry.Module.Prelude.T2(x11)(x12))(st))(x4)(st))(st)} in let {x15 = Curry.Module.Prelude.c_filter(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_getCalledFuncs'46_'35lambda35(x5)))(Curry.Module.CompactFlatCurry.c_allFuncCalls(x13)(st))(st)} in let {x16 = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_getImplicitlyRequired(x1)))(st))(x15)(st)} in let {x17 = Curry.Module.CompactFlatCurry.c_allConstructorsOfFunc(x13)(st)} in Curry.Module.Prelude.op_62_62_61(Curry.Module.CompactFlatCurry.c_getCalledFuncs(x1)(x2)(x3)(x4)(Curry.Module.Prelude.c_foldr(Curry.Module.SetRBT.c_insertRBT(st))(x5)(Curry.Module.Prelude.op_43_43(x15)(x16)(st))(st))(Curry.Module.Prelude.c_foldr(Curry.Module.SetRBT.c_insertRBT(st))(x6)(x17)(st))(Curry.Module.Prelude.c_foldr(Curry.Module.SetRBT.c_insertRBT(st))(x7)(Curry.Module.CompactFlatCurry.c_allTypesOfFunc(x13)(st))(st))(Curry.Module.Prelude.op_43_43(x10)(Curry.Module.Prelude.op_43_43(x15)(Curry.Module.Prelude.op_43_43(x16)(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_getCalledFuncs'46_'35lambda36(x6)))(x17)(st))(st))(st))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_getCalledFuncs'46_'35lambda37(x13)))(st)
c_getCalledFuncs_case_120 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_getCalledFuncs_case_120(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x10)(x11)(x12)(x)(st))(i)(xs)(st)
c_getCalledFuncs_case_120 x1 x2 x3 x4 x5 x6 x7 x10 x11 x12 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.getCalledFuncs_case_120")(x)



c_requiredInCompactProg_case_128 x1 x2 x3 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_mapIO(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_readCurrentFlatCurry))(st))(x3)(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_requiredInCompactProg'46_'35lambda28(x3)(x5)(x1)(x7)))(st)
c_requiredInCompactProg_case_128 x1 x2 x3 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.CompactFlatCurry.c_requiredInCompactProg_case_127(x1)(x2)(x3)(x4)(x6)(x7)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(Curry.Module.CompactFlatCurry.C_Exports)(st))(x2)(st))(st)
c_requiredInCompactProg_case_128 x1 x2 x3 x4 x5 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_requiredInCompactProg_case_128(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c_requiredInCompactProg_case_128 x1 x2 x3 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.requiredInCompactProg_case_128")(x)



c_requiredInCompactProg_case_127 x1 x2 x3 x4 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_mapIO(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_readCurrentFlatCurry))(st))(x3)(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_requiredInCompactProg'46_'35lambda29(x3)(x6)(x1)(x7)))(st)
c_requiredInCompactProg_case_127 x1 x2 x3 x4 x6 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.CompactFlatCurry.c_requiredInCompactProg_case_126(x1)(x2)(x3)(x4)(x6)(x7)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_any(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_isMainOption))(st))(x2)(st))(st)
c_requiredInCompactProg_case_127 x1 x2 x3 x4 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_requiredInCompactProg_case_127(x1)(x2)(x3)(x4)(x6)(x7)(x)(st))(i)(xs)(st)
c_requiredInCompactProg_case_127 x1 x2 x3 x4 x6 x7 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.requiredInCompactProg_case_127")(x)



c_requiredInCompactProg_case_126 x1 x2 x3 x4 x6 x7 x8@Curry.Module.Prelude.C_True st = let {x8 = Curry.Module.CompactFlatCurry.c_getMainFuncFromOptions(x2)(st)} in Curry.Module.CompactFlatCurry.c_requiredInCompactProg_case_125(x1)(x3)(x4)(x7)(x8)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(Curry.Module.Prelude.T2(x4)(x8))(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_functionName))(Curry.Module.CompactFlatCurry.c_moduleFuns(x1)(st))(st))(st))(st)
c_requiredInCompactProg_case_126 x1 x2 x3 x4 x6 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.CompactFlatCurry.c_requiredInCompactProg_case_124(x1)(x3)(x6)(x7)(Curry.Module.Prelude.c_otherwise(st))(st)
c_requiredInCompactProg_case_126 x1 x2 x3 x4 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_requiredInCompactProg_case_126(x1)(x2)(x3)(x4)(x6)(x7)(x)(st))(i)(xs)(st)
c_requiredInCompactProg_case_126 x1 x2 x3 x4 x6 x7 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.requiredInCompactProg_case_126")(x)



c_requiredInCompactProg_case_124 x1 x3 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_mapIO(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_readCurrentFlatCurry))(st))(Curry.Module.List.c_nub(Curry.Module.Prelude.op_43_43(x3)(Curry.Module.CompactFlatCurry.c_moduleImports(x1)(st))(st))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_requiredInCompactProg'46_'35lambda31(x6)(x1)(x7)))(st)
c_requiredInCompactProg_case_124 x1 x3 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_requiredInCompactProg_case_124(x1)(x3)(x6)(x7)(x)(st))(i)(xs)(st)
c_requiredInCompactProg_case_124 x1 x3 x6 x7 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.requiredInCompactProg_case_124")(x)



c_requiredInCompactProg_case_125 x1 x3 x4 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_mapIO(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_readCurrentFlatCurry))(st))(x3)(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_requiredInCompactProg'46_'35lambda30(x8)(x3)(x1)(x4)(x7)))(st)
c_requiredInCompactProg_case_125 x1 x3 x4 x7 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_error))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(Curry.Module.Prelude.op_43_43(x8)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))(Curry.Module.Prelude.List)))(st))(st))(st)
c_requiredInCompactProg_case_125 x1 x3 x4 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_requiredInCompactProg_case_125(x1)(x3)(x4)(x7)(x8)(x)(st))(i)(xs)(st)
c_requiredInCompactProg_case_125 x1 x3 x4 x7 x8 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.requiredInCompactProg_case_125")(x)



c_extendTConsWithConsType_case_130 x1 x2 x5 x4@(Curry.Module.FlatCurry.C_TypeSyn x6 x7 x8 x9) st = Curry.Module.CompactFlatCurry.c_extendTConsWithConsType(x1)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_insertRBT(st))(x6)(st))(x2)(st))(x5)(st)
c_extendTConsWithConsType_case_130 x1 x2 x5 x4@(Curry.Module.FlatCurry.C_Type x10 x11 x12 x13) st = Curry.Module.CompactFlatCurry.c_extendTConsWithConsType_case_129(x1)(x2)(x5)(x10)(x13)(Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(x10)(st))(Curry.Module.CompactFlatCurry.c_defaultRequiredTypes(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_any(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_extendTConsWithConsType'46_'35lambda18(x1)))(st))(x13)(st))(st))(st)
c_extendTConsWithConsType_case_130 x1 x2 x5 (Curry.Module.FlatCurry.C_TypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_extendTConsWithConsType_case_130(x1)(x2)(x5)(x)(st))(i)(xs)(st)
c_extendTConsWithConsType_case_130 x1 x2 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.extendTConsWithConsType_case_130")(x)



c_extendTConsWithConsType_case_129 x1 x2 x5 x10 x13 x14@Curry.Module.Prelude.C_True st = Curry.Module.CompactFlatCurry.c_extendTConsWithConsType(x1)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_insertRBT(st))(x10)(st))(x2)(st))(x5)(st)
c_extendTConsWithConsType_case_129 x1 x2 x5 x10 x13 x14@Curry.Module.Prelude.C_False st = Curry.Module.CompactFlatCurry.c_extendTConsWithConsType(x1)(x2)(x5)(st)
c_extendTConsWithConsType_case_129 x1 x2 x5 x10 x13 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_extendTConsWithConsType_case_129(x1)(x2)(x5)(x10)(x13)(x)(st))(i)(xs)(st)
c_extendTConsWithConsType_case_129 x1 x2 x5 x10 x13 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.extendTConsWithConsType_case_129")(x)



c_newTypeConsOfTDecl_case_131 x1 x7 x10 x11@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_filter(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_newTypeConsOfTDecl'46_'35lambda16(x1)))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_newTypeConsOfTDecl'46_'35lambda17))(st))(x10)(st))(st)
c_newTypeConsOfTDecl_case_131 x1 x7 x10 x11@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.List
c_newTypeConsOfTDecl_case_131 x1 x7 x10 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_newTypeConsOfTDecl_case_131(x1)(x7)(x10)(x)(st))(i)(xs)(st)
c_newTypeConsOfTDecl_case_131 x1 x7 x10 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.newTypeConsOfTDecl_case_131")(x)



c_newTypeConsOfTDecl_case_132 x1 x3 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_filter(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_newTypeConsOfTDecl'46_'35lambda15(x1)))(Curry.Module.CompactFlatCurry.c_allTypesOfTExpr(x6)(st))(st)
c_newTypeConsOfTDecl_case_132 x1 x3 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.List
c_newTypeConsOfTDecl_case_132 x1 x3 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_newTypeConsOfTDecl_case_132(x1)(x3)(x6)(x)(st))(i)(xs)(st)
c_newTypeConsOfTDecl_case_132 x1 x3 x6 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.newTypeConsOfTDecl_case_132")(x)



c_requiredDatatypes_case_133 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = x1
c_requiredDatatypes_case_133 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.CompactFlatCurry.c_requiredDatatypes(Curry.Module.Prelude.c_foldr(Curry.Module.SetRBT.c_insertRBT(st))(x1)(x3)(st))(x2)(st)
c_requiredDatatypes_case_133 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_requiredDatatypes_case_133(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_requiredDatatypes_case_133 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.requiredDatatypes_case_133")(x)



c_computeCompactFlatCurry_case_134 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))(st)
c_computeCompactFlatCurry_case_134 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_putStr((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))))))))(st))(Curry.Module.Prelude.op_62_62_61(Curry.Module.CompactFlatCurry.c_readCurrentFlatCurry(x2)(st))(Curry.Module.Prelude.pf(Curry.Module.CompactFlatCurry.c_computeCompactFlatCurry'46_'35lambda9(x3)))(st))(st)
c_computeCompactFlatCurry_case_134 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_computeCompactFlatCurry_case_134(x2)(x3)(x)(st))(i)(xs)(st)
c_computeCompactFlatCurry_case_134 x2 x3 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.computeCompactFlatCurry_case_134")(x)



c_getImplicitlyRequired'46getImpReq'4646_case_135 x1 x4 x5 x6@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)
c_getImplicitlyRequired'46getImpReq'4646_case_135 x1 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.List
c_getImplicitlyRequired'46getImpReq'4646_case_135 x1 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_getImplicitlyRequired'46getImpReq'4646_case_135(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_getImplicitlyRequired'46getImpReq'4646_case_135 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.getImplicitlyRequired.getImpReq.46_case_135")(x)



c_getRequiredInModule'46getImpReq'4638_case_137 x1 x3@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.CompactFlatCurry.c_getRequiredInModule'46getImpReq'4638_case_136(x1)(x4)(x5)(Curry.Module.Prelude.op_61_61(x4)(x1)(st))(st)
c_getRequiredInModule'46getImpReq'4638_case_137 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_getRequiredInModule'46getImpReq'4638_case_137(x1)(x)(st))(i)(xs)(st)
c_getRequiredInModule'46getImpReq'4638_case_137 x1 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.getRequiredInModule.getImpReq.38_case_137")(x)



c_getRequiredInModule'46getImpReq'4638_case_136 x1 x4 x5 x6@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(x4)(x5))(Curry.Module.Prelude.List)
c_getRequiredInModule'46getImpReq'4638_case_136 x1 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.List
c_getRequiredInModule'46getImpReq'4638_case_136 x1 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_getRequiredInModule'46getImpReq'4638_case_136(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_getRequiredInModule'46getImpReq'4638_case_136 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.getRequiredInModule.getImpReq.38_case_136")(x)



c_addImport2Options'46alwaysReqMod'4621_case_138 x2@(Curry.Module.Prelude.T2 x3 x4) st = (Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List)
c_addImport2Options'46alwaysReqMod'4621_case_138 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_addImport2Options'46alwaysReqMod'4621_case_138(x)(st))(i)(xs)(st)
c_addImport2Options'46alwaysReqMod'4621_case_138 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.addImport2Options.alwaysReqMod.21_case_138")(x)



c_getMainFuncFromOptions_case_139 x3 x2@(Curry.Module.CompactFlatCurry.C_Main x4) st = x4
c_getMainFuncFromOptions_case_139 x3 x2@Curry.Module.CompactFlatCurry.C_Verbose st = Curry.Module.CompactFlatCurry.c_getMainFuncFromOptions(x3)(st)
c_getMainFuncFromOptions_case_139 x3 x2@Curry.Module.CompactFlatCurry.C_Exports st = Curry.Module.CompactFlatCurry.c_getMainFuncFromOptions(x3)(st)
c_getMainFuncFromOptions_case_139 x3 x2@(Curry.Module.CompactFlatCurry.C_InitFuncs x5) st = Curry.Module.CompactFlatCurry.c_getMainFuncFromOptions(x3)(st)
c_getMainFuncFromOptions_case_139 x3 x2@(Curry.Module.CompactFlatCurry.C_Required x6) st = Curry.Module.CompactFlatCurry.c_getMainFuncFromOptions(x3)(st)
c_getMainFuncFromOptions_case_139 x3 x2@(Curry.Module.CompactFlatCurry.C_Import x7) st = Curry.Module.CompactFlatCurry.c_getMainFuncFromOptions(x3)(st)
c_getMainFuncFromOptions_case_139 x3 (Curry.Module.CompactFlatCurry.C_OptionOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CompactFlatCurry.c_getMainFuncFromOptions_case_139(x3)(x)(st))(i)(xs)(st)
c_getMainFuncFromOptions_case_139 x3 x st = Curry.RunTimeSystem.patternFail("CompactFlatCurry.getMainFuncFromOptions_case_139")(x)


