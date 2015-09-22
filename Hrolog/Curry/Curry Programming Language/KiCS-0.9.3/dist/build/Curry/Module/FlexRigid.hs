{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.FlexRigid (module Curry.Module.FlexRigid) where

import Curry.RunTimeSystem
import Curry.Module.FlatCurry
import Curry.Module.Prelude



-- begin included



-- end included

data C_FlexRigidResult = C_UnknownFR
  | C_ConflictFR
  | C_KnownFlex
  | C_KnownRigid
  | C_FlexRigidResultFail Curry.RunTimeSystem.C_Exceptions
  | C_FlexRigidResultOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.FlexRigid.C_FlexRigidResult)

instance BaseCurry Curry.Module.FlexRigid.C_FlexRigidResult where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.FlexRigid.C_FlexRigidResultOr(Curry.RunTimeSystem.mkRef(r)(0)(i))([Curry.Module.FlexRigid.C_UnknownFR,Curry.Module.FlexRigid.C_ConflictFR,Curry.Module.FlexRigid.C_KnownFlex,Curry.Module.FlexRigid.C_KnownRigid]))(0)

  failed  = Curry.Module.FlexRigid.C_FlexRigidResultFail

  branching  = Curry.Module.FlexRigid.C_FlexRigidResultOr

  consKind (Curry.Module.FlexRigid.C_FlexRigidResultOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.FlexRigid.C_FlexRigidResultFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.FlexRigid.C_FlexRigidResultFail x) = x

  orRef (Curry.Module.FlexRigid.C_FlexRigidResultOr x _) = x

  branches (Curry.Module.FlexRigid.C_FlexRigidResultOr _ x) = x





instance Curry Curry.Module.FlexRigid.C_FlexRigidResult where
  strEq Curry.Module.FlexRigid.C_UnknownFR Curry.Module.FlexRigid.C_UnknownFR st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.FlexRigid.C_ConflictFR Curry.Module.FlexRigid.C_ConflictFR st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.FlexRigid.C_KnownFlex Curry.Module.FlexRigid.C_KnownFlex st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.FlexRigid.C_KnownRigid Curry.Module.FlexRigid.C_KnownRigid st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.FlexRigid.C_UnknownFR Curry.Module.FlexRigid.C_UnknownFR st = Curry.Module.Prelude.C_True
  eq Curry.Module.FlexRigid.C_ConflictFR Curry.Module.FlexRigid.C_ConflictFR st = Curry.Module.Prelude.C_True
  eq Curry.Module.FlexRigid.C_KnownFlex Curry.Module.FlexRigid.C_KnownFlex st = Curry.Module.Prelude.C_True
  eq Curry.Module.FlexRigid.C_KnownRigid Curry.Module.FlexRigid.C_KnownRigid st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.FlexRigid.C_UnknownFR st = Curry.Module.FlexRigid.C_UnknownFR
  propagate f Curry.Module.FlexRigid.C_ConflictFR st = Curry.Module.FlexRigid.C_ConflictFR
  propagate f Curry.Module.FlexRigid.C_KnownFlex st = Curry.Module.FlexRigid.C_KnownFlex
  propagate f Curry.Module.FlexRigid.C_KnownRigid st = Curry.Module.FlexRigid.C_KnownRigid

  foldCurry f c Curry.Module.FlexRigid.C_UnknownFR st = c
  foldCurry f c Curry.Module.FlexRigid.C_ConflictFR st = c
  foldCurry f c Curry.Module.FlexRigid.C_KnownFlex st = c
  foldCurry f c Curry.Module.FlexRigid.C_KnownRigid st = c

  typeName _ = "FlexRigidResult"

  showQ _ Curry.Module.FlexRigid.C_UnknownFR = Prelude.showString("FlexRigid.UnknownFR")
  showQ _ Curry.Module.FlexRigid.C_ConflictFR = Prelude.showString("FlexRigid.ConflictFR")
  showQ _ Curry.Module.FlexRigid.C_KnownFlex = Prelude.showString("FlexRigid.KnownFlex")
  showQ _ Curry.Module.FlexRigid.C_KnownRigid = Prelude.showString("FlexRigid.KnownRigid")
  showQ _ (Curry.Module.FlexRigid.C_FlexRigidResultOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.FlexRigid.C_FlexRigidResult where
  showsPrec _ Curry.Module.FlexRigid.C_UnknownFR = Prelude.showString("UnknownFR")
  showsPrec _ Curry.Module.FlexRigid.C_ConflictFR = Prelude.showString("ConflictFR")
  showsPrec _ Curry.Module.FlexRigid.C_KnownFlex = Prelude.showString("KnownFlex")
  showsPrec _ Curry.Module.FlexRigid.C_KnownRigid = Prelude.showString("KnownRigid")
  showsPrec _ (Curry.Module.FlexRigid.C_FlexRigidResultOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Read Curry.Module.FlexRigid.C_FlexRigidResult where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.FlexRigid.C_UnknownFR)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlexRigid")("UnknownFR")(r)])(r))((Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.FlexRigid.C_ConflictFR)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlexRigid")("ConflictFR")(r)])(r))((Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.FlexRigid.C_KnownFlex)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlexRigid")("KnownFlex")(r)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.FlexRigid.C_KnownRigid)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("FlexRigid")("KnownRigid")(r)])(r))))





c_getFlexRigid :: Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlexRigid.C_FlexRigidResult
c_getFlexRigid x1@(Curry.Module.FlatCurry.C_Var x2) st = Curry.Module.FlexRigid.C_UnknownFR
c_getFlexRigid x1@(Curry.Module.FlatCurry.C_Lit x3) st = Curry.Module.FlexRigid.C_UnknownFR
c_getFlexRigid x1@(Curry.Module.FlatCurry.C_Comb x4 x5 x6) st = Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.FlexRigid.c_joinCaseTypes))(Curry.Module.FlexRigid.C_UnknownFR)(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.FlexRigid.c_getFlexRigid))(x6)(st))(st)
c_getFlexRigid x1@(Curry.Module.FlatCurry.C_Let x7 x8) st = Curry.Module.FlexRigid.c_getFlexRigid(x8)(st)
c_getFlexRigid x1@(Curry.Module.FlatCurry.C_Free x9 x10) st = Curry.Module.FlexRigid.c_getFlexRigid(x10)(st)
c_getFlexRigid x1@(Curry.Module.FlatCurry.C_Or x11 x12) st = Curry.Module.FlexRigid.c_joinCaseTypes(Curry.Module.FlexRigid.c_getFlexRigid(x11)(st))(Curry.Module.FlexRigid.c_getFlexRigid(x12)(st))(st)
c_getFlexRigid x1@(Curry.Module.FlatCurry.C_Case x13 x14 x15) st = Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.FlexRigid.c_joinCaseTypes))(Curry.Module.FlexRigid.c_getFlexRigid_case_4(x13)(Curry.Module.Prelude.op_61_61(x13)(Curry.Module.FlatCurry.C_Flex)(st))(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.FlexRigid.c_getFlexRigid))((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.FlexRigid.c_getFlexRigid'46_'35lambda2))(x15)(st)))(st))(st)
c_getFlexRigid (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlexRigid.c_getFlexRigid(x)(st))(i)(xs)(st)
c_getFlexRigid x st = Curry.RunTimeSystem.patternFail("FlexRigid.getFlexRigid")(x)



c_getFlexRigid'46_'35lambda2 :: Curry.Module.FlatCurry.C_BranchExpr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_getFlexRigid'46_'35lambda2 x1@(Curry.Module.FlatCurry.C_Branch x2 x3) st = x3
c_getFlexRigid'46_'35lambda2 (Curry.Module.FlatCurry.C_BranchExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlexRigid.c_getFlexRigid'46_'35lambda2(x)(st))(i)(xs)(st)
c_getFlexRigid'46_'35lambda2 x st = Curry.RunTimeSystem.patternFail("FlexRigid.getFlexRigid._#lambda2")(x)



c_joinCaseTypes :: Curry.Module.FlexRigid.C_FlexRigidResult -> Curry.Module.FlexRigid.C_FlexRigidResult -> Curry.RunTimeSystem.State -> Curry.Module.FlexRigid.C_FlexRigidResult
c_joinCaseTypes x1@Curry.Module.FlexRigid.C_ConflictFR x2 st = Curry.Module.FlexRigid.c_joinCaseTypes_case_3(x2)(st)
c_joinCaseTypes x1@Curry.Module.FlexRigid.C_UnknownFR x2 st = Curry.Module.FlexRigid.c_joinCaseTypes_case_2(x2)(st)
c_joinCaseTypes x1@Curry.Module.FlexRigid.C_KnownFlex x2 st = Curry.Module.FlexRigid.c_joinCaseTypes_case_1(x2)(st)
c_joinCaseTypes x1@Curry.Module.FlexRigid.C_KnownRigid x2 st = Curry.Module.FlexRigid.c_joinCaseTypes_case_0(x2)(st)
c_joinCaseTypes (Curry.Module.FlexRigid.C_FlexRigidResultOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlexRigid.c_joinCaseTypes(x)(x2)(st))(i)(xs)(st)
c_joinCaseTypes x x2 st = Curry.RunTimeSystem.patternFail("FlexRigid.joinCaseTypes")(x)



c_joinCaseTypes_case_0 x2@Curry.Module.FlexRigid.C_ConflictFR st = Curry.Module.FlexRigid.C_ConflictFR
c_joinCaseTypes_case_0 x2@Curry.Module.FlexRigid.C_UnknownFR st = Curry.Module.FlexRigid.C_KnownRigid
c_joinCaseTypes_case_0 x2@Curry.Module.FlexRigid.C_KnownFlex st = Curry.Module.FlexRigid.C_ConflictFR
c_joinCaseTypes_case_0 x2@Curry.Module.FlexRigid.C_KnownRigid st = Curry.Module.FlexRigid.C_KnownRigid
c_joinCaseTypes_case_0 (Curry.Module.FlexRigid.C_FlexRigidResultOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlexRigid.c_joinCaseTypes_case_0(x)(st))(i)(xs)(st)
c_joinCaseTypes_case_0 x st = Curry.RunTimeSystem.patternFail("FlexRigid.joinCaseTypes_case_0")(x)



c_joinCaseTypes_case_1 x2@Curry.Module.FlexRigid.C_ConflictFR st = Curry.Module.FlexRigid.C_ConflictFR
c_joinCaseTypes_case_1 x2@Curry.Module.FlexRigid.C_UnknownFR st = Curry.Module.FlexRigid.C_KnownFlex
c_joinCaseTypes_case_1 x2@Curry.Module.FlexRigid.C_KnownFlex st = Curry.Module.FlexRigid.C_KnownFlex
c_joinCaseTypes_case_1 x2@Curry.Module.FlexRigid.C_KnownRigid st = Curry.Module.FlexRigid.C_ConflictFR
c_joinCaseTypes_case_1 (Curry.Module.FlexRigid.C_FlexRigidResultOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlexRigid.c_joinCaseTypes_case_1(x)(st))(i)(xs)(st)
c_joinCaseTypes_case_1 x st = Curry.RunTimeSystem.patternFail("FlexRigid.joinCaseTypes_case_1")(x)



c_joinCaseTypes_case_2 x2@Curry.Module.FlexRigid.C_ConflictFR st = Curry.Module.FlexRigid.C_ConflictFR
c_joinCaseTypes_case_2 x2@Curry.Module.FlexRigid.C_UnknownFR st = Curry.Module.FlexRigid.C_UnknownFR
c_joinCaseTypes_case_2 x2@Curry.Module.FlexRigid.C_KnownFlex st = Curry.Module.FlexRigid.C_KnownFlex
c_joinCaseTypes_case_2 x2@Curry.Module.FlexRigid.C_KnownRigid st = Curry.Module.FlexRigid.C_KnownRigid
c_joinCaseTypes_case_2 (Curry.Module.FlexRigid.C_FlexRigidResultOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlexRigid.c_joinCaseTypes_case_2(x)(st))(i)(xs)(st)
c_joinCaseTypes_case_2 x st = Curry.RunTimeSystem.patternFail("FlexRigid.joinCaseTypes_case_2")(x)



c_joinCaseTypes_case_3 x2@Curry.Module.FlexRigid.C_ConflictFR st = Curry.Module.FlexRigid.C_ConflictFR
c_joinCaseTypes_case_3 x2@Curry.Module.FlexRigid.C_UnknownFR st = Curry.Module.FlexRigid.C_ConflictFR
c_joinCaseTypes_case_3 x2@Curry.Module.FlexRigid.C_KnownFlex st = Curry.Module.FlexRigid.C_ConflictFR
c_joinCaseTypes_case_3 x2@Curry.Module.FlexRigid.C_KnownRigid st = Curry.Module.FlexRigid.C_ConflictFR
c_joinCaseTypes_case_3 (Curry.Module.FlexRigid.C_FlexRigidResultOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlexRigid.c_joinCaseTypes_case_3(x)(st))(i)(xs)(st)
c_joinCaseTypes_case_3 x st = Curry.RunTimeSystem.patternFail("FlexRigid.joinCaseTypes_case_3")(x)



c_getFlexRigid_case_4 x13 x14@Curry.Module.Prelude.C_True st = Curry.Module.FlexRigid.C_KnownFlex
c_getFlexRigid_case_4 x13 x14@Curry.Module.Prelude.C_False st = Curry.Module.FlexRigid.C_KnownRigid
c_getFlexRigid_case_4 x13 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlexRigid.c_getFlexRigid_case_4(x13)(x)(st))(i)(xs)(st)
c_getFlexRigid_case_4 x13 x st = Curry.RunTimeSystem.patternFail("FlexRigid.getFlexRigid_case_4")(x)


