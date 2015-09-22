{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Array (module Curry.Module.Array) where

import Curry.RunTimeSystem
import Curry.Module.Integer
import Curry.Module.Prelude



-- begin included



-- end included

data C_Array t0 = C_Array (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t0)) (Curry.Module.Array.C_Entry t0)
  | C_ArrayFail Curry.RunTimeSystem.C_Exceptions
  | C_ArrayOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Array.C_Array t0))

data C_Entry t0 = C_Entry t0 (Curry.Module.Array.C_Entry t0) (Curry.Module.Array.C_Entry t0)
  | C_Empty
  | C_EntryFail Curry.RunTimeSystem.C_Exceptions
  | C_EntryOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Array.C_Entry t0))

instance (BaseCurry t0) => BaseCurry (Curry.Module.Array.C_Array t0) where
  nf f (Curry.Module.Array.C_Array x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.Array.C_Array(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Array.C_Array x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.Array.C_Array(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Array.C_ArrayOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.Array.C_Array(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.Array.C_ArrayFail

  branching  = Curry.Module.Array.C_ArrayOr

  consKind (Curry.Module.Array.C_ArrayOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Array.C_ArrayFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Array.C_ArrayFail x) = x

  orRef (Curry.Module.Array.C_ArrayOr x _) = x

  branches (Curry.Module.Array.C_ArrayOr _ x) = x





instance (BaseCurry t0) => BaseCurry (Curry.Module.Array.C_Entry t0) where
  nf f (Curry.Module.Array.C_Entry x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.Array.C_Entry(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Array.C_Entry x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.Array.C_Entry(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Array.C_EntryOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.Array.C_Entry(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.Array.C_Empty]))(3)

  failed  = Curry.Module.Array.C_EntryFail

  branching  = Curry.Module.Array.C_EntryOr

  consKind (Curry.Module.Array.C_EntryOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Array.C_EntryFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Array.C_EntryFail x) = x

  orRef (Curry.Module.Array.C_EntryOr x _) = x

  branches (Curry.Module.Array.C_EntryOr _ x) = x





instance (Curry t0) => Curry (Curry.Module.Array.C_Array t0) where
  strEq (Curry.Module.Array.C_Array x1 x2) (Curry.Module.Array.C_Array y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Array.C_Array x1 x2) (Curry.Module.Array.C_Array y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Array.C_Array x1 x2) st = Curry.Module.Array.C_Array(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.Array.C_Array x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "Array"

  showQ d (Curry.Module.Array.C_Array x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Array.Array "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.Array.C_ArrayOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0) => Curry (Curry.Module.Array.C_Entry t0) where
  strEq (Curry.Module.Array.C_Entry x1 x2 x3) (Curry.Module.Array.C_Entry y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq Curry.Module.Array.C_Empty Curry.Module.Array.C_Empty st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Array.C_Entry x1 x2 x3) (Curry.Module.Array.C_Entry y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq Curry.Module.Array.C_Empty Curry.Module.Array.C_Empty st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Array.C_Entry x1 x2 x3) st = Curry.Module.Array.C_Entry(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))
  propagate f Curry.Module.Array.C_Empty st = Curry.Module.Array.C_Empty

  foldCurry f c (Curry.Module.Array.C_Entry x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)
  foldCurry f c Curry.Module.Array.C_Empty st = c

  typeName _ = "Entry"

  showQ d (Curry.Module.Array.C_Entry x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Array.Entry "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ _ Curry.Module.Array.C_Empty = Prelude.showString("Array.Empty")
  showQ _ (Curry.Module.Array.C_EntryOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.Array.C_Array t0) where
  showsPrec d (Curry.Module.Array.C_Array x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Array "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.Array.C_ArrayOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.Array.C_Entry t0) where
  showsPrec d (Curry.Module.Array.C_Entry x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Entry "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec _ Curry.Module.Array.C_Empty = Prelude.showString("Empty")
  showsPrec _ (Curry.Module.Array.C_EntryOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Read t0) => Read (Curry.Module.Array.C_Array t0) where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Array.C_Array(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Array")("Array")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r)





instance (Read t0) => Read (Curry.Module.Array.C_Entry t0) where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Array.C_Entry(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Array")("Entry")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Array.C_Empty)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Array")("Empty")(r)])(r))





c_emptyErrorArray :: (Curry t0) => Curry.RunTimeSystem.State -> Curry.Module.Array.C_Array t0
c_emptyErrorArray st = Curry.Module.Array.c_emptyDefaultArray(Curry.Module.Prelude.pf(Curry.Module.Array.c_errorArray))(st)



c_errorArray :: (Curry t0) => Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t0
c_errorArray x1 st = Curry.Module.Prelude.c_error(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_show(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('z'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))(Curry.Module.Prelude.List)))))))))))))))))(st))(st))(st)



c_emptyDefaultArray :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Array.C_Array t0
c_emptyDefaultArray x1 st = Curry.Module.Array.C_Array(x1)(Curry.Module.Array.C_Empty)



op_47_47 :: (Curry t0) => (Curry.Module.Array.C_Array t0) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Array.C_Array t0
op_47_47 x1@(Curry.Module.Array.C_Array x3 x4) x2 st = Curry.Module.Array.C_Array(x3)(Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Array.c_'47'47'46_'35lambda2(x3)))(x4)(x2)(st))
op_47_47 (Curry.Module.Array.C_ArrayOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.op_47_47(x)(x2)(st))(i)(xs)(st)
op_47_47 x x2 st = Curry.RunTimeSystem.patternFail("Array.//")(x)



c_'47'47'46_'35lambda2 :: (Curry t72) => (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t72)) -> (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t72) -> (Curry.Module.Array.C_Entry t72) -> Curry.RunTimeSystem.State -> Curry.Module.Array.C_Entry t72
c_'47'47'46_'35lambda2 x1 x2@(Curry.Module.Prelude.T2 x4 x5) x3 st = Curry.Module.Array.c_at(Curry.Module.Prelude.c_apply(x1)(x4)(st))(x3)(x4)(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_const(x5)))(st)
c_'47'47'46_'35lambda2 x1 (Curry.Module.Prelude.T2Or i xs) x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_'47'47'46_'35lambda2(x1)(x)(x3)(st))(i)(xs)(st)
c_'47'47'46_'35lambda2 x1 x x3 st = Curry.RunTimeSystem.patternFail("Array.//._#lambda2")(x)



c_update :: (Curry t0) => (Curry.Module.Array.C_Array t0) -> Curry.Module.Prelude.C_Int -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Array.C_Array t0
c_update x1@(Curry.Module.Array.C_Array x4 x5) x2 x3 st = Curry.Module.Array.C_Array(x4)(Curry.Module.Array.c_at(Curry.Module.Prelude.c_apply(x4)(x2)(st))(x5)(x2)(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_const(x3)))(st))
c_update (Curry.Module.Array.C_ArrayOr i xs) x2 x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_update(x)(x2)(x3)(st))(i)(xs)(st)
c_update x x2 x3 st = Curry.RunTimeSystem.patternFail("Array.update")(x)



c_applyAt :: (Curry t0) => (Curry.Module.Array.C_Array t0) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Array.C_Array t0
c_applyAt x1@(Curry.Module.Array.C_Array x4 x5) x2 x3 st = Curry.Module.Array.C_Array(x4)(Curry.Module.Array.c_at(Curry.Module.Prelude.c_apply(x4)(x2)(st))(x5)(x2)(x3)(st))
c_applyAt (Curry.Module.Array.C_ArrayOr i xs) x2 x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_applyAt(x)(x2)(x3)(st))(i)(xs)(st)
c_applyAt x x2 x3 st = Curry.RunTimeSystem.patternFail("Array.applyAt")(x)



c_at :: (Curry t0) => t0 -> (Curry.Module.Array.C_Entry t0) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Array.C_Entry t0
c_at x1 x2@Curry.Module.Array.C_Empty x3 x4 st = Curry.Module.Array.c_at_case_14(x1)(x3)(x4)(Curry.Module.Prelude.op_61_61(x3)(Curry.Module.Prelude.C_Zero)(st))(st)
c_at x1 x2@(Curry.Module.Array.C_Entry x5 x6 x7) x3 x4 st = Curry.Module.Array.c_at_case_11(x1)(x3)(x4)(x5)(x6)(x7)(Curry.Module.Prelude.op_61_61(x3)(Curry.Module.Prelude.C_Zero)(st))(st)
c_at x1 (Curry.Module.Array.C_EntryOr i xs) x3 x4 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_at(x1)(x)(x3)(x4)(st))(i)(xs)(st)
c_at x1 x x3 x4 st = Curry.RunTimeSystem.patternFail("Array.at")(x)



op_33 :: (Curry t0) => (Curry.Module.Array.C_Array t0) -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t0
op_33 x1@(Curry.Module.Array.C_Array x3 x4) x2 st = Curry.Module.Array.c_from(Curry.Module.Prelude.c_apply(x3)(x2)(st))(x4)(x2)(st)
op_33 (Curry.Module.Array.C_ArrayOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.op_33(x)(x2)(st))(i)(xs)(st)
op_33 x x2 st = Curry.RunTimeSystem.patternFail("Array.!")(x)



c_from :: (Curry t0) => t0 -> (Curry.Module.Array.C_Entry t0) -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t0
c_from x1 x2@Curry.Module.Array.C_Empty x3 st = x1
c_from x1 x2@(Curry.Module.Array.C_Entry x4 x5 x6) x3 st = Curry.Module.Array.c_from_case_8(x1)(x3)(x4)(x5)(x6)(Curry.Module.Prelude.op_61_61(x3)(Curry.Module.Prelude.C_Zero)(st))(st)
c_from x1 (Curry.Module.Array.C_EntryOr i xs) x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_from(x1)(x)(x3)(st))(i)(xs)(st)
c_from x1 x x3 st = Curry.RunTimeSystem.patternFail("Array.from")(x)



c_split :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t0)
c_split x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)
c_split x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.Array.c_split_case_5(x2)(x3)(st)
c_split (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_split(x)(st))(i)(xs)(st)
c_split x st = Curry.RunTimeSystem.patternFail("Array.split")(x)



c_split'46_'35selFP3'35xs :: (Curry t126) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t126) (Curry.Module.Prelude.List t126)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t126
c_split'46_'35selFP3'35xs x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_split'46_'35selFP3'35xs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_split'46_'35selFP3'35xs(x)(st))(i)(xs)(st)
c_split'46_'35selFP3'35xs x st = Curry.RunTimeSystem.patternFail("Array.split._#selFP3#xs")(x)



c_split'46_'35selFP4'35ys :: (Curry t126) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t126) (Curry.Module.Prelude.List t126)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t126
c_split'46_'35selFP4'35ys x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_split'46_'35selFP4'35ys (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_split'46_'35selFP4'35ys(x)(st))(i)(xs)(st)
c_split'46_'35selFP4'35ys x st = Curry.RunTimeSystem.patternFail("Array.split._#selFP4#ys")(x)



c_listToDefaultArray :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Array.C_Array t0)
c_listToDefaultArray x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc(Curry.Module.Array.C_Array(x1)))(Curry.Module.Prelude.pf(Curry.Module.Array.c_listToArray))(st)



c_listToErrorArray :: (Curry t0) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Array.C_Array t0)
c_listToErrorArray st = Curry.Module.Array.c_listToDefaultArray(Curry.Module.Prelude.pf(Curry.Module.Array.c_errorArray))(st)



c_listToArray :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Array.C_Entry t0
c_listToArray x1@Curry.Module.Prelude.List st = Curry.Module.Array.C_Empty
c_listToArray x1@((Curry.Module.Prelude.:<) x2 x3) st = let {x4 = Curry.Module.Array.c_split(x3)(st)} in Curry.Module.Array.C_Entry(x2)(Curry.Module.Array.c_listToArray(Curry.Module.Array.c_listToArray'46_'35selFP6'35ys(x4)(st))(st))(Curry.Module.Array.c_listToArray(Curry.Module.Array.c_listToArray'46_'35selFP7'35zs(x4)(st))(st))
c_listToArray (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_listToArray(x)(st))(i)(xs)(st)
c_listToArray x st = Curry.RunTimeSystem.patternFail("Array.listToArray")(x)



c_listToArray'46_'35selFP6'35ys :: (Curry t137) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t137) (Curry.Module.Prelude.List t137)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t137
c_listToArray'46_'35selFP6'35ys x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_listToArray'46_'35selFP6'35ys (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_listToArray'46_'35selFP6'35ys(x)(st))(i)(xs)(st)
c_listToArray'46_'35selFP6'35ys x st = Curry.RunTimeSystem.patternFail("Array.listToArray._#selFP6#ys")(x)



c_listToArray'46_'35selFP7'35zs :: (Curry t137) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t137) (Curry.Module.Prelude.List t137)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t137
c_listToArray'46_'35selFP7'35zs x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_listToArray'46_'35selFP7'35zs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_listToArray'46_'35selFP7'35zs(x)(st))(i)(xs)(st)
c_listToArray'46_'35selFP7'35zs x st = Curry.RunTimeSystem.patternFail("Array.listToArray._#selFP7#zs")(x)



c_combine :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t2))) -> (Curry.Module.Array.C_Array t0) -> (Curry.Module.Array.C_Array t1) -> Curry.RunTimeSystem.State -> Curry.Module.Array.C_Array t2
c_combine x1 x2@(Curry.Module.Array.C_Array x4 x5) x3 st = Curry.Module.Array.c_combine_case_4(x1)(x4)(x5)(x3)(st)
c_combine x1 (Curry.Module.Array.C_ArrayOr i xs) x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_combine(x1)(x)(x3)(st))(i)(xs)(st)
c_combine x1 x x3 st = Curry.RunTimeSystem.patternFail("Array.combine")(x)



c_combine'46_'35lambda3 :: (Curry t252,Curry t255,Curry t263) => (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t252)) -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t255)) -> (Curry.Module.Prelude.Prim (t252 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t255 -> Curry.RunTimeSystem.State -> t263))) -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t263
c_combine'46_'35lambda3 x1 x2 x3 x4 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x3)(Curry.Module.Prelude.c_apply(x1)(x4)(st))(st))(Curry.Module.Prelude.c_apply(x2)(x4)(st))(st)



c_comb :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t2))) -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t0)) -> (Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t1)) -> (Curry.Module.Array.C_Entry t0) -> (Curry.Module.Array.C_Entry t1) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Array.C_Entry t2
c_comb x1 x2 x3 x4@Curry.Module.Array.C_Empty x5 x6 x7 st = Curry.Module.Array.c_comb_case_3(x1)(x2)(x3)(x6)(x7)(x5)(st)
c_comb x1 x2 x3 x4@(Curry.Module.Array.C_Entry x11 x12 x13) x5 x6 x7 st = Curry.Module.Array.c_comb_case_2(x1)(x2)(x3)(x6)(x7)(x11)(x12)(x13)(x5)(st)
c_comb x1 x2 x3 (Curry.Module.Array.C_EntryOr i xs) x5 x6 x7 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_comb(x1)(x2)(x3)(x)(x5)(x6)(x7)(st))(i)(xs)(st)
c_comb x1 x2 x3 x x5 x6 x7 st = Curry.RunTimeSystem.patternFail("Array.comb")(x)



c_combineSimilar :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0))) -> (Curry.Module.Array.C_Array t0) -> (Curry.Module.Array.C_Array t0) -> Curry.RunTimeSystem.State -> Curry.Module.Array.C_Array t0
c_combineSimilar x1 x2@(Curry.Module.Array.C_Array x4 x5) x3 st = Curry.Module.Array.c_combineSimilar_case_1(x1)(x4)(x5)(x3)(st)
c_combineSimilar x1 (Curry.Module.Array.C_ArrayOr i xs) x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_combineSimilar(x1)(x)(x3)(st))(i)(xs)(st)
c_combineSimilar x1 x x3 st = Curry.RunTimeSystem.patternFail("Array.combineSimilar")(x)



c_combSim :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0))) -> (Curry.Module.Array.C_Entry t0) -> (Curry.Module.Array.C_Entry t0) -> Curry.RunTimeSystem.State -> Curry.Module.Array.C_Entry t0
c_combSim x1 x2@Curry.Module.Array.C_Empty x3 st = x3
c_combSim x1 x2@(Curry.Module.Array.C_Entry x4 x5 x6) x3 st = Curry.Module.Array.c_combSim_case_0(x1)(x2)(x4)(x5)(x6)(x3)(st)
c_combSim x1 (Curry.Module.Array.C_EntryOr i xs) x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_combSim(x1)(x)(x3)(st))(i)(xs)(st)
c_combSim x1 x x3 st = Curry.RunTimeSystem.patternFail("Array.combSim")(x)



c_foldArray :: (Curry t0,Curry t1) => t0 -> (Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0)))) -> (Curry.Module.Array.C_Array t1) -> Curry.RunTimeSystem.State -> t0
c_foldArray x1 x2 x3@(Curry.Module.Array.C_Array x4 x5) st = Curry.Module.Array.c_foldEntries(x1)(x2)(x5)(st)
c_foldArray x1 x2 (Curry.Module.Array.C_ArrayOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_foldArray(x1)(x2)(x)(st))(i)(xs)(st)
c_foldArray x1 x2 x st = Curry.RunTimeSystem.patternFail("Array.foldArray")(x)



c_foldEntries :: (Curry t0,Curry t1) => t0 -> (Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0)))) -> (Curry.Module.Array.C_Entry t1) -> Curry.RunTimeSystem.State -> t0
c_foldEntries x1 x2 x3@Curry.Module.Array.C_Empty st = x1
c_foldEntries x1 x2 x3@(Curry.Module.Array.C_Entry x4 x5 x6) st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x2)(x4)(st))(Curry.Module.Array.c_foldEntries(x1)(x2)(x5)(st))(st))(Curry.Module.Array.c_foldEntries(x1)(x2)(x6)(st))(st)
c_foldEntries x1 x2 (Curry.Module.Array.C_EntryOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_foldEntries(x1)(x2)(x)(st))(i)(xs)(st)
c_foldEntries x1 x2 x st = Curry.RunTimeSystem.patternFail("Array.foldEntries")(x)



c_arrayToList :: (Curry t0) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Array.C_Array t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0)
c_arrayToList st = Curry.Module.Prelude.pf(Curry.Module.Array.c_foldArray(Curry.Module.Prelude.List)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Array.c_arrayToList'46_'35lambda4)))



c_arrayToList'46_'35lambda4 :: (Curry t345) => t345 -> (Curry.Module.Prelude.List t345) -> (Curry.Module.Prelude.List t345) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t345
c_arrayToList'46_'35lambda4 x1 x2 x3 st = (Curry.Module.Prelude.:<)(x1)(Curry.Module.Prelude.op_43_43(x2)(x3)(st))



c_combSim_case_0 x1 x2 x4 x5 x6 x3@Curry.Module.Array.C_Empty st = x2
c_combSim_case_0 x1 x2 x4 x5 x6 x3@(Curry.Module.Array.C_Entry x7 x8 x9) st = Curry.Module.Array.C_Entry(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x4)(st))(x7)(st))(Curry.Module.Array.c_combSim(x1)(x5)(x8)(st))(Curry.Module.Array.c_combSim(x1)(x6)(x9)(st))
c_combSim_case_0 x1 x2 x4 x5 x6 (Curry.Module.Array.C_EntryOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_combSim_case_0(x1)(x2)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_combSim_case_0 x1 x2 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("Array.combSim_case_0")(x)



c_combineSimilar_case_1 x1 x4 x5 x3@(Curry.Module.Array.C_Array x6 x7) st = Curry.Module.Array.C_Array(x4)(Curry.Module.Array.c_combSim(x1)(x5)(x7)(st))
c_combineSimilar_case_1 x1 x4 x5 (Curry.Module.Array.C_ArrayOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_combineSimilar_case_1(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_combineSimilar_case_1 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("Array.combineSimilar_case_1")(x)



c_comb_case_2 x1 x2 x3 x6 x7 x11 x12 x13 x5@Curry.Module.Array.C_Empty st = Curry.Module.Array.C_Entry(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x11)(st))(Curry.Module.Prelude.c_apply(x3)(Curry.Module.Prelude.op_45(Curry.Module.Prelude.op_43(x6)(x7)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st))(Curry.Module.Array.c_comb(x1)(x2)(x3)(x12)(Curry.Module.Array.C_Empty)(Curry.Module.Prelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x6)(st))(x7)(st))(Curry.Module.Array.c_comb(x1)(x2)(x3)(x13)(Curry.Module.Array.C_Empty)(Curry.Module.Prelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x6)(st))(Curry.Module.Prelude.op_43(x7)(x6)(st))(st))
c_comb_case_2 x1 x2 x3 x6 x7 x11 x12 x13 x5@(Curry.Module.Array.C_Entry x14 x15 x16) st = Curry.Module.Array.C_Entry(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x11)(st))(x14)(st))(Curry.Module.Array.c_comb(x1)(x2)(x3)(x12)(x15)(Curry.Module.Prelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x6)(st))(x7)(st))(Curry.Module.Array.c_comb(x1)(x2)(x3)(x13)(x16)(Curry.Module.Prelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x6)(st))(Curry.Module.Prelude.op_43(x7)(x6)(st))(st))
c_comb_case_2 x1 x2 x3 x6 x7 x11 x12 x13 (Curry.Module.Array.C_EntryOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_comb_case_2(x1)(x2)(x3)(x6)(x7)(x11)(x12)(x13)(x)(st))(i)(xs)(st)
c_comb_case_2 x1 x2 x3 x6 x7 x11 x12 x13 x st = Curry.RunTimeSystem.patternFail("Array.comb_case_2")(x)



c_comb_case_3 x1 x2 x3 x6 x7 x5@Curry.Module.Array.C_Empty st = Curry.Module.Array.C_Empty
c_comb_case_3 x1 x2 x3 x6 x7 x5@(Curry.Module.Array.C_Entry x8 x9 x10) st = Curry.Module.Array.C_Entry(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(Curry.Module.Prelude.c_apply(x2)(Curry.Module.Prelude.op_45(Curry.Module.Prelude.op_43(x6)(x7)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st))(x8)(st))(Curry.Module.Array.c_comb(x1)(x2)(x3)(Curry.Module.Array.C_Empty)(x9)(Curry.Module.Prelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x6)(st))(x7)(st))(Curry.Module.Array.c_comb(x1)(x2)(x3)(Curry.Module.Array.C_Empty)(x10)(Curry.Module.Prelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x6)(st))(Curry.Module.Prelude.op_43(x7)(x6)(st))(st))
c_comb_case_3 x1 x2 x3 x6 x7 (Curry.Module.Array.C_EntryOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_comb_case_3(x1)(x2)(x3)(x6)(x7)(x)(st))(i)(xs)(st)
c_comb_case_3 x1 x2 x3 x6 x7 x st = Curry.RunTimeSystem.patternFail("Array.comb_case_3")(x)



c_combine_case_4 x1 x4 x5 x3@(Curry.Module.Array.C_Array x6 x7) st = Curry.Module.Array.C_Array(Curry.Module.Prelude.pf(Curry.Module.Array.c_combine'46_'35lambda3(x4)(x6)(x1)))(Curry.Module.Array.c_comb(x1)(x4)(x6)(x5)(x7)(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))
c_combine_case_4 x1 x4 x5 (Curry.Module.Array.C_ArrayOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_combine_case_4(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_combine_case_4 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("Array.combine_case_4")(x)



c_split_case_5 x2 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.Prelude.List)
c_split_case_5 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = let {x6 = Curry.Module.Array.c_split(x5)(st)} in Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x2)(Curry.Module.Array.c_split'46_'35selFP3'35xs(x6)(st)))((Curry.Module.Prelude.:<)(x4)(Curry.Module.Array.c_split'46_'35selFP4'35ys(x6)(st)))
c_split_case_5 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_split_case_5(x2)(x)(st))(i)(xs)(st)
c_split_case_5 x2 x st = Curry.RunTimeSystem.patternFail("Array.split_case_5")(x)



c_from_case_8 x1 x3 x4 x5 x6 x7@Curry.Module.Prelude.C_True st = x4
c_from_case_8 x1 x3 x4 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.Array.c_from_case_7(x1)(x3)(x5)(x6)(Curry.Module.Integer.c_odd(x3)(st))(st)
c_from_case_8 x1 x3 x4 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_from_case_8(x1)(x3)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_from_case_8 x1 x3 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("Array.from_case_8")(x)



c_from_case_7 x1 x3 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Array.c_from(x1)(x5)(Curry.Module.Prelude.c_div(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(st)
c_from_case_7 x1 x3 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.Array.c_from_case_6(x1)(x3)(x6)(Curry.Module.Prelude.c_otherwise(st))(st)
c_from_case_7 x1 x3 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_from_case_7(x1)(x3)(x5)(x6)(x)(st))(i)(xs)(st)
c_from_case_7 x1 x3 x5 x6 x st = Curry.RunTimeSystem.patternFail("Array.from_case_7")(x)



c_from_case_6 x1 x3 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Array.c_from(x1)(x6)(Curry.Module.Prelude.op_45(Curry.Module.Prelude.c_div(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st)
c_from_case_6 x1 x3 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_from_case_6(x1)(x3)(x6)(x)(st))(i)(xs)(st)
c_from_case_6 x1 x3 x6 x st = Curry.RunTimeSystem.patternFail("Array.from_case_6")(x)



c_at_case_11 x1 x3 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.Array.C_Entry(Curry.Module.Prelude.c_apply(x4)(x5)(st))(x6)(x7)
c_at_case_11 x1 x3 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.Array.c_at_case_10(x1)(x3)(x4)(x5)(x6)(x7)(Curry.Module.Integer.c_odd(x3)(st))(st)
c_at_case_11 x1 x3 x4 x5 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_at_case_11(x1)(x3)(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c_at_case_11 x1 x3 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("Array.at_case_11")(x)



c_at_case_10 x1 x3 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.Array.C_Entry(x5)(Curry.Module.Array.c_at(x1)(x6)(Curry.Module.Prelude.c_div(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(x4)(st))(x7)
c_at_case_10 x1 x3 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.Array.c_at_case_9(x1)(x3)(x4)(x5)(x6)(x7)(Curry.Module.Prelude.c_otherwise(st))(st)
c_at_case_10 x1 x3 x4 x5 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_at_case_10(x1)(x3)(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c_at_case_10 x1 x3 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("Array.at_case_10")(x)



c_at_case_9 x1 x3 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.Array.C_Entry(x5)(x6)(Curry.Module.Array.c_at(x1)(x7)(Curry.Module.Prelude.op_45(Curry.Module.Prelude.c_div(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x4)(st))
c_at_case_9 x1 x3 x4 x5 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_at_case_9(x1)(x3)(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c_at_case_9 x1 x3 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("Array.at_case_9")(x)



c_at_case_14 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Array.C_Entry(Curry.Module.Prelude.c_apply(x4)(x1)(st))(Curry.Module.Array.C_Empty)(Curry.Module.Array.C_Empty)
c_at_case_14 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Array.c_at_case_13(x1)(x3)(x4)(Curry.Module.Integer.c_odd(x3)(st))(st)
c_at_case_14 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_at_case_14(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_at_case_14 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("Array.at_case_14")(x)



c_at_case_13 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Array.C_Entry(x1)(Curry.Module.Array.c_at(x1)(Curry.Module.Array.C_Empty)(Curry.Module.Prelude.c_div(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(x4)(st))(Curry.Module.Array.C_Empty)
c_at_case_13 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Array.c_at_case_12(x1)(x3)(x4)(Curry.Module.Prelude.c_otherwise(st))(st)
c_at_case_13 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_at_case_13(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_at_case_13 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("Array.at_case_13")(x)



c_at_case_12 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Array.C_Entry(x1)(Curry.Module.Array.C_Empty)(Curry.Module.Array.c_at(x1)(Curry.Module.Array.C_Empty)(Curry.Module.Prelude.op_45(Curry.Module.Prelude.c_div(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x4)(st))
c_at_case_12 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Array.c_at_case_12(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_at_case_12 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("Array.at_case_12")(x)


