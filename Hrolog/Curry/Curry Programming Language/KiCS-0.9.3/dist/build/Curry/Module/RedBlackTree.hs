{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.RedBlackTree (module Curry.Module.RedBlackTree) where

import Curry.RunTimeSystem
import Curry.Module.Prelude



-- begin included



-- end included

data C_RedBlackTree t0 = C_RedBlackTree (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) (Curry.Module.RedBlackTree.C_Tree t0)
  | C_RedBlackTreeFail Curry.RunTimeSystem.C_Exceptions
  | C_RedBlackTreeOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.RedBlackTree.C_RedBlackTree t0))

data C_Color = C_Red
  | C_Black
  | C_DoublyBlack
  | C_ColorFail Curry.RunTimeSystem.C_Exceptions
  | C_ColorOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.RedBlackTree.C_Color)

data C_Tree t0 = C_Tree Curry.Module.RedBlackTree.C_Color t0 (Curry.Module.RedBlackTree.C_Tree t0) (Curry.Module.RedBlackTree.C_Tree t0)
  | C_Empty
  | C_TreeFail Curry.RunTimeSystem.C_Exceptions
  | C_TreeOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.RedBlackTree.C_Tree t0))

instance (BaseCurry t0) => BaseCurry (Curry.Module.RedBlackTree.C_RedBlackTree t0) where
  nf f (Curry.Module.RedBlackTree.C_RedBlackTree x1 x2 x3 x4) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> f(Curry.Module.RedBlackTree.C_RedBlackTree(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.RedBlackTree.C_RedBlackTree x1 x2 x3 x4) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> f(Curry.Module.RedBlackTree.C_RedBlackTree(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.RedBlackTree.C_RedBlackTreeOr(Curry.RunTimeSystem.mkRef(r)(4)(i))([Curry.Module.RedBlackTree.C_RedBlackTree(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(4)

  failed  = Curry.Module.RedBlackTree.C_RedBlackTreeFail

  branching  = Curry.Module.RedBlackTree.C_RedBlackTreeOr

  consKind (Curry.Module.RedBlackTree.C_RedBlackTreeOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.RedBlackTree.C_RedBlackTreeFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.RedBlackTree.C_RedBlackTreeFail x) = x

  orRef (Curry.Module.RedBlackTree.C_RedBlackTreeOr x _) = x

  branches (Curry.Module.RedBlackTree.C_RedBlackTreeOr _ x) = x





instance BaseCurry Curry.Module.RedBlackTree.C_Color where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.RedBlackTree.C_ColorOr(Curry.RunTimeSystem.mkRef(r)(0)(i))([Curry.Module.RedBlackTree.C_Red,Curry.Module.RedBlackTree.C_Black,Curry.Module.RedBlackTree.C_DoublyBlack]))(0)

  failed  = Curry.Module.RedBlackTree.C_ColorFail

  branching  = Curry.Module.RedBlackTree.C_ColorOr

  consKind (Curry.Module.RedBlackTree.C_ColorOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.RedBlackTree.C_ColorFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.RedBlackTree.C_ColorFail x) = x

  orRef (Curry.Module.RedBlackTree.C_ColorOr x _) = x

  branches (Curry.Module.RedBlackTree.C_ColorOr _ x) = x





instance (BaseCurry t0) => BaseCurry (Curry.Module.RedBlackTree.C_Tree t0) where
  nf f (Curry.Module.RedBlackTree.C_Tree x1 x2 x3 x4) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> f(Curry.Module.RedBlackTree.C_Tree(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.RedBlackTree.C_Tree x1 x2 x3 x4) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> f(Curry.Module.RedBlackTree.C_Tree(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.RedBlackTree.C_TreeOr(Curry.RunTimeSystem.mkRef(r)(4)(i))([Curry.Module.RedBlackTree.C_Tree(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.RedBlackTree.C_Empty]))(4)

  failed  = Curry.Module.RedBlackTree.C_TreeFail

  branching  = Curry.Module.RedBlackTree.C_TreeOr

  consKind (Curry.Module.RedBlackTree.C_TreeOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.RedBlackTree.C_TreeFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.RedBlackTree.C_TreeFail x) = x

  orRef (Curry.Module.RedBlackTree.C_TreeOr x _) = x

  branches (Curry.Module.RedBlackTree.C_TreeOr _ x) = x





instance (Curry t0) => Curry (Curry.Module.RedBlackTree.C_RedBlackTree t0) where
  strEq (Curry.Module.RedBlackTree.C_RedBlackTree x1 x2 x3 x4) (Curry.Module.RedBlackTree.C_RedBlackTree y1 y2 y3 y4) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.RedBlackTree.C_RedBlackTree x1 x2 x3 x4) (Curry.Module.RedBlackTree.C_RedBlackTree y1 y2 y3 y4) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.genEq(x4)(y4)(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.RedBlackTree.C_RedBlackTree x1 x2 x3 x4) st = Curry.Module.RedBlackTree.C_RedBlackTree(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))

  foldCurry f c (Curry.Module.RedBlackTree.C_RedBlackTree x1 x2 x3 x4) st = f(x1)(f(x2)(f(x3)(f(x4)(c)(st))(st))(st))(st)

  typeName _ = "RedBlackTree"

  showQ d (Curry.Module.RedBlackTree.C_RedBlackTree x1 x2 x3 x4) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("RedBlackTree.RedBlackTree "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))))))))


  showQ _ (Curry.Module.RedBlackTree.C_RedBlackTreeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.RedBlackTree.C_Color where
  strEq Curry.Module.RedBlackTree.C_Red Curry.Module.RedBlackTree.C_Red st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.RedBlackTree.C_Black Curry.Module.RedBlackTree.C_Black st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.RedBlackTree.C_DoublyBlack Curry.Module.RedBlackTree.C_DoublyBlack st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.RedBlackTree.C_Red Curry.Module.RedBlackTree.C_Red st = Curry.Module.Prelude.C_True
  eq Curry.Module.RedBlackTree.C_Black Curry.Module.RedBlackTree.C_Black st = Curry.Module.Prelude.C_True
  eq Curry.Module.RedBlackTree.C_DoublyBlack Curry.Module.RedBlackTree.C_DoublyBlack st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.RedBlackTree.C_Red st = Curry.Module.RedBlackTree.C_Red
  propagate f Curry.Module.RedBlackTree.C_Black st = Curry.Module.RedBlackTree.C_Black
  propagate f Curry.Module.RedBlackTree.C_DoublyBlack st = Curry.Module.RedBlackTree.C_DoublyBlack

  foldCurry f c Curry.Module.RedBlackTree.C_Red st = c
  foldCurry f c Curry.Module.RedBlackTree.C_Black st = c
  foldCurry f c Curry.Module.RedBlackTree.C_DoublyBlack st = c

  typeName _ = "Color"

  showQ _ Curry.Module.RedBlackTree.C_Red = Prelude.showString("RedBlackTree.Red")
  showQ _ Curry.Module.RedBlackTree.C_Black = Prelude.showString("RedBlackTree.Black")
  showQ _ Curry.Module.RedBlackTree.C_DoublyBlack = Prelude.showString("RedBlackTree.DoublyBlack")
  showQ _ (Curry.Module.RedBlackTree.C_ColorOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0) => Curry (Curry.Module.RedBlackTree.C_Tree t0) where
  strEq (Curry.Module.RedBlackTree.C_Tree x1 x2 x3 x4) (Curry.Module.RedBlackTree.C_Tree y1 y2 y3 y4) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(st))(st))(st)
  strEq Curry.Module.RedBlackTree.C_Empty Curry.Module.RedBlackTree.C_Empty st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.RedBlackTree.C_Tree x1 x2 x3 x4) (Curry.Module.RedBlackTree.C_Tree y1 y2 y3 y4) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.genEq(x4)(y4)(st))(st))(st))(st)
  eq Curry.Module.RedBlackTree.C_Empty Curry.Module.RedBlackTree.C_Empty st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.RedBlackTree.C_Tree x1 x2 x3 x4) st = Curry.Module.RedBlackTree.C_Tree(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))
  propagate f Curry.Module.RedBlackTree.C_Empty st = Curry.Module.RedBlackTree.C_Empty

  foldCurry f c (Curry.Module.RedBlackTree.C_Tree x1 x2 x3 x4) st = f(x1)(f(x2)(f(x3)(f(x4)(c)(st))(st))(st))(st)
  foldCurry f c Curry.Module.RedBlackTree.C_Empty st = c

  typeName _ = "Tree"

  showQ d (Curry.Module.RedBlackTree.C_Tree x1 x2 x3 x4) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("RedBlackTree.Tree "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))))))))


  showQ _ Curry.Module.RedBlackTree.C_Empty = Prelude.showString("RedBlackTree.Empty")
  showQ _ (Curry.Module.RedBlackTree.C_TreeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.RedBlackTree.C_RedBlackTree t0) where
  showsPrec d (Curry.Module.RedBlackTree.C_RedBlackTree x1 x2 x3 x4) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("RedBlackTree "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))))))))


  showsPrec _ (Curry.Module.RedBlackTree.C_RedBlackTreeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.RedBlackTree.C_Color where
  showsPrec _ Curry.Module.RedBlackTree.C_Red = Prelude.showString("Red")
  showsPrec _ Curry.Module.RedBlackTree.C_Black = Prelude.showString("Black")
  showsPrec _ Curry.Module.RedBlackTree.C_DoublyBlack = Prelude.showString("DoublyBlack")
  showsPrec _ (Curry.Module.RedBlackTree.C_ColorOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.RedBlackTree.C_Tree t0) where
  showsPrec d (Curry.Module.RedBlackTree.C_Tree x1 x2 x3 x4) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Tree "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))))))))


  showsPrec _ Curry.Module.RedBlackTree.C_Empty = Prelude.showString("Empty")
  showsPrec _ (Curry.Module.RedBlackTree.C_TreeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Read t0) => Read (Curry.Module.RedBlackTree.C_RedBlackTree t0) where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.RedBlackTree.C_RedBlackTree(x1)(x2)(x3)(x4))(r4) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("RedBlackTree")("RedBlackTree")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3)])(r)





instance Read Curry.Module.RedBlackTree.C_Color where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.RedBlackTree.C_Red)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("RedBlackTree")("Red")(r)])(r))((Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.RedBlackTree.C_Black)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("RedBlackTree")("Black")(r)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.RedBlackTree.C_DoublyBlack)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("RedBlackTree")("DoublyBlack")(r)])(r)))





instance (Read t0) => Read (Curry.Module.RedBlackTree.C_Tree t0) where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.RedBlackTree.C_Tree(x1)(x2)(x3)(x4))(r4) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("RedBlackTree")("Tree")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.RedBlackTree.C_Empty)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("RedBlackTree")("Empty")(r)])(r))





c_empty :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_RedBlackTree t0
c_empty x1 x2 x3 st = Curry.Module.RedBlackTree.C_RedBlackTree(x1)(x2)(x3)(Curry.Module.RedBlackTree.C_Empty)



c_isEmpty :: (Curry t0) => (Curry.Module.RedBlackTree.C_RedBlackTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isEmpty x1@(Curry.Module.RedBlackTree.C_RedBlackTree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_isEmpty_case_177(x5)(st)
c_isEmpty (Curry.Module.RedBlackTree.C_RedBlackTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_isEmpty(x)(st))(i)(xs)(st)
c_isEmpty x st = Curry.RunTimeSystem.patternFail("RedBlackTree.isEmpty")(x)



c_newTreeLike :: (Curry t0) => (Curry.Module.RedBlackTree.C_RedBlackTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_RedBlackTree t0
c_newTreeLike x1@(Curry.Module.RedBlackTree.C_RedBlackTree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.C_RedBlackTree(x2)(x3)(x4)(Curry.Module.RedBlackTree.C_Empty)
c_newTreeLike (Curry.Module.RedBlackTree.C_RedBlackTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_newTreeLike(x)(st))(i)(xs)(st)
c_newTreeLike x st = Curry.RunTimeSystem.patternFail("RedBlackTree.newTreeLike")(x)



c_lookup :: (Curry t0) => t0 -> (Curry.Module.RedBlackTree.C_RedBlackTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t0
c_lookup x1 x2@(Curry.Module.RedBlackTree.C_RedBlackTree x3 x4 x5 x6) st = Curry.Module.RedBlackTree.c_lookupTree(x4)(x5)(x1)(x6)(st)
c_lookup x1 (Curry.Module.RedBlackTree.C_RedBlackTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_lookup(x1)(x)(st))(i)(xs)(st)
c_lookup x1 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.lookup")(x)



c_lookupTree :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> t0 -> (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t0
c_lookupTree x1 x2 x3 x4@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.Prelude.C_Nothing
c_lookupTree x1 x2 x3 x4@(Curry.Module.RedBlackTree.C_Tree x5 x6 x7 x8) st = Curry.Module.RedBlackTree.c_lookupTree_case_176(x1)(x2)(x3)(x6)(x7)(x8)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x3)(st))(x6)(st))(st)
c_lookupTree x1 x2 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_lookupTree(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_lookupTree x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.lookupTree")(x)



c_update :: (Curry t0) => t0 -> (Curry.Module.RedBlackTree.C_RedBlackTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_RedBlackTree t0
c_update x1 x2@(Curry.Module.RedBlackTree.C_RedBlackTree x3 x4 x5 x6) st = Curry.Module.RedBlackTree.C_RedBlackTree(x3)(x4)(x5)(Curry.Module.RedBlackTree.c_updateTree(x3)(x5)(x1)(x6)(st))
c_update x1 (Curry.Module.RedBlackTree.C_RedBlackTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_update(x1)(x)(st))(i)(xs)(st)
c_update x1 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.update")(x)



c_updateTree :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> t0 -> (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_updateTree x1 x2 x3 x4 st = let {x5 = Curry.Module.RedBlackTree.c_updateTree'46upd'4635(x3)(x1)(x2)(x4)(st)} in Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.RedBlackTree.c_updateTree'46_'35selFP3'35e2(x5)(st))(Curry.Module.RedBlackTree.c_updateTree'46_'35selFP4'35l(x5)(st))(Curry.Module.RedBlackTree.c_updateTree'46_'35selFP5'35r(x5)(st))



c_updateTree'46upd'4635 :: (Curry t183) => t183 -> (Curry.Module.Prelude.Prim (t183 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t183 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.Prim (t183 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t183 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.RedBlackTree.C_Tree t183) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t183
c_updateTree'46upd'4635 x1 x2 x3 x4@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Red)(x1)(Curry.Module.RedBlackTree.C_Empty)(Curry.Module.RedBlackTree.C_Empty)
c_updateTree'46upd'4635 x1 x2 x3 x4@(Curry.Module.RedBlackTree.C_Tree x5 x6 x7 x8) st = Curry.Module.RedBlackTree.c_updateTree'46upd'4635_case_173(x1)(x2)(x3)(x5)(x6)(x7)(x8)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x2)(x1)(st))(x6)(st))(st)
c_updateTree'46upd'4635 x1 x2 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_updateTree'46upd'4635(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_updateTree'46upd'4635 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.updateTree.upd.35")(x)



c_updateTree'46_'35selFP3'35e2 :: (Curry t183) => (Curry.Module.RedBlackTree.C_Tree t183) -> Curry.RunTimeSystem.State -> t183
c_updateTree'46_'35selFP3'35e2 x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = x3
c_updateTree'46_'35selFP3'35e2 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_updateTree'46_'35selFP3'35e2(x)(st))(i)(xs)(st)
c_updateTree'46_'35selFP3'35e2 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.updateTree._#selFP3#e2")(x)



c_updateTree'46_'35selFP4'35l :: (Curry t183) => (Curry.Module.RedBlackTree.C_Tree t183) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t183
c_updateTree'46_'35selFP4'35l x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = x4
c_updateTree'46_'35selFP4'35l (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_updateTree'46_'35selFP4'35l(x)(st))(i)(xs)(st)
c_updateTree'46_'35selFP4'35l x st = Curry.RunTimeSystem.patternFail("RedBlackTree.updateTree._#selFP4#l")(x)



c_updateTree'46_'35selFP5'35r :: (Curry t183) => (Curry.Module.RedBlackTree.C_Tree t183) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t183
c_updateTree'46_'35selFP5'35r x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = x5
c_updateTree'46_'35selFP5'35r (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_updateTree'46_'35selFP5'35r(x)(st))(i)(xs)(st)
c_updateTree'46_'35selFP5'35r x st = Curry.RunTimeSystem.patternFail("RedBlackTree.updateTree._#selFP5#r")(x)



c_delete :: (Curry t0) => t0 -> (Curry.Module.RedBlackTree.C_RedBlackTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_RedBlackTree t0
c_delete x1 x2@(Curry.Module.RedBlackTree.C_RedBlackTree x3 x4 x5 x6) st = Curry.Module.RedBlackTree.C_RedBlackTree(x3)(x4)(x5)(Curry.Module.RedBlackTree.c_delete'46blackenRoot'4644(Curry.Module.RedBlackTree.c_deleteTree(x4)(x5)(x1)(x6)(st))(st))
c_delete x1 (Curry.Module.RedBlackTree.C_RedBlackTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_delete(x1)(x)(st))(i)(xs)(st)
c_delete x1 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.delete")(x)



c_delete'46blackenRoot'4644 :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_delete'46blackenRoot'4644 x1@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.RedBlackTree.C_Empty
c_delete'46blackenRoot'4644 x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(x3)(x4)(x5)
c_delete'46blackenRoot'4644 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_delete'46blackenRoot'4644(x)(st))(i)(xs)(st)
c_delete'46blackenRoot'4644 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.delete.blackenRoot.44")(x)



c_deleteTree :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> t0 -> (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_deleteTree x1 x2 x3 x4@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.RedBlackTree.C_Empty
c_deleteTree x1 x2 x3 x4@(Curry.Module.RedBlackTree.C_Tree x5 x6 x7 x8) st = Curry.Module.RedBlackTree.c_deleteTree_case_170(x1)(x2)(x3)(x5)(x6)(x7)(x8)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x3)(st))(x6)(st))(st)
c_deleteTree x1 x2 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_deleteTree(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_deleteTree x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.deleteTree")(x)



c_deleteTree'46addColor'4656 :: (Curry t0) => Curry.Module.RedBlackTree.C_Color -> (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_deleteTree'46addColor'4656 x1@Curry.Module.RedBlackTree.C_Red x2 st = x2
c_deleteTree'46addColor'4656 x1@Curry.Module.RedBlackTree.C_Black x2 st = Curry.Module.RedBlackTree.c_deleteTree'46addColor'4656_case_165(x2)(st)
c_deleteTree'46addColor'4656 (Curry.Module.RedBlackTree.C_ColorOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_deleteTree'46addColor'4656(x)(x2)(st))(i)(xs)(st)
c_deleteTree'46addColor'4656 x x2 st = Curry.RunTimeSystem.patternFail("RedBlackTree.deleteTree.addColor.56")(x)



c_deleteTree'46rightMost'4656 :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> t0
c_deleteTree'46rightMost'4656 x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_deleteTree'46rightMost'4656_case_163(x3)(x5)(Curry.Module.Prelude.op_61_61(x5)(Curry.Module.RedBlackTree.C_Empty)(st))(st)
c_deleteTree'46rightMost'4656 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_deleteTree'46rightMost'4656(x)(st))(i)(xs)(st)
c_deleteTree'46rightMost'4656 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.deleteTree.rightMost.56")(x)



c_tree2list :: (Curry t0) => (Curry.Module.RedBlackTree.C_RedBlackTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_tree2list x1@(Curry.Module.RedBlackTree.C_RedBlackTree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_tree2listTree(x5)(st)
c_tree2list (Curry.Module.RedBlackTree.C_RedBlackTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_tree2list(x)(st))(i)(xs)(st)
c_tree2list x st = Curry.RunTimeSystem.patternFail("RedBlackTree.tree2list")(x)



c_tree2listTree :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_tree2listTree x1 st = Curry.Module.RedBlackTree.c_tree2listTree'46t2l'4677(x1)(Curry.Module.Prelude.List)(st)



c_tree2listTree'46t2l'4677 :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_tree2listTree'46t2l'4677 x1@Curry.Module.RedBlackTree.C_Empty x2 st = x2
c_tree2listTree'46t2l'4677 x1@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) x2 st = Curry.Module.RedBlackTree.c_tree2listTree'46t2l'4677(x5)((Curry.Module.Prelude.:<)(x4)(Curry.Module.RedBlackTree.c_tree2listTree'46t2l'4677(x6)(x2)(st)))(st)
c_tree2listTree'46t2l'4677 (Curry.Module.RedBlackTree.C_TreeOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_tree2listTree'46t2l'4677(x)(x2)(st))(i)(xs)(st)
c_tree2listTree'46t2l'4677 x x2 st = Curry.RunTimeSystem.patternFail("RedBlackTree.tree2listTree.t2l.77")(x)



c_sort :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_sort x1 x2 st = Curry.Module.RedBlackTree.c_tree2list(Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.RedBlackTree.c_update))(Curry.Module.RedBlackTree.c_empty(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.RedBlackTree.c_sort'46_'35lambda2))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_61_61))(x1)(st))(x2)(st))(st)



c_sort'46_'35lambda2 :: (Curry t520) => t520 -> t520 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_sort'46_'35lambda2 x1 x2 st = Curry.Module.Prelude.C_False



c_setInsertEquivalence :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.RedBlackTree.C_RedBlackTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_RedBlackTree t0
c_setInsertEquivalence x1 x2@(Curry.Module.RedBlackTree.C_RedBlackTree x3 x4 x5 x6) st = Curry.Module.RedBlackTree.C_RedBlackTree(x1)(x4)(x5)(x6)
c_setInsertEquivalence x1 (Curry.Module.RedBlackTree.C_RedBlackTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_setInsertEquivalence(x1)(x)(st))(i)(xs)(st)
c_setInsertEquivalence x1 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.setInsertEquivalence")(x)



c_rbt :: (Curry t0) => (Curry.Module.RedBlackTree.C_RedBlackTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_rbt x1@(Curry.Module.RedBlackTree.C_RedBlackTree x2 x3 x4 x5) st = x5
c_rbt (Curry.Module.RedBlackTree.C_RedBlackTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_rbt(x)(st))(i)(xs)(st)
c_rbt x st = Curry.RunTimeSystem.patternFail("RedBlackTree.rbt")(x)



c_isBlack :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isBlack x1@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.Prelude.C_True
c_isBlack x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.Prelude.op_61_61(x2)(Curry.Module.RedBlackTree.C_Black)(st)
c_isBlack (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_isBlack(x)(st))(i)(xs)(st)
c_isBlack x st = Curry.RunTimeSystem.patternFail("RedBlackTree.isBlack")(x)



c_isRed :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isRed x1@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.Prelude.C_False
c_isRed x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.Prelude.op_61_61(x2)(Curry.Module.RedBlackTree.C_Red)(st)
c_isRed (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_isRed(x)(st))(i)(xs)(st)
c_isRed x st = Curry.RunTimeSystem.patternFail("RedBlackTree.isRed")(x)



c_isDoublyBlack :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isDoublyBlack x1@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.Prelude.C_True
c_isDoublyBlack x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.Prelude.op_61_61(x2)(Curry.Module.RedBlackTree.C_DoublyBlack)(st)
c_isDoublyBlack (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_isDoublyBlack(x)(st))(i)(xs)(st)
c_isDoublyBlack x st = Curry.RunTimeSystem.patternFail("RedBlackTree.isDoublyBlack")(x)



c_element :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> t0
c_element x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = x3
c_element (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_element(x)(st))(i)(xs)(st)
c_element x st = Curry.RunTimeSystem.patternFail("RedBlackTree.element")(x)



c_left :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_left x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = x4
c_left (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_left(x)(st))(i)(xs)(st)
c_left x st = Curry.RunTimeSystem.patternFail("RedBlackTree.left")(x)



c_right :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_right x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = x5
c_right (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_right(x)(st))(i)(xs)(st)
c_right x st = Curry.RunTimeSystem.patternFail("RedBlackTree.right")(x)



c_singleBlack :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_singleBlack x1@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.RedBlackTree.C_Empty
c_singleBlack x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_singleBlack_case_162(x3)(x4)(x5)(x2)(st)
c_singleBlack (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_singleBlack(x)(st))(i)(xs)(st)
c_singleBlack x st = Curry.RunTimeSystem.patternFail("RedBlackTree.singleBlack")(x)



c_balanceL :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_balanceL x1 st = let {x2 = Curry.Module.RedBlackTree.c_left(x1)(st)} in Curry.Module.RedBlackTree.c_balanceL_case_161(x1)(x2)(Curry.Module.Prelude.op_38_38(Curry.Module.RedBlackTree.c_isRed(x2)(st))(Curry.Module.RedBlackTree.c_isRed(Curry.Module.RedBlackTree.c_left(x2)(st))(st))(st))(st)



c_balanceL'46_'35selFP7'35z :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.RunTimeSystem.State -> t88
c_balanceL'46_'35selFP7'35z x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP7'35z_case_158(x3)(x4)(st)
c_balanceL'46_'35selFP7'35z (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP7'35z(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP7'35z x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP7#z")(x)



c_balanceL'46_'35selFP8'35y :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.RunTimeSystem.State -> t88
c_balanceL'46_'35selFP8'35y x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP8'35y_case_156(x4)(st)
c_balanceL'46_'35selFP8'35y (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP8'35y(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP8'35y x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP8#y")(x)



c_balanceL'46_'35selFP9'35x :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.RunTimeSystem.State -> t88
c_balanceL'46_'35selFP9'35x x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP9'35x_case_154(x4)(st)
c_balanceL'46_'35selFP9'35x (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP9'35x(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP9'35x x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP9#x")(x)



c_balanceL'46_'35selFP10'35a :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t88
c_balanceL'46_'35selFP10'35a x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP10'35a_case_152(x4)(st)
c_balanceL'46_'35selFP10'35a (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP10'35a(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP10'35a x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP10#a")(x)



c_balanceL'46_'35selFP11'35b :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t88
c_balanceL'46_'35selFP11'35b x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP11'35b_case_150(x4)(st)
c_balanceL'46_'35selFP11'35b (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP11'35b(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP11'35b x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP11#b")(x)



c_balanceL'46_'35selFP12'35c :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t88
c_balanceL'46_'35selFP12'35c x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP12'35c_case_148(x4)(st)
c_balanceL'46_'35selFP12'35c (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP12'35c(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP12'35c x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP12#c")(x)



c_balanceL'46_'35selFP13'35d :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t88
c_balanceL'46_'35selFP13'35d x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP13'35d_case_146(x5)(x4)(st)
c_balanceL'46_'35selFP13'35d (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP13'35d(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP13'35d x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP13#d")(x)



c_balanceL'46_'35selFP15'35z :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.RunTimeSystem.State -> t88
c_balanceL'46_'35selFP15'35z x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP15'35z_case_144(x3)(x4)(st)
c_balanceL'46_'35selFP15'35z (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP15'35z(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP15'35z x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP15#z")(x)



c_balanceL'46_'35selFP16'35x :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.RunTimeSystem.State -> t88
c_balanceL'46_'35selFP16'35x x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP16'35x_case_142(x4)(st)
c_balanceL'46_'35selFP16'35x (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP16'35x(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP16'35x x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP16#x")(x)



c_balanceL'46_'35selFP17'35a :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t88
c_balanceL'46_'35selFP17'35a x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP17'35a_case_140(x4)(st)
c_balanceL'46_'35selFP17'35a (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP17'35a(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP17'35a x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP17#a")(x)



c_balanceL'46_'35selFP18'35y :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.RunTimeSystem.State -> t88
c_balanceL'46_'35selFP18'35y x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP18'35y_case_138(x4)(st)
c_balanceL'46_'35selFP18'35y (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP18'35y(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP18'35y x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP18#y")(x)



c_balanceL'46_'35selFP19'35b :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t88
c_balanceL'46_'35selFP19'35b x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP19'35b_case_136(x4)(st)
c_balanceL'46_'35selFP19'35b (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP19'35b(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP19'35b x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP19#b")(x)



c_balanceL'46_'35selFP20'35c :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t88
c_balanceL'46_'35selFP20'35c x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP20'35c_case_134(x4)(st)
c_balanceL'46_'35selFP20'35c (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP20'35c(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP20'35c x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP20#c")(x)



c_balanceL'46_'35selFP21'35d :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t88
c_balanceL'46_'35selFP21'35d x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP21'35d_case_132(x5)(x4)(st)
c_balanceL'46_'35selFP21'35d (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP21'35d(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP21'35d x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP21#d")(x)



c_balanceR :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_balanceR x1 st = let {x2 = Curry.Module.RedBlackTree.c_right(x1)(st)} in Curry.Module.RedBlackTree.c_balanceR_case_130(x1)(x2)(Curry.Module.Prelude.op_38_38(Curry.Module.RedBlackTree.c_isRed(x2)(st))(Curry.Module.RedBlackTree.c_isRed(Curry.Module.RedBlackTree.c_right(x2)(st))(st))(st))(st)



c_balanceR'46_'35selFP23'35x :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.RunTimeSystem.State -> t131
c_balanceR'46_'35selFP23'35x x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP23'35x_case_127(x3)(x5)(st)
c_balanceR'46_'35selFP23'35x (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP23'35x(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP23'35x x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP23#x")(x)



c_balanceR'46_'35selFP24'35a :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t131
c_balanceR'46_'35selFP24'35a x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP24'35a_case_125(x4)(x5)(st)
c_balanceR'46_'35selFP24'35a (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP24'35a(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP24'35a x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP24#a")(x)



c_balanceR'46_'35selFP25'35y :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.RunTimeSystem.State -> t131
c_balanceR'46_'35selFP25'35y x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP25'35y_case_123(x5)(st)
c_balanceR'46_'35selFP25'35y (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP25'35y(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP25'35y x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP25#y")(x)



c_balanceR'46_'35selFP26'35b :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t131
c_balanceR'46_'35selFP26'35b x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP26'35b_case_121(x5)(st)
c_balanceR'46_'35selFP26'35b (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP26'35b(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP26'35b x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP26#b")(x)



c_balanceR'46_'35selFP27'35z :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.RunTimeSystem.State -> t131
c_balanceR'46_'35selFP27'35z x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP27'35z_case_119(x5)(st)
c_balanceR'46_'35selFP27'35z (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP27'35z(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP27'35z x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP27#z")(x)



c_balanceR'46_'35selFP28'35c :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t131
c_balanceR'46_'35selFP28'35c x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP28'35c_case_117(x5)(st)
c_balanceR'46_'35selFP28'35c (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP28'35c(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP28'35c x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP28#c")(x)



c_balanceR'46_'35selFP29'35d :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t131
c_balanceR'46_'35selFP29'35d x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP29'35d_case_115(x5)(st)
c_balanceR'46_'35selFP29'35d (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP29'35d(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP29'35d x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP29#d")(x)



c_balanceR'46_'35selFP31'35x :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.RunTimeSystem.State -> t131
c_balanceR'46_'35selFP31'35x x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP31'35x_case_113(x3)(x5)(st)
c_balanceR'46_'35selFP31'35x (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP31'35x(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP31'35x x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP31#x")(x)



c_balanceR'46_'35selFP32'35a :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t131
c_balanceR'46_'35selFP32'35a x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP32'35a_case_111(x4)(x5)(st)
c_balanceR'46_'35selFP32'35a (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP32'35a(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP32'35a x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP32#a")(x)



c_balanceR'46_'35selFP33'35z :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.RunTimeSystem.State -> t131
c_balanceR'46_'35selFP33'35z x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP33'35z_case_109(x5)(st)
c_balanceR'46_'35selFP33'35z (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP33'35z(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP33'35z x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP33#z")(x)



c_balanceR'46_'35selFP34'35y :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.RunTimeSystem.State -> t131
c_balanceR'46_'35selFP34'35y x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP34'35y_case_107(x5)(st)
c_balanceR'46_'35selFP34'35y (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP34'35y(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP34'35y x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP34#y")(x)



c_balanceR'46_'35selFP35'35b :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t131
c_balanceR'46_'35selFP35'35b x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP35'35b_case_105(x5)(st)
c_balanceR'46_'35selFP35'35b (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP35'35b(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP35'35b x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP35#b")(x)



c_balanceR'46_'35selFP36'35c :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t131
c_balanceR'46_'35selFP36'35c x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP36'35c_case_103(x5)(st)
c_balanceR'46_'35selFP36'35c (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP36'35c(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP36'35c x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP36#c")(x)



c_balanceR'46_'35selFP37'35d :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t131
c_balanceR'46_'35selFP37'35d x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP37'35d_case_101(x5)(st)
c_balanceR'46_'35selFP37'35d (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP37'35d(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP37'35d x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP37#d")(x)



c_delBalanceL :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_delBalanceL x1 st = Curry.Module.RedBlackTree.c_delBalanceL_case_99(x1)(Curry.Module.RedBlackTree.c_isDoublyBlack(Curry.Module.RedBlackTree.c_left(x1)(st))(st))(st)



c_reviseLeft :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_reviseLeft x1 st = let {x2 = Curry.Module.RedBlackTree.c_right(x1)(st)} in let {x3 = Curry.Module.RedBlackTree.c_isBlack(x2)(st)} in Curry.Module.RedBlackTree.c_reviseLeft_case_98(x1)(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.RedBlackTree.C_Empty)(st))(st)



c_reviseLeft'46_'35selFP39'35col :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Color
c_reviseLeft'46_'35selFP39'35col x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP39'35col_case_92(x2)(x5)(st)
c_reviseLeft'46_'35selFP39'35col (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP39'35col(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP39'35col x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP39#col")(x)



c_reviseLeft'46_'35selFP40'35x :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP40'35x x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP40'35x_case_90(x3)(x5)(st)
c_reviseLeft'46_'35selFP40'35x (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP40'35x(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP40'35x x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP40#x")(x)



c_reviseLeft'46_'35selFP41'35a :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP41'35a x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP41'35a_case_88(x4)(x5)(st)
c_reviseLeft'46_'35selFP41'35a (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP41'35a(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP41'35a x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP41#a")(x)



c_reviseLeft'46_'35selFP42'35z :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP42'35z x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP42'35z_case_86(x5)(st)
c_reviseLeft'46_'35selFP42'35z (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP42'35z(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP42'35z x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP42#z")(x)



c_reviseLeft'46_'35selFP43'35y :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP43'35y x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP43'35y_case_84(x5)(st)
c_reviseLeft'46_'35selFP43'35y (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP43'35y(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP43'35y x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP43#y")(x)



c_reviseLeft'46_'35selFP44'35b :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP44'35b x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP44'35b_case_82(x5)(st)
c_reviseLeft'46_'35selFP44'35b (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP44'35b(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP44'35b x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP44#b")(x)



c_reviseLeft'46_'35selFP45'35c :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP45'35c x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP45'35c_case_80(x5)(st)
c_reviseLeft'46_'35selFP45'35c (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP45'35c(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP45'35c x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP45#c")(x)



c_reviseLeft'46_'35selFP46'35d :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP46'35d x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP46'35d_case_78(x5)(st)
c_reviseLeft'46_'35selFP46'35d (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP46'35d(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP46'35d x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP46#d")(x)



c_reviseLeft'46_'35selFP48'35col :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Color
c_reviseLeft'46_'35selFP48'35col x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP48'35col_case_76(x2)(x5)(st)
c_reviseLeft'46_'35selFP48'35col (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP48'35col(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP48'35col x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP48#col")(x)



c_reviseLeft'46_'35selFP49'35x :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP49'35x x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP49'35x_case_74(x3)(x5)(st)
c_reviseLeft'46_'35selFP49'35x (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP49'35x(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP49'35x x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP49#x")(x)



c_reviseLeft'46_'35selFP50'35a :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP50'35a x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP50'35a_case_72(x4)(x5)(st)
c_reviseLeft'46_'35selFP50'35a (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP50'35a(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP50'35a x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP50#a")(x)



c_reviseLeft'46_'35selFP51'35y :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP51'35y x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP51'35y_case_70(x5)(st)
c_reviseLeft'46_'35selFP51'35y (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP51'35y(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP51'35y x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP51#y")(x)



c_reviseLeft'46_'35selFP52'35b :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP52'35b x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP52'35b_case_68(x5)(st)
c_reviseLeft'46_'35selFP52'35b (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP52'35b(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP52'35b x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP52#b")(x)



c_reviseLeft'46_'35selFP53'35z :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP53'35z x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP53'35z_case_66(x5)(st)
c_reviseLeft'46_'35selFP53'35z (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP53'35z(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP53'35z x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP53#z")(x)



c_reviseLeft'46_'35selFP54'35c :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP54'35c x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP54'35c_case_64(x5)(st)
c_reviseLeft'46_'35selFP54'35c (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP54'35c(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP54'35c x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP54#c")(x)



c_reviseLeft'46_'35selFP55'35d :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP55'35d x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP55'35d_case_62(x5)(st)
c_reviseLeft'46_'35selFP55'35d (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP55'35d(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP55'35d x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP55#d")(x)



c_reviseLeft'46_'35selFP57'35col :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Color
c_reviseLeft'46_'35selFP57'35col x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP57'35col_case_60(x2)(x5)(st)
c_reviseLeft'46_'35selFP57'35col (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP57'35col(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP57'35col x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP57#col")(x)



c_reviseLeft'46_'35selFP58'35x :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP58'35x x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP58'35x_case_59(x3)(x5)(st)
c_reviseLeft'46_'35selFP58'35x (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP58'35x(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP58'35x x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP58#x")(x)



c_reviseLeft'46_'35selFP59'35a :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP59'35a x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP59'35a_case_58(x4)(x5)(st)
c_reviseLeft'46_'35selFP59'35a (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP59'35a(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP59'35a x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP59#a")(x)



c_reviseLeft'46_'35selFP60'35y :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP60'35y x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP60'35y_case_57(x5)(st)
c_reviseLeft'46_'35selFP60'35y (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP60'35y(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP60'35y x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP60#y")(x)



c_reviseLeft'46_'35selFP61'35b :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP61'35b x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP61'35b_case_56(x5)(st)
c_reviseLeft'46_'35selFP61'35b (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP61'35b(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP61'35b x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP61#b")(x)



c_reviseLeft'46_'35selFP62'35c :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP62'35c x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP62'35c_case_55(x5)(st)
c_reviseLeft'46_'35selFP62'35c (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP62'35c(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP62'35c x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP62#c")(x)



c_reviseLeft'46_'35selFP64'35x :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP64'35x x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP64'35x_case_54(x3)(x5)(st)
c_reviseLeft'46_'35selFP64'35x (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP64'35x(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP64'35x x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP64#x")(x)



c_reviseLeft'46_'35selFP65'35a :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP65'35a x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP65'35a_case_53(x4)(x5)(st)
c_reviseLeft'46_'35selFP65'35a (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP65'35a(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP65'35a x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP65#a")(x)



c_reviseLeft'46_'35selFP66'35y :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP66'35y x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP66'35y_case_52(x5)(st)
c_reviseLeft'46_'35selFP66'35y (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP66'35y(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP66'35y x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP66#y")(x)



c_reviseLeft'46_'35selFP67'35b :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP67'35b x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP67'35b_case_51(x5)(st)
c_reviseLeft'46_'35selFP67'35b (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP67'35b(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP67'35b x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP67#b")(x)



c_reviseLeft'46_'35selFP68'35c :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP68'35c x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP68'35c_case_50(x5)(st)
c_reviseLeft'46_'35selFP68'35c (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP68'35c(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP68'35c x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP68#c")(x)



c_delBalanceR :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_delBalanceR x1 st = Curry.Module.RedBlackTree.c_delBalanceR_case_49(x1)(Curry.Module.RedBlackTree.c_isDoublyBlack(Curry.Module.RedBlackTree.c_right(x1)(st))(st))(st)



c_reviseRight :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_reviseRight x1 st = let {x2 = Curry.Module.RedBlackTree.c_left(x1)(st)} in let {x3 = Curry.Module.RedBlackTree.c_isBlack(x2)(st)} in Curry.Module.RedBlackTree.c_reviseRight_case_48(x1)(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.RedBlackTree.C_Empty)(st))(st)



c_reviseRight'46_'35selFP70'35col :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Color
c_reviseRight'46_'35selFP70'35col x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP70'35col_case_42(x2)(x4)(st)
c_reviseRight'46_'35selFP70'35col (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP70'35col(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP70'35col x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP70#col")(x)



c_reviseRight'46_'35selFP71'35x :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP71'35x x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP71'35x_case_40(x3)(x4)(st)
c_reviseRight'46_'35selFP71'35x (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP71'35x(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP71'35x x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP71#x")(x)



c_reviseRight'46_'35selFP72'35y :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP72'35y x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP72'35y_case_38(x4)(st)
c_reviseRight'46_'35selFP72'35y (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP72'35y(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP72'35y x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP72#y")(x)



c_reviseRight'46_'35selFP73'35z :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP73'35z x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP73'35z_case_36(x4)(st)
c_reviseRight'46_'35selFP73'35z (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP73'35z(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP73'35z x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP73#z")(x)



c_reviseRight'46_'35selFP74'35d :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP74'35d x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP74'35d_case_34(x4)(st)
c_reviseRight'46_'35selFP74'35d (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP74'35d(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP74'35d x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP74#d")(x)



c_reviseRight'46_'35selFP75'35c :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP75'35c x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP75'35c_case_32(x4)(st)
c_reviseRight'46_'35selFP75'35c (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP75'35c(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP75'35c x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP75#c")(x)



c_reviseRight'46_'35selFP76'35b :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP76'35b x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP76'35b_case_30(x4)(st)
c_reviseRight'46_'35selFP76'35b (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP76'35b(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP76'35b x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP76#b")(x)



c_reviseRight'46_'35selFP77'35a :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP77'35a x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP77'35a_case_28(x5)(x4)(st)
c_reviseRight'46_'35selFP77'35a (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP77'35a(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP77'35a x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP77#a")(x)



c_reviseRight'46_'35selFP79'35col :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Color
c_reviseRight'46_'35selFP79'35col x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP79'35col_case_26(x2)(x4)(st)
c_reviseRight'46_'35selFP79'35col (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP79'35col(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP79'35col x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP79#col")(x)



c_reviseRight'46_'35selFP80'35x :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP80'35x x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP80'35x_case_24(x3)(x4)(st)
c_reviseRight'46_'35selFP80'35x (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP80'35x(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP80'35x x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP80#x")(x)



c_reviseRight'46_'35selFP81'35z :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP81'35z x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP81'35z_case_22(x4)(st)
c_reviseRight'46_'35selFP81'35z (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP81'35z(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP81'35z x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP81#z")(x)



c_reviseRight'46_'35selFP82'35d :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP82'35d x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP82'35d_case_20(x4)(st)
c_reviseRight'46_'35selFP82'35d (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP82'35d(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP82'35d x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP82#d")(x)



c_reviseRight'46_'35selFP83'35y :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP83'35y x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP83'35y_case_18(x4)(st)
c_reviseRight'46_'35selFP83'35y (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP83'35y(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP83'35y x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP83#y")(x)



c_reviseRight'46_'35selFP84'35c :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP84'35c x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP84'35c_case_16(x4)(st)
c_reviseRight'46_'35selFP84'35c (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP84'35c(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP84'35c x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP84#c")(x)



c_reviseRight'46_'35selFP85'35b :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP85'35b x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP85'35b_case_14(x4)(st)
c_reviseRight'46_'35selFP85'35b (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP85'35b(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP85'35b x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP85#b")(x)



c_reviseRight'46_'35selFP86'35a :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP86'35a x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP86'35a_case_12(x5)(x4)(st)
c_reviseRight'46_'35selFP86'35a (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP86'35a(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP86'35a x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP86#a")(x)



c_reviseRight'46_'35selFP88'35col :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Color
c_reviseRight'46_'35selFP88'35col x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP88'35col_case_10(x2)(x4)(st)
c_reviseRight'46_'35selFP88'35col (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP88'35col(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP88'35col x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP88#col")(x)



c_reviseRight'46_'35selFP89'35x :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP89'35x x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP89'35x_case_9(x3)(x4)(st)
c_reviseRight'46_'35selFP89'35x (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP89'35x(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP89'35x x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP89#x")(x)



c_reviseRight'46_'35selFP90'35y :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP90'35y x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP90'35y_case_8(x4)(st)
c_reviseRight'46_'35selFP90'35y (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP90'35y(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP90'35y x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP90#y")(x)



c_reviseRight'46_'35selFP91'35c :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP91'35c x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP91'35c_case_7(x4)(st)
c_reviseRight'46_'35selFP91'35c (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP91'35c(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP91'35c x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP91#c")(x)



c_reviseRight'46_'35selFP92'35b :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP92'35b x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP92'35b_case_6(x4)(st)
c_reviseRight'46_'35selFP92'35b (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP92'35b(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP92'35b x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP92#b")(x)



c_reviseRight'46_'35selFP93'35a :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP93'35a x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP93'35a_case_5(x5)(x4)(st)
c_reviseRight'46_'35selFP93'35a (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP93'35a(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP93'35a x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP93#a")(x)



c_reviseRight'46_'35selFP95'35x :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP95'35x x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP95'35x_case_4(x3)(x4)(st)
c_reviseRight'46_'35selFP95'35x (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP95'35x(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP95'35x x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP95#x")(x)



c_reviseRight'46_'35selFP96'35y :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP96'35y x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP96'35y_case_3(x4)(st)
c_reviseRight'46_'35selFP96'35y (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP96'35y(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP96'35y x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP96#y")(x)



c_reviseRight'46_'35selFP97'35c :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP97'35c x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP97'35c_case_2(x4)(st)
c_reviseRight'46_'35selFP97'35c (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP97'35c(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP97'35c x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP97#c")(x)



c_reviseRight'46_'35selFP98'35b :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP98'35b x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP98'35b_case_1(x4)(st)
c_reviseRight'46_'35selFP98'35b (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP98'35b(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP98'35b x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP98#b")(x)



c_reviseRight'46_'35selFP99'35a :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP99'35a x1@(Curry.Module.RedBlackTree.C_Tree x2 x3 x4 x5) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP99'35a_case_0(x5)(x4)(st)
c_reviseRight'46_'35selFP99'35a (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP99'35a(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP99'35a x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP99#a")(x)



c_reviseRight'46_'35selFP99'35a_case_0 x5 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x5
c_reviseRight'46_'35selFP99'35a_case_0 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP99'35a_case_0(x5)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP99'35a_case_0 x5 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP99#a_case_0")(x)



c_reviseRight'46_'35selFP98'35b_case_1 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x9
c_reviseRight'46_'35selFP98'35b_case_1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP98'35b_case_1(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP98'35b_case_1 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP98#b_case_1")(x)



c_reviseRight'46_'35selFP97'35c_case_2 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x8
c_reviseRight'46_'35selFP97'35c_case_2 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP97'35c_case_2(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP97'35c_case_2 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP97#c_case_2")(x)



c_reviseRight'46_'35selFP96'35y_case_3 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x7
c_reviseRight'46_'35selFP96'35y_case_3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP96'35y_case_3(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP96'35y_case_3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP96#y_case_3")(x)



c_reviseRight'46_'35selFP95'35x_case_4 x3 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x3
c_reviseRight'46_'35selFP95'35x_case_4 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP95'35x_case_4(x3)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP95'35x_case_4 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP95#x_case_4")(x)



c_reviseRight'46_'35selFP93'35a_case_5 x5 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x5
c_reviseRight'46_'35selFP93'35a_case_5 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP93'35a_case_5(x5)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP93'35a_case_5 x5 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP93#a_case_5")(x)



c_reviseRight'46_'35selFP92'35b_case_6 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x9
c_reviseRight'46_'35selFP92'35b_case_6 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP92'35b_case_6(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP92'35b_case_6 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP92#b_case_6")(x)



c_reviseRight'46_'35selFP91'35c_case_7 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x8
c_reviseRight'46_'35selFP91'35c_case_7 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP91'35c_case_7(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP91'35c_case_7 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP91#c_case_7")(x)



c_reviseRight'46_'35selFP90'35y_case_8 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x7
c_reviseRight'46_'35selFP90'35y_case_8 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP90'35y_case_8(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP90'35y_case_8 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP90#y_case_8")(x)



c_reviseRight'46_'35selFP89'35x_case_9 x3 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x3
c_reviseRight'46_'35selFP89'35x_case_9 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP89'35x_case_9(x3)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP89'35x_case_9 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP89#x_case_9")(x)



c_reviseRight'46_'35selFP88'35col_case_10 x2 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x2
c_reviseRight'46_'35selFP88'35col_case_10 x2 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP88'35col_case_10(x2)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP88'35col_case_10 x2 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP88#col_case_10")(x)



c_reviseRight'46_'35selFP86'35a_case_12 x5 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP86'35a_case_11(x5)(x9)(st)
c_reviseRight'46_'35selFP86'35a_case_12 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP86'35a_case_12(x5)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP86'35a_case_12 x5 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP86#a_case_12")(x)



c_reviseRight'46_'35selFP86'35a_case_11 x5 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x5
c_reviseRight'46_'35selFP86'35a_case_11 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP86'35a_case_11(x5)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP86'35a_case_11 x5 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP86#a_case_11")(x)



c_reviseRight'46_'35selFP85'35b_case_14 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP85'35b_case_13(x9)(st)
c_reviseRight'46_'35selFP85'35b_case_14 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP85'35b_case_14(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP85'35b_case_14 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP85#b_case_14")(x)



c_reviseRight'46_'35selFP85'35b_case_13 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x13
c_reviseRight'46_'35selFP85'35b_case_13 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP85'35b_case_13(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP85'35b_case_13 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP85#b_case_13")(x)



c_reviseRight'46_'35selFP84'35c_case_16 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP84'35c_case_15(x9)(st)
c_reviseRight'46_'35selFP84'35c_case_16 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP84'35c_case_16(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP84'35c_case_16 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP84#c_case_16")(x)



c_reviseRight'46_'35selFP84'35c_case_15 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x12
c_reviseRight'46_'35selFP84'35c_case_15 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP84'35c_case_15(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP84'35c_case_15 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP84#c_case_15")(x)



c_reviseRight'46_'35selFP83'35y_case_18 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP83'35y_case_17(x9)(st)
c_reviseRight'46_'35selFP83'35y_case_18 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP83'35y_case_18(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP83'35y_case_18 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP83#y_case_18")(x)



c_reviseRight'46_'35selFP83'35y_case_17 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x11
c_reviseRight'46_'35selFP83'35y_case_17 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP83'35y_case_17(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP83'35y_case_17 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP83#y_case_17")(x)



c_reviseRight'46_'35selFP82'35d_case_20 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP82'35d_case_19(x8)(x9)(st)
c_reviseRight'46_'35selFP82'35d_case_20 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP82'35d_case_20(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP82'35d_case_20 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP82#d_case_20")(x)



c_reviseRight'46_'35selFP82'35d_case_19 x8 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x8
c_reviseRight'46_'35selFP82'35d_case_19 x8 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP82'35d_case_19(x8)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP82'35d_case_19 x8 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP82#d_case_19")(x)



c_reviseRight'46_'35selFP81'35z_case_22 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP81'35z_case_21(x7)(x9)(st)
c_reviseRight'46_'35selFP81'35z_case_22 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP81'35z_case_22(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP81'35z_case_22 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP81#z_case_22")(x)



c_reviseRight'46_'35selFP81'35z_case_21 x7 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x7
c_reviseRight'46_'35selFP81'35z_case_21 x7 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP81'35z_case_21(x7)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP81'35z_case_21 x7 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP81#z_case_21")(x)



c_reviseRight'46_'35selFP80'35x_case_24 x3 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP80'35x_case_23(x3)(x9)(st)
c_reviseRight'46_'35selFP80'35x_case_24 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP80'35x_case_24(x3)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP80'35x_case_24 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP80#x_case_24")(x)



c_reviseRight'46_'35selFP80'35x_case_23 x3 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x3
c_reviseRight'46_'35selFP80'35x_case_23 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP80'35x_case_23(x3)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP80'35x_case_23 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP80#x_case_23")(x)



c_reviseRight'46_'35selFP79'35col_case_26 x2 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP79'35col_case_25(x2)(x9)(st)
c_reviseRight'46_'35selFP79'35col_case_26 x2 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP79'35col_case_26(x2)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP79'35col_case_26 x2 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP79#col_case_26")(x)



c_reviseRight'46_'35selFP79'35col_case_25 x2 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x2
c_reviseRight'46_'35selFP79'35col_case_25 x2 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP79'35col_case_25(x2)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP79'35col_case_25 x2 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP79#col_case_25")(x)



c_reviseRight'46_'35selFP77'35a_case_28 x5 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP77'35a_case_27(x5)(x8)(st)
c_reviseRight'46_'35selFP77'35a_case_28 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP77'35a_case_28(x5)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP77'35a_case_28 x5 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP77#a_case_28")(x)



c_reviseRight'46_'35selFP77'35a_case_27 x5 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x5
c_reviseRight'46_'35selFP77'35a_case_27 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP77'35a_case_27(x5)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP77'35a_case_27 x5 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP77#a_case_27")(x)



c_reviseRight'46_'35selFP76'35b_case_30 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP76'35b_case_29(x9)(x8)(st)
c_reviseRight'46_'35selFP76'35b_case_30 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP76'35b_case_30(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP76'35b_case_30 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP76#b_case_30")(x)



c_reviseRight'46_'35selFP76'35b_case_29 x9 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x9
c_reviseRight'46_'35selFP76'35b_case_29 x9 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP76'35b_case_29(x9)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP76'35b_case_29 x9 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP76#b_case_29")(x)



c_reviseRight'46_'35selFP75'35c_case_32 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP75'35c_case_31(x8)(st)
c_reviseRight'46_'35selFP75'35c_case_32 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP75'35c_case_32(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP75'35c_case_32 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP75#c_case_32")(x)



c_reviseRight'46_'35selFP75'35c_case_31 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x13
c_reviseRight'46_'35selFP75'35c_case_31 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP75'35c_case_31(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP75'35c_case_31 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP75#c_case_31")(x)



c_reviseRight'46_'35selFP74'35d_case_34 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP74'35d_case_33(x8)(st)
c_reviseRight'46_'35selFP74'35d_case_34 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP74'35d_case_34(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP74'35d_case_34 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP74#d_case_34")(x)



c_reviseRight'46_'35selFP74'35d_case_33 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x12
c_reviseRight'46_'35selFP74'35d_case_33 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP74'35d_case_33(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP74'35d_case_33 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP74#d_case_33")(x)



c_reviseRight'46_'35selFP73'35z_case_36 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP73'35z_case_35(x8)(st)
c_reviseRight'46_'35selFP73'35z_case_36 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP73'35z_case_36(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP73'35z_case_36 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP73#z_case_36")(x)



c_reviseRight'46_'35selFP73'35z_case_35 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x11
c_reviseRight'46_'35selFP73'35z_case_35 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP73'35z_case_35(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP73'35z_case_35 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP73#z_case_35")(x)



c_reviseRight'46_'35selFP72'35y_case_38 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP72'35y_case_37(x7)(x8)(st)
c_reviseRight'46_'35selFP72'35y_case_38 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP72'35y_case_38(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP72'35y_case_38 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP72#y_case_38")(x)



c_reviseRight'46_'35selFP72'35y_case_37 x7 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x7
c_reviseRight'46_'35selFP72'35y_case_37 x7 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP72'35y_case_37(x7)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP72'35y_case_37 x7 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP72#y_case_37")(x)



c_reviseRight'46_'35selFP71'35x_case_40 x3 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP71'35x_case_39(x3)(x8)(st)
c_reviseRight'46_'35selFP71'35x_case_40 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP71'35x_case_40(x3)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP71'35x_case_40 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP71#x_case_40")(x)



c_reviseRight'46_'35selFP71'35x_case_39 x3 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x3
c_reviseRight'46_'35selFP71'35x_case_39 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP71'35x_case_39(x3)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP71'35x_case_39 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP71#x_case_39")(x)



c_reviseRight'46_'35selFP70'35col_case_42 x2 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP70'35col_case_41(x2)(x8)(st)
c_reviseRight'46_'35selFP70'35col_case_42 x2 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP70'35col_case_42(x2)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP70'35col_case_42 x2 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP70#col_case_42")(x)



c_reviseRight'46_'35selFP70'35col_case_41 x2 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x2
c_reviseRight'46_'35selFP70'35col_case_41 x2 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP70'35col_case_41(x2)(x)(st))(i)(xs)(st)
c_reviseRight'46_'35selFP70'35col_case_41 x2 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight._#selFP70#col_case_41")(x)



c_reviseRight_case_48 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = x1
c_reviseRight_case_48 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_reviseRight_case_47(x1)(x2)(x3)(Curry.Module.Prelude.op_38_38(x3)(Curry.Module.RedBlackTree.c_isRed(Curry.Module.RedBlackTree.c_left(x2)(st))(st))(st))(st)
c_reviseRight_case_48 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight_case_48(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_reviseRight_case_48 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight_case_48")(x)



c_reviseRight_case_47 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP70'35col(x1)(st))(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP72'35y(x1)(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP73'35z(x1)(st))(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP74'35d(x1)(st))(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP75'35c(x1)(st)))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP71'35x(x1)(st))(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP76'35b(x1)(st))(Curry.Module.RedBlackTree.c_singleBlack(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP77'35a(x1)(st))(st)))
c_reviseRight_case_47 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_reviseRight_case_46(x1)(x2)(x3)(Curry.Module.Prelude.op_38_38(x3)(Curry.Module.RedBlackTree.c_isRed(Curry.Module.RedBlackTree.c_right(x2)(st))(st))(st))(st)
c_reviseRight_case_47 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight_case_47(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_reviseRight_case_47 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight_case_47")(x)



c_reviseRight_case_46 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP79'35col(x1)(st))(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP83'35y(x1)(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP81'35z(x1)(st))(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP82'35d(x1)(st))(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP84'35c(x1)(st)))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP80'35x(x1)(st))(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP85'35b(x1)(st))(Curry.Module.RedBlackTree.c_singleBlack(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP86'35a(x1)(st))(st)))
c_reviseRight_case_46 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_reviseRight_case_45(x1)(x3)(st)
c_reviseRight_case_46 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight_case_46(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_reviseRight_case_46 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight_case_46")(x)



c_reviseRight_case_45 x1 x3@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.c_reviseRight_case_44(x1)(Curry.Module.Prelude.op_61_61(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP88'35col(x1)(st))(Curry.Module.RedBlackTree.C_Red)(st))(st))(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP89'35x(x1)(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Red)(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP90'35y(x1)(st))(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP91'35c(x1)(st))(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP92'35b(x1)(st)))(Curry.Module.RedBlackTree.c_singleBlack(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP93'35a(x1)(st))(st))
c_reviseRight_case_45 x1 x3@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_reviseRight_case_43(x1)(Curry.Module.Prelude.c_otherwise(st))(st)
c_reviseRight_case_45 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight_case_45(x1)(x)(st))(i)(xs)(st)
c_reviseRight_case_45 x1 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight_case_45")(x)



c_reviseRight_case_43 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP96'35y(x1)(st))(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP97'35c(x1)(st))(Curry.Module.RedBlackTree.c_reviseRight(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Red)(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP95'35x(x1)(st))(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP98'35b(x1)(st))(Curry.Module.RedBlackTree.c_reviseRight'46_'35selFP99'35a(x1)(st)))(st))
c_reviseRight_case_43 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight_case_43(x1)(x)(st))(i)(xs)(st)
c_reviseRight_case_43 x1 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight_case_43")(x)



c_reviseRight_case_44 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.C_Black
c_reviseRight_case_44 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.C_DoublyBlack
c_reviseRight_case_44 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseRight_case_44(x1)(x)(st))(i)(xs)(st)
c_reviseRight_case_44 x1 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseRight_case_44")(x)



c_delBalanceR_case_49 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.c_reviseRight(x1)(st)
c_delBalanceR_case_49 x1 x2@Curry.Module.Prelude.C_False st = x1
c_delBalanceR_case_49 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_delBalanceR_case_49(x1)(x)(st))(i)(xs)(st)
c_delBalanceR_case_49 x1 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.delBalanceR_case_49")(x)



c_reviseLeft'46_'35selFP68'35c_case_50 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x9
c_reviseLeft'46_'35selFP68'35c_case_50 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP68'35c_case_50(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP68'35c_case_50 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP68#c_case_50")(x)



c_reviseLeft'46_'35selFP67'35b_case_51 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x8
c_reviseLeft'46_'35selFP67'35b_case_51 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP67'35b_case_51(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP67'35b_case_51 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP67#b_case_51")(x)



c_reviseLeft'46_'35selFP66'35y_case_52 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x7
c_reviseLeft'46_'35selFP66'35y_case_52 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP66'35y_case_52(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP66'35y_case_52 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP66#y_case_52")(x)



c_reviseLeft'46_'35selFP65'35a_case_53 x4 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x4
c_reviseLeft'46_'35selFP65'35a_case_53 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP65'35a_case_53(x4)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP65'35a_case_53 x4 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP65#a_case_53")(x)



c_reviseLeft'46_'35selFP64'35x_case_54 x3 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x3
c_reviseLeft'46_'35selFP64'35x_case_54 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP64'35x_case_54(x3)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP64'35x_case_54 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP64#x_case_54")(x)



c_reviseLeft'46_'35selFP62'35c_case_55 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x9
c_reviseLeft'46_'35selFP62'35c_case_55 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP62'35c_case_55(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP62'35c_case_55 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP62#c_case_55")(x)



c_reviseLeft'46_'35selFP61'35b_case_56 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x8
c_reviseLeft'46_'35selFP61'35b_case_56 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP61'35b_case_56(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP61'35b_case_56 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP61#b_case_56")(x)



c_reviseLeft'46_'35selFP60'35y_case_57 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x7
c_reviseLeft'46_'35selFP60'35y_case_57 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP60'35y_case_57(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP60'35y_case_57 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP60#y_case_57")(x)



c_reviseLeft'46_'35selFP59'35a_case_58 x4 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x4
c_reviseLeft'46_'35selFP59'35a_case_58 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP59'35a_case_58(x4)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP59'35a_case_58 x4 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP59#a_case_58")(x)



c_reviseLeft'46_'35selFP58'35x_case_59 x3 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x3
c_reviseLeft'46_'35selFP58'35x_case_59 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP58'35x_case_59(x3)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP58'35x_case_59 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP58#x_case_59")(x)



c_reviseLeft'46_'35selFP57'35col_case_60 x2 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = x2
c_reviseLeft'46_'35selFP57'35col_case_60 x2 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP57'35col_case_60(x2)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP57'35col_case_60 x2 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP57#col_case_60")(x)



c_reviseLeft'46_'35selFP55'35d_case_62 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP55'35d_case_61(x9)(st)
c_reviseLeft'46_'35selFP55'35d_case_62 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP55'35d_case_62(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP55'35d_case_62 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP55#d_case_62")(x)



c_reviseLeft'46_'35selFP55'35d_case_61 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x13
c_reviseLeft'46_'35selFP55'35d_case_61 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP55'35d_case_61(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP55'35d_case_61 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP55#d_case_61")(x)



c_reviseLeft'46_'35selFP54'35c_case_64 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP54'35c_case_63(x9)(st)
c_reviseLeft'46_'35selFP54'35c_case_64 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP54'35c_case_64(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP54'35c_case_64 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP54#c_case_64")(x)



c_reviseLeft'46_'35selFP54'35c_case_63 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x12
c_reviseLeft'46_'35selFP54'35c_case_63 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP54'35c_case_63(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP54'35c_case_63 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP54#c_case_63")(x)



c_reviseLeft'46_'35selFP53'35z_case_66 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP53'35z_case_65(x9)(st)
c_reviseLeft'46_'35selFP53'35z_case_66 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP53'35z_case_66(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP53'35z_case_66 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP53#z_case_66")(x)



c_reviseLeft'46_'35selFP53'35z_case_65 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x11
c_reviseLeft'46_'35selFP53'35z_case_65 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP53'35z_case_65(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP53'35z_case_65 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP53#z_case_65")(x)



c_reviseLeft'46_'35selFP52'35b_case_68 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP52'35b_case_67(x8)(x9)(st)
c_reviseLeft'46_'35selFP52'35b_case_68 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP52'35b_case_68(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP52'35b_case_68 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP52#b_case_68")(x)



c_reviseLeft'46_'35selFP52'35b_case_67 x8 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x8
c_reviseLeft'46_'35selFP52'35b_case_67 x8 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP52'35b_case_67(x8)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP52'35b_case_67 x8 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP52#b_case_67")(x)



c_reviseLeft'46_'35selFP51'35y_case_70 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP51'35y_case_69(x7)(x9)(st)
c_reviseLeft'46_'35selFP51'35y_case_70 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP51'35y_case_70(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP51'35y_case_70 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP51#y_case_70")(x)



c_reviseLeft'46_'35selFP51'35y_case_69 x7 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x7
c_reviseLeft'46_'35selFP51'35y_case_69 x7 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP51'35y_case_69(x7)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP51'35y_case_69 x7 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP51#y_case_69")(x)



c_reviseLeft'46_'35selFP50'35a_case_72 x4 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP50'35a_case_71(x4)(x9)(st)
c_reviseLeft'46_'35selFP50'35a_case_72 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP50'35a_case_72(x4)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP50'35a_case_72 x4 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP50#a_case_72")(x)



c_reviseLeft'46_'35selFP50'35a_case_71 x4 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x4
c_reviseLeft'46_'35selFP50'35a_case_71 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP50'35a_case_71(x4)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP50'35a_case_71 x4 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP50#a_case_71")(x)



c_reviseLeft'46_'35selFP49'35x_case_74 x3 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP49'35x_case_73(x3)(x9)(st)
c_reviseLeft'46_'35selFP49'35x_case_74 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP49'35x_case_74(x3)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP49'35x_case_74 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP49#x_case_74")(x)



c_reviseLeft'46_'35selFP49'35x_case_73 x3 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x3
c_reviseLeft'46_'35selFP49'35x_case_73 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP49'35x_case_73(x3)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP49'35x_case_73 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP49#x_case_73")(x)



c_reviseLeft'46_'35selFP48'35col_case_76 x2 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP48'35col_case_75(x2)(x9)(st)
c_reviseLeft'46_'35selFP48'35col_case_76 x2 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP48'35col_case_76(x2)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP48'35col_case_76 x2 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP48#col_case_76")(x)



c_reviseLeft'46_'35selFP48'35col_case_75 x2 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x2
c_reviseLeft'46_'35selFP48'35col_case_75 x2 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP48'35col_case_75(x2)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP48'35col_case_75 x2 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP48#col_case_75")(x)



c_reviseLeft'46_'35selFP46'35d_case_78 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP46'35d_case_77(x9)(x8)(st)
c_reviseLeft'46_'35selFP46'35d_case_78 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP46'35d_case_78(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP46'35d_case_78 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP46#d_case_78")(x)



c_reviseLeft'46_'35selFP46'35d_case_77 x9 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x9
c_reviseLeft'46_'35selFP46'35d_case_77 x9 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP46'35d_case_77(x9)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP46'35d_case_77 x9 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP46#d_case_77")(x)



c_reviseLeft'46_'35selFP45'35c_case_80 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP45'35c_case_79(x8)(st)
c_reviseLeft'46_'35selFP45'35c_case_80 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP45'35c_case_80(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP45'35c_case_80 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP45#c_case_80")(x)



c_reviseLeft'46_'35selFP45'35c_case_79 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x13
c_reviseLeft'46_'35selFP45'35c_case_79 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP45'35c_case_79(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP45'35c_case_79 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP45#c_case_79")(x)



c_reviseLeft'46_'35selFP44'35b_case_82 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP44'35b_case_81(x8)(st)
c_reviseLeft'46_'35selFP44'35b_case_82 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP44'35b_case_82(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP44'35b_case_82 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP44#b_case_82")(x)



c_reviseLeft'46_'35selFP44'35b_case_81 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x12
c_reviseLeft'46_'35selFP44'35b_case_81 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP44'35b_case_81(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP44'35b_case_81 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP44#b_case_81")(x)



c_reviseLeft'46_'35selFP43'35y_case_84 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP43'35y_case_83(x8)(st)
c_reviseLeft'46_'35selFP43'35y_case_84 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP43'35y_case_84(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP43'35y_case_84 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP43#y_case_84")(x)



c_reviseLeft'46_'35selFP43'35y_case_83 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x11
c_reviseLeft'46_'35selFP43'35y_case_83 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP43'35y_case_83(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP43'35y_case_83 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP43#y_case_83")(x)



c_reviseLeft'46_'35selFP42'35z_case_86 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP42'35z_case_85(x7)(x8)(st)
c_reviseLeft'46_'35selFP42'35z_case_86 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP42'35z_case_86(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP42'35z_case_86 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP42#z_case_86")(x)



c_reviseLeft'46_'35selFP42'35z_case_85 x7 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x7
c_reviseLeft'46_'35selFP42'35z_case_85 x7 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP42'35z_case_85(x7)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP42'35z_case_85 x7 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP42#z_case_85")(x)



c_reviseLeft'46_'35selFP41'35a_case_88 x4 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP41'35a_case_87(x4)(x8)(st)
c_reviseLeft'46_'35selFP41'35a_case_88 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP41'35a_case_88(x4)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP41'35a_case_88 x4 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP41#a_case_88")(x)



c_reviseLeft'46_'35selFP41'35a_case_87 x4 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x4
c_reviseLeft'46_'35selFP41'35a_case_87 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP41'35a_case_87(x4)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP41'35a_case_87 x4 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP41#a_case_87")(x)



c_reviseLeft'46_'35selFP40'35x_case_90 x3 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP40'35x_case_89(x3)(x8)(st)
c_reviseLeft'46_'35selFP40'35x_case_90 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP40'35x_case_90(x3)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP40'35x_case_90 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP40#x_case_90")(x)



c_reviseLeft'46_'35selFP40'35x_case_89 x3 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x3
c_reviseLeft'46_'35selFP40'35x_case_89 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP40'35x_case_89(x3)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP40'35x_case_89 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP40#x_case_89")(x)



c_reviseLeft'46_'35selFP39'35col_case_92 x2 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP39'35col_case_91(x2)(x8)(st)
c_reviseLeft'46_'35selFP39'35col_case_92 x2 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP39'35col_case_92(x2)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP39'35col_case_92 x2 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP39#col_case_92")(x)



c_reviseLeft'46_'35selFP39'35col_case_91 x2 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x2
c_reviseLeft'46_'35selFP39'35col_case_91 x2 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP39'35col_case_91(x2)(x)(st))(i)(xs)(st)
c_reviseLeft'46_'35selFP39'35col_case_91 x2 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft._#selFP39#col_case_91")(x)



c_reviseLeft_case_98 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = x1
c_reviseLeft_case_98 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_reviseLeft_case_97(x1)(x2)(x3)(Curry.Module.Prelude.op_38_38(x3)(Curry.Module.RedBlackTree.c_isRed(Curry.Module.RedBlackTree.c_left(x2)(st))(st))(st))(st)
c_reviseLeft_case_98 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft_case_98(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_reviseLeft_case_98 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft_case_98")(x)



c_reviseLeft_case_97 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP39'35col(x1)(st))(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP43'35y(x1)(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP40'35x(x1)(st))(Curry.Module.RedBlackTree.c_singleBlack(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP41'35a(x1)(st))(st))(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP44'35b(x1)(st)))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP42'35z(x1)(st))(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP45'35c(x1)(st))(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP46'35d(x1)(st)))
c_reviseLeft_case_97 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_reviseLeft_case_96(x1)(x2)(x3)(Curry.Module.Prelude.op_38_38(x3)(Curry.Module.RedBlackTree.c_isRed(Curry.Module.RedBlackTree.c_right(x2)(st))(st))(st))(st)
c_reviseLeft_case_97 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft_case_97(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_reviseLeft_case_97 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft_case_97")(x)



c_reviseLeft_case_96 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP48'35col(x1)(st))(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP51'35y(x1)(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP49'35x(x1)(st))(Curry.Module.RedBlackTree.c_singleBlack(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP50'35a(x1)(st))(st))(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP52'35b(x1)(st)))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP53'35z(x1)(st))(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP54'35c(x1)(st))(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP55'35d(x1)(st)))
c_reviseLeft_case_96 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_reviseLeft_case_95(x1)(x3)(st)
c_reviseLeft_case_96 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft_case_96(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_reviseLeft_case_96 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft_case_96")(x)



c_reviseLeft_case_95 x1 x3@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.c_reviseLeft_case_94(x1)(Curry.Module.Prelude.op_61_61(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP57'35col(x1)(st))(Curry.Module.RedBlackTree.C_Red)(st))(st))(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP58'35x(x1)(st))(Curry.Module.RedBlackTree.c_singleBlack(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP59'35a(x1)(st))(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Red)(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP60'35y(x1)(st))(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP61'35b(x1)(st))(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP62'35c(x1)(st)))
c_reviseLeft_case_95 x1 x3@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_reviseLeft_case_93(x1)(Curry.Module.Prelude.c_otherwise(st))(st)
c_reviseLeft_case_95 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft_case_95(x1)(x)(st))(i)(xs)(st)
c_reviseLeft_case_95 x1 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft_case_95")(x)



c_reviseLeft_case_93 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP66'35y(x1)(st))(Curry.Module.RedBlackTree.c_reviseLeft(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Red)(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP64'35x(x1)(st))(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP65'35a(x1)(st))(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP67'35b(x1)(st)))(st))(Curry.Module.RedBlackTree.c_reviseLeft'46_'35selFP68'35c(x1)(st))
c_reviseLeft_case_93 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft_case_93(x1)(x)(st))(i)(xs)(st)
c_reviseLeft_case_93 x1 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft_case_93")(x)



c_reviseLeft_case_94 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.C_Black
c_reviseLeft_case_94 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.C_DoublyBlack
c_reviseLeft_case_94 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_reviseLeft_case_94(x1)(x)(st))(i)(xs)(st)
c_reviseLeft_case_94 x1 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.reviseLeft_case_94")(x)



c_delBalanceL_case_99 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.c_reviseLeft(x1)(st)
c_delBalanceL_case_99 x1 x2@Curry.Module.Prelude.C_False st = x1
c_delBalanceL_case_99 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_delBalanceL_case_99(x1)(x)(st))(i)(xs)(st)
c_delBalanceL_case_99 x1 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.delBalanceL_case_99")(x)



c_balanceR'46_'35selFP37'35d_case_101 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP37'35d_case_100(x9)(x8)(st)
c_balanceR'46_'35selFP37'35d_case_101 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP37'35d_case_101(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP37'35d_case_101 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP37#d_case_101")(x)



c_balanceR'46_'35selFP37'35d_case_100 x9 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x9
c_balanceR'46_'35selFP37'35d_case_100 x9 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP37'35d_case_100(x9)(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP37'35d_case_100 x9 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP37#d_case_100")(x)



c_balanceR'46_'35selFP36'35c_case_103 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP36'35c_case_102(x8)(st)
c_balanceR'46_'35selFP36'35c_case_103 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP36'35c_case_103(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP36'35c_case_103 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP36#c_case_103")(x)



c_balanceR'46_'35selFP36'35c_case_102 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x13
c_balanceR'46_'35selFP36'35c_case_102 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP36'35c_case_102(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP36'35c_case_102 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP36#c_case_102")(x)



c_balanceR'46_'35selFP35'35b_case_105 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP35'35b_case_104(x8)(st)
c_balanceR'46_'35selFP35'35b_case_105 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP35'35b_case_105(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP35'35b_case_105 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP35#b_case_105")(x)



c_balanceR'46_'35selFP35'35b_case_104 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x12
c_balanceR'46_'35selFP35'35b_case_104 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP35'35b_case_104(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP35'35b_case_104 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP35#b_case_104")(x)



c_balanceR'46_'35selFP34'35y_case_107 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP34'35y_case_106(x8)(st)
c_balanceR'46_'35selFP34'35y_case_107 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP34'35y_case_107(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP34'35y_case_107 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP34#y_case_107")(x)



c_balanceR'46_'35selFP34'35y_case_106 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x11
c_balanceR'46_'35selFP34'35y_case_106 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP34'35y_case_106(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP34'35y_case_106 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP34#y_case_106")(x)



c_balanceR'46_'35selFP33'35z_case_109 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP33'35z_case_108(x7)(x8)(st)
c_balanceR'46_'35selFP33'35z_case_109 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP33'35z_case_109(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP33'35z_case_109 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP33#z_case_109")(x)



c_balanceR'46_'35selFP33'35z_case_108 x7 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x7
c_balanceR'46_'35selFP33'35z_case_108 x7 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP33'35z_case_108(x7)(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP33'35z_case_108 x7 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP33#z_case_108")(x)



c_balanceR'46_'35selFP32'35a_case_111 x4 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP32'35a_case_110(x4)(x8)(st)
c_balanceR'46_'35selFP32'35a_case_111 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP32'35a_case_111(x4)(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP32'35a_case_111 x4 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP32#a_case_111")(x)



c_balanceR'46_'35selFP32'35a_case_110 x4 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x4
c_balanceR'46_'35selFP32'35a_case_110 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP32'35a_case_110(x4)(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP32'35a_case_110 x4 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP32#a_case_110")(x)



c_balanceR'46_'35selFP31'35x_case_113 x3 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP31'35x_case_112(x3)(x8)(st)
c_balanceR'46_'35selFP31'35x_case_113 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP31'35x_case_113(x3)(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP31'35x_case_113 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP31#x_case_113")(x)



c_balanceR'46_'35selFP31'35x_case_112 x3 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x3
c_balanceR'46_'35selFP31'35x_case_112 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP31'35x_case_112(x3)(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP31'35x_case_112 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP31#x_case_112")(x)



c_balanceR'46_'35selFP29'35d_case_115 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP29'35d_case_114(x9)(st)
c_balanceR'46_'35selFP29'35d_case_115 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP29'35d_case_115(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP29'35d_case_115 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP29#d_case_115")(x)



c_balanceR'46_'35selFP29'35d_case_114 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x13
c_balanceR'46_'35selFP29'35d_case_114 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP29'35d_case_114(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP29'35d_case_114 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP29#d_case_114")(x)



c_balanceR'46_'35selFP28'35c_case_117 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP28'35c_case_116(x9)(st)
c_balanceR'46_'35selFP28'35c_case_117 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP28'35c_case_117(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP28'35c_case_117 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP28#c_case_117")(x)



c_balanceR'46_'35selFP28'35c_case_116 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x12
c_balanceR'46_'35selFP28'35c_case_116 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP28'35c_case_116(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP28'35c_case_116 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP28#c_case_116")(x)



c_balanceR'46_'35selFP27'35z_case_119 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP27'35z_case_118(x9)(st)
c_balanceR'46_'35selFP27'35z_case_119 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP27'35z_case_119(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP27'35z_case_119 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP27#z_case_119")(x)



c_balanceR'46_'35selFP27'35z_case_118 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x11
c_balanceR'46_'35selFP27'35z_case_118 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP27'35z_case_118(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP27'35z_case_118 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP27#z_case_118")(x)



c_balanceR'46_'35selFP26'35b_case_121 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP26'35b_case_120(x8)(x9)(st)
c_balanceR'46_'35selFP26'35b_case_121 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP26'35b_case_121(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP26'35b_case_121 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP26#b_case_121")(x)



c_balanceR'46_'35selFP26'35b_case_120 x8 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x8
c_balanceR'46_'35selFP26'35b_case_120 x8 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP26'35b_case_120(x8)(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP26'35b_case_120 x8 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP26#b_case_120")(x)



c_balanceR'46_'35selFP25'35y_case_123 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP25'35y_case_122(x7)(x9)(st)
c_balanceR'46_'35selFP25'35y_case_123 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP25'35y_case_123(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP25'35y_case_123 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP25#y_case_123")(x)



c_balanceR'46_'35selFP25'35y_case_122 x7 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x7
c_balanceR'46_'35selFP25'35y_case_122 x7 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP25'35y_case_122(x7)(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP25'35y_case_122 x7 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP25#y_case_122")(x)



c_balanceR'46_'35selFP24'35a_case_125 x4 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP24'35a_case_124(x4)(x9)(st)
c_balanceR'46_'35selFP24'35a_case_125 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP24'35a_case_125(x4)(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP24'35a_case_125 x4 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP24#a_case_125")(x)



c_balanceR'46_'35selFP24'35a_case_124 x4 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x4
c_balanceR'46_'35selFP24'35a_case_124 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP24'35a_case_124(x4)(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP24'35a_case_124 x4 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP24#a_case_124")(x)



c_balanceR'46_'35selFP23'35x_case_127 x3 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceR'46_'35selFP23'35x_case_126(x3)(x9)(st)
c_balanceR'46_'35selFP23'35x_case_127 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP23'35x_case_127(x3)(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP23'35x_case_127 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP23#x_case_127")(x)



c_balanceR'46_'35selFP23'35x_case_126 x3 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x3
c_balanceR'46_'35selFP23'35x_case_126 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR'46_'35selFP23'35x_case_126(x3)(x)(st))(i)(xs)(st)
c_balanceR'46_'35selFP23'35x_case_126 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR._#selFP23#x_case_126")(x)



c_balanceR_case_130 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Red)(Curry.Module.RedBlackTree.c_balanceR'46_'35selFP25'35y(x1)(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.RedBlackTree.c_balanceR'46_'35selFP23'35x(x1)(st))(Curry.Module.RedBlackTree.c_balanceR'46_'35selFP24'35a(x1)(st))(Curry.Module.RedBlackTree.c_balanceR'46_'35selFP26'35b(x1)(st)))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.RedBlackTree.c_balanceR'46_'35selFP27'35z(x1)(st))(Curry.Module.RedBlackTree.c_balanceR'46_'35selFP28'35c(x1)(st))(Curry.Module.RedBlackTree.c_balanceR'46_'35selFP29'35d(x1)(st)))
c_balanceR_case_130 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_balanceR_case_129(x1)(x2)(Curry.Module.Prelude.op_38_38(Curry.Module.RedBlackTree.c_isRed(x2)(st))(Curry.Module.RedBlackTree.c_isRed(Curry.Module.RedBlackTree.c_left(x2)(st))(st))(st))(st)
c_balanceR_case_130 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR_case_130(x1)(x2)(x)(st))(i)(xs)(st)
c_balanceR_case_130 x1 x2 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR_case_130")(x)



c_balanceR_case_129 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Red)(Curry.Module.RedBlackTree.c_balanceR'46_'35selFP34'35y(x1)(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.RedBlackTree.c_balanceR'46_'35selFP31'35x(x1)(st))(Curry.Module.RedBlackTree.c_balanceR'46_'35selFP32'35a(x1)(st))(Curry.Module.RedBlackTree.c_balanceR'46_'35selFP35'35b(x1)(st)))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.RedBlackTree.c_balanceR'46_'35selFP33'35z(x1)(st))(Curry.Module.RedBlackTree.c_balanceR'46_'35selFP36'35c(x1)(st))(Curry.Module.RedBlackTree.c_balanceR'46_'35selFP37'35d(x1)(st)))
c_balanceR_case_129 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_balanceR_case_128(x1)(Curry.Module.Prelude.c_otherwise(st))(st)
c_balanceR_case_129 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR_case_129(x1)(x2)(x)(st))(i)(xs)(st)
c_balanceR_case_129 x1 x2 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR_case_129")(x)



c_balanceR_case_128 x1 x2@Curry.Module.Prelude.C_True st = x1
c_balanceR_case_128 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceR_case_128(x1)(x)(st))(i)(xs)(st)
c_balanceR_case_128 x1 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceR_case_128")(x)



c_balanceL'46_'35selFP21'35d_case_132 x5 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP21'35d_case_131(x5)(x9)(st)
c_balanceL'46_'35selFP21'35d_case_132 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP21'35d_case_132(x5)(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP21'35d_case_132 x5 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP21#d_case_132")(x)



c_balanceL'46_'35selFP21'35d_case_131 x5 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x5
c_balanceL'46_'35selFP21'35d_case_131 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP21'35d_case_131(x5)(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP21'35d_case_131 x5 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP21#d_case_131")(x)



c_balanceL'46_'35selFP20'35c_case_134 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP20'35c_case_133(x9)(st)
c_balanceL'46_'35selFP20'35c_case_134 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP20'35c_case_134(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP20'35c_case_134 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP20#c_case_134")(x)



c_balanceL'46_'35selFP20'35c_case_133 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x13
c_balanceL'46_'35selFP20'35c_case_133 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP20'35c_case_133(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP20'35c_case_133 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP20#c_case_133")(x)



c_balanceL'46_'35selFP19'35b_case_136 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP19'35b_case_135(x9)(st)
c_balanceL'46_'35selFP19'35b_case_136 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP19'35b_case_136(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP19'35b_case_136 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP19#b_case_136")(x)



c_balanceL'46_'35selFP19'35b_case_135 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x12
c_balanceL'46_'35selFP19'35b_case_135 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP19'35b_case_135(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP19'35b_case_135 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP19#b_case_135")(x)



c_balanceL'46_'35selFP18'35y_case_138 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP18'35y_case_137(x9)(st)
c_balanceL'46_'35selFP18'35y_case_138 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP18'35y_case_138(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP18'35y_case_138 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP18#y_case_138")(x)



c_balanceL'46_'35selFP18'35y_case_137 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x11
c_balanceL'46_'35selFP18'35y_case_137 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP18'35y_case_137(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP18'35y_case_137 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP18#y_case_137")(x)



c_balanceL'46_'35selFP17'35a_case_140 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP17'35a_case_139(x8)(x9)(st)
c_balanceL'46_'35selFP17'35a_case_140 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP17'35a_case_140(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP17'35a_case_140 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP17#a_case_140")(x)



c_balanceL'46_'35selFP17'35a_case_139 x8 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x8
c_balanceL'46_'35selFP17'35a_case_139 x8 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP17'35a_case_139(x8)(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP17'35a_case_139 x8 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP17#a_case_139")(x)



c_balanceL'46_'35selFP16'35x_case_142 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP16'35x_case_141(x7)(x9)(st)
c_balanceL'46_'35selFP16'35x_case_142 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP16'35x_case_142(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP16'35x_case_142 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP16#x_case_142")(x)



c_balanceL'46_'35selFP16'35x_case_141 x7 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x7
c_balanceL'46_'35selFP16'35x_case_141 x7 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP16'35x_case_141(x7)(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP16'35x_case_141 x7 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP16#x_case_141")(x)



c_balanceL'46_'35selFP15'35z_case_144 x3 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP15'35z_case_143(x3)(x9)(st)
c_balanceL'46_'35selFP15'35z_case_144 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP15'35z_case_144(x3)(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP15'35z_case_144 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP15#z_case_144")(x)



c_balanceL'46_'35selFP15'35z_case_143 x3 x9@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x3
c_balanceL'46_'35selFP15'35z_case_143 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP15'35z_case_143(x3)(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP15'35z_case_143 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP15#z_case_143")(x)



c_balanceL'46_'35selFP13'35d_case_146 x5 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP13'35d_case_145(x5)(x8)(st)
c_balanceL'46_'35selFP13'35d_case_146 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP13'35d_case_146(x5)(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP13'35d_case_146 x5 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP13#d_case_146")(x)



c_balanceL'46_'35selFP13'35d_case_145 x5 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x5
c_balanceL'46_'35selFP13'35d_case_145 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP13'35d_case_145(x5)(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP13'35d_case_145 x5 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP13#d_case_145")(x)



c_balanceL'46_'35selFP12'35c_case_148 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP12'35c_case_147(x9)(x8)(st)
c_balanceL'46_'35selFP12'35c_case_148 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP12'35c_case_148(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP12'35c_case_148 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP12#c_case_148")(x)



c_balanceL'46_'35selFP12'35c_case_147 x9 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x9
c_balanceL'46_'35selFP12'35c_case_147 x9 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP12'35c_case_147(x9)(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP12'35c_case_147 x9 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP12#c_case_147")(x)



c_balanceL'46_'35selFP11'35b_case_150 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP11'35b_case_149(x8)(st)
c_balanceL'46_'35selFP11'35b_case_150 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP11'35b_case_150(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP11'35b_case_150 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP11#b_case_150")(x)



c_balanceL'46_'35selFP11'35b_case_149 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x13
c_balanceL'46_'35selFP11'35b_case_149 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP11'35b_case_149(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP11'35b_case_149 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP11#b_case_149")(x)



c_balanceL'46_'35selFP10'35a_case_152 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP10'35a_case_151(x8)(st)
c_balanceL'46_'35selFP10'35a_case_152 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP10'35a_case_152(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP10'35a_case_152 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP10#a_case_152")(x)



c_balanceL'46_'35selFP10'35a_case_151 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x12
c_balanceL'46_'35selFP10'35a_case_151 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP10'35a_case_151(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP10'35a_case_151 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP10#a_case_151")(x)



c_balanceL'46_'35selFP9'35x_case_154 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP9'35x_case_153(x8)(st)
c_balanceL'46_'35selFP9'35x_case_154 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP9'35x_case_154(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP9'35x_case_154 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP9#x_case_154")(x)



c_balanceL'46_'35selFP9'35x_case_153 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x11
c_balanceL'46_'35selFP9'35x_case_153 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP9'35x_case_153(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP9'35x_case_153 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP9#x_case_153")(x)



c_balanceL'46_'35selFP8'35y_case_156 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP8'35y_case_155(x7)(x8)(st)
c_balanceL'46_'35selFP8'35y_case_156 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP8'35y_case_156(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP8'35y_case_156 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP8#y_case_156")(x)



c_balanceL'46_'35selFP8'35y_case_155 x7 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x7
c_balanceL'46_'35selFP8'35y_case_155 x7 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP8'35y_case_155(x7)(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP8'35y_case_155 x7 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP8#y_case_155")(x)



c_balanceL'46_'35selFP7'35z_case_158 x3 x4@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.RedBlackTree.c_balanceL'46_'35selFP7'35z_case_157(x3)(x8)(st)
c_balanceL'46_'35selFP7'35z_case_158 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP7'35z_case_158(x3)(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP7'35z_case_158 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP7#z_case_158")(x)



c_balanceL'46_'35selFP7'35z_case_157 x3 x8@(Curry.Module.RedBlackTree.C_Tree x10 x11 x12 x13) st = x3
c_balanceL'46_'35selFP7'35z_case_157 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL'46_'35selFP7'35z_case_157(x3)(x)(st))(i)(xs)(st)
c_balanceL'46_'35selFP7'35z_case_157 x3 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL._#selFP7#z_case_157")(x)



c_balanceL_case_161 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Red)(Curry.Module.RedBlackTree.c_balanceL'46_'35selFP8'35y(x1)(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.RedBlackTree.c_balanceL'46_'35selFP9'35x(x1)(st))(Curry.Module.RedBlackTree.c_balanceL'46_'35selFP10'35a(x1)(st))(Curry.Module.RedBlackTree.c_balanceL'46_'35selFP11'35b(x1)(st)))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.RedBlackTree.c_balanceL'46_'35selFP7'35z(x1)(st))(Curry.Module.RedBlackTree.c_balanceL'46_'35selFP12'35c(x1)(st))(Curry.Module.RedBlackTree.c_balanceL'46_'35selFP13'35d(x1)(st)))
c_balanceL_case_161 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_balanceL_case_160(x1)(x2)(Curry.Module.Prelude.op_38_38(Curry.Module.RedBlackTree.c_isRed(x2)(st))(Curry.Module.RedBlackTree.c_isRed(Curry.Module.RedBlackTree.c_right(x2)(st))(st))(st))(st)
c_balanceL_case_161 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL_case_161(x1)(x2)(x)(st))(i)(xs)(st)
c_balanceL_case_161 x1 x2 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL_case_161")(x)



c_balanceL_case_160 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Red)(Curry.Module.RedBlackTree.c_balanceL'46_'35selFP18'35y(x1)(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.RedBlackTree.c_balanceL'46_'35selFP16'35x(x1)(st))(Curry.Module.RedBlackTree.c_balanceL'46_'35selFP17'35a(x1)(st))(Curry.Module.RedBlackTree.c_balanceL'46_'35selFP19'35b(x1)(st)))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.RedBlackTree.c_balanceL'46_'35selFP15'35z(x1)(st))(Curry.Module.RedBlackTree.c_balanceL'46_'35selFP20'35c(x1)(st))(Curry.Module.RedBlackTree.c_balanceL'46_'35selFP21'35d(x1)(st)))
c_balanceL_case_160 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_balanceL_case_159(x1)(Curry.Module.Prelude.c_otherwise(st))(st)
c_balanceL_case_160 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL_case_160(x1)(x2)(x)(st))(i)(xs)(st)
c_balanceL_case_160 x1 x2 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL_case_160")(x)



c_balanceL_case_159 x1 x2@Curry.Module.Prelude.C_True st = x1
c_balanceL_case_159 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_balanceL_case_159(x1)(x)(st))(i)(xs)(st)
c_balanceL_case_159 x1 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.balanceL_case_159")(x)



c_singleBlack_case_162 x3 x4 x5 x2@Curry.Module.RedBlackTree.C_DoublyBlack st = Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(x3)(x4)(x5)
c_singleBlack_case_162 x3 x4 x5 (Curry.Module.RedBlackTree.C_ColorOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_singleBlack_case_162(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c_singleBlack_case_162 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.singleBlack_case_162")(x)



c_deleteTree'46rightMost'4656_case_163 x3 x5 x6@Curry.Module.Prelude.C_True st = x3
c_deleteTree'46rightMost'4656_case_163 x3 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_deleteTree'46rightMost'4656(x5)(st)
c_deleteTree'46rightMost'4656_case_163 x3 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_deleteTree'46rightMost'4656_case_163(x3)(x5)(x)(st))(i)(xs)(st)
c_deleteTree'46rightMost'4656_case_163 x3 x5 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.deleteTree.rightMost.56_case_163")(x)



c_deleteTree'46addColor'4656_case_165 x2@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.RedBlackTree.C_Empty
c_deleteTree'46addColor'4656_case_165 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.RedBlackTree.c_deleteTree'46addColor'4656_case_164(x4)(x5)(x6)(x3)(st)
c_deleteTree'46addColor'4656_case_165 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_deleteTree'46addColor'4656_case_165(x)(st))(i)(xs)(st)
c_deleteTree'46addColor'4656_case_165 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.deleteTree.addColor.56_case_165")(x)



c_deleteTree'46addColor'4656_case_164 x4 x5 x6 x3@Curry.Module.RedBlackTree.C_Red st = Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(x4)(x5)(x6)
c_deleteTree'46addColor'4656_case_164 x4 x5 x6 x3@Curry.Module.RedBlackTree.C_Black st = Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_DoublyBlack)(x4)(x5)(x6)
c_deleteTree'46addColor'4656_case_164 x4 x5 x6 (Curry.Module.RedBlackTree.C_ColorOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_deleteTree'46addColor'4656_case_164(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_deleteTree'46addColor'4656_case_164 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.deleteTree.addColor.56_case_164")(x)



c_deleteTree_case_170 x1 x2 x3 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.c_deleteTree_case_169(x1)(x2)(x5)(x7)(x8)(Curry.Module.Prelude.op_61_61(x7)(Curry.Module.RedBlackTree.C_Empty)(st))(st)
c_deleteTree_case_170 x1 x2 x3 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_deleteTree_case_167(x1)(x2)(x3)(x5)(x6)(x7)(x8)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x2)(x3)(st))(x6)(st))(st)
c_deleteTree_case_170 x1 x2 x3 x5 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_deleteTree_case_170(x1)(x2)(x3)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c_deleteTree_case_170 x1 x2 x3 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.deleteTree_case_170")(x)



c_deleteTree_case_167 x1 x2 x3 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.c_delBalanceL(Curry.Module.RedBlackTree.C_Tree(x5)(x6)(Curry.Module.RedBlackTree.c_deleteTree(x1)(x2)(x3)(x7)(st))(x8))(st)
c_deleteTree_case_167 x1 x2 x3 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_deleteTree_case_166(x1)(x2)(x3)(x5)(x6)(x7)(x8)(Curry.Module.Prelude.c_otherwise(st))(st)
c_deleteTree_case_167 x1 x2 x3 x5 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_deleteTree_case_167(x1)(x2)(x3)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c_deleteTree_case_167 x1 x2 x3 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.deleteTree_case_167")(x)



c_deleteTree_case_166 x1 x2 x3 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.c_delBalanceR(Curry.Module.RedBlackTree.C_Tree(x5)(x6)(x7)(Curry.Module.RedBlackTree.c_deleteTree(x1)(x2)(x3)(x8)(st)))(st)
c_deleteTree_case_166 x1 x2 x3 x5 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_deleteTree_case_166(x1)(x2)(x3)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c_deleteTree_case_166 x1 x2 x3 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.deleteTree_case_166")(x)



c_deleteTree_case_169 x1 x2 x5 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.c_deleteTree'46addColor'4656(x5)(x8)(st)
c_deleteTree_case_169 x1 x2 x5 x7 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_deleteTree_case_168(x1)(x2)(x5)(x7)(x8)(Curry.Module.Prelude.op_61_61(x8)(Curry.Module.RedBlackTree.C_Empty)(st))(st)
c_deleteTree_case_169 x1 x2 x5 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_deleteTree_case_169(x1)(x2)(x5)(x7)(x8)(x)(st))(i)(xs)(st)
c_deleteTree_case_169 x1 x2 x5 x7 x8 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.deleteTree_case_169")(x)



c_deleteTree_case_168 x1 x2 x5 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.c_deleteTree'46addColor'4656(x5)(x7)(st)
c_deleteTree_case_168 x1 x2 x5 x7 x8 x9@Curry.Module.Prelude.C_False st = let {x9 = Curry.Module.RedBlackTree.c_deleteTree'46rightMost'4656(x7)(st)} in Curry.Module.RedBlackTree.c_delBalanceL(Curry.Module.RedBlackTree.C_Tree(x5)(x9)(Curry.Module.RedBlackTree.c_deleteTree(x1)(x2)(x9)(x7)(st))(x8))(st)
c_deleteTree_case_168 x1 x2 x5 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_deleteTree_case_168(x1)(x2)(x5)(x7)(x8)(x)(st))(i)(xs)(st)
c_deleteTree_case_168 x1 x2 x5 x7 x8 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.deleteTree_case_168")(x)



c_updateTree'46upd'4635_case_173 x1 x2 x3 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.C_Tree(x5)(x1)(x7)(x8)
c_updateTree'46upd'4635_case_173 x1 x2 x3 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_updateTree'46upd'4635_case_172(x1)(x2)(x3)(x5)(x6)(x7)(x8)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x3)(x1)(st))(x6)(st))(st)
c_updateTree'46upd'4635_case_173 x1 x2 x3 x5 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_updateTree'46upd'4635_case_173(x1)(x2)(x3)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c_updateTree'46upd'4635_case_173 x1 x2 x3 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.updateTree.upd.35_case_173")(x)



c_updateTree'46upd'4635_case_172 x1 x2 x3 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.c_balanceL(Curry.Module.RedBlackTree.C_Tree(x5)(x6)(Curry.Module.RedBlackTree.c_updateTree'46upd'4635(x1)(x2)(x3)(x7)(st))(x8))(st)
c_updateTree'46upd'4635_case_172 x1 x2 x3 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_updateTree'46upd'4635_case_171(x1)(x2)(x3)(x5)(x6)(x7)(x8)(Curry.Module.Prelude.c_otherwise(st))(st)
c_updateTree'46upd'4635_case_172 x1 x2 x3 x5 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_updateTree'46upd'4635_case_172(x1)(x2)(x3)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c_updateTree'46upd'4635_case_172 x1 x2 x3 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.updateTree.upd.35_case_172")(x)



c_updateTree'46upd'4635_case_171 x1 x2 x3 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.c_balanceR(Curry.Module.RedBlackTree.C_Tree(x5)(x6)(x7)(Curry.Module.RedBlackTree.c_updateTree'46upd'4635(x1)(x2)(x3)(x8)(st)))(st)
c_updateTree'46upd'4635_case_171 x1 x2 x3 x5 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_updateTree'46upd'4635_case_171(x1)(x2)(x3)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c_updateTree'46upd'4635_case_171 x1 x2 x3 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.updateTree.upd.35_case_171")(x)



c_lookupTree_case_176 x1 x2 x3 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_Just(x6)
c_lookupTree_case_176 x1 x2 x3 x6 x7 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_lookupTree_case_175(x1)(x2)(x3)(x6)(x7)(x8)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x2)(x3)(st))(x6)(st))(st)
c_lookupTree_case_176 x1 x2 x3 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_lookupTree_case_176(x1)(x2)(x3)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c_lookupTree_case_176 x1 x2 x3 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.lookupTree_case_176")(x)



c_lookupTree_case_175 x1 x2 x3 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.c_lookupTree(x1)(x2)(x3)(x7)(st)
c_lookupTree_case_175 x1 x2 x3 x6 x7 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.RedBlackTree.c_lookupTree_case_174(x1)(x2)(x3)(x8)(Curry.Module.Prelude.c_otherwise(st))(st)
c_lookupTree_case_175 x1 x2 x3 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_lookupTree_case_175(x1)(x2)(x3)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c_lookupTree_case_175 x1 x2 x3 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.lookupTree_case_175")(x)



c_lookupTree_case_174 x1 x2 x3 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.RedBlackTree.c_lookupTree(x1)(x2)(x3)(x8)(st)
c_lookupTree_case_174 x1 x2 x3 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_lookupTree_case_174(x1)(x2)(x3)(x8)(x)(st))(i)(xs)(st)
c_lookupTree_case_174 x1 x2 x3 x8 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.lookupTree_case_174")(x)



c_isEmpty_case_177 x5@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.Prelude.C_True
c_isEmpty_case_177 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = Curry.Module.Prelude.C_False
c_isEmpty_case_177 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RedBlackTree.c_isEmpty_case_177(x)(st))(i)(xs)(st)
c_isEmpty_case_177 x st = Curry.RunTimeSystem.patternFail("RedBlackTree.isEmpty_case_177")(x)


