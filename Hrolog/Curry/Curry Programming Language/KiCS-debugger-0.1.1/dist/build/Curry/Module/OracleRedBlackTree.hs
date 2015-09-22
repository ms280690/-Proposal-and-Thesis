{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleRedBlackTree (module Curry.Module.OracleRedBlackTree) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.RedBlackTree
import Curry.Module.Prelude
import Curry.Module.OraclePrelude



-- begin included



-- end included

data C_RedBlackTree t0 = C_RedBlackTree (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) (Curry.Module.RedBlackTree.C_Tree t0)
  | C_RedBlackTreeFail Curry.RunTimeSystem.C_Exceptions
  | C_RedBlackTreeOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.OracleRedBlackTree.C_RedBlackTree t0))

instance (BaseCurry t0) => BaseCurry (Curry.Module.OracleRedBlackTree.C_RedBlackTree t0) where
  nf f (Curry.Module.OracleRedBlackTree.C_RedBlackTree x1 x2 x3 x4) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> f(Curry.Module.OracleRedBlackTree.C_RedBlackTree(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.OracleRedBlackTree.C_RedBlackTree x1 x2 x3 x4) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> f(Curry.Module.OracleRedBlackTree.C_RedBlackTree(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.OracleRedBlackTree.C_RedBlackTreeOr(Curry.RunTimeSystem.mkRef(r)(4)(i))([Curry.Module.OracleRedBlackTree.C_RedBlackTree(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(4)

  failed  = Curry.Module.OracleRedBlackTree.C_RedBlackTreeFail

  branching  = Curry.Module.OracleRedBlackTree.C_RedBlackTreeOr

  consKind (Curry.Module.OracleRedBlackTree.C_RedBlackTreeOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.OracleRedBlackTree.C_RedBlackTreeFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.OracleRedBlackTree.C_RedBlackTreeFail x) = x

  orRef (Curry.Module.OracleRedBlackTree.C_RedBlackTreeOr x _) = x

  branches (Curry.Module.OracleRedBlackTree.C_RedBlackTreeOr _ x) = x





instance (Curry t0) => Curry (Curry.Module.OracleRedBlackTree.C_RedBlackTree t0) where
  strEq (Curry.Module.OracleRedBlackTree.C_RedBlackTree x1 x2 x3 x4) (Curry.Module.OracleRedBlackTree.C_RedBlackTree y1 y2 y3 y4) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.OracleRedBlackTree.C_RedBlackTree x1 x2 x3 x4) (Curry.Module.OracleRedBlackTree.C_RedBlackTree y1 y2 y3 y4) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.genEq(x4)(y4)(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.OracleRedBlackTree.C_RedBlackTree x1 x2 x3 x4) st = Curry.Module.OracleRedBlackTree.C_RedBlackTree(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))

  foldCurry f c (Curry.Module.OracleRedBlackTree.C_RedBlackTree x1 x2 x3 x4) st = f(x1)(f(x2)(f(x3)(f(x4)(c)(st))(st))(st))(st)

  typeName _ = "RedBlackTree"

  showQ d (Curry.Module.OracleRedBlackTree.C_RedBlackTree x1 x2 x3 x4) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("OracleRedBlackTree.RedBlackTree "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))))))))


  showQ _ (Curry.Module.OracleRedBlackTree.C_RedBlackTreeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.OracleRedBlackTree.C_RedBlackTree t0) where
  showsPrec d (Curry.Module.OracleRedBlackTree.C_RedBlackTree x1 x2 x3 x4) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("RedBlackTree "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))))))))


  showsPrec _ (Curry.Module.OracleRedBlackTree.C_RedBlackTreeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Read t0) => Read (Curry.Module.OracleRedBlackTree.C_RedBlackTree t0) where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.OracleRedBlackTree.C_RedBlackTree(x1)(x2)(x3)(x4))(r4) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("OracleRedBlackTree")("RedBlackTree")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3)])(r)





c_empty :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleRedBlackTree.C_RedBlackTree t0
c_empty x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.OracleRedBlackTree.C_RedBlackTree(x2)(x3)(x4)(Curry.Module.RedBlackTree.C_Empty))(st)



c_isEmpty :: (Curry t0) => (Curry.Module.OracleRedBlackTree.C_RedBlackTree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isEmpty x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_284(x2)(x1)(st))(st)



c_newTreeLike :: (Curry t0) => (Curry.Module.OracleRedBlackTree.C_RedBlackTree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleRedBlackTree.C_RedBlackTree t0
c_newTreeLike x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_282(x2)(x1)(st))(st)



c_lookup :: (Curry t0) => t0 -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t0
c_lookup x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_281(x2)(x3)(x1)(st))(st)



c_lookupTree :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> t0 -> (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t0
c_lookupTree x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_280(x2)(x3)(x4)(x5)(x1)(st))(st)



c_update :: (Curry t0) => t0 -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleRedBlackTree.C_RedBlackTree t0
c_update x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_276(x2)(x3)(x1)(st))(st)



c_updateTree :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> t0 -> (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_updateTree x2 x3 x4 x5 x1 st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(let {x6 = Curry.Module.OracleRedBlackTree.c_updateTree'46upd'4635(x4)(x2)(x3)(x5)(x1)(st)} in let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x13)(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.OracleRedBlackTree.c_updateTree'46_'35selFP3'35e2(x6)(x10)(st))(Curry.Module.OracleRedBlackTree.c_updateTree'46_'35selFP4'35l(x6)(x11)(st))(Curry.Module.OracleRedBlackTree.c_updateTree'46_'35selFP5'35r(x6)(x12)(st)))(st))(st))(st))(st))(st)



c_updateTree'46upd'4635 :: (Curry t183) => t183 -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t183 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t183 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t183 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t183 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> (Curry.Module.RedBlackTree.C_Tree t183) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t183
c_updateTree'46upd'4635 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_275(x2)(x3)(x4)(x5)(x1)(st))(st)



c_updateTree'46_'35selFP3'35e2 :: (Curry t183) => (Curry.Module.RedBlackTree.C_Tree t183) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t183
c_updateTree'46_'35selFP3'35e2 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_271(x2)(x1)(st))(st)



c_updateTree'46_'35selFP4'35l :: (Curry t183) => (Curry.Module.RedBlackTree.C_Tree t183) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t183
c_updateTree'46_'35selFP4'35l x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_270(x2)(x1)(st))(st)



c_updateTree'46_'35selFP5'35r :: (Curry t183) => (Curry.Module.RedBlackTree.C_Tree t183) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t183
c_updateTree'46_'35selFP5'35r x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_269(x2)(x1)(st))(st)



c_delete :: (Curry t0) => t0 -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleRedBlackTree.C_RedBlackTree t0
c_delete x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_268(x2)(x3)(x1)(st))(st)



c_delete'46blackenRoot'4644 :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_delete'46blackenRoot'4644 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_267(x2)(x1)(st))(st)



c_deleteTree :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> t0 -> (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_deleteTree x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_266(x2)(x3)(x4)(x5)(x1)(st))(st)



c_deleteTree'46addColor'4656 :: (Curry t0) => Curry.Module.RedBlackTree.C_Color -> (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_deleteTree'46addColor'4656 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_260(x3)(x2)(x1)(st))(st)



c_deleteTree'46rightMost'4656 :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_deleteTree'46rightMost'4656 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_257(x2)(x1)(st))(st)



c_tree2list :: (Curry t0) => (Curry.Module.OracleRedBlackTree.C_RedBlackTree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_tree2list x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_255(x2)(x1)(st))(st)



c_tree2listTree :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_tree2listTree x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c_tree2listTree'46t2l'4677(x2)(Curry.Module.Prelude.List)(x1)(st))(st)



c_tree2listTree'46t2l'4677 :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_tree2listTree'46t2l'4677 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_254(x3)(x2)(x1)(st))(st)



c_sort :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_sort x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OracleRedBlackTree.c_tree2list(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleRedBlackTree.c_update))(st))(Curry.Module.OracleRedBlackTree.c_empty(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleRedBlackTree.c_sort'46_'35lambda2))(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_61_61))(st))(x2)(x1)(st))(x3)(x4)(st))(x5)(st))(st)



c_sort'46_'35lambda2 :: (Curry t520) => t520 -> t520 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_sort'46_'35lambda2 x2 x3 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)



c_setInsertEquivalence :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleRedBlackTree.C_RedBlackTree t0
c_setInsertEquivalence x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_253(x2)(x3)(x1)(st))(st)



c_rbt :: (Curry t0) => (Curry.Module.OracleRedBlackTree.C_RedBlackTree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_rbt x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_252(x2)(x1)(st))(st)



c_isBlack :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isBlack x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_251(x2)(x1)(st))(st)



c_isRed :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isRed x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_250(x2)(x1)(st))(st)



c_isDoublyBlack :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isDoublyBlack x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_249(x2)(x1)(st))(st)



c_element :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_element x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_248(x2)(x1)(st))(st)



c_left :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_left x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_247(x2)(x1)(st))(st)



c_right :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_right x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_246(x2)(x1)(st))(st)



c_singleBlack :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_singleBlack x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_245(x2)(x1)(st))(st)



c_balanceL :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_balanceL x2 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(let {x3 = Curry.Module.OracleRedBlackTree.c_left(x2)(x1)(st)} in let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))))(Curry.Module.OracleRedBlackTree.c__case_243(x2)(x3)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OracleRedBlackTree.c_isRed(x3)(x4)(st))(Curry.Module.OracleRedBlackTree.c_isRed(Curry.Module.OracleRedBlackTree.c_left(x3)(x5)(st))(x6)(st))(x7)(st))(x8)(st))(st))(st)



c_balanceL'46_'35selFP7'35z :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t88
c_balanceL'46_'35selFP7'35z x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_240(x2)(x1)(st))(st)



c_balanceL'46_'35selFP8'35y :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t88
c_balanceL'46_'35selFP8'35y x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_237(x2)(x1)(st))(st)



c_balanceL'46_'35selFP9'35x :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t88
c_balanceL'46_'35selFP9'35x x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_234(x2)(x1)(st))(st)



c_balanceL'46_'35selFP10'35a :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t88
c_balanceL'46_'35selFP10'35a x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_231(x2)(x1)(st))(st)



c_balanceL'46_'35selFP11'35b :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t88
c_balanceL'46_'35selFP11'35b x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_228(x2)(x1)(st))(st)



c_balanceL'46_'35selFP12'35c :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t88
c_balanceL'46_'35selFP12'35c x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_225(x2)(x1)(st))(st)



c_balanceL'46_'35selFP13'35d :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t88
c_balanceL'46_'35selFP13'35d x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_222(x2)(x1)(st))(st)



c_balanceL'46_'35selFP15'35z :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t88
c_balanceL'46_'35selFP15'35z x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_219(x2)(x1)(st))(st)



c_balanceL'46_'35selFP16'35x :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t88
c_balanceL'46_'35selFP16'35x x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_216(x2)(x1)(st))(st)



c_balanceL'46_'35selFP17'35a :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t88
c_balanceL'46_'35selFP17'35a x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_213(x2)(x1)(st))(st)



c_balanceL'46_'35selFP18'35y :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t88
c_balanceL'46_'35selFP18'35y x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_210(x2)(x1)(st))(st)



c_balanceL'46_'35selFP19'35b :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t88
c_balanceL'46_'35selFP19'35b x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_207(x2)(x1)(st))(st)



c_balanceL'46_'35selFP20'35c :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t88
c_balanceL'46_'35selFP20'35c x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_204(x2)(x1)(st))(st)



c_balanceL'46_'35selFP21'35d :: (Curry t88) => (Curry.Module.RedBlackTree.C_Tree t88) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t88
c_balanceL'46_'35selFP21'35d x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_201(x2)(x1)(st))(st)



c_balanceR :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_balanceR x2 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(let {x3 = Curry.Module.OracleRedBlackTree.c_right(x2)(x1)(st)} in let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))))(Curry.Module.OracleRedBlackTree.c__case_198(x2)(x3)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OracleRedBlackTree.c_isRed(x3)(x4)(st))(Curry.Module.OracleRedBlackTree.c_isRed(Curry.Module.OracleRedBlackTree.c_right(x3)(x5)(st))(x6)(st))(x7)(st))(x8)(st))(st))(st)



c_balanceR'46_'35selFP23'35x :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t131
c_balanceR'46_'35selFP23'35x x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_195(x2)(x1)(st))(st)



c_balanceR'46_'35selFP24'35a :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t131
c_balanceR'46_'35selFP24'35a x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_192(x2)(x1)(st))(st)



c_balanceR'46_'35selFP25'35y :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t131
c_balanceR'46_'35selFP25'35y x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_189(x2)(x1)(st))(st)



c_balanceR'46_'35selFP26'35b :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t131
c_balanceR'46_'35selFP26'35b x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_186(x2)(x1)(st))(st)



c_balanceR'46_'35selFP27'35z :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t131
c_balanceR'46_'35selFP27'35z x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_183(x2)(x1)(st))(st)



c_balanceR'46_'35selFP28'35c :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t131
c_balanceR'46_'35selFP28'35c x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_180(x2)(x1)(st))(st)



c_balanceR'46_'35selFP29'35d :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t131
c_balanceR'46_'35selFP29'35d x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_177(x2)(x1)(st))(st)



c_balanceR'46_'35selFP31'35x :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t131
c_balanceR'46_'35selFP31'35x x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_174(x2)(x1)(st))(st)



c_balanceR'46_'35selFP32'35a :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t131
c_balanceR'46_'35selFP32'35a x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_171(x2)(x1)(st))(st)



c_balanceR'46_'35selFP33'35z :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t131
c_balanceR'46_'35selFP33'35z x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_168(x2)(x1)(st))(st)



c_balanceR'46_'35selFP34'35y :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t131
c_balanceR'46_'35selFP34'35y x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_165(x2)(x1)(st))(st)



c_balanceR'46_'35selFP35'35b :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t131
c_balanceR'46_'35selFP35'35b x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_162(x2)(x1)(st))(st)



c_balanceR'46_'35selFP36'35c :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t131
c_balanceR'46_'35selFP36'35c x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_159(x2)(x1)(st))(st)



c_balanceR'46_'35selFP37'35d :: (Curry t131) => (Curry.Module.RedBlackTree.C_Tree t131) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t131
c_balanceR'46_'35selFP37'35d x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_156(x2)(x1)(st))(st)



c_delBalanceL :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_delBalanceL x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OracleRedBlackTree.c__case_153(x2)(Curry.Module.OracleRedBlackTree.c_isDoublyBlack(Curry.Module.OracleRedBlackTree.c_left(x2)(x1)(st))(x3)(st))(x4)(st))(st)



c_reviseLeft :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_reviseLeft x2 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(let {x3 = Curry.Module.OracleRedBlackTree.c_right(x2)(x1)(st)} in let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleRedBlackTree.c__case_152(x2)(x3)(Curry.Module.OracleRedBlackTree.c_isBlack(x3)(x5)(st))(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.RedBlackTree.C_Empty)(x6)(st))(x7)(st))(st))(st))(st)



c_reviseLeft'46_'35selFP39'35col :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Color
c_reviseLeft'46_'35selFP39'35col x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_146(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP40'35x :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP40'35x x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_143(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP41'35a :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP41'35a x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_140(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP42'35z :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP42'35z x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_137(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP43'35y :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP43'35y x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_134(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP44'35b :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP44'35b x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_131(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP45'35c :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP45'35c x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_128(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP46'35d :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP46'35d x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_125(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP48'35col :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Color
c_reviseLeft'46_'35selFP48'35col x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_122(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP49'35x :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP49'35x x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_119(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP50'35a :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP50'35a x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_116(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP51'35y :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP51'35y x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_113(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP52'35b :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP52'35b x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_110(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP53'35z :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP53'35z x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_107(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP54'35c :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP54'35c x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_104(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP55'35d :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP55'35d x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_101(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP57'35col :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Color
c_reviseLeft'46_'35selFP57'35col x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_98(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP58'35x :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP58'35x x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_96(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP59'35a :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP59'35a x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_94(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP60'35y :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP60'35y x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_92(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP61'35b :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP61'35b x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_90(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP62'35c :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP62'35c x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_88(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP64'35x :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP64'35x x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_86(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP65'35a :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP65'35a x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_84(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP66'35y :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t249
c_reviseLeft'46_'35selFP66'35y x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_82(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP67'35b :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP67'35b x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_80(x2)(x1)(st))(st)



c_reviseLeft'46_'35selFP68'35c :: (Curry t249) => (Curry.Module.RedBlackTree.C_Tree t249) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t249
c_reviseLeft'46_'35selFP68'35c x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_78(x2)(x1)(st))(st)



c_delBalanceR :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_delBalanceR x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OracleRedBlackTree.c__case_76(x2)(Curry.Module.OracleRedBlackTree.c_isDoublyBlack(Curry.Module.OracleRedBlackTree.c_right(x2)(x1)(st))(x3)(st))(x4)(st))(st)



c_reviseRight :: (Curry t0) => (Curry.Module.RedBlackTree.C_Tree t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t0
c_reviseRight x2 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(let {x3 = Curry.Module.OracleRedBlackTree.c_left(x2)(x1)(st)} in let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleRedBlackTree.c__case_75(x2)(x3)(Curry.Module.OracleRedBlackTree.c_isBlack(x3)(x5)(st))(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.RedBlackTree.C_Empty)(x6)(st))(x7)(st))(st))(st))(st)



c_reviseRight'46_'35selFP70'35col :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Color
c_reviseRight'46_'35selFP70'35col x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_69(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP71'35x :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP71'35x x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_66(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP72'35y :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP72'35y x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_63(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP73'35z :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP73'35z x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_60(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP74'35d :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP74'35d x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_57(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP75'35c :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP75'35c x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_54(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP76'35b :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP76'35b x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_51(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP77'35a :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP77'35a x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_48(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP79'35col :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Color
c_reviseRight'46_'35selFP79'35col x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_45(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP80'35x :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP80'35x x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_42(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP81'35z :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP81'35z x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_39(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP82'35d :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP82'35d x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_36(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP83'35y :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP83'35y x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_33(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP84'35c :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP84'35c x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_30(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP85'35b :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP85'35b x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_27(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP86'35a :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP86'35a x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_24(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP88'35col :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Color
c_reviseRight'46_'35selFP88'35col x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_21(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP89'35x :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP89'35x x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_19(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP90'35y :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP90'35y x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_17(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP91'35c :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP91'35c x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_15(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP92'35b :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP92'35b x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_13(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP93'35a :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP93'35a x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_11(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP95'35x :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP95'35x x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_9(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP96'35y :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t327
c_reviseRight'46_'35selFP96'35y x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_7(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP97'35c :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP97'35c x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_5(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP98'35b :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP98'35b x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_3(x2)(x1)(st))(st)



c_reviseRight'46_'35selFP99'35a :: (Curry t327) => (Curry.Module.RedBlackTree.C_Tree t327) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_Tree t327
c_reviseRight'46_'35selFP99'35a x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_1(x2)(x1)(st))(st)



c__case_1 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_1_case__284(x1)(x2)(st))(st)



c__case_0 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_0_case__283(x1)(x6)(x5)(st))(st)



c__case_3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_3_case__282(x1)(x2)(st))(st)



c__case_2 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_2_case__281(x1)(x5)(st))(st)



c__case_5 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_5_case__280(x1)(x2)(st))(st)



c__case_4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_4_case__279(x1)(x5)(st))(st)



c__case_7 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_7_case__278(x1)(x2)(st))(st)



c__case_6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_6_case__277(x1)(x5)(st))(st)



c__case_9 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_9_case__276(x1)(x2)(st))(st)



c__case_8 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_8_case__275(x1)(x4)(x5)(st))(st)



c__case_11 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_11_case__274(x1)(x2)(st))(st)



c__case_10 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_10_case__273(x1)(x6)(x5)(st))(st)



c__case_13 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_13_case__272(x1)(x2)(st))(st)



c__case_12 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_12_case__271(x1)(x5)(st))(st)



c__case_15 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_15_case__270(x1)(x2)(st))(st)



c__case_14 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_14_case__269(x1)(x5)(st))(st)



c__case_17 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_17_case__268(x1)(x2)(st))(st)



c__case_16 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_16_case__267(x1)(x5)(st))(st)



c__case_19 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_19_case__266(x1)(x2)(st))(st)



c__case_18 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_18_case__265(x1)(x4)(x5)(st))(st)



c__case_21 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_21_case__264(x1)(x2)(st))(st)



c__case_20 x3 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_20_case__263(x1)(x3)(x5)(st))(st)



c__case_24 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_24_case__262(x1)(x2)(st))(st)



c__case_23 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_23_case__261(x1)(x6)(x5)(st))(st)



c__case_22 x6 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_22_case__260(x1)(x6)(x10)(st))(st)



c__case_27 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_27_case__259(x1)(x2)(st))(st)



c__case_26 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_26_case__258(x1)(x5)(st))(st)



c__case_25 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_25_case__257(x1)(x10)(st))(st)



c__case_30 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_30_case__256(x1)(x2)(st))(st)



c__case_29 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_29_case__255(x1)(x5)(st))(st)



c__case_28 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_28_case__254(x1)(x10)(st))(st)



c__case_33 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_33_case__253(x1)(x2)(st))(st)



c__case_32 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_32_case__252(x1)(x5)(st))(st)



c__case_31 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_31_case__251(x1)(x10)(st))(st)



c__case_36 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_36_case__250(x1)(x2)(st))(st)



c__case_35 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_35_case__249(x1)(x5)(st))(st)



c__case_34 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_34_case__248(x1)(x9)(x10)(st))(st)



c__case_39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_39_case__247(x1)(x2)(st))(st)



c__case_38 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_38_case__246(x1)(x5)(st))(st)



c__case_37 x8 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_37_case__245(x1)(x8)(x10)(st))(st)



c__case_42 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_42_case__244(x1)(x2)(st))(st)



c__case_41 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_41_case__243(x1)(x4)(x5)(st))(st)



c__case_40 x4 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_40_case__242(x1)(x4)(x10)(st))(st)



c__case_45 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_45_case__241(x1)(x2)(st))(st)



c__case_44 x3 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_44_case__240(x1)(x3)(x5)(st))(st)



c__case_43 x3 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_43_case__239(x1)(x3)(x10)(st))(st)



c__case_48 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_48_case__238(x1)(x2)(st))(st)



c__case_47 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_47_case__237(x1)(x6)(x5)(st))(st)



c__case_46 x6 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_46_case__236(x1)(x6)(x9)(st))(st)



c__case_51 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_51_case__235(x1)(x2)(st))(st)



c__case_50 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_50_case__234(x1)(x5)(st))(st)



c__case_49 x10 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_49_case__233(x1)(x10)(x9)(st))(st)



c__case_54 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_54_case__232(x1)(x2)(st))(st)



c__case_53 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_53_case__231(x1)(x5)(st))(st)



c__case_52 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_52_case__230(x1)(x9)(st))(st)



c__case_57 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_57_case__229(x1)(x2)(st))(st)



c__case_56 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_56_case__228(x1)(x5)(st))(st)



c__case_55 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_55_case__227(x1)(x9)(st))(st)



c__case_60 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_60_case__226(x1)(x2)(st))(st)



c__case_59 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_59_case__225(x1)(x5)(st))(st)



c__case_58 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_58_case__224(x1)(x9)(st))(st)



c__case_63 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_63_case__223(x1)(x2)(st))(st)



c__case_62 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_62_case__222(x1)(x5)(st))(st)



c__case_61 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_61_case__221(x1)(x8)(x9)(st))(st)



c__case_66 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_66_case__220(x1)(x2)(st))(st)



c__case_65 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_65_case__219(x1)(x4)(x5)(st))(st)



c__case_64 x4 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_64_case__218(x1)(x4)(x9)(st))(st)



c__case_69 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_69_case__217(x1)(x2)(st))(st)



c__case_68 x3 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_68_case__216(x1)(x3)(x5)(st))(st)



c__case_67 x3 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_67_case__215(x1)(x3)(x9)(st))(st)



c__case_75 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_75_case__214(x1)(x2)(x3)(x4)(x5)(st))(st)



c__case_74 x2 x3 x4 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_74_case__213(x1)(x2)(x3)(x4)(x14)(st))(st)



c__case_73 x2 x3 x4 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_73_case__212(x1)(x2)(x4)(x23)(st))(st)



c__case_72 x2 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_72_case__211(x1)(x2)(x4)(st))(st)



c__case_70 x2 x36 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_70_case__210(x1)(x2)(x36)(st))(st)



c__case_71 x24 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_71_case__209(x1)(x25)(st))(st)



c__case_76 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_76_case__208(x1)(x2)(x3)(st))(st)



c__case_78 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_78_case__207(x1)(x2)(st))(st)



c__case_77 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_77_case__206(x1)(x6)(st))(st)



c__case_80 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_80_case__205(x1)(x2)(st))(st)



c__case_79 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_79_case__204(x1)(x6)(st))(st)



c__case_82 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_82_case__203(x1)(x2)(st))(st)



c__case_81 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_81_case__202(x1)(x6)(st))(st)



c__case_84 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_84_case__201(x1)(x2)(st))(st)



c__case_83 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_83_case__200(x1)(x5)(x6)(st))(st)



c__case_86 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_86_case__199(x1)(x2)(st))(st)



c__case_85 x4 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_85_case__198(x1)(x4)(x6)(st))(st)



c__case_88 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_88_case__197(x1)(x2)(st))(st)



c__case_87 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_87_case__196(x1)(x6)(st))(st)



c__case_90 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_90_case__195(x1)(x2)(st))(st)



c__case_89 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_89_case__194(x1)(x6)(st))(st)



c__case_92 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_92_case__193(x1)(x2)(st))(st)



c__case_91 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_91_case__192(x1)(x6)(st))(st)



c__case_94 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_94_case__191(x1)(x2)(st))(st)



c__case_93 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_93_case__190(x1)(x5)(x6)(st))(st)



c__case_96 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_96_case__189(x1)(x2)(st))(st)



c__case_95 x4 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_95_case__188(x1)(x4)(x6)(st))(st)



c__case_98 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_98_case__187(x1)(x2)(st))(st)



c__case_97 x3 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_97_case__186(x1)(x3)(x6)(st))(st)



c__case_101 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_101_case__185(x1)(x2)(st))(st)



c__case_100 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_100_case__184(x1)(x6)(st))(st)



c__case_99 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_99_case__183(x1)(x10)(st))(st)



c__case_104 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_104_case__182(x1)(x2)(st))(st)



c__case_103 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_103_case__181(x1)(x6)(st))(st)



c__case_102 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_102_case__180(x1)(x10)(st))(st)



c__case_107 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_107_case__179(x1)(x2)(st))(st)



c__case_106 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_106_case__178(x1)(x6)(st))(st)



c__case_105 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_105_case__177(x1)(x10)(st))(st)



c__case_110 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_110_case__176(x1)(x2)(st))(st)



c__case_109 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_109_case__175(x1)(x6)(st))(st)



c__case_108 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_108_case__174(x1)(x9)(x10)(st))(st)



c__case_113 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_113_case__173(x1)(x2)(st))(st)



c__case_112 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_112_case__172(x1)(x6)(st))(st)



c__case_111 x8 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_111_case__171(x1)(x8)(x10)(st))(st)



c__case_116 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_116_case__170(x1)(x2)(st))(st)



c__case_115 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_115_case__169(x1)(x5)(x6)(st))(st)



c__case_114 x5 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_114_case__168(x1)(x5)(x10)(st))(st)



c__case_119 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_119_case__167(x1)(x2)(st))(st)



c__case_118 x4 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_118_case__166(x1)(x4)(x6)(st))(st)



c__case_117 x4 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_117_case__165(x1)(x4)(x10)(st))(st)



c__case_122 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_122_case__164(x1)(x2)(st))(st)



c__case_121 x3 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_121_case__163(x1)(x3)(x6)(st))(st)



c__case_120 x3 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_120_case__162(x1)(x3)(x10)(st))(st)



c__case_125 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_125_case__161(x1)(x2)(st))(st)



c__case_124 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_124_case__160(x1)(x6)(st))(st)



c__case_123 x10 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_123_case__159(x1)(x10)(x9)(st))(st)



c__case_128 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_128_case__158(x1)(x2)(st))(st)



c__case_127 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_127_case__157(x1)(x6)(st))(st)



c__case_126 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_126_case__156(x1)(x9)(st))(st)



c__case_131 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_131_case__155(x1)(x2)(st))(st)



c__case_130 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_130_case__154(x1)(x6)(st))(st)



c__case_129 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_129_case__153(x1)(x9)(st))(st)



c__case_134 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_134_case__152(x1)(x2)(st))(st)



c__case_133 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_133_case__151(x1)(x6)(st))(st)



c__case_132 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_132_case__150(x1)(x9)(st))(st)



c__case_137 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_137_case__149(x1)(x2)(st))(st)



c__case_136 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_136_case__148(x1)(x6)(st))(st)



c__case_135 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_135_case__147(x1)(x8)(x9)(st))(st)



c__case_140 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_140_case__146(x1)(x2)(st))(st)



c__case_139 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_139_case__145(x1)(x5)(x6)(st))(st)



c__case_138 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_138_case__144(x1)(x5)(x9)(st))(st)



c__case_143 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_143_case__143(x1)(x2)(st))(st)



c__case_142 x4 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_142_case__142(x1)(x4)(x6)(st))(st)



c__case_141 x4 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_141_case__141(x1)(x4)(x9)(st))(st)



c__case_146 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_146_case__140(x1)(x2)(st))(st)



c__case_145 x3 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_145_case__139(x1)(x3)(x6)(st))(st)



c__case_144 x3 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_144_case__138(x1)(x3)(x9)(st))(st)



c__case_152 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_152_case__137(x1)(x2)(x3)(x4)(x5)(st))(st)



c__case_151 x2 x3 x4 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_151_case__136(x1)(x2)(x3)(x4)(x14)(st))(st)



c__case_150 x2 x3 x4 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_150_case__135(x1)(x2)(x4)(x23)(st))(st)



c__case_149 x2 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_149_case__134(x1)(x2)(x4)(st))(st)



c__case_147 x2 x36 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_147_case__133(x1)(x2)(x36)(st))(st)



c__case_148 x24 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_148_case__132(x1)(x25)(st))(st)



c__case_153 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_153_case__131(x1)(x2)(x3)(st))(st)



c__case_156 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_156_case__130(x1)(x2)(st))(st)



c__case_155 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_155_case__129(x1)(x6)(st))(st)



c__case_154 x10 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_154_case__128(x1)(x10)(x9)(st))(st)



c__case_159 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_159_case__127(x1)(x2)(st))(st)



c__case_158 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_158_case__126(x1)(x6)(st))(st)



c__case_157 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_157_case__125(x1)(x9)(st))(st)



c__case_162 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_162_case__124(x1)(x2)(st))(st)



c__case_161 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_161_case__123(x1)(x6)(st))(st)



c__case_160 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_160_case__122(x1)(x9)(st))(st)



c__case_165 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_165_case__121(x1)(x2)(st))(st)



c__case_164 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_164_case__120(x1)(x6)(st))(st)



c__case_163 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_163_case__119(x1)(x9)(st))(st)



c__case_168 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_168_case__118(x1)(x2)(st))(st)



c__case_167 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_167_case__117(x1)(x6)(st))(st)



c__case_166 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_166_case__116(x1)(x8)(x9)(st))(st)



c__case_171 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_171_case__115(x1)(x2)(st))(st)



c__case_170 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_170_case__114(x1)(x5)(x6)(st))(st)



c__case_169 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_169_case__113(x1)(x5)(x9)(st))(st)



c__case_174 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_174_case__112(x1)(x2)(st))(st)



c__case_173 x4 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_173_case__111(x1)(x4)(x6)(st))(st)



c__case_172 x4 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_172_case__110(x1)(x4)(x9)(st))(st)



c__case_177 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_177_case__109(x1)(x2)(st))(st)



c__case_176 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_176_case__108(x1)(x6)(st))(st)



c__case_175 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_175_case__107(x1)(x10)(st))(st)



c__case_180 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_180_case__106(x1)(x2)(st))(st)



c__case_179 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_179_case__105(x1)(x6)(st))(st)



c__case_178 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_178_case__104(x1)(x10)(st))(st)



c__case_183 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_183_case__103(x1)(x2)(st))(st)



c__case_182 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_182_case__102(x1)(x6)(st))(st)



c__case_181 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_181_case__101(x1)(x10)(st))(st)



c__case_186 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_186_case__100(x1)(x2)(st))(st)



c__case_185 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_185_case__99(x1)(x6)(st))(st)



c__case_184 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_184_case__98(x1)(x9)(x10)(st))(st)



c__case_189 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_189_case__97(x1)(x2)(st))(st)



c__case_188 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_188_case__96(x1)(x6)(st))(st)



c__case_187 x8 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_187_case__95(x1)(x8)(x10)(st))(st)



c__case_192 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_192_case__94(x1)(x2)(st))(st)



c__case_191 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_191_case__93(x1)(x5)(x6)(st))(st)



c__case_190 x5 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_190_case__92(x1)(x5)(x10)(st))(st)



c__case_195 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_195_case__91(x1)(x2)(st))(st)



c__case_194 x4 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_194_case__90(x1)(x4)(x6)(st))(st)



c__case_193 x4 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_193_case__89(x1)(x4)(x10)(st))(st)



c__case_198 x2 x3 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_198_case__88(x1)(x2)(x3)(x12)(st))(st)



c__case_197 x2 x3 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_197_case__87(x1)(x2)(x20)(st))(st)



c__case_196 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_196_case__86(x1)(x2)(x3)(st))(st)



c__case_201 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_201_case__85(x1)(x2)(st))(st)



c__case_200 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_200_case__84(x1)(x6)(x5)(st))(st)



c__case_199 x6 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_199_case__83(x1)(x6)(x10)(st))(st)



c__case_204 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_204_case__82(x1)(x2)(st))(st)



c__case_203 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_203_case__81(x1)(x5)(st))(st)



c__case_202 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_202_case__80(x1)(x10)(st))(st)



c__case_207 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_207_case__79(x1)(x2)(st))(st)



c__case_206 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_206_case__78(x1)(x5)(st))(st)



c__case_205 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_205_case__77(x1)(x10)(st))(st)



c__case_210 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_210_case__76(x1)(x2)(st))(st)



c__case_209 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_209_case__75(x1)(x5)(st))(st)



c__case_208 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_208_case__74(x1)(x10)(st))(st)



c__case_213 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_213_case__73(x1)(x2)(st))(st)



c__case_212 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_212_case__72(x1)(x5)(st))(st)



c__case_211 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_211_case__71(x1)(x9)(x10)(st))(st)



c__case_216 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_216_case__70(x1)(x2)(st))(st)



c__case_215 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_215_case__69(x1)(x5)(st))(st)



c__case_214 x8 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_214_case__68(x1)(x8)(x10)(st))(st)



c__case_219 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_219_case__67(x1)(x2)(st))(st)



c__case_218 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_218_case__66(x1)(x4)(x5)(st))(st)



c__case_217 x4 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_217_case__65(x1)(x4)(x10)(st))(st)



c__case_222 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_222_case__64(x1)(x2)(st))(st)



c__case_221 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_221_case__63(x1)(x6)(x5)(st))(st)



c__case_220 x6 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_220_case__62(x1)(x6)(x9)(st))(st)



c__case_225 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_225_case__61(x1)(x2)(st))(st)



c__case_224 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_224_case__60(x1)(x5)(st))(st)



c__case_223 x10 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_223_case__59(x1)(x10)(x9)(st))(st)



c__case_228 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_228_case__58(x1)(x2)(st))(st)



c__case_227 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_227_case__57(x1)(x5)(st))(st)



c__case_226 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_226_case__56(x1)(x9)(st))(st)



c__case_231 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_231_case__55(x1)(x2)(st))(st)



c__case_230 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_230_case__54(x1)(x5)(st))(st)



c__case_229 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_229_case__53(x1)(x9)(st))(st)



c__case_234 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_234_case__52(x1)(x2)(st))(st)



c__case_233 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_233_case__51(x1)(x5)(st))(st)



c__case_232 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_232_case__50(x1)(x9)(st))(st)



c__case_237 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_237_case__49(x1)(x2)(st))(st)



c__case_236 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_236_case__48(x1)(x5)(st))(st)



c__case_235 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_235_case__47(x1)(x8)(x9)(st))(st)



c__case_240 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_240_case__46(x1)(x2)(st))(st)



c__case_239 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_239_case__45(x1)(x4)(x5)(st))(st)



c__case_238 x4 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_238_case__44(x1)(x4)(x9)(st))(st)



c__case_243 x2 x3 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_243_case__43(x1)(x2)(x3)(x12)(st))(st)



c__case_242 x2 x3 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_242_case__42(x1)(x2)(x20)(st))(st)



c__case_241 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_241_case__41(x1)(x2)(x3)(st))(st)



c__case_245 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_245_case__40(x1)(x2)(st))(st)



c__case_244 x4 x5 x6 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_244_case__39(x1)(x4)(x5)(x6)(x3)(st))(st)



c__case_246 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_246_case__38(x1)(x2)(st))(st)



c__case_247 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_247_case__37(x1)(x2)(st))(st)



c__case_248 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_248_case__36(x1)(x2)(st))(st)



c__case_249 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_249_case__35(x1)(x2)(st))(st)



c__case_250 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_250_case__34(x1)(x2)(st))(st)



c__case_251 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_251_case__33(x1)(x2)(st))(st)



c__case_252 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_252_case__32(x1)(x2)(st))(st)



c__case_253 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_253_case__31(x1)(x2)(x3)(st))(st)



c__case_254 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_254_case__30(x1)(x3)(x2)(st))(st)



c__case_255 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_255_case__29(x1)(x2)(st))(st)



c__case_257 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_257_case__28(x1)(x2)(st))(st)



c__case_256 x4 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_256_case__27(x1)(x4)(x6)(x7)(st))(st)



c__case_260 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_260_case__26(x1)(x3)(x2)(st))(st)



c__case_259 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_259_case__25(x1)(x3)(st))(st)



c__case_258 x5 x6 x7 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_258_case__24(x1)(x5)(x6)(x7)(x4)(st))(st)



c__case_266 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_266_case__23(x1)(x2)(x3)(x4)(x5)(st))(st)



c__case_265 x2 x3 x4 x6 x7 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_265_case__22(x1)(x2)(x3)(x4)(x6)(x7)(x8)(x9)(x10)(st))(st)



c__case_262 x2 x3 x4 x6 x7 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_262_case__21(x1)(x2)(x3)(x4)(x6)(x7)(x8)(x9)(x10)(st))(st)



c__case_261 x2 x3 x4 x6 x7 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_261_case__20(x1)(x2)(x3)(x4)(x6)(x7)(x8)(x9)(x10)(st))(st)



c__case_264 x2 x3 x6 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_264_case__19(x1)(x2)(x3)(x6)(x8)(x9)(x10)(st))(st)



c__case_263 x2 x3 x6 x8 x9 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_263_case__18(x1)(x2)(x3)(x6)(x8)(x9)(x11)(st))(st)



c__case_267 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_267_case__17(x1)(x2)(st))(st)



c__case_268 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_268_case__16(x1)(x2)(x3)(st))(st)



c__case_269 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_269_case__15(x1)(x2)(st))(st)



c__case_270 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_270_case__14(x1)(x2)(st))(st)



c__case_271 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_271_case__13(x1)(x2)(st))(st)



c__case_275 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_275_case__12(x1)(x2)(x3)(x4)(x5)(st))(st)



c__case_274 x2 x3 x4 x6 x7 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_274_case__11(x1)(x2)(x3)(x4)(x6)(x7)(x8)(x9)(x10)(st))(st)



c__case_273 x2 x3 x4 x6 x7 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_273_case__10(x1)(x2)(x3)(x4)(x6)(x7)(x8)(x9)(x10)(st))(st)



c__case_272 x2 x3 x4 x6 x7 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_272_case__9(x1)(x2)(x3)(x4)(x6)(x7)(x8)(x9)(x10)(st))(st)



c__case_276 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_276_case__8(x1)(x2)(x3)(st))(st)



c__case_280 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_280_case__7(x1)(x2)(x3)(x4)(x5)(st))(st)



c__case_279 x2 x3 x4 x7 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_279_case__6(x1)(x2)(x3)(x4)(x7)(x8)(x9)(x10)(st))(st)



c__case_278 x2 x3 x4 x7 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_278_case__5(x1)(x2)(x3)(x4)(x8)(x9)(x10)(st))(st)



c__case_277 x2 x3 x4 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_277_case__4(x1)(x2)(x3)(x4)(x9)(x10)(st))(st)



c__case_281 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_281_case__3(x1)(x2)(x3)(st))(st)



c__case_282 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_282_case__2(x1)(x2)(st))(st)



c__case_284 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_284_case__1(x1)(x2)(st))(st)



c__case_283 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_283_case__0(x1)(x6)(st))(st)



c__case_283_case__0 x1 x6@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_283_case__0 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_283_case__0 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_283_case__0(x1)(x)(st))(i)(xs)(st)
c__case_283_case__0 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_283_case__0")(x)



c__case_284_case__1 x1 x2@(Curry.Module.OracleRedBlackTree.C_RedBlackTree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_283(x6)(x1)(st))(st)
c__case_284_case__1 x1 (Curry.Module.OracleRedBlackTree.C_RedBlackTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_284_case__1(x1)(x)(st))(i)(xs)(st)
c__case_284_case__1 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_284_case__1")(x)



c__case_282_case__2 x1 x2@(Curry.Module.OracleRedBlackTree.C_RedBlackTree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.OracleRedBlackTree.C_RedBlackTree(x3)(x4)(x5)(Curry.Module.RedBlackTree.C_Empty))(st)
c__case_282_case__2 x1 (Curry.Module.OracleRedBlackTree.C_RedBlackTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_282_case__2(x1)(x)(st))(i)(xs)(st)
c__case_282_case__2 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_282_case__2")(x)



c__case_281_case__3 x1 x2 x3@(Curry.Module.OracleRedBlackTree.C_RedBlackTree x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c_lookupTree(x5)(x6)(x2)(x7)(x1)(st))(st)
c__case_281_case__3 x1 x2 (Curry.Module.OracleRedBlackTree.C_RedBlackTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_281_case__3(x1)(x2)(x)(st))(i)(xs)(st)
c__case_281_case__3 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_281_case__3")(x)



c__case_277_case__4 x1 x2 x3 x4 x9 x10@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c_lookupTree(x2)(x3)(x4)(x9)(x1)(st))(st)
c__case_277_case__4 x1 x2 x3 x4 x9 x10@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_277_case__4 x1 x2 x3 x4 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_277_case__4(x1)(x2)(x3)(x4)(x9)(x)(st))(i)(xs)(st)
c__case_277_case__4 x1 x2 x3 x4 x9 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_277_case__4")(x)



c__case_278_case__5 x1 x2 x3 x4 x8 x9 x10@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c_lookupTree(x2)(x3)(x4)(x8)(x1)(st))(st)
c__case_278_case__5 x1 x2 x3 x4 x8 x9 x10@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleRedBlackTree.c__case_277(x2)(x3)(x4)(x9)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x11)(st))(st)
c__case_278_case__5 x1 x2 x3 x4 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_278_case__5(x1)(x2)(x3)(x4)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_278_case__5 x1 x2 x3 x4 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_278_case__5")(x)



c__case_279_case__6 x1 x2 x3 x4 x7 x8 x9 x10@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Just(x7))(st)
c__case_279_case__6 x1 x2 x3 x4 x7 x8 x9 x10@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List)))(Curry.Module.OracleRedBlackTree.c__case_278(x2)(x3)(x4)(x7)(x8)(x9)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x3)(x4)(x1)(st))(x7)(x11)(st))(x12)(st))(st)
c__case_279_case__6 x1 x2 x3 x4 x7 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_279_case__6(x1)(x2)(x3)(x4)(x7)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_279_case__6 x1 x2 x3 x4 x7 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_279_case__6")(x)



c__case_280_case__7 x1 x2 x3 x4 x5@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_280_case__7 x1 x2 x3 x4 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))(Curry.Module.OracleRedBlackTree.c__case_279(x2)(x3)(x4)(x7)(x8)(x9)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x4)(x1)(st))(x7)(x10)(st))(x11)(st))(st)
c__case_280_case__7 x1 x2 x3 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_280_case__7(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_280_case__7 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_280_case__7")(x)



c__case_276_case__8 x1 x2 x3@(Curry.Module.OracleRedBlackTree.C_RedBlackTree x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.C_RedBlackTree(x4)(x5)(x6)(Curry.Module.OracleRedBlackTree.c_updateTree(x4)(x6)(x2)(x7)(x1)(st)))(st)
c__case_276_case__8 x1 x2 (Curry.Module.OracleRedBlackTree.C_RedBlackTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_276_case__8(x1)(x2)(x)(st))(i)(xs)(st)
c__case_276_case__8 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_276_case__8")(x)



c__case_272_case__9 x1 x2 x3 x4 x6 x7 x8 x9 x10@Curry.Module.Prelude.C_True st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleRedBlackTree.c_balanceR(Curry.Module.RedBlackTree.C_Tree(x6)(x7)(x8)(Curry.Module.OracleRedBlackTree.c_updateTree'46upd'4635(x2)(x3)(x4)(x9)(x1)(st)))(x11)(st))(st)
c__case_272_case__9 x1 x2 x3 x4 x6 x7 x8 x9 x10@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_272_case__9 x1 x2 x3 x4 x6 x7 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_272_case__9(x1)(x2)(x3)(x4)(x6)(x7)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_272_case__9 x1 x2 x3 x4 x6 x7 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_272_case__9")(x)



c__case_273_case__10 x1 x2 x3 x4 x6 x7 x8 x9 x10@Curry.Module.Prelude.C_True st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleRedBlackTree.c_balanceL(Curry.Module.RedBlackTree.C_Tree(x6)(x7)(Curry.Module.OracleRedBlackTree.c_updateTree'46upd'4635(x2)(x3)(x4)(x8)(x1)(st))(x9))(x11)(st))(st)
c__case_273_case__10 x1 x2 x3 x4 x6 x7 x8 x9 x10@Curry.Module.Prelude.C_False st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(Curry.Module.OracleRedBlackTree.c__case_272(x2)(x3)(x4)(x6)(x7)(x8)(x9)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x12)(st))(st)
c__case_273_case__10 x1 x2 x3 x4 x6 x7 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_273_case__10(x1)(x2)(x3)(x4)(x6)(x7)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_273_case__10 x1 x2 x3 x4 x6 x7 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_273_case__10")(x)



c__case_274_case__11 x1 x2 x3 x4 x6 x7 x8 x9 x10@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.RedBlackTree.C_Tree(x6)(x2)(x8)(x9))(st)
c__case_274_case__11 x1 x2 x3 x4 x6 x7 x8 x9 x10@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List)))(Curry.Module.OracleRedBlackTree.c__case_273(x2)(x3)(x4)(x6)(x7)(x8)(x9)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x4)(x2)(x1)(st))(x7)(x11)(st))(x12)(st))(st)
c__case_274_case__11 x1 x2 x3 x4 x6 x7 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_274_case__11(x1)(x2)(x3)(x4)(x6)(x7)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_274_case__11 x1 x2 x3 x4 x6 x7 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_274_case__11")(x)



c__case_275_case__12 x1 x2 x3 x4 x5@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Red)(x2)(Curry.Module.RedBlackTree.C_Empty)(Curry.Module.RedBlackTree.C_Empty))(st)
c__case_275_case__12 x1 x2 x3 x4 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))(Curry.Module.OracleRedBlackTree.c__case_274(x2)(x3)(x4)(x6)(x7)(x8)(x9)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x3)(x2)(x1)(st))(x7)(x10)(st))(x11)(st))(st)
c__case_275_case__12 x1 x2 x3 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_275_case__12(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_275_case__12 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_275_case__12")(x)



c__case_271_case__13 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_271_case__13 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_271_case__13(x1)(x)(st))(i)(xs)(st)
c__case_271_case__13 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_271_case__13")(x)



c__case_270_case__14 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_270_case__14 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_270_case__14(x1)(x)(st))(i)(xs)(st)
c__case_270_case__14 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_270_case__14")(x)



c__case_269_case__15 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_269_case__15 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_269_case__15(x1)(x)(st))(i)(xs)(st)
c__case_269_case__15 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_269_case__15")(x)



c__case_268_case__16 x1 x2 x3@(Curry.Module.OracleRedBlackTree.C_RedBlackTree x4 x5 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleRedBlackTree.C_RedBlackTree(x4)(x5)(x6)(Curry.Module.OracleRedBlackTree.c_delete'46blackenRoot'4644(Curry.Module.OracleRedBlackTree.c_deleteTree(x5)(x6)(x2)(x7)(x1)(st))(x8)(st)))(st)
c__case_268_case__16 x1 x2 (Curry.Module.OracleRedBlackTree.C_RedBlackTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_268_case__16(x1)(x2)(x)(st))(i)(xs)(st)
c__case_268_case__16 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_268_case__16")(x)



c__case_267_case__17 x1 x2@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.RedBlackTree.C_Empty)(st)
c__case_267_case__17 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(x4)(x5)(x6))(st)
c__case_267_case__17 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_267_case__17(x1)(x)(st))(i)(xs)(st)
c__case_267_case__17 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_267_case__17")(x)



c__case_263_case__18 x1 x2 x3 x6 x8 x9 x11@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c_deleteTree'46addColor'4656(x6)(x8)(x1)(st))(st)
c__case_263_case__18 x1 x2 x3 x6 x8 x9 x11@Curry.Module.Prelude.C_False st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(let {x10 = Curry.Module.OracleRedBlackTree.c_deleteTree'46rightMost'4656(x8)(x1)(st)} in let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(Curry.Module.OracleRedBlackTree.c_delBalanceL(Curry.Module.RedBlackTree.C_Tree(x6)(x10)(Curry.Module.OracleRedBlackTree.c_deleteTree(x2)(x3)(x10)(x8)(x12)(st))(x9))(x13)(st))(st))(st)
c__case_263_case__18 x1 x2 x3 x6 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_263_case__18(x1)(x2)(x3)(x6)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_263_case__18 x1 x2 x3 x6 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_263_case__18")(x)



c__case_264_case__19 x1 x2 x3 x6 x8 x9 x10@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c_deleteTree'46addColor'4656(x6)(x9)(x1)(st))(st)
c__case_264_case__19 x1 x2 x3 x6 x8 x9 x10@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleRedBlackTree.c__case_263(x2)(x3)(x6)(x8)(x9)(Curry.Module.OraclePrelude.op_61_61(x9)(Curry.Module.RedBlackTree.C_Empty)(x1)(st))(x11)(st))(st)
c__case_264_case__19 x1 x2 x3 x6 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_264_case__19(x1)(x2)(x3)(x6)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_264_case__19 x1 x2 x3 x6 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_264_case__19")(x)



c__case_261_case__20 x1 x2 x3 x4 x6 x7 x8 x9 x10@Curry.Module.Prelude.C_True st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleRedBlackTree.c_delBalanceR(Curry.Module.RedBlackTree.C_Tree(x6)(x7)(x8)(Curry.Module.OracleRedBlackTree.c_deleteTree(x2)(x3)(x4)(x9)(x1)(st)))(x11)(st))(st)
c__case_261_case__20 x1 x2 x3 x4 x6 x7 x8 x9 x10@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_261_case__20 x1 x2 x3 x4 x6 x7 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_261_case__20(x1)(x2)(x3)(x4)(x6)(x7)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_261_case__20 x1 x2 x3 x4 x6 x7 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_261_case__20")(x)



c__case_262_case__21 x1 x2 x3 x4 x6 x7 x8 x9 x10@Curry.Module.Prelude.C_True st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleRedBlackTree.c_delBalanceL(Curry.Module.RedBlackTree.C_Tree(x6)(x7)(Curry.Module.OracleRedBlackTree.c_deleteTree(x2)(x3)(x4)(x8)(x1)(st))(x9))(x11)(st))(st)
c__case_262_case__21 x1 x2 x3 x4 x6 x7 x8 x9 x10@Curry.Module.Prelude.C_False st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(Curry.Module.OracleRedBlackTree.c__case_261(x2)(x3)(x4)(x6)(x7)(x8)(x9)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x12)(st))(st)
c__case_262_case__21 x1 x2 x3 x4 x6 x7 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_262_case__21(x1)(x2)(x3)(x4)(x6)(x7)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_262_case__21 x1 x2 x3 x4 x6 x7 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_262_case__21")(x)



c__case_265_case__22 x1 x2 x3 x4 x6 x7 x8 x9 x10@Curry.Module.Prelude.C_True st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleRedBlackTree.c__case_264(x2)(x3)(x6)(x8)(x9)(Curry.Module.OraclePrelude.op_61_61(x8)(Curry.Module.RedBlackTree.C_Empty)(x1)(st))(x11)(st))(st)
c__case_265_case__22 x1 x2 x3 x4 x6 x7 x8 x9 x10@Curry.Module.Prelude.C_False st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List)))(Curry.Module.OracleRedBlackTree.c__case_262(x2)(x3)(x4)(x6)(x7)(x8)(x9)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x3)(x4)(x1)(st))(x7)(x12)(st))(x13)(st))(st)
c__case_265_case__22 x1 x2 x3 x4 x6 x7 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_265_case__22(x1)(x2)(x3)(x4)(x6)(x7)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_265_case__22 x1 x2 x3 x4 x6 x7 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_265_case__22")(x)



c__case_266_case__23 x1 x2 x3 x4 x5@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.RedBlackTree.C_Empty)(st)
c__case_266_case__23 x1 x2 x3 x4 x5@(Curry.Module.RedBlackTree.C_Tree x6 x7 x8 x9) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))(Curry.Module.OracleRedBlackTree.c__case_265(x2)(x3)(x4)(x6)(x7)(x8)(x9)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x4)(x1)(st))(x7)(x10)(st))(x11)(st))(st)
c__case_266_case__23 x1 x2 x3 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_266_case__23(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_266_case__23 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_266_case__23")(x)



c__case_258_case__24 x1 x5 x6 x7 x4@Curry.Module.RedBlackTree.C_Red st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(x5)(x6)(x7))(st)
c__case_258_case__24 x1 x5 x6 x7 x4@Curry.Module.RedBlackTree.C_Black st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_DoublyBlack)(x5)(x6)(x7))(st)
c__case_258_case__24 x1 x5 x6 x7 (Curry.Module.RedBlackTree.C_ColorOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_258_case__24(x1)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_258_case__24 x1 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_258_case__24")(x)



c__case_259_case__25 x1 x3@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.RedBlackTree.C_Empty)(st)
c__case_259_case__25 x1 x3@(Curry.Module.RedBlackTree.C_Tree x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_258(x5)(x6)(x7)(x4)(x1)(st))(st)
c__case_259_case__25 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_259_case__25(x1)(x)(st))(i)(xs)(st)
c__case_259_case__25 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_259_case__25")(x)



c__case_260_case__26 x1 x3 x2@Curry.Module.RedBlackTree.C_Red st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_260_case__26 x1 x3 x2@Curry.Module.RedBlackTree.C_Black st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_259(x3)(x1)(st))(st)
c__case_260_case__26 x1 x3 (Curry.Module.RedBlackTree.C_ColorOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_260_case__26(x1)(x3)(x)(st))(i)(xs)(st)
c__case_260_case__26 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_260_case__26")(x)



c__case_256_case__27 x1 x4 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_256_case__27 x1 x4 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c_deleteTree'46rightMost'4656(x6)(x1)(st))(st)
c__case_256_case__27 x1 x4 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_256_case__27(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_256_case__27 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_256_case__27")(x)



c__case_257_case__28 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleRedBlackTree.c__case_256(x4)(x6)(Curry.Module.OraclePrelude.op_61_61(x6)(Curry.Module.RedBlackTree.C_Empty)(x1)(st))(x7)(st))(st)
c__case_257_case__28 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_257_case__28(x1)(x)(st))(i)(xs)(st)
c__case_257_case__28 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_257_case__28")(x)



c__case_255_case__29 x1 x2@(Curry.Module.OracleRedBlackTree.C_RedBlackTree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c_tree2listTree(x6)(x1)(st))(st)
c__case_255_case__29 x1 (Curry.Module.OracleRedBlackTree.C_RedBlackTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_255_case__29(x1)(x)(st))(i)(xs)(st)
c__case_255_case__29 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_255_case__29")(x)



c__case_254_case__30 x1 x3 x2@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_254_case__30 x1 x3 x2@(Curry.Module.RedBlackTree.C_Tree x4 x5 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleRedBlackTree.c_tree2listTree'46t2l'4677(x6)((Curry.Module.Prelude.:<)(x5)(Curry.Module.OracleRedBlackTree.c_tree2listTree'46t2l'4677(x7)(x3)(x1)(st)))(x8)(st))(st)
c__case_254_case__30 x1 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_254_case__30(x1)(x3)(x)(st))(i)(xs)(st)
c__case_254_case__30 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_254_case__30")(x)



c__case_253_case__31 x1 x2 x3@(Curry.Module.OracleRedBlackTree.C_RedBlackTree x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.OracleRedBlackTree.C_RedBlackTree(x2)(x5)(x6)(x7))(st)
c__case_253_case__31 x1 x2 (Curry.Module.OracleRedBlackTree.C_RedBlackTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_253_case__31(x1)(x2)(x)(st))(i)(xs)(st)
c__case_253_case__31 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_253_case__31")(x)



c__case_252_case__32 x1 x2@(Curry.Module.OracleRedBlackTree.C_RedBlackTree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_252_case__32 x1 (Curry.Module.OracleRedBlackTree.C_RedBlackTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_252_case__32(x1)(x)(st))(i)(xs)(st)
c__case_252_case__32 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_252_case__32")(x)



c__case_251_case__33 x1 x2@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_251_case__33 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.RedBlackTree.C_Black)(x1)(st))(st)
c__case_251_case__33 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_251_case__33(x1)(x)(st))(i)(xs)(st)
c__case_251_case__33 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_251_case__33")(x)



c__case_250_case__34 x1 x2@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_250_case__34 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.RedBlackTree.C_Red)(x1)(st))(st)
c__case_250_case__34 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_250_case__34(x1)(x)(st))(i)(xs)(st)
c__case_250_case__34 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_250_case__34")(x)



c__case_249_case__35 x1 x2@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_249_case__35 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.RedBlackTree.C_DoublyBlack)(x1)(st))(st)
c__case_249_case__35 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_249_case__35(x1)(x)(st))(i)(xs)(st)
c__case_249_case__35 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_249_case__35")(x)



c__case_248_case__36 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_248_case__36 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_248_case__36(x1)(x)(st))(i)(xs)(st)
c__case_248_case__36 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_248_case__36")(x)



c__case_247_case__37 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_247_case__37 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_247_case__37(x1)(x)(st))(i)(xs)(st)
c__case_247_case__37 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_247_case__37")(x)



c__case_246_case__38 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_246_case__38 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_246_case__38(x1)(x)(st))(i)(xs)(st)
c__case_246_case__38 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_246_case__38")(x)



c__case_244_case__39 x1 x4 x5 x6 x3@Curry.Module.RedBlackTree.C_DoublyBlack st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(x4)(x5)(x6))(st)
c__case_244_case__39 x1 x4 x5 x6 (Curry.Module.RedBlackTree.C_ColorOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_244_case__39(x1)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_244_case__39 x1 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_244_case__39")(x)



c__case_245_case__40 x1 x2@Curry.Module.RedBlackTree.C_Empty st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.RedBlackTree.C_Empty)(st)
c__case_245_case__40 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_244(x4)(x5)(x6)(x3)(x1)(st))(st)
c__case_245_case__40 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_245_case__40(x1)(x)(st))(i)(xs)(st)
c__case_245_case__40 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_245_case__40")(x)



c__case_241_case__41 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_241_case__41 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_241_case__41 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_241_case__41(x1)(x2)(x)(st))(i)(xs)(st)
c__case_241_case__41 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_241_case__41")(x)



c__case_242_case__42 x1 x2 x20@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))(let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x21)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))(let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x22)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List))(let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x23)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List))(let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x24)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))(let {x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x25)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List))(let {x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x27)(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Red)(Curry.Module.OracleRedBlackTree.c_balanceL'46_'35selFP18'35y(x2)(x23)(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.OracleRedBlackTree.c_balanceL'46_'35selFP16'35x(x2)(x21)(st))(Curry.Module.OracleRedBlackTree.c_balanceL'46_'35selFP17'35a(x2)(x22)(st))(Curry.Module.OracleRedBlackTree.c_balanceL'46_'35selFP19'35b(x2)(x24)(st)))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.OracleRedBlackTree.c_balanceL'46_'35selFP15'35z(x2)(x1)(st))(Curry.Module.OracleRedBlackTree.c_balanceL'46_'35selFP20'35c(x2)(x25)(st))(Curry.Module.OracleRedBlackTree.c_balanceL'46_'35selFP21'35d(x2)(x26)(st))))(st))(st))(st))(st))(st))(st))(st))(st))(st)
c__case_242_case__42 x1 x2 x20@Curry.Module.Prelude.C_False st = let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x28)(Curry.Module.Prelude.List))(Curry.Module.OracleRedBlackTree.c__case_241(x2)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x28)(st))(st)
c__case_242_case__42 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_242_case__42(x1)(x2)(x)(st))(i)(xs)(st)
c__case_242_case__42 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_242_case__42")(x)



c__case_243_case__43 x1 x2 x3 x12@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))(let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))(let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x19)(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Red)(Curry.Module.OracleRedBlackTree.c_balanceL'46_'35selFP8'35y(x2)(x13)(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.OracleRedBlackTree.c_balanceL'46_'35selFP9'35x(x2)(x14)(st))(Curry.Module.OracleRedBlackTree.c_balanceL'46_'35selFP10'35a(x2)(x15)(st))(Curry.Module.OracleRedBlackTree.c_balanceL'46_'35selFP11'35b(x2)(x16)(st)))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.OracleRedBlackTree.c_balanceL'46_'35selFP7'35z(x2)(x1)(st))(Curry.Module.OracleRedBlackTree.c_balanceL'46_'35selFP12'35c(x2)(x17)(st))(Curry.Module.OracleRedBlackTree.c_balanceL'46_'35selFP13'35d(x2)(x18)(st))))(st))(st))(st))(st))(st))(st))(st))(st))(st)
c__case_243_case__43 x1 x2 x3 x12@Curry.Module.Prelude.C_False st = let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List)))))(Curry.Module.OracleRedBlackTree.c__case_242(x2)(x3)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OracleRedBlackTree.c_isRed(x3)(x1)(st))(Curry.Module.OracleRedBlackTree.c_isRed(Curry.Module.OracleRedBlackTree.c_right(x3)(x20)(st))(x21)(st))(x22)(st))(x23)(st))(st)
c__case_243_case__43 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_243_case__43(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_243_case__43 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_243_case__43")(x)



c__case_238_case__44 x1 x4 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_238_case__44 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_238_case__44(x1)(x4)(x)(st))(i)(xs)(st)
c__case_238_case__44 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_238_case__44")(x)



c__case_239_case__45 x1 x4 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_238(x4)(x9)(x1)(st))(st)
c__case_239_case__45 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_239_case__45(x1)(x4)(x)(st))(i)(xs)(st)
c__case_239_case__45 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_239_case__45")(x)



c__case_240_case__46 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_239(x4)(x5)(x1)(st))(st)
c__case_240_case__46 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_240_case__46(x1)(x)(st))(i)(xs)(st)
c__case_240_case__46 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_240_case__46")(x)



c__case_235_case__47 x1 x8 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_235_case__47 x1 x8 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_235_case__47(x1)(x8)(x)(st))(i)(xs)(st)
c__case_235_case__47 x1 x8 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_235_case__47")(x)



c__case_236_case__48 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_235(x8)(x9)(x1)(st))(st)
c__case_236_case__48 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_236_case__48(x1)(x)(st))(i)(xs)(st)
c__case_236_case__48 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_236_case__48")(x)



c__case_237_case__49 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_236(x5)(x1)(st))(st)
c__case_237_case__49 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_237_case__49(x1)(x)(st))(i)(xs)(st)
c__case_237_case__49 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_237_case__49")(x)



c__case_232_case__50 x1 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x12)(st)
c__case_232_case__50 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_232_case__50(x1)(x)(st))(i)(xs)(st)
c__case_232_case__50 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_232_case__50")(x)



c__case_233_case__51 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_232(x9)(x1)(st))(st)
c__case_233_case__51 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_233_case__51(x1)(x)(st))(i)(xs)(st)
c__case_233_case__51 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_233_case__51")(x)



c__case_234_case__52 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_233(x5)(x1)(st))(st)
c__case_234_case__52 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_234_case__52(x1)(x)(st))(i)(xs)(st)
c__case_234_case__52 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_234_case__52")(x)



c__case_229_case__53 x1 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x13)(st)
c__case_229_case__53 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_229_case__53(x1)(x)(st))(i)(xs)(st)
c__case_229_case__53 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_229_case__53")(x)



c__case_230_case__54 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_229(x9)(x1)(st))(st)
c__case_230_case__54 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_230_case__54(x1)(x)(st))(i)(xs)(st)
c__case_230_case__54 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_230_case__54")(x)



c__case_231_case__55 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_230(x5)(x1)(st))(st)
c__case_231_case__55 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_231_case__55(x1)(x)(st))(i)(xs)(st)
c__case_231_case__55 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_231_case__55")(x)



c__case_226_case__56 x1 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x14)(st)
c__case_226_case__56 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_226_case__56(x1)(x)(st))(i)(xs)(st)
c__case_226_case__56 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_226_case__56")(x)



c__case_227_case__57 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_226(x9)(x1)(st))(st)
c__case_227_case__57 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_227_case__57(x1)(x)(st))(i)(xs)(st)
c__case_227_case__57 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_227_case__57")(x)



c__case_228_case__58 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_227(x5)(x1)(st))(st)
c__case_228_case__58 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_228_case__58(x1)(x)(st))(i)(xs)(st)
c__case_228_case__58 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_228_case__58")(x)



c__case_223_case__59 x1 x10 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x10)(st)
c__case_223_case__59 x1 x10 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_223_case__59(x1)(x10)(x)(st))(i)(xs)(st)
c__case_223_case__59 x1 x10 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_223_case__59")(x)



c__case_224_case__60 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_223(x10)(x9)(x1)(st))(st)
c__case_224_case__60 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_224_case__60(x1)(x)(st))(i)(xs)(st)
c__case_224_case__60 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_224_case__60")(x)



c__case_225_case__61 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_224(x5)(x1)(st))(st)
c__case_225_case__61 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_225_case__61(x1)(x)(st))(i)(xs)(st)
c__case_225_case__61 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_225_case__61")(x)



c__case_220_case__62 x1 x6 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_220_case__62 x1 x6 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_220_case__62(x1)(x6)(x)(st))(i)(xs)(st)
c__case_220_case__62 x1 x6 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_220_case__62")(x)



c__case_221_case__63 x1 x6 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_220(x6)(x9)(x1)(st))(st)
c__case_221_case__63 x1 x6 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_221_case__63(x1)(x6)(x)(st))(i)(xs)(st)
c__case_221_case__63 x1 x6 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_221_case__63")(x)



c__case_222_case__64 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_221(x6)(x5)(x1)(st))(st)
c__case_222_case__64 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_222_case__64(x1)(x)(st))(i)(xs)(st)
c__case_222_case__64 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_222_case__64")(x)



c__case_217_case__65 x1 x4 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_217_case__65 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_217_case__65(x1)(x4)(x)(st))(i)(xs)(st)
c__case_217_case__65 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_217_case__65")(x)



c__case_218_case__66 x1 x4 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_217(x4)(x10)(x1)(st))(st)
c__case_218_case__66 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_218_case__66(x1)(x4)(x)(st))(i)(xs)(st)
c__case_218_case__66 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_218_case__66")(x)



c__case_219_case__67 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_218(x4)(x5)(x1)(st))(st)
c__case_219_case__67 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_219_case__67(x1)(x)(st))(i)(xs)(st)
c__case_219_case__67 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_219_case__67")(x)



c__case_214_case__68 x1 x8 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_214_case__68 x1 x8 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_214_case__68(x1)(x8)(x)(st))(i)(xs)(st)
c__case_214_case__68 x1 x8 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_214_case__68")(x)



c__case_215_case__69 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_214(x8)(x10)(x1)(st))(st)
c__case_215_case__69 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_215_case__69(x1)(x)(st))(i)(xs)(st)
c__case_215_case__69 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_215_case__69")(x)



c__case_216_case__70 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_215(x5)(x1)(st))(st)
c__case_216_case__70 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_216_case__70(x1)(x)(st))(i)(xs)(st)
c__case_216_case__70 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_216_case__70")(x)



c__case_211_case__71 x1 x9 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x9)(st)
c__case_211_case__71 x1 x9 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_211_case__71(x1)(x9)(x)(st))(i)(xs)(st)
c__case_211_case__71 x1 x9 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_211_case__71")(x)



c__case_212_case__72 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_211(x9)(x10)(x1)(st))(st)
c__case_212_case__72 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_212_case__72(x1)(x)(st))(i)(xs)(st)
c__case_212_case__72 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_212_case__72")(x)



c__case_213_case__73 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_212(x5)(x1)(st))(st)
c__case_213_case__73 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_213_case__73(x1)(x)(st))(i)(xs)(st)
c__case_213_case__73 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_213_case__73")(x)



c__case_208_case__74 x1 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x12)(st)
c__case_208_case__74 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_208_case__74(x1)(x)(st))(i)(xs)(st)
c__case_208_case__74 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_208_case__74")(x)



c__case_209_case__75 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_208(x10)(x1)(st))(st)
c__case_209_case__75 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_209_case__75(x1)(x)(st))(i)(xs)(st)
c__case_209_case__75 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_209_case__75")(x)



c__case_210_case__76 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_209(x5)(x1)(st))(st)
c__case_210_case__76 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_210_case__76(x1)(x)(st))(i)(xs)(st)
c__case_210_case__76 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_210_case__76")(x)



c__case_205_case__77 x1 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x13)(st)
c__case_205_case__77 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_205_case__77(x1)(x)(st))(i)(xs)(st)
c__case_205_case__77 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_205_case__77")(x)



c__case_206_case__78 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_205(x10)(x1)(st))(st)
c__case_206_case__78 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_206_case__78(x1)(x)(st))(i)(xs)(st)
c__case_206_case__78 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_206_case__78")(x)



c__case_207_case__79 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_206(x5)(x1)(st))(st)
c__case_207_case__79 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_207_case__79(x1)(x)(st))(i)(xs)(st)
c__case_207_case__79 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_207_case__79")(x)



c__case_202_case__80 x1 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x14)(st)
c__case_202_case__80 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_202_case__80(x1)(x)(st))(i)(xs)(st)
c__case_202_case__80 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_202_case__80")(x)



c__case_203_case__81 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_202(x10)(x1)(st))(st)
c__case_203_case__81 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_203_case__81(x1)(x)(st))(i)(xs)(st)
c__case_203_case__81 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_203_case__81")(x)



c__case_204_case__82 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_203(x5)(x1)(st))(st)
c__case_204_case__82 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_204_case__82(x1)(x)(st))(i)(xs)(st)
c__case_204_case__82 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_204_case__82")(x)



c__case_199_case__83 x1 x6 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_199_case__83 x1 x6 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_199_case__83(x1)(x6)(x)(st))(i)(xs)(st)
c__case_199_case__83 x1 x6 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_199_case__83")(x)



c__case_200_case__84 x1 x6 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_199(x6)(x10)(x1)(st))(st)
c__case_200_case__84 x1 x6 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_200_case__84(x1)(x6)(x)(st))(i)(xs)(st)
c__case_200_case__84 x1 x6 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_200_case__84")(x)



c__case_201_case__85 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_200(x6)(x5)(x1)(st))(st)
c__case_201_case__85 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_201_case__85(x1)(x)(st))(i)(xs)(st)
c__case_201_case__85 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_201_case__85")(x)



c__case_196_case__86 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_196_case__86 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_196_case__86 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_196_case__86(x1)(x2)(x)(st))(i)(xs)(st)
c__case_196_case__86 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_196_case__86")(x)



c__case_197_case__87 x1 x2 x20@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))(let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x21)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))(let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x22)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List))(let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x23)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List))(let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x24)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))(let {x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x25)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List))(let {x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x27)(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Red)(Curry.Module.OracleRedBlackTree.c_balanceR'46_'35selFP34'35y(x2)(x23)(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.OracleRedBlackTree.c_balanceR'46_'35selFP31'35x(x2)(x1)(st))(Curry.Module.OracleRedBlackTree.c_balanceR'46_'35selFP32'35a(x2)(x21)(st))(Curry.Module.OracleRedBlackTree.c_balanceR'46_'35selFP35'35b(x2)(x24)(st)))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.OracleRedBlackTree.c_balanceR'46_'35selFP33'35z(x2)(x22)(st))(Curry.Module.OracleRedBlackTree.c_balanceR'46_'35selFP36'35c(x2)(x25)(st))(Curry.Module.OracleRedBlackTree.c_balanceR'46_'35selFP37'35d(x2)(x26)(st))))(st))(st))(st))(st))(st))(st))(st))(st))(st)
c__case_197_case__87 x1 x2 x20@Curry.Module.Prelude.C_False st = let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x28)(Curry.Module.Prelude.List))(Curry.Module.OracleRedBlackTree.c__case_196(x2)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x28)(st))(st)
c__case_197_case__87 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_197_case__87(x1)(x2)(x)(st))(i)(xs)(st)
c__case_197_case__87 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_197_case__87")(x)



c__case_198_case__88 x1 x2 x3 x12@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))(let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))(let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x19)(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Red)(Curry.Module.OracleRedBlackTree.c_balanceR'46_'35selFP25'35y(x2)(x14)(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.OracleRedBlackTree.c_balanceR'46_'35selFP23'35x(x2)(x1)(st))(Curry.Module.OracleRedBlackTree.c_balanceR'46_'35selFP24'35a(x2)(x13)(st))(Curry.Module.OracleRedBlackTree.c_balanceR'46_'35selFP26'35b(x2)(x15)(st)))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.OracleRedBlackTree.c_balanceR'46_'35selFP27'35z(x2)(x16)(st))(Curry.Module.OracleRedBlackTree.c_balanceR'46_'35selFP28'35c(x2)(x17)(st))(Curry.Module.OracleRedBlackTree.c_balanceR'46_'35selFP29'35d(x2)(x18)(st))))(st))(st))(st))(st))(st))(st))(st))(st))(st)
c__case_198_case__88 x1 x2 x3 x12@Curry.Module.Prelude.C_False st = let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List)))))(Curry.Module.OracleRedBlackTree.c__case_197(x2)(x3)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OracleRedBlackTree.c_isRed(x3)(x1)(st))(Curry.Module.OracleRedBlackTree.c_isRed(Curry.Module.OracleRedBlackTree.c_left(x3)(x20)(st))(x21)(st))(x22)(st))(x23)(st))(st)
c__case_198_case__88 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_198_case__88(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_198_case__88 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_198_case__88")(x)



c__case_193_case__89 x1 x4 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_193_case__89 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_193_case__89(x1)(x4)(x)(st))(i)(xs)(st)
c__case_193_case__89 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_193_case__89")(x)



c__case_194_case__90 x1 x4 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_193(x4)(x10)(x1)(st))(st)
c__case_194_case__90 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_194_case__90(x1)(x4)(x)(st))(i)(xs)(st)
c__case_194_case__90 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_194_case__90")(x)



c__case_195_case__91 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_194(x4)(x6)(x1)(st))(st)
c__case_195_case__91 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_195_case__91(x1)(x)(st))(i)(xs)(st)
c__case_195_case__91 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_195_case__91")(x)



c__case_190_case__92 x1 x5 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_190_case__92 x1 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_190_case__92(x1)(x5)(x)(st))(i)(xs)(st)
c__case_190_case__92 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_190_case__92")(x)



c__case_191_case__93 x1 x5 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_190(x5)(x10)(x1)(st))(st)
c__case_191_case__93 x1 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_191_case__93(x1)(x5)(x)(st))(i)(xs)(st)
c__case_191_case__93 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_191_case__93")(x)



c__case_192_case__94 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_191(x5)(x6)(x1)(st))(st)
c__case_192_case__94 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_192_case__94(x1)(x)(st))(i)(xs)(st)
c__case_192_case__94 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_192_case__94")(x)



c__case_187_case__95 x1 x8 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_187_case__95 x1 x8 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_187_case__95(x1)(x8)(x)(st))(i)(xs)(st)
c__case_187_case__95 x1 x8 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_187_case__95")(x)



c__case_188_case__96 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_187(x8)(x10)(x1)(st))(st)
c__case_188_case__96 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_188_case__96(x1)(x)(st))(i)(xs)(st)
c__case_188_case__96 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_188_case__96")(x)



c__case_189_case__97 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_188(x6)(x1)(st))(st)
c__case_189_case__97 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_189_case__97(x1)(x)(st))(i)(xs)(st)
c__case_189_case__97 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_189_case__97")(x)



c__case_184_case__98 x1 x9 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x9)(st)
c__case_184_case__98 x1 x9 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_184_case__98(x1)(x9)(x)(st))(i)(xs)(st)
c__case_184_case__98 x1 x9 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_184_case__98")(x)



c__case_185_case__99 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_184(x9)(x10)(x1)(st))(st)
c__case_185_case__99 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_185_case__99(x1)(x)(st))(i)(xs)(st)
c__case_185_case__99 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_185_case__99")(x)



c__case_186_case__100 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_185(x6)(x1)(st))(st)
c__case_186_case__100 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_186_case__100(x1)(x)(st))(i)(xs)(st)
c__case_186_case__100 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_186_case__100")(x)



c__case_181_case__101 x1 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x12)(st)
c__case_181_case__101 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_181_case__101(x1)(x)(st))(i)(xs)(st)
c__case_181_case__101 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_181_case__101")(x)



c__case_182_case__102 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_181(x10)(x1)(st))(st)
c__case_182_case__102 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_182_case__102(x1)(x)(st))(i)(xs)(st)
c__case_182_case__102 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_182_case__102")(x)



c__case_183_case__103 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_182(x6)(x1)(st))(st)
c__case_183_case__103 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_183_case__103(x1)(x)(st))(i)(xs)(st)
c__case_183_case__103 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_183_case__103")(x)



c__case_178_case__104 x1 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x13)(st)
c__case_178_case__104 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_178_case__104(x1)(x)(st))(i)(xs)(st)
c__case_178_case__104 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_178_case__104")(x)



c__case_179_case__105 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_178(x10)(x1)(st))(st)
c__case_179_case__105 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_179_case__105(x1)(x)(st))(i)(xs)(st)
c__case_179_case__105 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_179_case__105")(x)



c__case_180_case__106 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_179(x6)(x1)(st))(st)
c__case_180_case__106 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_180_case__106(x1)(x)(st))(i)(xs)(st)
c__case_180_case__106 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_180_case__106")(x)



c__case_175_case__107 x1 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x14)(st)
c__case_175_case__107 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_175_case__107(x1)(x)(st))(i)(xs)(st)
c__case_175_case__107 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_175_case__107")(x)



c__case_176_case__108 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_175(x10)(x1)(st))(st)
c__case_176_case__108 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_176_case__108(x1)(x)(st))(i)(xs)(st)
c__case_176_case__108 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_176_case__108")(x)



c__case_177_case__109 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_176(x6)(x1)(st))(st)
c__case_177_case__109 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_177_case__109(x1)(x)(st))(i)(xs)(st)
c__case_177_case__109 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_177_case__109")(x)



c__case_172_case__110 x1 x4 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_172_case__110 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_172_case__110(x1)(x4)(x)(st))(i)(xs)(st)
c__case_172_case__110 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_172_case__110")(x)



c__case_173_case__111 x1 x4 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_172(x4)(x9)(x1)(st))(st)
c__case_173_case__111 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_173_case__111(x1)(x4)(x)(st))(i)(xs)(st)
c__case_173_case__111 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_173_case__111")(x)



c__case_174_case__112 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_173(x4)(x6)(x1)(st))(st)
c__case_174_case__112 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_174_case__112(x1)(x)(st))(i)(xs)(st)
c__case_174_case__112 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_174_case__112")(x)



c__case_169_case__113 x1 x5 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_169_case__113 x1 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_169_case__113(x1)(x5)(x)(st))(i)(xs)(st)
c__case_169_case__113 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_169_case__113")(x)



c__case_170_case__114 x1 x5 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_169(x5)(x9)(x1)(st))(st)
c__case_170_case__114 x1 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_170_case__114(x1)(x5)(x)(st))(i)(xs)(st)
c__case_170_case__114 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_170_case__114")(x)



c__case_171_case__115 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_170(x5)(x6)(x1)(st))(st)
c__case_171_case__115 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_171_case__115(x1)(x)(st))(i)(xs)(st)
c__case_171_case__115 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_171_case__115")(x)



c__case_166_case__116 x1 x8 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_166_case__116 x1 x8 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_166_case__116(x1)(x8)(x)(st))(i)(xs)(st)
c__case_166_case__116 x1 x8 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_166_case__116")(x)



c__case_167_case__117 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_166(x8)(x9)(x1)(st))(st)
c__case_167_case__117 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_167_case__117(x1)(x)(st))(i)(xs)(st)
c__case_167_case__117 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_167_case__117")(x)



c__case_168_case__118 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_167(x6)(x1)(st))(st)
c__case_168_case__118 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_168_case__118(x1)(x)(st))(i)(xs)(st)
c__case_168_case__118 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_168_case__118")(x)



c__case_163_case__119 x1 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x12)(st)
c__case_163_case__119 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_163_case__119(x1)(x)(st))(i)(xs)(st)
c__case_163_case__119 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_163_case__119")(x)



c__case_164_case__120 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_163(x9)(x1)(st))(st)
c__case_164_case__120 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_164_case__120(x1)(x)(st))(i)(xs)(st)
c__case_164_case__120 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_164_case__120")(x)



c__case_165_case__121 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_164(x6)(x1)(st))(st)
c__case_165_case__121 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_165_case__121(x1)(x)(st))(i)(xs)(st)
c__case_165_case__121 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_165_case__121")(x)



c__case_160_case__122 x1 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x13)(st)
c__case_160_case__122 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_160_case__122(x1)(x)(st))(i)(xs)(st)
c__case_160_case__122 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_160_case__122")(x)



c__case_161_case__123 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_160(x9)(x1)(st))(st)
c__case_161_case__123 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_161_case__123(x1)(x)(st))(i)(xs)(st)
c__case_161_case__123 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_161_case__123")(x)



c__case_162_case__124 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_161(x6)(x1)(st))(st)
c__case_162_case__124 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_162_case__124(x1)(x)(st))(i)(xs)(st)
c__case_162_case__124 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_162_case__124")(x)



c__case_157_case__125 x1 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x14)(st)
c__case_157_case__125 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_157_case__125(x1)(x)(st))(i)(xs)(st)
c__case_157_case__125 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_157_case__125")(x)



c__case_158_case__126 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_157(x9)(x1)(st))(st)
c__case_158_case__126 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_158_case__126(x1)(x)(st))(i)(xs)(st)
c__case_158_case__126 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_158_case__126")(x)



c__case_159_case__127 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_158(x6)(x1)(st))(st)
c__case_159_case__127 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_159_case__127(x1)(x)(st))(i)(xs)(st)
c__case_159_case__127 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_159_case__127")(x)



c__case_154_case__128 x1 x10 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x10)(st)
c__case_154_case__128 x1 x10 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_154_case__128(x1)(x10)(x)(st))(i)(xs)(st)
c__case_154_case__128 x1 x10 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_154_case__128")(x)



c__case_155_case__129 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_154(x10)(x9)(x1)(st))(st)
c__case_155_case__129 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_155_case__129(x1)(x)(st))(i)(xs)(st)
c__case_155_case__129 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_155_case__129")(x)



c__case_156_case__130 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_155(x6)(x1)(st))(st)
c__case_156_case__130 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_156_case__130(x1)(x)(st))(i)(xs)(st)
c__case_156_case__130 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_156_case__130")(x)



c__case_153_case__131 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c_reviseLeft(x2)(x1)(st))(st)
c__case_153_case__131 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_153_case__131 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_153_case__131(x1)(x2)(x)(st))(i)(xs)(st)
c__case_153_case__131 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_153_case__131")(x)



c__case_148_case__132 x1 x25@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.RedBlackTree.C_Black)(st)
c__case_148_case__132 x1 x25@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.RedBlackTree.C_DoublyBlack)(st)
c__case_148_case__132 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_148_case__132(x1)(x)(st))(i)(xs)(st)
c__case_148_case__132 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_148_case__132")(x)



c__case_147_case__133 x1 x2 x36@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(let {x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x37)(Curry.Module.Prelude.List))(let {x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x37)((Curry.Module.Prelude.:<)(x38)(Curry.Module.Prelude.List))(let {x39 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x38)((Curry.Module.Prelude.:<)(x39)(Curry.Module.Prelude.List))(let {x40 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x39)((Curry.Module.Prelude.:<)(x40)(Curry.Module.Prelude.List))(let {x41 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x40)((Curry.Module.Prelude.:<)(x41)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x41)(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP66'35y(x2)(x38)(st))(Curry.Module.OracleRedBlackTree.c_reviseLeft(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Red)(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP64'35x(x2)(x1)(st))(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP65'35a(x2)(x37)(st))(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP67'35b(x2)(x39)(st)))(x41)(st))(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP68'35c(x2)(x40)(st)))(st))(st))(st))(st))(st))(st))(st)
c__case_147_case__133 x1 x2 x36@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_147_case__133 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_147_case__133(x1)(x2)(x)(st))(i)(xs)(st)
c__case_147_case__133 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_147_case__133")(x)



c__case_149_case__134 x1 x2 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(let {x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x30)(Curry.Module.Prelude.List))(let {x24 = Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP57'35col(x2)(x1)(st)} in let {x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x30)((Curry.Module.Prelude.:<)(x31)(Curry.Module.Prelude.List))(let {x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x31)((Curry.Module.Prelude.:<)(x32)(Curry.Module.Prelude.List))(let {x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x32)((Curry.Module.Prelude.:<)(x33)(Curry.Module.Prelude.List))(let {x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x33)((Curry.Module.Prelude.:<)(x34)(Curry.Module.Prelude.List))(let {x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x34)((Curry.Module.Prelude.:<)(x35)(Curry.Module.Prelude.List))(let {x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x35)((Curry.Module.Prelude.:<)(x36)((Curry.Module.Prelude.:<)(x37)(Curry.Module.Prelude.List)))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.OracleRedBlackTree.c__case_148(x24)(Curry.Module.OraclePrelude.op_61_61(x24)(Curry.Module.RedBlackTree.C_Red)(x35)(st))(x36)(st))(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP58'35x(x2)(x30)(st))(Curry.Module.OracleRedBlackTree.c_singleBlack(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP59'35a(x2)(x31)(st))(x37)(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Red)(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP60'35y(x2)(x32)(st))(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP61'35b(x2)(x33)(st))(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP62'35c(x2)(x34)(st))))(st))(st))(st))(st))(st))(st))(st))(st)
c__case_149_case__134 x1 x2 x4@Curry.Module.Prelude.C_False st = let {x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x38)(Curry.Module.Prelude.List))(Curry.Module.OracleRedBlackTree.c__case_147(x2)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x38)(st))(st)
c__case_149_case__134 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_149_case__134(x1)(x2)(x)(st))(i)(xs)(st)
c__case_149_case__134 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_149_case__134")(x)



c__case_150_case__135 x1 x2 x4 x23@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List))(let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x24)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))(let {x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x25)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List))(let {x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List))(let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x27)((Curry.Module.Prelude.:<)(x28)(Curry.Module.Prelude.List))(let {x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x28)((Curry.Module.Prelude.:<)(x29)(Curry.Module.Prelude.List))(let {x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x29)((Curry.Module.Prelude.:<)(x30)(Curry.Module.Prelude.List))(let {x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x30)((Curry.Module.Prelude.:<)(x31)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x31)(Curry.Module.RedBlackTree.C_Tree(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP48'35col(x2)(x1)(st))(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP51'35y(x2)(x26)(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP49'35x(x2)(x24)(st))(Curry.Module.OracleRedBlackTree.c_singleBlack(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP50'35a(x2)(x25)(st))(x31)(st))(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP52'35b(x2)(x27)(st)))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP53'35z(x2)(x28)(st))(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP54'35c(x2)(x29)(st))(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP55'35d(x2)(x30)(st))))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)
c__case_150_case__135 x1 x2 x4 x23@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_149(x2)(x4)(x1)(st))(st)
c__case_150_case__135 x1 x2 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_150_case__135(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_150_case__135 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_150_case__135")(x)



c__case_151_case__136 x1 x2 x3 x4 x14@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))(let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))(let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))(let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))(let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))(let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x21)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x22)(Curry.Module.RedBlackTree.C_Tree(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP39'35col(x2)(x1)(st))(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP43'35y(x2)(x18)(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP40'35x(x2)(x15)(st))(Curry.Module.OracleRedBlackTree.c_singleBlack(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP41'35a(x2)(x16)(st))(x22)(st))(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP44'35b(x2)(x19)(st)))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP42'35z(x2)(x17)(st))(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP45'35c(x2)(x20)(st))(Curry.Module.OracleRedBlackTree.c_reviseLeft'46_'35selFP46'35d(x2)(x21)(st))))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)
c__case_151_case__136 x1 x2 x3 x4 x14@Curry.Module.Prelude.C_False st = let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))))(Curry.Module.OracleRedBlackTree.c__case_150(x2)(x3)(x4)(Curry.Module.OraclePrelude.op_38_38(x4)(Curry.Module.OracleRedBlackTree.c_isRed(Curry.Module.OracleRedBlackTree.c_right(x3)(x1)(st))(x23)(st))(x24)(st))(x25)(st))(st)
c__case_151_case__136 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_151_case__136(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_151_case__136 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_151_case__136")(x)



c__case_152_case__137 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_152_case__137 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))))(Curry.Module.OracleRedBlackTree.c__case_151(x2)(x3)(x4)(Curry.Module.OraclePrelude.op_38_38(x4)(Curry.Module.OracleRedBlackTree.c_isRed(Curry.Module.OracleRedBlackTree.c_left(x3)(x1)(st))(x6)(st))(x7)(st))(x8)(st))(st)
c__case_152_case__137 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_152_case__137(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_152_case__137 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_152_case__137")(x)



c__case_144_case__138 x1 x3 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_144_case__138 x1 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_144_case__138(x1)(x3)(x)(st))(i)(xs)(st)
c__case_144_case__138 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_144_case__138")(x)



c__case_145_case__139 x1 x3 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_144(x3)(x9)(x1)(st))(st)
c__case_145_case__139 x1 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_145_case__139(x1)(x3)(x)(st))(i)(xs)(st)
c__case_145_case__139 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_145_case__139")(x)



c__case_146_case__140 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_145(x3)(x6)(x1)(st))(st)
c__case_146_case__140 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_146_case__140(x1)(x)(st))(i)(xs)(st)
c__case_146_case__140 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_146_case__140")(x)



c__case_141_case__141 x1 x4 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_141_case__141 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_141_case__141(x1)(x4)(x)(st))(i)(xs)(st)
c__case_141_case__141 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_141_case__141")(x)



c__case_142_case__142 x1 x4 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_141(x4)(x9)(x1)(st))(st)
c__case_142_case__142 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_142_case__142(x1)(x4)(x)(st))(i)(xs)(st)
c__case_142_case__142 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_142_case__142")(x)



c__case_143_case__143 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_142(x4)(x6)(x1)(st))(st)
c__case_143_case__143 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_143_case__143(x1)(x)(st))(i)(xs)(st)
c__case_143_case__143 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_143_case__143")(x)



c__case_138_case__144 x1 x5 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_138_case__144 x1 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_138_case__144(x1)(x5)(x)(st))(i)(xs)(st)
c__case_138_case__144 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_138_case__144")(x)



c__case_139_case__145 x1 x5 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_138(x5)(x9)(x1)(st))(st)
c__case_139_case__145 x1 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_139_case__145(x1)(x5)(x)(st))(i)(xs)(st)
c__case_139_case__145 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_139_case__145")(x)



c__case_140_case__146 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_139(x5)(x6)(x1)(st))(st)
c__case_140_case__146 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_140_case__146(x1)(x)(st))(i)(xs)(st)
c__case_140_case__146 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_140_case__146")(x)



c__case_135_case__147 x1 x8 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_135_case__147 x1 x8 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_135_case__147(x1)(x8)(x)(st))(i)(xs)(st)
c__case_135_case__147 x1 x8 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_135_case__147")(x)



c__case_136_case__148 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_135(x8)(x9)(x1)(st))(st)
c__case_136_case__148 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_136_case__148(x1)(x)(st))(i)(xs)(st)
c__case_136_case__148 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_136_case__148")(x)



c__case_137_case__149 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_136(x6)(x1)(st))(st)
c__case_137_case__149 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_137_case__149(x1)(x)(st))(i)(xs)(st)
c__case_137_case__149 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_137_case__149")(x)



c__case_132_case__150 x1 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x12)(st)
c__case_132_case__150 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_132_case__150(x1)(x)(st))(i)(xs)(st)
c__case_132_case__150 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_132_case__150")(x)



c__case_133_case__151 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_132(x9)(x1)(st))(st)
c__case_133_case__151 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_133_case__151(x1)(x)(st))(i)(xs)(st)
c__case_133_case__151 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_133_case__151")(x)



c__case_134_case__152 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_133(x6)(x1)(st))(st)
c__case_134_case__152 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_134_case__152(x1)(x)(st))(i)(xs)(st)
c__case_134_case__152 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_134_case__152")(x)



c__case_129_case__153 x1 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x13)(st)
c__case_129_case__153 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_129_case__153(x1)(x)(st))(i)(xs)(st)
c__case_129_case__153 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_129_case__153")(x)



c__case_130_case__154 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_129(x9)(x1)(st))(st)
c__case_130_case__154 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_130_case__154(x1)(x)(st))(i)(xs)(st)
c__case_130_case__154 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_130_case__154")(x)



c__case_131_case__155 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_130(x6)(x1)(st))(st)
c__case_131_case__155 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_131_case__155(x1)(x)(st))(i)(xs)(st)
c__case_131_case__155 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_131_case__155")(x)



c__case_126_case__156 x1 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x14)(st)
c__case_126_case__156 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_126_case__156(x1)(x)(st))(i)(xs)(st)
c__case_126_case__156 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_126_case__156")(x)



c__case_127_case__157 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_126(x9)(x1)(st))(st)
c__case_127_case__157 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_127_case__157(x1)(x)(st))(i)(xs)(st)
c__case_127_case__157 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_127_case__157")(x)



c__case_128_case__158 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_127(x6)(x1)(st))(st)
c__case_128_case__158 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_128_case__158(x1)(x)(st))(i)(xs)(st)
c__case_128_case__158 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_128_case__158")(x)



c__case_123_case__159 x1 x10 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x10)(st)
c__case_123_case__159 x1 x10 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_123_case__159(x1)(x10)(x)(st))(i)(xs)(st)
c__case_123_case__159 x1 x10 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_123_case__159")(x)



c__case_124_case__160 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_123(x10)(x9)(x1)(st))(st)
c__case_124_case__160 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_124_case__160(x1)(x)(st))(i)(xs)(st)
c__case_124_case__160 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_124_case__160")(x)



c__case_125_case__161 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_124(x6)(x1)(st))(st)
c__case_125_case__161 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_125_case__161(x1)(x)(st))(i)(xs)(st)
c__case_125_case__161 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_125_case__161")(x)



c__case_120_case__162 x1 x3 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_120_case__162 x1 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_120_case__162(x1)(x3)(x)(st))(i)(xs)(st)
c__case_120_case__162 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_120_case__162")(x)



c__case_121_case__163 x1 x3 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_120(x3)(x10)(x1)(st))(st)
c__case_121_case__163 x1 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_121_case__163(x1)(x3)(x)(st))(i)(xs)(st)
c__case_121_case__163 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_121_case__163")(x)



c__case_122_case__164 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_121(x3)(x6)(x1)(st))(st)
c__case_122_case__164 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_122_case__164(x1)(x)(st))(i)(xs)(st)
c__case_122_case__164 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_122_case__164")(x)



c__case_117_case__165 x1 x4 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_117_case__165 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_117_case__165(x1)(x4)(x)(st))(i)(xs)(st)
c__case_117_case__165 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_117_case__165")(x)



c__case_118_case__166 x1 x4 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_117(x4)(x10)(x1)(st))(st)
c__case_118_case__166 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_118_case__166(x1)(x4)(x)(st))(i)(xs)(st)
c__case_118_case__166 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_118_case__166")(x)



c__case_119_case__167 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_118(x4)(x6)(x1)(st))(st)
c__case_119_case__167 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_119_case__167(x1)(x)(st))(i)(xs)(st)
c__case_119_case__167 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_119_case__167")(x)



c__case_114_case__168 x1 x5 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_114_case__168 x1 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_114_case__168(x1)(x5)(x)(st))(i)(xs)(st)
c__case_114_case__168 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_114_case__168")(x)



c__case_115_case__169 x1 x5 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_114(x5)(x10)(x1)(st))(st)
c__case_115_case__169 x1 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_115_case__169(x1)(x5)(x)(st))(i)(xs)(st)
c__case_115_case__169 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_115_case__169")(x)



c__case_116_case__170 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_115(x5)(x6)(x1)(st))(st)
c__case_116_case__170 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_116_case__170(x1)(x)(st))(i)(xs)(st)
c__case_116_case__170 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_116_case__170")(x)



c__case_111_case__171 x1 x8 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_111_case__171 x1 x8 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_111_case__171(x1)(x8)(x)(st))(i)(xs)(st)
c__case_111_case__171 x1 x8 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_111_case__171")(x)



c__case_112_case__172 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_111(x8)(x10)(x1)(st))(st)
c__case_112_case__172 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_112_case__172(x1)(x)(st))(i)(xs)(st)
c__case_112_case__172 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_112_case__172")(x)



c__case_113_case__173 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_112(x6)(x1)(st))(st)
c__case_113_case__173 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_113_case__173(x1)(x)(st))(i)(xs)(st)
c__case_113_case__173 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_113_case__173")(x)



c__case_108_case__174 x1 x9 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x9)(st)
c__case_108_case__174 x1 x9 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_108_case__174(x1)(x9)(x)(st))(i)(xs)(st)
c__case_108_case__174 x1 x9 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_108_case__174")(x)



c__case_109_case__175 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_108(x9)(x10)(x1)(st))(st)
c__case_109_case__175 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_109_case__175(x1)(x)(st))(i)(xs)(st)
c__case_109_case__175 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_109_case__175")(x)



c__case_110_case__176 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_109(x6)(x1)(st))(st)
c__case_110_case__176 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_110_case__176(x1)(x)(st))(i)(xs)(st)
c__case_110_case__176 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_110_case__176")(x)



c__case_105_case__177 x1 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x12)(st)
c__case_105_case__177 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_105_case__177(x1)(x)(st))(i)(xs)(st)
c__case_105_case__177 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_105_case__177")(x)



c__case_106_case__178 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_105(x10)(x1)(st))(st)
c__case_106_case__178 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_106_case__178(x1)(x)(st))(i)(xs)(st)
c__case_106_case__178 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_106_case__178")(x)



c__case_107_case__179 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_106(x6)(x1)(st))(st)
c__case_107_case__179 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_107_case__179(x1)(x)(st))(i)(xs)(st)
c__case_107_case__179 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_107_case__179")(x)



c__case_102_case__180 x1 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x13)(st)
c__case_102_case__180 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_102_case__180(x1)(x)(st))(i)(xs)(st)
c__case_102_case__180 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_102_case__180")(x)



c__case_103_case__181 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_102(x10)(x1)(st))(st)
c__case_103_case__181 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_103_case__181(x1)(x)(st))(i)(xs)(st)
c__case_103_case__181 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_103_case__181")(x)



c__case_104_case__182 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_103(x6)(x1)(st))(st)
c__case_104_case__182 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_104_case__182(x1)(x)(st))(i)(xs)(st)
c__case_104_case__182 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_104_case__182")(x)



c__case_99_case__183 x1 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x14)(st)
c__case_99_case__183 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_99_case__183(x1)(x)(st))(i)(xs)(st)
c__case_99_case__183 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_99_case__183")(x)



c__case_100_case__184 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_99(x10)(x1)(st))(st)
c__case_100_case__184 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_100_case__184(x1)(x)(st))(i)(xs)(st)
c__case_100_case__184 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_100_case__184")(x)



c__case_101_case__185 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_100(x6)(x1)(st))(st)
c__case_101_case__185 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_101_case__185(x1)(x)(st))(i)(xs)(st)
c__case_101_case__185 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_101_case__185")(x)



c__case_97_case__186 x1 x3 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_97_case__186 x1 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_97_case__186(x1)(x3)(x)(st))(i)(xs)(st)
c__case_97_case__186 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_97_case__186")(x)



c__case_98_case__187 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_97(x3)(x6)(x1)(st))(st)
c__case_98_case__187 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_98_case__187(x1)(x)(st))(i)(xs)(st)
c__case_98_case__187 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_98_case__187")(x)



c__case_95_case__188 x1 x4 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_95_case__188 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_95_case__188(x1)(x4)(x)(st))(i)(xs)(st)
c__case_95_case__188 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_95_case__188")(x)



c__case_96_case__189 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_95(x4)(x6)(x1)(st))(st)
c__case_96_case__189 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_96_case__189(x1)(x)(st))(i)(xs)(st)
c__case_96_case__189 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_96_case__189")(x)



c__case_93_case__190 x1 x5 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_93_case__190 x1 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_93_case__190(x1)(x5)(x)(st))(i)(xs)(st)
c__case_93_case__190 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_93_case__190")(x)



c__case_94_case__191 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_93(x5)(x6)(x1)(st))(st)
c__case_94_case__191 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_94_case__191(x1)(x)(st))(i)(xs)(st)
c__case_94_case__191 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_94_case__191")(x)



c__case_91_case__192 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_91_case__192 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_91_case__192(x1)(x)(st))(i)(xs)(st)
c__case_91_case__192 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_91_case__192")(x)



c__case_92_case__193 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_91(x6)(x1)(st))(st)
c__case_92_case__193 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_92_case__193(x1)(x)(st))(i)(xs)(st)
c__case_92_case__193 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_92_case__193")(x)



c__case_89_case__194 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x9)(st)
c__case_89_case__194 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_89_case__194(x1)(x)(st))(i)(xs)(st)
c__case_89_case__194 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_89_case__194")(x)



c__case_90_case__195 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_89(x6)(x1)(st))(st)
c__case_90_case__195 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_90_case__195(x1)(x)(st))(i)(xs)(st)
c__case_90_case__195 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_90_case__195")(x)



c__case_87_case__196 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x10)(st)
c__case_87_case__196 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_87_case__196(x1)(x)(st))(i)(xs)(st)
c__case_87_case__196 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_87_case__196")(x)



c__case_88_case__197 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_87(x6)(x1)(st))(st)
c__case_88_case__197 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_88_case__197(x1)(x)(st))(i)(xs)(st)
c__case_88_case__197 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_88_case__197")(x)



c__case_85_case__198 x1 x4 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_85_case__198 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_85_case__198(x1)(x4)(x)(st))(i)(xs)(st)
c__case_85_case__198 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_85_case__198")(x)



c__case_86_case__199 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_85(x4)(x6)(x1)(st))(st)
c__case_86_case__199 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_86_case__199(x1)(x)(st))(i)(xs)(st)
c__case_86_case__199 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_86_case__199")(x)



c__case_83_case__200 x1 x5 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_83_case__200 x1 x5 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_83_case__200(x1)(x5)(x)(st))(i)(xs)(st)
c__case_83_case__200 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_83_case__200")(x)



c__case_84_case__201 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_83(x5)(x6)(x1)(st))(st)
c__case_84_case__201 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_84_case__201(x1)(x)(st))(i)(xs)(st)
c__case_84_case__201 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_84_case__201")(x)



c__case_81_case__202 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_81_case__202 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_81_case__202(x1)(x)(st))(i)(xs)(st)
c__case_81_case__202 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_81_case__202")(x)



c__case_82_case__203 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_81(x6)(x1)(st))(st)
c__case_82_case__203 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_82_case__203(x1)(x)(st))(i)(xs)(st)
c__case_82_case__203 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_82_case__203")(x)



c__case_79_case__204 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x9)(st)
c__case_79_case__204 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_79_case__204(x1)(x)(st))(i)(xs)(st)
c__case_79_case__204 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_79_case__204")(x)



c__case_80_case__205 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_79(x6)(x1)(st))(st)
c__case_80_case__205 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_80_case__205(x1)(x)(st))(i)(xs)(st)
c__case_80_case__205 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_80_case__205")(x)



c__case_77_case__206 x1 x6@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x10)(st)
c__case_77_case__206 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_77_case__206(x1)(x)(st))(i)(xs)(st)
c__case_77_case__206 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_77_case__206")(x)



c__case_78_case__207 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_77(x6)(x1)(st))(st)
c__case_78_case__207 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_78_case__207(x1)(x)(st))(i)(xs)(st)
c__case_78_case__207 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_78_case__207")(x)



c__case_76_case__208 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c_reviseRight(x2)(x1)(st))(st)
c__case_76_case__208 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_76_case__208 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_76_case__208(x1)(x2)(x)(st))(i)(xs)(st)
c__case_76_case__208 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_76_case__208")(x)



c__case_71_case__209 x1 x25@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.RedBlackTree.C_Black)(st)
c__case_71_case__209 x1 x25@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.RedBlackTree.C_DoublyBlack)(st)
c__case_71_case__209 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_71_case__209(x1)(x)(st))(i)(xs)(st)
c__case_71_case__209 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_71_case__209")(x)



c__case_70_case__210 x1 x2 x36@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(let {x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x37)(Curry.Module.Prelude.List))(let {x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x37)((Curry.Module.Prelude.:<)(x38)(Curry.Module.Prelude.List))(let {x39 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x38)((Curry.Module.Prelude.:<)(x39)(Curry.Module.Prelude.List))(let {x40 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x39)((Curry.Module.Prelude.:<)(x40)(Curry.Module.Prelude.List))(let {x41 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x40)((Curry.Module.Prelude.:<)(x41)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x41)(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP96'35y(x2)(x37)(st))(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP97'35c(x2)(x38)(st))(Curry.Module.OracleRedBlackTree.c_reviseRight(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Red)(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP95'35x(x2)(x1)(st))(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP98'35b(x2)(x39)(st))(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP99'35a(x2)(x40)(st)))(x41)(st)))(st))(st))(st))(st))(st))(st))(st)
c__case_70_case__210 x1 x2 x36@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_70_case__210 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_70_case__210(x1)(x2)(x)(st))(i)(xs)(st)
c__case_70_case__210 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_70_case__210")(x)



c__case_72_case__211 x1 x2 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(let {x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x30)(Curry.Module.Prelude.List))(let {x24 = Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP88'35col(x2)(x1)(st)} in let {x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x30)((Curry.Module.Prelude.:<)(x31)(Curry.Module.Prelude.List))(let {x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x31)((Curry.Module.Prelude.:<)(x32)(Curry.Module.Prelude.List))(let {x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x32)((Curry.Module.Prelude.:<)(x33)(Curry.Module.Prelude.List))(let {x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x33)((Curry.Module.Prelude.:<)(x34)(Curry.Module.Prelude.List))(let {x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x34)((Curry.Module.Prelude.:<)(x35)(Curry.Module.Prelude.List))(let {x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x35)((Curry.Module.Prelude.:<)(x36)((Curry.Module.Prelude.:<)(x37)(Curry.Module.Prelude.List)))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.OracleRedBlackTree.c__case_71(x24)(Curry.Module.OraclePrelude.op_61_61(x24)(Curry.Module.RedBlackTree.C_Red)(x35)(st))(x36)(st))(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP89'35x(x2)(x30)(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Red)(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP90'35y(x2)(x31)(st))(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP91'35c(x2)(x32)(st))(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP92'35b(x2)(x33)(st)))(Curry.Module.OracleRedBlackTree.c_singleBlack(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP93'35a(x2)(x34)(st))(x37)(st)))(st))(st))(st))(st))(st))(st))(st))(st)
c__case_72_case__211 x1 x2 x4@Curry.Module.Prelude.C_False st = let {x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x38)(Curry.Module.Prelude.List))(Curry.Module.OracleRedBlackTree.c__case_70(x2)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x38)(st))(st)
c__case_72_case__211 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_72_case__211(x1)(x2)(x)(st))(i)(xs)(st)
c__case_72_case__211 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_72_case__211")(x)



c__case_73_case__212 x1 x2 x4 x23@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List))(let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x24)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))(let {x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x25)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List))(let {x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List))(let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x27)((Curry.Module.Prelude.:<)(x28)(Curry.Module.Prelude.List))(let {x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x28)((Curry.Module.Prelude.:<)(x29)(Curry.Module.Prelude.List))(let {x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x29)((Curry.Module.Prelude.:<)(x30)(Curry.Module.Prelude.List))(let {x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x30)((Curry.Module.Prelude.:<)(x31)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x31)(Curry.Module.RedBlackTree.C_Tree(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP79'35col(x2)(x1)(st))(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP83'35y(x2)(x27)(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP81'35z(x2)(x25)(st))(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP82'35d(x2)(x26)(st))(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP84'35c(x2)(x28)(st)))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP80'35x(x2)(x24)(st))(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP85'35b(x2)(x29)(st))(Curry.Module.OracleRedBlackTree.c_singleBlack(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP86'35a(x2)(x30)(st))(x31)(st))))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)
c__case_73_case__212 x1 x2 x4 x23@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_72(x2)(x4)(x1)(st))(st)
c__case_73_case__212 x1 x2 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_73_case__212(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_73_case__212 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_73_case__212")(x)



c__case_74_case__213 x1 x2 x3 x4 x14@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))(let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))(let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))(let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))(let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))(let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x21)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x22)(Curry.Module.RedBlackTree.C_Tree(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP70'35col(x2)(x1)(st))(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP72'35y(x2)(x16)(st))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP73'35z(x2)(x17)(st))(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP74'35d(x2)(x18)(st))(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP75'35c(x2)(x19)(st)))(Curry.Module.RedBlackTree.C_Tree(Curry.Module.RedBlackTree.C_Black)(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP71'35x(x2)(x15)(st))(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP76'35b(x2)(x20)(st))(Curry.Module.OracleRedBlackTree.c_singleBlack(Curry.Module.OracleRedBlackTree.c_reviseRight'46_'35selFP77'35a(x2)(x21)(st))(x22)(st))))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)
c__case_74_case__213 x1 x2 x3 x4 x14@Curry.Module.Prelude.C_False st = let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))))(Curry.Module.OracleRedBlackTree.c__case_73(x2)(x3)(x4)(Curry.Module.OraclePrelude.op_38_38(x4)(Curry.Module.OracleRedBlackTree.c_isRed(Curry.Module.OracleRedBlackTree.c_right(x3)(x1)(st))(x23)(st))(x24)(st))(x25)(st))(st)
c__case_74_case__213 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_74_case__213(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_74_case__213 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_74_case__213")(x)



c__case_75_case__214 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_75_case__214 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))))(Curry.Module.OracleRedBlackTree.c__case_74(x2)(x3)(x4)(Curry.Module.OraclePrelude.op_38_38(x4)(Curry.Module.OracleRedBlackTree.c_isRed(Curry.Module.OracleRedBlackTree.c_left(x3)(x1)(st))(x6)(st))(x7)(st))(x8)(st))(st)
c__case_75_case__214 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_75_case__214(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_75_case__214 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_75_case__214")(x)



c__case_67_case__215 x1 x3 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_67_case__215 x1 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_67_case__215(x1)(x3)(x)(st))(i)(xs)(st)
c__case_67_case__215 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_67_case__215")(x)



c__case_68_case__216 x1 x3 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_67(x3)(x9)(x1)(st))(st)
c__case_68_case__216 x1 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_68_case__216(x1)(x3)(x)(st))(i)(xs)(st)
c__case_68_case__216 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_68_case__216")(x)



c__case_69_case__217 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_68(x3)(x5)(x1)(st))(st)
c__case_69_case__217 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_69_case__217(x1)(x)(st))(i)(xs)(st)
c__case_69_case__217 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_69_case__217")(x)



c__case_64_case__218 x1 x4 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_64_case__218 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_64_case__218(x1)(x4)(x)(st))(i)(xs)(st)
c__case_64_case__218 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_64_case__218")(x)



c__case_65_case__219 x1 x4 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_64(x4)(x9)(x1)(st))(st)
c__case_65_case__219 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_65_case__219(x1)(x4)(x)(st))(i)(xs)(st)
c__case_65_case__219 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_65_case__219")(x)



c__case_66_case__220 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_65(x4)(x5)(x1)(st))(st)
c__case_66_case__220 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_66_case__220(x1)(x)(st))(i)(xs)(st)
c__case_66_case__220 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_66_case__220")(x)



c__case_61_case__221 x1 x8 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_61_case__221 x1 x8 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_61_case__221(x1)(x8)(x)(st))(i)(xs)(st)
c__case_61_case__221 x1 x8 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_61_case__221")(x)



c__case_62_case__222 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_61(x8)(x9)(x1)(st))(st)
c__case_62_case__222 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_62_case__222(x1)(x)(st))(i)(xs)(st)
c__case_62_case__222 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_62_case__222")(x)



c__case_63_case__223 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_62(x5)(x1)(st))(st)
c__case_63_case__223 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_63_case__223(x1)(x)(st))(i)(xs)(st)
c__case_63_case__223 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_63_case__223")(x)



c__case_58_case__224 x1 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x12)(st)
c__case_58_case__224 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_58_case__224(x1)(x)(st))(i)(xs)(st)
c__case_58_case__224 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_58_case__224")(x)



c__case_59_case__225 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_58(x9)(x1)(st))(st)
c__case_59_case__225 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_59_case__225(x1)(x)(st))(i)(xs)(st)
c__case_59_case__225 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_59_case__225")(x)



c__case_60_case__226 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_59(x5)(x1)(st))(st)
c__case_60_case__226 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_60_case__226(x1)(x)(st))(i)(xs)(st)
c__case_60_case__226 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_60_case__226")(x)



c__case_55_case__227 x1 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x13)(st)
c__case_55_case__227 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_55_case__227(x1)(x)(st))(i)(xs)(st)
c__case_55_case__227 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_55_case__227")(x)



c__case_56_case__228 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_55(x9)(x1)(st))(st)
c__case_56_case__228 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_56_case__228(x1)(x)(st))(i)(xs)(st)
c__case_56_case__228 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_56_case__228")(x)



c__case_57_case__229 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_56(x5)(x1)(st))(st)
c__case_57_case__229 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_57_case__229(x1)(x)(st))(i)(xs)(st)
c__case_57_case__229 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_57_case__229")(x)



c__case_52_case__230 x1 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x14)(st)
c__case_52_case__230 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_52_case__230(x1)(x)(st))(i)(xs)(st)
c__case_52_case__230 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_52_case__230")(x)



c__case_53_case__231 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_52(x9)(x1)(st))(st)
c__case_53_case__231 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_53_case__231(x1)(x)(st))(i)(xs)(st)
c__case_53_case__231 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_53_case__231")(x)



c__case_54_case__232 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_53(x5)(x1)(st))(st)
c__case_54_case__232 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_54_case__232(x1)(x)(st))(i)(xs)(st)
c__case_54_case__232 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_54_case__232")(x)



c__case_49_case__233 x1 x10 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x10)(st)
c__case_49_case__233 x1 x10 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_49_case__233(x1)(x10)(x)(st))(i)(xs)(st)
c__case_49_case__233 x1 x10 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_49_case__233")(x)



c__case_50_case__234 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_49(x10)(x9)(x1)(st))(st)
c__case_50_case__234 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_50_case__234(x1)(x)(st))(i)(xs)(st)
c__case_50_case__234 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_50_case__234")(x)



c__case_51_case__235 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_50(x5)(x1)(st))(st)
c__case_51_case__235 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_51_case__235(x1)(x)(st))(i)(xs)(st)
c__case_51_case__235 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_51_case__235")(x)



c__case_46_case__236 x1 x6 x9@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_46_case__236 x1 x6 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_46_case__236(x1)(x6)(x)(st))(i)(xs)(st)
c__case_46_case__236 x1 x6 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_46_case__236")(x)



c__case_47_case__237 x1 x6 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_46(x6)(x9)(x1)(st))(st)
c__case_47_case__237 x1 x6 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_47_case__237(x1)(x6)(x)(st))(i)(xs)(st)
c__case_47_case__237 x1 x6 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_47_case__237")(x)



c__case_48_case__238 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_47(x6)(x5)(x1)(st))(st)
c__case_48_case__238 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_48_case__238(x1)(x)(st))(i)(xs)(st)
c__case_48_case__238 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_48_case__238")(x)



c__case_43_case__239 x1 x3 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_43_case__239 x1 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_43_case__239(x1)(x3)(x)(st))(i)(xs)(st)
c__case_43_case__239 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_43_case__239")(x)



c__case_44_case__240 x1 x3 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_43(x3)(x10)(x1)(st))(st)
c__case_44_case__240 x1 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_44_case__240(x1)(x3)(x)(st))(i)(xs)(st)
c__case_44_case__240 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_44_case__240")(x)



c__case_45_case__241 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_44(x3)(x5)(x1)(st))(st)
c__case_45_case__241 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_45_case__241(x1)(x)(st))(i)(xs)(st)
c__case_45_case__241 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_45_case__241")(x)



c__case_40_case__242 x1 x4 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_40_case__242 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_40_case__242(x1)(x4)(x)(st))(i)(xs)(st)
c__case_40_case__242 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_40_case__242")(x)



c__case_41_case__243 x1 x4 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_40(x4)(x10)(x1)(st))(st)
c__case_41_case__243 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_41_case__243(x1)(x4)(x)(st))(i)(xs)(st)
c__case_41_case__243 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_41_case__243")(x)



c__case_42_case__244 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_41(x4)(x5)(x1)(st))(st)
c__case_42_case__244 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_42_case__244(x1)(x)(st))(i)(xs)(st)
c__case_42_case__244 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_42_case__244")(x)



c__case_37_case__245 x1 x8 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_37_case__245 x1 x8 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_37_case__245(x1)(x8)(x)(st))(i)(xs)(st)
c__case_37_case__245 x1 x8 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_37_case__245")(x)



c__case_38_case__246 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_37(x8)(x10)(x1)(st))(st)
c__case_38_case__246 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_38_case__246(x1)(x)(st))(i)(xs)(st)
c__case_38_case__246 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_38_case__246")(x)



c__case_39_case__247 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_38(x5)(x1)(st))(st)
c__case_39_case__247 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_39_case__247(x1)(x)(st))(i)(xs)(st)
c__case_39_case__247 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_39_case__247")(x)



c__case_34_case__248 x1 x9 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x9)(st)
c__case_34_case__248 x1 x9 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_34_case__248(x1)(x9)(x)(st))(i)(xs)(st)
c__case_34_case__248 x1 x9 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_34_case__248")(x)



c__case_35_case__249 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_34(x9)(x10)(x1)(st))(st)
c__case_35_case__249 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_35_case__249(x1)(x)(st))(i)(xs)(st)
c__case_35_case__249 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_35_case__249")(x)



c__case_36_case__250 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_35(x5)(x1)(st))(st)
c__case_36_case__250 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_36_case__250(x1)(x)(st))(i)(xs)(st)
c__case_36_case__250 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_36_case__250")(x)



c__case_31_case__251 x1 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x12)(st)
c__case_31_case__251 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_31_case__251(x1)(x)(st))(i)(xs)(st)
c__case_31_case__251 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_31_case__251")(x)



c__case_32_case__252 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_31(x10)(x1)(st))(st)
c__case_32_case__252 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_32_case__252(x1)(x)(st))(i)(xs)(st)
c__case_32_case__252 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_32_case__252")(x)



c__case_33_case__253 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_32(x5)(x1)(st))(st)
c__case_33_case__253 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_33_case__253(x1)(x)(st))(i)(xs)(st)
c__case_33_case__253 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_33_case__253")(x)



c__case_28_case__254 x1 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x13)(st)
c__case_28_case__254 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_28_case__254(x1)(x)(st))(i)(xs)(st)
c__case_28_case__254 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_28_case__254")(x)



c__case_29_case__255 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_28(x10)(x1)(st))(st)
c__case_29_case__255 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_29_case__255(x1)(x)(st))(i)(xs)(st)
c__case_29_case__255 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_29_case__255")(x)



c__case_30_case__256 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_29(x5)(x1)(st))(st)
c__case_30_case__256 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_30_case__256(x1)(x)(st))(i)(xs)(st)
c__case_30_case__256 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_30_case__256")(x)



c__case_25_case__257 x1 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x14)(st)
c__case_25_case__257 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_25_case__257(x1)(x)(st))(i)(xs)(st)
c__case_25_case__257 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_25_case__257")(x)



c__case_26_case__258 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_25(x10)(x1)(st))(st)
c__case_26_case__258 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_26_case__258(x1)(x)(st))(i)(xs)(st)
c__case_26_case__258 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_26_case__258")(x)



c__case_27_case__259 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_26(x5)(x1)(st))(st)
c__case_27_case__259 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_27_case__259(x1)(x)(st))(i)(xs)(st)
c__case_27_case__259 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_27_case__259")(x)



c__case_22_case__260 x1 x6 x10@(Curry.Module.RedBlackTree.C_Tree x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_22_case__260 x1 x6 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_22_case__260(x1)(x6)(x)(st))(i)(xs)(st)
c__case_22_case__260 x1 x6 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_22_case__260")(x)



c__case_23_case__261 x1 x6 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_22(x6)(x10)(x1)(st))(st)
c__case_23_case__261 x1 x6 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_23_case__261(x1)(x6)(x)(st))(i)(xs)(st)
c__case_23_case__261 x1 x6 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_23_case__261")(x)



c__case_24_case__262 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_23(x6)(x5)(x1)(st))(st)
c__case_24_case__262 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_24_case__262(x1)(x)(st))(i)(xs)(st)
c__case_24_case__262 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_24_case__262")(x)



c__case_20_case__263 x1 x3 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_20_case__263 x1 x3 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_20_case__263(x1)(x3)(x)(st))(i)(xs)(st)
c__case_20_case__263 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_20_case__263")(x)



c__case_21_case__264 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_20(x3)(x5)(x1)(st))(st)
c__case_21_case__264 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_21_case__264(x1)(x)(st))(i)(xs)(st)
c__case_21_case__264 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_21_case__264")(x)



c__case_18_case__265 x1 x4 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_18_case__265 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_18_case__265(x1)(x4)(x)(st))(i)(xs)(st)
c__case_18_case__265 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_18_case__265")(x)



c__case_19_case__266 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_18(x4)(x5)(x1)(st))(st)
c__case_19_case__266 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_19_case__266(x1)(x)(st))(i)(xs)(st)
c__case_19_case__266 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_19_case__266")(x)



c__case_16_case__267 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_16_case__267 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_16_case__267(x1)(x)(st))(i)(xs)(st)
c__case_16_case__267 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_16_case__267")(x)



c__case_17_case__268 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_16(x5)(x1)(st))(st)
c__case_17_case__268 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_17_case__268(x1)(x)(st))(i)(xs)(st)
c__case_17_case__268 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_17_case__268")(x)



c__case_14_case__269 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x9)(st)
c__case_14_case__269 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_14_case__269(x1)(x)(st))(i)(xs)(st)
c__case_14_case__269 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_14_case__269")(x)



c__case_15_case__270 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_14(x5)(x1)(st))(st)
c__case_15_case__270 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_15_case__270(x1)(x)(st))(i)(xs)(st)
c__case_15_case__270 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_15_case__270")(x)



c__case_12_case__271 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x10)(st)
c__case_12_case__271 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_12_case__271(x1)(x)(st))(i)(xs)(st)
c__case_12_case__271 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_12_case__271")(x)



c__case_13_case__272 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_12(x5)(x1)(st))(st)
c__case_13_case__272 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_13_case__272(x1)(x)(st))(i)(xs)(st)
c__case_13_case__272 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_13_case__272")(x)



c__case_10_case__273 x1 x6 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_10_case__273 x1 x6 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_10_case__273(x1)(x6)(x)(st))(i)(xs)(st)
c__case_10_case__273 x1 x6 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_10_case__273")(x)



c__case_11_case__274 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_10(x6)(x5)(x1)(st))(st)
c__case_11_case__274 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_11_case__274(x1)(x)(st))(i)(xs)(st)
c__case_11_case__274 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_11_case__274")(x)



c__case_8_case__275 x1 x4 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_8_case__275 x1 x4 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_8_case__275(x1)(x4)(x)(st))(i)(xs)(st)
c__case_8_case__275 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_8_case__275")(x)



c__case_9_case__276 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_8(x4)(x5)(x1)(st))(st)
c__case_9_case__276 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_9_case__276(x1)(x)(st))(i)(xs)(st)
c__case_9_case__276 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_9_case__276")(x)



c__case_6_case__277 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_6_case__277 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_6_case__277(x1)(x)(st))(i)(xs)(st)
c__case_6_case__277 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_6_case__277")(x)



c__case_7_case__278 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_6(x5)(x1)(st))(st)
c__case_7_case__278 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_7_case__278(x1)(x)(st))(i)(xs)(st)
c__case_7_case__278 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_7_case__278")(x)



c__case_4_case__279 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x9)(st)
c__case_4_case__279 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_4_case__279(x1)(x)(st))(i)(xs)(st)
c__case_4_case__279 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_4_case__279")(x)



c__case_5_case__280 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_4(x5)(x1)(st))(st)
c__case_5_case__280 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_5_case__280(x1)(x)(st))(i)(xs)(st)
c__case_5_case__280 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_5_case__280")(x)



c__case_2_case__281 x1 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x10)(st)
c__case_2_case__281 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_2_case__281(x1)(x)(st))(i)(xs)(st)
c__case_2_case__281 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_2_case__281")(x)



c__case_3_case__282 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_2(x5)(x1)(st))(st)
c__case_3_case__282 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_3_case__282(x1)(x)(st))(i)(xs)(st)
c__case_3_case__282 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_3_case__282")(x)



c__case_0_case__283 x1 x6 x5@(Curry.Module.RedBlackTree.C_Tree x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_0_case__283 x1 x6 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_0_case__283(x1)(x6)(x)(st))(i)(xs)(st)
c__case_0_case__283 x1 x6 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_0_case__283")(x)



c__case_1_case__284 x1 x2@(Curry.Module.RedBlackTree.C_Tree x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleRedBlackTree.c__case_0(x6)(x5)(x1)(st))(st)
c__case_1_case__284 x1 (Curry.Module.RedBlackTree.C_TreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleRedBlackTree.c__case_1_case__284(x1)(x)(st))(i)(xs)(st)
c__case_1_case__284 x1 x st = Curry.RunTimeSystem.patternFail("OracleRedBlackTree._case_1_case__284")(x)


