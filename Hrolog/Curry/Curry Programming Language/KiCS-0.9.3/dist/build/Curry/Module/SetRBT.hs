{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.SetRBT (module Curry.Module.SetRBT) where

import Curry.RunTimeSystem
import Curry.Module.Maybe
import Curry.Module.Prelude
import Curry.Module.RedBlackTree



-- begin included



-- end included

type C_SetRBT t0 = Curry.Module.RedBlackTree.C_RedBlackTree t0

c_emptySetRBT :: (Curry t0) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_RedBlackTree t0)
c_emptySetRBT st = Curry.Module.Prelude.pf(Curry.Module.RedBlackTree.c_empty(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_61_61))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_61_61)))



c_elemRBT :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.RedBlackTree.C_RedBlackTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)
c_elemRBT x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Maybe.c_isJust))(Curry.Module.Prelude.pf(Curry.Module.RedBlackTree.c_lookup(x1)))(st)



c_insertRBT :: (Curry t0) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.RedBlackTree.C_RedBlackTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_RedBlackTree t0))
c_insertRBT st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.RedBlackTree.c_update)



c_insertMultiRBT :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.RedBlackTree.C_RedBlackTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_RedBlackTree t0)
c_insertMultiRBT x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.RedBlackTree.c_setInsertEquivalence(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_61_61))))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.RedBlackTree.c_update(x1)))(Curry.Module.Prelude.pf(Curry.Module.RedBlackTree.c_setInsertEquivalence(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.SetRBT.c_insertMultiRBT'46_'35lambda2))))(st))(st)



c_insertMultiRBT'46_'35lambda2 :: (Curry t27) => t27 -> t27 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_insertMultiRBT'46_'35lambda2 x1 x2 st = Curry.Module.Prelude.C_False



c_deleteRBT :: (Curry t0) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.RedBlackTree.C_RedBlackTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_RedBlackTree t0))
c_deleteRBT st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.RedBlackTree.c_delete)



c_setRBT2list :: (Curry t0) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.RedBlackTree.C_RedBlackTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0)
c_setRBT2list st = Curry.Module.Prelude.pf(Curry.Module.RedBlackTree.c_tree2list)



c_unionRBT :: (Curry t0) => (Curry.Module.RedBlackTree.C_RedBlackTree t0) -> (Curry.Module.RedBlackTree.C_RedBlackTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_RedBlackTree t0
c_unionRBT x1 x2 st = Curry.Module.Prelude.c_foldr(Curry.Module.SetRBT.c_insertRBT(st))(x2)(Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_setRBT2list(st))(x1)(st))(st)



c_intersectRBT :: (Curry t0) => (Curry.Module.RedBlackTree.C_RedBlackTree t0) -> (Curry.Module.RedBlackTree.C_RedBlackTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_RedBlackTree t0
c_intersectRBT x1 x2 st = Curry.Module.Prelude.c_foldr(Curry.Module.SetRBT.c_insertRBT(st))(Curry.Module.RedBlackTree.c_newTreeLike(x1)(st))(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.pf(Curry.Module.SetRBT.c_intersectRBT'46_'35lambda3(x2)))(Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_setRBT2list(st))(x1)(st))(st))(st)



c_intersectRBT'46_'35lambda3 :: (Curry t46) => (Curry.Module.RedBlackTree.C_RedBlackTree t46) -> t46 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_intersectRBT'46_'35lambda3 x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.SetRBT.c_elemRBT(x2)(st))(x1)(st)



c_sortRBT :: (Curry t0) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0))
c_sortRBT st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.RedBlackTree.c_sort)


