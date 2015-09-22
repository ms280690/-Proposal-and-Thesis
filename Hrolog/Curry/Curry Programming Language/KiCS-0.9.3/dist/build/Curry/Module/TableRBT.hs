{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.TableRBT (module Curry.Module.TableRBT) where

import Curry.RunTimeSystem
import Curry.Module.Prelude
import Curry.Module.RedBlackTree



-- begin included



-- end included

type C_TableRBT t0 t1 = Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 t0 t1)

c_emptyTableRBT :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 t0 t1)
c_emptyTableRBT x1 st = Curry.Module.RedBlackTree.c_empty(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.TableRBT.c_emptyTableRBT'46_'35lambda2))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.TableRBT.c_emptyTableRBT'46_'35lambda3))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.TableRBT.c_emptyTableRBT'46_'35lambda4(x1)))(st)



c_emptyTableRBT'46_'35lambda2 :: (Curry t22,Curry t23) => (Curry.Module.Prelude.T2 t22 t23) -> (Curry.Module.Prelude.T2 t22 t23) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_emptyTableRBT'46_'35lambda2 x1 x2 st = Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_fst(x1)(st))(Curry.Module.Prelude.c_fst(x2)(st))(st)



c_emptyTableRBT'46_'35lambda3 :: (Curry t22,Curry t23) => (Curry.Module.Prelude.T2 t22 t23) -> (Curry.Module.Prelude.T2 t22 t23) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_emptyTableRBT'46_'35lambda3 x1 x2 st = Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_fst(x1)(st))(Curry.Module.Prelude.c_fst(x2)(st))(st)



c_emptyTableRBT'46_'35lambda4 :: (Curry t22,Curry t23) => (Curry.Module.Prelude.Prim (t22 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t22 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.T2 t22 t23) -> (Curry.Module.Prelude.T2 t22 t23) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_emptyTableRBT'46_'35lambda4 x1 x2 x3 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(Curry.Module.Prelude.c_fst(x2)(st))(st))(Curry.Module.Prelude.c_fst(x3)(st))(st)



c_isEmptyTable :: (Curry t0,Curry t1) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 t0 t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)
c_isEmptyTable st = Curry.Module.Prelude.pf(Curry.Module.RedBlackTree.c_isEmpty)



c_lookupRBT :: (Curry t0,Curry t1) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 t0 t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t1)
c_lookupRBT x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.C_Nothing)(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc(Curry.Module.Prelude.C_Just))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))(st))))(Curry.Module.Prelude.pf(Curry.Module.RedBlackTree.c_lookup(Curry.Module.Prelude.T2(x1)(Curry.Module.Prelude.c_failed(st)))))(st)



c_updateRBT :: (Curry t0,Curry t1) => t0 -> t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 t0 t1)) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 t0 t1))
c_updateRBT x1 x2 st = Curry.Module.Prelude.pf(Curry.Module.RedBlackTree.c_update(Curry.Module.Prelude.T2(x1)(x2)))



c_tableRBT2list :: (Curry t0,Curry t1) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 t0 t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 t1))
c_tableRBT2list st = Curry.Module.Prelude.pf(Curry.Module.RedBlackTree.c_tree2list)



c_deleteRBT :: (Curry t0,Curry t1) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 t0 t1)) -> Curry.RunTimeSystem.State -> Curry.Module.RedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 t0 t1))
c_deleteRBT x1 st = Curry.Module.Prelude.pf(Curry.Module.RedBlackTree.c_delete(Curry.Module.Prelude.T2(x1)(Curry.Module.Prelude.c_failed(st))))


