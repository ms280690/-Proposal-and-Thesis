{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Traversal (module Curry.Module.Traversal) where

import Curry.RunTimeSystem
import Curry.Module.Prelude



-- begin included



-- end included

type C_Traversable t0 t1 = t0 -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t0))

type C_FunList t0 = (Curry.Module.Prelude.List t0) -> Curry.Module.Prelude.List t0

c_noChildren :: (Curry t0,Curry t1) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t0))
c_noChildren x1 st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_const(x1)))



c_children :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t0)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1)
c_children x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_fst))(x1)(st)



c_replaceChildren :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t0)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t0))
c_replaceChildren x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))(x1)(st)



c_mapChildren :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t0)))) -> (Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t1)) -> t0 -> Curry.RunTimeSystem.State -> t0
c_mapChildren x1 x2 x3 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Traversal.c_replaceChildren(x1)(st))(x3)(st))(Curry.Module.Prelude.c_map(x2)(Curry.Module.Prelude.c_apply(Curry.Module.Traversal.c_children(x1)(st))(x3)(st))(st))(st)



c_family :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> t0)))) -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_family x1 x2 st = Curry.Module.Traversal.c_familyFL(x1)(x2)(Curry.Module.Prelude.List)(st)



c_childFamilies :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t0)))) -> (Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t1)))) -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1
c_childFamilies x1 x2 x3 st = Curry.Module.Traversal.c_childFamiliesFL(x1)(x2)(x3)(Curry.Module.Prelude.List)(st)



c_mapFamily :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> t0)))) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0)
c_mapFamily x1 x2 st = Curry.Module.Prelude.op_46(x2)(Curry.Module.Prelude.c_apply(Curry.Module.Traversal.c_mapChildFamilies(x1)(x1)(st))(x2)(st))(st)



c_mapChildFamilies :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t0)))) -> (Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t1)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0))
c_mapChildFamilies x1 x2 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Traversal.c_mapChildren(x1)))(Curry.Module.Prelude.pf(Curry.Module.Traversal.c_mapFamily(x2)))(st)



c_evalFamily :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> t0)))) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0)
c_evalFamily x1 x2 st = Curry.Module.Traversal.c_mapFamily(x1)(Curry.Module.Prelude.pf(Curry.Module.Traversal.c_evalFamily'46g'4619(x2)(x1)))(st)



c_evalFamily'46g'4619 :: (Curry t100) => (Curry.Module.Prelude.Prim (t100 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t100)) -> (Curry.Module.Prelude.Prim (t100 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t100) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t100) -> Curry.RunTimeSystem.State -> t100)))) -> t100 -> Curry.RunTimeSystem.State -> t100
c_evalFamily'46g'4619 x1 x2 x3 st = Curry.Module.Prelude.c_maybe(x3)(Curry.Module.Traversal.c_mapFamily(x2)(Curry.Module.Prelude.pf(Curry.Module.Traversal.c_evalFamily'46g'4619(x1)(x2)))(st))(Curry.Module.Prelude.c_apply(x1)(x3)(st))(st)



c_evalChildFamilies :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t0)))) -> (Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t1)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0))
c_evalChildFamilies x1 x2 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Traversal.c_mapChildren(x1)))(Curry.Module.Prelude.pf(Curry.Module.Traversal.c_evalFamily(x2)))(st)



c_fold :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> t0)))) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t1))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1)
c_fold x1 x2 st = Curry.Module.Prelude.pf(Curry.Module.Traversal.c_foldChildren(x1)(x1)(x2)(x2))



c_foldChildren :: (Curry t0,Curry t1,Curry t2,Curry t3) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t0)))) -> (Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t1)))) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t2) -> Curry.RunTimeSystem.State -> t3))) -> (Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t2) -> Curry.RunTimeSystem.State -> t2))) -> t0 -> Curry.RunTimeSystem.State -> t3
c_foldChildren x1 x2 x3 x4 x5 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x3)(x5)(st))(Curry.Module.Prelude.c_map(Curry.Module.Traversal.c_fold(x2)(x4)(st))(Curry.Module.Prelude.c_apply(Curry.Module.Traversal.c_children(x1)(st))(x5)(st))(st))(st)



c_replaceChildrenIO :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t0)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))
c_replaceChildrenIO x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Traversal.c_liftIO))(Curry.Module.Traversal.c_replaceChildren(x1)(st))(st)



c_mapChildrenIO :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t0)))) -> (Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1)) -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_mapChildrenIO x1 x2 x3 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Traversal.c_replaceChildrenIO(x1)(st))(x3)(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_mapIO(x2)(st))(Curry.Module.Prelude.c_apply(Curry.Module.Traversal.c_children(x1)(st))(x3)(st))(st))(st)



c_mapFamilyIO :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> t0)))) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0)) -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_mapFamilyIO x1 x2 x3 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Traversal.c_mapChildFamiliesIO(x1)(x1)(st))(x2)(st))(x3)(st))(x2)(st)



c_mapChildFamiliesIO :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t0)))) -> (Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t1)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))
c_mapChildFamiliesIO x1 x2 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Traversal.c_mapChildrenIO(x1)))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Traversal.c_mapFamilyIO(x2)))(st)



c_evalFamilyIO :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> t0)))) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t0))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0)
c_evalFamilyIO x1 x2 st = Curry.Module.Prelude.pf(Curry.Module.Traversal.c_mapFamilyIO(x1)(Curry.Module.Prelude.pf(Curry.Module.Traversal.c_evalFamilyIO'46g'4637(x2)(x1))))



c_evalFamilyIO'46g'4637 :: (Curry t202) => (Curry.Module.Prelude.Prim (t202 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t202))) -> (Curry.Module.Prelude.Prim (t202 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t202) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t202) -> Curry.RunTimeSystem.State -> t202)))) -> t202 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t202
c_evalFamilyIO'46g'4637 x1 x2 x3 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_apply(x1)(x3)(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.c_return(x3)(st))(Curry.Module.Prelude.pf(Curry.Module.Traversal.c_mapFamilyIO(x2)(Curry.Module.Prelude.pf(Curry.Module.Traversal.c_evalFamilyIO'46g'4637(x1)(x2)))))))(st)



c_evalChildFamiliesIO :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t0)))) -> (Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t1)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t1))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))
c_evalChildFamiliesIO x1 x2 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Traversal.c_mapChildrenIO(x1)))(Curry.Module.Prelude.pf(Curry.Module.Traversal.c_evalFamilyIO(x2)))(st)



c_concatFL :: (Curry t0) => (Curry.Module.Prelude.List (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0))) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_concatFL x1@Curry.Module.Prelude.List x2 st = x2
c_concatFL x1@((Curry.Module.Prelude.:<) x3 x4) x2 st = Curry.Module.Prelude.c_apply(x3)(Curry.Module.Traversal.c_concatFL(x4)(x2)(st))(st)
c_concatFL (Curry.Module.Prelude.ListOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Traversal.c_concatFL(x)(x2)(st))(i)(xs)(st)
c_concatFL x x2 st = Curry.RunTimeSystem.patternFail("Traversal.concatFL")(x)



c_familyFL :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> t0)))) -> t0 -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_familyFL x1 x2 x3 st = (Curry.Module.Prelude.:<)(x2)(Curry.Module.Traversal.c_childFamiliesFL(x1)(x1)(x2)(x3)(st))



c_childFamiliesFL :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t0)))) -> (Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t1)))) -> t0 -> (Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1
c_childFamiliesFL x1 x2 x3 x4 st = Curry.Module.Traversal.c_concatFL(Curry.Module.Prelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Traversal.c_familyFL(x2)))(Curry.Module.Prelude.c_apply(Curry.Module.Traversal.c_children(x1)(st))(x3)(st))(st))(x4)(st)



c_liftIO :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1)) -> (Curry.Module.Prelude.C_IO t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1
c_liftIO x1 x2 st = Curry.Module.Prelude.op_62_62_61(x2)(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(x1)(st))(st)


