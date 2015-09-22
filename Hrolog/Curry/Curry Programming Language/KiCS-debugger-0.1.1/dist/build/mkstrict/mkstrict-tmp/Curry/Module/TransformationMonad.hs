{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.TransformationMonad (module Curry.Module.TransformationMonad) where

import Curry.RunTimeSystem
import Curry.Module.Prelude
import Curry.Module.SrcRef
import Curry.Module.Maybe



-- begin included



-- end included

type C_TMState = Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))

type C_TM t0 = (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) t0

c_ret :: (Curry t0) => t0 -> (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) t0
c_ret x1 x2 st = Curry.Module.Prelude.T2(x2)(x1)



op_62_62_61_46 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) t0)) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) t1))) -> (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) t1
op_62_62_61_46 x1 x2 x3 st = Curry.Module.TransformationMonad.c_'62'62'61'46_case_9(x1)(x2)(x3)(Curry.Module.Prelude.c_apply(x1)(x3)(st))(st)



op_62_62_46 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) t0)) -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) t1)
op_62_62_46 x1 x2 st = Curry.Module.Prelude.pf(Curry.Module.TransformationMonad.op_62_62_61_46(x1)(Curry.Module.Prelude.pf(Curry.Module.TransformationMonad.c_'62'62'46'46_'35lambda3(x2))))



c_'62'62'46'46_'35lambda3 :: (Curry t24,Curry t25) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) t24)) -> t25 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) t24)
c_'62'62'46'46_'35lambda3 x1 x2 st = x1



c_runVar :: (Curry t0) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) t0)) -> Curry.RunTimeSystem.State -> t0
c_runVar x1 x2 st = Curry.Module.Prelude.c_snd(Curry.Module.Prelude.c_apply(x2)(Curry.Module.Prelude.T2(x1)(Curry.Module.Prelude.C_Nothing))(st))(st)



c_run :: (Curry t0) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) t0)) -> Curry.RunTimeSystem.State -> t0)
c_run st = Curry.Module.Prelude.pf(Curry.Module.TransformationMonad.c_runVar(Curry.Module.Prelude.C_Zero))



c_state :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool)))
c_state x1 st = Curry.Module.Prelude.T2(x1)(x1)



c_freshVar :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) Curry.Module.Prelude.C_Int
c_freshVar x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.Prelude.T2(Curry.Module.Prelude.T2(Curry.Module.Prelude.op_43(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x3))(x2)
c_freshVar (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationMonad.c_freshVar(x)(st))(i)(xs)(st)
c_freshVar x st = Curry.RunTimeSystem.patternFail("TransformationMonad.freshVar")(x)



c_freshVars :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)
c_freshVars x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.Prelude.T2(Curry.Module.Prelude.T2(Curry.Module.Prelude.op_43(x3)(x1)(st))(x4))(Curry.Module.Prelude.c_take(x1)(Curry.Module.TransformationMonad.c_freshVars'46from'4623(x3)(st))(st))
c_freshVars x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationMonad.c_freshVars(x1)(x)(st))(i)(xs)(st)
c_freshVars x1 x st = Curry.RunTimeSystem.patternFail("TransformationMonad.freshVars")(x)



c_freshVars'46from'4623 :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_freshVars'46from'4623 x1 st = (Curry.Module.Prelude.:<)(x1)(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.TransformationMonad.c_freshVars'46from'4623))(Curry.Module.Prelude.op_43(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))



c_setInfoTree :: (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) -> (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) Curry.Module.Prelude.T0
c_setInfoTree x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.Prelude.T2(Curry.Module.Prelude.T2(x3)(Curry.Module.Prelude.C_Just(Curry.Module.Prelude.T2(x1)(Curry.Module.Prelude.C_True))))(Curry.Module.Prelude.T0)
c_setInfoTree x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationMonad.c_setInfoTree(x1)(x)(st))(i)(xs)(st)
c_setInfoTree x1 x st = Curry.RunTimeSystem.patternFail("TransformationMonad.setInfoTree")(x)



c_currentInfoTree :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)))
c_currentInfoTree x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.TransformationMonad.c_currentInfoTree_case_8(x1)(x3)(st)
c_currentInfoTree (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationMonad.c_currentInfoTree(x)(st))(i)(xs)(st)
c_currentInfoTree x st = Curry.RunTimeSystem.patternFail("TransformationMonad.currentInfoTree")(x)



c_nextSrcRefs :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)
c_nextSrcRefs x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.TransformationMonad.c_nextSrcRefs_case_6(x2)(x3)(st)
c_nextSrcRefs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationMonad.c_nextSrcRefs(x)(st))(i)(xs)(st)
c_nextSrcRefs x st = Curry.RunTimeSystem.patternFail("TransformationMonad.nextSrcRefs")(x)



c_nextSrcRefs'46_'35selFP3'35srcRefs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_nextSrcRefs'46_'35selFP3'35srcRefs x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_nextSrcRefs'46_'35selFP3'35srcRefs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationMonad.c_nextSrcRefs'46_'35selFP3'35srcRefs(x)(st))(i)(xs)(st)
c_nextSrcRefs'46_'35selFP3'35srcRefs x st = Curry.RunTimeSystem.patternFail("TransformationMonad.nextSrcRefs._#selFP3#srcRefs")(x)



c_nextSrcRefs'46_'35selFP4'35nextInfoTree :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))
c_nextSrcRefs'46_'35selFP4'35nextInfoTree x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_nextSrcRefs'46_'35selFP4'35nextInfoTree (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationMonad.c_nextSrcRefs'46_'35selFP4'35nextInfoTree(x)(st))(i)(xs)(st)
c_nextSrcRefs'46_'35selFP4'35nextInfoTree x st = Curry.RunTimeSystem.patternFail("TransformationMonad.nextSrcRefs._#selFP4#nextInfoTree")(x)



c_ignoreVarRefs :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) Curry.Module.Prelude.T0
c_ignoreVarRefs x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.TransformationMonad.c_ignoreVarRefs_case_4(x2)(x3)(st)
c_ignoreVarRefs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationMonad.c_ignoreVarRefs(x)(st))(i)(xs)(st)
c_ignoreVarRefs x st = Curry.RunTimeSystem.patternFail("TransformationMonad.ignoreVarRefs")(x)



c_skipNextVar :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) Curry.Module.Prelude.T0
c_skipNextVar x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.TransformationMonad.c_skipNextVar_case_1(x2)(x3)(st)
c_skipNextVar (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationMonad.c_skipNextVar(x)(st))(i)(xs)(st)
c_skipNextVar x st = Curry.RunTimeSystem.patternFail("TransformationMonad.skipNextVar")(x)



c_sequence :: (Curry t0) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) t0))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) (Curry.Module.Prelude.List t0)))
c_sequence st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.TransformationMonad.c_sequence'46mcons'4647))(Curry.Module.Prelude.pf(Curry.Module.TransformationMonad.c_ret(Curry.Module.Prelude.List))))



c_sequence'46mcons'4647 :: (Curry t0) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) t0)) -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) (Curry.Module.Prelude.List t0))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) (Curry.Module.Prelude.List t0))
c_sequence'46mcons'4647 x1 x2 st = Curry.Module.Prelude.pf(Curry.Module.TransformationMonad.op_62_62_61_46(x1)(Curry.Module.Prelude.pf(Curry.Module.TransformationMonad.c_sequence'46mcons'4647'46_'35lambda4(x2))))



c_sequence'46mcons'4647'46_'35lambda4 :: (Curry t88) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) (Curry.Module.Prelude.List t88))) -> t88 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) (Curry.Module.Prelude.List t88))
c_sequence'46mcons'4647'46_'35lambda4 x1 x2 st = Curry.Module.Prelude.pf(Curry.Module.TransformationMonad.op_62_62_61_46(x1)(Curry.Module.Prelude.pf(Curry.Module.TransformationMonad.c_sequence'46mcons'4647'46_'35lambda4'46_'35lambda5(x2))))



c_sequence'46mcons'4647'46_'35lambda4'46_'35lambda5 :: (Curry t88) => t88 -> (Curry.Module.Prelude.List t88) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) (Curry.Module.Prelude.List t88))
c_sequence'46mcons'4647'46_'35lambda4'46_'35lambda5 x1 x2 st = Curry.Module.Prelude.pf(Curry.Module.TransformationMonad.c_ret((Curry.Module.Prelude.:<)(x1)(x2)))



c_skipNextVar_case_1 x2 x3@(Curry.Module.Prelude.C_Just x4) st = Curry.Module.TransformationMonad.c_skipNextVar_case_0(x2)(x4)(st)
c_skipNextVar_case_1 x2 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationMonad.c_skipNextVar_case_1(x2)(x)(st))(i)(xs)(st)
c_skipNextVar_case_1 x2 x st = Curry.RunTimeSystem.patternFail("TransformationMonad.skipNextVar_case_1")(x)



c_skipNextVar_case_0 x2 x4@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.Prelude.T2(Curry.Module.Prelude.T2(x2)(Curry.Module.Prelude.C_Just(Curry.Module.Prelude.T2(x5)(Curry.Module.Prelude.C_False))))(Curry.Module.Prelude.T0)
c_skipNextVar_case_0 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationMonad.c_skipNextVar_case_0(x2)(x)(st))(i)(xs)(st)
c_skipNextVar_case_0 x2 x st = Curry.RunTimeSystem.patternFail("TransformationMonad.skipNextVar_case_0")(x)



c_ignoreVarRefs_case_4 x2 x3@(Curry.Module.Prelude.C_Just x4) st = Curry.Module.TransformationMonad.c_ignoreVarRefs_case_3(x2)(x4)(st)
c_ignoreVarRefs_case_4 x2 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationMonad.c_ignoreVarRefs_case_4(x2)(x)(st))(i)(xs)(st)
c_ignoreVarRefs_case_4 x2 x st = Curry.RunTimeSystem.patternFail("TransformationMonad.ignoreVarRefs_case_4")(x)



c_ignoreVarRefs_case_3 x2 x4@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.Prelude.T2(Curry.Module.Prelude.T2(x2)(Curry.Module.Prelude.C_Just(Curry.Module.Prelude.T2(Curry.Module.TransformationMonad.c_ignoreVarRefs_case_2(x5)(x6)(st))(Curry.Module.Prelude.C_True))))(Curry.Module.Prelude.T0)
c_ignoreVarRefs_case_3 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationMonad.c_ignoreVarRefs_case_3(x2)(x)(st))(i)(xs)(st)
c_ignoreVarRefs_case_3 x2 x st = Curry.RunTimeSystem.patternFail("TransformationMonad.ignoreVarRefs_case_3")(x)



c_ignoreVarRefs_case_2 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))(Curry.Module.SrcRef.c_nextStaticInfo(x5)(st))(st)
c_ignoreVarRefs_case_2 x5 x6@Curry.Module.Prelude.C_False st = x5
c_ignoreVarRefs_case_2 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationMonad.c_ignoreVarRefs_case_2(x5)(x)(st))(i)(xs)(st)
c_ignoreVarRefs_case_2 x5 x st = Curry.RunTimeSystem.patternFail("TransformationMonad.ignoreVarRefs_case_2")(x)



c_nextSrcRefs_case_6 x2 x3@(Curry.Module.Prelude.C_Just x4) st = Curry.Module.TransformationMonad.c_nextSrcRefs_case_5(x2)(x4)(st)
c_nextSrcRefs_case_6 x2 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationMonad.c_nextSrcRefs_case_6(x2)(x)(st))(i)(xs)(st)
c_nextSrcRefs_case_6 x2 x st = Curry.RunTimeSystem.patternFail("TransformationMonad.nextSrcRefs_case_6")(x)



c_nextSrcRefs_case_5 x2 x4@(Curry.Module.Prelude.T2 x5 x6) st = let {x7 = Curry.Module.SrcRef.c_nextStaticInfo(x5)(st)} in Curry.Module.Prelude.T2(Curry.Module.Prelude.T2(x2)(Curry.Module.Prelude.C_Just(Curry.Module.Prelude.T2(Curry.Module.TransformationMonad.c_nextSrcRefs'46_'35selFP4'35nextInfoTree(x7)(st))(Curry.Module.Prelude.C_True))))(Curry.Module.TransformationMonad.c_nextSrcRefs'46_'35selFP3'35srcRefs(x7)(st))
c_nextSrcRefs_case_5 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationMonad.c_nextSrcRefs_case_5(x2)(x)(st))(i)(xs)(st)
c_nextSrcRefs_case_5 x2 x st = Curry.RunTimeSystem.patternFail("TransformationMonad.nextSrcRefs_case_5")(x)



c_currentInfoTree_case_8 x1 x3@(Curry.Module.Prelude.C_Just x4) st = Curry.Module.TransformationMonad.c_currentInfoTree_case_7(x1)(x4)(st)
c_currentInfoTree_case_8 x1 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationMonad.c_currentInfoTree_case_8(x1)(x)(st))(i)(xs)(st)
c_currentInfoTree_case_8 x1 x st = Curry.RunTimeSystem.patternFail("TransformationMonad.currentInfoTree_case_8")(x)



c_currentInfoTree_case_7 x1 x4@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.Prelude.T2(x1)(x5)
c_currentInfoTree_case_7 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationMonad.c_currentInfoTree_case_7(x1)(x)(st))(i)(xs)(st)
c_currentInfoTree_case_7 x1 x st = Curry.RunTimeSystem.patternFail("TransformationMonad.currentInfoTree_case_7")(x)



c_'62'62'61'46_case_9 x1 x2 x3 (Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x2)(x5)(st))(x4)(st)
c_'62'62'61'46_case_9 x1 x2 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationMonad.c_'62'62'61'46_case_9(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_'62'62'61'46_case_9 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("TransformationMonad.>>=._case_9")(x)


