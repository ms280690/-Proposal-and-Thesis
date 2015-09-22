{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Maybe (module Curry.Module.Maybe) where

import Curry.RunTimeSystem
import Curry.Module.Prelude



-- begin included



-- end included

c_isJust :: (Curry t0) => (Curry.Module.Prelude.C_Maybe t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isJust x1@(Curry.Module.Prelude.C_Just x2) st = Curry.Module.Prelude.C_True
c_isJust x1@Curry.Module.Prelude.C_Nothing st = Curry.Module.Prelude.C_False
c_isJust (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Maybe.c_isJust(x)(st))(i)(xs)(st)
c_isJust x st = Curry.RunTimeSystem.patternFail("Maybe.isJust")(x)



c_isNothing :: (Curry t0) => (Curry.Module.Prelude.C_Maybe t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isNothing x1@Curry.Module.Prelude.C_Nothing st = Curry.Module.Prelude.C_True
c_isNothing x1@(Curry.Module.Prelude.C_Just x2) st = Curry.Module.Prelude.C_False
c_isNothing (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Maybe.c_isNothing(x)(st))(i)(xs)(st)
c_isNothing x st = Curry.RunTimeSystem.patternFail("Maybe.isNothing")(x)



c_fromJust :: (Curry t0) => (Curry.Module.Prelude.C_Maybe t0) -> Curry.RunTimeSystem.State -> t0
c_fromJust x1@(Curry.Module.Prelude.C_Just x2) st = x2
c_fromJust x1@Curry.Module.Prelude.C_Nothing st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('J'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))(Curry.Module.Prelude.List))))))))))))))))))))))))(st)
c_fromJust (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Maybe.c_fromJust(x)(st))(i)(xs)(st)
c_fromJust x st = Curry.RunTimeSystem.patternFail("Maybe.fromJust")(x)



c_fromMaybe :: (Curry t0) => t0 -> (Curry.Module.Prelude.C_Maybe t0) -> Curry.RunTimeSystem.State -> t0
c_fromMaybe x1 x2@Curry.Module.Prelude.C_Nothing st = x1
c_fromMaybe x1 x2@(Curry.Module.Prelude.C_Just x3) st = x3
c_fromMaybe x1 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Maybe.c_fromMaybe(x1)(x)(st))(i)(xs)(st)
c_fromMaybe x1 x st = Curry.RunTimeSystem.patternFail("Maybe.fromMaybe")(x)



c_maybeToList :: (Curry t0) => (Curry.Module.Prelude.C_Maybe t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_maybeToList x1@Curry.Module.Prelude.C_Nothing st = Curry.Module.Prelude.List
c_maybeToList x1@(Curry.Module.Prelude.C_Just x2) st = (Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)
c_maybeToList (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Maybe.c_maybeToList(x)(st))(i)(xs)(st)
c_maybeToList x st = Curry.RunTimeSystem.patternFail("Maybe.maybeToList")(x)



c_listToMaybe :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t0
c_listToMaybe x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_listToMaybe x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.Prelude.C_Just(x2)
c_listToMaybe (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Maybe.c_listToMaybe(x)(st))(i)(xs)(st)
c_listToMaybe x st = Curry.RunTimeSystem.patternFail("Maybe.listToMaybe")(x)



c_catMaybes :: (Curry t0) => (Curry.Module.Prelude.List (Curry.Module.Prelude.C_Maybe t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_catMaybes x1 st = Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Maybe.c_catMaybes'46_'35lambda4))(Curry.Module.Prelude.List)(x1)(st)



c_catMaybes'46_'35lambda4 :: (Curry t37) => (Curry.Module.Prelude.C_Maybe t37) -> (Curry.Module.Prelude.List t37) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t37
c_catMaybes'46_'35lambda4 x1@(Curry.Module.Prelude.C_Just x3) x2 st = (Curry.Module.Prelude.:<)(x3)(x2)
c_catMaybes'46_'35lambda4 x1@Curry.Module.Prelude.C_Nothing x2 st = x2
c_catMaybes'46_'35lambda4 (Curry.Module.Prelude.C_MaybeOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Maybe.c_catMaybes'46_'35lambda4(x)(x2)(st))(i)(xs)(st)
c_catMaybes'46_'35lambda4 x x2 st = Curry.RunTimeSystem.patternFail("Maybe.catMaybes._#lambda4")(x)



c_mapMaybe :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1)
c_mapMaybe x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Maybe.c_catMaybes))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(x1)))(st)



op_62_62_45 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.C_Maybe t0) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t1
op_62_62_45 x1@Curry.Module.Prelude.C_Nothing x2 st = Curry.Module.Prelude.C_Nothing
op_62_62_45 x1@(Curry.Module.Prelude.C_Just x3) x2 st = Curry.Module.Prelude.c_apply(x2)(x3)(st)
op_62_62_45 (Curry.Module.Prelude.C_MaybeOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Maybe.op_62_62_45(x)(x2)(st))(i)(xs)(st)
op_62_62_45 x x2 st = Curry.RunTimeSystem.patternFail("Maybe.>>-")(x)



c_sequenceMaybe :: (Curry t0) => (Curry.Module.Prelude.List (Curry.Module.Prelude.C_Maybe t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List t0)
c_sequenceMaybe x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Just(Curry.Module.Prelude.List)
c_sequenceMaybe x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.Maybe.op_62_62_45(x2)(Curry.Module.Prelude.pf(Curry.Module.Maybe.c_sequenceMaybe'46_'35lambda6(x3)))(st)
c_sequenceMaybe (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Maybe.c_sequenceMaybe(x)(st))(i)(xs)(st)
c_sequenceMaybe x st = Curry.RunTimeSystem.patternFail("Maybe.sequenceMaybe")(x)



c_sequenceMaybe'46_'35lambda6 :: (Curry t66) => (Curry.Module.Prelude.List (Curry.Module.Prelude.C_Maybe t66)) -> t66 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List t66)
c_sequenceMaybe'46_'35lambda6 x1 x2 st = Curry.Module.Maybe.op_62_62_45(Curry.Module.Maybe.c_sequenceMaybe(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.Maybe.c_sequenceMaybe'46_'35lambda6'46_'35lambda7(x2)))(st)



c_sequenceMaybe'46_'35lambda6'46_'35lambda7 :: (Curry t66) => t66 -> (Curry.Module.Prelude.List t66) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List t66)
c_sequenceMaybe'46_'35lambda6'46_'35lambda7 x1 x2 st = Curry.Module.Prelude.C_Just((Curry.Module.Prelude.:<)(x1)(x2))



c_mapMMaybe :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List t1))
c_mapMMaybe x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Maybe.c_sequenceMaybe))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(x1)))(st)


