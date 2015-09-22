{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.RandomExternal (module Curry.Module.RandomExternal) where

import Curry.RunTimeSystem
import Curry.Module.Prelude



-- begin included



import System.Random

prim_split :: C_Int -> Result (T2 C_Int C_Int)
prim_split seed _ = toCurry (fst (next g1), fst (next g2))
 where
  (g1,g2) = split (mkStdGen (fromCurry seed))

prim_nextInt :: C_Int -> Result (List C_Int)
prim_nextInt seed _ = toCurry (nextInt (fromCurry seed))

nextInt :: Int -> [Int]
nextInt = randoms . mkStdGen

prim_nextIntRange :: C_Int -> C_Int -> Result (List C_Int)
prim_nextIntRange seed bound _ =
  toCurry (nextIntRange (fromCurry seed) (fromCurry bound))

nextIntRange :: Int -> Int -> [Int]
nextIntRange seed bound = randomRs (0,bound) (mkStdGen seed)

getRandomSeed :: Result (C_IO C_Int)
getRandomSeed = ioFunc0 (getStdRandom next)



-- end included

c_split :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_split x1 st = let {x2 = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.RandomExternal.c_prim_split))(x1)(st)} in (Curry.Module.Prelude.:<)(Curry.Module.RandomExternal.c_split'46_'35selFP3'35s1(x2)(st))(Curry.Module.RandomExternal.c_split(Curry.Module.RandomExternal.c_split'46_'35selFP4'35s2(x2)(st))(st))



c_split'46_'35selFP3'35s1 :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_split'46_'35selFP3'35s1 x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_split'46_'35selFP3'35s1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RandomExternal.c_split'46_'35selFP3'35s1(x)(st))(i)(xs)(st)
c_split'46_'35selFP3'35s1 x st = Curry.RunTimeSystem.patternFail("RandomExternal.split._#selFP3#s1")(x)



c_split'46_'35selFP4'35s2 :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_split'46_'35selFP4'35s2 x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_split'46_'35selFP4'35s2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.RandomExternal.c_split'46_'35selFP4'35s2(x)(st))(i)(xs)(st)
c_split'46_'35selFP4'35s2 x st = Curry.RunTimeSystem.patternFail("RandomExternal.split._#selFP4#s2")(x)



c_nextInt :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_nextInt x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.RandomExternal.c_prim_nextInt))(x1)(st)



c_nextIntRange :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_nextIntRange x1 x2 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.RandomExternal.c_prim_nextIntRange))(x1)(st))(x2)(st)



c_nextBoolean :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Bool
c_nextBoolean x1 st = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.C_Zero)))(Curry.Module.RandomExternal.c_nextIntRange(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st)



c_prim_split :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int
c_prim_split x1 st = Curry.Module.RandomExternal.prim_split(x1)(st)



c_prim_nextInt :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_prim_nextInt x1 st = Curry.Module.RandomExternal.prim_nextInt(x1)(st)



c_prim_nextIntRange :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_prim_nextIntRange x1 x2 st = Curry.Module.RandomExternal.prim_nextIntRange(x1)(x2)(st)



c_getRandomSeed :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Int
c_getRandomSeed st = Curry.Module.RandomExternal.getRandomSeed(st)


