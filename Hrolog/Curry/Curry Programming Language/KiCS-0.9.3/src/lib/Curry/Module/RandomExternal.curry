--- External (more efficient) implementation of random number generation.
--- 
--- @author sebf@informatik.uni-kiel.de
--- @version January 2008
---
module RandomExternal (

  split, nextInt, nextIntRange, nextBoolean,

  getRandomSeed

  ) where

--- Splits a random seed into an infinite list of uncorrelated random seeds.
---
split :: Int -> [Int]
split seed = s1 : split s2
 where (s1,s2) = prim_split $## seed

prim_split :: Int -> (Int,Int)
prim_split external

--- Computes an infinite list of random integers.
---
nextInt :: Int -> [Int]
nextInt seed = prim_nextInt $## seed

prim_nextInt :: Int -> [Int]
prim_nextInt external

--- Computes an infinite list of random integers between 0 and given bound.
--- Both 0 and the given bound may occur in the result.
---
nextIntRange :: Int -> Int -> [Int]
nextIntRange seed bound = (prim_nextIntRange $## seed) $## bound

prim_nextIntRange :: Int -> Int -> [Int]
prim_nextIntRange external

--- Computes an infinite list of random booleans.
---
nextBoolean :: Int -> [Bool]
nextBoolean seed = map (0==) (nextIntRange seed 1)

--- Computes an initial random seed.
---
getRandomSeed :: IO Int
getRandomSeed external


