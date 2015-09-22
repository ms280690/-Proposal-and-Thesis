module Curry.Compiler.Brace where

import Data.List

separate :: [a] -> [[a]] -> [a]
separate s xs = concat (intersperse s (filter (not . null) xs))

brace :: [a] -> [a] -> [a] -> [[a]] -> [a] 
brace _ _ _ [] = []
brace begin end sep xs = begin++separate sep xs++end
