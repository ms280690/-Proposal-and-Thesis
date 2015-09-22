module ZurgStrat where

import SearchStrat
import Data.List( (\\), delete, sort)
-- For Haskell 98 the module is List, but for Haskell 2010, use module Data.List.

-- Moreover, some Flags need to be set in order for it to work,
-- For Multi Parameter Type Classes (SearchProblem), -XMultiParamTypeClasses.
-- For Instance Declarations without Type Synonyms, -XTypeSynonymInstances.
-- For allowing multiple apprearances of the same variable in an instance head, -XFlexibleInstances.


data Toy = Buzz | Hamm | Rex | Woody  deriving (Eq,Ord,Show)
data Pos = L | R                      deriving (Eq,Show)
type Group = [Toy]
type BridgePos = (Pos,Group)
type Move = Either Toy Group

toys :: [Toy]
toys = [Buzz,Hamm,Rex,Woody]

time :: Toy -> Int
time Buzz  = 5
time Woody = 10
time Rex   = 20
time Hamm  = 25

duration :: [Move] -> Int
duration = sum . map (either time (maximum . map time))

backw :: Group -> [(Move,BridgePos)]
backw xs = [(Left x,(L,sort (x:(toys \\ xs)))) | x <- xs]

forw :: Group -> [(Move,BridgePos)]
forw xs = [(Right [x,y],(R,delete y ys)) | 
              x <- xs,let ys=delete x xs, y <- ys, x<y]

instance SearchProblem BridgePos Move where
  trans (L,l)  = forw l
  trans (R,l)  = backw (toys \\ l)
  isSolution (ms,s) = s == (R,[]) && duration ms <= 60

solution :: [Move]
(solution,_):_ = solutions bfs (L,toys)

-- Running
{--

Run ZurgStrat.hs

solution
[Right [Buzz,Woody],Left Buzz,Right [Hamm,Rex],Left Woody,Right [Buzz,Woody]]

--}