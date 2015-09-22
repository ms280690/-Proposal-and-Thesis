module Zurg where

import Data.List (nub, (\\))
-- For Haskell 98 the module is List, but for Haskell 2010, use module Data.List.


-- TOY 		TIME
-- Buzz 	5
-- Woody	10
-- Rex		20
-- Hamm		25

-- Total time bound is 60.
-- Toys move from left to right.

type Time = Int
type Toy  = Time
type Toys = [Toy]


-- The maximum number of crossings is limited to 5.
-- Direction	No.of toys crossing the bridge			Total no.of toys on the right side of the bridge. 
-- Left  		2 									 	(2)
-- Right 		1 										(1)
-- Left  		2 										(3)
-- Right 		1 										(2)
-- Left  		2 										(4) 

type Crossing = (Toys,Toy,Toys,Toy,Toys)

-- Times, Buzz, Woody, Rex, Hamm.
toys :: Toys
toys = [5,10,20,25]

-- ??
fwd :: Toys -> [(Time,Toys,Toys)]
fwd g = [ (max x y,[x,y],g'\\[y]) | x <- g, let g'=g\\[x], y <- g', x<y]

{--


--}

bwd :: Toys -> [(Time,Toys)]
bwd g = [ (x,g++[x]) | x <- toys\\g]
{--
x <- [5,10,20,25]\\g
(difference, g++difference)
--}



cross :: Toys -> [(Time,Crossing)]
cross g = [ (t1+t2+t3+t4+max x y,(ts1,t2,ts3,t4,g4)) |
            (t1,ts1,g1)   <- fwd g,
            (t2,g2)       <- bwd g1,
            (t3,ts3,g3)   <- fwd g2,
            (t4,g4@[x,y]) <- bwd g3
          ]

solution :: [Crossing]
solution = [ c | (t,c) <- cross toys, t<=60]
