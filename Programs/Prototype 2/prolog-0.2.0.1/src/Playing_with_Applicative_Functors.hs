
-- Mehul Solanki.

-- Playing with Applicative Functors.

-- An applicative functor has more structure than a functor but less than a monad.

import Control.Monad
import Control.Applicative


data Tree a =  Leaf a | Node (Tree a) (Tree a) deriving (Show,Eq)


instance Monad Tree where
 		
	return a = Leaf a
 
	(Leaf a) >>= f = f a
	(Node left right) >>= f = Node (left >>= f) (right >>= f)

join :: Tree (Tree a) -> Tree a
join (Leaf t) = t
join (Node left right) = Node (Main.join left) (Main.join right)


instance Functor Tree where

	fmap f (Leaf a) = Leaf (f a)
	fmap f (Node left right) = Node (fmap f left) (fmap f right)


instance Applicative Tree where
	pure = Leaf
	(Leaf f) <*> (Leaf x) = Leaf (f x)
	(Leaf f) <*> (Node left right) = Node ((Leaf f) <*> left) ((Leaf f) <*> right)
--	(Node left right) <*> (Leaf f) = Node ((Leaf f) <*> left) ((Leaf f) <*> right)
--	(Node left1 right1) <*> (Node left2 right2) = Node (left1 <*> left2) (right1 <*> right2)

