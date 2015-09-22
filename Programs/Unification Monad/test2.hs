
-- Mehul Solanki.

-- test2 based on unification-fd tutorial.

import Data.Foldable

import qualified Data.Traversable as T

import Control.Applicative

import Data.Monoid

data T a = T String [a]

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Show, Read, Eq)
{--
instance Functor Tree where
	fmap f Empty = Empty
	fmap f (Leaf a) = Leaf (f a) 
	fmap f (Node l a r) = Node (fmap f l) (f a) (fmap f r)

instance Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Leaf x) = f x
    foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r
--}

instance Functor Tree where
	fmap = T.fmapDefault 

instance Foldable Tree where
	foldMap = T.foldMapDefault

instance T.Traversable Tree where
    traverse f Empty = pure Empty
    traverse f (Leaf x) = Leaf <$> f x
    traverse f (Node l k r) = Node <$> (T.traverse f l) <*> f k <*> (T.traverse f r)
