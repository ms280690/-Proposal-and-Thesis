{-# LANGUAGE 	DeriveDataTypeable, 
				ViewPatterns, 
				ScopedTypeVariables, 
				FlexibleInstances, 
				DefaultSignatures,
				TypeOperators,
				FlexibleContexts,
				TypeFamilies,
				DataKinds
#-}

import Data.Generics (Data(..), Typeable(..))	
import Data.Functor.Fixedpoint as DFF
import Data.Traversable as T
import Data.Foldable
import Control.Applicative ((<$>),(<*>),pure,Applicative)
import Control.Unification
import Data.List.Extras.Pair

-- As per the changes recommended by Dr.Casperson after the 
-- meeting on Friday 24th April 2015.

type Atom = String

data VariableName = VariableName Int String deriving Show

data FlatTerm a = 
		Struct Atom [a]
	| 	Var VariableName 
	|	Wildcard
	|	Cut Int deriving Show

data Prolog = P (Fix FlatTerm) deriving Show

instance Functor (FlatTerm) where
	fmap 	= T.fmapDefault

instance Foldable (FlatTerm) where
 	foldMap = T.foldMapDefault

instance Traversable (FlatTerm) where
  	traverse f (Struct atom x)		=	Struct atom <$> sequenceA (map f x)
  	traverse _ (Var v)				=	pure (Var v)
  	traverse _ Wildcard				=	pure (Wildcard)
  	traverse _ (Cut i)				= 	pure (Cut i)

instance Unifiable (FlatTerm) where
	zipMatch (Struct al ls) (Struct ar rs) = 
		if (al == ar) && (length ls == length rs) 
			then Struct al <$> pairWith (\l r -> Right (l,r)) ls rs  		
			else Nothing
--	zipMatch (Var (VariableName i1 s1)) (Var (VariableName i2 s2)) = if (i1 == i2 && s1 == s2) 
--		then (Just (Var $ VariableName i1 s1))
--		else (Just (Struct "" [Right (Var (VariableName i1 s1), Var (VariableName i2 s2))]))	 
--		else Nothing
	zipMatch Wildcard _ = Just Wildcard
	zipMatch _ Wildcard = Just Wildcard
	zipMatch (Cut i1) (Cut i2) = if (i1 == i2) 
		then Just (Cut i1) 
--		else Just (Struct "" [Right ((Cut i1), (Cut i2))])		
		else Nothing

{--
instance Unifiable FlatTerm where
	zipMatch (Struct al ls) x = case x of 
		(Struct ar rs) -> if (al == ar) && (length ls == length rs) 
			then Struct al <$> pairWith (\l r -> Right (l,r)) ls rs  		
			else Nothing
		x -> Just (Struct " " <$> [(Right ((Struct al ls), x))])	 
		otherwise -> Nothing 
--}









--		else  Struct " " <$> pairWith (\l r -> Right (l,r)) [(Var (VariableName i1 s1))] 
--[(Var (VariableName i2 s2))]
		--(Right (Var (VariableName i1 s1),Var (VariableName i2 s2)))]  
		
--	zipMatch (Var (VariableName i1 s1)) (Var (VariableName i2 s2)) = if i1 == i2 then 
--		Just (Var $ VariableName i1 s1, ) else Nothing
--	zipMatch _ _ = Nothing

--unifiableMap :: (t -> t -> t) -> [t] -> [t] -> [t]
--unifiableMap f x y 
--				| length x == length y = 			

{--
instance Functor Prolog where
	fmap 	= T.fmapDefault

instance Foldable Prolog where
	foldMap = T.foldMapDefault

instance Traversable Prolog where
	traverse f (P x) = P <$> (f x)
--}


--myfmap f (Struct a (x : xs))	=	Struct a $ (f x) : (myfmap f xs)
--myfmap f (Var v)				= 	Var v 

q :: x -> [x]
q x = [x]



----------------- TRANSLATE FUNCTION -------------------------
--------------------------------------------------------------
--translateToUTerm