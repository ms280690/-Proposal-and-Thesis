{-# LANGUAGE 	DeriveDataTypeable, 
				ViewPatterns, 
				ScopedTypeVariables, 
				FlexibleInstances, 
				DefaultSignatures,
				TypeOperators,
				FlexibleContexts,
				TypeFamilies
				#-}

--import Syntax


import Data.Generics (Data(..), Typeable(..))	
import Data.Functor.Fixedpoint as DFF
import Data.Traversable as T
import Data.Foldable
import Control.Applicative ((<$>),(<*>),pure,Applicative)
import Control.Unification

{--
data PrologTerm t = PrologTerm t deriving Show

newtype P p = P {unP :: Fix (PrologTerm p)} deriving Show
--}

type Atom = String

data VariableName = VariableName Int String deriving Show

data NullTerm = NullTerm deriving Show

data Term = 
			Struct Atom [Term]
--		| 	Wildcard
--		|	Cut Int
		|	Var VariableName
	deriving Show

--data PrologTerm v t = TermT t deriving Show

--newtype PT1 t = PT1 { unPT1 :: Fix (PrologTerm t) } deriving Show

data Prolog n t = PNull n | PTerm t deriving Show

--newtype Fix p = Fix {unFix :: DFF.Fix (Prolog p)} deriving Show

newtype P1 n = P1 {unP1 :: Fix (Prolog n) } deriving Show
{--
instance Functor (PrologTerm v) where
	fmap f x = case x of
		TermT t 	-> case t of
					Struct a (z:zs)		-> TermT $ Struct a (f z) : (zs)
					Wildcard			-> TermT $ Wildcard	 
					Cut i 			    -> TermT $ Cut i	
		VarT v		-> VarT v 			
--}

instance Functor (Prolog v) where
	fmap 	= T.fmapDefault
			
instance Foldable (Prolog v) where
	foldMap = T.foldMapDefault 
{--
instance Traversable (PrologTerm v) where
	traverse f x = case x of
--		TermT (Struct a (z:zs))		-> 	
		TermT w						-> pure (TermT x) 		
		VarT  v  					-> pure (VarT v) 
--}			

{--
instance Traversable (PrologTerm v) where
	sequenceA x = case x of
		TermT (Struct a (z:zs))		-> TermT (Struct a (liftA2 (:) (z) (sequenceA zs))) 
--}

instance Traversable (Prolog n) where
	traverse f (PTerm x) = PTerm <$> f x 
	traverse f (PNull n) = pure (PNull n)

--instance Unifiable (Prolog t) where
--	zipMatch t1 t2 = 

type UnFix v = Prolog v (Fix (Prolog v))
type UnP1 v = Fix (Prolog v)

{--
instance Functor P1 where
		fmap = T.fmapDefault

instance Foldable P1 where
 		foldMap = T.foldMapDefault  	

instance Traversable P1 where
 			traverse f p =
--}

q :: x -> [x]
q x = [x]




