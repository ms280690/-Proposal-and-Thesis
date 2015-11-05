{-# LANGUAGE 	DeriveDataTypeable, 
				ViewPatterns, 
				ScopedTypeVariables, 
				FlexibleInstances, 
				DefaultSignatures,
				TypeOperators,
				FlexibleContexts,
				TypeFamilies,
				DataKinds,
				OverlappingInstances,
				DataKinds,
				PolyKinds,
				TypeOperators,
				LiberalTypeSynonyms,
				TemplateHaskell,
				RankNTypes,
				AllowAmbiguousTypes
				#-}
module STVarTest where

import qualified Control.Unification as U
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Control.Unification.IntVar as IV
import qualified Control.Unification.STVar as ST
import Control.Monad.Identity (runIdentity,Identity)
import Control.Monad.Error --(ErrorT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Error (runErrorT)
import Control.Unification.Types as UT

import Control.Unification.Types as UT
----------------------------------------------------------------------
import Control.Monad.Trans.Except
----------------------------------------
import STVarExperiment



import Data.Generics (Data(..), Typeable(..))	
import Data.Functor.Fixedpoint as DFF
import Data.Traversable as T
import Data.Foldable as DF
import Control.Applicative ((<$>),(<*>),pure,Applicative)
import Data.List.Extras.Pair

-- Playing with STVar(STRef)
import Control.Unification
import Control.Unification.Types as UT
import Control.Unification.STVar as ST 
--import Data.STRef
--import Control.Monad.Fix
--import Control.Monad.Trans.Class
--import Control.Monad.Identity as I
import Control.Monad.Error

import qualified Data.Set as S
import Data.Map as Map
--import Control.Monad.ST
--import Flatten

import Control.Monad.Trans.Except


--instance (UT.Variable v, Functor t) => Error (UT.UFailure t v) where {}


test1 ::
  ErrorT (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
           (ST.STBinding s)
            (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
             Map VariableName (ST.STVar s (FlatTerm)))
test1 = do
    let
        t1a = (Fix $ Var $ VariableName 0 "x")
        t2a = (Fix $ Var $ VariableName 1 "y")
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- unify x1 x2
    return (x3, d1 `Map.union` d2)
{--
test2 ::
  ErrorT (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
           (ST.STBinding s)
            (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
             Map VariableName (ST.STVar s (FlatTerm)))
test2 = do
    let
        t1a = (Fix $ Struct "a" [(Fix $ Struct "b" [Fix $ Var $ VariableName 0 "x"]), (Fix $ Var $ VariableName 2 "z")])
        t2a = (Fix $ Var $ VariableName 1 "y")
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- unify x1 x2
    return (x3, d1 `Map.union` d2)

test3 ::
  ErrorT (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
           (ST.STBinding s)
            (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
             Map VariableName (ST.STVar s (FlatTerm)))
test3 = do
    let
        t1a = (Fix Wildcard)
        t2a = (Fix $ Var $ VariableName 1 "y")
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- unify x1 x2
    return (x3, d1 `Map.union` d2)

{--
--test4 :: ErrorT (ST.STBinding s) (UT.UTerm FlatTerm (ST.STVar s FlatTerm),
--                                  Map VariableName (ST.STVar s FlatTerm))
test4 :: 
	ErrorT (ST.STBinding s) (UT.UTerm FlatTerm (ST.STVar s FlatTerm),
                                  Map VariableName (ST.STVar s FlatTerm))
test4 = do
	let t = (Fix $ Struct "a" [(Fix $ Struct "b" [Fix $ Var $ VariableName 0 "x"]), (Fix $ Var $ VariableName 2 "z")])
	(x1,d1)	<- lift . translateToUTerm $ t
	return (x1,d1)
--}
--}
goTest :: (Show b) => (forall s . (ErrorT (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
           (ST.STBinding s)
            (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
             Map VariableName (ST.STVar s (FlatTerm))))) -> String

goTest test = ST.runSTBinding $ do
  answer <- runErrorT $ test
  return $! case answer of
    (Left x)  -> "error: " ++ show x
    (Right y) -> "ok:    " ++ show y

{--
{--
translateToUTermTest = ST.runSTBinding $ do
	answer <- runErrorT $ lift . translateToUTerm $ (Fix $ Struct "a" [(Fix $ Struct "b" [Fix $ Var $ VariableName 0 "x"]), (Fix $ Var $ VariableName 2 "z")])
	return answer
--}	

f1
  :: (MonadTrans t, Monad (t (STBinding s))) => t (STBinding s) (UTerm FlatTerm (STVar s FlatTerm),
                         Map VariableName (STVar s FlatTerm))
f1 = do
	(a,b) <- lift . translateToUTerm $ (Fix Wildcard)
	return (a,b)
{--
gof1
  :: (Error e) =>
     Fix FlatTerm
     -> STBinding
          s
          (Either
             e
             (UTerm FlatTerm (STVar s FlatTerm),
              Map VariableName (STVar s FlatTerm)))

gof1 = ST.runSTBinding $ do
	a <- runErrorT $ f1 
	return $! case a of
		(Left x)  -> "error: " ++ show x
		(Right y) -> "ok:    " ++ show y
--}
--}