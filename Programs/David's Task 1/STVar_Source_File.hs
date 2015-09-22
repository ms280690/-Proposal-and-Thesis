
-- STVar Source File.


{-# LANGUAGE CPP
           , Rank2Types
           , MultiParamTypeClasses
           , UndecidableInstances
           , FlexibleInstances
           , MagicHash
           #-}
{-# OPTIONS_GHC -Wall -fwarn-tabs #-}
----------------------------------------------------------------
--                                                  ~ 2015.03.29
-- |
-- Module      :  Control.Unification.STVar
-- Copyright   :  Copyright (c) 2007--2015 wren gayle romano
-- License     :  BSD
-- Maintainer  :  wren@community.haskell.org
-- Stability   :  experimental
-- Portability :  semi-portable (Rank2Types, MPTCs,...)
--
-- This module defines an implementation of unification variables
-- using the 'ST' monad.
----------------------------------------------------------------
module STVar_Source_File where 
--    ( STVar()
--    , STBinding()
--    , runSTBinding
--    , _newSTVar
--    , lookupVar
--    , newVar
--    , bindVar
--    , freeVar
--    ) where

import Prelude hiding (mapM, sequence, foldr, foldr1, foldl, foldl1)

import Data.STRef
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative  (Applicative(..), (<$>))
#endif
import Control.Monad        (ap)
import Control.Monad.Trans  (lift)
import Control.Monad.ST
import Control.Monad.Reader (ReaderT, runReaderT, ask)
import Control.Unification.Types
----------------------------------------------------------------
import Syntax_2
--import Data.Primitive.MutVar
import Control.Monad.ST()
----------------------------------------------------------------

-- | Unification variables implemented by 'STRef's. In addition to
-- the @STRef@ for the term itself, we also track the variable's
-- ID (to support visited-sets).
data STVar s t =
    STVar
        {-# UNPACK #-} !Int
        {-# UNPACK #-} !(STRef s (Maybe (UTerm t (STVar s t))))

instance Show (STVar s t) where
    show (STVar i _) = "STVar " ++ show i

instance Eq (STVar s t) where
    (STVar i _) == (STVar j _) = (i == j)

instance Variable (STVar s t) where
    getVarID (STVar i _) = i


----------------------------------------------------------------
-- TODO: parameterize this so we can use BacktrackST too. Or course,
-- that means defining another class for STRef-like variables
--
-- TODO: parameterize this so we can share the implementation for STVar and STRVar
--
-- TODO: does MTL still have the overhead that'd make it worthwhile
-- to do this manually instead of using ReaderT?
--
-- | A monad for handling 'STVar' bindings.
newtype STBinding s a = STB { unSTB :: ReaderT (STRef s Int) (ST s) a }


-- | Run the 'ST' ranked binding monad. N.B., because 'STVar' are
-- rank-2 quantified, this guarantees that the return value has no
-- such references. However, in order to remove the references from
-- terms, you'll need to explicitly apply the bindings and ground
-- the term.
runSTBinding :: (forall s. STBinding s a) -> a
runSTBinding stb =
    runST (newSTRef minBound >>= runReaderT (unSTB stb))


-- For portability reasons, we're intentionally avoiding
-- -XDeriveFunctor, -XGeneralizedNewtypeDeriving, and the like.

instance Functor (STBinding s) where
    fmap f = STB . fmap f . unSTB

instance Applicative (STBinding s) where
    pure   = return
    (<*>)  = ap
    (*>)   = (>>)
    x <* y = x >>= \a -> y >> return a

instance Monad (STBinding s) where
    return    = STB . return
    stb >>= f = STB (unSTB stb >>= unSTB . f)


----------------------------------------------------------------

_newSTVar
    :: String
    -> Maybe (UTerm t (STVar s t))
    -> STBinding s (STVar s t)
_newSTVar fun mb = STB $ do
    nr <- ask
    lift $ do
        n <- readSTRef nr
        if n == maxBound
            then error $ fun ++ ": no more variables!"
            else do
                writeSTRef nr $! n+1
                STVar n <$> newSTRef mb

instance (Unifiable t) =>
    BindingMonad t (STVar s t) (STBinding s)
    where

    lookupVar (STVar _ p) = STB . lift $ readSTRef p
    
    freeVar  = _newSTVar "freeVar" Nothing
    
    newVar t = _newSTVar "newVar" (Just t)
    
    bindVar (STVar _ p) t = STB . lift $ writeSTRef p (Just t)

----------------------------------------------------------------
----------------------------------------------------------- fin.

{--
:t runSTBinding $ return $ _newSTVar "x" (Just $ freeVar)

runSTBinding $ return $ _newSTVar "x" (Just $ freeVar)
  :: BindingMonad t1 (STVar s t) (UTerm t) => STBinding s (STVar s t)

:t _newSTVar "z" Nothing 
_newSTVar "z" Nothing :: STBinding s (STVar s t)

runSTBinding $ return $ _newSTVar "z" Nothing 
--}

--variableTranslator :: (Unifiable t, Unifiable t1) => STVar s t -> STBinding s (STVar s t1)
variableTranslator  v = do
	x <- newVar (UVar v)
	y <- _newSTVar "a" Nothing	
	z <- freeVar
	return $! z


--f1 = print $ runSTBinding $ return $ _newSTVar "x" Nothing

--f2 (STB a) = a
--f1 :: Monad m => m (STBinding s (STVar s FlatTerm))
--f1 :: STBinding s (STVar s FlatTerm)
f1 :: STBinding s (STVar s FlatTerm)
f1 = runSTBinding $ return $ _newSTVar "a" (Just $ UTerm Wildcard)

--f2 (ST _ a) = a