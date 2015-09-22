{-# LANGUAGE DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , FlexibleInstances
           #-}
{-# OPTIONS_GHC -Wall -fwarn-tabs
    -fno-warn-deprecations
    -fno-warn-missing-signatures
    -fno-warn-unused-do-bind
    #-}

import Data.Foldable
import Data.Traversable
import Data.Monoid            (Monoid(..), First(First))
import Control.Applicative
import Control.Monad          (MonadPlus(..))
import Control.Monad.Trans    (MonadTrans(lift))
import Control.Monad.Identity (Identity(..))
import Control.Monad.Except   (MonadError(..), ExceptT(..), runExceptT)
import Control.Monad.State    (MonadState(..), StateT(..), evalStateT, execStateT)
import Control.Monad.Logic    (MonadLogic(..), Logic(), runLogic)

----------------------------------------------------------------
-- HACK: this instance is necessary in order to be able to derive
-- @MonadLogic (Test e s)@
--
-- The @Monoid e@ requirement arises from superclass instances for
-- ExceptT; EitherKT shouldn't have that problem since only Alternative
-- and MonadPlus require the monoid.
instance (Monoid e, MonadLogic m) => MonadLogic (ExceptT e m) where
    msplit = ExceptT . loop . runExceptT
        where
        loop m = do
            x <- msplit m
            case x of
                Nothing       -> return (Right Nothing)
                Just (ea, m') ->
                    case ea of
                    Left _e -> loop m'
                    Right a -> return (Right (Just (a, ExceptT m')))
    
    interleave (ExceptT l) (ExceptT r) = ExceptT (interleave l r)
    
    -- ExceptT m >>- f = ExceptT (m >>- (runExceptT . f))
    
    -- ifte (ExceptT b) t (ExceptT f) = ExceptT (ifte b (runExceptT . t) f)
    
    once (ExceptT m) = ExceptT (once m)

----------------------------------------------------------------
{-
ExceptT e (StateT s Logic) a
== StateT s Logic (Either e a)
== s -> Logic (Either e a, s)
== s -> (forall r. ((Either e a, s) -> r -> r) -> r -> r)
-}
newtype Test e s a = Test { unTest :: ExceptT e (StateT s Logic) a }
    deriving
    ( Functor
    , Applicative
    , Monad
    -- N.B., in order to get backtracking over StateT modifications,
    -- we cannot derive Alternative/MonadPlus, since they'll pick
    -- up the ExceptT instance instead of the Logic instance.
    , MonadLogic
    , MonadState s
    , MonadError e
    )
-- Here, we explicitly unwrap the ExceptT layer in order to pick
-- up the Logic instance (which is carried through by StateT).
instance Alternative (Test e s) where
    empty = Test $ ExceptT empty
    Test x <|> Test y = Test $ ExceptT (runExceptT x <|> runExceptT y)
instance MonadPlus (Test e s) where
    mzero = empty
    mplus = (<|>)

runTest :: Test e s a -> s -> Maybe (Either e a, s)
runTest t s = observeMaybe (runStateT (runExceptT (unTest t)) s)

evalTest :: Test e s a -> s -> Maybe (Either e a)
evalTest t s = observeMaybe (evalStateT (runExceptT (unTest t)) s)

execTest :: Test e s a -> s -> Maybe s
execTest t s = observeMaybe (execStateT (runExceptT (unTest t)) s)

observeMaybe :: Logic a -> Maybe a
observeMaybe mx = runLogic mx (\a _ -> Just a) Nothing

----------------------------------------------------------------

-- @runTest test1 _@ should evaluate to @Just (Right (), True)@.
test1 :: Test () Bool ()
test1 = do
    put True
    (put False >> mzero) <|> return ()

----------------------------------------------------------------
----------------------------------------------------------- fin.
