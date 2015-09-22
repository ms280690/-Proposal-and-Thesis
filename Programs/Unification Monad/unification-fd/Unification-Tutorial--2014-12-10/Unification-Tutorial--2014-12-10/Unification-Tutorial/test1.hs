{-# LANGUAGE DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , FlexibleInstances
           , StandaloneDeriving
           , TemplateHaskell
           , UndecidableInstances
           , NoMonomorphismRestriction
           #-}
{-# OPTIONS_GHC -Wall -fwarn-tabs
    -fno-warn-deprecations
    -fno-warn-missing-signatures
    -fno-warn-unused-do-bind
    -fno-warn-orphans
    #-}

--import Data.FoldableX
--import Data.Traversable
import Data.Monoid            (Monoid(..))
import Control.Applicative
import Control.Monad          (MonadPlus(..))
import Control.Monad.Trans.Error    (ErrorT(..))
--import Control.Monad.Identity (Identity(..))
import Control.Monad.Error    (Error,MonadError(..))
import Control.Monad.State    (MonadState(..), StateT(..), evalStateT, execStateT)
import Control.Monad.Logic    (MonadLogic(..), Logic(), runLogic) 
----------------------------------------------------------------
 

-- HACK: this instance is necessary in order to be able to derive
-- @MonadLogic (Test e s)@  

-- The @Monoid e@ requirement arises from superclass instances for
-- ExceptT; EitherKT shouldn't have that problem since only Alternative
-- and MonadPlus require the monoid.
instance (Error e, Monoid e, MonadLogic m) => MonadLogic (ErrorT e m) where
--    msplit :: MonadLogic m => m a -> m (Maybe (a, m a))
    msplit = ErrorT . loop . runErrorT
        where
        loop m = do
            x <- msplit m
            case x of
                Nothing       -> return (Right Nothing)
                Just (ea, m') ->
                    case ea of
                    Left _e -> loop m'
                    Right a -> return (Right (Just (a, ErrorT m')))

    interleave (ErrorT l) (ErrorT r) = ErrorT (interleave l r) -- fair disjunction

    -- ExceptT m >>- f = ExceptT (m >>- (runExceptT . f)) -- fair conjunction

    -- ifte (ExceptT b) t (ExceptT f) = ExceptT (ifte b (runExceptT . t) f) -- Prolog soft cut, logical conditional

    once (ErrorT m) = ErrorT (once m) -- pruning 

--loop1 :: MonadLogic m => m (Either e a1) -> m (Either a (Maybe (a1, ErrorT e m a1)))
loop1 m = do
            x <- msplit m
            case x of
                Nothing       -> return (Right Nothing)
                Just (ea, m') ->
                    case ea of
                    Left _e -> loop1 m'
                    Right a -> return (Right (Just (a, ErrorT m')))

{--
msplit which returns the first result and a computation to produce the remaining results.

:t (ErrorT (Left 1))
(ErrorT (Left 1)) :: Num a1 => ErrorT e (Either a1) a

:t runErrorT $ (ErrorT (Left 1))
runErrorT $ (ErrorT (Left 1)) :: Num a1 => Either a1 (Either e a)

:t loop1 . runErrorT $ (ErrorT (Left 1))
loop1 . runErrorT $ (ErrorT (Left 1)) :: (MonadLogic (Either a2), Num a2) => Either a2 (Either a (Maybe (a1, ErrorT e (Either a2) a1)))

let y = ErrorT . loop1 . runErrorT $ ErrorT (Left 1)
:t y
y :: (MonadLogic (Either a), Num a) => ErrorT e (Either a) (Maybe (a1, ErrorT e1 (Either a) a1))
--}

----------------------------------------------------------------
{-
ErrorT e (StateT s Logic) a
== StateT s Logic (Either e a)
== s -> Logic (Either e a, s)
== s -> (forall r. ((Either e a, s) -> r -> r) -> r -> r)
-}
newtype Test e s a = Test { unTest :: ErrorT e (StateT s Logic) a }
    deriving
    ( Functor
    , Applicative
    , Monad
    -- N.B., in order to get backtracking over StateT modifications,
    -- we cannot derive Alternative/MonadPlus, since they'll pick
    -- up the ErrorT instance instead of the Logic instance.
    , MonadLogic
    , MonadState s
    , MonadError e
    )
-- Here, we explicitly unwrap the ErrorT layer in order to pick
-- up the Logic instance (which is carried through by StateT).
instance Alternative (Test e s) where
    empty = Test $ ErrorT empty
    Test x <|> Test y = Test $ ErrorT (runErrorT x <|> runErrorT y)
instance Error e => MonadPlus (Test e s) where
    mzero = empty
    mplus = (<|>)

runTest :: Test e s a -> s -> Maybe (Either e a, s)
runTest t s = observeMaybe (runStateT (runErrorT (unTest t)) s)

evalTest :: Test e s a -> s -> Maybe (Either e a)
evalTest t s = observeMaybe (evalStateT (runErrorT (unTest t)) s)

execTest :: Test e s a -> s -> Maybe s
execTest t s = observeMaybe (execStateT (runErrorT (unTest t)) s)

observeMaybe :: Logic a -> Maybe a
observeMaybe mx = runLogic mx (\a _ -> Just a) Nothing

----------------------------------------------------------------


{--
put :: s -> m () Source

Replace the state inside the monad.
--}

-- @runTest test1 _@ should evaluate to @Just (Right (), True)@.
test1 :: Test () Bool ()
test1 = do
    put True
    (put False >> mzero) <|> return ()

--test2 :: Test () Int Int
test2 = do
  put 3
  (put 2 >>= return 1) <|> return 1

instance Control.Monad.Error.Error () where {}

{--
($!) (+) 1 2
*** Parser:
*** Desugar:
*** Simplify:
*** CorePrep:
*** ByteCodeGen:
3
--}

main:: IO ()
main = do
  x <- return $! runTest test1 undefined
  print x

----------------------------------------------------------------
----------------------------------------------------------- fin.

myFunc1 = do
  let x = loop1 . runErrorT $ ErrorT (Right 1)
  putStrLn "Hello"
  return x
