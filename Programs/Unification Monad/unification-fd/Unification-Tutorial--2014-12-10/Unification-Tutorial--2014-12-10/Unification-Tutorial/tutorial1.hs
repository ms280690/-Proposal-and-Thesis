{-# LANGUAGE DeriveFunctor
           , DeriveFoldable
           , DeriveTraversable
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , FlexibleInstances
           , NoMonomorphismRestriction
           , KindSignatures
           , ConstraintKinds
           , RankNTypes
           , FlexibleContexts
           #-}
{-# OPTIONS_GHC -Wall -fwarn-tabs
    -fno-warn-deprecations
    -fno-warn-missing-signatures
    -fno-warn-unused-do-bind
    #-}

import Data.List.Extras.Pair  (pairWith)
import Data.Foldable
import Data.Traversable
--import Data.Monoid            (Monoid(..), First(First))
import Control.Applicative
import Control.Monad.Error    ({--MonadError(..),--} ErrorT(..), runErrorT)
import Control.Monad.Identity (Identity(..))
import Control.Monad.Logic    ({--MonadLogic(..),--} Logic(), runLogic)
import Control.Monad.Trans    (MonadTrans(lift))
--import Control.Monad          (MonadPlus(..))
import Control.Unification
import Control.Unification.IntVar

import Data.Maybe

import Control.Monad.MaybeK


{-----------------------------------------------------------------
UTerm :- Something to store logic terms. 
"FREE MONAD" :- 
v --> Logic Variables (Leaves)
functor t --> constructor description for logic terms (nodes / layers of t structures) 
------------------------------------------------------------------}


----------------------------------------------------------------
data T a = T String [a]
    deriving (Show, Functor, Foldable, Traversable)

-- String :- Name of tree constructors
-- [a] :- Ordered sequence of subterms

foo :: UTerm T v -> UTerm T v -> UTerm T v
foo x y = UTerm$T "foo" [x,y] -- foo(X, Y)
{--
foo (UVar "x") (UVar "y")
T "foo" ["x","y"]
--}

bar :: UTerm T v
bar     = UTerm$T "bar" []    -- bar
{--
bar
T "bar" []
--}

baz :: UTerm T v -> UTerm T v
baz x   = UTerm$T "baz" [x]   -- baz(X)
{--
baz (UVar "x")
T "baz" ["x"]
--}

atom :: String -> UTerm T v
atom n  = UTerm$T n []
{--
atom "hello"
T "hello" []
--}

{--
foo (bar) (baz(UVar "x"))
T "foo" [T "bar" [],T "baz" ["x"]]
--}

{--
For a data type to work with the library it must have an instance of Unifiable.
In order to derive Unifiable we need the following,
1. Functor (fmap)
fmap :: (a -> b) -> f a -> f b

2. Foldable (foldMap / foldr)
foldMap :: Monoid m => (a -> m) -> t a -> m

3. Traversable (traverse / sequenceA) 
traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

4. Unifiable
zipMatch :: t a -> t a -> Maybe (t (Either a (a, a)))

--}

instance Unifiable T where
    zipMatch (T m ls) (T n rs)
        | m /= n    = Nothing
        | otherwise =
            T n <$> pairWith (\l r -> Right(l,r)) ls rs
{--
zipMatch (T "hello" []) (T "hello" [])
Just (T "hello" [])

zipMatch (T "hello" []) (T "hello1" [])
Nothing
--}

----------------------------------------------------------------
-- Some aliases for simplifying type signatures:
type PrologTerm           = UTerm T IntVar
{--
:t IntVar
IntVar :: Int -> IntVar
IntVar 5
    
UTerm (T "hello" []) :: PrologTerm :: PrologTerm

(UTerm $ T "hello" [UVar $ IntVar 1]) :: PrologTerm :: PrologTerm
--}

type PrologFailure        = UnificationFailure T IntVar
{--
:t OccursIn (IntVar 1) (UTerm $ T "hello" []) :: PrologFailure

OccursIn (IntVar 1) (UTerm $ T "hello" []) :: PrologFailure :: PrologFailure
--}

type PrologBindingState   = IntBindingState T
{--

--}


type FallibleBindingMonad = ErrorT PrologFailure (IntBindingT T Identity)
{--

--}

type PrologMonad          = ErrorT PrologFailure (IntBindingT T Logic)
{--

--}

----------------------------------------------------------------
{--
The example functions unify prolog terms in different ways.  

The functions unify and (=:=) are the same and have the same signature as 
well.

These definitions are in the Control.Unification model
unify :: (BindingMonad t v m, MonadTrans e, Functor (e m), MonadError (UnificationFailure t v) (e m)) => UTerm t v -> UTerm t v -> e m (UTerm t v)

(=:=) :: (BindingMonad t v m, MonadTrans e, Functor (e m), MonadError (UnificationFailure t v) (e m)) => UTerm t v -> UTerm t v -> e m (UTerm t v)


while the following are in the Control.Unification.ranked module,
unify :: (RankedBindingMonad t v m, MonadTrans e, Functor (e m), MonadError (UnificationFailure t v) (e m)) => UTerm t v -> UTerm t v -> e m (UTerm t v)

(=:=) :: (RankedBindingMonad t v m, MonadTrans e, Functor (e m), MonadError (UnificationFailure t v) (e m)) => UTerm t v -> UTerm t v -> e m (UTerm t v)
    

The only difference being that of the BindingMonad and the RankedBindingMonad

--}


-- | @example1(X,Y,Z) :- X = Y, Y = Z.@
-- example1 :: PrologTerm -> PrologTerm -> PrologTerm -> Example
{--
 
--}
example1 x y z = do
    x =:= y
    y =:= z


-- | A more efficient implementation of 'example1'.
-- example1' :: PrologTerm -> PrologTerm -> PrologTerm -> Example
example1' x y z = do
    y' <- x =:= y
    y' =:= z


-- example1' using unify function.
example1'' x y z = do
    y' <- x =:= y
    y' =:= z

-- N.B., This type signature is (unfortunately) necessary in order
-- to avoid ambiguity when we discard the variable it returns. But,
-- if you never discard the result, then you should be able to get
-- away with commenting out the signature.
getFreeVar
    :: (Applicative m, Monad m)
    => ErrorT PrologFailure (IntBindingT T m) PrologTerm
getFreeVar = lift (UVar <$> freeVar)


-- | @example2(X,Z) :- X = Y, Y = Z.@
-- example2 :: PrologTerm -> PrologTerm -> Example
example2 x z = do
    y <- getFreeVar
    x =:= y
    y =:= z

-- | @example3(X,Z) :- example1(X,Y,Z).@
-- example3 :: PrologTerm -> PrologTerm -> Example
example3 x z = do
    y <- getFreeVar
    example1 x y z


-- TODO: transformers-0.4.1.0 deprecated Control.Monad.Trans.Error  (transformers-0.3.0.0 says it's fine).
-- BUG: in order to use Control.Monad.Trans.Except, we need a monoid instance
-- TODO: redefine UnificationFailure to deal with all that crap...
-- example4 :: PrologTerm -> Example
example4 x = (x =:= bar) <|> (x =:= atom "other")

backtrackingTest
    :: (Applicative m, Monad m)
    => ErrorT PrologFailure (IntBindingT T m) PrologTerm
backtrackingTest = do
    x <- getFreeVar
    y <- getFreeVar
    (x =:= y >> failure) <|> return (foo x y)
    where
    failure = atom "a" =:= atom "b"

----------------------------------------------------------------
runFBM
    :: FallibleBindingMonad a
    -> (Either PrologFailure a, PrologBindingState)
runFBM = runIdentity . runIntBindingT . runErrorT

evalFBM :: FallibleBindingMonad a -> Either PrologFailure a
evalFBM = runIdentity . evalIntBindingT . runErrorT

execFBM :: FallibleBindingMonad a -> PrologBindingState
execFBM = runIdentity . execIntBindingT . runErrorT


runProlog
    :: PrologMonad a
    -> Maybe (Either PrologFailure a, PrologBindingState)
runProlog = observeMaybe . runIntBindingT . runErrorT

evalProlog :: PrologMonad a -> Maybe (Either PrologFailure a)
evalProlog = observeMaybe . evalIntBindingT . runErrorT

execProlog :: PrologMonad a -> Maybe PrologBindingState
execProlog = observeMaybe . execIntBindingT . runErrorT

observeMaybe :: Logic a -> Maybe a
observeMaybe mx = runLogic mx (\a _ -> Just a) Nothing

---------------------------------------------------------------
------------------------------------------------------------ fin.

main:: IO ()
main = undefined

{--
Traversable stuff

traverse ("a" ++) (T "hello" ["world"])
[T "hello" "a",T "hello" "w",T "hello" "o",T "hello" "r",T "hello" "l",T "hello"

traverse (++ "a") (T "hello" ["world"])
[T "hello" "w",T "hello" "o",T "hello" "r",T "hello" "l",T "hello" "d",T "hello"
--}

{--
Foldable Stuff



--}



myFunc1 x y z = do
    let w = runProlog $ example1 x y z
    print w
{--
example1 :-
myFunc1 (UTerm $ T "hello" []) (UTerm $ T "hello" []) (UTerm $ T "hello" [])
Just (Right (T "hello" []),IntBindingState { nextFreeVar = -9223372036854775808, varBindings = fromList []})




--}


myFunc2 x y z = do
    let w = evalProlog $ example1 x y z
    print w
{--
example 1 :- 
myFunc2 (UTerm $ T "hello" []) (UTerm $ T "hello" []) (UTerm $ T "hello" [])
Just (Right (T "hello" []))

--}

myFunc3 x y z = do
    let w = execProlog $ example1 x y z
    print w
{--
example 1 :-
myFunc3 (UTerm $ T "hello" []) (UTerm $ T "hello" []) (UTerm $ T "hello" [])
Just IntBindingState { nextFreeVar = -9223372036854775808, varBindings = fromList []}
--}


myFunc4 = do
    let y = getFreeVar
    print $ runProlog y
{--
myFunc4
Just (Right (IntVar (-9223372036854775808)),IntBindingState { nextFreeVar = -9223372036854775807, varBindings = fromList []})
--}

myFunc5 x y z = do
    let w = runFBM $ example1 x y z
    print w
{--
myFunc5 (UTerm $ T "hello" [UVar $ IntVar 1]) (UTerm $ T "hello" [UVar $ IntVar 2]) (UTerm $ T "hello" [UVar $ IntVar 3])
(Right (T "hello" [IntVar 3]),IntBindingState { nextFreeVar = -9223372036854775808, varBindings = fromList [(1,IntVar 2),(2,IntVar 3)]})
--}


myFunc6 x y z = do
    let w = execProlog $ example1'' x y z
    print w
{--
myFunc6 (UTerm $ T "hello" []) (UTerm $ T "hello" []) (UTerm $ T "hello" [])
Just IntBindingState { nextFreeVar = -9223372036854775808, varBindings = fromList []}
--}



----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{--

Some other functions, from the library with accompanying examples.

--}


{--

Firstly we begin with the functions in the module Control.Unification. 

--}



{--
1. freeze :: Traversable t => UTerm t v -> Maybe (Fix t)

Extract a "pure" term from a mutable one and if the term contains variables then return Nothing.

--}

myFreezeFunction t = do
    let w = freeze t
    return w
{--
myFreezeFunction (UTerm $ T "hello" [])
Just (T "hello" [])

myFreezeFunction (UTerm $ T "hello" [UVar $ IntVar 1])
Nothing

--}


{--
2. unfreeze :: Functor t => Fix t -> UTerm t v

Reverse of freeze. 

--}

myUnFreezeFunction t = do
    let w = unfreeze $ fromJust $ freeze t
    return w
{--
myUnFreezeFunction (UTerm $ T "hello" [])
T "hello" []

myUnFreezeFunction (UTerm $ T "hello" [UVar $ IntVar 1])
*** Exception: Maybe.fromJust: Nothing

--}


{--
3. getVarID :: v -> Int

--}

--myGetVarIDFunction :: BindingMonad t IntVar m => m Int
myGetVarIDFunction = do
    x <- newVar (UVar $ IntVar 1)
    return $ getVarID x

mySymbolicTerms :: BindingMonad t v m => UTerm t v -> UTerm t v -> m Bool
mySymbolicTerms x y = do
    z <- x === y
    case z of True      ->  return True
              otherwise ->  return False 








