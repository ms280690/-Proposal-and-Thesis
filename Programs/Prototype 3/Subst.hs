{-# LANGUAGE  DeriveDataTypeable,
              ViewPatterns,
              ScopedTypeVariables,
              DefaultSignatures,
              TypeOperators,
              TypeFamilies,
              DataKinds,
              DataKinds,
              PolyKinds,
              OverlappingInstances,
              TypeOperators,
              LiberalTypeSynonyms,
              TemplateHaskell,
              AllowAmbiguousTypes,
              ConstraintKinds, 
              Rank2Types, 
              MultiParamTypeClasses,
              FunctionalDependencies,
              FlexibleContexts,
              FlexibleInstances,
              UndecidableInstances 
              #-}

-- Substitutions and Unification of Prolog Terms
-- Mark P. Jones November 1990, modified for Gofer 20th July 1991,
-- and for Hugs 1.3 June 1996.
--
-- Suitable for use with Hugs 98.
--

module Subst where

import Prolog
import CustomSyntax
import Data.Map as Map
import Data.Maybe
import Data.Either

--Unification
import Control.Unification.IntVar
import Control.Unification.STVar as ST

import Control.Unification.Ranked.IntVar
import Control.Unification.Ranked.STVar

import Control.Unification.Types as UT

import Control.Monad.State.UnificationExtras
import Control.Unification as U

-- Monads
import Control.Monad.Error
import Control.Monad.Trans.Except

import Data.Functor.Fixedpoint as DFF

--State
import Control.Monad.State.Lazy
import Control.Monad.ST
import Control.Monad.Trans.State as Trans

infixr 3 @@
infix  4 ->-

--- Substitutions:

type Subst = Id -> Term

newtype SubstP = SubstP { unSubstP :: Subst }

-- instance Show SubstP where
--  show (i) = show $ Var i
-- substitutions are represented by functions mapping identifiers to terms.
--
-- app s     extends the substitution s to a function mapping terms to terms
{--
Looks like an apply function that applies a substitution function tho the variables in a term.
--}


-- nullSubst is the empty substitution which maps every identifier to the same identifier (as a term).



-- i ->- t   is the substitution which maps the identifier i to the term t, but otherwise behaves like nullSubst.


-- s1@@ s2  is the composition of substitutions s1 and s2
--           N.B.  app is a monoid homomorphism from (Subst,nullSubst,(@@))
--           to (Term -> Term, id, (.)) in the sense that:
--                  app (s1 @@ s2) = app s1 . app s2
--                 s @@ nullSubst = s = nullSubst @@ s

app                     :: Subst -> Term -> Term
app s (Var i)            = s i
app s (Struct a ts)      = Struct a (Prelude.map (app s) ts)
{--
app (substFunction) (Struct "hello" [Var (0, "Var")])
hello(Var_2) :: Term

--}


nullSubst               :: Subst
nullSubst i              = Var i
{--
nullSubst (0, "Var")
Var :: Term
--}


--
(->-)                   :: Id -> Term -> Subst
(i ->- t) j | j==i       = t
            | otherwise  = Var j
{--
:t (->-) (1,"X") (Struct "hello" [])
(1,"X") ->- Struct "hello" [] :: (Int,[Char]) -> Term
--}


-- Function composition for applying two substitution functions.
(@@)                    :: Subst -> Subst -> Subst
s1 @@ s2                 = app s1 . s2


monadicUnification :: (BindingMonad FTS (STVar s FTS) (ST.STBinding s)) => (forall s. (Term -> Term -> ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s) (UT.UTerm (FTS) (ST.STVar s (FTS)),
            Map Id (ST.STVar s (FTS)))))
monadicUnification t1 t2 = do
  let
    t1f = termFlattener t1
    t2f = termFlattener t2
  (x1,d1) <- lift . translateToUTerm $ t1f
  (x2,d2) <- lift . translateToUTerm $ t2f
  x3 <- U.unify x1 x2
  --get state from somehwere, state -> dict
  return $! (x3, d1 `Map.union` d2)


goUnify ::
  (forall s. (BindingMonad FTS (STVar s FTS) (ST.STBinding s))
  =>
      (ErrorT
          (UT.UFailure FTS (ST.STVar s FTS))
          (ST.STBinding s)
          (UT.UTerm FTS (ST.STVar s FTS),
             Map Id (ST.STVar s FTS)))
     )
  -> [(Id, Prolog)]
goUnify test = ST.runSTBinding $ do
  answer <- runErrorT $ test --ERROR
  case answer of
    (Left _)            -> return []
    (Right (_, dict))   -> f1 dict


f1 ::
  (BindingMonad FTS (STVar s FTS) (ST.STBinding s))
  => (forall s. Map Id (STVar s FTS)
      -> (ST.STBinding s [(Id, Prolog)])
     )
f1 dict = do
  let ld1 = Map.toList dict
  ld2 <- sequence [ v1 | (k,v) <- ld1, let v1 = UT.lookupVar v]
  let ld3 = [ (k,v) | ((k,_),Just v) <- ld1 `zip` ld2]
      ld4 = [ (k,v) | (k,v2) <- ld3, let v = translateFromUTerm dict v2 ]
  return ld4


--unify :: Term -> Term -> [Subst]
unify t1 t2 = substConvertor (goUnify (monadicUnification t1 t2))


varX :: Term
varX = Var (0,"x")

varY :: Term
varY = Var (1,"y")


substConvertor :: [(Id, Prolog)] -> [Subst]
substConvertor xs = Prelude.map (\(varId, p) -> (->-) varId (unFlatten $ unP $ p)) xs 





























{--
//M
{-# LANGUAGE  DeriveDataTypeable, 
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
              Rank2Types,
              AllowAmbiguousTypes, 
              MultiParamTypeClasses, 
              FunctionalDependencies,
              ConstraintKinds,
              ExistentialQuantification
              #-}

-- Substitutions and Unification of Prolog Terms
-- Mark P. Jones November 1990, modified for Gofer 20th July 1991,
-- and for Hugs 1.3 June 1996.
--
-- Suitable for use with Hugs 98.
--

module Subst where

import Prolog
import CustomSyntax
import Data.Map as Map
import Data.Maybe
import Data.Either

--Unification
import Control.Unification.IntVar
import Control.Unification.STVar as ST

import Control.Unification.Ranked.IntVar
import Control.Unification.Ranked.STVar

import Control.Unification.Types as UT

import Control.Monad.State.UnificationExtras
import Control.Unification as U

-- Monads
import Control.Monad.Error
import Control.Monad.Trans.Except

import Data.Functor.Fixedpoint as DFF

--State
import Control.Monad.State.Lazy
import Control.Monad.ST
import Control.Monad.Trans.State as Trans

infixr 3 @@
infix  4 ->-

--- Substitutions:

type Subst = Id -> Term

newtype SubstP = SubstP { unSubstP :: Subst }

-- instance Show SubstP where
--	show (i) = show $ Var i   
-- substitutions are represented by functions mapping identifiers to terms.
--
-- app s     extends the substitution s to a function mapping terms to terms
{--
Looks like an apply function that applies a substitution function tho the variables in a term.
--}


-- nullSubst is the empty substitution which maps every identifier to the same identifier (as a term).



-- i ->- t   is the substitution which maps the identifier i to the term t, but otherwise behaves like nullSubst.


-- s1@@ s2  is the composition of substitutions s1 and s2
--           N.B.  app is a monoid homomorphism from (Subst,nullSubst,(@@))
--           to (Term -> Term, id, (.)) in the sense that:
--                  app (s1 @@ s2) = app s1 . app s2
--                 s @@ nullSubst = s = nullSubst @@ s

app                     :: Subst -> Term -> Term
app s (Var i)            = s i
app s (Struct a ts)      = Struct a (Prelude.map (app s) ts)
{--
app (substFunction) (Struct "hello" [Var (0, "Var")])
hello(Var_2) :: Term

--}


nullSubst               :: Subst
nullSubst i              = Var i
{--
nullSubst (0, "Var")
Var :: Term
--}


-- 
(->-)                   :: Id -> Term -> Subst
(i ->- t) j | j==i       = t
            | otherwise  = Var j
{--
:t (->-) (1,"X") (Struct "hello" [])
(1,"X") ->- Struct "hello" [] :: (Int,[Char]) -> Term
--}


-- Function composition for applying two substitution functions.
(@@)                    :: Subst -> Subst -> Subst
s1 @@ s2                 = app s1 . s2 
{--

--}


--- Unification:

-- unify t1 t2 returns a list containing a single substitution s which is
--             the most general unifier of terms t1 t2.  If no unifier
--             exists, the list returned is empty.
{--
unify :: Term -> Term -> [Subst]
unify (Var x)       (Var y)       = if x==y then [nullSubst] else [x->-Var y]
unify (Var x)       t2            = [ x ->- t2 | x `notElem` varsIn t2 ]
unify t1            (Var y)       = [ y ->- t1 | y `notElem` varsIn t1 ]
unify (Struct a ts) (Struct b ss) = [ u | a==b, u<-listUnify ts ss ]

listUnify :: [Term] -> [Term] -> [Subst]
listUnify []     []     = [nullSubst]
listUnify []     (r:rs) = []
listUnify (t:ts) []     = []
listUnify (t:ts) (r:rs) = [ u2 @@ u1 | u1<-unify t r,
                                       u2<-listUnify (Prelude.map (app u1) ts)
                                                     (Prelude.map (app u1) rs) ]
--}
--- End of Subst.hs

------------------------------------------------------------------------------------------
substFunction :: Id -> Term
substFunction (int, str) = Var (2, str)	-- What substituions need to be done for the variables


----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------

--unify :: Term -> Term -> [Subst]
--unify t1 t2 = Prelude.map (->-) (goUnify $ monadicUnification t1 t2)



-- Some code to convert 
{--
///M
monadicUnification :: (BindingMonad FTS (STVar s FTS) Maybe) => (forall s. (Term -> Term -> ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s) (UT.UTerm (FTS) (ST.STVar s (FTS)), 
           	Map Id (ST.STVar s (FTS)))))
monadicUnification t1 t2 = do 
	let 
		t1f = termFlattener t1
		t2f = termFlattener t2
	(x1,d1) <- lift . translateToUTerm $ t1f
	(x2,d2) <- lift . translateToUTerm $ t2f
	x3 <- U.unify x1 x2
	--get state from somehwere, state -> dict
	return $! (x3, d1 `Map.union` d2)	


goUnify :: (BindingMonad FTS (STVar s FTS) Maybe) => (forall s. 
  (ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map Id (ST.STVar s (FTS))))) -> [(Id, Term)]
goUnify test = ST.runSTBinding $ do
  answer <- runErrorT $ test
  return $! case answer of
  	(Left x)  			-> [] 
	(Right (_, dict)) 	-> f1 dict
    --[unVar $ unFlatten $ unP $ convertToSubst y]
f1 :: (BindingMonad FTS (STVar s FTS) Maybe) => (forall s. Map Id (STVar s FTS))
	-> [(Id, Term)]
f1 dict = Prelude.map (\(k,v) -> (k, unFlatten $ unP $ translateFromUTerm dict v)) 
	(Prelude.map f2 (Map.toList dict))

f2 :: (BindingMonad FTS (ST.STVar s FTS) Maybe) => (forall s. (Id, ST.STVar s FTS) 
	-> (Id, UTerm FTS (ST.STVar s FTS)))
f2 (k,v) = (k, fromJust $ fromJust $ UT.lookupVar v) 
--}

--instance (UT.Unifiable t, UT.Variable v) => BindingMonad (t) (v) (Maybe) where {}

{--
checkFTS (FS _ _) 	= undefined
checkFTS (FV id) 	= FV id

checkTerm (Struct _ _) 	= undefined
checkTerm (Var v) 		= v
--}

--f3 x = translateFromUTerm x

-- DGC
monadicUnification :: Term -> Term -> ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS))) (ST.STBinding s) (UT.UTerm (FTS) (ST.STVar s (FTS)), Map Id (ST.STVar s (FTS)))
monadicUnification t1 t2 = do
  let
    t1f = termFlattener t1
    t2f = termFlattener t2
  (x1,d1) <- lift . translateToUTerm $ t1f
  (x2,d2) <- lift . translateToUTerm $ t2f
  x3 <- U.unify x1 x2
  --get state from somehwere, state -> dict
  return $! (x3, d1 `Map.union` d2)

{--
goUnify :: (forall s. BindingMonad FTS (STVar s FTS) Maybe) => 
	(forall s. ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS))) (ST.STBinding s) 
		(UT.UTerm (FTS) (ST.STVar s (FTS)), Map Id (ST.STVar s (FTS)))) 
	-> [(Id, Term)]
--}
{--
goUnify :: (BindingMonad FTS (STVar s FTS) Maybe) => (forall e s t. 
	ErrorT e (STBinding s) (t, Map Id (STVar s FTS))) 
	-> [(Id, Term)] 
--}
goUnify ::ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS))) (ST.STBinding s) (UT.UTerm (FTS) (ST.STVar s (FTS)), Map Id (ST.STVar s (FTS)))
  -> String
goUnify test = ST.runSTBinding $ do
  answer <- runErrorT $ test
  return $! case answer of
    (Left _)              -> "undefined"
    (Right _)     -> "answer" --ERROR
    --[unVar $ unFlatten $ unP $ convertToSubst y]

f0 :: (BindingMonad FTS (STVar s FTS) Maybe) => Either t (t1, Map Id (STVar s FTS)) 
	-> [(Id, Term)]
f0 (Left _) 		= undefined
f0 (Right (_,d)) 	= f1 d

f1 :: (BindingMonad FTS (STVar s FTS) Maybe) => Map Id (STVar s FTS) -> [(Id, Term)]
f1 dict =
  Prelude.map (\(k,v) -> (k, unFlatten $ unP $ translateFromUTerm dict v))
              (Prelude.map f2 (Map.toList dict))

f2 :: (BindingMonad t1 v Maybe) => (t, v) -> (t, UTerm t1 v)
f2 (k,v) = (k, fromJust $ fromJust $ UT.lookupVar v)

convertToSubst :: (UTerm FTS (STVar s FTS), Map Id (STVar s FTS)) -> Prolog
convertToSubst (uTerm, dict) = translateFromUTerm dict uTerm

unVar :: Term -> Id
unVar (Var v) = v
unVar _ 	  = (undefined, "not applicable")	

getLangvar (u, m) = UT.lookupVar u 

eitherSolver :: Either t t1 -> t1
eitherSolver (Right x) = x
eitherSolver (Left y) = undefined

justMaybe (Just x) = x
justMaybe Nothing = undefined

{--
--State
{--
monadicUnification :: Term -> Term -> ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s) (UT.UTerm (FTS) (ST.STVar s (FTS)), 
           	Map Id (ST.STVar s (FTS)))
--}
{--
monadicUnificationState
  :: (Fallible FTS (STVar s FTS) e, MonadTrans t,
      MonadError e (t (STBinding s)), MonadState t1 (t (STBinding s)),
      Functor (t (STBinding s))) =>
     Term
     -> Term
     -> t (STBinding s) (UTerm FTS (STVar s FTS),
                         Map Id (STVar s FTS),
                         t1)
--}
{--
monadicUnificationState :: (Fallible FTS (STVar s FTS) e, MonadError e (STBinding s))
 => Term -> Term -> StateT t (STBinding s) (UTerm FTS (STVar s FTS), 
 	Map Id (STVar s FTS), t)
--}
monadicUnificationState
  :: (Fallible FTS (STVar s FTS) e, MonadError e (STBinding s)) =>
     Term -> Term -> StateT b (STBinding s) b
monadicUnificationState t1 t2 = do 
	let 
		t1f = termFlattener t1
		t2f = termFlattener t2
	(x1,d1) <- lift . translateToUTerm $ t1f
	(x2,d2) <- lift . translateToUTerm $ t2f
	x3 <- U.unify x1 x2
	state <- Trans.get
--	return $! (x3, d1 `Map.union` d2, state)	
	return state

{--
goUnifyState :: (forall s . (STBinding s (Either e (UTerm FTS (STVar s FTS), 
	Map Id (STVar s FTS), t)))) -> String
--}
{--
goUnifyState  :: (Fallible FTS (STVar s FTS) e, MonadError e (STBinding s)) =>
     a -> STBinding s (a, a)
--}
goUnifyState test = ST.runSTBinding $ do
  	let answer = Trans.runStateT $ test
	return answer

--  return $! case answer of
--    (Left x)  -> "error " ++ show x 
--    (Right y) -> "ok " ++ show y
--}


///M
--}