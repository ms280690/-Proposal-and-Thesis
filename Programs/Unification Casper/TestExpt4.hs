{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes #-}

module TestExpt4 where

import Expt4
import qualified Control.Unification as U
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Control.Unification.IntVar as IV
import qualified Control.Unification.STVar as SV
import Control.Monad.Identity (runIdentity,Identity)
import Control.Monad.Error --(ErrorT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Error (runErrorT)
import Control.Unification.Types as UT

instance (UT.Variable v, Functor t) => Error (UT.UFailure t v) where {}
test1 ::
  ErrorT (UT.UFailure (E Expt4.Variable) (SV.STVar s (E Expt4.Variable)))
           (SV.STBinding s)
            (U.UTerm (E Expt4.Variable) (SV.STVar s (E Expt4.Variable)),
             Map Expt4.Variable (SV.STVar s (E Expt4.Variable)))
test1 = do
    let
        t1raw = Add (Atom . Var $ "x") (Factor . Atom . Var $ "y")
        t2raw = Add (Atom . Var $ "w") (Factor . Atom . Var $ "w")
        t1a = transl (Term1 t1raw)
        t2a = transl (Term1 t2raw)
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- U.unify x1 x2
    return (x3, d1 `Map.union` d2)


test2 ::
  ErrorT (UT.UFailure (E Expt4.Variable) (SV.STVar s (E Expt4.Variable)))
           (SV.STBinding s)
            (U.UTerm (E Expt4.Variable) (SV.STVar s (E Expt4.Variable)),
             Map Expt4.Variable (SV.STVar s (E Expt4.Variable)))
test2 = do
    let
        t1raw = Mul (Atom . Var $ "x") (Var $ "y")
        t2raw = Add (Atom . Var $ "w") (Factor . Atom . Var $ "w")
        t1a = transl (Factor1 t1raw)
        t2a = transl (Term1 t2raw)
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- U.unify x1 x2
    return (x3, d1 `Map.union` d2)


test3 = let
    t1raw = Add (Atom . Var $ "x") (Factor . Atom . Var $ "y")
    t2raw = Add (Atom . Var $ "w") (Factor . Atom . Var $ "w")
    t1a = transl (Term1 t1raw)
    t2a = transl (Term1 t2raw)
    in unifyTerms t1a t2a

test4 = let
    t1raw = Mul (Atom . Var $ "x") (Var $ "y")
    t2raw = Add (Atom . Var $ "w") (Factor . Atom . Var $ "w")
    t1a = transl (Factor1 t1raw)
    t2a = transl (Term1 t2raw)
    in unifyTerms t1a t2a

test5 = do
  let
    t1raw = Var $ "u"
    t2raw = Var $ "w"
    t1a = transl (Atom1 t1raw)
    t2a = transl (Atom1 t2raw)
  unifyTermsX t1a t2a

goTest :: (Show b) =>
    (forall s . (ErrorT (UT.UFailure (E Expt4.Variable)
                        (SV.STVar s (E Expt4.Variable)))
                 (SV.STBinding s) b))
    ->   String
goTest test = SV.runSTBinding $ do
  answer <- runErrorT $ test
  return $! case answer of
    (Left x)  -> "error: " ++ show x
    (Right y) -> "ok:    " ++ show y
