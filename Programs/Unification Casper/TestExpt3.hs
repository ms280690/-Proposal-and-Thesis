module TestExpt3 where

import Expt3
import qualified Control.Unification as U
import Data.Map(Map)
import qualified Data.Map as Map
import qualified Control.Unification.IntVar as IV
import Control.Monad.Identity (runIdentity,Identity)
import Control.Monad.Error --(ErrorT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Error (runErrorT)
import Control.Unification.Types as UT


import Control.Unification.Types (UFailure)


instance (UT.Variable v, Functor t) => Error (UT.UFailure t v) where {}
test1 :: () =>
    ErrorT (UFailure (E IV.IntVar) IV.IntVar)
           (IV.IntBindingT (E IV.IntVar) Identity)
            (U.UTerm (E IV.IntVar) IV.IntVar,
             Map Expt3.Variable IV.IntVar)
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

test2 :: () =>
         ((U.UTerm (E IV.IntVar) IV.IntVar,
                      Map Expt3.Variable IV.IntVar),
                     IV.IntBindingState (E IV.IntVar))
test2 =  case runIdentity . IV.runIntBindingT . runErrorT $ test1 of
  (Left x,_)  -> error . show $ x
  (Right y,s) -> (y,s)

