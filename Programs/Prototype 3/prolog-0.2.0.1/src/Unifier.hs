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
        AllowAmbiguousTypes
        #-}
{--
module Unifier
   ( Unifier, Substitution
   , unify, unify_with_occurs_check
   , apply, (+++)
   )
where
--}

module Unifier where

import Control.Monad
import Control.Monad (MonadPlus, mzero)
import Control.Arrow (second)
import Data.Function (fix)
import Data.Generics (everything, mkQ)

import Syntax

import Control.Monad.State.UnificationExtras
import Control.Unification as U


import Data.Functor.Fixedpoint as DFF


import Control.Unification.IntVar
import Control.Unification.STVar as ST

import Control.Unification.Ranked.IntVar
import Control.Unification.Ranked.STVar

import Control.Unification.Types as UT



import Data.Traversable as T 
import Data.Functor 
import Data.Foldable
import Control.Applicative


import Data.List.Extras.Pair
import Data.Map as Map
import Data.Set as S

import Control.Monad.Error
import Control.Monad.Trans.Except


import Data.Map.Lazy as MapLazy

type Unifier      = [Substitution]
type Substitution = (VariableName, Term)

-----remove start
{--
unify, unify_with_occurs_check :: MonadPlus m => Term -> Term -> m Unifier

unify = fix unify'

unify_with_occurs_check =
   fix $ \self t1 t2 -> if (t1 `occursIn` t2 || t2 `occursIn` t1)
                           then fail "occurs check"
                           else unify' self t1 t2
 where
   occursIn t = everything (||) (mkQ False (==t))

unify' :: MonadPlus m => (Term -> Term -> m Unifier) -> Term -> Term -> m [(VariableName, Term)]

-- If either of the terms are don't cares then no unifiers exist
unify' _ Wildcard _ = return []
unify' _ _ Wildcard = return []

-- If one is a variable then equate the term to its value which forms the unifier
unify' _ (Var v) t  = return [(v,t)]
unify' _ t (Var v)  = return [(v,t)]

-- Match the names and the length of their parameter list and then match the elements of list one by one. 
unify' self (Struct a1 ts1) (Struct a2 ts2) 
            | a1 == a2 && same length ts1 ts2 = unifyList self (zip ts1 ts2)

-- ?????
unify' _ _ _ = mzero

same :: Eq b => (a -> b) -> a -> a -> Bool
same f x y = f x == f y

-- Match the elements of each of the tuples in the list. 
unifyList :: Monad m => (Term -> Term -> m Unifier) -> [(Term, Term)] -> m Unifier
unifyList _ [] = return []
unifyList unify ((x,y):xys) = do
   u  <- unify x y
   u' <- unifyList unify (Prelude.map (both (apply u)) xys)
   return (u++u')
-----------------------------------------------------remove end
--}
both :: (t -> t1) -> (t, t) -> (t1, t1)
both f (x,y) = (f x, f y)

(+++) :: [Substitution] -> [Substitution] -> Unifier
u1 +++ u2 = simplify $ u1 ++ u2

simplify :: Unifier -> Unifier
simplify u = Prelude.map (second (apply u)) u

apply :: Unifier -> Term -> Term
apply = flip $ Prelude.foldl $ flip substitute
  where
    substitute (v,t) (Var v') | v == v' = t
    substitute s     (Struct a ts)      = Struct a (Prelude.map (substitute s) ts)
    substitute _     t                  = t


{--
g :: Monad m => (t -> m a) -> Maybe t -> m (Maybe a)
g f Nothing    = return Nothing
g f (Just x)   = liftM Just $ f x {- or return Just `ap` f x -}
                  {- or do { y <- f x ; return $! Just y } -}
--}
{--
term1 = Var $ VariableName 0 "x"

term2 = Var $ VariableName 1 "y"

unifyTest :: (MonadPlus m) => m Unifier
unifyTest = do 
  u <- Unifier.unify term1 term2
  return u  
--}

----------------------------------------------------------------------------------
----------------------------------------------------------------------------------



termFlattener :: Term -> Fix FTS
termFlattener (Var v)           =   DFF.Fix $ FV v
termFlattener (Wildcard)        =   DFF.Fix FW
termFlattener (Cut i)           =   DFF.Fix $ FC i
termFlattener (Struct a xs)     =   DFF.Fix $ FS a (Prelude.map termFlattener xs)

unFlatten :: Fix FTS -> Term
unFlatten (DFF.Fix (FV v))      =   Var v
unFlatten (DFF.Fix FW)          =   Wildcard
unFlatten (DFF.Fix (FC i))      =   Cut i
unFlatten (DFF.Fix (FS a xs))   =   Struct a (Prelude.map unFlatten xs)


variableExtractor :: Fix FTS -> [Fix FTS]
variableExtractor (Fix x) = case x of
  (FS _ xs)   ->  Prelude.concat $ Prelude.map variableExtractor xs
  (FV v)     ->  [Fix $ FV v]
  _       ->  [] 

variableNameExtractor :: Fix FTS -> [VariableName]
variableNameExtractor (Fix x) = case x of
  (FS _ xs) -> Prelude.concat $ Prelude.map variableNameExtractor xs
  (FV v)     -> [v]
  _         -> [] 

variableSet :: [Fix FTS] -> S.Set (Fix FTS)
variableSet a = S.fromList a

variableNameSet :: [VariableName] -> S.Set (VariableName)
variableNameSet a = S.fromList a

varsToDictM :: (Ord a, Unifiable t) =>
    S.Set a -> ST.STBinding s (Map a (ST.STVar s t))
varsToDictM set = foldrM addElt Map.empty set where
  addElt sv dict = do
    iv <- freeVar
    return $! Map.insert sv iv dict


uTermify 
  :: Map VariableName (ST.STVar s (FTS)) 
  -> UTerm FTS (ST.STVar s (FTS)) 
  -> UTerm FTS (ST.STVar s (FTS))
uTermify varMap ux = case ux of
  UT.UVar _             -> ux
  UT.UTerm (FV v)       -> maybe (error "bad map") UT.UVar $ Map.lookup v varMap
 -- UT.UTerm t            -> UT.UTerm $! fmap (uTermify varMap) t
  UT.UTerm (FS a xs)    -> UT.UTerm $ FS a $! fmap (uTermify varMap) xs   
  UT.UTerm (FW)         -> UT.UTerm FW
  UT.UTerm (FC i)       -> UT.UTerm (FC i)

translateToUTerm ::
    Fix FTS -> ST.STBinding s
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map VariableName (ST.STVar s (FTS)))
translateToUTerm e1Term = do
  let vs = variableNameSet $ variableNameExtractor e1Term
  varMap <- varsToDictM vs
  let t2 = uTermify varMap . unfreeze $ e1Term
  return (t2,varMap)


-- | vTermify recursively converts @UVar x@ into @UTerm (VarA x).
-- This is a subroutine of @ translateFromUTerm @.  The resulting
-- term has no (UVar x) subterms.

vTermify :: Map Int VariableName ->
            UT.UTerm (FTS) (ST.STVar s (FTS)) ->
            UT.UTerm (FTS) (ST.STVar s (FTS))
vTermify dict t1 = case t1 of
  UT.UVar x  -> maybe (error "logic") (UT.UTerm . FV) $ Map.lookup (UT.getVarID x) dict
  UT.UTerm r ->
    case r of
      FV iv   -> t1
      _       -> UT.UTerm . fmap (vTermify dict) $ r

translateFromUTerm :: 
    Map VariableName (ST.STVar s (FTS)) ->
    UT.UTerm (FTS) (ST.STVar s (FTS)) -> Prolog
translateFromUTerm dict uTerm =
  P .  maybe (error "Logic") id . freeze . vTermify varIdDict $ uTerm where
    forKV dict initial fn = Map.foldlWithKey' (\a k v -> fn k v a) initial dict
    varIdDict = forKV dict Map.empty $ \ k v -> Map.insert (UT.getVarID v) k


-- | Unify two (E1 a) terms resulting in maybe a dictionary
-- of variable bindings (to terms).
--
-- NB !!!!
-- The current interface assumes that the variables in t1 and t2 are
-- disjoint.  This is likely a mistake that needs fixing

unifyTerms :: Fix FTS -> Fix FTS -> Maybe (Map VariableName (Prolog))
unifyTerms t1 t2 = ST.runSTBinding $ do
  answer <- runExceptT $ unifyTermsX t1 t2
  return $! either (const Nothing) Just answer

-- | Unify two (E1 a) terms resulting in maybe a dictionary
-- of variable bindings (to terms).
--
-- This routine works in the unification monad

unifyTermsX ::
    (Fix FTS) -> (Fix FTS) ->
    ExceptT  (UT.UFailure (FTS) (ST.STVar s (FTS)))
        (ST.STBinding s)
        (Map VariableName (Prolog))
unifyTermsX t1 t2 = do
    (x1,d1) <- lift . translateToUTerm $ t1
    (x2,d2) <- lift . translateToUTerm $ t2
    _ <- U.unify x1 x2
    makeDicts $ (d1,d2)

mapWithKeyM :: (Ord k,Applicative m,Monad m)
               => (k -> a -> m b) -> Map k a -> m (Map k b)
mapWithKeyM = Map.traverseWithKey


makeDict :: 
            Map VariableName (ST.STVar s (FTS)) -> ST.STBinding s (Map VariableName (Prolog))
makeDict sVarDict =
    flip mapWithKeyM sVarDict $ \ _ -> \ iKey -> do
        Just xx <- UT.lookupVar $ iKey
        return $! (translateFromUTerm sVarDict) xx


-- | recover the bindings for the variables of the two terms
-- unified from the monad.

makeDicts :: 
    (Map VariableName (ST.STVar s (FTS)), Map VariableName (ST.STVar s (FTS))) ->
    ExceptT  (UT.UFailure (FTS) (ST.STVar s (FTS)))
    (ST.STBinding s) (Map VariableName (Prolog))
makeDicts (svDict1, svDict2) = do
  let svDict3 = (svDict1 `Map.union` svDict2)
  let ivs = Prelude.map UT.UVar . Map.elems $ svDict3
  applyBindingsAll ivs
  -- the interface below is dangerous because Map.union is left-biased.
  -- variables that are duplicated across terms may have different
  -- bindings because `translateToUTerm` is run separately on each
  -- term.
  lift . makeDict $ svDict3

instance (UT.Variable v, Functor t) => Error (UT.UFailure t v) where {}

test1 ::
  ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map VariableName (ST.STVar s (FTS)))
test1 = do
    let
        t1a = (Fix $ FV $ VariableName 0 "x")
        t2a = (Fix $ FV $ VariableName 1 "y")
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- U.unify x1 x2
    return (x3, d1 `Map.union` d2)

test2 ::
  ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map VariableName (ST.STVar s (FTS)))
test2 = do
    let
        t1a = (Fix $ FS "a" [Fix $ FV $ VariableName 0 "x"])
        t2a = (Fix $ FV $ VariableName 1 "y")
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- U.unify x1 x2
    return (x3, d1 `Map.union` d2)


test3 ::
  ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map VariableName (ST.STVar s (FTS)))
test3 = do
    let
        t1a = (Fix $ FS "a" [Fix $ FV $ VariableName 0 "x"])
        t2a = (Fix $ FV $ VariableName 0 "x")
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- U.unify x1 x2
    return (x3, d1 `Map.union` d2)
{--
goTest test3
"ok:    STVar -9223372036854775807 
[(VariableName 0 \"x\",STVar -9223372036854775808)]"
--}

test4 ::
  ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map VariableName (ST.STVar s (FTS)))
test4 = do
    let
        t1a = (Fix $ FS "a" [Fix $ FV $ VariableName 0 "x"])
        t2a = (Fix $ FV $ VariableName 0 "x")
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- U.unifyOccurs x1 x2
    return (x3, d1 `Map.union` d2)
{--
goTest test4
"ok:    STVar -9223372036854775807 
[(VariableName 0 \"x\",STVar -9223372036854775808)]"
--}

test5 ::
  ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map VariableName (ST.STVar s (FTS)))
test5 = do
    let
        t1a = (Fix $ FS "a" [Fix $ FV $ VariableName 0 "x"])
        t2a = (Fix $ FS "b" [Fix $ FV $ VariableName 0 "y"])
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- U.unify x1 x2
    return (x3, d1 `Map.union` d2)

goTest :: (Show b) => (forall s . 
  (ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s)
            (UT.UTerm (FTS) (ST.STVar s (FTS)),
             Map VariableName (ST.STVar s (FTS))))) -> String
goTest test = ST.runSTBinding $ do
  answer <- runErrorT $ test
  return $! case answer of
    (Left x)  -> "error: " ++ show x 
    (Right y) -> "ok:    " ++ show y 


monadicUnification :: (BindingMonad FTS (STVar s FTS) (ST.STBinding s)) => (forall s. ((Fix FTS) -> (Fix FTS) -> 
  ErrorT (UT.UFailure (FTS) (ST.STVar s (FTS)))
           (ST.STBinding s) (UT.UTerm (FTS) (ST.STVar s (FTS)),
            Map VariableName (ST.STVar s (FTS)))))
monadicUnification t1 t2 = do
--  let
--    t1f = termFlattener t1
--    t2f = termFlattener t2
  (x1,d1) <- lift . translateToUTerm $ t1
  (x2,d2) <- lift . translateToUTerm $ t2
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
             Map VariableName (ST.STVar s FTS)))
     )
  -> [(VariableName, Prolog)]
goUnify test = ST.runSTBinding $ do
  answer <- runErrorT $ test --ERROR
  case answer of
    (Left _)            -> return []
    (Right (_, dict))   -> f1 dict


f1 ::
  (BindingMonad FTS (STVar s FTS) (ST.STBinding s))
  => (forall s. Map VariableName (STVar s FTS)
      -> (ST.STBinding s [(VariableName, Prolog)])
     )
f1 dict = do
  let ld1 = Map.toList dict
  ld2 <- Control.Monad.Error.sequence [ v1 | (k,v) <- ld1, let v1 = UT.lookupVar v]
  let ld3 = [ (k,v) | ((k,_),Just v) <- ld1 `zip` ld2]
      ld4 = [ (k,v) | (k,v2) <- ld3, let v = translateFromUTerm dict v2 ]
  return ld4

{--
fix1 = (Fix $ Struct "a" [(Fix $ Var $ VariableName 0 "x"), 
  (Fix Wildcard), (Fix $ Cut 0), (Fix $ Struct "b" 
    [(Fix $ Var $ VariableName 1 "y"), (Fix Wildcard), 
    (Fix $ Cut 1), (Fix $ Struct "c" [(Fix $ Var $ VariableName 2 "z"), 
      (Fix Wildcard), (Fix $ Cut 2), (Fix $ Struct "d" [])])])])


fix2 = Fix $ Struct "a" [(Fix $ Var $ VariableName 0 "x"), (Fix $ Cut 0), 
    (Fix $ Wildcard)]

fix3 = (Fix $ Var $ VariableName 1 "x")

fix4 = (Fix $ Var $ VariableName 2 "y")
--}

unifierConvertor :: [(VariableName, Prolog)] -> Unifier
unifierConvertor xs = Prelude.map (\(v, p) -> (v, (unFlatten $ unP $ p))) xs 
 

unify :: MonadPlus m => Term -> Term -> m Unifier
unify t1 t2 = unifierConvertor (goUnify (monadicUnification (termFlattener t1) (termFlattener t2)))

--unify_with_occurs_check 