{-# LANGUAGE TemplateHaskell, FlexibleContexts, RankNTypes #-}
{-|
Module          : Expt3
Description     : experiments in using the unification-fd monad
Copyright       : (c) 2014 David Casperson
Maintainer      : David.Casperson@unbc.ca
Stability       : experimental

This module contains two or three different languages.

`Term', `Factor', `Atom', `Expr' `Variable' provide a concrete syntax
that might correspond fairly directly to a small language.  There is
no anti-quoting mechanism or parser yet.

The data type `E' provides the structure of the above language in a
/non-recursive/ fashion.  This is almost impossible to use "raw" for
type related reasons.  For instance

> AddE (VarA "x") (ConstantE 3)

has type @E a (E String b)@, but

> AddE (VarA "x") (MulE (ConstantE 3) (VarA "y"))

has type @E a (E String (E String t))@.

The data type `E1' provides the fixed point of the (E v) type.  The
functions @transl@ and @untransl@ map between the concrete language
and the E1 language.

Both (E v) and E1 have various category theoretic properties.

E v is an instance of
    * Functor
    * Foldable
    * Traversable
    * Unifiable

The type function @\ t -> E v t@ doesn't have a name, but if it did,
it would be a Functor.  @functorV@ is the fmap function for that
Functor.

E1 is an instance of
    * Functor
    * Applicative (note that E v is NOT.)
    * Foldable
    * Traversable
    * Unifiable

-}

module Expt4 where

import Data.Functor.Fixedpoint
import Data.Foldable
import Data.Set(Set)
import qualified Data.Maybe
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Traversable
import qualified Control.Unification as U
import qualified Control.Unification.Types as UT
import qualified Control.Unification.STVar as SV
import Control.Applicative((<$>),(<*>),pure,Applicative)

import Prelude hiding (foldr)
import Control.Monad.Identity (Identity)
import Control.Monad.Identity (runIdentity)
--import Control.Monad.Error (runExceptT , ExceptT )
import Control.Monad.Trans.Class (lift)



----------------------------------------------------------------------
import Control.Monad.Trans.Except
----------------------------------------------------------------------


{-
    Term, Factor, Atom, Expr, Variable sublanguage.
    translate from/to with transl / untransl
-}

data Term = Add Factor Term
          | Sub Term Factor
          | Factor Factor
                                        deriving (Show, Ord, Eq)
data Factor = Mul Factor Atom
            | Div Factor Atom
            | Atom Atom
                                        deriving (Show, Ord, Eq)
data Atom = Bracket Term
          | Var Variable
          | Constant Int                deriving (Show, Ord, Eq)


data Expr = Term1 Term
          | Factor1 Factor
          | Atom1 Atom                   deriving (Show, Ord, Eq)

type Variable = String


{-
    E v and Fix (E v) are the kind of language expected by
    the unification libraries.
-}

data E v t   =
               AddE t t
             | SubE t t
             | MulE t t
             | DivE t t
             | BracketE t
             | ConstantE Int
             | VarA v                   deriving (Show, Ord, Eq)


newtype E1 v = E1 { unE1 :: Fix (E v) } deriving (Show, Ord, Eq)




{-========================================================================-}
  -- instances for (E v) :
  -- Functor, Foldable, Traversable, U.Unifiable
{-========================================================================-}




instance Functor (E v)  where fmap = fmapDefault
instance Foldable (E v) where foldMap = foldMapDefault

instance Traversable (E v) where
  traverse g data1 = case data1 of
    AddE t1 t2      -> AddE <$> (g t1) <*> (g t2)
    SubE t1 t2      -> SubE <$> (g t1) <*> (g t2)
    MulE t1 t2      -> MulE <$> (g t1) <*> (g t2)
    DivE t1 t2      -> DivE <$> (g t1) <*> (g t2)
    BracketE t      -> BracketE <$> (g t)
    ConstantE i     -> pure (ConstantE i)
    VarA v          -> pure (VarA v)

instance U.Unifiable (E v) where
  zipMatch t1 t2 = result where
    noMatch _ _ = Nothing
    match x1 _  = Just (Left <$> x1)
    pair  x1 = Just  (Right <$> x1)
    result = case (t1, t2) of
      (AddE a1 a2, AddE b1 b2) -> pair (AddE (a1,b1) (a2,b2))
      (SubE a1 a2, SubE b1 b2) -> pair (SubE (a1,b1) (a2,b2))
      (MulE a1 a2, MulE b1 b2) -> pair (MulE (a1,b1) (a2,b2))
      (DivE a1 a2, DivE b1 b2) -> pair (DivE (a1,b1) (a2,b2))
      (BracketE a0, BracketE b0) -> pair (BracketE (a0,b0))
      (ConstantE j, ConstantE i )       -> (if i==j
                                            then match else noMatch)
                                             t1 t2
      (VarA _     , VarA _      )       -> match t1 t2
      _                                 -> noMatch t1 t2

functorV :: (u -> v) -> E u t -> E v t
functorV g data1 = case data1 of
    VarA v          -> VarA $ g v
    AddE t1 t2      -> AddE t1 t2
    SubE t1 t2      -> SubE t1 t2
    MulE t1 t2      -> MulE t1 t2
    DivE t1 t2      -> DivE t1 t2
    BracketE t      -> BracketE t
    ConstantE i     -> ConstantE i


type UnFix v = E v (Fix (E v))
type UnE1  v = Fix (E v)
type Unary v = v -> v
type Binary v = v -> v -> v



{-========================================================================-}
  -- instances for E1 v :
  -- Functor, Applicative, Foldable, Traversable, U.Unifiable
{-========================================================================-}

instance Functor E1 where
  fmap g = E1 . hmap (functorV g) . unE1

instance Applicative E1 where
  pure = E1 . Fix . VarA
  f <*> x = join $ (<$> x) <$> f where
    join :: (E1 (E1 v)) -> E1 v
    join (E1 (Fix y)) = case y of
        VarA v          -> v
        AddE t1 t2      -> c2 AddE t1 t2
        SubE t1 t2      -> c2 SubE t1 t2
        MulE t1 t2      -> c2 MulE t1 t2
        DivE t1 t2      -> c2 DivE t1 t2
        BracketE t      -> c1 BracketE t
        ConstantE i     -> E1 . Fix . ConstantE $ i
    xjoin = unE1 . join . E1
    c2 ff a b = E1 . Fix $ ff (xjoin a) (xjoin b)
    c1 ff a   = E1 . Fix $ ff (xjoin a)


instance Foldable E1 where
  foldr g zero (E1 data1) = let
    h data2 acc = foldr g acc (E1 data2)
    in case unFix data1 of
        AddE t1 t2      -> (h t1) . (h t2) $ zero
        SubE t1 t2      -> (h t1) . (h t2) $ zero
        MulE t1 t2      -> (h t1) . (h t2) $ zero
        DivE t1 t2      -> (h t1) . (h t2) $ zero
        BracketE t      -> h t zero
        ConstantE _     -> zero
        VarA v          -> g v zero

instance Traversable E1 where
  traverse f data1 = E1 <$> traverse1 (unE1 data1) where
    traverse1 data2 = case unFix data2 of
        AddE t1 t2      -> y2 AddE t1 t2
        SubE t1 t2      -> y2 SubE t1 t2
        MulE t1 t2      -> y2 MulE t1 t2
        DivE t1 t2      -> y2 DivE t1 t2
        BracketE t      -> y1 BracketE t
        ConstantE i     -> pure . Fix . ConstantE $ i
        VarA v          -> x1 VarA <$> f v
      where
        y2 g a b = x2 g <$> (traverse1 a) <*> (traverse1 b)
        y1 g a   = x1 g <$> (traverse1 a)
        x2 g a b = Fix (g a b)
        x1 g a   = Fix (g a  )


instance U.Unifiable E1 where
  zipMatch t1 t2 = action t1 t2 where
    noMatch _ _ = Nothing
    match x1 _  = Just (Left <$> x1)
    pair  x1 x2 = Just $ xpair x1 x2
    xpair x1 x2 = Right <$> ( (,) <$> x1 <*> x2)
    action = case (unFix . unE1 $ t1, unFix . unE1 $ t2) of
      (AddE {}, AddE {})                -> pair
      (SubE {}, SubE {})                -> pair
      (MulE {}, MulE {})                -> pair
      (DivE {}, DivE {})                -> pair
      (BracketE _ , BracketE _ )        -> pair
      (ConstantE j, ConstantE i )       -> if i==j then match else noMatch
      (VarA _     , VarA _      )       -> pair
      _                                 -> noMatch



{-========================================================================-}
  -- translation tools
{-========================================================================-}

transl :: Expr -> E1 Variable
transl = E1 . ana transl_E where
  transl_E :: Expr -> E Variable Expr
  transl_E (Term1 t)      = transl_T t
  transl_E (Factor1 f)    = transl_F f
  transl_E (Atom1 a)      = transl_A a

  transl_T :: Term -> E Variable Expr
  transl_T t1 = case t1 of
    Add f t     -> AddE (Factor1 f) (Term1 t)
    Sub t f     -> SubE (Term1 t) (Factor1 f)
    Factor f    -> transl_F f

  transl_F :: Factor -> E Variable Expr
  transl_F f1 = case f1 of
    Mul f a     -> MulE (Factor1 f) (Atom1 a)
    Div f a     -> DivE (Factor1 f) (Atom1 a)
    Atom  a     -> transl_A a

  transl_A :: Atom -> E Variable Expr
  transl_A a1 = case a1 of
    Bracket t       -> BracketE (Term1 t)
    Constant i      -> ConstantE i
    Var v           -> VarA v


untransl :: E1 Variable -> Expr
untransl = cata untransl_E . unE1 where
  untransl_E :: E Variable Expr -> Expr
  untransl_E e = case e of
       AddE t1 t2      -> Term1   $ Add (e2f t1) (e2t t2)
       SubE t1 t2      -> Term1   $ Sub (e2t t1) (e2f t2)
       MulE t1 t2      -> Factor1 $ Mul (e2f t1) (e2a t2)
       DivE t1 t2      -> Factor1 $ Div (e2f t1) (e2a t2)
       BracketE t      -> Atom1   $ Bracket (e2t t)
       ConstantE i     -> Atom1   $ Constant i
       VarA v          -> Atom1   $ Var v
  e2t :: Expr -> Term
  e2t xx = case xx of
    Term1 t         -> t
    Factor1 (Atom a)-> e2t (Atom1 a)
    Factor1 f       -> Factor f
    Atom1 (Bracket t)-> t
    Atom1 a         -> (Factor . Atom) $ a
  e2f :: Expr -> Factor
  e2f xx = case xx of
    Term1 (Factor f)    -> f
    Term1 t             -> Atom $ Bracket $ t
    Factor1 f           -> f
    Atom1 (Bracket t)   -> e2f (Term1 t)
    Atom1 a             -> Atom a
  e2a :: Expr -> Atom
  e2a xx = case xx of
    Term1 (Factor f)    -> e2a (Factor1 f)
    Term1 t             -> Bracket $ t
    Factor1 (Atom a)    -> a
    Factor1 f           -> Bracket $! Factor $ f
    Atom1 a             -> a


{-========================================================================-}
  -- Interface with unification library
{-========================================================================-}


-- | return the VarA variables in an E1 term
variablesOf :: Ord a => E1 a -> Set a
variablesOf t = foldMap Set.singleton t


-- | monadicly find new IntVar s for each old variable name
-- and create a Dictionary (Map) from old names to new ones
varsToDictM :: (Ord a, U.Unifiable t) =>
    Set a -> SV.STBinding s (Map a (SV.STVar s t))
varsToDictM set = foldrM addElt Map.empty set where
  addElt sv dict = do
    iv <- U.freeVar
    return $! Map.insert sv iv dict

-- | uTermify recursively converts @UTerm (VarA x)@ into @UVar x@.
-- This is a subroutine of @ translateToUTerm @.  The resulting
-- term has no (VarA x) subterms.
uTermify :: Ord a =>
            Map a (SV.STVar s (E a)) ->
            U.UTerm (E a) (SV.STVar s (E a)) ->
            U.UTerm (E a) (SV.STVar s (E a))
uTermify varMap ux = case ux of
  UT.UVar v          -> ux
  UT.UTerm (VarA v)  -> maybe (error "bad map") UT.UVar $ Map.lookup v  varMap
  UT.UTerm t         -> UT.UTerm $! fmap (uTermify varMap) t


-- | vTermify recursively converts @UVar x@ into @UTerm (VarA x).
-- This is a subroutine of @ translateFromUTerm @.  The resulting
-- term has no (UVar x) subterms.
vTermify :: Map Int a ->
            U.UTerm (E a) (SV.STVar s (E a)) ->
            U.UTerm (E a) (SV.STVar s (E a))
vTermify dict t1 = case t1 of
  U.UVar x  -> maybe (error "logic") (U.UTerm . VarA) $ Map.lookup (UT.getVarID x) dict
  U.UTerm r ->
    case r of
      VarA iv   -> t1
      _         -> U.UTerm . fmap (vTermify dict) $ r

-- | translate an (E1 a) term into a U.UTerm (E IV.IntVar) IV.IntVar
-- and capture the @a@ to IV.IntVar dictionary used to translate the
-- the variables.  Happens in a monad.
translateToUTerm :: (Ord a) =>
    E1 a -> SV.STBinding s
            (U.UTerm (E a) (SV.STVar s (E a)),
             Map a (SV.STVar s (E a)))
translateToUTerm e1Term = do
  let vs = variablesOf e1Term
  varMap <- varsToDictM vs
  let t2 = uTermify varMap . U.unfreeze . unE1 $ e1Term
  return (t2,varMap)

-- | Using an @a@ to (SV.STVar s t) dictionary, translate a
-- U.UTerm (E (SV.STVar s t)) (SV.STVar s t) back into  an (E1 a) term.
-- Does not require a monad.
translateFromUTerm :: (Ord a) =>
    Map a (SV.STVar s (E a)) ->
    U.UTerm (E a) (SV.STVar s (E a)) -> (E1 a)
translateFromUTerm dict uTerm =
  E1 .  maybe (error "Logic") id . U.freeze . vTermify varIdDict $ uTerm where
    forKV dict initial fn = Map.foldlWithKey' (\a k v -> fn k v a) initial dict
    varIdDict = forKV dict Map.empty $ \ k v -> Map.insert (UT.getVarID v) k


-- | Unify two (E1 a) terms resulting in maybe a dictionary
-- of variable bindings (to terms).
--
-- NB !!!!
-- The current interface assumes that the variables in t1 and t2 are
-- disjoint.  This is likely a mistake that needs fixing
unifyTerms :: (Ord a) => E1 a -> E1 a -> Maybe (Map a (E1 a))
unifyTerms t1 t2 = SV.runSTBinding $ do
  answer <- runExceptT $ unifyTermsX t1 t2
  return $! either (const Nothing) Just answer

-- | Unify two (E1 a) terms resulting in maybe a dictionary
-- of variable bindings (to terms).
--
-- This routine works in the unification monad

unifyTermsX :: (Ord a) =>
    E1 a -> E1 a ->
    ExceptT  (UT.UFailure (E a) (SV.STVar s (E a)))
        (SV.STBinding s)
        (Map a (E1 a))
unifyTermsX t1 t2 = do
    (x1,d1) <- lift . translateToUTerm $ t1
    (x2,d2) <- lift . translateToUTerm $ t2
    _ <- U.unify x1 x2
    makeDicts $ (d1,d2)

mapWithKeyM :: (Ord k,Applicative m,Monad m)
               => (k -> a -> m b) -> Map k a -> m (Map k b)
mapWithKeyM = Map.traverseWithKey

-- | given a dictionary mapping variable names to (SV.STVar s t)'s,
-- consult the monad to create a dictionary whose entries are
-- for variables that have non-trivial bindings in the monad.
-- We assume that (a) all the (SV.STVar s t)'s occurring in
-- intDict have had their bindings applied,
-- and (b) that all remaining free (SV.STVar s t)'s are in the intDict.

makeDict :: (Ord a) =>
            Map a (SV.STVar s (E a)) -> SV.STBinding s (Map a (E1 a))
makeDict sVarDict =
    flip mapWithKeyM sVarDict $ \ _ -> \ iKey -> do
        Just xx <- UT.lookupVar $ iKey
        return $! (translateFromUTerm sVarDict) xx

-- | recover the bindings for the variables of the two terms
-- unified from the monad.
makeDicts :: (Ord a) =>
    (Map a (SV.STVar s (E a)), Map a (SV.STVar s (E a))) ->
    ExceptT  (UT.UFailure (E a) (SV.STVar s (E a)))
    (SV.STBinding s) (Map a (E1 a))
makeDicts (svDict1, svDict2) = do
  let svDict3 = (svDict1 `Map.union` svDict2)
  let ivs = map U.UVar . Map.elems $ svDict3
  U.applyBindingsAll ivs
  -- the interface below is dangerous because Map.union is left-biased.
  -- variables that are duplicated across terms may have different
  -- bindings because `translateToUTerm` is run separately on each
  -- term.
  lift . makeDict $ svDict3




----------------------------------------------------
----------------------------------------------------
{--
SV.runSTBinding $ return $ varsToDictM $ variablesOf $ E1 $ Fix $ VarA $ Var "x"
  :: UT.Unifiable t => SV.STBinding s (Map Atom (SV.STVar s t))
--}

