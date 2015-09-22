module Expt3 where

import Data.Functor.Fixedpoint
import Data.Foldable
import Data.Set(Set)
import qualified Data.Set as Set
import Data.Map(Map)
import qualified Data.Map as Map
import Data.Traversable
import qualified Control.Unification.Types as U
import qualified Control.Unification.IntVar as IV
import Control.Applicative((<$>),(<*>),pure,Applicative)

import Prelude hiding (foldr)
import Control.Monad.Identity (Identity)
import Control.Monad.Identity (runIdentity)

import Control.Monad.Trans.Class (lift)


data Term = Add Factor Term
          | Sub Term Factor
          | Factor Factor
                                        deriving Show
{--
Factor (Atom (Constant 5))

Sub (Factor (Atom (Constant 5 ))) (Atom (Constant 4))

Add (Atom (Constant 5)) (Factor (Atom (Constant 4)))
--}

data Factor = Mul Factor Atom
            | Div Factor Atom
            | Atom Atom
                                        deriving Show
{--
:t Atom (Constant 5)
Atom (Constant 5) :: Factor

Mul (Atom (Constant 5)) (Constant 4)

Div (Atom (Constant 5)) (Constant 4)
--}

data Atom = Bracket Term
          | Var Variable
          | Constant Int                deriving Show
{--
Constant 5

Var ("X" :: Variable )
Var "X"

Bracket (Factor (Atom (Constant 5)))
--}

data Expr = Term1 Term
          | Factor1 Factor
          | Atom1 Atom                   deriving Show

{--
Term1 (Sub (Factor (Atom (Constant 5 ))) (Atom (Constant 4)))

Factor1 (Atom (Constant 5))

Atom1 (Constant 5)
--}

type Variable = String
{--
:t ("X" :: Variable )
("X" :: Variable ) :: Variable
--}

data E v t   =
               AddE t t
             | SubE t t
             | MulE t t
             | DivE t t
             | BracketE t
             | ConstantE Int
             | VarA v                   deriving Show
{--
AddE (Factor $ Atom $ Constant 5) (Factor $ Atom $ Constant 4)

SubE (Factor $ Atom $ Constant 5) (Factor $ Atom $ Constant 4)

DivE (Factor $ Atom $ Constant 5) (Factor $ Atom $ Constant 4)

MulE (Factor $ Atom $ Constant 5) (Factor $ Atom $ Constant 4)

BracketE (Factor $ Atom $ Constant 5)

VarA (Var ("X" :: Variable))
--}


newtype E1 v = E1 { unE1 :: Fix (E v) } deriving Show

{--
E1 $ Fix $ ConstantE 5
E1 {unE1 = ConstantE 5}

--}


instance Functor (E v) where
  fmap g data1 = case data1 of
    AddE t1 t2      -> AddE (g t1) (g t2)
    SubE t1 t2      -> SubE (g t1) (g t2)
    MulE t1 t2      -> MulE (g t1) (g t2)
    DivE t1 t2      -> DivE (g t1) (g t2)
    BracketE t      -> BracketE (g t)
    ConstantE x     -> ConstantE x
    VarA v          -> VarA v

{--


--}

instance Foldable (E v) where
  foldr g zero data1 = foldx g data1 zero where
     foldx :: (t -> b -> b) -> E v t -> b -> b
     foldx g data1 = case data1 of
       AddE t1 t2      -> (g t1) . (g t2)
       SubE t1 t2      -> (g t1) . (g t2)
       MulE t1 t2      -> (g t1) . (g t2)
       DivE t1 t2      -> (g t1) . (g t2)
       BracketE t      -> (g t)
       ConstantE _     -> id
       VarA _          -> id

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

{-
instance Functor E1 where
  fmap g = E1 . ffmap g . unE1 where
    ffmap :: (u -> v) -> Fix (E u) -> Fix (E v)
    ffmap g =  Fix . fffmap g . unFix
    fffmap g data1 = case data1 of
        VarA v          -> VarA $ g v
        AddE t1 t2      -> AddE (ffmap g t1) (ffmap g t2)
        SubE t1 t2      -> SubE (ffmap g t1) (ffmap g t2)
        MulE t1 t2      -> MulE (ffmap g t1) (ffmap g t2)
        DivE t1 t2      -> DivE (ffmap g t1) (ffmap g t2)
        BracketE t      -> BracketE (ffmap g t)
        ConstantE i     -> ConstantE i

-}

functorV :: (u -> v) -> E u t -> E v t
functorV g data1 = case data1 of
    VarA v          -> VarA $ g v
    AddE t1 t2      -> AddE t1 t2
    SubE t1 t2      -> SubE t1 t2
    MulE t1 t2      -> MulE t1 t2
    DivE t1 t2      -> DivE t1 t2
    BracketE t      -> BracketE t
    ConstantE i     -> ConstantE i

instance Functor E1 where
  fmap g = E1 . hmap (functorV g) . unE1


type UnFix v = E v (Fix (E v))
type UnE1  v = Fix (E v)
type Unary v = v -> v
type Binary v = v -> v -> v

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


transl :: Expr -> E1 Variable
transl e = case e of
      Term1 t        -> translT  t
      Factor1 f      -> translF  f
      Atom1 a        -> translA  a
 where
   translA :: Atom   -> E1 Variable
   translF :: Factor -> E1 Variable
   translT :: Term   -> E1 Variable

   f1 constr x      = E1 . Fix $ constr (unE1 x)
   f2 constr x y    = E1 . Fix $ constr (unE1 x) (unE1 y)
   addE             = f2 AddE
   subE             = f2 SubE
   mulE             = f2 MulE
   divE             = f2 DivE
   bracketE         = f1 BracketE
   constantE        = E1 . Fix . ConstantE
   varA             = E1 . Fix . VarA

   translA (Bracket t) = bracketE . translT $ t
   translA (Constant i) = constantE i
   translA (Var v)     = varA v

   translF (Mul f a)  = mulE (translF f) (translA a)
   translF (Div f a)  = divE (translF f) (translA a)
   translF (Atom a)   = translA a

   translT (Add f t)  = addE (translF f) (translT t)
   translT (Sub t f)  = subE (translT t) (translF f)
   translT (Factor f) = translF f
{--
transl $ Atom1 (Constant 1) 
E1 {unE1 = ConstantE 1}
--}


untransl :: E1 Variable -> Expr
untransl = untransl_1 . unE1 where
   untransl_1 e = case unFix e of
       AddE t1 t2      -> addE t1 t2
       SubE t1 t2      -> subE t1 t2
       MulE t1 t2      -> mulE t1 t2
       DivE t1 t2      -> divE t1 t2
       BracketE t      -> bracketE t
       ConstantE i     -> constantE i
       VarA v          -> varA v

   l2 :: (a -> Expr) -> (b -> c -> a) -> (Expr -> b) -> (Expr -> c)
         -> (Fix (E Variable) -> Fix (E Variable) -> Expr)
   l2 h g f1 f2 x1 x2  = h $ g (f1 . untransl_1 $ x1) (f2 . untransl_1 $ x2)
   l1 h g f1    x1     = h $ g (f1 . untransl_1 $ x1)
   addE                = l2 Term1          Add     xFactor xTerm
   subE                = l2 Term1          Sub     xTerm   xFactor
   mulE                = l2 Factor1        Mul     xFactor xAtom
   divE                = l2 Factor1        Div     xFactor xAtom
   bracketE            = l1 Atom1          Bracket xTerm
   constantE           = Atom1 . Constant
   varA                = Atom1 . Var

   xTerm (Term1 t)     = t
   xTerm e             = Factor . xFactor $ e

   xFactor (Factor1 f) = f
   xFactor e           = Atom . xAtom $ e

   xAtom (Atom1 a)     = a
   xAtom e             = Bracket . xTerm $ e
{--
untransl $ transl $ Atom1 (Constant 1)
Atom1 (Constant 1)
--}


--variablesOf :: Ord a => E1 a -> Set a
variablesOf t = foldMap Set.singleton t
{--
variablesOf $ E1 $ Fix $ ConstantE 1 
fromList []
--}

varsToDict :: Ord a => Set a -> Map a IV.IntVar
varsToDict vSet =  fst. runIdentity . IV.runIntBindingT $ qset where
  ff :: (Ord a) => Set a -> IV.IntBindingT E1 Identity (Map a IV.IntVar)
  ff = varsToDictM
  qset = ff vSet

varsToDictM :: (Ord a, U.Unifiable t) =>
    Set a -> IV.IntBindingT t Identity (Map a IV.IntVar)
varsToDictM set = foldrM addElt Map.empty set where
  addElt sv dict = do
    iv <- U.freeVar
    return $! Map.insert sv iv dict
    

varsToDictM2 :: (Ord a, U.Unifiable t) =>
    IV.IntBindingT t Identity (Set a)
    -> IV.IntBindingT t Identity (Map a IV.IntVar)
varsToDictM2 set = do { set' <- set ; varsToDictM set' }

uTermify :: U.UTerm (E IV.IntVar) IV.IntVar ->
            U.UTerm (E IV.IntVar) IV.IntVar
uTermify t1 = case t1 of
  U.UVar _  -> t1
  U.UTerm r ->
    case r of
      VarA iv   -> U.UVar iv
      _         -> U.UTerm . fmap uTermify $ r

translateToUTerm :: (Ord a) =>
    E1 a -> IV.IntBindingT (E IV.IntVar)
            Identity
            (U.UTerm (E IV.IntVar) IV.IntVar, Map a IV.IntVar)
translateToUTerm e1Term = do
  let vs = variablesOf e1Term
  varMap <- varsToDictM vs
  let trV v = case Map.lookup v varMap of
        Nothing -> error "varMap incomplete"
        Just iv -> iv
      t2 = uTermify . U.unfreeze . hmap (functorV trV) . unE1 $ e1Term
  return (t2,varMap)
