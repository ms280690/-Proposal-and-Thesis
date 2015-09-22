{-# LANGUAGE 	DeriveDataTypeable, 
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
module STVarExperiment where

import Data.Generics (Data(..), Typeable(..))	
import Data.Functor.Fixedpoint as DFF
import Data.Traversable as T
import Data.Foldable as DF
import Control.Applicative ((<$>),(<*>),pure,Applicative)
import Data.List.Extras.Pair

-- Playing with STVar(STRef)
import Control.Unification as U
import Control.Unification.Types as UT
import Control.Unification.STVar as ST 
--import Data.STRef
--import Control.Monad.Fix
--import Control.Monad.Trans.Class
--import Control.Monad.Identity as I
import Control.Monad.Error

import qualified Data.Set as S
import Data.Map as Map
--import Control.Monad.ST
--import Flatten

import Control.Monad.Trans.Except
import Data.IntMap

import Data.Char
--import Data.List


type Atom = String

data VariableName = VariableName Int String  deriving (Show, Eq, Ord)

data FlatTerm a = 
		Struct Atom [a]
	| Var VariableName
	|	Wildcard
	|	Cut Int deriving (Show, Eq, Ord)

{--
(Struct "a" [(Var $ VariableName 0 "x"), (Wildcard), (Cut 0), (Struct "b" [(Var $ VariableName 0 "y"), (Wildcard), (Cut 1), (Struct "c" [(Var $ VariableName 0 "z"), (Wildcard), (Cut 0), (Struct "d" [])])])])

--}

data Prolog = P (Fix FlatTerm) deriving (Show,Eq,Ord)

instance Functor (FlatTerm) where
	fmap 							= T.fmapDefault

instance Foldable (FlatTerm) where
 	foldMap 						= T.foldMapDefault

instance Traversable (FlatTerm) where
  	traverse f (Struct atom x)		=	Struct atom <$> sequenceA (Prelude.map f x)
  	traverse _ (Var v)				=	pure (Var v)
  	traverse _ Wildcard				=	pure (Wildcard)
  	traverse _ (Cut i)				= 	pure (Cut i)

instance Unifiable (FlatTerm) where
	zipMatch (Struct al ls) (Struct ar rs) = 
		if (al == ar) && (length ls == length rs) 
			then Struct al <$> pairWith (\l r -> Right (l,r)) ls rs  		
			else Nothing
	zipMatch Wildcard _ = Just Wildcard
	zipMatch _ Wildcard = Just Wildcard
	zipMatch (Cut i1) (Cut i2) = if (i1 == i2) 
		then Just (Cut i1) 
		else Nothing

instance Applicative (FlatTerm) where
	pure x 									= 	Struct "" [x] 
	_ 				<*> 	Wildcard		= 	Wildcard
	_				<*> 	(Cut i) 		= 	Cut i
	_				<*>		(Var v)			=	(Var v)
	(Struct a fs)	<*> 	(Struct b xs) 	= Struct (a ++ b) [f x | f <- fs, x <- xs] 

variableExtractor :: Fix FlatTerm -> [Fix FlatTerm]
variableExtractor (Fix x) = case x of
	(Struct _ xs) 	->	Prelude.concat $ Prelude.map variableExtractor xs
	(Var v)			->	[Fix $ Var v]
	_				->	[] 

variableNameExtractor :: Fix FlatTerm -> [VariableName]
variableNameExtractor (Fix x) = case x of
	(Struct _ xs)	-> Prelude.concat $ Prelude.map variableNameExtractor xs
	(Var v)			-> [v]
	_ 				-> []	

variableSet :: [Fix FlatTerm] -> S.Set (Fix FlatTerm)
variableSet a = S.fromList a

variableNameSet :: [VariableName] -> S.Set (VariableName)
variableNameSet a = S.fromList a

varsToDictM :: (Ord a, Unifiable t) =>
    S.Set a -> ST.STBinding s (Map a (ST.STVar s t))
varsToDictM set = foldrM addElt Map.empty set where
  addElt sv dict = do
    iv <- freeVar
    return $! Map.insert sv iv dict

{--
uTermify :: Ord a =>
            Map a (ST.STVar s (FlatTerm)) ->
            UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)) ->
            UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm))
--}
uTermify 
  :: Map VariableName (ST.STVar s (FlatTerm)) 
  -> UTerm FlatTerm (ST.STVar s (FlatTerm)) 
  -> UTerm FlatTerm (ST.STVar s (FlatTerm))
uTermify varMap ux = case ux of
  UT.UVar _          		-> ux
  UT.UTerm (Var v)  		-> maybe (error "bad map") UT.UVar $ Map.lookup v varMap
 -- UT.UTerm t         		-> UT.UTerm $! fmap (uTermify varMap) t
  UT.UTerm (Struct a xs)	-> UT.UTerm $ Struct a $! fmap (uTermify varMap) xs  	
  UT.UTerm (Wildcard)		-> UT.UTerm Wildcard
  UT.UTerm (Cut i)			-> UT.UTerm (Cut i)

translateToUTerm ::
    Fix FlatTerm -> ST.STBinding s
            (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
             Map VariableName (ST.STVar s (FlatTerm)))
translateToUTerm e1Term = do
  let vs = variableNameSet $ variableNameExtractor e1Term
  varMap <- varsToDictM vs
  let t2 = uTermify varMap . unfreeze $ e1Term
  return (t2,varMap)


-- | vTermify recursively converts @UVar x@ into @UTerm (VarA x).
-- This is a subroutine of @ translateFromUTerm @.  The resulting
-- term has no (UVar x) subterms.

vTermify :: Map Int VariableName ->
            UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)) ->
            UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm))
vTermify dict t1 = case t1 of
  UT.UVar x  -> maybe (error "logic") (UT.UTerm . Var) $ Map.lookup (UT.getVarID x) dict
  UT.UTerm r ->
    case r of
      Var iv   -> t1
      _         -> UT.UTerm . fmap (vTermify dict) $ r

translateFromUTerm :: 
    Map VariableName (ST.STVar s (FlatTerm)) ->
    UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)) -> Prolog
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

unifyTerms :: Fix FlatTerm -> Fix FlatTerm -> Maybe (Map VariableName (Prolog))
unifyTerms t1 t2 = ST.runSTBinding $ do
  answer <- runExceptT $ unifyTermsX t1 t2
  return $! either (const Nothing) Just answer

-- | Unify two (E1 a) terms resulting in maybe a dictionary
-- of variable bindings (to terms).
--
-- This routine works in the unification monad

unifyTermsX :: 
    Fix FlatTerm -> Fix FlatTerm ->
    ExceptT  (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
        (ST.STBinding s)
        (Map VariableName (Prolog))
unifyTermsX t1 t2 = do
    (x1,d1) <- lift . translateToUTerm $ t1
    (x2,d2) <- lift . translateToUTerm $ t2
    _ <- unify x1 x2
    makeDicts $ (d1,d2)

mapWithKeyM :: (Ord k,Applicative m,Monad m)
               => (k -> a -> m b) -> Map k a -> m (Map k b)
mapWithKeyM = Map.traverseWithKey


makeDict :: 
            Map VariableName (ST.STVar s (FlatTerm)) -> ST.STBinding s (Map VariableName (Prolog))
makeDict sVarDict =
    flip mapWithKeyM sVarDict $ \ _ -> \ iKey -> do
        Just xx <- UT.lookupVar $ iKey
        return $! (translateFromUTerm sVarDict) xx


-- | recover the bindings for the variables of the two terms
-- unified from the monad.

makeDicts :: 
    (Map VariableName (ST.STVar s (FlatTerm)), Map VariableName (ST.STVar s (FlatTerm))) ->
    ExceptT  (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
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
  ErrorT (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
           (ST.STBinding s)
            (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
             Map VariableName (ST.STVar s (FlatTerm)))
test1 = do
    let
        t1a = (Fix $ Var $ VariableName 0 "x")
        t2a = (Fix $ Var $ VariableName 1 "y")
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- unify x1 x2
    return (x3, d1 `Map.union` d2)

test2 ::
  ErrorT (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
           (ST.STBinding s)
            (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
             Map VariableName (ST.STVar s (FlatTerm)))
test2 = do
    let
        t1a = (Fix $ Struct "a" [Fix $ Var $ VariableName 0 "x"])
        t2a = (Fix $ Var $ VariableName 1 "y")
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- unify x1 x2
    return (x3, d1 `Map.union` d2)


test3 ::
  ErrorT (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
           (ST.STBinding s)
            (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
             Map VariableName (ST.STVar s (FlatTerm)))
test3 = do
    let
        t1a = (Fix $ Struct "a" [Fix $ Var $ VariableName 0 "x"])
        t2a = (Fix $ Var $ VariableName 0 "x")
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- unify x1 x2
    return (x3, d1 `Map.union` d2)
{--
goTest test3
"ok:    STVar -9223372036854775807 
[(VariableName 0 \"x\",STVar -9223372036854775808)]"
--}

test4 ::
  ErrorT (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
           (ST.STBinding s)
            (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
             Map VariableName (ST.STVar s (FlatTerm)))
test4 = do
    let
        t1a = (Fix $ Struct "a" [Fix $ Var $ VariableName 0 "x"])
        t2a = (Fix $ Var $ VariableName 0 "x")
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- unifyOccurs x1 x2
    return (x3, d1 `Map.union` d2)
{--
goTest test4
"ok:    STVar -9223372036854775807 
[(VariableName 0 \"x\",STVar -9223372036854775808)]"
--}

test5 ::
  ErrorT (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
           (ST.STBinding s)
            (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
             Map VariableName (ST.STVar s (FlatTerm)))
test5 = do
    let
        t1a = (Fix $ Struct "a" [Fix $ Var $ VariableName 0 "x"])
        t2a = (Fix $ Struct "b" [Fix $ Var $ VariableName 0 "y"])
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- unify x1 x2
    return (x3, d1 `Map.union` d2)


test6 ::
  ErrorT (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
           (ST.STBinding s)
            (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
             Map VariableName (ST.STVar s (FlatTerm)))
test6 = do
    let
        t1a = (Fix $ Struct "a" [Fix $ Var $ VariableName 0 "x"])
        t2a = (Fix $ Struct "a" [Fix $ Var $ VariableName 0 "y"])
    (x1,d1) <- lift . translateToUTerm $ t1a --error
    (x2,d2) <- lift . translateToUTerm $ t2a
    x3 <- unify x1 x2
    return (x3, d1 `Map.union` d2)


stExtract ::
  ErrorT (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
           (ST.STBinding s)
            (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
             Map VariableName (ST.STVar s (FlatTerm)))
stExtract = do
	(x, d) <- lift . translateToUTerm $ fix2
	return (x, d)

goTest :: (Show b) => (forall s . 
	(ErrorT (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
           (ST.STBinding s)
            (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
             Map VariableName (ST.STVar s (FlatTerm))))) -> String
goTest test = ST.runSTBinding $ do
  answer <- runErrorT $ test
  return $! case answer of
    (Left x)  -> "error: " ++ show x
    (Right y) -> "ok:    " ++ show y


monadicUnification :: (BindingMonad FlatTerm (STVar s FlatTerm) (ST.STBinding s)) => (forall s. ((Fix FlatTerm) -> (Fix FlatTerm) -> 
  ErrorT (UT.UFailure (FlatTerm) (ST.STVar s (FlatTerm)))
           (ST.STBinding s) (UT.UTerm (FlatTerm) (ST.STVar s (FlatTerm)),
            Map VariableName (ST.STVar s (FlatTerm)))))
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
  (forall s. (BindingMonad FlatTerm (STVar s FlatTerm) (ST.STBinding s))
  =>
      (ErrorT
          (UT.UFailure FlatTerm (ST.STVar s FlatTerm))
          (ST.STBinding s)
          (UT.UTerm FlatTerm (ST.STVar s FlatTerm),
             Map VariableName (ST.STVar s FlatTerm)))
     )
  -> [(VariableName, Prolog)]
goUnify test = ST.runSTBinding $ do
  answer <- runErrorT $ test --ERROR
  case answer of
    (Left _)            -> return []
    (Right (_, dict))   -> f1 dict


f1 ::
  (BindingMonad FlatTerm (STVar s FlatTerm) (ST.STBinding s))
  => (forall s. Map VariableName (STVar s FlatTerm)
      -> (ST.STBinding s [(VariableName, Prolog)])
     )
f1 dict = do
  let ld1 = Map.toList dict
  ld2 <- Control.Monad.Error.sequence [ v1 | (k,v) <- ld1, let v1 = UT.lookupVar v]
  let ld3 = [ (k,v) | ((k,_),Just v) <- ld1 `zip` ld2]
      ld4 = [ (k,v) | (k,v2) <- ld3, let v = translateFromUTerm dict v2 ]
  return ld4

fix1 = (Fix $ Struct "a" [(Fix $ Var $ VariableName 0 "x"), 
	(Fix Wildcard), (Fix $ Cut 0), (Fix $ Struct "b" 
		[(Fix $ Var $ VariableName 1 "y"), (Fix Wildcard), 
		(Fix $ Cut 1), (Fix $ Struct "c" [(Fix $ Var $ VariableName 2 "z"), 
			(Fix Wildcard), (Fix $ Cut 2), (Fix $ Struct "d" [])])])])


fix2 = Fix $ Struct "a" [(Fix $ Var $ VariableName 0 "x"), (Fix $ Cut 0), 
		(Fix $ Wildcard)]

fix3 = (Fix $ Var $ VariableName 1 "x")

fix4 = (Fix $ Var $ VariableName 2 "y")


{--
prettyDisplay (UT.UTerm x,  m)  =   show x ++ " " ++ variableMapPrinter m
prettyDisplay (UT.UVar  v,  m)  =   show v ++ " " ++ variableMapPrinter m 

variableMapPrinter :: (Show a, Show k) => Map k a -> String
variableMapPrinter m = show $ Map.toList m


instance Show VariableName where
  show (VariableName i (x:xs)) = "Var " ++ show ((toUpper x) : xs)  
                      ++ " Val " ++ show i    

instance Show (Fix FlatTerm) where
  show (Fix (Struct atom xs)) = " Struct " ++ atom ++ " ["  ++ 
                  Prelude.concat (Prelude.map show xs) ++ "] "                
  show (Fix (Var v))      = " FlatVar " ++ show v ++ " "
  show (Fix (Cut i))      = " Cut " ++ show i ++ " ,"
  show (Fix (Wildcard))   = " Wildcard ," 

fixflatTermPrettyPrint :: Fix FlatTerm -> String
fixflatTermPrettyPrint (Fix (Struct a xs))  = " Struct " ++ a ++ " ["
          ++ Prelude.concat (Prelude.map fixflatTermPrettyPrint xs) 
          ++ "] "
fixflatTermPrettyPrint (Fix (Var v))    = " FlatVar " ++ show v
fixflatTermPrettyPrint (Fix (Cut i))    = " Cut " ++ show i
fixflatTermPrettyPrint (Fix Wildcard)   = " Wildcard "
--}





























{--
variablesOf t = listFilter $ DF.concatMap variableExtractor t

--listFilter :: [FlatTerm t] -> [FlatTerm t]
listFilter []	= []
listFilter (x:xs) = case x of
	(Var v)		-> x : (listFilter xs)
	otherwise	-> listFilter xs

--variableExtractor :: FlatTerm t -> [FlatTerm a]
variableExtractor (Struct a xs) 		= 	subtermEvaluator xs
variableExtractor (Var v)				= 	[Var v]
variableExtractor _						= 	[]

--subtermEvaluator :: [FlatTerm t] -> [FlatTerm a]
subtermEvaluator []					= []
subtermEvaluator (x:xs)				= case x of
	(Var v)				->	(Var v) : subtermEvaluator xs
	(Struct a zs)		->	subtermEvaluator xs ++ zs  
	otherwise			-> 	subtermEvaluator xs

q x = [x]

--translateFlat (Struct a [])		= UTerm		(Struct a [])	
translateFlat (Var v)			= UVar 		(Var v)
translateFlat (Wildcard)		= UTerm 	(Wildcard)	
translateFlat (Cut i)			= UTerm 	(Cut i)
--translateFlat s 				= UTerm		(fmap translateFlat s)

subtermTranslator [] 		= []
subtermTranslator (x:xs)	= case x of
	(Var v)				-> (UVar $ Var v) 								: (subtermTranslator xs)
	(Wildcard)			-> (UTerm $ Wildcard) 							: (subtermTranslator xs)
	(Cut i)				-> (UTerm $ Cut i)								: (subtermTranslator xs)	
--	(Struct a zs) 		-> (UTerm $ Struct a (subtermTranslator zs))	: (subtermTranslator xs)


tf2 t =	case t of 
	UVar _ 	-> t
	UTerm r -> case r of 
		Var v 		-> UVar $ Var v
		Wildcard	-> UTerm $ Wildcard
		Cut i 		-> UTerm $ Cut i
		otherwise 	-> UTerm $ fmap tf2 r

{--
translateToFix (Struct a [])	= Fix (Struct a [])
--translateToFix (Struct a xs)	= Fix (Struct a (fmap translateToFix xs))
translateToFix (Var v) 			= Fix (Var v)
translateToFix (Wildcard)		= Fix (Wildcard)
translateToFix (Cut i)			= Fix (Cut i)
--}

--}

{--
variableNameToDictM :: (Ord a, Unifiable t) =>
    S.Set a -> ST.STBinding s (Map a (ST.STVar s t))
variableNameToDictM set = foldrM addElt Map.empty set where
  addElt sv dict = do
    iv <- freeVar
    return $! Map.insert (sv) iv dict
--}


{--
translateToUTerm :: Fix FlatTerm -> UTerm FlatTerm (FlatTerm a)
translateToUTerm (Fix x) = case x of
	Var v				-> UVar $ Var v
	Struct a xs			-> UTerm $ Struct a (Prelude.map translateToUTerm xs)
	Wildcard			-> UTerm Wildcard
	Cut i				-> UTerm $ Cut i
--}
--variableTranslator v varMap = do

{--
translateToUTerm ft = do
  let vs = variableSet ft
  varMap <- varsToDictM vs
  let t2 = uTermify varMap . U.unfreeze ft
  return (t2,varMap)
--}

{--
variableDictionary vSet = foldrM entryInsert Map.empty vSet

--entryInsert :: (Ord k, BindingMonad t a m) => k -> Map k a -> m (Map k (ST.STVar a s))
entryInsert flatVar dict = do
	stVar <- freeVar
	return $! Map.insert flatVar stVar dict

variableTranslator x = do
	x <- newVar (UVar x)
	y <- _newSTVar "a" Nothing	
	z <- freeVar
	return z
--}
