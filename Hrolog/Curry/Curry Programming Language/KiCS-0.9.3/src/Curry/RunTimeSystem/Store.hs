{-# LANGUAGE DeriveDataTypeable #-}

module Curry.RunTimeSystem.Store
  (Store,
  
   emptyStore,changeStore, storeSize,


   OrRef,OrRefKind(..),
   deref,genInfo,cover,uncover,mkRef,isCovered,
   manipulateStore,

   mkRefWithGenInfo,equalFromTo,
   
   isGenerator, isConstr,updRef, 


   narrowOrRef
   ) where

import Data.Generics (Data,Typeable)
import Data.IntMap
import Prelude hiding (lookup)
import System.IO.Unsafe

trace s x = unsafePerformIO (putStrLn s >> return x) 
trace' x = trace (show x) x

----------------------------
-- or references
----------------------------

data OrRefKind = Generator Int Int | Narrowed Int Int | NoGenerator
                 deriving (Data,Typeable,Eq,Ord,Show,Read)

minMax :: OrRefKind -> (Int->Entry,Maybe (Int,Int))
minMax NoGenerator     = (Choice,Nothing)
minMax (Generator a b) = (Binding a b,Just (a,b))
minMax (Narrowed a b)  = (Binding a b,Just (a,b))

data OrRef = OrRef OrRefKind Int 
           | Layer OrRef 
           | Equality Int Int Int Int Int Int deriving (Data,Typeable,Eq,Ord,Show,Read)

uncover :: OrRef -> OrRef
uncover (Layer x)   = x
uncover x           = x

-- constructors
cover :: OrRef -> OrRef
cover = Layer

mkRef :: Int -> Int -> Int -> OrRef
mkRef i j = OrRef (Generator i (i+j-1))

mkRefWithGenInfo :: OrRefKind -> Int -> OrRef
mkRefWithGenInfo = OrRef

-- selectors
deref :: OrRef -> Int
deref r = case uncover r of
  OrRef _ i -> i
  _         -> (-42)
  
genInfo :: OrRef -> (Int,Int,Int)
genInfo r = case uncover r of
  OrRef (Generator i j) k -> (i,j,k)

--refKind :: OrRef -> OrRefKind
--refKind r = (\ (OrRef x _) -> x) (uncover r)

-- tester
isCovered :: OrRef -> Bool
isCovered (Layer _)   = True
isCovered _           = False

isGenerator :: OrRef -> Bool
isGenerator r = case uncover r of
  OrRef (Generator _ _) _ -> True
  _                       -> False


--operations
updKind :: (OrRefKind -> OrRefKind) -> OrRef -> OrRef
updKind f (Layer r)   = Layer (updKind f r)
updKind f (OrRef k i) = OrRef (f k) i
updKind f c@(Equality _ _ _ _ _ _) = c

updRef :: (Int -> Int) -> OrRef -> OrRef
updRef f (Layer r)   = Layer (updRef f r)
updRef f (OrRef k i) = OrRef k (f i)
updRef f c@(Equality _ _ _ _ _ _) = c


narrowOrRef :: OrRef -> OrRef
narrowOrRef = updKind narrow
  where 
    narrow o@NoGenerator    = o
    narrow o@(Narrowed _ _)= o
    narrow (Generator i j) = Narrowed i j


equalFromTo :: Int -> Int -> Int -> Int -> Int -> Int -> OrRef 
equalFromTo = Equality

isConstr :: OrRef -> Bool
isConstr (Equality _ _ _ _ _ _)  = True
isConstr _                       = False

-------------------------------------------------------
-- finally: the store
-------------------------------------------------------
-- negative numbers are references to other variables
-------------------------------------------------------

data Entry = Equal Int
           | Choice Int
           | Binding Int Int Int deriving (Eq,Ord,Show)

choice :: Entry -> Int
choice (Choice i) = i
choice (Binding _ _ i) = i

newtype Store = Store (IntMap Entry) deriving (Eq,Ord,Show)

emptyStore :: Store
emptyStore = Store empty 

data StoreResult = Inconsistent
                 | NoBinding OrRef (Int -> Store)
                 | Found Int
                 | NewInfo OrRef Store 
                 | FoundAndNewInfo Int OrRef Store 


instance Show StoreResult where
  show Inconsistent = "I"
  show (NoBinding i _) = "no"++show i
  show (Found i) = "f "++show i
  show (NewInfo r st) = "n"++show (r,st)
  show (FoundAndNewInfo i r st) = "fn"++show (i,r,st)

changeStore :: OrRef -> Store -> StoreResult
changeStore r st = 
  case uncover r of
    ref@(OrRef k r) -> let (toEntry,mima) = minMax k in
                 access (\ i -> updRef (\_->i) ref) 
                        toEntry 
                        (mima >>= \ (i,j) -> Just (i,j,r)) 
                        r 
                        st
    eq        -> chainInStore eq st
    
chainInStore :: OrRef -> Store -> StoreResult
chainInStore r@(Equality fromMin fromMax from toMin toMax to) = 
   maybe Inconsistent (NewInfo r) .
   foldChain (from:[fromMin .. fromMax]) (to:[toMin .. toMax])

 
foldChain :: [Int] -> [Int] -> Store -> Maybe Store
foldChain xs@(x:_) ys@(y:_) st = Prelude.foldl (>>=) (Just st) $
  case compare x y of
    EQ -> [Just]
    LT -> zipWith insertChain xs ys
    GT -> zipWith insertChain ys xs

---------------------------------------------------------------
-- insert a chain, i.e. one variable referring to another
-- for future work:
-- result should have shortest chains and maximal entries
-- implement occur check along the line
---------------------------------------------------------------

insertChain :: Int -> Int -> Store -> Maybe Store
insertChain key val st@(Store store) = 
  case lookup key store of
    Nothing        -> Just (Store (insert key (Equal val) store))
    Just (Equal i) -> case compare i val of
                        EQ -> Just st
                        LT -> insertChain i val st
                        GT -> insertChain val i st
    Just e         -> insertEntry val e st


insertEntry :: Int -> Entry -> Store -> Maybe Store
insertEntry key e st@(Store store) = case lookup key store of
  Nothing -> Just (Store (insert key e store))
  Just (Equal key') -> insertEntry key' e st
  Just e' -> if   choice e==choice e' 
             then Just st
             else Nothing

---------------------------------------------------------------
-- access a reference, i.e. give back entry or insert function 
-- for future work:
-- result should have shortest chains and maximal entries
---------------------------------------------------------------

access :: (Int->OrRef) -> (Int->Entry) -> Maybe (Int,Int,Int) -> Int -> Store -> StoreResult
access toOrRef toEntry mima key st@(Store store) = case lookup key store of
  Nothing -> NoBinding (toOrRef key) (\ i -> Store (insert key (toEntry i) store))
  Just (Equal key') -> access toOrRef toEntry mima key' st
  Just (Choice i)   -> Found i
  Just (Binding bmin bmax i) -> case mima of
    Nothing               -> Found i
    Just (amin,amax,key0) -> case compare amin bmin of
      EQ -> Found i
      _  -> let info = Equality amin amax key0 bmin bmax key in
            maybe Inconsistent (FoundAndNewInfo i info) $
            foldChain [amin .. amax] [bmin .. bmax] st        

                                 
storeSize :: Store -> Int
storeSize (Store st) = size st


-- this is the way to access store from outside
manipulateStore :: a -> (b -> Store -> a) 
                     -> (OrRef -> (Int -> Store) -> a)
                     -> (OrRef -> b -> Store -> a)
                     -> OrRef -> [b] -> Store -> a
manipulateStore err det br new ref bs st = case changeStore ref st of
  Inconsistent             -> err
  Found i                  -> det (bs!!i) st
  NoBinding i contSt       -> br i contSt
  NewInfo ref st           -> new ref (head bs) st
  FoundAndNewInfo i ref st -> new ref (bs!!i)   st