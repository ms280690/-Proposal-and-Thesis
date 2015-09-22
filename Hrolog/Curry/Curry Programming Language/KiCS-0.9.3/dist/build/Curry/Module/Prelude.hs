{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Prelude (module Curry.Module.Prelude) where

import Curry.RunTimeSystem



-- begin included



import Data.Char
import Data.List
import System.IO.Unsafe
import Data.IORef

import Prelude hiding ((==),(>>=),return,catch)
import qualified Prelude ((==),(>>=),return) 
import System.IO

#if __GLASGOW_HASKELL__ >= 610
import Control.OldException (catch)
#else
import Control.Exception (catch)
#endif

-----------------------------------------------------------------
-- curry number types
-----------------------------------------------------------------

type C_Float = Prim Float

-----------------------------------------------------------------
-- The curry IO monad
-----------------------------------------------------------------

data C_IO t0 = C_IO (State -> IO (IOVal t0))
  | C_IOFail C_Exceptions
  | C_IOOr OrRef (Branches (C_IO t0))

data IOVal t0 = IOVal t0
  | IOValFail C_Exceptions
  | IOValOr OrRef (Branches (IO (IOVal t0)))

data C_Bool = C_False
  | C_True
  | C_BoolFail Curry.RunTimeSystem.C_Exceptions
  | C_BoolOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches C_Bool)
  | C_BoolAnd [C_Bool]

data C_Char = C_Char !Char
  | SearchChar  C_Four  C_Four  C_Four  C_Four
  | C_CharFail C_Exceptions
  | C_CharOr OrRef (Branches C_Char)

trace s x = unsafePerformIO (putStrLn s >> preturn x) 
-----------------------------------------------------------------
-- type classes to extend BaseCurry to full Curry
-----------------------------------------------------------------

type StrEqResult = C_Bool

class (BaseCurry a,Show a,Read a) => Curry a where
  -- basic equalities 
  strEq :: a -> a -> Result StrEqResult
  eq    :: a -> a -> Result C_Bool

  -- some generics
  propagate :: (forall b. Curry b => Int -> b -> Result b) -> a -> Result a
  foldCurry :: (forall c. Curry c => c -> b -> Result b) -> b -> a -> Result b

  -- name of the type
  typeName :: a -> String

  -- show qualified terms
  showQ :: Int -> a -> String -> String 
  showQ = showsPrec

  showQList :: [a] -> String -> String
  showQList = showQStandardList

  -- generic programming
  --toC_Term   :: HNFMode -> State  -> a -> C_Data
  --fromC_Term :: C_Data -> a

class Generate a where
  genFree    :: Int -> [a]
  maxArity   :: a -> Int

-----------------------------------------------------------------
-- external Show instances
-----------------------------------------------------------------


instance (Show t0) => Show (IOVal t0) where
  showsPrec d (IOVal x1) = showParen (d>10) showStr
   where
    showStr  = showString "IOVal" . showsPrec 11 x1
  showsPrec _ (IOValOr i _) = showString ('_':show (deref i))

instance Show (IO (IOVal a)) where
  show _  = "IO"

instance Show (C_IO a) where
  show _  = "IO"

instance Show C_Success where
  showsPrec _ C_Success = showString "success"
  showsPrec _ (C_SuccessOr ref _) = showString ('_':show (deref ref))

instance Show (a->b) where
  show _ = "FUNCTION"

instance Show a => Show (Prim a) where
  show (PrimValue x) = show x
  show (PrimOr r _) = "_"++show (deref r)

instance Show a => Show (List a) where
    showsPrec = showsPrecList (showsPrec 0) (showsPrec 0)

showsPrecList :: (a -> ShowS) -> ([a] -> ShowS) -> Int -> List a -> ShowS
showsPrecList recursiveCall listCall _ (ListOr r _) = 
  showString ('_':show (deref r))
showsPrecList recursiveCall listCall _ xs 
  | isFreeList xs = showChar '(' . showFreel xs
  | otherwise     = listCall (toHaskellList xs)
      where
        isFreeList List = False
        isFreeList (ListOr _ _) = True
        isFreeList (_ :< xs) = isFreeList xs
        isFreeList _ = True

        showFreel (x:<xs)         = recursiveCall x . showChar ':' . showFreel xs
	showFreel (ListOr r _)    = showString ('_':show (deref r)++")")

showQStandardList :: Curry a => [a] -> ShowS
showQStandardList xs = showChar '[' . 
                       foldr (.) (showChar ']') 
                             (intersperse (showChar ',') (map (showQ 0) xs))

fourToInt :: C_Four -> Either String Int
fourToInt  C_F0 = Right 0
fourToInt  C_F1 = Right 1
fourToInt  C_F2 = Right 2
fourToInt  C_F3 = Right 3
fourToInt  x@(C_FourOr _ _) = Left (show x)

intToFour :: Int -> C_Four
intToFour  0 = C_F0
intToFour  1 = C_F1
intToFour  2 = C_F2
intToFour  3 = C_F3

scToChar ::  C_Four ->  C_Four ->  C_Four ->  C_Four -> Either String Char
scToChar f1 f2 f3 f4 
  = chr' ((fourToInt f1**64)+++(fourToInt f2**16)+++(fourToInt f3**4)+++fourToInt f4)
  where 
    Left s  ** _  = Left s
    Right i ** j  = Right (i*j)
    
    Left s  +++ _  = Left s
    Right i +++ Left s  = Left s
    Right i +++ Right j = Right (i+j)
    chr' (Right i) = Right (chr i)
    chr' (Left s)  = Left s

charToSc ::  Char -> C_Char
charToSc c = SearchChar (intToFour d64) (intToFour d16) (intToFour d4) (intToFour m4)
  where
    o = ord c
    (d64,m64) = divMod o 64
    (d16,m16) = divMod m64 16
    (d4,m4)   = divMod m16 4
    
instance Show C_Four where
  showsPrec d (C_FourOr r _) = showChar '_' . showsPrec d (deref r)
  showsPrec _ _ = error "probably due to usage of ($#) instead of ($##) \
                        \for an external function with argument type string or character"

instance Show C_Char where
  show (C_Char c) = show c
  show (SearchChar f1 f2 f3 f4) 
    = either id show (scToChar f1 f2 f3 f4)
  show (C_CharOr r _) = '_':show (deref r)

  showList cs = if any isFreeChar cs
                  then showChar '[' . showFreel cs
                  else showChar '"' . showl cs   -- "
    where 
      showl []       = showChar '"'
      showl (C_Char '"':cs) = showString "\\\"" . showl cs
      showl (C_Char c:cs)
       | oc <= 7   = showString "\\00" . shows oc . showl cs
       | oc <= 10  = showLitChar c . showl cs
       | oc <= 12  = showString "\\0" . shows oc . showl cs
       | oc <= 13  = showLitChar c . showl cs
       | oc <= 31  = showString "\\0" . shows oc . showl cs
       | oc <= 126 = showLitChar c . showl cs
       | otherwise = showString "\\" . shows oc . showl cs
       where oc = ord c
      showl (SearchChar f1 f2 f3 f4:cs) = 
        either showString showLitChar (scToChar f1 f2 f3 f4) . showl cs
  
      showFreel [] = showString "]"
      showFreel [c] = showString (show c) . showString "]"
      showFreel (c:cs)   = showString (show c++",") . showFreel cs
      
      isFreeChar (SearchChar f1 f2 f3 f4) = 
        Prelude.any ((Prelude.== Branching) . consKind) [f1,f2,f3,f4] 
      isFreeChar _              = False

protectEsc p f             = f . cont
 where cont s@(c:_) | p c  = "\\&" ++ s
       cont s              = s

asciiTab = zip ['\NUL'..' ']
	   ["NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL",
	    "BS",  "HT",  "LF",  "VT",  "FF",  "CR",  "SO",  "SI",
	    "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB",
	    "CAN", "EM",  "SUB", "ESC", "FS",  "GS",  "RS",  "US",
	    "SP"]

instance Show C_Nat where
  showsPrec d x | isFreeNat x = showsPrecNat d x
                | otherwise   = showsPrec d (fromCurry x::Integer)


isFreeNat :: C_Nat -> Bool
isFreeNat (C_NatOr _ _)    = True
isFreeNat C_IHi            = False
isFreeNat (C_I n)          = isFreeNat n
isFreeNat (C_O n)          = isFreeNat n

showsPrecNat :: Int -> C_Nat -> ShowS
showsPrecNat _ C_IHi = Prelude.showString((:)('I')((:)('H')((:)('i')([]))))
showsPrecNat d (C_O x1) = Prelude.showParen((Prelude.>)(d)(Prelude.fromInteger((10))))(showStr)
 where
  showStr  = (Prelude..)(Prelude.showString((:)('O')((:)(' ')([]))))(showsPrecNat(Prelude.fromInteger((11)))(x1))

showsPrecNat d (C_I x1) = Prelude.showParen((Prelude.>)(d)(Prelude.fromInteger((10))))(showStr)
 where
  showStr  = (Prelude..)(Prelude.showString((:)('I')((:)(' ')([]))))(showsPrecNat(Prelude.fromInteger((11)))(x1))

showsPrecNat _ (C_NatOr i _) = Prelude.showString((:)('_')(Prelude.show(deref i)))

instance Show C_Int where
  showsPrec _ C_Zero = showChar '0'
  showsPrec d x@(C_Pos n) 
    | isFreeNat n = showParen (d>10) (showString "Pos " . showsPrecNat 11 n)
    | otherwise   = showsPrec d (fromCurry x::Integer)
  showsPrec d x@(C_Neg n) 
    | isFreeNat n = showParen (d>10) (showString "Neg " . showsPrecNat 11 n)
    | otherwise   = showsPrec d (fromCurry x::Integer)
  showsPrec _ (C_IntOr i _) = showChar '_' . shows (deref i)

-----------------------------------------------------------------
-- external Read instances
-----------------------------------------------------------------

instance Read C_Four where
  readsPrec _ _ = error "I won't read four"

instance (Read t0) => Read (IOVal t0) where
  readsPrec d r = readParen (d>10) 
    (\ r -> [ (IOVal x1,r1) | (_,r0) <- readQualified "Prelude" "IOVal" r, 
                              (x1,r1) <- readsPrec 11 r0]) r

instance Read (IO (IOVal a)) where
  readsPrec = error "no reading IO"

instance Read (C_IO a) where
  readsPrec = error "no reading IO"

instance Read C_Success where
  readsPrec d r = Prelude.readParen(Prelude.False)
                  (\ r -> [(,)(C_Success)(r0) | 
                           (_,r0) <- readQualified "Prelude" "Success" r])(r)

instance Read a => Read (Prim a) where
  readsPrec p s = map (\(x,y) -> (PrimValue x,y)) (readsPrec p s)

instance Read a => Read (List a) where
    readsPrec p = map (\ (x,y) -> (fromHaskellList x,y)) . readsPrec p

instance Read C_Char where
  readsPrec p s = map (\ (x,y) -> (toCurry x,y))
                      (((readsPrec p)::ReadS Char) s)

  readList s = map (\ (x,y) -> (map toCurry x,y))
                      ((readList::ReadS String) s)

instance Read (a->b) where
  readsPrec = error "reading FUNCTION"

instance Read C_Nat where
  readsPrec d r =  
       readParen False  (\ r -> [(C_IHi,r0)  | (_ ,r0) <- readQualified "Prelude" "IHi" r]) r
    ++ readParen (d>10) (\ r -> [(C_O x1,r1) | (_ ,r0) <- readQualified "Prelude" "O"   r, 
                                               (x1,r1) <- readsPrec 11 r0]) r
    ++ readParen (d>10) (\ r -> [(C_I x1,r1) | (_ ,r0) <- readQualified "Prelude" "I"   r, 
                                               (x1,r1) <- readsPrec 11 r0]) r
    ++ [(toCurry i,r0) | (i::Integer,r0) <- reads r]

instance Read C_Int where
  readsPrec d r = 
       readParen (d>10) (\ r -> [(C_Neg x1,r1)  | (_ ,r0) <- readQualified "Prelude" "Neg" r, 
                                                  (x1,r1) <- readsPrec 11 r0]) r
    ++ readParen False  (\ r -> [(C_Zero,r0)    | (_ ,r0) <- readQualified "Prelude" "Zero" r]) r 
    ++ readParen (d>10) (\ r -> [(C_Pos x1,r1)  | (_ ,r0) <- readQualified "Prelude" "Pos" r,
                                                  (x1,r1) <- readsPrec 11 r0]) r
    ++ [(toCurry i,r0) | (i::Integer,r0) <- reads r]


-----------------------------------------------------------------
-- external BaseCurry instances
-----------------------------------------------------------------

instance (BaseCurry t0) => BaseCurry (IOVal t0) where
  nf f (IOVal x1) state0 = nfCTC(\ v1 state1 -> f(IOVal(v1)) (state1))(x1) (state0)
  nf f x state = f(x) (state)

  gnf f (IOVal x1) state0 = gnfCTC(\ v1 state1 -> f(IOVal(v1)) (state1))(x1) (state0)
  gnf f x state = f(x) (state)

  generator i    = IOVal (generator i)

  failed  = IOValFail

  branching r bs = IOValOr r (map preturn bs)

  consKind (IOValOr _ _) = Branching
  consKind (IOValFail _) = Failed
  consKind _ = Val

  exceptions (IOValFail x) = x

  orRef (IOValOr x _) = x

  branches (IOValOr _ bs) = map unsafePerformIO bs

instance (BaseCurry t0) => BaseCurry (IO (IOVal t0)) where
  nf f x state = f(x) (state)
  gnf f x state = f(x)(state)

  failed x = preturn (IOValFail x)

  generator u       = preturn (generator u)

  branching r bs = preturn (IOValOr r bs)

  consKind x = consKind (unsafePerformIO x)

  exceptions x = exceptions (unsafePerformIO x)

  orRef x = orRef (unsafePerformIO x)

  branches x = unsafePerformIO (x Prelude.>>= \ (IOValOr _ bs) -> preturn bs)

instance (BaseCurry t0) => BaseCurry (C_IO t0) where
  nf f x state = f(x)(state)
  gnf f x state = f(x)(state)

  generator i    = C_IO (\ _ -> generator i)

  failed  = C_IOFail

  branching  = C_IOOr

  consKind (C_IOOr _ _) = Branching
  consKind (C_IOFail _) = Failed
  consKind _ = Val

  exceptions (C_IOFail x) = x

  orRef (C_IOOr x _) = x

  branches (C_IOOr _ x) = x


instance BaseCurry C_Char where
  nf f (SearchChar x1 x2 x3 x4) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> f(SearchChar(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x store = f(x)(store)

  gnf f (SearchChar x1 x2 x3 x4) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> f(SearchChar(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x store = f(x)(store)
  

  consKind (C_CharOr _ _) = Branching
  consKind (C_CharFail _) = Failed
  consKind _ = Val

  generator i = withRef ( \r -> SearchChar (generator r) 
                                           (generator (r+1)) 
                                           (generator (r+2)) 
                                           (generator (r+3))) 3

  orRef      (C_CharOr x _) = x
  branches   (C_CharOr _ x) = x

  failed = C_CharFail

  exceptions (C_CharFail x) = x

  branching  = C_CharOr


instance Generate a => BaseCurry (Prim a) where
  nf f x store = f(x)(store)

  gnf f x store = f(x)(store)

  generator i    = gen genFree 
    where
      gen f = let max = maxArity (head (f 0)) in
        withRef (\r -> PrimOr (mkRef r max i)
                              (map PrimValue (f r)))
                max

  failed = PrimFail
  branching = PrimOr

  consKind (PrimOr _ _) = Branching
  consKind (PrimFail _) = Failed
  consKind _ = Val

  exceptions (PrimFail x) = x

  orRef (PrimOr x _) = x

  branches (PrimOr _ x) = x


instance (BaseCurry t0) => BaseCurry (List t0) where
  nf f ((:<) x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f((:<)(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f ((:<) x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f((:<)(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = withRef (\ r -> ListOr (mkRef r 2 i) 
                        ([List,(:<)(generator(r+1))(generator(r+2))])) 2

  failed  = ListFail

  branching  = ListOr

  consKind (ListOr _ _) = Curry.RunTimeSystem.Branching
  consKind (ListFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (ListFail x) = x

  orRef (ListOr x _) = x

  branches (ListOr _ x) = x


-----------------------------------------------------------------
-- converting between curry and haskell
-----------------------------------------------------------------

-- In Order to integrate Haskell functions we sometimes 
-- need to convert values.
-- (Do we really need both directions? Or rather convert a b for both?)
class ConvertCH a b where
  fromCurry :: a -> b
  fromCurry = error "fromCurry"
  toCurry :: b -> a
  toCurry = error "toCurry"

instance ConvertCH C_Bool Bool where
  fromCurry C_True  = True
  fromCurry C_False = False

  toCurry True  = C_True
  toCurry False = C_False

isC_True C_True = True
isC_True _      = False

instance ConvertCH C_Char Char where
  fromCurry (C_Char c) = c
  fromCurry (SearchChar f0 f1 f2 f3) = 
    either (error "convert to char") id (scToChar f0 f1 f2 f3)
  toCurry c = C_Char c

instance (ConvertCH a b) => ConvertCH (List a) [b] where
  fromCurry List = []
  fromCurry (x :< xs) = fromCurry x : fromCurry xs
  fromCurry (ListOr _ _) = error "or list"

  toCurry [] = List
  toCurry (x:xs) = toCurry x :< toCurry xs

-- sometimes you need conversion of lists without converting the elements
-- eg Searchtree, Show instance

toHaskellList :: List a -> [a]
toHaskellList List = []
toHaskellList (x :< xs) = x : toHaskellList xs

fromHaskellList :: [a] -> List a
fromHaskellList [] = List
fromHaskellList (x : xs) = x :< fromHaskellList xs

-- specify result type of toCurry "..." for code generation
fromHaskellString :: String -> List C_Char
fromHaskellString = toCurry

instance ConvertCH C_Int Integer where
  fromCurry C_Zero    = 0
  fromCurry (C_Pos i) = fromCurry i
  fromCurry (C_Neg i) = negate (fromCurry i)

  toCurry n = case compare n 0 of
   LT -> C_Neg (toCurry (abs n))
   EQ -> C_Zero
   GT -> C_Pos (toCurry (abs n))

instance ConvertCH C_Nat Integer where
  fromCurry (C_I bs) = 2 Prelude.* fromCurry bs Prelude.+ 1
  fromCurry (C_O bs) = 2 Prelude.* fromCurry bs
  fromCurry C_IHi    = 1

  toCurry n = case mod n 2 of
                1 -> if m Prelude.== 0 then C_IHi else C_I (toCurry m)
                0 -> C_O (toCurry m)
    where m = Prelude.div n 2


instance ConvertCH C_Int Int where
  fromCurry c = fromInteger (fromCurry c)
  toCurry i   = toCurry (toInteger i)

instance ConvertCH (Prim a) a where
  toCurry = PrimValue 
  fromCurry (PrimValue x) = x

-------------------------------------------------------------
-- basic functions used in instances of class GenericCurry
-------------------------------------------------------------
-- obscure names come from the standard operator 
-- renaming scheme of the compiler.

-- implementation of concurrent (&)
-- no other implementation
-- basic concept: if one value suspends evaluate the other 
-- TODO: include state information!
concAnd :: StrEqResult -> StrEqResult -> Result StrEqResult
concAnd C_True y _ = y
concAnd x@(C_BoolOr _ _) y st = maySwitch y x st
--concAnd (C_BoolOr r xs) y = C_BoolOr r (map (flip concAnd y) xs)
concAnd x@(C_BoolFail _) _ _ = x
concAnd x@C_False _ _ = x

maySwitch :: StrEqResult -> StrEqResult -> Result StrEqResult
maySwitch C_True x _ = x
maySwitch y@(C_BoolOr _ _) (C_BoolOr r xs) st = 
             C_BoolOr r (map (\ x -> concAnd y x st) xs)
maySwitch x@(C_BoolFail _) _ _ = x
maySwitch x@C_False _ _ = x
{-
startBreadth :: [StrEqResult] -> Result StrEqResult
startBreadth cs st = onLists st [] cs

instance Eq C_Bool where
  C_True == C_True = True
  C_False == C_False = True
  _ == _ = False

allSame :: Eq a => [a] -> Bool
allSame []     = True
allSame (x:xs) = all (x==) xs

onLists :: Store -> [StrEqResult] -> [StrEqResult] -> StrEqResult
onLists _ []  []      = strEqSuccess
onLists _ _   (x@(C_BoolFail _):_) = x
onLists _ _   (C_False:_)   = C_False
onLists st ors (C_True:xs) = onLists st ors xs
onLists st ors (C_BoolAnd xs:ys) = onLists st ors (xs++ys)
onLists st ors (C_BoolOr ref xs:ys) 
  | isChain ref = chain (\ x st -> onLists st ors (x:ys)) ref xs st
  | otherwise   = case fromStore ref st of
  Nothing -> onLists st (insertOr ref xs ors) ys
  Just i  -> onLists st ors (xs!!i : ys)
onLists st (C_BoolOr ref xs:ors) [] = 
  let inBranch i x = maybe (failed $ curryError "onLists")
                           (\st -> onLists st ors [x])
                           (addToStore ref i st)
  in  C_BoolOr ref (zipWith inBranch [0..] xs)

insertOr ref xs [] = [C_BoolOr ref xs]
insertOr ref xs (o@(C_BoolOr ref2 xs2):ys) 
  | ref==ref2 = C_BoolOr ref (zipWith insertAnd xs xs2) : ys
  | otherwise = o : insertOr ref xs ys

insertAnd C_True           y       	    = y
insertAnd C_False          _       	    = C_False
insertAnd x@(C_BoolFail _) _       	    = x
insertAnd x                C_True  	    = x
insertAnd _                C_False 	    = C_False
insertAnd _                x@(C_BoolFail _) = x
insertAnd o1@(C_BoolOr ref1 xs1) o2@(C_BoolOr ref2 xs2) 
  | ref1 == ref2 = C_BoolOr ref1 (zipWith insertAnd xs1 xs2)
  | otherwise    = C_BoolAnd [o1,o2]
insertAnd o@(C_BoolOr _ _) (C_BoolAnd ys)   = C_BoolAnd (o:ys)
insertAnd (C_BoolAnd ys)   o@(C_BoolOr _ _) = C_BoolAnd (o:ys)
insertAnd (C_BoolAnd xs)   (C_BoolAnd ys)   = C_BoolAnd (xs++ys)
-}
--- implementation of (==)
--- no other implementation
genEq :: Curry t0 => t0 -> t0 -> Result C_Bool
genEq x y = ghnfCTC (\x'-> ghnfCTC (eq x') y) x

--- implementation of (=:=)
--- no other implementation
--- TODO: use state information
genStrEq :: Curry t0 => t0 -> t0 -> Result StrEqResult
genStrEq a b = (\ a' -> (onceMore a') `hnfCTC` b)  `hnfCTC` a
  where
    onceMore a' b' = (\ a'' -> unify a'' b') `hnfCTC` a'
    unify x y st = checkFree (consKind x) (consKind y)
      where
      checkFree Val Val = strEq x y st

      checkFree Branching Branching  
         | drx Prelude.== dry
         = C_True
         | otherwise = branching (equalFromTo ax bx drx ay by dry) [C_True]
         where (ax,bx,drx)=genInfo (orRef x)
               (ay,by,dry)=genInfo (orRef y)

      checkFree Branching _ = 
        hnfCTC (\ x' -> unify x' y) 
               (branching (narrowOrRef (orRef x)) (branches x)) st

      checkFree _ Branching = 
        hnfCTC (unify x)
               (branching (narrowOrRef (orRef y)) (branches y)) st

      checkFree x   y   = error $ "checkFree " ++ show (x,y)

strEqFail :: String -> StrEqResult
strEqFail s = C_False --C_SuccessFail (ErrorCall ("(=:=) for type "++s))

strEqSuccess :: StrEqResult
strEqSuccess = C_True

--hcAppend [] ys = ys
--hcAppend (x:xs) ys = x:< hcAppend xs ys

-----------------------------------------------------------------
-- external Generate instances
-----------------------------------------------------------------

instance BaseCurry b => Generate (a -> Result b) where
  genFree i  = mkBranches (generator i)
  maxArity _ = 1

mkBranches :: BaseCurry b => b -> [a -> Result b]
mkBranches x = case consKind x of
       Val       -> [const (const x)]
       Branching -> map (const . const) (branches x)

instance Generate Float where
  genFree    = error "free variable of type Float"
  maxArity _ = error "free variable of type Float"

-----------------------------------------------------------------
-- external Curry instances
-----------------------------------------------------------------

instance (Curry t0) => Curry (List t0) where
  strEq List List st = strEqSuccess
  strEq ((:<) x1 x2) ((:<) y1 y2) st = concAnd(genStrEq(x1)(y1)(st))(genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = strEqFail(typeName(x0))

  eq List List st = C_True
  eq ((:<) x1 x2) ((:<) y1 y2) st = op_38_38(genEq(x1)(y1)(st))(genEq(x2)(y2)(st))(st)
  eq _ _ _ = C_False

  propagate f List st = List
  propagate f ((:<) x1 x2) st = (:<)(f 0 (x1)(st))(f 1 (x2)(st))

  foldCurry f c List st = c
  foldCurry f c ((:<) x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "[]"

  showQ  = showsPrecList (showQ 0) showQList 

instance Curry C_Four where
  strEq C_F0 C_F0 _ = strEqSuccess
  strEq C_F1 C_F1 _ = strEqSuccess
  strEq C_F2 C_F2 _ = strEqSuccess
  strEq C_F3 C_F3 _ = strEqSuccess
  strEq x0   _    _ = strEqFail(typeName(x0))

  eq C_F0 C_F0 _ = C_True
  eq C_F1 C_F1 _ = C_True
  eq C_F2 C_F2 _ = C_True
  eq C_F3 C_F3 _ = C_True
  eq _    _    _ = C_False

  propagate _ C_F0 _ = C_F0
  propagate _ C_F1 _ = C_F1
  propagate _ C_F2 _ = C_F2
  propagate _ C_F3 _ = C_F3

  foldCurry _ c C_F0 _ = c
  foldCurry _ c C_F1 _ = c
  foldCurry _ c C_F2 _ = c
  foldCurry _ c C_F3 _ = c

  typeName _ = "Four"


instance BaseCurry a => Curry (IO (IOVal a)) where 
  strEq x y = error "IO.strEq"

  eq _ _ = error "IO.eq"

  propagate _ _ = error "propagate IOVal"

  foldCurry _ _ _ = error "foldCurry IOVal"

  typeName _ = "IOVal"

  --toC_Term _ _ _ = error "IO.toC_Term"
  --fromC_Term _   = error "IO.fromC_Term"


instance BaseCurry a => Curry (C_IO a) where
  strEq _ _ = error "strEq IO"

  eq _ _ = error "eq IO"

  --subst store x = x

  propagate _ _ = error "propagate IO"

  foldCurry _ _ _ = error "foldCurry IO"

  typeName _ = "IO"

  --toC_Term _ _ (C_IOFreeVar r) = C_Free(C_Int(Prelude.toInteger(r)))
  --toC_Term _ _ _ = C_Data (C_Int 1) (toCurry "IO") List

  --fromC_Term (C_Free (C_Int r)) = C_IOFreeVar(Prelude.fromInteger(r))
  --fromC_Term _ = error "no converting IO"

instance Curry C_Char where
  strEq x@(C_Char c1) (C_Char c2) _
    | c1 Prelude.== c2 = C_True
  strEq c1@(SearchChar _ _ _ _) (C_Char c2) st = strEq c1 (charToSc c2) st 
  strEq (C_Char c1) c2@(SearchChar _ _ _ _) st = strEq (charToSc c1) c2 st 
  strEq (SearchChar x1 x2 x3 x4) (SearchChar y1 y2 y3 y4) st = concAnd (genEq(x1)(y1)st)(concAnd(genStrEq(x2)(y2)st)(concAnd(genStrEq(x3)(y3)st)(genStrEq(x4)(y4)st)st)st)st
  strEq _ x _ = strEqFail (typeName x)


  eq (C_Char x1) (C_Char y1)             _  = toCurry (x1 Prelude.== y1)
  eq c1@(SearchChar _ _ _ _) (C_Char c2) st = eq c1 (charToSc c2) st
  eq (C_Char c1) c2@(SearchChar _ _ _ _) st = eq (charToSc c1) c2 st
  eq (SearchChar x1 x2 x3 x4) (SearchChar y1 y2 y3 y4) st = op_38_38 (genEq (x1)(y1)st) (op_38_38 (genEq(x2)(y2)st) (op_38_38(genEq(x3)(y3)st)(genEq(x4)(y4)st)st)st)st
  eq _ _ _ = C_False

  propagate _ c@(C_Char _) _ = c
  propagate f (SearchChar f0 f1 f2 f3) st = 
    SearchChar (f 0 f0 st) (f 1 f1 st) (f 2 f2 st) (f 3 f3 st)

  foldCurry _ c (C_Char _) _ = c
  foldCurry f c (SearchChar f0 f1 f2 f3) st = f f0 (f f1 (f f2 (f f3 c st)st)st)st

  typeName _ = "Char"

  showQList = showList  

  --toC_Term _ _ (C_Char c) = C_Data (C_Int (toInteger (ord c))) (toCurry (show c)) List
  --toC_Term _ _ (C_CharFreeVar r) = C_Free(C_Int(Prelude.toInteger(r)))

  --fromC_Term (C_Data (C_Int (i::Integer)) _ List) = C_Char (chr (fromInteger i))
  --fromC_Term (C_Data (C_IntFreeVar _) name List) = C_Char (read (fromCurry name))
  --fromC_Term (C_Free (C_Int r)) = C_CharFreeVar(Prelude.fromInteger(r))


instance (Generate a,Show a,Read a,Eq a) => Curry (Prim a) where
  strEq x@(PrimValue v1) (PrimValue v2) _
    | v1 Prelude.== v2 = C_True --C_Success
    | otherwise = strEqFail (typeName x)

  eq (PrimValue v1) (PrimValue v2) _ = toCurry (v1 Prelude.== v2)

  propagate _ (PrimValue v1) _ = PrimValue v1

  foldCurry _ c (PrimValue _) _ = c

  --toC_Term _ _ (PrimValue x1) = let sx = show x1 in
  --    C_Data (C_Int (string2int sx)) (toCurry sx) List
  --toC_Term _ _ (PrimFreeVar r) = C_Free(C_Int(Prelude.toInteger(r)))

  --fromC_Term (C_Data _ name List) = PrimValue (read (fromCurry name))
  --fromC_Term (C_Free (C_Int r)) = PrimFreeVar(Prelude.fromInteger(r))
 
  typeName _ = "Prim"



-----------------------------------------------------------------
-- external Curry instances
-----------------------------------------------------------------

instance Eq (a->b) where
  (==) = error "comparing FUNCTION"





infix  4 ===
infixr 0 & 

-----------------------------------------------------------------------
-- IO starter
-----------------------------------------------------------------------

preturn = Prelude.return

optChangeStore :: a -> (b -> Store -> a) -> ((Int -> Store) -> a) 
               -> OrRef -> Branches b -> Store -> a
optChangeStore err det br = 
  manipulateStore err det (\ _ -> br) (\ _ -> det)

curryIO :: Curry a => (Result (C_IO a)) -> IO a
curryIO x = let st = emptyStore in ioStart st (x st)

curryIOVoid :: Curry a => (Result (C_IO a)) -> IO ()
curryIOVoid x = curryIO x >> Prelude.return ()

ioStart :: Curry a => Store -> C_IO a -> IO a
ioStart st (C_IO act)            = act st Prelude.>>= curryDo st
ioStart _  (C_IOFail es)         = printExceptions es
ioStart st (C_IOOr ref bs)       =
  optChangeStore 
    (printExceptions (curryError "ioStart"))
    (flip ioStart)
    (\st -> searchValC_IO [] (zipWith (mkChoice st) [0..] bs))
    ref 
    bs 
    st

curryDo :: Curry a => Store -> IOVal a -> IO a
curryDo _  (IOVal x)        = Prelude.return x
curryDo _  (IOValFail es)   = printExceptions es
curryDo st (IOValOr ref bs) =     
  optChangeStore 
    (printExceptions (curryError "curryDo")) 
    (\ x st -> x Prelude.>>= curryDo st)
    (\st -> searchIOVal [] (zipWith (mkChoice st) [0..] bs))
    ref 
    bs
    st

mkChoice :: BaseCurry a => (Int -> Store) -> Int -> a -> (Store,a)
mkChoice st i x = (st i,x)

searchValC_IO :: Curry a => [C_Exceptions] -> [(Store,C_IO a)] -> IO a
searchValC_IO es []     = 
  mapM_ printException es >> error "no solution in branching io value"
searchValC_IO _ ((st,C_IO act)   : _)  = act st Prelude.>>= curryDo st
searchValC_IO es ((_ ,C_IOFail e@(ErrorCall _)) : xs) = 
  searchValC_IO (e:es) xs
searchValC_IO es ((_ ,C_IOFail e) : xs) = searchValC_IO es xs
searchValC_IO es ((st,C_IOOr ref bs) : xs) =  
  optChangeStore
    (searchValC_IO es xs)
    (\ x st -> case x of
        C_IO act   -> act st Prelude.>>= curryDo st
        C_IOOr _ _ -> searchValC_IO es ((st,x):xs)
        C_IOFail _ -> searchValC_IO es xs)
    -- switch arguments of (++) for breadth first (bad.), cf. also below
    (\ st -> searchValC_IO es (zipWith (mkChoice st) [0..] bs ++ xs))
    ref bs st

searchIOVal :: Curry a => [C_Exceptions] -> [(Store,IO (IOVal a))] -> IO a
searchIOVal es []                = 
  mapM_ printException es >> error "no solution in branching io value"
searchIOVal es ((st,act) : stacts) = do
  x <- act
  case x of
    IOVal a        -> Prelude.return a
    IOValFail e@(ErrorCall _) -> searchIOVal (e:es) stacts
    IOValFail _    -> searchIOVal es stacts
      -- switch arguments of (++) for breadth first (bad.)
    IOValOr ref bs -> 
      optChangeStore 
        (searchIOVal (curryError "inconsistent Store":es) stacts)
        (\ x st -> searchIOVal es ((st,x):stacts))
        (\st -> searchIOVal es (zipWith (mkChoice st) [0..] bs ++ stacts))
        ref bs st

-- this is the place to change for implicit breadth first search
searchVal :: (Store -> a -> b) -> Store -> OrRef -> Branches a -> b
searchVal cont store ref [] =  error "top io failed"
searchVal cont store ref (x:bs) = cont store x

printException :: C_Exceptions -> IO ()
printException (PatternMatchFail s) = 
  hPutStrLn stderr ("non-exhaustive patterns in function "++s)
printException (AssertionFailed s) = 
  hPutStrLn stderr ("assertion failed with label "++s)
printException (IOException s) = 
  hPutStrLn stderr ("io exception: " ++ s)
printException (ErrorCall s) = 
  hPutStrLn stderr ("error : " ++s)
printException PreludeFailed = hPutStrLn stderr "Prelude.failed"

printExceptions :: C_Exceptions -> IO a
printExceptions e = 
  printException e >> error "program error"

-----------------------------------------------------------------------
-- Int and Float
-----------------------------------------------------------------------


instance Eq C_Int where
  x == y = (fromCurry x::Integer) Prelude.== fromCurry y

instance Num C_Int where
  fromInteger x = toCurry x
  x + y = toCurry ((fromCurry x::Integer) + fromCurry y)
  x * y = toCurry ((fromCurry x::Integer) * fromCurry y)
  
  abs (C_Neg x) = C_Pos x
  abs x = x

  signum (C_Pos _) = C_Pos C_IHi
  signum (C_Neg _) = C_Neg C_IHi
  signum x = x

instance Eq a => Eq (Prim a) where
  (PrimValue x) == (PrimValue y) = x Prelude.== y

instance (Num a) => Num (Prim a) where
  (PrimValue x) + (PrimValue y) = PrimValue (x+y)
  (PrimValue x) - (PrimValue y) = PrimValue (x-y)
  (PrimValue x) * (PrimValue y) = PrimValue (x*y)
  negate (PrimValue x) = PrimValue (negate x)
  abs    (PrimValue x) = PrimValue (abs x)
  signum (PrimValue x) = PrimValue (signum x)
  fromInteger x = PrimValue (fromInteger x)

instance Enum a => Enum (Prim a) where 
    toEnum i = PrimValue (toEnum i)
    fromEnum (PrimValue x) = fromEnum x

instance Real a => Real (Prim a) where 
    toRational (PrimValue x) = toRational x

instance Integral a => Integral (Prim a) where 
    quotRem (PrimValue x) (PrimValue y) = let (x',y') = quotRem x y in 
                                           (PrimValue x', PrimValue y')
    toInteger (PrimValue x) = toInteger x

instance Ord a => Ord (Prim a) where
   (PrimValue x) <= (PrimValue y) = x<=y

-----------------------------------------------------------------------
-- T0 is unit (), needed for IO primitives
-----------------------------------------------------------------------

instance ConvertCH T0 () where
  toCurry () = T0
  fromCurry T0 = () 

instance (ConvertCH a ha, ConvertCH b hb) => ConvertCH (T2 a b) (ha,hb) where
  toCurry (x,y) = T2 (toCurry x) (toCurry y)
  fromCurry (T2 x y) = (fromCurry x, fromCurry y) 

instance (ConvertCH a ha, ConvertCH b hb, ConvertCH c hc) =>
         ConvertCH (T3 a b c) (ha,hb,hc) where
  toCurry (x,y,z) = T3 (toCurry x) (toCurry y) (toCurry z)
  fromCurry (T3 x y z) = (fromCurry x, fromCurry y, fromCurry z) 

-----------------------------------------------------------------------
-- Maybe
-----------------------------------------------------------------------

instance (ConvertCH a b) => ConvertCH (C_Maybe a) (Maybe b) where
  fromCurry C_Nothing  = Nothing
  fromCurry (C_Just x) = Just (fromCurry x)

  toCurry Nothing  = C_Nothing
  toCurry (Just x) = C_Just (toCurry x)


---------------------------------------------------------------------------------
-- external functions for Prelude
---------------------------------------------------------------------------------

($#) :: (Curry a, Curry b) => Prim (a -> Result b) -> a -> Result b
($#) cont x = prepApply ghnfCTC x cont 

($!) :: (Curry a,Curry b) => Prim (a -> Result b) -> a -> Result b
($!) cont x = prepApply hnfCTC x cont

($!!) :: (Curry a, Curry b) => Prim (a -> Result b) -> a -> Result b
($!!) cont x = prepApply nfCTC x cont

($##) :: (Curry a, Curry b) => Prim (a -> Result b) -> a -> Result b
($##) cont x = prepApply gnfCTC x cont

prim_error :: Curry a => C_String -> Result a
prim_error s _ = Curry.RunTimeSystem.failed (ErrorCall (fromCurry s))

failed :: Curry a => Result a
failed _ = Curry.RunTimeSystem.failed PreludeFailed 

(==) :: Curry a => a -> a -> Result C_Bool
(==) = genEq 

prim_ord :: C_Char -> Result C_Int
prim_ord cc _ = toCurry (ord (fromCurry cc))

prim_chr :: C_Int -> Result C_Char
prim_chr ci _ = toCurry (chr (fromCurry ci))

(===) :: Curry a => a -> a -> Result C_Bool --C_Success
(===) = genStrEq
 
prim_negateFloat :: C_Float -> Result C_Float
prim_negateFloat x _ = negate x

success :: C_Success
success = C_Success

--concAnd' x y st = startBreadth [x,y] st

(&) :: C_Success -> C_Success -> Result C_Success
 -- (&) x y st = boolToSuccess (concAnd' (successToBool x) (successToBool y) st)
(&) x y st = boolToSuccess 
               (concAnd (successToBool x st) 
                        (successToBool y st) st) st

boolToSuccess C_True            _  = C_Success
boolToSuccess C_False           _  = C_SuccessFail (ErrorCall "&")
boolToSuccess (C_BoolFail e)    _  = C_SuccessFail e
boolToSuccess (C_BoolOr r xs)   st = mapOr boolToSuccess r xs st


successToBool :: C_Success -> Result C_Bool
successToBool C_Success                _  = C_True
successToBool (C_SuccessFail e)        _  = C_BoolFail e
successToBool (C_SuccessOr r xs)       st = mapOr successToBool r xs st

--andBreadth :: List C_Bool -> Result C_Bool
--andBreadth xs st = startBreadth (toHaskellList xs) st

-- TODO: C_IO without State??? also other io-functions.
(>>=) :: (Curry a,Curry b) => C_IO a -> Prim (a -> Result (C_IO b)) -> Result (C_IO b)
(>>=) m f _ = C_IO (hnfCTC (exec f) m)

exec :: (Curry a,Curry b) => Prim (a -> Result (C_IO b)) -> C_IO a -> Result (IO (IOVal b))
exec f (C_IO m) st = m st Prelude.>>= \ x -> prim_do f x st

-- if it wasn't io, we could just write 
--exec f st (C_IO m) = m st Prelude.>>= hnfCTC (fromIOVal f) st
-- with fromIOVal simply being
--fromIOVal::(Curry a,Curry b)=>Prim(a->C_IO b)->State->IOVal a->IO(IOVal b)
--fromIOVal f st (IOVal res) = hnfCTC exec2 st (apply f res)
-- and everything would work fine. But then for the susp and or cases
-- we would use unsafe io...
-- Thus, prim_do has to copy the code of ctcStore False
-- IMPORTANT: This code should correspond to BaseCurry.RunTimeSystem.ctcStore

prim_do ::  (Curry a,Curry b) => 
            Prim (a -> Result (C_IO b)) -> IOVal a ->  Result (IO (IOVal b))
prim_do f x state = case x of
  IOVal res      -> hnfCTC exec2 (apply f res state) state
  IOValFail es   -> Prelude.return (IOValFail es)
  IOValOr ref bs -> 
    optChangeStore
       (Curry.RunTimeSystem.failed $ curryError "prim_do")
       (\ x st -> x Prelude.>>= \ x' -> prim_do f x' st)
       (\ st -> Prelude.return (IOValOr ref 
                  (zipWith (\ i x -> x Prelude.>>= \ x' -> cont x' (st i)) 
                           [0..] bs)))
       ref bs state
  where
    cont x st = prim_do f x st

exec2 :: C_IO b -> Result (IO (IOVal b))
exec2 (C_IO f) = f 


return :: a -> Result (C_IO a)
return a _ = C_IO (\ _ -> Prelude.return (IOVal a))

prim_putChar :: C_Char -> Result (C_IO T0)
prim_putChar = ioFunc1 putChar 

getChar :: Result (C_IO C_Char)
getChar = ioFunc0 Prelude.getChar
 
prim_readFile :: C_String -> Result (C_IO C_String)
prim_readFile = ioFunc1 readFile 

prim_writeFile :: C_String -> C_String -> Result (C_IO T0)
prim_writeFile = ioFunc2 writeFile 

prim_appendFile :: C_String -> C_String -> Result (C_IO T0)
prim_appendFile = ioFunc2 appendFile 

catchFail :: Curry a => C_IO a -> C_IO a -> Result (C_IO a)
catchFail (C_IO act) err _ = 
  C_IO (\ st -> catch (act st) (const (hnfCTC exec2 err st)))
catchFail (C_IOFail _) err _ = err
catchFail (C_IOOr ref bs) err st =
  optChangeStore 
    err
    (flip catchFail err)
    (\st -> searchValCatch (zipWith (mkChoice st) [0..] bs) err)
    ref bs st

searchValCatch :: Curry a => [(Store,C_IO a)] -> C_IO a -> C_IO a
searchValCatch []     err = err
searchValCatch ((st,C_IO act)   : _)  err = catchFail (C_IO act) err st
searchValCatch ((_ ,C_IOFail _) : xs) err = searchValCatch xs err
searchValCatch ((st,C_IOOr ref bs) : xs)  err =  
  optChangeStore 
    (searchValCatch xs err)
    (\ x st -> catchFail x err st)
    (\ st -> searchValCatch (zipWith (mkChoice st) [0..] bs ++ xs) err)
    ref bs st





prim_show :: (Show a,Curry a) => a -> Result C_String
prim_show x _ = toCurry (show x)

getSearchTree :: Curry a => a -> Result (C_IO (C_SearchTree a))
getSearchTree x _ = C_IO (\ state -> Prelude.return (IOVal (searchTr x state)))

 
searchTr :: Curry a => a -> Result (C_SearchTree a)
searchTr x state = transVal (nfCTC (nfCTC const) x state)
  where
    transVal x = case consKind x of
                   Val       -> C_Value x
                   Failed    -> C_Fail
                   Branching 
                     | isGenerator (orRef x) -> C_Value x
                     | otherwise -> transBranching (branches x)

    transBranching []         = C_Fail
    transBranching [x]        = transVal x
    transBranching xs@(_:_:_) = C_Choice (fromHaskellList (map transVal xs))

{-
toData :: Curry a => a -> Result C_Data
toData _ st = prim_error (toCurry "toData not implemented") st --ctcStore True (toC_Term True) Nothing


toNumData :: Curry a => a -> Result C_NumData
toNumData _ st = prim_error (toCurry "toNumData not implemented") st
  --ctcStore True (\ store x -> (conv2num (toC_Term True store x))) Nothing



cmap _ List = List
cmap f (x :< xs) = f x :< cmap f xs

fromData :: Curry a => C_Data -> Result a
fromData _ st = prim_error (toCurry "fromData not implemented") st --fromC_Term
-}

prepApply :: (BaseCurry a,BaseCurry b) => 
  ((b -> Result a) -> b -> Result a) -> b -> (Prim (b -> Result a)) -> Result a
prepApply  prep x (PrimValue f)     st = prep f x st
prepApply  prep x (PrimOr r bs)     st = mapOr (prepApply prep x) r bs st
prepApply  _    _  cont             _  = patternFail "Prelude.prepApply" cont

--apply :: (Curry b, Curry (Prim (a -> b))) => Prim (a -> b) -> a -> b
apply (PrimValue f)     x st = f x st
apply (PrimOr r bs)     x st = mapOr (\ f -> apply f x) r bs st
apply cont              _ st = patternFail "Prelude.apply" cont

-- these functions are employed for higher order
pf :: Curry b => (a -> Result b) -> Prim (a -> Result b)
pf = PrimValue 

pc :: Curry b => (a -> b) -> (Prim (a -> Result b))
pc f = PrimValue (\ x _  -> f x)

pa :: Curry c => (a -> Prim (b -> Result c)) -> Prim (a -> Result (Prim (b -> Result c)))
pa f = PrimValue (\ x _  -> f x)

cp :: (b -> c) -> (a -> b) -> a -> c
cp f g x = f (g x)


cond :: Curry a => C_Success -> a -> Result a
cond C_Success  x _ = x
cond (C_SuccessOr r bs)     x st = mapOr (\ c -> cond c x) r bs st
cond x _ _ = patternFail "Prelude.cond" x


ifVar :: (Curry a,Curry b) => b -> a -> a -> a
ifVar = error "ifVar not implemented"

---------------------------------------------
-- to ease connecting external functions 
---------------------------------------------

extFunc1 :: (Curry a,Curry d,ConvertCH a b,ConvertCH d c) => (b->c) -> a -> Result d
extFunc1 f = gnfCTC (\ x' _ -> toCurry (f (fromCurry x'))) 

extFunc2 :: (Curry a, Curry c,Curry f,ConvertCH a b,ConvertCH c d,ConvertCH f e) => 
            (b->d->e) -> a -> c -> Result f
extFunc2 f x y = 
  gnfCTC (\x'->gnfCTC (\ y' _ -> toCurry (f (fromCurry x') (fromCurry y'))) y) x

extFunc3 :: (Curry c1, Curry c2, Curry c3, Curry cv,
             ConvertCH c1 h1,ConvertCH c2 h2,ConvertCH c3 h3,ConvertCH cv hv) => 
            (h1->h2->h3->hv) -> c1 -> c2 -> c3 -> Result cv
extFunc3 f x y z = 
  gnfCTC (\x' ->
  gnfCTC (\y' -> 
  gnfCTC (\z' _ -> toCurry (f (fromCurry x') (fromCurry y') (fromCurry z'))) z ) y) x

extFunc4 :: (Curry c1, Curry c2, Curry c3, Curry c4, Curry cv,
             ConvertCH c1 h1,ConvertCH c2 h2,ConvertCH c3 h3,ConvertCH c4 h4,ConvertCH cv hv) => 
            (h1->h2->h3->h4->hv) -> c1 -> c2 -> c3 -> c4 -> Result cv
extFunc4 f x1 x2 x3 x4 = 
  gnfCTC (\x1' ->
  gnfCTC (\x2' -> 
  gnfCTC (\x3' -> 
  gnfCTC (\x4' _ -> toCurry (f (fromCurry x1') (fromCurry x2') (fromCurry x3') (fromCurry x4'))) 
         x4) x3) x2) x1


hnf2 :: (Curry a, Curry b,Curry c) => (a->b->c) -> a -> b -> Result c
hnf2 f x y = hnfCTC (\ x' -> hnfCTC (\ y' _ -> f x' y') y) x

ioFunc0 :: (Curry b,ConvertCH b a) => IO a -> Result (C_IO b)
ioFunc0 iof _ = C_IO (\ _ -> iof Prelude.>>= \hv -> Prelude.return (IOVal (toCurry hv)))


ioFunc1 :: (Curry a,Curry d,ConvertCH a b,ConvertCH d c) => (b->IO c) -> a -> Result (C_IO d)
ioFunc1 iof x _ = C_IO (\ _ ->
           iof (fromCurry x) Prelude.>>= \hv ->
           Prelude.return (IOVal (toCurry hv)))

ioFunc2 :: (Curry a, Curry c,Curry f,ConvertCH a b,ConvertCH c d,ConvertCH f e) => 
            (b->d->IO e) -> a -> c -> Result (C_IO f)
ioFunc2 iof x y _ = C_IO (\ _ ->
           iof (fromCurry x) (fromCurry y) Prelude.>>= \hv ->
           Prelude.return (IOVal (toCurry hv)))

ioFunc3 iof x y z _ = C_IO (\ _ ->
           iof (fromCurry x) (fromCurry y) (fromCurry z) Prelude.>>= \hv ->
           Prelude.return (IOVal (toCurry hv)))

ghnfCTC2 :: (Curry a, Curry b,Curry c) => (a->b->c) -> a -> b -> Result c
ghnfCTC2 f x y = ghnfCTC (\x'-> ghnfCTC (\ y' _ -> f x' y') y) x



(=:<=) = error "function patterns not implemented"

-- from old autogenerated

data Prim t0 = PrimValue t0
  | PrimFail C_Exceptions
  | PrimOr OrRef (Branches (Prim t0))

data C_Four = C_F0
  | C_F1
  | C_F2
  | C_F3
  | C_FourFail C_Exceptions
  | C_FourOr OrRef (Branches C_Four)
  deriving (Eq)

instance BaseCurry C_Success where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = withRef(\ r -> C_SuccessOr(mkRef(r)(0)(i))([C_Success]))(0)

  failed  = C_SuccessFail

  branching  = C_SuccessOr

  consKind (C_SuccessOr _ _) = Branching
  consKind (C_SuccessFail _) = Failed
  consKind _ = Val

  exceptions (C_SuccessFail x) = x

  orRef (C_SuccessOr x _) = x

  branches (C_SuccessOr _ x) = x





instance BaseCurry C_Bool where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = withRef(\ r -> C_BoolOr(mkRef(r)(0)(i))([C_False,C_True]))(0)

  failed  = C_BoolFail

  branching  = C_BoolOr

  consKind (C_BoolOr _ _) = Branching
  consKind (C_BoolFail _) = Failed
  consKind _ = Val

  exceptions (C_BoolFail x) = x

  orRef (C_BoolOr x _) = x

  branches (C_BoolOr _ x) = x





instance BaseCurry C_Four where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = withRef(\ r -> C_FourOr(mkRef(r)(0)(i))([C_F0,C_F1,C_F2,C_F3]))(0)

  failed  = C_FourFail

  branching  = C_FourOr

  consKind (C_FourOr _ _) = Branching
  consKind (C_FourFail _) = Failed
  consKind _ = Val

  exceptions (C_FourFail x) = x

  orRef (C_FourOr x _) = x

  branches (C_FourOr _ x) = x








-- end included

type C_String = Curry.Module.Prelude.List Curry.Module.Prelude.C_Char

data T0 = T0
  | T0Fail Curry.RunTimeSystem.C_Exceptions
  | T0Or Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.Prelude.T0)

data List t0 = List
  | (:<) t0 (Curry.Module.Prelude.List t0)
  | ListFail Curry.RunTimeSystem.C_Exceptions
  | ListOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Prelude.List t0))

data T2 t0 t1 = T2 t0 t1
  | T2Fail Curry.RunTimeSystem.C_Exceptions
  | T2Or Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Prelude.T2 t0 t1))

data T3 t0 t1 t2 = T3 t0 t1 t2
  | T3Fail Curry.RunTimeSystem.C_Exceptions
  | T3Or Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Prelude.T3 t0 t1 t2))

data T4 t0 t1 t2 t3 = T4 t0 t1 t2 t3
  | T4Fail Curry.RunTimeSystem.C_Exceptions
  | T4Or Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Prelude.T4 t0 t1 t2 t3))

data T5 t0 t1 t2 t3 t4 = T5 t0 t1 t2 t3 t4
  | T5Fail Curry.RunTimeSystem.C_Exceptions
  | T5Or Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Prelude.T5 t0 t1 t2 t3 t4))

data T6 t0 t1 t2 t3 t4 t5 = T6 t0 t1 t2 t3 t4 t5
  | T6Fail Curry.RunTimeSystem.C_Exceptions
  | T6Or Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Prelude.T6 t0 t1 t2 t3 t4 t5))

data T7 t0 t1 t2 t3 t4 t5 t6 = T7 t0 t1 t2 t3 t4 t5 t6
  | T7Fail Curry.RunTimeSystem.C_Exceptions
  | T7Or Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Prelude.T7 t0 t1 t2 t3 t4 t5 t6))

data T8 t0 t1 t2 t3 t4 t5 t6 t7 = T8 t0 t1 t2 t3 t4 t5 t6 t7
  | T8Fail Curry.RunTimeSystem.C_Exceptions
  | T8Or Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Prelude.T8 t0 t1 t2 t3 t4 t5 t6 t7))

data T9 t0 t1 t2 t3 t4 t5 t6 t7 t8 = T9 t0 t1 t2 t3 t4 t5 t6 t7 t8
  | T9Fail Curry.RunTimeSystem.C_Exceptions
  | T9Or Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Prelude.T9 t0 t1 t2 t3 t4 t5 t6 t7 t8))

data T10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 = T10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9
  | T10Fail Curry.RunTimeSystem.C_Exceptions
  | T10Or Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Prelude.T10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9))

data T11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 = T11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10
  | T11Fail Curry.RunTimeSystem.C_Exceptions
  | T11Or Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Prelude.T11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10))

data T12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 = T12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11
  | T12Fail Curry.RunTimeSystem.C_Exceptions
  | T12Or Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Prelude.T12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11))

data T13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 = T13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12
  | T13Fail Curry.RunTimeSystem.C_Exceptions
  | T13Or Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Prelude.T13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12))

data T14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 = T14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13
  | T14Fail Curry.RunTimeSystem.C_Exceptions
  | T14Or Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Prelude.T14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13))

data T15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 = T15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
  | T15Fail Curry.RunTimeSystem.C_Exceptions
  | T15Or Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Prelude.T15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14))

data C_Ordering = C_LT
  | C_EQ
  | C_GT
  | C_OrderingFail Curry.RunTimeSystem.C_Exceptions
  | C_OrderingOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.Prelude.C_Ordering)

data C_Nat = C_IHi
  | C_O Curry.Module.Prelude.C_Nat
  | C_I Curry.Module.Prelude.C_Nat
  | C_NatFail Curry.RunTimeSystem.C_Exceptions
  | C_NatOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.Prelude.C_Nat)

data C_Int = C_Neg Curry.Module.Prelude.C_Nat
  | C_Zero
  | C_Pos Curry.Module.Prelude.C_Nat
  | C_IntFail Curry.RunTimeSystem.C_Exceptions
  | C_IntOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.Prelude.C_Int)

data C_Success = C_Success
  | C_SuccessFail Curry.RunTimeSystem.C_Exceptions
  | C_SuccessOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.Prelude.C_Success)

data C_Maybe t0 = C_Nothing
  | C_Just t0
  | C_MaybeFail Curry.RunTimeSystem.C_Exceptions
  | C_MaybeOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Prelude.C_Maybe t0))

data C_Either t0 t1 = C_Left t0
  | C_Right t1
  | C_EitherFail Curry.RunTimeSystem.C_Exceptions
  | C_EitherOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Prelude.C_Either t0 t1))

data C_SearchTree t0 = C_Fail
  | C_Value t0
  | C_Choice (Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t0))
  | C_Suspend
  | C_SearchTreeFail Curry.RunTimeSystem.C_Exceptions
  | C_SearchTreeOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Prelude.C_SearchTree t0))

instance BaseCurry Curry.Module.Prelude.T0 where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.T0Or(Curry.RunTimeSystem.mkRef(r)(0)(i))([Curry.Module.Prelude.T0]))(0)

  failed  = Curry.Module.Prelude.T0Fail

  branching  = Curry.Module.Prelude.T0Or

  consKind (Curry.Module.Prelude.T0Or _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.T0Fail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.T0Fail x) = x

  orRef (Curry.Module.Prelude.T0Or x _) = x

  branches (Curry.Module.Prelude.T0Or _ x) = x





instance (BaseCurry t0,BaseCurry t1) => BaseCurry (Curry.Module.Prelude.T2 t0 t1) where
  nf f (Curry.Module.Prelude.T2 x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.Prelude.T2(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Prelude.T2 x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.Prelude.T2(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.T2Or(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.Prelude.T2(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.Prelude.T2Fail

  branching  = Curry.Module.Prelude.T2Or

  consKind (Curry.Module.Prelude.T2Or _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.T2Fail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.T2Fail x) = x

  orRef (Curry.Module.Prelude.T2Or x _) = x

  branches (Curry.Module.Prelude.T2Or _ x) = x





instance (BaseCurry t0,BaseCurry t1,BaseCurry t2) => BaseCurry (Curry.Module.Prelude.T3 t0 t1 t2) where
  nf f (Curry.Module.Prelude.T3 x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.Prelude.T3(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Prelude.T3 x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.Prelude.T3(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.T3Or(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.Prelude.T3(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(3)

  failed  = Curry.Module.Prelude.T3Fail

  branching  = Curry.Module.Prelude.T3Or

  consKind (Curry.Module.Prelude.T3Or _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.T3Fail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.T3Fail x) = x

  orRef (Curry.Module.Prelude.T3Or x _) = x

  branches (Curry.Module.Prelude.T3Or _ x) = x





instance (BaseCurry t0,BaseCurry t1,BaseCurry t2,BaseCurry t3) => BaseCurry (Curry.Module.Prelude.T4 t0 t1 t2 t3) where
  nf f (Curry.Module.Prelude.T4 x1 x2 x3 x4) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> f(Curry.Module.Prelude.T4(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Prelude.T4 x1 x2 x3 x4) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> f(Curry.Module.Prelude.T4(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.T4Or(Curry.RunTimeSystem.mkRef(r)(4)(i))([Curry.Module.Prelude.T4(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(4)

  failed  = Curry.Module.Prelude.T4Fail

  branching  = Curry.Module.Prelude.T4Or

  consKind (Curry.Module.Prelude.T4Or _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.T4Fail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.T4Fail x) = x

  orRef (Curry.Module.Prelude.T4Or x _) = x

  branches (Curry.Module.Prelude.T4Or _ x) = x





instance (BaseCurry t0,BaseCurry t1,BaseCurry t2,BaseCurry t3,BaseCurry t4) => BaseCurry (Curry.Module.Prelude.T5 t0 t1 t2 t3 t4) where
  nf f (Curry.Module.Prelude.T5 x1 x2 x3 x4 x5) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> f(Curry.Module.Prelude.T5(v1)(v2)(v3)(v4)(v5))(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Prelude.T5 x1 x2 x3 x4 x5) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> f(Curry.Module.Prelude.T5(v1)(v2)(v3)(v4)(v5))(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.T5Or(Curry.RunTimeSystem.mkRef(r)(5)(i))([Curry.Module.Prelude.T5(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(5)

  failed  = Curry.Module.Prelude.T5Fail

  branching  = Curry.Module.Prelude.T5Or

  consKind (Curry.Module.Prelude.T5Or _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.T5Fail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.T5Fail x) = x

  orRef (Curry.Module.Prelude.T5Or x _) = x

  branches (Curry.Module.Prelude.T5Or _ x) = x





instance (BaseCurry t0,BaseCurry t1,BaseCurry t2,BaseCurry t3,BaseCurry t4,BaseCurry t5) => BaseCurry (Curry.Module.Prelude.T6 t0 t1 t2 t3 t4 t5) where
  nf f (Curry.Module.Prelude.T6 x1 x2 x3 x4 x5 x6) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> Curry.RunTimeSystem.nfCTC(\ v6 state6 -> f(Curry.Module.Prelude.T6(v1)(v2)(v3)(v4)(v5)(v6))(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Prelude.T6 x1 x2 x3 x4 x5 x6) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> Curry.RunTimeSystem.gnfCTC(\ v6 state6 -> f(Curry.Module.Prelude.T6(v1)(v2)(v3)(v4)(v5)(v6))(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.T6Or(Curry.RunTimeSystem.mkRef(r)(6)(i))([Curry.Module.Prelude.T6(Curry.RunTimeSystem.generator((Prelude.+)(r)((5::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(6)

  failed  = Curry.Module.Prelude.T6Fail

  branching  = Curry.Module.Prelude.T6Or

  consKind (Curry.Module.Prelude.T6Or _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.T6Fail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.T6Fail x) = x

  orRef (Curry.Module.Prelude.T6Or x _) = x

  branches (Curry.Module.Prelude.T6Or _ x) = x





instance (BaseCurry t0,BaseCurry t1,BaseCurry t2,BaseCurry t3,BaseCurry t4,BaseCurry t5,BaseCurry t6) => BaseCurry (Curry.Module.Prelude.T7 t0 t1 t2 t3 t4 t5 t6) where
  nf f (Curry.Module.Prelude.T7 x1 x2 x3 x4 x5 x6 x7) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> Curry.RunTimeSystem.nfCTC(\ v6 state6 -> Curry.RunTimeSystem.nfCTC(\ v7 state7 -> f(Curry.Module.Prelude.T7(v1)(v2)(v3)(v4)(v5)(v6)(v7))(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Prelude.T7 x1 x2 x3 x4 x5 x6 x7) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> Curry.RunTimeSystem.gnfCTC(\ v6 state6 -> Curry.RunTimeSystem.gnfCTC(\ v7 state7 -> f(Curry.Module.Prelude.T7(v1)(v2)(v3)(v4)(v5)(v6)(v7))(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.T7Or(Curry.RunTimeSystem.mkRef(r)(7)(i))([Curry.Module.Prelude.T7(Curry.RunTimeSystem.generator((Prelude.+)(r)((6::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((5::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(7)

  failed  = Curry.Module.Prelude.T7Fail

  branching  = Curry.Module.Prelude.T7Or

  consKind (Curry.Module.Prelude.T7Or _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.T7Fail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.T7Fail x) = x

  orRef (Curry.Module.Prelude.T7Or x _) = x

  branches (Curry.Module.Prelude.T7Or _ x) = x





instance (BaseCurry t0,BaseCurry t1,BaseCurry t2,BaseCurry t3,BaseCurry t4,BaseCurry t5,BaseCurry t6,BaseCurry t7) => BaseCurry (Curry.Module.Prelude.T8 t0 t1 t2 t3 t4 t5 t6 t7) where
  nf f (Curry.Module.Prelude.T8 x1 x2 x3 x4 x5 x6 x7 x8) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> Curry.RunTimeSystem.nfCTC(\ v6 state6 -> Curry.RunTimeSystem.nfCTC(\ v7 state7 -> Curry.RunTimeSystem.nfCTC(\ v8 state8 -> f(Curry.Module.Prelude.T8(v1)(v2)(v3)(v4)(v5)(v6)(v7)(v8))(state8))(x8)(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Prelude.T8 x1 x2 x3 x4 x5 x6 x7 x8) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> Curry.RunTimeSystem.gnfCTC(\ v6 state6 -> Curry.RunTimeSystem.gnfCTC(\ v7 state7 -> Curry.RunTimeSystem.gnfCTC(\ v8 state8 -> f(Curry.Module.Prelude.T8(v1)(v2)(v3)(v4)(v5)(v6)(v7)(v8))(state8))(x8)(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.T8Or(Curry.RunTimeSystem.mkRef(r)(8)(i))([Curry.Module.Prelude.T8(Curry.RunTimeSystem.generator((Prelude.+)(r)((7::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((6::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((5::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(8)

  failed  = Curry.Module.Prelude.T8Fail

  branching  = Curry.Module.Prelude.T8Or

  consKind (Curry.Module.Prelude.T8Or _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.T8Fail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.T8Fail x) = x

  orRef (Curry.Module.Prelude.T8Or x _) = x

  branches (Curry.Module.Prelude.T8Or _ x) = x





instance (BaseCurry t0,BaseCurry t1,BaseCurry t2,BaseCurry t3,BaseCurry t4,BaseCurry t5,BaseCurry t6,BaseCurry t7,BaseCurry t8) => BaseCurry (Curry.Module.Prelude.T9 t0 t1 t2 t3 t4 t5 t6 t7 t8) where
  nf f (Curry.Module.Prelude.T9 x1 x2 x3 x4 x5 x6 x7 x8 x9) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> Curry.RunTimeSystem.nfCTC(\ v6 state6 -> Curry.RunTimeSystem.nfCTC(\ v7 state7 -> Curry.RunTimeSystem.nfCTC(\ v8 state8 -> Curry.RunTimeSystem.nfCTC(\ v9 state9 -> f(Curry.Module.Prelude.T9(v1)(v2)(v3)(v4)(v5)(v6)(v7)(v8)(v9))(state9))(x9)(state8))(x8)(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Prelude.T9 x1 x2 x3 x4 x5 x6 x7 x8 x9) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> Curry.RunTimeSystem.gnfCTC(\ v6 state6 -> Curry.RunTimeSystem.gnfCTC(\ v7 state7 -> Curry.RunTimeSystem.gnfCTC(\ v8 state8 -> Curry.RunTimeSystem.gnfCTC(\ v9 state9 -> f(Curry.Module.Prelude.T9(v1)(v2)(v3)(v4)(v5)(v6)(v7)(v8)(v9))(state9))(x9)(state8))(x8)(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.T9Or(Curry.RunTimeSystem.mkRef(r)(9)(i))([Curry.Module.Prelude.T9(Curry.RunTimeSystem.generator((Prelude.+)(r)((8::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((7::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((6::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((5::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(9)

  failed  = Curry.Module.Prelude.T9Fail

  branching  = Curry.Module.Prelude.T9Or

  consKind (Curry.Module.Prelude.T9Or _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.T9Fail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.T9Fail x) = x

  orRef (Curry.Module.Prelude.T9Or x _) = x

  branches (Curry.Module.Prelude.T9Or _ x) = x





instance (BaseCurry t0,BaseCurry t1,BaseCurry t2,BaseCurry t3,BaseCurry t4,BaseCurry t5,BaseCurry t6,BaseCurry t7,BaseCurry t8,BaseCurry t9) => BaseCurry (Curry.Module.Prelude.T10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9) where
  nf f (Curry.Module.Prelude.T10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> Curry.RunTimeSystem.nfCTC(\ v6 state6 -> Curry.RunTimeSystem.nfCTC(\ v7 state7 -> Curry.RunTimeSystem.nfCTC(\ v8 state8 -> Curry.RunTimeSystem.nfCTC(\ v9 state9 -> Curry.RunTimeSystem.nfCTC(\ v10 state10 -> f(Curry.Module.Prelude.T10(v1)(v2)(v3)(v4)(v5)(v6)(v7)(v8)(v9)(v10))(state10))(x10)(state9))(x9)(state8))(x8)(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Prelude.T10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> Curry.RunTimeSystem.gnfCTC(\ v6 state6 -> Curry.RunTimeSystem.gnfCTC(\ v7 state7 -> Curry.RunTimeSystem.gnfCTC(\ v8 state8 -> Curry.RunTimeSystem.gnfCTC(\ v9 state9 -> Curry.RunTimeSystem.gnfCTC(\ v10 state10 -> f(Curry.Module.Prelude.T10(v1)(v2)(v3)(v4)(v5)(v6)(v7)(v8)(v9)(v10))(state10))(x10)(state9))(x9)(state8))(x8)(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.T10Or(Curry.RunTimeSystem.mkRef(r)(10)(i))([Curry.Module.Prelude.T10(Curry.RunTimeSystem.generator((Prelude.+)(r)((9::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((8::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((7::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((6::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((5::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(10)

  failed  = Curry.Module.Prelude.T10Fail

  branching  = Curry.Module.Prelude.T10Or

  consKind (Curry.Module.Prelude.T10Or _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.T10Fail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.T10Fail x) = x

  orRef (Curry.Module.Prelude.T10Or x _) = x

  branches (Curry.Module.Prelude.T10Or _ x) = x





instance (BaseCurry t0,BaseCurry t1,BaseCurry t2,BaseCurry t3,BaseCurry t4,BaseCurry t5,BaseCurry t6,BaseCurry t7,BaseCurry t8,BaseCurry t9,BaseCurry t10) => BaseCurry (Curry.Module.Prelude.T11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10) where
  nf f (Curry.Module.Prelude.T11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> Curry.RunTimeSystem.nfCTC(\ v6 state6 -> Curry.RunTimeSystem.nfCTC(\ v7 state7 -> Curry.RunTimeSystem.nfCTC(\ v8 state8 -> Curry.RunTimeSystem.nfCTC(\ v9 state9 -> Curry.RunTimeSystem.nfCTC(\ v10 state10 -> Curry.RunTimeSystem.nfCTC(\ v11 state11 -> f(Curry.Module.Prelude.T11(v1)(v2)(v3)(v4)(v5)(v6)(v7)(v8)(v9)(v10)(v11))(state11))(x11)(state10))(x10)(state9))(x9)(state8))(x8)(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Prelude.T11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> Curry.RunTimeSystem.gnfCTC(\ v6 state6 -> Curry.RunTimeSystem.gnfCTC(\ v7 state7 -> Curry.RunTimeSystem.gnfCTC(\ v8 state8 -> Curry.RunTimeSystem.gnfCTC(\ v9 state9 -> Curry.RunTimeSystem.gnfCTC(\ v10 state10 -> Curry.RunTimeSystem.gnfCTC(\ v11 state11 -> f(Curry.Module.Prelude.T11(v1)(v2)(v3)(v4)(v5)(v6)(v7)(v8)(v9)(v10)(v11))(state11))(x11)(state10))(x10)(state9))(x9)(state8))(x8)(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.T11Or(Curry.RunTimeSystem.mkRef(r)(11)(i))([Curry.Module.Prelude.T11(Curry.RunTimeSystem.generator((Prelude.+)(r)((10::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((9::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((8::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((7::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((6::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((5::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(11)

  failed  = Curry.Module.Prelude.T11Fail

  branching  = Curry.Module.Prelude.T11Or

  consKind (Curry.Module.Prelude.T11Or _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.T11Fail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.T11Fail x) = x

  orRef (Curry.Module.Prelude.T11Or x _) = x

  branches (Curry.Module.Prelude.T11Or _ x) = x





instance (BaseCurry t0,BaseCurry t1,BaseCurry t2,BaseCurry t3,BaseCurry t4,BaseCurry t5,BaseCurry t6,BaseCurry t7,BaseCurry t8,BaseCurry t9,BaseCurry t10,BaseCurry t11) => BaseCurry (Curry.Module.Prelude.T12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) where
  nf f (Curry.Module.Prelude.T12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> Curry.RunTimeSystem.nfCTC(\ v6 state6 -> Curry.RunTimeSystem.nfCTC(\ v7 state7 -> Curry.RunTimeSystem.nfCTC(\ v8 state8 -> Curry.RunTimeSystem.nfCTC(\ v9 state9 -> Curry.RunTimeSystem.nfCTC(\ v10 state10 -> Curry.RunTimeSystem.nfCTC(\ v11 state11 -> Curry.RunTimeSystem.nfCTC(\ v12 state12 -> f(Curry.Module.Prelude.T12(v1)(v2)(v3)(v4)(v5)(v6)(v7)(v8)(v9)(v10)(v11)(v12))(state12))(x12)(state11))(x11)(state10))(x10)(state9))(x9)(state8))(x8)(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Prelude.T12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> Curry.RunTimeSystem.gnfCTC(\ v6 state6 -> Curry.RunTimeSystem.gnfCTC(\ v7 state7 -> Curry.RunTimeSystem.gnfCTC(\ v8 state8 -> Curry.RunTimeSystem.gnfCTC(\ v9 state9 -> Curry.RunTimeSystem.gnfCTC(\ v10 state10 -> Curry.RunTimeSystem.gnfCTC(\ v11 state11 -> Curry.RunTimeSystem.gnfCTC(\ v12 state12 -> f(Curry.Module.Prelude.T12(v1)(v2)(v3)(v4)(v5)(v6)(v7)(v8)(v9)(v10)(v11)(v12))(state12))(x12)(state11))(x11)(state10))(x10)(state9))(x9)(state8))(x8)(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.T12Or(Curry.RunTimeSystem.mkRef(r)(12)(i))([Curry.Module.Prelude.T12(Curry.RunTimeSystem.generator((Prelude.+)(r)((11::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((10::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((9::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((8::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((7::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((6::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((5::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(12)

  failed  = Curry.Module.Prelude.T12Fail

  branching  = Curry.Module.Prelude.T12Or

  consKind (Curry.Module.Prelude.T12Or _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.T12Fail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.T12Fail x) = x

  orRef (Curry.Module.Prelude.T12Or x _) = x

  branches (Curry.Module.Prelude.T12Or _ x) = x





instance (BaseCurry t0,BaseCurry t1,BaseCurry t2,BaseCurry t3,BaseCurry t4,BaseCurry t5,BaseCurry t6,BaseCurry t7,BaseCurry t8,BaseCurry t9,BaseCurry t10,BaseCurry t11,BaseCurry t12) => BaseCurry (Curry.Module.Prelude.T13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12) where
  nf f (Curry.Module.Prelude.T13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> Curry.RunTimeSystem.nfCTC(\ v6 state6 -> Curry.RunTimeSystem.nfCTC(\ v7 state7 -> Curry.RunTimeSystem.nfCTC(\ v8 state8 -> Curry.RunTimeSystem.nfCTC(\ v9 state9 -> Curry.RunTimeSystem.nfCTC(\ v10 state10 -> Curry.RunTimeSystem.nfCTC(\ v11 state11 -> Curry.RunTimeSystem.nfCTC(\ v12 state12 -> Curry.RunTimeSystem.nfCTC(\ v13 state13 -> f(Curry.Module.Prelude.T13(v1)(v2)(v3)(v4)(v5)(v6)(v7)(v8)(v9)(v10)(v11)(v12)(v13))(state13))(x13)(state12))(x12)(state11))(x11)(state10))(x10)(state9))(x9)(state8))(x8)(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Prelude.T13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> Curry.RunTimeSystem.gnfCTC(\ v6 state6 -> Curry.RunTimeSystem.gnfCTC(\ v7 state7 -> Curry.RunTimeSystem.gnfCTC(\ v8 state8 -> Curry.RunTimeSystem.gnfCTC(\ v9 state9 -> Curry.RunTimeSystem.gnfCTC(\ v10 state10 -> Curry.RunTimeSystem.gnfCTC(\ v11 state11 -> Curry.RunTimeSystem.gnfCTC(\ v12 state12 -> Curry.RunTimeSystem.gnfCTC(\ v13 state13 -> f(Curry.Module.Prelude.T13(v1)(v2)(v3)(v4)(v5)(v6)(v7)(v8)(v9)(v10)(v11)(v12)(v13))(state13))(x13)(state12))(x12)(state11))(x11)(state10))(x10)(state9))(x9)(state8))(x8)(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.T13Or(Curry.RunTimeSystem.mkRef(r)(13)(i))([Curry.Module.Prelude.T13(Curry.RunTimeSystem.generator((Prelude.+)(r)((12::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((11::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((10::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((9::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((8::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((7::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((6::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((5::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(13)

  failed  = Curry.Module.Prelude.T13Fail

  branching  = Curry.Module.Prelude.T13Or

  consKind (Curry.Module.Prelude.T13Or _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.T13Fail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.T13Fail x) = x

  orRef (Curry.Module.Prelude.T13Or x _) = x

  branches (Curry.Module.Prelude.T13Or _ x) = x





instance (BaseCurry t0,BaseCurry t1,BaseCurry t2,BaseCurry t3,BaseCurry t4,BaseCurry t5,BaseCurry t6,BaseCurry t7,BaseCurry t8,BaseCurry t9,BaseCurry t10,BaseCurry t11,BaseCurry t12,BaseCurry t13) => BaseCurry (Curry.Module.Prelude.T14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13) where
  nf f (Curry.Module.Prelude.T14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> Curry.RunTimeSystem.nfCTC(\ v6 state6 -> Curry.RunTimeSystem.nfCTC(\ v7 state7 -> Curry.RunTimeSystem.nfCTC(\ v8 state8 -> Curry.RunTimeSystem.nfCTC(\ v9 state9 -> Curry.RunTimeSystem.nfCTC(\ v10 state10 -> Curry.RunTimeSystem.nfCTC(\ v11 state11 -> Curry.RunTimeSystem.nfCTC(\ v12 state12 -> Curry.RunTimeSystem.nfCTC(\ v13 state13 -> Curry.RunTimeSystem.nfCTC(\ v14 state14 -> f(Curry.Module.Prelude.T14(v1)(v2)(v3)(v4)(v5)(v6)(v7)(v8)(v9)(v10)(v11)(v12)(v13)(v14))(state14))(x14)(state13))(x13)(state12))(x12)(state11))(x11)(state10))(x10)(state9))(x9)(state8))(x8)(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Prelude.T14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> Curry.RunTimeSystem.gnfCTC(\ v6 state6 -> Curry.RunTimeSystem.gnfCTC(\ v7 state7 -> Curry.RunTimeSystem.gnfCTC(\ v8 state8 -> Curry.RunTimeSystem.gnfCTC(\ v9 state9 -> Curry.RunTimeSystem.gnfCTC(\ v10 state10 -> Curry.RunTimeSystem.gnfCTC(\ v11 state11 -> Curry.RunTimeSystem.gnfCTC(\ v12 state12 -> Curry.RunTimeSystem.gnfCTC(\ v13 state13 -> Curry.RunTimeSystem.gnfCTC(\ v14 state14 -> f(Curry.Module.Prelude.T14(v1)(v2)(v3)(v4)(v5)(v6)(v7)(v8)(v9)(v10)(v11)(v12)(v13)(v14))(state14))(x14)(state13))(x13)(state12))(x12)(state11))(x11)(state10))(x10)(state9))(x9)(state8))(x8)(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.T14Or(Curry.RunTimeSystem.mkRef(r)(14)(i))([Curry.Module.Prelude.T14(Curry.RunTimeSystem.generator((Prelude.+)(r)((13::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((12::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((11::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((10::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((9::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((8::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((7::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((6::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((5::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(14)

  failed  = Curry.Module.Prelude.T14Fail

  branching  = Curry.Module.Prelude.T14Or

  consKind (Curry.Module.Prelude.T14Or _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.T14Fail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.T14Fail x) = x

  orRef (Curry.Module.Prelude.T14Or x _) = x

  branches (Curry.Module.Prelude.T14Or _ x) = x





instance (BaseCurry t0,BaseCurry t1,BaseCurry t2,BaseCurry t3,BaseCurry t4,BaseCurry t5,BaseCurry t6,BaseCurry t7,BaseCurry t8,BaseCurry t9,BaseCurry t10,BaseCurry t11,BaseCurry t12,BaseCurry t13,BaseCurry t14) => BaseCurry (Curry.Module.Prelude.T15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14) where
  nf f (Curry.Module.Prelude.T15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> Curry.RunTimeSystem.nfCTC(\ v6 state6 -> Curry.RunTimeSystem.nfCTC(\ v7 state7 -> Curry.RunTimeSystem.nfCTC(\ v8 state8 -> Curry.RunTimeSystem.nfCTC(\ v9 state9 -> Curry.RunTimeSystem.nfCTC(\ v10 state10 -> Curry.RunTimeSystem.nfCTC(\ v11 state11 -> Curry.RunTimeSystem.nfCTC(\ v12 state12 -> Curry.RunTimeSystem.nfCTC(\ v13 state13 -> Curry.RunTimeSystem.nfCTC(\ v14 state14 -> Curry.RunTimeSystem.nfCTC(\ v15 state15 -> f(Curry.Module.Prelude.T15(v1)(v2)(v3)(v4)(v5)(v6)(v7)(v8)(v9)(v10)(v11)(v12)(v13)(v14)(v15))(state15))(x15)(state14))(x14)(state13))(x13)(state12))(x12)(state11))(x11)(state10))(x10)(state9))(x9)(state8))(x8)(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Prelude.T15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> Curry.RunTimeSystem.gnfCTC(\ v6 state6 -> Curry.RunTimeSystem.gnfCTC(\ v7 state7 -> Curry.RunTimeSystem.gnfCTC(\ v8 state8 -> Curry.RunTimeSystem.gnfCTC(\ v9 state9 -> Curry.RunTimeSystem.gnfCTC(\ v10 state10 -> Curry.RunTimeSystem.gnfCTC(\ v11 state11 -> Curry.RunTimeSystem.gnfCTC(\ v12 state12 -> Curry.RunTimeSystem.gnfCTC(\ v13 state13 -> Curry.RunTimeSystem.gnfCTC(\ v14 state14 -> Curry.RunTimeSystem.gnfCTC(\ v15 state15 -> f(Curry.Module.Prelude.T15(v1)(v2)(v3)(v4)(v5)(v6)(v7)(v8)(v9)(v10)(v11)(v12)(v13)(v14)(v15))(state15))(x15)(state14))(x14)(state13))(x13)(state12))(x12)(state11))(x11)(state10))(x10)(state9))(x9)(state8))(x8)(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.T15Or(Curry.RunTimeSystem.mkRef(r)(15)(i))([Curry.Module.Prelude.T15(Curry.RunTimeSystem.generator((Prelude.+)(r)((14::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((13::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((12::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((11::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((10::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((9::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((8::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((7::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((6::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((5::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(15)

  failed  = Curry.Module.Prelude.T15Fail

  branching  = Curry.Module.Prelude.T15Or

  consKind (Curry.Module.Prelude.T15Or _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.T15Fail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.T15Fail x) = x

  orRef (Curry.Module.Prelude.T15Or x _) = x

  branches (Curry.Module.Prelude.T15Or _ x) = x





instance BaseCurry Curry.Module.Prelude.C_Ordering where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.C_OrderingOr(Curry.RunTimeSystem.mkRef(r)(0)(i))([Curry.Module.Prelude.C_LT,Curry.Module.Prelude.C_EQ,Curry.Module.Prelude.C_GT]))(0)

  failed  = Curry.Module.Prelude.C_OrderingFail

  branching  = Curry.Module.Prelude.C_OrderingOr

  consKind (Curry.Module.Prelude.C_OrderingOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.C_OrderingFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.C_OrderingFail x) = x

  orRef (Curry.Module.Prelude.C_OrderingOr x _) = x

  branches (Curry.Module.Prelude.C_OrderingOr _ x) = x





instance BaseCurry Curry.Module.Prelude.C_Nat where
  nf f (Curry.Module.Prelude.C_O x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Prelude.C_O(v1))(state1))(x1)(state0)
  nf f (Curry.Module.Prelude.C_I x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Prelude.C_I(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Prelude.C_O x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Prelude.C_O(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.Prelude.C_I x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Prelude.C_I(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.C_NatOr(Curry.RunTimeSystem.mkRef(r)(1)(i))([Curry.Module.Prelude.C_IHi,Curry.Module.Prelude.C_O(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.Prelude.C_I(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(1)

  failed  = Curry.Module.Prelude.C_NatFail

  branching  = Curry.Module.Prelude.C_NatOr

  consKind (Curry.Module.Prelude.C_NatOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.C_NatFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.C_NatFail x) = x

  orRef (Curry.Module.Prelude.C_NatOr x _) = x

  branches (Curry.Module.Prelude.C_NatOr _ x) = x





instance BaseCurry Curry.Module.Prelude.C_Int where
  nf f (Curry.Module.Prelude.C_Neg x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Prelude.C_Neg(v1))(state1))(x1)(state0)
  nf f (Curry.Module.Prelude.C_Pos x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Prelude.C_Pos(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Prelude.C_Neg x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Prelude.C_Neg(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.Prelude.C_Pos x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Prelude.C_Pos(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.C_IntOr(Curry.RunTimeSystem.mkRef(r)(1)(i))([Curry.Module.Prelude.C_Neg(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.Prelude.C_Zero,Curry.Module.Prelude.C_Pos(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(1)

  failed  = Curry.Module.Prelude.C_IntFail

  branching  = Curry.Module.Prelude.C_IntOr

  consKind (Curry.Module.Prelude.C_IntOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.C_IntFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.C_IntFail x) = x

  orRef (Curry.Module.Prelude.C_IntOr x _) = x

  branches (Curry.Module.Prelude.C_IntOr _ x) = x





instance (BaseCurry t0) => BaseCurry (Curry.Module.Prelude.C_Maybe t0) where
  nf f (Curry.Module.Prelude.C_Just x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Prelude.C_Just(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Prelude.C_Just x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Prelude.C_Just(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.C_MaybeOr(Curry.RunTimeSystem.mkRef(r)(1)(i))([Curry.Module.Prelude.C_Nothing,Curry.Module.Prelude.C_Just(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(1)

  failed  = Curry.Module.Prelude.C_MaybeFail

  branching  = Curry.Module.Prelude.C_MaybeOr

  consKind (Curry.Module.Prelude.C_MaybeOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.C_MaybeFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.C_MaybeFail x) = x

  orRef (Curry.Module.Prelude.C_MaybeOr x _) = x

  branches (Curry.Module.Prelude.C_MaybeOr _ x) = x





instance (BaseCurry t0,BaseCurry t1) => BaseCurry (Curry.Module.Prelude.C_Either t0 t1) where
  nf f (Curry.Module.Prelude.C_Left x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Prelude.C_Left(v1))(state1))(x1)(state0)
  nf f (Curry.Module.Prelude.C_Right x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Prelude.C_Right(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Prelude.C_Left x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Prelude.C_Left(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.Prelude.C_Right x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Prelude.C_Right(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.C_EitherOr(Curry.RunTimeSystem.mkRef(r)(1)(i))([Curry.Module.Prelude.C_Left(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.Prelude.C_Right(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(1)

  failed  = Curry.Module.Prelude.C_EitherFail

  branching  = Curry.Module.Prelude.C_EitherOr

  consKind (Curry.Module.Prelude.C_EitherOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.C_EitherFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.C_EitherFail x) = x

  orRef (Curry.Module.Prelude.C_EitherOr x _) = x

  branches (Curry.Module.Prelude.C_EitherOr _ x) = x





instance (BaseCurry t0) => BaseCurry (Curry.Module.Prelude.C_SearchTree t0) where
  nf f (Curry.Module.Prelude.C_Value x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Prelude.C_Value(v1))(state1))(x1)(state0)
  nf f (Curry.Module.Prelude.C_Choice x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Prelude.C_Choice(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Prelude.C_Value x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Prelude.C_Value(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.Prelude.C_Choice x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Prelude.C_Choice(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Prelude.C_SearchTreeOr(Curry.RunTimeSystem.mkRef(r)(1)(i))([Curry.Module.Prelude.C_Fail,Curry.Module.Prelude.C_Value(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.Prelude.C_Choice(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.Prelude.C_Suspend]))(1)

  failed  = Curry.Module.Prelude.C_SearchTreeFail

  branching  = Curry.Module.Prelude.C_SearchTreeOr

  consKind (Curry.Module.Prelude.C_SearchTreeOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Prelude.C_SearchTreeFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Prelude.C_SearchTreeFail x) = x

  orRef (Curry.Module.Prelude.C_SearchTreeOr x _) = x

  branches (Curry.Module.Prelude.C_SearchTreeOr _ x) = x





instance Curry Curry.Module.Prelude.T0 where
  strEq Curry.Module.Prelude.T0 Curry.Module.Prelude.T0 st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.Prelude.T0 Curry.Module.Prelude.T0 st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.Prelude.T0 st = Curry.Module.Prelude.T0

  foldCurry f c Curry.Module.Prelude.T0 st = c

  typeName _ = "()"

  showQ _ Curry.Module.Prelude.T0 = Prelude.showString("()")
  showQ _ (Curry.Module.Prelude.T0Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0,Curry t1) => Curry (Curry.Module.Prelude.T2 t0 t1) where
  strEq (Curry.Module.Prelude.T2 x1 x2) (Curry.Module.Prelude.T2 y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Prelude.T2 x1 x2) (Curry.Module.Prelude.T2 y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Prelude.T2 x1 x2) st = Curry.Module.Prelude.T2(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.Prelude.T2 x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "(,)"

  showQ d (Curry.Module.Prelude.T2 x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x2))))


  showQ _ (Curry.Module.Prelude.T2Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0,Curry t1,Curry t2) => Curry (Curry.Module.Prelude.T3 t0 t1 t2) where
  strEq (Curry.Module.Prelude.T3 x1 x2 x3) (Curry.Module.Prelude.T3 y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Prelude.T3 x1 x2 x3) (Curry.Module.Prelude.T3 y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Prelude.T3 x1 x2 x3) st = Curry.Module.Prelude.T3(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))

  foldCurry f c (Curry.Module.Prelude.T3 x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)

  typeName _ = "(,,)"

  showQ d (Curry.Module.Prelude.T3 x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x3))))))


  showQ _ (Curry.Module.Prelude.T3Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0,Curry t1,Curry t2,Curry t3) => Curry (Curry.Module.Prelude.T4 t0 t1 t2 t3) where
  strEq (Curry.Module.Prelude.T4 x1 x2 x3 x4) (Curry.Module.Prelude.T4 y1 y2 y3 y4) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Prelude.T4 x1 x2 x3 x4) (Curry.Module.Prelude.T4 y1 y2 y3 y4) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.genEq(x4)(y4)(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Prelude.T4 x1 x2 x3 x4) st = Curry.Module.Prelude.T4(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))

  foldCurry f c (Curry.Module.Prelude.T4 x1 x2 x3 x4) st = f(x1)(f(x2)(f(x3)(f(x4)(c)(st))(st))(st))(st)

  typeName _ = "(,,,)"

  showQ d (Curry.Module.Prelude.T4 x1 x2 x3 x4) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x4))))))))


  showQ _ (Curry.Module.Prelude.T4Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4) => Curry (Curry.Module.Prelude.T5 t0 t1 t2 t3 t4) where
  strEq (Curry.Module.Prelude.T5 x1 x2 x3 x4 x5) (Curry.Module.Prelude.T5 y1 y2 y3 y4 y5) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Prelude.T5 x1 x2 x3 x4 x5) (Curry.Module.Prelude.T5 y1 y2 y3 y4 y5) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.genEq(x5)(y5)(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Prelude.T5 x1 x2 x3 x4 x5) st = Curry.Module.Prelude.T5(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))

  foldCurry f c (Curry.Module.Prelude.T5 x1 x2 x3 x4 x5) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(c)(st))(st))(st))(st))(st)

  typeName _ = "(,,,,)"

  showQ d (Curry.Module.Prelude.T5 x1 x2 x3 x4 x5) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x5))))))))))


  showQ _ (Curry.Module.Prelude.T5Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5) => Curry (Curry.Module.Prelude.T6 t0 t1 t2 t3 t4 t5) where
  strEq (Curry.Module.Prelude.T6 x1 x2 x3 x4 x5 x6) (Curry.Module.Prelude.T6 y1 y2 y3 y4 y5 y6) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(Curry.Module.Prelude.genStrEq(x6)(y6)(st))(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Prelude.T6 x1 x2 x3 x4 x5 x6) (Curry.Module.Prelude.T6 y1 y2 y3 y4 y5 y6) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x5)(y5)(st))(Curry.Module.Prelude.genEq(x6)(y6)(st))(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Prelude.T6 x1 x2 x3 x4 x5 x6) st = Curry.Module.Prelude.T6(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))(f((5::Int))(x6)(st))

  foldCurry f c (Curry.Module.Prelude.T6 x1 x2 x3 x4 x5 x6) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(f(x6)(c)(st))(st))(st))(st))(st))(st)

  typeName _ = "(,,,,,)"

  showQ d (Curry.Module.Prelude.T6 x1 x2 x3 x4 x5 x6) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x6))))))))))))


  showQ _ (Curry.Module.Prelude.T6Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6) => Curry (Curry.Module.Prelude.T7 t0 t1 t2 t3 t4 t5 t6) where
  strEq (Curry.Module.Prelude.T7 x1 x2 x3 x4 x5 x6 x7) (Curry.Module.Prelude.T7 y1 y2 y3 y4 y5 y6 y7) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x6)(y6)(st))(Curry.Module.Prelude.genStrEq(x7)(y7)(st))(st))(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Prelude.T7 x1 x2 x3 x4 x5 x6 x7) (Curry.Module.Prelude.T7 y1 y2 y3 y4 y5 y6 y7) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x5)(y5)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x6)(y6)(st))(Curry.Module.Prelude.genEq(x7)(y7)(st))(st))(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Prelude.T7 x1 x2 x3 x4 x5 x6 x7) st = Curry.Module.Prelude.T7(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))(f((5::Int))(x6)(st))(f((6::Int))(x7)(st))

  foldCurry f c (Curry.Module.Prelude.T7 x1 x2 x3 x4 x5 x6 x7) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(f(x6)(f(x7)(c)(st))(st))(st))(st))(st))(st))(st)

  typeName _ = "(,,,,,,)"

  showQ d (Curry.Module.Prelude.T7 x1 x2 x3 x4 x5 x6 x7) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x6))((Prelude..)(Prelude.showChar(','))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x7))))))))))))))


  showQ _ (Curry.Module.Prelude.T7Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7) => Curry (Curry.Module.Prelude.T8 t0 t1 t2 t3 t4 t5 t6 t7) where
  strEq (Curry.Module.Prelude.T8 x1 x2 x3 x4 x5 x6 x7 x8) (Curry.Module.Prelude.T8 y1 y2 y3 y4 y5 y6 y7 y8) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x6)(y6)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x7)(y7)(st))(Curry.Module.Prelude.genStrEq(x8)(y8)(st))(st))(st))(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Prelude.T8 x1 x2 x3 x4 x5 x6 x7 x8) (Curry.Module.Prelude.T8 y1 y2 y3 y4 y5 y6 y7 y8) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x5)(y5)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x6)(y6)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x7)(y7)(st))(Curry.Module.Prelude.genEq(x8)(y8)(st))(st))(st))(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Prelude.T8 x1 x2 x3 x4 x5 x6 x7 x8) st = Curry.Module.Prelude.T8(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))(f((5::Int))(x6)(st))(f((6::Int))(x7)(st))(f((7::Int))(x8)(st))

  foldCurry f c (Curry.Module.Prelude.T8 x1 x2 x3 x4 x5 x6 x7 x8) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(f(x6)(f(x7)(f(x8)(c)(st))(st))(st))(st))(st))(st))(st))(st)

  typeName _ = "(,,,,,,,)"

  showQ d (Curry.Module.Prelude.T8 x1 x2 x3 x4 x5 x6 x7 x8) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x6))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x7))((Prelude..)(Prelude.showChar(','))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x8))))))))))))))))


  showQ _ (Curry.Module.Prelude.T8Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8) => Curry (Curry.Module.Prelude.T9 t0 t1 t2 t3 t4 t5 t6 t7 t8) where
  strEq (Curry.Module.Prelude.T9 x1 x2 x3 x4 x5 x6 x7 x8 x9) (Curry.Module.Prelude.T9 y1 y2 y3 y4 y5 y6 y7 y8 y9) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x6)(y6)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x7)(y7)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x8)(y8)(st))(Curry.Module.Prelude.genStrEq(x9)(y9)(st))(st))(st))(st))(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Prelude.T9 x1 x2 x3 x4 x5 x6 x7 x8 x9) (Curry.Module.Prelude.T9 y1 y2 y3 y4 y5 y6 y7 y8 y9) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x5)(y5)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x6)(y6)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x7)(y7)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x8)(y8)(st))(Curry.Module.Prelude.genEq(x9)(y9)(st))(st))(st))(st))(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Prelude.T9 x1 x2 x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.Prelude.T9(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))(f((5::Int))(x6)(st))(f((6::Int))(x7)(st))(f((7::Int))(x8)(st))(f((8::Int))(x9)(st))

  foldCurry f c (Curry.Module.Prelude.T9 x1 x2 x3 x4 x5 x6 x7 x8 x9) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(f(x6)(f(x7)(f(x8)(f(x9)(c)(st))(st))(st))(st))(st))(st))(st))(st))(st)

  typeName _ = "(,,,,,,,,)"

  showQ d (Curry.Module.Prelude.T9 x1 x2 x3 x4 x5 x6 x7 x8 x9) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x6))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x7))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x8))((Prelude..)(Prelude.showChar(','))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x9))))))))))))))))))


  showQ _ (Curry.Module.Prelude.T9Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8,Curry t9) => Curry (Curry.Module.Prelude.T10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9) where
  strEq (Curry.Module.Prelude.T10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) (Curry.Module.Prelude.T10 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x6)(y6)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x7)(y7)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x8)(y8)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x9)(y9)(st))(Curry.Module.Prelude.genStrEq(x10)(y10)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Prelude.T10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) (Curry.Module.Prelude.T10 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x5)(y5)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x6)(y6)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x7)(y7)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x8)(y8)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x9)(y9)(st))(Curry.Module.Prelude.genEq(x10)(y10)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Prelude.T10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) st = Curry.Module.Prelude.T10(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))(f((5::Int))(x6)(st))(f((6::Int))(x7)(st))(f((7::Int))(x8)(st))(f((8::Int))(x9)(st))(f((9::Int))(x10)(st))

  foldCurry f c (Curry.Module.Prelude.T10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(f(x6)(f(x7)(f(x8)(f(x9)(f(x10)(c)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)

  typeName _ = "(,,,,,,,,,)"

  showQ d (Curry.Module.Prelude.T10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x6))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x7))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x8))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x9))((Prelude..)(Prelude.showChar(','))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x10))))))))))))))))))))


  showQ _ (Curry.Module.Prelude.T10Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8,Curry t9,Curry t10) => Curry (Curry.Module.Prelude.T11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10) where
  strEq (Curry.Module.Prelude.T11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) (Curry.Module.Prelude.T11 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x6)(y6)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x7)(y7)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x8)(y8)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x9)(y9)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x10)(y10)(st))(Curry.Module.Prelude.genStrEq(x11)(y11)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Prelude.T11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) (Curry.Module.Prelude.T11 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x5)(y5)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x6)(y6)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x7)(y7)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x8)(y8)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x9)(y9)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x10)(y10)(st))(Curry.Module.Prelude.genEq(x11)(y11)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Prelude.T11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) st = Curry.Module.Prelude.T11(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))(f((5::Int))(x6)(st))(f((6::Int))(x7)(st))(f((7::Int))(x8)(st))(f((8::Int))(x9)(st))(f((9::Int))(x10)(st))(f((10::Int))(x11)(st))

  foldCurry f c (Curry.Module.Prelude.T11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(f(x6)(f(x7)(f(x8)(f(x9)(f(x10)(f(x11)(c)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)

  typeName _ = "(,,,,,,,,,,)"

  showQ d (Curry.Module.Prelude.T11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x6))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x7))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x8))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x9))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x10))((Prelude..)(Prelude.showChar(','))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x11))))))))))))))))))))))


  showQ _ (Curry.Module.Prelude.T11Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8,Curry t9,Curry t10,Curry t11) => Curry (Curry.Module.Prelude.T12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) where
  strEq (Curry.Module.Prelude.T12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) (Curry.Module.Prelude.T12 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x6)(y6)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x7)(y7)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x8)(y8)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x9)(y9)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x10)(y10)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x11)(y11)(st))(Curry.Module.Prelude.genStrEq(x12)(y12)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Prelude.T12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) (Curry.Module.Prelude.T12 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x5)(y5)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x6)(y6)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x7)(y7)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x8)(y8)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x9)(y9)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x10)(y10)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x11)(y11)(st))(Curry.Module.Prelude.genEq(x12)(y12)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Prelude.T12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) st = Curry.Module.Prelude.T12(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))(f((5::Int))(x6)(st))(f((6::Int))(x7)(st))(f((7::Int))(x8)(st))(f((8::Int))(x9)(st))(f((9::Int))(x10)(st))(f((10::Int))(x11)(st))(f((11::Int))(x12)(st))

  foldCurry f c (Curry.Module.Prelude.T12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(f(x6)(f(x7)(f(x8)(f(x9)(f(x10)(f(x11)(f(x12)(c)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)

  typeName _ = "(,,,,,,,,,,,)"

  showQ d (Curry.Module.Prelude.T12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x6))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x7))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x8))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x9))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x10))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x11))((Prelude..)(Prelude.showChar(','))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x12))))))))))))))))))))))))


  showQ _ (Curry.Module.Prelude.T12Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8,Curry t9,Curry t10,Curry t11,Curry t12) => Curry (Curry.Module.Prelude.T13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12) where
  strEq (Curry.Module.Prelude.T13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) (Curry.Module.Prelude.T13 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x6)(y6)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x7)(y7)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x8)(y8)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x9)(y9)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x10)(y10)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x11)(y11)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x12)(y12)(st))(Curry.Module.Prelude.genStrEq(x13)(y13)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Prelude.T13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) (Curry.Module.Prelude.T13 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x5)(y5)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x6)(y6)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x7)(y7)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x8)(y8)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x9)(y9)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x10)(y10)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x11)(y11)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x12)(y12)(st))(Curry.Module.Prelude.genEq(x13)(y13)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Prelude.T13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) st = Curry.Module.Prelude.T13(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))(f((5::Int))(x6)(st))(f((6::Int))(x7)(st))(f((7::Int))(x8)(st))(f((8::Int))(x9)(st))(f((9::Int))(x10)(st))(f((10::Int))(x11)(st))(f((11::Int))(x12)(st))(f((12::Int))(x13)(st))

  foldCurry f c (Curry.Module.Prelude.T13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(f(x6)(f(x7)(f(x8)(f(x9)(f(x10)(f(x11)(f(x12)(f(x13)(c)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)

  typeName _ = "(,,,,,,,,,,,,)"

  showQ d (Curry.Module.Prelude.T13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x6))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x7))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x8))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x9))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x10))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x11))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x12))((Prelude..)(Prelude.showChar(','))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x13))))))))))))))))))))))))))


  showQ _ (Curry.Module.Prelude.T13Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8,Curry t9,Curry t10,Curry t11,Curry t12,Curry t13) => Curry (Curry.Module.Prelude.T14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13) where
  strEq (Curry.Module.Prelude.T14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) (Curry.Module.Prelude.T14 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x6)(y6)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x7)(y7)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x8)(y8)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x9)(y9)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x10)(y10)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x11)(y11)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x12)(y12)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x13)(y13)(st))(Curry.Module.Prelude.genStrEq(x14)(y14)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Prelude.T14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) (Curry.Module.Prelude.T14 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x5)(y5)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x6)(y6)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x7)(y7)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x8)(y8)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x9)(y9)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x10)(y10)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x11)(y11)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x12)(y12)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x13)(y13)(st))(Curry.Module.Prelude.genEq(x14)(y14)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Prelude.T14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) st = Curry.Module.Prelude.T14(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))(f((5::Int))(x6)(st))(f((6::Int))(x7)(st))(f((7::Int))(x8)(st))(f((8::Int))(x9)(st))(f((9::Int))(x10)(st))(f((10::Int))(x11)(st))(f((11::Int))(x12)(st))(f((12::Int))(x13)(st))(f((13::Int))(x14)(st))

  foldCurry f c (Curry.Module.Prelude.T14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(f(x6)(f(x7)(f(x8)(f(x9)(f(x10)(f(x11)(f(x12)(f(x13)(f(x14)(c)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)

  typeName _ = "(,,,,,,,,,,,,,)"

  showQ d (Curry.Module.Prelude.T14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x6))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x7))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x8))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x9))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x10))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x11))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x12))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x13))((Prelude..)(Prelude.showChar(','))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x14))))))))))))))))))))))))))))


  showQ _ (Curry.Module.Prelude.T14Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4,Curry t5,Curry t6,Curry t7,Curry t8,Curry t9,Curry t10,Curry t11,Curry t12,Curry t13,Curry t14) => Curry (Curry.Module.Prelude.T15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14) where
  strEq (Curry.Module.Prelude.T15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (Curry.Module.Prelude.T15 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x6)(y6)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x7)(y7)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x8)(y8)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x9)(y9)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x10)(y10)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x11)(y11)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x12)(y12)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x13)(y13)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x14)(y14)(st))(Curry.Module.Prelude.genStrEq(x15)(y15)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Prelude.T15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) (Curry.Module.Prelude.T15 y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14 y15) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x5)(y5)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x6)(y6)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x7)(y7)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x8)(y8)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x9)(y9)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x10)(y10)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x11)(y11)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x12)(y12)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x13)(y13)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x14)(y14)(st))(Curry.Module.Prelude.genEq(x15)(y15)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Prelude.T15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) st = Curry.Module.Prelude.T15(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))(f((5::Int))(x6)(st))(f((6::Int))(x7)(st))(f((7::Int))(x8)(st))(f((8::Int))(x9)(st))(f((9::Int))(x10)(st))(f((10::Int))(x11)(st))(f((11::Int))(x12)(st))(f((12::Int))(x13)(st))(f((13::Int))(x14)(st))(f((14::Int))(x15)(st))

  foldCurry f c (Curry.Module.Prelude.T15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(f(x6)(f(x7)(f(x8)(f(x9)(f(x10)(f(x11)(f(x12)(f(x13)(f(x14)(f(x15)(c)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)

  typeName _ = "(,,,,,,,,,,,,,,)"

  showQ d (Curry.Module.Prelude.T15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x6))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x7))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x8))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x9))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x10))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x11))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x12))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x13))((Prelude..)(Prelude.showChar(','))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x14))((Prelude..)(Prelude.showChar(','))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.zero)(x15))))))))))))))))))))))))))))))


  showQ _ (Curry.Module.Prelude.T15Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.Prelude.C_Bool where
  strEq Curry.Module.Prelude.C_False Curry.Module.Prelude.C_False st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.Prelude.C_True Curry.Module.Prelude.C_True st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.Prelude.C_False Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_True
  eq Curry.Module.Prelude.C_True Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_False
  propagate f Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_True

  foldCurry f c Curry.Module.Prelude.C_False st = c
  foldCurry f c Curry.Module.Prelude.C_True st = c

  typeName _ = "Bool"

  showQ _ Curry.Module.Prelude.C_False = Prelude.showString("Prelude.False")
  showQ _ Curry.Module.Prelude.C_True = Prelude.showString("Prelude.True")
  showQ _ (Curry.Module.Prelude.C_BoolOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.Prelude.C_Ordering where
  strEq Curry.Module.Prelude.C_LT Curry.Module.Prelude.C_LT st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.Prelude.C_EQ Curry.Module.Prelude.C_EQ st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.Prelude.C_GT Curry.Module.Prelude.C_GT st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.Prelude.C_LT Curry.Module.Prelude.C_LT st = Curry.Module.Prelude.C_True
  eq Curry.Module.Prelude.C_EQ Curry.Module.Prelude.C_EQ st = Curry.Module.Prelude.C_True
  eq Curry.Module.Prelude.C_GT Curry.Module.Prelude.C_GT st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.Prelude.C_LT st = Curry.Module.Prelude.C_LT
  propagate f Curry.Module.Prelude.C_EQ st = Curry.Module.Prelude.C_EQ
  propagate f Curry.Module.Prelude.C_GT st = Curry.Module.Prelude.C_GT

  foldCurry f c Curry.Module.Prelude.C_LT st = c
  foldCurry f c Curry.Module.Prelude.C_EQ st = c
  foldCurry f c Curry.Module.Prelude.C_GT st = c

  typeName _ = "Ordering"

  showQ _ Curry.Module.Prelude.C_LT = Prelude.showString("Prelude.LT")
  showQ _ Curry.Module.Prelude.C_EQ = Prelude.showString("Prelude.EQ")
  showQ _ Curry.Module.Prelude.C_GT = Prelude.showString("Prelude.GT")
  showQ _ (Curry.Module.Prelude.C_OrderingOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.Prelude.C_Nat where
  strEq Curry.Module.Prelude.C_IHi Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.strEqSuccess
  strEq (Curry.Module.Prelude.C_O x1) (Curry.Module.Prelude.C_O y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.Prelude.C_I x1) (Curry.Module.Prelude.C_I y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.Prelude.C_IHi Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.C_True
  eq (Curry.Module.Prelude.C_O x1) (Curry.Module.Prelude.C_O y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.Prelude.C_I x1) (Curry.Module.Prelude.C_I y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.C_IHi
  propagate f (Curry.Module.Prelude.C_O x1) st = Curry.Module.Prelude.C_O(f((0::Int))(x1)(st))
  propagate f (Curry.Module.Prelude.C_I x1) st = Curry.Module.Prelude.C_I(f((0::Int))(x1)(st))

  foldCurry f c Curry.Module.Prelude.C_IHi st = c
  foldCurry f c (Curry.Module.Prelude.C_O x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.Prelude.C_I x1) st = f(x1)(c)(st)

  typeName _ = "Nat"

  showQ  = Prelude.showsPrec





instance Curry Curry.Module.Prelude.C_Int where
  strEq (Curry.Module.Prelude.C_Neg x1) (Curry.Module.Prelude.C_Neg y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq Curry.Module.Prelude.C_Zero Curry.Module.Prelude.C_Zero st = Curry.Module.Prelude.strEqSuccess
  strEq (Curry.Module.Prelude.C_Pos x1) (Curry.Module.Prelude.C_Pos y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Prelude.C_Neg x1) (Curry.Module.Prelude.C_Neg y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq Curry.Module.Prelude.C_Zero Curry.Module.Prelude.C_Zero st = Curry.Module.Prelude.C_True
  eq (Curry.Module.Prelude.C_Pos x1) (Curry.Module.Prelude.C_Pos y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Prelude.C_Neg x1) st = Curry.Module.Prelude.C_Neg(f((0::Int))(x1)(st))
  propagate f Curry.Module.Prelude.C_Zero st = Curry.Module.Prelude.C_Zero
  propagate f (Curry.Module.Prelude.C_Pos x1) st = Curry.Module.Prelude.C_Pos(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.Prelude.C_Neg x1) st = f(x1)(c)(st)
  foldCurry f c Curry.Module.Prelude.C_Zero st = c
  foldCurry f c (Curry.Module.Prelude.C_Pos x1) st = f(x1)(c)(st)

  typeName _ = "Int"

  showQ  = Prelude.showsPrec





instance Curry Curry.Module.Prelude.C_Success where
  strEq Curry.Module.Prelude.C_Success Curry.Module.Prelude.C_Success st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.Prelude.C_Success Curry.Module.Prelude.C_Success st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.Prelude.C_Success st = Curry.Module.Prelude.C_Success

  foldCurry f c Curry.Module.Prelude.C_Success st = c

  typeName _ = "Success"

  showQ  = Prelude.showsPrec





instance (Curry t0) => Curry (Curry.Module.Prelude.C_Maybe t0) where
  strEq Curry.Module.Prelude.C_Nothing Curry.Module.Prelude.C_Nothing st = Curry.Module.Prelude.strEqSuccess
  strEq (Curry.Module.Prelude.C_Just x1) (Curry.Module.Prelude.C_Just y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.Prelude.C_Nothing Curry.Module.Prelude.C_Nothing st = Curry.Module.Prelude.C_True
  eq (Curry.Module.Prelude.C_Just x1) (Curry.Module.Prelude.C_Just y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.Prelude.C_Nothing st = Curry.Module.Prelude.C_Nothing
  propagate f (Curry.Module.Prelude.C_Just x1) st = Curry.Module.Prelude.C_Just(f((0::Int))(x1)(st))

  foldCurry f c Curry.Module.Prelude.C_Nothing st = c
  foldCurry f c (Curry.Module.Prelude.C_Just x1) st = f(x1)(c)(st)

  typeName _ = "Maybe"

  showQ _ Curry.Module.Prelude.C_Nothing = Prelude.showString("Prelude.Nothing")
  showQ d (Curry.Module.Prelude.C_Just x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Prelude.Just "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.Prelude.C_MaybeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0,Curry t1) => Curry (Curry.Module.Prelude.C_Either t0 t1) where
  strEq (Curry.Module.Prelude.C_Left x1) (Curry.Module.Prelude.C_Left y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.Prelude.C_Right x1) (Curry.Module.Prelude.C_Right y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Prelude.C_Left x1) (Curry.Module.Prelude.C_Left y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.Prelude.C_Right x1) (Curry.Module.Prelude.C_Right y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Prelude.C_Left x1) st = Curry.Module.Prelude.C_Left(f((0::Int))(x1)(st))
  propagate f (Curry.Module.Prelude.C_Right x1) st = Curry.Module.Prelude.C_Right(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.Prelude.C_Left x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.Prelude.C_Right x1) st = f(x1)(c)(st)

  typeName _ = "Either"

  showQ d (Curry.Module.Prelude.C_Left x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Prelude.Left "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.Prelude.C_Right x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Prelude.Right "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.Prelude.C_EitherOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0) => Curry (Curry.Module.Prelude.C_SearchTree t0) where
  strEq Curry.Module.Prelude.C_Fail Curry.Module.Prelude.C_Fail st = Curry.Module.Prelude.strEqSuccess
  strEq (Curry.Module.Prelude.C_Value x1) (Curry.Module.Prelude.C_Value y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.Prelude.C_Choice x1) (Curry.Module.Prelude.C_Choice y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq Curry.Module.Prelude.C_Suspend Curry.Module.Prelude.C_Suspend st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.Prelude.C_Fail Curry.Module.Prelude.C_Fail st = Curry.Module.Prelude.C_True
  eq (Curry.Module.Prelude.C_Value x1) (Curry.Module.Prelude.C_Value y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.Prelude.C_Choice x1) (Curry.Module.Prelude.C_Choice y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq Curry.Module.Prelude.C_Suspend Curry.Module.Prelude.C_Suspend st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.Prelude.C_Fail st = Curry.Module.Prelude.C_Fail
  propagate f (Curry.Module.Prelude.C_Value x1) st = Curry.Module.Prelude.C_Value(f((0::Int))(x1)(st))
  propagate f (Curry.Module.Prelude.C_Choice x1) st = Curry.Module.Prelude.C_Choice(f((0::Int))(x1)(st))
  propagate f Curry.Module.Prelude.C_Suspend st = Curry.Module.Prelude.C_Suspend

  foldCurry f c Curry.Module.Prelude.C_Fail st = c
  foldCurry f c (Curry.Module.Prelude.C_Value x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.Prelude.C_Choice x1) st = f(x1)(c)(st)
  foldCurry f c Curry.Module.Prelude.C_Suspend st = c

  typeName _ = "SearchTree"

  showQ _ Curry.Module.Prelude.C_Fail = Prelude.showString("Prelude.Fail")
  showQ d (Curry.Module.Prelude.C_Value x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Prelude.Value "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.Prelude.C_Choice x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Prelude.Choice "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ Curry.Module.Prelude.C_Suspend = Prelude.showString("Prelude.Suspend")
  showQ _ (Curry.Module.Prelude.C_SearchTreeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.Prelude.T0 where
  showsPrec _ Curry.Module.Prelude.T0 = Prelude.showString("()")
  showsPrec _ (Curry.Module.Prelude.T0Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0,Show t1) => Show (Curry.Module.Prelude.T2 t0 t1) where
  showsPrec d (Curry.Module.Prelude.T2 x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x2))))


  showsPrec _ (Curry.Module.Prelude.T2Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0,Show t1,Show t2) => Show (Curry.Module.Prelude.T3 t0 t1 t2) where
  showsPrec d (Curry.Module.Prelude.T3 x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x3))))))


  showsPrec _ (Curry.Module.Prelude.T3Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0,Show t1,Show t2,Show t3) => Show (Curry.Module.Prelude.T4 t0 t1 t2 t3) where
  showsPrec d (Curry.Module.Prelude.T4 x1 x2 x3 x4) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x4))))))))


  showsPrec _ (Curry.Module.Prelude.T4Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0,Show t1,Show t2,Show t3,Show t4) => Show (Curry.Module.Prelude.T5 t0 t1 t2 t3 t4) where
  showsPrec d (Curry.Module.Prelude.T5 x1 x2 x3 x4 x5) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x5))))))))))


  showsPrec _ (Curry.Module.Prelude.T5Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5) => Show (Curry.Module.Prelude.T6 t0 t1 t2 t3 t4 t5) where
  showsPrec d (Curry.Module.Prelude.T6 x1 x2 x3 x4 x5 x6) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x6))))))))))))


  showsPrec _ (Curry.Module.Prelude.T6Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5,Show t6) => Show (Curry.Module.Prelude.T7 t0 t1 t2 t3 t4 t5 t6) where
  showsPrec d (Curry.Module.Prelude.T7 x1 x2 x3 x4 x5 x6 x7) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x6))((Prelude..)(Prelude.showChar(','))(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x7))))))))))))))


  showsPrec _ (Curry.Module.Prelude.T7Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5,Show t6,Show t7) => Show (Curry.Module.Prelude.T8 t0 t1 t2 t3 t4 t5 t6 t7) where
  showsPrec d (Curry.Module.Prelude.T8 x1 x2 x3 x4 x5 x6 x7 x8) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x6))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x7))((Prelude..)(Prelude.showChar(','))(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x8))))))))))))))))


  showsPrec _ (Curry.Module.Prelude.T8Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5,Show t6,Show t7,Show t8) => Show (Curry.Module.Prelude.T9 t0 t1 t2 t3 t4 t5 t6 t7 t8) where
  showsPrec d (Curry.Module.Prelude.T9 x1 x2 x3 x4 x5 x6 x7 x8 x9) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x6))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x7))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x8))((Prelude..)(Prelude.showChar(','))(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x9))))))))))))))))))


  showsPrec _ (Curry.Module.Prelude.T9Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5,Show t6,Show t7,Show t8,Show t9) => Show (Curry.Module.Prelude.T10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9) where
  showsPrec d (Curry.Module.Prelude.T10 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x6))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x7))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x8))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x9))((Prelude..)(Prelude.showChar(','))(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x10))))))))))))))))))))


  showsPrec _ (Curry.Module.Prelude.T10Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5,Show t6,Show t7,Show t8,Show t9,Show t10) => Show (Curry.Module.Prelude.T11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10) where
  showsPrec d (Curry.Module.Prelude.T11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x6))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x7))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x8))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x9))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x10))((Prelude..)(Prelude.showChar(','))(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x11))))))))))))))))))))))


  showsPrec _ (Curry.Module.Prelude.T11Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5,Show t6,Show t7,Show t8,Show t9,Show t10,Show t11) => Show (Curry.Module.Prelude.T12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) where
  showsPrec d (Curry.Module.Prelude.T12 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x6))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x7))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x8))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x9))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x10))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x11))((Prelude..)(Prelude.showChar(','))(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x12))))))))))))))))))))))))


  showsPrec _ (Curry.Module.Prelude.T12Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5,Show t6,Show t7,Show t8,Show t9,Show t10,Show t11,Show t12) => Show (Curry.Module.Prelude.T13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12) where
  showsPrec d (Curry.Module.Prelude.T13 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x6))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x7))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x8))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x9))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x10))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x11))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x12))((Prelude..)(Prelude.showChar(','))(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x13))))))))))))))))))))))))))


  showsPrec _ (Curry.Module.Prelude.T13Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5,Show t6,Show t7,Show t8,Show t9,Show t10,Show t11,Show t12,Show t13) => Show (Curry.Module.Prelude.T14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13) where
  showsPrec d (Curry.Module.Prelude.T14 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x6))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x7))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x8))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x9))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x10))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x11))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x12))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x13))((Prelude..)(Prelude.showChar(','))(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x14))))))))))))))))))))))))))))


  showsPrec _ (Curry.Module.Prelude.T14Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0,Show t1,Show t2,Show t3,Show t4,Show t5,Show t6,Show t7,Show t8,Show t9,Show t10,Show t11,Show t12,Show t13,Show t14) => Show (Curry.Module.Prelude.T15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14) where
  showsPrec d (Curry.Module.Prelude.T15 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString([]))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x1))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x2))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x3))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x4))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x5))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x6))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x7))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x8))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x9))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x10))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x11))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x12))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x13))((Prelude..)(Prelude.showChar(','))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x14))((Prelude..)(Prelude.showChar(','))(Prelude.showsPrec(Curry.RunTimeSystem.zero)(x15))))))))))))))))))))))))))))))


  showsPrec _ (Curry.Module.Prelude.T15Or r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.Prelude.C_Bool where
  showsPrec _ Curry.Module.Prelude.C_False = Prelude.showString("False")
  showsPrec _ Curry.Module.Prelude.C_True = Prelude.showString("True")
  showsPrec _ (Curry.Module.Prelude.C_BoolOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.Prelude.C_Ordering where
  showsPrec _ Curry.Module.Prelude.C_LT = Prelude.showString("LT")
  showsPrec _ Curry.Module.Prelude.C_EQ = Prelude.showString("EQ")
  showsPrec _ Curry.Module.Prelude.C_GT = Prelude.showString("GT")
  showsPrec _ (Curry.Module.Prelude.C_OrderingOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.Prelude.C_Maybe t0) where
  showsPrec _ Curry.Module.Prelude.C_Nothing = Prelude.showString("Nothing")
  showsPrec d (Curry.Module.Prelude.C_Just x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Just "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.Prelude.C_MaybeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0,Show t1) => Show (Curry.Module.Prelude.C_Either t0 t1) where
  showsPrec d (Curry.Module.Prelude.C_Left x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Left "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.Prelude.C_Right x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Right "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.Prelude.C_EitherOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.Prelude.C_SearchTree t0) where
  showsPrec _ Curry.Module.Prelude.C_Fail = Prelude.showString("Fail")
  showsPrec d (Curry.Module.Prelude.C_Value x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Value "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.Prelude.C_Choice x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Choice "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ Curry.Module.Prelude.C_Suspend = Prelude.showString("Suspend")
  showsPrec _ (Curry.Module.Prelude.C_SearchTreeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Read Curry.Module.Prelude.T0 where
  readsPrec d r = Prelude.map(readTup)(Prelude.readsPrec(d)(r))
   where
    readTup ((,) () s) = (,)(Curry.Module.Prelude.T0)(s)







instance (Read t0,Read t1) => Read (Curry.Module.Prelude.T2 t0 t1) where
  readsPrec d r = Prelude.map(readTup)(Prelude.readsPrec(d)(r))
   where
    readTup ((,) ((,) x1 x2) s) = (,)(Curry.Module.Prelude.T2(x1)(x2))(s)







instance (Read t0,Read t1,Read t2) => Read (Curry.Module.Prelude.T3 t0 t1 t2) where
  readsPrec d r = Prelude.map(readTup)(Prelude.readsPrec(d)(r))
   where
    readTup ((,) ((,,) x1 x2 x3) s) = (,)(Curry.Module.Prelude.T3(x1)(x2)(x3))(s)







instance (Read t0,Read t1,Read t2,Read t3) => Read (Curry.Module.Prelude.T4 t0 t1 t2 t3) where
  readsPrec d r = Prelude.map(readTup)(Prelude.readsPrec(d)(r))
   where
    readTup ((,) ((,,,) x1 x2 x3 x4) s) = (,)(Curry.Module.Prelude.T4(x1)(x2)(x3)(x4))(s)







instance (Read t0,Read t1,Read t2,Read t3,Read t4) => Read (Curry.Module.Prelude.T5 t0 t1 t2 t3 t4) where
  readsPrec d r = Prelude.map(readTup)(Prelude.readsPrec(d)(r))
   where
    readTup ((,) ((,,,,) x1 x2 x3 x4 x5) s) = (,)(Curry.Module.Prelude.T5(x1)(x2)(x3)(x4)(x5))(s)







instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5) => Read (Curry.Module.Prelude.T6 t0 t1 t2 t3 t4 t5) where
  readsPrec d r = Prelude.map(readTup)(Prelude.readsPrec(d)(r))
   where
    readTup ((,) ((,,,,,) x1 x2 x3 x4 x5 x6) s) = (,)(Curry.Module.Prelude.T6(x1)(x2)(x3)(x4)(x5)(x6))(s)







instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5,Read t6) => Read (Curry.Module.Prelude.T7 t0 t1 t2 t3 t4 t5 t6) where
  readsPrec d r = Prelude.map(readTup)(Prelude.readsPrec(d)(r))
   where
    readTup ((,) ((,,,,,,) x1 x2 x3 x4 x5 x6 x7) s) = (,)(Curry.Module.Prelude.T7(x1)(x2)(x3)(x4)(x5)(x6)(x7))(s)







instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5,Read t6,Read t7) => Read (Curry.Module.Prelude.T8 t0 t1 t2 t3 t4 t5 t6 t7) where
  readsPrec d r = Prelude.map(readTup)(Prelude.readsPrec(d)(r))
   where
    readTup ((,) ((,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8) s) = (,)(Curry.Module.Prelude.T8(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8))(s)







instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5,Read t6,Read t7,Read t8) => Read (Curry.Module.Prelude.T9 t0 t1 t2 t3 t4 t5 t6 t7 t8) where
  readsPrec d r = Prelude.map(readTup)(Prelude.readsPrec(d)(r))
   where
    readTup ((,) ((,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9) s) = (,)(Curry.Module.Prelude.T9(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9))(s)







instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5,Read t6,Read t7,Read t8,Read t9) => Read (Curry.Module.Prelude.T10 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9) where
  readsPrec d r = Prelude.map(readTup)(Prelude.readsPrec(d)(r))
   where
    readTup ((,) ((,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10) s) = (,)(Curry.Module.Prelude.T10(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x10))(s)







instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5,Read t6,Read t7,Read t8,Read t9,Read t10) => Read (Curry.Module.Prelude.T11 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10) where
  readsPrec d r = Prelude.map(readTup)(Prelude.readsPrec(d)(r))
   where
    readTup ((,) ((,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) s) = (,)(Curry.Module.Prelude.T11(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x10)(x11))(s)







instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5,Read t6,Read t7,Read t8,Read t9,Read t10,Read t11) => Read (Curry.Module.Prelude.T12 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11) where
  readsPrec d r = Prelude.map(readTup)(Prelude.readsPrec(d)(r))
   where
    readTup ((,) ((,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) s) = (,)(Curry.Module.Prelude.T12(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x10)(x11)(x12))(s)







instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5,Read t6,Read t7,Read t8,Read t9,Read t10,Read t11,Read t12) => Read (Curry.Module.Prelude.T13 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12) where
  readsPrec d r = Prelude.map(readTup)(Prelude.readsPrec(d)(r))
   where
    readTup ((,) ((,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) s) = (,)(Curry.Module.Prelude.T13(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x10)(x11)(x12)(x13))(s)







instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5,Read t6,Read t7,Read t8,Read t9,Read t10,Read t11,Read t12,Read t13) => Read (Curry.Module.Prelude.T14 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13) where
  readsPrec d r = Prelude.map(readTup)(Prelude.readsPrec(d)(r))
   where
    readTup ((,) ((,,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) s) = (,)(Curry.Module.Prelude.T14(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x10)(x11)(x12)(x13)(x14))(s)







instance (Read t0,Read t1,Read t2,Read t3,Read t4,Read t5,Read t6,Read t7,Read t8,Read t9,Read t10,Read t11,Read t12,Read t13,Read t14) => Read (Curry.Module.Prelude.T15 t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14) where
  readsPrec d r = Prelude.map(readTup)(Prelude.readsPrec(d)(r))
   where
    readTup ((,) ((,,,,,,,,,,,,,,) x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15) s) = (,)(Curry.Module.Prelude.T15(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x10)(x11)(x12)(x13)(x14)(x15))(s)







instance Read Curry.Module.Prelude.C_Bool where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Prelude.C_False)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Prelude")("False")(r)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Prelude.C_True)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Prelude")("True")(r)])(r))





instance Read Curry.Module.Prelude.C_Ordering where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Prelude.C_LT)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Prelude")("LT")(r)])(r))((Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Prelude.C_EQ)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Prelude")("EQ")(r)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Prelude.C_GT)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Prelude")("GT")(r)])(r)))





instance (Read t0) => Read (Curry.Module.Prelude.C_Maybe t0) where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Prelude.C_Nothing)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Prelude")("Nothing")(r)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Prelude.C_Just(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Prelude")("Just")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))





instance (Read t0,Read t1) => Read (Curry.Module.Prelude.C_Either t0 t1) where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Prelude.C_Left(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Prelude")("Left")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Prelude.C_Right(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Prelude")("Right")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))





instance (Read t0) => Read (Curry.Module.Prelude.C_SearchTree t0) where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Prelude.C_Fail)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Prelude")("Fail")(r)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Prelude.C_Value(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Prelude")("Value")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Prelude.C_Choice(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Prelude")("Choice")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Prelude.C_Suspend)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Prelude")("Suspend")(r)])(r))))





op_46 :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1)) -> (Curry.Module.Prelude.Prim (t2 -> Curry.RunTimeSystem.State -> t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t2 -> Curry.RunTimeSystem.State -> t1)
op_46 x1 x2 st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_'46'46_'35lambda2(x1)(x2))



c_'46'46_'35lambda2 :: (Curry t9,Curry t11,Curry t7) => (Curry.Module.Prelude.Prim (t9 -> Curry.RunTimeSystem.State -> t11)) -> (Curry.Module.Prelude.Prim (t7 -> Curry.RunTimeSystem.State -> t9)) -> t7 -> Curry.RunTimeSystem.State -> t11
c_'46'46_'35lambda2 x1 x2 x3 st = Curry.Module.Prelude.c_apply(x1)(Curry.Module.Prelude.c_apply(x2)(x3)(st))(st)



c_id :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> t0
c_id x1 st = x1



c_const :: (Curry t0,Curry t1) => t0 -> t1 -> Curry.RunTimeSystem.State -> t0
c_const x1 x2 st = x1



c_curry :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 t0 t1) -> Curry.RunTimeSystem.State -> t2)) -> t0 -> t1 -> Curry.RunTimeSystem.State -> t2
c_curry x1 x2 x3 st = Curry.Module.Prelude.c_apply(x1)(Curry.Module.Prelude.T2(x2)(x3))(st)



c_uncurry :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t2))) -> (Curry.Module.Prelude.T2 t0 t1) -> Curry.RunTimeSystem.State -> t2
c_uncurry x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x3)(st))(x4)(st)
c_uncurry x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_uncurry(x1)(x)(st))(i)(xs)(st)
c_uncurry x1 x st = Curry.RunTimeSystem.patternFail("Prelude.uncurry")(x)



c_flip :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t2))) -> t1 -> t0 -> Curry.RunTimeSystem.State -> t2
c_flip x1 x2 x3 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x3)(st))(x2)(st)



c_until :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0)) -> t0 -> Curry.RunTimeSystem.State -> t0
c_until x1 x2 x3 st = Curry.Module.Prelude.c_until_case_59(x1)(x2)(x3)(Curry.Module.Prelude.c_apply(x1)(x3)(st))(st)



op_36 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1)) -> t0 -> Curry.RunTimeSystem.State -> t1
op_36 x1 x2 st = Curry.Module.Prelude.c_apply(x1)(x2)(st)



c_ensureSpine :: (Curry t0) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0)
c_ensureSpine st = Curry.Module.Prelude.pf(Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_ensureSpine'46ensureList'4621)))



c_ensureSpine'46ensureList'4621 :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_ensureSpine'46ensureList'4621 x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_ensureSpine'46ensureList'4621 x1@((Curry.Module.Prelude.:<) x2 x3) st = (Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_ensureSpine(st))(x3)(st))
c_ensureSpine'46ensureList'4621 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_ensureSpine'46ensureList'4621(x)(st))(i)(xs)(st)
c_ensureSpine'46ensureList'4621 x st = Curry.RunTimeSystem.patternFail("Prelude.ensureSpine.ensureList.21")(x)



c_seq :: (Curry t0,Curry t1) => t0 -> t1 -> Curry.RunTimeSystem.State -> t1
c_seq x1 x2 st = Curry.Module.Prelude.op_36_33(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_const(x2)))(x1)(st)



c_error :: (Curry t0) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> t0
c_error x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_prim_error))(x1)(st)



op_38_38 :: Curry.Module.Prelude.C_Bool -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_38_38 x1@Curry.Module.Prelude.C_True x2 st = x2
op_38_38 x1@Curry.Module.Prelude.C_False x2 st = Curry.Module.Prelude.C_False
op_38_38 (Curry.Module.Prelude.C_BoolOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.op_38_38(x)(x2)(st))(i)(xs)(st)
op_38_38 x x2 st = Curry.RunTimeSystem.patternFail("Prelude.&&")(x)



op_124_124 :: Curry.Module.Prelude.C_Bool -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_124_124 x1@Curry.Module.Prelude.C_True x2 st = Curry.Module.Prelude.C_True
op_124_124 x1@Curry.Module.Prelude.C_False x2 st = x2
op_124_124 (Curry.Module.Prelude.C_BoolOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.op_124_124(x)(x2)(st))(i)(xs)(st)
op_124_124 x x2 st = Curry.RunTimeSystem.patternFail("Prelude.||")(x)



c_not :: Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_not x1@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_False
c_not x1@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_True
c_not (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_not(x)(st))(i)(xs)(st)
c_not x st = Curry.RunTimeSystem.patternFail("Prelude.not")(x)



c_otherwise :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_otherwise st = Curry.Module.Prelude.C_True



c_if_then_else :: (Curry t0) => Curry.Module.Prelude.C_Bool -> t0 -> t0 -> Curry.RunTimeSystem.State -> t0
c_if_then_else x1@Curry.Module.Prelude.C_True x2 x3 st = x2
c_if_then_else x1@Curry.Module.Prelude.C_False x2 x3 st = x3
c_if_then_else (Curry.Module.Prelude.C_BoolOr i xs) x2 x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_if_then_else(x)(x2)(x3)(st))(i)(xs)(st)
c_if_then_else x x2 x3 st = Curry.RunTimeSystem.patternFail("Prelude.if_then_else")(x)



c_isLT :: Curry.Module.Prelude.C_Ordering -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isLT x1@Curry.Module.Prelude.C_LT st = Curry.Module.Prelude.C_True
c_isLT x1@Curry.Module.Prelude.C_GT st = Curry.Module.Prelude.C_False
c_isLT x1@Curry.Module.Prelude.C_EQ st = Curry.Module.Prelude.C_False
c_isLT (Curry.Module.Prelude.C_OrderingOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_isLT(x)(st))(i)(xs)(st)
c_isLT x st = Curry.RunTimeSystem.patternFail("Prelude.isLT")(x)



c_isGT :: Curry.Module.Prelude.C_Ordering -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isGT x1@Curry.Module.Prelude.C_LT st = Curry.Module.Prelude.C_False
c_isGT x1@Curry.Module.Prelude.C_GT st = Curry.Module.Prelude.C_True
c_isGT x1@Curry.Module.Prelude.C_EQ st = Curry.Module.Prelude.C_False
c_isGT (Curry.Module.Prelude.C_OrderingOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_isGT(x)(st))(i)(xs)(st)
c_isGT x st = Curry.RunTimeSystem.patternFail("Prelude.isGT")(x)



c_isEQ :: Curry.Module.Prelude.C_Ordering -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isEQ x1@Curry.Module.Prelude.C_LT st = Curry.Module.Prelude.C_False
c_isEQ x1@Curry.Module.Prelude.C_GT st = Curry.Module.Prelude.C_False
c_isEQ x1@Curry.Module.Prelude.C_EQ st = Curry.Module.Prelude.C_True
c_isEQ (Curry.Module.Prelude.C_OrderingOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_isEQ(x)(st))(i)(xs)(st)
c_isEQ x st = Curry.RunTimeSystem.patternFail("Prelude.isEQ")(x)



c_compare :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_compare x1@Curry.Module.Prelude.C_Zero x2 st = Curry.Module.Prelude.c_compare_case_58(x2)(st)
c_compare x1@(Curry.Module.Prelude.C_Pos x5) x2 st = Curry.Module.Prelude.c_compare_case_57(x5)(x2)(st)
c_compare x1@(Curry.Module.Prelude.C_Neg x8) x2 st = Curry.Module.Prelude.c_compare_case_56(x8)(x2)(st)
c_compare (Curry.Module.Prelude.C_IntOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_compare(x)(x2)(st))(i)(xs)(st)
c_compare x x2 st = Curry.RunTimeSystem.patternFail("Prelude.compare")(x)



op_60 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_60 x1 x2 st = Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_compare(x1)(x2)(st))(Curry.Module.Prelude.C_LT)(st)



op_62 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_62 x1 x2 st = Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_compare(x1)(x2)(st))(Curry.Module.Prelude.C_GT)(st)



op_60_61 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_60_61 x1 x2 st = Curry.Module.Prelude.op_47_61(Curry.Module.Prelude.c_compare(x1)(x2)(st))(Curry.Module.Prelude.C_GT)(st)



op_62_61 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_62_61 x1 x2 st = Curry.Module.Prelude.op_47_61(Curry.Module.Prelude.c_compare(x1)(x2)(st))(Curry.Module.Prelude.C_LT)(st)



c_max :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_max x1 x2 st = Curry.Module.Prelude.c_max_case_55(x1)(x2)(Curry.Module.Prelude.c_compare(x1)(x2)(st))(st)



c_min :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_min x1 x2 st = Curry.Module.Prelude.c_min_case_54(x1)(x2)(Curry.Module.Prelude.c_compare(x1)(x2)(st))(st)



op_47_61 :: (Curry t0) => t0 -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_47_61 x1 x2 st = Curry.Module.Prelude.c_not(Curry.Module.Prelude.op_61_61(x1)(x2)(st))(st)



c_fst :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T2 t0 t1) -> Curry.RunTimeSystem.State -> t0
c_fst x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_fst (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_fst(x)(st))(i)(xs)(st)
c_fst x st = Curry.RunTimeSystem.patternFail("Prelude.fst")(x)



c_snd :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T2 t0 t1) -> Curry.RunTimeSystem.State -> t1
c_snd x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_snd (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_snd(x)(st))(i)(xs)(st)
c_snd x st = Curry.RunTimeSystem.patternFail("Prelude.snd")(x)



c_head :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> t0
c_head x1@((Curry.Module.Prelude.:<) x2 x3) st = x2
c_head (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_head(x)(st))(i)(xs)(st)
c_head x st = Curry.RunTimeSystem.patternFail("Prelude.head")(x)



c_tail :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_tail x1@((Curry.Module.Prelude.:<) x2 x3) st = x3
c_tail (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_tail(x)(st))(i)(xs)(st)
c_tail x st = Curry.RunTimeSystem.patternFail("Prelude.tail")(x)



c_null :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_null x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_True
c_null x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.Prelude.C_False
c_null (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_null(x)(st))(i)(xs)(st)
c_null x st = Curry.RunTimeSystem.patternFail("Prelude.null")(x)



op_43_43 :: (Curry t0) => (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
op_43_43 x1@Curry.Module.Prelude.List x2 st = x2
op_43_43 x1@((Curry.Module.Prelude.:<) x3 x4) x2 st = (Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.op_43_43(x4)(x2)(st))
op_43_43 (Curry.Module.Prelude.ListOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.op_43_43(x)(x2)(st))(i)(xs)(st)
op_43_43 x x2 st = Curry.RunTimeSystem.patternFail("Prelude.++")(x)



c_length :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_length x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Zero
c_length x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.Prelude.op_43(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(Curry.Module.Prelude.c_length(x3)(st))(st)
c_length (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_length(x)(st))(i)(xs)(st)
c_length x st = Curry.RunTimeSystem.patternFail("Prelude.length")(x)



op_33_33 :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t0
op_33_33 x1@((Curry.Module.Prelude.:<) x3 x4) x2 st = Curry.Module.Prelude.c_'33'33_case_53(x2)(x3)(x4)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Zero)(st))(st)
op_33_33 (Curry.Module.Prelude.ListOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.op_33_33(x)(x2)(st))(i)(xs)(st)
op_33_33 x x2 st = Curry.RunTimeSystem.patternFail("Prelude.!!")(x)



c_map :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1)) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1
c_map x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_map x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(x1)(x3)(st))(Curry.Module.Prelude.c_map(x1)(x4)(st))
c_map x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_map(x1)(x)(st))(i)(xs)(st)
c_map x1 x st = Curry.RunTimeSystem.patternFail("Prelude.map")(x)



c_foldl :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t0))) -> t0 -> (Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> t0
c_foldl x1 x2 x3@Curry.Module.Prelude.List st = x2
c_foldl x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.Prelude.c_foldl(x1)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x2)(st))(x4)(st))(x5)(st)
c_foldl x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_foldl(x1)(x2)(x)(st))(i)(xs)(st)
c_foldl x1 x2 x st = Curry.RunTimeSystem.patternFail("Prelude.foldl")(x)



c_foldl1 :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0))) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> t0
c_foldl1 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.Prelude.c_foldl(x1)(x3)(x4)(st)
c_foldl1 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_foldl1(x1)(x)(st))(i)(xs)(st)
c_foldl1 x1 x st = Curry.RunTimeSystem.patternFail("Prelude.foldl1")(x)



c_foldr :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t1))) -> t1 -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> t1
c_foldr x1 x2 x3@Curry.Module.Prelude.List st = x2
c_foldr x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x4)(st))(Curry.Module.Prelude.c_foldr(x1)(x2)(x5)(st))(st)
c_foldr x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_foldr(x1)(x2)(x)(st))(i)(xs)(st)
c_foldr x1 x2 x st = Curry.RunTimeSystem.patternFail("Prelude.foldr")(x)



c_foldr1 :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0))) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> t0
c_foldr1 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.Prelude.c_foldr1_case_51(x1)(x3)(x4)(st)
c_foldr1 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_foldr1(x1)(x)(st))(i)(xs)(st)
c_foldr1 x1 x st = Curry.RunTimeSystem.patternFail("Prelude.foldr1")(x)



c_filter :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_filter x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_filter x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.Prelude.c_filter_case_50(x1)(x3)(x4)(Curry.Module.Prelude.c_apply(x1)(x3)(st))(st)
c_filter x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_filter(x1)(x)(st))(i)(xs)(st)
c_filter x1 x st = Curry.RunTimeSystem.patternFail("Prelude.filter")(x)



c_zip :: (Curry t0,Curry t1) => (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 t1)
c_zip x1@Curry.Module.Prelude.List x2 st = Curry.Module.Prelude.List
c_zip x1@((Curry.Module.Prelude.:<) x3 x4) x2 st = Curry.Module.Prelude.c_zip_case_49(x3)(x4)(x2)(st)
c_zip (Curry.Module.Prelude.ListOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_zip(x)(x2)(st))(i)(xs)(st)
c_zip x x2 st = Curry.RunTimeSystem.patternFail("Prelude.zip")(x)



c_zip3 :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t1) -> (Curry.Module.Prelude.List t2) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T3 t0 t1 t2)
c_zip3 x1@Curry.Module.Prelude.List x2 x3 st = Curry.Module.Prelude.List
c_zip3 x1@((Curry.Module.Prelude.:<) x4 x5) x2 x3 st = Curry.Module.Prelude.c_zip3_case_48(x3)(x4)(x5)(x2)(st)
c_zip3 (Curry.Module.Prelude.ListOr i xs) x2 x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_zip3(x)(x2)(x3)(st))(i)(xs)(st)
c_zip3 x x2 x3 st = Curry.RunTimeSystem.patternFail("Prelude.zip3")(x)



c_zipWith :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t2))) -> (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t2
c_zipWith x1 x2@Curry.Module.Prelude.List x3 st = Curry.Module.Prelude.List
c_zipWith x1 x2@((Curry.Module.Prelude.:<) x4 x5) x3 st = Curry.Module.Prelude.c_zipWith_case_46(x1)(x4)(x5)(x3)(st)
c_zipWith x1 (Curry.Module.Prelude.ListOr i xs) x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_zipWith(x1)(x)(x3)(st))(i)(xs)(st)
c_zipWith x1 x x3 st = Curry.RunTimeSystem.patternFail("Prelude.zipWith")(x)



c_zipWith3 :: (Curry t0,Curry t1,Curry t2,Curry t3) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t2 -> Curry.RunTimeSystem.State -> t3)))) -> (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t1) -> (Curry.Module.Prelude.List t2) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t3
c_zipWith3 x1 x2@Curry.Module.Prelude.List x3 x4 st = Curry.Module.Prelude.List
c_zipWith3 x1 x2@((Curry.Module.Prelude.:<) x5 x6) x3 x4 st = Curry.Module.Prelude.c_zipWith3_case_45(x1)(x4)(x5)(x6)(x3)(st)
c_zipWith3 x1 (Curry.Module.Prelude.ListOr i xs) x3 x4 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_zipWith3(x1)(x)(x3)(x4)(st))(i)(xs)(st)
c_zipWith3 x1 x x3 x4 st = Curry.RunTimeSystem.patternFail("Prelude.zipWith3")(x)



c_unzip :: (Curry t0,Curry t1) => (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t1)
c_unzip x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)
c_unzip x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.Prelude.c_unzip_case_43(x3)(x2)(st)
c_unzip (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_unzip(x)(st))(i)(xs)(st)
c_unzip x st = Curry.RunTimeSystem.patternFail("Prelude.unzip")(x)



c_unzip'46_'35selFP3'35xs :: (Curry t476,Curry t477) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t476) (Curry.Module.Prelude.List t477)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t476
c_unzip'46_'35selFP3'35xs x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_unzip'46_'35selFP3'35xs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_unzip'46_'35selFP3'35xs(x)(st))(i)(xs)(st)
c_unzip'46_'35selFP3'35xs x st = Curry.RunTimeSystem.patternFail("Prelude.unzip._#selFP3#xs")(x)



c_unzip'46_'35selFP4'35ys :: (Curry t476,Curry t477) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t476) (Curry.Module.Prelude.List t477)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t477
c_unzip'46_'35selFP4'35ys x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_unzip'46_'35selFP4'35ys (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_unzip'46_'35selFP4'35ys(x)(st))(i)(xs)(st)
c_unzip'46_'35selFP4'35ys x st = Curry.RunTimeSystem.patternFail("Prelude.unzip._#selFP4#ys")(x)



c_unzip3 :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.List (Curry.Module.Prelude.T3 t0 t1 t2)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t1) (Curry.Module.Prelude.List t2)
c_unzip3 x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.T3(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)
c_unzip3 x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.Prelude.c_unzip3_case_42(x3)(x2)(st)
c_unzip3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_unzip3(x)(st))(i)(xs)(st)
c_unzip3 x st = Curry.RunTimeSystem.patternFail("Prelude.unzip3")(x)



c_unzip3'46_'35selFP6'35xs :: (Curry t493,Curry t494,Curry t495) => (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List t493) (Curry.Module.Prelude.List t494) (Curry.Module.Prelude.List t495)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t493
c_unzip3'46_'35selFP6'35xs x1@(Curry.Module.Prelude.T3 x2 x3 x4) st = x2
c_unzip3'46_'35selFP6'35xs (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_unzip3'46_'35selFP6'35xs(x)(st))(i)(xs)(st)
c_unzip3'46_'35selFP6'35xs x st = Curry.RunTimeSystem.patternFail("Prelude.unzip3._#selFP6#xs")(x)



c_unzip3'46_'35selFP7'35ys :: (Curry t493,Curry t494,Curry t495) => (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List t493) (Curry.Module.Prelude.List t494) (Curry.Module.Prelude.List t495)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t494
c_unzip3'46_'35selFP7'35ys x1@(Curry.Module.Prelude.T3 x2 x3 x4) st = x3
c_unzip3'46_'35selFP7'35ys (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_unzip3'46_'35selFP7'35ys(x)(st))(i)(xs)(st)
c_unzip3'46_'35selFP7'35ys x st = Curry.RunTimeSystem.patternFail("Prelude.unzip3._#selFP7#ys")(x)



c_unzip3'46_'35selFP8'35zs :: (Curry t493,Curry t494,Curry t495) => (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List t493) (Curry.Module.Prelude.List t494) (Curry.Module.Prelude.List t495)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t495
c_unzip3'46_'35selFP8'35zs x1@(Curry.Module.Prelude.T3 x2 x3 x4) st = x4
c_unzip3'46_'35selFP8'35zs (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_unzip3'46_'35selFP8'35zs(x)(st))(i)(xs)(st)
c_unzip3'46_'35selFP8'35zs x st = Curry.RunTimeSystem.patternFail("Prelude.unzip3._#selFP8#zs")(x)



c_concat :: (Curry t0) => (Curry.Module.Prelude.List (Curry.Module.Prelude.List t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_concat x1 st = Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_43_43))(Curry.Module.Prelude.List)(x1)(st)



c_concatMap :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1)
c_concatMap x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_concat))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(x1)))(st)



c_iterate :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0)) -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_iterate x1 x2 st = (Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.c_iterate(x1)(Curry.Module.Prelude.c_apply(x1)(x2)(st))(st))



c_repeat :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_repeat x1 st = (Curry.Module.Prelude.:<)(x1)(Curry.Module.Prelude.c_repeat(x1)(st))



c_replicate :: (Curry t0) => Curry.Module.Prelude.C_Int -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_replicate x1 x2 st = Curry.Module.Prelude.c_take(x1)(Curry.Module.Prelude.c_repeat(x2)(st))(st)



c_take :: (Curry t0) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_take x1@(Curry.Module.Prelude.C_Neg x3) x2 st = Curry.Module.Prelude.List
c_take x1@Curry.Module.Prelude.C_Zero x2 st = Curry.Module.Prelude.List
c_take x1@(Curry.Module.Prelude.C_Pos x4) x2 st = Curry.Module.Prelude.c_take_case_41(x4)(x2)(st)
c_take (Curry.Module.Prelude.C_IntOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_take(x)(x2)(st))(i)(xs)(st)
c_take x x2 st = Curry.RunTimeSystem.patternFail("Prelude.take")(x)



c_drop :: (Curry t0) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_drop x1 x2 st = Curry.Module.Prelude.c_drop_case_40(x1)(x2)(Curry.Module.Prelude.op_60_61(x1)(Curry.Module.Prelude.C_Zero)(st))(st)



c_drop'46dropp'46272 :: (Curry t0) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_drop'46dropp'46272 x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_drop'46dropp'46272 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.Prelude.c_drop(Curry.Module.Prelude.op_45(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x4)(st)
c_drop'46dropp'46272 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_drop'46dropp'46272(x1)(x)(st))(i)(xs)(st)
c_drop'46dropp'46272 x1 x st = Curry.RunTimeSystem.patternFail("Prelude.drop.dropp.272")(x)



c_splitAt :: (Curry t0) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t0)
c_splitAt x1 x2 st = Curry.Module.Prelude.c_splitAt_case_39(x1)(x2)(Curry.Module.Prelude.op_60_61(x1)(Curry.Module.Prelude.C_Zero)(st))(st)



c_splitAt'46splitAtp'46282 :: (Curry t0) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t0)
c_splitAt'46splitAtp'46282 x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)
c_splitAt'46splitAtp'46282 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.Prelude.c_splitAt(Curry.Module.Prelude.op_45(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x4)(st)} in Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.c_splitAt'46splitAtp'46282'46_'35selFP10'35ys(x5)(st)))(Curry.Module.Prelude.c_splitAt'46splitAtp'46282'46_'35selFP11'35zs(x5)(st))
c_splitAt'46splitAtp'46282 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_splitAt'46splitAtp'46282(x1)(x)(st))(i)(xs)(st)
c_splitAt'46splitAtp'46282 x1 x st = Curry.RunTimeSystem.patternFail("Prelude.splitAt.splitAtp.282")(x)



c_splitAt'46splitAtp'46282'46_'35selFP10'35ys :: (Curry t576) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t576) (Curry.Module.Prelude.List t576)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t576
c_splitAt'46splitAtp'46282'46_'35selFP10'35ys x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_splitAt'46splitAtp'46282'46_'35selFP10'35ys (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_splitAt'46splitAtp'46282'46_'35selFP10'35ys(x)(st))(i)(xs)(st)
c_splitAt'46splitAtp'46282'46_'35selFP10'35ys x st = Curry.RunTimeSystem.patternFail("Prelude.splitAt.splitAtp.282._#selFP10#ys")(x)



c_splitAt'46splitAtp'46282'46_'35selFP11'35zs :: (Curry t576) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t576) (Curry.Module.Prelude.List t576)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t576
c_splitAt'46splitAtp'46282'46_'35selFP11'35zs x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_splitAt'46splitAtp'46282'46_'35selFP11'35zs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_splitAt'46splitAtp'46282'46_'35selFP11'35zs(x)(st))(i)(xs)(st)
c_splitAt'46splitAtp'46282'46_'35selFP11'35zs x st = Curry.RunTimeSystem.patternFail("Prelude.splitAt.splitAtp.282._#selFP11#zs")(x)



c_takeWhile :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_takeWhile x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_takeWhile x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.Prelude.c_takeWhile_case_38(x1)(x3)(x4)(Curry.Module.Prelude.c_apply(x1)(x3)(st))(st)
c_takeWhile x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_takeWhile(x1)(x)(st))(i)(xs)(st)
c_takeWhile x1 x st = Curry.RunTimeSystem.patternFail("Prelude.takeWhile")(x)



c_dropWhile :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_dropWhile x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_dropWhile x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.Prelude.c_dropWhile_case_37(x1)(x3)(x4)(Curry.Module.Prelude.c_apply(x1)(x3)(st))(st)
c_dropWhile x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_dropWhile(x1)(x)(st))(i)(xs)(st)
c_dropWhile x1 x st = Curry.RunTimeSystem.patternFail("Prelude.dropWhile")(x)



c_span :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t0)
c_span x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)
c_span x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.Prelude.c_span_case_36(x1)(x3)(x4)(Curry.Module.Prelude.c_apply(x1)(x3)(st))(st)
c_span x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_span(x1)(x)(st))(i)(xs)(st)
c_span x1 x st = Curry.RunTimeSystem.patternFail("Prelude.span")(x)



c_span'46_'35selFP13'35ys :: (Curry t627) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t627) (Curry.Module.Prelude.List t627)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t627
c_span'46_'35selFP13'35ys x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_span'46_'35selFP13'35ys (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_span'46_'35selFP13'35ys(x)(st))(i)(xs)(st)
c_span'46_'35selFP13'35ys x st = Curry.RunTimeSystem.patternFail("Prelude.span._#selFP13#ys")(x)



c_span'46_'35selFP14'35zs :: (Curry t627) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t627) (Curry.Module.Prelude.List t627)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t627
c_span'46_'35selFP14'35zs x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_span'46_'35selFP14'35zs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_span'46_'35selFP14'35zs(x)(st))(i)(xs)(st)
c_span'46_'35selFP14'35zs x st = Curry.RunTimeSystem.patternFail("Prelude.span._#selFP14#zs")(x)



c_break :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t0))
c_break x1 st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_span(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_not))(x1)(st)))



c_lines :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_lines x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_lines x1@((Curry.Module.Prelude.:<) x2 x3) st = let {x4 = Curry.Module.Prelude.c_lines'46splitline'46314((Curry.Module.Prelude.:<)(x2)(x3))(st)} in (Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_lines'46_'35selFP19'35l(x4)(st))(Curry.Module.Prelude.c_lines(Curry.Module.Prelude.c_lines'46_'35selFP20'35xs_l(x4)(st))(st))
c_lines (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_lines(x)(st))(i)(xs)(st)
c_lines x st = Curry.RunTimeSystem.patternFail("Prelude.lines")(x)



c_lines'46splitline'46314 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_lines'46splitline'46314 x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)
c_lines'46splitline'46314 x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.Prelude.c_lines'46splitline'46314_case_35(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\n'))(st))(st)
c_lines'46splitline'46314 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_lines'46splitline'46314(x)(st))(i)(xs)(st)
c_lines'46splitline'46314 x st = Curry.RunTimeSystem.patternFail("Prelude.lines.splitline.314")(x)



c_lines'46splitline'46314'46_'35selFP16'35ds :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_lines'46splitline'46314'46_'35selFP16'35ds x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_lines'46splitline'46314'46_'35selFP16'35ds (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_lines'46splitline'46314'46_'35selFP16'35ds(x)(st))(i)(xs)(st)
c_lines'46splitline'46314'46_'35selFP16'35ds x st = Curry.RunTimeSystem.patternFail("Prelude.lines.splitline.314._#selFP16#ds")(x)



c_lines'46splitline'46314'46_'35selFP17'35es :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_lines'46splitline'46314'46_'35selFP17'35es x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_lines'46splitline'46314'46_'35selFP17'35es (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_lines'46splitline'46314'46_'35selFP17'35es(x)(st))(i)(xs)(st)
c_lines'46splitline'46314'46_'35selFP17'35es x st = Curry.RunTimeSystem.patternFail("Prelude.lines.splitline.314._#selFP17#es")(x)



c_lines'46_'35selFP19'35l :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_lines'46_'35selFP19'35l x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_lines'46_'35selFP19'35l (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_lines'46_'35selFP19'35l(x)(st))(i)(xs)(st)
c_lines'46_'35selFP19'35l x st = Curry.RunTimeSystem.patternFail("Prelude.lines._#selFP19#l")(x)



c_lines'46_'35selFP20'35xs_l :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_lines'46_'35selFP20'35xs_l x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_lines'46_'35selFP20'35xs_l (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_lines'46_'35selFP20'35xs_l(x)(st))(i)(xs)(st)
c_lines'46_'35selFP20'35xs_l x st = Curry.RunTimeSystem.patternFail("Prelude.lines._#selFP20#xs_l")(x)



c_unlines :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_unlines x1 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_43_43))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))))(st))(x1)(st)



c_words :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_words x1 st = let {x2 = Curry.Module.Prelude.c_dropWhile(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_words'46isSpace'46326))(x1)(st)} in Curry.Module.Prelude.c_words_case_34(x2)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.List)(st))(st)



c_words'46isSpace'46326 :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_words'46isSpace'46326 x1 st = Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Char(' '))(st))(Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Char('\t'))(st))(Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Char('\n'))(st))(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Char('\r'))(st))(st))(st))(st)



c_words'46_'35selFP22'35w :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_words'46_'35selFP22'35w x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_words'46_'35selFP22'35w (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_words'46_'35selFP22'35w(x)(st))(i)(xs)(st)
c_words'46_'35selFP22'35w x st = Curry.RunTimeSystem.patternFail("Prelude.words._#selFP22#w")(x)



c_words'46_'35selFP23'35s2 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_words'46_'35selFP23'35s2 x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_words'46_'35selFP23'35s2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_words'46_'35selFP23'35s2(x)(st))(i)(xs)(st)
c_words'46_'35selFP23'35s2 x st = Curry.RunTimeSystem.patternFail("Prelude.words._#selFP23#s2")(x)



c_unwords :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_unwords x1 st = Curry.Module.Prelude.c_unwords_case_33(x1)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.List)(st))(st)



c_unwords'46_'35lambda6 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_unwords'46_'35lambda6 x1 x2 st = Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(x2))(st)



c_reverse :: (Curry t0) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0)
c_reverse st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_foldl(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pc))((Curry.Module.Prelude.:<)))))(Curry.Module.Prelude.List))



c_and :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Bool) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)
c_and st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_38_38))(Curry.Module.Prelude.C_True))



c_or :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Bool) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)
c_or st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_124_124))(Curry.Module.Prelude.C_False))



c_any :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)
c_any x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.c_or(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(x1)))(st)



c_all :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)
c_all x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.c_and(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(x1)))(st)



c_elem :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)
c_elem x1 st = Curry.Module.Prelude.c_any(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_61_61(x1)))(st)



c_notElem :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)
c_notElem x1 st = Curry.Module.Prelude.c_all(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_47_61(x1)))(st)



c_lookup :: (Curry t0,Curry t1) => t0 -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t1
c_lookup x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_lookup x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.Prelude.c_lookup_case_32(x1)(x4)(x3)(st)
c_lookup x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_lookup(x1)(x)(st))(i)(xs)(st)
c_lookup x1 x st = Curry.RunTimeSystem.patternFail("Prelude.lookup")(x)



c_enumFrom :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_enumFrom x1 st = (Curry.Module.Prelude.:<)(x1)(Curry.Module.Prelude.c_enumFrom(Curry.Module.Prelude.op_43(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))



c_enumFromThen :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_enumFromThen x1 x2 st = Curry.Module.Prelude.c_iterate(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_43(Curry.Module.Prelude.op_45(x2)(x1)(st))))(x1)(st)



c_enumFromTo :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_enumFromTo x1 x2 st = Curry.Module.Prelude.c_enumFromTo_case_30(x1)(x2)(Curry.Module.Prelude.op_62(x1)(x2)(st))(st)



c_enumFromThenTo :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_enumFromThenTo x1 x2 x3 st = Curry.Module.Prelude.c_takeWhile(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_enumFromThenTo'46p'46364(x3)(x1)(x2)))(Curry.Module.Prelude.c_enumFromThen(x1)(x2)(st))(st)



c_enumFromThenTo'46p'46364 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_enumFromThenTo'46p'46364 x1 x2 x3 x4 st = Curry.Module.Prelude.c_enumFromThenTo'46p'46364_case_29(x1)(x2)(x3)(x4)(Curry.Module.Prelude.op_62_61(x3)(x2)(st))(st)



c_ord :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ord x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_prim_ord))(x1)(st)



c_chr :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_chr x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_prim_chr))(x1)(st)



c_succ :: Curry.Module.Prelude.C_Nat -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Nat
c_succ x1@(Curry.Module.Prelude.C_O x2) st = Curry.Module.Prelude.C_I(x2)
c_succ x1@(Curry.Module.Prelude.C_I x3) st = Curry.Module.Prelude.C_O(Curry.Module.Prelude.c_succ(x3)(st))
c_succ x1@Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)
c_succ (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_succ(x)(st))(i)(xs)(st)
c_succ x st = Curry.RunTimeSystem.patternFail("Prelude.succ")(x)



op_43_94 :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Nat
op_43_94 x1@(Curry.Module.Prelude.C_O x3) x2 st = Curry.Module.Prelude.c_'43'94_case_28(x3)(x2)(st)
op_43_94 x1@(Curry.Module.Prelude.C_I x6) x2 st = Curry.Module.Prelude.c_'43'94_case_27(x6)(x2)(st)
op_43_94 x1@Curry.Module.Prelude.C_IHi x2 st = Curry.Module.Prelude.c_succ(x2)(st)
op_43_94 (Curry.Module.Prelude.C_NatOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.op_43_94(x)(x2)(st))(i)(xs)(st)
op_43_94 x x2 st = Curry.RunTimeSystem.patternFail("Prelude.+^")(x)



c_cmpNat :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_cmpNat x1@Curry.Module.Prelude.C_IHi x2 st = Curry.Module.Prelude.c_cmpNat_case_26(x2)(st)
c_cmpNat x1@(Curry.Module.Prelude.C_O x5) x2 st = Curry.Module.Prelude.c_cmpNat_case_25(x5)(x2)(st)
c_cmpNat x1@(Curry.Module.Prelude.C_I x8) x2 st = Curry.Module.Prelude.c_cmpNat_case_24(x8)(x2)(st)
c_cmpNat (Curry.Module.Prelude.C_NatOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_cmpNat(x)(x2)(st))(i)(xs)(st)
c_cmpNat x x2 st = Curry.RunTimeSystem.patternFail("Prelude.cmpNat")(x)



c_cmpNatLT :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_cmpNatLT x1@Curry.Module.Prelude.C_IHi x2 st = Curry.Module.Prelude.C_LT
c_cmpNatLT x1@(Curry.Module.Prelude.C_O x3) x2 st = Curry.Module.Prelude.c_cmpNatLT_case_23(x3)(x2)(st)
c_cmpNatLT x1@(Curry.Module.Prelude.C_I x6) x2 st = Curry.Module.Prelude.c_cmpNatLT_case_22(x6)(x2)(st)
c_cmpNatLT (Curry.Module.Prelude.C_NatOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_cmpNatLT(x)(x2)(st))(i)(xs)(st)
c_cmpNatLT x x2 st = Curry.RunTimeSystem.patternFail("Prelude.cmpNatLT")(x)



c_cmpNatGT :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_cmpNatGT x1 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.C_GT
c_cmpNatGT x1 x2@(Curry.Module.Prelude.C_O x3) st = Curry.Module.Prelude.c_cmpNatGT_case_21(x3)(x1)(st)
c_cmpNatGT x1 x2@(Curry.Module.Prelude.C_I x6) st = Curry.Module.Prelude.c_cmpNatGT_case_20(x6)(x1)(st)
c_cmpNatGT x1 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_cmpNatGT(x1)(x)(st))(i)(xs)(st)
c_cmpNatGT x1 x st = Curry.RunTimeSystem.patternFail("Prelude.cmpNatGT")(x)



op_60_94 :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_60_94 x1 x2 st = Curry.Module.Prelude.c_isLT(Curry.Module.Prelude.c_cmpNat(x1)(x2)(st))(st)



op_62_94 :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_62_94 x1 x2 st = Curry.Module.Prelude.c_isGT(Curry.Module.Prelude.c_cmpNat(x1)(x2)(st))(st)



op_60_61_94 :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_60_61_94 x1 x2 st = Curry.Module.Prelude.c_not(Curry.Module.Prelude.c_isGT(Curry.Module.Prelude.c_cmpNat(x1)(x2)(st))(st))(st)



op_62_61_94 :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_62_61_94 x1 x2 st = Curry.Module.Prelude.c_not(Curry.Module.Prelude.c_isLT(Curry.Module.Prelude.c_cmpNat(x1)(x2)(st))(st))(st)



op_42_94 :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Nat
op_42_94 x1@Curry.Module.Prelude.C_IHi x2 st = x2
op_42_94 x1@(Curry.Module.Prelude.C_I x3) x2 st = Curry.Module.Prelude.op_43_94(Curry.Module.Prelude.C_O(Curry.Module.Prelude.op_42_94(x2)(x3)(st)))(x2)(st)
op_42_94 x1@(Curry.Module.Prelude.C_O x4) x2 st = Curry.Module.Prelude.C_O(Curry.Module.Prelude.op_42_94(x4)(x2)(st))
op_42_94 (Curry.Module.Prelude.C_NatOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.op_42_94(x)(x2)(st))(i)(xs)(st)
op_42_94 x x2 st = Curry.RunTimeSystem.patternFail("Prelude.*^")(x)



c_pred :: Curry.Module.Prelude.C_Nat -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Nat
c_pred x1@(Curry.Module.Prelude.C_O x2) st = Curry.Module.Prelude.c_pred_case_19(x2)(st)
c_pred x1@(Curry.Module.Prelude.C_I x5) st = Curry.Module.Prelude.C_O(x5)
c_pred (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_pred(x)(st))(i)(xs)(st)
c_pred x st = Curry.RunTimeSystem.patternFail("Prelude.pred")(x)



c_inc :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_inc x1@Curry.Module.Prelude.C_Zero st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi)
c_inc x1@(Curry.Module.Prelude.C_Pos x2) st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.c_succ(x2)(st))
c_inc x1@(Curry.Module.Prelude.C_Neg x3) st = Curry.Module.Prelude.c_inc_case_18(x3)(st)
c_inc (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_inc(x)(st))(i)(xs)(st)
c_inc x st = Curry.RunTimeSystem.patternFail("Prelude.inc")(x)



c_dec :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_dec x1@Curry.Module.Prelude.C_Zero st = Curry.Module.Prelude.C_Neg(Curry.Module.Prelude.C_IHi)
c_dec x1@(Curry.Module.Prelude.C_Neg x2) st = Curry.Module.Prelude.C_Neg(Curry.Module.Prelude.c_succ(x2)(st))
c_dec x1@(Curry.Module.Prelude.C_Pos x3) st = Curry.Module.Prelude.c_dec_case_17(x3)(st)
c_dec (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_dec(x)(st))(i)(xs)(st)
c_dec x st = Curry.RunTimeSystem.patternFail("Prelude.dec")(x)



c_mult2 :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_mult2 x1@(Curry.Module.Prelude.C_Pos x2) st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(x2))
c_mult2 x1@Curry.Module.Prelude.C_Zero st = Curry.Module.Prelude.C_Zero
c_mult2 x1@(Curry.Module.Prelude.C_Neg x3) st = Curry.Module.Prelude.C_Neg(Curry.Module.Prelude.C_O(x3))
c_mult2 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_mult2(x)(st))(i)(xs)(st)
c_mult2 x st = Curry.RunTimeSystem.patternFail("Prelude.mult2")(x)



op_45_94 :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
op_45_94 x1@Curry.Module.Prelude.C_IHi x2 st = Curry.Module.Prelude.c_inc(Curry.Module.Prelude.C_Neg(x2))(st)
op_45_94 x1@(Curry.Module.Prelude.C_O x3) x2 st = Curry.Module.Prelude.c_'45'94_case_16(x3)(x2)(st)
op_45_94 x1@(Curry.Module.Prelude.C_I x6) x2 st = Curry.Module.Prelude.c_'45'94_case_15(x6)(x2)(st)
op_45_94 (Curry.Module.Prelude.C_NatOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.op_45_94(x)(x2)(st))(i)(xs)(st)
op_45_94 x x2 st = Curry.RunTimeSystem.patternFail("Prelude.-^")(x)



c_div2 :: Curry.Module.Prelude.C_Nat -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Nat
c_div2 x1@(Curry.Module.Prelude.C_O x2) st = x2
c_div2 x1@(Curry.Module.Prelude.C_I x3) st = x3
c_div2 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_div2(x)(st))(i)(xs)(st)
c_div2 x st = Curry.RunTimeSystem.patternFail("Prelude.div2")(x)



c_mod2 :: Curry.Module.Prelude.C_Nat -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_mod2 x1@Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi)
c_mod2 x1@(Curry.Module.Prelude.C_O x2) st = Curry.Module.Prelude.C_Zero
c_mod2 x1@(Curry.Module.Prelude.C_I x3) st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi)
c_mod2 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_mod2(x)(st))(i)(xs)(st)
c_mod2 x st = Curry.RunTimeSystem.patternFail("Prelude.mod2")(x)



c_divmodNat :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int
c_divmodNat x1 x2 st = Curry.Module.Prelude.c_divmodNat_case_14(x1)(x2)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_IHi)(st))(st)



c_divmodNat'46shift'46523 :: Curry.Module.Prelude.C_Nat -> Curry.Module.Prelude.C_Nat -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Nat
c_divmodNat'46shift'46523 x1@(Curry.Module.Prelude.C_O x3) x2 st = Curry.Module.Prelude.C_O(x2)
c_divmodNat'46shift'46523 x1@(Curry.Module.Prelude.C_I x4) x2 st = Curry.Module.Prelude.C_I(x2)
c_divmodNat'46shift'46523 (Curry.Module.Prelude.C_NatOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_divmodNat'46shift'46523(x)(x2)(st))(i)(xs)(st)
c_divmodNat'46shift'46523 x x2 st = Curry.RunTimeSystem.patternFail("Prelude.divmodNat.shift.523")(x)



op_43 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
op_43 x1@(Curry.Module.Prelude.C_Pos x3) x2 st = Curry.Module.Prelude.c_'43_case_7(x1)(x3)(x2)(st)
op_43 x1@(Curry.Module.Prelude.C_Neg x6) x2 st = Curry.Module.Prelude.c_'43_case_6(x1)(x6)(x2)(st)
op_43 x1@Curry.Module.Prelude.C_Zero x2 st = x2
op_43 (Curry.Module.Prelude.C_IntOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.op_43(x)(x2)(st))(i)(xs)(st)
op_43 x x2 st = Curry.RunTimeSystem.patternFail("Prelude.+")(x)



op_45 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
op_45 x1 x2@(Curry.Module.Prelude.C_Neg x3) st = Curry.Module.Prelude.op_43(x1)(Curry.Module.Prelude.C_Pos(x3))(st)
op_45 x1 x2@(Curry.Module.Prelude.C_Pos x4) st = Curry.Module.Prelude.op_43(x1)(Curry.Module.Prelude.C_Neg(x4))(st)
op_45 x1 x2@Curry.Module.Prelude.C_Zero st = x1
op_45 x1 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.op_45(x1)(x)(st))(i)(xs)(st)
op_45 x1 x st = Curry.RunTimeSystem.patternFail("Prelude.-")(x)



op_42 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
op_42 x1@(Curry.Module.Prelude.C_Pos x3) x2 st = Curry.Module.Prelude.c_'42_case_5(x3)(x2)(st)
op_42 x1@(Curry.Module.Prelude.C_Neg x6) x2 st = Curry.Module.Prelude.c_'42_case_4(x6)(x2)(st)
op_42 x1@Curry.Module.Prelude.C_Zero x2 st = Curry.Module.Prelude.C_Zero
op_42 (Curry.Module.Prelude.C_IntOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.op_42(x)(x2)(st))(i)(xs)(st)
op_42 x x2 st = Curry.RunTimeSystem.patternFail("Prelude.*")(x)



c_divmod :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int
c_divmod x1@Curry.Module.Prelude.C_Zero x2 st = Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.C_Zero)
c_divmod x1@(Curry.Module.Prelude.C_Pos x3) x2 st = Curry.Module.Prelude.c_divmod_case_3(x3)(x2)(st)
c_divmod x1@(Curry.Module.Prelude.C_Neg x9) x2 st = Curry.Module.Prelude.c_divmod_case_2(x9)(x2)(st)
c_divmod (Curry.Module.Prelude.C_IntOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_divmod(x)(x2)(st))(i)(xs)(st)
c_divmod x x2 st = Curry.RunTimeSystem.patternFail("Prelude.divmod")(x)



c_divmod'46_'35selFP25'35d :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_divmod'46_'35selFP25'35d x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_divmod'46_'35selFP25'35d (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_divmod'46_'35selFP25'35d(x)(st))(i)(xs)(st)
c_divmod'46_'35selFP25'35d x st = Curry.RunTimeSystem.patternFail("Prelude.divmod._#selFP25#d")(x)



c_divmod'46_'35selFP26'35m :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_divmod'46_'35selFP26'35m x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_divmod'46_'35selFP26'35m (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_divmod'46_'35selFP26'35m(x)(st))(i)(xs)(st)
c_divmod'46_'35selFP26'35m x st = Curry.RunTimeSystem.patternFail("Prelude.divmod._#selFP26#m")(x)



c_divmod'46_'35selFP28'35d :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_divmod'46_'35selFP28'35d x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_divmod'46_'35selFP28'35d (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_divmod'46_'35selFP28'35d(x)(st))(i)(xs)(st)
c_divmod'46_'35selFP28'35d x st = Curry.RunTimeSystem.patternFail("Prelude.divmod._#selFP28#d")(x)



c_divmod'46_'35selFP29'35m :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_divmod'46_'35selFP29'35m x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_divmod'46_'35selFP29'35m (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_divmod'46_'35selFP29'35m(x)(st))(i)(xs)(st)
c_divmod'46_'35selFP29'35m x st = Curry.RunTimeSystem.patternFail("Prelude.divmod._#selFP29#m")(x)



c_divmod'46_'35selFP31'35d :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_divmod'46_'35selFP31'35d x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_divmod'46_'35selFP31'35d (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_divmod'46_'35selFP31'35d(x)(st))(i)(xs)(st)
c_divmod'46_'35selFP31'35d x st = Curry.RunTimeSystem.patternFail("Prelude.divmod._#selFP31#d")(x)



c_divmod'46_'35selFP32'35m :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_divmod'46_'35selFP32'35m x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_divmod'46_'35selFP32'35m (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_divmod'46_'35selFP32'35m(x)(st))(i)(xs)(st)
c_divmod'46_'35selFP32'35m x st = Curry.RunTimeSystem.patternFail("Prelude.divmod._#selFP32#m")(x)



c_div :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_div x1 x2 st = Curry.Module.Prelude.c_fst(Curry.Module.Prelude.c_divmod(x1)(x2)(st))(st)



c_mod :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_mod x1 x2 st = Curry.Module.Prelude.c_snd(Curry.Module.Prelude.c_divmod(x1)(x2)(st))(st)



c_negate :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_negate x1@Curry.Module.Prelude.C_Zero st = Curry.Module.Prelude.C_Zero
c_negate x1@(Curry.Module.Prelude.C_Pos x2) st = Curry.Module.Prelude.C_Neg(x2)
c_negate x1@(Curry.Module.Prelude.C_Neg x3) st = Curry.Module.Prelude.C_Pos(x3)
c_negate (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_negate(x)(st))(i)(xs)(st)
c_negate x st = Curry.RunTimeSystem.patternFail("Prelude.negate")(x)



c_negateFloat :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_negateFloat x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_prim_negateFloat))(x1)(st)



c_success :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success
c_success st = Curry.Module.Prelude.C_Success



op_61_58_61 :: (Curry t0) => t0 -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success
op_61_58_61 x1 x2 st = Curry.Module.Prelude.c_'61'58'61_case_1(x1)(x2)(Curry.Module.Prelude.op_61_61_61(x1)(x2)(st))(st)



op_38_62 :: (Curry t0) => Curry.Module.Prelude.C_Success -> t0 -> Curry.RunTimeSystem.State -> t0
op_38_62 x1 x2 st = Curry.Module.Prelude.c_cond(x1)(x2)(st)



c_maybe :: (Curry t0,Curry t1) => t0 -> (Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t0)) -> (Curry.Module.Prelude.C_Maybe t1) -> Curry.RunTimeSystem.State -> t0
c_maybe x1 x2 x3@Curry.Module.Prelude.C_Nothing st = x1
c_maybe x1 x2 x3@(Curry.Module.Prelude.C_Just x4) st = Curry.Module.Prelude.c_apply(x2)(x4)(st)
c_maybe x1 x2 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_maybe(x1)(x2)(x)(st))(i)(xs)(st)
c_maybe x1 x2 x st = Curry.RunTimeSystem.patternFail("Prelude.maybe")(x)



c_either :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1)) -> (Curry.Module.Prelude.Prim (t2 -> Curry.RunTimeSystem.State -> t1)) -> (Curry.Module.Prelude.C_Either t0 t2) -> Curry.RunTimeSystem.State -> t1
c_either x1 x2 x3@(Curry.Module.Prelude.C_Left x4) st = Curry.Module.Prelude.c_apply(x1)(x4)(st)
c_either x1 x2 x3@(Curry.Module.Prelude.C_Right x5) st = Curry.Module.Prelude.c_apply(x2)(x5)(st)
c_either x1 x2 (Curry.Module.Prelude.C_EitherOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_either(x1)(x2)(x)(st))(i)(xs)(st)
c_either x1 x2 x st = Curry.RunTimeSystem.patternFail("Prelude.either")(x)



op_62_62 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.C_IO t0) -> (Curry.Module.Prelude.C_IO t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1
op_62_62 x1 x2 st = Curry.Module.Prelude.op_62_62_61(x1)(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_const(x2)))(st)



c_done :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_done st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.T0)(st)



c_putChar :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_putChar x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_prim_putChar))(x1)(st)



c_readFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_readFile x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_prim_readFile))(x1)(st)



c_writeFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_writeFile x1 x2 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.c_prim_writeFile))(x1)(st))(x2)(st)



c_appendFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_appendFile x1 x2 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.c_prim_appendFile))(x1)(st))(x2)(st)



c_putStr :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_putStr x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.c_done(st)
c_putStr x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_putChar(x2)(st))(Curry.Module.Prelude.c_putStr(x3)(st))(st)
c_putStr (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_putStr(x)(st))(i)(xs)(st)
c_putStr x st = Curry.RunTimeSystem.patternFail("Prelude.putStr")(x)



c_putStrLn :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_putStrLn x1 st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_putStr(x1)(st))(Curry.Module.Prelude.c_putChar(Curry.Module.Prelude.C_Char('\n'))(st))(st)



c_getLine :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_getLine st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_getChar(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_getLine'46_'35lambda10))(st)



c_getLine'46_'35lambda10 :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_getLine'46_'35lambda10 x1 st = Curry.Module.Prelude.c_getLine'46_'35lambda10_case_0(x1)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Char('\n'))(st))(st)



c_getLine'46_'35lambda10'46_'35lambda11 :: Curry.Module.Prelude.C_Char -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_getLine'46_'35lambda10'46_'35lambda11 x1 x2 st = Curry.Module.Prelude.c_return((Curry.Module.Prelude.:<)(x1)(x2))(st)



c_show :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_show x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_prim_show))(x1)(st)



c_print :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_print x1 st = Curry.Module.Prelude.c_putStrLn(Curry.Module.Prelude.c_show(x1)(st))(st)



c_doSolve :: Curry.Module.Prelude.C_Success -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_doSolve x1 st = Curry.Module.Prelude.c_cond(x1)(Curry.Module.Prelude.c_done(st))(st)



c_sequenceIO :: (Curry t0) => (Curry.Module.Prelude.List (Curry.Module.Prelude.C_IO t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List t0)
c_sequenceIO x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.List)(st)
c_sequenceIO x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.Prelude.op_62_62_61(x2)(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_sequenceIO'46_'35lambda12(x3)))(st)
c_sequenceIO (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_sequenceIO(x)(st))(i)(xs)(st)
c_sequenceIO x st = Curry.RunTimeSystem.patternFail("Prelude.sequenceIO")(x)



c_sequenceIO'46_'35lambda12 :: (Curry t940) => (Curry.Module.Prelude.List (Curry.Module.Prelude.C_IO t940)) -> t940 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List t940)
c_sequenceIO'46_'35lambda12 x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_sequenceIO(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_sequenceIO'46_'35lambda12'46_'35lambda13(x2)))(st)



c_sequenceIO'46_'35lambda12'46_'35lambda13 :: (Curry t940) => t940 -> (Curry.Module.Prelude.List t940) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List t940)
c_sequenceIO'46_'35lambda12'46_'35lambda13 x1 x2 st = Curry.Module.Prelude.c_return((Curry.Module.Prelude.:<)(x1)(x2))(st)



c_sequenceIO_ :: (Curry t0) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.C_IO t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0)
c_sequenceIO_ st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_62_62))(Curry.Module.Prelude.c_done(st)))



c_mapIO :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List t1))
c_mapIO x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_sequenceIO))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(x1)))(st)



c_mapIO_ :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0)
c_mapIO_ x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.c_sequenceIO_(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(x1)))(st)



op_63 :: (Curry t0) => t0 -> t0 -> Curry.RunTimeSystem.State -> t0
op_63 x1 x2 st = Curry.RunTimeSystem.orF(x1)(x2)



c_allValuesD :: (Curry t0) => (Curry.Module.Prelude.C_SearchTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_allValuesD x1@(Curry.Module.Prelude.C_Value x2) st = (Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)
c_allValuesD x1@Curry.Module.Prelude.C_Fail st = Curry.Module.Prelude.List
c_allValuesD x1@Curry.Module.Prelude.C_Suspend st = Curry.Module.Prelude.List
c_allValuesD x1@(Curry.Module.Prelude.C_Choice x3) st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_allValuesD))(st))(x3)(st)
c_allValuesD (Curry.Module.Prelude.C_SearchTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_allValuesD(x)(st))(i)(xs)(st)
c_allValuesD x st = Curry.RunTimeSystem.patternFail("Prelude.allValuesD")(x)



c_allValuesB :: (Curry t0) => (Curry.Module.Prelude.C_SearchTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_allValuesB x1 st = Curry.Module.Prelude.c_allValuesB'46unfoldOrs'46694((Curry.Module.Prelude.:<)(x1)(Curry.Module.Prelude.List))(st)



c_allValuesB'46partition'46694 :: (Curry t0) => (Curry.Module.Prelude.C_SearchTree t0) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t0))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t0))
c_allValuesB'46partition'46694 x1@(Curry.Module.Prelude.C_Value x3) x2 st = Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.c_allValuesB'46partition'46694'46_'35selFP34'35vs(x2)(st)))(Curry.Module.Prelude.c_allValuesB'46partition'46694'46_'35selFP35'35ors(x2)(st))
c_allValuesB'46partition'46694 x1@(Curry.Module.Prelude.C_Choice x7) x2 st = Curry.Module.Prelude.T2(Curry.Module.Prelude.c_allValuesB'46partition'46694'46_'35selFP37'35vs(x2)(st))(Curry.Module.Prelude.op_43_43(x7)(Curry.Module.Prelude.c_allValuesB'46partition'46694'46_'35selFP38'35ors(x2)(st))(st))
c_allValuesB'46partition'46694 x1@Curry.Module.Prelude.C_Fail x2 st = x2
c_allValuesB'46partition'46694 x1@Curry.Module.Prelude.C_Suspend x2 st = x2
c_allValuesB'46partition'46694 (Curry.Module.Prelude.C_SearchTreeOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_allValuesB'46partition'46694(x)(x2)(st))(i)(xs)(st)
c_allValuesB'46partition'46694 x x2 st = Curry.RunTimeSystem.patternFail("Prelude.allValuesB.partition.694")(x)



c_allValuesB'46partition'46694'46_'35selFP34'35vs :: (Curry t1004) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1004) (Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t1004))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1004
c_allValuesB'46partition'46694'46_'35selFP34'35vs x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_allValuesB'46partition'46694'46_'35selFP34'35vs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_allValuesB'46partition'46694'46_'35selFP34'35vs(x)(st))(i)(xs)(st)
c_allValuesB'46partition'46694'46_'35selFP34'35vs x st = Curry.RunTimeSystem.patternFail("Prelude.allValuesB.partition.694._#selFP34#vs")(x)



c_allValuesB'46partition'46694'46_'35selFP35'35ors :: (Curry t1004) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1004) (Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t1004))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t1004)
c_allValuesB'46partition'46694'46_'35selFP35'35ors x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_allValuesB'46partition'46694'46_'35selFP35'35ors (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_allValuesB'46partition'46694'46_'35selFP35'35ors(x)(st))(i)(xs)(st)
c_allValuesB'46partition'46694'46_'35selFP35'35ors x st = Curry.RunTimeSystem.patternFail("Prelude.allValuesB.partition.694._#selFP35#ors")(x)



c_allValuesB'46partition'46694'46_'35selFP37'35vs :: (Curry t1004) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1004) (Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t1004))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1004
c_allValuesB'46partition'46694'46_'35selFP37'35vs x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_allValuesB'46partition'46694'46_'35selFP37'35vs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_allValuesB'46partition'46694'46_'35selFP37'35vs(x)(st))(i)(xs)(st)
c_allValuesB'46partition'46694'46_'35selFP37'35vs x st = Curry.RunTimeSystem.patternFail("Prelude.allValuesB.partition.694._#selFP37#vs")(x)



c_allValuesB'46partition'46694'46_'35selFP38'35ors :: (Curry t1004) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1004) (Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t1004))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t1004)
c_allValuesB'46partition'46694'46_'35selFP38'35ors x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_allValuesB'46partition'46694'46_'35selFP38'35ors (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_allValuesB'46partition'46694'46_'35selFP38'35ors(x)(st))(i)(xs)(st)
c_allValuesB'46partition'46694'46_'35selFP38'35ors x st = Curry.RunTimeSystem.patternFail("Prelude.allValuesB.partition.694._#selFP38#ors")(x)



c_allValuesB'46unfoldOrs'46694 :: (Curry t0) => (Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_allValuesB'46unfoldOrs'46694 x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_allValuesB'46unfoldOrs'46694 x1@((Curry.Module.Prelude.:<) x2 x3) st = let {x4 = Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.c_allValuesB'46partition'46694))(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(x2)(x3))(st)} in Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_allValuesB'46unfoldOrs'46694'46_'35selFP40'35vals(x4)(st))(Curry.Module.Prelude.c_allValuesB'46unfoldOrs'46694(Curry.Module.Prelude.c_allValuesB'46unfoldOrs'46694'46_'35selFP41'35ors(x4)(st))(st))(st)
c_allValuesB'46unfoldOrs'46694 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_allValuesB'46unfoldOrs'46694(x)(st))(i)(xs)(st)
c_allValuesB'46unfoldOrs'46694 x st = Curry.RunTimeSystem.patternFail("Prelude.allValuesB.unfoldOrs.694")(x)



c_allValuesB'46unfoldOrs'46694'46_'35selFP40'35vals :: (Curry t1017) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1017) (Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t1017))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1017
c_allValuesB'46unfoldOrs'46694'46_'35selFP40'35vals x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_allValuesB'46unfoldOrs'46694'46_'35selFP40'35vals (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_allValuesB'46unfoldOrs'46694'46_'35selFP40'35vals(x)(st))(i)(xs)(st)
c_allValuesB'46unfoldOrs'46694'46_'35selFP40'35vals x st = Curry.RunTimeSystem.patternFail("Prelude.allValuesB.unfoldOrs.694._#selFP40#vals")(x)



c_allValuesB'46unfoldOrs'46694'46_'35selFP41'35ors :: (Curry t1017) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t1017) (Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t1017))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.C_SearchTree t1017)
c_allValuesB'46unfoldOrs'46694'46_'35selFP41'35ors x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_allValuesB'46unfoldOrs'46694'46_'35selFP41'35ors (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_allValuesB'46unfoldOrs'46694'46_'35selFP41'35ors(x)(st))(i)(xs)(st)
c_allValuesB'46unfoldOrs'46694'46_'35selFP41'35ors x st = Curry.RunTimeSystem.patternFail("Prelude.allValuesB.unfoldOrs.694._#selFP41#ors")(x)



c_inject :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success)) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success)
c_inject x1 x2 st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_inject'46_'35lambda14(x1)(x2))



c_inject'46_'35lambda14 :: (Curry t1028) => (Curry.Module.Prelude.Prim (t1028 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success)) -> (Curry.Module.Prelude.Prim (t1028 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success)) -> t1028 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success
c_inject'46_'35lambda14 x1 x2 x3 st = Curry.Module.Prelude.op_38(Curry.Module.Prelude.c_apply(x2)(x3)(st))(Curry.Module.Prelude.c_apply(x1)(x3)(st))(st)



c_PEVAL :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> t0
c_PEVAL x1 st = x1



c_unknown :: (Curry t0) => Curry.RunTimeSystem.State -> t0
c_unknown st = Curry.RunTimeSystem.freeF(\ x1 -> x1)



c_getLine'46_'35lambda10_case_0 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.List)(st)
c_getLine'46_'35lambda10_case_0 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_getLine(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_getLine'46_'35lambda10'46_'35lambda11(x1)))(st)
c_getLine'46_'35lambda10_case_0 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_getLine'46_'35lambda10_case_0(x1)(x)(st))(i)(xs)(st)
c_getLine'46_'35lambda10_case_0 x1 x st = Curry.RunTimeSystem.patternFail("Prelude.getLine._#lambda10_case_0")(x)



c_'61'58'61_case_1 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_Success
c_'61'58'61_case_1 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_'61'58'61_case_1(x1)(x2)(x)(st))(i)(xs)(st)
c_'61'58'61_case_1 x1 x2 x st = Curry.RunTimeSystem.patternFail("Prelude.=:=_case_1")(x)



c_divmod_case_2 x9 x2@Curry.Module.Prelude.C_Zero st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('0'))(Curry.Module.Prelude.List))))))))))))))(st)
c_divmod_case_2 x9 x2@(Curry.Module.Prelude.C_Pos x10) st = let {x11 = Curry.Module.Prelude.c_divmodNat(x9)(x10)(st)} in Curry.Module.Prelude.T2(Curry.Module.Prelude.c_negate(Curry.Module.Prelude.c_divmod'46_'35selFP28'35d(x11)(st))(st))(Curry.Module.Prelude.c_negate(Curry.Module.Prelude.c_divmod'46_'35selFP29'35m(x11)(st))(st))
c_divmod_case_2 x9 x2@(Curry.Module.Prelude.C_Neg x14) st = let {x15 = Curry.Module.Prelude.c_divmodNat(x9)(x14)(st)} in Curry.Module.Prelude.T2(Curry.Module.Prelude.c_divmod'46_'35selFP31'35d(x15)(st))(Curry.Module.Prelude.c_negate(Curry.Module.Prelude.c_divmod'46_'35selFP32'35m(x15)(st))(st))
c_divmod_case_2 x9 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_divmod_case_2(x9)(x)(st))(i)(xs)(st)
c_divmod_case_2 x9 x st = Curry.RunTimeSystem.patternFail("Prelude.divmod_case_2")(x)



c_divmod_case_3 x3 x2@Curry.Module.Prelude.C_Zero st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('0'))(Curry.Module.Prelude.List))))))))))))))(st)
c_divmod_case_3 x3 x2@(Curry.Module.Prelude.C_Pos x4) st = Curry.Module.Prelude.c_divmodNat(x3)(x4)(st)
c_divmod_case_3 x3 x2@(Curry.Module.Prelude.C_Neg x5) st = let {x6 = Curry.Module.Prelude.c_divmodNat(x3)(x5)(st)} in Curry.Module.Prelude.T2(Curry.Module.Prelude.c_negate(Curry.Module.Prelude.c_divmod'46_'35selFP25'35d(x6)(st))(st))(Curry.Module.Prelude.c_divmod'46_'35selFP26'35m(x6)(st))
c_divmod_case_3 x3 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_divmod_case_3(x3)(x)(st))(i)(xs)(st)
c_divmod_case_3 x3 x st = Curry.RunTimeSystem.patternFail("Prelude.divmod_case_3")(x)



c_'42_case_4 x6 x2@(Curry.Module.Prelude.C_Neg x7) st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.op_42_94(x6)(x7)(st))
c_'42_case_4 x6 x2@(Curry.Module.Prelude.C_Pos x8) st = Curry.Module.Prelude.C_Neg(Curry.Module.Prelude.op_42_94(x6)(x8)(st))
c_'42_case_4 x6 x2@Curry.Module.Prelude.C_Zero st = Curry.Module.Prelude.C_Zero
c_'42_case_4 x6 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_'42_case_4(x6)(x)(st))(i)(xs)(st)
c_'42_case_4 x6 x st = Curry.RunTimeSystem.patternFail("Prelude.*_case_4")(x)



c_'42_case_5 x3 x2@(Curry.Module.Prelude.C_Pos x4) st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.op_42_94(x3)(x4)(st))
c_'42_case_5 x3 x2@(Curry.Module.Prelude.C_Neg x5) st = Curry.Module.Prelude.C_Neg(Curry.Module.Prelude.op_42_94(x3)(x5)(st))
c_'42_case_5 x3 x2@Curry.Module.Prelude.C_Zero st = Curry.Module.Prelude.C_Zero
c_'42_case_5 x3 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_'42_case_5(x3)(x)(st))(i)(xs)(st)
c_'42_case_5 x3 x st = Curry.RunTimeSystem.patternFail("Prelude.*_case_5")(x)



c_'43_case_6 x1 x6 x2@(Curry.Module.Prelude.C_Neg x7) st = Curry.Module.Prelude.C_Neg(Curry.Module.Prelude.op_43_94(x6)(x7)(st))
c_'43_case_6 x1 x6 x2@(Curry.Module.Prelude.C_Pos x8) st = Curry.Module.Prelude.op_45_94(x8)(x6)(st)
c_'43_case_6 x1 x6 x2@Curry.Module.Prelude.C_Zero st = x1
c_'43_case_6 x1 x6 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_'43_case_6(x1)(x6)(x)(st))(i)(xs)(st)
c_'43_case_6 x1 x6 x st = Curry.RunTimeSystem.patternFail("Prelude.+_case_6")(x)



c_'43_case_7 x1 x3 x2@(Curry.Module.Prelude.C_Pos x4) st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.op_43_94(x3)(x4)(st))
c_'43_case_7 x1 x3 x2@(Curry.Module.Prelude.C_Neg x5) st = Curry.Module.Prelude.op_45_94(x3)(x5)(st)
c_'43_case_7 x1 x3 x2@Curry.Module.Prelude.C_Zero st = x1
c_'43_case_7 x1 x3 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_'43_case_7(x1)(x3)(x)(st))(i)(xs)(st)
c_'43_case_7 x1 x3 x st = Curry.RunTimeSystem.patternFail("Prelude.+_case_7")(x)



c_divmodNat_case_14 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Pos(x1))(Curry.Module.Prelude.C_Zero)
c_divmodNat_case_14 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_divmodNat_case_13(x1)(x2)(Curry.Module.Prelude.c_cmpNat(x1)(x2)(st))(st)
c_divmodNat_case_14 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_divmodNat_case_14(x1)(x2)(x)(st))(i)(xs)(st)
c_divmodNat_case_14 x1 x2 x st = Curry.RunTimeSystem.patternFail("Prelude.divmodNat_case_14")(x)



c_divmodNat_case_13 x1 x2 x3@Curry.Module.Prelude.C_EQ st = Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(Curry.Module.Prelude.C_Zero)
c_divmodNat_case_13 x1 x2 x3@Curry.Module.Prelude.C_LT st = Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.C_Pos(x1))
c_divmodNat_case_13 x1 x2 x3@Curry.Module.Prelude.C_GT st = Curry.Module.Prelude.c_divmodNat_case_12(x1)(x2)(Curry.Module.Prelude.c_divmodNat(Curry.Module.Prelude.c_div2(x1)(st))(x2)(st))(st)
c_divmodNat_case_13 x1 x2 (Curry.Module.Prelude.C_OrderingOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_divmodNat_case_13(x1)(x2)(x)(st))(i)(xs)(st)
c_divmodNat_case_13 x1 x2 x st = Curry.RunTimeSystem.patternFail("Prelude.divmodNat_case_13")(x)



c_divmodNat_case_12 x1 x2 (Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.Prelude.c_divmodNat_case_11(x1)(x2)(x4)(x3)(st)
c_divmodNat_case_12 x1 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_divmodNat_case_12(x1)(x2)(x)(st))(i)(xs)(st)
c_divmodNat_case_12 x1 x2 x st = Curry.RunTimeSystem.patternFail("Prelude.divmodNat_case_12")(x)



c_divmodNat_case_11 x1 x2 x4 x3@Curry.Module.Prelude.C_Zero st = Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(Curry.Module.Prelude.op_45_94(x1)(x2)(st))
c_divmodNat_case_11 x1 x2 x4 x3@(Curry.Module.Prelude.C_Pos x5) st = Curry.Module.Prelude.c_divmodNat_case_10(x1)(x2)(x5)(x4)(st)
c_divmodNat_case_11 x1 x2 x4 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_divmodNat_case_11(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c_divmodNat_case_11 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("Prelude.divmodNat_case_11")(x)



c_divmodNat_case_10 x1 x2 x5 x4@Curry.Module.Prelude.C_Zero st = Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(x5)))(Curry.Module.Prelude.c_mod2(x1)(st))
c_divmodNat_case_10 x1 x2 x5 x4@(Curry.Module.Prelude.C_Pos x6) st = Curry.Module.Prelude.c_divmodNat_case_9(x1)(x2)(x5)(x6)(Curry.Module.Prelude.c_divmodNat(Curry.Module.Prelude.c_divmodNat'46shift'46523(x1)(x6)(st))(x2)(st))(st)
c_divmodNat_case_10 x1 x2 x5 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_divmodNat_case_10(x1)(x2)(x5)(x)(st))(i)(xs)(st)
c_divmodNat_case_10 x1 x2 x5 x st = Curry.RunTimeSystem.patternFail("Prelude.divmodNat_case_10")(x)



c_divmodNat_case_9 x1 x2 x5 x6 (Curry.Module.Prelude.T2 x7 x8) st = Curry.Module.Prelude.c_divmodNat_case_8(x5)(x8)(x7)(st)
c_divmodNat_case_9 x1 x2 x5 x6 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_divmodNat_case_9(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c_divmodNat_case_9 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("Prelude.divmodNat_case_9")(x)



c_divmodNat_case_8 x5 x8 x7@Curry.Module.Prelude.C_Zero st = Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(x5)))(x8)
c_divmodNat_case_8 x5 x8 x7@(Curry.Module.Prelude.C_Pos x9) st = Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.op_43_94(Curry.Module.Prelude.C_O(x5))(x9)(st)))(x8)
c_divmodNat_case_8 x5 x8 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_divmodNat_case_8(x5)(x8)(x)(st))(i)(xs)(st)
c_divmodNat_case_8 x5 x8 x st = Curry.RunTimeSystem.patternFail("Prelude.divmodNat_case_8")(x)



c_'45'94_case_15 x6 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(x6))
c_'45'94_case_15 x6 x2@(Curry.Module.Prelude.C_O x7) st = Curry.Module.Prelude.c_inc(Curry.Module.Prelude.c_mult2(Curry.Module.Prelude.op_45_94(x6)(x7)(st))(st))(st)
c_'45'94_case_15 x6 x2@(Curry.Module.Prelude.C_I x8) st = Curry.Module.Prelude.c_mult2(Curry.Module.Prelude.op_45_94(x6)(x8)(st))(st)
c_'45'94_case_15 x6 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_'45'94_case_15(x6)(x)(st))(i)(xs)(st)
c_'45'94_case_15 x6 x st = Curry.RunTimeSystem.patternFail("Prelude.-^_case_15")(x)



c_'45'94_case_16 x3 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.c_pred(Curry.Module.Prelude.C_O(x3))(st))
c_'45'94_case_16 x3 x2@(Curry.Module.Prelude.C_O x4) st = Curry.Module.Prelude.c_mult2(Curry.Module.Prelude.op_45_94(x3)(x4)(st))(st)
c_'45'94_case_16 x3 x2@(Curry.Module.Prelude.C_I x5) st = Curry.Module.Prelude.c_dec(Curry.Module.Prelude.c_mult2(Curry.Module.Prelude.op_45_94(x3)(x5)(st))(st))(st)
c_'45'94_case_16 x3 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_'45'94_case_16(x3)(x)(st))(i)(xs)(st)
c_'45'94_case_16 x3 x st = Curry.RunTimeSystem.patternFail("Prelude.-^_case_16")(x)



c_dec_case_17 x3@Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.C_Zero
c_dec_case_17 x3@(Curry.Module.Prelude.C_O x4) st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.c_pred(Curry.Module.Prelude.C_O(x4))(st))
c_dec_case_17 x3@(Curry.Module.Prelude.C_I x5) st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(x5))
c_dec_case_17 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_dec_case_17(x)(st))(i)(xs)(st)
c_dec_case_17 x st = Curry.RunTimeSystem.patternFail("Prelude.dec_case_17")(x)



c_inc_case_18 x3@Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.C_Zero
c_inc_case_18 x3@(Curry.Module.Prelude.C_O x4) st = Curry.Module.Prelude.C_Neg(Curry.Module.Prelude.c_pred(Curry.Module.Prelude.C_O(x4))(st))
c_inc_case_18 x3@(Curry.Module.Prelude.C_I x5) st = Curry.Module.Prelude.C_Neg(Curry.Module.Prelude.C_O(x5))
c_inc_case_18 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_inc_case_18(x)(st))(i)(xs)(st)
c_inc_case_18 x st = Curry.RunTimeSystem.patternFail("Prelude.inc_case_18")(x)



c_pred_case_19 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.C_IHi
c_pred_case_19 x2@(Curry.Module.Prelude.C_O x3) st = Curry.Module.Prelude.C_I(Curry.Module.Prelude.c_pred(x2)(st))
c_pred_case_19 x2@(Curry.Module.Prelude.C_I x4) st = Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(x4))
c_pred_case_19 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_pred_case_19(x)(st))(i)(xs)(st)
c_pred_case_19 x st = Curry.RunTimeSystem.patternFail("Prelude.pred_case_19")(x)



c_cmpNatGT_case_20 x6 x1@Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.C_LT
c_cmpNatGT_case_20 x6 x1@(Curry.Module.Prelude.C_I x7) st = Curry.Module.Prelude.c_cmpNatGT(x7)(x6)(st)
c_cmpNatGT_case_20 x6 x1@(Curry.Module.Prelude.C_O x8) st = Curry.Module.Prelude.c_cmpNatLT(x8)(x6)(st)
c_cmpNatGT_case_20 x6 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_cmpNatGT_case_20(x6)(x)(st))(i)(xs)(st)
c_cmpNatGT_case_20 x6 x st = Curry.RunTimeSystem.patternFail("Prelude.cmpNatGT_case_20")(x)



c_cmpNatGT_case_21 x3 x1@Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.C_LT
c_cmpNatGT_case_21 x3 x1@(Curry.Module.Prelude.C_O x4) st = Curry.Module.Prelude.c_cmpNatGT(x4)(x3)(st)
c_cmpNatGT_case_21 x3 x1@(Curry.Module.Prelude.C_I x5) st = Curry.Module.Prelude.c_cmpNatGT(x5)(x3)(st)
c_cmpNatGT_case_21 x3 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_cmpNatGT_case_21(x3)(x)(st))(i)(xs)(st)
c_cmpNatGT_case_21 x3 x st = Curry.RunTimeSystem.patternFail("Prelude.cmpNatGT_case_21")(x)



c_cmpNatLT_case_22 x6 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.C_GT
c_cmpNatLT_case_22 x6 x2@(Curry.Module.Prelude.C_I x7) st = Curry.Module.Prelude.c_cmpNatLT(x6)(x7)(st)
c_cmpNatLT_case_22 x6 x2@(Curry.Module.Prelude.C_O x8) st = Curry.Module.Prelude.c_cmpNatGT(x6)(x8)(st)
c_cmpNatLT_case_22 x6 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_cmpNatLT_case_22(x6)(x)(st))(i)(xs)(st)
c_cmpNatLT_case_22 x6 x st = Curry.RunTimeSystem.patternFail("Prelude.cmpNatLT_case_22")(x)



c_cmpNatLT_case_23 x3 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.C_GT
c_cmpNatLT_case_23 x3 x2@(Curry.Module.Prelude.C_O x4) st = Curry.Module.Prelude.c_cmpNatLT(x3)(x4)(st)
c_cmpNatLT_case_23 x3 x2@(Curry.Module.Prelude.C_I x5) st = Curry.Module.Prelude.c_cmpNatLT(x3)(x5)(st)
c_cmpNatLT_case_23 x3 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_cmpNatLT_case_23(x3)(x)(st))(i)(xs)(st)
c_cmpNatLT_case_23 x3 x st = Curry.RunTimeSystem.patternFail("Prelude.cmpNatLT_case_23")(x)



c_cmpNat_case_24 x8 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.C_GT
c_cmpNat_case_24 x8 x2@(Curry.Module.Prelude.C_I x9) st = Curry.Module.Prelude.c_cmpNat(x8)(x9)(st)
c_cmpNat_case_24 x8 x2@(Curry.Module.Prelude.C_O x10) st = Curry.Module.Prelude.c_cmpNatGT(x8)(x10)(st)
c_cmpNat_case_24 x8 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_cmpNat_case_24(x8)(x)(st))(i)(xs)(st)
c_cmpNat_case_24 x8 x st = Curry.RunTimeSystem.patternFail("Prelude.cmpNat_case_24")(x)



c_cmpNat_case_25 x5 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.C_GT
c_cmpNat_case_25 x5 x2@(Curry.Module.Prelude.C_O x6) st = Curry.Module.Prelude.c_cmpNat(x5)(x6)(st)
c_cmpNat_case_25 x5 x2@(Curry.Module.Prelude.C_I x7) st = Curry.Module.Prelude.c_cmpNatLT(x5)(x7)(st)
c_cmpNat_case_25 x5 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_cmpNat_case_25(x5)(x)(st))(i)(xs)(st)
c_cmpNat_case_25 x5 x st = Curry.RunTimeSystem.patternFail("Prelude.cmpNat_case_25")(x)



c_cmpNat_case_26 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.C_EQ
c_cmpNat_case_26 x2@(Curry.Module.Prelude.C_O x3) st = Curry.Module.Prelude.C_LT
c_cmpNat_case_26 x2@(Curry.Module.Prelude.C_I x4) st = Curry.Module.Prelude.C_LT
c_cmpNat_case_26 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_cmpNat_case_26(x)(st))(i)(xs)(st)
c_cmpNat_case_26 x st = Curry.RunTimeSystem.patternFail("Prelude.cmpNat_case_26")(x)



c_'43'94_case_27 x6 x2@(Curry.Module.Prelude.C_O x7) st = Curry.Module.Prelude.C_I(Curry.Module.Prelude.op_43_94(x6)(x7)(st))
c_'43'94_case_27 x6 x2@(Curry.Module.Prelude.C_I x8) st = Curry.Module.Prelude.C_O(Curry.Module.Prelude.op_43_94(Curry.Module.Prelude.c_succ(x6)(st))(x8)(st))
c_'43'94_case_27 x6 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.C_O(Curry.Module.Prelude.c_succ(x6)(st))
c_'43'94_case_27 x6 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_'43'94_case_27(x6)(x)(st))(i)(xs)(st)
c_'43'94_case_27 x6 x st = Curry.RunTimeSystem.patternFail("Prelude.+^_case_27")(x)



c_'43'94_case_28 x3 x2@(Curry.Module.Prelude.C_O x4) st = Curry.Module.Prelude.C_O(Curry.Module.Prelude.op_43_94(x3)(x4)(st))
c_'43'94_case_28 x3 x2@(Curry.Module.Prelude.C_I x5) st = Curry.Module.Prelude.C_I(Curry.Module.Prelude.op_43_94(x3)(x5)(st))
c_'43'94_case_28 x3 x2@Curry.Module.Prelude.C_IHi st = Curry.Module.Prelude.C_I(x3)
c_'43'94_case_28 x3 (Curry.Module.Prelude.C_NatOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_'43'94_case_28(x3)(x)(st))(i)(xs)(st)
c_'43'94_case_28 x3 x st = Curry.RunTimeSystem.patternFail("Prelude.+^_case_28")(x)



c_enumFromThenTo'46p'46364_case_29 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_60_61(x4)(x1)(st)
c_enumFromThenTo'46p'46364_case_29 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_62_61(x4)(x1)(st)
c_enumFromThenTo'46p'46364_case_29 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_enumFromThenTo'46p'46364_case_29(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_enumFromThenTo'46p'46364_case_29 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("Prelude.enumFromThenTo.p.364_case_29")(x)



c_enumFromTo_case_30 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.List
c_enumFromTo_case_30 x1 x2 x3@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(x1)(Curry.Module.Prelude.c_enumFromTo(Curry.Module.Prelude.op_43(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x2)(st))
c_enumFromTo_case_30 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_enumFromTo_case_30(x1)(x2)(x)(st))(i)(xs)(st)
c_enumFromTo_case_30 x1 x2 x st = Curry.RunTimeSystem.patternFail("Prelude.enumFromTo_case_30")(x)



c_lookup_case_32 x1 x4 x3@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.Prelude.c_lookup_case_31(x1)(x4)(x5)(x6)(Curry.Module.Prelude.op_61_61(x1)(x5)(st))(st)
c_lookup_case_32 x1 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_lookup_case_32(x1)(x4)(x)(st))(i)(xs)(st)
c_lookup_case_32 x1 x4 x st = Curry.RunTimeSystem.patternFail("Prelude.lookup_case_32")(x)



c_lookup_case_31 x1 x4 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_Just(x6)
c_lookup_case_31 x1 x4 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_lookup(x1)(x4)(st)
c_lookup_case_31 x1 x4 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_lookup_case_31(x1)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_lookup_case_31 x1 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("Prelude.lookup_case_31")(x)



c_unwords_case_33 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.List
c_unwords_case_33 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_foldr1(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.c_unwords'46_'35lambda6))(x1)(st)
c_unwords_case_33 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_unwords_case_33(x1)(x)(st))(i)(xs)(st)
c_unwords_case_33 x1 x st = Curry.RunTimeSystem.patternFail("Prelude.unwords_case_33")(x)



c_words_case_34 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.List
c_words_case_34 x2 x3@Curry.Module.Prelude.C_False st = let {x3 = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_break(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_words'46isSpace'46326))(st))(x2)(st)} in (Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_words'46_'35selFP22'35w(x3)(st))(Curry.Module.Prelude.c_words(Curry.Module.Prelude.c_words'46_'35selFP23'35s2(x3)(st))(st))
c_words_case_34 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_words_case_34(x2)(x)(st))(i)(xs)(st)
c_words_case_34 x2 x st = Curry.RunTimeSystem.patternFail("Prelude.words_case_34")(x)



c_lines'46splitline'46314_case_35 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(x3)
c_lines'46splitline'46314_case_35 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x4 = Curry.Module.Prelude.c_lines'46splitline'46314(x3)(st)} in Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.c_lines'46splitline'46314'46_'35selFP16'35ds(x4)(st)))(Curry.Module.Prelude.c_lines'46splitline'46314'46_'35selFP17'35es(x4)(st))
c_lines'46splitline'46314_case_35 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_lines'46splitline'46314_case_35(x2)(x3)(x)(st))(i)(xs)(st)
c_lines'46splitline'46314_case_35 x2 x3 x st = Curry.RunTimeSystem.patternFail("Prelude.lines.splitline.314_case_35")(x)



c_span_case_36 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.Prelude.c_span(x1)(x4)(st)} in Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.c_span'46_'35selFP13'35ys(x5)(st)))(Curry.Module.Prelude.c_span'46_'35selFP14'35zs(x5)(st))
c_span_case_36 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)((Curry.Module.Prelude.:<)(x3)(x4))
c_span_case_36 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_span_case_36(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_span_case_36 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("Prelude.span_case_36")(x)



c_dropWhile_case_37 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_dropWhile(x1)(x4)(st)
c_dropWhile_case_37 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(x3)(x4)
c_dropWhile_case_37 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_dropWhile_case_37(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_dropWhile_case_37 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("Prelude.dropWhile_case_37")(x)



c_takeWhile_case_38 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.c_takeWhile(x1)(x4)(st))
c_takeWhile_case_38 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.List
c_takeWhile_case_38 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_takeWhile_case_38(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_takeWhile_case_38 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("Prelude.takeWhile_case_38")(x)



c_splitAt_case_39 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(x2)
c_splitAt_case_39 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_splitAt'46splitAtp'46282(x1)(x2)(st)
c_splitAt_case_39 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_splitAt_case_39(x1)(x2)(x)(st))(i)(xs)(st)
c_splitAt_case_39 x1 x2 x st = Curry.RunTimeSystem.patternFail("Prelude.splitAt_case_39")(x)



c_drop_case_40 x1 x2 x3@Curry.Module.Prelude.C_True st = x2
c_drop_case_40 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_drop'46dropp'46272(x1)(x2)(st)
c_drop_case_40 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_drop_case_40(x1)(x2)(x)(st))(i)(xs)(st)
c_drop_case_40 x1 x2 x st = Curry.RunTimeSystem.patternFail("Prelude.drop_case_40")(x)



c_take_case_41 x4 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_take_case_41 x4 x2@((Curry.Module.Prelude.:<) x5 x6) st = (Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.c_take(Curry.Module.Prelude.op_45(Curry.Module.Prelude.C_Pos(x4))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x6)(st))
c_take_case_41 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_take_case_41(x4)(x)(st))(i)(xs)(st)
c_take_case_41 x4 x st = Curry.RunTimeSystem.patternFail("Prelude.take_case_41")(x)



c_unzip3_case_42 x3 x2@(Curry.Module.Prelude.T3 x4 x5 x6) st = let {x7 = Curry.Module.Prelude.c_unzip3(x3)(st)} in Curry.Module.Prelude.T3((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.c_unzip3'46_'35selFP6'35xs(x7)(st)))((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.c_unzip3'46_'35selFP7'35ys(x7)(st)))((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.c_unzip3'46_'35selFP8'35zs(x7)(st)))
c_unzip3_case_42 x3 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_unzip3_case_42(x3)(x)(st))(i)(xs)(st)
c_unzip3_case_42 x3 x st = Curry.RunTimeSystem.patternFail("Prelude.unzip3_case_42")(x)



c_unzip_case_43 x3 x2@(Curry.Module.Prelude.T2 x4 x5) st = let {x6 = Curry.Module.Prelude.c_unzip(x3)(st)} in Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.c_unzip'46_'35selFP3'35xs(x6)(st)))((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.c_unzip'46_'35selFP4'35ys(x6)(st)))
c_unzip_case_43 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_unzip_case_43(x3)(x)(st))(i)(xs)(st)
c_unzip_case_43 x3 x st = Curry.RunTimeSystem.patternFail("Prelude.unzip_case_43")(x)



c_zipWith3_case_45 x1 x4 x5 x6 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_zipWith3_case_45 x1 x4 x5 x6 x3@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.Prelude.c_zipWith3_case_44(x1)(x5)(x6)(x7)(x8)(x4)(st)
c_zipWith3_case_45 x1 x4 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_zipWith3_case_45(x1)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_zipWith3_case_45 x1 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("Prelude.zipWith3_case_45")(x)



c_zipWith3_case_44 x1 x5 x6 x7 x8 x4@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_zipWith3_case_44 x1 x5 x6 x7 x8 x4@((Curry.Module.Prelude.:<) x9 x10) st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x5)(st))(x7)(st))(x9)(st))(Curry.Module.Prelude.c_zipWith3(x1)(x6)(x8)(x10)(st))
c_zipWith3_case_44 x1 x5 x6 x7 x8 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_zipWith3_case_44(x1)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c_zipWith3_case_44 x1 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("Prelude.zipWith3_case_44")(x)



c_zipWith_case_46 x1 x4 x5 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_zipWith_case_46 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x4)(st))(x6)(st))(Curry.Module.Prelude.c_zipWith(x1)(x5)(x7)(st))
c_zipWith_case_46 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_zipWith_case_46(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_zipWith_case_46 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("Prelude.zipWith_case_46")(x)



c_zip3_case_48 x3 x4 x5 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_zip3_case_48 x3 x4 x5 x2@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.Prelude.c_zip3_case_47(x4)(x5)(x6)(x7)(x3)(st)
c_zip3_case_48 x3 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_zip3_case_48(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c_zip3_case_48 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("Prelude.zip3_case_48")(x)



c_zip3_case_47 x4 x5 x6 x7 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_zip3_case_47 x4 x5 x6 x7 x3@((Curry.Module.Prelude.:<) x8 x9) st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.T3(x4)(x6)(x8))(Curry.Module.Prelude.c_zip3(x5)(x7)(x9)(st))
c_zip3_case_47 x4 x5 x6 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_zip3_case_47(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c_zip3_case_47 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("Prelude.zip3_case_47")(x)



c_zip_case_49 x3 x4 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_zip_case_49 x3 x4 x2@((Curry.Module.Prelude.:<) x5 x6) st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(x3)(x5))(Curry.Module.Prelude.c_zip(x4)(x6)(st))
c_zip_case_49 x3 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_zip_case_49(x3)(x4)(x)(st))(i)(xs)(st)
c_zip_case_49 x3 x4 x st = Curry.RunTimeSystem.patternFail("Prelude.zip_case_49")(x)



c_filter_case_50 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.c_filter(x1)(x4)(st))
c_filter_case_50 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_filter(x1)(x4)(st)
c_filter_case_50 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_filter_case_50(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_filter_case_50 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("Prelude.filter_case_50")(x)



c_foldr1_case_51 x1 x3 x4@Curry.Module.Prelude.List st = x3
c_foldr1_case_51 x1 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x3)(st))(Curry.Module.Prelude.c_foldr1(x1)((Curry.Module.Prelude.:<)(x5)(x6))(st))(st)
c_foldr1_case_51 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_foldr1_case_51(x1)(x3)(x)(st))(i)(xs)(st)
c_foldr1_case_51 x1 x3 x st = Curry.RunTimeSystem.patternFail("Prelude.foldr1_case_51")(x)



c_'33'33_case_53 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = x3
c_'33'33_case_53 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_'33'33_case_52(x2)(x4)(Curry.Module.Prelude.op_62(x2)(Curry.Module.Prelude.C_Zero)(st))(st)
c_'33'33_case_53 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_'33'33_case_53(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_'33'33_case_53 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("Prelude.!!_case_53")(x)



c_'33'33_case_52 x2 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_33_33(x4)(Curry.Module.Prelude.op_45(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st)
c_'33'33_case_52 x2 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_'33'33_case_52(x2)(x4)(x)(st))(i)(xs)(st)
c_'33'33_case_52 x2 x4 x st = Curry.RunTimeSystem.patternFail("Prelude.!!_case_52")(x)



c_min_case_54 x1 x2 x3@Curry.Module.Prelude.C_GT st = x2
c_min_case_54 x1 x2 x3@Curry.Module.Prelude.C_LT st = x1
c_min_case_54 x1 x2 x3@Curry.Module.Prelude.C_EQ st = x1
c_min_case_54 x1 x2 (Curry.Module.Prelude.C_OrderingOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_min_case_54(x1)(x2)(x)(st))(i)(xs)(st)
c_min_case_54 x1 x2 x st = Curry.RunTimeSystem.patternFail("Prelude.min_case_54")(x)



c_max_case_55 x1 x2 x3@Curry.Module.Prelude.C_LT st = x2
c_max_case_55 x1 x2 x3@Curry.Module.Prelude.C_EQ st = x1
c_max_case_55 x1 x2 x3@Curry.Module.Prelude.C_GT st = x1
c_max_case_55 x1 x2 (Curry.Module.Prelude.C_OrderingOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_max_case_55(x1)(x2)(x)(st))(i)(xs)(st)
c_max_case_55 x1 x2 x st = Curry.RunTimeSystem.patternFail("Prelude.max_case_55")(x)



c_compare_case_56 x8 x2@Curry.Module.Prelude.C_Zero st = Curry.Module.Prelude.C_LT
c_compare_case_56 x8 x2@(Curry.Module.Prelude.C_Pos x9) st = Curry.Module.Prelude.C_LT
c_compare_case_56 x8 x2@(Curry.Module.Prelude.C_Neg x10) st = Curry.Module.Prelude.c_cmpNat(x10)(x8)(st)
c_compare_case_56 x8 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_compare_case_56(x8)(x)(st))(i)(xs)(st)
c_compare_case_56 x8 x st = Curry.RunTimeSystem.patternFail("Prelude.compare_case_56")(x)



c_compare_case_57 x5 x2@Curry.Module.Prelude.C_Zero st = Curry.Module.Prelude.C_GT
c_compare_case_57 x5 x2@(Curry.Module.Prelude.C_Pos x6) st = Curry.Module.Prelude.c_cmpNat(x5)(x6)(st)
c_compare_case_57 x5 x2@(Curry.Module.Prelude.C_Neg x7) st = Curry.Module.Prelude.C_GT
c_compare_case_57 x5 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_compare_case_57(x5)(x)(st))(i)(xs)(st)
c_compare_case_57 x5 x st = Curry.RunTimeSystem.patternFail("Prelude.compare_case_57")(x)



c_compare_case_58 x2@Curry.Module.Prelude.C_Zero st = Curry.Module.Prelude.C_EQ
c_compare_case_58 x2@(Curry.Module.Prelude.C_Pos x3) st = Curry.Module.Prelude.C_LT
c_compare_case_58 x2@(Curry.Module.Prelude.C_Neg x4) st = Curry.Module.Prelude.C_GT
c_compare_case_58 (Curry.Module.Prelude.C_IntOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_compare_case_58(x)(st))(i)(xs)(st)
c_compare_case_58 x st = Curry.RunTimeSystem.patternFail("Prelude.compare_case_58")(x)



c_until_case_59 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = x3
c_until_case_59 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_until(x1)(x2)(Curry.Module.Prelude.c_apply(x2)(x3)(st))(st)
c_until_case_59 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Prelude.c_until_case_59(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_until_case_59 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("Prelude.until_case_59")(x)



op_36_33 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1)) -> t0 -> Curry.RunTimeSystem.State -> t1
op_36_33 x1 x2 st = (Curry.Module.Prelude.$!)(x1)(x2)(st)



op_36_33_33 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1)) -> t0 -> Curry.RunTimeSystem.State -> t1
op_36_33_33 x1 x2 st = (Curry.Module.Prelude.$!!)(x1)(x2)(st)



op_36_35 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1)) -> t0 -> Curry.RunTimeSystem.State -> t1
op_36_35 x1 x2 st = (Curry.Module.Prelude.$#)(x1)(x2)(st)



op_36_35_35 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1)) -> t0 -> Curry.RunTimeSystem.State -> t1
op_36_35_35 x1 x2 st = (Curry.Module.Prelude.$##)(x1)(x2)(st)



c_prim_error :: (Curry t0) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> t0
c_prim_error x1 st = Curry.Module.Prelude.prim_error(x1)(st)



c_failed :: (Curry t0) => Curry.RunTimeSystem.State -> t0
c_failed st = Curry.Module.Prelude.failed(st)



op_61_61 :: (Curry t0) => t0 -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_61_61 x1 x2 st = (Curry.Module.Prelude.==)(x1)(x2)(st)



c_prim_ord :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_prim_ord x1 st = Curry.Module.Prelude.prim_ord(x1)(st)



c_prim_chr :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_prim_chr x1 st = Curry.Module.Prelude.prim_chr(x1)(st)



c_prim_negateFloat :: Curry.Module.Prelude.C_Float -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Float
c_prim_negateFloat x1 st = Curry.Module.Prelude.prim_negateFloat(x1)(st)



op_61_61_61 :: (Curry t0) => t0 -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
op_61_61_61 x1 x2 st = (Curry.Module.Prelude.===)(x1)(x2)(st)



op_38 :: Curry.Module.Prelude.C_Success -> Curry.Module.Prelude.C_Success -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success
op_38 x1 x2 st = (Curry.Module.Prelude.&)(x1)(x2)(st)



op_62_62_61 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.C_IO t0) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1
op_62_62_61 x1 x2 st = (Curry.Module.Prelude.>>=)(x1)(x2)(st)



c_return :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_return x1 st = Curry.Module.Prelude.return(x1)(st)



c_prim_putChar :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_prim_putChar x1 st = Curry.Module.Prelude.prim_putChar(x1)(st)



c_getChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Char
c_getChar st = Curry.Module.Prelude.getChar(st)



c_prim_readFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_prim_readFile x1 st = Curry.Module.Prelude.prim_readFile(x1)(st)



c_prim_writeFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_prim_writeFile x1 x2 st = Curry.Module.Prelude.prim_writeFile(x1)(x2)(st)



c_prim_appendFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_prim_appendFile x1 x2 st = Curry.Module.Prelude.prim_appendFile(x1)(x2)(st)



c_catchFail :: (Curry t0) => (Curry.Module.Prelude.C_IO t0) -> (Curry.Module.Prelude.C_IO t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_catchFail x1 x2 st = Curry.Module.Prelude.catchFail(x1)(x2)(st)



c_prim_show :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_prim_show x1 st = Curry.Module.Prelude.prim_show(x1)(st)



c_getSearchTree :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_SearchTree t0)
c_getSearchTree x1 st = Curry.Module.Prelude.getSearchTree(x1)(st)



c_apply :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1)) -> t0 -> Curry.RunTimeSystem.State -> t1
c_apply x1 x2 st = Curry.Module.Prelude.apply(x1)(x2)(st)



c_cond :: (Curry t0) => Curry.Module.Prelude.C_Success -> t0 -> Curry.RunTimeSystem.State -> t0
c_cond x1 x2 st = Curry.Module.Prelude.cond(x1)(x2)(st)



op_61_58_60_61 :: (Curry t0) => t0 -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Success
op_61_58_60_61 x1 x2 st = (Curry.Module.Prelude.=:<=)(x1)(x2)(st)


