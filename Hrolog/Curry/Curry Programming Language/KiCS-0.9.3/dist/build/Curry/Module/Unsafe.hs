{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Unsafe (module Curry.Module.Unsafe) where

import Curry.RunTimeSystem
import Curry.Module.Char
import Curry.Module.Meta
import Curry.Module.Prelude



-- begin included



import System.IO.Unsafe 
import Data.IORef

prim_isVar :: Curry a => a -> Result C_Bool
prim_isVar x _ = case consKind x of
  Branching -> toCurry (isGenerator (orRef x))
  _         -> C_False

prim_unsafePerformIO :: Curry a => C_IO a -> Result a
prim_unsafePerformIO (C_IO action) st = unsafe (unsafePerformIO (action st)) st
 

unsafe :: Curry a => IOVal a -> Result a
unsafe (IOVal v)        _  = v
unsafe (IOValFail e)    _  = Curry.RunTimeSystem.failed e
unsafe (IOValOr r bs)   st = mapOr (\ x -> unsafe (unsafePerformIO x)) r bs st
{-
-- the main point about unsafe:  the value of suspensions has to be stored.
unsafe (IOValSusp _ susp) = let sRef = nextSuspRef () in
  suspend False  (saveSuspValue sRef (const (tra sRef "cont" unsafe)) 
                                (\st -> tra sRef "" (unsafePerformIO (susp (Just st)))))

-- Sometimes we have to make sure that suspensions are not evaluated twice.
-- Most notably this is true for suspended unsafely performed io actions .
-- This function saves its result in the store and retrieves it whenever 
-- called. Only when there has been no value stored yet, the given suspension 
-- is called with the store.
-- Saving suspensions values this way is a bit complicated.
-- First, look if suspension already lifted from somewhere else. 
--   if so, the result has already been stored
--   if not, test if suspension can be lifted with given store
--             if so, call cont and save result in store (shared!)
--             if not, test if suspension did at least something (test sref)
--                        if so, recursive call with new suspension
--                        if not, suspend with given sRef "done Nothing"
saveSuspValue :: (Curry a,Curry b) => 
             FreeVarRef -> (State -> a -> b) -> (Store -> a) -> Store -> b
saveSuspValue sRef cont susp store = 
  trace "saveSuspValue " $
  case fromStore store sRef of
     Nothing -> trace "saveSusp: Nothing" $ let x = susp store in
       case consKind x of
        Suspended -> if suspRef x then saveSuspValue sRef cont (suspCont x) store 
                                  else suspend False (saveSuspValue sRef cont (suspCont x))
        _ -> tr "saveSusp: not suspended" $ let res = cont (Just store) x in
          branching (nextOrRef ())
                [(\st -> Modified (addToStore st sRef res),trace "saveSusp: res" res)]
     Just v -> trace "saveSusp: Just" trV v v
  where
    tr = tra sRef
    trV x =  case consKind x of
       Branching -> case branches x of
                     [(_,b)] -> trV b
                     bs -> trace $ "Branch length " ++ (show $ length bs)
       ck -> tr (show ck)

-}

prim_identicalVar :: Curry a => a  -> a -> Result C_Bool
prim_identicalVar x y _ = case (consKind x,consKind y) of
  (Branching,Branching) -> let rx = orRef x
                               ry = orRef y in
                           if isGenerator rx && isGenerator ry
                           then toCurry (deref rx Prelude.== deref ry)
                           else C_False
  _                     -> C_False


prim_showAnyTerm t _ = toCurry (show t)

prim_showAnyQTerm :: Curry a => a -> Result C_String
prim_showAnyQTerm x _ = toCurry (showQ 0 x "")

prim_readsAnyUnqualifiedTerm = error "ExternalFunctionsUnsafe.prim_readsAnyUnqualifiedTerm"

prim_readsAnyQTerm = error "ExternalFunctionsUnsafe.prim_readsAnyQTerm"

showAnyExpression = error "ExternalFunctionsUnsafe.prim_showAnyExpression"


showAnyQExpression = error "ExternalFunctionsUnsafe.prim_showAnyQExpression"

--spawnConstraint :: Curry a => C_Success -> a -> a
spawnConstraint _ c x = undefined 

try :: Curry a => a -> Result (C_Either a (T2 C_OrRef (List a)))
try x _ = case consKind x of
          Val       -> C_Left x
          Branching -> C_Right (T2 (C_OrRef (orRef x)) 
                                   (fromHaskellList (branches x)))
          c -> error ("try: consKind is "++show c)

orsWithOrRef :: Curry a => C_OrRef -> List a -> Result a
orsWithOrRef (C_OrRef r) xs _ = branching r (toHaskellList xs)

generateChoice :: Curry a => List a -> Result a
generateChoice xs _ = branching (mkRef 0 0 (nextRef 0)) (toHaskellList xs)


nrOfChoices :: Result (C_IO C_Int)
nrOfChoices st = 
  C_IO (\ st' -> Prelude.return (IOVal (toCurry (max (size' st) (size' st')))))
  where
    size' = storeSize 




-- end included

c_unsafePerformIO :: (Curry t0) => (Curry.Module.Prelude.C_IO t0) -> Curry.RunTimeSystem.State -> t0
c_unsafePerformIO x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.Unsafe.c_prim_unsafePerformIO))(x1)(st)



c_trace :: (Curry t0) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> t0 -> Curry.RunTimeSystem.State -> t0
c_trace x1 x2 st = Curry.Module.Unsafe.c_unsafePerformIO(Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_putStr(x1)(st))(Curry.Module.Prelude.c_return(x2)(st))(st))(st)



c_isVar :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isVar x1 st = Curry.Module.Unsafe.c_unsafePerformIO(Curry.Module.Prelude.op_62_62_61(Curry.Module.Meta.c_isFree(x1)(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_either(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_const(Curry.Module.Prelude.C_True)))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_const(Curry.Module.Prelude.C_False)))))(st))(st))(st)



c_identicalVar :: (Curry t0) => t0 -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_identicalVar x1 x2 st = Curry.Module.Prelude.op_36_33(Curry.Module.Prelude.op_36_33(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Unsafe.c_prim_identicalVar))(x1)(st))(x2)(st)



c_showAnyTerm :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showAnyTerm x1 st = Curry.Module.Prelude.op_36_33_33(Curry.Module.Prelude.pf(Curry.Module.Unsafe.c_prim_showAnyTerm))(x1)(st)



c_showAnyQTerm :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showAnyQTerm x1 st = Curry.Module.Prelude.op_36_33_33(Curry.Module.Prelude.pf(Curry.Module.Unsafe.c_prim_showAnyQTerm))(x1)(st)



c_readsAnyUnqualifiedTerm :: (Curry t0) => (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_readsAnyUnqualifiedTerm x1@Curry.Module.Prelude.List x2 st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('U'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('q'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))(st)
c_readsAnyUnqualifiedTerm x1@((Curry.Module.Prelude.:<) x3 x4) x2 st = Curry.Module.Unsafe.c_readsAnyUnqualifiedTermWithPrefixes((Curry.Module.Prelude.:<)(x3)(x4))(x2)(st)
c_readsAnyUnqualifiedTerm (Curry.Module.Prelude.ListOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Unsafe.c_readsAnyUnqualifiedTerm(x)(x2)(st))(i)(xs)(st)
c_readsAnyUnqualifiedTerm x x2 st = Curry.RunTimeSystem.patternFail("Unsafe.readsAnyUnqualifiedTerm")(x)



c_readsAnyUnqualifiedTermWithPrefixes :: (Curry t0) => (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_readsAnyUnqualifiedTermWithPrefixes x1 x2 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Unsafe.c_prim_readsAnyUnqualifiedTerm))(x1)(st))(x2)(st)



c_readAnyUnqualifiedTerm :: (Curry t0) => (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> t0
c_readAnyUnqualifiedTerm x1 x2 st = Curry.Module.Unsafe.c_readAnyUnqualifiedTerm_case_7(x1)(x2)(Curry.Module.Unsafe.c_readsAnyUnqualifiedTerm(x1)(x2)(st))(st)



c_readsAnyQTerm :: (Curry t0) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_readsAnyQTerm x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Unsafe.c_prim_readsAnyQTerm))(x1)(st)



c_readAnyQTerm :: (Curry t0) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> t0
c_readAnyQTerm x1 st = Curry.Module.Unsafe.c_readAnyQTerm_case_3(x1)(Curry.Module.Unsafe.c_readsAnyQTerm(x1)(st))(st)



c_readAnyQTerm_case_3 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.Unsafe.c_readAnyQTerm_case_2(x4)(x3)(st)
c_readAnyQTerm_case_3 x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('U'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('Q'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))(st)
c_readAnyQTerm_case_3 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Unsafe.c_readAnyQTerm_case_3(x1)(x)(st))(i)(xs)(st)
c_readAnyQTerm_case_3 x1 x st = Curry.RunTimeSystem.patternFail("Unsafe.readAnyQTerm_case_3")(x)



c_readAnyQTerm_case_2 x4 x3@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.Unsafe.c_readAnyQTerm_case_1(x5)(x6)(x4)(st)
c_readAnyQTerm_case_2 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Unsafe.c_readAnyQTerm_case_2(x4)(x)(st))(i)(xs)(st)
c_readAnyQTerm_case_2 x4 x st = Curry.RunTimeSystem.patternFail("Unsafe.readAnyQTerm_case_2")(x)



c_readAnyQTerm_case_1 x5 x6 x4@Curry.Module.Prelude.List st = Curry.Module.Unsafe.c_readAnyQTerm_case_0(x5)(x6)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_all(Curry.Module.Prelude.pf(Curry.Module.Char.c_isSpace))(st))(x6)(st))(st)
c_readAnyQTerm_case_1 x5 x6 x4@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('U'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('Q'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))(st)
c_readAnyQTerm_case_1 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Unsafe.c_readAnyQTerm_case_1(x5)(x6)(x)(st))(i)(xs)(st)
c_readAnyQTerm_case_1 x5 x6 x st = Curry.RunTimeSystem.patternFail("Unsafe.readAnyQTerm_case_1")(x)



c_readAnyQTerm_case_0 x5 x6 x7@Curry.Module.Prelude.C_True st = x5
c_readAnyQTerm_case_0 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('U'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('Q'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))(st)
c_readAnyQTerm_case_0 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Unsafe.c_readAnyQTerm_case_0(x5)(x6)(x)(st))(i)(xs)(st)
c_readAnyQTerm_case_0 x5 x6 x st = Curry.RunTimeSystem.patternFail("Unsafe.readAnyQTerm_case_0")(x)



c_readAnyUnqualifiedTerm_case_7 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.Unsafe.c_readAnyUnqualifiedTerm_case_6(x5)(x4)(st)
c_readAnyUnqualifiedTerm_case_7 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('U'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('U'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('q'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))))))))(st)
c_readAnyUnqualifiedTerm_case_7 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Unsafe.c_readAnyUnqualifiedTerm_case_7(x1)(x2)(x)(st))(i)(xs)(st)
c_readAnyUnqualifiedTerm_case_7 x1 x2 x st = Curry.RunTimeSystem.patternFail("Unsafe.readAnyUnqualifiedTerm_case_7")(x)



c_readAnyUnqualifiedTerm_case_6 x5 x4@(Curry.Module.Prelude.T2 x6 x7) st = Curry.Module.Unsafe.c_readAnyUnqualifiedTerm_case_5(x6)(x7)(x5)(st)
c_readAnyUnqualifiedTerm_case_6 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Unsafe.c_readAnyUnqualifiedTerm_case_6(x5)(x)(st))(i)(xs)(st)
c_readAnyUnqualifiedTerm_case_6 x5 x st = Curry.RunTimeSystem.patternFail("Unsafe.readAnyUnqualifiedTerm_case_6")(x)



c_readAnyUnqualifiedTerm_case_5 x6 x7 x5@Curry.Module.Prelude.List st = Curry.Module.Unsafe.c_readAnyUnqualifiedTerm_case_4(x6)(x7)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_all(Curry.Module.Prelude.pf(Curry.Module.Char.c_isSpace))(st))(x7)(st))(st)
c_readAnyUnqualifiedTerm_case_5 x6 x7 x5@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('U'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('U'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('q'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))(st)
c_readAnyUnqualifiedTerm_case_5 x6 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Unsafe.c_readAnyUnqualifiedTerm_case_5(x6)(x7)(x)(st))(i)(xs)(st)
c_readAnyUnqualifiedTerm_case_5 x6 x7 x st = Curry.RunTimeSystem.patternFail("Unsafe.readAnyUnqualifiedTerm_case_5")(x)



c_readAnyUnqualifiedTerm_case_4 x6 x7 x8@Curry.Module.Prelude.C_True st = x6
c_readAnyUnqualifiedTerm_case_4 x6 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_error(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('U'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('U'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('q'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))(x7)(st))(st)
c_readAnyUnqualifiedTerm_case_4 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Unsafe.c_readAnyUnqualifiedTerm_case_4(x6)(x7)(x)(st))(i)(xs)(st)
c_readAnyUnqualifiedTerm_case_4 x6 x7 x st = Curry.RunTimeSystem.patternFail("Unsafe.readAnyUnqualifiedTerm_case_4")(x)



c_prim_unsafePerformIO :: (Curry t0) => (Curry.Module.Prelude.C_IO t0) -> Curry.RunTimeSystem.State -> t0
c_prim_unsafePerformIO x1 st = Curry.Module.Unsafe.prim_unsafePerformIO(x1)(st)



c_spawnConstraint :: (Curry t0) => Curry.Module.Prelude.C_Success -> t0 -> Curry.RunTimeSystem.State -> t0
c_spawnConstraint x1 x2 st = Curry.Module.Unsafe.spawnConstraint(x1)(x2)(st)



c_prim_isVar :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_prim_isVar x1 st = Curry.Module.Unsafe.prim_isVar(x1)(st)



c_prim_identicalVar :: (Curry t0) => t0 -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_prim_identicalVar x1 x2 st = Curry.Module.Unsafe.prim_identicalVar(x1)(x2)(st)



c_prim_showAnyTerm :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_prim_showAnyTerm x1 st = Curry.Module.Unsafe.prim_showAnyTerm(x1)(st)



c_prim_showAnyQTerm :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_prim_showAnyQTerm x1 st = Curry.Module.Unsafe.prim_showAnyQTerm(x1)(st)



c_prim_readsAnyUnqualifiedTerm :: (Curry t0) => (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_prim_readsAnyUnqualifiedTerm x1 x2 st = Curry.Module.Unsafe.prim_readsAnyUnqualifiedTerm(x1)(x2)(st)



c_prim_readsAnyQTerm :: (Curry t0) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_prim_readsAnyQTerm x1 st = Curry.Module.Unsafe.prim_readsAnyQTerm(x1)(st)



c_showAnyExpression :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showAnyExpression x1 st = Curry.Module.Unsafe.showAnyExpression(x1)(st)



c_showAnyQExpression :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showAnyQExpression x1 st = Curry.Module.Unsafe.showAnyQExpression(x1)(st)



c_try :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Either t0 (Curry.Module.Prelude.T2 Curry.Module.Meta.C_OrRef (Curry.Module.Prelude.List t0))
c_try x1 st = Curry.Module.Unsafe.try(x1)(st)



c_orsWithOrRef :: (Curry t0) => Curry.Module.Meta.C_OrRef -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> t0
c_orsWithOrRef x1 x2 st = Curry.Module.Unsafe.orsWithOrRef(x1)(x2)(st)



c_generateChoice :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> t0
c_generateChoice x1 st = Curry.Module.Unsafe.generateChoice(x1)(st)



c_nrOfChoices :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Int
c_nrOfChoices st = Curry.Module.Unsafe.nrOfChoices(st)


