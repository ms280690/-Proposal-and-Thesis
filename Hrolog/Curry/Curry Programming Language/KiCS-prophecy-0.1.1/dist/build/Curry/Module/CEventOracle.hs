{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}

{-# INCLUDE <coracle.h> #-}

module Curry.Module.CEventOracle (module Curry.Module.CEventOracle) where

import Curry.RunTimeSystem
import Curry.Module.IOExts
import Curry.Module.Prelude
import Curry.Module.System



-- begin included



import Foreign

import Foreign
import Foreign.C.String	

foreign import ccall unsafe "static coracle.h init"      cinitialize :: Ref
foreign import ccall unsafe "static coracle.h nextref"   cfresh      :: Int -> Ref

foreign import ccall unsafe "static coracle.h collapse"  ccollapse   :: Ref -> ()
foreign import ccall unsafe "static coracle.h close_ref" ccloseRef   :: Ref -> ()
foreign import ccall unsafe "static coracle.h inconly"   creplace    :: Ref -> ()
foreign import ccall unsafe "static coracle.h expand"    cexpand     :: Ref -> Int -> ()

foreign import ccall unsafe "coracle.h finalize" cfinalize :: CString -> ()

data CRef

type Ref = Ptr CRef

newtype C_Ref = C_Ref Ref

instance Show C_Ref
instance Read C_Ref
instance Eq   C_Ref
instance BaseCurry C_Ref
instance Curry C_Ref

initRef :: Result (C_IO C_Ref)
initRef = let ref = cinitialize in seq ref (Curry.Module.Prelude.return (C_Ref ref))

finalize :: C_String -> Result (C_IO T0)
finalize s _ = C_IO (\ _ -> do
  s' <- newCString (fromCurry s)
  seq (cfinalize s') (Prelude.return (IOVal (T0))))

--- Side effect that computes a fresh reference.
fresh :: a -> Result C_Ref
fresh _ _ =  C_Ref (cfresh 0)

--- increase couter of ref by one
replace :: C_Ref -> a -> b -> a
replace  (C_Ref ref) x _ = seq (creplace ref) x

--- Remove a ref and combine and counter +1
collapse :: C_Ref -> a -> b -> a
collapse  (C_Ref ref) x _ = seq (ccollapse ref) x

--- Remove a ref and combine and counter +1
closeRef :: C_Ref -> a -> b -> a
closeRef  (C_Ref ref) x _ = seq (ccloseRef ref) x


--- Projection on last argument that releases an event as a side effect.
-- increment step counter of first ref and add remaining refs
-- list has to be at least of size 1
expand :: C_Ref -> List C_Ref -> a -> Result a
expand  (C_Ref ref) refs x _
  = seq (cexpand ref (len refs)) (seq (evalRefs refs) x)
 where
  len List      = 0
  len (_ :< rs) = 1+len rs

  evalRefs List = ()
  evalRefs (r :< rs) = seq r (evalRefs rs)

-- compare to BaseCurry.mapOr
c_onBranches :: (BaseCurry a,BaseCurry b) => 
  C_Ref -> (C_Ref -> a -> Result b) -> OrRef -> Branches a -> Result b
c_onBranches r cont orref bs = 
  evalRef' True (\ r x st -> replace r (cont r x st) st) (branching orref bs) r 
  
liftCase :: (BaseCurry a,BaseCurry b) => 
  C_Ref -> (C_Ref -> a -> Result b) -> OrRef -> Branches a -> (Int -> State) -> b
liftCase ref f orref bs = 
  expand' ref (length bs - 1) (\ refs -> lift ($) orref (zipWith f refs bs))

expand' :: C_Ref -> Int -> ([C_Ref] -> a) -> a
expand' (C_Ref ref) l cont = 
   cexpand ref l `seq`
   evalRefs refs `seq`
   cont (map C_Ref (ref:refs))
 where
   refs = map cfresh (replicate l 0)
   
   evalRefs [] = ()
   evalRefs (r : rs) = seq r (evalRefs rs)
   

--- generating a fresh variable
unknown :: Curry a => C_Ref -> Result a
unknown r st = freeF (\x -> gen r x st)

gen :: Curry a => C_Ref -> a -> Result a
gen r x st = case consKind x of
  Val       -> push r x st
  Branching -> liftCase r gen (orRef x) (branches x) (const st)

push :: Curry a => C_Ref -> a -> Result a
push r x st = case foldCurry (\ _ n _ -> n+1) 0 x st of
  0 -> collapse r x st
  1 -> replace r (propagate (\_ -> gen r) x st) st
  n -> expand' r (n-1) (\ refs -> propagate (distRefs refs) x st)

  where
    distRefs rs i x = gen (rs!!i) x
  
 
type RefFun a b = C_Ref -> Result (Prim (a -> Result b))

($!), ($#),($!!),($##), apply :: (Curry a,Curry b) => 
  Prim (RefFun a b) -> a -> C_Ref -> Result b
(cont $! x)  r = Curry.Module.CEventOracle.prepApply hnfRef x cont r
(cont $# x)  r = Curry.Module.CEventOracle.prepApply (evalRef True) x cont r
(cont $!! x) r st = Curry.Module.CEventOracle.prepApply hnfRef (nfCTC const x st) cont r st
(cont $## x) r st = Curry.Module.CEventOracle.prepApply (evalRef True) (gnfCTC const x st) cont r st


apply cont x r = Curry.Module.CEventOracle.prepApply (\ cont' x' r' st' -> 
                               replace r' (Curry.Module.Prelude.apply (cont' r' st') x' st') st') x cont r

prepApply :: (BaseCurry a,BaseCurry b) => 
  (RefFun b a -> b -> C_Ref -> Result a) -> b -> Prim (RefFun b a) -> C_Ref -> Result a
prepApply  prep x (PrimValue f) cref st = prep f x cref st
prepApply  prep x (PrimOr r bs) cref st = 
  c_onBranches cref (\ r' f' -> Curry.Module.CEventOracle.prepApply prep x f' r') r bs st
prepApply  _    _ f                r st  = collapse r (patternFail "Prelude.prepApply" f) st

hnfRef :: (BaseCurry a,BaseCurry b) => RefFun b a -> b -> C_Ref -> Result a
hnfRef = evalRef False 

evalRef :: (BaseCurry a,BaseCurry b) => HNFMode -> RefFun a b -> a -> C_Ref -> Result b
evalRef mode cont = evalRef' mode (\ r x st ->  replace r (Curry.Module.Prelude.apply (cont r st) x st) st)

evalRef' :: (BaseCurry a,BaseCurry b) => 
            HNFMode -> (C_Ref -> a -> Result b) -> a -> C_Ref -> Result b
evalRef' mode cont x r state = 
 case consKind x of
   Failed    -> closeRef r (addException err x) state
   Branching -> let orref = orRef x
                    bs = branches x in
    manipulateStore
      (closeRef r (Curry.RunTimeSystem.failed (curryError "=:=")) state)
      contEval
      (\ ref' contSt -> if mode || not (isGenerator orref)
                      then liftCase r contEvalRef (narrowOrRef orref) bs contSt
                      else cont r (branching ref' bs) state)
      (\ orref' x' st' -> branching orref' [contEval x' st'])
      orref bs state
   Val       -> cont r x state

  where
    err = curryError ("Prelude."++if mode then "$#" else "$!")
    contEvalRef r' x' st' = evalRef' mode cont x' r' st'
    contEval = contEvalRef r




-- end included

c_initialize :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_initialize x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.System.c_getProgName(st))(Curry.Module.Prelude.pf(Curry.Module.CEventOracle.c_initialize'46_'35lambda2(x1)))(st)



c_initialize'46_'35lambda2 :: (Curry t10) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t10)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_initialize'46_'35lambda2 x1 x2 st = let {x3 = Curry.Module.Prelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))(st)} in Curry.Module.Prelude.op_62_62(Curry.Module.IOExts.c_setAssoc((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List))))))(x3)(st))(Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_writeFile(x3)(Curry.Module.Prelude.List)(st))(Curry.Module.Prelude.op_62_62_61(Curry.Module.CEventOracle.c_initRef(st))(Curry.Module.Prelude.pf(Curry.Module.CEventOracle.c_initialize'46_'35lambda2'46_'35lambda3(x1)(x2)))(st))(st))(st)



c_initialize'46_'35lambda2'46_'35lambda3 :: (Curry t10) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t10)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_initialize'46_'35lambda2'46_'35lambda3 x1 x2 x3 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_apply(x1)(x3)(st))(Curry.Module.Prelude.pf(Curry.Module.CEventOracle.c_initialize'46_'35lambda2'46_'35lambda3'46_'35lambda4(x2)))(st)



c_initialize'46_'35lambda2'46_'35lambda3'46_'35lambda4 :: (Curry t10) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> t10 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_initialize'46_'35lambda2'46_'35lambda3'46_'35lambda4 x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_getSearchTree(x2)(st))(Curry.Module.Prelude.pf(Curry.Module.CEventOracle.c_initialize'46_'35lambda2'46_'35lambda3'46_'35lambda4'46_'35lambda5(x1)))(st)



c_initialize'46_'35lambda2'46_'35lambda3'46_'35lambda4'46_'35lambda5 :: (Curry t10) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.C_SearchTree t10) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_initialize'46_'35lambda2'46_'35lambda3'46_'35lambda4'46_'35lambda5 x1 x2 st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_catchFail(Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_putStrLn((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))))))))))))))))))))(st))(Curry.Module.Prelude.op_36_33_33(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(x2)(st))(st))(Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_putStrLn((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('W'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))(st))(Curry.Module.Prelude.c_return(Curry.Module.Prelude.c_failed(st))(st))(st))(st))(Curry.Module.CEventOracle.c_finalize(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))))(st))(st))(st)



c_initRef :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.CEventOracle.C_Ref
c_initRef st = Curry.Module.CEventOracle.initRef(st)



c_finalize :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_finalize x1 st = Curry.Module.CEventOracle.finalize(x1)(st)



c_fresh :: Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.CEventOracle.C_Ref
c_fresh x1 st = Curry.Module.CEventOracle.fresh(x1)(st)



c_replace :: (Curry t0) => Curry.Module.CEventOracle.C_Ref -> t0 -> Curry.RunTimeSystem.State -> t0
c_replace x1 x2 st = Curry.Module.CEventOracle.replace(x1)(x2)(st)



c_collapse :: (Curry t0) => Curry.Module.CEventOracle.C_Ref -> t0 -> Curry.RunTimeSystem.State -> t0
c_collapse x1 x2 st = Curry.Module.CEventOracle.collapse(x1)(x2)(st)



c_closeRef :: (Curry t0) => Curry.Module.CEventOracle.C_Ref -> t0 -> Curry.RunTimeSystem.State -> t0
c_closeRef x1 x2 st = Curry.Module.CEventOracle.closeRef(x1)(x2)(st)



c_expand :: (Curry t0) => Curry.Module.CEventOracle.C_Ref -> (Curry.Module.Prelude.List Curry.Module.CEventOracle.C_Ref) -> t0 -> Curry.RunTimeSystem.State -> t0
c_expand x1 x2 x3 st = Curry.Module.CEventOracle.expand(x1)(x2)(x3)(st)



c_unknown :: (Curry t0) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_unknown x1 st = Curry.Module.CEventOracle.unknown(x1)(st)



op_36_33 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1))) -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t1
op_36_33 x1 x2 x3 st = (Curry.Module.CEventOracle.$!)(x1)(x2)(x3)(st)



op_36_33_33 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1))) -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t1
op_36_33_33 x1 x2 x3 st = (Curry.Module.CEventOracle.$!!)(x1)(x2)(x3)(st)



op_36_35 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1))) -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t1
op_36_35 x1 x2 x3 st = (Curry.Module.CEventOracle.$#)(x1)(x2)(x3)(st)



op_36_35_35 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1))) -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t1
op_36_35_35 x1 x2 x3 st = (Curry.Module.CEventOracle.$##)(x1)(x2)(x3)(st)



c_apply :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1))) -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t1
c_apply x1 x2 x3 st = Curry.Module.CEventOracle.apply(x1)(x2)(x3)(st)


