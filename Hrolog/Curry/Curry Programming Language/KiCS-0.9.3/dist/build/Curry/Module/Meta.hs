{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Meta (module Curry.Module.Meta) where

import Curry.RunTimeSystem
import Curry.Module.Prelude



-- begin included



import System.Mem.Weak ( addFinalizer )
import Control.Concurrent
import System.IO.Unsafe ( unsafeInterleaveIO )
import Debug.Trace ( trace )
import Data.List

data C_OrRef = C_OrRef OrRef
  | C_OrRefFail Curry.RunTimeSystem.C_Exceptions
  | C_OrRefOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches C_OrRef)

instance BaseCurry C_OrRef where
  nf f x state = f(x)(state)

  gnf f x state = f(x)(state)

  generator _ = error "free Variable of type OrRef"

  failed  = C_OrRefFail

  branching  = C_OrRefOr

  consKind (C_OrRefOr _ _) = Curry.RunTimeSystem.Branching
  consKind (C_OrRefFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (C_OrRefFail x) = x

  orRef (C_OrRefOr x _) = x

  branches (C_OrRefOr _ x) = x

instance Curry C_OrRef where
  strEq (C_OrRef x1) (C_OrRef y1) _ 
     = if x1 Prelude.== y1 then strEqSuccess else strEqFail "OrRef"
  strEq x0 _ _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (C_OrRef x1) (C_OrRef y1) _ = 
    if x1 Prelude.== y1 then C_True else C_False
  eq _ _ _ = C_False

  typeName _ = "OrRef"

  propagate _ o _ = o
  
  foldCurry _ c _ _ = c

  showQ d (C_OrRef x1) = showParen (d>10) (showString "Unsafe.OrRef" . showsPrec d x1)





instance Show C_OrRef where
  showsPrec d (C_OrRef x1) = showParen (d>10) (showString "OrRef" . showsPrec d x1)


instance Read C_OrRef where
  readsPrec d r = [ (C_OrRef ref,s) | (ref,s) <- readsPrec d r]


---------------------------------------------------------------------------------
-- test for free variable
---------------------------------------------------------------------------------

prim_isFree :: (Curry t0) => t0 -> Result (C_IO (C_Either t0 t0))
prim_isFree x _ = C_IO (\ _ -> case consKind x of
  Branching -> Prelude.return (IOVal (if isGenerator (orRef x) 
                              then C_Left x 
                              else C_Right x))
  _         -> Prelude.return (IOVal (C_Right x)))

---------------------------------------------------------------------------------
-- various normal forms in io monad
---------------------------------------------------------------------------------

-- yield head normal form with current state 
-- (including fetching and looking up variable bindings)
-- then apply continuation on it and make sure that you got a value
-- of type io before finally executing that action.

headNormalFormIO :: (Curry a,Curry b) => Prim (a -> Result (C_IO b)) -> a -> Result (C_IO b)
headNormalFormIO cont x _ = 
  C_IO (hnfCTC (\ x' st -> hnfCTC exec2 (apply cont x' st) st) x)

searchTree :: Curry a => a -> Result (C_SearchTree a)
searchTree = searchTr 

hnfIO  x _ = C_IO (hnfCTC  (\ x _ -> Prelude.return (IOVal x)) x)
nfIO   x _ = C_IO (nfCTC   (\ x _ -> Prelude.return (IOVal x)) x)
gnfIO  x _ = C_IO (ghnfCTC (\ x _ -> Prelude.return (IOVal x)) x)
ghnfIO x _ = C_IO (ghnfCTC (\ x _ -> Prelude.return (IOVal x)) x)

---------------------------------------------------------------------------------
-- rich search trees
---------------------------------------------------------------------------------

getRichSearchTree :: Curry a => a -> Result (C_IO (C_RichSearchTree a))
getRichSearchTree x _ = C_IO (\ state -> Prelude.return (IOVal (richSearchTr x state)))

richSearchTree :: Curry a => a -> Result (C_RichSearchTree a)
richSearchTree = richSearchTr 

--inject :: Curry a => C_Context -> a -> C_RichSearchTree a
--inject (Context c) = richSearchTr c
 
richSearchTr :: Curry a => a -> Result (C_RichSearchTree a)
richSearchTr x state = 
  transVal (nfCTC (nfCTC (\ x _ -> x)) x state)
  where
    transVal x = case consKind x of
                   Val       -> C_RichValue x
                   Failed    -> C_RichFail (toCurry (exceptions x))
                   Branching -> transBranching (orRef x) (branches x)

    transBranching _  []         = C_RichFail (C_ErrorCall List)
    transBranching _  [x]        = transVal x
    transBranching r  xs@(_:_:_) = C_RichChoice (C_OrRef r)
                                            (fromHaskellList (map transVal xs))

instance ConvertCH C_Exception Exception where
  toCurry (ErrorCall s)        = C_ErrorCall (toCurry s)
  toCurry (PatternMatchFail s) = C_PatternMatchFail (toCurry s)
  toCurry (AssertionFailed s)  = C_AssertionFailed (toCurry s)
  toCurry (IOException s)      = C_IOException (toCurry s)
  toCurry PreludeFailed        = C_PreludeFailed

  fromCurry (C_ErrorCall s)        = ErrorCall (fromCurry s)
  fromCurry (C_PatternMatchFail s) = PatternMatchFail (fromCurry s)
  fromCurry (C_AssertionFailed s)  = AssertionFailed (fromCurry s)
  fromCurry (C_IOException s)      = IOException (fromCurry s)
  fromCurry C_PreludeFailed        = PreludeFailed

---------------------------------------------------------------------------------
-- parallel search
---------------------------------------------------------------------------------

parallelSearch :: Curry a => a -> Result (C_IO (List a))
parallelSearch v _ = C_IO (\state -> do
  chan <- newChan
  mvar <- newEmptyMVar
  qsem <- newMyQSem 0
  tid <- forkIO (searchThread qsem mvar chan 
                              (nfCTC (nfCTC (\ x _ -> x)) v state))
  putMVar mvar [tid]
  --addFinalizer res (stopSearch mvar2)
  res <- myGetChanContents qsem chan
  Prelude.return (IOVal (fromHaskellList res)))


myGetChanContents :: Show a => MyQSem -> Chan (Maybe a) -> IO [a]
myGetChanContents qsem chan =
  unsafeInterleaveIO ( do
    decMyQSem qsem
    x <- readChan chan
    case x of
         Nothing -> Prelude.return []
         Just y -> do
            xs <- myGetChanContents qsem chan
            Prelude.return (y:xs) )


stopSearch :: MVar [ThreadId] -> IO ()
stopSearch mvar = do
  print "start"
  ids <- takeMVar mvar
  mapM_ killThread ids
  --putMVar mvar []


removeId :: MVar [ThreadId] -> Chan (Maybe a) -> ThreadId -> IO ()
removeId mvar chan tid = do
  ids <- takeMVar mvar
  let newids = delete tid ids
  case newids of
       [] -> writeChan chan Nothing -- >> putMVar mvar []
       _  -> putMVar mvar newids 


searchThread :: Curry a => MyQSem -> MVar [ThreadId] -> Chan (Maybe a) 
             -> a -> IO ()
searchThread qsem mvar chan x = do
  case consKind x of
    Val       -> incMyQSem qsem >> writeChan chan (Just x) >> terminate
    Failed    -> terminate
    Branching -> do
      --yield
      testMyQSem qsem
      let b:bs = branches x
      -- to prevent the threads from terminating till their Ids are registered
      ids <- takeMVar mvar  
      newIds <- mapM (forkIO . searchThread qsem mvar chan) bs
      putMVar mvar (newIds++ids)
      searchThread qsem mvar chan b
 where
  noThreads = do
    ids <- takeMVar mvar
    putStrLn ("noThreads: " ++ show (length ids))
    putMVar mvar ids
  terminate = do
    tid <- myThreadId
    removeId mvar chan tid

newtype MyQSem = MyQSem (MVar (Int, [MVar ()]))

-- |Build a new 'MyQSem'
newMyQSem :: Int -> IO MyQSem
newMyQSem init = do
   sem <- newMVar (init,[])
   Prelude.return (MyQSem sem)


-- |Wait for a unit to become available
incMyQSem :: MyQSem -> IO ()
incMyQSem (MyQSem sem) = do
   (avail,blocked) <- takeMVar sem
   putMVar sem (avail+1,blocked)


-- |Signal that a unit of the 'MyQSem' is available
decMyQSem :: MyQSem -> IO ()
decMyQSem (MyQSem sem) = do
   (avail,blocked) <- takeMVar sem
   if avail>0 then putMVar sem (avail-1,blocked)
              else mapM_ (flip putMVar ()) blocked >> putMVar sem (avail-1,[]) 


testMyQSem :: MyQSem -> IO ()
testMyQSem (MyQSem sem) = do
   x@(avail,blocked) <- takeMVar sem
   if avail<0 then putMVar sem x
              else do
                block <- newEmptyMVar
                putMVar sem (avail,block:blocked)
                takeMVar block

-------------------------------
-- covering non-determinism
-------------------------------

cover :: Curry a => a -> Result a
cover x st = case consKind x of
              Branching -> branching (Curry.RunTimeSystem.cover (orRef x)) 
                                     (map (flip Curry.Module.Meta.cover st) (branches x))
              _ -> x

-----------------------------------
-- encapsulate to head normal form
-----------------------------------

st :: Curry a => a -> Result (C_SearchTree a)
st x s = transVal (hnfCTC (\ x _ -> x) x s)
  where
    transVal x = case consKind x of
        Val       -> C_Value x
        Failed    -> C_Fail
        Branching -> let ref = orRef x in 
          if   isCovered ref
          then C_SearchTreeOr (uncover ref) (map (flip st s) (branches x))
          else C_Choice (fromHaskellList (map transVal (branches x)))

-----------------------------------
-- encapsulate to head normal form
-----------------------------------

richST :: Curry a => a -> Result (C_RichSearchTree a)
richST x s = transVal (hnfCTC (\ x _ -> x) x s)
  where
    transVal x = case consKind x of
        Val       -> C_RichValue x
        Failed    -> C_RichFail (toCurry (exceptions x))
        Branching -> let ref = orRef x in 
          if   isCovered ref
          then C_RichSearchTreeOr (uncover ref) 
                                  (map (flip richST s) (branches x))
          else C_RichChoice (C_OrRef (orRef x))
                        (fromHaskellList (map transVal (branches x)))

-----------------------------
-- the general question mark
-----------------------------

ors :: Curry a => List a -> Result a
ors xs _ = branching (error "Unsafe.ors") (toHaskellList xs)


-- temporarily added

prim_throw :: Curry a => C_Exception -> Result a
prim_throw e _ = Curry.RunTimeSystem.failed (fromCurry e)


-- end included

data C_RichSearchTree t0 = C_RichFail Curry.Module.Meta.C_Exception
  | C_RichValue t0
  | C_RichChoice Curry.Module.Meta.C_OrRef (Curry.Module.Prelude.List (Curry.Module.Meta.C_RichSearchTree t0))
  | C_RichSuspend
  | C_RichSearchTreeFail Curry.RunTimeSystem.C_Exceptions
  | C_RichSearchTreeOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Meta.C_RichSearchTree t0))

data C_Exception = C_ErrorCall (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_PatternMatchFail (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_AssertionFailed (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_IOException (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_PreludeFailed
  | C_ExceptionFail Curry.RunTimeSystem.C_Exceptions
  | C_ExceptionOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.Meta.C_Exception)

data C_Seq t0 = C_Nil
  | C_Cons t0 (Curry.Module.Meta.C_Seq t0)
  | C_Continued (Curry.Module.Meta.C_Seq t0)
  | C_SeqFail Curry.RunTimeSystem.C_Exceptions
  | C_SeqOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Meta.C_Seq t0))

instance (BaseCurry t0) => BaseCurry (Curry.Module.Meta.C_RichSearchTree t0) where
  nf f (Curry.Module.Meta.C_RichFail x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Meta.C_RichFail(v1))(state1))(x1)(state0)
  nf f (Curry.Module.Meta.C_RichValue x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Meta.C_RichValue(v1))(state1))(x1)(state0)
  nf f (Curry.Module.Meta.C_RichChoice x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.Meta.C_RichChoice(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Meta.C_RichFail x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Meta.C_RichFail(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.Meta.C_RichValue x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Meta.C_RichValue(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.Meta.C_RichChoice x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.Meta.C_RichChoice(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Meta.C_RichSearchTreeOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.Meta.C_RichFail(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.Meta.C_RichValue(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.Meta.C_RichChoice(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.Meta.C_RichSuspend]))(2)

  failed  = Curry.Module.Meta.C_RichSearchTreeFail

  branching  = Curry.Module.Meta.C_RichSearchTreeOr

  consKind (Curry.Module.Meta.C_RichSearchTreeOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Meta.C_RichSearchTreeFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Meta.C_RichSearchTreeFail x) = x

  orRef (Curry.Module.Meta.C_RichSearchTreeOr x _) = x

  branches (Curry.Module.Meta.C_RichSearchTreeOr _ x) = x





instance BaseCurry Curry.Module.Meta.C_Exception where
  nf f (Curry.Module.Meta.C_ErrorCall x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Meta.C_ErrorCall(v1))(state1))(x1)(state0)
  nf f (Curry.Module.Meta.C_PatternMatchFail x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Meta.C_PatternMatchFail(v1))(state1))(x1)(state0)
  nf f (Curry.Module.Meta.C_AssertionFailed x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Meta.C_AssertionFailed(v1))(state1))(x1)(state0)
  nf f (Curry.Module.Meta.C_IOException x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Meta.C_IOException(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Meta.C_ErrorCall x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Meta.C_ErrorCall(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.Meta.C_PatternMatchFail x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Meta.C_PatternMatchFail(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.Meta.C_AssertionFailed x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Meta.C_AssertionFailed(v1))(state1))(x1)(state0)
  gnf f (Curry.Module.Meta.C_IOException x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Meta.C_IOException(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Meta.C_ExceptionOr(Curry.RunTimeSystem.mkRef(r)(1)(i))([Curry.Module.Meta.C_ErrorCall(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.Meta.C_PatternMatchFail(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.Meta.C_AssertionFailed(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.Meta.C_IOException(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.Meta.C_PreludeFailed]))(1)

  failed  = Curry.Module.Meta.C_ExceptionFail

  branching  = Curry.Module.Meta.C_ExceptionOr

  consKind (Curry.Module.Meta.C_ExceptionOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Meta.C_ExceptionFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Meta.C_ExceptionFail x) = x

  orRef (Curry.Module.Meta.C_ExceptionOr x _) = x

  branches (Curry.Module.Meta.C_ExceptionOr _ x) = x





instance (BaseCurry t0) => BaseCurry (Curry.Module.Meta.C_Seq t0) where
  nf f (Curry.Module.Meta.C_Cons x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.Meta.C_Cons(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f (Curry.Module.Meta.C_Continued x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Meta.C_Continued(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Meta.C_Cons x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.Meta.C_Cons(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f (Curry.Module.Meta.C_Continued x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Meta.C_Continued(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Meta.C_SeqOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.Meta.C_Nil,Curry.Module.Meta.C_Cons(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.Meta.C_Continued(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.Meta.C_SeqFail

  branching  = Curry.Module.Meta.C_SeqOr

  consKind (Curry.Module.Meta.C_SeqOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Meta.C_SeqFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Meta.C_SeqFail x) = x

  orRef (Curry.Module.Meta.C_SeqOr x _) = x

  branches (Curry.Module.Meta.C_SeqOr _ x) = x





instance (Curry t0) => Curry (Curry.Module.Meta.C_RichSearchTree t0) where
  strEq (Curry.Module.Meta.C_RichFail x1) (Curry.Module.Meta.C_RichFail y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.Meta.C_RichValue x1) (Curry.Module.Meta.C_RichValue y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.Meta.C_RichChoice x1 x2) (Curry.Module.Meta.C_RichChoice y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq Curry.Module.Meta.C_RichSuspend Curry.Module.Meta.C_RichSuspend st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Meta.C_RichFail x1) (Curry.Module.Meta.C_RichFail y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.Meta.C_RichValue x1) (Curry.Module.Meta.C_RichValue y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.Meta.C_RichChoice x1 x2) (Curry.Module.Meta.C_RichChoice y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq Curry.Module.Meta.C_RichSuspend Curry.Module.Meta.C_RichSuspend st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Meta.C_RichFail x1) st = Curry.Module.Meta.C_RichFail(f((0::Int))(x1)(st))
  propagate f (Curry.Module.Meta.C_RichValue x1) st = Curry.Module.Meta.C_RichValue(f((0::Int))(x1)(st))
  propagate f (Curry.Module.Meta.C_RichChoice x1 x2) st = Curry.Module.Meta.C_RichChoice(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f Curry.Module.Meta.C_RichSuspend st = Curry.Module.Meta.C_RichSuspend

  foldCurry f c (Curry.Module.Meta.C_RichFail x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.Meta.C_RichValue x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.Meta.C_RichChoice x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c Curry.Module.Meta.C_RichSuspend st = c

  typeName _ = "RichSearchTree"

  showQ d (Curry.Module.Meta.C_RichFail x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Meta.RichFail "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.Meta.C_RichValue x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Meta.RichValue "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.Meta.C_RichChoice x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Meta.RichChoice "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ Curry.Module.Meta.C_RichSuspend = Prelude.showString("Meta.RichSuspend")
  showQ _ (Curry.Module.Meta.C_RichSearchTreeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.Meta.C_Exception where
  strEq (Curry.Module.Meta.C_ErrorCall x1) (Curry.Module.Meta.C_ErrorCall y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.Meta.C_PatternMatchFail x1) (Curry.Module.Meta.C_PatternMatchFail y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.Meta.C_AssertionFailed x1) (Curry.Module.Meta.C_AssertionFailed y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq (Curry.Module.Meta.C_IOException x1) (Curry.Module.Meta.C_IOException y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq Curry.Module.Meta.C_PreludeFailed Curry.Module.Meta.C_PreludeFailed st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Meta.C_ErrorCall x1) (Curry.Module.Meta.C_ErrorCall y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.Meta.C_PatternMatchFail x1) (Curry.Module.Meta.C_PatternMatchFail y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.Meta.C_AssertionFailed x1) (Curry.Module.Meta.C_AssertionFailed y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq (Curry.Module.Meta.C_IOException x1) (Curry.Module.Meta.C_IOException y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq Curry.Module.Meta.C_PreludeFailed Curry.Module.Meta.C_PreludeFailed st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Meta.C_ErrorCall x1) st = Curry.Module.Meta.C_ErrorCall(f((0::Int))(x1)(st))
  propagate f (Curry.Module.Meta.C_PatternMatchFail x1) st = Curry.Module.Meta.C_PatternMatchFail(f((0::Int))(x1)(st))
  propagate f (Curry.Module.Meta.C_AssertionFailed x1) st = Curry.Module.Meta.C_AssertionFailed(f((0::Int))(x1)(st))
  propagate f (Curry.Module.Meta.C_IOException x1) st = Curry.Module.Meta.C_IOException(f((0::Int))(x1)(st))
  propagate f Curry.Module.Meta.C_PreludeFailed st = Curry.Module.Meta.C_PreludeFailed

  foldCurry f c (Curry.Module.Meta.C_ErrorCall x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.Meta.C_PatternMatchFail x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.Meta.C_AssertionFailed x1) st = f(x1)(c)(st)
  foldCurry f c (Curry.Module.Meta.C_IOException x1) st = f(x1)(c)(st)
  foldCurry f c Curry.Module.Meta.C_PreludeFailed st = c

  typeName _ = "Exception"

  showQ d (Curry.Module.Meta.C_ErrorCall x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Meta.ErrorCall "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.Meta.C_PatternMatchFail x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Meta.PatternMatchFail "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.Meta.C_AssertionFailed x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Meta.AssertionFailed "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ d (Curry.Module.Meta.C_IOException x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Meta.IOException "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ Curry.Module.Meta.C_PreludeFailed = Prelude.showString("Meta.PreludeFailed")
  showQ _ (Curry.Module.Meta.C_ExceptionOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Curry t0) => Curry (Curry.Module.Meta.C_Seq t0) where
  strEq Curry.Module.Meta.C_Nil Curry.Module.Meta.C_Nil st = Curry.Module.Prelude.strEqSuccess
  strEq (Curry.Module.Meta.C_Cons x1 x2) (Curry.Module.Meta.C_Cons y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq (Curry.Module.Meta.C_Continued x1) (Curry.Module.Meta.C_Continued y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.Meta.C_Nil Curry.Module.Meta.C_Nil st = Curry.Module.Prelude.C_True
  eq (Curry.Module.Meta.C_Cons x1 x2) (Curry.Module.Meta.C_Cons y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq (Curry.Module.Meta.C_Continued x1) (Curry.Module.Meta.C_Continued y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.Meta.C_Nil st = Curry.Module.Meta.C_Nil
  propagate f (Curry.Module.Meta.C_Cons x1 x2) st = Curry.Module.Meta.C_Cons(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))
  propagate f (Curry.Module.Meta.C_Continued x1) st = Curry.Module.Meta.C_Continued(f((0::Int))(x1)(st))

  foldCurry f c Curry.Module.Meta.C_Nil st = c
  foldCurry f c (Curry.Module.Meta.C_Cons x1 x2) st = f(x1)(f(x2)(c)(st))(st)
  foldCurry f c (Curry.Module.Meta.C_Continued x1) st = f(x1)(c)(st)

  typeName _ = "Seq"

  showQ _ Curry.Module.Meta.C_Nil = Prelude.showString("Meta.Nil")
  showQ d (Curry.Module.Meta.C_Cons x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Meta.Cons "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ d (Curry.Module.Meta.C_Continued x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Meta.Continued "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.Meta.C_SeqOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.Meta.C_RichSearchTree t0) where
  showsPrec d (Curry.Module.Meta.C_RichFail x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("RichFail "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.Meta.C_RichValue x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("RichValue "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.Meta.C_RichChoice x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("RichChoice "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ Curry.Module.Meta.C_RichSuspend = Prelude.showString("RichSuspend")
  showsPrec _ (Curry.Module.Meta.C_RichSearchTreeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.Meta.C_Exception where
  showsPrec d (Curry.Module.Meta.C_ErrorCall x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("ErrorCall "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.Meta.C_PatternMatchFail x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("PatternMatchFail "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.Meta.C_AssertionFailed x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("AssertionFailed "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec d (Curry.Module.Meta.C_IOException x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("IOException "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ Curry.Module.Meta.C_PreludeFailed = Prelude.showString("PreludeFailed")
  showsPrec _ (Curry.Module.Meta.C_ExceptionOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.Meta.C_Seq t0) where
  showsPrec _ Curry.Module.Meta.C_Nil = Prelude.showString("Nil")
  showsPrec d (Curry.Module.Meta.C_Cons x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Cons "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec d (Curry.Module.Meta.C_Continued x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Continued "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.Meta.C_SeqOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Read t0) => Read (Curry.Module.Meta.C_RichSearchTree t0) where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Meta.C_RichFail(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Meta")("RichFail")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Meta.C_RichValue(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Meta")("RichValue")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Meta.C_RichChoice(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Meta")("RichChoice")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Meta.C_RichSuspend)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Meta")("RichSuspend")(r)])(r))))





instance Read Curry.Module.Meta.C_Exception where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Meta.C_ErrorCall(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Meta")("ErrorCall")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Meta.C_PatternMatchFail(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Meta")("PatternMatchFail")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Meta.C_AssertionFailed(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Meta")("AssertionFailed")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Meta.C_IOException(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Meta")("IOException")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Meta.C_PreludeFailed)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Meta")("PreludeFailed")(r)])(r)))))





instance (Read t0) => Read (Curry.Module.Meta.C_Seq t0) where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Meta.C_Nil)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Meta")("Nil")(r)])(r))((Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Meta.C_Cons(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Meta")("Cons")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Meta.C_Continued(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Meta")("Continued")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r)))





c_isFree :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Either t0 t0)
c_isFree x1 st = Curry.Module.Meta.c_headNormalFormIO(Curry.Module.Prelude.pf(Curry.Module.Meta.c_prim_isFree))(x1)(st)



c_throw :: (Curry t0) => Curry.Module.Meta.C_Exception -> Curry.RunTimeSystem.State -> t0
c_throw x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Meta.c_prim_throw))(x1)(st)



c_list :: (Curry t0) => (Curry.Module.Meta.C_Seq t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_list x1@Curry.Module.Meta.C_Nil st = Curry.Module.Prelude.List
c_list x1@(Curry.Module.Meta.C_Cons x2 x3) st = (Curry.Module.Prelude.:<)(x2)(Curry.Module.Meta.c_list(x3)(st))
c_list x1@(Curry.Module.Meta.C_Continued x4) st = Curry.Module.Meta.c_list(x4)(st)
c_list (Curry.Module.Meta.C_SeqOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Meta.c_list(x)(st))(i)(xs)(st)
c_list x st = Curry.RunTimeSystem.patternFail("Meta.list")(x)



c_interleave :: (Curry t0) => (Curry.Module.Meta.C_Seq t0) -> (Curry.Module.Meta.C_Seq t0) -> Curry.RunTimeSystem.State -> Curry.Module.Meta.C_Seq t0
c_interleave x1@Curry.Module.Meta.C_Nil x2 st = Curry.Module.Meta.C_Continued(x2)
c_interleave x1@(Curry.Module.Meta.C_Cons x3 x4) x2 st = Curry.Module.Meta.C_Cons(x3)(Curry.Module.Meta.c_interleave(x2)(x4)(st))
c_interleave x1@(Curry.Module.Meta.C_Continued x5) x2 st = Curry.Module.Meta.c_interleave_case_0(x1)(x5)(x2)(st)
c_interleave (Curry.Module.Meta.C_SeqOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Meta.c_interleave(x)(x2)(st))(i)(xs)(st)
c_interleave x x2 st = Curry.RunTimeSystem.patternFail("Meta.interleave")(x)



c_seq :: (Curry t0) => (Curry.Module.Prelude.C_SearchTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.Meta.C_Seq t0
c_seq x1@Curry.Module.Prelude.C_Fail st = Curry.Module.Meta.C_Nil
c_seq x1@(Curry.Module.Prelude.C_Value x2) st = Curry.Module.Meta.C_Cons(x2)(Curry.Module.Meta.C_Nil)
c_seq x1@(Curry.Module.Prelude.C_Choice x3) st = Curry.Module.Prelude.c_foldr1(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Meta.c_interleave))((Curry.Module.Prelude.:<)(Curry.Module.Meta.C_Nil)(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Meta.c_seq))(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.pf(Curry.Module.Meta.c_isValOrChoice))(x3)(st))(st)))(st)
c_seq x1@Curry.Module.Prelude.C_Suspend st = Curry.Module.Meta.C_Nil
c_seq (Curry.Module.Prelude.C_SearchTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Meta.c_seq(x)(st))(i)(xs)(st)
c_seq x st = Curry.RunTimeSystem.patternFail("Meta.seq")(x)



c_isValOrChoice :: (Curry t0) => (Curry.Module.Prelude.C_SearchTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isValOrChoice x1@Curry.Module.Prelude.C_Fail st = Curry.Module.Prelude.C_False
c_isValOrChoice x1@(Curry.Module.Prelude.C_Value x2) st = Curry.Module.Prelude.C_True
c_isValOrChoice x1@(Curry.Module.Prelude.C_Choice x3) st = Curry.Module.Prelude.C_True
c_isValOrChoice x1@Curry.Module.Prelude.C_Suspend st = Curry.Module.Prelude.C_False
c_isValOrChoice (Curry.Module.Prelude.C_SearchTreeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Meta.c_isValOrChoice(x)(st))(i)(xs)(st)
c_isValOrChoice x st = Curry.RunTimeSystem.patternFail("Meta.isValOrChoice")(x)



c_allValuesI :: (Curry t0) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.C_SearchTree t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0)
c_allValuesI st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Meta.c_list))(Curry.Module.Prelude.pf(Curry.Module.Meta.c_seq))(st)



c_interleave_case_0 x1 x5 x2@Curry.Module.Meta.C_Nil st = x1
c_interleave_case_0 x1 x5 x2@(Curry.Module.Meta.C_Cons x6 x7) st = Curry.Module.Meta.C_Cons(x6)(Curry.Module.Meta.c_interleave(x5)(x7)(st))
c_interleave_case_0 x1 x5 x2@(Curry.Module.Meta.C_Continued x8) st = Curry.Module.Meta.C_Continued(Curry.Module.Meta.c_interleave(x5)(x8)(st))
c_interleave_case_0 x1 x5 (Curry.Module.Meta.C_SeqOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Meta.c_interleave_case_0(x1)(x5)(x)(st))(i)(xs)(st)
c_interleave_case_0 x1 x5 x st = Curry.RunTimeSystem.patternFail("Meta.interleave_case_0")(x)



c_prim_isFree :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Either t0 t0)
c_prim_isFree x1 st = Curry.Module.Meta.prim_isFree(x1)(st)



c_headNormalFormIO :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1)) -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1
c_headNormalFormIO x1 x2 st = Curry.Module.Meta.headNormalFormIO(x1)(x2)(st)



c_searchTree :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_SearchTree t0
c_searchTree x1 st = Curry.Module.Meta.searchTree(x1)(st)



c_gnfIO :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_gnfIO x1 st = Curry.Module.Meta.gnfIO(x1)(st)



c_ghnfIO :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_ghnfIO x1 st = Curry.Module.Meta.ghnfIO(x1)(st)



c_nfIO :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_nfIO x1 st = Curry.Module.Meta.nfIO(x1)(st)



c_hnfIO :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_hnfIO x1 st = Curry.Module.Meta.hnfIO(x1)(st)



c_getRichSearchTree :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Meta.C_RichSearchTree t0)
c_getRichSearchTree x1 st = Curry.Module.Meta.getRichSearchTree(x1)(st)



c_richSearchTree :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Meta.C_RichSearchTree t0
c_richSearchTree x1 st = Curry.Module.Meta.richSearchTree(x1)(st)



c_parallelSearch :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List t0)
c_parallelSearch x1 st = Curry.Module.Meta.parallelSearch(x1)(st)



c_cover :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> t0
c_cover x1 st = Curry.Module.Meta.cover(x1)(st)



c_st :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_SearchTree t0
c_st x1 st = Curry.Module.Meta.st(x1)(st)



c_richST :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Meta.C_RichSearchTree t0
c_richST x1 st = Curry.Module.Meta.richST(x1)(st)



c_ors :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> t0
c_ors x1 st = Curry.Module.Meta.ors(x1)(st)



c_prim_throw :: (Curry t0) => Curry.Module.Meta.C_Exception -> Curry.RunTimeSystem.State -> t0
c_prim_throw x1 st = Curry.Module.Meta.prim_throw(x1)(st)


