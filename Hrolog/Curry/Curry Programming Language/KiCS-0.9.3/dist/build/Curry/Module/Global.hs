{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Global (module Curry.Module.Global) where

import Curry.RunTimeSystem
import Curry.Module.Prelude



-- begin included



import System.IO.Unsafe
import qualified Data.IORef as Ref

type C_Global t0 = Prim (Ref.IORef t0)

global :: (Curry t0) => t0 -> C_GlobalSpec -> Result (C_Global t0)
global x spec = ref `seq` (\ _ -> PrimValue ref)
  where ref = unsafePerformIO (Ref.newIORef x) 

prim_readGlobal :: (Curry t0) => C_Global t0 -> Result (C_IO t0)
prim_readGlobal  = prim_readIORef

prim_writeGlobal :: (Curry t0) => C_Global t0 -> t0 -> Result (C_IO T0)
prim_writeGlobal  = prim_writeIORef

----------------------
-- preparing io ref
----------------------

type C_IORef a = Prim (Ref.IORef a)

instance Show (Ref.IORef a) where
  show _ = "IOREF"

instance Read (Ref.IORef a) where
  readsPrec = error "reading IOREF"

instance Generate (Ref.IORef a) where
  genFree    = error "free variable of type IOExts.IORef"
  maxArity _ = error "free variable of type IOExts.IORef"

newIORef :: Curry t0 => t0 -> Result (C_IO (C_IORef t0))
newIORef x = ioFunc0 (Ref.newIORef x) 

prim_readIORef :: Curry t0 => C_IORef t0 -> Result (C_IO t0)
prim_readIORef (PrimValue ref) _ = 
   C_IO (\ _ -> do 
           v <- Ref.readIORef ref 
           Prelude.return (IOVal v))

prim_writeIORef :: Curry t0 => C_IORef t0 -> t0 -> Result (C_IO T0)
prim_writeIORef (PrimValue ref) x = ioFunc0 (Ref.writeIORef ref x) 


-- end included

data C_GlobalSpec = C_Temporary
  | C_Persistent (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_GlobalSpecFail Curry.RunTimeSystem.C_Exceptions
  | C_GlobalSpecOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.Global.C_GlobalSpec)

instance BaseCurry Curry.Module.Global.C_GlobalSpec where
  nf f (Curry.Module.Global.C_Persistent x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Global.C_Persistent(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Global.C_Persistent x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Global.C_Persistent(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Global.C_GlobalSpecOr(Curry.RunTimeSystem.mkRef(r)(1)(i))([Curry.Module.Global.C_Temporary,Curry.Module.Global.C_Persistent(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(1)

  failed  = Curry.Module.Global.C_GlobalSpecFail

  branching  = Curry.Module.Global.C_GlobalSpecOr

  consKind (Curry.Module.Global.C_GlobalSpecOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Global.C_GlobalSpecFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Global.C_GlobalSpecFail x) = x

  orRef (Curry.Module.Global.C_GlobalSpecOr x _) = x

  branches (Curry.Module.Global.C_GlobalSpecOr _ x) = x





instance Curry Curry.Module.Global.C_GlobalSpec where
  strEq Curry.Module.Global.C_Temporary Curry.Module.Global.C_Temporary st = Curry.Module.Prelude.strEqSuccess
  strEq (Curry.Module.Global.C_Persistent x1) (Curry.Module.Global.C_Persistent y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.Global.C_Temporary Curry.Module.Global.C_Temporary st = Curry.Module.Prelude.C_True
  eq (Curry.Module.Global.C_Persistent x1) (Curry.Module.Global.C_Persistent y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.Global.C_Temporary st = Curry.Module.Global.C_Temporary
  propagate f (Curry.Module.Global.C_Persistent x1) st = Curry.Module.Global.C_Persistent(f((0::Int))(x1)(st))

  foldCurry f c Curry.Module.Global.C_Temporary st = c
  foldCurry f c (Curry.Module.Global.C_Persistent x1) st = f(x1)(c)(st)

  typeName _ = "GlobalSpec"

  showQ _ Curry.Module.Global.C_Temporary = Prelude.showString("Global.Temporary")
  showQ d (Curry.Module.Global.C_Persistent x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Global.Persistent "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.Global.C_GlobalSpecOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.Global.C_GlobalSpec where
  showsPrec _ Curry.Module.Global.C_Temporary = Prelude.showString("Temporary")
  showsPrec d (Curry.Module.Global.C_Persistent x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Persistent "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.Global.C_GlobalSpecOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Read Curry.Module.Global.C_GlobalSpec where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Global.C_Temporary)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Global")("Temporary")(r)])(r))(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Global.C_Persistent(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Global")("Persistent")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r))





c_readGlobal :: (Curry t0) => (Curry.Module.Global.C_Global t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_readGlobal x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.Global.c_prim_readGlobal))(x1)(st)



c_writeGlobal :: (Curry t0) => (Curry.Module.Global.C_Global t0) -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_writeGlobal x1 x2 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Global.c_prim_writeGlobal))(x1)(st))(x2)(st)



c_global :: (Curry t0) => t0 -> Curry.Module.Global.C_GlobalSpec -> Curry.RunTimeSystem.State -> Curry.Module.Global.C_Global t0
c_global x1 x2 st = Curry.Module.Global.global(x1)(x2)(st)



c_prim_readGlobal :: (Curry t0) => (Curry.Module.Global.C_Global t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_prim_readGlobal x1 st = Curry.Module.Global.prim_readGlobal(x1)(st)



c_prim_writeGlobal :: (Curry t0) => (Curry.Module.Global.C_Global t0) -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_prim_writeGlobal x1 x2 st = Curry.Module.Global.prim_writeGlobal(x1)(x2)(st)


