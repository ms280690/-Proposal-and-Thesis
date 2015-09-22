{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.System (module Curry.Module.System) where

import Curry.RunTimeSystem
import Curry.Module.Global
import Curry.Module.Prelude



-- begin included



import qualified System.Environment as SE
import qualified System.CPUTime as SC
import System.Cmd
import System.Exit
import qualified Network.BSD as NB
import System.Posix.Process

instance ConvertCH C_Int ExitCode where
   toCurry ExitSuccess = toCurry (0::Integer)
   toCurry (ExitFailure i) = toCurry i

   fromCurry i = if hi Prelude.== 0 then ExitSuccess else ExitFailure hi
     where hi = fromCurry i

getCPUTime :: Result (C_IO C_Int)
getCPUTime = ioFunc0 (SC.getCPUTime Prelude.>>= Prelude.return . (`div` 1000000000))

getElapsedTime :: Result (C_IO C_Int)
getElapsedTime = error "getElapsedTime not provided"

getArgs :: Result (C_IO (List (List C_Char)))
getArgs = ioFunc0 SE.getArgs

prim_getEnviron :: (List C_Char) -> Result (C_IO (List C_Char))
prim_getEnviron = 
  ioFunc1 (\s -> do
             env <- SE.getEnvironment
             case Prelude.lookup s env of
              Nothing -> Prelude.return ""
              Just v  -> Prelude.return v)

getHostname :: Result (C_IO (List C_Char))
getHostname = ioFunc0 NB.getHostName

getPID :: Result (C_IO C_Int)
getPID = ioFunc0 (do
                    pid <- getProcessID 
                    Prelude.return (toInteger pid))

getProgName :: Result (C_IO (List C_Char))
getProgName = ioFunc0 (Curry.RunTimeSystem.getProgName)
  -- conform with haskell would be: SE.getProgName

prim_system :: (List C_Char) -> Result (C_IO C_Int)
prim_system = ioFunc1 system

prim_sleep :: C_Int -> Result (C_IO T0)
prim_sleep = ioFunc1 (\ t -> do
                          system ("sleep "++Prelude.show (t::Integer)) 
                          Prelude.return ())

prim_exitWith :: Curry a => C_Int -> Result (C_IO a)
prim_exitWith e _ = C_IO (\ _ -> do
  ex <- exitWith (fromCurry e) 
  Prelude.return (IOVal ex))



-- end included

c_environ :: Curry.RunTimeSystem.State -> Curry.Module.Global.C_Global (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_environ  = let {st = Prelude.Nothing} in Curry.Module.Global.c_global(Curry.Module.Prelude.List)(Curry.Module.Global.C_Temporary)



c_getEnviron :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_getEnviron x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Global.c_readGlobal(Curry.Module.System.c_environ(st))(st))(Curry.Module.Prelude.pf(Curry.Module.System.c_getEnviron'46_'35lambda2(x1)))(st)



c_getEnviron'46_'35lambda2 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_getEnviron'46_'35lambda2 x1 x2 st = Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.System.c_prim_getEnviron))(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Prelude.c_lookup(x1)(x2)(st))(st)



c_setEnviron :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_setEnviron x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Global.c_readGlobal(Curry.Module.System.c_environ(st))(st))(Curry.Module.Prelude.pf(Curry.Module.System.c_setEnviron'46_'35lambda3(x1)(x2)))(st)



c_setEnviron'46_'35lambda3 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_setEnviron'46_'35lambda3 x1 x2 x3 st = Curry.Module.Global.c_writeGlobal(Curry.Module.System.c_environ(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(x1)(x2))(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_47_61))(x1)))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_fst))(st))(x3)(st)))(st)



c_unsetEnviron :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_unsetEnviron x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Global.c_readGlobal(Curry.Module.System.c_environ(st))(st))(Curry.Module.Prelude.pf(Curry.Module.System.c_unsetEnviron'46_'35lambda4(x1)))(st)



c_unsetEnviron'46_'35lambda4 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_unsetEnviron'46_'35lambda4 x1 x2 st = Curry.Module.Global.c_writeGlobal(Curry.Module.System.c_environ(st))(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_47_61))(x1)))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_fst))(st))(x2)(st))(st)



c_system :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Int
c_system x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Global.c_readGlobal(Curry.Module.System.c_environ(st))(st))(Curry.Module.Prelude.pf(Curry.Module.System.c_system'46_'35lambda5(x1)))(st)



c_system'46set'4614 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_system'46set'4614 x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))(Curry.Module.Prelude.op_43_43(x2)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(st))(st))(st))(st)
c_system'46set'4614 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.System.c_system'46set'4614(x)(st))(i)(xs)(st)
c_system'46set'4614 x st = Curry.RunTimeSystem.patternFail("System.system.set.14")(x)



c_system'46_'35lambda5 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Int
c_system'46_'35lambda5 x1 x2 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.System.c_prim_system))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.System.c_system'46set'4614))(st))(x2)(st))(x1)(st))(st)



c_exitWith :: (Curry t0) => Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_exitWith x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.System.c_prim_exitWith))(x1)(st)



c_sleep :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_sleep x1 st = Curry.Module.Prelude.op_36_35(Curry.Module.Prelude.pf(Curry.Module.System.c_prim_sleep))(x1)(st)



c_getCPUTime :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Int
c_getCPUTime st = Curry.Module.System.getCPUTime(st)



c_getElapsedTime :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Int
c_getElapsedTime st = Curry.Module.System.getElapsedTime(st)



c_getArgs :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getArgs st = Curry.Module.System.getArgs(st)



c_prim_getEnviron :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_prim_getEnviron x1 st = Curry.Module.System.prim_getEnviron(x1)(st)



c_getHostname :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_getHostname st = Curry.Module.System.getHostname(st)



c_getPID :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Int
c_getPID st = Curry.Module.System.getPID(st)



c_getProgName :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_getProgName st = Curry.Module.System.getProgName(st)



c_prim_system :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Int
c_prim_system x1 st = Curry.Module.System.prim_system(x1)(st)



c_prim_exitWith :: (Curry t0) => Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_prim_exitWith x1 st = Curry.Module.System.prim_exitWith(x1)(st)



c_prim_sleep :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_prim_sleep x1 st = Curry.Module.System.prim_sleep(x1)(st)


