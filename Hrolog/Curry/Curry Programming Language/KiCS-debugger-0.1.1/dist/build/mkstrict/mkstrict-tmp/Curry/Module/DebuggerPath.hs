{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.DebuggerPath (module Curry.Module.DebuggerPath) where

import Curry.RunTimeSystem
import Curry.Module.Distribution
import Curry.Module.FileGoodies
import Curry.Module.Prelude



-- begin included



import Curry.Files.KiCSDebugPath

debuggerLibPath :: Result (C_IO C_String)
debuggerLibPath = ioFunc0 getOracleLibDir

-- end included

c_getDebugLibPath :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_getDebugLibPath st = Curry.Module.Prelude.op_62_62_61(Curry.Module.DebuggerPath.c_debuggerLibPath(st))(Curry.Module.Prelude.pf(Curry.Module.DebuggerPath.c_getDebugLibPath'46_'35lambda2))(st)



c_getDebugLibPath'46_'35lambda2 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_getDebugLibPath'46_'35lambda2 x1 st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.FileGoodies.c_separatorChar(st))(Curry.Module.Prelude.List))(st))(st)



c_getDebugLoadPath :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getDebugLoadPath st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_getLoadPath(st))(Curry.Module.Prelude.pf(Curry.Module.DebuggerPath.c_getDebugLoadPath'46_'35lambda3))(st)



c_getDebugLoadPath'46_'35lambda3 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getDebugLoadPath'46_'35lambda3 x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.DebuggerPath.c_getDebugLibPath(st))(Curry.Module.Prelude.pf(Curry.Module.DebuggerPath.c_getDebugLoadPath'46_'35lambda3'46_'35lambda4(x1)))(st)



c_getDebugLoadPath'46_'35lambda3'46_'35lambda4 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getDebugLoadPath'46_'35lambda3'46_'35lambda4 x1 x2 st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(st))(st)



c_getDebugFrontendParams :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Distribution.C_FrontendParams
c_getDebugFrontendParams st = Curry.Module.Prelude.op_62_62_61(Curry.Module.DebuggerPath.c_getDebugLoadPath(st))(Curry.Module.Prelude.pf(Curry.Module.DebuggerPath.c_getDebugFrontendParams'46_'35lambda5))(st)



c_getDebugFrontendParams'46_'35lambda5 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Distribution.C_FrontendParams
c_getDebugFrontendParams'46_'35lambda5 x1 st = Curry.Module.Prelude.c_return(Curry.Module.Distribution.c_setFullPath(x1)(Curry.Module.Distribution.c_defaultParams(st))(st))(st)



c_debuggerLibPath :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_debuggerLibPath st = Curry.Module.DebuggerPath.debuggerLibPath(st)


