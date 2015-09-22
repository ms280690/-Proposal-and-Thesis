{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleDistribution (module Curry.Module.OracleDistribution) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.Distribution
import Curry.Module.Char
import Curry.Module.FileGoodies
import Curry.Module.IO
import Curry.Module.List
import Curry.Module.Prelude
import Curry.Module.PropertyFile
import Curry.Module.System
import Curry.Module.Directory
import Curry.Module.OracleChar
import Curry.Module.OracleFileGoodies
import Curry.Module.OracleIO
import Curry.Module.OracleList
import Curry.Module.OraclePrelude
import Curry.Module.OraclePropertyFile
import Curry.Module.OracleSystem
import Curry.Module.OracleDirectory



-- begin included



-- end included

c_rcFileName :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_rcFileName x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))))))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleSystem.c_getEnviron((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('H'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))(Curry.Module.Prelude.List)))))(x1)(st))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_return))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_43_43))(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleDistribution.c_curryCompiler(x2)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))(Curry.Module.Prelude.List)))(x3)(st))(x4)(st))))))(x5)(st))(x6)(st))(st)



c_rcFileContents :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))))
c_rcFileContents x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDistribution.c_rcFileName(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePropertyFile.c_readPropertyFile))))(x2)(st))(st)



c_getRcVar :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_getRcVar x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDistribution.c_getRcVars((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(x1)(st))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_return))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_head))))(x3)(st))(x4)(st))(st)



c_getRcVars :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))))
c_getRcVars x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDistribution.c_rcFileContents(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleDistribution.c_getRcVars'46_'35lambda2(x2)))))(x3)(st))(st)



c_getRcVars'46_'35lambda2 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))))
c_getRcVars'46_'35lambda2 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.c_return(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.c_lookup))(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleDistribution.c_getRcVars'46_'35lambda2'46_'35lambda3))))(x3)(x1)(st))))))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleChar.c_toLower))))))))(x2)(x4)(st))(x5)(st))(x6)(st))(st)



c_getRcVars'46_'35lambda2'46_'35lambda3 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_getRcVars'46_'35lambda2'46_'35lambda3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_25(x2)(x1)(st))(st)



c_currySubdir :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_currySubdir x1 st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st)



c_inCurrySubdir :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_inCurrySubdir x2 x1 st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(let {x3 = Curry.Module.OracleFileGoodies.c_splitDirectoryBaseName(x2)(x1)(st)} in let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleDistribution.c__case_24(Curry.Module.OracleDistribution.c_inCurrySubdir'46_'35selFP3'35base(x3)(x6)(st))(x8)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleDistribution.c_currySubdir(x9)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.OracleDistribution.c_inCurrySubdir'46_'35selFP4'35file(x3)(x7)(st)))(x10)(st))(x11)(st))(st))(st))(st))(st)



c_inCurrySubdir'46_'35selFP3'35base :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_inCurrySubdir'46_'35selFP3'35base x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_21(x2)(x1)(st))(st)



c_inCurrySubdir'46_'35selFP4'35file :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_inCurrySubdir'46_'35selFP4'35file x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_20(x2)(x1)(st))(st)



c_addCurrySubdir :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_addCurrySubdir x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.OracleDistribution.c_currySubdir(x1)(st)))(x3)(st))(st)



c_getSysLibPath :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_getSysLibPath x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List)))(Curry.Module.OracleDistribution.c__case_19(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OracleDistribution.c_curryCompiler(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))(x2)(st))(x3)(st))(st)



c_getSysLibPath'46_'35lambda5 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_getSysLibPath'46_'35lambda5 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_return))))(Curry.Module.OracleDistribution.c__case_17(x2)(Curry.Module.OraclePrelude.c_null(x2)(x1)(st))(x3)(st))(x4)(st))(st)



c_lookupFileInLoadPath :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_lookupFileInLoadPath x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDistribution.c_getLoadPathForFile(x2)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFileGoodies.c_lookupFileInPath(Curry.Module.OracleFileGoodies.c_baseName(x2)(x3)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))))))(x4)(st))(st)



c_findFileInLoadPath :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_findFileInLoadPath x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDistribution.c_getLoadPathForFile(x2)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFileGoodies.c_getFileInPath(Curry.Module.OracleFileGoodies.c_baseName(x2)(x3)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))))))(x4)(st))(st)



c_readFirstFileInLoadPath :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_readFirstFileInLoadPath x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDistribution.c_findFileInLoadPath(x2)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_readFile))))(x3)(st))(st)



c_getLoadPath :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_getLoadPath x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c_getLoadPathForFile((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))(Curry.Module.Prelude.List))))(x1)(st))(st)



c_getLoadPathForFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_getLoadPathForFile x2 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDistribution.c_getSysLibPath(x4)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleDistribution.c_getLoadPathForFile'46_'35lambda7(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleDistribution.c_getLoadPathForFile'46_'35lambda6))))(x1)(st))(x2)))))(x5)(st))(st))(st)



c_getLoadPathForFile'46_'35lambda6 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_getLoadPathForFile'46_'35lambda6 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(Curry.Module.OracleDistribution.c_addCurrySubdir(x2)(x1)(st))(Curry.Module.Prelude.List)))(st)



c_getLoadPathForFile'46_'35lambda7 :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_getLoadPathForFile'46_'35lambda7 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDistribution.c_getRcVar((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))))))(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleDistribution.c_getLoadPathForFile'46_'35lambda7'46_'35lambda8(x2)(x3)(x4)))))(x5)(st))(st)



c_getLoadPathForFile'46_'35lambda7'46_'35lambda8 :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_getLoadPathForFile'46_'35lambda7'46_'35lambda8 x2 x3 x4 x5 x1 st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))))(Curry.Module.OracleDistribution.c__case_16(x2)(x4)(x5)(Curry.Module.OracleFileGoodies.c_dirName(x3)(x1)(st))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OracleDistribution.c_curryCompiler(x7)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))(x8)(st))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OracleDistribution.c_curryCompiler(x9)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))(x10)(st))(x11)(st))(x12)(st))(st))(st)



c_getLoadPathForFile'46_'35lambda7'46_'35lambda8'46_'35lambda9 :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_getLoadPathForFile'46_'35lambda7'46_'35lambda8'46_'35lambda9 x2 x3 x4 x5 x6 x1 st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.c_return(Curry.Module.Oracle.c_apply(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleDistribution.c__case_15(x6)(Curry.Module.OraclePrelude.c_null(x6)(x8)(st))(x9)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_maybe(Curry.Module.Prelude.List)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFileGoodies.c_splitPath))))(x4)(x1)(st))(x5)(x10)(st))(x11)(st)))(x12)(st))(x13)(st))(st))(st)



c_defaultParams :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Distribution.C_FrontendParams
c_defaultParams x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Distribution.C_FrontendParams(Curry.Module.Prelude.C_False)(Curry.Module.Prelude.C_Nothing)(Curry.Module.Prelude.C_Nothing)(Curry.Module.Prelude.C_Nothing))(st)



c_setQuiet :: Curry.Module.Prelude.C_Bool -> Curry.Module.Distribution.C_FrontendParams -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Distribution.C_FrontendParams
c_setQuiet x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_14(x2)(x3)(x1)(st))(st)



c_setFullPath :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.Distribution.C_FrontendParams -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Distribution.C_FrontendParams
c_setFullPath x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_13(x2)(x3)(x1)(st))(st)



c_setOutfile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Distribution.C_FrontendParams -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Distribution.C_FrontendParams
c_setOutfile x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_12(x2)(x3)(x1)(st))(st)



c_setLogfile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Distribution.C_FrontendParams -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Distribution.C_FrontendParams
c_setLogfile x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_11(x2)(x3)(x1)(st))(st)



c_quiet :: Curry.Module.Distribution.C_FrontendParams -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_quiet x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_10(x2)(x1)(st))(st)



c_fullPath :: Curry.Module.Distribution.C_FrontendParams -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_fullPath x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_9(x2)(x1)(st))(st)



c_outfile :: Curry.Module.Distribution.C_FrontendParams -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_outfile x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_8(x2)(x1)(st))(st)



c_logfile :: Curry.Module.Distribution.C_FrontendParams -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_logfile x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_7(x2)(x1)(st))(st)



c_callFrontend :: Curry.Module.Distribution.C_FrontendTarget -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))))
c_callFrontend x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleDistribution.c_callFrontendWithParams(x2)(Curry.Module.OracleDistribution.c_defaultParams(x1)(st))))))(st)



c_callFrontendWithParams :: Curry.Module.Distribution.C_FrontendTarget -> Curry.Module.Distribution.C_FrontendParams -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_callFrontendWithParams x2 x3 x4 x1 st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))))(let {x5 = Curry.Module.OracleDistribution.c__case_3(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OracleDistribution.c_curryCompiler(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))(x9)(st))(x10)(st)} in let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List)))))))))(Curry.Module.CEventOracle.c_replace(x21)(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDistribution.c__case_4(x3)(x5)(x11)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleDistribution.c_callFrontendWithParams'46_'35lambda13(x3)(x4)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleDistribution.c__case_6(x3)(Curry.Module.OracleDistribution.c__case_5(x5)(x12)(st))(Curry.Module.OracleDistribution.c_quiet(x3)(x13)(st))(x14)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_maybe(Curry.Module.Prelude.List)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))(Curry.Module.OracleDistribution.c_outfile(x3)(x15)(st))(x16)(st))(Curry.Module.OraclePrelude.c_maybe(Curry.Module.Prelude.List)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleDistribution.c_callFrontendWithParams'46_'35lambda12))))(Curry.Module.OracleDistribution.c_fullPath(x3)(x17)(st))(x18)(st))(x19)(st))(x20)(st))(x2)))))(x21)(st))(st))(st))(st))(st))(st)



c_callFrontendWithParams'46_'35lambda10 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_callFrontendWithParams'46_'35lambda10 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDistribution.c_getFrontendCall(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleDistribution.c_callFrontendWithParams'46_'35lambda10'46_'35lambda11(x2)))))(x3)(st))(st)



c_callFrontendWithParams'46_'35lambda10'46_'35lambda11 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_callFrontendWithParams'46_'35lambda10'46_'35lambda11 x2 x3 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))))(Curry.Module.CEventOracle.c_replace(x7)(Curry.Module.OraclePrelude.c_return(Curry.Module.OraclePrelude.op_43_43(x3)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))(Curry.Module.Prelude.List))))))))(x1)(st))(x2)(x5)(st))(x6)(st))(x7)(st))(st))(st)



c_callFrontendWithParams'46showFrontendTarget'4684 :: Curry.Module.Distribution.C_FrontendTarget -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_callFrontendWithParams'46showFrontendTarget'4684 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_1(x2)(x1)(st))(st)



c_callFrontendWithParams'46_'35lambda12 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_callFrontendWithParams'46_'35lambda12 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))(Curry.Module.OraclePrelude.c_concat(Curry.Module.OracleList.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(x2)(x1)(st))(x3)(st))(x4)(st))(st)



c_callFrontendWithParams'46_'35lambda13 :: Curry.Module.Distribution.C_FrontendParams -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Distribution.C_FrontendTarget -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_callFrontendWithParams'46_'35lambda13 x2 x3 x4 x5 x6 x1 st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))(let {x7 = Curry.Module.OraclePrelude.c_maybe(Curry.Module.Prelude.List)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_id))))(Curry.Module.OracleDistribution.c_logfile(x2)(x1)(st))(x9)(st)} in let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List)))))))(let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_62_62(Curry.Module.OracleDistribution.c__case_0(x7)(Curry.Module.OraclePrelude.op_43_43(x6)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleDistribution.c_callFrontendWithParams'46showFrontendTarget'4684(x5)(x10)(st))(Curry.Module.OraclePrelude.op_43_43(x4)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(x3)(x11)(st))(x12)(st))(x13)(st))(x14)(st))(x15)(st))(Curry.Module.OraclePrelude.c_null(x7)(x16)(st))(x17)(st))(Curry.Module.OraclePrelude.c_return(Curry.Module.Prelude.T0)(x18)(st))(x19)(st))(st))(st))(st)



c_rcErr :: (Curry t0) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))
c_rcErr x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_62_62(Curry.Module.OracleIO.c_hPutStrLn(Curry.Module.OracleIO.c_stderr(x1)(st))(Curry.Module.OraclePrelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))))))))))))))))(x4)(st))(x5)(st))(Curry.Module.OraclePrelude.c_return(x3)(x6)(st))(x7)(st))(st)



c__case_0 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_0_case__25(x1)(x7)(x8)(x9)(st))(st)



c__case_1 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_1_case__24(x1)(x2)(st))(st)



c__case_3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_3_case__23(x1)(x2)(st))(st)



c__case_2 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_2_case__22(x1)(x2)(st))(st)



c__case_4 x3 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_4_case__21(x1)(x3)(x5)(st))(st)



c__case_5 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_5_case__20(x1)(x5)(st))(st)



c__case_6 x3 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_6_case__19(x1)(x7)(x8)(st))(st)



c__case_7 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_7_case__18(x1)(x2)(st))(st)



c__case_8 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_8_case__17(x1)(x2)(st))(st)



c__case_9 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_9_case__16(x1)(x2)(st))(st)



c__case_10 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_10_case__15(x1)(x2)(st))(st)



c__case_11 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_11_case__14(x1)(x2)(x3)(st))(st)



c__case_12 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_12_case__13(x1)(x2)(x3)(st))(st)



c__case_13 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_13_case__12(x1)(x2)(x3)(st))(st)



c__case_14 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_14_case__11(x1)(x2)(x3)(st))(st)



c__case_15 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_15_case__10(x1)(x6)(x7)(st))(st)



c__case_16 x2 x4 x5 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_16_case__9(x1)(x2)(x4)(x5)(x6)(x7)(st))(st)



c__case_17 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_17_case__8(x1)(x2)(x3)(st))(st)



c__case_19 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_19_case__7(x1)(x2)(st))(st)



c__case_18 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_18_case__6(x1)(x2)(st))(st)



c__case_20 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_20_case__5(x1)(x2)(st))(st)



c__case_21 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_21_case__4(x1)(x2)(st))(st)



c__case_24 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_24_case__3(x1)(x4)(st))(st)



c__case_23 x4 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_23_case__2(x1)(x4)(x7)(x8)(st))(st)



c__case_22 x4 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_22_case__1(x1)(x4)(x7)(st))(st)



c__case_25 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_25_case__0(x1)(x2)(st))(st)



c_curryCompiler :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_curryCompiler x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Distribution.c_curryCompiler(st))(st)



c_curryCompilerMajorVersion :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_curryCompilerMajorVersion x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Distribution.c_curryCompilerMajorVersion(st))(st)



c_curryCompilerMinorVersion :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_curryCompilerMinorVersion x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Distribution.c_curryCompilerMinorVersion(st))(st)



c_curryRuntime :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_curryRuntime x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Distribution.c_curryRuntime(st))(st)



c_curryRuntimeMajorVersion :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_curryRuntimeMajorVersion x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Distribution.c_curryRuntimeMajorVersion(st))(st)



c_curryRuntimeMinorVersion :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_curryRuntimeMinorVersion x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Distribution.c_curryRuntimeMinorVersion(st))(st)



c_getStdLibDir :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_getStdLibDir x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Oracle.c_safeIOResult(Curry.Module.Distribution.c_getStdLibDir(st))(st))))(st)



c_getFrontendCall :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_getFrontendCall x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Oracle.c_safeIOResult(Curry.Module.Distribution.c_getFrontendCall(st))(st))))(st)



c_installDir :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_installDir x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Distribution.c_installDir(st))(st)



c__case_25_case__0 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.T2(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleChar.c_toLower))))(x3)(x1)(st))(x4))(st)
c__case_25_case__0 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_25_case__0(x1)(x)(st))(i)(xs)(st)
c__case_25_case__0 x1 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_25_case__0")(x)



c__case_22_case__1 x1 x4 x7@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_22_case__1 x1 x4 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_43_43(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.Prelude.List))(x1)(st))(st)
c__case_22_case__1 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_22_case__1(x1)(x4)(x)(st))(i)(xs)(st)
c__case_22_case__1 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_22_case__1")(x)



c__case_23_case__2 x1 x4 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c__case_22(x4)(x7)(x1)(st))(st)
c__case_23_case__2 x1 x4 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_43_43(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.Prelude.List))(x1)(st))(st)
c__case_23_case__2 x1 x4 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_23_case__2(x1)(x4)(x7)(x)(st))(i)(xs)(st)
c__case_23_case__2 x1 x4 x7 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_23_case__2")(x)



c__case_24_case__3 x1 x4@((Curry.Module.Prelude.:<) x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleDistribution.c__case_23(x4)(x6)(x7)(Curry.Module.OraclePrelude.op_61_61(x6)(Curry.Module.Prelude.C_Char('.'))(x1)(st))(x8)(st))(st)
c__case_24_case__3 x1 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_43_43(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.Prelude.List))(x1)(st))(st)
c__case_24_case__3 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_24_case__3(x1)(x)(st))(i)(xs)(st)
c__case_24_case__3 x1 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_24_case__3")(x)



c__case_21_case__4 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_21_case__4 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_21_case__4(x1)(x)(st))(i)(xs)(st)
c__case_21_case__4 x1 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_21_case__4")(x)



c__case_20_case__5 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_20_case__5 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_20_case__5(x1)(x)(st))(i)(xs)(st)
c__case_20_case__5 x1 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_20_case__5")(x)



c__case_18_case__6 x1 x2@Curry.Module.Prelude.C_True st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDistribution.c_getStdLibDir(x1)(st))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_return))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partCons))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pc))((Curry.Module.Prelude.:<)))(st))(Curry.Module.Prelude.List)))))(x3)(st))(x4)(st))(st)
c__case_18_case__6 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))))))))))))))))))(x1)(st))(st)
c__case_18_case__6 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_18_case__6(x1)(x)(st))(i)(xs)(st)
c__case_18_case__6 x1 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_18_case__6")(x)



c__case_19_case__7 x1 x2@Curry.Module.Prelude.C_True st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleSystem.c_getEnviron((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('K'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('B'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('H'))(Curry.Module.Prelude.List)))))))))))))(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleDistribution.c_getSysLibPath'46_'35lambda5))))(x3)(st))(st)
c__case_19_case__7 x1 x2@Curry.Module.Prelude.C_False st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OracleDistribution.c__case_18(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OracleDistribution.c_curryCompiler(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))(x4)(st))(x5)(st))(st)
c__case_19_case__7 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_19_case__7(x1)(x)(st))(i)(xs)(st)
c__case_19_case__7 x1 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_19_case__7")(x)



c__case_17_case__8 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleDistribution.c_installDir(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))(Curry.Module.Prelude.List)))))(x4)(st))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleDistribution.c_installDir(x5)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))(Curry.Module.Prelude.List))))))))))(x6)(st))(Curry.Module.Prelude.List)))(st)
c__case_17_case__8 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c_splitPath(x2)(x1)(st))(st)
c__case_17_case__8 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_17_case__8(x1)(x2)(x)(st))(i)(xs)(st)
c__case_17_case__8 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_17_case__8")(x)



c__case_16_case__9 x1 x2 x4 x5 x6 x7@Curry.Module.Prelude.C_True st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleSystem.c_getEnviron((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('U'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('Y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('H'))(Curry.Module.Prelude.List))))))))))(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleDistribution.c_getLoadPathForFile'46_'35lambda7'46_'35lambda8'46_'35lambda9(x2)(x6)(x5)(x4)))))(x8)(st))(st)
c__case_16_case__9 x1 x2 x4 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))))))))))(x1)(st))(st)
c__case_16_case__9 x1 x2 x4 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_16_case__9(x1)(x2)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_16_case__9 x1 x2 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_16_case__9")(x)



c__case_15_case__10 x1 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_15_case__10 x1 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c_splitPath(x6)(x1)(st))(st)
c__case_15_case__10 x1 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_15_case__10(x1)(x6)(x)(st))(i)(xs)(st)
c__case_15_case__10 x1 x6 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_15_case__10")(x)



c__case_14_case__11 x1 x2 x3@(Curry.Module.Distribution.C_FrontendParams x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Distribution.C_FrontendParams(x2)(x5)(x6)(x7))(st)
c__case_14_case__11 x1 x2 (Curry.Module.Distribution.C_FrontendParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_14_case__11(x1)(x2)(x)(st))(i)(xs)(st)
c__case_14_case__11 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_14_case__11")(x)



c__case_13_case__12 x1 x2 x3@(Curry.Module.Distribution.C_FrontendParams x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Distribution.C_FrontendParams(x4)(Curry.Module.Prelude.C_Just(x2))(x6)(x7))(st)
c__case_13_case__12 x1 x2 (Curry.Module.Distribution.C_FrontendParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_13_case__12(x1)(x2)(x)(st))(i)(xs)(st)
c__case_13_case__12 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_13_case__12")(x)



c__case_12_case__13 x1 x2 x3@(Curry.Module.Distribution.C_FrontendParams x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Distribution.C_FrontendParams(x4)(x5)(Curry.Module.Prelude.C_Just(x2))(x7))(st)
c__case_12_case__13 x1 x2 (Curry.Module.Distribution.C_FrontendParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_12_case__13(x1)(x2)(x)(st))(i)(xs)(st)
c__case_12_case__13 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_12_case__13")(x)



c__case_11_case__14 x1 x2 x3@(Curry.Module.Distribution.C_FrontendParams x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Distribution.C_FrontendParams(x4)(x5)(x6)(Curry.Module.Prelude.C_Just(x2)))(st)
c__case_11_case__14 x1 x2 (Curry.Module.Distribution.C_FrontendParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_11_case__14(x1)(x2)(x)(st))(i)(xs)(st)
c__case_11_case__14 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_11_case__14")(x)



c__case_10_case__15 x1 x2@(Curry.Module.Distribution.C_FrontendParams x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_10_case__15 x1 (Curry.Module.Distribution.C_FrontendParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_10_case__15(x1)(x)(st))(i)(xs)(st)
c__case_10_case__15 x1 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_10_case__15")(x)



c__case_9_case__16 x1 x2@(Curry.Module.Distribution.C_FrontendParams x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_9_case__16 x1 (Curry.Module.Distribution.C_FrontendParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_9_case__16(x1)(x)(st))(i)(xs)(st)
c__case_9_case__16 x1 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_9_case__16")(x)



c__case_8_case__17 x1 x2@(Curry.Module.Distribution.C_FrontendParams x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_8_case__17 x1 (Curry.Module.Distribution.C_FrontendParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_8_case__17(x1)(x)(st))(i)(xs)(st)
c__case_8_case__17 x1 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_8_case__17")(x)



c__case_7_case__18 x1 x2@(Curry.Module.Distribution.C_FrontendParams x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_7_case__18 x1 (Curry.Module.Distribution.C_FrontendParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_7_case__18(x1)(x)(st))(i)(xs)(st)
c__case_7_case__18 x1 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_7_case__18")(x)



c__case_6_case__19 x1 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x7)(st)
c__case_6_case__19 x1 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_6_case__19 x1 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_6_case__19(x1)(x7)(x)(st))(i)(xs)(st)
c__case_6_case__19 x1 x7 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_6_case__19")(x)



c__case_5_case__20 x1 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('q'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))(st)
c__case_5_case__20 x1 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))))))))(st)
c__case_5_case__20 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_5_case__20(x1)(x)(st))(i)(xs)(st)
c__case_5_case__20 x1 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_5_case__20")(x)



c__case_4_case__21 x1 x3 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_return(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleDistribution.c_installDir(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))))))))))(x6)(st))(x7)(st))(st)
c__case_4_case__21 x1 x3 x5@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OraclePrelude.c_maybe(Curry.Module.OracleDistribution.c_getLoadPath(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_return))))(Curry.Module.OracleDistribution.c_fullPath(x3)(x8)(st))(x9)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleDistribution.c_callFrontendWithParams'46_'35lambda10))))(x10)(st))(st)
c__case_4_case__21 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_4_case__21(x1)(x3)(x)(st))(i)(xs)(st)
c__case_4_case__21 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_4_case__21")(x)



c__case_2_case__22 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_2_case__22 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))))(x1)(st))(st)
c__case_2_case__22 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_2_case__22(x1)(x)(st))(i)(xs)(st)
c__case_2_case__22 x1 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_2_case__22")(x)



c__case_3_case__23 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_3_case__23 x1 x2@Curry.Module.Prelude.C_False st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OracleDistribution.c__case_2(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OracleDistribution.c_curryCompiler(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))(x3)(st))(x4)(st))(st)
c__case_3_case__23 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_3_case__23(x1)(x)(st))(i)(xs)(st)
c__case_3_case__23 x1 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_3_case__23")(x)



c__case_1_case__24 x1 x2@Curry.Module.Distribution.C_FCY st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))))(st)
c__case_1_case__24 x1 x2@Curry.Module.Distribution.C_FINT st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))))(st)
c__case_1_case__24 x1 x2@Curry.Module.Distribution.C_ACY st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))(st)
c__case_1_case__24 x1 x2@Curry.Module.Distribution.C_UACY st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st)
c__case_1_case__24 x1 x2@Curry.Module.Distribution.C_HTML st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))))(st)
c__case_1_case__24 x1 x2@Curry.Module.Distribution.C_CY st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))))))))(st)
c__case_1_case__24 x1 (Curry.Module.Distribution.C_FrontendTargetOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_1_case__24(x1)(x)(st))(i)(xs)(st)
c__case_1_case__24 x1 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_1_case__24")(x)



c__case_0_case__25 x1 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSystem.c_system(x8)(x1)(st))(st)
c__case_0_case__25 x1 x7 x8 x9@Curry.Module.Prelude.C_False st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))(Curry.Module.OracleSystem.c_system(Curry.Module.OraclePrelude.op_43_43(x8)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(x7)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('2'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('1'))(Curry.Module.Prelude.List))))))(x1)(st))(x10)(st))(x11)(st))(x12)(st))(st)
c__case_0_case__25 x1 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleDistribution.c__case_0_case__25(x1)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_0_case__25 x1 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleDistribution._case_0_case__25")(x)


