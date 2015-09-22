{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleFlatCurry (module Curry.Module.OracleFlatCurry) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.FlatCurry
import Curry.Module.Directory
import Curry.Module.Distribution
import Curry.Module.FileGoodies
import Curry.Module.Prelude
import Curry.Module.ReadShowTerm
import Curry.Module.OracleDirectory
import Curry.Module.OracleDistribution
import Curry.Module.OracleFileGoodies
import Curry.Module.OraclePrelude
import Curry.Module.OracleReadShowTerm



-- begin included



-- end included

c_readFlatCurry :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_readFlatCurry x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OracleFlatCurry.c_readFlatCurryWithParseOptions(x2)(Curry.Module.OracleDistribution.c_setQuiet(Curry.Module.Prelude.C_True)(Curry.Module.OracleDistribution.c_defaultParams(x1)(st))(x3)(st))(x4)(st))(st)



c_readFlatCurryWithParseOptions :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Distribution.C_FrontendParams -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_readFlatCurryWithParseOptions x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDistribution.c_lookupFileInLoadPath(Curry.Module.OraclePrelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(x1)(st))(x4)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurry.c_readFlatCurryWithParseOptions'46_'35lambda2(x3)(x2)))))(x5)(st))(st)



c_readFlatCurryWithParseOptions'46_'35lambda2 :: Curry.Module.Distribution.C_FrontendParams -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_readFlatCurryWithParseOptions'46_'35lambda2 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDistribution.c_lookupFileInLoadPath(Curry.Module.OraclePrelude.op_43_43(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))(x1)(st))(x5)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurry.c_readFlatCurryWithParseOptions'46_'35lambda2'46_'35lambda3(x4)(x2)(x3)))))(x6)(st))(st)



c_readFlatCurryWithParseOptions'46_'35lambda2'46_'35lambda3 :: (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.Distribution.C_FrontendParams -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_readFlatCurryWithParseOptions'46_'35lambda2'46_'35lambda3 x2 x3 x4 x5 x1 st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.op_62_62(Curry.Module.OracleFlatCurry.c__case_5(x2)(x3)(x4)(x5)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Nothing)(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x5)(Curry.Module.Prelude.C_Nothing)(x6)(st))(x7)(st))(x8)(st))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDistribution.c_findFileInLoadPath(Curry.Module.OraclePrelude.op_43_43(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))(x9)(st))(x10)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurry.c_readFlatCurryWithParseOptions'46_'35lambda2'46_'35lambda3'46_'35lambda4))))(x11)(st))(x12)(st))(st)



c_readFlatCurryWithParseOptions'46_'35lambda2'46_'35lambda3'46_'35lambda4 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_readFlatCurryWithParseOptions'46_'35lambda2'46_'35lambda3'46_'35lambda4 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurry.c_readFlatCurryFile(x2)(x1)(st))(st)



c_flatCurryFileName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_flatCurryFileName x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))))(Curry.Module.OracleDistribution.c_inCurrySubdir(Curry.Module.OraclePrelude.op_43_43(Curry.Module.Oracle.c_apply(Curry.Module.OracleFileGoodies.c_stripSuffix(x1)(st))(x2)(x3)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))(x4)(st))(x5)(st))(st)



c_flatCurryIntName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_flatCurryIntName x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))))(Curry.Module.OracleDistribution.c_inCurrySubdir(Curry.Module.OraclePrelude.op_43_43(Curry.Module.Oracle.c_apply(Curry.Module.OracleFileGoodies.c_stripSuffix(x1)(st))(x2)(x3)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))))(x4)(st))(x5)(st))(st)



c_readFlatCurryFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_readFlatCurryFile x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDirectory.c_doesFileExist(x2)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurry.c_readFlatCurryFile'46_'35lambda6(x2)))))(x3)(st))(st)



c_readFlatCurryFile'46readExistingFCY'4614 :: (Curry t0) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))
c_readFlatCurryFile'46readExistingFCY'4614 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OraclePrelude.c_readFile(x2)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurry.c_readFlatCurryFile'46readExistingFCY'4614'46_'35lambda5))))(x3)(st))(st)



c_readFlatCurryFile'46readExistingFCY'4614'46_'35lambda5 :: (Curry t1) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1))
c_readFlatCurryFile'46readExistingFCY'4614'46_'35lambda5 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_return(Curry.Module.OracleReadShowTerm.c_readTerm(x2)(x1)(st))(x3)(st))(st)



c_readFlatCurryFile'46_'35lambda6 :: (Curry t2) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t2))
c_readFlatCurryFile'46_'35lambda6 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurry.c__case_4(x2)(x3)(x1)(st))(st)



c_readFlatCurryFile'46_'35lambda6'46_'35lambda7 :: (Curry t3) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t3))
c_readFlatCurryFile'46_'35lambda6'46_'35lambda7 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurry.c__case_3(x2)(x3)(x4)(x1)(st))(st)



c_readFlatCurryInt :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_readFlatCurryInt x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDirectory.c_doesFileExist(Curry.Module.OraclePrelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(x1)(st))(x3)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurry.c_readFlatCurryInt'46_'35lambda8(x2)))))(x4)(st))(st)



c_readFlatCurryInt'46_'35lambda8 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_readFlatCurryInt'46_'35lambda8 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDirectory.c_doesFileExist(Curry.Module.OraclePrelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))(x1)(st))(x4)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurry.c_readFlatCurryInt'46_'35lambda8'46_'35lambda9(x3)(x2)))))(x5)(st))(st)



c_readFlatCurryInt'46_'35lambda8'46_'35lambda9 :: Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_readFlatCurryInt'46_'35lambda8'46_'35lambda9 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_62_62(Curry.Module.OracleFlatCurry.c__case_2(x2)(x3)(x4)(Curry.Module.OraclePrelude.op_124_124(x2)(x4)(x1)(st))(x5)(st))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDistribution.c_findFileInLoadPath(Curry.Module.OraclePrelude.op_43_43(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))))(x6)(st))(x7)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurry.c_readFlatCurryInt'46_'35lambda8'46_'35lambda9'46_'35lambda10))))(x8)(st))(x9)(st))(st)



c_readFlatCurryInt'46_'35lambda8'46_'35lambda9'46_'35lambda10 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_readFlatCurryInt'46_'35lambda8'46_'35lambda9'46_'35lambda10 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurry.c_readFlatCurryFile(x2)(x1)(st))(st)



c_writeFCY :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_Prog -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_writeFCY x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_writeFile(x2)(Curry.Module.OracleReadShowTerm.c_showTerm(x3)(x1)(st))(x4)(st))(st)



c_showQNameInModule :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showQNameInModule x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurry.c__case_1(x2)(x3)(x1)(st))(st)



c__case_1 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurry.c__case_1_case__5(x1)(x2)(x3)(st))(st)



c__case_0 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurry.c__case_0_case__4(x1)(x4)(x5)(x6)(st))(st)



c__case_2 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurry.c__case_2_case__3(x1)(x3)(x5)(st))(st)



c__case_3 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurry.c__case_3_case__2(x1)(x2)(x3)(x4)(st))(st)



c__case_4 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurry.c__case_4_case__1(x1)(x2)(x3)(st))(st)



c__case_5 x2 x3 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurry.c__case_5_case__0(x1)(x3)(x4)(x6)(st))(st)



c__case_5_case__0 x1 x3 x4 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_done(x1)(st))(st)
c__case_5_case__0 x1 x3 x4 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleDistribution.c_callFrontendWithParams(Curry.Module.Distribution.C_FCY)(x3)(x4)(x1)(st))(st)
c__case_5_case__0 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurry.c__case_5_case__0(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_5_case__0 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurry._case_5_case__0")(x)



c__case_4_case__1 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurry.c_readFlatCurryFile'46readExistingFCY'4614(x2)(x1)(st))(st)
c__case_4_case__1 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(let {x4 = Curry.Module.OracleDistribution.c_inCurrySubdir(x2)(x1)(st)} in let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDirectory.c_doesFileExist(x4)(x5)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurry.c_readFlatCurryFile'46_'35lambda6'46_'35lambda7(x2)(x4)))))(x6)(st))(st))(st)
c__case_4_case__1 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurry.c__case_4_case__1(x1)(x2)(x)(st))(i)(xs)(st)
c__case_4_case__1 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurry._case_4_case__1")(x)



c__case_3_case__2 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurry.c_readFlatCurryFile'46readExistingFCY'4614(x3)(x1)(st))(st)
c__case_3_case__2 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_error(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('X'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))(Curry.Module.OraclePrelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))))))))))))))(x1)(st))(x5)(st))(x6)(st))(st)
c__case_3_case__2 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurry.c__case_3_case__2(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_3_case__2 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurry._case_3_case__2")(x)



c__case_2_case__3 x1 x3 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OracleDistribution.c_callFrontend(Curry.Module.Distribution.C_FINT)(x1)(st))(x3)(x6)(st))(st)
c__case_2_case__3 x1 x3 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_done(x1)(st))(st)
c__case_2_case__3 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurry.c__case_2_case__3(x1)(x3)(x)(st))(i)(xs)(st)
c__case_2_case__3 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurry._case_2_case__3")(x)



c__case_0_case__4 x1 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_0_case__4 x1 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x4)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List))(x5)(x1)(st))(x7)(st))(st)
c__case_0_case__4 x1 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurry.c__case_0_case__4(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_0_case__4 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurry._case_0_case__4")(x)



c__case_1_case__5 x1 x2 x3@(Curry.Module.Prelude.T2 x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))))(Curry.Module.OracleFlatCurry.c__case_0(x2)(x4)(x5)(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_61_61(x4)(x2)(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(x6)(st))(x7)(st))(x8)(st))(st)
c__case_1_case__5 x1 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurry.c__case_1_case__5(x1)(x2)(x)(st))(i)(xs)(st)
c__case_1_case__5 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurry._case_1_case__5")(x)


