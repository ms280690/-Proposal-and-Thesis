{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleFileGoodies (module Curry.Module.OracleFileGoodies) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.FileGoodies
import Curry.Module.Directory
import Curry.Module.List
import Curry.Module.Prelude
import Curry.Module.OracleDirectory
import Curry.Module.OracleList
import Curry.Module.OraclePrelude



-- begin included



-- end included

c_separatorChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_separatorChar x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Char('/'))(st)



c_pathSeparatorChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_pathSeparatorChar x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Char(':'))(st)



c_suffixSeparatorChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_suffixSeparatorChar x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Char('.'))(st)



c_isAbsolute :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isAbsolute x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_19(x2)(x1)(st))(st)



c_dirName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_dirName x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_fst(Curry.Module.OracleFileGoodies.c_splitDirectoryBaseName(x2)(x1)(st))(x3)(st))(st)



c_baseName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_baseName x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_snd(Curry.Module.OracleFileGoodies.c_splitDirectoryBaseName(x2)(x1)(st))(x3)(st))(st)



c_splitDirectoryBaseName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_splitDirectoryBaseName x2 x1 st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))))))(let {x3 = Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_break(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_61_61))(st))(Curry.Module.OracleFileGoodies.c_separatorChar(x1)(st))))))(x6)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_reverse(x7)(st))(x2)(x8)(st))(x9)(st)} in let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(let {x5 = Curry.Module.OracleFileGoodies.c_splitDirectoryBaseName'46_'35selFP4'35rdir(x3)(x11)(st)} in let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(Curry.Module.OracleFileGoodies.c__case_18(Curry.Module.OracleFileGoodies.c_splitDirectoryBaseName'46_'35selFP3'35rbase(x3)(x10)(st))(x5)(Curry.Module.OraclePrelude.c_null(x5)(x12)(st))(x13)(st))(st))(st))(st))(st)



c_splitDirectoryBaseName'46_'35selFP3'35rbase :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_splitDirectoryBaseName'46_'35selFP3'35rbase x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_17(x2)(x1)(st))(st)



c_splitDirectoryBaseName'46_'35selFP4'35rdir :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_splitDirectoryBaseName'46_'35selFP4'35rdir x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_16(x2)(x1)(st))(st)



c_stripSuffix :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_stripSuffix x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_fst))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFileGoodies.c_splitBaseName))))(x1)(st))(st)



c_fileSuffix :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_fileSuffix x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_snd))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFileGoodies.c_splitBaseName))))(x1)(st))(st)



c_splitBaseName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_splitBaseName x2 x1 st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))))))(let {x3 = Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_break(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_61_61))(st))(Curry.Module.OracleFileGoodies.c_suffixSeparatorChar(x1)(st))))))(x6)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_reverse(x7)(st))(x2)(x8)(st))(x9)(st)} in let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(let {x4 = Curry.Module.OracleFileGoodies.c_splitBaseName'46_'35selFP6'35rsuffix(x3)(x10)(st)} in let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(let {x5 = Curry.Module.OracleFileGoodies.c_splitBaseName'46_'35selFP7'35rbase(x3)(x11)(st)} in let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))))))(Curry.Module.OracleFileGoodies.c__case_15(x2)(x4)(x5)(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.c_null(x5)(x12)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_elem(Curry.Module.OracleFileGoodies.c_separatorChar(x13)(st))(x14)(st))(x4)(x15)(st))(x16)(st))(x17)(st))(st))(st))(st))(st)



c_splitBaseName'46_'35selFP6'35rsuffix :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_splitBaseName'46_'35selFP6'35rsuffix x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_14(x2)(x1)(st))(st)



c_splitBaseName'46_'35selFP7'35rbase :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_splitBaseName'46_'35selFP7'35rbase x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_13(x2)(x1)(st))(st)



c_splitPath :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_splitPath x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_12(x2)(x1)(st))(st)



c_splitPath'46_'35selFP9'35ys :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_splitPath'46_'35selFP9'35ys x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_10(x2)(x1)(st))(st)



c_splitPath'46_'35selFP10'35zs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_splitPath'46_'35selFP10'35zs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_9(x2)(x1)(st))(st)



c_findFileInPath :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))))))
c_findFileInPath x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.pf(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))))(Curry.Module.OracleFileGoodies.c_lookupFileInPath))(st))(st)



c_lookupFileInPath :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_lookupFileInPath x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleFileGoodies.c__case_8(x2)(x3)(x4)(Curry.Module.OracleFileGoodies.c_isAbsolute(x2)(x1)(st))(x5)(st))(st)



c_lookupFileInPath'46lookupFirstFileWithSuffix'4636 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_lookupFileInPath'46lookupFirstFileWithSuffix'4636 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_7(x2)(x3)(x1)(st))(st)



c_lookupFileInPath'46lookupFirstFileWithSuffix'4636'46_'35lambda3 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.Prelude.C_Bool -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_lookupFileInPath'46lookupFirstFileWithSuffix'4636'46_'35lambda3 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_6(x2)(x3)(x4)(x5)(x1)(st))(st)



c_lookupFileInPath'46lookupFirstFile'4636 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_lookupFileInPath'46lookupFirstFile'4636 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_5(x2)(x3)(x4)(x1)(st))(st)



c_lookupFileInPath'46lookupFirstFile'4636'46_'35lambda2 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_lookupFileInPath'46lookupFirstFile'4636'46_'35lambda2 x2 x3 x4 x5 x1 st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_maybe(Curry.Module.OracleFileGoodies.c_lookupFileInPath'46lookupFirstFile'4636(x3)(x4)(x2)(x1)(st))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_return))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc(Curry.Module.Prelude.C_Just))))(x6)(st))(x5)(x7)(st))(st)



c_getFileInPath :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_getFileInPath x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleFileGoodies.c_lookupFileInPath(x2)(x3)(x4)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFileGoodies.c_getFileInPath'46_'35lambda4(x2)(x4)))))(x5)(st))(st)



c_getFileInPath'46_'35lambda4 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_getFileInPath'46_'35lambda4 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.c_maybe(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_error))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43(x2)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))))))))))))(Curry.Module.OraclePrelude.c_concat(Curry.Module.OracleList.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.OracleFileGoodies.c_pathSeparatorChar(x1)(st))(Curry.Module.Prelude.List))(x3)(x5)(st))(x6)(st))(x7)(st))(x8)(st))(x9)(st))(x10)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_return))))(x4)(x11)(st))(st)



c_replaceFileName :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_replaceFileName x2 x3 x1 st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(let {x4 = Curry.Module.OracleFileGoodies.c_splitDirectoryBaseName(x3)(x1)(st)} in let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x9)(Curry.Module.OracleFileGoodies.c__case_4(x2)(Curry.Module.OracleFileGoodies.c_replaceFileName'46_'35selFP13'35fn(x4)(x8)(st))(Curry.Module.OracleFileGoodies.c_replaceFileName'46_'35selFP12'35dir(x4)(x7)(st))(x9)(st))(st))(st))(st))(st)



c_replaceFileName'46_'35selFP12'35dir :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_replaceFileName'46_'35selFP12'35dir x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_1(x2)(x1)(st))(st)



c_replaceFileName'46_'35selFP13'35fn :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_replaceFileName'46_'35selFP13'35fn x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_0(x2)(x1)(st))(st)



c__case_0 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_0_case__19(x1)(x2)(st))(st)



c__case_1 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_1_case__18(x1)(x2)(st))(st)



c__case_4 x2 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_4_case__17(x1)(x2)(x6)(x5)(st))(st)



c__case_3 x2 x5 x6 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_3_case__16(x1)(x2)(x5)(x6)(x8)(x9)(st))(st)



c__case_2 x2 x5 x6 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_2_case__15(x1)(x2)(x5)(x6)(x8)(st))(st)



c__case_5 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_5_case__14(x1)(x2)(x3)(x4)(st))(st)



c__case_6 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_6_case__13(x1)(x2)(x3)(x4)(x5)(st))(st)



c__case_7 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_7_case__12(x1)(x2)(x3)(st))(st)



c__case_8 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_8_case__11(x1)(x2)(x3)(x4)(x5)(st))(st)



c__case_9 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_9_case__10(x1)(x2)(st))(st)



c__case_10 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_10_case__9(x1)(x2)(st))(st)



c__case_12 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_12_case__8(x1)(x2)(st))(st)



c__case_11 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_11_case__7(x1)(x6)(x7)(x8)(st))(st)



c__case_13 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_13_case__6(x1)(x2)(st))(st)



c__case_14 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_14_case__5(x1)(x2)(st))(st)



c__case_15 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_15_case__4(x1)(x2)(x4)(x5)(x6)(st))(st)



c__case_16 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_16_case__3(x1)(x2)(st))(st)



c__case_17 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_17_case__2(x1)(x2)(st))(st)



c__case_18 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_18_case__1(x1)(x4)(x5)(x6)(st))(st)



c__case_19 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_19_case__0(x1)(x2)(st))(st)



c__case_19_case__0 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.OracleFileGoodies.c_separatorChar(x1)(st))(x5)(st))(st)
c__case_19_case__0 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_19_case__0(x1)(x)(st))(i)(xs)(st)
c__case_19_case__0 x1 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_19_case__0")(x)



c__case_18_case__1 x1 x4 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_reverse(x1)(st))(x4)(x7)(st)))(st)
c__case_18_case__1 x1 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))))(Curry.Module.Prelude.T2(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_reverse(x1)(st))(Curry.Module.OraclePrelude.c_tail(x5)(x8)(st))(x9)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_reverse(x10)(st))(x4)(x11)(st)))(st)
c__case_18_case__1 x1 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_18_case__1(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_18_case__1 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_18_case__1")(x)



c__case_17_case__2 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_17_case__2 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_17_case__2(x1)(x)(st))(i)(xs)(st)
c__case_17_case__2 x1 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_17_case__2")(x)



c__case_16_case__3 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_16_case__3 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_16_case__3(x1)(x)(st))(i)(xs)(st)
c__case_16_case__3 x1 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_16_case__3")(x)



c__case_15_case__4 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(x2)(Curry.Module.Prelude.List))(st)
c__case_15_case__4 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))))(Curry.Module.Prelude.T2(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_reverse(x1)(st))(Curry.Module.OraclePrelude.c_tail(x5)(x7)(st))(x8)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_reverse(x9)(st))(x4)(x10)(st)))(st)
c__case_15_case__4 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_15_case__4(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_15_case__4 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_15_case__4")(x)



c__case_14_case__5 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_14_case__5 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_14_case__5(x1)(x)(st))(i)(xs)(st)
c__case_14_case__5 x1 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_14_case__5")(x)



c__case_13_case__6 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_13_case__6 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_13_case__6(x1)(x)(st))(i)(xs)(st)
c__case_13_case__6 x1 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_13_case__6")(x)



c__case_11_case__7 x1 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(st)
c__case_11_case__7 x1 x6 x7 x8@Curry.Module.Prelude.C_False st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(x6)(Curry.Module.OracleFileGoodies.c_splitPath(Curry.Module.OraclePrelude.c_tail(x7)(x1)(st))(x9)(st)))(st)
c__case_11_case__7 x1 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_11_case__7(x1)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_11_case__7 x1 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_11_case__7")(x)



c__case_12_case__8 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_12_case__8 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))))(let {x5 = Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_break(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_61_61))(st))(Curry.Module.OracleFileGoodies.c_pathSeparatorChar(x1)(st))))))(x8)(st))((Curry.Module.Prelude.:<)(x3)(x4))(x9)(st)} in let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(let {x7 = Curry.Module.OracleFileGoodies.c_splitPath'46_'35selFP10'35zs(x5)(x11)(st)} in let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(Curry.Module.OracleFileGoodies.c__case_11(Curry.Module.OracleFileGoodies.c_splitPath'46_'35selFP9'35ys(x5)(x10)(st))(x7)(Curry.Module.OraclePrelude.c_null(x7)(x12)(st))(x13)(st))(st))(st))(st))(st)
c__case_12_case__8 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_12_case__8(x1)(x)(st))(i)(xs)(st)
c__case_12_case__8 x1 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_12_case__8")(x)



c__case_10_case__9 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_10_case__9 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_10_case__9(x1)(x)(st))(i)(xs)(st)
c__case_10_case__9 x1 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_10_case__9")(x)



c__case_9_case__10 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_9_case__10 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_9_case__10(x1)(x)(st))(i)(xs)(st)
c__case_9_case__10 x1 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_9_case__10")(x)



c__case_8_case__11 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c_lookupFileInPath'46lookupFirstFileWithSuffix'4636(x2)(x3)(x1)(st))(st)
c__case_8_case__11 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c_lookupFileInPath'46lookupFirstFile'4636(x2)(x3)(x4)(x1)(st))(st)
c__case_8_case__11 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_8_case__11(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_8_case__11 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_8_case__11")(x)



c__case_7_case__12 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_return(Curry.Module.Prelude.C_Nothing)(x1)(st))(st)
c__case_7_case__12 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(let {x6 = Curry.Module.OraclePrelude.op_43_43(x2)(x4)(x1)(st)} in let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDirectory.c_doesFileExist(x6)(x7)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFileGoodies.c_lookupFileInPath'46lookupFirstFileWithSuffix'4636'46_'35lambda3(x2)(x6)(x5)))))(x8)(st))(st))(st)
c__case_7_case__12 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_7_case__12(x1)(x2)(x)(st))(i)(xs)(st)
c__case_7_case__12 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_7_case__12")(x)



c__case_6_case__13 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_return(Curry.Module.Prelude.C_Just(x3))(x1)(st))(st)
c__case_6_case__13 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c_lookupFileInPath'46lookupFirstFileWithSuffix'4636(x2)(x4)(x1)(st))(st)
c__case_6_case__13 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_6_case__13(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_6_case__13 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_6_case__13")(x)



c__case_5_case__14 x1 x2 x3 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_return(Curry.Module.Prelude.C_Nothing)(x1)(st))(st)
c__case_5_case__14 x1 x2 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleFileGoodies.c_lookupFileInPath'46lookupFirstFileWithSuffix'4636(Curry.Module.OraclePrelude.op_43_43(x5)((Curry.Module.Prelude.:<)(Curry.Module.OracleFileGoodies.c_separatorChar(x1)(st))(x2))(x7)(st))(x3)(x8)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFileGoodies.c_lookupFileInPath'46lookupFirstFile'4636'46_'35lambda2(x6)(x2)(x3)))))(x9)(st))(st)
c__case_5_case__14 x1 x2 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_5_case__14(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_5_case__14 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_5_case__14")(x)



c__case_2_case__15 x1 x2 x5 x6 x8@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.c_apply(x2)(x6)(x1)(st))(st)
c__case_2_case__15 x1 x2 x5 x6 x8@((Curry.Module.Prelude.:<) x9 x10) st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(x5)((Curry.Module.Prelude.:<)(Curry.Module.OracleFileGoodies.c_separatorChar(x1)(st))(Curry.Module.Oracle.c_apply(x2)(x6)(x11)(st)))(x12)(st))(st)
c__case_2_case__15 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_2_case__15(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_2_case__15 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_2_case__15")(x)



c__case_3_case__16 x1 x2 x5 x6 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFileGoodies.c__case_2(x2)(x5)(x6)(x8)(x1)(st))(st)
c__case_3_case__16 x1 x2 x5 x6 x8 x9@Curry.Module.Prelude.C_False st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(x5)((Curry.Module.Prelude.:<)(Curry.Module.OracleFileGoodies.c_separatorChar(x1)(st))(Curry.Module.Oracle.c_apply(x2)(x6)(x10)(st)))(x11)(st))(st)
c__case_3_case__16 x1 x2 x5 x6 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_3_case__16(x1)(x2)(x5)(x6)(x8)(x)(st))(i)(xs)(st)
c__case_3_case__16 x1 x2 x5 x6 x8 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_3_case__16")(x)



c__case_4_case__17 x1 x2 x6 x5@((Curry.Module.Prelude.:<) x7 x8) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OracleFileGoodies.c__case_3(x2)(x5)(x6)(x7)(x8)(Curry.Module.OraclePrelude.op_61_61(x7)(Curry.Module.Prelude.C_Char('.'))(x1)(st))(x9)(st))(st)
c__case_4_case__17 x1 x2 x6 x5@Curry.Module.Prelude.List st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(x5)((Curry.Module.Prelude.:<)(Curry.Module.OracleFileGoodies.c_separatorChar(x1)(st))(Curry.Module.Oracle.c_apply(x2)(x6)(x10)(st)))(x11)(st))(st)
c__case_4_case__17 x1 x2 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_4_case__17(x1)(x2)(x6)(x)(st))(i)(xs)(st)
c__case_4_case__17 x1 x2 x6 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_4_case__17")(x)



c__case_1_case__18 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_1_case__18 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_1_case__18(x1)(x)(st))(i)(xs)(st)
c__case_1_case__18 x1 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_1_case__18")(x)



c__case_0_case__19 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_0_case__19 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFileGoodies.c__case_0_case__19(x1)(x)(st))(i)(xs)(st)
c__case_0_case__19 x1 x st = Curry.RunTimeSystem.patternFail("OracleFileGoodies._case_0_case__19")(x)


