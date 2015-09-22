{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.FileGoodies (module Curry.Module.FileGoodies) where

import Curry.RunTimeSystem
import Curry.Module.Directory
import Curry.Module.List
import Curry.Module.Prelude



-- begin included



-- end included

c_separatorChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_separatorChar st = Curry.Module.Prelude.C_Char('/')



c_pathSeparatorChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_pathSeparatorChar st = Curry.Module.Prelude.C_Char(':')



c_suffixSeparatorChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_suffixSeparatorChar st = Curry.Module.Prelude.C_Char('.')



c_isAbsolute :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isAbsolute x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('/'))(st)
c_isAbsolute (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_isAbsolute(x)(st))(i)(xs)(st)
c_isAbsolute x st = Curry.RunTimeSystem.patternFail("FileGoodies.isAbsolute")(x)



c_dirName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_dirName x1 st = Curry.Module.Prelude.c_fst(Curry.Module.FileGoodies.c_splitDirectoryBaseName(x1)(st))(st)



c_baseName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_baseName x1 st = Curry.Module.Prelude.c_snd(Curry.Module.FileGoodies.c_splitDirectoryBaseName(x1)(st))(st)



c_splitDirectoryBaseName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_splitDirectoryBaseName x1 st = let {x2 = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_break(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_61_61))(Curry.Module.Prelude.C_Char('/'))))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_reverse(st))(x1)(st))(st)} in let {x3 = Curry.Module.FileGoodies.c_splitDirectoryBaseName'46_'35selFP3'35rbase(x2)(st)} in let {x4 = Curry.Module.FileGoodies.c_splitDirectoryBaseName'46_'35selFP4'35rdir(x2)(st)} in Curry.Module.FileGoodies.c_splitDirectoryBaseName_case_6(x3)(x4)(Curry.Module.Prelude.c_null(x4)(st))(st)



c_splitDirectoryBaseName'46_'35selFP3'35rbase :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_splitDirectoryBaseName'46_'35selFP3'35rbase x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_splitDirectoryBaseName'46_'35selFP3'35rbase (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_splitDirectoryBaseName'46_'35selFP3'35rbase(x)(st))(i)(xs)(st)
c_splitDirectoryBaseName'46_'35selFP3'35rbase x st = Curry.RunTimeSystem.patternFail("FileGoodies.splitDirectoryBaseName._#selFP3#rbase")(x)



c_splitDirectoryBaseName'46_'35selFP4'35rdir :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_splitDirectoryBaseName'46_'35selFP4'35rdir x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_splitDirectoryBaseName'46_'35selFP4'35rdir (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_splitDirectoryBaseName'46_'35selFP4'35rdir(x)(st))(i)(xs)(st)
c_splitDirectoryBaseName'46_'35selFP4'35rdir x st = Curry.RunTimeSystem.patternFail("FileGoodies.splitDirectoryBaseName._#selFP4#rdir")(x)



c_stripSuffix :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_stripSuffix st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_fst))(Curry.Module.Prelude.pf(Curry.Module.FileGoodies.c_splitBaseName))(st)



c_fileSuffix :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_fileSuffix st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))(Curry.Module.Prelude.pf(Curry.Module.FileGoodies.c_splitBaseName))(st)



c_splitBaseName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_splitBaseName x1 st = let {x2 = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_break(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_61_61))(Curry.Module.Prelude.C_Char('.'))))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_reverse(st))(x1)(st))(st)} in let {x3 = Curry.Module.FileGoodies.c_splitBaseName'46_'35selFP6'35rsuffix(x2)(st)} in let {x4 = Curry.Module.FileGoodies.c_splitBaseName'46_'35selFP7'35rbase(x2)(st)} in Curry.Module.FileGoodies.c_splitBaseName_case_5(x1)(x3)(x4)(Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.c_null(x4)(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(Curry.Module.Prelude.C_Char('/'))(st))(x3)(st))(st))(st)



c_splitBaseName'46_'35selFP6'35rsuffix :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_splitBaseName'46_'35selFP6'35rsuffix x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_splitBaseName'46_'35selFP6'35rsuffix (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_splitBaseName'46_'35selFP6'35rsuffix(x)(st))(i)(xs)(st)
c_splitBaseName'46_'35selFP6'35rsuffix x st = Curry.RunTimeSystem.patternFail("FileGoodies.splitBaseName._#selFP6#rsuffix")(x)



c_splitBaseName'46_'35selFP7'35rbase :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_splitBaseName'46_'35selFP7'35rbase x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_splitBaseName'46_'35selFP7'35rbase (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_splitBaseName'46_'35selFP7'35rbase(x)(st))(i)(xs)(st)
c_splitBaseName'46_'35selFP7'35rbase x st = Curry.RunTimeSystem.patternFail("FileGoodies.splitBaseName._#selFP7#rbase")(x)



c_splitPath :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_splitPath x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_splitPath x1@((Curry.Module.Prelude.:<) x2 x3) st = let {x4 = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_break(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_61_61))(Curry.Module.Prelude.C_Char(':'))))(st))((Curry.Module.Prelude.:<)(x2)(x3))(st)} in let {x5 = Curry.Module.FileGoodies.c_splitPath'46_'35selFP9'35ys(x4)(st)} in let {x6 = Curry.Module.FileGoodies.c_splitPath'46_'35selFP10'35zs(x4)(st)} in Curry.Module.FileGoodies.c_splitPath_case_4(x5)(x6)(Curry.Module.Prelude.c_null(x6)(st))(st)
c_splitPath (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_splitPath(x)(st))(i)(xs)(st)
c_splitPath x st = Curry.RunTimeSystem.patternFail("FileGoodies.splitPath")(x)



c_splitPath'46_'35selFP9'35ys :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_splitPath'46_'35selFP9'35ys x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_splitPath'46_'35selFP9'35ys (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_splitPath'46_'35selFP9'35ys(x)(st))(i)(xs)(st)
c_splitPath'46_'35selFP9'35ys x st = Curry.RunTimeSystem.patternFail("FileGoodies.splitPath._#selFP9#ys")(x)



c_splitPath'46_'35selFP10'35zs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_splitPath'46_'35selFP10'35zs x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_splitPath'46_'35selFP10'35zs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_splitPath'46_'35selFP10'35zs(x)(st))(i)(xs)(st)
c_splitPath'46_'35selFP10'35zs x st = Curry.RunTimeSystem.patternFail("FileGoodies.splitPath._#selFP10#zs")(x)



c_findFileInPath :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))))
c_findFileInPath st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.FileGoodies.c_lookupFileInPath)



c_lookupFileInPath :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_lookupFileInPath x1 x2 x3 st = Curry.Module.FileGoodies.c_lookupFileInPath_case_3(x1)(x2)(x3)(Curry.Module.FileGoodies.c_isAbsolute(x1)(st))(st)



c_lookupFileInPath'46lookupFirstFileWithSuffix'4636 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_lookupFileInPath'46lookupFirstFileWithSuffix'4636 x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.C_Nothing)(st)
c_lookupFileInPath'46lookupFirstFileWithSuffix'4636 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.Prelude.op_43_43(x1)(x3)(st)} in Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesFileExist(x5)(st))(Curry.Module.Prelude.pf(Curry.Module.FileGoodies.c_lookupFileInPath'46lookupFirstFileWithSuffix'4636'46_'35lambda3(x1)(x5)(x4)))(st)
c_lookupFileInPath'46lookupFirstFileWithSuffix'4636 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_lookupFileInPath'46lookupFirstFileWithSuffix'4636(x1)(x)(st))(i)(xs)(st)
c_lookupFileInPath'46lookupFirstFileWithSuffix'4636 x1 x st = Curry.RunTimeSystem.patternFail("FileGoodies.lookupFileInPath.lookupFirstFileWithSuffix.36")(x)



c_lookupFileInPath'46lookupFirstFileWithSuffix'4636'46_'35lambda3 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_lookupFileInPath'46lookupFirstFileWithSuffix'4636'46_'35lambda3 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.C_Just(x2))(st)
c_lookupFileInPath'46lookupFirstFileWithSuffix'4636'46_'35lambda3 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.FileGoodies.c_lookupFileInPath'46lookupFirstFileWithSuffix'4636(x1)(x3)(st)
c_lookupFileInPath'46lookupFirstFileWithSuffix'4636'46_'35lambda3 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_lookupFileInPath'46lookupFirstFileWithSuffix'4636'46_'35lambda3(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_lookupFileInPath'46lookupFirstFileWithSuffix'4636'46_'35lambda3 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("FileGoodies.lookupFileInPath.lookupFirstFileWithSuffix.36._#lambda3")(x)



c_lookupFileInPath'46lookupFirstFile'4636 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_lookupFileInPath'46lookupFirstFile'4636 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.C_Nothing)(st)
c_lookupFileInPath'46lookupFirstFile'4636 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.Prelude.op_62_62_61(Curry.Module.FileGoodies.c_lookupFileInPath'46lookupFirstFileWithSuffix'4636(Curry.Module.Prelude.op_43_43(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(x1))(st))(x2)(st))(Curry.Module.Prelude.pf(Curry.Module.FileGoodies.c_lookupFileInPath'46lookupFirstFile'4636'46_'35lambda2(x5)(x1)(x2)))(st)
c_lookupFileInPath'46lookupFirstFile'4636 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_lookupFileInPath'46lookupFirstFile'4636(x1)(x2)(x)(st))(i)(xs)(st)
c_lookupFileInPath'46lookupFirstFile'4636 x1 x2 x st = Curry.RunTimeSystem.patternFail("FileGoodies.lookupFileInPath.lookupFirstFile.36")(x)



c_lookupFileInPath'46lookupFirstFile'4636'46_'35lambda2 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_lookupFileInPath'46lookupFirstFile'4636'46_'35lambda2 x1 x2 x3 x4 st = Curry.Module.Prelude.c_maybe(Curry.Module.FileGoodies.c_lookupFileInPath'46lookupFirstFile'4636(x2)(x3)(x1)(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Prelude.pc(Curry.Module.Prelude.C_Just))(st))(x4)(st)



c_getFileInPath :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_getFileInPath x1 x2 x3 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.FileGoodies.c_lookupFileInPath(x1)(x2)(x3)(st))(Curry.Module.Prelude.pf(Curry.Module.FileGoodies.c_getFileInPath'46_'35lambda4(x1)(x3)))(st)



c_getFileInPath'46_'35lambda4 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_getFileInPath'46_'35lambda4 x1 x2 x3 st = Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_error))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.op_43_43(x1)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))))))))))))(Curry.Module.Prelude.c_concat(Curry.Module.List.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(x2)(st))(st))(st))(st))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(x3)(st)



c_replaceFileName :: (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_replaceFileName x1 x2 st = let {x3 = Curry.Module.FileGoodies.c_splitDirectoryBaseName(x2)(st)} in let {x4 = Curry.Module.FileGoodies.c_replaceFileName'46_'35selFP12'35dir(x3)(st)} in let {x5 = Curry.Module.FileGoodies.c_replaceFileName'46_'35selFP13'35fn(x3)(st)} in Curry.Module.FileGoodies.c_replaceFileName_case_2(x1)(x5)(x4)(st)



c_replaceFileName'46_'35selFP12'35dir :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_replaceFileName'46_'35selFP12'35dir x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_replaceFileName'46_'35selFP12'35dir (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_replaceFileName'46_'35selFP12'35dir(x)(st))(i)(xs)(st)
c_replaceFileName'46_'35selFP12'35dir x st = Curry.RunTimeSystem.patternFail("FileGoodies.replaceFileName._#selFP12#dir")(x)



c_replaceFileName'46_'35selFP13'35fn :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_replaceFileName'46_'35selFP13'35fn x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_replaceFileName'46_'35selFP13'35fn (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_replaceFileName'46_'35selFP13'35fn(x)(st))(i)(xs)(st)
c_replaceFileName'46_'35selFP13'35fn x st = Curry.RunTimeSystem.patternFail("FileGoodies.replaceFileName._#selFP13#fn")(x)



c_replaceFileName_case_2 x1 x5 x4@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.FileGoodies.c_replaceFileName_case_1(x1)(x4)(x5)(x6)(x7)(Curry.Module.Prelude.op_61_61(x6)(Curry.Module.Prelude.C_Char('.'))(st))(st)
c_replaceFileName_case_2 x1 x5 x4@Curry.Module.Prelude.List st = Curry.Module.Prelude.op_43_43(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.Prelude.c_apply(x1)(x5)(st)))(st)
c_replaceFileName_case_2 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_replaceFileName_case_2(x1)(x5)(x)(st))(i)(xs)(st)
c_replaceFileName_case_2 x1 x5 x st = Curry.RunTimeSystem.patternFail("FileGoodies.replaceFileName_case_2")(x)



c_replaceFileName_case_1 x1 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.FileGoodies.c_replaceFileName_case_0(x1)(x4)(x5)(x7)(st)
c_replaceFileName_case_1 x1 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_43_43(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.Prelude.c_apply(x1)(x5)(st)))(st)
c_replaceFileName_case_1 x1 x4 x5 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_replaceFileName_case_1(x1)(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c_replaceFileName_case_1 x1 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("FileGoodies.replaceFileName_case_1")(x)



c_replaceFileName_case_0 x1 x4 x5 x7@Curry.Module.Prelude.List st = Curry.Module.Prelude.c_apply(x1)(x5)(st)
c_replaceFileName_case_0 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.Prelude.op_43_43(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.Prelude.c_apply(x1)(x5)(st)))(st)
c_replaceFileName_case_0 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_replaceFileName_case_0(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_replaceFileName_case_0 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("FileGoodies.replaceFileName_case_0")(x)



c_lookupFileInPath_case_3 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.FileGoodies.c_lookupFileInPath'46lookupFirstFileWithSuffix'4636(x1)(x2)(st)
c_lookupFileInPath_case_3 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.FileGoodies.c_lookupFileInPath'46lookupFirstFile'4636(x1)(x2)(x3)(st)
c_lookupFileInPath_case_3 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_lookupFileInPath_case_3(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_lookupFileInPath_case_3 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("FileGoodies.lookupFileInPath_case_3")(x)



c_splitPath_case_4 x5 x6 x7@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)
c_splitPath_case_4 x5 x6 x7@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(x5)(Curry.Module.FileGoodies.c_splitPath(Curry.Module.Prelude.c_tail(x6)(st))(st))
c_splitPath_case_4 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_splitPath_case_4(x5)(x6)(x)(st))(i)(xs)(st)
c_splitPath_case_4 x5 x6 x st = Curry.RunTimeSystem.patternFail("FileGoodies.splitPath_case_4")(x)



c_splitBaseName_case_5 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2(x1)(Curry.Module.Prelude.List)
c_splitBaseName_case_5 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.T2(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_reverse(st))(Curry.Module.Prelude.c_tail(x4)(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_reverse(st))(x3)(st))
c_splitBaseName_case_5 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_splitBaseName_case_5(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_splitBaseName_case_5 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("FileGoodies.splitBaseName_case_5")(x)



c_splitDirectoryBaseName_case_6 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_reverse(st))(x3)(st))
c_splitDirectoryBaseName_case_6 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.T2(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_reverse(st))(Curry.Module.Prelude.c_tail(x4)(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_reverse(st))(x3)(st))
c_splitDirectoryBaseName_case_6 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FileGoodies.c_splitDirectoryBaseName_case_6(x3)(x4)(x)(st))(i)(xs)(st)
c_splitDirectoryBaseName_case_6 x3 x4 x st = Curry.RunTimeSystem.patternFail("FileGoodies.splitDirectoryBaseName_case_6")(x)


