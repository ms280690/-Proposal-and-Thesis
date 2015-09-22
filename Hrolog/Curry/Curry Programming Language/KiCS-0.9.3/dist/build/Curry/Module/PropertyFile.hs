{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.PropertyFile (module Curry.Module.PropertyFile) where

import Curry.RunTimeSystem
import Curry.Module.Char
import Curry.Module.Directory
import Curry.Module.IOExts
import Curry.Module.Prelude



-- begin included



-- end included

c_readPropertyFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_readPropertyFile x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesFileExist(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.PropertyFile.c_readPropertyFile'46_'35lambda3(x1)))(st)



c_readPropertyFile'46splitEqs'463 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_readPropertyFile'46splitEqs'463 x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_readPropertyFile'46splitEqs'463 x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.PropertyFile.c_readPropertyFile'46splitEqs'463_case_4(x2)(x3)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_break(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_61_61))(Curry.Module.Prelude.C_Char('='))))(st))(x2)(st))(st)
c_readPropertyFile'46splitEqs'463 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PropertyFile.c_readPropertyFile'46splitEqs'463(x)(st))(i)(xs)(st)
c_readPropertyFile'46splitEqs'463 x st = Curry.RunTimeSystem.patternFail("PropertyFile.readPropertyFile.splitEqs.3")(x)



c_readPropertyFile'46_'35lambda3 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_readPropertyFile'46_'35lambda3 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_readFile(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.PropertyFile.c_readPropertyFile'46_'35lambda3'46_'35lambda4))(st)
c_readPropertyFile'46_'35lambda3 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.List)(st)
c_readPropertyFile'46_'35lambda3 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PropertyFile.c_readPropertyFile'46_'35lambda3(x1)(x)(st))(i)(xs)(st)
c_readPropertyFile'46_'35lambda3 x1 x st = Curry.RunTimeSystem.patternFail("PropertyFile.readPropertyFile._#lambda3")(x)



c_readPropertyFile'46_'35lambda3'46_'35lambda4 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_readPropertyFile'46_'35lambda3'46_'35lambda4 x1 st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Prelude.op_36(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.PropertyFile.c_readPropertyFile'46splitEqs'463))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.pf(Curry.Module.PropertyFile.c_readPropertyFile'46_'35lambda3'46_'35lambda4'46_'35lambda5))))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_lines))(st))(st))(x1)(st))(st)



c_readPropertyFile'46_'35lambda3'46_'35lambda4'46_'35lambda5 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_readPropertyFile'46_'35lambda3'46_'35lambda4'46_'35lambda5 x1 st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.c_not(Curry.Module.Prelude.c_null(x1)(st))(st))(Curry.Module.Char.c_isAlpha(Curry.Module.Prelude.c_head(x1)(st))(st))(st)



c_updatePropertyFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_updatePropertyFile x1 x2 x3 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.PropertyFile.c_readPropertyFile(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.PropertyFile.c_updatePropertyFile'46_'35lambda6(x1)(x2)(x3)))(st)



c_updatePropertyFile'46_'35lambda6 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_updatePropertyFile'46_'35lambda6 x1 x2 x3 x4 st = Curry.Module.PropertyFile.c_updatePropertyFile'46_'35lambda6_case_2(x1)(x2)(x3)(x4)(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_lookup(x2)(x4)(st))(Curry.Module.Prelude.C_Nothing)(st))(st)



c_changePropertyInFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_changePropertyInFile x1 x2 x3 st = Curry.Module.IOExts.c_updateFile(Curry.Module.Prelude.pf(Curry.Module.PropertyFile.c_changePropertyInFile'46_'35lambda7(x2)(x3)))(x1)(st)



c_changePropertyInFile'46changeProp'4621 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_changePropertyInFile'46changeProp'4621 x1 x2 x3 st = let {x4 = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_break(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_61_61))(Curry.Module.Prelude.C_Char('='))))(st))(x3)(st)} in let {x5 = Curry.Module.PropertyFile.c_changePropertyInFile'46changeProp'4621'46_'35selFP3'35s1(x4)(st)} in Curry.Module.PropertyFile.c_changePropertyInFile'46changeProp'4621_case_1(x1)(x2)(x3)(x4)(x5)(Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.c_null(x3)(st))(Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.c_not(Curry.Module.Char.c_isAlpha(Curry.Module.Prelude.c_head(x3)(st))(st))(st))(Curry.Module.Prelude.c_null(Curry.Module.PropertyFile.c_changePropertyInFile'46changeProp'4621'46_'35selFP4'35s2(x4)(st))(st))(st))(st))(st)



c_changePropertyInFile'46changeProp'4621'46_'35selFP3'35s1 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_changePropertyInFile'46changeProp'4621'46_'35selFP3'35s1 x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_changePropertyInFile'46changeProp'4621'46_'35selFP3'35s1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PropertyFile.c_changePropertyInFile'46changeProp'4621'46_'35selFP3'35s1(x)(st))(i)(xs)(st)
c_changePropertyInFile'46changeProp'4621'46_'35selFP3'35s1 x st = Curry.RunTimeSystem.patternFail("PropertyFile.changePropertyInFile.changeProp.21._#selFP3#s1")(x)



c_changePropertyInFile'46changeProp'4621'46_'35selFP4'35s2 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_changePropertyInFile'46changeProp'4621'46_'35selFP4'35s2 x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_changePropertyInFile'46changeProp'4621'46_'35selFP4'35s2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PropertyFile.c_changePropertyInFile'46changeProp'4621'46_'35selFP4'35s2(x)(st))(i)(xs)(st)
c_changePropertyInFile'46changeProp'4621'46_'35selFP4'35s2 x st = Curry.RunTimeSystem.patternFail("PropertyFile.changePropertyInFile.changeProp.21._#selFP4#s2")(x)



c_changePropertyInFile'46_'35lambda7 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_changePropertyInFile'46_'35lambda7 x1 x2 x3 st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_unlines))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.PropertyFile.c_changePropertyInFile'46changeProp'4621(x1)(x2)))))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_lines))(st))(st))(x3)(st)



c_changePropertyInFile'46changeProp'4621_case_1 x1 x2 x3 x4 x5 x6@Curry.Module.Prelude.C_True st = x3
c_changePropertyInFile'46changeProp'4621_case_1 x1 x2 x3 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.PropertyFile.c_changePropertyInFile'46changeProp'4621_case_0(x1)(x2)(x3)(x5)(Curry.Module.Prelude.op_61_61(x5)(x1)(st))(st)
c_changePropertyInFile'46changeProp'4621_case_1 x1 x2 x3 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PropertyFile.c_changePropertyInFile'46changeProp'4621_case_1(x1)(x2)(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c_changePropertyInFile'46changeProp'4621_case_1 x1 x2 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("PropertyFile.changePropertyInFile.changeProp.21_case_1")(x)



c_changePropertyInFile'46changeProp'4621_case_0 x1 x2 x3 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43(x5)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))(Curry.Module.Prelude.List))(x2)(st))(st)
c_changePropertyInFile'46changeProp'4621_case_0 x1 x2 x3 x5 x6@Curry.Module.Prelude.C_False st = x3
c_changePropertyInFile'46changeProp'4621_case_0 x1 x2 x3 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PropertyFile.c_changePropertyInFile'46changeProp'4621_case_0(x1)(x2)(x3)(x5)(x)(st))(i)(xs)(st)
c_changePropertyInFile'46changeProp'4621_case_0 x1 x2 x3 x5 x st = Curry.RunTimeSystem.patternFail("PropertyFile.changePropertyInFile.changeProp.21_case_0")(x)



c_updatePropertyFile'46_'35lambda6_case_2 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_appendFile(x1)(Curry.Module.Prelude.op_43_43(x2)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(st))(st))(st))(st)
c_updatePropertyFile'46_'35lambda6_case_2 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.PropertyFile.c_changePropertyInFile(x1)(x2)(x3)(st)
c_updatePropertyFile'46_'35lambda6_case_2 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PropertyFile.c_updatePropertyFile'46_'35lambda6_case_2(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_updatePropertyFile'46_'35lambda6_case_2 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("PropertyFile.updatePropertyFile._#lambda6_case_2")(x)



c_readPropertyFile'46splitEqs'463_case_4 x2 x3 (Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.PropertyFile.c_readPropertyFile'46splitEqs'463_case_3(x3)(x4)(x5)(st)
c_readPropertyFile'46splitEqs'463_case_4 x2 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PropertyFile.c_readPropertyFile'46splitEqs'463_case_4(x2)(x3)(x)(st))(i)(xs)(st)
c_readPropertyFile'46splitEqs'463_case_4 x2 x3 x st = Curry.RunTimeSystem.patternFail("PropertyFile.readPropertyFile.splitEqs.3_case_4")(x)



c_readPropertyFile'46splitEqs'463_case_3 x3 x4 x5@((Curry.Module.Prelude.:<) x6 x7) st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(x4)(x7))(Curry.Module.PropertyFile.c_readPropertyFile'46splitEqs'463(x3)(st))
c_readPropertyFile'46splitEqs'463_case_3 x3 x4 x5@Curry.Module.Prelude.List st = Curry.Module.PropertyFile.c_readPropertyFile'46splitEqs'463(x3)(st)
c_readPropertyFile'46splitEqs'463_case_3 x3 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PropertyFile.c_readPropertyFile'46splitEqs'463_case_3(x3)(x4)(x)(st))(i)(xs)(st)
c_readPropertyFile'46splitEqs'463_case_3 x3 x4 x st = Curry.RunTimeSystem.patternFail("PropertyFile.readPropertyFile.splitEqs.3_case_3")(x)


