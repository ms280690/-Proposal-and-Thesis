{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.TransformationDependencies (module Curry.Module.TransformationDependencies) where

import Curry.RunTimeSystem
import Curry.Module.Directory
import Curry.Module.Distribution
import Curry.Module.FileGoodies
import Curry.Module.FlatCurry
import Curry.Module.List
import Curry.Module.Make
import Curry.Module.Maybe
import Curry.Module.Prelude
import Curry.Module.Transformation
import Curry.Module.TransformationDebugInfo
import Curry.Module.System
import Curry.Module.FlatCurryGoodies



-- begin included



-- end included

c_aux2 :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_aux2 st = Curry.Module.TransformationDependencies.c_main(st)



c_main :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_main st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Make.c_parseArgs(st))(Curry.Module.Prelude.pf(Curry.Module.TransformationDependencies.c_main'46_'35lambda2))(st)



c_main'46_'35lambda2 :: Curry.Module.Make.C_Parameter -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_main'46_'35lambda2 x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_getStdLibDir(st))(Curry.Module.Prelude.pf(Curry.Module.TransformationDependencies.c_main'46_'35lambda2'46_'35lambda3(x1)))(st)



c_main'46_'35lambda2'46_'35lambda3 :: Curry.Module.Make.C_Parameter -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_main'46_'35lambda2'46_'35lambda3 x1 x2 st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.c_done(st))(Curry.Module.Prelude.pf(Curry.Module.TransformationDependencies.c_mayCreateDirectory))(Curry.Module.Make.c_output(x1)(st))(st))(Curry.Module.TransformationDependencies.c_transformProgs(x2)(x1)(st))(st)



c_mayCreateDirectory :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_mayCreateDirectory x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesDirectoryExist(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.TransformationDependencies.c_mayCreateDirectory'46_'35lambda4(x1)))(st)



c_mayCreateDirectory'46_'35lambda4 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_mayCreateDirectory'46_'35lambda4 x1 x2 st = Curry.Module.Make.c_unless(x2)(Curry.Module.Directory.c_createDirectory(x1)(st))(st)



c_transformProgs :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Make.C_Parameter -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_transformProgs x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Make.c_make(st))(Curry.Module.Make.c_quiet(x2)(st))(st))(Curry.Module.Make.c_modulename(x2)(st))(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.TransformationDependencies.c_testTransformNecessary(x1)(Curry.Module.Make.c_quiet(x2)(st))(Curry.Module.Make.c_output(x2)(st))))(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.TransformationDependencies.c_transformProgs'46_'35lambda5(x2)))(st)



c_transformProgs'46_'35lambda5 :: Curry.Module.Make.C_Parameter -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.T0) -> Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_transformProgs'46_'35lambda5 x1 x2 x3 x4 st = Curry.Module.Transformation.c_transformFlatCurry(Curry.Module.Make.c_quiet(x1)(st))(Curry.Module.Make.c_output(x1)(st))(x2)(x4)(Curry.Module.Prelude.C_Nothing)(st)



c_testTransformNecessary :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe Curry.Module.Prelude.T0)
c_testTransformNecessary x1 x2 x3 x4 x5 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Make.c_obsolete(x2)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.TransformationDependencies.c_testTransformNecessary'46_'35lambda6(x3)))(Curry.Module.Prelude.c_map(Curry.Module.TransformationDependencies.c_renameFile(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.pf(Curry.Module.TransformationDependencies.c_testTransformNecessary'46_'35lambda7))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_43_43))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(Curry.Module.Prelude.List)))(st))(Curry.Module.Prelude.pf(Curry.Module.TransformationDependencies.c_testTransformNecessary'46_'35lambda8))(x4)(x5)(st))(Curry.Module.Prelude.pf(Curry.Module.TransformationDependencies.c_testTransformNecessary'46_'35lambda9(x1)(x5)(x3)(x4)(x2)))(st)



c_testTransformNecessary'46_'35lambda6 :: (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_testTransformNecessary'46_'35lambda6 x1 x2 x3 st = Curry.Module.TransformationDebugInfo.c_outputFile(x2)(x1)(x3)(st)



c_testTransformNecessary'46_'35lambda7 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_testTransformNecessary'46_'35lambda7 x1 st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))))))(st))(st)



c_testTransformNecessary'46_'35lambda8 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_testTransformNecessary'46_'35lambda8 x1 st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.T0)(st)



c_testTransformNecessary'46_'35lambda9 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.C_Maybe Curry.Module.Prelude.T0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe Curry.Module.Prelude.T0)
c_testTransformNecessary'46_'35lambda9 x1 x2 x3 x4 x5 x6 st = Curry.Module.TransformationDependencies.c_testTransformNecessary'46_'35lambda9_case_0(x1)(x2)(x3)(x4)(x5)(x6)(Curry.Module.Prelude.op_124_124(Curry.Module.Maybe.c_isJust(x6)(st))(Curry.Module.Prelude.op_124_124(Curry.Module.Maybe.c_isJust(x3)(st))(Curry.Module.Prelude.c_not(Curry.Module.List.c_isPrefixOf(x1)(x4)(st))(st))(st))(st))(st)



c_renameFile :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_renameFile st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.FileGoodies.c_replaceFileName)



c_testTransformNecessary'46_'35lambda9_case_0 x1 x2 x3 x4 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_return(x6)(st)
c_testTransformNecessary'46_'35lambda9_case_0 x1 x2 x3 x4 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_62_62(Curry.Module.Make.c_unless(x5)(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_putStrLn))(Curry.Module.Prelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))))))))))))))))))(st))(st))(st))(Curry.Module.Prelude.c_return(Curry.Module.Prelude.C_Just(Curry.Module.Prelude.T0))(st))(st)
c_testTransformNecessary'46_'35lambda9_case_0 x1 x2 x3 x4 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.TransformationDependencies.c_testTransformNecessary'46_'35lambda9_case_0(x1)(x2)(x3)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_testTransformNecessary'46_'35lambda9_case_0 x1 x2 x3 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("TransformationDependencies.testTransformNecessary._#lambda9_case_0")(x)


