{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.FlatCurryRead (module Curry.Module.FlatCurryRead) where

import Curry.RunTimeSystem
import Curry.Module.Directory
import Curry.Module.Distribution
import Curry.Module.FileGoodies
import Curry.Module.FlatCurry
import Curry.Module.Prelude
import Curry.Module.Time
import Curry.Module.System
import Curry.Module.List



-- begin included



-- end included

c_readFlatCurryWithImports :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog)
c_readFlatCurryWithImports x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_getLoadPathForFile(Curry.Module.FlatCurry.c_flatCurryFileName(x1)(st))(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurryRead.c_readFlatCurryWithImports'46_'35lambda2(x1)))(st)



c_readFlatCurryWithImports'46_'35lambda2 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog)
c_readFlatCurryWithImports'46_'35lambda2 x1 x2 st = Curry.Module.FlatCurryRead.c_readFlatCurryFileWithImports(x2)(Curry.Module.FileGoodies.c_baseName(x1)(st))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))(Curry.Module.Prelude.List))(st)



c_readFlatCurryWithImportsInPath :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog)
c_readFlatCurryWithImportsInPath x1 x2 st = Curry.Module.FlatCurryRead.c_readFlatCurryFileWithImports(x1)(x2)((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))(Curry.Module.Prelude.List))(st)



c_readFlatCurryIntWithImports :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog)
c_readFlatCurryIntWithImports x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_getLoadPathForFile(Curry.Module.FlatCurry.c_flatCurryIntName(x1)(st))(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurryRead.c_readFlatCurryIntWithImports'46_'35lambda3(x1)))(st)



c_readFlatCurryIntWithImports'46_'35lambda3 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog)
c_readFlatCurryIntWithImports'46_'35lambda3 x1 x2 st = Curry.Module.FlatCurryRead.c_readFlatCurryFileWithImports(x2)(Curry.Module.FileGoodies.c_baseName(x1)(st))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))(Curry.Module.Prelude.List)))(st)



c_readFlatCurryIntWithImportsInPath :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog)
c_readFlatCurryIntWithImportsInPath x1 x2 st = Curry.Module.FlatCurryRead.c_readFlatCurryFileWithImports(x1)(x2)((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))(Curry.Module.Prelude.List)))(st)



c_readFlatCurryFileWithImports :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog)
c_readFlatCurryFileWithImports x1 x2 x3 st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_putStr((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))(st))(Curry.Module.Prelude.op_62_62_61(Curry.Module.FlatCurryRead.c_tryReadFlatCurryFileWithImports(x1)(x2)(x3)(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurryRead.c_readFlatCurryFileWithImports'46_'35lambda4(x1)(x2)(x3)))(st))(st)



c_readFlatCurryFileWithImports'46_'35lambda4 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog)
c_readFlatCurryFileWithImports'46_'35lambda4 x1 x2 x3 x4 st = Curry.Module.Prelude.c_maybe(Curry.Module.FlatCurryRead.c_parseFlatCurryFileWithImports(x1)(x2)(x3)(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(x4)(st)



c_parseFlatCurryFileWithImports :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog)
c_parseFlatCurryFileWithImports x1 x2 x3 st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_putStrLn))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))))))))))(Curry.Module.Prelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List)))))(st))(st))(st))(Curry.Module.Prelude.op_62_62(Curry.Module.Distribution.c_callFrontendWithParams(Curry.Module.Distribution.C_FCY)(Curry.Module.Distribution.c_setQuiet(Curry.Module.Prelude.C_True)(Curry.Module.Distribution.c_setFullPath(x1)(Curry.Module.Distribution.c_defaultParams(st))(st))(st))(x2)(st))(Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_putStr((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))(st))(Curry.Module.FlatCurryRead.c_parseFlatCurryFileWithImports'46collectMods'4616(x1)(x3)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.Prelude.List)(st))(st))(st))(st)



c_parseFlatCurryFileWithImports'46collectMods'4616 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog)
c_parseFlatCurryFileWithImports'46collectMods'4616 x1 x2 x3@Curry.Module.Prelude.List x4 st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_putStrLn((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))(st))(Curry.Module.Prelude.c_return(Curry.Module.Prelude.List)(st))(st)
c_parseFlatCurryFileWithImports'46collectMods'4616 x1 x2 x3@((Curry.Module.Prelude.:<) x5 x6) x4 st = Curry.Module.FlatCurryRead.c_parseFlatCurryFileWithImports'46collectMods'4616_case_2(x1)(x2)(x4)(x5)(x6)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(x5)(st))(x4)(st))(st)
c_parseFlatCurryFileWithImports'46collectMods'4616 x1 x2 (Curry.Module.Prelude.ListOr i xs) x4 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatCurryRead.c_parseFlatCurryFileWithImports'46collectMods'4616(x1)(x2)(x)(x4)(st))(i)(xs)(st)
c_parseFlatCurryFileWithImports'46collectMods'4616 x1 x2 x x4 st = Curry.RunTimeSystem.patternFail("FlatCurryRead.parseFlatCurryFileWithImports.collectMods.16")(x)



c_parseFlatCurryFileWithImports'46collectMods'4616'46_'35lambda5 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog)
c_parseFlatCurryFileWithImports'46collectMods'4616'46_'35lambda5 x1 x2 x3 x4 x5 x6 st = Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_error))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))(Curry.Module.Prelude.op_43_43(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))(Curry.Module.Prelude.List)))))))))))))(st))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurryRead.c_parseFlatCurryFileWithImports'46collectMods'4616'46_'35lambda5'46_'35lambda6(x1)(x2)(x3)(x4)(x5)))(x6)(st)



c_parseFlatCurryFileWithImports'46collectMods'4616'46_'35lambda5'46_'35lambda6 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog)
c_parseFlatCurryFileWithImports'46collectMods'4616'46_'35lambda5'46_'35lambda6 x1 x2 x3 x4 x5 x6 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_putStr(Curry.Module.Prelude.op_43_43(x6)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(st))(st))(Curry.Module.FlatCurry.c_readFlatCurryFile(x6)(st))(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurryRead.c_parseFlatCurryFileWithImports'46collectMods'4616'46_'35lambda5'46_'35lambda6'46_'35lambda7(x1)(x2)(x3)(x4)(x5)))(st)



c_parseFlatCurryFileWithImports'46collectMods'4616'46_'35lambda5'46_'35lambda6'46_'35lambda7 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog)
c_parseFlatCurryFileWithImports'46collectMods'4616'46_'35lambda5'46_'35lambda6'46_'35lambda7 x1 x2 x3 x4 x5 x6 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.FlatCurryRead.c_parseFlatCurryFileWithImports'46collectMods'4616(x2)(x5)(Curry.Module.Prelude.op_43_43(x4)(Curry.Module.FlatCurryRead.c_importsOf(x6)(st))(st))((Curry.Module.Prelude.:<)(x3)(x1))(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurryRead.c_parseFlatCurryFileWithImports'46collectMods'4616'46_'35lambda5'46_'35lambda6'46_'35lambda7'46_'35lambda8(x6)))(st)



c_parseFlatCurryFileWithImports'46collectMods'4616'46_'35lambda5'46_'35lambda6'46_'35lambda7'46_'35lambda8 :: Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog)
c_parseFlatCurryFileWithImports'46collectMods'4616'46_'35lambda5'46_'35lambda6'46_'35lambda7'46_'35lambda8 x1 x2 st = Curry.Module.Prelude.c_return((Curry.Module.Prelude.:<)(x1)(x2))(st)



c_tryReadFlatCurryFileWithImports :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog))
c_tryReadFlatCurryFileWithImports x1 x2 x3 st = Curry.Module.FlatCurryRead.c_tryReadFlatCurryFileWithImports'46collectMods'4627(x1)(x3)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.Prelude.List)(st)



c_tryReadFlatCurryFileWithImports'46collectMods'4627 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog))
c_tryReadFlatCurryFileWithImports'46collectMods'4627 x1 x2 x3@Curry.Module.Prelude.List x4 st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_putStrLn((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))(st))(Curry.Module.Prelude.c_return(Curry.Module.Prelude.C_Just(Curry.Module.Prelude.List))(st))(st)
c_tryReadFlatCurryFileWithImports'46collectMods'4627 x1 x2 x3@((Curry.Module.Prelude.:<) x5 x6) x4 st = Curry.Module.FlatCurryRead.c_tryReadFlatCurryFileWithImports'46collectMods'4627_case_1(x1)(x2)(x4)(x5)(x6)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(x5)(st))(x4)(st))(st)
c_tryReadFlatCurryFileWithImports'46collectMods'4627 x1 x2 (Curry.Module.Prelude.ListOr i xs) x4 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatCurryRead.c_tryReadFlatCurryFileWithImports'46collectMods'4627(x1)(x2)(x)(x4)(st))(i)(xs)(st)
c_tryReadFlatCurryFileWithImports'46collectMods'4627 x1 x2 x x4 st = Curry.RunTimeSystem.patternFail("FlatCurryRead.tryReadFlatCurryFileWithImports.collectMods.27")(x)



c_tryReadFlatCurryFileWithImports'46collectMods'4627'46_'35lambda9 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.C_Maybe Curry.Module.FlatCurry.C_Prog) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog))
c_tryReadFlatCurryFileWithImports'46collectMods'4627'46_'35lambda9 x1 x2 x3 x4 x5 x6 st = Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.c_return(Curry.Module.Prelude.C_Nothing)(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurryRead.c_tryReadFlatCurryFileWithImports'46collectMods'4627'46_'35lambda9'46_'35lambda10(x1)(x2)(x3)(x4)(x5)))(x6)(st)



c_tryReadFlatCurryFileWithImports'46collectMods'4627'46_'35lambda9'46_'35lambda10 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog))
c_tryReadFlatCurryFileWithImports'46collectMods'4627'46_'35lambda9'46_'35lambda10 x1 x2 x3 x4 x5 x6 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.FlatCurryRead.c_tryReadFlatCurryFileWithImports'46collectMods'4627(x2)(x5)(Curry.Module.Prelude.op_43_43(x4)(Curry.Module.FlatCurryRead.c_importsOf(x6)(st))(st))((Curry.Module.Prelude.:<)(x3)(x1))(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurryRead.c_tryReadFlatCurryFileWithImports'46collectMods'4627'46_'35lambda9'46_'35lambda10'46_'35lambda11(x6)))(st)



c_tryReadFlatCurryFileWithImports'46collectMods'4627'46_'35lambda9'46_'35lambda10'46_'35lambda11 :: Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog))
c_tryReadFlatCurryFileWithImports'46collectMods'4627'46_'35lambda9'46_'35lambda10'46_'35lambda11 x1 x2 st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.C_Nothing)(Curry.Module.Prelude.pf(Curry.Module.FlatCurryRead.c_tryReadFlatCurryFileWithImports'46collectMods'4627'46_'35lambda9'46_'35lambda10'46_'35lambda11'46_'35lambda12(x1)))(x2)(st))(st)



c_tryReadFlatCurryFileWithImports'46collectMods'4627'46_'35lambda9'46_'35lambda10'46_'35lambda11'46_'35lambda12 :: Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog)
c_tryReadFlatCurryFileWithImports'46collectMods'4627'46_'35lambda9'46_'35lambda10'46_'35lambda11'46_'35lambda12 x1 x2 st = Curry.Module.Prelude.C_Just((Curry.Module.Prelude.:<)(x1)(x2))



c_importsOf :: Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_importsOf x1@(Curry.Module.FlatCurry.C_Prog x2 x3 x4 x5 x6) st = x3
c_importsOf (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatCurryRead.c_importsOf(x)(st))(i)(xs)(st)
c_importsOf x st = Curry.RunTimeSystem.patternFail("FlatCurryRead.importsOf")(x)



c_readFlatCurryIfPossible :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe Curry.Module.FlatCurry.C_Prog)
c_readFlatCurryIfPossible x1 x2 x3 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.FileGoodies.c_lookupFileInPath(x2)((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(Curry.Module.Prelude.List)))(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurryRead.c_readFlatCurryIfPossible'46_'35lambda13(x1)(x2)(x3)))(st)



c_readFlatCurryIfPossible'46_'35lambda13 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe Curry.Module.FlatCurry.C_Prog)
c_readFlatCurryIfPossible'46_'35lambda13 x1 x2 x3 x4 st = Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.op_62_62_61(Curry.Module.FileGoodies.c_lookupFileInPath(x2)(x3)(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurryRead.c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda14))(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurryRead.c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16(x2)(x3)))(x4)(st)



c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda14 :: (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe Curry.Module.FlatCurry.C_Prog)
c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda14 x1 st = Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.c_return(Curry.Module.Prelude.C_Nothing)(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurryRead.c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda14'46_'35lambda15))(x1)(st)



c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda14'46_'35lambda15 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe Curry.Module.FlatCurry.C_Prog)
c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda14'46_'35lambda15 x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.FlatCurry.c_readFlatCurryFile(x1)(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Prelude.pc(Curry.Module.Prelude.C_Just))(st))(st)



c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe Curry.Module.FlatCurry.C_Prog)
c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16 x1 x2 x3 st = let {x4 = Curry.Module.FileGoodies.c_dirName(x3)(st)} in Curry.Module.Prelude.op_62_62_61(Curry.Module.FileGoodies.c_lookupFileInPath(x1)(x2)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(Curry.Module.Distribution.c_addCurrySubdir(x4)(st))(Curry.Module.Prelude.List)))(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurryRead.c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16'46_'35lambda17(x3)))(st)



c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16'46_'35lambda17 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe Curry.Module.FlatCurry.C_Prog)
c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16'46_'35lambda17 x1 x2 st = Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.c_return(Curry.Module.Prelude.C_Nothing)(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurryRead.c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16'46_'35lambda17'46_'35lambda18(x1)))(x2)(st)



c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16'46_'35lambda17'46_'35lambda18 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe Curry.Module.FlatCurry.C_Prog)
c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16'46_'35lambda17'46_'35lambda18 x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_getModificationTime(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurryRead.c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16'46_'35lambda17'46_'35lambda18'46_'35lambda19(x2)))(st)



c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16'46_'35lambda17'46_'35lambda18'46_'35lambda19 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Time.C_ClockTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe Curry.Module.FlatCurry.C_Prog)
c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16'46_'35lambda17'46_'35lambda18'46_'35lambda19 x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_getModificationTime(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurryRead.c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16'46_'35lambda17'46_'35lambda18'46_'35lambda19'46_'35lambda20(x2)(x1)))(st)



c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16'46_'35lambda17'46_'35lambda18'46_'35lambda19'46_'35lambda20 :: Curry.Module.Time.C_ClockTime -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Time.C_ClockTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe Curry.Module.FlatCurry.C_Prog)
c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16'46_'35lambda17'46_'35lambda18'46_'35lambda19'46_'35lambda20 x1 x2 x3 st = Curry.Module.FlatCurryRead.c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16'46_'35lambda17'46_'35lambda18'46_'35lambda19'46_'35lambda20_case_0(x1)(x2)(x3)(Curry.Module.Prelude.op_62(Curry.Module.Time.c_clockTimeToInt(x1)(st))(Curry.Module.Time.c_clockTimeToInt(x3)(st))(st))(st)



c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16'46_'35lambda17'46_'35lambda18'46_'35lambda19'46_'35lambda20_case_0 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.C_Nothing)(st)
c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16'46_'35lambda17'46_'35lambda18'46_'35lambda19'46_'35lambda20_case_0 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_putStr(Curry.Module.Prelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(st))(st))(Curry.Module.FlatCurry.c_readFlatCurryFile(x2)(st))(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Prelude.pc(Curry.Module.Prelude.C_Just))(st))(st)
c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16'46_'35lambda17'46_'35lambda18'46_'35lambda19'46_'35lambda20_case_0 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatCurryRead.c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16'46_'35lambda17'46_'35lambda18'46_'35lambda19'46_'35lambda20_case_0(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_readFlatCurryIfPossible'46_'35lambda13'46_'35lambda16'46_'35lambda17'46_'35lambda18'46_'35lambda19'46_'35lambda20_case_0 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("FlatCurryRead.readFlatCurryIfPossible._#lambda13._#lambda16._#lambda17._#lambda18._#lambda19._#lambda20_case_0")(x)



c_tryReadFlatCurryFileWithImports'46collectMods'4627_case_1 x1 x2 x4 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.FlatCurryRead.c_tryReadFlatCurryFileWithImports'46collectMods'4627(x1)(x2)(x6)(x4)(st)
c_tryReadFlatCurryFileWithImports'46collectMods'4627_case_1 x1 x2 x4 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_62_62_61(Curry.Module.FlatCurryRead.c_readFlatCurryIfPossible(x1)(x5)(x2)(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurryRead.c_tryReadFlatCurryFileWithImports'46collectMods'4627'46_'35lambda9(x4)(x1)(x5)(x6)(x2)))(st)
c_tryReadFlatCurryFileWithImports'46collectMods'4627_case_1 x1 x2 x4 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatCurryRead.c_tryReadFlatCurryFileWithImports'46collectMods'4627_case_1(x1)(x2)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_tryReadFlatCurryFileWithImports'46collectMods'4627_case_1 x1 x2 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("FlatCurryRead.tryReadFlatCurryFileWithImports.collectMods.27_case_1")(x)



c_parseFlatCurryFileWithImports'46collectMods'4616_case_2 x1 x2 x4 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.FlatCurryRead.c_parseFlatCurryFileWithImports'46collectMods'4616(x1)(x2)(x6)(x4)(st)
c_parseFlatCurryFileWithImports'46collectMods'4616_case_2 x1 x2 x4 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_62_62_61(Curry.Module.FileGoodies.c_lookupFileInPath(x5)(x2)(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.FlatCurryRead.c_parseFlatCurryFileWithImports'46collectMods'4616'46_'35lambda5(x4)(x1)(x5)(x6)(x2)))(st)
c_parseFlatCurryFileWithImports'46collectMods'4616_case_2 x1 x2 x4 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.FlatCurryRead.c_parseFlatCurryFileWithImports'46collectMods'4616_case_2(x1)(x2)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_parseFlatCurryFileWithImports'46collectMods'4616_case_2 x1 x2 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("FlatCurryRead.parseFlatCurryFileWithImports.collectMods.16_case_2")(x)


