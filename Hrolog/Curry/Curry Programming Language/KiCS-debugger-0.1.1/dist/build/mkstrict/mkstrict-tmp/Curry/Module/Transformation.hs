{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Transformation (module Curry.Module.Transformation) where

import Curry.RunTimeSystem
import Curry.Module.AbstractCurry
import Curry.Module.AbstractHaskell
import Curry.Module.Directory
import Curry.Module.ExternalStubs
import Curry.Module.FlatCurry
import Curry.Module.FlatCurryGoodies
import Curry.Module.FlatToAbstractCurry
import Curry.Module.LiftCases
import Curry.Module.List
import Curry.Module.Make
import Curry.Module.Prelude
import Curry.Module.SrcRef
import Curry.Module.TransformationDebugInfo
import Curry.Module.TransformationExpr
import Curry.Module.TransformationInstances
import Curry.Module.TransformationMonad
import Curry.Module.TransformationPartCalls
import Curry.Module.TransformationPrint
import Curry.Module.TransformationSignatures
import Curry.Module.System
import Curry.Module.IO
import Curry.Module.IOExts
import Curry.Module.Assertion



-- begin included



-- end included

c_transformFlatCurry :: Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_transformFlatCurry x1 x2 x3 x4 x5 st = let {x7 = Curry.Module.Transformation.c_transformProg(x4)(x5)(Curry.Module.SrcRef.c_treeN(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))(st))(Curry.Module.TransformationDebugInfo.c_higherOrderTypes(st))(st)} in Curry.Module.Prelude.op_62_62_61(Curry.Module.Transformation.c_handleExternals(x1)(x3)(x2)(x4)(st))(Curry.Module.Prelude.pf(Curry.Module.Transformation.c_transformFlatCurry'46_'35lambda2(Curry.Module.Transformation.c_transformFlatCurry'46_'35selFP5'35a(x7)(st))(Curry.Module.Transformation.c_transformFlatCurry'46_'35selFP6'35b(x7)(st))(Curry.Module.Transformation.c_transformFlatCurry'46_'35selFP7'35c(x7)(st))(Curry.Module.Transformation.c_transformFlatCurry'46_'35selFP8'35d(x7)(st))(x3)(Curry.Module.Transformation.c_transformFlatCurry'46_'35selFP9'35e(x7)(st))(Curry.Module.Transformation.c_transformFlatCurry'46_'35selFP4'35f(x7)(st))(Curry.Module.Transformation.c_transformFlatCurry'46_'35selFP3'35name(x7)(st))(x2)(x4)))(st)



c_transformFlatCurry'46_'35selFP3'35name :: Curry.Module.AbstractHaskell.C_HaskellProg -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_transformFlatCurry'46_'35selFP3'35name x1@(Curry.Module.AbstractHaskell.C_HaskellProg x2 x3 x4 x5 x6 x7 x8 x9) st = x2
c_transformFlatCurry'46_'35selFP3'35name (Curry.Module.AbstractHaskell.C_HaskellProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformFlatCurry'46_'35selFP3'35name(x)(st))(i)(xs)(st)
c_transformFlatCurry'46_'35selFP3'35name x st = Curry.RunTimeSystem.patternFail("Transformation.transformFlatCurry._#selFP3#name")(x)



c_transformFlatCurry'46_'35selFP4'35f :: Curry.Module.AbstractHaskell.C_HaskellProg -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_transformFlatCurry'46_'35selFP4'35f x1@(Curry.Module.AbstractHaskell.C_HaskellProg x2 x3 x4 x5 x6 x7 x8 x9) st = x3
c_transformFlatCurry'46_'35selFP4'35f (Curry.Module.AbstractHaskell.C_HaskellProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformFlatCurry'46_'35selFP4'35f(x)(st))(i)(xs)(st)
c_transformFlatCurry'46_'35selFP4'35f x st = Curry.RunTimeSystem.patternFail("Transformation.transformFlatCurry._#selFP4#f")(x)



c_transformFlatCurry'46_'35selFP5'35a :: Curry.Module.AbstractHaskell.C_HaskellProg -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_transformFlatCurry'46_'35selFP5'35a x1@(Curry.Module.AbstractHaskell.C_HaskellProg x2 x3 x4 x5 x6 x7 x8 x9) st = x4
c_transformFlatCurry'46_'35selFP5'35a (Curry.Module.AbstractHaskell.C_HaskellProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformFlatCurry'46_'35selFP5'35a(x)(st))(i)(xs)(st)
c_transformFlatCurry'46_'35selFP5'35a x st = Curry.RunTimeSystem.patternFail("Transformation.transformFlatCurry._#selFP5#a")(x)



c_transformFlatCurry'46_'35selFP6'35b :: Curry.Module.AbstractHaskell.C_HaskellProg -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HTypeDecl
c_transformFlatCurry'46_'35selFP6'35b x1@(Curry.Module.AbstractHaskell.C_HaskellProg x2 x3 x4 x5 x6 x7 x8 x9) st = x6
c_transformFlatCurry'46_'35selFP6'35b (Curry.Module.AbstractHaskell.C_HaskellProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformFlatCurry'46_'35selFP6'35b(x)(st))(i)(xs)(st)
c_transformFlatCurry'46_'35selFP6'35b x st = Curry.RunTimeSystem.patternFail("Transformation.transformFlatCurry._#selFP6#b")(x)



c_transformFlatCurry'46_'35selFP7'35c :: Curry.Module.AbstractHaskell.C_HaskellProg -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_InstanceDecl
c_transformFlatCurry'46_'35selFP7'35c x1@(Curry.Module.AbstractHaskell.C_HaskellProg x2 x3 x4 x5 x6 x7 x8 x9) st = x7
c_transformFlatCurry'46_'35selFP7'35c (Curry.Module.AbstractHaskell.C_HaskellProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformFlatCurry'46_'35selFP7'35c(x)(st))(i)(xs)(st)
c_transformFlatCurry'46_'35selFP7'35c x st = Curry.RunTimeSystem.patternFail("Transformation.transformFlatCurry._#selFP7#c")(x)



c_transformFlatCurry'46_'35selFP8'35d :: Curry.Module.AbstractHaskell.C_HaskellProg -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HFuncDecl
c_transformFlatCurry'46_'35selFP8'35d x1@(Curry.Module.AbstractHaskell.C_HaskellProg x2 x3 x4 x5 x6 x7 x8 x9) st = x8
c_transformFlatCurry'46_'35selFP8'35d (Curry.Module.AbstractHaskell.C_HaskellProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformFlatCurry'46_'35selFP8'35d(x)(st))(i)(xs)(st)
c_transformFlatCurry'46_'35selFP8'35d x st = Curry.RunTimeSystem.patternFail("Transformation.transformFlatCurry._#selFP8#d")(x)



c_transformFlatCurry'46_'35selFP9'35e :: Curry.Module.AbstractHaskell.C_HaskellProg -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_COpDecl
c_transformFlatCurry'46_'35selFP9'35e x1@(Curry.Module.AbstractHaskell.C_HaskellProg x2 x3 x4 x5 x6 x7 x8 x9) st = x9
c_transformFlatCurry'46_'35selFP9'35e (Curry.Module.AbstractHaskell.C_HaskellProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformFlatCurry'46_'35selFP9'35e(x)(st))(i)(xs)(st)
c_transformFlatCurry'46_'35selFP9'35e x st = Curry.RunTimeSystem.patternFail("Transformation.transformFlatCurry._#selFP9#e")(x)



c_transformFlatCurry'46_'35lambda2 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HTypeDecl) -> (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_InstanceDecl) -> (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HFuncDecl) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_COpDecl) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_transformFlatCurry'46_'35lambda2 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 st = let {x12 = Curry.Module.TransformationDebugInfo.c_outputFile(x5)(x9)(Curry.Module.Prelude.c_apply(Curry.Module.FlatCurryGoodies.c_progName(st))(x10)(st))(st)} in Curry.Module.Prelude.op_62_62(Curry.Module.TransformationPrint.c_prettyacy(x12)(Curry.Module.AbstractHaskell.C_HaskellProg(x8)(x7)(x1)(x11)(x2)(x3)(x4)(x6))(st))(Curry.Module.Prelude.c_putStrLn(Curry.Module.Prelude.op_43_43(x12)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List))))))))))(st))(st))(st)



c_transformProg :: Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractHaskell.C_HaskellProg
c_transformProg x1@(Curry.Module.FlatCurry.C_Prog x5 x6 x7 x8 x9) x2 x3 x4 st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.FlatCurryGoodies.c_trProg(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))))))(Curry.Module.Transformation.c_transformProg'46transProg'4618(x4)(x3)(x2)(x5)))))(Curry.Module.FlatCurryGoodies.c_updQNamesInProg(Curry.Module.Prelude.pf(Curry.Module.TransformationDebugInfo.c_renameModule))(st))(st))(Curry.Module.LiftCases.c_liftCases(Curry.Module.Prelude.C_False)(x1)(st))(st)
c_transformProg (Curry.Module.FlatCurry.C_ProgOr i xs) x2 x3 x4 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformProg(x)(x2)(x3)(x4)(st))(i)(xs)(st)
c_transformProg x x2 x3 x4 st = Curry.RunTimeSystem.patternFail("Transformation.transformProg")(x)



c_transformProg'46transProg'4618 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> t0 -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_TypeDecl) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) -> t1 -> Curry.RunTimeSystem.State -> Curry.Module.AbstractHaskell.C_HaskellProg
c_transformProg'46transProg'4618 x1 x2 x3 x4 x5 x6 x7 x8 x9 st = let {x10 = Curry.Module.Prelude.c_splitAt(Curry.Module.Prelude.c_length(x7)(st))(Curry.Module.Prelude.c_apply(Curry.Module.SrcRef.c_infoChildren(st))(x2)(st))(st)} in let {x11 = Curry.Module.Transformation.c_transformProg'46transProg'4618'46_'35selFP17'35typeRefs(x10)(st)} in let {x13 = Curry.Module.Transformation.c_transformTypeDecls(x7)(x11)(x1)(st)} in let {x17 = Curry.Module.TransformationInstances.c_instancesGenerics(x7)(x1)(st)} in let {x20 = Curry.Module.Prelude.op_43_43(Curry.Module.TransformationDebugInfo.c_modulePrefix(st))(x4)(st)} in Curry.Module.AbstractHaskell.C_HaskellProg(x20)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('{'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('#'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('G'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('U'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('G'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('V'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('#'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('}'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.List.c_delete(x20)))(Curry.Module.Prelude.c_apply(Curry.Module.Transformation.c_transformImports(st))(x6)(st))(st))(Curry.Module.Prelude.List)(Curry.Module.Transformation.c_transformProg'46transProg'4618'46_'35selFP15'35types'39(x13)(st))(Curry.Module.Prelude.op_43_43(Curry.Module.Transformation.c_transformProg'46transProg'4618'46_'35selFP13'35instsGenerics(x17)(st))(Curry.Module.TransformationInstances.c_instancesGenTerm(x7)(x11)(x1)(st))(st))(Curry.Module.Prelude.op_43_43(Curry.Module.Transformation.c_transformProg'46transProg'4618'46_'35selFP14'35funcsGenerics(x17)(st))(Curry.Module.Prelude.op_43_43(Curry.Module.Transformation.c_transformFuncs(x8)(Curry.Module.Transformation.c_transformProg'46transProg'4618'46_'35selFP18'35funcRefs(x10)(st))(x3)(x1)(st))(Curry.Module.Prelude.op_43_43(Curry.Module.Transformation.c_transformProg'46transProg'4618'46_'35selFP16'35pcConsTerms(x13)(st))(Curry.Module.Prelude.c_apply(Curry.Module.Transformation.c_createPCHelpers(x20)(st))(x8)(st))(st))(st))(st))(Curry.Module.Prelude.List)



c_transformProg'46transProg'4618'46_'35selFP17'35typeRefs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)))) (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)))
c_transformProg'46transProg'4618'46_'35selFP17'35typeRefs x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_transformProg'46transProg'4618'46_'35selFP17'35typeRefs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformProg'46transProg'4618'46_'35selFP17'35typeRefs(x)(st))(i)(xs)(st)
c_transformProg'46transProg'4618'46_'35selFP17'35typeRefs x st = Curry.RunTimeSystem.patternFail("Transformation.transformProg.transProg.18._#selFP17#typeRefs")(x)



c_transformProg'46transProg'4618'46_'35selFP18'35funcRefs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)))) (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)))
c_transformProg'46transProg'4618'46_'35selFP18'35funcRefs x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_transformProg'46transProg'4618'46_'35selFP18'35funcRefs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformProg'46transProg'4618'46_'35selFP18'35funcRefs(x)(st))(i)(xs)(st)
c_transformProg'46transProg'4618'46_'35selFP18'35funcRefs x st = Curry.RunTimeSystem.patternFail("Transformation.transformProg.transProg.18._#selFP18#funcRefs")(x)



c_transformProg'46transProg'4618'46_'35selFP15'35types'39 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HTypeDecl) (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HFuncDecl)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HTypeDecl
c_transformProg'46transProg'4618'46_'35selFP15'35types'39 x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_transformProg'46transProg'4618'46_'35selFP15'35types'39 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformProg'46transProg'4618'46_'35selFP15'35types'39(x)(st))(i)(xs)(st)
c_transformProg'46transProg'4618'46_'35selFP15'35types'39 x st = Curry.RunTimeSystem.patternFail("Transformation.transformProg.transProg.18._#selFP15#types'")(x)



c_transformProg'46transProg'4618'46_'35selFP16'35pcConsTerms :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HTypeDecl) (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HFuncDecl)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HFuncDecl
c_transformProg'46transProg'4618'46_'35selFP16'35pcConsTerms x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_transformProg'46transProg'4618'46_'35selFP16'35pcConsTerms (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformProg'46transProg'4618'46_'35selFP16'35pcConsTerms(x)(st))(i)(xs)(st)
c_transformProg'46transProg'4618'46_'35selFP16'35pcConsTerms x st = Curry.RunTimeSystem.patternFail("Transformation.transformProg.transProg.18._#selFP16#pcConsTerms")(x)



c_transformProg'46transProg'4618'46_'35selFP13'35instsGenerics :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_InstanceDecl) (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HFuncDecl)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_InstanceDecl
c_transformProg'46transProg'4618'46_'35selFP13'35instsGenerics x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_transformProg'46transProg'4618'46_'35selFP13'35instsGenerics (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformProg'46transProg'4618'46_'35selFP13'35instsGenerics(x)(st))(i)(xs)(st)
c_transformProg'46transProg'4618'46_'35selFP13'35instsGenerics x st = Curry.RunTimeSystem.patternFail("Transformation.transformProg.transProg.18._#selFP13#instsGenerics")(x)



c_transformProg'46transProg'4618'46_'35selFP14'35funcsGenerics :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_InstanceDecl) (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HFuncDecl)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HFuncDecl
c_transformProg'46transProg'4618'46_'35selFP14'35funcsGenerics x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_transformProg'46transProg'4618'46_'35selFP14'35funcsGenerics (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformProg'46transProg'4618'46_'35selFP14'35funcsGenerics(x)(st))(i)(xs)(st)
c_transformProg'46transProg'4618'46_'35selFP14'35funcsGenerics x st = Curry.RunTimeSystem.patternFail("Transformation.transformProg.transProg.18._#selFP14#funcsGenerics")(x)



c_transformImports :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_transformImports st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc((Curry.Module.Prelude.:<)(Curry.Module.FlatToAbstractCurry.c_prelude(st))))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc((Curry.Module.Prelude.:<)(Curry.Module.TransformationDebugInfo.c_debugMonadImport(st))))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc((Curry.Module.Prelude.:<)(Curry.Module.TransformationDebugInfo.c_debugInfoImport(st))))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc((Curry.Module.Prelude.:<)(Curry.Module.TransformationPartCalls.c_partCallsImport(st))))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc((Curry.Module.Prelude.:<)(Curry.Module.TransformationDebugInfo.c_dataGenericsImport(st))))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Transformation.c_transformImports'46_'35lambda3))))(st))(st))(st))(st))(st)



c_transformImports'46_'35lambda3 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_transformImports'46_'35lambda3 x1 st = Curry.Module.Prelude.op_43_43(Curry.Module.TransformationDebugInfo.c_modulePrefix(st))(x1)(st)



c_transformTypeDecls :: (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_TypeDecl) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)))) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HTypeDecl) (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HFuncDecl)
c_transformTypeDecls x1@Curry.Module.Prelude.List x2 x3 st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)
c_transformTypeDecls x1@((Curry.Module.Prelude.:<) x4 x5) x2 x3 st = Curry.Module.Transformation.c_transformTypeDecls_case_24(x2)(x3)(x5)(x4)(st)
c_transformTypeDecls (Curry.Module.Prelude.ListOr i xs) x2 x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformTypeDecls(x)(x2)(x3)(st))(i)(xs)(st)
c_transformTypeDecls x x2 x3 st = Curry.RunTimeSystem.patternFail("Transformation.transformTypeDecls")(x)



c_transformTypeDecls'46_'35selFP20'35ttds :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HTypeDecl) (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HFuncDecl)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HTypeDecl
c_transformTypeDecls'46_'35selFP20'35ttds x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_transformTypeDecls'46_'35selFP20'35ttds (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformTypeDecls'46_'35selFP20'35ttds(x)(st))(i)(xs)(st)
c_transformTypeDecls'46_'35selFP20'35ttds x st = Curry.RunTimeSystem.patternFail("Transformation.transformTypeDecls._#selFP20#ttds")(x)



c_transformTypeDecls'46_'35selFP21'35pcs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HTypeDecl) (Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HFuncDecl)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HFuncDecl
c_transformTypeDecls'46_'35selFP21'35pcs x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_transformTypeDecls'46_'35selFP21'35pcs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformTypeDecls'46_'35selFP21'35pcs(x)(st))(i)(xs)(st)
c_transformTypeDecls'46_'35selFP21'35pcs x st = Curry.RunTimeSystem.patternFail("Transformation.transformTypeDecls._#selFP21#pcs")(x)



c_createPCConsTerms :: (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_ConsDecl) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HFuncDecl
c_createPCConsTerms x1@Curry.Module.Prelude.List x2 st = Curry.Module.Prelude.List
c_createPCConsTerms x1@((Curry.Module.Prelude.:<) x3 x4) x2 st = Curry.Module.Transformation.c_createPCConsTerms_case_19(x3)(x4)(x2)(st)
c_createPCConsTerms (Curry.Module.Prelude.ListOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_createPCConsTerms(x)(x2)(st))(i)(xs)(st)
c_createPCConsTerms x x2 st = Curry.RunTimeSystem.patternFail("Transformation.createPCConsTerms")(x)



c_transformTypeDecl :: Curry.Module.FlatCurry.C_TypeDecl -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractHaskell.C_HTypeDecl
c_transformTypeDecl x1@(Curry.Module.FlatCurry.C_Type x3 x4 x5 x6) x2 st = let {x7 = Curry.Module.Prelude.T2(Curry.Module.Prelude.c_negate(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.TransformationDebugInfo.c_debugTVarName(st))} in let {x8 = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.FlatToAbstractCurry.c_convertTypeVariable))(x5)(st)} in let {x9 = Curry.Module.Transformation.c_transformTypeDecl_case_15(x2)(x3)(x7)(x8)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(x3)(st))(x2)(st))(st)} in let {x10 = Curry.Module.TransformationDebugInfo.c_renameType(x3)(st)} in let {x11 = Curry.Module.Transformation.c_transformTypeDecl'46_'35selFP23'35qn'39(x10)(st)} in let {x12 = Curry.Module.Transformation.c_transformTypeDecl'46_'35selFP24'35mod(x10)(st)} in let {x13 = Curry.Module.Transformation.c_transformTypeDecl'46_'35selFP25'35name(x10)(st)} in Curry.Module.AbstractHaskell.C_HTypeDecl(Curry.Module.Transformation.c_transformTypeDecl_case_17(x2)(x3)(x7)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(x3)(st))(x2)(st))(st))(Curry.Module.AbstractCurry.C_CType(x11)(Curry.Module.FlatToAbstractCurry.c_convertVisibility(x4)(st))(x9)((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CCons(Curry.Module.Prelude.T2(x12)(Curry.Module.Prelude.op_43_43(x13)(Curry.Module.TransformationDebugInfo.c_failSuffix(st))(st)))(Curry.Module.Prelude.C_Zero)(Curry.Module.AbstractCurry.C_Public)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CCons(Curry.Module.Prelude.T2(x12)(Curry.Module.Prelude.op_43_43(x13)(Curry.Module.TransformationDebugInfo.c_orSuffix(st))(st)))(Curry.Module.Prelude.C_Zero)(Curry.Module.AbstractCurry.C_Public)((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CTCons(Curry.Module.Prelude.T2(Curry.Module.TransformationDebugInfo.c_debugMonadAs(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))(Curry.Module.Prelude.List)))))))(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CTCons(Curry.Module.Prelude.T2(Curry.Module.FlatToAbstractCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CTCons(x11)(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CTVar))(x9)(st)))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CCons(Curry.Module.Prelude.T2(x12)(Curry.Module.Prelude.op_43_43(x13)(Curry.Module.TransformationDebugInfo.c_underscoreSuffix(st))(st)))(Curry.Module.Prelude.C_Zero)(Curry.Module.AbstractCurry.C_Public)(Curry.Module.Prelude.List))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Transformation.c_transformTypeDecl'46_'35lambda4(x2)))(x6)(st))))))(Curry.Module.Transformation.c_transformTypeDecl_case_16(x6)(Curry.Module.Prelude.op_60(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.c_max))(Curry.Module.Prelude.C_Zero)))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Transformation.c_transformTypeDecl'46numArgs'4671))))(st))(x6)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(st))(st))
c_transformTypeDecl (Curry.Module.FlatCurry.C_TypeDeclOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformTypeDecl(x)(x2)(st))(i)(xs)(st)
c_transformTypeDecl x x2 st = Curry.RunTimeSystem.patternFail("Transformation.transformTypeDecl")(x)



c_transformTypeDecl'46_'35selFP23'35qn'39 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_transformTypeDecl'46_'35selFP23'35qn'39 x1@(Curry.Module.Prelude.T2 x2 x3) st = x1
c_transformTypeDecl'46_'35selFP23'35qn'39 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformTypeDecl'46_'35selFP23'35qn'39(x)(st))(i)(xs)(st)
c_transformTypeDecl'46_'35selFP23'35qn'39 x st = Curry.RunTimeSystem.patternFail("Transformation.transformTypeDecl._#selFP23#qn'")(x)



c_transformTypeDecl'46_'35selFP24'35mod :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_transformTypeDecl'46_'35selFP24'35mod x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_transformTypeDecl'46_'35selFP24'35mod (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformTypeDecl'46_'35selFP24'35mod(x)(st))(i)(xs)(st)
c_transformTypeDecl'46_'35selFP24'35mod x st = Curry.RunTimeSystem.patternFail("Transformation.transformTypeDecl._#selFP24#mod")(x)



c_transformTypeDecl'46_'35selFP25'35name :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_transformTypeDecl'46_'35selFP25'35name x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_transformTypeDecl'46_'35selFP25'35name (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformTypeDecl'46_'35selFP25'35name(x)(st))(i)(xs)(st)
c_transformTypeDecl'46_'35selFP25'35name x st = Curry.RunTimeSystem.patternFail("Transformation.transformTypeDecl._#selFP25#name")(x)



c_transformTypeDecl'46numArgs'4671 :: Curry.Module.FlatCurry.C_ConsDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_transformTypeDecl'46numArgs'4671 x1@(Curry.Module.FlatCurry.C_Cons x2 x3 x4 x5) st = x3
c_transformTypeDecl'46numArgs'4671 (Curry.Module.FlatCurry.C_ConsDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformTypeDecl'46numArgs'4671(x)(st))(i)(xs)(st)
c_transformTypeDecl'46numArgs'4671 x st = Curry.RunTimeSystem.patternFail("Transformation.transformTypeDecl.numArgs.71")(x)



c_transformTypeDecl'46_'35lambda4 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.FlatCurry.C_ConsDecl -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CConsDecl
c_transformTypeDecl'46_'35lambda4 x1 x2 st = Curry.Module.Transformation.c_transformCons(x2)(x1)(st)



c_transformCons :: Curry.Module.FlatCurry.C_ConsDecl -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CConsDecl
c_transformCons x1@(Curry.Module.FlatCurry.C_Cons x3 x4 x5 x6) x2 st = Curry.Module.AbstractCurry.C_CCons(Curry.Module.TransformationDebugInfo.c_renameCons(x3)(st))(x4)(Curry.Module.FlatToAbstractCurry.c_convertVisibility(x5)(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Transformation.c_transformCons'46transformType'39'4695(x2)))(x6)(st))
c_transformCons (Curry.Module.FlatCurry.C_ConsDeclOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformCons(x)(x2)(st))(i)(xs)(st)
c_transformCons x x2 st = Curry.RunTimeSystem.patternFail("Transformation.transformCons")(x)



c_transformCons'46transformType'39'4695 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.FlatCurry.C_TypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CTypeExpr
c_transformCons'46transformType'39'4695 x1 x2@(Curry.Module.FlatCurry.C_TVar x3) st = Curry.Module.FlatToAbstractCurry.c_convertType(x2)(st)
c_transformCons'46transformType'39'4695 x1 x2@(Curry.Module.FlatCurry.C_FuncType x4 x5) st = Curry.Module.TransformationSignatures.c_transformType(Curry.Module.Prelude.c_negate(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x2)(x1)(st)
c_transformCons'46transformType'39'4695 x1 x2@(Curry.Module.FlatCurry.C_TCons x6 x7) st = Curry.Module.TransformationSignatures.c_transformType(Curry.Module.Prelude.c_negate(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x2)(x1)(st)
c_transformCons'46transformType'39'4695 x1 (Curry.Module.FlatCurry.C_TypeExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformCons'46transformType'39'4695(x1)(x)(st))(i)(xs)(st)
c_transformCons'46transformType'39'4695 x1 x st = Curry.RunTimeSystem.patternFail("Transformation.transformCons.transformType'.95")(x)



c_transformFuncs :: (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)))) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HFuncDecl
c_transformFuncs x1@Curry.Module.Prelude.List x2 x3 x4 st = Curry.Module.Prelude.List
c_transformFuncs x1@((Curry.Module.Prelude.:<) x5 x6) x2 x3 x4 st = Curry.Module.Transformation.c_transformFuncs_case_14(x2)(x3)(x4)(x6)(x5)(st)
c_transformFuncs (Curry.Module.Prelude.ListOr i xs) x2 x3 x4 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformFuncs(x)(x2)(x3)(x4)(st))(i)(xs)(st)
c_transformFuncs x x2 x3 x4 st = Curry.RunTimeSystem.patternFail("Transformation.transformFuncs")(x)



c_createPCHelpers :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_HFuncDecl)
c_createPCHelpers x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.TransformationPartCalls.c_mkPartCallHelper(x1)))))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_62))(Curry.Module.TransformationPartCalls.c_arityThreshold(st))))))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.List.c_nub))(Curry.Module.Transformation.c_calcMissings(st))(st))(st))(st)



c_calcMissings :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)
c_calcMissings st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_concat))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Transformation.c_calcMissings'46calcMissings'39'46140(Curry.Module.Prelude.pf(Curry.Module.FlatCurryGoodies.c_trExpr(Curry.Module.Prelude.pf(Curry.Module.Transformation.c_calcMissings'46var'46140))(Curry.Module.Prelude.pf(Curry.Module.Transformation.c_calcMissings'46lit'46140))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Transformation.c_calcMissings'46comb'46140))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Transformation.c_calcMissings'46leT'46140))(Curry.Module.Prelude.pf(Curry.Module.Transformation.c_calcMissings'46freE'46140))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_43_43))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Transformation.c_calcMissings'46casE'46140))(Curry.Module.Prelude.pf(Curry.Module.Transformation.c_calcMissings'46bran'46140))))))))(st)



c_calcMissings'46var'46140 :: (Curry t0,Curry t1) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1
c_calcMissings'46var'46140 x1 st = Curry.Module.Prelude.List



c_calcMissings'46lit'46140 :: (Curry t0,Curry t1) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1
c_calcMissings'46lit'46140 x1 st = Curry.Module.Prelude.List



c_calcMissings'46comb'46140 :: (Curry t0) => Curry.Module.FlatCurry.C_CombType -> t0 -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_calcMissings'46comb'46140 x1 x2 x3 st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.FlatCurryGoodies.c_missingArgs(st))(x1)(st))(Curry.Module.Prelude.c_concat(x3)(st))



c_calcMissings'46leT'46140 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 (Curry.Module.Prelude.List t1))) -> (Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1
c_calcMissings'46leT'46140 x1 x2 st = Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_concat))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))))(st))(x1)(st))(x2)(st)



c_calcMissings'46freE'46140 :: (Curry t0,Curry t1) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t1)
c_calcMissings'46freE'46140 x1 st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id)



c_calcMissings'46casE'46140 :: (Curry t0,Curry t1) => t0 -> (Curry.Module.Prelude.List t1) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1
c_calcMissings'46casE'46140 x1 x2 x3 st = Curry.Module.Prelude.op_43_43(x2)(Curry.Module.Prelude.c_concat(x3)(st))(st)



c_calcMissings'46bran'46140 :: (Curry t0,Curry t1) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t1)
c_calcMissings'46bran'46140 x1 st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id)



c_calcMissings'46calcMissings'39'46140 :: (Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)) -> Curry.Module.FlatCurry.C_FuncDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_calcMissings'46calcMissings'39'46140 x1 x2@(Curry.Module.FlatCurry.C_Func x3 x4 x5 x6 x7) st = Curry.Module.Transformation.c_calcMissings'46calcMissings'39'46140_case_7(x1)(x7)(st)
c_calcMissings'46calcMissings'39'46140 x1 (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_calcMissings'46calcMissings'39'46140(x1)(x)(st))(i)(xs)(st)
c_calcMissings'46calcMissings'39'46140 x1 x st = Curry.RunTimeSystem.patternFail("Transformation.calcMissings.calcMissings'.140")(x)



c_createHookHelper :: Curry.Module.FlatCurry.C_FuncDecl -> (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractHaskell.C_HFuncDecl
c_createHookHelper x1@(Curry.Module.FlatCurry.C_Func x3 x4 x5 x6 x7) x2 st = Curry.Module.Transformation.c_createHookHelper_case_6(x2)(x4)(x3)(st)
c_createHookHelper (Curry.Module.FlatCurry.C_FuncDeclOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_createHookHelper(x)(x2)(st))(i)(xs)(st)
c_createHookHelper x x2 st = Curry.RunTimeSystem.patternFail("Transformation.createHookHelper")(x)



c_createHookHelper'46insertHook'46181 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_createHookHelper'46insertHook'46181 x1 x2 x3 st = Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_comb(Curry.Module.Prelude.T2(Curry.Module.TransformationDebugInfo.c_debugMonadAs(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('H'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))(Curry.Module.Prelude.List))))))))))))))(st))((Curry.Module.Prelude.:<)(Curry.Module.FlatToAbstractCurry.c_acyStr(x1)(st))((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))))(st)



c_createPCTermGen :: Curry.Module.FlatCurry.C_FuncDecl -> (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractHaskell.C_HFuncDecl
c_createPCTermGen x1@(Curry.Module.FlatCurry.C_Func x3 x4 x5 x6 x7) x2 st = Curry.Module.Transformation.c_createPCTermGen_case_5(x2)(x3)(st)
c_createPCTermGen (Curry.Module.FlatCurry.C_FuncDeclOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_createPCTermGen(x)(x2)(st))(i)(xs)(st)
c_createPCTermGen x x2 st = Curry.RunTimeSystem.patternFail("Transformation.createPCTermGen")(x)



c_createPCConsTerm :: Curry.Module.FlatCurry.C_ConsDecl -> (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractHaskell.C_HFuncDecl
c_createPCConsTerm x1@(Curry.Module.FlatCurry.C_Cons x3 x4 x5 x6) x2 st = Curry.Module.Transformation.c_createPCConsTerm_case_4(x2)(x3)(st)
c_createPCConsTerm (Curry.Module.FlatCurry.C_ConsDeclOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_createPCConsTerm(x)(x2)(st))(i)(xs)(st)
c_createPCConsTerm x x2 st = Curry.RunTimeSystem.patternFail("Transformation.createPCConsTerm")(x)



c_transformFunc :: Curry.Module.FlatCurry.C_FuncDecl -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) Curry.Module.Prelude.C_Bool))) Curry.Module.AbstractHaskell.C_HFuncDecl)
c_transformFunc x1@(Curry.Module.FlatCurry.C_Func x5 x6 x7 x8 x9) x2 x3 x4 st = Curry.Module.Prelude.pf(Curry.Module.TransformationMonad.c_ret(Curry.Module.AbstractHaskell.C_HFunc(Curry.Module.TransformationDebugInfo.c_renameFunc(x5)(st))(x6)(Curry.Module.FlatToAbstractCurry.c_convertVisibility(x7)(st))((Curry.Module.Prelude.:<)(Curry.Module.TransformationDebugInfo.c_debugMonadConstraint(st))(Curry.Module.Transformation.c_createConstraints(x8)(st)))(Curry.Module.Transformation.c_transformFunc_case_3(x4)(x6)(x8)(x2)(st))(Curry.Module.Transformation.c_transformRule(x9)(x5)(x2)(x3)(st))))
c_transformFunc (Curry.Module.FlatCurry.C_FuncDeclOr i xs) x2 x3 x4 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformFunc(x)(x2)(x3)(x4)(st))(i)(xs)(st)
c_transformFunc x x2 x3 x4 st = Curry.RunTimeSystem.patternFail("Transformation.transformFunc")(x)



c_createConstraints :: Curry.Module.FlatCurry.C_TypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractHaskell.C_TypeClass
c_createConstraints x1 st = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Transformation.c_createConstraints'46createConstraint'46216))(Curry.Module.FlatToAbstractCurry.c_extractTVars(x1)(st))(st)



c_createConstraints'46createConstraint'46216 :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.AbstractHaskell.C_TypeClass
c_createConstraints'46createConstraint'46216 x1 st = Curry.Module.AbstractHaskell.C_TypeClass(Curry.Module.TransformationDebugInfo.c_staticInfoClass(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_tx(st))(x1)(st))(Curry.Module.Prelude.List))



c_transformRule :: Curry.Module.FlatCurry.C_Rule -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CRules
c_transformRule x1@(Curry.Module.FlatCurry.C_Rule x5 x6) x2 x3 x4 st = Curry.Module.Transformation.c_transformRule_case_2(x3)(x4)(x5)(x6)(x2)(st)
c_transformRule (Curry.Module.FlatCurry.C_RuleOr i xs) x2 x3 x4 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformRule(x)(x2)(x3)(x4)(st))(i)(xs)(st)
c_transformRule x x2 x3 x4 st = Curry.RunTimeSystem.patternFail("Transformation.transformRule")(x)



c_transformRule'46_'35selFP27'35srcRefs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_transformRule'46_'35selFP27'35srcRefs x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_transformRule'46_'35selFP27'35srcRefs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformRule'46_'35selFP27'35srcRefs(x)(st))(i)(xs)(st)
c_transformRule'46_'35selFP27'35srcRefs x st = Curry.RunTimeSystem.patternFail("Transformation.transformRule._#selFP27#srcRefs")(x)



c_transformRule'46_'35selFP28'35exprInfoTree :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) (Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.SrcRef.C_AdrTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))
c_transformRule'46_'35selFP28'35exprInfoTree x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_transformRule'46_'35selFP28'35exprInfoTree (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformRule'46_'35selFP28'35exprInfoTree(x)(st))(i)(xs)(st)
c_transformRule'46_'35selFP28'35exprInfoTree x st = Curry.RunTimeSystem.patternFail("Transformation.transformRule._#selFP28#exprInfoTree")(x)



c_transformRule'46insertHook'46220 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_transformRule'46insertHook'46220 x1 x2 x3 st = Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_comb(Curry.Module.Prelude.T2(Curry.Module.TransformationDebugInfo.c_debugMonadAs(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('H'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))(Curry.Module.Prelude.List))))))))))))))(st))((Curry.Module.Prelude.:<)(Curry.Module.FlatToAbstractCurry.c_acyStr(x1)(st))((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))))(st)



c_x :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr)
c_x st = Curry.Module.Prelude.pf(Curry.Module.Transformation.c_x'46_'35lambda7)



c_x'46_'35lambda7 :: Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_x'46_'35lambda7 x1 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.op_36_36(st))(x1)(st))(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CSymbol))(Curry.Module.TransformationDebugInfo.c_renameModule(Curry.Module.Prelude.T2(Curry.Module.FlatToAbstractCurry.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('U'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))))(st))(st))(st)



c_handleExternals :: Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_handleExternals x1 x2 x3 x4@(Curry.Module.FlatCurry.C_Prog x5 x6 x7 x8 x9) st = let {x10 = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Make.c_unless(x1)))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_putStrLn))(st)} in Curry.Module.Transformation.c_handleExternals_case_0(x2)(x3)(x4)(x5)(x10)(Curry.Module.Prelude.c_not(Curry.Module.ExternalStubs.c_containsExt(x4)(st))(st))(st)
c_handleExternals x1 x2 x3 (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_handleExternals(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_handleExternals x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("Transformation.handleExternals")(x)



c_handleExternals'46manualName'46236 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_handleExternals'46manualName'46236 x1 x2 st = Curry.Module.Prelude.op_43_43(x2)(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))))))(st))(st)



c_handleExternals'46stubs'46236 :: Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_handleExternals'46stubs'46236 x1 x2 x3 st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.op_62_62(Curry.Module.ExternalStubs.c_writeStubs(x3)(x1)(st))(Curry.Module.Prelude.c_putStrLn(x2)(st))(st))(Curry.Module.Prelude.c_return(x3)(st))(st)



c_handleExternals'46found'46236 :: (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_handleExternals'46found'46236 x1 x2 st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_apply(x1)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))(x2)(st))(st))(Curry.Module.Prelude.c_return(x2)(st))(st)



c_handleExternals'46test'39'46236 :: Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_handleExternals'46test'39'46236 x1 x2 x3 x4 x5@Curry.Module.Prelude.List st = Curry.Module.Transformation.c_handleExternals'46stubs'46236(x1)(x3)(x4)(st)
c_handleExternals'46test'39'46236 x1 x2 x3 x4 x5@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.op_36(x2)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))))))))))))))(x6)(st))(st))(Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesFileExist(x6)(st))(Curry.Module.Prelude.pf(Curry.Module.Transformation.c_handleExternals'46test'39'46236'46_'35lambda8(x6)(x7)(x1)(x2)(x3)))(st))(st)
c_handleExternals'46test'39'46236 x1 x2 x3 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_handleExternals'46test'39'46236(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_handleExternals'46test'39'46236 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("Transformation.handleExternals.test'.236")(x)



c_handleExternals'46test'39'46236'46_'35lambda8 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_handleExternals'46test'39'46236'46_'35lambda8 x1 x2 x3 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.Transformation.c_handleExternals'46found'46236(x4)(x1)(st)
c_handleExternals'46test'39'46236'46_'35lambda8 x1 x2 x3 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.Transformation.c_handleExternals'46test'39'46236(x3)(x4)(x5)(x1)(x2)(st)
c_handleExternals'46test'39'46236'46_'35lambda8 x1 x2 x3 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_handleExternals'46test'39'46236'46_'35lambda8(x1)(x2)(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c_handleExternals'46test'39'46236'46_'35lambda8 x1 x2 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("Transformation.handleExternals.test'.236._#lambda8")(x)



c_handleExternals'46test'46236 :: Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_handleExternals'46test'46236 x1 x2 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.Transformation.c_handleExternals'46test'39'46236(x1)(x2)(x3)(x5)(x4)(st)
c_handleExternals'46test'46236 x1 x2 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_handleExternals'46test'46236(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_handleExternals'46test'46236 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("Transformation.handleExternals.test.236")(x)



c_handleExternals_case_0 x2 x3 x4 x5 x10 x11@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_apply(x10)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))))))))))))))))))(st))(Curry.Module.Prelude.c_return(Curry.Module.Prelude.List)(st))(st)
c_handleExternals_case_0 x2 x3 x4 x5 x10 x11@Curry.Module.Prelude.C_False st = Curry.Module.Transformation.c_handleExternals'46test'46236(x4)(x10)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('W'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))))))))))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Transformation.c_handleExternals'46manualName'46236(x5)))((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.Prelude.List))))(st))(Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.List)(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pc))((Curry.Module.Prelude.:<)))(Curry.Module.Prelude.List)))(x3)(st))))(st))(st)
c_handleExternals_case_0 x2 x3 x4 x5 x10 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_handleExternals_case_0(x2)(x3)(x4)(x5)(x10)(x)(st))(i)(xs)(st)
c_handleExternals_case_0 x2 x3 x4 x5 x10 x st = Curry.RunTimeSystem.patternFail("Transformation.handleExternals_case_0")(x)



c_transformRule_case_2 x3 x4 x5 x6 x2@(Curry.Module.Prelude.T2 x7 x8) st = let {x10 = Curry.Module.SrcRef.c_nextStaticInfo(x4)(st)} in Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_rules(st))((Curry.Module.Prelude.:<)(Curry.Module.FlatToAbstractCurry.c_simpleRule(Curry.Module.Prelude.c_map(Curry.Module.FlatToAbstractCurry.c_px(st))(x5)(st))(Curry.Module.Prelude.op_36(Curry.Module.TransformationDebugInfo.c_wrapEval(st))(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Transformation.c_transformRule'46insertHook'46220(x8)(Curry.Module.TransformationDebugInfo.c_debugInfo(Curry.Module.TransformationDebugInfo.c_createStaticInfo(x7)(Curry.Module.Transformation.c_transformRule'46_'35selFP27'35srcRefs(x10)(st))(st))(Curry.Module.TransformationDebugInfo.c_simpleDynInfo(Curry.Module.FlatToAbstractCurry.c_list(Curry.Module.Prelude.c_map(Curry.Module.TransformationDebugInfo.c_genTermCallVar(st))(x5)(st))(st))(st))(st))))(Curry.Module.Prelude.op_36(Curry.Module.Transformation.c_transformRule_case_1(x3)(st))(Curry.Module.TransformationMonad.c_runVar(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_freshVariable(st))(x6)(st))(Curry.Module.TransformationExpr.c_transformExpr(x6)(Curry.Module.Prelude.T2(x7)(x8))(x5)(Curry.Module.Transformation.c_transformRule'46_'35selFP28'35exprInfoTree(x10)(st))(st))(st))(st))(st))(st))(st))(Curry.Module.Prelude.List))(st)
c_transformRule_case_2 x3 x4 x5 x6 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformRule_case_2(x3)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_transformRule_case_2 x3 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("Transformation.transformRule_case_2")(x)



c_transformRule_case_1 x3@Curry.Module.Prelude.C_True st = Curry.Module.Transformation.c_x(st)
c_transformRule_case_1 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id)
c_transformRule_case_1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformRule_case_1(x)(st))(i)(xs)(st)
c_transformRule_case_1 x st = Curry.RunTimeSystem.patternFail("Transformation.transformRule_case_1")(x)



c_transformFunc_case_3 x4 x6 x8 x2@Curry.Module.Prelude.C_True st = Curry.Module.FlatToAbstractCurry.c_untyped(st)
c_transformFunc_case_3 x4 x6 x8 x2@Curry.Module.Prelude.C_False st = Curry.Module.TransformationSignatures.c_transformType(x6)(x8)(x4)(st)
c_transformFunc_case_3 x4 x6 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformFunc_case_3(x4)(x6)(x8)(x)(st))(i)(xs)(st)
c_transformFunc_case_3 x4 x6 x8 x st = Curry.RunTimeSystem.patternFail("Transformation.transformFunc_case_3")(x)



c_createPCConsTerm_case_4 x2 x3@(Curry.Module.Prelude.T2 x7 x8) st = Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_untypedFunc(Curry.Module.TransformationDebugInfo.c_pcConsName(x3)(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.FlatToAbstractCurry.c_simpleRule((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_px(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.Prelude.List))(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_comb(Curry.Module.TransformationDebugInfo.c_staticInfoCons(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.FlatToAbstractCurry.c_acyStr(x8)(st))((Curry.Module.Prelude.:<)(Curry.Module.TransformationDebugInfo.c_createStaticInfo(x7)(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_fst))(Curry.Module.SrcRef.c_nextStaticInfo(x2)(st))(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_xx(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.Prelude.List))))(st))(st))(Curry.Module.Prelude.List))(st)
c_createPCConsTerm_case_4 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_createPCConsTerm_case_4(x2)(x)(st))(i)(xs)(st)
c_createPCConsTerm_case_4 x2 x st = Curry.RunTimeSystem.patternFail("Transformation.createPCConsTerm_case_4")(x)



c_createPCTermGen_case_5 x2 x3@(Curry.Module.Prelude.T2 x8 x9) st = Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_untypedFunc(Curry.Module.TransformationDebugInfo.c_pcTermName(x3)(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.FlatToAbstractCurry.c_simpleRule((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_px(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.Prelude.List))(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_comb(Curry.Module.TransformationDebugInfo.c_staticInfoCons(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.FlatToAbstractCurry.c_acyStr(x9)(st))((Curry.Module.Prelude.:<)(Curry.Module.TransformationDebugInfo.c_createStaticInfo(x8)(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_fst))(Curry.Module.SrcRef.c_nextStaticInfo(x2)(st))(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_xx(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.Prelude.List))))(st))(st))(Curry.Module.Prelude.List))(st)
c_createPCTermGen_case_5 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_createPCTermGen_case_5(x2)(x)(st))(i)(xs)(st)
c_createPCTermGen_case_5 x2 x st = Curry.RunTimeSystem.patternFail("Transformation.createPCTermGen_case_5")(x)



c_createHookHelper_case_6 x2 x4 x3@(Curry.Module.Prelude.T2 x8 x9) st = let {x11 = Curry.Module.Prelude.c_enumFromTo(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x4)(st)} in let {x12 = Curry.Module.Prelude.T2(Curry.Module.Prelude.op_43(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))} in Curry.Module.Prelude.c_apply(Curry.Module.FlatToAbstractCurry.c_untypedFunc(Curry.Module.TransformationDebugInfo.c_hookHelperName(x3)(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.FlatToAbstractCurry.c_simpleRule(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_map(Curry.Module.FlatToAbstractCurry.c_px(st))(x11)(st))((Curry.Module.Prelude.:<)(Curry.Module.AbstractCurry.C_CPVar(x12))(Curry.Module.Prelude.List))(st))(Curry.Module.Prelude.op_36(Curry.Module.TransformationDebugInfo.c_wrapEval(st))(Curry.Module.Transformation.c_createHookHelper'46insertHook'46181(x9)(Curry.Module.TransformationDebugInfo.c_debugInfo(Curry.Module.TransformationDebugInfo.c_createStaticInfo(x8)(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_fst))(Curry.Module.SrcRef.c_nextStaticInfo(x2)(st))(st))(st))(Curry.Module.TransformationDebugInfo.c_simpleDynInfo(Curry.Module.FlatToAbstractCurry.c_list(Curry.Module.Prelude.c_map(Curry.Module.TransformationDebugInfo.c_genTermCallVar(st))(x11)(st))(st))(st))(st))(Curry.Module.AbstractCurry.C_CVar(x12))(st))(st))(st))(Curry.Module.Prelude.List))(st)
c_createHookHelper_case_6 x2 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_createHookHelper_case_6(x2)(x4)(x)(st))(i)(xs)(st)
c_createHookHelper_case_6 x2 x4 x st = Curry.RunTimeSystem.patternFail("Transformation.createHookHelper_case_6")(x)



c_calcMissings'46calcMissings'39'46140_case_7 x1 x7@(Curry.Module.FlatCurry.C_Rule x8 x9) st = Curry.Module.Prelude.c_apply(x1)(x9)(st)
c_calcMissings'46calcMissings'39'46140_case_7 x1 x7@(Curry.Module.FlatCurry.C_External x10) st = Curry.Module.Prelude.List
c_calcMissings'46calcMissings'39'46140_case_7 x1 (Curry.Module.FlatCurry.C_RuleOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_calcMissings'46calcMissings'39'46140_case_7(x1)(x)(st))(i)(xs)(st)
c_calcMissings'46calcMissings'39'46140_case_7 x1 x st = Curry.RunTimeSystem.patternFail("Transformation.calcMissings.calcMissings'.140_case_7")(x)



c_transformFuncs_case_14 x2 x3 x4 x6 x5@(Curry.Module.FlatCurry.C_Func x7 x8 x9 x10 x11) st = Curry.Module.Transformation.c_transformFuncs_case_13(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x11)(st)
c_transformFuncs_case_14 x2 x3 x4 x6 (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformFuncs_case_14(x2)(x3)(x4)(x6)(x)(st))(i)(xs)(st)
c_transformFuncs_case_14 x2 x3 x4 x6 x st = Curry.RunTimeSystem.patternFail("Transformation.transformFuncs_case_14")(x)



c_transformFuncs_case_13 x2 x3 x4 x5 x6 x7 x8 x11@(Curry.Module.FlatCurry.C_External x12) st = Curry.Module.Transformation.c_transformFuncs_case_12(x3)(x4)(x5)(x6)(x2)(st)
c_transformFuncs_case_13 x2 x3 x4 x5 x6 x7 x8 x11@(Curry.Module.FlatCurry.C_Rule x17 x18) st = Curry.Module.Transformation.c_transformFuncs_case_11(x2)(x3)(x4)(x5)(x6)(x8)(x7)(st)
c_transformFuncs_case_13 x2 x3 x4 x5 x6 x7 x8 (Curry.Module.FlatCurry.C_RuleOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformFuncs_case_13(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c_transformFuncs_case_13 x2 x3 x4 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("Transformation.transformFuncs_case_13")(x)



c_transformFuncs_case_11 x2 x3 x4 x5 x6 x8 x7@(Curry.Module.Prelude.T2 x19 x20) st = Curry.Module.Transformation.c_transformFuncs_case_10(x3)(x4)(x5)(x6)(x8)(x20)(x2)(st)
c_transformFuncs_case_11 x2 x3 x4 x5 x6 x8 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformFuncs_case_11(x2)(x3)(x4)(x5)(x6)(x8)(x)(st))(i)(xs)(st)
c_transformFuncs_case_11 x2 x3 x4 x5 x6 x8 x st = Curry.RunTimeSystem.patternFail("Transformation.transformFuncs_case_11")(x)



c_transformFuncs_case_10 x3 x4 x5 x6 x8 x20 x2@((Curry.Module.Prelude.:<) x21 x22) st = let {x25 = Curry.Module.Prelude.c_apply(Curry.Module.TransformationMonad.c_run(st))(Curry.Module.Transformation.c_transformFunc(x5)(Curry.Module.Transformation.c_transformFuncs_case_8(x20)(x3)(st))(x21)(x4)(st))(st)} in let {x26 = Curry.Module.Transformation.c_transformFuncs(x6)(x22)(x3)(x4)(st)} in Curry.Module.Transformation.c_transformFuncs_case_9(x5)(x8)(x21)(x25)(x26)(Curry.Module.Prelude.op_62(x8)(Curry.Module.Prelude.C_Zero)(st))(st)
c_transformFuncs_case_10 x3 x4 x5 x6 x8 x20 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformFuncs_case_10(x3)(x4)(x5)(x6)(x8)(x20)(x)(st))(i)(xs)(st)
c_transformFuncs_case_10 x3 x4 x5 x6 x8 x20 x st = Curry.RunTimeSystem.patternFail("Transformation.transformFuncs_case_10")(x)



c_transformFuncs_case_8 x20 x3@Curry.Module.Prelude.C_Nothing st = Curry.Module.Prelude.C_False
c_transformFuncs_case_8 x20 x3@(Curry.Module.Prelude.C_Just x24) st = Curry.Module.Prelude.op_61_61(x20)(x24)(st)
c_transformFuncs_case_8 x20 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformFuncs_case_8(x20)(x)(st))(i)(xs)(st)
c_transformFuncs_case_8 x20 x st = Curry.RunTimeSystem.patternFail("Transformation.transformFuncs_case_8")(x)



c_transformFuncs_case_9 x5 x8 x21 x25 x26 x27@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(Curry.Module.Transformation.c_createPCTermGen(x5)(x21)(st))(x26))
c_transformFuncs_case_9 x5 x8 x21 x25 x26 x27@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(x25)(x26)
c_transformFuncs_case_9 x5 x8 x21 x25 x26 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformFuncs_case_9(x5)(x8)(x21)(x25)(x26)(x)(st))(i)(xs)(st)
c_transformFuncs_case_9 x5 x8 x21 x25 x26 x st = Curry.RunTimeSystem.patternFail("Transformation.transformFuncs_case_9")(x)



c_transformFuncs_case_12 x3 x4 x5 x6 x2@((Curry.Module.Prelude.:<) x13 x14) st = (Curry.Module.Prelude.:<)(Curry.Module.Transformation.c_createHookHelper(x5)(x13)(st))((Curry.Module.Prelude.:<)(Curry.Module.Transformation.c_createPCTermGen(x5)(x13)(st))(Curry.Module.Transformation.c_transformFuncs(x6)(x14)(x3)(x4)(st)))
c_transformFuncs_case_12 x3 x4 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformFuncs_case_12(x3)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_transformFuncs_case_12 x3 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("Transformation.transformFuncs_case_12")(x)



c_transformTypeDecl_case_15 x2 x3 x7 x8 x9@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x7)(x8)
c_transformTypeDecl_case_15 x2 x3 x7 x8 x9@Curry.Module.Prelude.C_False st = x8
c_transformTypeDecl_case_15 x2 x3 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformTypeDecl_case_15(x2)(x3)(x7)(x8)(x)(st))(i)(xs)(st)
c_transformTypeDecl_case_15 x2 x3 x7 x8 x st = Curry.RunTimeSystem.patternFail("Transformation.transformTypeDecl_case_15")(x)



c_transformTypeDecl_case_16 x6 x7@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.TransformationDebugInfo.c_typeableClass(st))((Curry.Module.Prelude.:<)(Curry.Module.TransformationDebugInfo.c_dataClass(st))(Curry.Module.Prelude.List))
c_transformTypeDecl_case_16 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.List
c_transformTypeDecl_case_16 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformTypeDecl_case_16(x6)(x)(st))(i)(xs)(st)
c_transformTypeDecl_case_16 x6 x st = Curry.RunTimeSystem.patternFail("Transformation.transformTypeDecl_case_16")(x)



c_transformTypeDecl_case_17 x2 x3 x7 x8@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.FlatToAbstractCurry.c_constraint(Curry.Module.TransformationDebugInfo.c_debugMonadClass(st))(x7)(st))(Curry.Module.Prelude.List)
c_transformTypeDecl_case_17 x2 x3 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.List
c_transformTypeDecl_case_17 x2 x3 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformTypeDecl_case_17(x2)(x3)(x7)(x)(st))(i)(xs)(st)
c_transformTypeDecl_case_17 x2 x3 x7 x st = Curry.RunTimeSystem.patternFail("Transformation.transformTypeDecl_case_17")(x)



c_createPCConsTerms_case_19 x3 x4 x2@((Curry.Module.Prelude.:<) x5 x6) st = let {x7 = Curry.Module.Transformation.c_createPCConsTerms(x4)(x6)(st)} in Curry.Module.Transformation.c_createPCConsTerms_case_18(x3)(x5)(x7)(Curry.Module.Prelude.op_62(Curry.Module.Prelude.c_apply(Curry.Module.FlatCurryGoodies.c_consArity(st))(x3)(st))(Curry.Module.Prelude.C_Zero)(st))(st)
c_createPCConsTerms_case_19 x3 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_createPCConsTerms_case_19(x3)(x4)(x)(st))(i)(xs)(st)
c_createPCConsTerms_case_19 x3 x4 x st = Curry.RunTimeSystem.patternFail("Transformation.createPCConsTerms_case_19")(x)



c_createPCConsTerms_case_18 x3 x5 x7 x8@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Transformation.c_createPCConsTerm(x3)(x5)(st))(x7)
c_createPCConsTerms_case_18 x3 x5 x7 x8@Curry.Module.Prelude.C_False st = x7
c_createPCConsTerms_case_18 x3 x5 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_createPCConsTerms_case_18(x3)(x5)(x7)(x)(st))(i)(xs)(st)
c_createPCConsTerms_case_18 x3 x5 x7 x st = Curry.RunTimeSystem.patternFail("Transformation.createPCConsTerms_case_18")(x)



c_transformTypeDecls_case_24 x2 x3 x5 x4@(Curry.Module.FlatCurry.C_TypeSyn x6 x7 x8 x9) st = Curry.Module.Transformation.c_transformTypeDecls_case_23(x3)(x5)(x2)(st)
c_transformTypeDecls_case_24 x2 x3 x5 x4@(Curry.Module.FlatCurry.C_Type x12 x13 x14 x15) st = Curry.Module.Transformation.c_transformTypeDecls_case_22(x2)(x3)(x4)(x5)(x15)(st)
c_transformTypeDecls_case_24 x2 x3 x5 (Curry.Module.FlatCurry.C_TypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformTypeDecls_case_24(x2)(x3)(x5)(x)(st))(i)(xs)(st)
c_transformTypeDecls_case_24 x2 x3 x5 x st = Curry.RunTimeSystem.patternFail("Transformation.transformTypeDecls_case_24")(x)



c_transformTypeDecls_case_22 x2 x3 x4 x5 x15@Curry.Module.Prelude.List st = Curry.Module.Transformation.c_transformTypeDecls_case_21(x3)(x5)(x2)(st)
c_transformTypeDecls_case_22 x2 x3 x4 x5 x15@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.Transformation.c_transformTypeDecls_case_20(x3)(x4)(x5)(x15)(x2)(st)
c_transformTypeDecls_case_22 x2 x3 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformTypeDecls_case_22(x2)(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c_transformTypeDecls_case_22 x2 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("Transformation.transformTypeDecls_case_22")(x)



c_transformTypeDecls_case_20 x3 x4 x5 x15 x2@((Curry.Module.Prelude.:<) x20 x21) st = let {x22 = Curry.Module.Transformation.c_transformTypeDecls(x5)(x21)(x3)(st)} in Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Transformation.c_transformTypeDecl(x4)(x3)(st))(Curry.Module.Transformation.c_transformTypeDecls'46_'35selFP20'35ttds(x22)(st)))(Curry.Module.Prelude.op_43_43(Curry.Module.Transformation.c_createPCConsTerms(x15)(Curry.Module.Prelude.op_36(Curry.Module.SrcRef.c_infoChildren(st))(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))(Curry.Module.SrcRef.c_nextStaticInfo(x20)(st))(st))(st))(st))(Curry.Module.Transformation.c_transformTypeDecls'46_'35selFP21'35pcs(x22)(st))(st))
c_transformTypeDecls_case_20 x3 x4 x5 x15 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformTypeDecls_case_20(x3)(x4)(x5)(x15)(x)(st))(i)(xs)(st)
c_transformTypeDecls_case_20 x3 x4 x5 x15 x st = Curry.RunTimeSystem.patternFail("Transformation.transformTypeDecls_case_20")(x)



c_transformTypeDecls_case_21 x3 x5 x2@((Curry.Module.Prelude.:<) x16 x17) st = Curry.Module.Transformation.c_transformTypeDecls(x5)(x17)(x3)(st)
c_transformTypeDecls_case_21 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformTypeDecls_case_21(x3)(x5)(x)(st))(i)(xs)(st)
c_transformTypeDecls_case_21 x3 x5 x st = Curry.RunTimeSystem.patternFail("Transformation.transformTypeDecls_case_21")(x)



c_transformTypeDecls_case_23 x3 x5 x2@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.Transformation.c_transformTypeDecls(x5)(x11)(x3)(st)
c_transformTypeDecls_case_23 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Transformation.c_transformTypeDecls_case_23(x3)(x5)(x)(st))(i)(xs)(st)
c_transformTypeDecls_case_23 x3 x5 x st = Curry.RunTimeSystem.patternFail("Transformation.transformTypeDecls_case_23")(x)


