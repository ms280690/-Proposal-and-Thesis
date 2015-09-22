{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleAbstractCurryPrinter (module Curry.Module.OracleAbstractCurryPrinter) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.AbstractCurryPrinter
import Curry.Module.AbstractCurry
import Curry.Module.Char
import Curry.Module.FiniteMap
import Curry.Module.List
import Curry.Module.Maybe
import Curry.Module.Prelude
import Curry.Module.Sort
import Curry.Module.Read
import Curry.Module.OracleAbstractCurry
import Curry.Module.OracleChar
import Curry.Module.OracleFiniteMap
import Curry.Module.OracleList
import Curry.Module.OracleMaybe
import Curry.Module.OraclePrelude
import Curry.Module.OracleSort
import Curry.Module.OracleRead



-- begin included



-- end included

type C_NameFM = Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0

type C_Options = Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)

c_showProg :: Curry.Module.AbstractCurry.C_CurryProg -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showProg x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_289(x2)(x1)(st))(st)



c_defaultOptions :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_defaultOptions x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.T2(Curry.Module.OracleFiniteMap.c_emptyFM(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleAbstractCurryPrinter.c_lessString))(st))(x1)(st))(Curry.Module.Prelude.List))(st)



c_showExports :: (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeDecl) -> (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CFuncDecl) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showExports x2 x3 x1 st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(let {x5 = Curry.Module.OracleList.c_partition(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showExports'46allPublicCons'469))))(Curry.Module.OraclePrelude.c_filter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showExports'46isPublicType'469))))(x2)(x1)(st))(x8)(st)} in let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List)))))))))(Curry.Module.OraclePrelude.c_concat(Curry.Module.OracleList.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_map(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_43_43))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List)))))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showExports'46getTypeName'469))))(x11)(st))(Curry.Module.OracleAbstractCurryPrinter.c_showExports'46_'35selFP3'35withCons(x5)(x9)(st))(x12)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showExports'46getTypeName'469))))(Curry.Module.OracleAbstractCurryPrinter.c_showExports'46_'35selFP4'35withoutCons(x5)(x10)(st))(x13)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showExports'46getFuncName'469))))(Curry.Module.OraclePrelude.c_filter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showExports'46isPublicFunc'469))))(x3)(x14)(st))(x15)(st))(x16)(st))(x17)(st))(x18)(st))(x19)(st))(st))(st))(st))(st))(st)



c_showExports'46isPublicType'469 :: Curry.Module.AbstractCurry.C_CTypeDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_showExports'46isPublicType'469 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_287(x2)(x1)(st))(st)



c_showExports'46isPublicFunc'469 :: Curry.Module.AbstractCurry.C_CFuncDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_showExports'46isPublicFunc'469 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_286(x2)(x1)(st))(st)



c_showExports'46getTypeName'469 :: Curry.Module.AbstractCurry.C_CTypeDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showExports'46getTypeName'469 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_285(x2)(x1)(st))(st)



c_showExports'46allPublicCons'469 :: Curry.Module.AbstractCurry.C_CTypeDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_showExports'46allPublicCons'469 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_282(x2)(x1)(st))(st)



c_showExports'46allPublicCons'469'46isPublicCons'4649 :: Curry.Module.AbstractCurry.C_CConsDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_showExports'46allPublicCons'469'46isPublicCons'4649 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_281(x2)(x1)(st))(st)



c_showExports'46getFuncName'469 :: Curry.Module.AbstractCurry.C_CFuncDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showExports'46getFuncName'469 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_280(x2)(x1)(st))(st)



c_showExports'46_'35selFP3'35withCons :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeDecl) (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeDecl)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeDecl
c_showExports'46_'35selFP3'35withCons x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_277(x2)(x1)(st))(st)



c_showExports'46_'35selFP4'35withoutCons :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeDecl) (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeDecl)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeDecl
c_showExports'46_'35selFP4'35withoutCons x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_276(x2)(x1)(st))(st)



c_showImports :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showImports x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_prefixInter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showImport))))(Curry.Module.OraclePrelude.c_filter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_47_61))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))))))(x2)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x3)(st))(Curry.Module.OracleAbstractCurryPrinter.c__case_275(x2)(Curry.Module.OraclePrelude.op_61_61(x2)((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(Curry.Module.Prelude.List))(x4)(st))(x5)(st))(x6)(st))(st)



c_showImport :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showImport x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_274(x2)(Curry.Module.OraclePrelude.op_47_61(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(x1)(st))(x3)(st))(st)



c_showOpDecls :: (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_COpDecl) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showOpDecls x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_prefixInter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showOpDecl))))(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x1)(st))(Curry.Module.OracleAbstractCurryPrinter.c__case_273(x2)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.List)(x3)(st))(x4)(st))(x5)(st))(st)



c_showOpDecl :: Curry.Module.AbstractCurry.C_COpDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showOpDecl x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_272(x2)(x1)(st))(st)



c_showFixity :: Curry.Module.AbstractCurry.C_CFixity -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFixity x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_269(x2)(x1)(st))(st)



c_showTypeDecls :: (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeDecl) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showTypeDecls x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_prefixInter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showTypeDecl))))(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))(x1)(st))(Curry.Module.OracleAbstractCurryPrinter.c__case_268(x2)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.List)(x3)(st))(x4)(st))(x5)(st))(st)



c_showTypeDecl :: Curry.Module.AbstractCurry.C_CTypeDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showTypeDecl x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_267(x2)(x1)(st))(st)



c_showConsDecl :: Curry.Module.AbstractCurry.C_CConsDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showConsDecl x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_264(x2)(x1)(st))(st)



c_showTypeExpr :: Curry.Module.Prelude.C_Bool -> Curry.Module.AbstractCurry.C_CTypeExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showTypeExpr x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_262(x2)(x3)(x1)(st))(st)



c_showTypeVar :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showTypeVar x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_257(x2)(x1)(st))(st)



c_showIdentifier :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_showIdentifier x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_filter(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_not))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_elem))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))(Curry.Module.Prelude.List)))))))(x1)(st))))))(st)



c_isCFuncType :: Curry.Module.AbstractCurry.C_CTypeExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isCFuncType x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_255(x2)(x1)(st))(st)



c_showFuncDecl :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CFuncDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_showFuncDecl x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showFuncDeclOpt(Curry.Module.OracleAbstractCurryPrinter.c_defaultOptions(x1)(st))))))(st)



c_showFuncDeclOpt :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CFuncDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFuncDeclOpt x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_254(x2)(x3)(x1)(st))(st)



c_showCmtFunc :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.AbstractCurry.C_CFuncDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCmtFunc x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_253(x2)(x3)(x4)(x1)(st))(st)



c_showCmtFunc'46insertName'46139 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCmtFunc'46insertName'46139 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_245(x2)(x3)(x1)(st))(st)



c_funcComment :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_funcComment x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_unlines))))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_lines))))(x1)(st))(x2)(st))(st)



c_showLocalFuncDecl :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CFuncDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_showLocalFuncDecl x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showFuncDeclOpt(x2)))))(st)



c_showRule :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CRule -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showRule x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_244(x2)(x3)(x1)(st))(st)



c_showEvalAnnot :: Curry.Module.AbstractCurry.C_CEvalAnnot -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showEvalAnnot x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_242(x2)(x1)(st))(st)



c_showCrhsList :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.AbstractCurry.C_CExpr Curry.Module.AbstractCurry.C_CExpr)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCrhsList x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_241(x2)(x3)(x1)(st))(st)



c_showCrhs :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.T2 Curry.Module.AbstractCurry.C_CExpr Curry.Module.AbstractCurry.C_CExpr) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCrhs x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_237(x2)(x3)(x1)(st))(st)



c_showLocalDecl :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CLocalDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showLocalDecl x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_236(x2)(x3)(x1)(st))(st)



c_showExpr :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_showExpr x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showExprOpt(Curry.Module.OracleAbstractCurryPrinter.c_defaultOptions(x1)(st))))))(st)



c_showExprOpt :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showExprOpt x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_234(x2)(x3)(x1)(st))(st)



c_showSymbol :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showSymbol x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_231(x3)(x2)(x1)(st))(st)



c_showLambdaOrSection :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CPattern) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showLambdaOrSection x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_226(x2)(x4)(x3)(x1)(st))(st)



c_showLambda :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CPattern) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showLambda x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showPattern))))(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.OracleAbstractCurryPrinter.c_showExprOpt(x2)(x4)(x5)(st))(x6)(st))(x7)(st))(x8)(st))(st)



c_showStatement :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CStatement -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showStatement x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_199(x2)(x3)(x1)(st))(st)



c_showPattern :: Curry.Module.AbstractCurry.C_CPattern -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showPattern x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_196(x2)(x1)(st))(st)



c_showPreludeCons :: Curry.Module.AbstractCurry.C_CPattern -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showPreludeCons x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(let {x4 = Curry.Module.OracleAbstractCurryPrinter.c_showPreludeCons'46_'35selFP6'35name(x2)(x1)(st)} in let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_189(x2)(x4)(Curry.Module.OracleAbstractCurryPrinter.c_showPreludeCons'46_'35selFP7'35pattlist(x2)(x6)(st))(Curry.Module.OraclePrelude.op_61_61(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(x7)(st))(x8)(st))(st))(st))(st))(st)



c_showPreludeCons'46_'35selFP6'35name :: Curry.Module.AbstractCurry.C_CPattern -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showPreludeCons'46_'35selFP6'35name x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_186(x2)(x1)(st))(st)



c_showPreludeCons'46_'35selFP7'35pattlist :: Curry.Module.AbstractCurry.C_CPattern -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CPattern
c_showPreludeCons'46_'35selFP7'35pattlist x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_184(x2)(x1)(st))(st)



c_showPatternList :: Curry.Module.AbstractCurry.C_CPattern -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showPatternList x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_182(x2)(Curry.Module.OracleAbstractCurryPrinter.c_isClosedStringPattern(x2)(x1)(st))(x3)(st))(st)



c_showPatListElems :: Curry.Module.AbstractCurry.C_CPattern -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_showPatListElems x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_178(x2)(x1)(st))(st)



c_isClosedPatternList :: Curry.Module.AbstractCurry.C_CPattern -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isClosedPatternList x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_151(x2)(x1)(st))(st)



c_isClosedStringPattern :: Curry.Module.AbstractCurry.C_CPattern -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isClosedStringPattern x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_124(x2)(x1)(st))(st)



c_isCharPattern :: Curry.Module.AbstractCurry.C_CPattern -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isCharPattern x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_97(x2)(x1)(st))(st)



c_isAsPattern :: Curry.Module.AbstractCurry.C_CPattern -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isAsPattern x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_95(x2)(x1)(st))(st)



c_showAsPatternList :: Curry.Module.AbstractCurry.C_CPattern -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showAsPatternList x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_94(x2)(x1)(st))(st)



c_showBranchExpr :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CBranchExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showBranchExpr x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_92(x2)(x3)(x1)(st))(st)



c_showLiteral :: Curry.Module.AbstractCurry.C_CLiteral -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showLiteral x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_91(x2)(x1)(st))(st)



c_showCCharc :: Curry.Module.AbstractCurry.C_CLiteral -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCCharc x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_90(x2)(x1)(st))(st)



c_showBlock :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showBlock x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))))(Curry.Module.OracleAbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_id))))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))(Curry.Module.OraclePrelude.c_filter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.op_47_61(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.c_lines(x2)(x1)(st))(x3)(st))(x4)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x5)(st))(st)



c_showTypeCons :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeExpr) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showTypeCons x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_84(x2)(x3)(x4)(x1)(st))(st)



c_showPreludeTypeCons :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeExpr) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showPreludeTypeCons x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))))(Curry.Module.OracleAbstractCurryPrinter.c__case_81(x2)(x3)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List)))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_head(x3)(x4)(st))(Curry.Module.AbstractCurry.C_CTCons(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.List))(x5)(st))(x6)(st))(x7)(st))(st)



c_showApplication :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showApplication x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_77(x2)(x3)(Curry.Module.OracleAbstractCurryPrinter.c_applicationHead(x3)(x1)(st))(x4)(st))(st)



c_applicationHead :: Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_applicationHead x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_76(x2)(x1)(st))(st)



c_showSymbolApplication :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showSymbolApplication x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_75(x2)(x4)(x3)(x1)(st))(st)



c_showListApplication :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showListApplication x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_70(x2)(x3)(Curry.Module.OracleAbstractCurryPrinter.c_isStringList(x3)(x1)(st))(x4)(st))(st)



c_showCharListApplication :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCharListApplication x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_67(x2)(x3)(x1)(st))(st)



c_showConsListApplication :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showConsListApplication x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_63(x2)(x3)(x1)(st))(st)



c_showSimpleListApplication :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showSimpleListApplication x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_60(x2)(x3)(x1)(st))(st)



c_showInfixApplication :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showInfixApplication x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_56(x2)(x3)(x4)(x1)(st))(st)



c_showITEApplication :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showITEApplication x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_54(x2)(x3)(x1)(st))(st)



c_showTupleApplication :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showTupleApplication x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showTupleApplication'46p_showTuple'46386(x2)(x3)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x4)(st))(x5)(st))(st)



c_showTupleApplication'46p_showTuple'46386 :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showTupleApplication'46p_showTuple'46386 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_50(x2)(x3)(x1)(st))(st)



c_showSimpleApplication :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showSimpleApplication x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_48(x2)(x3)(x1)(st))(st)



c_showBoxedExpr :: (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showBoxedExpr x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_47(x2)(x3)(Curry.Module.OracleAbstractCurryPrinter.c_isSimpleExpr(x3)(x1)(st))(x4)(st))(st)



c_prefixMap :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_prefixMap x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.op_43_43(x4)))))(x1)(st))(Curry.Module.OraclePrelude.c_map(x2)(x3)(x5)(st))(x6)(st))(st)



c_prefixInter :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_prefixInter x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_concat))))(Curry.Module.OracleList.c_intersperse(x4)(Curry.Module.OraclePrelude.c_map(x2)(x3)(x1)(st))(x5)(st))(x6)(st))(st)



c_combineMap :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_combineMap x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_45(x2)(x4)(x3)(x1)(st))(st)



c_dropTags :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_dropTags x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_44(x2)(x1)(st))(st)



c_isInfixOpName :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))
c_isInfixOpName x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_all(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_elem))))(Curry.Module.OracleAbstractCurryPrinter.c_infixIDs(x1)(st))))))(x2)(st))(st)



c_isStringList :: Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isStringList x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_41(x2)(x1)(st))(st)



c_isClosedList :: Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isClosedList x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_36(x2)(x1)(st))(st)



c_isSimpleExpr :: Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isSimpleExpr x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_31(x2)(x1)(st))(st)



c_isAtom :: Curry.Module.AbstractCurry.C_CExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isAtom x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_12(x2)(x1)(st))(st)



c_isUntyped :: Curry.Module.AbstractCurry.C_CTypeExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isUntyped x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_10(x2)(x1)(st))(st)



c_isTuple :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isTuple x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_7(x2)(x1)(st))(st)



c_isTuple'46p1_isTuple'46492 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isTuple'46p1_isTuple'46492 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_6(x2)(x1)(st))(st)



c_infixIDs :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_infixIDs x1 st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('~'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('@'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('#'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('$'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('%'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('^'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('*'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('+'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('?'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('|'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List)))))))))))))))))))))(st)



c_maybeShowBrackets :: Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_maybeShowBrackets x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c__case_4(x2)(x1)(st))(Curry.Module.OraclePrelude.op_43_43(x3)(Curry.Module.OracleAbstractCurryPrinter.c__case_3(x2)(x4)(st))(x5)(st))(x6)(st))(st)



c_nameFM :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CFuncDecl) -> Curry.RunTimeSystem.State -> Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0))
c_nameFM x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleAbstractCurryPrinter.c_addName))(st))(Curry.Module.OracleFiniteMap.c_emptyFM(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleAbstractCurryPrinter.c_lessString))(st))(x1)(st))))))(st)



c_addName :: Curry.Module.AbstractCurry.C_CFuncDecl -> (Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleFiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0
c_addName x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_2(x3)(x2)(x1)(st))(st)



c_lessString :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_lessString x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.Prelude.C_LT)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OracleSort.c_cmpString(x1)(st))(x2)(x4)(st))(x3)(x5)(st))(x6)(st))(st)



c__case_2 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_2_case__289(x1)(x3)(x2)(st))(st)



c__case_0 x3 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_0_case__288(x1)(x3)(x12)(st))(st)



c__case_1 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_1_case__287(x1)(x3)(x4)(st))(st)



c__case_3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_3_case__286(x1)(x2)(st))(st)



c__case_4 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_4_case__285(x1)(x2)(st))(st)



c__case_6 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_6_case__284(x1)(x2)(st))(st)



c__case_5 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_5_case__283(x1)(x3)(x4)(st))(st)



c__case_7 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_7_case__282(x1)(x2)(st))(st)



c__case_10 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_10_case__281(x1)(x2)(st))(st)



c__case_9 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_9_case__280(x1)(x4)(x3)(st))(st)



c__case_8 x5 x6 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_8_case__279(x1)(x5)(x6)(x4)(st))(st)



c__case_12 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_12_case__278(x1)(x2)(st))(st)



c__case_11 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_11_case__277(x1)(x5)(st))(st)



c__case_31 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_31_case__276(x1)(x2)(st))(st)



c__case_29 x8 x42 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_29_case__275(x1)(x42)(st))(st)



c__case_28 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_28_case__274(x1)(x10)(st))(st)



c__case_27 x12 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_27_case__273(x1)(x12)(x11)(st))(st)



c__case_26 x12 x13 x14 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_26_case__272(x1)(x12)(x14)(x15)(st))(st)



c__case_25 x12 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_25_case__271(x1)(x12)(x14)(st))(st)



c__case_24 x12 x15 x16 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_24_case__270(x1)(x12)(x16)(x17)(st))(st)



c__case_23 x12 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_23_case__269(x1)(x12)(x16)(st))(st)



c__case_22 x12 x17 x18 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_22_case__268(x1)(x12)(x18)(x19)(st))(st)



c__case_21 x12 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_21_case__267(x1)(x12)(x18)(st))(st)



c__case_20 x12 x19 x20 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_20_case__266(x1)(x12)(x20)(x21)(st))(st)



c__case_19 x12 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_19_case__265(x1)(x12)(x20)(st))(st)



c__case_18 x12 x21 x22 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_18_case__264(x1)(x12)(x22)(x23)(st))(st)



c__case_17 x12 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_17_case__263(x1)(x12)(x22)(st))(st)



c__case_16 x12 x23 x24 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_16_case__262(x1)(x12)(x24)(x25)(st))(st)



c__case_15 x12 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_15_case__261(x1)(x12)(x24)(st))(st)



c__case_14 x12 x25 x26 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_14_case__260(x1)(x12)(x26)(x27)(st))(st)



c__case_13 x12 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_13_case__259(x1)(x12)(x26)(st))(st)



c__case_30 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_30_case__258(x1)(x5)(st))(st)



c__case_36 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_36_case__257(x1)(x2)(st))(st)



c__case_32 x35 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_32_case__256(x1)(x35)(st))(st)



c__case_35 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_35_case__255(x1)(x4)(x3)(st))(st)



c__case_34 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_34_case__254(x1)(x4)(x5)(st))(st)



c__case_33 x4 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_33_case__253(x1)(x4)(x7)(st))(st)



c__case_41 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_41_case__252(x1)(x2)(st))(st)



c__case_39 x8 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_39_case__251(x1)(x8)(x7)(st))(st)



c__case_38 x8 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_38_case__250(x1)(x8)(x10)(st))(st)



c__case_37 x8 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_37_case__249(x1)(x8)(x11)(st))(st)



c__case_40 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_40_case__248(x1)(x3)(st))(st)



c__case_44 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_44_case__247(x1)(x2)(st))(st)



c__case_43 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_43_case__246(x1)(x3)(x4)(x5)(st))(st)



c__case_42 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_42_case__245(x1)(x4)(x5)(st))(st)



c__case_45 x2 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_45_case__244(x1)(x2)(x4)(x3)(st))(st)



c__case_47 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_47_case__243(x1)(x2)(x3)(x4)(st))(st)



c__case_46 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_46_case__242(x1)(x2)(x3)(x4)(st))(st)



c__case_48 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_48_case__241(x1)(x2)(x3)(st))(st)



c__case_50 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_50_case__240(x1)(x2)(x3)(st))(st)



c__case_49 x2 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_49_case__239(x1)(x2)(x5)(x4)(st))(st)



c__case_54 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_54_case__238(x1)(x2)(x3)(st))(st)



c__case_53 x2 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_53_case__237(x1)(x2)(x5)(x4)(st))(st)



c__case_52 x2 x4 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_52_case__236(x1)(x2)(x4)(x5)(x7)(x6)(st))(st)



c__case_51 x2 x4 x5 x7 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_51_case__235(x1)(x2)(x4)(x5)(x7)(x9)(x8)(st))(st)



c__case_56 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_56_case__234(x1)(x2)(x3)(x4)(st))(st)



c__case_55 x2 x3 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_55_case__233(x1)(x2)(x3)(x6)(x5)(st))(st)



c__case_60 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_60_case__232(x1)(x2)(x3)(st))(st)



c__case_59 x2 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_59_case__231(x1)(x2)(x5)(x4)(st))(st)



c__case_57 x2 x5 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_57_case__230(x1)(x2)(x5)(x22)(st))(st)



c__case_58 x2 x7 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_58_case__229(x1)(x2)(x7)(x5)(st))(st)



c__case_63 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_63_case__228(x1)(x2)(x3)(st))(st)



c__case_62 x2 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_62_case__227(x1)(x2)(x5)(x4)(st))(st)



c__case_61 x2 x7 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_61_case__226(x1)(x2)(x7)(x5)(st))(st)



c__case_67 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_67_case__225(x1)(x2)(x3)(st))(st)



c__case_66 x2 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_66_case__224(x1)(x2)(x5)(x4)(st))(st)



c__case_65 x2 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_65_case__223(x1)(x2)(x5)(x7)(st))(st)



c__case_64 x2 x8 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_64_case__222(x1)(x2)(x8)(x5)(st))(st)



c__case_70 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_70_case__221(x1)(x2)(x3)(x4)(st))(st)



c__case_69 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_69_case__220(x1)(x2)(x3)(x4)(st))(st)



c__case_68 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_68_case__219(x1)(x2)(x3)(x4)(st))(st)



c__case_75 x2 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_75_case__218(x1)(x2)(x4)(x3)(st))(st)



c__case_74 x2 x4 x5 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_74_case__217(x1)(x2)(x4)(x5)(x6)(x7)(st))(st)



c__case_73 x2 x4 x5 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_73_case__216(x1)(x2)(x4)(x6)(x7)(st))(st)



c__case_72 x2 x4 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_72_case__215(x1)(x2)(x4)(x7)(st))(st)



c__case_71 x2 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_71_case__214(x1)(x2)(x4)(x5)(st))(st)



c__case_76 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_76_case__213(x1)(x2)(st))(st)



c__case_77 x2 x3 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_77_case__212(x1)(x2)(x3)(x18)(st))(st)



c__case_81 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_81_case__211(x1)(x2)(x3)(x4)(st))(st)



c__case_80 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_80_case__210(x1)(x2)(x3)(x4)(st))(st)



c__case_79 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_79_case__209(x1)(x2)(x3)(x4)(st))(st)



c__case_78 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_78_case__208(x1)(x2)(x3)(x4)(st))(st)



c__case_84 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_84_case__207(x1)(x2)(x3)(x4)(st))(st)



c__case_83 x2 x3 x5 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_83_case__206(x1)(x3)(x5)(x6)(x7)(st))(st)



c__case_82 x3 x5 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_82_case__205(x1)(x3)(x5)(x6)(x7)(st))(st)



c__case_90 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_90_case__204(x1)(x2)(st))(st)



c__case_89 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_89_case__203(x1)(x3)(x4)(st))(st)



c__case_88 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_88_case__202(x1)(x3)(x4)(st))(st)



c__case_87 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_87_case__201(x1)(x3)(x4)(st))(st)



c__case_86 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_86_case__200(x1)(x3)(x4)(st))(st)



c__case_85 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_85_case__199(x1)(x3)(x4)(st))(st)



c__case_91 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_91_case__198(x1)(x2)(st))(st)



c__case_92 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_92_case__197(x1)(x2)(x3)(st))(st)



c__case_94 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_94_case__196(x1)(x2)(st))(st)



c__case_93 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_93_case__195(x1)(x4)(x3)(st))(st)



c__case_95 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_95_case__194(x1)(x2)(st))(st)



c__case_97 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_97_case__193(x1)(x2)(st))(st)



c__case_96 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_96_case__192(x1)(x3)(st))(st)



c__case_124 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_124_case__191(x1)(x2)(st))(st)



c__case_123 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_123_case__190(x1)(x4)(x3)(st))(st)



c__case_122 x4 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_122_case__189(x1)(x4)(x6)(x5)(st))(st)



c__case_121 x4 x6 x8 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_121_case__188(x1)(x4)(x6)(x8)(x7)(st))(st)



c__case_120 x4 x6 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_120_case__187(x1)(x4)(x6)(x8)(st))(st)



c__case_119 x4 x6 x10 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_119_case__186(x1)(x4)(x6)(x10)(x9)(st))(st)



c__case_118 x4 x6 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_118_case__185(x1)(x4)(x6)(x10)(st))(st)



c__case_117 x4 x6 x12 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_117_case__184(x1)(x4)(x6)(x12)(x11)(st))(st)



c__case_116 x4 x6 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_116_case__183(x1)(x4)(x6)(x12)(st))(st)



c__case_115 x4 x6 x14 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_115_case__182(x1)(x4)(x6)(x14)(x13)(st))(st)



c__case_114 x4 x6 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_114_case__181(x1)(x4)(x6)(x14)(st))(st)



c__case_113 x4 x6 x16 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_113_case__180(x1)(x4)(x6)(x16)(x15)(st))(st)



c__case_112 x4 x6 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_112_case__179(x1)(x4)(x6)(x16)(st))(st)



c__case_111 x4 x6 x18 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_111_case__178(x1)(x4)(x6)(x18)(x17)(st))(st)



c__case_110 x4 x6 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_110_case__177(x1)(x4)(x6)(x18)(st))(st)



c__case_109 x4 x6 x20 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_109_case__176(x1)(x4)(x6)(x20)(x19)(st))(st)



c__case_108 x4 x6 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_108_case__175(x1)(x4)(x6)(x20)(st))(st)



c__case_107 x4 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_107_case__174(x1)(x4)(x6)(st))(st)



c__case_106 x4 x22 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_106_case__173(x1)(x4)(x22)(x21)(st))(st)



c__case_101 x4 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_101_case__172(x1)(x4)(x22)(st))(st)



c__case_100 x4 x28 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_100_case__171(x1)(x4)(x28)(x27)(st))(st)



c__case_99 x4 x28 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_99_case__170(x1)(x4)(x28)(st))(st)



c__case_98 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_98_case__169(x1)(x4)(st))(st)



c__case_105 x4 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_105_case__168(x1)(x4)(x22)(st))(st)



c__case_104 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_104_case__167(x1)(x4)(st))(st)



c__case_103 x23 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_103_case__166(x1)(x23)(x24)(st))(st)



c__case_102 x23 x25 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_102_case__165(x1)(x23)(x25)(x26)(st))(st)



c__case_151 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_151_case__164(x1)(x2)(st))(st)



c__case_150 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_150_case__163(x1)(x4)(x3)(st))(st)



c__case_149 x4 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_149_case__162(x1)(x4)(x6)(x5)(st))(st)



c__case_148 x4 x6 x8 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_148_case__161(x1)(x4)(x6)(x8)(x7)(st))(st)



c__case_147 x4 x6 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_147_case__160(x1)(x4)(x6)(x8)(st))(st)



c__case_146 x4 x6 x10 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_146_case__159(x1)(x4)(x6)(x10)(x9)(st))(st)



c__case_145 x4 x6 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_145_case__158(x1)(x4)(x6)(x10)(st))(st)



c__case_144 x4 x6 x12 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_144_case__157(x1)(x4)(x6)(x12)(x11)(st))(st)



c__case_143 x4 x6 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_143_case__156(x1)(x4)(x6)(x12)(st))(st)



c__case_142 x4 x6 x14 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_142_case__155(x1)(x4)(x6)(x14)(x13)(st))(st)



c__case_141 x4 x6 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_141_case__154(x1)(x4)(x6)(x14)(st))(st)



c__case_140 x4 x6 x16 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_140_case__153(x1)(x4)(x6)(x16)(x15)(st))(st)



c__case_139 x4 x6 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_139_case__152(x1)(x4)(x6)(x16)(st))(st)



c__case_138 x4 x6 x18 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_138_case__151(x1)(x4)(x6)(x18)(x17)(st))(st)



c__case_137 x4 x6 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_137_case__150(x1)(x4)(x6)(x18)(st))(st)



c__case_136 x4 x6 x20 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_136_case__149(x1)(x4)(x6)(x20)(x19)(st))(st)



c__case_135 x4 x6 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_135_case__148(x1)(x4)(x6)(x20)(st))(st)



c__case_134 x4 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_134_case__147(x1)(x4)(x6)(st))(st)



c__case_133 x4 x22 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_133_case__146(x1)(x4)(x22)(x21)(st))(st)



c__case_128 x4 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_128_case__145(x1)(x4)(x22)(st))(st)



c__case_127 x4 x28 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_127_case__144(x1)(x4)(x28)(x27)(st))(st)



c__case_126 x4 x28 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_126_case__143(x1)(x4)(x28)(st))(st)



c__case_125 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_125_case__142(x1)(x4)(st))(st)



c__case_132 x4 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_132_case__141(x1)(x4)(x22)(st))(st)



c__case_131 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_131_case__140(x1)(x4)(st))(st)



c__case_130 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_130_case__139(x1)(x24)(st))(st)



c__case_129 x25 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_129_case__138(x1)(x25)(x26)(st))(st)



c__case_178 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_178_case__137(x1)(x2)(st))(st)



c__case_177 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_177_case__136(x1)(x4)(x3)(st))(st)



c__case_176 x4 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_176_case__135(x1)(x4)(x6)(x5)(st))(st)



c__case_175 x4 x6 x8 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_175_case__134(x1)(x4)(x6)(x8)(x7)(st))(st)



c__case_174 x4 x6 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_174_case__133(x1)(x4)(x6)(x8)(st))(st)



c__case_173 x4 x6 x10 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_173_case__132(x1)(x4)(x6)(x10)(x9)(st))(st)



c__case_172 x4 x6 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_172_case__131(x1)(x4)(x6)(x10)(st))(st)



c__case_171 x4 x6 x12 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_171_case__130(x1)(x4)(x6)(x12)(x11)(st))(st)



c__case_170 x4 x6 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_170_case__129(x1)(x4)(x6)(x12)(st))(st)



c__case_169 x4 x6 x14 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_169_case__128(x1)(x4)(x6)(x14)(x13)(st))(st)



c__case_168 x4 x6 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_168_case__127(x1)(x4)(x6)(x14)(st))(st)



c__case_167 x4 x6 x16 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_167_case__126(x1)(x4)(x6)(x16)(x15)(st))(st)



c__case_166 x4 x6 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_166_case__125(x1)(x4)(x6)(x16)(st))(st)



c__case_165 x4 x6 x18 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_165_case__124(x1)(x4)(x6)(x18)(x17)(st))(st)



c__case_164 x4 x6 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_164_case__123(x1)(x4)(x6)(x18)(st))(st)



c__case_163 x4 x6 x20 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_163_case__122(x1)(x4)(x6)(x20)(x19)(st))(st)



c__case_162 x4 x6 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_162_case__121(x1)(x4)(x6)(x20)(st))(st)



c__case_161 x4 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_161_case__120(x1)(x4)(x6)(st))(st)



c__case_160 x4 x22 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_160_case__119(x1)(x4)(x22)(x21)(st))(st)



c__case_155 x4 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_155_case__118(x1)(x4)(x22)(st))(st)



c__case_154 x4 x28 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_154_case__117(x1)(x4)(x28)(x27)(st))(st)



c__case_153 x4 x28 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_153_case__116(x1)(x4)(x28)(st))(st)



c__case_152 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_152_case__115(x1)(x4)(st))(st)



c__case_159 x4 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_159_case__114(x1)(x4)(x22)(st))(st)



c__case_158 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_158_case__113(x1)(x4)(st))(st)



c__case_157 x23 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_157_case__112(x1)(x23)(x24)(st))(st)



c__case_156 x23 x25 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_156_case__111(x1)(x23)(x25)(x26)(st))(st)



c__case_182 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_182_case__110(x1)(x2)(x3)(st))(st)



c__case_181 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_181_case__109(x1)(x2)(x3)(st))(st)



c__case_180 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_180_case__108(x1)(x2)(x3)(st))(st)



c__case_179 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_179_case__107(x1)(x2)(x3)(st))(st)



c__case_184 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_184_case__106(x1)(x2)(st))(st)



c__case_183 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_183_case__105(x1)(x4)(x3)(st))(st)



c__case_186 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_186_case__104(x1)(x2)(st))(st)



c__case_185 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_185_case__103(x1)(x3)(st))(st)



c__case_189 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_189_case__102(x1)(x2)(x4)(x5)(x6)(st))(st)



c__case_188 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_188_case__101(x1)(x4)(x5)(x6)(st))(st)



c__case_187 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_187_case__100(x1)(x4)(x5)(x6)(st))(st)



c__case_196 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_196_case__99(x1)(x2)(st))(st)



c__case_190 x14 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_190_case__98(x1)(x14)(x13)(st))(st)



c__case_194 x8 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_194_case__97(x1)(x8)(x7)(st))(st)



c__case_193 x9 x10 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_193_case__96(x1)(x9)(x10)(x8)(st))(st)



c__case_192 x9 x10 x11 x12 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_192_case__95(x1)(x9)(x10)(x11)(x12)(x13)(st))(st)



c__case_191 x10 x11 x12 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_191_case__94(x1)(x10)(x11)(x12)(x13)(st))(st)



c__case_195 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_195_case__93(x1)(x3)(st))(st)



c__case_199 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_199_case__92(x1)(x2)(x3)(st))(st)



c__case_198 x2 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_198_case__91(x1)(x2)(x7)(st))(st)



c__case_197 x2 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_197_case__90(x1)(x2)(x7)(x8)(x9)(st))(st)



c__case_226 x2 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_226_case__89(x1)(x2)(x4)(x3)(st))(st)



c__case_225 x2 x3 x4 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_225_case__88(x1)(x2)(x3)(x4)(x6)(x5)(st))(st)



c__case_224 x2 x3 x4 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_224_case__87(x1)(x2)(x3)(x4)(x7)(x6)(st))(st)



c__case_223 x2 x3 x7 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_223_case__86(x1)(x2)(x3)(x7)(x4)(st))(st)



c__case_222 x2 x3 x4 x7 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_222_case__85(x1)(x2)(x3)(x4)(x7)(x9)(x8)(st))(st)



c__case_221 x2 x3 x4 x7 x9 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_221_case__84(x1)(x2)(x3)(x4)(x7)(x9)(x11)(x10)(st))(st)



c__case_220 x2 x3 x4 x7 x9 x11 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_220_case__83(x1)(x2)(x3)(x4)(x7)(x9)(x11)(x12)(st))(st)



c__case_219 x2 x3 x4 x7 x11 x14 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_219_case__82(x1)(x2)(x3)(x4)(x7)(x11)(x14)(x9)(st))(st)



c__case_201 x2 x3 x4 x7 x14 x125 x126 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_201_case__81(x1)(x2)(x3)(x4)(x7)(x14)(x125)(x126)(x11)(st))(st)



c__case_200 x2 x3 x4 x7 x14 x125 x126 x127 x128 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_200_case__80(x1)(x2)(x3)(x4)(x14)(x125)(x126)(x128)(st))(st)



c__case_203 x2 x3 x4 x7 x14 x109 x110 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_203_case__79(x1)(x2)(x3)(x4)(x7)(x14)(x109)(x110)(x11)(st))(st)



c__case_202 x2 x3 x4 x7 x14 x109 x110 x111 x112 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_202_case__78(x1)(x2)(x3)(x4)(x14)(x109)(x110)(x112)(st))(st)



c__case_205 x2 x3 x4 x7 x14 x94 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_205_case__77(x1)(x2)(x3)(x4)(x7)(x14)(x94)(x11)(st))(st)



c__case_204 x2 x3 x4 x7 x14 x94 x95 x96 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_204_case__76(x1)(x2)(x3)(x4)(x14)(x94)(x96)(st))(st)



c__case_207 x2 x3 x4 x7 x14 x78 x79 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_207_case__75(x1)(x2)(x3)(x4)(x7)(x14)(x78)(x79)(x11)(st))(st)



c__case_206 x2 x3 x4 x7 x14 x78 x79 x80 x81 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_206_case__74(x1)(x2)(x3)(x4)(x14)(x78)(x79)(x81)(st))(st)



c__case_209 x2 x3 x4 x7 x14 x62 x63 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_209_case__73(x1)(x2)(x3)(x4)(x7)(x14)(x62)(x63)(x11)(st))(st)



c__case_208 x2 x3 x4 x7 x14 x62 x63 x64 x65 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_208_case__72(x1)(x2)(x3)(x4)(x14)(x62)(x63)(x65)(st))(st)



c__case_211 x2 x3 x4 x7 x14 x46 x47 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_211_case__71(x1)(x2)(x3)(x4)(x7)(x14)(x46)(x47)(x11)(st))(st)



c__case_210 x2 x3 x4 x7 x14 x46 x47 x48 x49 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_210_case__70(x1)(x2)(x3)(x4)(x14)(x46)(x47)(x49)(st))(st)



c__case_213 x2 x3 x4 x7 x14 x31 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_213_case__69(x1)(x2)(x3)(x4)(x7)(x14)(x31)(x11)(st))(st)



c__case_212 x2 x3 x4 x7 x14 x31 x32 x33 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_212_case__68(x1)(x2)(x3)(x4)(x14)(x31)(x33)(st))(st)



c__case_215 x2 x3 x4 x7 x14 x16 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_215_case__67(x1)(x2)(x3)(x4)(x7)(x14)(x16)(x11)(st))(st)



c__case_214 x2 x3 x4 x7 x14 x16 x17 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_214_case__66(x1)(x2)(x3)(x4)(x14)(x16)(x18)(st))(st)



c__case_218 x2 x3 x4 x7 x11 x14 x15 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_218_case__65(x1)(x2)(x3)(x4)(x7)(x11)(x14)(x15)(x16)(st))(st)



c__case_217 x2 x3 x4 x7 x11 x14 x15 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_217_case__64(x1)(x2)(x3)(x4)(x7)(x11)(x14)(x15)(x16)(st))(st)



c__case_216 x2 x3 x4 x7 x11 x14 x15 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_216_case__63(x1)(x2)(x3)(x4)(x14)(x15)(x16)(st))(st)



c__case_231 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_231_case__62(x1)(x3)(x2)(st))(st)



c__case_230 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_230_case__61(x1)(x4)(x5)(x3)(st))(st)



c__case_229 x4 x5 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_229_case__60(x1)(x4)(x6)(x7)(x8)(st))(st)



c__case_228 x4 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_228_case__59(x1)(x6)(x7)(x8)(st))(st)



c__case_227 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_227_case__58(x1)(x7)(x8)(st))(st)



c__case_234 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_234_case__57(x1)(x2)(x3)(st))(st)



c__case_232 x2 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_232_case__56(x1)(x2)(x8)(x9)(st))(st)



c__case_233 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_233_case__55(x1)(x4)(st))(st)



c__case_236 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_236_case__54(x1)(x2)(x3)(st))(st)



c__case_235 x2 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_235_case__53(x1)(x2)(x7)(x8)(st))(st)



c__case_237 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_237_case__52(x1)(x2)(x3)(st))(st)



c__case_241 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_241_case__51(x1)(x2)(x3)(st))(st)



c__case_240 x2 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_240_case__50(x1)(x2)(x5)(x4)(st))(st)



c__case_239 x2 x5 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_239_case__49(x1)(x2)(x5)(x6)(x7)(x8)(st))(st)



c__case_238 x2 x5 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_238_case__48(x1)(x2)(x5)(x6)(x7)(x8)(st))(st)



c__case_242 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_242_case__47(x1)(x2)(st))(st)



c__case_244 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_244_case__46(x1)(x2)(x3)(st))(st)



c__case_243 x2 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_243_case__45(x1)(x2)(x6)(x7)(st))(st)



c__case_245 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_245_case__44(x1)(x2)(x3)(st))(st)



c__case_253 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_253_case__43(x1)(x2)(x3)(x4)(st))(st)



c__case_252 x2 x3 x8 x9 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_252_case__42(x1)(x2)(x3)(x8)(x9)(x5)(st))(st)



c__case_251 x2 x3 x8 x11 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_251_case__41(x1)(x2)(x3)(x8)(x11)(x9)(st))(st)



c__case_246 x11 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_246_case__40(x1)(x11)(x12)(st))(st)



c__case_247 x11 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_247_case__39(x1)(x11)(x14)(st))(st)



c__case_248 x2 x11 x13 x16 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_248_case__38(x1)(x2)(x11)(x13)(x16)(x14)(st))(st)



c__case_249 x8 x15 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_249_case__37(x1)(x8)(x15)(x16)(st))(st)



c__case_250 x12 x15 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_250_case__36(x1)(x12)(x15)(x16)(st))(st)



c__case_254 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_254_case__35(x1)(x2)(x3)(st))(st)



c__case_255 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_255_case__34(x1)(x2)(st))(st)



c__case_257 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_257_case__33(x1)(x2)(st))(st)



c__case_256 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_256_case__32(x1)(x3)(x4)(x5)(st))(st)



c__case_262 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_262_case__31(x1)(x2)(x3)(st))(st)



c__case_260 x2 x10 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_260_case__30(x1)(x2)(x10)(x9)(st))(st)



c__case_259 x2 x10 x11 x12 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_259_case__29(x1)(x2)(x10)(x11)(x12)(x13)(st))(st)



c__case_258 x2 x10 x11 x12 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_258_case__28(x1)(x2)(x10)(x11)(x12)(x13)(st))(st)



c__case_261 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_261_case__27(x1)(x4)(st))(st)



c__case_264 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_264_case__26(x1)(x2)(st))(st)



c__case_263 x6 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_263_case__25(x1)(x6)(x3)(st))(st)



c__case_267 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_267_case__24(x1)(x2)(st))(st)



c__case_265 x11 x12 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_265_case__23(x1)(x11)(x12)(x9)(st))(st)



c__case_266 x5 x6 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_266_case__22(x1)(x5)(x6)(x3)(st))(st)



c__case_268 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_268_case__21(x1)(x3)(st))(st)



c__case_269 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_269_case__20(x1)(x2)(st))(st)



c__case_272 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_272_case__19(x1)(x2)(st))(st)



c__case_271 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_271_case__18(x1)(x4)(x5)(x3)(st))(st)



c__case_270 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_270_case__17(x1)(x7)(x8)(st))(st)



c__case_273 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_273_case__16(x1)(x3)(st))(st)



c__case_274 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_274_case__15(x1)(x2)(x3)(st))(st)



c__case_275 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_275_case__14(x1)(x3)(st))(st)



c__case_276 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_276_case__13(x1)(x2)(st))(st)



c__case_277 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_277_case__12(x1)(x2)(st))(st)



c__case_280 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_280_case__11(x1)(x2)(st))(st)



c__case_278 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_278_case__10(x1)(x11)(st))(st)



c__case_279 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_279_case__9(x1)(x3)(st))(st)



c__case_281 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_281_case__8(x1)(x2)(st))(st)



c__case_282 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_282_case__7(x1)(x2)(st))(st)



c__case_285 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_285_case__6(x1)(x2)(st))(st)



c__case_283 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_283_case__5(x1)(x9)(st))(st)



c__case_284 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_284_case__4(x1)(x3)(st))(st)



c__case_286 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_286_case__3(x1)(x2)(st))(st)



c__case_287 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_287_case__2(x1)(x2)(st))(st)



c__case_289 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_289_case__1(x1)(x2)(st))(st)



c__case_288 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_288_case__0(x1)(x8)(x9)(st))(st)



c__case_288_case__0 x1 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_288_case__0 x1 x8 x9@Curry.Module.Prelude.C_False st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x8)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x1)(st))(x10)(st))(st)
c__case_288_case__0 x1 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_288_case__0(x1)(x8)(x)(st))(i)(xs)(st)
c__case_288_case__0 x1 x8 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_288_case__0")(x)



c__case_289_case__1 x1 x2@(Curry.Module.AbstractCurry.C_CurryProg x3 x4 x5 x6 x7) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(let {x8 = Curry.Module.OracleAbstractCurryPrinter.c_showExports(x5)(x6)(x1)(st)} in let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List))))))))))))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.op_43_43(x3)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c__case_288(x8)(Curry.Module.OraclePrelude.op_61_61(x8)(Curry.Module.Prelude.List)(x9)(st))(x10)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showImports(x4)(x11)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showOpDecls(x7)(x12)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showTypeDecls(x5)(x13)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_prefixInter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showFuncDeclOpt(Curry.Module.Prelude.T2(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_nameFM(x14)(st))(x6)(x15)(st))(x3))))))(x6)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))(x16)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x17)(st))(x18)(st))(x19)(st))(x20)(st))(x21)(st))(x22)(st))(x23)(st))(x24)(st))(st))(st)
c__case_289_case__1 x1 (Curry.Module.AbstractCurry.C_CurryProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_289_case__1(x1)(x)(st))(i)(xs)(st)
c__case_289_case__1 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_289_case__1")(x)



c__case_287_case__2 x1 x2@(Curry.Module.AbstractCurry.C_CType x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.AbstractCurry.C_Public)(x1)(st))(st)
c__case_287_case__2 x1 x2@(Curry.Module.AbstractCurry.C_CTypeSyn x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_61_61(x8)(Curry.Module.AbstractCurry.C_Public)(x1)(st))(st)
c__case_287_case__2 x1 (Curry.Module.AbstractCurry.C_CTypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_287_case__2(x1)(x)(st))(i)(xs)(st)
c__case_287_case__2 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_287_case__2")(x)



c__case_286_case__3 x1 x2@(Curry.Module.AbstractCurry.C_CFunc x3 x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_61_61(x5)(Curry.Module.AbstractCurry.C_Public)(x1)(st))(st)
c__case_286_case__3 x1 x2@(Curry.Module.AbstractCurry.C_CmtFunc x8 x9 x10 x11 x12 x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_61_61(x11)(Curry.Module.AbstractCurry.C_Public)(x1)(st))(st)
c__case_286_case__3 x1 (Curry.Module.AbstractCurry.C_CFuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_286_case__3(x1)(x)(st))(i)(xs)(st)
c__case_286_case__3 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_286_case__3")(x)



c__case_284_case__4 x1 x3@(Curry.Module.Prelude.T2 x7 x8) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_284_case__4 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_284_case__4(x1)(x)(st))(i)(xs)(st)
c__case_284_case__4 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_284_case__4")(x)



c__case_283_case__5 x1 x9@(Curry.Module.Prelude.T2 x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x14)(st)
c__case_283_case__5 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_283_case__5(x1)(x)(st))(i)(xs)(st)
c__case_283_case__5 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_283_case__5")(x)



c__case_285_case__6 x1 x2@(Curry.Module.AbstractCurry.C_CType x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_284(x3)(x1)(st))(st)
c__case_285_case__6 x1 x2@(Curry.Module.AbstractCurry.C_CTypeSyn x9 x10 x11 x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_283(x9)(x1)(st))(st)
c__case_285_case__6 x1 (Curry.Module.AbstractCurry.C_CTypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_285_case__6(x1)(x)(st))(i)(xs)(st)
c__case_285_case__6 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_285_case__6")(x)



c__case_282_case__7 x1 x2@(Curry.Module.AbstractCurry.C_CType x3 x4 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_length(Curry.Module.OraclePrelude.c_filter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showExports'46allPublicCons'469'46isPublicCons'4649))))(x6)(x1)(st))(x7)(st))(Curry.Module.OraclePrelude.c_length(x6)(x8)(st))(x9)(st))(st)
c__case_282_case__7 x1 (Curry.Module.AbstractCurry.C_CTypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_282_case__7(x1)(x)(st))(i)(xs)(st)
c__case_282_case__7 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_282_case__7")(x)



c__case_281_case__8 x1 x2@(Curry.Module.AbstractCurry.C_CCons x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_61_61(x5)(Curry.Module.AbstractCurry.C_Public)(x1)(st))(st)
c__case_281_case__8 x1 (Curry.Module.AbstractCurry.C_CConsDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_281_case__8(x1)(x)(st))(i)(xs)(st)
c__case_281_case__8 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_281_case__8")(x)



c__case_279_case__9 x1 x3@(Curry.Module.Prelude.T2 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x9)(st)
c__case_279_case__9 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_279_case__9(x1)(x)(st))(i)(xs)(st)
c__case_279_case__9 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_279_case__9")(x)



c__case_278_case__10 x1 x11@(Curry.Module.Prelude.T2 x16 x17) st = Curry.Module.CEventOracle.c_collapse(x1)(x17)(st)
c__case_278_case__10 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_278_case__10(x1)(x)(st))(i)(xs)(st)
c__case_278_case__10 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_278_case__10")(x)



c__case_280_case__11 x1 x2@(Curry.Module.AbstractCurry.C_CFunc x3 x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_279(x3)(x1)(st))(st)
c__case_280_case__11 x1 x2@(Curry.Module.AbstractCurry.C_CmtFunc x10 x11 x12 x13 x14 x15) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_278(x11)(x1)(st))(st)
c__case_280_case__11 x1 (Curry.Module.AbstractCurry.C_CFuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_280_case__11(x1)(x)(st))(i)(xs)(st)
c__case_280_case__11 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_280_case__11")(x)



c__case_277_case__12 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_277_case__12 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_277_case__12(x1)(x)(st))(i)(xs)(st)
c__case_277_case__12 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_277_case__12")(x)



c__case_276_case__13 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_276_case__13 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_276_case__13(x1)(x)(st))(i)(xs)(st)
c__case_276_case__13 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_276_case__13")(x)



c__case_275_case__14 x1 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_275_case__14 x1 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))(st)
c__case_275_case__14 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_275_case__14(x1)(x)(st))(i)(xs)(st)
c__case_275_case__14 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_275_case__14")(x)



c__case_274_case__15 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))(x2)(x1)(st))(st)
c__case_274_case__15 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_274_case__15 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_274_case__15(x1)(x2)(x)(st))(i)(xs)(st)
c__case_274_case__15 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_274_case__15")(x)



c__case_273_case__16 x1 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_273_case__16 x1 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))(st)
c__case_273_case__16 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_273_case__16(x1)(x)(st))(i)(xs)(st)
c__case_273_case__16 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_273_case__16")(x)



c__case_270_case__17 x1 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x7)(st)
c__case_270_case__17 x1 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('`'))(Curry.Module.OraclePrelude.op_43_43(x7)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('`'))(Curry.Module.Prelude.List))(x1)(st)))(st)
c__case_270_case__17 x1 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_270_case__17(x1)(x7)(x)(st))(i)(xs)(st)
c__case_270_case__17 x1 x7 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_270_case__17")(x)



c__case_271_case__18 x1 x4 x5 x3@(Curry.Module.Prelude.T2 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List)))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showFixity(x4)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x5)(x8)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_270(x7)(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_isInfixOpName(x9)(st))(x7)(x10)(st))(x11)(st))(x12)(st))(x13)(st))(x14)(st))(x15)(st))(st)
c__case_271_case__18 x1 x4 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_271_case__18(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_271_case__18 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_271_case__18")(x)



c__case_272_case__19 x1 x2@(Curry.Module.AbstractCurry.C_COp x3 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_271(x4)(x5)(x3)(x1)(st))(st)
c__case_272_case__19 x1 (Curry.Module.AbstractCurry.C_COpDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_272_case__19(x1)(x)(st))(i)(xs)(st)
c__case_272_case__19 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_272_case__19")(x)



c__case_269_case__20 x1 x2@Curry.Module.AbstractCurry.C_CInfixOp st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))(Curry.Module.Prelude.List))))))(st)
c__case_269_case__20 x1 x2@Curry.Module.AbstractCurry.C_CInfixlOp st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))))(st)
c__case_269_case__20 x1 x2@Curry.Module.AbstractCurry.C_CInfixrOp st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List)))))))(st)
c__case_269_case__20 x1 (Curry.Module.AbstractCurry.C_CFixityOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_269_case__20(x1)(x)(st))(i)(xs)(st)
c__case_269_case__20 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_269_case__20")(x)



c__case_268_case__21 x1 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_268_case__21 x1 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))(st)
c__case_268_case__21 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_268_case__21(x1)(x)(st))(i)(xs)(st)
c__case_268_case__21 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_268_case__21")(x)



c__case_266_case__22 x1 x5 x6 x3@(Curry.Module.Prelude.T2 x7 x8) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43(x8)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_False)))))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CTVar))))(x5)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(x9)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))(Curry.Module.OracleAbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_False)(x6)(x10)(st))(x11)(st))(x12)(st))(x13)(st))(x14)(st))(st)
c__case_266_case__22 x1 x5 x6 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_266_case__22(x1)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_266_case__22 x1 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_266_case__22")(x)



c__case_265_case__23 x1 x11 x12 x9@(Curry.Module.Prelude.T2 x13 x14) st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List)))))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43(x14)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_False)))))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CTVar))))(x11)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(x15)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showBlock(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.OracleAbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showConsDecl))))(x12)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('|'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))(x16)(st))(x17)(st))(x18)(st))(x19)(st))(x20)(st))(x21)(st))(x22)(st))(st)
c__case_265_case__23 x1 x11 x12 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_265_case__23(x1)(x11)(x12)(x)(st))(i)(xs)(st)
c__case_265_case__23 x1 x11 x12 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_265_case__23")(x)



c__case_267_case__24 x1 x2@(Curry.Module.AbstractCurry.C_CTypeSyn x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_266(x5)(x6)(x3)(x1)(st))(st)
c__case_267_case__24 x1 x2@(Curry.Module.AbstractCurry.C_CType x9 x10 x11 x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_265(x11)(x12)(x9)(x1)(st))(st)
c__case_267_case__24 x1 (Curry.Module.AbstractCurry.C_CTypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_267_case__24(x1)(x)(st))(i)(xs)(st)
c__case_267_case__24 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_267_case__24")(x)



c__case_263_case__25 x1 x6 x3@(Curry.Module.Prelude.T2 x7 x8) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x8)(Curry.Module.OracleAbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_True)))))(x6)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(x1)(st))(x9)(st))(st)
c__case_263_case__25 x1 x6 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_263_case__25(x1)(x6)(x)(st))(i)(xs)(st)
c__case_263_case__25 x1 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_263_case__25")(x)



c__case_264_case__26 x1 x2@(Curry.Module.AbstractCurry.C_CCons x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_263(x6)(x3)(x1)(st))(st)
c__case_264_case__26 x1 (Curry.Module.AbstractCurry.C_CConsDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_264_case__26(x1)(x)(st))(i)(xs)(st)
c__case_264_case__26 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_264_case__26")(x)



c__case_261_case__27 x1 x4@(Curry.Module.Prelude.T2 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.OracleAbstractCurryPrinter.c_showTypeVar(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_showIdentifier(x1)(st))(x6)(x7)(st))(x8)(st))(st)
c__case_261_case__27 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_261_case__27(x1)(x)(st))(i)(xs)(st)
c__case_261_case__27 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_261_case__27")(x)



c__case_258_case__28 x1 x2 x10 x11 x12 x13@Curry.Module.Prelude.C_True st = let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List)))))(Curry.Module.OracleAbstractCurryPrinter.c_maybeShowBrackets(Curry.Module.OraclePrelude.op_38_38(x2)(Curry.Module.OraclePrelude.c_not(Curry.Module.OraclePrelude.c_null(x10)(x1)(st))(x14)(st))(x15)(st))(Curry.Module.OracleAbstractCurryPrinter.c_showTypeCons(x11)(x12)(x10)(x16)(st))(x17)(st))(st)
c__case_258_case__28 x1 x2 x10 x11 x12 x13@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_258_case__28 x1 x2 x10 x11 x12 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_258_case__28(x1)(x2)(x10)(x11)(x12)(x)(st))(i)(xs)(st)
c__case_258_case__28 x1 x2 x10 x11 x12 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_258_case__28")(x)



c__case_259_case__29 x1 x2 x10 x11 x12 x13@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))(Curry.Module.Prelude.List))(st)
c__case_259_case__29 x1 x2 x10 x11 x12 x13@Curry.Module.Prelude.C_False st = let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_258(x2)(x10)(x11)(x12)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x14)(st))(st)
c__case_259_case__29 x1 x2 x10 x11 x12 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_259_case__29(x1)(x2)(x10)(x11)(x12)(x)(st))(i)(xs)(st)
c__case_259_case__29 x1 x2 x10 x11 x12 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_259_case__29")(x)



c__case_260_case__30 x1 x2 x10 x9@(Curry.Module.Prelude.T2 x11 x12) st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))))(Curry.Module.OracleAbstractCurryPrinter.c__case_259(x2)(x10)(x11)(x12)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x11)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x12)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))(Curry.Module.Prelude.List))))))))(x13)(st))(x14)(st))(x15)(st))(st)
c__case_260_case__30 x1 x2 x10 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_260_case__30(x1)(x2)(x10)(x)(st))(i)(xs)(st)
c__case_260_case__30 x1 x2 x10 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_260_case__30")(x)



c__case_262_case__31 x1 x2 x3@(Curry.Module.AbstractCurry.C_CTVar x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_261(x4)(x1)(st))(st)
c__case_262_case__31 x1 x2 x3@(Curry.Module.AbstractCurry.C_CFuncType x7 x8) st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))))))(Curry.Module.OracleAbstractCurryPrinter.c_maybeShowBrackets(x2)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showTypeExpr(Curry.Module.OracleAbstractCurryPrinter.c_isCFuncType(x7)(x1)(st))(x7)(x11)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.OracleAbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_False)(x8)(x12)(st))(x13)(st))(x14)(st))(x15)(st))(st)
c__case_262_case__31 x1 x2 x3@(Curry.Module.AbstractCurry.C_CTCons x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_260(x2)(x10)(x9)(x1)(st))(st)
c__case_262_case__31 x1 x2 (Curry.Module.AbstractCurry.C_CTypeExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_262_case__31(x1)(x2)(x)(st))(i)(xs)(st)
c__case_262_case__31 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_262_case__31")(x)



c__case_256_case__32 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))(x4)))(st)
c__case_256_case__32 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x3)(x4))(st)
c__case_256_case__32 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_256_case__32(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_256_case__32 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_256_case__32")(x)



c__case_257_case__33 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))))))))(Curry.Module.OracleAbstractCurryPrinter.c__case_256(x3)(x4)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('a'))(x1)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.c_not(Curry.Module.OraclePrelude.c_null(x4)(x5)(st))(x6)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_all(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleChar.c_isDigit))))(x7)(st))(x4)(x8)(st))(x9)(st))(x10)(st))(x11)(st))(st)
c__case_257_case__33 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_257_case__33(x1)(x)(st))(i)(xs)(st)
c__case_257_case__33 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_257_case__33")(x)



c__case_255_case__34 x1 x2@(Curry.Module.AbstractCurry.C_CFuncType x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_255_case__34 x1 x2@(Curry.Module.AbstractCurry.C_CTVar x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_255_case__34 x1 x2@(Curry.Module.AbstractCurry.C_CTCons x6 x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_255_case__34 x1 (Curry.Module.AbstractCurry.C_CTypeExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_255_case__34(x1)(x)(st))(i)(xs)(st)
c__case_255_case__34 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_255_case__34")(x)



c__case_254_case__35 x1 x2 x3@(Curry.Module.AbstractCurry.C_CmtFunc x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showCmtFunc(x2)(x4)(Curry.Module.AbstractCurry.C_CFunc(x5)(x6)(x7)(x8)(x9))(x1)(st))(st)
c__case_254_case__35 x1 x2 x3@(Curry.Module.AbstractCurry.C_CFunc x10 x11 x12 x13 x14) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showCmtFunc(x2)(Curry.Module.Prelude.List)(x3)(x1)(st))(st)
c__case_254_case__35 x1 x2 (Curry.Module.AbstractCurry.C_CFuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_254_case__35(x1)(x2)(x)(st))(i)(xs)(st)
c__case_254_case__35 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_254_case__35")(x)



c__case_250_case__36 x1 x12 x15 x16@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_250_case__36 x1 x12 x15 x16@Curry.Module.Prelude.C_False st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(x15)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showEvalAnnot(x12)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x17)(st))(x18)(st))(x19)(st))(st)
c__case_250_case__36 x1 x12 x15 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_250_case__36(x1)(x12)(x15)(x)(st))(i)(xs)(st)
c__case_250_case__36 x1 x12 x15 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_250_case__36")(x)



c__case_249_case__37 x1 x8 x15 x16@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(st)
c__case_249_case__37 x1 x8 x15 x16@Curry.Module.Prelude.C_False st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(x15)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_False)(x8)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x17)(st))(x18)(st))(x19)(st))(st)
c__case_249_case__37 x1 x8 x15 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_249_case__37(x1)(x8)(x15)(x)(st))(i)(xs)(st)
c__case_249_case__37 x1 x8 x15 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_249_case__37")(x)



c__case_248_case__38 x1 x2 x11 x13 x16 x14@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x16)(st)
c__case_248_case__38 x1 x2 x11 x13 x16 x14@Curry.Module.Prelude.C_False st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(x11)(Curry.Module.OracleAbstractCurryPrinter.c_prefixInter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showRule(x2)))))(x13)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x11)(x1)(st))(x17)(st))(x18)(st))(st)
c__case_248_case__38 x1 x2 x11 x13 x16 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_248_case__38(x1)(x2)(x11)(x13)(x16)(x)(st))(i)(xs)(st)
c__case_248_case__38 x1 x2 x11 x13 x16 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_248_case__38")(x)



c__case_247_case__39 x1 x11 x14@Curry.Module.Prelude.C_True st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x11)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x1)(st))(x15)(st))(st)
c__case_247_case__39 x1 x11 x14@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(x11)(st)
c__case_247_case__39 x1 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_247_case__39(x1)(x11)(x)(st))(i)(xs)(st)
c__case_247_case__39 x1 x11 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_247_case__39")(x)



c__case_246_case__40 x1 x11 x12@Curry.Module.Prelude.C_True st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x11)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x1)(st))(x13)(st))(st)
c__case_246_case__40 x1 x11 x12@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(x11)(st)
c__case_246_case__40 x1 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_246_case__40(x1)(x11)(x)(st))(i)(xs)(st)
c__case_246_case__40 x1 x11 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_246_case__40")(x)



c__case_251_case__41 x1 x2 x3 x8 x11 x9@(Curry.Module.AbstractCurry.C_CRules x12 x13) st = let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List)))(let {x14 = Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_isInfixOpName(x1)(st))(x11)(x19)(st)} in let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))(let {x15 = Curry.Module.OracleAbstractCurryPrinter.c__case_247(x11)(x14)(x20)(st)} in let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List)))))))(let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x27)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)((Curry.Module.Prelude.:<)(x30)((Curry.Module.Prelude.:<)(x31)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)((Curry.Module.Prelude.:<)(x34)((Curry.Module.Prelude.:<)(x35)((Curry.Module.Prelude.:<)(x36)(Curry.Module.Prelude.List))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_funcComment(x27)(st))(x3)(x28)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c__case_250(x12)(x15)(Curry.Module.OraclePrelude.op_61_61(x12)(Curry.Module.AbstractCurry.C_CFlex)(x29)(st))(x30)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c__case_249(x8)(x15)(Curry.Module.OracleAbstractCurryPrinter.c_isUntyped(x8)(x31)(st))(x32)(st))(Curry.Module.OracleAbstractCurryPrinter.c__case_248(x2)(x11)(x13)(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_concat))))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleList.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.c_map(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showCmtFunc'46insertName'46139(x11)))))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_span(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_47_61))(st))(Curry.Module.Prelude.C_Char(' '))))))))))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_tail))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showRule(x2)))))(x21)(st))(x22)(st))(x23)(st))(x13)(x24)(st))(x25)(st))(x26)(st))(x14)(x33)(st))(x34)(st))(x35)(st))(x36)(st))(st))(st))(st))(st)
c__case_251_case__41 x1 x2 x3 x8 x11 x9@(Curry.Module.AbstractCurry.C_CExternal x17) st = let {x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x39 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x37)((Curry.Module.Prelude.:<)(x38)((Curry.Module.Prelude.:<)(x39)(Curry.Module.Prelude.List))))(let {x18 = Curry.Module.OracleAbstractCurryPrinter.c__case_246(x11)(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_isInfixOpName(x1)(st))(x11)(x37)(st))(x38)(st)} in let {x40 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x41 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x42 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x43 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x44 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x45 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x46 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x47 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x39)((Curry.Module.Prelude.:<)(x40)((Curry.Module.Prelude.:<)(x41)((Curry.Module.Prelude.:<)(x42)((Curry.Module.Prelude.:<)(x43)((Curry.Module.Prelude.:<)(x44)((Curry.Module.Prelude.:<)(x45)((Curry.Module.Prelude.:<)(x46)((Curry.Module.Prelude.:<)(x47)(Curry.Module.Prelude.List)))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_funcComment(x39)(st))(x3)(x40)(st))(Curry.Module.OraclePrelude.op_43_43(x18)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_False)(x8)(x41)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x18)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List))))))))))(x42)(st))(x43)(st))(x44)(st))(x45)(st))(x46)(st))(x47)(st))(st))(st)
c__case_251_case__41 x1 x2 x3 x8 x11 (Curry.Module.AbstractCurry.C_CRulesOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_251_case__41(x1)(x2)(x3)(x8)(x11)(x)(st))(i)(xs)(st)
c__case_251_case__41 x1 x2 x3 x8 x11 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_251_case__41")(x)



c__case_252_case__42 x1 x2 x3 x8 x9 x5@(Curry.Module.Prelude.T2 x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_251(x2)(x3)(x8)(x11)(x9)(x1)(st))(st)
c__case_252_case__42 x1 x2 x3 x8 x9 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_252_case__42(x1)(x2)(x3)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_252_case__42 x1 x2 x3 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_252_case__42")(x)



c__case_253_case__43 x1 x2 x3 x4@(Curry.Module.AbstractCurry.C_CFunc x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_252(x2)(x3)(x8)(x9)(x5)(x1)(st))(st)
c__case_253_case__43 x1 x2 x3 (Curry.Module.AbstractCurry.C_CFuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_253_case__43(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_253_case__43 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_253_case__43")(x)



c__case_245_case__44 x1 x2 x3@(Curry.Module.Prelude.T2 x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(x4)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x2)(x5)(x1)(st))(x6)(st))(x7)(st))(st)
c__case_245_case__44 x1 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_245_case__44(x1)(x2)(x)(st))(i)(xs)(st)
c__case_245_case__44 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_245_case__44")(x)



c__case_243_case__45 x1 x2 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_243_case__45 x1 x2 x6 x7@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))))))))(Curry.Module.OracleAbstractCurryPrinter.c_showBlock(Curry.Module.OracleAbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showLocalDecl(x2)))))(x6)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x1)(st))(x8)(st))(x9)(st))(st)
c__case_243_case__45 x1 x2 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_243_case__45(x1)(x2)(x6)(x)(st))(i)(xs)(st)
c__case_243_case__45 x1 x2 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_243_case__45")(x)



c__case_244_case__46 x1 x2 x3@(Curry.Module.AbstractCurry.C_CRule x4 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showPattern))))(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(x1)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showCrhsList(x2)(x5)(x7)(st))(Curry.Module.OracleAbstractCurryPrinter.c__case_243(x2)(x6)(Curry.Module.OraclePrelude.c_null(x6)(x8)(st))(x9)(st))(x10)(st))(x11)(st))(st)
c__case_244_case__46 x1 x2 (Curry.Module.AbstractCurry.C_CRuleOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_244_case__46(x1)(x2)(x)(st))(i)(xs)(st)
c__case_244_case__46 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_244_case__46")(x)



c__case_242_case__47 x1 x2@Curry.Module.AbstractCurry.C_CFlex st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))(Curry.Module.Prelude.List)))))(st)
c__case_242_case__47 x1 x2@Curry.Module.AbstractCurry.C_CRigid st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))(Curry.Module.Prelude.List))))))(st)
c__case_242_case__47 x1 x2@Curry.Module.AbstractCurry.C_CChoice st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))(st)
c__case_242_case__47 x1 (Curry.Module.AbstractCurry.C_CEvalAnnotOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_242_case__47(x1)(x)(st))(i)(xs)(st)
c__case_242_case__47 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_242_case__47")(x)



c__case_238_case__48 x1 x2 x5 x6 x7 x8@Curry.Module.Prelude.C_True st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showBlock(Curry.Module.OracleAbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showCrhs(x2)))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(x6)(x7))(x5))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x1)(st))(x9)(st))(x10)(st))(st)
c__case_238_case__48 x1 x2 x5 x6 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_238_case__48 x1 x2 x5 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_238_case__48(x1)(x2)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_238_case__48 x1 x2 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_238_case__48")(x)



c__case_239_case__49 x1 x2 x5 x6 x7 x8@Curry.Module.Prelude.C_True st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))(Curry.Module.OracleAbstractCurryPrinter.c_showExprOpt(x2)(x7)(x1)(st))(x9)(st))(st)
c__case_239_case__49 x1 x2 x5 x6 x7 x8@Curry.Module.Prelude.C_False st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_238(x2)(x5)(x6)(x7)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x10)(st))(st)
c__case_239_case__49 x1 x2 x5 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_239_case__49(x1)(x2)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_239_case__49 x1 x2 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_239_case__49")(x)



c__case_240_case__50 x1 x2 x5 x4@(Curry.Module.Prelude.T2 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))))(Curry.Module.OracleAbstractCurryPrinter.c__case_239(x2)(x5)(x6)(x7)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x5)(Curry.Module.Prelude.List)(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x6)(Curry.Module.AbstractCurry.C_CSymbol(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))))))(x8)(st))(x9)(st))(x10)(st))(st)
c__case_240_case__50 x1 x2 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_240_case__50(x1)(x2)(x5)(x)(st))(i)(xs)(st)
c__case_240_case__50 x1 x2 x5 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_240_case__50")(x)



c__case_241_case__51 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_241_case__51 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_240(x2)(x5)(x4)(x1)(st))(st)
c__case_241_case__51 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_241_case__51(x1)(x2)(x)(st))(i)(xs)(st)
c__case_241_case__51 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_241_case__51")(x)



c__case_237_case__52 x1 x2 x3@(Curry.Module.Prelude.T2 x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('|'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showExprOpt(x2)(x4)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))(Curry.Module.OracleAbstractCurryPrinter.c_showExprOpt(x2)(x5)(x6)(st))(x7)(st))(x8)(st))(x9)(st))(st)
c__case_237_case__52 x1 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_237_case__52(x1)(x2)(x)(st))(i)(xs)(st)
c__case_237_case__52 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_237_case__52")(x)



c__case_235_case__53 x1 x2 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_235_case__53 x1 x2 x7 x8@Curry.Module.Prelude.C_False st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))))))))(Curry.Module.OracleAbstractCurryPrinter.c_showBlock(Curry.Module.OracleAbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showLocalDecl(x2)))))(x7)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x1)(st))(x9)(st))(x10)(st))(st)
c__case_235_case__53 x1 x2 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_235_case__53(x1)(x2)(x7)(x)(st))(i)(xs)(st)
c__case_235_case__53 x1 x2 x7 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_235_case__53")(x)



c__case_236_case__54 x1 x2 x3@(Curry.Module.AbstractCurry.C_CLocalFunc x4) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_showLocalFuncDecl(x2)(x1)(st))(x4)(x9)(st))(st)
c__case_236_case__54 x1 x2 x3@(Curry.Module.AbstractCurry.C_CLocalPat x5 x6 x7) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showPattern(x5)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showExprOpt(x2)(x6)(x10)(st))(Curry.Module.OracleAbstractCurryPrinter.c__case_235(x2)(x7)(Curry.Module.OraclePrelude.c_null(x7)(x11)(st))(x12)(st))(x13)(st))(x14)(st))(x15)(st))(st)
c__case_236_case__54 x1 x2 x3@(Curry.Module.AbstractCurry.C_CLocalVar x8) st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showPattern(Curry.Module.AbstractCurry.C_CPVar(x8))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))(x16)(st))(st)
c__case_236_case__54 x1 x2 (Curry.Module.AbstractCurry.C_CLocalDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_236_case__54(x1)(x2)(x)(st))(i)(xs)(st)
c__case_236_case__54 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_236_case__54")(x)



c__case_233_case__55 x1 x4@(Curry.Module.Prelude.T2 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_showIdentifier(x1)(st))(x6)(x7)(st))(st)
c__case_233_case__55 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_233_case__55(x1)(x)(st))(i)(xs)(st)
c__case_233_case__55 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_233_case__55")(x)



c__case_232_case__56 x1 x2 x8 x9@Curry.Module.Prelude.C_True st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showSymbol(x2)(x8)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x10)(st))(x11)(st))(st)
c__case_232_case__56 x1 x2 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showSymbol(x2)(x8)(x1)(st))(st)
c__case_232_case__56 x1 x2 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_232_case__56(x1)(x2)(x8)(x)(st))(i)(xs)(st)
c__case_232_case__56 x1 x2 x8 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_232_case__56")(x)



c__case_234_case__57 x1 x2 x3@(Curry.Module.AbstractCurry.C_CVar x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_233(x4)(x1)(st))(st)
c__case_234_case__57 x1 x2 x3@(Curry.Module.AbstractCurry.C_CLit x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLiteral(x7)(x1)(st))(st)
c__case_234_case__57 x1 x2 x3@(Curry.Module.AbstractCurry.C_CSymbol x8) st = let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))))(Curry.Module.OracleAbstractCurryPrinter.c__case_232(x2)(x8)(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_isInfixOpName(x1)(st))(Curry.Module.OraclePrelude.c_snd(x8)(x20)(st))(x21)(st))(x22)(st))(st)
c__case_234_case__57 x1 x2 x3@(Curry.Module.AbstractCurry.C_CApply x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showApplication(x2)(Curry.Module.AbstractCurry.C_CApply(x9)(x10))(x1)(st))(st)
c__case_234_case__57 x1 x2 x3@(Curry.Module.AbstractCurry.C_CLambda x11 x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambdaOrSection(x2)(x11)(x12)(x1)(st))(st)
c__case_234_case__57 x1 x2 x3@(Curry.Module.AbstractCurry.C_CLetDecl x13 x14) st = let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))(Curry.Module.OracleAbstractCurryPrinter.c_showBlock(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showLocalDecl(x2)))))(x13)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x14)(x23)(st))(x24)(st))(x25)(st))(x26)(st))(x27)(st))(st)
c__case_234_case__57 x1 x2 x3@(Curry.Module.AbstractCurry.C_CDoExpr x15) st = let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))))))(Curry.Module.OracleAbstractCurryPrinter.c_showBlock(Curry.Module.OracleAbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showStatement(x2)))))(x15)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x1)(st))(x28)(st))(x29)(st))(st)
c__case_234_case__57 x1 x2 x3@(Curry.Module.AbstractCurry.C_CListComp x16 x17) st = let {x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x30)((Curry.Module.Prelude.:<)(x31)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)((Curry.Module.Prelude.:<)(x34)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x16)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('|'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showStatement(x2)))))(x17)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(x30)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))(x31)(st))(x32)(st))(x33)(st))(x34)(st))(st)
c__case_234_case__57 x1 x2 x3@(Curry.Module.AbstractCurry.C_CCase x18 x19) st = let {x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x39 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x35)((Curry.Module.Prelude.:<)(x36)((Curry.Module.Prelude.:<)(x37)((Curry.Module.Prelude.:<)(x38)((Curry.Module.Prelude.:<)(x39)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x18)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))(Curry.Module.OracleAbstractCurryPrinter.c_showBlock(Curry.Module.OracleAbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showBranchExpr(x2)))))(x19)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x35)(st))(x36)(st))(x37)(st))(x38)(st))(x39)(st))(st)
c__case_234_case__57 x1 x2 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_234_case__57(x1)(x2)(x)(st))(i)(xs)(st)
c__case_234_case__57 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_234_case__57")(x)



c__case_227_case__58 x1 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x7)(st)
c__case_227_case__58 x1 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_227_case__58 x1 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_227_case__58(x1)(x7)(x)(st))(i)(xs)(st)
c__case_227_case__58 x1 x7 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_227_case__58")(x)



c__case_228_case__59 x1 x6 x7 x8@Curry.Module.Prelude.C_True st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x6)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List))(x7)(x1)(st))(x9)(st))(st)
c__case_228_case__59 x1 x6 x7 x8@Curry.Module.Prelude.C_False st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_227(x7)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x10)(st))(st)
c__case_228_case__59 x1 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_228_case__59(x1)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_228_case__59 x1 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_228_case__59")(x)



c__case_229_case__60 x1 x4 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x7)(st)
c__case_229_case__60 x1 x4 x6 x7 x8@Curry.Module.Prelude.C_False st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))(Curry.Module.OracleAbstractCurryPrinter.c__case_228(x4)(x6)(x7)(Curry.Module.OracleMaybe.c_isJust(Curry.Module.OracleFiniteMap.c_lookupFM(x4)(x7)(x1)(st))(x9)(st))(x10)(st))(st)
c__case_229_case__60 x1 x4 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_229_case__60(x1)(x4)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_229_case__60 x1 x4 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_229_case__60")(x)



c__case_230_case__61 x1 x4 x5 x3@(Curry.Module.Prelude.T2 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_229(x4)(x5)(x6)(x7)(Curry.Module.OraclePrelude.op_61_61(x5)(x6)(x1)(st))(x8)(st))(st)
c__case_230_case__61 x1 x4 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_230_case__61(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_230_case__61 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_230_case__61")(x)



c__case_231_case__62 x1 x3 x2@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_230(x4)(x5)(x3)(x1)(st))(st)
c__case_231_case__62 x1 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_231_case__62(x1)(x3)(x)(st))(i)(xs)(st)
c__case_231_case__62 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_231_case__62")(x)



c__case_216_case__63 x1 x2 x3 x4 x14 x15 x16@Curry.Module.Prelude.C_True st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x14)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showExprOpt(x2)(Curry.Module.AbstractCurry.C_CVar(x15))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x17)(st))(x18)(st))(x19)(st))(x20)(st))(st)
c__case_216_case__63 x1 x2 x3 x4 x14 x15 x16@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_216_case__63 x1 x2 x3 x4 x14 x15 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_216_case__63(x1)(x2)(x3)(x4)(x14)(x15)(x)(st))(i)(xs)(st)
c__case_216_case__63 x1 x2 x3 x4 x14 x15 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_216_case__63")(x)



c__case_217_case__64 x1 x2 x3 x4 x7 x11 x14 x15 x16@Curry.Module.Prelude.C_True st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x11)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x14)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x17)(st))(x18)(st))(x19)(st))(x20)(st))(st)
c__case_217_case__64 x1 x2 x3 x4 x7 x11 x14 x15 x16@Curry.Module.Prelude.C_False st = let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_216(x2)(x3)(x4)(x7)(x11)(x14)(x15)(Curry.Module.OraclePrelude.op_61_61(x11)(Curry.Module.AbstractCurry.C_CVar(x7))(x1)(st))(x21)(st))(st)
c__case_217_case__64 x1 x2 x3 x4 x7 x11 x14 x15 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_217_case__64(x1)(x2)(x3)(x4)(x7)(x11)(x14)(x15)(x)(st))(i)(xs)(st)
c__case_217_case__64 x1 x2 x3 x4 x7 x11 x14 x15 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_217_case__64")(x)



c__case_218_case__65 x1 x2 x3 x4 x7 x11 x14 x15 x16@Curry.Module.Prelude.C_True st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_217(x2)(x3)(x4)(x7)(x11)(x14)(x15)(Curry.Module.OraclePrelude.op_61_61(x7)(x15)(x1)(st))(x17)(st))(st)
c__case_218_case__65 x1 x2 x3 x4 x7 x11 x14 x15 x16@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_218_case__65 x1 x2 x3 x4 x7 x11 x14 x15 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_218_case__65(x1)(x2)(x3)(x4)(x7)(x11)(x14)(x15)(x)(st))(i)(xs)(st)
c__case_218_case__65 x1 x2 x3 x4 x7 x11 x14 x15 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_218_case__65")(x)



c__case_214_case__66 x1 x2 x3 x4 x14 x16 x18@Curry.Module.Prelude.C_True st = let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x14)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(Curry.Module.AbstractCurry.C_CLit(x16))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x19)(st))(x20)(st))(x21)(st))(x22)(st))(st)
c__case_214_case__66 x1 x2 x3 x4 x14 x16 x18@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_214_case__66 x1 x2 x3 x4 x14 x16 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_214_case__66(x1)(x2)(x3)(x4)(x14)(x16)(x)(st))(i)(xs)(st)
c__case_214_case__66 x1 x2 x3 x4 x14 x16 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_214_case__66")(x)



c__case_215_case__67 x1 x2 x3 x4 x7 x14 x16 x11@(Curry.Module.AbstractCurry.C_CVar x17) st = let {x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x31)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)((Curry.Module.Prelude.:<)(x34)((Curry.Module.Prelude.:<)(x35)((Curry.Module.Prelude.:<)(x36)((Curry.Module.Prelude.:<)(x37)((Curry.Module.Prelude.:<)(x38)(Curry.Module.Prelude.List)))))))))(Curry.Module.OracleAbstractCurryPrinter.c__case_214(x2)(x3)(x4)(x7)(x14)(x16)(x17)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_isInfixOpName(x1)(st))(x14)(x31)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x7)(x17)(x32)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OracleAbstractCurryPrinter.c_isAtom(Curry.Module.AbstractCurry.C_CLit(x16))(x33)(st))(Curry.Module.OraclePrelude.op_47_61(Curry.Module.AbstractCurry.C_CVar(x17))(Curry.Module.AbstractCurry.C_CLit(x16))(x34)(st))(x35)(st))(x36)(st))(x37)(st))(x38)(st))(st)
c__case_215_case__67 x1 x2 x3 x4 x7 x14 x16 x11@(Curry.Module.AbstractCurry.C_CLit x18) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_215_case__67 x1 x2 x3 x4 x7 x14 x16 x11@(Curry.Module.AbstractCurry.C_CSymbol x19) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_215_case__67 x1 x2 x3 x4 x7 x14 x16 x11@(Curry.Module.AbstractCurry.C_CApply x20 x21) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_215_case__67 x1 x2 x3 x4 x7 x14 x16 x11@(Curry.Module.AbstractCurry.C_CLambda x22 x23) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_215_case__67 x1 x2 x3 x4 x7 x14 x16 x11@(Curry.Module.AbstractCurry.C_CLetDecl x24 x25) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_215_case__67 x1 x2 x3 x4 x7 x14 x16 x11@(Curry.Module.AbstractCurry.C_CDoExpr x26) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_215_case__67 x1 x2 x3 x4 x7 x14 x16 x11@(Curry.Module.AbstractCurry.C_CListComp x27 x28) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_215_case__67 x1 x2 x3 x4 x7 x14 x16 x11@(Curry.Module.AbstractCurry.C_CCase x29 x30) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_215_case__67 x1 x2 x3 x4 x7 x14 x16 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_215_case__67(x1)(x2)(x3)(x4)(x7)(x14)(x16)(x)(st))(i)(xs)(st)
c__case_215_case__67 x1 x2 x3 x4 x7 x14 x16 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_215_case__67")(x)



c__case_212_case__68 x1 x2 x3 x4 x14 x31 x33@Curry.Module.Prelude.C_True st = let {x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x34)((Curry.Module.Prelude.:<)(x35)((Curry.Module.Prelude.:<)(x36)((Curry.Module.Prelude.:<)(x37)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x14)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(Curry.Module.AbstractCurry.C_CSymbol(x31))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x34)(st))(x35)(st))(x36)(st))(x37)(st))(st)
c__case_212_case__68 x1 x2 x3 x4 x14 x31 x33@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_212_case__68 x1 x2 x3 x4 x14 x31 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_212_case__68(x1)(x2)(x3)(x4)(x14)(x31)(x)(st))(i)(xs)(st)
c__case_212_case__68 x1 x2 x3 x4 x14 x31 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_212_case__68")(x)



c__case_213_case__69 x1 x2 x3 x4 x7 x14 x31 x11@(Curry.Module.AbstractCurry.C_CVar x32) st = let {x46 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x47 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x48 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x49 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x50 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x51 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x52 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x53 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x46)((Curry.Module.Prelude.:<)(x47)((Curry.Module.Prelude.:<)(x48)((Curry.Module.Prelude.:<)(x49)((Curry.Module.Prelude.:<)(x50)((Curry.Module.Prelude.:<)(x51)((Curry.Module.Prelude.:<)(x52)((Curry.Module.Prelude.:<)(x53)(Curry.Module.Prelude.List)))))))))(Curry.Module.OracleAbstractCurryPrinter.c__case_212(x2)(x3)(x4)(x7)(x14)(x31)(x32)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_isInfixOpName(x1)(st))(x14)(x46)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x7)(x32)(x47)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OracleAbstractCurryPrinter.c_isAtom(Curry.Module.AbstractCurry.C_CSymbol(x31))(x48)(st))(Curry.Module.OraclePrelude.op_47_61(Curry.Module.AbstractCurry.C_CVar(x32))(Curry.Module.AbstractCurry.C_CSymbol(x31))(x49)(st))(x50)(st))(x51)(st))(x52)(st))(x53)(st))(st)
c__case_213_case__69 x1 x2 x3 x4 x7 x14 x31 x11@(Curry.Module.AbstractCurry.C_CLit x33) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_213_case__69 x1 x2 x3 x4 x7 x14 x31 x11@(Curry.Module.AbstractCurry.C_CSymbol x34) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_213_case__69 x1 x2 x3 x4 x7 x14 x31 x11@(Curry.Module.AbstractCurry.C_CApply x35 x36) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_213_case__69 x1 x2 x3 x4 x7 x14 x31 x11@(Curry.Module.AbstractCurry.C_CLambda x37 x38) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_213_case__69 x1 x2 x3 x4 x7 x14 x31 x11@(Curry.Module.AbstractCurry.C_CLetDecl x39 x40) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_213_case__69 x1 x2 x3 x4 x7 x14 x31 x11@(Curry.Module.AbstractCurry.C_CDoExpr x41) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_213_case__69 x1 x2 x3 x4 x7 x14 x31 x11@(Curry.Module.AbstractCurry.C_CListComp x42 x43) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_213_case__69 x1 x2 x3 x4 x7 x14 x31 x11@(Curry.Module.AbstractCurry.C_CCase x44 x45) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_213_case__69 x1 x2 x3 x4 x7 x14 x31 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_213_case__69(x1)(x2)(x3)(x4)(x7)(x14)(x31)(x)(st))(i)(xs)(st)
c__case_213_case__69 x1 x2 x3 x4 x7 x14 x31 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_213_case__69")(x)



c__case_210_case__70 x1 x2 x3 x4 x14 x46 x47 x49@Curry.Module.Prelude.C_True st = let {x50 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x51 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x52 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x53 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x50)((Curry.Module.Prelude.:<)(x51)((Curry.Module.Prelude.:<)(x52)((Curry.Module.Prelude.:<)(x53)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x14)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(Curry.Module.AbstractCurry.C_CApply(x46)(x47))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x50)(st))(x51)(st))(x52)(st))(x53)(st))(st)
c__case_210_case__70 x1 x2 x3 x4 x14 x46 x47 x49@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_210_case__70 x1 x2 x3 x4 x14 x46 x47 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_210_case__70(x1)(x2)(x3)(x4)(x14)(x46)(x47)(x)(st))(i)(xs)(st)
c__case_210_case__70 x1 x2 x3 x4 x14 x46 x47 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_210_case__70")(x)



c__case_211_case__71 x1 x2 x3 x4 x7 x14 x46 x47 x11@(Curry.Module.AbstractCurry.C_CVar x48) st = let {x62 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x63 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x64 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x65 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x66 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x67 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x68 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x69 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x62)((Curry.Module.Prelude.:<)(x63)((Curry.Module.Prelude.:<)(x64)((Curry.Module.Prelude.:<)(x65)((Curry.Module.Prelude.:<)(x66)((Curry.Module.Prelude.:<)(x67)((Curry.Module.Prelude.:<)(x68)((Curry.Module.Prelude.:<)(x69)(Curry.Module.Prelude.List)))))))))(Curry.Module.OracleAbstractCurryPrinter.c__case_210(x2)(x3)(x4)(x7)(x14)(x46)(x47)(x48)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_isInfixOpName(x1)(st))(x14)(x62)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x7)(x48)(x63)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OracleAbstractCurryPrinter.c_isAtom(Curry.Module.AbstractCurry.C_CApply(x46)(x47))(x64)(st))(Curry.Module.OraclePrelude.op_47_61(Curry.Module.AbstractCurry.C_CVar(x48))(Curry.Module.AbstractCurry.C_CApply(x46)(x47))(x65)(st))(x66)(st))(x67)(st))(x68)(st))(x69)(st))(st)
c__case_211_case__71 x1 x2 x3 x4 x7 x14 x46 x47 x11@(Curry.Module.AbstractCurry.C_CLit x49) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_211_case__71 x1 x2 x3 x4 x7 x14 x46 x47 x11@(Curry.Module.AbstractCurry.C_CSymbol x50) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_211_case__71 x1 x2 x3 x4 x7 x14 x46 x47 x11@(Curry.Module.AbstractCurry.C_CApply x51 x52) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_211_case__71 x1 x2 x3 x4 x7 x14 x46 x47 x11@(Curry.Module.AbstractCurry.C_CLambda x53 x54) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_211_case__71 x1 x2 x3 x4 x7 x14 x46 x47 x11@(Curry.Module.AbstractCurry.C_CLetDecl x55 x56) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_211_case__71 x1 x2 x3 x4 x7 x14 x46 x47 x11@(Curry.Module.AbstractCurry.C_CDoExpr x57) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_211_case__71 x1 x2 x3 x4 x7 x14 x46 x47 x11@(Curry.Module.AbstractCurry.C_CListComp x58 x59) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_211_case__71 x1 x2 x3 x4 x7 x14 x46 x47 x11@(Curry.Module.AbstractCurry.C_CCase x60 x61) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_211_case__71 x1 x2 x3 x4 x7 x14 x46 x47 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_211_case__71(x1)(x2)(x3)(x4)(x7)(x14)(x46)(x47)(x)(st))(i)(xs)(st)
c__case_211_case__71 x1 x2 x3 x4 x7 x14 x46 x47 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_211_case__71")(x)



c__case_208_case__72 x1 x2 x3 x4 x14 x62 x63 x65@Curry.Module.Prelude.C_True st = let {x66 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x67 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x68 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x69 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x66)((Curry.Module.Prelude.:<)(x67)((Curry.Module.Prelude.:<)(x68)((Curry.Module.Prelude.:<)(x69)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x14)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(Curry.Module.AbstractCurry.C_CLambda(x62)(x63))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x66)(st))(x67)(st))(x68)(st))(x69)(st))(st)
c__case_208_case__72 x1 x2 x3 x4 x14 x62 x63 x65@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_208_case__72 x1 x2 x3 x4 x14 x62 x63 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_208_case__72(x1)(x2)(x3)(x4)(x14)(x62)(x63)(x)(st))(i)(xs)(st)
c__case_208_case__72 x1 x2 x3 x4 x14 x62 x63 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_208_case__72")(x)



c__case_209_case__73 x1 x2 x3 x4 x7 x14 x62 x63 x11@(Curry.Module.AbstractCurry.C_CVar x64) st = let {x78 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x79 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x80 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x81 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x82 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x83 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x84 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x85 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x78)((Curry.Module.Prelude.:<)(x79)((Curry.Module.Prelude.:<)(x80)((Curry.Module.Prelude.:<)(x81)((Curry.Module.Prelude.:<)(x82)((Curry.Module.Prelude.:<)(x83)((Curry.Module.Prelude.:<)(x84)((Curry.Module.Prelude.:<)(x85)(Curry.Module.Prelude.List)))))))))(Curry.Module.OracleAbstractCurryPrinter.c__case_208(x2)(x3)(x4)(x7)(x14)(x62)(x63)(x64)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_isInfixOpName(x1)(st))(x14)(x78)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x7)(x64)(x79)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OracleAbstractCurryPrinter.c_isAtom(Curry.Module.AbstractCurry.C_CLambda(x62)(x63))(x80)(st))(Curry.Module.OraclePrelude.op_47_61(Curry.Module.AbstractCurry.C_CVar(x64))(Curry.Module.AbstractCurry.C_CLambda(x62)(x63))(x81)(st))(x82)(st))(x83)(st))(x84)(st))(x85)(st))(st)
c__case_209_case__73 x1 x2 x3 x4 x7 x14 x62 x63 x11@(Curry.Module.AbstractCurry.C_CLit x65) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_209_case__73 x1 x2 x3 x4 x7 x14 x62 x63 x11@(Curry.Module.AbstractCurry.C_CSymbol x66) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_209_case__73 x1 x2 x3 x4 x7 x14 x62 x63 x11@(Curry.Module.AbstractCurry.C_CApply x67 x68) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_209_case__73 x1 x2 x3 x4 x7 x14 x62 x63 x11@(Curry.Module.AbstractCurry.C_CLambda x69 x70) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_209_case__73 x1 x2 x3 x4 x7 x14 x62 x63 x11@(Curry.Module.AbstractCurry.C_CLetDecl x71 x72) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_209_case__73 x1 x2 x3 x4 x7 x14 x62 x63 x11@(Curry.Module.AbstractCurry.C_CDoExpr x73) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_209_case__73 x1 x2 x3 x4 x7 x14 x62 x63 x11@(Curry.Module.AbstractCurry.C_CListComp x74 x75) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_209_case__73 x1 x2 x3 x4 x7 x14 x62 x63 x11@(Curry.Module.AbstractCurry.C_CCase x76 x77) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_209_case__73 x1 x2 x3 x4 x7 x14 x62 x63 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_209_case__73(x1)(x2)(x3)(x4)(x7)(x14)(x62)(x63)(x)(st))(i)(xs)(st)
c__case_209_case__73 x1 x2 x3 x4 x7 x14 x62 x63 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_209_case__73")(x)



c__case_206_case__74 x1 x2 x3 x4 x14 x78 x79 x81@Curry.Module.Prelude.C_True st = let {x82 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x83 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x84 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x85 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x82)((Curry.Module.Prelude.:<)(x83)((Curry.Module.Prelude.:<)(x84)((Curry.Module.Prelude.:<)(x85)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x14)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(Curry.Module.AbstractCurry.C_CLetDecl(x78)(x79))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x82)(st))(x83)(st))(x84)(st))(x85)(st))(st)
c__case_206_case__74 x1 x2 x3 x4 x14 x78 x79 x81@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_206_case__74 x1 x2 x3 x4 x14 x78 x79 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_206_case__74(x1)(x2)(x3)(x4)(x14)(x78)(x79)(x)(st))(i)(xs)(st)
c__case_206_case__74 x1 x2 x3 x4 x14 x78 x79 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_206_case__74")(x)



c__case_207_case__75 x1 x2 x3 x4 x7 x14 x78 x79 x11@(Curry.Module.AbstractCurry.C_CVar x80) st = let {x94 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x95 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x96 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x97 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x98 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x99 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x100 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x101 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x94)((Curry.Module.Prelude.:<)(x95)((Curry.Module.Prelude.:<)(x96)((Curry.Module.Prelude.:<)(x97)((Curry.Module.Prelude.:<)(x98)((Curry.Module.Prelude.:<)(x99)((Curry.Module.Prelude.:<)(x100)((Curry.Module.Prelude.:<)(x101)(Curry.Module.Prelude.List)))))))))(Curry.Module.OracleAbstractCurryPrinter.c__case_206(x2)(x3)(x4)(x7)(x14)(x78)(x79)(x80)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_isInfixOpName(x1)(st))(x14)(x94)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x7)(x80)(x95)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OracleAbstractCurryPrinter.c_isAtom(Curry.Module.AbstractCurry.C_CLetDecl(x78)(x79))(x96)(st))(Curry.Module.OraclePrelude.op_47_61(Curry.Module.AbstractCurry.C_CVar(x80))(Curry.Module.AbstractCurry.C_CLetDecl(x78)(x79))(x97)(st))(x98)(st))(x99)(st))(x100)(st))(x101)(st))(st)
c__case_207_case__75 x1 x2 x3 x4 x7 x14 x78 x79 x11@(Curry.Module.AbstractCurry.C_CLit x81) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_207_case__75 x1 x2 x3 x4 x7 x14 x78 x79 x11@(Curry.Module.AbstractCurry.C_CSymbol x82) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_207_case__75 x1 x2 x3 x4 x7 x14 x78 x79 x11@(Curry.Module.AbstractCurry.C_CApply x83 x84) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_207_case__75 x1 x2 x3 x4 x7 x14 x78 x79 x11@(Curry.Module.AbstractCurry.C_CLambda x85 x86) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_207_case__75 x1 x2 x3 x4 x7 x14 x78 x79 x11@(Curry.Module.AbstractCurry.C_CLetDecl x87 x88) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_207_case__75 x1 x2 x3 x4 x7 x14 x78 x79 x11@(Curry.Module.AbstractCurry.C_CDoExpr x89) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_207_case__75 x1 x2 x3 x4 x7 x14 x78 x79 x11@(Curry.Module.AbstractCurry.C_CListComp x90 x91) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_207_case__75 x1 x2 x3 x4 x7 x14 x78 x79 x11@(Curry.Module.AbstractCurry.C_CCase x92 x93) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_207_case__75 x1 x2 x3 x4 x7 x14 x78 x79 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_207_case__75(x1)(x2)(x3)(x4)(x7)(x14)(x78)(x79)(x)(st))(i)(xs)(st)
c__case_207_case__75 x1 x2 x3 x4 x7 x14 x78 x79 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_207_case__75")(x)



c__case_204_case__76 x1 x2 x3 x4 x14 x94 x96@Curry.Module.Prelude.C_True st = let {x97 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x98 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x99 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x100 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x97)((Curry.Module.Prelude.:<)(x98)((Curry.Module.Prelude.:<)(x99)((Curry.Module.Prelude.:<)(x100)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x14)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(Curry.Module.AbstractCurry.C_CDoExpr(x94))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x97)(st))(x98)(st))(x99)(st))(x100)(st))(st)
c__case_204_case__76 x1 x2 x3 x4 x14 x94 x96@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_204_case__76 x1 x2 x3 x4 x14 x94 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_204_case__76(x1)(x2)(x3)(x4)(x14)(x94)(x)(st))(i)(xs)(st)
c__case_204_case__76 x1 x2 x3 x4 x14 x94 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_204_case__76")(x)



c__case_205_case__77 x1 x2 x3 x4 x7 x14 x94 x11@(Curry.Module.AbstractCurry.C_CVar x95) st = let {x109 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x110 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x111 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x112 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x113 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x114 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x115 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x116 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x109)((Curry.Module.Prelude.:<)(x110)((Curry.Module.Prelude.:<)(x111)((Curry.Module.Prelude.:<)(x112)((Curry.Module.Prelude.:<)(x113)((Curry.Module.Prelude.:<)(x114)((Curry.Module.Prelude.:<)(x115)((Curry.Module.Prelude.:<)(x116)(Curry.Module.Prelude.List)))))))))(Curry.Module.OracleAbstractCurryPrinter.c__case_204(x2)(x3)(x4)(x7)(x14)(x94)(x95)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_isInfixOpName(x1)(st))(x14)(x109)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x7)(x95)(x110)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OracleAbstractCurryPrinter.c_isAtom(Curry.Module.AbstractCurry.C_CDoExpr(x94))(x111)(st))(Curry.Module.OraclePrelude.op_47_61(Curry.Module.AbstractCurry.C_CVar(x95))(Curry.Module.AbstractCurry.C_CDoExpr(x94))(x112)(st))(x113)(st))(x114)(st))(x115)(st))(x116)(st))(st)
c__case_205_case__77 x1 x2 x3 x4 x7 x14 x94 x11@(Curry.Module.AbstractCurry.C_CLit x96) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_205_case__77 x1 x2 x3 x4 x7 x14 x94 x11@(Curry.Module.AbstractCurry.C_CSymbol x97) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_205_case__77 x1 x2 x3 x4 x7 x14 x94 x11@(Curry.Module.AbstractCurry.C_CApply x98 x99) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_205_case__77 x1 x2 x3 x4 x7 x14 x94 x11@(Curry.Module.AbstractCurry.C_CLambda x100 x101) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_205_case__77 x1 x2 x3 x4 x7 x14 x94 x11@(Curry.Module.AbstractCurry.C_CLetDecl x102 x103) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_205_case__77 x1 x2 x3 x4 x7 x14 x94 x11@(Curry.Module.AbstractCurry.C_CDoExpr x104) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_205_case__77 x1 x2 x3 x4 x7 x14 x94 x11@(Curry.Module.AbstractCurry.C_CListComp x105 x106) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_205_case__77 x1 x2 x3 x4 x7 x14 x94 x11@(Curry.Module.AbstractCurry.C_CCase x107 x108) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_205_case__77 x1 x2 x3 x4 x7 x14 x94 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_205_case__77(x1)(x2)(x3)(x4)(x7)(x14)(x94)(x)(st))(i)(xs)(st)
c__case_205_case__77 x1 x2 x3 x4 x7 x14 x94 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_205_case__77")(x)



c__case_202_case__78 x1 x2 x3 x4 x14 x109 x110 x112@Curry.Module.Prelude.C_True st = let {x113 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x114 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x115 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x116 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x113)((Curry.Module.Prelude.:<)(x114)((Curry.Module.Prelude.:<)(x115)((Curry.Module.Prelude.:<)(x116)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x14)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(Curry.Module.AbstractCurry.C_CListComp(x109)(x110))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x113)(st))(x114)(st))(x115)(st))(x116)(st))(st)
c__case_202_case__78 x1 x2 x3 x4 x14 x109 x110 x112@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_202_case__78 x1 x2 x3 x4 x14 x109 x110 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_202_case__78(x1)(x2)(x3)(x4)(x14)(x109)(x110)(x)(st))(i)(xs)(st)
c__case_202_case__78 x1 x2 x3 x4 x14 x109 x110 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_202_case__78")(x)



c__case_203_case__79 x1 x2 x3 x4 x7 x14 x109 x110 x11@(Curry.Module.AbstractCurry.C_CVar x111) st = let {x125 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x126 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x127 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x128 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x129 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x130 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x131 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x132 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x125)((Curry.Module.Prelude.:<)(x126)((Curry.Module.Prelude.:<)(x127)((Curry.Module.Prelude.:<)(x128)((Curry.Module.Prelude.:<)(x129)((Curry.Module.Prelude.:<)(x130)((Curry.Module.Prelude.:<)(x131)((Curry.Module.Prelude.:<)(x132)(Curry.Module.Prelude.List)))))))))(Curry.Module.OracleAbstractCurryPrinter.c__case_202(x2)(x3)(x4)(x7)(x14)(x109)(x110)(x111)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_isInfixOpName(x1)(st))(x14)(x125)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x7)(x111)(x126)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OracleAbstractCurryPrinter.c_isAtom(Curry.Module.AbstractCurry.C_CListComp(x109)(x110))(x127)(st))(Curry.Module.OraclePrelude.op_47_61(Curry.Module.AbstractCurry.C_CVar(x111))(Curry.Module.AbstractCurry.C_CListComp(x109)(x110))(x128)(st))(x129)(st))(x130)(st))(x131)(st))(x132)(st))(st)
c__case_203_case__79 x1 x2 x3 x4 x7 x14 x109 x110 x11@(Curry.Module.AbstractCurry.C_CLit x112) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_203_case__79 x1 x2 x3 x4 x7 x14 x109 x110 x11@(Curry.Module.AbstractCurry.C_CSymbol x113) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_203_case__79 x1 x2 x3 x4 x7 x14 x109 x110 x11@(Curry.Module.AbstractCurry.C_CApply x114 x115) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_203_case__79 x1 x2 x3 x4 x7 x14 x109 x110 x11@(Curry.Module.AbstractCurry.C_CLambda x116 x117) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_203_case__79 x1 x2 x3 x4 x7 x14 x109 x110 x11@(Curry.Module.AbstractCurry.C_CLetDecl x118 x119) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_203_case__79 x1 x2 x3 x4 x7 x14 x109 x110 x11@(Curry.Module.AbstractCurry.C_CDoExpr x120) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_203_case__79 x1 x2 x3 x4 x7 x14 x109 x110 x11@(Curry.Module.AbstractCurry.C_CListComp x121 x122) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_203_case__79 x1 x2 x3 x4 x7 x14 x109 x110 x11@(Curry.Module.AbstractCurry.C_CCase x123 x124) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_203_case__79 x1 x2 x3 x4 x7 x14 x109 x110 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_203_case__79(x1)(x2)(x3)(x4)(x7)(x14)(x109)(x110)(x)(st))(i)(xs)(st)
c__case_203_case__79 x1 x2 x3 x4 x7 x14 x109 x110 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_203_case__79")(x)



c__case_200_case__80 x1 x2 x3 x4 x14 x125 x126 x128@Curry.Module.Prelude.C_True st = let {x129 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x130 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x131 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x132 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x129)((Curry.Module.Prelude.:<)(x130)((Curry.Module.Prelude.:<)(x131)((Curry.Module.Prelude.:<)(x132)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x14)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(Curry.Module.AbstractCurry.C_CCase(x125)(x126))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x129)(st))(x130)(st))(x131)(st))(x132)(st))(st)
c__case_200_case__80 x1 x2 x3 x4 x14 x125 x126 x128@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_200_case__80 x1 x2 x3 x4 x14 x125 x126 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_200_case__80(x1)(x2)(x3)(x4)(x14)(x125)(x126)(x)(st))(i)(xs)(st)
c__case_200_case__80 x1 x2 x3 x4 x14 x125 x126 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_200_case__80")(x)



c__case_201_case__81 x1 x2 x3 x4 x7 x14 x125 x126 x11@(Curry.Module.AbstractCurry.C_CVar x127) st = let {x141 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x142 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x143 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x144 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x145 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x146 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x147 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x148 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x141)((Curry.Module.Prelude.:<)(x142)((Curry.Module.Prelude.:<)(x143)((Curry.Module.Prelude.:<)(x144)((Curry.Module.Prelude.:<)(x145)((Curry.Module.Prelude.:<)(x146)((Curry.Module.Prelude.:<)(x147)((Curry.Module.Prelude.:<)(x148)(Curry.Module.Prelude.List)))))))))(Curry.Module.OracleAbstractCurryPrinter.c__case_200(x2)(x3)(x4)(x7)(x14)(x125)(x126)(x127)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_isInfixOpName(x1)(st))(x14)(x141)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x7)(x127)(x142)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OracleAbstractCurryPrinter.c_isAtom(Curry.Module.AbstractCurry.C_CCase(x125)(x126))(x143)(st))(Curry.Module.OraclePrelude.op_47_61(Curry.Module.AbstractCurry.C_CVar(x127))(Curry.Module.AbstractCurry.C_CCase(x125)(x126))(x144)(st))(x145)(st))(x146)(st))(x147)(st))(x148)(st))(st)
c__case_201_case__81 x1 x2 x3 x4 x7 x14 x125 x126 x11@(Curry.Module.AbstractCurry.C_CLit x128) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_201_case__81 x1 x2 x3 x4 x7 x14 x125 x126 x11@(Curry.Module.AbstractCurry.C_CSymbol x129) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_201_case__81 x1 x2 x3 x4 x7 x14 x125 x126 x11@(Curry.Module.AbstractCurry.C_CApply x130 x131) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_201_case__81 x1 x2 x3 x4 x7 x14 x125 x126 x11@(Curry.Module.AbstractCurry.C_CLambda x132 x133) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_201_case__81 x1 x2 x3 x4 x7 x14 x125 x126 x11@(Curry.Module.AbstractCurry.C_CLetDecl x134 x135) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_201_case__81 x1 x2 x3 x4 x7 x14 x125 x126 x11@(Curry.Module.AbstractCurry.C_CDoExpr x136) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_201_case__81 x1 x2 x3 x4 x7 x14 x125 x126 x11@(Curry.Module.AbstractCurry.C_CListComp x137 x138) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_201_case__81 x1 x2 x3 x4 x7 x14 x125 x126 x11@(Curry.Module.AbstractCurry.C_CCase x139 x140) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_201_case__81 x1 x2 x3 x4 x7 x14 x125 x126 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_201_case__81(x1)(x2)(x3)(x4)(x7)(x14)(x125)(x126)(x)(st))(i)(xs)(st)
c__case_201_case__81 x1 x2 x3 x4 x7 x14 x125 x126 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_201_case__81")(x)



c__case_219_case__82 x1 x2 x3 x4 x7 x11 x14 x9@(Curry.Module.AbstractCurry.C_CVar x15) st = let {x127 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x128 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x129 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x130 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x131 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x132 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x127)((Curry.Module.Prelude.:<)(x128)((Curry.Module.Prelude.:<)(x129)((Curry.Module.Prelude.:<)(x130)((Curry.Module.Prelude.:<)(x131)((Curry.Module.Prelude.:<)(x132)(Curry.Module.Prelude.List)))))))(Curry.Module.OracleAbstractCurryPrinter.c__case_218(x2)(x3)(x4)(x7)(x11)(x14)(x15)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_isInfixOpName(x1)(st))(x14)(x127)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OracleAbstractCurryPrinter.c_isAtom(x11)(x128)(st))(Curry.Module.OraclePrelude.op_47_61(Curry.Module.AbstractCurry.C_CVar(x15))(x11)(x129)(st))(x130)(st))(x131)(st))(x132)(st))(st)
c__case_219_case__82 x1 x2 x3 x4 x7 x11 x14 x9@(Curry.Module.AbstractCurry.C_CLit x16) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_215(x2)(x3)(x4)(x7)(x14)(x16)(x11)(x1)(st))(st)
c__case_219_case__82 x1 x2 x3 x4 x7 x11 x14 x9@(Curry.Module.AbstractCurry.C_CSymbol x31) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_213(x2)(x3)(x4)(x7)(x14)(x31)(x11)(x1)(st))(st)
c__case_219_case__82 x1 x2 x3 x4 x7 x11 x14 x9@(Curry.Module.AbstractCurry.C_CApply x46 x47) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_211(x2)(x3)(x4)(x7)(x14)(x46)(x47)(x11)(x1)(st))(st)
c__case_219_case__82 x1 x2 x3 x4 x7 x11 x14 x9@(Curry.Module.AbstractCurry.C_CLambda x62 x63) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_209(x2)(x3)(x4)(x7)(x14)(x62)(x63)(x11)(x1)(st))(st)
c__case_219_case__82 x1 x2 x3 x4 x7 x11 x14 x9@(Curry.Module.AbstractCurry.C_CLetDecl x78 x79) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_207(x2)(x3)(x4)(x7)(x14)(x78)(x79)(x11)(x1)(st))(st)
c__case_219_case__82 x1 x2 x3 x4 x7 x11 x14 x9@(Curry.Module.AbstractCurry.C_CDoExpr x94) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_205(x2)(x3)(x4)(x7)(x14)(x94)(x11)(x1)(st))(st)
c__case_219_case__82 x1 x2 x3 x4 x7 x11 x14 x9@(Curry.Module.AbstractCurry.C_CListComp x109 x110) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_203(x2)(x3)(x4)(x7)(x14)(x109)(x110)(x11)(x1)(st))(st)
c__case_219_case__82 x1 x2 x3 x4 x7 x11 x14 x9@(Curry.Module.AbstractCurry.C_CCase x125 x126) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_201(x2)(x3)(x4)(x7)(x14)(x125)(x126)(x11)(x1)(st))(st)
c__case_219_case__82 x1 x2 x3 x4 x7 x11 x14 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_219_case__82(x1)(x2)(x3)(x4)(x7)(x11)(x14)(x)(st))(i)(xs)(st)
c__case_219_case__82 x1 x2 x3 x4 x7 x11 x14 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_219_case__82")(x)



c__case_220_case__83 x1 x2 x3 x4 x7 x9 x11 x12@(Curry.Module.Prelude.T2 x13 x14) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_219(x2)(x3)(x4)(x7)(x11)(x14)(x9)(x1)(st))(st)
c__case_220_case__83 x1 x2 x3 x4 x7 x9 x11 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_220_case__83(x1)(x2)(x3)(x4)(x7)(x9)(x11)(x)(st))(i)(xs)(st)
c__case_220_case__83 x1 x2 x3 x4 x7 x9 x11 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_220_case__83")(x)



c__case_221_case__84 x1 x2 x3 x4 x7 x9 x11 x10@(Curry.Module.AbstractCurry.C_CSymbol x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_220(x2)(x3)(x4)(x7)(x9)(x11)(x12)(x1)(st))(st)
c__case_221_case__84 x1 x2 x3 x4 x7 x9 x11 x10@(Curry.Module.AbstractCurry.C_CVar x141) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_221_case__84 x1 x2 x3 x4 x7 x9 x11 x10@(Curry.Module.AbstractCurry.C_CLit x142) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_221_case__84 x1 x2 x3 x4 x7 x9 x11 x10@(Curry.Module.AbstractCurry.C_CApply x143 x144) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_221_case__84 x1 x2 x3 x4 x7 x9 x11 x10@(Curry.Module.AbstractCurry.C_CLambda x145 x146) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_221_case__84 x1 x2 x3 x4 x7 x9 x11 x10@(Curry.Module.AbstractCurry.C_CLetDecl x147 x148) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_221_case__84 x1 x2 x3 x4 x7 x9 x11 x10@(Curry.Module.AbstractCurry.C_CDoExpr x149) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_221_case__84 x1 x2 x3 x4 x7 x9 x11 x10@(Curry.Module.AbstractCurry.C_CListComp x150 x151) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_221_case__84 x1 x2 x3 x4 x7 x9 x11 x10@(Curry.Module.AbstractCurry.C_CCase x152 x153) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_221_case__84 x1 x2 x3 x4 x7 x9 x11 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_221_case__84(x1)(x2)(x3)(x4)(x7)(x9)(x11)(x)(st))(i)(xs)(st)
c__case_221_case__84 x1 x2 x3 x4 x7 x9 x11 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_221_case__84")(x)



c__case_222_case__85 x1 x2 x3 x4 x7 x9 x8@(Curry.Module.AbstractCurry.C_CApply x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_221(x2)(x3)(x4)(x7)(x9)(x11)(x10)(x1)(st))(st)
c__case_222_case__85 x1 x2 x3 x4 x7 x9 x8@(Curry.Module.AbstractCurry.C_CVar x154) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_222_case__85 x1 x2 x3 x4 x7 x9 x8@(Curry.Module.AbstractCurry.C_CLit x155) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_222_case__85 x1 x2 x3 x4 x7 x9 x8@(Curry.Module.AbstractCurry.C_CSymbol x156) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_222_case__85 x1 x2 x3 x4 x7 x9 x8@(Curry.Module.AbstractCurry.C_CLambda x157 x158) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_222_case__85 x1 x2 x3 x4 x7 x9 x8@(Curry.Module.AbstractCurry.C_CLetDecl x159 x160) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_222_case__85 x1 x2 x3 x4 x7 x9 x8@(Curry.Module.AbstractCurry.C_CDoExpr x161) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_222_case__85 x1 x2 x3 x4 x7 x9 x8@(Curry.Module.AbstractCurry.C_CListComp x162 x163) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_222_case__85 x1 x2 x3 x4 x7 x9 x8@(Curry.Module.AbstractCurry.C_CCase x164 x165) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_222_case__85 x1 x2 x3 x4 x7 x9 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_222_case__85(x1)(x2)(x3)(x4)(x7)(x9)(x)(st))(i)(xs)(st)
c__case_222_case__85 x1 x2 x3 x4 x7 x9 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_222_case__85")(x)



c__case_223_case__86 x1 x2 x3 x7 x4@(Curry.Module.AbstractCurry.C_CApply x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_222(x2)(x3)(x4)(x7)(x9)(x8)(x1)(st))(st)
c__case_223_case__86 x1 x2 x3 x7 x4@(Curry.Module.AbstractCurry.C_CVar x166) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_223_case__86 x1 x2 x3 x7 x4@(Curry.Module.AbstractCurry.C_CLit x167) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_223_case__86 x1 x2 x3 x7 x4@(Curry.Module.AbstractCurry.C_CSymbol x168) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_223_case__86 x1 x2 x3 x7 x4@(Curry.Module.AbstractCurry.C_CLambda x169 x170) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_223_case__86 x1 x2 x3 x7 x4@(Curry.Module.AbstractCurry.C_CLetDecl x171 x172) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_223_case__86 x1 x2 x3 x7 x4@(Curry.Module.AbstractCurry.C_CDoExpr x173) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_223_case__86 x1 x2 x3 x7 x4@(Curry.Module.AbstractCurry.C_CListComp x174 x175) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_223_case__86 x1 x2 x3 x7 x4@(Curry.Module.AbstractCurry.C_CCase x176 x177) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_223_case__86 x1 x2 x3 x7 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_223_case__86(x1)(x2)(x3)(x7)(x)(st))(i)(xs)(st)
c__case_223_case__86 x1 x2 x3 x7 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_223_case__86")(x)



c__case_224_case__87 x1 x2 x3 x4 x7 x6@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_223(x2)(x3)(x7)(x4)(x1)(st))(st)
c__case_224_case__87 x1 x2 x3 x4 x7 x6@((Curry.Module.Prelude.:<) x178 x179) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_224_case__87 x1 x2 x3 x4 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_224_case__87(x1)(x2)(x3)(x4)(x7)(x)(st))(i)(xs)(st)
c__case_224_case__87 x1 x2 x3 x4 x7 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_224_case__87")(x)



c__case_225_case__88 x1 x2 x3 x4 x6 x5@(Curry.Module.AbstractCurry.C_CPVar x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_224(x2)(x3)(x4)(x7)(x6)(x1)(st))(st)
c__case_225_case__88 x1 x2 x3 x4 x6 x5@(Curry.Module.AbstractCurry.C_CPLit x180) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_225_case__88 x1 x2 x3 x4 x6 x5@(Curry.Module.AbstractCurry.C_CPComb x181 x182) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_225_case__88 x1 x2 x3 x4 x6 x5@(Curry.Module.AbstractCurry.C_CPAs x183 x184) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_225_case__88 x1 x2 x3 x4 x6 x5@(Curry.Module.AbstractCurry.C_CPFuncComb x185 x186) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_225_case__88 x1 x2 x3 x4 x6 (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_225_case__88(x1)(x2)(x3)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_225_case__88 x1 x2 x3 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_225_case__88")(x)



c__case_226_case__89 x1 x2 x4 x3@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_225(x2)(x3)(x4)(x6)(x5)(x1)(st))(st)
c__case_226_case__89 x1 x2 x4 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLambda(x2)(x3)(x4)(x1)(st))(st)
c__case_226_case__89 x1 x2 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_226_case__89(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_226_case__89 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_226_case__89")(x)



c__case_197_case__90 x1 x2 x7 x8 x9@Curry.Module.Prelude.List st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.OracleAbstractCurryPrinter.c_showLocalDecl(x2)(x8)(x1)(st))(x12)(st))(st)
c__case_197_case__90 x1 x2 x7 x8 x9@((Curry.Module.Prelude.:<) x10 x11) st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))(Curry.Module.OracleAbstractCurryPrinter.c_showBlock(Curry.Module.OracleAbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showLocalDecl(x2)))))(x7)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x1)(st))(x13)(st))(x14)(st))(st)
c__case_197_case__90 x1 x2 x7 x8 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_197_case__90(x1)(x2)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_197_case__90 x1 x2 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_197_case__90")(x)



c__case_198_case__91 x1 x2 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_197(x2)(x7)(x8)(x9)(x1)(st))(st)
c__case_198_case__91 x1 x2 x7@Curry.Module.Prelude.List st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))(Curry.Module.OracleAbstractCurryPrinter.c_showBlock(Curry.Module.OracleAbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showLocalDecl(x2)))))(x7)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x1)(st))(x10)(st))(x11)(st))(st)
c__case_198_case__91 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_198_case__91(x1)(x2)(x)(st))(i)(xs)(st)
c__case_198_case__91 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_198_case__91")(x)



c__case_199_case__92 x1 x2 x3@(Curry.Module.AbstractCurry.C_CSExpr x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showExprOpt(x2)(x4)(x1)(st))(st)
c__case_199_case__92 x1 x2 x3@(Curry.Module.AbstractCurry.C_CSPat x5 x6) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showPattern(x5)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.OracleAbstractCurryPrinter.c_showExprOpt(x2)(x6)(x8)(st))(x9)(st))(x10)(st))(st)
c__case_199_case__92 x1 x2 x3@(Curry.Module.AbstractCurry.C_CSLet x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_198(x2)(x7)(x1)(st))(st)
c__case_199_case__92 x1 x2 (Curry.Module.AbstractCurry.C_CStatementOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_199_case__92(x1)(x2)(x)(st))(i)(xs)(st)
c__case_199_case__92 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_199_case__92")(x)



c__case_195_case__93 x1 x3@(Curry.Module.Prelude.T2 x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_showIdentifier(x1)(st))(x5)(x6)(st))(st)
c__case_195_case__93 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_195_case__93(x1)(x)(st))(i)(xs)(st)
c__case_195_case__93 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_195_case__93")(x)



c__case_191_case__94 x1 x10 x11 x12 x13@Curry.Module.Prelude.C_True st = let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x10)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showPattern))))((Curry.Module.Prelude.:<)(x11)(x12))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x14)(st))(x15)(st))(x16)(st))(st)
c__case_191_case__94 x1 x10 x11 x12 x13@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_191_case__94 x1 x10 x11 x12 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_191_case__94(x1)(x10)(x11)(x12)(x)(st))(i)(xs)(st)
c__case_191_case__94 x1 x10 x11 x12 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_191_case__94")(x)



c__case_192_case__95 x1 x9 x10 x11 x12 x13@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showPreludeCons(Curry.Module.AbstractCurry.C_CPComb(Curry.Module.Prelude.T2(x9)(x10))((Curry.Module.Prelude.:<)(x11)(x12)))(x1)(st))(st)
c__case_192_case__95 x1 x9 x10 x11 x12 x13@Curry.Module.Prelude.C_False st = let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_191(x10)(x11)(x12)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x14)(st))(st)
c__case_192_case__95 x1 x9 x10 x11 x12 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_192_case__95(x1)(x9)(x10)(x11)(x12)(x)(st))(i)(xs)(st)
c__case_192_case__95 x1 x9 x10 x11 x12 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_192_case__95")(x)



c__case_193_case__96 x1 x9 x10 x8@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x10)(st)
c__case_193_case__96 x1 x9 x10 x8@((Curry.Module.Prelude.:<) x11 x12) st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_192(x9)(x10)(x11)(x12)(Curry.Module.OraclePrelude.op_61_61(x9)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(x1)(st))(x13)(st))(st)
c__case_193_case__96 x1 x9 x10 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_193_case__96(x1)(x9)(x10)(x)(st))(i)(xs)(st)
c__case_193_case__96 x1 x9 x10 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_193_case__96")(x)



c__case_194_case__97 x1 x8 x7@(Curry.Module.Prelude.T2 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_193(x9)(x10)(x8)(x1)(st))(st)
c__case_194_case__97 x1 x8 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_194_case__97(x1)(x8)(x)(st))(i)(xs)(st)
c__case_194_case__97 x1 x8 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_194_case__97")(x)



c__case_190_case__98 x1 x14 x13@(Curry.Module.Prelude.T2 x15 x16) st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_showIdentifier(x1)(st))(x16)(x17)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('@'))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showPattern(x14)(x18)(st))(x19)(st))(x20)(st))(st)
c__case_190_case__98 x1 x14 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_190_case__98(x1)(x14)(x)(st))(i)(xs)(st)
c__case_190_case__98 x1 x14 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_190_case__98")(x)



c__case_196_case__99 x1 x2@(Curry.Module.AbstractCurry.C_CPVar x3) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_195(x3)(x1)(st))(st)
c__case_196_case__99 x1 x2@(Curry.Module.AbstractCurry.C_CPLit x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showLiteral(x6)(x1)(st))(st)
c__case_196_case__99 x1 x2@(Curry.Module.AbstractCurry.C_CPComb x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_194(x8)(x7)(x1)(st))(st)
c__case_196_case__99 x1 x2@(Curry.Module.AbstractCurry.C_CPAs x13 x14) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_190(x14)(x13)(x1)(st))(st)
c__case_196_case__99 x1 x2@(Curry.Module.AbstractCurry.C_CPFuncComb x17 x18) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showPattern(Curry.Module.AbstractCurry.C_CPComb(x17)(x18))(x1)(st))(st)
c__case_196_case__99 x1 (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_196_case__99(x1)(x)(st))(i)(xs)(st)
c__case_196_case__99 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_196_case__99")(x)



c__case_187_case__100 x1 x4 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x4)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showPattern))))(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x7)(st))(x8)(st))(x9)(st))(st)
c__case_187_case__100 x1 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_187_case__100 x1 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_187_case__100(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_187_case__100 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_187_case__100")(x)



c__case_188_case__101 x1 x4 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showPattern))))(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x7)(st))(x8)(st))(st)
c__case_188_case__101 x1 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_187(x4)(x5)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x9)(st))(st)
c__case_188_case__101 x1 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_188_case__101(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_188_case__101 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_188_case__101")(x)



c__case_189_case__102 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showPatternList(x2)(x1)(st))(st)
c__case_189_case__102 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_188(x4)(x5)(Curry.Module.OracleAbstractCurryPrinter.c_isTuple(x4)(x1)(st))(x7)(st))(st)
c__case_189_case__102 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_189_case__102(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_189_case__102 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_189_case__102")(x)



c__case_185_case__103 x1 x3@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_185_case__103 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_185_case__103(x1)(x)(st))(i)(xs)(st)
c__case_185_case__103 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_185_case__103")(x)



c__case_186_case__104 x1 x2@(Curry.Module.AbstractCurry.C_CPComb x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_185(x3)(x1)(st))(st)
c__case_186_case__104 x1 (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_186_case__104(x1)(x)(st))(i)(xs)(st)
c__case_186_case__104 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_186_case__104")(x)



c__case_183_case__105 x1 x4 x3@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_183_case__105 x1 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_183_case__105(x1)(x4)(x)(st))(i)(xs)(st)
c__case_183_case__105 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_183_case__105")(x)



c__case_184_case__106 x1 x2@(Curry.Module.AbstractCurry.C_CPComb x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_183(x4)(x3)(x1)(st))(st)
c__case_184_case__106 x1 (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_184_case__106(x1)(x)(st))(i)(xs)(st)
c__case_184_case__106 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_184_case__106")(x)



c__case_179_case__107 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_concat(Curry.Module.OracleList.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showPatListElems(x2)(x1)(st))(x4)(st))(x5)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x6)(st))(x7)(st))(st)
c__case_179_case__107 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_179_case__107 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_179_case__107(x1)(x2)(x)(st))(i)(xs)(st)
c__case_179_case__107 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_179_case__107")(x)



c__case_180_case__108 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showAsPatternList(x2)(x1)(st))(st)
c__case_180_case__108 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_179(x2)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x4)(st))(st)
c__case_180_case__108 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_180_case__108(x1)(x2)(x)(st))(i)(xs)(st)
c__case_180_case__108 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_180_case__108")(x)



c__case_181_case__109 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_concat(Curry.Module.OracleList.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showPatListElems(x2)(x1)(st))(x4)(st))(x5)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))(x6)(st))(x7)(st))(st)
c__case_181_case__109 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_180(x2)(Curry.Module.OracleAbstractCurryPrinter.c_isAsPattern(x2)(x1)(st))(x8)(st))(st)
c__case_181_case__109 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_181_case__109(x1)(x2)(x)(st))(i)(xs)(st)
c__case_181_case__109 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_181_case__109")(x)



c__case_182_case__110 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_filter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_47_61))(st))(Curry.Module.Prelude.C_Char('\''))))))(Curry.Module.OraclePrelude.c_concat(Curry.Module.OracleAbstractCurryPrinter.c_showPatListElems(x2)(x1)(st))(x4)(st))(x5)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List))(x6)(st)))(st)
c__case_182_case__110 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_181(x2)(Curry.Module.OracleAbstractCurryPrinter.c_isClosedPatternList(x2)(x1)(st))(x7)(st))(st)
c__case_182_case__110 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_182_case__110(x1)(x2)(x)(st))(i)(xs)(st)
c__case_182_case__110 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_182_case__110")(x)



c__case_156_case__111 x1 x23 x25 x26@Curry.Module.Prelude.List st = let {x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.OracleAbstractCurryPrinter.c_showPattern(x23)(x1)(st))(Curry.Module.OracleAbstractCurryPrinter.c_showPatListElems(x25)(x27)(st)))(st)
c__case_156_case__111 x1 x23 x25 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_156_case__111(x1)(x23)(x25)(x)(st))(i)(xs)(st)
c__case_156_case__111 x1 x23 x25 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_156_case__111")(x)



c__case_157_case__112 x1 x23 x24@((Curry.Module.Prelude.:<) x25 x26) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_156(x23)(x25)(x26)(x1)(st))(st)
c__case_157_case__112 x1 x23 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_157_case__112(x1)(x23)(x)(st))(i)(xs)(st)
c__case_157_case__112 x1 x23 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_157_case__112")(x)



c__case_158_case__113 x1 x4@((Curry.Module.Prelude.:<) x23 x24) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_157(x23)(x24)(x1)(st))(st)
c__case_158_case__113 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_158_case__113(x1)(x)(st))(i)(xs)(st)
c__case_158_case__113 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_158_case__113")(x)



c__case_159_case__114 x1 x4 x22@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_158(x4)(x1)(st))(st)
c__case_159_case__114 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_159_case__114(x1)(x4)(x)(st))(i)(xs)(st)
c__case_159_case__114 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_159_case__114")(x)



c__case_152_case__115 x1 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_152_case__115 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_152_case__115(x1)(x)(st))(i)(xs)(st)
c__case_152_case__115 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_152_case__115")(x)



c__case_153_case__116 x1 x4 x28@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_152(x4)(x1)(st))(st)
c__case_153_case__116 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_153_case__116(x1)(x4)(x)(st))(i)(xs)(st)
c__case_153_case__116 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_153_case__116")(x)



c__case_154_case__117 x1 x4 x28 x27 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x27)(Curry.Module.Prelude.C_Char(']'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_153(x4)(x28)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_155_case__118 x1 x4 x22@((Curry.Module.Prelude.:<) x27 x28) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_154(x4)(x28)(x27)(x1)(st))(st)
c__case_155_case__118 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_155_case__118(x1)(x4)(x)(st))(i)(xs)(st)
c__case_155_case__118 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_155_case__118")(x)



c__case_160_case__119 x1 x4 x22 x21 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x21)(Curry.Module.Prelude.C_Char(':'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_159(x4)(x22)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x21)(Curry.Module.Prelude.C_Char('['))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_155(x4)(x22)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st)



c__case_161_case__120 x1 x4 x6@((Curry.Module.Prelude.:<) x21 x22) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_160(x4)(x22)(x21)(x1)(st))(st)
c__case_161_case__120 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_161_case__120(x1)(x4)(x)(st))(i)(xs)(st)
c__case_161_case__120 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_161_case__120")(x)



c__case_162_case__121 x1 x4 x6 x20@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_161(x4)(x6)(x1)(st))(st)
c__case_162_case__121 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_162_case__121(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_162_case__121 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_162_case__121")(x)



c__case_163_case__122 x1 x4 x6 x20 x19 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x19)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_162(x4)(x6)(x20)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_164_case__123 x1 x4 x6 x18@((Curry.Module.Prelude.:<) x19 x20) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_163(x4)(x6)(x20)(x19)(x1)(st))(st)
c__case_164_case__123 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_164_case__123(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_164_case__123 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_164_case__123")(x)



c__case_165_case__124 x1 x4 x6 x18 x17 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x17)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_164(x4)(x6)(x18)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_166_case__125 x1 x4 x6 x16@((Curry.Module.Prelude.:<) x17 x18) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_165(x4)(x6)(x18)(x17)(x1)(st))(st)
c__case_166_case__125 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_166_case__125(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_166_case__125 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_166_case__125")(x)



c__case_167_case__126 x1 x4 x6 x16 x15 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x15)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_166(x4)(x6)(x16)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_168_case__127 x1 x4 x6 x14@((Curry.Module.Prelude.:<) x15 x16) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_167(x4)(x6)(x16)(x15)(x1)(st))(st)
c__case_168_case__127 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_168_case__127(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_168_case__127 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_168_case__127")(x)



c__case_169_case__128 x1 x4 x6 x14 x13 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x13)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_168(x4)(x6)(x14)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_170_case__129 x1 x4 x6 x12@((Curry.Module.Prelude.:<) x13 x14) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_169(x4)(x6)(x14)(x13)(x1)(st))(st)
c__case_170_case__129 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_170_case__129(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_170_case__129 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_170_case__129")(x)



c__case_171_case__130 x1 x4 x6 x12 x11 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x11)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_170(x4)(x6)(x12)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_172_case__131 x1 x4 x6 x10@((Curry.Module.Prelude.:<) x11 x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_171(x4)(x6)(x12)(x11)(x1)(st))(st)
c__case_172_case__131 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_172_case__131(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_172_case__131 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_172_case__131")(x)



c__case_173_case__132 x1 x4 x6 x10 x9 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x9)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_172(x4)(x6)(x10)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_174_case__133 x1 x4 x6 x8@((Curry.Module.Prelude.:<) x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_173(x4)(x6)(x10)(x9)(x1)(st))(st)
c__case_174_case__133 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_174_case__133(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_174_case__133 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_174_case__133")(x)



c__case_175_case__134 x1 x4 x6 x8 x7 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x7)(Curry.Module.Prelude.C_Char('P'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_174(x4)(x6)(x8)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_176_case__135 x1 x4 x6 x5@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_175(x4)(x6)(x8)(x7)(x1)(st))(st)
c__case_176_case__135 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_176_case__135(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_176_case__135 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_176_case__135")(x)



c__case_177_case__136 x1 x4 x3@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_176(x4)(x6)(x5)(x1)(st))(st)
c__case_177_case__136 x1 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_177_case__136(x1)(x4)(x)(st))(i)(xs)(st)
c__case_177_case__136 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_177_case__136")(x)



c__case_178_case__137 x1 x2@(Curry.Module.AbstractCurry.C_CPComb x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_177(x4)(x3)(x1)(st))(st)
c__case_178_case__137 x1 x2@(Curry.Module.AbstractCurry.C_CPVar x29) st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.OracleAbstractCurryPrinter.c_showPattern(Curry.Module.AbstractCurry.C_CPVar(x29))(x1)(st))(Curry.Module.Prelude.List))(st)
c__case_178_case__137 x1 x2@(Curry.Module.AbstractCurry.C_CPAs x30 x31) st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.OracleAbstractCurryPrinter.c_showPattern(Curry.Module.AbstractCurry.C_CPAs(x30)(x31))(x1)(st))(Curry.Module.Prelude.List))(st)
c__case_178_case__137 x1 (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_178_case__137(x1)(x)(st))(i)(xs)(st)
c__case_178_case__137 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_178_case__137")(x)



c__case_129_case__138 x1 x25 x26@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_isClosedPatternList(x25)(x1)(st))(st)
c__case_129_case__138 x1 x25 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_129_case__138(x1)(x25)(x)(st))(i)(xs)(st)
c__case_129_case__138 x1 x25 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_129_case__138")(x)



c__case_130_case__139 x1 x24@((Curry.Module.Prelude.:<) x25 x26) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_129(x25)(x26)(x1)(st))(st)
c__case_130_case__139 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_130_case__139(x1)(x)(st))(i)(xs)(st)
c__case_130_case__139 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_130_case__139")(x)



c__case_131_case__140 x1 x4@((Curry.Module.Prelude.:<) x23 x24) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_130(x24)(x1)(st))(st)
c__case_131_case__140 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_131_case__140(x1)(x)(st))(i)(xs)(st)
c__case_131_case__140 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_131_case__140")(x)



c__case_132_case__141 x1 x4 x22@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_131(x4)(x1)(st))(st)
c__case_132_case__141 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_132_case__141(x1)(x4)(x)(st))(i)(xs)(st)
c__case_132_case__141 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_132_case__141")(x)



c__case_125_case__142 x1 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_125_case__142 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_125_case__142(x1)(x)(st))(i)(xs)(st)
c__case_125_case__142 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_125_case__142")(x)



c__case_126_case__143 x1 x4 x28@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_125(x4)(x1)(st))(st)
c__case_126_case__143 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_126_case__143(x1)(x4)(x)(st))(i)(xs)(st)
c__case_126_case__143 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_126_case__143")(x)



c__case_127_case__144 x1 x4 x28 x27 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x27)(Curry.Module.Prelude.C_Char(']'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_126(x4)(x28)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_128_case__145 x1 x4 x22@((Curry.Module.Prelude.:<) x27 x28) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_127(x4)(x28)(x27)(x1)(st))(st)
c__case_128_case__145 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_128_case__145(x1)(x4)(x)(st))(i)(xs)(st)
c__case_128_case__145 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_128_case__145")(x)



c__case_133_case__146 x1 x4 x22 x21 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x21)(Curry.Module.Prelude.C_Char(':'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_132(x4)(x22)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x21)(Curry.Module.Prelude.C_Char('['))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_128(x4)(x22)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st)



c__case_134_case__147 x1 x4 x6@((Curry.Module.Prelude.:<) x21 x22) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_133(x4)(x22)(x21)(x1)(st))(st)
c__case_134_case__147 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_134_case__147(x1)(x4)(x)(st))(i)(xs)(st)
c__case_134_case__147 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_134_case__147")(x)



c__case_135_case__148 x1 x4 x6 x20@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_134(x4)(x6)(x1)(st))(st)
c__case_135_case__148 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_135_case__148(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_135_case__148 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_135_case__148")(x)



c__case_136_case__149 x1 x4 x6 x20 x19 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x19)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_135(x4)(x6)(x20)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_137_case__150 x1 x4 x6 x18@((Curry.Module.Prelude.:<) x19 x20) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_136(x4)(x6)(x20)(x19)(x1)(st))(st)
c__case_137_case__150 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_137_case__150(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_137_case__150 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_137_case__150")(x)



c__case_138_case__151 x1 x4 x6 x18 x17 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x17)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_137(x4)(x6)(x18)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_139_case__152 x1 x4 x6 x16@((Curry.Module.Prelude.:<) x17 x18) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_138(x4)(x6)(x18)(x17)(x1)(st))(st)
c__case_139_case__152 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_139_case__152(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_139_case__152 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_139_case__152")(x)



c__case_140_case__153 x1 x4 x6 x16 x15 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x15)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_139(x4)(x6)(x16)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_141_case__154 x1 x4 x6 x14@((Curry.Module.Prelude.:<) x15 x16) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_140(x4)(x6)(x16)(x15)(x1)(st))(st)
c__case_141_case__154 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_141_case__154(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_141_case__154 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_141_case__154")(x)



c__case_142_case__155 x1 x4 x6 x14 x13 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x13)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_141(x4)(x6)(x14)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_143_case__156 x1 x4 x6 x12@((Curry.Module.Prelude.:<) x13 x14) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_142(x4)(x6)(x14)(x13)(x1)(st))(st)
c__case_143_case__156 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_143_case__156(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_143_case__156 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_143_case__156")(x)



c__case_144_case__157 x1 x4 x6 x12 x11 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x11)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_143(x4)(x6)(x12)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_145_case__158 x1 x4 x6 x10@((Curry.Module.Prelude.:<) x11 x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_144(x4)(x6)(x12)(x11)(x1)(st))(st)
c__case_145_case__158 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_145_case__158(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_145_case__158 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_145_case__158")(x)



c__case_146_case__159 x1 x4 x6 x10 x9 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x9)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_145(x4)(x6)(x10)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_147_case__160 x1 x4 x6 x8@((Curry.Module.Prelude.:<) x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_146(x4)(x6)(x10)(x9)(x1)(st))(st)
c__case_147_case__160 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_147_case__160(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_147_case__160 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_147_case__160")(x)



c__case_148_case__161 x1 x4 x6 x8 x7 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x7)(Curry.Module.Prelude.C_Char('P'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_147(x4)(x6)(x8)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_149_case__162 x1 x4 x6 x5@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_148(x4)(x6)(x8)(x7)(x1)(st))(st)
c__case_149_case__162 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_149_case__162(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_149_case__162 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_149_case__162")(x)



c__case_150_case__163 x1 x4 x3@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_149(x4)(x6)(x5)(x1)(st))(st)
c__case_150_case__163 x1 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_150_case__163(x1)(x4)(x)(st))(i)(xs)(st)
c__case_150_case__163 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_150_case__163")(x)



c__case_151_case__164 x1 x2@(Curry.Module.AbstractCurry.C_CPComb x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_150(x4)(x3)(x1)(st))(st)
c__case_151_case__164 x1 x2@(Curry.Module.AbstractCurry.C_CPVar x29) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_151_case__164 x1 x2@(Curry.Module.AbstractCurry.C_CPAs x30 x31) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_isClosedPatternList(x31)(x1)(st))(st)
c__case_151_case__164 x1 (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_151_case__164(x1)(x)(st))(i)(xs)(st)
c__case_151_case__164 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_151_case__164")(x)



c__case_102_case__165 x1 x23 x25 x26@Curry.Module.Prelude.List st = let {x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x27)((Curry.Module.Prelude.:<)(x28)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OracleAbstractCurryPrinter.c_isCharPattern(x23)(x1)(st))(Curry.Module.OracleAbstractCurryPrinter.c_isClosedStringPattern(x25)(x27)(st))(x28)(st))(st)
c__case_102_case__165 x1 x23 x25 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_102_case__165(x1)(x23)(x25)(x)(st))(i)(xs)(st)
c__case_102_case__165 x1 x23 x25 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_102_case__165")(x)



c__case_103_case__166 x1 x23 x24@((Curry.Module.Prelude.:<) x25 x26) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_102(x23)(x25)(x26)(x1)(st))(st)
c__case_103_case__166 x1 x23 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_103_case__166(x1)(x23)(x)(st))(i)(xs)(st)
c__case_103_case__166 x1 x23 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_103_case__166")(x)



c__case_104_case__167 x1 x4@((Curry.Module.Prelude.:<) x23 x24) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_103(x23)(x24)(x1)(st))(st)
c__case_104_case__167 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_104_case__167(x1)(x)(st))(i)(xs)(st)
c__case_104_case__167 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_104_case__167")(x)



c__case_105_case__168 x1 x4 x22@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_104(x4)(x1)(st))(st)
c__case_105_case__168 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_105_case__168(x1)(x4)(x)(st))(i)(xs)(st)
c__case_105_case__168 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_105_case__168")(x)



c__case_98_case__169 x1 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_98_case__169 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_98_case__169(x1)(x)(st))(i)(xs)(st)
c__case_98_case__169 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_98_case__169")(x)



c__case_99_case__170 x1 x4 x28@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_98(x4)(x1)(st))(st)
c__case_99_case__170 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_99_case__170(x1)(x4)(x)(st))(i)(xs)(st)
c__case_99_case__170 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_99_case__170")(x)



c__case_100_case__171 x1 x4 x28 x27 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x27)(Curry.Module.Prelude.C_Char(']'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_99(x4)(x28)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_101_case__172 x1 x4 x22@((Curry.Module.Prelude.:<) x27 x28) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_100(x4)(x28)(x27)(x1)(st))(st)
c__case_101_case__172 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_101_case__172(x1)(x4)(x)(st))(i)(xs)(st)
c__case_101_case__172 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_101_case__172")(x)



c__case_106_case__173 x1 x4 x22 x21 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x21)(Curry.Module.Prelude.C_Char(':'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_105(x4)(x22)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x21)(Curry.Module.Prelude.C_Char('['))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_101(x4)(x22)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st)



c__case_107_case__174 x1 x4 x6@((Curry.Module.Prelude.:<) x21 x22) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_106(x4)(x22)(x21)(x1)(st))(st)
c__case_107_case__174 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_107_case__174(x1)(x4)(x)(st))(i)(xs)(st)
c__case_107_case__174 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_107_case__174")(x)



c__case_108_case__175 x1 x4 x6 x20@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_107(x4)(x6)(x1)(st))(st)
c__case_108_case__175 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_108_case__175(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_108_case__175 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_108_case__175")(x)



c__case_109_case__176 x1 x4 x6 x20 x19 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x19)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_108(x4)(x6)(x20)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_110_case__177 x1 x4 x6 x18@((Curry.Module.Prelude.:<) x19 x20) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_109(x4)(x6)(x20)(x19)(x1)(st))(st)
c__case_110_case__177 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_110_case__177(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_110_case__177 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_110_case__177")(x)



c__case_111_case__178 x1 x4 x6 x18 x17 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x17)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_110(x4)(x6)(x18)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_112_case__179 x1 x4 x6 x16@((Curry.Module.Prelude.:<) x17 x18) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_111(x4)(x6)(x18)(x17)(x1)(st))(st)
c__case_112_case__179 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_112_case__179(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_112_case__179 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_112_case__179")(x)



c__case_113_case__180 x1 x4 x6 x16 x15 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x15)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_112(x4)(x6)(x16)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_114_case__181 x1 x4 x6 x14@((Curry.Module.Prelude.:<) x15 x16) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_113(x4)(x6)(x16)(x15)(x1)(st))(st)
c__case_114_case__181 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_114_case__181(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_114_case__181 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_114_case__181")(x)



c__case_115_case__182 x1 x4 x6 x14 x13 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x13)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_114(x4)(x6)(x14)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_116_case__183 x1 x4 x6 x12@((Curry.Module.Prelude.:<) x13 x14) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_115(x4)(x6)(x14)(x13)(x1)(st))(st)
c__case_116_case__183 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_116_case__183(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_116_case__183 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_116_case__183")(x)



c__case_117_case__184 x1 x4 x6 x12 x11 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x11)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_116(x4)(x6)(x12)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_118_case__185 x1 x4 x6 x10@((Curry.Module.Prelude.:<) x11 x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_117(x4)(x6)(x12)(x11)(x1)(st))(st)
c__case_118_case__185 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_118_case__185(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_118_case__185 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_118_case__185")(x)



c__case_119_case__186 x1 x4 x6 x10 x9 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x9)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_118(x4)(x6)(x10)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_120_case__187 x1 x4 x6 x8@((Curry.Module.Prelude.:<) x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_119(x4)(x6)(x10)(x9)(x1)(st))(st)
c__case_120_case__187 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_120_case__187(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_120_case__187 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_120_case__187")(x)



c__case_121_case__188 x1 x4 x6 x8 x7 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x7)(Curry.Module.Prelude.C_Char('P'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_120(x4)(x6)(x8)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_122_case__189 x1 x4 x6 x5@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_121(x4)(x6)(x8)(x7)(x1)(st))(st)
c__case_122_case__189 x1 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_122_case__189(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_122_case__189 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_122_case__189")(x)



c__case_123_case__190 x1 x4 x3@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_122(x4)(x6)(x5)(x1)(st))(st)
c__case_123_case__190 x1 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_123_case__190(x1)(x4)(x)(st))(i)(xs)(st)
c__case_123_case__190 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_123_case__190")(x)



c__case_124_case__191 x1 x2@(Curry.Module.AbstractCurry.C_CPComb x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_123(x4)(x3)(x1)(st))(st)
c__case_124_case__191 x1 x2@(Curry.Module.AbstractCurry.C_CPVar x29) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_124_case__191 x1 (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_124_case__191(x1)(x)(st))(i)(xs)(st)
c__case_124_case__191 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_124_case__191")(x)



c__case_96_case__192 x1 x3@(Curry.Module.AbstractCurry.C_CCharc x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_96_case__192 x1 x3@(Curry.Module.AbstractCurry.C_CIntc x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_96_case__192 x1 x3@(Curry.Module.AbstractCurry.C_CFloatc x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_96_case__192 x1 (Curry.Module.AbstractCurry.C_CLiteralOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_96_case__192(x1)(x)(st))(i)(xs)(st)
c__case_96_case__192 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_96_case__192")(x)



c__case_97_case__193 x1 x2@(Curry.Module.AbstractCurry.C_CPLit x3) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_96(x3)(x1)(st))(st)
c__case_97_case__193 x1 x2@(Curry.Module.AbstractCurry.C_CPVar x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_97_case__193 x1 x2@(Curry.Module.AbstractCurry.C_CPComb x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_97_case__193 x1 x2@(Curry.Module.AbstractCurry.C_CPAs x10 x11) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_97_case__193 x1 x2@(Curry.Module.AbstractCurry.C_CPFuncComb x12 x13) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_97_case__193 x1 (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_97_case__193(x1)(x)(st))(i)(xs)(st)
c__case_97_case__193 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_97_case__193")(x)



c__case_95_case__194 x1 x2@(Curry.Module.AbstractCurry.C_CPAs x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_95_case__194 x1 x2@(Curry.Module.AbstractCurry.C_CPVar x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_95_case__194 x1 x2@(Curry.Module.AbstractCurry.C_CPLit x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_95_case__194 x1 x2@(Curry.Module.AbstractCurry.C_CPComb x7 x8) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_95_case__194 x1 x2@(Curry.Module.AbstractCurry.C_CPFuncComb x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_95_case__194 x1 (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_95_case__194(x1)(x)(st))(i)(xs)(st)
c__case_95_case__194 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_95_case__194")(x)



c__case_93_case__195 x1 x4 x3@(Curry.Module.Prelude.T2 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_43_43(x6)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('@'))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_concat(Curry.Module.OracleList.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showPatListElems(x4)(x1)(st))(x7)(st))(x8)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x9)(st))(x10)(st))(x11)(st))(x12)(st))(st)
c__case_93_case__195 x1 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_93_case__195(x1)(x4)(x)(st))(i)(xs)(st)
c__case_93_case__195 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_93_case__195")(x)



c__case_94_case__196 x1 x2@(Curry.Module.AbstractCurry.C_CPAs x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_93(x4)(x3)(x1)(st))(st)
c__case_94_case__196 x1 (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_94_case__196(x1)(x)(st))(i)(xs)(st)
c__case_94_case__196 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_94_case__196")(x)



c__case_92_case__197 x1 x2 x3@(Curry.Module.AbstractCurry.C_CBranch x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showPattern(x4)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.OracleAbstractCurryPrinter.c_showExprOpt(x2)(x5)(x6)(st))(x7)(st))(x8)(st))(st)
c__case_92_case__197 x1 x2 (Curry.Module.AbstractCurry.C_CBranchExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_92_case__197(x1)(x2)(x)(st))(i)(xs)(st)
c__case_92_case__197 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_92_case__197")(x)



c__case_91_case__198 x1 x2@(Curry.Module.AbstractCurry.C_CIntc x3) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_show(x3)(x1)(st))(st)
c__case_91_case__198 x1 x2@(Curry.Module.AbstractCurry.C_CFloatc x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_show(x4)(x1)(st))(st)
c__case_91_case__198 x1 x2@(Curry.Module.AbstractCurry.C_CCharc x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showCCharc(Curry.Module.AbstractCurry.C_CCharc(x5))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))(Curry.Module.Prelude.List))(x6)(st))(x7)(st))(st)
c__case_91_case__198 x1 (Curry.Module.AbstractCurry.C_CLiteralOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_91_case__198(x1)(x)(st))(i)(xs)(st)
c__case_91_case__198 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_91_case__198")(x)



c__case_85_case__199 x1 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(st)
c__case_85_case__199 x1 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_85_case__199 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_85_case__199(x1)(x3)(x)(st))(i)(xs)(st)
c__case_85_case__199 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_85_case__199")(x)



c__case_86_case__200 x1 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List)))(st)
c__case_86_case__200 x1 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_85(x3)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x5)(st))(st)
c__case_86_case__200 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_86_case__200(x1)(x3)(x)(st))(i)(xs)(st)
c__case_86_case__200 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_86_case__200")(x)



c__case_87_case__201 x1 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))(Curry.Module.Prelude.List)))(st)
c__case_87_case__201 x1 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_86(x3)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('\"'))(x1)(st))(x5)(st))(st)
c__case_87_case__201 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_87_case__201(x1)(x3)(x)(st))(i)(xs)(st)
c__case_87_case__201 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_87_case__201")(x)



c__case_88_case__202 x1 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List)))(st)
c__case_88_case__202 x1 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_87(x3)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('\\'))(x1)(st))(x5)(st))(st)
c__case_88_case__202 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_88_case__202(x1)(x3)(x)(st))(i)(xs)(st)
c__case_88_case__202 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_88_case__202")(x)



c__case_89_case__203 x1 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List)))(st)
c__case_89_case__203 x1 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_88(x3)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('\r'))(x1)(st))(x5)(st))(st)
c__case_89_case__203 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_89_case__203(x1)(x3)(x)(st))(i)(xs)(st)
c__case_89_case__203 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_89_case__203")(x)



c__case_90_case__204 x1 x2@(Curry.Module.AbstractCurry.C_CCharc x3) st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_89(x3)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('\n'))(x1)(st))(x4)(st))(st)
c__case_90_case__204 x1 (Curry.Module.AbstractCurry.C_CLiteralOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_90_case__204(x1)(x)(st))(i)(xs)(st)
c__case_90_case__204 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_90_case__204")(x)



c__case_82_case__205 x1 x3 x5 x6 x7@Curry.Module.Prelude.C_True st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x3)(Curry.Module.OracleAbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_True)))))((Curry.Module.Prelude.:<)(x5)(x6))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(x1)(st))(x8)(st))(st)
c__case_82_case__205 x1 x3 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_82_case__205 x1 x3 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_82_case__205(x1)(x3)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_82_case__205 x1 x3 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_82_case__205")(x)



c__case_83_case__206 x1 x3 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showPreludeTypeCons(x3)((Curry.Module.Prelude.:<)(x5)(x6))(x1)(st))(st)
c__case_83_case__206 x1 x3 x5 x6 x7@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_82(x3)(x5)(x6)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x8)(st))(st)
c__case_83_case__206 x1 x3 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_83_case__206(x1)(x3)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_83_case__206 x1 x3 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_83_case__206")(x)



c__case_84_case__207 x1 x2 x3 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_84_case__207 x1 x2 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_83(x2)(x3)(x5)(x6)(Curry.Module.OraclePrelude.op_61_61(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(x1)(st))(x7)(st))(st)
c__case_84_case__207 x1 x2 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_84_case__207(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_84_case__207 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_84_case__207")(x)



c__case_78_case__208 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x2)(Curry.Module.OracleAbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_True)))))(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(x1)(st))(x5)(st))(st)
c__case_78_case__208 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_78_case__208 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_78_case__208(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_78_case__208 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_78_case__208")(x)



c__case_79_case__209 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_False)))))(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x5)(st))(x6)(st))(st)
c__case_79_case__209 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_78(x2)(x3)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x7)(st))(st)
c__case_79_case__209 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_79_case__209(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_79_case__209 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_79_case__209")(x)



c__case_80_case__210 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_False)(Curry.Module.OraclePrelude.c_head(x3)(x1)(st))(x5)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))(x6)(st))(x7)(st))(st)
c__case_80_case__210 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_79(x2)(x3)(Curry.Module.OracleAbstractCurryPrinter.c_isTuple(x2)(x1)(st))(x8)(st))(st)
c__case_80_case__210 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_80_case__210(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_80_case__210 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_80_case__210")(x)



c__case_81_case__211 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))(Curry.Module.Prelude.List)))))))(st)
c__case_81_case__211 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_80(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List)))(x1)(st))(x5)(st))(st)
c__case_81_case__211 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_81_case__211(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_81_case__211 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_81_case__211")(x)



c__case_77_case__212 x1 x2 x3 x18@(Curry.Module.AbstractCurry.C_CSymbol x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showSymbolApplication(x2)(x4)(x3)(x1)(st))(st)
c__case_77_case__212 x1 x2 x3 x18@(Curry.Module.AbstractCurry.C_CVar x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showSimpleApplication(x2)(x3)(x1)(st))(st)
c__case_77_case__212 x1 x2 x3 x18@(Curry.Module.AbstractCurry.C_CLit x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showSimpleApplication(x2)(x3)(x1)(st))(st)
c__case_77_case__212 x1 x2 x3 x18@(Curry.Module.AbstractCurry.C_CApply x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showSimpleApplication(x2)(x3)(x1)(st))(st)
c__case_77_case__212 x1 x2 x3 x18@(Curry.Module.AbstractCurry.C_CLambda x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showSimpleApplication(x2)(x3)(x1)(st))(st)
c__case_77_case__212 x1 x2 x3 x18@(Curry.Module.AbstractCurry.C_CLetDecl x11 x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showSimpleApplication(x2)(x3)(x1)(st))(st)
c__case_77_case__212 x1 x2 x3 x18@(Curry.Module.AbstractCurry.C_CDoExpr x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showSimpleApplication(x2)(x3)(x1)(st))(st)
c__case_77_case__212 x1 x2 x3 x18@(Curry.Module.AbstractCurry.C_CListComp x14 x15) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showSimpleApplication(x2)(x3)(x1)(st))(st)
c__case_77_case__212 x1 x2 x3 x18@(Curry.Module.AbstractCurry.C_CCase x16 x17) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showSimpleApplication(x2)(x3)(x1)(st))(st)
c__case_77_case__212 x1 x2 x3 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_77_case__212(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_77_case__212 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_77_case__212")(x)



c__case_76_case__213 x1 x2@(Curry.Module.AbstractCurry.C_CApply x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_applicationHead(x3)(x1)(st))(st)
c__case_76_case__213 x1 x2@(Curry.Module.AbstractCurry.C_CVar x5) st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_76_case__213 x1 x2@(Curry.Module.AbstractCurry.C_CLit x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_76_case__213 x1 x2@(Curry.Module.AbstractCurry.C_CSymbol x7) st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_76_case__213 x1 x2@(Curry.Module.AbstractCurry.C_CLambda x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_76_case__213 x1 x2@(Curry.Module.AbstractCurry.C_CLetDecl x10 x11) st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_76_case__213 x1 x2@(Curry.Module.AbstractCurry.C_CDoExpr x12) st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_76_case__213 x1 x2@(Curry.Module.AbstractCurry.C_CListComp x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_76_case__213 x1 x2@(Curry.Module.AbstractCurry.C_CCase x15 x16) st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_76_case__213 x1 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_76_case__213(x1)(x)(st))(i)(xs)(st)
c__case_76_case__213 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_76_case__213")(x)



c__case_71_case__214 x1 x2 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showSimpleApplication(x2)(x4)(x1)(st))(st)
c__case_71_case__214 x1 x2 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_71_case__214 x1 x2 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_71_case__214(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_71_case__214 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_71_case__214")(x)



c__case_72_case__215 x1 x2 x4 x7@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showTupleApplication(x2)(x4)(x1)(st))(st)
c__case_72_case__215 x1 x2 x4 x7@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_71(x2)(x4)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x8)(st))(st)
c__case_72_case__215 x1 x2 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_72_case__215(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_72_case__215 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_72_case__215")(x)



c__case_73_case__216 x1 x2 x4 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showITEApplication(x2)(x4)(x1)(st))(st)
c__case_73_case__216 x1 x2 x4 x6 x7@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_72(x2)(x4)(x6)(Curry.Module.OracleAbstractCurryPrinter.c_isTuple(x6)(x1)(st))(x8)(st))(st)
c__case_73_case__216 x1 x2 x4 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_73_case__216(x1)(x2)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_73_case__216 x1 x2 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_73_case__216")(x)



c__case_74_case__217 x1 x2 x4 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showListApplication(x2)(x4)(x1)(st))(st)
c__case_74_case__217 x1 x2 x4 x5 x6 x7@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))))(Curry.Module.OracleAbstractCurryPrinter.c__case_73(x2)(x4)(x5)(x6)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x6)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))))))))(x8)(st))(x9)(st))(x10)(st))(st)
c__case_74_case__217 x1 x2 x4 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_74_case__217(x1)(x2)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_74_case__217 x1 x2 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_74_case__217")(x)



c__case_75_case__218 x1 x2 x4 x3@(Curry.Module.Prelude.T2 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))))(Curry.Module.OracleAbstractCurryPrinter.c__case_74(x2)(x4)(x5)(x6)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x6)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(x7)(st))(x8)(st))(x9)(st))(st)
c__case_75_case__218 x1 x2 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_75_case__218(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_75_case__218 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_75_case__218")(x)



c__case_68_case__219 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showSimpleListApplication(x2)(x3)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x5)(st))(x6)(st))(st)
c__case_68_case__219 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_68_case__219 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_68_case__219(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_68_case__219 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_68_case__219")(x)



c__case_69_case__220 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showConsListApplication(x2)(x3)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))(x5)(st))(x6)(st))(st)
c__case_69_case__220 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_68(x2)(x3)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x7)(st))(st)
c__case_69_case__220 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_69_case__220(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_69_case__220 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_69_case__220")(x)



c__case_70_case__221 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showCharListApplication(x2)(x3)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List))(x5)(st))(x6)(st))(st)
c__case_70_case__221 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_69(x2)(x3)(Curry.Module.OracleAbstractCurryPrinter.c_isClosedList(x3)(x1)(st))(x7)(st))(st)
c__case_70_case__221 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_70_case__221(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_70_case__221 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_70_case__221")(x)



c__case_64_case__222 x1 x2 x8 x5@(Curry.Module.AbstractCurry.C_CSymbol x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showCCharc(x8)(x1)(st))(st)
c__case_64_case__222 x1 x2 x8 x5@(Curry.Module.AbstractCurry.C_CVar x10) st = let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showCCharc(x8)(x1)(st))(Curry.Module.OracleAbstractCurryPrinter.c_showCharListApplication(x2)(x5)(x23)(st))(x24)(st))(st)
c__case_64_case__222 x1 x2 x8 x5@(Curry.Module.AbstractCurry.C_CLit x11) st = let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showCCharc(x8)(x1)(st))(Curry.Module.OracleAbstractCurryPrinter.c_showCharListApplication(x2)(x5)(x25)(st))(x26)(st))(st)
c__case_64_case__222 x1 x2 x8 x5@(Curry.Module.AbstractCurry.C_CApply x12 x13) st = let {x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x27)((Curry.Module.Prelude.:<)(x28)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showCCharc(x8)(x1)(st))(Curry.Module.OracleAbstractCurryPrinter.c_showCharListApplication(x2)(x5)(x27)(st))(x28)(st))(st)
c__case_64_case__222 x1 x2 x8 x5@(Curry.Module.AbstractCurry.C_CLambda x14 x15) st = let {x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x29)((Curry.Module.Prelude.:<)(x30)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showCCharc(x8)(x1)(st))(Curry.Module.OracleAbstractCurryPrinter.c_showCharListApplication(x2)(x5)(x29)(st))(x30)(st))(st)
c__case_64_case__222 x1 x2 x8 x5@(Curry.Module.AbstractCurry.C_CLetDecl x16 x17) st = let {x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x31)((Curry.Module.Prelude.:<)(x32)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showCCharc(x8)(x1)(st))(Curry.Module.OracleAbstractCurryPrinter.c_showCharListApplication(x2)(x5)(x31)(st))(x32)(st))(st)
c__case_64_case__222 x1 x2 x8 x5@(Curry.Module.AbstractCurry.C_CDoExpr x18) st = let {x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x33)((Curry.Module.Prelude.:<)(x34)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showCCharc(x8)(x1)(st))(Curry.Module.OracleAbstractCurryPrinter.c_showCharListApplication(x2)(x5)(x33)(st))(x34)(st))(st)
c__case_64_case__222 x1 x2 x8 x5@(Curry.Module.AbstractCurry.C_CListComp x19 x20) st = let {x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x35)((Curry.Module.Prelude.:<)(x36)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showCCharc(x8)(x1)(st))(Curry.Module.OracleAbstractCurryPrinter.c_showCharListApplication(x2)(x5)(x35)(st))(x36)(st))(st)
c__case_64_case__222 x1 x2 x8 x5@(Curry.Module.AbstractCurry.C_CCase x21 x22) st = let {x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x37)((Curry.Module.Prelude.:<)(x38)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showCCharc(x8)(x1)(st))(Curry.Module.OracleAbstractCurryPrinter.c_showCharListApplication(x2)(x5)(x37)(st))(x38)(st))(st)
c__case_64_case__222 x1 x2 x8 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_64_case__222(x1)(x2)(x8)(x)(st))(i)(xs)(st)
c__case_64_case__222 x1 x2 x8 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_64_case__222")(x)



c__case_65_case__223 x1 x2 x5 x7@(Curry.Module.AbstractCurry.C_CLit x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_64(x2)(x8)(x5)(x1)(st))(st)
c__case_65_case__223 x1 x2 x5 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_65_case__223(x1)(x2)(x5)(x)(st))(i)(xs)(st)
c__case_65_case__223 x1 x2 x5 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_65_case__223")(x)



c__case_66_case__224 x1 x2 x5 x4@(Curry.Module.AbstractCurry.C_CApply x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_65(x2)(x5)(x7)(x1)(st))(st)
c__case_66_case__224 x1 x2 x5 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_66_case__224(x1)(x2)(x5)(x)(st))(i)(xs)(st)
c__case_66_case__224 x1 x2 x5 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_66_case__224")(x)



c__case_67_case__225 x1 x2 x3@(Curry.Module.AbstractCurry.C_CApply x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_66(x2)(x5)(x4)(x1)(st))(st)
c__case_67_case__225 x1 x2 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_67_case__225(x1)(x2)(x)(st))(i)(xs)(st)
c__case_67_case__225 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_67_case__225")(x)



c__case_61_case__226 x1 x2 x7 x5@(Curry.Module.AbstractCurry.C_CSymbol x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x7)(x1)(st))(st)
c__case_61_case__226 x1 x2 x7 x5@(Curry.Module.AbstractCurry.C_CVar x9) st = let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x7)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showConsListApplication(x2)(x5)(x22)(st))(x23)(st))(x24)(st))(st)
c__case_61_case__226 x1 x2 x7 x5@(Curry.Module.AbstractCurry.C_CLit x10) st = let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x7)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showConsListApplication(x2)(x5)(x25)(st))(x26)(st))(x27)(st))(st)
c__case_61_case__226 x1 x2 x7 x5@(Curry.Module.AbstractCurry.C_CApply x11 x12) st = let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)((Curry.Module.Prelude.:<)(x30)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x7)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showConsListApplication(x2)(x5)(x28)(st))(x29)(st))(x30)(st))(st)
c__case_61_case__226 x1 x2 x7 x5@(Curry.Module.AbstractCurry.C_CLambda x13 x14) st = let {x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x31)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x7)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showConsListApplication(x2)(x5)(x31)(st))(x32)(st))(x33)(st))(st)
c__case_61_case__226 x1 x2 x7 x5@(Curry.Module.AbstractCurry.C_CLetDecl x15 x16) st = let {x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x34)((Curry.Module.Prelude.:<)(x35)((Curry.Module.Prelude.:<)(x36)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x7)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showConsListApplication(x2)(x5)(x34)(st))(x35)(st))(x36)(st))(st)
c__case_61_case__226 x1 x2 x7 x5@(Curry.Module.AbstractCurry.C_CDoExpr x17) st = let {x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x39 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x37)((Curry.Module.Prelude.:<)(x38)((Curry.Module.Prelude.:<)(x39)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x7)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showConsListApplication(x2)(x5)(x37)(st))(x38)(st))(x39)(st))(st)
c__case_61_case__226 x1 x2 x7 x5@(Curry.Module.AbstractCurry.C_CListComp x18 x19) st = let {x40 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x41 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x42 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x40)((Curry.Module.Prelude.:<)(x41)((Curry.Module.Prelude.:<)(x42)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x7)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showConsListApplication(x2)(x5)(x40)(st))(x41)(st))(x42)(st))(st)
c__case_61_case__226 x1 x2 x7 x5@(Curry.Module.AbstractCurry.C_CCase x20 x21) st = let {x43 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x44 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x45 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x43)((Curry.Module.Prelude.:<)(x44)((Curry.Module.Prelude.:<)(x45)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x7)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showConsListApplication(x2)(x5)(x43)(st))(x44)(st))(x45)(st))(st)
c__case_61_case__226 x1 x2 x7 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_61_case__226(x1)(x2)(x7)(x)(st))(i)(xs)(st)
c__case_61_case__226 x1 x2 x7 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_61_case__226")(x)



c__case_62_case__227 x1 x2 x5 x4@(Curry.Module.AbstractCurry.C_CApply x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_61(x2)(x7)(x5)(x1)(st))(st)
c__case_62_case__227 x1 x2 x5 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_62_case__227(x1)(x2)(x5)(x)(st))(i)(xs)(st)
c__case_62_case__227 x1 x2 x5 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_62_case__227")(x)



c__case_63_case__228 x1 x2 x3@(Curry.Module.AbstractCurry.C_CApply x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_62(x2)(x5)(x4)(x1)(st))(st)
c__case_63_case__228 x1 x2 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_63_case__228(x1)(x2)(x)(st))(i)(xs)(st)
c__case_63_case__228 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_63_case__228")(x)



c__case_58_case__229 x1 x2 x7 x5@(Curry.Module.AbstractCurry.C_CSymbol x8) st = let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x7)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))))(x22)(st))(st)
c__case_58_case__229 x1 x2 x7 x5@(Curry.Module.AbstractCurry.C_CVar x9) st = let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x7)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x5)(x23)(st))(x24)(st))(x25)(st))(st)
c__case_58_case__229 x1 x2 x7 x5@(Curry.Module.AbstractCurry.C_CLit x10) st = let {x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)((Curry.Module.Prelude.:<)(x28)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x7)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x5)(x26)(st))(x27)(st))(x28)(st))(st)
c__case_58_case__229 x1 x2 x7 x5@(Curry.Module.AbstractCurry.C_CApply x11 x12) st = let {x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x29)((Curry.Module.Prelude.:<)(x30)((Curry.Module.Prelude.:<)(x31)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x7)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x5)(x29)(st))(x30)(st))(x31)(st))(st)
c__case_58_case__229 x1 x2 x7 x5@(Curry.Module.AbstractCurry.C_CLambda x13 x14) st = let {x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)((Curry.Module.Prelude.:<)(x34)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x7)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x5)(x32)(st))(x33)(st))(x34)(st))(st)
c__case_58_case__229 x1 x2 x7 x5@(Curry.Module.AbstractCurry.C_CLetDecl x15 x16) st = let {x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x35)((Curry.Module.Prelude.:<)(x36)((Curry.Module.Prelude.:<)(x37)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x7)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x5)(x35)(st))(x36)(st))(x37)(st))(st)
c__case_58_case__229 x1 x2 x7 x5@(Curry.Module.AbstractCurry.C_CDoExpr x17) st = let {x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x39 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x40 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x38)((Curry.Module.Prelude.:<)(x39)((Curry.Module.Prelude.:<)(x40)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x7)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x5)(x38)(st))(x39)(st))(x40)(st))(st)
c__case_58_case__229 x1 x2 x7 x5@(Curry.Module.AbstractCurry.C_CListComp x18 x19) st = let {x41 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x42 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x43 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x41)((Curry.Module.Prelude.:<)(x42)((Curry.Module.Prelude.:<)(x43)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x7)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x5)(x41)(st))(x42)(st))(x43)(st))(st)
c__case_58_case__229 x1 x2 x7 x5@(Curry.Module.AbstractCurry.C_CCase x20 x21) st = let {x44 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x45 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x46 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x44)((Curry.Module.Prelude.:<)(x45)((Curry.Module.Prelude.:<)(x46)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x7)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x5)(x44)(st))(x45)(st))(x46)(st))(st)
c__case_58_case__229 x1 x2 x7 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_58_case__229(x1)(x2)(x7)(x)(st))(i)(xs)(st)
c__case_58_case__229 x1 x2 x7 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_58_case__229")(x)



c__case_57_case__230 x1 x2 x5 x22@(Curry.Module.Prelude.T2 x23 x24) st = let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x5)(x1)(st))(x24)(x25)(st))(st)
c__case_57_case__230 x1 x2 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_57_case__230(x1)(x2)(x5)(x)(st))(i)(xs)(st)
c__case_57_case__230 x1 x2 x5 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_57_case__230")(x)



c__case_59_case__231 x1 x2 x5 x4@(Curry.Module.AbstractCurry.C_CApply x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_58(x2)(x7)(x5)(x1)(st))(st)
c__case_59_case__231 x1 x2 x5 x4@(Curry.Module.AbstractCurry.C_CSymbol x22) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_57(x2)(x5)(x22)(x1)(st))(st)
c__case_59_case__231 x1 x2 x5 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_59_case__231(x1)(x2)(x5)(x)(st))(i)(xs)(st)
c__case_59_case__231 x1 x2 x5 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_59_case__231")(x)



c__case_60_case__232 x1 x2 x3@(Curry.Module.AbstractCurry.C_CApply x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_59(x2)(x5)(x4)(x1)(st))(st)
c__case_60_case__232 x1 x2 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_60_case__232(x1)(x2)(x)(st))(i)(xs)(st)
c__case_60_case__232 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_60_case__232")(x)



c__case_55_case__233 x1 x2 x3 x6 x5@(Curry.Module.AbstractCurry.C_CApply x7 x8) st = let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x8)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showSymbol(x2)(x3)(x21)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x6)(x22)(st))(x23)(st))(x24)(st))(x25)(st))(x26)(st))(st)
c__case_55_case__233 x1 x2 x3 x6 x5@(Curry.Module.AbstractCurry.C_CVar x9) st = let {x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x27)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)((Curry.Module.Prelude.:<)(x30)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showSymbol(x2)(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x6)(x27)(st))(x28)(st))(x29)(st))(x30)(st))(st)
c__case_55_case__233 x1 x2 x3 x6 x5@(Curry.Module.AbstractCurry.C_CLit x10) st = let {x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x31)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)((Curry.Module.Prelude.:<)(x34)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showSymbol(x2)(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x6)(x31)(st))(x32)(st))(x33)(st))(x34)(st))(st)
c__case_55_case__233 x1 x2 x3 x6 x5@(Curry.Module.AbstractCurry.C_CSymbol x11) st = let {x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x35)((Curry.Module.Prelude.:<)(x36)((Curry.Module.Prelude.:<)(x37)((Curry.Module.Prelude.:<)(x38)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showSymbol(x2)(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x6)(x35)(st))(x36)(st))(x37)(st))(x38)(st))(st)
c__case_55_case__233 x1 x2 x3 x6 x5@(Curry.Module.AbstractCurry.C_CLambda x12 x13) st = let {x39 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x40 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x41 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x42 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x39)((Curry.Module.Prelude.:<)(x40)((Curry.Module.Prelude.:<)(x41)((Curry.Module.Prelude.:<)(x42)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showSymbol(x2)(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x6)(x39)(st))(x40)(st))(x41)(st))(x42)(st))(st)
c__case_55_case__233 x1 x2 x3 x6 x5@(Curry.Module.AbstractCurry.C_CLetDecl x14 x15) st = let {x43 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x44 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x45 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x46 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x43)((Curry.Module.Prelude.:<)(x44)((Curry.Module.Prelude.:<)(x45)((Curry.Module.Prelude.:<)(x46)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showSymbol(x2)(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x6)(x43)(st))(x44)(st))(x45)(st))(x46)(st))(st)
c__case_55_case__233 x1 x2 x3 x6 x5@(Curry.Module.AbstractCurry.C_CDoExpr x16) st = let {x47 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x48 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x49 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x50 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x47)((Curry.Module.Prelude.:<)(x48)((Curry.Module.Prelude.:<)(x49)((Curry.Module.Prelude.:<)(x50)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showSymbol(x2)(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x6)(x47)(st))(x48)(st))(x49)(st))(x50)(st))(st)
c__case_55_case__233 x1 x2 x3 x6 x5@(Curry.Module.AbstractCurry.C_CListComp x17 x18) st = let {x51 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x52 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x53 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x54 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x51)((Curry.Module.Prelude.:<)(x52)((Curry.Module.Prelude.:<)(x53)((Curry.Module.Prelude.:<)(x54)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showSymbol(x2)(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x6)(x51)(st))(x52)(st))(x53)(st))(x54)(st))(st)
c__case_55_case__233 x1 x2 x3 x6 x5@(Curry.Module.AbstractCurry.C_CCase x19 x20) st = let {x55 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x56 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x57 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x58 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x55)((Curry.Module.Prelude.:<)(x56)((Curry.Module.Prelude.:<)(x57)((Curry.Module.Prelude.:<)(x58)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showSymbol(x2)(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x6)(x55)(st))(x56)(st))(x57)(st))(x58)(st))(st)
c__case_55_case__233 x1 x2 x3 x6 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_55_case__233(x1)(x2)(x3)(x6)(x)(st))(i)(xs)(st)
c__case_55_case__233 x1 x2 x3 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_55_case__233")(x)



c__case_56_case__234 x1 x2 x3 x4@(Curry.Module.AbstractCurry.C_CApply x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_55(x2)(x3)(x6)(x5)(x1)(st))(st)
c__case_56_case__234 x1 x2 x3 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_56_case__234(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_56_case__234 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_56_case__234")(x)



c__case_51_case__235 x1 x2 x4 x5 x7 x9 x8@(Curry.Module.AbstractCurry.C_CSymbol x10) st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showExprOpt(x2)(x9)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showExprOpt(x2)(x7)(x13)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))(Curry.Module.OracleAbstractCurryPrinter.c_showExprOpt(x2)(x5)(x14)(st))(x15)(st))(x16)(st))(x17)(st))(x18)(st))(x19)(st))(st)
c__case_51_case__235 x1 x2 x4 x5 x7 x9 x8@(Curry.Module.AbstractCurry.C_CApply x11 x12) st = let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showITEApplication(x2)(x4)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x5)(x20)(st))(x21)(st))(x22)(st))(x23)(st))(st)
c__case_51_case__235 x1 x2 x4 x5 x7 x9 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_51_case__235(x1)(x2)(x4)(x5)(x7)(x9)(x)(st))(i)(xs)(st)
c__case_51_case__235 x1 x2 x4 x5 x7 x9 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_51_case__235")(x)



c__case_52_case__236 x1 x2 x4 x5 x7 x6@(Curry.Module.AbstractCurry.C_CApply x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_51(x2)(x4)(x5)(x7)(x9)(x8)(x1)(st))(st)
c__case_52_case__236 x1 x2 x4 x5 x7 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_52_case__236(x1)(x2)(x4)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_52_case__236 x1 x2 x4 x5 x7 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_52_case__236")(x)



c__case_53_case__237 x1 x2 x5 x4@(Curry.Module.AbstractCurry.C_CApply x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_52(x2)(x4)(x5)(x7)(x6)(x1)(st))(st)
c__case_53_case__237 x1 x2 x5 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_53_case__237(x1)(x2)(x5)(x)(st))(i)(xs)(st)
c__case_53_case__237 x1 x2 x5 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_53_case__237")(x)



c__case_54_case__238 x1 x2 x3@(Curry.Module.AbstractCurry.C_CApply x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_53(x2)(x5)(x4)(x1)(st))(st)
c__case_54_case__238 x1 x2 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_54_case__238(x1)(x2)(x)(st))(i)(xs)(st)
c__case_54_case__238 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_54_case__238")(x)



c__case_49_case__239 x1 x2 x5 x4@(Curry.Module.AbstractCurry.C_CSymbol x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showExprOpt(x2)(x5)(x1)(st))(st)
c__case_49_case__239 x1 x2 x5 x4@(Curry.Module.AbstractCurry.C_CApply x7 x8) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showTupleApplication'46p_showTuple'46386(x2)(Curry.Module.AbstractCurry.C_CApply(x7)(x8))(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showExprOpt(x2)(x5)(x9)(st))(x10)(st))(x11)(st))(st)
c__case_49_case__239 x1 x2 x5 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_49_case__239(x1)(x2)(x5)(x)(st))(i)(xs)(st)
c__case_49_case__239 x1 x2 x5 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_49_case__239")(x)



c__case_50_case__240 x1 x2 x3@(Curry.Module.AbstractCurry.C_CApply x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_49(x2)(x5)(x4)(x1)(st))(st)
c__case_50_case__240 x1 x2 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_50_case__240(x1)(x2)(x)(st))(i)(xs)(st)
c__case_50_case__240 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_50_case__240")(x)



c__case_48_case__241 x1 x2 x3@(Curry.Module.AbstractCurry.C_CApply x4 x5) st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showSimpleApplication(x2)(x4)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x5)(x18)(st))(x19)(st))(x20)(st))(st)
c__case_48_case__241 x1 x2 x3@(Curry.Module.AbstractCurry.C_CVar x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x3)(x1)(st))(st)
c__case_48_case__241 x1 x2 x3@(Curry.Module.AbstractCurry.C_CLit x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x3)(x1)(st))(st)
c__case_48_case__241 x1 x2 x3@(Curry.Module.AbstractCurry.C_CSymbol x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x3)(x1)(st))(st)
c__case_48_case__241 x1 x2 x3@(Curry.Module.AbstractCurry.C_CLambda x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x3)(x1)(st))(st)
c__case_48_case__241 x1 x2 x3@(Curry.Module.AbstractCurry.C_CLetDecl x11 x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x3)(x1)(st))(st)
c__case_48_case__241 x1 x2 x3@(Curry.Module.AbstractCurry.C_CDoExpr x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x3)(x1)(st))(st)
c__case_48_case__241 x1 x2 x3@(Curry.Module.AbstractCurry.C_CListComp x14 x15) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x3)(x1)(st))(st)
c__case_48_case__241 x1 x2 x3@(Curry.Module.AbstractCurry.C_CCase x16 x17) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showBoxedExpr(x2)(x3)(x1)(st))(st)
c__case_48_case__241 x1 x2 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_48_case__241(x1)(x2)(x)(st))(i)(xs)(st)
c__case_48_case__241 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_48_case__241")(x)



c__case_46_case__242 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleAbstractCurryPrinter.c_showExprOpt(x2)(x3)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x5)(st))(x6)(st))(st)
c__case_46_case__242 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_46_case__242 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_46_case__242(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_46_case__242 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_46_case__242")(x)



c__case_47_case__243 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_showExprOpt(x2)(x3)(x1)(st))(st)
c__case_47_case__243 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_46(x2)(x3)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x5)(st))(st)
c__case_47_case__243 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_47_case__243(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_47_case__243 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_47_case__243")(x)



c__case_45_case__244 x1 x2 x4 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_45_case__244 x1 x2 x4 x3@((Curry.Module.Prelude.:<) x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.Oracle.c_apply(x2)(x5)(x1)(st))(Curry.Module.OracleAbstractCurryPrinter.c_prefixMap(x2)(x6)(x4)(x7)(st))(x8)(st))(st)
c__case_45_case__244 x1 x2 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_45_case__244(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_45_case__244 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_45_case__244")(x)



c__case_42_case__245 x1 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_42_case__245 x1 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_dropTags(x4)(x1)(st))(st)
c__case_42_case__245 x1 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_42_case__245(x1)(x4)(x)(st))(i)(xs)(st)
c__case_42_case__245 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_42_case__245")(x)



c__case_43_case__246 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAbstractCurryPrinter.c_dropTags))))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_tail))))(Curry.Module.OraclePrelude.c_dropWhile(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_47_61))(st))(Curry.Module.Prelude.C_Char('\"'))))))(x4)(x1)(st))(x6)(st))(x7)(st))(st)
c__case_43_case__246 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_42(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('>'))(x1)(st))(x8)(st))(st)
c__case_43_case__246 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_43_case__246(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_43_case__246 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_43_case__246")(x)



c__case_44_case__247 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_43(x3)(x4)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('\"'))(x1)(st))(x5)(st))(st)
c__case_44_case__247 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_44_case__247(x1)(x)(st))(i)(xs)(st)
c__case_44_case__247 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_44_case__247")(x)



c__case_40_case__248 x1 x3@(Curry.Module.Prelude.T2 x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List)))(x6)(st))(x7)(st))(st)
c__case_40_case__248 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_40_case__248(x1)(x)(st))(i)(xs)(st)
c__case_40_case__248 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_40_case__248")(x)



c__case_37_case__249 x1 x8 x11@(Curry.Module.AbstractCurry.C_CCharc x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c_isStringList(x8)(x1)(st))(st)
c__case_37_case__249 x1 x8 x11@(Curry.Module.AbstractCurry.C_CIntc x13) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_37_case__249 x1 x8 x11@(Curry.Module.AbstractCurry.C_CFloatc x14) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_37_case__249 x1 x8 (Curry.Module.AbstractCurry.C_CLiteralOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_37_case__249(x1)(x8)(x)(st))(i)(xs)(st)
c__case_37_case__249 x1 x8 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_37_case__249")(x)



c__case_38_case__250 x1 x8 x10@(Curry.Module.AbstractCurry.C_CLit x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_37(x8)(x11)(x1)(st))(st)
c__case_38_case__250 x1 x8 x10@(Curry.Module.AbstractCurry.C_CVar x15) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_38_case__250 x1 x8 x10@(Curry.Module.AbstractCurry.C_CSymbol x16) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_38_case__250 x1 x8 x10@(Curry.Module.AbstractCurry.C_CApply x17 x18) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_38_case__250 x1 x8 x10@(Curry.Module.AbstractCurry.C_CLambda x19 x20) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_38_case__250 x1 x8 x10@(Curry.Module.AbstractCurry.C_CLetDecl x21 x22) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_38_case__250 x1 x8 x10@(Curry.Module.AbstractCurry.C_CDoExpr x23) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_38_case__250 x1 x8 x10@(Curry.Module.AbstractCurry.C_CListComp x24 x25) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_38_case__250 x1 x8 x10@(Curry.Module.AbstractCurry.C_CCase x26 x27) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_38_case__250 x1 x8 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_38_case__250(x1)(x8)(x)(st))(i)(xs)(st)
c__case_38_case__250 x1 x8 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_38_case__250")(x)



c__case_39_case__251 x1 x8 x7@(Curry.Module.AbstractCurry.C_CApply x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_38(x8)(x10)(x1)(st))(st)
c__case_39_case__251 x1 x8 x7@(Curry.Module.AbstractCurry.C_CVar x28) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_39_case__251 x1 x8 x7@(Curry.Module.AbstractCurry.C_CLit x29) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_39_case__251 x1 x8 x7@(Curry.Module.AbstractCurry.C_CSymbol x30) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_39_case__251 x1 x8 x7@(Curry.Module.AbstractCurry.C_CLambda x31 x32) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_39_case__251 x1 x8 x7@(Curry.Module.AbstractCurry.C_CLetDecl x33 x34) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_39_case__251 x1 x8 x7@(Curry.Module.AbstractCurry.C_CDoExpr x35) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_39_case__251 x1 x8 x7@(Curry.Module.AbstractCurry.C_CListComp x36 x37) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_39_case__251 x1 x8 x7@(Curry.Module.AbstractCurry.C_CCase x38 x39) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_39_case__251 x1 x8 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_39_case__251(x1)(x8)(x)(st))(i)(xs)(st)
c__case_39_case__251 x1 x8 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_39_case__251")(x)



c__case_41_case__252 x1 x2@(Curry.Module.AbstractCurry.C_CSymbol x3) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_40(x3)(x1)(st))(st)
c__case_41_case__252 x1 x2@(Curry.Module.AbstractCurry.C_CVar x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_41_case__252 x1 x2@(Curry.Module.AbstractCurry.C_CApply x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_39(x8)(x7)(x1)(st))(st)
c__case_41_case__252 x1 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_41_case__252(x1)(x)(st))(i)(xs)(st)
c__case_41_case__252 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_41_case__252")(x)



c__case_33_case__253 x1 x4 x7@(Curry.Module.Prelude.T2 x8 x9) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x8)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(x1)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x9)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(x10)(st))(Curry.Module.OracleAbstractCurryPrinter.c_isClosedList(x4)(x11)(st))(x12)(st))(x13)(st))(st)
c__case_33_case__253 x1 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_33_case__253(x1)(x4)(x)(st))(i)(xs)(st)
c__case_33_case__253 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_33_case__253")(x)



c__case_34_case__254 x1 x4 x5@(Curry.Module.AbstractCurry.C_CSymbol x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_33(x4)(x7)(x1)(st))(st)
c__case_34_case__254 x1 x4 x5@(Curry.Module.AbstractCurry.C_CVar x10) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_34_case__254 x1 x4 x5@(Curry.Module.AbstractCurry.C_CLit x11) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_34_case__254 x1 x4 x5@(Curry.Module.AbstractCurry.C_CApply x12 x13) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_34_case__254 x1 x4 x5@(Curry.Module.AbstractCurry.C_CLambda x14 x15) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_34_case__254 x1 x4 x5@(Curry.Module.AbstractCurry.C_CLetDecl x16 x17) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_34_case__254 x1 x4 x5@(Curry.Module.AbstractCurry.C_CDoExpr x18) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_34_case__254 x1 x4 x5@(Curry.Module.AbstractCurry.C_CListComp x19 x20) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_34_case__254 x1 x4 x5@(Curry.Module.AbstractCurry.C_CCase x21 x22) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_34_case__254 x1 x4 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_34_case__254(x1)(x4)(x)(st))(i)(xs)(st)
c__case_34_case__254 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_34_case__254")(x)



c__case_35_case__255 x1 x4 x3@(Curry.Module.AbstractCurry.C_CApply x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_34(x4)(x5)(x1)(st))(st)
c__case_35_case__255 x1 x4 x3@(Curry.Module.AbstractCurry.C_CVar x23) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_35_case__255 x1 x4 x3@(Curry.Module.AbstractCurry.C_CLit x24) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_35_case__255 x1 x4 x3@(Curry.Module.AbstractCurry.C_CSymbol x25) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_35_case__255 x1 x4 x3@(Curry.Module.AbstractCurry.C_CLambda x26 x27) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_35_case__255 x1 x4 x3@(Curry.Module.AbstractCurry.C_CLetDecl x28 x29) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_35_case__255 x1 x4 x3@(Curry.Module.AbstractCurry.C_CDoExpr x30) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_35_case__255 x1 x4 x3@(Curry.Module.AbstractCurry.C_CListComp x31 x32) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_35_case__255 x1 x4 x3@(Curry.Module.AbstractCurry.C_CCase x33 x34) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_35_case__255 x1 x4 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_35_case__255(x1)(x4)(x)(st))(i)(xs)(st)
c__case_35_case__255 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_35_case__255")(x)



c__case_32_case__256 x1 x35@(Curry.Module.Prelude.T2 x36 x37) st = let {x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x39 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x38)((Curry.Module.Prelude.:<)(x39)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x36)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x37)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List)))(x38)(st))(x39)(st))(st)
c__case_32_case__256 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_32_case__256(x1)(x)(st))(i)(xs)(st)
c__case_32_case__256 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_32_case__256")(x)



c__case_36_case__257 x1 x2@(Curry.Module.AbstractCurry.C_CApply x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_35(x4)(x3)(x1)(st))(st)
c__case_36_case__257 x1 x2@(Curry.Module.AbstractCurry.C_CSymbol x35) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_32(x35)(x1)(st))(st)
c__case_36_case__257 x1 x2@(Curry.Module.AbstractCurry.C_CVar x38) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_36_case__257 x1 x2@(Curry.Module.AbstractCurry.C_CLit x39) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_36_case__257 x1 x2@(Curry.Module.AbstractCurry.C_CLambda x40 x41) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_36_case__257 x1 x2@(Curry.Module.AbstractCurry.C_CLetDecl x42 x43) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_36_case__257 x1 x2@(Curry.Module.AbstractCurry.C_CDoExpr x44) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_36_case__257 x1 x2@(Curry.Module.AbstractCurry.C_CListComp x45 x46) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_36_case__257 x1 x2@(Curry.Module.AbstractCurry.C_CCase x47 x48) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_36_case__257 x1 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_36_case__257(x1)(x)(st))(i)(xs)(st)
c__case_36_case__257 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_36_case__257")(x)



c__case_30_case__258 x1 x5@(Curry.Module.Prelude.T2 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_not))))(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_isInfixOpName(x1)(st))(x7)(x8)(st))(x9)(st))(st)
c__case_30_case__258 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_30_case__258(x1)(x)(st))(i)(xs)(st)
c__case_30_case__258 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_30_case__258")(x)



c__case_13_case__259 x1 x12 x26@Curry.Module.Prelude.List st = let {x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x29)((Curry.Module.Prelude.:<)(x30)((Curry.Module.Prelude.:<)(x31)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)((Curry.Module.Prelude.:<)(x34)(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_61_61(x12)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(x1)(st))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_61_61(x12)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List)))(x29)(st))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_61_61(x12)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List)))(x30)(st))(Curry.Module.OracleAbstractCurryPrinter.c_isTuple(x12)(x31)(st))(x32)(st))(x33)(st))(x34)(st))(st)
c__case_13_case__259 x1 x12 x26@((Curry.Module.Prelude.:<) x27 x28) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_13_case__259 x1 x12 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_13_case__259(x1)(x12)(x)(st))(i)(xs)(st)
c__case_13_case__259 x1 x12 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_13_case__259")(x)



c__case_14_case__260 x1 x12 x26 x27@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_13(x12)(x26)(x1)(st))(st)
c__case_14_case__260 x1 x12 x26 x27@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_14_case__260 x1 x12 x26 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_14_case__260(x1)(x12)(x26)(x)(st))(i)(xs)(st)
c__case_14_case__260 x1 x12 x26 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_14_case__260")(x)



c__case_15_case__261 x1 x12 x24@((Curry.Module.Prelude.:<) x25 x26) st = let {x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_14(x12)(x25)(x26)(Curry.Module.OraclePrelude.op_61_61(x25)(Curry.Module.Prelude.C_Char('e'))(x1)(st))(x27)(st))(st)
c__case_15_case__261 x1 x12 x24@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_15_case__261 x1 x12 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_15_case__261(x1)(x12)(x)(st))(i)(xs)(st)
c__case_15_case__261 x1 x12 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_15_case__261")(x)



c__case_16_case__262 x1 x12 x24 x25@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_15(x12)(x24)(x1)(st))(st)
c__case_16_case__262 x1 x12 x24 x25@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_16_case__262 x1 x12 x24 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_16_case__262(x1)(x12)(x24)(x)(st))(i)(xs)(st)
c__case_16_case__262 x1 x12 x24 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_16_case__262")(x)



c__case_17_case__263 x1 x12 x22@((Curry.Module.Prelude.:<) x23 x24) st = let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_16(x12)(x23)(x24)(Curry.Module.OraclePrelude.op_61_61(x23)(Curry.Module.Prelude.C_Char('d'))(x1)(st))(x25)(st))(st)
c__case_17_case__263 x1 x12 x22@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_17_case__263 x1 x12 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_17_case__263(x1)(x12)(x)(st))(i)(xs)(st)
c__case_17_case__263 x1 x12 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_17_case__263")(x)



c__case_18_case__264 x1 x12 x22 x23@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_17(x12)(x22)(x1)(st))(st)
c__case_18_case__264 x1 x12 x22 x23@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_18_case__264 x1 x12 x22 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_18_case__264(x1)(x12)(x22)(x)(st))(i)(xs)(st)
c__case_18_case__264 x1 x12 x22 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_18_case__264")(x)



c__case_19_case__265 x1 x12 x20@((Curry.Module.Prelude.:<) x21 x22) st = let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_18(x12)(x21)(x22)(Curry.Module.OraclePrelude.op_61_61(x21)(Curry.Module.Prelude.C_Char('u'))(x1)(st))(x23)(st))(st)
c__case_19_case__265 x1 x12 x20@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_19_case__265 x1 x12 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_19_case__265(x1)(x12)(x)(st))(i)(xs)(st)
c__case_19_case__265 x1 x12 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_19_case__265")(x)



c__case_20_case__266 x1 x12 x20 x21@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_19(x12)(x20)(x1)(st))(st)
c__case_20_case__266 x1 x12 x20 x21@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_20_case__266 x1 x12 x20 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_20_case__266(x1)(x12)(x20)(x)(st))(i)(xs)(st)
c__case_20_case__266 x1 x12 x20 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_20_case__266")(x)



c__case_21_case__267 x1 x12 x18@((Curry.Module.Prelude.:<) x19 x20) st = let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_20(x12)(x19)(x20)(Curry.Module.OraclePrelude.op_61_61(x19)(Curry.Module.Prelude.C_Char('l'))(x1)(st))(x21)(st))(st)
c__case_21_case__267 x1 x12 x18@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_21_case__267 x1 x12 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_21_case__267(x1)(x12)(x)(st))(i)(xs)(st)
c__case_21_case__267 x1 x12 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_21_case__267")(x)



c__case_22_case__268 x1 x12 x18 x19@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_21(x12)(x18)(x1)(st))(st)
c__case_22_case__268 x1 x12 x18 x19@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_22_case__268 x1 x12 x18 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_22_case__268(x1)(x12)(x18)(x)(st))(i)(xs)(st)
c__case_22_case__268 x1 x12 x18 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_22_case__268")(x)



c__case_23_case__269 x1 x12 x16@((Curry.Module.Prelude.:<) x17 x18) st = let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_22(x12)(x17)(x18)(Curry.Module.OraclePrelude.op_61_61(x17)(Curry.Module.Prelude.C_Char('e'))(x1)(st))(x19)(st))(st)
c__case_23_case__269 x1 x12 x16@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_23_case__269 x1 x12 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_23_case__269(x1)(x12)(x)(st))(i)(xs)(st)
c__case_23_case__269 x1 x12 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_23_case__269")(x)



c__case_24_case__270 x1 x12 x16 x17@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_23(x12)(x16)(x1)(st))(st)
c__case_24_case__270 x1 x12 x16 x17@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_24_case__270 x1 x12 x16 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_24_case__270(x1)(x12)(x16)(x)(st))(i)(xs)(st)
c__case_24_case__270 x1 x12 x16 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_24_case__270")(x)



c__case_25_case__271 x1 x12 x14@((Curry.Module.Prelude.:<) x15 x16) st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_24(x12)(x15)(x16)(Curry.Module.OraclePrelude.op_61_61(x15)(Curry.Module.Prelude.C_Char('r'))(x1)(st))(x17)(st))(st)
c__case_25_case__271 x1 x12 x14@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_25_case__271 x1 x12 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_25_case__271(x1)(x12)(x)(st))(i)(xs)(st)
c__case_25_case__271 x1 x12 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_25_case__271")(x)



c__case_26_case__272 x1 x12 x14 x15@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_25(x12)(x14)(x1)(st))(st)
c__case_26_case__272 x1 x12 x14 x15@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_26_case__272 x1 x12 x14 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_26_case__272(x1)(x12)(x14)(x)(st))(i)(xs)(st)
c__case_26_case__272 x1 x12 x14 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_26_case__272")(x)



c__case_27_case__273 x1 x12 x11@((Curry.Module.Prelude.:<) x13 x14) st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_26(x12)(x13)(x14)(Curry.Module.OraclePrelude.op_61_61(x13)(Curry.Module.Prelude.C_Char('P'))(x1)(st))(x15)(st))(st)
c__case_27_case__273 x1 x12 x11@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_27_case__273 x1 x12 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_27_case__273(x1)(x12)(x)(st))(i)(xs)(st)
c__case_27_case__273 x1 x12 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_27_case__273")(x)



c__case_28_case__274 x1 x10@(Curry.Module.Prelude.T2 x11 x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_27(x12)(x11)(x1)(st))(st)
c__case_28_case__274 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_28_case__274(x1)(x)(st))(i)(xs)(st)
c__case_28_case__274 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_28_case__274")(x)



c__case_29_case__275 x1 x42@(Curry.Module.AbstractCurry.C_CSymbol x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_28(x10)(x1)(st))(st)
c__case_29_case__275 x1 x42@(Curry.Module.AbstractCurry.C_CVar x29) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_29_case__275 x1 x42@(Curry.Module.AbstractCurry.C_CLit x30) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_29_case__275 x1 x42@(Curry.Module.AbstractCurry.C_CApply x31 x32) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_29_case__275 x1 x42@(Curry.Module.AbstractCurry.C_CLambda x33 x34) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_29_case__275 x1 x42@(Curry.Module.AbstractCurry.C_CLetDecl x35 x36) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_29_case__275 x1 x42@(Curry.Module.AbstractCurry.C_CDoExpr x37) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_29_case__275 x1 x42@(Curry.Module.AbstractCurry.C_CListComp x38 x39) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_29_case__275 x1 x42@(Curry.Module.AbstractCurry.C_CCase x40 x41) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_29_case__275 x1 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_29_case__275(x1)(x)(st))(i)(xs)(st)
c__case_29_case__275 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_29_case__275")(x)



c__case_31_case__276 x1 x2@(Curry.Module.AbstractCurry.C_CVar x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_31_case__276 x1 x2@(Curry.Module.AbstractCurry.C_CLit x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_31_case__276 x1 x2@(Curry.Module.AbstractCurry.C_CSymbol x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_30(x5)(x1)(st))(st)
c__case_31_case__276 x1 x2@(Curry.Module.AbstractCurry.C_CApply x8 x9) st = let {x51 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x51)(Curry.Module.Prelude.List))(Curry.Module.OracleAbstractCurryPrinter.c__case_29(x8)(Curry.Module.OracleAbstractCurryPrinter.c_applicationHead(x8)(x1)(st))(x51)(st))(st)
c__case_31_case__276 x1 x2@(Curry.Module.AbstractCurry.C_CLambda x42 x43) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_31_case__276 x1 x2@(Curry.Module.AbstractCurry.C_CLetDecl x44 x45) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_31_case__276 x1 x2@(Curry.Module.AbstractCurry.C_CDoExpr x46) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_31_case__276 x1 x2@(Curry.Module.AbstractCurry.C_CListComp x47 x48) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_31_case__276 x1 x2@(Curry.Module.AbstractCurry.C_CCase x49 x50) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_31_case__276 x1 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_31_case__276(x1)(x)(st))(i)(xs)(st)
c__case_31_case__276 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_31_case__276")(x)



c__case_11_case__277 x1 x5@(Curry.Module.Prelude.T2 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_not))))(Curry.Module.Oracle.c_apply(Curry.Module.OracleAbstractCurryPrinter.c_isInfixOpName(x1)(st))(x7)(x8)(st))(x9)(st))(st)
c__case_11_case__277 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_11_case__277(x1)(x)(st))(i)(xs)(st)
c__case_11_case__277 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_11_case__277")(x)



c__case_12_case__278 x1 x2@(Curry.Module.AbstractCurry.C_CVar x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_12_case__278 x1 x2@(Curry.Module.AbstractCurry.C_CLit x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_12_case__278 x1 x2@(Curry.Module.AbstractCurry.C_CSymbol x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_11(x5)(x1)(st))(st)
c__case_12_case__278 x1 x2@(Curry.Module.AbstractCurry.C_CApply x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_12_case__278 x1 x2@(Curry.Module.AbstractCurry.C_CLambda x10 x11) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_12_case__278 x1 x2@(Curry.Module.AbstractCurry.C_CLetDecl x12 x13) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_12_case__278 x1 x2@(Curry.Module.AbstractCurry.C_CDoExpr x14) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_12_case__278 x1 x2@(Curry.Module.AbstractCurry.C_CListComp x15 x16) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_12_case__278 x1 x2@(Curry.Module.AbstractCurry.C_CCase x17 x18) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_12_case__278 x1 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_12_case__278(x1)(x)(st))(i)(xs)(st)
c__case_12_case__278 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_12_case__278")(x)



c__case_8_case__279 x1 x5 x6 x4@Curry.Module.Prelude.List st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x6)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))(Curry.Module.Prelude.List))))))))(x9)(st))(x10)(st))(st)
c__case_8_case__279 x1 x5 x6 x4@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_8_case__279 x1 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_8_case__279(x1)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_8_case__279 x1 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_8_case__279")(x)



c__case_9_case__280 x1 x4 x3@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_8(x5)(x6)(x4)(x1)(st))(st)
c__case_9_case__280 x1 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_9_case__280(x1)(x4)(x)(st))(i)(xs)(st)
c__case_9_case__280 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_9_case__280")(x)



c__case_10_case__281 x1 x2@(Curry.Module.AbstractCurry.C_CTCons x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_9(x4)(x3)(x1)(st))(st)
c__case_10_case__281 x1 x2@(Curry.Module.AbstractCurry.C_CTVar x9) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_10_case__281 x1 x2@(Curry.Module.AbstractCurry.C_CFuncType x10 x11) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_10_case__281 x1 (Curry.Module.AbstractCurry.C_CTypeExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_10_case__281(x1)(x)(st))(i)(xs)(st)
c__case_10_case__281 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_10_case__281")(x)



c__case_7_case__282 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_7_case__282 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('('))(x1)(st))(Curry.Module.OracleAbstractCurryPrinter.c_isTuple'46p1_isTuple'46492(x4)(x5)(st))(x6)(st))(st)
c__case_7_case__282 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_7_case__282(x1)(x)(st))(i)(xs)(st)
c__case_7_case__282 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_7_case__282")(x)



c__case_5_case__283 x1 x3 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char(')'))(x1)(st))(st)
c__case_5_case__283 x1 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.C_Char(','))(x1)(st))(Curry.Module.OracleAbstractCurryPrinter.c_isTuple'46p1_isTuple'46492((Curry.Module.Prelude.:<)(x5)(x6))(x7)(st))(x8)(st))(st)
c__case_5_case__283 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_5_case__283(x1)(x3)(x)(st))(i)(xs)(st)
c__case_5_case__283 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_5_case__283")(x)



c__case_6_case__284 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_6_case__284 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_5(x3)(x4)(x1)(st))(st)
c__case_6_case__284 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_6_case__284(x1)(x)(st))(i)(xs)(st)
c__case_6_case__284 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_6_case__284")(x)



c__case_4_case__285 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(st)
c__case_4_case__285 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_4_case__285 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_4_case__285(x1)(x)(st))(i)(xs)(st)
c__case_4_case__285 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_4_case__285")(x)



c__case_3_case__286 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st)
c__case_3_case__286 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_3_case__286 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_3_case__286(x1)(x)(st))(i)(xs)(st)
c__case_3_case__286 x1 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_3_case__286")(x)



c__case_1_case__287 x1 x3 x4@(Curry.Module.Prelude.T2 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFiniteMap.c_addToFM(x3)(x10)(Curry.Module.Prelude.T0)(x1)(st))(st)
c__case_1_case__287 x1 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_1_case__287(x1)(x3)(x)(st))(i)(xs)(st)
c__case_1_case__287 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_1_case__287")(x)



c__case_0_case__288 x1 x3 x12@(Curry.Module.Prelude.T2 x17 x18) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFiniteMap.c_addToFM(x3)(x18)(Curry.Module.Prelude.T0)(x1)(st))(st)
c__case_0_case__288 x1 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_0_case__288(x1)(x3)(x)(st))(i)(xs)(st)
c__case_0_case__288 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_0_case__288")(x)



c__case_2_case__289 x1 x3 x2@(Curry.Module.AbstractCurry.C_CFunc x4 x5 x6 x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_1(x3)(x4)(x1)(st))(st)
c__case_2_case__289 x1 x3 x2@(Curry.Module.AbstractCurry.C_CmtFunc x11 x12 x13 x14 x15 x16) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAbstractCurryPrinter.c__case_0(x3)(x12)(x1)(st))(st)
c__case_2_case__289 x1 x3 (Curry.Module.AbstractCurry.C_CFuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAbstractCurryPrinter.c__case_2_case__289(x1)(x3)(x)(st))(i)(xs)(st)
c__case_2_case__289 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleAbstractCurryPrinter._case_2_case__289")(x)


