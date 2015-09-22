{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleCompactFlatCurry (module Curry.Module.OracleCompactFlatCurry) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.CompactFlatCurry
import Curry.Module.Directory
import Curry.Module.Distribution
import Curry.Module.FileGoodies
import Curry.Module.FlatCurry
import Curry.Module.List
import Curry.Module.Maybe
import Curry.Module.Prelude
import Curry.Module.RedBlackTree
import Curry.Module.SetRBT
import Curry.Module.Sort
import Curry.Module.TableRBT
import Curry.Module.Time
import Curry.Module.XML
import Curry.Module.OracleDirectory
import Curry.Module.OracleDistribution
import Curry.Module.OracleFileGoodies
import Curry.Module.OracleFlatCurry
import Curry.Module.OracleList
import Curry.Module.OracleMaybe
import Curry.Module.OraclePrelude
import Curry.Module.OracleRedBlackTree
import Curry.Module.OracleSetRBT
import Curry.Module.OracleSort
import Curry.Module.OracleTableRBT
import Curry.Module.OracleTime
import Curry.Module.OracleXML



-- begin included



-- end included

c_isMainOption :: Curry.Module.CompactFlatCurry.C_Option -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isMainOption x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_182(x2)(x1)(st))(st)



c_getMainFuncFromOptions :: (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_getMainFuncFromOptions x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_181(x2)(x1)(st))(st)



c_getRequiredFromOptions :: (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_RequiredSpec
c_getRequiredFromOptions x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_concat(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleCompactFlatCurry.c_getRequiredFromOptions'46_'35lambda6))(st))(Curry.Module.Prelude.List)(x2)(x1)(st))(x3)(st))(st)



c_getRequiredFromOptions'46_'35lambda6 :: Curry.Module.CompactFlatCurry.C_Option -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_RequiredSpec)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_RequiredSpec)
c_getRequiredFromOptions'46_'35lambda6 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_179(x3)(x2)(x1)(st))(st)



c_addImport2Options :: (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option
c_addImport2Options x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43(x2)(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc(Curry.Module.CompactFlatCurry.C_Import))))(Curry.Module.OracleList.c_nub(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_addImport2Options'46alwaysReqMod'4621))))(x1)(st))(Curry.Module.OracleCompactFlatCurry.c_getRequiredFromOptions(x2)(x3)(st))(x4)(st))(x5)(st))(x6)(st))(x7)(st))(st)



c_addImport2Options'46alwaysReqMod'4621 :: Curry.Module.CompactFlatCurry.C_RequiredSpec -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_addImport2Options'46alwaysReqMod'4621 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_178(x2)(x1)(st))(st)



c_requires :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.CompactFlatCurry.C_RequiredSpec
c_requires x2 x3 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.CompactFlatCurry.C_Requires(x2)(x3))(st)



c_alwaysRequired :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.CompactFlatCurry.C_RequiredSpec
c_alwaysRequired x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.CompactFlatCurry.C_AlwaysReq(x2))(st)



c_defaultRequired :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_RequiredSpec
c_defaultRequired x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)((Curry.Module.Prelude.:<)(x30)((Curry.Module.Prelude.:<)(x31)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)((Curry.Module.Prelude.:<)(x34)((Curry.Module.Prelude.:<)(x35)((Curry.Module.Prelude.:<)(x36)(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))))((Curry.Module.Prelude.:<)(Curry.Module.OracleCompactFlatCurry.c_alwaysRequired(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(x2)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleCompactFlatCurry.c_alwaysRequired(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x3)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))(Curry.Module.Prelude.List))))))))(x4)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleCompactFlatCurry.c_alwaysRequired(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x5)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))(Curry.Module.Prelude.List))))))(x6)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleCompactFlatCurry.c_alwaysRequired(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x7)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))))(x8)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleCompactFlatCurry.c_requires(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x9)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x10)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))(Curry.Module.Prelude.List))))(x11)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleCompactFlatCurry.c_requires(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x12)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))(Curry.Module.Prelude.List)))))(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x13)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))(Curry.Module.Prelude.List)))(x14)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleCompactFlatCurry.c_requires(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x15)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x16)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('V'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List)))))))(x17)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleCompactFlatCurry.c_requires(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x18)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x19)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))(Curry.Module.Prelude.List)))))(x20)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleCompactFlatCurry.c_requires(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x21)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x22)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))(Curry.Module.Prelude.List))))(x23)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleCompactFlatCurry.c_requires(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x24)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))(Curry.Module.Prelude.List)))))))(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x25)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))(Curry.Module.Prelude.List)))(x26)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleCompactFlatCurry.c_requires(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x27)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('$'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('#'))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x28)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))))))))))(x29)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleCompactFlatCurry.c_requires(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x30)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))))(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x31)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))))))))))))))))))))(x32)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleCompactFlatCurry.c_requires(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))))))))))))))))))))(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))(Curry.Module.Prelude.List)))))))))))))))))(x33)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleCompactFlatCurry.c_requires(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))(Curry.Module.Prelude.List))))))))))))))))))))))(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))(Curry.Module.Prelude.List)))))))))))))))))(x34)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleCompactFlatCurry.c_requires(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))(Curry.Module.Prelude.List)))))))))))))))))(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))(Curry.Module.Prelude.List)))))))))))))))))(x35)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleCompactFlatCurry.c_requires(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('K'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))))))))))))))))(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('K'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))))))))))(x36)(st))(Curry.Module.Prelude.List)))))))))))))))))(st)



c_prelude :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_prelude x1 st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(st)



c_getRequiredInModule :: (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_RequiredSpec) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getRequiredInModule x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_getRequiredInModule'46getImpReq'4638(x3)))))(x1)(st))(x2)(x4)(st))(st)



c_getRequiredInModule'46getImpReq'4638 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CompactFlatCurry.C_RequiredSpec -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getRequiredInModule'46getImpReq'4638 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_176(x2)(x3)(x1)(st))(st)



c_getImplicitlyRequired :: (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_RequiredSpec) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getImplicitlyRequired x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_getImplicitlyRequired'46getImpReq'4646(x3)))))(x1)(st))(x2)(x4)(st))(st)



c_getImplicitlyRequired'46getImpReq'4646 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CompactFlatCurry.C_RequiredSpec -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getImplicitlyRequired'46getImpReq'4646 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_173(x2)(x3)(x1)(st))(st)



c_defaultRequiredTypes :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_defaultRequiredTypes x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x2)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x3)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x4)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x5)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.OracleCompactFlatCurry.c_prelude(x6)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.List)))))))(st)



c_generateCompactFlatCurryFile :: (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_generateCompactFlatCurryFile x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleCompactFlatCurry.c_computeCompactFlatCurry(x2)(x3)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_generateCompactFlatCurryFile'46_'35lambda8(x4)))))(x5)(st))(st)



c_generateCompactFlatCurryFile'46_'35lambda8 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_Prog -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_generateCompactFlatCurryFile'46_'35lambda8 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_62_62(Curry.Module.OracleFlatCurry.c_writeFCY(x2)(x3)(x1)(st))(Curry.Module.OraclePrelude.c_done(x4)(st))(x5)(st))(st)



c_computeCompactFlatCurry :: (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_computeCompactFlatCurry x2 x3 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(let {x4 = Curry.Module.OracleCompactFlatCurry.c_addImport2Options(x2)(x1)(st)} in let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))))))(Curry.Module.OracleCompactFlatCurry.c__case_171(x3)(x4)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_elem(Curry.Module.CompactFlatCurry.C_Exports)(x5)(st))(x4)(x6)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_any(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_isMainOption))))(x7)(st))(x4)(x8)(st))(x9)(st))(x10)(st))(st))(st)



c_computeCompactFlatCurry'46_'35lambda9 :: (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option) -> Curry.Module.FlatCurry.C_Prog -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_computeCompactFlatCurry'46_'35lambda9 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleCompactFlatCurry.c_makeCompactFlatCurry(x3)(x2)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_computeCompactFlatCurry'46_'35lambda9'46_'35lambda10))))(x4)(st))(st)



c_computeCompactFlatCurry'46_'35lambda9'46_'35lambda10 :: Curry.Module.FlatCurry.C_Prog -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_computeCompactFlatCurry'46_'35lambda9'46_'35lambda10 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_62_62(Curry.Module.OraclePrelude.c_putStrLn(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('z'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))))))))))))))))))))))(Curry.Module.OraclePrelude.c_show(Curry.Module.OraclePrelude.c_length(Curry.Module.OracleCompactFlatCurry.c_moduleFuns(x2)(x1)(st))(x3)(st))(x4)(st))(x5)(st))(x6)(st))(Curry.Module.OraclePrelude.c_return(x2)(x7)(st))(x8)(st))(st)



c_makeCompactFlatCurry :: Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_makeCompactFlatCurry x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleCompactFlatCurry.c_requiredInCompactProg(x2)(x3)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_makeCompactFlatCurry'46_'35lambda11(x2)(x3)))))(x4)(st))(st)



c_makeCompactFlatCurry'46_'35lambda11 :: Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option) -> (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_makeCompactFlatCurry'46_'35lambda11 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_170(x2)(x3)(x4)(x1)(st))(st)



c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12 :: Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_169(x2)(x3)(x1)(st))(st)



c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12'46_'35lambda13 :: (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.FlatCurry.C_TypeDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12'46_'35lambda13 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OracleSetRBT.c_elemRBT(Curry.Module.OracleCompactFlatCurry.c_tconsName(x3)(x1)(st))(x4)(st))(x2)(x5)(st))(st)



c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12'46_'35lambda14 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.FlatCurry.C_OpDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12'46_'35lambda14 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_168(x2)(x3)(x1)(st))(st)



c_requiredDatatypes :: (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_TypeDecl) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_requiredDatatypes x2 x3 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(let {x4 = Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_newTypeConsOfTDecl(x2)))))(x1)(st))(x3)(x5)(st)} in let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleCompactFlatCurry.c__case_167(x2)(x3)(x4)(Curry.Module.OraclePrelude.c_null(x4)(x6)(st))(x7)(st))(st))(st)



c_newTypeConsOfTDecl :: (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.FlatCurry.C_TypeDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_newTypeConsOfTDecl x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_166(x2)(x3)(x1)(st))(st)



c_newTypeConsOfTDecl'46_'35lambda15 :: (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_newTypeConsOfTDecl'46_'35lambda15 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_not(Curry.Module.Oracle.c_apply(Curry.Module.OracleSetRBT.c_elemRBT(x3)(x1)(st))(x2)(x4)(st))(x5)(st))(st)



c_newTypeConsOfTDecl'46_'35lambda16 :: (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_newTypeConsOfTDecl'46_'35lambda16 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_not(Curry.Module.Oracle.c_apply(Curry.Module.OracleSetRBT.c_elemRBT(x3)(x1)(st))(x2)(x4)(st))(x5)(st))(st)



c_newTypeConsOfTDecl'46_'35lambda17 :: Curry.Module.FlatCurry.C_ConsDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_newTypeConsOfTDecl'46_'35lambda17 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_163(x2)(x1)(st))(st)



c_extendTConsWithConsType :: (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_TypeDecl) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_extendTConsWithConsType x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_162(x2)(x3)(x4)(x1)(st))(st)



c_extendTConsWithConsType'46_'35lambda18 :: (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.FlatCurry.C_ConsDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_extendTConsWithConsType'46_'35lambda18 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OracleSetRBT.c_elemRBT(Curry.Module.OracleCompactFlatCurry.c_consName(x3)(x1)(st))(x4)(st))(x2)(x5)(st))(st)



c_extendFuncTable :: (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.FlatCurry.C_FuncDecl)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.FlatCurry.C_FuncDecl)
c_extendFuncTable x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleCompactFlatCurry.c_extendFuncTable'46_'35lambda19))(st))(x2)(x3)(x1)(st))(st)



c_extendFuncTable'46_'35lambda19 :: Curry.Module.FlatCurry.C_FuncDecl -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.FlatCurry.C_FuncDecl)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.FlatCurry.C_FuncDecl)
c_extendFuncTable'46_'35lambda19 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OracleTableRBT.c_updateRBT(Curry.Module.OracleCompactFlatCurry.c_functionName(x2)(x1)(st))(x2)(x4)(st))(x3)(x5)(st))(st)



c_requiredInCompactProg :: Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_Option) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog))))
c_requiredInCompactProg x2 x3 x1 st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(let {x5 = Curry.Module.OracleCompactFlatCurry.c_moduleName(x2)(x10)(st)} in let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(let {x6 = Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleCompactFlatCurry.c_requiredInCompactProg'46_'35lambda26))(st))(Curry.Module.Prelude.List)(x3)(x11)(st)} in let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))(let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List)))))))(let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List)))(Curry.Module.OracleCompactFlatCurry.c__case_159(x2)(x3)(Curry.Module.OracleList.c_nub(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleCompactFlatCurry.c_requiredInCompactProg'46_'35lambda22))(st))(Curry.Module.Prelude.List)(x3)(x1)(st))(x9)(st))(x5)(x6)(Curry.Module.OracleCompactFlatCurry.c_exportedFuncNames(Curry.Module.OracleCompactFlatCurry.c_moduleFuns(x2)(x12)(st))(x13)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OracleSetRBT.c_insertRBT(x14)(st))(x5)(x15)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OracleSetRBT.c_emptySetRBT(x16)(st))(Curry.Module.OracleSort.c_leqString(x17)(st))(x18)(st))(x19)(st))(Curry.Module.OraclePrelude.c_not(Curry.Module.OraclePrelude.c_null(x6)(x20)(st))(x21)(st))(x22)(st))(st))(st))(st))(st))(st))(st)



c_requiredInCompactProg'46_'35lambda22 :: Curry.Module.CompactFlatCurry.C_Option -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_requiredInCompactProg'46_'35lambda22 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_154(x3)(x2)(x1)(st))(st)



c_requiredInCompactProg'46_'35lambda26 :: Curry.Module.CompactFlatCurry.C_Option -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_requiredInCompactProg'46_'35lambda26 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_153(x3)(x2)(x1)(st))(st)



c_requiredInCompactProg'46add2mainmodset'46118 :: (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_requiredInCompactProg'46add2mainmodset'46118 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_foldr(Curry.Module.OracleSetRBT.c_insertRBT(x1)(st))(x2)(x3)(x4)(st))(st)



c_requiredInCompactProg'46_'35lambda28 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))) -> Curry.Module.FlatCurry.C_Prog -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog))))
c_requiredInCompactProg'46_'35lambda28 x2 x3 x4 x5 x6 x1 st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_return(Curry.Module.Prelude.T3(Curry.Module.OraclePrelude.c_concat(x3)(x1)(st))(Curry.Module.OracleCompactFlatCurry.c_requiredInCompactProg'46add2mainmodset'46118(x5)(x2)(x7)(st))((Curry.Module.Prelude.:<)(x4)(x6)))(x8)(st))(st)



c_requiredInCompactProg'46_'35lambda29 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.FlatCurry.C_Prog -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog))))
c_requiredInCompactProg'46_'35lambda29 x2 x3 x4 x5 x6 x1 st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_return(Curry.Module.Prelude.T3(Curry.Module.OracleList.c_nub(x3)(x1)(st))(Curry.Module.OracleCompactFlatCurry.c_requiredInCompactProg'46add2mainmodset'46118(x5)(x2)(x7)(st))((Curry.Module.Prelude.:<)(x4)(x6)))(x8)(st))(st)



c_requiredInCompactProg'46_'35lambda30 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog))))
c_requiredInCompactProg'46_'35lambda30 x2 x3 x4 x5 x6 x7 x1 st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_return(Curry.Module.Prelude.T3((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(x5)(x2))(Curry.Module.Prelude.List))(Curry.Module.OracleCompactFlatCurry.c_requiredInCompactProg'46add2mainmodset'46118(x6)(x3)(x1)(st))((Curry.Module.Prelude.:<)(x4)(x7)))(x8)(st))(st)



c_requiredInCompactProg'46_'35lambda31 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.FlatCurry.C_Prog -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog))))
c_requiredInCompactProg'46_'35lambda31 x2 x3 x4 x5 x1 st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.c_return(Curry.Module.Prelude.T3(Curry.Module.OracleList.c_nub(Curry.Module.OraclePrelude.op_43_43(x2)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_exportedFuncNames))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_moduleFuns))))(x1)(st))(x6)(st))(x5)(x7)(st))(x8)(st))(x9)(st))(Curry.Module.OracleCompactFlatCurry.c_requiredInCompactProg'46add2mainmodset'46118(x4)(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_moduleName))))(x5)(x10)(st))(x11)(st))((Curry.Module.Prelude.:<)(x3)(x5)))(x12)(st))(st)



c_exportedFuncNames :: (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_exportedFuncNames x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_exportedFuncNames'46_'35lambda32))))(Curry.Module.OraclePrelude.c_filter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_exportedFuncNames'46_'35lambda33))))(x2)(x1)(st))(x3)(st))(st)



c_exportedFuncNames'46_'35lambda32 :: Curry.Module.FlatCurry.C_FuncDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_exportedFuncNames'46_'35lambda32 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_152(x2)(x1)(st))(st)



c_exportedFuncNames'46_'35lambda33 :: Curry.Module.FlatCurry.C_FuncDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_exportedFuncNames'46_'35lambda33 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_151(x2)(x1)(st))(st)



c_getCalledFuncs :: (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_RequiredSpec) -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.FlatCurry.C_FuncDecl)) -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))
c_getCalledFuncs x2 x3 x4 x5 x6 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_150(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x1)(st))(st)



c_getCalledFuncs'46_'35lambda34 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.FlatCurry.C_FuncDecl)) -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) -> (Curry.Module.Prelude.List Curry.Module.CompactFlatCurry.C_RequiredSpec) -> Curry.Module.FlatCurry.C_Prog -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))
c_getCalledFuncs'46_'35lambda34 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x1 st = let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(let {x13 = Curry.Module.OracleCompactFlatCurry.c_getRequiredInModule(x11)(x9)(x1)(st)} in let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List)))))))))(Curry.Module.OracleCompactFlatCurry.c_getCalledFuncs(x11)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OracleSetRBT.c_insertRBT(x14)(st))(x9)(x15)(st))(x7)(x16)(st))((Curry.Module.Prelude.:<)(x12)(x10))(Curry.Module.OracleCompactFlatCurry.c_extendFuncTable(x4)(Curry.Module.OracleCompactFlatCurry.c_moduleFuns(x12)(x17)(st))(x18)(st))(Curry.Module.OraclePrelude.c_foldr(Curry.Module.OracleSetRBT.c_insertRBT(x19)(st))(x6)(x13)(x20)(st))(x5)(x8)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(x9)(x2))(Curry.Module.OraclePrelude.op_43_43(x3)(x13)(x21)(st)))(x22)(st))(st))(st)



c_getCalledFuncs'46_'35lambda35 :: (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_getCalledFuncs'46_'35lambda35 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_not(Curry.Module.Oracle.c_apply(Curry.Module.OracleSetRBT.c_elemRBT(x3)(x1)(st))(x2)(x4)(st))(x5)(st))(st)



c_getCalledFuncs'46_'35lambda36 :: (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_getCalledFuncs'46_'35lambda36 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_not(Curry.Module.Oracle.c_apply(Curry.Module.OracleSetRBT.c_elemRBT(x3)(x1)(st))(x2)(x4)(st))(x5)(st))(st)



c_getCalledFuncs'46_'35lambda37 :: Curry.Module.FlatCurry.C_FuncDecl -> (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Prog) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.OracleRedBlackTree.C_RedBlackTree (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))
c_getCalledFuncs'46_'35lambda37 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_145(x2)(x3)(x1)(st))(st)



c_allFuncCalls :: Curry.Module.FlatCurry.C_FuncDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_allFuncCalls x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_144(x2)(x1)(st))(st)



c_allFuncCallsOfExpr :: Curry.Module.FlatCurry.C_Expr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_allFuncCallsOfExpr x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_142(x2)(x1)(st))(st)



c_allFuncCallsOfBranchExpr :: Curry.Module.FlatCurry.C_BranchExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_allFuncCallsOfBranchExpr x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_140(x2)(x1)(st))(st)



c_allConstructorsOfFunc :: Curry.Module.FlatCurry.C_FuncDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_allConstructorsOfFunc x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_139(x2)(x1)(st))(st)



c_allConsOfExpr :: Curry.Module.FlatCurry.C_Expr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_allConsOfExpr x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_137(x2)(x1)(st))(st)



c_allConsOfExpr'46consOfBranch'46252 :: Curry.Module.FlatCurry.C_BranchExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_allConsOfExpr'46consOfBranch'46252 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_135(x2)(x1)(st))(st)



c_allTypesOfFunc :: Curry.Module.FlatCurry.C_FuncDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_allTypesOfFunc x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_133(x2)(x1)(st))(st)



c_allTypesOfTExpr :: Curry.Module.FlatCurry.C_TypeExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_allTypesOfTExpr x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_132(x2)(x1)(st))(st)



c_unionMap :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1))
c_unionMap x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleList.c_union))(st))(Curry.Module.Prelude.List)))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(x2)))))(x1)(st))(st)



c_functionName :: Curry.Module.FlatCurry.C_FuncDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_functionName x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_131(x2)(x1)(st))(st)



c_consName :: Curry.Module.FlatCurry.C_ConsDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_consName x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_130(x2)(x1)(st))(st)



c_tconsName :: Curry.Module.FlatCurry.C_TypeDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_tconsName x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_129(x2)(x1)(st))(st)



c_moduleImports :: Curry.Module.FlatCurry.C_Prog -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_moduleImports x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_128(x2)(x1)(st))(st)



c_moduleTypes :: Curry.Module.FlatCurry.C_Prog -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.FlatCurry.C_TypeDecl
c_moduleTypes x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_127(x2)(x1)(st))(st)



c_moduleOps :: Curry.Module.FlatCurry.C_Prog -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.FlatCurry.C_OpDecl
c_moduleOps x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_126(x2)(x1)(st))(st)



c_moduleName :: Curry.Module.FlatCurry.C_Prog -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_moduleName x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_125(x2)(x1)(st))(st)



c_moduleFuns :: Curry.Module.FlatCurry.C_Prog -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl
c_moduleFuns x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_124(x2)(x1)(st))(st)



c_leqQName :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_leqQName x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_123(x3)(x2)(x1)(st))(st)



c_readCurrentFlatCurry :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_readCurrentFlatCurry x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_62_62(Curry.Module.OraclePrelude.c_putStr(Curry.Module.OraclePrelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List))))(x1)(st))(x3)(st))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleCompactFlatCurry.c_findSourceFileInLoadPath(x2)(x4)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_readCurrentFlatCurry'46_'35lambda40))))(x5)(st))(x6)(st))(st)



c_readCurrentFlatCurry'46_'35lambda40 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_readCurrentFlatCurry'46_'35lambda40 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDirectory.c_doesFileExist(Curry.Module.OracleFlatCurry.c_flatCurryFileName(x2)(x1)(st))(x3)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41(x2)))))(x4)(st))(st)



c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleCompactFlatCurry.c__case_121(x2)(x3)(Curry.Module.OraclePrelude.c_not(x3)(x1)(st))(x4)(st))(st)



c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41'46_'35lambda42 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Time.C_ClockTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41'46_'35lambda42 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDirectory.c_getModificationTime(Curry.Module.OracleFlatCurry.c_flatCurryFileName(x2)(x1)(st))(x4)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41'46_'35lambda42'46_'35lambda43(x3)(x2)))))(x5)(st))(st)



c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41'46_'35lambda42'46_'35lambda43 :: Curry.Module.Time.C_ClockTime -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Time.C_ClockTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41'46_'35lambda42'46_'35lambda43 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))))(Curry.Module.OracleCompactFlatCurry.c__case_120(x2)(x3)(x4)(Curry.Module.OraclePrelude.op_62(Curry.Module.OracleTime.c_clockTimeToInt(x2)(x1)(st))(Curry.Module.OracleTime.c_clockTimeToInt(x4)(x5)(st))(x6)(st))(x7)(st))(st)



c_getSourceModificationTime :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Time.C_ClockTime))
c_getSourceModificationTime x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDirectory.c_doesFileExist(Curry.Module.OraclePrelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))(x1)(st))(x3)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_getSourceModificationTime'46_'35lambda44(x2)))))(x4)(st))(st)



c_getSourceModificationTime'46_'35lambda44 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Time.C_ClockTime))
c_getSourceModificationTime'46_'35lambda44 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_119(x2)(x3)(x1)(st))(st)



c_findSourceFileInLoadPath :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_findSourceFileInLoadPath x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDistribution.c_getLoadPathForFile(x2)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_findSourceFileInLoadPath'46_'35lambda45(x2)))))(x3)(st))(st)



c_findSourceFileInLoadPath'46_'35lambda45 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_findSourceFileInLoadPath'46_'35lambda45 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleFileGoodies.c_lookupFileInPath(Curry.Module.OracleFileGoodies.c_baseName(x2)(x1)(st))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(Curry.Module.Prelude.List)))(x3)(x4)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_findSourceFileInLoadPath'46_'35lambda45'46_'35lambda46(x2)))))(x5)(st))(st)



c_findSourceFileInLoadPath'46_'35lambda45'46_'35lambda46 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_findSourceFileInLoadPath'46_'35lambda45'46_'35lambda46 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.c_maybe(Curry.Module.OraclePrelude.c_error(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List))))))))))))))))))))))))(Curry.Module.OraclePrelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))(Curry.Module.Prelude.List)))))))))))))(x1)(st))(x4)(st))(x5)(st))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_return))))(Curry.Module.OracleFileGoodies.c_stripSuffix(x6)(st))(x7)(st))(x3)(x8)(st))(st)



c_processPrimitives :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_Prog -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_processPrimitives x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleCompactFlatCurry.c_readPrimSpec(Curry.Module.OracleCompactFlatCurry.c_moduleName(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('2'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))(Curry.Module.Prelude.List))))))))))(x4)(st))(x5)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_processPrimitives'46_'35lambda47(x3)))))(x6)(st))(st)



c_processPrimitives'46_'35lambda47 :: Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_processPrimitives'46_'35lambda47 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_return(Curry.Module.OracleCompactFlatCurry.c_mergePrimSpecIntoModule(x3)(x2)(x1)(st))(x4)(st))(st)



c_mergePrimSpecIntoModule :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))) -> Curry.Module.FlatCurry.C_Prog -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Prog
c_mergePrimSpecIntoModule x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_118(x2)(x3)(x1)(st))(st)



c_mergePrimSpecIntoFunc :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))) -> Curry.Module.FlatCurry.C_FuncDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl
c_mergePrimSpecIntoFunc x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_117(x2)(x3)(x1)(st))(st)



c_mergePrimSpecIntoFunc'46_'35selFP3'35lib :: (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_mergePrimSpecIntoFunc'46_'35selFP3'35lib x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_114(x2)(x1)(st))(st)



c_mergePrimSpecIntoFunc'46_'35selFP4'35entry :: (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_mergePrimSpecIntoFunc'46_'35selFP4'35entry x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_112(x2)(x1)(st))(st)



c_readPrimSpec :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))
c_readPrimSpec x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleDirectory.c_doesFileExist(x3)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_readPrimSpec'46_'35lambda48(x2)(x3)))))(x4)(st))(st)



c_readPrimSpec'46_'35lambda48 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))
c_readPrimSpec'46_'35lambda48 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_110(x2)(x3)(x4)(x1)(st))(st)



c_readPrimSpec'46_'35lambda48'46_'35lambda49 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))))
c_readPrimSpec'46_'35lambda48'46_'35lambda49 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_return(Curry.Module.OracleCompactFlatCurry.c_xml2primtrans(x2)(x3)(x1)(st))(x4)(st))(st)



c_xml2primtrans :: (Curry t0) => t0 -> Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 t0 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_xml2primtrans x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_109(x2)(x3)(x1)(st))(st)



c_xml2primtrans'46xml2prim'46358 :: (Curry t248) => t248 -> Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 t248 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_xml2primtrans'46xml2prim'46358 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_86(x2)(x3)(x1)(st))(st)



c__case_86 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_86_case__182(x1)(x2)(x3)(st))(st)



c__case_85 x2 x5 x6 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_85_case__181(x1)(x2)(x5)(x6)(x4)(st))(st)



c__case_84 x2 x5 x6 x8 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_84_case__180(x1)(x2)(x5)(x6)(x8)(x7)(st))(st)



c__case_22 x2 x5 x6 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_22_case__179(x1)(x2)(x5)(x6)(x8)(st))(st)



c__case_21 x2 x5 x6 x72 x71 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_21_case__178(x1)(x2)(x5)(x6)(x72)(x71)(st))(st)



c__case_20 x2 x5 x6 x72 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_20_case__177(x1)(x2)(x5)(x6)(x72)(st))(st)



c__case_19 x2 x5 x6 x74 x73 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_19_case__176(x1)(x2)(x5)(x6)(x74)(x73)(st))(st)



c__case_18 x2 x5 x6 x74 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_18_case__175(x1)(x2)(x5)(x6)(x74)(st))(st)



c__case_17 x2 x5 x6 x76 x75 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_17_case__174(x1)(x2)(x5)(x6)(x76)(x75)(st))(st)



c__case_16 x2 x5 x6 x76 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_16_case__173(x1)(x2)(x5)(x6)(x76)(st))(st)



c__case_15 x2 x5 x6 x78 x77 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_15_case__172(x1)(x2)(x5)(x6)(x78)(x77)(st))(st)



c__case_14 x2 x5 x6 x78 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_14_case__171(x1)(x2)(x5)(x6)(x78)(st))(st)



c__case_13 x2 x5 x6 x80 x79 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_13_case__170(x1)(x2)(x5)(x6)(x80)(x79)(st))(st)



c__case_12 x2 x5 x6 x80 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_12_case__169(x1)(x2)(x5)(x6)(x80)(st))(st)



c__case_11 x2 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_11_case__168(x1)(x2)(x6)(x5)(st))(st)



c__case_10 x2 x6 x81 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_10_case__167(x1)(x2)(x6)(x81)(st))(st)



c__case_9 x2 x6 x84 x83 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_9_case__166(x1)(x2)(x6)(x84)(x83)(st))(st)



c__case_8 x2 x6 x84 x86 x85 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_8_case__165(x1)(x2)(x6)(x84)(x86)(x85)(st))(st)



c__case_7 x2 x6 x84 x86 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_7_case__164(x1)(x2)(x6)(x84)(x86)(st))(st)



c__case_6 x2 x6 x84 x88 x87 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_6_case__163(x1)(x2)(x6)(x84)(x88)(x87)(st))(st)



c__case_5 x2 x6 x84 x88 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_5_case__162(x1)(x2)(x6)(x84)(x88)(st))(st)



c__case_4 x2 x6 x84 x90 x89 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_4_case__161(x1)(x2)(x6)(x84)(x90)(x89)(st))(st)



c__case_3 x2 x6 x84 x90 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_3_case__160(x1)(x2)(x6)(x84)(x90)(st))(st)



c__case_2 x2 x6 x84 x92 x91 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_2_case__159(x1)(x2)(x6)(x84)(x92)(x91)(st))(st)



c__case_1 x2 x6 x84 x92 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_1_case__158(x1)(x2)(x6)(x84)(x92)(st))(st)



c__case_0 x2 x84 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_0_case__157(x1)(x2)(x84)(x6)(st))(st)



c__case_83 x2 x5 x6 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_83_case__156(x1)(x2)(x5)(x6)(x8)(st))(st)



c__case_82 x2 x5 x6 x10 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_82_case__155(x1)(x2)(x5)(x6)(x10)(x9)(st))(st)



c__case_81 x2 x5 x6 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_81_case__154(x1)(x2)(x5)(x6)(x10)(st))(st)



c__case_80 x2 x5 x6 x12 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_80_case__153(x1)(x2)(x5)(x6)(x12)(x11)(st))(st)



c__case_79 x2 x5 x6 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_79_case__152(x1)(x2)(x5)(x6)(x12)(st))(st)



c__case_78 x2 x5 x6 x14 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_78_case__151(x1)(x2)(x5)(x6)(x14)(x13)(st))(st)



c__case_77 x2 x5 x6 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_77_case__150(x1)(x2)(x5)(x6)(x14)(st))(st)



c__case_76 x2 x5 x6 x16 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_76_case__149(x1)(x2)(x5)(x6)(x16)(x15)(st))(st)



c__case_75 x2 x5 x6 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_75_case__148(x1)(x2)(x5)(x6)(x16)(st))(st)



c__case_74 x2 x5 x6 x18 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_74_case__147(x1)(x2)(x5)(x6)(x18)(x17)(st))(st)



c__case_73 x2 x5 x6 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_73_case__146(x1)(x2)(x5)(x6)(x18)(st))(st)



c__case_72 x2 x5 x6 x20 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_72_case__145(x1)(x2)(x5)(x6)(x20)(x19)(st))(st)



c__case_71 x2 x5 x6 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_71_case__144(x1)(x2)(x5)(x6)(x20)(st))(st)



c__case_70 x2 x5 x6 x22 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_70_case__143(x1)(x2)(x5)(x6)(x22)(x21)(st))(st)



c__case_69 x2 x5 x6 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_69_case__142(x1)(x2)(x5)(x6)(x22)(st))(st)



c__case_68 x2 x5 x6 x24 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_68_case__141(x1)(x2)(x5)(x6)(x24)(x23)(st))(st)



c__case_67 x2 x5 x6 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_67_case__140(x1)(x2)(x5)(x6)(x24)(st))(st)



c__case_66 x2 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_66_case__139(x1)(x2)(x6)(x5)(st))(st)



c__case_65 x2 x6 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_65_case__138(x1)(x2)(x6)(x25)(st))(st)



c__case_64 x2 x6 x28 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_64_case__137(x1)(x2)(x6)(x28)(x27)(st))(st)



c__case_63 x2 x6 x28 x30 x29 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_63_case__136(x1)(x2)(x6)(x28)(x30)(x29)(st))(st)



c__case_62 x2 x6 x28 x30 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_62_case__135(x1)(x2)(x6)(x28)(x30)(st))(st)



c__case_61 x2 x6 x28 x32 x31 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_61_case__134(x1)(x2)(x6)(x28)(x32)(x31)(st))(st)



c__case_60 x2 x6 x28 x32 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_60_case__133(x1)(x2)(x6)(x28)(x32)(st))(st)



c__case_59 x2 x6 x28 x34 x33 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_59_case__132(x1)(x2)(x6)(x28)(x34)(x33)(st))(st)



c__case_58 x2 x6 x28 x34 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_58_case__131(x1)(x2)(x6)(x28)(x34)(st))(st)



c__case_57 x2 x6 x28 x36 x35 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_57_case__130(x1)(x2)(x6)(x28)(x36)(x35)(st))(st)



c__case_56 x2 x6 x28 x36 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_56_case__129(x1)(x2)(x6)(x28)(x36)(st))(st)



c__case_55 x2 x28 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_55_case__128(x1)(x2)(x28)(x6)(st))(st)



c__case_54 x2 x28 x38 x37 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_54_case__127(x1)(x2)(x28)(x38)(x37)(st))(st)



c__case_53 x2 x28 x38 x40 x41 x39 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_53_case__126(x1)(x2)(x28)(x38)(x40)(x41)(x39)(st))(st)



c__case_52 x2 x28 x38 x40 x41 x43 x42 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_52_case__125(x1)(x2)(x28)(x38)(x40)(x41)(x43)(x42)(st))(st)



c__case_51 x2 x28 x38 x40 x41 x43 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_51_case__124(x1)(x2)(x28)(x38)(x40)(x41)(x43)(st))(st)



c__case_50 x2 x28 x38 x40 x41 x45 x44 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_50_case__123(x1)(x2)(x28)(x38)(x40)(x41)(x45)(x44)(st))(st)



c__case_49 x2 x28 x38 x40 x41 x45 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_49_case__122(x1)(x2)(x28)(x38)(x40)(x41)(x45)(st))(st)



c__case_48 x2 x28 x38 x40 x41 x47 x46 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_48_case__121(x1)(x2)(x28)(x38)(x40)(x41)(x47)(x46)(st))(st)



c__case_47 x2 x28 x38 x40 x41 x47 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_47_case__120(x1)(x2)(x28)(x38)(x40)(x41)(x47)(st))(st)



c__case_46 x2 x28 x38 x40 x41 x49 x48 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_46_case__119(x1)(x2)(x28)(x38)(x40)(x41)(x49)(x48)(st))(st)



c__case_45 x2 x28 x38 x40 x41 x49 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_45_case__118(x1)(x2)(x28)(x38)(x40)(x41)(x49)(st))(st)



c__case_44 x2 x28 x38 x40 x41 x51 x50 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_44_case__117(x1)(x2)(x28)(x38)(x40)(x41)(x51)(x50)(st))(st)



c__case_43 x2 x28 x38 x40 x41 x51 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_43_case__116(x1)(x2)(x28)(x38)(x40)(x41)(x51)(st))(st)



c__case_42 x2 x28 x38 x40 x41 x53 x52 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_42_case__115(x1)(x2)(x28)(x38)(x40)(x41)(x53)(x52)(st))(st)



c__case_41 x2 x28 x38 x40 x41 x53 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_41_case__114(x1)(x2)(x28)(x38)(x40)(x41)(x53)(st))(st)



c__case_40 x2 x28 x38 x40 x41 x55 x54 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_40_case__113(x1)(x2)(x28)(x38)(x40)(x41)(x55)(x54)(st))(st)



c__case_39 x2 x28 x38 x40 x41 x55 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_39_case__112(x1)(x2)(x28)(x38)(x40)(x41)(x55)(st))(st)



c__case_38 x2 x28 x38 x41 x40 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_38_case__111(x1)(x2)(x28)(x38)(x41)(x40)(st))(st)



c__case_37 x2 x28 x41 x38 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_37_case__110(x1)(x2)(x28)(x41)(x38)(st))(st)



c__case_36 x2 x28 x41 x57 x56 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_36_case__109(x1)(x2)(x28)(x41)(x57)(x56)(st))(st)



c__case_35 x2 x28 x41 x57 x59 x60 x58 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_35_case__108(x1)(x2)(x28)(x41)(x57)(x59)(x60)(x58)(st))(st)



c__case_34 x2 x28 x41 x57 x59 x60 x62 x61 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_34_case__107(x1)(x2)(x28)(x41)(x57)(x59)(x60)(x62)(x61)(st))(st)



c__case_33 x2 x28 x41 x57 x59 x60 x62 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_33_case__106(x1)(x2)(x28)(x41)(x57)(x59)(x60)(x62)(st))(st)



c__case_32 x2 x28 x41 x57 x59 x60 x64 x63 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_32_case__105(x1)(x2)(x28)(x41)(x57)(x59)(x60)(x64)(x63)(st))(st)



c__case_31 x2 x28 x41 x57 x59 x60 x64 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_31_case__104(x1)(x2)(x28)(x41)(x57)(x59)(x60)(x64)(st))(st)



c__case_30 x2 x28 x41 x57 x59 x60 x66 x65 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_30_case__103(x1)(x2)(x28)(x41)(x57)(x59)(x60)(x66)(x65)(st))(st)



c__case_29 x2 x28 x41 x57 x59 x60 x66 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_29_case__102(x1)(x2)(x28)(x41)(x57)(x59)(x60)(x66)(st))(st)



c__case_28 x2 x28 x41 x57 x59 x60 x68 x67 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_28_case__101(x1)(x2)(x28)(x41)(x57)(x59)(x60)(x68)(x67)(st))(st)



c__case_27 x2 x28 x41 x57 x59 x60 x68 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_27_case__100(x1)(x2)(x28)(x41)(x57)(x59)(x60)(x68)(st))(st)



c__case_26 x2 x28 x41 x57 x59 x60 x70 x69 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_26_case__99(x1)(x2)(x28)(x41)(x57)(x59)(x60)(x70)(x69)(st))(st)



c__case_25 x2 x28 x41 x57 x59 x60 x70 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_25_case__98(x1)(x2)(x28)(x41)(x57)(x59)(x60)(x70)(st))(st)



c__case_24 x2 x28 x41 x57 x60 x59 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_24_case__97(x1)(x2)(x28)(x41)(x57)(x60)(x59)(st))(st)



c__case_23 x2 x28 x41 x60 x57 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_23_case__96(x1)(x2)(x28)(x41)(x60)(x57)(st))(st)



c__case_109 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_109_case__95(x1)(x2)(x3)(st))(st)



c__case_108 x2 x5 x6 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_108_case__94(x1)(x2)(x5)(x6)(x4)(st))(st)



c__case_107 x2 x5 x6 x8 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_107_case__93(x1)(x2)(x5)(x6)(x8)(x7)(st))(st)



c__case_106 x2 x5 x6 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_106_case__92(x1)(x2)(x5)(x6)(x8)(st))(st)



c__case_105 x2 x5 x6 x10 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_105_case__91(x1)(x2)(x5)(x6)(x10)(x9)(st))(st)



c__case_104 x2 x5 x6 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_104_case__90(x1)(x2)(x5)(x6)(x10)(st))(st)



c__case_103 x2 x5 x6 x12 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_103_case__89(x1)(x2)(x5)(x6)(x12)(x11)(st))(st)



c__case_102 x2 x5 x6 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_102_case__88(x1)(x2)(x5)(x6)(x12)(st))(st)



c__case_101 x2 x5 x6 x14 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_101_case__87(x1)(x2)(x5)(x6)(x14)(x13)(st))(st)



c__case_100 x2 x5 x6 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_100_case__86(x1)(x2)(x5)(x6)(x14)(st))(st)



c__case_99 x2 x5 x6 x16 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_99_case__85(x1)(x2)(x5)(x6)(x16)(x15)(st))(st)



c__case_98 x2 x5 x6 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_98_case__84(x1)(x2)(x5)(x6)(x16)(st))(st)



c__case_97 x2 x5 x6 x18 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_97_case__83(x1)(x2)(x5)(x6)(x18)(x17)(st))(st)



c__case_96 x2 x5 x6 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_96_case__82(x1)(x2)(x5)(x6)(x18)(st))(st)



c__case_95 x2 x5 x6 x20 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_95_case__81(x1)(x2)(x5)(x6)(x20)(x19)(st))(st)



c__case_94 x2 x5 x6 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_94_case__80(x1)(x2)(x5)(x6)(x20)(st))(st)



c__case_93 x2 x5 x6 x22 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_93_case__79(x1)(x2)(x5)(x6)(x22)(x21)(st))(st)



c__case_92 x2 x5 x6 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_92_case__78(x1)(x2)(x5)(x6)(x22)(st))(st)



c__case_91 x2 x5 x6 x24 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_91_case__77(x1)(x2)(x5)(x6)(x24)(x23)(st))(st)



c__case_90 x2 x5 x6 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_90_case__76(x1)(x2)(x5)(x6)(x24)(st))(st)



c__case_89 x2 x5 x6 x26 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_89_case__75(x1)(x2)(x5)(x6)(x26)(x25)(st))(st)



c__case_88 x2 x5 x6 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_88_case__74(x1)(x2)(x5)(x6)(x26)(st))(st)



c__case_87 x2 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_87_case__73(x1)(x2)(x6)(x5)(st))(st)



c__case_110 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_110_case__72(x1)(x2)(x3)(x4)(st))(st)



c__case_112 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_112_case__71(x1)(x2)(st))(st)



c__case_111 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_111_case__70(x1)(x3)(st))(st)



c__case_114 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_114_case__69(x1)(x2)(st))(st)



c__case_113 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_113_case__68(x1)(x3)(st))(st)



c__case_117 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_117_case__67(x1)(x2)(x3)(st))(st)



c__case_116 x4 x5 x6 x7 x8 x9 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_116_case__66(x1)(x4)(x5)(x6)(x7)(x8)(x9)(x13)(st))(st)



c__case_115 x4 x5 x6 x7 x11 x12 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_115_case__65(x1)(x4)(x5)(x6)(x7)(x11)(x12)(x13)(st))(st)



c__case_118 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_118_case__64(x1)(x2)(x3)(st))(st)



c__case_119 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_119_case__63(x1)(x2)(x3)(st))(st)



c__case_120 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_120_case__62(x1)(x3)(x5)(st))(st)



c__case_121 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_121_case__61(x1)(x2)(x4)(st))(st)



c__case_123 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_123_case__60(x1)(x3)(x2)(st))(st)



c__case_122 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_122_case__59(x1)(x4)(x5)(x3)(st))(st)



c__case_124 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_124_case__58(x1)(x2)(st))(st)



c__case_125 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_125_case__57(x1)(x2)(st))(st)



c__case_126 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_126_case__56(x1)(x2)(st))(st)



c__case_127 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_127_case__55(x1)(x2)(st))(st)



c__case_128 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_128_case__54(x1)(x2)(st))(st)



c__case_129 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_129_case__53(x1)(x2)(st))(st)



c__case_130 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_130_case__52(x1)(x2)(st))(st)



c__case_131 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_131_case__51(x1)(x2)(st))(st)



c__case_132 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_132_case__50(x1)(x2)(st))(st)



c__case_133 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_133_case__49(x1)(x2)(st))(st)



c__case_135 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_135_case__48(x1)(x2)(st))(st)



c__case_134 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_134_case__47(x1)(x4)(x3)(st))(st)



c__case_137 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_137_case__46(x1)(x2)(st))(st)



c__case_136 x6 x8 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_136_case__45(x1)(x6)(x8)(x5)(st))(st)



c__case_139 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_139_case__44(x1)(x2)(st))(st)



c__case_138 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_138_case__43(x1)(x7)(st))(st)



c__case_140 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_140_case__42(x1)(x2)(st))(st)



c__case_142 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_142_case__41(x1)(x2)(st))(st)



c__case_141 x6 x8 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_141_case__40(x1)(x6)(x8)(x5)(st))(st)



c__case_144 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_144_case__39(x1)(x2)(st))(st)



c__case_143 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_143_case__38(x1)(x7)(st))(st)



c__case_145 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_145_case__37(x1)(x2)(x3)(st))(st)



c__case_150 x2 x3 x4 x5 x6 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_150_case__36(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(st))(st)



c__case_149 x2 x3 x4 x5 x6 x7 x8 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_149_case__35(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x11)(x10)(st))(st)



c__case_148 x2 x3 x4 x5 x6 x7 x8 x11 x12 x13 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_148_case__34(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x11)(x12)(x13)(x14)(st))(st)



c__case_147 x2 x3 x4 x5 x6 x7 x8 x11 x12 x13 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_147_case__33(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x11)(x12)(x13)(x14)(st))(st)



c__case_146 x2 x3 x4 x5 x6 x7 x8 x11 x12 x13 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_146_case__32(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x11)(x12)(x13)(x21)(st))(st)



c__case_151 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_151_case__31(x1)(x2)(st))(st)



c__case_152 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_152_case__30(x1)(x2)(st))(st)



c__case_153 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_153_case__29(x1)(x3)(x2)(st))(st)



c__case_154 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_154_case__28(x1)(x3)(x2)(st))(st)



c__case_159 x2 x3 x4 x5 x6 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_159_case__27(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(st))(st)



c__case_158 x2 x3 x4 x5 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_158_case__26(x1)(x2)(x3)(x4)(x5)(x7)(x8)(x9)(st))(st)



c__case_157 x2 x3 x4 x5 x7 x8 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_157_case__25(x1)(x2)(x3)(x4)(x5)(x7)(x8)(x10)(st))(st)



c__case_155 x2 x4 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_155_case__24(x1)(x2)(x4)(x7)(x8)(x9)(st))(st)



c__case_156 x2 x4 x5 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_156_case__23(x1)(x2)(x4)(x5)(x8)(x9)(x10)(st))(st)



c__case_162 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_162_case__22(x1)(x2)(x3)(x4)(st))(st)



c__case_161 x2 x3 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_161_case__21(x1)(x2)(x3)(x6)(x5)(st))(st)



c__case_160 x2 x3 x6 x11 x14 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_160_case__20(x1)(x2)(x3)(x6)(x11)(x15)(st))(st)



c__case_163 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_163_case__19(x1)(x2)(st))(st)



c__case_166 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_166_case__18(x1)(x2)(x3)(st))(st)



c__case_164 x2 x8 x11 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_164_case__17(x1)(x2)(x11)(x12)(st))(st)



c__case_165 x2 x4 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_165_case__16(x1)(x2)(x7)(x8)(st))(st)



c__case_167 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_167_case__15(x1)(x2)(x3)(x4)(x5)(st))(st)



c__case_168 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_168_case__14(x1)(x2)(x3)(st))(st)



c__case_169 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_169_case__13(x1)(x2)(x3)(st))(st)



c__case_170 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_170_case__12(x1)(x2)(x3)(x4)(st))(st)



c__case_171 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_171_case__11(x1)(x3)(x4)(x5)(st))(st)



c__case_173 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_173_case__10(x1)(x2)(x3)(st))(st)



c__case_172 x2 x5 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_172_case__9(x1)(x6)(x7)(st))(st)



c__case_176 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_176_case__8(x1)(x2)(x3)(st))(st)



c__case_175 x2 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_175_case__7(x1)(x2)(x4)(st))(st)



c__case_174 x2 x5 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_174_case__6(x1)(x5)(x6)(x7)(st))(st)



c__case_178 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_178_case__5(x1)(x2)(st))(st)



c__case_177 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_177_case__4(x1)(x3)(st))(st)



c__case_179 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_179_case__3(x1)(x3)(x2)(st))(st)



c__case_181 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_181_case__2(x1)(x2)(st))(st)



c__case_180 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_180_case__1(x1)(x4)(x3)(st))(st)



c__case_182 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_182_case__0(x1)(x2)(st))(st)



c__case_182_case__0 x1 x2@(Curry.Module.CompactFlatCurry.C_Main x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_182_case__0 x1 x2@Curry.Module.CompactFlatCurry.C_Verbose st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_182_case__0 x1 x2@Curry.Module.CompactFlatCurry.C_Exports st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_182_case__0 x1 x2@(Curry.Module.CompactFlatCurry.C_InitFuncs x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_182_case__0 x1 x2@(Curry.Module.CompactFlatCurry.C_Required x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_182_case__0 x1 x2@(Curry.Module.CompactFlatCurry.C_Import x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_182_case__0 x1 (Curry.Module.CompactFlatCurry.C_OptionOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_182_case__0(x1)(x)(st))(i)(xs)(st)
c__case_182_case__0 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_182_case__0")(x)



c__case_180_case__1 x1 x4 x3@(Curry.Module.CompactFlatCurry.C_Main x5) st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_180_case__1 x1 x4 x3@Curry.Module.CompactFlatCurry.C_Verbose st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c_getMainFuncFromOptions(x4)(x1)(st))(st)
c__case_180_case__1 x1 x4 x3@Curry.Module.CompactFlatCurry.C_Exports st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c_getMainFuncFromOptions(x4)(x1)(st))(st)
c__case_180_case__1 x1 x4 x3@(Curry.Module.CompactFlatCurry.C_InitFuncs x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c_getMainFuncFromOptions(x4)(x1)(st))(st)
c__case_180_case__1 x1 x4 x3@(Curry.Module.CompactFlatCurry.C_Required x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c_getMainFuncFromOptions(x4)(x1)(st))(st)
c__case_180_case__1 x1 x4 x3@(Curry.Module.CompactFlatCurry.C_Import x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c_getMainFuncFromOptions(x4)(x1)(st))(st)
c__case_180_case__1 x1 x4 (Curry.Module.CompactFlatCurry.C_OptionOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_180_case__1(x1)(x4)(x)(st))(i)(xs)(st)
c__case_180_case__1 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_180_case__1")(x)



c__case_181_case__2 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_180(x4)(x3)(x1)(st))(st)
c__case_181_case__2 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_181_case__2(x1)(x)(st))(i)(xs)(st)
c__case_181_case__2 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_181_case__2")(x)



c__case_179_case__3 x1 x3 x2@(Curry.Module.CompactFlatCurry.C_Required x4) st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x4)(x3))(st)
c__case_179_case__3 x1 x3 x2@Curry.Module.CompactFlatCurry.C_Verbose st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_179_case__3 x1 x3 x2@(Curry.Module.CompactFlatCurry.C_Main x5) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_179_case__3 x1 x3 x2@Curry.Module.CompactFlatCurry.C_Exports st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_179_case__3 x1 x3 x2@(Curry.Module.CompactFlatCurry.C_InitFuncs x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_179_case__3 x1 x3 x2@(Curry.Module.CompactFlatCurry.C_Import x7) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_179_case__3 x1 x3 (Curry.Module.CompactFlatCurry.C_OptionOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_179_case__3(x1)(x3)(x)(st))(i)(xs)(st)
c__case_179_case__3 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_179_case__3")(x)



c__case_177_case__4 x1 x3@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(st)
c__case_177_case__4 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_177_case__4(x1)(x)(st))(i)(xs)(st)
c__case_177_case__4 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_177_case__4")(x)



c__case_178_case__5 x1 x2@(Curry.Module.CompactFlatCurry.C_AlwaysReq x3) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_177(x3)(x1)(st))(st)
c__case_178_case__5 x1 x2@(Curry.Module.CompactFlatCurry.C_Requires x6 x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_178_case__5 x1 (Curry.Module.CompactFlatCurry.C_RequiredSpecOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_178_case__5(x1)(x)(st))(i)(xs)(st)
c__case_178_case__5 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_178_case__5")(x)



c__case_174_case__6 x1 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(x5)(x6))(Curry.Module.Prelude.List))(st)
c__case_174_case__6 x1 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_174_case__6 x1 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_174_case__6(x1)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_174_case__6 x1 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_174_case__6")(x)



c__case_175_case__7 x1 x2 x4@(Curry.Module.Prelude.T2 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleCompactFlatCurry.c__case_174(x2)(x5)(x6)(Curry.Module.OraclePrelude.op_61_61(x5)(x2)(x1)(st))(x7)(st))(st)
c__case_175_case__7 x1 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_175_case__7(x1)(x2)(x)(st))(i)(xs)(st)
c__case_175_case__7 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_175_case__7")(x)



c__case_176_case__8 x1 x2 x3@(Curry.Module.CompactFlatCurry.C_AlwaysReq x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_175(x2)(x4)(x1)(st))(st)
c__case_176_case__8 x1 x2 x3@(Curry.Module.CompactFlatCurry.C_Requires x7 x8) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_176_case__8 x1 x2 (Curry.Module.CompactFlatCurry.C_RequiredSpecOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_176_case__8(x1)(x2)(x)(st))(i)(xs)(st)
c__case_176_case__8 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_176_case__8")(x)



c__case_172_case__9 x1 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(st)
c__case_172_case__9 x1 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_172_case__9 x1 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_172_case__9(x1)(x6)(x)(st))(i)(xs)(st)
c__case_172_case__9 x1 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_172_case__9")(x)



c__case_173_case__10 x1 x2 x3@(Curry.Module.CompactFlatCurry.C_AlwaysReq x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_173_case__10 x1 x2 x3@(Curry.Module.CompactFlatCurry.C_Requires x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleCompactFlatCurry.c__case_172(x2)(x5)(x6)(Curry.Module.OraclePrelude.op_61_61(x5)(x2)(x1)(st))(x7)(st))(st)
c__case_173_case__10 x1 x2 (Curry.Module.CompactFlatCurry.C_RequiredSpecOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_173_case__10(x1)(x2)(x)(st))(i)(xs)(st)
c__case_173_case__10 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_173_case__10")(x)



c__case_171_case__11 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))(x1)(st))(st)
c__case_171_case__11 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_62_62(Curry.Module.OraclePrelude.c_putStr((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))))))))(x1)(st))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleCompactFlatCurry.c_readCurrentFlatCurry(x3)(x6)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_computeCompactFlatCurry'46_'35lambda9(x4)))))(x7)(st))(x8)(st))(st)
c__case_171_case__11 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_171_case__11(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_171_case__11 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_171_case__11")(x)



c__case_170_case__12 x1 x2 x3 x4@(Curry.Module.Prelude.T3 x5 x6 x7) st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List)))))(let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(let {x9 = Curry.Module.OracleCompactFlatCurry.c_getRequiredFromOptions(x3)(x15)(st)} in let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))))(let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))(let {x11 = Curry.Module.OraclePrelude.op_43_43(x5)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_getRequiredInModule(x9)))))(x16)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_moduleName))))(x7)(x17)(st))(x18)(st))(x19)(st)} in let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)(Curry.Module.Prelude.List))))))))))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleCompactFlatCurry.c_getCalledFuncs(x9)(x6)(x7)(Curry.Module.OracleCompactFlatCurry.c_extendFuncTable(Curry.Module.OracleTableRBT.c_emptyTableRBT(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleCompactFlatCurry.c_leqQName))(st))(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_moduleFuns))))(x12)(st))(x7)(x13)(st))(x14)(st))(Curry.Module.OraclePrelude.c_foldr(Curry.Module.OracleSetRBT.c_insertRBT(x20)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OracleSetRBT.c_emptySetRBT(x21)(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleCompactFlatCurry.c_leqQName))(st))(x22)(st))(x11)(x23)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OracleSetRBT.c_emptySetRBT(x24)(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleCompactFlatCurry.c_leqQName))(st))(x25)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OracleSetRBT.c_emptySetRBT(x26)(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleCompactFlatCurry.c_leqQName))(st))(x27)(st))(x11)(x28)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12(x2)))))(x29)(st))(st))(st))(st))(st))(st)
c__case_170_case__12 x1 x2 x3 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_170_case__12(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_170_case__12 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_170_case__12")(x)



c__case_169_case__13 x1 x2 x3@(Curry.Module.Prelude.T4 x4 x5 x6 x7) st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x28)(Curry.Module.Prelude.List)))))))))(Curry.Module.OraclePrelude.op_62_62(Curry.Module.OraclePrelude.c_putStrLn(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))(Curry.Module.OraclePrelude.c_show(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_43))(st))(Curry.Module.Prelude.C_Zero)(Curry.Module.OraclePrelude.c_map(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_length))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_moduleFuns))))(x1)(st))(x4)(x12)(st))(x13)(st))(x14)(st))(x15)(st))(x16)(st))(let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.c_return(Curry.Module.FlatCurry.C_Prog(Curry.Module.OracleCompactFlatCurry.c_moduleName(x2)(x18)(st))(Curry.Module.Prelude.List)(let {x9 = Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_moduleTypes))))(x19)(st))(x4)(x20)(st)} in let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x21)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))(let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x22)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x23)(Curry.Module.OraclePrelude.c_filter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12'46_'35lambda13(Curry.Module.OracleCompactFlatCurry.c_requiredDatatypes(Curry.Module.OracleCompactFlatCurry.c_extendTConsWithConsType(x6)(x7)(x9)(x21)(st))(x9)(x22)(st))))))(x9)(x23)(st))(st))(st))(st))(x5)(Curry.Module.OraclePrelude.c_filter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_makeCompactFlatCurry'46_'35lambda11'46_'35lambda12'46_'35lambda14(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_functionName))))(x5)(x17)(st))))))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_moduleOps))))(x24)(st))(x4)(x25)(st))(x26)(st)))(x27)(st))(st))(x28)(st))(st)
c__case_169_case__13 x1 x2 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_169_case__13(x1)(x2)(x)(st))(i)(xs)(st)
c__case_169_case__13 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_169_case__13")(x)



c__case_168_case__14 x1 x2 x3@(Curry.Module.FlatCurry.C_Op x4 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_elem(x4)(x1)(st))(x2)(x7)(st))(st)
c__case_168_case__14 x1 x2 (Curry.Module.FlatCurry.C_OpDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_168_case__14(x1)(x2)(x)(st))(i)(xs)(st)
c__case_168_case__14 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_168_case__14")(x)



c__case_167_case__15 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_167_case__15 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.OracleCompactFlatCurry.c_requiredDatatypes(Curry.Module.OraclePrelude.c_foldr(Curry.Module.OracleSetRBT.c_insertRBT(x1)(st))(x2)(x4)(x6)(st))(x3)(x7)(st))(st)
c__case_167_case__15 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_167_case__15(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_167_case__15 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_167_case__15")(x)



c__case_165_case__16 x1 x2 x7 x8@Curry.Module.Prelude.C_True st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_filter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_newTypeConsOfTDecl'46_'35lambda15(x2)))))(Curry.Module.OracleCompactFlatCurry.c_allTypesOfTExpr(x7)(x1)(st))(x9)(st))(st)
c__case_165_case__16 x1 x2 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_165_case__16 x1 x2 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_165_case__16(x1)(x2)(x7)(x)(st))(i)(xs)(st)
c__case_165_case__16 x1 x2 x7 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_165_case__16")(x)



c__case_164_case__17 x1 x2 x11 x12@Curry.Module.Prelude.C_True st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_filter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_newTypeConsOfTDecl'46_'35lambda16(x2)))))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_newTypeConsOfTDecl'46_'35lambda17))))(x1)(st))(x11)(x13)(st))(x14)(st))(st)
c__case_164_case__17 x1 x2 x11 x12@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_164_case__17 x1 x2 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_164_case__17(x1)(x2)(x11)(x)(st))(i)(xs)(st)
c__case_164_case__17 x1 x2 x11 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_164_case__17")(x)



c__case_166_case__18 x1 x2 x3@(Curry.Module.FlatCurry.C_TypeSyn x4 x5 x6 x7) st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List)))(Curry.Module.OracleCompactFlatCurry.c__case_165(x2)(x4)(x7)(Curry.Module.Oracle.c_apply(Curry.Module.OracleSetRBT.c_elemRBT(x4)(x1)(st))(x2)(x12)(st))(x13)(st))(st)
c__case_166_case__18 x1 x2 x3@(Curry.Module.FlatCurry.C_Type x8 x9 x10 x11) st = let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List)))(Curry.Module.OracleCompactFlatCurry.c__case_164(x2)(x8)(x11)(Curry.Module.Oracle.c_apply(Curry.Module.OracleSetRBT.c_elemRBT(x8)(x1)(st))(x2)(x14)(st))(x15)(st))(st)
c__case_166_case__18 x1 x2 (Curry.Module.FlatCurry.C_TypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_166_case__18(x1)(x2)(x)(st))(i)(xs)(st)
c__case_166_case__18 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_166_case__18")(x)



c__case_163_case__19 x1 x2@(Curry.Module.FlatCurry.C_Cons x3 x4 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_allTypesOfTExpr))))(x1)(st))(x6)(x7)(st))(st)
c__case_163_case__19 x1 (Curry.Module.FlatCurry.C_ConsDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_163_case__19(x1)(x)(st))(i)(xs)(st)
c__case_163_case__19 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_163_case__19")(x)



c__case_160_case__20 x1 x2 x3 x6 x11 x15@Curry.Module.Prelude.C_True st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))))(Curry.Module.OracleCompactFlatCurry.c_extendTConsWithConsType(x2)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OracleSetRBT.c_insertRBT(x1)(st))(x11)(x16)(st))(x3)(x17)(st))(x6)(x18)(st))(st)
c__case_160_case__20 x1 x2 x3 x6 x11 x15@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c_extendTConsWithConsType(x2)(x3)(x6)(x1)(st))(st)
c__case_160_case__20 x1 x2 x3 x6 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_160_case__20(x1)(x2)(x3)(x6)(x11)(x)(st))(i)(xs)(st)
c__case_160_case__20 x1 x2 x3 x6 x11 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_160_case__20")(x)



c__case_161_case__21 x1 x2 x3 x6 x5@(Curry.Module.FlatCurry.C_TypeSyn x7 x8 x9 x10) st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))))(Curry.Module.OracleCompactFlatCurry.c_extendTConsWithConsType(x2)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OracleSetRBT.c_insertRBT(x1)(st))(x7)(x15)(st))(x3)(x16)(st))(x6)(x17)(st))(st)
c__case_161_case__21 x1 x2 x3 x6 x5@(Curry.Module.FlatCurry.C_Type x11 x12 x13 x14) st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List)))))))(Curry.Module.OracleCompactFlatCurry.c__case_160(x2)(x3)(x6)(x11)(x14)(Curry.Module.OraclePrelude.op_124_124(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_elem(x11)(x1)(st))(Curry.Module.OracleCompactFlatCurry.c_defaultRequiredTypes(x18)(st))(x19)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_any(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_extendTConsWithConsType'46_'35lambda18(x2)))))(x20)(st))(x14)(x21)(st))(x22)(st))(x23)(st))(st)
c__case_161_case__21 x1 x2 x3 x6 (Curry.Module.FlatCurry.C_TypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_161_case__21(x1)(x2)(x3)(x6)(x)(st))(i)(xs)(st)
c__case_161_case__21 x1 x2 x3 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_161_case__21")(x)



c__case_162_case__22 x1 x2 x3 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_162_case__22 x1 x2 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_161(x2)(x3)(x6)(x5)(x1)(st))(st)
c__case_162_case__22 x1 x2 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_162_case__22(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_162_case__22 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_162_case__22")(x)



c__case_156_case__23 x1 x2 x4 x5 x8 x9 x10@Curry.Module.Prelude.C_True st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_mapIO(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_readCurrentFlatCurry))))(x1)(st))(x4)(x11)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_requiredInCompactProg'46_'35lambda30(x9)(x4)(x2)(x5)(x8)))))(x12)(st))(st)
c__case_156_case__23 x1 x2 x4 x5 x8 x9 x10@Curry.Module.Prelude.C_False st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_error))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(Curry.Module.OraclePrelude.op_43_43(x9)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))(Curry.Module.Prelude.List)))(x1)(st))(x13)(st))(x14)(st))(st)
c__case_156_case__23 x1 x2 x4 x5 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_156_case__23(x1)(x2)(x4)(x5)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_156_case__23 x1 x2 x4 x5 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_156_case__23")(x)



c__case_155_case__24 x1 x2 x4 x7 x8 x9@Curry.Module.Prelude.C_True st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))))))(Curry.Module.Oracle.op_62_62_61(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_mapIO(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_readCurrentFlatCurry))))(x1)(st))(Curry.Module.OracleList.c_nub(Curry.Module.OraclePrelude.op_43_43(x4)(Curry.Module.OracleCompactFlatCurry.c_moduleImports(x2)(x10)(st))(x11)(st))(x12)(st))(x13)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_requiredInCompactProg'46_'35lambda31(x7)(x2)(x8)))))(x14)(st))(st)
c__case_155_case__24 x1 x2 x4 x7 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_155_case__24 x1 x2 x4 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_155_case__24(x1)(x2)(x4)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_155_case__24 x1 x2 x4 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_155_case__24")(x)



c__case_157_case__25 x1 x2 x3 x4 x5 x7 x8 x10@Curry.Module.Prelude.C_True st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(let {x9 = Curry.Module.OracleCompactFlatCurry.c_getMainFuncFromOptions(x3)(x1)(st)} in let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List)))))(Curry.Module.OracleCompactFlatCurry.c__case_156(x2)(x4)(x5)(x8)(x9)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_elem(Curry.Module.Prelude.T2(x5)(x9))(x11)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_functionName))))(Curry.Module.OracleCompactFlatCurry.c_moduleFuns(x2)(x12)(st))(x13)(st))(x14)(st))(x15)(st))(st))(st)
c__case_157_case__25 x1 x2 x3 x4 x5 x7 x8 x10@Curry.Module.Prelude.C_False st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(Curry.Module.OracleCompactFlatCurry.c__case_155(x2)(x4)(x7)(x8)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x16)(st))(st)
c__case_157_case__25 x1 x2 x3 x4 x5 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_157_case__25(x1)(x2)(x3)(x4)(x5)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_157_case__25 x1 x2 x3 x4 x5 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_157_case__25")(x)



c__case_158_case__26 x1 x2 x3 x4 x5 x7 x8 x9@Curry.Module.Prelude.C_True st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_mapIO(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_readCurrentFlatCurry))))(x1)(st))(x4)(x10)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_requiredInCompactProg'46_'35lambda29(x4)(x7)(x2)(x8)))))(x11)(st))(st)
c__case_158_case__26 x1 x2 x3 x4 x5 x7 x8 x9@Curry.Module.Prelude.C_False st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List)))(Curry.Module.OracleCompactFlatCurry.c__case_157(x2)(x3)(x4)(x5)(x7)(x8)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_any(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_isMainOption))))(x1)(st))(x3)(x12)(st))(x13)(st))(st)
c__case_158_case__26 x1 x2 x3 x4 x5 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_158_case__26(x1)(x2)(x3)(x4)(x5)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_158_case__26 x1 x2 x3 x4 x5 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_158_case__26")(x)



c__case_159_case__27 x1 x2 x3 x4 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_mapIO(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_readCurrentFlatCurry))))(x1)(st))(x4)(x10)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_requiredInCompactProg'46_'35lambda28(x4)(x6)(x2)(x8)))))(x11)(st))(st)
c__case_159_case__27 x1 x2 x3 x4 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_False st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List)))(Curry.Module.OracleCompactFlatCurry.c__case_158(x2)(x3)(x4)(x5)(x7)(x8)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_elem(Curry.Module.CompactFlatCurry.C_Exports)(x1)(st))(x3)(x12)(st))(x13)(st))(st)
c__case_159_case__27 x1 x2 x3 x4 x5 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_159_case__27(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_159_case__27 x1 x2 x3 x4 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_159_case__27")(x)



c__case_154_case__28 x1 x3 x2@(Curry.Module.CompactFlatCurry.C_Import x4) st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x4)(x3))(st)
c__case_154_case__28 x1 x3 x2@Curry.Module.CompactFlatCurry.C_Verbose st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_154_case__28 x1 x3 x2@(Curry.Module.CompactFlatCurry.C_Main x5) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_154_case__28 x1 x3 x2@Curry.Module.CompactFlatCurry.C_Exports st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_154_case__28 x1 x3 x2@(Curry.Module.CompactFlatCurry.C_InitFuncs x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_154_case__28 x1 x3 x2@(Curry.Module.CompactFlatCurry.C_Required x7) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_154_case__28 x1 x3 (Curry.Module.CompactFlatCurry.C_OptionOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_154_case__28(x1)(x3)(x)(st))(i)(xs)(st)
c__case_154_case__28 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_154_case__28")(x)



c__case_153_case__29 x1 x3 x2@(Curry.Module.CompactFlatCurry.C_InitFuncs x4) st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x4)(x3))(st)
c__case_153_case__29 x1 x3 x2@Curry.Module.CompactFlatCurry.C_Verbose st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_153_case__29 x1 x3 x2@(Curry.Module.CompactFlatCurry.C_Main x5) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_153_case__29 x1 x3 x2@Curry.Module.CompactFlatCurry.C_Exports st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_153_case__29 x1 x3 x2@(Curry.Module.CompactFlatCurry.C_Required x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_153_case__29 x1 x3 x2@(Curry.Module.CompactFlatCurry.C_Import x7) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_153_case__29 x1 x3 (Curry.Module.CompactFlatCurry.C_OptionOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_153_case__29(x1)(x3)(x)(st))(i)(xs)(st)
c__case_153_case__29 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_153_case__29")(x)



c__case_152_case__30 x1 x2@(Curry.Module.FlatCurry.C_Func x3 x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_152_case__30 x1 (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_152_case__30(x1)(x)(st))(i)(xs)(st)
c__case_152_case__30 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_152_case__30")(x)



c__case_151_case__31 x1 x2@(Curry.Module.FlatCurry.C_Func x3 x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_61_61(x5)(Curry.Module.FlatCurry.C_Public)(x1)(st))(st)
c__case_151_case__31 x1 (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_151_case__31(x1)(x)(st))(i)(xs)(st)
c__case_151_case__31 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_151_case__31")(x)



c__case_146_case__32 x1 x2 x3 x4 x5 x6 x7 x8 x11 x12 x13 x21@Curry.Module.Prelude.C_True st = let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List))))(let {x14 = Curry.Module.OracleMaybe.c_fromJust(Curry.Module.Oracle.c_apply(Curry.Module.OracleTableRBT.c_lookupRBT(Curry.Module.Prelude.T2(x12)(x13))(x1)(st))(x5)(x22)(st))(x23)(st)} in let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x24)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))(let {x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x25)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List))(let {x16 = Curry.Module.OraclePrelude.c_filter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_getCalledFuncs'46_'35lambda35(x6)))))(Curry.Module.OracleCompactFlatCurry.c_allFuncCalls(x14)(x24)(st))(x25)(st)} in let {x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x26)((Curry.Module.Prelude.:<)(x27)((Curry.Module.Prelude.:<)(x28)(Curry.Module.Prelude.List)))(let {x17 = Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_getImplicitlyRequired(x2)))))(x26)(st))(x16)(x27)(st)} in let {x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x28)((Curry.Module.Prelude.:<)(x29)(Curry.Module.Prelude.List))(let {x18 = Curry.Module.OracleCompactFlatCurry.c_allConstructorsOfFunc(x14)(x28)(st)} in let {x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x29)((Curry.Module.Prelude.:<)(x30)(Curry.Module.Prelude.List))(let {x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x30)((Curry.Module.Prelude.:<)(x31)(Curry.Module.Prelude.List))(let {x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x39 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x40 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x41 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x42 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x31)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)((Curry.Module.Prelude.:<)(x34)((Curry.Module.Prelude.:<)(x35)((Curry.Module.Prelude.:<)(x36)((Curry.Module.Prelude.:<)(x37)((Curry.Module.Prelude.:<)(x38)((Curry.Module.Prelude.:<)(x39)((Curry.Module.Prelude.:<)(x40)((Curry.Module.Prelude.:<)(x41)((Curry.Module.Prelude.:<)(x42)(Curry.Module.Prelude.List))))))))))))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleCompactFlatCurry.c_getCalledFuncs(x2)(x3)(x4)(x5)(Curry.Module.OraclePrelude.c_foldr(Curry.Module.OracleSetRBT.c_insertRBT(x31)(st))(x6)(Curry.Module.OraclePrelude.op_43_43(x16)(x17)(x32)(st))(x33)(st))(Curry.Module.OraclePrelude.c_foldr(Curry.Module.OracleSetRBT.c_insertRBT(x34)(st))(x7)(x18)(x35)(st))(Curry.Module.OraclePrelude.c_foldr(Curry.Module.OracleSetRBT.c_insertRBT(x36)(st))(x8)(Curry.Module.OracleCompactFlatCurry.c_allTypesOfFunc(x14)(x30)(st))(x37)(st))(Curry.Module.OraclePrelude.op_43_43(x11)(Curry.Module.OraclePrelude.op_43_43(x16)(Curry.Module.OraclePrelude.op_43_43(x17)(Curry.Module.OraclePrelude.c_filter(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_getCalledFuncs'46_'35lambda36(x7)))))(x18)(x29)(st))(x38)(st))(x39)(st))(x40)(st))(x41)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_getCalledFuncs'46_'35lambda37(x14)))))(x42)(st))(st))(st))(st))(st))(st))(st))(st))(st)
c__case_146_case__32 x1 x2 x3 x4 x5 x6 x7 x8 x11 x12 x13 x21@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_146_case__32 x1 x2 x3 x4 x5 x6 x7 x8 x11 x12 x13 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_146_case__32(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x11)(x12)(x13)(x)(st))(i)(xs)(st)
c__case_146_case__32 x1 x2 x3 x4 x5 x6 x7 x8 x11 x12 x13 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_146_case__32")(x)



c__case_147_case__33 x1 x2 x3 x4 x5 x6 x7 x8 x11 x12 x13 x14@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c_getCalledFuncs(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x11)(x1)(st))(st)
c__case_147_case__33 x1 x2 x3 x4 x5 x6 x7 x8 x11 x12 x13 x14@Curry.Module.Prelude.C_False st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(Curry.Module.OracleCompactFlatCurry.c__case_146(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x11)(x12)(x13)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x15)(st))(st)
c__case_147_case__33 x1 x2 x3 x4 x5 x6 x7 x8 x11 x12 x13 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_147_case__33(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x11)(x12)(x13)(x)(st))(i)(xs)(st)
c__case_147_case__33 x1 x2 x3 x4 x5 x6 x7 x8 x11 x12 x13 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_147_case__33")(x)



c__case_148_case__34 x1 x2 x3 x4 x5 x6 x7 x8 x11 x12 x13 x14@Curry.Module.Prelude.C_True st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleCompactFlatCurry.c_readCurrentFlatCurry(x12)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_getCalledFuncs'46_'35lambda34(x13)(x11)(x5)(x7)(x6)(x3)(x8)(x12)(x4)(x2)))))(x15)(st))(st)
c__case_148_case__34 x1 x2 x3 x4 x5 x6 x7 x8 x11 x12 x13 x14@Curry.Module.Prelude.C_False st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))))(Curry.Module.OracleCompactFlatCurry.c__case_147(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x11)(x12)(x13)(Curry.Module.OraclePrelude.op_61_61(Curry.Module.Oracle.c_apply(Curry.Module.OracleTableRBT.c_lookupRBT(Curry.Module.Prelude.T2(x12)(x13))(x1)(st))(x5)(x16)(st))(Curry.Module.Prelude.C_Nothing)(x17)(st))(x18)(st))(st)
c__case_148_case__34 x1 x2 x3 x4 x5 x6 x7 x8 x11 x12 x13 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_148_case__34(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x11)(x12)(x13)(x)(st))(i)(xs)(st)
c__case_148_case__34 x1 x2 x3 x4 x5 x6 x7 x8 x11 x12 x13 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_148_case__34")(x)



c__case_149_case__35 x1 x2 x3 x4 x5 x6 x7 x8 x11 x10@(Curry.Module.Prelude.T2 x12 x13) st = let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))))(Curry.Module.OracleCompactFlatCurry.c__case_148(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x11)(x12)(x13)(Curry.Module.OraclePrelude.c_not(Curry.Module.Oracle.c_apply(Curry.Module.OracleSetRBT.c_elemRBT(x12)(x1)(st))(x3)(x14)(st))(x15)(st))(x16)(st))(st)
c__case_149_case__35 x1 x2 x3 x4 x5 x6 x7 x8 x11 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_149_case__35(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x11)(x)(st))(i)(xs)(st)
c__case_149_case__35 x1 x2 x3 x4 x5 x6 x7 x8 x11 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_149_case__35")(x)



c__case_150_case__36 x1 x2 x3 x4 x5 x6 x7 x8 x9@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_return(Curry.Module.Prelude.T4(x4)(Curry.Module.Prelude.List)(x7)(x8))(x1)(st))(st)
c__case_150_case__36 x1 x2 x3 x4 x5 x6 x7 x8 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_149(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x11)(x10)(x1)(st))(st)
c__case_150_case__36 x1 x2 x3 x4 x5 x6 x7 x8 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_150_case__36(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_150_case__36 x1 x2 x3 x4 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_150_case__36")(x)



c__case_145_case__37 x1 x2 x3@(Curry.Module.Prelude.T4 x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_return(Curry.Module.Prelude.T4(x4)((Curry.Module.Prelude.:<)(x2)(x5))(x6)(x7))(x1)(st))(st)
c__case_145_case__37 x1 x2 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_145_case__37(x1)(x2)(x)(st))(i)(xs)(st)
c__case_145_case__37 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_145_case__37")(x)



c__case_143_case__38 x1 x7@(Curry.Module.FlatCurry.C_External x8) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_143_case__38 x1 x7@(Curry.Module.FlatCurry.C_Rule x9 x10) st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleList.c_nub(Curry.Module.OracleCompactFlatCurry.c_allFuncCallsOfExpr(x10)(x1)(st))(x11)(st))(st)
c__case_143_case__38 x1 (Curry.Module.FlatCurry.C_RuleOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_143_case__38(x1)(x)(st))(i)(xs)(st)
c__case_143_case__38 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_143_case__38")(x)



c__case_144_case__39 x1 x2@(Curry.Module.FlatCurry.C_Func x3 x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_143(x7)(x1)(st))(st)
c__case_144_case__39 x1 (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_144_case__39(x1)(x)(st))(i)(xs)(st)
c__case_144_case__39 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_144_case__39")(x)



c__case_141_case__40 x1 x6 x8 x5@Curry.Module.FlatCurry.C_FuncCall st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x6)(x8))(st)
c__case_141_case__40 x1 x6 x8 x5@(Curry.Module.FlatCurry.C_FuncPartCall x9) st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x6)(x8))(st)
c__case_141_case__40 x1 x6 x8 x5@Curry.Module.FlatCurry.C_ConsCall st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_141_case__40 x1 x6 x8 x5@(Curry.Module.FlatCurry.C_ConsPartCall x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_141_case__40 x1 x6 x8 (Curry.Module.FlatCurry.C_CombTypeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_141_case__40(x1)(x6)(x8)(x)(st))(i)(xs)(st)
c__case_141_case__40 x1 x6 x8 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_141_case__40")(x)



c__case_142_case__41 x1 x2@(Curry.Module.FlatCurry.C_Var x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_142_case__41 x1 x2@(Curry.Module.FlatCurry.C_Lit x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_142_case__41 x1 x2@(Curry.Module.FlatCurry.C_Comb x5 x6 x7) st = let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List)))(Curry.Module.CEventOracle.c_replace(x21)(Curry.Module.OracleCompactFlatCurry.c__case_141(x6)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_allFuncCallsOfExpr))))(x1)(st))(x7)(x20)(st))(x5)(x21)(st))(st))(st)
c__case_142_case__41 x1 x2@(Curry.Module.FlatCurry.C_Free x11 x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c_allFuncCallsOfExpr(x12)(x1)(st))(st)
c__case_142_case__41 x1 x2@(Curry.Module.FlatCurry.C_Let x13 x14) st = let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_allFuncCallsOfExpr))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_snd))))(x1)(st))(x22)(st))(x13)(x23)(st))(Curry.Module.OracleCompactFlatCurry.c_allFuncCallsOfExpr(x14)(x24)(st))(x25)(st))(st)
c__case_142_case__41 x1 x2@(Curry.Module.FlatCurry.C_Or x15 x16) st = let {x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleCompactFlatCurry.c_allFuncCallsOfExpr(x15)(x1)(st))(Curry.Module.OracleCompactFlatCurry.c_allFuncCallsOfExpr(x16)(x26)(st))(x27)(st))(st)
c__case_142_case__41 x1 x2@(Curry.Module.FlatCurry.C_Case x17 x18 x19) st = let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)((Curry.Module.Prelude.:<)(x30)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleCompactFlatCurry.c_allFuncCallsOfExpr(x18)(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_allFuncCallsOfBranchExpr))))(x28)(st))(x19)(x29)(st))(x30)(st))(st)
c__case_142_case__41 x1 (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_142_case__41(x1)(x)(st))(i)(xs)(st)
c__case_142_case__41 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_142_case__41")(x)



c__case_140_case__42 x1 x2@(Curry.Module.FlatCurry.C_Branch x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c_allFuncCallsOfExpr(x4)(x1)(st))(st)
c__case_140_case__42 x1 (Curry.Module.FlatCurry.C_BranchExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_140_case__42(x1)(x)(st))(i)(xs)(st)
c__case_140_case__42 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_140_case__42")(x)



c__case_138_case__43 x1 x7@(Curry.Module.FlatCurry.C_External x8) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_138_case__43 x1 x7@(Curry.Module.FlatCurry.C_Rule x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c_allConsOfExpr(x10)(x1)(st))(st)
c__case_138_case__43 x1 (Curry.Module.FlatCurry.C_RuleOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_138_case__43(x1)(x)(st))(i)(xs)(st)
c__case_138_case__43 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_138_case__43")(x)



c__case_139_case__44 x1 x2@(Curry.Module.FlatCurry.C_Func x3 x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_138(x7)(x1)(st))(st)
c__case_139_case__44 x1 (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_139_case__44(x1)(x)(st))(i)(xs)(st)
c__case_139_case__44 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_139_case__44")(x)



c__case_136_case__45 x1 x6 x8 x5@Curry.Module.FlatCurry.C_ConsCall st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x6)(x8))(st)
c__case_136_case__45 x1 x6 x8 x5@(Curry.Module.FlatCurry.C_ConsPartCall x9) st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x6)(x8))(st)
c__case_136_case__45 x1 x6 x8 x5@Curry.Module.FlatCurry.C_FuncCall st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_136_case__45 x1 x6 x8 x5@(Curry.Module.FlatCurry.C_FuncPartCall x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_136_case__45 x1 x6 x8 (Curry.Module.FlatCurry.C_CombTypeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_136_case__45(x1)(x6)(x8)(x)(st))(i)(xs)(st)
c__case_136_case__45 x1 x6 x8 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_136_case__45")(x)



c__case_137_case__46 x1 x2@(Curry.Module.FlatCurry.C_Var x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_137_case__46 x1 x2@(Curry.Module.FlatCurry.C_Lit x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_137_case__46 x1 x2@(Curry.Module.FlatCurry.C_Comb x5 x6 x7) st = let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List)))(Curry.Module.CEventOracle.c_replace(x21)(Curry.Module.OracleCompactFlatCurry.c__case_136(x6)(Curry.Module.Oracle.c_apply(Curry.Module.OracleCompactFlatCurry.c_unionMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_allConsOfExpr))))(x1)(st))(x7)(x20)(st))(x5)(x21)(st))(st))(st)
c__case_137_case__46 x1 x2@(Curry.Module.FlatCurry.C_Free x11 x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c_allConsOfExpr(x12)(x1)(st))(st)
c__case_137_case__46 x1 x2@(Curry.Module.FlatCurry.C_Let x13 x14) st = let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List)))))(Curry.Module.OracleList.c_union(Curry.Module.Oracle.c_apply(Curry.Module.OracleCompactFlatCurry.c_unionMap(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_allConsOfExpr))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_snd))))(x1)(st))(x22)(st))(x13)(x23)(st))(Curry.Module.OracleCompactFlatCurry.c_allConsOfExpr(x14)(x24)(st))(x25)(st))(st)
c__case_137_case__46 x1 x2@(Curry.Module.FlatCurry.C_Or x15 x16) st = let {x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List)))(Curry.Module.OracleList.c_union(Curry.Module.OracleCompactFlatCurry.c_allConsOfExpr(x15)(x1)(st))(Curry.Module.OracleCompactFlatCurry.c_allConsOfExpr(x16)(x26)(st))(x27)(st))(st)
c__case_137_case__46 x1 x2@(Curry.Module.FlatCurry.C_Case x17 x18 x19) st = let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)((Curry.Module.Prelude.:<)(x30)(Curry.Module.Prelude.List))))(Curry.Module.OracleList.c_union(Curry.Module.OracleCompactFlatCurry.c_allConsOfExpr(x18)(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OracleCompactFlatCurry.c_unionMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_allConsOfExpr'46consOfBranch'46252))))(x28)(st))(x19)(x29)(st))(x30)(st))(st)
c__case_137_case__46 x1 (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_137_case__46(x1)(x)(st))(i)(xs)(st)
c__case_137_case__46 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_137_case__46")(x)



c__case_134_case__47 x1 x4 x3@(Curry.Module.FlatCurry.C_LPattern x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c_allConsOfExpr(x4)(x1)(st))(st)
c__case_134_case__47 x1 x4 x3@(Curry.Module.FlatCurry.C_Pattern x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleList.c_union((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleCompactFlatCurry.c_allConsOfExpr(x4)(x1)(st))(x8)(st))(st)
c__case_134_case__47 x1 x4 (Curry.Module.FlatCurry.C_PatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_134_case__47(x1)(x4)(x)(st))(i)(xs)(st)
c__case_134_case__47 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_134_case__47")(x)



c__case_135_case__48 x1 x2@(Curry.Module.FlatCurry.C_Branch x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_134(x4)(x3)(x1)(st))(st)
c__case_135_case__48 x1 (Curry.Module.FlatCurry.C_BranchExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_135_case__48(x1)(x)(st))(i)(xs)(st)
c__case_135_case__48 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_135_case__48")(x)



c__case_133_case__49 x1 x2@(Curry.Module.FlatCurry.C_Func x3 x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c_allTypesOfTExpr(x6)(x1)(st))(st)
c__case_133_case__49 x1 (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_133_case__49(x1)(x)(st))(i)(xs)(st)
c__case_133_case__49 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_133_case__49")(x)



c__case_132_case__50 x1 x2@(Curry.Module.FlatCurry.C_TVar x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_132_case__50 x1 x2@(Curry.Module.FlatCurry.C_FuncType x4 x5) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OracleList.c_union(Curry.Module.OracleCompactFlatCurry.c_allTypesOfTExpr(x4)(x1)(st))(Curry.Module.OracleCompactFlatCurry.c_allTypesOfTExpr(x5)(x8)(st))(x9)(st))(st)
c__case_132_case__50 x1 x2@(Curry.Module.FlatCurry.C_TCons x6 x7) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))(Curry.Module.OracleList.c_union((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OracleCompactFlatCurry.c_unionMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_allTypesOfTExpr))))(x1)(st))(x7)(x10)(st))(x11)(st))(st)
c__case_132_case__50 x1 (Curry.Module.FlatCurry.C_TypeExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_132_case__50(x1)(x)(st))(i)(xs)(st)
c__case_132_case__50 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_132_case__50")(x)



c__case_131_case__51 x1 x2@(Curry.Module.FlatCurry.C_Func x3 x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_131_case__51 x1 (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_131_case__51(x1)(x)(st))(i)(xs)(st)
c__case_131_case__51 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_131_case__51")(x)



c__case_130_case__52 x1 x2@(Curry.Module.FlatCurry.C_Cons x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_130_case__52 x1 (Curry.Module.FlatCurry.C_ConsDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_130_case__52(x1)(x)(st))(i)(xs)(st)
c__case_130_case__52 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_130_case__52")(x)



c__case_129_case__53 x1 x2@(Curry.Module.FlatCurry.C_Type x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_129_case__53 x1 x2@(Curry.Module.FlatCurry.C_TypeSyn x7 x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x7)(st)
c__case_129_case__53 x1 (Curry.Module.FlatCurry.C_TypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_129_case__53(x1)(x)(st))(i)(xs)(st)
c__case_129_case__53 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_129_case__53")(x)



c__case_128_case__54 x1 x2@(Curry.Module.FlatCurry.C_Prog x3 x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_128_case__54 x1 (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_128_case__54(x1)(x)(st))(i)(xs)(st)
c__case_128_case__54 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_128_case__54")(x)



c__case_127_case__55 x1 x2@(Curry.Module.FlatCurry.C_Prog x3 x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_127_case__55 x1 (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_127_case__55(x1)(x)(st))(i)(xs)(st)
c__case_127_case__55 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_127_case__55")(x)



c__case_126_case__56 x1 x2@(Curry.Module.FlatCurry.C_Prog x3 x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_collapse(x1)(x7)(st)
c__case_126_case__56 x1 (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_126_case__56(x1)(x)(st))(i)(xs)(st)
c__case_126_case__56 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_126_case__56")(x)



c__case_125_case__57 x1 x2@(Curry.Module.FlatCurry.C_Prog x3 x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_125_case__57 x1 (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_125_case__57(x1)(x)(st))(i)(xs)(st)
c__case_125_case__57 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_125_case__57")(x)



c__case_124_case__58 x1 x2@(Curry.Module.FlatCurry.C_Prog x3 x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_124_case__58 x1 (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_124_case__58(x1)(x)(st))(i)(xs)(st)
c__case_124_case__58 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_124_case__58")(x)



c__case_122_case__59 x1 x4 x5 x3@(Curry.Module.Prelude.T2 x6 x7) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))))(let {x8 = Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OracleSort.c_cmpString(x1)(st))(x4)(x9)(st))(x6)(x10)(st)} in let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_61_61(x8)(Curry.Module.Prelude.C_LT)(x11)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x8)(Curry.Module.Prelude.C_EQ)(x12)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OracleSort.c_leqString(x13)(st))(x5)(x14)(st))(x7)(x15)(st))(x16)(st))(x17)(st))(st))(st)
c__case_122_case__59 x1 x4 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_122_case__59(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_122_case__59 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_122_case__59")(x)



c__case_123_case__60 x1 x3 x2@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_122(x4)(x5)(x3)(x1)(st))(st)
c__case_123_case__60 x1 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_123_case__60(x1)(x3)(x)(st))(i)(xs)(st)
c__case_123_case__60 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_123_case__60")(x)



c__case_121_case__61 x1 x2 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleFlatCurry.c_readFlatCurry(x2)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_processPrimitives(x2)))))(x5)(st))(st)
c__case_121_case__61 x1 x2 x4@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleCompactFlatCurry.c_getSourceModificationTime(x2)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_readCurrentFlatCurry'46_'35lambda40'46_'35lambda41'46_'35lambda42(x2)))))(x6)(st))(st)
c__case_121_case__61 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_121_case__61(x1)(x2)(x)(st))(i)(xs)(st)
c__case_121_case__61 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_121_case__61")(x)



c__case_120_case__62 x1 x3 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleFlatCurry.c_readFlatCurry(x3)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_processPrimitives(x3)))))(x6)(st))(st)
c__case_120_case__62 x1 x3 x5@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleFlatCurry.c_readFlatCurryFile(Curry.Module.OracleFlatCurry.c_flatCurryFileName(x3)(x1)(st))(x7)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_processPrimitives(x3)))))(x8)(st))(st)
c__case_120_case__62 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_120_case__62(x1)(x3)(x)(st))(i)(xs)(st)
c__case_120_case__62 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_120_case__62")(x)



c__case_119_case__63 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleDirectory.c_getModificationTime(Curry.Module.OraclePrelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))(x1)(st))(x4)(st))(st)
c__case_119_case__63 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleDirectory.c_getModificationTime(Curry.Module.OraclePrelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(x1)(st))(x5)(st))(st)
c__case_119_case__63 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_119_case__63(x1)(x2)(x)(st))(i)(xs)(st)
c__case_119_case__63 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_119_case__63")(x)



c__case_118_case__64 x1 x2 x3@(Curry.Module.FlatCurry.C_Prog x4 x5 x6 x7 x8) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.FlatCurry.C_Prog(x4)(x5)(x6)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_mergePrimSpecIntoFunc(x2)))))(x1)(st))(x7)(x9)(st))(x8))(st)
c__case_118_case__64 x1 x2 (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_118_case__64(x1)(x2)(x)(st))(i)(xs)(st)
c__case_118_case__64 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_118_case__64")(x)



c__case_115_case__65 x1 x4 x5 x6 x7 x11 x12 x13@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_115_case__65 x1 x4 x5 x6 x7 x11 x12 x13@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.FlatCurry.C_Func(x4)(x5)(x6)(x7)(Curry.Module.FlatCurry.C_External(Curry.Module.OraclePrelude.op_43_43(x11)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(x12))(x1)(st))))(Curry.Module.Prelude.List))(st)
c__case_115_case__65 x1 x4 x5 x6 x7 x11 x12 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_115_case__65(x1)(x4)(x5)(x6)(x7)(x11)(x12)(x)(st))(i)(xs)(st)
c__case_115_case__65 x1 x4 x5 x6 x7 x11 x12 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_115_case__65")(x)



c__case_116_case__66 x1 x4 x5 x6 x7 x8 x9 x13@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.FlatCurry.C_Func(x4)(x5)(x6)(x7)(x8))(Curry.Module.Prelude.List))(st)
c__case_116_case__66 x1 x4 x5 x6 x7 x8 x9 x13@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(let {x12 = Curry.Module.OracleCompactFlatCurry.c_mergePrimSpecIntoFunc'46_'35selFP4'35entry(x9)(x14)(st)} in let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(Curry.Module.OracleCompactFlatCurry.c__case_115(x4)(x5)(x6)(x7)(Curry.Module.OracleCompactFlatCurry.c_mergePrimSpecIntoFunc'46_'35selFP3'35lib(x9)(x1)(st))(x12)(Curry.Module.OraclePrelude.c_null(x12)(x15)(st))(x16)(st))(st))(st))(st))(st)
c__case_116_case__66 x1 x4 x5 x6 x7 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_116_case__66(x1)(x4)(x5)(x6)(x7)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_116_case__66 x1 x4 x5 x6 x7 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_116_case__66")(x)



c__case_117_case__67 x1 x2 x3@(Curry.Module.FlatCurry.C_Func x4 x5 x6 x7 x8) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(let {x9 = Curry.Module.OraclePrelude.c_lookup(x4)(x2)(x1)(st)} in let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleCompactFlatCurry.c__case_116(x4)(x5)(x6)(x7)(x8)(x9)(Curry.Module.OraclePrelude.op_61_61(x9)(Curry.Module.Prelude.C_Nothing)(x10)(st))(x11)(st))(st))(st)
c__case_117_case__67 x1 x2 (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_117_case__67(x1)(x2)(x)(st))(i)(xs)(st)
c__case_117_case__67 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_117_case__67")(x)



c__case_113_case__68 x1 x3@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_113_case__68 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_113_case__68(x1)(x)(st))(i)(xs)(st)
c__case_113_case__68 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_113_case__68")(x)



c__case_114_case__69 x1 x2@(Curry.Module.Prelude.C_Just x3) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_113(x3)(x1)(st))(st)
c__case_114_case__69 x1 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_114_case__69(x1)(x)(st))(i)(xs)(st)
c__case_114_case__69 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_114_case__69")(x)



c__case_111_case__70 x1 x3@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_111_case__70 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_111_case__70(x1)(x)(st))(i)(xs)(st)
c__case_111_case__70 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_111_case__70")(x)



c__case_112_case__71 x1 x2@(Curry.Module.Prelude.C_Just x3) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_111(x3)(x1)(st))(st)
c__case_112_case__71 x1 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_112_case__71(x1)(x)(st))(i)(xs)(st)
c__case_112_case__71 x1 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_112_case__71")(x)



c__case_110_case__72 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleXML.c_readXmlFile(x3)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_readPrimSpec'46_'35lambda48'46_'35lambda49(x2)))))(x5)(st))(st)
c__case_110_case__72 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_return(Curry.Module.Prelude.List)(x1)(st))(st)
c__case_110_case__72 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_110_case__72(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_110_case__72 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_110_case__72")(x)



c__case_87_case__73 x1 x2 x6 x5@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCompactFlatCurry.c_xml2primtrans'46xml2prim'46358(x2)))))(x6)(x1)(st))(st)
c__case_87_case__73 x1 x2 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_87_case__73(x1)(x2)(x6)(x)(st))(i)(xs)(st)
c__case_87_case__73 x1 x2 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_87_case__73")(x)



c__case_88_case__74 x1 x2 x5 x6 x26@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_87(x2)(x6)(x5)(x1)(st))(st)
c__case_88_case__74 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_88_case__74(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_88_case__74 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_88_case__74")(x)



c__case_89_case__75 x1 x2 x5 x6 x26 x25 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x25)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_88(x2)(x5)(x6)(x26)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_90_case__76 x1 x2 x5 x6 x24@((Curry.Module.Prelude.:<) x25 x26) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_89(x2)(x5)(x6)(x26)(x25)(x1)(st))(st)
c__case_90_case__76 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_90_case__76(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_90_case__76 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_90_case__76")(x)



c__case_91_case__77 x1 x2 x5 x6 x24 x23 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x23)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_90(x2)(x5)(x6)(x24)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_92_case__78 x1 x2 x5 x6 x22@((Curry.Module.Prelude.:<) x23 x24) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_91(x2)(x5)(x6)(x24)(x23)(x1)(st))(st)
c__case_92_case__78 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_92_case__78(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_92_case__78 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_92_case__78")(x)



c__case_93_case__79 x1 x2 x5 x6 x22 x21 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x21)(Curry.Module.Prelude.C_Char('v'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_92(x2)(x5)(x6)(x22)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_94_case__80 x1 x2 x5 x6 x20@((Curry.Module.Prelude.:<) x21 x22) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_93(x2)(x5)(x6)(x22)(x21)(x1)(st))(st)
c__case_94_case__80 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_94_case__80(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_94_case__80 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_94_case__80")(x)



c__case_95_case__81 x1 x2 x5 x6 x20 x19 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x19)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_94(x2)(x5)(x6)(x20)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_96_case__82 x1 x2 x5 x6 x18@((Curry.Module.Prelude.:<) x19 x20) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_95(x2)(x5)(x6)(x20)(x19)(x1)(st))(st)
c__case_96_case__82 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_96_case__82(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_96_case__82 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_96_case__82")(x)



c__case_97_case__83 x1 x2 x5 x6 x18 x17 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x17)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_96(x2)(x5)(x6)(x18)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_98_case__84 x1 x2 x5 x6 x16@((Curry.Module.Prelude.:<) x17 x18) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_97(x2)(x5)(x6)(x18)(x17)(x1)(st))(st)
c__case_98_case__84 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_98_case__84(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_98_case__84 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_98_case__84")(x)



c__case_99_case__85 x1 x2 x5 x6 x16 x15 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x15)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_98(x2)(x5)(x6)(x16)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_100_case__86 x1 x2 x5 x6 x14@((Curry.Module.Prelude.:<) x15 x16) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_99(x2)(x5)(x6)(x16)(x15)(x1)(st))(st)
c__case_100_case__86 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_100_case__86(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_100_case__86 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_100_case__86")(x)



c__case_101_case__87 x1 x2 x5 x6 x14 x13 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x13)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_100(x2)(x5)(x6)(x14)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_102_case__88 x1 x2 x5 x6 x12@((Curry.Module.Prelude.:<) x13 x14) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_101(x2)(x5)(x6)(x14)(x13)(x1)(st))(st)
c__case_102_case__88 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_102_case__88(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_102_case__88 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_102_case__88")(x)



c__case_103_case__89 x1 x2 x5 x6 x12 x11 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x11)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_102(x2)(x5)(x6)(x12)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_104_case__90 x1 x2 x5 x6 x10@((Curry.Module.Prelude.:<) x11 x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_103(x2)(x5)(x6)(x12)(x11)(x1)(st))(st)
c__case_104_case__90 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_104_case__90(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_104_case__90 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_104_case__90")(x)



c__case_105_case__91 x1 x2 x5 x6 x10 x9 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x9)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_104(x2)(x5)(x6)(x10)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_106_case__92 x1 x2 x5 x6 x8@((Curry.Module.Prelude.:<) x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_105(x2)(x5)(x6)(x10)(x9)(x1)(st))(st)
c__case_106_case__92 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_106_case__92(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_106_case__92 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_106_case__92")(x)



c__case_107_case__93 x1 x2 x5 x6 x8 x7 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x7)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_106(x2)(x5)(x6)(x8)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_108_case__94 x1 x2 x5 x6 x4@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_107(x2)(x5)(x6)(x8)(x7)(x1)(st))(st)
c__case_108_case__94 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_108_case__94(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_108_case__94 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_108_case__94")(x)



c__case_109_case__95 x1 x2 x3@(Curry.Module.XML.C_XElem x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_108(x2)(x5)(x6)(x4)(x1)(st))(st)
c__case_109_case__95 x1 x2 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_109_case__95(x1)(x2)(x)(st))(i)(xs)(st)
c__case_109_case__95 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_109_case__95")(x)



c__case_23_case__96 x1 x2 x28 x41 x60 x57@Curry.Module.Prelude.List st = let {x61 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x61)(Curry.Module.Prelude.List))(Curry.Module.Prelude.T2(Curry.Module.Prelude.T2(x2)(x28))(Curry.Module.Prelude.T2(Curry.Module.OracleXML.c_textOfXml(x41)(x1)(st))(Curry.Module.OracleXML.c_textOfXml(x60)(x61)(st))))(st)
c__case_23_case__96 x1 x2 x28 x41 x60 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_23_case__96(x1)(x2)(x28)(x41)(x60)(x)(st))(i)(xs)(st)
c__case_23_case__96 x1 x2 x28 x41 x60 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_23_case__96")(x)



c__case_24_case__97 x1 x2 x28 x41 x57 x60 x59@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_23(x2)(x28)(x41)(x60)(x57)(x1)(st))(st)
c__case_24_case__97 x1 x2 x28 x41 x57 x60 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_24_case__97(x1)(x2)(x28)(x41)(x57)(x60)(x)(st))(i)(xs)(st)
c__case_24_case__97 x1 x2 x28 x41 x57 x60 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_24_case__97")(x)



c__case_25_case__98 x1 x2 x28 x41 x57 x59 x60 x70@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_24(x2)(x28)(x41)(x57)(x60)(x59)(x1)(st))(st)
c__case_25_case__98 x1 x2 x28 x41 x57 x59 x60 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_25_case__98(x1)(x2)(x28)(x41)(x57)(x59)(x60)(x)(st))(i)(xs)(st)
c__case_25_case__98 x1 x2 x28 x41 x57 x59 x60 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_25_case__98")(x)



c__case_26_case__99 x1 x2 x28 x41 x57 x59 x60 x70 x69 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x69)(Curry.Module.Prelude.C_Char('y'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_25(x2)(x28)(x41)(x57)(x59)(x60)(x70)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_27_case__100 x1 x2 x28 x41 x57 x59 x60 x68@((Curry.Module.Prelude.:<) x69 x70) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_26(x2)(x28)(x41)(x57)(x59)(x60)(x70)(x69)(x1)(st))(st)
c__case_27_case__100 x1 x2 x28 x41 x57 x59 x60 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_27_case__100(x1)(x2)(x28)(x41)(x57)(x59)(x60)(x)(st))(i)(xs)(st)
c__case_27_case__100 x1 x2 x28 x41 x57 x59 x60 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_27_case__100")(x)



c__case_28_case__101 x1 x2 x28 x41 x57 x59 x60 x68 x67 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x67)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_27(x2)(x28)(x41)(x57)(x59)(x60)(x68)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_29_case__102 x1 x2 x28 x41 x57 x59 x60 x66@((Curry.Module.Prelude.:<) x67 x68) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_28(x2)(x28)(x41)(x57)(x59)(x60)(x68)(x67)(x1)(st))(st)
c__case_29_case__102 x1 x2 x28 x41 x57 x59 x60 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_29_case__102(x1)(x2)(x28)(x41)(x57)(x59)(x60)(x)(st))(i)(xs)(st)
c__case_29_case__102 x1 x2 x28 x41 x57 x59 x60 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_29_case__102")(x)



c__case_30_case__103 x1 x2 x28 x41 x57 x59 x60 x66 x65 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x65)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_29(x2)(x28)(x41)(x57)(x59)(x60)(x66)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_31_case__104 x1 x2 x28 x41 x57 x59 x60 x64@((Curry.Module.Prelude.:<) x65 x66) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_30(x2)(x28)(x41)(x57)(x59)(x60)(x66)(x65)(x1)(st))(st)
c__case_31_case__104 x1 x2 x28 x41 x57 x59 x60 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_31_case__104(x1)(x2)(x28)(x41)(x57)(x59)(x60)(x)(st))(i)(xs)(st)
c__case_31_case__104 x1 x2 x28 x41 x57 x59 x60 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_31_case__104")(x)



c__case_32_case__105 x1 x2 x28 x41 x57 x59 x60 x64 x63 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x63)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_31(x2)(x28)(x41)(x57)(x59)(x60)(x64)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_33_case__106 x1 x2 x28 x41 x57 x59 x60 x62@((Curry.Module.Prelude.:<) x63 x64) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_32(x2)(x28)(x41)(x57)(x59)(x60)(x64)(x63)(x1)(st))(st)
c__case_33_case__106 x1 x2 x28 x41 x57 x59 x60 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_33_case__106(x1)(x2)(x28)(x41)(x57)(x59)(x60)(x)(st))(i)(xs)(st)
c__case_33_case__106 x1 x2 x28 x41 x57 x59 x60 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_33_case__106")(x)



c__case_34_case__107 x1 x2 x28 x41 x57 x59 x60 x62 x61 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x61)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_33(x2)(x28)(x41)(x57)(x59)(x60)(x62)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_35_case__108 x1 x2 x28 x41 x57 x59 x60 x58@((Curry.Module.Prelude.:<) x61 x62) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_34(x2)(x28)(x41)(x57)(x59)(x60)(x62)(x61)(x1)(st))(st)
c__case_35_case__108 x1 x2 x28 x41 x57 x59 x60 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_35_case__108(x1)(x2)(x28)(x41)(x57)(x59)(x60)(x)(st))(i)(xs)(st)
c__case_35_case__108 x1 x2 x28 x41 x57 x59 x60 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_35_case__108")(x)



c__case_36_case__109 x1 x2 x28 x41 x57 x56@(Curry.Module.XML.C_XElem x58 x59 x60) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_35(x2)(x28)(x41)(x57)(x59)(x60)(x58)(x1)(st))(st)
c__case_36_case__109 x1 x2 x28 x41 x57 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_36_case__109(x1)(x2)(x28)(x41)(x57)(x)(st))(i)(xs)(st)
c__case_36_case__109 x1 x2 x28 x41 x57 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_36_case__109")(x)



c__case_37_case__110 x1 x2 x28 x41 x38@((Curry.Module.Prelude.:<) x56 x57) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_36(x2)(x28)(x41)(x57)(x56)(x1)(st))(st)
c__case_37_case__110 x1 x2 x28 x41 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_37_case__110(x1)(x2)(x28)(x41)(x)(st))(i)(xs)(st)
c__case_37_case__110 x1 x2 x28 x41 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_37_case__110")(x)



c__case_38_case__111 x1 x2 x28 x38 x41 x40@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_37(x2)(x28)(x41)(x38)(x1)(st))(st)
c__case_38_case__111 x1 x2 x28 x38 x41 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_38_case__111(x1)(x2)(x28)(x38)(x41)(x)(st))(i)(xs)(st)
c__case_38_case__111 x1 x2 x28 x38 x41 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_38_case__111")(x)



c__case_39_case__112 x1 x2 x28 x38 x40 x41 x55@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_38(x2)(x28)(x38)(x41)(x40)(x1)(st))(st)
c__case_39_case__112 x1 x2 x28 x38 x40 x41 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_39_case__112(x1)(x2)(x28)(x38)(x40)(x41)(x)(st))(i)(xs)(st)
c__case_39_case__112 x1 x2 x28 x38 x40 x41 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_39_case__112")(x)



c__case_40_case__113 x1 x2 x28 x38 x40 x41 x55 x54 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x54)(Curry.Module.Prelude.C_Char('y'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_39(x2)(x28)(x38)(x40)(x41)(x55)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_41_case__114 x1 x2 x28 x38 x40 x41 x53@((Curry.Module.Prelude.:<) x54 x55) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_40(x2)(x28)(x38)(x40)(x41)(x55)(x54)(x1)(st))(st)
c__case_41_case__114 x1 x2 x28 x38 x40 x41 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_41_case__114(x1)(x2)(x28)(x38)(x40)(x41)(x)(st))(i)(xs)(st)
c__case_41_case__114 x1 x2 x28 x38 x40 x41 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_41_case__114")(x)



c__case_42_case__115 x1 x2 x28 x38 x40 x41 x53 x52 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x52)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_41(x2)(x28)(x38)(x40)(x41)(x53)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_43_case__116 x1 x2 x28 x38 x40 x41 x51@((Curry.Module.Prelude.:<) x52 x53) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_42(x2)(x28)(x38)(x40)(x41)(x53)(x52)(x1)(st))(st)
c__case_43_case__116 x1 x2 x28 x38 x40 x41 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_43_case__116(x1)(x2)(x28)(x38)(x40)(x41)(x)(st))(i)(xs)(st)
c__case_43_case__116 x1 x2 x28 x38 x40 x41 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_43_case__116")(x)



c__case_44_case__117 x1 x2 x28 x38 x40 x41 x51 x50 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x50)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_43(x2)(x28)(x38)(x40)(x41)(x51)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_45_case__118 x1 x2 x28 x38 x40 x41 x49@((Curry.Module.Prelude.:<) x50 x51) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_44(x2)(x28)(x38)(x40)(x41)(x51)(x50)(x1)(st))(st)
c__case_45_case__118 x1 x2 x28 x38 x40 x41 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_45_case__118(x1)(x2)(x28)(x38)(x40)(x41)(x)(st))(i)(xs)(st)
c__case_45_case__118 x1 x2 x28 x38 x40 x41 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_45_case__118")(x)



c__case_46_case__119 x1 x2 x28 x38 x40 x41 x49 x48 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x48)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_45(x2)(x28)(x38)(x40)(x41)(x49)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_47_case__120 x1 x2 x28 x38 x40 x41 x47@((Curry.Module.Prelude.:<) x48 x49) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_46(x2)(x28)(x38)(x40)(x41)(x49)(x48)(x1)(st))(st)
c__case_47_case__120 x1 x2 x28 x38 x40 x41 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_47_case__120(x1)(x2)(x28)(x38)(x40)(x41)(x)(st))(i)(xs)(st)
c__case_47_case__120 x1 x2 x28 x38 x40 x41 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_47_case__120")(x)



c__case_48_case__121 x1 x2 x28 x38 x40 x41 x47 x46 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x46)(Curry.Module.Prelude.C_Char('b'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_47(x2)(x28)(x38)(x40)(x41)(x47)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_49_case__122 x1 x2 x28 x38 x40 x41 x45@((Curry.Module.Prelude.:<) x46 x47) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_48(x2)(x28)(x38)(x40)(x41)(x47)(x46)(x1)(st))(st)
c__case_49_case__122 x1 x2 x28 x38 x40 x41 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_49_case__122(x1)(x2)(x28)(x38)(x40)(x41)(x)(st))(i)(xs)(st)
c__case_49_case__122 x1 x2 x28 x38 x40 x41 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_49_case__122")(x)



c__case_50_case__123 x1 x2 x28 x38 x40 x41 x45 x44 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x44)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_49(x2)(x28)(x38)(x40)(x41)(x45)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_51_case__124 x1 x2 x28 x38 x40 x41 x43@((Curry.Module.Prelude.:<) x44 x45) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_50(x2)(x28)(x38)(x40)(x41)(x45)(x44)(x1)(st))(st)
c__case_51_case__124 x1 x2 x28 x38 x40 x41 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_51_case__124(x1)(x2)(x28)(x38)(x40)(x41)(x)(st))(i)(xs)(st)
c__case_51_case__124 x1 x2 x28 x38 x40 x41 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_51_case__124")(x)



c__case_52_case__125 x1 x2 x28 x38 x40 x41 x43 x42 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x42)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_51(x2)(x28)(x38)(x40)(x41)(x43)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_53_case__126 x1 x2 x28 x38 x40 x41 x39@((Curry.Module.Prelude.:<) x42 x43) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_52(x2)(x28)(x38)(x40)(x41)(x43)(x42)(x1)(st))(st)
c__case_53_case__126 x1 x2 x28 x38 x40 x41 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_53_case__126(x1)(x2)(x28)(x38)(x40)(x41)(x)(st))(i)(xs)(st)
c__case_53_case__126 x1 x2 x28 x38 x40 x41 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_53_case__126")(x)



c__case_54_case__127 x1 x2 x28 x38 x37@(Curry.Module.XML.C_XElem x39 x40 x41) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_53(x2)(x28)(x38)(x40)(x41)(x39)(x1)(st))(st)
c__case_54_case__127 x1 x2 x28 x38 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_54_case__127(x1)(x2)(x28)(x38)(x)(st))(i)(xs)(st)
c__case_54_case__127 x1 x2 x28 x38 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_54_case__127")(x)



c__case_55_case__128 x1 x2 x28 x6@((Curry.Module.Prelude.:<) x37 x38) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_54(x2)(x28)(x38)(x37)(x1)(st))(st)
c__case_55_case__128 x1 x2 x28 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_55_case__128(x1)(x2)(x28)(x)(st))(i)(xs)(st)
c__case_55_case__128 x1 x2 x28 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_55_case__128")(x)



c__case_56_case__129 x1 x2 x6 x28 x36@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_55(x2)(x28)(x6)(x1)(st))(st)
c__case_56_case__129 x1 x2 x6 x28 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_56_case__129(x1)(x2)(x6)(x28)(x)(st))(i)(xs)(st)
c__case_56_case__129 x1 x2 x6 x28 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_56_case__129")(x)



c__case_57_case__130 x1 x2 x6 x28 x36 x35 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x35)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_56(x2)(x6)(x28)(x36)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_58_case__131 x1 x2 x6 x28 x34@((Curry.Module.Prelude.:<) x35 x36) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_57(x2)(x6)(x28)(x36)(x35)(x1)(st))(st)
c__case_58_case__131 x1 x2 x6 x28 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_58_case__131(x1)(x2)(x6)(x28)(x)(st))(i)(xs)(st)
c__case_58_case__131 x1 x2 x6 x28 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_58_case__131")(x)



c__case_59_case__132 x1 x2 x6 x28 x34 x33 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x33)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_58(x2)(x6)(x28)(x34)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_60_case__133 x1 x2 x6 x28 x32@((Curry.Module.Prelude.:<) x33 x34) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_59(x2)(x6)(x28)(x34)(x33)(x1)(st))(st)
c__case_60_case__133 x1 x2 x6 x28 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_60_case__133(x1)(x2)(x6)(x28)(x)(st))(i)(xs)(st)
c__case_60_case__133 x1 x2 x6 x28 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_60_case__133")(x)



c__case_61_case__134 x1 x2 x6 x28 x32 x31 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x31)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_60(x2)(x6)(x28)(x32)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_62_case__135 x1 x2 x6 x28 x30@((Curry.Module.Prelude.:<) x31 x32) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_61(x2)(x6)(x28)(x32)(x31)(x1)(st))(st)
c__case_62_case__135 x1 x2 x6 x28 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_62_case__135(x1)(x2)(x6)(x28)(x)(st))(i)(xs)(st)
c__case_62_case__135 x1 x2 x6 x28 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_62_case__135")(x)



c__case_63_case__136 x1 x2 x6 x28 x30 x29 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x29)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_62(x2)(x6)(x28)(x30)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_64_case__137 x1 x2 x6 x28 x27@((Curry.Module.Prelude.:<) x29 x30) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_63(x2)(x6)(x28)(x30)(x29)(x1)(st))(st)
c__case_64_case__137 x1 x2 x6 x28 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_64_case__137(x1)(x2)(x6)(x28)(x)(st))(i)(xs)(st)
c__case_64_case__137 x1 x2 x6 x28 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_64_case__137")(x)



c__case_65_case__138 x1 x2 x6 x25@(Curry.Module.Prelude.T2 x27 x28) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_64(x2)(x6)(x28)(x27)(x1)(st))(st)
c__case_65_case__138 x1 x2 x6 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_65_case__138(x1)(x2)(x6)(x)(st))(i)(xs)(st)
c__case_65_case__138 x1 x2 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_65_case__138")(x)



c__case_66_case__139 x1 x2 x6 x5@((Curry.Module.Prelude.:<) x25 x26) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_65(x2)(x6)(x25)(x1)(st))(st)
c__case_66_case__139 x1 x2 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_66_case__139(x1)(x2)(x6)(x)(st))(i)(xs)(st)
c__case_66_case__139 x1 x2 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_66_case__139")(x)



c__case_67_case__140 x1 x2 x5 x6 x24@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_66(x2)(x6)(x5)(x1)(st))(st)
c__case_67_case__140 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_67_case__140(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_67_case__140 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_67_case__140")(x)



c__case_68_case__141 x1 x2 x5 x6 x24 x23 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x23)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_67(x2)(x5)(x6)(x24)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_69_case__142 x1 x2 x5 x6 x22@((Curry.Module.Prelude.:<) x23 x24) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_68(x2)(x5)(x6)(x24)(x23)(x1)(st))(st)
c__case_69_case__142 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_69_case__142(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_69_case__142 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_69_case__142")(x)



c__case_70_case__143 x1 x2 x5 x6 x22 x21 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x21)(Curry.Module.Prelude.C_Char('v'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_69(x2)(x5)(x6)(x22)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_71_case__144 x1 x2 x5 x6 x20@((Curry.Module.Prelude.:<) x21 x22) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_70(x2)(x5)(x6)(x22)(x21)(x1)(st))(st)
c__case_71_case__144 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_71_case__144(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_71_case__144 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_71_case__144")(x)



c__case_72_case__145 x1 x2 x5 x6 x20 x19 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x19)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_71(x2)(x5)(x6)(x20)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_73_case__146 x1 x2 x5 x6 x18@((Curry.Module.Prelude.:<) x19 x20) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_72(x2)(x5)(x6)(x20)(x19)(x1)(st))(st)
c__case_73_case__146 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_73_case__146(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_73_case__146 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_73_case__146")(x)



c__case_74_case__147 x1 x2 x5 x6 x18 x17 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x17)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_73(x2)(x5)(x6)(x18)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_75_case__148 x1 x2 x5 x6 x16@((Curry.Module.Prelude.:<) x17 x18) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_74(x2)(x5)(x6)(x18)(x17)(x1)(st))(st)
c__case_75_case__148 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_75_case__148(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_75_case__148 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_75_case__148")(x)



c__case_76_case__149 x1 x2 x5 x6 x16 x15 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x15)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_75(x2)(x5)(x6)(x16)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_77_case__150 x1 x2 x5 x6 x14@((Curry.Module.Prelude.:<) x15 x16) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_76(x2)(x5)(x6)(x16)(x15)(x1)(st))(st)
c__case_77_case__150 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_77_case__150(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_77_case__150 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_77_case__150")(x)



c__case_78_case__151 x1 x2 x5 x6 x14 x13 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x13)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_77(x2)(x5)(x6)(x14)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_79_case__152 x1 x2 x5 x6 x12@((Curry.Module.Prelude.:<) x13 x14) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_78(x2)(x5)(x6)(x14)(x13)(x1)(st))(st)
c__case_79_case__152 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_79_case__152(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_79_case__152 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_79_case__152")(x)



c__case_80_case__153 x1 x2 x5 x6 x12 x11 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x11)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_79(x2)(x5)(x6)(x12)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_81_case__154 x1 x2 x5 x6 x10@((Curry.Module.Prelude.:<) x11 x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_80(x2)(x5)(x6)(x12)(x11)(x1)(st))(st)
c__case_81_case__154 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_81_case__154(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_81_case__154 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_81_case__154")(x)



c__case_82_case__155 x1 x2 x5 x6 x10 x9 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x9)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_81(x2)(x5)(x6)(x10)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_83_case__156 x1 x2 x5 x6 x8@((Curry.Module.Prelude.:<) x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_82(x2)(x5)(x6)(x10)(x9)(x1)(st))(st)
c__case_83_case__156 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_83_case__156(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_83_case__156 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_83_case__156")(x)



c__case_0_case__157 x1 x2 x84 x6@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.T2(x2)(x84))(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)))(st)
c__case_0_case__157 x1 x2 x84 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_0_case__157(x1)(x2)(x84)(x)(st))(i)(xs)(st)
c__case_0_case__157 x1 x2 x84 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_0_case__157")(x)



c__case_1_case__158 x1 x2 x6 x84 x92@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_0(x2)(x84)(x6)(x1)(st))(st)
c__case_1_case__158 x1 x2 x6 x84 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_1_case__158(x1)(x2)(x6)(x84)(x)(st))(i)(xs)(st)
c__case_1_case__158 x1 x2 x6 x84 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_1_case__158")(x)



c__case_2_case__159 x1 x2 x6 x84 x92 x91 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x91)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_1(x2)(x6)(x84)(x92)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_3_case__160 x1 x2 x6 x84 x90@((Curry.Module.Prelude.:<) x91 x92) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_2(x2)(x6)(x84)(x92)(x91)(x1)(st))(st)
c__case_3_case__160 x1 x2 x6 x84 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_3_case__160(x1)(x2)(x6)(x84)(x)(st))(i)(xs)(st)
c__case_3_case__160 x1 x2 x6 x84 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_3_case__160")(x)



c__case_4_case__161 x1 x2 x6 x84 x90 x89 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x89)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_3(x2)(x6)(x84)(x90)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_5_case__162 x1 x2 x6 x84 x88@((Curry.Module.Prelude.:<) x89 x90) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_4(x2)(x6)(x84)(x90)(x89)(x1)(st))(st)
c__case_5_case__162 x1 x2 x6 x84 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_5_case__162(x1)(x2)(x6)(x84)(x)(st))(i)(xs)(st)
c__case_5_case__162 x1 x2 x6 x84 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_5_case__162")(x)



c__case_6_case__163 x1 x2 x6 x84 x88 x87 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x87)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_5(x2)(x6)(x84)(x88)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_7_case__164 x1 x2 x6 x84 x86@((Curry.Module.Prelude.:<) x87 x88) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_6(x2)(x6)(x84)(x88)(x87)(x1)(st))(st)
c__case_7_case__164 x1 x2 x6 x84 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_7_case__164(x1)(x2)(x6)(x84)(x)(st))(i)(xs)(st)
c__case_7_case__164 x1 x2 x6 x84 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_7_case__164")(x)



c__case_8_case__165 x1 x2 x6 x84 x86 x85 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x85)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_7(x2)(x6)(x84)(x86)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_9_case__166 x1 x2 x6 x84 x83@((Curry.Module.Prelude.:<) x85 x86) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_8(x2)(x6)(x84)(x86)(x85)(x1)(st))(st)
c__case_9_case__166 x1 x2 x6 x84 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_9_case__166(x1)(x2)(x6)(x84)(x)(st))(i)(xs)(st)
c__case_9_case__166 x1 x2 x6 x84 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_9_case__166")(x)



c__case_10_case__167 x1 x2 x6 x81@(Curry.Module.Prelude.T2 x83 x84) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_9(x2)(x6)(x84)(x83)(x1)(st))(st)
c__case_10_case__167 x1 x2 x6 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_10_case__167(x1)(x2)(x6)(x)(st))(i)(xs)(st)
c__case_10_case__167 x1 x2 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_10_case__167")(x)



c__case_11_case__168 x1 x2 x6 x5@((Curry.Module.Prelude.:<) x81 x82) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_10(x2)(x6)(x81)(x1)(st))(st)
c__case_11_case__168 x1 x2 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_11_case__168(x1)(x2)(x6)(x)(st))(i)(xs)(st)
c__case_11_case__168 x1 x2 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_11_case__168")(x)



c__case_12_case__169 x1 x2 x5 x6 x80@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_11(x2)(x6)(x5)(x1)(st))(st)
c__case_12_case__169 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_12_case__169(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_12_case__169 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_12_case__169")(x)



c__case_13_case__170 x1 x2 x5 x6 x80 x79 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x79)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_12(x2)(x5)(x6)(x80)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_14_case__171 x1 x2 x5 x6 x78@((Curry.Module.Prelude.:<) x79 x80) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_13(x2)(x5)(x6)(x80)(x79)(x1)(st))(st)
c__case_14_case__171 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_14_case__171(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_14_case__171 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_14_case__171")(x)



c__case_15_case__172 x1 x2 x5 x6 x78 x77 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x77)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_14(x2)(x5)(x6)(x78)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_16_case__173 x1 x2 x5 x6 x76@((Curry.Module.Prelude.:<) x77 x78) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_15(x2)(x5)(x6)(x78)(x77)(x1)(st))(st)
c__case_16_case__173 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_16_case__173(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_16_case__173 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_16_case__173")(x)



c__case_17_case__174 x1 x2 x5 x6 x76 x75 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x75)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_16(x2)(x5)(x6)(x76)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_18_case__175 x1 x2 x5 x6 x74@((Curry.Module.Prelude.:<) x75 x76) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_17(x2)(x5)(x6)(x76)(x75)(x1)(st))(st)
c__case_18_case__175 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_18_case__175(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_18_case__175 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_18_case__175")(x)



c__case_19_case__176 x1 x2 x5 x6 x74 x73 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x73)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_18(x2)(x5)(x6)(x74)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_20_case__177 x1 x2 x5 x6 x72@((Curry.Module.Prelude.:<) x73 x74) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_19(x2)(x5)(x6)(x74)(x73)(x1)(st))(st)
c__case_20_case__177 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_20_case__177(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_20_case__177 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_20_case__177")(x)



c__case_21_case__178 x1 x2 x5 x6 x72 x71 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x71)(Curry.Module.Prelude.C_Char('g'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_20(x2)(x5)(x6)(x72)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_22_case__179 x1 x2 x5 x6 x8@((Curry.Module.Prelude.:<) x71 x72) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_21(x2)(x5)(x6)(x72)(x71)(x1)(st))(st)
c__case_22_case__179 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_22_case__179(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_22_case__179 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_22_case__179")(x)



c__case_84_case__180 x1 x2 x5 x6 x8 x7 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x7)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_83(x2)(x5)(x6)(x8)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x7)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_22(x2)(x5)(x6)(x8)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st)



c__case_85_case__181 x1 x2 x5 x6 x4@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_84(x2)(x5)(x6)(x8)(x7)(x1)(st))(st)
c__case_85_case__181 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_85_case__181(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_85_case__181 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_85_case__181")(x)



c__case_86_case__182 x1 x2 x3@(Curry.Module.XML.C_XElem x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCompactFlatCurry.c__case_85(x2)(x5)(x6)(x4)(x1)(st))(st)
c__case_86_case__182 x1 x2 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCompactFlatCurry.c__case_86_case__182(x1)(x2)(x)(st))(i)(xs)(st)
c__case_86_case__182 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleCompactFlatCurry._case_86_case__182")(x)


