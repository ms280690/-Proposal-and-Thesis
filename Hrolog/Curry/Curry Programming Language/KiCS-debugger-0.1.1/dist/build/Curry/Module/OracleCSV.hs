{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleCSV (module Curry.Module.OracleCSV) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.CSV
import Curry.Module.List
import Curry.Module.Prelude
import Curry.Module.OracleList
import Curry.Module.OraclePrelude



-- begin included



-- end included

c_writeCSVFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_writeCSVFile x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_writeFile(x2)(Curry.Module.OracleCSV.c_showCSV(x3)(x1)(st))(x4)(st))(st)



c_showCSV :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCSV x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCSV.c_showCSVLine))))(x1)(st))(x2)(x3)(st))(st)



c_showCSVLine :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCSVLine x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_concat(Curry.Module.OracleList.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCSV.c_showCSVLine'46convert'467))))(x2)(x1)(st))(x3)(st))(x4)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x5)(st))(st)



c_showCSVLine'46convert'467 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCSVLine'46convert'467 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OracleCSV.c__case_16(x2)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_any(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCSV.c_showCSVLine'46convert'467'46_'35lambda2))))(x1)(st))(x2)(x3)(st))(x4)(st))(st)



c_showCSVLine'46convert'467'46_'35lambda2 :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_showCSVLine'46convert'467'46_'35lambda2 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_elem(x2)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List)))))(x3)(st))(st)



c_showCSVLine'46convert'467'46_'35lambda3 :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCSVLine'46convert'467'46_'35lambda3 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OracleCSV.c__case_15(x2)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\"'))(x1)(st))(x3)(st))(st)



c_readCSVFile :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))))))
c_readCSVFile x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCSV.c_readCSVFileWithDelims((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))))))(st)



c_readCSVFileWithDelims :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))))
c_readCSVFileWithDelims x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OraclePrelude.c_readFile(x3)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCSV.c_readCSVFileWithDelims'46_'35lambda4(x2)))))(x4)(st))(st)



c_readCSVFileWithDelims'46_'35lambda4 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))))
c_readCSVFileWithDelims'46_'35lambda4 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_return(Curry.Module.OracleCSV.c_readCSVWithDelims(x2)(x3)(x1)(st))(x4)(st))(st)



c_readCSV :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_readCSV x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCSV.c_readCSVWithDelims((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))))))(st)



c_readCSVWithDelims :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_readCSVWithDelims x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCSV.c_components(x2)))))(Curry.Module.OraclePrelude.c_lines(x3)(x1)(st))(x4)(st))(st)



c_components :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_components x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_14(x2)(x3)(x1)(st))(st)



c_components'46breakString'4625 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_components'46breakString'4625 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_11(x2)(x3)(x4)(x1)(st))(st)



c_components'46breakString'4625'46_'35selFP3'35b :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_components'46breakString'4625'46_'35selFP3'35b x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_5(x2)(x1)(st))(st)



c_components'46breakString'4625'46_'35selFP4'35bs :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_components'46breakString'4625'46_'35selFP4'35bs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_4(x2)(x1)(st))(st)



c_components'46breakString'4625'46_'35selFP6'35b :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_components'46breakString'4625'46_'35selFP6'35b x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_3(x2)(x1)(st))(st)



c_components'46breakString'4625'46_'35selFP7'35bs :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_components'46breakString'4625'46_'35selFP7'35bs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_2(x2)(x1)(st))(st)



c_components'46_'35selFP9'35e :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_components'46_'35selFP9'35e x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_1(x2)(x1)(st))(st)



c_components'46_'35selFP10'35s :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_components'46_'35selFP10'35s x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_0(x2)(x1)(st))(st)



c__case_0 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_0_case__16(x1)(x2)(st))(st)



c__case_1 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_1_case__15(x1)(x2)(st))(st)



c__case_2 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_2_case__14(x1)(x2)(st))(st)



c__case_3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_3_case__13(x1)(x2)(st))(st)



c__case_4 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_4_case__12(x1)(x2)(st))(st)



c__case_5 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_5_case__11(x1)(x2)(st))(st)



c__case_11 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_11_case__10(x1)(x2)(x3)(x4)(st))(st)



c__case_10 x2 x3 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_10_case__9(x1)(x2)(x3)(x5)(x6)(st))(st)



c__case_8 x2 x3 x5 x7 x8 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_8_case__8(x1)(x2)(x3)(x5)(x7)(x8)(x12)(st))(st)



c__case_7 x2 x3 x5 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_7_case__7(x1)(x2)(x3)(x5)(x7)(x8)(x9)(st))(st)



c__case_6 x2 x3 x5 x7 x8 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_6_case__6(x1)(x2)(x3)(x5)(x7)(x8)(x15)(st))(st)



c__case_9 x2 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_9_case__5(x1)(x2)(x6)(st))(st)



c__case_14 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_14_case__4(x1)(x2)(x3)(st))(st)



c__case_13 x2 x4 x5 x6 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_13_case__3(x1)(x2)(x4)(x5)(x6)(x10)(st))(st)



c__case_12 x2 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_12_case__2(x1)(x2)(x9)(x10)(st))(st)



c__case_15 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_15_case__1(x1)(x2)(x3)(st))(st)



c__case_16 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_16_case__0(x1)(x2)(x3)(st))(st)



c__case_16_case__0 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCSV.c_showCSVLine'46convert'467'46_'35lambda3))))(x1)(st))(x2)(x4)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List))(x5)(st)))(st)
c__case_16_case__0 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_16_case__0 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCSV.c__case_16_case__0(x1)(x2)(x)(st))(i)(xs)(st)
c__case_16_case__0 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleCSV._case_16_case__0")(x)



c__case_15_case__1 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)))(st)
c__case_15_case__1 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(st)
c__case_15_case__1 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCSV.c__case_15_case__1(x1)(x2)(x)(st))(i)(xs)(st)
c__case_15_case__1 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleCSV._case_15_case__1")(x)



c__case_12_case__2 x1 x2 x9 x10@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_12_case__2 x1 x2 x9 x10@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleCSV.c_components(x2)(Curry.Module.OraclePrelude.c_tail(x9)(x1)(st))(x11)(st))(st)
c__case_12_case__2 x1 x2 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCSV.c__case_12_case__2(x1)(x2)(x9)(x)(st))(i)(xs)(st)
c__case_12_case__2 x1 x2 x9 x st = Curry.RunTimeSystem.patternFail("OracleCSV._case_12_case__2")(x)



c__case_13_case__3 x1 x2 x4 x5 x6 x10@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c_components'46breakString'4625(x6)(x2)(x5)(x1)(st))(st)
c__case_13_case__3 x1 x2 x4 x5 x6 x10@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List)))(let {x7 = Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_break(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_elem))))(x2)))))(x1)(st))((Curry.Module.Prelude.:<)(x4)(x5))(x11)(st)} in let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(let {x9 = Curry.Module.OracleCSV.c_components'46_'35selFP10'35s(x7)(x13)(st)} in let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.OracleCSV.c_components'46_'35selFP9'35e(x7)(x12)(st))(Curry.Module.OracleCSV.c__case_12(x2)(x9)(Curry.Module.OraclePrelude.c_null(x9)(x14)(st))(x15)(st)))(st))(st))(st))(st)
c__case_13_case__3 x1 x2 x4 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCSV.c__case_13_case__3(x1)(x2)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_13_case__3 x1 x2 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCSV._case_13_case__3")(x)



c__case_14_case__4 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(st)
c__case_14_case__4 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleCSV.c__case_13(x2)(x4)(x5)(Curry.Module.OraclePrelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('V'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.Prelude.C_Char('\"'))(x7)(st))(x8)(st))(st))(st)
c__case_14_case__4 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCSV.c__case_14_case__4(x1)(x2)(x)(st))(i)(xs)(st)
c__case_14_case__4 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleCSV._case_14_case__4")(x)



c__case_9_case__5 x1 x2 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(st)
c__case_9_case__5 x1 x2 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_9_case__5 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCSV.c__case_9_case__5(x1)(x2)(x)(st))(i)(xs)(st)
c__case_9_case__5 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleCSV._case_9_case__5")(x)



c__case_6_case__6 x1 x2 x3 x5 x7 x8 x15@Curry.Module.Prelude.C_True st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(let {x12 = Curry.Module.OracleCSV.c_components'46breakString'4625(x2)(x3)((Curry.Module.Prelude.:<)(x7)(x8))(x1)(st)} in let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))(let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x18)((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(x5)(Curry.Module.OracleCSV.c_components'46breakString'4625'46_'35selFP6'35b(x12)(x16)(st)))(Curry.Module.OracleCSV.c_components'46breakString'4625'46_'35selFP7'35bs(x12)(x17)(st)))(st))(st))(st))(st)
c__case_6_case__6 x1 x2 x3 x5 x7 x8 x15@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_6_case__6 x1 x2 x3 x5 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCSV.c__case_6_case__6(x1)(x2)(x3)(x5)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_6_case__6 x1 x2 x3 x5 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleCSV._case_6_case__6")(x)



c__case_7_case__7 x1 x2 x3 x5 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.List)(Curry.Module.OracleCSV.c_components(x3)(x8)(x1)(st)))(st)
c__case_7_case__7 x1 x2 x3 x5 x7 x8 x9@Curry.Module.Prelude.C_False st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.OracleCSV.c__case_6(x2)(x3)(x5)(x7)(x8)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x10)(st))(st)
c__case_7_case__7 x1 x2 x3 x5 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCSV.c__case_7_case__7(x1)(x2)(x3)(x5)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_7_case__7 x1 x2 x3 x5 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleCSV._case_7_case__7")(x)



c__case_8_case__8 x1 x2 x3 x5 x7 x8 x12@Curry.Module.Prelude.C_True st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(let {x9 = Curry.Module.OracleCSV.c_components'46breakString'4625(x2)(x3)(x8)(x1)(st)} in let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x15)((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(x5)(Curry.Module.OracleCSV.c_components'46breakString'4625'46_'35selFP3'35b(x9)(x13)(st)))(Curry.Module.OracleCSV.c_components'46breakString'4625'46_'35selFP4'35bs(x9)(x14)(st)))(st))(st))(st))(st)
c__case_8_case__8 x1 x2 x3 x5 x7 x8 x12@Curry.Module.Prelude.C_False st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List)))))(Curry.Module.OracleCSV.c__case_7(x2)(x3)(x5)(x7)(x8)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x5)(Curry.Module.Prelude.C_Char('\"'))(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_elem(x7)(x16)(st))(x3)(x17)(st))(x18)(st))(x19)(st))(st)
c__case_8_case__8 x1 x2 x3 x5 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCSV.c__case_8_case__8(x1)(x2)(x3)(x5)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_8_case__8 x1 x2 x3 x5 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleCSV._case_8_case__8")(x)



c__case_10_case__9 x1 x2 x3 x5 x6@Curry.Module.Prelude.List st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OracleCSV.c__case_9(x2)(x5)(Curry.Module.OraclePrelude.op_61_61(x5)(Curry.Module.Prelude.C_Char('\"'))(x1)(st))(x9)(st))(st)
c__case_10_case__9 x1 x2 x3 x5 x6@((Curry.Module.Prelude.:<) x7 x8) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))(Curry.Module.OracleCSV.c__case_8(x2)(x3)(x5)(x7)(x8)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x5)(Curry.Module.Prelude.C_Char('\"'))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x7)(Curry.Module.Prelude.C_Char('\"'))(x10)(st))(x11)(st))(x12)(st))(st)
c__case_10_case__9 x1 x2 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCSV.c__case_10_case__9(x1)(x2)(x3)(x5)(x)(st))(i)(xs)(st)
c__case_10_case__9 x1 x2 x3 x5 x st = Curry.RunTimeSystem.patternFail("OracleCSV._case_10_case__9")(x)



c__case_11_case__10 x1 x2 x3 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_11_case__10 x1 x2 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCSV.c__case_10(x2)(x3)(x5)(x6)(x1)(st))(st)
c__case_11_case__10 x1 x2 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCSV.c__case_11_case__10(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_11_case__10 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleCSV._case_11_case__10")(x)



c__case_5_case__11 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_5_case__11 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCSV.c__case_5_case__11(x1)(x)(st))(i)(xs)(st)
c__case_5_case__11 x1 x st = Curry.RunTimeSystem.patternFail("OracleCSV._case_5_case__11")(x)



c__case_4_case__12 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_4_case__12 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCSV.c__case_4_case__12(x1)(x)(st))(i)(xs)(st)
c__case_4_case__12 x1 x st = Curry.RunTimeSystem.patternFail("OracleCSV._case_4_case__12")(x)



c__case_3_case__13 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_3_case__13 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCSV.c__case_3_case__13(x1)(x)(st))(i)(xs)(st)
c__case_3_case__13 x1 x st = Curry.RunTimeSystem.patternFail("OracleCSV._case_3_case__13")(x)



c__case_2_case__14 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_2_case__14 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCSV.c__case_2_case__14(x1)(x)(st))(i)(xs)(st)
c__case_2_case__14 x1 x st = Curry.RunTimeSystem.patternFail("OracleCSV._case_2_case__14")(x)



c__case_1_case__15 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_1_case__15 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCSV.c__case_1_case__15(x1)(x)(st))(i)(xs)(st)
c__case_1_case__15 x1 x st = Curry.RunTimeSystem.patternFail("OracleCSV._case_1_case__15")(x)



c__case_0_case__16 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_0_case__16 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCSV.c__case_0_case__16(x1)(x)(st))(i)(xs)(st)
c__case_0_case__16 x1 x st = Curry.RunTimeSystem.patternFail("OracleCSV._case_0_case__16")(x)


