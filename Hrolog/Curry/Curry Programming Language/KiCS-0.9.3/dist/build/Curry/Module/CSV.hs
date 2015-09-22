{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.CSV (module Curry.Module.CSV) where

import Curry.RunTimeSystem
import Curry.Module.List
import Curry.Module.Prelude



-- begin included



-- end included

c_writeCSVFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_writeCSVFile x1 x2 st = Curry.Module.Prelude.c_writeFile(x1)(Curry.Module.CSV.c_showCSV(x2)(st))(st)



c_showCSV :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCSV x1 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.CSV.c_showCSVLine))(st))(x1)(st)



c_showCSVLine :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCSVLine x1 st = Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_concat(Curry.Module.List.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.CSV.c_showCSVLine'46convert'467))(x1)(st))(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(st)



c_showCSVLine'46convert'467 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCSVLine'46convert'467 x1 st = Curry.Module.CSV.c_showCSVLine'46convert'467_case_8(x1)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_any(Curry.Module.Prelude.pf(Curry.Module.CSV.c_showCSVLine'46convert'467'46_'35lambda2))(st))(x1)(st))(st)



c_showCSVLine'46convert'467'46_'35lambda2 :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_showCSVLine'46convert'467'46_'35lambda2 x1 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List)))))(st)



c_showCSVLine'46convert'467'46_'35lambda3 :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCSVLine'46convert'467'46_'35lambda3 x1 st = Curry.Module.CSV.c_showCSVLine'46convert'467'46_'35lambda3_case_7(x1)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Char('\"'))(st))(st)



c_readCSVFile :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_readCSVFile st = Curry.Module.Prelude.pf(Curry.Module.CSV.c_readCSVFileWithDelims((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List)))



c_readCSVFileWithDelims :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_readCSVFileWithDelims x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_readFile(x2)(st))(Curry.Module.Prelude.pf(Curry.Module.CSV.c_readCSVFileWithDelims'46_'35lambda4(x1)))(st)



c_readCSVFileWithDelims'46_'35lambda4 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_readCSVFileWithDelims'46_'35lambda4 x1 x2 st = Curry.Module.Prelude.c_return(Curry.Module.CSV.c_readCSVWithDelims(x1)(x2)(st))(st)



c_readCSV :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_readCSV st = Curry.Module.Prelude.pf(Curry.Module.CSV.c_readCSVWithDelims((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List)))



c_readCSVWithDelims :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_readCSVWithDelims x1 x2 st = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.CSV.c_components(x1)))(Curry.Module.Prelude.c_lines(x2)(st))(st)



c_components :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_components x1 x2@Curry.Module.Prelude.List st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)
c_components x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CSV.c_components_case_6(x1)(x3)(x4)(Curry.Module.Prelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('\"'))(st))(st)
c_components x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CSV.c_components(x1)(x)(st))(i)(xs)(st)
c_components x1 x st = Curry.RunTimeSystem.patternFail("CSV.components")(x)



c_components'46breakString'4625 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_components'46breakString'4625 x1 x2 x3@Curry.Module.Prelude.List st = x1
c_components'46breakString'4625 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CSV.c_components'46breakString'4625_case_4(x1)(x2)(x4)(x5)(st)
c_components'46breakString'4625 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CSV.c_components'46breakString'4625(x1)(x2)(x)(st))(i)(xs)(st)
c_components'46breakString'4625 x1 x2 x st = Curry.RunTimeSystem.patternFail("CSV.components.breakString.25")(x)



c_components'46breakString'4625'46_'35selFP3'35b :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_components'46breakString'4625'46_'35selFP3'35b x1@((Curry.Module.Prelude.:<) x2 x3) st = x2
c_components'46breakString'4625'46_'35selFP3'35b (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CSV.c_components'46breakString'4625'46_'35selFP3'35b(x)(st))(i)(xs)(st)
c_components'46breakString'4625'46_'35selFP3'35b x st = Curry.RunTimeSystem.patternFail("CSV.components.breakString.25._#selFP3#b")(x)



c_components'46breakString'4625'46_'35selFP4'35bs :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_components'46breakString'4625'46_'35selFP4'35bs x1@((Curry.Module.Prelude.:<) x2 x3) st = x3
c_components'46breakString'4625'46_'35selFP4'35bs (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CSV.c_components'46breakString'4625'46_'35selFP4'35bs(x)(st))(i)(xs)(st)
c_components'46breakString'4625'46_'35selFP4'35bs x st = Curry.RunTimeSystem.patternFail("CSV.components.breakString.25._#selFP4#bs")(x)



c_components'46breakString'4625'46_'35selFP6'35b :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_components'46breakString'4625'46_'35selFP6'35b x1@((Curry.Module.Prelude.:<) x2 x3) st = x2
c_components'46breakString'4625'46_'35selFP6'35b (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CSV.c_components'46breakString'4625'46_'35selFP6'35b(x)(st))(i)(xs)(st)
c_components'46breakString'4625'46_'35selFP6'35b x st = Curry.RunTimeSystem.patternFail("CSV.components.breakString.25._#selFP6#b")(x)



c_components'46breakString'4625'46_'35selFP7'35bs :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_components'46breakString'4625'46_'35selFP7'35bs x1@((Curry.Module.Prelude.:<) x2 x3) st = x3
c_components'46breakString'4625'46_'35selFP7'35bs (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CSV.c_components'46breakString'4625'46_'35selFP7'35bs(x)(st))(i)(xs)(st)
c_components'46breakString'4625'46_'35selFP7'35bs x st = Curry.RunTimeSystem.patternFail("CSV.components.breakString.25._#selFP7#bs")(x)



c_components'46_'35selFP9'35e :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_components'46_'35selFP9'35e x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_components'46_'35selFP9'35e (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CSV.c_components'46_'35selFP9'35e(x)(st))(i)(xs)(st)
c_components'46_'35selFP9'35e x st = Curry.RunTimeSystem.patternFail("CSV.components._#selFP9#e")(x)



c_components'46_'35selFP10'35s :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_components'46_'35selFP10'35s x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_components'46_'35selFP10'35s (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CSV.c_components'46_'35selFP10'35s(x)(st))(i)(xs)(st)
c_components'46_'35selFP10'35s x st = Curry.RunTimeSystem.patternFail("CSV.components._#selFP10#s")(x)



c_components'46breakString'4625_case_4 x1 x2 x4 x5@Curry.Module.Prelude.List st = Curry.Module.CSV.c_components'46breakString'4625_case_3(x1)(x4)(Curry.Module.Prelude.op_61_61(x4)(Curry.Module.Prelude.C_Char('\"'))(st))(st)
c_components'46breakString'4625_case_4 x1 x2 x4 x5@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CSV.c_components'46breakString'4625_case_2(x1)(x2)(x4)(x6)(x7)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x4)(Curry.Module.Prelude.C_Char('\"'))(st))(Curry.Module.Prelude.op_61_61(x6)(Curry.Module.Prelude.C_Char('\"'))(st))(st))(st)
c_components'46breakString'4625_case_4 x1 x2 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CSV.c_components'46breakString'4625_case_4(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c_components'46breakString'4625_case_4 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("CSV.components.breakString.25_case_4")(x)



c_components'46breakString'4625_case_2 x1 x2 x4 x6 x7 x8@Curry.Module.Prelude.C_True st = let {x8 = Curry.Module.CSV.c_components'46breakString'4625(x1)(x2)(x7)(st)} in (Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(x4)(Curry.Module.CSV.c_components'46breakString'4625'46_'35selFP3'35b(x8)(st)))(Curry.Module.CSV.c_components'46breakString'4625'46_'35selFP4'35bs(x8)(st))
c_components'46breakString'4625_case_2 x1 x2 x4 x6 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.CSV.c_components'46breakString'4625_case_1(x1)(x2)(x4)(x6)(x7)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x4)(Curry.Module.Prelude.C_Char('\"'))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(x6)(st))(x2)(st))(st))(st)
c_components'46breakString'4625_case_2 x1 x2 x4 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CSV.c_components'46breakString'4625_case_2(x1)(x2)(x4)(x6)(x7)(x)(st))(i)(xs)(st)
c_components'46breakString'4625_case_2 x1 x2 x4 x6 x7 x st = Curry.RunTimeSystem.patternFail("CSV.components.breakString.25_case_2")(x)



c_components'46breakString'4625_case_1 x1 x2 x4 x6 x7 x8@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.List)(Curry.Module.CSV.c_components(x2)(x7)(st))
c_components'46breakString'4625_case_1 x1 x2 x4 x6 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.CSV.c_components'46breakString'4625_case_0(x1)(x2)(x4)(x6)(x7)(Curry.Module.Prelude.c_otherwise(st))(st)
c_components'46breakString'4625_case_1 x1 x2 x4 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CSV.c_components'46breakString'4625_case_1(x1)(x2)(x4)(x6)(x7)(x)(st))(i)(xs)(st)
c_components'46breakString'4625_case_1 x1 x2 x4 x6 x7 x st = Curry.RunTimeSystem.patternFail("CSV.components.breakString.25_case_1")(x)



c_components'46breakString'4625_case_0 x1 x2 x4 x6 x7 x8@Curry.Module.Prelude.C_True st = let {x11 = Curry.Module.CSV.c_components'46breakString'4625(x1)(x2)((Curry.Module.Prelude.:<)(x6)(x7))(st)} in (Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(x4)(Curry.Module.CSV.c_components'46breakString'4625'46_'35selFP6'35b(x11)(st)))(Curry.Module.CSV.c_components'46breakString'4625'46_'35selFP7'35bs(x11)(st))
c_components'46breakString'4625_case_0 x1 x2 x4 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CSV.c_components'46breakString'4625_case_0(x1)(x2)(x4)(x6)(x7)(x)(st))(i)(xs)(st)
c_components'46breakString'4625_case_0 x1 x2 x4 x6 x7 x st = Curry.RunTimeSystem.patternFail("CSV.components.breakString.25_case_0")(x)



c_components'46breakString'4625_case_3 x1 x4 x5@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)
c_components'46breakString'4625_case_3 x1 x4 x5@Curry.Module.Prelude.C_False st = x1
c_components'46breakString'4625_case_3 x1 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CSV.c_components'46breakString'4625_case_3(x1)(x4)(x)(st))(i)(xs)(st)
c_components'46breakString'4625_case_3 x1 x4 x st = Curry.RunTimeSystem.patternFail("CSV.components.breakString.25_case_3")(x)



c_components_case_6 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CSV.c_components'46breakString'4625(Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('V'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(st))(x1)(x4)(st)
c_components_case_6 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_break(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_elem))(x1)))(st))((Curry.Module.Prelude.:<)(x3)(x4))(st)} in let {x8 = Curry.Module.CSV.c_components'46_'35selFP10'35s(x6)(st)} in (Curry.Module.Prelude.:<)(Curry.Module.CSV.c_components'46_'35selFP9'35e(x6)(st))(Curry.Module.CSV.c_components_case_5(x1)(x8)(Curry.Module.Prelude.c_null(x8)(st))(st))
c_components_case_6 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CSV.c_components_case_6(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_components_case_6 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("CSV.components_case_6")(x)



c_components_case_5 x1 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.List
c_components_case_5 x1 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.CSV.c_components(x1)(Curry.Module.Prelude.c_tail(x8)(st))(st)
c_components_case_5 x1 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CSV.c_components_case_5(x1)(x8)(x)(st))(i)(xs)(st)
c_components_case_5 x1 x8 x st = Curry.RunTimeSystem.patternFail("CSV.components_case_5")(x)



c_showCSVLine'46convert'467'46_'35lambda3_case_7 x1 x2@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x1)((Curry.Module.Prelude.:<)(x1)(Curry.Module.Prelude.List))
c_showCSVLine'46convert'467'46_'35lambda3_case_7 x1 x2@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(x1)(Curry.Module.Prelude.List)
c_showCSVLine'46convert'467'46_'35lambda3_case_7 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CSV.c_showCSVLine'46convert'467'46_'35lambda3_case_7(x1)(x)(st))(i)(xs)(st)
c_showCSVLine'46convert'467'46_'35lambda3_case_7 x1 x st = Curry.RunTimeSystem.patternFail("CSV.showCSVLine.convert.7._#lambda3_case_7")(x)



c_showCSVLine'46convert'467_case_8 x1 x2@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.CSV.c_showCSVLine'46convert'467'46_'35lambda3))(st))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List))(st))
c_showCSVLine'46convert'467_case_8 x1 x2@Curry.Module.Prelude.C_False st = x1
c_showCSVLine'46convert'467_case_8 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.CSV.c_showCSVLine'46convert'467_case_8(x1)(x)(st))(i)(xs)(st)
c_showCSVLine'46convert'467_case_8 x1 x st = Curry.RunTimeSystem.patternFail("CSV.showCSVLine.convert.7_case_8")(x)


