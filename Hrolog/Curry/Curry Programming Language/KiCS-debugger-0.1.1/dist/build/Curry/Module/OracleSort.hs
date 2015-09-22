{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleSort (module Curry.Module.OracleSort) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.Sort
import Curry.Module.Char
import Curry.Module.Prelude
import Curry.Module.OracleChar
import Curry.Module.OraclePrelude



-- begin included



-- end included

c_quickSort :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_quickSort x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_43(x2)(x3)(x1)(st))(st)



c_quickSort'46split'466 :: (Curry t8) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t8 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t8 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> t8 -> (Curry.Module.Prelude.List t8) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t8) (Curry.Module.Prelude.List t8)
c_quickSort'46split'466 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_42(x2)(x3)(x4)(x1)(st))(st)



c_quickSort'46split'466'46_'35selFP3'35l :: (Curry t8) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t8) (Curry.Module.Prelude.List t8)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t8
c_quickSort'46split'466'46_'35selFP3'35l x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_39(x2)(x1)(st))(st)



c_quickSort'46split'466'46_'35selFP4'35r :: (Curry t8) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t8) (Curry.Module.Prelude.List t8)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t8
c_quickSort'46split'466'46_'35selFP4'35r x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_38(x2)(x1)(st))(st)



c_quickSort'46_'35selFP6'35l :: (Curry t8) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t8) (Curry.Module.Prelude.List t8)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t8
c_quickSort'46_'35selFP6'35l x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_37(x2)(x1)(st))(st)



c_quickSort'46_'35selFP7'35r :: (Curry t8) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t8) (Curry.Module.Prelude.List t8)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t8
c_quickSort'46_'35selFP7'35r x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_36(x2)(x1)(st))(st)



c_mergeSort :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_mergeSort x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleSort.c_mergeSort'46mergeLists'4616(x2)(Curry.Module.OracleSort.c_mergeSort'46genRuns'4616(x2)(x3)(x1)(st))(x4)(st))(st)



c_mergeSort'46genRuns'4616 :: (Curry t79) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t79 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t79 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> (Curry.Module.Prelude.List t79) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List t79)
c_mergeSort'46genRuns'4616 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_35(x2)(x3)(x1)(st))(st)



c_mergeSort'46mergePairs'4616 :: (Curry t79) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t79 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t79 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List t79)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List t79)
c_mergeSort'46mergePairs'4616 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_31(x2)(x3)(x1)(st))(st)



c_mergeSort'46mergeLists'4616 :: (Curry t79) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t79 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t79 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List t79)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t79
c_mergeSort'46mergeLists'4616 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_29(x2)(x3)(x1)(st))(st)



c_merge :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_merge x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_27(x2)(x4)(x3)(x1)(st))(st)



c_leqList :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))) -> (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_leqList x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_23(x2)(x4)(x3)(x1)(st))(st)



c_cmpList :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering))))) -> (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_cmpList x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_19(x2)(x4)(x3)(x1)(st))(st)



c_leqChar :: Curry.Module.Prelude.C_Char -> Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_leqChar x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_60_61(Curry.Module.OraclePrelude.c_ord(x2)(x1)(st))(Curry.Module.OraclePrelude.c_ord(x3)(x4)(st))(x5)(st))(st)



c_cmpChar :: Curry.Module.Prelude.C_Char -> Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_cmpChar x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleSort.c__case_14(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x2)(x3)(x1)(st))(x4)(st))(st)



c_leqCharIgnoreCase :: Curry.Module.Prelude.C_Char -> Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_leqCharIgnoreCase x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_60_61(Curry.Module.OraclePrelude.c_ord(Curry.Module.OracleChar.c_toUpper(x2)(x1)(st))(x4)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.OracleChar.c_toUpper(x3)(x5)(st))(x6)(st))(x7)(st))(st)



c_leqString :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))
c_leqString x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleSort.c_leqList(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleSort.c_leqChar))(st))))(st))(st)



c_cmpString :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering))))
c_cmpString x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleSort.c_cmpList(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleSort.c_cmpChar))(st))))(st))(st)



c_leqStringIgnoreCase :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))))
c_leqStringIgnoreCase x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleSort.c_leqList(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleSort.c_leqCharIgnoreCase))(st))))(st))(st)



c_leqLexGerman :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_leqLexGerman x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_12(x3)(x2)(x1)(st))(st)



c_leqLexGerman'46glex'4689 :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_leqLexGerman'46glex'4689 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))))))(Curry.Module.OracleSort.c__case_8(x2)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_62_61(x2)(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('A'))(x1)(st))(x3)(st))(Curry.Module.OraclePrelude.op_60_61(x2)(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('Z'))(x4)(st))(x5)(st))(x6)(st))(x7)(st))(st)



c__case_8 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_8_case__43(x1)(x2)(x3)(st))(st)



c__case_7 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_7_case__42(x1)(x2)(x3)(st))(st)



c__case_6 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_6_case__41(x1)(x2)(x3)(st))(st)



c__case_5 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_5_case__40(x1)(x2)(x3)(st))(st)



c__case_4 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_4_case__39(x1)(x2)(x3)(st))(st)



c__case_3 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_3_case__38(x1)(x2)(x3)(st))(st)



c__case_2 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_2_case__37(x1)(x2)(x3)(st))(st)



c__case_1 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_1_case__36(x1)(x2)(x3)(st))(st)



c__case_0 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_0_case__35(x1)(x2)(x3)(st))(st)



c__case_12 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_12_case__34(x1)(x3)(x2)(st))(st)



c__case_11 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_11_case__33(x1)(x4)(x5)(x3)(st))(st)



c__case_10 x5 x7 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_10_case__32(x1)(x5)(x7)(x8)(x9)(x10)(st))(st)



c__case_9 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_9_case__31(x1)(x8)(x9)(x10)(st))(st)



c__case_14 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_14_case__30(x1)(x2)(x3)(x4)(st))(st)



c__case_13 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_13_case__29(x1)(x4)(st))(st)



c__case_19 x2 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_19_case__28(x1)(x2)(x4)(x3)(st))(st)



c__case_17 x2 x7 x8 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_17_case__27(x1)(x2)(x7)(x8)(x4)(st))(st)



c__case_16 x2 x7 x8 x9 x10 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_16_case__26(x1)(x2)(x7)(x8)(x9)(x10)(x11)(st))(st)



c__case_15 x2 x7 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_15_case__25(x1)(x2)(x7)(x9)(x10)(st))(st)



c__case_18 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_18_case__24(x1)(x4)(st))(st)



c__case_23 x2 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_23_case__23(x1)(x2)(x4)(x3)(st))(st)



c__case_22 x2 x5 x6 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_22_case__22(x1)(x2)(x5)(x6)(x4)(st))(st)



c__case_21 x2 x5 x6 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_21_case__21(x1)(x2)(x5)(x6)(x7)(x8)(x9)(st))(st)



c__case_20 x2 x5 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_20_case__20(x1)(x2)(x5)(x7)(x8)(st))(st)



c__case_27 x2 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_27_case__19(x1)(x2)(x4)(x3)(st))(st)



c__case_26 x2 x5 x6 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_26_case__18(x1)(x2)(x5)(x6)(x4)(st))(st)



c__case_25 x2 x5 x6 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_25_case__17(x1)(x2)(x5)(x6)(x7)(x8)(x9)(st))(st)



c__case_24 x2 x5 x6 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_24_case__16(x1)(x2)(x5)(x6)(x7)(x8)(x9)(st))(st)



c__case_29 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_29_case__15(x1)(x2)(x3)(st))(st)



c__case_28 x2 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_28_case__14(x1)(x2)(x4)(x5)(st))(st)



c__case_31 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_31_case__13(x1)(x2)(x3)(st))(st)



c__case_30 x2 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_30_case__12(x1)(x2)(x4)(x5)(st))(st)



c__case_35 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_35_case__11(x1)(x2)(x3)(st))(st)



c__case_34 x2 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_34_case__10(x1)(x2)(x4)(x5)(st))(st)



c__case_33 x2 x4 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_33_case__9(x1)(x2)(x4)(x6)(x7)(x8)(st))(st)



c__case_32 x2 x4 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_32_case__8(x1)(x2)(x4)(x6)(x7)(x8)(st))(st)



c__case_36 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_36_case__7(x1)(x2)(st))(st)



c__case_37 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_37_case__6(x1)(x2)(st))(st)



c__case_38 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_38_case__5(x1)(x2)(st))(st)



c__case_39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_39_case__4(x1)(x2)(st))(st)



c__case_42 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_42_case__3(x1)(x2)(x3)(x4)(st))(st)



c__case_41 x2 x3 x5 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_41_case__2(x1)(x5)(x8)(x9)(x10)(st))(st)



c__case_40 x5 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_40_case__1(x1)(x5)(x8)(x9)(x10)(st))(st)



c__case_43 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_43_case__0(x1)(x2)(x3)(st))(st)



c__case_43_case__0 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_43_case__0 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(let {x6 = Curry.Module.OracleSort.c_quickSort'46split'466(x2)(x4)(x5)(x1)(st)} in let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleSort.c_quickSort(x2)(Curry.Module.OracleSort.c_quickSort'46_'35selFP6'35l(x6)(x9)(st))(x11)(st))((Curry.Module.Prelude.:<)(x4)(Curry.Module.OracleSort.c_quickSort(x2)(Curry.Module.OracleSort.c_quickSort'46_'35selFP7'35r(x6)(x10)(st))(x12)(st)))(x13)(st))(st))(st))(st))(st)
c__case_43_case__0 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_43_case__0(x1)(x2)(x)(st))(i)(xs)(st)
c__case_43_case__0 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_43_case__0")(x)



c__case_40_case__1 x1 x5 x8 x9 x10@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(x8)((Curry.Module.Prelude.:<)(x5)(x9)))(st)
c__case_40_case__1 x1 x5 x8 x9 x10@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_40_case__1 x1 x5 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_40_case__1(x1)(x5)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_40_case__1 x1 x5 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_40_case__1")(x)



c__case_41_case__2 x1 x5 x8 x9 x10@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x5)(x8))(x9))(st)
c__case_41_case__2 x1 x5 x8 x9 x10@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleSort.c__case_40(x5)(x8)(x9)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x11)(st))(st)
c__case_41_case__2 x1 x5 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_41_case__2(x1)(x5)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_41_case__2 x1 x5 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_41_case__2")(x)



c__case_42_case__3 x1 x2 x3 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(st)
c__case_42_case__3 x1 x2 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(let {x7 = Curry.Module.OracleSort.c_quickSort'46split'466(x2)(x3)(x6)(x1)(st)} in let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))(Curry.Module.OracleSort.c__case_41(x2)(x3)(x5)(Curry.Module.OracleSort.c_quickSort'46split'466'46_'35selFP3'35l(x7)(x10)(st))(Curry.Module.OracleSort.c_quickSort'46split'466'46_'35selFP4'35r(x7)(x11)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x5)(x12)(st))(x3)(x13)(st))(x14)(st))(st))(st))(st))(st)
c__case_42_case__3 x1 x2 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_42_case__3(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_42_case__3 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_42_case__3")(x)



c__case_39_case__4 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_39_case__4 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_39_case__4(x1)(x)(st))(i)(xs)(st)
c__case_39_case__4 x1 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_39_case__4")(x)



c__case_38_case__5 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_38_case__5 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_38_case__5(x1)(x)(st))(i)(xs)(st)
c__case_38_case__5 x1 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_38_case__5")(x)



c__case_37_case__6 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_37_case__6 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_37_case__6(x1)(x)(st))(i)(xs)(st)
c__case_37_case__6 x1 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_37_case__6")(x)



c__case_36_case__7 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_36_case__7 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_36_case__7(x1)(x)(st))(i)(xs)(st)
c__case_36_case__7 x1 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_36_case__7")(x)



c__case_32_case__8 x1 x2 x4 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OracleSort.c_mergeSort'46genRuns'4616(x2)(x7)(x1)(st)))(st)
c__case_32_case__8 x1 x2 x4 x6 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_32_case__8 x1 x2 x4 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_32_case__8(x1)(x2)(x4)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_32_case__8 x1 x2 x4 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_32_case__8")(x)



c__case_33_case__9 x1 x2 x4 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OracleSort.c_mergeSort'46genRuns'4616(x2)(x7)(x1)(st)))(st)
c__case_33_case__9 x1 x2 x4 x6 x7 x8@Curry.Module.Prelude.C_False st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OracleSort.c__case_32(x2)(x4)(x6)(x7)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x9)(st))(st)
c__case_33_case__9 x1 x2 x4 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_33_case__9(x1)(x2)(x4)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_33_case__9 x1 x2 x4 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_33_case__9")(x)



c__case_34_case__10 x1 x2 x4 x5@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.Prelude.List))(st)
c__case_34_case__10 x1 x2 x4 x5@((Curry.Module.Prelude.:<) x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OracleSort.c__case_33(x2)(x4)(x6)(x7)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x4)(x1)(st))(x6)(x8)(st))(x9)(st))(st)
c__case_34_case__10 x1 x2 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_34_case__10(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_34_case__10 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_34_case__10")(x)



c__case_35_case__11 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_35_case__11 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_34(x2)(x4)(x5)(x1)(st))(st)
c__case_35_case__11 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_35_case__11(x1)(x2)(x)(st))(i)(xs)(st)
c__case_35_case__11 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_35_case__11")(x)



c__case_30_case__12 x1 x2 x4 x5@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(st)
c__case_30_case__12 x1 x2 x4 x5@((Curry.Module.Prelude.:<) x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.OracleSort.c_merge(x2)(x4)(x6)(x1)(st))(Curry.Module.OracleSort.c_mergeSort'46mergePairs'4616(x2)(x7)(x8)(st)))(st)
c__case_30_case__12 x1 x2 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_30_case__12(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_30_case__12 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_30_case__12")(x)



c__case_31_case__13 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_31_case__13 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_30(x2)(x4)(x5)(x1)(st))(st)
c__case_31_case__13 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_31_case__13(x1)(x2)(x)(st))(i)(xs)(st)
c__case_31_case__13 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_31_case__13")(x)



c__case_28_case__14 x1 x2 x4 x5@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_28_case__14 x1 x2 x4 x5@((Curry.Module.Prelude.:<) x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OracleSort.c_mergeSort'46mergeLists'4616(x2)((Curry.Module.Prelude.:<)(Curry.Module.OracleSort.c_merge(x2)(x4)(x6)(x1)(st))(Curry.Module.OracleSort.c_mergeSort'46mergePairs'4616(x2)(x7)(x8)(st)))(x9)(st))(st)
c__case_28_case__14 x1 x2 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_28_case__14(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_28_case__14 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_28_case__14")(x)



c__case_29_case__15 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_29_case__15 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_28(x2)(x4)(x5)(x1)(st))(st)
c__case_29_case__15 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_29_case__15(x1)(x2)(x)(st))(i)(xs)(st)
c__case_29_case__15 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_29_case__15")(x)



c__case_24_case__16 x1 x2 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.OracleSort.c_merge(x2)((Curry.Module.Prelude.:<)(x5)(x6))(x8)(x1)(st)))(st)
c__case_24_case__16 x1 x2 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_24_case__16 x1 x2 x5 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_24_case__16(x1)(x2)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_24_case__16 x1 x2 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_24_case__16")(x)



c__case_25_case__17 x1 x2 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.OracleSort.c_merge(x2)(x6)((Curry.Module.Prelude.:<)(x7)(x8))(x1)(st)))(st)
c__case_25_case__17 x1 x2 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_False st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.OracleSort.c__case_24(x2)(x5)(x6)(x7)(x8)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x10)(st))(st)
c__case_25_case__17 x1 x2 x5 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_25_case__17(x1)(x2)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_25_case__17 x1 x2 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_25_case__17")(x)



c__case_26_case__18 x1 x2 x5 x6 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x5)(x6))(st)
c__case_26_case__18 x1 x2 x5 x6 x4@((Curry.Module.Prelude.:<) x7 x8) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))(Curry.Module.OracleSort.c__case_25(x2)(x5)(x6)(x7)(x8)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x5)(x1)(st))(x7)(x9)(st))(x10)(st))(st)
c__case_26_case__18 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_26_case__18(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_26_case__18 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_26_case__18")(x)



c__case_27_case__19 x1 x2 x4 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_27_case__19 x1 x2 x4 x3@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_26(x2)(x5)(x6)(x4)(x1)(st))(st)
c__case_27_case__19 x1 x2 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_27_case__19(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_27_case__19 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_27_case__19")(x)



c__case_20_case__20 x1 x2 x5 x7 x8@Curry.Module.Prelude.C_True st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x5)(x1)(st))(x7)(x9)(st))(st)
c__case_20_case__20 x1 x2 x5 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_20_case__20 x1 x2 x5 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_20_case__20(x1)(x2)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_20_case__20 x1 x2 x5 x7 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_20_case__20")(x)



c__case_21_case__21 x1 x2 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c_leqList(x2)(x6)(x8)(x1)(st))(st)
c__case_21_case__21 x1 x2 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_False st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.OracleSort.c__case_20(x2)(x5)(x7)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x10)(st))(st)
c__case_21_case__21 x1 x2 x5 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_21_case__21(x1)(x2)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_21_case__21 x1 x2 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_21_case__21")(x)



c__case_22_case__22 x1 x2 x5 x6 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_22_case__22 x1 x2 x5 x6 x4@((Curry.Module.Prelude.:<) x7 x8) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OracleSort.c__case_21(x2)(x5)(x6)(x7)(x8)(Curry.Module.OraclePrelude.op_61_61(x5)(x7)(x1)(st))(x9)(st))(st)
c__case_22_case__22 x1 x2 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_22_case__22(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_22_case__22 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_22_case__22")(x)



c__case_23_case__23 x1 x2 x4 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_23_case__23 x1 x2 x4 x3@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_22(x2)(x5)(x6)(x4)(x1)(st))(st)
c__case_23_case__23 x1 x2 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_23_case__23(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_23_case__23 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_23_case__23")(x)



c__case_18_case__24 x1 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_EQ)(st)
c__case_18_case__24 x1 x4@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_LT)(st)
c__case_18_case__24 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_18_case__24(x1)(x)(st))(i)(xs)(st)
c__case_18_case__24 x1 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_18_case__24")(x)



c__case_15_case__25 x1 x2 x7 x9 x10@Curry.Module.Prelude.C_True st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x7)(x1)(st))(x9)(x11)(st))(st)
c__case_15_case__25 x1 x2 x7 x9 x10@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_15_case__25 x1 x2 x7 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_15_case__25(x1)(x2)(x7)(x9)(x)(st))(i)(xs)(st)
c__case_15_case__25 x1 x2 x7 x9 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_15_case__25")(x)



c__case_16_case__26 x1 x2 x7 x8 x9 x10 x11@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c_cmpList(x2)(x8)(x10)(x1)(st))(st)
c__case_16_case__26 x1 x2 x7 x8 x9 x10 x11@Curry.Module.Prelude.C_False st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(Curry.Module.OracleSort.c__case_15(x2)(x7)(x9)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x12)(st))(st)
c__case_16_case__26 x1 x2 x7 x8 x9 x10 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_16_case__26(x1)(x2)(x7)(x8)(x9)(x10)(x)(st))(i)(xs)(st)
c__case_16_case__26 x1 x2 x7 x8 x9 x10 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_16_case__26")(x)



c__case_17_case__27 x1 x2 x7 x8 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_GT)(st)
c__case_17_case__27 x1 x2 x7 x8 x4@((Curry.Module.Prelude.:<) x9 x10) st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))))(Curry.Module.OracleSort.c__case_16(x2)(x7)(x8)(x9)(x10)(Curry.Module.OraclePrelude.op_61_61(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x7)(x1)(st))(x9)(x11)(st))(Curry.Module.Prelude.C_EQ)(x12)(st))(x13)(st))(st)
c__case_17_case__27 x1 x2 x7 x8 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_17_case__27(x1)(x2)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_17_case__27 x1 x2 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_17_case__27")(x)



c__case_19_case__28 x1 x2 x4 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_18(x4)(x1)(st))(st)
c__case_19_case__28 x1 x2 x4 x3@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_17(x2)(x7)(x8)(x4)(x1)(st))(st)
c__case_19_case__28 x1 x2 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_19_case__28(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_19_case__28 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_19_case__28")(x)



c__case_13_case__29 x1 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_LT)(st)
c__case_13_case__29 x1 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_GT)(st)
c__case_13_case__29 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_13_case__29(x1)(x)(st))(i)(xs)(st)
c__case_13_case__29 x1 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_13_case__29")(x)



c__case_14_case__30 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_EQ)(st)
c__case_14_case__30 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))))(Curry.Module.OracleSort.c__case_13(x2)(x3)(Curry.Module.OraclePrelude.op_60(Curry.Module.OraclePrelude.c_ord(x2)(x1)(st))(Curry.Module.OraclePrelude.c_ord(x3)(x5)(st))(x6)(st))(x7)(st))(st)
c__case_14_case__30 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_14_case__30(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_14_case__30 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_14_case__30")(x)



c__case_9_case__31 x1 x8 x9 x10@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_60(x8)(x9)(x1)(st))(st)
c__case_9_case__31 x1 x8 x9 x10@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_9_case__31 x1 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_9_case__31(x1)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_9_case__31 x1 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_9_case__31")(x)



c__case_10_case__32 x1 x5 x7 x8 x9 x10@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c_leqLexGerman(x5)(x7)(x1)(st))(st)
c__case_10_case__32 x1 x5 x7 x8 x9 x10@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleSort.c__case_9(x8)(x9)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x11)(st))(st)
c__case_10_case__32 x1 x5 x7 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_10_case__32(x1)(x5)(x7)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_10_case__32 x1 x5 x7 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_10_case__32")(x)



c__case_11_case__33 x1 x4 x5 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_11_case__33 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))(let {x8 = Curry.Module.OracleSort.c_leqLexGerman'46glex'4689(Curry.Module.OraclePrelude.c_ord(x4)(x1)(st))(x10)(st)} in let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List)))(let {x9 = Curry.Module.OracleSort.c_leqLexGerman'46glex'4689(Curry.Module.OraclePrelude.c_ord(x6)(x11)(st))(x12)(st)} in let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(Curry.Module.OracleSort.c__case_10(x5)(x7)(x8)(x9)(Curry.Module.OraclePrelude.op_61_61(x8)(x9)(x13)(st))(x14)(st))(st))(st))(st)
c__case_11_case__33 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_11_case__33(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_11_case__33 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_11_case__33")(x)



c__case_12_case__34 x1 x3 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_12_case__34 x1 x3 x2@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleSort.c__case_11(x4)(x5)(x3)(x1)(st))(st)
c__case_12_case__34 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_12_case__34(x1)(x3)(x)(st))(i)(xs)(st)
c__case_12_case__34 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_12_case__34")(x)



c__case_0_case__35 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_0_case__35 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_0_case__35 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_0_case__35(x1)(x2)(x)(st))(i)(xs)(st)
c__case_0_case__35 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_0_case__35")(x)



c__case_1_case__36 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('s'))(x1)(st))(st)
c__case_1_case__36 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleSort.c__case_0(x2)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x4)(st))(st)
c__case_1_case__36 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_1_case__36(x1)(x2)(x)(st))(i)(xs)(st)
c__case_1_case__36 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_1_case__36")(x)



c__case_2_case__37 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('u'))(x1)(st))(st)
c__case_2_case__37 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleSort.c__case_1(x2)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(x1)(st))(x4)(st))(st)
c__case_2_case__37 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_2_case__37(x1)(x2)(x)(st))(i)(xs)(st)
c__case_2_case__37 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_2_case__37")(x)



c__case_3_case__38 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('o'))(x1)(st))(st)
c__case_3_case__38 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleSort.c__case_2(x2)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(x1)(st))(x4)(st))(st)
c__case_3_case__38 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_3_case__38(x1)(x2)(x)(st))(i)(xs)(st)
c__case_3_case__38 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_3_case__38")(x)



c__case_4_case__39 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('a'))(x1)(st))(st)
c__case_4_case__39 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleSort.c__case_3(x2)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(x1)(st))(x4)(st))(st)
c__case_4_case__39 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_4_case__39(x1)(x2)(x)(st))(i)(xs)(st)
c__case_4_case__39 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_4_case__39")(x)



c__case_5_case__40 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('u'))(x1)(st))(st)
c__case_5_case__40 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleSort.c__case_4(x2)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(x1)(st))(x4)(st))(st)
c__case_5_case__40 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_5_case__40(x1)(x2)(x)(st))(i)(xs)(st)
c__case_5_case__40 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_5_case__40")(x)



c__case_6_case__41 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('o'))(x1)(st))(st)
c__case_6_case__41 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleSort.c__case_5(x2)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(x1)(st))(x4)(st))(st)
c__case_6_case__41 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_6_case__41(x1)(x2)(x)(st))(i)(xs)(st)
c__case_6_case__41 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_6_case__41")(x)



c__case_7_case__42 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('a'))(x1)(st))(st)
c__case_7_case__42 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleSort.c__case_6(x2)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(x1)(st))(x4)(st))(st)
c__case_7_case__42 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_7_case__42(x1)(x2)(x)(st))(i)(xs)(st)
c__case_7_case__42 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_7_case__42")(x)



c__case_8_case__43 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43(x2)(Curry.Module.OraclePrelude.op_45(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('a'))(x1)(st))(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('A'))(x4)(st))(x5)(st))(x6)(st))(st)
c__case_8_case__43 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleSort.c__case_7(x2)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(x1)(st))(x7)(st))(st)
c__case_8_case__43 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleSort.c__case_8_case__43(x1)(x2)(x)(st))(i)(xs)(st)
c__case_8_case__43 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleSort._case_8_case__43")(x)


