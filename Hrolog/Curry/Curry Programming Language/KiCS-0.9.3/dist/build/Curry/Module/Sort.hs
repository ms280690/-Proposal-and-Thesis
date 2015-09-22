{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Sort (module Curry.Module.Sort) where

import Curry.RunTimeSystem
import Curry.Module.Char
import Curry.Module.Prelude



-- begin included



-- end included

c_quickSort :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_quickSort x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_quickSort x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.Sort.c_quickSort'46split'466(x1)(x3)(x4)(st)} in Curry.Module.Prelude.op_43_43(Curry.Module.Sort.c_quickSort(x1)(Curry.Module.Sort.c_quickSort'46_'35selFP6'35l(x5)(st))(st))((Curry.Module.Prelude.:<)(x3)(Curry.Module.Sort.c_quickSort(x1)(Curry.Module.Sort.c_quickSort'46_'35selFP7'35r(x5)(st))(st)))(st)
c_quickSort x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_quickSort(x1)(x)(st))(i)(xs)(st)
c_quickSort x1 x st = Curry.RunTimeSystem.patternFail("Sort.quickSort")(x)



c_quickSort'46split'466 :: (Curry t8) => (Curry.Module.Prelude.Prim (t8 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t8 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> t8 -> (Curry.Module.Prelude.List t8) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t8) (Curry.Module.Prelude.List t8)
c_quickSort'46split'466 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List)
c_quickSort'46split'466 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = let {x6 = Curry.Module.Sort.c_quickSort'46split'466(x1)(x2)(x5)(st)} in let {x7 = Curry.Module.Sort.c_quickSort'46split'466'46_'35selFP3'35l(x6)(st)} in let {x8 = Curry.Module.Sort.c_quickSort'46split'466'46_'35selFP4'35r(x6)(st)} in Curry.Module.Sort.c_quickSort'46split'466_case_30(x1)(x2)(x4)(x7)(x8)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x4)(st))(x2)(st))(st)
c_quickSort'46split'466 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_quickSort'46split'466(x1)(x2)(x)(st))(i)(xs)(st)
c_quickSort'46split'466 x1 x2 x st = Curry.RunTimeSystem.patternFail("Sort.quickSort.split.6")(x)



c_quickSort'46split'466'46_'35selFP3'35l :: (Curry t8) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t8) (Curry.Module.Prelude.List t8)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t8
c_quickSort'46split'466'46_'35selFP3'35l x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_quickSort'46split'466'46_'35selFP3'35l (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_quickSort'46split'466'46_'35selFP3'35l(x)(st))(i)(xs)(st)
c_quickSort'46split'466'46_'35selFP3'35l x st = Curry.RunTimeSystem.patternFail("Sort.quickSort.split.6._#selFP3#l")(x)



c_quickSort'46split'466'46_'35selFP4'35r :: (Curry t8) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t8) (Curry.Module.Prelude.List t8)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t8
c_quickSort'46split'466'46_'35selFP4'35r x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_quickSort'46split'466'46_'35selFP4'35r (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_quickSort'46split'466'46_'35selFP4'35r(x)(st))(i)(xs)(st)
c_quickSort'46split'466'46_'35selFP4'35r x st = Curry.RunTimeSystem.patternFail("Sort.quickSort.split.6._#selFP4#r")(x)



c_quickSort'46_'35selFP6'35l :: (Curry t8) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t8) (Curry.Module.Prelude.List t8)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t8
c_quickSort'46_'35selFP6'35l x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_quickSort'46_'35selFP6'35l (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_quickSort'46_'35selFP6'35l(x)(st))(i)(xs)(st)
c_quickSort'46_'35selFP6'35l x st = Curry.RunTimeSystem.patternFail("Sort.quickSort._#selFP6#l")(x)



c_quickSort'46_'35selFP7'35r :: (Curry t8) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t8) (Curry.Module.Prelude.List t8)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t8
c_quickSort'46_'35selFP7'35r x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_quickSort'46_'35selFP7'35r (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_quickSort'46_'35selFP7'35r(x)(st))(i)(xs)(st)
c_quickSort'46_'35selFP7'35r x st = Curry.RunTimeSystem.patternFail("Sort.quickSort._#selFP7#r")(x)



c_mergeSort :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_mergeSort x1 x2 st = Curry.Module.Sort.c_mergeSort'46mergeLists'4616(x1)(Curry.Module.Sort.c_mergeSort'46genRuns'4616(x1)(x2)(st))(st)



c_mergeSort'46genRuns'4616 :: (Curry t79) => (Curry.Module.Prelude.Prim (t79 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t79 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.List t79) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List t79)
c_mergeSort'46genRuns'4616 x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_mergeSort'46genRuns'4616 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.Sort.c_mergeSort'46genRuns'4616_case_28(x1)(x3)(x4)(st)
c_mergeSort'46genRuns'4616 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_mergeSort'46genRuns'4616(x1)(x)(st))(i)(xs)(st)
c_mergeSort'46genRuns'4616 x1 x st = Curry.RunTimeSystem.patternFail("Sort.mergeSort.genRuns.16")(x)



c_mergeSort'46mergePairs'4616 :: (Curry t79) => (Curry.Module.Prelude.Prim (t79 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t79 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List t79)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List t79)
c_mergeSort'46mergePairs'4616 x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_mergeSort'46mergePairs'4616 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.Sort.c_mergeSort'46mergePairs'4616_case_25(x1)(x3)(x4)(st)
c_mergeSort'46mergePairs'4616 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_mergeSort'46mergePairs'4616(x1)(x)(st))(i)(xs)(st)
c_mergeSort'46mergePairs'4616 x1 x st = Curry.RunTimeSystem.patternFail("Sort.mergeSort.mergePairs.16")(x)



c_mergeSort'46mergeLists'4616 :: (Curry t79) => (Curry.Module.Prelude.Prim (t79 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t79 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List t79)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t79
c_mergeSort'46mergeLists'4616 x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_mergeSort'46mergeLists'4616 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.Sort.c_mergeSort'46mergeLists'4616_case_24(x1)(x3)(x4)(st)
c_mergeSort'46mergeLists'4616 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_mergeSort'46mergeLists'4616(x1)(x)(st))(i)(xs)(st)
c_mergeSort'46mergeLists'4616 x1 x st = Curry.RunTimeSystem.patternFail("Sort.mergeSort.mergeLists.16")(x)



c_merge :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_merge x1 x2@Curry.Module.Prelude.List x3 st = x3
c_merge x1 x2@((Curry.Module.Prelude.:<) x4 x5) x3 st = Curry.Module.Sort.c_merge_case_23(x1)(x4)(x5)(x3)(st)
c_merge x1 (Curry.Module.Prelude.ListOr i xs) x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_merge(x1)(x)(x3)(st))(i)(xs)(st)
c_merge x1 x x3 st = Curry.RunTimeSystem.patternFail("Sort.merge")(x)



c_leqList :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_leqList x1 x2@Curry.Module.Prelude.List x3 st = Curry.Module.Prelude.C_True
c_leqList x1 x2@((Curry.Module.Prelude.:<) x4 x5) x3 st = Curry.Module.Sort.c_leqList_case_20(x1)(x4)(x5)(x3)(st)
c_leqList x1 (Curry.Module.Prelude.ListOr i xs) x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_leqList(x1)(x)(x3)(st))(i)(xs)(st)
c_leqList x1 x x3 st = Curry.RunTimeSystem.patternFail("Sort.leqList")(x)



c_cmpList :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering))) -> (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_cmpList x1 x2@Curry.Module.Prelude.List x3 st = Curry.Module.Sort.c_cmpList_case_17(x3)(st)
c_cmpList x1 x2@((Curry.Module.Prelude.:<) x6 x7) x3 st = Curry.Module.Sort.c_cmpList_case_16(x1)(x6)(x7)(x3)(st)
c_cmpList x1 (Curry.Module.Prelude.ListOr i xs) x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_cmpList(x1)(x)(x3)(st))(i)(xs)(st)
c_cmpList x1 x x3 st = Curry.RunTimeSystem.patternFail("Sort.cmpList")(x)



c_leqChar :: Curry.Module.Prelude.C_Char -> Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_leqChar x1 x2 st = Curry.Module.Prelude.op_60_61(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(x2)(st))(st)



c_cmpChar :: Curry.Module.Prelude.C_Char -> Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_cmpChar x1 x2 st = Curry.Module.Sort.c_cmpChar_case_13(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)(x2)(st))(st)



c_leqCharIgnoreCase :: Curry.Module.Prelude.C_Char -> Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_leqCharIgnoreCase x1 x2 st = Curry.Module.Prelude.op_60_61(Curry.Module.Prelude.c_ord(Curry.Module.Char.c_toUpper(x1)(st))(st))(Curry.Module.Prelude.c_ord(Curry.Module.Char.c_toUpper(x2)(st))(st))(st)



c_leqString :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))
c_leqString st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Sort.c_leqList(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Sort.c_leqChar)))



c_cmpString :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering))
c_cmpString st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Sort.c_cmpList(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Sort.c_cmpChar)))



c_leqStringIgnoreCase :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))
c_leqStringIgnoreCase st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Sort.c_leqList(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Sort.c_leqCharIgnoreCase)))



c_leqLexGerman :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_leqLexGerman x1@Curry.Module.Prelude.List x2 st = Curry.Module.Prelude.C_True
c_leqLexGerman x1@((Curry.Module.Prelude.:<) x3 x4) x2 st = Curry.Module.Sort.c_leqLexGerman_case_11(x3)(x4)(x2)(st)
c_leqLexGerman (Curry.Module.Prelude.ListOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_leqLexGerman(x)(x2)(st))(i)(xs)(st)
c_leqLexGerman x x2 st = Curry.RunTimeSystem.patternFail("Sort.leqLexGerman")(x)



c_leqLexGerman'46glex'4689 :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_leqLexGerman'46glex'4689 x1 st = Curry.Module.Sort.c_leqLexGerman'46glex'4689_case_8(x1)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_62_61(x1)(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('A'))(st))(st))(Curry.Module.Prelude.op_60_61(x1)(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('Z'))(st))(st))(st))(st)



c_leqLexGerman'46glex'4689_case_8 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43(x1)(Curry.Module.Prelude.op_45(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('A'))(st))(st))(st)
c_leqLexGerman'46glex'4689_case_8 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Sort.c_leqLexGerman'46glex'4689_case_7(x1)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(st))(st)
c_leqLexGerman'46glex'4689_case_8 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_leqLexGerman'46glex'4689_case_8(x1)(x)(st))(i)(xs)(st)
c_leqLexGerman'46glex'4689_case_8 x1 x st = Curry.RunTimeSystem.patternFail("Sort.leqLexGerman.glex.89_case_8")(x)



c_leqLexGerman'46glex'4689_case_7 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('a'))(st)
c_leqLexGerman'46glex'4689_case_7 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Sort.c_leqLexGerman'46glex'4689_case_6(x1)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(st))(st)
c_leqLexGerman'46glex'4689_case_7 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_leqLexGerman'46glex'4689_case_7(x1)(x)(st))(i)(xs)(st)
c_leqLexGerman'46glex'4689_case_7 x1 x st = Curry.RunTimeSystem.patternFail("Sort.leqLexGerman.glex.89_case_7")(x)



c_leqLexGerman'46glex'4689_case_6 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('o'))(st)
c_leqLexGerman'46glex'4689_case_6 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Sort.c_leqLexGerman'46glex'4689_case_5(x1)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(st))(st)
c_leqLexGerman'46glex'4689_case_6 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_leqLexGerman'46glex'4689_case_6(x1)(x)(st))(i)(xs)(st)
c_leqLexGerman'46glex'4689_case_6 x1 x st = Curry.RunTimeSystem.patternFail("Sort.leqLexGerman.glex.89_case_6")(x)



c_leqLexGerman'46glex'4689_case_5 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('u'))(st)
c_leqLexGerman'46glex'4689_case_5 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Sort.c_leqLexGerman'46glex'4689_case_4(x1)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(st))(st)
c_leqLexGerman'46glex'4689_case_5 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_leqLexGerman'46glex'4689_case_5(x1)(x)(st))(i)(xs)(st)
c_leqLexGerman'46glex'4689_case_5 x1 x st = Curry.RunTimeSystem.patternFail("Sort.leqLexGerman.glex.89_case_5")(x)



c_leqLexGerman'46glex'4689_case_4 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('a'))(st)
c_leqLexGerman'46glex'4689_case_4 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Sort.c_leqLexGerman'46glex'4689_case_3(x1)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(st))(st)
c_leqLexGerman'46glex'4689_case_4 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_leqLexGerman'46glex'4689_case_4(x1)(x)(st))(i)(xs)(st)
c_leqLexGerman'46glex'4689_case_4 x1 x st = Curry.RunTimeSystem.patternFail("Sort.leqLexGerman.glex.89_case_4")(x)



c_leqLexGerman'46glex'4689_case_3 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('o'))(st)
c_leqLexGerman'46glex'4689_case_3 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Sort.c_leqLexGerman'46glex'4689_case_2(x1)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(st))(st)
c_leqLexGerman'46glex'4689_case_3 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_leqLexGerman'46glex'4689_case_3(x1)(x)(st))(i)(xs)(st)
c_leqLexGerman'46glex'4689_case_3 x1 x st = Curry.RunTimeSystem.patternFail("Sort.leqLexGerman.glex.89_case_3")(x)



c_leqLexGerman'46glex'4689_case_2 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('u'))(st)
c_leqLexGerman'46glex'4689_case_2 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Sort.c_leqLexGerman'46glex'4689_case_1(x1)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))(st))(st)
c_leqLexGerman'46glex'4689_case_2 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_leqLexGerman'46glex'4689_case_2(x1)(x)(st))(i)(xs)(st)
c_leqLexGerman'46glex'4689_case_2 x1 x st = Curry.RunTimeSystem.patternFail("Sort.leqLexGerman.glex.89_case_2")(x)



c_leqLexGerman'46glex'4689_case_1 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('s'))(st)
c_leqLexGerman'46glex'4689_case_1 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Sort.c_leqLexGerman'46glex'4689_case_0(x1)(Curry.Module.Prelude.c_otherwise(st))(st)
c_leqLexGerman'46glex'4689_case_1 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_leqLexGerman'46glex'4689_case_1(x1)(x)(st))(i)(xs)(st)
c_leqLexGerman'46glex'4689_case_1 x1 x st = Curry.RunTimeSystem.patternFail("Sort.leqLexGerman.glex.89_case_1")(x)



c_leqLexGerman'46glex'4689_case_0 x1 x2@Curry.Module.Prelude.C_True st = x1
c_leqLexGerman'46glex'4689_case_0 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_leqLexGerman'46glex'4689_case_0(x1)(x)(st))(i)(xs)(st)
c_leqLexGerman'46glex'4689_case_0 x1 x st = Curry.RunTimeSystem.patternFail("Sort.leqLexGerman.glex.89_case_0")(x)



c_leqLexGerman_case_11 x3 x4 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_False
c_leqLexGerman_case_11 x3 x4 x2@((Curry.Module.Prelude.:<) x5 x6) st = let {x7 = Curry.Module.Sort.c_leqLexGerman'46glex'4689(Curry.Module.Prelude.c_ord(x3)(st))(st)} in let {x8 = Curry.Module.Sort.c_leqLexGerman'46glex'4689(Curry.Module.Prelude.c_ord(x5)(st))(st)} in Curry.Module.Sort.c_leqLexGerman_case_10(x4)(x6)(x7)(x8)(Curry.Module.Prelude.op_61_61(x7)(x8)(st))(st)
c_leqLexGerman_case_11 x3 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_leqLexGerman_case_11(x3)(x4)(x)(st))(i)(xs)(st)
c_leqLexGerman_case_11 x3 x4 x st = Curry.RunTimeSystem.patternFail("Sort.leqLexGerman_case_11")(x)



c_leqLexGerman_case_10 x4 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.Sort.c_leqLexGerman(x4)(x6)(st)
c_leqLexGerman_case_10 x4 x6 x7 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.Sort.c_leqLexGerman_case_9(x7)(x8)(Curry.Module.Prelude.c_otherwise(st))(st)
c_leqLexGerman_case_10 x4 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_leqLexGerman_case_10(x4)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c_leqLexGerman_case_10 x4 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("Sort.leqLexGerman_case_10")(x)



c_leqLexGerman_case_9 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_60(x7)(x8)(st)
c_leqLexGerman_case_9 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_leqLexGerman_case_9(x7)(x8)(x)(st))(i)(xs)(st)
c_leqLexGerman_case_9 x7 x8 x st = Curry.RunTimeSystem.patternFail("Sort.leqLexGerman_case_9")(x)



c_cmpChar_case_13 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_EQ
c_cmpChar_case_13 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Sort.c_cmpChar_case_12(x1)(x2)(Curry.Module.Prelude.op_60(Curry.Module.Prelude.c_ord(x1)(st))(Curry.Module.Prelude.c_ord(x2)(st))(st))(st)
c_cmpChar_case_13 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_cmpChar_case_13(x1)(x2)(x)(st))(i)(xs)(st)
c_cmpChar_case_13 x1 x2 x st = Curry.RunTimeSystem.patternFail("Sort.cmpChar_case_13")(x)



c_cmpChar_case_12 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_LT
c_cmpChar_case_12 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_GT
c_cmpChar_case_12 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_cmpChar_case_12(x1)(x2)(x)(st))(i)(xs)(st)
c_cmpChar_case_12 x1 x2 x st = Curry.RunTimeSystem.patternFail("Sort.cmpChar_case_12")(x)



c_cmpList_case_16 x1 x6 x7 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_GT
c_cmpList_case_16 x1 x6 x7 x3@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.Sort.c_cmpList_case_15(x1)(x6)(x7)(x8)(x9)(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x6)(st))(x8)(st))(Curry.Module.Prelude.C_EQ)(st))(st)
c_cmpList_case_16 x1 x6 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_cmpList_case_16(x1)(x6)(x7)(x)(st))(i)(xs)(st)
c_cmpList_case_16 x1 x6 x7 x st = Curry.RunTimeSystem.patternFail("Sort.cmpList_case_16")(x)



c_cmpList_case_15 x1 x6 x7 x8 x9 x10@Curry.Module.Prelude.C_True st = Curry.Module.Sort.c_cmpList(x1)(x7)(x9)(st)
c_cmpList_case_15 x1 x6 x7 x8 x9 x10@Curry.Module.Prelude.C_False st = Curry.Module.Sort.c_cmpList_case_14(x1)(x6)(x8)(Curry.Module.Prelude.c_otherwise(st))(st)
c_cmpList_case_15 x1 x6 x7 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_cmpList_case_15(x1)(x6)(x7)(x8)(x9)(x)(st))(i)(xs)(st)
c_cmpList_case_15 x1 x6 x7 x8 x9 x st = Curry.RunTimeSystem.patternFail("Sort.cmpList_case_15")(x)



c_cmpList_case_14 x1 x6 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x6)(st))(x8)(st)
c_cmpList_case_14 x1 x6 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_cmpList_case_14(x1)(x6)(x8)(x)(st))(i)(xs)(st)
c_cmpList_case_14 x1 x6 x8 x st = Curry.RunTimeSystem.patternFail("Sort.cmpList_case_14")(x)



c_cmpList_case_17 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_EQ
c_cmpList_case_17 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.Prelude.C_LT
c_cmpList_case_17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_cmpList_case_17(x)(st))(i)(xs)(st)
c_cmpList_case_17 x st = Curry.RunTimeSystem.patternFail("Sort.cmpList_case_17")(x)



c_leqList_case_20 x1 x4 x5 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_False
c_leqList_case_20 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.Sort.c_leqList_case_19(x1)(x4)(x5)(x6)(x7)(Curry.Module.Prelude.op_61_61(x4)(x6)(st))(st)
c_leqList_case_20 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_leqList_case_20(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_leqList_case_20 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("Sort.leqList_case_20")(x)



c_leqList_case_19 x1 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.Sort.c_leqList(x1)(x5)(x7)(st)
c_leqList_case_19 x1 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.Sort.c_leqList_case_18(x1)(x4)(x6)(Curry.Module.Prelude.c_otherwise(st))(st)
c_leqList_case_19 x1 x4 x5 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_leqList_case_19(x1)(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c_leqList_case_19 x1 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("Sort.leqList_case_19")(x)



c_leqList_case_18 x1 x4 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x4)(st))(x6)(st)
c_leqList_case_18 x1 x4 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_leqList_case_18(x1)(x4)(x6)(x)(st))(i)(xs)(st)
c_leqList_case_18 x1 x4 x6 x st = Curry.RunTimeSystem.patternFail("Sort.leqList_case_18")(x)



c_merge_case_23 x1 x4 x5 x3@Curry.Module.Prelude.List st = (Curry.Module.Prelude.:<)(x4)(x5)
c_merge_case_23 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.Sort.c_merge_case_22(x1)(x4)(x5)(x6)(x7)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x4)(st))(x6)(st))(st)
c_merge_case_23 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_merge_case_23(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_merge_case_23 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("Sort.merge_case_23")(x)



c_merge_case_22 x1 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x4)(Curry.Module.Sort.c_merge(x1)(x5)((Curry.Module.Prelude.:<)(x6)(x7))(st))
c_merge_case_22 x1 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.Sort.c_merge_case_21(x1)(x4)(x5)(x6)(x7)(Curry.Module.Prelude.c_otherwise(st))(st)
c_merge_case_22 x1 x4 x5 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_merge_case_22(x1)(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c_merge_case_22 x1 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("Sort.merge_case_22")(x)



c_merge_case_21 x1 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x6)(Curry.Module.Sort.c_merge(x1)((Curry.Module.Prelude.:<)(x4)(x5))(x7)(st))
c_merge_case_21 x1 x4 x5 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_merge_case_21(x1)(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c_merge_case_21 x1 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("Sort.merge_case_21")(x)



c_mergeSort'46mergeLists'4616_case_24 x1 x3 x4@Curry.Module.Prelude.List st = x3
c_mergeSort'46mergeLists'4616_case_24 x1 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.Sort.c_mergeSort'46mergeLists'4616(x1)((Curry.Module.Prelude.:<)(Curry.Module.Sort.c_merge(x1)(x3)(x5)(st))(Curry.Module.Sort.c_mergeSort'46mergePairs'4616(x1)(x6)(st)))(st)
c_mergeSort'46mergeLists'4616_case_24 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_mergeSort'46mergeLists'4616_case_24(x1)(x3)(x)(st))(i)(xs)(st)
c_mergeSort'46mergeLists'4616_case_24 x1 x3 x st = Curry.RunTimeSystem.patternFail("Sort.mergeSort.mergeLists.16_case_24")(x)



c_mergeSort'46mergePairs'4616_case_25 x1 x3 x4@Curry.Module.Prelude.List st = (Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List)
c_mergeSort'46mergePairs'4616_case_25 x1 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = (Curry.Module.Prelude.:<)(Curry.Module.Sort.c_merge(x1)(x3)(x5)(st))(Curry.Module.Sort.c_mergeSort'46mergePairs'4616(x1)(x6)(st))
c_mergeSort'46mergePairs'4616_case_25 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_mergeSort'46mergePairs'4616_case_25(x1)(x3)(x)(st))(i)(xs)(st)
c_mergeSort'46mergePairs'4616_case_25 x1 x3 x st = Curry.RunTimeSystem.patternFail("Sort.mergeSort.mergePairs.16_case_25")(x)



c_mergeSort'46genRuns'4616_case_28 x1 x3 x4@Curry.Module.Prelude.List st = (Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.Prelude.List)
c_mergeSort'46genRuns'4616_case_28 x1 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.Sort.c_mergeSort'46genRuns'4616_case_27(x1)(x3)(x5)(x6)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x3)(st))(x5)(st))(st)
c_mergeSort'46genRuns'4616_case_28 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_mergeSort'46genRuns'4616_case_28(x1)(x3)(x)(st))(i)(xs)(st)
c_mergeSort'46genRuns'4616_case_28 x1 x3 x st = Curry.RunTimeSystem.patternFail("Sort.mergeSort.genRuns.16_case_28")(x)



c_mergeSort'46genRuns'4616_case_27 x1 x3 x5 x6 x7@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.Sort.c_mergeSort'46genRuns'4616(x1)(x6)(st))
c_mergeSort'46genRuns'4616_case_27 x1 x3 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.Sort.c_mergeSort'46genRuns'4616_case_26(x1)(x3)(x5)(x6)(Curry.Module.Prelude.c_otherwise(st))(st)
c_mergeSort'46genRuns'4616_case_27 x1 x3 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_mergeSort'46genRuns'4616_case_27(x1)(x3)(x5)(x6)(x)(st))(i)(xs)(st)
c_mergeSort'46genRuns'4616_case_27 x1 x3 x5 x6 x st = Curry.RunTimeSystem.patternFail("Sort.mergeSort.genRuns.16_case_27")(x)



c_mergeSort'46genRuns'4616_case_26 x1 x3 x5 x6 x7@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List)))(Curry.Module.Sort.c_mergeSort'46genRuns'4616(x1)(x6)(st))
c_mergeSort'46genRuns'4616_case_26 x1 x3 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_mergeSort'46genRuns'4616_case_26(x1)(x3)(x5)(x6)(x)(st))(i)(xs)(st)
c_mergeSort'46genRuns'4616_case_26 x1 x3 x5 x6 x st = Curry.RunTimeSystem.patternFail("Sort.mergeSort.genRuns.16_case_26")(x)



c_quickSort'46split'466_case_30 x1 x2 x4 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x4)(x7))(x8)
c_quickSort'46split'466_case_30 x1 x2 x4 x7 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.Sort.c_quickSort'46split'466_case_29(x4)(x7)(x8)(Curry.Module.Prelude.c_otherwise(st))(st)
c_quickSort'46split'466_case_30 x1 x2 x4 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_quickSort'46split'466_case_30(x1)(x2)(x4)(x7)(x8)(x)(st))(i)(xs)(st)
c_quickSort'46split'466_case_30 x1 x2 x4 x7 x8 x st = Curry.RunTimeSystem.patternFail("Sort.quickSort.split.6_case_30")(x)



c_quickSort'46split'466_case_29 x4 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2(x7)((Curry.Module.Prelude.:<)(x4)(x8))
c_quickSort'46split'466_case_29 x4 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Sort.c_quickSort'46split'466_case_29(x4)(x7)(x8)(x)(st))(i)(xs)(st)
c_quickSort'46split'466_case_29 x4 x7 x8 x st = Curry.RunTimeSystem.patternFail("Sort.quickSort.split.6_case_29")(x)


