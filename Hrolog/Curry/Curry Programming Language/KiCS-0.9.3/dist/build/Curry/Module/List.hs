{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.List (module Curry.Module.List) where

import Curry.RunTimeSystem
import Curry.Module.Maybe
import Curry.Module.Prelude



-- begin included



-- end included

c_elemIndex :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe Curry.Module.Prelude.C_Int)
c_elemIndex x1 st = Curry.Module.List.c_findIndex(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_61_61(x1)))(st)



c_elemIndices :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)
c_elemIndices x1 st = Curry.Module.Prelude.pf(Curry.Module.List.c_findIndices(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_61_61(x1))))



c_find :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t0)
c_find x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Maybe.c_listToMaybe))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_filter(x1)))(st)



c_findIndex :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe Curry.Module.Prelude.C_Int)
c_findIndex x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Maybe.c_listToMaybe))(Curry.Module.Prelude.pf(Curry.Module.List.c_findIndices(x1)))(st)



c_findIndices :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_findIndices x1 x2 st = Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.List.c_findIndices'46_'35lambda4(x1)))(Curry.Module.Prelude.List)(Curry.Module.Prelude.c_zip(x2)(Curry.Module.Prelude.c_enumFrom(Curry.Module.Prelude.C_Zero)(st))(st))(st)



c_findIndices'46_'35lambda4 :: (Curry t7) => (Curry.Module.Prelude.Prim (t7 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)) -> (Curry.Module.Prelude.T2 t7 Curry.Module.Prelude.C_Int) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_findIndices'46_'35lambda4 x1 x2@(Curry.Module.Prelude.T2 x4 x5) x3 st = Curry.Module.Prelude.op_43_43(Curry.Module.List.c_findIndices'46_'35lambda4_case_11(x1)(x4)(x5)(Curry.Module.Prelude.c_apply(x1)(x4)(st))(st))(x3)(st)
c_findIndices'46_'35lambda4 x1 (Curry.Module.Prelude.T2Or i xs) x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_findIndices'46_'35lambda4(x1)(x)(x3)(st))(i)(xs)(st)
c_findIndices'46_'35lambda4 x1 x x3 st = Curry.RunTimeSystem.patternFail("List.findIndices._#lambda4")(x)



c_nub :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_nub x1 st = Curry.Module.List.c_nubBy(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_61_61))(x1)(st)



c_nubBy :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_nubBy x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_nubBy x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = (Curry.Module.Prelude.:<)(x3)(Curry.Module.List.c_nubBy(x1)(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.pf(Curry.Module.List.c_nubBy'46_'35lambda6(x1)(x3)))(x4)(st))(st))
c_nubBy x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_nubBy(x1)(x)(st))(i)(xs)(st)
c_nubBy x1 x st = Curry.RunTimeSystem.patternFail("List.nubBy")(x)



c_nubBy'46_'35lambda6 :: (Curry t45) => (Curry.Module.Prelude.Prim (t45 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t45 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> t45 -> t45 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_nubBy'46_'35lambda6 x1 x2 x3 st = Curry.Module.Prelude.c_not(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x2)(st))(x3)(st))(st)



c_delete :: (Curry t0) => t0 -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_delete x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_delete x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.List.c_delete_case_10(x1)(x3)(x4)(Curry.Module.Prelude.op_61_61(x1)(x3)(st))(st)
c_delete x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_delete(x1)(x)(st))(i)(xs)(st)
c_delete x1 x st = Curry.RunTimeSystem.patternFail("List.delete")(x)



op_92_92 :: (Curry t0) => (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
op_92_92 x1 x2 st = Curry.Module.Prelude.c_foldl(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.List.c_delete))))(x1)(x2)(st)



c_union :: (Curry t0) => (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_union x1@Curry.Module.Prelude.List x2 st = x2
c_union x1@((Curry.Module.Prelude.:<) x3 x4) x2 st = Curry.Module.List.c_union_case_9(x2)(x3)(x4)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(x3)(st))(x2)(st))(st)
c_union (Curry.Module.Prelude.ListOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_union(x)(x2)(st))(i)(xs)(st)
c_union x x2 st = Curry.RunTimeSystem.patternFail("List.union")(x)



c_intersect :: (Curry t0) => (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_intersect x1@Curry.Module.Prelude.List x2 st = Curry.Module.Prelude.List
c_intersect x1@((Curry.Module.Prelude.:<) x3 x4) x2 st = Curry.Module.List.c_intersect_case_8(x2)(x3)(x4)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(x3)(st))(x2)(st))(st)
c_intersect (Curry.Module.Prelude.ListOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_intersect(x)(x2)(st))(i)(xs)(st)
c_intersect x x2 st = Curry.RunTimeSystem.patternFail("List.intersect")(x)



c_intersperse :: (Curry t0) => t0 -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_intersperse x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_intersperse x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.List.c_intersperse_case_7(x1)(x3)(x4)(st)
c_intersperse x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_intersperse(x1)(x)(st))(i)(xs)(st)
c_intersperse x1 x st = Curry.RunTimeSystem.patternFail("List.intersperse")(x)



c_transpose :: (Curry t0) => (Curry.Module.Prelude.List (Curry.Module.Prelude.List t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List t0)
c_transpose x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_transpose x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.List.c_transpose_case_6(x3)(x2)(st)
c_transpose (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_transpose(x)(st))(i)(xs)(st)
c_transpose x st = Curry.RunTimeSystem.patternFail("List.transpose")(x)



c_partition :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t0)
c_partition x1 x2 st = Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.List.c_partition'46select'4653(x1)))(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(x2)(st)



c_partition'46select'4653 :: (Curry t146) => (Curry.Module.Prelude.Prim (t146 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)) -> t146 -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t146) (Curry.Module.Prelude.List t146)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t146) (Curry.Module.Prelude.List t146)
c_partition'46select'4653 x1 x2 x3@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.List.c_partition'46select'4653_case_5(x1)(x2)(x4)(x5)(Curry.Module.Prelude.c_apply(x1)(x2)(st))(st)
c_partition'46select'4653 x1 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_partition'46select'4653(x1)(x2)(x)(st))(i)(xs)(st)
c_partition'46select'4653 x1 x2 x st = Curry.RunTimeSystem.patternFail("List.partition.select.53")(x)



c_group :: (Curry t0) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List t0))
c_group st = Curry.Module.Prelude.pf(Curry.Module.List.c_groupBy(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_61_61)))



c_groupBy :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List t0)
c_groupBy x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_groupBy x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.Prelude.c_span(Curry.Module.Prelude.c_apply(x1)(x3)(st))(x4)(st)} in (Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(x3)(Curry.Module.List.c_groupBy'46_'35selFP3'35ys(x5)(st)))(Curry.Module.List.c_groupBy(x1)(Curry.Module.List.c_groupBy'46_'35selFP4'35zs(x5)(st))(st))
c_groupBy x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_groupBy(x1)(x)(st))(i)(xs)(st)
c_groupBy x1 x st = Curry.RunTimeSystem.patternFail("List.groupBy")(x)



c_groupBy'46_'35selFP3'35ys :: (Curry t154) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t154) (Curry.Module.Prelude.List t154)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t154
c_groupBy'46_'35selFP3'35ys x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_groupBy'46_'35selFP3'35ys (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_groupBy'46_'35selFP3'35ys(x)(st))(i)(xs)(st)
c_groupBy'46_'35selFP3'35ys x st = Curry.RunTimeSystem.patternFail("List.groupBy._#selFP3#ys")(x)



c_groupBy'46_'35selFP4'35zs :: (Curry t154) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t154) (Curry.Module.Prelude.List t154)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t154
c_groupBy'46_'35selFP4'35zs x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_groupBy'46_'35selFP4'35zs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_groupBy'46_'35selFP4'35zs(x)(st))(i)(xs)(st)
c_groupBy'46_'35selFP4'35zs x st = Curry.RunTimeSystem.patternFail("List.groupBy._#selFP4#zs")(x)



c_replace :: (Curry t0) => t0 -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_replace x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_replace x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.List.c_replace_case_4(x1)(x2)(x4)(x5)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Zero)(st))(st)
c_replace x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_replace(x1)(x2)(x)(st))(i)(xs)(st)
c_replace x1 x2 x st = Curry.RunTimeSystem.patternFail("List.replace")(x)



c_isPrefixOf :: (Curry t0) => (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isPrefixOf x1@Curry.Module.Prelude.List x2 st = Curry.Module.Prelude.C_True
c_isPrefixOf x1@((Curry.Module.Prelude.:<) x3 x4) x2 st = Curry.Module.List.c_isPrefixOf_case_2(x3)(x4)(x2)(st)
c_isPrefixOf (Curry.Module.Prelude.ListOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_isPrefixOf(x)(x2)(st))(i)(xs)(st)
c_isPrefixOf x x2 st = Curry.RunTimeSystem.patternFail("List.isPrefixOf")(x)



c_sortBy :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0)
c_sortBy x1 st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.List.c_insertBy(x1)))(Curry.Module.Prelude.List))



c_insertBy :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))) -> t0 -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_insertBy x1 x2 x3@Curry.Module.Prelude.List st = (Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)
c_insertBy x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.List.c_insertBy_case_1(x1)(x2)(x4)(x5)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x2)(st))(x4)(st))(st)
c_insertBy x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_insertBy(x1)(x2)(x)(st))(i)(xs)(st)
c_insertBy x1 x2 x st = Curry.RunTimeSystem.patternFail("List.insertBy")(x)



c_last :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> t0
c_last x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.List.c_last_case_0(x2)(x3)(st)
c_last (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_last(x)(st))(i)(xs)(st)
c_last x st = Curry.RunTimeSystem.patternFail("List.last")(x)



c_last_case_0 x2 x3@Curry.Module.Prelude.List st = x2
c_last_case_0 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.List.c_last((Curry.Module.Prelude.:<)(x4)(x5))(st)
c_last_case_0 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_last_case_0(x2)(x)(st))(i)(xs)(st)
c_last_case_0 x2 x st = Curry.RunTimeSystem.patternFail("List.last_case_0")(x)



c_insertBy_case_1 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x4)(x5))
c_insertBy_case_1 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(x4)(Curry.Module.List.c_insertBy(x1)(x2)(x5)(st))
c_insertBy_case_1 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_insertBy_case_1(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c_insertBy_case_1 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("List.insertBy_case_1")(x)



c_isPrefixOf_case_2 x3 x4 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_False
c_isPrefixOf_case_2 x3 x4 x2@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x3)(x5)(st))(Curry.Module.List.c_isPrefixOf(x4)(x6)(st))(st)
c_isPrefixOf_case_2 x3 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_isPrefixOf_case_2(x3)(x4)(x)(st))(i)(xs)(st)
c_isPrefixOf_case_2 x3 x4 x st = Curry.RunTimeSystem.patternFail("List.isPrefixOf_case_2")(x)



c_replace_case_4 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x1)(x5)
c_replace_case_4 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.List.c_replace_case_3(x1)(x2)(x4)(x5)(Curry.Module.Prelude.c_otherwise(st))(st)
c_replace_case_4 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_replace_case_4(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c_replace_case_4 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("List.replace_case_4")(x)



c_replace_case_3 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x4)(Curry.Module.List.c_replace(x1)(Curry.Module.Prelude.op_45(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x5)(st))
c_replace_case_3 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_replace_case_3(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c_replace_case_3 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("List.replace_case_3")(x)



c_partition'46select'4653_case_5 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x2)(x4))(x5)
c_partition'46select'4653_case_5 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.T2(x4)((Curry.Module.Prelude.:<)(x2)(x5))
c_partition'46select'4653_case_5 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_partition'46select'4653_case_5(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c_partition'46select'4653_case_5 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("List.partition.select.53_case_5")(x)



c_transpose_case_6 x3 x2@Curry.Module.Prelude.List st = Curry.Module.List.c_transpose(x3)(st)
c_transpose_case_6 x3 x2@((Curry.Module.Prelude.:<) x4 x5) st = (Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_head))(x3)(st)))(Curry.Module.List.c_transpose((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_tail))(x3)(st)))(st))
c_transpose_case_6 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_transpose_case_6(x3)(x)(st))(i)(xs)(st)
c_transpose_case_6 x3 x st = Curry.RunTimeSystem.patternFail("List.transpose_case_6")(x)



c_intersperse_case_7 x1 x3 x4@Curry.Module.Prelude.List st = (Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List)
c_intersperse_case_7 x1 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = (Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x1)(Curry.Module.List.c_intersperse(x1)((Curry.Module.Prelude.:<)(x5)(x6))(st)))
c_intersperse_case_7 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_intersperse_case_7(x1)(x3)(x)(st))(i)(xs)(st)
c_intersperse_case_7 x1 x3 x st = Curry.RunTimeSystem.patternFail("List.intersperse_case_7")(x)



c_intersect_case_8 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x3)(Curry.Module.List.c_intersect(x4)(x2)(st))
c_intersect_case_8 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.List.c_intersect(x4)(x2)(st)
c_intersect_case_8 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_intersect_case_8(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_intersect_case_8 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("List.intersect_case_8")(x)



c_union_case_9 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.List.c_union(x4)(x2)(st)
c_union_case_9 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(x3)(Curry.Module.List.c_union(x4)(x2)(st))
c_union_case_9 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_union_case_9(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_union_case_9 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("List.union_case_9")(x)



c_delete_case_10 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = x4
c_delete_case_10 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(x3)(Curry.Module.List.c_delete(x1)(x4)(st))
c_delete_case_10 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_delete_case_10(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_delete_case_10 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("List.delete_case_10")(x)



c_findIndices'46_'35lambda4_case_11 x1 x4 x5 x6@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)
c_findIndices'46_'35lambda4_case_11 x1 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.List
c_findIndices'46_'35lambda4_case_11 x1 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.List.c_findIndices'46_'35lambda4_case_11(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c_findIndices'46_'35lambda4_case_11 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("List.findIndices._#lambda4_case_11")(x)


