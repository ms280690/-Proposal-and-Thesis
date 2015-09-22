{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Wrapper (module Curry.Module.Wrapper) where

import Curry.RunTimeSystem
import Curry.Module.FileGoodies
import Curry.Module.FlatCurry
import Curry.Module.Prelude



-- begin included



-- end included

c_prelude :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_prelude st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))



c_eventMod :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_eventMod st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))))))



c_oracle :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_oracle st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))



c_ioexts :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_ioexts st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))



c_newModName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_newModName x1 st = let {x2 = Curry.Module.FileGoodies.c_splitDirectoryBaseName(x1)(st)} in let {x3 = Curry.Module.Wrapper.c_newModName'46_'35selFP3'35d(x2)(st)} in Curry.Module.Prelude.op_43_43(Curry.Module.Wrapper.c_newModName_case_12(x3)(st))(Curry.Module.Prelude.op_43_43(Curry.Module.Wrapper.c_oracle(st))(Curry.Module.Wrapper.c_newModName'46_'35selFP4'35f(x2)(st))(st))(st)



c_newModName'46_'35selFP3'35d :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_newModName'46_'35selFP3'35d x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_newModName'46_'35selFP3'35d (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_newModName'46_'35selFP3'35d(x)(st))(i)(xs)(st)
c_newModName'46_'35selFP3'35d x st = Curry.RunTimeSystem.patternFail("Wrapper.newModName._#selFP3#d")(x)



c_newModName'46_'35selFP4'35f :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_newModName'46_'35selFP4'35f x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_newModName'46_'35selFP4'35f (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_newModName'46_'35selFP4'35f(x)(st))(i)(xs)(st)
c_newModName'46_'35selFP4'35f x st = Curry.RunTimeSystem.patternFail("Wrapper.newModName._#selFP4#f")(x)



c_newModNameQ :: (Curry t0) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) t0
c_newModNameQ x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_newModName(x2)(st))(x3)
c_newModNameQ (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_newModNameQ(x)(st))(i)(xs)(st)
c_newModNameQ x st = Curry.RunTimeSystem.patternFail("Wrapper.newModNameQ")(x)



c_tRef :: Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_TypeExpr
c_tRef st = Curry.Module.FlatCurry.C_TCons(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_eventMod(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))(Curry.Module.Prelude.List)))))(Curry.Module.Prelude.List)



c_tR :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_TypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_TypeExpr)
c_tR st = Curry.Module.Prelude.pc(Curry.Module.FlatCurry.C_FuncType(Curry.Module.Wrapper.c_tRef(st)))



c_lET :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Expr)) -> Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_lET x1 x2 st = Curry.Module.Wrapper.c_lET_case_9(x1)(x2)(Curry.Module.Prelude.c_null(x1)(st))(st)



c_ref :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ref st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi)



c_flatFunc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr) -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr))
c_flatFunc st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pc))(Curry.Module.FlatCurry.C_Comb(Curry.Module.FlatCurry.C_FuncCall))



c_flatCons :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr) -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr))
c_flatCons st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pc))(Curry.Module.FlatCurry.C_Comb(Curry.Module.FlatCurry.C_ConsCall))



c_func :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr) -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr)
c_func x1 st = Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_flatFunc(st))(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_eventMod(st))(x1))(st)



c_list :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr) -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr)
c_list st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Wrapper.c_colon))(Curry.Module.Wrapper.c_nil(st)))



c_char :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr)
c_char st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc(Curry.Module.FlatCurry.C_Lit))(Curry.Module.Prelude.pc(Curry.Module.FlatCurry.C_Charc))(st)



c_string :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr)
c_string st = Curry.Module.Prelude.op_46(Curry.Module.Wrapper.c_list(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Wrapper.c_char(st))))(st)



c_nil :: Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_nil st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_flatCons(st))(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))))(st))(Curry.Module.Prelude.List)(st)



c_colon :: Curry.Module.FlatCurry.C_Expr -> Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_colon x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_flatCons(st))(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List)))(st))((Curry.Module.Prelude.:<)(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)))(st)



c_run :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_run x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_func((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List))))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_string(st))(x1)(st))((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)))(st)



c_event :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_event x1@Curry.Module.Prelude.List x2 x3 x4 st = Curry.Module.Wrapper.c_unfold(x2)(x3)(x4)(st)
c_event x1@((Curry.Module.Prelude.:<) x5 x6) x2 x3 x4 st = Curry.Module.Wrapper.c_collapse(x2)(x4)(st)
c_event (Curry.Module.Prelude.ListOr i xs) x2 x3 x4 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_event(x)(x2)(x3)(x4)(st))(i)(xs)(st)
c_event x x2 x3 x4 st = Curry.RunTimeSystem.patternFail("Wrapper.event")(x)



c_collapse :: Curry.Module.Prelude.C_Int -> Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_collapse x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_func((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))))(st))((Curry.Module.Prelude.:<)(Curry.Module.FlatCurry.C_Var(x1))((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)))(st)



c_closeRef :: Curry.Module.Prelude.C_Int -> Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_closeRef x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_func((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))(Curry.Module.Prelude.List)))))))))(st))((Curry.Module.Prelude.:<)(Curry.Module.FlatCurry.C_Var(x1))((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)))(st)



c_extIO :: Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_extIO x1 st = Curry.Module.Wrapper.c_listFunc(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_oracle(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))(Curry.Module.Prelude.List))))))))))))))((Curry.Module.Prelude.:<)(x1)(Curry.Module.Prelude.List))(st)



c_unfold :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_unfold x1 x2 x3 st = Curry.Module.Wrapper.c_unfold_case_8(x1)(x2)(x3)(Curry.Module.Prelude.c_null(x2)(st))(st)



c_replace :: Curry.Module.Prelude.C_Int -> Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_replace x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_func((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(st))((Curry.Module.Prelude.:<)(Curry.Module.FlatCurry.C_Var(x1))((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)))(st)



c_expand :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_expand x1 x2 x3 st = Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_func((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))(Curry.Module.Prelude.List)))))))(st))((Curry.Module.Prelude.:<)(Curry.Module.FlatCurry.C_Var(x1))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_list(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pc(Curry.Module.FlatCurry.C_Var))(x2)(st))(st))((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))))(st)



c_fresh :: Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_fresh st = Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_func((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))(Curry.Module.Prelude.List))))))(st))((Curry.Module.Prelude.:<)(Curry.Module.Wrapper.c_unit(st))(Curry.Module.Prelude.List))(st)



c_unit :: Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_unit st = Curry.Module.FlatCurry.C_Comb(Curry.Module.FlatCurry.C_ConsCall)(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.List)



c_apply :: Curry.Module.FlatCurry.C_Expr -> Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_apply x1@(Curry.Module.FlatCurry.C_Comb x3 x4 x5) x2 st = Curry.Module.Wrapper.c_apply_case_7(x1)(x2)(x4)(x5)(x3)(st)
c_apply x1@(Curry.Module.FlatCurry.C_Var x8) x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_flatFunc(st))(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st))((Curry.Module.Prelude.:<)(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)))(st)
c_apply x1@(Curry.Module.FlatCurry.C_Lit x9) x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_flatFunc(st))(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st))((Curry.Module.Prelude.:<)(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)))(st)
c_apply x1@(Curry.Module.FlatCurry.C_Let x10 x11) x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_flatFunc(st))(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st))((Curry.Module.Prelude.:<)(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)))(st)
c_apply x1@(Curry.Module.FlatCurry.C_Free x12 x13) x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_flatFunc(st))(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st))((Curry.Module.Prelude.:<)(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)))(st)
c_apply x1@(Curry.Module.FlatCurry.C_Or x14 x15) x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_flatFunc(st))(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st))((Curry.Module.Prelude.:<)(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)))(st)
c_apply x1@(Curry.Module.FlatCurry.C_Case x16 x17 x18) x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_flatFunc(st))(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st))((Curry.Module.Prelude.:<)(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)))(st)
c_apply (Curry.Module.FlatCurry.C_ExprOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_apply(x)(x2)(st))(i)(xs)(st)
c_apply x x2 st = Curry.RunTimeSystem.patternFail("Wrapper.apply")(x)



c_listFunc :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr) -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_listFunc x1 x2 x3 st = let {x4 = Curry.Module.Prelude.op_45(x1)(Curry.Module.Prelude.c_length(x3)(st))(st)} in Curry.Module.Wrapper.c_listFunc_case_4(x2)(x3)(x4)(Curry.Module.Prelude.op_61_61(x4)(Curry.Module.Prelude.C_Zero)(st))(st)



c_compose :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr) -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr)
c_compose st = Curry.Module.Prelude.pf(Curry.Module.Wrapper.c_listFunc(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_oracle(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))))



c_unknown :: Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_unknown x1 st = Curry.Module.Wrapper.c_listFunc(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_oracle(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List)))))))))((Curry.Module.Prelude.:<)(x1)(Curry.Module.Prelude.List))(st)



c_oracleTry :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr) -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_oracleTry x1 x2 st = let {x3 = Curry.Module.Prelude.c_splitAt(Curry.Module.Prelude.op_45(Curry.Module.Prelude.c_length(x2)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(x2)(st)} in Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_func((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))))(st))((Curry.Module.Prelude.:<)(Curry.Module.FlatCurry.C_Comb(Curry.Module.FlatCurry.C_FuncPartCall(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))(x1)(Curry.Module.Wrapper.c_oracleTry'46_'35selFP6'35argsf(x3)(st)))(Curry.Module.Wrapper.c_oracleTry'46_'35selFP7'35argst(x3)(st)))(st)



c_oracleTry'46_'35selFP6'35argsf :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr
c_oracleTry'46_'35selFP6'35argsf x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_oracleTry'46_'35selFP6'35argsf (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_oracleTry'46_'35selFP6'35argsf(x)(st))(i)(xs)(st)
c_oracleTry'46_'35selFP6'35argsf x st = Curry.RunTimeSystem.patternFail("Wrapper.oracleTry._#selFP6#argsf")(x)



c_oracleTry'46_'35selFP7'35argst :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr) (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr
c_oracleTry'46_'35selFP7'35argst x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_oracleTry'46_'35selFP7'35argst (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_oracleTry'46_'35selFP7'35argst(x)(st))(i)(xs)(st)
c_oracleTry'46_'35selFP7'35argst x st = Curry.RunTimeSystem.patternFail("Wrapper.oracleTry._#selFP7#argst")(x)



c_partCons :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr))
c_partCons st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Wrapper.c_apply))(Curry.Module.Prelude.pf(Curry.Module.Wrapper.c_partial(Curry.Module.Wrapper.c_pc(st))))(st)



c_partFunc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr))
c_partFunc st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Wrapper.c_apply))(Curry.Module.Prelude.pf(Curry.Module.Wrapper.c_partial(Curry.Module.Wrapper.c_pf(st))))(st)



c_pc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr) -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr)
c_pc st = Curry.Module.Prelude.pf(Curry.Module.Wrapper.c_listFunc(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_oracle(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))))))))



c_pf :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr) -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr)
c_pf st = Curry.Module.Prelude.pf(Curry.Module.Wrapper.c_listFunc(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_oracle(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))(Curry.Module.Prelude.List)))))))))))



c_partCall :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr) -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr)
c_partCall st = Curry.Module.Prelude.pf(Curry.Module.Wrapper.c_listFunc(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_oracle(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))))))))



c_partial :: (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr) -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr)) -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_partial x1 x2 st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_foldr1(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Wrapper.c_partial'46_'35lambda4))))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Wrapper.c_partial'46_'35lambda5))))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_reverse(st))(Curry.Module.Prelude.c_zip(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_reverse(st))(Curry.Module.Prelude.c_enumFromTo(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x2)(st))(st))((Curry.Module.Prelude.:<)(x1)(Curry.Module.Prelude.c_repeat(Curry.Module.Wrapper.c_partCall(st))(st)))(st))(st))(st)



c_partial'46_'35lambda4 :: Curry.Module.FlatCurry.C_Expr -> Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_partial'46_'35lambda4 x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_compose(st))((Curry.Module.Prelude.:<)(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)))(st)



c_partial'46_'35lambda5 :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr) -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr))) -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_partial'46_'35lambda5 x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.Wrapper.c_dotted(Curry.Module.Prelude.op_45(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.Prelude.c_apply(x3)(Curry.Module.Prelude.List)(st))(st)
c_partial'46_'35lambda5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_partial'46_'35lambda5(x)(st))(i)(xs)(st)
c_partial'46_'35lambda5 x st = Curry.RunTimeSystem.patternFail("Wrapper.partial._#lambda5")(x)



c_dotted :: Curry.Module.Prelude.C_Int -> Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_dotted x1 x2 st = Curry.Module.Wrapper.c_dotted_case_2(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Zero)(st))(st)



c_inOraclePartCall :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr) -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr)
c_inOraclePartCall x1 st = Curry.Module.Prelude.pf(Curry.Module.Wrapper.c_listFunc(Curry.Module.Wrapper.c_arityInOracle(x1)(st))(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_oracle(st))(x1)))



c_errorCall :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_errorCall x1 st = Curry.Module.FlatCurry.C_Comb(Curry.Module.FlatCurry.C_FuncCall)(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_string(st))(x1)(st))(Curry.Module.Prelude.List))



c_safeIOResult :: Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_safeIOResult x1 st = Curry.Module.Wrapper.c_listFunc(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_oracle(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))))))))))))((Curry.Module.Prelude.:<)(x1)(Curry.Module.Prelude.List))(st)



c_specialIOs :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_specialIOs st = Curry.Module.Prelude.c_zip(Curry.Module.Prelude.c_repeat(Curry.Module.Wrapper.c_prelude(st))(st))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List))))))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))))))))(Curry.Module.Prelude.List))))(st)



c_implementedInOracle :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_implementedInOracle st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('U'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))(Curry.Module.Prelude.List))))))))))))))))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))(Curry.Module.Prelude.List)))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))(Curry.Module.Prelude.List))))))))))))))))))(Curry.Module.Prelude.c_zip(Curry.Module.Prelude.c_repeat(Curry.Module.Wrapper.c_prelude(st))(st))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('$'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))(Curry.Module.Prelude.List)))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('$'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('$'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('#'))(Curry.Module.Prelude.List)))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('$'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('#'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('#'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List))))))))))(Curry.Module.Prelude.List))))))))(st)))



c_arityInOracle :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_arityInOracle x1 st = Curry.Module.Wrapper.c_arityInOracle_case_0(x1)(Curry.Module.Prelude.op_61_61(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))(Curry.Module.Prelude.List)))))))))))))))))))))(st))(st)



c_addOrc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_addOrc x1 x2 x3 st = Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_maybe(x1)(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id))(x2)(st))(Curry.Module.Wrapper.c_newModName(x3)(st))(st)



c_addFcy :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_addFcy st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_43_43))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))



c_arityInOracle_case_0 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))
c_arityInOracle_case_0 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))
c_arityInOracle_case_0 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_arityInOracle_case_0(x1)(x)(st))(i)(xs)(st)
c_arityInOracle_case_0 x1 x st = Curry.RunTimeSystem.patternFail("Wrapper.arityInOracle_case_0")(x)



c_dotted_case_2 x1 x2 x3@Curry.Module.Prelude.C_True st = x2
c_dotted_case_2 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Wrapper.c_dotted_case_1(x1)(x2)(Curry.Module.Prelude.c_otherwise(st))(st)
c_dotted_case_2 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_dotted_case_2(x1)(x2)(x)(st))(i)(xs)(st)
c_dotted_case_2 x1 x2 x st = Curry.RunTimeSystem.patternFail("Wrapper.dotted_case_2")(x)



c_dotted_case_1 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Wrapper.c_dotted(Curry.Module.Prelude.op_45(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_compose(st))((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(st))(st)
c_dotted_case_1 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_dotted_case_1(x1)(x2)(x)(st))(i)(xs)(st)
c_dotted_case_1 x1 x2 x st = Curry.RunTimeSystem.patternFail("Wrapper.dotted_case_1")(x)



c_listFunc_case_4 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.FlatCurry.C_Comb(Curry.Module.FlatCurry.C_FuncCall)(x2)(x3)
c_listFunc_case_4 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Wrapper.c_listFunc_case_3(x2)(x3)(x4)(Curry.Module.Prelude.c_otherwise(st))(st)
c_listFunc_case_4 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_listFunc_case_4(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_listFunc_case_4 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("Wrapper.listFunc_case_4")(x)



c_listFunc_case_3 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.FlatCurry.C_Comb(Curry.Module.FlatCurry.C_FuncPartCall(x4))(x2)(x3)
c_listFunc_case_3 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_listFunc_case_3(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_listFunc_case_3 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("Wrapper.listFunc_case_3")(x)



c_apply_case_7 x1 x2 x4 x5 x3@(Curry.Module.FlatCurry.C_FuncPartCall x6) st = Curry.Module.Wrapper.c_apply_case_6(x2)(x4)(x5)(x6)(Curry.Module.Prelude.op_61_61(x6)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st)
c_apply_case_7 x1 x2 x4 x5 x3@(Curry.Module.FlatCurry.C_ConsPartCall x7) st = Curry.Module.Wrapper.c_apply_case_5(x2)(x4)(x5)(x7)(Curry.Module.Prelude.op_61_61(x7)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st)
c_apply_case_7 x1 x2 x4 x5 x3@Curry.Module.FlatCurry.C_FuncCall st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_flatFunc(st))(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st))((Curry.Module.Prelude.:<)(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)))(st)
c_apply_case_7 x1 x2 x4 x5 x3@Curry.Module.FlatCurry.C_ConsCall st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Wrapper.c_flatFunc(st))(Curry.Module.Prelude.T2(Curry.Module.Wrapper.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st))((Curry.Module.Prelude.:<)(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)))(st)
c_apply_case_7 x1 x2 x4 x5 (Curry.Module.FlatCurry.C_CombTypeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_apply_case_7(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c_apply_case_7 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("Wrapper.apply_case_7")(x)



c_apply_case_5 x2 x4 x5 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.FlatCurry.C_Comb(Curry.Module.FlatCurry.C_ConsCall)(x4)(Curry.Module.Prelude.op_43_43(x5)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(st))
c_apply_case_5 x2 x4 x5 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.FlatCurry.C_Comb(Curry.Module.FlatCurry.C_ConsPartCall(Curry.Module.Prelude.op_45(x7)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st)))(x4)(Curry.Module.Prelude.op_43_43(x5)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(st))
c_apply_case_5 x2 x4 x5 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_apply_case_5(x2)(x4)(x5)(x7)(x)(st))(i)(xs)(st)
c_apply_case_5 x2 x4 x5 x7 x st = Curry.RunTimeSystem.patternFail("Wrapper.apply_case_5")(x)



c_apply_case_6 x2 x4 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.FlatCurry.C_Comb(Curry.Module.FlatCurry.C_FuncCall)(x4)(Curry.Module.Prelude.op_43_43(x5)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(st))
c_apply_case_6 x2 x4 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.FlatCurry.C_Comb(Curry.Module.FlatCurry.C_FuncPartCall(Curry.Module.Prelude.op_45(x6)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st)))(x4)(Curry.Module.Prelude.op_43_43(x5)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(st))
c_apply_case_6 x2 x4 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_apply_case_6(x2)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_apply_case_6 x2 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("Wrapper.apply_case_6")(x)



c_unfold_case_8 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Wrapper.c_replace(x1)(x3)(st)
c_unfold_case_8 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.Wrapper.c_lET(Curry.Module.Prelude.c_zip(x2)(Curry.Module.Prelude.c_repeat(Curry.Module.Wrapper.c_fresh(st))(st))(st))(Curry.Module.Wrapper.c_expand(x1)(x2)(x3)(st))(st)
c_unfold_case_8 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_unfold_case_8(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_unfold_case_8 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("Wrapper.unfold_case_8")(x)



c_lET_case_9 x1 x2 x3@Curry.Module.Prelude.C_True st = x2
c_lET_case_9 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.FlatCurry.C_Let(x1)(x2)
c_lET_case_9 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_lET_case_9(x1)(x2)(x)(st))(i)(xs)(st)
c_lET_case_9 x1 x2 x st = Curry.RunTimeSystem.patternFail("Wrapper.lET_case_9")(x)



c_newModName_case_12 x3@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.Wrapper.c_newModName_case_11(x3)(x5)(x6)(Curry.Module.Prelude.op_61_61(x5)(Curry.Module.Prelude.C_Char('.'))(st))(st)
c_newModName_case_12 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.op_43_43(x3)((Curry.Module.Prelude.:<)(Curry.Module.FileGoodies.c_separatorChar(st))(Curry.Module.Prelude.List))(st)
c_newModName_case_12 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_newModName_case_12(x)(st))(i)(xs)(st)
c_newModName_case_12 x st = Curry.RunTimeSystem.patternFail("Wrapper.newModName_case_12")(x)



c_newModName_case_11 x3 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Wrapper.c_newModName_case_10(x3)(x6)(st)
c_newModName_case_11 x3 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_43_43(x3)((Curry.Module.Prelude.:<)(Curry.Module.FileGoodies.c_separatorChar(st))(Curry.Module.Prelude.List))(st)
c_newModName_case_11 x3 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_newModName_case_11(x3)(x5)(x6)(x)(st))(i)(xs)(st)
c_newModName_case_11 x3 x5 x6 x st = Curry.RunTimeSystem.patternFail("Wrapper.newModName_case_11")(x)



c_newModName_case_10 x3 x6@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_newModName_case_10 x3 x6@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.Prelude.op_43_43(x3)((Curry.Module.Prelude.:<)(Curry.Module.FileGoodies.c_separatorChar(st))(Curry.Module.Prelude.List))(st)
c_newModName_case_10 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Wrapper.c_newModName_case_10(x3)(x)(st))(i)(xs)(st)
c_newModName_case_10 x3 x st = Curry.RunTimeSystem.patternFail("Wrapper.newModName_case_10")(x)


