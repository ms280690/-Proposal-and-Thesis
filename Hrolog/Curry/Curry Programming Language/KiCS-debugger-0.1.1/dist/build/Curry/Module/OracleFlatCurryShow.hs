{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleFlatCurryShow (module Curry.Module.OracleFlatCurryShow) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.FlatCurryShow
import Curry.Module.Char
import Curry.Module.FlatCurry
import Curry.Module.List
import Curry.Module.Prelude
import Curry.Module.OracleChar
import Curry.Module.OracleFlatCurry
import Curry.Module.OracleList
import Curry.Module.OraclePrelude



-- begin included



-- end included

c_showFlatProg :: Curry.Module.FlatCurry.C_Prog -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFlatProg x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_122(x2)(x1)(st))(st)



c_showFlatVisibility :: Curry.Module.FlatCurry.C_Visibility -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFlatVisibility x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_119(x2)(x1)(st))(st)



c_showFlatFixity :: Curry.Module.FlatCurry.C_Fixity -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFlatFixity x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_118(x2)(x1)(st))(st)



c_showFlatOp :: Curry.Module.FlatCurry.C_OpDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFlatOp x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_117(x2)(x1)(st))(st)



c_showFlatType :: Curry.Module.FlatCurry.C_TypeDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFlatType x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_116(x2)(x1)(st))(st)



c_showFlatCons :: Curry.Module.FlatCurry.C_ConsDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFlatCons x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_115(x2)(x1)(st))(st)



c_showFlatFunc :: Curry.Module.FlatCurry.C_FuncDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFlatFunc x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_114(x2)(x1)(st))(st)



c_showFlatRule :: Curry.Module.FlatCurry.C_Rule -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFlatRule x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_113(x2)(x1)(st))(st)



c_showFlatTypeExpr :: Curry.Module.FlatCurry.C_TypeExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFlatTypeExpr x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_112(x2)(x1)(st))(st)



c_showFlatCombType :: Curry.Module.FlatCurry.C_CombType -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFlatCombType x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_111(x2)(x1)(st))(st)



c_showFlatExpr :: Curry.Module.FlatCurry.C_Expr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFlatExpr x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_110(x2)(x1)(st))(st)



c_showFlatExpr'46showFlatBinding'4649 :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Expr) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFlatExpr'46showFlatBinding'4649 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_108(x2)(x1)(st))(st)



c_showFlatLit :: Curry.Module.FlatCurry.C_Literal -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFlatLit x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_107(x2)(x1)(st))(st)



c_showFlatBranch :: Curry.Module.FlatCurry.C_BranchExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFlatBranch x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_106(x2)(x1)(st))(st)



c_showFlatPattern :: Curry.Module.FlatCurry.C_Pattern -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFlatPattern x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_105(x2)(x1)(st))(st)



c_showFlatList :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFlatList x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatListElems(x2)(x3)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(x4)(st))(x5)(st))(st)



c_showFlatListElems :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFlatListElems x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_concat(Curry.Module.OracleList.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_map(x2)(x3)(x1)(st))(x4)(st))(x5)(st))(st)



c_showCurryType :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.Prelude.C_Bool -> Curry.Module.FlatCurry.C_TypeExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCurryType x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_104(x2)(x3)(x4)(x1)(st))(st)



c_showCurryType'46_'35lambda2 :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.FlatCurry.C_TypeExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCurryType'46_'35lambda2 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.OracleFlatCurryShow.c_showCurryType(x2)(Curry.Module.Prelude.C_True)(x3)(x1)(st)))(st)



c_isFuncType :: Curry.Module.FlatCurry.C_TypeExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isFuncType x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_97(x2)(x1)(st))(st)



c_showCurryExpr :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.Prelude.C_Bool -> Curry.Module.Prelude.C_Int -> Curry.Module.FlatCurry.C_Expr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCurryExpr x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_96(x2)(x3)(x4)(x5)(x1)(st))(st)



c_showCurryExpr'46_'35lambda3 :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Expr) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCurryExpr'46_'35lambda3 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_81(x2)(x3)(x4)(x1)(st))(st)



c_showCurryVar :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCurryVar x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))(x3)(st))(st)



c_showCurryId :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCurryId x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OracleFlatCurryShow.c__case_80(x2)(Curry.Module.OracleChar.c_isAlpha(Curry.Module.OraclePrelude.c_head(x2)(x1)(st))(x3)(st))(x4)(st))(st)



c_showCurryLit :: Curry.Module.FlatCurry.C_Literal -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCurryLit x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_77(x2)(x1)(st))(st)



c_showCurryCase :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.Prelude.C_Int -> Curry.Module.FlatCurry.C_BranchExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCurryCase x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_76(x2)(x3)(x4)(x1)(st))(st)



c_showCurryCase'46showPattern'46151 :: (Curry t0) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCurryCase'46showPattern'46151 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_74(x2)(x3)(x1)(st))(st)



c_showCurryFiniteList :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.Prelude.C_Int -> Curry.Module.FlatCurry.C_Expr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_showCurryFiniteList x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_68(x2)(x3)(x4)(x1)(st))(st)



c_showCurryStringConstant :: Curry.Module.FlatCurry.C_Expr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCurryStringConstant x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_41(x2)(x1)(st))(st)



c_showCharExpr :: Curry.Module.FlatCurry.C_Expr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCharExpr x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_14(x2)(x1)(st))(st)



c_showCurryElems :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCurryElems x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_concat(Curry.Module.OracleList.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_map(x2)(x3)(x1)(st))(x4)(st))(x5)(st))(st)



c_showBracketsIf :: Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showBracketsIf x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_7(x3)(x2)(x1)(st))(st)



c_sceBlanks :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_sceBlanks x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_take(x2)(Curry.Module.OraclePrelude.c_repeat(Curry.Module.Prelude.C_Char(' '))(x1)(st))(x3)(st))(st)



c_isFiniteList :: Curry.Module.FlatCurry.C_Expr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isFiniteList x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_6(x2)(x1)(st))(st)



c_isStringConstant :: Curry.Module.FlatCurry.C_Expr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isStringConstant x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_2(x2)(x1)(st))(st)



c_isCharConstant :: Curry.Module.FlatCurry.C_Expr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isCharConstant x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_1(x2)(x1)(st))(st)



c__case_1 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_1_case__122(x1)(x2)(st))(st)



c__case_0 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_0_case__121(x1)(x3)(st))(st)



c__case_2 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_2_case__120(x1)(x2)(st))(st)



c__case_6 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_6_case__119(x1)(x2)(st))(st)



c__case_5 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_5_case__118(x1)(x6)(x7)(x8)(st))(st)



c__case_4 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_4_case__117(x1)(x7)(x8)(st))(st)



c__case_3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_3_case__116(x1)(x2)(st))(st)



c__case_7 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_7_case__115(x1)(x3)(x2)(st))(st)



c__case_14 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_14_case__114(x1)(x2)(st))(st)



c__case_13 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_13_case__113(x1)(x3)(st))(st)



c__case_12 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_12_case__112(x1)(x4)(x5)(x6)(st))(st)



c__case_11 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_11_case__111(x1)(x4)(x5)(x6)(st))(st)



c__case_10 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_10_case__110(x1)(x4)(x5)(x6)(st))(st)



c__case_9 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_9_case__109(x1)(x4)(x5)(x6)(st))(st)



c__case_8 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_8_case__108(x1)(x4)(x5)(st))(st)



c__case_41 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_41_case__107(x1)(x2)(st))(st)



c__case_40 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_40_case__106(x1)(x5)(x4)(st))(st)



c__case_39 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_39_case__105(x1)(x5)(x7)(x6)(st))(st)



c__case_38 x5 x7 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_38_case__104(x1)(x5)(x7)(x9)(x8)(st))(st)



c__case_37 x5 x7 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_37_case__103(x1)(x5)(x7)(x9)(st))(st)



c__case_36 x5 x7 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_36_case__102(x1)(x5)(x7)(x11)(x10)(st))(st)



c__case_35 x5 x7 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_35_case__101(x1)(x5)(x7)(x11)(st))(st)



c__case_34 x5 x7 x13 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_34_case__100(x1)(x5)(x7)(x13)(x12)(st))(st)



c__case_33 x5 x7 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_33_case__99(x1)(x5)(x7)(x13)(st))(st)



c__case_32 x5 x7 x15 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_32_case__98(x1)(x5)(x7)(x15)(x14)(st))(st)



c__case_31 x5 x7 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_31_case__97(x1)(x5)(x7)(x15)(st))(st)



c__case_30 x5 x7 x17 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_30_case__96(x1)(x5)(x7)(x17)(x16)(st))(st)



c__case_29 x5 x7 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_29_case__95(x1)(x5)(x7)(x17)(st))(st)



c__case_28 x5 x7 x19 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_28_case__94(x1)(x5)(x7)(x19)(x18)(st))(st)



c__case_27 x5 x7 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_27_case__93(x1)(x5)(x7)(x19)(st))(st)



c__case_26 x5 x7 x21 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_26_case__92(x1)(x5)(x7)(x21)(x20)(st))(st)



c__case_25 x5 x7 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_25_case__91(x1)(x5)(x7)(x21)(st))(st)



c__case_24 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_24_case__90(x1)(x5)(x7)(st))(st)



c__case_23 x5 x23 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_23_case__89(x1)(x5)(x23)(x22)(st))(st)



c__case_18 x5 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_18_case__88(x1)(x5)(x23)(st))(st)



c__case_17 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_17_case__87(x1)(x5)(st))(st)



c__case_16 x26 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_16_case__86(x1)(x26)(x27)(st))(st)



c__case_15 x26 x28 x29 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_15_case__85(x1)(x26)(x28)(x29)(st))(st)



c__case_22 x5 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_22_case__84(x1)(x5)(x23)(st))(st)



c__case_21 x5 x25 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_21_case__83(x1)(x5)(x25)(x24)(st))(st)



c__case_20 x5 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_20_case__82(x1)(x5)(x25)(st))(st)



c__case_19 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_19_case__81(x1)(x5)(st))(st)



c__case_68 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_68_case__80(x1)(x2)(x3)(x4)(st))(st)



c__case_67 x2 x3 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_67_case__79(x1)(x2)(x3)(x7)(x6)(st))(st)



c__case_66 x2 x3 x7 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_66_case__78(x1)(x2)(x3)(x7)(x9)(x8)(st))(st)



c__case_65 x2 x3 x7 x9 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_65_case__77(x1)(x2)(x3)(x7)(x9)(x11)(x10)(st))(st)



c__case_64 x2 x3 x7 x9 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_64_case__76(x1)(x2)(x3)(x7)(x9)(x11)(st))(st)



c__case_63 x2 x3 x7 x9 x13 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_63_case__75(x1)(x2)(x3)(x7)(x9)(x13)(x12)(st))(st)



c__case_62 x2 x3 x7 x9 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_62_case__74(x1)(x2)(x3)(x7)(x9)(x13)(st))(st)



c__case_61 x2 x3 x7 x9 x15 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_61_case__73(x1)(x2)(x3)(x7)(x9)(x15)(x14)(st))(st)



c__case_60 x2 x3 x7 x9 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_60_case__72(x1)(x2)(x3)(x7)(x9)(x15)(st))(st)



c__case_59 x2 x3 x7 x9 x17 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_59_case__71(x1)(x2)(x3)(x7)(x9)(x17)(x16)(st))(st)



c__case_58 x2 x3 x7 x9 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_58_case__70(x1)(x2)(x3)(x7)(x9)(x17)(st))(st)



c__case_57 x2 x3 x7 x9 x19 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_57_case__69(x1)(x2)(x3)(x7)(x9)(x19)(x18)(st))(st)



c__case_56 x2 x3 x7 x9 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_56_case__68(x1)(x2)(x3)(x7)(x9)(x19)(st))(st)



c__case_55 x2 x3 x7 x9 x21 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_55_case__67(x1)(x2)(x3)(x7)(x9)(x21)(x20)(st))(st)



c__case_54 x2 x3 x7 x9 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_54_case__66(x1)(x2)(x3)(x7)(x9)(x21)(st))(st)



c__case_53 x2 x3 x7 x9 x23 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_53_case__65(x1)(x2)(x3)(x7)(x9)(x23)(x22)(st))(st)



c__case_52 x2 x3 x7 x9 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_52_case__64(x1)(x2)(x3)(x7)(x9)(x23)(st))(st)



c__case_51 x2 x3 x7 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_51_case__63(x1)(x2)(x3)(x7)(x9)(st))(st)



c__case_50 x2 x3 x7 x25 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_50_case__62(x1)(x2)(x3)(x7)(x25)(x24)(st))(st)



c__case_45 x2 x3 x7 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_45_case__61(x1)(x2)(x3)(x7)(x25)(st))(st)



c__case_44 x2 x3 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_44_case__60(x1)(x2)(x3)(x7)(st))(st)



c__case_43 x2 x3 x28 x29 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_43_case__59(x1)(x2)(x3)(x28)(x29)(st))(st)



c__case_42 x2 x3 x28 x30 x31 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_42_case__58(x1)(x2)(x3)(x28)(x30)(x31)(st))(st)



c__case_49 x7 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_49_case__57(x1)(x7)(x25)(st))(st)



c__case_48 x7 x27 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_48_case__56(x1)(x7)(x27)(x26)(st))(st)



c__case_47 x7 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_47_case__55(x1)(x7)(x27)(st))(st)



c__case_46 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_46_case__54(x1)(x7)(st))(st)



c__case_74 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_74_case__53(x1)(x2)(x3)(st))(st)



c__case_73 x2 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_73_case__52(x1)(x2)(x4)(x5)(st))(st)



c__case_72 x2 x4 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_72_case__51(x1)(x2)(x4)(x6)(x7)(st))(st)



c__case_69 x2 x4 x6 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_69_case__50(x1)(x2)(x4)(x6)(x8)(x9)(x10)(st))(st)



c__case_71 x2 x4 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_71_case__49(x1)(x2)(x4)(x6)(x7)(st))(st)



c__case_70 x2 x4 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_70_case__48(x1)(x2)(x4)(x6)(x7)(st))(st)



c__case_76 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_76_case__47(x1)(x2)(x3)(x4)(st))(st)



c__case_75 x2 x3 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_75_case__46(x1)(x2)(x3)(x6)(x5)(st))(st)



c__case_77 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_77_case__45(x1)(x2)(st))(st)



c__case_80 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_80_case__44(x1)(x2)(x3)(st))(st)



c__case_79 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_79_case__43(x1)(x2)(x3)(st))(st)



c__case_78 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_78_case__42(x1)(x2)(x3)(st))(st)



c__case_81 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_81_case__41(x1)(x2)(x3)(x4)(st))(st)



c__case_96 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_96_case__40(x1)(x2)(x3)(x4)(x5)(st))(st)



c__case_82 x25 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_82_case__39(x1)(x26)(st))(st)



c__case_83 x2 x3 x4 x20 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_83_case__38(x1)(x2)(x3)(x4)(x20)(x19)(st))(st)



c__case_95 x2 x3 x4 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_95_case__37(x1)(x2)(x3)(x4)(x8)(x9)(x10)(st))(st)



c__case_94 x2 x3 x4 x8 x9 x11 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_94_case__36(x1)(x2)(x3)(x4)(x8)(x9)(x11)(x12)(st))(st)



c__case_93 x2 x3 x4 x8 x9 x11 x13 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_93_case__35(x1)(x2)(x3)(x4)(x8)(x9)(x11)(x13)(x14)(st))(st)



c__case_86 x2 x3 x4 x9 x11 x13 x15 x16 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_86_case__34(x1)(x2)(x3)(x4)(x9)(x11)(x13)(x15)(x16)(x17)(st))(st)



c__case_85 x2 x3 x4 x9 x11 x13 x15 x16 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_85_case__33(x1)(x2)(x3)(x4)(x9)(x11)(x13)(x15)(x16)(x17)(st))(st)



c__case_84 x2 x3 x4 x9 x11 x13 x15 x16 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_84_case__32(x1)(x2)(x3)(x4)(x9)(x11)(x13)(x15)(x16)(x17)(st))(st)



c__case_92 x2 x3 x4 x8 x9 x11 x13 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_92_case__31(x1)(x2)(x3)(x4)(x8)(x9)(x11)(x13)(x14)(st))(st)



c__case_91 x2 x3 x4 x8 x9 x11 x13 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_91_case__30(x1)(x2)(x3)(x4)(x8)(x9)(x11)(x13)(x14)(st))(st)



c__case_90 x2 x3 x4 x8 x9 x11 x13 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_90_case__29(x1)(x2)(x3)(x4)(x8)(x9)(x11)(x13)(x14)(st))(st)



c__case_88 x2 x3 x4 x9 x11 x13 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_88_case__28(x1)(x2)(x3)(x4)(x9)(x11)(x13)(x14)(st))(st)



c__case_87 x2 x3 x4 x9 x11 x13 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_87_case__27(x1)(x2)(x3)(x4)(x9)(x11)(x13)(x14)(st))(st)



c__case_89 x2 x4 x8 x9 x11 x13 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_89_case__26(x1)(x2)(x4)(x8)(x9)(x11)(x13)(x14)(st))(st)



c__case_97 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_97_case__25(x1)(x2)(st))(st)



c__case_104 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_104_case__24(x1)(x2)(x3)(x4)(st))(st)



c__case_102 x2 x3 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_102_case__23(x1)(x2)(x3)(x8)(x9)(x10)(st))(st)



c__case_101 x2 x3 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_101_case__22(x1)(x2)(x3)(x8)(x9)(x10)(st))(st)



c__case_100 x2 x3 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_100_case__21(x1)(x2)(x3)(x8)(x9)(x10)(st))(st)



c__case_99 x2 x3 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_99_case__20(x1)(x2)(x3)(x8)(x9)(x10)(st))(st)



c__case_98 x2 x3 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_98_case__19(x1)(x2)(x3)(x8)(x9)(x10)(st))(st)



c__case_103 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_103_case__18(x1)(x5)(x6)(st))(st)



c__case_105 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_105_case__17(x1)(x2)(st))(st)



c__case_106 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_106_case__16(x1)(x2)(st))(st)



c__case_107 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_107_case__15(x1)(x2)(st))(st)



c__case_108 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_108_case__14(x1)(x2)(st))(st)



c__case_110 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_110_case__13(x1)(x2)(st))(st)



c__case_109 x15 x16 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_109_case__12(x1)(x15)(x16)(x14)(st))(st)



c__case_111 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_111_case__11(x1)(x2)(st))(st)



c__case_112 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_112_case__10(x1)(x2)(st))(st)



c__case_113 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_113_case__9(x1)(x2)(st))(st)



c__case_114 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_114_case__8(x1)(x2)(st))(st)



c__case_115 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_115_case__7(x1)(x2)(st))(st)



c__case_116 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_116_case__6(x1)(x2)(st))(st)



c__case_117 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_117_case__5(x1)(x2)(st))(st)



c__case_118 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_118_case__4(x1)(x2)(st))(st)



c__case_119 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_119_case__3(x1)(x2)(st))(st)



c__case_122 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_122_case__2(x1)(x2)(st))(st)



c__case_120 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_120_case__1(x1)(x5)(x6)(st))(st)



c__case_121 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_121_case__0(x1)(x4)(x5)(st))(st)



c__case_121_case__0 x1 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))))))(st)
c__case_121_case__0 x1 x4 x5@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatListElems(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_show))))(x4)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))(x6)(st))(x7)(st))(st)
c__case_121_case__0 x1 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_121_case__0(x1)(x4)(x)(st))(i)(xs)(st)
c__case_121_case__0 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_121_case__0")(x)



c__case_120_case__1 x1 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))))))(st)
c__case_120_case__1 x1 x5 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatListElems(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showFlatType))))(x5)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))))(x7)(st))(x8)(st))(st)
c__case_120_case__1 x1 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_120_case__1(x1)(x5)(x)(st))(i)(xs)(st)
c__case_120_case__1 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_120_case__1")(x)



c__case_122_case__2 x1 x2@(Curry.Module.FlatCurry.C_Prog x3 x4 x5 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))))))))))))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c__case_121(x4)(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.Prelude.List)(x8)(st))(x9)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c__case_120(x5)(Curry.Module.OraclePrelude.op_61_61(x5)(Curry.Module.Prelude.List)(x10)(st))(x11)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatListElems(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showFlatFunc))))(x6)(x12)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatList(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showFlatOp))))(x7)(x13)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))(x14)(st))(x15)(st))(x16)(st))(x17)(st))(x18)(st))(x19)(st))(x20)(st))(x21)(st))(x22)(st))(st)
c__case_122_case__2 x1 (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_122_case__2(x1)(x)(st))(i)(xs)(st)
c__case_122_case__2 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_122_case__2")(x)



c__case_119_case__3 x1 x2@Curry.Module.FlatCurry.C_Public st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))(st)
c__case_119_case__3 x1 x2@Curry.Module.FlatCurry.C_Private st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))(st)
c__case_119_case__3 x1 (Curry.Module.FlatCurry.C_VisibilityOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_119_case__3(x1)(x)(st))(i)(xs)(st)
c__case_119_case__3 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_119_case__3")(x)



c__case_118_case__4 x1 x2@Curry.Module.FlatCurry.C_InfixOp st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))(st)
c__case_118_case__4 x1 x2@Curry.Module.FlatCurry.C_InfixlOp st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))(st)
c__case_118_case__4 x1 x2@Curry.Module.FlatCurry.C_InfixrOp st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))(st)
c__case_118_case__4 x1 (Curry.Module.FlatCurry.C_FixityOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_118_case__4(x1)(x)(st))(i)(xs)(st)
c__case_118_case__4 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_118_case__4")(x)



c__case_117_case__5 x1 x2@(Curry.Module.FlatCurry.C_Op x3 x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatFixity(x4)(x6)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x5)(x7)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x8)(st))(x9)(st))(x10)(st))(x11)(st))(st)
c__case_117_case__5 x1 (Curry.Module.FlatCurry.C_OpDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_117_case__5(x1)(x)(st))(i)(xs)(st)
c__case_117_case__5 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_117_case__5")(x)



c__case_116_case__6 x1 x2@(Curry.Module.FlatCurry.C_Type x3 x4 x5 x6) st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List)))))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatVisibility(x4)(x11)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatList(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_show))))(x5)(x12)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatList(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showFlatCons))))(x6)(x13)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x14)(st))(x15)(st))(x16)(st))(x17)(st))(x18)(st))(st)
c__case_116_case__6 x1 x2@(Curry.Module.FlatCurry.C_TypeSyn x7 x8 x9 x10) st = let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List)))))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x7)(x1)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatVisibility(x8)(x19)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatList(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_show))))(x9)(x20)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatTypeExpr(x10)(x21)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x22)(st))(x23)(st))(x24)(st))(x25)(st))(x26)(st))(st)
c__case_116_case__6 x1 (Curry.Module.FlatCurry.C_TypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_116_case__6(x1)(x)(st))(i)(xs)(st)
c__case_116_case__6 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_116_case__6")(x)



c__case_115_case__7 x1 x2@(Curry.Module.FlatCurry.C_Cons x3 x4 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))))))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x4)(x7)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatVisibility(x5)(x8)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatList(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showFlatTypeExpr))))(x6)(x9)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x10)(st))(x11)(st))(x12)(st))(x13)(st))(x14)(st))(x15)(st))(st)
c__case_115_case__7 x1 (Curry.Module.FlatCurry.C_ConsDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_115_case__7(x1)(x)(st))(i)(xs)(st)
c__case_115_case__7 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_115_case__7")(x)



c__case_114_case__8 x1 x2@(Curry.Module.FlatCurry.C_Func x3 x4 x5 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List)))))))))))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x4)(x8)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatVisibility(x5)(x9)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatTypeExpr(x6)(x10)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatRule(x7)(x11)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x12)(st))(x13)(st))(x14)(st))(x15)(st))(x16)(st))(x17)(st))(x18)(st))(x19)(st))(x20)(st))(x21)(st))(st)
c__case_114_case__8 x1 (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_114_case__8(x1)(x)(st))(i)(xs)(st)
c__case_114_case__8 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_114_case__8")(x)



c__case_113_case__9 x1 x2@(Curry.Module.FlatCurry.C_Rule x3 x4) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatList(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_show))))(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatExpr(x4)(x6)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x7)(st))(x8)(st))(x9)(st))(st)
c__case_113_case__9 x1 x2@(Curry.Module.FlatCurry.C_External x5) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x5)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x10)(st))(x11)(st))(st)
c__case_113_case__9 x1 (Curry.Module.FlatCurry.C_RuleOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_113_case__9(x1)(x)(st))(i)(xs)(st)
c__case_113_case__9 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_113_case__9")(x)



c__case_112_case__10 x1 x2@(Curry.Module.FlatCurry.C_FuncType x3 x4) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatTypeExpr(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatTypeExpr(x4)(x8)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x9)(st))(x10)(st))(x11)(st))(x12)(st))(st)
c__case_112_case__10 x1 x2@(Curry.Module.FlatCurry.C_TCons x5 x6) st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x5)(x1)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatList(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showFlatTypeExpr))))(x6)(x13)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x14)(st))(x15)(st))(x16)(st))(st)
c__case_112_case__10 x1 x2@(Curry.Module.FlatCurry.C_TVar x7) st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('V'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x7)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x17)(st))(x18)(st))(st)
c__case_112_case__10 x1 (Curry.Module.FlatCurry.C_TypeExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_112_case__10(x1)(x)(st))(i)(xs)(st)
c__case_112_case__10 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_112_case__10")(x)



c__case_111_case__11 x1 x2@Curry.Module.FlatCurry.C_FuncCall st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))))))(st)
c__case_111_case__11 x1 x2@Curry.Module.FlatCurry.C_ConsCall st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))))))(st)
c__case_111_case__11 x1 x2@(Curry.Module.FlatCurry.C_FuncPartCall x3) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x3)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x5)(st))(x6)(st))(st)
c__case_111_case__11 x1 x2@(Curry.Module.FlatCurry.C_ConsPartCall x4) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x4)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x7)(st))(x8)(st))(st)
c__case_111_case__11 x1 (Curry.Module.FlatCurry.C_CombTypeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_111_case__11(x1)(x)(st))(i)(xs)(st)
c__case_111_case__11 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_111_case__11")(x)



c__case_109_case__12 x1 x15 x16 x14@Curry.Module.FlatCurry.C_Rigid st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatExpr(x15)(x1)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatList(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showFlatBranch))))(x16)(x17)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x18)(st))(x19)(st))(x20)(st))(st)
c__case_109_case__12 x1 x15 x16 x14@Curry.Module.FlatCurry.C_Flex st = let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatExpr(x15)(x1)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatList(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showFlatBranch))))(x16)(x21)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x22)(st))(x23)(st))(x24)(st))(st)
c__case_109_case__12 x1 x15 x16 (Curry.Module.FlatCurry.C_CaseTypeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_109_case__12(x1)(x15)(x16)(x)(st))(i)(xs)(st)
c__case_109_case__12 x1 x15 x16 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_109_case__12")(x)



c__case_110_case__13 x1 x2@(Curry.Module.FlatCurry.C_Var x3) st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('V'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x3)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x17)(st))(x18)(st))(st)
c__case_110_case__13 x1 x2@(Curry.Module.FlatCurry.C_Lit x4) st = let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatLit(x4)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x19)(st))(x20)(st))(st)
c__case_110_case__13 x1 x2@(Curry.Module.FlatCurry.C_Comb x5 x6 x7) st = let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatCombType(x5)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x6)(x21)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatList(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showFlatExpr))))(x7)(x22)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x23)(st))(x24)(st))(x25)(st))(x26)(st))(x27)(st))(st)
c__case_110_case__13 x1 x2@(Curry.Module.FlatCurry.C_Let x8 x9) st = let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)((Curry.Module.Prelude.:<)(x30)((Curry.Module.Prelude.:<)(x31)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatList(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showFlatExpr'46showFlatBinding'4649))))(x8)(x1)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatExpr(x9)(x28)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x29)(st))(x30)(st))(x31)(st))(st)
c__case_110_case__13 x1 x2@(Curry.Module.FlatCurry.C_Free x10 x11) st = let {x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)((Curry.Module.Prelude.:<)(x34)((Curry.Module.Prelude.:<)(x35)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatList(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_show))))(x10)(x1)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatExpr(x11)(x32)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x33)(st))(x34)(st))(x35)(st))(st)
c__case_110_case__13 x1 x2@(Curry.Module.FlatCurry.C_Or x12 x13) st = let {x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x39 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x40 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x36)((Curry.Module.Prelude.:<)(x37)((Curry.Module.Prelude.:<)(x38)((Curry.Module.Prelude.:<)(x39)((Curry.Module.Prelude.:<)(x40)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatExpr(x12)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatExpr(x13)(x36)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x37)(st))(x38)(st))(x39)(st))(x40)(st))(st)
c__case_110_case__13 x1 x2@(Curry.Module.FlatCurry.C_Case x14 x15 x16) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_109(x15)(x16)(x14)(x1)(st))(st)
c__case_110_case__13 x1 (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_110_case__13(x1)(x)(st))(i)(xs)(st)
c__case_110_case__13 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_110_case__13")(x)



c__case_108_case__14 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatExpr(x4)(x5)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x6)(st))(x7)(st))(x8)(st))(x9)(st))(st)
c__case_108_case__14 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_108_case__14(x1)(x)(st))(i)(xs)(st)
c__case_108_case__14 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_108_case__14")(x)



c__case_107_case__15 x1 x2@(Curry.Module.FlatCurry.C_Intc x3) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x3)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x6)(st))(x7)(st))(st)
c__case_107_case__15 x1 x2@(Curry.Module.FlatCurry.C_Floatc x4) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x4)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x8)(st))(x9)(st))(st)
c__case_107_case__15 x1 x2@(Curry.Module.FlatCurry.C_Charc x5) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x5)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x10)(st))(x11)(st))(st)
c__case_107_case__15 x1 (Curry.Module.FlatCurry.C_LiteralOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_107_case__15(x1)(x)(st))(i)(xs)(st)
c__case_107_case__15 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_107_case__15")(x)



c__case_106_case__16 x1 x2@(Curry.Module.FlatCurry.C_Branch x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('B'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatPattern(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatExpr(x4)(x5)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x6)(st))(x7)(st))(x8)(st))(st)
c__case_106_case__16 x1 (Curry.Module.FlatCurry.C_BranchExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_106_case__16(x1)(x)(st))(i)(xs)(st)
c__case_106_case__16 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_106_case__16")(x)



c__case_105_case__17 x1 x2@(Curry.Module.FlatCurry.C_Pattern x3 x4) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatList(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_show))))(x4)(x6)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x7)(st))(x8)(st))(x9)(st))(st)
c__case_105_case__17 x1 x2@(Curry.Module.FlatCurry.C_LPattern x5) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showFlatLit(x5)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x10)(st))(x11)(st))(st)
c__case_105_case__17 x1 (Curry.Module.FlatCurry.C_PatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_105_case__17(x1)(x)(st))(i)(xs)(st)
c__case_105_case__17 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_105_case__17")(x)



c__case_103_case__18 x1 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.c_chr(Curry.Module.OraclePrelude.op_43(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))))(x5)(x1)(st))(x7)(st))(Curry.Module.Prelude.List))(st)
c__case_103_case__18 x1 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.OraclePrelude.c_show(x5)(x1)(st)))(st)
c__case_103_case__18 x1 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_103_case__18(x1)(x5)(x)(st))(i)(xs)(st)
c__case_103_case__18 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_103_case__18")(x)



c__case_98_case__19 x1 x2 x3 x8 x9 x10@Curry.Module.Prelude.C_True st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))))(Curry.Module.OracleFlatCurryShow.c_showBracketsIf(x3)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.Oracle.c_apply(x2)(x8)(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showCurryType'46_'35lambda2(x2)))))(x11)(st))(x9)(x12)(st))(x13)(st))(x14)(st))(st)
c__case_98_case__19 x1 x2 x3 x8 x9 x10@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_98_case__19 x1 x2 x3 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_98_case__19(x1)(x2)(x3)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_98_case__19 x1 x2 x3 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_98_case__19")(x)



c__case_99_case__20 x1 x2 x3 x8 x9 x10@Curry.Module.Prelude.C_True st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_concat(Curry.Module.OracleList.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showCurryType(x2)(Curry.Module.Prelude.C_False)))))(x9)(x1)(st))(x11)(st))(x12)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x13)(st))(x14)(st))(st)
c__case_99_case__20 x1 x2 x3 x8 x9 x10@Curry.Module.Prelude.C_False st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c__case_98(x2)(x3)(x8)(x9)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x15)(st))(st)
c__case_99_case__20 x1 x2 x3 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_99_case__20(x1)(x2)(x3)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_99_case__20 x1 x2 x3 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_99_case__20")(x)



c__case_100_case__21 x1 x2 x3 x8 x9 x10@Curry.Module.Prelude.C_True st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryType(x2)(Curry.Module.Prelude.C_False)(Curry.Module.OraclePrelude.c_head(x9)(x1)(st))(x11)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))(x12)(st))(x13)(st))(st)
c__case_100_case__21 x1 x2 x3 x8 x9 x10@Curry.Module.Prelude.C_False st = let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))))(Curry.Module.OracleFlatCurryShow.c__case_99(x2)(x3)(x8)(x9)(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_take(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(Curry.Module.OraclePrelude.c_snd(x8)(x1)(st))(x14)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List)))(x15)(st))(x16)(st))(st)
c__case_100_case__21 x1 x2 x3 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_100_case__21(x1)(x2)(x3)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_100_case__21 x1 x2 x3 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_100_case__21")(x)



c__case_101_case__22 x1 x2 x3 x8 x9 x10@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))(Curry.Module.Prelude.List)))))))(st)
c__case_101_case__22 x1 x2 x3 x8 x9 x10@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c__case_100(x2)(x3)(x8)(x9)(Curry.Module.OraclePrelude.op_61_61(x8)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))))(x1)(st))(x11)(st))(st)
c__case_101_case__22 x1 x2 x3 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_101_case__22(x1)(x2)(x3)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_101_case__22 x1 x2 x3 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_101_case__22")(x)



c__case_102_case__23 x1 x2 x3 x8 x9 x10@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.c_apply(x2)(x8)(x1)(st))(st)
c__case_102_case__23 x1 x2 x3 x8 x9 x10@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))))(Curry.Module.OracleFlatCurryShow.c__case_101(x2)(x3)(x8)(x9)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x8)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_head(x9)(x11)(st))(Curry.Module.FlatCurry.C_TCons(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.List))(x12)(st))(x13)(st))(x14)(st))(st)
c__case_102_case__23 x1 x2 x3 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_102_case__23(x1)(x2)(x3)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_102_case__23 x1 x2 x3 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_102_case__23")(x)



c__case_104_case__24 x1 x2 x3 x4@(Curry.Module.FlatCurry.C_TVar x5) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c__case_103(x5)(Curry.Module.OraclePrelude.op_60(x5)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))(x1)(st))(x10)(st))(st)
c__case_104_case__24 x1 x2 x3 x4@(Curry.Module.FlatCurry.C_FuncType x6 x7) st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))))))(Curry.Module.OracleFlatCurryShow.c_showBracketsIf(x3)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryType(x2)(Curry.Module.OracleFlatCurryShow.c_isFuncType(x6)(x1)(st))(x6)(x11)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.OracleFlatCurryShow.c_showCurryType(x2)(Curry.Module.Prelude.C_False)(x7)(x12)(st))(x13)(st))(x14)(st))(x15)(st))(st)
c__case_104_case__24 x1 x2 x3 x4@(Curry.Module.FlatCurry.C_TCons x8 x9) st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c__case_102(x2)(x3)(x8)(x9)(Curry.Module.OraclePrelude.op_61_61(x9)(Curry.Module.Prelude.List)(x1)(st))(x16)(st))(st)
c__case_104_case__24 x1 x2 x3 (Curry.Module.FlatCurry.C_TypeExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_104_case__24(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_104_case__24 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_104_case__24")(x)



c__case_97_case__25 x1 x2@(Curry.Module.FlatCurry.C_TVar x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_97_case__25 x1 x2@(Curry.Module.FlatCurry.C_FuncType x4 x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_97_case__25 x1 x2@(Curry.Module.FlatCurry.C_TCons x6 x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_97_case__25 x1 (Curry.Module.FlatCurry.C_TypeExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_97_case__25(x1)(x)(st))(i)(xs)(st)
c__case_97_case__25 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_97_case__25")(x)



c__case_89_case__26 x1 x2 x4 x8 x9 x11 x13 x14@Curry.Module.Prelude.C_True st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryStringConstant(Curry.Module.FlatCurry.C_Comb(x8)(x9)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List))(x15)(st))(x16)(st))(st)
c__case_89_case__26 x1 x2 x4 x8 x9 x11 x13 x14@Curry.Module.Prelude.C_False st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_concat(Curry.Module.OracleList.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c_showCurryFiniteList(x2)(x4)(Curry.Module.FlatCurry.C_Comb(x8)(x9)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))))(x1)(st))(x17)(st))(x18)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))(x19)(st))(x20)(st))(st)
c__case_89_case__26 x1 x2 x4 x8 x9 x11 x13 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_89_case__26(x1)(x2)(x4)(x8)(x9)(x11)(x13)(x)(st))(i)(xs)(st)
c__case_89_case__26 x1 x2 x4 x8 x9 x11 x13 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_89_case__26")(x)



c__case_87_case__27 x1 x2 x3 x4 x9 x11 x13 x14@Curry.Module.Prelude.C_True st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))))))))(Curry.Module.OracleFlatCurryShow.c_showBracketsIf(x3)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_True)(x4)(x11)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.Oracle.c_apply(x2)(x9)(x15)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_True)(x4)(x13)(x16)(st))(x17)(st))(x18)(st))(x19)(st))(x20)(st))(x21)(st))(st)
c__case_87_case__27 x1 x2 x3 x4 x9 x11 x13 x14@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_87_case__27 x1 x2 x3 x4 x9 x11 x13 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_87_case__27(x1)(x2)(x3)(x4)(x9)(x11)(x13)(x)(st))(i)(xs)(st)
c__case_87_case__27 x1 x2 x3 x4 x9 x11 x13 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_87_case__27")(x)



c__case_88_case__28 x1 x2 x3 x4 x9 x11 x13 x14@Curry.Module.Prelude.C_True st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_False)(x4)(x11)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_False)(x4)(x13)(x15)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x16)(st))(x17)(st))(x18)(st))(x19)(st))(st)
c__case_88_case__28 x1 x2 x3 x4 x9 x11 x13 x14@Curry.Module.Prelude.C_False st = let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c__case_87(x2)(x3)(x4)(x9)(x11)(x13)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x20)(st))(st)
c__case_88_case__28 x1 x2 x3 x4 x9 x11 x13 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_88_case__28(x1)(x2)(x3)(x4)(x9)(x11)(x13)(x)(st))(i)(xs)(st)
c__case_88_case__28 x1 x2 x3 x4 x9 x11 x13 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_88_case__28")(x)



c__case_90_case__29 x1 x2 x3 x4 x8 x9 x11 x13 x14@Curry.Module.Prelude.C_True st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c__case_89(x2)(x4)(x8)(x9)(x11)(x13)(Curry.Module.OracleFlatCurryShow.c_isStringConstant(Curry.Module.FlatCurry.C_Comb(x8)(x9)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))))(x1)(st))(x15)(st))(st)
c__case_90_case__29 x1 x2 x3 x4 x8 x9 x11 x13 x14@Curry.Module.Prelude.C_False st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List)))(Curry.Module.OracleFlatCurryShow.c__case_88(x2)(x3)(x4)(x9)(x11)(x13)(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_snd(x9)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))))(x16)(st))(x17)(st))(st)
c__case_90_case__29 x1 x2 x3 x4 x8 x9 x11 x13 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_90_case__29(x1)(x2)(x3)(x4)(x8)(x9)(x11)(x13)(x)(st))(i)(xs)(st)
c__case_90_case__29 x1 x2 x3 x4 x8 x9 x11 x13 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_90_case__29")(x)



c__case_91_case__30 x1 x2 x3 x4 x8 x9 x11 x13 x14@Curry.Module.Prelude.C_True st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List)))))(Curry.Module.OracleFlatCurryShow.c_showBracketsIf(x3)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.Oracle.c_apply(x2)(x9)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c_showCurryElems(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_True)(x4)))))((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List)))(x15)(st))(x16)(st))(x17)(st))(x18)(st))(st)
c__case_91_case__30 x1 x2 x3 x4 x8 x9 x11 x13 x14@Curry.Module.Prelude.C_False st = let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c__case_90(x2)(x3)(x4)(x8)(x9)(x11)(x13)(Curry.Module.OracleFlatCurryShow.c_isFiniteList(Curry.Module.FlatCurry.C_Comb(x8)(x9)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))))(x1)(st))(x19)(st))(st)
c__case_91_case__30 x1 x2 x3 x4 x8 x9 x11 x13 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_91_case__30(x1)(x2)(x3)(x4)(x8)(x9)(x11)(x13)(x)(st))(i)(xs)(st)
c__case_91_case__30 x1 x2 x3 x4 x8 x9 x11 x13 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_91_case__30")(x)



c__case_92_case__31 x1 x2 x3 x4 x8 x9 x11 x13 x14@Curry.Module.Prelude.C_True st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List)))))(Curry.Module.OracleFlatCurryShow.c_showBracketsIf(x3)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_True)(x4)(x11)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_True)(x4)(x13)(x15)(st))(x16)(st))(x17)(st))(x18)(st))(st)
c__case_92_case__31 x1 x2 x3 x4 x8 x9 x11 x13 x14@Curry.Module.Prelude.C_False st = let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))))(Curry.Module.OracleFlatCurryShow.c__case_91(x2)(x3)(x4)(x8)(x9)(x11)(x13)(Curry.Module.OracleChar.c_isAlpha(Curry.Module.OraclePrelude.c_head(Curry.Module.OraclePrelude.c_snd(x9)(x1)(st))(x19)(st))(x20)(st))(x21)(st))(st)
c__case_92_case__31 x1 x2 x3 x4 x8 x9 x11 x13 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_92_case__31(x1)(x2)(x3)(x4)(x8)(x9)(x11)(x13)(x)(st))(i)(xs)(st)
c__case_92_case__31 x1 x2 x3 x4 x8 x9 x11 x13 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_92_case__31")(x)



c__case_84_case__32 x1 x2 x3 x4 x9 x11 x13 x15 x16 x17@Curry.Module.Prelude.C_True st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))))))(Curry.Module.OracleFlatCurryShow.c_showBracketsIf(x3)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryId(Curry.Module.Oracle.c_apply(x2)(x9)(x1)(st))(x18)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c_showCurryElems(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_True)(x4)))))((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x15)(x16))))(x19)(st))(x20)(st))(x21)(st))(x22)(st))(st)
c__case_84_case__32 x1 x2 x3 x4 x9 x11 x13 x15 x16 x17@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_84_case__32 x1 x2 x3 x4 x9 x11 x13 x15 x16 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_84_case__32(x1)(x2)(x3)(x4)(x9)(x11)(x13)(x15)(x16)(x)(st))(i)(xs)(st)
c__case_84_case__32 x1 x2 x3 x4 x9 x11 x13 x15 x16 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_84_case__32")(x)



c__case_85_case__33 x1 x2 x3 x4 x9 x11 x13 x15 x16 x17@Curry.Module.Prelude.C_True st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_concat(Curry.Module.OracleList.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_False)(x4)))))((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x15)(x16))))(x1)(st))(x18)(st))(x19)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x20)(st))(x21)(st))(st)
c__case_85_case__33 x1 x2 x3 x4 x9 x11 x13 x15 x16 x17@Curry.Module.Prelude.C_False st = let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c__case_84(x2)(x3)(x4)(x9)(x11)(x13)(x15)(x16)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x22)(st))(st)
c__case_85_case__33 x1 x2 x3 x4 x9 x11 x13 x15 x16 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_85_case__33(x1)(x2)(x3)(x4)(x9)(x11)(x13)(x15)(x16)(x)(st))(i)(xs)(st)
c__case_85_case__33 x1 x2 x3 x4 x9 x11 x13 x15 x16 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_85_case__33")(x)



c__case_86_case__34 x1 x2 x3 x4 x9 x11 x13 x15 x16 x17@Curry.Module.Prelude.C_True st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)((Curry.Module.Prelude.:<)(x30)((Curry.Module.Prelude.:<)(x31)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)((Curry.Module.Prelude.:<)(x34)((Curry.Module.Prelude.:<)(x35)((Curry.Module.Prelude.:<)(x36)((Curry.Module.Prelude.:<)(x37)(Curry.Module.Prelude.List)))))))))))))))))))))(Curry.Module.OracleFlatCurryShow.c_showBracketsIf(x3)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_sceBlanks(x4)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_False)(Curry.Module.OraclePrelude.op_43(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x18)(st))(x11)(x19)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_sceBlanks(x4)(x20)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_False)(Curry.Module.OraclePrelude.op_43(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x21)(st))(x13)(x22)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_sceBlanks(x4)(x23)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_False)(Curry.Module.OraclePrelude.op_43(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x24)(st))(x15)(x25)(st))(x26)(st))(x27)(st))(x28)(st))(x29)(st))(x30)(st))(x31)(st))(x32)(st))(x33)(st))(x34)(st))(x35)(st))(x36)(st))(x37)(st))(st)
c__case_86_case__34 x1 x2 x3 x4 x9 x11 x13 x15 x16 x17@Curry.Module.Prelude.C_False st = let {x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x39 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x40 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x38)((Curry.Module.Prelude.:<)(x39)((Curry.Module.Prelude.:<)(x40)(Curry.Module.Prelude.List))))(Curry.Module.OracleFlatCurryShow.c__case_85(x2)(x3)(x4)(x9)(x11)(x13)(x15)(x16)(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_take(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(Curry.Module.OraclePrelude.c_snd(x9)(x1)(st))(x38)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List)))(x39)(st))(x40)(st))(st)
c__case_86_case__34 x1 x2 x3 x4 x9 x11 x13 x15 x16 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_86_case__34(x1)(x2)(x3)(x4)(x9)(x11)(x13)(x15)(x16)(x)(st))(i)(xs)(st)
c__case_86_case__34 x1 x2 x3 x4 x9 x11 x13 x15 x16 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_86_case__34")(x)



c__case_93_case__35 x1 x2 x3 x4 x8 x9 x11 x13 x14@Curry.Module.Prelude.List st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c__case_92(x2)(x3)(x4)(x8)(x9)(x11)(x13)(Curry.Module.OraclePrelude.op_61_61(x9)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(x1)(st))(x17)(st))(st)
c__case_93_case__35 x1 x2 x3 x4 x8 x9 x11 x13 x14@((Curry.Module.Prelude.:<) x15 x16) st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))))(Curry.Module.OracleFlatCurryShow.c__case_86(x2)(x3)(x4)(x9)(x11)(x13)(x15)(x16)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x9)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))))))))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x16)(Curry.Module.Prelude.List)(x18)(st))(x19)(st))(x20)(st))(st)
c__case_93_case__35 x1 x2 x3 x4 x8 x9 x11 x13 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_93_case__35(x1)(x2)(x3)(x4)(x8)(x9)(x11)(x13)(x)(st))(i)(xs)(st)
c__case_93_case__35 x1 x2 x3 x4 x8 x9 x11 x13 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_93_case__35")(x)



c__case_94_case__36 x1 x2 x3 x4 x8 x9 x11 x12@Curry.Module.Prelude.List st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))))))(Curry.Module.OracleFlatCurryShow.c_showBracketsIf(x3)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryId(Curry.Module.Oracle.c_apply(x2)(x9)(x1)(st))(x15)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_True)(x4)(x11)(x16)(st))(x17)(st))(x18)(st))(x19)(st))(st)
c__case_94_case__36 x1 x2 x3 x4 x8 x9 x11 x12@((Curry.Module.Prelude.:<) x13 x14) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_93(x2)(x3)(x4)(x8)(x9)(x11)(x13)(x14)(x1)(st))(st)
c__case_94_case__36 x1 x2 x3 x4 x8 x9 x11 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_94_case__36(x1)(x2)(x3)(x4)(x8)(x9)(x11)(x)(st))(i)(xs)(st)
c__case_94_case__36 x1 x2 x3 x4 x8 x9 x11 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_94_case__36")(x)



c__case_95_case__37 x1 x2 x3 x4 x8 x9 x10@Curry.Module.Prelude.List st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c_showCurryId(Curry.Module.Oracle.c_apply(x2)(x9)(x1)(st))(x13)(st))(st)
c__case_95_case__37 x1 x2 x3 x4 x8 x9 x10@((Curry.Module.Prelude.:<) x11 x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_94(x2)(x3)(x4)(x8)(x9)(x11)(x12)(x1)(st))(st)
c__case_95_case__37 x1 x2 x3 x4 x8 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_95_case__37(x1)(x2)(x3)(x4)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_95_case__37 x1 x2 x3 x4 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_95_case__37")(x)



c__case_83_case__38 x1 x2 x3 x4 x20 x19@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(x3)(x4)(x20)(x1)(st))(st)
c__case_83_case__38 x1 x2 x3 x4 x20 x19@((Curry.Module.Prelude.:<) x21 x22) st = let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)(Curry.Module.Prelude.List))))))))(Curry.Module.OracleFlatCurryShow.c_showBracketsIf(x3)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_concat(Curry.Module.OracleList.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showCurryVar))))((Curry.Module.Prelude.:<)(x21)(x22))(x1)(st))(x23)(st))(x24)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_False)(x4)(x20)(x25)(st))(x26)(st))(x27)(st))(x28)(st))(x29)(st))(st)
c__case_83_case__38 x1 x2 x3 x4 x20 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_83_case__38(x1)(x2)(x3)(x4)(x20)(x)(st))(i)(xs)(st)
c__case_83_case__38 x1 x2 x3 x4 x20 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_83_case__38")(x)



c__case_82_case__39 x1 x26@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(st)
c__case_82_case__39 x1 x26@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))(st)
c__case_82_case__39 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_82_case__39(x1)(x)(st))(i)(xs)(st)
c__case_82_case__39 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_82_case__39")(x)



c__case_96_case__40 x1 x2 x3 x4 x5@(Curry.Module.FlatCurry.C_Var x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c_showCurryVar(x6)(x1)(st))(st)
c__case_96_case__40 x1 x2 x3 x4 x5@(Curry.Module.FlatCurry.C_Lit x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c_showCurryLit(x7)(x1)(st))(st)
c__case_96_case__40 x1 x2 x3 x4 x5@(Curry.Module.FlatCurry.C_Comb x8 x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_95(x2)(x3)(x4)(x8)(x9)(x10)(x1)(st))(st)
c__case_96_case__40 x1 x2 x3 x4 x5@(Curry.Module.FlatCurry.C_Let x17 x18) st = let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x39 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x40 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x41 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x42 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x43 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)((Curry.Module.Prelude.:<)(x30)((Curry.Module.Prelude.:<)(x31)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)((Curry.Module.Prelude.:<)(x34)((Curry.Module.Prelude.:<)(x35)((Curry.Module.Prelude.:<)(x36)((Curry.Module.Prelude.:<)(x37)((Curry.Module.Prelude.:<)(x38)((Curry.Module.Prelude.:<)(x39)((Curry.Module.Prelude.:<)(x40)((Curry.Module.Prelude.:<)(x41)((Curry.Module.Prelude.:<)(x42)((Curry.Module.Prelude.:<)(x43)(Curry.Module.Prelude.List)))))))))))))))))(Curry.Module.OracleFlatCurryShow.c_showBracketsIf(x3)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_sceBlanks(x4)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_concat(Curry.Module.OracleList.c_intersperse(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.OracleFlatCurryShow.c_sceBlanks(x4)(x28)(st))(x29)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showCurryExpr'46_'35lambda3(x4)(x2)))))(x17)(x30)(st))(x31)(st))(x32)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_sceBlanks(x4)(x33)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(x34)(st))(x35)(st))(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_False)(Curry.Module.OraclePrelude.op_43(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))(x36)(st))(x18)(x37)(st))(x38)(st))(x39)(st))(x40)(st))(x41)(st))(x42)(st))(x43)(st))(st)
c__case_96_case__40 x1 x2 x3 x4 x5@(Curry.Module.FlatCurry.C_Free x19 x20) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_83(x2)(x3)(x4)(x20)(x19)(x1)(st))(st)
c__case_96_case__40 x1 x2 x3 x4 x5@(Curry.Module.FlatCurry.C_Or x23 x24) st = let {x44 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x45 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x46 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x47 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x44)((Curry.Module.Prelude.:<)(x45)((Curry.Module.Prelude.:<)(x46)((Curry.Module.Prelude.:<)(x47)(Curry.Module.Prelude.List)))))(Curry.Module.OracleFlatCurryShow.c_showBracketsIf(x3)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_True)(x4)(x23)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('?'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_True)(x4)(x24)(x44)(st))(x45)(st))(x46)(st))(x47)(st))(st)
c__case_96_case__40 x1 x2 x3 x4 x5@(Curry.Module.FlatCurry.C_Case x25 x26 x27) st = let {x48 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x49 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x50 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x51 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x52 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x53 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x54 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x55 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x56 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x57 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x48)((Curry.Module.Prelude.:<)(x49)((Curry.Module.Prelude.:<)(x50)((Curry.Module.Prelude.:<)(x51)((Curry.Module.Prelude.:<)(x52)((Curry.Module.Prelude.:<)(x53)((Curry.Module.Prelude.:<)(x54)((Curry.Module.Prelude.:<)(x55)((Curry.Module.Prelude.:<)(x56)((Curry.Module.Prelude.:<)(x57)(Curry.Module.Prelude.List)))))))))))(Curry.Module.OracleFlatCurryShow.c_showBracketsIf(x3)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c__case_82(x25)(Curry.Module.OraclePrelude.op_61_61(x25)(Curry.Module.FlatCurry.C_Rigid)(x1)(st))(x48)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_True)(x4)(x26)(x49)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryElems(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showCurryCase(x2)(Curry.Module.OraclePrelude.op_43(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x50)(st))))))(x27)(x51)(st))(Curry.Module.OracleFlatCurryShow.c_sceBlanks(x4)(x52)(st))(x53)(st))(x54)(st))(x55)(st))(x56)(st))(x57)(st))(st)
c__case_96_case__40 x1 x2 x3 x4 (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_96_case__40(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_96_case__40 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_96_case__40")(x)



c__case_81_case__41 x1 x2 x3 x4@(Curry.Module.Prelude.T2 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryVar(x5)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x3)(Curry.Module.Prelude.C_False)(Curry.Module.OraclePrelude.op_43(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))(x7)(st))(x6)(x8)(st))(x9)(st))(x10)(st))(st)
c__case_81_case__41 x1 x2 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_81_case__41(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_81_case__41 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_81_case__41")(x)



c__case_78_case__42 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(x2))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x1)(st))(st)
c__case_78_case__42 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_78_case__42 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_78_case__42(x1)(x2)(x)(st))(i)(xs)(st)
c__case_78_case__42 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_78_case__42")(x)



c__case_79_case__43 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_79_case__43 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c__case_78(x2)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x4)(st))(st)
c__case_79_case__43 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_79_case__43(x1)(x2)(x)(st))(i)(xs)(st)
c__case_79_case__43 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_79_case__43")(x)



c__case_80_case__44 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_80_case__44 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c__case_79(x2)(Curry.Module.OraclePrelude.op_61_61(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List)))(x1)(st))(x4)(st))(st)
c__case_80_case__44 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_80_case__44(x1)(x2)(x)(st))(i)(xs)(st)
c__case_80_case__44 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_80_case__44")(x)



c__case_77_case__45 x1 x2@(Curry.Module.FlatCurry.C_Intc x3) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_show(x3)(x1)(st))(st)
c__case_77_case__45 x1 x2@(Curry.Module.FlatCurry.C_Floatc x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_show(x4)(x1)(st))(st)
c__case_77_case__45 x1 x2@(Curry.Module.FlatCurry.C_Charc x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_show(x5)(x1)(st))(st)
c__case_77_case__45 x1 (Curry.Module.FlatCurry.C_LiteralOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_77_case__45(x1)(x)(st))(i)(xs)(st)
c__case_77_case__45 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_77_case__45")(x)



c__case_75_case__46 x1 x2 x3 x6 x5@(Curry.Module.FlatCurry.C_Pattern x7 x8) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_sceBlanks(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryCase'46showPattern'46151(Curry.Module.Oracle.c_apply(x2)(x7)(x10)(st))(x8)(x11)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_False)(x3)(x6)(x12)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x13)(st))(x14)(st))(x15)(st))(x16)(st))(st)
c__case_75_case__46 x1 x2 x3 x6 x5@(Curry.Module.FlatCurry.C_LPattern x9) st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_sceBlanks(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryLit(x9)(x17)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_False)(x3)(x6)(x18)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x19)(st))(x20)(st))(x21)(st))(x22)(st))(x23)(st))(st)
c__case_75_case__46 x1 x2 x3 x6 (Curry.Module.FlatCurry.C_PatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_75_case__46(x1)(x2)(x3)(x6)(x)(st))(i)(xs)(st)
c__case_75_case__46 x1 x2 x3 x6 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_75_case__46")(x)



c__case_76_case__47 x1 x2 x3 x4@(Curry.Module.FlatCurry.C_Branch x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_75(x2)(x3)(x6)(x5)(x1)(st))(st)
c__case_76_case__47 x1 x2 x3 (Curry.Module.FlatCurry.C_BranchExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_76_case__47(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_76_case__47 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_76_case__47")(x)



c__case_70_case__48 x1 x2 x4 x6 x7@Curry.Module.Prelude.C_True st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryVar(x4)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryVar(x6)(x8)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x9)(st))(x10)(st))(x11)(st))(x12)(st))(st)
c__case_70_case__48 x1 x2 x4 x6 x7@Curry.Module.Prelude.C_False st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryVar(x4)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x2)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c_showCurryVar(x6)(x13)(st))(x14)(st))(x15)(st))(x16)(st))(x17)(st))(st)
c__case_70_case__48 x1 x2 x4 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_70_case__48(x1)(x2)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_70_case__48 x1 x2 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_70_case__48")(x)



c__case_71_case__49 x1 x2 x4 x6 x7@Curry.Module.Prelude.C_True st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_43_43(x2)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCurryVar(x4)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c_showCurryVar(x6)(x8)(st))(x9)(st))(x10)(st))(x11)(st))(x12)(st))(st)
c__case_71_case__49 x1 x2 x4 x6 x7@Curry.Module.Prelude.C_False st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c__case_70(x2)(x4)(x6)(Curry.Module.OraclePrelude.op_61_61(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))))(x1)(st))(x13)(st))(st)
c__case_71_case__49 x1 x2 x4 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_71_case__49(x1)(x2)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_71_case__49 x1 x2 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_71_case__49")(x)



c__case_69_case__50 x1 x2 x4 x6 x8 x9 x10@Curry.Module.Prelude.C_True st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_concat(Curry.Module.OracleList.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showCurryVar))))((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x8)(x9))))(x1)(st))(x11)(st))(x12)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x13)(st))(x14)(st))(st)
c__case_69_case__50 x1 x2 x4 x6 x8 x9 x10@Curry.Module.Prelude.C_False st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(x2)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c_showCurryElems(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryShow.c_showCurryVar))))((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x8)(x9))))(x1)(st))(x15)(st))(x16)(st))(st)
c__case_69_case__50 x1 x2 x4 x6 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_69_case__50(x1)(x2)(x4)(x6)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_69_case__50 x1 x2 x4 x6 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_69_case__50")(x)



c__case_72_case__51 x1 x2 x4 x6 x7@Curry.Module.Prelude.List st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))(Curry.Module.OracleFlatCurryShow.c__case_71(x2)(x4)(x6)(Curry.Module.OracleChar.c_isAlpha(Curry.Module.OraclePrelude.c_head(x2)(x1)(st))(x10)(st))(x11)(st))(st)
c__case_72_case__51 x1 x2 x4 x6 x7@((Curry.Module.Prelude.:<) x8 x9) st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List)))(Curry.Module.OracleFlatCurryShow.c__case_69(x2)(x4)(x6)(x8)(x9)(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_take(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x2)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List)))(x12)(st))(x13)(st))(st)
c__case_72_case__51 x1 x2 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_72_case__51(x1)(x2)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_72_case__51 x1 x2 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_72_case__51")(x)



c__case_73_case__52 x1 x2 x4 x5@Curry.Module.Prelude.List st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(x2)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c_showCurryVar(x4)(x1)(st))(x8)(st))(x9)(st))(st)
c__case_73_case__52 x1 x2 x4 x5@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_72(x2)(x4)(x6)(x7)(x1)(st))(st)
c__case_73_case__52 x1 x2 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_73_case__52(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_73_case__52 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_73_case__52")(x)



c__case_74_case__53 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_74_case__53 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_73(x2)(x4)(x5)(x1)(st))(st)
c__case_74_case__53 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_74_case__53(x1)(x2)(x)(st))(i)(xs)(st)
c__case_74_case__53 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_74_case__53")(x)



c__case_46_case__54 x1 x7@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_46_case__54 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_46_case__54(x1)(x)(st))(i)(xs)(st)
c__case_46_case__54 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_46_case__54")(x)



c__case_47_case__55 x1 x7 x27@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_46(x7)(x1)(st))(st)
c__case_47_case__55 x1 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_47_case__55(x1)(x7)(x)(st))(i)(xs)(st)
c__case_47_case__55 x1 x7 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_47_case__55")(x)



c__case_48_case__56 x1 x7 x27 x26 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x26)(Curry.Module.Prelude.C_Char(']'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_47(x7)(x27)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_49_case__57 x1 x7 x25@((Curry.Module.Prelude.:<) x26 x27) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_48(x7)(x27)(x26)(x1)(st))(st)
c__case_49_case__57 x1 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_49_case__57(x1)(x7)(x)(st))(i)(xs)(st)
c__case_49_case__57 x1 x7 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_49_case__57")(x)



c__case_42_case__58 x1 x2 x3 x28 x30 x31@Curry.Module.Prelude.List st = let {x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x32)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.OracleFlatCurryShow.c_showCurryExpr(x2)(Curry.Module.Prelude.C_False)(x3)(x28)(x1)(st))(Curry.Module.OracleFlatCurryShow.c_showCurryFiniteList(x2)(x3)(x30)(x32)(st)))(st)
c__case_42_case__58 x1 x2 x3 x28 x30 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_42_case__58(x1)(x2)(x3)(x28)(x30)(x)(st))(i)(xs)(st)
c__case_42_case__58 x1 x2 x3 x28 x30 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_42_case__58")(x)



c__case_43_case__59 x1 x2 x3 x28 x29@((Curry.Module.Prelude.:<) x30 x31) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_42(x2)(x3)(x28)(x30)(x31)(x1)(st))(st)
c__case_43_case__59 x1 x2 x3 x28 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_43_case__59(x1)(x2)(x3)(x28)(x)(st))(i)(xs)(st)
c__case_43_case__59 x1 x2 x3 x28 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_43_case__59")(x)



c__case_44_case__60 x1 x2 x3 x7@((Curry.Module.Prelude.:<) x28 x29) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_43(x2)(x3)(x28)(x29)(x1)(st))(st)
c__case_44_case__60 x1 x2 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_44_case__60(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_44_case__60 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_44_case__60")(x)



c__case_45_case__61 x1 x2 x3 x7 x25@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_44(x2)(x3)(x7)(x1)(st))(st)
c__case_45_case__61 x1 x2 x3 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_45_case__61(x1)(x2)(x3)(x7)(x)(st))(i)(xs)(st)
c__case_45_case__61 x1 x2 x3 x7 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_45_case__61")(x)



c__case_50_case__62 x1 x2 x3 x7 x25 x24 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x24)(Curry.Module.Prelude.C_Char('['))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_49(x7)(x25)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x24)(Curry.Module.Prelude.C_Char(':'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_45(x2)(x3)(x7)(x25)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st)



c__case_51_case__63 x1 x2 x3 x7 x9@((Curry.Module.Prelude.:<) x24 x25) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_50(x2)(x3)(x7)(x25)(x24)(x1)(st))(st)
c__case_51_case__63 x1 x2 x3 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_51_case__63(x1)(x2)(x3)(x7)(x)(st))(i)(xs)(st)
c__case_51_case__63 x1 x2 x3 x7 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_51_case__63")(x)



c__case_52_case__64 x1 x2 x3 x7 x9 x23@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_51(x2)(x3)(x7)(x9)(x1)(st))(st)
c__case_52_case__64 x1 x2 x3 x7 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_52_case__64(x1)(x2)(x3)(x7)(x9)(x)(st))(i)(xs)(st)
c__case_52_case__64 x1 x2 x3 x7 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_52_case__64")(x)



c__case_53_case__65 x1 x2 x3 x7 x9 x23 x22 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x22)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_52(x2)(x3)(x7)(x9)(x23)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_54_case__66 x1 x2 x3 x7 x9 x21@((Curry.Module.Prelude.:<) x22 x23) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_53(x2)(x3)(x7)(x9)(x23)(x22)(x1)(st))(st)
c__case_54_case__66 x1 x2 x3 x7 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_54_case__66(x1)(x2)(x3)(x7)(x9)(x)(st))(i)(xs)(st)
c__case_54_case__66 x1 x2 x3 x7 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_54_case__66")(x)



c__case_55_case__67 x1 x2 x3 x7 x9 x21 x20 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x20)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_54(x2)(x3)(x7)(x9)(x21)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_56_case__68 x1 x2 x3 x7 x9 x19@((Curry.Module.Prelude.:<) x20 x21) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_55(x2)(x3)(x7)(x9)(x21)(x20)(x1)(st))(st)
c__case_56_case__68 x1 x2 x3 x7 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_56_case__68(x1)(x2)(x3)(x7)(x9)(x)(st))(i)(xs)(st)
c__case_56_case__68 x1 x2 x3 x7 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_56_case__68")(x)



c__case_57_case__69 x1 x2 x3 x7 x9 x19 x18 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x18)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_56(x2)(x3)(x7)(x9)(x19)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_58_case__70 x1 x2 x3 x7 x9 x17@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_57(x2)(x3)(x7)(x9)(x19)(x18)(x1)(st))(st)
c__case_58_case__70 x1 x2 x3 x7 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_58_case__70(x1)(x2)(x3)(x7)(x9)(x)(st))(i)(xs)(st)
c__case_58_case__70 x1 x2 x3 x7 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_58_case__70")(x)



c__case_59_case__71 x1 x2 x3 x7 x9 x17 x16 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x16)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_58(x2)(x3)(x7)(x9)(x17)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_60_case__72 x1 x2 x3 x7 x9 x15@((Curry.Module.Prelude.:<) x16 x17) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_59(x2)(x3)(x7)(x9)(x17)(x16)(x1)(st))(st)
c__case_60_case__72 x1 x2 x3 x7 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_60_case__72(x1)(x2)(x3)(x7)(x9)(x)(st))(i)(xs)(st)
c__case_60_case__72 x1 x2 x3 x7 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_60_case__72")(x)



c__case_61_case__73 x1 x2 x3 x7 x9 x15 x14 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x14)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_60(x2)(x3)(x7)(x9)(x15)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_62_case__74 x1 x2 x3 x7 x9 x13@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_61(x2)(x3)(x7)(x9)(x15)(x14)(x1)(st))(st)
c__case_62_case__74 x1 x2 x3 x7 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_62_case__74(x1)(x2)(x3)(x7)(x9)(x)(st))(i)(xs)(st)
c__case_62_case__74 x1 x2 x3 x7 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_62_case__74")(x)



c__case_63_case__75 x1 x2 x3 x7 x9 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_62(x2)(x3)(x7)(x9)(x13)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_64_case__76 x1 x2 x3 x7 x9 x11@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_63(x2)(x3)(x7)(x9)(x13)(x12)(x1)(st))(st)
c__case_64_case__76 x1 x2 x3 x7 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_64_case__76(x1)(x2)(x3)(x7)(x9)(x)(st))(i)(xs)(st)
c__case_64_case__76 x1 x2 x3 x7 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_64_case__76")(x)



c__case_65_case__77 x1 x2 x3 x7 x9 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('P'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_64(x2)(x3)(x7)(x9)(x11)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_66_case__78 x1 x2 x3 x7 x9 x8@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_65(x2)(x3)(x7)(x9)(x11)(x10)(x1)(st))(st)
c__case_66_case__78 x1 x2 x3 x7 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_66_case__78(x1)(x2)(x3)(x7)(x9)(x)(st))(i)(xs)(st)
c__case_66_case__78 x1 x2 x3 x7 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_66_case__78")(x)



c__case_67_case__79 x1 x2 x3 x7 x6@(Curry.Module.Prelude.T2 x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_66(x2)(x3)(x7)(x9)(x8)(x1)(st))(st)
c__case_67_case__79 x1 x2 x3 x7 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_67_case__79(x1)(x2)(x3)(x7)(x)(st))(i)(xs)(st)
c__case_67_case__79 x1 x2 x3 x7 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_67_case__79")(x)



c__case_68_case__80 x1 x2 x3 x4@(Curry.Module.FlatCurry.C_Comb x5 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_67(x2)(x3)(x7)(x6)(x1)(st))(st)
c__case_68_case__80 x1 x2 x3 (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_68_case__80(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_68_case__80 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_68_case__80")(x)



c__case_19_case__81 x1 x5@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_19_case__81 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_19_case__81(x1)(x)(st))(i)(xs)(st)
c__case_19_case__81 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_19_case__81")(x)



c__case_20_case__82 x1 x5 x25@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_19(x5)(x1)(st))(st)
c__case_20_case__82 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_20_case__82(x1)(x5)(x)(st))(i)(xs)(st)
c__case_20_case__82 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_20_case__82")(x)



c__case_21_case__83 x1 x5 x25 x24 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x24)(Curry.Module.Prelude.C_Char(']'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_20(x5)(x25)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_22_case__84 x1 x5 x23@((Curry.Module.Prelude.:<) x24 x25) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_21(x5)(x25)(x24)(x1)(st))(st)
c__case_22_case__84 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_22_case__84(x1)(x5)(x)(st))(i)(xs)(st)
c__case_22_case__84 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_22_case__84")(x)



c__case_15_case__85 x1 x26 x28 x29@Curry.Module.Prelude.List st = let {x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x30)((Curry.Module.Prelude.:<)(x31)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryShow.c_showCharExpr(x26)(x1)(st))(Curry.Module.OracleFlatCurryShow.c_showCurryStringConstant(x28)(x30)(st))(x31)(st))(st)
c__case_15_case__85 x1 x26 x28 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_15_case__85(x1)(x26)(x28)(x)(st))(i)(xs)(st)
c__case_15_case__85 x1 x26 x28 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_15_case__85")(x)



c__case_16_case__86 x1 x26 x27@((Curry.Module.Prelude.:<) x28 x29) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_15(x26)(x28)(x29)(x1)(st))(st)
c__case_16_case__86 x1 x26 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_16_case__86(x1)(x26)(x)(st))(i)(xs)(st)
c__case_16_case__86 x1 x26 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_16_case__86")(x)



c__case_17_case__87 x1 x5@((Curry.Module.Prelude.:<) x26 x27) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_16(x26)(x27)(x1)(st))(st)
c__case_17_case__87 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_17_case__87(x1)(x)(st))(i)(xs)(st)
c__case_17_case__87 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_17_case__87")(x)



c__case_18_case__88 x1 x5 x23@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_17(x5)(x1)(st))(st)
c__case_18_case__88 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_18_case__88(x1)(x5)(x)(st))(i)(xs)(st)
c__case_18_case__88 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_18_case__88")(x)



c__case_23_case__89 x1 x5 x23 x22 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x22)(Curry.Module.Prelude.C_Char('['))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_22(x5)(x23)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x22)(Curry.Module.Prelude.C_Char(':'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_18(x5)(x23)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st)



c__case_24_case__90 x1 x5 x7@((Curry.Module.Prelude.:<) x22 x23) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_23(x5)(x23)(x22)(x1)(st))(st)
c__case_24_case__90 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_24_case__90(x1)(x5)(x)(st))(i)(xs)(st)
c__case_24_case__90 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_24_case__90")(x)



c__case_25_case__91 x1 x5 x7 x21@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_24(x5)(x7)(x1)(st))(st)
c__case_25_case__91 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_25_case__91(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_25_case__91 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_25_case__91")(x)



c__case_26_case__92 x1 x5 x7 x21 x20 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x20)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_25(x5)(x7)(x21)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_27_case__93 x1 x5 x7 x19@((Curry.Module.Prelude.:<) x20 x21) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_26(x5)(x7)(x21)(x20)(x1)(st))(st)
c__case_27_case__93 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_27_case__93(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_27_case__93 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_27_case__93")(x)



c__case_28_case__94 x1 x5 x7 x19 x18 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x18)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_27(x5)(x7)(x19)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_29_case__95 x1 x5 x7 x17@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_28(x5)(x7)(x19)(x18)(x1)(st))(st)
c__case_29_case__95 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_29_case__95(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_29_case__95 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_29_case__95")(x)



c__case_30_case__96 x1 x5 x7 x17 x16 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x16)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_29(x5)(x7)(x17)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_31_case__97 x1 x5 x7 x15@((Curry.Module.Prelude.:<) x16 x17) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_30(x5)(x7)(x17)(x16)(x1)(st))(st)
c__case_31_case__97 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_31_case__97(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_31_case__97 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_31_case__97")(x)



c__case_32_case__98 x1 x5 x7 x15 x14 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x14)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_31(x5)(x7)(x15)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_33_case__99 x1 x5 x7 x13@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_32(x5)(x7)(x15)(x14)(x1)(st))(st)
c__case_33_case__99 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_33_case__99(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_33_case__99 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_33_case__99")(x)



c__case_34_case__100 x1 x5 x7 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_33(x5)(x7)(x13)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_35_case__101 x1 x5 x7 x11@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_34(x5)(x7)(x13)(x12)(x1)(st))(st)
c__case_35_case__101 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_35_case__101(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_35_case__101 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_35_case__101")(x)



c__case_36_case__102 x1 x5 x7 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_35(x5)(x7)(x11)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_37_case__103 x1 x5 x7 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_36(x5)(x7)(x11)(x10)(x1)(st))(st)
c__case_37_case__103 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_37_case__103(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_37_case__103 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_37_case__103")(x)



c__case_38_case__104 x1 x5 x7 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('P'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_37(x5)(x7)(x9)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_39_case__105 x1 x5 x7 x6@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_38(x5)(x7)(x9)(x8)(x1)(st))(st)
c__case_39_case__105 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_39_case__105(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_39_case__105 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_39_case__105")(x)



c__case_40_case__106 x1 x5 x4@(Curry.Module.Prelude.T2 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_39(x5)(x7)(x6)(x1)(st))(st)
c__case_40_case__106 x1 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_40_case__106(x1)(x5)(x)(st))(i)(xs)(st)
c__case_40_case__106 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_40_case__106")(x)



c__case_41_case__107 x1 x2@(Curry.Module.FlatCurry.C_Comb x3 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_40(x5)(x4)(x1)(st))(st)
c__case_41_case__107 x1 (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_41_case__107(x1)(x)(st))(i)(xs)(st)
c__case_41_case__107 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_41_case__107")(x)



c__case_8_case__108 x1 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(st)
c__case_8_case__108 x1 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_8_case__108 x1 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_8_case__108(x1)(x4)(x)(st))(i)(xs)(st)
c__case_8_case__108 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_8_case__108")(x)



c__case_9_case__109 x1 x4 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.c_chr(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.c_div(x5)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))))(x1)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))(x7)(st))(x8)(st))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.c_chr(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.c_div(Curry.Module.OraclePrelude.c_mod(x5)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))))(x9)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(x10)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))(x11)(st))(x12)(st))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.c_chr(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.c_mod(x5)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(x13)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))(x14)(st))(x15)(st))(Curry.Module.Prelude.List)))))(st)
c__case_9_case__109 x1 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c__case_8(x4)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x16)(st))(st)
c__case_9_case__109 x1 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_9_case__109(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_9_case__109 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_9_case__109")(x)



c__case_10_case__110 x1 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List)))(st)
c__case_10_case__110 x1 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))))(Curry.Module.OracleFlatCurryShow.c__case_9(x4)(x5)(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_60(x5)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))(x1)(st))(Curry.Module.OraclePrelude.op_62(x5)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))))(x7)(st))(x8)(st))(x9)(st))(st)
c__case_10_case__110 x1 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_10_case__110(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_10_case__110 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_10_case__110")(x)



c__case_11_case__111 x1 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))(Curry.Module.Prelude.List)))(st)
c__case_11_case__111 x1 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c__case_10(x4)(x5)(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.Prelude.C_Char('\n'))(x1)(st))(x7)(st))(st)
c__case_11_case__111 x1 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_11_case__111(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_11_case__111 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_11_case__111")(x)



c__case_12_case__112 x1 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List)))(st)
c__case_12_case__112 x1 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c__case_11(x4)(x5)(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.Prelude.C_Char('\''))(x1)(st))(x7)(st))(st)
c__case_12_case__112 x1 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_12_case__112(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_12_case__112 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_12_case__112")(x)



c__case_13_case__113 x1 x3@(Curry.Module.FlatCurry.C_Charc x4) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c__case_12(x4)(Curry.Module.OraclePrelude.c_ord(x4)(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.Prelude.C_Char('\"'))(x6)(st))(x7)(st))(st))(st)
c__case_13_case__113 x1 (Curry.Module.FlatCurry.C_LiteralOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_13_case__113(x1)(x)(st))(i)(xs)(st)
c__case_13_case__113 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_13_case__113")(x)



c__case_14_case__114 x1 x2@(Curry.Module.FlatCurry.C_Lit x3) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_13(x3)(x1)(st))(st)
c__case_14_case__114 x1 (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_14_case__114(x1)(x)(st))(i)(xs)(st)
c__case_14_case__114 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_14_case__114")(x)



c__case_7_case__115 x1 x3 x2@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.OraclePrelude.op_43_43(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(x1)(st)))(st)
c__case_7_case__115 x1 x3 x2@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_7_case__115 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_7_case__115(x1)(x3)(x)(st))(i)(xs)(st)
c__case_7_case__115 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_7_case__115")(x)



c__case_3_case__116 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_3_case__116 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_3_case__116 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_3_case__116(x1)(x)(st))(i)(xs)(st)
c__case_3_case__116 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_3_case__116")(x)



c__case_4_case__117 x1 x7 x8@Curry.Module.Prelude.C_True st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c_isFiniteList(Curry.Module.OraclePrelude.op_33_33(x7)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x9)(st))(st)
c__case_4_case__117 x1 x7 x8@Curry.Module.Prelude.C_False st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryShow.c__case_3(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x10)(st))(st)
c__case_4_case__117 x1 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_4_case__117(x1)(x7)(x)(st))(i)(xs)(st)
c__case_4_case__117 x1 x7 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_4_case__117")(x)



c__case_5_case__118 x1 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_5_case__118 x1 x6 x7 x8@Curry.Module.Prelude.C_False st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List)))))(Curry.Module.OracleFlatCurryShow.c__case_4(x6)(x7)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x6)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List)))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_length(x7)(x9)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x10)(st))(x11)(st))(x12)(st))(st)
c__case_5_case__118 x1 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_5_case__118(x1)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_5_case__118 x1 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_5_case__118")(x)



c__case_6_case__119 x1 x2@(Curry.Module.FlatCurry.C_Var x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_6_case__119 x1 x2@(Curry.Module.FlatCurry.C_Lit x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_6_case__119 x1 x2@(Curry.Module.FlatCurry.C_Comb x5 x6 x7) st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))))(Curry.Module.OracleFlatCurryShow.c__case_5(x6)(x7)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x6)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x7)(Curry.Module.Prelude.List)(x17)(st))(x18)(st))(x19)(st))(st)
c__case_6_case__119 x1 x2@(Curry.Module.FlatCurry.C_Let x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_6_case__119 x1 x2@(Curry.Module.FlatCurry.C_Free x10 x11) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_6_case__119 x1 x2@(Curry.Module.FlatCurry.C_Or x12 x13) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_6_case__119 x1 x2@(Curry.Module.FlatCurry.C_Case x14 x15 x16) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_6_case__119 x1 (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_6_case__119(x1)(x)(st))(i)(xs)(st)
c__case_6_case__119 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_6_case__119")(x)



c__case_2_case__120 x1 x2@(Curry.Module.FlatCurry.C_Comb x3 x4 x5) st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)(Curry.Module.Prelude.List))))))))))))))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))))(x1)(st))(Curry.Module.OraclePrelude.c_null(x5)(x17)(st))(x18)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List)))(x19)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_length(x5)(x20)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x21)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OracleFlatCurryShow.c_isCharConstant(Curry.Module.OraclePrelude.c_head(x5)(x22)(st))(x23)(st))(Curry.Module.OracleFlatCurryShow.c_isStringConstant(Curry.Module.OraclePrelude.op_33_33(x5)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x24)(st))(x25)(st))(x26)(st))(x27)(st))(x28)(st))(x29)(st))(st)
c__case_2_case__120 x1 x2@(Curry.Module.FlatCurry.C_Var x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_2_case__120 x1 x2@(Curry.Module.FlatCurry.C_Lit x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_2_case__120 x1 x2@(Curry.Module.FlatCurry.C_Let x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_2_case__120 x1 x2@(Curry.Module.FlatCurry.C_Free x10 x11) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_2_case__120 x1 x2@(Curry.Module.FlatCurry.C_Or x12 x13) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_2_case__120 x1 x2@(Curry.Module.FlatCurry.C_Case x14 x15 x16) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_2_case__120 x1 (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_2_case__120(x1)(x)(st))(i)(xs)(st)
c__case_2_case__120 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_2_case__120")(x)



c__case_0_case__121 x1 x3@(Curry.Module.FlatCurry.C_Charc x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_0_case__121 x1 x3@(Curry.Module.FlatCurry.C_Intc x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_0_case__121 x1 x3@(Curry.Module.FlatCurry.C_Floatc x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_0_case__121 x1 (Curry.Module.FlatCurry.C_LiteralOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_0_case__121(x1)(x)(st))(i)(xs)(st)
c__case_0_case__121 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_0_case__121")(x)



c__case_1_case__122 x1 x2@(Curry.Module.FlatCurry.C_Lit x3) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryShow.c__case_0(x3)(x1)(st))(st)
c__case_1_case__122 x1 x2@(Curry.Module.FlatCurry.C_Var x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_1_case__122 x1 x2@(Curry.Module.FlatCurry.C_Comb x8 x9 x10) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_1_case__122 x1 x2@(Curry.Module.FlatCurry.C_Let x11 x12) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_1_case__122 x1 x2@(Curry.Module.FlatCurry.C_Free x13 x14) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_1_case__122 x1 x2@(Curry.Module.FlatCurry.C_Or x15 x16) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_1_case__122 x1 x2@(Curry.Module.FlatCurry.C_Case x17 x18 x19) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_1_case__122 x1 (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryShow.c__case_1_case__122(x1)(x)(st))(i)(xs)(st)
c__case_1_case__122 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryShow._case_1_case__122")(x)


