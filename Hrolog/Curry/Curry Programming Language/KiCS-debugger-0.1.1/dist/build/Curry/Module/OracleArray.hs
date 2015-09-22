{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleArray (module Curry.Module.OracleArray) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.Array
import Curry.Module.Integer
import Curry.Module.Prelude
import Curry.Module.OracleInteger
import Curry.Module.OraclePrelude



-- begin included



-- end included

data C_Array t0 = C_Array (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t0))) (Curry.Module.Array.C_Entry t0)
  | C_ArrayFail Curry.RunTimeSystem.C_Exceptions
  | C_ArrayOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.OracleArray.C_Array t0))

instance (BaseCurry t0) => BaseCurry (Curry.Module.OracleArray.C_Array t0) where
  nf f (Curry.Module.OracleArray.C_Array x1 x2) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> f(Curry.Module.OracleArray.C_Array(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.OracleArray.C_Array x1 x2) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> f(Curry.Module.OracleArray.C_Array(v1)(v2))(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.OracleArray.C_ArrayOr(Curry.RunTimeSystem.mkRef(r)(2)(i))([Curry.Module.OracleArray.C_Array(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(2)

  failed  = Curry.Module.OracleArray.C_ArrayFail

  branching  = Curry.Module.OracleArray.C_ArrayOr

  consKind (Curry.Module.OracleArray.C_ArrayOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.OracleArray.C_ArrayFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.OracleArray.C_ArrayFail x) = x

  orRef (Curry.Module.OracleArray.C_ArrayOr x _) = x

  branches (Curry.Module.OracleArray.C_ArrayOr _ x) = x





instance (Curry t0) => Curry (Curry.Module.OracleArray.C_Array t0) where
  strEq (Curry.Module.OracleArray.C_Array x1 x2) (Curry.Module.OracleArray.C_Array y1 y2) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.OracleArray.C_Array x1 x2) (Curry.Module.OracleArray.C_Array y1 y2) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.genEq(x2)(y2)(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.OracleArray.C_Array x1 x2) st = Curry.Module.OracleArray.C_Array(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))

  foldCurry f c (Curry.Module.OracleArray.C_Array x1 x2) st = f(x1)(f(x2)(c)(st))(st)

  typeName _ = "Array"

  showQ d (Curry.Module.OracleArray.C_Array x1 x2) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("OracleArray.Array "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))))


  showQ _ (Curry.Module.OracleArray.C_ArrayOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.OracleArray.C_Array t0) where
  showsPrec d (Curry.Module.OracleArray.C_Array x1 x2) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Array "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))))


  showsPrec _ (Curry.Module.OracleArray.C_ArrayOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Read t0) => Read (Curry.Module.OracleArray.C_Array t0) where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.OracleArray.C_Array(x1)(x2))(r2) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("OracleArray")("Array")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1)])(r)





c_emptyErrorArray :: (Curry t0) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleArray.C_Array t0
c_emptyErrorArray x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c_emptyDefaultArray(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleArray.c_errorArray))))(x1)(st))(st)



c_errorArray :: (Curry t0) => Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_errorArray x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.c_error(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('z'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))(Curry.Module.Prelude.List)))))))))))))))))(x3)(st))(x4)(st))(x5)(st))(st)



c_emptyDefaultArray :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t0))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleArray.C_Array t0
c_emptyDefaultArray x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.OracleArray.C_Array(x2)(Curry.Module.Array.C_Empty))(st)



op_47_47 :: (Curry t0) => (Curry.Module.OracleArray.C_Array t0) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleArray.C_Array t0
op_47_47 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_33(x3)(x2)(x1)(st))(st)



c_'47'47'46_'35lambda2 :: (Curry t72) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t72))) -> (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t72) -> (Curry.Module.Array.C_Entry t72) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Array.C_Entry t72
c_'47'47'46_'35lambda2 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_32(x2)(x4)(x3)(x1)(st))(st)



c_update :: (Curry t0) => (Curry.Module.OracleArray.C_Array t0) -> Curry.Module.Prelude.C_Int -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleArray.C_Array t0
c_update x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_31(x3)(x4)(x2)(x1)(st))(st)



c_applyAt :: (Curry t0) => (Curry.Module.OracleArray.C_Array t0) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleArray.C_Array t0
c_applyAt x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_30(x3)(x4)(x2)(x1)(st))(st)



c_at :: (Curry t0) => t0 -> (Curry.Module.Array.C_Entry t0) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Array.C_Entry t0
c_at x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_29(x2)(x4)(x5)(x3)(x1)(st))(st)



op_33 :: (Curry t0) => (Curry.Module.OracleArray.C_Array t0) -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
op_33 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_22(x3)(x2)(x1)(st))(st)



c_from :: (Curry t0) => t0 -> (Curry.Module.Array.C_Entry t0) -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_from x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_21(x2)(x4)(x3)(x1)(st))(st)



c_split :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t0)
c_split x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_17(x2)(x1)(st))(st)



c_split'46_'35selFP3'35xs :: (Curry t126) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t126) (Curry.Module.Prelude.List t126)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t126
c_split'46_'35selFP3'35xs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_15(x2)(x1)(st))(st)



c_split'46_'35selFP4'35ys :: (Curry t126) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t126) (Curry.Module.Prelude.List t126)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t126
c_split'46_'35selFP4'35ys x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_14(x2)(x1)(st))(st)



c_listToDefaultArray :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t0))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.OracleArray.C_Array t0))
c_listToDefaultArray x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc(Curry.Module.OracleArray.C_Array(x2)))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleArray.c_listToArray))))(x1)(st))(st)



c_listToErrorArray :: (Curry t0) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.OracleArray.C_Array t0))
c_listToErrorArray x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c_listToDefaultArray(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleArray.c_errorArray))))(x1)(st))(st)



c_listToArray :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Array.C_Entry t0
c_listToArray x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_13(x2)(x1)(st))(st)



c_listToArray'46_'35selFP6'35ys :: (Curry t137) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t137) (Curry.Module.Prelude.List t137)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t137
c_listToArray'46_'35selFP6'35ys x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_12(x2)(x1)(st))(st)



c_listToArray'46_'35selFP7'35zs :: (Curry t137) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t137) (Curry.Module.Prelude.List t137)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t137
c_listToArray'46_'35selFP7'35zs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_11(x2)(x1)(st))(st)



c_combine :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t2))))) -> (Curry.Module.OracleArray.C_Array t0) -> (Curry.Module.OracleArray.C_Array t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleArray.C_Array t2
c_combine x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_10(x2)(x4)(x3)(x1)(st))(st)



c_combine'46_'35lambda3 :: (Curry t252,Curry t255,Curry t263) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t252))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t255))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t252 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t255 -> Curry.RunTimeSystem.State -> t263))))) -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t263
c_combine'46_'35lambda3 x2 x3 x4 x5 x1 st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x4)(Curry.Module.Oracle.c_apply(x2)(x5)(x1)(st))(x6)(st))(Curry.Module.Oracle.c_apply(x3)(x5)(x7)(st))(x8)(st))(st)



c_comb :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t2))))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t0))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> t1))) -> (Curry.Module.Array.C_Entry t0) -> (Curry.Module.Array.C_Entry t1) -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Array.C_Entry t2
c_comb x2 x3 x4 x5 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_8(x2)(x3)(x4)(x6)(x7)(x8)(x5)(x1)(st))(st)



c_combineSimilar :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0))))) -> (Curry.Module.OracleArray.C_Array t0) -> (Curry.Module.OracleArray.C_Array t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleArray.C_Array t0
c_combineSimilar x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_5(x2)(x4)(x3)(x1)(st))(st)



c_combSim :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0))))) -> (Curry.Module.Array.C_Entry t0) -> (Curry.Module.Array.C_Entry t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Array.C_Entry t0
c_combSim x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_3(x2)(x4)(x3)(x1)(st))(st)



c_foldArray :: (Curry t0,Curry t1) => t0 -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0))))))) -> (Curry.Module.OracleArray.C_Array t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_foldArray x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_1(x2)(x3)(x4)(x1)(st))(st)



c_foldEntries :: (Curry t0,Curry t1) => t0 -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0))))))) -> (Curry.Module.Array.C_Entry t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_foldEntries x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_0(x2)(x3)(x4)(x1)(st))(st)



c_arrayToList :: (Curry t0) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleArray.C_Array t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0))
c_arrayToList x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleArray.c_foldArray(Curry.Module.Prelude.List)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.pf(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))))(Curry.Module.OracleArray.c_arrayToList'46_'35lambda4))(st))))))(st)



c_arrayToList'46_'35lambda4 :: (Curry t345) => t345 -> (Curry.Module.Prelude.List t345) -> (Curry.Module.Prelude.List t345) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t345
c_arrayToList'46_'35lambda4 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.OraclePrelude.op_43_43(x3)(x4)(x1)(st)))(st)



c__case_0 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_0_case__33(x1)(x2)(x3)(x4)(st))(st)



c__case_1 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_1_case__32(x1)(x2)(x3)(x4)(st))(st)



c__case_3 x2 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_3_case__31(x1)(x2)(x4)(x3)(st))(st)



c__case_2 x2 x3 x5 x6 x7 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_2_case__30(x1)(x2)(x3)(x5)(x6)(x7)(x4)(st))(st)



c__case_5 x2 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_5_case__29(x1)(x2)(x4)(x3)(st))(st)



c__case_4 x2 x5 x6 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_4_case__28(x1)(x2)(x5)(x6)(x4)(st))(st)



c__case_8 x2 x3 x4 x6 x7 x8 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_8_case__27(x1)(x2)(x3)(x4)(x6)(x7)(x8)(x5)(st))(st)



c__case_6 x2 x3 x4 x7 x8 x12 x13 x14 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_6_case__26(x1)(x2)(x3)(x4)(x7)(x8)(x12)(x13)(x14)(x6)(st))(st)



c__case_7 x2 x3 x4 x7 x8 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_7_case__25(x1)(x2)(x3)(x4)(x7)(x8)(x6)(st))(st)



c__case_10 x2 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_10_case__24(x1)(x2)(x4)(x3)(st))(st)



c__case_9 x2 x5 x6 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_9_case__23(x1)(x2)(x5)(x6)(x4)(st))(st)



c__case_11 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_11_case__22(x1)(x2)(st))(st)



c__case_12 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_12_case__21(x1)(x2)(st))(st)



c__case_13 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_13_case__20(x1)(x2)(st))(st)



c__case_14 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_14_case__19(x1)(x2)(st))(st)



c__case_15 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_15_case__18(x1)(x2)(st))(st)



c__case_17 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_17_case__17(x1)(x2)(st))(st)



c__case_16 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_16_case__16(x1)(x3)(x4)(st))(st)



c__case_21 x2 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_21_case__15(x1)(x2)(x4)(x3)(st))(st)



c__case_20 x2 x4 x5 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_20_case__14(x1)(x2)(x4)(x5)(x6)(x7)(x8)(st))(st)



c__case_19 x2 x4 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_19_case__13(x1)(x2)(x4)(x6)(x7)(x8)(st))(st)



c__case_18 x2 x4 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_18_case__12(x1)(x2)(x4)(x7)(x8)(st))(st)



c__case_22 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_22_case__11(x1)(x3)(x2)(st))(st)



c__case_29 x2 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_29_case__10(x1)(x2)(x4)(x5)(x3)(st))(st)



c__case_25 x2 x4 x5 x6 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_25_case__9(x1)(x2)(x4)(x5)(x6)(x7)(x8)(x9)(st))(st)



c__case_24 x2 x4 x5 x6 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_24_case__8(x1)(x2)(x4)(x5)(x6)(x7)(x8)(x9)(st))(st)



c__case_23 x2 x4 x5 x6 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_23_case__7(x1)(x2)(x4)(x5)(x6)(x7)(x8)(x9)(st))(st)



c__case_28 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_28_case__6(x1)(x2)(x4)(x5)(x6)(st))(st)



c__case_27 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_27_case__5(x1)(x2)(x4)(x5)(x6)(st))(st)



c__case_26 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_26_case__4(x1)(x2)(x4)(x5)(x6)(st))(st)



c__case_30 x3 x4 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_30_case__3(x1)(x3)(x4)(x2)(st))(st)



c__case_31 x3 x4 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_31_case__2(x1)(x3)(x4)(x2)(st))(st)



c__case_32 x2 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_32_case__1(x1)(x2)(x4)(x3)(st))(st)



c__case_33 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_33_case__0(x1)(x3)(x2)(st))(st)



c__case_33_case__0 x1 x3 x2@(Curry.Module.OracleArray.C_Array x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.C_Array(x4)(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleArray.c_'47'47'46_'35lambda2(x4)))(st))(x5)(x3)(x1)(st)))(st)
c__case_33_case__0 x1 x3 (Curry.Module.OracleArray.C_ArrayOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_33_case__0(x1)(x3)(x)(st))(i)(xs)(st)
c__case_33_case__0 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_33_case__0")(x)



c__case_32_case__1 x1 x2 x4 x3@(Curry.Module.Prelude.T2 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleArray.c_at(Curry.Module.Oracle.c_apply(x2)(x5)(x1)(st))(x4)(x5)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_const(x6)))))(x7)(st))(st)
c__case_32_case__1 x1 x2 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_32_case__1(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_32_case__1 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_32_case__1")(x)



c__case_31_case__2 x1 x3 x4 x2@(Curry.Module.OracleArray.C_Array x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleArray.C_Array(x5)(Curry.Module.OracleArray.c_at(Curry.Module.Oracle.c_apply(x5)(x3)(x1)(st))(x6)(x3)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_const(x4)))))(x7)(st)))(st)
c__case_31_case__2 x1 x3 x4 (Curry.Module.OracleArray.C_ArrayOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_31_case__2(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_31_case__2 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_31_case__2")(x)



c__case_30_case__3 x1 x3 x4 x2@(Curry.Module.OracleArray.C_Array x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleArray.C_Array(x5)(Curry.Module.OracleArray.c_at(Curry.Module.Oracle.c_apply(x5)(x3)(x1)(st))(x6)(x3)(x4)(x7)(st)))(st)
c__case_30_case__3 x1 x3 x4 (Curry.Module.OracleArray.C_ArrayOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_30_case__3(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_30_case__3 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_30_case__3")(x)



c__case_26_case__4 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.Array.C_Entry(x2)(Curry.Module.Array.C_Empty)(Curry.Module.OracleArray.c_at(x2)(Curry.Module.Array.C_Empty)(Curry.Module.OraclePrelude.op_45(Curry.Module.OraclePrelude.c_div(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x7)(st))(x5)(x8)(st)))(st)
c__case_26_case__4 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_26_case__4 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_26_case__4(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_26_case__4 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_26_case__4")(x)



c__case_27_case__5 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.Array.C_Entry(x2)(Curry.Module.OracleArray.c_at(x2)(Curry.Module.Array.C_Empty)(Curry.Module.OraclePrelude.c_div(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(x5)(x7)(st))(Curry.Module.Array.C_Empty))(st)
c__case_27_case__5 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleArray.c__case_26(x2)(x4)(x5)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x8)(st))(st)
c__case_27_case__5 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_27_case__5(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_27_case__5 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_27_case__5")(x)



c__case_28_case__6 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Array.C_Entry(Curry.Module.Oracle.c_apply(x5)(x2)(x1)(st))(Curry.Module.Array.C_Empty)(Curry.Module.Array.C_Empty))(st)
c__case_28_case__6 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleArray.c__case_27(x2)(x4)(x5)(Curry.Module.OracleInteger.c_odd(x4)(x1)(st))(x7)(st))(st)
c__case_28_case__6 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_28_case__6(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_28_case__6 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_28_case__6")(x)



c__case_23_case__7 x1 x2 x4 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))(Curry.Module.Array.C_Entry(x6)(x7)(Curry.Module.OracleArray.c_at(x2)(x8)(Curry.Module.OraclePrelude.op_45(Curry.Module.OraclePrelude.c_div(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x10)(st))(x5)(x11)(st)))(st)
c__case_23_case__7 x1 x2 x4 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_23_case__7 x1 x2 x4 x5 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_23_case__7(x1)(x2)(x4)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_23_case__7 x1 x2 x4 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_23_case__7")(x)



c__case_24_case__8 x1 x2 x4 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.Array.C_Entry(x6)(Curry.Module.OracleArray.c_at(x2)(x7)(Curry.Module.OraclePrelude.c_div(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(x5)(x10)(st))(x8))(st)
c__case_24_case__8 x1 x2 x4 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleArray.c__case_23(x2)(x4)(x5)(x6)(x7)(x8)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x11)(st))(st)
c__case_24_case__8 x1 x2 x4 x5 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_24_case__8(x1)(x2)(x4)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_24_case__8 x1 x2 x4 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_24_case__8")(x)



c__case_25_case__9 x1 x2 x4 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Array.C_Entry(Curry.Module.Oracle.c_apply(x5)(x6)(x1)(st))(x7)(x8))(st)
c__case_25_case__9 x1 x2 x4 x5 x6 x7 x8 x9@Curry.Module.Prelude.C_False st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.OracleArray.c__case_24(x2)(x4)(x5)(x6)(x7)(x8)(Curry.Module.OracleInteger.c_odd(x4)(x1)(st))(x10)(st))(st)
c__case_25_case__9 x1 x2 x4 x5 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_25_case__9(x1)(x2)(x4)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_25_case__9 x1 x2 x4 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_25_case__9")(x)



c__case_29_case__10 x1 x2 x4 x5 x3@Curry.Module.Array.C_Empty st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OracleArray.c__case_28(x2)(x4)(x5)(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.Prelude.C_Zero)(x1)(st))(x9)(st))(st)
c__case_29_case__10 x1 x2 x4 x5 x3@(Curry.Module.Array.C_Entry x6 x7 x8) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.OracleArray.c__case_25(x2)(x4)(x5)(x6)(x7)(x8)(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.Prelude.C_Zero)(x1)(st))(x10)(st))(st)
c__case_29_case__10 x1 x2 x4 x5 (Curry.Module.Array.C_EntryOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_29_case__10(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_29_case__10 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_29_case__10")(x)



c__case_22_case__11 x1 x3 x2@(Curry.Module.OracleArray.C_Array x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleArray.c_from(Curry.Module.Oracle.c_apply(x4)(x3)(x1)(st))(x5)(x3)(x6)(st))(st)
c__case_22_case__11 x1 x3 (Curry.Module.OracleArray.C_ArrayOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_22_case__11(x1)(x3)(x)(st))(i)(xs)(st)
c__case_22_case__11 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_22_case__11")(x)



c__case_18_case__12 x1 x2 x4 x7 x8@Curry.Module.Prelude.C_True st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))(Curry.Module.OracleArray.c_from(x2)(x7)(Curry.Module.OraclePrelude.op_45(Curry.Module.OraclePrelude.c_div(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x9)(st))(x10)(st))(st)
c__case_18_case__12 x1 x2 x4 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_18_case__12 x1 x2 x4 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_18_case__12(x1)(x2)(x4)(x7)(x)(st))(i)(xs)(st)
c__case_18_case__12 x1 x2 x4 x7 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_18_case__12")(x)



c__case_19_case__13 x1 x2 x4 x6 x7 x8@Curry.Module.Prelude.C_True st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OracleArray.c_from(x2)(x6)(Curry.Module.OraclePrelude.c_div(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(x9)(st))(st)
c__case_19_case__13 x1 x2 x4 x6 x7 x8@Curry.Module.Prelude.C_False st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.OracleArray.c__case_18(x2)(x4)(x7)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x10)(st))(st)
c__case_19_case__13 x1 x2 x4 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_19_case__13(x1)(x2)(x4)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_19_case__13 x1 x2 x4 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_19_case__13")(x)



c__case_20_case__14 x1 x2 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_20_case__14 x1 x2 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_False st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OracleArray.c__case_19(x2)(x4)(x6)(x7)(Curry.Module.OracleInteger.c_odd(x4)(x1)(st))(x9)(st))(st)
c__case_20_case__14 x1 x2 x4 x5 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_20_case__14(x1)(x2)(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_20_case__14 x1 x2 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_20_case__14")(x)



c__case_21_case__15 x1 x2 x4 x3@Curry.Module.Array.C_Empty st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_21_case__15 x1 x2 x4 x3@(Curry.Module.Array.C_Entry x5 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OracleArray.c__case_20(x2)(x4)(x5)(x6)(x7)(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.Prelude.C_Zero)(x1)(st))(x8)(st))(st)
c__case_21_case__15 x1 x2 x4 (Curry.Module.Array.C_EntryOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_21_case__15(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_21_case__15 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_21_case__15")(x)



c__case_16_case__16 x1 x3 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.Prelude.List))(st)
c__case_16_case__16 x1 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(let {x7 = Curry.Module.OracleArray.c_split(x6)(x1)(st)} in let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x12)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x3)(Curry.Module.OracleArray.c_split'46_'35selFP3'35xs(x7)(x10)(st)))((Curry.Module.Prelude.:<)(x5)(Curry.Module.OracleArray.c_split'46_'35selFP4'35ys(x7)(x11)(st))))(st))(st))(st))(st)
c__case_16_case__16 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_16_case__16(x1)(x3)(x)(st))(i)(xs)(st)
c__case_16_case__16 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_16_case__16")(x)



c__case_17_case__17 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(st)
c__case_17_case__17 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_16(x3)(x4)(x1)(st))(st)
c__case_17_case__17 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_17_case__17(x1)(x)(st))(i)(xs)(st)
c__case_17_case__17 x1 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_17_case__17")(x)



c__case_15_case__18 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_15_case__18 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_15_case__18(x1)(x)(st))(i)(xs)(st)
c__case_15_case__18 x1 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_15_case__18")(x)



c__case_14_case__19 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_14_case__19 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_14_case__19(x1)(x)(st))(i)(xs)(st)
c__case_14_case__19 x1 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_14_case__19")(x)



c__case_13_case__20 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Array.C_Empty)(st)
c__case_13_case__20 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(let {x5 = Curry.Module.OracleArray.c_split(x4)(x1)(st)} in let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.Array.C_Entry(x3)(Curry.Module.OracleArray.c_listToArray(Curry.Module.OracleArray.c_listToArray'46_'35selFP6'35ys(x5)(x8)(st))(x10)(st))(Curry.Module.OracleArray.c_listToArray(Curry.Module.OracleArray.c_listToArray'46_'35selFP7'35zs(x5)(x9)(st))(x11)(st)))(st))(st))(st))(st)
c__case_13_case__20 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_13_case__20(x1)(x)(st))(i)(xs)(st)
c__case_13_case__20 x1 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_13_case__20")(x)



c__case_12_case__21 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_12_case__21 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_12_case__21(x1)(x)(st))(i)(xs)(st)
c__case_12_case__21 x1 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_12_case__21")(x)



c__case_11_case__22 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_11_case__22 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_11_case__22(x1)(x)(st))(i)(xs)(st)
c__case_11_case__22 x1 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_11_case__22")(x)



c__case_9_case__23 x1 x2 x5 x6 x4@(Curry.Module.OracleArray.C_Array x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.C_Array(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleArray.c_combine'46_'35lambda3(x5)(x7)(x2)))))(Curry.Module.OracleArray.c_comb(x2)(x5)(x7)(x6)(x8)(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st)))(st)
c__case_9_case__23 x1 x2 x5 x6 (Curry.Module.OracleArray.C_ArrayOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_9_case__23(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_9_case__23 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_9_case__23")(x)



c__case_10_case__24 x1 x2 x4 x3@(Curry.Module.OracleArray.C_Array x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_9(x2)(x5)(x6)(x4)(x1)(st))(st)
c__case_10_case__24 x1 x2 x4 (Curry.Module.OracleArray.C_ArrayOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_10_case__24(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_10_case__24 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_10_case__24")(x)



c__case_7_case__25 x1 x2 x3 x4 x7 x8 x6@Curry.Module.Array.C_Empty st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Array.C_Empty)(st)
c__case_7_case__25 x1 x2 x3 x4 x7 x8 x6@(Curry.Module.Array.C_Entry x9 x10 x11) st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))))))))))(Curry.Module.Array.C_Entry(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(Curry.Module.Oracle.c_apply(x3)(Curry.Module.OraclePrelude.op_45(Curry.Module.OraclePrelude.op_43(x7)(x8)(x1)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x12)(st))(x13)(st))(x14)(st))(x9)(x15)(st))(Curry.Module.OracleArray.c_comb(x2)(x3)(x4)(Curry.Module.Array.C_Empty)(x10)(Curry.Module.OraclePrelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x7)(x16)(st))(x8)(x17)(st))(Curry.Module.OracleArray.c_comb(x2)(x3)(x4)(Curry.Module.Array.C_Empty)(x11)(Curry.Module.OraclePrelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x7)(x18)(st))(Curry.Module.OraclePrelude.op_43(x8)(x7)(x19)(st))(x20)(st)))(st)
c__case_7_case__25 x1 x2 x3 x4 x7 x8 (Curry.Module.Array.C_EntryOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_7_case__25(x1)(x2)(x3)(x4)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_7_case__25 x1 x2 x3 x4 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_7_case__25")(x)



c__case_6_case__26 x1 x2 x3 x4 x7 x8 x12 x13 x14 x6@Curry.Module.Array.C_Empty st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List))))))))))(Curry.Module.Array.C_Entry(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x12)(x1)(st))(Curry.Module.Oracle.c_apply(x4)(Curry.Module.OraclePrelude.op_45(Curry.Module.OraclePrelude.op_43(x7)(x8)(x18)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x19)(st))(x20)(st))(x21)(st))(Curry.Module.OracleArray.c_comb(x2)(x3)(x4)(x13)(Curry.Module.Array.C_Empty)(Curry.Module.OraclePrelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x7)(x22)(st))(x8)(x23)(st))(Curry.Module.OracleArray.c_comb(x2)(x3)(x4)(x14)(Curry.Module.Array.C_Empty)(Curry.Module.OraclePrelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x7)(x24)(st))(Curry.Module.OraclePrelude.op_43(x8)(x7)(x25)(st))(x26)(st)))(st)
c__case_6_case__26 x1 x2 x3 x4 x7 x8 x12 x13 x14 x6@(Curry.Module.Array.C_Entry x15 x16 x17) st = let {x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x27)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)((Curry.Module.Prelude.:<)(x30)((Curry.Module.Prelude.:<)(x31)((Curry.Module.Prelude.:<)(x32)(Curry.Module.Prelude.List)))))))(Curry.Module.Array.C_Entry(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x12)(x1)(st))(x15)(x27)(st))(Curry.Module.OracleArray.c_comb(x2)(x3)(x4)(x13)(x16)(Curry.Module.OraclePrelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x7)(x28)(st))(x8)(x29)(st))(Curry.Module.OracleArray.c_comb(x2)(x3)(x4)(x14)(x17)(Curry.Module.OraclePrelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x7)(x30)(st))(Curry.Module.OraclePrelude.op_43(x8)(x7)(x31)(st))(x32)(st)))(st)
c__case_6_case__26 x1 x2 x3 x4 x7 x8 x12 x13 x14 (Curry.Module.Array.C_EntryOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_6_case__26(x1)(x2)(x3)(x4)(x7)(x8)(x12)(x13)(x14)(x)(st))(i)(xs)(st)
c__case_6_case__26 x1 x2 x3 x4 x7 x8 x12 x13 x14 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_6_case__26")(x)



c__case_8_case__27 x1 x2 x3 x4 x6 x7 x8 x5@Curry.Module.Array.C_Empty st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_7(x2)(x3)(x4)(x7)(x8)(x6)(x1)(st))(st)
c__case_8_case__27 x1 x2 x3 x4 x6 x7 x8 x5@(Curry.Module.Array.C_Entry x12 x13 x14) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_6(x2)(x3)(x4)(x7)(x8)(x12)(x13)(x14)(x6)(x1)(st))(st)
c__case_8_case__27 x1 x2 x3 x4 x6 x7 x8 (Curry.Module.Array.C_EntryOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_8_case__27(x1)(x2)(x3)(x4)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_8_case__27 x1 x2 x3 x4 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_8_case__27")(x)



c__case_4_case__28 x1 x2 x5 x6 x4@(Curry.Module.OracleArray.C_Array x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.C_Array(x5)(Curry.Module.OracleArray.c_combSim(x2)(x6)(x8)(x1)(st)))(st)
c__case_4_case__28 x1 x2 x5 x6 (Curry.Module.OracleArray.C_ArrayOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_4_case__28(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_4_case__28 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_4_case__28")(x)



c__case_5_case__29 x1 x2 x4 x3@(Curry.Module.OracleArray.C_Array x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_4(x2)(x5)(x6)(x4)(x1)(st))(st)
c__case_5_case__29 x1 x2 x4 (Curry.Module.OracleArray.C_ArrayOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_5_case__29(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_5_case__29 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_5_case__29")(x)



c__case_2_case__30 x1 x2 x3 x5 x6 x7 x4@Curry.Module.Array.C_Empty st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_2_case__30 x1 x2 x3 x5 x6 x7 x4@(Curry.Module.Array.C_Entry x8 x9 x10) st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))))(Curry.Module.Array.C_Entry(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x5)(x1)(st))(x8)(x11)(st))(Curry.Module.OracleArray.c_combSim(x2)(x6)(x9)(x12)(st))(Curry.Module.OracleArray.c_combSim(x2)(x7)(x10)(x13)(st)))(st)
c__case_2_case__30 x1 x2 x3 x5 x6 x7 (Curry.Module.Array.C_EntryOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_2_case__30(x1)(x2)(x3)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_2_case__30 x1 x2 x3 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_2_case__30")(x)



c__case_3_case__31 x1 x2 x4 x3@Curry.Module.Array.C_Empty st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_3_case__31 x1 x2 x4 x3@(Curry.Module.Array.C_Entry x5 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c__case_2(x2)(x3)(x5)(x6)(x7)(x4)(x1)(st))(st)
c__case_3_case__31 x1 x2 x4 (Curry.Module.Array.C_EntryOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_3_case__31(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_3_case__31 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_3_case__31")(x)



c__case_1_case__32 x1 x2 x3 x4@(Curry.Module.OracleArray.C_Array x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleArray.c_foldEntries(x2)(x3)(x6)(x1)(st))(st)
c__case_1_case__32 x1 x2 x3 (Curry.Module.OracleArray.C_ArrayOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_1_case__32(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_1_case__32 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_1_case__32")(x)



c__case_0_case__33 x1 x2 x3 x4@Curry.Module.Array.C_Empty st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_0_case__33 x1 x2 x3 x4@(Curry.Module.Array.C_Entry x5 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x3)(x5)(x1)(st))(Curry.Module.OracleArray.c_foldEntries(x2)(x3)(x6)(x8)(st))(x9)(st))(Curry.Module.OracleArray.c_foldEntries(x2)(x3)(x7)(x10)(st))(x11)(st))(st)
c__case_0_case__33 x1 x2 x3 (Curry.Module.Array.C_EntryOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleArray.c__case_0_case__33(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_0_case__33 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleArray._case_0_case__33")(x)


