{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Oracle (module Curry.Module.Oracle) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.IOExts
import Curry.Module.Prelude
import Curry.Module.Unsafe
import Curry.Module.Meta



-- begin included



-- end included

c_partCons :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1)) -> Curry.Module.CEventOracle.C_Ref -> t0 -> Curry.RunTimeSystem.State -> t1
c_partCons x1 x2 x3 st = Curry.Module.CEventOracle.c_closeRef(x2)(Curry.Module.Prelude.c_apply(x1)(x3)(st))(st)



c_partFunc :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t1))) -> Curry.Module.CEventOracle.C_Ref -> t0 -> Curry.RunTimeSystem.State -> t1
c_partFunc x1 x2 x3 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x3)(st))(x2)(st)



c_partCall :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_partCall x1 x2 st = Curry.Module.CEventOracle.c_closeRef(x2)(x1)(st)



c_apply :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1))) -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t1
c_apply x1 x2 x3 st = Curry.Module.CEventOracle.c_apply(x1)(x2)(x3)(st)



op_36_33 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1))) -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t1
op_36_33 x1 x2 x3 st = Curry.Module.CEventOracle.op_36_33(x1)(x2)(x3)(st)



op_36_33_33 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1))) -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t1
op_36_33_33 x1 x2 x3 st = Curry.Module.CEventOracle.op_36_33_33(x1)(x2)(x3)(st)



op_36_35 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1))) -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t1
op_36_35 x1 x2 x3 st = Curry.Module.CEventOracle.op_36_35(x1)(x2)(x3)(st)



op_36_35_35 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1))) -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t1
op_36_35_35 x1 x2 x3 st = Curry.Module.CEventOracle.op_36_35_35(x1)(x2)(x3)(st)



c_headNormalFormIO :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1))))) -> t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1))
c_headNormalFormIO x1 x2 x3 st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))(Curry.Module.Prelude.List)))))))))))))))))(st)



c_prim_unsafePerformIO :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_prim_unsafePerformIO x1 x2 st = Curry.Module.CEventOracle.c_replace(x2)(Curry.Module.Unsafe.c_unsafePerformIO(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x2)(st))(Curry.Module.Prelude.T0)(st))(st))(st)



c_lambda_world :: (Curry t0) => (Curry.Module.Prelude.C_IO t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_lambda_world x1 x2 x3 st = Curry.Module.CEventOracle.c_collapse(x2)(x1)(st)



op_62_62_61 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1))
op_62_62_61 x1 x2 x3 st = Curry.Module.CEventOracle.c_collapse(x3)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_bind(x1)(x2)))))(st)



c_bind :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1))))) -> Curry.Module.Prelude.T0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1
c_bind x1 x2 x3 x4 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_62_62_61(Curry.Module.Oracle.c_apply(x1)(x3)(x4)(st))(Curry.Module.Prelude.pf(Curry.Module.Oracle.c_bind2(x2)(x3)(x5)))(st))(st)



c_bind2 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1))))) -> Curry.Module.Prelude.T0 -> Curry.Module.CEventOracle.C_Ref -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t1
c_bind2 x1 x2 x3 x4 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x3)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x1)(x4)(x3)(st))(x2)(x5)(st))(st)



c_catchFail :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))
c_catchFail x1 x2 x3 st = Curry.Module.CEventOracle.c_collapse(x3)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_catch(x1)(x2)))))(st)



c_catch :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))) -> Curry.Module.Prelude.T0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_catch x1 x2 x3 x4 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_catchFail(Curry.Module.Prelude.op_62_62_61(Curry.Module.Oracle.c_apply(x1)(x3)(x4)(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Prelude.pc(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_True)))(st))(st))(Curry.Module.Prelude.op_62_62_61(Curry.Module.Oracle.c_apply(x2)(x3)(x5)(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Prelude.pc(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_False)))(st))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.Oracle.c_catch'46_'35lambda2))(st))(st)



c_catch'46_'35lambda2 :: (Curry t162) => (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Bool t162) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t162
c_catch'46_'35lambda2 x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.Prelude.op_62_62(Curry.Module.Oracle.c_safeIOResult(Curry.Module.Prelude.c_return(x2)(st))(st))(Curry.Module.Prelude.c_return(x3)(st))(st)
c_catch'46_'35lambda2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Oracle.c_catch'46_'35lambda2(x)(st))(i)(xs)(st)
c_catch'46_'35lambda2 x st = Curry.RunTimeSystem.patternFail("Oracle.catch._#lambda2")(x)



c_compose :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1)) -> (Curry.Module.Prelude.Prim (t2 -> Curry.RunTimeSystem.State -> t0)) -> t2 -> Curry.RunTimeSystem.State -> t1
c_compose x1 x2 x3 st = Curry.Module.Prelude.c_apply(x1)(Curry.Module.Prelude.c_apply(x2)(x3)(st))(st)



c_oracle :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_oracle x1 st = Curry.Module.CEventOracle.c_initialize(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(x1)(st))(st)



c_oracleIO :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_oracleIO x1 st = Curry.Module.CEventOracle.c_initialize(Curry.Module.Prelude.pf(Curry.Module.Oracle.c_oracleIO'46_'35lambda3(x1)))(st)



c_oracleIO'46_'35lambda3 :: (Curry t195) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t195)))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t195
c_oracleIO'46_'35lambda3 x1 x2 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x2)(st))(x3)(st))(Curry.Module.Prelude.T0)(st))(st)



c_safeIOResult :: (Curry t0) => (Curry.Module.Prelude.C_IO t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_safeIOResult x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.IOExts.c_getAssoc((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List))))))(st))(Curry.Module.Prelude.pf(Curry.Module.Oracle.c_safeIOResult'46_'35lambda4(x1)))(st)



c_safeIOResult'46_'35lambda4 :: (Curry t120) => (Curry.Module.Prelude.C_IO t120) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t120
c_safeIOResult'46_'35lambda4 x1 x2@(Curry.Module.Prelude.C_Just x3) st = Curry.Module.Prelude.op_62_62_61(x1)(Curry.Module.Prelude.pf(Curry.Module.Oracle.c_safeIOResult'46_'35lambda4'46_'35lambda5(x3)))(st)
c_safeIOResult'46_'35lambda4 x1 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Oracle.c_safeIOResult'46_'35lambda4(x1)(x)(st))(i)(xs)(st)
c_safeIOResult'46_'35lambda4 x1 x st = Curry.RunTimeSystem.patternFail("Oracle.safeIOResult._#lambda4")(x)



c_safeIOResult'46_'35lambda4'46_'35lambda5 :: (Curry t120) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> t120 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t120
c_safeIOResult'46_'35lambda4'46_'35lambda5 x1 x2 st = let {x3 = Curry.Module.Prelude.c_show(x2)(st)} in Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_appendFile(x1)(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_show(Curry.Module.Prelude.c_length(x3)(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(x3))(st))(st))(Curry.Module.Prelude.c_return(x2)(st))(st)



c_unknown :: (Curry t0) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_unknown x1 st = Curry.Module.CEventOracle.c_unknown(x1)(st)


