{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Parser (module Curry.Module.Parser) where

import Curry.RunTimeSystem
import Curry.Module.Prelude



-- begin included



-- end included

type C_Parser t0 = (Curry.Module.Prelude.List t0) -> Curry.Module.Prelude.List t0

type C_ParserRep t0 t1 = t0 -> (Curry.Module.Prelude.List t1) -> Curry.Module.Prelude.List t1

op_60_124_62 :: (Curry t0) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0)) -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0)
op_60_124_62 x1 x2 st = Curry.RunTimeSystem.orF(Curry.Module.Prelude.pf(Curry.Module.Parser.c_'60'124'62'46_'35lambda2(x1)))(Curry.Module.Prelude.pf(Curry.Module.Parser.c_'60'124'62'46_'35lambda3(x2)))



c_'60'124'62'46_'35lambda2 :: (Curry t2) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t2) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t2)) -> (Curry.Module.Prelude.List t2) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t2
c_'60'124'62'46_'35lambda2 x1 x2 st = Curry.Module.Prelude.c_apply(x1)(x2)(st)



c_'60'124'62'46_'35lambda3 :: (Curry t2) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t2) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t2)) -> (Curry.Module.Prelude.List t2) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t2
c_'60'124'62'46_'35lambda3 x1 x2 st = Curry.Module.Prelude.c_apply(x1)(x2)(st)



op_60_124_124_62 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1))) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1))
op_60_124_124_62 x1 x2 st = Curry.Module.Prelude.pf(Curry.Module.Parser.c_'60'124'124'62'46_'35lambda4(x1)(x2))



c_'60'124'124'62'46_'35lambda4 :: (Curry t17,Curry t18) => (Curry.Module.Prelude.Prim (t17 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t18) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t18))) -> (Curry.Module.Prelude.Prim (t17 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t18) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t18))) -> t17 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t18) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t18)
c_'60'124'124'62'46_'35lambda4 x1 x2 x3 st = Curry.Module.Parser.op_60_124_62(Curry.Module.Prelude.c_apply(x1)(x3)(st))(Curry.Module.Prelude.c_apply(x2)(x3)(st))(st)



op_60_42_62 :: (Curry t0) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0)) -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0)
op_60_42_62 x1 x2 st = Curry.Module.Prelude.pf(Curry.Module.Parser.c_'60'42'62'46seq'4614(x1)(x2))



c_'60'42'62'46seq'4614 :: (Curry t23) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t23) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t23)) -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t23) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t23)) -> (Curry.Module.Prelude.List t23) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t23
c_'60'42'62'46seq'4614 x1 x2 x3 st = Curry.Module.Prelude.op_36_33_33(x2)(Curry.Module.Prelude.c_apply(x1)(x3)(st))(st)



op_62_62_62 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0)) -> t1 -> t1 -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
op_62_62_62 x1 x2 x3 x4 st = Curry.Module.Prelude.op_36_33_33(Curry.Module.Prelude.pf(Curry.Module.Parser.c_'62'62'62'46attach'4618(x3)(x2)))(Curry.Module.Prelude.c_apply(x1)(x4)(st))(st)



c_'62'62'62'46attach'4618 :: (Curry t36,Curry t0) => t36 -> t36 -> t0 -> Curry.RunTimeSystem.State -> t0
c_'62'62'62'46attach'4618 x1 x2 x3 st = Curry.Module.Prelude.c_cond(Curry.Module.Prelude.op_61_58_61(x2)(x1)(st))(x3)(st)



c_empty :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_empty x1 st = x1



c_terminal :: (Curry t0) => t0 -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_terminal x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.Prelude.c_cond(Curry.Module.Prelude.op_61_58_61(x1)(x3)(st))(x4)(st)
c_terminal x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Parser.c_terminal(x1)(x)(st))(i)(xs)(st)
c_terminal x1 x st = Curry.RunTimeSystem.patternFail("Parser.terminal")(x)



c_satisfy :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)) -> t0 -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_satisfy x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.Prelude.c_cond(Curry.Module.Prelude.op_38(Curry.Module.Prelude.op_61_58_61(Curry.Module.Prelude.c_apply(x1)(x4)(st))(Curry.Module.Prelude.C_True)(st))(Curry.Module.Prelude.op_61_58_61(x2)(x4)(st))(st))(x5)(st)
c_satisfy x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Parser.c_satisfy(x1)(x2)(x)(st))(i)(xs)(st)
c_satisfy x1 x2 x st = Curry.RunTimeSystem.patternFail("Parser.satisfy")(x)



c_star :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1))
c_star x1 st = Curry.RunTimeSystem.freeF(\ x2 -> Curry.RunTimeSystem.freeF(\ x3 -> Curry.Module.Parser.op_60_124_124_62(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Parser.op_62_62_62(Curry.Module.Parser.op_60_42_62(Curry.Module.Prelude.c_apply(x1)(x2)(st))(Curry.Module.Prelude.c_apply(Curry.Module.Parser.c_star(x1)(st))(x3)(st))(st))((Curry.Module.Prelude.:<)(x2)(x3))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Parser.op_62_62_62(Curry.Module.Prelude.pf(Curry.Module.Parser.c_empty))(Curry.Module.Prelude.List)))(st)))



c_some :: (Curry t0,Curry t1) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1))
c_some x1 st = Curry.RunTimeSystem.freeF(\ x2 -> Curry.RunTimeSystem.freeF(\ x3 -> Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Parser.op_62_62_62(Curry.Module.Parser.op_60_42_62(Curry.Module.Prelude.c_apply(x1)(x2)(st))(Curry.Module.Prelude.c_apply(Curry.Module.Parser.c_star(x1)(st))(x3)(st))(st))((Curry.Module.Prelude.:<)(x2)(x3)))))


