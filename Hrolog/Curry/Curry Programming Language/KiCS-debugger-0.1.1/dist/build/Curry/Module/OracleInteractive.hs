{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleInteractive (module Curry.Module.OracleInteractive) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.Interactive
import Curry.Module.Prelude
import Curry.Module.OraclePrelude



-- begin included



-- end included

c_interactiveSols :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_interactiveSols x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteractive.c__case_3(x2)(x1)(st))(st)



c_interactiveSols'46_'35lambda2 :: (Curry t5) => (Curry.Module.Prelude.List t5) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_interactiveSols'46_'35lambda2 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteractive.c__case_2(x2)(x3)(x1)(st))(st)



c_printIO :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_printIO x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.op_62_62_61(x2)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleInteractive.c_printIO'46_'35lambda4))))(x1)(st))(st)



c_printIO'46_'35lambda4 :: (Curry t25) => t25 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_printIO'46_'35lambda4 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_62_62(Curry.Module.OraclePrelude.c_putStr((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(x1)(st))(Curry.Module.Oracle.op_36_33_33(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleInteractive.c_printTerm))))(x2)(x3)(st))(x4)(st))(st)



c__case_2 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteractive.c__case_2_case__3(x1)(x2)(x3)(st))(st)



c__case_1 x2 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteractive.c__case_1_case__2(x1)(x2)(x4)(x5)(st))(st)



c__case_0 x2 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteractive.c__case_0_case__1(x1)(x2)(x5)(st))(st)



c__case_3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteractive.c__case_3_case__0(x1)(x2)(st))(st)



c_printTerm :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_printTerm x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Interactive.c_printTerm(x2)(st))))(st)



c__case_3_case__0 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_putStrLn((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))))))))))))))(x1)(st))(st)
c__case_3_case__0 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_62_62(Curry.Module.OracleInteractive.c_printTerm(x3)(x1)(st))(Curry.Module.OraclePrelude.op_62_62(Curry.Module.OraclePrelude.c_putStrLn((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('?'))(Curry.Module.Prelude.List))))))(x5)(st))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OraclePrelude.c_getLine(x6)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleInteractive.c_interactiveSols'46_'35lambda2(x4)))))(x7)(st))(x8)(st))(x9)(st))(st)
c__case_3_case__0 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteractive.c__case_3_case__0(x1)(x)(st))(i)(xs)(st)
c__case_3_case__0 x1 x st = Curry.RunTimeSystem.patternFail("OracleInteractive._case_3_case__0")(x)



c__case_0_case__1 x1 x2 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_return(Curry.Module.Prelude.T0)(x1)(st))(st)
c__case_0_case__1 x1 x2 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteractive.c_interactiveSols(x2)(x1)(st))(st)
c__case_0_case__1 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteractive.c__case_0_case__1(x1)(x2)(x)(st))(i)(xs)(st)
c__case_0_case__1 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleInteractive._case_0_case__1")(x)



c__case_1_case__2 x1 x2 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_return(Curry.Module.Prelude.T0)(x1)(st))(st)
c__case_1_case__2 x1 x2 x4 x5@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleInteractive.c__case_0(x2)(x4)(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.Prelude.C_Char('N'))(x1)(st))(x6)(st))(st)
c__case_1_case__2 x1 x2 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteractive.c__case_1_case__2(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_1_case__2 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleInteractive._case_1_case__2")(x)



c__case_2_case__3 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleInteractive.c__case_1(x2)(x4)(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.Prelude.C_Char('n'))(x1)(st))(x6)(st))(st)
c__case_2_case__3 x1 x2 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleInteractive.c_interactiveSols(x2)(x1)(st))(st)
c__case_2_case__3 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleInteractive.c__case_2_case__3(x1)(x2)(x)(st))(i)(xs)(st)
c__case_2_case__3 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleInteractive._case_2_case__3")(x)


