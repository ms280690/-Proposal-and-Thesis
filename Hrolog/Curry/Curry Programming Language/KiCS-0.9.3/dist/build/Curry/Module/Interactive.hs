{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Interactive (module Curry.Module.Interactive) where

import Curry.RunTimeSystem
import Curry.Module.Prelude



-- begin included



printTerm :: (Show t0,Curry t0) => t0 -> Result (C_IO T0)
printTerm x _ = C_IO (\ _ -> do print x 
                                Prelude.return (IOVal T0))


-- end included

c_interactiveSols :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_interactiveSols x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.c_putStrLn((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))))))))))))))(st)
c_interactiveSols x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.Prelude.op_62_62(Curry.Module.Interactive.c_printTerm(x2)(st))(Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_putStrLn((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('?'))(Curry.Module.Prelude.List))))))(st))(Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_getLine(st))(Curry.Module.Prelude.pf(Curry.Module.Interactive.c_interactiveSols'46_'35lambda2(x3)))(st))(st))(st)
c_interactiveSols (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Interactive.c_interactiveSols(x)(st))(i)(xs)(st)
c_interactiveSols x st = Curry.RunTimeSystem.patternFail("Interactive.interactiveSols")(x)



c_interactiveSols'46_'35lambda2 :: (Curry t5) => (Curry.Module.Prelude.List t5) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_interactiveSols'46_'35lambda2 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.Interactive.c_interactiveSols'46_'35lambda2_case_1(x1)(x3)(Curry.Module.Prelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('n'))(st))(st)
c_interactiveSols'46_'35lambda2 x1 x2@Curry.Module.Prelude.List st = Curry.Module.Interactive.c_interactiveSols(x1)(st)
c_interactiveSols'46_'35lambda2 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Interactive.c_interactiveSols'46_'35lambda2(x1)(x)(st))(i)(xs)(st)
c_interactiveSols'46_'35lambda2 x1 x st = Curry.RunTimeSystem.patternFail("Interactive.interactiveSols._#lambda2")(x)



c_printIO :: (Curry t0) => (Curry.Module.Prelude.C_IO t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_printIO x1 st = Curry.Module.Prelude.op_62_62_61(x1)(Curry.Module.Prelude.pf(Curry.Module.Interactive.c_printIO'46_'35lambda4))(st)



c_printIO'46_'35lambda4 :: (Curry t25) => t25 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_printIO'46_'35lambda4 x1 st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_putStr((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(st))(Curry.Module.Prelude.op_36_33_33(Curry.Module.Prelude.pf(Curry.Module.Interactive.c_printTerm))(x1)(st))(st)



c_interactiveSols'46_'35lambda2_case_1 x1 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.T0)(st)
c_interactiveSols'46_'35lambda2_case_1 x1 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.Interactive.c_interactiveSols'46_'35lambda2_case_0(x1)(x3)(Curry.Module.Prelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('N'))(st))(st)
c_interactiveSols'46_'35lambda2_case_1 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Interactive.c_interactiveSols'46_'35lambda2_case_1(x1)(x3)(x)(st))(i)(xs)(st)
c_interactiveSols'46_'35lambda2_case_1 x1 x3 x st = Curry.RunTimeSystem.patternFail("Interactive.interactiveSols._#lambda2_case_1")(x)



c_interactiveSols'46_'35lambda2_case_0 x1 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.T0)(st)
c_interactiveSols'46_'35lambda2_case_0 x1 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.Interactive.c_interactiveSols(x1)(st)
c_interactiveSols'46_'35lambda2_case_0 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Interactive.c_interactiveSols'46_'35lambda2_case_0(x1)(x3)(x)(st))(i)(xs)(st)
c_interactiveSols'46_'35lambda2_case_0 x1 x3 x st = Curry.RunTimeSystem.patternFail("Interactive.interactiveSols._#lambda2_case_0")(x)



c_printTerm :: (Curry t0) => t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_printTerm x1 st = Curry.Module.Interactive.printTerm(x1)(st)


