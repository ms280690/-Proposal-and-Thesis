{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Make (module Curry.Module.Make) where

import Curry.RunTimeSystem
import Curry.Module.Directory
import Curry.Module.Distribution
import Curry.Module.FileGoodies
import Curry.Module.FiniteMap
import Curry.Module.FlatCurry
import Curry.Module.IOExts
import Curry.Module.Prelude
import Curry.Module.Sort
import Curry.Module.System
import Curry.Module.Time
import Curry.Module.FlatCurryGoodies



-- begin included



-- end included

type C_ModuleName = Curry.Module.Prelude.List Curry.Module.Prelude.C_Char

type C_Path = Curry.Module.Prelude.List Curry.Module.Prelude.C_Char

type C_FileName = Curry.Module.Prelude.List Curry.Module.Prelude.C_Char

type C_TestAct t0 = (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t0)

type C_ProgAct t0 = (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List t0) -> Curry.Module.FlatCurry.C_Prog -> Curry.Module.Prelude.C_IO t0

type C_Done t0 = Curry.Module.IOExts.C_IORef (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) t0)

type C_Getter t0 = Curry.Module.Make.C_Parameter -> t0

type C_Setter t0 = t0 -> Curry.Module.Make.C_Parameter -> Curry.Module.Make.C_Parameter

data C_Parameter = C_Parameter Curry.Module.Prelude.C_Bool Curry.Module.Prelude.C_Bool (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
  | C_ParameterFail Curry.RunTimeSystem.C_Exceptions
  | C_ParameterOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.Make.C_Parameter)

instance BaseCurry Curry.Module.Make.C_Parameter where
  nf f (Curry.Module.Make.C_Parameter x1 x2 x3 x4 x5) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> f(Curry.Module.Make.C_Parameter(v1)(v2)(v3)(v4)(v5))(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Make.C_Parameter x1 x2 x3 x4 x5) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> f(Curry.Module.Make.C_Parameter(v1)(v2)(v3)(v4)(v5))(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Make.C_ParameterOr(Curry.RunTimeSystem.mkRef(r)(5)(i))([Curry.Module.Make.C_Parameter(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(5)

  failed  = Curry.Module.Make.C_ParameterFail

  branching  = Curry.Module.Make.C_ParameterOr

  consKind (Curry.Module.Make.C_ParameterOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Make.C_ParameterFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Make.C_ParameterFail x) = x

  orRef (Curry.Module.Make.C_ParameterOr x _) = x

  branches (Curry.Module.Make.C_ParameterOr _ x) = x





instance Curry Curry.Module.Make.C_Parameter where
  strEq (Curry.Module.Make.C_Parameter x1 x2 x3 x4 x5) (Curry.Module.Make.C_Parameter y1 y2 y3 y4 y5) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Make.C_Parameter x1 x2 x3 x4 x5) (Curry.Module.Make.C_Parameter y1 y2 y3 y4 y5) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.genEq(x5)(y5)(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Make.C_Parameter x1 x2 x3 x4 x5) st = Curry.Module.Make.C_Parameter(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))

  foldCurry f c (Curry.Module.Make.C_Parameter x1 x2 x3 x4 x5) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(c)(st))(st))(st))(st))(st)

  typeName _ = "Parameter"

  showQ d (Curry.Module.Make.C_Parameter x1 x2 x3 x4 x5) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Make.Parameter "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x5))))))))))


  showQ _ (Curry.Module.Make.C_ParameterOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.Make.C_Parameter where
  showsPrec d (Curry.Module.Make.C_Parameter x1 x2 x3 x4 x5) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Parameter "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x5))))))))))


  showsPrec _ (Curry.Module.Make.C_ParameterOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Read Curry.Module.Make.C_Parameter where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Make.C_Parameter(x1)(x2)(x3)(x4)(x5))(r5) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Make")("Parameter")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3), ((,) x5 r5) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r4)])(r)





c_defaults :: Curry.RunTimeSystem.State -> Curry.Module.Make.C_Parameter
c_defaults st = Curry.Module.Make.C_Parameter(Curry.Module.Prelude.C_False)(Curry.Module.Prelude.C_False)(Curry.Module.Prelude.C_Nothing)(Curry.Module.Prelude.C_Nothing)(Curry.Module.Prelude.List)



c_quiet :: Curry.Module.Make.C_Parameter -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_quiet x1@(Curry.Module.Make.C_Parameter x2 x3 x4 x5 x6) st = x2
c_quiet (Curry.Module.Make.C_ParameterOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_quiet(x)(st))(i)(xs)(st)
c_quiet x st = Curry.RunTimeSystem.patternFail("Make.quiet")(x)



c_setQuiet :: Curry.Module.Prelude.C_Bool -> Curry.Module.Make.C_Parameter -> Curry.RunTimeSystem.State -> Curry.Module.Make.C_Parameter
c_setQuiet x1 x2@(Curry.Module.Make.C_Parameter x3 x4 x5 x6 x7) st = Curry.Module.Make.C_Parameter(x1)(x4)(x5)(x6)(x7)
c_setQuiet x1 (Curry.Module.Make.C_ParameterOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_setQuiet(x1)(x)(st))(i)(xs)(st)
c_setQuiet x1 x st = Curry.RunTimeSystem.patternFail("Make.setQuiet")(x)



c_force :: Curry.Module.Make.C_Parameter -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_force x1@(Curry.Module.Make.C_Parameter x2 x3 x4 x5 x6) st = x3
c_force (Curry.Module.Make.C_ParameterOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_force(x)(st))(i)(xs)(st)
c_force x st = Curry.RunTimeSystem.patternFail("Make.force")(x)



c_setForce :: Curry.Module.Prelude.C_Bool -> Curry.Module.Make.C_Parameter -> Curry.RunTimeSystem.State -> Curry.Module.Make.C_Parameter
c_setForce x1 x2@(Curry.Module.Make.C_Parameter x3 x4 x5 x6 x7) st = Curry.Module.Make.C_Parameter(x3)(x1)(x5)(x6)(x7)
c_setForce x1 (Curry.Module.Make.C_ParameterOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_setForce(x1)(x)(st))(i)(xs)(st)
c_setForce x1 x st = Curry.RunTimeSystem.patternFail("Make.setForce")(x)



c_output :: Curry.Module.Make.C_Parameter -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_output x1@(Curry.Module.Make.C_Parameter x2 x3 x4 x5 x6) st = x4
c_output (Curry.Module.Make.C_ParameterOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_output(x)(st))(i)(xs)(st)
c_output x st = Curry.RunTimeSystem.patternFail("Make.output")(x)



c_setOutput :: (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.Make.C_Parameter -> Curry.RunTimeSystem.State -> Curry.Module.Make.C_Parameter
c_setOutput x1 x2@(Curry.Module.Make.C_Parameter x3 x4 x5 x6 x7) st = Curry.Module.Make.C_Parameter(x3)(x4)(x1)(x6)(x7)
c_setOutput x1 (Curry.Module.Make.C_ParameterOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_setOutput(x1)(x)(st))(i)(xs)(st)
c_setOutput x1 x st = Curry.RunTimeSystem.patternFail("Make.setOutput")(x)



c_main :: Curry.Module.Make.C_Parameter -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_main x1@(Curry.Module.Make.C_Parameter x2 x3 x4 x5 x6) st = x5
c_main (Curry.Module.Make.C_ParameterOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_main(x)(st))(i)(xs)(st)
c_main x st = Curry.RunTimeSystem.patternFail("Make.main")(x)



c_setMain :: (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.Make.C_Parameter -> Curry.RunTimeSystem.State -> Curry.Module.Make.C_Parameter
c_setMain x1 x2@(Curry.Module.Make.C_Parameter x3 x4 x5 x6 x7) st = Curry.Module.Make.C_Parameter(x3)(x4)(x5)(x1)(x7)
c_setMain x1 (Curry.Module.Make.C_ParameterOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_setMain(x1)(x)(st))(i)(xs)(st)
c_setMain x1 x st = Curry.RunTimeSystem.patternFail("Make.setMain")(x)



c_modulename :: Curry.Module.Make.C_Parameter -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_modulename x1@(Curry.Module.Make.C_Parameter x2 x3 x4 x5 x6) st = x6
c_modulename (Curry.Module.Make.C_ParameterOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_modulename(x)(st))(i)(xs)(st)
c_modulename x st = Curry.RunTimeSystem.patternFail("Make.modulename")(x)



c_setModulename :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Make.C_Parameter -> Curry.RunTimeSystem.State -> Curry.Module.Make.C_Parameter
c_setModulename x1 x2@(Curry.Module.Make.C_Parameter x3 x4 x5 x6 x7) st = Curry.Module.Make.C_Parameter(x3)(x4)(x5)(x6)(x1)
c_setModulename x1 (Curry.Module.Make.C_ParameterOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_setModulename(x1)(x)(st))(i)(xs)(st)
c_setModulename x1 x st = Curry.RunTimeSystem.patternFail("Make.setModulename")(x)



c_parseArgs :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Make.C_Parameter
c_parseArgs st = Curry.Module.Prelude.op_62_62_61(Curry.Module.System.c_getArgs(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Prelude.pf(Curry.Module.Make.c_parseArgs'46parse'4650(Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('q'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('q'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))(st))(Curry.Module.Make.c_defaults(st))))(st))(st)



c_parseArgs'46parse'4650 :: Curry.Module.Make.C_Parameter -> Curry.Module.Make.C_Parameter -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Make.C_Parameter
c_parseArgs'46parse'4650 x1 x2 x3@Curry.Module.Prelude.List st = x1
c_parseArgs'46parse'4650 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.Make.c_parseArgs'46parse'4650_case_15(x1)(x2)(x4)(x5)(Curry.Module.Prelude.op_61_61(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('q'))(Curry.Module.Prelude.List)))(st))(st)
c_parseArgs'46parse'4650 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_parseArgs'46parse'4650(x1)(x2)(x)(st))(i)(xs)(st)
c_parseArgs'46parse'4650 x1 x2 x st = Curry.RunTimeSystem.patternFail("Make.parseArgs.parse.50")(x)



c_make :: (Curry t0) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t0)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))))
c_make st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))))(Curry.Module.Make.c_makeWithFrontendParams(Curry.Module.Distribution.c_defaultParams(st)))



c_makeWithFrontendParams :: (Curry t0) => Curry.Module.Distribution.C_FrontendParams -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t0)))) -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_makeWithFrontendParams x1 x2 x3 x4 x5 st = let {x6 = Curry.Module.FileGoodies.c_splitDirectoryBaseName(x3)(st)} in Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_maybe(Curry.Module.Distribution.c_getLoadPath(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Distribution.c_fullPath(x1)(st))(st))(Curry.Module.Prelude.pf(Curry.Module.Make.c_makeWithFrontendParams'46_'35lambda4(x5)(Curry.Module.Make.c_makeWithFrontendParams'46_'35selFP3'35dir(x6)(st))(x1)(Curry.Module.Make.c_makeWithFrontendParams'46_'35selFP4'35modu(x6)(st))(x2)(x4)))(st)



c_makeWithFrontendParams'46_'35selFP3'35dir :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_makeWithFrontendParams'46_'35selFP3'35dir x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_makeWithFrontendParams'46_'35selFP3'35dir (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_makeWithFrontendParams'46_'35selFP3'35dir(x)(st))(i)(xs)(st)
c_makeWithFrontendParams'46_'35selFP3'35dir x st = Curry.RunTimeSystem.patternFail("Make.makeWithFrontendParams._#selFP3#dir")(x)



c_makeWithFrontendParams'46_'35selFP4'35modu :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_makeWithFrontendParams'46_'35selFP4'35modu x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_makeWithFrontendParams'46_'35selFP4'35modu (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_makeWithFrontendParams'46_'35selFP4'35modu(x)(st))(i)(xs)(st)
c_makeWithFrontendParams'46_'35selFP4'35modu x st = Curry.RunTimeSystem.patternFail("Make.makeWithFrontendParams._#selFP4#modu")(x)



c_makeWithFrontendParams'46_'35lambda4 :: (Curry t209) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t209) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t209)))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Distribution.C_FrontendParams -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t209)))) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_makeWithFrontendParams'46_'35lambda4 x1 x2 x3 x4 x5 x6 x7 st = let {x8 = Curry.Module.Make.c_makeWithFrontendParams'46_'35lambda4_case_6(x2)(x7)(Curry.Module.Prelude.op_61_61(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List))(st))(st)} in Curry.Module.Prelude.op_62_62_61(Curry.Module.FileGoodies.c_lookupFileInPath(x4)((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))(Curry.Module.Prelude.List)))(x8)(st))(Curry.Module.Prelude.pf(Curry.Module.Make.c_makeWithFrontendParams'46_'35lambda4'46_'35lambda5(x1)(x8)(x4)(Curry.Module.Distribution.c_setFullPath(x8)(x3)(st))(x5)(x6)))(st)



c_makeWithFrontendParams'46_'35lambda4'46_'35lambda5 :: (Curry t209) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t209) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t209)))) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Distribution.C_FrontendParams -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t209)))) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_makeWithFrontendParams'46_'35lambda4'46_'35lambda5 x1 x2 x3 x4 x5 x6 x7 st = Curry.Module.Prelude.op_62_62(Curry.Module.Make.c_unless(Curry.Module.Prelude.op_61_61(x7)(Curry.Module.Prelude.C_Nothing)(st))(Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Make.c_unless(x5)))(Curry.Module.Prelude.c_putStrLn((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))))))))(st))(st))(Curry.Module.Prelude.op_62_62(Curry.Module.Distribution.c_callFrontendWithParams(Curry.Module.Distribution.C_FCY)(Curry.Module.Distribution.c_setQuiet(Curry.Module.Prelude.C_True)(x4)(st))(x3)(st))(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Make.c_unless(x5)))(Curry.Module.Prelude.c_putStrLn((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))(Curry.Module.Prelude.List)))))))))))(st))(st))(st))(st))(st))(Curry.Module.Prelude.op_62_62_61(Curry.Module.IOExts.c_newIORef(Curry.Module.FiniteMap.c_emptyFM(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Make.c_makeWithFrontendParams'46_'35lambda4'46_'35lambda5'46_'35lambda6))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.Make.c_makeWithFrontendParams'46_'35lambda4'46_'35lambda5'46_'35lambda7(x1)(x2)(x3)(x6)))(st))(st)



c_makeWithFrontendParams'46_'35lambda4'46_'35lambda5'46_'35lambda6 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_makeWithFrontendParams'46_'35lambda4'46_'35lambda5'46_'35lambda6 x1 x2 st = Curry.Module.Prelude.c_not(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Sort.c_leqString(st))(x1)(st))(x2)(st))(st)



c_makeWithFrontendParams'46_'35lambda4'46_'35lambda5'46_'35lambda7 :: (Curry t209) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t209) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t209)))) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t209)))) -> (Curry.Module.IOExts.C_IORef (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) t209)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_makeWithFrontendParams'46_'35lambda4'46_'35lambda5'46_'35lambda7 x1 x2 x3 x4 x5 st = Curry.Module.Prelude.op_62_62(Curry.Module.Make.c_workUpDependence(x2)(x5)(x4)(x1)(x3)(st))(Curry.Module.Prelude.c_return(Curry.Module.Prelude.T0)(st))(st)



c_workUpDependence :: (Curry t0) => (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.IOExts.C_IORef (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) t0)) -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t0)))) -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0)))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_workUpDependence x1 x2 x3 x4 x5 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.IOExts.c_readIORef(x2)(st))(Curry.Module.Prelude.pf(Curry.Module.Make.c_workUpDependence'46_'35lambda8(x4)(x2)(x5)(x1)(x3)))(st)



c_workUpDependence'46_'35lambda8 :: (Curry t141) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t141) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t141)))) -> (Curry.Module.IOExts.C_IORef (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) t141)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t141)))) -> (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) t141) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t141
c_workUpDependence'46_'35lambda8 x1 x2 x3 x4 x5 x6 st = Curry.Module.Prelude.c_maybe(Curry.Module.Make.c_process(x4)(x2)(x5)(x1)(x3)(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.FiniteMap.c_lookupFM(x6)(x3)(st))(st)



c_process :: (Curry t0) => (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.IOExts.C_IORef (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) t0)) -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t0)))) -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0)))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_process x1 x2 x3 x4 x5 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.FileGoodies.c_getFileInPath(Curry.Module.Prelude.op_43_43(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.Make.c_process'46_'35lambda9(x4)(x2)(x5)(x1)(x3)))(st)



c_process'46_'35lambda9 :: (Curry t167) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t167) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t167)))) -> (Curry.Module.IOExts.C_IORef (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) t167)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t167)))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t167
c_process'46_'35lambda9 x1 x2 x3 x4 x5 x6 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.op_62_62_61(Curry.Module.Make.c_fastReadImports(x6)(st))(Curry.Module.Prelude.c_mapIO(Curry.Module.Prelude.pf(Curry.Module.Make.c_workUpDependence(x4)(x2)(x5)(x1)))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.Make.c_process'46_'35lambda9'46_'35lambda10(x1)(x2)(x6)(x3)(x5)))(st)



c_process'46_'35lambda9'46_'35lambda10 :: (Curry t167) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t167) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t167)))) -> (Curry.Module.IOExts.C_IORef (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) t167)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t167)))) -> (Curry.Module.Prelude.List t167) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t167
c_process'46_'35lambda9'46_'35lambda10 x1 x2 x3 x4 x5 x6 st = let {x7 = Curry.Module.Prelude.op_43_43(Curry.Module.FileGoodies.c_dirName(x3)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.Prelude.List))(st)} in Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x5)(x7)(st))(x4)(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.op_62_62_61(Curry.Module.FlatCurry.c_readFlatCurryFile(x3)(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x7)(st))(x6)(st))(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))))(st))(Curry.Module.Prelude.pf(Curry.Module.Make.c_process'46_'35lambda9'46_'35lambda10'46_'35lambda11(x2)(x4)))(st)



c_process'46_'35lambda9'46_'35lambda10'46_'35lambda11 :: (Curry t167) => (Curry.Module.IOExts.C_IORef (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) t167)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> t167 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t167
c_process'46_'35lambda9'46_'35lambda10'46_'35lambda11 x1 x2 x3 st = Curry.Module.Prelude.op_62_62(Curry.Module.Make.c_updateIORef(x1)(Curry.Module.Prelude.pf(Curry.Module.Make.c_process'46_'35lambda9'46_'35lambda10'46_'35lambda11'46_'35lambda12(x2)(x3)))(st))(Curry.Module.Prelude.c_return(x3)(st))(st)



c_process'46_'35lambda9'46_'35lambda10'46_'35lambda11'46_'35lambda12 :: (Curry t167) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> t167 -> (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) t167) -> Curry.RunTimeSystem.State -> Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) t167
c_process'46_'35lambda9'46_'35lambda10'46_'35lambda11'46_'35lambda12 x1 x2 x3 st = Curry.Module.FiniteMap.c_addToFM(x3)(x1)(x2)(st)



c_obsolete :: (Curry t0) => Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t0)
c_obsolete x1 x2 x3 x4 x5 x6 st = let {x7 = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x2)(x5)(st))(x6)(st)} in Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesFileExist(x7)(st))(Curry.Module.Prelude.pf(Curry.Module.Make.c_obsolete'46_'35lambda15(x4)(x7)(Curry.Module.Prelude.c_map(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_43_43(x5)))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_36))(x6)))(st))(x3)(st))(x1)))(st)



c_obsolete'46isNewerThan'4686 :: Curry.Module.Time.C_ClockTime -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Bool
c_obsolete'46isNewerThan'4686 x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_doesFileExist(x2)(st))(Curry.Module.Prelude.pf(Curry.Module.Make.c_obsolete'46isNewerThan'4686'46_'35lambda13(x2)(x1)))(st)



c_obsolete'46isNewerThan'4686'46_'35lambda13 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Time.C_ClockTime -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Bool
c_obsolete'46isNewerThan'4686'46_'35lambda13 x1 x2 x3 st = Curry.Module.Make.c_obsolete'46isNewerThan'4686'46_'35lambda13_case_5(x1)(x2)(x3)(Curry.Module.Prelude.c_not(x3)(st))(st)



c_obsolete'46isNewerThan'4686'46_'35lambda13'46_'35lambda14 :: Curry.Module.Time.C_ClockTime -> Curry.Module.Time.C_ClockTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.C_Bool
c_obsolete'46isNewerThan'4686'46_'35lambda13'46_'35lambda14 x1 x2 st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.op_47_61(Curry.Module.Time.c_compareClockTime(x1)(x2)(st))(Curry.Module.Prelude.C_GT)(st))(st)



c_obsolete'46_'35lambda15 :: (Curry t287) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t287)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.Prelude.C_Bool -> Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t287)
c_obsolete'46_'35lambda15 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_getModificationTime(x2)(st))(Curry.Module.Prelude.pf(Curry.Module.Make.c_obsolete'46_'35lambda15'46_'35lambda16(x1)(x2)(x3)(x4)))(st)
c_obsolete'46_'35lambda15 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Make.c_unless(x4)))(Curry.Module.Prelude.c_putStrLn(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))(x2)(st))(st))(st))(Curry.Module.Prelude.c_return(Curry.Module.Prelude.C_Nothing)(st))(st)
c_obsolete'46_'35lambda15 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_obsolete'46_'35lambda15(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_obsolete'46_'35lambda15 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("Make.obsolete._#lambda15")(x)



c_obsolete'46_'35lambda15'46_'35lambda16 :: (Curry t287) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t287)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.Prelude.C_Bool -> Curry.Module.Time.C_ClockTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t287)
c_obsolete'46_'35lambda15'46_'35lambda16 x1 x2 x3 x4 x5 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_mapIO(Curry.Module.Prelude.pf(Curry.Module.Make.c_obsolete'46isNewerThan'4686(x5)))(st))(x3)(st))(Curry.Module.Prelude.pf(Curry.Module.Make.c_obsolete'46_'35lambda15'46_'35lambda16'46_'35lambda17(x1)(x2)(x3)(x4)))(st)



c_obsolete'46_'35lambda15'46_'35lambda16'46_'35lambda17 :: (Curry t287) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t287)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Bool) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe t287)
c_obsolete'46_'35lambda15'46_'35lambda16'46_'35lambda17 x1 x2 x3 x4 x5 st = Curry.Module.Make.c_obsolete'46_'35lambda15'46_'35lambda16'46_'35lambda17_case_4(x1)(x2)(x3)(x4)(x5)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_or(st))(x5)(st))(st)



c_fastReadImports :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_fastReadImports x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_readFile(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.Make.c_fastReadImports'46_'35lambda18))(st)



c_fastReadImports'46_'35lambda18 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_fastReadImports'46_'35lambda18 x1 st = Curry.Module.Prelude.c_return(Curry.Module.Make.c_strings(Curry.Module.Prelude.c_takeWhile(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_47_61))(Curry.Module.Prelude.C_Char(']'))))(Curry.Module.Prelude.c_dropWhile(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_47_61))(Curry.Module.Prelude.C_Char('['))))(x1)(st))(st))(st))(st)



c_strings :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_strings x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_strings x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.Make.c_strings_case_3(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\"'))(st))(st)
c_strings (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_strings(x)(st))(i)(xs)(st)
c_strings x st = Curry.RunTimeSystem.patternFail("Make.strings")(x)



c_updateIORef :: (Curry t0) => (Curry.Module.IOExts.C_IORef t0) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_updateIORef x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.IOExts.c_readIORef(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.Make.c_updateIORef'46_'35lambda20(x2)(x1)))(st)



c_updateIORef'46_'35lambda20 :: (Curry t127) => (Curry.Module.Prelude.Prim (t127 -> Curry.RunTimeSystem.State -> t127)) -> (Curry.Module.IOExts.C_IORef t127) -> t127 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_updateIORef'46_'35lambda20 x1 x2 x3 st = Curry.Module.IOExts.c_writeIORef(x2)(Curry.Module.Prelude.c_apply(x1)(x3)(st))(st)



c_unless :: Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_unless x1@Curry.Module.Prelude.C_True x2 st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.T0)(st)
c_unless x1@Curry.Module.Prelude.C_False x2 st = x2
c_unless (Curry.Module.Prelude.C_BoolOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_unless(x)(x2)(st))(i)(xs)(st)
c_unless x x2 st = Curry.RunTimeSystem.patternFail("Make.unless")(x)



c_strings_case_3 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Make.c_strings_case_2(x3)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_break(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_61_61))(Curry.Module.Prelude.C_Char('\"'))))(st))(x3)(st))(st)
c_strings_case_3 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.Make.c_strings_case_0(x3)(Curry.Module.Prelude.c_otherwise(st))(st)
c_strings_case_3 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_strings_case_3(x2)(x3)(x)(st))(i)(xs)(st)
c_strings_case_3 x2 x3 x st = Curry.RunTimeSystem.patternFail("Make.strings_case_3")(x)



c_strings_case_0 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Make.c_strings(x3)(st)
c_strings_case_0 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_strings_case_0(x3)(x)(st))(i)(xs)(st)
c_strings_case_0 x3 x st = Curry.RunTimeSystem.patternFail("Make.strings_case_0")(x)



c_strings_case_2 x3 (Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.Make.c_strings_case_1(x4)(x5)(st)
c_strings_case_2 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_strings_case_2(x3)(x)(st))(i)(xs)(st)
c_strings_case_2 x3 x st = Curry.RunTimeSystem.patternFail("Make.strings_case_2")(x)



c_strings_case_1 x4 x5@((Curry.Module.Prelude.:<) x6 x7) st = (Curry.Module.Prelude.:<)(x4)(Curry.Module.Make.c_strings(x7)(st))
c_strings_case_1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_strings_case_1(x4)(x)(st))(i)(xs)(st)
c_strings_case_1 x4 x st = Curry.RunTimeSystem.patternFail("Make.strings_case_1")(x)



c_obsolete'46_'35lambda15'46_'35lambda16'46_'35lambda17_case_4 x1 x2 x3 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Make.c_unless(x4)))(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_putStrLn))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))(x2)(st))(st))(st))(Curry.Module.Prelude.c_return(Curry.Module.Prelude.C_Nothing)(st))(st)
c_obsolete'46_'35lambda15'46_'35lambda16'46_'35lambda17_case_4 x1 x2 x3 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Make.c_unless(x4)))(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_putStrLn))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))(x2)(st))(st))(st))(Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_apply(x1)(x3)(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Prelude.pc(Curry.Module.Prelude.C_Just))(st))(st))(st)
c_obsolete'46_'35lambda15'46_'35lambda16'46_'35lambda17_case_4 x1 x2 x3 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_obsolete'46_'35lambda15'46_'35lambda16'46_'35lambda17_case_4(x1)(x2)(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c_obsolete'46_'35lambda15'46_'35lambda16'46_'35lambda17_case_4 x1 x2 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("Make.obsolete._#lambda15._#lambda16._#lambda17_case_4")(x)



c_obsolete'46isNewerThan'4686'46_'35lambda13_case_5 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.C_False)(st)
c_obsolete'46isNewerThan'4686'46_'35lambda13_case_5 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Directory.c_getModificationTime(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.Make.c_obsolete'46isNewerThan'4686'46_'35lambda13'46_'35lambda14(x2)))(st)
c_obsolete'46isNewerThan'4686'46_'35lambda13_case_5 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_obsolete'46isNewerThan'4686'46_'35lambda13_case_5(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_obsolete'46isNewerThan'4686'46_'35lambda13_case_5 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("Make.obsolete.isNewerThan.86._#lambda13_case_5")(x)



c_makeWithFrontendParams'46_'35lambda4_case_6 x2 x7 x8@Curry.Module.Prelude.C_True st = x7
c_makeWithFrontendParams'46_'35lambda4_case_6 x2 x7 x8@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(x2)(x7)
c_makeWithFrontendParams'46_'35lambda4_case_6 x2 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_makeWithFrontendParams'46_'35lambda4_case_6(x2)(x7)(x)(st))(i)(xs)(st)
c_makeWithFrontendParams'46_'35lambda4_case_6 x2 x7 x st = Curry.RunTimeSystem.patternFail("Make.makeWithFrontendParams._#lambda4_case_6")(x)



c_parseArgs'46parse'4650_case_15 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.Make.c_parseArgs'46parse'4650(x1)(Curry.Module.Make.c_setQuiet(Curry.Module.Prelude.C_True)(x2)(st))(x5)(st)
c_parseArgs'46parse'4650_case_15 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.Make.c_parseArgs'46parse'4650_case_14(x1)(x2)(x4)(x5)(Curry.Module.Prelude.op_61_61(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('q'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))))))(st))(st)
c_parseArgs'46parse'4650_case_15 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_parseArgs'46parse'4650_case_15(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c_parseArgs'46parse'4650_case_15 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("Make.parseArgs.parse.50_case_15")(x)



c_parseArgs'46parse'4650_case_14 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.Make.c_parseArgs'46parse'4650(x1)(Curry.Module.Make.c_setQuiet(Curry.Module.Prelude.C_True)(x2)(st))(x5)(st)
c_parseArgs'46parse'4650_case_14 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.Make.c_parseArgs'46parse'4650_case_13(x1)(x2)(x4)(x5)(Curry.Module.Prelude.op_61_61(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))(Curry.Module.Prelude.List)))(st))(st)
c_parseArgs'46parse'4650_case_14 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_parseArgs'46parse'4650_case_14(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c_parseArgs'46parse'4650_case_14 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("Make.parseArgs.parse.50_case_14")(x)



c_parseArgs'46parse'4650_case_13 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.Make.c_parseArgs'46parse'4650(x1)(Curry.Module.Make.c_setForce(Curry.Module.Prelude.C_True)(x2)(st))(x5)(st)
c_parseArgs'46parse'4650_case_13 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.Make.c_parseArgs'46parse'4650_case_12(x1)(x2)(x4)(x5)(Curry.Module.Prelude.op_61_61(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(st))(st)
c_parseArgs'46parse'4650_case_13 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_parseArgs'46parse'4650_case_13(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c_parseArgs'46parse'4650_case_13 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("Make.parseArgs.parse.50_case_13")(x)



c_parseArgs'46parse'4650_case_12 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.Make.c_parseArgs'46parse'4650(x1)(Curry.Module.Make.c_setForce(Curry.Module.Prelude.C_True)(x2)(st))(x5)(st)
c_parseArgs'46parse'4650_case_12 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.Make.c_parseArgs'46parse'4650_case_11(x1)(x2)(x4)(x5)(Curry.Module.Prelude.op_61_61(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))(Curry.Module.Prelude.List)))(st))(st)
c_parseArgs'46parse'4650_case_12 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_parseArgs'46parse'4650_case_12(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c_parseArgs'46parse'4650_case_12 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("Make.parseArgs.parse.50_case_12")(x)



c_parseArgs'46parse'4650_case_11 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.Make.c_parseArgs'46parse'4650_case_10(x1)(x2)(x5)(st)
c_parseArgs'46parse'4650_case_11 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.Make.c_parseArgs'46parse'4650_case_9(x1)(x2)(x4)(x5)(Curry.Module.Prelude.op_61_61(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))(Curry.Module.Prelude.List)))(st))(st)
c_parseArgs'46parse'4650_case_11 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_parseArgs'46parse'4650_case_11(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c_parseArgs'46parse'4650_case_11 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("Make.parseArgs.parse.50_case_11")(x)



c_parseArgs'46parse'4650_case_9 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.Make.c_parseArgs'46parse'4650_case_8(x1)(x2)(x5)(st)
c_parseArgs'46parse'4650_case_9 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.Make.c_parseArgs'46parse'4650_case_7(x2)(x4)(x5)(Curry.Module.Prelude.c_null(x5)(st))(st)
c_parseArgs'46parse'4650_case_9 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_parseArgs'46parse'4650_case_9(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c_parseArgs'46parse'4650_case_9 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("Make.parseArgs.parse.50_case_9")(x)



c_parseArgs'46parse'4650_case_7 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.Make.c_setModulename(x4)(x2)(st)
c_parseArgs'46parse'4650_case_7 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_parseArgs'46parse'4650_case_7(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c_parseArgs'46parse'4650_case_7 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("Make.parseArgs.parse.50_case_7")(x)



c_parseArgs'46parse'4650_case_8 x1 x2 x5@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.Make.c_parseArgs'46parse'4650(x1)(Curry.Module.Make.c_setMain(Curry.Module.Prelude.C_Just(x8))(x2)(st))(x9)(st)
c_parseArgs'46parse'4650_case_8 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_parseArgs'46parse'4650_case_8(x1)(x2)(x)(st))(i)(xs)(st)
c_parseArgs'46parse'4650_case_8 x1 x2 x st = Curry.RunTimeSystem.patternFail("Make.parseArgs.parse.50_case_8")(x)



c_parseArgs'46parse'4650_case_10 x1 x2 x5@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.Make.c_parseArgs'46parse'4650(x1)(Curry.Module.Make.c_setOutput(Curry.Module.Prelude.C_Just(x6))(x2)(st))(x7)(st)
c_parseArgs'46parse'4650_case_10 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Make.c_parseArgs'46parse'4650_case_10(x1)(x2)(x)(st))(i)(xs)(st)
c_parseArgs'46parse'4650_case_10 x1 x2 x st = Curry.RunTimeSystem.patternFail("Make.parseArgs.parse.50_case_10")(x)


