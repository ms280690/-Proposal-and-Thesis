{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Time (module Curry.Module.Time) where

import Curry.RunTimeSystem
import Curry.Module.Prelude



-- begin included



import System.Time hiding (getClockTime)
import qualified System.Time (getClockTime) 
import Data.Ix

instance ConvertCH C_ClockTime ClockTime where
  fromCurry (C_CTime i) = TOD (fromCurry i) 0
  toCurry (TOD i _) = C_CTime (toCurry i)

instance ConvertCH C_CalendarTime CalendarTime where
  fromCurry (C_CalendarTime y m d h min s tz ) = 
             CalendarTime (fromCurry y) 
                          (toEnum (fromCurry m-1))
                          (fromCurry d)
                          (fromCurry h)
                          (fromCurry min)
                          (fromCurry s)
                          0 undefined undefined undefined
                          (fromCurry tz)
                          undefined
           
  toCurry (CalendarTime y m d h min s _ _ _ _ tz _) = 
          C_CalendarTime (toCurry y) 
                         (toCurry (fromEnum m)+1) 
                         (toCurry d)
                         (toCurry h)
                         (toCurry min)
                         (toCurry s)
                         (toCurry tz)


getClockTime :: Result (C_IO C_ClockTime)
getClockTime = ioFunc0 (System.Time.getClockTime)

prim_toCalendarTime :: C_ClockTime -> Result (C_IO C_CalendarTime)
prim_toCalendarTime = ioFunc1 toCalendarTime

prim_toUTCTime :: C_ClockTime -> Result C_CalendarTime
prim_toUTCTime = extFunc1 toUTCTime

prim_toClockTime :: C_CalendarTime -> Result C_ClockTime
prim_toClockTime x _ = toCurry (toClockTime (fromCurry x))

-- end included

data C_ClockTime = C_CTime Curry.Module.Prelude.C_Int
  | C_ClockTimeFail Curry.RunTimeSystem.C_Exceptions
  | C_ClockTimeOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.Time.C_ClockTime)

data C_CalendarTime = C_CalendarTime Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int
  | C_CalendarTimeFail Curry.RunTimeSystem.C_Exceptions
  | C_CalendarTimeOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.Time.C_CalendarTime)

instance BaseCurry Curry.Module.Time.C_ClockTime where
  nf f (Curry.Module.Time.C_CTime x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.Time.C_CTime(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Time.C_CTime x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.Time.C_CTime(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Time.C_ClockTimeOr(Curry.RunTimeSystem.mkRef(r)(1)(i))([Curry.Module.Time.C_CTime(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(1)

  failed  = Curry.Module.Time.C_ClockTimeFail

  branching  = Curry.Module.Time.C_ClockTimeOr

  consKind (Curry.Module.Time.C_ClockTimeOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Time.C_ClockTimeFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Time.C_ClockTimeFail x) = x

  orRef (Curry.Module.Time.C_ClockTimeOr x _) = x

  branches (Curry.Module.Time.C_ClockTimeOr _ x) = x





instance BaseCurry Curry.Module.Time.C_CalendarTime where
  nf f (Curry.Module.Time.C_CalendarTime x1 x2 x3 x4 x5 x6 x7) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> Curry.RunTimeSystem.nfCTC(\ v5 state5 -> Curry.RunTimeSystem.nfCTC(\ v6 state6 -> Curry.RunTimeSystem.nfCTC(\ v7 state7 -> f(Curry.Module.Time.C_CalendarTime(v1)(v2)(v3)(v4)(v5)(v6)(v7))(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Time.C_CalendarTime x1 x2 x3 x4 x5 x6 x7) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> Curry.RunTimeSystem.gnfCTC(\ v5 state5 -> Curry.RunTimeSystem.gnfCTC(\ v6 state6 -> Curry.RunTimeSystem.gnfCTC(\ v7 state7 -> f(Curry.Module.Time.C_CalendarTime(v1)(v2)(v3)(v4)(v5)(v6)(v7))(state7))(x7)(state6))(x6)(state5))(x5)(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Time.C_CalendarTimeOr(Curry.RunTimeSystem.mkRef(r)(7)(i))([Curry.Module.Time.C_CalendarTime(Curry.RunTimeSystem.generator((Prelude.+)(r)((6::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((5::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((4::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(7)

  failed  = Curry.Module.Time.C_CalendarTimeFail

  branching  = Curry.Module.Time.C_CalendarTimeOr

  consKind (Curry.Module.Time.C_CalendarTimeOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Time.C_CalendarTimeFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Time.C_CalendarTimeFail x) = x

  orRef (Curry.Module.Time.C_CalendarTimeOr x _) = x

  branches (Curry.Module.Time.C_CalendarTimeOr _ x) = x





instance Curry Curry.Module.Time.C_ClockTime where
  strEq (Curry.Module.Time.C_CTime x1) (Curry.Module.Time.C_CTime y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Time.C_CTime x1) (Curry.Module.Time.C_CTime y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Time.C_CTime x1) st = Curry.Module.Time.C_CTime(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.Time.C_CTime x1) st = f(x1)(c)(st)

  typeName _ = "ClockTime"

  showQ d (Curry.Module.Time.C_CTime x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Time.CTime "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.Time.C_ClockTimeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.Time.C_CalendarTime where
  strEq (Curry.Module.Time.C_CalendarTime x1 x2 x3 x4 x5 x6 x7) (Curry.Module.Time.C_CalendarTime y1 y2 y3 y4 y5 y6 y7) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x5)(y5)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x6)(y6)(st))(Curry.Module.Prelude.genStrEq(x7)(y7)(st))(st))(st))(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Time.C_CalendarTime x1 x2 x3 x4 x5 x6 x7) (Curry.Module.Time.C_CalendarTime y1 y2 y3 y4 y5 y6 y7) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x4)(y4)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x5)(y5)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x6)(y6)(st))(Curry.Module.Prelude.genEq(x7)(y7)(st))(st))(st))(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Time.C_CalendarTime x1 x2 x3 x4 x5 x6 x7) st = Curry.Module.Time.C_CalendarTime(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))(f((4::Int))(x5)(st))(f((5::Int))(x6)(st))(f((6::Int))(x7)(st))

  foldCurry f c (Curry.Module.Time.C_CalendarTime x1 x2 x3 x4 x5 x6 x7) st = f(x1)(f(x2)(f(x3)(f(x4)(f(x5)(f(x6)(f(x7)(c)(st))(st))(st))(st))(st))(st))(st)

  typeName _ = "CalendarTime"

  showQ d (Curry.Module.Time.C_CalendarTime x1 x2 x3 x4 x5 x6 x7) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Time.CalendarTime "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x5))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x6))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x7))))))))))))))


  showQ _ (Curry.Module.Time.C_CalendarTimeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.Time.C_ClockTime where
  showsPrec d (Curry.Module.Time.C_CTime x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CTime "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.Time.C_ClockTimeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.Time.C_CalendarTime where
  showsPrec d (Curry.Module.Time.C_CalendarTime x1 x2 x3 x4 x5 x6 x7) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("CalendarTime "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x5))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x6))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x7))))))))))))))


  showsPrec _ (Curry.Module.Time.C_CalendarTimeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Read Curry.Module.Time.C_ClockTime where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Time.C_CTime(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Time")("CTime")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r)





instance Read Curry.Module.Time.C_CalendarTime where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Time.C_CalendarTime(x1)(x2)(x3)(x4)(x5)(x6)(x7))(r7) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Time")("CalendarTime")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3), ((,) x5 r5) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r4), ((,) x6 r6) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r5), ((,) x7 r7) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r6)])(r)





c_ctYear :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ctYear x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x2
c_ctYear (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_ctYear(x)(st))(i)(xs)(st)
c_ctYear x st = Curry.RunTimeSystem.patternFail("Time.ctYear")(x)



c_ctMonth :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ctMonth x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x3
c_ctMonth (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_ctMonth(x)(st))(i)(xs)(st)
c_ctMonth x st = Curry.RunTimeSystem.patternFail("Time.ctMonth")(x)



c_ctDay :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ctDay x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x4
c_ctDay (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_ctDay(x)(st))(i)(xs)(st)
c_ctDay x st = Curry.RunTimeSystem.patternFail("Time.ctDay")(x)



c_ctHour :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ctHour x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x5
c_ctHour (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_ctHour(x)(st))(i)(xs)(st)
c_ctHour x st = Curry.RunTimeSystem.patternFail("Time.ctHour")(x)



c_ctMin :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ctMin x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x6
c_ctMin (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_ctMin(x)(st))(i)(xs)(st)
c_ctMin x st = Curry.RunTimeSystem.patternFail("Time.ctMin")(x)



c_ctSec :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ctSec x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x7
c_ctSec (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_ctSec(x)(st))(i)(xs)(st)
c_ctSec x st = Curry.RunTimeSystem.patternFail("Time.ctSec")(x)



c_ctTZ :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ctTZ x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x8
c_ctTZ (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_ctTZ(x)(st))(i)(xs)(st)
c_ctTZ x st = Curry.RunTimeSystem.patternFail("Time.ctTZ")(x)



c_getLocalTime :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Time.C_CalendarTime
c_getLocalTime st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Time.c_getClockTime(st))(Curry.Module.Prelude.pf(Curry.Module.Time.c_getLocalTime'46_'35lambda2))(st)



c_getLocalTime'46_'35lambda2 :: Curry.Module.Time.C_ClockTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Time.C_CalendarTime
c_getLocalTime'46_'35lambda2 x1 st = Curry.Module.Time.c_toCalendarTime(x1)(st)



c_clockTimeToInt :: Curry.Module.Time.C_ClockTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_clockTimeToInt x1@(Curry.Module.Time.C_CTime x2) st = x2
c_clockTimeToInt (Curry.Module.Time.C_ClockTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_clockTimeToInt(x)(st))(i)(xs)(st)
c_clockTimeToInt x st = Curry.RunTimeSystem.patternFail("Time.clockTimeToInt")(x)



c_toCalendarTime :: Curry.Module.Time.C_ClockTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Time.C_CalendarTime
c_toCalendarTime x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Time.c_prim_toCalendarTime))(x1)(st)



c_toUTCTime :: Curry.Module.Time.C_ClockTime -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_CalendarTime
c_toUTCTime x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Time.c_prim_toUTCTime))(x1)(st)



c_toClockTime :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_ClockTime
c_toClockTime x1 st = Curry.Module.Prelude.op_36_35_35(Curry.Module.Prelude.pf(Curry.Module.Time.c_prim_toClockTime))(x1)(st)



c_calendarTimeToString :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_calendarTimeToString x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.op_33_33((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('J'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('J'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('J'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.List)))))))))))))(Curry.Module.Prelude.op_45(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_show(x4)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.Time.c_toTimeString(x1)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.Prelude.c_show(x2)(st))(st))(st))(st))(st))(st))(st)
c_calendarTimeToString (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_calendarTimeToString(x)(st))(i)(xs)(st)
c_calendarTimeToString x st = Curry.RunTimeSystem.patternFail("Time.calendarTimeToString")(x)



c_toDayString :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_toDayString x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.op_33_33((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('J'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('J'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('J'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List)))))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List)))))))))(Curry.Module.Prelude.List)))))))))))))(Curry.Module.Prelude.op_45(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_show(x4)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.c_show(x2)(st))(st))(st))(st))(st)
c_toDayString (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_toDayString(x)(st))(i)(xs)(st)
c_toDayString x st = Curry.RunTimeSystem.patternFail("Time.toDayString")(x)



c_toTimeString :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_toTimeString x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = Curry.Module.Prelude.op_43_43(Curry.Module.Time.c_toTimeString'46digit2'4690(x5)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.Time.c_toTimeString'46digit2'4690(x6)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.Time.c_toTimeString'46digit2'4690(x7)(st))(st))(st))(st))(st)
c_toTimeString (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_toTimeString(x)(st))(i)(xs)(st)
c_toTimeString x st = Curry.RunTimeSystem.patternFail("Time.toTimeString")(x)



c_toTimeString'46digit2'4690 :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_toTimeString'46digit2'4690 x1 st = Curry.Module.Time.c_toTimeString'46digit2'4690_case_8(x1)(Curry.Module.Prelude.op_60(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(st))(st)



c_addSeconds :: Curry.Module.Prelude.C_Int -> Curry.Module.Time.C_ClockTime -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_ClockTime
c_addSeconds x1 x2@(Curry.Module.Time.C_CTime x3) st = Curry.Module.Time.C_CTime(Curry.Module.Prelude.op_43(x3)(x1)(st))
c_addSeconds x1 (Curry.Module.Time.C_ClockTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addSeconds(x1)(x)(st))(i)(xs)(st)
c_addSeconds x1 x st = Curry.RunTimeSystem.patternFail("Time.addSeconds")(x)



c_addMinutes :: Curry.Module.Prelude.C_Int -> Curry.Module.Time.C_ClockTime -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_ClockTime
c_addMinutes x1 x2@(Curry.Module.Time.C_CTime x3) st = Curry.Module.Time.C_CTime(Curry.Module.Prelude.op_43(x3)(Curry.Module.Prelude.op_42(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))(st))(st))
c_addMinutes x1 (Curry.Module.Time.C_ClockTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addMinutes(x1)(x)(st))(i)(xs)(st)
c_addMinutes x1 x st = Curry.RunTimeSystem.patternFail("Time.addMinutes")(x)



c_addHours :: Curry.Module.Prelude.C_Int -> Curry.Module.Time.C_ClockTime -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_ClockTime
c_addHours x1 x2@(Curry.Module.Time.C_CTime x3) st = Curry.Module.Time.C_CTime(Curry.Module.Prelude.op_43(x3)(Curry.Module.Prelude.op_42(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))))))(st))(st))
c_addHours x1 (Curry.Module.Time.C_ClockTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addHours(x1)(x)(st))(i)(xs)(st)
c_addHours x1 x st = Curry.RunTimeSystem.patternFail("Time.addHours")(x)



c_addDays :: Curry.Module.Prelude.C_Int -> Curry.Module.Time.C_ClockTime -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_ClockTime
c_addDays x1 x2@(Curry.Module.Time.C_CTime x3) st = Curry.Module.Time.C_CTime(Curry.Module.Prelude.op_43(x3)(Curry.Module.Prelude.op_42(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))))))))))))(st))(st))
c_addDays x1 (Curry.Module.Time.C_ClockTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addDays(x1)(x)(st))(i)(xs)(st)
c_addDays x1 x st = Curry.RunTimeSystem.patternFail("Time.addDays")(x)



c_addMonths :: Curry.Module.Prelude.C_Int -> Curry.Module.Time.C_ClockTime -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_ClockTime
c_addMonths x1 x2 st = let {x3 = Curry.Module.Time.c_toUTCTime(x2)(st)} in let {x4 = Curry.Module.Time.c_addMonths'46_'35selFP3'35y(x3)(st)} in let {x5 = Curry.Module.Time.c_addMonths'46_'35selFP4'35mo(x3)(st)} in let {x6 = Curry.Module.Time.c_addMonths'46_'35selFP5'35d(x3)(st)} in let {x7 = Curry.Module.Time.c_addMonths'46_'35selFP6'35h(x3)(st)} in let {x8 = Curry.Module.Time.c_addMonths'46_'35selFP7'35mi(x3)(st)} in let {x9 = Curry.Module.Time.c_addMonths'46_'35selFP8'35s(x3)(st)} in let {x10 = Curry.Module.Time.c_addMonths'46_'35selFP9'35tz(x3)(st)} in let {x11 = Curry.Module.Prelude.op_43(Curry.Module.Prelude.c_mod(Curry.Module.Prelude.op_43(Curry.Module.Prelude.op_45(x5)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x1)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st)} in Curry.Module.Time.c_addMonths_case_7(x1)(x4)(x5)(x6)(x7)(x8)(x9)(x10)(x11)(Curry.Module.Prelude.op_62(x11)(Curry.Module.Prelude.C_Zero)(st))(st)



c_addMonths'46_'35selFP3'35y :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addMonths'46_'35selFP3'35y x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x2
c_addMonths'46_'35selFP3'35y (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addMonths'46_'35selFP3'35y(x)(st))(i)(xs)(st)
c_addMonths'46_'35selFP3'35y x st = Curry.RunTimeSystem.patternFail("Time.addMonths._#selFP3#y")(x)



c_addMonths'46_'35selFP4'35mo :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addMonths'46_'35selFP4'35mo x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x3
c_addMonths'46_'35selFP4'35mo (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addMonths'46_'35selFP4'35mo(x)(st))(i)(xs)(st)
c_addMonths'46_'35selFP4'35mo x st = Curry.RunTimeSystem.patternFail("Time.addMonths._#selFP4#mo")(x)



c_addMonths'46_'35selFP5'35d :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addMonths'46_'35selFP5'35d x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x4
c_addMonths'46_'35selFP5'35d (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addMonths'46_'35selFP5'35d(x)(st))(i)(xs)(st)
c_addMonths'46_'35selFP5'35d x st = Curry.RunTimeSystem.patternFail("Time.addMonths._#selFP5#d")(x)



c_addMonths'46_'35selFP6'35h :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addMonths'46_'35selFP6'35h x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x5
c_addMonths'46_'35selFP6'35h (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addMonths'46_'35selFP6'35h(x)(st))(i)(xs)(st)
c_addMonths'46_'35selFP6'35h x st = Curry.RunTimeSystem.patternFail("Time.addMonths._#selFP6#h")(x)



c_addMonths'46_'35selFP7'35mi :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addMonths'46_'35selFP7'35mi x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x6
c_addMonths'46_'35selFP7'35mi (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addMonths'46_'35selFP7'35mi(x)(st))(i)(xs)(st)
c_addMonths'46_'35selFP7'35mi x st = Curry.RunTimeSystem.patternFail("Time.addMonths._#selFP7#mi")(x)



c_addMonths'46_'35selFP8'35s :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addMonths'46_'35selFP8'35s x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x7
c_addMonths'46_'35selFP8'35s (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addMonths'46_'35selFP8'35s(x)(st))(i)(xs)(st)
c_addMonths'46_'35selFP8'35s x st = Curry.RunTimeSystem.patternFail("Time.addMonths._#selFP8#s")(x)



c_addMonths'46_'35selFP9'35tz :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addMonths'46_'35selFP9'35tz x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x8
c_addMonths'46_'35selFP9'35tz (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addMonths'46_'35selFP9'35tz(x)(st))(i)(xs)(st)
c_addMonths'46_'35selFP9'35tz x st = Curry.RunTimeSystem.patternFail("Time.addMonths._#selFP9#tz")(x)



c_addYears :: Curry.Module.Prelude.C_Int -> Curry.Module.Time.C_ClockTime -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_ClockTime
c_addYears x1 x2 st = Curry.Module.Time.c_addYears_case_6(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.C_Zero)(st))(st)



c_addYears'46_'35selFP11'35y :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addYears'46_'35selFP11'35y x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x2
c_addYears'46_'35selFP11'35y (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addYears'46_'35selFP11'35y(x)(st))(i)(xs)(st)
c_addYears'46_'35selFP11'35y x st = Curry.RunTimeSystem.patternFail("Time.addYears._#selFP11#y")(x)



c_addYears'46_'35selFP12'35mo :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addYears'46_'35selFP12'35mo x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x3
c_addYears'46_'35selFP12'35mo (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addYears'46_'35selFP12'35mo(x)(st))(i)(xs)(st)
c_addYears'46_'35selFP12'35mo x st = Curry.RunTimeSystem.patternFail("Time.addYears._#selFP12#mo")(x)



c_addYears'46_'35selFP13'35d :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addYears'46_'35selFP13'35d x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x4
c_addYears'46_'35selFP13'35d (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addYears'46_'35selFP13'35d(x)(st))(i)(xs)(st)
c_addYears'46_'35selFP13'35d x st = Curry.RunTimeSystem.patternFail("Time.addYears._#selFP13#d")(x)



c_addYears'46_'35selFP14'35h :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addYears'46_'35selFP14'35h x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x5
c_addYears'46_'35selFP14'35h (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addYears'46_'35selFP14'35h(x)(st))(i)(xs)(st)
c_addYears'46_'35selFP14'35h x st = Curry.RunTimeSystem.patternFail("Time.addYears._#selFP14#h")(x)



c_addYears'46_'35selFP15'35mi :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addYears'46_'35selFP15'35mi x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x6
c_addYears'46_'35selFP15'35mi (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addYears'46_'35selFP15'35mi(x)(st))(i)(xs)(st)
c_addYears'46_'35selFP15'35mi x st = Curry.RunTimeSystem.patternFail("Time.addYears._#selFP15#mi")(x)



c_addYears'46_'35selFP16'35s :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addYears'46_'35selFP16'35s x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x7
c_addYears'46_'35selFP16'35s (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addYears'46_'35selFP16'35s(x)(st))(i)(xs)(st)
c_addYears'46_'35selFP16'35s x st = Curry.RunTimeSystem.patternFail("Time.addYears._#selFP16#s")(x)



c_addYears'46_'35selFP17'35tz :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addYears'46_'35selFP17'35tz x1@(Curry.Module.Time.C_CalendarTime x2 x3 x4 x5 x6 x7 x8) st = x8
c_addYears'46_'35selFP17'35tz (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addYears'46_'35selFP17'35tz(x)(st))(i)(xs)(st)
c_addYears'46_'35selFP17'35tz x st = Curry.RunTimeSystem.patternFail("Time.addYears._#selFP17#tz")(x)



c_daysOfMonth :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_daysOfMonth x1 x2 st = Curry.Module.Time.c_daysOfMonth_case_5(x1)(x2)(Curry.Module.Prelude.op_47_61(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(st)



c_validDate :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_validDate x1 x2 x3 st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_62(x2)(Curry.Module.Prelude.C_Zero)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_60(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_62(x3)(Curry.Module.Prelude.C_Zero)(st))(Curry.Module.Prelude.op_60_61(x3)(Curry.Module.Time.c_daysOfMonth(x2)(x1)(st))(st))(st))(st))(st)



c_compareDate :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering))
c_compareDate st = Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Time.c_compareCalendarTime)



c_compareCalendarTime :: Curry.Module.Time.C_CalendarTime -> Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_compareCalendarTime x1 x2 st = Curry.Module.Time.c_compareClockTime(Curry.Module.Time.c_toClockTime(x1)(st))(Curry.Module.Time.c_toClockTime(x2)(st))(st)



c_compareClockTime :: Curry.Module.Time.C_ClockTime -> Curry.Module.Time.C_ClockTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_compareClockTime x1@(Curry.Module.Time.C_CTime x3) x2 st = Curry.Module.Time.c_compareClockTime_case_3(x3)(x2)(st)
c_compareClockTime (Curry.Module.Time.C_ClockTimeOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_compareClockTime(x)(x2)(st))(i)(xs)(st)
c_compareClockTime x x2 st = Curry.RunTimeSystem.patternFail("Time.compareClockTime")(x)



c_compareClockTime_case_3 x3 x2@(Curry.Module.Time.C_CTime x4) st = Curry.Module.Time.c_compareClockTime_case_2(x3)(x4)(Curry.Module.Prelude.op_60(x3)(x4)(st))(st)
c_compareClockTime_case_3 x3 (Curry.Module.Time.C_ClockTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_compareClockTime_case_3(x3)(x)(st))(i)(xs)(st)
c_compareClockTime_case_3 x3 x st = Curry.RunTimeSystem.patternFail("Time.compareClockTime_case_3")(x)



c_compareClockTime_case_2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_LT
c_compareClockTime_case_2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Time.c_compareClockTime_case_1(x3)(x4)(Curry.Module.Prelude.op_62(x3)(x4)(st))(st)
c_compareClockTime_case_2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_compareClockTime_case_2(x3)(x4)(x)(st))(i)(xs)(st)
c_compareClockTime_case_2 x3 x4 x st = Curry.RunTimeSystem.patternFail("Time.compareClockTime_case_2")(x)



c_compareClockTime_case_1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_GT
c_compareClockTime_case_1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Time.c_compareClockTime_case_0(Curry.Module.Prelude.c_otherwise(st))(st)
c_compareClockTime_case_1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_compareClockTime_case_1(x3)(x4)(x)(st))(i)(xs)(st)
c_compareClockTime_case_1 x3 x4 x st = Curry.RunTimeSystem.patternFail("Time.compareClockTime_case_1")(x)



c_compareClockTime_case_0 x1@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_EQ
c_compareClockTime_case_0 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_compareClockTime_case_0(x)(st))(i)(xs)(st)
c_compareClockTime_case_0 x st = Curry.RunTimeSystem.patternFail("Time.compareClockTime_case_0")(x)



c_daysOfMonth_case_5 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_33_33((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(Curry.Module.Prelude.List)))))))))))))(Curry.Module.Prelude.op_45(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st)
c_daysOfMonth_case_5 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Time.c_daysOfMonth_case_4(x2)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_mod(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))(st))(Curry.Module.Prelude.C_Zero)(st))(Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_47_61(Curry.Module.Prelude.c_mod(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))))(st))(Curry.Module.Prelude.C_Zero)(st))(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_mod(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))))))(st))(Curry.Module.Prelude.C_Zero)(st))(st))(st))(st)
c_daysOfMonth_case_5 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_daysOfMonth_case_5(x1)(x2)(x)(st))(i)(xs)(st)
c_daysOfMonth_case_5 x1 x2 x st = Curry.RunTimeSystem.patternFail("Time.daysOfMonth_case_5")(x)



c_daysOfMonth_case_4 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))
c_daysOfMonth_case_4 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))
c_daysOfMonth_case_4 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_daysOfMonth_case_4(x2)(x)(st))(i)(xs)(st)
c_daysOfMonth_case_4 x2 x st = Curry.RunTimeSystem.patternFail("Time.daysOfMonth_case_4")(x)



c_addYears_case_6 x1 x2 x3@Curry.Module.Prelude.C_True st = x2
c_addYears_case_6 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x3 = Curry.Module.Time.c_toUTCTime(x2)(st)} in Curry.Module.Time.c_toClockTime(Curry.Module.Time.C_CalendarTime(Curry.Module.Prelude.op_43(Curry.Module.Time.c_addYears'46_'35selFP11'35y(x3)(st))(x1)(st))(Curry.Module.Time.c_addYears'46_'35selFP12'35mo(x3)(st))(Curry.Module.Time.c_addYears'46_'35selFP13'35d(x3)(st))(Curry.Module.Time.c_addYears'46_'35selFP14'35h(x3)(st))(Curry.Module.Time.c_addYears'46_'35selFP15'35mi(x3)(st))(Curry.Module.Time.c_addYears'46_'35selFP16'35s(x3)(st))(Curry.Module.Time.c_addYears'46_'35selFP17'35tz(x3)(st)))(st)
c_addYears_case_6 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addYears_case_6(x1)(x2)(x)(st))(i)(xs)(st)
c_addYears_case_6 x1 x2 x st = Curry.RunTimeSystem.patternFail("Time.addYears_case_6")(x)



c_addMonths_case_7 x1 x4 x5 x6 x7 x8 x9 x10 x11 x12@Curry.Module.Prelude.C_True st = Curry.Module.Time.c_addYears(Curry.Module.Prelude.c_div(Curry.Module.Prelude.op_43(Curry.Module.Prelude.op_45(x5)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x1)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))(st))(Curry.Module.Time.c_toClockTime(Curry.Module.Time.C_CalendarTime(x4)(x11)(x6)(x7)(x8)(x9)(x10))(st))(st)
c_addMonths_case_7 x1 x4 x5 x6 x7 x8 x9 x10 x11 x12@Curry.Module.Prelude.C_False st = Curry.Module.Time.c_addYears(Curry.Module.Prelude.op_45(Curry.Module.Prelude.c_div(Curry.Module.Prelude.op_43(Curry.Module.Prelude.op_45(x5)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x1)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.Time.c_toClockTime(Curry.Module.Time.C_CalendarTime(x4)(Curry.Module.Prelude.op_43(x11)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))(st))(x6)(x7)(x8)(x9)(x10))(st))(st)
c_addMonths_case_7 x1 x4 x5 x6 x7 x8 x9 x10 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_addMonths_case_7(x1)(x4)(x5)(x6)(x7)(x8)(x9)(x10)(x11)(x)(st))(i)(xs)(st)
c_addMonths_case_7 x1 x4 x5 x6 x7 x8 x9 x10 x11 x st = Curry.RunTimeSystem.patternFail("Time.addMonths_case_7")(x)



c_toTimeString'46digit2'4690_case_8 x1 x2@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('0'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_chr(Curry.Module.Prelude.op_43(Curry.Module.Prelude.c_ord(Curry.Module.Prelude.C_Char('0'))(st))(x1)(st))(st))(Curry.Module.Prelude.List))
c_toTimeString'46digit2'4690_case_8 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_show(x1)(st)
c_toTimeString'46digit2'4690_case_8 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Time.c_toTimeString'46digit2'4690_case_8(x1)(x)(st))(i)(xs)(st)
c_toTimeString'46digit2'4690_case_8 x1 x st = Curry.RunTimeSystem.patternFail("Time.toTimeString.digit2.90_case_8")(x)



c_getClockTime :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Time.C_ClockTime
c_getClockTime st = Curry.Module.Time.getClockTime(st)



c_prim_toCalendarTime :: Curry.Module.Time.C_ClockTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Time.C_CalendarTime
c_prim_toCalendarTime x1 st = Curry.Module.Time.prim_toCalendarTime(x1)(st)



c_prim_toUTCTime :: Curry.Module.Time.C_ClockTime -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_CalendarTime
c_prim_toUTCTime x1 st = Curry.Module.Time.prim_toUTCTime(x1)(st)



c_prim_toClockTime :: Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_ClockTime
c_prim_toClockTime x1 st = Curry.Module.Time.prim_toClockTime(x1)(st)


