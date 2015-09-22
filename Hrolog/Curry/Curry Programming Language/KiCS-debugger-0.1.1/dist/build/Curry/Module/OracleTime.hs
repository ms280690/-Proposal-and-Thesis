{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleTime (module Curry.Module.OracleTime) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.Time
import Curry.Module.Prelude
import Curry.Module.OraclePrelude



-- begin included



-- end included

c_ctYear :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ctYear x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_38(x2)(x1)(st))(st)



c_ctMonth :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ctMonth x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_37(x2)(x1)(st))(st)



c_ctDay :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ctDay x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_36(x2)(x1)(st))(st)



c_ctHour :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ctHour x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_35(x2)(x1)(st))(st)



c_ctMin :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ctMin x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_34(x2)(x1)(st))(st)



c_ctSec :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ctSec x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_33(x2)(x1)(st))(st)



c_ctTZ :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ctTZ x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_32(x2)(x1)(st))(st)



c_getLocalTime :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Time.C_CalendarTime))
c_getLocalTime x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleTime.c_getClockTime(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleTime.c_getLocalTime'46_'35lambda2))))(x2)(st))(st)



c_getLocalTime'46_'35lambda2 :: Curry.Module.Time.C_ClockTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Time.C_CalendarTime))
c_getLocalTime'46_'35lambda2 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c_toCalendarTime(x2)(x1)(st))(st)



c_clockTimeToInt :: Curry.Module.Time.C_ClockTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_clockTimeToInt x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_31(x2)(x1)(st))(st)



c_toCalendarTime :: Curry.Module.Time.C_ClockTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Time.C_CalendarTime))
c_toCalendarTime x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleTime.c_prim_toCalendarTime))))(x2)(x1)(st))(st)



c_toUTCTime :: Curry.Module.Time.C_ClockTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_CalendarTime
c_toUTCTime x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleTime.c_prim_toUTCTime))))(x2)(x1)(st))(st)



c_toClockTime :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_ClockTime
c_toClockTime x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.op_36_35_35(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleTime.c_prim_toClockTime))))(x2)(x1)(st))(st)



c_calendarTimeToString :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_calendarTimeToString x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_30(x2)(x1)(st))(st)



c_toDayString :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_toDayString x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_29(x2)(x1)(st))(st)



c_toTimeString :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_toTimeString x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_28(x2)(x1)(st))(st)



c_toTimeString'46digit2'4690 :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_toTimeString'46digit2'4690 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OracleTime.c__case_27(x2)(Curry.Module.OraclePrelude.op_60(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))(x1)(st))(x3)(st))(st)



c_addSeconds :: Curry.Module.Prelude.C_Int -> Curry.Module.Time.C_ClockTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_ClockTime
c_addSeconds x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_26(x2)(x3)(x1)(st))(st)



c_addMinutes :: Curry.Module.Prelude.C_Int -> Curry.Module.Time.C_ClockTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_ClockTime
c_addMinutes x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_25(x2)(x3)(x1)(st))(st)



c_addHours :: Curry.Module.Prelude.C_Int -> Curry.Module.Time.C_ClockTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_ClockTime
c_addHours x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_24(x2)(x3)(x1)(st))(st)



c_addDays :: Curry.Module.Prelude.C_Int -> Curry.Module.Time.C_ClockTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_ClockTime
c_addDays x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_23(x2)(x3)(x1)(st))(st)



c_addMonths :: Curry.Module.Prelude.C_Int -> Curry.Module.Time.C_ClockTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_ClockTime
c_addMonths x2 x3 x1 st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(let {x4 = Curry.Module.OracleTime.c_toUTCTime(x3)(x1)(st)} in let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(let {x6 = Curry.Module.OracleTime.c_addMonths'46_'35selFP4'35mo(x4)(x14)(st)} in let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))(let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))(let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))(let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))(let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List)))))(let {x12 = Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.c_mod(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_45(x6)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x20)(st))(x2)(x21)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))(x22)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x23)(st)} in let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x24)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))(Curry.Module.OracleTime.c__case_22(x2)(Curry.Module.OracleTime.c_addMonths'46_'35selFP3'35y(x4)(x13)(st))(x6)(Curry.Module.OracleTime.c_addMonths'46_'35selFP5'35d(x4)(x15)(st))(Curry.Module.OracleTime.c_addMonths'46_'35selFP6'35h(x4)(x16)(st))(Curry.Module.OracleTime.c_addMonths'46_'35selFP7'35mi(x4)(x17)(st))(Curry.Module.OracleTime.c_addMonths'46_'35selFP8'35s(x4)(x18)(st))(Curry.Module.OracleTime.c_addMonths'46_'35selFP9'35tz(x4)(x19)(st))(x12)(Curry.Module.OraclePrelude.op_62(x12)(Curry.Module.Prelude.C_Zero)(x24)(st))(x25)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)



c_addMonths'46_'35selFP3'35y :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addMonths'46_'35selFP3'35y x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_21(x2)(x1)(st))(st)



c_addMonths'46_'35selFP4'35mo :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addMonths'46_'35selFP4'35mo x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_20(x2)(x1)(st))(st)



c_addMonths'46_'35selFP5'35d :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addMonths'46_'35selFP5'35d x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_19(x2)(x1)(st))(st)



c_addMonths'46_'35selFP6'35h :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addMonths'46_'35selFP6'35h x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_18(x2)(x1)(st))(st)



c_addMonths'46_'35selFP7'35mi :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addMonths'46_'35selFP7'35mi x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_17(x2)(x1)(st))(st)



c_addMonths'46_'35selFP8'35s :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addMonths'46_'35selFP8'35s x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_16(x2)(x1)(st))(st)



c_addMonths'46_'35selFP9'35tz :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addMonths'46_'35selFP9'35tz x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_15(x2)(x1)(st))(st)



c_addYears :: Curry.Module.Prelude.C_Int -> Curry.Module.Time.C_ClockTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_ClockTime
c_addYears x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleTime.c__case_14(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Zero)(x1)(st))(x4)(st))(st)



c_addYears'46_'35selFP11'35y :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addYears'46_'35selFP11'35y x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_13(x2)(x1)(st))(st)



c_addYears'46_'35selFP12'35mo :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addYears'46_'35selFP12'35mo x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_12(x2)(x1)(st))(st)



c_addYears'46_'35selFP13'35d :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addYears'46_'35selFP13'35d x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_11(x2)(x1)(st))(st)



c_addYears'46_'35selFP14'35h :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addYears'46_'35selFP14'35h x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_10(x2)(x1)(st))(st)



c_addYears'46_'35selFP15'35mi :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addYears'46_'35selFP15'35mi x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_9(x2)(x1)(st))(st)



c_addYears'46_'35selFP16'35s :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addYears'46_'35selFP16'35s x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_8(x2)(x1)(st))(st)



c_addYears'46_'35selFP17'35tz :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_addYears'46_'35selFP17'35tz x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_7(x2)(x1)(st))(st)



c_daysOfMonth :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_daysOfMonth x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleTime.c__case_6(x2)(x3)(Curry.Module.OraclePrelude.op_47_61(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x1)(st))(x4)(st))(st)



c_validDate :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_validDate x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_62(x3)(Curry.Module.Prelude.C_Zero)(x1)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_60(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))(x5)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_62(x4)(Curry.Module.Prelude.C_Zero)(x6)(st))(Curry.Module.OraclePrelude.op_60_61(x4)(Curry.Module.OracleTime.c_daysOfMonth(x3)(x2)(x7)(st))(x8)(st))(x9)(st))(x10)(st))(x11)(st))(st)



c_compareDate :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Time.C_CalendarTime -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering))))
c_compareDate x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleTime.c_compareCalendarTime))(st))(st)



c_compareCalendarTime :: Curry.Module.Time.C_CalendarTime -> Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_compareCalendarTime x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OracleTime.c_compareClockTime(Curry.Module.OracleTime.c_toClockTime(x2)(x1)(st))(Curry.Module.OracleTime.c_toClockTime(x3)(x4)(st))(x5)(st))(st)



c_compareClockTime :: Curry.Module.Time.C_ClockTime -> Curry.Module.Time.C_ClockTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_compareClockTime x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_4(x3)(x2)(x1)(st))(st)



c__case_4 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_4_case__38(x1)(x3)(x2)(st))(st)



c__case_3 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_3_case__37(x1)(x4)(x3)(st))(st)



c__case_2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_2_case__36(x1)(x4)(x5)(x6)(st))(st)



c__case_1 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_1_case__35(x1)(x6)(st))(st)



c__case_0 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_0_case__34(x1)(x2)(st))(st)



c__case_6 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_6_case__33(x1)(x2)(x3)(x4)(st))(st)



c__case_5 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_5_case__32(x1)(x4)(st))(st)



c__case_7 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_7_case__31(x1)(x2)(st))(st)



c__case_8 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_8_case__30(x1)(x2)(st))(st)



c__case_9 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_9_case__29(x1)(x2)(st))(st)



c__case_10 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_10_case__28(x1)(x2)(st))(st)



c__case_11 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_11_case__27(x1)(x2)(st))(st)



c__case_12 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_12_case__26(x1)(x2)(st))(st)



c__case_13 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_13_case__25(x1)(x2)(st))(st)



c__case_14 x2 x3 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_14_case__24(x1)(x2)(x3)(x12)(st))(st)



c__case_15 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_15_case__23(x1)(x2)(st))(st)



c__case_16 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_16_case__22(x1)(x2)(st))(st)



c__case_17 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_17_case__21(x1)(x2)(st))(st)



c__case_18 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_18_case__20(x1)(x2)(st))(st)



c__case_19 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_19_case__19(x1)(x2)(st))(st)



c__case_20 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_20_case__18(x1)(x2)(st))(st)



c__case_21 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_21_case__17(x1)(x2)(st))(st)



c__case_22 x2 x5 x6 x7 x8 x9 x10 x11 x12 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_22_case__16(x1)(x2)(x5)(x6)(x7)(x8)(x9)(x10)(x11)(x12)(x13)(st))(st)



c__case_23 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_23_case__15(x1)(x2)(x3)(st))(st)



c__case_24 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_24_case__14(x1)(x2)(x3)(st))(st)



c__case_25 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_25_case__13(x1)(x2)(x3)(st))(st)



c__case_26 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_26_case__12(x1)(x2)(x3)(st))(st)



c__case_27 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_27_case__11(x1)(x2)(x3)(st))(st)



c__case_28 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_28_case__10(x1)(x2)(st))(st)



c__case_29 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_29_case__9(x1)(x2)(st))(st)



c__case_30 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_30_case__8(x1)(x2)(st))(st)



c__case_31 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_31_case__7(x1)(x2)(st))(st)



c__case_32 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_32_case__6(x1)(x2)(st))(st)



c__case_33 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_33_case__5(x1)(x2)(st))(st)



c__case_34 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_34_case__4(x1)(x2)(st))(st)



c__case_35 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_35_case__3(x1)(x2)(st))(st)



c__case_36 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_36_case__2(x1)(x2)(st))(st)



c__case_37 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_37_case__1(x1)(x2)(st))(st)



c__case_38 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_38_case__0(x1)(x2)(st))(st)



c_getClockTime :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Time.C_ClockTime))
c_getClockTime x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Oracle.c_safeIOResult(Curry.Module.Time.c_getClockTime(st))(st))))(st)



c_prim_toCalendarTime :: Curry.Module.Time.C_ClockTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Time.C_CalendarTime))
c_prim_toCalendarTime x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_lambda_world(Curry.Module.Oracle.c_safeIOResult(Curry.Module.Time.c_prim_toCalendarTime(x2)(st))(st))))(st)



c_prim_toUTCTime :: Curry.Module.Time.C_ClockTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_CalendarTime
c_prim_toUTCTime x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Time.c_prim_toUTCTime(x2)(st))(st)



c_prim_toClockTime :: Curry.Module.Time.C_CalendarTime -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Time.C_ClockTime
c_prim_toClockTime x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Time.c_prim_toClockTime(x2)(st))(st)



c__case_38_case__0 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_38_case__0 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_38_case__0(x1)(x)(st))(i)(xs)(st)
c__case_38_case__0 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_38_case__0")(x)



c__case_37_case__1 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_37_case__1 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_37_case__1(x1)(x)(st))(i)(xs)(st)
c__case_37_case__1 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_37_case__1")(x)



c__case_36_case__2 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_36_case__2 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_36_case__2(x1)(x)(st))(i)(xs)(st)
c__case_36_case__2 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_36_case__2")(x)



c__case_35_case__3 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_35_case__3 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_35_case__3(x1)(x)(st))(i)(xs)(st)
c__case_35_case__3 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_35_case__3")(x)



c__case_34_case__4 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x7)(st)
c__case_34_case__4 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_34_case__4(x1)(x)(st))(i)(xs)(st)
c__case_34_case__4 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_34_case__4")(x)



c__case_33_case__5 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_33_case__5 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_33_case__5(x1)(x)(st))(i)(xs)(st)
c__case_33_case__5 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_33_case__5")(x)



c__case_32_case__6 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x9)(st)
c__case_32_case__6 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_32_case__6(x1)(x)(st))(i)(xs)(st)
c__case_32_case__6 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_32_case__6")(x)



c__case_31_case__7 x1 x2@(Curry.Module.Time.C_CTime x3) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_31_case__7 x1 (Curry.Module.Time.C_ClockTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_31_case__7(x1)(x)(st))(i)(xs)(st)
c__case_31_case__7 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_31_case__7")(x)



c__case_30_case__8 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List)))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.op_33_33((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('J'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('J'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('J'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.List)))))))))))))(Curry.Module.OraclePrelude.op_45(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x11)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x5)(x12)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleTime.c_toTimeString(x2)(x13)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_show(x3)(x14)(st))(x15)(st))(x16)(st))(x17)(st))(x18)(st))(x19)(st))(x20)(st))(st))(st)
c__case_30_case__8 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_30_case__8(x1)(x)(st))(i)(xs)(st)
c__case_30_case__8 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_30_case__8")(x)



c__case_29_case__9 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.op_33_33((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('J'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('J'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('J'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List)))))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List)))))))))(Curry.Module.Prelude.List)))))))))))))(Curry.Module.OraclePrelude.op_45(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x11)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x5)(x12)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_show(x3)(x13)(st))(x14)(st))(x15)(st))(x16)(st))(x17)(st))(st))(st)
c__case_29_case__9 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_29_case__9(x1)(x)(st))(i)(xs)(st)
c__case_29_case__9 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_29_case__9")(x)



c__case_28_case__10 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleTime.c_toTimeString'46digit2'4690(x6)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleTime.c_toTimeString'46digit2'4690(x7)(x10)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.OracleTime.c_toTimeString'46digit2'4690(x8)(x11)(st))(x12)(st))(x13)(st))(x14)(st))(x15)(st))(st)
c__case_28_case__10 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_28_case__10(x1)(x)(st))(i)(xs)(st)
c__case_28_case__10 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_28_case__10")(x)



c__case_27_case__11 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('0'))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.c_chr(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.c_ord(Curry.Module.Prelude.C_Char('0'))(x1)(st))(x2)(x4)(st))(x5)(st))(Curry.Module.Prelude.List)))(st)
c__case_27_case__11 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))(st)
c__case_27_case__11 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_27_case__11(x1)(x2)(x)(st))(i)(xs)(st)
c__case_27_case__11 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_27_case__11")(x)



c__case_26_case__12 x1 x2 x3@(Curry.Module.Time.C_CTime x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Time.C_CTime(Curry.Module.OraclePrelude.op_43(x4)(x2)(x1)(st)))(st)
c__case_26_case__12 x1 x2 (Curry.Module.Time.C_ClockTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_26_case__12(x1)(x2)(x)(st))(i)(xs)(st)
c__case_26_case__12 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_26_case__12")(x)



c__case_25_case__13 x1 x2 x3@(Curry.Module.Time.C_CTime x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.Time.C_CTime(Curry.Module.OraclePrelude.op_43(x4)(Curry.Module.OraclePrelude.op_42(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))(x1)(st))(x5)(st)))(st)
c__case_25_case__13 x1 x2 (Curry.Module.Time.C_ClockTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_25_case__13(x1)(x2)(x)(st))(i)(xs)(st)
c__case_25_case__13 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_25_case__13")(x)



c__case_24_case__14 x1 x2 x3@(Curry.Module.Time.C_CTime x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.Time.C_CTime(Curry.Module.OraclePrelude.op_43(x4)(Curry.Module.OraclePrelude.op_42(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))))))))(x1)(st))(x5)(st)))(st)
c__case_24_case__14 x1 x2 (Curry.Module.Time.C_ClockTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_24_case__14(x1)(x2)(x)(st))(i)(xs)(st)
c__case_24_case__14 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_24_case__14")(x)



c__case_23_case__15 x1 x2 x3@(Curry.Module.Time.C_CTime x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.Time.C_CTime(Curry.Module.OraclePrelude.op_43(x4)(Curry.Module.OraclePrelude.op_42(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))))))))))))(x1)(st))(x5)(st)))(st)
c__case_23_case__15 x1 x2 (Curry.Module.Time.C_ClockTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_23_case__15(x1)(x2)(x)(st))(i)(xs)(st)
c__case_23_case__15 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_23_case__15")(x)



c__case_22_case__16 x1 x2 x5 x6 x7 x8 x9 x10 x11 x12 x13@Curry.Module.Prelude.C_True st = let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List)))))(Curry.Module.OracleTime.c_addYears(Curry.Module.OraclePrelude.c_div(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_45(x6)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x2)(x14)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))(x15)(st))(Curry.Module.OracleTime.c_toClockTime(Curry.Module.Time.C_CalendarTime(x5)(x12)(x7)(x8)(x9)(x10)(x11))(x16)(st))(x17)(st))(st)
c__case_22_case__16 x1 x2 x5 x6 x7 x8 x9 x10 x11 x12 x13@Curry.Module.Prelude.C_False st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List)))))))(Curry.Module.OracleTime.c_addYears(Curry.Module.OraclePrelude.op_45(Curry.Module.OraclePrelude.c_div(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.op_45(x6)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x2)(x18)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))(x19)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x20)(st))(Curry.Module.OracleTime.c_toClockTime(Curry.Module.Time.C_CalendarTime(x5)(Curry.Module.OraclePrelude.op_43(x12)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))(x21)(st))(x7)(x8)(x9)(x10)(x11))(x22)(st))(x23)(st))(st)
c__case_22_case__16 x1 x2 x5 x6 x7 x8 x9 x10 x11 x12 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_22_case__16(x1)(x2)(x5)(x6)(x7)(x8)(x9)(x10)(x11)(x12)(x)(st))(i)(xs)(st)
c__case_22_case__16 x1 x2 x5 x6 x7 x8 x9 x10 x11 x12 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_22_case__16")(x)



c__case_21_case__17 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_21_case__17 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_21_case__17(x1)(x)(st))(i)(xs)(st)
c__case_21_case__17 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_21_case__17")(x)



c__case_20_case__18 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_20_case__18 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_20_case__18(x1)(x)(st))(i)(xs)(st)
c__case_20_case__18 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_20_case__18")(x)



c__case_19_case__19 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_19_case__19 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_19_case__19(x1)(x)(st))(i)(xs)(st)
c__case_19_case__19 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_19_case__19")(x)



c__case_18_case__20 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_18_case__20 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_18_case__20(x1)(x)(st))(i)(xs)(st)
c__case_18_case__20 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_18_case__20")(x)



c__case_17_case__21 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x7)(st)
c__case_17_case__21 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_17_case__21(x1)(x)(st))(i)(xs)(st)
c__case_17_case__21 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_17_case__21")(x)



c__case_16_case__22 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_16_case__22 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_16_case__22(x1)(x)(st))(i)(xs)(st)
c__case_16_case__22 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_16_case__22")(x)



c__case_15_case__23 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x9)(st)
c__case_15_case__23 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_15_case__23(x1)(x)(st))(i)(xs)(st)
c__case_15_case__23 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_15_case__23")(x)



c__case_14_case__24 x1 x2 x3 x12@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_14_case__24 x1 x2 x3 x12@Curry.Module.Prelude.C_False st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(let {x4 = Curry.Module.OracleTime.c_toUTCTime(x3)(x1)(st)} in let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))(let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))(let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))(let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))(let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))(Curry.Module.OracleTime.c_toClockTime(Curry.Module.Time.C_CalendarTime(Curry.Module.OraclePrelude.op_43(Curry.Module.OracleTime.c_addYears'46_'35selFP11'35y(x4)(x13)(st))(x2)(x20)(st))(Curry.Module.OracleTime.c_addYears'46_'35selFP12'35mo(x4)(x14)(st))(Curry.Module.OracleTime.c_addYears'46_'35selFP13'35d(x4)(x15)(st))(Curry.Module.OracleTime.c_addYears'46_'35selFP14'35h(x4)(x16)(st))(Curry.Module.OracleTime.c_addYears'46_'35selFP15'35mi(x4)(x17)(st))(Curry.Module.OracleTime.c_addYears'46_'35selFP16'35s(x4)(x18)(st))(Curry.Module.OracleTime.c_addYears'46_'35selFP17'35tz(x4)(x19)(st)))(x21)(st))(st))(st))(st))(st))(st))(st))(st))(st))(st)
c__case_14_case__24 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_14_case__24(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_14_case__24 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_14_case__24")(x)



c__case_13_case__25 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_13_case__25 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_13_case__25(x1)(x)(st))(i)(xs)(st)
c__case_13_case__25 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_13_case__25")(x)



c__case_12_case__26 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_12_case__26 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_12_case__26(x1)(x)(st))(i)(xs)(st)
c__case_12_case__26 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_12_case__26")(x)



c__case_11_case__27 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_11_case__27 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_11_case__27(x1)(x)(st))(i)(xs)(st)
c__case_11_case__27 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_11_case__27")(x)



c__case_10_case__28 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_10_case__28 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_10_case__28(x1)(x)(st))(i)(xs)(st)
c__case_10_case__28 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_10_case__28")(x)



c__case_9_case__29 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x7)(st)
c__case_9_case__29 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_9_case__29(x1)(x)(st))(i)(xs)(st)
c__case_9_case__29 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_9_case__29")(x)



c__case_8_case__30 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_8_case__30 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_8_case__30(x1)(x)(st))(i)(xs)(st)
c__case_8_case__30 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_8_case__30")(x)



c__case_7_case__31 x1 x2@(Curry.Module.Time.C_CalendarTime x3 x4 x5 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x9)(st)
c__case_7_case__31 x1 (Curry.Module.Time.C_CalendarTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_7_case__31(x1)(x)(st))(i)(xs)(st)
c__case_7_case__31 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_7_case__31")(x)



c__case_5_case__32 x1 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(st)
c__case_5_case__32 x1 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(st)
c__case_5_case__32 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_5_case__32(x1)(x)(st))(i)(xs)(st)
c__case_5_case__32 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_5_case__32")(x)



c__case_6_case__33 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_33_33((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(Curry.Module.Prelude.List)))))))))))))(Curry.Module.OraclePrelude.op_45(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x1)(st))(x5)(st))(st)
c__case_6_case__33 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List)))))))))(Curry.Module.OracleTime.c__case_5(x3)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_mod(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))(x1)(st))(Curry.Module.Prelude.C_Zero)(x6)(st))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_47_61(Curry.Module.OraclePrelude.c_mod(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))))(x7)(st))(Curry.Module.Prelude.C_Zero)(x8)(st))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_mod(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))))))(x9)(st))(Curry.Module.Prelude.C_Zero)(x10)(st))(x11)(st))(x12)(st))(x13)(st))(st)
c__case_6_case__33 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_6_case__33(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_6_case__33 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_6_case__33")(x)



c__case_0_case__34 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_EQ)(st)
c__case_0_case__34 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_0_case__34 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_0_case__34(x1)(x)(st))(i)(xs)(st)
c__case_0_case__34 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_0_case__34")(x)



c__case_1_case__35 x1 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_GT)(st)
c__case_1_case__35 x1 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleTime.c__case_0(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x7)(st))(st)
c__case_1_case__35 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_1_case__35(x1)(x)(st))(i)(xs)(st)
c__case_1_case__35 x1 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_1_case__35")(x)



c__case_2_case__36 x1 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_LT)(st)
c__case_2_case__36 x1 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleTime.c__case_1(x4)(x5)(Curry.Module.OraclePrelude.op_62(x4)(x5)(x1)(st))(x7)(st))(st)
c__case_2_case__36 x1 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_2_case__36(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_2_case__36 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_2_case__36")(x)



c__case_3_case__37 x1 x4 x3@(Curry.Module.Time.C_CTime x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleTime.c__case_2(x4)(x5)(Curry.Module.OraclePrelude.op_60(x4)(x5)(x1)(st))(x6)(st))(st)
c__case_3_case__37 x1 x4 (Curry.Module.Time.C_ClockTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_3_case__37(x1)(x4)(x)(st))(i)(xs)(st)
c__case_3_case__37 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_3_case__37")(x)



c__case_4_case__38 x1 x3 x2@(Curry.Module.Time.C_CTime x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleTime.c__case_3(x4)(x3)(x1)(st))(st)
c__case_4_case__38 x1 x3 (Curry.Module.Time.C_ClockTimeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleTime.c__case_4_case__38(x1)(x3)(x)(st))(i)(xs)(st)
c__case_4_case__38 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleTime._case_4_case__38")(x)


