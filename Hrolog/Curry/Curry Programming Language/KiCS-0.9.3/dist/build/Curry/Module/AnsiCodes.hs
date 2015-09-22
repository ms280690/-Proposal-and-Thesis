{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.AnsiCodes (module Curry.Module.AnsiCodes) where

import Curry.RunTimeSystem
import Curry.Module.Char
import Curry.Module.List
import Curry.Module.Prelude



-- begin included



-- end included

c_esc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_esc st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(st)



c_cmd :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_cmd x1 st = (Curry.Module.Prelude.:<)(Curry.Module.AnsiCodes.c_esc(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))(Curry.Module.Prelude.List))(x1)(st))



c_cursorPos :: (Curry t0,Curry t1) => t0 -> t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_cursorPos x1 x2 st = Curry.Module.AnsiCodes.c_cmd(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_show(x1)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_show(x2)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('H'))(Curry.Module.Prelude.List))(st))(st))(st))(st)



c_cursorHome :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_cursorHome st = Curry.Module.AnsiCodes.c_cmd((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('H'))(Curry.Module.Prelude.List))(st)



c_moveCursor :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_moveCursor x1 x2 st = Curry.Module.AnsiCodes.c_cmd(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_show(x2)(st))(x1)(st))(st)



c_cursorUp :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_cursorUp st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_moveCursor((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))(Curry.Module.Prelude.List)))



c_cursorDown :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_cursorDown st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_moveCursor((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('B'))(Curry.Module.Prelude.List)))



c_cursorFwd :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_cursorFwd st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_moveCursor((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))(Curry.Module.Prelude.List)))



c_cursorBack :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_cursorBack st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_moveCursor((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))(Curry.Module.Prelude.List)))



c_saveCursor :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_saveCursor st = Curry.Module.AnsiCodes.c_cmd((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))(st)



c_restoreCursor :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_restoreCursor st = Curry.Module.AnsiCodes.c_cmd((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))(Curry.Module.Prelude.List))(st)



c_clear :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_clear st = Curry.Module.AnsiCodes.c_cmd((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('2'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('J'))(Curry.Module.Prelude.List)))(st)



c_eraseLine :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_eraseLine st = Curry.Module.AnsiCodes.c_cmd((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('K'))(Curry.Module.Prelude.List))(st)



c_mode :: (Curry t0) => t0 -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_mode x1 x2 st = let {x3 = Curry.Module.AnsiCodes.c_cmd((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('0'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))(Curry.Module.Prelude.List)))(st)} in Curry.Module.AnsiCodes.c_cmd(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_show(x1)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x2)(Curry.Module.AnsiCodes.c_mode_case_3(x2)(x3)(Curry.Module.Prelude.c_apply(Curry.Module.AnsiCodes.c_isSuffixOf(x3)(st))(x2)(st))(st))(st))(st))(st))(st)



c_isSuffixOf :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)
c_isSuffixOf x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.List.c_isPrefixOf(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_reverse(st))(x1)(st))))(Curry.Module.Prelude.c_reverse(st))(st)



c_bold :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_bold st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi)))



c_underline :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_underline st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))



c_revColors :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_revColors st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))



c_concealed :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_concealed st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))



c_black :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_black st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))



c_red :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_red st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))))))



c_green :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_green st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))



c_yellow :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_yellow st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))



c_blue :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_blue st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))



c_magenta :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_magenta st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))



c_cyan :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_cyan st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))



c_white :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_white st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))



c_bgBlack :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_bgBlack st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))



c_bgRed :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_bgRed st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))



c_bgGreen :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_bgGreen st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))



c_bgYellow :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_bgYellow st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))



c_bgBlue :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_bgBlue st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))



c_bgMagenta :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_bgMagenta st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))



c_bgCyan :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_bgCyan st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))



c_bgWhite :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_bgWhite st = Curry.Module.Prelude.pf(Curry.Module.AnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))



c_ansiLength :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ansiLength x1 st = Curry.Module.AnsiCodes.c_ansiLength'46aux'4695(x1)(Curry.Module.Prelude.c_length(x1)(st))(st)



c_ansiLength'46aux'4695 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ansiLength'46aux'4695 x1@Curry.Module.Prelude.List x2 st = x2
c_ansiLength'46aux'4695 x1@((Curry.Module.Prelude.:<) x3 x4) x2 st = Curry.Module.AnsiCodes.c_ansiLength'46aux'4695_case_2(x2)(x3)(x4)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x3)(Curry.Module.AnsiCodes.c_esc(st))(st))(Curry.Module.Char.c_isDigit(Curry.Module.Prelude.op_33_33(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(st))(st))(st)
c_ansiLength'46aux'4695 (Curry.Module.Prelude.ListOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AnsiCodes.c_ansiLength'46aux'4695(x)(x2)(st))(i)(xs)(st)
c_ansiLength'46aux'4695 x x2 st = Curry.RunTimeSystem.patternFail("AnsiCodes.ansiLength.aux.95")(x)



c_ansiLength'46aux'4695_case_2 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.AnsiCodes.c_ansiLength'46aux'4695(Curry.Module.Prelude.c_tail(Curry.Module.Prelude.c_tail(Curry.Module.Prelude.c_tail(Curry.Module.Prelude.c_tail(x4)(st))(st))(st))(st))(Curry.Module.Prelude.op_45(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))(st))(st)
c_ansiLength'46aux'4695_case_2 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.AnsiCodes.c_ansiLength'46aux'4695_case_1(x2)(x3)(x4)(Curry.Module.Prelude.op_61_61(x3)(Curry.Module.AnsiCodes.c_esc(st))(st))(st)
c_ansiLength'46aux'4695_case_2 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AnsiCodes.c_ansiLength'46aux'4695_case_2(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_ansiLength'46aux'4695_case_2 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("AnsiCodes.ansiLength.aux.95_case_2")(x)



c_ansiLength'46aux'4695_case_1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.AnsiCodes.c_ansiLength'46aux'4695(Curry.Module.Prelude.c_tail(Curry.Module.Prelude.c_tail(Curry.Module.Prelude.c_tail(x4)(st))(st))(st))(Curry.Module.Prelude.op_45(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))(st))(st)
c_ansiLength'46aux'4695_case_1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.AnsiCodes.c_ansiLength'46aux'4695_case_0(x2)(x4)(Curry.Module.Prelude.c_otherwise(st))(st)
c_ansiLength'46aux'4695_case_1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AnsiCodes.c_ansiLength'46aux'4695_case_1(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_ansiLength'46aux'4695_case_1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("AnsiCodes.ansiLength.aux.95_case_1")(x)



c_ansiLength'46aux'4695_case_0 x2 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.AnsiCodes.c_ansiLength'46aux'4695(x4)(x2)(st)
c_ansiLength'46aux'4695_case_0 x2 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AnsiCodes.c_ansiLength'46aux'4695_case_0(x2)(x4)(x)(st))(i)(xs)(st)
c_ansiLength'46aux'4695_case_0 x2 x4 x st = Curry.RunTimeSystem.patternFail("AnsiCodes.ansiLength.aux.95_case_0")(x)



c_mode_case_3 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.List
c_mode_case_3 x2 x3 x4@Curry.Module.Prelude.C_False st = x3
c_mode_case_3 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AnsiCodes.c_mode_case_3(x2)(x3)(x)(st))(i)(xs)(st)
c_mode_case_3 x2 x3 x st = Curry.RunTimeSystem.patternFail("AnsiCodes.mode_case_3")(x)


