{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleAnsiCodes (module Curry.Module.OracleAnsiCodes) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.AnsiCodes
import Curry.Module.Char
import Curry.Module.List
import Curry.Module.Prelude
import Curry.Module.OracleChar
import Curry.Module.OracleList
import Curry.Module.OraclePrelude



-- begin included



-- end included

c_esc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_esc x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(x1)(st))(st)



c_cmd :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_cmd x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.OracleAnsiCodes.c_esc(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))(Curry.Module.Prelude.List))(x2)(x3)(st)))(st)



c_cursorPos :: (Curry t0,Curry t1) => t0 -> t1 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_cursorPos x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))))))(Curry.Module.OracleAnsiCodes.c_cmd(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x3)(x4)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('H'))(Curry.Module.Prelude.List))(x5)(st))(x6)(st))(x7)(st))(x8)(st))(st)



c_cursorHome :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_cursorHome x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAnsiCodes.c_cmd((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('H'))(Curry.Module.Prelude.List))(x1)(st))(st)



c_moveCursor :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_moveCursor x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OracleAnsiCodes.c_cmd(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x3)(x1)(st))(x2)(x4)(st))(x5)(st))(st)



c_cursorUp :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_cursorUp x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_moveCursor((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))(Curry.Module.Prelude.List))))))(st)



c_cursorDown :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_cursorDown x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_moveCursor((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('B'))(Curry.Module.Prelude.List))))))(st)



c_cursorFwd :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_cursorFwd x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_moveCursor((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))(Curry.Module.Prelude.List))))))(st)



c_cursorBack :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_cursorBack x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_moveCursor((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))(Curry.Module.Prelude.List))))))(st)



c_saveCursor :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_saveCursor x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAnsiCodes.c_cmd((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))(x1)(st))(st)



c_restoreCursor :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_restoreCursor x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAnsiCodes.c_cmd((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))(Curry.Module.Prelude.List))(x1)(st))(st)



c_clear :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_clear x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAnsiCodes.c_cmd((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('2'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('J'))(Curry.Module.Prelude.List)))(x1)(st))(st)



c_eraseLine :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_eraseLine x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAnsiCodes.c_cmd((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('K'))(Curry.Module.Prelude.List))(x1)(st))(st)



c_mode :: (Curry t0) => t0 -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_mode x2 x3 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(let {x4 = Curry.Module.OracleAnsiCodes.c_cmd((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('0'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))(Curry.Module.Prelude.List)))(x1)(st)} in let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))))))(Curry.Module.OracleAnsiCodes.c_cmd(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x2)(x5)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x3)(Curry.Module.OracleAnsiCodes.c__case_4(x3)(x4)(Curry.Module.Oracle.c_apply(Curry.Module.OracleAnsiCodes.c_isSuffixOf(x4)(x6)(st))(x3)(x7)(st))(x8)(st))(x9)(st))(x10)(st))(x11)(st))(x12)(st))(st))(st)



c_isSuffixOf :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool))
c_isSuffixOf x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleList.c_isPrefixOf(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_reverse(x1)(st))(x2)(x3)(st))))))(Curry.Module.OraclePrelude.c_reverse(x4)(st))(x5)(st))(st)



c_bold :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bold x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))))))(st)



c_underline :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_underline x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))(st)



c_revColors :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_revColors x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))))(st)



c_concealed :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_concealed x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))))(st)



c_black :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_black x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))))))(st)



c_red :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_red x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))))))(st)



c_green :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_green x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))))))(st)



c_yellow :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_yellow x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))))))(st)



c_blue :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_blue x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))))))(st)



c_magenta :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_magenta x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))))))(st)



c_cyan :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_cyan x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))))))(st)



c_white :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_white x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))))))(st)



c_bgBlack :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgBlack x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))))))(st)



c_bgRed :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgRed x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))))))(st)



c_bgGreen :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgGreen x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))))))(st)



c_bgYellow :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgYellow x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))))))(st)



c_bgBlue :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgBlue x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))))))(st)



c_bgMagenta :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgMagenta x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))))))(st)



c_bgCyan :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgCyan x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))))))(st)



c_bgWhite :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgWhite x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleAnsiCodes.c_mode(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))))))))))(st)



c_ansiLength :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ansiLength x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OracleAnsiCodes.c_ansiLength'46aux'4695(x2)(Curry.Module.OraclePrelude.c_length(x2)(x1)(st))(x3)(st))(st)



c_ansiLength'46aux'4695 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_ansiLength'46aux'4695 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAnsiCodes.c__case_3(x3)(x2)(x1)(st))(st)



c__case_3 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAnsiCodes.c__case_3_case__4(x1)(x3)(x2)(st))(st)



c__case_2 x3 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAnsiCodes.c__case_2_case__3(x1)(x3)(x4)(x5)(x6)(st))(st)



c__case_1 x3 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAnsiCodes.c__case_1_case__2(x1)(x3)(x5)(x6)(st))(st)



c__case_0 x3 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAnsiCodes.c__case_0_case__1(x1)(x3)(x5)(x6)(st))(st)



c__case_4 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAnsiCodes.c__case_4_case__0(x1)(x4)(x5)(st))(st)



c__case_4_case__0 x1 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_4_case__0 x1 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_4_case__0 x1 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAnsiCodes.c__case_4_case__0(x1)(x4)(x)(st))(i)(xs)(st)
c__case_4_case__0 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleAnsiCodes._case_4_case__0")(x)



c__case_0_case__1 x1 x3 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleAnsiCodes.c_ansiLength'46aux'4695(x5)(x3)(x1)(st))(st)
c__case_0_case__1 x1 x3 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_0_case__1 x1 x3 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAnsiCodes.c__case_0_case__1(x1)(x3)(x5)(x)(st))(i)(xs)(st)
c__case_0_case__1 x1 x3 x5 x st = Curry.RunTimeSystem.patternFail("OracleAnsiCodes._case_0_case__1")(x)



c__case_1_case__2 x1 x3 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))))(Curry.Module.OracleAnsiCodes.c_ansiLength'46aux'4695(Curry.Module.OraclePrelude.c_tail(Curry.Module.OraclePrelude.c_tail(Curry.Module.OraclePrelude.c_tail(x5)(x1)(st))(x7)(st))(x8)(st))(Curry.Module.OraclePrelude.op_45(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))(x9)(st))(x10)(st))(st)
c__case_1_case__2 x1 x3 x5 x6@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleAnsiCodes.c__case_0(x3)(x5)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x11)(st))(st)
c__case_1_case__2 x1 x3 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAnsiCodes.c__case_1_case__2(x1)(x3)(x5)(x)(st))(i)(xs)(st)
c__case_1_case__2 x1 x3 x5 x st = Curry.RunTimeSystem.patternFail("OracleAnsiCodes._case_1_case__2")(x)



c__case_2_case__3 x1 x3 x4 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))))))(Curry.Module.OracleAnsiCodes.c_ansiLength'46aux'4695(Curry.Module.OraclePrelude.c_tail(Curry.Module.OraclePrelude.c_tail(Curry.Module.OraclePrelude.c_tail(Curry.Module.OraclePrelude.c_tail(x5)(x1)(st))(x7)(st))(x8)(st))(x9)(st))(Curry.Module.OraclePrelude.op_45(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))(x10)(st))(x11)(st))(st)
c__case_2_case__3 x1 x3 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List)))(Curry.Module.OracleAnsiCodes.c__case_1(x3)(x4)(x5)(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.OracleAnsiCodes.c_esc(x1)(st))(x12)(st))(x13)(st))(st)
c__case_2_case__3 x1 x3 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAnsiCodes.c__case_2_case__3(x1)(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_2_case__3 x1 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleAnsiCodes._case_2_case__3")(x)



c__case_3_case__4 x1 x3 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_3_case__4 x1 x3 x2@((Curry.Module.Prelude.:<) x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))))))(Curry.Module.OracleAnsiCodes.c__case_2(x3)(x4)(x5)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.OracleAnsiCodes.c_esc(x1)(st))(x6)(st))(Curry.Module.OracleChar.c_isDigit(Curry.Module.OraclePrelude.op_33_33(x5)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x7)(st))(x8)(st))(x9)(st))(x10)(st))(st)
c__case_3_case__4 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleAnsiCodes.c__case_3_case__4(x1)(x3)(x)(st))(i)(xs)(st)
c__case_3_case__4 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleAnsiCodes._case_3_case__4")(x)


