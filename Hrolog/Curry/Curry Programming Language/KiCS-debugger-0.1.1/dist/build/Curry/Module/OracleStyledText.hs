{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleStyledText (module Curry.Module.OracleStyledText) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.StyledText
import Curry.Module.AnsiCodes
import Curry.Module.Prelude
import Curry.Module.Pretty
import Curry.Module.Char
import Curry.Module.OracleAnsiCodes
import Curry.Module.OraclePrelude
import Curry.Module.OraclePretty
import Curry.Module.OracleChar



-- begin included



-- end included

c_boldChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_boldChar x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))(x1)(st))(st)



c_underlineChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_underlineChar x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))(x1)(st))(st)



c_endChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_endChar x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))(x1)(st))(st)



c_blackChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_blackChar x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(x1)(st))(st)



c_blueChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_blueChar x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(x1)(st))(st)



c_cyanChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_cyanChar x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(x1)(st))(st)



c_greenChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_greenChar x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(x1)(st))(st)



c_magentaChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_magentaChar x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(x1)(st))(st)



c_redChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_redChar x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(x1)(st))(st)



c_whiteChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_whiteChar x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(x1)(st))(st)



c_yellowChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_yellowChar x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(x1)(st))(st)



c_bgBlackChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_bgBlackChar x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(x1)(st))(st)



c_bgBlueChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_bgBlueChar x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(x1)(st))(st)



c_bgCyanChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_bgCyanChar x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(x1)(st))(st)



c_bgGreenChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_bgGreenChar x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(x1)(st))(st)



c_bgMagentaChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_bgMagentaChar x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(x1)(st))(st)



c_bgRedChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_bgRedChar x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(x1)(st))(st)



c_bgWhiteChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_bgWhiteChar x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(x1)(st))(st)



c_bgYellowChar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_bgYellowChar x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(x1)(st))(st)



c_bold :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bold x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_encl(Curry.Module.OracleStyledText.c_boldChar(x1)(st))))))(st)



c_underline :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_underline x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_encl(Curry.Module.OracleStyledText.c_underlineChar(x1)(st))))))(st)



c_black :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_black x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_encl(Curry.Module.OracleStyledText.c_blackChar(x1)(st))))))(st)



c_blue :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_blue x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_encl(Curry.Module.OracleStyledText.c_blueChar(x1)(st))))))(st)



c_cyan :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_cyan x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_encl(Curry.Module.OracleStyledText.c_cyanChar(x1)(st))))))(st)



c_green :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_green x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_encl(Curry.Module.OracleStyledText.c_greenChar(x1)(st))))))(st)



c_magenta :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_magenta x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_encl(Curry.Module.OracleStyledText.c_magentaChar(x1)(st))))))(st)



c_red :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_red x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_encl(Curry.Module.OracleStyledText.c_redChar(x1)(st))))))(st)



c_white :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_white x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_encl(Curry.Module.OracleStyledText.c_whiteChar(x1)(st))))))(st)



c_yellow :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_yellow x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_encl(Curry.Module.OracleStyledText.c_yellowChar(x1)(st))))))(st)



c_bgBlack :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgBlack x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_encl(Curry.Module.OracleStyledText.c_bgBlackChar(x1)(st))))))(st)



c_bgBlue :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgBlue x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_encl(Curry.Module.OracleStyledText.c_bgBlueChar(x1)(st))))))(st)



c_bgCyan :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgCyan x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_encl(Curry.Module.OracleStyledText.c_bgCyanChar(x1)(st))))))(st)



c_bgGreen :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgGreen x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_encl(Curry.Module.OracleStyledText.c_bgGreenChar(x1)(st))))))(st)



c_bgMagenta :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgMagenta x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_encl(Curry.Module.OracleStyledText.c_bgMagentaChar(x1)(st))))))(st)



c_bgRed :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgRed x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_encl(Curry.Module.OracleStyledText.c_bgRedChar(x1)(st))))))(st)



c_bgWhite :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgWhite x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_encl(Curry.Module.OracleStyledText.c_bgWhiteChar(x1)(st))))))(st)



c_bgYellow :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgYellow x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_encl(Curry.Module.OracleStyledText.c_bgYellowChar(x1)(st))))))(st)



c_encl :: Curry.Module.Prelude.C_Char -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_encl x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(x2)(Curry.Module.OraclePrelude.op_43_43(x3)((Curry.Module.Prelude.:<)(Curry.Module.OracleStyledText.c_endChar(x1)(st))(Curry.Module.Prelude.List))(x4)(st)))(st)



c_boldS :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_boldS x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_enclS(Curry.Module.OracleStyledText.c_boldChar(x1)(st))))))(st)



c_underlineS :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_underlineS x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_enclS(Curry.Module.OracleStyledText.c_underlineChar(x1)(st))))))(st)



c_blackS :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_blackS x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_enclS(Curry.Module.OracleStyledText.c_blackChar(x1)(st))))))(st)



c_blueS :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_blueS x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_enclS(Curry.Module.OracleStyledText.c_blueChar(x1)(st))))))(st)



c_cyanS :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_cyanS x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_enclS(Curry.Module.OracleStyledText.c_cyanChar(x1)(st))))))(st)



c_greenS :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_greenS x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_enclS(Curry.Module.OracleStyledText.c_greenChar(x1)(st))))))(st)



c_magentaS :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_magentaS x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_enclS(Curry.Module.OracleStyledText.c_magentaChar(x1)(st))))))(st)



c_redS :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_redS x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_enclS(Curry.Module.OracleStyledText.c_redChar(x1)(st))))))(st)



c_whiteS :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_whiteS x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_enclS(Curry.Module.OracleStyledText.c_whiteChar(x1)(st))))))(st)



c_yellowS :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_yellowS x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_enclS(Curry.Module.OracleStyledText.c_yellowChar(x1)(st))))))(st)



c_bgBlackS :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_bgBlackS x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_enclS(Curry.Module.OracleStyledText.c_bgBlackChar(x1)(st))))))(st)



c_bgBlueS :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_bgBlueS x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_enclS(Curry.Module.OracleStyledText.c_bgBlueChar(x1)(st))))))(st)



c_bgCyanS :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_bgCyanS x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_enclS(Curry.Module.OracleStyledText.c_bgCyanChar(x1)(st))))))(st)



c_bgGreenS :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_bgGreenS x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_enclS(Curry.Module.OracleStyledText.c_bgGreenChar(x1)(st))))))(st)



c_bgMagentaS :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_bgMagentaS x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_enclS(Curry.Module.OracleStyledText.c_bgMagentaChar(x1)(st))))))(st)



c_bgRedS :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_bgRedS x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_enclS(Curry.Module.OracleStyledText.c_bgRedChar(x1)(st))))))(st)



c_bgWhiteS :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_bgWhiteS x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_enclS(Curry.Module.OracleStyledText.c_bgWhiteChar(x1)(st))))))(st)



c_bgYellowS :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))))
c_bgYellowS x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_enclS(Curry.Module.OracleStyledText.c_bgYellowChar(x1)(st))))))(st)



c_enclS :: Curry.Module.Prelude.C_Char -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_enclS x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc((Curry.Module.Prelude.:<)(x2)))))(Curry.Module.OraclePrelude.op_46(x3)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc((Curry.Module.Prelude.:<)(Curry.Module.OracleStyledText.c_endChar(x1)(st))))))(x4)(st))(x5)(st))(st)



c_boldDoc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_boldDoc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c_enclDoc(Curry.Module.OracleStyledText.c_boldChar(x1)(st))(x2)(st))(st)



c_underlineDoc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_underlineDoc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c_enclDoc(Curry.Module.OracleStyledText.c_underlineChar(x1)(st))(x2)(st))(st)



c_blackDoc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_blackDoc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c_enclDoc(Curry.Module.OracleStyledText.c_blackChar(x1)(st))(x2)(st))(st)



c_blueDoc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_blueDoc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c_enclDoc(Curry.Module.OracleStyledText.c_blueChar(x1)(st))(x2)(st))(st)



c_cyanDoc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_cyanDoc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c_enclDoc(Curry.Module.OracleStyledText.c_cyanChar(x1)(st))(x2)(st))(st)



c_greenDoc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_greenDoc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c_enclDoc(Curry.Module.OracleStyledText.c_greenChar(x1)(st))(x2)(st))(st)



c_magentaDoc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_magentaDoc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c_enclDoc(Curry.Module.OracleStyledText.c_magentaChar(x1)(st))(x2)(st))(st)



c_redDoc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_redDoc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c_enclDoc(Curry.Module.OracleStyledText.c_redChar(x1)(st))(x2)(st))(st)



c_whiteDoc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_whiteDoc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c_enclDoc(Curry.Module.OracleStyledText.c_whiteChar(x1)(st))(x2)(st))(st)



c_yellowDoc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_yellowDoc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c_enclDoc(Curry.Module.OracleStyledText.c_yellowChar(x1)(st))(x2)(st))(st)



c_bgBlackDoc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_bgBlackDoc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c_enclDoc(Curry.Module.OracleStyledText.c_bgBlackChar(x1)(st))(x2)(st))(st)



c_bgBlueDoc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_bgBlueDoc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c_enclDoc(Curry.Module.OracleStyledText.c_bgBlueChar(x1)(st))(x2)(st))(st)



c_bgCyanDoc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_bgCyanDoc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c_enclDoc(Curry.Module.OracleStyledText.c_bgCyanChar(x1)(st))(x2)(st))(st)



c_bgGreenDoc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_bgGreenDoc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c_enclDoc(Curry.Module.OracleStyledText.c_bgGreenChar(x1)(st))(x2)(st))(st)



c_bgMagentaDoc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_bgMagentaDoc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c_enclDoc(Curry.Module.OracleStyledText.c_bgMagentaChar(x1)(st))(x2)(st))(st)



c_bgRedDoc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_bgRedDoc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c_enclDoc(Curry.Module.OracleStyledText.c_bgRedChar(x1)(st))(x2)(st))(st)



c_bgWhiteDoc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_bgWhiteDoc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c_enclDoc(Curry.Module.OracleStyledText.c_bgWhiteChar(x1)(st))(x2)(st))(st)



c_bgYellowDoc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_bgYellowDoc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c_enclDoc(Curry.Module.OracleStyledText.c_bgYellowChar(x1)(st))(x2)(st))(st)



c_enclDoc :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_enclDoc x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_enclose(Curry.Module.OraclePretty.c_char(x2)(x1)(st))(Curry.Module.OraclePretty.c_char(Curry.Module.OracleStyledText.c_endChar(x3)(st))(x4)(st))))))(st)



c_plainText :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_plainText x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_filter(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_not))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_special))))(x1)(st))))))(st)



c_special :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_special x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_elem(Curry.Module.OraclePrelude.c_ord(x2)(x1)(st))(x3)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.c_enumFromTo(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(x4)(st))(x5)(st))(x6)(st))(st)



c_interpret :: (Curry t0,Curry t1) => (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t1))))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0))))) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> t1))))) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T3 t0 t0 t0)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t1
c_interpret x2 x3 x4 x5 x6 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c__case_11(x3)(x4)(x5)(x6)(x7)(x2)(x1)(st))(st)



c_interpret'46_'35selFP3'35xs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_interpret'46_'35selFP3'35xs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c__case_1(x2)(x1)(st))(st)



c_interpret'46_'35selFP4'35ys :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_interpret'46_'35selFP4'35ys x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c__case_0(x2)(x1)(st))(st)



c_printStyledText :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))))
c_printStyledText x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_putStrLn))))(Curry.Module.OracleStyledText.c_toAnsiString(x1)(st))(x2)(st))(st)



c_toAnsiString :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_toAnsiString x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))))))))))))))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_interpret((Curry.Module.Prelude.:<)(Curry.Module.OracleAnsiCodes.c_bold(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleAnsiCodes.c_underline(x2)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleAnsiCodes.c_black(x3)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleAnsiCodes.c_blue(x4)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleAnsiCodes.c_cyan(x5)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleAnsiCodes.c_green(x6)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleAnsiCodes.c_magenta(x7)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleAnsiCodes.c_red(x8)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleAnsiCodes.c_white(x9)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleAnsiCodes.c_yellow(x10)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleAnsiCodes.c_bgBlack(x11)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleAnsiCodes.c_bgBlue(x12)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleAnsiCodes.c_bgCyan(x13)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleAnsiCodes.c_bgGreen(x14)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleAnsiCodes.c_bgMagenta(x15)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleAnsiCodes.c_bgRed(x16)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleAnsiCodes.c_bgWhite(x17)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleAnsiCodes.c_bgYellow(x18)(st))(Curry.Module.Prelude.List)))))))))))))))))))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_43_43))(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_46))(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_id))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T3(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_id))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_id))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_id)))))(Curry.Module.Prelude.List))))))(st)



c__case_0 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c__case_0_case__11(x1)(x2)(st))(st)



c__case_1 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c__case_1_case__10(x1)(x2)(st))(st)



c__case_11 x3 x4 x5 x6 x7 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c__case_11_case__9(x1)(x3)(x4)(x5)(x6)(x7)(x2)(st))(st)



c__case_10 x2 x3 x4 x5 x6 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c__case_10_case__8(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(st))(st)



c__case_9 x2 x3 x4 x5 x7 x8 x10 x11 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c__case_9_case__7(x1)(x2)(x3)(x4)(x5)(x7)(x8)(x10)(x11)(x6)(st))(st)



c__case_8 x2 x3 x4 x5 x7 x8 x10 x11 x13 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c__case_8_case__6(x1)(x2)(x3)(x4)(x5)(x7)(x8)(x10)(x11)(x13)(x12)(st))(st)



c__case_7 x2 x3 x4 x5 x8 x10 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c__case_7_case__5(x1)(x2)(x3)(x4)(x5)(x8)(x10)(x12)(x13)(x14)(x15)(x16)(x17)(x19)(x20)(x21)(x22)(x23)(st))(st)



c__case_6 x2 x3 x4 x5 x8 x10 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c__case_6_case__4(x1)(x2)(x3)(x4)(x5)(x8)(x10)(x12)(x13)(x14)(x15)(x16)(x17)(x19)(x20)(x21)(x22)(x23)(st))(st)



c__case_5 x2 x3 x4 x5 x10 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c__case_5_case__3(x1)(x2)(x3)(x4)(x5)(x10)(x12)(x13)(x14)(x15)(x16)(x17)(x19)(x20)(x21)(x22)(x23)(st))(st)



c__case_4 x2 x3 x4 x5 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c__case_4_case__2(x1)(x2)(x3)(x4)(x5)(x12)(x13)(x14)(x15)(x16)(x17)(x19)(x20)(x21)(x22)(x23)(st))(st)



c__case_3 x2 x3 x4 x5 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c__case_3_case__1(x1)(x2)(x3)(x4)(x5)(x12)(x13)(x14)(x15)(x16)(x17)(x19)(x20)(x22)(x23)(st))(st)



c__case_2 x2 x3 x4 x5 x12 x13 x14 x15 x17 x19 x20 x22 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c__case_2_case__0(x1)(x2)(x3)(x4)(x5)(x12)(x13)(x14)(x15)(x17)(x19)(x20)(x22)(x23)(st))(st)



c__case_2_case__0 x1 x2 x3 x4 x5 x12 x13 x14 x15 x17 x19 x20 x22 x23@Curry.Module.Prelude.C_True st = let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_36(Curry.Module.Oracle.c_apply(x3)(Curry.Module.Oracle.c_apply(x17)(x19)(x1)(st))(x24)(st))(Curry.Module.OracleStyledText.c_interpret(x2)(x3)(x4)(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T3(x14)(x15)(x22))((Curry.Module.Prelude.:<)(x12)(x13)))(Curry.Module.OraclePrelude.c_tail(x20)(x25)(st))(x26)(st))(x27)(st))(st)
c__case_2_case__0 x1 x2 x3 x4 x5 x12 x13 x14 x15 x17 x19 x20 x22 x23@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_2_case__0 x1 x2 x3 x4 x5 x12 x13 x14 x15 x17 x19 x20 x22 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleStyledText.c__case_2_case__0(x1)(x2)(x3)(x4)(x5)(x12)(x13)(x14)(x15)(x17)(x19)(x20)(x22)(x)(st))(i)(xs)(st)
c__case_2_case__0 x1 x2 x3 x4 x5 x12 x13 x14 x15 x17 x19 x20 x22 x st = Curry.RunTimeSystem.patternFail("OracleStyledText._case_2_case__0")(x)



c__case_3_case__1 x1 x2 x3 x4 x5 x12 x13 x14 x15 x16 x17 x19 x20 x22 x23@Curry.Module.Prelude.C_True st = let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_36(Curry.Module.Oracle.c_apply(x3)(Curry.Module.Oracle.c_apply(x17)(x19)(x1)(st))(x24)(st))(Curry.Module.OracleStyledText.c_interpret(x2)(x3)(x4)(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T3(x14)(x22)(x16))((Curry.Module.Prelude.:<)(x12)(x13)))(Curry.Module.OraclePrelude.c_tail(x20)(x25)(st))(x26)(st))(x27)(st))(st)
c__case_3_case__1 x1 x2 x3 x4 x5 x12 x13 x14 x15 x16 x17 x19 x20 x22 x23@Curry.Module.Prelude.C_False st = let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x28)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c__case_2(x2)(x3)(x4)(x5)(x12)(x13)(x14)(x15)(x17)(x19)(x20)(x22)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x28)(st))(st)
c__case_3_case__1 x1 x2 x3 x4 x5 x12 x13 x14 x15 x16 x17 x19 x20 x22 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleStyledText.c__case_3_case__1(x1)(x2)(x3)(x4)(x5)(x12)(x13)(x14)(x15)(x16)(x17)(x19)(x20)(x22)(x)(st))(i)(xs)(st)
c__case_3_case__1 x1 x2 x3 x4 x5 x12 x13 x14 x15 x16 x17 x19 x20 x22 x st = Curry.RunTimeSystem.patternFail("OracleStyledText._case_3_case__1")(x)



c__case_4_case__2 x1 x2 x3 x4 x5 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 x23@Curry.Module.Prelude.C_True st = let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_36(Curry.Module.Oracle.c_apply(x3)(Curry.Module.Oracle.c_apply(x17)(x19)(x1)(st))(x24)(st))(Curry.Module.OracleStyledText.c_interpret(x2)(x3)(x4)(x5)(x13)(Curry.Module.OraclePrelude.c_tail(x20)(x25)(st))(x26)(st))(x27)(st))(st)
c__case_4_case__2 x1 x2 x3 x4 x5 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 x23@Curry.Module.Prelude.C_False st = let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x28)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c__case_3(x2)(x3)(x4)(x5)(x12)(x13)(x14)(x15)(x16)(x17)(x19)(x20)(x21)(x22)(Curry.Module.OraclePrelude.op_60(x21)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(x1)(st))(x28)(st))(st)
c__case_4_case__2 x1 x2 x3 x4 x5 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleStyledText.c__case_4_case__2(x1)(x2)(x3)(x4)(x5)(x12)(x13)(x14)(x15)(x16)(x17)(x19)(x20)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_4_case__2 x1 x2 x3 x4 x5 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleStyledText._case_4_case__2")(x)



c__case_5_case__3 x1 x2 x3 x4 x5 x10 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 x23@Curry.Module.Prelude.C_True st = let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_36(Curry.Module.Oracle.c_apply(x3)(Curry.Module.Oracle.c_apply(x17)(x19)(x1)(st))(x24)(st))(Curry.Module.OracleStyledText.c_interpret(x2)(x3)(x4)(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T3(x10)(x15)(x16))((Curry.Module.Prelude.:<)(x12)(x13)))(Curry.Module.OraclePrelude.c_tail(x20)(x25)(st))(x26)(st))(x27)(st))(st)
c__case_5_case__3 x1 x2 x3 x4 x5 x10 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 x23@Curry.Module.Prelude.C_False st = let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x28)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c__case_4(x2)(x3)(x4)(x5)(x12)(x13)(x14)(x15)(x16)(x17)(x19)(x20)(x21)(x22)(Curry.Module.OraclePrelude.op_61_61(x21)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))(x1)(st))(x28)(st))(st)
c__case_5_case__3 x1 x2 x3 x4 x5 x10 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleStyledText.c__case_5_case__3(x1)(x2)(x3)(x4)(x5)(x10)(x12)(x13)(x14)(x15)(x16)(x17)(x19)(x20)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_5_case__3 x1 x2 x3 x4 x5 x10 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleStyledText._case_5_case__3")(x)



c__case_6_case__4 x1 x2 x3 x4 x5 x8 x10 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 x23@Curry.Module.Prelude.C_True st = let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_36(Curry.Module.Oracle.c_apply(x3)(Curry.Module.Oracle.c_apply(x17)(x19)(x1)(st))(x24)(st))(Curry.Module.OracleStyledText.c_interpret(x2)(x3)(x4)(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T3(x8)(x15)(x16))((Curry.Module.Prelude.:<)(x12)(x13)))(Curry.Module.OraclePrelude.c_tail(x20)(x25)(st))(x26)(st))(x27)(st))(st)
c__case_6_case__4 x1 x2 x3 x4 x5 x8 x10 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 x23@Curry.Module.Prelude.C_False st = let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x28)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c__case_5(x2)(x3)(x4)(x5)(x10)(x12)(x13)(x14)(x15)(x16)(x17)(x19)(x20)(x21)(x22)(Curry.Module.OraclePrelude.op_61_61(x21)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))(x1)(st))(x28)(st))(st)
c__case_6_case__4 x1 x2 x3 x4 x5 x8 x10 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleStyledText.c__case_6_case__4(x1)(x2)(x3)(x4)(x5)(x8)(x10)(x12)(x13)(x14)(x15)(x16)(x17)(x19)(x20)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_6_case__4 x1 x2 x3 x4 x5 x8 x10 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleStyledText._case_6_case__4")(x)



c__case_7_case__5 x1 x2 x3 x4 x5 x8 x10 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 x23@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Oracle.c_apply(x17)(x19)(x1)(st))(st)
c__case_7_case__5 x1 x2 x3 x4 x5 x8 x10 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 x23@Curry.Module.Prelude.C_False st = let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c__case_6(x2)(x3)(x4)(x5)(x8)(x10)(x12)(x13)(x14)(x15)(x16)(x17)(x19)(x20)(x21)(x22)(Curry.Module.OraclePrelude.op_61_61(x21)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))(x1)(st))(x24)(st))(st)
c__case_7_case__5 x1 x2 x3 x4 x5 x8 x10 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleStyledText.c__case_7_case__5(x1)(x2)(x3)(x4)(x5)(x8)(x10)(x12)(x13)(x14)(x15)(x16)(x17)(x19)(x20)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_7_case__5 x1 x2 x3 x4 x5 x8 x10 x12 x13 x14 x15 x16 x17 x19 x20 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleStyledText._case_7_case__5")(x)



c__case_8_case__6 x1 x2 x3 x4 x5 x7 x8 x10 x11 x13 x12@(Curry.Module.Prelude.T3 x14 x15 x16) st = let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List))))))(let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x27)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)(Curry.Module.Prelude.List)))(let {x18 = Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_break(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleStyledText.c_special))))(x27)(st))(x7)(x28)(st)} in let {x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x29)((Curry.Module.Prelude.:<)(x30)(Curry.Module.Prelude.List))(let {x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x30)((Curry.Module.Prelude.:<)(x31)(Curry.Module.Prelude.List))(let {x20 = Curry.Module.OracleStyledText.c_interpret'46_'35selFP4'35ys(x18)(x30)(st)} in let {x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x31)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)(Curry.Module.Prelude.List)))(let {x21 = Curry.Module.OraclePrelude.c_ord(Curry.Module.OraclePrelude.c_head(x20)(x31)(st))(x32)(st)} in let {x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x33)((Curry.Module.Prelude.:<)(x34)((Curry.Module.Prelude.:<)(x35)(Curry.Module.Prelude.List)))(let {x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x35)((Curry.Module.Prelude.:<)(x36)(Curry.Module.Prelude.List))(Curry.Module.OracleStyledText.c__case_7(x2)(x3)(x4)(x5)(x8)(x10)(x12)(x13)(x14)(x15)(x16)(Curry.Module.OraclePrelude.op_36(x5)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x4)(x14)(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x4)(x15)(x23)(st))(x16)(x24)(st))(x25)(st))(x26)(st))(Curry.Module.OracleStyledText.c_interpret'46_'35selFP3'35xs(x18)(x29)(st))(x20)(x21)(Curry.Module.OraclePrelude.op_33_33(x11)(Curry.Module.OraclePrelude.op_45(x21)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(x33)(st))(x34)(st))(Curry.Module.OraclePrelude.c_null(x20)(x35)(st))(x36)(st))(st))(st))(st))(st))(st))(st))(st)
c__case_8_case__6 x1 x2 x3 x4 x5 x7 x8 x10 x11 x13 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleStyledText.c__case_8_case__6(x1)(x2)(x3)(x4)(x5)(x7)(x8)(x10)(x11)(x13)(x)(st))(i)(xs)(st)
c__case_8_case__6 x1 x2 x3 x4 x5 x7 x8 x10 x11 x13 x st = Curry.RunTimeSystem.patternFail("OracleStyledText._case_8_case__6")(x)



c__case_9_case__7 x1 x2 x3 x4 x5 x7 x8 x10 x11 x6@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c__case_8(x2)(x3)(x4)(x5)(x7)(x8)(x10)(x11)(x13)(x12)(x1)(st))(st)
c__case_9_case__7 x1 x2 x3 x4 x5 x7 x8 x10 x11 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleStyledText.c__case_9_case__7(x1)(x2)(x3)(x4)(x5)(x7)(x8)(x10)(x11)(x)(st))(i)(xs)(st)
c__case_9_case__7 x1 x2 x3 x4 x5 x7 x8 x10 x11 x st = Curry.RunTimeSystem.patternFail("OracleStyledText._case_9_case__7")(x)



c__case_10_case__8 x1 x2 x3 x4 x5 x6 x7 x8 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c__case_9(x2)(x3)(x4)(x5)(x7)(x8)(x10)(x11)(x6)(x1)(st))(st)
c__case_10_case__8 x1 x2 x3 x4 x5 x6 x7 x8 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleStyledText.c__case_10_case__8(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_10_case__8 x1 x2 x3 x4 x5 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleStyledText._case_10_case__8")(x)



c__case_11_case__9 x1 x3 x4 x5 x6 x7 x2@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c__case_10(x2)(x3)(x4)(x5)(x6)(x7)(x8)(x9)(x1)(st))(st)
c__case_11_case__9 x1 x3 x4 x5 x6 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleStyledText.c__case_11_case__9(x1)(x3)(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_11_case__9 x1 x3 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleStyledText._case_11_case__9")(x)



c__case_1_case__10 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_1_case__10 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleStyledText.c__case_1_case__10(x1)(x)(st))(i)(xs)(st)
c__case_1_case__10 x1 x st = Curry.RunTimeSystem.patternFail("OracleStyledText._case_1_case__10")(x)



c__case_0_case__11 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_0_case__11 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleStyledText.c__case_0_case__11(x1)(x)(st))(i)(xs)(st)
c__case_0_case__11 x1 x st = Curry.RunTimeSystem.patternFail("OracleStyledText._case_0_case__11")(x)


