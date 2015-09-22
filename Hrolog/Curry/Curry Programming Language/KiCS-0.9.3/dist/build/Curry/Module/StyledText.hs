{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.StyledText (module Curry.Module.StyledText) where

import Curry.RunTimeSystem
import Curry.Module.Prelude
import Curry.Module.Pretty
import Curry.Module.Char
import Curry.Module.AnsiCodes



-- begin included



-- end included

type C_ShowS = (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char

c_boldChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_boldChar st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))(st)



c_underlineChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_underlineChar st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))(st)



c_endChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_endChar st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))(st)



c_blackChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_blackChar st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(st)



c_blueChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_blueChar st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(st)



c_cyanChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_cyanChar st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(st)



c_greenChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_greenChar st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(st)



c_magentaChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_magentaChar st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(st)



c_redChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_redChar st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(st)



c_whiteChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_whiteChar st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(st)



c_yellowChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_yellowChar st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(st)



c_bgBlackChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_bgBlackChar st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(st)



c_bgBlueChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_bgBlueChar st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(st)



c_bgCyanChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_bgCyanChar st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(st)



c_bgGreenChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_bgGreenChar st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(st)



c_bgMagentaChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_bgMagentaChar st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(st)



c_bgRedChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_bgRedChar st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(st)



c_bgWhiteChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_bgWhiteChar st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(st)



c_bgYellowChar :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Char
c_bgYellowChar st = Curry.Module.Prelude.c_chr(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(st)



c_bold :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_bold st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_encl(Curry.Module.StyledText.c_boldChar(st)))



c_underline :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_underline st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_encl(Curry.Module.StyledText.c_underlineChar(st)))



c_black :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_black st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_encl(Curry.Module.StyledText.c_blackChar(st)))



c_blue :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_blue st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_encl(Curry.Module.StyledText.c_blueChar(st)))



c_cyan :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_cyan st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_encl(Curry.Module.StyledText.c_cyanChar(st)))



c_green :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_green st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_encl(Curry.Module.StyledText.c_greenChar(st)))



c_magenta :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_magenta st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_encl(Curry.Module.StyledText.c_magentaChar(st)))



c_red :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_red st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_encl(Curry.Module.StyledText.c_redChar(st)))



c_white :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_white st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_encl(Curry.Module.StyledText.c_whiteChar(st)))



c_yellow :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_yellow st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_encl(Curry.Module.StyledText.c_yellowChar(st)))



c_bgBlack :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_bgBlack st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_encl(Curry.Module.StyledText.c_bgBlackChar(st)))



c_bgBlue :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_bgBlue st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_encl(Curry.Module.StyledText.c_bgBlueChar(st)))



c_bgCyan :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_bgCyan st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_encl(Curry.Module.StyledText.c_bgCyanChar(st)))



c_bgGreen :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_bgGreen st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_encl(Curry.Module.StyledText.c_bgGreenChar(st)))



c_bgMagenta :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_bgMagenta st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_encl(Curry.Module.StyledText.c_bgMagentaChar(st)))



c_bgRed :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_bgRed st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_encl(Curry.Module.StyledText.c_bgRedChar(st)))



c_bgWhite :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_bgWhite st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_encl(Curry.Module.StyledText.c_bgWhiteChar(st)))



c_bgYellow :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_bgYellow st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_encl(Curry.Module.StyledText.c_bgYellowChar(st)))



c_encl :: Curry.Module.Prelude.C_Char -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_encl x1 x2 st = (Curry.Module.Prelude.:<)(x1)(Curry.Module.Prelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_endChar(st))(Curry.Module.Prelude.List))(st))



c_boldS :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_boldS st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_enclS(Curry.Module.StyledText.c_boldChar(st)))



c_underlineS :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_underlineS st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_enclS(Curry.Module.StyledText.c_underlineChar(st)))



c_blackS :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_blackS st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_enclS(Curry.Module.StyledText.c_blackChar(st)))



c_blueS :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_blueS st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_enclS(Curry.Module.StyledText.c_blueChar(st)))



c_cyanS :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_cyanS st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_enclS(Curry.Module.StyledText.c_cyanChar(st)))



c_greenS :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_greenS st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_enclS(Curry.Module.StyledText.c_greenChar(st)))



c_magentaS :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_magentaS st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_enclS(Curry.Module.StyledText.c_magentaChar(st)))



c_redS :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_redS st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_enclS(Curry.Module.StyledText.c_redChar(st)))



c_whiteS :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_whiteS st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_enclS(Curry.Module.StyledText.c_whiteChar(st)))



c_yellowS :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_yellowS st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_enclS(Curry.Module.StyledText.c_yellowChar(st)))



c_bgBlackS :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgBlackS st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_enclS(Curry.Module.StyledText.c_bgBlackChar(st)))



c_bgBlueS :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgBlueS st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_enclS(Curry.Module.StyledText.c_bgBlueChar(st)))



c_bgCyanS :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgCyanS st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_enclS(Curry.Module.StyledText.c_bgCyanChar(st)))



c_bgGreenS :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgGreenS st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_enclS(Curry.Module.StyledText.c_bgGreenChar(st)))



c_bgMagentaS :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgMagentaS st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_enclS(Curry.Module.StyledText.c_bgMagentaChar(st)))



c_bgRedS :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgRedS st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_enclS(Curry.Module.StyledText.c_bgRedChar(st)))



c_bgWhiteS :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgWhiteS st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_enclS(Curry.Module.StyledText.c_bgWhiteChar(st)))



c_bgYellowS :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_bgYellowS st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_enclS(Curry.Module.StyledText.c_bgYellowChar(st)))



c_enclS :: Curry.Module.Prelude.C_Char -> (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_enclS x1 x2 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc((Curry.Module.Prelude.:<)(x1)))(Curry.Module.Prelude.op_46(x2)(Curry.Module.Prelude.pc((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_endChar(st))))(st))(st)



c_boldDoc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_boldDoc st = Curry.Module.StyledText.c_enclDoc(Curry.Module.StyledText.c_boldChar(st))(st)



c_underlineDoc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_underlineDoc st = Curry.Module.StyledText.c_enclDoc(Curry.Module.StyledText.c_underlineChar(st))(st)



c_blackDoc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_blackDoc st = Curry.Module.StyledText.c_enclDoc(Curry.Module.StyledText.c_blackChar(st))(st)



c_blueDoc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_blueDoc st = Curry.Module.StyledText.c_enclDoc(Curry.Module.StyledText.c_blueChar(st))(st)



c_cyanDoc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_cyanDoc st = Curry.Module.StyledText.c_enclDoc(Curry.Module.StyledText.c_cyanChar(st))(st)



c_greenDoc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_greenDoc st = Curry.Module.StyledText.c_enclDoc(Curry.Module.StyledText.c_greenChar(st))(st)



c_magentaDoc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_magentaDoc st = Curry.Module.StyledText.c_enclDoc(Curry.Module.StyledText.c_magentaChar(st))(st)



c_redDoc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_redDoc st = Curry.Module.StyledText.c_enclDoc(Curry.Module.StyledText.c_redChar(st))(st)



c_whiteDoc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_whiteDoc st = Curry.Module.StyledText.c_enclDoc(Curry.Module.StyledText.c_whiteChar(st))(st)



c_yellowDoc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_yellowDoc st = Curry.Module.StyledText.c_enclDoc(Curry.Module.StyledText.c_yellowChar(st))(st)



c_bgBlackDoc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_bgBlackDoc st = Curry.Module.StyledText.c_enclDoc(Curry.Module.StyledText.c_bgBlackChar(st))(st)



c_bgBlueDoc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_bgBlueDoc st = Curry.Module.StyledText.c_enclDoc(Curry.Module.StyledText.c_bgBlueChar(st))(st)



c_bgCyanDoc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_bgCyanDoc st = Curry.Module.StyledText.c_enclDoc(Curry.Module.StyledText.c_bgCyanChar(st))(st)



c_bgGreenDoc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_bgGreenDoc st = Curry.Module.StyledText.c_enclDoc(Curry.Module.StyledText.c_bgGreenChar(st))(st)



c_bgMagentaDoc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_bgMagentaDoc st = Curry.Module.StyledText.c_enclDoc(Curry.Module.StyledText.c_bgMagentaChar(st))(st)



c_bgRedDoc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_bgRedDoc st = Curry.Module.StyledText.c_enclDoc(Curry.Module.StyledText.c_bgRedChar(st))(st)



c_bgWhiteDoc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_bgWhiteDoc st = Curry.Module.StyledText.c_enclDoc(Curry.Module.StyledText.c_bgWhiteChar(st))(st)



c_bgYellowDoc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_bgYellowDoc st = Curry.Module.StyledText.c_enclDoc(Curry.Module.StyledText.c_bgYellowChar(st))(st)



c_enclDoc :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_enclDoc x1 st = Curry.Module.Prelude.pf(Curry.Module.Pretty.c_enclose(Curry.Module.Pretty.c_char(x1)(st))(Curry.Module.Pretty.c_char(Curry.Module.StyledText.c_endChar(st))(st)))



c_plainText :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_plainText st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_not))(Curry.Module.Prelude.pf(Curry.Module.StyledText.c_special))(st)))



c_special :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_special x1 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(Curry.Module.Prelude.c_ord(x1)(st))(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.c_enumFromTo(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(st))(st))(st)



c_interpret :: (Curry t0,Curry t1) => (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t1 -> Curry.RunTimeSystem.State -> t1))) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t0))) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> t1))) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T3 t0 t0 t0)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> t1
c_interpret x1@((Curry.Module.Prelude.:<) x7 x8) x2 x3 x4 x5 x6 st = Curry.Module.StyledText.c_interpret_case_8(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x8)(st)
c_interpret (Curry.Module.Prelude.ListOr i xs) x2 x3 x4 x5 x6 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.StyledText.c_interpret(x)(x2)(x3)(x4)(x5)(x6)(st))(i)(xs)(st)
c_interpret x x2 x3 x4 x5 x6 st = Curry.RunTimeSystem.patternFail("StyledText.interpret")(x)



c_interpret'46_'35selFP3'35xs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_interpret'46_'35selFP3'35xs x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_interpret'46_'35selFP3'35xs (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.StyledText.c_interpret'46_'35selFP3'35xs(x)(st))(i)(xs)(st)
c_interpret'46_'35selFP3'35xs x st = Curry.RunTimeSystem.patternFail("StyledText.interpret._#selFP3#xs")(x)



c_interpret'46_'35selFP4'35ys :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_interpret'46_'35selFP4'35ys x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_interpret'46_'35selFP4'35ys (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.StyledText.c_interpret'46_'35selFP4'35ys(x)(st))(i)(xs)(st)
c_interpret'46_'35selFP4'35ys x st = Curry.RunTimeSystem.patternFail("StyledText.interpret._#selFP4#ys")(x)



c_printStyledText :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0)
c_printStyledText st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_putStrLn))(Curry.Module.StyledText.c_toAnsiString(st))(st)



c_toAnsiString :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_toAnsiString st = Curry.Module.Prelude.pf(Curry.Module.StyledText.c_interpret((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_bold(st))((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_underline(st))((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_black(st))((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_blue(st))((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_cyan(st))((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_green(st))((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_magenta(st))((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_red(st))((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_white(st))((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_yellow(st))((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_bgBlack(st))((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_bgBlue(st))((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_bgCyan(st))((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_bgGreen(st))((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_bgMagenta(st))((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_bgRed(st))((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_bgWhite(st))((Curry.Module.Prelude.:<)(Curry.Module.StyledText.c_bgYellow(st))(Curry.Module.Prelude.List)))))))))))))))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_43_43))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_46))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T3(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id)))(Curry.Module.Prelude.List)))



c_interpret_case_8 x1 x2 x3 x4 x5 x6 x7 x8@((Curry.Module.Prelude.:<) x9 x10) st = Curry.Module.StyledText.c_interpret_case_7(x1)(x2)(x3)(x4)(x6)(x7)(x9)(x10)(x5)(st)
c_interpret_case_8 x1 x2 x3 x4 x5 x6 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.StyledText.c_interpret_case_8(x1)(x2)(x3)(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c_interpret_case_8 x1 x2 x3 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("StyledText.interpret_case_8")(x)



c_interpret_case_7 x1 x2 x3 x4 x6 x7 x9 x10 x5@((Curry.Module.Prelude.:<) x11 x12) st = Curry.Module.StyledText.c_interpret_case_6(x1)(x2)(x3)(x4)(x6)(x7)(x9)(x10)(x12)(x11)(st)
c_interpret_case_7 x1 x2 x3 x4 x6 x7 x9 x10 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.StyledText.c_interpret_case_7(x1)(x2)(x3)(x4)(x6)(x7)(x9)(x10)(x)(st))(i)(xs)(st)
c_interpret_case_7 x1 x2 x3 x4 x6 x7 x9 x10 x st = Curry.RunTimeSystem.patternFail("StyledText.interpret_case_7")(x)



c_interpret_case_6 x1 x2 x3 x4 x6 x7 x9 x10 x12 x11@(Curry.Module.Prelude.T3 x13 x14 x15) st = let {x16 = Curry.Module.Prelude.op_36(x4)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x3)(x13)(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x3)(x14)(st))(x15)(st))(st))(st)} in let {x17 = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_break(Curry.Module.Prelude.pf(Curry.Module.StyledText.c_special))(st))(x6)(st)} in let {x18 = Curry.Module.StyledText.c_interpret'46_'35selFP3'35xs(x17)(st)} in let {x19 = Curry.Module.StyledText.c_interpret'46_'35selFP4'35ys(x17)(st)} in let {x20 = Curry.Module.Prelude.c_ord(Curry.Module.Prelude.c_head(x19)(st))(st)} in let {x21 = Curry.Module.Prelude.op_33_33(x10)(Curry.Module.Prelude.op_45(x20)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))(st))(st)} in Curry.Module.StyledText.c_interpret_case_5(x1)(x2)(x3)(x4)(x7)(x9)(x11)(x12)(x13)(x14)(x15)(x16)(x18)(x19)(x20)(x21)(Curry.Module.Prelude.c_null(x19)(st))(st)
c_interpret_case_6 x1 x2 x3 x4 x6 x7 x9 x10 x12 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.StyledText.c_interpret_case_6(x1)(x2)(x3)(x4)(x6)(x7)(x9)(x10)(x12)(x)(st))(i)(xs)(st)
c_interpret_case_6 x1 x2 x3 x4 x6 x7 x9 x10 x12 x st = Curry.RunTimeSystem.patternFail("StyledText.interpret_case_6")(x)



c_interpret_case_5 x1 x2 x3 x4 x7 x9 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 x22@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_apply(x16)(x18)(st)
c_interpret_case_5 x1 x2 x3 x4 x7 x9 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 x22@Curry.Module.Prelude.C_False st = Curry.Module.StyledText.c_interpret_case_4(x1)(x2)(x3)(x4)(x7)(x9)(x11)(x12)(x13)(x14)(x15)(x16)(x18)(x19)(x20)(x21)(Curry.Module.Prelude.op_61_61(x20)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))(st))(st)
c_interpret_case_5 x1 x2 x3 x4 x7 x9 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.StyledText.c_interpret_case_5(x1)(x2)(x3)(x4)(x7)(x9)(x11)(x12)(x13)(x14)(x15)(x16)(x18)(x19)(x20)(x21)(x)(st))(i)(xs)(st)
c_interpret_case_5 x1 x2 x3 x4 x7 x9 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 x st = Curry.RunTimeSystem.patternFail("StyledText.interpret_case_5")(x)



c_interpret_case_4 x1 x2 x3 x4 x7 x9 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 x22@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.c_apply(x2)(Curry.Module.Prelude.c_apply(x16)(x18)(st))(st))(Curry.Module.StyledText.c_interpret(x1)(x2)(x3)(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T3(x7)(x14)(x15))((Curry.Module.Prelude.:<)(x11)(x12)))(Curry.Module.Prelude.c_tail(x19)(st))(st))(st)
c_interpret_case_4 x1 x2 x3 x4 x7 x9 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 x22@Curry.Module.Prelude.C_False st = Curry.Module.StyledText.c_interpret_case_3(x1)(x2)(x3)(x4)(x9)(x11)(x12)(x13)(x14)(x15)(x16)(x18)(x19)(x20)(x21)(Curry.Module.Prelude.op_61_61(x20)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))(st))(st)
c_interpret_case_4 x1 x2 x3 x4 x7 x9 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.StyledText.c_interpret_case_4(x1)(x2)(x3)(x4)(x7)(x9)(x11)(x12)(x13)(x14)(x15)(x16)(x18)(x19)(x20)(x21)(x)(st))(i)(xs)(st)
c_interpret_case_4 x1 x2 x3 x4 x7 x9 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 x st = Curry.RunTimeSystem.patternFail("StyledText.interpret_case_4")(x)



c_interpret_case_3 x1 x2 x3 x4 x9 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 x22@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.c_apply(x2)(Curry.Module.Prelude.c_apply(x16)(x18)(st))(st))(Curry.Module.StyledText.c_interpret(x1)(x2)(x3)(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T3(x9)(x14)(x15))((Curry.Module.Prelude.:<)(x11)(x12)))(Curry.Module.Prelude.c_tail(x19)(st))(st))(st)
c_interpret_case_3 x1 x2 x3 x4 x9 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 x22@Curry.Module.Prelude.C_False st = Curry.Module.StyledText.c_interpret_case_2(x1)(x2)(x3)(x4)(x11)(x12)(x13)(x14)(x15)(x16)(x18)(x19)(x20)(x21)(Curry.Module.Prelude.op_61_61(x20)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))(st))(st)
c_interpret_case_3 x1 x2 x3 x4 x9 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.StyledText.c_interpret_case_3(x1)(x2)(x3)(x4)(x9)(x11)(x12)(x13)(x14)(x15)(x16)(x18)(x19)(x20)(x21)(x)(st))(i)(xs)(st)
c_interpret_case_3 x1 x2 x3 x4 x9 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 x st = Curry.RunTimeSystem.patternFail("StyledText.interpret_case_3")(x)



c_interpret_case_2 x1 x2 x3 x4 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 x22@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.c_apply(x2)(Curry.Module.Prelude.c_apply(x16)(x18)(st))(st))(Curry.Module.StyledText.c_interpret(x1)(x2)(x3)(x4)(x12)(Curry.Module.Prelude.c_tail(x19)(st))(st))(st)
c_interpret_case_2 x1 x2 x3 x4 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 x22@Curry.Module.Prelude.C_False st = Curry.Module.StyledText.c_interpret_case_1(x1)(x2)(x3)(x4)(x11)(x12)(x13)(x14)(x15)(x16)(x18)(x19)(x20)(x21)(Curry.Module.Prelude.op_60(x20)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(st))(st)
c_interpret_case_2 x1 x2 x3 x4 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.StyledText.c_interpret_case_2(x1)(x2)(x3)(x4)(x11)(x12)(x13)(x14)(x15)(x16)(x18)(x19)(x20)(x21)(x)(st))(i)(xs)(st)
c_interpret_case_2 x1 x2 x3 x4 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 x st = Curry.RunTimeSystem.patternFail("StyledText.interpret_case_2")(x)



c_interpret_case_1 x1 x2 x3 x4 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 x22@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.c_apply(x2)(Curry.Module.Prelude.c_apply(x16)(x18)(st))(st))(Curry.Module.StyledText.c_interpret(x1)(x2)(x3)(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T3(x13)(x21)(x15))((Curry.Module.Prelude.:<)(x11)(x12)))(Curry.Module.Prelude.c_tail(x19)(st))(st))(st)
c_interpret_case_1 x1 x2 x3 x4 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 x22@Curry.Module.Prelude.C_False st = Curry.Module.StyledText.c_interpret_case_0(x1)(x2)(x3)(x4)(x11)(x12)(x13)(x14)(x16)(x18)(x19)(x21)(Curry.Module.Prelude.c_otherwise(st))(st)
c_interpret_case_1 x1 x2 x3 x4 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.StyledText.c_interpret_case_1(x1)(x2)(x3)(x4)(x11)(x12)(x13)(x14)(x15)(x16)(x18)(x19)(x20)(x21)(x)(st))(i)(xs)(st)
c_interpret_case_1 x1 x2 x3 x4 x11 x12 x13 x14 x15 x16 x18 x19 x20 x21 x st = Curry.RunTimeSystem.patternFail("StyledText.interpret_case_1")(x)



c_interpret_case_0 x1 x2 x3 x4 x11 x12 x13 x14 x16 x18 x19 x21 x22@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.c_apply(x2)(Curry.Module.Prelude.c_apply(x16)(x18)(st))(st))(Curry.Module.StyledText.c_interpret(x1)(x2)(x3)(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T3(x13)(x14)(x21))((Curry.Module.Prelude.:<)(x11)(x12)))(Curry.Module.Prelude.c_tail(x19)(st))(st))(st)
c_interpret_case_0 x1 x2 x3 x4 x11 x12 x13 x14 x16 x18 x19 x21 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.StyledText.c_interpret_case_0(x1)(x2)(x3)(x4)(x11)(x12)(x13)(x14)(x16)(x18)(x19)(x21)(x)(st))(i)(xs)(st)
c_interpret_case_0 x1 x2 x3 x4 x11 x12 x13 x14 x16 x18 x19 x21 x st = Curry.RunTimeSystem.patternFail("StyledText.interpret_case_0")(x)


