{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OraclePrettyFlat (module Curry.Module.OraclePrettyFlat) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.PrettyFlat
import Curry.Module.Char
import Curry.Module.FlatCurry
import Curry.Module.FlatCurryGoodies
import Curry.Module.Maybe
import Curry.Module.Prelude
import Curry.Module.Pretty
import Curry.Module.StyledText
import Curry.Module.System
import Curry.Module.OracleChar
import Curry.Module.OracleFlatCurry
import Curry.Module.OracleFlatCurryGoodies
import Curry.Module.OracleMaybe
import Curry.Module.OraclePrelude
import Curry.Module.OraclePretty
import Curry.Module.OracleStyledText
import Curry.Module.OracleSystem



-- begin included



-- end included

c_prelude :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_prelude x1 st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(st)



c_arrow :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_arrow x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_operator(x1)(st))(Curry.Module.OraclePretty.c_text((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))(Curry.Module.Prelude.List)))(x2)(st))(x3)(st))(st)



c_bar :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_bar x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_operator(x1)(st))(Curry.Module.OraclePretty.c_char(Curry.Module.Prelude.C_Char('|'))(x2)(st))(x3)(st))(st)



c_dcolon :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_dcolon x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_operator(x1)(st))(Curry.Module.OraclePretty.c_text((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List)))(x2)(st))(x3)(st))(st)



c_precFillEncloseSep :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.OraclePretty.C_Doc -> Curry.Module.OraclePretty.C_Doc -> Curry.Module.OraclePretty.C_Doc -> (Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_precFillEncloseSep x2 x3 x4 x5 x6 x7 x1 st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OraclePretty.c_fillEncloseSep(Curry.Module.OraclePrettyFlat.c_precFillEncloseSep'46pre'4611(x2)(x3)(x4)(x1)(st))(Curry.Module.OraclePrettyFlat.c_precFillEncloseSep'46pre'4611(x2)(x3)(x5)(x8)(st))(x6)(x7)(x9)(st))(st)



c_precFillEncloseSep'46pre'4611 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.OraclePretty.C_Doc -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_precFillEncloseSep'46pre'4611 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_117(x2)(x3)(x4)(Curry.Module.OraclePrelude.op_60(x2)(x3)(x1)(st))(x5)(st))(st)



c_isInfixName :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isInfixName x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_115(x2)(x1)(st))(st)



c_infixIDs :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_infixIDs x1 st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('~'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('@'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('#'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('$'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('%'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('^'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('*'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('+'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('?'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('|'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List)))))))))))))))))))))(st)



c_isTupleName :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isTupleName x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_114(x2)(x1)(st))(st)



c_showStyledProg :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_showStyledProg x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c_prettyProg(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))(x1)(st))(st)



c_prettyProg :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_prettyProg x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_pretty(x2)))))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_progDoc))))(Curry.Module.Oracle.c_apply(Curry.Module.OracleFlatCurryGoodies.c_updProgExps(x1)(st))(Curry.Module.OraclePrettyFlat.c_elimApp(x3)(st))(x4)(st))(x5)(st))(x6)(st))(st)



c_prettyTypeExpr :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_TypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_prettyTypeExpr x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_pretty(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_typeExprDoc(x2)(Curry.Module.Prelude.C_False)))))(x1)(st))(st)



c_prettyTypes :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_TypeDecl) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_prettyTypes x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_pretty(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))))))(Curry.Module.OraclePrettyFlat.c_typesDoc(x2)(x1)(st))(x3)(st))(st)



c_prettyOps :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_OpDecl) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_prettyOps x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_pretty(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_opsDoc))))(x1)(st))(st)



c_showProg :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_showProg x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_46(Curry.Module.OracleStyledText.c_plainText(x1)(st))(Curry.Module.OraclePrettyFlat.c_showStyledProg(x2)(st))(x3)(st))(st)



c_printStyledProg :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_printStyledProg x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleFlatCurry.c_readFlatCurry(x2)(x1)(st))(Curry.Module.OraclePrelude.op_46(Curry.Module.OracleStyledText.c_printStyledText(x3)(st))(Curry.Module.OraclePrettyFlat.c_showStyledProg(x4)(st))(x5)(st))(x6)(st))(st)



c_mainPrint :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_mainPrint x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleSystem.c_getArgs(x1)(st))(Curry.Module.OraclePrelude.c_mapIO_(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_printProg))))(x2)(st))(x3)(st))(st)



c_printProg :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_printProg x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleFlatCurry.c_readFlatCurryFile(x2)(x1)(st))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_putStrLn))))(Curry.Module.OraclePrettyFlat.c_showProg(x3)(st))(x4)(st))(x5)(st))(st)



c_keyword :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_keyword x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_46(Curry.Module.OracleStyledText.c_magentaDoc(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_text))))(x2)(st))(st)



c_consname :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_consname x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_46(Curry.Module.OracleStyledText.c_greenDoc(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_text))))(x2)(st))(st)



c_operator :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_operator x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c_blueDoc(x1)(st))(st)



c_literal :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_literal x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleStyledText.c_cyanDoc(x1)(st))(st)



c_marked :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_marked x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_46(Curry.Module.OracleStyledText.c_bgYellowDoc(x1)(st))(Curry.Module.OracleStyledText.c_boldDoc(x2)(st))(x3)(st))(st)



c_block :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_block x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_group))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_hang(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))))))(x1)(st))(st)



c_def :: Curry.Module.OraclePretty.C_Doc -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.OraclePretty.C_Doc -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_def x2 x3 x4 x1 st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_block(x7)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_36_62(x8)(st))(Curry.Module.OraclePretty.op_60_62(x2)(Curry.Module.OraclePrettyFlat.c__case_113(x3)(Curry.Module.OraclePrelude.c_null(x3)(x1)(st))(x6)(st))(x9)(st))(x10)(st))(x4)(x11)(st))(x12)(st))(st))(st)



c_app :: Curry.Module.OraclePretty.C_Doc -> (Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_app x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_112(x2)(x3)(Curry.Module.OraclePrelude.c_null(x3)(x1)(st))(x4)(st))(st)



c_layout :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_layout x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_46(Curry.Module.OraclePretty.c_align(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_compose(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.c_combine(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_linesep(x2)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(x3)(st))))(st))))))(x4)(st))(st)



c_qname :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_qname x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_111(x2)(x3)(x1)(st))(st)



c_qname'46txt'4665 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_qname'46txt'4665 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_105(x2)(x1)(st))(st)



c_correctName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_correctName x2 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.CEventOracle.c_replace(x5)(Curry.Module.OraclePrettyFlat.c__case_103(Curry.Module.OraclePrelude.c_filter(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_not))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_elem))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('#'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List)))))))(x1)(st))(x2)(x4)(st))(x5)(st))(st))(st)



op_60_36_62_62 :: Curry.Module.OraclePretty.C_Doc -> Curry.Module.OraclePretty.C_Doc -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
op_60_36_62_62 x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePretty.op_60_62(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_36_62(x1)(st))(x2)(x4)(st))(Curry.Module.OraclePretty.c_line(x5)(st))(x6)(st))(x3)(x7)(st))(st)



c_progDoc :: Curry.Module.FlatCurry.C_Prog -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_progDoc x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_101(x2)(x1)(st))(st)



c_precs :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_OpDecl) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)))
c_precs x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_precs'46_'35lambda3))))))))(st)



c_precs'46_'35lambda3 :: Curry.Module.FlatCurry.C_OpDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int
c_precs'46_'35lambda3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_100(x2)(x1)(st))(st)



c_exportedNames :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_Prog -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc
c_exportedNames x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_99(x2)(x3)(x1)(st))(st)



c_exportedNames'46typeExpDoc'4689 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_TypeDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_exportedNames'46typeExpDoc'4689 x2 x3 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))))(let {x4 = Curry.Module.OraclePrelude.c_filter(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.FlatCurry.C_Public)))))(Curry.Module.OracleFlatCurryGoodies.c_consVisibility(x1)(st))(x5)(st))(Curry.Module.OracleFlatCurryGoodies.c_trType(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.pf(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))))(Curry.Module.Prelude.pf(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))))))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))))))(Curry.Module.OraclePrettyFlat.c_exportedNames'46typeExpDoc'4689'46_'35lambda4))(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.pf(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))))(Curry.Module.Prelude.pf(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))))))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))))))(Curry.Module.OraclePrettyFlat.c_exportedNames'46typeExpDoc'4689'46_'35lambda5))(st))(x3)(x6)(st))(x7)(st)} in let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePretty.op_60_62(Curry.Module.OraclePrettyFlat.c_qname(x2)(Curry.Module.Oracle.c_apply(Curry.Module.OracleFlatCurryGoodies.c_typeName(x8)(st))(x3)(x9)(st))(x10)(st))(Curry.Module.OraclePrettyFlat.c__case_98(x4)(Curry.Module.OraclePrelude.c_null(x4)(x11)(st))(x12)(st))(x13)(st))(st))(st)



c_exportedNames'46typeExpDoc'4689'46_'35lambda4 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.FlatCurry.C_Visibility -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_ConsDecl) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.FlatCurry.C_ConsDecl
c_exportedNames'46typeExpDoc'4689'46_'35lambda4 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)



c_exportedNames'46typeExpDoc'4689'46_'35lambda5 :: (Curry t1) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.FlatCurry.C_Visibility -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.FlatCurry.C_TypeExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1
c_exportedNames'46typeExpDoc'4689'46_'35lambda5 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)



c_moduleHeaderDoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_moduleHeaderDoc x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))))))))))))))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x4)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x5)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_keyword(x6)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))(x7)(st))(x8)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_consname(x9)(st))(x2)(x10)(st))(x11)(st))(x12)(st))(Curry.Module.OraclePrettyFlat.c_exportsDoc(x3)(x13)(st))(x14)(st))(x15)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_keyword(x16)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))(x17)(st))(x18)(st))(st)



c_exportsDoc :: (Curry.Module.Prelude.List Curry.Module.OraclePretty.C_Doc) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_exportsDoc x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))))))))))))))))(Curry.Module.OraclePretty.c_group(Curry.Module.OraclePretty.c_nest(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_36_62(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_36_62(x3)(st))(Curry.Module.OraclePretty.c_lparen(x4)(st))(x5)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_align(x6)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_fillSep(x7)(st))(Curry.Module.OraclePretty.c_punctuate(Curry.Module.OraclePretty.c_comma(x8)(st))(x2)(x9)(st))(x10)(st))(x11)(st))(x12)(st))(x13)(st))(Curry.Module.OraclePretty.c_rparen(x14)(st))(x15)(st))(x16)(st))(x17)(st))(st)



c_impsDoc :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_impsDoc x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))))))))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_vcat(x1)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.OraclePrelude.op_46(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x3)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_keyword(x4)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))))(x5)(st))(x6)(st))(Curry.Module.OraclePrettyFlat.c_consname(x7)(st))(x8)(st))(x2)(x9)(st))(x10)(st))(st)



c_opsDoc :: (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_OpDecl) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_opsDoc x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_vcat(x1)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_opDoc))))(x2)(x3)(st))(x4)(st))(st)



c_opDoc :: Curry.Module.FlatCurry.C_OpDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_opDoc x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_97(x2)(x1)(st))(st)



c_opDoc'46fixDoc'46114 :: Curry.Module.FlatCurry.C_Fixity -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_opDoc'46fixDoc'46114 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_94(x2)(x1)(st))(st)



c_typesDoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_TypeDecl) -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_typesDoc x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_46(Curry.Module.OraclePretty.c_vcat(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_typeDoc(x2)))))))))(x3)(st))(st)



c_typeDoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_TypeDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_typeDoc x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_93(x2)(x3)(x1)(st))(st)



c_varDoc :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_varDoc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_text))))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_show))))(x1)(st))(x2)(st))(st)



c_tvarDoc :: Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_tvarDoc x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_92(x2)(Curry.Module.OraclePrelude.op_62(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(x1)(st))(x3)(st))(st)



c_consDeclsDoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_ConsDecl) -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_consDeclsDoc x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))))))))))))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_fillEncloseSep(Curry.Module.OraclePretty.op_60_62(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_operator(x1)(st))(Curry.Module.OraclePretty.c_equals(x3)(st))(x4)(st))(Curry.Module.OraclePretty.c_space(x5)(st))(x6)(st))(Curry.Module.OraclePretty.c_empty(x7)(st))(Curry.Module.OraclePretty.op_60_62(Curry.Module.OraclePrettyFlat.c_bar(x8)(st))(Curry.Module.OraclePretty.c_space(x9)(st))(x10)(st))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.op_60_62))(st))(Curry.Module.OraclePretty.c_space(x11)(st))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_consDeclDoc(x2)))))(x12)(st))))))(x13)(st))(st)



c_consDeclDoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_ConsDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_consDeclDoc x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_90(x2)(x3)(x1)(st))(st)



c_typeExprDoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.Module.FlatCurry.C_TypeExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_typeExprDoc x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_89(x2)(x3)(x4)(x1)(st))(st)



c_par :: Curry.Module.Prelude.C_Bool -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.OraclePretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_par x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_84(x2)(x1)(st))(st)



c_funcsDoc :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_funcsDoc x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_vcat(x1)(st))(Curry.Module.OraclePretty.c_punctuate(Curry.Module.OraclePretty.c_line(x5)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_funcDoc(x2)(x3)))))(x4)(x6)(st))(x7)(st))(x8)(st))(st)



c_funcDoc :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_FuncDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_funcDoc x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_83(x2)(x3)(x4)(x1)(st))(st)



c_funcTypeDeclDoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.FlatCurry.C_TypeExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_funcTypeDeclDoc x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrettyFlat.c_def(Curry.Module.OraclePrettyFlat.c_qname(x2)(x3)(x1)(st))(Curry.Module.Prelude.List)(Curry.Module.OraclePrettyFlat.c_funcTypeDoc(x2)(Curry.Module.OracleFlatCurryGoodies.c_argTypes(x4)(x5)(st))(Curry.Module.OracleFlatCurryGoodies.c_resultType(x4)(x6)(st))(x7)(st))(x8)(st))(st)



c_funcTypeDoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_TypeExpr) -> Curry.Module.FlatCurry.C_TypeExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_funcTypeDoc x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))))))))))))(Curry.Module.OraclePretty.c_fillEncloseSep(Curry.Module.OraclePretty.op_60_62(Curry.Module.OraclePrettyFlat.c_dcolon(x1)(st))(Curry.Module.OraclePretty.c_space(x5)(st))(x6)(st))(Curry.Module.OraclePretty.c_empty(x7)(st))(Curry.Module.OraclePretty.op_60_62(Curry.Module.OraclePrettyFlat.c_arrow(x8)(st))(Curry.Module.OraclePretty.c_space(x9)(st))(x10)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePretty.op_60_62))(st))(Curry.Module.OraclePretty.c_space(x11)(st))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_typeExprDoc(x2)(Curry.Module.Prelude.C_True)))))(x12)(st))(Curry.Module.OraclePrelude.op_43_43(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(x13)(st))(x14)(st))(x15)(st))(st)



c_ruleDoc :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.FlatCurry.C_Rule -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_ruleDoc x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_82(x2)(x3)(x4)(x5)(x1)(st))(st)



c_expDoc :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.Module.FlatCurry.C_Expr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_expDoc x2 x3 x4 x5 x6 x1 st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.c_maybe(Curry.Module.OraclePrelude.c_maybe(Curry.Module.OraclePrettyFlat.c_expDoc2(x2)(Curry.Module.Prelude.C_Zero)(x4)(x5)(x6)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_expDoc'46_'35lambda6(x4)(x2)))))(Curry.Module.OraclePrettyFlat.c_toList(x6)(x7)(st))(x8)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_expDoc'46_'35lambda7))))(Curry.Module.OraclePrettyFlat.c_toString(x6)(x9)(st))(x10)(st))(st)



c_expDoc'46_'35lambda6 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_expDoc'46_'35lambda6 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_list(x1)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_expDoc(x3)(Curry.Module.Prelude.C_Zero)(x2)(Curry.Module.Prelude.C_False)))))(x4)(x5)(st))(x6)(st))(st)



c_expDoc'46_'35lambda7 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_expDoc'46_'35lambda7 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_81(x2)(Curry.Module.OraclePrelude.c_null(x2)(x1)(st))(x3)(st))(st)



c_expDoc2 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.Module.FlatCurry.C_Expr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_expDoc2 x2 x3 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_80(x2)(x3)(x4)(x5)(x6)(x1)(st))(st)



c_expDoc2'46_'35selFP3'35lbr :: (Curry.Module.Prelude.T2 Curry.Module.OraclePretty.C_Doc Curry.Module.OraclePretty.C_Doc) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_expDoc2'46_'35selFP3'35lbr x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_73(x2)(x1)(st))(st)



c_expDoc2'46_'35selFP4'35rbr :: (Curry.Module.Prelude.T2 Curry.Module.OraclePretty.C_Doc Curry.Module.OraclePretty.C_Doc) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_expDoc2'46_'35selFP4'35rbr x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_72(x2)(x1)(st))(st)



c_branchDoc :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_BranchExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_branchDoc x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_71(x2)(x3)(x4)(x1)(st))(st)



c_caseTypeDoc :: Curry.Module.FlatCurry.C_CaseType -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_caseTypeDoc x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_70(x2)(x1)(st))(st)



c_patternDoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_Pattern -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_patternDoc x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_69(x2)(x3)(x1)(st))(st)



c_letBindsDoc :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Expr)) -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc))
c_letBindsDoc x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_46(Curry.Module.OraclePrettyFlat.c_layout(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_letBindDoc(x2)(x3)))))))))(x4)(st))(st)



c_letBindDoc :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Expr) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_letBindDoc x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_64(x2)(x3)(x4)(x1)(st))(st)



c_litDoc :: Curry.Module.FlatCurry.C_Literal -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OraclePretty.C_Doc
c_litDoc x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_63(x2)(x1)(st))(st)



c_quoteChar :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_quoteChar x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_maybe((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_id))))(Curry.Module.OraclePrelude.c_lookup(x2)(Curry.Module.OraclePrettyFlat.c_specialChars(x1)(st))(x3)(st))(x4)(st))(st)



c_specialChars :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Char (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_specialChars x1 st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Char('\r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Char('\t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.List)))))(st)



c_toString :: Curry.Module.FlatCurry.C_Expr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_toString x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_62(x2)(x1)(st))(st)



c_toList :: Curry.Module.FlatCurry.C_Expr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr)
c_toList x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_31(x2)(x1)(st))(st)



c_elimApp :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr))
c_elimApp x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryGoodies.c_updCombs(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.pf(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))))(Curry.Module.OraclePrettyFlat.c_elimApp'46elim'46276))(st))(x1)(st))(st)



c_elimApp'46extend'46276 :: Curry.Module.FlatCurry.C_Expr -> Curry.Module.FlatCurry.C_Expr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_elimApp'46extend'46276 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_2(x3)(x2)(x1)(st))(st)



c_elimApp'46elim'46276 :: Curry.Module.FlatCurry.C_CombType -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_elimApp'46elim'46276 x2 x3 x4 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List)))))))))))))(Curry.Module.OraclePrettyFlat.c__case_1(x2)(x3)(x4)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.FlatCurry.C_FuncCall)(x1)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.T2(Curry.Module.OraclePrettyFlat.c_prelude(x5)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(x6)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OracleFlatCurryGoodies.c_isComb(Curry.Module.OraclePrelude.c_head(x4)(x7)(st))(x8)(st))(Curry.Module.OraclePrelude.op_47_61(Curry.Module.OracleFlatCurryGoodies.c_combName(Curry.Module.OraclePrelude.c_head(x4)(x9)(st))(x10)(st))(Curry.Module.Prelude.T2(Curry.Module.OraclePrettyFlat.c_prelude(x11)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(x12)(st))(x13)(st))(x14)(st))(x15)(st))(x16)(st))(st)



c__case_1 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_1_case__117(x1)(x2)(x3)(x4)(x5)(st))(st)



c__case_0 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_0_case__116(x1)(x2)(x3)(x4)(x5)(st))(st)



c__case_2 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_2_case__115(x1)(x3)(x2)(st))(st)



c__case_31 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_31_case__114(x1)(x2)(st))(st)



c__case_30 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_30_case__113(x1)(x4)(x5)(x3)(st))(st)



c__case_29 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_29_case__112(x1)(x5)(x4)(st))(st)



c__case_28 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_28_case__111(x1)(x5)(x7)(x6)(st))(st)



c__case_27 x5 x7 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_27_case__110(x1)(x5)(x7)(x9)(x10)(st))(st)



c__case_26 x5 x7 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_26_case__109(x1)(x5)(x7)(x9)(st))(st)



c__case_25 x5 x7 x10 x11 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_25_case__108(x1)(x5)(x7)(x11)(x12)(st))(st)



c__case_24 x5 x7 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_24_case__107(x1)(x5)(x7)(x11)(st))(st)



c__case_23 x5 x7 x12 x13 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_23_case__106(x1)(x5)(x7)(x13)(x14)(st))(st)



c__case_22 x5 x7 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_22_case__105(x1)(x5)(x7)(x13)(st))(st)



c__case_21 x5 x7 x14 x15 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_21_case__104(x1)(x5)(x7)(x15)(x16)(st))(st)



c__case_20 x5 x7 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_20_case__103(x1)(x5)(x7)(x15)(st))(st)



c__case_19 x5 x7 x16 x17 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_19_case__102(x1)(x5)(x7)(x17)(x18)(st))(st)



c__case_18 x5 x7 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_18_case__101(x1)(x5)(x7)(x17)(st))(st)



c__case_17 x5 x7 x18 x19 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_17_case__100(x1)(x5)(x7)(x19)(x20)(st))(st)



c__case_16 x5 x7 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_16_case__99(x1)(x5)(x7)(x19)(st))(st)



c__case_15 x5 x7 x20 x21 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_15_case__98(x1)(x5)(x7)(x21)(x22)(st))(st)



c__case_14 x5 x7 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_14_case__97(x1)(x5)(x7)(x21)(st))(st)



c__case_13 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_13_case__96(x1)(x5)(x7)(st))(st)



c__case_12 x5 x22 x23 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_12_case__95(x1)(x5)(x22)(x23)(x24)(st))(st)



c__case_7 x5 x22 x23 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_7_case__94(x1)(x5)(x23)(x24)(st))(st)



c__case_6 x5 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_6_case__93(x1)(x5)(x23)(st))(st)



c__case_5 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_5_case__92(x1)(x5)(st))(st)



c__case_4 x30 x31 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_4_case__91(x1)(x30)(x31)(st))(st)



c__case_3 x30 x32 x33 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_3_case__90(x1)(x30)(x32)(x33)(st))(st)



c__case_11 x5 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_11_case__89(x1)(x5)(x23)(st))(st)



c__case_10 x5 x24 x25 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_10_case__88(x1)(x5)(x25)(x26)(st))(st)



c__case_9 x5 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_9_case__87(x1)(x5)(x25)(st))(st)



c__case_8 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_8_case__86(x1)(x5)(st))(st)



c__case_62 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_62_case__85(x1)(x2)(st))(st)



c__case_61 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_61_case__84(x1)(x4)(x5)(x3)(st))(st)



c__case_60 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_60_case__83(x1)(x5)(x4)(st))(st)



c__case_59 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_59_case__82(x1)(x5)(x7)(x6)(st))(st)



c__case_58 x5 x7 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_58_case__81(x1)(x5)(x7)(x9)(x10)(st))(st)



c__case_57 x5 x7 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_57_case__80(x1)(x5)(x7)(x9)(st))(st)



c__case_56 x5 x7 x10 x11 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_56_case__79(x1)(x5)(x7)(x11)(x12)(st))(st)



c__case_55 x5 x7 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_55_case__78(x1)(x5)(x7)(x11)(st))(st)



c__case_54 x5 x7 x12 x13 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_54_case__77(x1)(x5)(x7)(x13)(x14)(st))(st)



c__case_53 x5 x7 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_53_case__76(x1)(x5)(x7)(x13)(st))(st)



c__case_52 x5 x7 x14 x15 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_52_case__75(x1)(x5)(x7)(x15)(x16)(st))(st)



c__case_51 x5 x7 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_51_case__74(x1)(x5)(x7)(x15)(st))(st)



c__case_50 x5 x7 x16 x17 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_50_case__73(x1)(x5)(x7)(x17)(x18)(st))(st)



c__case_49 x5 x7 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_49_case__72(x1)(x5)(x7)(x17)(st))(st)



c__case_48 x5 x7 x18 x19 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_48_case__71(x1)(x5)(x7)(x19)(x20)(st))(st)



c__case_47 x5 x7 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_47_case__70(x1)(x5)(x7)(x19)(st))(st)



c__case_46 x5 x7 x20 x21 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_46_case__69(x1)(x5)(x7)(x21)(x22)(st))(st)



c__case_45 x5 x7 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_45_case__68(x1)(x5)(x7)(x21)(st))(st)



c__case_44 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_44_case__67(x1)(x5)(x7)(st))(st)



c__case_43 x5 x22 x23 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_43_case__66(x1)(x5)(x22)(x23)(x24)(st))(st)



c__case_38 x5 x22 x23 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_38_case__65(x1)(x5)(x23)(x24)(st))(st)



c__case_37 x5 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_37_case__64(x1)(x5)(x23)(st))(st)



c__case_36 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_36_case__63(x1)(x5)(st))(st)



c__case_35 x31 x30 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_35_case__62(x1)(x31)(x30)(st))(st)



c__case_34 x31 x32 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_34_case__61(x1)(x31)(x32)(st))(st)



c__case_33 x33 x31 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_33_case__60(x1)(x33)(x31)(st))(st)



c__case_32 x33 x34 x35 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_32_case__59(x1)(x33)(x34)(x35)(st))(st)



c__case_42 x5 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_42_case__58(x1)(x5)(x23)(st))(st)



c__case_41 x5 x24 x25 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_41_case__57(x1)(x5)(x25)(x26)(st))(st)



c__case_40 x5 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_40_case__56(x1)(x5)(x25)(st))(st)



c__case_39 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_39_case__55(x1)(x5)(st))(st)



c__case_63 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_63_case__54(x1)(x2)(st))(st)



c__case_64 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_64_case__53(x1)(x2)(x3)(x4)(st))(st)



c__case_69 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_69_case__52(x1)(x2)(x3)(st))(st)



c__case_68 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_68_case__51(x1)(x2)(x4)(x5)(x6)(st))(st)



c__case_67 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_67_case__50(x1)(x2)(x4)(x5)(x6)(st))(st)



c__case_66 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_66_case__49(x1)(x2)(x4)(x5)(x6)(st))(st)



c__case_65 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_65_case__48(x1)(x2)(x4)(x5)(x6)(st))(st)



c__case_70 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_70_case__47(x1)(x2)(st))(st)



c__case_71 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_71_case__46(x1)(x2)(x3)(x4)(st))(st)



c__case_72 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_72_case__45(x1)(x2)(st))(st)



c__case_73 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_73_case__44(x1)(x2)(st))(st)



c__case_80 x2 x3 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_80_case__43(x1)(x2)(x3)(x4)(x5)(x6)(st))(st)



c__case_74 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_74_case__42(x1)(x5)(st))(st)



c__case_75 x2 x10 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_75_case__41(x1)(x17)(st))(st)



c__case_79 x2 x3 x4 x5 x9 x10 x11 x13 x14 x15 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_79_case__40(x1)(x2)(x3)(x4)(x5)(x9)(x10)(x11)(x13)(x14)(x15)(x16)(st))(st)



c__case_78 x2 x3 x4 x5 x9 x10 x11 x13 x14 x15 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_78_case__39(x1)(x2)(x3)(x4)(x5)(x10)(x11)(x13)(x14)(x15)(x16)(st))(st)



c__case_77 x2 x3 x4 x5 x10 x11 x13 x14 x15 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_77_case__38(x1)(x2)(x3)(x4)(x5)(x10)(x11)(x13)(x14)(x15)(x16)(st))(st)



c__case_76 x2 x4 x5 x10 x11 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_76_case__37(x1)(x2)(x4)(x5)(x10)(x11)(x12)(st))(st)



c__case_81 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_81_case__36(x1)(x2)(x3)(st))(st)



c__case_82 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_82_case__35(x1)(x2)(x3)(x4)(x5)(st))(st)



c__case_83 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_83_case__34(x1)(x2)(x3)(x4)(st))(st)



c__case_84 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_84_case__33(x1)(x2)(st))(st)



c__case_89 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_89_case__32(x1)(x2)(x3)(x4)(st))(st)



c__case_88 x2 x3 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_88_case__31(x1)(x2)(x3)(x6)(x7)(x8)(st))(st)



c__case_87 x2 x3 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_87_case__30(x1)(x2)(x3)(x6)(x7)(x8)(st))(st)



c__case_86 x2 x3 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_86_case__29(x1)(x2)(x3)(x6)(x7)(x8)(st))(st)



c__case_85 x2 x3 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_85_case__28(x1)(x2)(x3)(x6)(x7)(x8)(st))(st)



c__case_90 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_90_case__27(x1)(x2)(x3)(st))(st)



c__case_92 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_92_case__26(x1)(x2)(x3)(st))(st)



c__case_91 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_91_case__25(x1)(x2)(x3)(st))(st)



c__case_93 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_93_case__24(x1)(x2)(x3)(st))(st)



c__case_94 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_94_case__23(x1)(x2)(st))(st)



c__case_97 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_97_case__22(x1)(x2)(st))(st)



c__case_96 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_96_case__21(x1)(x4)(x5)(x3)(st))(st)



c__case_95 x3 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_95_case__20(x1)(x7)(x8)(st))(st)



c__case_98 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_98_case__19(x1)(x5)(st))(st)



c__case_99 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_99_case__18(x1)(x2)(x3)(st))(st)



c__case_100 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_100_case__17(x1)(x2)(st))(st)



c__case_101 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_101_case__16(x1)(x2)(st))(st)



c__case_103 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_103_case__15(x1)(x3)(st))(st)



c__case_102 x3 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_102_case__14(x1)(x3)(x5)(x6)(st))(st)



c__case_105 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_105_case__13(x1)(x2)(st))(st)



c__case_104 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_104_case__12(x1)(x3)(x4)(x5)(st))(st)



c__case_111 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_111_case__11(x1)(x2)(x3)(st))(st)



c__case_110 x2 x3 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_110_case__10(x1)(x2)(x3)(x4)(x5)(x6)(st))(st)



c__case_109 x2 x3 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_109_case__9(x1)(x2)(x4)(x5)(x6)(st))(st)



c__case_107 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_107_case__8(x1)(x2)(x4)(x5)(x6)(st))(st)



c__case_106 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_106_case__7(x1)(x4)(x5)(x6)(st))(st)



c__case_108 x2 x4 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_108_case__6(x1)(x4)(x5)(x6)(st))(st)



c__case_112 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_112_case__5(x1)(x2)(x3)(x4)(st))(st)



c__case_113 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_113_case__4(x1)(x3)(x4)(st))(st)



c__case_114 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_114_case__3(x1)(x2)(st))(st)



c__case_115 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_115_case__2(x1)(x2)(st))(st)



c__case_117 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_117_case__1(x1)(x4)(x5)(st))(st)



c__case_116 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_116_case__0(x1)(x4)(x5)(st))(st)



c__case_116_case__0 x1 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_116_case__0 x1 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_116_case__0 x1 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_116_case__0(x1)(x4)(x)(st))(i)(xs)(st)
c__case_116_case__0 x1 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_116_case__0")(x)



c__case_117_case__1 x1 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_empty(x1)(st))(st)
c__case_117_case__1 x1 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_116(x4)(Curry.Module.Prelude.C_True)(x1)(st))(st)
c__case_117_case__1 x1 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_117_case__1(x1)(x4)(x)(st))(i)(xs)(st)
c__case_117_case__1 x1 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_117_case__1")(x)



c__case_115_case__2 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_all(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_elem))))(Curry.Module.OraclePrettyFlat.c_infixIDs(x1)(st))))))(x5)(st))(x4)(x6)(st))(st)
c__case_115_case__2 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_115_case__2(x1)(x)(st))(i)(xs)(st)
c__case_115_case__2 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_115_case__2")(x)



c__case_114_case__3 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.OraclePrettyFlat.c_prelude(x1)(st))(x5)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_elem(Curry.Module.OraclePrelude.c_take(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x4)(x6)(st))(x7)(st))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List)))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.List)))(x8)(st))(x9)(st))(st)
c__case_114_case__3 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_114_case__3(x1)(x)(st))(i)(xs)(st)
c__case_114_case__3 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_114_case__3")(x)



c__case_113_case__4 x1 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_empty(x1)(st))(st)
c__case_113_case__4 x1 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePretty.op_60_62(Curry.Module.OraclePretty.c_space(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_align(x5)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_fillSep(x6)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.OraclePrettyFlat.c_varDoc(x7)(st))(x3)(x8)(st))(x9)(st))(x10)(st))(x11)(st))(st)
c__case_113_case__4 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_113_case__4(x1)(x3)(x)(st))(i)(xs)(st)
c__case_113_case__4 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_113_case__4")(x)



c__case_112_case__5 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_112_case__5 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))))))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_block(x1)(st))(Curry.Module.OraclePretty.c_fillEncloseSep(Curry.Module.OraclePretty.c_empty(x5)(st))(Curry.Module.OraclePretty.c_empty(x6)(st))(Curry.Module.OraclePretty.c_space(x7)(st))((Curry.Module.Prelude.:<)(x2)(x3))(x8)(st))(x9)(st))(st)
c__case_112_case__5 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_112_case__5(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_112_case__5 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_112_case__5")(x)



c__case_108_case__6 x1 x4 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_parens(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_operator(x7)(st))(Curry.Module.OraclePretty.c_text(x5)(x8)(st))(x9)(st))(x10)(st))(st)
c__case_108_case__6 x1 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))))))))))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_parens(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_operator(x11)(st))(Curry.Module.OraclePretty.op_60_62(Curry.Module.OraclePretty.op_60_62(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_consname(x12)(st))(x4)(x13)(st))(Curry.Module.OraclePretty.c_dot(x14)(st))(x15)(st))(Curry.Module.OraclePrettyFlat.c_qname'46txt'4665(x5)(x16)(st))(x17)(st))(x18)(st))(x19)(st))(st)
c__case_108_case__6 x1 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_108_case__6(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_108_case__6 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_108_case__6")(x)



c__case_106_case__7 x1 x4 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c_qname'46txt'4665(Curry.Module.OraclePrettyFlat.c_correctName(x5)(x1)(st))(x7)(st))(st)
c__case_106_case__7 x1 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePretty.op_60_62(Curry.Module.OraclePretty.op_60_62(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_consname(x1)(st))(x4)(x8)(st))(Curry.Module.OraclePretty.c_dot(x9)(st))(x10)(st))(Curry.Module.OraclePrettyFlat.c_qname'46txt'4665(x5)(x11)(st))(x12)(st))(st)
c__case_106_case__7 x1 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_106_case__7(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_106_case__7 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_106_case__7")(x)



c__case_107_case__8 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_106(x2)(x4)(x5)(Curry.Module.OraclePrelude.op_61_61(x4)(x2)(x1)(st))(x7)(st))(st)
c__case_107_case__8 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_107_case__8 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_107_case__8(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_107_case__8 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_107_case__8")(x)



c__case_109_case__9 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_108(x2)(x4)(x5)(Curry.Module.OraclePrelude.op_61_61(x4)(x2)(x1)(st))(x7)(st))(st)
c__case_109_case__9 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_107(x2)(x4)(x5)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x8)(st))(st)
c__case_109_case__9 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_109_case__9(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_109_case__9 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_109_case__9")(x)



c__case_110_case__10 x1 x2 x3 x4 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_operator(x1)(st))(Curry.Module.OraclePretty.c_text(x5)(x7)(st))(x8)(st))(st)
c__case_110_case__10 x1 x2 x3 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_109(x2)(x3)(x4)(x5)(Curry.Module.OraclePrettyFlat.c_isInfixName(x3)(x1)(st))(x9)(st))(st)
c__case_110_case__10 x1 x2 x3 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_110_case__10(x1)(x2)(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_110_case__10 x1 x2 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_110_case__10")(x)



c__case_111_case__11 x1 x2 x3@(Curry.Module.Prelude.T2 x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrettyFlat.c__case_110(x2)(x3)(x4)(x5)(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.Prelude.T2(Curry.Module.OraclePrettyFlat.c_prelude(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))))(x6)(st))(Curry.Module.OraclePrettyFlat.c_isTupleName(x3)(x7)(st))(x8)(st))(x9)(st))(st)
c__case_111_case__11 x1 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_111_case__11(x1)(x2)(x)(st))(i)(xs)(st)
c__case_111_case__11 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_111_case__11")(x)



c__case_104_case__12 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_consname(x1)(st))((Curry.Module.Prelude.:<)(x3)(x4))(x6)(st))(st)
c__case_104_case__12 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_text((Curry.Module.Prelude.:<)(x3)(x4))(x1)(st))(st)
c__case_104_case__12 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_104_case__12(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_104_case__12 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_104_case__12")(x)



c__case_105_case__13 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_104(x3)(x4)(Curry.Module.OracleChar.c_isUpper(x3)(x1)(st))(x5)(st))(st)
c__case_105_case__13 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_105_case__13(x1)(x)(st))(i)(xs)(st)
c__case_105_case__13 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_105_case__13")(x)



c__case_102_case__14 x1 x3 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_102_case__14 x1 x3 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_102_case__14 x1 x3 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_102_case__14(x1)(x3)(x5)(x)(st))(i)(xs)(st)
c__case_102_case__14 x1 x3 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_102_case__14")(x)



c__case_103_case__15 x1 x3@((Curry.Module.Prelude.:<) x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_102(x3)(x4)(x5)(Curry.Module.OraclePrelude.op_61_61(x4)(Curry.Module.Prelude.C_Char('_'))(x1)(st))(x6)(st))(st)
c__case_103_case__15 x1 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_103_case__15 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_103_case__15(x1)(x)(st))(i)(xs)(st)
c__case_103_case__15 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_103_case__15")(x)



c__case_101_case__16 x1 x2@(Curry.Module.FlatCurry.C_Prog x3 x4 x5 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List)))))))))))))(Curry.Module.OraclePrettyFlat.op_60_36_62_62(Curry.Module.OraclePrettyFlat.op_60_36_62_62(Curry.Module.OraclePrettyFlat.op_60_36_62_62(Curry.Module.OraclePrettyFlat.op_60_36_62_62(Curry.Module.OraclePrettyFlat.c_moduleHeaderDoc(x3)(Curry.Module.OraclePrettyFlat.c_exportedNames(x3)(x2)(x1)(st))(x8)(st))(Curry.Module.OraclePrettyFlat.c_impsDoc(x4)(x9)(st))(x10)(st))(Curry.Module.OraclePrettyFlat.c_opsDoc(x7)(x11)(st))(x12)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_typesDoc(x3)(x13)(st))(x5)(x14)(st))(x15)(st))(Curry.Module.OraclePrettyFlat.c_funcsDoc(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_precs(x16)(st))(x7)(x17)(st))(x3)(x6)(x18)(st))(x19)(st))(st)
c__case_101_case__16 x1 (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_101_case__16(x1)(x)(st))(i)(xs)(st)
c__case_101_case__16 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_101_case__16")(x)



c__case_100_case__17 x1 x2@(Curry.Module.FlatCurry.C_Op x3 x4 x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(x3)(x5))(st)
c__case_100_case__17 x1 (Curry.Module.FlatCurry.C_OpDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_100_case__17(x1)(x)(st))(i)(xs)(st)
c__case_100_case__17 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_100_case__17")(x)



c__case_99_case__18 x1 x2 x3@(Curry.Module.FlatCurry.C_Prog x4 x5 x6 x7 x8) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List)))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_exportedNames'46typeExpDoc'4689(x2)))))(Curry.Module.OraclePrelude.c_filter(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.FlatCurry.C_Public)))))(Curry.Module.OracleFlatCurryGoodies.c_typeVisibility(x1)(st))(x9)(st))(x6)(x10)(st))(x11)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_qname(x2)))))(Curry.Module.OracleFlatCurryGoodies.c_funcName(x12)(st))(x13)(st))(Curry.Module.OraclePrelude.c_filter(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.FlatCurry.C_Public)))))(Curry.Module.OracleFlatCurryGoodies.c_funcVisibility(x14)(st))(x15)(st))(x7)(x16)(st))(x17)(st))(x18)(st))(st)
c__case_99_case__18 x1 x2 (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_99_case__18(x1)(x2)(x)(st))(i)(xs)(st)
c__case_99_case__18 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_99_case__18")(x)



c__case_98_case__19 x1 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_empty(x1)(st))(st)
c__case_98_case__19 x1 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_text((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List)))))(x1)(st))(st)
c__case_98_case__19 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_98_case__19(x1)(x)(st))(i)(xs)(st)
c__case_98_case__19 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_98_case__19")(x)



c__case_95_case__20 x1 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x7)(st)
c__case_95_case__20 x1 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('`'))(Curry.Module.OraclePrelude.op_43_43(x7)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('`'))(Curry.Module.Prelude.List))(x1)(st)))(st)
c__case_95_case__20 x1 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_95_case__20(x1)(x7)(x)(st))(i)(xs)(st)
c__case_95_case__20 x1 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_95_case__20")(x)



c__case_96_case__21 x1 x4 x5 x3@(Curry.Module.Prelude.T2 x6 x7) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List))))))))))))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x10)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x11)(st))(Curry.Module.OraclePretty.op_60_62(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_keyword(x12)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))(Curry.Module.Prelude.List))))))(x13)(st))(Curry.Module.OraclePrettyFlat.c_opDoc'46fixDoc'46114(x4)(x14)(st))(x15)(st))(x16)(st))(Curry.Module.OraclePretty.c_int(x5)(x17)(st))(x18)(st))(x19)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_operator(x20)(st))(Curry.Module.OraclePretty.c_text(Curry.Module.OraclePrettyFlat.c__case_95(x3)(x7)(Curry.Module.OraclePrettyFlat.c_isInfixName(x3)(x1)(st))(x9)(st))(x21)(st))(x22)(st))(x23)(st))(st))(st)
c__case_96_case__21 x1 x4 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_96_case__21(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_96_case__21 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_96_case__21")(x)



c__case_97_case__22 x1 x2@(Curry.Module.FlatCurry.C_Op x3 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_96(x4)(x5)(x3)(x1)(st))(st)
c__case_97_case__22 x1 (Curry.Module.FlatCurry.C_OpDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_97_case__22(x1)(x)(st))(i)(xs)(st)
c__case_97_case__22 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_97_case__22")(x)



c__case_94_case__23 x1 x2@Curry.Module.FlatCurry.C_InfixOp st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_empty(x1)(st))(st)
c__case_94_case__23 x1 x2@Curry.Module.FlatCurry.C_InfixlOp st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_keyword(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List))(x3)(st))(st)
c__case_94_case__23 x1 x2@Curry.Module.FlatCurry.C_InfixrOp st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_keyword(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))(x4)(st))(st)
c__case_94_case__23 x1 (Curry.Module.FlatCurry.C_FixityOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_94_case__23(x1)(x)(st))(i)(xs)(st)
c__case_94_case__23 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_94_case__23")(x)



c__case_93_case__24 x1 x2 x3@(Curry.Module.FlatCurry.C_Type x4 x5 x6 x7) st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List)))))))))(Curry.Module.OraclePrettyFlat.c_def(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_keyword(x12)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))(Curry.Module.Prelude.List)))))(x13)(st))(x14)(st))(Curry.Module.OraclePrettyFlat.c_qname(x2)(x4)(x15)(st))(x16)(st))(x6)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_consDeclsDoc(x2)(x17)(st))(x7)(x18)(st))(x19)(st))(st)
c__case_93_case__24 x1 x2 x3@(Curry.Module.FlatCurry.C_TypeSyn x8 x9 x10 x11) st = let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)((Curry.Module.Prelude.:<)(x30)((Curry.Module.Prelude.:<)(x31)((Curry.Module.Prelude.:<)(x32)(Curry.Module.Prelude.List))))))))))))))(Curry.Module.OraclePrettyFlat.c_def(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_keyword(x20)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))(x21)(st))(x22)(st))(Curry.Module.OraclePrettyFlat.c_qname(x2)(x8)(x23)(st))(x24)(st))(x10)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x25)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_operator(x26)(st))(Curry.Module.OraclePretty.c_equals(x27)(st))(x28)(st))(x29)(st))(Curry.Module.OraclePrettyFlat.c_typeExprDoc(x2)(Curry.Module.Prelude.C_False)(x11)(x30)(st))(x31)(st))(x32)(st))(st)
c__case_93_case__24 x1 x2 (Curry.Module.FlatCurry.C_TypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_93_case__24(x1)(x2)(x)(st))(i)(xs)(st)
c__case_93_case__24 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_93_case__24")(x)



c__case_91_case__25 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePretty.c_text((Curry.Module.Prelude.:<)(Curry.Module.OraclePrelude.c_chr(Curry.Module.OraclePrelude.op_43(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))))(x2)(x1)(st))(x4)(st))(Curry.Module.Prelude.List))(x5)(st))(st)
c__case_91_case__25 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_91_case__25 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_91_case__25(x1)(x2)(x)(st))(i)(xs)(st)
c__case_91_case__25 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_91_case__25")(x)



c__case_92_case__26 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePretty.c_text(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))(x4)(st))(x5)(st))(st)
c__case_92_case__26 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_91(x2)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x6)(st))(st)
c__case_92_case__26 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_92_case__26(x1)(x2)(x)(st))(i)(xs)(st)
c__case_92_case__26 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_92_case__26")(x)



c__case_90_case__27 x1 x2 x3@(Curry.Module.FlatCurry.C_Cons x4 x5 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrettyFlat.c_app(Curry.Module.OraclePrettyFlat.c_qname(x2)(x4)(x1)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_typeExprDoc(x2)(Curry.Module.Prelude.C_True)))))(x7)(x8)(st))(x9)(st))(st)
c__case_90_case__27 x1 x2 (Curry.Module.FlatCurry.C_ConsDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_90_case__27(x1)(x2)(x)(st))(i)(xs)(st)
c__case_90_case__27 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_90_case__27")(x)



c__case_85_case__28 x1 x2 x3 x6 x7 x8@Curry.Module.Prelude.C_True st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_36(Curry.Module.OraclePrettyFlat.c_par(x3)(x1)(st))(Curry.Module.OraclePrettyFlat.c_app(Curry.Module.OraclePrettyFlat.c_qname(x2)(x6)(x9)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_typeExprDoc(x2)(Curry.Module.Prelude.C_True)))))(x7)(x10)(st))(x11)(st))(x12)(st))(st)
c__case_85_case__28 x1 x2 x3 x6 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_85_case__28 x1 x2 x3 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_85_case__28(x1)(x2)(x3)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_85_case__28 x1 x2 x3 x6 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_85_case__28")(x)



c__case_86_case__29 x1 x2 x3 x6 x7 x8@Curry.Module.Prelude.C_True st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_tupled(x1)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_typeExprDoc(x2)(Curry.Module.Prelude.C_False)))))(x7)(x9)(st))(x10)(st))(st)
c__case_86_case__29 x1 x2 x3 x6 x7 x8@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_85(x2)(x3)(x6)(x7)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x11)(st))(st)
c__case_86_case__29 x1 x2 x3 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_86_case__29(x1)(x2)(x3)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_86_case__29 x1 x2 x3 x6 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_86_case__29")(x)



c__case_87_case__30 x1 x2 x3 x6 x7 x8@Curry.Module.Prelude.C_True st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_brackets(x1)(st))(Curry.Module.OraclePrettyFlat.c_typeExprDoc(x2)(Curry.Module.Prelude.C_False)(Curry.Module.OraclePrelude.c_head(x7)(x9)(st))(x10)(st))(x11)(st))(st)
c__case_87_case__30 x1 x2 x3 x6 x7 x8@Curry.Module.Prelude.C_False st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_86(x2)(x3)(x6)(x7)(Curry.Module.OraclePrettyFlat.c_isTupleName(x6)(x1)(st))(x12)(st))(st)
c__case_87_case__30 x1 x2 x3 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_87_case__30(x1)(x2)(x3)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_87_case__30 x1 x2 x3 x6 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_87_case__30")(x)



c__case_88_case__31 x1 x2 x3 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c_qname(x2)(x6)(x1)(st))(st)
c__case_88_case__31 x1 x2 x3 x6 x7 x8@Curry.Module.Prelude.C_False st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrettyFlat.c__case_87(x2)(x3)(x6)(x7)(Curry.Module.OraclePrelude.op_61_61(x6)(Curry.Module.Prelude.T2(Curry.Module.OraclePrettyFlat.c_prelude(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))))(x9)(st))(x10)(st))(st)
c__case_88_case__31 x1 x2 x3 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_88_case__31(x1)(x2)(x3)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_88_case__31 x1 x2 x3 x6 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_88_case__31")(x)



c__case_89_case__32 x1 x2 x3 x4@(Curry.Module.FlatCurry.C_TVar x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c_tvarDoc(x5)(x1)(st))(st)
c__case_89_case__32 x1 x2 x3 x4@(Curry.Module.FlatCurry.C_TCons x6 x7) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_88(x2)(x3)(x6)(x7)(Curry.Module.OraclePrelude.c_null(x7)(x1)(st))(x10)(st))(st)
c__case_89_case__32 x1 x2 x3 x4@(Curry.Module.FlatCurry.C_FuncType x8 x9) st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List)))))))))))))))(Curry.Module.OraclePrelude.op_36(Curry.Module.OraclePrettyFlat.c_par(x3)(x1)(st))(Curry.Module.OraclePretty.c_fillEncloseSep(Curry.Module.OraclePretty.c_empty(x11)(st))(Curry.Module.OraclePretty.c_empty(x12)(st))(Curry.Module.OraclePretty.op_60_62(Curry.Module.OraclePretty.op_60_62(Curry.Module.OraclePretty.c_space(x13)(st))(Curry.Module.OraclePrettyFlat.c_arrow(x14)(st))(x15)(st))(Curry.Module.OraclePretty.c_space(x16)(st))(x17)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_typeExprDoc(x2)(Curry.Module.Prelude.C_True)))))(Curry.Module.OracleFlatCurryGoodies.c_argTypes(x4)(x18)(st))(x19)(st))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrettyFlat.c_typeExprDoc(x2)(Curry.Module.Prelude.C_False)(Curry.Module.OracleFlatCurryGoodies.c_resultType(x4)(x20)(st))(x21)(st))(Curry.Module.Prelude.List))(x22)(st))(x23)(st))(x24)(st))(st)
c__case_89_case__32 x1 x2 x3 (Curry.Module.FlatCurry.C_TypeExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_89_case__32(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_89_case__32 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_89_case__32")(x)



c__case_84_case__33 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_parens(x1)(st))(st)
c__case_84_case__33 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_id))))(st)
c__case_84_case__33 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_84_case__33(x1)(x)(st))(i)(xs)(st)
c__case_84_case__33 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_84_case__33")(x)



c__case_83_case__34 x1 x2 x3 x4@(Curry.Module.FlatCurry.C_Func x5 x6 x7 x8 x9) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List)))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_36_62(x1)(st))(Curry.Module.OraclePrettyFlat.c_funcTypeDeclDoc(x3)(x5)(x8)(x10)(st))(x11)(st))(Curry.Module.OraclePrettyFlat.c_ruleDoc(x2)(x3)(x5)(x9)(x12)(st))(x13)(st))(st)
c__case_83_case__34 x1 x2 x3 (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_83_case__34(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_83_case__34 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_83_case__34")(x)



c__case_82_case__35 x1 x2 x3 x4 x5@(Curry.Module.FlatCurry.C_Rule x6 x7) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List)))))))))(Curry.Module.OraclePrettyFlat.c_def(Curry.Module.OraclePrettyFlat.c_qname(x3)(x4)(x1)(st))(x6)(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x9)(st))(Curry.Module.OraclePretty.c_equals(x10)(st))(x11)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_align(x12)(st))(Curry.Module.OraclePrettyFlat.c_expDoc(x2)(Curry.Module.Prelude.C_Zero)(x3)(Curry.Module.Prelude.C_False)(x7)(x13)(st))(x14)(st))(x15)(st))(x16)(st))(st)
c__case_82_case__35 x1 x2 x3 x4 x5@(Curry.Module.FlatCurry.C_External x8) st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x1)(st))(Curry.Module.OraclePrettyFlat.c_qname(x3)(x4)(x17)(st))(x18)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_keyword(x19)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))))))(x20)(st))(x21)(st))(st)
c__case_82_case__35 x1 x2 x3 x4 (Curry.Module.FlatCurry.C_RuleOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_82_case__35(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_82_case__35 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_82_case__35")(x)



c__case_81_case__36 x1 x2 x3@Curry.Module.Prelude.C_True st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_consname(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List)))(x4)(st))(st)
c__case_81_case__36 x1 x2 x3@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_literal(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_dquotes(x5)(st))(Curry.Module.OraclePretty.c_text(x2)(x6)(st))(x7)(st))(x8)(st))(st)
c__case_81_case__36 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_81_case__36(x1)(x2)(x)(st))(i)(xs)(st)
c__case_81_case__36 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_81_case__36")(x)



c__case_76_case__37 x1 x2 x4 x5 x10 x11 x12@Curry.Module.Prelude.C_True st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.op_36(Curry.Module.OraclePrettyFlat.c_par(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.c_not(Curry.Module.OraclePrelude.c_null(x11)(x1)(st))(x13)(st))(x5)(x14)(st))(x15)(st))(Curry.Module.OraclePrettyFlat.c_app(Curry.Module.OraclePrettyFlat.c_qname(x4)(x10)(x16)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_expDoc(x2)(Curry.Module.Prelude.C_Zero)(x4)(Curry.Module.Prelude.C_True)))))(x11)(x17)(st))(x18)(st))(x19)(st))(st)
c__case_76_case__37 x1 x2 x4 x5 x10 x11 x12@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_76_case__37 x1 x2 x4 x5 x10 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_76_case__37(x1)(x2)(x4)(x5)(x10)(x11)(x)(st))(i)(xs)(st)
c__case_76_case__37 x1 x2 x4 x5 x10 x11 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_76_case__37")(x)



c__case_77_case__38 x1 x2 x3 x4 x5 x10 x11 x13 x14 x15 x16@Curry.Module.Prelude.C_True st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)((Curry.Module.Prelude.:<)(x30)((Curry.Module.Prelude.:<)(x31)(Curry.Module.Prelude.List))))))))))))))))(Curry.Module.OraclePrelude.op_36(Curry.Module.OraclePretty.c_align(x1)(st))(Curry.Module.OraclePrettyFlat.c_precFillEncloseSep(x15)(x3)(x13)(x14)(Curry.Module.OraclePretty.c_empty(x17)(st))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrettyFlat.c_expDoc(x2)(x15)(x4)(Curry.Module.Prelude.C_True)(Curry.Module.OraclePrelude.op_33_33(x11)(Curry.Module.Prelude.C_Zero)(x18)(st))(x19)(st))((Curry.Module.Prelude.:<)(Curry.Module.OraclePretty.op_60_62(Curry.Module.OraclePretty.op_60_62(Curry.Module.OraclePretty.c_space(x20)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_operator(x21)(st))(Curry.Module.OraclePretty.c_text(Curry.Module.OraclePrelude.c_snd(x10)(x22)(st))(x23)(st))(x24)(st))(x25)(st))(Curry.Module.OraclePretty.c_space(x26)(st))(x27)(st))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrettyFlat.c_expDoc(x2)(x15)(x4)(Curry.Module.Prelude.C_True)(Curry.Module.OraclePrelude.op_33_33(x11)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x28)(st))(x29)(st))(Curry.Module.Prelude.List))))(x30)(st))(x31)(st))(st)
c__case_77_case__38 x1 x2 x3 x4 x5 x10 x11 x13 x14 x15 x16@Curry.Module.Prelude.C_False st = let {x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x32)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_76(x2)(x4)(x5)(x10)(x11)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x32)(st))(st)
c__case_77_case__38 x1 x2 x3 x4 x5 x10 x11 x13 x14 x15 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_77_case__38(x1)(x2)(x3)(x4)(x5)(x10)(x11)(x13)(x14)(x15)(x)(st))(i)(xs)(st)
c__case_77_case__38 x1 x2 x3 x4 x5 x10 x11 x13 x14 x15 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_77_case__38")(x)



c__case_78_case__39 x1 x2 x3 x4 x5 x10 x11 x13 x14 x15 x16@Curry.Module.Prelude.C_True st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_tupled(x1)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_expDoc(x2)(Curry.Module.Prelude.C_Zero)(x4)(Curry.Module.Prelude.C_False)))))(x11)(x17)(st))(x18)(st))(st)
c__case_78_case__39 x1 x2 x3 x4 x5 x10 x11 x13 x14 x15 x16@Curry.Module.Prelude.C_False st = let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrettyFlat.c__case_77(x2)(x3)(x4)(x5)(x10)(x11)(x13)(x14)(x15)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrettyFlat.c_isInfixName(x10)(x1)(st))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_length(x11)(x19)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x20)(st))(x21)(st))(x22)(st))(st)
c__case_78_case__39 x1 x2 x3 x4 x5 x10 x11 x13 x14 x15 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_78_case__39(x1)(x2)(x3)(x4)(x5)(x10)(x11)(x13)(x14)(x15)(x)(st))(i)(xs)(st)
c__case_78_case__39 x1 x2 x3 x4 x5 x10 x11 x13 x14 x15 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_78_case__39")(x)



c__case_79_case__40 x1 x2 x3 x4 x5 x9 x10 x11 x13 x14 x15 x16@Curry.Module.Prelude.C_True st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_36(Curry.Module.OraclePrettyFlat.c_par(x5)(x1)(st))(Curry.Module.OraclePrettyFlat.c_app(Curry.Module.OraclePrettyFlat.c_expDoc(x2)(Curry.Module.Prelude.C_Zero)(x4)(Curry.Module.Prelude.C_True)(Curry.Module.OraclePrelude.op_33_33(x11)(Curry.Module.Prelude.C_Zero)(x17)(st))(x18)(st))((Curry.Module.Prelude.:<)(Curry.Module.OraclePrettyFlat.c_expDoc(x2)(Curry.Module.Prelude.C_Zero)(x4)(Curry.Module.Prelude.C_True)(Curry.Module.OraclePrelude.op_33_33(x11)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x19)(st))(x20)(st))(Curry.Module.Prelude.List))(x21)(st))(x22)(st))(st)
c__case_79_case__40 x1 x2 x3 x4 x5 x9 x10 x11 x13 x14 x15 x16@Curry.Module.Prelude.C_False st = let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrettyFlat.c__case_78(x2)(x3)(x4)(x5)(x9)(x10)(x11)(x13)(x14)(x15)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x9)(Curry.Module.FlatCurry.C_ConsCall)(x1)(st))(Curry.Module.OraclePrettyFlat.c_isTupleName(x10)(x23)(st))(x24)(st))(x25)(st))(st)
c__case_79_case__40 x1 x2 x3 x4 x5 x9 x10 x11 x13 x14 x15 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_79_case__40(x1)(x2)(x3)(x4)(x5)(x9)(x10)(x11)(x13)(x14)(x15)(x)(st))(i)(xs)(st)
c__case_79_case__40 x1 x2 x3 x4 x5 x9 x10 x11 x13 x14 x15 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_79_case__40")(x)



c__case_75_case__41 x1 x17@(Curry.Module.Prelude.C_Just x16) st = Curry.Module.CEventOracle.c_collapse(x1)(x16)(st)
c__case_75_case__41 x1 x17@Curry.Module.Prelude.C_Nothing st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Zero)(st)
c__case_75_case__41 x1 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_75_case__41(x1)(x)(st))(i)(xs)(st)
c__case_75_case__41 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_75_case__41")(x)



c__case_74_case__42 x1 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.Prelude.T2(Curry.Module.OraclePretty.c_lparen(x1)(st))(Curry.Module.OraclePretty.c_rparen(x6)(st)))(st)
c__case_74_case__42 x1 x5@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.Prelude.T2(Curry.Module.OraclePretty.c_empty(x1)(st))(Curry.Module.OraclePretty.c_empty(x7)(st)))(st)
c__case_74_case__42 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_74_case__42(x1)(x)(st))(i)(xs)(st)
c__case_74_case__42 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_74_case__42")(x)



c__case_80_case__43 x1 x2 x3 x4 x5 x6@(Curry.Module.FlatCurry.C_Var x7) st = let {x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_varDoc(x1)(st))(x7)(x26)(st))(st)
c__case_80_case__43 x1 x2 x3 x4 x5 x6@(Curry.Module.FlatCurry.C_Lit x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c_litDoc(x8)(x1)(st))(st)
c__case_80_case__43 x1 x2 x3 x4 x5 x6@(Curry.Module.FlatCurry.C_Comb x9 x10 x11) st = let {x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List))(let {x12 = Curry.Module.OraclePrettyFlat.c__case_74(x5)(x1)(st)} in let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x27)((Curry.Module.Prelude.:<)(x28)(Curry.Module.Prelude.List))(let {x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x28)((Curry.Module.Prelude.:<)(x29)(Curry.Module.Prelude.List))(let {x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x29)((Curry.Module.Prelude.:<)(x30)((Curry.Module.Prelude.:<)(x31)(Curry.Module.Prelude.List)))(let {x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x31)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)((Curry.Module.Prelude.:<)(x34)((Curry.Module.Prelude.:<)(x35)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrettyFlat.c__case_79(x2)(x3)(x4)(x5)(x9)(x10)(x11)(Curry.Module.OraclePrettyFlat.c_expDoc2'46_'35selFP3'35lbr(x12)(x27)(st))(Curry.Module.OraclePrettyFlat.c_expDoc2'46_'35selFP4'35rbr(x12)(x28)(st))(Curry.Module.OraclePrettyFlat.c__case_75(x2)(x10)(Curry.Module.OraclePrelude.c_lookup(x10)(x2)(x29)(st))(x30)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x9)(Curry.Module.FlatCurry.C_FuncCall)(x31)(st))(Curry.Module.OraclePrelude.op_61_61(x10)(Curry.Module.Prelude.T2(Curry.Module.OraclePrettyFlat.c_prelude(x32)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(x33)(st))(x34)(st))(x35)(st))(st))(st))(st))(st))(st)
c__case_80_case__43 x1 x2 x3 x4 x5 x6@(Curry.Module.FlatCurry.C_Let x17 x18) st = let {x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x39 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x40 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x41 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x42 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x43 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x44 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x45 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x46 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x47 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x48 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x49 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x50 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x51 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x52 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x53 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x36)((Curry.Module.Prelude.:<)(x37)((Curry.Module.Prelude.:<)(x38)((Curry.Module.Prelude.:<)(x39)((Curry.Module.Prelude.:<)(x40)((Curry.Module.Prelude.:<)(x41)((Curry.Module.Prelude.:<)(x42)((Curry.Module.Prelude.:<)(x43)((Curry.Module.Prelude.:<)(x44)((Curry.Module.Prelude.:<)(x45)((Curry.Module.Prelude.:<)(x46)((Curry.Module.Prelude.:<)(x47)((Curry.Module.Prelude.:<)(x48)((Curry.Module.Prelude.:<)(x49)((Curry.Module.Prelude.:<)(x50)((Curry.Module.Prelude.:<)(x51)((Curry.Module.Prelude.:<)(x52)((Curry.Module.Prelude.:<)(x53)(Curry.Module.Prelude.List)))))))))))))))))))(Curry.Module.OraclePrelude.op_36(Curry.Module.OraclePrettyFlat.c_par(x5)(x1)(st))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_hang(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x36)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_36_62(x37)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x38)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_keyword(x39)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))(x40)(st))(x41)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_letBindsDoc(x2)(x4)(x42)(st))(x17)(x43)(st))(x44)(st))(x45)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_keyword(x46)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List)))(x47)(st))(x48)(st))(x49)(st))(Curry.Module.OraclePrettyFlat.c_expDoc(x2)(Curry.Module.Prelude.C_Zero)(x4)(Curry.Module.Prelude.C_False)(x18)(x50)(st))(x51)(st))(x52)(st))(x53)(st))(st)
c__case_80_case__43 x1 x2 x3 x4 x5 x6@(Curry.Module.FlatCurry.C_Free x19 x20) st = let {x54 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x55 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x56 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x57 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x58 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x59 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x60 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x61 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x62 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x63 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x64 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x65 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x66 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x67 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x68 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x69 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x70 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x71 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x72 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x73 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x74 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x75 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x76 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x77 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x78 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x79 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x80 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x81 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x82 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x54)((Curry.Module.Prelude.:<)(x55)((Curry.Module.Prelude.:<)(x56)((Curry.Module.Prelude.:<)(x57)((Curry.Module.Prelude.:<)(x58)((Curry.Module.Prelude.:<)(x59)((Curry.Module.Prelude.:<)(x60)((Curry.Module.Prelude.:<)(x61)((Curry.Module.Prelude.:<)(x62)((Curry.Module.Prelude.:<)(x63)((Curry.Module.Prelude.:<)(x64)((Curry.Module.Prelude.:<)(x65)((Curry.Module.Prelude.:<)(x66)((Curry.Module.Prelude.:<)(x67)((Curry.Module.Prelude.:<)(x68)((Curry.Module.Prelude.:<)(x69)((Curry.Module.Prelude.:<)(x70)((Curry.Module.Prelude.:<)(x71)((Curry.Module.Prelude.:<)(x72)((Curry.Module.Prelude.:<)(x73)((Curry.Module.Prelude.:<)(x74)((Curry.Module.Prelude.:<)(x75)((Curry.Module.Prelude.:<)(x76)((Curry.Module.Prelude.:<)(x77)((Curry.Module.Prelude.:<)(x78)((Curry.Module.Prelude.:<)(x79)((Curry.Module.Prelude.:<)(x80)((Curry.Module.Prelude.:<)(x81)((Curry.Module.Prelude.:<)(x82)(Curry.Module.Prelude.List))))))))))))))))))))))))))))))(Curry.Module.OraclePrelude.op_36(Curry.Module.OraclePrettyFlat.c_par(x5)(x1)(st))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_hang(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x54)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_36_62(x55)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x56)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x57)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_keyword(x58)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))(x59)(st))(x60)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_align(x61)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_fillSep(x62)(st))(Curry.Module.OraclePretty.c_punctuate(Curry.Module.OraclePretty.c_comma(x63)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.OraclePrettyFlat.c_varDoc(x64)(st))(x19)(x65)(st))(x66)(st))(x67)(st))(x68)(st))(x69)(st))(x70)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_keyword(x71)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))(x72)(st))(x73)(st))(x74)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_keyword(x75)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List)))(x76)(st))(x77)(st))(x78)(st))(Curry.Module.OraclePrettyFlat.c_expDoc(x2)(Curry.Module.Prelude.C_Zero)(x4)(Curry.Module.Prelude.C_False)(x20)(x79)(st))(x80)(st))(x81)(st))(x82)(st))(st)
c__case_80_case__43 x1 x2 x3 x4 x5 x6@(Curry.Module.FlatCurry.C_Or x21 x22) st = let {x83 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x83)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c_expDoc(x2)(Curry.Module.Prelude.C_Zero)(x4)(x5)(Curry.Module.FlatCurry.C_Comb(Curry.Module.FlatCurry.C_FuncCall)(Curry.Module.Prelude.T2(Curry.Module.OraclePrettyFlat.c_prelude(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('?'))(Curry.Module.Prelude.List)))((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))))(x83)(st))(st)
c__case_80_case__43 x1 x2 x3 x4 x5 x6@(Curry.Module.FlatCurry.C_Case x23 x24 x25) st = let {x84 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x85 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x86 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x87 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x88 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x89 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x90 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x91 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x92 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x93 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x94 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x95 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x96 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x97 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x98 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x99 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x100 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x101 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x102 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x103 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x104 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x105 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x106 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x84)((Curry.Module.Prelude.:<)(x85)((Curry.Module.Prelude.:<)(x86)((Curry.Module.Prelude.:<)(x87)((Curry.Module.Prelude.:<)(x88)((Curry.Module.Prelude.:<)(x89)((Curry.Module.Prelude.:<)(x90)((Curry.Module.Prelude.:<)(x91)((Curry.Module.Prelude.:<)(x92)((Curry.Module.Prelude.:<)(x93)((Curry.Module.Prelude.:<)(x94)((Curry.Module.Prelude.:<)(x95)((Curry.Module.Prelude.:<)(x96)((Curry.Module.Prelude.:<)(x97)((Curry.Module.Prelude.:<)(x98)((Curry.Module.Prelude.:<)(x99)((Curry.Module.Prelude.:<)(x100)((Curry.Module.Prelude.:<)(x101)((Curry.Module.Prelude.:<)(x102)((Curry.Module.Prelude.:<)(x103)((Curry.Module.Prelude.:<)(x104)((Curry.Module.Prelude.:<)(x105)((Curry.Module.Prelude.:<)(x106)(Curry.Module.Prelude.List))))))))))))))))))))))))(Curry.Module.OraclePrelude.op_36(Curry.Module.OraclePrettyFlat.c_par(x5)(x1)(st))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePretty.c_hang(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_36_62(x84)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x85)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x86)(st))(Curry.Module.OraclePretty.op_60_62(Curry.Module.OraclePrettyFlat.c_caseTypeDoc(x23)(x87)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_keyword(x88)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))(x89)(st))(x90)(st))(x91)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_align(x92)(st))(Curry.Module.OraclePrettyFlat.c_expDoc(x2)(Curry.Module.Prelude.C_Zero)(x4)(Curry.Module.Prelude.C_False)(x24)(x93)(st))(x94)(st))(x95)(st))(x96)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_keyword(x97)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))(Curry.Module.Prelude.List)))(x98)(st))(x99)(st))(x100)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_layout(x101)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrettyFlat.c_branchDoc(x2)(x4)))))(x25)(x102)(st))(x103)(st))(x104)(st))(x105)(st))(x106)(st))(st)
c__case_80_case__43 x1 x2 x3 x4 x5 (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_80_case__43(x1)(x2)(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_80_case__43 x1 x2 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_80_case__43")(x)



c__case_73_case__44 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_73_case__44 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_73_case__44(x1)(x)(st))(i)(xs)(st)
c__case_73_case__44 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_73_case__44")(x)



c__case_72_case__45 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_72_case__45 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_72_case__45(x1)(x)(st))(i)(xs)(st)
c__case_72_case__45 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_72_case__45")(x)



c__case_71_case__46 x1 x2 x3 x4@(Curry.Module.FlatCurry.C_Branch x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))))))))(Curry.Module.OraclePrettyFlat.c_def(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x1)(st))(Curry.Module.OraclePrettyFlat.c_patternDoc(x3)(x5)(x7)(st))(x8)(st))(Curry.Module.OraclePrettyFlat.c_arrow(x9)(st))(x10)(st))(Curry.Module.Prelude.List)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_align(x11)(st))(Curry.Module.OraclePrettyFlat.c_expDoc(x2)(Curry.Module.Prelude.C_Zero)(x3)(Curry.Module.Prelude.C_False)(x6)(x12)(st))(x13)(st))(x14)(st))(st)
c__case_71_case__46 x1 x2 x3 (Curry.Module.FlatCurry.C_BranchExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_71_case__46(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_71_case__46 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_71_case__46")(x)



c__case_70_case__47 x1 x2@Curry.Module.FlatCurry.C_Rigid st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_empty(x1)(st))(st)
c__case_70_case__47 x1 x2@Curry.Module.FlatCurry.C_Flex st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePretty.c_empty(x1)(st))(st)
c__case_70_case__47 x1 (Curry.Module.FlatCurry.C_CaseTypeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_70_case__47(x1)(x)(st))(i)(xs)(st)
c__case_70_case__47 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_70_case__47")(x)



c__case_65_case__48 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))))))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x1)(st))(Curry.Module.OraclePrettyFlat.c_qname(x2)(x4)(x7)(st))(x8)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_hsep(x9)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.OraclePrettyFlat.c_varDoc(x10)(st))(x5)(x11)(st))(x12)(st))(x13)(st))(st)
c__case_65_case__48 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_65_case__48 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_65_case__48(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_65_case__48 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_65_case__48")(x)



c__case_66_case__49 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))))))))))))(Curry.Module.OraclePretty.op_60_62(Curry.Module.OraclePretty.op_60_62(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_varDoc(x1)(st))(Curry.Module.OraclePrelude.op_33_33(x5)(Curry.Module.Prelude.C_Zero)(x7)(st))(x8)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_operator(x9)(st))(Curry.Module.OraclePretty.c_text(Curry.Module.OraclePrelude.c_snd(x4)(x10)(st))(x11)(st))(x12)(st))(x13)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_varDoc(x14)(st))(Curry.Module.OraclePrelude.op_33_33(x5)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x15)(st))(x16)(st))(x17)(st))(st)
c__case_66_case__49 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_65(x2)(x4)(x5)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x18)(st))(st)
c__case_66_case__49 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_66_case__49(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_66_case__49 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_66_case__49")(x)



c__case_67_case__50 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_tupled(x1)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.OraclePrettyFlat.c_varDoc(x7)(st))(x5)(x8)(st))(x9)(st))(st)
c__case_67_case__50 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrettyFlat.c__case_66(x2)(x4)(x5)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrettyFlat.c_isInfixName(x4)(x1)(st))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OraclePrelude.c_length(x5)(x10)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x11)(st))(x12)(st))(x13)(st))(st)
c__case_67_case__50 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_67_case__50(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_67_case__50 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_67_case__50")(x)



c__case_68_case__51 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c_qname(x2)(x4)(x1)(st))(st)
c__case_68_case__51 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_67(x2)(x4)(x5)(Curry.Module.OraclePrettyFlat.c_isTupleName(x4)(x1)(st))(x7)(st))(st)
c__case_68_case__51 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_68_case__51(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_68_case__51 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_68_case__51")(x)



c__case_69_case__52 x1 x2 x3@(Curry.Module.FlatCurry.C_Pattern x4 x5) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_68(x2)(x4)(x5)(Curry.Module.OraclePrelude.c_null(x5)(x1)(st))(x7)(st))(st)
c__case_69_case__52 x1 x2 x3@(Curry.Module.FlatCurry.C_LPattern x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c_litDoc(x6)(x1)(st))(st)
c__case_69_case__52 x1 x2 (Curry.Module.FlatCurry.C_PatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_69_case__52(x1)(x2)(x)(st))(i)(xs)(st)
c__case_69_case__52 x1 x2 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_69_case__52")(x)



c__case_64_case__53 x1 x2 x3 x4@(Curry.Module.Prelude.T2 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))))))))))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.op_60_43_62(x7)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_varDoc(x8)(st))(x5)(x9)(st))(x10)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_operator(x11)(st))(Curry.Module.OraclePretty.c_equals(x12)(st))(x13)(st))(x14)(st))(x15)(st))(Curry.Module.OraclePrettyFlat.c_expDoc(x2)(Curry.Module.Prelude.C_Zero)(x3)(Curry.Module.Prelude.C_False)(x6)(x16)(st))(x17)(st))(st)
c__case_64_case__53 x1 x2 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_64_case__53(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_64_case__53 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_64_case__53")(x)



c__case_63_case__54 x1 x2@(Curry.Module.FlatCurry.C_Intc x3) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_literal(x1)(st))(Curry.Module.OraclePretty.c_int(x3)(x6)(st))(x7)(st))(st)
c__case_63_case__54 x1 x2@(Curry.Module.FlatCurry.C_Floatc x4) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_literal(x1)(st))(Curry.Module.OraclePretty.c_float(x4)(x8)(st))(x9)(st))(st)
c__case_63_case__54 x1 x2@(Curry.Module.FlatCurry.C_Charc x5) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))))))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrettyFlat.c_literal(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePretty.c_squotes(x10)(st))(Curry.Module.OraclePretty.c_text(Curry.Module.OraclePrettyFlat.c_quoteChar(x5)(x11)(st))(x12)(st))(x13)(st))(x14)(st))(st)
c__case_63_case__54 x1 (Curry.Module.FlatCurry.C_LiteralOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_63_case__54(x1)(x)(st))(i)(xs)(st)
c__case_63_case__54 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_63_case__54")(x)



c__case_39_case__55 x1 x5@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Just(Curry.Module.Prelude.List))(st)
c__case_39_case__55 x1 x5@((Curry.Module.Prelude.:<) x26 x27) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_39_case__55 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_39_case__55(x1)(x)(st))(i)(xs)(st)
c__case_39_case__55 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_39_case__55")(x)



c__case_40_case__56 x1 x5 x25@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_39(x5)(x1)(st))(st)
c__case_40_case__56 x1 x5 x25@((Curry.Module.Prelude.:<) x28 x29) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_40_case__56 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_40_case__56(x1)(x5)(x)(st))(i)(xs)(st)
c__case_40_case__56 x1 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_40_case__56")(x)



c__case_41_case__57 x1 x5 x25 x26@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_40(x5)(x25)(x1)(st))(st)
c__case_41_case__57 x1 x5 x25 x26@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_41_case__57 x1 x5 x25 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_41_case__57(x1)(x5)(x25)(x)(st))(i)(xs)(st)
c__case_41_case__57 x1 x5 x25 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_41_case__57")(x)



c__case_42_case__58 x1 x5 x23@((Curry.Module.Prelude.:<) x24 x25) st = let {x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_41(x5)(x24)(x25)(Curry.Module.OraclePrelude.op_61_61(x24)(Curry.Module.Prelude.C_Char(']'))(x1)(st))(x26)(st))(st)
c__case_42_case__58 x1 x5 x23@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_42_case__58 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_42_case__58(x1)(x5)(x)(st))(i)(xs)(st)
c__case_42_case__58 x1 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_42_case__58")(x)



c__case_32_case__59 x1 x33 x34 x35@Curry.Module.Prelude.List st = let {x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x39 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x40 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x38)((Curry.Module.Prelude.:<)(x39)((Curry.Module.Prelude.:<)(x40)(Curry.Module.Prelude.List))))(Curry.Module.OracleMaybe.op_62_62_45(Curry.Module.OraclePrettyFlat.c_toString(x34)(x1)(st))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc(Curry.Module.Prelude.C_Just))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrettyFlat.c_quoteChar(x33)(x38)(st))))))(x39)(st))(x40)(st))(st)
c__case_32_case__59 x1 x33 x34 x35@((Curry.Module.Prelude.:<) x36 x37) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_32_case__59 x1 x33 x34 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_32_case__59(x1)(x33)(x34)(x)(st))(i)(xs)(st)
c__case_32_case__59 x1 x33 x34 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_32_case__59")(x)



c__case_33_case__60 x1 x33 x31@((Curry.Module.Prelude.:<) x34 x35) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_32(x33)(x34)(x35)(x1)(st))(st)
c__case_33_case__60 x1 x33 x31@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_33_case__60 x1 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_33_case__60(x1)(x33)(x)(st))(i)(xs)(st)
c__case_33_case__60 x1 x33 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_33_case__60")(x)



c__case_34_case__61 x1 x31 x32@(Curry.Module.FlatCurry.C_Charc x33) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_33(x33)(x31)(x1)(st))(st)
c__case_34_case__61 x1 x31 x32@(Curry.Module.FlatCurry.C_Intc x38) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_34_case__61 x1 x31 x32@(Curry.Module.FlatCurry.C_Floatc x39) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_34_case__61 x1 x31 (Curry.Module.FlatCurry.C_LiteralOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_34_case__61(x1)(x31)(x)(st))(i)(xs)(st)
c__case_34_case__61 x1 x31 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_34_case__61")(x)



c__case_35_case__62 x1 x31 x30@(Curry.Module.FlatCurry.C_Lit x32) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_34(x31)(x32)(x1)(st))(st)
c__case_35_case__62 x1 x31 x30@(Curry.Module.FlatCurry.C_Var x40) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_35_case__62 x1 x31 x30@(Curry.Module.FlatCurry.C_Comb x41 x42 x43) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_35_case__62 x1 x31 x30@(Curry.Module.FlatCurry.C_Let x44 x45) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_35_case__62 x1 x31 x30@(Curry.Module.FlatCurry.C_Free x46 x47) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_35_case__62 x1 x31 x30@(Curry.Module.FlatCurry.C_Or x48 x49) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_35_case__62 x1 x31 x30@(Curry.Module.FlatCurry.C_Case x50 x51 x52) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_35_case__62 x1 x31 (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_35_case__62(x1)(x31)(x)(st))(i)(xs)(st)
c__case_35_case__62 x1 x31 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_35_case__62")(x)



c__case_36_case__63 x1 x5@((Curry.Module.Prelude.:<) x30 x31) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_35(x31)(x30)(x1)(st))(st)
c__case_36_case__63 x1 x5@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_36_case__63 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_36_case__63(x1)(x)(st))(i)(xs)(st)
c__case_36_case__63 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_36_case__63")(x)



c__case_37_case__64 x1 x5 x23@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_36(x5)(x1)(st))(st)
c__case_37_case__64 x1 x5 x23@((Curry.Module.Prelude.:<) x53 x54) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_37_case__64 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_37_case__64(x1)(x5)(x)(st))(i)(xs)(st)
c__case_37_case__64 x1 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_37_case__64")(x)



c__case_38_case__65 x1 x5 x23 x24@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_37(x5)(x23)(x1)(st))(st)
c__case_38_case__65 x1 x5 x23 x24@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_38_case__65 x1 x5 x23 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_38_case__65(x1)(x5)(x23)(x)(st))(i)(xs)(st)
c__case_38_case__65 x1 x5 x23 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_38_case__65")(x)



c__case_43_case__66 x1 x5 x22 x23 x24@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_42(x5)(x23)(x1)(st))(st)
c__case_43_case__66 x1 x5 x22 x23 x24@Curry.Module.Prelude.C_False st = let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_38(x5)(x22)(x23)(Curry.Module.OraclePrelude.op_61_61(x22)(Curry.Module.Prelude.C_Char(':'))(x1)(st))(x25)(st))(st)
c__case_43_case__66 x1 x5 x22 x23 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_43_case__66(x1)(x5)(x22)(x23)(x)(st))(i)(xs)(st)
c__case_43_case__66 x1 x5 x22 x23 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_43_case__66")(x)



c__case_44_case__67 x1 x5 x7@((Curry.Module.Prelude.:<) x22 x23) st = let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_43(x5)(x22)(x23)(Curry.Module.OraclePrelude.op_61_61(x22)(Curry.Module.Prelude.C_Char('['))(x1)(st))(x24)(st))(st)
c__case_44_case__67 x1 x5 x7@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_44_case__67 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_44_case__67(x1)(x5)(x)(st))(i)(xs)(st)
c__case_44_case__67 x1 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_44_case__67")(x)



c__case_45_case__68 x1 x5 x7 x21@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_44(x5)(x7)(x1)(st))(st)
c__case_45_case__68 x1 x5 x7 x21@((Curry.Module.Prelude.:<) x55 x56) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_45_case__68 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_45_case__68(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_45_case__68 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_45_case__68")(x)



c__case_46_case__69 x1 x5 x7 x21 x22@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_45(x5)(x7)(x21)(x1)(st))(st)
c__case_46_case__69 x1 x5 x7 x21 x22@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_46_case__69 x1 x5 x7 x21 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_46_case__69(x1)(x5)(x7)(x21)(x)(st))(i)(xs)(st)
c__case_46_case__69 x1 x5 x7 x21 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_46_case__69")(x)



c__case_47_case__70 x1 x5 x7 x19@((Curry.Module.Prelude.:<) x20 x21) st = let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_46(x5)(x7)(x20)(x21)(Curry.Module.OraclePrelude.op_61_61(x20)(Curry.Module.Prelude.C_Char('e'))(x1)(st))(x22)(st))(st)
c__case_47_case__70 x1 x5 x7 x19@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_47_case__70 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_47_case__70(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_47_case__70 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_47_case__70")(x)



c__case_48_case__71 x1 x5 x7 x19 x20@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_47(x5)(x7)(x19)(x1)(st))(st)
c__case_48_case__71 x1 x5 x7 x19 x20@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_48_case__71 x1 x5 x7 x19 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_48_case__71(x1)(x5)(x7)(x19)(x)(st))(i)(xs)(st)
c__case_48_case__71 x1 x5 x7 x19 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_48_case__71")(x)



c__case_49_case__72 x1 x5 x7 x17@((Curry.Module.Prelude.:<) x18 x19) st = let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_48(x5)(x7)(x18)(x19)(Curry.Module.OraclePrelude.op_61_61(x18)(Curry.Module.Prelude.C_Char('d'))(x1)(st))(x20)(st))(st)
c__case_49_case__72 x1 x5 x7 x17@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_49_case__72 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_49_case__72(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_49_case__72 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_49_case__72")(x)



c__case_50_case__73 x1 x5 x7 x17 x18@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_49(x5)(x7)(x17)(x1)(st))(st)
c__case_50_case__73 x1 x5 x7 x17 x18@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_50_case__73 x1 x5 x7 x17 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_50_case__73(x1)(x5)(x7)(x17)(x)(st))(i)(xs)(st)
c__case_50_case__73 x1 x5 x7 x17 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_50_case__73")(x)



c__case_51_case__74 x1 x5 x7 x15@((Curry.Module.Prelude.:<) x16 x17) st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_50(x5)(x7)(x16)(x17)(Curry.Module.OraclePrelude.op_61_61(x16)(Curry.Module.Prelude.C_Char('u'))(x1)(st))(x18)(st))(st)
c__case_51_case__74 x1 x5 x7 x15@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_51_case__74 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_51_case__74(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_51_case__74 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_51_case__74")(x)



c__case_52_case__75 x1 x5 x7 x15 x16@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_51(x5)(x7)(x15)(x1)(st))(st)
c__case_52_case__75 x1 x5 x7 x15 x16@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_52_case__75 x1 x5 x7 x15 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_52_case__75(x1)(x5)(x7)(x15)(x)(st))(i)(xs)(st)
c__case_52_case__75 x1 x5 x7 x15 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_52_case__75")(x)



c__case_53_case__76 x1 x5 x7 x13@((Curry.Module.Prelude.:<) x14 x15) st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_52(x5)(x7)(x14)(x15)(Curry.Module.OraclePrelude.op_61_61(x14)(Curry.Module.Prelude.C_Char('l'))(x1)(st))(x16)(st))(st)
c__case_53_case__76 x1 x5 x7 x13@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_53_case__76 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_53_case__76(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_53_case__76 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_53_case__76")(x)



c__case_54_case__77 x1 x5 x7 x13 x14@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_53(x5)(x7)(x13)(x1)(st))(st)
c__case_54_case__77 x1 x5 x7 x13 x14@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_54_case__77 x1 x5 x7 x13 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_54_case__77(x1)(x5)(x7)(x13)(x)(st))(i)(xs)(st)
c__case_54_case__77 x1 x5 x7 x13 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_54_case__77")(x)



c__case_55_case__78 x1 x5 x7 x11@((Curry.Module.Prelude.:<) x12 x13) st = let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_54(x5)(x7)(x12)(x13)(Curry.Module.OraclePrelude.op_61_61(x12)(Curry.Module.Prelude.C_Char('e'))(x1)(st))(x14)(st))(st)
c__case_55_case__78 x1 x5 x7 x11@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_55_case__78 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_55_case__78(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_55_case__78 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_55_case__78")(x)



c__case_56_case__79 x1 x5 x7 x11 x12@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_55(x5)(x7)(x11)(x1)(st))(st)
c__case_56_case__79 x1 x5 x7 x11 x12@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_56_case__79 x1 x5 x7 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_56_case__79(x1)(x5)(x7)(x11)(x)(st))(i)(xs)(st)
c__case_56_case__79 x1 x5 x7 x11 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_56_case__79")(x)



c__case_57_case__80 x1 x5 x7 x9@((Curry.Module.Prelude.:<) x10 x11) st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_56(x5)(x7)(x10)(x11)(Curry.Module.OraclePrelude.op_61_61(x10)(Curry.Module.Prelude.C_Char('r'))(x1)(st))(x12)(st))(st)
c__case_57_case__80 x1 x5 x7 x9@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_57_case__80 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_57_case__80(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_57_case__80 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_57_case__80")(x)



c__case_58_case__81 x1 x5 x7 x9 x10@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_57(x5)(x7)(x9)(x1)(st))(st)
c__case_58_case__81 x1 x5 x7 x9 x10@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_58_case__81 x1 x5 x7 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_58_case__81(x1)(x5)(x7)(x9)(x)(st))(i)(xs)(st)
c__case_58_case__81 x1 x5 x7 x9 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_58_case__81")(x)



c__case_59_case__82 x1 x5 x7 x6@((Curry.Module.Prelude.:<) x8 x9) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_58(x5)(x7)(x8)(x9)(Curry.Module.OraclePrelude.op_61_61(x8)(Curry.Module.Prelude.C_Char('P'))(x1)(st))(x10)(st))(st)
c__case_59_case__82 x1 x5 x7 x6@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_59_case__82 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_59_case__82(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_59_case__82 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_59_case__82")(x)



c__case_60_case__83 x1 x5 x4@(Curry.Module.Prelude.T2 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_59(x5)(x7)(x6)(x1)(st))(st)
c__case_60_case__83 x1 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_60_case__83(x1)(x5)(x)(st))(i)(xs)(st)
c__case_60_case__83 x1 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_60_case__83")(x)



c__case_61_case__84 x1 x4 x5 x3@Curry.Module.FlatCurry.C_ConsCall st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_60(x5)(x4)(x1)(st))(st)
c__case_61_case__84 x1 x4 x5 x3@Curry.Module.FlatCurry.C_FuncCall st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_61_case__84 x1 x4 x5 x3@(Curry.Module.FlatCurry.C_FuncPartCall x57) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_61_case__84 x1 x4 x5 x3@(Curry.Module.FlatCurry.C_ConsPartCall x58) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_61_case__84 x1 x4 x5 (Curry.Module.FlatCurry.C_CombTypeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_61_case__84(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_61_case__84 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_61_case__84")(x)



c__case_62_case__85 x1 x2@(Curry.Module.FlatCurry.C_Comb x3 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_61(x4)(x5)(x3)(x1)(st))(st)
c__case_62_case__85 x1 x2@(Curry.Module.FlatCurry.C_Var x59) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_62_case__85 x1 x2@(Curry.Module.FlatCurry.C_Lit x60) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_62_case__85 x1 x2@(Curry.Module.FlatCurry.C_Let x61 x62) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_62_case__85 x1 x2@(Curry.Module.FlatCurry.C_Free x63 x64) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_62_case__85 x1 x2@(Curry.Module.FlatCurry.C_Or x65 x66) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_62_case__85 x1 x2@(Curry.Module.FlatCurry.C_Case x67 x68 x69) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_62_case__85 x1 (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_62_case__85(x1)(x)(st))(i)(xs)(st)
c__case_62_case__85 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_62_case__85")(x)



c__case_8_case__86 x1 x5@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Just(Curry.Module.Prelude.List))(st)
c__case_8_case__86 x1 x5@((Curry.Module.Prelude.:<) x26 x27) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_8_case__86 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_8_case__86(x1)(x)(st))(i)(xs)(st)
c__case_8_case__86 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_8_case__86")(x)



c__case_9_case__87 x1 x5 x25@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_8(x5)(x1)(st))(st)
c__case_9_case__87 x1 x5 x25@((Curry.Module.Prelude.:<) x28 x29) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_9_case__87 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_9_case__87(x1)(x5)(x)(st))(i)(xs)(st)
c__case_9_case__87 x1 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_9_case__87")(x)



c__case_10_case__88 x1 x5 x25 x26@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_9(x5)(x25)(x1)(st))(st)
c__case_10_case__88 x1 x5 x25 x26@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_10_case__88 x1 x5 x25 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_10_case__88(x1)(x5)(x25)(x)(st))(i)(xs)(st)
c__case_10_case__88 x1 x5 x25 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_10_case__88")(x)



c__case_11_case__89 x1 x5 x23@((Curry.Module.Prelude.:<) x24 x25) st = let {x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_10(x5)(x24)(x25)(Curry.Module.OraclePrelude.op_61_61(x24)(Curry.Module.Prelude.C_Char(']'))(x1)(st))(x26)(st))(st)
c__case_11_case__89 x1 x5 x23@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_11_case__89 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_11_case__89(x1)(x5)(x)(st))(i)(xs)(st)
c__case_11_case__89 x1 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_11_case__89")(x)



c__case_3_case__90 x1 x30 x32 x33@Curry.Module.Prelude.List st = let {x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x36)((Curry.Module.Prelude.:<)(x37)(Curry.Module.Prelude.List)))(Curry.Module.OracleMaybe.op_62_62_45(Curry.Module.OraclePrettyFlat.c_toList(x32)(x1)(st))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc(Curry.Module.Prelude.C_Just))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc((Curry.Module.Prelude.:<)(x30)))))(x36)(st))(x37)(st))(st)
c__case_3_case__90 x1 x30 x32 x33@((Curry.Module.Prelude.:<) x34 x35) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_3_case__90 x1 x30 x32 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_3_case__90(x1)(x30)(x32)(x)(st))(i)(xs)(st)
c__case_3_case__90 x1 x30 x32 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_3_case__90")(x)



c__case_4_case__91 x1 x30 x31@((Curry.Module.Prelude.:<) x32 x33) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_3(x30)(x32)(x33)(x1)(st))(st)
c__case_4_case__91 x1 x30 x31@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_4_case__91 x1 x30 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_4_case__91(x1)(x30)(x)(st))(i)(xs)(st)
c__case_4_case__91 x1 x30 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_4_case__91")(x)



c__case_5_case__92 x1 x5@((Curry.Module.Prelude.:<) x30 x31) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_4(x30)(x31)(x1)(st))(st)
c__case_5_case__92 x1 x5@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_5_case__92 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_5_case__92(x1)(x)(st))(i)(xs)(st)
c__case_5_case__92 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_5_case__92")(x)



c__case_6_case__93 x1 x5 x23@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_5(x5)(x1)(st))(st)
c__case_6_case__93 x1 x5 x23@((Curry.Module.Prelude.:<) x36 x37) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_6_case__93 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_6_case__93(x1)(x5)(x)(st))(i)(xs)(st)
c__case_6_case__93 x1 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_6_case__93")(x)



c__case_7_case__94 x1 x5 x23 x24@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_6(x5)(x23)(x1)(st))(st)
c__case_7_case__94 x1 x5 x23 x24@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_7_case__94 x1 x5 x23 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_7_case__94(x1)(x5)(x23)(x)(st))(i)(xs)(st)
c__case_7_case__94 x1 x5 x23 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_7_case__94")(x)



c__case_12_case__95 x1 x5 x22 x23 x24@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_11(x5)(x23)(x1)(st))(st)
c__case_12_case__95 x1 x5 x22 x23 x24@Curry.Module.Prelude.C_False st = let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_7(x5)(x22)(x23)(Curry.Module.OraclePrelude.op_61_61(x22)(Curry.Module.Prelude.C_Char(':'))(x1)(st))(x25)(st))(st)
c__case_12_case__95 x1 x5 x22 x23 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_12_case__95(x1)(x5)(x22)(x23)(x)(st))(i)(xs)(st)
c__case_12_case__95 x1 x5 x22 x23 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_12_case__95")(x)



c__case_13_case__96 x1 x5 x7@((Curry.Module.Prelude.:<) x22 x23) st = let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_12(x5)(x22)(x23)(Curry.Module.OraclePrelude.op_61_61(x22)(Curry.Module.Prelude.C_Char('['))(x1)(st))(x24)(st))(st)
c__case_13_case__96 x1 x5 x7@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_13_case__96 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_13_case__96(x1)(x5)(x)(st))(i)(xs)(st)
c__case_13_case__96 x1 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_13_case__96")(x)



c__case_14_case__97 x1 x5 x7 x21@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_13(x5)(x7)(x1)(st))(st)
c__case_14_case__97 x1 x5 x7 x21@((Curry.Module.Prelude.:<) x38 x39) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_14_case__97 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_14_case__97(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_14_case__97 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_14_case__97")(x)



c__case_15_case__98 x1 x5 x7 x21 x22@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_14(x5)(x7)(x21)(x1)(st))(st)
c__case_15_case__98 x1 x5 x7 x21 x22@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_15_case__98 x1 x5 x7 x21 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_15_case__98(x1)(x5)(x7)(x21)(x)(st))(i)(xs)(st)
c__case_15_case__98 x1 x5 x7 x21 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_15_case__98")(x)



c__case_16_case__99 x1 x5 x7 x19@((Curry.Module.Prelude.:<) x20 x21) st = let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_15(x5)(x7)(x20)(x21)(Curry.Module.OraclePrelude.op_61_61(x20)(Curry.Module.Prelude.C_Char('e'))(x1)(st))(x22)(st))(st)
c__case_16_case__99 x1 x5 x7 x19@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_16_case__99 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_16_case__99(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_16_case__99 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_16_case__99")(x)



c__case_17_case__100 x1 x5 x7 x19 x20@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_16(x5)(x7)(x19)(x1)(st))(st)
c__case_17_case__100 x1 x5 x7 x19 x20@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_17_case__100 x1 x5 x7 x19 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_17_case__100(x1)(x5)(x7)(x19)(x)(st))(i)(xs)(st)
c__case_17_case__100 x1 x5 x7 x19 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_17_case__100")(x)



c__case_18_case__101 x1 x5 x7 x17@((Curry.Module.Prelude.:<) x18 x19) st = let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_17(x5)(x7)(x18)(x19)(Curry.Module.OraclePrelude.op_61_61(x18)(Curry.Module.Prelude.C_Char('d'))(x1)(st))(x20)(st))(st)
c__case_18_case__101 x1 x5 x7 x17@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_18_case__101 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_18_case__101(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_18_case__101 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_18_case__101")(x)



c__case_19_case__102 x1 x5 x7 x17 x18@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_18(x5)(x7)(x17)(x1)(st))(st)
c__case_19_case__102 x1 x5 x7 x17 x18@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_19_case__102 x1 x5 x7 x17 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_19_case__102(x1)(x5)(x7)(x17)(x)(st))(i)(xs)(st)
c__case_19_case__102 x1 x5 x7 x17 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_19_case__102")(x)



c__case_20_case__103 x1 x5 x7 x15@((Curry.Module.Prelude.:<) x16 x17) st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_19(x5)(x7)(x16)(x17)(Curry.Module.OraclePrelude.op_61_61(x16)(Curry.Module.Prelude.C_Char('u'))(x1)(st))(x18)(st))(st)
c__case_20_case__103 x1 x5 x7 x15@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_20_case__103 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_20_case__103(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_20_case__103 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_20_case__103")(x)



c__case_21_case__104 x1 x5 x7 x15 x16@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_20(x5)(x7)(x15)(x1)(st))(st)
c__case_21_case__104 x1 x5 x7 x15 x16@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_21_case__104 x1 x5 x7 x15 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_21_case__104(x1)(x5)(x7)(x15)(x)(st))(i)(xs)(st)
c__case_21_case__104 x1 x5 x7 x15 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_21_case__104")(x)



c__case_22_case__105 x1 x5 x7 x13@((Curry.Module.Prelude.:<) x14 x15) st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_21(x5)(x7)(x14)(x15)(Curry.Module.OraclePrelude.op_61_61(x14)(Curry.Module.Prelude.C_Char('l'))(x1)(st))(x16)(st))(st)
c__case_22_case__105 x1 x5 x7 x13@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_22_case__105 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_22_case__105(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_22_case__105 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_22_case__105")(x)



c__case_23_case__106 x1 x5 x7 x13 x14@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_22(x5)(x7)(x13)(x1)(st))(st)
c__case_23_case__106 x1 x5 x7 x13 x14@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_23_case__106 x1 x5 x7 x13 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_23_case__106(x1)(x5)(x7)(x13)(x)(st))(i)(xs)(st)
c__case_23_case__106 x1 x5 x7 x13 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_23_case__106")(x)



c__case_24_case__107 x1 x5 x7 x11@((Curry.Module.Prelude.:<) x12 x13) st = let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_23(x5)(x7)(x12)(x13)(Curry.Module.OraclePrelude.op_61_61(x12)(Curry.Module.Prelude.C_Char('e'))(x1)(st))(x14)(st))(st)
c__case_24_case__107 x1 x5 x7 x11@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_24_case__107 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_24_case__107(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_24_case__107 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_24_case__107")(x)



c__case_25_case__108 x1 x5 x7 x11 x12@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_24(x5)(x7)(x11)(x1)(st))(st)
c__case_25_case__108 x1 x5 x7 x11 x12@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_25_case__108 x1 x5 x7 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_25_case__108(x1)(x5)(x7)(x11)(x)(st))(i)(xs)(st)
c__case_25_case__108 x1 x5 x7 x11 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_25_case__108")(x)



c__case_26_case__109 x1 x5 x7 x9@((Curry.Module.Prelude.:<) x10 x11) st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_25(x5)(x7)(x10)(x11)(Curry.Module.OraclePrelude.op_61_61(x10)(Curry.Module.Prelude.C_Char('r'))(x1)(st))(x12)(st))(st)
c__case_26_case__109 x1 x5 x7 x9@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_26_case__109 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_26_case__109(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_26_case__109 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_26_case__109")(x)



c__case_27_case__110 x1 x5 x7 x9 x10@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_26(x5)(x7)(x9)(x1)(st))(st)
c__case_27_case__110 x1 x5 x7 x9 x10@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_27_case__110 x1 x5 x7 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_27_case__110(x1)(x5)(x7)(x9)(x)(st))(i)(xs)(st)
c__case_27_case__110 x1 x5 x7 x9 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_27_case__110")(x)



c__case_28_case__111 x1 x5 x7 x6@((Curry.Module.Prelude.:<) x8 x9) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_27(x5)(x7)(x8)(x9)(Curry.Module.OraclePrelude.op_61_61(x8)(Curry.Module.Prelude.C_Char('P'))(x1)(st))(x10)(st))(st)
c__case_28_case__111 x1 x5 x7 x6@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_28_case__111 x1 x5 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_28_case__111(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_28_case__111 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_28_case__111")(x)



c__case_29_case__112 x1 x5 x4@(Curry.Module.Prelude.T2 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_28(x5)(x7)(x6)(x1)(st))(st)
c__case_29_case__112 x1 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_29_case__112(x1)(x5)(x)(st))(i)(xs)(st)
c__case_29_case__112 x1 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_29_case__112")(x)



c__case_30_case__113 x1 x4 x5 x3@Curry.Module.FlatCurry.C_ConsCall st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_29(x5)(x4)(x1)(st))(st)
c__case_30_case__113 x1 x4 x5 x3@Curry.Module.FlatCurry.C_FuncCall st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_30_case__113 x1 x4 x5 x3@(Curry.Module.FlatCurry.C_FuncPartCall x40) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_30_case__113 x1 x4 x5 x3@(Curry.Module.FlatCurry.C_ConsPartCall x41) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_30_case__113 x1 x4 x5 (Curry.Module.FlatCurry.C_CombTypeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_30_case__113(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_30_case__113 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_30_case__113")(x)



c__case_31_case__114 x1 x2@(Curry.Module.FlatCurry.C_Comb x3 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrettyFlat.c__case_30(x4)(x5)(x3)(x1)(st))(st)
c__case_31_case__114 x1 x2@(Curry.Module.FlatCurry.C_Var x42) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_31_case__114 x1 x2@(Curry.Module.FlatCurry.C_Lit x43) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_31_case__114 x1 x2@(Curry.Module.FlatCurry.C_Let x44 x45) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_31_case__114 x1 x2@(Curry.Module.FlatCurry.C_Free x46 x47) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_31_case__114 x1 x2@(Curry.Module.FlatCurry.C_Or x48 x49) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_31_case__114 x1 x2@(Curry.Module.FlatCurry.C_Case x50 x51 x52) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_Nothing)(st)
c__case_31_case__114 x1 (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_31_case__114(x1)(x)(st))(i)(xs)(st)
c__case_31_case__114 x1 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_31_case__114")(x)



c__case_2_case__115 x1 x3 x2@(Curry.Module.FlatCurry.C_Comb x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.FlatCurry.C_Comb(x4)(x5)(Curry.Module.OraclePrelude.op_43_43(x6)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(x1)(st)))(st)
c__case_2_case__115 x1 x3 (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_2_case__115(x1)(x3)(x)(st))(i)(xs)(st)
c__case_2_case__115 x1 x3 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_2_case__115")(x)



c__case_0_case__116 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlatCurry.C_Comb(x2)(x3)(x4))(st)
c__case_0_case__116 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_0_case__116 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_0_case__116(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_0_case__116 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_0_case__116")(x)



c__case_1_case__117 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrettyFlat.c_elimApp'46extend'46276(Curry.Module.OraclePrelude.c_head(x4)(x1)(st))(Curry.Module.OraclePrelude.op_33_33(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x6)(st))(x7)(st))(st)
c__case_1_case__117 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OraclePrettyFlat.c__case_0(x2)(x3)(x4)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x8)(st))(st)
c__case_1_case__117 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OraclePrettyFlat.c__case_1_case__117(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_1_case__117 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OraclePrettyFlat._case_1_case__117")(x)


