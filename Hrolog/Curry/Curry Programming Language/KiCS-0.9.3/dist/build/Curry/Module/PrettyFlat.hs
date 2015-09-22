{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.PrettyFlat (module Curry.Module.PrettyFlat) where

import Curry.RunTimeSystem
import Curry.Module.Char
import Curry.Module.FlatCurry
import Curry.Module.FlatCurryGoodies
import Curry.Module.Maybe
import Curry.Module.Prelude
import Curry.Module.Pretty
import Curry.Module.StyledText
import Curry.Module.System



-- begin included



-- end included

type C_Precs = Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)

c_prelude :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_prelude st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))



c_arrow :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_arrow st = Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_operator(st))(Curry.Module.Pretty.c_text((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))(Curry.Module.Prelude.List)))(st))(st)



c_bar :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_bar st = Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_operator(st))(Curry.Module.Pretty.c_char(Curry.Module.Prelude.C_Char('|'))(st))(st)



c_dcolon :: Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_dcolon st = Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_operator(st))(Curry.Module.Pretty.c_text((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List)))(st))(st)



c_precFillEncloseSep :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Pretty.C_Doc -> Curry.Module.Pretty.C_Doc -> Curry.Module.Pretty.C_Doc -> (Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_precFillEncloseSep x1 x2 x3 x4 x5 x6 st = Curry.Module.Pretty.c_fillEncloseSep(Curry.Module.PrettyFlat.c_precFillEncloseSep'46pre'4611(x1)(x2)(x3)(st))(Curry.Module.PrettyFlat.c_precFillEncloseSep'46pre'4611(x1)(x2)(x4)(st))(x5)(x6)(st)



c_precFillEncloseSep'46pre'4611 :: Curry.Module.Prelude.C_Int -> Curry.Module.Prelude.C_Int -> Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_precFillEncloseSep'46pre'4611 x1 x2 x3 st = Curry.Module.PrettyFlat.c_precFillEncloseSep'46pre'4611_case_90(x1)(x2)(x3)(Curry.Module.Prelude.op_60(x1)(x2)(st))(st)



c_isInfixName :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isInfixName x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_all(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_elem))(Curry.Module.PrettyFlat.c_infixIDs(st))))(st))(x3)(st)
c_isInfixName (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_isInfixName(x)(st))(i)(xs)(st)
c_isInfixName x st = Curry.RunTimeSystem.patternFail("PrettyFlat.isInfixName")(x)



c_infixIDs :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_infixIDs st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('~'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('@'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('#'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('$'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('%'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('^'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('*'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('+'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('?'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('|'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))))))))))))))))))))



c_isTupleName :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isTupleName x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.PrettyFlat.c_prelude(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_elem(Curry.Module.Prelude.c_take(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(x3)(st))(st))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List)))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.List)))(st))(st)
c_isTupleName (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_isTupleName(x)(st))(i)(xs)(st)
c_isTupleName x st = Curry.RunTimeSystem.patternFail("PrettyFlat.isTupleName")(x)



c_showStyledProg :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_showStyledProg st = Curry.Module.PrettyFlat.c_prettyProg(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))(st)



c_prettyProg :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_prettyProg x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_pretty(x1)))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_progDoc))(Curry.Module.Prelude.c_apply(Curry.Module.FlatCurryGoodies.c_updProgExps(st))(Curry.Module.PrettyFlat.c_elimApp(st))(st))(st))(st)



c_prettyTypeExpr :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_TypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_prettyTypeExpr x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_pretty(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))))(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_typeExprDoc(x1)(Curry.Module.Prelude.C_False)))(st)



c_prettyTypes :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_TypeDecl) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_prettyTypes x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_pretty(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))))(Curry.Module.PrettyFlat.c_typesDoc(x1)(st))(st)



c_prettyOps :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_OpDecl) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_prettyOps st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_pretty(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi))))))))))(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_opsDoc))(st)



c_showProg :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_showProg st = Curry.Module.Prelude.op_46(Curry.Module.StyledText.c_plainText(st))(Curry.Module.PrettyFlat.c_showStyledProg(st))(st)



c_printStyledProg :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_printStyledProg x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.FlatCurry.c_readFlatCurry(x1)(st))(Curry.Module.Prelude.op_46(Curry.Module.StyledText.c_printStyledText(st))(Curry.Module.PrettyFlat.c_showStyledProg(st))(st))(st)



c_mainPrint :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_mainPrint st = Curry.Module.Prelude.op_62_62_61(Curry.Module.System.c_getArgs(st))(Curry.Module.Prelude.c_mapIO_(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_printProg))(st))(st)



c_printProg :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_printProg x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.FlatCurry.c_readFlatCurryFile(x1)(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_putStrLn))(Curry.Module.PrettyFlat.c_showProg(st))(st))(st)



c_keyword :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_keyword st = Curry.Module.Prelude.op_46(Curry.Module.StyledText.c_magentaDoc(st))(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_text))(st)



c_consname :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_consname st = Curry.Module.Prelude.op_46(Curry.Module.StyledText.c_greenDoc(st))(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_text))(st)



c_operator :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_operator st = Curry.Module.StyledText.c_blueDoc(st)



c_literal :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_literal st = Curry.Module.StyledText.c_cyanDoc(st)



c_marked :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_marked st = Curry.Module.Prelude.op_46(Curry.Module.StyledText.c_bgYellowDoc(st))(Curry.Module.StyledText.c_boldDoc(st))(st)



c_block :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_block st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_group))(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_hang(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))))(st)



c_def :: Curry.Module.Pretty.C_Doc -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_def x1 x2 x3 st = Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_block(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_36_62(st))(Curry.Module.Pretty.op_60_62(x1)(Curry.Module.PrettyFlat.c_def_case_89(x2)(Curry.Module.Prelude.c_null(x2)(st))(st))(st))(st))(x3)(st))(st)



c_app :: Curry.Module.Pretty.C_Doc -> (Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_app x1 x2 st = Curry.Module.PrettyFlat.c_app_case_88(x1)(x2)(Curry.Module.Prelude.c_null(x2)(st))(st)



c_layout :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_layout st = Curry.Module.Prelude.op_46(Curry.Module.Pretty.c_align(st))(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.c_combine(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_linesep(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(';'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(st))))))(st)



c_qname :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_qname x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.PrettyFlat.c_qname_case_87(x1)(x2)(x3)(x4)(Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.T2(Curry.Module.PrettyFlat.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))))(st))(Curry.Module.PrettyFlat.c_isTupleName(x2)(st))(st))(st)
c_qname x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_qname(x1)(x)(st))(i)(xs)(st)
c_qname x1 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.qname")(x)



c_qname'46txt'4665 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_qname'46txt'4665 x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.PrettyFlat.c_qname'46txt'4665_case_82(x2)(x3)(Curry.Module.Char.c_isUpper(x2)(st))(st)
c_qname'46txt'4665 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_qname'46txt'4665(x)(st))(i)(xs)(st)
c_qname'46txt'4665 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.qname.txt.65")(x)



c_correctName :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_correctName x1 st = let {x2 = Curry.Module.Prelude.c_filter(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_not))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_elem))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('#'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List)))))(st))(x1)(st)} in Curry.Module.PrettyFlat.c_correctName_case_81(x2)(st)



op_60_36_62_62 :: Curry.Module.Pretty.C_Doc -> Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
op_60_36_62_62 x1 x2 st = Curry.Module.Pretty.op_60_62(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_36_62(st))(x1)(st))(Curry.Module.Pretty.c_line(st))(st))(x2)(st)



c_progDoc :: Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_progDoc x1@(Curry.Module.FlatCurry.C_Prog x2 x3 x4 x5 x6) st = Curry.Module.PrettyFlat.op_60_36_62_62(Curry.Module.PrettyFlat.op_60_36_62_62(Curry.Module.PrettyFlat.op_60_36_62_62(Curry.Module.PrettyFlat.op_60_36_62_62(Curry.Module.PrettyFlat.c_moduleHeaderDoc(x2)(Curry.Module.PrettyFlat.c_exportedNames(x2)(x1)(st))(st))(Curry.Module.PrettyFlat.c_impsDoc(x3)(st))(st))(Curry.Module.PrettyFlat.c_opsDoc(x6)(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_typesDoc(x2)(st))(x4)(st))(st))(Curry.Module.PrettyFlat.c_funcsDoc(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_precs(st))(x6)(st))(x2)(x5)(st))(st)
c_progDoc (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_progDoc(x)(st))(i)(xs)(st)
c_progDoc x st = Curry.RunTimeSystem.patternFail("PrettyFlat.progDoc")(x)



c_precs :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_OpDecl) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int))
c_precs st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_precs'46_'35lambda3)))



c_precs'46_'35lambda3 :: Curry.Module.FlatCurry.C_OpDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int
c_precs'46_'35lambda3 x1@(Curry.Module.FlatCurry.C_Op x2 x3 x4) st = Curry.Module.Prelude.T2(x2)(x4)
c_precs'46_'35lambda3 (Curry.Module.FlatCurry.C_OpDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_precs'46_'35lambda3(x)(st))(i)(xs)(st)
c_precs'46_'35lambda3 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.precs._#lambda3")(x)



c_exportedNames :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_Prog -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc
c_exportedNames x1 x2@(Curry.Module.FlatCurry.C_Prog x3 x4 x5 x6 x7) st = Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_exportedNames'46typeExpDoc'4689(x1)))(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_61_61(Curry.Module.FlatCurry.C_Public)))(Curry.Module.FlatCurryGoodies.c_typeVisibility(st))(st))(x5)(st))(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_qname(x1)))(Curry.Module.FlatCurryGoodies.c_funcName(st))(st))(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_61_61(Curry.Module.FlatCurry.C_Public)))(Curry.Module.FlatCurryGoodies.c_funcVisibility(st))(st))(x6)(st))(st))(st)
c_exportedNames x1 (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_exportedNames(x1)(x)(st))(i)(xs)(st)
c_exportedNames x1 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.exportedNames")(x)



c_exportedNames'46typeExpDoc'4689 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_TypeDecl -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_exportedNames'46typeExpDoc'4689 x1 x2 st = Curry.Module.Pretty.op_60_62(Curry.Module.PrettyFlat.c_qname(x1)(Curry.Module.Prelude.c_apply(Curry.Module.FlatCurryGoodies.c_typeName(st))(x2)(st))(st))(Curry.Module.PrettyFlat.c_exportedNames'46typeExpDoc'4689_case_79(x2)(Curry.Module.Prelude.c_null(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_61_61(Curry.Module.FlatCurry.C_Public)))(Curry.Module.FlatCurryGoodies.c_consVisibility(st))(st))(Curry.Module.FlatCurryGoodies.c_trType(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))))(Curry.Module.PrettyFlat.c_exportedNames'46typeExpDoc'4689'46_'35lambda4))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))))(Curry.Module.PrettyFlat.c_exportedNames'46typeExpDoc'4689'46_'35lambda5))(x2)(st))(st))(st))(st))(st)



c_exportedNames'46typeExpDoc'4689'46_'35lambda4 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.FlatCurry.C_Visibility -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_ConsDecl) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.FlatCurry.C_ConsDecl
c_exportedNames'46typeExpDoc'4689'46_'35lambda4 x1 x2 x3 x4 st = x4



c_exportedNames'46typeExpDoc'4689'46_'35lambda5 :: (Curry t1) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.FlatCurry.C_Visibility -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.Module.FlatCurry.C_TypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t1
c_exportedNames'46typeExpDoc'4689'46_'35lambda5 x1 x2 x3 x4 st = Curry.Module.Prelude.List



c_moduleHeaderDoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_moduleHeaderDoc x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_keyword(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_consname(st))(x1)(st))(st))(st))(Curry.Module.PrettyFlat.c_exportsDoc(x2)(st))(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_keyword(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))(st))(st)



c_exportsDoc :: (Curry.Module.Prelude.List Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_exportsDoc x1 st = Curry.Module.Pretty.c_group(Curry.Module.Pretty.c_nest(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_36_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_36_62(st))(Curry.Module.Pretty.c_lparen(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_align(st))(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_fillSep(st))(Curry.Module.Pretty.c_punctuate(Curry.Module.Pretty.c_comma(st))(x1)(st))(st))(st))(st))(st))(Curry.Module.Pretty.c_rparen(st))(st))(st))(st)



c_impsDoc :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_impsDoc x1 st = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_vcat(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.op_46(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_keyword(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))))(st))(st))(Curry.Module.PrettyFlat.c_consname(st))(st))(x1)(st))(st)



c_opsDoc :: (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_OpDecl) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_opsDoc x1 st = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_vcat(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_opDoc))(x1)(st))(st)



c_opDoc :: Curry.Module.FlatCurry.C_OpDecl -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_opDoc x1@(Curry.Module.FlatCurry.C_Op x2 x3 x4) st = Curry.Module.PrettyFlat.c_opDoc_case_78(x3)(x4)(x2)(st)
c_opDoc (Curry.Module.FlatCurry.C_OpDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_opDoc(x)(st))(i)(xs)(st)
c_opDoc x st = Curry.RunTimeSystem.patternFail("PrettyFlat.opDoc")(x)



c_opDoc'46fixDoc'46114 :: Curry.Module.FlatCurry.C_Fixity -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_opDoc'46fixDoc'46114 x1@Curry.Module.FlatCurry.C_InfixOp st = Curry.Module.Pretty.c_empty(st)
c_opDoc'46fixDoc'46114 x1@Curry.Module.FlatCurry.C_InfixlOp st = Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_keyword(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List))(st)
c_opDoc'46fixDoc'46114 x1@Curry.Module.FlatCurry.C_InfixrOp st = Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_keyword(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))(st)
c_opDoc'46fixDoc'46114 (Curry.Module.FlatCurry.C_FixityOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_opDoc'46fixDoc'46114(x)(st))(i)(xs)(st)
c_opDoc'46fixDoc'46114 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.opDoc.fixDoc.114")(x)



c_typesDoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_TypeDecl) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_typesDoc x1 st = Curry.Module.Prelude.op_46(Curry.Module.Pretty.c_vcat(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_typeDoc(x1)))))(st)



c_typeDoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_TypeDecl -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_typeDoc x1 x2@(Curry.Module.FlatCurry.C_Type x3 x4 x5 x6) st = Curry.Module.PrettyFlat.c_def(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_keyword(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))(Curry.Module.Prelude.List)))))(st))(st))(Curry.Module.PrettyFlat.c_qname(x1)(x3)(st))(st))(x5)(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_consDeclsDoc(x1)(st))(x6)(st))(st)
c_typeDoc x1 x2@(Curry.Module.FlatCurry.C_TypeSyn x7 x8 x9 x10) st = Curry.Module.PrettyFlat.c_def(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_keyword(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))(st))(st))(Curry.Module.PrettyFlat.c_qname(x1)(x7)(st))(st))(x9)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_operator(st))(Curry.Module.Pretty.c_equals(st))(st))(st))(Curry.Module.PrettyFlat.c_typeExprDoc(x1)(Curry.Module.Prelude.C_False)(x10)(st))(st))(st)
c_typeDoc x1 (Curry.Module.FlatCurry.C_TypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_typeDoc(x1)(x)(st))(i)(xs)(st)
c_typeDoc x1 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.typeDoc")(x)



c_varDoc :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_varDoc st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_text))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_show))(st))(st)



c_tvarDoc :: Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_tvarDoc x1 st = Curry.Module.PrettyFlat.c_tvarDoc_case_76(x1)(Curry.Module.Prelude.op_62(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))(st))(st)



c_consDeclsDoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.FlatCurry.C_ConsDecl) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_consDeclsDoc x1 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_fillEncloseSep(Curry.Module.Pretty.op_60_62(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_operator(st))(Curry.Module.Pretty.c_equals(st))(st))(Curry.Module.Pretty.c_space(st))(st))(Curry.Module.Pretty.c_empty(st))(Curry.Module.Pretty.op_60_62(Curry.Module.PrettyFlat.c_bar(st))(Curry.Module.Pretty.c_space(st))(st))))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.op_60_62))(Curry.Module.Pretty.c_space(st))))(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_consDeclDoc(x1)))(st))))(st)



c_consDeclDoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_ConsDecl -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_consDeclDoc x1 x2@(Curry.Module.FlatCurry.C_Cons x3 x4 x5 x6) st = Curry.Module.PrettyFlat.c_app(Curry.Module.PrettyFlat.c_qname(x1)(x3)(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_typeExprDoc(x1)(Curry.Module.Prelude.C_True)))(x6)(st))(st)
c_consDeclDoc x1 (Curry.Module.FlatCurry.C_ConsDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_consDeclDoc(x1)(x)(st))(i)(xs)(st)
c_consDeclDoc x1 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.consDeclDoc")(x)



c_typeExprDoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.Module.FlatCurry.C_TypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_typeExprDoc x1 x2 x3@(Curry.Module.FlatCurry.C_TVar x4) st = Curry.Module.PrettyFlat.c_tvarDoc(x4)(st)
c_typeExprDoc x1 x2 x3@(Curry.Module.FlatCurry.C_TCons x5 x6) st = Curry.Module.PrettyFlat.c_typeExprDoc_case_74(x1)(x2)(x5)(x6)(Curry.Module.Prelude.c_null(x6)(st))(st)
c_typeExprDoc x1 x2 x3@(Curry.Module.FlatCurry.C_FuncType x7 x8) st = Curry.Module.Prelude.op_36(Curry.Module.PrettyFlat.c_par(x2)(st))(Curry.Module.Pretty.c_fillEncloseSep(Curry.Module.Pretty.c_empty(st))(Curry.Module.Pretty.c_empty(st))(Curry.Module.Pretty.op_60_62(Curry.Module.Pretty.op_60_62(Curry.Module.Pretty.c_space(st))(Curry.Module.PrettyFlat.c_arrow(st))(st))(Curry.Module.Pretty.c_space(st))(st))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_typeExprDoc(x1)(Curry.Module.Prelude.C_True)))(Curry.Module.FlatCurryGoodies.c_argTypes(x3)(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.PrettyFlat.c_typeExprDoc(x1)(Curry.Module.Prelude.C_False)(Curry.Module.FlatCurryGoodies.c_resultType(x3)(st))(st))(Curry.Module.Prelude.List))(st))(st))(st)
c_typeExprDoc x1 x2 (Curry.Module.FlatCurry.C_TypeExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_typeExprDoc(x1)(x2)(x)(st))(i)(xs)(st)
c_typeExprDoc x1 x2 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.typeExprDoc")(x)



c_par :: Curry.Module.Prelude.C_Bool -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Pretty.C_Doc -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_par x1@Curry.Module.Prelude.C_True st = Curry.Module.Pretty.c_parens(st)
c_par x1@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id)
c_par (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_par(x)(st))(i)(xs)(st)
c_par x st = Curry.RunTimeSystem.patternFail("PrettyFlat.par")(x)



c_funcsDoc :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_FuncDecl) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_funcsDoc x1 x2 x3 st = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_vcat(st))(Curry.Module.Pretty.c_punctuate(Curry.Module.Pretty.c_line(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_funcDoc(x1)(x2)))(x3)(st))(st))(st)



c_funcDoc :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_FuncDecl -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_funcDoc x1 x2 x3@(Curry.Module.FlatCurry.C_Func x4 x5 x6 x7 x8) st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_36_62(st))(Curry.Module.PrettyFlat.c_funcTypeDeclDoc(x2)(x4)(x7)(st))(st))(Curry.Module.PrettyFlat.c_ruleDoc(x1)(x2)(x4)(x8)(st))(st)
c_funcDoc x1 x2 (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_funcDoc(x1)(x2)(x)(st))(i)(xs)(st)
c_funcDoc x1 x2 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.funcDoc")(x)



c_funcTypeDeclDoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.FlatCurry.C_TypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_funcTypeDeclDoc x1 x2 x3 st = Curry.Module.PrettyFlat.c_def(Curry.Module.PrettyFlat.c_qname(x1)(x2)(st))(Curry.Module.Prelude.List)(Curry.Module.PrettyFlat.c_funcTypeDoc(x1)(Curry.Module.FlatCurryGoodies.c_argTypes(x3)(st))(Curry.Module.FlatCurryGoodies.c_resultType(x3)(st))(st))(st)



c_funcTypeDoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_TypeExpr) -> Curry.Module.FlatCurry.C_TypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_funcTypeDoc x1 x2 x3 st = Curry.Module.Pretty.c_fillEncloseSep(Curry.Module.Pretty.op_60_62(Curry.Module.PrettyFlat.c_dcolon(st))(Curry.Module.Pretty.c_space(st))(st))(Curry.Module.Pretty.c_empty(st))(Curry.Module.Pretty.op_60_62(Curry.Module.PrettyFlat.c_arrow(st))(Curry.Module.Pretty.c_space(st))(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Pretty.op_60_62))(Curry.Module.Pretty.c_space(st))))(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_typeExprDoc(x1)(Curry.Module.Prelude.C_True)))(st))(Curry.Module.Prelude.op_43_43(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(st))(st))(st)



c_ruleDoc :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.FlatCurry.C_Rule -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_ruleDoc x1 x2 x3 x4@(Curry.Module.FlatCurry.C_Rule x5 x6) st = Curry.Module.PrettyFlat.c_def(Curry.Module.PrettyFlat.c_qname(x2)(x3)(st))(x5)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.Pretty.c_equals(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_align(st))(Curry.Module.PrettyFlat.c_expDoc(x1)(Curry.Module.Prelude.C_Zero)(x2)(Curry.Module.Prelude.C_False)(x6)(st))(st))(st))(st)
c_ruleDoc x1 x2 x3 x4@(Curry.Module.FlatCurry.C_External x7) st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.PrettyFlat.c_qname(x2)(x3)(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_keyword(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))))))(st))(st)
c_ruleDoc x1 x2 x3 (Curry.Module.FlatCurry.C_RuleOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_ruleDoc(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_ruleDoc x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.ruleDoc")(x)



c_expDoc :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_expDoc x1 x2 x3 x4 x5 st = Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.c_maybe(Curry.Module.PrettyFlat.c_expDoc2(x1)(Curry.Module.Prelude.C_Zero)(x3)(x4)(x5)(st))(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_expDoc'46_'35lambda6(x3)(x1)))(Curry.Module.PrettyFlat.c_toList(x5)(st))(st))(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_expDoc'46_'35lambda7))(Curry.Module.PrettyFlat.c_toString(x5)(st))(st)



c_expDoc'46_'35lambda6 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_expDoc'46_'35lambda6 x1 x2 x3 st = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_list(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_expDoc(x2)(Curry.Module.Prelude.C_Zero)(x1)(Curry.Module.Prelude.C_False)))(x3)(st))(st)



c_expDoc'46_'35lambda7 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_expDoc'46_'35lambda7 x1 st = Curry.Module.PrettyFlat.c_expDoc'46_'35lambda7_case_70(x1)(Curry.Module.Prelude.c_null(x1)(st))(st)



c_expDoc2 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Prelude.C_Bool -> Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_expDoc2 x1 x2 x3 x4 x5@(Curry.Module.FlatCurry.C_Var x6) st = Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_varDoc(st))(x6)(st)
c_expDoc2 x1 x2 x3 x4 x5@(Curry.Module.FlatCurry.C_Lit x7) st = Curry.Module.PrettyFlat.c_litDoc(x7)(st)
c_expDoc2 x1 x2 x3 x4 x5@(Curry.Module.FlatCurry.C_Comb x8 x9 x10) st = let {x11 = Curry.Module.PrettyFlat.c_expDoc2_case_64(x4)(st)} in let {x14 = Curry.Module.PrettyFlat.c_expDoc2_case_65(x1)(x9)(Curry.Module.Prelude.c_lookup(x9)(x1)(st))(st)} in Curry.Module.PrettyFlat.c_expDoc2_case_69(x1)(x2)(x3)(x4)(x8)(x9)(x10)(x11)(x14)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x8)(Curry.Module.FlatCurry.C_FuncCall)(st))(Curry.Module.Prelude.op_61_61(x9)(Curry.Module.Prelude.T2(Curry.Module.PrettyFlat.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st))(st))(st)
c_expDoc2 x1 x2 x3 x4 x5@(Curry.Module.FlatCurry.C_Let x16 x17) st = Curry.Module.Prelude.op_36(Curry.Module.PrettyFlat.c_par(x4)(st))(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_hang(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_36_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_keyword(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_letBindsDoc(x1)(x3)(st))(x16)(st))(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_keyword(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List)))(st))(st))(st))(Curry.Module.PrettyFlat.c_expDoc(x1)(Curry.Module.Prelude.C_Zero)(x3)(Curry.Module.Prelude.C_False)(x17)(st))(st))(st))(st)
c_expDoc2 x1 x2 x3 x4 x5@(Curry.Module.FlatCurry.C_Free x18 x19) st = Curry.Module.Prelude.op_36(Curry.Module.PrettyFlat.c_par(x4)(st))(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_hang(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_36_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_keyword(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_align(st))(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_fillSep(st))(Curry.Module.Pretty.c_punctuate(Curry.Module.Pretty.c_comma(st))(Curry.Module.Prelude.c_map(Curry.Module.PrettyFlat.c_varDoc(st))(x18)(st))(st))(st))(st))(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_keyword(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))(st))(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_keyword(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List)))(st))(st))(st))(Curry.Module.PrettyFlat.c_expDoc(x1)(Curry.Module.Prelude.C_Zero)(x3)(Curry.Module.Prelude.C_False)(x19)(st))(st))(st))(st)
c_expDoc2 x1 x2 x3 x4 x5@(Curry.Module.FlatCurry.C_Or x20 x21) st = Curry.Module.PrettyFlat.c_expDoc(x1)(Curry.Module.Prelude.C_Zero)(x3)(x4)(Curry.Module.FlatCurry.C_Comb(Curry.Module.FlatCurry.C_FuncCall)(Curry.Module.Prelude.T2(Curry.Module.PrettyFlat.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('?'))(Curry.Module.Prelude.List)))((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))))(st)
c_expDoc2 x1 x2 x3 x4 x5@(Curry.Module.FlatCurry.C_Case x22 x23 x24) st = Curry.Module.Prelude.op_36(Curry.Module.PrettyFlat.c_par(x4)(st))(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Pretty.c_hang(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_36_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.Pretty.op_60_62(Curry.Module.PrettyFlat.c_caseTypeDoc(x22)(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_keyword(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))(st))(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_align(st))(Curry.Module.PrettyFlat.c_expDoc(x1)(Curry.Module.Prelude.C_Zero)(x3)(Curry.Module.Prelude.C_False)(x23)(st))(st))(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_keyword(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))(Curry.Module.Prelude.List)))(st))(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_layout(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_branchDoc(x1)(x3)))(x24)(st))(st))(st))(st))(st)
c_expDoc2 x1 x2 x3 x4 (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_expDoc2(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_expDoc2 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.expDoc2")(x)



c_expDoc2'46_'35selFP3'35lbr :: (Curry.Module.Prelude.T2 Curry.Module.Pretty.C_Doc Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_expDoc2'46_'35selFP3'35lbr x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_expDoc2'46_'35selFP3'35lbr (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_expDoc2'46_'35selFP3'35lbr(x)(st))(i)(xs)(st)
c_expDoc2'46_'35selFP3'35lbr x st = Curry.RunTimeSystem.patternFail("PrettyFlat.expDoc2._#selFP3#lbr")(x)



c_expDoc2'46_'35selFP4'35rbr :: (Curry.Module.Prelude.T2 Curry.Module.Pretty.C_Doc Curry.Module.Pretty.C_Doc) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_expDoc2'46_'35selFP4'35rbr x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_expDoc2'46_'35selFP4'35rbr (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_expDoc2'46_'35selFP4'35rbr(x)(st))(i)(xs)(st)
c_expDoc2'46_'35selFP4'35rbr x st = Curry.RunTimeSystem.patternFail("PrettyFlat.expDoc2._#selFP4#rbr")(x)



c_branchDoc :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_BranchExpr -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_branchDoc x1 x2 x3@(Curry.Module.FlatCurry.C_Branch x4 x5) st = Curry.Module.PrettyFlat.c_def(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.PrettyFlat.c_patternDoc(x2)(x4)(st))(st))(Curry.Module.PrettyFlat.c_arrow(st))(st))(Curry.Module.Prelude.List)(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_align(st))(Curry.Module.PrettyFlat.c_expDoc(x1)(Curry.Module.Prelude.C_Zero)(x2)(Curry.Module.Prelude.C_False)(x5)(st))(st))(st)
c_branchDoc x1 x2 (Curry.Module.FlatCurry.C_BranchExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_branchDoc(x1)(x2)(x)(st))(i)(xs)(st)
c_branchDoc x1 x2 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.branchDoc")(x)



c_caseTypeDoc :: Curry.Module.FlatCurry.C_CaseType -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_caseTypeDoc x1@Curry.Module.FlatCurry.C_Rigid st = Curry.Module.Pretty.c_empty(st)
c_caseTypeDoc x1@Curry.Module.FlatCurry.C_Flex st = Curry.Module.Pretty.c_empty(st)
c_caseTypeDoc (Curry.Module.FlatCurry.C_CaseTypeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_caseTypeDoc(x)(st))(i)(xs)(st)
c_caseTypeDoc x st = Curry.RunTimeSystem.patternFail("PrettyFlat.caseTypeDoc")(x)



c_patternDoc :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.FlatCurry.C_Pattern -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_patternDoc x1 x2@(Curry.Module.FlatCurry.C_Pattern x3 x4) st = Curry.Module.PrettyFlat.c_patternDoc_case_63(x1)(x3)(x4)(Curry.Module.Prelude.c_null(x4)(st))(st)
c_patternDoc x1 x2@(Curry.Module.FlatCurry.C_LPattern x5) st = Curry.Module.PrettyFlat.c_litDoc(x5)(st)
c_patternDoc x1 (Curry.Module.FlatCurry.C_PatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_patternDoc(x1)(x)(st))(i)(xs)(st)
c_patternDoc x1 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.patternDoc")(x)



c_letBindsDoc :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Expr)) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc)
c_letBindsDoc x1 x2 st = Curry.Module.Prelude.op_46(Curry.Module.PrettyFlat.c_layout(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_letBindDoc(x1)(x2)))))(st)



c_letBindDoc :: (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) Curry.Module.Prelude.C_Int)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Expr) -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_letBindDoc x1 x2 x3@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_varDoc(st))(x4)(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_operator(st))(Curry.Module.Pretty.c_equals(st))(st))(st))(st))(Curry.Module.PrettyFlat.c_expDoc(x1)(Curry.Module.Prelude.C_Zero)(x2)(Curry.Module.Prelude.C_False)(x5)(st))(st)
c_letBindDoc x1 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_letBindDoc(x1)(x2)(x)(st))(i)(xs)(st)
c_letBindDoc x1 x2 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.letBindDoc")(x)



c_litDoc :: Curry.Module.FlatCurry.C_Literal -> Curry.RunTimeSystem.State -> Curry.Module.Pretty.C_Doc
c_litDoc x1@(Curry.Module.FlatCurry.C_Intc x2) st = Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_literal(st))(Curry.Module.Pretty.c_int(x2)(st))(st)
c_litDoc x1@(Curry.Module.FlatCurry.C_Floatc x3) st = Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_literal(st))(Curry.Module.Pretty.c_float(x3)(st))(st)
c_litDoc x1@(Curry.Module.FlatCurry.C_Charc x4) st = Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_literal(st))(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_squotes(st))(Curry.Module.Pretty.c_text(Curry.Module.PrettyFlat.c_quoteChar(x4)(st))(st))(st))(st)
c_litDoc (Curry.Module.FlatCurry.C_LiteralOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_litDoc(x)(st))(i)(xs)(st)
c_litDoc x st = Curry.RunTimeSystem.patternFail("PrettyFlat.litDoc")(x)



c_quoteChar :: Curry.Module.Prelude.C_Char -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_quoteChar x1 st = Curry.Module.Prelude.c_maybe((Curry.Module.Prelude.:<)(x1)(Curry.Module.Prelude.List))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id))(Curry.Module.Prelude.c_lookup(x1)(Curry.Module.PrettyFlat.c_specialChars(st))(st))(st)



c_specialChars :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Char (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_specialChars st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Char('\r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Char('\t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.List))))



c_toString :: Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_toString x1@(Curry.Module.FlatCurry.C_Comb x2 x3 x4) st = Curry.Module.PrettyFlat.c_toString_case_59(x3)(x4)(x2)(st)
c_toString x1@(Curry.Module.FlatCurry.C_Var x58) st = Curry.Module.Prelude.C_Nothing
c_toString x1@(Curry.Module.FlatCurry.C_Lit x59) st = Curry.Module.Prelude.C_Nothing
c_toString x1@(Curry.Module.FlatCurry.C_Let x60 x61) st = Curry.Module.Prelude.C_Nothing
c_toString x1@(Curry.Module.FlatCurry.C_Free x62 x63) st = Curry.Module.Prelude.C_Nothing
c_toString x1@(Curry.Module.FlatCurry.C_Or x64 x65) st = Curry.Module.Prelude.C_Nothing
c_toString x1@(Curry.Module.FlatCurry.C_Case x66 x67 x68) st = Curry.Module.Prelude.C_Nothing
c_toString (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString(x)(st))(i)(xs)(st)
c_toString x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString")(x)



c_toList :: Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr)
c_toList x1@(Curry.Module.FlatCurry.C_Comb x2 x3 x4) st = Curry.Module.PrettyFlat.c_toList_case_29(x3)(x4)(x2)(st)
c_toList x1@(Curry.Module.FlatCurry.C_Var x41) st = Curry.Module.Prelude.C_Nothing
c_toList x1@(Curry.Module.FlatCurry.C_Lit x42) st = Curry.Module.Prelude.C_Nothing
c_toList x1@(Curry.Module.FlatCurry.C_Let x43 x44) st = Curry.Module.Prelude.C_Nothing
c_toList x1@(Curry.Module.FlatCurry.C_Free x45 x46) st = Curry.Module.Prelude.C_Nothing
c_toList x1@(Curry.Module.FlatCurry.C_Or x47 x48) st = Curry.Module.Prelude.C_Nothing
c_toList x1@(Curry.Module.FlatCurry.C_Case x49 x50 x51) st = Curry.Module.Prelude.C_Nothing
c_toList (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList(x)(st))(i)(xs)(st)
c_toList x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList")(x)



c_elimApp :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr)
c_elimApp st = Curry.Module.FlatCurryGoodies.c_updCombs(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.PrettyFlat.c_elimApp'46elim'46276))(st)



c_elimApp'46extend'46276 :: Curry.Module.FlatCurry.C_Expr -> Curry.Module.FlatCurry.C_Expr -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_elimApp'46extend'46276 x1@(Curry.Module.FlatCurry.C_Comb x3 x4 x5) x2 st = Curry.Module.FlatCurry.C_Comb(x3)(x4)(Curry.Module.Prelude.op_43_43(x5)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(st))
c_elimApp'46extend'46276 (Curry.Module.FlatCurry.C_ExprOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_elimApp'46extend'46276(x)(x2)(st))(i)(xs)(st)
c_elimApp'46extend'46276 x x2 st = Curry.RunTimeSystem.patternFail("PrettyFlat.elimApp.extend.276")(x)



c_elimApp'46elim'46276 :: Curry.Module.FlatCurry.C_CombType -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.FlatCurry.C_Expr) -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_elimApp'46elim'46276 x1 x2 x3 st = Curry.Module.PrettyFlat.c_elimApp'46elim'46276_case_1(x1)(x2)(x3)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.FlatCurry.C_FuncCall)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.T2(Curry.Module.PrettyFlat.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st))(Curry.Module.Prelude.op_38_38(Curry.Module.FlatCurryGoodies.c_isComb(Curry.Module.Prelude.c_head(x3)(st))(st))(Curry.Module.Prelude.op_47_61(Curry.Module.FlatCurryGoodies.c_combName(Curry.Module.Prelude.c_head(x3)(st))(st))(Curry.Module.Prelude.T2(Curry.Module.PrettyFlat.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(st))(st))(st))(st))(st)



c_elimApp'46elim'46276_case_1 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_elimApp'46extend'46276(Curry.Module.Prelude.c_head(x3)(st))(Curry.Module.Prelude.op_33_33(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st)
c_elimApp'46elim'46276_case_1 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.PrettyFlat.c_elimApp'46elim'46276_case_0(x1)(x2)(x3)(Curry.Module.Prelude.c_otherwise(st))(st)
c_elimApp'46elim'46276_case_1 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_elimApp'46elim'46276_case_1(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_elimApp'46elim'46276_case_1 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.elimApp.elim.276_case_1")(x)



c_elimApp'46elim'46276_case_0 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.FlatCurry.C_Comb(x1)(x2)(x3)
c_elimApp'46elim'46276_case_0 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_elimApp'46elim'46276_case_0(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_elimApp'46elim'46276_case_0 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.elimApp.elim.276_case_0")(x)



c_toList_case_29 x3 x4 x2@Curry.Module.FlatCurry.C_ConsCall st = Curry.Module.PrettyFlat.c_toList_case_28(x4)(x3)(st)
c_toList_case_29 x3 x4 x2@Curry.Module.FlatCurry.C_FuncCall st = Curry.Module.Prelude.C_Nothing
c_toList_case_29 x3 x4 x2@(Curry.Module.FlatCurry.C_FuncPartCall x39) st = Curry.Module.Prelude.C_Nothing
c_toList_case_29 x3 x4 x2@(Curry.Module.FlatCurry.C_ConsPartCall x40) st = Curry.Module.Prelude.C_Nothing
c_toList_case_29 x3 x4 (Curry.Module.FlatCurry.C_CombTypeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_29(x3)(x4)(x)(st))(i)(xs)(st)
c_toList_case_29 x3 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_29")(x)



c_toList_case_28 x4 x3@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.PrettyFlat.c_toList_case_27(x4)(x6)(x5)(st)
c_toList_case_28 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_28(x4)(x)(st))(i)(xs)(st)
c_toList_case_28 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_28")(x)



c_toList_case_27 x4 x6 x5@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.PrettyFlat.c_toList_case_26(x4)(x6)(x7)(x8)(Curry.Module.Prelude.op_61_61(x7)(Curry.Module.Prelude.C_Char('P'))(st))(st)
c_toList_case_27 x4 x6 x5@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toList_case_27 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_27(x4)(x6)(x)(st))(i)(xs)(st)
c_toList_case_27 x4 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_27")(x)



c_toList_case_26 x4 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toList_case_25(x4)(x6)(x8)(st)
c_toList_case_26 x4 x6 x7 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Nothing
c_toList_case_26 x4 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_26(x4)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c_toList_case_26 x4 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_26")(x)



c_toList_case_25 x4 x6 x8@((Curry.Module.Prelude.:<) x9 x10) st = Curry.Module.PrettyFlat.c_toList_case_24(x4)(x6)(x9)(x10)(Curry.Module.Prelude.op_61_61(x9)(Curry.Module.Prelude.C_Char('r'))(st))(st)
c_toList_case_25 x4 x6 x8@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toList_case_25 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_25(x4)(x6)(x)(st))(i)(xs)(st)
c_toList_case_25 x4 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_25")(x)



c_toList_case_24 x4 x6 x9 x10 x11@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toList_case_23(x4)(x6)(x10)(st)
c_toList_case_24 x4 x6 x9 x10 x11@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Nothing
c_toList_case_24 x4 x6 x9 x10 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_24(x4)(x6)(x9)(x10)(x)(st))(i)(xs)(st)
c_toList_case_24 x4 x6 x9 x10 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_24")(x)



c_toList_case_23 x4 x6 x10@((Curry.Module.Prelude.:<) x11 x12) st = Curry.Module.PrettyFlat.c_toList_case_22(x4)(x6)(x11)(x12)(Curry.Module.Prelude.op_61_61(x11)(Curry.Module.Prelude.C_Char('e'))(st))(st)
c_toList_case_23 x4 x6 x10@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toList_case_23 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_23(x4)(x6)(x)(st))(i)(xs)(st)
c_toList_case_23 x4 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_23")(x)



c_toList_case_22 x4 x6 x11 x12 x13@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toList_case_21(x4)(x6)(x12)(st)
c_toList_case_22 x4 x6 x11 x12 x13@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Nothing
c_toList_case_22 x4 x6 x11 x12 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_22(x4)(x6)(x11)(x12)(x)(st))(i)(xs)(st)
c_toList_case_22 x4 x6 x11 x12 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_22")(x)



c_toList_case_21 x4 x6 x12@((Curry.Module.Prelude.:<) x13 x14) st = Curry.Module.PrettyFlat.c_toList_case_20(x4)(x6)(x13)(x14)(Curry.Module.Prelude.op_61_61(x13)(Curry.Module.Prelude.C_Char('l'))(st))(st)
c_toList_case_21 x4 x6 x12@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toList_case_21 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_21(x4)(x6)(x)(st))(i)(xs)(st)
c_toList_case_21 x4 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_21")(x)



c_toList_case_20 x4 x6 x13 x14 x15@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toList_case_19(x4)(x6)(x14)(st)
c_toList_case_20 x4 x6 x13 x14 x15@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Nothing
c_toList_case_20 x4 x6 x13 x14 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_20(x4)(x6)(x13)(x14)(x)(st))(i)(xs)(st)
c_toList_case_20 x4 x6 x13 x14 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_20")(x)



c_toList_case_19 x4 x6 x14@((Curry.Module.Prelude.:<) x15 x16) st = Curry.Module.PrettyFlat.c_toList_case_18(x4)(x6)(x15)(x16)(Curry.Module.Prelude.op_61_61(x15)(Curry.Module.Prelude.C_Char('u'))(st))(st)
c_toList_case_19 x4 x6 x14@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toList_case_19 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_19(x4)(x6)(x)(st))(i)(xs)(st)
c_toList_case_19 x4 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_19")(x)



c_toList_case_18 x4 x6 x15 x16 x17@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toList_case_17(x4)(x6)(x16)(st)
c_toList_case_18 x4 x6 x15 x16 x17@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Nothing
c_toList_case_18 x4 x6 x15 x16 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_18(x4)(x6)(x15)(x16)(x)(st))(i)(xs)(st)
c_toList_case_18 x4 x6 x15 x16 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_18")(x)



c_toList_case_17 x4 x6 x16@((Curry.Module.Prelude.:<) x17 x18) st = Curry.Module.PrettyFlat.c_toList_case_16(x4)(x6)(x17)(x18)(Curry.Module.Prelude.op_61_61(x17)(Curry.Module.Prelude.C_Char('d'))(st))(st)
c_toList_case_17 x4 x6 x16@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toList_case_17 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_17(x4)(x6)(x)(st))(i)(xs)(st)
c_toList_case_17 x4 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_17")(x)



c_toList_case_16 x4 x6 x17 x18 x19@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toList_case_15(x4)(x6)(x18)(st)
c_toList_case_16 x4 x6 x17 x18 x19@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Nothing
c_toList_case_16 x4 x6 x17 x18 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_16(x4)(x6)(x17)(x18)(x)(st))(i)(xs)(st)
c_toList_case_16 x4 x6 x17 x18 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_16")(x)



c_toList_case_15 x4 x6 x18@((Curry.Module.Prelude.:<) x19 x20) st = Curry.Module.PrettyFlat.c_toList_case_14(x4)(x6)(x19)(x20)(Curry.Module.Prelude.op_61_61(x19)(Curry.Module.Prelude.C_Char('e'))(st))(st)
c_toList_case_15 x4 x6 x18@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toList_case_15 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_15(x4)(x6)(x)(st))(i)(xs)(st)
c_toList_case_15 x4 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_15")(x)



c_toList_case_14 x4 x6 x19 x20 x21@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toList_case_13(x4)(x6)(x20)(st)
c_toList_case_14 x4 x6 x19 x20 x21@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Nothing
c_toList_case_14 x4 x6 x19 x20 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_14(x4)(x6)(x19)(x20)(x)(st))(i)(xs)(st)
c_toList_case_14 x4 x6 x19 x20 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_14")(x)



c_toList_case_13 x4 x6 x20@Curry.Module.Prelude.List st = Curry.Module.PrettyFlat.c_toList_case_12(x4)(x6)(st)
c_toList_case_13 x4 x6 x20@((Curry.Module.Prelude.:<) x37 x38) st = Curry.Module.Prelude.C_Nothing
c_toList_case_13 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_13(x4)(x6)(x)(st))(i)(xs)(st)
c_toList_case_13 x4 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_13")(x)



c_toList_case_12 x4 x6@((Curry.Module.Prelude.:<) x21 x22) st = Curry.Module.PrettyFlat.c_toList_case_11(x4)(x21)(x22)(Curry.Module.Prelude.op_61_61(x21)(Curry.Module.Prelude.C_Char('['))(st))(st)
c_toList_case_12 x4 x6@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toList_case_12 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_12(x4)(x)(st))(i)(xs)(st)
c_toList_case_12 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_12")(x)



c_toList_case_11 x4 x21 x22 x23@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toList_case_10(x4)(x22)(st)
c_toList_case_11 x4 x21 x22 x23@Curry.Module.Prelude.C_False st = Curry.Module.PrettyFlat.c_toList_case_6(x4)(x21)(x22)(Curry.Module.Prelude.op_61_61(x21)(Curry.Module.Prelude.C_Char(':'))(st))(st)
c_toList_case_11 x4 x21 x22 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_11(x4)(x21)(x22)(x)(st))(i)(xs)(st)
c_toList_case_11 x4 x21 x22 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_11")(x)



c_toList_case_6 x4 x21 x22 x23@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toList_case_5(x4)(x22)(st)
c_toList_case_6 x4 x21 x22 x23@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Nothing
c_toList_case_6 x4 x21 x22 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_6(x4)(x21)(x22)(x)(st))(i)(xs)(st)
c_toList_case_6 x4 x21 x22 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_6")(x)



c_toList_case_5 x4 x22@Curry.Module.Prelude.List st = Curry.Module.PrettyFlat.c_toList_case_4(x4)(st)
c_toList_case_5 x4 x22@((Curry.Module.Prelude.:<) x35 x36) st = Curry.Module.Prelude.C_Nothing
c_toList_case_5 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_5(x4)(x)(st))(i)(xs)(st)
c_toList_case_5 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_5")(x)



c_toList_case_4 x4@((Curry.Module.Prelude.:<) x29 x30) st = Curry.Module.PrettyFlat.c_toList_case_3(x29)(x30)(st)
c_toList_case_4 x4@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toList_case_4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_4(x)(st))(i)(xs)(st)
c_toList_case_4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_4")(x)



c_toList_case_3 x29 x30@((Curry.Module.Prelude.:<) x31 x32) st = Curry.Module.PrettyFlat.c_toList_case_2(x29)(x31)(x32)(st)
c_toList_case_3 x29 x30@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toList_case_3 x29 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_3(x29)(x)(st))(i)(xs)(st)
c_toList_case_3 x29 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_3")(x)



c_toList_case_2 x29 x31 x32@Curry.Module.Prelude.List st = Curry.Module.Maybe.op_62_62_45(Curry.Module.PrettyFlat.c_toList(x31)(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc(Curry.Module.Prelude.C_Just))(Curry.Module.Prelude.pc((Curry.Module.Prelude.:<)(x29)))(st))(st)
c_toList_case_2 x29 x31 x32@((Curry.Module.Prelude.:<) x33 x34) st = Curry.Module.Prelude.C_Nothing
c_toList_case_2 x29 x31 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_2(x29)(x31)(x)(st))(i)(xs)(st)
c_toList_case_2 x29 x31 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_2")(x)



c_toList_case_10 x4 x22@((Curry.Module.Prelude.:<) x23 x24) st = Curry.Module.PrettyFlat.c_toList_case_9(x4)(x23)(x24)(Curry.Module.Prelude.op_61_61(x23)(Curry.Module.Prelude.C_Char(']'))(st))(st)
c_toList_case_10 x4 x22@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toList_case_10 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_10(x4)(x)(st))(i)(xs)(st)
c_toList_case_10 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_10")(x)



c_toList_case_9 x4 x23 x24 x25@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toList_case_8(x4)(x24)(st)
c_toList_case_9 x4 x23 x24 x25@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Nothing
c_toList_case_9 x4 x23 x24 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_9(x4)(x23)(x24)(x)(st))(i)(xs)(st)
c_toList_case_9 x4 x23 x24 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_9")(x)



c_toList_case_8 x4 x24@Curry.Module.Prelude.List st = Curry.Module.PrettyFlat.c_toList_case_7(x4)(st)
c_toList_case_8 x4 x24@((Curry.Module.Prelude.:<) x27 x28) st = Curry.Module.Prelude.C_Nothing
c_toList_case_8 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_8(x4)(x)(st))(i)(xs)(st)
c_toList_case_8 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_8")(x)



c_toList_case_7 x4@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Just(Curry.Module.Prelude.List)
c_toList_case_7 x4@((Curry.Module.Prelude.:<) x25 x26) st = Curry.Module.Prelude.C_Nothing
c_toList_case_7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toList_case_7(x)(st))(i)(xs)(st)
c_toList_case_7 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toList_case_7")(x)



c_toString_case_59 x3 x4 x2@Curry.Module.FlatCurry.C_ConsCall st = Curry.Module.PrettyFlat.c_toString_case_58(x4)(x3)(st)
c_toString_case_59 x3 x4 x2@Curry.Module.FlatCurry.C_FuncCall st = Curry.Module.Prelude.C_Nothing
c_toString_case_59 x3 x4 x2@(Curry.Module.FlatCurry.C_FuncPartCall x56) st = Curry.Module.Prelude.C_Nothing
c_toString_case_59 x3 x4 x2@(Curry.Module.FlatCurry.C_ConsPartCall x57) st = Curry.Module.Prelude.C_Nothing
c_toString_case_59 x3 x4 (Curry.Module.FlatCurry.C_CombTypeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_59(x3)(x4)(x)(st))(i)(xs)(st)
c_toString_case_59 x3 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_59")(x)



c_toString_case_58 x4 x3@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.PrettyFlat.c_toString_case_57(x4)(x6)(x5)(st)
c_toString_case_58 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_58(x4)(x)(st))(i)(xs)(st)
c_toString_case_58 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_58")(x)



c_toString_case_57 x4 x6 x5@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.PrettyFlat.c_toString_case_56(x4)(x6)(x7)(x8)(Curry.Module.Prelude.op_61_61(x7)(Curry.Module.Prelude.C_Char('P'))(st))(st)
c_toString_case_57 x4 x6 x5@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toString_case_57 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_57(x4)(x6)(x)(st))(i)(xs)(st)
c_toString_case_57 x4 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_57")(x)



c_toString_case_56 x4 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toString_case_55(x4)(x6)(x8)(st)
c_toString_case_56 x4 x6 x7 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Nothing
c_toString_case_56 x4 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_56(x4)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c_toString_case_56 x4 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_56")(x)



c_toString_case_55 x4 x6 x8@((Curry.Module.Prelude.:<) x9 x10) st = Curry.Module.PrettyFlat.c_toString_case_54(x4)(x6)(x9)(x10)(Curry.Module.Prelude.op_61_61(x9)(Curry.Module.Prelude.C_Char('r'))(st))(st)
c_toString_case_55 x4 x6 x8@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toString_case_55 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_55(x4)(x6)(x)(st))(i)(xs)(st)
c_toString_case_55 x4 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_55")(x)



c_toString_case_54 x4 x6 x9 x10 x11@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toString_case_53(x4)(x6)(x10)(st)
c_toString_case_54 x4 x6 x9 x10 x11@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Nothing
c_toString_case_54 x4 x6 x9 x10 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_54(x4)(x6)(x9)(x10)(x)(st))(i)(xs)(st)
c_toString_case_54 x4 x6 x9 x10 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_54")(x)



c_toString_case_53 x4 x6 x10@((Curry.Module.Prelude.:<) x11 x12) st = Curry.Module.PrettyFlat.c_toString_case_52(x4)(x6)(x11)(x12)(Curry.Module.Prelude.op_61_61(x11)(Curry.Module.Prelude.C_Char('e'))(st))(st)
c_toString_case_53 x4 x6 x10@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toString_case_53 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_53(x4)(x6)(x)(st))(i)(xs)(st)
c_toString_case_53 x4 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_53")(x)



c_toString_case_52 x4 x6 x11 x12 x13@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toString_case_51(x4)(x6)(x12)(st)
c_toString_case_52 x4 x6 x11 x12 x13@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Nothing
c_toString_case_52 x4 x6 x11 x12 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_52(x4)(x6)(x11)(x12)(x)(st))(i)(xs)(st)
c_toString_case_52 x4 x6 x11 x12 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_52")(x)



c_toString_case_51 x4 x6 x12@((Curry.Module.Prelude.:<) x13 x14) st = Curry.Module.PrettyFlat.c_toString_case_50(x4)(x6)(x13)(x14)(Curry.Module.Prelude.op_61_61(x13)(Curry.Module.Prelude.C_Char('l'))(st))(st)
c_toString_case_51 x4 x6 x12@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toString_case_51 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_51(x4)(x6)(x)(st))(i)(xs)(st)
c_toString_case_51 x4 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_51")(x)



c_toString_case_50 x4 x6 x13 x14 x15@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toString_case_49(x4)(x6)(x14)(st)
c_toString_case_50 x4 x6 x13 x14 x15@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Nothing
c_toString_case_50 x4 x6 x13 x14 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_50(x4)(x6)(x13)(x14)(x)(st))(i)(xs)(st)
c_toString_case_50 x4 x6 x13 x14 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_50")(x)



c_toString_case_49 x4 x6 x14@((Curry.Module.Prelude.:<) x15 x16) st = Curry.Module.PrettyFlat.c_toString_case_48(x4)(x6)(x15)(x16)(Curry.Module.Prelude.op_61_61(x15)(Curry.Module.Prelude.C_Char('u'))(st))(st)
c_toString_case_49 x4 x6 x14@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toString_case_49 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_49(x4)(x6)(x)(st))(i)(xs)(st)
c_toString_case_49 x4 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_49")(x)



c_toString_case_48 x4 x6 x15 x16 x17@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toString_case_47(x4)(x6)(x16)(st)
c_toString_case_48 x4 x6 x15 x16 x17@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Nothing
c_toString_case_48 x4 x6 x15 x16 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_48(x4)(x6)(x15)(x16)(x)(st))(i)(xs)(st)
c_toString_case_48 x4 x6 x15 x16 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_48")(x)



c_toString_case_47 x4 x6 x16@((Curry.Module.Prelude.:<) x17 x18) st = Curry.Module.PrettyFlat.c_toString_case_46(x4)(x6)(x17)(x18)(Curry.Module.Prelude.op_61_61(x17)(Curry.Module.Prelude.C_Char('d'))(st))(st)
c_toString_case_47 x4 x6 x16@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toString_case_47 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_47(x4)(x6)(x)(st))(i)(xs)(st)
c_toString_case_47 x4 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_47")(x)



c_toString_case_46 x4 x6 x17 x18 x19@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toString_case_45(x4)(x6)(x18)(st)
c_toString_case_46 x4 x6 x17 x18 x19@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Nothing
c_toString_case_46 x4 x6 x17 x18 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_46(x4)(x6)(x17)(x18)(x)(st))(i)(xs)(st)
c_toString_case_46 x4 x6 x17 x18 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_46")(x)



c_toString_case_45 x4 x6 x18@((Curry.Module.Prelude.:<) x19 x20) st = Curry.Module.PrettyFlat.c_toString_case_44(x4)(x6)(x19)(x20)(Curry.Module.Prelude.op_61_61(x19)(Curry.Module.Prelude.C_Char('e'))(st))(st)
c_toString_case_45 x4 x6 x18@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toString_case_45 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_45(x4)(x6)(x)(st))(i)(xs)(st)
c_toString_case_45 x4 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_45")(x)



c_toString_case_44 x4 x6 x19 x20 x21@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toString_case_43(x4)(x6)(x20)(st)
c_toString_case_44 x4 x6 x19 x20 x21@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Nothing
c_toString_case_44 x4 x6 x19 x20 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_44(x4)(x6)(x19)(x20)(x)(st))(i)(xs)(st)
c_toString_case_44 x4 x6 x19 x20 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_44")(x)



c_toString_case_43 x4 x6 x20@Curry.Module.Prelude.List st = Curry.Module.PrettyFlat.c_toString_case_42(x4)(x6)(st)
c_toString_case_43 x4 x6 x20@((Curry.Module.Prelude.:<) x54 x55) st = Curry.Module.Prelude.C_Nothing
c_toString_case_43 x4 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_43(x4)(x6)(x)(st))(i)(xs)(st)
c_toString_case_43 x4 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_43")(x)



c_toString_case_42 x4 x6@((Curry.Module.Prelude.:<) x21 x22) st = Curry.Module.PrettyFlat.c_toString_case_41(x4)(x21)(x22)(Curry.Module.Prelude.op_61_61(x21)(Curry.Module.Prelude.C_Char('['))(st))(st)
c_toString_case_42 x4 x6@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toString_case_42 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_42(x4)(x)(st))(i)(xs)(st)
c_toString_case_42 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_42")(x)



c_toString_case_41 x4 x21 x22 x23@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toString_case_40(x4)(x22)(st)
c_toString_case_41 x4 x21 x22 x23@Curry.Module.Prelude.C_False st = Curry.Module.PrettyFlat.c_toString_case_36(x4)(x21)(x22)(Curry.Module.Prelude.op_61_61(x21)(Curry.Module.Prelude.C_Char(':'))(st))(st)
c_toString_case_41 x4 x21 x22 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_41(x4)(x21)(x22)(x)(st))(i)(xs)(st)
c_toString_case_41 x4 x21 x22 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_41")(x)



c_toString_case_36 x4 x21 x22 x23@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toString_case_35(x4)(x22)(st)
c_toString_case_36 x4 x21 x22 x23@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Nothing
c_toString_case_36 x4 x21 x22 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_36(x4)(x21)(x22)(x)(st))(i)(xs)(st)
c_toString_case_36 x4 x21 x22 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_36")(x)



c_toString_case_35 x4 x22@Curry.Module.Prelude.List st = Curry.Module.PrettyFlat.c_toString_case_34(x4)(st)
c_toString_case_35 x4 x22@((Curry.Module.Prelude.:<) x52 x53) st = Curry.Module.Prelude.C_Nothing
c_toString_case_35 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_35(x4)(x)(st))(i)(xs)(st)
c_toString_case_35 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_35")(x)



c_toString_case_34 x4@((Curry.Module.Prelude.:<) x29 x30) st = Curry.Module.PrettyFlat.c_toString_case_33(x30)(x29)(st)
c_toString_case_34 x4@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toString_case_34 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_34(x)(st))(i)(xs)(st)
c_toString_case_34 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_34")(x)



c_toString_case_33 x30 x29@(Curry.Module.FlatCurry.C_Lit x31) st = Curry.Module.PrettyFlat.c_toString_case_32(x30)(x31)(st)
c_toString_case_33 x30 x29@(Curry.Module.FlatCurry.C_Var x39) st = Curry.Module.Prelude.C_Nothing
c_toString_case_33 x30 x29@(Curry.Module.FlatCurry.C_Comb x40 x41 x42) st = Curry.Module.Prelude.C_Nothing
c_toString_case_33 x30 x29@(Curry.Module.FlatCurry.C_Let x43 x44) st = Curry.Module.Prelude.C_Nothing
c_toString_case_33 x30 x29@(Curry.Module.FlatCurry.C_Free x45 x46) st = Curry.Module.Prelude.C_Nothing
c_toString_case_33 x30 x29@(Curry.Module.FlatCurry.C_Or x47 x48) st = Curry.Module.Prelude.C_Nothing
c_toString_case_33 x30 x29@(Curry.Module.FlatCurry.C_Case x49 x50 x51) st = Curry.Module.Prelude.C_Nothing
c_toString_case_33 x30 (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_33(x30)(x)(st))(i)(xs)(st)
c_toString_case_33 x30 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_33")(x)



c_toString_case_32 x30 x31@(Curry.Module.FlatCurry.C_Charc x32) st = Curry.Module.PrettyFlat.c_toString_case_31(x32)(x30)(st)
c_toString_case_32 x30 x31@(Curry.Module.FlatCurry.C_Intc x37) st = Curry.Module.Prelude.C_Nothing
c_toString_case_32 x30 x31@(Curry.Module.FlatCurry.C_Floatc x38) st = Curry.Module.Prelude.C_Nothing
c_toString_case_32 x30 (Curry.Module.FlatCurry.C_LiteralOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_32(x30)(x)(st))(i)(xs)(st)
c_toString_case_32 x30 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_32")(x)



c_toString_case_31 x32 x30@((Curry.Module.Prelude.:<) x33 x34) st = Curry.Module.PrettyFlat.c_toString_case_30(x32)(x33)(x34)(st)
c_toString_case_31 x32 x30@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toString_case_31 x32 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_31(x32)(x)(st))(i)(xs)(st)
c_toString_case_31 x32 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_31")(x)



c_toString_case_30 x32 x33 x34@Curry.Module.Prelude.List st = Curry.Module.Maybe.op_62_62_45(Curry.Module.PrettyFlat.c_toString(x33)(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc(Curry.Module.Prelude.C_Just))(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_43_43(Curry.Module.PrettyFlat.c_quoteChar(x32)(st))))(st))(st)
c_toString_case_30 x32 x33 x34@((Curry.Module.Prelude.:<) x35 x36) st = Curry.Module.Prelude.C_Nothing
c_toString_case_30 x32 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_30(x32)(x33)(x)(st))(i)(xs)(st)
c_toString_case_30 x32 x33 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_30")(x)



c_toString_case_40 x4 x22@((Curry.Module.Prelude.:<) x23 x24) st = Curry.Module.PrettyFlat.c_toString_case_39(x4)(x23)(x24)(Curry.Module.Prelude.op_61_61(x23)(Curry.Module.Prelude.C_Char(']'))(st))(st)
c_toString_case_40 x4 x22@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_toString_case_40 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_40(x4)(x)(st))(i)(xs)(st)
c_toString_case_40 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_40")(x)



c_toString_case_39 x4 x23 x24 x25@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_toString_case_38(x4)(x24)(st)
c_toString_case_39 x4 x23 x24 x25@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_Nothing
c_toString_case_39 x4 x23 x24 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_39(x4)(x23)(x24)(x)(st))(i)(xs)(st)
c_toString_case_39 x4 x23 x24 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_39")(x)



c_toString_case_38 x4 x24@Curry.Module.Prelude.List st = Curry.Module.PrettyFlat.c_toString_case_37(x4)(st)
c_toString_case_38 x4 x24@((Curry.Module.Prelude.:<) x27 x28) st = Curry.Module.Prelude.C_Nothing
c_toString_case_38 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_38(x4)(x)(st))(i)(xs)(st)
c_toString_case_38 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_38")(x)



c_toString_case_37 x4@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Just(Curry.Module.Prelude.List)
c_toString_case_37 x4@((Curry.Module.Prelude.:<) x25 x26) st = Curry.Module.Prelude.C_Nothing
c_toString_case_37 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_toString_case_37(x)(st))(i)(xs)(st)
c_toString_case_37 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.toString_case_37")(x)



c_patternDoc_case_63 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_qname(x1)(x3)(st)
c_patternDoc_case_63 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.PrettyFlat.c_patternDoc_case_62(x1)(x3)(x4)(Curry.Module.PrettyFlat.c_isTupleName(x3)(st))(st)
c_patternDoc_case_63 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_patternDoc_case_63(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_patternDoc_case_63 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.patternDoc_case_63")(x)



c_patternDoc_case_62 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_tupled(st))(Curry.Module.Prelude.c_map(Curry.Module.PrettyFlat.c_varDoc(st))(x4)(st))(st)
c_patternDoc_case_62 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.PrettyFlat.c_patternDoc_case_61(x1)(x3)(x4)(Curry.Module.Prelude.op_38_38(Curry.Module.PrettyFlat.c_isInfixName(x3)(st))(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_length(x4)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(st))(st)
c_patternDoc_case_62 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_patternDoc_case_62(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_patternDoc_case_62 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.patternDoc_case_62")(x)



c_patternDoc_case_61 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Pretty.op_60_62(Curry.Module.Pretty.op_60_62(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_varDoc(st))(Curry.Module.Prelude.op_33_33(x4)(Curry.Module.Prelude.C_Zero)(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_operator(st))(Curry.Module.Pretty.c_text(Curry.Module.Prelude.c_snd(x3)(st))(st))(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_varDoc(st))(Curry.Module.Prelude.op_33_33(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st)
c_patternDoc_case_61 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.PrettyFlat.c_patternDoc_case_60(x1)(x3)(x4)(Curry.Module.Prelude.c_otherwise(st))(st)
c_patternDoc_case_61 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_patternDoc_case_61(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_patternDoc_case_61 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.patternDoc_case_61")(x)



c_patternDoc_case_60 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.PrettyFlat.c_qname(x1)(x3)(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_hsep(st))(Curry.Module.Prelude.c_map(Curry.Module.PrettyFlat.c_varDoc(st))(x4)(st))(st))(st)
c_patternDoc_case_60 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_patternDoc_case_60(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_patternDoc_case_60 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.patternDoc_case_60")(x)



c_expDoc2_case_64 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2(Curry.Module.Pretty.c_lparen(st))(Curry.Module.Pretty.c_rparen(st))
c_expDoc2_case_64 x4@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.T2(Curry.Module.Pretty.c_empty(st))(Curry.Module.Pretty.c_empty(st))
c_expDoc2_case_64 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_expDoc2_case_64(x)(st))(i)(xs)(st)
c_expDoc2_case_64 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.expDoc2_case_64")(x)



c_expDoc2_case_65 x1 x9 x10@(Curry.Module.Prelude.C_Just x15) st = x15
c_expDoc2_case_65 x1 x9 x10@Curry.Module.Prelude.C_Nothing st = Curry.Module.Prelude.C_Zero
c_expDoc2_case_65 x1 x9 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_expDoc2_case_65(x1)(x9)(x)(st))(i)(xs)(st)
c_expDoc2_case_65 x1 x9 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.expDoc2_case_65")(x)



c_expDoc2_case_69 x1 x2 x3 x4 x8 x9 x10 x11 x14 x15@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_36(Curry.Module.PrettyFlat.c_par(x4)(st))(Curry.Module.PrettyFlat.c_app(Curry.Module.PrettyFlat.c_expDoc(x1)(Curry.Module.Prelude.C_Zero)(x3)(Curry.Module.Prelude.C_True)(Curry.Module.Prelude.op_33_33(x10)(Curry.Module.Prelude.C_Zero)(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.PrettyFlat.c_expDoc(x1)(Curry.Module.Prelude.C_Zero)(x3)(Curry.Module.Prelude.C_True)(Curry.Module.Prelude.op_33_33(x10)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(Curry.Module.Prelude.List))(st))(st)
c_expDoc2_case_69 x1 x2 x3 x4 x8 x9 x10 x11 x14 x15@Curry.Module.Prelude.C_False st = Curry.Module.PrettyFlat.c_expDoc2_case_68(x1)(x2)(x3)(x4)(x8)(x9)(x10)(x11)(x14)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x8)(Curry.Module.FlatCurry.C_ConsCall)(st))(Curry.Module.PrettyFlat.c_isTupleName(x9)(st))(st))(st)
c_expDoc2_case_69 x1 x2 x3 x4 x8 x9 x10 x11 x14 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_expDoc2_case_69(x1)(x2)(x3)(x4)(x8)(x9)(x10)(x11)(x14)(x)(st))(i)(xs)(st)
c_expDoc2_case_69 x1 x2 x3 x4 x8 x9 x10 x11 x14 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.expDoc2_case_69")(x)



c_expDoc2_case_68 x1 x2 x3 x4 x8 x9 x10 x11 x14 x15@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_tupled(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_expDoc(x1)(Curry.Module.Prelude.C_Zero)(x3)(Curry.Module.Prelude.C_False)))(x10)(st))(st)
c_expDoc2_case_68 x1 x2 x3 x4 x8 x9 x10 x11 x14 x15@Curry.Module.Prelude.C_False st = Curry.Module.PrettyFlat.c_expDoc2_case_67(x1)(x2)(x3)(x4)(x9)(x10)(x11)(x14)(Curry.Module.Prelude.op_38_38(Curry.Module.PrettyFlat.c_isInfixName(x9)(st))(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_length(x10)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st))(st))(st)
c_expDoc2_case_68 x1 x2 x3 x4 x8 x9 x10 x11 x14 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_expDoc2_case_68(x1)(x2)(x3)(x4)(x8)(x9)(x10)(x11)(x14)(x)(st))(i)(xs)(st)
c_expDoc2_case_68 x1 x2 x3 x4 x8 x9 x10 x11 x14 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.expDoc2_case_68")(x)



c_expDoc2_case_67 x1 x2 x3 x4 x9 x10 x11 x14 x15@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_36(Curry.Module.Pretty.c_align(st))(Curry.Module.PrettyFlat.c_precFillEncloseSep(x14)(x2)(Curry.Module.PrettyFlat.c_expDoc2'46_'35selFP3'35lbr(x11)(st))(Curry.Module.PrettyFlat.c_expDoc2'46_'35selFP4'35rbr(x11)(st))(Curry.Module.Pretty.c_empty(st))((Curry.Module.Prelude.:<)(Curry.Module.PrettyFlat.c_expDoc(x1)(x14)(x3)(Curry.Module.Prelude.C_True)(Curry.Module.Prelude.op_33_33(x10)(Curry.Module.Prelude.C_Zero)(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.Pretty.op_60_62(Curry.Module.Pretty.op_60_62(Curry.Module.Pretty.c_space(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_operator(st))(Curry.Module.Pretty.c_text(Curry.Module.Prelude.c_snd(x9)(st))(st))(st))(st))(Curry.Module.Pretty.c_space(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.PrettyFlat.c_expDoc(x1)(x14)(x3)(Curry.Module.Prelude.C_True)(Curry.Module.Prelude.op_33_33(x10)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(Curry.Module.Prelude.List))))(st))(st)
c_expDoc2_case_67 x1 x2 x3 x4 x9 x10 x11 x14 x15@Curry.Module.Prelude.C_False st = Curry.Module.PrettyFlat.c_expDoc2_case_66(x1)(x3)(x4)(x9)(x10)(Curry.Module.Prelude.c_otherwise(st))(st)
c_expDoc2_case_67 x1 x2 x3 x4 x9 x10 x11 x14 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_expDoc2_case_67(x1)(x2)(x3)(x4)(x9)(x10)(x11)(x14)(x)(st))(i)(xs)(st)
c_expDoc2_case_67 x1 x2 x3 x4 x9 x10 x11 x14 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.expDoc2_case_67")(x)



c_expDoc2_case_66 x1 x3 x4 x9 x10 x11@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_36(Curry.Module.PrettyFlat.c_par(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.c_not(Curry.Module.Prelude.c_null(x10)(st))(st))(x4)(st))(st))(Curry.Module.PrettyFlat.c_app(Curry.Module.PrettyFlat.c_qname(x3)(x9)(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_expDoc(x1)(Curry.Module.Prelude.C_Zero)(x3)(Curry.Module.Prelude.C_True)))(x10)(st))(st))(st)
c_expDoc2_case_66 x1 x3 x4 x9 x10 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_expDoc2_case_66(x1)(x3)(x4)(x9)(x10)(x)(st))(i)(xs)(st)
c_expDoc2_case_66 x1 x3 x4 x9 x10 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.expDoc2_case_66")(x)



c_expDoc'46_'35lambda7_case_70 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_consname(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List)))(st)
c_expDoc'46_'35lambda7_case_70 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_literal(st))(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_dquotes(st))(Curry.Module.Pretty.c_text(x1)(st))(st))(st)
c_expDoc'46_'35lambda7_case_70 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_expDoc'46_'35lambda7_case_70(x1)(x)(st))(i)(xs)(st)
c_expDoc'46_'35lambda7_case_70 x1 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.expDoc._#lambda7_case_70")(x)



c_typeExprDoc_case_74 x1 x2 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_qname(x1)(x5)(st)
c_typeExprDoc_case_74 x1 x2 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.PrettyFlat.c_typeExprDoc_case_73(x1)(x2)(x5)(x6)(Curry.Module.Prelude.op_61_61(x5)(Curry.Module.Prelude.T2(Curry.Module.PrettyFlat.c_prelude(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))))(st))(st)
c_typeExprDoc_case_74 x1 x2 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_typeExprDoc_case_74(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c_typeExprDoc_case_74 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.typeExprDoc_case_74")(x)



c_typeExprDoc_case_73 x1 x2 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_brackets(st))(Curry.Module.PrettyFlat.c_typeExprDoc(x1)(Curry.Module.Prelude.C_False)(Curry.Module.Prelude.c_head(x6)(st))(st))(st)
c_typeExprDoc_case_73 x1 x2 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.PrettyFlat.c_typeExprDoc_case_72(x1)(x2)(x5)(x6)(Curry.Module.PrettyFlat.c_isTupleName(x5)(st))(st)
c_typeExprDoc_case_73 x1 x2 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_typeExprDoc_case_73(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c_typeExprDoc_case_73 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.typeExprDoc_case_73")(x)



c_typeExprDoc_case_72 x1 x2 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_tupled(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_typeExprDoc(x1)(Curry.Module.Prelude.C_False)))(x6)(st))(st)
c_typeExprDoc_case_72 x1 x2 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.PrettyFlat.c_typeExprDoc_case_71(x1)(x2)(x5)(x6)(Curry.Module.Prelude.c_otherwise(st))(st)
c_typeExprDoc_case_72 x1 x2 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_typeExprDoc_case_72(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c_typeExprDoc_case_72 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.typeExprDoc_case_72")(x)



c_typeExprDoc_case_71 x1 x2 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_36(Curry.Module.PrettyFlat.c_par(x2)(st))(Curry.Module.PrettyFlat.c_app(Curry.Module.PrettyFlat.c_qname(x1)(x5)(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.PrettyFlat.c_typeExprDoc(x1)(Curry.Module.Prelude.C_True)))(x6)(st))(st))(st)
c_typeExprDoc_case_71 x1 x2 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_typeExprDoc_case_71(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c_typeExprDoc_case_71 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.typeExprDoc_case_71")(x)



c_tvarDoc_case_76 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Pretty.c_text(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))(Curry.Module.Prelude.List))(Curry.Module.Prelude.c_show(x1)(st))(st))(st)
c_tvarDoc_case_76 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.PrettyFlat.c_tvarDoc_case_75(x1)(Curry.Module.Prelude.c_otherwise(st))(st)
c_tvarDoc_case_76 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_tvarDoc_case_76(x1)(x)(st))(i)(xs)(st)
c_tvarDoc_case_76 x1 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.tvarDoc_case_76")(x)



c_tvarDoc_case_75 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Pretty.c_text((Curry.Module.Prelude.:<)(Curry.Module.Prelude.c_chr(Curry.Module.Prelude.op_43(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi))))))))(x1)(st))(st))(Curry.Module.Prelude.List))(st)
c_tvarDoc_case_75 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_tvarDoc_case_75(x1)(x)(st))(i)(xs)(st)
c_tvarDoc_case_75 x1 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.tvarDoc_case_75")(x)



c_opDoc_case_78 x3 x4 x2@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.op_60_43_62(st))(Curry.Module.Pretty.op_60_62(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_keyword(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))(Curry.Module.Prelude.List))))))(st))(Curry.Module.PrettyFlat.c_opDoc'46fixDoc'46114(x3)(st))(st))(st))(Curry.Module.Pretty.c_int(x4)(st))(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_operator(st))(Curry.Module.Pretty.c_text(Curry.Module.PrettyFlat.c_opDoc_case_77(x2)(x6)(Curry.Module.PrettyFlat.c_isInfixName(x2)(st))(st))(st))(st))(st)
c_opDoc_case_78 x3 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_opDoc_case_78(x3)(x4)(x)(st))(i)(xs)(st)
c_opDoc_case_78 x3 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.opDoc_case_78")(x)



c_opDoc_case_77 x2 x6 x7@Curry.Module.Prelude.C_True st = x6
c_opDoc_case_77 x2 x6 x7@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('`'))(Curry.Module.Prelude.op_43_43(x6)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('`'))(Curry.Module.Prelude.List))(st))
c_opDoc_case_77 x2 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_opDoc_case_77(x2)(x6)(x)(st))(i)(xs)(st)
c_opDoc_case_77 x2 x6 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.opDoc_case_77")(x)



c_exportedNames'46typeExpDoc'4689_case_79 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Pretty.c_empty(st)
c_exportedNames'46typeExpDoc'4689_case_79 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Pretty.c_text((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List)))))(st)
c_exportedNames'46typeExpDoc'4689_case_79 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_exportedNames'46typeExpDoc'4689_case_79(x2)(x)(st))(i)(xs)(st)
c_exportedNames'46typeExpDoc'4689_case_79 x2 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.exportedNames.typeExpDoc.89_case_79")(x)



c_correctName_case_81 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.PrettyFlat.c_correctName_case_80(x2)(x3)(x4)(Curry.Module.Prelude.op_61_61(x3)(Curry.Module.Prelude.C_Char('_'))(st))(st)
c_correctName_case_81 x2@Curry.Module.Prelude.List st = x2
c_correctName_case_81 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_correctName_case_81(x)(st))(i)(xs)(st)
c_correctName_case_81 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.correctName_case_81")(x)



c_correctName_case_80 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = x4
c_correctName_case_80 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = x2
c_correctName_case_80 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_correctName_case_80(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_correctName_case_80 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.correctName_case_80")(x)



c_qname'46txt'4665_case_82 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_consname(st))((Curry.Module.Prelude.:<)(x2)(x3))(st)
c_qname'46txt'4665_case_82 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.Pretty.c_text((Curry.Module.Prelude.:<)(x2)(x3))(st)
c_qname'46txt'4665_case_82 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_qname'46txt'4665_case_82(x2)(x3)(x)(st))(i)(xs)(st)
c_qname'46txt'4665_case_82 x2 x3 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.qname.txt.65_case_82")(x)



c_qname_case_87 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_operator(st))(Curry.Module.Pretty.c_text(x4)(st))(st)
c_qname_case_87 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.PrettyFlat.c_qname_case_86(x1)(x2)(x3)(x4)(Curry.Module.PrettyFlat.c_isInfixName(x2)(st))(st)
c_qname_case_87 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_qname_case_87(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_qname_case_87 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.qname_case_87")(x)



c_qname_case_86 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_qname_case_85(x1)(x3)(x4)(Curry.Module.Prelude.op_61_61(x3)(x1)(st))(st)
c_qname_case_86 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.PrettyFlat.c_qname_case_84(x1)(x3)(x4)(Curry.Module.Prelude.c_otherwise(st))(st)
c_qname_case_86 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_qname_case_86(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_qname_case_86 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.qname_case_86")(x)



c_qname_case_84 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_qname_case_83(x1)(x3)(x4)(Curry.Module.Prelude.op_61_61(x3)(x1)(st))(st)
c_qname_case_84 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_qname_case_84(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_qname_case_84 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.qname_case_84")(x)



c_qname_case_83 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.PrettyFlat.c_qname'46txt'4665(Curry.Module.PrettyFlat.c_correctName(x4)(st))(st)
c_qname_case_83 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Pretty.op_60_62(Curry.Module.Pretty.op_60_62(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_consname(st))(x3)(st))(Curry.Module.Pretty.c_dot(st))(st))(Curry.Module.PrettyFlat.c_qname'46txt'4665(x4)(st))(st)
c_qname_case_83 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_qname_case_83(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_qname_case_83 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.qname_case_83")(x)



c_qname_case_85 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_parens(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_operator(st))(Curry.Module.Pretty.c_text(x4)(st))(st))(st)
c_qname_case_85 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_parens(st))(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_operator(st))(Curry.Module.Pretty.op_60_62(Curry.Module.Pretty.op_60_62(Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_consname(st))(x3)(st))(Curry.Module.Pretty.c_dot(st))(st))(Curry.Module.PrettyFlat.c_qname'46txt'4665(x4)(st))(st))(st))(st)
c_qname_case_85 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_qname_case_85(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_qname_case_85 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.qname_case_85")(x)



c_app_case_88 x1 x2 x3@Curry.Module.Prelude.C_True st = x1
c_app_case_88 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_apply(Curry.Module.PrettyFlat.c_block(st))(Curry.Module.Pretty.c_fillEncloseSep(Curry.Module.Pretty.c_empty(st))(Curry.Module.Pretty.c_empty(st))(Curry.Module.Pretty.c_space(st))((Curry.Module.Prelude.:<)(x1)(x2))(st))(st)
c_app_case_88 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_app_case_88(x1)(x2)(x)(st))(i)(xs)(st)
c_app_case_88 x1 x2 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.app_case_88")(x)



c_def_case_89 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Pretty.c_empty(st)
c_def_case_89 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.Pretty.op_60_62(Curry.Module.Pretty.c_space(st))(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_align(st))(Curry.Module.Prelude.c_apply(Curry.Module.Pretty.c_fillSep(st))(Curry.Module.Prelude.c_map(Curry.Module.PrettyFlat.c_varDoc(st))(x2)(st))(st))(st))(st)
c_def_case_89 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_def_case_89(x2)(x)(st))(i)(xs)(st)
c_def_case_89 x2 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.def_case_89")(x)



c_precFillEncloseSep'46pre'4611_case_90 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Pretty.c_empty(st)
c_precFillEncloseSep'46pre'4611_case_90 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = x3
c_precFillEncloseSep'46pre'4611_case_90 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.PrettyFlat.c_precFillEncloseSep'46pre'4611_case_90(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c_precFillEncloseSep'46pre'4611_case_90 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("PrettyFlat.precFillEncloseSep.pre.11_case_90")(x)


