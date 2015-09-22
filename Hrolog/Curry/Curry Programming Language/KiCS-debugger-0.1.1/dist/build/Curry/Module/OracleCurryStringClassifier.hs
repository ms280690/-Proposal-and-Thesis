{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleCurryStringClassifier (module Curry.Module.OracleCurryStringClassifier) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.CurryStringClassifier
import Curry.Module.Char
import Curry.Module.Prelude
import Curry.Module.OracleChar
import Curry.Module.OraclePrelude



-- begin included



-- end included

c_isSmallComment :: Curry.Module.CurryStringClassifier.C_Token -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isSmallComment x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_88(x2)(x1)(st))(st)



c_isBigComment :: Curry.Module.CurryStringClassifier.C_Token -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isBigComment x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_87(x2)(x1)(st))(st)



c_isComment :: Curry.Module.CurryStringClassifier.C_Token -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isComment x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OracleCurryStringClassifier.c_isSmallComment(x2)(x1)(st))(Curry.Module.OracleCurryStringClassifier.c_isBigComment(x2)(x3)(st))(x4)(st))(st)



c_isText :: Curry.Module.CurryStringClassifier.C_Token -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isText x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_86(x2)(x1)(st))(st)



c_isLetter :: Curry.Module.CurryStringClassifier.C_Token -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isLetter x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_85(x2)(x1)(st))(st)



c_isCode :: Curry.Module.CurryStringClassifier.C_Token -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isCode x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_84(x2)(x1)(st))(st)



c_isModuleHead :: Curry.Module.CurryStringClassifier.C_Token -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isModuleHead x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_83(x2)(x1)(st))(st)



c_isMeta :: Curry.Module.CurryStringClassifier.C_Token -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isMeta x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_82(x2)(x1)(st))(st)



c_weaveIntoCode :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token))) -> (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_weaveIntoCode x2 x3 x1 st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(let {x4 = Curry.Module.OracleCurryStringClassifier.c_unweaveCode(x3)(x1)(st)} in let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c_weave(Curry.Module.Prelude.T2(Curry.Module.Oracle.c_apply(x2)(Curry.Module.OracleCurryStringClassifier.c_weaveIntoCode'46_'35selFP3'35cs(x4)(x7)(st))(x9)(st))(Curry.Module.OracleCurryStringClassifier.c_weaveIntoCode'46_'35selFP4'35ncs(x4)(x8)(st)))(x10)(st))(st))(st))(st))(st)



c_weaveIntoCode'46_'35selFP3'35cs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_weaveIntoCode'46_'35selFP3'35cs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_81(x2)(x1)(st))(st)



c_weaveIntoCode'46_'35selFP4'35ncs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_weaveIntoCode'46_'35selFP4'35ncs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_80(x2)(x1)(st))(st)



c_unweaveCode :: (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token)
c_unweaveCode x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_79(x2)(x1)(st))(st)



c_unweaveCode'46_'35selFP6'35cs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_unweaveCode'46_'35selFP6'35cs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_77(x2)(x1)(st))(st)



c_unweaveCode'46_'35selFP7'35ncs :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_unweaveCode'46_'35selFP7'35ncs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_76(x2)(x1)(st))(st)



c_weave :: (Curry t0) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t0) (Curry.Module.Prelude.List t0)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_weave x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_75(x2)(x1)(st))(st)



c_scan :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_scan x2 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(let {x3 = Curry.Module.Oracle.c_unknown(x1)(st)} in Curry.Module.OracleCurryStringClassifier.c_modHead(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_id))))(Curry.Module.OracleCurryStringClassifier.c_stateScan(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(Curry.Module.CurryStringClassifier.C_Code(x3))(x3)(x2)(x4)(st))(x5)(st))(st)



c_stateScan :: Curry.Module.Prelude.C_Int -> Curry.Module.CurryStringClassifier.C_Token -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_stateScan x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_68(x2)(x3)(x4)(x5)(x1)(st))(st)



c_stateScan'46_'35selFP9'35comment :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_stateScan'46_'35selFP9'35comment x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_29(x2)(x1)(st))(st)



c_stateScan'46_'35selFP10'35rest :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_stateScan'46_'35selFP10'35rest x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_28(x2)(x1)(st))(st)



c_modHead :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_modHead x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_27(x2)(x3)(x1)(st))(st)



c_modHead'46_'35lambda11 :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_modHead'46_'35lambda11 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\n'))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\r'))(x3)(st))(x4)(st))(st)



c_modHeadInLine :: (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_modHeadInLine x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_19(x2)(x3)(x1)(st))(st)



c_modHeadInLine'46_'35lambda13 :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_modHeadInLine'46_'35lambda13 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\n'))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\r'))(x3)(st))(x4)(st))(st)



c_headers :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_headers x1 st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))(Curry.Module.Prelude.List)))))))(st)



c_lineBeginsWith :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_lineBeginsWith x2 x3 x1 st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(let {x4 = Curry.Module.OraclePrelude.c_length(x3)(x1)(st)} in let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c__case_15(x2)(x3)(x4)(Curry.Module.OraclePrelude.op_60(Curry.Module.OraclePrelude.c_length(x2)(x5)(st))(x4)(x6)(st))(x7)(st))(st))(st)



c_lineBeginsWith'46_'35selFP12'35s'39 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_lineBeginsWith'46_'35selFP12'35s'39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_13(x2)(x1)(st))(st)



c_lineBeginsWith'46_'35selFP13'35rest :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_lineBeginsWith'46_'35selFP13'35rest x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_12(x2)(x1)(st))(st)



c_isSep :: Curry.Module.Prelude.C_Char -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isSep x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OracleChar.c_isSpace(x2)(x1)(st))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_elem(x2)(x3)(st))(Curry.Module.OracleCurryStringClassifier.c_infixIDs(x4)(st))(x5)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_elem(x2)(x6)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('{'))(Curry.Module.Prelude.List))))(x7)(st))(x8)(st))(x9)(st))(st)



c_infixIDs :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_infixIDs x1 st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('~'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('@'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('#'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('$'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('%'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('^'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('*'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('+'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('?'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('|'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List)))))))))))))))))))))(st)



c_delimiters :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_delimiters x1 st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('{'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('}'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List)))))))))(st)



c_toBeEscaped :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_toBeEscaped x1 st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List))))))(st)



c_maybeCode :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_maybeCode x2 x3 x1 st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Code(x2))(x3))(st)



c_maybeMo :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token
c_maybeMo x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c__case_11(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x2)(Curry.Module.Prelude.List)(x1)(st))(x4)(st))(st)



c_plainCode :: (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_plainCode x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_8(x2)(x1)(st))(st)



c_unscan :: (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_unscan x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_4(x2)(x1)(st))(st)



c_readScan :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.CurryStringClassifier.C_Token)))
c_readScan x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OraclePrelude.c_readFile(x2)(x1)(st))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_return))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCurryStringClassifier.c_scan))))(x3)(st))(x4)(st))(st)



c_testScan :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_testScan x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OraclePrelude.c_readFile(x2)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCurryStringClassifier.c_testScan'46_'35lambda18))))(x3)(st))(st)



c_testScan'46_'35lambda18 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_testScan'46_'35lambda18 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.c_print(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OracleCurryStringClassifier.c_unscan(Curry.Module.OracleCurryStringClassifier.c_scan(x2)(x1)(st))(x3)(st))(x2)(x4)(st))(x5)(st))(st)



c_testWeave :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_testWeave x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OraclePrelude.c_readFile(x2)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCurryStringClassifier.c_testWeave'46_'35lambda19))))(x3)(st))(st)



c_testWeave'46_'35lambda19 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_testWeave'46_'35lambda19 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.c_print(Curry.Module.OraclePrelude.op_61_61(Curry.Module.OracleCurryStringClassifier.c_unscan(Curry.Module.OracleCurryStringClassifier.c_weave(Curry.Module.OracleCurryStringClassifier.c_unweaveCode(Curry.Module.OracleCurryStringClassifier.c_scan(x2)(x1)(st))(x3)(st))(x4)(st))(x5)(st))(x2)(x6)(st))(x7)(st))(st)



c__case_4 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_4_case__88(x1)(x2)(st))(st)



c__case_3 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_3_case__87(x1)(x4)(x3)(st))(st)



c__case_2 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_2_case__86(x1)(x4)(st))(st)



c__case_1 x4 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_1_case__85(x1)(x4)(x7)(x6)(st))(st)



c__case_0 x4 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_0_case__84(x1)(x4)(x7)(x8)(st))(st)



c__case_8 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_8_case__83(x1)(x2)(st))(st)



c__case_7 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_7_case__82(x1)(x4)(x3)(st))(st)



c__case_6 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_6_case__81(x1)(x5)(x4)(st))(st)



c__case_5 x4 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_5_case__80(x1)(x4)(x5)(x7)(x6)(st))(st)



c__case_11 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_11_case__79(x1)(x2)(x3)(x4)(st))(st)



c__case_10 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_10_case__78(x1)(x3)(st))(st)



c__case_9 x3 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_9_case__77(x1)(x3)(x5)(x4)(st))(st)



c__case_12 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_12_case__76(x1)(x2)(st))(st)



c__case_13 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_13_case__75(x1)(x2)(st))(st)



c__case_15 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_15_case__74(x1)(x2)(x3)(x5)(st))(st)



c__case_14 x2 x3 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_14_case__73(x1)(x2)(x3)(x8)(st))(st)



c__case_19 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_19_case__72(x1)(x2)(x3)(st))(st)



c__case_18 x2 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_18_case__71(x1)(x2)(x5)(x4)(st))(st)



c__case_17 x2 x5 x6 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_17_case__70(x1)(x2)(x5)(x6)(x9)(st))(st)



c__case_16 x2 x5 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_16_case__69(x1)(x2)(x5)(x6)(x7)(x8)(st))(st)



c__case_27 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_27_case__68(x1)(x2)(x3)(st))(st)



c__case_26 x2 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_26_case__67(x1)(x2)(x5)(x4)(st))(st)



c__case_25 x2 x5 x6 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_25_case__66(x1)(x2)(x5)(x6)(x9)(st))(st)



c__case_24 x2 x5 x6 x8 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_24_case__65(x1)(x2)(x5)(x6)(x8)(x7)(st))(st)



c__case_22 x2 x5 x6 x11 x12 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_22_case__64(x1)(x2)(x5)(x6)(x11)(x12)(x8)(st))(st)



c__case_20 x2 x5 x6 x11 x12 x13 x14 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_20_case__63(x1)(x2)(x5)(x6)(x11)(x12)(x13)(x14)(x15)(st))(st)



c__case_21 x2 x5 x6 x11 x12 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_21_case__62(x1)(x2)(x5)(x6)(x11)(x12)(x13)(st))(st)



c__case_23 x2 x5 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_23_case__61(x1)(x2)(x5)(x8)(st))(st)



c__case_28 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_28_case__60(x1)(x2)(st))(st)



c__case_29 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_29_case__59(x1)(x2)(st))(st)



c__case_68 x2 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_68_case__58(x1)(x2)(x3)(x4)(x5)(st))(st)



c__case_67 x2 x4 x6 x7 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_67_case__57(x1)(x2)(x4)(x6)(x7)(x3)(st))(st)



c__case_33 x2 x4 x6 x35 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_33_case__56(x1)(x2)(x4)(x6)(x35)(x7)(st))(st)



c__case_32 x2 x4 x6 x35 x36 x37 x38 x39 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_32_case__55(x1)(x2)(x4)(x6)(x35)(x36)(x37)(x38)(x39)(st))(st)



c__case_31 x2 x4 x6 x35 x36 x37 x38 x39 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_31_case__54(x1)(x2)(x4)(x6)(x35)(x36)(x37)(x38)(x39)(st))(st)



c__case_30 x2 x4 x6 x35 x36 x37 x38 x39 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_30_case__53(x1)(x2)(x4)(x6)(x35)(x36)(x37)(x38)(x39)(st))(st)



c__case_37 x2 x4 x6 x31 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_37_case__52(x1)(x2)(x4)(x6)(x31)(x7)(st))(st)



c__case_36 x2 x4 x6 x31 x32 x33 x34 x35 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_36_case__51(x1)(x2)(x4)(x6)(x31)(x32)(x33)(x34)(x35)(st))(st)



c__case_35 x2 x4 x6 x31 x32 x33 x34 x35 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_35_case__50(x1)(x2)(x4)(x6)(x31)(x32)(x33)(x34)(x35)(st))(st)



c__case_34 x2 x4 x6 x31 x32 x33 x34 x35 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_34_case__49(x1)(x2)(x4)(x6)(x31)(x32)(x33)(x34)(x35)(st))(st)



c__case_43 x2 x4 x6 x27 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_43_case__48(x1)(x2)(x4)(x6)(x27)(x7)(st))(st)



c__case_41 x2 x4 x6 x27 x28 x29 x30 x31 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_41_case__47(x1)(x2)(x4)(x6)(x27)(x28)(x29)(x30)(x31)(st))(st)



c__case_40 x2 x4 x6 x27 x28 x29 x30 x31 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_40_case__46(x1)(x2)(x4)(x6)(x27)(x28)(x29)(x30)(x31)(st))(st)



c__case_39 x2 x4 x6 x27 x28 x29 x30 x31 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_39_case__45(x1)(x2)(x4)(x6)(x27)(x28)(x29)(x30)(x31)(st))(st)



c__case_38 x2 x4 x6 x27 x28 x29 x30 x31 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_38_case__44(x1)(x2)(x4)(x6)(x27)(x28)(x29)(x30)(x31)(st))(st)



c__case_42 x6 x27 x28 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_42_case__43(x1)(x27)(x28)(st))(st)



c__case_66 x2 x4 x6 x8 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_66_case__42(x1)(x2)(x4)(x6)(x8)(x7)(st))(st)



c__case_65 x2 x4 x6 x8 x9 x10 x11 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_65_case__41(x1)(x2)(x4)(x6)(x8)(x9)(x10)(x11)(x12)(st))(st)



c__case_64 x2 x4 x6 x8 x9 x10 x11 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_64_case__40(x1)(x2)(x4)(x6)(x8)(x9)(x10)(x11)(x15)(st))(st)



c__case_63 x2 x4 x6 x8 x9 x10 x11 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_63_case__39(x1)(x2)(x4)(x6)(x8)(x9)(x10)(x11)(x12)(st))(st)



c__case_62 x2 x4 x6 x8 x9 x10 x11 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_62_case__38(x1)(x2)(x4)(x6)(x8)(x9)(x10)(x11)(x12)(st))(st)



c__case_61 x2 x4 x6 x8 x9 x10 x11 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_61_case__37(x1)(x2)(x4)(x6)(x8)(x9)(x10)(x11)(x12)(st))(st)



c__case_45 x2 x4 x6 x8 x9 x10 x11 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_45_case__36(x1)(x2)(x4)(x6)(x8)(x9)(x10)(x11)(x12)(st))(st)



c__case_44 x2 x4 x6 x8 x9 x10 x11 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_44_case__35(x1)(x2)(x4)(x6)(x8)(x9)(x10)(x11)(x12)(st))(st)



c__case_60 x2 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_60_case__34(x1)(x2)(x11)(x10)(st))(st)



c__case_59 x2 x11 x15 x16 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_59_case__33(x1)(x2)(x11)(x15)(x16)(x17)(st))(st)



c__case_47 x2 x11 x15 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_47_case__32(x1)(x2)(x11)(x15)(x16)(st))(st)



c__case_46 x2 x11 x15 x25 x26 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_46_case__31(x1)(x2)(x11)(x15)(x26)(x27)(st))(st)



c__case_58 x2 x11 x15 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_58_case__30(x1)(x2)(x11)(x15)(x16)(st))(st)



c__case_57 x2 x11 x15 x17 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_57_case__29(x1)(x2)(x11)(x15)(x17)(x18)(st))(st)



c__case_48 x2 x11 x15 x17 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_48_case__28(x1)(x2)(x11)(x15)(x18)(st))(st)



c__case_56 x2 x11 x15 x17 x18 x19 x20 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_56_case__27(x1)(x2)(x11)(x15)(x17)(x18)(x19)(x20)(x21)(st))(st)



c__case_55 x2 x11 x15 x17 x18 x19 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_55_case__26(x1)(x2)(x11)(x15)(x17)(x18)(x19)(x20)(st))(st)



c__case_49 x2 x11 x15 x17 x18 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_49_case__25(x1)(x2)(x11)(x15)(x18)(x19)(st))(st)



c__case_54 x2 x11 x15 x17 x18 x19 x21 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_54_case__24(x1)(x2)(x11)(x15)(x17)(x18)(x19)(x21)(x22)(st))(st)



c__case_50 x2 x11 x15 x17 x18 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_50_case__23(x1)(x2)(x11)(x15)(x18)(x19)(st))(st)



c__case_53 x2 x11 x15 x17 x18 x19 x21 x23 x24 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_53_case__22(x1)(x2)(x11)(x15)(x17)(x18)(x19)(x21)(x24)(x25)(st))(st)



c__case_51 x2 x11 x15 x17 x18 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_51_case__21(x1)(x2)(x11)(x15)(x18)(x19)(st))(st)



c__case_52 x2 x11 x17 x19 x21 x24 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_52_case__20(x1)(x2)(x11)(x17)(x19)(x21)(x24)(x25)(st))(st)



c__case_75 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_75_case__19(x1)(x2)(st))(st)



c__case_74 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_74_case__18(x1)(x4)(x3)(st))(st)



c__case_71 x4 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_71_case__17(x1)(x4)(x9)(x10)(st))(st)



c__case_69 x9 x13 x14 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_69_case__16(x1)(x9)(x13)(x14)(x4)(st))(st)



c__case_70 x9 x10 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_70_case__15(x1)(x9)(x10)(x4)(st))(st)



c__case_73 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_73_case__14(x1)(x4)(st))(st)



c__case_72 x5 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_72_case__13(x1)(x5)(x6)(st))(st)



c__case_76 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_76_case__12(x1)(x2)(st))(st)



c__case_77 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_77_case__11(x1)(x2)(st))(st)



c__case_79 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_79_case__10(x1)(x2)(st))(st)



c__case_78 x3 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_78_case__9(x1)(x3)(x6)(x7)(x8)(st))(st)



c__case_80 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_80_case__8(x1)(x2)(st))(st)



c__case_81 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_81_case__7(x1)(x2)(st))(st)



c__case_82 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_82_case__6(x1)(x2)(st))(st)



c__case_83 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_83_case__5(x1)(x2)(st))(st)



c__case_84 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_84_case__4(x1)(x2)(st))(st)



c__case_85 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_85_case__3(x1)(x2)(st))(st)



c__case_86 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_86_case__2(x1)(x2)(st))(st)



c__case_87 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_87_case__1(x1)(x2)(st))(st)



c__case_88 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_88_case__0(x1)(x2)(st))(st)



c__case_88_case__0 x1 x2@(Curry.Module.CurryStringClassifier.C_SmallComment x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_88_case__0 x1 x2@(Curry.Module.CurryStringClassifier.C_BigComment x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_88_case__0 x1 x2@(Curry.Module.CurryStringClassifier.C_Text x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_88_case__0 x1 x2@(Curry.Module.CurryStringClassifier.C_Letter x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_88_case__0 x1 x2@(Curry.Module.CurryStringClassifier.C_Code x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_88_case__0 x1 x2@(Curry.Module.CurryStringClassifier.C_ModuleHead x8) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_88_case__0 x1 x2@(Curry.Module.CurryStringClassifier.C_Meta x9) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_88_case__0 x1 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_88_case__0(x1)(x)(st))(i)(xs)(st)
c__case_88_case__0 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_88_case__0")(x)



c__case_87_case__1 x1 x2@(Curry.Module.CurryStringClassifier.C_BigComment x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_87_case__1 x1 x2@(Curry.Module.CurryStringClassifier.C_SmallComment x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_87_case__1 x1 x2@(Curry.Module.CurryStringClassifier.C_Text x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_87_case__1 x1 x2@(Curry.Module.CurryStringClassifier.C_Letter x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_87_case__1 x1 x2@(Curry.Module.CurryStringClassifier.C_Code x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_87_case__1 x1 x2@(Curry.Module.CurryStringClassifier.C_ModuleHead x8) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_87_case__1 x1 x2@(Curry.Module.CurryStringClassifier.C_Meta x9) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_87_case__1 x1 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_87_case__1(x1)(x)(st))(i)(xs)(st)
c__case_87_case__1 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_87_case__1")(x)



c__case_86_case__2 x1 x2@(Curry.Module.CurryStringClassifier.C_Text x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_86_case__2 x1 x2@(Curry.Module.CurryStringClassifier.C_SmallComment x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_86_case__2 x1 x2@(Curry.Module.CurryStringClassifier.C_BigComment x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_86_case__2 x1 x2@(Curry.Module.CurryStringClassifier.C_Letter x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_86_case__2 x1 x2@(Curry.Module.CurryStringClassifier.C_Code x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_86_case__2 x1 x2@(Curry.Module.CurryStringClassifier.C_ModuleHead x8) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_86_case__2 x1 x2@(Curry.Module.CurryStringClassifier.C_Meta x9) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_86_case__2 x1 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_86_case__2(x1)(x)(st))(i)(xs)(st)
c__case_86_case__2 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_86_case__2")(x)



c__case_85_case__3 x1 x2@(Curry.Module.CurryStringClassifier.C_Letter x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_85_case__3 x1 x2@(Curry.Module.CurryStringClassifier.C_SmallComment x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_85_case__3 x1 x2@(Curry.Module.CurryStringClassifier.C_BigComment x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_85_case__3 x1 x2@(Curry.Module.CurryStringClassifier.C_Text x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_85_case__3 x1 x2@(Curry.Module.CurryStringClassifier.C_Code x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_85_case__3 x1 x2@(Curry.Module.CurryStringClassifier.C_ModuleHead x8) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_85_case__3 x1 x2@(Curry.Module.CurryStringClassifier.C_Meta x9) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_85_case__3 x1 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_85_case__3(x1)(x)(st))(i)(xs)(st)
c__case_85_case__3 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_85_case__3")(x)



c__case_84_case__4 x1 x2@(Curry.Module.CurryStringClassifier.C_Code x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_84_case__4 x1 x2@(Curry.Module.CurryStringClassifier.C_SmallComment x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_84_case__4 x1 x2@(Curry.Module.CurryStringClassifier.C_BigComment x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_84_case__4 x1 x2@(Curry.Module.CurryStringClassifier.C_Text x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_84_case__4 x1 x2@(Curry.Module.CurryStringClassifier.C_Letter x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_84_case__4 x1 x2@(Curry.Module.CurryStringClassifier.C_ModuleHead x8) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_84_case__4 x1 x2@(Curry.Module.CurryStringClassifier.C_Meta x9) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_84_case__4 x1 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_84_case__4(x1)(x)(st))(i)(xs)(st)
c__case_84_case__4 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_84_case__4")(x)



c__case_83_case__5 x1 x2@(Curry.Module.CurryStringClassifier.C_ModuleHead x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_83_case__5 x1 x2@(Curry.Module.CurryStringClassifier.C_SmallComment x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_83_case__5 x1 x2@(Curry.Module.CurryStringClassifier.C_BigComment x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_83_case__5 x1 x2@(Curry.Module.CurryStringClassifier.C_Text x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_83_case__5 x1 x2@(Curry.Module.CurryStringClassifier.C_Letter x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_83_case__5 x1 x2@(Curry.Module.CurryStringClassifier.C_Code x8) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_83_case__5 x1 x2@(Curry.Module.CurryStringClassifier.C_Meta x9) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_83_case__5 x1 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_83_case__5(x1)(x)(st))(i)(xs)(st)
c__case_83_case__5 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_83_case__5")(x)



c__case_82_case__6 x1 x2@(Curry.Module.CurryStringClassifier.C_Meta x3) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_True)(st)
c__case_82_case__6 x1 x2@(Curry.Module.CurryStringClassifier.C_SmallComment x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_82_case__6 x1 x2@(Curry.Module.CurryStringClassifier.C_BigComment x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_82_case__6 x1 x2@(Curry.Module.CurryStringClassifier.C_Text x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_82_case__6 x1 x2@(Curry.Module.CurryStringClassifier.C_Letter x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_82_case__6 x1 x2@(Curry.Module.CurryStringClassifier.C_Code x8) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_82_case__6 x1 x2@(Curry.Module.CurryStringClassifier.C_ModuleHead x9) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_82_case__6 x1 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_82_case__6(x1)(x)(st))(i)(xs)(st)
c__case_82_case__6 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_82_case__6")(x)



c__case_81_case__7 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_81_case__7 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_81_case__7(x1)(x)(st))(i)(xs)(st)
c__case_81_case__7 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_81_case__7")(x)



c__case_80_case__8 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_80_case__8 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_80_case__8(x1)(x)(st))(i)(xs)(st)
c__case_80_case__8 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_80_case__8")(x)



c__case_78_case__9 x1 x3 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x3)(x6))(x7))(st)
c__case_78_case__9 x1 x3 x6 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(x6)((Curry.Module.Prelude.:<)(x3)(x7)))(st)
c__case_78_case__9 x1 x3 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_78_case__9(x1)(x3)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_78_case__9 x1 x3 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_78_case__9")(x)



c__case_79_case__10 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))(st)
c__case_79_case__10 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(let {x5 = Curry.Module.OracleCurryStringClassifier.c_unweaveCode(x4)(x1)(st)} in let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c__case_78(x3)(Curry.Module.OracleCurryStringClassifier.c_unweaveCode'46_'35selFP6'35cs(x5)(x8)(st))(Curry.Module.OracleCurryStringClassifier.c_unweaveCode'46_'35selFP7'35ncs(x5)(x9)(st))(Curry.Module.OracleCurryStringClassifier.c_isCode(x3)(x10)(st))(x11)(st))(st))(st))(st))(st)
c__case_79_case__10 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_79_case__10(x1)(x)(st))(i)(xs)(st)
c__case_79_case__10 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_79_case__10")(x)



c__case_77_case__11 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_77_case__11 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_77_case__11(x1)(x)(st))(i)(xs)(st)
c__case_77_case__11 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_77_case__11")(x)



c__case_76_case__12 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_76_case__12 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_76_case__12(x1)(x)(st))(i)(xs)(st)
c__case_76_case__12 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_76_case__12")(x)



c__case_72_case__13 x1 x5 x6@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(st)
c__case_72_case__13 x1 x5 x6@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_72_case__13 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_72_case__13(x1)(x5)(x)(st))(i)(xs)(st)
c__case_72_case__13 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_72_case__13")(x)



c__case_73_case__14 x1 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_73_case__14 x1 x4@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_72(x5)(x6)(x1)(st))(st)
c__case_73_case__14 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_73_case__14(x1)(x)(st))(i)(xs)(st)
c__case_73_case__14 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_73_case__14")(x)



c__case_70_case__15 x1 x9 x10 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(st)
c__case_70_case__15 x1 x9 x10 x4@((Curry.Module.Prelude.:<) x11 x12) st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x11)(Curry.Module.OracleCurryStringClassifier.c_weave(Curry.Module.Prelude.T2(x10)(x12))(x1)(st))))(st)
c__case_70_case__15 x1 x9 x10 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_70_case__15(x1)(x9)(x10)(x)(st))(i)(xs)(st)
c__case_70_case__15 x1 x9 x10 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_70_case__15")(x)



c__case_69_case__16 x1 x9 x13 x14 x4@((Curry.Module.Prelude.:<) x15 x16) st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x15)(Curry.Module.OracleCurryStringClassifier.c_weave(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(x13)(x14))(x16))(x1)(st))))(st)
c__case_69_case__16 x1 x9 x13 x14 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_69_case__16 x1 x9 x13 x14 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_69_case__16(x1)(x9)(x13)(x14)(x)(st))(i)(xs)(st)
c__case_69_case__16 x1 x9 x13 x14 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_69_case__16")(x)



c__case_71_case__17 x1 x4 x9 x10@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_70(x9)(x10)(x4)(x1)(st))(st)
c__case_71_case__17 x1 x4 x9 x10@((Curry.Module.Prelude.:<) x13 x14) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_69(x9)(x13)(x14)(x4)(x1)(st))(st)
c__case_71_case__17 x1 x4 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_71_case__17(x1)(x4)(x9)(x)(st))(i)(xs)(st)
c__case_71_case__17 x1 x4 x9 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_71_case__17")(x)



c__case_74_case__18 x1 x4 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_73(x4)(x1)(st))(st)
c__case_74_case__18 x1 x4 x3@((Curry.Module.Prelude.:<) x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_71(x4)(x9)(x10)(x1)(st))(st)
c__case_74_case__18 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_74_case__18(x1)(x4)(x)(st))(i)(xs)(st)
c__case_74_case__18 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_74_case__18")(x)



c__case_75_case__19 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_74(x4)(x3)(x1)(st))(st)
c__case_75_case__19 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_75_case__19(x1)(x)(st))(i)(xs)(st)
c__case_75_case__19 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_75_case__19")(x)



c__case_52_case__20 x1 x2 x11 x17 x19 x21 x24 x25@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Letter((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))))))(Curry.Module.OracleCurryStringClassifier.c_stateScan(x2)(Curry.Module.CurryStringClassifier.C_Code(x11))(x11)(x24)(x1)(st)))(st)
c__case_52_case__20 x1 x2 x11 x17 x19 x21 x24 x25@Curry.Module.Prelude.C_False st = let {x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_error))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))(x26)(st))(x27)(st))(st)
c__case_52_case__20 x1 x2 x11 x17 x19 x21 x24 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_52_case__20(x1)(x2)(x11)(x17)(x19)(x21)(x24)(x)(st))(i)(xs)(st)
c__case_52_case__20 x1 x2 x11 x17 x19 x21 x24 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_52_case__20")(x)



c__case_51_case__21 x1 x2 x11 x15 x18 x19@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Letter((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c_stateScan(x2)(Curry.Module.CurryStringClassifier.C_Code(x11))(x11)(x18)(x1)(st)))(st)
c__case_51_case__21 x1 x2 x11 x15 x18 x19@Curry.Module.Prelude.C_False st = let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_error))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))(x20)(st))(x21)(st))(st)
c__case_51_case__21 x1 x2 x11 x15 x18 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_51_case__21(x1)(x2)(x11)(x15)(x18)(x)(st))(i)(xs)(st)
c__case_51_case__21 x1 x2 x11 x15 x18 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_51_case__21")(x)



c__case_53_case__22 x1 x2 x11 x15 x17 x18 x19 x21 x24 x25@Curry.Module.Prelude.C_True st = let {x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c__case_52(x2)(x11)(x17)(x19)(x21)(x24)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_all(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleChar.c_isDigit))))(x1)(st))((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))))(x26)(st))(x27)(st))(st)
c__case_53_case__22 x1 x2 x11 x15 x17 x18 x19 x21 x24 x25@Curry.Module.Prelude.C_False st = let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x28)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c__case_51(x2)(x11)(x15)(x17)(x18)(Curry.Module.OraclePrelude.op_61_61(x17)(Curry.Module.Prelude.C_Char('\''))(x1)(st))(x28)(st))(st)
c__case_53_case__22 x1 x2 x11 x15 x17 x18 x19 x21 x24 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_53_case__22(x1)(x2)(x11)(x15)(x17)(x18)(x19)(x21)(x24)(x)(st))(i)(xs)(st)
c__case_53_case__22 x1 x2 x11 x15 x17 x18 x19 x21 x24 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_53_case__22")(x)



c__case_50_case__23 x1 x2 x11 x15 x18 x19@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Letter((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c_stateScan(x2)(Curry.Module.CurryStringClassifier.C_Code(x11))(x11)(x18)(x1)(st)))(st)
c__case_50_case__23 x1 x2 x11 x15 x18 x19@Curry.Module.Prelude.C_False st = let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_error))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))(x20)(st))(x21)(st))(st)
c__case_50_case__23 x1 x2 x11 x15 x18 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_50_case__23(x1)(x2)(x11)(x15)(x18)(x)(st))(i)(xs)(st)
c__case_50_case__23 x1 x2 x11 x15 x18 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_50_case__23")(x)



c__case_54_case__24 x1 x2 x11 x15 x17 x18 x19 x21 x22@((Curry.Module.Prelude.:<) x23 x24) st = let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c__case_53(x2)(x11)(x15)(x17)(x18)(x19)(x21)(x23)(x24)(Curry.Module.OraclePrelude.op_61_61(x23)(Curry.Module.Prelude.C_Char('\''))(x1)(st))(x25)(st))(st)
c__case_54_case__24 x1 x2 x11 x15 x17 x18 x19 x21 x22@Curry.Module.Prelude.List st = let {x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c__case_50(x2)(x11)(x15)(x17)(x18)(Curry.Module.OraclePrelude.op_61_61(x17)(Curry.Module.Prelude.C_Char('\''))(x1)(st))(x26)(st))(st)
c__case_54_case__24 x1 x2 x11 x15 x17 x18 x19 x21 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_54_case__24(x1)(x2)(x11)(x15)(x17)(x18)(x19)(x21)(x)(st))(i)(xs)(st)
c__case_54_case__24 x1 x2 x11 x15 x17 x18 x19 x21 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_54_case__24")(x)



c__case_49_case__25 x1 x2 x11 x15 x18 x19@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Letter((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c_stateScan(x2)(Curry.Module.CurryStringClassifier.C_Code(x11))(x11)(x18)(x1)(st)))(st)
c__case_49_case__25 x1 x2 x11 x15 x18 x19@Curry.Module.Prelude.C_False st = let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_error))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))(x20)(st))(x21)(st))(st)
c__case_49_case__25 x1 x2 x11 x15 x18 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_49_case__25(x1)(x2)(x11)(x15)(x18)(x)(st))(i)(xs)(st)
c__case_49_case__25 x1 x2 x11 x15 x18 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_49_case__25")(x)



c__case_55_case__26 x1 x2 x11 x15 x17 x18 x19 x20@((Curry.Module.Prelude.:<) x21 x22) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_54(x2)(x11)(x15)(x17)(x18)(x19)(x21)(x22)(x1)(st))(st)
c__case_55_case__26 x1 x2 x11 x15 x17 x18 x19 x20@Curry.Module.Prelude.List st = let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c__case_49(x2)(x11)(x15)(x17)(x18)(Curry.Module.OraclePrelude.op_61_61(x17)(Curry.Module.Prelude.C_Char('\''))(x1)(st))(x23)(st))(st)
c__case_55_case__26 x1 x2 x11 x15 x17 x18 x19 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_55_case__26(x1)(x2)(x11)(x15)(x17)(x18)(x19)(x)(st))(i)(xs)(st)
c__case_55_case__26 x1 x2 x11 x15 x17 x18 x19 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_55_case__26")(x)



c__case_56_case__27 x1 x2 x11 x15 x17 x18 x19 x20 x21@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Letter((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))))(Curry.Module.OracleCurryStringClassifier.c_stateScan(x2)(Curry.Module.CurryStringClassifier.C_Code(x11))(x11)(x20)(x1)(st)))(st)
c__case_56_case__27 x1 x2 x11 x15 x17 x18 x19 x20 x21@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_55(x2)(x11)(x15)(x17)(x18)(x19)(x20)(x1)(st))(st)
c__case_56_case__27 x1 x2 x11 x15 x17 x18 x19 x20 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_56_case__27(x1)(x2)(x11)(x15)(x17)(x18)(x19)(x20)(x)(st))(i)(xs)(st)
c__case_56_case__27 x1 x2 x11 x15 x17 x18 x19 x20 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_56_case__27")(x)



c__case_48_case__28 x1 x2 x11 x15 x18@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Letter((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c_stateScan(x2)(Curry.Module.CurryStringClassifier.C_Code(x11))(x11)(Curry.Module.Prelude.List)(x1)(st)))(st)
c__case_48_case__28 x1 x2 x11 x15 x18@Curry.Module.Prelude.C_False st = let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_error))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))(x19)(st))(x20)(st))(st)
c__case_48_case__28 x1 x2 x11 x15 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_48_case__28(x1)(x2)(x11)(x15)(x)(st))(i)(xs)(st)
c__case_48_case__28 x1 x2 x11 x15 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_48_case__28")(x)



c__case_57_case__29 x1 x2 x11 x15 x17 x18@((Curry.Module.Prelude.:<) x19 x20) st = let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c__case_56(x2)(x11)(x15)(x17)(x18)(x19)(x20)(Curry.Module.OraclePrelude.op_61_61(x19)(Curry.Module.Prelude.C_Char('\''))(x1)(st))(x21)(st))(st)
c__case_57_case__29 x1 x2 x11 x15 x17 x18@Curry.Module.Prelude.List st = let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c__case_48(x2)(x11)(x15)(x17)(Curry.Module.OraclePrelude.op_61_61(x17)(Curry.Module.Prelude.C_Char('\''))(x1)(st))(x22)(st))(st)
c__case_57_case__29 x1 x2 x11 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_57_case__29(x1)(x2)(x11)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_57_case__29 x1 x2 x11 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_57_case__29")(x)



c__case_58_case__30 x1 x2 x11 x15 x16@((Curry.Module.Prelude.:<) x17 x18) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_57(x2)(x11)(x15)(x17)(x18)(x1)(st))(st)
c__case_58_case__30 x1 x2 x11 x15 x16@Curry.Module.Prelude.List st = let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_error))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))(x19)(st))(x20)(st))(st)
c__case_58_case__30 x1 x2 x11 x15 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_58_case__30(x1)(x2)(x11)(x15)(x)(st))(i)(xs)(st)
c__case_58_case__30 x1 x2 x11 x15 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_58_case__30")(x)



c__case_46_case__31 x1 x2 x11 x15 x26 x27@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Letter((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c_stateScan(x2)(Curry.Module.CurryStringClassifier.C_Code(x11))(x11)(x26)(x1)(st)))(st)
c__case_46_case__31 x1 x2 x11 x15 x26 x27@Curry.Module.Prelude.C_False st = let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_error))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))(x28)(st))(x29)(st))(st)
c__case_46_case__31 x1 x2 x11 x15 x26 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_46_case__31(x1)(x2)(x11)(x15)(x26)(x)(st))(i)(xs)(st)
c__case_46_case__31 x1 x2 x11 x15 x26 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_46_case__31")(x)



c__case_47_case__32 x1 x2 x11 x15 x16@((Curry.Module.Prelude.:<) x25 x26) st = let {x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x27)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c__case_46(x2)(x11)(x15)(x25)(x26)(Curry.Module.OraclePrelude.op_61_61(x25)(Curry.Module.Prelude.C_Char('\''))(x1)(st))(x27)(st))(st)
c__case_47_case__32 x1 x2 x11 x15 x16@Curry.Module.Prelude.List st = let {x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_error))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))(x28)(st))(x29)(st))(st)
c__case_47_case__32 x1 x2 x11 x15 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_47_case__32(x1)(x2)(x11)(x15)(x)(st))(i)(xs)(st)
c__case_47_case__32 x1 x2 x11 x15 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_47_case__32")(x)



c__case_59_case__33 x1 x2 x11 x15 x16 x17@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_58(x2)(x11)(x15)(x16)(x1)(st))(st)
c__case_59_case__33 x1 x2 x11 x15 x16 x17@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_47(x2)(x11)(x15)(x16)(x1)(st))(st)
c__case_59_case__33 x1 x2 x11 x15 x16 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_59_case__33(x1)(x2)(x11)(x15)(x16)(x)(st))(i)(xs)(st)
c__case_59_case__33 x1 x2 x11 x15 x16 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_59_case__33")(x)



c__case_60_case__34 x1 x2 x11 x10@((Curry.Module.Prelude.:<) x15 x16) st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c__case_59(x2)(x11)(x15)(x16)(Curry.Module.OraclePrelude.op_61_61(x15)(Curry.Module.Prelude.C_Char('\\'))(x1)(st))(x17)(st))(st)
c__case_60_case__34 x1 x2 x11 x10@Curry.Module.Prelude.List st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_error))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))(x18)(st))(x19)(st))(st)
c__case_60_case__34 x1 x2 x11 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_60_case__34(x1)(x2)(x11)(x)(st))(i)(xs)(st)
c__case_60_case__34 x1 x2 x11 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_60_case__34")(x)



c__case_44_case__35 x1 x2 x4 x6 x8 x9 x10 x11 x12@Curry.Module.Prelude.C_True st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_38_62(Curry.Module.OraclePrelude.op_61_58_61(x4)((Curry.Module.Prelude.:<)(x6)(x11))(x1)(st))(Curry.Module.OracleCurryStringClassifier.c_stateScan(x2)(Curry.Module.CurryStringClassifier.C_Code(x8))(x11)((Curry.Module.Prelude.:<)(x9)(x10))(x13)(st))(x14)(st))(st)
c__case_44_case__35 x1 x2 x4 x6 x8 x9 x10 x11 x12@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_44_case__35 x1 x2 x4 x6 x8 x9 x10 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_44_case__35(x1)(x2)(x4)(x6)(x8)(x9)(x10)(x11)(x)(st))(i)(xs)(st)
c__case_44_case__35 x1 x2 x4 x6 x8 x9 x10 x11 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_44_case__35")(x)



c__case_45_case__36 x1 x2 x4 x6 x8 x9 x10 x11 x12@Curry.Module.Prelude.C_True st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_38_62(Curry.Module.OraclePrelude.op_61_58_61(x4)((Curry.Module.Prelude.:<)(x6)(x11))(x1)(st))(Curry.Module.OracleCurryStringClassifier.c_stateScan(Curry.Module.OraclePrelude.op_43(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x13)(st))(Curry.Module.CurryStringClassifier.C_Code(x8))(x11)((Curry.Module.Prelude.:<)(x9)(x10))(x14)(st))(x15)(st))(st)
c__case_45_case__36 x1 x2 x4 x6 x8 x9 x10 x11 x12@Curry.Module.Prelude.C_False st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c__case_44(x2)(x4)(x6)(x8)(x9)(x10)(x11)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x16)(st))(st)
c__case_45_case__36 x1 x2 x4 x6 x8 x9 x10 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_45_case__36(x1)(x2)(x4)(x6)(x8)(x9)(x10)(x11)(x)(st))(i)(xs)(st)
c__case_45_case__36 x1 x2 x4 x6 x8 x9 x10 x11 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_45_case__36")(x)



c__case_61_case__37 x1 x2 x4 x6 x8 x9 x10 x11 x12@Curry.Module.Prelude.C_True st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_38_62(Curry.Module.OraclePrelude.op_61_58_61(x4)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(x1)(st))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCurryStringClassifier.c_maybeCode(x8)))))(Curry.Module.OracleCurryStringClassifier.c__case_60(x2)(x11)(x10)(x13)(st))(x14)(st))(x15)(st))(st)
c__case_61_case__37 x1 x2 x4 x6 x8 x9 x10 x11 x12@Curry.Module.Prelude.C_False st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c__case_45(x2)(x4)(x6)(x8)(x9)(x10)(x11)(Curry.Module.OraclePrelude.op_61_61(x6)(Curry.Module.Prelude.C_Char('\n'))(x1)(st))(x16)(st))(st)
c__case_61_case__37 x1 x2 x4 x6 x8 x9 x10 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_61_case__37(x1)(x2)(x4)(x6)(x8)(x9)(x10)(x11)(x)(st))(i)(xs)(st)
c__case_61_case__37 x1 x2 x4 x6 x8 x9 x10 x11 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_61_case__37")(x)



c__case_62_case__38 x1 x2 x4 x6 x8 x9 x10 x11 x12@Curry.Module.Prelude.C_True st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_38_62(Curry.Module.OraclePrelude.op_61_58_61(x4)(Curry.Module.Prelude.List)(x1)(st))(Curry.Module.OracleCurryStringClassifier.c_maybeCode(x8)(Curry.Module.OracleCurryStringClassifier.c_stateScan(x2)(Curry.Module.CurryStringClassifier.C_Meta(x11))(x11)(x10)(x13)(st))(x14)(st))(x15)(st))(st)
c__case_62_case__38 x1 x2 x4 x6 x8 x9 x10 x11 x12@Curry.Module.Prelude.C_False st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))))))))(Curry.Module.OracleCurryStringClassifier.c__case_61(x2)(x4)(x6)(x8)(x9)(x10)(x11)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x9)(Curry.Module.Prelude.C_Char('\''))(x1)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_elem(x6)(x16)(st))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleCurryStringClassifier.c_infixIDs(x17)(st))(Curry.Module.OracleCurryStringClassifier.c_delimiters(x18)(st))(x19)(st))(x20)(st))(x21)(st))(x22)(st))(st)
c__case_62_case__38 x1 x2 x4 x6 x8 x9 x10 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_62_case__38(x1)(x2)(x4)(x6)(x8)(x9)(x10)(x11)(x)(st))(i)(xs)(st)
c__case_62_case__38 x1 x2 x4 x6 x8 x9 x10 x11 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_62_case__38")(x)



c__case_63_case__39 x1 x2 x4 x6 x8 x9 x10 x11 x12@Curry.Module.Prelude.C_True st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_38_62(Curry.Module.OraclePrelude.op_61_58_61(x4)(Curry.Module.Prelude.List)(x1)(st))(Curry.Module.OracleCurryStringClassifier.c_maybeCode(x8)(Curry.Module.OracleCurryStringClassifier.c_stateScan(x2)(Curry.Module.CurryStringClassifier.C_BigComment(x11))(x11)(x10)(x13)(st))(x14)(st))(x15)(st))(st)
c__case_63_case__39 x1 x2 x4 x6 x8 x9 x10 x11 x12@Curry.Module.Prelude.C_False st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))))(Curry.Module.OracleCurryStringClassifier.c__case_62(x2)(x4)(x6)(x8)(x9)(x10)(x11)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x6)(Curry.Module.Prelude.C_Char('{'))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x9)(Curry.Module.Prelude.C_Char('+'))(x16)(st))(x17)(st))(x18)(st))(st)
c__case_63_case__39 x1 x2 x4 x6 x8 x9 x10 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_63_case__39(x1)(x2)(x4)(x6)(x8)(x9)(x10)(x11)(x)(st))(i)(xs)(st)
c__case_63_case__39 x1 x2 x4 x6 x8 x9 x10 x11 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_63_case__39")(x)



c__case_64_case__40 x1 x2 x4 x6 x8 x9 x10 x11 x15@Curry.Module.Prelude.C_True st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(let {x12 = Curry.Module.OraclePrelude.c_span(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_47_61))(st))(Curry.Module.Prelude.C_Char('\n'))))))(x10)(x1)(st)} in let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))(let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))(let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_38_62(Curry.Module.OraclePrelude.op_61_58_61(x4)(Curry.Module.Prelude.List)(x18)(st))(Curry.Module.OracleCurryStringClassifier.c_maybeCode(x8)((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_SmallComment(Curry.Module.OracleCurryStringClassifier.c_stateScan'46_'35selFP9'35comment(x12)(x16)(st)))(Curry.Module.OracleCurryStringClassifier.c_stateScan(x2)(Curry.Module.CurryStringClassifier.C_Code(x11))(x11)(Curry.Module.OracleCurryStringClassifier.c_stateScan'46_'35selFP10'35rest(x12)(x17)(st))(x19)(st)))(x20)(st))(x21)(st))(st))(st))(st))(st)
c__case_64_case__40 x1 x2 x4 x6 x8 x9 x10 x11 x15@Curry.Module.Prelude.C_False st = let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List))))(Curry.Module.OracleCurryStringClassifier.c__case_63(x2)(x4)(x6)(x8)(x9)(x10)(x11)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x6)(Curry.Module.Prelude.C_Char('{'))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x9)(Curry.Module.Prelude.C_Char('-'))(x22)(st))(x23)(st))(x24)(st))(st)
c__case_64_case__40 x1 x2 x4 x6 x8 x9 x10 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_64_case__40(x1)(x2)(x4)(x6)(x8)(x9)(x10)(x11)(x)(st))(i)(xs)(st)
c__case_64_case__40 x1 x2 x4 x6 x8 x9 x10 x11 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_64_case__40")(x)



c__case_65_case__41 x1 x2 x4 x6 x8 x9 x10 x11 x12@Curry.Module.Prelude.C_True st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_38_62(Curry.Module.OraclePrelude.op_61_58_61(x4)(Curry.Module.Prelude.List)(x1)(st))(Curry.Module.OracleCurryStringClassifier.c_maybeCode(x8)(Curry.Module.OracleCurryStringClassifier.c_stateScan(x2)(Curry.Module.CurryStringClassifier.C_Text(x11))(x11)((Curry.Module.Prelude.:<)(x9)(x10))(x13)(st))(x14)(st))(x15)(st))(st)
c__case_65_case__41 x1 x2 x4 x6 x8 x9 x10 x11 x12@Curry.Module.Prelude.C_False st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))))(Curry.Module.OracleCurryStringClassifier.c__case_64(x2)(x4)(x6)(x8)(x9)(x10)(x11)(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x6)(Curry.Module.Prelude.C_Char('-'))(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x9)(Curry.Module.Prelude.C_Char('-'))(x16)(st))(x17)(st))(x18)(st))(st)
c__case_65_case__41 x1 x2 x4 x6 x8 x9 x10 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_65_case__41(x1)(x2)(x4)(x6)(x8)(x9)(x10)(x11)(x)(st))(i)(xs)(st)
c__case_65_case__41 x1 x2 x4 x6 x8 x9 x10 x11 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_65_case__41")(x)



c__case_66_case__42 x1 x2 x4 x6 x8 x7@Curry.Module.Prelude.List st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_cond(Curry.Module.OraclePrelude.op_61_58_61(x4)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(x1)(st))(Curry.Module.OracleCurryStringClassifier.c_maybeCode(x8)(Curry.Module.Prelude.List)(x12)(st))(x13)(st))(st)
c__case_66_case__42 x1 x2 x4 x6 x8 x7@((Curry.Module.Prelude.:<) x9 x10) st = let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c__case_65(x2)(x4)(x6)(x8)(x9)(x10)(Curry.Module.Oracle.c_unknown(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x6)(Curry.Module.Prelude.C_Char('\"'))(x14)(st))(x15)(st))(st)
c__case_66_case__42 x1 x2 x4 x6 x8 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_66_case__42(x1)(x2)(x4)(x6)(x8)(x)(st))(i)(xs)(st)
c__case_66_case__42 x1 x2 x4 x6 x8 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_66_case__42")(x)



c__case_42_case__43 x1 x27 x28@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Text(x27))(Curry.Module.Prelude.List))(st)
c__case_42_case__43 x1 x27 x28@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))(x1)(st))(st)
c__case_42_case__43 x1 x27 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_42_case__43(x1)(x27)(x)(st))(i)(xs)(st)
c__case_42_case__43 x1 x27 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_42_case__43")(x)



c__case_38_case__44 x1 x2 x4 x6 x27 x28 x29 x30 x31@Curry.Module.Prelude.C_True st = let {x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_38_62(Curry.Module.OraclePrelude.op_61_58_61(x4)((Curry.Module.Prelude.:<)(x6)(x30))(x1)(st))(Curry.Module.OracleCurryStringClassifier.c_stateScan(x2)(Curry.Module.CurryStringClassifier.C_Text(x27))(x30)((Curry.Module.Prelude.:<)(x28)(x29))(x32)(st))(x33)(st))(st)
c__case_38_case__44 x1 x2 x4 x6 x27 x28 x29 x30 x31@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_38_case__44 x1 x2 x4 x6 x27 x28 x29 x30 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_38_case__44(x1)(x2)(x4)(x6)(x27)(x28)(x29)(x30)(x)(st))(i)(xs)(st)
c__case_38_case__44 x1 x2 x4 x6 x27 x28 x29 x30 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_38_case__44")(x)



c__case_39_case__45 x1 x2 x4 x6 x27 x28 x29 x30 x31@Curry.Module.Prelude.C_True st = let {x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)((Curry.Module.Prelude.:<)(x34)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_36(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_error))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))((Curry.Module.Prelude.:<)(x6)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))))))))))))))(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))(x32)(st)))(x33)(st))(x34)(st))(st)
c__case_39_case__45 x1 x2 x4 x6 x27 x28 x29 x30 x31@Curry.Module.Prelude.C_False st = let {x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x35)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c__case_38(x2)(x4)(x6)(x27)(x28)(x29)(x30)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x35)(st))(st)
c__case_39_case__45 x1 x2 x4 x6 x27 x28 x29 x30 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_39_case__45(x1)(x2)(x4)(x6)(x27)(x28)(x29)(x30)(x)(st))(i)(xs)(st)
c__case_39_case__45 x1 x2 x4 x6 x27 x28 x29 x30 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_39_case__45")(x)



c__case_40_case__46 x1 x2 x4 x6 x27 x28 x29 x30 x31@Curry.Module.Prelude.C_True st = let {x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_38_62(Curry.Module.OraclePrelude.op_61_58_61(x4)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x28)(x30)))(x1)(st))(Curry.Module.OracleCurryStringClassifier.c_stateScan(x2)(Curry.Module.CurryStringClassifier.C_Text(x27))(x30)(x29)(x32)(st))(x33)(st))(st)
c__case_40_case__46 x1 x2 x4 x6 x27 x28 x29 x30 x31@Curry.Module.Prelude.C_False st = let {x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x34)((Curry.Module.Prelude.:<)(x35)((Curry.Module.Prelude.:<)(x36)(Curry.Module.Prelude.List))))(Curry.Module.OracleCurryStringClassifier.c__case_39(x2)(x4)(x6)(x27)(x28)(x29)(x30)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_elem(x6)(x1)(st))(Curry.Module.OracleCurryStringClassifier.c_toBeEscaped(x34)(st))(x35)(st))(x36)(st))(st)
c__case_40_case__46 x1 x2 x4 x6 x27 x28 x29 x30 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_40_case__46(x1)(x2)(x4)(x6)(x27)(x28)(x29)(x30)(x)(st))(i)(xs)(st)
c__case_40_case__46 x1 x2 x4 x6 x27 x28 x29 x30 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_40_case__46")(x)



c__case_41_case__47 x1 x2 x4 x6 x27 x28 x29 x30 x31@Curry.Module.Prelude.C_True st = let {x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_38_62(Curry.Module.OraclePrelude.op_61_58_61(x4)(Curry.Module.Prelude.List)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Text(x27))(Curry.Module.OracleCurryStringClassifier.c_stateScan(x2)(Curry.Module.CurryStringClassifier.C_Code(x30))(x30)((Curry.Module.Prelude.:<)(x28)(x29))(x32)(st)))(x33)(st))(st)
c__case_41_case__47 x1 x2 x4 x6 x27 x28 x29 x30 x31@Curry.Module.Prelude.C_False st = let {x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x34)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c__case_40(x2)(x4)(x6)(x27)(x28)(x29)(x30)(Curry.Module.OraclePrelude.op_61_61(x6)(Curry.Module.Prelude.C_Char('\\'))(x1)(st))(x34)(st))(st)
c__case_41_case__47 x1 x2 x4 x6 x27 x28 x29 x30 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_41_case__47(x1)(x2)(x4)(x6)(x27)(x28)(x29)(x30)(x)(st))(i)(xs)(st)
c__case_41_case__47 x1 x2 x4 x6 x27 x28 x29 x30 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_41_case__47")(x)



c__case_43_case__48 x1 x2 x4 x6 x27 x7@Curry.Module.Prelude.List st = let {x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x31)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.c_cond(Curry.Module.OraclePrelude.op_61_58_61(x4)(Curry.Module.Prelude.List)(x1)(st))(Curry.Module.OracleCurryStringClassifier.c__case_42(x6)(x27)(Curry.Module.OraclePrelude.op_61_61(x6)(Curry.Module.Prelude.C_Char('\"'))(x31)(st))(x32)(st))(x33)(st))(st)
c__case_43_case__48 x1 x2 x4 x6 x27 x7@((Curry.Module.Prelude.:<) x28 x29) st = let {x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x34)((Curry.Module.Prelude.:<)(x35)(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c__case_41(x2)(x4)(x6)(x27)(x28)(x29)(Curry.Module.Oracle.c_unknown(x1)(st))(Curry.Module.OraclePrelude.op_61_61(x6)(Curry.Module.Prelude.C_Char('\"'))(x34)(st))(x35)(st))(st)
c__case_43_case__48 x1 x2 x4 x6 x27 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_43_case__48(x1)(x2)(x4)(x6)(x27)(x)(st))(i)(xs)(st)
c__case_43_case__48 x1 x2 x4 x6 x27 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_43_case__48")(x)



c__case_34_case__49 x1 x2 x4 x6 x31 x32 x33 x34 x35@Curry.Module.Prelude.C_True st = let {x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x36)((Curry.Module.Prelude.:<)(x37)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_38_62(Curry.Module.OraclePrelude.op_61_58_61(x4)((Curry.Module.Prelude.:<)(x6)(x34))(x1)(st))(Curry.Module.OracleCurryStringClassifier.c_stateScan(x2)(Curry.Module.CurryStringClassifier.C_BigComment(x31))(x34)((Curry.Module.Prelude.:<)(x32)(x33))(x36)(st))(x37)(st))(st)
c__case_34_case__49 x1 x2 x4 x6 x31 x32 x33 x34 x35@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_34_case__49 x1 x2 x4 x6 x31 x32 x33 x34 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_34_case__49(x1)(x2)(x4)(x6)(x31)(x32)(x33)(x34)(x)(st))(i)(xs)(st)
c__case_34_case__49 x1 x2 x4 x6 x31 x32 x33 x34 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_34_case__49")(x)



c__case_35_case__50 x1 x2 x4 x6 x31 x32 x33 x34 x35@Curry.Module.Prelude.C_True st = let {x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x36)((Curry.Module.Prelude.:<)(x37)((Curry.Module.Prelude.:<)(x38)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_38_62(Curry.Module.OraclePrelude.op_61_58_61(x4)((Curry.Module.Prelude.:<)(x6)(x34))(x1)(st))(Curry.Module.OracleCurryStringClassifier.c_stateScan(Curry.Module.OraclePrelude.op_43(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x36)(st))(Curry.Module.CurryStringClassifier.C_BigComment(x31))(x34)((Curry.Module.Prelude.:<)(x32)(x33))(x37)(st))(x38)(st))(st)
c__case_35_case__50 x1 x2 x4 x6 x31 x32 x33 x34 x35@Curry.Module.Prelude.C_False st = let {x39 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x39)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c__case_34(x2)(x4)(x6)(x31)(x32)(x33)(x34)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x39)(st))(st)
c__case_35_case__50 x1 x2 x4 x6 x31 x32 x33 x34 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_35_case__50(x1)(x2)(x4)(x6)(x31)(x32)(x33)(x34)(x)(st))(i)(xs)(st)
c__case_35_case__50 x1 x2 x4 x6 x31 x32 x33 x34 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_35_case__50")(x)



c__case_36_case__51 x1 x2 x4 x6 x31 x32 x33 x34 x35@Curry.Module.Prelude.C_True st = let {x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x36)((Curry.Module.Prelude.:<)(x37)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_38_62(Curry.Module.OraclePrelude.op_61_58_61(x4)(Curry.Module.Prelude.List)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_BigComment(x31))(Curry.Module.OracleCurryStringClassifier.c_stateScan(x2)(Curry.Module.CurryStringClassifier.C_Code(x34))(x34)(x33)(x36)(st)))(x37)(st))(st)
c__case_36_case__51 x1 x2 x4 x6 x31 x32 x33 x34 x35@Curry.Module.Prelude.C_False st = let {x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x38)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c__case_35(x2)(x4)(x6)(x31)(x32)(x33)(x34)(Curry.Module.OraclePrelude.op_61_61(x6)(Curry.Module.Prelude.C_Char('\n'))(x1)(st))(x38)(st))(st)
c__case_36_case__51 x1 x2 x4 x6 x31 x32 x33 x34 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_36_case__51(x1)(x2)(x4)(x6)(x31)(x32)(x33)(x34)(x)(st))(i)(xs)(st)
c__case_36_case__51 x1 x2 x4 x6 x31 x32 x33 x34 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_36_case__51")(x)



c__case_37_case__52 x1 x2 x4 x6 x31 x7@Curry.Module.Prelude.List st = let {x35 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x36 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x35)((Curry.Module.Prelude.:<)(x36)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_cond(Curry.Module.OraclePrelude.op_61_58_61(x4)(Curry.Module.Prelude.List)(x1)(st))(Curry.Module.OraclePrelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('}'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))(x35)(st))(x36)(st))(st)
c__case_37_case__52 x1 x2 x4 x6 x31 x7@((Curry.Module.Prelude.:<) x32 x33) st = let {x37 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x38 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x39 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x40 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x37)((Curry.Module.Prelude.:<)(x38)((Curry.Module.Prelude.:<)(x39)((Curry.Module.Prelude.:<)(x40)(Curry.Module.Prelude.List)))))(Curry.Module.OracleCurryStringClassifier.c__case_36(x2)(x4)(x6)(x31)(x32)(x33)(Curry.Module.Oracle.c_unknown(x1)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x6)(Curry.Module.Prelude.C_Char('-'))(x37)(st))(Curry.Module.OraclePrelude.op_61_61(x32)(Curry.Module.Prelude.C_Char('}'))(x38)(st))(x39)(st))(x40)(st))(st)
c__case_37_case__52 x1 x2 x4 x6 x31 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_37_case__52(x1)(x2)(x4)(x6)(x31)(x)(st))(i)(xs)(st)
c__case_37_case__52 x1 x2 x4 x6 x31 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_37_case__52")(x)



c__case_30_case__53 x1 x2 x4 x6 x35 x36 x37 x38 x39@Curry.Module.Prelude.C_True st = let {x40 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x41 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x40)((Curry.Module.Prelude.:<)(x41)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_38_62(Curry.Module.OraclePrelude.op_61_58_61(x4)((Curry.Module.Prelude.:<)(x6)(x38))(x1)(st))(Curry.Module.OracleCurryStringClassifier.c_stateScan(x2)(Curry.Module.CurryStringClassifier.C_Meta(x35))(x38)((Curry.Module.Prelude.:<)(x36)(x37))(x40)(st))(x41)(st))(st)
c__case_30_case__53 x1 x2 x4 x6 x35 x36 x37 x38 x39@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_30_case__53 x1 x2 x4 x6 x35 x36 x37 x38 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_30_case__53(x1)(x2)(x4)(x6)(x35)(x36)(x37)(x38)(x)(st))(i)(xs)(st)
c__case_30_case__53 x1 x2 x4 x6 x35 x36 x37 x38 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_30_case__53")(x)



c__case_31_case__54 x1 x2 x4 x6 x35 x36 x37 x38 x39@Curry.Module.Prelude.C_True st = let {x40 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x41 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x42 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x40)((Curry.Module.Prelude.:<)(x41)((Curry.Module.Prelude.:<)(x42)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_38_62(Curry.Module.OraclePrelude.op_61_58_61(x4)((Curry.Module.Prelude.:<)(x6)(x38))(x1)(st))(Curry.Module.OracleCurryStringClassifier.c_stateScan(Curry.Module.OraclePrelude.op_43(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x40)(st))(Curry.Module.CurryStringClassifier.C_Meta(x35))(x38)((Curry.Module.Prelude.:<)(x36)(x37))(x41)(st))(x42)(st))(st)
c__case_31_case__54 x1 x2 x4 x6 x35 x36 x37 x38 x39@Curry.Module.Prelude.C_False st = let {x43 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x43)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c__case_30(x2)(x4)(x6)(x35)(x36)(x37)(x38)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x43)(st))(st)
c__case_31_case__54 x1 x2 x4 x6 x35 x36 x37 x38 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_31_case__54(x1)(x2)(x4)(x6)(x35)(x36)(x37)(x38)(x)(st))(i)(xs)(st)
c__case_31_case__54 x1 x2 x4 x6 x35 x36 x37 x38 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_31_case__54")(x)



c__case_32_case__55 x1 x2 x4 x6 x35 x36 x37 x38 x39@Curry.Module.Prelude.C_True st = let {x40 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x41 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x40)((Curry.Module.Prelude.:<)(x41)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_38_62(Curry.Module.OraclePrelude.op_61_58_61(x4)(Curry.Module.Prelude.List)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Meta(x35))(Curry.Module.OracleCurryStringClassifier.c_stateScan(x2)(Curry.Module.CurryStringClassifier.C_Code(x38))(x38)(x37)(x40)(st)))(x41)(st))(st)
c__case_32_case__55 x1 x2 x4 x6 x35 x36 x37 x38 x39@Curry.Module.Prelude.C_False st = let {x42 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x42)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c__case_31(x2)(x4)(x6)(x35)(x36)(x37)(x38)(Curry.Module.OraclePrelude.op_61_61(x6)(Curry.Module.Prelude.C_Char('\n'))(x1)(st))(x42)(st))(st)
c__case_32_case__55 x1 x2 x4 x6 x35 x36 x37 x38 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_32_case__55(x1)(x2)(x4)(x6)(x35)(x36)(x37)(x38)(x)(st))(i)(xs)(st)
c__case_32_case__55 x1 x2 x4 x6 x35 x36 x37 x38 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_32_case__55")(x)



c__case_33_case__56 x1 x2 x4 x6 x35 x7@Curry.Module.Prelude.List st = let {x39 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x40 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x39)((Curry.Module.Prelude.:<)(x40)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_cond(Curry.Module.OraclePrelude.op_61_58_61(x4)(Curry.Module.Prelude.List)(x1)(st))(Curry.Module.OraclePrelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('+'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('}'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))(x39)(st))(x40)(st))(st)
c__case_33_case__56 x1 x2 x4 x6 x35 x7@((Curry.Module.Prelude.:<) x36 x37) st = let {x41 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x42 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x43 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x44 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x41)((Curry.Module.Prelude.:<)(x42)((Curry.Module.Prelude.:<)(x43)((Curry.Module.Prelude.:<)(x44)(Curry.Module.Prelude.List)))))(Curry.Module.OracleCurryStringClassifier.c__case_32(x2)(x4)(x6)(x35)(x36)(x37)(Curry.Module.Oracle.c_unknown(x1)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x6)(Curry.Module.Prelude.C_Char('+'))(x41)(st))(Curry.Module.OraclePrelude.op_61_61(x36)(Curry.Module.Prelude.C_Char('}'))(x42)(st))(x43)(st))(x44)(st))(st)
c__case_33_case__56 x1 x2 x4 x6 x35 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_33_case__56(x1)(x2)(x4)(x6)(x35)(x)(st))(i)(xs)(st)
c__case_33_case__56 x1 x2 x4 x6 x35 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_33_case__56")(x)



c__case_67_case__57 x1 x2 x4 x6 x7 x3@(Curry.Module.CurryStringClassifier.C_Code x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_66(x2)(x4)(x6)(x8)(x7)(x1)(st))(st)
c__case_67_case__57 x1 x2 x4 x6 x7 x3@(Curry.Module.CurryStringClassifier.C_Text x27) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_43(x2)(x4)(x6)(x27)(x7)(x1)(st))(st)
c__case_67_case__57 x1 x2 x4 x6 x7 x3@(Curry.Module.CurryStringClassifier.C_BigComment x31) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_37(x2)(x4)(x6)(x31)(x7)(x1)(st))(st)
c__case_67_case__57 x1 x2 x4 x6 x7 x3@(Curry.Module.CurryStringClassifier.C_Meta x35) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_33(x2)(x4)(x6)(x35)(x7)(x1)(st))(st)
c__case_67_case__57 x1 x2 x4 x6 x7 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_67_case__57(x1)(x2)(x4)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_67_case__57 x1 x2 x4 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_67_case__57")(x)



c__case_68_case__58 x1 x2 x3 x4 x5@Curry.Module.Prelude.List st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_cond(Curry.Module.OraclePrelude.op_61_58_61(x4)(Curry.Module.Prelude.List)(x1)(st))((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(x8)(st))(st)
c__case_68_case__58 x1 x2 x3 x4 x5@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_67(x2)(x4)(x6)(x7)(x3)(x1)(st))(st)
c__case_68_case__58 x1 x2 x3 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_68_case__58(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_68_case__58 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_68_case__58")(x)



c__case_29_case__59 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_29_case__59 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_29_case__59(x1)(x)(st))(i)(xs)(st)
c__case_29_case__59 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_29_case__59")(x)



c__case_28_case__60 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_28_case__60 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_28_case__60(x1)(x)(st))(i)(xs)(st)
c__case_28_case__60 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_28_case__60")(x)



c__case_23_case__61 x1 x2 x5 x8@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c_modHead(x2)(x5)(x1)(st))(st)
c__case_23_case__61 x1 x2 x5 x8@((Curry.Module.Prelude.:<) x9 x10) st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c_modHead(Curry.Module.OraclePrelude.op_46(x2)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc((Curry.Module.Prelude.:<)(x9)))))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Code(x10))(x5))(x11)(st))(st)
c__case_23_case__61 x1 x2 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_23_case__61(x1)(x2)(x5)(x)(st))(i)(xs)(st)
c__case_23_case__61 x1 x2 x5 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_23_case__61")(x)



c__case_21_case__62 x1 x2 x5 x6 x11 x12 x13@Curry.Module.Prelude.C_True st = let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_ModuleHead(Curry.Module.Oracle.c_apply(x2)((Curry.Module.Prelude.:<)(x11)(x12))(x1)(st)))(Curry.Module.OracleCurryStringClassifier.c_modHeadInLine(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_id))))(x5)(x14)(st)))(st)
c__case_21_case__62 x1 x2 x5 x6 x11 x12 x13@Curry.Module.Prelude.C_False st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c_maybeMo(Curry.Module.Oracle.c_apply(x2)(Curry.Module.Prelude.List)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Code(x6))(x5))(x15)(st))(st)
c__case_21_case__62 x1 x2 x5 x6 x11 x12 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_21_case__62(x1)(x2)(x5)(x6)(x11)(x12)(x)(st))(i)(xs)(st)
c__case_21_case__62 x1 x2 x5 x6 x11 x12 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_21_case__62")(x)



c__case_20_case__63 x1 x2 x5 x6 x11 x12 x13 x14 x15@Curry.Module.Prelude.C_True st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c_modHead(Curry.Module.OraclePrelude.op_46(x2)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(x11)(x12))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc((Curry.Module.Prelude.:<)(x13)))))(x1)(st))(x16)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Code(x14))(x5))(x17)(st))(st)
c__case_20_case__63 x1 x2 x5 x6 x11 x12 x13 x14 x15@Curry.Module.Prelude.C_False st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c_maybeMo(Curry.Module.Oracle.c_apply(x2)(Curry.Module.Prelude.List)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Code(x6))(x5))(x18)(st))(st)
c__case_20_case__63 x1 x2 x5 x6 x11 x12 x13 x14 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_20_case__63(x1)(x2)(x5)(x6)(x11)(x12)(x13)(x14)(x)(st))(i)(xs)(st)
c__case_20_case__63 x1 x2 x5 x6 x11 x12 x13 x14 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_20_case__63")(x)



c__case_22_case__64 x1 x2 x5 x6 x11 x12 x8@Curry.Module.Prelude.List st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))))(Curry.Module.OracleCurryStringClassifier.c__case_21(x2)(x5)(x6)(x11)(x12)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_any(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCurryStringClassifier.c_lineBeginsWith((Curry.Module.Prelude.:<)(x11)(x12))))))(x1)(st))(Curry.Module.OracleCurryStringClassifier.c_headers(x15)(st))(x16)(st))(x17)(st))(st)
c__case_22_case__64 x1 x2 x5 x6 x11 x12 x8@((Curry.Module.Prelude.:<) x13 x14) st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))))(Curry.Module.OracleCurryStringClassifier.c__case_20(x2)(x5)(x6)(x11)(x12)(x13)(x14)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_any(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCurryStringClassifier.c_lineBeginsWith((Curry.Module.Prelude.:<)(x11)(x12))))))(x1)(st))(Curry.Module.OracleCurryStringClassifier.c_headers(x18)(st))(x19)(st))(x20)(st))(st)
c__case_22_case__64 x1 x2 x5 x6 x11 x12 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_22_case__64(x1)(x2)(x5)(x6)(x11)(x12)(x)(st))(i)(xs)(st)
c__case_22_case__64 x1 x2 x5 x6 x11 x12 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_22_case__64")(x)



c__case_24_case__65 x1 x2 x5 x6 x8 x7@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_23(x2)(x5)(x8)(x1)(st))(st)
c__case_24_case__65 x1 x2 x5 x6 x8 x7@((Curry.Module.Prelude.:<) x11 x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_22(x2)(x5)(x6)(x11)(x12)(x8)(x1)(st))(st)
c__case_24_case__65 x1 x2 x5 x6 x8 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_24_case__65(x1)(x2)(x5)(x6)(x8)(x)(st))(i)(xs)(st)
c__case_24_case__65 x1 x2 x5 x6 x8 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_24_case__65")(x)



c__case_25_case__66 x1 x2 x5 x6 x9@(Curry.Module.Prelude.T2 x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_24(x2)(x5)(x6)(x8)(x7)(x1)(st))(st)
c__case_25_case__66 x1 x2 x5 x6 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_25_case__66(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_25_case__66 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_25_case__66")(x)



c__case_26_case__67 x1 x2 x5 x4@(Curry.Module.CurryStringClassifier.C_Code x6) st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c__case_25(x2)(x5)(x6)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_break(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCurryStringClassifier.c_modHead'46_'35lambda11))))(x1)(st))(x6)(x18)(st))(x19)(st))(st)
c__case_26_case__67 x1 x2 x5 x4@(Curry.Module.CurryStringClassifier.C_BigComment x15) st = let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c_maybeMo(Curry.Module.Oracle.c_apply(x2)(Curry.Module.Prelude.List)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_BigComment(x15))(Curry.Module.OracleCurryStringClassifier.c_modHead(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_id))))(x5)(x20)(st)))(x21)(st))(st)
c__case_26_case__67 x1 x2 x5 x4@(Curry.Module.CurryStringClassifier.C_SmallComment x16) st = let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x22)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c_maybeMo(Curry.Module.Oracle.c_apply(x2)(Curry.Module.Prelude.List)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_SmallComment(x16))(Curry.Module.OracleCurryStringClassifier.c_modHead(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_id))))(x5)(x22)(st)))(x23)(st))(st)
c__case_26_case__67 x1 x2 x5 x4@(Curry.Module.CurryStringClassifier.C_Meta x17) st = let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c_maybeMo(Curry.Module.Oracle.c_apply(x2)(Curry.Module.Prelude.List)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Meta(x17))(x5))(x24)(st))(st)
c__case_26_case__67 x1 x2 x5 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_26_case__67(x1)(x2)(x5)(x)(st))(i)(xs)(st)
c__case_26_case__67 x1 x2 x5 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_26_case__67")(x)



c__case_27_case__68 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_26(x2)(x5)(x4)(x1)(st))(st)
c__case_27_case__68 x1 x2 x3@Curry.Module.Prelude.List st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c_maybeMo(Curry.Module.Oracle.c_apply(x2)(Curry.Module.Prelude.List)(x1)(st))(Curry.Module.Prelude.List)(x6)(st))(st)
c__case_27_case__68 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_27_case__68(x1)(x2)(x)(st))(i)(xs)(st)
c__case_27_case__68 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_27_case__68")(x)



c__case_16_case__69 x1 x2 x5 x6 x7 x8@((Curry.Module.Prelude.:<) x9 x10) st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c_modHead(Curry.Module.OraclePrelude.op_46(x2)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.op_43_43(x7)))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc((Curry.Module.Prelude.:<)(x9)))))(x1)(st))(x11)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Code(x10))(x5))(x12)(st))(st)
c__case_16_case__69 x1 x2 x5 x6 x7 x8@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c_modHead(x2)((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Code(x6))(x5))(x1)(st))(st)
c__case_16_case__69 x1 x2 x5 x6 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_16_case__69(x1)(x2)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_16_case__69 x1 x2 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_16_case__69")(x)



c__case_17_case__70 x1 x2 x5 x6 x9@(Curry.Module.Prelude.T2 x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_16(x2)(x5)(x6)(x7)(x8)(x1)(st))(st)
c__case_17_case__70 x1 x2 x5 x6 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_17_case__70(x1)(x2)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_17_case__70 x1 x2 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_17_case__70")(x)



c__case_18_case__71 x1 x2 x5 x4@(Curry.Module.CurryStringClassifier.C_Code x6) st = let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c__case_17(x2)(x5)(x6)(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_break(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleCurryStringClassifier.c_modHeadInLine'46_'35lambda13))))(x1)(st))(x6)(x14)(st))(x15)(st))(st)
c__case_18_case__71 x1 x2 x5 x4@(Curry.Module.CurryStringClassifier.C_BigComment x11) st = let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c_maybeMo(Curry.Module.Oracle.c_apply(x2)(Curry.Module.Prelude.List)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_BigComment(x11))(Curry.Module.OracleCurryStringClassifier.c_modHeadInLine(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_id))))(x5)(x16)(st)))(x17)(st))(st)
c__case_18_case__71 x1 x2 x5 x4@(Curry.Module.CurryStringClassifier.C_SmallComment x12) st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c_maybeMo(Curry.Module.Oracle.c_apply(x2)(Curry.Module.Prelude.List)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_SmallComment(x12))(Curry.Module.OracleCurryStringClassifier.c_modHeadInLine(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_id))))(x5)(x18)(st)))(x19)(st))(st)
c__case_18_case__71 x1 x2 x5 x4@(Curry.Module.CurryStringClassifier.C_Meta x13) st = let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c_maybeMo(Curry.Module.Oracle.c_apply(x2)(Curry.Module.Prelude.List)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Meta(x13))(x5))(x20)(st))(st)
c__case_18_case__71 x1 x2 x5 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_18_case__71(x1)(x2)(x5)(x)(st))(i)(xs)(st)
c__case_18_case__71 x1 x2 x5 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_18_case__71")(x)



c__case_19_case__72 x1 x2 x3@Curry.Module.Prelude.List st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c_maybeMo(Curry.Module.Oracle.c_apply(x2)(Curry.Module.Prelude.List)(x1)(st))(Curry.Module.Prelude.List)(x6)(st))(st)
c__case_19_case__72 x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_18(x2)(x5)(x4)(x1)(st))(st)
c__case_19_case__72 x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_19_case__72(x1)(x2)(x)(st))(i)(xs)(st)
c__case_19_case__72 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_19_case__72")(x)



c__case_14_case__73 x1 x2 x3 x8@Curry.Module.Prelude.C_True st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_61_61(x2)(x3)(x1)(st))(let {x5 = Curry.Module.OraclePrelude.c_splitAt(Curry.Module.OraclePrelude.c_length(x3)(x9)(st))(x2)(x10)(st)} in let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(let {x7 = Curry.Module.OracleCurryStringClassifier.c_lineBeginsWith'46_'35selFP13'35rest(x5)(x12)(st)} in let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x3)(Curry.Module.OracleCurryStringClassifier.c_lineBeginsWith'46_'35selFP12'35s'39(x5)(x11)(st))(x13)(st))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.c_null(x7)(x14)(st))(Curry.Module.OracleCurryStringClassifier.c_isSep(Curry.Module.OraclePrelude.c_head(x7)(x15)(st))(x16)(st))(x17)(st))(x18)(st))(st))(st))(st))(x19)(st))(st)
c__case_14_case__73 x1 x2 x3 x8@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_14_case__73 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_14_case__73(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_14_case__73 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_14_case__73")(x)



c__case_15_case__74 x1 x2 x3 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_False)(st)
c__case_15_case__74 x1 x2 x3 x5@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleCurryStringClassifier.c__case_14(x2)(x3)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x6)(st))(st)
c__case_15_case__74 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_15_case__74(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_15_case__74 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_15_case__74")(x)



c__case_13_case__75 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_13_case__75 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_13_case__75(x1)(x)(st))(i)(xs)(st)
c__case_13_case__75 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_13_case__75")(x)



c__case_12_case__76 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_12_case__76 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_12_case__76(x1)(x)(st))(i)(xs)(st)
c__case_12_case__76 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_12_case__76")(x)



c__case_9_case__77 x1 x3 x5 x4@(Curry.Module.CurryStringClassifier.C_Code x6) st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Code((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(x6)))(x5))(st)
c__case_9_case__77 x1 x3 x5 x4@(Curry.Module.CurryStringClassifier.C_SmallComment x7) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_9_case__77 x1 x3 x5 x4@(Curry.Module.CurryStringClassifier.C_BigComment x8) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_9_case__77 x1 x3 x5 x4@(Curry.Module.CurryStringClassifier.C_Text x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_9_case__77 x1 x3 x5 x4@(Curry.Module.CurryStringClassifier.C_Letter x10) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_9_case__77 x1 x3 x5 x4@(Curry.Module.CurryStringClassifier.C_ModuleHead x11) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_9_case__77 x1 x3 x5 x4@(Curry.Module.CurryStringClassifier.C_Meta x12) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_9_case__77 x1 x3 x5 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_9_case__77(x1)(x3)(x5)(x)(st))(i)(xs)(st)
c__case_9_case__77 x1 x3 x5 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_9_case__77")(x)



c__case_10_case__78 x1 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_9(x3)(x5)(x4)(x1)(st))(st)
c__case_10_case__78 x1 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_10_case__78 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_10_case__78(x1)(x)(st))(i)(xs)(st)
c__case_10_case__78 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_10_case__78")(x)



c__case_11_case__79 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_11_case__79 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_ModuleHead(x2))(Curry.Module.OracleCurryStringClassifier.c__case_10(x3)(x1)(st)))(st)
c__case_11_case__79 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_11_case__79(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_11_case__79 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_11_case__79")(x)



c__case_5_case__80 x1 x4 x5 x7 x6@(Curry.Module.CurryStringClassifier.C_Code x8) st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43(x5)(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_drop(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x8)(x1)(st))(Curry.Module.OracleCurryStringClassifier.c_plainCode(x7)(x15)(st))(x16)(st))(x17)(st))(st)
c__case_5_case__80 x1 x4 x5 x7 x6@(Curry.Module.CurryStringClassifier.C_SmallComment x9) st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x5)(Curry.Module.OracleCurryStringClassifier.c_plainCode(x4)(x1)(st))(x18)(st))(st)
c__case_5_case__80 x1 x4 x5 x7 x6@(Curry.Module.CurryStringClassifier.C_BigComment x10) st = let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x5)(Curry.Module.OracleCurryStringClassifier.c_plainCode(x4)(x1)(st))(x19)(st))(st)
c__case_5_case__80 x1 x4 x5 x7 x6@(Curry.Module.CurryStringClassifier.C_Text x11) st = let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x5)(Curry.Module.OracleCurryStringClassifier.c_plainCode(x4)(x1)(st))(x20)(st))(st)
c__case_5_case__80 x1 x4 x5 x7 x6@(Curry.Module.CurryStringClassifier.C_Letter x12) st = let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x5)(Curry.Module.OracleCurryStringClassifier.c_plainCode(x4)(x1)(st))(x21)(st))(st)
c__case_5_case__80 x1 x4 x5 x7 x6@(Curry.Module.CurryStringClassifier.C_ModuleHead x13) st = let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x5)(Curry.Module.OracleCurryStringClassifier.c_plainCode(x4)(x1)(st))(x22)(st))(st)
c__case_5_case__80 x1 x4 x5 x7 x6@(Curry.Module.CurryStringClassifier.C_Meta x14) st = let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x5)(Curry.Module.OracleCurryStringClassifier.c_plainCode(x4)(x1)(st))(x23)(st))(st)
c__case_5_case__80 x1 x4 x5 x7 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_5_case__80(x1)(x4)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_5_case__80 x1 x4 x5 x7 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_5_case__80")(x)



c__case_6_case__81 x1 x5 x4@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_5(x4)(x5)(x7)(x6)(x1)(st))(st)
c__case_6_case__81 x1 x5 x4@Curry.Module.Prelude.List st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x5)(Curry.Module.OracleCurryStringClassifier.c_plainCode(x4)(x1)(st))(x8)(st))(st)
c__case_6_case__81 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_6_case__81(x1)(x5)(x)(st))(i)(xs)(st)
c__case_6_case__81 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_6_case__81")(x)



c__case_7_case__82 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_ModuleHead x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_6(x5)(x4)(x1)(st))(st)
c__case_7_case__82 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_Code x15) st = let {x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x15)(Curry.Module.OracleCurryStringClassifier.c_plainCode(x4)(x1)(st))(x21)(st))(st)
c__case_7_case__82 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_Text x16) st = let {x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.OraclePrelude.op_43_43(x16)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.OracleCurryStringClassifier.c_plainCode(x4)(x1)(st)))(x22)(st)))(st)
c__case_7_case__82 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_Letter x17) st = let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))(Curry.Module.OraclePrelude.op_43_43(x17)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))(Curry.Module.OracleCurryStringClassifier.c_plainCode(x4)(x1)(st)))(x23)(st)))(st)
c__case_7_case__82 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_BigComment x18) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c_plainCode(x4)(x1)(st))(st)
c__case_7_case__82 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_SmallComment x19) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c_plainCode(x4)(x1)(st))(st)
c__case_7_case__82 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_Meta x20) st = let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x24)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('{'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('+'))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(x20)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('+'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('}'))(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c_plainCode(x4)(x1)(st))(x24)(st))(x25)(st))(x26)(st))(st)
c__case_7_case__82 x1 x4 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_7_case__82(x1)(x4)(x)(st))(i)(xs)(st)
c__case_7_case__82 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_7_case__82")(x)



c__case_8_case__83 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_7(x4)(x3)(x1)(st))(st)
c__case_8_case__83 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_8_case__83 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_8_case__83(x1)(x)(st))(i)(xs)(st)
c__case_8_case__83 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_8_case__83")(x)



c__case_0_case__84 x1 x4 x7 x8@((Curry.Module.Prelude.:<) x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c_unscan((Curry.Module.Prelude.:<)(Curry.Module.CurryStringClassifier.C_Code(x10))(x7))(x1)(st))(st)
c__case_0_case__84 x1 x4 x7 x8@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c_unscan(x4)(x1)(st))(st)
c__case_0_case__84 x1 x4 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_0_case__84(x1)(x4)(x7)(x)(st))(i)(xs)(st)
c__case_0_case__84 x1 x4 x7 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_0_case__84")(x)



c__case_1_case__85 x1 x4 x7 x6@(Curry.Module.CurryStringClassifier.C_Code x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_0(x4)(x7)(x8)(x1)(st))(st)
c__case_1_case__85 x1 x4 x7 x6@(Curry.Module.CurryStringClassifier.C_SmallComment x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c_unscan(x4)(x1)(st))(st)
c__case_1_case__85 x1 x4 x7 x6@(Curry.Module.CurryStringClassifier.C_BigComment x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c_unscan(x4)(x1)(st))(st)
c__case_1_case__85 x1 x4 x7 x6@(Curry.Module.CurryStringClassifier.C_Text x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c_unscan(x4)(x1)(st))(st)
c__case_1_case__85 x1 x4 x7 x6@(Curry.Module.CurryStringClassifier.C_Letter x14) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c_unscan(x4)(x1)(st))(st)
c__case_1_case__85 x1 x4 x7 x6@(Curry.Module.CurryStringClassifier.C_ModuleHead x15) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c_unscan(x4)(x1)(st))(st)
c__case_1_case__85 x1 x4 x7 x6@(Curry.Module.CurryStringClassifier.C_Meta x16) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c_unscan(x4)(x1)(st))(st)
c__case_1_case__85 x1 x4 x7 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_1_case__85(x1)(x4)(x7)(x)(st))(i)(xs)(st)
c__case_1_case__85 x1 x4 x7 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_1_case__85")(x)



c__case_2_case__86 x1 x4@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_1(x4)(x7)(x6)(x1)(st))(st)
c__case_2_case__86 x1 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c_unscan(x4)(x1)(st))(st)
c__case_2_case__86 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_2_case__86(x1)(x)(st))(i)(xs)(st)
c__case_2_case__86 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_2_case__86")(x)



c__case_3_case__87 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_ModuleHead x5) st = let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x5)(Curry.Module.OracleCurryStringClassifier.c__case_2(x4)(x1)(st))(x23)(st))(st)
c__case_3_case__87 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_Code x17) st = let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(x17)(Curry.Module.OracleCurryStringClassifier.c_unscan(x4)(x1)(st))(x24)(st))(st)
c__case_3_case__87 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_Text x18) st = let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.OraclePrelude.op_43_43(x18)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.OracleCurryStringClassifier.c_unscan(x4)(x1)(st)))(x25)(st)))(st)
c__case_3_case__87 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_Letter x19) st = let {x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x26)(Curry.Module.Prelude.List))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))(Curry.Module.OraclePrelude.op_43_43(x19)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))(Curry.Module.OracleCurryStringClassifier.c_unscan(x4)(x1)(st)))(x26)(st)))(st)
c__case_3_case__87 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_BigComment x20) st = let {x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x27)((Curry.Module.Prelude.:<)(x28)((Curry.Module.Prelude.:<)(x29)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('{'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(x20)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('}'))(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c_unscan(x4)(x1)(st))(x27)(st))(x28)(st))(x29)(st))(st)
c__case_3_case__87 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_SmallComment x21) st = let {x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x30)((Curry.Module.Prelude.:<)(x31)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(x21)(Curry.Module.OracleCurryStringClassifier.c_unscan(x4)(x1)(st))(x30)(st))(x31)(st))(st)
c__case_3_case__87 x1 x4 x3@(Curry.Module.CurryStringClassifier.C_Meta x22) st = let {x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x32)((Curry.Module.Prelude.:<)(x33)((Curry.Module.Prelude.:<)(x34)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('{'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('+'))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(x22)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('+'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('}'))(Curry.Module.Prelude.List)))(Curry.Module.OracleCurryStringClassifier.c_unscan(x4)(x1)(st))(x32)(st))(x33)(st))(x34)(st))(st)
c__case_3_case__87 x1 x4 (Curry.Module.CurryStringClassifier.C_TokenOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_3_case__87(x1)(x4)(x)(st))(i)(xs)(st)
c__case_3_case__87 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_3_case__87")(x)



c__case_4_case__88 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleCurryStringClassifier.c__case_3(x4)(x3)(x1)(st))(st)
c__case_4_case__88 x1 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.List)(st)
c__case_4_case__88 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleCurryStringClassifier.c__case_4_case__88(x1)(x)(st))(i)(xs)(st)
c__case_4_case__88 x1 x st = Curry.RunTimeSystem.patternFail("OracleCurryStringClassifier._case_4_case__88")(x)


