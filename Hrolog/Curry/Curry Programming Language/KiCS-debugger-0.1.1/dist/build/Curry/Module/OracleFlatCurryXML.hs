{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleFlatCurryXML (module Curry.Module.OracleFlatCurryXML) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.FlatCurryXML
import Curry.Module.FlatCurry
import Curry.Module.Prelude
import Curry.Module.Read
import Curry.Module.XML
import Curry.Module.OracleFlatCurry
import Curry.Module.OraclePrelude
import Curry.Module.OracleRead
import Curry.Module.OracleXML



-- begin included



-- end included

c_flatCurryDtd :: Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_flatCurryDtd x1 st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('~'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))))))))))))))))))))))))))(st)



c_flatCurry2XmlFile :: Curry.Module.FlatCurry.C_Prog -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0))
c_flatCurry2XmlFile x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.c_writeFile(x3)(Curry.Module.OracleXML.c_showXmlDocWithParams((Curry.Module.Prelude.:<)(Curry.Module.XML.C_DtdUrl(Curry.Module.OracleFlatCurryXML.c_flatCurryDtd(x1)(st)))(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryXML.c_flatCurry2Xml(x2)(x4)(st))(x5)(st))(x6)(st))(st)



c_flatCurry2Xml :: Curry.Module.FlatCurry.C_Prog -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_XmlExp
c_flatCurry2Xml x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1068(x2)(x1)(st))(st)



c_flatCurry2Xml'46_'35lambda2 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_XmlExp
c_flatCurry2Xml'46_'35lambda2 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)(Curry.Module.OracleXML.c_xtxt(x2)(x1)(st))(Curry.Module.Prelude.List))(x3)(st))(st)



c_qname2xmlattrs :: (Curry t0) => (Curry.Module.Prelude.T2 t0 t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) t0)
c_qname2xmlattrs x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1067(x2)(x1)(st))(st)



c_xmlShowVisibity :: Curry.Module.FlatCurry.C_Visibility -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_xmlShowVisibity x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1066(x2)(x1)(st))(st)



c_xmlShowType :: Curry.Module.FlatCurry.C_TypeDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_XmlExp
c_xmlShowType x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1065(x2)(x1)(st))(st)



c_xmlShowCons :: Curry.Module.FlatCurry.C_ConsDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_XmlExp
c_xmlShowCons x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1064(x2)(x1)(st))(st)



c_xmlShowTypeExpr :: Curry.Module.FlatCurry.C_TypeExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_XmlExp
c_xmlShowTypeExpr x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1063(x2)(x1)(st))(st)



c_xmlShowTVar :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_XmlExp
c_xmlShowTVar x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List)))))((Curry.Module.Prelude.:<)(Curry.Module.OracleXML.c_xtxt(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))(x3)(st))(Curry.Module.Prelude.List))(x4)(st))(st)



c_xmlShowFunc :: Curry.Module.FlatCurry.C_FuncDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_XmlExp
c_xmlShowFunc x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1062(x2)(x1)(st))(st)



c_xmlShowRule :: Curry.Module.FlatCurry.C_Rule -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_XmlExp
c_xmlShowRule x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1061(x2)(x1)(st))(st)



c_xmlShowVar :: (Curry t0) => t0 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_XmlExp
c_xmlShowVar x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List)))(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)(Curry.Module.OracleXML.c_xtxt(Curry.Module.OraclePrelude.c_show(x2)(x1)(st))(x3)(st))(Curry.Module.Prelude.List))(x4)(st))(st)



c_xmlShowExpr :: Curry.Module.FlatCurry.C_Expr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_XmlExp
c_xmlShowExpr x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1060(x2)(x1)(st))(st)



c_xmlShowExpr'46_'35lambda3 :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Expr) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_XmlExp
c_xmlShowExpr'46_'35lambda3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1057(x2)(x1)(st))(st)



c_xmlShowLit :: Curry.Module.FlatCurry.C_Literal -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_XmlExp
c_xmlShowLit x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1056(x2)(x1)(st))(st)



c_xmlShowBranch :: Curry.Module.FlatCurry.C_BranchExpr -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_XmlExp
c_xmlShowBranch x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1055(x2)(x1)(st))(st)



c_xmlShowOp :: Curry.Module.FlatCurry.C_OpDecl -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.XML.C_XmlExp
c_xmlShowOp x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1053(x2)(x1)(st))(st)



c_xmlFile2FlatCurry :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_xmlFile2FlatCurry x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.Oracle.op_62_62_61(Curry.Module.OracleXML.c_readXmlFile(x2)(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xmlFile2FlatCurry'46_'35lambda4))))(x3)(st))(st)



c_xmlFile2FlatCurry'46_'35lambda4 :: Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.FlatCurry.C_Prog))
c_xmlFile2FlatCurry'46_'35lambda4 x2 x1 st = let {x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_return(Curry.Module.OracleFlatCurryXML.c_xml2FlatCurry(x2)(x1)(st))(x3)(st))(st)



c_xml2FlatCurry :: Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Prog
c_xml2FlatCurry x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1052(x2)(x1)(st))(st)



c_xml2FlatCurry'46_'35lambda5 :: Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_xml2FlatCurry'46_'35lambda5 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_950(x2)(x1)(st))(st)



c_xml2FlatCurry'46_'35lambda6 :: Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_FuncDecl
c_xml2FlatCurry'46_'35lambda6 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_935(x2)(x1)(st))(st)



c_xml2FlatCurry'46_'35lambda7 :: Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_OpDecl
c_xml2FlatCurry'46_'35lambda7 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_881(x2)(x1)(st))(st)



c_flatx2typedecl :: Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_TypeDecl
c_flatx2typedecl x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_821(x2)(x1)(st))(st)



c_flatx2typedecl'46_'35lambda8 :: Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_flatx2typedecl'46_'35lambda8 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_715(x2)(x1)(st))(st)



c_flatx2typedecl'46_'35lambda9 :: Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_ConsDecl
c_flatx2typedecl'46_'35lambda9 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_704(x2)(x1)(st))(st)



c_flatx2typedecl'46_'35lambda10 :: Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_flatx2typedecl'46_'35lambda10 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_653(x2)(x1)(st))(st)



c_flatx2FunBody :: Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Rule
c_flatx2FunBody x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_642(x2)(x1)(st))(st)



c_flatx2var :: Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_flatx2var x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_592(x2)(x1)(st))(st)



c_flatx2exp :: Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_flatx2exp x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_583(x2)(x1)(st))(st)



c_flatx2exp'46_'35selFP3'35bindings :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Expr)) Curry.Module.FlatCurry.C_Expr) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Expr)
c_flatx2exp'46_'35selFP3'35bindings x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_302(x2)(x1)(st))(st)



c_flatx2exp'46_'35selFP4'35exp :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Expr)) Curry.Module.FlatCurry.C_Expr) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_flatx2exp'46_'35selFP4'35exp x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_301(x2)(x1)(st))(st)



c_flatx2exp'46_'35selFP6'35bindings :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Expr)) Curry.Module.FlatCurry.C_Expr) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Expr)
c_flatx2exp'46_'35selFP6'35bindings x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_300(x2)(x1)(st))(st)



c_flatx2exp'46_'35selFP7'35exp :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Expr)) Curry.Module.FlatCurry.C_Expr) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_flatx2exp'46_'35selFP7'35exp x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_299(x2)(x1)(st))(st)



c_flatx2let :: (Curry.Module.Prelude.List Curry.Module.XML.C_XmlExp) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Expr)) Curry.Module.FlatCurry.C_Expr
c_flatx2let x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_298(x2)(x1)(st))(st)



c_flatx2let'46_'35selFP9'35bindings :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Expr)) Curry.Module.FlatCurry.C_Expr) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Expr)
c_flatx2let'46_'35selFP9'35bindings x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_267(x2)(x1)(st))(st)



c_flatx2let'46_'35selFP10'35exp :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.FlatCurry.C_Expr)) Curry.Module.FlatCurry.C_Expr) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Expr
c_flatx2let'46_'35selFP10'35exp x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_266(x2)(x1)(st))(st)



c_flatx2branch :: Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_BranchExpr
c_flatx2branch x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_265(x2)(x1)(st))(st)



c_flatx2lit :: Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Literal
c_flatx2lit x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_167(x2)(x1)(st))(st)



c_flatx2texp :: Curry.Module.XML.C_XmlExp -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_TypeExpr
c_flatx2texp x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_134(x2)(x1)(st))(st)



c_xvis2vis :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Visibility
c_xvis2vis x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_70(x2)(x1)(st))(st)



c_flatx2Fixity :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.FlatCurry.C_Fixity
c_flatx2Fixity x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_24(x2)(x1)(st))(st)



c__case_24 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_24_case__1068(x1)(x2)(st))(st)



c__case_23 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_23_case__1067(x1)(x4)(x3)(st))(st)



c__case_22 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_22_case__1066(x1)(x4)(st))(st)



c__case_21 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_21_case__1065(x1)(x6)(x5)(st))(st)



c__case_20 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_20_case__1064(x1)(x6)(st))(st)



c__case_19 x8 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_19_case__1063(x1)(x8)(x7)(st))(st)



c__case_18 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_18_case__1062(x1)(x8)(st))(st)



c__case_17 x10 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_17_case__1061(x1)(x10)(x9)(st))(st)



c__case_16 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_16_case__1060(x1)(x10)(st))(st)



c__case_15 x12 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_15_case__1059(x1)(x12)(x11)(st))(st)



c__case_14 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_14_case__1058(x1)(x12)(st))(st)



c__case_13 x14 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_13_case__1057(x1)(x14)(x13)(st))(st)



c__case_4 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_4_case__1056(x1)(x14)(st))(st)



c__case_3 x22 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_3_case__1055(x1)(x22)(x21)(st))(st)



c__case_2 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_2_case__1054(x1)(x22)(st))(st)



c__case_1 x24 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1_case__1053(x1)(x24)(x23)(st))(st)



c__case_0 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_0_case__1052(x1)(x24)(st))(st)



c__case_9 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_9_case__1051(x1)(x14)(st))(st)



c__case_8 x18 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_8_case__1050(x1)(x18)(x17)(st))(st)



c__case_7 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_7_case__1049(x1)(x18)(st))(st)



c__case_6 x20 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_6_case__1048(x1)(x20)(x19)(st))(st)



c__case_5 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_5_case__1047(x1)(x20)(st))(st)



c__case_12 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_12_case__1046(x1)(x14)(st))(st)



c__case_11 x16 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_11_case__1045(x1)(x16)(x15)(st))(st)



c__case_10 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_10_case__1044(x1)(x16)(st))(st)



c__case_70 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_70_case__1043(x1)(x2)(st))(st)



c__case_69 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_69_case__1042(x1)(x4)(x3)(st))(st)



c__case_68 x4 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_68_case__1041(x1)(x4)(x6)(x5)(st))(st)



c__case_67 x4 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_67_case__1040(x1)(x4)(x6)(st))(st)



c__case_66 x4 x8 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_66_case__1039(x1)(x4)(x8)(x7)(st))(st)



c__case_65 x4 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_65_case__1038(x1)(x4)(x8)(st))(st)



c__case_64 x4 x10 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_64_case__1037(x1)(x4)(x10)(x9)(st))(st)



c__case_63 x4 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_63_case__1036(x1)(x4)(x10)(st))(st)



c__case_62 x4 x12 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_62_case__1035(x1)(x4)(x12)(x11)(st))(st)



c__case_61 x4 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_61_case__1034(x1)(x4)(x12)(st))(st)



c__case_60 x4 x14 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_60_case__1033(x1)(x4)(x14)(x13)(st))(st)



c__case_59 x4 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_59_case__1032(x1)(x4)(x14)(st))(st)



c__case_58 x4 x16 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_58_case__1031(x1)(x4)(x16)(x15)(st))(st)



c__case_57 x4 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_57_case__1030(x1)(x4)(x16)(st))(st)



c__case_56 x4 x18 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_56_case__1029(x1)(x4)(x18)(x17)(st))(st)



c__case_55 x4 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_55_case__1028(x1)(x4)(x18)(st))(st)



c__case_54 x4 x20 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_54_case__1027(x1)(x4)(x20)(x19)(st))(st)



c__case_53 x4 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_53_case__1026(x1)(x4)(x20)(st))(st)



c__case_52 x4 x22 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_52_case__1025(x1)(x4)(x22)(x21)(st))(st)



c__case_51 x4 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_51_case__1024(x1)(x4)(x22)(st))(st)



c__case_50 x4 x24 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_50_case__1023(x1)(x4)(x24)(x23)(st))(st)



c__case_49 x4 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_49_case__1022(x1)(x4)(x24)(st))(st)



c__case_48 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_48_case__1021(x1)(x4)(st))(st)



c__case_47 x26 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_47_case__1020(x1)(x26)(x25)(st))(st)



c__case_46 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_46_case__1019(x1)(x26)(st))(st)



c__case_45 x28 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_45_case__1018(x1)(x28)(x27)(st))(st)



c__case_35 x28 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_35_case__1017(x1)(x28)(st))(st)



c__case_34 x38 x37 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_34_case__1016(x1)(x38)(x37)(st))(st)



c__case_33 x38 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_33_case__1015(x1)(x38)(st))(st)



c__case_32 x40 x39 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_32_case__1014(x1)(x40)(x39)(st))(st)



c__case_31 x40 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_31_case__1013(x1)(x40)(st))(st)



c__case_30 x42 x41 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_30_case__1012(x1)(x42)(x41)(st))(st)



c__case_29 x42 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_29_case__1011(x1)(x42)(st))(st)



c__case_28 x44 x43 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_28_case__1010(x1)(x44)(x43)(st))(st)



c__case_27 x44 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_27_case__1009(x1)(x44)(st))(st)



c__case_26 x46 x45 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_26_case__1008(x1)(x46)(x45)(st))(st)



c__case_25 x46 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_25_case__1007(x1)(x46)(st))(st)



c__case_44 x28 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_44_case__1006(x1)(x28)(st))(st)



c__case_43 x30 x29 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_43_case__1005(x1)(x30)(x29)(st))(st)



c__case_42 x30 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_42_case__1004(x1)(x30)(st))(st)



c__case_41 x32 x31 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_41_case__1003(x1)(x32)(x31)(st))(st)



c__case_40 x32 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_40_case__1002(x1)(x32)(st))(st)



c__case_39 x34 x33 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_39_case__1001(x1)(x34)(x33)(st))(st)



c__case_38 x34 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_38_case__1000(x1)(x34)(st))(st)



c__case_37 x36 x35 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_37_case__999(x1)(x36)(x35)(st))(st)



c__case_36 x36 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_36_case__998(x1)(x36)(st))(st)



c__case_134 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_134_case__997(x1)(x2)(st))(st)



c__case_133 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_133_case__996(x1)(x4)(x5)(x3)(st))(st)



c__case_132 x4 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_132_case__995(x1)(x4)(x5)(x7)(x6)(st))(st)



c__case_89 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_89_case__994(x1)(x4)(x5)(x7)(st))(st)



c__case_88 x4 x5 x49 x48 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_88_case__993(x1)(x4)(x5)(x49)(x48)(st))(st)



c__case_87 x4 x5 x49 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_87_case__992(x1)(x4)(x5)(x49)(st))(st)



c__case_86 x4 x5 x51 x50 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_86_case__991(x1)(x4)(x5)(x51)(x50)(st))(st)



c__case_85 x4 x5 x51 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_85_case__990(x1)(x4)(x5)(x51)(st))(st)



c__case_84 x4 x5 x53 x52 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_84_case__989(x1)(x4)(x5)(x53)(x52)(st))(st)



c__case_83 x4 x5 x53 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_83_case__988(x1)(x4)(x5)(x53)(st))(st)



c__case_82 x4 x5 x55 x54 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_82_case__987(x1)(x4)(x5)(x55)(x54)(st))(st)



c__case_81 x4 x5 x55 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_81_case__986(x1)(x4)(x5)(x55)(st))(st)



c__case_80 x4 x5 x57 x56 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_80_case__985(x1)(x4)(x5)(x57)(x56)(st))(st)



c__case_79 x4 x5 x57 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_79_case__984(x1)(x4)(x5)(x57)(st))(st)



c__case_78 x4 x5 x59 x58 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_78_case__983(x1)(x4)(x5)(x59)(x58)(st))(st)



c__case_77 x4 x5 x59 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_77_case__982(x1)(x4)(x5)(x59)(st))(st)



c__case_76 x4 x5 x61 x60 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_76_case__981(x1)(x4)(x5)(x61)(x60)(st))(st)



c__case_75 x4 x5 x61 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_75_case__980(x1)(x4)(x5)(x61)(st))(st)



c__case_74 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_74_case__979(x1)(x5)(x4)(st))(st)



c__case_73 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_73_case__978(x1)(x5)(st))(st)



c__case_72 x62 x63 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_72_case__977(x1)(x62)(x63)(st))(st)



c__case_71 x62 x64 x65 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_71_case__976(x1)(x62)(x64)(x65)(st))(st)



c__case_131 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_131_case__975(x1)(x4)(x5)(x7)(st))(st)



c__case_130 x4 x5 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_130_case__974(x1)(x4)(x5)(x9)(x8)(st))(st)



c__case_123 x4 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_123_case__973(x1)(x4)(x5)(x9)(st))(st)



c__case_122 x4 x5 x15 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_122_case__972(x1)(x4)(x5)(x15)(x14)(st))(st)



c__case_121 x4 x5 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_121_case__971(x1)(x4)(x5)(x15)(st))(st)



c__case_120 x4 x5 x17 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_120_case__970(x1)(x4)(x5)(x17)(x16)(st))(st)



c__case_119 x4 x5 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_119_case__969(x1)(x4)(x5)(x17)(st))(st)



c__case_118 x4 x5 x19 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_118_case__968(x1)(x4)(x5)(x19)(x18)(st))(st)



c__case_117 x4 x5 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_117_case__967(x1)(x4)(x5)(x19)(st))(st)



c__case_116 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_116_case__966(x1)(x5)(x4)(st))(st)



c__case_115 x5 x21 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_115_case__965(x1)(x5)(x21)(x20)(st))(st)



c__case_114 x5 x21 x23 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_114_case__964(x1)(x5)(x21)(x23)(x22)(st))(st)



c__case_113 x5 x21 x23 x25 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_113_case__963(x1)(x5)(x21)(x23)(x25)(x24)(st))(st)



c__case_112 x5 x21 x23 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_112_case__962(x1)(x5)(x21)(x23)(x25)(st))(st)



c__case_111 x5 x21 x23 x27 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_111_case__961(x1)(x5)(x21)(x23)(x27)(x26)(st))(st)



c__case_110 x5 x21 x23 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_110_case__960(x1)(x5)(x21)(x23)(x27)(st))(st)



c__case_109 x5 x21 x23 x29 x28 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_109_case__959(x1)(x5)(x21)(x23)(x29)(x28)(st))(st)



c__case_108 x5 x21 x23 x29 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_108_case__958(x1)(x5)(x21)(x23)(x29)(st))(st)



c__case_107 x5 x21 x23 x31 x30 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_107_case__957(x1)(x5)(x21)(x23)(x31)(x30)(st))(st)



c__case_106 x5 x21 x23 x31 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_106_case__956(x1)(x5)(x21)(x23)(x31)(st))(st)



c__case_105 x5 x21 x23 x33 x32 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_105_case__955(x1)(x5)(x21)(x23)(x33)(x32)(st))(st)



c__case_104 x5 x21 x23 x33 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_104_case__954(x1)(x5)(x21)(x23)(x33)(st))(st)



c__case_103 x5 x21 x23 x35 x34 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_103_case__953(x1)(x5)(x21)(x23)(x35)(x34)(st))(st)



c__case_102 x5 x21 x23 x35 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_102_case__952(x1)(x5)(x21)(x23)(x35)(st))(st)



c__case_101 x5 x23 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_101_case__951(x1)(x5)(x23)(x21)(st))(st)



c__case_100 x5 x23 x37 x36 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_100_case__950(x1)(x5)(x23)(x37)(x36)(st))(st)



c__case_99 x5 x23 x37 x39 x38 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_99_case__949(x1)(x5)(x23)(x37)(x39)(x38)(st))(st)



c__case_98 x5 x23 x37 x39 x41 x40 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_98_case__948(x1)(x5)(x23)(x37)(x39)(x41)(x40)(st))(st)



c__case_97 x5 x23 x37 x39 x41 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_97_case__947(x1)(x5)(x23)(x37)(x39)(x41)(st))(st)



c__case_96 x5 x23 x37 x39 x43 x42 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_96_case__946(x1)(x5)(x23)(x37)(x39)(x43)(x42)(st))(st)



c__case_95 x5 x23 x37 x39 x43 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_95_case__945(x1)(x5)(x23)(x37)(x39)(x43)(st))(st)



c__case_94 x5 x23 x37 x39 x45 x44 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_94_case__944(x1)(x5)(x23)(x37)(x39)(x45)(x44)(st))(st)



c__case_93 x5 x23 x37 x39 x45 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_93_case__943(x1)(x5)(x23)(x37)(x39)(x45)(st))(st)



c__case_92 x5 x23 x37 x39 x47 x46 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_92_case__942(x1)(x5)(x23)(x37)(x39)(x47)(x46)(st))(st)



c__case_91 x5 x23 x37 x39 x47 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_91_case__941(x1)(x5)(x23)(x37)(x39)(x47)(st))(st)



c__case_90 x5 x23 x39 x37 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_90_case__940(x1)(x5)(x23)(x39)(x37)(st))(st)



c__case_129 x4 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_129_case__939(x1)(x4)(x5)(x9)(st))(st)



c__case_128 x4 x5 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_128_case__938(x1)(x4)(x5)(x11)(x10)(st))(st)



c__case_127 x4 x5 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_127_case__937(x1)(x4)(x5)(x11)(st))(st)



c__case_126 x4 x5 x13 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_126_case__936(x1)(x4)(x5)(x13)(x12)(st))(st)



c__case_125 x4 x5 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_125_case__935(x1)(x4)(x5)(x13)(st))(st)



c__case_124 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_124_case__934(x1)(x5)(x4)(st))(st)



c__case_167 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_167_case__933(x1)(x2)(st))(st)



c__case_166 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_166_case__932(x1)(x4)(x5)(x3)(st))(st)



c__case_165 x4 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_165_case__931(x1)(x4)(x5)(x7)(x6)(st))(st)



c__case_144 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_144_case__930(x1)(x4)(x5)(x7)(st))(st)



c__case_143 x4 x5 x25 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_143_case__929(x1)(x4)(x5)(x25)(x24)(st))(st)



c__case_142 x4 x5 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_142_case__928(x1)(x4)(x5)(x25)(st))(st)



c__case_141 x4 x5 x27 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_141_case__927(x1)(x4)(x5)(x27)(x26)(st))(st)



c__case_140 x4 x5 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_140_case__926(x1)(x4)(x5)(x27)(st))(st)



c__case_139 x4 x5 x29 x28 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_139_case__925(x1)(x4)(x5)(x29)(x28)(st))(st)



c__case_138 x4 x5 x29 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_138_case__924(x1)(x4)(x5)(x29)(st))(st)



c__case_137 x4 x5 x31 x30 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_137_case__923(x1)(x4)(x5)(x31)(x30)(st))(st)



c__case_136 x4 x5 x31 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_136_case__922(x1)(x4)(x5)(x31)(st))(st)



c__case_135 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_135_case__921(x1)(x5)(x4)(st))(st)



c__case_156 x4 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_156_case__920(x1)(x4)(x7)(st))(st)



c__case_155 x4 x15 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_155_case__919(x1)(x4)(x15)(x14)(st))(st)



c__case_154 x4 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_154_case__918(x1)(x4)(x15)(st))(st)



c__case_153 x4 x17 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_153_case__917(x1)(x4)(x17)(x16)(st))(st)



c__case_152 x4 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_152_case__916(x1)(x4)(x17)(st))(st)



c__case_151 x4 x19 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_151_case__915(x1)(x4)(x19)(x18)(st))(st)



c__case_150 x4 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_150_case__914(x1)(x4)(x19)(st))(st)



c__case_149 x4 x21 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_149_case__913(x1)(x4)(x21)(x20)(st))(st)



c__case_148 x4 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_148_case__912(x1)(x4)(x21)(st))(st)



c__case_147 x4 x23 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_147_case__911(x1)(x4)(x23)(x22)(st))(st)



c__case_146 x4 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_146_case__910(x1)(x4)(x23)(st))(st)



c__case_145 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_145_case__909(x1)(x4)(st))(st)



c__case_164 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_164_case__908(x1)(x4)(x5)(x7)(st))(st)



c__case_163 x4 x5 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_163_case__907(x1)(x4)(x5)(x9)(x8)(st))(st)



c__case_162 x4 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_162_case__906(x1)(x4)(x5)(x9)(st))(st)



c__case_161 x4 x5 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_161_case__905(x1)(x4)(x5)(x11)(x10)(st))(st)



c__case_160 x4 x5 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_160_case__904(x1)(x4)(x5)(x11)(st))(st)



c__case_159 x4 x5 x13 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_159_case__903(x1)(x4)(x5)(x13)(x12)(st))(st)



c__case_158 x4 x5 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_158_case__902(x1)(x4)(x5)(x13)(st))(st)



c__case_157 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_157_case__901(x1)(x5)(x4)(st))(st)



c__case_265 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_265_case__900(x1)(x2)(st))(st)



c__case_264 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_264_case__899(x1)(x4)(x5)(x3)(st))(st)



c__case_263 x4 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_263_case__898(x1)(x4)(x5)(x7)(x6)(st))(st)



c__case_262 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_262_case__897(x1)(x4)(x5)(x7)(st))(st)



c__case_261 x4 x5 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_261_case__896(x1)(x4)(x5)(x9)(x8)(st))(st)



c__case_260 x4 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_260_case__895(x1)(x4)(x5)(x9)(st))(st)



c__case_259 x4 x5 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_259_case__894(x1)(x4)(x5)(x11)(x10)(st))(st)



c__case_258 x4 x5 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_258_case__893(x1)(x4)(x5)(x11)(st))(st)



c__case_257 x4 x5 x13 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_257_case__892(x1)(x4)(x5)(x13)(x12)(st))(st)



c__case_256 x4 x5 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_256_case__891(x1)(x4)(x5)(x13)(st))(st)



c__case_255 x4 x5 x15 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_255_case__890(x1)(x4)(x5)(x15)(x14)(st))(st)



c__case_254 x4 x5 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_254_case__889(x1)(x4)(x5)(x15)(st))(st)



c__case_253 x4 x5 x17 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_253_case__888(x1)(x4)(x5)(x17)(x16)(st))(st)



c__case_252 x4 x5 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_252_case__887(x1)(x4)(x5)(x17)(st))(st)



c__case_251 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_251_case__886(x1)(x5)(x4)(st))(st)



c__case_250 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_250_case__885(x1)(x5)(st))(st)



c__case_249 x19 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_249_case__884(x1)(x19)(x18)(st))(st)



c__case_248 x19 x21 x22 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_248_case__883(x1)(x19)(x21)(x22)(x20)(st))(st)



c__case_247 x19 x21 x22 x24 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_247_case__882(x1)(x19)(x21)(x22)(x24)(x23)(st))(st)



c__case_184 x19 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_184_case__881(x1)(x19)(x24)(st))(st)



c__case_183 x19 x86 x85 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_183_case__880(x1)(x19)(x86)(x85)(st))(st)



c__case_182 x19 x86 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_182_case__879(x1)(x19)(x86)(st))(st)



c__case_181 x19 x88 x87 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_181_case__878(x1)(x19)(x88)(x87)(st))(st)



c__case_180 x19 x88 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_180_case__877(x1)(x19)(x88)(st))(st)



c__case_179 x19 x90 x89 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_179_case__876(x1)(x19)(x90)(x89)(st))(st)



c__case_178 x19 x90 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_178_case__875(x1)(x19)(x90)(st))(st)



c__case_177 x19 x92 x91 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_177_case__874(x1)(x19)(x92)(x91)(st))(st)



c__case_176 x19 x92 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_176_case__873(x1)(x19)(x92)(st))(st)



c__case_175 x19 x94 x93 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_175_case__872(x1)(x19)(x94)(x93)(st))(st)



c__case_174 x19 x94 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_174_case__871(x1)(x19)(x94)(st))(st)



c__case_173 x19 x96 x95 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_173_case__870(x1)(x19)(x96)(x95)(st))(st)



c__case_172 x19 x96 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_172_case__869(x1)(x19)(x96)(st))(st)



c__case_171 x19 x98 x97 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_171_case__868(x1)(x19)(x98)(x97)(st))(st)



c__case_170 x19 x98 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_170_case__867(x1)(x19)(x98)(st))(st)



c__case_169 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_169_case__866(x1)(x19)(st))(st)



c__case_168 x100 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_168_case__865(x1)(x100)(st))(st)



c__case_204 x19 x21 x22 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_204_case__864(x1)(x19)(x21)(x22)(x24)(st))(st)



c__case_203 x19 x21 x22 x68 x67 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_203_case__863(x1)(x19)(x21)(x22)(x68)(x67)(st))(st)



c__case_202 x19 x21 x22 x68 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_202_case__862(x1)(x19)(x21)(x22)(x68)(st))(st)



c__case_201 x19 x21 x22 x70 x69 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_201_case__861(x1)(x19)(x21)(x22)(x70)(x69)(st))(st)



c__case_200 x19 x21 x22 x70 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_200_case__860(x1)(x19)(x21)(x22)(x70)(st))(st)



c__case_199 x19 x21 x22 x72 x71 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_199_case__859(x1)(x19)(x21)(x22)(x72)(x71)(st))(st)



c__case_198 x19 x21 x22 x72 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_198_case__858(x1)(x19)(x21)(x22)(x72)(st))(st)



c__case_197 x19 x21 x22 x74 x73 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_197_case__857(x1)(x19)(x21)(x22)(x74)(x73)(st))(st)



c__case_196 x19 x21 x22 x74 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_196_case__856(x1)(x19)(x21)(x22)(x74)(st))(st)



c__case_195 x19 x21 x22 x76 x75 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_195_case__855(x1)(x19)(x21)(x22)(x76)(x75)(st))(st)



c__case_194 x19 x21 x22 x76 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_194_case__854(x1)(x19)(x21)(x22)(x76)(st))(st)



c__case_193 x19 x21 x22 x78 x77 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_193_case__853(x1)(x19)(x21)(x22)(x78)(x77)(st))(st)



c__case_192 x19 x21 x22 x78 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_192_case__852(x1)(x19)(x21)(x22)(x78)(st))(st)



c__case_191 x19 x21 x22 x80 x79 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_191_case__851(x1)(x19)(x21)(x22)(x80)(x79)(st))(st)



c__case_190 x19 x21 x22 x80 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_190_case__850(x1)(x19)(x21)(x22)(x80)(st))(st)



c__case_189 x19 x22 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_189_case__849(x1)(x19)(x22)(x21)(st))(st)



c__case_188 x19 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_188_case__848(x1)(x19)(x22)(st))(st)



c__case_187 x19 x81 x82 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_187_case__847(x1)(x19)(x81)(x82)(st))(st)



c__case_186 x81 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_186_case__846(x1)(x81)(x19)(st))(st)



c__case_185 x81 x83 x84 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_185_case__845(x1)(x81)(x83)(x84)(st))(st)



c__case_246 x19 x21 x22 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_246_case__844(x1)(x19)(x21)(x22)(x24)(st))(st)



c__case_245 x19 x21 x22 x26 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_245_case__843(x1)(x19)(x21)(x22)(x26)(x25)(st))(st)



c__case_244 x19 x21 x22 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_244_case__842(x1)(x19)(x21)(x22)(x26)(st))(st)



c__case_243 x19 x21 x22 x28 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_243_case__841(x1)(x19)(x21)(x22)(x28)(x27)(st))(st)



c__case_242 x19 x21 x22 x28 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_242_case__840(x1)(x19)(x21)(x22)(x28)(st))(st)



c__case_241 x19 x21 x22 x30 x29 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_241_case__839(x1)(x19)(x21)(x22)(x30)(x29)(st))(st)



c__case_240 x19 x21 x22 x30 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_240_case__838(x1)(x19)(x21)(x22)(x30)(st))(st)



c__case_239 x19 x21 x22 x32 x31 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_239_case__837(x1)(x19)(x21)(x22)(x32)(x31)(st))(st)



c__case_238 x19 x21 x22 x32 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_238_case__836(x1)(x19)(x21)(x22)(x32)(st))(st)



c__case_237 x19 x21 x22 x34 x33 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_237_case__835(x1)(x19)(x21)(x22)(x34)(x33)(st))(st)



c__case_236 x19 x21 x22 x34 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_236_case__834(x1)(x19)(x21)(x22)(x34)(st))(st)



c__case_235 x19 x21 x22 x36 x35 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_235_case__833(x1)(x19)(x21)(x22)(x36)(x35)(st))(st)



c__case_234 x19 x21 x22 x36 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_234_case__832(x1)(x19)(x21)(x22)(x36)(st))(st)



c__case_233 x19 x22 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_233_case__831(x1)(x19)(x22)(x21)(st))(st)



c__case_232 x19 x22 x38 x37 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_232_case__830(x1)(x19)(x22)(x38)(x37)(st))(st)



c__case_231 x19 x22 x38 x40 x39 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_231_case__829(x1)(x19)(x22)(x38)(x40)(x39)(st))(st)



c__case_230 x19 x22 x38 x40 x42 x41 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_230_case__828(x1)(x19)(x22)(x38)(x40)(x42)(x41)(st))(st)



c__case_229 x19 x22 x38 x40 x42 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_229_case__827(x1)(x19)(x22)(x38)(x40)(x42)(st))(st)



c__case_228 x19 x22 x38 x40 x44 x43 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_228_case__826(x1)(x19)(x22)(x38)(x40)(x44)(x43)(st))(st)



c__case_227 x19 x22 x38 x40 x44 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_227_case__825(x1)(x19)(x22)(x38)(x40)(x44)(st))(st)



c__case_226 x19 x22 x38 x40 x46 x45 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_226_case__824(x1)(x19)(x22)(x38)(x40)(x46)(x45)(st))(st)



c__case_225 x19 x22 x38 x40 x46 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_225_case__823(x1)(x19)(x22)(x38)(x40)(x46)(st))(st)



c__case_224 x19 x22 x38 x40 x48 x47 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_224_case__822(x1)(x19)(x22)(x38)(x40)(x48)(x47)(st))(st)



c__case_223 x19 x22 x38 x40 x48 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_223_case__821(x1)(x19)(x22)(x38)(x40)(x48)(st))(st)



c__case_222 x19 x22 x38 x40 x50 x49 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_222_case__820(x1)(x19)(x22)(x38)(x40)(x50)(x49)(st))(st)



c__case_221 x19 x22 x38 x40 x50 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_221_case__819(x1)(x19)(x22)(x38)(x40)(x50)(st))(st)



c__case_220 x19 x22 x38 x40 x52 x51 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_220_case__818(x1)(x19)(x22)(x38)(x40)(x52)(x51)(st))(st)



c__case_219 x19 x22 x38 x40 x52 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_219_case__817(x1)(x19)(x22)(x38)(x40)(x52)(st))(st)



c__case_218 x19 x22 x40 x38 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_218_case__816(x1)(x19)(x22)(x40)(x38)(st))(st)



c__case_217 x19 x22 x40 x54 x53 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_217_case__815(x1)(x19)(x22)(x40)(x54)(x53)(st))(st)



c__case_216 x19 x22 x40 x54 x56 x55 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_216_case__814(x1)(x19)(x22)(x40)(x54)(x56)(x55)(st))(st)



c__case_215 x19 x22 x40 x54 x56 x58 x57 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_215_case__813(x1)(x19)(x22)(x40)(x54)(x56)(x58)(x57)(st))(st)



c__case_214 x19 x22 x40 x54 x56 x58 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_214_case__812(x1)(x19)(x22)(x40)(x54)(x56)(x58)(st))(st)



c__case_213 x19 x22 x40 x54 x56 x60 x59 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_213_case__811(x1)(x19)(x22)(x40)(x54)(x56)(x60)(x59)(st))(st)



c__case_212 x19 x22 x40 x54 x56 x60 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_212_case__810(x1)(x19)(x22)(x40)(x54)(x56)(x60)(st))(st)



c__case_211 x19 x22 x40 x54 x56 x62 x61 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_211_case__809(x1)(x19)(x22)(x40)(x54)(x56)(x62)(x61)(st))(st)



c__case_210 x19 x22 x40 x54 x56 x62 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_210_case__808(x1)(x19)(x22)(x40)(x54)(x56)(x62)(st))(st)



c__case_209 x19 x22 x40 x54 x56 x64 x63 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_209_case__807(x1)(x19)(x22)(x40)(x54)(x56)(x64)(x63)(st))(st)



c__case_208 x19 x22 x40 x54 x56 x64 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_208_case__806(x1)(x19)(x22)(x40)(x54)(x56)(x64)(st))(st)



c__case_207 x19 x22 x40 x56 x54 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_207_case__805(x1)(x19)(x22)(x40)(x56)(x54)(st))(st)



c__case_206 x22 x40 x56 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_206_case__804(x1)(x22)(x40)(x56)(x19)(st))(st)



c__case_205 x22 x40 x56 x65 x66 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_205_case__803(x1)(x22)(x40)(x56)(x65)(x66)(st))(st)



c__case_266 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_266_case__802(x1)(x2)(st))(st)



c__case_267 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_267_case__801(x1)(x2)(st))(st)



c__case_298 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_298_case__800(x1)(x2)(st))(st)



c__case_297 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_297_case__799(x1)(x3)(x4)(st))(st)



c__case_296 x5 x6 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_296_case__798(x1)(x5)(x6)(x3)(st))(st)



c__case_295 x5 x6 x8 x9 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_295_case__797(x1)(x5)(x6)(x8)(x9)(x7)(st))(st)



c__case_294 x5 x6 x8 x9 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_294_case__796(x1)(x5)(x6)(x8)(x9)(x11)(x10)(st))(st)



c__case_293 x5 x6 x8 x9 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_293_case__795(x1)(x5)(x6)(x8)(x9)(x11)(st))(st)



c__case_292 x5 x6 x8 x9 x13 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_292_case__794(x1)(x5)(x6)(x8)(x9)(x13)(x12)(st))(st)



c__case_291 x5 x6 x8 x9 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_291_case__793(x1)(x5)(x6)(x8)(x9)(x13)(st))(st)



c__case_290 x5 x6 x8 x9 x15 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_290_case__792(x1)(x5)(x6)(x8)(x9)(x15)(x14)(st))(st)



c__case_289 x5 x6 x8 x9 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_289_case__791(x1)(x5)(x6)(x8)(x9)(x15)(st))(st)



c__case_288 x5 x6 x8 x9 x17 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_288_case__790(x1)(x5)(x6)(x8)(x9)(x17)(x16)(st))(st)



c__case_287 x5 x6 x8 x9 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_287_case__789(x1)(x5)(x6)(x8)(x9)(x17)(st))(st)



c__case_286 x5 x6 x8 x9 x19 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_286_case__788(x1)(x5)(x6)(x8)(x9)(x19)(x18)(st))(st)



c__case_285 x5 x6 x8 x9 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_285_case__787(x1)(x5)(x6)(x8)(x9)(x19)(st))(st)



c__case_284 x5 x6 x8 x9 x21 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_284_case__786(x1)(x5)(x6)(x8)(x9)(x21)(x20)(st))(st)



c__case_283 x5 x6 x8 x9 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_283_case__785(x1)(x5)(x6)(x8)(x9)(x21)(st))(st)



c__case_282 x5 x6 x8 x9 x23 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_282_case__784(x1)(x5)(x6)(x8)(x9)(x23)(x22)(st))(st)



c__case_281 x5 x6 x8 x9 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_281_case__783(x1)(x5)(x6)(x8)(x9)(x23)(st))(st)



c__case_280 x5 x6 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_280_case__782(x1)(x5)(x6)(x9)(x8)(st))(st)



c__case_279 x5 x6 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_279_case__781(x1)(x5)(x6)(x9)(st))(st)



c__case_278 x5 x6 x25 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_278_case__780(x1)(x5)(x6)(x25)(x24)(st))(st)



c__case_277 x5 x6 x25 x27 x28 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_277_case__779(x1)(x5)(x6)(x25)(x27)(x28)(x26)(st))(st)



c__case_276 x5 x6 x25 x27 x28 x30 x29 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_276_case__778(x1)(x5)(x6)(x25)(x27)(x28)(x30)(x29)(st))(st)



c__case_275 x5 x6 x25 x27 x28 x30 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_275_case__777(x1)(x5)(x6)(x25)(x27)(x28)(x30)(st))(st)



c__case_274 x5 x6 x25 x27 x28 x32 x31 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_274_case__776(x1)(x5)(x6)(x25)(x27)(x28)(x32)(x31)(st))(st)



c__case_273 x5 x6 x25 x27 x28 x32 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_273_case__775(x1)(x5)(x6)(x25)(x27)(x28)(x32)(st))(st)



c__case_272 x5 x6 x25 x27 x28 x34 x33 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_272_case__774(x1)(x5)(x6)(x25)(x27)(x28)(x34)(x33)(st))(st)



c__case_271 x5 x6 x25 x27 x28 x34 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_271_case__773(x1)(x5)(x6)(x25)(x27)(x28)(x34)(st))(st)



c__case_270 x5 x6 x25 x28 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_270_case__772(x1)(x5)(x6)(x25)(x28)(x27)(st))(st)



c__case_269 x5 x6 x28 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_269_case__771(x1)(x5)(x6)(x28)(x25)(st))(st)



c__case_268 x5 x6 x28 x35 x36 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_268_case__770(x1)(x5)(x6)(x28)(x35)(x36)(st))(st)



c__case_299 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_299_case__769(x1)(x2)(st))(st)



c__case_300 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_300_case__768(x1)(x2)(st))(st)



c__case_301 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_301_case__767(x1)(x2)(st))(st)



c__case_302 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_302_case__766(x1)(x2)(st))(st)



c__case_583 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_583_case__765(x1)(x2)(st))(st)



c__case_582 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_582_case__764(x1)(x4)(x5)(x3)(st))(st)



c__case_581 x4 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_581_case__763(x1)(x4)(x5)(x7)(x6)(st))(st)



c__case_309 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_309_case__762(x1)(x4)(x5)(x7)(st))(st)



c__case_308 x4 x5 x278 x277 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_308_case__761(x1)(x4)(x5)(x278)(x277)(st))(st)



c__case_307 x4 x5 x278 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_307_case__760(x1)(x4)(x5)(x278)(st))(st)



c__case_306 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_306_case__759(x1)(x5)(x4)(st))(st)



c__case_305 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_305_case__758(x1)(x5)(st))(st)



c__case_304 x279 x280 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_304_case__757(x1)(x279)(x280)(st))(st)



c__case_303 x279 x281 x282 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_303_case__756(x1)(x279)(x281)(x282)(st))(st)



c__case_417 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_417_case__755(x1)(x4)(x5)(x7)(st))(st)



c__case_416 x4 x5 x170 x169 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_416_case__754(x1)(x4)(x5)(x170)(x169)(st))(st)



c__case_316 x4 x5 x170 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_316_case__753(x1)(x4)(x5)(x170)(st))(st)



c__case_315 x4 x5 x272 x271 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_315_case__752(x1)(x4)(x5)(x272)(x271)(st))(st)



c__case_314 x4 x5 x272 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_314_case__751(x1)(x4)(x5)(x272)(st))(st)



c__case_313 x4 x5 x274 x273 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_313_case__750(x1)(x4)(x5)(x274)(x273)(st))(st)



c__case_312 x4 x5 x274 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_312_case__749(x1)(x4)(x5)(x274)(st))(st)



c__case_311 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_311_case__748(x1)(x5)(x4)(st))(st)



c__case_310 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_310_case__747(x1)(x5)(st))(st)



c__case_415 x4 x5 x170 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_415_case__746(x1)(x4)(x5)(x170)(st))(st)



c__case_414 x4 x5 x172 x171 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_414_case__745(x1)(x4)(x5)(x172)(x171)(st))(st)



c__case_413 x4 x5 x172 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_413_case__744(x1)(x4)(x5)(x172)(st))(st)



c__case_412 x4 x5 x174 x173 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_412_case__743(x1)(x4)(x5)(x174)(x173)(st))(st)



c__case_411 x4 x5 x174 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_411_case__742(x1)(x4)(x5)(x174)(st))(st)



c__case_410 x4 x5 x176 x175 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_410_case__741(x1)(x4)(x5)(x176)(x175)(st))(st)



c__case_375 x4 x5 x176 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_375_case__740(x1)(x4)(x5)(x176)(st))(st)



c__case_374 x4 x5 x212 x211 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_374_case__739(x1)(x4)(x5)(x212)(x211)(st))(st)



c__case_373 x4 x5 x212 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_373_case__738(x1)(x4)(x5)(x212)(st))(st)



c__case_372 x4 x5 x214 x213 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_372_case__737(x1)(x4)(x5)(x214)(x213)(st))(st)



c__case_371 x4 x5 x214 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_371_case__736(x1)(x4)(x5)(x214)(st))(st)



c__case_370 x4 x5 x216 x215 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_370_case__735(x1)(x4)(x5)(x216)(x215)(st))(st)



c__case_369 x4 x5 x216 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_369_case__734(x1)(x4)(x5)(x216)(st))(st)



c__case_368 x4 x5 x218 x217 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_368_case__733(x1)(x4)(x5)(x218)(x217)(st))(st)



c__case_367 x4 x5 x218 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_367_case__732(x1)(x4)(x5)(x218)(st))(st)



c__case_366 x4 x5 x220 x219 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_366_case__731(x1)(x4)(x5)(x220)(x219)(st))(st)



c__case_365 x4 x5 x220 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_365_case__730(x1)(x4)(x5)(x220)(st))(st)



c__case_364 x4 x5 x222 x221 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_364_case__729(x1)(x4)(x5)(x222)(x221)(st))(st)



c__case_363 x4 x5 x222 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_363_case__728(x1)(x4)(x5)(x222)(st))(st)



c__case_362 x4 x5 x224 x223 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_362_case__727(x1)(x4)(x5)(x224)(x223)(st))(st)



c__case_361 x4 x5 x224 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_361_case__726(x1)(x4)(x5)(x224)(st))(st)



c__case_360 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_360_case__725(x1)(x5)(x4)(st))(st)



c__case_359 x5 x226 x225 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_359_case__724(x1)(x5)(x226)(x225)(st))(st)



c__case_358 x5 x226 x228 x227 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_358_case__723(x1)(x5)(x226)(x228)(x227)(st))(st)



c__case_357 x5 x226 x228 x230 x229 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_357_case__722(x1)(x5)(x226)(x228)(x230)(x229)(st))(st)



c__case_356 x5 x226 x228 x230 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_356_case__721(x1)(x5)(x226)(x228)(x230)(st))(st)



c__case_355 x5 x226 x228 x232 x231 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_355_case__720(x1)(x5)(x226)(x228)(x232)(x231)(st))(st)



c__case_354 x5 x226 x228 x232 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_354_case__719(x1)(x5)(x226)(x228)(x232)(st))(st)



c__case_353 x5 x226 x228 x234 x233 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_353_case__718(x1)(x5)(x226)(x228)(x234)(x233)(st))(st)



c__case_352 x5 x226 x228 x234 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_352_case__717(x1)(x5)(x226)(x228)(x234)(st))(st)



c__case_351 x5 x226 x228 x236 x235 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_351_case__716(x1)(x5)(x226)(x228)(x236)(x235)(st))(st)



c__case_350 x5 x226 x228 x236 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_350_case__715(x1)(x5)(x226)(x228)(x236)(st))(st)



c__case_349 x5 x226 x228 x238 x237 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_349_case__714(x1)(x5)(x226)(x228)(x238)(x237)(st))(st)



c__case_348 x5 x226 x228 x238 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_348_case__713(x1)(x5)(x226)(x228)(x238)(st))(st)



c__case_347 x5 x226 x228 x240 x239 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_347_case__712(x1)(x5)(x226)(x228)(x240)(x239)(st))(st)



c__case_346 x5 x226 x228 x240 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_346_case__711(x1)(x5)(x226)(x228)(x240)(st))(st)



c__case_345 x5 x228 x226 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_345_case__710(x1)(x5)(x228)(x226)(st))(st)



c__case_344 x5 x228 x242 x241 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_344_case__709(x1)(x5)(x228)(x242)(x241)(st))(st)



c__case_343 x5 x228 x242 x244 x243 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_343_case__708(x1)(x5)(x228)(x242)(x244)(x243)(st))(st)



c__case_342 x5 x228 x242 x244 x246 x245 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_342_case__707(x1)(x5)(x228)(x242)(x244)(x246)(x245)(st))(st)



c__case_341 x5 x228 x242 x244 x246 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_341_case__706(x1)(x5)(x228)(x242)(x244)(x246)(st))(st)



c__case_340 x5 x228 x242 x244 x248 x247 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_340_case__705(x1)(x5)(x228)(x242)(x244)(x248)(x247)(st))(st)



c__case_339 x5 x228 x242 x244 x248 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_339_case__704(x1)(x5)(x228)(x242)(x244)(x248)(st))(st)



c__case_338 x5 x228 x242 x244 x250 x249 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_338_case__703(x1)(x5)(x228)(x242)(x244)(x250)(x249)(st))(st)



c__case_337 x5 x228 x242 x244 x250 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_337_case__702(x1)(x5)(x228)(x242)(x244)(x250)(st))(st)



c__case_336 x5 x228 x242 x244 x252 x251 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_336_case__701(x1)(x5)(x228)(x242)(x244)(x252)(x251)(st))(st)



c__case_335 x5 x228 x242 x244 x252 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_335_case__700(x1)(x5)(x228)(x242)(x244)(x252)(st))(st)



c__case_334 x5 x228 x244 x242 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_334_case__699(x1)(x5)(x228)(x244)(x242)(st))(st)



c__case_333 x5 x228 x244 x254 x253 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_333_case__698(x1)(x5)(x228)(x244)(x254)(x253)(st))(st)



c__case_332 x5 x228 x244 x254 x256 x255 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_332_case__697(x1)(x5)(x228)(x244)(x254)(x256)(x255)(st))(st)



c__case_331 x5 x228 x244 x254 x256 x258 x257 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_331_case__696(x1)(x5)(x228)(x244)(x254)(x256)(x258)(x257)(st))(st)



c__case_330 x5 x228 x244 x254 x256 x258 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_330_case__695(x1)(x5)(x228)(x244)(x254)(x256)(x258)(st))(st)



c__case_329 x5 x228 x244 x254 x256 x260 x259 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_329_case__694(x1)(x5)(x228)(x244)(x254)(x256)(x260)(x259)(st))(st)



c__case_328 x5 x228 x244 x254 x256 x260 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_328_case__693(x1)(x5)(x228)(x244)(x254)(x256)(x260)(st))(st)



c__case_327 x5 x228 x244 x254 x256 x262 x261 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_327_case__692(x1)(x5)(x228)(x244)(x254)(x256)(x262)(x261)(st))(st)



c__case_326 x5 x228 x244 x254 x256 x262 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_326_case__691(x1)(x5)(x228)(x244)(x254)(x256)(x262)(st))(st)



c__case_325 x5 x228 x244 x254 x256 x264 x263 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_325_case__690(x1)(x5)(x228)(x244)(x254)(x256)(x264)(x263)(st))(st)



c__case_324 x5 x228 x244 x254 x256 x264 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_324_case__689(x1)(x5)(x228)(x244)(x254)(x256)(x264)(st))(st)



c__case_323 x5 x228 x244 x254 x256 x266 x265 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_323_case__688(x1)(x5)(x228)(x244)(x254)(x256)(x266)(x265)(st))(st)



c__case_322 x5 x228 x244 x254 x256 x266 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_322_case__687(x1)(x5)(x228)(x244)(x254)(x256)(x266)(st))(st)



c__case_321 x5 x228 x244 x254 x256 x268 x267 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_321_case__686(x1)(x5)(x228)(x244)(x254)(x256)(x268)(x267)(st))(st)



c__case_320 x5 x228 x244 x254 x256 x268 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_320_case__685(x1)(x5)(x228)(x244)(x254)(x256)(x268)(st))(st)



c__case_319 x5 x228 x244 x254 x256 x270 x269 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_319_case__684(x1)(x5)(x228)(x244)(x254)(x256)(x270)(x269)(st))(st)



c__case_318 x5 x228 x244 x254 x256 x270 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_318_case__683(x1)(x5)(x228)(x244)(x254)(x256)(x270)(st))(st)



c__case_317 x5 x228 x244 x256 x254 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_317_case__682(x1)(x5)(x228)(x244)(x256)(x254)(st))(st)



c__case_409 x4 x5 x176 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_409_case__681(x1)(x4)(x5)(x176)(st))(st)



c__case_408 x4 x5 x178 x177 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_408_case__680(x1)(x4)(x5)(x178)(x177)(st))(st)



c__case_407 x4 x5 x178 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_407_case__679(x1)(x4)(x5)(x178)(st))(st)



c__case_406 x4 x5 x180 x179 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_406_case__678(x1)(x4)(x5)(x180)(x179)(st))(st)



c__case_405 x4 x5 x180 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_405_case__677(x1)(x4)(x5)(x180)(st))(st)



c__case_404 x4 x5 x182 x181 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_404_case__676(x1)(x4)(x5)(x182)(x181)(st))(st)



c__case_403 x4 x5 x182 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_403_case__675(x1)(x4)(x5)(x182)(st))(st)



c__case_402 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_402_case__674(x1)(x5)(x4)(st))(st)



c__case_401 x5 x184 x183 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_401_case__673(x1)(x5)(x184)(x183)(st))(st)



c__case_400 x5 x184 x186 x185 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_400_case__672(x1)(x5)(x184)(x186)(x185)(st))(st)



c__case_399 x5 x184 x186 x188 x187 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_399_case__671(x1)(x5)(x184)(x186)(x188)(x187)(st))(st)



c__case_398 x5 x184 x186 x188 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_398_case__670(x1)(x5)(x184)(x186)(x188)(st))(st)



c__case_397 x5 x184 x186 x190 x189 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_397_case__669(x1)(x5)(x184)(x186)(x190)(x189)(st))(st)



c__case_396 x5 x184 x186 x190 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_396_case__668(x1)(x5)(x184)(x186)(x190)(st))(st)



c__case_395 x5 x184 x186 x192 x191 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_395_case__667(x1)(x5)(x184)(x186)(x192)(x191)(st))(st)



c__case_394 x5 x184 x186 x192 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_394_case__666(x1)(x5)(x184)(x186)(x192)(st))(st)



c__case_393 x5 x184 x186 x194 x193 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_393_case__665(x1)(x5)(x184)(x186)(x194)(x193)(st))(st)



c__case_392 x5 x184 x186 x194 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_392_case__664(x1)(x5)(x184)(x186)(x194)(st))(st)



c__case_391 x5 x184 x186 x196 x195 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_391_case__663(x1)(x5)(x184)(x186)(x196)(x195)(st))(st)



c__case_390 x5 x184 x186 x196 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_390_case__662(x1)(x5)(x184)(x186)(x196)(st))(st)



c__case_389 x5 x184 x186 x198 x197 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_389_case__661(x1)(x5)(x184)(x186)(x198)(x197)(st))(st)



c__case_388 x5 x184 x186 x198 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_388_case__660(x1)(x5)(x184)(x186)(x198)(st))(st)



c__case_387 x5 x186 x184 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_387_case__659(x1)(x5)(x186)(x184)(st))(st)



c__case_386 x5 x186 x200 x199 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_386_case__658(x1)(x5)(x186)(x200)(x199)(st))(st)



c__case_385 x5 x186 x200 x202 x201 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_385_case__657(x1)(x5)(x186)(x200)(x202)(x201)(st))(st)



c__case_384 x5 x186 x200 x202 x204 x203 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_384_case__656(x1)(x5)(x186)(x200)(x202)(x204)(x203)(st))(st)



c__case_383 x5 x186 x200 x202 x204 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_383_case__655(x1)(x5)(x186)(x200)(x202)(x204)(st))(st)



c__case_382 x5 x186 x200 x202 x206 x205 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_382_case__654(x1)(x5)(x186)(x200)(x202)(x206)(x205)(st))(st)



c__case_381 x5 x186 x200 x202 x206 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_381_case__653(x1)(x5)(x186)(x200)(x202)(x206)(st))(st)



c__case_380 x5 x186 x200 x202 x208 x207 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_380_case__652(x1)(x5)(x186)(x200)(x202)(x208)(x207)(st))(st)



c__case_379 x5 x186 x200 x202 x208 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_379_case__651(x1)(x5)(x186)(x200)(x202)(x208)(st))(st)



c__case_378 x5 x186 x200 x202 x210 x209 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_378_case__650(x1)(x5)(x186)(x200)(x202)(x210)(x209)(st))(st)



c__case_377 x5 x186 x200 x202 x210 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_377_case__649(x1)(x5)(x186)(x200)(x202)(x210)(st))(st)



c__case_376 x5 x186 x202 x200 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_376_case__648(x1)(x5)(x186)(x202)(x200)(st))(st)



c__case_555 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_555_case__647(x1)(x4)(x5)(x7)(st))(st)



c__case_554 x4 x5 x33 x32 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_554_case__646(x1)(x4)(x5)(x33)(x32)(st))(st)



c__case_426 x4 x5 x33 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_426_case__645(x1)(x4)(x5)(x33)(st))(st)



c__case_425 x4 x5 x162 x161 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_425_case__644(x1)(x4)(x5)(x162)(x161)(st))(st)



c__case_424 x4 x5 x162 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_424_case__643(x1)(x4)(x5)(x162)(st))(st)



c__case_423 x4 x5 x164 x163 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_423_case__642(x1)(x4)(x5)(x164)(x163)(st))(st)



c__case_422 x4 x5 x164 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_422_case__641(x1)(x4)(x5)(x164)(st))(st)



c__case_421 x4 x5 x166 x165 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_421_case__640(x1)(x4)(x5)(x166)(x165)(st))(st)



c__case_420 x4 x5 x166 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_420_case__639(x1)(x4)(x5)(x166)(st))(st)



c__case_419 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_419_case__638(x1)(x5)(x4)(st))(st)



c__case_418 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_418_case__637(x1)(x5)(st))(st)



c__case_454 x4 x5 x33 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_454_case__636(x1)(x4)(x5)(x33)(st))(st)



c__case_453 x4 x5 x135 x134 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_453_case__635(x1)(x4)(x5)(x135)(x134)(st))(st)



c__case_452 x4 x5 x135 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_452_case__634(x1)(x4)(x5)(x135)(st))(st)



c__case_451 x4 x5 x137 x136 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_451_case__633(x1)(x4)(x5)(x137)(x136)(st))(st)



c__case_450 x4 x5 x137 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_450_case__632(x1)(x4)(x5)(x137)(st))(st)



c__case_449 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_449_case__631(x1)(x5)(x4)(st))(st)



c__case_448 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_448_case__630(x1)(x5)(st))(st)



c__case_447 x139 x138 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_447_case__629(x1)(x139)(x138)(st))(st)



c__case_446 x139 x141 x142 x140 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_446_case__628(x1)(x139)(x141)(x142)(x140)(st))(st)



c__case_445 x139 x141 x142 x144 x143 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_445_case__627(x1)(x139)(x141)(x142)(x144)(x143)(st))(st)



c__case_444 x139 x141 x142 x144 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_444_case__626(x1)(x139)(x141)(x142)(x144)(st))(st)



c__case_443 x139 x141 x142 x146 x145 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_443_case__625(x1)(x139)(x141)(x142)(x146)(x145)(st))(st)



c__case_442 x139 x141 x142 x146 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_442_case__624(x1)(x139)(x141)(x142)(x146)(st))(st)



c__case_441 x139 x141 x142 x148 x147 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_441_case__623(x1)(x139)(x141)(x142)(x148)(x147)(st))(st)



c__case_440 x139 x141 x142 x148 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_440_case__622(x1)(x139)(x141)(x142)(x148)(st))(st)



c__case_439 x139 x141 x142 x150 x149 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_439_case__621(x1)(x139)(x141)(x142)(x150)(x149)(st))(st)



c__case_438 x139 x141 x142 x150 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_438_case__620(x1)(x139)(x141)(x142)(x150)(st))(st)



c__case_437 x139 x141 x142 x152 x151 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_437_case__619(x1)(x139)(x141)(x142)(x152)(x151)(st))(st)



c__case_436 x139 x141 x142 x152 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_436_case__618(x1)(x139)(x141)(x142)(x152)(st))(st)



c__case_435 x139 x141 x142 x154 x153 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_435_case__617(x1)(x139)(x141)(x142)(x154)(x153)(st))(st)



c__case_434 x139 x141 x142 x154 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_434_case__616(x1)(x139)(x141)(x142)(x154)(st))(st)



c__case_433 x139 x141 x142 x156 x155 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_433_case__615(x1)(x139)(x141)(x142)(x156)(x155)(st))(st)



c__case_432 x139 x141 x142 x156 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_432_case__614(x1)(x139)(x141)(x142)(x156)(st))(st)



c__case_431 x139 x141 x142 x158 x157 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_431_case__613(x1)(x139)(x141)(x142)(x158)(x157)(st))(st)



c__case_430 x139 x141 x142 x158 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_430_case__612(x1)(x139)(x141)(x142)(x158)(st))(st)



c__case_429 x139 x142 x141 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_429_case__611(x1)(x139)(x142)(x141)(st))(st)



c__case_428 x142 x139 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_428_case__610(x1)(x142)(x139)(st))(st)



c__case_427 x142 x159 x160 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_427_case__609(x1)(x142)(x159)(x160)(st))(st)



c__case_553 x4 x5 x33 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_553_case__608(x1)(x4)(x5)(x33)(st))(st)



c__case_552 x4 x5 x35 x34 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_552_case__607(x1)(x4)(x5)(x35)(x34)(st))(st)



c__case_551 x4 x5 x35 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_551_case__606(x1)(x4)(x5)(x35)(st))(st)



c__case_550 x4 x5 x37 x36 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_550_case__605(x1)(x4)(x5)(x37)(x36)(st))(st)



c__case_549 x4 x5 x37 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_549_case__604(x1)(x4)(x5)(x37)(st))(st)



c__case_548 x4 x5 x39 x38 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_548_case__603(x1)(x4)(x5)(x39)(x38)(st))(st)



c__case_513 x4 x5 x39 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_513_case__602(x1)(x4)(x5)(x39)(st))(st)



c__case_512 x4 x5 x75 x74 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_512_case__601(x1)(x4)(x5)(x75)(x74)(st))(st)



c__case_511 x4 x5 x75 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_511_case__600(x1)(x4)(x5)(x75)(st))(st)



c__case_510 x4 x5 x77 x76 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_510_case__599(x1)(x4)(x5)(x77)(x76)(st))(st)



c__case_509 x4 x5 x77 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_509_case__598(x1)(x4)(x5)(x77)(st))(st)



c__case_508 x4 x5 x79 x78 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_508_case__597(x1)(x4)(x5)(x79)(x78)(st))(st)



c__case_507 x4 x5 x79 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_507_case__596(x1)(x4)(x5)(x79)(st))(st)



c__case_506 x4 x5 x81 x80 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_506_case__595(x1)(x4)(x5)(x81)(x80)(st))(st)



c__case_505 x4 x5 x81 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_505_case__594(x1)(x4)(x5)(x81)(st))(st)



c__case_504 x4 x5 x83 x82 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_504_case__593(x1)(x4)(x5)(x83)(x82)(st))(st)



c__case_503 x4 x5 x83 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_503_case__592(x1)(x4)(x5)(x83)(st))(st)



c__case_502 x4 x5 x85 x84 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_502_case__591(x1)(x4)(x5)(x85)(x84)(st))(st)



c__case_501 x4 x5 x85 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_501_case__590(x1)(x4)(x5)(x85)(st))(st)



c__case_500 x4 x5 x87 x86 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_500_case__589(x1)(x4)(x5)(x87)(x86)(st))(st)



c__case_499 x4 x5 x87 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_499_case__588(x1)(x4)(x5)(x87)(st))(st)



c__case_498 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_498_case__587(x1)(x5)(x4)(st))(st)



c__case_497 x5 x89 x88 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_497_case__586(x1)(x5)(x89)(x88)(st))(st)



c__case_496 x5 x89 x91 x90 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_496_case__585(x1)(x5)(x89)(x91)(x90)(st))(st)



c__case_495 x5 x89 x91 x93 x92 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_495_case__584(x1)(x5)(x89)(x91)(x93)(x92)(st))(st)



c__case_494 x5 x89 x91 x93 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_494_case__583(x1)(x5)(x89)(x91)(x93)(st))(st)



c__case_493 x5 x89 x91 x95 x94 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_493_case__582(x1)(x5)(x89)(x91)(x95)(x94)(st))(st)



c__case_492 x5 x89 x91 x95 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_492_case__581(x1)(x5)(x89)(x91)(x95)(st))(st)



c__case_491 x5 x89 x91 x97 x96 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_491_case__580(x1)(x5)(x89)(x91)(x97)(x96)(st))(st)



c__case_490 x5 x89 x91 x97 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_490_case__579(x1)(x5)(x89)(x91)(x97)(st))(st)



c__case_489 x5 x89 x91 x99 x98 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_489_case__578(x1)(x5)(x89)(x91)(x99)(x98)(st))(st)



c__case_488 x5 x89 x91 x99 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_488_case__577(x1)(x5)(x89)(x91)(x99)(st))(st)



c__case_487 x5 x89 x91 x101 x100 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_487_case__576(x1)(x5)(x89)(x91)(x101)(x100)(st))(st)



c__case_486 x5 x89 x91 x101 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_486_case__575(x1)(x5)(x89)(x91)(x101)(st))(st)



c__case_485 x5 x89 x91 x103 x102 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_485_case__574(x1)(x5)(x89)(x91)(x103)(x102)(st))(st)



c__case_484 x5 x89 x91 x103 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_484_case__573(x1)(x5)(x89)(x91)(x103)(st))(st)



c__case_483 x5 x91 x89 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_483_case__572(x1)(x5)(x91)(x89)(st))(st)



c__case_482 x5 x91 x105 x104 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_482_case__571(x1)(x5)(x91)(x105)(x104)(st))(st)



c__case_481 x5 x91 x105 x107 x106 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_481_case__570(x1)(x5)(x91)(x105)(x107)(x106)(st))(st)



c__case_480 x5 x91 x105 x107 x109 x108 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_480_case__569(x1)(x5)(x91)(x105)(x107)(x109)(x108)(st))(st)



c__case_479 x5 x91 x105 x107 x109 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_479_case__568(x1)(x5)(x91)(x105)(x107)(x109)(st))(st)



c__case_478 x5 x91 x105 x107 x111 x110 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_478_case__567(x1)(x5)(x91)(x105)(x107)(x111)(x110)(st))(st)



c__case_477 x5 x91 x105 x107 x111 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_477_case__566(x1)(x5)(x91)(x105)(x107)(x111)(st))(st)



c__case_476 x5 x91 x105 x107 x113 x112 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_476_case__565(x1)(x5)(x91)(x105)(x107)(x113)(x112)(st))(st)



c__case_475 x5 x91 x105 x107 x113 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_475_case__564(x1)(x5)(x91)(x105)(x107)(x113)(st))(st)



c__case_474 x5 x91 x105 x107 x115 x114 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_474_case__563(x1)(x5)(x91)(x105)(x107)(x115)(x114)(st))(st)



c__case_473 x5 x91 x105 x107 x115 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_473_case__562(x1)(x5)(x91)(x105)(x107)(x115)(st))(st)



c__case_472 x5 x91 x107 x105 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_472_case__561(x1)(x5)(x91)(x107)(x105)(st))(st)



c__case_471 x5 x91 x107 x117 x116 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_471_case__560(x1)(x5)(x91)(x107)(x117)(x116)(st))(st)



c__case_470 x5 x91 x107 x117 x119 x118 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_470_case__559(x1)(x5)(x91)(x107)(x117)(x119)(x118)(st))(st)



c__case_469 x5 x91 x107 x117 x119 x121 x120 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_469_case__558(x1)(x5)(x91)(x107)(x117)(x119)(x121)(x120)(st))(st)



c__case_468 x5 x91 x107 x117 x119 x121 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_468_case__557(x1)(x5)(x91)(x107)(x117)(x119)(x121)(st))(st)



c__case_467 x5 x91 x107 x117 x119 x123 x122 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_467_case__556(x1)(x5)(x91)(x107)(x117)(x119)(x123)(x122)(st))(st)



c__case_466 x5 x91 x107 x117 x119 x123 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_466_case__555(x1)(x5)(x91)(x107)(x117)(x119)(x123)(st))(st)



c__case_465 x5 x91 x107 x117 x119 x125 x124 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_465_case__554(x1)(x5)(x91)(x107)(x117)(x119)(x125)(x124)(st))(st)



c__case_464 x5 x91 x107 x117 x119 x125 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_464_case__553(x1)(x5)(x91)(x107)(x117)(x119)(x125)(st))(st)



c__case_463 x5 x91 x107 x117 x119 x127 x126 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_463_case__552(x1)(x5)(x91)(x107)(x117)(x119)(x127)(x126)(st))(st)



c__case_462 x5 x91 x107 x117 x119 x127 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_462_case__551(x1)(x5)(x91)(x107)(x117)(x119)(x127)(st))(st)



c__case_461 x5 x91 x107 x117 x119 x129 x128 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_461_case__550(x1)(x5)(x91)(x107)(x117)(x119)(x129)(x128)(st))(st)



c__case_460 x5 x91 x107 x117 x119 x129 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_460_case__549(x1)(x5)(x91)(x107)(x117)(x119)(x129)(st))(st)



c__case_459 x5 x91 x107 x117 x119 x131 x130 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_459_case__548(x1)(x5)(x91)(x107)(x117)(x119)(x131)(x130)(st))(st)



c__case_458 x5 x91 x107 x117 x119 x131 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_458_case__547(x1)(x5)(x91)(x107)(x117)(x119)(x131)(st))(st)



c__case_457 x5 x91 x107 x117 x119 x133 x132 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_457_case__546(x1)(x5)(x91)(x107)(x117)(x119)(x133)(x132)(st))(st)



c__case_456 x5 x91 x107 x117 x119 x133 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_456_case__545(x1)(x5)(x91)(x107)(x117)(x119)(x133)(st))(st)



c__case_455 x5 x91 x107 x119 x117 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_455_case__544(x1)(x5)(x91)(x107)(x119)(x117)(st))(st)



c__case_547 x4 x5 x39 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_547_case__543(x1)(x4)(x5)(x39)(st))(st)



c__case_546 x4 x5 x41 x40 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_546_case__542(x1)(x4)(x5)(x41)(x40)(st))(st)



c__case_545 x4 x5 x41 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_545_case__541(x1)(x4)(x5)(x41)(st))(st)



c__case_544 x4 x5 x43 x42 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_544_case__540(x1)(x4)(x5)(x43)(x42)(st))(st)



c__case_543 x4 x5 x43 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_543_case__539(x1)(x4)(x5)(x43)(st))(st)



c__case_542 x4 x5 x45 x44 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_542_case__538(x1)(x4)(x5)(x45)(x44)(st))(st)



c__case_541 x4 x5 x45 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_541_case__537(x1)(x4)(x5)(x45)(st))(st)



c__case_540 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_540_case__536(x1)(x5)(x4)(st))(st)



c__case_539 x5 x47 x46 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_539_case__535(x1)(x5)(x47)(x46)(st))(st)



c__case_538 x5 x47 x49 x48 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_538_case__534(x1)(x5)(x47)(x49)(x48)(st))(st)



c__case_537 x5 x47 x49 x51 x50 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_537_case__533(x1)(x5)(x47)(x49)(x51)(x50)(st))(st)



c__case_536 x5 x47 x49 x51 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_536_case__532(x1)(x5)(x47)(x49)(x51)(st))(st)



c__case_535 x5 x47 x49 x53 x52 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_535_case__531(x1)(x5)(x47)(x49)(x53)(x52)(st))(st)



c__case_534 x5 x47 x49 x53 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_534_case__530(x1)(x5)(x47)(x49)(x53)(st))(st)



c__case_533 x5 x47 x49 x55 x54 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_533_case__529(x1)(x5)(x47)(x49)(x55)(x54)(st))(st)



c__case_532 x5 x47 x49 x55 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_532_case__528(x1)(x5)(x47)(x49)(x55)(st))(st)



c__case_531 x5 x47 x49 x57 x56 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_531_case__527(x1)(x5)(x47)(x49)(x57)(x56)(st))(st)



c__case_530 x5 x47 x49 x57 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_530_case__526(x1)(x5)(x47)(x49)(x57)(st))(st)



c__case_529 x5 x47 x49 x59 x58 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_529_case__525(x1)(x5)(x47)(x49)(x59)(x58)(st))(st)



c__case_528 x5 x47 x49 x59 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_528_case__524(x1)(x5)(x47)(x49)(x59)(st))(st)



c__case_527 x5 x47 x49 x61 x60 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_527_case__523(x1)(x5)(x47)(x49)(x61)(x60)(st))(st)



c__case_526 x5 x47 x49 x61 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_526_case__522(x1)(x5)(x47)(x49)(x61)(st))(st)



c__case_525 x5 x49 x47 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_525_case__521(x1)(x5)(x49)(x47)(st))(st)



c__case_524 x5 x49 x63 x62 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_524_case__520(x1)(x5)(x49)(x63)(x62)(st))(st)



c__case_523 x5 x49 x63 x65 x64 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_523_case__519(x1)(x5)(x49)(x63)(x65)(x64)(st))(st)



c__case_522 x5 x49 x63 x65 x67 x66 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_522_case__518(x1)(x5)(x49)(x63)(x65)(x67)(x66)(st))(st)



c__case_521 x5 x49 x63 x65 x67 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_521_case__517(x1)(x5)(x49)(x63)(x65)(x67)(st))(st)



c__case_520 x5 x49 x63 x65 x69 x68 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_520_case__516(x1)(x5)(x49)(x63)(x65)(x69)(x68)(st))(st)



c__case_519 x5 x49 x63 x65 x69 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_519_case__515(x1)(x5)(x49)(x63)(x65)(x69)(st))(st)



c__case_518 x5 x49 x63 x65 x71 x70 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_518_case__514(x1)(x5)(x49)(x63)(x65)(x71)(x70)(st))(st)



c__case_517 x5 x49 x63 x65 x71 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_517_case__513(x1)(x5)(x49)(x63)(x65)(x71)(st))(st)



c__case_516 x5 x49 x63 x65 x73 x72 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_516_case__512(x1)(x5)(x49)(x63)(x65)(x73)(x72)(st))(st)



c__case_515 x5 x49 x63 x65 x73 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_515_case__511(x1)(x5)(x49)(x63)(x65)(x73)(st))(st)



c__case_514 x5 x49 x65 x63 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_514_case__510(x1)(x5)(x49)(x65)(x63)(st))(st)



c__case_574 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_574_case__509(x1)(x4)(x5)(x7)(st))(st)



c__case_573 x4 x5 x13 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_573_case__508(x1)(x4)(x5)(x13)(x12)(st))(st)



c__case_566 x4 x5 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_566_case__507(x1)(x4)(x5)(x13)(st))(st)



c__case_565 x4 x5 x19 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_565_case__506(x1)(x4)(x5)(x19)(x18)(st))(st)



c__case_564 x4 x5 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_564_case__505(x1)(x4)(x5)(x19)(st))(st)



c__case_562 x4 x5 x24 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_562_case__504(x1)(x4)(x5)(x24)(x23)(st))(st)



c__case_561 x4 x5 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_561_case__503(x1)(x4)(x5)(x24)(st))(st)



c__case_560 x4 x5 x26 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_560_case__502(x1)(x4)(x5)(x26)(x25)(st))(st)



c__case_559 x4 x5 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_559_case__501(x1)(x4)(x5)(x26)(st))(st)



c__case_558 x4 x5 x28 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_558_case__500(x1)(x4)(x5)(x28)(x27)(st))(st)



c__case_557 x4 x5 x28 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_557_case__499(x1)(x4)(x5)(x28)(st))(st)



c__case_556 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_556_case__498(x1)(x5)(x4)(st))(st)



c__case_563 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_563_case__497(x1)(x5)(x4)(st))(st)



c__case_572 x4 x5 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_572_case__496(x1)(x4)(x5)(x13)(st))(st)



c__case_571 x4 x5 x15 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_571_case__495(x1)(x4)(x5)(x15)(x14)(st))(st)



c__case_570 x4 x5 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_570_case__494(x1)(x4)(x5)(x15)(st))(st)



c__case_569 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_569_case__493(x1)(x5)(x4)(st))(st)



c__case_568 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_568_case__492(x1)(x5)(st))(st)



c__case_567 x16 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_567_case__491(x1)(x16)(x17)(st))(st)



c__case_580 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_580_case__490(x1)(x4)(x5)(x7)(st))(st)



c__case_579 x4 x5 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_579_case__489(x1)(x4)(x5)(x9)(x8)(st))(st)



c__case_578 x4 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_578_case__488(x1)(x4)(x5)(x9)(st))(st)



c__case_577 x4 x5 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_577_case__487(x1)(x4)(x5)(x11)(x10)(st))(st)



c__case_576 x4 x5 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_576_case__486(x1)(x4)(x5)(x11)(st))(st)



c__case_575 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_575_case__485(x1)(x5)(x4)(st))(st)



c__case_592 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_592_case__484(x1)(x2)(st))(st)



c__case_591 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_591_case__483(x1)(x4)(x5)(x3)(st))(st)



c__case_590 x4 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_590_case__482(x1)(x4)(x5)(x7)(x6)(st))(st)



c__case_589 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_589_case__481(x1)(x4)(x5)(x7)(st))(st)



c__case_588 x4 x5 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_588_case__480(x1)(x4)(x5)(x9)(x8)(st))(st)



c__case_587 x4 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_587_case__479(x1)(x4)(x5)(x9)(st))(st)



c__case_586 x4 x5 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_586_case__478(x1)(x4)(x5)(x11)(x10)(st))(st)



c__case_585 x4 x5 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_585_case__477(x1)(x4)(x5)(x11)(st))(st)



c__case_584 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_584_case__476(x1)(x5)(x4)(st))(st)



c__case_642 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_642_case__475(x1)(x2)(st))(st)



c__case_641 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_641_case__474(x1)(x4)(x5)(x3)(st))(st)



c__case_640 x4 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_640_case__473(x1)(x4)(x5)(x7)(x6)(st))(st)



c__case_623 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_623_case__472(x1)(x4)(x5)(x7)(st))(st)



c__case_622 x4 x5 x23 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_622_case__471(x1)(x4)(x5)(x23)(x22)(st))(st)



c__case_621 x4 x5 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_621_case__470(x1)(x4)(x5)(x23)(st))(st)



c__case_620 x4 x5 x25 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_620_case__469(x1)(x4)(x5)(x25)(x24)(st))(st)



c__case_619 x4 x5 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_619_case__468(x1)(x4)(x5)(x25)(st))(st)



c__case_618 x4 x5 x27 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_618_case__467(x1)(x4)(x5)(x27)(x26)(st))(st)



c__case_617 x4 x5 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_617_case__466(x1)(x4)(x5)(x27)(st))(st)



c__case_616 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_616_case__465(x1)(x5)(x4)(st))(st)



c__case_615 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_615_case__464(x1)(x5)(st))(st)



c__case_614 x29 x28 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_614_case__463(x1)(x29)(x28)(st))(st)



c__case_613 x29 x31 x32 x30 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_613_case__462(x1)(x29)(x31)(x32)(x30)(st))(st)



c__case_612 x29 x31 x32 x34 x33 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_612_case__461(x1)(x29)(x31)(x32)(x34)(x33)(st))(st)



c__case_611 x29 x31 x32 x34 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_611_case__460(x1)(x29)(x31)(x32)(x34)(st))(st)



c__case_610 x29 x31 x32 x36 x35 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_610_case__459(x1)(x29)(x31)(x32)(x36)(x35)(st))(st)



c__case_609 x29 x31 x32 x36 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_609_case__458(x1)(x29)(x31)(x32)(x36)(st))(st)



c__case_608 x29 x31 x32 x38 x37 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_608_case__457(x1)(x29)(x31)(x32)(x38)(x37)(st))(st)



c__case_607 x29 x31 x32 x38 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_607_case__456(x1)(x29)(x31)(x32)(x38)(st))(st)



c__case_606 x29 x32 x31 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_606_case__455(x1)(x29)(x32)(x31)(st))(st)



c__case_605 x32 x29 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_605_case__454(x1)(x32)(x29)(st))(st)



c__case_604 x32 x40 x39 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_604_case__453(x1)(x32)(x40)(x39)(st))(st)



c__case_603 x32 x40 x42 x43 x41 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_603_case__452(x1)(x32)(x40)(x42)(x43)(x41)(st))(st)



c__case_602 x32 x40 x42 x43 x45 x44 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_602_case__451(x1)(x32)(x40)(x42)(x43)(x45)(x44)(st))(st)



c__case_601 x32 x40 x42 x43 x45 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_601_case__450(x1)(x32)(x40)(x42)(x43)(x45)(st))(st)



c__case_600 x32 x40 x42 x43 x47 x46 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_600_case__449(x1)(x32)(x40)(x42)(x43)(x47)(x46)(st))(st)



c__case_599 x32 x40 x42 x43 x47 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_599_case__448(x1)(x32)(x40)(x42)(x43)(x47)(st))(st)



c__case_598 x32 x40 x42 x43 x49 x48 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_598_case__447(x1)(x32)(x40)(x42)(x43)(x49)(x48)(st))(st)



c__case_597 x32 x40 x42 x43 x49 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_597_case__446(x1)(x32)(x40)(x42)(x43)(x49)(st))(st)



c__case_596 x32 x40 x43 x42 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_596_case__445(x1)(x32)(x40)(x43)(x42)(st))(st)



c__case_595 x32 x40 x43 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_595_case__444(x1)(x32)(x40)(x43)(st))(st)



c__case_594 x32 x40 x50 x51 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_594_case__443(x1)(x32)(x40)(x50)(x51)(st))(st)



c__case_593 x32 x50 x40 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_593_case__442(x1)(x32)(x50)(x40)(st))(st)



c__case_639 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_639_case__441(x1)(x4)(x5)(x7)(st))(st)



c__case_638 x4 x5 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_638_case__440(x1)(x4)(x5)(x9)(x8)(st))(st)



c__case_637 x4 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_637_case__439(x1)(x4)(x5)(x9)(st))(st)



c__case_636 x4 x5 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_636_case__438(x1)(x4)(x5)(x11)(x10)(st))(st)



c__case_635 x4 x5 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_635_case__437(x1)(x4)(x5)(x11)(st))(st)



c__case_634 x4 x5 x13 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_634_case__436(x1)(x4)(x5)(x13)(x12)(st))(st)



c__case_633 x4 x5 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_633_case__435(x1)(x4)(x5)(x13)(st))(st)



c__case_632 x4 x5 x15 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_632_case__434(x1)(x4)(x5)(x15)(x14)(st))(st)



c__case_631 x4 x5 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_631_case__433(x1)(x4)(x5)(x15)(st))(st)



c__case_630 x4 x5 x17 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_630_case__432(x1)(x4)(x5)(x17)(x16)(st))(st)



c__case_629 x4 x5 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_629_case__431(x1)(x4)(x5)(x17)(st))(st)



c__case_628 x4 x5 x19 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_628_case__430(x1)(x4)(x5)(x19)(x18)(st))(st)



c__case_627 x4 x5 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_627_case__429(x1)(x4)(x5)(x19)(st))(st)



c__case_626 x4 x5 x21 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_626_case__428(x1)(x4)(x5)(x21)(x20)(st))(st)



c__case_625 x4 x5 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_625_case__427(x1)(x4)(x5)(x21)(st))(st)



c__case_624 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_624_case__426(x1)(x5)(x4)(st))(st)



c__case_653 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_653_case__425(x1)(x2)(st))(st)



c__case_652 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_652_case__424(x1)(x4)(x5)(x3)(st))(st)



c__case_651 x4 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_651_case__423(x1)(x4)(x5)(x7)(x6)(st))(st)



c__case_650 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_650_case__422(x1)(x4)(x5)(x7)(st))(st)



c__case_649 x4 x5 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_649_case__421(x1)(x4)(x5)(x9)(x8)(st))(st)



c__case_648 x4 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_648_case__420(x1)(x4)(x5)(x9)(st))(st)



c__case_647 x4 x5 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_647_case__419(x1)(x4)(x5)(x11)(x10)(st))(st)



c__case_646 x4 x5 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_646_case__418(x1)(x4)(x5)(x11)(st))(st)



c__case_645 x4 x5 x13 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_645_case__417(x1)(x4)(x5)(x13)(x12)(st))(st)



c__case_644 x4 x5 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_644_case__416(x1)(x4)(x5)(x13)(st))(st)



c__case_643 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_643_case__415(x1)(x5)(x4)(st))(st)



c__case_704 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_704_case__414(x1)(x2)(st))(st)



c__case_703 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_703_case__413(x1)(x4)(x5)(x3)(st))(st)



c__case_702 x4 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_702_case__412(x1)(x4)(x5)(x7)(x6)(st))(st)



c__case_701 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_701_case__411(x1)(x4)(x5)(x7)(st))(st)



c__case_700 x4 x5 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_700_case__410(x1)(x4)(x5)(x9)(x8)(st))(st)



c__case_699 x4 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_699_case__409(x1)(x4)(x5)(x9)(st))(st)



c__case_698 x4 x5 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_698_case__408(x1)(x4)(x5)(x11)(x10)(st))(st)



c__case_697 x4 x5 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_697_case__407(x1)(x4)(x5)(x11)(st))(st)



c__case_696 x4 x5 x13 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_696_case__406(x1)(x4)(x5)(x13)(x12)(st))(st)



c__case_695 x4 x5 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_695_case__405(x1)(x4)(x5)(x13)(st))(st)



c__case_694 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_694_case__404(x1)(x5)(x4)(st))(st)



c__case_693 x5 x15 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_693_case__403(x1)(x5)(x15)(x14)(st))(st)



c__case_692 x5 x15 x17 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_692_case__402(x1)(x5)(x15)(x17)(x16)(st))(st)



c__case_691 x5 x15 x17 x19 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_691_case__401(x1)(x5)(x15)(x17)(x19)(x18)(st))(st)



c__case_690 x5 x15 x17 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_690_case__400(x1)(x5)(x15)(x17)(x19)(st))(st)



c__case_689 x5 x15 x17 x21 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_689_case__399(x1)(x5)(x15)(x17)(x21)(x20)(st))(st)



c__case_688 x5 x15 x17 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_688_case__398(x1)(x5)(x15)(x17)(x21)(st))(st)



c__case_687 x5 x15 x17 x23 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_687_case__397(x1)(x5)(x15)(x17)(x23)(x22)(st))(st)



c__case_686 x5 x15 x17 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_686_case__396(x1)(x5)(x15)(x17)(x23)(st))(st)



c__case_685 x5 x15 x17 x25 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_685_case__395(x1)(x5)(x15)(x17)(x25)(x24)(st))(st)



c__case_684 x5 x15 x17 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_684_case__394(x1)(x5)(x15)(x17)(x25)(st))(st)



c__case_683 x5 x15 x17 x27 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_683_case__393(x1)(x5)(x15)(x17)(x27)(x26)(st))(st)



c__case_682 x5 x15 x17 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_682_case__392(x1)(x5)(x15)(x17)(x27)(st))(st)



c__case_681 x5 x15 x17 x29 x28 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_681_case__391(x1)(x5)(x15)(x17)(x29)(x28)(st))(st)



c__case_680 x5 x15 x17 x29 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_680_case__390(x1)(x5)(x15)(x17)(x29)(st))(st)



c__case_679 x5 x17 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_679_case__389(x1)(x5)(x17)(x15)(st))(st)



c__case_678 x5 x17 x31 x30 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_678_case__388(x1)(x5)(x17)(x31)(x30)(st))(st)



c__case_677 x5 x17 x31 x33 x32 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_677_case__387(x1)(x5)(x17)(x31)(x33)(x32)(st))(st)



c__case_676 x5 x17 x31 x33 x35 x34 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_676_case__386(x1)(x5)(x17)(x31)(x33)(x35)(x34)(st))(st)



c__case_675 x5 x17 x31 x33 x35 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_675_case__385(x1)(x5)(x17)(x31)(x33)(x35)(st))(st)



c__case_674 x5 x17 x31 x33 x37 x36 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_674_case__384(x1)(x5)(x17)(x31)(x33)(x37)(x36)(st))(st)



c__case_673 x5 x17 x31 x33 x37 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_673_case__383(x1)(x5)(x17)(x31)(x33)(x37)(st))(st)



c__case_672 x5 x17 x31 x33 x39 x38 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_672_case__382(x1)(x5)(x17)(x31)(x33)(x39)(x38)(st))(st)



c__case_671 x5 x17 x31 x33 x39 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_671_case__381(x1)(x5)(x17)(x31)(x33)(x39)(st))(st)



c__case_670 x5 x17 x31 x33 x41 x40 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_670_case__380(x1)(x5)(x17)(x31)(x33)(x41)(x40)(st))(st)



c__case_669 x5 x17 x31 x33 x41 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_669_case__379(x1)(x5)(x17)(x31)(x33)(x41)(st))(st)



c__case_668 x5 x17 x33 x31 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_668_case__378(x1)(x5)(x17)(x33)(x31)(st))(st)



c__case_667 x5 x17 x33 x43 x42 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_667_case__377(x1)(x5)(x17)(x33)(x43)(x42)(st))(st)



c__case_666 x5 x17 x33 x43 x45 x44 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_666_case__376(x1)(x5)(x17)(x33)(x43)(x45)(x44)(st))(st)



c__case_665 x5 x17 x33 x43 x45 x47 x46 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_665_case__375(x1)(x5)(x17)(x33)(x43)(x45)(x47)(x46)(st))(st)



c__case_664 x5 x17 x33 x43 x45 x47 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_664_case__374(x1)(x5)(x17)(x33)(x43)(x45)(x47)(st))(st)



c__case_663 x5 x17 x33 x43 x45 x49 x48 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_663_case__373(x1)(x5)(x17)(x33)(x43)(x45)(x49)(x48)(st))(st)



c__case_662 x5 x17 x33 x43 x45 x49 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_662_case__372(x1)(x5)(x17)(x33)(x43)(x45)(x49)(st))(st)



c__case_661 x5 x17 x33 x43 x45 x51 x50 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_661_case__371(x1)(x5)(x17)(x33)(x43)(x45)(x51)(x50)(st))(st)



c__case_660 x5 x17 x33 x43 x45 x51 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_660_case__370(x1)(x5)(x17)(x33)(x43)(x45)(x51)(st))(st)



c__case_659 x5 x17 x33 x43 x45 x53 x52 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_659_case__369(x1)(x5)(x17)(x33)(x43)(x45)(x53)(x52)(st))(st)



c__case_658 x5 x17 x33 x43 x45 x53 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_658_case__368(x1)(x5)(x17)(x33)(x43)(x45)(x53)(st))(st)



c__case_657 x5 x17 x33 x43 x45 x55 x54 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_657_case__367(x1)(x5)(x17)(x33)(x43)(x45)(x55)(x54)(st))(st)



c__case_656 x5 x17 x33 x43 x45 x55 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_656_case__366(x1)(x5)(x17)(x33)(x43)(x45)(x55)(st))(st)



c__case_655 x5 x17 x33 x45 x43 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_655_case__365(x1)(x5)(x17)(x33)(x45)(x43)(st))(st)



c__case_654 x5 x17 x33 x45 x56 x57 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_654_case__364(x1)(x5)(x17)(x33)(x45)(x56)(x57)(st))(st)



c__case_715 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_715_case__363(x1)(x2)(st))(st)



c__case_714 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_714_case__362(x1)(x4)(x5)(x3)(st))(st)



c__case_713 x4 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_713_case__361(x1)(x4)(x5)(x7)(x6)(st))(st)



c__case_712 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_712_case__360(x1)(x4)(x5)(x7)(st))(st)



c__case_711 x4 x5 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_711_case__359(x1)(x4)(x5)(x9)(x8)(st))(st)



c__case_710 x4 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_710_case__358(x1)(x4)(x5)(x9)(st))(st)



c__case_709 x4 x5 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_709_case__357(x1)(x4)(x5)(x11)(x10)(st))(st)



c__case_708 x4 x5 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_708_case__356(x1)(x4)(x5)(x11)(st))(st)



c__case_707 x4 x5 x13 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_707_case__355(x1)(x4)(x5)(x13)(x12)(st))(st)



c__case_706 x4 x5 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_706_case__354(x1)(x4)(x5)(x13)(st))(st)



c__case_705 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_705_case__353(x1)(x5)(x4)(st))(st)



c__case_821 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_821_case__352(x1)(x2)(st))(st)



c__case_820 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_820_case__351(x1)(x4)(x5)(x3)(st))(st)



c__case_819 x4 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_819_case__350(x1)(x4)(x5)(x7)(x6)(st))(st)



c__case_818 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_818_case__349(x1)(x4)(x5)(x7)(st))(st)



c__case_817 x4 x5 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_817_case__348(x1)(x4)(x5)(x9)(x8)(st))(st)



c__case_816 x4 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_816_case__347(x1)(x4)(x5)(x9)(st))(st)



c__case_815 x4 x5 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_815_case__346(x1)(x4)(x5)(x11)(x10)(st))(st)



c__case_814 x4 x5 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_814_case__345(x1)(x4)(x5)(x11)(st))(st)



c__case_813 x4 x5 x13 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_813_case__344(x1)(x4)(x5)(x13)(x12)(st))(st)



c__case_812 x4 x5 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_812_case__343(x1)(x4)(x5)(x13)(st))(st)



c__case_767 x4 x5 x62 x61 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_767_case__342(x1)(x4)(x5)(x62)(x61)(st))(st)



c__case_766 x4 x5 x62 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_766_case__341(x1)(x4)(x5)(x62)(st))(st)



c__case_765 x4 x5 x64 x63 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_765_case__340(x1)(x4)(x5)(x64)(x63)(st))(st)



c__case_764 x4 x5 x64 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_764_case__339(x1)(x4)(x5)(x64)(st))(st)



c__case_763 x4 x5 x66 x65 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_763_case__338(x1)(x4)(x5)(x66)(x65)(st))(st)



c__case_762 x4 x5 x66 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_762_case__337(x1)(x4)(x5)(x66)(st))(st)



c__case_761 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_761_case__336(x1)(x5)(x4)(st))(st)



c__case_760 x5 x68 x67 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_760_case__335(x1)(x5)(x68)(x67)(st))(st)



c__case_759 x5 x68 x70 x69 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_759_case__334(x1)(x5)(x68)(x70)(x69)(st))(st)



c__case_758 x5 x68 x70 x72 x71 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_758_case__333(x1)(x5)(x68)(x70)(x72)(x71)(st))(st)



c__case_757 x5 x68 x70 x72 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_757_case__332(x1)(x5)(x68)(x70)(x72)(st))(st)



c__case_756 x5 x68 x70 x74 x73 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_756_case__331(x1)(x5)(x68)(x70)(x74)(x73)(st))(st)



c__case_755 x5 x68 x70 x74 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_755_case__330(x1)(x5)(x68)(x70)(x74)(st))(st)



c__case_754 x5 x68 x70 x76 x75 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_754_case__329(x1)(x5)(x68)(x70)(x76)(x75)(st))(st)



c__case_753 x5 x68 x70 x76 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_753_case__328(x1)(x5)(x68)(x70)(x76)(st))(st)



c__case_752 x5 x68 x70 x78 x77 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_752_case__327(x1)(x5)(x68)(x70)(x78)(x77)(st))(st)



c__case_751 x5 x68 x70 x78 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_751_case__326(x1)(x5)(x68)(x70)(x78)(st))(st)



c__case_750 x5 x68 x70 x80 x79 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_750_case__325(x1)(x5)(x68)(x70)(x80)(x79)(st))(st)



c__case_749 x5 x68 x70 x80 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_749_case__324(x1)(x5)(x68)(x70)(x80)(st))(st)



c__case_748 x5 x68 x70 x82 x81 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_748_case__323(x1)(x5)(x68)(x70)(x82)(x81)(st))(st)



c__case_747 x5 x68 x70 x82 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_747_case__322(x1)(x5)(x68)(x70)(x82)(st))(st)



c__case_746 x5 x70 x68 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_746_case__321(x1)(x5)(x70)(x68)(st))(st)



c__case_745 x5 x70 x84 x83 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_745_case__320(x1)(x5)(x70)(x84)(x83)(st))(st)



c__case_744 x5 x70 x84 x86 x85 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_744_case__319(x1)(x5)(x70)(x84)(x86)(x85)(st))(st)



c__case_743 x5 x70 x84 x86 x88 x87 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_743_case__318(x1)(x5)(x70)(x84)(x86)(x88)(x87)(st))(st)



c__case_742 x5 x70 x84 x86 x88 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_742_case__317(x1)(x5)(x70)(x84)(x86)(x88)(st))(st)



c__case_741 x5 x70 x84 x86 x90 x89 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_741_case__316(x1)(x5)(x70)(x84)(x86)(x90)(x89)(st))(st)



c__case_740 x5 x70 x84 x86 x90 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_740_case__315(x1)(x5)(x70)(x84)(x86)(x90)(st))(st)



c__case_739 x5 x70 x84 x86 x92 x91 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_739_case__314(x1)(x5)(x70)(x84)(x86)(x92)(x91)(st))(st)



c__case_738 x5 x70 x84 x86 x92 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_738_case__313(x1)(x5)(x70)(x84)(x86)(x92)(st))(st)



c__case_737 x5 x70 x84 x86 x94 x93 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_737_case__312(x1)(x5)(x70)(x84)(x86)(x94)(x93)(st))(st)



c__case_736 x5 x70 x84 x86 x94 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_736_case__311(x1)(x5)(x70)(x84)(x86)(x94)(st))(st)



c__case_735 x5 x70 x86 x84 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_735_case__310(x1)(x5)(x70)(x86)(x84)(st))(st)



c__case_734 x5 x70 x86 x95 x96 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_734_case__309(x1)(x5)(x70)(x86)(x95)(x96)(st))(st)



c__case_733 x70 x86 x95 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_733_case__308(x1)(x70)(x86)(x95)(x5)(st))(st)



c__case_732 x70 x86 x95 x98 x97 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_732_case__307(x1)(x70)(x86)(x95)(x98)(x97)(st))(st)



c__case_731 x70 x86 x95 x98 x100 x101 x99 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_731_case__306(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x99)(st))(st)



c__case_730 x70 x86 x95 x98 x100 x101 x103 x102 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_730_case__305(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x103)(x102)(st))(st)



c__case_729 x70 x86 x95 x98 x100 x101 x103 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_729_case__304(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x103)(st))(st)



c__case_728 x70 x86 x95 x98 x100 x101 x105 x104 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_728_case__303(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x105)(x104)(st))(st)



c__case_727 x70 x86 x95 x98 x100 x101 x105 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_727_case__302(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x105)(st))(st)



c__case_726 x70 x86 x95 x98 x100 x101 x107 x106 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_726_case__301(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x107)(x106)(st))(st)



c__case_725 x70 x86 x95 x98 x100 x101 x107 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_725_case__300(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x107)(st))(st)



c__case_724 x70 x86 x95 x98 x100 x101 x109 x108 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_724_case__299(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x109)(x108)(st))(st)



c__case_723 x70 x86 x95 x98 x100 x101 x109 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_723_case__298(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x109)(st))(st)



c__case_722 x70 x86 x95 x98 x100 x101 x111 x110 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_722_case__297(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x111)(x110)(st))(st)



c__case_721 x70 x86 x95 x98 x100 x101 x111 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_721_case__296(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x111)(st))(st)



c__case_720 x70 x86 x95 x98 x100 x101 x113 x112 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_720_case__295(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x113)(x112)(st))(st)



c__case_719 x70 x86 x95 x98 x100 x101 x113 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_719_case__294(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x113)(st))(st)



c__case_718 x70 x86 x95 x98 x101 x100 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_718_case__293(x1)(x70)(x86)(x95)(x98)(x101)(x100)(st))(st)



c__case_717 x70 x86 x95 x101 x98 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_717_case__292(x1)(x70)(x86)(x95)(x101)(x98)(st))(st)



c__case_716 x70 x86 x95 x101 x114 x115 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_716_case__291(x1)(x70)(x86)(x95)(x101)(x114)(x115)(st))(st)



c__case_811 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_811_case__290(x1)(x5)(x4)(st))(st)



c__case_810 x5 x15 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_810_case__289(x1)(x5)(x15)(x14)(st))(st)



c__case_809 x5 x15 x17 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_809_case__288(x1)(x5)(x15)(x17)(x16)(st))(st)



c__case_808 x5 x15 x17 x19 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_808_case__287(x1)(x5)(x15)(x17)(x19)(x18)(st))(st)



c__case_807 x5 x15 x17 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_807_case__286(x1)(x5)(x15)(x17)(x19)(st))(st)



c__case_806 x5 x15 x17 x21 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_806_case__285(x1)(x5)(x15)(x17)(x21)(x20)(st))(st)



c__case_805 x5 x15 x17 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_805_case__284(x1)(x5)(x15)(x17)(x21)(st))(st)



c__case_804 x5 x15 x17 x23 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_804_case__283(x1)(x5)(x15)(x17)(x23)(x22)(st))(st)



c__case_803 x5 x15 x17 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_803_case__282(x1)(x5)(x15)(x17)(x23)(st))(st)



c__case_802 x5 x15 x17 x25 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_802_case__281(x1)(x5)(x15)(x17)(x25)(x24)(st))(st)



c__case_801 x5 x15 x17 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_801_case__280(x1)(x5)(x15)(x17)(x25)(st))(st)



c__case_800 x5 x15 x17 x27 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_800_case__279(x1)(x5)(x15)(x17)(x27)(x26)(st))(st)



c__case_799 x5 x15 x17 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_799_case__278(x1)(x5)(x15)(x17)(x27)(st))(st)



c__case_798 x5 x15 x17 x29 x28 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_798_case__277(x1)(x5)(x15)(x17)(x29)(x28)(st))(st)



c__case_797 x5 x15 x17 x29 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_797_case__276(x1)(x5)(x15)(x17)(x29)(st))(st)



c__case_796 x5 x17 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_796_case__275(x1)(x5)(x17)(x15)(st))(st)



c__case_795 x5 x17 x31 x30 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_795_case__274(x1)(x5)(x17)(x31)(x30)(st))(st)



c__case_794 x5 x17 x31 x33 x32 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_794_case__273(x1)(x5)(x17)(x31)(x33)(x32)(st))(st)



c__case_793 x5 x17 x31 x33 x35 x34 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_793_case__272(x1)(x5)(x17)(x31)(x33)(x35)(x34)(st))(st)



c__case_792 x5 x17 x31 x33 x35 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_792_case__271(x1)(x5)(x17)(x31)(x33)(x35)(st))(st)



c__case_791 x5 x17 x31 x33 x37 x36 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_791_case__270(x1)(x5)(x17)(x31)(x33)(x37)(x36)(st))(st)



c__case_790 x5 x17 x31 x33 x37 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_790_case__269(x1)(x5)(x17)(x31)(x33)(x37)(st))(st)



c__case_789 x5 x17 x31 x33 x39 x38 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_789_case__268(x1)(x5)(x17)(x31)(x33)(x39)(x38)(st))(st)



c__case_788 x5 x17 x31 x33 x39 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_788_case__267(x1)(x5)(x17)(x31)(x33)(x39)(st))(st)



c__case_787 x5 x17 x31 x33 x41 x40 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_787_case__266(x1)(x5)(x17)(x31)(x33)(x41)(x40)(st))(st)



c__case_786 x5 x17 x31 x33 x41 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_786_case__265(x1)(x5)(x17)(x31)(x33)(x41)(st))(st)



c__case_785 x5 x17 x33 x31 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_785_case__264(x1)(x5)(x17)(x33)(x31)(st))(st)



c__case_784 x5 x17 x33 x42 x43 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_784_case__263(x1)(x5)(x17)(x33)(x42)(x43)(st))(st)



c__case_783 x17 x33 x42 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_783_case__262(x1)(x17)(x33)(x42)(x5)(st))(st)



c__case_782 x17 x33 x42 x45 x44 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_782_case__261(x1)(x17)(x33)(x42)(x45)(x44)(st))(st)



c__case_781 x17 x33 x42 x45 x47 x48 x46 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_781_case__260(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x46)(st))(st)



c__case_780 x17 x33 x42 x45 x47 x48 x50 x49 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_780_case__259(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x50)(x49)(st))(st)



c__case_779 x17 x33 x42 x45 x47 x48 x50 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_779_case__258(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x50)(st))(st)



c__case_778 x17 x33 x42 x45 x47 x48 x52 x51 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_778_case__257(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x52)(x51)(st))(st)



c__case_777 x17 x33 x42 x45 x47 x48 x52 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_777_case__256(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x52)(st))(st)



c__case_776 x17 x33 x42 x45 x47 x48 x54 x53 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_776_case__255(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x54)(x53)(st))(st)



c__case_775 x17 x33 x42 x45 x47 x48 x54 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_775_case__254(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x54)(st))(st)



c__case_774 x17 x33 x42 x45 x47 x48 x56 x55 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_774_case__253(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x56)(x55)(st))(st)



c__case_773 x17 x33 x42 x45 x47 x48 x56 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_773_case__252(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x56)(st))(st)



c__case_772 x17 x33 x42 x45 x47 x48 x58 x57 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_772_case__251(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x58)(x57)(st))(st)



c__case_771 x17 x33 x42 x45 x47 x48 x58 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_771_case__250(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x58)(st))(st)



c__case_770 x17 x33 x42 x45 x47 x48 x60 x59 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_770_case__249(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x60)(x59)(st))(st)



c__case_769 x17 x33 x42 x45 x47 x48 x60 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_769_case__248(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x60)(st))(st)



c__case_768 x17 x33 x42 x45 x48 x47 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_768_case__247(x1)(x17)(x33)(x42)(x45)(x48)(x47)(st))(st)



c__case_881 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_881_case__246(x1)(x2)(st))(st)



c__case_880 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_880_case__245(x1)(x4)(x5)(x3)(st))(st)



c__case_879 x4 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_879_case__244(x1)(x4)(x5)(x7)(x6)(st))(st)



c__case_878 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_878_case__243(x1)(x4)(x5)(x7)(st))(st)



c__case_877 x4 x5 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_877_case__242(x1)(x4)(x5)(x9)(x8)(st))(st)



c__case_876 x4 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_876_case__241(x1)(x4)(x5)(x9)(st))(st)



c__case_875 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_875_case__240(x1)(x5)(x4)(st))(st)



c__case_874 x5 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_874_case__239(x1)(x5)(x11)(x10)(st))(st)



c__case_873 x5 x11 x13 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_873_case__238(x1)(x5)(x11)(x13)(x12)(st))(st)



c__case_872 x5 x11 x13 x15 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_872_case__237(x1)(x5)(x11)(x13)(x15)(x14)(st))(st)



c__case_871 x5 x11 x13 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_871_case__236(x1)(x5)(x11)(x13)(x15)(st))(st)



c__case_870 x5 x11 x13 x17 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_870_case__235(x1)(x5)(x11)(x13)(x17)(x16)(st))(st)



c__case_869 x5 x11 x13 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_869_case__234(x1)(x5)(x11)(x13)(x17)(st))(st)



c__case_868 x5 x11 x13 x19 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_868_case__233(x1)(x5)(x11)(x13)(x19)(x18)(st))(st)



c__case_867 x5 x11 x13 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_867_case__232(x1)(x5)(x11)(x13)(x19)(st))(st)



c__case_866 x5 x11 x13 x21 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_866_case__231(x1)(x5)(x11)(x13)(x21)(x20)(st))(st)



c__case_865 x5 x11 x13 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_865_case__230(x1)(x5)(x11)(x13)(x21)(st))(st)



c__case_864 x5 x11 x13 x23 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_864_case__229(x1)(x5)(x11)(x13)(x23)(x22)(st))(st)



c__case_863 x5 x11 x13 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_863_case__228(x1)(x5)(x11)(x13)(x23)(st))(st)



c__case_862 x5 x11 x13 x25 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_862_case__227(x1)(x5)(x11)(x13)(x25)(x24)(st))(st)



c__case_861 x5 x11 x13 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_861_case__226(x1)(x5)(x11)(x13)(x25)(st))(st)



c__case_860 x5 x13 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_860_case__225(x1)(x5)(x13)(x11)(st))(st)



c__case_859 x5 x13 x27 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_859_case__224(x1)(x5)(x13)(x27)(x26)(st))(st)



c__case_858 x5 x13 x27 x29 x28 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_858_case__223(x1)(x5)(x13)(x27)(x29)(x28)(st))(st)



c__case_857 x5 x13 x27 x29 x31 x30 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_857_case__222(x1)(x5)(x13)(x27)(x29)(x31)(x30)(st))(st)



c__case_856 x5 x13 x27 x29 x31 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_856_case__221(x1)(x5)(x13)(x27)(x29)(x31)(st))(st)



c__case_855 x5 x13 x27 x29 x33 x32 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_855_case__220(x1)(x5)(x13)(x27)(x29)(x33)(x32)(st))(st)



c__case_854 x5 x13 x27 x29 x33 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_854_case__219(x1)(x5)(x13)(x27)(x29)(x33)(st))(st)



c__case_853 x5 x13 x27 x29 x35 x34 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_853_case__218(x1)(x5)(x13)(x27)(x29)(x35)(x34)(st))(st)



c__case_852 x5 x13 x27 x29 x35 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_852_case__217(x1)(x5)(x13)(x27)(x29)(x35)(st))(st)



c__case_851 x5 x13 x27 x29 x37 x36 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_851_case__216(x1)(x5)(x13)(x27)(x29)(x37)(x36)(st))(st)



c__case_850 x5 x13 x27 x29 x37 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_850_case__215(x1)(x5)(x13)(x27)(x29)(x37)(st))(st)



c__case_849 x5 x13 x29 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_849_case__214(x1)(x5)(x13)(x29)(x27)(st))(st)



c__case_848 x5 x13 x29 x39 x38 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_848_case__213(x1)(x5)(x13)(x29)(x39)(x38)(st))(st)



c__case_847 x5 x13 x29 x39 x41 x40 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_847_case__212(x1)(x5)(x13)(x29)(x39)(x41)(x40)(st))(st)



c__case_846 x5 x13 x29 x39 x41 x43 x42 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_846_case__211(x1)(x5)(x13)(x29)(x39)(x41)(x43)(x42)(st))(st)



c__case_845 x5 x13 x29 x39 x41 x43 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_845_case__210(x1)(x5)(x13)(x29)(x39)(x41)(x43)(st))(st)



c__case_844 x5 x13 x29 x39 x41 x45 x44 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_844_case__209(x1)(x5)(x13)(x29)(x39)(x41)(x45)(x44)(st))(st)



c__case_843 x5 x13 x29 x39 x41 x45 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_843_case__208(x1)(x5)(x13)(x29)(x39)(x41)(x45)(st))(st)



c__case_842 x5 x13 x29 x39 x41 x47 x46 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_842_case__207(x1)(x5)(x13)(x29)(x39)(x41)(x47)(x46)(st))(st)



c__case_841 x5 x13 x29 x39 x41 x47 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_841_case__206(x1)(x5)(x13)(x29)(x39)(x41)(x47)(st))(st)



c__case_840 x5 x13 x29 x39 x41 x49 x48 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_840_case__205(x1)(x5)(x13)(x29)(x39)(x41)(x49)(x48)(st))(st)



c__case_839 x5 x13 x29 x39 x41 x49 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_839_case__204(x1)(x5)(x13)(x29)(x39)(x41)(x49)(st))(st)



c__case_838 x5 x13 x29 x39 x41 x51 x50 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_838_case__203(x1)(x5)(x13)(x29)(x39)(x41)(x51)(x50)(st))(st)



c__case_837 x5 x13 x29 x39 x41 x51 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_837_case__202(x1)(x5)(x13)(x29)(x39)(x41)(x51)(st))(st)



c__case_836 x5 x13 x29 x39 x41 x53 x52 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_836_case__201(x1)(x5)(x13)(x29)(x39)(x41)(x53)(x52)(st))(st)



c__case_835 x5 x13 x29 x39 x41 x53 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_835_case__200(x1)(x5)(x13)(x29)(x39)(x41)(x53)(st))(st)



c__case_834 x5 x13 x29 x41 x39 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_834_case__199(x1)(x5)(x13)(x29)(x41)(x39)(st))(st)



c__case_833 x5 x13 x29 x41 x55 x54 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_833_case__198(x1)(x5)(x13)(x29)(x41)(x55)(x54)(st))(st)



c__case_832 x5 x13 x29 x41 x55 x57 x56 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_832_case__197(x1)(x5)(x13)(x29)(x41)(x55)(x57)(x56)(st))(st)



c__case_831 x5 x13 x29 x41 x55 x57 x59 x58 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_831_case__196(x1)(x5)(x13)(x29)(x41)(x55)(x57)(x59)(x58)(st))(st)



c__case_830 x5 x13 x29 x41 x55 x57 x59 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_830_case__195(x1)(x5)(x13)(x29)(x41)(x55)(x57)(x59)(st))(st)



c__case_829 x5 x13 x29 x41 x55 x57 x61 x60 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_829_case__194(x1)(x5)(x13)(x29)(x41)(x55)(x57)(x61)(x60)(st))(st)



c__case_828 x5 x13 x29 x41 x55 x57 x61 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_828_case__193(x1)(x5)(x13)(x29)(x41)(x55)(x57)(x61)(st))(st)



c__case_827 x5 x13 x29 x41 x55 x57 x63 x62 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_827_case__192(x1)(x5)(x13)(x29)(x41)(x55)(x57)(x63)(x62)(st))(st)



c__case_826 x5 x13 x29 x41 x55 x57 x63 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_826_case__191(x1)(x5)(x13)(x29)(x41)(x55)(x57)(x63)(st))(st)



c__case_825 x5 x13 x29 x41 x55 x57 x65 x64 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_825_case__190(x1)(x5)(x13)(x29)(x41)(x55)(x57)(x65)(x64)(st))(st)



c__case_824 x5 x13 x29 x41 x55 x57 x65 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_824_case__189(x1)(x5)(x13)(x29)(x41)(x55)(x57)(x65)(st))(st)



c__case_823 x5 x13 x29 x41 x57 x55 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_823_case__188(x1)(x5)(x13)(x29)(x41)(x57)(x55)(st))(st)



c__case_822 x13 x29 x41 x57 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_822_case__187(x1)(x13)(x29)(x41)(x57)(x5)(st))(st)



c__case_935 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_935_case__186(x1)(x2)(st))(st)



c__case_934 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_934_case__185(x1)(x4)(x5)(x3)(st))(st)



c__case_933 x4 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_933_case__184(x1)(x4)(x5)(x7)(x6)(st))(st)



c__case_932 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_932_case__183(x1)(x4)(x5)(x7)(st))(st)



c__case_931 x4 x5 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_931_case__182(x1)(x4)(x5)(x9)(x8)(st))(st)



c__case_930 x4 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_930_case__181(x1)(x4)(x5)(x9)(st))(st)



c__case_929 x4 x5 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_929_case__180(x1)(x4)(x5)(x11)(x10)(st))(st)



c__case_928 x4 x5 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_928_case__179(x1)(x4)(x5)(x11)(st))(st)



c__case_927 x4 x5 x13 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_927_case__178(x1)(x4)(x5)(x13)(x12)(st))(st)



c__case_926 x4 x5 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_926_case__177(x1)(x4)(x5)(x13)(st))(st)



c__case_925 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_925_case__176(x1)(x5)(x4)(st))(st)



c__case_924 x5 x15 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_924_case__175(x1)(x5)(x15)(x14)(st))(st)



c__case_923 x5 x15 x17 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_923_case__174(x1)(x5)(x15)(x17)(x16)(st))(st)



c__case_922 x5 x15 x17 x19 x18 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_922_case__173(x1)(x5)(x15)(x17)(x19)(x18)(st))(st)



c__case_921 x5 x15 x17 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_921_case__172(x1)(x5)(x15)(x17)(x19)(st))(st)



c__case_920 x5 x15 x17 x21 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_920_case__171(x1)(x5)(x15)(x17)(x21)(x20)(st))(st)



c__case_919 x5 x15 x17 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_919_case__170(x1)(x5)(x15)(x17)(x21)(st))(st)



c__case_918 x5 x15 x17 x23 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_918_case__169(x1)(x5)(x15)(x17)(x23)(x22)(st))(st)



c__case_917 x5 x15 x17 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_917_case__168(x1)(x5)(x15)(x17)(x23)(st))(st)



c__case_916 x5 x15 x17 x25 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_916_case__167(x1)(x5)(x15)(x17)(x25)(x24)(st))(st)



c__case_915 x5 x15 x17 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_915_case__166(x1)(x5)(x15)(x17)(x25)(st))(st)



c__case_914 x5 x15 x17 x27 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_914_case__165(x1)(x5)(x15)(x17)(x27)(x26)(st))(st)



c__case_913 x5 x15 x17 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_913_case__164(x1)(x5)(x15)(x17)(x27)(st))(st)



c__case_912 x5 x15 x17 x29 x28 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_912_case__163(x1)(x5)(x15)(x17)(x29)(x28)(st))(st)



c__case_911 x5 x15 x17 x29 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_911_case__162(x1)(x5)(x15)(x17)(x29)(st))(st)



c__case_910 x5 x17 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_910_case__161(x1)(x5)(x17)(x15)(st))(st)



c__case_909 x5 x17 x31 x30 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_909_case__160(x1)(x5)(x17)(x31)(x30)(st))(st)



c__case_908 x5 x17 x31 x33 x32 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_908_case__159(x1)(x5)(x17)(x31)(x33)(x32)(st))(st)



c__case_907 x5 x17 x31 x33 x35 x34 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_907_case__158(x1)(x5)(x17)(x31)(x33)(x35)(x34)(st))(st)



c__case_906 x5 x17 x31 x33 x35 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_906_case__157(x1)(x5)(x17)(x31)(x33)(x35)(st))(st)



c__case_905 x5 x17 x31 x33 x37 x36 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_905_case__156(x1)(x5)(x17)(x31)(x33)(x37)(x36)(st))(st)



c__case_904 x5 x17 x31 x33 x37 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_904_case__155(x1)(x5)(x17)(x31)(x33)(x37)(st))(st)



c__case_903 x5 x17 x31 x33 x39 x38 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_903_case__154(x1)(x5)(x17)(x31)(x33)(x39)(x38)(st))(st)



c__case_902 x5 x17 x31 x33 x39 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_902_case__153(x1)(x5)(x17)(x31)(x33)(x39)(st))(st)



c__case_901 x5 x17 x31 x33 x41 x40 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_901_case__152(x1)(x5)(x17)(x31)(x33)(x41)(x40)(st))(st)



c__case_900 x5 x17 x31 x33 x41 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_900_case__151(x1)(x5)(x17)(x31)(x33)(x41)(st))(st)



c__case_899 x5 x17 x33 x31 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_899_case__150(x1)(x5)(x17)(x33)(x31)(st))(st)



c__case_898 x5 x17 x33 x43 x42 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_898_case__149(x1)(x5)(x17)(x33)(x43)(x42)(st))(st)



c__case_897 x5 x17 x33 x43 x45 x44 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_897_case__148(x1)(x5)(x17)(x33)(x43)(x45)(x44)(st))(st)



c__case_896 x5 x17 x33 x43 x45 x47 x46 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_896_case__147(x1)(x5)(x17)(x33)(x43)(x45)(x47)(x46)(st))(st)



c__case_895 x5 x17 x33 x43 x45 x47 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_895_case__146(x1)(x5)(x17)(x33)(x43)(x45)(x47)(st))(st)



c__case_894 x5 x17 x33 x43 x45 x49 x48 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_894_case__145(x1)(x5)(x17)(x33)(x43)(x45)(x49)(x48)(st))(st)



c__case_893 x5 x17 x33 x43 x45 x49 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_893_case__144(x1)(x5)(x17)(x33)(x43)(x45)(x49)(st))(st)



c__case_892 x5 x17 x33 x43 x45 x51 x50 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_892_case__143(x1)(x5)(x17)(x33)(x43)(x45)(x51)(x50)(st))(st)



c__case_891 x5 x17 x33 x43 x45 x51 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_891_case__142(x1)(x5)(x17)(x33)(x43)(x45)(x51)(st))(st)



c__case_890 x5 x17 x33 x43 x45 x53 x52 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_890_case__141(x1)(x5)(x17)(x33)(x43)(x45)(x53)(x52)(st))(st)



c__case_889 x5 x17 x33 x43 x45 x53 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_889_case__140(x1)(x5)(x17)(x33)(x43)(x45)(x53)(st))(st)



c__case_888 x5 x17 x33 x43 x45 x55 x54 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_888_case__139(x1)(x5)(x17)(x33)(x43)(x45)(x55)(x54)(st))(st)



c__case_887 x5 x17 x33 x43 x45 x55 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_887_case__138(x1)(x5)(x17)(x33)(x43)(x45)(x55)(st))(st)



c__case_886 x5 x17 x33 x45 x43 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_886_case__137(x1)(x5)(x17)(x33)(x45)(x43)(st))(st)



c__case_885 x5 x17 x33 x45 x56 x57 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_885_case__136(x1)(x5)(x17)(x33)(x45)(x56)(x57)(st))(st)



c__case_884 x17 x33 x45 x56 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_884_case__135(x1)(x17)(x33)(x45)(x56)(x5)(st))(st)



c__case_883 x17 x33 x45 x56 x58 x59 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_883_case__134(x1)(x17)(x33)(x45)(x56)(x58)(x59)(st))(st)



c__case_882 x17 x33 x45 x56 x58 x60 x61 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_882_case__133(x1)(x17)(x33)(x45)(x56)(x58)(x60)(x61)(st))(st)



c__case_950 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_950_case__132(x1)(x2)(st))(st)



c__case_949 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_949_case__131(x1)(x4)(x5)(x3)(st))(st)



c__case_948 x4 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_948_case__130(x1)(x4)(x5)(x7)(x6)(st))(st)



c__case_947 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_947_case__129(x1)(x4)(x5)(x7)(st))(st)



c__case_946 x4 x5 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_946_case__128(x1)(x4)(x5)(x9)(x8)(st))(st)



c__case_945 x4 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_945_case__127(x1)(x4)(x5)(x9)(st))(st)



c__case_944 x4 x5 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_944_case__126(x1)(x4)(x5)(x11)(x10)(st))(st)



c__case_943 x4 x5 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_943_case__125(x1)(x4)(x5)(x11)(st))(st)



c__case_942 x4 x5 x13 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_942_case__124(x1)(x4)(x5)(x13)(x12)(st))(st)



c__case_941 x4 x5 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_941_case__123(x1)(x4)(x5)(x13)(st))(st)



c__case_940 x4 x5 x15 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_940_case__122(x1)(x4)(x5)(x15)(x14)(st))(st)



c__case_939 x4 x5 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_939_case__121(x1)(x4)(x5)(x15)(st))(st)



c__case_938 x4 x5 x17 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_938_case__120(x1)(x4)(x5)(x17)(x16)(st))(st)



c__case_937 x4 x5 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_937_case__119(x1)(x4)(x5)(x17)(st))(st)



c__case_936 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_936_case__118(x1)(x5)(x4)(st))(st)



c__case_1052 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1052_case__117(x1)(x2)(st))(st)



c__case_1051 x4 x5 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1051_case__116(x1)(x4)(x5)(x3)(st))(st)



c__case_1050 x4 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1050_case__115(x1)(x4)(x5)(x7)(x6)(st))(st)



c__case_1049 x4 x5 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1049_case__114(x1)(x4)(x5)(x7)(st))(st)



c__case_1048 x4 x5 x9 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1048_case__113(x1)(x4)(x5)(x9)(x8)(st))(st)



c__case_1047 x4 x5 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1047_case__112(x1)(x4)(x5)(x9)(st))(st)



c__case_1046 x4 x5 x11 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1046_case__111(x1)(x4)(x5)(x11)(x10)(st))(st)



c__case_1045 x4 x5 x11 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1045_case__110(x1)(x4)(x5)(x11)(st))(st)



c__case_1044 x4 x5 x13 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1044_case__109(x1)(x4)(x5)(x13)(x12)(st))(st)



c__case_1043 x4 x5 x13 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1043_case__108(x1)(x4)(x5)(x13)(st))(st)



c__case_1042 x5 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1042_case__107(x1)(x5)(x4)(st))(st)



c__case_1041 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1041_case__106(x1)(x5)(st))(st)



c__case_1040 x15 x14 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1040_case__105(x1)(x15)(x14)(st))(st)



c__case_1039 x15 x17 x18 x16 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1039_case__104(x1)(x15)(x17)(x18)(x16)(st))(st)



c__case_1038 x15 x17 x18 x20 x19 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1038_case__103(x1)(x15)(x17)(x18)(x20)(x19)(st))(st)



c__case_1037 x15 x17 x18 x20 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1037_case__102(x1)(x15)(x17)(x18)(x20)(st))(st)



c__case_1036 x15 x17 x18 x22 x21 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1036_case__101(x1)(x15)(x17)(x18)(x22)(x21)(st))(st)



c__case_1035 x15 x17 x18 x22 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1035_case__100(x1)(x15)(x17)(x18)(x22)(st))(st)



c__case_1034 x15 x17 x18 x24 x23 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1034_case__99(x1)(x15)(x17)(x18)(x24)(x23)(st))(st)



c__case_1033 x15 x17 x18 x24 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1033_case__98(x1)(x15)(x17)(x18)(x24)(st))(st)



c__case_1032 x15 x17 x18 x26 x25 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1032_case__97(x1)(x15)(x17)(x18)(x26)(x25)(st))(st)



c__case_1031 x15 x17 x18 x26 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1031_case__96(x1)(x15)(x17)(x18)(x26)(st))(st)



c__case_1030 x15 x17 x18 x28 x27 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1030_case__95(x1)(x15)(x17)(x18)(x28)(x27)(st))(st)



c__case_1029 x15 x17 x18 x28 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1029_case__94(x1)(x15)(x17)(x18)(x28)(st))(st)



c__case_1028 x15 x17 x18 x30 x29 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1028_case__93(x1)(x15)(x17)(x18)(x30)(x29)(st))(st)



c__case_1027 x15 x17 x18 x30 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1027_case__92(x1)(x15)(x17)(x18)(x30)(st))(st)



c__case_1026 x15 x18 x17 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1026_case__91(x1)(x15)(x18)(x17)(st))(st)



c__case_1025 x18 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1025_case__90(x1)(x18)(x15)(st))(st)



c__case_1024 x18 x32 x31 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1024_case__89(x1)(x18)(x32)(x31)(st))(st)



c__case_1023 x18 x32 x34 x35 x33 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1023_case__88(x1)(x18)(x32)(x34)(x35)(x33)(st))(st)



c__case_1022 x18 x32 x34 x35 x37 x36 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1022_case__87(x1)(x18)(x32)(x34)(x35)(x37)(x36)(st))(st)



c__case_1021 x18 x32 x34 x35 x37 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1021_case__86(x1)(x18)(x32)(x34)(x35)(x37)(st))(st)



c__case_1020 x18 x32 x34 x35 x39 x38 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1020_case__85(x1)(x18)(x32)(x34)(x35)(x39)(x38)(st))(st)



c__case_1019 x18 x32 x34 x35 x39 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1019_case__84(x1)(x18)(x32)(x34)(x35)(x39)(st))(st)



c__case_1018 x18 x32 x34 x35 x41 x40 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1018_case__83(x1)(x18)(x32)(x34)(x35)(x41)(x40)(st))(st)



c__case_1017 x18 x32 x34 x35 x41 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1017_case__82(x1)(x18)(x32)(x34)(x35)(x41)(st))(st)



c__case_1016 x18 x32 x34 x35 x43 x42 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1016_case__81(x1)(x18)(x32)(x34)(x35)(x43)(x42)(st))(st)



c__case_1015 x18 x32 x34 x35 x43 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1015_case__80(x1)(x18)(x32)(x34)(x35)(x43)(st))(st)



c__case_1014 x18 x32 x34 x35 x45 x44 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1014_case__79(x1)(x18)(x32)(x34)(x35)(x45)(x44)(st))(st)



c__case_1013 x18 x32 x34 x35 x45 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1013_case__78(x1)(x18)(x32)(x34)(x35)(x45)(st))(st)



c__case_1012 x18 x32 x34 x35 x47 x46 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1012_case__77(x1)(x18)(x32)(x34)(x35)(x47)(x46)(st))(st)



c__case_1011 x18 x32 x34 x35 x47 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1011_case__76(x1)(x18)(x32)(x34)(x35)(x47)(st))(st)



c__case_1010 x18 x32 x35 x34 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1010_case__75(x1)(x18)(x32)(x35)(x34)(st))(st)



c__case_1009 x18 x35 x32 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1009_case__74(x1)(x18)(x35)(x32)(st))(st)



c__case_1008 x18 x35 x49 x48 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1008_case__73(x1)(x18)(x35)(x49)(x48)(st))(st)



c__case_1007 x18 x35 x49 x51 x52 x50 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1007_case__72(x1)(x18)(x35)(x49)(x51)(x52)(x50)(st))(st)



c__case_1006 x18 x35 x49 x51 x52 x54 x53 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1006_case__71(x1)(x18)(x35)(x49)(x51)(x52)(x54)(x53)(st))(st)



c__case_1005 x18 x35 x49 x51 x52 x54 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1005_case__70(x1)(x18)(x35)(x49)(x51)(x52)(x54)(st))(st)



c__case_1004 x18 x35 x49 x51 x52 x56 x55 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1004_case__69(x1)(x18)(x35)(x49)(x51)(x52)(x56)(x55)(st))(st)



c__case_1003 x18 x35 x49 x51 x52 x56 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1003_case__68(x1)(x18)(x35)(x49)(x51)(x52)(x56)(st))(st)



c__case_1002 x18 x35 x49 x51 x52 x58 x57 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1002_case__67(x1)(x18)(x35)(x49)(x51)(x52)(x58)(x57)(st))(st)



c__case_1001 x18 x35 x49 x51 x52 x58 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1001_case__66(x1)(x18)(x35)(x49)(x51)(x52)(x58)(st))(st)



c__case_1000 x18 x35 x49 x51 x52 x60 x59 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1000_case__65(x1)(x18)(x35)(x49)(x51)(x52)(x60)(x59)(st))(st)



c__case_999 x18 x35 x49 x51 x52 x60 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_999_case__64(x1)(x18)(x35)(x49)(x51)(x52)(x60)(st))(st)



c__case_998 x18 x35 x49 x51 x52 x62 x61 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_998_case__63(x1)(x18)(x35)(x49)(x51)(x52)(x62)(x61)(st))(st)



c__case_997 x18 x35 x49 x51 x52 x62 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_997_case__62(x1)(x18)(x35)(x49)(x51)(x52)(x62)(st))(st)



c__case_996 x18 x35 x49 x52 x51 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_996_case__61(x1)(x18)(x35)(x49)(x52)(x51)(st))(st)



c__case_995 x18 x35 x52 x49 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_995_case__60(x1)(x18)(x35)(x52)(x49)(st))(st)



c__case_994 x18 x35 x52 x64 x63 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_994_case__59(x1)(x18)(x35)(x52)(x64)(x63)(st))(st)



c__case_993 x18 x35 x52 x64 x66 x67 x65 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_993_case__58(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x65)(st))(st)



c__case_992 x18 x35 x52 x64 x66 x67 x69 x68 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_992_case__57(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x69)(x68)(st))(st)



c__case_991 x18 x35 x52 x64 x66 x67 x69 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_991_case__56(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x69)(st))(st)



c__case_990 x18 x35 x52 x64 x66 x67 x71 x70 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_990_case__55(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x71)(x70)(st))(st)



c__case_989 x18 x35 x52 x64 x66 x67 x71 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_989_case__54(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x71)(st))(st)



c__case_988 x18 x35 x52 x64 x66 x67 x73 x72 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_988_case__53(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x73)(x72)(st))(st)



c__case_987 x18 x35 x52 x64 x66 x67 x73 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_987_case__52(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x73)(st))(st)



c__case_986 x18 x35 x52 x64 x66 x67 x75 x74 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_986_case__51(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x75)(x74)(st))(st)



c__case_985 x18 x35 x52 x64 x66 x67 x75 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_985_case__50(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x75)(st))(st)



c__case_984 x18 x35 x52 x64 x66 x67 x77 x76 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_984_case__49(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x77)(x76)(st))(st)



c__case_983 x18 x35 x52 x64 x66 x67 x77 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_983_case__48(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x77)(st))(st)



c__case_982 x18 x35 x52 x64 x66 x67 x79 x78 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_982_case__47(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x79)(x78)(st))(st)



c__case_981 x18 x35 x52 x64 x66 x67 x79 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_981_case__46(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x79)(st))(st)



c__case_980 x18 x35 x52 x64 x66 x67 x81 x80 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_980_case__45(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x81)(x80)(st))(st)



c__case_979 x18 x35 x52 x64 x66 x67 x81 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_979_case__44(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x81)(st))(st)



c__case_978 x18 x35 x52 x64 x66 x67 x83 x82 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_978_case__43(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x83)(x82)(st))(st)



c__case_977 x18 x35 x52 x64 x66 x67 x83 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_977_case__42(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x83)(st))(st)



c__case_976 x18 x35 x52 x64 x66 x67 x85 x84 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_976_case__41(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x85)(x84)(st))(st)



c__case_975 x18 x35 x52 x64 x66 x67 x85 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_975_case__40(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x85)(st))(st)



c__case_974 x18 x35 x52 x64 x67 x66 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_974_case__39(x1)(x18)(x35)(x52)(x64)(x67)(x66)(st))(st)



c__case_973 x18 x35 x52 x67 x64 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_973_case__38(x1)(x18)(x35)(x52)(x67)(x64)(st))(st)



c__case_972 x18 x35 x52 x67 x87 x86 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_972_case__37(x1)(x18)(x35)(x52)(x67)(x87)(x86)(st))(st)



c__case_971 x18 x35 x52 x67 x87 x89 x90 x88 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_971_case__36(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x88)(st))(st)



c__case_970 x18 x35 x52 x67 x87 x89 x90 x92 x91 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_970_case__35(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x92)(x91)(st))(st)



c__case_969 x18 x35 x52 x67 x87 x89 x90 x92 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_969_case__34(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x92)(st))(st)



c__case_968 x18 x35 x52 x67 x87 x89 x90 x94 x93 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_968_case__33(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x94)(x93)(st))(st)



c__case_967 x18 x35 x52 x67 x87 x89 x90 x94 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_967_case__32(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x94)(st))(st)



c__case_966 x18 x35 x52 x67 x87 x89 x90 x96 x95 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_966_case__31(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x96)(x95)(st))(st)



c__case_965 x18 x35 x52 x67 x87 x89 x90 x96 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_965_case__30(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x96)(st))(st)



c__case_964 x18 x35 x52 x67 x87 x89 x90 x98 x97 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_964_case__29(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x98)(x97)(st))(st)



c__case_963 x18 x35 x52 x67 x87 x89 x90 x98 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_963_case__28(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x98)(st))(st)



c__case_962 x18 x35 x52 x67 x87 x89 x90 x100 x99 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_962_case__27(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x100)(x99)(st))(st)



c__case_961 x18 x35 x52 x67 x87 x89 x90 x100 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_961_case__26(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x100)(st))(st)



c__case_960 x18 x35 x52 x67 x87 x89 x90 x102 x101 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_960_case__25(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x102)(x101)(st))(st)



c__case_959 x18 x35 x52 x67 x87 x89 x90 x102 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_959_case__24(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x102)(st))(st)



c__case_958 x18 x35 x52 x67 x87 x89 x90 x104 x103 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_958_case__23(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x104)(x103)(st))(st)



c__case_957 x18 x35 x52 x67 x87 x89 x90 x104 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_957_case__22(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x104)(st))(st)



c__case_956 x18 x35 x52 x67 x87 x89 x90 x106 x105 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_956_case__21(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x106)(x105)(st))(st)



c__case_955 x18 x35 x52 x67 x87 x89 x90 x106 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_955_case__20(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x106)(st))(st)



c__case_954 x18 x35 x52 x67 x87 x89 x90 x108 x107 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_954_case__19(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x108)(x107)(st))(st)



c__case_953 x18 x35 x52 x67 x87 x89 x90 x108 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_953_case__18(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x108)(st))(st)



c__case_952 x18 x35 x52 x67 x87 x90 x89 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_952_case__17(x1)(x18)(x35)(x52)(x67)(x87)(x90)(x89)(st))(st)



c__case_951 x18 x35 x52 x67 x90 x87 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_951_case__16(x1)(x18)(x35)(x52)(x67)(x90)(x87)(st))(st)



c__case_1053 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1053_case__15(x1)(x2)(st))(st)



c__case_1055 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1055_case__14(x1)(x2)(st))(st)



c__case_1054 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1054_case__13(x1)(x4)(x3)(st))(st)



c__case_1056 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1056_case__12(x1)(x2)(st))(st)



c__case_1057 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1057_case__11(x1)(x2)(st))(st)



c__case_1060 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1060_case__10(x1)(x2)(st))(st)



c__case_1058 x14 x15 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1058_case__9(x1)(x15)(st))(st)



c__case_1059 x6 x7 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1059_case__8(x1)(x6)(x7)(x5)(st))(st)



c__case_1061 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1061_case__7(x1)(x2)(st))(st)



c__case_1062 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1062_case__6(x1)(x2)(st))(st)



c__case_1063 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1063_case__5(x1)(x2)(st))(st)



c__case_1064 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1064_case__4(x1)(x2)(st))(st)



c__case_1065 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1065_case__3(x1)(x2)(st))(st)



c__case_1066 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1066_case__2(x1)(x2)(st))(st)



c__case_1067 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1067_case__1(x1)(x2)(st))(st)



c__case_1068 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1068_case__0(x1)(x2)(st))(st)



c__case_1068_case__0 x1 x2@(Curry.Module.FlatCurry.C_Prog x3 x4 x5 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List)))))))))))(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))(Curry.Module.Prelude.List)))))((Curry.Module.Prelude.:<)(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)(Curry.Module.OracleXML.c_xtxt(x3)(x1)(st))(Curry.Module.Prelude.List))(x8)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_flatCurry2Xml'46_'35lambda2))))(x4)(x9)(st))(x10)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xmlShowType))))(x5)(x11)(st))(x12)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))))))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xmlShowFunc))))(x6)(x13)(st))(x14)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))))))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xmlShowOp))))(x7)(x15)(st))(x16)(st))(Curry.Module.Prelude.List))))))(x17)(st))(st)
c__case_1068_case__0 x1 (Curry.Module.FlatCurry.C_ProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1068_case__0(x1)(x)(st))(i)(xs)(st)
c__case_1068_case__0 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1068_case__0")(x)



c__case_1067_case__1 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))(x3))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))(x4))(Curry.Module.Prelude.List)))(st)
c__case_1067_case__1 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1067_case__1(x1)(x)(st))(i)(xs)(st)
c__case_1067_case__1 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1067_case__1")(x)



c__case_1066_case__2 x1 x2@Curry.Module.FlatCurry.C_Public st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))(Curry.Module.Prelude.List))))))))(Curry.Module.Prelude.List))(st)
c__case_1066_case__2 x1 x2@Curry.Module.FlatCurry.C_Private st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))))(Curry.Module.Prelude.List))(st)
c__case_1066_case__2 x1 (Curry.Module.FlatCurry.C_VisibilityOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1066_case__2(x1)(x)(st))(i)(xs)(st)
c__case_1066_case__2 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1066_case__2")(x)



c__case_1065_case__3 x1 x2@(Curry.Module.FlatCurry.C_Type x3 x4 x5 x6) st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List)))))))(Curry.Module.XML.C_XElem((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryXML.c_qname2xmlattrs(x3)(x1)(st))(Curry.Module.OracleFlatCurryXML.c_xmlShowVisibity(x4)(x11)(st))(x12)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xmlShowTVar))))(x5)(x13)(st))(x14)(st))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xmlShowCons))))(x6)(x15)(st))(x16)(st)))(st)
c__case_1065_case__3 x1 x2@(Curry.Module.FlatCurry.C_TypeSyn x7 x8 x9 x10) st = let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)((Curry.Module.Prelude.:<)(x19)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)(Curry.Module.Prelude.List))))))(Curry.Module.XML.C_XElem((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryXML.c_qname2xmlattrs(x7)(x1)(st))(Curry.Module.OracleFlatCurryXML.c_xmlShowVisibity(x8)(x17)(st))(x18)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xmlShowTVar))))(x9)(x19)(st))(x20)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleFlatCurryXML.c_xmlShowTypeExpr(x10)(x21)(st))(Curry.Module.Prelude.List))))(st)
c__case_1065_case__3 x1 (Curry.Module.FlatCurry.C_TypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1065_case__3(x1)(x)(st))(i)(xs)(st)
c__case_1065_case__3 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1065_case__3")(x)



c__case_1064_case__4 x1 x2@(Curry.Module.FlatCurry.C_Cons x3 x4 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))))))(Curry.Module.XML.C_XElem((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryXML.c_qname2xmlattrs(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.c_show(x4)(x7)(st)))(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryXML.c_xmlShowVisibity(x5)(x8)(st))(x9)(st))(x10)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xmlShowTypeExpr))))(x6)(x11)(st)))(st)
c__case_1064_case__4 x1 (Curry.Module.FlatCurry.C_ConsDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1064_case__4(x1)(x)(st))(i)(xs)(st)
c__case_1064_case__4 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1064_case__4")(x)



c__case_1063_case__5 x1 x2@(Curry.Module.FlatCurry.C_FuncType x3 x4) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))))((Curry.Module.Prelude.:<)(Curry.Module.OracleFlatCurryXML.c_xmlShowTypeExpr(x3)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleFlatCurryXML.c_xmlShowTypeExpr(x4)(x8)(st))(Curry.Module.Prelude.List)))(x9)(st))(st)
c__case_1063_case__5 x1 x2@(Curry.Module.FlatCurry.C_TCons x5 x6) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.XML.C_XElem((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))(Curry.Module.OracleFlatCurryXML.c_qname2xmlattrs(x5)(x1)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xmlShowTypeExpr))))(x6)(x10)(st)))(st)
c__case_1063_case__5 x1 x2@(Curry.Module.FlatCurry.C_TVar x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c_xmlShowTVar(x7)(x1)(st))(st)
c__case_1063_case__5 x1 (Curry.Module.FlatCurry.C_TypeExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1063_case__5(x1)(x)(st))(i)(xs)(st)
c__case_1063_case__5 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1063_case__5")(x)



c__case_1062_case__6 x1 x2@(Curry.Module.FlatCurry.C_Func x3 x4 x5 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List)))))))(Curry.Module.XML.C_XElem((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryXML.c_qname2xmlattrs(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.c_show(x4)(x8)(st)))(Curry.Module.Prelude.List))(Curry.Module.OracleFlatCurryXML.c_xmlShowVisibity(x5)(x9)(st))(x10)(st))(x11)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleFlatCurryXML.c_xmlShowTypeExpr(x6)(x12)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleFlatCurryXML.c_xmlShowRule(x7)(x13)(st))(Curry.Module.Prelude.List))))(st)
c__case_1062_case__6 x1 (Curry.Module.FlatCurry.C_FuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1062_case__6(x1)(x)(st))(i)(xs)(st)
c__case_1062_case__6 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1062_case__6")(x)



c__case_1061_case__7 x1 x2@(Curry.Module.FlatCurry.C_Rule x3 x4) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))))(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))((Curry.Module.Prelude.:<)(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xmlShowVar))))(x3)(x1)(st))(x6)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)(Curry.Module.OracleFlatCurryXML.c_xmlShowExpr(x4)(x7)(st))(Curry.Module.Prelude.List))(x8)(st))(Curry.Module.Prelude.List)))(x9)(st))(st)
c__case_1061_case__7 x1 x2@(Curry.Module.FlatCurry.C_External x5) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))))))((Curry.Module.Prelude.:<)(Curry.Module.OracleXML.c_xtxt(x5)(x1)(st))(Curry.Module.Prelude.List))(x10)(st))(st)
c__case_1061_case__7 x1 (Curry.Module.FlatCurry.C_RuleOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1061_case__7(x1)(x)(st))(i)(xs)(st)
c__case_1061_case__7 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1061_case__7")(x)



c__case_1059_case__8 x1 x6 x7 x5@Curry.Module.FlatCurry.C_FuncCall st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(Curry.Module.XML.C_XElem((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))))))(Curry.Module.OracleFlatCurryXML.c_qname2xmlattrs(x6)(x1)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xmlShowExpr))))(x7)(x10)(st)))(st)
c__case_1059_case__8 x1 x6 x7 x5@Curry.Module.FlatCurry.C_ConsCall st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.XML.C_XElem((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))))))(Curry.Module.OracleFlatCurryXML.c_qname2xmlattrs(x6)(x1)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xmlShowExpr))))(x7)(x11)(st)))(st)
c__case_1059_case__8 x1 x6 x7 x5@(Curry.Module.FlatCurry.C_FuncPartCall x8) st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))))(Curry.Module.XML.C_XElem((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryXML.c_qname2xmlattrs(x6)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.c_show(x8)(x12)(st)))(Curry.Module.Prelude.List))(x13)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xmlShowExpr))))(x7)(x14)(st)))(st)
c__case_1059_case__8 x1 x6 x7 x5@(Curry.Module.FlatCurry.C_ConsPartCall x9) st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))))(Curry.Module.XML.C_XElem((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List)))))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryXML.c_qname2xmlattrs(x6)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))(Curry.Module.Prelude.List))))))))(Curry.Module.OraclePrelude.c_show(x9)(x15)(st)))(Curry.Module.Prelude.List))(x16)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xmlShowExpr))))(x7)(x17)(st)))(st)
c__case_1059_case__8 x1 x6 x7 (Curry.Module.FlatCurry.C_CombTypeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1059_case__8(x1)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_1059_case__8 x1 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1059_case__8")(x)



c__case_1058_case__9 x1 x15@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))(st)
c__case_1058_case__9 x1 x15@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_collapse(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))(st)
c__case_1058_case__9 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1058_case__9(x1)(x)(st))(i)(xs)(st)
c__case_1058_case__9 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1058_case__9")(x)



c__case_1060_case__10 x1 x2@(Curry.Module.FlatCurry.C_Var x3) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c_xmlShowVar(x3)(x1)(st))(st)
c__case_1060_case__10 x1 x2@(Curry.Module.FlatCurry.C_Lit x4) st = let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))((Curry.Module.Prelude.:<)(Curry.Module.OracleFlatCurryXML.c_xmlShowLit(x4)(x1)(st))(Curry.Module.Prelude.List))(x19)(st))(st)
c__case_1060_case__10 x1 x2@(Curry.Module.FlatCurry.C_Comb x5 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1059(x6)(x7)(x5)(x1)(st))(st)
c__case_1060_case__10 x1 x2@(Curry.Module.FlatCurry.C_Free x10 x11) st = let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x21 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x22 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x20)((Curry.Module.Prelude.:<)(x21)((Curry.Module.Prelude.:<)(x22)(Curry.Module.Prelude.List))))(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))((Curry.Module.Prelude.:<)(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))))))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xmlShowVar))))(x10)(x1)(st))(x20)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleFlatCurryXML.c_xmlShowExpr(x11)(x21)(st))(Curry.Module.Prelude.List)))(x22)(st))(st)
c__case_1060_case__10 x1 x2@(Curry.Module.FlatCurry.C_Or x12 x13) st = let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x23)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List)))(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List)))((Curry.Module.Prelude.:<)(Curry.Module.OracleFlatCurryXML.c_xmlShowExpr(x12)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleFlatCurryXML.c_xmlShowExpr(x13)(x23)(st))(Curry.Module.Prelude.List)))(x24)(st))(st)
c__case_1060_case__10 x1 x2@(Curry.Module.FlatCurry.C_Case x14 x15 x16) st = let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x26 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x27 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x28 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x25)((Curry.Module.Prelude.:<)(x26)((Curry.Module.Prelude.:<)(x27)((Curry.Module.Prelude.:<)(x28)(Curry.Module.Prelude.List)))))(Curry.Module.XML.C_XElem(Curry.Module.OracleFlatCurryXML.c__case_1058(x14)(Curry.Module.OraclePrelude.op_61_61(x14)(Curry.Module.FlatCurry.C_Flex)(x1)(st))(x25)(st))(Curry.Module.Prelude.List)(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.OracleFlatCurryXML.c_xmlShowExpr(x15)(x26)(st))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xmlShowBranch))))(x16)(x27)(st))(x28)(st)))(st)
c__case_1060_case__10 x1 x2@(Curry.Module.FlatCurry.C_Let x17 x18) st = let {x29 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x30 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x31 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x29)((Curry.Module.Prelude.:<)(x30)((Curry.Module.Prelude.:<)(x31)(Curry.Module.Prelude.List))))(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xmlShowExpr'46_'35lambda3))))(x17)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleFlatCurryXML.c_xmlShowExpr(x18)(x29)(st))(Curry.Module.Prelude.List))(x30)(st))(x31)(st))(st)
c__case_1060_case__10 x1 (Curry.Module.FlatCurry.C_ExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1060_case__10(x1)(x)(st))(i)(xs)(st)
c__case_1060_case__10 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1060_case__10")(x)



c__case_1057_case__11 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)(Curry.Module.OracleFlatCurryXML.c_xmlShowVar(x3)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleFlatCurryXML.c_xmlShowExpr(x4)(x5)(st))(Curry.Module.Prelude.List)))(x6)(st))(st)
c__case_1057_case__11 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1057_case__11(x1)(x)(st))(i)(xs)(st)
c__case_1057_case__11 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1057_case__11")(x)



c__case_1056_case__12 x1 x2@(Curry.Module.FlatCurry.C_Intc x3) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))(Curry.Module.Prelude.List)))))((Curry.Module.Prelude.:<)(Curry.Module.OracleXML.c_xtxt(Curry.Module.OraclePrelude.c_show(x3)(x1)(st))(x6)(st))(Curry.Module.Prelude.List))(x7)(st))(st)
c__case_1056_case__12 x1 x2@(Curry.Module.FlatCurry.C_Floatc x4) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)(Curry.Module.OracleXML.c_xtxt(Curry.Module.OraclePrelude.c_show(x4)(x1)(st))(x8)(st))(Curry.Module.Prelude.List))(x9)(st))(st)
c__case_1056_case__12 x1 x2@(Curry.Module.FlatCurry.C_Charc x5) st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))))(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))(Curry.Module.Prelude.List))))))((Curry.Module.Prelude.:<)(Curry.Module.OracleXML.c_xtxt(Curry.Module.OraclePrelude.c_show(Curry.Module.OraclePrelude.c_ord(x5)(x1)(st))(x10)(st))(x11)(st))(Curry.Module.Prelude.List))(x12)(st))(st)
c__case_1056_case__12 x1 (Curry.Module.FlatCurry.C_LiteralOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1056_case__12(x1)(x)(st))(i)(xs)(st)
c__case_1056_case__12 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1056_case__12")(x)



c__case_1054_case__13 x1 x4 x3@(Curry.Module.FlatCurry.C_Pattern x5 x6) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))))(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)(Curry.Module.XML.C_XElem((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List))))))))(Curry.Module.OracleFlatCurryXML.c_qname2xmlattrs(x5)(x1)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xmlShowVar))))(x6)(x8)(st)))((Curry.Module.Prelude.:<)(Curry.Module.OracleFlatCurryXML.c_xmlShowExpr(x4)(x9)(st))(Curry.Module.Prelude.List)))(x10)(st))(st)
c__case_1054_case__13 x1 x4 x3@(Curry.Module.FlatCurry.C_LPattern x7) st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))))(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))(Curry.Module.Prelude.List)))))))((Curry.Module.Prelude.:<)(Curry.Module.OracleXML.c_xml((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List)))))))))((Curry.Module.Prelude.:<)(Curry.Module.OracleFlatCurryXML.c_xmlShowLit(x7)(x1)(st))(Curry.Module.Prelude.List))(x11)(st))((Curry.Module.Prelude.:<)(Curry.Module.OracleFlatCurryXML.c_xmlShowExpr(x4)(x12)(st))(Curry.Module.Prelude.List)))(x13)(st))(st)
c__case_1054_case__13 x1 x4 (Curry.Module.FlatCurry.C_PatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1054_case__13(x1)(x4)(x)(st))(i)(xs)(st)
c__case_1054_case__13 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1054_case__13")(x)



c__case_1055_case__14 x1 x2@(Curry.Module.FlatCurry.C_Branch x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1054(x4)(x3)(x1)(st))(st)
c__case_1055_case__14 x1 (Curry.Module.FlatCurry.C_BranchExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1055_case__14(x1)(x)(st))(i)(xs)(st)
c__case_1055_case__14 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1055_case__14")(x)



c__case_1053_case__15 x1 x2@(Curry.Module.FlatCurry.C_Op x3 x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))))(Curry.Module.XML.C_XElem((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OracleFlatCurryXML.c_qname2xmlattrs(x3)(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.c_show(x4)(x6)(st)))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))(Curry.Module.Prelude.List)))))(Curry.Module.OraclePrelude.c_show(x5)(x7)(st)))(Curry.Module.Prelude.List)))(x8)(st))(Curry.Module.Prelude.List))(st)
c__case_1053_case__15 x1 (Curry.Module.FlatCurry.C_OpDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1053_case__15(x1)(x)(st))(i)(xs)(st)
c__case_1053_case__15 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1053_case__15")(x)



c__case_951_case__16 x1 x18 x35 x52 x67 x90 x87@Curry.Module.Prelude.List st = let {x91 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x92 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x93 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x94 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x91)((Curry.Module.Prelude.:<)(x92)((Curry.Module.Prelude.:<)(x93)((Curry.Module.Prelude.:<)(x94)(Curry.Module.Prelude.List)))))(Curry.Module.FlatCurry.C_Prog(Curry.Module.OracleXML.c_textOfXml(x18)(x1)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xml2FlatCurry'46_'35lambda5))))(x35)(x91)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_flatx2typedecl))))(x52)(x92)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xml2FlatCurry'46_'35lambda6))))(x67)(x93)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_xml2FlatCurry'46_'35lambda7))))(x90)(x94)(st)))(st)
c__case_951_case__16 x1 x18 x35 x52 x67 x90 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_951_case__16(x1)(x18)(x35)(x52)(x67)(x90)(x)(st))(i)(xs)(st)
c__case_951_case__16 x1 x18 x35 x52 x67 x90 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_951_case__16")(x)



c__case_952_case__17 x1 x18 x35 x52 x67 x87 x90 x89@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_951(x18)(x35)(x52)(x67)(x90)(x87)(x1)(st))(st)
c__case_952_case__17 x1 x18 x35 x52 x67 x87 x90 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_952_case__17(x1)(x18)(x35)(x52)(x67)(x87)(x90)(x)(st))(i)(xs)(st)
c__case_952_case__17 x1 x18 x35 x52 x67 x87 x90 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_952_case__17")(x)



c__case_953_case__18 x1 x18 x35 x52 x67 x87 x89 x90 x108@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_952(x18)(x35)(x52)(x67)(x87)(x90)(x89)(x1)(st))(st)
c__case_953_case__18 x1 x18 x35 x52 x67 x87 x89 x90 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_953_case__18(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x)(st))(i)(xs)(st)
c__case_953_case__18 x1 x18 x35 x52 x67 x87 x89 x90 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_953_case__18")(x)



c__case_954_case__19 x1 x18 x35 x52 x67 x87 x89 x90 x108 x107 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x107)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_953(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x108)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_955_case__20 x1 x18 x35 x52 x67 x87 x89 x90 x106@((Curry.Module.Prelude.:<) x107 x108) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_954(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x108)(x107)(x1)(st))(st)
c__case_955_case__20 x1 x18 x35 x52 x67 x87 x89 x90 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_955_case__20(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x)(st))(i)(xs)(st)
c__case_955_case__20 x1 x18 x35 x52 x67 x87 x89 x90 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_955_case__20")(x)



c__case_956_case__21 x1 x18 x35 x52 x67 x87 x89 x90 x106 x105 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x105)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_955(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x106)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_957_case__22 x1 x18 x35 x52 x67 x87 x89 x90 x104@((Curry.Module.Prelude.:<) x105 x106) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_956(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x106)(x105)(x1)(st))(st)
c__case_957_case__22 x1 x18 x35 x52 x67 x87 x89 x90 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_957_case__22(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x)(st))(i)(xs)(st)
c__case_957_case__22 x1 x18 x35 x52 x67 x87 x89 x90 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_957_case__22")(x)



c__case_958_case__23 x1 x18 x35 x52 x67 x87 x89 x90 x104 x103 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x103)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_957(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x104)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_959_case__24 x1 x18 x35 x52 x67 x87 x89 x90 x102@((Curry.Module.Prelude.:<) x103 x104) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_958(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x104)(x103)(x1)(st))(st)
c__case_959_case__24 x1 x18 x35 x52 x67 x87 x89 x90 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_959_case__24(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x)(st))(i)(xs)(st)
c__case_959_case__24 x1 x18 x35 x52 x67 x87 x89 x90 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_959_case__24")(x)



c__case_960_case__25 x1 x18 x35 x52 x67 x87 x89 x90 x102 x101 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x101)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_959(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x102)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_961_case__26 x1 x18 x35 x52 x67 x87 x89 x90 x100@((Curry.Module.Prelude.:<) x101 x102) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_960(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x102)(x101)(x1)(st))(st)
c__case_961_case__26 x1 x18 x35 x52 x67 x87 x89 x90 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_961_case__26(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x)(st))(i)(xs)(st)
c__case_961_case__26 x1 x18 x35 x52 x67 x87 x89 x90 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_961_case__26")(x)



c__case_962_case__27 x1 x18 x35 x52 x67 x87 x89 x90 x100 x99 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x99)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_961(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x100)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_963_case__28 x1 x18 x35 x52 x67 x87 x89 x90 x98@((Curry.Module.Prelude.:<) x99 x100) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_962(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x100)(x99)(x1)(st))(st)
c__case_963_case__28 x1 x18 x35 x52 x67 x87 x89 x90 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_963_case__28(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x)(st))(i)(xs)(st)
c__case_963_case__28 x1 x18 x35 x52 x67 x87 x89 x90 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_963_case__28")(x)



c__case_964_case__29 x1 x18 x35 x52 x67 x87 x89 x90 x98 x97 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x97)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_963(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x98)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_965_case__30 x1 x18 x35 x52 x67 x87 x89 x90 x96@((Curry.Module.Prelude.:<) x97 x98) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_964(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x98)(x97)(x1)(st))(st)
c__case_965_case__30 x1 x18 x35 x52 x67 x87 x89 x90 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_965_case__30(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x)(st))(i)(xs)(st)
c__case_965_case__30 x1 x18 x35 x52 x67 x87 x89 x90 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_965_case__30")(x)



c__case_966_case__31 x1 x18 x35 x52 x67 x87 x89 x90 x96 x95 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x95)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_965(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x96)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_967_case__32 x1 x18 x35 x52 x67 x87 x89 x90 x94@((Curry.Module.Prelude.:<) x95 x96) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_966(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x96)(x95)(x1)(st))(st)
c__case_967_case__32 x1 x18 x35 x52 x67 x87 x89 x90 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_967_case__32(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x)(st))(i)(xs)(st)
c__case_967_case__32 x1 x18 x35 x52 x67 x87 x89 x90 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_967_case__32")(x)



c__case_968_case__33 x1 x18 x35 x52 x67 x87 x89 x90 x94 x93 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x93)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_967(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x94)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_969_case__34 x1 x18 x35 x52 x67 x87 x89 x90 x92@((Curry.Module.Prelude.:<) x93 x94) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_968(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x94)(x93)(x1)(st))(st)
c__case_969_case__34 x1 x18 x35 x52 x67 x87 x89 x90 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_969_case__34(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x)(st))(i)(xs)(st)
c__case_969_case__34 x1 x18 x35 x52 x67 x87 x89 x90 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_969_case__34")(x)



c__case_970_case__35 x1 x18 x35 x52 x67 x87 x89 x90 x92 x91 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x91)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_969(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x92)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_971_case__36 x1 x18 x35 x52 x67 x87 x89 x90 x88@((Curry.Module.Prelude.:<) x91 x92) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_970(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x92)(x91)(x1)(st))(st)
c__case_971_case__36 x1 x18 x35 x52 x67 x87 x89 x90 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_971_case__36(x1)(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x)(st))(i)(xs)(st)
c__case_971_case__36 x1 x18 x35 x52 x67 x87 x89 x90 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_971_case__36")(x)



c__case_972_case__37 x1 x18 x35 x52 x67 x87 x86@(Curry.Module.XML.C_XElem x88 x89 x90) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_971(x18)(x35)(x52)(x67)(x87)(x89)(x90)(x88)(x1)(st))(st)
c__case_972_case__37 x1 x18 x35 x52 x67 x87 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_972_case__37(x1)(x18)(x35)(x52)(x67)(x87)(x)(st))(i)(xs)(st)
c__case_972_case__37 x1 x18 x35 x52 x67 x87 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_972_case__37")(x)



c__case_973_case__38 x1 x18 x35 x52 x67 x64@((Curry.Module.Prelude.:<) x86 x87) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_972(x18)(x35)(x52)(x67)(x87)(x86)(x1)(st))(st)
c__case_973_case__38 x1 x18 x35 x52 x67 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_973_case__38(x1)(x18)(x35)(x52)(x67)(x)(st))(i)(xs)(st)
c__case_973_case__38 x1 x18 x35 x52 x67 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_973_case__38")(x)



c__case_974_case__39 x1 x18 x35 x52 x64 x67 x66@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_973(x18)(x35)(x52)(x67)(x64)(x1)(st))(st)
c__case_974_case__39 x1 x18 x35 x52 x64 x67 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_974_case__39(x1)(x18)(x35)(x52)(x64)(x67)(x)(st))(i)(xs)(st)
c__case_974_case__39 x1 x18 x35 x52 x64 x67 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_974_case__39")(x)



c__case_975_case__40 x1 x18 x35 x52 x64 x66 x67 x85@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_974(x18)(x35)(x52)(x64)(x67)(x66)(x1)(st))(st)
c__case_975_case__40 x1 x18 x35 x52 x64 x66 x67 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_975_case__40(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x)(st))(i)(xs)(st)
c__case_975_case__40 x1 x18 x35 x52 x64 x66 x67 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_975_case__40")(x)



c__case_976_case__41 x1 x18 x35 x52 x64 x66 x67 x85 x84 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x84)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_975(x18)(x35)(x52)(x64)(x66)(x67)(x85)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_977_case__42 x1 x18 x35 x52 x64 x66 x67 x83@((Curry.Module.Prelude.:<) x84 x85) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_976(x18)(x35)(x52)(x64)(x66)(x67)(x85)(x84)(x1)(st))(st)
c__case_977_case__42 x1 x18 x35 x52 x64 x66 x67 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_977_case__42(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x)(st))(i)(xs)(st)
c__case_977_case__42 x1 x18 x35 x52 x64 x66 x67 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_977_case__42")(x)



c__case_978_case__43 x1 x18 x35 x52 x64 x66 x67 x83 x82 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x82)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_977(x18)(x35)(x52)(x64)(x66)(x67)(x83)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_979_case__44 x1 x18 x35 x52 x64 x66 x67 x81@((Curry.Module.Prelude.:<) x82 x83) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_978(x18)(x35)(x52)(x64)(x66)(x67)(x83)(x82)(x1)(st))(st)
c__case_979_case__44 x1 x18 x35 x52 x64 x66 x67 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_979_case__44(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x)(st))(i)(xs)(st)
c__case_979_case__44 x1 x18 x35 x52 x64 x66 x67 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_979_case__44")(x)



c__case_980_case__45 x1 x18 x35 x52 x64 x66 x67 x81 x80 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x80)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_979(x18)(x35)(x52)(x64)(x66)(x67)(x81)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_981_case__46 x1 x18 x35 x52 x64 x66 x67 x79@((Curry.Module.Prelude.:<) x80 x81) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_980(x18)(x35)(x52)(x64)(x66)(x67)(x81)(x80)(x1)(st))(st)
c__case_981_case__46 x1 x18 x35 x52 x64 x66 x67 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_981_case__46(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x)(st))(i)(xs)(st)
c__case_981_case__46 x1 x18 x35 x52 x64 x66 x67 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_981_case__46")(x)



c__case_982_case__47 x1 x18 x35 x52 x64 x66 x67 x79 x78 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x78)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_981(x18)(x35)(x52)(x64)(x66)(x67)(x79)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_983_case__48 x1 x18 x35 x52 x64 x66 x67 x77@((Curry.Module.Prelude.:<) x78 x79) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_982(x18)(x35)(x52)(x64)(x66)(x67)(x79)(x78)(x1)(st))(st)
c__case_983_case__48 x1 x18 x35 x52 x64 x66 x67 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_983_case__48(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x)(st))(i)(xs)(st)
c__case_983_case__48 x1 x18 x35 x52 x64 x66 x67 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_983_case__48")(x)



c__case_984_case__49 x1 x18 x35 x52 x64 x66 x67 x77 x76 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x76)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_983(x18)(x35)(x52)(x64)(x66)(x67)(x77)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_985_case__50 x1 x18 x35 x52 x64 x66 x67 x75@((Curry.Module.Prelude.:<) x76 x77) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_984(x18)(x35)(x52)(x64)(x66)(x67)(x77)(x76)(x1)(st))(st)
c__case_985_case__50 x1 x18 x35 x52 x64 x66 x67 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_985_case__50(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x)(st))(i)(xs)(st)
c__case_985_case__50 x1 x18 x35 x52 x64 x66 x67 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_985_case__50")(x)



c__case_986_case__51 x1 x18 x35 x52 x64 x66 x67 x75 x74 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x74)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_985(x18)(x35)(x52)(x64)(x66)(x67)(x75)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_987_case__52 x1 x18 x35 x52 x64 x66 x67 x73@((Curry.Module.Prelude.:<) x74 x75) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_986(x18)(x35)(x52)(x64)(x66)(x67)(x75)(x74)(x1)(st))(st)
c__case_987_case__52 x1 x18 x35 x52 x64 x66 x67 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_987_case__52(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x)(st))(i)(xs)(st)
c__case_987_case__52 x1 x18 x35 x52 x64 x66 x67 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_987_case__52")(x)



c__case_988_case__53 x1 x18 x35 x52 x64 x66 x67 x73 x72 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x72)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_987(x18)(x35)(x52)(x64)(x66)(x67)(x73)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_989_case__54 x1 x18 x35 x52 x64 x66 x67 x71@((Curry.Module.Prelude.:<) x72 x73) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_988(x18)(x35)(x52)(x64)(x66)(x67)(x73)(x72)(x1)(st))(st)
c__case_989_case__54 x1 x18 x35 x52 x64 x66 x67 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_989_case__54(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x)(st))(i)(xs)(st)
c__case_989_case__54 x1 x18 x35 x52 x64 x66 x67 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_989_case__54")(x)



c__case_990_case__55 x1 x18 x35 x52 x64 x66 x67 x71 x70 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x70)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_989(x18)(x35)(x52)(x64)(x66)(x67)(x71)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_991_case__56 x1 x18 x35 x52 x64 x66 x67 x69@((Curry.Module.Prelude.:<) x70 x71) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_990(x18)(x35)(x52)(x64)(x66)(x67)(x71)(x70)(x1)(st))(st)
c__case_991_case__56 x1 x18 x35 x52 x64 x66 x67 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_991_case__56(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x)(st))(i)(xs)(st)
c__case_991_case__56 x1 x18 x35 x52 x64 x66 x67 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_991_case__56")(x)



c__case_992_case__57 x1 x18 x35 x52 x64 x66 x67 x69 x68 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x68)(Curry.Module.Prelude.C_Char('f'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_991(x18)(x35)(x52)(x64)(x66)(x67)(x69)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_993_case__58 x1 x18 x35 x52 x64 x66 x67 x65@((Curry.Module.Prelude.:<) x68 x69) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_992(x18)(x35)(x52)(x64)(x66)(x67)(x69)(x68)(x1)(st))(st)
c__case_993_case__58 x1 x18 x35 x52 x64 x66 x67 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_993_case__58(x1)(x18)(x35)(x52)(x64)(x66)(x67)(x)(st))(i)(xs)(st)
c__case_993_case__58 x1 x18 x35 x52 x64 x66 x67 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_993_case__58")(x)



c__case_994_case__59 x1 x18 x35 x52 x64 x63@(Curry.Module.XML.C_XElem x65 x66 x67) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_993(x18)(x35)(x52)(x64)(x66)(x67)(x65)(x1)(st))(st)
c__case_994_case__59 x1 x18 x35 x52 x64 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_994_case__59(x1)(x18)(x35)(x52)(x64)(x)(st))(i)(xs)(st)
c__case_994_case__59 x1 x18 x35 x52 x64 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_994_case__59")(x)



c__case_995_case__60 x1 x18 x35 x52 x49@((Curry.Module.Prelude.:<) x63 x64) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_994(x18)(x35)(x52)(x64)(x63)(x1)(st))(st)
c__case_995_case__60 x1 x18 x35 x52 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_995_case__60(x1)(x18)(x35)(x52)(x)(st))(i)(xs)(st)
c__case_995_case__60 x1 x18 x35 x52 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_995_case__60")(x)



c__case_996_case__61 x1 x18 x35 x49 x52 x51@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_995(x18)(x35)(x52)(x49)(x1)(st))(st)
c__case_996_case__61 x1 x18 x35 x49 x52 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_996_case__61(x1)(x18)(x35)(x49)(x52)(x)(st))(i)(xs)(st)
c__case_996_case__61 x1 x18 x35 x49 x52 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_996_case__61")(x)



c__case_997_case__62 x1 x18 x35 x49 x51 x52 x62@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_996(x18)(x35)(x49)(x52)(x51)(x1)(st))(st)
c__case_997_case__62 x1 x18 x35 x49 x51 x52 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_997_case__62(x1)(x18)(x35)(x49)(x51)(x52)(x)(st))(i)(xs)(st)
c__case_997_case__62 x1 x18 x35 x49 x51 x52 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_997_case__62")(x)



c__case_998_case__63 x1 x18 x35 x49 x51 x52 x62 x61 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x61)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_997(x18)(x35)(x49)(x51)(x52)(x62)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_999_case__64 x1 x18 x35 x49 x51 x52 x60@((Curry.Module.Prelude.:<) x61 x62) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_998(x18)(x35)(x49)(x51)(x52)(x62)(x61)(x1)(st))(st)
c__case_999_case__64 x1 x18 x35 x49 x51 x52 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_999_case__64(x1)(x18)(x35)(x49)(x51)(x52)(x)(st))(i)(xs)(st)
c__case_999_case__64 x1 x18 x35 x49 x51 x52 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_999_case__64")(x)



c__case_1000_case__65 x1 x18 x35 x49 x51 x52 x60 x59 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x59)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_999(x18)(x35)(x49)(x51)(x52)(x60)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1001_case__66 x1 x18 x35 x49 x51 x52 x58@((Curry.Module.Prelude.:<) x59 x60) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1000(x18)(x35)(x49)(x51)(x52)(x60)(x59)(x1)(st))(st)
c__case_1001_case__66 x1 x18 x35 x49 x51 x52 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1001_case__66(x1)(x18)(x35)(x49)(x51)(x52)(x)(st))(i)(xs)(st)
c__case_1001_case__66 x1 x18 x35 x49 x51 x52 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1001_case__66")(x)



c__case_1002_case__67 x1 x18 x35 x49 x51 x52 x58 x57 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x57)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1001(x18)(x35)(x49)(x51)(x52)(x58)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1003_case__68 x1 x18 x35 x49 x51 x52 x56@((Curry.Module.Prelude.:<) x57 x58) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1002(x18)(x35)(x49)(x51)(x52)(x58)(x57)(x1)(st))(st)
c__case_1003_case__68 x1 x18 x35 x49 x51 x52 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1003_case__68(x1)(x18)(x35)(x49)(x51)(x52)(x)(st))(i)(xs)(st)
c__case_1003_case__68 x1 x18 x35 x49 x51 x52 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1003_case__68")(x)



c__case_1004_case__69 x1 x18 x35 x49 x51 x52 x56 x55 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x55)(Curry.Module.Prelude.C_Char('y'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1003(x18)(x35)(x49)(x51)(x52)(x56)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1005_case__70 x1 x18 x35 x49 x51 x52 x54@((Curry.Module.Prelude.:<) x55 x56) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1004(x18)(x35)(x49)(x51)(x52)(x56)(x55)(x1)(st))(st)
c__case_1005_case__70 x1 x18 x35 x49 x51 x52 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1005_case__70(x1)(x18)(x35)(x49)(x51)(x52)(x)(st))(i)(xs)(st)
c__case_1005_case__70 x1 x18 x35 x49 x51 x52 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1005_case__70")(x)



c__case_1006_case__71 x1 x18 x35 x49 x51 x52 x54 x53 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x53)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1005(x18)(x35)(x49)(x51)(x52)(x54)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1007_case__72 x1 x18 x35 x49 x51 x52 x50@((Curry.Module.Prelude.:<) x53 x54) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1006(x18)(x35)(x49)(x51)(x52)(x54)(x53)(x1)(st))(st)
c__case_1007_case__72 x1 x18 x35 x49 x51 x52 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1007_case__72(x1)(x18)(x35)(x49)(x51)(x52)(x)(st))(i)(xs)(st)
c__case_1007_case__72 x1 x18 x35 x49 x51 x52 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1007_case__72")(x)



c__case_1008_case__73 x1 x18 x35 x49 x48@(Curry.Module.XML.C_XElem x50 x51 x52) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1007(x18)(x35)(x49)(x51)(x52)(x50)(x1)(st))(st)
c__case_1008_case__73 x1 x18 x35 x49 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1008_case__73(x1)(x18)(x35)(x49)(x)(st))(i)(xs)(st)
c__case_1008_case__73 x1 x18 x35 x49 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1008_case__73")(x)



c__case_1009_case__74 x1 x18 x35 x32@((Curry.Module.Prelude.:<) x48 x49) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1008(x18)(x35)(x49)(x48)(x1)(st))(st)
c__case_1009_case__74 x1 x18 x35 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1009_case__74(x1)(x18)(x35)(x)(st))(i)(xs)(st)
c__case_1009_case__74 x1 x18 x35 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1009_case__74")(x)



c__case_1010_case__75 x1 x18 x32 x35 x34@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1009(x18)(x35)(x32)(x1)(st))(st)
c__case_1010_case__75 x1 x18 x32 x35 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1010_case__75(x1)(x18)(x32)(x35)(x)(st))(i)(xs)(st)
c__case_1010_case__75 x1 x18 x32 x35 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1010_case__75")(x)



c__case_1011_case__76 x1 x18 x32 x34 x35 x47@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1010(x18)(x32)(x35)(x34)(x1)(st))(st)
c__case_1011_case__76 x1 x18 x32 x34 x35 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1011_case__76(x1)(x18)(x32)(x34)(x35)(x)(st))(i)(xs)(st)
c__case_1011_case__76 x1 x18 x32 x34 x35 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1011_case__76")(x)



c__case_1012_case__77 x1 x18 x32 x34 x35 x47 x46 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x46)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1011(x18)(x32)(x34)(x35)(x47)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1013_case__78 x1 x18 x32 x34 x35 x45@((Curry.Module.Prelude.:<) x46 x47) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1012(x18)(x32)(x34)(x35)(x47)(x46)(x1)(st))(st)
c__case_1013_case__78 x1 x18 x32 x34 x35 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1013_case__78(x1)(x18)(x32)(x34)(x35)(x)(st))(i)(xs)(st)
c__case_1013_case__78 x1 x18 x32 x34 x35 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1013_case__78")(x)



c__case_1014_case__79 x1 x18 x32 x34 x35 x45 x44 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x44)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1013(x18)(x32)(x34)(x35)(x45)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1015_case__80 x1 x18 x32 x34 x35 x43@((Curry.Module.Prelude.:<) x44 x45) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1014(x18)(x32)(x34)(x35)(x45)(x44)(x1)(st))(st)
c__case_1015_case__80 x1 x18 x32 x34 x35 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1015_case__80(x1)(x18)(x32)(x34)(x35)(x)(st))(i)(xs)(st)
c__case_1015_case__80 x1 x18 x32 x34 x35 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1015_case__80")(x)



c__case_1016_case__81 x1 x18 x32 x34 x35 x43 x42 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x42)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1015(x18)(x32)(x34)(x35)(x43)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1017_case__82 x1 x18 x32 x34 x35 x41@((Curry.Module.Prelude.:<) x42 x43) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1016(x18)(x32)(x34)(x35)(x43)(x42)(x1)(st))(st)
c__case_1017_case__82 x1 x18 x32 x34 x35 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1017_case__82(x1)(x18)(x32)(x34)(x35)(x)(st))(i)(xs)(st)
c__case_1017_case__82 x1 x18 x32 x34 x35 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1017_case__82")(x)



c__case_1018_case__83 x1 x18 x32 x34 x35 x41 x40 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x40)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1017(x18)(x32)(x34)(x35)(x41)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1019_case__84 x1 x18 x32 x34 x35 x39@((Curry.Module.Prelude.:<) x40 x41) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1018(x18)(x32)(x34)(x35)(x41)(x40)(x1)(st))(st)
c__case_1019_case__84 x1 x18 x32 x34 x35 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1019_case__84(x1)(x18)(x32)(x34)(x35)(x)(st))(i)(xs)(st)
c__case_1019_case__84 x1 x18 x32 x34 x35 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1019_case__84")(x)



c__case_1020_case__85 x1 x18 x32 x34 x35 x39 x38 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x38)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1019(x18)(x32)(x34)(x35)(x39)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1021_case__86 x1 x18 x32 x34 x35 x37@((Curry.Module.Prelude.:<) x38 x39) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1020(x18)(x32)(x34)(x35)(x39)(x38)(x1)(st))(st)
c__case_1021_case__86 x1 x18 x32 x34 x35 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1021_case__86(x1)(x18)(x32)(x34)(x35)(x)(st))(i)(xs)(st)
c__case_1021_case__86 x1 x18 x32 x34 x35 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1021_case__86")(x)



c__case_1022_case__87 x1 x18 x32 x34 x35 x37 x36 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x36)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1021(x18)(x32)(x34)(x35)(x37)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1023_case__88 x1 x18 x32 x34 x35 x33@((Curry.Module.Prelude.:<) x36 x37) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1022(x18)(x32)(x34)(x35)(x37)(x36)(x1)(st))(st)
c__case_1023_case__88 x1 x18 x32 x34 x35 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1023_case__88(x1)(x18)(x32)(x34)(x35)(x)(st))(i)(xs)(st)
c__case_1023_case__88 x1 x18 x32 x34 x35 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1023_case__88")(x)



c__case_1024_case__89 x1 x18 x32 x31@(Curry.Module.XML.C_XElem x33 x34 x35) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1023(x18)(x32)(x34)(x35)(x33)(x1)(st))(st)
c__case_1024_case__89 x1 x18 x32 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1024_case__89(x1)(x18)(x32)(x)(st))(i)(xs)(st)
c__case_1024_case__89 x1 x18 x32 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1024_case__89")(x)



c__case_1025_case__90 x1 x18 x15@((Curry.Module.Prelude.:<) x31 x32) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1024(x18)(x32)(x31)(x1)(st))(st)
c__case_1025_case__90 x1 x18 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1025_case__90(x1)(x18)(x)(st))(i)(xs)(st)
c__case_1025_case__90 x1 x18 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1025_case__90")(x)



c__case_1026_case__91 x1 x15 x18 x17@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1025(x18)(x15)(x1)(st))(st)
c__case_1026_case__91 x1 x15 x18 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1026_case__91(x1)(x15)(x18)(x)(st))(i)(xs)(st)
c__case_1026_case__91 x1 x15 x18 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1026_case__91")(x)



c__case_1027_case__92 x1 x15 x17 x18 x30@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1026(x15)(x18)(x17)(x1)(st))(st)
c__case_1027_case__92 x1 x15 x17 x18 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1027_case__92(x1)(x15)(x17)(x18)(x)(st))(i)(xs)(st)
c__case_1027_case__92 x1 x15 x17 x18 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1027_case__92")(x)



c__case_1028_case__93 x1 x15 x17 x18 x30 x29 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x29)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1027(x15)(x17)(x18)(x30)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1029_case__94 x1 x15 x17 x18 x28@((Curry.Module.Prelude.:<) x29 x30) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1028(x15)(x17)(x18)(x30)(x29)(x1)(st))(st)
c__case_1029_case__94 x1 x15 x17 x18 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1029_case__94(x1)(x15)(x17)(x18)(x)(st))(i)(xs)(st)
c__case_1029_case__94 x1 x15 x17 x18 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1029_case__94")(x)



c__case_1030_case__95 x1 x15 x17 x18 x28 x27 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x27)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1029(x15)(x17)(x18)(x28)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1031_case__96 x1 x15 x17 x18 x26@((Curry.Module.Prelude.:<) x27 x28) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1030(x15)(x17)(x18)(x28)(x27)(x1)(st))(st)
c__case_1031_case__96 x1 x15 x17 x18 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1031_case__96(x1)(x15)(x17)(x18)(x)(st))(i)(xs)(st)
c__case_1031_case__96 x1 x15 x17 x18 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1031_case__96")(x)



c__case_1032_case__97 x1 x15 x17 x18 x26 x25 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x25)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1031(x15)(x17)(x18)(x26)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1033_case__98 x1 x15 x17 x18 x24@((Curry.Module.Prelude.:<) x25 x26) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1032(x15)(x17)(x18)(x26)(x25)(x1)(st))(st)
c__case_1033_case__98 x1 x15 x17 x18 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1033_case__98(x1)(x15)(x17)(x18)(x)(st))(i)(xs)(st)
c__case_1033_case__98 x1 x15 x17 x18 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1033_case__98")(x)



c__case_1034_case__99 x1 x15 x17 x18 x24 x23 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x23)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1033(x15)(x17)(x18)(x24)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1035_case__100 x1 x15 x17 x18 x22@((Curry.Module.Prelude.:<) x23 x24) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1034(x15)(x17)(x18)(x24)(x23)(x1)(st))(st)
c__case_1035_case__100 x1 x15 x17 x18 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1035_case__100(x1)(x15)(x17)(x18)(x)(st))(i)(xs)(st)
c__case_1035_case__100 x1 x15 x17 x18 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1035_case__100")(x)



c__case_1036_case__101 x1 x15 x17 x18 x22 x21 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x21)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1035(x15)(x17)(x18)(x22)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1037_case__102 x1 x15 x17 x18 x20@((Curry.Module.Prelude.:<) x21 x22) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1036(x15)(x17)(x18)(x22)(x21)(x1)(st))(st)
c__case_1037_case__102 x1 x15 x17 x18 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1037_case__102(x1)(x15)(x17)(x18)(x)(st))(i)(xs)(st)
c__case_1037_case__102 x1 x15 x17 x18 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1037_case__102")(x)



c__case_1038_case__103 x1 x15 x17 x18 x20 x19 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x19)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1037(x15)(x17)(x18)(x20)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1039_case__104 x1 x15 x17 x18 x16@((Curry.Module.Prelude.:<) x19 x20) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1038(x15)(x17)(x18)(x20)(x19)(x1)(st))(st)
c__case_1039_case__104 x1 x15 x17 x18 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1039_case__104(x1)(x15)(x17)(x18)(x)(st))(i)(xs)(st)
c__case_1039_case__104 x1 x15 x17 x18 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1039_case__104")(x)



c__case_1040_case__105 x1 x15 x14@(Curry.Module.XML.C_XElem x16 x17 x18) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1039(x15)(x17)(x18)(x16)(x1)(st))(st)
c__case_1040_case__105 x1 x15 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1040_case__105(x1)(x15)(x)(st))(i)(xs)(st)
c__case_1040_case__105 x1 x15 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1040_case__105")(x)



c__case_1041_case__106 x1 x5@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1040(x15)(x14)(x1)(st))(st)
c__case_1041_case__106 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1041_case__106(x1)(x)(st))(i)(xs)(st)
c__case_1041_case__106 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1041_case__106")(x)



c__case_1042_case__107 x1 x5 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1041(x5)(x1)(st))(st)
c__case_1042_case__107 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1042_case__107(x1)(x5)(x)(st))(i)(xs)(st)
c__case_1042_case__107 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1042_case__107")(x)



c__case_1043_case__108 x1 x4 x5 x13@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1042(x5)(x4)(x1)(st))(st)
c__case_1043_case__108 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1043_case__108(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_1043_case__108 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1043_case__108")(x)



c__case_1044_case__109 x1 x4 x5 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('g'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1043(x4)(x5)(x13)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1045_case__110 x1 x4 x5 x11@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1044(x4)(x5)(x13)(x12)(x1)(st))(st)
c__case_1045_case__110 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1045_case__110(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_1045_case__110 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1045_case__110")(x)



c__case_1046_case__111 x1 x4 x5 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1045(x4)(x5)(x11)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1047_case__112 x1 x4 x5 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1046(x4)(x5)(x11)(x10)(x1)(st))(st)
c__case_1047_case__112 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1047_case__112(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_1047_case__112 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1047_case__112")(x)



c__case_1048_case__113 x1 x4 x5 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1047(x4)(x5)(x9)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1049_case__114 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1048(x4)(x5)(x9)(x8)(x1)(st))(st)
c__case_1049_case__114 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1049_case__114(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_1049_case__114 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1049_case__114")(x)



c__case_1050_case__115 x1 x4 x5 x7 x6 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1049(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_1051_case__116 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1050(x4)(x5)(x7)(x6)(x1)(st))(st)
c__case_1051_case__116 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1051_case__116(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_1051_case__116 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1051_case__116")(x)



c__case_1052_case__117 x1 x2@(Curry.Module.XML.C_XElem x3 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1051(x4)(x5)(x3)(x1)(st))(st)
c__case_1052_case__117 x1 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_1052_case__117(x1)(x)(st))(i)(xs)(st)
c__case_1052_case__117 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_1052_case__117")(x)



c__case_936_case__118 x1 x5 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleXML.c_textOfXml(x5)(x1)(st))(st)
c__case_936_case__118 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_936_case__118(x1)(x5)(x)(st))(i)(xs)(st)
c__case_936_case__118 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_936_case__118")(x)



c__case_937_case__119 x1 x4 x5 x17@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_936(x5)(x4)(x1)(st))(st)
c__case_937_case__119 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_937_case__119(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_937_case__119 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_937_case__119")(x)



c__case_938_case__120 x1 x4 x5 x17 x16 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x16)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_937(x4)(x5)(x17)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_939_case__121 x1 x4 x5 x15@((Curry.Module.Prelude.:<) x16 x17) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_938(x4)(x5)(x17)(x16)(x1)(st))(st)
c__case_939_case__121 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_939_case__121(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_939_case__121 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_939_case__121")(x)



c__case_940_case__122 x1 x4 x5 x15 x14 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x14)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_939(x4)(x5)(x15)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_941_case__123 x1 x4 x5 x13@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_940(x4)(x5)(x15)(x14)(x1)(st))(st)
c__case_941_case__123 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_941_case__123(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_941_case__123 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_941_case__123")(x)



c__case_942_case__124 x1 x4 x5 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_941(x4)(x5)(x13)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_943_case__125 x1 x4 x5 x11@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_942(x4)(x5)(x13)(x12)(x1)(st))(st)
c__case_943_case__125 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_943_case__125(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_943_case__125 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_943_case__125")(x)



c__case_944_case__126 x1 x4 x5 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_943(x4)(x5)(x11)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_945_case__127 x1 x4 x5 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_944(x4)(x5)(x11)(x10)(x1)(st))(st)
c__case_945_case__127 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_945_case__127(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_945_case__127 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_945_case__127")(x)



c__case_946_case__128 x1 x4 x5 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_945(x4)(x5)(x9)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_947_case__129 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_946(x4)(x5)(x9)(x8)(x1)(st))(st)
c__case_947_case__129 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_947_case__129(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_947_case__129 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_947_case__129")(x)



c__case_948_case__130 x1 x4 x5 x7 x6 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_947(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_949_case__131 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_948(x4)(x5)(x7)(x6)(x1)(st))(st)
c__case_949_case__131 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_949_case__131(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_949_case__131 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_949_case__131")(x)



c__case_950_case__132 x1 x2@(Curry.Module.XML.C_XElem x3 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_949(x4)(x5)(x3)(x1)(st))(st)
c__case_950_case__132 x1 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_950_case__132(x1)(x)(st))(i)(xs)(st)
c__case_950_case__132 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_950_case__132")(x)



c__case_882_case__133 x1 x17 x33 x45 x56 x58 x60 x61@Curry.Module.Prelude.List st = let {x62 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x63 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x64 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x62)((Curry.Module.Prelude.:<)(x63)((Curry.Module.Prelude.:<)(x64)(Curry.Module.Prelude.List))))(Curry.Module.FlatCurry.C_Func(Curry.Module.Prelude.T2(x17)(x33))(Curry.Module.OracleRead.c_readNat(x45)(x1)(st))(Curry.Module.OracleFlatCurryXML.c_xvis2vis(x56)(x62)(st))(Curry.Module.OracleFlatCurryXML.c_flatx2texp(x58)(x63)(st))(Curry.Module.OracleFlatCurryXML.c_flatx2FunBody(x60)(x64)(st)))(st)
c__case_882_case__133 x1 x17 x33 x45 x56 x58 x60 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_882_case__133(x1)(x17)(x33)(x45)(x56)(x58)(x60)(x)(st))(i)(xs)(st)
c__case_882_case__133 x1 x17 x33 x45 x56 x58 x60 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_882_case__133")(x)



c__case_883_case__134 x1 x17 x33 x45 x56 x58 x59@((Curry.Module.Prelude.:<) x60 x61) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_882(x17)(x33)(x45)(x56)(x58)(x60)(x61)(x1)(st))(st)
c__case_883_case__134 x1 x17 x33 x45 x56 x58 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_883_case__134(x1)(x17)(x33)(x45)(x56)(x58)(x)(st))(i)(xs)(st)
c__case_883_case__134 x1 x17 x33 x45 x56 x58 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_883_case__134")(x)



c__case_884_case__135 x1 x17 x33 x45 x56 x5@((Curry.Module.Prelude.:<) x58 x59) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_883(x17)(x33)(x45)(x56)(x58)(x59)(x1)(st))(st)
c__case_884_case__135 x1 x17 x33 x45 x56 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_884_case__135(x1)(x17)(x33)(x45)(x56)(x)(st))(i)(xs)(st)
c__case_884_case__135 x1 x17 x33 x45 x56 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_884_case__135")(x)



c__case_885_case__136 x1 x5 x17 x33 x45 x56 x57@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_884(x17)(x33)(x45)(x56)(x5)(x1)(st))(st)
c__case_885_case__136 x1 x5 x17 x33 x45 x56 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_885_case__136(x1)(x5)(x17)(x33)(x45)(x56)(x)(st))(i)(xs)(st)
c__case_885_case__136 x1 x5 x17 x33 x45 x56 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_885_case__136")(x)



c__case_886_case__137 x1 x5 x17 x33 x45 x43@((Curry.Module.Prelude.:<) x56 x57) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_885(x5)(x17)(x33)(x45)(x56)(x57)(x1)(st))(st)
c__case_886_case__137 x1 x5 x17 x33 x45 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_886_case__137(x1)(x5)(x17)(x33)(x45)(x)(st))(i)(xs)(st)
c__case_886_case__137 x1 x5 x17 x33 x45 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_886_case__137")(x)



c__case_887_case__138 x1 x5 x17 x33 x43 x45 x55@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_886(x5)(x17)(x33)(x45)(x43)(x1)(st))(st)
c__case_887_case__138 x1 x5 x17 x33 x43 x45 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_887_case__138(x1)(x5)(x17)(x33)(x43)(x45)(x)(st))(i)(xs)(st)
c__case_887_case__138 x1 x5 x17 x33 x43 x45 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_887_case__138")(x)



c__case_888_case__139 x1 x5 x17 x33 x43 x45 x55 x54 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x54)(Curry.Module.Prelude.C_Char('y'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_887(x5)(x17)(x33)(x43)(x45)(x55)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_889_case__140 x1 x5 x17 x33 x43 x45 x53@((Curry.Module.Prelude.:<) x54 x55) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_888(x5)(x17)(x33)(x43)(x45)(x55)(x54)(x1)(st))(st)
c__case_889_case__140 x1 x5 x17 x33 x43 x45 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_889_case__140(x1)(x5)(x17)(x33)(x43)(x45)(x)(st))(i)(xs)(st)
c__case_889_case__140 x1 x5 x17 x33 x43 x45 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_889_case__140")(x)



c__case_890_case__141 x1 x5 x17 x33 x43 x45 x53 x52 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x52)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_889(x5)(x17)(x33)(x43)(x45)(x53)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_891_case__142 x1 x5 x17 x33 x43 x45 x51@((Curry.Module.Prelude.:<) x52 x53) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_890(x5)(x17)(x33)(x43)(x45)(x53)(x52)(x1)(st))(st)
c__case_891_case__142 x1 x5 x17 x33 x43 x45 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_891_case__142(x1)(x5)(x17)(x33)(x43)(x45)(x)(st))(i)(xs)(st)
c__case_891_case__142 x1 x5 x17 x33 x43 x45 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_891_case__142")(x)



c__case_892_case__143 x1 x5 x17 x33 x43 x45 x51 x50 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x50)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_891(x5)(x17)(x33)(x43)(x45)(x51)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_893_case__144 x1 x5 x17 x33 x43 x45 x49@((Curry.Module.Prelude.:<) x50 x51) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_892(x5)(x17)(x33)(x43)(x45)(x51)(x50)(x1)(st))(st)
c__case_893_case__144 x1 x5 x17 x33 x43 x45 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_893_case__144(x1)(x5)(x17)(x33)(x43)(x45)(x)(st))(i)(xs)(st)
c__case_893_case__144 x1 x5 x17 x33 x43 x45 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_893_case__144")(x)



c__case_894_case__145 x1 x5 x17 x33 x43 x45 x49 x48 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x48)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_893(x5)(x17)(x33)(x43)(x45)(x49)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_895_case__146 x1 x5 x17 x33 x43 x45 x47@((Curry.Module.Prelude.:<) x48 x49) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_894(x5)(x17)(x33)(x43)(x45)(x49)(x48)(x1)(st))(st)
c__case_895_case__146 x1 x5 x17 x33 x43 x45 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_895_case__146(x1)(x5)(x17)(x33)(x43)(x45)(x)(st))(i)(xs)(st)
c__case_895_case__146 x1 x5 x17 x33 x43 x45 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_895_case__146")(x)



c__case_896_case__147 x1 x5 x17 x33 x43 x45 x47 x46 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x46)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_895(x5)(x17)(x33)(x43)(x45)(x47)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_897_case__148 x1 x5 x17 x33 x43 x45 x44@((Curry.Module.Prelude.:<) x46 x47) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_896(x5)(x17)(x33)(x43)(x45)(x47)(x46)(x1)(st))(st)
c__case_897_case__148 x1 x5 x17 x33 x43 x45 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_897_case__148(x1)(x5)(x17)(x33)(x43)(x45)(x)(st))(i)(xs)(st)
c__case_897_case__148 x1 x5 x17 x33 x43 x45 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_897_case__148")(x)



c__case_898_case__149 x1 x5 x17 x33 x43 x42@(Curry.Module.Prelude.T2 x44 x45) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_897(x5)(x17)(x33)(x43)(x45)(x44)(x1)(st))(st)
c__case_898_case__149 x1 x5 x17 x33 x43 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_898_case__149(x1)(x5)(x17)(x33)(x43)(x)(st))(i)(xs)(st)
c__case_898_case__149 x1 x5 x17 x33 x43 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_898_case__149")(x)



c__case_899_case__150 x1 x5 x17 x33 x31@((Curry.Module.Prelude.:<) x42 x43) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_898(x5)(x17)(x33)(x43)(x42)(x1)(st))(st)
c__case_899_case__150 x1 x5 x17 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_899_case__150(x1)(x5)(x17)(x33)(x)(st))(i)(xs)(st)
c__case_899_case__150 x1 x5 x17 x33 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_899_case__150")(x)



c__case_900_case__151 x1 x5 x17 x31 x33 x41@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_899(x5)(x17)(x33)(x31)(x1)(st))(st)
c__case_900_case__151 x1 x5 x17 x31 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_900_case__151(x1)(x5)(x17)(x31)(x33)(x)(st))(i)(xs)(st)
c__case_900_case__151 x1 x5 x17 x31 x33 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_900_case__151")(x)



c__case_901_case__152 x1 x5 x17 x31 x33 x41 x40 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x40)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_900(x5)(x17)(x31)(x33)(x41)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_902_case__153 x1 x5 x17 x31 x33 x39@((Curry.Module.Prelude.:<) x40 x41) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_901(x5)(x17)(x31)(x33)(x41)(x40)(x1)(st))(st)
c__case_902_case__153 x1 x5 x17 x31 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_902_case__153(x1)(x5)(x17)(x31)(x33)(x)(st))(i)(xs)(st)
c__case_902_case__153 x1 x5 x17 x31 x33 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_902_case__153")(x)



c__case_903_case__154 x1 x5 x17 x31 x33 x39 x38 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x38)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_902(x5)(x17)(x31)(x33)(x39)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_904_case__155 x1 x5 x17 x31 x33 x37@((Curry.Module.Prelude.:<) x38 x39) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_903(x5)(x17)(x31)(x33)(x39)(x38)(x1)(st))(st)
c__case_904_case__155 x1 x5 x17 x31 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_904_case__155(x1)(x5)(x17)(x31)(x33)(x)(st))(i)(xs)(st)
c__case_904_case__155 x1 x5 x17 x31 x33 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_904_case__155")(x)



c__case_905_case__156 x1 x5 x17 x31 x33 x37 x36 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x36)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_904(x5)(x17)(x31)(x33)(x37)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_906_case__157 x1 x5 x17 x31 x33 x35@((Curry.Module.Prelude.:<) x36 x37) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_905(x5)(x17)(x31)(x33)(x37)(x36)(x1)(st))(st)
c__case_906_case__157 x1 x5 x17 x31 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_906_case__157(x1)(x5)(x17)(x31)(x33)(x)(st))(i)(xs)(st)
c__case_906_case__157 x1 x5 x17 x31 x33 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_906_case__157")(x)



c__case_907_case__158 x1 x5 x17 x31 x33 x35 x34 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x34)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_906(x5)(x17)(x31)(x33)(x35)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_908_case__159 x1 x5 x17 x31 x33 x32@((Curry.Module.Prelude.:<) x34 x35) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_907(x5)(x17)(x31)(x33)(x35)(x34)(x1)(st))(st)
c__case_908_case__159 x1 x5 x17 x31 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_908_case__159(x1)(x5)(x17)(x31)(x33)(x)(st))(i)(xs)(st)
c__case_908_case__159 x1 x5 x17 x31 x33 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_908_case__159")(x)



c__case_909_case__160 x1 x5 x17 x31 x30@(Curry.Module.Prelude.T2 x32 x33) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_908(x5)(x17)(x31)(x33)(x32)(x1)(st))(st)
c__case_909_case__160 x1 x5 x17 x31 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_909_case__160(x1)(x5)(x17)(x31)(x)(st))(i)(xs)(st)
c__case_909_case__160 x1 x5 x17 x31 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_909_case__160")(x)



c__case_910_case__161 x1 x5 x17 x15@((Curry.Module.Prelude.:<) x30 x31) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_909(x5)(x17)(x31)(x30)(x1)(st))(st)
c__case_910_case__161 x1 x5 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_910_case__161(x1)(x5)(x17)(x)(st))(i)(xs)(st)
c__case_910_case__161 x1 x5 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_910_case__161")(x)



c__case_911_case__162 x1 x5 x15 x17 x29@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_910(x5)(x17)(x15)(x1)(st))(st)
c__case_911_case__162 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_911_case__162(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_911_case__162 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_911_case__162")(x)



c__case_912_case__163 x1 x5 x15 x17 x29 x28 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x28)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_911(x5)(x15)(x17)(x29)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_913_case__164 x1 x5 x15 x17 x27@((Curry.Module.Prelude.:<) x28 x29) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_912(x5)(x15)(x17)(x29)(x28)(x1)(st))(st)
c__case_913_case__164 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_913_case__164(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_913_case__164 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_913_case__164")(x)



c__case_914_case__165 x1 x5 x15 x17 x27 x26 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x26)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_913(x5)(x15)(x17)(x27)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_915_case__166 x1 x5 x15 x17 x25@((Curry.Module.Prelude.:<) x26 x27) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_914(x5)(x15)(x17)(x27)(x26)(x1)(st))(st)
c__case_915_case__166 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_915_case__166(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_915_case__166 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_915_case__166")(x)



c__case_916_case__167 x1 x5 x15 x17 x25 x24 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x24)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_915(x5)(x15)(x17)(x25)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_917_case__168 x1 x5 x15 x17 x23@((Curry.Module.Prelude.:<) x24 x25) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_916(x5)(x15)(x17)(x25)(x24)(x1)(st))(st)
c__case_917_case__168 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_917_case__168(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_917_case__168 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_917_case__168")(x)



c__case_918_case__169 x1 x5 x15 x17 x23 x22 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x22)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_917(x5)(x15)(x17)(x23)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_919_case__170 x1 x5 x15 x17 x21@((Curry.Module.Prelude.:<) x22 x23) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_918(x5)(x15)(x17)(x23)(x22)(x1)(st))(st)
c__case_919_case__170 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_919_case__170(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_919_case__170 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_919_case__170")(x)



c__case_920_case__171 x1 x5 x15 x17 x21 x20 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x20)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_919(x5)(x15)(x17)(x21)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_921_case__172 x1 x5 x15 x17 x19@((Curry.Module.Prelude.:<) x20 x21) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_920(x5)(x15)(x17)(x21)(x20)(x1)(st))(st)
c__case_921_case__172 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_921_case__172(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_921_case__172 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_921_case__172")(x)



c__case_922_case__173 x1 x5 x15 x17 x19 x18 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x18)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_921(x5)(x15)(x17)(x19)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_923_case__174 x1 x5 x15 x17 x16@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_922(x5)(x15)(x17)(x19)(x18)(x1)(st))(st)
c__case_923_case__174 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_923_case__174(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_923_case__174 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_923_case__174")(x)



c__case_924_case__175 x1 x5 x15 x14@(Curry.Module.Prelude.T2 x16 x17) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_923(x5)(x15)(x17)(x16)(x1)(st))(st)
c__case_924_case__175 x1 x5 x15 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_924_case__175(x1)(x5)(x15)(x)(st))(i)(xs)(st)
c__case_924_case__175 x1 x5 x15 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_924_case__175")(x)



c__case_925_case__176 x1 x5 x4@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_924(x5)(x15)(x14)(x1)(st))(st)
c__case_925_case__176 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_925_case__176(x1)(x5)(x)(st))(i)(xs)(st)
c__case_925_case__176 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_925_case__176")(x)



c__case_926_case__177 x1 x4 x5 x13@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_925(x5)(x4)(x1)(st))(st)
c__case_926_case__177 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_926_case__177(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_926_case__177 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_926_case__177")(x)



c__case_927_case__178 x1 x4 x5 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_926(x4)(x5)(x13)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_928_case__179 x1 x4 x5 x11@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_927(x4)(x5)(x13)(x12)(x1)(st))(st)
c__case_928_case__179 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_928_case__179(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_928_case__179 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_928_case__179")(x)



c__case_929_case__180 x1 x4 x5 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_928(x4)(x5)(x11)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_930_case__181 x1 x4 x5 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_929(x4)(x5)(x11)(x10)(x1)(st))(st)
c__case_930_case__181 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_930_case__181(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_930_case__181 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_930_case__181")(x)



c__case_931_case__182 x1 x4 x5 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_930(x4)(x5)(x9)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_932_case__183 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_931(x4)(x5)(x9)(x8)(x1)(st))(st)
c__case_932_case__183 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_932_case__183(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_932_case__183 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_932_case__183")(x)



c__case_933_case__184 x1 x4 x5 x7 x6 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('f'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_932(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_934_case__185 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_933(x4)(x5)(x7)(x6)(x1)(st))(st)
c__case_934_case__185 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_934_case__185(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_934_case__185 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_934_case__185")(x)



c__case_935_case__186 x1 x2@(Curry.Module.XML.C_XElem x3 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_934(x4)(x5)(x3)(x1)(st))(st)
c__case_935_case__186 x1 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_935_case__186(x1)(x)(st))(i)(xs)(st)
c__case_935_case__186 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_935_case__186")(x)



c__case_822_case__187 x1 x13 x29 x41 x57 x5@Curry.Module.Prelude.List st = let {x58 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x58)(Curry.Module.Prelude.List))(Curry.Module.FlatCurry.C_Op(Curry.Module.Prelude.T2(x13)(x29))(Curry.Module.OracleFlatCurryXML.c_flatx2Fixity(x41)(x1)(st))(Curry.Module.OracleRead.c_readNat(x57)(x58)(st)))(st)
c__case_822_case__187 x1 x13 x29 x41 x57 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_822_case__187(x1)(x13)(x29)(x41)(x57)(x)(st))(i)(xs)(st)
c__case_822_case__187 x1 x13 x29 x41 x57 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_822_case__187")(x)



c__case_823_case__188 x1 x5 x13 x29 x41 x57 x55@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_822(x13)(x29)(x41)(x57)(x5)(x1)(st))(st)
c__case_823_case__188 x1 x5 x13 x29 x41 x57 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_823_case__188(x1)(x5)(x13)(x29)(x41)(x57)(x)(st))(i)(xs)(st)
c__case_823_case__188 x1 x5 x13 x29 x41 x57 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_823_case__188")(x)



c__case_824_case__189 x1 x5 x13 x29 x41 x55 x57 x65@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_823(x5)(x13)(x29)(x41)(x57)(x55)(x1)(st))(st)
c__case_824_case__189 x1 x5 x13 x29 x41 x55 x57 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_824_case__189(x1)(x5)(x13)(x29)(x41)(x55)(x57)(x)(st))(i)(xs)(st)
c__case_824_case__189 x1 x5 x13 x29 x41 x55 x57 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_824_case__189")(x)



c__case_825_case__190 x1 x5 x13 x29 x41 x55 x57 x65 x64 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x64)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_824(x5)(x13)(x29)(x41)(x55)(x57)(x65)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_826_case__191 x1 x5 x13 x29 x41 x55 x57 x63@((Curry.Module.Prelude.:<) x64 x65) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_825(x5)(x13)(x29)(x41)(x55)(x57)(x65)(x64)(x1)(st))(st)
c__case_826_case__191 x1 x5 x13 x29 x41 x55 x57 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_826_case__191(x1)(x5)(x13)(x29)(x41)(x55)(x57)(x)(st))(i)(xs)(st)
c__case_826_case__191 x1 x5 x13 x29 x41 x55 x57 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_826_case__191")(x)



c__case_827_case__192 x1 x5 x13 x29 x41 x55 x57 x63 x62 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x62)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_826(x5)(x13)(x29)(x41)(x55)(x57)(x63)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_828_case__193 x1 x5 x13 x29 x41 x55 x57 x61@((Curry.Module.Prelude.:<) x62 x63) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_827(x5)(x13)(x29)(x41)(x55)(x57)(x63)(x62)(x1)(st))(st)
c__case_828_case__193 x1 x5 x13 x29 x41 x55 x57 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_828_case__193(x1)(x5)(x13)(x29)(x41)(x55)(x57)(x)(st))(i)(xs)(st)
c__case_828_case__193 x1 x5 x13 x29 x41 x55 x57 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_828_case__193")(x)



c__case_829_case__194 x1 x5 x13 x29 x41 x55 x57 x61 x60 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x60)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_828(x5)(x13)(x29)(x41)(x55)(x57)(x61)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_830_case__195 x1 x5 x13 x29 x41 x55 x57 x59@((Curry.Module.Prelude.:<) x60 x61) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_829(x5)(x13)(x29)(x41)(x55)(x57)(x61)(x60)(x1)(st))(st)
c__case_830_case__195 x1 x5 x13 x29 x41 x55 x57 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_830_case__195(x1)(x5)(x13)(x29)(x41)(x55)(x57)(x)(st))(i)(xs)(st)
c__case_830_case__195 x1 x5 x13 x29 x41 x55 x57 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_830_case__195")(x)



c__case_831_case__196 x1 x5 x13 x29 x41 x55 x57 x59 x58 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x58)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_830(x5)(x13)(x29)(x41)(x55)(x57)(x59)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_832_case__197 x1 x5 x13 x29 x41 x55 x57 x56@((Curry.Module.Prelude.:<) x58 x59) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_831(x5)(x13)(x29)(x41)(x55)(x57)(x59)(x58)(x1)(st))(st)
c__case_832_case__197 x1 x5 x13 x29 x41 x55 x57 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_832_case__197(x1)(x5)(x13)(x29)(x41)(x55)(x57)(x)(st))(i)(xs)(st)
c__case_832_case__197 x1 x5 x13 x29 x41 x55 x57 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_832_case__197")(x)



c__case_833_case__198 x1 x5 x13 x29 x41 x55 x54@(Curry.Module.Prelude.T2 x56 x57) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_832(x5)(x13)(x29)(x41)(x55)(x57)(x56)(x1)(st))(st)
c__case_833_case__198 x1 x5 x13 x29 x41 x55 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_833_case__198(x1)(x5)(x13)(x29)(x41)(x55)(x)(st))(i)(xs)(st)
c__case_833_case__198 x1 x5 x13 x29 x41 x55 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_833_case__198")(x)



c__case_834_case__199 x1 x5 x13 x29 x41 x39@((Curry.Module.Prelude.:<) x54 x55) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_833(x5)(x13)(x29)(x41)(x55)(x54)(x1)(st))(st)
c__case_834_case__199 x1 x5 x13 x29 x41 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_834_case__199(x1)(x5)(x13)(x29)(x41)(x)(st))(i)(xs)(st)
c__case_834_case__199 x1 x5 x13 x29 x41 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_834_case__199")(x)



c__case_835_case__200 x1 x5 x13 x29 x39 x41 x53@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_834(x5)(x13)(x29)(x41)(x39)(x1)(st))(st)
c__case_835_case__200 x1 x5 x13 x29 x39 x41 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_835_case__200(x1)(x5)(x13)(x29)(x39)(x41)(x)(st))(i)(xs)(st)
c__case_835_case__200 x1 x5 x13 x29 x39 x41 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_835_case__200")(x)



c__case_836_case__201 x1 x5 x13 x29 x39 x41 x53 x52 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x52)(Curry.Module.Prelude.C_Char('y'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_835(x5)(x13)(x29)(x39)(x41)(x53)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_837_case__202 x1 x5 x13 x29 x39 x41 x51@((Curry.Module.Prelude.:<) x52 x53) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_836(x5)(x13)(x29)(x39)(x41)(x53)(x52)(x1)(st))(st)
c__case_837_case__202 x1 x5 x13 x29 x39 x41 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_837_case__202(x1)(x5)(x13)(x29)(x39)(x41)(x)(st))(i)(xs)(st)
c__case_837_case__202 x1 x5 x13 x29 x39 x41 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_837_case__202")(x)



c__case_838_case__203 x1 x5 x13 x29 x39 x41 x51 x50 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x50)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_837(x5)(x13)(x29)(x39)(x41)(x51)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_839_case__204 x1 x5 x13 x29 x39 x41 x49@((Curry.Module.Prelude.:<) x50 x51) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_838(x5)(x13)(x29)(x39)(x41)(x51)(x50)(x1)(st))(st)
c__case_839_case__204 x1 x5 x13 x29 x39 x41 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_839_case__204(x1)(x5)(x13)(x29)(x39)(x41)(x)(st))(i)(xs)(st)
c__case_839_case__204 x1 x5 x13 x29 x39 x41 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_839_case__204")(x)



c__case_840_case__205 x1 x5 x13 x29 x39 x41 x49 x48 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x48)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_839(x5)(x13)(x29)(x39)(x41)(x49)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_841_case__206 x1 x5 x13 x29 x39 x41 x47@((Curry.Module.Prelude.:<) x48 x49) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_840(x5)(x13)(x29)(x39)(x41)(x49)(x48)(x1)(st))(st)
c__case_841_case__206 x1 x5 x13 x29 x39 x41 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_841_case__206(x1)(x5)(x13)(x29)(x39)(x41)(x)(st))(i)(xs)(st)
c__case_841_case__206 x1 x5 x13 x29 x39 x41 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_841_case__206")(x)



c__case_842_case__207 x1 x5 x13 x29 x39 x41 x47 x46 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x46)(Curry.Module.Prelude.C_Char('x'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_841(x5)(x13)(x29)(x39)(x41)(x47)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_843_case__208 x1 x5 x13 x29 x39 x41 x45@((Curry.Module.Prelude.:<) x46 x47) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_842(x5)(x13)(x29)(x39)(x41)(x47)(x46)(x1)(st))(st)
c__case_843_case__208 x1 x5 x13 x29 x39 x41 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_843_case__208(x1)(x5)(x13)(x29)(x39)(x41)(x)(st))(i)(xs)(st)
c__case_843_case__208 x1 x5 x13 x29 x39 x41 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_843_case__208")(x)



c__case_844_case__209 x1 x5 x13 x29 x39 x41 x45 x44 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x44)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_843(x5)(x13)(x29)(x39)(x41)(x45)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_845_case__210 x1 x5 x13 x29 x39 x41 x43@((Curry.Module.Prelude.:<) x44 x45) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_844(x5)(x13)(x29)(x39)(x41)(x45)(x44)(x1)(st))(st)
c__case_845_case__210 x1 x5 x13 x29 x39 x41 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_845_case__210(x1)(x5)(x13)(x29)(x39)(x41)(x)(st))(i)(xs)(st)
c__case_845_case__210 x1 x5 x13 x29 x39 x41 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_845_case__210")(x)



c__case_846_case__211 x1 x5 x13 x29 x39 x41 x43 x42 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x42)(Curry.Module.Prelude.C_Char('f'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_845(x5)(x13)(x29)(x39)(x41)(x43)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_847_case__212 x1 x5 x13 x29 x39 x41 x40@((Curry.Module.Prelude.:<) x42 x43) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_846(x5)(x13)(x29)(x39)(x41)(x43)(x42)(x1)(st))(st)
c__case_847_case__212 x1 x5 x13 x29 x39 x41 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_847_case__212(x1)(x5)(x13)(x29)(x39)(x41)(x)(st))(i)(xs)(st)
c__case_847_case__212 x1 x5 x13 x29 x39 x41 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_847_case__212")(x)



c__case_848_case__213 x1 x5 x13 x29 x39 x38@(Curry.Module.Prelude.T2 x40 x41) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_847(x5)(x13)(x29)(x39)(x41)(x40)(x1)(st))(st)
c__case_848_case__213 x1 x5 x13 x29 x39 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_848_case__213(x1)(x5)(x13)(x29)(x39)(x)(st))(i)(xs)(st)
c__case_848_case__213 x1 x5 x13 x29 x39 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_848_case__213")(x)



c__case_849_case__214 x1 x5 x13 x29 x27@((Curry.Module.Prelude.:<) x38 x39) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_848(x5)(x13)(x29)(x39)(x38)(x1)(st))(st)
c__case_849_case__214 x1 x5 x13 x29 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_849_case__214(x1)(x5)(x13)(x29)(x)(st))(i)(xs)(st)
c__case_849_case__214 x1 x5 x13 x29 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_849_case__214")(x)



c__case_850_case__215 x1 x5 x13 x27 x29 x37@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_849(x5)(x13)(x29)(x27)(x1)(st))(st)
c__case_850_case__215 x1 x5 x13 x27 x29 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_850_case__215(x1)(x5)(x13)(x27)(x29)(x)(st))(i)(xs)(st)
c__case_850_case__215 x1 x5 x13 x27 x29 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_850_case__215")(x)



c__case_851_case__216 x1 x5 x13 x27 x29 x37 x36 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x36)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_850(x5)(x13)(x27)(x29)(x37)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_852_case__217 x1 x5 x13 x27 x29 x35@((Curry.Module.Prelude.:<) x36 x37) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_851(x5)(x13)(x27)(x29)(x37)(x36)(x1)(st))(st)
c__case_852_case__217 x1 x5 x13 x27 x29 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_852_case__217(x1)(x5)(x13)(x27)(x29)(x)(st))(i)(xs)(st)
c__case_852_case__217 x1 x5 x13 x27 x29 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_852_case__217")(x)



c__case_853_case__218 x1 x5 x13 x27 x29 x35 x34 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x34)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_852(x5)(x13)(x27)(x29)(x35)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_854_case__219 x1 x5 x13 x27 x29 x33@((Curry.Module.Prelude.:<) x34 x35) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_853(x5)(x13)(x27)(x29)(x35)(x34)(x1)(st))(st)
c__case_854_case__219 x1 x5 x13 x27 x29 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_854_case__219(x1)(x5)(x13)(x27)(x29)(x)(st))(i)(xs)(st)
c__case_854_case__219 x1 x5 x13 x27 x29 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_854_case__219")(x)



c__case_855_case__220 x1 x5 x13 x27 x29 x33 x32 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x32)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_854(x5)(x13)(x27)(x29)(x33)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_856_case__221 x1 x5 x13 x27 x29 x31@((Curry.Module.Prelude.:<) x32 x33) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_855(x5)(x13)(x27)(x29)(x33)(x32)(x1)(st))(st)
c__case_856_case__221 x1 x5 x13 x27 x29 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_856_case__221(x1)(x5)(x13)(x27)(x29)(x)(st))(i)(xs)(st)
c__case_856_case__221 x1 x5 x13 x27 x29 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_856_case__221")(x)



c__case_857_case__222 x1 x5 x13 x27 x29 x31 x30 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x30)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_856(x5)(x13)(x27)(x29)(x31)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_858_case__223 x1 x5 x13 x27 x29 x28@((Curry.Module.Prelude.:<) x30 x31) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_857(x5)(x13)(x27)(x29)(x31)(x30)(x1)(st))(st)
c__case_858_case__223 x1 x5 x13 x27 x29 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_858_case__223(x1)(x5)(x13)(x27)(x29)(x)(st))(i)(xs)(st)
c__case_858_case__223 x1 x5 x13 x27 x29 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_858_case__223")(x)



c__case_859_case__224 x1 x5 x13 x27 x26@(Curry.Module.Prelude.T2 x28 x29) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_858(x5)(x13)(x27)(x29)(x28)(x1)(st))(st)
c__case_859_case__224 x1 x5 x13 x27 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_859_case__224(x1)(x5)(x13)(x27)(x)(st))(i)(xs)(st)
c__case_859_case__224 x1 x5 x13 x27 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_859_case__224")(x)



c__case_860_case__225 x1 x5 x13 x11@((Curry.Module.Prelude.:<) x26 x27) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_859(x5)(x13)(x27)(x26)(x1)(st))(st)
c__case_860_case__225 x1 x5 x13 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_860_case__225(x1)(x5)(x13)(x)(st))(i)(xs)(st)
c__case_860_case__225 x1 x5 x13 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_860_case__225")(x)



c__case_861_case__226 x1 x5 x11 x13 x25@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_860(x5)(x13)(x11)(x1)(st))(st)
c__case_861_case__226 x1 x5 x11 x13 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_861_case__226(x1)(x5)(x11)(x13)(x)(st))(i)(xs)(st)
c__case_861_case__226 x1 x5 x11 x13 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_861_case__226")(x)



c__case_862_case__227 x1 x5 x11 x13 x25 x24 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x24)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_861(x5)(x11)(x13)(x25)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_863_case__228 x1 x5 x11 x13 x23@((Curry.Module.Prelude.:<) x24 x25) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_862(x5)(x11)(x13)(x25)(x24)(x1)(st))(st)
c__case_863_case__228 x1 x5 x11 x13 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_863_case__228(x1)(x5)(x11)(x13)(x)(st))(i)(xs)(st)
c__case_863_case__228 x1 x5 x11 x13 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_863_case__228")(x)



c__case_864_case__229 x1 x5 x11 x13 x23 x22 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x22)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_863(x5)(x11)(x13)(x23)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_865_case__230 x1 x5 x11 x13 x21@((Curry.Module.Prelude.:<) x22 x23) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_864(x5)(x11)(x13)(x23)(x22)(x1)(st))(st)
c__case_865_case__230 x1 x5 x11 x13 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_865_case__230(x1)(x5)(x11)(x13)(x)(st))(i)(xs)(st)
c__case_865_case__230 x1 x5 x11 x13 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_865_case__230")(x)



c__case_866_case__231 x1 x5 x11 x13 x21 x20 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x20)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_865(x5)(x11)(x13)(x21)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_867_case__232 x1 x5 x11 x13 x19@((Curry.Module.Prelude.:<) x20 x21) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_866(x5)(x11)(x13)(x21)(x20)(x1)(st))(st)
c__case_867_case__232 x1 x5 x11 x13 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_867_case__232(x1)(x5)(x11)(x13)(x)(st))(i)(xs)(st)
c__case_867_case__232 x1 x5 x11 x13 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_867_case__232")(x)



c__case_868_case__233 x1 x5 x11 x13 x19 x18 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x18)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_867(x5)(x11)(x13)(x19)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_869_case__234 x1 x5 x11 x13 x17@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_868(x5)(x11)(x13)(x19)(x18)(x1)(st))(st)
c__case_869_case__234 x1 x5 x11 x13 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_869_case__234(x1)(x5)(x11)(x13)(x)(st))(i)(xs)(st)
c__case_869_case__234 x1 x5 x11 x13 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_869_case__234")(x)



c__case_870_case__235 x1 x5 x11 x13 x17 x16 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x16)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_869(x5)(x11)(x13)(x17)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_871_case__236 x1 x5 x11 x13 x15@((Curry.Module.Prelude.:<) x16 x17) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_870(x5)(x11)(x13)(x17)(x16)(x1)(st))(st)
c__case_871_case__236 x1 x5 x11 x13 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_871_case__236(x1)(x5)(x11)(x13)(x)(st))(i)(xs)(st)
c__case_871_case__236 x1 x5 x11 x13 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_871_case__236")(x)



c__case_872_case__237 x1 x5 x11 x13 x15 x14 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x14)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_871(x5)(x11)(x13)(x15)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_873_case__238 x1 x5 x11 x13 x12@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_872(x5)(x11)(x13)(x15)(x14)(x1)(st))(st)
c__case_873_case__238 x1 x5 x11 x13 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_873_case__238(x1)(x5)(x11)(x13)(x)(st))(i)(xs)(st)
c__case_873_case__238 x1 x5 x11 x13 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_873_case__238")(x)



c__case_874_case__239 x1 x5 x11 x10@(Curry.Module.Prelude.T2 x12 x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_873(x5)(x11)(x13)(x12)(x1)(st))(st)
c__case_874_case__239 x1 x5 x11 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_874_case__239(x1)(x5)(x11)(x)(st))(i)(xs)(st)
c__case_874_case__239 x1 x5 x11 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_874_case__239")(x)



c__case_875_case__240 x1 x5 x4@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_874(x5)(x11)(x10)(x1)(st))(st)
c__case_875_case__240 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_875_case__240(x1)(x5)(x)(st))(i)(xs)(st)
c__case_875_case__240 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_875_case__240")(x)



c__case_876_case__241 x1 x4 x5 x9@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_875(x5)(x4)(x1)(st))(st)
c__case_876_case__241 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_876_case__241(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_876_case__241 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_876_case__241")(x)



c__case_877_case__242 x1 x4 x5 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_876(x4)(x5)(x9)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_878_case__243 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_877(x4)(x5)(x9)(x8)(x1)(st))(st)
c__case_878_case__243 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_878_case__243(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_878_case__243 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_878_case__243")(x)



c__case_879_case__244 x1 x4 x5 x7 x6 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_878(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_880_case__245 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_879(x4)(x5)(x7)(x6)(x1)(st))(st)
c__case_880_case__245 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_880_case__245(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_880_case__245 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_880_case__245")(x)



c__case_881_case__246 x1 x2@(Curry.Module.XML.C_XElem x3 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_880(x4)(x5)(x3)(x1)(st))(st)
c__case_881_case__246 x1 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_881_case__246(x1)(x)(st))(i)(xs)(st)
c__case_881_case__246 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_881_case__246")(x)



c__case_768_case__247 x1 x17 x33 x42 x45 x48 x47@Curry.Module.Prelude.List st = let {x49 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x50 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x49)((Curry.Module.Prelude.:<)(x50)(Curry.Module.Prelude.List)))(Curry.Module.FlatCurry.C_Type(Curry.Module.Prelude.T2(x17)(x33))(Curry.Module.OracleFlatCurryXML.c_xvis2vis(x42)(x1)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_flatx2typedecl'46_'35lambda8))))(x48)(x49)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_flatx2typedecl'46_'35lambda9))))(x45)(x50)(st)))(st)
c__case_768_case__247 x1 x17 x33 x42 x45 x48 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_768_case__247(x1)(x17)(x33)(x42)(x45)(x48)(x)(st))(i)(xs)(st)
c__case_768_case__247 x1 x17 x33 x42 x45 x48 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_768_case__247")(x)



c__case_769_case__248 x1 x17 x33 x42 x45 x47 x48 x60@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_768(x17)(x33)(x42)(x45)(x48)(x47)(x1)(st))(st)
c__case_769_case__248 x1 x17 x33 x42 x45 x47 x48 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_769_case__248(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x)(st))(i)(xs)(st)
c__case_769_case__248 x1 x17 x33 x42 x45 x47 x48 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_769_case__248")(x)



c__case_770_case__249 x1 x17 x33 x42 x45 x47 x48 x60 x59 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x59)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_769(x17)(x33)(x42)(x45)(x47)(x48)(x60)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_771_case__250 x1 x17 x33 x42 x45 x47 x48 x58@((Curry.Module.Prelude.:<) x59 x60) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_770(x17)(x33)(x42)(x45)(x47)(x48)(x60)(x59)(x1)(st))(st)
c__case_771_case__250 x1 x17 x33 x42 x45 x47 x48 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_771_case__250(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x)(st))(i)(xs)(st)
c__case_771_case__250 x1 x17 x33 x42 x45 x47 x48 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_771_case__250")(x)



c__case_772_case__251 x1 x17 x33 x42 x45 x47 x48 x58 x57 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x57)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_771(x17)(x33)(x42)(x45)(x47)(x48)(x58)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_773_case__252 x1 x17 x33 x42 x45 x47 x48 x56@((Curry.Module.Prelude.:<) x57 x58) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_772(x17)(x33)(x42)(x45)(x47)(x48)(x58)(x57)(x1)(st))(st)
c__case_773_case__252 x1 x17 x33 x42 x45 x47 x48 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_773_case__252(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x)(st))(i)(xs)(st)
c__case_773_case__252 x1 x17 x33 x42 x45 x47 x48 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_773_case__252")(x)



c__case_774_case__253 x1 x17 x33 x42 x45 x47 x48 x56 x55 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x55)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_773(x17)(x33)(x42)(x45)(x47)(x48)(x56)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_775_case__254 x1 x17 x33 x42 x45 x47 x48 x54@((Curry.Module.Prelude.:<) x55 x56) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_774(x17)(x33)(x42)(x45)(x47)(x48)(x56)(x55)(x1)(st))(st)
c__case_775_case__254 x1 x17 x33 x42 x45 x47 x48 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_775_case__254(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x)(st))(i)(xs)(st)
c__case_775_case__254 x1 x17 x33 x42 x45 x47 x48 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_775_case__254")(x)



c__case_776_case__255 x1 x17 x33 x42 x45 x47 x48 x54 x53 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x53)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_775(x17)(x33)(x42)(x45)(x47)(x48)(x54)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_777_case__256 x1 x17 x33 x42 x45 x47 x48 x52@((Curry.Module.Prelude.:<) x53 x54) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_776(x17)(x33)(x42)(x45)(x47)(x48)(x54)(x53)(x1)(st))(st)
c__case_777_case__256 x1 x17 x33 x42 x45 x47 x48 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_777_case__256(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x)(st))(i)(xs)(st)
c__case_777_case__256 x1 x17 x33 x42 x45 x47 x48 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_777_case__256")(x)



c__case_778_case__257 x1 x17 x33 x42 x45 x47 x48 x52 x51 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x51)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_777(x17)(x33)(x42)(x45)(x47)(x48)(x52)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_779_case__258 x1 x17 x33 x42 x45 x47 x48 x50@((Curry.Module.Prelude.:<) x51 x52) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_778(x17)(x33)(x42)(x45)(x47)(x48)(x52)(x51)(x1)(st))(st)
c__case_779_case__258 x1 x17 x33 x42 x45 x47 x48 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_779_case__258(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x)(st))(i)(xs)(st)
c__case_779_case__258 x1 x17 x33 x42 x45 x47 x48 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_779_case__258")(x)



c__case_780_case__259 x1 x17 x33 x42 x45 x47 x48 x50 x49 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x49)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_779(x17)(x33)(x42)(x45)(x47)(x48)(x50)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_781_case__260 x1 x17 x33 x42 x45 x47 x48 x46@((Curry.Module.Prelude.:<) x49 x50) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_780(x17)(x33)(x42)(x45)(x47)(x48)(x50)(x49)(x1)(st))(st)
c__case_781_case__260 x1 x17 x33 x42 x45 x47 x48 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_781_case__260(x1)(x17)(x33)(x42)(x45)(x47)(x48)(x)(st))(i)(xs)(st)
c__case_781_case__260 x1 x17 x33 x42 x45 x47 x48 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_781_case__260")(x)



c__case_782_case__261 x1 x17 x33 x42 x45 x44@(Curry.Module.XML.C_XElem x46 x47 x48) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_781(x17)(x33)(x42)(x45)(x47)(x48)(x46)(x1)(st))(st)
c__case_782_case__261 x1 x17 x33 x42 x45 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_782_case__261(x1)(x17)(x33)(x42)(x45)(x)(st))(i)(xs)(st)
c__case_782_case__261 x1 x17 x33 x42 x45 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_782_case__261")(x)



c__case_783_case__262 x1 x17 x33 x42 x5@((Curry.Module.Prelude.:<) x44 x45) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_782(x17)(x33)(x42)(x45)(x44)(x1)(st))(st)
c__case_783_case__262 x1 x17 x33 x42 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_783_case__262(x1)(x17)(x33)(x42)(x)(st))(i)(xs)(st)
c__case_783_case__262 x1 x17 x33 x42 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_783_case__262")(x)



c__case_784_case__263 x1 x5 x17 x33 x42 x43@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_783(x17)(x33)(x42)(x5)(x1)(st))(st)
c__case_784_case__263 x1 x5 x17 x33 x42 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_784_case__263(x1)(x5)(x17)(x33)(x42)(x)(st))(i)(xs)(st)
c__case_784_case__263 x1 x5 x17 x33 x42 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_784_case__263")(x)



c__case_785_case__264 x1 x5 x17 x33 x31@((Curry.Module.Prelude.:<) x42 x43) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_784(x5)(x17)(x33)(x42)(x43)(x1)(st))(st)
c__case_785_case__264 x1 x5 x17 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_785_case__264(x1)(x5)(x17)(x33)(x)(st))(i)(xs)(st)
c__case_785_case__264 x1 x5 x17 x33 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_785_case__264")(x)



c__case_786_case__265 x1 x5 x17 x31 x33 x41@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_785(x5)(x17)(x33)(x31)(x1)(st))(st)
c__case_786_case__265 x1 x5 x17 x31 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_786_case__265(x1)(x5)(x17)(x31)(x33)(x)(st))(i)(xs)(st)
c__case_786_case__265 x1 x5 x17 x31 x33 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_786_case__265")(x)



c__case_787_case__266 x1 x5 x17 x31 x33 x41 x40 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x40)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_786(x5)(x17)(x31)(x33)(x41)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_788_case__267 x1 x5 x17 x31 x33 x39@((Curry.Module.Prelude.:<) x40 x41) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_787(x5)(x17)(x31)(x33)(x41)(x40)(x1)(st))(st)
c__case_788_case__267 x1 x5 x17 x31 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_788_case__267(x1)(x5)(x17)(x31)(x33)(x)(st))(i)(xs)(st)
c__case_788_case__267 x1 x5 x17 x31 x33 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_788_case__267")(x)



c__case_789_case__268 x1 x5 x17 x31 x33 x39 x38 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x38)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_788(x5)(x17)(x31)(x33)(x39)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_790_case__269 x1 x5 x17 x31 x33 x37@((Curry.Module.Prelude.:<) x38 x39) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_789(x5)(x17)(x31)(x33)(x39)(x38)(x1)(st))(st)
c__case_790_case__269 x1 x5 x17 x31 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_790_case__269(x1)(x5)(x17)(x31)(x33)(x)(st))(i)(xs)(st)
c__case_790_case__269 x1 x5 x17 x31 x33 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_790_case__269")(x)



c__case_791_case__270 x1 x5 x17 x31 x33 x37 x36 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x36)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_790(x5)(x17)(x31)(x33)(x37)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_792_case__271 x1 x5 x17 x31 x33 x35@((Curry.Module.Prelude.:<) x36 x37) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_791(x5)(x17)(x31)(x33)(x37)(x36)(x1)(st))(st)
c__case_792_case__271 x1 x5 x17 x31 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_792_case__271(x1)(x5)(x17)(x31)(x33)(x)(st))(i)(xs)(st)
c__case_792_case__271 x1 x5 x17 x31 x33 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_792_case__271")(x)



c__case_793_case__272 x1 x5 x17 x31 x33 x35 x34 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x34)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_792(x5)(x17)(x31)(x33)(x35)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_794_case__273 x1 x5 x17 x31 x33 x32@((Curry.Module.Prelude.:<) x34 x35) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_793(x5)(x17)(x31)(x33)(x35)(x34)(x1)(st))(st)
c__case_794_case__273 x1 x5 x17 x31 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_794_case__273(x1)(x5)(x17)(x31)(x33)(x)(st))(i)(xs)(st)
c__case_794_case__273 x1 x5 x17 x31 x33 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_794_case__273")(x)



c__case_795_case__274 x1 x5 x17 x31 x30@(Curry.Module.Prelude.T2 x32 x33) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_794(x5)(x17)(x31)(x33)(x32)(x1)(st))(st)
c__case_795_case__274 x1 x5 x17 x31 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_795_case__274(x1)(x5)(x17)(x31)(x)(st))(i)(xs)(st)
c__case_795_case__274 x1 x5 x17 x31 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_795_case__274")(x)



c__case_796_case__275 x1 x5 x17 x15@((Curry.Module.Prelude.:<) x30 x31) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_795(x5)(x17)(x31)(x30)(x1)(st))(st)
c__case_796_case__275 x1 x5 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_796_case__275(x1)(x5)(x17)(x)(st))(i)(xs)(st)
c__case_796_case__275 x1 x5 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_796_case__275")(x)



c__case_797_case__276 x1 x5 x15 x17 x29@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_796(x5)(x17)(x15)(x1)(st))(st)
c__case_797_case__276 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_797_case__276(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_797_case__276 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_797_case__276")(x)



c__case_798_case__277 x1 x5 x15 x17 x29 x28 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x28)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_797(x5)(x15)(x17)(x29)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_799_case__278 x1 x5 x15 x17 x27@((Curry.Module.Prelude.:<) x28 x29) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_798(x5)(x15)(x17)(x29)(x28)(x1)(st))(st)
c__case_799_case__278 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_799_case__278(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_799_case__278 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_799_case__278")(x)



c__case_800_case__279 x1 x5 x15 x17 x27 x26 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x26)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_799(x5)(x15)(x17)(x27)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_801_case__280 x1 x5 x15 x17 x25@((Curry.Module.Prelude.:<) x26 x27) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_800(x5)(x15)(x17)(x27)(x26)(x1)(st))(st)
c__case_801_case__280 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_801_case__280(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_801_case__280 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_801_case__280")(x)



c__case_802_case__281 x1 x5 x15 x17 x25 x24 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x24)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_801(x5)(x15)(x17)(x25)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_803_case__282 x1 x5 x15 x17 x23@((Curry.Module.Prelude.:<) x24 x25) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_802(x5)(x15)(x17)(x25)(x24)(x1)(st))(st)
c__case_803_case__282 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_803_case__282(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_803_case__282 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_803_case__282")(x)



c__case_804_case__283 x1 x5 x15 x17 x23 x22 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x22)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_803(x5)(x15)(x17)(x23)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_805_case__284 x1 x5 x15 x17 x21@((Curry.Module.Prelude.:<) x22 x23) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_804(x5)(x15)(x17)(x23)(x22)(x1)(st))(st)
c__case_805_case__284 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_805_case__284(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_805_case__284 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_805_case__284")(x)



c__case_806_case__285 x1 x5 x15 x17 x21 x20 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x20)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_805(x5)(x15)(x17)(x21)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_807_case__286 x1 x5 x15 x17 x19@((Curry.Module.Prelude.:<) x20 x21) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_806(x5)(x15)(x17)(x21)(x20)(x1)(st))(st)
c__case_807_case__286 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_807_case__286(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_807_case__286 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_807_case__286")(x)



c__case_808_case__287 x1 x5 x15 x17 x19 x18 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x18)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_807(x5)(x15)(x17)(x19)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_809_case__288 x1 x5 x15 x17 x16@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_808(x5)(x15)(x17)(x19)(x18)(x1)(st))(st)
c__case_809_case__288 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_809_case__288(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_809_case__288 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_809_case__288")(x)



c__case_810_case__289 x1 x5 x15 x14@(Curry.Module.Prelude.T2 x16 x17) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_809(x5)(x15)(x17)(x16)(x1)(st))(st)
c__case_810_case__289 x1 x5 x15 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_810_case__289(x1)(x5)(x15)(x)(st))(i)(xs)(st)
c__case_810_case__289 x1 x5 x15 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_810_case__289")(x)



c__case_811_case__290 x1 x5 x4@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_810(x5)(x15)(x14)(x1)(st))(st)
c__case_811_case__290 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_811_case__290(x1)(x5)(x)(st))(i)(xs)(st)
c__case_811_case__290 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_811_case__290")(x)



c__case_716_case__291 x1 x70 x86 x95 x101 x114 x115@Curry.Module.Prelude.List st = let {x116 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x117 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x116)((Curry.Module.Prelude.:<)(x117)(Curry.Module.Prelude.List)))(Curry.Module.FlatCurry.C_TypeSyn(Curry.Module.Prelude.T2(x70)(x86))(Curry.Module.OracleFlatCurryXML.c_xvis2vis(x95)(x1)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_flatx2typedecl'46_'35lambda10))))(x101)(x116)(st))(Curry.Module.OracleFlatCurryXML.c_flatx2texp(x114)(x117)(st)))(st)
c__case_716_case__291 x1 x70 x86 x95 x101 x114 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_716_case__291(x1)(x70)(x86)(x95)(x101)(x114)(x)(st))(i)(xs)(st)
c__case_716_case__291 x1 x70 x86 x95 x101 x114 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_716_case__291")(x)



c__case_717_case__292 x1 x70 x86 x95 x101 x98@((Curry.Module.Prelude.:<) x114 x115) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_716(x70)(x86)(x95)(x101)(x114)(x115)(x1)(st))(st)
c__case_717_case__292 x1 x70 x86 x95 x101 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_717_case__292(x1)(x70)(x86)(x95)(x101)(x)(st))(i)(xs)(st)
c__case_717_case__292 x1 x70 x86 x95 x101 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_717_case__292")(x)



c__case_718_case__293 x1 x70 x86 x95 x98 x101 x100@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_717(x70)(x86)(x95)(x101)(x98)(x1)(st))(st)
c__case_718_case__293 x1 x70 x86 x95 x98 x101 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_718_case__293(x1)(x70)(x86)(x95)(x98)(x101)(x)(st))(i)(xs)(st)
c__case_718_case__293 x1 x70 x86 x95 x98 x101 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_718_case__293")(x)



c__case_719_case__294 x1 x70 x86 x95 x98 x100 x101 x113@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_718(x70)(x86)(x95)(x98)(x101)(x100)(x1)(st))(st)
c__case_719_case__294 x1 x70 x86 x95 x98 x100 x101 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_719_case__294(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x)(st))(i)(xs)(st)
c__case_719_case__294 x1 x70 x86 x95 x98 x100 x101 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_719_case__294")(x)



c__case_720_case__295 x1 x70 x86 x95 x98 x100 x101 x113 x112 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x112)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_719(x70)(x86)(x95)(x98)(x100)(x101)(x113)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_721_case__296 x1 x70 x86 x95 x98 x100 x101 x111@((Curry.Module.Prelude.:<) x112 x113) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_720(x70)(x86)(x95)(x98)(x100)(x101)(x113)(x112)(x1)(st))(st)
c__case_721_case__296 x1 x70 x86 x95 x98 x100 x101 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_721_case__296(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x)(st))(i)(xs)(st)
c__case_721_case__296 x1 x70 x86 x95 x98 x100 x101 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_721_case__296")(x)



c__case_722_case__297 x1 x70 x86 x95 x98 x100 x101 x111 x110 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x110)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_721(x70)(x86)(x95)(x98)(x100)(x101)(x111)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_723_case__298 x1 x70 x86 x95 x98 x100 x101 x109@((Curry.Module.Prelude.:<) x110 x111) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_722(x70)(x86)(x95)(x98)(x100)(x101)(x111)(x110)(x1)(st))(st)
c__case_723_case__298 x1 x70 x86 x95 x98 x100 x101 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_723_case__298(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x)(st))(i)(xs)(st)
c__case_723_case__298 x1 x70 x86 x95 x98 x100 x101 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_723_case__298")(x)



c__case_724_case__299 x1 x70 x86 x95 x98 x100 x101 x109 x108 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x108)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_723(x70)(x86)(x95)(x98)(x100)(x101)(x109)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_725_case__300 x1 x70 x86 x95 x98 x100 x101 x107@((Curry.Module.Prelude.:<) x108 x109) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_724(x70)(x86)(x95)(x98)(x100)(x101)(x109)(x108)(x1)(st))(st)
c__case_725_case__300 x1 x70 x86 x95 x98 x100 x101 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_725_case__300(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x)(st))(i)(xs)(st)
c__case_725_case__300 x1 x70 x86 x95 x98 x100 x101 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_725_case__300")(x)



c__case_726_case__301 x1 x70 x86 x95 x98 x100 x101 x107 x106 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x106)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_725(x70)(x86)(x95)(x98)(x100)(x101)(x107)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_727_case__302 x1 x70 x86 x95 x98 x100 x101 x105@((Curry.Module.Prelude.:<) x106 x107) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_726(x70)(x86)(x95)(x98)(x100)(x101)(x107)(x106)(x1)(st))(st)
c__case_727_case__302 x1 x70 x86 x95 x98 x100 x101 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_727_case__302(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x)(st))(i)(xs)(st)
c__case_727_case__302 x1 x70 x86 x95 x98 x100 x101 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_727_case__302")(x)



c__case_728_case__303 x1 x70 x86 x95 x98 x100 x101 x105 x104 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x104)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_727(x70)(x86)(x95)(x98)(x100)(x101)(x105)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_729_case__304 x1 x70 x86 x95 x98 x100 x101 x103@((Curry.Module.Prelude.:<) x104 x105) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_728(x70)(x86)(x95)(x98)(x100)(x101)(x105)(x104)(x1)(st))(st)
c__case_729_case__304 x1 x70 x86 x95 x98 x100 x101 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_729_case__304(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x)(st))(i)(xs)(st)
c__case_729_case__304 x1 x70 x86 x95 x98 x100 x101 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_729_case__304")(x)



c__case_730_case__305 x1 x70 x86 x95 x98 x100 x101 x103 x102 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x102)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_729(x70)(x86)(x95)(x98)(x100)(x101)(x103)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_731_case__306 x1 x70 x86 x95 x98 x100 x101 x99@((Curry.Module.Prelude.:<) x102 x103) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_730(x70)(x86)(x95)(x98)(x100)(x101)(x103)(x102)(x1)(st))(st)
c__case_731_case__306 x1 x70 x86 x95 x98 x100 x101 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_731_case__306(x1)(x70)(x86)(x95)(x98)(x100)(x101)(x)(st))(i)(xs)(st)
c__case_731_case__306 x1 x70 x86 x95 x98 x100 x101 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_731_case__306")(x)



c__case_732_case__307 x1 x70 x86 x95 x98 x97@(Curry.Module.XML.C_XElem x99 x100 x101) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_731(x70)(x86)(x95)(x98)(x100)(x101)(x99)(x1)(st))(st)
c__case_732_case__307 x1 x70 x86 x95 x98 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_732_case__307(x1)(x70)(x86)(x95)(x98)(x)(st))(i)(xs)(st)
c__case_732_case__307 x1 x70 x86 x95 x98 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_732_case__307")(x)



c__case_733_case__308 x1 x70 x86 x95 x5@((Curry.Module.Prelude.:<) x97 x98) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_732(x70)(x86)(x95)(x98)(x97)(x1)(st))(st)
c__case_733_case__308 x1 x70 x86 x95 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_733_case__308(x1)(x70)(x86)(x95)(x)(st))(i)(xs)(st)
c__case_733_case__308 x1 x70 x86 x95 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_733_case__308")(x)



c__case_734_case__309 x1 x5 x70 x86 x95 x96@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_733(x70)(x86)(x95)(x5)(x1)(st))(st)
c__case_734_case__309 x1 x5 x70 x86 x95 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_734_case__309(x1)(x5)(x70)(x86)(x95)(x)(st))(i)(xs)(st)
c__case_734_case__309 x1 x5 x70 x86 x95 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_734_case__309")(x)



c__case_735_case__310 x1 x5 x70 x86 x84@((Curry.Module.Prelude.:<) x95 x96) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_734(x5)(x70)(x86)(x95)(x96)(x1)(st))(st)
c__case_735_case__310 x1 x5 x70 x86 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_735_case__310(x1)(x5)(x70)(x86)(x)(st))(i)(xs)(st)
c__case_735_case__310 x1 x5 x70 x86 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_735_case__310")(x)



c__case_736_case__311 x1 x5 x70 x84 x86 x94@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_735(x5)(x70)(x86)(x84)(x1)(st))(st)
c__case_736_case__311 x1 x5 x70 x84 x86 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_736_case__311(x1)(x5)(x70)(x84)(x86)(x)(st))(i)(xs)(st)
c__case_736_case__311 x1 x5 x70 x84 x86 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_736_case__311")(x)



c__case_737_case__312 x1 x5 x70 x84 x86 x94 x93 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x93)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_736(x5)(x70)(x84)(x86)(x94)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_738_case__313 x1 x5 x70 x84 x86 x92@((Curry.Module.Prelude.:<) x93 x94) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_737(x5)(x70)(x84)(x86)(x94)(x93)(x1)(st))(st)
c__case_738_case__313 x1 x5 x70 x84 x86 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_738_case__313(x1)(x5)(x70)(x84)(x86)(x)(st))(i)(xs)(st)
c__case_738_case__313 x1 x5 x70 x84 x86 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_738_case__313")(x)



c__case_739_case__314 x1 x5 x70 x84 x86 x92 x91 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x91)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_738(x5)(x70)(x84)(x86)(x92)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_740_case__315 x1 x5 x70 x84 x86 x90@((Curry.Module.Prelude.:<) x91 x92) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_739(x5)(x70)(x84)(x86)(x92)(x91)(x1)(st))(st)
c__case_740_case__315 x1 x5 x70 x84 x86 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_740_case__315(x1)(x5)(x70)(x84)(x86)(x)(st))(i)(xs)(st)
c__case_740_case__315 x1 x5 x70 x84 x86 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_740_case__315")(x)



c__case_741_case__316 x1 x5 x70 x84 x86 x90 x89 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x89)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_740(x5)(x70)(x84)(x86)(x90)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_742_case__317 x1 x5 x70 x84 x86 x88@((Curry.Module.Prelude.:<) x89 x90) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_741(x5)(x70)(x84)(x86)(x90)(x89)(x1)(st))(st)
c__case_742_case__317 x1 x5 x70 x84 x86 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_742_case__317(x1)(x5)(x70)(x84)(x86)(x)(st))(i)(xs)(st)
c__case_742_case__317 x1 x5 x70 x84 x86 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_742_case__317")(x)



c__case_743_case__318 x1 x5 x70 x84 x86 x88 x87 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x87)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_742(x5)(x70)(x84)(x86)(x88)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_744_case__319 x1 x5 x70 x84 x86 x85@((Curry.Module.Prelude.:<) x87 x88) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_743(x5)(x70)(x84)(x86)(x88)(x87)(x1)(st))(st)
c__case_744_case__319 x1 x5 x70 x84 x86 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_744_case__319(x1)(x5)(x70)(x84)(x86)(x)(st))(i)(xs)(st)
c__case_744_case__319 x1 x5 x70 x84 x86 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_744_case__319")(x)



c__case_745_case__320 x1 x5 x70 x84 x83@(Curry.Module.Prelude.T2 x85 x86) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_744(x5)(x70)(x84)(x86)(x85)(x1)(st))(st)
c__case_745_case__320 x1 x5 x70 x84 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_745_case__320(x1)(x5)(x70)(x84)(x)(st))(i)(xs)(st)
c__case_745_case__320 x1 x5 x70 x84 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_745_case__320")(x)



c__case_746_case__321 x1 x5 x70 x68@((Curry.Module.Prelude.:<) x83 x84) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_745(x5)(x70)(x84)(x83)(x1)(st))(st)
c__case_746_case__321 x1 x5 x70 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_746_case__321(x1)(x5)(x70)(x)(st))(i)(xs)(st)
c__case_746_case__321 x1 x5 x70 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_746_case__321")(x)



c__case_747_case__322 x1 x5 x68 x70 x82@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_746(x5)(x70)(x68)(x1)(st))(st)
c__case_747_case__322 x1 x5 x68 x70 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_747_case__322(x1)(x5)(x68)(x70)(x)(st))(i)(xs)(st)
c__case_747_case__322 x1 x5 x68 x70 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_747_case__322")(x)



c__case_748_case__323 x1 x5 x68 x70 x82 x81 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x81)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_747(x5)(x68)(x70)(x82)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_749_case__324 x1 x5 x68 x70 x80@((Curry.Module.Prelude.:<) x81 x82) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_748(x5)(x68)(x70)(x82)(x81)(x1)(st))(st)
c__case_749_case__324 x1 x5 x68 x70 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_749_case__324(x1)(x5)(x68)(x70)(x)(st))(i)(xs)(st)
c__case_749_case__324 x1 x5 x68 x70 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_749_case__324")(x)



c__case_750_case__325 x1 x5 x68 x70 x80 x79 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x79)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_749(x5)(x68)(x70)(x80)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_751_case__326 x1 x5 x68 x70 x78@((Curry.Module.Prelude.:<) x79 x80) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_750(x5)(x68)(x70)(x80)(x79)(x1)(st))(st)
c__case_751_case__326 x1 x5 x68 x70 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_751_case__326(x1)(x5)(x68)(x70)(x)(st))(i)(xs)(st)
c__case_751_case__326 x1 x5 x68 x70 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_751_case__326")(x)



c__case_752_case__327 x1 x5 x68 x70 x78 x77 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x77)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_751(x5)(x68)(x70)(x78)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_753_case__328 x1 x5 x68 x70 x76@((Curry.Module.Prelude.:<) x77 x78) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_752(x5)(x68)(x70)(x78)(x77)(x1)(st))(st)
c__case_753_case__328 x1 x5 x68 x70 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_753_case__328(x1)(x5)(x68)(x70)(x)(st))(i)(xs)(st)
c__case_753_case__328 x1 x5 x68 x70 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_753_case__328")(x)



c__case_754_case__329 x1 x5 x68 x70 x76 x75 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x75)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_753(x5)(x68)(x70)(x76)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_755_case__330 x1 x5 x68 x70 x74@((Curry.Module.Prelude.:<) x75 x76) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_754(x5)(x68)(x70)(x76)(x75)(x1)(st))(st)
c__case_755_case__330 x1 x5 x68 x70 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_755_case__330(x1)(x5)(x68)(x70)(x)(st))(i)(xs)(st)
c__case_755_case__330 x1 x5 x68 x70 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_755_case__330")(x)



c__case_756_case__331 x1 x5 x68 x70 x74 x73 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x73)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_755(x5)(x68)(x70)(x74)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_757_case__332 x1 x5 x68 x70 x72@((Curry.Module.Prelude.:<) x73 x74) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_756(x5)(x68)(x70)(x74)(x73)(x1)(st))(st)
c__case_757_case__332 x1 x5 x68 x70 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_757_case__332(x1)(x5)(x68)(x70)(x)(st))(i)(xs)(st)
c__case_757_case__332 x1 x5 x68 x70 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_757_case__332")(x)



c__case_758_case__333 x1 x5 x68 x70 x72 x71 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x71)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_757(x5)(x68)(x70)(x72)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_759_case__334 x1 x5 x68 x70 x69@((Curry.Module.Prelude.:<) x71 x72) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_758(x5)(x68)(x70)(x72)(x71)(x1)(st))(st)
c__case_759_case__334 x1 x5 x68 x70 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_759_case__334(x1)(x5)(x68)(x70)(x)(st))(i)(xs)(st)
c__case_759_case__334 x1 x5 x68 x70 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_759_case__334")(x)



c__case_760_case__335 x1 x5 x68 x67@(Curry.Module.Prelude.T2 x69 x70) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_759(x5)(x68)(x70)(x69)(x1)(st))(st)
c__case_760_case__335 x1 x5 x68 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_760_case__335(x1)(x5)(x68)(x)(st))(i)(xs)(st)
c__case_760_case__335 x1 x5 x68 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_760_case__335")(x)



c__case_761_case__336 x1 x5 x4@((Curry.Module.Prelude.:<) x67 x68) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_760(x5)(x68)(x67)(x1)(st))(st)
c__case_761_case__336 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_761_case__336(x1)(x5)(x)(st))(i)(xs)(st)
c__case_761_case__336 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_761_case__336")(x)



c__case_762_case__337 x1 x4 x5 x66@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_761(x5)(x4)(x1)(st))(st)
c__case_762_case__337 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_762_case__337(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_762_case__337 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_762_case__337")(x)



c__case_763_case__338 x1 x4 x5 x66 x65 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x65)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_762(x4)(x5)(x66)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_764_case__339 x1 x4 x5 x64@((Curry.Module.Prelude.:<) x65 x66) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_763(x4)(x5)(x66)(x65)(x1)(st))(st)
c__case_764_case__339 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_764_case__339(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_764_case__339 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_764_case__339")(x)



c__case_765_case__340 x1 x4 x5 x64 x63 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x63)(Curry.Module.Prelude.C_Char('y'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_764(x4)(x5)(x64)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_766_case__341 x1 x4 x5 x62@((Curry.Module.Prelude.:<) x63 x64) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_765(x4)(x5)(x64)(x63)(x1)(st))(st)
c__case_766_case__341 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_766_case__341(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_766_case__341 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_766_case__341")(x)



c__case_767_case__342 x1 x4 x5 x62 x61 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x61)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_766(x4)(x5)(x62)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_812_case__343 x1 x4 x5 x13@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_811(x5)(x4)(x1)(st))(st)
c__case_812_case__343 x1 x4 x5 x13@((Curry.Module.Prelude.:<) x61 x62) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_767(x4)(x5)(x62)(x61)(x1)(st))(st)
c__case_812_case__343 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_812_case__343(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_812_case__343 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_812_case__343")(x)



c__case_813_case__344 x1 x4 x5 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_812(x4)(x5)(x13)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_814_case__345 x1 x4 x5 x11@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_813(x4)(x5)(x13)(x12)(x1)(st))(st)
c__case_814_case__345 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_814_case__345(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_814_case__345 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_814_case__345")(x)



c__case_815_case__346 x1 x4 x5 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_814(x4)(x5)(x11)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_816_case__347 x1 x4 x5 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_815(x4)(x5)(x11)(x10)(x1)(st))(st)
c__case_816_case__347 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_816_case__347(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_816_case__347 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_816_case__347")(x)



c__case_817_case__348 x1 x4 x5 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('y'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_816(x4)(x5)(x9)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_818_case__349 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_817(x4)(x5)(x9)(x8)(x1)(st))(st)
c__case_818_case__349 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_818_case__349(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_818_case__349 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_818_case__349")(x)



c__case_819_case__350 x1 x4 x5 x7 x6 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_818(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_820_case__351 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_819(x4)(x5)(x7)(x6)(x1)(st))(st)
c__case_820_case__351 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_820_case__351(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_820_case__351 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_820_case__351")(x)



c__case_821_case__352 x1 x2@(Curry.Module.XML.C_XElem x3 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_820(x4)(x5)(x3)(x1)(st))(st)
c__case_821_case__352 x1 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_821_case__352(x1)(x)(st))(i)(xs)(st)
c__case_821_case__352 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_821_case__352")(x)



c__case_705_case__353 x1 x5 x4@Curry.Module.Prelude.List st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleRead.c_readNat(Curry.Module.OracleXML.c_textOfXml(x5)(x1)(st))(x6)(st))(st)
c__case_705_case__353 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_705_case__353(x1)(x5)(x)(st))(i)(xs)(st)
c__case_705_case__353 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_705_case__353")(x)



c__case_706_case__354 x1 x4 x5 x13@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_705(x5)(x4)(x1)(st))(st)
c__case_706_case__354 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_706_case__354(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_706_case__354 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_706_case__354")(x)



c__case_707_case__355 x1 x4 x5 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_706(x4)(x5)(x13)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_708_case__356 x1 x4 x5 x11@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_707(x4)(x5)(x13)(x12)(x1)(st))(st)
c__case_708_case__356 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_708_case__356(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_708_case__356 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_708_case__356")(x)



c__case_709_case__357 x1 x4 x5 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_708(x4)(x5)(x11)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_710_case__358 x1 x4 x5 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_709(x4)(x5)(x11)(x10)(x1)(st))(st)
c__case_710_case__358 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_710_case__358(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_710_case__358 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_710_case__358")(x)



c__case_711_case__359 x1 x4 x5 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('v'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_710(x4)(x5)(x9)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_712_case__360 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_711(x4)(x5)(x9)(x8)(x1)(st))(st)
c__case_712_case__360 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_712_case__360(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_712_case__360 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_712_case__360")(x)



c__case_713_case__361 x1 x4 x5 x7 x6 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_712(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_714_case__362 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_713(x4)(x5)(x7)(x6)(x1)(st))(st)
c__case_714_case__362 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_714_case__362(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_714_case__362 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_714_case__362")(x)



c__case_715_case__363 x1 x2@(Curry.Module.XML.C_XElem x3 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_714(x4)(x5)(x3)(x1)(st))(st)
c__case_715_case__363 x1 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_715_case__363(x1)(x)(st))(i)(xs)(st)
c__case_715_case__363 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_715_case__363")(x)



c__case_654_case__364 x1 x5 x17 x33 x45 x56 x57@Curry.Module.Prelude.List st = let {x58 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x59 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x58)((Curry.Module.Prelude.:<)(x59)(Curry.Module.Prelude.List)))(Curry.Module.FlatCurry.C_Cons(Curry.Module.Prelude.T2(x17)(x33))(Curry.Module.OracleRead.c_readNat(x45)(x1)(st))(Curry.Module.OracleFlatCurryXML.c_xvis2vis(x56)(x58)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_flatx2texp))))(x5)(x59)(st)))(st)
c__case_654_case__364 x1 x5 x17 x33 x45 x56 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_654_case__364(x1)(x5)(x17)(x33)(x45)(x56)(x)(st))(i)(xs)(st)
c__case_654_case__364 x1 x5 x17 x33 x45 x56 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_654_case__364")(x)



c__case_655_case__365 x1 x5 x17 x33 x45 x43@((Curry.Module.Prelude.:<) x56 x57) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_654(x5)(x17)(x33)(x45)(x56)(x57)(x1)(st))(st)
c__case_655_case__365 x1 x5 x17 x33 x45 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_655_case__365(x1)(x5)(x17)(x33)(x45)(x)(st))(i)(xs)(st)
c__case_655_case__365 x1 x5 x17 x33 x45 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_655_case__365")(x)



c__case_656_case__366 x1 x5 x17 x33 x43 x45 x55@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_655(x5)(x17)(x33)(x45)(x43)(x1)(st))(st)
c__case_656_case__366 x1 x5 x17 x33 x43 x45 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_656_case__366(x1)(x5)(x17)(x33)(x43)(x45)(x)(st))(i)(xs)(st)
c__case_656_case__366 x1 x5 x17 x33 x43 x45 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_656_case__366")(x)



c__case_657_case__367 x1 x5 x17 x33 x43 x45 x55 x54 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x54)(Curry.Module.Prelude.C_Char('y'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_656(x5)(x17)(x33)(x43)(x45)(x55)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_658_case__368 x1 x5 x17 x33 x43 x45 x53@((Curry.Module.Prelude.:<) x54 x55) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_657(x5)(x17)(x33)(x43)(x45)(x55)(x54)(x1)(st))(st)
c__case_658_case__368 x1 x5 x17 x33 x43 x45 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_658_case__368(x1)(x5)(x17)(x33)(x43)(x45)(x)(st))(i)(xs)(st)
c__case_658_case__368 x1 x5 x17 x33 x43 x45 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_658_case__368")(x)



c__case_659_case__369 x1 x5 x17 x33 x43 x45 x53 x52 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x52)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_658(x5)(x17)(x33)(x43)(x45)(x53)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_660_case__370 x1 x5 x17 x33 x43 x45 x51@((Curry.Module.Prelude.:<) x52 x53) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_659(x5)(x17)(x33)(x43)(x45)(x53)(x52)(x1)(st))(st)
c__case_660_case__370 x1 x5 x17 x33 x43 x45 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_660_case__370(x1)(x5)(x17)(x33)(x43)(x45)(x)(st))(i)(xs)(st)
c__case_660_case__370 x1 x5 x17 x33 x43 x45 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_660_case__370")(x)



c__case_661_case__371 x1 x5 x17 x33 x43 x45 x51 x50 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x50)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_660(x5)(x17)(x33)(x43)(x45)(x51)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_662_case__372 x1 x5 x17 x33 x43 x45 x49@((Curry.Module.Prelude.:<) x50 x51) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_661(x5)(x17)(x33)(x43)(x45)(x51)(x50)(x1)(st))(st)
c__case_662_case__372 x1 x5 x17 x33 x43 x45 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_662_case__372(x1)(x5)(x17)(x33)(x43)(x45)(x)(st))(i)(xs)(st)
c__case_662_case__372 x1 x5 x17 x33 x43 x45 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_662_case__372")(x)



c__case_663_case__373 x1 x5 x17 x33 x43 x45 x49 x48 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x48)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_662(x5)(x17)(x33)(x43)(x45)(x49)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_664_case__374 x1 x5 x17 x33 x43 x45 x47@((Curry.Module.Prelude.:<) x48 x49) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_663(x5)(x17)(x33)(x43)(x45)(x49)(x48)(x1)(st))(st)
c__case_664_case__374 x1 x5 x17 x33 x43 x45 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_664_case__374(x1)(x5)(x17)(x33)(x43)(x45)(x)(st))(i)(xs)(st)
c__case_664_case__374 x1 x5 x17 x33 x43 x45 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_664_case__374")(x)



c__case_665_case__375 x1 x5 x17 x33 x43 x45 x47 x46 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x46)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_664(x5)(x17)(x33)(x43)(x45)(x47)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_666_case__376 x1 x5 x17 x33 x43 x45 x44@((Curry.Module.Prelude.:<) x46 x47) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_665(x5)(x17)(x33)(x43)(x45)(x47)(x46)(x1)(st))(st)
c__case_666_case__376 x1 x5 x17 x33 x43 x45 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_666_case__376(x1)(x5)(x17)(x33)(x43)(x45)(x)(st))(i)(xs)(st)
c__case_666_case__376 x1 x5 x17 x33 x43 x45 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_666_case__376")(x)



c__case_667_case__377 x1 x5 x17 x33 x43 x42@(Curry.Module.Prelude.T2 x44 x45) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_666(x5)(x17)(x33)(x43)(x45)(x44)(x1)(st))(st)
c__case_667_case__377 x1 x5 x17 x33 x43 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_667_case__377(x1)(x5)(x17)(x33)(x43)(x)(st))(i)(xs)(st)
c__case_667_case__377 x1 x5 x17 x33 x43 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_667_case__377")(x)



c__case_668_case__378 x1 x5 x17 x33 x31@((Curry.Module.Prelude.:<) x42 x43) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_667(x5)(x17)(x33)(x43)(x42)(x1)(st))(st)
c__case_668_case__378 x1 x5 x17 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_668_case__378(x1)(x5)(x17)(x33)(x)(st))(i)(xs)(st)
c__case_668_case__378 x1 x5 x17 x33 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_668_case__378")(x)



c__case_669_case__379 x1 x5 x17 x31 x33 x41@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_668(x5)(x17)(x33)(x31)(x1)(st))(st)
c__case_669_case__379 x1 x5 x17 x31 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_669_case__379(x1)(x5)(x17)(x31)(x33)(x)(st))(i)(xs)(st)
c__case_669_case__379 x1 x5 x17 x31 x33 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_669_case__379")(x)



c__case_670_case__380 x1 x5 x17 x31 x33 x41 x40 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x40)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_669(x5)(x17)(x31)(x33)(x41)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_671_case__381 x1 x5 x17 x31 x33 x39@((Curry.Module.Prelude.:<) x40 x41) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_670(x5)(x17)(x31)(x33)(x41)(x40)(x1)(st))(st)
c__case_671_case__381 x1 x5 x17 x31 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_671_case__381(x1)(x5)(x17)(x31)(x33)(x)(st))(i)(xs)(st)
c__case_671_case__381 x1 x5 x17 x31 x33 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_671_case__381")(x)



c__case_672_case__382 x1 x5 x17 x31 x33 x39 x38 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x38)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_671(x5)(x17)(x31)(x33)(x39)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_673_case__383 x1 x5 x17 x31 x33 x37@((Curry.Module.Prelude.:<) x38 x39) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_672(x5)(x17)(x31)(x33)(x39)(x38)(x1)(st))(st)
c__case_673_case__383 x1 x5 x17 x31 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_673_case__383(x1)(x5)(x17)(x31)(x33)(x)(st))(i)(xs)(st)
c__case_673_case__383 x1 x5 x17 x31 x33 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_673_case__383")(x)



c__case_674_case__384 x1 x5 x17 x31 x33 x37 x36 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x36)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_673(x5)(x17)(x31)(x33)(x37)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_675_case__385 x1 x5 x17 x31 x33 x35@((Curry.Module.Prelude.:<) x36 x37) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_674(x5)(x17)(x31)(x33)(x37)(x36)(x1)(st))(st)
c__case_675_case__385 x1 x5 x17 x31 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_675_case__385(x1)(x5)(x17)(x31)(x33)(x)(st))(i)(xs)(st)
c__case_675_case__385 x1 x5 x17 x31 x33 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_675_case__385")(x)



c__case_676_case__386 x1 x5 x17 x31 x33 x35 x34 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x34)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_675(x5)(x17)(x31)(x33)(x35)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_677_case__387 x1 x5 x17 x31 x33 x32@((Curry.Module.Prelude.:<) x34 x35) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_676(x5)(x17)(x31)(x33)(x35)(x34)(x1)(st))(st)
c__case_677_case__387 x1 x5 x17 x31 x33 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_677_case__387(x1)(x5)(x17)(x31)(x33)(x)(st))(i)(xs)(st)
c__case_677_case__387 x1 x5 x17 x31 x33 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_677_case__387")(x)



c__case_678_case__388 x1 x5 x17 x31 x30@(Curry.Module.Prelude.T2 x32 x33) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_677(x5)(x17)(x31)(x33)(x32)(x1)(st))(st)
c__case_678_case__388 x1 x5 x17 x31 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_678_case__388(x1)(x5)(x17)(x31)(x)(st))(i)(xs)(st)
c__case_678_case__388 x1 x5 x17 x31 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_678_case__388")(x)



c__case_679_case__389 x1 x5 x17 x15@((Curry.Module.Prelude.:<) x30 x31) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_678(x5)(x17)(x31)(x30)(x1)(st))(st)
c__case_679_case__389 x1 x5 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_679_case__389(x1)(x5)(x17)(x)(st))(i)(xs)(st)
c__case_679_case__389 x1 x5 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_679_case__389")(x)



c__case_680_case__390 x1 x5 x15 x17 x29@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_679(x5)(x17)(x15)(x1)(st))(st)
c__case_680_case__390 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_680_case__390(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_680_case__390 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_680_case__390")(x)



c__case_681_case__391 x1 x5 x15 x17 x29 x28 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x28)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_680(x5)(x15)(x17)(x29)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_682_case__392 x1 x5 x15 x17 x27@((Curry.Module.Prelude.:<) x28 x29) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_681(x5)(x15)(x17)(x29)(x28)(x1)(st))(st)
c__case_682_case__392 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_682_case__392(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_682_case__392 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_682_case__392")(x)



c__case_683_case__393 x1 x5 x15 x17 x27 x26 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x26)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_682(x5)(x15)(x17)(x27)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_684_case__394 x1 x5 x15 x17 x25@((Curry.Module.Prelude.:<) x26 x27) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_683(x5)(x15)(x17)(x27)(x26)(x1)(st))(st)
c__case_684_case__394 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_684_case__394(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_684_case__394 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_684_case__394")(x)



c__case_685_case__395 x1 x5 x15 x17 x25 x24 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x24)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_684(x5)(x15)(x17)(x25)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_686_case__396 x1 x5 x15 x17 x23@((Curry.Module.Prelude.:<) x24 x25) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_685(x5)(x15)(x17)(x25)(x24)(x1)(st))(st)
c__case_686_case__396 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_686_case__396(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_686_case__396 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_686_case__396")(x)



c__case_687_case__397 x1 x5 x15 x17 x23 x22 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x22)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_686(x5)(x15)(x17)(x23)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_688_case__398 x1 x5 x15 x17 x21@((Curry.Module.Prelude.:<) x22 x23) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_687(x5)(x15)(x17)(x23)(x22)(x1)(st))(st)
c__case_688_case__398 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_688_case__398(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_688_case__398 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_688_case__398")(x)



c__case_689_case__399 x1 x5 x15 x17 x21 x20 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x20)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_688(x5)(x15)(x17)(x21)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_690_case__400 x1 x5 x15 x17 x19@((Curry.Module.Prelude.:<) x20 x21) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_689(x5)(x15)(x17)(x21)(x20)(x1)(st))(st)
c__case_690_case__400 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_690_case__400(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_690_case__400 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_690_case__400")(x)



c__case_691_case__401 x1 x5 x15 x17 x19 x18 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x18)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_690(x5)(x15)(x17)(x19)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_692_case__402 x1 x5 x15 x17 x16@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_691(x5)(x15)(x17)(x19)(x18)(x1)(st))(st)
c__case_692_case__402 x1 x5 x15 x17 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_692_case__402(x1)(x5)(x15)(x17)(x)(st))(i)(xs)(st)
c__case_692_case__402 x1 x5 x15 x17 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_692_case__402")(x)



c__case_693_case__403 x1 x5 x15 x14@(Curry.Module.Prelude.T2 x16 x17) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_692(x5)(x15)(x17)(x16)(x1)(st))(st)
c__case_693_case__403 x1 x5 x15 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_693_case__403(x1)(x5)(x15)(x)(st))(i)(xs)(st)
c__case_693_case__403 x1 x5 x15 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_693_case__403")(x)



c__case_694_case__404 x1 x5 x4@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_693(x5)(x15)(x14)(x1)(st))(st)
c__case_694_case__404 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_694_case__404(x1)(x5)(x)(st))(i)(xs)(st)
c__case_694_case__404 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_694_case__404")(x)



c__case_695_case__405 x1 x4 x5 x13@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_694(x5)(x4)(x1)(st))(st)
c__case_695_case__405 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_695_case__405(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_695_case__405 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_695_case__405")(x)



c__case_696_case__406 x1 x4 x5 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_695(x4)(x5)(x13)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_697_case__407 x1 x4 x5 x11@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_696(x4)(x5)(x13)(x12)(x1)(st))(st)
c__case_697_case__407 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_697_case__407(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_697_case__407 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_697_case__407")(x)



c__case_698_case__408 x1 x4 x5 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_697(x4)(x5)(x11)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_699_case__409 x1 x4 x5 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_698(x4)(x5)(x11)(x10)(x1)(st))(st)
c__case_699_case__409 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_699_case__409(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_699_case__409 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_699_case__409")(x)



c__case_700_case__410 x1 x4 x5 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_699(x4)(x5)(x9)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_701_case__411 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_700(x4)(x5)(x9)(x8)(x1)(st))(st)
c__case_701_case__411 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_701_case__411(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_701_case__411 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_701_case__411")(x)



c__case_702_case__412 x1 x4 x5 x7 x6 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_701(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_703_case__413 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_702(x4)(x5)(x7)(x6)(x1)(st))(st)
c__case_703_case__413 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_703_case__413(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_703_case__413 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_703_case__413")(x)



c__case_704_case__414 x1 x2@(Curry.Module.XML.C_XElem x3 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_703(x4)(x5)(x3)(x1)(st))(st)
c__case_704_case__414 x1 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_704_case__414(x1)(x)(st))(i)(xs)(st)
c__case_704_case__414 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_704_case__414")(x)



c__case_643_case__415 x1 x5 x4@Curry.Module.Prelude.List st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleRead.c_readNat(Curry.Module.OracleXML.c_textOfXml(x5)(x1)(st))(x6)(st))(st)
c__case_643_case__415 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_643_case__415(x1)(x5)(x)(st))(i)(xs)(st)
c__case_643_case__415 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_643_case__415")(x)



c__case_644_case__416 x1 x4 x5 x13@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_643(x5)(x4)(x1)(st))(st)
c__case_644_case__416 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_644_case__416(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_644_case__416 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_644_case__416")(x)



c__case_645_case__417 x1 x4 x5 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_644(x4)(x5)(x13)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_646_case__418 x1 x4 x5 x11@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_645(x4)(x5)(x13)(x12)(x1)(st))(st)
c__case_646_case__418 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_646_case__418(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_646_case__418 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_646_case__418")(x)



c__case_647_case__419 x1 x4 x5 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_646(x4)(x5)(x11)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_648_case__420 x1 x4 x5 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_647(x4)(x5)(x11)(x10)(x1)(st))(st)
c__case_648_case__420 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_648_case__420(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_648_case__420 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_648_case__420")(x)



c__case_649_case__421 x1 x4 x5 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('v'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_648(x4)(x5)(x9)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_650_case__422 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_649(x4)(x5)(x9)(x8)(x1)(st))(st)
c__case_650_case__422 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_650_case__422(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_650_case__422 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_650_case__422")(x)



c__case_651_case__423 x1 x4 x5 x7 x6 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_650(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_652_case__424 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_651(x4)(x5)(x7)(x6)(x1)(st))(st)
c__case_652_case__424 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_652_case__424(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_652_case__424 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_652_case__424")(x)



c__case_653_case__425 x1 x2@(Curry.Module.XML.C_XElem x3 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_652(x4)(x5)(x3)(x1)(st))(st)
c__case_653_case__425 x1 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_653_case__425(x1)(x)(st))(i)(xs)(st)
c__case_653_case__425 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_653_case__425")(x)



c__case_624_case__426 x1 x5 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.FlatCurry.C_External(Curry.Module.OracleXML.c_textOfXml(x5)(x1)(st)))(st)
c__case_624_case__426 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_624_case__426(x1)(x5)(x)(st))(i)(xs)(st)
c__case_624_case__426 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_624_case__426")(x)



c__case_625_case__427 x1 x4 x5 x21@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_624(x5)(x4)(x1)(st))(st)
c__case_625_case__427 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_625_case__427(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_625_case__427 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_625_case__427")(x)



c__case_626_case__428 x1 x4 x5 x21 x20 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x20)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_625(x4)(x5)(x21)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_627_case__429 x1 x4 x5 x19@((Curry.Module.Prelude.:<) x20 x21) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_626(x4)(x5)(x21)(x20)(x1)(st))(st)
c__case_627_case__429 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_627_case__429(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_627_case__429 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_627_case__429")(x)



c__case_628_case__430 x1 x4 x5 x19 x18 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x18)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_627(x4)(x5)(x19)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_629_case__431 x1 x4 x5 x17@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_628(x4)(x5)(x19)(x18)(x1)(st))(st)
c__case_629_case__431 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_629_case__431(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_629_case__431 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_629_case__431")(x)



c__case_630_case__432 x1 x4 x5 x17 x16 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x16)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_629(x4)(x5)(x17)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_631_case__433 x1 x4 x5 x15@((Curry.Module.Prelude.:<) x16 x17) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_630(x4)(x5)(x17)(x16)(x1)(st))(st)
c__case_631_case__433 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_631_case__433(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_631_case__433 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_631_case__433")(x)



c__case_632_case__434 x1 x4 x5 x15 x14 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x14)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_631(x4)(x5)(x15)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_633_case__435 x1 x4 x5 x13@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_632(x4)(x5)(x15)(x14)(x1)(st))(st)
c__case_633_case__435 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_633_case__435(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_633_case__435 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_633_case__435")(x)



c__case_634_case__436 x1 x4 x5 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_633(x4)(x5)(x13)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_635_case__437 x1 x4 x5 x11@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_634(x4)(x5)(x13)(x12)(x1)(st))(st)
c__case_635_case__437 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_635_case__437(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_635_case__437 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_635_case__437")(x)



c__case_636_case__438 x1 x4 x5 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_635(x4)(x5)(x11)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_637_case__439 x1 x4 x5 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_636(x4)(x5)(x11)(x10)(x1)(st))(st)
c__case_637_case__439 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_637_case__439(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_637_case__439 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_637_case__439")(x)



c__case_638_case__440 x1 x4 x5 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('x'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_637(x4)(x5)(x9)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_639_case__441 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_638(x4)(x5)(x9)(x8)(x1)(st))(st)
c__case_639_case__441 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_639_case__441(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_639_case__441 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_639_case__441")(x)



c__case_593_case__442 x1 x32 x50 x40@Curry.Module.Prelude.List st = let {x51 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x51)(Curry.Module.Prelude.List))(Curry.Module.FlatCurry.C_Rule(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_flatx2var))))(x32)(x1)(st))(Curry.Module.OracleFlatCurryXML.c_flatx2exp(x50)(x51)(st)))(st)
c__case_593_case__442 x1 x32 x50 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_593_case__442(x1)(x32)(x50)(x)(st))(i)(xs)(st)
c__case_593_case__442 x1 x32 x50 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_593_case__442")(x)



c__case_594_case__443 x1 x32 x40 x50 x51@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_593(x32)(x50)(x40)(x1)(st))(st)
c__case_594_case__443 x1 x32 x40 x50 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_594_case__443(x1)(x32)(x40)(x50)(x)(st))(i)(xs)(st)
c__case_594_case__443 x1 x32 x40 x50 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_594_case__443")(x)



c__case_595_case__444 x1 x32 x40 x43@((Curry.Module.Prelude.:<) x50 x51) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_594(x32)(x40)(x50)(x51)(x1)(st))(st)
c__case_595_case__444 x1 x32 x40 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_595_case__444(x1)(x32)(x40)(x)(st))(i)(xs)(st)
c__case_595_case__444 x1 x32 x40 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_595_case__444")(x)



c__case_596_case__445 x1 x32 x40 x43 x42@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_595(x32)(x40)(x43)(x1)(st))(st)
c__case_596_case__445 x1 x32 x40 x43 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_596_case__445(x1)(x32)(x40)(x43)(x)(st))(i)(xs)(st)
c__case_596_case__445 x1 x32 x40 x43 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_596_case__445")(x)



c__case_597_case__446 x1 x32 x40 x42 x43 x49@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_596(x32)(x40)(x43)(x42)(x1)(st))(st)
c__case_597_case__446 x1 x32 x40 x42 x43 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_597_case__446(x1)(x32)(x40)(x42)(x43)(x)(st))(i)(xs)(st)
c__case_597_case__446 x1 x32 x40 x42 x43 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_597_case__446")(x)



c__case_598_case__447 x1 x32 x40 x42 x43 x49 x48 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x48)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_597(x32)(x40)(x42)(x43)(x49)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_599_case__448 x1 x32 x40 x42 x43 x47@((Curry.Module.Prelude.:<) x48 x49) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_598(x32)(x40)(x42)(x43)(x49)(x48)(x1)(st))(st)
c__case_599_case__448 x1 x32 x40 x42 x43 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_599_case__448(x1)(x32)(x40)(x42)(x43)(x)(st))(i)(xs)(st)
c__case_599_case__448 x1 x32 x40 x42 x43 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_599_case__448")(x)



c__case_600_case__449 x1 x32 x40 x42 x43 x47 x46 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x46)(Curry.Module.Prelude.C_Char('h'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_599(x32)(x40)(x42)(x43)(x47)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_601_case__450 x1 x32 x40 x42 x43 x45@((Curry.Module.Prelude.:<) x46 x47) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_600(x32)(x40)(x42)(x43)(x47)(x46)(x1)(st))(st)
c__case_601_case__450 x1 x32 x40 x42 x43 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_601_case__450(x1)(x32)(x40)(x42)(x43)(x)(st))(i)(xs)(st)
c__case_601_case__450 x1 x32 x40 x42 x43 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_601_case__450")(x)



c__case_602_case__451 x1 x32 x40 x42 x43 x45 x44 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x44)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_601(x32)(x40)(x42)(x43)(x45)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_603_case__452 x1 x32 x40 x42 x43 x41@((Curry.Module.Prelude.:<) x44 x45) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_602(x32)(x40)(x42)(x43)(x45)(x44)(x1)(st))(st)
c__case_603_case__452 x1 x32 x40 x42 x43 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_603_case__452(x1)(x32)(x40)(x42)(x43)(x)(st))(i)(xs)(st)
c__case_603_case__452 x1 x32 x40 x42 x43 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_603_case__452")(x)



c__case_604_case__453 x1 x32 x40 x39@(Curry.Module.XML.C_XElem x41 x42 x43) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_603(x32)(x40)(x42)(x43)(x41)(x1)(st))(st)
c__case_604_case__453 x1 x32 x40 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_604_case__453(x1)(x32)(x40)(x)(st))(i)(xs)(st)
c__case_604_case__453 x1 x32 x40 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_604_case__453")(x)



c__case_605_case__454 x1 x32 x29@((Curry.Module.Prelude.:<) x39 x40) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_604(x32)(x40)(x39)(x1)(st))(st)
c__case_605_case__454 x1 x32 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_605_case__454(x1)(x32)(x)(st))(i)(xs)(st)
c__case_605_case__454 x1 x32 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_605_case__454")(x)



c__case_606_case__455 x1 x29 x32 x31@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_605(x32)(x29)(x1)(st))(st)
c__case_606_case__455 x1 x29 x32 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_606_case__455(x1)(x29)(x32)(x)(st))(i)(xs)(st)
c__case_606_case__455 x1 x29 x32 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_606_case__455")(x)



c__case_607_case__456 x1 x29 x31 x32 x38@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_606(x29)(x32)(x31)(x1)(st))(st)
c__case_607_case__456 x1 x29 x31 x32 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_607_case__456(x1)(x29)(x31)(x32)(x)(st))(i)(xs)(st)
c__case_607_case__456 x1 x29 x31 x32 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_607_case__456")(x)



c__case_608_case__457 x1 x29 x31 x32 x38 x37 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x37)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_607(x29)(x31)(x32)(x38)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_609_case__458 x1 x29 x31 x32 x36@((Curry.Module.Prelude.:<) x37 x38) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_608(x29)(x31)(x32)(x38)(x37)(x1)(st))(st)
c__case_609_case__458 x1 x29 x31 x32 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_609_case__458(x1)(x29)(x31)(x32)(x)(st))(i)(xs)(st)
c__case_609_case__458 x1 x29 x31 x32 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_609_case__458")(x)



c__case_610_case__459 x1 x29 x31 x32 x36 x35 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x35)(Curry.Module.Prelude.C_Char('h'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_609(x29)(x31)(x32)(x36)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_611_case__460 x1 x29 x31 x32 x34@((Curry.Module.Prelude.:<) x35 x36) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_610(x29)(x31)(x32)(x36)(x35)(x1)(st))(st)
c__case_611_case__460 x1 x29 x31 x32 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_611_case__460(x1)(x29)(x31)(x32)(x)(st))(i)(xs)(st)
c__case_611_case__460 x1 x29 x31 x32 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_611_case__460")(x)



c__case_612_case__461 x1 x29 x31 x32 x34 x33 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x33)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_611(x29)(x31)(x32)(x34)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_613_case__462 x1 x29 x31 x32 x30@((Curry.Module.Prelude.:<) x33 x34) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_612(x29)(x31)(x32)(x34)(x33)(x1)(st))(st)
c__case_613_case__462 x1 x29 x31 x32 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_613_case__462(x1)(x29)(x31)(x32)(x)(st))(i)(xs)(st)
c__case_613_case__462 x1 x29 x31 x32 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_613_case__462")(x)



c__case_614_case__463 x1 x29 x28@(Curry.Module.XML.C_XElem x30 x31 x32) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_613(x29)(x31)(x32)(x30)(x1)(st))(st)
c__case_614_case__463 x1 x29 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_614_case__463(x1)(x29)(x)(st))(i)(xs)(st)
c__case_614_case__463 x1 x29 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_614_case__463")(x)



c__case_615_case__464 x1 x5@((Curry.Module.Prelude.:<) x28 x29) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_614(x29)(x28)(x1)(st))(st)
c__case_615_case__464 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_615_case__464(x1)(x)(st))(i)(xs)(st)
c__case_615_case__464 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_615_case__464")(x)



c__case_616_case__465 x1 x5 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_615(x5)(x1)(st))(st)
c__case_616_case__465 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_616_case__465(x1)(x5)(x)(st))(i)(xs)(st)
c__case_616_case__465 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_616_case__465")(x)



c__case_617_case__466 x1 x4 x5 x27@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_616(x5)(x4)(x1)(st))(st)
c__case_617_case__466 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_617_case__466(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_617_case__466 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_617_case__466")(x)



c__case_618_case__467 x1 x4 x5 x27 x26 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x26)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_617(x4)(x5)(x27)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_619_case__468 x1 x4 x5 x25@((Curry.Module.Prelude.:<) x26 x27) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_618(x4)(x5)(x27)(x26)(x1)(st))(st)
c__case_619_case__468 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_619_case__468(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_619_case__468 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_619_case__468")(x)



c__case_620_case__469 x1 x4 x5 x25 x24 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x24)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_619(x4)(x5)(x25)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_621_case__470 x1 x4 x5 x23@((Curry.Module.Prelude.:<) x24 x25) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_620(x4)(x5)(x25)(x24)(x1)(st))(st)
c__case_621_case__470 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_621_case__470(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_621_case__470 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_621_case__470")(x)



c__case_622_case__471 x1 x4 x5 x23 x22 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x22)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_621(x4)(x5)(x23)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_623_case__472 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x22 x23) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_622(x4)(x5)(x23)(x22)(x1)(st))(st)
c__case_623_case__472 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_623_case__472(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_623_case__472 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_623_case__472")(x)



c__case_640_case__473 x1 x4 x5 x7 x6 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_639(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_623(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st)



c__case_641_case__474 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_640(x4)(x5)(x7)(x6)(x1)(st))(st)
c__case_641_case__474 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_641_case__474(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_641_case__474 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_641_case__474")(x)



c__case_642_case__475 x1 x2@(Curry.Module.XML.C_XElem x3 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_641(x4)(x5)(x3)(x1)(st))(st)
c__case_642_case__475 x1 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_642_case__475(x1)(x)(st))(i)(xs)(st)
c__case_642_case__475 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_642_case__475")(x)



c__case_584_case__476 x1 x5 x4@Curry.Module.Prelude.List st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleRead.c_readNat(Curry.Module.OracleXML.c_textOfXml(x5)(x1)(st))(x6)(st))(st)
c__case_584_case__476 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_584_case__476(x1)(x5)(x)(st))(i)(xs)(st)
c__case_584_case__476 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_584_case__476")(x)



c__case_585_case__477 x1 x4 x5 x11@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_584(x5)(x4)(x1)(st))(st)
c__case_585_case__477 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_585_case__477(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_585_case__477 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_585_case__477")(x)



c__case_586_case__478 x1 x4 x5 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_585(x4)(x5)(x11)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_587_case__479 x1 x4 x5 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_586(x4)(x5)(x11)(x10)(x1)(st))(st)
c__case_587_case__479 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_587_case__479(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_587_case__479 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_587_case__479")(x)



c__case_588_case__480 x1 x4 x5 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_587(x4)(x5)(x9)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_589_case__481 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_588(x4)(x5)(x9)(x8)(x1)(st))(st)
c__case_589_case__481 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_589_case__481(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_589_case__481 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_589_case__481")(x)



c__case_590_case__482 x1 x4 x5 x7 x6 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('v'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_589(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_591_case__483 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_590(x4)(x5)(x7)(x6)(x1)(st))(st)
c__case_591_case__483 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_591_case__483(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_591_case__483 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_591_case__483")(x)



c__case_592_case__484 x1 x2@(Curry.Module.XML.C_XElem x3 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_591(x4)(x5)(x3)(x1)(st))(st)
c__case_592_case__484 x1 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_592_case__484(x1)(x)(st))(i)(xs)(st)
c__case_592_case__484 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_592_case__484")(x)



c__case_575_case__485 x1 x5 x4@Curry.Module.Prelude.List st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.FlatCurry.C_Var(Curry.Module.OracleRead.c_readNat(Curry.Module.OracleXML.c_textOfXml(x5)(x1)(st))(x6)(st)))(st)
c__case_575_case__485 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_575_case__485(x1)(x5)(x)(st))(i)(xs)(st)
c__case_575_case__485 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_575_case__485")(x)



c__case_576_case__486 x1 x4 x5 x11@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_575(x5)(x4)(x1)(st))(st)
c__case_576_case__486 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_576_case__486(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_576_case__486 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_576_case__486")(x)



c__case_577_case__487 x1 x4 x5 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_576(x4)(x5)(x11)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_578_case__488 x1 x4 x5 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_577(x4)(x5)(x11)(x10)(x1)(st))(st)
c__case_578_case__488 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_578_case__488(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_578_case__488 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_578_case__488")(x)



c__case_579_case__489 x1 x4 x5 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_578(x4)(x5)(x9)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_580_case__490 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_579(x4)(x5)(x9)(x8)(x1)(st))(st)
c__case_580_case__490 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_580_case__490(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_580_case__490 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_580_case__490")(x)



c__case_567_case__491 x1 x16 x17@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.FlatCurry.C_Lit(Curry.Module.OracleFlatCurryXML.c_flatx2lit(x16)(x1)(st)))(st)
c__case_567_case__491 x1 x16 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_567_case__491(x1)(x16)(x)(st))(i)(xs)(st)
c__case_567_case__491 x1 x16 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_567_case__491")(x)



c__case_568_case__492 x1 x5@((Curry.Module.Prelude.:<) x16 x17) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_567(x16)(x17)(x1)(st))(st)
c__case_568_case__492 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_568_case__492(x1)(x)(st))(i)(xs)(st)
c__case_568_case__492 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_568_case__492")(x)



c__case_569_case__493 x1 x5 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_568(x5)(x1)(st))(st)
c__case_569_case__493 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_569_case__493(x1)(x5)(x)(st))(i)(xs)(st)
c__case_569_case__493 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_569_case__493")(x)



c__case_570_case__494 x1 x4 x5 x15@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_569(x5)(x4)(x1)(st))(st)
c__case_570_case__494 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_570_case__494(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_570_case__494 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_570_case__494")(x)



c__case_571_case__495 x1 x4 x5 x15 x14 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x14)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_570(x4)(x5)(x15)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_572_case__496 x1 x4 x5 x13@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_571(x4)(x5)(x15)(x14)(x1)(st))(st)
c__case_572_case__496 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_572_case__496(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_572_case__496 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_572_case__496")(x)



c__case_563_case__497 x1 x5 x4@Curry.Module.Prelude.List st = let {x23 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x23)(Curry.Module.Prelude.List))(let {x20 = Curry.Module.OracleFlatCurryXML.c_flatx2let(x5)(x1)(st)} in let {x24 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x23)((Curry.Module.Prelude.:<)(x24)(Curry.Module.Prelude.List))(let {x25 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x24)((Curry.Module.Prelude.:<)(x25)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x25)(Curry.Module.FlatCurry.C_Let(Curry.Module.OracleFlatCurryXML.c_flatx2exp'46_'35selFP3'35bindings(x20)(x23)(st))(Curry.Module.OracleFlatCurryXML.c_flatx2exp'46_'35selFP4'35exp(x20)(x24)(st)))(st))(st))(st))(st)
c__case_563_case__497 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_563_case__497(x1)(x5)(x)(st))(i)(xs)(st)
c__case_563_case__497 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_563_case__497")(x)



c__case_556_case__498 x1 x5 x4@Curry.Module.Prelude.List st = let {x32 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x32)(Curry.Module.Prelude.List))(let {x29 = Curry.Module.OracleFlatCurryXML.c_flatx2let(x5)(x1)(st)} in let {x33 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x32)((Curry.Module.Prelude.:<)(x33)(Curry.Module.Prelude.List))(let {x34 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x33)((Curry.Module.Prelude.:<)(x34)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x34)(Curry.Module.FlatCurry.C_Let(Curry.Module.OracleFlatCurryXML.c_flatx2exp'46_'35selFP6'35bindings(x29)(x32)(st))(Curry.Module.OracleFlatCurryXML.c_flatx2exp'46_'35selFP7'35exp(x29)(x33)(st)))(st))(st))(st))(st)
c__case_556_case__498 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_556_case__498(x1)(x5)(x)(st))(i)(xs)(st)
c__case_556_case__498 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_556_case__498")(x)



c__case_557_case__499 x1 x4 x5 x28@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_556(x5)(x4)(x1)(st))(st)
c__case_557_case__499 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_557_case__499(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_557_case__499 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_557_case__499")(x)



c__case_558_case__500 x1 x4 x5 x28 x27 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x27)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_557(x4)(x5)(x28)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_559_case__501 x1 x4 x5 x26@((Curry.Module.Prelude.:<) x27 x28) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_558(x4)(x5)(x28)(x27)(x1)(st))(st)
c__case_559_case__501 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_559_case__501(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_559_case__501 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_559_case__501")(x)



c__case_560_case__502 x1 x4 x5 x26 x25 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x25)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_559(x4)(x5)(x26)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_561_case__503 x1 x4 x5 x24@((Curry.Module.Prelude.:<) x25 x26) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_560(x4)(x5)(x26)(x25)(x1)(st))(st)
c__case_561_case__503 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_561_case__503(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_561_case__503 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_561_case__503")(x)



c__case_562_case__504 x1 x4 x5 x24 x23 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x23)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_561(x4)(x5)(x24)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_564_case__505 x1 x4 x5 x19@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_563(x5)(x4)(x1)(st))(st)
c__case_564_case__505 x1 x4 x5 x19@((Curry.Module.Prelude.:<) x23 x24) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_562(x4)(x5)(x24)(x23)(x1)(st))(st)
c__case_564_case__505 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_564_case__505(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_564_case__505 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_564_case__505")(x)



c__case_565_case__506 x1 x4 x5 x19 x18 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x18)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_564(x4)(x5)(x19)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_566_case__507 x1 x4 x5 x13@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_565(x4)(x5)(x19)(x18)(x1)(st))(st)
c__case_566_case__507 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_566_case__507(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_566_case__507 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_566_case__507")(x)



c__case_573_case__508 x1 x4 x5 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_572(x4)(x5)(x13)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_566(x4)(x5)(x13)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st)



c__case_574_case__509 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_573(x4)(x5)(x13)(x12)(x1)(st))(st)
c__case_574_case__509 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_574_case__509(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_574_case__509 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_574_case__509")(x)



c__case_514_case__510 x1 x5 x49 x65 x63@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.FlatCurry.C_Comb(Curry.Module.FlatCurry.C_FuncCall)(Curry.Module.Prelude.T2(x49)(x65))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_flatx2exp))))(x5)(x1)(st)))(st)
c__case_514_case__510 x1 x5 x49 x65 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_514_case__510(x1)(x5)(x49)(x65)(x)(st))(i)(xs)(st)
c__case_514_case__510 x1 x5 x49 x65 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_514_case__510")(x)



c__case_515_case__511 x1 x5 x49 x63 x65 x73@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_514(x5)(x49)(x65)(x63)(x1)(st))(st)
c__case_515_case__511 x1 x5 x49 x63 x65 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_515_case__511(x1)(x5)(x49)(x63)(x65)(x)(st))(i)(xs)(st)
c__case_515_case__511 x1 x5 x49 x63 x65 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_515_case__511")(x)



c__case_516_case__512 x1 x5 x49 x63 x65 x73 x72 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x72)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_515(x5)(x49)(x63)(x65)(x73)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_517_case__513 x1 x5 x49 x63 x65 x71@((Curry.Module.Prelude.:<) x72 x73) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_516(x5)(x49)(x63)(x65)(x73)(x72)(x1)(st))(st)
c__case_517_case__513 x1 x5 x49 x63 x65 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_517_case__513(x1)(x5)(x49)(x63)(x65)(x)(st))(i)(xs)(st)
c__case_517_case__513 x1 x5 x49 x63 x65 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_517_case__513")(x)



c__case_518_case__514 x1 x5 x49 x63 x65 x71 x70 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x70)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_517(x5)(x49)(x63)(x65)(x71)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_519_case__515 x1 x5 x49 x63 x65 x69@((Curry.Module.Prelude.:<) x70 x71) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_518(x5)(x49)(x63)(x65)(x71)(x70)(x1)(st))(st)
c__case_519_case__515 x1 x5 x49 x63 x65 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_519_case__515(x1)(x5)(x49)(x63)(x65)(x)(st))(i)(xs)(st)
c__case_519_case__515 x1 x5 x49 x63 x65 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_519_case__515")(x)



c__case_520_case__516 x1 x5 x49 x63 x65 x69 x68 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x68)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_519(x5)(x49)(x63)(x65)(x69)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_521_case__517 x1 x5 x49 x63 x65 x67@((Curry.Module.Prelude.:<) x68 x69) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_520(x5)(x49)(x63)(x65)(x69)(x68)(x1)(st))(st)
c__case_521_case__517 x1 x5 x49 x63 x65 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_521_case__517(x1)(x5)(x49)(x63)(x65)(x)(st))(i)(xs)(st)
c__case_521_case__517 x1 x5 x49 x63 x65 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_521_case__517")(x)



c__case_522_case__518 x1 x5 x49 x63 x65 x67 x66 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x66)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_521(x5)(x49)(x63)(x65)(x67)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_523_case__519 x1 x5 x49 x63 x65 x64@((Curry.Module.Prelude.:<) x66 x67) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_522(x5)(x49)(x63)(x65)(x67)(x66)(x1)(st))(st)
c__case_523_case__519 x1 x5 x49 x63 x65 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_523_case__519(x1)(x5)(x49)(x63)(x65)(x)(st))(i)(xs)(st)
c__case_523_case__519 x1 x5 x49 x63 x65 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_523_case__519")(x)



c__case_524_case__520 x1 x5 x49 x63 x62@(Curry.Module.Prelude.T2 x64 x65) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_523(x5)(x49)(x63)(x65)(x64)(x1)(st))(st)
c__case_524_case__520 x1 x5 x49 x63 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_524_case__520(x1)(x5)(x49)(x63)(x)(st))(i)(xs)(st)
c__case_524_case__520 x1 x5 x49 x63 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_524_case__520")(x)



c__case_525_case__521 x1 x5 x49 x47@((Curry.Module.Prelude.:<) x62 x63) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_524(x5)(x49)(x63)(x62)(x1)(st))(st)
c__case_525_case__521 x1 x5 x49 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_525_case__521(x1)(x5)(x49)(x)(st))(i)(xs)(st)
c__case_525_case__521 x1 x5 x49 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_525_case__521")(x)



c__case_526_case__522 x1 x5 x47 x49 x61@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_525(x5)(x49)(x47)(x1)(st))(st)
c__case_526_case__522 x1 x5 x47 x49 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_526_case__522(x1)(x5)(x47)(x49)(x)(st))(i)(xs)(st)
c__case_526_case__522 x1 x5 x47 x49 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_526_case__522")(x)



c__case_527_case__523 x1 x5 x47 x49 x61 x60 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x60)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_526(x5)(x47)(x49)(x61)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_528_case__524 x1 x5 x47 x49 x59@((Curry.Module.Prelude.:<) x60 x61) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_527(x5)(x47)(x49)(x61)(x60)(x1)(st))(st)
c__case_528_case__524 x1 x5 x47 x49 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_528_case__524(x1)(x5)(x47)(x49)(x)(st))(i)(xs)(st)
c__case_528_case__524 x1 x5 x47 x49 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_528_case__524")(x)



c__case_529_case__525 x1 x5 x47 x49 x59 x58 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x58)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_528(x5)(x47)(x49)(x59)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_530_case__526 x1 x5 x47 x49 x57@((Curry.Module.Prelude.:<) x58 x59) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_529(x5)(x47)(x49)(x59)(x58)(x1)(st))(st)
c__case_530_case__526 x1 x5 x47 x49 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_530_case__526(x1)(x5)(x47)(x49)(x)(st))(i)(xs)(st)
c__case_530_case__526 x1 x5 x47 x49 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_530_case__526")(x)



c__case_531_case__527 x1 x5 x47 x49 x57 x56 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x56)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_530(x5)(x47)(x49)(x57)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_532_case__528 x1 x5 x47 x49 x55@((Curry.Module.Prelude.:<) x56 x57) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_531(x5)(x47)(x49)(x57)(x56)(x1)(st))(st)
c__case_532_case__528 x1 x5 x47 x49 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_532_case__528(x1)(x5)(x47)(x49)(x)(st))(i)(xs)(st)
c__case_532_case__528 x1 x5 x47 x49 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_532_case__528")(x)



c__case_533_case__529 x1 x5 x47 x49 x55 x54 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x54)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_532(x5)(x47)(x49)(x55)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_534_case__530 x1 x5 x47 x49 x53@((Curry.Module.Prelude.:<) x54 x55) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_533(x5)(x47)(x49)(x55)(x54)(x1)(st))(st)
c__case_534_case__530 x1 x5 x47 x49 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_534_case__530(x1)(x5)(x47)(x49)(x)(st))(i)(xs)(st)
c__case_534_case__530 x1 x5 x47 x49 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_534_case__530")(x)



c__case_535_case__531 x1 x5 x47 x49 x53 x52 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x52)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_534(x5)(x47)(x49)(x53)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_536_case__532 x1 x5 x47 x49 x51@((Curry.Module.Prelude.:<) x52 x53) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_535(x5)(x47)(x49)(x53)(x52)(x1)(st))(st)
c__case_536_case__532 x1 x5 x47 x49 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_536_case__532(x1)(x5)(x47)(x49)(x)(st))(i)(xs)(st)
c__case_536_case__532 x1 x5 x47 x49 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_536_case__532")(x)



c__case_537_case__533 x1 x5 x47 x49 x51 x50 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x50)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_536(x5)(x47)(x49)(x51)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_538_case__534 x1 x5 x47 x49 x48@((Curry.Module.Prelude.:<) x50 x51) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_537(x5)(x47)(x49)(x51)(x50)(x1)(st))(st)
c__case_538_case__534 x1 x5 x47 x49 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_538_case__534(x1)(x5)(x47)(x49)(x)(st))(i)(xs)(st)
c__case_538_case__534 x1 x5 x47 x49 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_538_case__534")(x)



c__case_539_case__535 x1 x5 x47 x46@(Curry.Module.Prelude.T2 x48 x49) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_538(x5)(x47)(x49)(x48)(x1)(st))(st)
c__case_539_case__535 x1 x5 x47 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_539_case__535(x1)(x5)(x47)(x)(st))(i)(xs)(st)
c__case_539_case__535 x1 x5 x47 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_539_case__535")(x)



c__case_540_case__536 x1 x5 x4@((Curry.Module.Prelude.:<) x46 x47) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_539(x5)(x47)(x46)(x1)(st))(st)
c__case_540_case__536 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_540_case__536(x1)(x5)(x)(st))(i)(xs)(st)
c__case_540_case__536 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_540_case__536")(x)



c__case_541_case__537 x1 x4 x5 x45@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_540(x5)(x4)(x1)(st))(st)
c__case_541_case__537 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_541_case__537(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_541_case__537 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_541_case__537")(x)



c__case_542_case__538 x1 x4 x5 x45 x44 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x44)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_541(x4)(x5)(x45)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_543_case__539 x1 x4 x5 x43@((Curry.Module.Prelude.:<) x44 x45) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_542(x4)(x5)(x45)(x44)(x1)(st))(st)
c__case_543_case__539 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_543_case__539(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_543_case__539 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_543_case__539")(x)



c__case_544_case__540 x1 x4 x5 x43 x42 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x42)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_543(x4)(x5)(x43)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_545_case__541 x1 x4 x5 x41@((Curry.Module.Prelude.:<) x42 x43) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_544(x4)(x5)(x43)(x42)(x1)(st))(st)
c__case_545_case__541 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_545_case__541(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_545_case__541 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_545_case__541")(x)



c__case_546_case__542 x1 x4 x5 x41 x40 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x40)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_545(x4)(x5)(x41)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_547_case__543 x1 x4 x5 x39@((Curry.Module.Prelude.:<) x40 x41) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_546(x4)(x5)(x41)(x40)(x1)(st))(st)
c__case_547_case__543 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_547_case__543(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_547_case__543 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_547_case__543")(x)



c__case_455_case__544 x1 x5 x91 x107 x119 x117@Curry.Module.Prelude.List st = let {x120 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x120)(Curry.Module.Prelude.List))(Curry.Module.FlatCurry.C_Comb(Curry.Module.FlatCurry.C_FuncPartCall(Curry.Module.OracleRead.c_readNat(x119)(x1)(st)))(Curry.Module.Prelude.T2(x91)(x107))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_flatx2exp))))(x5)(x120)(st)))(st)
c__case_455_case__544 x1 x5 x91 x107 x119 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_455_case__544(x1)(x5)(x91)(x107)(x119)(x)(st))(i)(xs)(st)
c__case_455_case__544 x1 x5 x91 x107 x119 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_455_case__544")(x)



c__case_456_case__545 x1 x5 x91 x107 x117 x119 x133@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_455(x5)(x91)(x107)(x119)(x117)(x1)(st))(st)
c__case_456_case__545 x1 x5 x91 x107 x117 x119 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_456_case__545(x1)(x5)(x91)(x107)(x117)(x119)(x)(st))(i)(xs)(st)
c__case_456_case__545 x1 x5 x91 x107 x117 x119 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_456_case__545")(x)



c__case_457_case__546 x1 x5 x91 x107 x117 x119 x133 x132 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x132)(Curry.Module.Prelude.C_Char('g'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_456(x5)(x91)(x107)(x117)(x119)(x133)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_458_case__547 x1 x5 x91 x107 x117 x119 x131@((Curry.Module.Prelude.:<) x132 x133) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_457(x5)(x91)(x107)(x117)(x119)(x133)(x132)(x1)(st))(st)
c__case_458_case__547 x1 x5 x91 x107 x117 x119 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_458_case__547(x1)(x5)(x91)(x107)(x117)(x119)(x)(st))(i)(xs)(st)
c__case_458_case__547 x1 x5 x91 x107 x117 x119 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_458_case__547")(x)



c__case_459_case__548 x1 x5 x91 x107 x117 x119 x131 x130 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x130)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_458(x5)(x91)(x107)(x117)(x119)(x131)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_460_case__549 x1 x5 x91 x107 x117 x119 x129@((Curry.Module.Prelude.:<) x130 x131) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_459(x5)(x91)(x107)(x117)(x119)(x131)(x130)(x1)(st))(st)
c__case_460_case__549 x1 x5 x91 x107 x117 x119 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_460_case__549(x1)(x5)(x91)(x107)(x117)(x119)(x)(st))(i)(xs)(st)
c__case_460_case__549 x1 x5 x91 x107 x117 x119 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_460_case__549")(x)



c__case_461_case__550 x1 x5 x91 x107 x117 x119 x129 x128 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x128)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_460(x5)(x91)(x107)(x117)(x119)(x129)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_462_case__551 x1 x5 x91 x107 x117 x119 x127@((Curry.Module.Prelude.:<) x128 x129) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_461(x5)(x91)(x107)(x117)(x119)(x129)(x128)(x1)(st))(st)
c__case_462_case__551 x1 x5 x91 x107 x117 x119 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_462_case__551(x1)(x5)(x91)(x107)(x117)(x119)(x)(st))(i)(xs)(st)
c__case_462_case__551 x1 x5 x91 x107 x117 x119 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_462_case__551")(x)



c__case_463_case__552 x1 x5 x91 x107 x117 x119 x127 x126 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x126)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_462(x5)(x91)(x107)(x117)(x119)(x127)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_464_case__553 x1 x5 x91 x107 x117 x119 x125@((Curry.Module.Prelude.:<) x126 x127) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_463(x5)(x91)(x107)(x117)(x119)(x127)(x126)(x1)(st))(st)
c__case_464_case__553 x1 x5 x91 x107 x117 x119 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_464_case__553(x1)(x5)(x91)(x107)(x117)(x119)(x)(st))(i)(xs)(st)
c__case_464_case__553 x1 x5 x91 x107 x117 x119 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_464_case__553")(x)



c__case_465_case__554 x1 x5 x91 x107 x117 x119 x125 x124 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x124)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_464(x5)(x91)(x107)(x117)(x119)(x125)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_466_case__555 x1 x5 x91 x107 x117 x119 x123@((Curry.Module.Prelude.:<) x124 x125) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_465(x5)(x91)(x107)(x117)(x119)(x125)(x124)(x1)(st))(st)
c__case_466_case__555 x1 x5 x91 x107 x117 x119 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_466_case__555(x1)(x5)(x91)(x107)(x117)(x119)(x)(st))(i)(xs)(st)
c__case_466_case__555 x1 x5 x91 x107 x117 x119 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_466_case__555")(x)



c__case_467_case__556 x1 x5 x91 x107 x117 x119 x123 x122 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x122)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_466(x5)(x91)(x107)(x117)(x119)(x123)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_468_case__557 x1 x5 x91 x107 x117 x119 x121@((Curry.Module.Prelude.:<) x122 x123) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_467(x5)(x91)(x107)(x117)(x119)(x123)(x122)(x1)(st))(st)
c__case_468_case__557 x1 x5 x91 x107 x117 x119 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_468_case__557(x1)(x5)(x91)(x107)(x117)(x119)(x)(st))(i)(xs)(st)
c__case_468_case__557 x1 x5 x91 x107 x117 x119 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_468_case__557")(x)



c__case_469_case__558 x1 x5 x91 x107 x117 x119 x121 x120 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x120)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_468(x5)(x91)(x107)(x117)(x119)(x121)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_470_case__559 x1 x5 x91 x107 x117 x119 x118@((Curry.Module.Prelude.:<) x120 x121) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_469(x5)(x91)(x107)(x117)(x119)(x121)(x120)(x1)(st))(st)
c__case_470_case__559 x1 x5 x91 x107 x117 x119 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_470_case__559(x1)(x5)(x91)(x107)(x117)(x119)(x)(st))(i)(xs)(st)
c__case_470_case__559 x1 x5 x91 x107 x117 x119 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_470_case__559")(x)



c__case_471_case__560 x1 x5 x91 x107 x117 x116@(Curry.Module.Prelude.T2 x118 x119) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_470(x5)(x91)(x107)(x117)(x119)(x118)(x1)(st))(st)
c__case_471_case__560 x1 x5 x91 x107 x117 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_471_case__560(x1)(x5)(x91)(x107)(x117)(x)(st))(i)(xs)(st)
c__case_471_case__560 x1 x5 x91 x107 x117 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_471_case__560")(x)



c__case_472_case__561 x1 x5 x91 x107 x105@((Curry.Module.Prelude.:<) x116 x117) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_471(x5)(x91)(x107)(x117)(x116)(x1)(st))(st)
c__case_472_case__561 x1 x5 x91 x107 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_472_case__561(x1)(x5)(x91)(x107)(x)(st))(i)(xs)(st)
c__case_472_case__561 x1 x5 x91 x107 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_472_case__561")(x)



c__case_473_case__562 x1 x5 x91 x105 x107 x115@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_472(x5)(x91)(x107)(x105)(x1)(st))(st)
c__case_473_case__562 x1 x5 x91 x105 x107 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_473_case__562(x1)(x5)(x91)(x105)(x107)(x)(st))(i)(xs)(st)
c__case_473_case__562 x1 x5 x91 x105 x107 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_473_case__562")(x)



c__case_474_case__563 x1 x5 x91 x105 x107 x115 x114 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x114)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_473(x5)(x91)(x105)(x107)(x115)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_475_case__564 x1 x5 x91 x105 x107 x113@((Curry.Module.Prelude.:<) x114 x115) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_474(x5)(x91)(x105)(x107)(x115)(x114)(x1)(st))(st)
c__case_475_case__564 x1 x5 x91 x105 x107 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_475_case__564(x1)(x5)(x91)(x105)(x107)(x)(st))(i)(xs)(st)
c__case_475_case__564 x1 x5 x91 x105 x107 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_475_case__564")(x)



c__case_476_case__565 x1 x5 x91 x105 x107 x113 x112 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x112)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_475(x5)(x91)(x105)(x107)(x113)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_477_case__566 x1 x5 x91 x105 x107 x111@((Curry.Module.Prelude.:<) x112 x113) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_476(x5)(x91)(x105)(x107)(x113)(x112)(x1)(st))(st)
c__case_477_case__566 x1 x5 x91 x105 x107 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_477_case__566(x1)(x5)(x91)(x105)(x107)(x)(st))(i)(xs)(st)
c__case_477_case__566 x1 x5 x91 x105 x107 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_477_case__566")(x)



c__case_478_case__567 x1 x5 x91 x105 x107 x111 x110 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x110)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_477(x5)(x91)(x105)(x107)(x111)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_479_case__568 x1 x5 x91 x105 x107 x109@((Curry.Module.Prelude.:<) x110 x111) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_478(x5)(x91)(x105)(x107)(x111)(x110)(x1)(st))(st)
c__case_479_case__568 x1 x5 x91 x105 x107 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_479_case__568(x1)(x5)(x91)(x105)(x107)(x)(st))(i)(xs)(st)
c__case_479_case__568 x1 x5 x91 x105 x107 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_479_case__568")(x)



c__case_480_case__569 x1 x5 x91 x105 x107 x109 x108 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x108)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_479(x5)(x91)(x105)(x107)(x109)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_481_case__570 x1 x5 x91 x105 x107 x106@((Curry.Module.Prelude.:<) x108 x109) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_480(x5)(x91)(x105)(x107)(x109)(x108)(x1)(st))(st)
c__case_481_case__570 x1 x5 x91 x105 x107 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_481_case__570(x1)(x5)(x91)(x105)(x107)(x)(st))(i)(xs)(st)
c__case_481_case__570 x1 x5 x91 x105 x107 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_481_case__570")(x)



c__case_482_case__571 x1 x5 x91 x105 x104@(Curry.Module.Prelude.T2 x106 x107) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_481(x5)(x91)(x105)(x107)(x106)(x1)(st))(st)
c__case_482_case__571 x1 x5 x91 x105 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_482_case__571(x1)(x5)(x91)(x105)(x)(st))(i)(xs)(st)
c__case_482_case__571 x1 x5 x91 x105 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_482_case__571")(x)



c__case_483_case__572 x1 x5 x91 x89@((Curry.Module.Prelude.:<) x104 x105) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_482(x5)(x91)(x105)(x104)(x1)(st))(st)
c__case_483_case__572 x1 x5 x91 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_483_case__572(x1)(x5)(x91)(x)(st))(i)(xs)(st)
c__case_483_case__572 x1 x5 x91 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_483_case__572")(x)



c__case_484_case__573 x1 x5 x89 x91 x103@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_483(x5)(x91)(x89)(x1)(st))(st)
c__case_484_case__573 x1 x5 x89 x91 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_484_case__573(x1)(x5)(x89)(x91)(x)(st))(i)(xs)(st)
c__case_484_case__573 x1 x5 x89 x91 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_484_case__573")(x)



c__case_485_case__574 x1 x5 x89 x91 x103 x102 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x102)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_484(x5)(x89)(x91)(x103)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_486_case__575 x1 x5 x89 x91 x101@((Curry.Module.Prelude.:<) x102 x103) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_485(x5)(x89)(x91)(x103)(x102)(x1)(st))(st)
c__case_486_case__575 x1 x5 x89 x91 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_486_case__575(x1)(x5)(x89)(x91)(x)(st))(i)(xs)(st)
c__case_486_case__575 x1 x5 x89 x91 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_486_case__575")(x)



c__case_487_case__576 x1 x5 x89 x91 x101 x100 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x100)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_486(x5)(x89)(x91)(x101)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_488_case__577 x1 x5 x89 x91 x99@((Curry.Module.Prelude.:<) x100 x101) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_487(x5)(x89)(x91)(x101)(x100)(x1)(st))(st)
c__case_488_case__577 x1 x5 x89 x91 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_488_case__577(x1)(x5)(x89)(x91)(x)(st))(i)(xs)(st)
c__case_488_case__577 x1 x5 x89 x91 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_488_case__577")(x)



c__case_489_case__578 x1 x5 x89 x91 x99 x98 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x98)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_488(x5)(x89)(x91)(x99)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_490_case__579 x1 x5 x89 x91 x97@((Curry.Module.Prelude.:<) x98 x99) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_489(x5)(x89)(x91)(x99)(x98)(x1)(st))(st)
c__case_490_case__579 x1 x5 x89 x91 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_490_case__579(x1)(x5)(x89)(x91)(x)(st))(i)(xs)(st)
c__case_490_case__579 x1 x5 x89 x91 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_490_case__579")(x)



c__case_491_case__580 x1 x5 x89 x91 x97 x96 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x96)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_490(x5)(x89)(x91)(x97)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_492_case__581 x1 x5 x89 x91 x95@((Curry.Module.Prelude.:<) x96 x97) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_491(x5)(x89)(x91)(x97)(x96)(x1)(st))(st)
c__case_492_case__581 x1 x5 x89 x91 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_492_case__581(x1)(x5)(x89)(x91)(x)(st))(i)(xs)(st)
c__case_492_case__581 x1 x5 x89 x91 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_492_case__581")(x)



c__case_493_case__582 x1 x5 x89 x91 x95 x94 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x94)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_492(x5)(x89)(x91)(x95)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_494_case__583 x1 x5 x89 x91 x93@((Curry.Module.Prelude.:<) x94 x95) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_493(x5)(x89)(x91)(x95)(x94)(x1)(st))(st)
c__case_494_case__583 x1 x5 x89 x91 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_494_case__583(x1)(x5)(x89)(x91)(x)(st))(i)(xs)(st)
c__case_494_case__583 x1 x5 x89 x91 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_494_case__583")(x)



c__case_495_case__584 x1 x5 x89 x91 x93 x92 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x92)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_494(x5)(x89)(x91)(x93)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_496_case__585 x1 x5 x89 x91 x90@((Curry.Module.Prelude.:<) x92 x93) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_495(x5)(x89)(x91)(x93)(x92)(x1)(st))(st)
c__case_496_case__585 x1 x5 x89 x91 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_496_case__585(x1)(x5)(x89)(x91)(x)(st))(i)(xs)(st)
c__case_496_case__585 x1 x5 x89 x91 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_496_case__585")(x)



c__case_497_case__586 x1 x5 x89 x88@(Curry.Module.Prelude.T2 x90 x91) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_496(x5)(x89)(x91)(x90)(x1)(st))(st)
c__case_497_case__586 x1 x5 x89 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_497_case__586(x1)(x5)(x89)(x)(st))(i)(xs)(st)
c__case_497_case__586 x1 x5 x89 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_497_case__586")(x)



c__case_498_case__587 x1 x5 x4@((Curry.Module.Prelude.:<) x88 x89) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_497(x5)(x89)(x88)(x1)(st))(st)
c__case_498_case__587 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_498_case__587(x1)(x5)(x)(st))(i)(xs)(st)
c__case_498_case__587 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_498_case__587")(x)



c__case_499_case__588 x1 x4 x5 x87@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_498(x5)(x4)(x1)(st))(st)
c__case_499_case__588 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_499_case__588(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_499_case__588 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_499_case__588")(x)



c__case_500_case__589 x1 x4 x5 x87 x86 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x86)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_499(x4)(x5)(x87)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_501_case__590 x1 x4 x5 x85@((Curry.Module.Prelude.:<) x86 x87) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_500(x4)(x5)(x87)(x86)(x1)(st))(st)
c__case_501_case__590 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_501_case__590(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_501_case__590 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_501_case__590")(x)



c__case_502_case__591 x1 x4 x5 x85 x84 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x84)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_501(x4)(x5)(x85)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_503_case__592 x1 x4 x5 x83@((Curry.Module.Prelude.:<) x84 x85) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_502(x4)(x5)(x85)(x84)(x1)(st))(st)
c__case_503_case__592 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_503_case__592(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_503_case__592 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_503_case__592")(x)



c__case_504_case__593 x1 x4 x5 x83 x82 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x82)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_503(x4)(x5)(x83)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_505_case__594 x1 x4 x5 x81@((Curry.Module.Prelude.:<) x82 x83) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_504(x4)(x5)(x83)(x82)(x1)(st))(st)
c__case_505_case__594 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_505_case__594(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_505_case__594 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_505_case__594")(x)



c__case_506_case__595 x1 x4 x5 x81 x80 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x80)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_505(x4)(x5)(x81)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_507_case__596 x1 x4 x5 x79@((Curry.Module.Prelude.:<) x80 x81) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_506(x4)(x5)(x81)(x80)(x1)(st))(st)
c__case_507_case__596 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_507_case__596(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_507_case__596 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_507_case__596")(x)



c__case_508_case__597 x1 x4 x5 x79 x78 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x78)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_507(x4)(x5)(x79)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_509_case__598 x1 x4 x5 x77@((Curry.Module.Prelude.:<) x78 x79) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_508(x4)(x5)(x79)(x78)(x1)(st))(st)
c__case_509_case__598 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_509_case__598(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_509_case__598 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_509_case__598")(x)



c__case_510_case__599 x1 x4 x5 x77 x76 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x76)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_509(x4)(x5)(x77)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_511_case__600 x1 x4 x5 x75@((Curry.Module.Prelude.:<) x76 x77) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_510(x4)(x5)(x77)(x76)(x1)(st))(st)
c__case_511_case__600 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_511_case__600(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_511_case__600 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_511_case__600")(x)



c__case_512_case__601 x1 x4 x5 x75 x74 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x74)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_511(x4)(x5)(x75)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_513_case__602 x1 x4 x5 x39@((Curry.Module.Prelude.:<) x74 x75) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_512(x4)(x5)(x75)(x74)(x1)(st))(st)
c__case_513_case__602 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_513_case__602(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_513_case__602 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_513_case__602")(x)



c__case_548_case__603 x1 x4 x5 x39 x38 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x38)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_547(x4)(x5)(x39)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x38)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_513(x4)(x5)(x39)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st)



c__case_549_case__604 x1 x4 x5 x37@((Curry.Module.Prelude.:<) x38 x39) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_548(x4)(x5)(x39)(x38)(x1)(st))(st)
c__case_549_case__604 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_549_case__604(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_549_case__604 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_549_case__604")(x)



c__case_550_case__605 x1 x4 x5 x37 x36 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x36)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_549(x4)(x5)(x37)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_551_case__606 x1 x4 x5 x35@((Curry.Module.Prelude.:<) x36 x37) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_550(x4)(x5)(x37)(x36)(x1)(st))(st)
c__case_551_case__606 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_551_case__606(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_551_case__606 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_551_case__606")(x)



c__case_552_case__607 x1 x4 x5 x35 x34 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x34)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_551(x4)(x5)(x35)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_553_case__608 x1 x4 x5 x33@((Curry.Module.Prelude.:<) x34 x35) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_552(x4)(x5)(x35)(x34)(x1)(st))(st)
c__case_553_case__608 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_553_case__608(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_553_case__608 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_553_case__608")(x)



c__case_427_case__609 x1 x142 x159 x160@Curry.Module.Prelude.List st = let {x161 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x161)(Curry.Module.Prelude.List))(Curry.Module.FlatCurry.C_Free(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_flatx2var))))(x142)(x1)(st))(Curry.Module.OracleFlatCurryXML.c_flatx2exp(x159)(x161)(st)))(st)
c__case_427_case__609 x1 x142 x159 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_427_case__609(x1)(x142)(x159)(x)(st))(i)(xs)(st)
c__case_427_case__609 x1 x142 x159 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_427_case__609")(x)



c__case_428_case__610 x1 x142 x139@((Curry.Module.Prelude.:<) x159 x160) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_427(x142)(x159)(x160)(x1)(st))(st)
c__case_428_case__610 x1 x142 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_428_case__610(x1)(x142)(x)(st))(i)(xs)(st)
c__case_428_case__610 x1 x142 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_428_case__610")(x)



c__case_429_case__611 x1 x139 x142 x141@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_428(x142)(x139)(x1)(st))(st)
c__case_429_case__611 x1 x139 x142 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_429_case__611(x1)(x139)(x142)(x)(st))(i)(xs)(st)
c__case_429_case__611 x1 x139 x142 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_429_case__611")(x)



c__case_430_case__612 x1 x139 x141 x142 x158@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_429(x139)(x142)(x141)(x1)(st))(st)
c__case_430_case__612 x1 x139 x141 x142 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_430_case__612(x1)(x139)(x141)(x142)(x)(st))(i)(xs)(st)
c__case_430_case__612 x1 x139 x141 x142 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_430_case__612")(x)



c__case_431_case__613 x1 x139 x141 x142 x158 x157 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x157)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_430(x139)(x141)(x142)(x158)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_432_case__614 x1 x139 x141 x142 x156@((Curry.Module.Prelude.:<) x157 x158) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_431(x139)(x141)(x142)(x158)(x157)(x1)(st))(st)
c__case_432_case__614 x1 x139 x141 x142 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_432_case__614(x1)(x139)(x141)(x142)(x)(st))(i)(xs)(st)
c__case_432_case__614 x1 x139 x141 x142 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_432_case__614")(x)



c__case_433_case__615 x1 x139 x141 x142 x156 x155 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x155)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_432(x139)(x141)(x142)(x156)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_434_case__616 x1 x139 x141 x142 x154@((Curry.Module.Prelude.:<) x155 x156) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_433(x139)(x141)(x142)(x156)(x155)(x1)(st))(st)
c__case_434_case__616 x1 x139 x141 x142 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_434_case__616(x1)(x139)(x141)(x142)(x)(st))(i)(xs)(st)
c__case_434_case__616 x1 x139 x141 x142 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_434_case__616")(x)



c__case_435_case__617 x1 x139 x141 x142 x154 x153 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x153)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_434(x139)(x141)(x142)(x154)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_436_case__618 x1 x139 x141 x142 x152@((Curry.Module.Prelude.:<) x153 x154) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_435(x139)(x141)(x142)(x154)(x153)(x1)(st))(st)
c__case_436_case__618 x1 x139 x141 x142 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_436_case__618(x1)(x139)(x141)(x142)(x)(st))(i)(xs)(st)
c__case_436_case__618 x1 x139 x141 x142 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_436_case__618")(x)



c__case_437_case__619 x1 x139 x141 x142 x152 x151 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x151)(Curry.Module.Prelude.C_Char('v'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_436(x139)(x141)(x142)(x152)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_438_case__620 x1 x139 x141 x142 x150@((Curry.Module.Prelude.:<) x151 x152) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_437(x139)(x141)(x142)(x152)(x151)(x1)(st))(st)
c__case_438_case__620 x1 x139 x141 x142 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_438_case__620(x1)(x139)(x141)(x142)(x)(st))(i)(xs)(st)
c__case_438_case__620 x1 x139 x141 x142 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_438_case__620")(x)



c__case_439_case__621 x1 x139 x141 x142 x150 x149 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x149)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_438(x139)(x141)(x142)(x150)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_440_case__622 x1 x139 x141 x142 x148@((Curry.Module.Prelude.:<) x149 x150) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_439(x139)(x141)(x142)(x150)(x149)(x1)(st))(st)
c__case_440_case__622 x1 x139 x141 x142 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_440_case__622(x1)(x139)(x141)(x142)(x)(st))(i)(xs)(st)
c__case_440_case__622 x1 x139 x141 x142 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_440_case__622")(x)



c__case_441_case__623 x1 x139 x141 x142 x148 x147 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x147)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_440(x139)(x141)(x142)(x148)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_442_case__624 x1 x139 x141 x142 x146@((Curry.Module.Prelude.:<) x147 x148) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_441(x139)(x141)(x142)(x148)(x147)(x1)(st))(st)
c__case_442_case__624 x1 x139 x141 x142 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_442_case__624(x1)(x139)(x141)(x142)(x)(st))(i)(xs)(st)
c__case_442_case__624 x1 x139 x141 x142 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_442_case__624")(x)



c__case_443_case__625 x1 x139 x141 x142 x146 x145 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x145)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_442(x139)(x141)(x142)(x146)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_444_case__626 x1 x139 x141 x142 x144@((Curry.Module.Prelude.:<) x145 x146) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_443(x139)(x141)(x142)(x146)(x145)(x1)(st))(st)
c__case_444_case__626 x1 x139 x141 x142 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_444_case__626(x1)(x139)(x141)(x142)(x)(st))(i)(xs)(st)
c__case_444_case__626 x1 x139 x141 x142 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_444_case__626")(x)



c__case_445_case__627 x1 x139 x141 x142 x144 x143 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x143)(Curry.Module.Prelude.C_Char('f'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_444(x139)(x141)(x142)(x144)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_446_case__628 x1 x139 x141 x142 x140@((Curry.Module.Prelude.:<) x143 x144) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_445(x139)(x141)(x142)(x144)(x143)(x1)(st))(st)
c__case_446_case__628 x1 x139 x141 x142 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_446_case__628(x1)(x139)(x141)(x142)(x)(st))(i)(xs)(st)
c__case_446_case__628 x1 x139 x141 x142 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_446_case__628")(x)



c__case_447_case__629 x1 x139 x138@(Curry.Module.XML.C_XElem x140 x141 x142) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_446(x139)(x141)(x142)(x140)(x1)(st))(st)
c__case_447_case__629 x1 x139 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_447_case__629(x1)(x139)(x)(st))(i)(xs)(st)
c__case_447_case__629 x1 x139 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_447_case__629")(x)



c__case_448_case__630 x1 x5@((Curry.Module.Prelude.:<) x138 x139) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_447(x139)(x138)(x1)(st))(st)
c__case_448_case__630 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_448_case__630(x1)(x)(st))(i)(xs)(st)
c__case_448_case__630 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_448_case__630")(x)



c__case_449_case__631 x1 x5 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_448(x5)(x1)(st))(st)
c__case_449_case__631 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_449_case__631(x1)(x5)(x)(st))(i)(xs)(st)
c__case_449_case__631 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_449_case__631")(x)



c__case_450_case__632 x1 x4 x5 x137@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_449(x5)(x4)(x1)(st))(st)
c__case_450_case__632 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_450_case__632(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_450_case__632 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_450_case__632")(x)



c__case_451_case__633 x1 x4 x5 x137 x136 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x136)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_450(x4)(x5)(x137)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_452_case__634 x1 x4 x5 x135@((Curry.Module.Prelude.:<) x136 x137) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_451(x4)(x5)(x137)(x136)(x1)(st))(st)
c__case_452_case__634 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_452_case__634(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_452_case__634 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_452_case__634")(x)



c__case_453_case__635 x1 x4 x5 x135 x134 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x134)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_452(x4)(x5)(x135)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_454_case__636 x1 x4 x5 x33@((Curry.Module.Prelude.:<) x134 x135) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_453(x4)(x5)(x135)(x134)(x1)(st))(st)
c__case_454_case__636 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_454_case__636(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_454_case__636 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_454_case__636")(x)



c__case_418_case__637 x1 x5@((Curry.Module.Prelude.:<) x167 x168) st = let {x169 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x169)(Curry.Module.Prelude.List))(Curry.Module.FlatCurry.C_Case(Curry.Module.FlatCurry.C_Flex)(Curry.Module.OracleFlatCurryXML.c_flatx2exp(x167)(x1)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_flatx2branch))))(x168)(x169)(st)))(st)
c__case_418_case__637 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_418_case__637(x1)(x)(st))(i)(xs)(st)
c__case_418_case__637 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_418_case__637")(x)



c__case_419_case__638 x1 x5 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_418(x5)(x1)(st))(st)
c__case_419_case__638 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_419_case__638(x1)(x5)(x)(st))(i)(xs)(st)
c__case_419_case__638 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_419_case__638")(x)



c__case_420_case__639 x1 x4 x5 x166@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_419(x5)(x4)(x1)(st))(st)
c__case_420_case__639 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_420_case__639(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_420_case__639 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_420_case__639")(x)



c__case_421_case__640 x1 x4 x5 x166 x165 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x165)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_420(x4)(x5)(x166)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_422_case__641 x1 x4 x5 x164@((Curry.Module.Prelude.:<) x165 x166) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_421(x4)(x5)(x166)(x165)(x1)(st))(st)
c__case_422_case__641 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_422_case__641(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_422_case__641 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_422_case__641")(x)



c__case_423_case__642 x1 x4 x5 x164 x163 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x163)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_422(x4)(x5)(x164)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_424_case__643 x1 x4 x5 x162@((Curry.Module.Prelude.:<) x163 x164) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_423(x4)(x5)(x164)(x163)(x1)(st))(st)
c__case_424_case__643 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_424_case__643(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_424_case__643 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_424_case__643")(x)



c__case_425_case__644 x1 x4 x5 x162 x161 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x161)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_424(x4)(x5)(x162)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_426_case__645 x1 x4 x5 x33@((Curry.Module.Prelude.:<) x161 x162) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_425(x4)(x5)(x162)(x161)(x1)(st))(st)
c__case_426_case__645 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_426_case__645(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_426_case__645 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_426_case__645")(x)



c__case_554_case__646 x1 x4 x5 x33 x32 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x32)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_553(x4)(x5)(x33)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x32)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_454(x4)(x5)(x33)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x32)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_426(x4)(x5)(x33)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st))(st)



c__case_555_case__647 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x32 x33) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_554(x4)(x5)(x33)(x32)(x1)(st))(st)
c__case_555_case__647 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_555_case__647(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_555_case__647 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_555_case__647")(x)



c__case_376_case__648 x1 x5 x186 x202 x200@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.FlatCurry.C_Comb(Curry.Module.FlatCurry.C_ConsCall)(Curry.Module.Prelude.T2(x186)(x202))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_flatx2exp))))(x5)(x1)(st)))(st)
c__case_376_case__648 x1 x5 x186 x202 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_376_case__648(x1)(x5)(x186)(x202)(x)(st))(i)(xs)(st)
c__case_376_case__648 x1 x5 x186 x202 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_376_case__648")(x)



c__case_377_case__649 x1 x5 x186 x200 x202 x210@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_376(x5)(x186)(x202)(x200)(x1)(st))(st)
c__case_377_case__649 x1 x5 x186 x200 x202 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_377_case__649(x1)(x5)(x186)(x200)(x202)(x)(st))(i)(xs)(st)
c__case_377_case__649 x1 x5 x186 x200 x202 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_377_case__649")(x)



c__case_378_case__650 x1 x5 x186 x200 x202 x210 x209 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x209)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_377(x5)(x186)(x200)(x202)(x210)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_379_case__651 x1 x5 x186 x200 x202 x208@((Curry.Module.Prelude.:<) x209 x210) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_378(x5)(x186)(x200)(x202)(x210)(x209)(x1)(st))(st)
c__case_379_case__651 x1 x5 x186 x200 x202 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_379_case__651(x1)(x5)(x186)(x200)(x202)(x)(st))(i)(xs)(st)
c__case_379_case__651 x1 x5 x186 x200 x202 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_379_case__651")(x)



c__case_380_case__652 x1 x5 x186 x200 x202 x208 x207 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x207)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_379(x5)(x186)(x200)(x202)(x208)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_381_case__653 x1 x5 x186 x200 x202 x206@((Curry.Module.Prelude.:<) x207 x208) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_380(x5)(x186)(x200)(x202)(x208)(x207)(x1)(st))(st)
c__case_381_case__653 x1 x5 x186 x200 x202 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_381_case__653(x1)(x5)(x186)(x200)(x202)(x)(st))(i)(xs)(st)
c__case_381_case__653 x1 x5 x186 x200 x202 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_381_case__653")(x)



c__case_382_case__654 x1 x5 x186 x200 x202 x206 x205 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x205)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_381(x5)(x186)(x200)(x202)(x206)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_383_case__655 x1 x5 x186 x200 x202 x204@((Curry.Module.Prelude.:<) x205 x206) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_382(x5)(x186)(x200)(x202)(x206)(x205)(x1)(st))(st)
c__case_383_case__655 x1 x5 x186 x200 x202 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_383_case__655(x1)(x5)(x186)(x200)(x202)(x)(st))(i)(xs)(st)
c__case_383_case__655 x1 x5 x186 x200 x202 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_383_case__655")(x)



c__case_384_case__656 x1 x5 x186 x200 x202 x204 x203 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x203)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_383(x5)(x186)(x200)(x202)(x204)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_385_case__657 x1 x5 x186 x200 x202 x201@((Curry.Module.Prelude.:<) x203 x204) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_384(x5)(x186)(x200)(x202)(x204)(x203)(x1)(st))(st)
c__case_385_case__657 x1 x5 x186 x200 x202 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_385_case__657(x1)(x5)(x186)(x200)(x202)(x)(st))(i)(xs)(st)
c__case_385_case__657 x1 x5 x186 x200 x202 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_385_case__657")(x)



c__case_386_case__658 x1 x5 x186 x200 x199@(Curry.Module.Prelude.T2 x201 x202) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_385(x5)(x186)(x200)(x202)(x201)(x1)(st))(st)
c__case_386_case__658 x1 x5 x186 x200 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_386_case__658(x1)(x5)(x186)(x200)(x)(st))(i)(xs)(st)
c__case_386_case__658 x1 x5 x186 x200 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_386_case__658")(x)



c__case_387_case__659 x1 x5 x186 x184@((Curry.Module.Prelude.:<) x199 x200) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_386(x5)(x186)(x200)(x199)(x1)(st))(st)
c__case_387_case__659 x1 x5 x186 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_387_case__659(x1)(x5)(x186)(x)(st))(i)(xs)(st)
c__case_387_case__659 x1 x5 x186 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_387_case__659")(x)



c__case_388_case__660 x1 x5 x184 x186 x198@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_387(x5)(x186)(x184)(x1)(st))(st)
c__case_388_case__660 x1 x5 x184 x186 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_388_case__660(x1)(x5)(x184)(x186)(x)(st))(i)(xs)(st)
c__case_388_case__660 x1 x5 x184 x186 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_388_case__660")(x)



c__case_389_case__661 x1 x5 x184 x186 x198 x197 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x197)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_388(x5)(x184)(x186)(x198)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_390_case__662 x1 x5 x184 x186 x196@((Curry.Module.Prelude.:<) x197 x198) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_389(x5)(x184)(x186)(x198)(x197)(x1)(st))(st)
c__case_390_case__662 x1 x5 x184 x186 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_390_case__662(x1)(x5)(x184)(x186)(x)(st))(i)(xs)(st)
c__case_390_case__662 x1 x5 x184 x186 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_390_case__662")(x)



c__case_391_case__663 x1 x5 x184 x186 x196 x195 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x195)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_390(x5)(x184)(x186)(x196)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_392_case__664 x1 x5 x184 x186 x194@((Curry.Module.Prelude.:<) x195 x196) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_391(x5)(x184)(x186)(x196)(x195)(x1)(st))(st)
c__case_392_case__664 x1 x5 x184 x186 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_392_case__664(x1)(x5)(x184)(x186)(x)(st))(i)(xs)(st)
c__case_392_case__664 x1 x5 x184 x186 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_392_case__664")(x)



c__case_393_case__665 x1 x5 x184 x186 x194 x193 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x193)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_392(x5)(x184)(x186)(x194)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_394_case__666 x1 x5 x184 x186 x192@((Curry.Module.Prelude.:<) x193 x194) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_393(x5)(x184)(x186)(x194)(x193)(x1)(st))(st)
c__case_394_case__666 x1 x5 x184 x186 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_394_case__666(x1)(x5)(x184)(x186)(x)(st))(i)(xs)(st)
c__case_394_case__666 x1 x5 x184 x186 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_394_case__666")(x)



c__case_395_case__667 x1 x5 x184 x186 x192 x191 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x191)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_394(x5)(x184)(x186)(x192)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_396_case__668 x1 x5 x184 x186 x190@((Curry.Module.Prelude.:<) x191 x192) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_395(x5)(x184)(x186)(x192)(x191)(x1)(st))(st)
c__case_396_case__668 x1 x5 x184 x186 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_396_case__668(x1)(x5)(x184)(x186)(x)(st))(i)(xs)(st)
c__case_396_case__668 x1 x5 x184 x186 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_396_case__668")(x)



c__case_397_case__669 x1 x5 x184 x186 x190 x189 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x189)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_396(x5)(x184)(x186)(x190)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_398_case__670 x1 x5 x184 x186 x188@((Curry.Module.Prelude.:<) x189 x190) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_397(x5)(x184)(x186)(x190)(x189)(x1)(st))(st)
c__case_398_case__670 x1 x5 x184 x186 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_398_case__670(x1)(x5)(x184)(x186)(x)(st))(i)(xs)(st)
c__case_398_case__670 x1 x5 x184 x186 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_398_case__670")(x)



c__case_399_case__671 x1 x5 x184 x186 x188 x187 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x187)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_398(x5)(x184)(x186)(x188)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_400_case__672 x1 x5 x184 x186 x185@((Curry.Module.Prelude.:<) x187 x188) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_399(x5)(x184)(x186)(x188)(x187)(x1)(st))(st)
c__case_400_case__672 x1 x5 x184 x186 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_400_case__672(x1)(x5)(x184)(x186)(x)(st))(i)(xs)(st)
c__case_400_case__672 x1 x5 x184 x186 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_400_case__672")(x)



c__case_401_case__673 x1 x5 x184 x183@(Curry.Module.Prelude.T2 x185 x186) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_400(x5)(x184)(x186)(x185)(x1)(st))(st)
c__case_401_case__673 x1 x5 x184 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_401_case__673(x1)(x5)(x184)(x)(st))(i)(xs)(st)
c__case_401_case__673 x1 x5 x184 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_401_case__673")(x)



c__case_402_case__674 x1 x5 x4@((Curry.Module.Prelude.:<) x183 x184) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_401(x5)(x184)(x183)(x1)(st))(st)
c__case_402_case__674 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_402_case__674(x1)(x5)(x)(st))(i)(xs)(st)
c__case_402_case__674 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_402_case__674")(x)



c__case_403_case__675 x1 x4 x5 x182@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_402(x5)(x4)(x1)(st))(st)
c__case_403_case__675 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_403_case__675(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_403_case__675 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_403_case__675")(x)



c__case_404_case__676 x1 x4 x5 x182 x181 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x181)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_403(x4)(x5)(x182)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_405_case__677 x1 x4 x5 x180@((Curry.Module.Prelude.:<) x181 x182) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_404(x4)(x5)(x182)(x181)(x1)(st))(st)
c__case_405_case__677 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_405_case__677(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_405_case__677 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_405_case__677")(x)



c__case_406_case__678 x1 x4 x5 x180 x179 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x179)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_405(x4)(x5)(x180)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_407_case__679 x1 x4 x5 x178@((Curry.Module.Prelude.:<) x179 x180) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_406(x4)(x5)(x180)(x179)(x1)(st))(st)
c__case_407_case__679 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_407_case__679(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_407_case__679 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_407_case__679")(x)



c__case_408_case__680 x1 x4 x5 x178 x177 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x177)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_407(x4)(x5)(x178)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_409_case__681 x1 x4 x5 x176@((Curry.Module.Prelude.:<) x177 x178) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_408(x4)(x5)(x178)(x177)(x1)(st))(st)
c__case_409_case__681 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_409_case__681(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_409_case__681 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_409_case__681")(x)



c__case_317_case__682 x1 x5 x228 x244 x256 x254@Curry.Module.Prelude.List st = let {x257 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x257)(Curry.Module.Prelude.List))(Curry.Module.FlatCurry.C_Comb(Curry.Module.FlatCurry.C_ConsPartCall(Curry.Module.OracleRead.c_readNat(x256)(x1)(st)))(Curry.Module.Prelude.T2(x228)(x244))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_flatx2exp))))(x5)(x257)(st)))(st)
c__case_317_case__682 x1 x5 x228 x244 x256 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_317_case__682(x1)(x5)(x228)(x244)(x256)(x)(st))(i)(xs)(st)
c__case_317_case__682 x1 x5 x228 x244 x256 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_317_case__682")(x)



c__case_318_case__683 x1 x5 x228 x244 x254 x256 x270@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_317(x5)(x228)(x244)(x256)(x254)(x1)(st))(st)
c__case_318_case__683 x1 x5 x228 x244 x254 x256 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_318_case__683(x1)(x5)(x228)(x244)(x254)(x256)(x)(st))(i)(xs)(st)
c__case_318_case__683 x1 x5 x228 x244 x254 x256 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_318_case__683")(x)



c__case_319_case__684 x1 x5 x228 x244 x254 x256 x270 x269 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x269)(Curry.Module.Prelude.C_Char('g'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_318(x5)(x228)(x244)(x254)(x256)(x270)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_320_case__685 x1 x5 x228 x244 x254 x256 x268@((Curry.Module.Prelude.:<) x269 x270) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_319(x5)(x228)(x244)(x254)(x256)(x270)(x269)(x1)(st))(st)
c__case_320_case__685 x1 x5 x228 x244 x254 x256 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_320_case__685(x1)(x5)(x228)(x244)(x254)(x256)(x)(st))(i)(xs)(st)
c__case_320_case__685 x1 x5 x228 x244 x254 x256 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_320_case__685")(x)



c__case_321_case__686 x1 x5 x228 x244 x254 x256 x268 x267 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x267)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_320(x5)(x228)(x244)(x254)(x256)(x268)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_322_case__687 x1 x5 x228 x244 x254 x256 x266@((Curry.Module.Prelude.:<) x267 x268) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_321(x5)(x228)(x244)(x254)(x256)(x268)(x267)(x1)(st))(st)
c__case_322_case__687 x1 x5 x228 x244 x254 x256 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_322_case__687(x1)(x5)(x228)(x244)(x254)(x256)(x)(st))(i)(xs)(st)
c__case_322_case__687 x1 x5 x228 x244 x254 x256 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_322_case__687")(x)



c__case_323_case__688 x1 x5 x228 x244 x254 x256 x266 x265 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x265)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_322(x5)(x228)(x244)(x254)(x256)(x266)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_324_case__689 x1 x5 x228 x244 x254 x256 x264@((Curry.Module.Prelude.:<) x265 x266) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_323(x5)(x228)(x244)(x254)(x256)(x266)(x265)(x1)(st))(st)
c__case_324_case__689 x1 x5 x228 x244 x254 x256 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_324_case__689(x1)(x5)(x228)(x244)(x254)(x256)(x)(st))(i)(xs)(st)
c__case_324_case__689 x1 x5 x228 x244 x254 x256 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_324_case__689")(x)



c__case_325_case__690 x1 x5 x228 x244 x254 x256 x264 x263 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x263)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_324(x5)(x228)(x244)(x254)(x256)(x264)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_326_case__691 x1 x5 x228 x244 x254 x256 x262@((Curry.Module.Prelude.:<) x263 x264) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_325(x5)(x228)(x244)(x254)(x256)(x264)(x263)(x1)(st))(st)
c__case_326_case__691 x1 x5 x228 x244 x254 x256 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_326_case__691(x1)(x5)(x228)(x244)(x254)(x256)(x)(st))(i)(xs)(st)
c__case_326_case__691 x1 x5 x228 x244 x254 x256 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_326_case__691")(x)



c__case_327_case__692 x1 x5 x228 x244 x254 x256 x262 x261 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x261)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_326(x5)(x228)(x244)(x254)(x256)(x262)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_328_case__693 x1 x5 x228 x244 x254 x256 x260@((Curry.Module.Prelude.:<) x261 x262) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_327(x5)(x228)(x244)(x254)(x256)(x262)(x261)(x1)(st))(st)
c__case_328_case__693 x1 x5 x228 x244 x254 x256 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_328_case__693(x1)(x5)(x228)(x244)(x254)(x256)(x)(st))(i)(xs)(st)
c__case_328_case__693 x1 x5 x228 x244 x254 x256 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_328_case__693")(x)



c__case_329_case__694 x1 x5 x228 x244 x254 x256 x260 x259 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x259)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_328(x5)(x228)(x244)(x254)(x256)(x260)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_330_case__695 x1 x5 x228 x244 x254 x256 x258@((Curry.Module.Prelude.:<) x259 x260) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_329(x5)(x228)(x244)(x254)(x256)(x260)(x259)(x1)(st))(st)
c__case_330_case__695 x1 x5 x228 x244 x254 x256 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_330_case__695(x1)(x5)(x228)(x244)(x254)(x256)(x)(st))(i)(xs)(st)
c__case_330_case__695 x1 x5 x228 x244 x254 x256 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_330_case__695")(x)



c__case_331_case__696 x1 x5 x228 x244 x254 x256 x258 x257 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x257)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_330(x5)(x228)(x244)(x254)(x256)(x258)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_332_case__697 x1 x5 x228 x244 x254 x256 x255@((Curry.Module.Prelude.:<) x257 x258) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_331(x5)(x228)(x244)(x254)(x256)(x258)(x257)(x1)(st))(st)
c__case_332_case__697 x1 x5 x228 x244 x254 x256 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_332_case__697(x1)(x5)(x228)(x244)(x254)(x256)(x)(st))(i)(xs)(st)
c__case_332_case__697 x1 x5 x228 x244 x254 x256 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_332_case__697")(x)



c__case_333_case__698 x1 x5 x228 x244 x254 x253@(Curry.Module.Prelude.T2 x255 x256) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_332(x5)(x228)(x244)(x254)(x256)(x255)(x1)(st))(st)
c__case_333_case__698 x1 x5 x228 x244 x254 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_333_case__698(x1)(x5)(x228)(x244)(x254)(x)(st))(i)(xs)(st)
c__case_333_case__698 x1 x5 x228 x244 x254 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_333_case__698")(x)



c__case_334_case__699 x1 x5 x228 x244 x242@((Curry.Module.Prelude.:<) x253 x254) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_333(x5)(x228)(x244)(x254)(x253)(x1)(st))(st)
c__case_334_case__699 x1 x5 x228 x244 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_334_case__699(x1)(x5)(x228)(x244)(x)(st))(i)(xs)(st)
c__case_334_case__699 x1 x5 x228 x244 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_334_case__699")(x)



c__case_335_case__700 x1 x5 x228 x242 x244 x252@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_334(x5)(x228)(x244)(x242)(x1)(st))(st)
c__case_335_case__700 x1 x5 x228 x242 x244 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_335_case__700(x1)(x5)(x228)(x242)(x244)(x)(st))(i)(xs)(st)
c__case_335_case__700 x1 x5 x228 x242 x244 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_335_case__700")(x)



c__case_336_case__701 x1 x5 x228 x242 x244 x252 x251 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x251)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_335(x5)(x228)(x242)(x244)(x252)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_337_case__702 x1 x5 x228 x242 x244 x250@((Curry.Module.Prelude.:<) x251 x252) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_336(x5)(x228)(x242)(x244)(x252)(x251)(x1)(st))(st)
c__case_337_case__702 x1 x5 x228 x242 x244 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_337_case__702(x1)(x5)(x228)(x242)(x244)(x)(st))(i)(xs)(st)
c__case_337_case__702 x1 x5 x228 x242 x244 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_337_case__702")(x)



c__case_338_case__703 x1 x5 x228 x242 x244 x250 x249 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x249)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_337(x5)(x228)(x242)(x244)(x250)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_339_case__704 x1 x5 x228 x242 x244 x248@((Curry.Module.Prelude.:<) x249 x250) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_338(x5)(x228)(x242)(x244)(x250)(x249)(x1)(st))(st)
c__case_339_case__704 x1 x5 x228 x242 x244 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_339_case__704(x1)(x5)(x228)(x242)(x244)(x)(st))(i)(xs)(st)
c__case_339_case__704 x1 x5 x228 x242 x244 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_339_case__704")(x)



c__case_340_case__705 x1 x5 x228 x242 x244 x248 x247 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x247)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_339(x5)(x228)(x242)(x244)(x248)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_341_case__706 x1 x5 x228 x242 x244 x246@((Curry.Module.Prelude.:<) x247 x248) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_340(x5)(x228)(x242)(x244)(x248)(x247)(x1)(st))(st)
c__case_341_case__706 x1 x5 x228 x242 x244 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_341_case__706(x1)(x5)(x228)(x242)(x244)(x)(st))(i)(xs)(st)
c__case_341_case__706 x1 x5 x228 x242 x244 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_341_case__706")(x)



c__case_342_case__707 x1 x5 x228 x242 x244 x246 x245 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x245)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_341(x5)(x228)(x242)(x244)(x246)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_343_case__708 x1 x5 x228 x242 x244 x243@((Curry.Module.Prelude.:<) x245 x246) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_342(x5)(x228)(x242)(x244)(x246)(x245)(x1)(st))(st)
c__case_343_case__708 x1 x5 x228 x242 x244 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_343_case__708(x1)(x5)(x228)(x242)(x244)(x)(st))(i)(xs)(st)
c__case_343_case__708 x1 x5 x228 x242 x244 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_343_case__708")(x)



c__case_344_case__709 x1 x5 x228 x242 x241@(Curry.Module.Prelude.T2 x243 x244) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_343(x5)(x228)(x242)(x244)(x243)(x1)(st))(st)
c__case_344_case__709 x1 x5 x228 x242 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_344_case__709(x1)(x5)(x228)(x242)(x)(st))(i)(xs)(st)
c__case_344_case__709 x1 x5 x228 x242 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_344_case__709")(x)



c__case_345_case__710 x1 x5 x228 x226@((Curry.Module.Prelude.:<) x241 x242) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_344(x5)(x228)(x242)(x241)(x1)(st))(st)
c__case_345_case__710 x1 x5 x228 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_345_case__710(x1)(x5)(x228)(x)(st))(i)(xs)(st)
c__case_345_case__710 x1 x5 x228 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_345_case__710")(x)



c__case_346_case__711 x1 x5 x226 x228 x240@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_345(x5)(x228)(x226)(x1)(st))(st)
c__case_346_case__711 x1 x5 x226 x228 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_346_case__711(x1)(x5)(x226)(x228)(x)(st))(i)(xs)(st)
c__case_346_case__711 x1 x5 x226 x228 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_346_case__711")(x)



c__case_347_case__712 x1 x5 x226 x228 x240 x239 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x239)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_346(x5)(x226)(x228)(x240)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_348_case__713 x1 x5 x226 x228 x238@((Curry.Module.Prelude.:<) x239 x240) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_347(x5)(x226)(x228)(x240)(x239)(x1)(st))(st)
c__case_348_case__713 x1 x5 x226 x228 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_348_case__713(x1)(x5)(x226)(x228)(x)(st))(i)(xs)(st)
c__case_348_case__713 x1 x5 x226 x228 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_348_case__713")(x)



c__case_349_case__714 x1 x5 x226 x228 x238 x237 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x237)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_348(x5)(x226)(x228)(x238)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_350_case__715 x1 x5 x226 x228 x236@((Curry.Module.Prelude.:<) x237 x238) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_349(x5)(x226)(x228)(x238)(x237)(x1)(st))(st)
c__case_350_case__715 x1 x5 x226 x228 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_350_case__715(x1)(x5)(x226)(x228)(x)(st))(i)(xs)(st)
c__case_350_case__715 x1 x5 x226 x228 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_350_case__715")(x)



c__case_351_case__716 x1 x5 x226 x228 x236 x235 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x235)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_350(x5)(x226)(x228)(x236)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_352_case__717 x1 x5 x226 x228 x234@((Curry.Module.Prelude.:<) x235 x236) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_351(x5)(x226)(x228)(x236)(x235)(x1)(st))(st)
c__case_352_case__717 x1 x5 x226 x228 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_352_case__717(x1)(x5)(x226)(x228)(x)(st))(i)(xs)(st)
c__case_352_case__717 x1 x5 x226 x228 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_352_case__717")(x)



c__case_353_case__718 x1 x5 x226 x228 x234 x233 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x233)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_352(x5)(x226)(x228)(x234)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_354_case__719 x1 x5 x226 x228 x232@((Curry.Module.Prelude.:<) x233 x234) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_353(x5)(x226)(x228)(x234)(x233)(x1)(st))(st)
c__case_354_case__719 x1 x5 x226 x228 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_354_case__719(x1)(x5)(x226)(x228)(x)(st))(i)(xs)(st)
c__case_354_case__719 x1 x5 x226 x228 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_354_case__719")(x)



c__case_355_case__720 x1 x5 x226 x228 x232 x231 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x231)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_354(x5)(x226)(x228)(x232)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_356_case__721 x1 x5 x226 x228 x230@((Curry.Module.Prelude.:<) x231 x232) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_355(x5)(x226)(x228)(x232)(x231)(x1)(st))(st)
c__case_356_case__721 x1 x5 x226 x228 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_356_case__721(x1)(x5)(x226)(x228)(x)(st))(i)(xs)(st)
c__case_356_case__721 x1 x5 x226 x228 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_356_case__721")(x)



c__case_357_case__722 x1 x5 x226 x228 x230 x229 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x229)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_356(x5)(x226)(x228)(x230)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_358_case__723 x1 x5 x226 x228 x227@((Curry.Module.Prelude.:<) x229 x230) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_357(x5)(x226)(x228)(x230)(x229)(x1)(st))(st)
c__case_358_case__723 x1 x5 x226 x228 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_358_case__723(x1)(x5)(x226)(x228)(x)(st))(i)(xs)(st)
c__case_358_case__723 x1 x5 x226 x228 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_358_case__723")(x)



c__case_359_case__724 x1 x5 x226 x225@(Curry.Module.Prelude.T2 x227 x228) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_358(x5)(x226)(x228)(x227)(x1)(st))(st)
c__case_359_case__724 x1 x5 x226 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_359_case__724(x1)(x5)(x226)(x)(st))(i)(xs)(st)
c__case_359_case__724 x1 x5 x226 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_359_case__724")(x)



c__case_360_case__725 x1 x5 x4@((Curry.Module.Prelude.:<) x225 x226) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_359(x5)(x226)(x225)(x1)(st))(st)
c__case_360_case__725 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_360_case__725(x1)(x5)(x)(st))(i)(xs)(st)
c__case_360_case__725 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_360_case__725")(x)



c__case_361_case__726 x1 x4 x5 x224@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_360(x5)(x4)(x1)(st))(st)
c__case_361_case__726 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_361_case__726(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_361_case__726 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_361_case__726")(x)



c__case_362_case__727 x1 x4 x5 x224 x223 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x223)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_361(x4)(x5)(x224)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_363_case__728 x1 x4 x5 x222@((Curry.Module.Prelude.:<) x223 x224) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_362(x4)(x5)(x224)(x223)(x1)(st))(st)
c__case_363_case__728 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_363_case__728(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_363_case__728 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_363_case__728")(x)



c__case_364_case__729 x1 x4 x5 x222 x221 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x221)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_363(x4)(x5)(x222)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_365_case__730 x1 x4 x5 x220@((Curry.Module.Prelude.:<) x221 x222) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_364(x4)(x5)(x222)(x221)(x1)(st))(st)
c__case_365_case__730 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_365_case__730(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_365_case__730 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_365_case__730")(x)



c__case_366_case__731 x1 x4 x5 x220 x219 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x219)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_365(x4)(x5)(x220)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_367_case__732 x1 x4 x5 x218@((Curry.Module.Prelude.:<) x219 x220) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_366(x4)(x5)(x220)(x219)(x1)(st))(st)
c__case_367_case__732 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_367_case__732(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_367_case__732 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_367_case__732")(x)



c__case_368_case__733 x1 x4 x5 x218 x217 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x217)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_367(x4)(x5)(x218)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_369_case__734 x1 x4 x5 x216@((Curry.Module.Prelude.:<) x217 x218) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_368(x4)(x5)(x218)(x217)(x1)(st))(st)
c__case_369_case__734 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_369_case__734(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_369_case__734 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_369_case__734")(x)



c__case_370_case__735 x1 x4 x5 x216 x215 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x215)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_369(x4)(x5)(x216)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_371_case__736 x1 x4 x5 x214@((Curry.Module.Prelude.:<) x215 x216) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_370(x4)(x5)(x216)(x215)(x1)(st))(st)
c__case_371_case__736 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_371_case__736(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_371_case__736 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_371_case__736")(x)



c__case_372_case__737 x1 x4 x5 x214 x213 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x213)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_371(x4)(x5)(x214)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_373_case__738 x1 x4 x5 x212@((Curry.Module.Prelude.:<) x213 x214) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_372(x4)(x5)(x214)(x213)(x1)(st))(st)
c__case_373_case__738 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_373_case__738(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_373_case__738 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_373_case__738")(x)



c__case_374_case__739 x1 x4 x5 x212 x211 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x211)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_373(x4)(x5)(x212)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_375_case__740 x1 x4 x5 x176@((Curry.Module.Prelude.:<) x211 x212) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_374(x4)(x5)(x212)(x211)(x1)(st))(st)
c__case_375_case__740 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_375_case__740(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_375_case__740 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_375_case__740")(x)



c__case_410_case__741 x1 x4 x5 x176 x175 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x175)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_409(x4)(x5)(x176)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x175)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_375(x4)(x5)(x176)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st)



c__case_411_case__742 x1 x4 x5 x174@((Curry.Module.Prelude.:<) x175 x176) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_410(x4)(x5)(x176)(x175)(x1)(st))(st)
c__case_411_case__742 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_411_case__742(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_411_case__742 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_411_case__742")(x)



c__case_412_case__743 x1 x4 x5 x174 x173 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x173)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_411(x4)(x5)(x174)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_413_case__744 x1 x4 x5 x172@((Curry.Module.Prelude.:<) x173 x174) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_412(x4)(x5)(x174)(x173)(x1)(st))(st)
c__case_413_case__744 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_413_case__744(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_413_case__744 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_413_case__744")(x)



c__case_414_case__745 x1 x4 x5 x172 x171 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x171)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_413(x4)(x5)(x172)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_415_case__746 x1 x4 x5 x170@((Curry.Module.Prelude.:<) x171 x172) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_414(x4)(x5)(x172)(x171)(x1)(st))(st)
c__case_415_case__746 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_415_case__746(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_415_case__746 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_415_case__746")(x)



c__case_310_case__747 x1 x5@((Curry.Module.Prelude.:<) x275 x276) st = let {x277 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x277)(Curry.Module.Prelude.List))(Curry.Module.FlatCurry.C_Case(Curry.Module.FlatCurry.C_Rigid)(Curry.Module.OracleFlatCurryXML.c_flatx2exp(x275)(x1)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_flatx2branch))))(x276)(x277)(st)))(st)
c__case_310_case__747 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_310_case__747(x1)(x)(st))(i)(xs)(st)
c__case_310_case__747 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_310_case__747")(x)



c__case_311_case__748 x1 x5 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_310(x5)(x1)(st))(st)
c__case_311_case__748 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_311_case__748(x1)(x5)(x)(st))(i)(xs)(st)
c__case_311_case__748 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_311_case__748")(x)



c__case_312_case__749 x1 x4 x5 x274@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_311(x5)(x4)(x1)(st))(st)
c__case_312_case__749 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_312_case__749(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_312_case__749 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_312_case__749")(x)



c__case_313_case__750 x1 x4 x5 x274 x273 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x273)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_312(x4)(x5)(x274)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_314_case__751 x1 x4 x5 x272@((Curry.Module.Prelude.:<) x273 x274) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_313(x4)(x5)(x274)(x273)(x1)(st))(st)
c__case_314_case__751 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_314_case__751(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_314_case__751 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_314_case__751")(x)



c__case_315_case__752 x1 x4 x5 x272 x271 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x271)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_314(x4)(x5)(x272)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_316_case__753 x1 x4 x5 x170@((Curry.Module.Prelude.:<) x271 x272) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_315(x4)(x5)(x272)(x271)(x1)(st))(st)
c__case_316_case__753 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_316_case__753(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_316_case__753 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_316_case__753")(x)



c__case_416_case__754 x1 x4 x5 x170 x169 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x169)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_415(x4)(x5)(x170)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x169)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_316(x4)(x5)(x170)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st)



c__case_417_case__755 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x169 x170) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_416(x4)(x5)(x170)(x169)(x1)(st))(st)
c__case_417_case__755 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_417_case__755(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_417_case__755 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_417_case__755")(x)



c__case_303_case__756 x1 x279 x281 x282@Curry.Module.Prelude.List st = let {x283 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x283)(Curry.Module.Prelude.List))(Curry.Module.FlatCurry.C_Or(Curry.Module.OracleFlatCurryXML.c_flatx2exp(x279)(x1)(st))(Curry.Module.OracleFlatCurryXML.c_flatx2exp(x281)(x283)(st)))(st)
c__case_303_case__756 x1 x279 x281 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_303_case__756(x1)(x279)(x281)(x)(st))(i)(xs)(st)
c__case_303_case__756 x1 x279 x281 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_303_case__756")(x)



c__case_304_case__757 x1 x279 x280@((Curry.Module.Prelude.:<) x281 x282) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_303(x279)(x281)(x282)(x1)(st))(st)
c__case_304_case__757 x1 x279 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_304_case__757(x1)(x279)(x)(st))(i)(xs)(st)
c__case_304_case__757 x1 x279 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_304_case__757")(x)



c__case_305_case__758 x1 x5@((Curry.Module.Prelude.:<) x279 x280) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_304(x279)(x280)(x1)(st))(st)
c__case_305_case__758 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_305_case__758(x1)(x)(st))(i)(xs)(st)
c__case_305_case__758 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_305_case__758")(x)



c__case_306_case__759 x1 x5 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_305(x5)(x1)(st))(st)
c__case_306_case__759 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_306_case__759(x1)(x5)(x)(st))(i)(xs)(st)
c__case_306_case__759 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_306_case__759")(x)



c__case_307_case__760 x1 x4 x5 x278@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_306(x5)(x4)(x1)(st))(st)
c__case_307_case__760 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_307_case__760(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_307_case__760 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_307_case__760")(x)



c__case_308_case__761 x1 x4 x5 x278 x277 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x277)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_307(x4)(x5)(x278)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_309_case__762 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x277 x278) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_308(x4)(x5)(x278)(x277)(x1)(st))(st)
c__case_309_case__762 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_309_case__762(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_309_case__762 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_309_case__762")(x)



c__case_581_case__763 x1 x4 x5 x7 x6 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('v'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_580(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_574(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('f'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_555(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_417(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_309(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st))(st))(st))(st)



c__case_582_case__764 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_581(x4)(x5)(x7)(x6)(x1)(st))(st)
c__case_582_case__764 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_582_case__764(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_582_case__764 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_582_case__764")(x)



c__case_583_case__765 x1 x2@(Curry.Module.XML.C_XElem x3 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_582(x4)(x5)(x3)(x1)(st))(st)
c__case_583_case__765 x1 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_583_case__765(x1)(x)(st))(i)(xs)(st)
c__case_583_case__765 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_583_case__765")(x)



c__case_302_case__766 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_302_case__766 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_302_case__766(x1)(x)(st))(i)(xs)(st)
c__case_302_case__766 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_302_case__766")(x)



c__case_301_case__767 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_301_case__767 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_301_case__767(x1)(x)(st))(i)(xs)(st)
c__case_301_case__767 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_301_case__767")(x)



c__case_300_case__768 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_300_case__768 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_300_case__768(x1)(x)(st))(i)(xs)(st)
c__case_300_case__768 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_300_case__768")(x)



c__case_299_case__769 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_299_case__769 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_299_case__769(x1)(x)(st))(i)(xs)(st)
c__case_299_case__769 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_299_case__769")(x)



c__case_268_case__770 x1 x5 x6 x28 x35 x36@Curry.Module.Prelude.List st = let {x40 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x40)(Curry.Module.Prelude.List))(let {x37 = Curry.Module.OracleFlatCurryXML.c_flatx2let((Curry.Module.Prelude.:<)(x5)(x6))(x1)(st)} in let {x41 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x40)((Curry.Module.Prelude.:<)(x41)(Curry.Module.Prelude.List))(let {x42 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x41)((Curry.Module.Prelude.:<)(x42)(Curry.Module.Prelude.List))(let {x43 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x44 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x42)((Curry.Module.Prelude.:<)(x43)((Curry.Module.Prelude.:<)(x44)(Curry.Module.Prelude.List)))(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(Curry.Module.OracleRead.c_readNat(Curry.Module.OracleXML.c_textOfXml(x28)(x42)(st))(x43)(st))(Curry.Module.OracleFlatCurryXML.c_flatx2exp(x35)(x44)(st)))(Curry.Module.OracleFlatCurryXML.c_flatx2let'46_'35selFP9'35bindings(x37)(x40)(st)))(Curry.Module.OracleFlatCurryXML.c_flatx2let'46_'35selFP10'35exp(x37)(x41)(st)))(st))(st))(st))(st)
c__case_268_case__770 x1 x5 x6 x28 x35 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_268_case__770(x1)(x5)(x6)(x28)(x35)(x)(st))(i)(xs)(st)
c__case_268_case__770 x1 x5 x6 x28 x35 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_268_case__770")(x)



c__case_269_case__771 x1 x5 x6 x28 x25@((Curry.Module.Prelude.:<) x35 x36) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_268(x5)(x6)(x28)(x35)(x36)(x1)(st))(st)
c__case_269_case__771 x1 x5 x6 x28 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_269_case__771(x1)(x5)(x6)(x28)(x)(st))(i)(xs)(st)
c__case_269_case__771 x1 x5 x6 x28 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_269_case__771")(x)



c__case_270_case__772 x1 x5 x6 x25 x28 x27@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_269(x5)(x6)(x28)(x25)(x1)(st))(st)
c__case_270_case__772 x1 x5 x6 x25 x28 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_270_case__772(x1)(x5)(x6)(x25)(x28)(x)(st))(i)(xs)(st)
c__case_270_case__772 x1 x5 x6 x25 x28 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_270_case__772")(x)



c__case_271_case__773 x1 x5 x6 x25 x27 x28 x34@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_270(x5)(x6)(x25)(x28)(x27)(x1)(st))(st)
c__case_271_case__773 x1 x5 x6 x25 x27 x28 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_271_case__773(x1)(x5)(x6)(x25)(x27)(x28)(x)(st))(i)(xs)(st)
c__case_271_case__773 x1 x5 x6 x25 x27 x28 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_271_case__773")(x)



c__case_272_case__774 x1 x5 x6 x25 x27 x28 x34 x33 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x33)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_271(x5)(x6)(x25)(x27)(x28)(x34)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_273_case__775 x1 x5 x6 x25 x27 x28 x32@((Curry.Module.Prelude.:<) x33 x34) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_272(x5)(x6)(x25)(x27)(x28)(x34)(x33)(x1)(st))(st)
c__case_273_case__775 x1 x5 x6 x25 x27 x28 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_273_case__775(x1)(x5)(x6)(x25)(x27)(x28)(x)(st))(i)(xs)(st)
c__case_273_case__775 x1 x5 x6 x25 x27 x28 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_273_case__775")(x)



c__case_274_case__776 x1 x5 x6 x25 x27 x28 x32 x31 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x31)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_273(x5)(x6)(x25)(x27)(x28)(x32)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_275_case__777 x1 x5 x6 x25 x27 x28 x30@((Curry.Module.Prelude.:<) x31 x32) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_274(x5)(x6)(x25)(x27)(x28)(x32)(x31)(x1)(st))(st)
c__case_275_case__777 x1 x5 x6 x25 x27 x28 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_275_case__777(x1)(x5)(x6)(x25)(x27)(x28)(x)(st))(i)(xs)(st)
c__case_275_case__777 x1 x5 x6 x25 x27 x28 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_275_case__777")(x)



c__case_276_case__778 x1 x5 x6 x25 x27 x28 x30 x29 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x29)(Curry.Module.Prelude.C_Char('v'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_275(x5)(x6)(x25)(x27)(x28)(x30)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_277_case__779 x1 x5 x6 x25 x27 x28 x26@((Curry.Module.Prelude.:<) x29 x30) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_276(x5)(x6)(x25)(x27)(x28)(x30)(x29)(x1)(st))(st)
c__case_277_case__779 x1 x5 x6 x25 x27 x28 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_277_case__779(x1)(x5)(x6)(x25)(x27)(x28)(x)(st))(i)(xs)(st)
c__case_277_case__779 x1 x5 x6 x25 x27 x28 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_277_case__779")(x)



c__case_278_case__780 x1 x5 x6 x25 x24@(Curry.Module.XML.C_XElem x26 x27 x28) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_277(x5)(x6)(x25)(x27)(x28)(x26)(x1)(st))(st)
c__case_278_case__780 x1 x5 x6 x25 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_278_case__780(x1)(x5)(x6)(x25)(x)(st))(i)(xs)(st)
c__case_278_case__780 x1 x5 x6 x25 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_278_case__780")(x)



c__case_279_case__781 x1 x5 x6 x9@((Curry.Module.Prelude.:<) x24 x25) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_278(x5)(x6)(x25)(x24)(x1)(st))(st)
c__case_279_case__781 x1 x5 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_279_case__781(x1)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_279_case__781 x1 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_279_case__781")(x)



c__case_280_case__782 x1 x5 x6 x9 x8@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_279(x5)(x6)(x9)(x1)(st))(st)
c__case_280_case__782 x1 x5 x6 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_280_case__782(x1)(x5)(x6)(x9)(x)(st))(i)(xs)(st)
c__case_280_case__782 x1 x5 x6 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_280_case__782")(x)



c__case_281_case__783 x1 x5 x6 x8 x9 x23@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_280(x5)(x6)(x9)(x8)(x1)(st))(st)
c__case_281_case__783 x1 x5 x6 x8 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_281_case__783(x1)(x5)(x6)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_281_case__783 x1 x5 x6 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_281_case__783")(x)



c__case_282_case__784 x1 x5 x6 x8 x9 x23 x22 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x22)(Curry.Module.Prelude.C_Char('g'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_281(x5)(x6)(x8)(x9)(x23)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_283_case__785 x1 x5 x6 x8 x9 x21@((Curry.Module.Prelude.:<) x22 x23) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_282(x5)(x6)(x8)(x9)(x23)(x22)(x1)(st))(st)
c__case_283_case__785 x1 x5 x6 x8 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_283_case__785(x1)(x5)(x6)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_283_case__785 x1 x5 x6 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_283_case__785")(x)



c__case_284_case__786 x1 x5 x6 x8 x9 x21 x20 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x20)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_283(x5)(x6)(x8)(x9)(x21)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_285_case__787 x1 x5 x6 x8 x9 x19@((Curry.Module.Prelude.:<) x20 x21) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_284(x5)(x6)(x8)(x9)(x21)(x20)(x1)(st))(st)
c__case_285_case__787 x1 x5 x6 x8 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_285_case__787(x1)(x5)(x6)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_285_case__787 x1 x5 x6 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_285_case__787")(x)



c__case_286_case__788 x1 x5 x6 x8 x9 x19 x18 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x18)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_285(x5)(x6)(x8)(x9)(x19)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_287_case__789 x1 x5 x6 x8 x9 x17@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_286(x5)(x6)(x8)(x9)(x19)(x18)(x1)(st))(st)
c__case_287_case__789 x1 x5 x6 x8 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_287_case__789(x1)(x5)(x6)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_287_case__789 x1 x5 x6 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_287_case__789")(x)



c__case_288_case__790 x1 x5 x6 x8 x9 x17 x16 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x16)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_287(x5)(x6)(x8)(x9)(x17)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_289_case__791 x1 x5 x6 x8 x9 x15@((Curry.Module.Prelude.:<) x16 x17) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_288(x5)(x6)(x8)(x9)(x17)(x16)(x1)(st))(st)
c__case_289_case__791 x1 x5 x6 x8 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_289_case__791(x1)(x5)(x6)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_289_case__791 x1 x5 x6 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_289_case__791")(x)



c__case_290_case__792 x1 x5 x6 x8 x9 x15 x14 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x14)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_289(x5)(x6)(x8)(x9)(x15)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_291_case__793 x1 x5 x6 x8 x9 x13@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_290(x5)(x6)(x8)(x9)(x15)(x14)(x1)(st))(st)
c__case_291_case__793 x1 x5 x6 x8 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_291_case__793(x1)(x5)(x6)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_291_case__793 x1 x5 x6 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_291_case__793")(x)



c__case_292_case__794 x1 x5 x6 x8 x9 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_291(x5)(x6)(x8)(x9)(x13)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_293_case__795 x1 x5 x6 x8 x9 x11@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_292(x5)(x6)(x8)(x9)(x13)(x12)(x1)(st))(st)
c__case_293_case__795 x1 x5 x6 x8 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_293_case__795(x1)(x5)(x6)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_293_case__795 x1 x5 x6 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_293_case__795")(x)



c__case_294_case__796 x1 x5 x6 x8 x9 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('b'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_293(x5)(x6)(x8)(x9)(x11)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_295_case__797 x1 x5 x6 x8 x9 x7@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_294(x5)(x6)(x8)(x9)(x11)(x10)(x1)(st))(st)
c__case_295_case__797 x1 x5 x6 x8 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_295_case__797(x1)(x5)(x6)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_295_case__797 x1 x5 x6 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_295_case__797")(x)



c__case_296_case__798 x1 x5 x6 x3@(Curry.Module.XML.C_XElem x7 x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_295(x5)(x6)(x8)(x9)(x7)(x1)(st))(st)
c__case_296_case__798 x1 x5 x6 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_296_case__798(x1)(x5)(x6)(x)(st))(i)(xs)(st)
c__case_296_case__798 x1 x5 x6 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_296_case__798")(x)



c__case_297_case__799 x1 x3 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.List)(Curry.Module.OracleFlatCurryXML.c_flatx2exp(x3)(x1)(st)))(st)
c__case_297_case__799 x1 x3 x4@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_296(x5)(x6)(x3)(x1)(st))(st)
c__case_297_case__799 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_297_case__799(x1)(x3)(x)(st))(i)(xs)(st)
c__case_297_case__799 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_297_case__799")(x)



c__case_298_case__800 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_297(x3)(x4)(x1)(st))(st)
c__case_298_case__800 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_298_case__800(x1)(x)(st))(i)(xs)(st)
c__case_298_case__800 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_298_case__800")(x)



c__case_267_case__801 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_267_case__801 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_267_case__801(x1)(x)(st))(i)(xs)(st)
c__case_267_case__801 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_267_case__801")(x)



c__case_266_case__802 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_266_case__802 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_266_case__802(x1)(x)(st))(i)(xs)(st)
c__case_266_case__802 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_266_case__802")(x)



c__case_205_case__803 x1 x22 x40 x56 x65 x66@Curry.Module.Prelude.List st = let {x67 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x67)(Curry.Module.Prelude.List))(Curry.Module.FlatCurry.C_Branch(Curry.Module.FlatCurry.C_Pattern(Curry.Module.Prelude.T2(x40)(x56))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_flatx2var))))(x22)(x1)(st)))(Curry.Module.OracleFlatCurryXML.c_flatx2exp(x65)(x67)(st)))(st)
c__case_205_case__803 x1 x22 x40 x56 x65 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_205_case__803(x1)(x22)(x40)(x56)(x65)(x)(st))(i)(xs)(st)
c__case_205_case__803 x1 x22 x40 x56 x65 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_205_case__803")(x)



c__case_206_case__804 x1 x22 x40 x56 x19@((Curry.Module.Prelude.:<) x65 x66) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_205(x22)(x40)(x56)(x65)(x66)(x1)(st))(st)
c__case_206_case__804 x1 x22 x40 x56 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_206_case__804(x1)(x22)(x40)(x56)(x)(st))(i)(xs)(st)
c__case_206_case__804 x1 x22 x40 x56 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_206_case__804")(x)



c__case_207_case__805 x1 x19 x22 x40 x56 x54@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_206(x22)(x40)(x56)(x19)(x1)(st))(st)
c__case_207_case__805 x1 x19 x22 x40 x56 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_207_case__805(x1)(x19)(x22)(x40)(x56)(x)(st))(i)(xs)(st)
c__case_207_case__805 x1 x19 x22 x40 x56 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_207_case__805")(x)



c__case_208_case__806 x1 x19 x22 x40 x54 x56 x64@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_207(x19)(x22)(x40)(x56)(x54)(x1)(st))(st)
c__case_208_case__806 x1 x19 x22 x40 x54 x56 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_208_case__806(x1)(x19)(x22)(x40)(x54)(x56)(x)(st))(i)(xs)(st)
c__case_208_case__806 x1 x19 x22 x40 x54 x56 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_208_case__806")(x)



c__case_209_case__807 x1 x19 x22 x40 x54 x56 x64 x63 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x63)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_208(x19)(x22)(x40)(x54)(x56)(x64)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_210_case__808 x1 x19 x22 x40 x54 x56 x62@((Curry.Module.Prelude.:<) x63 x64) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_209(x19)(x22)(x40)(x54)(x56)(x64)(x63)(x1)(st))(st)
c__case_210_case__808 x1 x19 x22 x40 x54 x56 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_210_case__808(x1)(x19)(x22)(x40)(x54)(x56)(x)(st))(i)(xs)(st)
c__case_210_case__808 x1 x19 x22 x40 x54 x56 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_210_case__808")(x)



c__case_211_case__809 x1 x19 x22 x40 x54 x56 x62 x61 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x61)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_210(x19)(x22)(x40)(x54)(x56)(x62)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_212_case__810 x1 x19 x22 x40 x54 x56 x60@((Curry.Module.Prelude.:<) x61 x62) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_211(x19)(x22)(x40)(x54)(x56)(x62)(x61)(x1)(st))(st)
c__case_212_case__810 x1 x19 x22 x40 x54 x56 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_212_case__810(x1)(x19)(x22)(x40)(x54)(x56)(x)(st))(i)(xs)(st)
c__case_212_case__810 x1 x19 x22 x40 x54 x56 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_212_case__810")(x)



c__case_213_case__811 x1 x19 x22 x40 x54 x56 x60 x59 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x59)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_212(x19)(x22)(x40)(x54)(x56)(x60)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_214_case__812 x1 x19 x22 x40 x54 x56 x58@((Curry.Module.Prelude.:<) x59 x60) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_213(x19)(x22)(x40)(x54)(x56)(x60)(x59)(x1)(st))(st)
c__case_214_case__812 x1 x19 x22 x40 x54 x56 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_214_case__812(x1)(x19)(x22)(x40)(x54)(x56)(x)(st))(i)(xs)(st)
c__case_214_case__812 x1 x19 x22 x40 x54 x56 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_214_case__812")(x)



c__case_215_case__813 x1 x19 x22 x40 x54 x56 x58 x57 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x57)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_214(x19)(x22)(x40)(x54)(x56)(x58)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_216_case__814 x1 x19 x22 x40 x54 x56 x55@((Curry.Module.Prelude.:<) x57 x58) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_215(x19)(x22)(x40)(x54)(x56)(x58)(x57)(x1)(st))(st)
c__case_216_case__814 x1 x19 x22 x40 x54 x56 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_216_case__814(x1)(x19)(x22)(x40)(x54)(x56)(x)(st))(i)(xs)(st)
c__case_216_case__814 x1 x19 x22 x40 x54 x56 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_216_case__814")(x)



c__case_217_case__815 x1 x19 x22 x40 x54 x53@(Curry.Module.Prelude.T2 x55 x56) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_216(x19)(x22)(x40)(x54)(x56)(x55)(x1)(st))(st)
c__case_217_case__815 x1 x19 x22 x40 x54 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_217_case__815(x1)(x19)(x22)(x40)(x54)(x)(st))(i)(xs)(st)
c__case_217_case__815 x1 x19 x22 x40 x54 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_217_case__815")(x)



c__case_218_case__816 x1 x19 x22 x40 x38@((Curry.Module.Prelude.:<) x53 x54) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_217(x19)(x22)(x40)(x54)(x53)(x1)(st))(st)
c__case_218_case__816 x1 x19 x22 x40 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_218_case__816(x1)(x19)(x22)(x40)(x)(st))(i)(xs)(st)
c__case_218_case__816 x1 x19 x22 x40 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_218_case__816")(x)



c__case_219_case__817 x1 x19 x22 x38 x40 x52@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_218(x19)(x22)(x40)(x38)(x1)(st))(st)
c__case_219_case__817 x1 x19 x22 x38 x40 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_219_case__817(x1)(x19)(x22)(x38)(x40)(x)(st))(i)(xs)(st)
c__case_219_case__817 x1 x19 x22 x38 x40 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_219_case__817")(x)



c__case_220_case__818 x1 x19 x22 x38 x40 x52 x51 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x51)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_219(x19)(x22)(x38)(x40)(x52)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_221_case__819 x1 x19 x22 x38 x40 x50@((Curry.Module.Prelude.:<) x51 x52) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_220(x19)(x22)(x38)(x40)(x52)(x51)(x1)(st))(st)
c__case_221_case__819 x1 x19 x22 x38 x40 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_221_case__819(x1)(x19)(x22)(x38)(x40)(x)(st))(i)(xs)(st)
c__case_221_case__819 x1 x19 x22 x38 x40 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_221_case__819")(x)



c__case_222_case__820 x1 x19 x22 x38 x40 x50 x49 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x49)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_221(x19)(x22)(x38)(x40)(x50)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_223_case__821 x1 x19 x22 x38 x40 x48@((Curry.Module.Prelude.:<) x49 x50) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_222(x19)(x22)(x38)(x40)(x50)(x49)(x1)(st))(st)
c__case_223_case__821 x1 x19 x22 x38 x40 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_223_case__821(x1)(x19)(x22)(x38)(x40)(x)(st))(i)(xs)(st)
c__case_223_case__821 x1 x19 x22 x38 x40 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_223_case__821")(x)



c__case_224_case__822 x1 x19 x22 x38 x40 x48 x47 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x47)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_223(x19)(x22)(x38)(x40)(x48)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_225_case__823 x1 x19 x22 x38 x40 x46@((Curry.Module.Prelude.:<) x47 x48) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_224(x19)(x22)(x38)(x40)(x48)(x47)(x1)(st))(st)
c__case_225_case__823 x1 x19 x22 x38 x40 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_225_case__823(x1)(x19)(x22)(x38)(x40)(x)(st))(i)(xs)(st)
c__case_225_case__823 x1 x19 x22 x38 x40 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_225_case__823")(x)



c__case_226_case__824 x1 x19 x22 x38 x40 x46 x45 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x45)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_225(x19)(x22)(x38)(x40)(x46)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_227_case__825 x1 x19 x22 x38 x40 x44@((Curry.Module.Prelude.:<) x45 x46) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_226(x19)(x22)(x38)(x40)(x46)(x45)(x1)(st))(st)
c__case_227_case__825 x1 x19 x22 x38 x40 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_227_case__825(x1)(x19)(x22)(x38)(x40)(x)(st))(i)(xs)(st)
c__case_227_case__825 x1 x19 x22 x38 x40 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_227_case__825")(x)



c__case_228_case__826 x1 x19 x22 x38 x40 x44 x43 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x43)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_227(x19)(x22)(x38)(x40)(x44)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_229_case__827 x1 x19 x22 x38 x40 x42@((Curry.Module.Prelude.:<) x43 x44) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_228(x19)(x22)(x38)(x40)(x44)(x43)(x1)(st))(st)
c__case_229_case__827 x1 x19 x22 x38 x40 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_229_case__827(x1)(x19)(x22)(x38)(x40)(x)(st))(i)(xs)(st)
c__case_229_case__827 x1 x19 x22 x38 x40 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_229_case__827")(x)



c__case_230_case__828 x1 x19 x22 x38 x40 x42 x41 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x41)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_229(x19)(x22)(x38)(x40)(x42)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_231_case__829 x1 x19 x22 x38 x40 x39@((Curry.Module.Prelude.:<) x41 x42) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_230(x19)(x22)(x38)(x40)(x42)(x41)(x1)(st))(st)
c__case_231_case__829 x1 x19 x22 x38 x40 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_231_case__829(x1)(x19)(x22)(x38)(x40)(x)(st))(i)(xs)(st)
c__case_231_case__829 x1 x19 x22 x38 x40 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_231_case__829")(x)



c__case_232_case__830 x1 x19 x22 x38 x37@(Curry.Module.Prelude.T2 x39 x40) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_231(x19)(x22)(x38)(x40)(x39)(x1)(st))(st)
c__case_232_case__830 x1 x19 x22 x38 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_232_case__830(x1)(x19)(x22)(x38)(x)(st))(i)(xs)(st)
c__case_232_case__830 x1 x19 x22 x38 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_232_case__830")(x)



c__case_233_case__831 x1 x19 x22 x21@((Curry.Module.Prelude.:<) x37 x38) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_232(x19)(x22)(x38)(x37)(x1)(st))(st)
c__case_233_case__831 x1 x19 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_233_case__831(x1)(x19)(x22)(x)(st))(i)(xs)(st)
c__case_233_case__831 x1 x19 x22 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_233_case__831")(x)



c__case_234_case__832 x1 x19 x21 x22 x36@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_233(x19)(x22)(x21)(x1)(st))(st)
c__case_234_case__832 x1 x19 x21 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_234_case__832(x1)(x19)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_234_case__832 x1 x19 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_234_case__832")(x)



c__case_235_case__833 x1 x19 x21 x22 x36 x35 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x35)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_234(x19)(x21)(x22)(x36)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_236_case__834 x1 x19 x21 x22 x34@((Curry.Module.Prelude.:<) x35 x36) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_235(x19)(x21)(x22)(x36)(x35)(x1)(st))(st)
c__case_236_case__834 x1 x19 x21 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_236_case__834(x1)(x19)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_236_case__834 x1 x19 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_236_case__834")(x)



c__case_237_case__835 x1 x19 x21 x22 x34 x33 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x33)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_236(x19)(x21)(x22)(x34)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_238_case__836 x1 x19 x21 x22 x32@((Curry.Module.Prelude.:<) x33 x34) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_237(x19)(x21)(x22)(x34)(x33)(x1)(st))(st)
c__case_238_case__836 x1 x19 x21 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_238_case__836(x1)(x19)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_238_case__836 x1 x19 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_238_case__836")(x)



c__case_239_case__837 x1 x19 x21 x22 x32 x31 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x31)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_238(x19)(x21)(x22)(x32)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_240_case__838 x1 x19 x21 x22 x30@((Curry.Module.Prelude.:<) x31 x32) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_239(x19)(x21)(x22)(x32)(x31)(x1)(st))(st)
c__case_240_case__838 x1 x19 x21 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_240_case__838(x1)(x19)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_240_case__838 x1 x19 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_240_case__838")(x)



c__case_241_case__839 x1 x19 x21 x22 x30 x29 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x29)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_240(x19)(x21)(x22)(x30)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_242_case__840 x1 x19 x21 x22 x28@((Curry.Module.Prelude.:<) x29 x30) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_241(x19)(x21)(x22)(x30)(x29)(x1)(st))(st)
c__case_242_case__840 x1 x19 x21 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_242_case__840(x1)(x19)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_242_case__840 x1 x19 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_242_case__840")(x)



c__case_243_case__841 x1 x19 x21 x22 x28 x27 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x27)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_242(x19)(x21)(x22)(x28)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_244_case__842 x1 x19 x21 x22 x26@((Curry.Module.Prelude.:<) x27 x28) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_243(x19)(x21)(x22)(x28)(x27)(x1)(st))(st)
c__case_244_case__842 x1 x19 x21 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_244_case__842(x1)(x19)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_244_case__842 x1 x19 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_244_case__842")(x)



c__case_245_case__843 x1 x19 x21 x22 x26 x25 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x25)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_244(x19)(x21)(x22)(x26)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_246_case__844 x1 x19 x21 x22 x24@((Curry.Module.Prelude.:<) x25 x26) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_245(x19)(x21)(x22)(x26)(x25)(x1)(st))(st)
c__case_246_case__844 x1 x19 x21 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_246_case__844(x1)(x19)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_246_case__844 x1 x19 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_246_case__844")(x)



c__case_185_case__845 x1 x81 x83 x84@Curry.Module.Prelude.List st = let {x85 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x85)(Curry.Module.Prelude.List))(Curry.Module.FlatCurry.C_Branch(Curry.Module.FlatCurry.C_LPattern(Curry.Module.OracleFlatCurryXML.c_flatx2lit(x81)(x1)(st)))(Curry.Module.OracleFlatCurryXML.c_flatx2exp(x83)(x85)(st)))(st)
c__case_185_case__845 x1 x81 x83 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_185_case__845(x1)(x81)(x83)(x)(st))(i)(xs)(st)
c__case_185_case__845 x1 x81 x83 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_185_case__845")(x)



c__case_186_case__846 x1 x81 x19@((Curry.Module.Prelude.:<) x83 x84) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_185(x81)(x83)(x84)(x1)(st))(st)
c__case_186_case__846 x1 x81 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_186_case__846(x1)(x81)(x)(st))(i)(xs)(st)
c__case_186_case__846 x1 x81 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_186_case__846")(x)



c__case_187_case__847 x1 x19 x81 x82@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_186(x81)(x19)(x1)(st))(st)
c__case_187_case__847 x1 x19 x81 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_187_case__847(x1)(x19)(x81)(x)(st))(i)(xs)(st)
c__case_187_case__847 x1 x19 x81 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_187_case__847")(x)



c__case_188_case__848 x1 x19 x22@((Curry.Module.Prelude.:<) x81 x82) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_187(x19)(x81)(x82)(x1)(st))(st)
c__case_188_case__848 x1 x19 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_188_case__848(x1)(x19)(x)(st))(i)(xs)(st)
c__case_188_case__848 x1 x19 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_188_case__848")(x)



c__case_189_case__849 x1 x19 x22 x21@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_188(x19)(x22)(x1)(st))(st)
c__case_189_case__849 x1 x19 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_189_case__849(x1)(x19)(x22)(x)(st))(i)(xs)(st)
c__case_189_case__849 x1 x19 x22 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_189_case__849")(x)



c__case_190_case__850 x1 x19 x21 x22 x80@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_189(x19)(x22)(x21)(x1)(st))(st)
c__case_190_case__850 x1 x19 x21 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_190_case__850(x1)(x19)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_190_case__850 x1 x19 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_190_case__850")(x)



c__case_191_case__851 x1 x19 x21 x22 x80 x79 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x79)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_190(x19)(x21)(x22)(x80)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_192_case__852 x1 x19 x21 x22 x78@((Curry.Module.Prelude.:<) x79 x80) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_191(x19)(x21)(x22)(x80)(x79)(x1)(st))(st)
c__case_192_case__852 x1 x19 x21 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_192_case__852(x1)(x19)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_192_case__852 x1 x19 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_192_case__852")(x)



c__case_193_case__853 x1 x19 x21 x22 x78 x77 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x77)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_192(x19)(x21)(x22)(x78)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_194_case__854 x1 x19 x21 x22 x76@((Curry.Module.Prelude.:<) x77 x78) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_193(x19)(x21)(x22)(x78)(x77)(x1)(st))(st)
c__case_194_case__854 x1 x19 x21 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_194_case__854(x1)(x19)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_194_case__854 x1 x19 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_194_case__854")(x)



c__case_195_case__855 x1 x19 x21 x22 x76 x75 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x75)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_194(x19)(x21)(x22)(x76)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_196_case__856 x1 x19 x21 x22 x74@((Curry.Module.Prelude.:<) x75 x76) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_195(x19)(x21)(x22)(x76)(x75)(x1)(st))(st)
c__case_196_case__856 x1 x19 x21 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_196_case__856(x1)(x19)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_196_case__856 x1 x19 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_196_case__856")(x)



c__case_197_case__857 x1 x19 x21 x22 x74 x73 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x73)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_196(x19)(x21)(x22)(x74)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_198_case__858 x1 x19 x21 x22 x72@((Curry.Module.Prelude.:<) x73 x74) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_197(x19)(x21)(x22)(x74)(x73)(x1)(st))(st)
c__case_198_case__858 x1 x19 x21 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_198_case__858(x1)(x19)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_198_case__858 x1 x19 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_198_case__858")(x)



c__case_199_case__859 x1 x19 x21 x22 x72 x71 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x71)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_198(x19)(x21)(x22)(x72)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_200_case__860 x1 x19 x21 x22 x70@((Curry.Module.Prelude.:<) x71 x72) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_199(x19)(x21)(x22)(x72)(x71)(x1)(st))(st)
c__case_200_case__860 x1 x19 x21 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_200_case__860(x1)(x19)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_200_case__860 x1 x19 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_200_case__860")(x)



c__case_201_case__861 x1 x19 x21 x22 x70 x69 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x69)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_200(x19)(x21)(x22)(x70)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_202_case__862 x1 x19 x21 x22 x68@((Curry.Module.Prelude.:<) x69 x70) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_201(x19)(x21)(x22)(x70)(x69)(x1)(st))(st)
c__case_202_case__862 x1 x19 x21 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_202_case__862(x1)(x19)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_202_case__862 x1 x19 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_202_case__862")(x)



c__case_203_case__863 x1 x19 x21 x22 x68 x67 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x67)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_202(x19)(x21)(x22)(x68)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_204_case__864 x1 x19 x21 x22 x24@((Curry.Module.Prelude.:<) x67 x68) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_203(x19)(x21)(x22)(x68)(x67)(x1)(st))(st)
c__case_204_case__864 x1 x19 x21 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_204_case__864(x1)(x19)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_204_case__864 x1 x19 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_204_case__864")(x)



c__case_168_case__865 x1 x100@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('H'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))(x1)(st))(st)
c__case_168_case__865 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_168_case__865(x1)(x)(st))(i)(xs)(st)
c__case_168_case__865 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_168_case__865")(x)



c__case_169_case__866 x1 x19@((Curry.Module.Prelude.:<) x99 x100) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_168(x100)(x1)(st))(st)
c__case_169_case__866 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_169_case__866(x1)(x)(st))(i)(xs)(st)
c__case_169_case__866 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_169_case__866")(x)



c__case_170_case__867 x1 x19 x98@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_169(x19)(x1)(st))(st)
c__case_170_case__867 x1 x19 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_170_case__867(x1)(x19)(x)(st))(i)(xs)(st)
c__case_170_case__867 x1 x19 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_170_case__867")(x)



c__case_171_case__868 x1 x19 x98 x97 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x97)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_170(x19)(x98)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_172_case__869 x1 x19 x96@((Curry.Module.Prelude.:<) x97 x98) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_171(x19)(x98)(x97)(x1)(st))(st)
c__case_172_case__869 x1 x19 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_172_case__869(x1)(x19)(x)(st))(i)(xs)(st)
c__case_172_case__869 x1 x19 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_172_case__869")(x)



c__case_173_case__870 x1 x19 x96 x95 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x95)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_172(x19)(x96)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_174_case__871 x1 x19 x94@((Curry.Module.Prelude.:<) x95 x96) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_173(x19)(x96)(x95)(x1)(st))(st)
c__case_174_case__871 x1 x19 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_174_case__871(x1)(x19)(x)(st))(i)(xs)(st)
c__case_174_case__871 x1 x19 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_174_case__871")(x)



c__case_175_case__872 x1 x19 x94 x93 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x93)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_174(x19)(x94)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_176_case__873 x1 x19 x92@((Curry.Module.Prelude.:<) x93 x94) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_175(x19)(x94)(x93)(x1)(st))(st)
c__case_176_case__873 x1 x19 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_176_case__873(x1)(x19)(x)(st))(i)(xs)(st)
c__case_176_case__873 x1 x19 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_176_case__873")(x)



c__case_177_case__874 x1 x19 x92 x91 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x91)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_176(x19)(x92)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_178_case__875 x1 x19 x90@((Curry.Module.Prelude.:<) x91 x92) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_177(x19)(x92)(x91)(x1)(st))(st)
c__case_178_case__875 x1 x19 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_178_case__875(x1)(x19)(x)(st))(i)(xs)(st)
c__case_178_case__875 x1 x19 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_178_case__875")(x)



c__case_179_case__876 x1 x19 x90 x89 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x89)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_178(x19)(x90)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_180_case__877 x1 x19 x88@((Curry.Module.Prelude.:<) x89 x90) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_179(x19)(x90)(x89)(x1)(st))(st)
c__case_180_case__877 x1 x19 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_180_case__877(x1)(x19)(x)(st))(i)(xs)(st)
c__case_180_case__877 x1 x19 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_180_case__877")(x)



c__case_181_case__878 x1 x19 x88 x87 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x87)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_180(x19)(x88)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_182_case__879 x1 x19 x86@((Curry.Module.Prelude.:<) x87 x88) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_181(x19)(x88)(x87)(x1)(st))(st)
c__case_182_case__879 x1 x19 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_182_case__879(x1)(x19)(x)(st))(i)(xs)(st)
c__case_182_case__879 x1 x19 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_182_case__879")(x)



c__case_183_case__880 x1 x19 x86 x85 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x85)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_182(x19)(x86)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_184_case__881 x1 x19 x24@((Curry.Module.Prelude.:<) x85 x86) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_183(x19)(x86)(x85)(x1)(st))(st)
c__case_184_case__881 x1 x19 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_184_case__881(x1)(x19)(x)(st))(i)(xs)(st)
c__case_184_case__881 x1 x19 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_184_case__881")(x)



c__case_247_case__882 x1 x19 x21 x22 x24 x23 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x23)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_246(x19)(x21)(x22)(x24)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x23)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_204(x19)(x21)(x22)(x24)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x23)(Curry.Module.Prelude.C_Char('h'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_184(x19)(x24)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st))(st)



c__case_248_case__883 x1 x19 x21 x22 x20@((Curry.Module.Prelude.:<) x23 x24) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_247(x19)(x21)(x22)(x24)(x23)(x1)(st))(st)
c__case_248_case__883 x1 x19 x21 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_248_case__883(x1)(x19)(x21)(x22)(x)(st))(i)(xs)(st)
c__case_248_case__883 x1 x19 x21 x22 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_248_case__883")(x)



c__case_249_case__884 x1 x19 x18@(Curry.Module.XML.C_XElem x20 x21 x22) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_248(x19)(x21)(x22)(x20)(x1)(st))(st)
c__case_249_case__884 x1 x19 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_249_case__884(x1)(x19)(x)(st))(i)(xs)(st)
c__case_249_case__884 x1 x19 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_249_case__884")(x)



c__case_250_case__885 x1 x5@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_249(x19)(x18)(x1)(st))(st)
c__case_250_case__885 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_250_case__885(x1)(x)(st))(i)(xs)(st)
c__case_250_case__885 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_250_case__885")(x)



c__case_251_case__886 x1 x5 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_250(x5)(x1)(st))(st)
c__case_251_case__886 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_251_case__886(x1)(x5)(x)(st))(i)(xs)(st)
c__case_251_case__886 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_251_case__886")(x)



c__case_252_case__887 x1 x4 x5 x17@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_251(x5)(x4)(x1)(st))(st)
c__case_252_case__887 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_252_case__887(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_252_case__887 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_252_case__887")(x)



c__case_253_case__888 x1 x4 x5 x17 x16 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x16)(Curry.Module.Prelude.C_Char('h'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_252(x4)(x5)(x17)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_254_case__889 x1 x4 x5 x15@((Curry.Module.Prelude.:<) x16 x17) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_253(x4)(x5)(x17)(x16)(x1)(st))(st)
c__case_254_case__889 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_254_case__889(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_254_case__889 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_254_case__889")(x)



c__case_255_case__890 x1 x4 x5 x15 x14 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x14)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_254(x4)(x5)(x15)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_256_case__891 x1 x4 x5 x13@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_255(x4)(x5)(x15)(x14)(x1)(st))(st)
c__case_256_case__891 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_256_case__891(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_256_case__891 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_256_case__891")(x)



c__case_257_case__892 x1 x4 x5 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_256(x4)(x5)(x13)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_258_case__893 x1 x4 x5 x11@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_257(x4)(x5)(x13)(x12)(x1)(st))(st)
c__case_258_case__893 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_258_case__893(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_258_case__893 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_258_case__893")(x)



c__case_259_case__894 x1 x4 x5 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_258(x4)(x5)(x11)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_260_case__895 x1 x4 x5 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_259(x4)(x5)(x11)(x10)(x1)(st))(st)
c__case_260_case__895 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_260_case__895(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_260_case__895 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_260_case__895")(x)



c__case_261_case__896 x1 x4 x5 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_260(x4)(x5)(x9)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_262_case__897 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_261(x4)(x5)(x9)(x8)(x1)(st))(st)
c__case_262_case__897 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_262_case__897(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_262_case__897 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_262_case__897")(x)



c__case_263_case__898 x1 x4 x5 x7 x6 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('b'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_262(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_264_case__899 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_263(x4)(x5)(x7)(x6)(x1)(st))(st)
c__case_264_case__899 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_264_case__899(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_264_case__899 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_264_case__899")(x)



c__case_265_case__900 x1 x2@(Curry.Module.XML.C_XElem x3 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_264(x4)(x5)(x3)(x1)(st))(st)
c__case_265_case__900 x1 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_265_case__900(x1)(x)(st))(i)(xs)(st)
c__case_265_case__900 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_265_case__900")(x)



c__case_157_case__901 x1 x5 x4@Curry.Module.Prelude.List st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.FlatCurry.C_Intc(Curry.Module.OracleRead.c_readNat(Curry.Module.OracleXML.c_textOfXml(x5)(x1)(st))(x6)(st)))(st)
c__case_157_case__901 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_157_case__901(x1)(x5)(x)(st))(i)(xs)(st)
c__case_157_case__901 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_157_case__901")(x)



c__case_158_case__902 x1 x4 x5 x13@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_157(x5)(x4)(x1)(st))(st)
c__case_158_case__902 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_158_case__902(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_158_case__902 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_158_case__902")(x)



c__case_159_case__903 x1 x4 x5 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_158(x4)(x5)(x13)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_160_case__904 x1 x4 x5 x11@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_159(x4)(x5)(x13)(x12)(x1)(st))(st)
c__case_160_case__904 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_160_case__904(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_160_case__904 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_160_case__904")(x)



c__case_161_case__905 x1 x4 x5 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_160(x4)(x5)(x11)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_162_case__906 x1 x4 x5 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_161(x4)(x5)(x11)(x10)(x1)(st))(st)
c__case_162_case__906 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_162_case__906(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_162_case__906 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_162_case__906")(x)



c__case_163_case__907 x1 x4 x5 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_162(x4)(x5)(x9)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_164_case__908 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_163(x4)(x5)(x9)(x8)(x1)(st))(st)
c__case_164_case__908 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_164_case__908(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_164_case__908 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_164_case__908")(x)



c__case_145_case__909 x1 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))(x1)(st))(st)
c__case_145_case__909 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_145_case__909(x1)(x)(st))(i)(xs)(st)
c__case_145_case__909 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_145_case__909")(x)



c__case_146_case__910 x1 x4 x23@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_145(x4)(x1)(st))(st)
c__case_146_case__910 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_146_case__910(x1)(x4)(x)(st))(i)(xs)(st)
c__case_146_case__910 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_146_case__910")(x)



c__case_147_case__911 x1 x4 x23 x22 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x22)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_146(x4)(x23)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_148_case__912 x1 x4 x21@((Curry.Module.Prelude.:<) x22 x23) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_147(x4)(x23)(x22)(x1)(st))(st)
c__case_148_case__912 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_148_case__912(x1)(x4)(x)(st))(i)(xs)(st)
c__case_148_case__912 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_148_case__912")(x)



c__case_149_case__913 x1 x4 x21 x20 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x20)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_148(x4)(x21)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_150_case__914 x1 x4 x19@((Curry.Module.Prelude.:<) x20 x21) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_149(x4)(x21)(x20)(x1)(st))(st)
c__case_150_case__914 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_150_case__914(x1)(x4)(x)(st))(i)(xs)(st)
c__case_150_case__914 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_150_case__914")(x)



c__case_151_case__915 x1 x4 x19 x18 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x18)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_150(x4)(x19)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_152_case__916 x1 x4 x17@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_151(x4)(x19)(x18)(x1)(st))(st)
c__case_152_case__916 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_152_case__916(x1)(x4)(x)(st))(i)(xs)(st)
c__case_152_case__916 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_152_case__916")(x)



c__case_153_case__917 x1 x4 x17 x16 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x16)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_152(x4)(x17)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_154_case__918 x1 x4 x15@((Curry.Module.Prelude.:<) x16 x17) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_153(x4)(x17)(x16)(x1)(st))(st)
c__case_154_case__918 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_154_case__918(x1)(x4)(x)(st))(i)(xs)(st)
c__case_154_case__918 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_154_case__918")(x)



c__case_155_case__919 x1 x4 x15 x14 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x14)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_154(x4)(x15)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_156_case__920 x1 x4 x7@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_155(x4)(x15)(x14)(x1)(st))(st)
c__case_156_case__920 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_156_case__920(x1)(x4)(x)(st))(i)(xs)(st)
c__case_156_case__920 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_156_case__920")(x)



c__case_135_case__921 x1 x5 x4@Curry.Module.Prelude.List st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.FlatCurry.C_Charc(Curry.Module.OraclePrelude.c_chr(Curry.Module.OracleRead.c_readNat(Curry.Module.OracleXML.c_textOfXml(x5)(x1)(st))(x6)(st))(x7)(st)))(st)
c__case_135_case__921 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_135_case__921(x1)(x5)(x)(st))(i)(xs)(st)
c__case_135_case__921 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_135_case__921")(x)



c__case_136_case__922 x1 x4 x5 x31@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_135(x5)(x4)(x1)(st))(st)
c__case_136_case__922 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_136_case__922(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_136_case__922 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_136_case__922")(x)



c__case_137_case__923 x1 x4 x5 x31 x30 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x30)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_136(x4)(x5)(x31)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_138_case__924 x1 x4 x5 x29@((Curry.Module.Prelude.:<) x30 x31) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_137(x4)(x5)(x31)(x30)(x1)(st))(st)
c__case_138_case__924 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_138_case__924(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_138_case__924 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_138_case__924")(x)



c__case_139_case__925 x1 x4 x5 x29 x28 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x28)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_138(x4)(x5)(x29)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_140_case__926 x1 x4 x5 x27@((Curry.Module.Prelude.:<) x28 x29) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_139(x4)(x5)(x29)(x28)(x1)(st))(st)
c__case_140_case__926 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_140_case__926(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_140_case__926 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_140_case__926")(x)



c__case_141_case__927 x1 x4 x5 x27 x26 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x26)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_140(x4)(x5)(x27)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_142_case__928 x1 x4 x5 x25@((Curry.Module.Prelude.:<) x26 x27) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_141(x4)(x5)(x27)(x26)(x1)(st))(st)
c__case_142_case__928 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_142_case__928(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_142_case__928 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_142_case__928")(x)



c__case_143_case__929 x1 x4 x5 x25 x24 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x24)(Curry.Module.Prelude.C_Char('h'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_142(x4)(x5)(x25)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_144_case__930 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x24 x25) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_143(x4)(x5)(x25)(x24)(x1)(st))(st)
c__case_144_case__930 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_144_case__930(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_144_case__930 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_144_case__930")(x)



c__case_165_case__931 x1 x4 x5 x7 x6 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_164(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('f'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_156(x4)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_144(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st))(st)



c__case_166_case__932 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_165(x4)(x5)(x7)(x6)(x1)(st))(st)
c__case_166_case__932 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_166_case__932(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_166_case__932 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_166_case__932")(x)



c__case_167_case__933 x1 x2@(Curry.Module.XML.C_XElem x3 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_166(x4)(x5)(x3)(x1)(st))(st)
c__case_167_case__933 x1 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_167_case__933(x1)(x)(st))(i)(xs)(st)
c__case_167_case__933 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_167_case__933")(x)



c__case_124_case__934 x1 x5 x4@Curry.Module.Prelude.List st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.FlatCurry.C_TVar(Curry.Module.OracleRead.c_readNat(Curry.Module.OracleXML.c_textOfXml(x5)(x1)(st))(x6)(st)))(st)
c__case_124_case__934 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_124_case__934(x1)(x5)(x)(st))(i)(xs)(st)
c__case_124_case__934 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_124_case__934")(x)



c__case_125_case__935 x1 x4 x5 x13@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_124(x5)(x4)(x1)(st))(st)
c__case_125_case__935 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_125_case__935(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_125_case__935 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_125_case__935")(x)



c__case_126_case__936 x1 x4 x5 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_125(x4)(x5)(x13)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_127_case__937 x1 x4 x5 x11@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_126(x4)(x5)(x13)(x12)(x1)(st))(st)
c__case_127_case__937 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_127_case__937(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_127_case__937 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_127_case__937")(x)



c__case_128_case__938 x1 x4 x5 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_127(x4)(x5)(x11)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_129_case__939 x1 x4 x5 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_128(x4)(x5)(x11)(x10)(x1)(st))(st)
c__case_129_case__939 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_129_case__939(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_129_case__939 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_129_case__939")(x)



c__case_90_case__940 x1 x5 x23 x39 x37@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.FlatCurry.C_TCons(Curry.Module.Prelude.T2(x23)(x39))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleFlatCurryXML.c_flatx2texp))))(x5)(x1)(st)))(st)
c__case_90_case__940 x1 x5 x23 x39 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_90_case__940(x1)(x5)(x23)(x39)(x)(st))(i)(xs)(st)
c__case_90_case__940 x1 x5 x23 x39 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_90_case__940")(x)



c__case_91_case__941 x1 x5 x23 x37 x39 x47@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_90(x5)(x23)(x39)(x37)(x1)(st))(st)
c__case_91_case__941 x1 x5 x23 x37 x39 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_91_case__941(x1)(x5)(x23)(x37)(x39)(x)(st))(i)(xs)(st)
c__case_91_case__941 x1 x5 x23 x37 x39 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_91_case__941")(x)



c__case_92_case__942 x1 x5 x23 x37 x39 x47 x46 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x46)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_91(x5)(x23)(x37)(x39)(x47)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_93_case__943 x1 x5 x23 x37 x39 x45@((Curry.Module.Prelude.:<) x46 x47) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_92(x5)(x23)(x37)(x39)(x47)(x46)(x1)(st))(st)
c__case_93_case__943 x1 x5 x23 x37 x39 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_93_case__943(x1)(x5)(x23)(x37)(x39)(x)(st))(i)(xs)(st)
c__case_93_case__943 x1 x5 x23 x37 x39 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_93_case__943")(x)



c__case_94_case__944 x1 x5 x23 x37 x39 x45 x44 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x44)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_93(x5)(x23)(x37)(x39)(x45)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_95_case__945 x1 x5 x23 x37 x39 x43@((Curry.Module.Prelude.:<) x44 x45) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_94(x5)(x23)(x37)(x39)(x45)(x44)(x1)(st))(st)
c__case_95_case__945 x1 x5 x23 x37 x39 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_95_case__945(x1)(x5)(x23)(x37)(x39)(x)(st))(i)(xs)(st)
c__case_95_case__945 x1 x5 x23 x37 x39 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_95_case__945")(x)



c__case_96_case__946 x1 x5 x23 x37 x39 x43 x42 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x42)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_95(x5)(x23)(x37)(x39)(x43)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_97_case__947 x1 x5 x23 x37 x39 x41@((Curry.Module.Prelude.:<) x42 x43) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_96(x5)(x23)(x37)(x39)(x43)(x42)(x1)(st))(st)
c__case_97_case__947 x1 x5 x23 x37 x39 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_97_case__947(x1)(x5)(x23)(x37)(x39)(x)(st))(i)(xs)(st)
c__case_97_case__947 x1 x5 x23 x37 x39 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_97_case__947")(x)



c__case_98_case__948 x1 x5 x23 x37 x39 x41 x40 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x40)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_97(x5)(x23)(x37)(x39)(x41)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_99_case__949 x1 x5 x23 x37 x39 x38@((Curry.Module.Prelude.:<) x40 x41) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_98(x5)(x23)(x37)(x39)(x41)(x40)(x1)(st))(st)
c__case_99_case__949 x1 x5 x23 x37 x39 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_99_case__949(x1)(x5)(x23)(x37)(x39)(x)(st))(i)(xs)(st)
c__case_99_case__949 x1 x5 x23 x37 x39 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_99_case__949")(x)



c__case_100_case__950 x1 x5 x23 x37 x36@(Curry.Module.Prelude.T2 x38 x39) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_99(x5)(x23)(x37)(x39)(x38)(x1)(st))(st)
c__case_100_case__950 x1 x5 x23 x37 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_100_case__950(x1)(x5)(x23)(x37)(x)(st))(i)(xs)(st)
c__case_100_case__950 x1 x5 x23 x37 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_100_case__950")(x)



c__case_101_case__951 x1 x5 x23 x21@((Curry.Module.Prelude.:<) x36 x37) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_100(x5)(x23)(x37)(x36)(x1)(st))(st)
c__case_101_case__951 x1 x5 x23 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_101_case__951(x1)(x5)(x23)(x)(st))(i)(xs)(st)
c__case_101_case__951 x1 x5 x23 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_101_case__951")(x)



c__case_102_case__952 x1 x5 x21 x23 x35@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_101(x5)(x23)(x21)(x1)(st))(st)
c__case_102_case__952 x1 x5 x21 x23 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_102_case__952(x1)(x5)(x21)(x23)(x)(st))(i)(xs)(st)
c__case_102_case__952 x1 x5 x21 x23 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_102_case__952")(x)



c__case_103_case__953 x1 x5 x21 x23 x35 x34 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x34)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_102(x5)(x21)(x23)(x35)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_104_case__954 x1 x5 x21 x23 x33@((Curry.Module.Prelude.:<) x34 x35) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_103(x5)(x21)(x23)(x35)(x34)(x1)(st))(st)
c__case_104_case__954 x1 x5 x21 x23 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_104_case__954(x1)(x5)(x21)(x23)(x)(st))(i)(xs)(st)
c__case_104_case__954 x1 x5 x21 x23 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_104_case__954")(x)



c__case_105_case__955 x1 x5 x21 x23 x33 x32 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x32)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_104(x5)(x21)(x23)(x33)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_106_case__956 x1 x5 x21 x23 x31@((Curry.Module.Prelude.:<) x32 x33) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_105(x5)(x21)(x23)(x33)(x32)(x1)(st))(st)
c__case_106_case__956 x1 x5 x21 x23 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_106_case__956(x1)(x5)(x21)(x23)(x)(st))(i)(xs)(st)
c__case_106_case__956 x1 x5 x21 x23 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_106_case__956")(x)



c__case_107_case__957 x1 x5 x21 x23 x31 x30 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x30)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_106(x5)(x21)(x23)(x31)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_108_case__958 x1 x5 x21 x23 x29@((Curry.Module.Prelude.:<) x30 x31) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_107(x5)(x21)(x23)(x31)(x30)(x1)(st))(st)
c__case_108_case__958 x1 x5 x21 x23 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_108_case__958(x1)(x5)(x21)(x23)(x)(st))(i)(xs)(st)
c__case_108_case__958 x1 x5 x21 x23 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_108_case__958")(x)



c__case_109_case__959 x1 x5 x21 x23 x29 x28 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x28)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_108(x5)(x21)(x23)(x29)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_110_case__960 x1 x5 x21 x23 x27@((Curry.Module.Prelude.:<) x28 x29) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_109(x5)(x21)(x23)(x29)(x28)(x1)(st))(st)
c__case_110_case__960 x1 x5 x21 x23 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_110_case__960(x1)(x5)(x21)(x23)(x)(st))(i)(xs)(st)
c__case_110_case__960 x1 x5 x21 x23 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_110_case__960")(x)



c__case_111_case__961 x1 x5 x21 x23 x27 x26 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x26)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_110(x5)(x21)(x23)(x27)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_112_case__962 x1 x5 x21 x23 x25@((Curry.Module.Prelude.:<) x26 x27) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_111(x5)(x21)(x23)(x27)(x26)(x1)(st))(st)
c__case_112_case__962 x1 x5 x21 x23 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_112_case__962(x1)(x5)(x21)(x23)(x)(st))(i)(xs)(st)
c__case_112_case__962 x1 x5 x21 x23 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_112_case__962")(x)



c__case_113_case__963 x1 x5 x21 x23 x25 x24 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x24)(Curry.Module.Prelude.C_Char('m'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_112(x5)(x21)(x23)(x25)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_114_case__964 x1 x5 x21 x23 x22@((Curry.Module.Prelude.:<) x24 x25) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_113(x5)(x21)(x23)(x25)(x24)(x1)(st))(st)
c__case_114_case__964 x1 x5 x21 x23 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_114_case__964(x1)(x5)(x21)(x23)(x)(st))(i)(xs)(st)
c__case_114_case__964 x1 x5 x21 x23 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_114_case__964")(x)



c__case_115_case__965 x1 x5 x21 x20@(Curry.Module.Prelude.T2 x22 x23) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_114(x5)(x21)(x23)(x22)(x1)(st))(st)
c__case_115_case__965 x1 x5 x21 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_115_case__965(x1)(x5)(x21)(x)(st))(i)(xs)(st)
c__case_115_case__965 x1 x5 x21 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_115_case__965")(x)



c__case_116_case__966 x1 x5 x4@((Curry.Module.Prelude.:<) x20 x21) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_115(x5)(x21)(x20)(x1)(st))(st)
c__case_116_case__966 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_116_case__966(x1)(x5)(x)(st))(i)(xs)(st)
c__case_116_case__966 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_116_case__966")(x)



c__case_117_case__967 x1 x4 x5 x19@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_116(x5)(x4)(x1)(st))(st)
c__case_117_case__967 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_117_case__967(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_117_case__967 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_117_case__967")(x)



c__case_118_case__968 x1 x4 x5 x19 x18 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x18)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_117(x4)(x5)(x19)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_119_case__969 x1 x4 x5 x17@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_118(x4)(x5)(x19)(x18)(x1)(st))(st)
c__case_119_case__969 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_119_case__969(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_119_case__969 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_119_case__969")(x)



c__case_120_case__970 x1 x4 x5 x17 x16 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x16)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_119(x4)(x5)(x17)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_121_case__971 x1 x4 x5 x15@((Curry.Module.Prelude.:<) x16 x17) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_120(x4)(x5)(x17)(x16)(x1)(st))(st)
c__case_121_case__971 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_121_case__971(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_121_case__971 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_121_case__971")(x)



c__case_122_case__972 x1 x4 x5 x15 x14 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x14)(Curry.Module.Prelude.C_Char('o'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_121(x4)(x5)(x15)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_123_case__973 x1 x4 x5 x9@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_122(x4)(x5)(x15)(x14)(x1)(st))(st)
c__case_123_case__973 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_123_case__973(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_123_case__973 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_123_case__973")(x)



c__case_130_case__974 x1 x4 x5 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('v'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_129(x4)(x5)(x9)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_123(x4)(x5)(x9)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st)



c__case_131_case__975 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_130(x4)(x5)(x9)(x8)(x1)(st))(st)
c__case_131_case__975 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_131_case__975(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_131_case__975 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_131_case__975")(x)



c__case_71_case__976 x1 x62 x64 x65@Curry.Module.Prelude.List st = let {x66 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x66)(Curry.Module.Prelude.List))(Curry.Module.FlatCurry.C_FuncType(Curry.Module.OracleFlatCurryXML.c_flatx2texp(x62)(x1)(st))(Curry.Module.OracleFlatCurryXML.c_flatx2texp(x64)(x66)(st)))(st)
c__case_71_case__976 x1 x62 x64 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_71_case__976(x1)(x62)(x64)(x)(st))(i)(xs)(st)
c__case_71_case__976 x1 x62 x64 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_71_case__976")(x)



c__case_72_case__977 x1 x62 x63@((Curry.Module.Prelude.:<) x64 x65) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_71(x62)(x64)(x65)(x1)(st))(st)
c__case_72_case__977 x1 x62 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_72_case__977(x1)(x62)(x)(st))(i)(xs)(st)
c__case_72_case__977 x1 x62 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_72_case__977")(x)



c__case_73_case__978 x1 x5@((Curry.Module.Prelude.:<) x62 x63) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_72(x62)(x63)(x1)(st))(st)
c__case_73_case__978 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_73_case__978(x1)(x)(st))(i)(xs)(st)
c__case_73_case__978 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_73_case__978")(x)



c__case_74_case__979 x1 x5 x4@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_73(x5)(x1)(st))(st)
c__case_74_case__979 x1 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_74_case__979(x1)(x5)(x)(st))(i)(xs)(st)
c__case_74_case__979 x1 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_74_case__979")(x)



c__case_75_case__980 x1 x4 x5 x61@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_74(x5)(x4)(x1)(st))(st)
c__case_75_case__980 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_75_case__980(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_75_case__980 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_75_case__980")(x)



c__case_76_case__981 x1 x4 x5 x61 x60 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x60)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_75(x4)(x5)(x61)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_77_case__982 x1 x4 x5 x59@((Curry.Module.Prelude.:<) x60 x61) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_76(x4)(x5)(x61)(x60)(x1)(st))(st)
c__case_77_case__982 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_77_case__982(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_77_case__982 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_77_case__982")(x)



c__case_78_case__983 x1 x4 x5 x59 x58 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x58)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_77(x4)(x5)(x59)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_79_case__984 x1 x4 x5 x57@((Curry.Module.Prelude.:<) x58 x59) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_78(x4)(x5)(x59)(x58)(x1)(st))(st)
c__case_79_case__984 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_79_case__984(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_79_case__984 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_79_case__984")(x)



c__case_80_case__985 x1 x4 x5 x57 x56 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x56)(Curry.Module.Prelude.C_Char('y'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_79(x4)(x5)(x57)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_81_case__986 x1 x4 x5 x55@((Curry.Module.Prelude.:<) x56 x57) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_80(x4)(x5)(x57)(x56)(x1)(st))(st)
c__case_81_case__986 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_81_case__986(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_81_case__986 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_81_case__986")(x)



c__case_82_case__987 x1 x4 x5 x55 x54 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x54)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_81(x4)(x5)(x55)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_83_case__988 x1 x4 x5 x53@((Curry.Module.Prelude.:<) x54 x55) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_82(x4)(x5)(x55)(x54)(x1)(st))(st)
c__case_83_case__988 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_83_case__988(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_83_case__988 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_83_case__988")(x)



c__case_84_case__989 x1 x4 x5 x53 x52 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x52)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_83(x4)(x5)(x53)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_85_case__990 x1 x4 x5 x51@((Curry.Module.Prelude.:<) x52 x53) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_84(x4)(x5)(x53)(x52)(x1)(st))(st)
c__case_85_case__990 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_85_case__990(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_85_case__990 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_85_case__990")(x)



c__case_86_case__991 x1 x4 x5 x51 x50 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x50)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_85(x4)(x5)(x51)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_87_case__992 x1 x4 x5 x49@((Curry.Module.Prelude.:<) x50 x51) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_86(x4)(x5)(x51)(x50)(x1)(st))(st)
c__case_87_case__992 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_87_case__992(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_87_case__992 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_87_case__992")(x)



c__case_88_case__993 x1 x4 x5 x49 x48 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x48)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_87(x4)(x5)(x49)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_89_case__994 x1 x4 x5 x7@((Curry.Module.Prelude.:<) x48 x49) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_88(x4)(x5)(x49)(x48)(x1)(st))(st)
c__case_89_case__994 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_89_case__994(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_89_case__994 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_89_case__994")(x)



c__case_132_case__995 x1 x4 x5 x7 x6 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_131(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('f'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_89(x4)(x5)(x7)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st)



c__case_133_case__996 x1 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_132(x4)(x5)(x7)(x6)(x1)(st))(st)
c__case_133_case__996 x1 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_133_case__996(x1)(x4)(x5)(x)(st))(i)(xs)(st)
c__case_133_case__996 x1 x4 x5 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_133_case__996")(x)



c__case_134_case__997 x1 x2@(Curry.Module.XML.C_XElem x3 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_133(x4)(x5)(x3)(x1)(st))(st)
c__case_134_case__997 x1 (Curry.Module.XML.C_XmlExpOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_134_case__997(x1)(x)(st))(i)(xs)(st)
c__case_134_case__997 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_134_case__997")(x)



c__case_36_case__998 x1 x36@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlatCurry.C_Public)(st)
c__case_36_case__998 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_36_case__998(x1)(x)(st))(i)(xs)(st)
c__case_36_case__998 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_36_case__998")(x)



c__case_37_case__999 x1 x36 x35 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x35)(Curry.Module.Prelude.C_Char('c'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_36(x36)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_38_case__1000 x1 x34@((Curry.Module.Prelude.:<) x35 x36) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_37(x36)(x35)(x1)(st))(st)
c__case_38_case__1000 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_38_case__1000(x1)(x)(st))(i)(xs)(st)
c__case_38_case__1000 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_38_case__1000")(x)



c__case_39_case__1001 x1 x34 x33 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x33)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_38(x34)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_40_case__1002 x1 x32@((Curry.Module.Prelude.:<) x33 x34) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_39(x34)(x33)(x1)(st))(st)
c__case_40_case__1002 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_40_case__1002(x1)(x)(st))(i)(xs)(st)
c__case_40_case__1002 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_40_case__1002")(x)



c__case_41_case__1003 x1 x32 x31 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x31)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_40(x32)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_42_case__1004 x1 x30@((Curry.Module.Prelude.:<) x31 x32) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_41(x32)(x31)(x1)(st))(st)
c__case_42_case__1004 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_42_case__1004(x1)(x)(st))(i)(xs)(st)
c__case_42_case__1004 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_42_case__1004")(x)



c__case_43_case__1005 x1 x30 x29 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x29)(Curry.Module.Prelude.C_Char('b'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_42(x30)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_44_case__1006 x1 x28@((Curry.Module.Prelude.:<) x29 x30) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_43(x30)(x29)(x1)(st))(st)
c__case_44_case__1006 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_44_case__1006(x1)(x)(st))(i)(xs)(st)
c__case_44_case__1006 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_44_case__1006")(x)



c__case_25_case__1007 x1 x46@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlatCurry.C_Private)(st)
c__case_25_case__1007 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_25_case__1007(x1)(x)(st))(i)(xs)(st)
c__case_25_case__1007 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_25_case__1007")(x)



c__case_26_case__1008 x1 x46 x45 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x45)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_25(x46)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_27_case__1009 x1 x44@((Curry.Module.Prelude.:<) x45 x46) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_26(x46)(x45)(x1)(st))(st)
c__case_27_case__1009 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_27_case__1009(x1)(x)(st))(i)(xs)(st)
c__case_27_case__1009 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_27_case__1009")(x)



c__case_28_case__1010 x1 x44 x43 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x43)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_27(x44)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_29_case__1011 x1 x42@((Curry.Module.Prelude.:<) x43 x44) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_28(x44)(x43)(x1)(st))(st)
c__case_29_case__1011 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_29_case__1011(x1)(x)(st))(i)(xs)(st)
c__case_29_case__1011 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_29_case__1011")(x)



c__case_30_case__1012 x1 x42 x41 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x41)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_29(x42)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_31_case__1013 x1 x40@((Curry.Module.Prelude.:<) x41 x42) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_30(x42)(x41)(x1)(st))(st)
c__case_31_case__1013 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_31_case__1013(x1)(x)(st))(i)(xs)(st)
c__case_31_case__1013 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_31_case__1013")(x)



c__case_32_case__1014 x1 x40 x39 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x39)(Curry.Module.Prelude.C_Char('v'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_31(x40)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_33_case__1015 x1 x38@((Curry.Module.Prelude.:<) x39 x40) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_32(x40)(x39)(x1)(st))(st)
c__case_33_case__1015 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_33_case__1015(x1)(x)(st))(i)(xs)(st)
c__case_33_case__1015 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_33_case__1015")(x)



c__case_34_case__1016 x1 x38 x37 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x37)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_33(x38)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_35_case__1017 x1 x28@((Curry.Module.Prelude.:<) x37 x38) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_34(x38)(x37)(x1)(st))(st)
c__case_35_case__1017 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_35_case__1017(x1)(x)(st))(i)(xs)(st)
c__case_35_case__1017 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_35_case__1017")(x)



c__case_45_case__1018 x1 x28 x27 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x27)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_44(x28)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x27)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_35(x28)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st)



c__case_46_case__1019 x1 x26@((Curry.Module.Prelude.:<) x27 x28) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_45(x28)(x27)(x1)(st))(st)
c__case_46_case__1019 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_46_case__1019(x1)(x)(st))(i)(xs)(st)
c__case_46_case__1019 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_46_case__1019")(x)



c__case_47_case__1020 x1 x26 x25 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x25)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_46(x26)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_48_case__1021 x1 x4@((Curry.Module.Prelude.:<) x25 x26) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_47(x26)(x25)(x1)(st))(st)
c__case_48_case__1021 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_48_case__1021(x1)(x)(st))(i)(xs)(st)
c__case_48_case__1021 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_48_case__1021")(x)



c__case_49_case__1022 x1 x4 x24@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_48(x4)(x1)(st))(st)
c__case_49_case__1022 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_49_case__1022(x1)(x4)(x)(st))(i)(xs)(st)
c__case_49_case__1022 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_49_case__1022")(x)



c__case_50_case__1023 x1 x4 x24 x23 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x23)(Curry.Module.Prelude.C_Char('y'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_49(x4)(x24)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_51_case__1024 x1 x4 x22@((Curry.Module.Prelude.:<) x23 x24) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_50(x4)(x24)(x23)(x1)(st))(st)
c__case_51_case__1024 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_51_case__1024(x1)(x4)(x)(st))(i)(xs)(st)
c__case_51_case__1024 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_51_case__1024")(x)



c__case_52_case__1025 x1 x4 x22 x21 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x21)(Curry.Module.Prelude.C_Char('t'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_51(x4)(x22)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_53_case__1026 x1 x4 x20@((Curry.Module.Prelude.:<) x21 x22) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_52(x4)(x22)(x21)(x1)(st))(st)
c__case_53_case__1026 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_53_case__1026(x1)(x4)(x)(st))(i)(xs)(st)
c__case_53_case__1026 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_53_case__1026")(x)



c__case_54_case__1027 x1 x4 x20 x19 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x19)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_53(x4)(x20)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_55_case__1028 x1 x4 x18@((Curry.Module.Prelude.:<) x19 x20) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_54(x4)(x20)(x19)(x1)(st))(st)
c__case_55_case__1028 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_55_case__1028(x1)(x4)(x)(st))(i)(xs)(st)
c__case_55_case__1028 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_55_case__1028")(x)



c__case_56_case__1029 x1 x4 x18 x17 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x17)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_55(x4)(x18)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_57_case__1030 x1 x4 x16@((Curry.Module.Prelude.:<) x17 x18) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_56(x4)(x18)(x17)(x1)(st))(st)
c__case_57_case__1030 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_57_case__1030(x1)(x4)(x)(st))(i)(xs)(st)
c__case_57_case__1030 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_57_case__1030")(x)



c__case_58_case__1031 x1 x4 x16 x15 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x15)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_57(x4)(x16)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_59_case__1032 x1 x4 x14@((Curry.Module.Prelude.:<) x15 x16) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_58(x4)(x16)(x15)(x1)(st))(st)
c__case_59_case__1032 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_59_case__1032(x1)(x4)(x)(st))(i)(xs)(st)
c__case_59_case__1032 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_59_case__1032")(x)



c__case_60_case__1033 x1 x4 x14 x13 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x13)(Curry.Module.Prelude.C_Char('b'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_59(x4)(x14)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_61_case__1034 x1 x4 x12@((Curry.Module.Prelude.:<) x13 x14) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_60(x4)(x14)(x13)(x1)(st))(st)
c__case_61_case__1034 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_61_case__1034(x1)(x4)(x)(st))(i)(xs)(st)
c__case_61_case__1034 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_61_case__1034")(x)



c__case_62_case__1035 x1 x4 x12 x11 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x11)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_61(x4)(x12)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_63_case__1036 x1 x4 x10@((Curry.Module.Prelude.:<) x11 x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_62(x4)(x12)(x11)(x1)(st))(st)
c__case_63_case__1036 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_63_case__1036(x1)(x4)(x)(st))(i)(xs)(st)
c__case_63_case__1036 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_63_case__1036")(x)



c__case_64_case__1037 x1 x4 x10 x9 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x9)(Curry.Module.Prelude.C_Char('s'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_63(x4)(x10)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_65_case__1038 x1 x4 x8@((Curry.Module.Prelude.:<) x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_64(x4)(x10)(x9)(x1)(st))(st)
c__case_65_case__1038 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_65_case__1038(x1)(x4)(x)(st))(i)(xs)(st)
c__case_65_case__1038 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_65_case__1038")(x)



c__case_66_case__1039 x1 x4 x8 x7 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x7)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_65(x4)(x8)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_67_case__1040 x1 x4 x6@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_66(x4)(x8)(x7)(x1)(st))(st)
c__case_67_case__1040 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_67_case__1040(x1)(x4)(x)(st))(i)(xs)(st)
c__case_67_case__1040 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_67_case__1040")(x)



c__case_68_case__1041 x1 x4 x6 x5 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x5)(Curry.Module.Prelude.C_Char('v'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_67(x4)(x6)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_69_case__1042 x1 x4 x3@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_68(x4)(x6)(x5)(x1)(st))(st)
c__case_69_case__1042 x1 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_69_case__1042(x1)(x4)(x)(st))(i)(xs)(st)
c__case_69_case__1042 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_69_case__1042")(x)



c__case_70_case__1043 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_69(x4)(x3)(x1)(st))(st)
c__case_70_case__1043 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_70_case__1043(x1)(x)(st))(i)(xs)(st)
c__case_70_case__1043 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_70_case__1043")(x)



c__case_10_case__1044 x1 x16@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlatCurry.C_InfixOp)(st)
c__case_10_case__1044 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_10_case__1044(x1)(x)(st))(i)(xs)(st)
c__case_10_case__1044 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_10_case__1044")(x)



c__case_11_case__1045 x1 x16 x15 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x15)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_10(x16)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_12_case__1046 x1 x14@((Curry.Module.Prelude.:<) x15 x16) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_11(x16)(x15)(x1)(st))(st)
c__case_12_case__1046 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_12_case__1046(x1)(x)(st))(i)(xs)(st)
c__case_12_case__1046 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_12_case__1046")(x)



c__case_5_case__1047 x1 x20@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlatCurry.C_InfixlOp)(st)
c__case_5_case__1047 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_5_case__1047(x1)(x)(st))(i)(xs)(st)
c__case_5_case__1047 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_5_case__1047")(x)



c__case_6_case__1048 x1 x20 x19 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x19)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_5(x20)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_7_case__1049 x1 x18@((Curry.Module.Prelude.:<) x19 x20) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_6(x20)(x19)(x1)(st))(st)
c__case_7_case__1049 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_7_case__1049(x1)(x)(st))(i)(xs)(st)
c__case_7_case__1049 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_7_case__1049")(x)



c__case_8_case__1050 x1 x18 x17 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x17)(Curry.Module.Prelude.C_Char('O'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_7(x18)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_9_case__1051 x1 x14@((Curry.Module.Prelude.:<) x17 x18) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_8(x18)(x17)(x1)(st))(st)
c__case_9_case__1051 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_9_case__1051(x1)(x)(st))(i)(xs)(st)
c__case_9_case__1051 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_9_case__1051")(x)



c__case_0_case__1052 x1 x24@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.FlatCurry.C_InfixrOp)(st)
c__case_0_case__1052 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_0_case__1052(x1)(x)(st))(i)(xs)(st)
c__case_0_case__1052 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_0_case__1052")(x)



c__case_1_case__1053 x1 x24 x23 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x23)(Curry.Module.Prelude.C_Char('p'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_0(x24)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_2_case__1054 x1 x22@((Curry.Module.Prelude.:<) x23 x24) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_1(x24)(x23)(x1)(st))(st)
c__case_2_case__1054 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_2_case__1054(x1)(x)(st))(i)(xs)(st)
c__case_2_case__1054 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_2_case__1054")(x)



c__case_3_case__1055 x1 x22 x21 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x21)(Curry.Module.Prelude.C_Char('O'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_2(x22)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_4_case__1056 x1 x14@((Curry.Module.Prelude.:<) x21 x22) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_3(x22)(x21)(x1)(st))(st)
c__case_4_case__1056 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_4_case__1056(x1)(x)(st))(i)(xs)(st)
c__case_4_case__1056 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_4_case__1056")(x)



c__case_13_case__1057 x1 x14 x13 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x13)(Curry.Module.Prelude.C_Char('O'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_12(x14)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x13)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_9(x14)(x1)(st))(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x13)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_4(x14)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st))(st))(st)



c__case_14_case__1058 x1 x12@((Curry.Module.Prelude.:<) x13 x14) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_13(x14)(x13)(x1)(st))(st)
c__case_14_case__1058 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_14_case__1058(x1)(x)(st))(i)(xs)(st)
c__case_14_case__1058 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_14_case__1058")(x)



c__case_15_case__1059 x1 x12 x11 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x11)(Curry.Module.Prelude.C_Char('x'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_14(x12)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_16_case__1060 x1 x10@((Curry.Module.Prelude.:<) x11 x12) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_15(x12)(x11)(x1)(st))(st)
c__case_16_case__1060 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_16_case__1060(x1)(x)(st))(i)(xs)(st)
c__case_16_case__1060 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_16_case__1060")(x)



c__case_17_case__1061 x1 x10 x9 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x9)(Curry.Module.Prelude.C_Char('i'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_16(x10)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_18_case__1062 x1 x8@((Curry.Module.Prelude.:<) x9 x10) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_17(x10)(x9)(x1)(st))(st)
c__case_18_case__1062 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_18_case__1062(x1)(x)(st))(i)(xs)(st)
c__case_18_case__1062 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_18_case__1062")(x)



c__case_19_case__1063 x1 x8 x7 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x7)(Curry.Module.Prelude.C_Char('f'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_18(x8)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_20_case__1064 x1 x6@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_19(x8)(x7)(x1)(st))(st)
c__case_20_case__1064 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_20_case__1064(x1)(x)(st))(i)(xs)(st)
c__case_20_case__1064 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_20_case__1064")(x)



c__case_21_case__1065 x1 x6 x5 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x5)(Curry.Module.Prelude.C_Char('n'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_20(x6)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_22_case__1066 x1 x4@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_21(x6)(x5)(x1)(st))(st)
c__case_22_case__1066 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_22_case__1066(x1)(x)(st))(i)(xs)(st)
c__case_22_case__1066 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_22_case__1066")(x)



c__case_23_case__1067 x1 x4 x3 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x3)(Curry.Module.Prelude.C_Char('I'))(st))(Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_22(x4)(x1)(st))(st))(Curry.Module.Prelude.c_failed(st))(st)



c__case_24_case__1068 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFlatCurryXML.c__case_23(x4)(x3)(x1)(st))(st)
c__case_24_case__1068 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleFlatCurryXML.c__case_24_case__1068(x1)(x)(st))(i)(xs)(st)
c__case_24_case__1068 x1 x st = Curry.RunTimeSystem.patternFail("OracleFlatCurryXML._case_24_case__1068")(x)


