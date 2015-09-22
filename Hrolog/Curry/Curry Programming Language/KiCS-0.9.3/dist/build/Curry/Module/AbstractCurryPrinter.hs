{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.AbstractCurryPrinter (module Curry.Module.AbstractCurryPrinter) where

import Curry.RunTimeSystem
import Curry.Module.AbstractCurry
import Curry.Module.Char
import Curry.Module.FiniteMap
import Curry.Module.List
import Curry.Module.Maybe
import Curry.Module.Prelude
import Curry.Module.Sort
import Curry.Module.Read



-- begin included



-- end included

type C_NameFM = Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0

type C_Options = Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)

c_showProg :: Curry.Module.AbstractCurry.C_CurryProg -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showProg x1@(Curry.Module.AbstractCurry.C_CurryProg x2 x3 x4 x5 x6) st = let {x7 = Curry.Module.AbstractCurryPrinter.c_showExports(x4)(x5)(st)} in Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))(Curry.Module.Prelude.op_43_43(x2)(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showProg_case_229(x7)(Curry.Module.Prelude.op_61_61(x7)(Curry.Module.Prelude.List)(st))(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))))))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showImports(x3)(st))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showOpDecls(x6)(st))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showTypeDecls(x4)(st))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_prefixInter(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showFuncDeclOpt(Curry.Module.Prelude.T2(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_nameFM(st))(x5)(st))(x2))))(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(st))(st))(st))(st))(st))(st))(st))(st)
c_showProg (Curry.Module.AbstractCurry.C_CurryProgOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showProg(x)(st))(i)(xs)(st)
c_showProg x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showProg")(x)



c_defaultOptions :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_defaultOptions st = Curry.Module.Prelude.T2(Curry.Module.FiniteMap.c_emptyFM(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.AbstractCurryPrinter.c_lessString))(st))(Curry.Module.Prelude.List)



c_showExports :: (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeDecl) -> (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CFuncDecl) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showExports x1 x2 st = let {x4 = Curry.Module.List.c_partition(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showExports'46allPublicCons'469))(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showExports'46isPublicType'469))(x1)(st))(st)} in Curry.Module.Prelude.c_concat(Curry.Module.List.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_map(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_43_43))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List)))))))(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showExports'46getTypeName'469))(st))(Curry.Module.AbstractCurryPrinter.c_showExports'46_'35selFP3'35withCons(x4)(st))(st))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showExports'46getTypeName'469))(Curry.Module.AbstractCurryPrinter.c_showExports'46_'35selFP4'35withoutCons(x4)(st))(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showExports'46getFuncName'469))(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showExports'46isPublicFunc'469))(x2)(st))(st))(st))(st))(st))(st)



c_showExports'46isPublicType'469 :: Curry.Module.AbstractCurry.C_CTypeDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_showExports'46isPublicType'469 x1@(Curry.Module.AbstractCurry.C_CType x2 x3 x4 x5) st = Curry.Module.Prelude.op_61_61(x3)(Curry.Module.AbstractCurry.C_Public)(st)
c_showExports'46isPublicType'469 x1@(Curry.Module.AbstractCurry.C_CTypeSyn x6 x7 x8 x9) st = Curry.Module.Prelude.op_61_61(x7)(Curry.Module.AbstractCurry.C_Public)(st)
c_showExports'46isPublicType'469 (Curry.Module.AbstractCurry.C_CTypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showExports'46isPublicType'469(x)(st))(i)(xs)(st)
c_showExports'46isPublicType'469 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showExports.isPublicType.9")(x)



c_showExports'46isPublicFunc'469 :: Curry.Module.AbstractCurry.C_CFuncDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_showExports'46isPublicFunc'469 x1@(Curry.Module.AbstractCurry.C_CFunc x2 x3 x4 x5 x6) st = Curry.Module.Prelude.op_61_61(x4)(Curry.Module.AbstractCurry.C_Public)(st)
c_showExports'46isPublicFunc'469 x1@(Curry.Module.AbstractCurry.C_CmtFunc x7 x8 x9 x10 x11 x12) st = Curry.Module.Prelude.op_61_61(x10)(Curry.Module.AbstractCurry.C_Public)(st)
c_showExports'46isPublicFunc'469 (Curry.Module.AbstractCurry.C_CFuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showExports'46isPublicFunc'469(x)(st))(i)(xs)(st)
c_showExports'46isPublicFunc'469 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showExports.isPublicFunc.9")(x)



c_showExports'46getTypeName'469 :: Curry.Module.AbstractCurry.C_CTypeDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showExports'46getTypeName'469 x1@(Curry.Module.AbstractCurry.C_CType x2 x3 x4 x5) st = Curry.Module.AbstractCurryPrinter.c_showExports'46getTypeName'469_case_228(x2)(st)
c_showExports'46getTypeName'469 x1@(Curry.Module.AbstractCurry.C_CTypeSyn x8 x9 x10 x11) st = Curry.Module.AbstractCurryPrinter.c_showExports'46getTypeName'469_case_227(x8)(st)
c_showExports'46getTypeName'469 (Curry.Module.AbstractCurry.C_CTypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showExports'46getTypeName'469(x)(st))(i)(xs)(st)
c_showExports'46getTypeName'469 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showExports.getTypeName.9")(x)



c_showExports'46allPublicCons'469 :: Curry.Module.AbstractCurry.C_CTypeDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_showExports'46allPublicCons'469 x1@(Curry.Module.AbstractCurry.C_CType x2 x3 x4 x5) st = Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_length(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showExports'46allPublicCons'469'46isPublicCons'4649))(x5)(st))(st))(Curry.Module.Prelude.c_length(x5)(st))(st)
c_showExports'46allPublicCons'469 (Curry.Module.AbstractCurry.C_CTypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showExports'46allPublicCons'469(x)(st))(i)(xs)(st)
c_showExports'46allPublicCons'469 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showExports.allPublicCons.9")(x)



c_showExports'46allPublicCons'469'46isPublicCons'4649 :: Curry.Module.AbstractCurry.C_CConsDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_showExports'46allPublicCons'469'46isPublicCons'4649 x1@(Curry.Module.AbstractCurry.C_CCons x2 x3 x4 x5) st = Curry.Module.Prelude.op_61_61(x4)(Curry.Module.AbstractCurry.C_Public)(st)
c_showExports'46allPublicCons'469'46isPublicCons'4649 (Curry.Module.AbstractCurry.C_CConsDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showExports'46allPublicCons'469'46isPublicCons'4649(x)(st))(i)(xs)(st)
c_showExports'46allPublicCons'469'46isPublicCons'4649 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showExports.allPublicCons.9.isPublicCons.49")(x)



c_showExports'46getFuncName'469 :: Curry.Module.AbstractCurry.C_CFuncDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showExports'46getFuncName'469 x1@(Curry.Module.AbstractCurry.C_CFunc x2 x3 x4 x5 x6) st = Curry.Module.AbstractCurryPrinter.c_showExports'46getFuncName'469_case_226(x2)(st)
c_showExports'46getFuncName'469 x1@(Curry.Module.AbstractCurry.C_CmtFunc x9 x10 x11 x12 x13 x14) st = Curry.Module.AbstractCurryPrinter.c_showExports'46getFuncName'469_case_225(x10)(st)
c_showExports'46getFuncName'469 (Curry.Module.AbstractCurry.C_CFuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showExports'46getFuncName'469(x)(st))(i)(xs)(st)
c_showExports'46getFuncName'469 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showExports.getFuncName.9")(x)



c_showExports'46_'35selFP3'35withCons :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeDecl) (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeDecl)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeDecl
c_showExports'46_'35selFP3'35withCons x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_showExports'46_'35selFP3'35withCons (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showExports'46_'35selFP3'35withCons(x)(st))(i)(xs)(st)
c_showExports'46_'35selFP3'35withCons x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showExports._#selFP3#withCons")(x)



c_showExports'46_'35selFP4'35withoutCons :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeDecl) (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeDecl)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeDecl
c_showExports'46_'35selFP4'35withoutCons x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_showExports'46_'35selFP4'35withoutCons (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showExports'46_'35selFP4'35withoutCons(x)(st))(i)(xs)(st)
c_showExports'46_'35selFP4'35withoutCons x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showExports._#selFP4#withoutCons")(x)



c_showImports :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showImports x1 st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_prefixInter(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showImport))(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_47_61))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))))(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(st))(Curry.Module.AbstractCurryPrinter.c_showImports_case_224(x1)(Curry.Module.Prelude.op_61_61(x1)((Curry.Module.Prelude.:<)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(Curry.Module.Prelude.List))(st))(st))(st)



c_showImport :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showImport x1 st = Curry.Module.AbstractCurryPrinter.c_showImport_case_223(x1)(Curry.Module.Prelude.op_47_61(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(st))(st)



c_showOpDecls :: (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_COpDecl) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showOpDecls x1 st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_prefixInter(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showOpDecl))(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(st))(Curry.Module.AbstractCurryPrinter.c_showOpDecls_case_222(x1)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.List)(st))(st))(st)



c_showOpDecl :: Curry.Module.AbstractCurry.C_COpDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showOpDecl x1@(Curry.Module.AbstractCurry.C_COp x2 x3 x4) st = Curry.Module.AbstractCurryPrinter.c_showOpDecl_case_221(x3)(x4)(x2)(st)
c_showOpDecl (Curry.Module.AbstractCurry.C_COpDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showOpDecl(x)(st))(i)(xs)(st)
c_showOpDecl x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showOpDecl")(x)



c_showFixity :: Curry.Module.AbstractCurry.C_CFixity -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFixity x1@Curry.Module.AbstractCurry.C_CInfixOp st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))(Curry.Module.Prelude.List)))))
c_showFixity x1@Curry.Module.AbstractCurry.C_CInfixlOp st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List))))))
c_showFixity x1@Curry.Module.AbstractCurry.C_CInfixrOp st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))))
c_showFixity (Curry.Module.AbstractCurry.C_CFixityOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showFixity(x)(st))(i)(xs)(st)
c_showFixity x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showFixity")(x)



c_showTypeDecls :: (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeDecl) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showTypeDecls x1 st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_prefixInter(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showTypeDecl))(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))(st))(Curry.Module.AbstractCurryPrinter.c_showTypeDecls_case_219(x1)(Curry.Module.Prelude.op_61_61(x1)(Curry.Module.Prelude.List)(st))(st))(st)



c_showTypeDecl :: Curry.Module.AbstractCurry.C_CTypeDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showTypeDecl x1@(Curry.Module.AbstractCurry.C_CTypeSyn x2 x3 x4 x5) st = Curry.Module.AbstractCurryPrinter.c_showTypeDecl_case_218(x4)(x5)(x2)(st)
c_showTypeDecl x1@(Curry.Module.AbstractCurry.C_CType x8 x9 x10 x11) st = Curry.Module.AbstractCurryPrinter.c_showTypeDecl_case_217(x10)(x11)(x8)(st)
c_showTypeDecl (Curry.Module.AbstractCurry.C_CTypeDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showTypeDecl(x)(st))(i)(xs)(st)
c_showTypeDecl x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showTypeDecl")(x)



c_showConsDecl :: Curry.Module.AbstractCurry.C_CConsDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showConsDecl x1@(Curry.Module.AbstractCurry.C_CCons x2 x3 x4 x5) st = Curry.Module.AbstractCurryPrinter.c_showConsDecl_case_216(x5)(x2)(st)
c_showConsDecl (Curry.Module.AbstractCurry.C_CConsDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showConsDecl(x)(st))(i)(xs)(st)
c_showConsDecl x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showConsDecl")(x)



c_showTypeExpr :: Curry.Module.Prelude.C_Bool -> Curry.Module.AbstractCurry.C_CTypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showTypeExpr x1 x2@(Curry.Module.AbstractCurry.C_CTVar x3) st = Curry.Module.AbstractCurryPrinter.c_showTypeExpr_case_215(x3)(st)
c_showTypeExpr x1 x2@(Curry.Module.AbstractCurry.C_CFuncType x6 x7) st = Curry.Module.AbstractCurryPrinter.c_maybeShowBrackets(x1)(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showTypeExpr(Curry.Module.AbstractCurryPrinter.c_isCFuncType(x6)(st))(x6)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.AbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_False)(x7)(st))(st))(st))(st)
c_showTypeExpr x1 x2@(Curry.Module.AbstractCurry.C_CTCons x8 x9) st = Curry.Module.AbstractCurryPrinter.c_showTypeExpr_case_214(x1)(x9)(x8)(st)
c_showTypeExpr x1 (Curry.Module.AbstractCurry.C_CTypeExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showTypeExpr(x1)(x)(st))(i)(xs)(st)
c_showTypeExpr x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showTypeExpr")(x)



c_showTypeVar :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showTypeVar x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.AbstractCurryPrinter.c_showTypeVar_case_211(x2)(x3)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('a'))(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.c_not(Curry.Module.Prelude.c_null(x3)(st))(st))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_all(Curry.Module.Prelude.pf(Curry.Module.Char.c_isDigit))(st))(x3)(st))(st))(st))(st)
c_showTypeVar (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showTypeVar(x)(st))(i)(xs)(st)
c_showTypeVar x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showTypeVar")(x)



c_showIdentifier :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_showIdentifier st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_not))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_elem))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))(Curry.Module.Prelude.List)))))(st)))



c_isCFuncType :: Curry.Module.AbstractCurry.C_CTypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isCFuncType x1@(Curry.Module.AbstractCurry.C_CFuncType x2 x3) st = Curry.Module.Prelude.C_True
c_isCFuncType x1@(Curry.Module.AbstractCurry.C_CTVar x4) st = Curry.Module.Prelude.C_False
c_isCFuncType x1@(Curry.Module.AbstractCurry.C_CTCons x5 x6) st = Curry.Module.Prelude.C_False
c_isCFuncType (Curry.Module.AbstractCurry.C_CTypeExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isCFuncType(x)(st))(i)(xs)(st)
c_isCFuncType x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isCFuncType")(x)



c_showFuncDecl :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CFuncDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_showFuncDecl st = Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showFuncDeclOpt(Curry.Module.AbstractCurryPrinter.c_defaultOptions(st)))



c_showFuncDeclOpt :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CFuncDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showFuncDeclOpt x1 x2@(Curry.Module.AbstractCurry.C_CmtFunc x3 x4 x5 x6 x7 x8) st = Curry.Module.AbstractCurryPrinter.c_showCmtFunc(x1)(x3)(Curry.Module.AbstractCurry.C_CFunc(x4)(x5)(x6)(x7)(x8))(st)
c_showFuncDeclOpt x1 x2@(Curry.Module.AbstractCurry.C_CFunc x9 x10 x11 x12 x13) st = Curry.Module.AbstractCurryPrinter.c_showCmtFunc(x1)(Curry.Module.Prelude.List)(x2)(st)
c_showFuncDeclOpt x1 (Curry.Module.AbstractCurry.C_CFuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showFuncDeclOpt(x1)(x)(st))(i)(xs)(st)
c_showFuncDeclOpt x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showFuncDeclOpt")(x)



c_showCmtFunc :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.AbstractCurry.C_CFuncDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCmtFunc x1 x2 x3@(Curry.Module.AbstractCurry.C_CFunc x4 x5 x6 x7 x8) st = Curry.Module.AbstractCurryPrinter.c_showCmtFunc_case_210(x1)(x2)(x7)(x8)(x4)(st)
c_showCmtFunc x1 x2 (Curry.Module.AbstractCurry.C_CFuncDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCmtFunc(x1)(x2)(x)(st))(i)(xs)(st)
c_showCmtFunc x1 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCmtFunc")(x)



c_showCmtFunc'46insertName'46139 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCmtFunc'46insertName'46139 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.Prelude.op_43_43(x3)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x1)(x4)(st))(st))(st)
c_showCmtFunc'46insertName'46139 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCmtFunc'46insertName'46139(x1)(x)(st))(i)(xs)(st)
c_showCmtFunc'46insertName'46139 x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCmtFunc.insertName.139")(x)



c_funcComment :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_funcComment st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_unlines))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_lines))(st))(st)



c_showLocalFuncDecl :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CFuncDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_showLocalFuncDecl x1 st = Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showFuncDeclOpt(x1))



c_showRule :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CRule -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showRule x1 x2@(Curry.Module.AbstractCurry.C_CRule x3 x4 x5) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showPattern))(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(st))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showCrhsList(x1)(x4)(st))(Curry.Module.AbstractCurryPrinter.c_showRule_case_203(x1)(x5)(Curry.Module.Prelude.c_null(x5)(st))(st))(st))(st)
c_showRule x1 (Curry.Module.AbstractCurry.C_CRuleOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showRule(x1)(x)(st))(i)(xs)(st)
c_showRule x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showRule")(x)



c_showEvalAnnot :: Curry.Module.AbstractCurry.C_CEvalAnnot -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showEvalAnnot x1@Curry.Module.AbstractCurry.C_CFlex st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))(Curry.Module.Prelude.List))))
c_showEvalAnnot x1@Curry.Module.AbstractCurry.C_CRigid st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))(Curry.Module.Prelude.List)))))
c_showEvalAnnot x1@Curry.Module.AbstractCurry.C_CChoice st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))
c_showEvalAnnot (Curry.Module.AbstractCurry.C_CEvalAnnotOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showEvalAnnot(x)(st))(i)(xs)(st)
c_showEvalAnnot x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showEvalAnnot")(x)



c_showCrhsList :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.AbstractCurry.C_CExpr Curry.Module.AbstractCurry.C_CExpr)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCrhsList x1 x2@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_showCrhsList x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.AbstractCurryPrinter.c_showCrhsList_case_202(x1)(x4)(x3)(st)
c_showCrhsList x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCrhsList(x1)(x)(st))(i)(xs)(st)
c_showCrhsList x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCrhsList")(x)



c_showCrhs :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.T2 Curry.Module.AbstractCurry.C_CExpr Curry.Module.AbstractCurry.C_CExpr) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCrhs x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('|'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showExprOpt(x1)(x3)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))(Curry.Module.AbstractCurryPrinter.c_showExprOpt(x1)(x4)(st))(st))(st))(st)
c_showCrhs x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCrhs(x1)(x)(st))(i)(xs)(st)
c_showCrhs x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCrhs")(x)



c_showLocalDecl :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CLocalDecl -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showLocalDecl x1 x2@(Curry.Module.AbstractCurry.C_CLocalFunc x3) st = Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_showLocalFuncDecl(x1)(st))(x3)(st)
c_showLocalDecl x1 x2@(Curry.Module.AbstractCurry.C_CLocalPat x4 x5 x6) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showPattern(x4)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showExprOpt(x1)(x5)(st))(Curry.Module.AbstractCurryPrinter.c_showLocalDecl_case_199(x1)(x6)(Curry.Module.Prelude.c_null(x6)(st))(st))(st))(st))(st)
c_showLocalDecl x1 x2@(Curry.Module.AbstractCurry.C_CLocalVar x7) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showPattern(Curry.Module.AbstractCurry.C_CPVar(x7))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))(st)
c_showLocalDecl x1 (Curry.Module.AbstractCurry.C_CLocalDeclOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLocalDecl(x1)(x)(st))(i)(xs)(st)
c_showLocalDecl x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLocalDecl")(x)



c_showExpr :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_showExpr st = Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showExprOpt(Curry.Module.AbstractCurryPrinter.c_defaultOptions(st)))



c_showExprOpt :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showExprOpt x1 x2@(Curry.Module.AbstractCurry.C_CVar x3) st = Curry.Module.AbstractCurryPrinter.c_showExprOpt_case_198(x3)(st)
c_showExprOpt x1 x2@(Curry.Module.AbstractCurry.C_CLit x6) st = Curry.Module.AbstractCurryPrinter.c_showLiteral(x6)(st)
c_showExprOpt x1 x2@(Curry.Module.AbstractCurry.C_CSymbol x7) st = Curry.Module.AbstractCurryPrinter.c_showExprOpt_case_197(x1)(x7)(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_isInfixOpName(st))(Curry.Module.Prelude.c_snd(x7)(st))(st))(st)
c_showExprOpt x1 x2@(Curry.Module.AbstractCurry.C_CApply x8 x9) st = Curry.Module.AbstractCurryPrinter.c_showApplication(x1)(Curry.Module.AbstractCurry.C_CApply(x8)(x9))(st)
c_showExprOpt x1 x2@(Curry.Module.AbstractCurry.C_CLambda x10 x11) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection(x1)(x10)(x11)(st)
c_showExprOpt x1 x2@(Curry.Module.AbstractCurry.C_CLetDecl x12 x13) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))(Curry.Module.AbstractCurryPrinter.c_showBlock(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showLocalDecl(x1)))(x12)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x13)(st))(st))(st))(st))(st)
c_showExprOpt x1 x2@(Curry.Module.AbstractCurry.C_CDoExpr x14) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))))))(Curry.Module.AbstractCurryPrinter.c_showBlock(Curry.Module.AbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showStatement(x1)))(x14)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(st))(st))(st)
c_showExprOpt x1 x2@(Curry.Module.AbstractCurry.C_CListComp x15 x16) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x15)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('|'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showStatement(x1)))(x16)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))(st))(st))(st))(st)
c_showExprOpt x1 x2@(Curry.Module.AbstractCurry.C_CCase x17 x18) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x17)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))(Curry.Module.AbstractCurryPrinter.c_showBlock(Curry.Module.AbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showBranchExpr(x1)))(x18)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(st))(st))(st))(st))(st)
c_showExprOpt x1 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showExprOpt(x1)(x)(st))(i)(xs)(st)
c_showExprOpt x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showExprOpt")(x)



c_showSymbol :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showSymbol x1@(Curry.Module.Prelude.T2 x3 x4) x2 st = Curry.Module.AbstractCurryPrinter.c_showSymbol_case_196(x3)(x4)(x2)(st)
c_showSymbol (Curry.Module.Prelude.T2Or i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showSymbol(x)(x2)(st))(i)(xs)(st)
c_showSymbol x x2 st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showSymbol")(x)



c_showLambdaOrSection :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CPattern) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showLambdaOrSection x1 x2@((Curry.Module.Prelude.:<) x4 x5) x3 st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_192(x1)(x2)(x3)(x5)(x4)(st)
c_showLambdaOrSection x1 x2@Curry.Module.Prelude.List x3 st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection x1 (Curry.Module.Prelude.ListOr i xs) x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection(x1)(x)(x3)(st))(i)(xs)(st)
c_showLambdaOrSection x1 x x3 st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection")(x)



c_showLambda :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CPattern) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showLambda x1 x2 x3 st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showPattern))(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.AbstractCurryPrinter.c_showExprOpt(x1)(x3)(st))(st))(st))(st)



c_showStatement :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CStatement -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showStatement x1 x2@(Curry.Module.AbstractCurry.C_CSExpr x3) st = Curry.Module.AbstractCurryPrinter.c_showExprOpt(x1)(x3)(st)
c_showStatement x1 x2@(Curry.Module.AbstractCurry.C_CSPat x4 x5) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showPattern(x4)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.AbstractCurryPrinter.c_showExprOpt(x1)(x5)(st))(st))(st)
c_showStatement x1 x2@(Curry.Module.AbstractCurry.C_CSLet x6) st = Curry.Module.AbstractCurryPrinter.c_showStatement_case_166(x1)(x6)(st)
c_showStatement x1 (Curry.Module.AbstractCurry.C_CStatementOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showStatement(x1)(x)(st))(i)(xs)(st)
c_showStatement x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showStatement")(x)



c_showPattern :: Curry.Module.AbstractCurry.C_CPattern -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showPattern x1@(Curry.Module.AbstractCurry.C_CPVar x2) st = Curry.Module.AbstractCurryPrinter.c_showPattern_case_164(x2)(st)
c_showPattern x1@(Curry.Module.AbstractCurry.C_CPLit x5) st = Curry.Module.AbstractCurryPrinter.c_showLiteral(x5)(st)
c_showPattern x1@(Curry.Module.AbstractCurry.C_CPComb x6 x7) st = Curry.Module.AbstractCurryPrinter.c_showPattern_case_163(x7)(x6)(st)
c_showPattern x1@(Curry.Module.AbstractCurry.C_CPAs x12 x13) st = Curry.Module.AbstractCurryPrinter.c_showPattern_case_159(x13)(x12)(st)
c_showPattern x1@(Curry.Module.AbstractCurry.C_CPFuncComb x16 x17) st = Curry.Module.AbstractCurryPrinter.c_showPattern(Curry.Module.AbstractCurry.C_CPComb(x16)(x17))(st)
c_showPattern (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPattern(x)(st))(i)(xs)(st)
c_showPattern x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPattern")(x)



c_showPreludeCons :: Curry.Module.AbstractCurry.C_CPattern -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showPreludeCons x1 st = let {x3 = Curry.Module.AbstractCurryPrinter.c_showPreludeCons'46_'35selFP6'35name(x1)(st)} in let {x4 = Curry.Module.AbstractCurryPrinter.c_showPreludeCons'46_'35selFP7'35pattlist(x1)(st)} in Curry.Module.AbstractCurryPrinter.c_showPreludeCons_case_158(x1)(x3)(x4)(Curry.Module.Prelude.op_61_61(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(st))(st)



c_showPreludeCons'46_'35selFP6'35name :: Curry.Module.AbstractCurry.C_CPattern -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showPreludeCons'46_'35selFP6'35name x1@(Curry.Module.AbstractCurry.C_CPComb x2 x3) st = Curry.Module.AbstractCurryPrinter.c_showPreludeCons'46_'35selFP6'35name_case_155(x2)(st)
c_showPreludeCons'46_'35selFP6'35name (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPreludeCons'46_'35selFP6'35name(x)(st))(i)(xs)(st)
c_showPreludeCons'46_'35selFP6'35name x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPreludeCons._#selFP6#name")(x)



c_showPreludeCons'46_'35selFP7'35pattlist :: Curry.Module.AbstractCurry.C_CPattern -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CPattern
c_showPreludeCons'46_'35selFP7'35pattlist x1@(Curry.Module.AbstractCurry.C_CPComb x2 x3) st = Curry.Module.AbstractCurryPrinter.c_showPreludeCons'46_'35selFP7'35pattlist_case_154(x3)(x2)(st)
c_showPreludeCons'46_'35selFP7'35pattlist (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPreludeCons'46_'35selFP7'35pattlist(x)(st))(i)(xs)(st)
c_showPreludeCons'46_'35selFP7'35pattlist x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPreludeCons._#selFP7#pattlist")(x)



c_showPatternList :: Curry.Module.AbstractCurry.C_CPattern -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showPatternList x1 st = Curry.Module.AbstractCurryPrinter.c_showPatternList_case_153(x1)(Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern(x1)(st))(st)



c_showPatListElems :: Curry.Module.AbstractCurry.C_CPattern -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_showPatListElems x1@(Curry.Module.AbstractCurry.C_CPComb x2 x3) st = Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_149(x3)(x2)(st)
c_showPatListElems x1@(Curry.Module.AbstractCurry.C_CPVar x28) st = (Curry.Module.Prelude.:<)(Curry.Module.AbstractCurryPrinter.c_showPattern(Curry.Module.AbstractCurry.C_CPVar(x28))(st))(Curry.Module.Prelude.List)
c_showPatListElems x1@(Curry.Module.AbstractCurry.C_CPAs x29 x30) st = (Curry.Module.Prelude.:<)(Curry.Module.AbstractCurryPrinter.c_showPattern(Curry.Module.AbstractCurry.C_CPAs(x29)(x30))(st))(Curry.Module.Prelude.List)
c_showPatListElems (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatListElems(x)(st))(i)(xs)(st)
c_showPatListElems x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatListElems")(x)



c_isClosedPatternList :: Curry.Module.AbstractCurry.C_CPattern -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isClosedPatternList x1@(Curry.Module.AbstractCurry.C_CPComb x2 x3) st = Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_123(x3)(x2)(st)
c_isClosedPatternList x1@(Curry.Module.AbstractCurry.C_CPVar x28) st = Curry.Module.Prelude.C_False
c_isClosedPatternList x1@(Curry.Module.AbstractCurry.C_CPAs x29 x30) st = Curry.Module.AbstractCurryPrinter.c_isClosedPatternList(x30)(st)
c_isClosedPatternList (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedPatternList(x)(st))(i)(xs)(st)
c_isClosedPatternList x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedPatternList")(x)



c_isClosedStringPattern :: Curry.Module.AbstractCurry.C_CPattern -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isClosedStringPattern x1@(Curry.Module.AbstractCurry.C_CPComb x2 x3) st = Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_97(x3)(x2)(st)
c_isClosedStringPattern x1@(Curry.Module.AbstractCurry.C_CPVar x28) st = Curry.Module.Prelude.C_False
c_isClosedStringPattern (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern(x)(st))(i)(xs)(st)
c_isClosedStringPattern x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedStringPattern")(x)



c_isCharPattern :: Curry.Module.AbstractCurry.C_CPattern -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isCharPattern x1@(Curry.Module.AbstractCurry.C_CPLit x2) st = Curry.Module.AbstractCurryPrinter.c_isCharPattern_case_71(x2)(st)
c_isCharPattern x1@(Curry.Module.AbstractCurry.C_CPVar x6) st = Curry.Module.Prelude.C_False
c_isCharPattern x1@(Curry.Module.AbstractCurry.C_CPComb x7 x8) st = Curry.Module.Prelude.C_False
c_isCharPattern x1@(Curry.Module.AbstractCurry.C_CPAs x9 x10) st = Curry.Module.Prelude.C_False
c_isCharPattern x1@(Curry.Module.AbstractCurry.C_CPFuncComb x11 x12) st = Curry.Module.Prelude.C_False
c_isCharPattern (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isCharPattern(x)(st))(i)(xs)(st)
c_isCharPattern x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isCharPattern")(x)



c_isAsPattern :: Curry.Module.AbstractCurry.C_CPattern -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isAsPattern x1@(Curry.Module.AbstractCurry.C_CPAs x2 x3) st = Curry.Module.Prelude.C_True
c_isAsPattern x1@(Curry.Module.AbstractCurry.C_CPVar x4) st = Curry.Module.Prelude.C_False
c_isAsPattern x1@(Curry.Module.AbstractCurry.C_CPLit x5) st = Curry.Module.Prelude.C_False
c_isAsPattern x1@(Curry.Module.AbstractCurry.C_CPComb x6 x7) st = Curry.Module.Prelude.C_False
c_isAsPattern x1@(Curry.Module.AbstractCurry.C_CPFuncComb x8 x9) st = Curry.Module.Prelude.C_False
c_isAsPattern (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isAsPattern(x)(st))(i)(xs)(st)
c_isAsPattern x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isAsPattern")(x)



c_showAsPatternList :: Curry.Module.AbstractCurry.C_CPattern -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showAsPatternList x1@(Curry.Module.AbstractCurry.C_CPAs x2 x3) st = Curry.Module.AbstractCurryPrinter.c_showAsPatternList_case_70(x3)(x2)(st)
c_showAsPatternList (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showAsPatternList(x)(st))(i)(xs)(st)
c_showAsPatternList x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showAsPatternList")(x)



c_showBranchExpr :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CBranchExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showBranchExpr x1 x2@(Curry.Module.AbstractCurry.C_CBranch x3 x4) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showPattern(x3)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.AbstractCurryPrinter.c_showExprOpt(x1)(x4)(st))(st))(st)
c_showBranchExpr x1 (Curry.Module.AbstractCurry.C_CBranchExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showBranchExpr(x1)(x)(st))(i)(xs)(st)
c_showBranchExpr x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showBranchExpr")(x)



c_showLiteral :: Curry.Module.AbstractCurry.C_CLiteral -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showLiteral x1@(Curry.Module.AbstractCurry.C_CIntc x2) st = Curry.Module.Prelude.c_show(x2)(st)
c_showLiteral x1@(Curry.Module.AbstractCurry.C_CFloatc x3) st = Curry.Module.Prelude.c_show(x3)(st)
c_showLiteral x1@(Curry.Module.AbstractCurry.C_CCharc x4) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showCCharc(Curry.Module.AbstractCurry.C_CCharc(x4))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\''))(Curry.Module.Prelude.List))(st))(st)
c_showLiteral (Curry.Module.AbstractCurry.C_CLiteralOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLiteral(x)(st))(i)(xs)(st)
c_showLiteral x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLiteral")(x)



c_showCCharc :: Curry.Module.AbstractCurry.C_CLiteral -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCCharc x1@(Curry.Module.AbstractCurry.C_CCharc x2) st = Curry.Module.AbstractCurryPrinter.c_showCCharc_case_69(x2)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\n'))(st))(st)
c_showCCharc (Curry.Module.AbstractCurry.C_CLiteralOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCCharc(x)(st))(i)(xs)(st)
c_showCCharc x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCCharc")(x)



c_showBlock :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showBlock x1 st = Curry.Module.AbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_47_61(Curry.Module.Prelude.List)))(Curry.Module.Prelude.c_lines(x1)(st))(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(st)



c_showTypeCons :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeExpr) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showTypeCons x1 x2 x3@Curry.Module.Prelude.List st = x2
c_showTypeCons x1 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.AbstractCurryPrinter.c_showTypeCons_case_64(x1)(x2)(x4)(x5)(Curry.Module.Prelude.op_61_61(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(st))(st)
c_showTypeCons x1 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showTypeCons(x1)(x2)(x)(st))(i)(xs)(st)
c_showTypeCons x1 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showTypeCons")(x)



c_showPreludeTypeCons :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CTypeExpr) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showPreludeTypeCons x1 x2 st = Curry.Module.AbstractCurryPrinter.c_showPreludeTypeCons_case_62(x1)(x2)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List)))(st))(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_head(x2)(st))(Curry.Module.AbstractCurry.C_CTCons(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.List))(st))(st))(st)



c_showApplication :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showApplication x1 x2 st = Curry.Module.AbstractCurryPrinter.c_showApplication_case_58(x1)(x2)(Curry.Module.AbstractCurryPrinter.c_applicationHead(x2)(st))(st)



c_applicationHead :: Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.AbstractCurry.C_CExpr
c_applicationHead x1@(Curry.Module.AbstractCurry.C_CApply x2 x3) st = Curry.Module.AbstractCurryPrinter.c_applicationHead(x2)(st)
c_applicationHead x1@(Curry.Module.AbstractCurry.C_CVar x4) st = x1
c_applicationHead x1@(Curry.Module.AbstractCurry.C_CLit x5) st = x1
c_applicationHead x1@(Curry.Module.AbstractCurry.C_CSymbol x6) st = x1
c_applicationHead x1@(Curry.Module.AbstractCurry.C_CLambda x7 x8) st = x1
c_applicationHead x1@(Curry.Module.AbstractCurry.C_CLetDecl x9 x10) st = x1
c_applicationHead x1@(Curry.Module.AbstractCurry.C_CDoExpr x11) st = x1
c_applicationHead x1@(Curry.Module.AbstractCurry.C_CListComp x12 x13) st = x1
c_applicationHead x1@(Curry.Module.AbstractCurry.C_CCase x14 x15) st = x1
c_applicationHead (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_applicationHead(x)(st))(i)(xs)(st)
c_applicationHead x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.applicationHead")(x)



c_showSymbolApplication :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showSymbolApplication x1 x2@(Curry.Module.Prelude.T2 x4 x5) x3 st = Curry.Module.AbstractCurryPrinter.c_showSymbolApplication_case_57(x1)(x3)(x4)(x5)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(st))(Curry.Module.Prelude.op_61_61(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(st))(st))(st)
c_showSymbolApplication x1 (Curry.Module.Prelude.T2Or i xs) x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showSymbolApplication(x1)(x)(x3)(st))(i)(xs)(st)
c_showSymbolApplication x1 x x3 st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showSymbolApplication")(x)



c_showListApplication :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showListApplication x1 x2 st = Curry.Module.AbstractCurryPrinter.c_showListApplication_case_53(x1)(x2)(Curry.Module.AbstractCurryPrinter.c_isStringList(x2)(st))(st)



c_showCharListApplication :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showCharListApplication x1 x2@(Curry.Module.AbstractCurry.C_CApply x3 x4) st = Curry.Module.AbstractCurryPrinter.c_showCharListApplication_case_50(x1)(x4)(x3)(st)
c_showCharListApplication x1 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCharListApplication(x1)(x)(st))(i)(xs)(st)
c_showCharListApplication x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCharListApplication")(x)



c_showConsListApplication :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showConsListApplication x1 x2@(Curry.Module.AbstractCurry.C_CApply x3 x4) st = Curry.Module.AbstractCurryPrinter.c_showConsListApplication_case_47(x1)(x4)(x3)(st)
c_showConsListApplication x1 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showConsListApplication(x1)(x)(st))(i)(xs)(st)
c_showConsListApplication x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showConsListApplication")(x)



c_showSimpleListApplication :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showSimpleListApplication x1 x2@(Curry.Module.AbstractCurry.C_CApply x3 x4) st = Curry.Module.AbstractCurryPrinter.c_showSimpleListApplication_case_45(x1)(x4)(x3)(st)
c_showSimpleListApplication x1 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showSimpleListApplication(x1)(x)(st))(i)(xs)(st)
c_showSimpleListApplication x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showSimpleListApplication")(x)



c_showInfixApplication :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showInfixApplication x1 x2 x3@(Curry.Module.AbstractCurry.C_CApply x4 x5) st = Curry.Module.AbstractCurryPrinter.c_showInfixApplication_case_42(x1)(x2)(x5)(x4)(st)
c_showInfixApplication x1 x2 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showInfixApplication(x1)(x2)(x)(st))(i)(xs)(st)
c_showInfixApplication x1 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showInfixApplication")(x)



c_showITEApplication :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showITEApplication x1 x2@(Curry.Module.AbstractCurry.C_CApply x3 x4) st = Curry.Module.AbstractCurryPrinter.c_showITEApplication_case_41(x1)(x4)(x3)(st)
c_showITEApplication x1 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showITEApplication(x1)(x)(st))(i)(xs)(st)
c_showITEApplication x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showITEApplication")(x)



c_showTupleApplication :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showTupleApplication x1 x2 st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showTupleApplication'46p_showTuple'46386(x1)(x2)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st)



c_showTupleApplication'46p_showTuple'46386 :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showTupleApplication'46p_showTuple'46386 x1 x2@(Curry.Module.AbstractCurry.C_CApply x3 x4) st = Curry.Module.AbstractCurryPrinter.c_showTupleApplication'46p_showTuple'46386_case_38(x1)(x4)(x3)(st)
c_showTupleApplication'46p_showTuple'46386 x1 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showTupleApplication'46p_showTuple'46386(x1)(x)(st))(i)(xs)(st)
c_showTupleApplication'46p_showTuple'46386 x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showTupleApplication.p_showTuple.386")(x)



c_showSimpleApplication :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showSimpleApplication x1 x2@(Curry.Module.AbstractCurry.C_CApply x3 x4) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showSimpleApplication(x1)(x3)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x4)(st))(st))(st)
c_showSimpleApplication x1 x2@(Curry.Module.AbstractCurry.C_CVar x5) st = Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x2)(st)
c_showSimpleApplication x1 x2@(Curry.Module.AbstractCurry.C_CLit x6) st = Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x2)(st)
c_showSimpleApplication x1 x2@(Curry.Module.AbstractCurry.C_CSymbol x7) st = Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x2)(st)
c_showSimpleApplication x1 x2@(Curry.Module.AbstractCurry.C_CLambda x8 x9) st = Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x2)(st)
c_showSimpleApplication x1 x2@(Curry.Module.AbstractCurry.C_CLetDecl x10 x11) st = Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x2)(st)
c_showSimpleApplication x1 x2@(Curry.Module.AbstractCurry.C_CDoExpr x12) st = Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x2)(st)
c_showSimpleApplication x1 x2@(Curry.Module.AbstractCurry.C_CListComp x13 x14) st = Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x2)(st)
c_showSimpleApplication x1 x2@(Curry.Module.AbstractCurry.C_CCase x15 x16) st = Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x2)(st)
c_showSimpleApplication x1 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showSimpleApplication(x1)(x)(st))(i)(xs)(st)
c_showSimpleApplication x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showSimpleApplication")(x)



c_showBoxedExpr :: (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showBoxedExpr x1 x2 st = Curry.Module.AbstractCurryPrinter.c_showBoxedExpr_case_37(x1)(x2)(Curry.Module.AbstractCurryPrinter.c_isSimpleExpr(x2)(st))(st)



c_prefixMap :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_prefixMap x1 x2 x3 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_43_43(x3)))(st))(Curry.Module.Prelude.c_map(x1)(x2)(st))(st)



c_prefixInter :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_prefixInter x1 x2 x3 st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_concat))(Curry.Module.List.c_intersperse(x3)(Curry.Module.Prelude.c_map(x1)(x2)(st))(st))(st)



c_combineMap :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List t0) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_combineMap x1 x2@Curry.Module.Prelude.List x3 st = Curry.Module.Prelude.List
c_combineMap x1 x2@((Curry.Module.Prelude.:<) x4 x5) x3 st = Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_apply(x1)(x4)(st))(Curry.Module.AbstractCurryPrinter.c_prefixMap(x1)(x5)(x3)(st))(st)
c_combineMap x1 (Curry.Module.Prelude.ListOr i xs) x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_combineMap(x1)(x)(x3)(st))(i)(xs)(st)
c_combineMap x1 x x3 st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.combineMap")(x)



c_dropTags :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_dropTags x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.AbstractCurryPrinter.c_dropTags_case_35(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\"'))(st))(st)
c_dropTags (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_dropTags(x)(st))(i)(xs)(st)
c_dropTags x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.dropTags")(x)



c_isInfixOpName :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool)
c_isInfixOpName st = Curry.Module.Prelude.c_all(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_elem))(Curry.Module.AbstractCurryPrinter.c_infixIDs(st))))(st)



c_isStringList :: Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isStringList x1@(Curry.Module.AbstractCurry.C_CSymbol x2) st = Curry.Module.AbstractCurryPrinter.c_isStringList_case_33(x2)(st)
c_isStringList x1@(Curry.Module.AbstractCurry.C_CVar x5) st = Curry.Module.Prelude.C_False
c_isStringList x1@(Curry.Module.AbstractCurry.C_CApply x6 x7) st = Curry.Module.AbstractCurryPrinter.c_isStringList_case_32(x7)(x6)(st)
c_isStringList (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isStringList(x)(st))(i)(xs)(st)
c_isStringList x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isStringList")(x)



c_isClosedList :: Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isClosedList x1@(Curry.Module.AbstractCurry.C_CApply x2 x3) st = Curry.Module.AbstractCurryPrinter.c_isClosedList_case_29(x3)(x2)(st)
c_isClosedList x1@(Curry.Module.AbstractCurry.C_CSymbol x34) st = Curry.Module.AbstractCurryPrinter.c_isClosedList_case_26(x34)(st)
c_isClosedList x1@(Curry.Module.AbstractCurry.C_CVar x37) st = Curry.Module.Prelude.C_False
c_isClosedList x1@(Curry.Module.AbstractCurry.C_CLit x38) st = Curry.Module.Prelude.C_False
c_isClosedList x1@(Curry.Module.AbstractCurry.C_CLambda x39 x40) st = Curry.Module.Prelude.C_False
c_isClosedList x1@(Curry.Module.AbstractCurry.C_CLetDecl x41 x42) st = Curry.Module.Prelude.C_False
c_isClosedList x1@(Curry.Module.AbstractCurry.C_CDoExpr x43) st = Curry.Module.Prelude.C_False
c_isClosedList x1@(Curry.Module.AbstractCurry.C_CListComp x44 x45) st = Curry.Module.Prelude.C_False
c_isClosedList x1@(Curry.Module.AbstractCurry.C_CCase x46 x47) st = Curry.Module.Prelude.C_False
c_isClosedList (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedList(x)(st))(i)(xs)(st)
c_isClosedList x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedList")(x)



c_isSimpleExpr :: Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isSimpleExpr x1@(Curry.Module.AbstractCurry.C_CVar x2) st = Curry.Module.Prelude.C_True
c_isSimpleExpr x1@(Curry.Module.AbstractCurry.C_CLit x3) st = Curry.Module.Prelude.C_True
c_isSimpleExpr x1@(Curry.Module.AbstractCurry.C_CSymbol x4) st = Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_25(x4)(st)
c_isSimpleExpr x1@(Curry.Module.AbstractCurry.C_CApply x7 x8) st = Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_24(x7)(Curry.Module.AbstractCurryPrinter.c_applicationHead(x7)(st))(st)
c_isSimpleExpr x1@(Curry.Module.AbstractCurry.C_CLambda x41 x42) st = Curry.Module.Prelude.C_False
c_isSimpleExpr x1@(Curry.Module.AbstractCurry.C_CLetDecl x43 x44) st = Curry.Module.Prelude.C_False
c_isSimpleExpr x1@(Curry.Module.AbstractCurry.C_CDoExpr x45) st = Curry.Module.Prelude.C_False
c_isSimpleExpr x1@(Curry.Module.AbstractCurry.C_CListComp x46 x47) st = Curry.Module.Prelude.C_False
c_isSimpleExpr x1@(Curry.Module.AbstractCurry.C_CCase x48 x49) st = Curry.Module.Prelude.C_False
c_isSimpleExpr (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isSimpleExpr(x)(st))(i)(xs)(st)
c_isSimpleExpr x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isSimpleExpr")(x)



c_isAtom :: Curry.Module.AbstractCurry.C_CExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isAtom x1@(Curry.Module.AbstractCurry.C_CVar x2) st = Curry.Module.Prelude.C_True
c_isAtom x1@(Curry.Module.AbstractCurry.C_CLit x3) st = Curry.Module.Prelude.C_True
c_isAtom x1@(Curry.Module.AbstractCurry.C_CSymbol x4) st = Curry.Module.AbstractCurryPrinter.c_isAtom_case_7(x4)(st)
c_isAtom x1@(Curry.Module.AbstractCurry.C_CApply x7 x8) st = Curry.Module.Prelude.C_False
c_isAtom x1@(Curry.Module.AbstractCurry.C_CLambda x9 x10) st = Curry.Module.Prelude.C_False
c_isAtom x1@(Curry.Module.AbstractCurry.C_CLetDecl x11 x12) st = Curry.Module.Prelude.C_False
c_isAtom x1@(Curry.Module.AbstractCurry.C_CDoExpr x13) st = Curry.Module.Prelude.C_False
c_isAtom x1@(Curry.Module.AbstractCurry.C_CListComp x14 x15) st = Curry.Module.Prelude.C_False
c_isAtom x1@(Curry.Module.AbstractCurry.C_CCase x16 x17) st = Curry.Module.Prelude.C_False
c_isAtom (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isAtom(x)(st))(i)(xs)(st)
c_isAtom x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isAtom")(x)



c_isUntyped :: Curry.Module.AbstractCurry.C_CTypeExpr -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isUntyped x1@(Curry.Module.AbstractCurry.C_CTCons x2 x3) st = Curry.Module.AbstractCurryPrinter.c_isUntyped_case_6(x3)(x2)(st)
c_isUntyped x1@(Curry.Module.AbstractCurry.C_CTVar x8) st = Curry.Module.Prelude.C_False
c_isUntyped x1@(Curry.Module.AbstractCurry.C_CFuncType x9 x10) st = Curry.Module.Prelude.C_False
c_isUntyped (Curry.Module.AbstractCurry.C_CTypeExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isUntyped(x)(st))(i)(xs)(st)
c_isUntyped x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isUntyped")(x)



c_isTuple :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isTuple x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_False
c_isTuple x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('('))(st))(Curry.Module.AbstractCurryPrinter.c_isTuple'46p1_isTuple'46492(x3)(st))(st)
c_isTuple (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isTuple(x)(st))(i)(xs)(st)
c_isTuple x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isTuple")(x)



c_isTuple'46p1_isTuple'46492 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isTuple'46p1_isTuple'46492 x1@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_False
c_isTuple'46p1_isTuple'46492 x1@((Curry.Module.Prelude.:<) x2 x3) st = Curry.Module.AbstractCurryPrinter.c_isTuple'46p1_isTuple'46492_case_4(x2)(x3)(st)
c_isTuple'46p1_isTuple'46492 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isTuple'46p1_isTuple'46492(x)(st))(i)(xs)(st)
c_isTuple'46p1_isTuple'46492 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isTuple.p1_isTuple.492")(x)



c_infixIDs :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_infixIDs st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('~'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('!'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('@'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('#'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('$'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('%'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('^'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('*'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('+'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('<'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('?'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('|'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))))))))))))))))))))



c_maybeShowBrackets :: Curry.Module.Prelude.C_Bool -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_maybeShowBrackets x1 x2 st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_maybeShowBrackets_case_3(x1)(st))(Curry.Module.Prelude.op_43_43(x2)(Curry.Module.AbstractCurryPrinter.c_maybeShowBrackets_case_2(x1)(st))(st))(st)



c_nameFM :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.AbstractCurry.C_CFuncDecl) -> Curry.RunTimeSystem.State -> Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0)
c_nameFM st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.AbstractCurryPrinter.c_addName))(Curry.Module.FiniteMap.c_emptyFM(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.AbstractCurryPrinter.c_lessString))(st)))



c_addName :: Curry.Module.AbstractCurry.C_CFuncDecl -> (Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0) -> Curry.RunTimeSystem.State -> Curry.Module.FiniteMap.C_FM (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) Curry.Module.Prelude.T0
c_addName x1@(Curry.Module.AbstractCurry.C_CFunc x3 x4 x5 x6 x7) x2 st = Curry.Module.AbstractCurryPrinter.c_addName_case_1(x2)(x3)(st)
c_addName x1@(Curry.Module.AbstractCurry.C_CmtFunc x10 x11 x12 x13 x14 x15) x2 st = Curry.Module.AbstractCurryPrinter.c_addName_case_0(x2)(x11)(st)
c_addName (Curry.Module.AbstractCurry.C_CFuncDeclOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_addName(x)(x2)(st))(i)(xs)(st)
c_addName x x2 st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.addName")(x)



c_lessString :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_lessString x1 x2 st = Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.C_LT)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Sort.c_cmpString(st))(x1)(st))(x2)(st))(st)



c_addName_case_0 x2 x11@(Curry.Module.Prelude.T2 x16 x17) st = Curry.Module.FiniteMap.c_addToFM(x2)(x17)(Curry.Module.Prelude.T0)(st)
c_addName_case_0 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_addName_case_0(x2)(x)(st))(i)(xs)(st)
c_addName_case_0 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.addName_case_0")(x)



c_addName_case_1 x2 x3@(Curry.Module.Prelude.T2 x8 x9) st = Curry.Module.FiniteMap.c_addToFM(x2)(x9)(Curry.Module.Prelude.T0)(st)
c_addName_case_1 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_addName_case_1(x2)(x)(st))(i)(xs)(st)
c_addName_case_1 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.addName_case_1")(x)



c_maybeShowBrackets_case_2 x1@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List)
c_maybeShowBrackets_case_2 x1@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.List
c_maybeShowBrackets_case_2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_maybeShowBrackets_case_2(x)(st))(i)(xs)(st)
c_maybeShowBrackets_case_2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.maybeShowBrackets_case_2")(x)



c_maybeShowBrackets_case_3 x1@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List)
c_maybeShowBrackets_case_3 x1@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.List
c_maybeShowBrackets_case_3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_maybeShowBrackets_case_3(x)(st))(i)(xs)(st)
c_maybeShowBrackets_case_3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.maybeShowBrackets_case_3")(x)



c_isTuple'46p1_isTuple'46492_case_4 x2 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char(')'))(st)
c_isTuple'46p1_isTuple'46492_case_4 x2 x3@((Curry.Module.Prelude.:<) x4 x5) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char(','))(st))(Curry.Module.AbstractCurryPrinter.c_isTuple'46p1_isTuple'46492((Curry.Module.Prelude.:<)(x4)(x5))(st))(st)
c_isTuple'46p1_isTuple'46492_case_4 x2 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isTuple'46p1_isTuple'46492_case_4(x2)(x)(st))(i)(xs)(st)
c_isTuple'46p1_isTuple'46492_case_4 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isTuple.p1_isTuple.492_case_4")(x)



c_isUntyped_case_6 x3 x2@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.AbstractCurryPrinter.c_isUntyped_case_5(x4)(x5)(x3)(st)
c_isUntyped_case_6 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isUntyped_case_6(x3)(x)(st))(i)(xs)(st)
c_isUntyped_case_6 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isUntyped_case_6")(x)



c_isUntyped_case_5 x4 x5 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(st))(Curry.Module.Prelude.op_61_61(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))(Curry.Module.Prelude.List))))))))(st))(st)
c_isUntyped_case_5 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.Prelude.C_False
c_isUntyped_case_5 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isUntyped_case_5(x4)(x5)(x)(st))(i)(xs)(st)
c_isUntyped_case_5 x4 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isUntyped_case_5")(x)



c_isAtom_case_7 x4@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_not))(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_isInfixOpName(st))(x6)(st))(st)
c_isAtom_case_7 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isAtom_case_7(x)(st))(i)(xs)(st)
c_isAtom_case_7 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isAtom_case_7")(x)



c_isSimpleExpr_case_24 x7 x8@(Curry.Module.AbstractCurry.C_CSymbol x9) st = Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_23(x9)(st)
c_isSimpleExpr_case_24 x7 x8@(Curry.Module.AbstractCurry.C_CVar x28) st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_24 x7 x8@(Curry.Module.AbstractCurry.C_CLit x29) st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_24 x7 x8@(Curry.Module.AbstractCurry.C_CApply x30 x31) st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_24 x7 x8@(Curry.Module.AbstractCurry.C_CLambda x32 x33) st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_24 x7 x8@(Curry.Module.AbstractCurry.C_CLetDecl x34 x35) st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_24 x7 x8@(Curry.Module.AbstractCurry.C_CDoExpr x36) st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_24 x7 x8@(Curry.Module.AbstractCurry.C_CListComp x37 x38) st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_24 x7 x8@(Curry.Module.AbstractCurry.C_CCase x39 x40) st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_24 x7 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_24(x7)(x)(st))(i)(xs)(st)
c_isSimpleExpr_case_24 x7 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isSimpleExpr_case_24")(x)



c_isSimpleExpr_case_23 x9@(Curry.Module.Prelude.T2 x10 x11) st = Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_22(x11)(x10)(st)
c_isSimpleExpr_case_23 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_23(x)(st))(i)(xs)(st)
c_isSimpleExpr_case_23 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isSimpleExpr_case_23")(x)



c_isSimpleExpr_case_22 x11 x10@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_21(x11)(x12)(x13)(Curry.Module.Prelude.op_61_61(x12)(Curry.Module.Prelude.C_Char('P'))(st))(st)
c_isSimpleExpr_case_22 x11 x10@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_22 x11 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_22(x11)(x)(st))(i)(xs)(st)
c_isSimpleExpr_case_22 x11 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isSimpleExpr_case_22")(x)



c_isSimpleExpr_case_21 x11 x12 x13 x14@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_20(x11)(x13)(st)
c_isSimpleExpr_case_21 x11 x12 x13 x14@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_21 x11 x12 x13 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_21(x11)(x12)(x13)(x)(st))(i)(xs)(st)
c_isSimpleExpr_case_21 x11 x12 x13 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isSimpleExpr_case_21")(x)



c_isSimpleExpr_case_20 x11 x13@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_19(x11)(x14)(x15)(Curry.Module.Prelude.op_61_61(x14)(Curry.Module.Prelude.C_Char('r'))(st))(st)
c_isSimpleExpr_case_20 x11 x13@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_20 x11 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_20(x11)(x)(st))(i)(xs)(st)
c_isSimpleExpr_case_20 x11 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isSimpleExpr_case_20")(x)



c_isSimpleExpr_case_19 x11 x14 x15 x16@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_18(x11)(x15)(st)
c_isSimpleExpr_case_19 x11 x14 x15 x16@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_19 x11 x14 x15 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_19(x11)(x14)(x15)(x)(st))(i)(xs)(st)
c_isSimpleExpr_case_19 x11 x14 x15 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isSimpleExpr_case_19")(x)



c_isSimpleExpr_case_18 x11 x15@((Curry.Module.Prelude.:<) x16 x17) st = Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_17(x11)(x16)(x17)(Curry.Module.Prelude.op_61_61(x16)(Curry.Module.Prelude.C_Char('e'))(st))(st)
c_isSimpleExpr_case_18 x11 x15@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_18 x11 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_18(x11)(x)(st))(i)(xs)(st)
c_isSimpleExpr_case_18 x11 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isSimpleExpr_case_18")(x)



c_isSimpleExpr_case_17 x11 x16 x17 x18@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_16(x11)(x17)(st)
c_isSimpleExpr_case_17 x11 x16 x17 x18@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_17 x11 x16 x17 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_17(x11)(x16)(x17)(x)(st))(i)(xs)(st)
c_isSimpleExpr_case_17 x11 x16 x17 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isSimpleExpr_case_17")(x)



c_isSimpleExpr_case_16 x11 x17@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_15(x11)(x18)(x19)(Curry.Module.Prelude.op_61_61(x18)(Curry.Module.Prelude.C_Char('l'))(st))(st)
c_isSimpleExpr_case_16 x11 x17@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_16 x11 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_16(x11)(x)(st))(i)(xs)(st)
c_isSimpleExpr_case_16 x11 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isSimpleExpr_case_16")(x)



c_isSimpleExpr_case_15 x11 x18 x19 x20@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_14(x11)(x19)(st)
c_isSimpleExpr_case_15 x11 x18 x19 x20@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_15 x11 x18 x19 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_15(x11)(x18)(x19)(x)(st))(i)(xs)(st)
c_isSimpleExpr_case_15 x11 x18 x19 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isSimpleExpr_case_15")(x)



c_isSimpleExpr_case_14 x11 x19@((Curry.Module.Prelude.:<) x20 x21) st = Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_13(x11)(x20)(x21)(Curry.Module.Prelude.op_61_61(x20)(Curry.Module.Prelude.C_Char('u'))(st))(st)
c_isSimpleExpr_case_14 x11 x19@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_14 x11 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_14(x11)(x)(st))(i)(xs)(st)
c_isSimpleExpr_case_14 x11 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isSimpleExpr_case_14")(x)



c_isSimpleExpr_case_13 x11 x20 x21 x22@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_12(x11)(x21)(st)
c_isSimpleExpr_case_13 x11 x20 x21 x22@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_13 x11 x20 x21 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_13(x11)(x20)(x21)(x)(st))(i)(xs)(st)
c_isSimpleExpr_case_13 x11 x20 x21 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isSimpleExpr_case_13")(x)



c_isSimpleExpr_case_12 x11 x21@((Curry.Module.Prelude.:<) x22 x23) st = Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_11(x11)(x22)(x23)(Curry.Module.Prelude.op_61_61(x22)(Curry.Module.Prelude.C_Char('d'))(st))(st)
c_isSimpleExpr_case_12 x11 x21@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_12 x11 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_12(x11)(x)(st))(i)(xs)(st)
c_isSimpleExpr_case_12 x11 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isSimpleExpr_case_12")(x)



c_isSimpleExpr_case_11 x11 x22 x23 x24@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_10(x11)(x23)(st)
c_isSimpleExpr_case_11 x11 x22 x23 x24@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_11 x11 x22 x23 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_11(x11)(x22)(x23)(x)(st))(i)(xs)(st)
c_isSimpleExpr_case_11 x11 x22 x23 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isSimpleExpr_case_11")(x)



c_isSimpleExpr_case_10 x11 x23@((Curry.Module.Prelude.:<) x24 x25) st = Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_9(x11)(x24)(x25)(Curry.Module.Prelude.op_61_61(x24)(Curry.Module.Prelude.C_Char('e'))(st))(st)
c_isSimpleExpr_case_10 x11 x23@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_10 x11 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_10(x11)(x)(st))(i)(xs)(st)
c_isSimpleExpr_case_10 x11 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isSimpleExpr_case_10")(x)



c_isSimpleExpr_case_9 x11 x24 x25 x26@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_8(x11)(x25)(st)
c_isSimpleExpr_case_9 x11 x24 x25 x26@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_9 x11 x24 x25 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_9(x11)(x24)(x25)(x)(st))(i)(xs)(st)
c_isSimpleExpr_case_9 x11 x24 x25 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isSimpleExpr_case_9")(x)



c_isSimpleExpr_case_8 x11 x25@Curry.Module.Prelude.List st = Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_61_61(x11)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(st))(Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_61_61(x11)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List)))(st))(Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_61_61(x11)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List)))(st))(Curry.Module.AbstractCurryPrinter.c_isTuple(x11)(st))(st))(st))(st)
c_isSimpleExpr_case_8 x11 x25@((Curry.Module.Prelude.:<) x26 x27) st = Curry.Module.Prelude.C_False
c_isSimpleExpr_case_8 x11 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_8(x11)(x)(st))(i)(xs)(st)
c_isSimpleExpr_case_8 x11 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isSimpleExpr_case_8")(x)



c_isSimpleExpr_case_25 x4@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_not))(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_isInfixOpName(st))(x6)(st))(st)
c_isSimpleExpr_case_25 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isSimpleExpr_case_25(x)(st))(i)(xs)(st)
c_isSimpleExpr_case_25 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isSimpleExpr_case_25")(x)



c_isClosedList_case_26 x34@(Curry.Module.Prelude.T2 x35 x36) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x35)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(st))(Curry.Module.Prelude.op_61_61(x36)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List)))(st))(st)
c_isClosedList_case_26 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedList_case_26(x)(st))(i)(xs)(st)
c_isClosedList_case_26 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedList_case_26")(x)



c_isClosedList_case_29 x3 x2@(Curry.Module.AbstractCurry.C_CApply x4 x5) st = Curry.Module.AbstractCurryPrinter.c_isClosedList_case_28(x3)(x4)(st)
c_isClosedList_case_29 x3 x2@(Curry.Module.AbstractCurry.C_CVar x22) st = Curry.Module.Prelude.C_False
c_isClosedList_case_29 x3 x2@(Curry.Module.AbstractCurry.C_CLit x23) st = Curry.Module.Prelude.C_False
c_isClosedList_case_29 x3 x2@(Curry.Module.AbstractCurry.C_CSymbol x24) st = Curry.Module.Prelude.C_False
c_isClosedList_case_29 x3 x2@(Curry.Module.AbstractCurry.C_CLambda x25 x26) st = Curry.Module.Prelude.C_False
c_isClosedList_case_29 x3 x2@(Curry.Module.AbstractCurry.C_CLetDecl x27 x28) st = Curry.Module.Prelude.C_False
c_isClosedList_case_29 x3 x2@(Curry.Module.AbstractCurry.C_CDoExpr x29) st = Curry.Module.Prelude.C_False
c_isClosedList_case_29 x3 x2@(Curry.Module.AbstractCurry.C_CListComp x30 x31) st = Curry.Module.Prelude.C_False
c_isClosedList_case_29 x3 x2@(Curry.Module.AbstractCurry.C_CCase x32 x33) st = Curry.Module.Prelude.C_False
c_isClosedList_case_29 x3 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedList_case_29(x3)(x)(st))(i)(xs)(st)
c_isClosedList_case_29 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedList_case_29")(x)



c_isClosedList_case_28 x3 x4@(Curry.Module.AbstractCurry.C_CSymbol x6) st = Curry.Module.AbstractCurryPrinter.c_isClosedList_case_27(x3)(x6)(st)
c_isClosedList_case_28 x3 x4@(Curry.Module.AbstractCurry.C_CVar x9) st = Curry.Module.Prelude.C_False
c_isClosedList_case_28 x3 x4@(Curry.Module.AbstractCurry.C_CLit x10) st = Curry.Module.Prelude.C_False
c_isClosedList_case_28 x3 x4@(Curry.Module.AbstractCurry.C_CApply x11 x12) st = Curry.Module.Prelude.C_False
c_isClosedList_case_28 x3 x4@(Curry.Module.AbstractCurry.C_CLambda x13 x14) st = Curry.Module.Prelude.C_False
c_isClosedList_case_28 x3 x4@(Curry.Module.AbstractCurry.C_CLetDecl x15 x16) st = Curry.Module.Prelude.C_False
c_isClosedList_case_28 x3 x4@(Curry.Module.AbstractCurry.C_CDoExpr x17) st = Curry.Module.Prelude.C_False
c_isClosedList_case_28 x3 x4@(Curry.Module.AbstractCurry.C_CListComp x18 x19) st = Curry.Module.Prelude.C_False
c_isClosedList_case_28 x3 x4@(Curry.Module.AbstractCurry.C_CCase x20 x21) st = Curry.Module.Prelude.C_False
c_isClosedList_case_28 x3 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedList_case_28(x3)(x)(st))(i)(xs)(st)
c_isClosedList_case_28 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedList_case_28")(x)



c_isClosedList_case_27 x3 x6@(Curry.Module.Prelude.T2 x7 x8) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x7)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x8)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedList(x3)(st))(st))(st)
c_isClosedList_case_27 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedList_case_27(x3)(x)(st))(i)(xs)(st)
c_isClosedList_case_27 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedList_case_27")(x)



c_isStringList_case_32 x7 x6@(Curry.Module.AbstractCurry.C_CApply x8 x9) st = Curry.Module.AbstractCurryPrinter.c_isStringList_case_31(x7)(x9)(st)
c_isStringList_case_32 x7 x6@(Curry.Module.AbstractCurry.C_CVar x27) st = Curry.Module.Prelude.C_False
c_isStringList_case_32 x7 x6@(Curry.Module.AbstractCurry.C_CLit x28) st = Curry.Module.Prelude.C_False
c_isStringList_case_32 x7 x6@(Curry.Module.AbstractCurry.C_CSymbol x29) st = Curry.Module.Prelude.C_False
c_isStringList_case_32 x7 x6@(Curry.Module.AbstractCurry.C_CLambda x30 x31) st = Curry.Module.Prelude.C_False
c_isStringList_case_32 x7 x6@(Curry.Module.AbstractCurry.C_CLetDecl x32 x33) st = Curry.Module.Prelude.C_False
c_isStringList_case_32 x7 x6@(Curry.Module.AbstractCurry.C_CDoExpr x34) st = Curry.Module.Prelude.C_False
c_isStringList_case_32 x7 x6@(Curry.Module.AbstractCurry.C_CListComp x35 x36) st = Curry.Module.Prelude.C_False
c_isStringList_case_32 x7 x6@(Curry.Module.AbstractCurry.C_CCase x37 x38) st = Curry.Module.Prelude.C_False
c_isStringList_case_32 x7 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isStringList_case_32(x7)(x)(st))(i)(xs)(st)
c_isStringList_case_32 x7 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isStringList_case_32")(x)



c_isStringList_case_31 x7 x9@(Curry.Module.AbstractCurry.C_CLit x10) st = Curry.Module.AbstractCurryPrinter.c_isStringList_case_30(x7)(x10)(st)
c_isStringList_case_31 x7 x9@(Curry.Module.AbstractCurry.C_CVar x14) st = Curry.Module.Prelude.C_False
c_isStringList_case_31 x7 x9@(Curry.Module.AbstractCurry.C_CSymbol x15) st = Curry.Module.Prelude.C_False
c_isStringList_case_31 x7 x9@(Curry.Module.AbstractCurry.C_CApply x16 x17) st = Curry.Module.Prelude.C_False
c_isStringList_case_31 x7 x9@(Curry.Module.AbstractCurry.C_CLambda x18 x19) st = Curry.Module.Prelude.C_False
c_isStringList_case_31 x7 x9@(Curry.Module.AbstractCurry.C_CLetDecl x20 x21) st = Curry.Module.Prelude.C_False
c_isStringList_case_31 x7 x9@(Curry.Module.AbstractCurry.C_CDoExpr x22) st = Curry.Module.Prelude.C_False
c_isStringList_case_31 x7 x9@(Curry.Module.AbstractCurry.C_CListComp x23 x24) st = Curry.Module.Prelude.C_False
c_isStringList_case_31 x7 x9@(Curry.Module.AbstractCurry.C_CCase x25 x26) st = Curry.Module.Prelude.C_False
c_isStringList_case_31 x7 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isStringList_case_31(x7)(x)(st))(i)(xs)(st)
c_isStringList_case_31 x7 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isStringList_case_31")(x)



c_isStringList_case_30 x7 x10@(Curry.Module.AbstractCurry.C_CCharc x11) st = Curry.Module.AbstractCurryPrinter.c_isStringList(x7)(st)
c_isStringList_case_30 x7 x10@(Curry.Module.AbstractCurry.C_CIntc x12) st = Curry.Module.Prelude.C_False
c_isStringList_case_30 x7 x10@(Curry.Module.AbstractCurry.C_CFloatc x13) st = Curry.Module.Prelude.C_False
c_isStringList_case_30 x7 (Curry.Module.AbstractCurry.C_CLiteralOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isStringList_case_30(x7)(x)(st))(i)(xs)(st)
c_isStringList_case_30 x7 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isStringList_case_30")(x)



c_isStringList_case_33 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(st))(Curry.Module.Prelude.op_61_61(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List)))(st))(st)
c_isStringList_case_33 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isStringList_case_33(x)(st))(i)(xs)(st)
c_isStringList_case_33 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isStringList_case_33")(x)



c_dropTags_case_35 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_dropTags))(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_tail))(Curry.Module.Prelude.c_dropWhile(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_47_61))(Curry.Module.Prelude.C_Char('\"'))))(x3)(st))(st))(st)
c_dropTags_case_35 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_dropTags_case_34(x2)(x3)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('>'))(st))(st)
c_dropTags_case_35 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_dropTags_case_35(x2)(x3)(x)(st))(i)(xs)(st)
c_dropTags_case_35 x2 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.dropTags_case_35")(x)



c_dropTags_case_34 x2 x3 x4@Curry.Module.Prelude.C_True st = x3
c_dropTags_case_34 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_dropTags(x3)(st)
c_dropTags_case_34 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_dropTags_case_34(x2)(x3)(x)(st))(i)(xs)(st)
c_dropTags_case_34 x2 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.dropTags_case_34")(x)



c_showBoxedExpr_case_37 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurryPrinter.c_showExprOpt(x1)(x2)(st)
c_showBoxedExpr_case_37 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showBoxedExpr_case_36(x1)(x2)(Curry.Module.Prelude.c_otherwise(st))(st)
c_showBoxedExpr_case_37 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showBoxedExpr_case_37(x1)(x2)(x)(st))(i)(xs)(st)
c_showBoxedExpr_case_37 x1 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showBoxedExpr_case_37")(x)



c_showBoxedExpr_case_36 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showExprOpt(x1)(x2)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st)
c_showBoxedExpr_case_36 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showBoxedExpr_case_36(x1)(x2)(x)(st))(i)(xs)(st)
c_showBoxedExpr_case_36 x1 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showBoxedExpr_case_36")(x)



c_showTupleApplication'46p_showTuple'46386_case_38 x1 x4 x3@(Curry.Module.AbstractCurry.C_CSymbol x5) st = Curry.Module.AbstractCurryPrinter.c_showExprOpt(x1)(x4)(st)
c_showTupleApplication'46p_showTuple'46386_case_38 x1 x4 x3@(Curry.Module.AbstractCurry.C_CApply x6 x7) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showTupleApplication'46p_showTuple'46386(x1)(Curry.Module.AbstractCurry.C_CApply(x6)(x7))(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showExprOpt(x1)(x4)(st))(st))(st)
c_showTupleApplication'46p_showTuple'46386_case_38 x1 x4 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showTupleApplication'46p_showTuple'46386_case_38(x1)(x4)(x)(st))(i)(xs)(st)
c_showTupleApplication'46p_showTuple'46386_case_38 x1 x4 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showTupleApplication.p_showTuple.386_case_38")(x)



c_showITEApplication_case_41 x1 x4 x3@(Curry.Module.AbstractCurry.C_CApply x5 x6) st = Curry.Module.AbstractCurryPrinter.c_showITEApplication_case_40(x1)(x3)(x4)(x6)(x5)(st)
c_showITEApplication_case_41 x1 x4 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showITEApplication_case_41(x1)(x4)(x)(st))(i)(xs)(st)
c_showITEApplication_case_41 x1 x4 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showITEApplication_case_41")(x)



c_showITEApplication_case_40 x1 x3 x4 x6 x5@(Curry.Module.AbstractCurry.C_CApply x7 x8) st = Curry.Module.AbstractCurryPrinter.c_showITEApplication_case_39(x1)(x3)(x4)(x6)(x8)(x7)(st)
c_showITEApplication_case_40 x1 x3 x4 x6 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showITEApplication_case_40(x1)(x3)(x4)(x6)(x)(st))(i)(xs)(st)
c_showITEApplication_case_40 x1 x3 x4 x6 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showITEApplication_case_40")(x)



c_showITEApplication_case_39 x1 x3 x4 x6 x8 x7@(Curry.Module.AbstractCurry.C_CSymbol x9) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showExprOpt(x1)(x8)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showExprOpt(x1)(x6)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))(Curry.Module.AbstractCurryPrinter.c_showExprOpt(x1)(x4)(st))(st))(st))(st))(st))(st)
c_showITEApplication_case_39 x1 x3 x4 x6 x8 x7@(Curry.Module.AbstractCurry.C_CApply x10 x11) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showITEApplication(x1)(x3)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x4)(st))(st))(st))(st)
c_showITEApplication_case_39 x1 x3 x4 x6 x8 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showITEApplication_case_39(x1)(x3)(x4)(x6)(x8)(x)(st))(i)(xs)(st)
c_showITEApplication_case_39 x1 x3 x4 x6 x8 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showITEApplication_case_39")(x)



c_showInfixApplication_case_42 x1 x2 x5 x4@(Curry.Module.AbstractCurry.C_CApply x6 x7) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x7)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showSymbol(x1)(x2)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x5)(st))(st))(st))(st))(st)
c_showInfixApplication_case_42 x1 x2 x5 x4@(Curry.Module.AbstractCurry.C_CVar x8) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showSymbol(x1)(x2)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x5)(st))(st))(st))(st)
c_showInfixApplication_case_42 x1 x2 x5 x4@(Curry.Module.AbstractCurry.C_CLit x9) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showSymbol(x1)(x2)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x5)(st))(st))(st))(st)
c_showInfixApplication_case_42 x1 x2 x5 x4@(Curry.Module.AbstractCurry.C_CSymbol x10) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showSymbol(x1)(x2)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x5)(st))(st))(st))(st)
c_showInfixApplication_case_42 x1 x2 x5 x4@(Curry.Module.AbstractCurry.C_CLambda x11 x12) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showSymbol(x1)(x2)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x5)(st))(st))(st))(st)
c_showInfixApplication_case_42 x1 x2 x5 x4@(Curry.Module.AbstractCurry.C_CLetDecl x13 x14) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showSymbol(x1)(x2)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x5)(st))(st))(st))(st)
c_showInfixApplication_case_42 x1 x2 x5 x4@(Curry.Module.AbstractCurry.C_CDoExpr x15) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showSymbol(x1)(x2)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x5)(st))(st))(st))(st)
c_showInfixApplication_case_42 x1 x2 x5 x4@(Curry.Module.AbstractCurry.C_CListComp x16 x17) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showSymbol(x1)(x2)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x5)(st))(st))(st))(st)
c_showInfixApplication_case_42 x1 x2 x5 x4@(Curry.Module.AbstractCurry.C_CCase x18 x19) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showSymbol(x1)(x2)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x5)(st))(st))(st))(st)
c_showInfixApplication_case_42 x1 x2 x5 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showInfixApplication_case_42(x1)(x2)(x5)(x)(st))(i)(xs)(st)
c_showInfixApplication_case_42 x1 x2 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showInfixApplication_case_42")(x)



c_showSimpleListApplication_case_45 x1 x4 x3@(Curry.Module.AbstractCurry.C_CApply x5 x6) st = Curry.Module.AbstractCurryPrinter.c_showSimpleListApplication_case_44(x1)(x6)(x4)(st)
c_showSimpleListApplication_case_45 x1 x4 x3@(Curry.Module.AbstractCurry.C_CSymbol x21) st = Curry.Module.AbstractCurryPrinter.c_showSimpleListApplication_case_43(x1)(x4)(x21)(st)
c_showSimpleListApplication_case_45 x1 x4 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showSimpleListApplication_case_45(x1)(x4)(x)(st))(i)(xs)(st)
c_showSimpleListApplication_case_45 x1 x4 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showSimpleListApplication_case_45")(x)



c_showSimpleListApplication_case_43 x1 x4 x21@(Curry.Module.Prelude.T2 x22 x23) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x4)(st))(x23)(st)
c_showSimpleListApplication_case_43 x1 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showSimpleListApplication_case_43(x1)(x4)(x)(st))(i)(xs)(st)
c_showSimpleListApplication_case_43 x1 x4 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showSimpleListApplication_case_43")(x)



c_showSimpleListApplication_case_44 x1 x6 x4@(Curry.Module.AbstractCurry.C_CSymbol x7) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x6)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))))(st)
c_showSimpleListApplication_case_44 x1 x6 x4@(Curry.Module.AbstractCurry.C_CVar x8) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x6)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x4)(st))(st))(st)
c_showSimpleListApplication_case_44 x1 x6 x4@(Curry.Module.AbstractCurry.C_CLit x9) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x6)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x4)(st))(st))(st)
c_showSimpleListApplication_case_44 x1 x6 x4@(Curry.Module.AbstractCurry.C_CApply x10 x11) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x6)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x4)(st))(st))(st)
c_showSimpleListApplication_case_44 x1 x6 x4@(Curry.Module.AbstractCurry.C_CLambda x12 x13) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x6)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x4)(st))(st))(st)
c_showSimpleListApplication_case_44 x1 x6 x4@(Curry.Module.AbstractCurry.C_CLetDecl x14 x15) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x6)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x4)(st))(st))(st)
c_showSimpleListApplication_case_44 x1 x6 x4@(Curry.Module.AbstractCurry.C_CDoExpr x16) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x6)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x4)(st))(st))(st)
c_showSimpleListApplication_case_44 x1 x6 x4@(Curry.Module.AbstractCurry.C_CListComp x17 x18) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x6)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x4)(st))(st))(st)
c_showSimpleListApplication_case_44 x1 x6 x4@(Curry.Module.AbstractCurry.C_CCase x19 x20) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x6)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x4)(st))(st))(st)
c_showSimpleListApplication_case_44 x1 x6 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showSimpleListApplication_case_44(x1)(x6)(x)(st))(i)(xs)(st)
c_showSimpleListApplication_case_44 x1 x6 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showSimpleListApplication_case_44")(x)



c_showConsListApplication_case_47 x1 x4 x3@(Curry.Module.AbstractCurry.C_CApply x5 x6) st = Curry.Module.AbstractCurryPrinter.c_showConsListApplication_case_46(x1)(x6)(x4)(st)
c_showConsListApplication_case_47 x1 x4 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showConsListApplication_case_47(x1)(x4)(x)(st))(i)(xs)(st)
c_showConsListApplication_case_47 x1 x4 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showConsListApplication_case_47")(x)



c_showConsListApplication_case_46 x1 x6 x4@(Curry.Module.AbstractCurry.C_CSymbol x7) st = Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x6)(st)
c_showConsListApplication_case_46 x1 x6 x4@(Curry.Module.AbstractCurry.C_CVar x8) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x6)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showConsListApplication(x1)(x4)(st))(st))(st)
c_showConsListApplication_case_46 x1 x6 x4@(Curry.Module.AbstractCurry.C_CLit x9) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x6)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showConsListApplication(x1)(x4)(st))(st))(st)
c_showConsListApplication_case_46 x1 x6 x4@(Curry.Module.AbstractCurry.C_CApply x10 x11) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x6)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showConsListApplication(x1)(x4)(st))(st))(st)
c_showConsListApplication_case_46 x1 x6 x4@(Curry.Module.AbstractCurry.C_CLambda x12 x13) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x6)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showConsListApplication(x1)(x4)(st))(st))(st)
c_showConsListApplication_case_46 x1 x6 x4@(Curry.Module.AbstractCurry.C_CLetDecl x14 x15) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x6)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showConsListApplication(x1)(x4)(st))(st))(st)
c_showConsListApplication_case_46 x1 x6 x4@(Curry.Module.AbstractCurry.C_CDoExpr x16) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x6)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showConsListApplication(x1)(x4)(st))(st))(st)
c_showConsListApplication_case_46 x1 x6 x4@(Curry.Module.AbstractCurry.C_CListComp x17 x18) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x6)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showConsListApplication(x1)(x4)(st))(st))(st)
c_showConsListApplication_case_46 x1 x6 x4@(Curry.Module.AbstractCurry.C_CCase x19 x20) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x6)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showConsListApplication(x1)(x4)(st))(st))(st)
c_showConsListApplication_case_46 x1 x6 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showConsListApplication_case_46(x1)(x6)(x)(st))(i)(xs)(st)
c_showConsListApplication_case_46 x1 x6 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showConsListApplication_case_46")(x)



c_showCharListApplication_case_50 x1 x4 x3@(Curry.Module.AbstractCurry.C_CApply x5 x6) st = Curry.Module.AbstractCurryPrinter.c_showCharListApplication_case_49(x1)(x4)(x6)(st)
c_showCharListApplication_case_50 x1 x4 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCharListApplication_case_50(x1)(x4)(x)(st))(i)(xs)(st)
c_showCharListApplication_case_50 x1 x4 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCharListApplication_case_50")(x)



c_showCharListApplication_case_49 x1 x4 x6@(Curry.Module.AbstractCurry.C_CLit x7) st = Curry.Module.AbstractCurryPrinter.c_showCharListApplication_case_48(x1)(x7)(x4)(st)
c_showCharListApplication_case_49 x1 x4 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCharListApplication_case_49(x1)(x4)(x)(st))(i)(xs)(st)
c_showCharListApplication_case_49 x1 x4 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCharListApplication_case_49")(x)



c_showCharListApplication_case_48 x1 x7 x4@(Curry.Module.AbstractCurry.C_CSymbol x8) st = Curry.Module.AbstractCurryPrinter.c_showCCharc(x7)(st)
c_showCharListApplication_case_48 x1 x7 x4@(Curry.Module.AbstractCurry.C_CVar x9) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showCCharc(x7)(st))(Curry.Module.AbstractCurryPrinter.c_showCharListApplication(x1)(x4)(st))(st)
c_showCharListApplication_case_48 x1 x7 x4@(Curry.Module.AbstractCurry.C_CLit x10) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showCCharc(x7)(st))(Curry.Module.AbstractCurryPrinter.c_showCharListApplication(x1)(x4)(st))(st)
c_showCharListApplication_case_48 x1 x7 x4@(Curry.Module.AbstractCurry.C_CApply x11 x12) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showCCharc(x7)(st))(Curry.Module.AbstractCurryPrinter.c_showCharListApplication(x1)(x4)(st))(st)
c_showCharListApplication_case_48 x1 x7 x4@(Curry.Module.AbstractCurry.C_CLambda x13 x14) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showCCharc(x7)(st))(Curry.Module.AbstractCurryPrinter.c_showCharListApplication(x1)(x4)(st))(st)
c_showCharListApplication_case_48 x1 x7 x4@(Curry.Module.AbstractCurry.C_CLetDecl x15 x16) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showCCharc(x7)(st))(Curry.Module.AbstractCurryPrinter.c_showCharListApplication(x1)(x4)(st))(st)
c_showCharListApplication_case_48 x1 x7 x4@(Curry.Module.AbstractCurry.C_CDoExpr x17) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showCCharc(x7)(st))(Curry.Module.AbstractCurryPrinter.c_showCharListApplication(x1)(x4)(st))(st)
c_showCharListApplication_case_48 x1 x7 x4@(Curry.Module.AbstractCurry.C_CListComp x18 x19) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showCCharc(x7)(st))(Curry.Module.AbstractCurryPrinter.c_showCharListApplication(x1)(x4)(st))(st)
c_showCharListApplication_case_48 x1 x7 x4@(Curry.Module.AbstractCurry.C_CCase x20 x21) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showCCharc(x7)(st))(Curry.Module.AbstractCurryPrinter.c_showCharListApplication(x1)(x4)(st))(st)
c_showCharListApplication_case_48 x1 x7 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCharListApplication_case_48(x1)(x7)(x)(st))(i)(xs)(st)
c_showCharListApplication_case_48 x1 x7 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCharListApplication_case_48")(x)



c_showListApplication_case_53 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showCharListApplication(x1)(x2)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List))(st))(st)
c_showListApplication_case_53 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showListApplication_case_52(x1)(x2)(Curry.Module.AbstractCurryPrinter.c_isClosedList(x2)(st))(st)
c_showListApplication_case_53 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showListApplication_case_53(x1)(x2)(x)(st))(i)(xs)(st)
c_showListApplication_case_53 x1 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showListApplication_case_53")(x)



c_showListApplication_case_52 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showConsListApplication(x1)(x2)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))(st))(st)
c_showListApplication_case_52 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showListApplication_case_51(x1)(x2)(Curry.Module.Prelude.c_otherwise(st))(st)
c_showListApplication_case_52 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showListApplication_case_52(x1)(x2)(x)(st))(i)(xs)(st)
c_showListApplication_case_52 x1 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showListApplication_case_52")(x)



c_showListApplication_case_51 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showSimpleListApplication(x1)(x2)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st)
c_showListApplication_case_51 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showListApplication_case_51(x1)(x2)(x)(st))(i)(xs)(st)
c_showListApplication_case_51 x1 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showListApplication_case_51")(x)



c_showSymbolApplication_case_57 x1 x3 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurryPrinter.c_showListApplication(x1)(x3)(st)
c_showSymbolApplication_case_57 x1 x3 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showSymbolApplication_case_56(x1)(x3)(x4)(x5)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(st))(Curry.Module.Prelude.op_61_61(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List)))))))))))))(st))(st))(st)
c_showSymbolApplication_case_57 x1 x3 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showSymbolApplication_case_57(x1)(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c_showSymbolApplication_case_57 x1 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showSymbolApplication_case_57")(x)



c_showSymbolApplication_case_56 x1 x3 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurryPrinter.c_showITEApplication(x1)(x3)(st)
c_showSymbolApplication_case_56 x1 x3 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showSymbolApplication_case_55(x1)(x3)(x5)(Curry.Module.AbstractCurryPrinter.c_isTuple(x5)(st))(st)
c_showSymbolApplication_case_56 x1 x3 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showSymbolApplication_case_56(x1)(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c_showSymbolApplication_case_56 x1 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showSymbolApplication_case_56")(x)



c_showSymbolApplication_case_55 x1 x3 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurryPrinter.c_showTupleApplication(x1)(x3)(st)
c_showSymbolApplication_case_55 x1 x3 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showSymbolApplication_case_54(x1)(x3)(Curry.Module.Prelude.c_otherwise(st))(st)
c_showSymbolApplication_case_55 x1 x3 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showSymbolApplication_case_55(x1)(x3)(x5)(x)(st))(i)(xs)(st)
c_showSymbolApplication_case_55 x1 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showSymbolApplication_case_55")(x)



c_showSymbolApplication_case_54 x1 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurryPrinter.c_showSimpleApplication(x1)(x3)(st)
c_showSymbolApplication_case_54 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showSymbolApplication_case_54(x1)(x3)(x)(st))(i)(xs)(st)
c_showSymbolApplication_case_54 x1 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showSymbolApplication_case_54")(x)



c_showApplication_case_58 x1 x2 (Curry.Module.AbstractCurry.C_CSymbol x3) st = Curry.Module.AbstractCurryPrinter.c_showSymbolApplication(x1)(x3)(x2)(st)
c_showApplication_case_58 x1 x2 x3@(Curry.Module.AbstractCurry.C_CVar x4) st = Curry.Module.AbstractCurryPrinter.c_showSimpleApplication(x1)(x2)(st)
c_showApplication_case_58 x1 x2 x3@(Curry.Module.AbstractCurry.C_CLit x5) st = Curry.Module.AbstractCurryPrinter.c_showSimpleApplication(x1)(x2)(st)
c_showApplication_case_58 x1 x2 x3@(Curry.Module.AbstractCurry.C_CApply x6 x7) st = Curry.Module.AbstractCurryPrinter.c_showSimpleApplication(x1)(x2)(st)
c_showApplication_case_58 x1 x2 x3@(Curry.Module.AbstractCurry.C_CLambda x8 x9) st = Curry.Module.AbstractCurryPrinter.c_showSimpleApplication(x1)(x2)(st)
c_showApplication_case_58 x1 x2 x3@(Curry.Module.AbstractCurry.C_CLetDecl x10 x11) st = Curry.Module.AbstractCurryPrinter.c_showSimpleApplication(x1)(x2)(st)
c_showApplication_case_58 x1 x2 x3@(Curry.Module.AbstractCurry.C_CDoExpr x12) st = Curry.Module.AbstractCurryPrinter.c_showSimpleApplication(x1)(x2)(st)
c_showApplication_case_58 x1 x2 x3@(Curry.Module.AbstractCurry.C_CListComp x13 x14) st = Curry.Module.AbstractCurryPrinter.c_showSimpleApplication(x1)(x2)(st)
c_showApplication_case_58 x1 x2 x3@(Curry.Module.AbstractCurry.C_CCase x15 x16) st = Curry.Module.AbstractCurryPrinter.c_showSimpleApplication(x1)(x2)(st)
c_showApplication_case_58 x1 x2 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showApplication_case_58(x1)(x2)(x)(st))(i)(xs)(st)
c_showApplication_case_58 x1 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showApplication_case_58")(x)



c_showPreludeTypeCons_case_62 x1 x2 x3@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))(Curry.Module.Prelude.List))))))
c_showPreludeTypeCons_case_62 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showPreludeTypeCons_case_61(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List)))(st))(st)
c_showPreludeTypeCons_case_62 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPreludeTypeCons_case_62(x1)(x2)(x)(st))(i)(xs)(st)
c_showPreludeTypeCons_case_62 x1 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPreludeTypeCons_case_62")(x)



c_showPreludeTypeCons_case_61 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_False)(Curry.Module.Prelude.c_head(x2)(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))(st))(st)
c_showPreludeTypeCons_case_61 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showPreludeTypeCons_case_60(x1)(x2)(Curry.Module.AbstractCurryPrinter.c_isTuple(x1)(st))(st)
c_showPreludeTypeCons_case_61 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPreludeTypeCons_case_61(x1)(x2)(x)(st))(i)(xs)(st)
c_showPreludeTypeCons_case_61 x1 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPreludeTypeCons_case_61")(x)



c_showPreludeTypeCons_case_60 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_False)))(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st)
c_showPreludeTypeCons_case_60 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showPreludeTypeCons_case_59(x1)(x2)(Curry.Module.Prelude.c_otherwise(st))(st)
c_showPreludeTypeCons_case_60 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPreludeTypeCons_case_60(x1)(x2)(x)(st))(i)(xs)(st)
c_showPreludeTypeCons_case_60 x1 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPreludeTypeCons_case_60")(x)



c_showPreludeTypeCons_case_59 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43(x1)(Curry.Module.AbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_True)))(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(st))(st)
c_showPreludeTypeCons_case_59 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPreludeTypeCons_case_59(x1)(x2)(x)(st))(i)(xs)(st)
c_showPreludeTypeCons_case_59 x1 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPreludeTypeCons_case_59")(x)



c_showTypeCons_case_64 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurryPrinter.c_showPreludeTypeCons(x2)((Curry.Module.Prelude.:<)(x4)(x5))(st)
c_showTypeCons_case_64 x1 x2 x4 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showTypeCons_case_63(x2)(x4)(x5)(Curry.Module.Prelude.c_otherwise(st))(st)
c_showTypeCons_case_64 x1 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showTypeCons_case_64(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c_showTypeCons_case_64 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showTypeCons_case_64")(x)



c_showTypeCons_case_63 x2 x4 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43(x2)(Curry.Module.AbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_True)))((Curry.Module.Prelude.:<)(x4)(x5))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(st))(st)
c_showTypeCons_case_63 x2 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showTypeCons_case_63(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c_showTypeCons_case_63 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showTypeCons_case_63")(x)



c_showCCharc_case_69 x2 x3@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List))
c_showCCharc_case_69 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showCCharc_case_68(x2)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\r'))(st))(st)
c_showCCharc_case_69 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCCharc_case_69(x2)(x)(st))(i)(xs)(st)
c_showCCharc_case_69 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCCharc_case_69")(x)



c_showCCharc_case_68 x2 x3@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))
c_showCCharc_case_68 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showCCharc_case_67(x2)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\\'))(st))(st)
c_showCCharc_case_68 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCCharc_case_68(x2)(x)(st))(i)(xs)(st)
c_showCCharc_case_68 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCCharc_case_68")(x)



c_showCCharc_case_67 x2 x3@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))(Curry.Module.Prelude.List))
c_showCCharc_case_67 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showCCharc_case_66(x2)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Char('\"'))(st))(st)
c_showCCharc_case_67 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCCharc_case_67(x2)(x)(st))(i)(xs)(st)
c_showCCharc_case_67 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCCharc_case_67")(x)



c_showCCharc_case_66 x2 x3@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\\'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List))
c_showCCharc_case_66 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showCCharc_case_65(x2)(Curry.Module.Prelude.c_otherwise(st))(st)
c_showCCharc_case_66 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCCharc_case_66(x2)(x)(st))(i)(xs)(st)
c_showCCharc_case_66 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCCharc_case_66")(x)



c_showCCharc_case_65 x2 x3@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List)
c_showCCharc_case_65 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCCharc_case_65(x2)(x)(st))(i)(xs)(st)
c_showCCharc_case_65 x2 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCCharc_case_65")(x)



c_showAsPatternList_case_70 x3 x2@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.Prelude.op_43_43(x5)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('@'))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_concat(Curry.Module.List.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showPatListElems(x3)(st))(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st))(st))(st)
c_showAsPatternList_case_70 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showAsPatternList_case_70(x3)(x)(st))(i)(xs)(st)
c_showAsPatternList_case_70 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showAsPatternList_case_70")(x)



c_isCharPattern_case_71 x2@(Curry.Module.AbstractCurry.C_CCharc x3) st = Curry.Module.Prelude.C_True
c_isCharPattern_case_71 x2@(Curry.Module.AbstractCurry.C_CIntc x4) st = Curry.Module.Prelude.C_False
c_isCharPattern_case_71 x2@(Curry.Module.AbstractCurry.C_CFloatc x5) st = Curry.Module.Prelude.C_False
c_isCharPattern_case_71 (Curry.Module.AbstractCurry.C_CLiteralOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isCharPattern_case_71(x)(st))(i)(xs)(st)
c_isCharPattern_case_71 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isCharPattern_case_71")(x)



c_isClosedStringPattern_case_97 x3 x2@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_96(x3)(x5)(x4)(st)
c_isClosedStringPattern_case_97 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_97(x3)(x)(st))(i)(xs)(st)
c_isClosedStringPattern_case_97 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedStringPattern_case_97")(x)



c_isClosedStringPattern_case_96 x3 x5 x4@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_95(x3)(x5)(x7)(x6)(st)
c_isClosedStringPattern_case_96 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_96(x3)(x5)(x)(st))(i)(xs)(st)
c_isClosedStringPattern_case_96 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedStringPattern_case_96")(x)



c_isClosedStringPattern_case_95 x3 x5 x7 x6 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('P'))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_94(x3)(x5)(x7)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_isClosedStringPattern_case_94 x3 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_93(x3)(x5)(x9)(x8)(st)
c_isClosedStringPattern_case_94 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_94(x3)(x5)(x)(st))(i)(xs)(st)
c_isClosedStringPattern_case_94 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedStringPattern_case_94")(x)



c_isClosedStringPattern_case_93 x3 x5 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_92(x3)(x5)(x9)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_isClosedStringPattern_case_92 x3 x5 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_91(x3)(x5)(x11)(x10)(st)
c_isClosedStringPattern_case_92 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_92(x3)(x5)(x)(st))(i)(xs)(st)
c_isClosedStringPattern_case_92 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedStringPattern_case_92")(x)



c_isClosedStringPattern_case_91 x3 x5 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_90(x3)(x5)(x11)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_isClosedStringPattern_case_90 x3 x5 x11@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_89(x3)(x5)(x13)(x12)(st)
c_isClosedStringPattern_case_90 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_90(x3)(x5)(x)(st))(i)(xs)(st)
c_isClosedStringPattern_case_90 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedStringPattern_case_90")(x)



c_isClosedStringPattern_case_89 x3 x5 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_88(x3)(x5)(x13)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_isClosedStringPattern_case_88 x3 x5 x13@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_87(x3)(x5)(x15)(x14)(st)
c_isClosedStringPattern_case_88 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_88(x3)(x5)(x)(st))(i)(xs)(st)
c_isClosedStringPattern_case_88 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedStringPattern_case_88")(x)



c_isClosedStringPattern_case_87 x3 x5 x15 x14 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x14)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_86(x3)(x5)(x15)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_isClosedStringPattern_case_86 x3 x5 x15@((Curry.Module.Prelude.:<) x16 x17) st = Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_85(x3)(x5)(x17)(x16)(st)
c_isClosedStringPattern_case_86 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_86(x3)(x5)(x)(st))(i)(xs)(st)
c_isClosedStringPattern_case_86 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedStringPattern_case_86")(x)



c_isClosedStringPattern_case_85 x3 x5 x17 x16 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x16)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_84(x3)(x5)(x17)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_isClosedStringPattern_case_84 x3 x5 x17@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_83(x3)(x5)(x19)(x18)(st)
c_isClosedStringPattern_case_84 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_84(x3)(x5)(x)(st))(i)(xs)(st)
c_isClosedStringPattern_case_84 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedStringPattern_case_84")(x)



c_isClosedStringPattern_case_83 x3 x5 x19 x18 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x18)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_82(x3)(x5)(x19)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_isClosedStringPattern_case_82 x3 x5 x19@Curry.Module.Prelude.List st = Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_81(x3)(x5)(st)
c_isClosedStringPattern_case_82 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_82(x3)(x5)(x)(st))(i)(xs)(st)
c_isClosedStringPattern_case_82 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedStringPattern_case_82")(x)



c_isClosedStringPattern_case_81 x3 x5@((Curry.Module.Prelude.:<) x20 x21) st = Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_80(x3)(x21)(x20)(st)
c_isClosedStringPattern_case_81 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_81(x3)(x)(st))(i)(xs)(st)
c_isClosedStringPattern_case_81 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedStringPattern_case_81")(x)



c_isClosedStringPattern_case_80 x3 x21 x20 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x20)(Curry.Module.Prelude.C_Char(':'))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_79(x3)(x21)(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x20)(Curry.Module.Prelude.C_Char('['))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_75(x3)(x21)(st))(Curry.Module.Prelude.c_failed(st))(st))(st)



c_isClosedStringPattern_case_75 x3 x21@((Curry.Module.Prelude.:<) x26 x27) st = Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_74(x3)(x27)(x26)(st)
c_isClosedStringPattern_case_75 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_75(x3)(x)(st))(i)(xs)(st)
c_isClosedStringPattern_case_75 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedStringPattern_case_75")(x)



c_isClosedStringPattern_case_74 x3 x27 x26 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x26)(Curry.Module.Prelude.C_Char(']'))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_73(x3)(x27)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_isClosedStringPattern_case_73 x3 x27@Curry.Module.Prelude.List st = Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_72(x3)(st)
c_isClosedStringPattern_case_73 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_73(x3)(x)(st))(i)(xs)(st)
c_isClosedStringPattern_case_73 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedStringPattern_case_73")(x)



c_isClosedStringPattern_case_72 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_True
c_isClosedStringPattern_case_72 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_72(x)(st))(i)(xs)(st)
c_isClosedStringPattern_case_72 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedStringPattern_case_72")(x)



c_isClosedStringPattern_case_79 x3 x21@Curry.Module.Prelude.List st = Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_78(x3)(st)
c_isClosedStringPattern_case_79 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_79(x3)(x)(st))(i)(xs)(st)
c_isClosedStringPattern_case_79 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedStringPattern_case_79")(x)



c_isClosedStringPattern_case_78 x3@((Curry.Module.Prelude.:<) x22 x23) st = Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_77(x22)(x23)(st)
c_isClosedStringPattern_case_78 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_78(x)(st))(i)(xs)(st)
c_isClosedStringPattern_case_78 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedStringPattern_case_78")(x)



c_isClosedStringPattern_case_77 x22 x23@((Curry.Module.Prelude.:<) x24 x25) st = Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_76(x22)(x24)(x25)(st)
c_isClosedStringPattern_case_77 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_77(x22)(x)(st))(i)(xs)(st)
c_isClosedStringPattern_case_77 x22 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedStringPattern_case_77")(x)



c_isClosedStringPattern_case_76 x22 x24 x25@Curry.Module.Prelude.List st = Curry.Module.Prelude.op_38_38(Curry.Module.AbstractCurryPrinter.c_isCharPattern(x22)(st))(Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern(x24)(st))(st)
c_isClosedStringPattern_case_76 x22 x24 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedStringPattern_case_76(x22)(x24)(x)(st))(i)(xs)(st)
c_isClosedStringPattern_case_76 x22 x24 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedStringPattern_case_76")(x)



c_isClosedPatternList_case_123 x3 x2@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_122(x3)(x5)(x4)(st)
c_isClosedPatternList_case_123 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_123(x3)(x)(st))(i)(xs)(st)
c_isClosedPatternList_case_123 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedPatternList_case_123")(x)



c_isClosedPatternList_case_122 x3 x5 x4@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_121(x3)(x5)(x7)(x6)(st)
c_isClosedPatternList_case_122 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_122(x3)(x5)(x)(st))(i)(xs)(st)
c_isClosedPatternList_case_122 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedPatternList_case_122")(x)



c_isClosedPatternList_case_121 x3 x5 x7 x6 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('P'))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_120(x3)(x5)(x7)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_isClosedPatternList_case_120 x3 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_119(x3)(x5)(x9)(x8)(st)
c_isClosedPatternList_case_120 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_120(x3)(x5)(x)(st))(i)(xs)(st)
c_isClosedPatternList_case_120 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedPatternList_case_120")(x)



c_isClosedPatternList_case_119 x3 x5 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_118(x3)(x5)(x9)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_isClosedPatternList_case_118 x3 x5 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_117(x3)(x5)(x11)(x10)(st)
c_isClosedPatternList_case_118 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_118(x3)(x5)(x)(st))(i)(xs)(st)
c_isClosedPatternList_case_118 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedPatternList_case_118")(x)



c_isClosedPatternList_case_117 x3 x5 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_116(x3)(x5)(x11)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_isClosedPatternList_case_116 x3 x5 x11@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_115(x3)(x5)(x13)(x12)(st)
c_isClosedPatternList_case_116 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_116(x3)(x5)(x)(st))(i)(xs)(st)
c_isClosedPatternList_case_116 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedPatternList_case_116")(x)



c_isClosedPatternList_case_115 x3 x5 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_114(x3)(x5)(x13)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_isClosedPatternList_case_114 x3 x5 x13@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_113(x3)(x5)(x15)(x14)(st)
c_isClosedPatternList_case_114 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_114(x3)(x5)(x)(st))(i)(xs)(st)
c_isClosedPatternList_case_114 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedPatternList_case_114")(x)



c_isClosedPatternList_case_113 x3 x5 x15 x14 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x14)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_112(x3)(x5)(x15)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_isClosedPatternList_case_112 x3 x5 x15@((Curry.Module.Prelude.:<) x16 x17) st = Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_111(x3)(x5)(x17)(x16)(st)
c_isClosedPatternList_case_112 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_112(x3)(x5)(x)(st))(i)(xs)(st)
c_isClosedPatternList_case_112 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedPatternList_case_112")(x)



c_isClosedPatternList_case_111 x3 x5 x17 x16 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x16)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_110(x3)(x5)(x17)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_isClosedPatternList_case_110 x3 x5 x17@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_109(x3)(x5)(x19)(x18)(st)
c_isClosedPatternList_case_110 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_110(x3)(x5)(x)(st))(i)(xs)(st)
c_isClosedPatternList_case_110 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedPatternList_case_110")(x)



c_isClosedPatternList_case_109 x3 x5 x19 x18 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x18)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_108(x3)(x5)(x19)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_isClosedPatternList_case_108 x3 x5 x19@Curry.Module.Prelude.List st = Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_107(x3)(x5)(st)
c_isClosedPatternList_case_108 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_108(x3)(x5)(x)(st))(i)(xs)(st)
c_isClosedPatternList_case_108 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedPatternList_case_108")(x)



c_isClosedPatternList_case_107 x3 x5@((Curry.Module.Prelude.:<) x20 x21) st = Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_106(x3)(x21)(x20)(st)
c_isClosedPatternList_case_107 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_107(x3)(x)(st))(i)(xs)(st)
c_isClosedPatternList_case_107 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedPatternList_case_107")(x)



c_isClosedPatternList_case_106 x3 x21 x20 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x20)(Curry.Module.Prelude.C_Char(':'))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_105(x3)(x21)(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x20)(Curry.Module.Prelude.C_Char('['))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_101(x3)(x21)(st))(Curry.Module.Prelude.c_failed(st))(st))(st)



c_isClosedPatternList_case_101 x3 x21@((Curry.Module.Prelude.:<) x26 x27) st = Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_100(x3)(x27)(x26)(st)
c_isClosedPatternList_case_101 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_101(x3)(x)(st))(i)(xs)(st)
c_isClosedPatternList_case_101 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedPatternList_case_101")(x)



c_isClosedPatternList_case_100 x3 x27 x26 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x26)(Curry.Module.Prelude.C_Char(']'))(st))(Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_99(x3)(x27)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_isClosedPatternList_case_99 x3 x27@Curry.Module.Prelude.List st = Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_98(x3)(st)
c_isClosedPatternList_case_99 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_99(x3)(x)(st))(i)(xs)(st)
c_isClosedPatternList_case_99 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedPatternList_case_99")(x)



c_isClosedPatternList_case_98 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_True
c_isClosedPatternList_case_98 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_98(x)(st))(i)(xs)(st)
c_isClosedPatternList_case_98 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedPatternList_case_98")(x)



c_isClosedPatternList_case_105 x3 x21@Curry.Module.Prelude.List st = Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_104(x3)(st)
c_isClosedPatternList_case_105 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_105(x3)(x)(st))(i)(xs)(st)
c_isClosedPatternList_case_105 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedPatternList_case_105")(x)



c_isClosedPatternList_case_104 x3@((Curry.Module.Prelude.:<) x22 x23) st = Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_103(x23)(st)
c_isClosedPatternList_case_104 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_104(x)(st))(i)(xs)(st)
c_isClosedPatternList_case_104 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedPatternList_case_104")(x)



c_isClosedPatternList_case_103 x23@((Curry.Module.Prelude.:<) x24 x25) st = Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_102(x24)(x25)(st)
c_isClosedPatternList_case_103 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_103(x)(st))(i)(xs)(st)
c_isClosedPatternList_case_103 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedPatternList_case_103")(x)



c_isClosedPatternList_case_102 x24 x25@Curry.Module.Prelude.List st = Curry.Module.AbstractCurryPrinter.c_isClosedPatternList(x24)(st)
c_isClosedPatternList_case_102 x24 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_isClosedPatternList_case_102(x24)(x)(st))(i)(xs)(st)
c_isClosedPatternList_case_102 x24 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.isClosedPatternList_case_102")(x)



c_showPatListElems_case_149 x3 x2@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_148(x3)(x5)(x4)(st)
c_showPatListElems_case_149 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_149(x3)(x)(st))(i)(xs)(st)
c_showPatListElems_case_149 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatListElems_case_149")(x)



c_showPatListElems_case_148 x3 x5 x4@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_147(x3)(x5)(x7)(x6)(st)
c_showPatListElems_case_148 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_148(x3)(x5)(x)(st))(i)(xs)(st)
c_showPatListElems_case_148 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatListElems_case_148")(x)



c_showPatListElems_case_147 x3 x5 x7 x6 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x6)(Curry.Module.Prelude.C_Char('P'))(st))(Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_146(x3)(x5)(x7)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_showPatListElems_case_146 x3 x5 x7@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_145(x3)(x5)(x9)(x8)(st)
c_showPatListElems_case_146 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_146(x3)(x5)(x)(st))(i)(xs)(st)
c_showPatListElems_case_146 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatListElems_case_146")(x)



c_showPatListElems_case_145 x3 x5 x9 x8 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x8)(Curry.Module.Prelude.C_Char('r'))(st))(Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_144(x3)(x5)(x9)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_showPatListElems_case_144 x3 x5 x9@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_143(x3)(x5)(x11)(x10)(st)
c_showPatListElems_case_144 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_144(x3)(x5)(x)(st))(i)(xs)(st)
c_showPatListElems_case_144 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatListElems_case_144")(x)



c_showPatListElems_case_143 x3 x5 x11 x10 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x10)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_142(x3)(x5)(x11)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_showPatListElems_case_142 x3 x5 x11@((Curry.Module.Prelude.:<) x12 x13) st = Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_141(x3)(x5)(x13)(x12)(st)
c_showPatListElems_case_142 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_142(x3)(x5)(x)(st))(i)(xs)(st)
c_showPatListElems_case_142 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatListElems_case_142")(x)



c_showPatListElems_case_141 x3 x5 x13 x12 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x12)(Curry.Module.Prelude.C_Char('l'))(st))(Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_140(x3)(x5)(x13)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_showPatListElems_case_140 x3 x5 x13@((Curry.Module.Prelude.:<) x14 x15) st = Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_139(x3)(x5)(x15)(x14)(st)
c_showPatListElems_case_140 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_140(x3)(x5)(x)(st))(i)(xs)(st)
c_showPatListElems_case_140 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatListElems_case_140")(x)



c_showPatListElems_case_139 x3 x5 x15 x14 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x14)(Curry.Module.Prelude.C_Char('u'))(st))(Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_138(x3)(x5)(x15)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_showPatListElems_case_138 x3 x5 x15@((Curry.Module.Prelude.:<) x16 x17) st = Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_137(x3)(x5)(x17)(x16)(st)
c_showPatListElems_case_138 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_138(x3)(x5)(x)(st))(i)(xs)(st)
c_showPatListElems_case_138 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatListElems_case_138")(x)



c_showPatListElems_case_137 x3 x5 x17 x16 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x16)(Curry.Module.Prelude.C_Char('d'))(st))(Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_136(x3)(x5)(x17)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_showPatListElems_case_136 x3 x5 x17@((Curry.Module.Prelude.:<) x18 x19) st = Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_135(x3)(x5)(x19)(x18)(st)
c_showPatListElems_case_136 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_136(x3)(x5)(x)(st))(i)(xs)(st)
c_showPatListElems_case_136 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatListElems_case_136")(x)



c_showPatListElems_case_135 x3 x5 x19 x18 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x18)(Curry.Module.Prelude.C_Char('e'))(st))(Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_134(x3)(x5)(x19)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_showPatListElems_case_134 x3 x5 x19@Curry.Module.Prelude.List st = Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_133(x3)(x5)(st)
c_showPatListElems_case_134 x3 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_134(x3)(x5)(x)(st))(i)(xs)(st)
c_showPatListElems_case_134 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatListElems_case_134")(x)



c_showPatListElems_case_133 x3 x5@((Curry.Module.Prelude.:<) x20 x21) st = Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_132(x3)(x21)(x20)(st)
c_showPatListElems_case_133 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_133(x3)(x)(st))(i)(xs)(st)
c_showPatListElems_case_133 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatListElems_case_133")(x)



c_showPatListElems_case_132 x3 x21 x20 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x20)(Curry.Module.Prelude.C_Char(':'))(st))(Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_131(x3)(x21)(st))(Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x20)(Curry.Module.Prelude.C_Char('['))(st))(Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_127(x3)(x21)(st))(Curry.Module.Prelude.c_failed(st))(st))(st)



c_showPatListElems_case_127 x3 x21@((Curry.Module.Prelude.:<) x26 x27) st = Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_126(x3)(x27)(x26)(st)
c_showPatListElems_case_127 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_127(x3)(x)(st))(i)(xs)(st)
c_showPatListElems_case_127 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatListElems_case_127")(x)



c_showPatListElems_case_126 x3 x27 x26 st = Curry.Module.Prelude.c_if_then_else(Curry.Module.Prelude.op_61_61_61(x26)(Curry.Module.Prelude.C_Char(']'))(st))(Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_125(x3)(x27)(st))(Curry.Module.Prelude.c_failed(st))(st)



c_showPatListElems_case_125 x3 x27@Curry.Module.Prelude.List st = Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_124(x3)(st)
c_showPatListElems_case_125 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_125(x3)(x)(st))(i)(xs)(st)
c_showPatListElems_case_125 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatListElems_case_125")(x)



c_showPatListElems_case_124 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_showPatListElems_case_124 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_124(x)(st))(i)(xs)(st)
c_showPatListElems_case_124 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatListElems_case_124")(x)



c_showPatListElems_case_131 x3 x21@Curry.Module.Prelude.List st = Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_130(x3)(st)
c_showPatListElems_case_131 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_131(x3)(x)(st))(i)(xs)(st)
c_showPatListElems_case_131 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatListElems_case_131")(x)



c_showPatListElems_case_130 x3@((Curry.Module.Prelude.:<) x22 x23) st = Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_129(x22)(x23)(st)
c_showPatListElems_case_130 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_130(x)(st))(i)(xs)(st)
c_showPatListElems_case_130 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatListElems_case_130")(x)



c_showPatListElems_case_129 x22 x23@((Curry.Module.Prelude.:<) x24 x25) st = Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_128(x22)(x24)(x25)(st)
c_showPatListElems_case_129 x22 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_129(x22)(x)(st))(i)(xs)(st)
c_showPatListElems_case_129 x22 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatListElems_case_129")(x)



c_showPatListElems_case_128 x22 x24 x25@Curry.Module.Prelude.List st = (Curry.Module.Prelude.:<)(Curry.Module.AbstractCurryPrinter.c_showPattern(x22)(st))(Curry.Module.AbstractCurryPrinter.c_showPatListElems(x24)(st))
c_showPatListElems_case_128 x22 x24 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatListElems_case_128(x22)(x24)(x)(st))(i)(xs)(st)
c_showPatListElems_case_128 x22 x24 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatListElems_case_128")(x)



c_showPatternList_case_153 x1 x2@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_47_61))(Curry.Module.Prelude.C_Char('\''))))(Curry.Module.Prelude.c_concat(Curry.Module.AbstractCurryPrinter.c_showPatListElems(x1)(st))(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\"'))(Curry.Module.Prelude.List))(st))
c_showPatternList_case_153 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showPatternList_case_152(x1)(Curry.Module.AbstractCurryPrinter.c_isClosedPatternList(x1)(st))(st)
c_showPatternList_case_153 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatternList_case_153(x1)(x)(st))(i)(xs)(st)
c_showPatternList_case_153 x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatternList_case_153")(x)



c_showPatternList_case_152 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('['))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_concat(Curry.Module.List.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showPatListElems(x1)(st))(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(']'))(Curry.Module.Prelude.List))(st))(st)
c_showPatternList_case_152 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showPatternList_case_151(x1)(Curry.Module.AbstractCurryPrinter.c_isAsPattern(x1)(st))(st)
c_showPatternList_case_152 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatternList_case_152(x1)(x)(st))(i)(xs)(st)
c_showPatternList_case_152 x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatternList_case_152")(x)



c_showPatternList_case_151 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurryPrinter.c_showAsPatternList(x1)(st)
c_showPatternList_case_151 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showPatternList_case_150(x1)(Curry.Module.Prelude.c_otherwise(st))(st)
c_showPatternList_case_151 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatternList_case_151(x1)(x)(st))(i)(xs)(st)
c_showPatternList_case_151 x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatternList_case_151")(x)



c_showPatternList_case_150 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_concat(Curry.Module.List.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showPatListElems(x1)(st))(st))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st)
c_showPatternList_case_150 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPatternList_case_150(x1)(x)(st))(i)(xs)(st)
c_showPatternList_case_150 x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPatternList_case_150")(x)



c_showPreludeCons'46_'35selFP7'35pattlist_case_154 x3 x2@(Curry.Module.Prelude.T2 x4 x5) st = x3
c_showPreludeCons'46_'35selFP7'35pattlist_case_154 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPreludeCons'46_'35selFP7'35pattlist_case_154(x3)(x)(st))(i)(xs)(st)
c_showPreludeCons'46_'35selFP7'35pattlist_case_154 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPreludeCons._#selFP7#pattlist_case_154")(x)



c_showPreludeCons'46_'35selFP6'35name_case_155 x2@(Curry.Module.Prelude.T2 x4 x5) st = x5
c_showPreludeCons'46_'35selFP6'35name_case_155 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPreludeCons'46_'35selFP6'35name_case_155(x)(st))(i)(xs)(st)
c_showPreludeCons'46_'35selFP6'35name_case_155 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPreludeCons._#selFP6#name_case_155")(x)



c_showPreludeCons_case_158 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurryPrinter.c_showPatternList(x1)(st)
c_showPreludeCons_case_158 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showPreludeCons_case_157(x3)(x4)(Curry.Module.AbstractCurryPrinter.c_isTuple(x3)(st))(st)
c_showPreludeCons_case_158 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPreludeCons_case_158(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c_showPreludeCons_case_158 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPreludeCons_case_158")(x)



c_showPreludeCons_case_157 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showPattern))(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))(Curry.Module.Prelude.List))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st)
c_showPreludeCons_case_157 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showPreludeCons_case_156(x3)(x4)(Curry.Module.Prelude.c_otherwise(st))(st)
c_showPreludeCons_case_157 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPreludeCons_case_157(x3)(x4)(x)(st))(i)(xs)(st)
c_showPreludeCons_case_157 x3 x4 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPreludeCons_case_157")(x)



c_showPreludeCons_case_156 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x3)(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showPattern))(x4)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st))(st)
c_showPreludeCons_case_156 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPreludeCons_case_156(x3)(x4)(x)(st))(i)(xs)(st)
c_showPreludeCons_case_156 x3 x4 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPreludeCons_case_156")(x)



c_showPattern_case_159 x13 x12@(Curry.Module.Prelude.T2 x14 x15) st = Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_showIdentifier(st))(x15)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('@'))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showPattern(x13)(st))(st))(st)
c_showPattern_case_159 x13 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPattern_case_159(x13)(x)(st))(i)(xs)(st)
c_showPattern_case_159 x13 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPattern_case_159")(x)



c_showPattern_case_163 x7 x6@(Curry.Module.Prelude.T2 x8 x9) st = Curry.Module.AbstractCurryPrinter.c_showPattern_case_162(x8)(x9)(x7)(st)
c_showPattern_case_163 x7 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPattern_case_163(x7)(x)(st))(i)(xs)(st)
c_showPattern_case_163 x7 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPattern_case_163")(x)



c_showPattern_case_162 x8 x9 x7@Curry.Module.Prelude.List st = x9
c_showPattern_case_162 x8 x9 x7@((Curry.Module.Prelude.:<) x10 x11) st = Curry.Module.AbstractCurryPrinter.c_showPattern_case_161(x8)(x9)(x10)(x11)(Curry.Module.Prelude.op_61_61(x8)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(st))(st)
c_showPattern_case_162 x8 x9 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPattern_case_162(x8)(x9)(x)(st))(i)(xs)(st)
c_showPattern_case_162 x8 x9 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPattern_case_162")(x)



c_showPattern_case_161 x8 x9 x10 x11 x12@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurryPrinter.c_showPreludeCons(Curry.Module.AbstractCurry.C_CPComb(Curry.Module.Prelude.T2(x8)(x9))((Curry.Module.Prelude.:<)(x10)(x11)))(st)
c_showPattern_case_161 x8 x9 x10 x11 x12@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showPattern_case_160(x9)(x10)(x11)(Curry.Module.Prelude.c_otherwise(st))(st)
c_showPattern_case_161 x8 x9 x10 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPattern_case_161(x8)(x9)(x10)(x11)(x)(st))(i)(xs)(st)
c_showPattern_case_161 x8 x9 x10 x11 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPattern_case_161")(x)



c_showPattern_case_160 x9 x10 x11 x12@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x9)(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showPattern))((Curry.Module.Prelude.:<)(x10)(x11))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st))(st)
c_showPattern_case_160 x9 x10 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPattern_case_160(x9)(x10)(x11)(x)(st))(i)(xs)(st)
c_showPattern_case_160 x9 x10 x11 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPattern_case_160")(x)



c_showPattern_case_164 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_showIdentifier(st))(x4)(st)
c_showPattern_case_164 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showPattern_case_164(x)(st))(i)(xs)(st)
c_showPattern_case_164 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showPattern_case_164")(x)



c_showStatement_case_166 x1 x6@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.AbstractCurryPrinter.c_showStatement_case_165(x1)(x6)(x7)(x8)(st)
c_showStatement_case_166 x1 x6@Curry.Module.Prelude.List st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))(Curry.Module.AbstractCurryPrinter.c_showBlock(Curry.Module.AbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showLocalDecl(x1)))(x6)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(st))(st))(st)
c_showStatement_case_166 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showStatement_case_166(x1)(x)(st))(i)(xs)(st)
c_showStatement_case_166 x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showStatement_case_166")(x)



c_showStatement_case_165 x1 x6 x7 x8@Curry.Module.Prelude.List st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.AbstractCurryPrinter.c_showLocalDecl(x1)(x7)(st))(st)
c_showStatement_case_165 x1 x6 x7 x8@((Curry.Module.Prelude.:<) x9 x10) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))(Curry.Module.AbstractCurryPrinter.c_showBlock(Curry.Module.AbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showLocalDecl(x1)))(x6)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(st))(st))(st)
c_showStatement_case_165 x1 x6 x7 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showStatement_case_165(x1)(x6)(x7)(x)(st))(i)(xs)(st)
c_showStatement_case_165 x1 x6 x7 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showStatement_case_165")(x)



c_showLambdaOrSection_case_192 x1 x2 x3 x5 x4@(Curry.Module.AbstractCurry.C_CPVar x6) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_191(x1)(x2)(x3)(x6)(x5)(st)
c_showLambdaOrSection_case_192 x1 x2 x3 x5 x4@(Curry.Module.AbstractCurry.C_CPLit x179) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_192 x1 x2 x3 x5 x4@(Curry.Module.AbstractCurry.C_CPComb x180 x181) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_192 x1 x2 x3 x5 x4@(Curry.Module.AbstractCurry.C_CPAs x182 x183) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_192 x1 x2 x3 x5 x4@(Curry.Module.AbstractCurry.C_CPFuncComb x184 x185) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_192 x1 x2 x3 x5 (Curry.Module.AbstractCurry.C_CPatternOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_192(x1)(x2)(x3)(x5)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_192 x1 x2 x3 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_192")(x)



c_showLambdaOrSection_case_191 x1 x2 x3 x6 x5@Curry.Module.Prelude.List st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_190(x1)(x2)(x6)(x3)(st)
c_showLambdaOrSection_case_191 x1 x2 x3 x6 x5@((Curry.Module.Prelude.:<) x177 x178) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_191 x1 x2 x3 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_191(x1)(x2)(x3)(x6)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_191 x1 x2 x3 x6 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_191")(x)



c_showLambdaOrSection_case_190 x1 x2 x6 x3@(Curry.Module.AbstractCurry.C_CApply x7 x8) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_189(x1)(x2)(x3)(x6)(x8)(x7)(st)
c_showLambdaOrSection_case_190 x1 x2 x6 x3@(Curry.Module.AbstractCurry.C_CVar x165) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_190 x1 x2 x6 x3@(Curry.Module.AbstractCurry.C_CLit x166) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_190 x1 x2 x6 x3@(Curry.Module.AbstractCurry.C_CSymbol x167) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_190 x1 x2 x6 x3@(Curry.Module.AbstractCurry.C_CLambda x168 x169) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_190 x1 x2 x6 x3@(Curry.Module.AbstractCurry.C_CLetDecl x170 x171) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_190 x1 x2 x6 x3@(Curry.Module.AbstractCurry.C_CDoExpr x172) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_190 x1 x2 x6 x3@(Curry.Module.AbstractCurry.C_CListComp x173 x174) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_190 x1 x2 x6 x3@(Curry.Module.AbstractCurry.C_CCase x175 x176) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_190 x1 x2 x6 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_190(x1)(x2)(x6)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_190 x1 x2 x6 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_190")(x)



c_showLambdaOrSection_case_189 x1 x2 x3 x6 x8 x7@(Curry.Module.AbstractCurry.C_CApply x9 x10) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_188(x1)(x2)(x3)(x6)(x8)(x10)(x9)(st)
c_showLambdaOrSection_case_189 x1 x2 x3 x6 x8 x7@(Curry.Module.AbstractCurry.C_CVar x153) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_189 x1 x2 x3 x6 x8 x7@(Curry.Module.AbstractCurry.C_CLit x154) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_189 x1 x2 x3 x6 x8 x7@(Curry.Module.AbstractCurry.C_CSymbol x155) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_189 x1 x2 x3 x6 x8 x7@(Curry.Module.AbstractCurry.C_CLambda x156 x157) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_189 x1 x2 x3 x6 x8 x7@(Curry.Module.AbstractCurry.C_CLetDecl x158 x159) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_189 x1 x2 x3 x6 x8 x7@(Curry.Module.AbstractCurry.C_CDoExpr x160) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_189 x1 x2 x3 x6 x8 x7@(Curry.Module.AbstractCurry.C_CListComp x161 x162) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_189 x1 x2 x3 x6 x8 x7@(Curry.Module.AbstractCurry.C_CCase x163 x164) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_189 x1 x2 x3 x6 x8 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_189(x1)(x2)(x3)(x6)(x8)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_189 x1 x2 x3 x6 x8 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_189")(x)



c_showLambdaOrSection_case_188 x1 x2 x3 x6 x8 x10 x9@(Curry.Module.AbstractCurry.C_CSymbol x11) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_187(x1)(x2)(x3)(x6)(x8)(x10)(x11)(st)
c_showLambdaOrSection_case_188 x1 x2 x3 x6 x8 x10 x9@(Curry.Module.AbstractCurry.C_CVar x140) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_188 x1 x2 x3 x6 x8 x10 x9@(Curry.Module.AbstractCurry.C_CLit x141) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_188 x1 x2 x3 x6 x8 x10 x9@(Curry.Module.AbstractCurry.C_CApply x142 x143) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_188 x1 x2 x3 x6 x8 x10 x9@(Curry.Module.AbstractCurry.C_CLambda x144 x145) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_188 x1 x2 x3 x6 x8 x10 x9@(Curry.Module.AbstractCurry.C_CLetDecl x146 x147) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_188 x1 x2 x3 x6 x8 x10 x9@(Curry.Module.AbstractCurry.C_CDoExpr x148) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_188 x1 x2 x3 x6 x8 x10 x9@(Curry.Module.AbstractCurry.C_CListComp x149 x150) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_188 x1 x2 x3 x6 x8 x10 x9@(Curry.Module.AbstractCurry.C_CCase x151 x152) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_188 x1 x2 x3 x6 x8 x10 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_188(x1)(x2)(x3)(x6)(x8)(x10)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_188 x1 x2 x3 x6 x8 x10 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_188")(x)



c_showLambdaOrSection_case_187 x1 x2 x3 x6 x8 x10 x11@(Curry.Module.Prelude.T2 x12 x13) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_186(x1)(x2)(x3)(x6)(x10)(x13)(x8)(st)
c_showLambdaOrSection_case_187 x1 x2 x3 x6 x8 x10 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_187(x1)(x2)(x3)(x6)(x8)(x10)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_187 x1 x2 x3 x6 x8 x10 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_187")(x)



c_showLambdaOrSection_case_186 x1 x2 x3 x6 x10 x13 x8@(Curry.Module.AbstractCurry.C_CVar x14) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_185(x1)(x2)(x3)(x6)(x10)(x13)(x14)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_isInfixOpName(st))(x13)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.AbstractCurryPrinter.c_isAtom(x10)(st))(Curry.Module.Prelude.op_47_61(Curry.Module.AbstractCurry.C_CVar(x14))(x10)(st))(st))(st))(st)
c_showLambdaOrSection_case_186 x1 x2 x3 x6 x10 x13 x8@(Curry.Module.AbstractCurry.C_CLit x15) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_182(x1)(x2)(x3)(x6)(x13)(x15)(x10)(st)
c_showLambdaOrSection_case_186 x1 x2 x3 x6 x10 x13 x8@(Curry.Module.AbstractCurry.C_CSymbol x30) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_180(x1)(x2)(x3)(x6)(x13)(x30)(x10)(st)
c_showLambdaOrSection_case_186 x1 x2 x3 x6 x10 x13 x8@(Curry.Module.AbstractCurry.C_CApply x45 x46) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_178(x1)(x2)(x3)(x6)(x13)(x45)(x46)(x10)(st)
c_showLambdaOrSection_case_186 x1 x2 x3 x6 x10 x13 x8@(Curry.Module.AbstractCurry.C_CLambda x61 x62) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_176(x1)(x2)(x3)(x6)(x13)(x61)(x62)(x10)(st)
c_showLambdaOrSection_case_186 x1 x2 x3 x6 x10 x13 x8@(Curry.Module.AbstractCurry.C_CLetDecl x77 x78) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_174(x1)(x2)(x3)(x6)(x13)(x77)(x78)(x10)(st)
c_showLambdaOrSection_case_186 x1 x2 x3 x6 x10 x13 x8@(Curry.Module.AbstractCurry.C_CDoExpr x93) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_172(x1)(x2)(x3)(x6)(x13)(x93)(x10)(st)
c_showLambdaOrSection_case_186 x1 x2 x3 x6 x10 x13 x8@(Curry.Module.AbstractCurry.C_CListComp x108 x109) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_170(x1)(x2)(x3)(x6)(x13)(x108)(x109)(x10)(st)
c_showLambdaOrSection_case_186 x1 x2 x3 x6 x10 x13 x8@(Curry.Module.AbstractCurry.C_CCase x124 x125) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_168(x1)(x2)(x3)(x6)(x13)(x124)(x125)(x10)(st)
c_showLambdaOrSection_case_186 x1 x2 x3 x6 x10 x13 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_186(x1)(x2)(x3)(x6)(x10)(x13)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_186 x1 x2 x3 x6 x10 x13 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_186")(x)



c_showLambdaOrSection_case_168 x1 x2 x3 x6 x13 x124 x125 x10@(Curry.Module.AbstractCurry.C_CVar x126) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_167(x1)(x2)(x3)(x6)(x13)(x124)(x125)(x126)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_isInfixOpName(st))(x13)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x6)(x126)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.AbstractCurryPrinter.c_isAtom(Curry.Module.AbstractCurry.C_CCase(x124)(x125))(st))(Curry.Module.Prelude.op_47_61(Curry.Module.AbstractCurry.C_CVar(x126))(Curry.Module.AbstractCurry.C_CCase(x124)(x125))(st))(st))(st))(st))(st)
c_showLambdaOrSection_case_168 x1 x2 x3 x6 x13 x124 x125 x10@(Curry.Module.AbstractCurry.C_CLit x127) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_168 x1 x2 x3 x6 x13 x124 x125 x10@(Curry.Module.AbstractCurry.C_CSymbol x128) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_168 x1 x2 x3 x6 x13 x124 x125 x10@(Curry.Module.AbstractCurry.C_CApply x129 x130) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_168 x1 x2 x3 x6 x13 x124 x125 x10@(Curry.Module.AbstractCurry.C_CLambda x131 x132) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_168 x1 x2 x3 x6 x13 x124 x125 x10@(Curry.Module.AbstractCurry.C_CLetDecl x133 x134) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_168 x1 x2 x3 x6 x13 x124 x125 x10@(Curry.Module.AbstractCurry.C_CDoExpr x135) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_168 x1 x2 x3 x6 x13 x124 x125 x10@(Curry.Module.AbstractCurry.C_CListComp x136 x137) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_168 x1 x2 x3 x6 x13 x124 x125 x10@(Curry.Module.AbstractCurry.C_CCase x138 x139) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_168 x1 x2 x3 x6 x13 x124 x125 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_168(x1)(x2)(x3)(x6)(x13)(x124)(x125)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_168 x1 x2 x3 x6 x13 x124 x125 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_168")(x)



c_showLambdaOrSection_case_167 x1 x2 x3 x6 x13 x124 x125 x126 x127@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x13)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(Curry.Module.AbstractCurry.C_CCase(x124)(x125))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st))(st))(st)
c_showLambdaOrSection_case_167 x1 x2 x3 x6 x13 x124 x125 x126 x127@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_167 x1 x2 x3 x6 x13 x124 x125 x126 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_167(x1)(x2)(x3)(x6)(x13)(x124)(x125)(x126)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_167 x1 x2 x3 x6 x13 x124 x125 x126 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_167")(x)



c_showLambdaOrSection_case_170 x1 x2 x3 x6 x13 x108 x109 x10@(Curry.Module.AbstractCurry.C_CVar x110) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_169(x1)(x2)(x3)(x6)(x13)(x108)(x109)(x110)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_isInfixOpName(st))(x13)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x6)(x110)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.AbstractCurryPrinter.c_isAtom(Curry.Module.AbstractCurry.C_CListComp(x108)(x109))(st))(Curry.Module.Prelude.op_47_61(Curry.Module.AbstractCurry.C_CVar(x110))(Curry.Module.AbstractCurry.C_CListComp(x108)(x109))(st))(st))(st))(st))(st)
c_showLambdaOrSection_case_170 x1 x2 x3 x6 x13 x108 x109 x10@(Curry.Module.AbstractCurry.C_CLit x111) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_170 x1 x2 x3 x6 x13 x108 x109 x10@(Curry.Module.AbstractCurry.C_CSymbol x112) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_170 x1 x2 x3 x6 x13 x108 x109 x10@(Curry.Module.AbstractCurry.C_CApply x113 x114) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_170 x1 x2 x3 x6 x13 x108 x109 x10@(Curry.Module.AbstractCurry.C_CLambda x115 x116) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_170 x1 x2 x3 x6 x13 x108 x109 x10@(Curry.Module.AbstractCurry.C_CLetDecl x117 x118) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_170 x1 x2 x3 x6 x13 x108 x109 x10@(Curry.Module.AbstractCurry.C_CDoExpr x119) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_170 x1 x2 x3 x6 x13 x108 x109 x10@(Curry.Module.AbstractCurry.C_CListComp x120 x121) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_170 x1 x2 x3 x6 x13 x108 x109 x10@(Curry.Module.AbstractCurry.C_CCase x122 x123) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_170 x1 x2 x3 x6 x13 x108 x109 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_170(x1)(x2)(x3)(x6)(x13)(x108)(x109)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_170 x1 x2 x3 x6 x13 x108 x109 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_170")(x)



c_showLambdaOrSection_case_169 x1 x2 x3 x6 x13 x108 x109 x110 x111@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x13)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(Curry.Module.AbstractCurry.C_CListComp(x108)(x109))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st))(st))(st)
c_showLambdaOrSection_case_169 x1 x2 x3 x6 x13 x108 x109 x110 x111@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_169 x1 x2 x3 x6 x13 x108 x109 x110 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_169(x1)(x2)(x3)(x6)(x13)(x108)(x109)(x110)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_169 x1 x2 x3 x6 x13 x108 x109 x110 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_169")(x)



c_showLambdaOrSection_case_172 x1 x2 x3 x6 x13 x93 x10@(Curry.Module.AbstractCurry.C_CVar x94) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_171(x1)(x2)(x3)(x6)(x13)(x93)(x94)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_isInfixOpName(st))(x13)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x6)(x94)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.AbstractCurryPrinter.c_isAtom(Curry.Module.AbstractCurry.C_CDoExpr(x93))(st))(Curry.Module.Prelude.op_47_61(Curry.Module.AbstractCurry.C_CVar(x94))(Curry.Module.AbstractCurry.C_CDoExpr(x93))(st))(st))(st))(st))(st)
c_showLambdaOrSection_case_172 x1 x2 x3 x6 x13 x93 x10@(Curry.Module.AbstractCurry.C_CLit x95) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_172 x1 x2 x3 x6 x13 x93 x10@(Curry.Module.AbstractCurry.C_CSymbol x96) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_172 x1 x2 x3 x6 x13 x93 x10@(Curry.Module.AbstractCurry.C_CApply x97 x98) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_172 x1 x2 x3 x6 x13 x93 x10@(Curry.Module.AbstractCurry.C_CLambda x99 x100) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_172 x1 x2 x3 x6 x13 x93 x10@(Curry.Module.AbstractCurry.C_CLetDecl x101 x102) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_172 x1 x2 x3 x6 x13 x93 x10@(Curry.Module.AbstractCurry.C_CDoExpr x103) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_172 x1 x2 x3 x6 x13 x93 x10@(Curry.Module.AbstractCurry.C_CListComp x104 x105) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_172 x1 x2 x3 x6 x13 x93 x10@(Curry.Module.AbstractCurry.C_CCase x106 x107) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_172 x1 x2 x3 x6 x13 x93 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_172(x1)(x2)(x3)(x6)(x13)(x93)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_172 x1 x2 x3 x6 x13 x93 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_172")(x)



c_showLambdaOrSection_case_171 x1 x2 x3 x6 x13 x93 x94 x95@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x13)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(Curry.Module.AbstractCurry.C_CDoExpr(x93))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st))(st))(st)
c_showLambdaOrSection_case_171 x1 x2 x3 x6 x13 x93 x94 x95@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_171 x1 x2 x3 x6 x13 x93 x94 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_171(x1)(x2)(x3)(x6)(x13)(x93)(x94)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_171 x1 x2 x3 x6 x13 x93 x94 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_171")(x)



c_showLambdaOrSection_case_174 x1 x2 x3 x6 x13 x77 x78 x10@(Curry.Module.AbstractCurry.C_CVar x79) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_173(x1)(x2)(x3)(x6)(x13)(x77)(x78)(x79)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_isInfixOpName(st))(x13)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x6)(x79)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.AbstractCurryPrinter.c_isAtom(Curry.Module.AbstractCurry.C_CLetDecl(x77)(x78))(st))(Curry.Module.Prelude.op_47_61(Curry.Module.AbstractCurry.C_CVar(x79))(Curry.Module.AbstractCurry.C_CLetDecl(x77)(x78))(st))(st))(st))(st))(st)
c_showLambdaOrSection_case_174 x1 x2 x3 x6 x13 x77 x78 x10@(Curry.Module.AbstractCurry.C_CLit x80) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_174 x1 x2 x3 x6 x13 x77 x78 x10@(Curry.Module.AbstractCurry.C_CSymbol x81) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_174 x1 x2 x3 x6 x13 x77 x78 x10@(Curry.Module.AbstractCurry.C_CApply x82 x83) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_174 x1 x2 x3 x6 x13 x77 x78 x10@(Curry.Module.AbstractCurry.C_CLambda x84 x85) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_174 x1 x2 x3 x6 x13 x77 x78 x10@(Curry.Module.AbstractCurry.C_CLetDecl x86 x87) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_174 x1 x2 x3 x6 x13 x77 x78 x10@(Curry.Module.AbstractCurry.C_CDoExpr x88) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_174 x1 x2 x3 x6 x13 x77 x78 x10@(Curry.Module.AbstractCurry.C_CListComp x89 x90) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_174 x1 x2 x3 x6 x13 x77 x78 x10@(Curry.Module.AbstractCurry.C_CCase x91 x92) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_174 x1 x2 x3 x6 x13 x77 x78 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_174(x1)(x2)(x3)(x6)(x13)(x77)(x78)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_174 x1 x2 x3 x6 x13 x77 x78 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_174")(x)



c_showLambdaOrSection_case_173 x1 x2 x3 x6 x13 x77 x78 x79 x80@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x13)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(Curry.Module.AbstractCurry.C_CLetDecl(x77)(x78))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st))(st))(st)
c_showLambdaOrSection_case_173 x1 x2 x3 x6 x13 x77 x78 x79 x80@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_173 x1 x2 x3 x6 x13 x77 x78 x79 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_173(x1)(x2)(x3)(x6)(x13)(x77)(x78)(x79)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_173 x1 x2 x3 x6 x13 x77 x78 x79 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_173")(x)



c_showLambdaOrSection_case_176 x1 x2 x3 x6 x13 x61 x62 x10@(Curry.Module.AbstractCurry.C_CVar x63) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_175(x1)(x2)(x3)(x6)(x13)(x61)(x62)(x63)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_isInfixOpName(st))(x13)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x6)(x63)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.AbstractCurryPrinter.c_isAtom(Curry.Module.AbstractCurry.C_CLambda(x61)(x62))(st))(Curry.Module.Prelude.op_47_61(Curry.Module.AbstractCurry.C_CVar(x63))(Curry.Module.AbstractCurry.C_CLambda(x61)(x62))(st))(st))(st))(st))(st)
c_showLambdaOrSection_case_176 x1 x2 x3 x6 x13 x61 x62 x10@(Curry.Module.AbstractCurry.C_CLit x64) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_176 x1 x2 x3 x6 x13 x61 x62 x10@(Curry.Module.AbstractCurry.C_CSymbol x65) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_176 x1 x2 x3 x6 x13 x61 x62 x10@(Curry.Module.AbstractCurry.C_CApply x66 x67) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_176 x1 x2 x3 x6 x13 x61 x62 x10@(Curry.Module.AbstractCurry.C_CLambda x68 x69) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_176 x1 x2 x3 x6 x13 x61 x62 x10@(Curry.Module.AbstractCurry.C_CLetDecl x70 x71) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_176 x1 x2 x3 x6 x13 x61 x62 x10@(Curry.Module.AbstractCurry.C_CDoExpr x72) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_176 x1 x2 x3 x6 x13 x61 x62 x10@(Curry.Module.AbstractCurry.C_CListComp x73 x74) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_176 x1 x2 x3 x6 x13 x61 x62 x10@(Curry.Module.AbstractCurry.C_CCase x75 x76) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_176 x1 x2 x3 x6 x13 x61 x62 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_176(x1)(x2)(x3)(x6)(x13)(x61)(x62)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_176 x1 x2 x3 x6 x13 x61 x62 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_176")(x)



c_showLambdaOrSection_case_175 x1 x2 x3 x6 x13 x61 x62 x63 x64@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x13)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(Curry.Module.AbstractCurry.C_CLambda(x61)(x62))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st))(st))(st)
c_showLambdaOrSection_case_175 x1 x2 x3 x6 x13 x61 x62 x63 x64@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_175 x1 x2 x3 x6 x13 x61 x62 x63 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_175(x1)(x2)(x3)(x6)(x13)(x61)(x62)(x63)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_175 x1 x2 x3 x6 x13 x61 x62 x63 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_175")(x)



c_showLambdaOrSection_case_178 x1 x2 x3 x6 x13 x45 x46 x10@(Curry.Module.AbstractCurry.C_CVar x47) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_177(x1)(x2)(x3)(x6)(x13)(x45)(x46)(x47)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_isInfixOpName(st))(x13)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x6)(x47)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.AbstractCurryPrinter.c_isAtom(Curry.Module.AbstractCurry.C_CApply(x45)(x46))(st))(Curry.Module.Prelude.op_47_61(Curry.Module.AbstractCurry.C_CVar(x47))(Curry.Module.AbstractCurry.C_CApply(x45)(x46))(st))(st))(st))(st))(st)
c_showLambdaOrSection_case_178 x1 x2 x3 x6 x13 x45 x46 x10@(Curry.Module.AbstractCurry.C_CLit x48) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_178 x1 x2 x3 x6 x13 x45 x46 x10@(Curry.Module.AbstractCurry.C_CSymbol x49) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_178 x1 x2 x3 x6 x13 x45 x46 x10@(Curry.Module.AbstractCurry.C_CApply x50 x51) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_178 x1 x2 x3 x6 x13 x45 x46 x10@(Curry.Module.AbstractCurry.C_CLambda x52 x53) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_178 x1 x2 x3 x6 x13 x45 x46 x10@(Curry.Module.AbstractCurry.C_CLetDecl x54 x55) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_178 x1 x2 x3 x6 x13 x45 x46 x10@(Curry.Module.AbstractCurry.C_CDoExpr x56) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_178 x1 x2 x3 x6 x13 x45 x46 x10@(Curry.Module.AbstractCurry.C_CListComp x57 x58) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_178 x1 x2 x3 x6 x13 x45 x46 x10@(Curry.Module.AbstractCurry.C_CCase x59 x60) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_178 x1 x2 x3 x6 x13 x45 x46 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_178(x1)(x2)(x3)(x6)(x13)(x45)(x46)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_178 x1 x2 x3 x6 x13 x45 x46 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_178")(x)



c_showLambdaOrSection_case_177 x1 x2 x3 x6 x13 x45 x46 x47 x48@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x13)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(Curry.Module.AbstractCurry.C_CApply(x45)(x46))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st))(st))(st)
c_showLambdaOrSection_case_177 x1 x2 x3 x6 x13 x45 x46 x47 x48@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_177 x1 x2 x3 x6 x13 x45 x46 x47 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_177(x1)(x2)(x3)(x6)(x13)(x45)(x46)(x47)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_177 x1 x2 x3 x6 x13 x45 x46 x47 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_177")(x)



c_showLambdaOrSection_case_180 x1 x2 x3 x6 x13 x30 x10@(Curry.Module.AbstractCurry.C_CVar x31) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_179(x1)(x2)(x3)(x6)(x13)(x30)(x31)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_isInfixOpName(st))(x13)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x6)(x31)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.AbstractCurryPrinter.c_isAtom(Curry.Module.AbstractCurry.C_CSymbol(x30))(st))(Curry.Module.Prelude.op_47_61(Curry.Module.AbstractCurry.C_CVar(x31))(Curry.Module.AbstractCurry.C_CSymbol(x30))(st))(st))(st))(st))(st)
c_showLambdaOrSection_case_180 x1 x2 x3 x6 x13 x30 x10@(Curry.Module.AbstractCurry.C_CLit x32) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_180 x1 x2 x3 x6 x13 x30 x10@(Curry.Module.AbstractCurry.C_CSymbol x33) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_180 x1 x2 x3 x6 x13 x30 x10@(Curry.Module.AbstractCurry.C_CApply x34 x35) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_180 x1 x2 x3 x6 x13 x30 x10@(Curry.Module.AbstractCurry.C_CLambda x36 x37) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_180 x1 x2 x3 x6 x13 x30 x10@(Curry.Module.AbstractCurry.C_CLetDecl x38 x39) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_180 x1 x2 x3 x6 x13 x30 x10@(Curry.Module.AbstractCurry.C_CDoExpr x40) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_180 x1 x2 x3 x6 x13 x30 x10@(Curry.Module.AbstractCurry.C_CListComp x41 x42) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_180 x1 x2 x3 x6 x13 x30 x10@(Curry.Module.AbstractCurry.C_CCase x43 x44) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_180 x1 x2 x3 x6 x13 x30 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_180(x1)(x2)(x3)(x6)(x13)(x30)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_180 x1 x2 x3 x6 x13 x30 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_180")(x)



c_showLambdaOrSection_case_179 x1 x2 x3 x6 x13 x30 x31 x32@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x13)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(Curry.Module.AbstractCurry.C_CSymbol(x30))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st))(st))(st)
c_showLambdaOrSection_case_179 x1 x2 x3 x6 x13 x30 x31 x32@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_179 x1 x2 x3 x6 x13 x30 x31 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_179(x1)(x2)(x3)(x6)(x13)(x30)(x31)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_179 x1 x2 x3 x6 x13 x30 x31 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_179")(x)



c_showLambdaOrSection_case_182 x1 x2 x3 x6 x13 x15 x10@(Curry.Module.AbstractCurry.C_CVar x16) st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_181(x1)(x2)(x3)(x6)(x13)(x15)(x16)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_isInfixOpName(st))(x13)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x6)(x16)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.AbstractCurryPrinter.c_isAtom(Curry.Module.AbstractCurry.C_CLit(x15))(st))(Curry.Module.Prelude.op_47_61(Curry.Module.AbstractCurry.C_CVar(x16))(Curry.Module.AbstractCurry.C_CLit(x15))(st))(st))(st))(st))(st)
c_showLambdaOrSection_case_182 x1 x2 x3 x6 x13 x15 x10@(Curry.Module.AbstractCurry.C_CLit x17) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_182 x1 x2 x3 x6 x13 x15 x10@(Curry.Module.AbstractCurry.C_CSymbol x18) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_182 x1 x2 x3 x6 x13 x15 x10@(Curry.Module.AbstractCurry.C_CApply x19 x20) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_182 x1 x2 x3 x6 x13 x15 x10@(Curry.Module.AbstractCurry.C_CLambda x21 x22) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_182 x1 x2 x3 x6 x13 x15 x10@(Curry.Module.AbstractCurry.C_CLetDecl x23 x24) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_182 x1 x2 x3 x6 x13 x15 x10@(Curry.Module.AbstractCurry.C_CDoExpr x25) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_182 x1 x2 x3 x6 x13 x15 x10@(Curry.Module.AbstractCurry.C_CListComp x26 x27) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_182 x1 x2 x3 x6 x13 x15 x10@(Curry.Module.AbstractCurry.C_CCase x28 x29) st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_182 x1 x2 x3 x6 x13 x15 (Curry.Module.AbstractCurry.C_CExprOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_182(x1)(x2)(x3)(x6)(x13)(x15)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_182 x1 x2 x3 x6 x13 x15 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_182")(x)



c_showLambdaOrSection_case_181 x1 x2 x3 x6 x13 x15 x16 x17@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x13)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(Curry.Module.AbstractCurry.C_CLit(x15))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st))(st))(st)
c_showLambdaOrSection_case_181 x1 x2 x3 x6 x13 x15 x16 x17@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_181 x1 x2 x3 x6 x13 x15 x16 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_181(x1)(x2)(x3)(x6)(x13)(x15)(x16)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_181 x1 x2 x3 x6 x13 x15 x16 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_181")(x)



c_showLambdaOrSection_case_185 x1 x2 x3 x6 x10 x13 x14 x15@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_184(x1)(x2)(x3)(x6)(x10)(x13)(x14)(Curry.Module.Prelude.op_61_61(x6)(x14)(st))(st)
c_showLambdaOrSection_case_185 x1 x2 x3 x6 x10 x13 x14 x15@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_185 x1 x2 x3 x6 x10 x13 x14 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_185(x1)(x2)(x3)(x6)(x10)(x13)(x14)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_185 x1 x2 x3 x6 x10 x13 x14 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_185")(x)



c_showLambdaOrSection_case_184 x1 x2 x3 x6 x10 x13 x14 x15@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showBoxedExpr(x1)(x10)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x13)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st))(st))(st)
c_showLambdaOrSection_case_184 x1 x2 x3 x6 x10 x13 x14 x15@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_183(x1)(x2)(x3)(x6)(x10)(x13)(x14)(Curry.Module.Prelude.op_61_61(x10)(Curry.Module.AbstractCurry.C_CVar(x6))(st))(st)
c_showLambdaOrSection_case_184 x1 x2 x3 x6 x10 x13 x14 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_184(x1)(x2)(x3)(x6)(x10)(x13)(x14)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_184 x1 x2 x3 x6 x10 x13 x14 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_184")(x)



c_showLambdaOrSection_case_183 x1 x2 x3 x6 x10 x13 x14 x15@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x13)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showExprOpt(x1)(Curry.Module.AbstractCurry.C_CVar(x14))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st))(st))(st)
c_showLambdaOrSection_case_183 x1 x2 x3 x6 x10 x13 x14 x15@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showLambda(x1)(x2)(x3)(st)
c_showLambdaOrSection_case_183 x1 x2 x3 x6 x10 x13 x14 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLambdaOrSection_case_183(x1)(x2)(x3)(x6)(x10)(x13)(x14)(x)(st))(i)(xs)(st)
c_showLambdaOrSection_case_183 x1 x2 x3 x6 x10 x13 x14 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLambdaOrSection_case_183")(x)



c_showSymbol_case_196 x3 x4 x2@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.AbstractCurryPrinter.c_showSymbol_case_195(x3)(x4)(x5)(x6)(Curry.Module.Prelude.op_61_61(x4)(x5)(st))(st)
c_showSymbol_case_196 x3 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showSymbol_case_196(x3)(x4)(x)(st))(i)(xs)(st)
c_showSymbol_case_196 x3 x4 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showSymbol_case_196")(x)



c_showSymbol_case_195 x3 x4 x5 x6 x7@Curry.Module.Prelude.C_True st = x6
c_showSymbol_case_195 x3 x4 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showSymbol_case_194(x3)(x5)(x6)(Curry.Module.Maybe.c_isJust(Curry.Module.FiniteMap.c_lookupFM(x3)(x6)(st))(st))(st)
c_showSymbol_case_195 x3 x4 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showSymbol_case_195(x3)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_showSymbol_case_195 x3 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showSymbol_case_195")(x)



c_showSymbol_case_194 x3 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43(x5)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List))(x6)(st))(st)
c_showSymbol_case_194 x3 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showSymbol_case_193(x6)(Curry.Module.Prelude.c_otherwise(st))(st)
c_showSymbol_case_194 x3 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showSymbol_case_194(x3)(x5)(x6)(x)(st))(i)(xs)(st)
c_showSymbol_case_194 x3 x5 x6 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showSymbol_case_194")(x)



c_showSymbol_case_193 x6 x7@Curry.Module.Prelude.C_True st = x6
c_showSymbol_case_193 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showSymbol_case_193(x6)(x)(st))(i)(xs)(st)
c_showSymbol_case_193 x6 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showSymbol_case_193")(x)



c_showExprOpt_case_197 x1 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showSymbol(x1)(x7)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st)
c_showExprOpt_case_197 x1 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showSymbol(x1)(x7)(st)
c_showExprOpt_case_197 x1 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showExprOpt_case_197(x1)(x7)(x)(st))(i)(xs)(st)
c_showExprOpt_case_197 x1 x7 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showExprOpt_case_197")(x)



c_showExprOpt_case_198 x3@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_showIdentifier(st))(x5)(st)
c_showExprOpt_case_198 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showExprOpt_case_198(x)(st))(i)(xs)(st)
c_showExprOpt_case_198 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showExprOpt_case_198")(x)



c_showLocalDecl_case_199 x1 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.List
c_showLocalDecl_case_199 x1 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))))))))(Curry.Module.AbstractCurryPrinter.c_showBlock(Curry.Module.AbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showLocalDecl(x1)))(x6)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(st))(st))(st)
c_showLocalDecl_case_199 x1 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showLocalDecl_case_199(x1)(x6)(x)(st))(i)(xs)(st)
c_showLocalDecl_case_199 x1 x6 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showLocalDecl_case_199")(x)



c_showCrhsList_case_202 x1 x4 x3@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.AbstractCurryPrinter.c_showCrhsList_case_201(x1)(x4)(x5)(x6)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x4)(Curry.Module.Prelude.List)(st))(Curry.Module.Prelude.op_61_61(x5)(Curry.Module.AbstractCurry.C_CSymbol(Curry.Module.Prelude.T2((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))))))(st))(st))(st)
c_showCrhsList_case_202 x1 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCrhsList_case_202(x1)(x4)(x)(st))(i)(xs)(st)
c_showCrhsList_case_202 x1 x4 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCrhsList_case_202")(x)



c_showCrhsList_case_201 x1 x4 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))(Curry.Module.AbstractCurryPrinter.c_showExprOpt(x1)(x6)(st))(st)
c_showCrhsList_case_201 x1 x4 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showCrhsList_case_200(x1)(x4)(x5)(x6)(Curry.Module.Prelude.c_otherwise(st))(st)
c_showCrhsList_case_201 x1 x4 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCrhsList_case_201(x1)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_showCrhsList_case_201 x1 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCrhsList_case_201")(x)



c_showCrhsList_case_200 x1 x4 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showBlock(Curry.Module.AbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showCrhs(x1)))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(x5)(x6))(x4))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(st))(st))(st)
c_showCrhsList_case_200 x1 x4 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCrhsList_case_200(x1)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_showCrhsList_case_200 x1 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCrhsList_case_200")(x)



c_showRule_case_203 x1 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.List
c_showRule_case_203 x1 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)))))))))))(Curry.Module.AbstractCurryPrinter.c_showBlock(Curry.Module.AbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showLocalDecl(x1)))(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(st))(st))(st)
c_showRule_case_203 x1 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showRule_case_203(x1)(x5)(x)(st))(i)(xs)(st)
c_showRule_case_203 x1 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showRule_case_203")(x)



c_showCmtFunc_case_210 x1 x2 x7 x8 x4@(Curry.Module.Prelude.T2 x9 x10) st = Curry.Module.AbstractCurryPrinter.c_showCmtFunc_case_209(x1)(x2)(x7)(x10)(x8)(st)
c_showCmtFunc_case_210 x1 x2 x7 x8 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCmtFunc_case_210(x1)(x2)(x7)(x8)(x)(st))(i)(xs)(st)
c_showCmtFunc_case_210 x1 x2 x7 x8 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCmtFunc_case_210")(x)



c_showCmtFunc_case_209 x1 x2 x7 x10 x8@(Curry.Module.AbstractCurry.C_CRules x11 x12) st = let {x13 = Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_isInfixOpName(st))(x10)(st)} in let {x14 = Curry.Module.AbstractCurryPrinter.c_showCmtFunc_case_205(x10)(x13)(st)} in Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_funcComment(st))(x2)(st))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showCmtFunc_case_208(x11)(x14)(Curry.Module.Prelude.op_61_61(x11)(Curry.Module.AbstractCurry.C_CFlex)(st))(st))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showCmtFunc_case_207(x7)(x14)(Curry.Module.AbstractCurryPrinter.c_isUntyped(x7)(st))(st))(Curry.Module.AbstractCurryPrinter.c_showCmtFunc_case_206(x1)(x10)(x12)(x13)(st))(st))(st))(st)
c_showCmtFunc_case_209 x1 x2 x7 x10 x8@(Curry.Module.AbstractCurry.C_CExternal x16) st = let {x17 = Curry.Module.AbstractCurryPrinter.c_showCmtFunc_case_204(x10)(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_isInfixOpName(st))(x10)(st))(st)} in Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_funcComment(st))(x2)(st))(Curry.Module.Prelude.op_43_43(x17)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_False)(x7)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x17)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List))))))))))(st))(st))(st))(st))(st))(st)
c_showCmtFunc_case_209 x1 x2 x7 x10 (Curry.Module.AbstractCurry.C_CRulesOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCmtFunc_case_209(x1)(x2)(x7)(x10)(x)(st))(i)(xs)(st)
c_showCmtFunc_case_209 x1 x2 x7 x10 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCmtFunc_case_209")(x)



c_showCmtFunc_case_204 x10 x11@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x10)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st)
c_showCmtFunc_case_204 x10 x11@Curry.Module.Prelude.C_False st = x10
c_showCmtFunc_case_204 x10 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCmtFunc_case_204(x10)(x)(st))(i)(xs)(st)
c_showCmtFunc_case_204 x10 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCmtFunc_case_204")(x)



c_showCmtFunc_case_205 x10 x13@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x10)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st)
c_showCmtFunc_case_205 x10 x13@Curry.Module.Prelude.C_False st = x10
c_showCmtFunc_case_205 x10 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCmtFunc_case_205(x10)(x)(st))(i)(xs)(st)
c_showCmtFunc_case_205 x10 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCmtFunc_case_205")(x)



c_showCmtFunc_case_206 x1 x10 x12 x13@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_concat))(Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.List.c_intersperse((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showCmtFunc'46insertName'46139(x10)))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_span(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_47_61))(Curry.Module.Prelude.C_Char(' '))))))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_tail))(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showRule(x1)))(st))(st))(st))(x12)(st))(st))(st)
c_showCmtFunc_case_206 x1 x10 x12 x13@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_43_43(x10)(Curry.Module.AbstractCurryPrinter.c_prefixInter(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showRule(x1)))(x12)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(x10)(st))(st))(st)
c_showCmtFunc_case_206 x1 x10 x12 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCmtFunc_case_206(x1)(x10)(x12)(x)(st))(i)(xs)(st)
c_showCmtFunc_case_206 x1 x10 x12 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCmtFunc_case_206")(x)



c_showCmtFunc_case_207 x7 x14 x15@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List)
c_showCmtFunc_case_207 x7 x14 x15@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_43_43(x14)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_False)(x7)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(st))(st))(st)
c_showCmtFunc_case_207 x7 x14 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCmtFunc_case_207(x7)(x14)(x)(st))(i)(xs)(st)
c_showCmtFunc_case_207 x7 x14 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCmtFunc_case_207")(x)



c_showCmtFunc_case_208 x11 x14 x15@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.List
c_showCmtFunc_case_208 x11 x14 x15@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_43_43(x14)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showEvalAnnot(x11)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(st))(st))(st)
c_showCmtFunc_case_208 x11 x14 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showCmtFunc_case_208(x11)(x14)(x)(st))(i)(xs)(st)
c_showCmtFunc_case_208 x11 x14 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showCmtFunc_case_208")(x)



c_showTypeVar_case_211 x2 x3 x4@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('_'))(x3))
c_showTypeVar_case_211 x2 x3 x4@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(x2)(x3)
c_showTypeVar_case_211 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showTypeVar_case_211(x2)(x3)(x)(st))(i)(xs)(st)
c_showTypeVar_case_211 x2 x3 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showTypeVar_case_211")(x)



c_showTypeExpr_case_214 x1 x9 x8@(Curry.Module.Prelude.T2 x10 x11) st = Curry.Module.AbstractCurryPrinter.c_showTypeExpr_case_213(x1)(x9)(x10)(x11)(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x10)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))(st))(Curry.Module.Prelude.op_61_61(x11)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))(Curry.Module.Prelude.List))))))))(st))(st))(st)
c_showTypeExpr_case_214 x1 x9 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showTypeExpr_case_214(x1)(x9)(x)(st))(i)(xs)(st)
c_showTypeExpr_case_214 x1 x9 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showTypeExpr_case_214")(x)



c_showTypeExpr_case_213 x1 x9 x10 x11 x12@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))(Curry.Module.Prelude.List)
c_showTypeExpr_case_213 x1 x9 x10 x11 x12@Curry.Module.Prelude.C_False st = Curry.Module.AbstractCurryPrinter.c_showTypeExpr_case_212(x1)(x9)(x10)(x11)(Curry.Module.Prelude.c_otherwise(st))(st)
c_showTypeExpr_case_213 x1 x9 x10 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showTypeExpr_case_213(x1)(x9)(x10)(x11)(x)(st))(i)(xs)(st)
c_showTypeExpr_case_213 x1 x9 x10 x11 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showTypeExpr_case_213")(x)



c_showTypeExpr_case_212 x1 x9 x10 x11 x12@Curry.Module.Prelude.C_True st = Curry.Module.AbstractCurryPrinter.c_maybeShowBrackets(Curry.Module.Prelude.op_38_38(x1)(Curry.Module.Prelude.c_not(Curry.Module.Prelude.c_null(x9)(st))(st))(st))(Curry.Module.AbstractCurryPrinter.c_showTypeCons(x10)(x11)(x9)(st))(st)
c_showTypeExpr_case_212 x1 x9 x10 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showTypeExpr_case_212(x1)(x9)(x10)(x11)(x)(st))(i)(xs)(st)
c_showTypeExpr_case_212 x1 x9 x10 x11 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showTypeExpr_case_212")(x)



c_showTypeExpr_case_215 x3@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.AbstractCurryPrinter.c_showTypeVar(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_showIdentifier(st))(x5)(st))(st)
c_showTypeExpr_case_215 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showTypeExpr_case_215(x)(st))(i)(xs)(st)
c_showTypeExpr_case_215 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showTypeExpr_case_215")(x)



c_showConsDecl_case_216 x5 x2@(Curry.Module.Prelude.T2 x6 x7) st = Curry.Module.Prelude.op_43_43(x7)(Curry.Module.AbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_True)))(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(st))(st)
c_showConsDecl_case_216 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showConsDecl_case_216(x5)(x)(st))(i)(xs)(st)
c_showConsDecl_case_216 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showConsDecl_case_216")(x)



c_showTypeDecl_case_217 x10 x11 x8@(Curry.Module.Prelude.T2 x12 x13) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.op_43_43(x13)(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_False)))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CTVar))(x10)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showBlock(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.AbstractCurryPrinter.c_combineMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showConsDecl))(x11)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('|'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))(st))(st))(st))(st))(st))(st))(st)
c_showTypeDecl_case_217 x10 x11 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showTypeDecl_case_217(x10)(x11)(x)(st))(i)(xs)(st)
c_showTypeDecl_case_217 x10 x11 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showTypeDecl_case_217")(x)



c_showTypeDecl_case_218 x4 x5 x2@(Curry.Module.Prelude.T2 x6 x7) st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.op_43_43(x7)(Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_prefixMap(Curry.Module.Prelude.pf(Curry.Module.AbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_False)))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pc(Curry.Module.AbstractCurry.C_CTVar))(x4)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('='))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))(Curry.Module.AbstractCurryPrinter.c_showTypeExpr(Curry.Module.Prelude.C_False)(x5)(st))(st))(st))(st))(st)
c_showTypeDecl_case_218 x4 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showTypeDecl_case_218(x4)(x5)(x)(st))(i)(xs)(st)
c_showTypeDecl_case_218 x4 x5 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showTypeDecl_case_218")(x)



c_showTypeDecls_case_219 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.List
c_showTypeDecls_case_219 x1 x2@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))
c_showTypeDecls_case_219 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showTypeDecls_case_219(x1)(x)(st))(i)(xs)(st)
c_showTypeDecls_case_219 x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showTypeDecls_case_219")(x)



c_showOpDecl_case_221 x3 x4 x2@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.Prelude.op_43_43(Curry.Module.AbstractCurryPrinter.c_showFixity(x3)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_show(x4)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.AbstractCurryPrinter.c_showOpDecl_case_220(x6)(Curry.Module.Prelude.c_apply(Curry.Module.AbstractCurryPrinter.c_isInfixOpName(st))(x6)(st))(st))(st))(st))(st))(st)
c_showOpDecl_case_221 x3 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showOpDecl_case_221(x3)(x4)(x)(st))(i)(xs)(st)
c_showOpDecl_case_221 x3 x4 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showOpDecl_case_221")(x)



c_showOpDecl_case_220 x6 x7@Curry.Module.Prelude.C_True st = x6
c_showOpDecl_case_220 x6 x7@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('`'))(Curry.Module.Prelude.op_43_43(x6)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('`'))(Curry.Module.Prelude.List))(st))
c_showOpDecl_case_220 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showOpDecl_case_220(x6)(x)(st))(i)(xs)(st)
c_showOpDecl_case_220 x6 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showOpDecl_case_220")(x)



c_showOpDecls_case_222 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.List
c_showOpDecls_case_222 x1 x2@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))
c_showOpDecls_case_222 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showOpDecls_case_222(x1)(x)(st))(i)(xs)(st)
c_showOpDecls_case_222 x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showOpDecls_case_222")(x)



c_showImport_case_223 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))(x1)(st)
c_showImport_case_223 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.List
c_showImport_case_223 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showImport_case_223(x1)(x)(st))(i)(xs)(st)
c_showImport_case_223 x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showImport_case_223")(x)



c_showImports_case_224 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.List
c_showImports_case_224 x1 x2@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))
c_showImports_case_224 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showImports_case_224(x1)(x)(st))(i)(xs)(st)
c_showImports_case_224 x1 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showImports_case_224")(x)



c_showExports'46getFuncName'469_case_225 x10@(Curry.Module.Prelude.T2 x15 x16) st = x16
c_showExports'46getFuncName'469_case_225 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showExports'46getFuncName'469_case_225(x)(st))(i)(xs)(st)
c_showExports'46getFuncName'469_case_225 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showExports.getFuncName.9_case_225")(x)



c_showExports'46getFuncName'469_case_226 x2@(Curry.Module.Prelude.T2 x7 x8) st = x8
c_showExports'46getFuncName'469_case_226 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showExports'46getFuncName'469_case_226(x)(st))(i)(xs)(st)
c_showExports'46getFuncName'469_case_226 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showExports.getFuncName.9_case_226")(x)



c_showExports'46getTypeName'469_case_227 x8@(Curry.Module.Prelude.T2 x12 x13) st = x13
c_showExports'46getTypeName'469_case_227 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showExports'46getTypeName'469_case_227(x)(st))(i)(xs)(st)
c_showExports'46getTypeName'469_case_227 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showExports.getTypeName.9_case_227")(x)



c_showExports'46getTypeName'469_case_228 x2@(Curry.Module.Prelude.T2 x6 x7) st = x7
c_showExports'46getTypeName'469_case_228 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showExports'46getTypeName'469_case_228(x)(st))(i)(xs)(st)
c_showExports'46getTypeName'469_case_228 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showExports.getTypeName.9_case_228")(x)



c_showProg_case_229 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.List
c_showProg_case_229 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('('))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(x7)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(')'))(Curry.Module.Prelude.List))(st))(st)
c_showProg_case_229 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.AbstractCurryPrinter.c_showProg_case_229(x7)(x)(st))(i)(xs)(st)
c_showProg_case_229 x7 x st = Curry.RunTimeSystem.patternFail("AbstractCurryPrinter.showProg_case_229")(x)


