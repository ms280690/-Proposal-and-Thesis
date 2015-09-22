{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Distribution (module Curry.Module.Distribution) where

import Curry.RunTimeSystem
import Curry.Module.Char
import Curry.Module.FileGoodies
import Curry.Module.IO
import Curry.Module.Prelude
import Curry.Module.PropertyFile
import Curry.Module.System
import Curry.Module.List
import Curry.Module.Directory



-- begin included



import Curry.Files.KiCSPath   
import Curry.Files.CymakePath 

curryCompiler :: Result C_String
curryCompiler _ = toCurry "kics"

curryCompilerMajorVersion :: Result C_Int
curryCompilerMajorVersion _ = 0

curryCompilerMinorVersion :: Result C_Int
curryCompilerMinorVersion _ = 8

getStdLibDir :: Result (C_IO C_String)
getStdLibDir = ioFunc0 getKiCSLibDir

-- finding the frontend binary
getFrontendCall :: Result (C_IO C_String)
getFrontendCall = ioFunc0 getCymake

installDir :: Result C_String
installDir _ = toCurry ""

curryRuntime :: Result C_String
curryRuntime _ = toCurry "ghc"

curryRuntimeMajorVersion :: Result C_Int
curryRuntimeMajorVersion _ = 6

curryRuntimeMinorVersion :: Result C_Int
curryRuntimeMinorVersion _ = 10



-- end included

data C_FrontendTarget = C_FCY
  | C_FINT
  | C_ACY
  | C_UACY
  | C_HTML
  | C_CY
  | C_FrontendTargetFail Curry.RunTimeSystem.C_Exceptions
  | C_FrontendTargetOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.Distribution.C_FrontendTarget)

data C_FrontendParams = C_FrontendParams Curry.Module.Prelude.C_Bool (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
  | C_FrontendParamsFail Curry.RunTimeSystem.C_Exceptions
  | C_FrontendParamsOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.Distribution.C_FrontendParams)

instance BaseCurry Curry.Module.Distribution.C_FrontendTarget where
  nf f x st = f(x)(st)

  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Distribution.C_FrontendTargetOr(Curry.RunTimeSystem.mkRef(r)(0)(i))([Curry.Module.Distribution.C_FCY,Curry.Module.Distribution.C_FINT,Curry.Module.Distribution.C_ACY,Curry.Module.Distribution.C_UACY,Curry.Module.Distribution.C_HTML,Curry.Module.Distribution.C_CY]))(0)

  failed  = Curry.Module.Distribution.C_FrontendTargetFail

  branching  = Curry.Module.Distribution.C_FrontendTargetOr

  consKind (Curry.Module.Distribution.C_FrontendTargetOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Distribution.C_FrontendTargetFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Distribution.C_FrontendTargetFail x) = x

  orRef (Curry.Module.Distribution.C_FrontendTargetOr x _) = x

  branches (Curry.Module.Distribution.C_FrontendTargetOr _ x) = x





instance BaseCurry Curry.Module.Distribution.C_FrontendParams where
  nf f (Curry.Module.Distribution.C_FrontendParams x1 x2 x3 x4) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> f(Curry.Module.Distribution.C_FrontendParams(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Distribution.C_FrontendParams x1 x2 x3 x4) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> f(Curry.Module.Distribution.C_FrontendParams(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Distribution.C_FrontendParamsOr(Curry.RunTimeSystem.mkRef(r)(4)(i))([Curry.Module.Distribution.C_FrontendParams(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(4)

  failed  = Curry.Module.Distribution.C_FrontendParamsFail

  branching  = Curry.Module.Distribution.C_FrontendParamsOr

  consKind (Curry.Module.Distribution.C_FrontendParamsOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Distribution.C_FrontendParamsFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Distribution.C_FrontendParamsFail x) = x

  orRef (Curry.Module.Distribution.C_FrontendParamsOr x _) = x

  branches (Curry.Module.Distribution.C_FrontendParamsOr _ x) = x





instance Curry Curry.Module.Distribution.C_FrontendTarget where
  strEq Curry.Module.Distribution.C_FCY Curry.Module.Distribution.C_FCY st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.Distribution.C_FINT Curry.Module.Distribution.C_FINT st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.Distribution.C_ACY Curry.Module.Distribution.C_ACY st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.Distribution.C_UACY Curry.Module.Distribution.C_UACY st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.Distribution.C_HTML Curry.Module.Distribution.C_HTML st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.Distribution.C_CY Curry.Module.Distribution.C_CY st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq Curry.Module.Distribution.C_FCY Curry.Module.Distribution.C_FCY st = Curry.Module.Prelude.C_True
  eq Curry.Module.Distribution.C_FINT Curry.Module.Distribution.C_FINT st = Curry.Module.Prelude.C_True
  eq Curry.Module.Distribution.C_ACY Curry.Module.Distribution.C_ACY st = Curry.Module.Prelude.C_True
  eq Curry.Module.Distribution.C_UACY Curry.Module.Distribution.C_UACY st = Curry.Module.Prelude.C_True
  eq Curry.Module.Distribution.C_HTML Curry.Module.Distribution.C_HTML st = Curry.Module.Prelude.C_True
  eq Curry.Module.Distribution.C_CY Curry.Module.Distribution.C_CY st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f Curry.Module.Distribution.C_FCY st = Curry.Module.Distribution.C_FCY
  propagate f Curry.Module.Distribution.C_FINT st = Curry.Module.Distribution.C_FINT
  propagate f Curry.Module.Distribution.C_ACY st = Curry.Module.Distribution.C_ACY
  propagate f Curry.Module.Distribution.C_UACY st = Curry.Module.Distribution.C_UACY
  propagate f Curry.Module.Distribution.C_HTML st = Curry.Module.Distribution.C_HTML
  propagate f Curry.Module.Distribution.C_CY st = Curry.Module.Distribution.C_CY

  foldCurry f c Curry.Module.Distribution.C_FCY st = c
  foldCurry f c Curry.Module.Distribution.C_FINT st = c
  foldCurry f c Curry.Module.Distribution.C_ACY st = c
  foldCurry f c Curry.Module.Distribution.C_UACY st = c
  foldCurry f c Curry.Module.Distribution.C_HTML st = c
  foldCurry f c Curry.Module.Distribution.C_CY st = c

  typeName _ = "FrontendTarget"

  showQ _ Curry.Module.Distribution.C_FCY = Prelude.showString("Distribution.FCY")
  showQ _ Curry.Module.Distribution.C_FINT = Prelude.showString("Distribution.FINT")
  showQ _ Curry.Module.Distribution.C_ACY = Prelude.showString("Distribution.ACY")
  showQ _ Curry.Module.Distribution.C_UACY = Prelude.showString("Distribution.UACY")
  showQ _ Curry.Module.Distribution.C_HTML = Prelude.showString("Distribution.HTML")
  showQ _ Curry.Module.Distribution.C_CY = Prelude.showString("Distribution.CY")
  showQ _ (Curry.Module.Distribution.C_FrontendTargetOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Curry Curry.Module.Distribution.C_FrontendParams where
  strEq (Curry.Module.Distribution.C_FrontendParams x1 x2 x3 x4) (Curry.Module.Distribution.C_FrontendParams y1 y2 y3 y4) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Distribution.C_FrontendParams x1 x2 x3 x4) (Curry.Module.Distribution.C_FrontendParams y1 y2 y3 y4) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.genEq(x4)(y4)(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Distribution.C_FrontendParams x1 x2 x3 x4) st = Curry.Module.Distribution.C_FrontendParams(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))

  foldCurry f c (Curry.Module.Distribution.C_FrontendParams x1 x2 x3 x4) st = f(x1)(f(x2)(f(x3)(f(x4)(c)(st))(st))(st))(st)

  typeName _ = "FrontendParams"

  showQ d (Curry.Module.Distribution.C_FrontendParams x1 x2 x3 x4) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Distribution.FrontendParams "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))))))))


  showQ _ (Curry.Module.Distribution.C_FrontendParamsOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.Distribution.C_FrontendTarget where
  showsPrec _ Curry.Module.Distribution.C_FCY = Prelude.showString("FCY")
  showsPrec _ Curry.Module.Distribution.C_FINT = Prelude.showString("FINT")
  showsPrec _ Curry.Module.Distribution.C_ACY = Prelude.showString("ACY")
  showsPrec _ Curry.Module.Distribution.C_UACY = Prelude.showString("UACY")
  showsPrec _ Curry.Module.Distribution.C_HTML = Prelude.showString("HTML")
  showsPrec _ Curry.Module.Distribution.C_CY = Prelude.showString("CY")
  showsPrec _ (Curry.Module.Distribution.C_FrontendTargetOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.Distribution.C_FrontendParams where
  showsPrec d (Curry.Module.Distribution.C_FrontendParams x1 x2 x3 x4) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("FrontendParams "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))))))))


  showsPrec _ (Curry.Module.Distribution.C_FrontendParamsOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Read Curry.Module.Distribution.C_FrontendTarget where
  readsPrec d r = (Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Distribution.C_FCY)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Distribution")("FCY")(r)])(r))((Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Distribution.C_FINT)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Distribution")("FINT")(r)])(r))((Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Distribution.C_ACY)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Distribution")("ACY")(r)])(r))((Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Distribution.C_UACY)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Distribution")("UACY")(r)])(r))((Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Distribution.C_HTML)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Distribution")("HTML")(r)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.Distribution.C_CY)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Distribution")("CY")(r)])(r))))))





instance Read Curry.Module.Distribution.C_FrontendParams where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Distribution.C_FrontendParams(x1)(x2)(x3)(x4))(r4) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Distribution")("FrontendParams")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3)])(r)





c_rcFileName :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_rcFileName st = Curry.Module.Prelude.op_62_62_61(Curry.Module.System.c_getEnviron((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('H'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('O'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))(Curry.Module.Prelude.List)))))(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_43_43))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.op_43_43(Curry.Module.Distribution.c_curryCompiler(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))(Curry.Module.Prelude.List)))(st))(st))))(st))(st)



c_rcFileContents :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_rcFileContents st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_rcFileName(st))(Curry.Module.Prelude.pf(Curry.Module.PropertyFile.c_readPropertyFile))(st)



c_getRcVar :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getRcVar x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_getRcVars((Curry.Module.Prelude.:<)(x1)(Curry.Module.Prelude.List))(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_head))(st))(st)



c_getRcVars :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_getRcVars x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_rcFileContents(st))(Curry.Module.Prelude.pf(Curry.Module.Distribution.c_getRcVars'46_'35lambda2(x1)))(st)



c_getRcVars'46_'35lambda2 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)))
c_getRcVars'46_'35lambda2 x1 x2 st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.c_lookup))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Distribution.c_getRcVars'46_'35lambda2'46_'35lambda3))(x2)(st))))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Char.c_toLower))))(x1)(st))(st))(st)



c_getRcVars'46_'35lambda2'46_'35lambda3 :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_getRcVars'46_'35lambda2'46_'35lambda3 x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.Prelude.T2(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Char.c_toLower))(x2)(st))(x3)
c_getRcVars'46_'35lambda2'46_'35lambda3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_getRcVars'46_'35lambda2'46_'35lambda3(x)(st))(i)(xs)(st)
c_getRcVars'46_'35lambda2'46_'35lambda3 x st = Curry.RunTimeSystem.patternFail("Distribution.getRcVars._#lambda2._#lambda3")(x)



c_currySubdir :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_currySubdir st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))



c_inCurrySubdir :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_inCurrySubdir x1 st = let {x2 = Curry.Module.FileGoodies.c_splitDirectoryBaseName(x1)(st)} in let {x3 = Curry.Module.Distribution.c_inCurrySubdir'46_'35selFP3'35base(x2)(st)} in Curry.Module.Prelude.op_43_43(Curry.Module.Distribution.c_inCurrySubdir_case_13(x3)(st))(Curry.Module.Prelude.op_43_43(Curry.Module.Distribution.c_currySubdir(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.Distribution.c_inCurrySubdir'46_'35selFP4'35file(x2)(st)))(st))(st)



c_inCurrySubdir'46_'35selFP3'35base :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_inCurrySubdir'46_'35selFP3'35base x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_inCurrySubdir'46_'35selFP3'35base (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_inCurrySubdir'46_'35selFP3'35base(x)(st))(i)(xs)(st)
c_inCurrySubdir'46_'35selFP3'35base x st = Curry.RunTimeSystem.patternFail("Distribution.inCurrySubdir._#selFP3#base")(x)



c_inCurrySubdir'46_'35selFP4'35file :: (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_inCurrySubdir'46_'35selFP4'35file x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_inCurrySubdir'46_'35selFP4'35file (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_inCurrySubdir'46_'35selFP4'35file(x)(st))(i)(xs)(st)
c_inCurrySubdir'46_'35selFP4'35file x st = Curry.RunTimeSystem.patternFail("Distribution.inCurrySubdir._#selFP4#file")(x)



c_addCurrySubdir :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_addCurrySubdir x1 st = Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.Distribution.c_currySubdir(st)))(st)



c_getSysLibPath :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getSysLibPath st = Curry.Module.Distribution.c_getSysLibPath_case_10(Curry.Module.Prelude.op_61_61(Curry.Module.Distribution.c_curryCompiler(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))(st))(st)



c_getSysLibPath'46_'35lambda5 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getSysLibPath'46_'35lambda5 x1 st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Distribution.c_getSysLibPath'46_'35lambda5_case_8(x1)(Curry.Module.Prelude.c_null(x1)(st))(st))(st)



c_lookupFileInLoadPath :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_lookupFileInLoadPath x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_getLoadPathForFile(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.FileGoodies.c_lookupFileInPath(Curry.Module.FileGoodies.c_baseName(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))))(st)



c_findFileInLoadPath :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_findFileInLoadPath x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_getLoadPathForFile(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.FileGoodies.c_getFileInPath(Curry.Module.FileGoodies.c_baseName(x1)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.List)(Curry.Module.Prelude.List))))(st)



c_readFirstFileInLoadPath :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_readFirstFileInLoadPath x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_findFileInLoadPath(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_readFile))(st)



c_getLoadPath :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getLoadPath st = Curry.Module.Distribution.c_getLoadPathForFile((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))(Curry.Module.Prelude.List))))(st)



c_getLoadPathForFile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getLoadPathForFile x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_getSysLibPath(st))(Curry.Module.Prelude.pf(Curry.Module.Distribution.c_getLoadPathForFile'46_'35lambda7(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.Distribution.c_getLoadPathForFile'46_'35lambda6))(st))(x1)))(st)



c_getLoadPathForFile'46_'35lambda6 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_getLoadPathForFile'46_'35lambda6 x1 st = (Curry.Module.Prelude.:<)(x1)((Curry.Module.Prelude.:<)(Curry.Module.Distribution.c_addCurrySubdir(x1)(st))(Curry.Module.Prelude.List))



c_getLoadPathForFile'46_'35lambda7 :: (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getLoadPathForFile'46_'35lambda7 x1 x2 x3 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_getRcVar((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))))))(st))(Curry.Module.Prelude.pf(Curry.Module.Distribution.c_getLoadPathForFile'46_'35lambda7'46_'35lambda8(x1)(x2)(x3)))(st)



c_getLoadPathForFile'46_'35lambda7'46_'35lambda8 :: (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getLoadPathForFile'46_'35lambda7'46_'35lambda8 x1 x2 x3 x4 st = Curry.Module.Distribution.c_getLoadPathForFile'46_'35lambda7'46_'35lambda8_case_7(x1)(x2)(x3)(x4)(Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_61_61(Curry.Module.Distribution.c_curryCompiler(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))(st))(Curry.Module.Prelude.op_61_61(Curry.Module.Distribution.c_curryCompiler(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))(st))(st))(st)



c_getLoadPathForFile'46_'35lambda7'46_'35lambda8'46_'35lambda9 :: (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_getLoadPathForFile'46_'35lambda7'46_'35lambda8'46_'35lambda9 x1 x2 x3 x4 x5 st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.c_apply(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.op_43_43(Curry.Module.Distribution.c_getLoadPathForFile'46_'35lambda7'46_'35lambda8'46_'35lambda9_case_6(x5)(Curry.Module.Prelude.c_null(x5)(st))(st))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.List)(Curry.Module.Prelude.pf(Curry.Module.FileGoodies.c_splitPath))(x3)(st))(x4)(st))(st)))(st))(st)



c_defaultParams :: Curry.RunTimeSystem.State -> Curry.Module.Distribution.C_FrontendParams
c_defaultParams st = Curry.Module.Distribution.C_FrontendParams(Curry.Module.Prelude.C_False)(Curry.Module.Prelude.C_Nothing)(Curry.Module.Prelude.C_Nothing)(Curry.Module.Prelude.C_Nothing)



c_setQuiet :: Curry.Module.Prelude.C_Bool -> Curry.Module.Distribution.C_FrontendParams -> Curry.RunTimeSystem.State -> Curry.Module.Distribution.C_FrontendParams
c_setQuiet x1 x2@(Curry.Module.Distribution.C_FrontendParams x3 x4 x5 x6) st = Curry.Module.Distribution.C_FrontendParams(x1)(x4)(x5)(x6)
c_setQuiet x1 (Curry.Module.Distribution.C_FrontendParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_setQuiet(x1)(x)(st))(i)(xs)(st)
c_setQuiet x1 x st = Curry.RunTimeSystem.patternFail("Distribution.setQuiet")(x)



c_setFullPath :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.Module.Distribution.C_FrontendParams -> Curry.RunTimeSystem.State -> Curry.Module.Distribution.C_FrontendParams
c_setFullPath x1 x2@(Curry.Module.Distribution.C_FrontendParams x3 x4 x5 x6) st = Curry.Module.Distribution.C_FrontendParams(x3)(Curry.Module.Prelude.C_Just(x1))(x5)(x6)
c_setFullPath x1 (Curry.Module.Distribution.C_FrontendParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_setFullPath(x1)(x)(st))(i)(xs)(st)
c_setFullPath x1 x st = Curry.RunTimeSystem.patternFail("Distribution.setFullPath")(x)



c_setOutfile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Distribution.C_FrontendParams -> Curry.RunTimeSystem.State -> Curry.Module.Distribution.C_FrontendParams
c_setOutfile x1 x2@(Curry.Module.Distribution.C_FrontendParams x3 x4 x5 x6) st = Curry.Module.Distribution.C_FrontendParams(x3)(x4)(Curry.Module.Prelude.C_Just(x1))(x6)
c_setOutfile x1 (Curry.Module.Distribution.C_FrontendParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_setOutfile(x1)(x)(st))(i)(xs)(st)
c_setOutfile x1 x st = Curry.RunTimeSystem.patternFail("Distribution.setOutfile")(x)



c_setLogfile :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Distribution.C_FrontendParams -> Curry.RunTimeSystem.State -> Curry.Module.Distribution.C_FrontendParams
c_setLogfile x1 x2@(Curry.Module.Distribution.C_FrontendParams x3 x4 x5 x6) st = Curry.Module.Distribution.C_FrontendParams(x3)(x4)(x5)(Curry.Module.Prelude.C_Just(x1))
c_setLogfile x1 (Curry.Module.Distribution.C_FrontendParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_setLogfile(x1)(x)(st))(i)(xs)(st)
c_setLogfile x1 x st = Curry.RunTimeSystem.patternFail("Distribution.setLogfile")(x)



c_quiet :: Curry.Module.Distribution.C_FrontendParams -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_quiet x1@(Curry.Module.Distribution.C_FrontendParams x2 x3 x4 x5) st = x2
c_quiet (Curry.Module.Distribution.C_FrontendParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_quiet(x)(st))(i)(xs)(st)
c_quiet x st = Curry.RunTimeSystem.patternFail("Distribution.quiet")(x)



c_fullPath :: Curry.Module.Distribution.C_FrontendParams -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char))
c_fullPath x1@(Curry.Module.Distribution.C_FrontendParams x2 x3 x4 x5) st = x3
c_fullPath (Curry.Module.Distribution.C_FrontendParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_fullPath(x)(st))(i)(xs)(st)
c_fullPath x st = Curry.RunTimeSystem.patternFail("Distribution.fullPath")(x)



c_outfile :: Curry.Module.Distribution.C_FrontendParams -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_outfile x1@(Curry.Module.Distribution.C_FrontendParams x2 x3 x4 x5) st = x4
c_outfile (Curry.Module.Distribution.C_FrontendParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_outfile(x)(st))(i)(xs)(st)
c_outfile x st = Curry.RunTimeSystem.patternFail("Distribution.outfile")(x)



c_logfile :: Curry.Module.Distribution.C_FrontendParams -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_logfile x1@(Curry.Module.Distribution.C_FrontendParams x2 x3 x4 x5) st = x5
c_logfile (Curry.Module.Distribution.C_FrontendParamsOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_logfile(x)(st))(i)(xs)(st)
c_logfile x st = Curry.RunTimeSystem.patternFail("Distribution.logfile")(x)



c_callFrontend :: Curry.Module.Distribution.C_FrontendTarget -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0)
c_callFrontend x1 st = Curry.Module.Prelude.pf(Curry.Module.Distribution.c_callFrontendWithParams(x1)(Curry.Module.Distribution.c_defaultParams(st)))



c_callFrontendWithParams :: Curry.Module.Distribution.C_FrontendTarget -> Curry.Module.Distribution.C_FrontendParams -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_callFrontendWithParams x1 x2 x3 st = let {x4 = Curry.Module.Distribution.c_callFrontendWithParams_case_2(Curry.Module.Prelude.op_61_61(Curry.Module.Distribution.c_curryCompiler(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List))))))(st))(st)} in Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_callFrontendWithParams_case_5(x2)(x4)(st))(Curry.Module.Prelude.pf(Curry.Module.Distribution.c_callFrontendWithParams'46_'35lambda12(x2)(x3)(Curry.Module.Prelude.op_43_43(Curry.Module.Distribution.c_callFrontendWithParams_case_4(x2)(x4)(Curry.Module.Distribution.c_quiet(x2)(st))(st))(Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.List)(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))(Curry.Module.Distribution.c_outfile(x2)(st))(st))(st))(x1)))(st)



c_callFrontendWithParams'46_'35lambda10 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_callFrontendWithParams'46_'35lambda10 x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_getFrontendCall(st))(Curry.Module.Prelude.pf(Curry.Module.Distribution.c_callFrontendWithParams'46_'35lambda10'46_'35lambda11(x1)))(st)



c_callFrontendWithParams'46_'35lambda10'46_'35lambda11 :: (Curry.Module.Prelude.List (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_callFrontendWithParams'46_'35lambda10'46_'35lambda11 x1 x2 st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.op_43_43(x2)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))(Curry.Module.Prelude.List))))))(st))(x1)(st))(st))(st)



c_callFrontendWithParams'46showFrontendTarget'4684 :: Curry.Module.Distribution.C_FrontendTarget -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_callFrontendWithParams'46showFrontendTarget'4684 x1@Curry.Module.Distribution.C_FCY st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))))
c_callFrontendWithParams'46showFrontendTarget'4684 x1@Curry.Module.Distribution.C_FINT st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List))))))
c_callFrontendWithParams'46showFrontendTarget'4684 x1@Curry.Module.Distribution.C_ACY st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List)))))
c_callFrontendWithParams'46showFrontendTarget'4684 x1@Curry.Module.Distribution.C_UACY st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))
c_callFrontendWithParams'46showFrontendTarget'4684 x1@Curry.Module.Distribution.C_HTML st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))(Curry.Module.Prelude.List))))))
c_callFrontendWithParams'46showFrontendTarget'4684 x1@Curry.Module.Distribution.C_CY st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))))))
c_callFrontendWithParams'46showFrontendTarget'4684 (Curry.Module.Distribution.C_FrontendTargetOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_callFrontendWithParams'46showFrontendTarget'4684(x)(st))(i)(xs)(st)
c_callFrontendWithParams'46showFrontendTarget'4684 x st = Curry.RunTimeSystem.patternFail("Distribution.callFrontendWithParams.showFrontendTarget.84")(x)



c_callFrontendWithParams'46_'35lambda12 :: Curry.Module.Distribution.C_FrontendParams -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.Module.Distribution.C_FrontendTarget -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_callFrontendWithParams'46_'35lambda12 x1 x2 x3 x4 x5 st = let {x6 = Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.List)(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_id))(Curry.Module.Distribution.c_logfile(x1)(st))(st)} in let {x7 = Curry.Module.Prelude.op_43_43(x5)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.Distribution.c_callFrontendWithParams'46showFrontendTarget'4684(x4)(st))(Curry.Module.Prelude.op_43_43(x3)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))(x2)(st))(st))(st))(st))(st)} in Curry.Module.Prelude.op_62_62(Curry.Module.Distribution.c_callFrontendWithParams'46_'35lambda12_case_0(x6)(x7)(Curry.Module.Prelude.c_null(x6)(st))(st))(Curry.Module.Prelude.c_return(Curry.Module.Prelude.T0)(st))(st)



c_rcErr :: (Curry t0) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0
c_rcErr x1 x2 st = Curry.Module.Prelude.op_62_62(Curry.Module.IO.c_hPutStrLn(Curry.Module.IO.c_stderr(st))(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))))))))))))))))(st))(st))(Curry.Module.Prelude.c_return(x2)(st))(st)



c_callFrontendWithParams'46_'35lambda12_case_0 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.System.c_system(x7)(st)
c_callFrontendWithParams'46_'35lambda12_case_0 x6 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.System.c_system(Curry.Module.Prelude.op_43_43(x7)(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))(Curry.Module.Prelude.op_43_43(x6)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('2'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('&'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('1'))(Curry.Module.Prelude.List))))))(st))(st))(st))(st)
c_callFrontendWithParams'46_'35lambda12_case_0 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_callFrontendWithParams'46_'35lambda12_case_0(x6)(x7)(x)(st))(i)(xs)(st)
c_callFrontendWithParams'46_'35lambda12_case_0 x6 x7 x st = Curry.RunTimeSystem.patternFail("Distribution.callFrontendWithParams._#lambda12_case_0")(x)



c_callFrontendWithParams_case_2 x1@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_True
c_callFrontendWithParams_case_2 x1@Curry.Module.Prelude.C_False st = Curry.Module.Distribution.c_callFrontendWithParams_case_1(Curry.Module.Prelude.op_61_61(Curry.Module.Distribution.c_curryCompiler(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))(st))(st)
c_callFrontendWithParams_case_2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_callFrontendWithParams_case_2(x)(st))(i)(xs)(st)
c_callFrontendWithParams_case_2 x st = Curry.RunTimeSystem.patternFail("Distribution.callFrontendWithParams_case_2")(x)



c_callFrontendWithParams_case_1 x1@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_False
c_callFrontendWithParams_case_1 x1@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))))(st)
c_callFrontendWithParams_case_1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_callFrontendWithParams_case_1(x)(st))(i)(xs)(st)
c_callFrontendWithParams_case_1 x st = Curry.RunTimeSystem.patternFail("Distribution.callFrontendWithParams_case_1")(x)



c_callFrontendWithParams_case_4 x2 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Distribution.c_callFrontendWithParams_case_3(x4)(st)
c_callFrontendWithParams_case_4 x2 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.List
c_callFrontendWithParams_case_4 x2 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_callFrontendWithParams_case_4(x2)(x4)(x)(st))(i)(xs)(st)
c_callFrontendWithParams_case_4 x2 x4 x st = Curry.RunTimeSystem.patternFail("Distribution.callFrontendWithParams_case_4")(x)



c_callFrontendWithParams_case_3 x4@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('q'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))
c_callFrontendWithParams_case_3 x4@Curry.Module.Prelude.C_False st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('v'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))
c_callFrontendWithParams_case_3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_callFrontendWithParams_case_3(x)(st))(i)(xs)(st)
c_callFrontendWithParams_case_3 x st = Curry.RunTimeSystem.patternFail("Distribution.callFrontendWithParams_case_3")(x)



c_callFrontendWithParams_case_5 x2 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.op_43_43(Curry.Module.Distribution.c_installDir(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))(Curry.Module.Prelude.List))))))))))))))))(st))(st)
c_callFrontendWithParams_case_5 x2 x4@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_maybe(Curry.Module.Distribution.c_getLoadPath(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Distribution.c_fullPath(x2)(st))(st))(Curry.Module.Prelude.pf(Curry.Module.Distribution.c_callFrontendWithParams'46_'35lambda10))(st)
c_callFrontendWithParams_case_5 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_callFrontendWithParams_case_5(x2)(x)(st))(i)(xs)(st)
c_callFrontendWithParams_case_5 x2 x st = Curry.RunTimeSystem.patternFail("Distribution.callFrontendWithParams_case_5")(x)



c_getLoadPathForFile'46_'35lambda7'46_'35lambda8'46_'35lambda9_case_6 x5 x6@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.List
c_getLoadPathForFile'46_'35lambda7'46_'35lambda8'46_'35lambda9_case_6 x5 x6@Curry.Module.Prelude.C_False st = Curry.Module.FileGoodies.c_splitPath(x5)(st)
c_getLoadPathForFile'46_'35lambda7'46_'35lambda8'46_'35lambda9_case_6 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_getLoadPathForFile'46_'35lambda7'46_'35lambda8'46_'35lambda9_case_6(x5)(x)(st))(i)(xs)(st)
c_getLoadPathForFile'46_'35lambda7'46_'35lambda8'46_'35lambda9_case_6 x5 x st = Curry.RunTimeSystem.patternFail("Distribution.getLoadPathForFile._#lambda7._#lambda8._#lambda9_case_6")(x)



c_getLoadPathForFile'46_'35lambda7'46_'35lambda8_case_7 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_62_62_61(Curry.Module.System.c_getEnviron((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('U'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('Y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('H'))(Curry.Module.Prelude.List))))))))))(st))(Curry.Module.Prelude.pf(Curry.Module.Distribution.c_getLoadPathForFile'46_'35lambda7'46_'35lambda8'46_'35lambda9(x1)(Curry.Module.FileGoodies.c_dirName(x2)(st))(x4)(x3)))(st)
c_getLoadPathForFile'46_'35lambda7'46_'35lambda8_case_7 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('F'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))))))))))(st)
c_getLoadPathForFile'46_'35lambda7'46_'35lambda8_case_7 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_getLoadPathForFile'46_'35lambda7'46_'35lambda8_case_7(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_getLoadPathForFile'46_'35lambda7'46_'35lambda8_case_7 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("Distribution.getLoadPathForFile._#lambda7._#lambda8_case_7")(x)



c_getSysLibPath'46_'35lambda5_case_8 x1 x2@Curry.Module.Prelude.C_True st = (Curry.Module.Prelude.:<)(Curry.Module.Prelude.op_43_43(Curry.Module.Distribution.c_installDir(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))(Curry.Module.Prelude.List)))))(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.op_43_43(Curry.Module.Distribution.c_installDir(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))(Curry.Module.Prelude.List))))))))))(st))(Curry.Module.Prelude.List))
c_getSysLibPath'46_'35lambda5_case_8 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.FileGoodies.c_splitPath(x1)(st)
c_getSysLibPath'46_'35lambda5_case_8 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_getSysLibPath'46_'35lambda5_case_8(x1)(x)(st))(i)(xs)(st)
c_getSysLibPath'46_'35lambda5_case_8 x1 x st = Curry.RunTimeSystem.patternFail("Distribution.getSysLibPath._#lambda5_case_8")(x)



c_getSysLibPath_case_10 x1@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_62_62_61(Curry.Module.System.c_getEnviron((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('K'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('B'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('T'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('H'))(Curry.Module.Prelude.List)))))))))))))(st))(Curry.Module.Prelude.pf(Curry.Module.Distribution.c_getSysLibPath'46_'35lambda5))(st)
c_getSysLibPath_case_10 x1@Curry.Module.Prelude.C_False st = Curry.Module.Distribution.c_getSysLibPath_case_9(Curry.Module.Prelude.op_61_61(Curry.Module.Distribution.c_curryCompiler(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))(st))(st)
c_getSysLibPath_case_10 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_getSysLibPath_case_10(x)(st))(i)(xs)(st)
c_getSysLibPath_case_10 x st = Curry.RunTimeSystem.patternFail("Distribution.getSysLibPath_case_10")(x)



c_getSysLibPath_case_9 x1@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Distribution.c_getStdLibDir(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pc))((Curry.Module.Prelude.:<)))(Curry.Module.Prelude.List)))(st))(st)
c_getSysLibPath_case_9 x1@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('D'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('S'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('L'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('b'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('P'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('w'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('C'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))))))))))))))))))(st)
c_getSysLibPath_case_9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_getSysLibPath_case_9(x)(st))(i)(xs)(st)
c_getSysLibPath_case_9 x st = Curry.RunTimeSystem.patternFail("Distribution.getSysLibPath_case_9")(x)



c_inCurrySubdir_case_13 x3@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.Distribution.c_inCurrySubdir_case_12(x3)(x5)(x6)(Curry.Module.Prelude.op_61_61(x5)(Curry.Module.Prelude.C_Char('.'))(st))(st)
c_inCurrySubdir_case_13 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.op_43_43(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.Prelude.List))(st)
c_inCurrySubdir_case_13 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_inCurrySubdir_case_13(x)(st))(i)(xs)(st)
c_inCurrySubdir_case_13 x st = Curry.RunTimeSystem.patternFail("Distribution.inCurrySubdir_case_13")(x)



c_inCurrySubdir_case_12 x3 x5 x6 x7@Curry.Module.Prelude.C_True st = Curry.Module.Distribution.c_inCurrySubdir_case_11(x3)(x6)(st)
c_inCurrySubdir_case_12 x3 x5 x6 x7@Curry.Module.Prelude.C_False st = Curry.Module.Prelude.op_43_43(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.Prelude.List))(st)
c_inCurrySubdir_case_12 x3 x5 x6 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_inCurrySubdir_case_12(x3)(x5)(x6)(x)(st))(i)(xs)(st)
c_inCurrySubdir_case_12 x3 x5 x6 x st = Curry.RunTimeSystem.patternFail("Distribution.inCurrySubdir_case_12")(x)



c_inCurrySubdir_case_11 x3 x6@Curry.Module.Prelude.List st = Curry.Module.Prelude.List
c_inCurrySubdir_case_11 x3 x6@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.Prelude.op_43_43(x3)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('/'))(Curry.Module.Prelude.List))(st)
c_inCurrySubdir_case_11 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Distribution.c_inCurrySubdir_case_11(x3)(x)(st))(i)(xs)(st)
c_inCurrySubdir_case_11 x3 x st = Curry.RunTimeSystem.patternFail("Distribution.inCurrySubdir_case_11")(x)



c_curryCompiler :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_curryCompiler st = Curry.Module.Distribution.curryCompiler(st)



c_curryCompilerMajorVersion :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_curryCompilerMajorVersion st = Curry.Module.Distribution.curryCompilerMajorVersion(st)



c_curryCompilerMinorVersion :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_curryCompilerMinorVersion st = Curry.Module.Distribution.curryCompilerMinorVersion(st)



c_curryRuntime :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_curryRuntime st = Curry.Module.Distribution.curryRuntime(st)



c_curryRuntimeMajorVersion :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_curryRuntimeMajorVersion st = Curry.Module.Distribution.curryRuntimeMajorVersion(st)



c_curryRuntimeMinorVersion :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_curryRuntimeMinorVersion st = Curry.Module.Distribution.curryRuntimeMinorVersion(st)



c_getStdLibDir :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_getStdLibDir st = Curry.Module.Distribution.getStdLibDir(st)



c_getFrontendCall :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char)
c_getFrontendCall st = Curry.Module.Distribution.getFrontendCall(st)



c_installDir :: Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_installDir st = Curry.Module.Distribution.installDir(st)


