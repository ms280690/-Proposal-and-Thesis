{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.EventOracle (module Curry.Module.EventOracle) where

import Curry.RunTimeSystem
import Curry.Module.IOExts
import Curry.Module.Prelude
import Curry.Module.System
import Curry.Module.Unsafe



-- begin included



-- end included

type C_Ref = Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node

type C_Cost = Curry.Module.Prelude.C_Int

data C_Node = C_Node (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) Curry.Module.Prelude.C_Int (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node)
  | C_Marker
  | C_Collapsed
  | C_NodeFail Curry.RunTimeSystem.C_Exceptions
  | C_NodeOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches Curry.Module.EventOracle.C_Node)

instance BaseCurry Curry.Module.EventOracle.C_Node where
  nf f (Curry.Module.EventOracle.C_Node x1 x2 x3) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> f(Curry.Module.EventOracle.C_Node(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.EventOracle.C_Node x1 x2 x3) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> f(Curry.Module.EventOracle.C_Node(v1)(v2)(v3))(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.EventOracle.C_NodeOr(Curry.RunTimeSystem.mkRef(r)(3)(i))([Curry.Module.EventOracle.C_Node(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int)))),Curry.Module.EventOracle.C_Marker,Curry.Module.EventOracle.C_Collapsed]))(3)

  failed  = Curry.Module.EventOracle.C_NodeFail

  branching  = Curry.Module.EventOracle.C_NodeOr

  consKind (Curry.Module.EventOracle.C_NodeOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.EventOracle.C_NodeFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.EventOracle.C_NodeFail x) = x

  orRef (Curry.Module.EventOracle.C_NodeOr x _) = x

  branches (Curry.Module.EventOracle.C_NodeOr _ x) = x





instance Curry Curry.Module.EventOracle.C_Node where
  strEq (Curry.Module.EventOracle.C_Node x1 x2 x3) (Curry.Module.EventOracle.C_Node y1 y2 y3) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(st))(st)
  strEq Curry.Module.EventOracle.C_Marker Curry.Module.EventOracle.C_Marker st = Curry.Module.Prelude.strEqSuccess
  strEq Curry.Module.EventOracle.C_Collapsed Curry.Module.EventOracle.C_Collapsed st = Curry.Module.Prelude.strEqSuccess
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.EventOracle.C_Node x1 x2 x3) (Curry.Module.EventOracle.C_Node y1 y2 y3) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.genEq(x3)(y3)(st))(st))(st)
  eq Curry.Module.EventOracle.C_Marker Curry.Module.EventOracle.C_Marker st = Curry.Module.Prelude.C_True
  eq Curry.Module.EventOracle.C_Collapsed Curry.Module.EventOracle.C_Collapsed st = Curry.Module.Prelude.C_True
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.EventOracle.C_Node x1 x2 x3) st = Curry.Module.EventOracle.C_Node(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))
  propagate f Curry.Module.EventOracle.C_Marker st = Curry.Module.EventOracle.C_Marker
  propagate f Curry.Module.EventOracle.C_Collapsed st = Curry.Module.EventOracle.C_Collapsed

  foldCurry f c (Curry.Module.EventOracle.C_Node x1 x2 x3) st = f(x1)(f(x2)(f(x3)(c)(st))(st))(st)
  foldCurry f c Curry.Module.EventOracle.C_Marker st = c
  foldCurry f c Curry.Module.EventOracle.C_Collapsed st = c

  typeName _ = "Node"

  showQ d (Curry.Module.EventOracle.C_Node x1 x2 x3) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("EventOracle.Node "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))))))


  showQ _ Curry.Module.EventOracle.C_Marker = Prelude.showString("EventOracle.Marker")
  showQ _ Curry.Module.EventOracle.C_Collapsed = Prelude.showString("EventOracle.Collapsed")
  showQ _ (Curry.Module.EventOracle.C_NodeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Show Curry.Module.EventOracle.C_Node where
  showsPrec d (Curry.Module.EventOracle.C_Node x1 x2 x3) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Node "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))))))


  showsPrec _ Curry.Module.EventOracle.C_Marker = Prelude.showString("Marker")
  showsPrec _ Curry.Module.EventOracle.C_Collapsed = Prelude.showString("Collapsed")
  showsPrec _ (Curry.Module.EventOracle.C_NodeOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance Read Curry.Module.EventOracle.C_Node where
  readsPrec d r = (Prelude.++)(Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.EventOracle.C_Node(x1)(x2)(x3))(r3) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("EventOracle")("Node")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2)])(r))((Prelude.++)(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.EventOracle.C_Marker)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("EventOracle")("Marker")(r)])(r))(Prelude.readParen(Prelude.False)(\ r -> [(,)(Curry.Module.EventOracle.C_Collapsed)(r0) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("EventOracle")("Collapsed")(r)])(r)))





c_initialize :: (Curry t0) => (Curry.Module.Prelude.Prim ((Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t0)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_initialize x1 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.System.c_getProgName(st))(Curry.Module.Prelude.pf(Curry.Module.EventOracle.c_initialize'46_'35lambda2(x1)))(st)



c_initialize'46_'35lambda2 :: (Curry t59) => (Curry.Module.Prelude.Prim ((Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t59)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_initialize'46_'35lambda2 x1 x2 st = let {x3 = Curry.Module.Prelude.op_43_43(x2)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))(Curry.Module.Prelude.List)))))(st)} in Curry.Module.Prelude.op_62_62(Curry.Module.IOExts.c_setAssoc((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))(Curry.Module.Prelude.List))))))(x3)(st))(Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_writeFile(x3)(Curry.Module.Prelude.List)(st))(Curry.Module.Prelude.op_62_62_61(Curry.Module.IOExts.c_newIORef(Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))))))))))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.EventOracle.c_initialize'46_'35lambda2'46_'35lambda3(x1)(x2)))(st))(st))(st)



c_initialize'46_'35lambda2'46_'35lambda3 :: (Curry t59) => (Curry.Module.Prelude.Prim ((Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t59)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_initialize'46_'35lambda2'46_'35lambda3 x1 x2 x3 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.IOExts.c_newIORef(Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('k'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))(Curry.Module.Prelude.List))))))))))(st))(st))(Curry.Module.Prelude.pf(Curry.Module.EventOracle.c_initialize'46_'35lambda2'46_'35lambda3'46_'35lambda4(x1)(x2)(x3)))(st)



c_initialize'46_'35lambda2'46_'35lambda3'46_'35lambda4 :: (Curry t59) => (Curry.Module.Prelude.Prim ((Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t59)) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_initialize'46_'35lambda2'46_'35lambda3'46_'35lambda4 x1 x2 x3 x4 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.IOExts.c_newIORef(Curry.Module.EventOracle.C_Node(x3)(Curry.Module.Prelude.C_Zero)(x4))(st))(Curry.Module.Prelude.pf(Curry.Module.EventOracle.c_initialize'46_'35lambda2'46_'35lambda3'46_'35lambda4'46_'35lambda5(x1)(x4)(x2)(x3)))(st)



c_initialize'46_'35lambda2'46_'35lambda3'46_'35lambda4'46_'35lambda5 :: (Curry t59) => (Curry.Module.Prelude.Prim ((Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t59)) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_initialize'46_'35lambda2'46_'35lambda3'46_'35lambda4'46_'35lambda5 x1 x2 x3 x4 x5 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.IOExts.c_newIORef(Curry.Module.EventOracle.C_Marker)(st))(Curry.Module.Prelude.pf(Curry.Module.EventOracle.c_initialize'46_'35lambda2'46_'35lambda3'46_'35lambda4'46_'35lambda5'46_'35lambda6(x1)(x2)(x5)(x3)(x4)))(st)



c_initialize'46_'35lambda2'46_'35lambda3'46_'35lambda4'46_'35lambda5'46_'35lambda6 :: (Curry t59) => (Curry.Module.Prelude.Prim ((Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t59)) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_initialize'46_'35lambda2'46_'35lambda3'46_'35lambda4'46_'35lambda5'46_'35lambda6 x1 x2 x3 x4 x5 x6 st = Curry.Module.Prelude.op_62_62(Curry.Module.IOExts.c_writeIORef(x5)(Curry.Module.EventOracle.C_Node(x6)(Curry.Module.Prelude.C_Zero)(x3))(st))(Curry.Module.Prelude.op_62_62(Curry.Module.IOExts.c_writeIORef(x2)(Curry.Module.EventOracle.C_Node(x3)(Curry.Module.Prelude.C_Zero)(x6))(st))(Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_apply(x1)(x3)(st))(Curry.Module.Prelude.pf(Curry.Module.EventOracle.c_initialize'46_'35lambda2'46_'35lambda3'46_'35lambda4'46_'35lambda5'46_'35lambda6'46_'35lambda7(x2)(x4)(x5)))(st))(st))(st)



c_initialize'46_'35lambda2'46_'35lambda3'46_'35lambda4'46_'35lambda5'46_'35lambda6'46_'35lambda7 :: (Curry t59) => (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> t59 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_initialize'46_'35lambda2'46_'35lambda3'46_'35lambda4'46_'35lambda5'46_'35lambda6'46_'35lambda7 x1 x2 x3 x4 st = Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.op_36_33_33(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_return))(x4)(st))(Curry.Module.Prelude.op_62_62(Curry.Module.Prelude.c_putStrLn((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('z'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))(Curry.Module.Prelude.List)))))))))))(st))(Curry.Module.Prelude.op_62_62(Curry.Module.EventOracle.c_finalize(x2)(x3)(x1)(st))(Curry.Module.Prelude.c_return(Curry.Module.Prelude.T0)(st))(st))(st))(st)



c_finalize :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_finalize x1 x2 x3 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.EventOracle.c_pointerToList(x2)(x3)(st))(Curry.Module.Prelude.pf(Curry.Module.EventOracle.c_finalize'46_'35lambda8(x1)))(st)



c_finalize'46_'35lambda8 :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_finalize'46_'35lambda8 x1 x2@((Curry.Module.Prelude.:<) x3 x4) st = Curry.Module.Prelude.c_writeFile(Curry.Module.Prelude.op_43_43(x1)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))(Curry.Module.Prelude.List)))))))(st))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_show(x4)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('.'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('\n'))(Curry.Module.Prelude.List))))(st))(st)
c_finalize'46_'35lambda8 x1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.EventOracle.c_finalize'46_'35lambda8(x1)(x)(st))(i)(xs)(st)
c_finalize'46_'35lambda8 x1 x st = Curry.RunTimeSystem.patternFail("EventOracle.finalize._#lambda8")(x)



c_fresh :: Curry.Module.Prelude.T0 -> Curry.RunTimeSystem.State -> Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node
c_fresh x1 st = Curry.Module.Unsafe.c_unsafePerformIO(Curry.Module.IOExts.c_newIORef(Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))(Curry.Module.Prelude.List))))))(st))(st))(st)



c_replace :: (Curry t0) => (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> t0 -> Curry.RunTimeSystem.State -> t0
c_replace x1 x2 st = Curry.Module.Unsafe.c_unsafePerformIO(Curry.Module.Prelude.op_62_62_61(Curry.Module.IOExts.c_readIORef(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.EventOracle.c_replace'46_'35lambda9(x1)(x2)))(st))(st)



c_replace'46_'35lambda9 :: (Curry t76) => (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> t76 -> Curry.Module.EventOracle.C_Node -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t76
c_replace'46_'35lambda9 x1 x2 x3 st = Curry.Module.Prelude.op_62_62(Curry.Module.EventOracle.c_replace'46_'35lambda9_case_4(x1)(x3)(st))(Curry.Module.Prelude.c_return(x2)(st))(st)



c_collapse :: (Curry t0) => (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> t0 -> Curry.RunTimeSystem.State -> t0
c_collapse x1 x2 st = Curry.Module.Unsafe.c_unsafePerformIO(Curry.Module.Prelude.op_62_62_61(Curry.Module.IOExts.c_readIORef(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.EventOracle.c_collapse'46_'35lambda11(x1)(x2)))(st))(st)



c_collapse'46_'35lambda11 :: (Curry t93) => (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> t93 -> Curry.Module.EventOracle.C_Node -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t93
c_collapse'46_'35lambda11 x1 x2 x3 st = Curry.Module.Prelude.op_62_62(Curry.Module.EventOracle.c_collapse'46_'35lambda11_case_3(x1)(x3)(st))(Curry.Module.Prelude.c_return(x2)(st))(st)



c_collapse'46_'35lambda11'46_'35lambda13 :: Curry.Module.Prelude.C_Int -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.Module.EventOracle.C_Node -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_collapse'46_'35lambda11'46_'35lambda13 x1 x2 x3 x4 x5@(Curry.Module.EventOracle.C_Node x6 x7 x8) st = Curry.Module.Prelude.op_62_62_61(Curry.Module.IOExts.c_readIORef(x4)(st))(Curry.Module.Prelude.pf(Curry.Module.EventOracle.c_collapse'46_'35lambda11'46_'35lambda13'46_'35lambda14(x1)(x2)(x7)(x6)(x3)(x4)))(st)
c_collapse'46_'35lambda11'46_'35lambda13 x1 x2 x3 x4 (Curry.Module.EventOracle.C_NodeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.EventOracle.c_collapse'46_'35lambda11'46_'35lambda13(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_collapse'46_'35lambda11'46_'35lambda13 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("EventOracle.collapse._#lambda11._#lambda13")(x)



c_collapse'46_'35lambda11'46_'35lambda13'46_'35lambda14 :: Curry.Module.Prelude.C_Int -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.Module.Prelude.C_Int -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.Module.EventOracle.C_Node -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_collapse'46_'35lambda11'46_'35lambda13'46_'35lambda14 x1 x2 x3 x4 x5 x6 x7@(Curry.Module.EventOracle.C_Node x8 x9 x10) st = Curry.Module.Prelude.op_62_62(Curry.Module.IOExts.c_writeIORef(x5)(Curry.Module.EventOracle.C_Collapsed)(st))(Curry.Module.Prelude.op_62_62(Curry.Module.IOExts.c_writeIORef(x2)(Curry.Module.EventOracle.C_Node(x4)(x3)(x6))(st))(Curry.Module.Prelude.op_36_33_33(Curry.Module.Prelude.pf(Curry.Module.IOExts.c_writeIORef(x6)))(Curry.Module.EventOracle.C_Node(x2)(Curry.Module.Prelude.op_43(Curry.Module.Prelude.op_43(x1)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x9)(st))(x10))(st))(st))(st)
c_collapse'46_'35lambda11'46_'35lambda13'46_'35lambda14 x1 x2 x3 x4 x5 x6 (Curry.Module.EventOracle.C_NodeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.EventOracle.c_collapse'46_'35lambda11'46_'35lambda13'46_'35lambda14(x1)(x2)(x3)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_collapse'46_'35lambda11'46_'35lambda13'46_'35lambda14 x1 x2 x3 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("EventOracle.collapse._#lambda11._#lambda13._#lambda14")(x)



c_closeRef :: (Curry t0) => (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> t0 -> Curry.RunTimeSystem.State -> t0
c_closeRef x1 x2 st = Curry.Module.Unsafe.c_unsafePerformIO(Curry.Module.Prelude.op_62_62_61(Curry.Module.IOExts.c_readIORef(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.EventOracle.c_closeRef'46_'35lambda15(x1)(x2)))(st))(st)



c_closeRef'46_'35lambda15 :: (Curry t123) => (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> t123 -> Curry.Module.EventOracle.C_Node -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t123
c_closeRef'46_'35lambda15 x1 x2 x3 st = Curry.Module.Prelude.op_62_62(Curry.Module.EventOracle.c_closeRef'46_'35lambda15_case_2(x1)(x3)(st))(Curry.Module.Prelude.c_return(x2)(st))(st)



c_closeRef'46_'35lambda15'46_'35lambda17 :: Curry.Module.Prelude.C_Int -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.Module.EventOracle.C_Node -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_closeRef'46_'35lambda15'46_'35lambda17 x1 x2 x3 x4 x5@(Curry.Module.EventOracle.C_Node x6 x7 x8) st = Curry.Module.Prelude.op_62_62_61(Curry.Module.IOExts.c_readIORef(x4)(st))(Curry.Module.Prelude.pf(Curry.Module.EventOracle.c_closeRef'46_'35lambda15'46_'35lambda17'46_'35lambda18(x1)(x2)(x7)(x6)(x3)(x4)))(st)
c_closeRef'46_'35lambda15'46_'35lambda17 x1 x2 x3 x4 (Curry.Module.EventOracle.C_NodeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.EventOracle.c_closeRef'46_'35lambda15'46_'35lambda17(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_closeRef'46_'35lambda15'46_'35lambda17 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("EventOracle.closeRef._#lambda15._#lambda17")(x)



c_closeRef'46_'35lambda15'46_'35lambda17'46_'35lambda18 :: Curry.Module.Prelude.C_Int -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.Module.Prelude.C_Int -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.Module.EventOracle.C_Node -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_closeRef'46_'35lambda15'46_'35lambda17'46_'35lambda18 x1 x2 x3 x4 x5 x6 x7@(Curry.Module.EventOracle.C_Node x8 x9 x10) st = Curry.Module.Prelude.op_62_62(Curry.Module.IOExts.c_writeIORef(x5)(Curry.Module.EventOracle.C_Collapsed)(st))(Curry.Module.Prelude.op_62_62(Curry.Module.IOExts.c_writeIORef(x2)(Curry.Module.EventOracle.C_Node(x4)(x3)(x6))(st))(Curry.Module.Prelude.op_36_33_33(Curry.Module.Prelude.pf(Curry.Module.IOExts.c_writeIORef(x6)))(Curry.Module.EventOracle.C_Node(x2)(Curry.Module.Prelude.op_43(x1)(x9)(st))(x10))(st))(st))(st)
c_closeRef'46_'35lambda15'46_'35lambda17'46_'35lambda18 x1 x2 x3 x4 x5 x6 (Curry.Module.EventOracle.C_NodeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.EventOracle.c_closeRef'46_'35lambda15'46_'35lambda17'46_'35lambda18(x1)(x2)(x3)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_closeRef'46_'35lambda15'46_'35lambda17'46_'35lambda18 x1 x2 x3 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("EventOracle.closeRef._#lambda15._#lambda17._#lambda18")(x)



c_expand :: (Curry t0) => (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.Prelude.List (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node)) -> t0 -> Curry.RunTimeSystem.State -> t0
c_expand x1 x2 x3 st = Curry.Module.Unsafe.c_unsafePerformIO(Curry.Module.Prelude.op_62_62_61(Curry.Module.IOExts.c_readIORef(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.EventOracle.c_expand'46_'35lambda19(x1)(x2)(x3)))(st))(st)



c_expand'46_'35lambda19 :: (Curry t175) => (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.Prelude.List (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node)) -> t175 -> Curry.Module.EventOracle.C_Node -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO t175
c_expand'46_'35lambda19 x1 x2 x3 x4 st = Curry.Module.Prelude.op_62_62(Curry.Module.EventOracle.c_expand'46_'35lambda19_case_1(x1)(x2)(x4)(st))(Curry.Module.Prelude.c_return(x3)(st))(st)



c_expand'46_'35lambda19'46_'35lambda21 :: (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_expand'46_'35lambda19'46_'35lambda21 x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.IOExts.c_readIORef(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.EventOracle.c_expand'46_'35lambda19'46_'35lambda21'46_'35lambda22(x2)(x1)))(st)



c_expand'46_'35lambda19'46_'35lambda21'46_'35lambda22 :: (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.Module.EventOracle.C_Node -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_expand'46_'35lambda19'46_'35lambda21'46_'35lambda22 x1 x2 x3@Curry.Module.EventOracle.C_Collapsed st = Curry.Module.EventOracle.c_warning((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('u'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))))))))))))))))))(st)
c_expand'46_'35lambda19'46_'35lambda21'46_'35lambda22 x1 x2 x3@(Curry.Module.EventOracle.C_Node x4 x5 x6) st = Curry.Module.IOExts.c_writeIORef(x2)(Curry.Module.EventOracle.C_Node(x1)(x5)(x6))(st)
c_expand'46_'35lambda19'46_'35lambda21'46_'35lambda22 x1 x2 x3@Curry.Module.EventOracle.C_Marker st = Curry.Module.Prelude.c_done(st)
c_expand'46_'35lambda19'46_'35lambda21'46_'35lambda22 x1 x2 (Curry.Module.EventOracle.C_NodeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.EventOracle.c_expand'46_'35lambda19'46_'35lambda21'46_'35lambda22(x1)(x2)(x)(st))(i)(xs)(st)
c_expand'46_'35lambda19'46_'35lambda21'46_'35lambda22 x1 x2 x st = Curry.RunTimeSystem.patternFail("EventOracle.expand._#lambda19._#lambda21._#lambda22")(x)



c_toAssocList :: Curry.Module.Prelude.C_Int -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.Prelude.List (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node)) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node)
c_toAssocList x1 x2 x3@((Curry.Module.Prelude.:<) x5 x6) x4 st = Curry.Module.EventOracle.c_toAssocList_case_0(x1)(x2)(x4)(x5)(x6)(st)
c_toAssocList x1 x2 (Curry.Module.Prelude.ListOr i xs) x4 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.EventOracle.c_toAssocList(x1)(x2)(x)(x4)(st))(i)(xs)(st)
c_toAssocList x1 x2 x x4 st = Curry.RunTimeSystem.patternFail("EventOracle.toAssocList")(x)



c_pointerToList :: (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)
c_pointerToList x1 x2 st = Curry.Module.Prelude.op_62_62_61(Curry.Module.IOExts.c_readIORef(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.EventOracle.c_pointerToList'46_'35lambda24(x2)))(st)



c_pointerToList'46_'35lambda24 :: (Curry.Module.IOExts.C_IORef Curry.Module.EventOracle.C_Node) -> Curry.Module.EventOracle.C_Node -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)
c_pointerToList'46_'35lambda24 x1 x2@Curry.Module.EventOracle.C_Collapsed st = Curry.Module.Prelude.op_62_62(Curry.Module.EventOracle.c_warning((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))))))))))(st))(Curry.Module.Prelude.c_return(Curry.Module.Prelude.List)(st))(st)
c_pointerToList'46_'35lambda24 x1 x2@(Curry.Module.EventOracle.C_Node x3 x4 x5) st = Curry.Module.Prelude.op_62_62_61(Curry.Module.EventOracle.c_pointerToList(x5)(x1)(st))(Curry.Module.Prelude.pf(Curry.Module.EventOracle.c_pointerToList'46_'35lambda24'46_'35lambda26(x4)))(st)
c_pointerToList'46_'35lambda24 x1 x2@Curry.Module.EventOracle.C_Marker st = Curry.Module.Prelude.c_return(Curry.Module.Prelude.List)(st)
c_pointerToList'46_'35lambda24 x1 (Curry.Module.EventOracle.C_NodeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.EventOracle.c_pointerToList'46_'35lambda24(x1)(x)(st))(i)(xs)(st)
c_pointerToList'46_'35lambda24 x1 x st = Curry.RunTimeSystem.patternFail("EventOracle.pointerToList._#lambda24")(x)



c_pointerToList'46_'35lambda24'46_'35lambda26 :: Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)
c_pointerToList'46_'35lambda24'46_'35lambda26 x1 x2 st = Curry.Module.Prelude.c_return((Curry.Module.Prelude.:<)(x1)(x2))(st)



c_warning :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Char) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_IO Curry.Module.Prelude.T0
c_warning x1 st = Curry.Module.Prelude.op_36(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_putStrLn))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('W'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('A'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('R'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('I'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('G'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))(x1)(st))(st)



c_toAssocList_case_0 x1 x2 x4 x5 x6@Curry.Module.Prelude.List st = Curry.Module.Prelude.op_62_62(Curry.Module.IOExts.c_writeIORef(x5)(Curry.Module.EventOracle.C_Node(x2)(x1)(x4))(st))(Curry.Module.Prelude.c_return(x5)(st))(st)
c_toAssocList_case_0 x1 x2 x4 x5 x6@((Curry.Module.Prelude.:<) x7 x8) st = Curry.Module.Prelude.op_62_62(Curry.Module.IOExts.c_writeIORef(x5)(Curry.Module.EventOracle.C_Node(x2)(x1)(x7))(st))(Curry.Module.EventOracle.c_toAssocList(Curry.Module.Prelude.C_Zero)(x5)(x6)(x4)(st))(st)
c_toAssocList_case_0 x1 x2 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.EventOracle.c_toAssocList_case_0(x1)(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c_toAssocList_case_0 x1 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("EventOracle.toAssocList_case_0")(x)



c_expand'46_'35lambda19_case_1 x1 x2 x4@Curry.Module.EventOracle.C_Collapsed st = Curry.Module.EventOracle.c_warning((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))(st)
c_expand'46_'35lambda19_case_1 x1 x2 x4@(Curry.Module.EventOracle.C_Node x5 x6 x7) st = Curry.Module.Prelude.op_62_62_61(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.op_36_33_33(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))))(Curry.Module.EventOracle.c_toAssocList))(Curry.Module.Prelude.op_43(x6)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(x5)(st))((Curry.Module.Prelude.:<)(x1)(x2))(st))(x7)(st))(Curry.Module.Prelude.pf(Curry.Module.EventOracle.c_expand'46_'35lambda19'46_'35lambda21(x7)))(st)
c_expand'46_'35lambda19_case_1 x1 x2 (Curry.Module.EventOracle.C_NodeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.EventOracle.c_expand'46_'35lambda19_case_1(x1)(x2)(x)(st))(i)(xs)(st)
c_expand'46_'35lambda19_case_1 x1 x2 x st = Curry.RunTimeSystem.patternFail("EventOracle.expand._#lambda19_case_1")(x)



c_closeRef'46_'35lambda15_case_2 x1 x3@Curry.Module.EventOracle.C_Collapsed st = Curry.Module.EventOracle.c_warning((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))(st)
c_closeRef'46_'35lambda15_case_2 x1 x3@(Curry.Module.EventOracle.C_Node x4 x5 x6) st = Curry.Module.Prelude.op_62_62_61(Curry.Module.IOExts.c_readIORef(x4)(st))(Curry.Module.Prelude.pf(Curry.Module.EventOracle.c_closeRef'46_'35lambda15'46_'35lambda17(x5)(x4)(x1)(x6)))(st)
c_closeRef'46_'35lambda15_case_2 x1 (Curry.Module.EventOracle.C_NodeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.EventOracle.c_closeRef'46_'35lambda15_case_2(x1)(x)(st))(i)(xs)(st)
c_closeRef'46_'35lambda15_case_2 x1 x st = Curry.RunTimeSystem.patternFail("EventOracle.closeRef._#lambda15_case_2")(x)



c_collapse'46_'35lambda11_case_3 x1 x3@Curry.Module.EventOracle.C_Collapsed st = Curry.Module.EventOracle.c_warning((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))(Curry.Module.Prelude.List))))))))))))))))))))))))))))))))(st)
c_collapse'46_'35lambda11_case_3 x1 x3@(Curry.Module.EventOracle.C_Node x4 x5 x6) st = Curry.Module.Prelude.op_62_62_61(Curry.Module.IOExts.c_readIORef(x4)(st))(Curry.Module.Prelude.pf(Curry.Module.EventOracle.c_collapse'46_'35lambda11'46_'35lambda13(x5)(x4)(x1)(x6)))(st)
c_collapse'46_'35lambda11_case_3 x1 (Curry.Module.EventOracle.C_NodeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.EventOracle.c_collapse'46_'35lambda11_case_3(x1)(x)(st))(i)(xs)(st)
c_collapse'46_'35lambda11_case_3 x1 x st = Curry.RunTimeSystem.patternFail("EventOracle.collapse._#lambda11_case_3")(x)



c_replace'46_'35lambda9_case_4 x1 x3@Curry.Module.EventOracle.C_Collapsed st = Curry.Module.EventOracle.c_warning((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('l'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('s'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('f'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))))(st)
c_replace'46_'35lambda9_case_4 x1 x3@(Curry.Module.EventOracle.C_Node x4 x5 x6) st = Curry.Module.Prelude.op_36_33_33(Curry.Module.Prelude.pf(Curry.Module.IOExts.c_writeIORef(x1)))(Curry.Module.EventOracle.C_Node(x4)(Curry.Module.Prelude.op_43(x5)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x6))(st)
c_replace'46_'35lambda9_case_4 x1 (Curry.Module.EventOracle.C_NodeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.EventOracle.c_replace'46_'35lambda9_case_4(x1)(x)(st))(i)(xs)(st)
c_replace'46_'35lambda9_case_4 x1 x st = Curry.RunTimeSystem.patternFail("EventOracle.replace._#lambda9_case_4")(x)


