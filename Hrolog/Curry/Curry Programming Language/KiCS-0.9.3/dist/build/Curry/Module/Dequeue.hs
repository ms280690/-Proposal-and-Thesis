{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.Dequeue (module Curry.Module.Dequeue) where

import Curry.RunTimeSystem
import Curry.Module.Prelude



-- begin included



-- end included

data C_Queue t0 = C_S Curry.Module.Prelude.C_Int (Curry.Module.Prelude.List t0) Curry.Module.Prelude.C_Int (Curry.Module.Prelude.List t0)
  | C_QueueFail Curry.RunTimeSystem.C_Exceptions
  | C_QueueOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.Dequeue.C_Queue t0))

instance (BaseCurry t0) => BaseCurry (Curry.Module.Dequeue.C_Queue t0) where
  nf f (Curry.Module.Dequeue.C_S x1 x2 x3 x4) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> Curry.RunTimeSystem.nfCTC(\ v2 state2 -> Curry.RunTimeSystem.nfCTC(\ v3 state3 -> Curry.RunTimeSystem.nfCTC(\ v4 state4 -> f(Curry.Module.Dequeue.C_S(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.Dequeue.C_S x1 x2 x3 x4) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> Curry.RunTimeSystem.gnfCTC(\ v2 state2 -> Curry.RunTimeSystem.gnfCTC(\ v3 state3 -> Curry.RunTimeSystem.gnfCTC(\ v4 state4 -> f(Curry.Module.Dequeue.C_S(v1)(v2)(v3)(v4))(state4))(x4)(state3))(x3)(state2))(x2)(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.Dequeue.C_QueueOr(Curry.RunTimeSystem.mkRef(r)(4)(i))([Curry.Module.Dequeue.C_S(Curry.RunTimeSystem.generator((Prelude.+)(r)((3::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((2::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((1::Int))))(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(4)

  failed  = Curry.Module.Dequeue.C_QueueFail

  branching  = Curry.Module.Dequeue.C_QueueOr

  consKind (Curry.Module.Dequeue.C_QueueOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.Dequeue.C_QueueFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.Dequeue.C_QueueFail x) = x

  orRef (Curry.Module.Dequeue.C_QueueOr x _) = x

  branches (Curry.Module.Dequeue.C_QueueOr _ x) = x





instance (Curry t0) => Curry (Curry.Module.Dequeue.C_Queue t0) where
  strEq (Curry.Module.Dequeue.C_S x1 x2 x3 x4) (Curry.Module.Dequeue.C_S y1 y2 y3 y4) st = Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x1)(y1)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x2)(y2)(st))(Curry.Module.Prelude.concAnd(Curry.Module.Prelude.genStrEq(x3)(y3)(st))(Curry.Module.Prelude.genStrEq(x4)(y4)(st))(st))(st))(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.Dequeue.C_S x1 x2 x3 x4) (Curry.Module.Dequeue.C_S y1 y2 y3 y4) st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x1)(y1)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x2)(y2)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.genEq(x3)(y3)(st))(Curry.Module.Prelude.genEq(x4)(y4)(st))(st))(st))(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.Dequeue.C_S x1 x2 x3 x4) st = Curry.Module.Dequeue.C_S(f((0::Int))(x1)(st))(f((1::Int))(x2)(st))(f((2::Int))(x3)(st))(f((3::Int))(x4)(st))

  foldCurry f c (Curry.Module.Dequeue.C_S x1 x2 x3 x4) st = f(x1)(f(x2)(f(x3)(f(x4)(c)(st))(st))(st))(st)

  typeName _ = "Queue"

  showQ d (Curry.Module.Dequeue.C_S x1 x2 x3 x4) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Dequeue.S "))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x4))))))))


  showQ _ (Curry.Module.Dequeue.C_QueueOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0) => Show (Curry.Module.Dequeue.C_Queue t0) where
  showsPrec d (Curry.Module.Dequeue.C_S x1 x2 x3 x4) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("S "))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x2))((Prelude..)(Prelude.showChar(' '))((Prelude..)(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x3))((Prelude..)(Prelude.showChar(' '))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x4))))))))


  showsPrec _ (Curry.Module.Dequeue.C_QueueOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Read t0) => Read (Curry.Module.Dequeue.C_Queue t0) where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.Dequeue.C_S(x1)(x2)(x3)(x4))(r4) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("Dequeue")("S")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0), ((,) x2 r2) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r1), ((,) x3 r3) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r2), ((,) x4 r4) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r3)])(r)





c_empty :: (Curry t0) => Curry.RunTimeSystem.State -> Curry.Module.Dequeue.C_Queue t0
c_empty st = Curry.Module.Dequeue.C_S(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.List)(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.List)



c_isEmpty :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isEmpty x1@(Curry.Module.Dequeue.C_S x2 x3 x4 x5) st = Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.op_43(x2)(x4)(st))(Curry.Module.Prelude.C_Zero)(st)
c_isEmpty (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_isEmpty(x)(st))(i)(xs)(st)
c_isEmpty x st = Curry.RunTimeSystem.patternFail("Dequeue.isEmpty")(x)



c_deqHead :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.RunTimeSystem.State -> t0
c_deqHead x1@(Curry.Module.Dequeue.C_S x2 x3 x4 x5) st = Curry.Module.Prelude.c_head(Curry.Module.Dequeue.c_deqHead_case_11(x2)(x3)(x5)(Curry.Module.Prelude.op_61_61(x2)(Curry.Module.Prelude.C_Zero)(st))(st))(st)
c_deqHead (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_deqHead(x)(st))(i)(xs)(st)
c_deqHead x st = Curry.RunTimeSystem.patternFail("Dequeue.deqHead")(x)



c_deqLast :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.RunTimeSystem.State -> t0
c_deqLast x1@(Curry.Module.Dequeue.C_S x2 x3 x4 x5) st = Curry.Module.Prelude.c_head(Curry.Module.Dequeue.c_deqLast_case_10(x3)(x4)(x5)(Curry.Module.Prelude.op_61_61(x4)(Curry.Module.Prelude.C_Zero)(st))(st))(st)
c_deqLast (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_deqLast(x)(st))(i)(xs)(st)
c_deqLast x st = Curry.RunTimeSystem.patternFail("Dequeue.deqLast")(x)



c_cons :: (Curry t0) => t0 -> (Curry.Module.Dequeue.C_Queue t0) -> Curry.RunTimeSystem.State -> Curry.Module.Dequeue.C_Queue t0
c_cons x1 x2@(Curry.Module.Dequeue.C_S x3 x4 x5 x6) st = Curry.Module.Dequeue.c_check(Curry.Module.Prelude.op_43(x3)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))((Curry.Module.Prelude.:<)(x1)(x4))(x5)(x6)(st)
c_cons x1 (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_cons(x1)(x)(st))(i)(xs)(st)
c_cons x1 x st = Curry.RunTimeSystem.patternFail("Dequeue.cons")(x)



c_deqTail :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.RunTimeSystem.State -> Curry.Module.Dequeue.C_Queue t0
c_deqTail x1@(Curry.Module.Dequeue.C_S x2 x3 x4 x5) st = Curry.Module.Dequeue.c_deqTail_case_9(x2)(x4)(x5)(x3)(st)
c_deqTail (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_deqTail(x)(st))(i)(xs)(st)
c_deqTail x st = Curry.RunTimeSystem.patternFail("Dequeue.deqTail")(x)



c_snoc :: (Curry t0) => t0 -> (Curry.Module.Dequeue.C_Queue t0) -> Curry.RunTimeSystem.State -> Curry.Module.Dequeue.C_Queue t0
c_snoc x1 x2@(Curry.Module.Dequeue.C_S x3 x4 x5 x6) st = Curry.Module.Dequeue.c_deqReverse(Curry.Module.Dequeue.c_check(Curry.Module.Prelude.op_43(x5)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))((Curry.Module.Prelude.:<)(x1)(x6))(x3)(x4)(st))(st)
c_snoc x1 (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_snoc(x1)(x)(st))(i)(xs)(st)
c_snoc x1 x st = Curry.RunTimeSystem.patternFail("Dequeue.snoc")(x)



c_deqInit :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.RunTimeSystem.State -> Curry.Module.Dequeue.C_Queue t0
c_deqInit x1@(Curry.Module.Dequeue.C_S x2 x3 x4 x5) st = Curry.Module.Dequeue.c_deqInit_case_8(x2)(x3)(x4)(x5)(st)
c_deqInit (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_deqInit(x)(st))(i)(xs)(st)
c_deqInit x st = Curry.RunTimeSystem.patternFail("Dequeue.deqInit")(x)



c_deqReverse :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.RunTimeSystem.State -> Curry.Module.Dequeue.C_Queue t0
c_deqReverse x1@(Curry.Module.Dequeue.C_S x2 x3 x4 x5) st = Curry.Module.Dequeue.C_S(x4)(x5)(x2)(x3)
c_deqReverse (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_deqReverse(x)(st))(i)(xs)(st)
c_deqReverse x st = Curry.RunTimeSystem.patternFail("Dequeue.deqReverse")(x)



c_check :: (Curry t0) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List t0) -> Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Dequeue.C_Queue t0
c_check x1 x2 x3 x4 st = let {x5 = Curry.Module.Prelude.op_43(x1)(x3)(st)} in let {x6 = Curry.Module.Prelude.c_div(x5)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_O(Curry.Module.Prelude.C_IHi)))(st)} in let {x8 = Curry.Module.Prelude.c_splitAt(x6)(x2)(st)} in Curry.Module.Dequeue.c_check_case_7(x1)(x2)(x3)(x4)(x5)(x6)(x8)(Curry.Module.Prelude.op_60_61(x1)(Curry.Module.Prelude.op_43(Curry.Module.Prelude.op_42(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_I(Curry.Module.Prelude.C_IHi)))(x3)(st))(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(st))(st)



c_check'46_'35selFP3'35f'39 :: (Curry t45) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t45) (Curry.Module.Prelude.List t45)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t45
c_check'46_'35selFP3'35f'39 x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_check'46_'35selFP3'35f'39 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_check'46_'35selFP3'35f'39(x)(st))(i)(xs)(st)
c_check'46_'35selFP3'35f'39 x st = Curry.RunTimeSystem.patternFail("Dequeue.check._#selFP3#f'")(x)



c_check'46_'35selFP4'35rf'39 :: (Curry t45) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.List t45) (Curry.Module.Prelude.List t45)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t45
c_check'46_'35selFP4'35rf'39 x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_check'46_'35selFP4'35rf'39 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_check'46_'35selFP4'35rf'39(x)(st))(i)(xs)(st)
c_check'46_'35selFP4'35rf'39 x st = Curry.RunTimeSystem.patternFail("Dequeue.check._#selFP4#rf'")(x)



c_listToDeq :: (Curry t0) => (Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Dequeue.C_Queue t0
c_listToDeq x1 st = Curry.Module.Dequeue.c_check(Curry.Module.Prelude.c_length(x1)(st))(x1)(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.List)(st)



c_deqToList :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0
c_deqToList x1@(Curry.Module.Dequeue.C_S x2 x3 x4 x5) st = Curry.Module.Prelude.op_43_43(x3)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_reverse(st))(x5)(st))(st)
c_deqToList (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_deqToList(x)(st))(i)(xs)(st)
c_deqToList x st = Curry.RunTimeSystem.patternFail("Dequeue.deqToList")(x)



c_deqLength :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_deqLength x1@(Curry.Module.Dequeue.C_S x2 x3 x4 x5) st = Curry.Module.Prelude.op_43(x2)(x4)(st)
c_deqLength (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_deqLength(x)(st))(i)(xs)(st)
c_deqLength x st = Curry.RunTimeSystem.patternFail("Dequeue.deqLength")(x)



c_rotate :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.RunTimeSystem.State -> Curry.Module.Dequeue.C_Queue t0
c_rotate x1 st = Curry.Module.Dequeue.c_snoc(Curry.Module.Dequeue.c_deqHead(x1)(st))(Curry.Module.Dequeue.c_deqTail(x1)(st))(st)



c_matchHead :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 t0 (Curry.Module.Dequeue.C_Queue t0))
c_matchHead x1@(Curry.Module.Dequeue.C_S x2 x3 x4 x5) st = Curry.Module.Dequeue.c_matchHead_case_5(x2)(x4)(x5)(x3)(st)
c_matchHead (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_matchHead(x)(st))(i)(xs)(st)
c_matchHead x st = Curry.RunTimeSystem.patternFail("Dequeue.matchHead")(x)



c_matchLast :: (Curry t0) => (Curry.Module.Dequeue.C_Queue t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T2 t0 (Curry.Module.Dequeue.C_Queue t0))
c_matchLast x1@(Curry.Module.Dequeue.C_S x2 x3 x4 x5) st = Curry.Module.Dequeue.c_matchLast_case_2(x2)(x3)(x4)(x5)(st)
c_matchLast (Curry.Module.Dequeue.C_QueueOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_matchLast(x)(st))(i)(xs)(st)
c_matchLast x st = Curry.RunTimeSystem.patternFail("Dequeue.matchLast")(x)



c_matchLast_case_2 x2 x3 x4 x5@Curry.Module.Prelude.List st = Curry.Module.Dequeue.c_matchLast_case_1(x3)(st)
c_matchLast_case_2 x2 x3 x4 x5@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.Prelude.C_Just(Curry.Module.Prelude.T2(x8)(Curry.Module.Dequeue.c_check(x2)(x3)(Curry.Module.Prelude.op_45(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x9)(st)))
c_matchLast_case_2 x2 x3 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_matchLast_case_2(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_matchLast_case_2 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("Dequeue.matchLast_case_2")(x)



c_matchLast_case_1 x3@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_matchLast_case_1 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.Dequeue.c_matchLast_case_0(x6)(x7)(st)
c_matchLast_case_1 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_matchLast_case_1(x)(st))(i)(xs)(st)
c_matchLast_case_1 x st = Curry.RunTimeSystem.patternFail("Dequeue.matchLast_case_1")(x)



c_matchLast_case_0 x6 x7@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Just(Curry.Module.Prelude.T2(x6)(Curry.Module.Dequeue.c_empty(st)))
c_matchLast_case_0 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_matchLast_case_0(x6)(x)(st))(i)(xs)(st)
c_matchLast_case_0 x6 x st = Curry.RunTimeSystem.patternFail("Dequeue.matchLast_case_0")(x)



c_matchHead_case_5 x2 x4 x5 x3@Curry.Module.Prelude.List st = Curry.Module.Dequeue.c_matchHead_case_4(x5)(st)
c_matchHead_case_5 x2 x4 x5 x3@((Curry.Module.Prelude.:<) x8 x9) st = Curry.Module.Prelude.C_Just(Curry.Module.Prelude.T2(x8)(Curry.Module.Dequeue.c_deqReverse(Curry.Module.Dequeue.c_check(x4)(x5)(Curry.Module.Prelude.op_45(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x9)(st))(st)))
c_matchHead_case_5 x2 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_matchHead_case_5(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c_matchHead_case_5 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("Dequeue.matchHead_case_5")(x)



c_matchHead_case_4 x5@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Nothing
c_matchHead_case_4 x5@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.Dequeue.c_matchHead_case_3(x6)(x7)(st)
c_matchHead_case_4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_matchHead_case_4(x)(st))(i)(xs)(st)
c_matchHead_case_4 x st = Curry.RunTimeSystem.patternFail("Dequeue.matchHead_case_4")(x)



c_matchHead_case_3 x6 x7@Curry.Module.Prelude.List st = Curry.Module.Prelude.C_Just(Curry.Module.Prelude.T2(x6)(Curry.Module.Dequeue.c_empty(st)))
c_matchHead_case_3 x6 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_matchHead_case_3(x6)(x)(st))(i)(xs)(st)
c_matchHead_case_3 x6 x st = Curry.RunTimeSystem.patternFail("Dequeue.matchHead_case_3")(x)



c_check_case_7 x1 x2 x3 x4 x5 x6 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.Dequeue.C_S(x1)(x2)(x3)(x4)
c_check_case_7 x1 x2 x3 x4 x5 x6 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.Dequeue.c_check_case_6(x4)(x5)(x6)(x8)(Curry.Module.Prelude.c_otherwise(st))(st)
c_check_case_7 x1 x2 x3 x4 x5 x6 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_check_case_7(x1)(x2)(x3)(x4)(x5)(x6)(x8)(x)(st))(i)(xs)(st)
c_check_case_7 x1 x2 x3 x4 x5 x6 x8 x st = Curry.RunTimeSystem.patternFail("Dequeue.check_case_7")(x)



c_check_case_6 x4 x5 x6 x8 x9@Curry.Module.Prelude.C_True st = Curry.Module.Dequeue.C_S(x6)(Curry.Module.Dequeue.c_check'46_'35selFP3'35f'39(x8)(st))(Curry.Module.Prelude.op_45(x5)(x6)(st))(Curry.Module.Prelude.op_43_43(x4)(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_reverse(st))(Curry.Module.Dequeue.c_check'46_'35selFP4'35rf'39(x8)(st))(st))(st))
c_check_case_6 x4 x5 x6 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_check_case_6(x4)(x5)(x6)(x8)(x)(st))(i)(xs)(st)
c_check_case_6 x4 x5 x6 x8 x st = Curry.RunTimeSystem.patternFail("Dequeue.check_case_6")(x)



c_deqInit_case_8 x2 x3 x4 x5@Curry.Module.Prelude.List st = Curry.Module.Dequeue.c_empty(st)
c_deqInit_case_8 x2 x3 x4 x5@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.Dequeue.c_check(x2)(x3)(Curry.Module.Prelude.op_45(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x7)(st)
c_deqInit_case_8 x2 x3 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_deqInit_case_8(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_deqInit_case_8 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("Dequeue.deqInit_case_8")(x)



c_deqTail_case_9 x2 x4 x5 x3@Curry.Module.Prelude.List st = Curry.Module.Dequeue.c_empty(st)
c_deqTail_case_9 x2 x4 x5 x3@((Curry.Module.Prelude.:<) x6 x7) st = Curry.Module.Dequeue.c_deqReverse(Curry.Module.Dequeue.c_check(x4)(x5)(Curry.Module.Prelude.op_45(x2)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(x7)(st))(st)
c_deqTail_case_9 x2 x4 x5 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_deqTail_case_9(x2)(x4)(x5)(x)(st))(i)(xs)(st)
c_deqTail_case_9 x2 x4 x5 x st = Curry.RunTimeSystem.patternFail("Dequeue.deqTail_case_9")(x)



c_deqLast_case_10 x3 x4 x5 x6@Curry.Module.Prelude.C_True st = x3
c_deqLast_case_10 x3 x4 x5 x6@Curry.Module.Prelude.C_False st = x5
c_deqLast_case_10 x3 x4 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_deqLast_case_10(x3)(x4)(x5)(x)(st))(i)(xs)(st)
c_deqLast_case_10 x3 x4 x5 x st = Curry.RunTimeSystem.patternFail("Dequeue.deqLast_case_10")(x)



c_deqHead_case_11 x2 x3 x5 x6@Curry.Module.Prelude.C_True st = x5
c_deqHead_case_11 x2 x3 x5 x6@Curry.Module.Prelude.C_False st = x3
c_deqHead_case_11 x2 x3 x5 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.Dequeue.c_deqHead_case_11(x2)(x3)(x5)(x)(st))(i)(xs)(st)
c_deqHead_case_11 x2 x3 x5 x st = Curry.RunTimeSystem.patternFail("Dequeue.deqHead_case_11")(x)


