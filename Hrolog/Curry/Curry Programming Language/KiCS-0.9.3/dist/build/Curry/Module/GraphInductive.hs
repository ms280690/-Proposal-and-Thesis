{-# OPTIONS -cpp -O0  #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.GraphInductive (module Curry.Module.GraphInductive) where

import Curry.RunTimeSystem
import Curry.Module.FiniteMap
import Curry.Module.Maybe
import Curry.Module.Prelude
import Curry.Module.Sort



-- begin included



-- end included

type C_Node = Curry.Module.Prelude.C_Int

type C_LNode t0 = Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0

type C_UNode = Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.T0

type C_Edge = Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int

type C_LEdge t0 = Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t0

type C_UEdge = Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int Curry.Module.Prelude.T0

type C_Context t0 t1 = Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t0 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int))

type C_Adj t0 = Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)

type C_MContext t0 t1 = Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t0 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)))

type C_Context'39 t0 t1 = Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)) t0 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int))

type C_UContext = Curry.Module.Prelude.T3 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) Curry.Module.Prelude.C_Int (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)

type C_GDecomp t0 t1 = Curry.Module.Prelude.T2 (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t0 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int))) (Curry.Module.GraphInductive.C_Graph t0 t1)

type C_Decomp t0 t1 = Curry.Module.Prelude.T2 (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t0 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)))) (Curry.Module.GraphInductive.C_Graph t0 t1)

type C_UDecomp t0 = Curry.Module.Prelude.T2 (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) Curry.Module.Prelude.C_Int (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))) t0

type C_Path = Curry.Module.Prelude.List Curry.Module.Prelude.C_Int

type C_LPath t0 = Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0)

type C_UPath = Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.T0)

type C_GraphRep t0 t1 = Curry.Module.FiniteMap.C_FM Curry.Module.Prelude.C_Int (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)) t0 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)))

type C_UGr = Curry.Module.GraphInductive.C_Graph Curry.Module.Prelude.T0 Curry.Module.Prelude.T0

data C_Graph t0 t1 = C_Gr (Curry.Module.FiniteMap.C_FM Curry.Module.Prelude.C_Int (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)) t0 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int))))
  | C_GraphFail Curry.RunTimeSystem.C_Exceptions
  | C_GraphOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.GraphInductive.C_Graph t0 t1))

instance (BaseCurry t0,BaseCurry t1) => BaseCurry (Curry.Module.GraphInductive.C_Graph t0 t1) where
  nf f (Curry.Module.GraphInductive.C_Gr x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.GraphInductive.C_Gr(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.GraphInductive.C_Gr x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.GraphInductive.C_Gr(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.GraphInductive.C_GraphOr(Curry.RunTimeSystem.mkRef(r)(1)(i))([Curry.Module.GraphInductive.C_Gr(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(1)

  failed  = Curry.Module.GraphInductive.C_GraphFail

  branching  = Curry.Module.GraphInductive.C_GraphOr

  consKind (Curry.Module.GraphInductive.C_GraphOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.GraphInductive.C_GraphFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.GraphInductive.C_GraphFail x) = x

  orRef (Curry.Module.GraphInductive.C_GraphOr x _) = x

  branches (Curry.Module.GraphInductive.C_GraphOr _ x) = x





instance (Curry t0,Curry t1) => Curry (Curry.Module.GraphInductive.C_Graph t0 t1) where
  strEq (Curry.Module.GraphInductive.C_Gr x1) (Curry.Module.GraphInductive.C_Gr y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.GraphInductive.C_Gr x1) (Curry.Module.GraphInductive.C_Gr y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.GraphInductive.C_Gr x1) st = Curry.Module.GraphInductive.C_Gr(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.GraphInductive.C_Gr x1) st = f(x1)(c)(st)

  typeName _ = "Graph"

  showQ d (Curry.Module.GraphInductive.C_Gr x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("GraphInductive.Gr "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.GraphInductive.C_GraphOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0,Show t1) => Show (Curry.Module.GraphInductive.C_Graph t0 t1) where
  showsPrec d (Curry.Module.GraphInductive.C_Gr x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Gr "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.GraphInductive.C_GraphOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Read t0,Read t1) => Read (Curry.Module.GraphInductive.C_Graph t0 t1) where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.GraphInductive.C_Gr(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("GraphInductive")("Gr")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r)





op_58_38 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> (Curry.Module.GraphInductive.C_Graph t1 t0) -> Curry.RunTimeSystem.State -> Curry.Module.GraphInductive.C_Graph t1 t0
op_58_38 x1@(Curry.Module.Prelude.T4 x3 x4 x5 x6) x2 st = Curry.Module.GraphInductive.c_'58'38_case_38(x3)(x4)(x5)(x6)(x2)(st)
op_58_38 (Curry.Module.Prelude.T4Or i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.op_58_38(x)(x2)(st))(i)(xs)(st)
op_58_38 x x2 st = Curry.RunTimeSystem.patternFail("GraphInductive.:&")(x)



c_matchAny :: (Curry t0,Curry t1) => (Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t0 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int))) (Curry.Module.GraphInductive.C_Graph t0 t1)
c_matchAny x1@(Curry.Module.GraphInductive.C_Gr x2) st = Curry.Module.GraphInductive.c_matchAny_case_35(x2)(Curry.Module.FiniteMap.c_isEmptyFM(x2)(st))(st)
c_matchAny (Curry.Module.GraphInductive.C_GraphOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_matchAny(x)(st))(i)(xs)(st)
c_matchAny x st = Curry.RunTimeSystem.patternFail("GraphInductive.matchAny")(x)



c_empty :: (Curry t0,Curry t1) => Curry.RunTimeSystem.State -> Curry.Module.GraphInductive.C_Graph t0 t1
c_empty st = Curry.Module.GraphInductive.C_Gr(Curry.Module.FiniteMap.c_emptyFM(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_60))(st))



c_mkGraph :: (Curry t0,Curry t1) => (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t1)) -> Curry.RunTimeSystem.State -> Curry.Module.GraphInductive.C_Graph t0 t1
c_mkGraph x1 x2 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_insEdges(x2)))(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_insNodes(x1)))(st))(Curry.Module.GraphInductive.c_empty(st))(st)



c_buildGr :: (Curry t0,Curry t1) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)))) -> Curry.RunTimeSystem.State -> Curry.Module.GraphInductive.C_Graph t1 t0)
c_buildGr st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.GraphInductive.op_58_38))(Curry.Module.GraphInductive.c_empty(st)))



c_mkUGraph :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int)) -> Curry.RunTimeSystem.State -> Curry.Module.GraphInductive.C_Graph Curry.Module.Prelude.T0 Curry.Module.Prelude.T0
c_mkUGraph x1 x2 st = Curry.Module.GraphInductive.c_mkGraph(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.c_labUNodes(st))(x1)(st))(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.c_labUEdges(st))(x2)(st))(st)



c_insNode :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.GraphInductive.C_Graph t0 t1)
c_insNode x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.Prelude.pf(Curry.Module.GraphInductive.op_58_38(Curry.Module.Prelude.T4(Curry.Module.Prelude.List)(x2)(x3)(Curry.Module.Prelude.List)))
c_insNode (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_insNode(x)(st))(i)(xs)(st)
c_insNode x st = Curry.RunTimeSystem.patternFail("GraphInductive.insNode")(x)



c_insEdge :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t0) -> (Curry.Module.GraphInductive.C_Graph t1 t0) -> Curry.RunTimeSystem.State -> Curry.Module.GraphInductive.C_Graph t1 t0
c_insEdge x1@(Curry.Module.Prelude.T3 x3 x4 x5) x2 st = let {x6 = Curry.Module.GraphInductive.c_match(x3)(x2)(st)} in Curry.Module.GraphInductive.op_58_38(Curry.Module.Prelude.T4(Curry.Module.GraphInductive.c_insEdge'46_'35selFP3'35pr(x6)(st))(x3)(Curry.Module.GraphInductive.c_insEdge'46_'35selFP4'35la(x6)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(x5)(x4))(Curry.Module.GraphInductive.c_insEdge'46_'35selFP5'35su(x6)(st))))(Curry.Module.GraphInductive.c_insEdge'46_'35selFP6'35g'39(x6)(st))(st)
c_insEdge (Curry.Module.Prelude.T3Or i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_insEdge(x)(x2)(st))(i)(xs)(st)
c_insEdge x x2 st = Curry.RunTimeSystem.patternFail("GraphInductive.insEdge")(x)



c_insEdge'46_'35selFP3'35pr :: (Curry t213,Curry t212) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t212 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)))) (Curry.Module.GraphInductive.C_Graph t212 t213)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)
c_insEdge'46_'35selFP3'35pr x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.GraphInductive.c_insEdge'46_'35selFP3'35pr_case_30(x2)(st)
c_insEdge'46_'35selFP3'35pr (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_insEdge'46_'35selFP3'35pr(x)(st))(i)(xs)(st)
c_insEdge'46_'35selFP3'35pr x st = Curry.RunTimeSystem.patternFail("GraphInductive.insEdge._#selFP3#pr")(x)



c_insEdge'46_'35selFP4'35la :: (Curry t213,Curry t212) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t212 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)))) (Curry.Module.GraphInductive.C_Graph t212 t213)) -> Curry.RunTimeSystem.State -> t212
c_insEdge'46_'35selFP4'35la x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.GraphInductive.c_insEdge'46_'35selFP4'35la_case_28(x2)(st)
c_insEdge'46_'35selFP4'35la (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_insEdge'46_'35selFP4'35la(x)(st))(i)(xs)(st)
c_insEdge'46_'35selFP4'35la x st = Curry.RunTimeSystem.patternFail("GraphInductive.insEdge._#selFP4#la")(x)



c_insEdge'46_'35selFP5'35su :: (Curry t213,Curry t212) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t212 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)))) (Curry.Module.GraphInductive.C_Graph t212 t213)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)
c_insEdge'46_'35selFP5'35su x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.GraphInductive.c_insEdge'46_'35selFP5'35su_case_26(x2)(st)
c_insEdge'46_'35selFP5'35su (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_insEdge'46_'35selFP5'35su(x)(st))(i)(xs)(st)
c_insEdge'46_'35selFP5'35su x st = Curry.RunTimeSystem.patternFail("GraphInductive.insEdge._#selFP5#su")(x)



c_insEdge'46_'35selFP6'35g'39 :: (Curry t213,Curry t212) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t212 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)))) (Curry.Module.GraphInductive.C_Graph t212 t213)) -> Curry.RunTimeSystem.State -> Curry.Module.GraphInductive.C_Graph t212 t213
c_insEdge'46_'35selFP6'35g'39 x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.GraphInductive.c_insEdge'46_'35selFP6'35g'39_case_24(x3)(x2)(st)
c_insEdge'46_'35selFP6'35g'39 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_insEdge'46_'35selFP6'35g'39(x)(st))(i)(xs)(st)
c_insEdge'46_'35selFP6'35g'39 x st = Curry.RunTimeSystem.patternFail("GraphInductive.insEdge._#selFP6#g'")(x)



c_delNode :: (Curry t0,Curry t1) => Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.GraphInductive.C_Graph t0 t1)
c_delNode x1 st = Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_delNodes((Curry.Module.Prelude.:<)(x1)(Curry.Module.Prelude.List)))



c_delEdge :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> (Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.GraphInductive.C_Graph t0 t1
c_delEdge x1@(Curry.Module.Prelude.T2 x3 x4) x2 st = Curry.Module.GraphInductive.c_delEdge_case_22(x2)(x3)(x4)(Curry.Module.GraphInductive.c_match(x3)(x2)(st))(st)
c_delEdge (Curry.Module.Prelude.T2Or i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_delEdge(x)(x2)(st))(i)(xs)(st)
c_delEdge x x2 st = Curry.RunTimeSystem.patternFail("GraphInductive.delEdge")(x)



c_insNodes :: (Curry t0,Curry t1) => (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0)) -> (Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.GraphInductive.C_Graph t0 t1
c_insNodes x1 x2 st = Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_insNode))(x2)(x1)(st)



c_insEdges :: (Curry t0,Curry t1) => (Curry.Module.Prelude.List (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t0)) -> (Curry.Module.GraphInductive.C_Graph t1 t0) -> Curry.RunTimeSystem.State -> Curry.Module.GraphInductive.C_Graph t1 t0
c_insEdges x1 x2 st = Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.GraphInductive.c_insEdge))(x2)(x1)(st)



c_delNodes :: (Curry t0,Curry t1) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> (Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.GraphInductive.C_Graph t0 t1
c_delNodes x1@Curry.Module.Prelude.List x2 st = x2
c_delNodes x1@((Curry.Module.Prelude.:<) x3 x4) x2 st = Curry.Module.GraphInductive.c_delNodes(x4)(Curry.Module.Prelude.c_snd(Curry.Module.GraphInductive.c_match(x3)(x2)(st))(st))(st)
c_delNodes (Curry.Module.Prelude.ListOr i xs) x2 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_delNodes(x)(x2)(st))(i)(xs)(st)
c_delNodes x x2 st = Curry.RunTimeSystem.patternFail("GraphInductive.delNodes")(x)



c_delEdges :: (Curry t0,Curry t1) => (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int)) -> (Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.GraphInductive.C_Graph t0 t1
c_delEdges x1 x2 st = Curry.Module.Prelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.GraphInductive.c_delEdge))(x2)(x1)(st)



c_isEmpty :: (Curry t0,Curry t1) => (Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isEmpty x1@(Curry.Module.GraphInductive.C_Gr x2) st = Curry.Module.FiniteMap.c_isEmptyFM(x2)(st)
c_isEmpty (Curry.Module.GraphInductive.C_GraphOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_isEmpty(x)(st))(i)(xs)(st)
c_isEmpty x st = Curry.RunTimeSystem.patternFail("GraphInductive.isEmpty")(x)



c_match :: (Curry t0,Curry t1) => Curry.Module.Prelude.C_Int -> (Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t0 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)))) (Curry.Module.GraphInductive.C_Graph t0 t1)
c_match x1 x2@(Curry.Module.GraphInductive.C_Gr x3) st = Curry.Module.Prelude.c_maybe(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Nothing)(Curry.Module.GraphInductive.C_Gr(x3)))(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_match'46_'35lambda5(x1)))(Curry.Module.FiniteMap.c_splitFM(x3)(x1)(st))(st)
c_match x1 (Curry.Module.GraphInductive.C_GraphOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_match(x1)(x)(st))(i)(xs)(st)
c_match x1 x st = Curry.RunTimeSystem.patternFail("GraphInductive.match")(x)



c_match'46_'35lambda5 :: (Curry t146,Curry t148) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.T2 (Curry.Module.FiniteMap.C_FM Curry.Module.Prelude.C_Int (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t146 Curry.Module.Prelude.C_Int)) t148 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t146 Curry.Module.Prelude.C_Int)))) (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t146 Curry.Module.Prelude.C_Int)) t148 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t146 Curry.Module.Prelude.C_Int))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t146 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t148 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t146 Curry.Module.Prelude.C_Int)))) (Curry.Module.GraphInductive.C_Graph t148 t146)
c_match'46_'35lambda5 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.GraphInductive.c_match'46_'35lambda5_case_19(x1)(x3)(x4)(st)
c_match'46_'35lambda5 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_match'46_'35lambda5(x1)(x)(st))(i)(xs)(st)
c_match'46_'35lambda5 x1 x st = Curry.RunTimeSystem.patternFail("GraphInductive.match._#lambda5")(x)



c_noNodes :: (Curry t0,Curry t1) => (Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_noNodes x1@(Curry.Module.GraphInductive.C_Gr x2) st = Curry.Module.FiniteMap.c_sizeFM(x2)(st)
c_noNodes (Curry.Module.GraphInductive.C_GraphOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_noNodes(x)(st))(i)(xs)(st)
c_noNodes x st = Curry.RunTimeSystem.patternFail("GraphInductive.noNodes")(x)



c_nodeRange :: (Curry t0,Curry t1) => (Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int
c_nodeRange x1@(Curry.Module.GraphInductive.C_Gr x2) st = let {x3 = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_fst))(Curry.Module.Prelude.pf(Curry.Module.Maybe.c_fromJust))(st)} in Curry.Module.GraphInductive.c_nodeRange_case_17(x2)(x3)(Curry.Module.FiniteMap.c_isEmptyFM(x2)(st))(st)
c_nodeRange (Curry.Module.GraphInductive.C_GraphOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_nodeRange(x)(st))(i)(xs)(st)
c_nodeRange x st = Curry.RunTimeSystem.patternFail("GraphInductive.nodeRange")(x)



c_context :: (Curry t0,Curry t1) => (Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t0 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int))
c_context x1 x2 st = Curry.Module.GraphInductive.c_context_case_15(x1)(x2)(Curry.Module.GraphInductive.c_match(x2)(x1)(st))(st)



c_lab :: (Curry t0,Curry t1) => (Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t0
c_lab x1 x2 st = Curry.Module.Maybe.op_62_62_45(Curry.Module.Prelude.c_fst(Curry.Module.GraphInductive.c_match(x2)(x1)(st))(st))(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pc(Curry.Module.Prelude.C_Just))(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_lab'39))(st))(st)



c_neighbors :: (Curry t0,Curry t1) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))
c_neighbors st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.op_46_58(st))(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_neighbors'46_'35lambda7))(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.GraphInductive.c_context))(st)



c_neighbors'46_'35lambda7 :: (Curry t416,Curry t415) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t416 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t415 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t416 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_neighbors'46_'35lambda7 x1@(Curry.Module.Prelude.T4 x2 x3 x4 x5) st = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))(Curry.Module.Prelude.op_43_43(x2)(x5)(st))(st)
c_neighbors'46_'35lambda7 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_neighbors'46_'35lambda7(x)(st))(i)(xs)(st)
c_neighbors'46_'35lambda7 x st = Curry.RunTimeSystem.patternFail("GraphInductive.neighbors._#lambda7")(x)



c_suc :: (Curry t0,Curry t1) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))
c_suc st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.op_46_58(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))))(st))(Curry.Module.GraphInductive.c_context4(st))(st)



c_pre :: (Curry t0,Curry t1) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))
c_pre st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.op_46_58(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))))(st))(Curry.Module.GraphInductive.c_context1(st))(st)



c_lsuc :: (Curry t0,Curry t1) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t1)))
c_lsuc st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.op_46_58(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_flip2))))(st))(Curry.Module.GraphInductive.c_context4(st))(st)



c_lpre :: (Curry t0,Curry t1) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t1)))
c_lpre st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.op_46_58(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_flip2))))(st))(Curry.Module.GraphInductive.c_context1(st))(st)



c_out :: (Curry t0,Curry t1) => (Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t1)
c_out x1 x2 st = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_out'46_'35lambda8(x2)))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.c_context4(st))(x1)(st))(x2)(st))(st)



c_out'46_'35lambda8 :: (Curry t511) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.T2 t511 Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t511
c_out'46_'35lambda8 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.Prelude.T3(x1)(x4)(x3)
c_out'46_'35lambda8 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_out'46_'35lambda8(x1)(x)(st))(i)(xs)(st)
c_out'46_'35lambda8 x1 x st = Curry.RunTimeSystem.patternFail("GraphInductive.out._#lambda8")(x)



c_inn :: (Curry t0,Curry t1) => (Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t1)
c_inn x1 x2 st = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_inn'46_'35lambda9(x2)))(Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.c_context1(st))(x1)(st))(x2)(st))(st)



c_inn'46_'35lambda9 :: (Curry t521) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.T2 t521 Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t521
c_inn'46_'35lambda9 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.Prelude.T3(x4)(x1)(x3)
c_inn'46_'35lambda9 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_inn'46_'35lambda9(x1)(x)(st))(i)(xs)(st)
c_inn'46_'35lambda9 x1 x st = Curry.RunTimeSystem.patternFail("GraphInductive.inn._#lambda9")(x)



c_outdeg :: (Curry t0,Curry t1) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int))
c_outdeg st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.op_46_58(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_length))(st))(Curry.Module.GraphInductive.c_context4(st))(st)



c_indeg :: (Curry t0,Curry t1) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int))
c_indeg st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.op_46_58(st))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_length))(st))(Curry.Module.GraphInductive.c_context1(st))(st)



c_deg :: (Curry t0,Curry t1) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int))
c_deg st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.op_46_58(st))(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_deg'46_'35lambda10))(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.GraphInductive.c_context))(st)



c_deg'46_'35lambda10 :: (Curry t553,Curry t552) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t553 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t552 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t553 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_deg'46_'35lambda10 x1@(Curry.Module.Prelude.T4 x2 x3 x4 x5) st = Curry.Module.Prelude.op_43(Curry.Module.Prelude.c_length(x2)(st))(Curry.Module.Prelude.c_length(x5)(st))(st)
c_deg'46_'35lambda10 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_deg'46_'35lambda10(x)(st))(i)(xs)(st)
c_deg'46_'35lambda10 x st = Curry.RunTimeSystem.patternFail("GraphInductive.deg._#lambda10")(x)



c_gelem :: (Curry t0,Curry t1) => Curry.Module.Prelude.C_Int -> (Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_gelem x1 x2 st = Curry.Module.Maybe.c_isJust(Curry.Module.Prelude.c_fst(Curry.Module.GraphInductive.c_match(x1)(x2)(st))(st))(st)



c_equal :: (Curry t0,Curry t1) => (Curry.Module.GraphInductive.C_Graph t0 t1) -> (Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_equal x1 x2 st = Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.c_slabNodes(st))(x1)(st))(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.c_slabNodes(st))(x2)(st))(st))(Curry.Module.Prelude.op_61_61(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.c_slabEdges(st))(x1)(st))(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.c_slabEdges(st))(x2)(st))(st))(st)



c_nodeComp :: (Curry t0) => (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0) -> (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_nodeComp x1 x2 st = Curry.Module.GraphInductive.c_nodeComp_case_13(x1)(x2)(Curry.Module.Prelude.op_61_61(x1)(x2)(st))(st)



c_slabNodes :: (Curry t0,Curry t1) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0))
c_slabNodes st = Curry.Module.Prelude.op_46(Curry.Module.GraphInductive.c_sortBy(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.GraphInductive.c_nodeComp))(st))(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_labNodes))(st)



c_edgeComp :: (Curry t0) => (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t0) -> (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_edgeComp x1 x2 st = let {x4 = Curry.Module.GraphInductive.c_edgeComp'46_'35selFP11'35v(x1)(st)} in let {x7 = Curry.Module.GraphInductive.c_edgeComp'46_'35selFP9'35x(x2)(st)} in Curry.Module.GraphInductive.c_edgeComp_case_10(x1)(x2)(x4)(x7)(Curry.Module.Prelude.op_61_61(x1)(x2)(st))(st)



c_edgeComp'46_'35selFP11'35v :: (Curry t613) => (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t613) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_edgeComp'46_'35selFP11'35v x1@(Curry.Module.Prelude.T3 x2 x3 x4) st = x2
c_edgeComp'46_'35selFP11'35v (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_edgeComp'46_'35selFP11'35v(x)(st))(i)(xs)(st)
c_edgeComp'46_'35selFP11'35v x st = Curry.RunTimeSystem.patternFail("GraphInductive.edgeComp._#selFP11#v")(x)



c_edgeComp'46_'35selFP12'35w :: (Curry t613) => (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t613) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_edgeComp'46_'35selFP12'35w x1@(Curry.Module.Prelude.T3 x2 x3 x4) st = x3
c_edgeComp'46_'35selFP12'35w (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_edgeComp'46_'35selFP12'35w(x)(st))(i)(xs)(st)
c_edgeComp'46_'35selFP12'35w x st = Curry.RunTimeSystem.patternFail("GraphInductive.edgeComp._#selFP12#w")(x)



c_edgeComp'46_'35selFP9'35x :: (Curry t613) => (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t613) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_edgeComp'46_'35selFP9'35x x1@(Curry.Module.Prelude.T3 x2 x3 x4) st = x2
c_edgeComp'46_'35selFP9'35x (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_edgeComp'46_'35selFP9'35x(x)(st))(i)(xs)(st)
c_edgeComp'46_'35selFP9'35x x st = Curry.RunTimeSystem.patternFail("GraphInductive.edgeComp._#selFP9#x")(x)



c_edgeComp'46_'35selFP10'35y :: (Curry t613) => (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t613) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_edgeComp'46_'35selFP10'35y x1@(Curry.Module.Prelude.T3 x2 x3 x4) st = x3
c_edgeComp'46_'35selFP10'35y (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_edgeComp'46_'35selFP10'35y(x)(st))(i)(xs)(st)
c_edgeComp'46_'35selFP10'35y x st = Curry.RunTimeSystem.patternFail("GraphInductive.edgeComp._#selFP10#y")(x)



c_slabEdges :: (Curry t0,Curry t1) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t1))
c_slabEdges st = Curry.Module.Prelude.op_46(Curry.Module.GraphInductive.c_sortBy(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.GraphInductive.c_edgeComp))(st))(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_labEdges))(st)



c_node'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_node'39 x1@(Curry.Module.Prelude.T4 x2 x3 x4 x5) st = x3
c_node'39 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_node'39(x)(st))(i)(xs)(st)
c_node'39 x st = Curry.RunTimeSystem.patternFail("GraphInductive.node'")(x)



c_lab'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> t1
c_lab'39 x1@(Curry.Module.Prelude.T4 x2 x3 x4 x5) st = x4
c_lab'39 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_lab'39(x)(st))(i)(xs)(st)
c_lab'39 x st = Curry.RunTimeSystem.patternFail("GraphInductive.lab'")(x)



c_labNode'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t1
c_labNode'39 x1@(Curry.Module.Prelude.T4 x2 x3 x4 x5) st = Curry.Module.Prelude.T2(x3)(x4)
c_labNode'39 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_labNode'39(x)(st))(i)(xs)(st)
c_labNode'39 x st = Curry.RunTimeSystem.patternFail("GraphInductive.labNode'")(x)



c_neighbors'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_neighbors'39 x1@(Curry.Module.Prelude.T4 x2 x3 x4 x5) st = Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))(x2)(st))(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))(x5)(st))(st)
c_neighbors'39 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_neighbors'39(x)(st))(i)(xs)(st)
c_neighbors'39 x st = Curry.RunTimeSystem.patternFail("GraphInductive.neighbors'")(x)



c_suc'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_suc'39 x1@(Curry.Module.Prelude.T4 x2 x3 x4 x5) st = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))(x5)(st)
c_suc'39 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_suc'39(x)(st))(i)(xs)(st)
c_suc'39 x st = Curry.RunTimeSystem.patternFail("GraphInductive.suc'")(x)



c_pre'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_pre'39 x1@(Curry.Module.Prelude.T4 x2 x3 x4 x5) st = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))(x2)(st)
c_pre'39 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_pre'39(x)(st))(i)(xs)(st)
c_pre'39 x st = Curry.RunTimeSystem.patternFail("GraphInductive.pre'")(x)



c_lpre'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0)
c_lpre'39 x1@(Curry.Module.Prelude.T4 x2 x3 x4 x5) st = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_flip2))(x2)(st)
c_lpre'39 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_lpre'39(x)(st))(i)(xs)(st)
c_lpre'39 x st = Curry.RunTimeSystem.patternFail("GraphInductive.lpre'")(x)



c_lsuc'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0)
c_lsuc'39 x1@(Curry.Module.Prelude.T4 x2 x3 x4 x5) st = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_flip2))(x5)(st)
c_lsuc'39 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_lsuc'39(x)(st))(i)(xs)(st)
c_lsuc'39 x st = Curry.RunTimeSystem.patternFail("GraphInductive.lsuc'")(x)



c_out'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t0)
c_out'39 x1@(Curry.Module.Prelude.T4 x2 x3 x4 x5) st = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_out'39'46_'35lambda11(x3)))(x5)(st)
c_out'39 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_out'39(x)(st))(i)(xs)(st)
c_out'39 x st = Curry.RunTimeSystem.patternFail("GraphInductive.out'")(x)



c_out'39'46_'35lambda11 :: (Curry t732) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.T2 t732 Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t732
c_out'39'46_'35lambda11 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.Prelude.T3(x1)(x4)(x3)
c_out'39'46_'35lambda11 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_out'39'46_'35lambda11(x1)(x)(st))(i)(xs)(st)
c_out'39'46_'35lambda11 x1 x st = Curry.RunTimeSystem.patternFail("GraphInductive.out'._#lambda11")(x)



c_inn'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t0)
c_inn'39 x1@(Curry.Module.Prelude.T4 x2 x3 x4 x5) st = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_inn'39'46_'35lambda12(x3)))(x2)(st)
c_inn'39 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_inn'39(x)(st))(i)(xs)(st)
c_inn'39 x st = Curry.RunTimeSystem.patternFail("GraphInductive.inn'")(x)



c_inn'39'46_'35lambda12 :: (Curry t742) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.T2 t742 Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t742
c_inn'39'46_'35lambda12 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.Prelude.T3(x4)(x1)(x3)
c_inn'39'46_'35lambda12 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_inn'39'46_'35lambda12(x1)(x)(st))(i)(xs)(st)
c_inn'39'46_'35lambda12 x1 x st = Curry.RunTimeSystem.patternFail("GraphInductive.inn'._#lambda12")(x)



c_outdeg'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_outdeg'39 x1@(Curry.Module.Prelude.T4 x2 x3 x4 x5) st = Curry.Module.Prelude.c_length(x5)(st)
c_outdeg'39 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_outdeg'39(x)(st))(i)(xs)(st)
c_outdeg'39 x st = Curry.RunTimeSystem.patternFail("GraphInductive.outdeg'")(x)



c_indeg'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_indeg'39 x1@(Curry.Module.Prelude.T4 x2 x3 x4 x5) st = Curry.Module.Prelude.c_length(x2)(st)
c_indeg'39 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_indeg'39(x)(st))(i)(xs)(st)
c_indeg'39 x st = Curry.RunTimeSystem.patternFail("GraphInductive.indeg'")(x)



c_deg'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_deg'39 x1@(Curry.Module.Prelude.T4 x2 x3 x4 x5) st = Curry.Module.Prelude.op_43(Curry.Module.Prelude.c_length(x2)(st))(Curry.Module.Prelude.c_length(x5)(st))(st)
c_deg'39 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_deg'39(x)(st))(i)(xs)(st)
c_deg'39 x st = Curry.RunTimeSystem.patternFail("GraphInductive.deg'")(x)



c_labNodes :: (Curry t0,Curry t1) => (Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0)
c_labNodes x1@(Curry.Module.GraphInductive.C_Gr x2) st = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_labNodes'46_'35lambda13))(Curry.Module.FiniteMap.c_fmToList(x2)(st))(st)
c_labNodes (Curry.Module.GraphInductive.C_GraphOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_labNodes(x)(st))(i)(xs)(st)
c_labNodes x st = Curry.RunTimeSystem.patternFail("GraphInductive.labNodes")(x)



c_labNodes'46_'35lambda13 :: (Curry t574,Curry t573) => (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t574 Curry.Module.Prelude.C_Int)) t573 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t574 Curry.Module.Prelude.C_Int)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t573
c_labNodes'46_'35lambda13 x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.GraphInductive.c_labNodes'46_'35lambda13_case_7(x2)(x3)(st)
c_labNodes'46_'35lambda13 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_labNodes'46_'35lambda13(x)(st))(i)(xs)(st)
c_labNodes'46_'35lambda13 x st = Curry.RunTimeSystem.patternFail("GraphInductive.labNodes._#lambda13")(x)



c_labEdges :: (Curry t0,Curry t1) => (Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t1)
c_labEdges x1@(Curry.Module.GraphInductive.C_Gr x2) st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_concatMap(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_labEdges'46_'35lambda14))(st))(Curry.Module.FiniteMap.c_fmToList(x2)(st))(st)
c_labEdges (Curry.Module.GraphInductive.C_GraphOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_labEdges(x)(st))(i)(xs)(st)
c_labEdges x st = Curry.RunTimeSystem.patternFail("GraphInductive.labEdges")(x)



c_labEdges'46_'35lambda14 :: (Curry t620,Curry t619) => (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t620 Curry.Module.Prelude.C_Int)) t619 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t620 Curry.Module.Prelude.C_Int)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t620)
c_labEdges'46_'35lambda14 x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.GraphInductive.c_labEdges'46_'35lambda14_case_6(x2)(x3)(st)
c_labEdges'46_'35lambda14 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_labEdges'46_'35lambda14(x)(st))(i)(xs)(st)
c_labEdges'46_'35lambda14 x st = Curry.RunTimeSystem.patternFail("GraphInductive.labEdges._#lambda14")(x)



c_labEdges'46_'35lambda14'46_'35lambda15 :: (Curry t620) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.T2 t620 Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t620
c_labEdges'46_'35lambda14'46_'35lambda15 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.Prelude.T3(x1)(x4)(x3)
c_labEdges'46_'35lambda14'46_'35lambda15 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_labEdges'46_'35lambda14'46_'35lambda15(x1)(x)(st))(i)(xs)(st)
c_labEdges'46_'35lambda14'46_'35lambda15 x1 x st = Curry.RunTimeSystem.patternFail("GraphInductive.labEdges._#lambda14._#lambda15")(x)



c_nodes :: (Curry t0,Curry t1) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int)
c_nodes st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_fst))))(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_labNodes))(st)



c_edges :: (Curry t0,Curry t1) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int))
c_edges st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_edges'46_'35lambda16))))(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_labEdges))(st)



c_edges'46_'35lambda16 :: (Curry t788) => (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t788) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int
c_edges'46_'35lambda16 x1@(Curry.Module.Prelude.T3 x2 x3 x4) st = Curry.Module.Prelude.T2(x2)(x3)
c_edges'46_'35lambda16 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_edges'46_'35lambda16(x)(st))(i)(xs)(st)
c_edges'46_'35lambda16 x st = Curry.RunTimeSystem.patternFail("GraphInductive.edges._#lambda16")(x)



c_newNodes :: (Curry t0,Curry t1) => Curry.Module.Prelude.C_Int -> (Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_newNodes x1 x2 st = let {x4 = Curry.Module.GraphInductive.c_newNodes'46_'35selFP14'35n(Curry.Module.GraphInductive.c_nodeRange(x2)(st))(st)} in Curry.Module.Prelude.c_enumFromTo(Curry.Module.Prelude.op_43(x4)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(st))(Curry.Module.Prelude.op_43(x4)(x1)(st))(st)



c_newNodes'46_'35selFP14'35n :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_newNodes'46_'35selFP14'35n x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_newNodes'46_'35selFP14'35n (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_newNodes'46_'35selFP14'35n(x)(st))(i)(xs)(st)
c_newNodes'46_'35selFP14'35n x st = Curry.RunTimeSystem.patternFail("GraphInductive.newNodes._#selFP14#n")(x)



c_ufold :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t2 -> Curry.RunTimeSystem.State -> t2))) -> t2 -> (Curry.Module.GraphInductive.C_Graph t1 t0) -> Curry.RunTimeSystem.State -> t2
c_ufold x1 x2 x3 st = let {x4 = Curry.Module.GraphInductive.c_matchAny(x3)(st)} in Curry.Module.GraphInductive.c_ufold_case_5(x1)(x2)(x3)(x4)(Curry.Module.GraphInductive.c_isEmpty(x3)(st))(st)



c_ufold'46_'35selFP16'35c :: (Curry t807,Curry t806) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t807 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t806 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t807 Curry.Module.Prelude.C_Int))) (Curry.Module.GraphInductive.C_Graph t806 t807)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t807 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t806 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t807 Curry.Module.Prelude.C_Int))
c_ufold'46_'35selFP16'35c x1@(Curry.Module.Prelude.T2 x2 x3) st = x2
c_ufold'46_'35selFP16'35c (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_ufold'46_'35selFP16'35c(x)(st))(i)(xs)(st)
c_ufold'46_'35selFP16'35c x st = Curry.RunTimeSystem.patternFail("GraphInductive.ufold._#selFP16#c")(x)



c_ufold'46_'35selFP17'35g'39 :: (Curry t807,Curry t806) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t807 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t806 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t807 Curry.Module.Prelude.C_Int))) (Curry.Module.GraphInductive.C_Graph t806 t807)) -> Curry.RunTimeSystem.State -> Curry.Module.GraphInductive.C_Graph t806 t807
c_ufold'46_'35selFP17'35g'39 x1@(Curry.Module.Prelude.T2 x2 x3) st = x3
c_ufold'46_'35selFP17'35g'39 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_ufold'46_'35selFP17'35g'39(x)(st))(i)(xs)(st)
c_ufold'46_'35selFP17'35g'39 x st = Curry.RunTimeSystem.patternFail("GraphInductive.ufold._#selFP17#g'")(x)



c_gmap :: (Curry t0,Curry t1,Curry t2,Curry t3) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t2 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t2 Curry.Module.Prelude.C_Int)))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t1 t0) -> Curry.RunTimeSystem.State -> Curry.Module.GraphInductive.C_Graph t3 t2)
c_gmap x1 st = Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_ufold(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_gmap'46_'35lambda17(x1)))(Curry.Module.GraphInductive.c_empty(st)))



c_gmap'46_'35lambda17 :: (Curry t821,Curry t822,Curry t830,Curry t829) => (Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t821 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t822 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t821 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t830 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t829 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t830 Curry.Module.Prelude.C_Int)))) -> (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t821 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t822 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t821 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t829 t830) -> Curry.RunTimeSystem.State -> Curry.Module.GraphInductive.C_Graph t829 t830)
c_gmap'46_'35lambda17 x1 x2 st = Curry.Module.Prelude.pf(Curry.Module.GraphInductive.op_58_38(Curry.Module.Prelude.c_apply(x1)(x2)(st)))



c_nmap :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t0 t2) -> Curry.RunTimeSystem.State -> Curry.Module.GraphInductive.C_Graph t1 t2)
c_nmap x1 st = Curry.Module.GraphInductive.c_gmap(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_nmap'46_'35lambda18(x1)))(st)



c_nmap'46_'35lambda18 :: (Curry t841,Curry t844,Curry t837) => (Curry.Module.Prelude.Prim (t841 -> Curry.RunTimeSystem.State -> t844)) -> (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t837 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t841 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t837 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t837 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t844 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t837 Curry.Module.Prelude.C_Int))
c_nmap'46_'35lambda18 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = Curry.Module.Prelude.T4(x3)(x4)(Curry.Module.Prelude.c_apply(x1)(x5)(st))(x6)
c_nmap'46_'35lambda18 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_nmap'46_'35lambda18(x1)(x)(st))(i)(xs)(st)
c_nmap'46_'35lambda18 x1 x st = Curry.RunTimeSystem.patternFail("GraphInductive.nmap._#lambda18")(x)



c_emap :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t2 t0) -> Curry.RunTimeSystem.State -> Curry.Module.GraphInductive.C_Graph t2 t1)
c_emap x1 st = Curry.Module.GraphInductive.c_gmap(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_emap'46_'35lambda20(x1)))(st)



c_emap'46map1'46213 :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 t2)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 t2))
c_emap'46map1'46213 x1 st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_emap'46map1'46213'46_'35lambda19(x1))))



c_emap'46map1'46213'46_'35lambda19 :: (Curry t853,Curry t856,Curry t854) => (Curry.Module.Prelude.Prim (t853 -> Curry.RunTimeSystem.State -> t856)) -> (Curry.Module.Prelude.T2 t853 t854) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 t856 t854
c_emap'46map1'46213'46_'35lambda19 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.Prelude.T2(Curry.Module.Prelude.c_apply(x1)(x3)(st))(x4)
c_emap'46map1'46213'46_'35lambda19 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_emap'46map1'46213'46_'35lambda19(x1)(x)(st))(i)(xs)(st)
c_emap'46map1'46213'46_'35lambda19 x1 x st = Curry.RunTimeSystem.patternFail("GraphInductive.emap.map1.213._#lambda19")(x)



c_emap'46_'35lambda20 :: (Curry t865,Curry t866,Curry t863) => (Curry.Module.Prelude.Prim (t865 -> Curry.RunTimeSystem.State -> t866)) -> (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t865 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t863 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t865 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t866 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t863 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t866 Curry.Module.Prelude.C_Int))
c_emap'46_'35lambda20 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = Curry.Module.Prelude.T4(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.c_emap'46map1'46213(x1)(st))(x3)(st))(x4)(x5)(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.c_emap'46map1'46213(x1)(st))(x6)(st))
c_emap'46_'35lambda20 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_emap'46_'35lambda20(x1)(x)(st))(i)(xs)(st)
c_emap'46_'35lambda20 x1 x st = Curry.RunTimeSystem.patternFail("GraphInductive.emap._#lambda20")(x)



c_labUEdges :: (Curry t0,Curry t1) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T3 t0 t1 Curry.Module.Prelude.T0))
c_labUEdges st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_labUEdges'46_'35lambda21)))



c_labUEdges'46_'35lambda21 :: (Curry t249,Curry t250) => (Curry.Module.Prelude.T2 t249 t250) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 t249 t250 Curry.Module.Prelude.T0
c_labUEdges'46_'35lambda21 x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.Prelude.T3(x2)(x3)(Curry.Module.Prelude.T0)
c_labUEdges'46_'35lambda21 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_labUEdges'46_'35lambda21(x)(st))(i)(xs)(st)
c_labUEdges'46_'35lambda21 x st = Curry.RunTimeSystem.patternFail("GraphInductive.labUEdges._#lambda21")(x)



c_labUNodes :: (Curry t0) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.T0))
c_labUNodes st = Curry.Module.Prelude.pf(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_labUNodes'46_'35lambda22)))



c_labUNodes'46_'35lambda22 :: (Curry t254) => t254 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 t254 Curry.Module.Prelude.T0
c_labUNodes'46_'35lambda22 x1 st = Curry.Module.Prelude.T2(x1)(Curry.Module.Prelude.T0)



c_showGraph :: (Curry t0,Curry t1) => (Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showGraph x1@(Curry.Module.GraphInductive.C_Gr x2) st = Curry.Module.Prelude.c_unlines(Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_showNode))(Curry.Module.FiniteMap.c_fmToList(x2)(st))(st))(st)
c_showGraph (Curry.Module.GraphInductive.C_GraphOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_showGraph(x)(st))(i)(xs)(st)
c_showGraph x st = Curry.RunTimeSystem.patternFail("GraphInductive.showGraph")(x)



c_showNode :: (Curry t0,Curry t1,Curry t2,Curry t3) => (Curry.Module.Prelude.T2 t0 (Curry.Module.Prelude.T3 t1 t2 t3)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showNode x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.GraphInductive.c_showNode_case_3(x2)(x3)(st)
c_showNode (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_showNode(x)(st))(i)(xs)(st)
c_showNode x st = Curry.RunTimeSystem.patternFail("GraphInductive.showNode")(x)



op_46_58 :: (Curry t0,Curry t1,Curry t2,Curry t3) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (t2 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t3 -> Curry.RunTimeSystem.State -> t0))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t2 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t3 -> Curry.RunTimeSystem.State -> t1))))
op_46_58 st = Curry.Module.Prelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_46))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_46))(st)



c_fst4 :: (Curry t0,Curry t1,Curry t2,Curry t3) => (Curry.Module.Prelude.T4 t0 t1 t2 t3) -> Curry.RunTimeSystem.State -> t0
c_fst4 x1@(Curry.Module.Prelude.T4 x2 x3 x4 x5) st = x2
c_fst4 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_fst4(x)(st))(i)(xs)(st)
c_fst4 x st = Curry.RunTimeSystem.patternFail("GraphInductive.fst4")(x)



c_fth4 :: (Curry t0,Curry t1,Curry t2,Curry t3) => (Curry.Module.Prelude.T4 t0 t1 t2 t3) -> Curry.RunTimeSystem.State -> t3
c_fth4 x1@(Curry.Module.Prelude.T4 x2 x3 x4 x5) st = x5
c_fth4 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_fth4(x)(st))(i)(xs)(st)
c_fth4 x st = Curry.RunTimeSystem.patternFail("GraphInductive.fth4")(x)



c_flip2 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T2 t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 t1 t0
c_flip2 x1@(Curry.Module.Prelude.T2 x2 x3) st = Curry.Module.Prelude.T2(x3)(x2)
c_flip2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_flip2(x)(st))(i)(xs)(st)
c_flip2 x st = Curry.RunTimeSystem.patternFail("GraphInductive.flip2")(x)



c_context1 :: (Curry t0,Curry t1) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)))
c_context1 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.op_46_58(st))(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_fst4))(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.GraphInductive.c_context))(st)



c_context4 :: (Curry t0,Curry t1) => Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.GraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)))
c_context4 st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(Curry.Module.GraphInductive.op_46_58(st))(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_fth4))(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.GraphInductive.c_context))(st)



c_addSucc :: (Curry t0,Curry t1,Curry t2,Curry t3) => t0 -> t1 -> (Curry.Module.Prelude.T3 t2 t3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 t0))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 t2 t3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 t0))
c_addSucc x1 x2 x3@(Curry.Module.Prelude.T3 x4 x5 x6) st = Curry.Module.Prelude.T3(x4)(x5)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(x2)(x1))(x6))
c_addSucc x1 x2 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_addSucc(x1)(x2)(x)(st))(i)(xs)(st)
c_addSucc x1 x2 x st = Curry.RunTimeSystem.patternFail("GraphInductive.addSucc")(x)



c_addPred :: (Curry t0,Curry t1,Curry t2,Curry t3) => t0 -> t1 -> (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 t0)) t2 t3) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 t0)) t2 t3
c_addPred x1 x2 x3@(Curry.Module.Prelude.T3 x4 x5 x6) st = Curry.Module.Prelude.T3((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(x2)(x1))(x4))(x5)(x6)
c_addPred x1 x2 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_addPred(x1)(x2)(x)(st))(i)(xs)(st)
c_addPred x1 x2 x st = Curry.RunTimeSystem.patternFail("GraphInductive.addPred")(x)



c_clearSucc :: (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4) => t0 -> t1 -> (Curry.Module.Prelude.T3 t2 t3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t4 t0))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 t2 t3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t4 t0))
c_clearSucc x1 x2 x3@(Curry.Module.Prelude.T3 x4 x5 x6) st = Curry.Module.Prelude.T3(x4)(x5)(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_47_61))(x1)))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))(st))(x6)(st))
c_clearSucc x1 x2 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_clearSucc(x1)(x2)(x)(st))(i)(xs)(st)
c_clearSucc x1 x2 x st = Curry.RunTimeSystem.patternFail("GraphInductive.clearSucc")(x)



c_clearPred :: (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4) => t0 -> t1 -> (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t2 t0)) t3 t4) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t2 t0)) t3 t4
c_clearPred x1 x2 x3@(Curry.Module.Prelude.T3 x4 x5 x6) st = Curry.Module.Prelude.T3(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_47_61))(x1)))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))(st))(x4)(st))(x5)(x6)
c_clearPred x1 x2 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_clearPred(x1)(x2)(x)(st))(i)(xs)(st)
c_clearPred x1 x2 x st = Curry.RunTimeSystem.patternFail("GraphInductive.clearPred")(x)



c_updAdj :: (Curry t0,Curry t1) => (Curry.Module.FiniteMap.C_FM Curry.Module.Prelude.C_Int (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)))) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) -> (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))))) -> Curry.RunTimeSystem.State -> Curry.Module.FiniteMap.C_FM Curry.Module.Prelude.C_Int (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)))
c_updAdj x1 x2@Curry.Module.Prelude.List x3 st = x1
c_updAdj x1 x2@((Curry.Module.Prelude.:<) x4 x5) x3 st = Curry.Module.GraphInductive.c_updAdj_case_2(x1)(x3)(x5)(x4)(st)
c_updAdj x1 (Curry.Module.Prelude.ListOr i xs) x3 st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_updAdj(x1)(x)(x3)(st))(i)(xs)(st)
c_updAdj x1 x x3 st = Curry.RunTimeSystem.patternFail("GraphInductive.updAdj")(x)



c_sortBy :: (Curry t0) => (Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0)
c_sortBy x1 st = Curry.Module.Prelude.pf(Curry.Module.Sort.c_mergeSort(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.GraphInductive.c_sortBy'46_'35lambda23(x1))))



c_sortBy'46_'35lambda23 :: (Curry t587) => (Curry.Module.Prelude.Prim (t587 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t587 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering))) -> t587 -> t587 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_sortBy'46_'35lambda23 x1 x2 x3 st = let {x4 = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(x2)(st))(x3)(st)} in Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_61_61(x4)(Curry.Module.Prelude.C_EQ)(st))(Curry.Module.Prelude.op_61_61(x4)(Curry.Module.Prelude.C_LT)(st))(st)



c_updAdj_case_2 x1 x3 x5 x4@(Curry.Module.Prelude.T2 x6 x7) st = Curry.Module.GraphInductive.c_updAdj_case_1(x1)(x3)(x5)(x6)(x7)(Curry.Module.FiniteMap.c_elemFM(x7)(x1)(st))(st)
c_updAdj_case_2 x1 x3 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_updAdj_case_2(x1)(x3)(x5)(x)(st))(i)(xs)(st)
c_updAdj_case_2 x1 x3 x5 x st = Curry.RunTimeSystem.patternFail("GraphInductive.updAdj_case_2")(x)



c_updAdj_case_1 x1 x3 x5 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.GraphInductive.c_updAdj(Curry.Module.FiniteMap.c_updFM(x1)(x7)(Curry.Module.Prelude.c_apply(x3)(x6)(st))(st))(x5)(x3)(st)
c_updAdj_case_1 x1 x3 x5 x6 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.GraphInductive.c_updAdj_case_0(x7)(Curry.Module.Prelude.c_otherwise(st))(st)
c_updAdj_case_1 x1 x3 x5 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_updAdj_case_1(x1)(x3)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c_updAdj_case_1 x1 x3 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("GraphInductive.updAdj_case_1")(x)



c_updAdj_case_0 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_error(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))(Curry.Module.Prelude.c_show(x7)(st))(st))(st)
c_updAdj_case_0 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_updAdj_case_0(x7)(x)(st))(i)(xs)(st)
c_updAdj_case_0 x7 x st = Curry.RunTimeSystem.patternFail("GraphInductive.updAdj_case_0")(x)



c_showNode_case_3 x2 x3@(Curry.Module.Prelude.T3 x4 x5 x6) st = Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_show(x2)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_show(x5)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.c_show(x6)(st))(st))(st))(st))(st)
c_showNode_case_3 x2 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_showNode_case_3(x2)(x)(st))(i)(xs)(st)
c_showNode_case_3 x2 x st = Curry.RunTimeSystem.patternFail("GraphInductive.showNode_case_3")(x)



c_ufold_case_5 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_True st = x2
c_ufold_case_5 x1 x2 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.GraphInductive.c_ufold_case_4(x1)(x2)(x4)(Curry.Module.Prelude.c_otherwise(st))(st)
c_ufold_case_5 x1 x2 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_ufold_case_5(x1)(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_ufold_case_5 x1 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("GraphInductive.ufold_case_5")(x)



c_ufold_case_4 x1 x2 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_apply(Curry.Module.Prelude.c_apply(x1)(Curry.Module.GraphInductive.c_ufold'46_'35selFP16'35c(x4)(st))(st))(Curry.Module.GraphInductive.c_ufold(x1)(x2)(Curry.Module.GraphInductive.c_ufold'46_'35selFP17'35g'39(x4)(st))(st))(st)
c_ufold_case_4 x1 x2 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_ufold_case_4(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c_ufold_case_4 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("GraphInductive.ufold_case_4")(x)



c_labEdges'46_'35lambda14_case_6 x2 x3@(Curry.Module.Prelude.T3 x4 x5 x6) st = Curry.Module.Prelude.c_map(Curry.Module.Prelude.pf(Curry.Module.GraphInductive.c_labEdges'46_'35lambda14'46_'35lambda15(x2)))(x6)(st)
c_labEdges'46_'35lambda14_case_6 x2 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_labEdges'46_'35lambda14_case_6(x2)(x)(st))(i)(xs)(st)
c_labEdges'46_'35lambda14_case_6 x2 x st = Curry.RunTimeSystem.patternFail("GraphInductive.labEdges._#lambda14_case_6")(x)



c_labNodes'46_'35lambda13_case_7 x2 x3@(Curry.Module.Prelude.T3 x4 x5 x6) st = Curry.Module.Prelude.T2(x2)(x5)
c_labNodes'46_'35lambda13_case_7 x2 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_labNodes'46_'35lambda13_case_7(x2)(x)(st))(i)(xs)(st)
c_labNodes'46_'35lambda13_case_7 x2 x st = Curry.RunTimeSystem.patternFail("GraphInductive.labNodes._#lambda13_case_7")(x)



c_edgeComp_case_10 x1 x2 x4 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_EQ
c_edgeComp_case_10 x1 x2 x4 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.GraphInductive.c_edgeComp_case_9(x1)(x2)(x4)(x7)(Curry.Module.Prelude.op_124_124(Curry.Module.Prelude.op_60(x4)(x7)(st))(Curry.Module.Prelude.op_38_38(Curry.Module.Prelude.op_61_61(x4)(x7)(st))(Curry.Module.Prelude.op_60(Curry.Module.GraphInductive.c_edgeComp'46_'35selFP12'35w(x1)(st))(Curry.Module.GraphInductive.c_edgeComp'46_'35selFP10'35y(x2)(st))(st))(st))(st))(st)
c_edgeComp_case_10 x1 x2 x4 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_edgeComp_case_10(x1)(x2)(x4)(x7)(x)(st))(i)(xs)(st)
c_edgeComp_case_10 x1 x2 x4 x7 x st = Curry.RunTimeSystem.patternFail("GraphInductive.edgeComp_case_10")(x)



c_edgeComp_case_9 x1 x2 x4 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_LT
c_edgeComp_case_9 x1 x2 x4 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.GraphInductive.c_edgeComp_case_8(Curry.Module.Prelude.c_otherwise(st))(st)
c_edgeComp_case_9 x1 x2 x4 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_edgeComp_case_9(x1)(x2)(x4)(x7)(x)(st))(i)(xs)(st)
c_edgeComp_case_9 x1 x2 x4 x7 x st = Curry.RunTimeSystem.patternFail("GraphInductive.edgeComp_case_9")(x)



c_edgeComp_case_8 x1@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_GT
c_edgeComp_case_8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_edgeComp_case_8(x)(st))(i)(xs)(st)
c_edgeComp_case_8 x st = Curry.RunTimeSystem.patternFail("GraphInductive.edgeComp_case_8")(x)



c_nodeComp_case_13 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_EQ
c_nodeComp_case_13 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.GraphInductive.c_nodeComp_case_12(x1)(x2)(Curry.Module.Prelude.op_60(Curry.Module.Prelude.c_fst(x1)(st))(Curry.Module.Prelude.c_fst(x2)(st))(st))(st)
c_nodeComp_case_13 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_nodeComp_case_13(x1)(x2)(x)(st))(i)(xs)(st)
c_nodeComp_case_13 x1 x2 x st = Curry.RunTimeSystem.patternFail("GraphInductive.nodeComp_case_13")(x)



c_nodeComp_case_12 x1 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_LT
c_nodeComp_case_12 x1 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.GraphInductive.c_nodeComp_case_11(Curry.Module.Prelude.c_otherwise(st))(st)
c_nodeComp_case_12 x1 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_nodeComp_case_12(x1)(x2)(x)(st))(i)(xs)(st)
c_nodeComp_case_12 x1 x2 x st = Curry.RunTimeSystem.patternFail("GraphInductive.nodeComp_case_12")(x)



c_nodeComp_case_11 x1@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.C_GT
c_nodeComp_case_11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_nodeComp_case_11(x)(st))(i)(xs)(st)
c_nodeComp_case_11 x st = Curry.RunTimeSystem.patternFail("GraphInductive.nodeComp_case_11")(x)



c_context_case_15 x1 x2 (Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.GraphInductive.c_context_case_14(x2)(x3)(st)
c_context_case_15 x1 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_context_case_15(x1)(x2)(x)(st))(i)(xs)(st)
c_context_case_15 x1 x2 x st = Curry.RunTimeSystem.patternFail("GraphInductive.context_case_15")(x)



c_context_case_14 x2 x3@Curry.Module.Prelude.C_Nothing st = Curry.Module.Prelude.c_error(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))))))))))))))))(Curry.Module.Prelude.c_show(x2)(st))(st))(st)
c_context_case_14 x2 x3@(Curry.Module.Prelude.C_Just x5) st = x5
c_context_case_14 x2 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_context_case_14(x2)(x)(st))(i)(xs)(st)
c_context_case_14 x2 x st = Curry.RunTimeSystem.patternFail("GraphInductive.context_case_14")(x)



c_nodeRange_case_17 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.C_Zero)
c_nodeRange_case_17 x2 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.GraphInductive.c_nodeRange_case_16(x2)(x3)(Curry.Module.Prelude.c_otherwise(st))(st)
c_nodeRange_case_17 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_nodeRange_case_17(x2)(x3)(x)(st))(i)(xs)(st)
c_nodeRange_case_17 x2 x3 x st = Curry.RunTimeSystem.patternFail("GraphInductive.nodeRange_case_17")(x)



c_nodeRange_case_16 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.T2(Curry.Module.Prelude.c_apply(x3)(Curry.Module.Prelude.c_apply(Curry.Module.FiniteMap.c_minFM(st))(x2)(st))(st))(Curry.Module.Prelude.c_apply(x3)(Curry.Module.Prelude.c_apply(Curry.Module.FiniteMap.c_maxFM(st))(x2)(st))(st))
c_nodeRange_case_16 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_nodeRange_case_16(x2)(x3)(x)(st))(i)(xs)(st)
c_nodeRange_case_16 x2 x3 x st = Curry.RunTimeSystem.patternFail("GraphInductive.nodeRange_case_16")(x)



c_match'46_'35lambda5_case_19 x1 x3 x4@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.GraphInductive.c_match'46_'35lambda5_case_18(x1)(x3)(x6)(st)
c_match'46_'35lambda5_case_19 x1 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_match'46_'35lambda5_case_19(x1)(x3)(x)(st))(i)(xs)(st)
c_match'46_'35lambda5_case_19 x1 x3 x st = Curry.RunTimeSystem.patternFail("GraphInductive.match._#lambda5_case_19")(x)



c_match'46_'35lambda5_case_18 x1 x3 x6@(Curry.Module.Prelude.T3 x7 x8 x9) st = let {x11 = Curry.Module.Prelude.c_filter(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_47_61))(x1)))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))(st))(x7)(st)} in Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Just(Curry.Module.Prelude.T4(x11)(x1)(x8)(x9)))(Curry.Module.GraphInductive.C_Gr(Curry.Module.GraphInductive.c_updAdj(Curry.Module.GraphInductive.c_updAdj(x3)(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_47_61))(x1)))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))(st))(x9)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.GraphInductive.c_clearPred(x1)))(st))(x11)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.GraphInductive.c_clearSucc(x1)))(st)))
c_match'46_'35lambda5_case_18 x1 x3 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_match'46_'35lambda5_case_18(x1)(x3)(x)(st))(i)(xs)(st)
c_match'46_'35lambda5_case_18 x1 x3 x st = Curry.RunTimeSystem.patternFail("GraphInductive.match._#lambda5_case_18")(x)



c_delEdge_case_22 x2 x3 x4 (Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.GraphInductive.c_delEdge_case_21(x2)(x4)(x6)(x5)(st)
c_delEdge_case_22 x2 x3 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_delEdge_case_22(x2)(x3)(x4)(x)(st))(i)(xs)(st)
c_delEdge_case_22 x2 x3 x4 x st = Curry.RunTimeSystem.patternFail("GraphInductive.delEdge_case_22")(x)



c_delEdge_case_21 x2 x4 x6 x5@Curry.Module.Prelude.C_Nothing st = x2
c_delEdge_case_21 x2 x4 x6 x5@(Curry.Module.Prelude.C_Just x7) st = Curry.Module.GraphInductive.c_delEdge_case_20(x4)(x6)(x7)(st)
c_delEdge_case_21 x2 x4 x6 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_delEdge_case_21(x2)(x4)(x6)(x)(st))(i)(xs)(st)
c_delEdge_case_21 x2 x4 x6 x st = Curry.RunTimeSystem.patternFail("GraphInductive.delEdge_case_21")(x)



c_delEdge_case_20 x4 x6 x7@(Curry.Module.Prelude.T4 x8 x9 x10 x11) st = Curry.Module.GraphInductive.op_58_38(Curry.Module.Prelude.T4(x8)(x9)(x10)(Curry.Module.Prelude.c_filter(Curry.Module.Prelude.op_46(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_flip(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Prelude.op_47_61))(x4)))(Curry.Module.Prelude.pf(Curry.Module.Prelude.c_snd))(st))(x11)(st)))(x6)(st)
c_delEdge_case_20 x4 x6 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_delEdge_case_20(x4)(x6)(x)(st))(i)(xs)(st)
c_delEdge_case_20 x4 x6 x st = Curry.RunTimeSystem.patternFail("GraphInductive.delEdge_case_20")(x)



c_insEdge'46_'35selFP6'35g'39_case_24 x3 x2@(Curry.Module.Prelude.C_Just x4) st = Curry.Module.GraphInductive.c_insEdge'46_'35selFP6'35g'39_case_23(x3)(x4)(st)
c_insEdge'46_'35selFP6'35g'39_case_24 x3 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_insEdge'46_'35selFP6'35g'39_case_24(x3)(x)(st))(i)(xs)(st)
c_insEdge'46_'35selFP6'35g'39_case_24 x3 x st = Curry.RunTimeSystem.patternFail("GraphInductive.insEdge._#selFP6#g'_case_24")(x)



c_insEdge'46_'35selFP6'35g'39_case_23 x3 x4@(Curry.Module.Prelude.T4 x5 x6 x7 x8) st = x3
c_insEdge'46_'35selFP6'35g'39_case_23 x3 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_insEdge'46_'35selFP6'35g'39_case_23(x3)(x)(st))(i)(xs)(st)
c_insEdge'46_'35selFP6'35g'39_case_23 x3 x st = Curry.RunTimeSystem.patternFail("GraphInductive.insEdge._#selFP6#g'_case_23")(x)



c_insEdge'46_'35selFP5'35su_case_26 x2@(Curry.Module.Prelude.C_Just x4) st = Curry.Module.GraphInductive.c_insEdge'46_'35selFP5'35su_case_25(x4)(st)
c_insEdge'46_'35selFP5'35su_case_26 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_insEdge'46_'35selFP5'35su_case_26(x)(st))(i)(xs)(st)
c_insEdge'46_'35selFP5'35su_case_26 x st = Curry.RunTimeSystem.patternFail("GraphInductive.insEdge._#selFP5#su_case_26")(x)



c_insEdge'46_'35selFP5'35su_case_25 x4@(Curry.Module.Prelude.T4 x5 x6 x7 x8) st = x8
c_insEdge'46_'35selFP5'35su_case_25 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_insEdge'46_'35selFP5'35su_case_25(x)(st))(i)(xs)(st)
c_insEdge'46_'35selFP5'35su_case_25 x st = Curry.RunTimeSystem.patternFail("GraphInductive.insEdge._#selFP5#su_case_25")(x)



c_insEdge'46_'35selFP4'35la_case_28 x2@(Curry.Module.Prelude.C_Just x4) st = Curry.Module.GraphInductive.c_insEdge'46_'35selFP4'35la_case_27(x4)(st)
c_insEdge'46_'35selFP4'35la_case_28 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_insEdge'46_'35selFP4'35la_case_28(x)(st))(i)(xs)(st)
c_insEdge'46_'35selFP4'35la_case_28 x st = Curry.RunTimeSystem.patternFail("GraphInductive.insEdge._#selFP4#la_case_28")(x)



c_insEdge'46_'35selFP4'35la_case_27 x4@(Curry.Module.Prelude.T4 x5 x6 x7 x8) st = x7
c_insEdge'46_'35selFP4'35la_case_27 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_insEdge'46_'35selFP4'35la_case_27(x)(st))(i)(xs)(st)
c_insEdge'46_'35selFP4'35la_case_27 x st = Curry.RunTimeSystem.patternFail("GraphInductive.insEdge._#selFP4#la_case_27")(x)



c_insEdge'46_'35selFP3'35pr_case_30 x2@(Curry.Module.Prelude.C_Just x4) st = Curry.Module.GraphInductive.c_insEdge'46_'35selFP3'35pr_case_29(x4)(st)
c_insEdge'46_'35selFP3'35pr_case_30 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_insEdge'46_'35selFP3'35pr_case_30(x)(st))(i)(xs)(st)
c_insEdge'46_'35selFP3'35pr_case_30 x st = Curry.RunTimeSystem.patternFail("GraphInductive.insEdge._#selFP3#pr_case_30")(x)



c_insEdge'46_'35selFP3'35pr_case_29 x4@(Curry.Module.Prelude.T4 x5 x6 x7 x8) st = x5
c_insEdge'46_'35selFP3'35pr_case_29 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_insEdge'46_'35selFP3'35pr_case_29(x)(st))(i)(xs)(st)
c_insEdge'46_'35selFP3'35pr_case_29 x st = Curry.RunTimeSystem.patternFail("GraphInductive.insEdge._#selFP3#pr_case_29")(x)



c_matchAny_case_35 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('G'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))(st)
c_matchAny_case_35 x2 x3@Curry.Module.Prelude.C_False st = Curry.Module.GraphInductive.c_matchAny_case_34(x2)(Curry.Module.Prelude.c_otherwise(st))(st)
c_matchAny_case_35 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_matchAny_case_35(x2)(x)(st))(i)(xs)(st)
c_matchAny_case_35 x2 x st = Curry.RunTimeSystem.patternFail("GraphInductive.matchAny_case_35")(x)



c_matchAny_case_34 x2 x3@Curry.Module.Prelude.C_True st = Curry.Module.GraphInductive.c_matchAny_case_33(x2)(Curry.Module.Prelude.c_head(Curry.Module.FiniteMap.c_fmToListPreOrder(x2)(st))(st))(st)
c_matchAny_case_34 x2 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_matchAny_case_34(x2)(x)(st))(i)(xs)(st)
c_matchAny_case_34 x2 x st = Curry.RunTimeSystem.patternFail("GraphInductive.matchAny_case_34")(x)



c_matchAny_case_33 x2 (Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.GraphInductive.c_matchAny_case_32(x2)(x3)(Curry.Module.GraphInductive.c_match(x3)(Curry.Module.GraphInductive.C_Gr(x2))(st))(st)
c_matchAny_case_33 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_matchAny_case_33(x2)(x)(st))(i)(xs)(st)
c_matchAny_case_33 x2 x st = Curry.RunTimeSystem.patternFail("GraphInductive.matchAny_case_33")(x)



c_matchAny_case_32 x2 x3 x4@(Curry.Module.Prelude.T2 x5 x6) st = Curry.Module.GraphInductive.c_matchAny_case_31(x6)(x5)(st)
c_matchAny_case_32 x2 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_matchAny_case_32(x2)(x3)(x)(st))(i)(xs)(st)
c_matchAny_case_32 x2 x3 x st = Curry.RunTimeSystem.patternFail("GraphInductive.matchAny_case_32")(x)



c_matchAny_case_31 x6 x5@(Curry.Module.Prelude.C_Just x7) st = Curry.Module.Prelude.T2(x7)(x6)
c_matchAny_case_31 x6 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_matchAny_case_31(x6)(x)(st))(i)(xs)(st)
c_matchAny_case_31 x6 x st = Curry.RunTimeSystem.patternFail("GraphInductive.matchAny_case_31")(x)



c_'58'38_case_38 x3 x4 x5 x6 x2@(Curry.Module.GraphInductive.C_Gr x7) st = Curry.Module.GraphInductive.c_'58'38_case_37(x3)(x4)(x5)(x6)(x7)(Curry.Module.FiniteMap.c_elemFM(x4)(x7)(st))(st)
c_'58'38_case_38 x3 x4 x5 x6 (Curry.Module.GraphInductive.C_GraphOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_'58'38_case_38(x3)(x4)(x5)(x6)(x)(st))(i)(xs)(st)
c_'58'38_case_38 x3 x4 x5 x6 x st = Curry.RunTimeSystem.patternFail("GraphInductive.:&_case_38")(x)



c_'58'38_case_37 x3 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.Prelude.c_error(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))(Curry.Module.Prelude.op_43_43(Curry.Module.Prelude.c_show(x4)(st))(Curry.Module.Prelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.Prelude.c_show(x5)(st))(st))(st))(st))(st)
c_'58'38_case_37 x3 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.GraphInductive.c_'58'38_case_36(x3)(x4)(x5)(x6)(x7)(Curry.Module.Prelude.c_otherwise(st))(st)
c_'58'38_case_37 x3 x4 x5 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_'58'38_case_37(x3)(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c_'58'38_case_37 x3 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("GraphInductive.:&_case_37")(x)



c_'58'38_case_36 x3 x4 x5 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.GraphInductive.C_Gr(Curry.Module.GraphInductive.c_updAdj(Curry.Module.GraphInductive.c_updAdj(Curry.Module.FiniteMap.c_addToFM(x7)(x4)(Curry.Module.Prelude.T3(x3)(x5)(x6))(st))(x3)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.GraphInductive.c_addSucc(x4)))(st))(x6)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.GraphInductive.c_addPred(x4)))(st))
c_'58'38_case_36 x3 x4 x5 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.GraphInductive.c_'58'38_case_36(x3)(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c_'58'38_case_36 x3 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("GraphInductive.:&_case_36")(x)


