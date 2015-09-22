{-# OPTIONS -cpp #-}

{-# LANGUAGE RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, TypeSynonymInstances #-}

module Curry.Module.OracleGraphInductive (module Curry.Module.OracleGraphInductive) where

import Curry.RunTimeSystem
import Curry.Module.CEventOracle
import Curry.Module.Oracle
import Curry.Module.IOExts
import Curry.Module.GraphInductive
import Curry.Module.FiniteMap
import Curry.Module.Maybe
import Curry.Module.Prelude
import Curry.Module.Sort
import Curry.Module.OracleFiniteMap
import Curry.Module.OracleMaybe
import Curry.Module.OraclePrelude
import Curry.Module.OracleSort



-- begin included



-- end included

type C_GDecomp t0 t1 = Curry.Module.Prelude.T2 (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t0 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int))) (Curry.Module.GraphInductive.C_Graph t0 t1)

type C_Decomp t0 t1 = Curry.Module.Prelude.T2 (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t0 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)))) (Curry.Module.GraphInductive.C_Graph t0 t1)

type C_GraphRep t0 t1 = Curry.Module.FiniteMap.C_FM Curry.Module.Prelude.C_Int (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)) t0 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)))

type C_UGr = Curry.Module.GraphInductive.C_Graph Curry.Module.Prelude.T0 Curry.Module.Prelude.T0

data C_Graph t0 t1 = C_Gr (Curry.Module.OracleFiniteMap.C_FM Curry.Module.Prelude.C_Int (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)) t0 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int))))
  | C_GraphFail Curry.RunTimeSystem.C_Exceptions
  | C_GraphOr Curry.RunTimeSystem.OrRef (Curry.RunTimeSystem.Branches (Curry.Module.OracleGraphInductive.C_Graph t0 t1))

instance (BaseCurry t0,BaseCurry t1) => BaseCurry (Curry.Module.OracleGraphInductive.C_Graph t0 t1) where
  nf f (Curry.Module.OracleGraphInductive.C_Gr x1) state0 = Curry.RunTimeSystem.nfCTC(\ v1 state1 -> f(Curry.Module.OracleGraphInductive.C_Gr(v1))(state1))(x1)(state0)
  nf f x st = f(x)(st)

  gnf f (Curry.Module.OracleGraphInductive.C_Gr x1) state0 = Curry.RunTimeSystem.gnfCTC(\ v1 state1 -> f(Curry.Module.OracleGraphInductive.C_Gr(v1))(state1))(x1)(state0)
  gnf f x st = f(x)(st)

  generator i = Curry.RunTimeSystem.withRef(\ r -> Curry.Module.OracleGraphInductive.C_GraphOr(Curry.RunTimeSystem.mkRef(r)(1)(i))([Curry.Module.OracleGraphInductive.C_Gr(Curry.RunTimeSystem.generator((Prelude.+)(r)((0::Int))))]))(1)

  failed  = Curry.Module.OracleGraphInductive.C_GraphFail

  branching  = Curry.Module.OracleGraphInductive.C_GraphOr

  consKind (Curry.Module.OracleGraphInductive.C_GraphOr _ _) = Curry.RunTimeSystem.Branching
  consKind (Curry.Module.OracleGraphInductive.C_GraphFail _) = Curry.RunTimeSystem.Failed
  consKind _ = Curry.RunTimeSystem.Val

  exceptions (Curry.Module.OracleGraphInductive.C_GraphFail x) = x

  orRef (Curry.Module.OracleGraphInductive.C_GraphOr x _) = x

  branches (Curry.Module.OracleGraphInductive.C_GraphOr _ x) = x





instance (Curry t0,Curry t1) => Curry (Curry.Module.OracleGraphInductive.C_Graph t0 t1) where
  strEq (Curry.Module.OracleGraphInductive.C_Gr x1) (Curry.Module.OracleGraphInductive.C_Gr y1) st = Curry.Module.Prelude.genStrEq(x1)(y1)(st)
  strEq _ x0 _ = Curry.Module.Prelude.strEqFail(Curry.Module.Prelude.typeName(x0))

  eq (Curry.Module.OracleGraphInductive.C_Gr x1) (Curry.Module.OracleGraphInductive.C_Gr y1) st = Curry.Module.Prelude.genEq(x1)(y1)(st)
  eq _ _ _ = Curry.Module.Prelude.C_False

  propagate f (Curry.Module.OracleGraphInductive.C_Gr x1) st = Curry.Module.OracleGraphInductive.C_Gr(f((0::Int))(x1)(st))

  foldCurry f c (Curry.Module.OracleGraphInductive.C_Gr x1) st = f(x1)(c)(st)

  typeName _ = "Graph"

  showQ d (Curry.Module.OracleGraphInductive.C_Gr x1) = Prelude.showParen(Prelude.True)(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("OracleGraphInductive.Gr "))(Curry.Module.Prelude.showQ(Curry.RunTimeSystem.eleven)(x1))


  showQ _ (Curry.Module.OracleGraphInductive.C_GraphOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Show t0,Show t1) => Show (Curry.Module.OracleGraphInductive.C_Graph t0 t1) where
  showsPrec d (Curry.Module.OracleGraphInductive.C_Gr x1) = Prelude.showParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(showStr)
   where
    showStr  = (Prelude..)(Prelude.showString("Gr "))(Prelude.showsPrec(Curry.RunTimeSystem.eleven)(x1))


  showsPrec _ (Curry.Module.OracleGraphInductive.C_GraphOr r _) = Prelude.showString((:)('_')(Prelude.show(Curry.RunTimeSystem.deref(r))))





instance (Read t0,Read t1) => Read (Curry.Module.OracleGraphInductive.C_Graph t0 t1) where
  readsPrec d r = Prelude.readParen((Prelude.>)(d)(Curry.RunTimeSystem.ten))(\ r -> [(,)(Curry.Module.OracleGraphInductive.C_Gr(x1))(r1) | ((,) _ r0) <- Curry.RunTimeSystem.readQualified("OracleGraphInductive")("Gr")(r), ((,) x1 r1) <- Prelude.readsPrec(Curry.RunTimeSystem.eleven)(r0)])(r)





op_58_38 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> (Curry.Module.OracleGraphInductive.C_Graph t1 t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleGraphInductive.C_Graph t1 t0
op_58_38 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_99(x3)(x2)(x1)(st))(st)



c_matchAny :: (Curry t0,Curry t1) => (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t0 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int))) (Curry.Module.OracleGraphInductive.C_Graph t0 t1)
c_matchAny x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_95(x2)(x1)(st))(st)



c_empty :: (Curry t0,Curry t1) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleGraphInductive.C_Graph t0 t1
c_empty x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.C_Gr(Curry.Module.OracleFiniteMap.c_emptyFM(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_60))(st))(x1)(st)))(st)



c_mkGraph :: (Curry t0,Curry t1) => (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0)) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t1)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleGraphInductive.C_Graph t0 t1
c_mkGraph x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_insEdges(x3)))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_insNodes(x2)))))(x1)(st))(Curry.Module.OracleGraphInductive.c_empty(x4)(st))(x5)(st))(st)



c_buildGr :: (Curry t0,Curry t1) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)))) -> Curry.RunTimeSystem.State -> Curry.Module.OracleGraphInductive.C_Graph t1 t0))
c_buildGr x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleGraphInductive.op_58_38))(st))(Curry.Module.OracleGraphInductive.c_empty(x1)(st))))))(st)



c_mkUGraph :: (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleGraphInductive.C_Graph Curry.Module.Prelude.T0 Curry.Module.Prelude.T0
c_mkUGraph x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))))(Curry.Module.OracleGraphInductive.c_mkGraph(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.c_labUNodes(x1)(st))(x2)(x4)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.c_labUEdges(x5)(st))(x3)(x6)(st))(x7)(st))(st)



c_insNode :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.OracleGraphInductive.C_Graph t0 t1))
c_insNode x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_89(x2)(x1)(st))(st)



c_insEdge :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t0) -> (Curry.Module.OracleGraphInductive.C_Graph t1 t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleGraphInductive.C_Graph t1 t0
c_insEdge x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_88(x3)(x2)(x1)(st))(st)



c_insEdge'46_'35selFP3'35pr :: (Curry t213,Curry t212) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t212 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)))) (Curry.Module.OracleGraphInductive.C_Graph t212 t213)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)
c_insEdge'46_'35selFP3'35pr x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_87(x2)(x1)(st))(st)



c_insEdge'46_'35selFP4'35la :: (Curry t213,Curry t212) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t212 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)))) (Curry.Module.OracleGraphInductive.C_Graph t212 t213)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t212
c_insEdge'46_'35selFP4'35la x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_84(x2)(x1)(st))(st)



c_insEdge'46_'35selFP5'35su :: (Curry t213,Curry t212) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t212 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)))) (Curry.Module.OracleGraphInductive.C_Graph t212 t213)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)
c_insEdge'46_'35selFP5'35su x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_81(x2)(x1)(st))(st)



c_insEdge'46_'35selFP6'35g'39 :: (Curry t213,Curry t212) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t212 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t213 Curry.Module.Prelude.C_Int)))) (Curry.Module.OracleGraphInductive.C_Graph t212 t213)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleGraphInductive.C_Graph t212 t213
c_insEdge'46_'35selFP6'35g'39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_78(x2)(x1)(st))(st)



c_delNode :: (Curry t0,Curry t1) => Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.OracleGraphInductive.C_Graph t0 t1))
c_delNode x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_delNodes((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))))))(st)



c_delEdge :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleGraphInductive.C_Graph t0 t1
c_delEdge x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_75(x3)(x2)(x1)(st))(st)



c_insNodes :: (Curry t0,Curry t1) => (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0)) -> (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleGraphInductive.C_Graph t0 t1
c_insNodes x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_insNode))))(x3)(x2)(x1)(st))(st)



c_insEdges :: (Curry t0,Curry t1) => (Curry.Module.Prelude.List (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t0)) -> (Curry.Module.OracleGraphInductive.C_Graph t1 t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleGraphInductive.C_Graph t1 t0
c_insEdges x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleGraphInductive.c_insEdge))(st))(x3)(x2)(x1)(st))(st)



c_delNodes :: (Curry t0,Curry t1) => (Curry.Module.Prelude.List Curry.Module.Prelude.C_Int) -> (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleGraphInductive.C_Graph t0 t1
c_delNodes x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_71(x3)(x2)(x1)(st))(st)



c_delEdges :: (Curry t0,Curry t1) => (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int)) -> (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleGraphInductive.C_Graph t0 t1
c_delEdges x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_foldr(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleGraphInductive.c_delEdge))(st))(x3)(x2)(x1)(st))(st)



c_isEmpty :: (Curry t0,Curry t1) => (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_isEmpty x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_70(x2)(x1)(st))(st)



c_match :: (Curry t0,Curry t1) => Curry.Module.Prelude.C_Int -> (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t0 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)))) (Curry.Module.OracleGraphInductive.C_Graph t0 t1)
c_match x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_69(x2)(x3)(x1)(st))(st)



c_match'46_'35lambda5 :: (Curry t146,Curry t148) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.T2 (Curry.Module.OracleFiniteMap.C_FM Curry.Module.Prelude.C_Int (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t146 Curry.Module.Prelude.C_Int)) t148 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t146 Curry.Module.Prelude.C_Int)))) (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t146 Curry.Module.Prelude.C_Int)) t148 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t146 Curry.Module.Prelude.C_Int))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 (Curry.Module.Prelude.C_Maybe (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t146 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t148 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t146 Curry.Module.Prelude.C_Int)))) (Curry.Module.OracleGraphInductive.C_Graph t148 t146)
c_match'46_'35lambda5 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_68(x2)(x3)(x1)(st))(st)



c_noNodes :: (Curry t0,Curry t1) => (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_noNodes x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_65(x2)(x1)(st))(st)



c_nodeRange :: (Curry t0,Curry t1) => (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int
c_nodeRange x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_64(x2)(x1)(st))(st)



c_context :: (Curry t0,Curry t1) => (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t0 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int))
c_context x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleGraphInductive.c__case_61(x2)(x3)(Curry.Module.OracleGraphInductive.c_match(x3)(x2)(x1)(st))(x4)(st))(st)



c_lab :: (Curry t0,Curry t1) => (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Maybe t0
c_lab x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))))(Curry.Module.OracleMaybe.op_62_62_45(Curry.Module.OraclePrelude.c_fst(Curry.Module.OracleGraphInductive.c_match(x3)(x2)(x1)(st))(x4)(st))(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCons(Curry.Module.Prelude.pc(Curry.Module.Prelude.C_Just))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_lab'39))))(x5)(st))(x6)(st))(st)



c_neighbors :: (Curry t0,Curry t1) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))))
c_neighbors x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.op_46_58(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_neighbors'46_'35lambda7))))(x2)(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleGraphInductive.c_context))(st))(x3)(st))(st)



c_neighbors'46_'35lambda7 :: (Curry t416,Curry t415) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t416 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t415 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t416 Curry.Module.Prelude.C_Int))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_neighbors'46_'35lambda7 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_59(x2)(x1)(st))(st)



c_suc :: (Curry t0,Curry t1) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))))
c_suc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.op_46_58(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_snd))))))))(x2)(st))(Curry.Module.OracleGraphInductive.c_context4(x3)(st))(x4)(st))(st)



c_pre :: (Curry t0,Curry t1) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))))
c_pre x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.op_46_58(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_snd))))))))(x2)(st))(Curry.Module.OracleGraphInductive.c_context1(x3)(st))(x4)(st))(st)



c_lsuc :: (Curry t0,Curry t1) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t1)))))
c_lsuc x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.op_46_58(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_flip2))))))))(x2)(st))(Curry.Module.OracleGraphInductive.c_context4(x3)(st))(x4)(st))(st)



c_lpre :: (Curry t0,Curry t1) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t1)))))
c_lpre x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.op_46_58(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_flip2))))))))(x2)(st))(Curry.Module.OracleGraphInductive.c_context1(x3)(st))(x4)(st))(st)



c_out :: (Curry t0,Curry t1) => (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t1)
c_out x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_out'46_'35lambda8(x3)))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.c_context4(x1)(st))(x2)(x4)(st))(x3)(x5)(st))(x6)(st))(st)



c_out'46_'35lambda8 :: (Curry t511) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.T2 t511 Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t511
c_out'46_'35lambda8 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_58(x2)(x3)(x1)(st))(st)



c_inn :: (Curry t0,Curry t1) => (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.Module.Prelude.C_Int -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t1)
c_inn x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_inn'46_'35lambda9(x3)))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.c_context1(x1)(st))(x2)(x4)(st))(x3)(x5)(st))(x6)(st))(st)



c_inn'46_'35lambda9 :: (Curry t521) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.T2 t521 Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t521
c_inn'46_'35lambda9 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_57(x2)(x3)(x1)(st))(st)



c_outdeg :: (Curry t0,Curry t1) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int))))
c_outdeg x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.op_46_58(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_length))))(x2)(st))(Curry.Module.OracleGraphInductive.c_context4(x3)(st))(x4)(st))(st)



c_indeg :: (Curry t0,Curry t1) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int))))
c_indeg x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.op_46_58(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_length))))(x2)(st))(Curry.Module.OracleGraphInductive.c_context1(x3)(st))(x4)(st))(st)



c_deg :: (Curry t0,Curry t1) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int))))
c_deg x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.op_46_58(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_deg'46_'35lambda10))))(x2)(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleGraphInductive.c_context))(st))(x3)(st))(st)



c_deg'46_'35lambda10 :: (Curry t553,Curry t552) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t553 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t552 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t553 Curry.Module.Prelude.C_Int))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_deg'46_'35lambda10 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_56(x2)(x1)(st))(st)



c_gelem :: (Curry t0,Curry t1) => Curry.Module.Prelude.C_Int -> (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_gelem x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OracleMaybe.c_isJust(Curry.Module.OraclePrelude.c_fst(Curry.Module.OracleGraphInductive.c_match(x2)(x3)(x1)(st))(x4)(st))(x5)(st))(st)



c_equal :: (Curry t0,Curry t1) => (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_equal x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List)))))))))))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.c_slabNodes(x1)(st))(x2)(x4)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.c_slabNodes(x5)(st))(x3)(x6)(st))(x7)(st))(Curry.Module.OraclePrelude.op_61_61(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.c_slabEdges(x8)(st))(x2)(x9)(st))(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.c_slabEdges(x10)(st))(x3)(x11)(st))(x12)(st))(x13)(st))(st)



c_nodeComp :: (Curry t0) => (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0) -> (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_nodeComp x2 x3 x1 st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleGraphInductive.c__case_55(x2)(x3)(Curry.Module.OraclePrelude.op_61_61(x2)(x3)(x1)(st))(x4)(st))(st)



c_slabNodes :: (Curry t0,Curry t1) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0)))
c_slabNodes x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_46(Curry.Module.OracleGraphInductive.c_sortBy(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleGraphInductive.c_nodeComp))(st))(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_labNodes))))(x2)(st))(st)



c_edgeComp :: (Curry t0) => (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t0) -> (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering
c_edgeComp x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x11)(let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x11)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(Curry.Module.OracleGraphInductive.c__case_52(x2)(x3)(Curry.Module.OracleGraphInductive.c_edgeComp'46_'35selFP11'35v(x2)(x1)(st))(Curry.Module.OracleGraphInductive.c_edgeComp'46_'35selFP12'35w(x2)(x10)(st))(Curry.Module.OracleGraphInductive.c_edgeComp'46_'35selFP9'35x(x3)(x11)(st))(Curry.Module.OracleGraphInductive.c_edgeComp'46_'35selFP10'35y(x3)(x12)(st))(Curry.Module.OraclePrelude.op_61_61(x2)(x3)(x13)(st))(x14)(st))(st))(st))(st))(st))(st))(st))(st)



c_edgeComp'46_'35selFP11'35v :: (Curry t613) => (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t613) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_edgeComp'46_'35selFP11'35v x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_49(x2)(x1)(st))(st)



c_edgeComp'46_'35selFP12'35w :: (Curry t613) => (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t613) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_edgeComp'46_'35selFP12'35w x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_48(x2)(x1)(st))(st)



c_edgeComp'46_'35selFP9'35x :: (Curry t613) => (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t613) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_edgeComp'46_'35selFP9'35x x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_47(x2)(x1)(st))(st)



c_edgeComp'46_'35selFP10'35y :: (Curry t613) => (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t613) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_edgeComp'46_'35selFP10'35y x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_46(x2)(x1)(st))(st)



c_slabEdges :: (Curry t0,Curry t1) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t1)))
c_slabEdges x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_46(Curry.Module.OracleGraphInductive.c_sortBy(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleGraphInductive.c_edgeComp))(st))(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_labEdges))))(x2)(st))(st)



c_node'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_node'39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_45(x2)(x1)(st))(st)



c_lab'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t1
c_lab'39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_44(x2)(x1)(st))(st)



c_labNode'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t1
c_labNode'39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_43(x2)(x1)(st))(st)



c_neighbors'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_neighbors'39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_42(x2)(x1)(st))(st)



c_suc'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_suc'39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_41(x2)(x1)(st))(st)



c_pre'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_pre'39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_40(x2)(x1)(st))(st)



c_lpre'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0)
c_lpre'39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_39(x2)(x1)(st))(st)



c_lsuc'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0)
c_lsuc'39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_38(x2)(x1)(st))(st)



c_out'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t0)
c_out'39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_37(x2)(x1)(st))(st)



c_out'39'46_'35lambda11 :: (Curry t732) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.T2 t732 Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t732
c_out'39'46_'35lambda11 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_36(x2)(x3)(x1)(st))(st)



c_inn'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t0)
c_inn'39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_35(x2)(x1)(st))(st)



c_inn'39'46_'35lambda12 :: (Curry t742) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.T2 t742 Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t742
c_inn'39'46_'35lambda12 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_34(x2)(x3)(x1)(st))(st)



c_outdeg'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_outdeg'39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_33(x2)(x1)(st))(st)



c_indeg'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_indeg'39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_32(x2)(x1)(st))(st)



c_deg'39 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_deg'39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_31(x2)(x1)(st))(st)



c_labNodes :: (Curry t0,Curry t1) => (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t0)
c_labNodes x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_30(x2)(x1)(st))(st)



c_labNodes'46_'35lambda13 :: (Curry t574,Curry t573) => (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t574 Curry.Module.Prelude.C_Int)) t573 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t574 Curry.Module.Prelude.C_Int)))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int t573
c_labNodes'46_'35lambda13 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_29(x2)(x1)(st))(st)



c_labEdges :: (Curry t0,Curry t1) => (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t1)
c_labEdges x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_27(x2)(x1)(st))(st)



c_labEdges'46_'35lambda14 :: (Curry t620,Curry t619) => (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t620 Curry.Module.Prelude.C_Int)) t619 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t620 Curry.Module.Prelude.C_Int)))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t620)
c_labEdges'46_'35lambda14 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_26(x2)(x1)(st))(st)



c_labEdges'46_'35lambda14'46_'35lambda15 :: (Curry t620) => Curry.Module.Prelude.C_Int -> (Curry.Module.Prelude.T2 t620 Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t620
c_labEdges'46_'35lambda14'46_'35lambda15 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_24(x2)(x3)(x1)(st))(st)



c_nodes :: (Curry t0,Curry t1) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int))
c_nodes x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_fst))))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_labNodes))))(x1)(st))(st)



c_edges :: (Curry t0,Curry t1) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int)))
c_edges x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_edges'46_'35lambda16))))))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_labEdges))))(x1)(st))(st)



c_edges'46_'35lambda16 :: (Curry t788) => (Curry.Module.Prelude.T3 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int t788) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int
c_edges'46_'35lambda16 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_23(x2)(x1)(st))(st)



c_newNodes :: (Curry t0,Curry t1) => Curry.Module.Prelude.C_Int -> (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Int
c_newNodes x2 x3 x1 st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(let {x5 = Curry.Module.OracleGraphInductive.c_newNodes'46_'35selFP14'35n(Curry.Module.OracleGraphInductive.c_nodeRange(x3)(x1)(st))(x6)(st)} in let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_enumFromTo(Curry.Module.OraclePrelude.op_43(x5)(Curry.Module.Prelude.C_Pos(Curry.Module.Prelude.C_IHi))(x7)(st))(Curry.Module.OraclePrelude.op_43(x5)(x2)(x8)(st))(x9)(st))(st))(st))(st)



c_newNodes'46_'35selFP14'35n :: (Curry.Module.Prelude.T2 Curry.Module.Prelude.C_Int Curry.Module.Prelude.C_Int) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Int
c_newNodes'46_'35selFP14'35n x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_22(x2)(x1)(st))(st)



c_ufold :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t2 -> Curry.RunTimeSystem.State -> t2))))) -> t2 -> (Curry.Module.OracleGraphInductive.C_Graph t1 t0) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t2
c_ufold x2 x3 x4 x1 st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(let {x5 = Curry.Module.OracleGraphInductive.c_matchAny(x4)(x1)(st)} in let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))(let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleGraphInductive.c__case_21(x2)(x3)(x4)(Curry.Module.OracleGraphInductive.c_ufold'46_'35selFP16'35c(x5)(x8)(st))(Curry.Module.OracleGraphInductive.c_ufold'46_'35selFP17'35g'39(x5)(x9)(st))(Curry.Module.OracleGraphInductive.c_isEmpty(x4)(x10)(st))(x11)(st))(st))(st))(st))(st)



c_ufold'46_'35selFP16'35c :: (Curry t807,Curry t806) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t807 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t806 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t807 Curry.Module.Prelude.C_Int))) (Curry.Module.OracleGraphInductive.C_Graph t806 t807)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t807 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t806 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t807 Curry.Module.Prelude.C_Int))
c_ufold'46_'35selFP16'35c x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_19(x2)(x1)(st))(st)



c_ufold'46_'35selFP17'35g'39 :: (Curry t807,Curry t806) => (Curry.Module.Prelude.T2 (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t807 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t806 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t807 Curry.Module.Prelude.C_Int))) (Curry.Module.OracleGraphInductive.C_Graph t806 t807)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleGraphInductive.C_Graph t806 t807
c_ufold'46_'35selFP17'35g'39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_18(x2)(x1)(st))(st)



c_gmap :: (Curry t0,Curry t1,Curry t2,Curry t3) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t2 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t2 Curry.Module.Prelude.C_Int))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t1 t0) -> Curry.RunTimeSystem.State -> Curry.Module.OracleGraphInductive.C_Graph t3 t2))
c_gmap x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_ufold(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_gmap'46_'35lambda17(x2)))))(Curry.Module.OracleGraphInductive.c_empty(x1)(st))))))(st)



c_gmap'46_'35lambda17 :: (Curry t821,Curry t822,Curry t830,Curry t829) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t821 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t822 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t821 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t830 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t829 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t830 Curry.Module.Prelude.C_Int))))) -> (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t821 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t822 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t821 Curry.Module.Prelude.C_Int))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t829 t830) -> Curry.RunTimeSystem.State -> Curry.Module.OracleGraphInductive.C_Graph t829 t830))
c_gmap'46_'35lambda17 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.op_58_38(Curry.Module.Oracle.c_apply(x2)(x3)(x1)(st))))))(st)



c_nmap :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t0 t2) -> Curry.RunTimeSystem.State -> Curry.Module.OracleGraphInductive.C_Graph t1 t2))
c_nmap x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c_gmap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_nmap'46_'35lambda18(x2)))))(x1)(st))(st)



c_nmap'46_'35lambda18 :: (Curry t841,Curry t844,Curry t837) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t841 -> Curry.RunTimeSystem.State -> t844))) -> (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t837 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t841 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t837 Curry.Module.Prelude.C_Int))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t837 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t844 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t837 Curry.Module.Prelude.C_Int))
c_nmap'46_'35lambda18 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_17(x2)(x3)(x1)(st))(st)



c_emap :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t2 t0) -> Curry.RunTimeSystem.State -> Curry.Module.OracleGraphInductive.C_Graph t2 t1))
c_emap x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c_gmap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_emap'46_'35lambda20(x2)))))(x1)(st))(st)



c_emap'46map1'46213 :: (Curry t0,Curry t1,Curry t2) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 t2)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 t2)))
c_emap'46map1'46213 x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_emap'46map1'46213'46_'35lambda19(x2)))))))))(st)



c_emap'46map1'46213'46_'35lambda19 :: (Curry t853,Curry t856,Curry t854) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t853 -> Curry.RunTimeSystem.State -> t856))) -> (Curry.Module.Prelude.T2 t853 t854) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 t856 t854
c_emap'46map1'46213'46_'35lambda19 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_16(x2)(x3)(x1)(st))(st)



c_emap'46_'35lambda20 :: (Curry t865,Curry t866,Curry t863) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t865 -> Curry.RunTimeSystem.State -> t866))) -> (Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t865 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t863 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t865 Curry.Module.Prelude.C_Int))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T4 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t866 Curry.Module.Prelude.C_Int)) Curry.Module.Prelude.C_Int t863 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t866 Curry.Module.Prelude.C_Int))
c_emap'46_'35lambda20 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_15(x2)(x3)(x1)(st))(st)



c_labUEdges :: (Curry t0,Curry t1) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 t1)) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T3 t0 t1 Curry.Module.Prelude.T0)))
c_labUEdges x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_labUEdges'46_'35lambda21))))))))(st)



c_labUEdges'46_'35lambda21 :: (Curry t249,Curry t250) => (Curry.Module.Prelude.T2 t249 t250) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 t249 t250 Curry.Module.Prelude.T0
c_labUEdges'46_'35lambda21 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_14(x2)(x1)(st))(st)



c_labUNodes :: (Curry t0) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.T0)))
c_labUNodes x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_labUNodes'46_'35lambda22))))))))(st)



c_labUNodes'46_'35lambda22 :: (Curry t254) => t254 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 t254 Curry.Module.Prelude.T0
c_labUNodes'46_'35lambda22 x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(x2)(Curry.Module.Prelude.T0))(st)



c_showGraph :: (Curry t0,Curry t1) => (Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showGraph x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_13(x2)(x1)(st))(st)



c_showNode :: (Curry t0,Curry t1,Curry t2,Curry t3) => (Curry.Module.Prelude.T2 t0 (Curry.Module.Prelude.T3 t1 t2 t3)) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List Curry.Module.Prelude.C_Char
c_showNode x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_12(x2)(x1)(st))(st)



op_46_58 :: (Curry t0,Curry t1,Curry t2,Curry t3) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> t1))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t2 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t3 -> Curry.RunTimeSystem.State -> t0))))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t2 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t3 -> Curry.RunTimeSystem.State -> t1))))))))
op_46_58 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.op_46(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_46))(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_46))(st))(x1)(st))(st)



c_fst4 :: (Curry t0,Curry t1,Curry t2,Curry t3) => (Curry.Module.Prelude.T4 t0 t1 t2 t3) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t0
c_fst4 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_10(x2)(x1)(st))(st)



c_fth4 :: (Curry t0,Curry t1,Curry t2,Curry t3) => (Curry.Module.Prelude.T4 t0 t1 t2 t3) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> t3
c_fth4 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_9(x2)(x1)(st))(st)



c_flip2 :: (Curry t0,Curry t1) => (Curry.Module.Prelude.T2 t0 t1) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T2 t1 t0
c_flip2 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_8(x2)(x1)(st))(st)



c_context1 :: (Curry t0,Curry t1) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)))))
c_context1 x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.op_46_58(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_fst4))))(x2)(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleGraphInductive.c_context))(st))(x3)(st))(st)



c_context4 :: (Curry t0,Curry t1) => Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.OracleGraphInductive.C_Graph t0 t1) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.Prelude.C_Int -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 Curry.Module.Prelude.C_Int)))))
c_context4 x1 st = let {x2 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x3 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x2)((Curry.Module.Prelude.:<)(x3)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.op_46_58(x1)(st))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_fth4))))(x2)(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleGraphInductive.c_context))(st))(x3)(st))(st)



c_addSucc :: (Curry t0,Curry t1,Curry t2,Curry t3) => t0 -> t1 -> (Curry.Module.Prelude.T3 t2 t3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 t0))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 t2 t3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 t0))
c_addSucc x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_7(x2)(x3)(x4)(x1)(st))(st)



c_addPred :: (Curry t0,Curry t1,Curry t2,Curry t3) => t0 -> t1 -> (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 t0)) t2 t3) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t1 t0)) t2 t3
c_addPred x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_6(x2)(x3)(x4)(x1)(st))(st)



c_clearSucc :: (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4) => t0 -> t1 -> (Curry.Module.Prelude.T3 t2 t3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t4 t0))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 t2 t3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t4 t0))
c_clearSucc x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_5(x2)(x4)(x1)(st))(st)



c_clearPred :: (Curry t0,Curry t1,Curry t2,Curry t3,Curry t4) => t0 -> t1 -> (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t2 t0)) t3 t4) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t2 t0)) t3 t4
c_clearPred x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_4(x2)(x4)(x1)(st))(st)



c_updAdj :: (Curry t0,Curry t1) => (Curry.Module.OracleFiniteMap.C_FM Curry.Module.Prelude.C_Int (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)))) -> (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) -> (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int))))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.OracleFiniteMap.C_FM Curry.Module.Prelude.C_Int (Curry.Module.Prelude.T3 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)) t1 (Curry.Module.Prelude.List (Curry.Module.Prelude.T2 t0 Curry.Module.Prelude.C_Int)))
c_updAdj x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_3(x2)(x4)(x3)(x1)(st))(st)



c_sortBy :: (Curry t0) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t0 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering))))) -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim ((Curry.Module.Prelude.List t0) -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.List t0))
c_sortBy x2 x1 st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleSort.c_mergeSort(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleGraphInductive.c_sortBy'46_'35lambda23(x2)))(st))))))(st)



c_sortBy'46_'35lambda23 :: (Curry t587) => (Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t587 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.Prim (t587 -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Ordering))))) -> t587 -> t587 -> Curry.Module.CEventOracle.C_Ref -> Curry.RunTimeSystem.State -> Curry.Module.Prelude.C_Bool
c_sortBy'46_'35lambda23 x2 x3 x4 x1 st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(let {x5 = Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x3)(x1)(st))(x4)(x6)(st)} in let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_61_61(x5)(Curry.Module.Prelude.C_EQ)(x7)(st))(Curry.Module.OraclePrelude.op_61_61(x5)(Curry.Module.Prelude.C_LT)(x8)(st))(x9)(st))(st))(st)



c__case_3 x2 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_3_case__99(x1)(x2)(x4)(x3)(st))(st)



c__case_2 x2 x4 x6 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_2_case__98(x1)(x2)(x4)(x6)(x5)(st))(st)



c__case_1 x2 x4 x6 x7 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_1_case__97(x1)(x2)(x4)(x6)(x7)(x8)(x9)(st))(st)



c__case_0 x8 x9 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_0_case__96(x1)(x8)(x9)(st))(st)



c__case_4 x2 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_4_case__95(x1)(x2)(x4)(st))(st)



c__case_5 x2 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_5_case__94(x1)(x2)(x4)(st))(st)



c__case_6 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_6_case__93(x1)(x2)(x3)(x4)(st))(st)



c__case_7 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_7_case__92(x1)(x2)(x3)(x4)(st))(st)



c__case_8 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_8_case__91(x1)(x2)(st))(st)



c__case_9 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_9_case__90(x1)(x2)(st))(st)



c__case_10 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_10_case__89(x1)(x2)(st))(st)



c__case_12 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_12_case__88(x1)(x2)(st))(st)



c__case_11 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_11_case__87(x1)(x3)(x4)(st))(st)



c__case_13 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_13_case__86(x1)(x2)(st))(st)



c__case_14 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_14_case__85(x1)(x2)(st))(st)



c__case_15 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_15_case__84(x1)(x2)(x3)(st))(st)



c__case_16 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_16_case__83(x1)(x2)(x3)(st))(st)



c__case_17 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_17_case__82(x1)(x2)(x3)(st))(st)



c__case_18 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_18_case__81(x1)(x2)(st))(st)



c__case_19 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_19_case__80(x1)(x2)(st))(st)



c__case_21 x2 x3 x4 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_21_case__79(x1)(x2)(x3)(x6)(x7)(x8)(st))(st)



c__case_20 x2 x3 x6 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_20_case__78(x1)(x2)(x3)(x6)(x7)(x8)(st))(st)



c__case_22 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_22_case__77(x1)(x2)(st))(st)



c__case_23 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_23_case__76(x1)(x2)(st))(st)



c__case_24 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_24_case__75(x1)(x2)(x3)(st))(st)



c__case_26 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_26_case__74(x1)(x2)(st))(st)



c__case_25 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_25_case__73(x1)(x3)(x4)(st))(st)



c__case_27 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_27_case__72(x1)(x2)(st))(st)



c__case_29 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_29_case__71(x1)(x2)(st))(st)



c__case_28 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_28_case__70(x1)(x3)(x4)(st))(st)



c__case_30 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_30_case__69(x1)(x2)(st))(st)



c__case_31 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_31_case__68(x1)(x2)(st))(st)



c__case_32 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_32_case__67(x1)(x2)(st))(st)



c__case_33 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_33_case__66(x1)(x2)(st))(st)



c__case_34 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_34_case__65(x1)(x2)(x3)(st))(st)



c__case_35 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_35_case__64(x1)(x2)(st))(st)



c__case_36 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_36_case__63(x1)(x2)(x3)(st))(st)



c__case_37 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_37_case__62(x1)(x2)(st))(st)



c__case_38 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_38_case__61(x1)(x2)(st))(st)



c__case_39 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_39_case__60(x1)(x2)(st))(st)



c__case_40 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_40_case__59(x1)(x2)(st))(st)



c__case_41 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_41_case__58(x1)(x2)(st))(st)



c__case_42 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_42_case__57(x1)(x2)(st))(st)



c__case_43 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_43_case__56(x1)(x2)(st))(st)



c__case_44 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_44_case__55(x1)(x2)(st))(st)



c__case_45 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_45_case__54(x1)(x2)(st))(st)



c__case_46 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_46_case__53(x1)(x2)(st))(st)



c__case_47 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_47_case__52(x1)(x2)(st))(st)



c__case_48 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_48_case__51(x1)(x2)(st))(st)



c__case_49 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_49_case__50(x1)(x2)(st))(st)



c__case_52 x2 x3 x5 x6 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_52_case__49(x1)(x5)(x6)(x8)(x9)(x10)(st))(st)



c__case_51 x5 x6 x8 x9 x10 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_51_case__48(x1)(x10)(st))(st)



c__case_50 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_50_case__47(x1)(x2)(st))(st)



c__case_55 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_55_case__46(x1)(x2)(x3)(x4)(st))(st)



c__case_54 x2 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_54_case__45(x1)(x4)(st))(st)



c__case_53 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_53_case__44(x1)(x2)(st))(st)



c__case_56 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_56_case__43(x1)(x2)(st))(st)



c__case_57 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_57_case__42(x1)(x2)(x3)(st))(st)



c__case_58 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_58_case__41(x1)(x2)(x3)(st))(st)



c__case_59 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_59_case__40(x1)(x2)(st))(st)



c__case_61 x2 x3 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_61_case__39(x1)(x3)(x6)(st))(st)



c__case_60 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_60_case__38(x1)(x3)(x4)(st))(st)



c__case_64 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_64_case__37(x1)(x2)(st))(st)



c__case_63 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_63_case__36(x1)(x3)(x4)(x5)(st))(st)



c__case_62 x3 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_62_case__35(x1)(x3)(x4)(x5)(st))(st)



c__case_65 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_65_case__34(x1)(x2)(st))(st)



c__case_68 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_68_case__33(x1)(x2)(x3)(st))(st)



c__case_67 x2 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_67_case__32(x1)(x2)(x4)(x5)(st))(st)



c__case_66 x2 x4 x7 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_66_case__31(x1)(x2)(x4)(x7)(st))(st)



c__case_69 x2 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_69_case__30(x1)(x2)(x3)(st))(st)



c__case_70 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_70_case__29(x1)(x2)(st))(st)



c__case_71 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_71_case__28(x1)(x3)(x2)(st))(st)



c__case_75 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_75_case__27(x1)(x3)(x2)(st))(st)



c__case_74 x3 x4 x5 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_74_case__26(x1)(x3)(x5)(x8)(st))(st)



c__case_73 x3 x5 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_73_case__25(x1)(x3)(x5)(x7)(x6)(st))(st)



c__case_72 x5 x7 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_72_case__24(x1)(x5)(x7)(x8)(st))(st)



c__case_78 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_78_case__23(x1)(x2)(st))(st)



c__case_77 x4 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_77_case__22(x1)(x4)(x3)(st))(st)



c__case_76 x4 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_76_case__21(x1)(x4)(x5)(st))(st)



c__case_81 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_81_case__20(x1)(x2)(st))(st)



c__case_80 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_80_case__19(x1)(x3)(st))(st)



c__case_79 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_79_case__18(x1)(x5)(st))(st)



c__case_84 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_84_case__17(x1)(x2)(st))(st)



c__case_83 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_83_case__16(x1)(x3)(st))(st)



c__case_82 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_82_case__15(x1)(x5)(st))(st)



c__case_87 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_87_case__14(x1)(x2)(st))(st)



c__case_86 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_86_case__13(x1)(x3)(st))(st)



c__case_85 x5 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_85_case__12(x1)(x5)(st))(st)



c__case_88 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_88_case__11(x1)(x3)(x2)(st))(st)



c__case_89 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_89_case__10(x1)(x2)(st))(st)



c__case_95 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_95_case__9(x1)(x2)(st))(st)



c__case_94 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_94_case__8(x1)(x3)(x4)(st))(st)



c__case_93 x3 x4 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_93_case__7(x1)(x3)(x4)(st))(st)



c__case_92 x3 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_92_case__6(x1)(x3)(x6)(st))(st)



c__case_91 x3 x4 x8 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_91_case__5(x1)(x8)(st))(st)



c__case_90 x7 x6 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_90_case__4(x1)(x7)(x6)(st))(st)



c__case_99 x3 x2 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_99_case__3(x1)(x3)(x2)(st))(st)



c__case_98 x4 x5 x6 x7 x3 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_98_case__2(x1)(x4)(x5)(x6)(x7)(x3)(st))(st)



c__case_97 x5 x6 x8 x11 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_97_case__1(x1)(x5)(x6)(x11)(x12)(st))(st)



c__case_96 x11 x12 x1 st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_96_case__0(x1)(x11)(x12)(st))(st)



c__case_96_case__0 x1 x11 x12@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.OracleGraphInductive.C_Gr(x11))(st)
c__case_96_case__0 x1 x11 x12@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_96_case__0 x1 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_96_case__0(x1)(x11)(x)(st))(i)(xs)(st)
c__case_96_case__0 x1 x11 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_96_case__0")(x)



c__case_97_case__1 x1 x5 x6 x11 x12@Curry.Module.Prelude.C_True st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)((Curry.Module.Prelude.:<)(x17)(Curry.Module.Prelude.List))))))(Curry.Module.OraclePrelude.c_error(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x5)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_show(x6)(x13)(st))(x14)(st))(x15)(st))(x16)(st))(x17)(st))(st)
c__case_97_case__1 x1 x5 x6 x11 x12@Curry.Module.Prelude.C_False st = let {x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List))(Curry.Module.OracleGraphInductive.c__case_96(x11)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x18)(st))(st)
c__case_97_case__1 x1 x5 x6 x11 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_97_case__1(x1)(x5)(x6)(x11)(x)(st))(i)(xs)(st)
c__case_97_case__1 x1 x5 x6 x11 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_97_case__1")(x)



c__case_98_case__2 x1 x4 x5 x6 x7 x3@(Curry.Module.OracleGraphInductive.C_Gr x8) st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(Curry.Module.OracleGraphInductive.c__case_97(x5)(x6)(x8)(Curry.Module.OracleGraphInductive.c_updAdj(Curry.Module.OracleGraphInductive.c_updAdj(Curry.Module.OracleFiniteMap.c_addToFM(x8)(x5)(Curry.Module.Prelude.T3(x4)(x6)(x7))(x1)(st))(x4)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleGraphInductive.c_addSucc(x5)))(st))(x12)(st))(x7)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleGraphInductive.c_addPred(x5)))(st))(x13)(st))(Curry.Module.OracleFiniteMap.c_elemFM(x5)(x8)(x14)(st))(x15)(st))(st))(st))(st))(st)
c__case_98_case__2 x1 x4 x5 x6 x7 (Curry.Module.OracleGraphInductive.C_GraphOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_98_case__2(x1)(x4)(x5)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_98_case__2 x1 x4 x5 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_98_case__2")(x)



c__case_99_case__3 x1 x3 x2@(Curry.Module.Prelude.T4 x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_98(x4)(x5)(x6)(x7)(x3)(x1)(st))(st)
c__case_99_case__3 x1 x3 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_99_case__3(x1)(x3)(x)(st))(i)(xs)(st)
c__case_99_case__3 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_99_case__3")(x)



c__case_90_case__4 x1 x7 x6@(Curry.Module.Prelude.C_Just x8) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(x8)(x7))(st)
c__case_90_case__4 x1 x7 x6@Curry.Module.Prelude.C_Nothing st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_90_case__4 x1 x7 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_90_case__4(x1)(x7)(x)(st))(i)(xs)(st)
c__case_90_case__4 x1 x7 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_90_case__4")(x)



c__case_91_case__5 x1 x8@(Curry.Module.Prelude.T2 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_90(x7)(x6)(x1)(st))(st)
c__case_91_case__5 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_91_case__5(x1)(x)(st))(i)(xs)(st)
c__case_91_case__5 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_91_case__5")(x)



c__case_92_case__6 x1 x3 x6@(Curry.Module.Prelude.T2 x4 x5) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OracleGraphInductive.c__case_91(x3)(x4)(Curry.Module.OracleGraphInductive.c_match(x4)(Curry.Module.OracleGraphInductive.C_Gr(x3))(x1)(st))(x7)(st))(st)
c__case_92_case__6 x1 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_92_case__6(x1)(x3)(x)(st))(i)(xs)(st)
c__case_92_case__6 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_92_case__6")(x)



c__case_93_case__7 x1 x3 x4@Curry.Module.Prelude.C_True st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List)))(Curry.Module.OracleGraphInductive.c__case_92(x3)(Curry.Module.OraclePrelude.c_head(Curry.Module.OracleFiniteMap.c_fmToListPreOrder(x3)(x1)(st))(x5)(st))(x6)(st))(st)
c__case_93_case__7 x1 x3 x4@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_93_case__7 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_93_case__7(x1)(x3)(x)(st))(i)(xs)(st)
c__case_93_case__7 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_93_case__7")(x)



c__case_94_case__8 x1 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_error((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('m'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('y'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('G'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('r'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))(Curry.Module.Prelude.List)))))))))))))))))))))))))))))(x1)(st))(st)
c__case_94_case__8 x1 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleGraphInductive.c__case_93(x3)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x5)(st))(st)
c__case_94_case__8 x1 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_94_case__8(x1)(x3)(x)(st))(i)(xs)(st)
c__case_94_case__8 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_94_case__8")(x)



c__case_95_case__9 x1 x2@(Curry.Module.OracleGraphInductive.C_Gr x3) st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OracleGraphInductive.c__case_94(x3)(Curry.Module.OracleFiniteMap.c_isEmptyFM(x3)(x1)(st))(x4)(st))(st)
c__case_95_case__9 x1 (Curry.Module.OracleGraphInductive.C_GraphOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_95_case__9(x1)(x)(st))(i)(xs)(st)
c__case_95_case__9 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_95_case__9")(x)



c__case_89_case__10 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.op_58_38(Curry.Module.Prelude.T4(Curry.Module.Prelude.List)(x3)(x4)(Curry.Module.Prelude.List))))))(st)
c__case_89_case__10 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_89_case__10(x1)(x)(st))(i)(xs)(st)
c__case_89_case__10 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_89_case__10")(x)



c__case_88_case__11 x1 x3 x2@(Curry.Module.Prelude.T3 x4 x5 x6) st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(let {x7 = Curry.Module.OracleGraphInductive.c_match(x4)(x3)(x1)(st)} in let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List))(let {x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List))(let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))(let {x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_replace(x16)(Curry.Module.OracleGraphInductive.op_58_38(Curry.Module.Prelude.T4(Curry.Module.OracleGraphInductive.c_insEdge'46_'35selFP3'35pr(x7)(x12)(st))(x4)(Curry.Module.OracleGraphInductive.c_insEdge'46_'35selFP4'35la(x7)(x13)(st))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(x6)(x5))(Curry.Module.OracleGraphInductive.c_insEdge'46_'35selFP5'35su(x7)(x14)(st))))(Curry.Module.OracleGraphInductive.c_insEdge'46_'35selFP6'35g'39(x7)(x15)(st))(x16)(st))(st))(st))(st))(st))(st))(st)
c__case_88_case__11 x1 x3 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_88_case__11(x1)(x3)(x)(st))(i)(xs)(st)
c__case_88_case__11 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_88_case__11")(x)



c__case_85_case__12 x1 x5@(Curry.Module.Prelude.T4 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_85_case__12 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_85_case__12(x1)(x)(st))(i)(xs)(st)
c__case_85_case__12 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_85_case__12")(x)



c__case_86_case__13 x1 x3@(Curry.Module.Prelude.C_Just x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_85(x5)(x1)(st))(st)
c__case_86_case__13 x1 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_86_case__13(x1)(x)(st))(i)(xs)(st)
c__case_86_case__13 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_86_case__13")(x)



c__case_87_case__14 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_86(x3)(x1)(st))(st)
c__case_87_case__14 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_87_case__14(x1)(x)(st))(i)(xs)(st)
c__case_87_case__14 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_87_case__14")(x)



c__case_82_case__15 x1 x5@(Curry.Module.Prelude.T4 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x8)(st)
c__case_82_case__15 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_82_case__15(x1)(x)(st))(i)(xs)(st)
c__case_82_case__15 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_82_case__15")(x)



c__case_83_case__16 x1 x3@(Curry.Module.Prelude.C_Just x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_82(x5)(x1)(st))(st)
c__case_83_case__16 x1 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_83_case__16(x1)(x)(st))(i)(xs)(st)
c__case_83_case__16 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_83_case__16")(x)



c__case_84_case__17 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_83(x3)(x1)(st))(st)
c__case_84_case__17 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_84_case__17(x1)(x)(st))(i)(xs)(st)
c__case_84_case__17 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_84_case__17")(x)



c__case_79_case__18 x1 x5@(Curry.Module.Prelude.T4 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x9)(st)
c__case_79_case__18 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_79_case__18(x1)(x)(st))(i)(xs)(st)
c__case_79_case__18 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_79_case__18")(x)



c__case_80_case__19 x1 x3@(Curry.Module.Prelude.C_Just x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_79(x5)(x1)(st))(st)
c__case_80_case__19 x1 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_80_case__19(x1)(x)(st))(i)(xs)(st)
c__case_80_case__19 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_80_case__19")(x)



c__case_81_case__20 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_80(x3)(x1)(st))(st)
c__case_81_case__20 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_81_case__20(x1)(x)(st))(i)(xs)(st)
c__case_81_case__20 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_81_case__20")(x)



c__case_76_case__21 x1 x4 x5@(Curry.Module.Prelude.T4 x6 x7 x8 x9) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_76_case__21 x1 x4 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_76_case__21(x1)(x4)(x)(st))(i)(xs)(st)
c__case_76_case__21 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_76_case__21")(x)



c__case_77_case__22 x1 x4 x3@(Curry.Module.Prelude.C_Just x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_76(x4)(x5)(x1)(st))(st)
c__case_77_case__22 x1 x4 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_77_case__22(x1)(x4)(x)(st))(i)(xs)(st)
c__case_77_case__22 x1 x4 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_77_case__22")(x)



c__case_78_case__23 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_77(x4)(x3)(x1)(st))(st)
c__case_78_case__23 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_78_case__23(x1)(x)(st))(i)(xs)(st)
c__case_78_case__23 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_78_case__23")(x)



c__case_72_case__24 x1 x5 x7 x8@(Curry.Module.Prelude.T4 x9 x10 x11 x12) st = let {x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)(Curry.Module.Prelude.List)))(Curry.Module.OracleGraphInductive.op_58_38(Curry.Module.Prelude.T4(x9)(x10)(x11)(Curry.Module.OraclePrelude.c_filter(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_47_61))(st))(x5)))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_snd))))(x1)(st))(x12)(x13)(st)))(x7)(x14)(st))(st)
c__case_72_case__24 x1 x5 x7 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_72_case__24(x1)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_72_case__24 x1 x5 x7 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_72_case__24")(x)



c__case_73_case__25 x1 x3 x5 x7 x6@Curry.Module.Prelude.C_Nothing st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_73_case__25 x1 x3 x5 x7 x6@(Curry.Module.Prelude.C_Just x8) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_72(x5)(x7)(x8)(x1)(st))(st)
c__case_73_case__25 x1 x3 x5 x7 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_73_case__25(x1)(x3)(x5)(x7)(x)(st))(i)(xs)(st)
c__case_73_case__25 x1 x3 x5 x7 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_73_case__25")(x)



c__case_74_case__26 x1 x3 x5 x8@(Curry.Module.Prelude.T2 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_73(x3)(x5)(x7)(x6)(x1)(st))(st)
c__case_74_case__26 x1 x3 x5 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_74_case__26(x1)(x3)(x5)(x)(st))(i)(xs)(st)
c__case_74_case__26 x1 x3 x5 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_74_case__26")(x)



c__case_75_case__27 x1 x3 x2@(Curry.Module.Prelude.T2 x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleGraphInductive.c__case_74(x3)(x4)(x5)(Curry.Module.OracleGraphInductive.c_match(x4)(x3)(x1)(st))(x6)(st))(st)
c__case_75_case__27 x1 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_75_case__27(x1)(x3)(x)(st))(i)(xs)(st)
c__case_75_case__27 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_75_case__27")(x)



c__case_71_case__28 x1 x3 x2@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_71_case__28 x1 x3 x2@((Curry.Module.Prelude.:<) x4 x5) st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List)))(Curry.Module.OracleGraphInductive.c_delNodes(x5)(Curry.Module.OraclePrelude.c_snd(Curry.Module.OracleGraphInductive.c_match(x4)(x3)(x1)(st))(x6)(st))(x7)(st))(st)
c__case_71_case__28 x1 x3 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_71_case__28(x1)(x3)(x)(st))(i)(xs)(st)
c__case_71_case__28 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_71_case__28")(x)



c__case_70_case__29 x1 x2@(Curry.Module.OracleGraphInductive.C_Gr x3) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFiniteMap.c_isEmptyFM(x3)(x1)(st))(st)
c__case_70_case__29 x1 (Curry.Module.OracleGraphInductive.C_GraphOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_70_case__29(x1)(x)(st))(i)(xs)(st)
c__case_70_case__29 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_70_case__29")(x)



c__case_69_case__30 x1 x2 x3@(Curry.Module.OracleGraphInductive.C_Gr x4) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_maybe(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Nothing)(Curry.Module.OracleGraphInductive.C_Gr(x4)))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_match'46_'35lambda5(x2)))))(Curry.Module.OracleFiniteMap.c_splitFM(x4)(x2)(x1)(st))(x5)(st))(st)
c__case_69_case__30 x1 x2 (Curry.Module.OracleGraphInductive.C_GraphOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_69_case__30(x1)(x2)(x)(st))(i)(xs)(st)
c__case_69_case__30 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_69_case__30")(x)



c__case_66_case__31 x1 x2 x4 x7@(Curry.Module.Prelude.T3 x8 x9 x10) st = let {x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x16 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x15)((Curry.Module.Prelude.:<)(x16)(Curry.Module.Prelude.List)))(let {x17 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x18 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x16)((Curry.Module.Prelude.:<)(x17)((Curry.Module.Prelude.:<)(x18)(Curry.Module.Prelude.List)))(let {x12 = Curry.Module.OraclePrelude.c_filter(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_47_61))(st))(x2)))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_snd))))(x16)(st))(x8)(x17)(st)} in let {x19 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x18)((Curry.Module.Prelude.:<)(x19)(Curry.Module.Prelude.List))(let {x20 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x19)((Curry.Module.Prelude.:<)(x20)(Curry.Module.Prelude.List))(Curry.Module.CEventOracle.c_collapse(x20)(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Just(Curry.Module.Prelude.T4(x12)(x2)(x9)(x10)))(Curry.Module.OracleGraphInductive.C_Gr(Curry.Module.OracleGraphInductive.c_updAdj(Curry.Module.OracleGraphInductive.c_updAdj(x4)(Curry.Module.OraclePrelude.c_filter(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_47_61))(st))(x2)))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_snd))))(x1)(st))(x10)(x15)(st))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleGraphInductive.c_clearPred(x2)))(st))(x18)(st))(x12)(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OracleGraphInductive.c_clearSucc(x2)))(st))(x19)(st))))(st))(st))(st))(st))(st)
c__case_66_case__31 x1 x2 x4 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_66_case__31(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_66_case__31 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_66_case__31")(x)



c__case_67_case__32 x1 x2 x4 x5@(Curry.Module.Prelude.T2 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_66(x2)(x4)(x7)(x1)(st))(st)
c__case_67_case__32 x1 x2 x4 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_67_case__32(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_67_case__32 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_67_case__32")(x)



c__case_68_case__33 x1 x2 x3@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_67(x2)(x4)(x5)(x1)(st))(st)
c__case_68_case__33 x1 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_68_case__33(x1)(x2)(x)(st))(i)(xs)(st)
c__case_68_case__33 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_68_case__33")(x)



c__case_65_case__34 x1 x2@(Curry.Module.OracleGraphInductive.C_Gr x3) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleFiniteMap.c_sizeFM(x3)(x1)(st))(st)
c__case_65_case__34 x1 (Curry.Module.OracleGraphInductive.C_GraphOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_65_case__34(x1)(x)(st))(i)(xs)(st)
c__case_65_case__34 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_65_case__34")(x)



c__case_62_case__35 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))))))(Curry.Module.Prelude.T2(Curry.Module.Oracle.c_apply(x4)(Curry.Module.Oracle.c_apply(Curry.Module.OracleFiniteMap.c_minFM(x1)(st))(x3)(x6)(st))(x7)(st))(Curry.Module.Oracle.c_apply(x4)(Curry.Module.Oracle.c_apply(Curry.Module.OracleFiniteMap.c_maxFM(x8)(st))(x3)(x9)(st))(x10)(st)))(st)
c__case_62_case__35 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_62_case__35 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_62_case__35(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_62_case__35 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_62_case__35")(x)



c__case_63_case__36 x1 x3 x4 x5@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(Curry.Module.Prelude.C_Zero)(Curry.Module.Prelude.C_Zero))(st)
c__case_63_case__36 x1 x3 x4 x5@Curry.Module.Prelude.C_False st = let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleGraphInductive.c__case_62(x3)(x4)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x6)(st))(st)
c__case_63_case__36 x1 x3 x4 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_63_case__36(x1)(x3)(x4)(x)(st))(i)(xs)(st)
c__case_63_case__36 x1 x3 x4 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_63_case__36")(x)



c__case_64_case__37 x1 x2@(Curry.Module.OracleGraphInductive.C_Gr x3) st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(let {x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x5)((Curry.Module.Prelude.:<)(x6)(Curry.Module.Prelude.List))(Curry.Module.OracleGraphInductive.c__case_63(x3)(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_fst))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleMaybe.c_fromJust))))(x1)(st))(Curry.Module.OracleFiniteMap.c_isEmptyFM(x3)(x5)(st))(x6)(st))(st))(st)
c__case_64_case__37 x1 (Curry.Module.OracleGraphInductive.C_GraphOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_64_case__37(x1)(x)(st))(i)(xs)(st)
c__case_64_case__37 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_64_case__37")(x)



c__case_60_case__38 x1 x3 x4@Curry.Module.Prelude.C_Nothing st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_error(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('M'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('a'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('h'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List))))))))))))))))))))))))(Curry.Module.OraclePrelude.c_show(x3)(x1)(st))(x7)(st))(x8)(st))(st)
c__case_60_case__38 x1 x3 x4@(Curry.Module.Prelude.C_Just x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_60_case__38 x1 x3 (Curry.Module.Prelude.C_MaybeOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_60_case__38(x1)(x3)(x)(st))(i)(xs)(st)
c__case_60_case__38 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_60_case__38")(x)



c__case_61_case__39 x1 x3 x6@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_60(x3)(x4)(x1)(st))(st)
c__case_61_case__39 x1 x3 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_61_case__39(x1)(x3)(x)(st))(i)(xs)(st)
c__case_61_case__39 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_61_case__39")(x)



c__case_59_case__40 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_snd))))(Curry.Module.OraclePrelude.op_43_43(x3)(x6)(x1)(st))(x7)(st))(st)
c__case_59_case__40 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_59_case__40(x1)(x)(st))(i)(xs)(st)
c__case_59_case__40 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_59_case__40")(x)



c__case_58_case__41 x1 x2 x3@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T3(x2)(x5)(x4))(st)
c__case_58_case__41 x1 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_58_case__41(x1)(x2)(x)(st))(i)(xs)(st)
c__case_58_case__41 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_58_case__41")(x)



c__case_57_case__42 x1 x2 x3@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T3(x5)(x2)(x4))(st)
c__case_57_case__42 x1 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_57_case__42(x1)(x2)(x)(st))(i)(xs)(st)
c__case_57_case__42 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_57_case__42")(x)



c__case_56_case__43 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.c_length(x3)(x1)(st))(Curry.Module.OraclePrelude.c_length(x6)(x7)(st))(x8)(st))(st)
c__case_56_case__43 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_56_case__43(x1)(x)(st))(i)(xs)(st)
c__case_56_case__43 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_56_case__43")(x)



c__case_53_case__44 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_GT)(st)
c__case_53_case__44 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_53_case__44 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_53_case__44(x1)(x)(st))(i)(xs)(st)
c__case_53_case__44 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_53_case__44")(x)



c__case_54_case__45 x1 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_LT)(st)
c__case_54_case__45 x1 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List))(Curry.Module.OracleGraphInductive.c__case_53(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x5)(st))(st)
c__case_54_case__45 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_54_case__45(x1)(x)(st))(i)(xs)(st)
c__case_54_case__45 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_54_case__45")(x)



c__case_55_case__46 x1 x2 x3 x4@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_EQ)(st)
c__case_55_case__46 x1 x2 x3 x4@Curry.Module.Prelude.C_False st = let {x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x6 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x5)((Curry.Module.Prelude.:<)(x6)((Curry.Module.Prelude.:<)(x7)(Curry.Module.Prelude.List))))(Curry.Module.OracleGraphInductive.c__case_54(x2)(x3)(Curry.Module.OraclePrelude.op_60(Curry.Module.OraclePrelude.c_fst(x2)(x1)(st))(Curry.Module.OraclePrelude.c_fst(x3)(x5)(st))(x6)(st))(x7)(st))(st)
c__case_55_case__46 x1 x2 x3 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_55_case__46(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_55_case__46 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_55_case__46")(x)



c__case_50_case__47 x1 x2@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_GT)(st)
c__case_50_case__47 x1 x2@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_50_case__47 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_50_case__47(x1)(x)(st))(i)(xs)(st)
c__case_50_case__47 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_50_case__47")(x)



c__case_51_case__48 x1 x10@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_LT)(st)
c__case_51_case__48 x1 x10@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List))(Curry.Module.OracleGraphInductive.c__case_50(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x11)(st))(st)
c__case_51_case__48 x1 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_51_case__48(x1)(x)(st))(i)(xs)(st)
c__case_51_case__48 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_51_case__48")(x)



c__case_52_case__49 x1 x5 x6 x8 x9 x10@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.C_EQ)(st)
c__case_52_case__49 x1 x5 x6 x8 x9 x10@Curry.Module.Prelude.C_False st = let {x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x14 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x15 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)((Curry.Module.Prelude.:<)(x14)((Curry.Module.Prelude.:<)(x15)(Curry.Module.Prelude.List))))))(Curry.Module.OracleGraphInductive.c__case_51(x5)(x6)(x8)(x9)(Curry.Module.OraclePrelude.op_124_124(Curry.Module.OraclePrelude.op_60(x5)(x8)(x1)(st))(Curry.Module.OraclePrelude.op_38_38(Curry.Module.OraclePrelude.op_61_61(x5)(x8)(x11)(st))(Curry.Module.OraclePrelude.op_60(x6)(x9)(x12)(st))(x13)(st))(x14)(st))(x15)(st))(st)
c__case_52_case__49 x1 x5 x6 x8 x9 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_52_case__49(x1)(x5)(x6)(x8)(x9)(x)(st))(i)(xs)(st)
c__case_52_case__49 x1 x5 x6 x8 x9 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_52_case__49")(x)



c__case_49_case__50 x1 x2@(Curry.Module.Prelude.T3 x3 x4 x5) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_49_case__50 x1 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_49_case__50(x1)(x)(st))(i)(xs)(st)
c__case_49_case__50 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_49_case__50")(x)



c__case_48_case__51 x1 x2@(Curry.Module.Prelude.T3 x3 x4 x5) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_48_case__51 x1 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_48_case__51(x1)(x)(st))(i)(xs)(st)
c__case_48_case__51 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_48_case__51")(x)



c__case_47_case__52 x1 x2@(Curry.Module.Prelude.T3 x3 x4 x5) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_47_case__52 x1 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_47_case__52(x1)(x)(st))(i)(xs)(st)
c__case_47_case__52 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_47_case__52")(x)



c__case_46_case__53 x1 x2@(Curry.Module.Prelude.T3 x3 x4 x5) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_46_case__53 x1 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_46_case__53(x1)(x)(st))(i)(xs)(st)
c__case_46_case__53 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_46_case__53")(x)



c__case_45_case__54 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_45_case__54 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_45_case__54(x1)(x)(st))(i)(xs)(st)
c__case_45_case__54 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_45_case__54")(x)



c__case_44_case__55 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x5)(st)
c__case_44_case__55 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_44_case__55(x1)(x)(st))(i)(xs)(st)
c__case_44_case__55 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_44_case__55")(x)



c__case_43_case__56 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(x4)(x5))(st)
c__case_43_case__56 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_43_case__56(x1)(x)(st))(i)(xs)(st)
c__case_43_case__56 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_43_case__56")(x)



c__case_42_case__57 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_snd))))(x3)(x1)(st))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_snd))))(x6)(x7)(st))(x8)(st))(st)
c__case_42_case__57 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_42_case__57(x1)(x)(st))(i)(xs)(st)
c__case_42_case__57 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_42_case__57")(x)



c__case_41_case__58 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_snd))))(x6)(x1)(st))(st)
c__case_41_case__58 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_41_case__58(x1)(x)(st))(i)(xs)(st)
c__case_41_case__58 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_41_case__58")(x)



c__case_40_case__59 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_snd))))(x3)(x1)(st))(st)
c__case_40_case__59 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_40_case__59(x1)(x)(st))(i)(xs)(st)
c__case_40_case__59 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_40_case__59")(x)



c__case_39_case__60 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_flip2))))(x3)(x1)(st))(st)
c__case_39_case__60 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_39_case__60(x1)(x)(st))(i)(xs)(st)
c__case_39_case__60 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_39_case__60")(x)



c__case_38_case__61 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_flip2))))(x6)(x1)(st))(st)
c__case_38_case__61 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_38_case__61(x1)(x)(st))(i)(xs)(st)
c__case_38_case__61 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_38_case__61")(x)



c__case_37_case__62 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_out'39'46_'35lambda11(x4)))))(x6)(x1)(st))(st)
c__case_37_case__62 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_37_case__62(x1)(x)(st))(i)(xs)(st)
c__case_37_case__62 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_37_case__62")(x)



c__case_36_case__63 x1 x2 x3@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T3(x2)(x5)(x4))(st)
c__case_36_case__63 x1 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_36_case__63(x1)(x2)(x)(st))(i)(xs)(st)
c__case_36_case__63 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_36_case__63")(x)



c__case_35_case__64 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_inn'39'46_'35lambda12(x4)))))(x3)(x1)(st))(st)
c__case_35_case__64 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_35_case__64(x1)(x)(st))(i)(xs)(st)
c__case_35_case__64 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_35_case__64")(x)



c__case_34_case__65 x1 x2 x3@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T3(x5)(x2)(x4))(st)
c__case_34_case__65 x1 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_34_case__65(x1)(x2)(x)(st))(i)(xs)(st)
c__case_34_case__65 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_34_case__65")(x)



c__case_33_case__66 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_length(x6)(x1)(st))(st)
c__case_33_case__66 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_33_case__66(x1)(x)(st))(i)(xs)(st)
c__case_33_case__66 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_33_case__66")(x)



c__case_32_case__67 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_length(x3)(x1)(st))(st)
c__case_32_case__67 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_32_case__67(x1)(x)(st))(i)(xs)(st)
c__case_32_case__67 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_32_case__67")(x)



c__case_31_case__68 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = let {x7 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x7)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.op_43(Curry.Module.OraclePrelude.c_length(x3)(x1)(st))(Curry.Module.OraclePrelude.c_length(x6)(x7)(st))(x8)(st))(st)
c__case_31_case__68 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_31_case__68(x1)(x)(st))(i)(xs)(st)
c__case_31_case__68 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_31_case__68")(x)



c__case_30_case__69 x1 x2@(Curry.Module.OracleGraphInductive.C_Gr x3) st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_labNodes'46_'35lambda13))))(Curry.Module.OracleFiniteMap.c_fmToList(x3)(x1)(st))(x4)(st))(st)
c__case_30_case__69 x1 (Curry.Module.OracleGraphInductive.C_GraphOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_30_case__69(x1)(x)(st))(i)(xs)(st)
c__case_30_case__69 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_30_case__69")(x)



c__case_28_case__70 x1 x3 x4@(Curry.Module.Prelude.T3 x5 x6 x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(x3)(x6))(st)
c__case_28_case__70 x1 x3 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_28_case__70(x1)(x3)(x)(st))(i)(xs)(st)
c__case_28_case__70 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_28_case__70")(x)



c__case_29_case__71 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_28(x3)(x4)(x1)(st))(st)
c__case_29_case__71 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_29_case__71(x1)(x)(st))(i)(xs)(st)
c__case_29_case__71 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_29_case__71")(x)



c__case_27_case__72 x1 x2@(Curry.Module.OracleGraphInductive.C_Gr x3) st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.OraclePrelude.c_concatMap(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_labEdges'46_'35lambda14))))(x1)(st))(Curry.Module.OracleFiniteMap.c_fmToList(x3)(x4)(st))(x5)(st))(st)
c__case_27_case__72 x1 (Curry.Module.OracleGraphInductive.C_GraphOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_27_case__72(x1)(x)(st))(i)(xs)(st)
c__case_27_case__72 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_27_case__72")(x)



c__case_25_case__73 x1 x3 x4@(Curry.Module.Prelude.T3 x5 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_labEdges'46_'35lambda14'46_'35lambda15(x3)))))(x7)(x1)(st))(st)
c__case_25_case__73 x1 x3 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_25_case__73(x1)(x3)(x)(st))(i)(xs)(st)
c__case_25_case__73 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_25_case__73")(x)



c__case_26_case__74 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_25(x3)(x4)(x1)(st))(st)
c__case_26_case__74 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_26_case__74(x1)(x)(st))(i)(xs)(st)
c__case_26_case__74 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_26_case__74")(x)



c__case_24_case__75 x1 x2 x3@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T3(x2)(x5)(x4))(st)
c__case_24_case__75 x1 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_24_case__75(x1)(x2)(x)(st))(i)(xs)(st)
c__case_24_case__75 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_24_case__75")(x)



c__case_23_case__76 x1 x2@(Curry.Module.Prelude.T3 x3 x4 x5) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(x3)(x4))(st)
c__case_23_case__76 x1 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_23_case__76(x1)(x)(st))(i)(xs)(st)
c__case_23_case__76 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_23_case__76")(x)



c__case_22_case__77 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_22_case__77 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_22_case__77(x1)(x)(st))(i)(xs)(st)
c__case_22_case__77 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_22_case__77")(x)



c__case_20_case__78 x1 x2 x3 x6 x7 x8@Curry.Module.Prelude.C_True st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List)))(Curry.Module.Oracle.c_apply(Curry.Module.Oracle.c_apply(x2)(x6)(x1)(st))(Curry.Module.OracleGraphInductive.c_ufold(x2)(x3)(x7)(x9)(st))(x10)(st))(st)
c__case_20_case__78 x1 x2 x3 x6 x7 x8@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_20_case__78 x1 x2 x3 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_20_case__78(x1)(x2)(x3)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_20_case__78 x1 x2 x3 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_20_case__78")(x)



c__case_21_case__79 x1 x2 x3 x6 x7 x8@Curry.Module.Prelude.C_True st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_21_case__79 x1 x2 x3 x6 x7 x8@Curry.Module.Prelude.C_False st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OracleGraphInductive.c__case_20(x2)(x3)(x6)(x7)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x9)(st))(st)
c__case_21_case__79 x1 x2 x3 x6 x7 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_21_case__79(x1)(x2)(x3)(x6)(x7)(x)(st))(i)(xs)(st)
c__case_21_case__79 x1 x2 x3 x6 x7 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_21_case__79")(x)



c__case_19_case__80 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_19_case__80 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_19_case__80(x1)(x)(st))(i)(xs)(st)
c__case_19_case__80 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_19_case__80")(x)



c__case_18_case__81 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(x4)(st)
c__case_18_case__81 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_18_case__81(x1)(x)(st))(i)(xs)(st)
c__case_18_case__81 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_18_case__81")(x)



c__case_17_case__82 x1 x2 x3@(Curry.Module.Prelude.T4 x4 x5 x6 x7) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.T4(x4)(x5)(Curry.Module.Oracle.c_apply(x2)(x6)(x1)(st))(x7))(st)
c__case_17_case__82 x1 x2 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_17_case__82(x1)(x2)(x)(st))(i)(xs)(st)
c__case_17_case__82 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_17_case__82")(x)



c__case_16_case__83 x1 x2 x3@(Curry.Module.Prelude.T2 x4 x5) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.Prelude.T2(Curry.Module.Oracle.c_apply(x2)(x4)(x1)(st))(x5))(st)
c__case_16_case__83 x1 x2 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_16_case__83(x1)(x2)(x)(st))(i)(xs)(st)
c__case_16_case__83 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_16_case__83")(x)



c__case_15_case__84 x1 x2 x3@(Curry.Module.Prelude.T4 x4 x5 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)(Curry.Module.Prelude.List))))(Curry.Module.Prelude.T4(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.c_emap'46map1'46213(x2)(x1)(st))(x4)(x8)(st))(x5)(x6)(Curry.Module.Oracle.c_apply(Curry.Module.OracleGraphInductive.c_emap'46map1'46213(x2)(x9)(st))(x7)(x10)(st)))(st)
c__case_15_case__84 x1 x2 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_15_case__84(x1)(x2)(x)(st))(i)(xs)(st)
c__case_15_case__84 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_15_case__84")(x)



c__case_14_case__85 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T3(x3)(x4)(Curry.Module.Prelude.T0))(st)
c__case_14_case__85 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_14_case__85(x1)(x)(st))(i)(xs)(st)
c__case_14_case__85 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_14_case__85")(x)



c__case_13_case__86 x1 x2@(Curry.Module.OracleGraphInductive.C_Gr x3) st = let {x4 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x5 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x4)((Curry.Module.Prelude.:<)(x5)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_unlines(Curry.Module.OraclePrelude.c_map(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OracleGraphInductive.c_showNode))))(Curry.Module.OracleFiniteMap.c_fmToList(x3)(x1)(st))(x4)(st))(x5)(st))(st)
c__case_13_case__86 x1 (Curry.Module.OracleGraphInductive.C_GraphOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_13_case__86(x1)(x)(st))(i)(xs)(st)
c__case_13_case__86 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_13_case__86")(x)



c__case_11_case__87 x1 x3 x4@(Curry.Module.Prelude.T3 x5 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x13 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)((Curry.Module.Prelude.:<)(x9)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)((Curry.Module.Prelude.:<)(x12)((Curry.Module.Prelude.:<)(x13)(Curry.Module.Prelude.List)))))))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x3)(x1)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))(Curry.Module.Prelude.List))(Curry.Module.OraclePrelude.op_43_43(Curry.Module.OraclePrelude.c_show(x6)(x8)(st))(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('-'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('>'))(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_show(x7)(x9)(st))(x10)(st))(x11)(st))(x12)(st))(x13)(st))(st)
c__case_11_case__87 x1 x3 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_11_case__87(x1)(x3)(x)(st))(i)(xs)(st)
c__case_11_case__87 x1 x3 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_11_case__87")(x)



c__case_12_case__88 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_11(x3)(x4)(x1)(st))(st)
c__case_12_case__88 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_12_case__88(x1)(x)(st))(i)(xs)(st)
c__case_12_case__88 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_12_case__88")(x)



c__case_10_case__89 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x3)(st)
c__case_10_case__89 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_10_case__89(x1)(x)(st))(i)(xs)(st)
c__case_10_case__89 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_10_case__89")(x)



c__case_9_case__90 x1 x2@(Curry.Module.Prelude.T4 x3 x4 x5 x6) st = Curry.Module.CEventOracle.c_collapse(x1)(x6)(st)
c__case_9_case__90 x1 (Curry.Module.Prelude.T4Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_9_case__90(x1)(x)(st))(i)(xs)(st)
c__case_9_case__90 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_9_case__90")(x)



c__case_8_case__91 x1 x2@(Curry.Module.Prelude.T2 x3 x4) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T2(x4)(x3))(st)
c__case_8_case__91 x1 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_8_case__91(x1)(x)(st))(i)(xs)(st)
c__case_8_case__91 x1 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_8_case__91")(x)



c__case_7_case__92 x1 x2 x3 x4@(Curry.Module.Prelude.T3 x5 x6 x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T3(x5)(x6)((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(x3)(x2))(x7)))(st)
c__case_7_case__92 x1 x2 x3 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_7_case__92(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_7_case__92 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_7_case__92")(x)



c__case_6_case__93 x1 x2 x3 x4@(Curry.Module.Prelude.T3 x5 x6 x7) st = Curry.Module.CEventOracle.c_collapse(x1)(Curry.Module.Prelude.T3((Curry.Module.Prelude.:<)(Curry.Module.Prelude.T2(x3)(x2))(x5))(x6)(x7))(st)
c__case_6_case__93 x1 x2 x3 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_6_case__93(x1)(x2)(x3)(x)(st))(i)(xs)(st)
c__case_6_case__93 x1 x2 x3 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_6_case__93")(x)



c__case_5_case__94 x1 x2 x4@(Curry.Module.Prelude.T3 x5 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.Prelude.T3(x5)(x6)(Curry.Module.OraclePrelude.c_filter(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_47_61))(st))(x2)))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_snd))))(x1)(st))(x7)(x8)(st)))(st)
c__case_5_case__94 x1 x2 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_5_case__94(x1)(x2)(x)(st))(i)(xs)(st)
c__case_5_case__94 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_5_case__94")(x)



c__case_4_case__95 x1 x2 x4@(Curry.Module.Prelude.T3 x5 x6 x7) st = let {x8 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x8)(Curry.Module.Prelude.List))(Curry.Module.Prelude.T3(Curry.Module.OraclePrelude.c_filter(Curry.Module.OraclePrelude.op_46(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_flip(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partCall))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_compose(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.Oracle.c_partFunc))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa))(Curry.Module.Prelude.cp(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))))(Curry.Module.OraclePrelude.op_47_61))(st))(x2)))))(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.Oracle.c_partFunc(Curry.Module.Prelude.cp(Curry.Module.Prelude.pa)(Curry.Module.Prelude.cp(Curry.Module.Prelude.pf))(Curry.Module.OraclePrelude.c_snd))))(x1)(st))(x5)(x8)(st))(x6)(x7))(st)
c__case_4_case__95 x1 x2 (Curry.Module.Prelude.T3Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_4_case__95(x1)(x2)(x)(st))(i)(xs)(st)
c__case_4_case__95 x1 x2 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_4_case__95")(x)



c__case_0_case__96 x1 x8 x9@Curry.Module.Prelude.C_True st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))(Curry.Module.OraclePrelude.c_error(Curry.Module.OraclePrelude.op_43_43((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('g'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('E'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('x'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('c'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('p'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('t'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('i'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('n'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(','))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('N'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('o'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('d'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char('e'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(':'))((Curry.Module.Prelude.:<)(Curry.Module.Prelude.C_Char(' '))(Curry.Module.Prelude.List)))))))))))))))))))))))(Curry.Module.OraclePrelude.c_show(x8)(x1)(st))(x10)(st))(x11)(st))(st)
c__case_0_case__96 x1 x8 x9@Curry.Module.Prelude.C_False st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OraclePrelude.c_failed(x1)(st))(st)
c__case_0_case__96 x1 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_0_case__96(x1)(x8)(x)(st))(i)(xs)(st)
c__case_0_case__96 x1 x8 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_0_case__96")(x)



c__case_1_case__97 x1 x2 x4 x6 x7 x8 x9@Curry.Module.Prelude.C_True st = let {x10 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st); x11 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x10)((Curry.Module.Prelude.:<)(x11)(Curry.Module.Prelude.List)))(Curry.Module.OracleGraphInductive.c_updAdj(Curry.Module.OracleFiniteMap.c_updFM(x2)(x8)(Curry.Module.Oracle.c_apply(x4)(x7)(x1)(st))(x10)(st))(x6)(x4)(x11)(st))(st)
c__case_1_case__97 x1 x2 x4 x6 x7 x8 x9@Curry.Module.Prelude.C_False st = let {x12 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x12)(Curry.Module.Prelude.List))(Curry.Module.OracleGraphInductive.c__case_0(x8)(Curry.Module.OraclePrelude.c_otherwise(x1)(st))(x12)(st))(st)
c__case_1_case__97 x1 x2 x4 x6 x7 x8 (Curry.Module.Prelude.C_BoolOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_1_case__97(x1)(x2)(x4)(x6)(x7)(x8)(x)(st))(i)(xs)(st)
c__case_1_case__97 x1 x2 x4 x6 x7 x8 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_1_case__97")(x)



c__case_2_case__98 x1 x2 x4 x6 x5@(Curry.Module.Prelude.T2 x7 x8) st = let {x9 = Curry.Module.CEventOracle.c_fresh(Curry.Module.Prelude.T0)(st)} in Curry.Module.CEventOracle.c_expand(x1)((Curry.Module.Prelude.:<)(x9)(Curry.Module.Prelude.List))(Curry.Module.OracleGraphInductive.c__case_1(x2)(x4)(x6)(x7)(x8)(Curry.Module.OracleFiniteMap.c_elemFM(x8)(x2)(x1)(st))(x9)(st))(st)
c__case_2_case__98 x1 x2 x4 x6 (Curry.Module.Prelude.T2Or i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_2_case__98(x1)(x2)(x4)(x6)(x)(st))(i)(xs)(st)
c__case_2_case__98 x1 x2 x4 x6 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_2_case__98")(x)



c__case_3_case__99 x1 x2 x4 x3@Curry.Module.Prelude.List st = Curry.Module.CEventOracle.c_collapse(x1)(x2)(st)
c__case_3_case__99 x1 x2 x4 x3@((Curry.Module.Prelude.:<) x5 x6) st = Curry.Module.CEventOracle.c_replace(x1)(Curry.Module.OracleGraphInductive.c__case_2(x2)(x4)(x6)(x5)(x1)(st))(st)
c__case_3_case__99 x1 x2 x4 (Curry.Module.Prelude.ListOr i xs) st = Curry.RunTimeSystem.mapOr(\ x st -> Curry.Module.OracleGraphInductive.c__case_3_case__99(x1)(x2)(x4)(x)(st))(i)(xs)(st)
c__case_3_case__99 x1 x2 x4 x st = Curry.RunTimeSystem.patternFail("OracleGraphInductive._case_3_case__99")(x)


