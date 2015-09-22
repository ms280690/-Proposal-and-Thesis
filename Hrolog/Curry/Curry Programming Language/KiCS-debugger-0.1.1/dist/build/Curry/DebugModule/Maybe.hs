{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Curry.DebugModule.Maybe where
import qualified Prelude
import qualified Curry.Debugger.DebugMonad as DM
import qualified Curry.Debugger.DebugInfo as DI
import qualified Curry.Debugger.PartCalls as PC
import qualified Data.Generics
import qualified Curry.DebugModule.Prelude
 
strict_isJust ::
              (DM.DM dm, DI.GenTerm a) =>
                Curry.DebugModule.Prelude.Maybe a ->
                  dm Curry.DebugModule.Prelude.Bool
strict_isJust x1
  = DM.eval
      (DM.funcDeclHook "isJust"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_8"
               (DI.DebugInfo (DI.SrcID "Maybe" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_8 x2)))
term_strict_isJust x1 = DI.Term "isJust" (DI.SrcID "Maybe" 0) x1
 
strict_isNothing ::
                 (DM.DM dm, DI.GenTerm a) =>
                   Curry.DebugModule.Prelude.Maybe a ->
                     dm Curry.DebugModule.Prelude.Bool
strict_isNothing x1
  = DM.eval
      (DM.funcDeclHook "isNothing"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_7"
               (DI.DebugInfo (DI.SrcID "Maybe" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_7 x2)))
term_strict_isNothing x1
  = DI.Term "isNothing" (DI.SrcID "Maybe" 0) x1
 
strict_fromJust ::
                (DM.DM dm, DI.GenTerm a) =>
                  Curry.DebugModule.Prelude.Maybe a -> dm a
strict_fromJust x1
  = DM.eval
      (DM.funcDeclHook "fromJust"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_6"
               (DI.DebugInfo (DI.SrcID "Maybe" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_6 x2)))
term_strict_fromJust x1
  = DI.Term "fromJust" (DI.SrcID "Maybe" 0) x1
 
strict_fromMaybe ::
                 (DM.DM dm, DI.GenTerm a) =>
                   a -> Curry.DebugModule.Prelude.Maybe a -> dm a
strict_fromMaybe x1 x2
  = DM.eval
      (DM.funcDeclHook "fromMaybe"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_5"
               (DI.DebugInfo (DI.SrcID "Maybe" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_5 x3 x4)))
term_strict_fromMaybe x1
  = DI.Term "fromMaybe" (DI.SrcID "Maybe" 0) x1
 
strict_maybeToList ::
                   (DM.DM dm, DI.GenTerm a) =>
                     Curry.DebugModule.Prelude.Maybe a ->
                       dm (Curry.DebugModule.Prelude.List a)
strict_maybeToList x1
  = DM.eval
      (DM.funcDeclHook "maybeToList"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_4"
               (DI.DebugInfo (DI.SrcID "Maybe" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_4 x2)))
term_strict_maybeToList x1
  = DI.Term "maybeToList" (DI.SrcID "Maybe" 0) x1
 
strict_listToMaybe ::
                   (DM.DM dm, DI.GenTerm a) =>
                     Curry.DebugModule.Prelude.List a ->
                       dm (Curry.DebugModule.Prelude.Maybe a)
strict_listToMaybe x1
  = DM.eval
      (DM.funcDeclHook "listToMaybe"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_3"
               (DI.DebugInfo (DI.SrcID "Maybe" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_3 x2)))
term_strict_listToMaybe x1
  = DI.Term "listToMaybe" (DI.SrcID "Maybe" 0) x1
 
strict_catMaybes ::
                 (DM.DM dm, DI.GenTerm a) =>
                   Curry.DebugModule.Prelude.List (Curry.DebugModule.Prelude.Maybe a)
                     -> dm (Curry.DebugModule.Prelude.List a)
strict_catMaybes x1
  = DM.eval
      (DM.funcDeclHook "catMaybes"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall2 (x'xterm_strict_catMaybes46_35lambda4 [])
                        x'xstrict_catMaybes46_35lambda4)
             x3 <- DM.constructorHook
                     (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                     (Prelude.return Curry.DebugModule.Prelude.Nil)
             x4 <- Prelude.return x1
             DM.funcCallHook "foldr"
               (DI.DebugInfo (DI.SrcID "Maybe" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3, DI.genTerm x4]))
               (Curry.DebugModule.Prelude.strict_foldr x2 x3 x4)))
term_strict_catMaybes x1
  = DI.Term "catMaybes" (DI.SrcID "Maybe" 0) x1
 
x'xstrict_catMaybes46_35lambda4 ::
                                (DM.DM dm, DI.GenTerm x37) =>
                                  Curry.DebugModule.Prelude.Maybe x37 ->
                                    Curry.DebugModule.Prelude.List x37 ->
                                      dm (Curry.DebugModule.Prelude.List x37)
x'xstrict_catMaybes46_35lambda4 x1 x2
  = DM.eval
      (DM.funcDeclHook "catMaybes._#lambda4"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_2"
               (DI.DebugInfo (DI.SrcID "Maybe" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_2 x3 x4)))
x'xterm_strict_catMaybes46_35lambda4 x1
  = DI.Term "catMaybes._#lambda4" (DI.SrcID "Maybe" 0) x1
 
strict_mapMaybe ::
                (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
                  DM.Func dm a (Curry.DebugModule.Prelude.Maybe b) ->
                    dm
                      (DM.Func dm (Curry.DebugModule.Prelude.List a)
                         (Curry.DebugModule.Prelude.List b))
strict_mapMaybe x1
  = DM.eval
      (DM.funcDeclHook "mapMaybe"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- Prelude.return
                     (PC.partCall1 (term_strict_catMaybes []) strict_catMaybes)
             x4 <- do x2 <- Prelude.return x1
                      Prelude.return
                        (PC.partCall1
                           (Curry.DebugModule.Prelude.term_strict_map [DI.genTerm x2])
                           (Curry.DebugModule.Prelude.strict_map x2))
             DM.funcCallHook "."
               (DI.DebugInfo (DI.SrcID "Maybe" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (Curry.DebugModule.Prelude.op_Point x3 x4)))
term_strict_mapMaybe x1
  = DI.Term "mapMaybe" (DI.SrcID "Maybe" 0) x1
 
op_GtGtMinus ::
             (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
               Curry.DebugModule.Prelude.Maybe a ->
                 DM.Func dm a (Curry.DebugModule.Prelude.Maybe b) ->
                   dm (Curry.DebugModule.Prelude.Maybe b)
op_GtGtMinus x1 x2
  = DM.eval
      (DM.funcDeclHook ">>-"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_1"
               (DI.DebugInfo (DI.SrcID "Maybe" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_1 x3 x4)))
term_op_GtGtMinus x1 = DI.Term ">>-" (DI.SrcID "Maybe" 0) x1
 
strict_sequenceMaybe ::
                     (DM.DM dm, DI.GenTerm a) =>
                       Curry.DebugModule.Prelude.List (Curry.DebugModule.Prelude.Maybe a)
                         ->
                         dm
                           (Curry.DebugModule.Prelude.Maybe
                              (Curry.DebugModule.Prelude.List a))
strict_sequenceMaybe x1
  = DM.eval
      (DM.funcDeclHook "sequenceMaybe"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_0"
               (DI.DebugInfo (DI.SrcID "Maybe" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_0 x2)))
term_strict_sequenceMaybe x1
  = DI.Term "sequenceMaybe" (DI.SrcID "Maybe" 0) x1
 
x'xstrict_sequenceMaybe46_35lambda6 ::
                                    (DM.DM dm, DI.GenTerm x66) =>
                                      Curry.DebugModule.Prelude.List
                                        (Curry.DebugModule.Prelude.Maybe x66)
                                        ->
                                        x66 ->
                                          dm
                                            (Curry.DebugModule.Prelude.Maybe
                                               (Curry.DebugModule.Prelude.List x66))
x'xstrict_sequenceMaybe46_35lambda6 x1 x2
  = DM.eval
      (DM.funcDeclHook "sequenceMaybe._#lambda6"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return x1
                      DM.funcCallHook "sequenceMaybe"
                        (DI.DebugInfo (DI.SrcID "Maybe" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3]))
                        (strict_sequenceMaybe x3)
             x6 <- do x4 <- Prelude.return x2
                      Prelude.return
                        (PC.partCall1
                           (x'xterm_strict_sequenceMaybe46_35lambda646_35lambda7
                              [DI.genTerm x4])
                           (x'xstrict_sequenceMaybe46_35lambda646_35lambda7 x4))
             DM.funcCallHook ">>-"
               (DI.DebugInfo (DI.SrcID "Maybe" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (op_GtGtMinus x5 x6)))
x'xterm_strict_sequenceMaybe46_35lambda6 x1
  = DI.Term "sequenceMaybe._#lambda6" (DI.SrcID "Maybe" 0) x1
 
x'xstrict_sequenceMaybe46_35lambda646_35lambda7 ::
                                                (DM.DM dm, DI.GenTerm x66) =>
                                                  x66 ->
                                                    Curry.DebugModule.Prelude.List x66 ->
                                                      dm
                                                        (Curry.DebugModule.Prelude.Maybe
                                                           (Curry.DebugModule.Prelude.List x66))
x'xstrict_sequenceMaybe46_35lambda646_35lambda7 x1 x2
  = DM.eval
      (DM.funcDeclHook "sequenceMaybe._#lambda6._#lambda7"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.constructorHook
                        (DI.DebugInfo (DI.SrcID "Maybe" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Prelude.return (Curry.DebugModule.Prelude.Cons x3 x4))
             DM.constructorHook
               (DI.DebugInfo (DI.SrcID "Maybe" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5]))
               (Prelude.return (Curry.DebugModule.Prelude.Just x5))))
x'xterm_strict_sequenceMaybe46_35lambda646_35lambda7 x1
  = DI.Term "sequenceMaybe._#lambda6._#lambda7" (DI.SrcID "Maybe" 0)
      x1
 
strict_mapMMaybe ::
                 (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
                   DM.Func dm a (Curry.DebugModule.Prelude.Maybe b) ->
                     dm
                       (DM.Func dm (Curry.DebugModule.Prelude.List a)
                          (Curry.DebugModule.Prelude.Maybe
                             (Curry.DebugModule.Prelude.List b)))
strict_mapMMaybe x1
  = DM.eval
      (DM.funcDeclHook "mapMMaybe"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- Prelude.return
                     (PC.partCall1 (term_strict_sequenceMaybe []) strict_sequenceMaybe)
             x4 <- do x2 <- Prelude.return x1
                      Prelude.return
                        (PC.partCall1
                           (Curry.DebugModule.Prelude.term_strict_map [DI.genTerm x2])
                           (Curry.DebugModule.Prelude.strict_map x2))
             DM.funcCallHook "."
               (DI.DebugInfo (DI.SrcID "Maybe" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (Curry.DebugModule.Prelude.op_Point x3 x4)))
term_strict_mapMMaybe x1
  = DI.Term "mapMMaybe" (DI.SrcID "Maybe" 0) x1
strict__case_0 x1
  = DM.eval
      (DM.funcDeclHook "_case_0"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x8 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Maybe" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                                          (Prelude.return Curry.DebugModule.Prelude.Nil)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Maybe" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Just x4))))
                    Curry.DebugModule.Prelude.Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x2
                                  x7 <- do x5 <- Prelude.return x3
                                           Prelude.return
                                             (PC.partCall1
                                                (x'xterm_strict_sequenceMaybe46_35lambda6
                                                   [DI.genTerm x5])
                                                (x'xstrict_sequenceMaybe46_35lambda6 x5))
                                  DM.funcCallHook ">>-"
                                    (DI.DebugInfo (DI.SrcID "Maybe" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (op_GtGtMinus x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Maybe" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           strict__case_0
                           x8)))
term_strict__case_0 x1 = DI.Term "_case_0" (DI.SrcID "Maybe" 0) x1
strict__case_1 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_1"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x6 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Maybe" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x6]))
               (case x6 of
                    Curry.DebugModule.Prelude.Nothing
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Nothing)))
                    Curry.DebugModule.Prelude.Just x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- Prelude.return x2
                                  x5 <- Prelude.return x3
                                  DM.funcCallHook "apply"
                                    (DI.DebugInfo (DI.SrcID "Maybe" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                    (Curry.DebugModule.Prelude.strict_apply x4 x5)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Maybe" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x6])))
                           (strict__case_1 x2)
                           x6)))
term_strict__case_1 x1 = DI.Term "_case_1" (DI.SrcID "Maybe" 0) x1
strict__case_2 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_2"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x6 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Maybe" 0)
                  (DI.DynamicInfo [] [DI.genTerm x6]))
               (case x6 of
                    Curry.DebugModule.Prelude.Just x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- Prelude.return x3
                                  x5 <- Prelude.return x2
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Maybe" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x4 x5))))
                    Curry.DebugModule.Prelude.Nothing
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Maybe" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x6])))
                           (strict__case_2 x2)
                           x6)))
term_strict__case_2 x1 = DI.Term "_case_2" (DI.SrcID "Maybe" 0) x1
strict__case_3 x1
  = DM.eval
      (DM.funcDeclHook "_case_3"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x5 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Maybe" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x5]))
               (case x5 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Nothing)))
                    Curry.DebugModule.Prelude.Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- Prelude.return x2
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Maybe" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Just x4))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Maybe" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x5])))
                           strict__case_3
                           x5)))
term_strict__case_3 x1 = DI.Term "_case_3" (DI.SrcID "Maybe" 0) x1
strict__case_4 x1
  = DM.eval
      (DM.funcDeclHook "_case_4"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x5 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Maybe" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x5]))
               (case x5 of
                    Curry.DebugModule.Prelude.Nothing
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Nil)))
                    Curry.DebugModule.Prelude.Just x2
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x3 <- Prelude.return x2
                                  x4 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                                          (Prelude.return Curry.DebugModule.Prelude.Nil)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Maybe" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x3 x4))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Maybe" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x5])))
                           strict__case_4
                           x5)))
term_strict__case_4 x1 = DI.Term "_case_4" (DI.SrcID "Maybe" 0) x1
strict__case_5 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_5"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x4 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Maybe" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.Nothing
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x1))
                    Curry.DebugModule.Prelude.Just x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Maybe" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           (strict__case_5 x1)
                           x4)))
term_strict__case_5 x1 = DI.Term "_case_5" (DI.SrcID "Maybe" 0) x1
strict__case_6 x1
  = DM.eval
      (DM.funcDeclHook "_case_6"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x50 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Maybe" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x50]))
               (case x50 of
                    Curry.DebugModule.Prelude.Just x2
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    Curry.DebugModule.Prelude.Nothing
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x49 <- do x47 <- DM.litHook
                                                     (DI.DebugInfo (DI.SrcID "Maybe" 0)
                                                        (DI.DynamicInfo [] []))
                                                     (Prelude.return
                                                        (Curry.DebugModule.Prelude.Char 'M'))
                                            x48 <- do x45 <- DM.litHook
                                                               (DI.DebugInfo (DI.SrcID "Maybe" 0)
                                                                  (DI.DynamicInfo [] []))
                                                               (Prelude.return
                                                                  (Curry.DebugModule.Prelude.Char
                                                                     'a'))
                                                      x46 <- do x43 <- DM.litHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Maybe" 0)
                                                                            (DI.DynamicInfo [] []))
                                                                         (Prelude.return
                                                                            (Curry.DebugModule.Prelude.Char
                                                                               'y'))
                                                                x44 <- do x41 <- DM.litHook
                                                                                   (DI.DebugInfo
                                                                                      (DI.SrcID
                                                                                         "Maybe"
                                                                                         0)
                                                                                      (DI.DynamicInfo
                                                                                         []
                                                                                         []))
                                                                                   (Prelude.return
                                                                                      (Curry.DebugModule.Prelude.Char
                                                                                         'b'))
                                                                          x42 <- do x39 <- DM.litHook
                                                                                             (DI.DebugInfo
                                                                                                (DI.SrcID
                                                                                                   "Maybe"
                                                                                                   0)
                                                                                                (DI.DynamicInfo
                                                                                                   []
                                                                                                   []))
                                                                                             (Prelude.return
                                                                                                (Curry.DebugModule.Prelude.Char
                                                                                                   'e'))
                                                                                    x40 <- do x37 <- DM.litHook
                                                                                                       (DI.DebugInfo
                                                                                                          (DI.SrcID
                                                                                                             "Maybe"
                                                                                                             0)
                                                                                                          (DI.DynamicInfo
                                                                                                             []
                                                                                                             []))
                                                                                                       (Prelude.return
                                                                                                          (Curry.DebugModule.Prelude.Char
                                                                                                             '.'))
                                                                                              x38 <- do x35 <- DM.litHook
                                                                                                                 (DI.DebugInfo
                                                                                                                    (DI.SrcID
                                                                                                                       "Maybe"
                                                                                                                       0)
                                                                                                                    (DI.DynamicInfo
                                                                                                                       []
                                                                                                                       []))
                                                                                                                 (Prelude.return
                                                                                                                    (Curry.DebugModule.Prelude.Char
                                                                                                                       'f'))
                                                                                                        x36 <- do x33 <- DM.litHook
                                                                                                                           (DI.DebugInfo
                                                                                                                              (DI.SrcID
                                                                                                                                 "Maybe"
                                                                                                                                 0)
                                                                                                                              (DI.DynamicInfo
                                                                                                                                 []
                                                                                                                                 []))
                                                                                                                           (Prelude.return
                                                                                                                              (Curry.DebugModule.Prelude.Char
                                                                                                                                 'r'))
                                                                                                                  x34 <- do x31 <- DM.litHook
                                                                                                                                     (DI.DebugInfo
                                                                                                                                        (DI.SrcID
                                                                                                                                           "Maybe"
                                                                                                                                           0)
                                                                                                                                        (DI.DynamicInfo
                                                                                                                                           []
                                                                                                                                           []))
                                                                                                                                     (Prelude.return
                                                                                                                                        (Curry.DebugModule.Prelude.Char
                                                                                                                                           'o'))
                                                                                                                            x32 <- do x29 <- DM.litHook
                                                                                                                                               (DI.DebugInfo
                                                                                                                                                  (DI.SrcID
                                                                                                                                                     "Maybe"
                                                                                                                                                     0)
                                                                                                                                                  (DI.DynamicInfo
                                                                                                                                                     []
                                                                                                                                                     []))
                                                                                                                                               (Prelude.return
                                                                                                                                                  (Curry.DebugModule.Prelude.Char
                                                                                                                                                     'm'))
                                                                                                                                      x30 <- do x27 <- DM.litHook
                                                                                                                                                         (DI.DebugInfo
                                                                                                                                                            (DI.SrcID
                                                                                                                                                               "Maybe"
                                                                                                                                                               0)
                                                                                                                                                            (DI.DynamicInfo
                                                                                                                                                               []
                                                                                                                                                               []))
                                                                                                                                                         (Prelude.return
                                                                                                                                                            (Curry.DebugModule.Prelude.Char
                                                                                                                                                               'J'))
                                                                                                                                                x28 <- do x25 <- DM.litHook
                                                                                                                                                                   (DI.DebugInfo
                                                                                                                                                                      (DI.SrcID
                                                                                                                                                                         "Maybe"
                                                                                                                                                                         0)
                                                                                                                                                                      (DI.DynamicInfo
                                                                                                                                                                         []
                                                                                                                                                                         []))
                                                                                                                                                                   (Prelude.return
                                                                                                                                                                      (Curry.DebugModule.Prelude.Char
                                                                                                                                                                         'u'))
                                                                                                                                                          x26 <- do x23 <- DM.litHook
                                                                                                                                                                             (DI.DebugInfo
                                                                                                                                                                                (DI.SrcID
                                                                                                                                                                                   "Maybe"
                                                                                                                                                                                   0)
                                                                                                                                                                                (DI.DynamicInfo
                                                                                                                                                                                   []
                                                                                                                                                                                   []))
                                                                                                                                                                             (Prelude.return
                                                                                                                                                                                (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                   's'))
                                                                                                                                                                    x24 <- do x21 <- DM.litHook
                                                                                                                                                                                       (DI.DebugInfo
                                                                                                                                                                                          (DI.SrcID
                                                                                                                                                                                             "Maybe"
                                                                                                                                                                                             0)
                                                                                                                                                                                          (DI.DynamicInfo
                                                                                                                                                                                             []
                                                                                                                                                                                             []))
                                                                                                                                                                                       (Prelude.return
                                                                                                                                                                                          (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                             't'))
                                                                                                                                                                              x22 <- do x19 <- DM.litHook
                                                                                                                                                                                                 (DI.DebugInfo
                                                                                                                                                                                                    (DI.SrcID
                                                                                                                                                                                                       "Maybe"
                                                                                                                                                                                                       0)
                                                                                                                                                                                                    (DI.DynamicInfo
                                                                                                                                                                                                       []
                                                                                                                                                                                                       []))
                                                                                                                                                                                                 (Prelude.return
                                                                                                                                                                                                    (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                       ':'))
                                                                                                                                                                                        x20 <- do x17 <- DM.litHook
                                                                                                                                                                                                           (DI.DebugInfo
                                                                                                                                                                                                              (DI.SrcID
                                                                                                                                                                                                                 "Maybe"
                                                                                                                                                                                                                 0)
                                                                                                                                                                                                              (DI.DynamicInfo
                                                                                                                                                                                                                 []
                                                                                                                                                                                                                 []))
                                                                                                                                                                                                           (Prelude.return
                                                                                                                                                                                                              (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                 ' '))
                                                                                                                                                                                                  x18 <- do x15 <- DM.litHook
                                                                                                                                                                                                                     (DI.DebugInfo
                                                                                                                                                                                                                        (DI.SrcID
                                                                                                                                                                                                                           "Maybe"
                                                                                                                                                                                                                           0)
                                                                                                                                                                                                                        (DI.DynamicInfo
                                                                                                                                                                                                                           []
                                                                                                                                                                                                                           []))
                                                                                                                                                                                                                     (Prelude.return
                                                                                                                                                                                                                        (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                           'N'))
                                                                                                                                                                                                            x16 <- do x13 <- DM.litHook
                                                                                                                                                                                                                               (DI.DebugInfo
                                                                                                                                                                                                                                  (DI.SrcID
                                                                                                                                                                                                                                     "Maybe"
                                                                                                                                                                                                                                     0)
                                                                                                                                                                                                                                  (DI.DynamicInfo
                                                                                                                                                                                                                                     []
                                                                                                                                                                                                                                     []))
                                                                                                                                                                                                                               (Prelude.return
                                                                                                                                                                                                                                  (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                     'o'))
                                                                                                                                                                                                                      x14 <- do x11 <- DM.litHook
                                                                                                                                                                                                                                         (DI.DebugInfo
                                                                                                                                                                                                                                            (DI.SrcID
                                                                                                                                                                                                                                               "Maybe"
                                                                                                                                                                                                                                               0)
                                                                                                                                                                                                                                            (DI.DynamicInfo
                                                                                                                                                                                                                                               []
                                                                                                                                                                                                                                               []))
                                                                                                                                                                                                                                         (Prelude.return
                                                                                                                                                                                                                                            (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                               't'))
                                                                                                                                                                                                                                x12 <- do x9 <- DM.litHook
                                                                                                                                                                                                                                                  (DI.DebugInfo
                                                                                                                                                                                                                                                     (DI.SrcID
                                                                                                                                                                                                                                                        "Maybe"
                                                                                                                                                                                                                                                        0)
                                                                                                                                                                                                                                                     (DI.DynamicInfo
                                                                                                                                                                                                                                                        []
                                                                                                                                                                                                                                                        []))
                                                                                                                                                                                                                                                  (Prelude.return
                                                                                                                                                                                                                                                     (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                        'h'))
                                                                                                                                                                                                                                          x10 <- do x7 <- DM.litHook
                                                                                                                                                                                                                                                            (DI.DebugInfo
                                                                                                                                                                                                                                                               (DI.SrcID
                                                                                                                                                                                                                                                                  "Maybe"
                                                                                                                                                                                                                                                                  0)
                                                                                                                                                                                                                                                               (DI.DynamicInfo
                                                                                                                                                                                                                                                                  []
                                                                                                                                                                                                                                                                  []))
                                                                                                                                                                                                                                                            (Prelude.return
                                                                                                                                                                                                                                                               (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                  'i'))
                                                                                                                                                                                                                                                    x8 <- do x5 <- DM.litHook
                                                                                                                                                                                                                                                                     (DI.DebugInfo
                                                                                                                                                                                                                                                                        (DI.SrcID
                                                                                                                                                                                                                                                                           "Maybe"
                                                                                                                                                                                                                                                                           0)
                                                                                                                                                                                                                                                                        (DI.DynamicInfo
                                                                                                                                                                                                                                                                           []
                                                                                                                                                                                                                                                                           []))
                                                                                                                                                                                                                                                                     (Prelude.return
                                                                                                                                                                                                                                                                        (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                           'n'))
                                                                                                                                                                                                                                                             x6 <- do x3 <- DM.litHook
                                                                                                                                                                                                                                                                              (DI.DebugInfo
                                                                                                                                                                                                                                                                                 (DI.SrcID
                                                                                                                                                                                                                                                                                    "Maybe"
                                                                                                                                                                                                                                                                                    0)
                                                                                                                                                                                                                                                                                 (DI.DynamicInfo
                                                                                                                                                                                                                                                                                    []
                                                                                                                                                                                                                                                                                    []))
                                                                                                                                                                                                                                                                              (Prelude.return
                                                                                                                                                                                                                                                                                 (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                    'g'))
                                                                                                                                                                                                                                                                      x4 <- DM.constructorHook
                                                                                                                                                                                                                                                                              (DI.DebugInfo
                                                                                                                                                                                                                                                                                 (DI.SrcID
                                                                                                                                                                                                                                                                                    "Maybe"
                                                                                                                                                                                                                                                                                    0)
                                                                                                                                                                                                                                                                                 (DI.DynamicInfo
                                                                                                                                                                                                                                                                                    []
                                                                                                                                                                                                                                                                                    []))
                                                                                                                                                                                                                                                                              (Prelude.return
                                                                                                                                                                                                                                                                                 Curry.DebugModule.Prelude.Nil)
                                                                                                                                                                                                                                                                      DM.constructorHook
                                                                                                                                                                                                                                                                        (DI.DebugInfo
                                                                                                                                                                                                                                                                           (DI.SrcID
                                                                                                                                                                                                                                                                              "Maybe"
                                                                                                                                                                                                                                                                              0)
                                                                                                                                                                                                                                                                           (DI.DynamicInfo
                                                                                                                                                                                                                                                                              []
                                                                                                                                                                                                                                                                              [DI.genTerm
                                                                                                                                                                                                                                                                                 x3,
                                                                                                                                                                                                                                                                               DI.genTerm
                                                                                                                                                                                                                                                                                 x4]))
                                                                                                                                                                                                                                                                        (Prelude.return
                                                                                                                                                                                                                                                                           (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                                              x3
                                                                                                                                                                                                                                                                              x4))
                                                                                                                                                                                                                                                             DM.constructorHook
                                                                                                                                                                                                                                                               (DI.DebugInfo
                                                                                                                                                                                                                                                                  (DI.SrcID
                                                                                                                                                                                                                                                                     "Maybe"
                                                                                                                                                                                                                                                                     0)
                                                                                                                                                                                                                                                                  (DI.DynamicInfo
                                                                                                                                                                                                                                                                     []
                                                                                                                                                                                                                                                                     [DI.genTerm
                                                                                                                                                                                                                                                                        x5,
                                                                                                                                                                                                                                                                      DI.genTerm
                                                                                                                                                                                                                                                                        x6]))
                                                                                                                                                                                                                                                               (Prelude.return
                                                                                                                                                                                                                                                                  (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                                     x5
                                                                                                                                                                                                                                                                     x6))
                                                                                                                                                                                                                                                    DM.constructorHook
                                                                                                                                                                                                                                                      (DI.DebugInfo
                                                                                                                                                                                                                                                         (DI.SrcID
                                                                                                                                                                                                                                                            "Maybe"
                                                                                                                                                                                                                                                            0)
                                                                                                                                                                                                                                                         (DI.DynamicInfo
                                                                                                                                                                                                                                                            []
                                                                                                                                                                                                                                                            [DI.genTerm
                                                                                                                                                                                                                                                               x7,
                                                                                                                                                                                                                                                             DI.genTerm
                                                                                                                                                                                                                                                               x8]))
                                                                                                                                                                                                                                                      (Prelude.return
                                                                                                                                                                                                                                                         (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                            x7
                                                                                                                                                                                                                                                            x8))
                                                                                                                                                                                                                                          DM.constructorHook
                                                                                                                                                                                                                                            (DI.DebugInfo
                                                                                                                                                                                                                                               (DI.SrcID
                                                                                                                                                                                                                                                  "Maybe"
                                                                                                                                                                                                                                                  0)
                                                                                                                                                                                                                                               (DI.DynamicInfo
                                                                                                                                                                                                                                                  []
                                                                                                                                                                                                                                                  [DI.genTerm
                                                                                                                                                                                                                                                     x9,
                                                                                                                                                                                                                                                   DI.genTerm
                                                                                                                                                                                                                                                     x10]))
                                                                                                                                                                                                                                            (Prelude.return
                                                                                                                                                                                                                                               (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                  x9
                                                                                                                                                                                                                                                  x10))
                                                                                                                                                                                                                                DM.constructorHook
                                                                                                                                                                                                                                  (DI.DebugInfo
                                                                                                                                                                                                                                     (DI.SrcID
                                                                                                                                                                                                                                        "Maybe"
                                                                                                                                                                                                                                        0)
                                                                                                                                                                                                                                     (DI.DynamicInfo
                                                                                                                                                                                                                                        []
                                                                                                                                                                                                                                        [DI.genTerm
                                                                                                                                                                                                                                           x11,
                                                                                                                                                                                                                                         DI.genTerm
                                                                                                                                                                                                                                           x12]))
                                                                                                                                                                                                                                  (Prelude.return
                                                                                                                                                                                                                                     (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                        x11
                                                                                                                                                                                                                                        x12))
                                                                                                                                                                                                                      DM.constructorHook
                                                                                                                                                                                                                        (DI.DebugInfo
                                                                                                                                                                                                                           (DI.SrcID
                                                                                                                                                                                                                              "Maybe"
                                                                                                                                                                                                                              0)
                                                                                                                                                                                                                           (DI.DynamicInfo
                                                                                                                                                                                                                              []
                                                                                                                                                                                                                              [DI.genTerm
                                                                                                                                                                                                                                 x13,
                                                                                                                                                                                                                               DI.genTerm
                                                                                                                                                                                                                                 x14]))
                                                                                                                                                                                                                        (Prelude.return
                                                                                                                                                                                                                           (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                              x13
                                                                                                                                                                                                                              x14))
                                                                                                                                                                                                            DM.constructorHook
                                                                                                                                                                                                              (DI.DebugInfo
                                                                                                                                                                                                                 (DI.SrcID
                                                                                                                                                                                                                    "Maybe"
                                                                                                                                                                                                                    0)
                                                                                                                                                                                                                 (DI.DynamicInfo
                                                                                                                                                                                                                    []
                                                                                                                                                                                                                    [DI.genTerm
                                                                                                                                                                                                                       x15,
                                                                                                                                                                                                                     DI.genTerm
                                                                                                                                                                                                                       x16]))
                                                                                                                                                                                                              (Prelude.return
                                                                                                                                                                                                                 (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                    x15
                                                                                                                                                                                                                    x16))
                                                                                                                                                                                                  DM.constructorHook
                                                                                                                                                                                                    (DI.DebugInfo
                                                                                                                                                                                                       (DI.SrcID
                                                                                                                                                                                                          "Maybe"
                                                                                                                                                                                                          0)
                                                                                                                                                                                                       (DI.DynamicInfo
                                                                                                                                                                                                          []
                                                                                                                                                                                                          [DI.genTerm
                                                                                                                                                                                                             x17,
                                                                                                                                                                                                           DI.genTerm
                                                                                                                                                                                                             x18]))
                                                                                                                                                                                                    (Prelude.return
                                                                                                                                                                                                       (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                          x17
                                                                                                                                                                                                          x18))
                                                                                                                                                                                        DM.constructorHook
                                                                                                                                                                                          (DI.DebugInfo
                                                                                                                                                                                             (DI.SrcID
                                                                                                                                                                                                "Maybe"
                                                                                                                                                                                                0)
                                                                                                                                                                                             (DI.DynamicInfo
                                                                                                                                                                                                []
                                                                                                                                                                                                [DI.genTerm
                                                                                                                                                                                                   x19,
                                                                                                                                                                                                 DI.genTerm
                                                                                                                                                                                                   x20]))
                                                                                                                                                                                          (Prelude.return
                                                                                                                                                                                             (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                x19
                                                                                                                                                                                                x20))
                                                                                                                                                                              DM.constructorHook
                                                                                                                                                                                (DI.DebugInfo
                                                                                                                                                                                   (DI.SrcID
                                                                                                                                                                                      "Maybe"
                                                                                                                                                                                      0)
                                                                                                                                                                                   (DI.DynamicInfo
                                                                                                                                                                                      []
                                                                                                                                                                                      [DI.genTerm
                                                                                                                                                                                         x21,
                                                                                                                                                                                       DI.genTerm
                                                                                                                                                                                         x22]))
                                                                                                                                                                                (Prelude.return
                                                                                                                                                                                   (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                      x21
                                                                                                                                                                                      x22))
                                                                                                                                                                    DM.constructorHook
                                                                                                                                                                      (DI.DebugInfo
                                                                                                                                                                         (DI.SrcID
                                                                                                                                                                            "Maybe"
                                                                                                                                                                            0)
                                                                                                                                                                         (DI.DynamicInfo
                                                                                                                                                                            []
                                                                                                                                                                            [DI.genTerm
                                                                                                                                                                               x23,
                                                                                                                                                                             DI.genTerm
                                                                                                                                                                               x24]))
                                                                                                                                                                      (Prelude.return
                                                                                                                                                                         (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                            x23
                                                                                                                                                                            x24))
                                                                                                                                                          DM.constructorHook
                                                                                                                                                            (DI.DebugInfo
                                                                                                                                                               (DI.SrcID
                                                                                                                                                                  "Maybe"
                                                                                                                                                                  0)
                                                                                                                                                               (DI.DynamicInfo
                                                                                                                                                                  []
                                                                                                                                                                  [DI.genTerm
                                                                                                                                                                     x25,
                                                                                                                                                                   DI.genTerm
                                                                                                                                                                     x26]))
                                                                                                                                                            (Prelude.return
                                                                                                                                                               (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                  x25
                                                                                                                                                                  x26))
                                                                                                                                                DM.constructorHook
                                                                                                                                                  (DI.DebugInfo
                                                                                                                                                     (DI.SrcID
                                                                                                                                                        "Maybe"
                                                                                                                                                        0)
                                                                                                                                                     (DI.DynamicInfo
                                                                                                                                                        []
                                                                                                                                                        [DI.genTerm
                                                                                                                                                           x27,
                                                                                                                                                         DI.genTerm
                                                                                                                                                           x28]))
                                                                                                                                                  (Prelude.return
                                                                                                                                                     (Curry.DebugModule.Prelude.Cons
                                                                                                                                                        x27
                                                                                                                                                        x28))
                                                                                                                                      DM.constructorHook
                                                                                                                                        (DI.DebugInfo
                                                                                                                                           (DI.SrcID
                                                                                                                                              "Maybe"
                                                                                                                                              0)
                                                                                                                                           (DI.DynamicInfo
                                                                                                                                              []
                                                                                                                                              [DI.genTerm
                                                                                                                                                 x29,
                                                                                                                                               DI.genTerm
                                                                                                                                                 x30]))
                                                                                                                                        (Prelude.return
                                                                                                                                           (Curry.DebugModule.Prelude.Cons
                                                                                                                                              x29
                                                                                                                                              x30))
                                                                                                                            DM.constructorHook
                                                                                                                              (DI.DebugInfo
                                                                                                                                 (DI.SrcID
                                                                                                                                    "Maybe"
                                                                                                                                    0)
                                                                                                                                 (DI.DynamicInfo
                                                                                                                                    []
                                                                                                                                    [DI.genTerm
                                                                                                                                       x31,
                                                                                                                                     DI.genTerm
                                                                                                                                       x32]))
                                                                                                                              (Prelude.return
                                                                                                                                 (Curry.DebugModule.Prelude.Cons
                                                                                                                                    x31
                                                                                                                                    x32))
                                                                                                                  DM.constructorHook
                                                                                                                    (DI.DebugInfo
                                                                                                                       (DI.SrcID
                                                                                                                          "Maybe"
                                                                                                                          0)
                                                                                                                       (DI.DynamicInfo
                                                                                                                          []
                                                                                                                          [DI.genTerm
                                                                                                                             x33,
                                                                                                                           DI.genTerm
                                                                                                                             x34]))
                                                                                                                    (Prelude.return
                                                                                                                       (Curry.DebugModule.Prelude.Cons
                                                                                                                          x33
                                                                                                                          x34))
                                                                                                        DM.constructorHook
                                                                                                          (DI.DebugInfo
                                                                                                             (DI.SrcID
                                                                                                                "Maybe"
                                                                                                                0)
                                                                                                             (DI.DynamicInfo
                                                                                                                []
                                                                                                                [DI.genTerm
                                                                                                                   x35,
                                                                                                                 DI.genTerm
                                                                                                                   x36]))
                                                                                                          (Prelude.return
                                                                                                             (Curry.DebugModule.Prelude.Cons
                                                                                                                x35
                                                                                                                x36))
                                                                                              DM.constructorHook
                                                                                                (DI.DebugInfo
                                                                                                   (DI.SrcID
                                                                                                      "Maybe"
                                                                                                      0)
                                                                                                   (DI.DynamicInfo
                                                                                                      []
                                                                                                      [DI.genTerm
                                                                                                         x37,
                                                                                                       DI.genTerm
                                                                                                         x38]))
                                                                                                (Prelude.return
                                                                                                   (Curry.DebugModule.Prelude.Cons
                                                                                                      x37
                                                                                                      x38))
                                                                                    DM.constructorHook
                                                                                      (DI.DebugInfo
                                                                                         (DI.SrcID
                                                                                            "Maybe"
                                                                                            0)
                                                                                         (DI.DynamicInfo
                                                                                            []
                                                                                            [DI.genTerm
                                                                                               x39,
                                                                                             DI.genTerm
                                                                                               x40]))
                                                                                      (Prelude.return
                                                                                         (Curry.DebugModule.Prelude.Cons
                                                                                            x39
                                                                                            x40))
                                                                          DM.constructorHook
                                                                            (DI.DebugInfo
                                                                               (DI.SrcID "Maybe" 0)
                                                                               (DI.DynamicInfo []
                                                                                  [DI.genTerm x41,
                                                                                   DI.genTerm x42]))
                                                                            (Prelude.return
                                                                               (Curry.DebugModule.Prelude.Cons
                                                                                  x41
                                                                                  x42))
                                                                DM.constructorHook
                                                                  (DI.DebugInfo (DI.SrcID "Maybe" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x43,
                                                                         DI.genTerm x44]))
                                                                  (Prelude.return
                                                                     (Curry.DebugModule.Prelude.Cons
                                                                        x43
                                                                        x44))
                                                      DM.constructorHook
                                                        (DI.DebugInfo (DI.SrcID "Maybe" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x45, DI.genTerm x46]))
                                                        (Prelude.return
                                                           (Curry.DebugModule.Prelude.Cons x45 x46))
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Maybe" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x47, DI.genTerm x48]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Cons x47 x48))
                                  DM.funcCallHook "error"
                                    (DI.DebugInfo (DI.SrcID "Maybe" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x49]))
                                    (Curry.DebugModule.Prelude.strict_error x49)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Maybe" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x50])))
                           strict__case_6
                           x50)))
term_strict__case_6 x1 = DI.Term "_case_6" (DI.SrcID "Maybe" 0) x1
strict__case_7 x1
  = DM.eval
      (DM.funcDeclHook "_case_7"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Maybe" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x3]))
               (case x3 of
                    Curry.DebugModule.Prelude.Nothing
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.True)))
                    Curry.DebugModule.Prelude.Just x2
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.False)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Maybe" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x3])))
                           strict__case_7
                           x3)))
term_strict__case_7 x1 = DI.Term "_case_7" (DI.SrcID "Maybe" 0) x1
strict__case_8 x1
  = DM.eval
      (DM.funcDeclHook "_case_8"
         (DI.DebugInfo (DI.SrcID "Maybe" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Maybe" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x3]))
               (case x3 of
                    Curry.DebugModule.Prelude.Just x2
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.True)))
                    Curry.DebugModule.Prelude.Nothing
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Maybe" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.False)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Maybe" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x3])))
                           strict__case_8
                           x3)))
term_strict__case_8 x1 = DI.Term "_case_8" (DI.SrcID "Maybe" 0) x1