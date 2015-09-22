{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Curry.DebugModule.List where
import qualified Prelude
import qualified Curry.Debugger.DebugMonad as DM
import qualified Curry.Debugger.DebugInfo as DI
import qualified Curry.Debugger.PartCalls as PC
import qualified Data.Generics
import qualified Curry.DebugModule.Maybe
import qualified Curry.DebugModule.Prelude
 
strict_elemIndex ::
                 (DM.DM dm, DI.GenTerm a) =>
                   a ->
                     dm
                       (DM.Func dm (Curry.DebugModule.Prelude.List a)
                          (Curry.DebugModule.Prelude.Maybe Curry.DebugModule.Prelude.Int))
strict_elemIndex x1
  = DM.eval
      (DM.funcDeclHook "elemIndex"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- do x2 <- Prelude.return x1
                      Prelude.return
                        (PC.partCall1
                           (Curry.DebugModule.Prelude.term_op_EqEq [DI.genTerm x2])
                           (Curry.DebugModule.Prelude.op_EqEq x2))
             DM.funcCallHook "findIndex"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3]))
               (strict_findIndex x3)))
term_strict_elemIndex x1
  = DI.Term "elemIndex" (DI.SrcID "List" 0) x1
 
strict_elemIndices ::
                   (DM.DM dm, DI.GenTerm a) =>
                     a ->
                       dm
                         (DM.Func dm (Curry.DebugModule.Prelude.List a)
                            (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Int))
strict_elemIndices x1
  = DM.eval
      (DM.funcDeclHook "elemIndices"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- do x2 <- Prelude.return x1
                      Prelude.return
                        (PC.partCall1
                           (Curry.DebugModule.Prelude.term_op_EqEq [DI.genTerm x2])
                           (Curry.DebugModule.Prelude.op_EqEq x2))
             Prelude.return
               (PC.partCall1 (term_strict_findIndices [DI.genTerm x3])
                  (strict_findIndices x3))))
term_strict_elemIndices x1
  = DI.Term "elemIndices" (DI.SrcID "List" 0) x1
 
strict_find ::
            (DM.DM dm, DI.GenTerm a) =>
              DM.Func dm a Curry.DebugModule.Prelude.Bool ->
                dm
                  (DM.Func dm (Curry.DebugModule.Prelude.List a)
                     (Curry.DebugModule.Prelude.Maybe a))
strict_find x1
  = DM.eval
      (DM.funcDeclHook "find"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- Prelude.return
                     (PC.partCall1 (Curry.DebugModule.Maybe.term_strict_listToMaybe [])
                        Curry.DebugModule.Maybe.strict_listToMaybe)
             x4 <- do x2 <- Prelude.return x1
                      Prelude.return
                        (PC.partCall1
                           (Curry.DebugModule.Prelude.term_strict_filter [DI.genTerm x2])
                           (Curry.DebugModule.Prelude.strict_filter x2))
             DM.funcCallHook "."
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (Curry.DebugModule.Prelude.op_Point x3 x4)))
term_strict_find x1 = DI.Term "find" (DI.SrcID "List" 0) x1
 
strict_findIndex ::
                 (DM.DM dm, DI.GenTerm a) =>
                   DM.Func dm a Curry.DebugModule.Prelude.Bool ->
                     dm
                       (DM.Func dm (Curry.DebugModule.Prelude.List a)
                          (Curry.DebugModule.Prelude.Maybe Curry.DebugModule.Prelude.Int))
strict_findIndex x1
  = DM.eval
      (DM.funcDeclHook "findIndex"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- Prelude.return
                     (PC.partCall1 (Curry.DebugModule.Maybe.term_strict_listToMaybe [])
                        Curry.DebugModule.Maybe.strict_listToMaybe)
             x4 <- do x2 <- Prelude.return x1
                      Prelude.return
                        (PC.partCall1 (term_strict_findIndices [DI.genTerm x2])
                           (strict_findIndices x2))
             DM.funcCallHook "."
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (Curry.DebugModule.Prelude.op_Point x3 x4)))
term_strict_findIndex x1
  = DI.Term "findIndex" (DI.SrcID "List" 0) x1
 
strict_findIndices ::
                   (DM.DM dm, DI.GenTerm a) =>
                     DM.Func dm a Curry.DebugModule.Prelude.Bool ->
                       Curry.DebugModule.Prelude.List a ->
                         dm (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Int)
strict_findIndices x1 x2
  = DM.eval
      (DM.funcDeclHook "findIndices"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x7 <- do x3 <- Prelude.return x1
                      Prelude.return
                        (PC.partCall2
                           (x'xterm_strict_findIndices46_35lambda4 [DI.genTerm x3])
                           (x'xstrict_findIndices46_35lambda4 x3))
             x8 <- DM.constructorHook
                     (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                     (Prelude.return Curry.DebugModule.Prelude.Nil)
             x9 <- do x5 <- Prelude.return x2
                      x6 <- do x4 <- DM.litHook
                                       (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                                       (Prelude.return Curry.DebugModule.Prelude.Zero)
                               DM.funcCallHook "enumFrom"
                                 (DI.DebugInfo (DI.SrcID "List" 0)
                                    (DI.DynamicInfo [] [DI.genTerm x4]))
                                 (Curry.DebugModule.Prelude.strict_enumFrom x4)
                      DM.funcCallHook "zip"
                        (DI.DebugInfo (DI.SrcID "List" 0)
                           (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                        (Curry.DebugModule.Prelude.strict_zip x5 x6)
             DM.funcCallHook "foldr"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8, DI.genTerm x9]))
               (Curry.DebugModule.Prelude.strict_foldr x7 x8 x9)))
term_strict_findIndices x1
  = DI.Term "findIndices" (DI.SrcID "List" 0) x1
 
x'xstrict_findIndices46_35lambda4 ::
                                  (DM.DM dm, DI.GenTerm h) =>
                                    DM.Func dm h Curry.DebugModule.Prelude.Bool ->
                                      Curry.DebugModule.Prelude.Tuple2 h
                                        Curry.DebugModule.Prelude.Int
                                        ->
                                        Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Int
                                          ->
                                          dm
                                            (Curry.DebugModule.Prelude.List
                                               Curry.DebugModule.Prelude.Int)
x'xstrict_findIndices46_35lambda4 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "findIndices._#lambda4"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x1
             x5 <- Prelude.return x3
             x6 <- Prelude.return x2
             DM.funcCallHook "_case_26"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
               (strict__case_26 x4 x5 x6)))
x'xterm_strict_findIndices46_35lambda4 x1
  = DI.Term "findIndices._#lambda4" (DI.SrcID "List" 0) x1
 
strict_nub ::
           (DM.DM dm, DI.GenTerm a) =>
             Curry.DebugModule.Prelude.List a ->
               dm (Curry.DebugModule.Prelude.List a)
strict_nub x1
  = DM.eval
      (DM.funcDeclHook "nub"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall2 (Curry.DebugModule.Prelude.term_op_EqEq [])
                        Curry.DebugModule.Prelude.op_EqEq)
             x3 <- Prelude.return x1
             DM.funcCallHook "nubBy"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (strict_nubBy x2 x3)))
term_strict_nub x1 = DI.Term "nub" (DI.SrcID "List" 0) x1
 
strict_nubBy ::
             (DM.DM dm, DI.GenTerm a) =>
               DM.Func dm a (DM.Func dm a Curry.DebugModule.Prelude.Bool) ->
                 Curry.DebugModule.Prelude.List a ->
                   dm (Curry.DebugModule.Prelude.List a)
strict_nubBy x1 x2
  = DM.eval
      (DM.funcDeclHook "nubBy"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_24"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_24 x3 x4)))
term_strict_nubBy x1 = DI.Term "nubBy" (DI.SrcID "List" 0) x1
 
x'xstrict_nubBy46_35lambda6 ::
                            (DM.DM dm, DI.GenTerm x45) =>
                              DM.Func dm x45 (DM.Func dm x45 Curry.DebugModule.Prelude.Bool) ->
                                x45 -> x45 -> dm Curry.DebugModule.Prelude.Bool
x'xstrict_nubBy46_35lambda6 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "nubBy._#lambda6"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x8 <- do x6 <- do x4 <- Prelude.return x1
                               x5 <- Prelude.return x2
                               DM.funcCallHook "apply"
                                 (DI.DebugInfo (DI.SrcID "List" 0)
                                    (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                 (Curry.DebugModule.Prelude.strict_apply x4 x5)
                      x7 <- Prelude.return x3
                      DM.funcCallHook "apply"
                        (DI.DebugInfo (DI.SrcID "List" 0)
                           (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                        (Curry.DebugModule.Prelude.strict_apply x6 x7)
             DM.funcCallHook "not"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (Curry.DebugModule.Prelude.strict_not x8)))
x'xterm_strict_nubBy46_35lambda6 x1
  = DI.Term "nubBy._#lambda6" (DI.SrcID "List" 0) x1
 
strict_delete ::
              (DM.DM dm, DI.GenTerm a) =>
                a ->
                  Curry.DebugModule.Prelude.List a ->
                    dm (Curry.DebugModule.Prelude.List a)
strict_delete x1 x2
  = DM.eval
      (DM.funcDeclHook "delete"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_23"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_23 x3 x4)))
term_strict_delete x1 = DI.Term "delete" (DI.SrcID "List" 0) x1
 
op_BSlashBSlash ::
                (DM.DM dm, DI.GenTerm a) =>
                  Curry.DebugModule.Prelude.List a ->
                    Curry.DebugModule.Prelude.List a ->
                      dm (Curry.DebugModule.Prelude.List a)
op_BSlashBSlash x1 x2
  = DM.eval
      (DM.funcDeclHook "\\\\"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x4 <- do x3 <- Prelude.return
                              (PC.partCall2 (term_strict_delete []) strict_delete)
                      Prelude.return
                        (PC.partCall2
                           (Curry.DebugModule.Prelude.term_strict_flip [DI.genTerm x3])
                           (Curry.DebugModule.Prelude.strict_flip x3))
             x5 <- Prelude.return x1
             x6 <- Prelude.return x2
             DM.funcCallHook "foldl"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
               (Curry.DebugModule.Prelude.strict_foldl x4 x5 x6)))
term_op_BSlashBSlash x1 = DI.Term "\\\\" (DI.SrcID "List" 0) x1
 
strict_union ::
             (DM.DM dm, DI.GenTerm a) =>
               Curry.DebugModule.Prelude.List a ->
                 Curry.DebugModule.Prelude.List a ->
                   dm (Curry.DebugModule.Prelude.List a)
strict_union x1 x2
  = DM.eval
      (DM.funcDeclHook "union"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_21"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_21 x3 x4)))
term_strict_union x1 = DI.Term "union" (DI.SrcID "List" 0) x1
 
strict_intersect ::
                 (DM.DM dm, DI.GenTerm a) =>
                   Curry.DebugModule.Prelude.List a ->
                     Curry.DebugModule.Prelude.List a ->
                       dm (Curry.DebugModule.Prelude.List a)
strict_intersect x1 x2
  = DM.eval
      (DM.funcDeclHook "intersect"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_19"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_19 x3 x4)))
term_strict_intersect x1
  = DI.Term "intersect" (DI.SrcID "List" 0) x1
 
strict_intersperse ::
                   (DM.DM dm, DI.GenTerm a) =>
                     a ->
                       Curry.DebugModule.Prelude.List a ->
                         dm (Curry.DebugModule.Prelude.List a)
strict_intersperse x1 x2
  = DM.eval
      (DM.funcDeclHook "intersperse"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_17"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_17 x3 x4)))
term_strict_intersperse x1
  = DI.Term "intersperse" (DI.SrcID "List" 0) x1
 
strict_transpose ::
                 (DM.DM dm, DI.GenTerm a) =>
                   Curry.DebugModule.Prelude.List (Curry.DebugModule.Prelude.List a)
                     ->
                     dm
                       (Curry.DebugModule.Prelude.List (Curry.DebugModule.Prelude.List a))
strict_transpose x1
  = DM.eval
      (DM.funcDeclHook "transpose"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_15"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_15 x2)))
term_strict_transpose x1
  = DI.Term "transpose" (DI.SrcID "List" 0) x1
 
strict_partition ::
                 (DM.DM dm, DI.GenTerm a) =>
                   DM.Func dm a Curry.DebugModule.Prelude.Bool ->
                     Curry.DebugModule.Prelude.List a ->
                       dm
                         (Curry.DebugModule.Prelude.Tuple2
                            (Curry.DebugModule.Prelude.List a)
                            (Curry.DebugModule.Prelude.List a))
strict_partition x1 x2
  = DM.eval
      (DM.funcDeclHook "partition"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x6 <- do x3 <- Prelude.return x1
                      Prelude.return
                        (PC.partCall2
                           (x'xterm_strict_partition46select4653 [DI.genTerm x3])
                           (x'xstrict_partition46select4653 x3))
             x7 <- do x4 <- DM.constructorHook
                              (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Nil)
                      x5 <- DM.constructorHook
                              (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Nil)
                      DM.constructorHook
                        (DI.DebugInfo (DI.SrcID "List" 0)
                           (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                        (Prelude.return (Curry.DebugModule.Prelude.Tuple2 x4 x5))
             x8 <- Prelude.return x2
             DM.funcCallHook "foldr"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7, DI.genTerm x8]))
               (Curry.DebugModule.Prelude.strict_foldr x6 x7 x8)))
term_strict_partition x1
  = DI.Term "partition" (DI.SrcID "List" 0) x1
 
x'xstrict_partition46select4653 ::
                                (DM.DM dm, DI.GenTerm x146) =>
                                  DM.Func dm x146 Curry.DebugModule.Prelude.Bool ->
                                    x146 ->
                                      Curry.DebugModule.Prelude.Tuple2
                                        (Curry.DebugModule.Prelude.List x146)
                                        (Curry.DebugModule.Prelude.List x146)
                                        ->
                                        dm
                                          (Curry.DebugModule.Prelude.Tuple2
                                             (Curry.DebugModule.Prelude.List x146)
                                             (Curry.DebugModule.Prelude.List x146))
x'xstrict_partition46select4653 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "partition.select.53"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x1
             x5 <- Prelude.return x2
             x6 <- Prelude.return x3
             DM.funcCallHook "_case_13"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
               (strict__case_13 x4 x5 x6)))
x'xterm_strict_partition46select4653 x1
  = DI.Term "partition.select.53" (DI.SrcID "List" 0) x1
 
strict_group ::
             (DM.DM dm, DI.GenTerm a) =>
               dm
                 (DM.Func dm (Curry.DebugModule.Prelude.List a)
                    (Curry.DebugModule.Prelude.List
                       (Curry.DebugModule.Prelude.List a)))
strict_group
  = DM.eval
      (DM.funcDeclHook "group"
         (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
         (do x0 <- Prelude.return
                     (PC.partCall2 (Curry.DebugModule.Prelude.term_op_EqEq [])
                        Curry.DebugModule.Prelude.op_EqEq)
             Prelude.return
               (PC.partCall1 (term_strict_groupBy [DI.genTerm x0])
                  (strict_groupBy x0))))
 
strict_groupBy ::
               (DM.DM dm, DI.GenTerm a) =>
                 DM.Func dm a (DM.Func dm a Curry.DebugModule.Prelude.Bool) ->
                   Curry.DebugModule.Prelude.List a ->
                     dm
                       (Curry.DebugModule.Prelude.List (Curry.DebugModule.Prelude.List a))
strict_groupBy x1 x2
  = DM.eval
      (DM.funcDeclHook "groupBy"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_11"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_11 x3 x4)))
term_strict_groupBy x1 = DI.Term "groupBy" (DI.SrcID "List" 0) x1
 
x'xstrict_groupBy46_35selFP335ys ::
                                 (DM.DM dm, DI.GenTerm x154) =>
                                   Curry.DebugModule.Prelude.Tuple2
                                     (Curry.DebugModule.Prelude.List x154)
                                     (Curry.DebugModule.Prelude.List x154)
                                     -> dm (Curry.DebugModule.Prelude.List x154)
x'xstrict_groupBy46_35selFP335ys x1
  = DM.eval
      (DM.funcDeclHook "groupBy._#selFP3#ys"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_10"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_10 x2)))
x'xterm_strict_groupBy46_35selFP335ys x1
  = DI.Term "groupBy._#selFP3#ys" (DI.SrcID "List" 0) x1
 
x'xstrict_groupBy46_35selFP435zs ::
                                 (DM.DM dm, DI.GenTerm x154) =>
                                   Curry.DebugModule.Prelude.Tuple2
                                     (Curry.DebugModule.Prelude.List x154)
                                     (Curry.DebugModule.Prelude.List x154)
                                     -> dm (Curry.DebugModule.Prelude.List x154)
x'xstrict_groupBy46_35selFP435zs x1
  = DM.eval
      (DM.funcDeclHook "groupBy._#selFP4#zs"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_9"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_9 x2)))
x'xterm_strict_groupBy46_35selFP435zs x1
  = DI.Term "groupBy._#selFP4#zs" (DI.SrcID "List" 0) x1
 
strict_replace ::
               (DM.DM dm, DI.GenTerm a) =>
                 a ->
                   Curry.DebugModule.Prelude.Int ->
                     Curry.DebugModule.Prelude.List a ->
                       dm (Curry.DebugModule.Prelude.List a)
strict_replace x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "replace"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x1
             x5 <- Prelude.return x2
             x6 <- Prelude.return x3
             DM.funcCallHook "_case_8"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
               (strict__case_8 x4 x5 x6)))
term_strict_replace x1 = DI.Term "replace" (DI.SrcID "List" 0) x1
 
strict_isPrefixOf ::
                  (DM.DM dm, DI.GenTerm a) =>
                    Curry.DebugModule.Prelude.List a ->
                      Curry.DebugModule.Prelude.List a ->
                        dm Curry.DebugModule.Prelude.Bool
strict_isPrefixOf x1 x2
  = DM.eval
      (DM.funcDeclHook "isPrefixOf"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_5"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_5 x3 x4)))
term_strict_isPrefixOf x1
  = DI.Term "isPrefixOf" (DI.SrcID "List" 0) x1
 
strict_sortBy ::
              (DM.DM dm, DI.GenTerm a) =>
                DM.Func dm a (DM.Func dm a Curry.DebugModule.Prelude.Bool) ->
                  dm
                    (DM.Func dm (Curry.DebugModule.Prelude.List a)
                       (Curry.DebugModule.Prelude.List a))
strict_sortBy x1
  = DM.eval
      (DM.funcDeclHook "sortBy"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- do x2 <- Prelude.return x1
                      Prelude.return
                        (PC.partCall2 (term_strict_insertBy [DI.genTerm x2])
                           (strict_insertBy x2))
             x4 <- DM.constructorHook
                     (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                     (Prelude.return Curry.DebugModule.Prelude.Nil)
             Prelude.return
               (PC.partCall1
                  (Curry.DebugModule.Prelude.term_strict_foldr
                     [DI.genTerm x3, DI.genTerm x4])
                  (Curry.DebugModule.Prelude.strict_foldr x3 x4))))
term_strict_sortBy x1 = DI.Term "sortBy" (DI.SrcID "List" 0) x1
 
strict_insertBy ::
                (DM.DM dm, DI.GenTerm a) =>
                  DM.Func dm a (DM.Func dm a Curry.DebugModule.Prelude.Bool) ->
                    a ->
                      Curry.DebugModule.Prelude.List a ->
                        dm (Curry.DebugModule.Prelude.List a)
strict_insertBy x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "insertBy"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x1
             x5 <- Prelude.return x2
             x6 <- Prelude.return x3
             DM.funcCallHook "_case_3"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
               (strict__case_3 x4 x5 x6)))
term_strict_insertBy x1 = DI.Term "insertBy" (DI.SrcID "List" 0) x1
 
strict_last ::
            (DM.DM dm, DI.GenTerm a) =>
              Curry.DebugModule.Prelude.List a -> dm a
strict_last x1
  = DM.eval
      (DM.funcDeclHook "last"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_1"
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_1 x2)))
term_strict_last x1 = DI.Term "last" (DI.SrcID "List" 0) x1
strict__case_1 x1
  = DM.eval
      (DM.funcDeclHook "_case_1"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x6 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x6]))
               (case x6 of
                    Curry.DebugModule.Prelude.Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- Prelude.return x2
                                  x5 <- Prelude.return x3
                                  DM.funcCallHook "_case_0"
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                    (strict__case_0 x4 x5)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "List" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x6])))
                           strict__case_1
                           x6)))
term_strict__case_1 x1 = DI.Term "_case_1" (DI.SrcID "List" 0) x1
strict__case_0 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_0"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
         (do x9 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x9]))
               (case x9 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    Curry.DebugModule.Prelude.Cons x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- do x6 <- Prelude.return x4
                                           x7 <- Prelude.return x5
                                           DM.constructorHook
                                             (DI.DebugInfo (DI.SrcID "List" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                             (Prelude.return (Curry.DebugModule.Prelude.Cons x6 x7))
                                  DM.funcCallHook "last"
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8]))
                                    (strict_last x8)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "List" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x9])))
                           (strict__case_0 x2)
                           x9)))
term_strict__case_0 x1 = DI.Term "_case_0" (DI.SrcID "List" 0) x1
strict__case_3 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_3"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x17 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x17]))
               (case x17 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x2
                                  x7 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                                          (Prelude.return Curry.DebugModule.Prelude.Nil)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x6 x7))))
                    Curry.DebugModule.Prelude.Cons x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- Prelude.return x1
                                  x13 <- Prelude.return x2
                                  x14 <- Prelude.return x4
                                  x15 <- Prelude.return x5
                                  x16 <- do x10 <- do x8 <- Prelude.return x1
                                                      x9 <- Prelude.return x2
                                                      DM.funcCallHook "apply"
                                                        (DI.DebugInfo (DI.SrcID "List" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x8, DI.genTerm x9]))
                                                        (Curry.DebugModule.Prelude.strict_apply x8
                                                           x9)
                                            x11 <- Prelude.return x4
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "List" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x10, DI.genTerm x11]))
                                              (Curry.DebugModule.Prelude.strict_apply x10 x11)
                                  DM.funcCallHook "_case_2"
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x12, DI.genTerm x13, DI.genTerm x14,
                                           DI.genTerm x15, DI.genTerm x16]))
                                    (strict__case_2 x12 x13 x14 x15 x16)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "List" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x17])))
                           (strict__case_3 x1 x2)
                           x17)))
term_strict__case_3 x1 = DI.Term "_case_3" (DI.SrcID "List" 0) x1
strict__case_2 x1 x2 x4 x5 x6
  = DM.eval
      (DM.funcDeclHook "_case_2"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x4, DI.genTerm x5,
                DI.genTerm x6]))
         (do x16 <- Prelude.return x6
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x16]))
               (case x16 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x2
                                  x10 <- do x7 <- Prelude.return x4
                                            x8 <- Prelude.return x5
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "List" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Cons x7 x8))
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x9 x10))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x14 <- Prelude.return x4
                                  x15 <- do x11 <- Prelude.return x1
                                            x12 <- Prelude.return x2
                                            x13 <- Prelude.return x5
                                            DM.funcCallHook "insertBy"
                                              (DI.DebugInfo (DI.SrcID "List" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x11, DI.genTerm x12,
                                                     DI.genTerm x13]))
                                              (strict_insertBy x11 x12 x13)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x14, DI.genTerm x15]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x14 x15))))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "List" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x16])))
                           (strict__case_2 x1 x2 x4 x5)
                           x16)))
term_strict__case_2 x1 = DI.Term "_case_2" (DI.SrcID "List" 0) x1
strict__case_5 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_5"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x8 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.True)))
                    Curry.DebugModule.Prelude.Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- Prelude.return x3
                                  x6 <- Prelude.return x4
                                  x7 <- Prelude.return x2
                                  DM.funcCallHook "_case_4"
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
                                    (strict__case_4 x5 x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "List" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_5 x2)
                           x8)))
term_strict__case_5 x1 = DI.Term "_case_5" (DI.SrcID "List" 0) x1
strict__case_4 x3 x4 x2
  = DM.eval
      (DM.funcDeclHook "_case_4"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4, DI.genTerm x2]))
         (do x13 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.False)))
                    Curry.DebugModule.Prelude.Cons x5 x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- do x7 <- Prelude.return x3
                                            x8 <- Prelude.return x5
                                            DM.funcCallHook "=="
                                              (DI.DebugInfo (DI.SrcID "List" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                              (Curry.DebugModule.Prelude.op_EqEq x7 x8)
                                  x12 <- do x9 <- Prelude.return x4
                                            x10 <- Prelude.return x6
                                            DM.funcCallHook "isPrefixOf"
                                              (DI.DebugInfo (DI.SrcID "List" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x9, DI.genTerm x10]))
                                              (strict_isPrefixOf x9 x10)
                                  DM.funcCallHook "&&"
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11, DI.genTerm x12]))
                                    (Curry.DebugModule.Prelude.op_AndAnd x11 x12)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "List" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_4 x3 x4)
                           x13)))
term_strict__case_4 x1 = DI.Term "_case_4" (DI.SrcID "List" 0) x1
strict__case_8 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_8"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x13 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Nil)))
                    Curry.DebugModule.Prelude.Cons x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x1
                                  x9 <- Prelude.return x2
                                  x10 <- Prelude.return x4
                                  x11 <- Prelude.return x5
                                  x12 <- do x6 <- Prelude.return x2
                                            x7 <- DM.litHook
                                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                                       (DI.DynamicInfo [] []))
                                                    (Prelude.return Curry.DebugModule.Prelude.Zero)
                                            DM.funcCallHook "=="
                                              (DI.DebugInfo (DI.SrcID "List" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                              (Curry.DebugModule.Prelude.op_EqEq x6 x7)
                                  DM.funcCallHook "_case_7"
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x8, DI.genTerm x9, DI.genTerm x10,
                                           DI.genTerm x11, DI.genTerm x12]))
                                    (strict__case_7 x8 x9 x10 x11 x12)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "List" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_8 x1 x2)
                           x13)))
term_strict__case_8 x1 = DI.Term "_case_8" (DI.SrcID "List" 0) x1
strict__case_7 x1 x2 x4 x5 x6
  = DM.eval
      (DM.funcDeclHook "_case_7"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x4, DI.genTerm x5,
                DI.genTerm x6]))
         (do x14 <- Prelude.return x6
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x14]))
               (case x14 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x1
                                  x8 <- Prelude.return x5
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x7 x8))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x1
                                  x10 <- Prelude.return x2
                                  x11 <- Prelude.return x4
                                  x12 <- Prelude.return x5
                                  x13 <- DM.funcCallHook "otherwise"
                                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                                           Curry.DebugModule.Prelude.strict_otherwise
                                  DM.funcCallHook "_case_6"
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x9, DI.genTerm x10, DI.genTerm x11,
                                           DI.genTerm x12, DI.genTerm x13]))
                                    (strict__case_6 x9 x10 x11 x12 x13)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "List" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x14])))
                           (strict__case_7 x1 x2 x4 x5)
                           x14)))
term_strict__case_7 x1 = DI.Term "_case_7" (DI.SrcID "List" 0) x1
strict__case_6 x1 x2 x4 x5 x6
  = DM.eval
      (DM.funcDeclHook "_case_6"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x4, DI.genTerm x5,
                DI.genTerm x6]))
         (do x14 <- Prelude.return x6
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x14]))
               (case x14 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- Prelude.return x4
                                  x13 <- do x9 <- Prelude.return x1
                                            x10 <- do x7 <- Prelude.return x2
                                                      x8 <- DM.litHook
                                                              (DI.DebugInfo (DI.SrcID "List" 0)
                                                                 (DI.DynamicInfo [] []))
                                                              (Prelude.return
                                                                 (Curry.DebugModule.Prelude.Pos
                                                                    Curry.DebugModule.Prelude.IHi))
                                                      DM.funcCallHook "-"
                                                        (DI.DebugInfo (DI.SrcID "List" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x7, DI.genTerm x8]))
                                                        (Curry.DebugModule.Prelude.op_Minus x7 x8)
                                            x11 <- Prelude.return x5
                                            DM.funcCallHook "replace"
                                              (DI.DebugInfo (DI.SrcID "List" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x9, DI.genTerm x10,
                                                     DI.genTerm x11]))
                                              (strict_replace x9 x10 x11)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x12, DI.genTerm x13]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x12 x13))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                                 Curry.DebugModule.Prelude.strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "List" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x14])))
                           (strict__case_6 x1 x2 x4 x5)
                           x14)))
term_strict__case_6 x1 = DI.Term "_case_6" (DI.SrcID "List" 0) x1
strict__case_9 x1
  = DM.eval
      (DM.funcDeclHook "_case_9"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "List" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_9
                           x4)))
term_strict__case_9 x1 = DI.Term "_case_9" (DI.SrcID "List" 0) x1
strict__case_10 x1
  = DM.eval
      (DM.funcDeclHook "_case_10"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "List" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_10
                           x4)))
term_strict__case_10 x1 = DI.Term "_case_10" (DI.SrcID "List" 0) x1
strict__case_11 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_11"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x20 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x20]))
               (case x20 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Nil)))
                    Curry.DebugModule.Prelude.Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x5 <- do x10 <- do x8 <- Prelude.return x1
                                                        x9 <- Prelude.return x3
                                                        DM.funcCallHook "apply"
                                                          (DI.DebugInfo (DI.SrcID "List" 0)
                                                             (DI.DynamicInfo []
                                                                [DI.genTerm x8, DI.genTerm x9]))
                                                          (Curry.DebugModule.Prelude.strict_apply x8
                                                             x9)
                                              x11 <- Prelude.return x4
                                              DM.funcCallHook "span"
                                                (DI.DebugInfo (DI.SrcID "List" 0)
                                                   (DI.DynamicInfo []
                                                      [DI.genTerm x10, DI.genTerm x11]))
                                                (Curry.DebugModule.Prelude.strict_span x10 x11)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x6 <- do x12 <- Prelude.return x5
                                                       DM.funcCallHook "groupBy._#selFP3#ys"
                                                         (DI.DebugInfo (DI.SrcID "List" 0)
                                                            (DI.DynamicInfo [] [DI.genTerm x12]))
                                                         (x'xstrict_groupBy46_35selFP335ys x12)
                                              DM.eval
                                                (DM.letHook
                                                   (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                                      (DI.DynamicInfo [] []))
                                                   (do x7 <- do x13 <- Prelude.return x5
                                                                DM.funcCallHook
                                                                  "groupBy._#selFP4#zs"
                                                                  (DI.DebugInfo (DI.SrcID "List" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x13]))
                                                                  (x'xstrict_groupBy46_35selFP435zs
                                                                     x13)
                                                       DM.eval
                                                         (do x18 <- do x14 <- Prelude.return x3
                                                                       x15 <- Prelude.return x6
                                                                       DM.constructorHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "List" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x14,
                                                                                DI.genTerm x15]))
                                                                         (Prelude.return
                                                                            (Curry.DebugModule.Prelude.Cons
                                                                               x14
                                                                               x15))
                                                             x19 <- do x16 <- Prelude.return x1
                                                                       x17 <- Prelude.return x7
                                                                       DM.funcCallHook "groupBy"
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "List" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x16,
                                                                                DI.genTerm x17]))
                                                                         (strict_groupBy x16 x17)
                                                             DM.constructorHook
                                                               (DI.DebugInfo (DI.SrcID "List" 0)
                                                                  (DI.DynamicInfo []
                                                                     [DI.genTerm x18,
                                                                      DI.genTerm x19]))
                                                               (Prelude.return
                                                                  (Curry.DebugModule.Prelude.Cons
                                                                     x18
                                                                     x19))))))))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "List" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x20])))
                           (strict__case_11 x1)
                           x20)))
term_strict__case_11 x1 = DI.Term "_case_11" (DI.SrcID "List" 0) x1
strict__case_13 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_13"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x13 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    Curry.DebugModule.Prelude.Tuple2 x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x1
                                  x9 <- Prelude.return x2
                                  x10 <- Prelude.return x4
                                  x11 <- Prelude.return x5
                                  x12 <- do x6 <- Prelude.return x1
                                            x7 <- Prelude.return x2
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "List" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                              (Curry.DebugModule.Prelude.strict_apply x6 x7)
                                  DM.funcCallHook "_case_12"
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x8, DI.genTerm x9, DI.genTerm x10,
                                           DI.genTerm x11, DI.genTerm x12]))
                                    (strict__case_12 x8 x9 x10 x11 x12)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "List" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_13 x1 x2)
                           x13)))
term_strict__case_13 x1 = DI.Term "_case_13" (DI.SrcID "List" 0) x1
strict__case_12 x1 x2 x4 x5 x6
  = DM.eval
      (DM.funcDeclHook "_case_12"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x4, DI.genTerm x5,
                DI.genTerm x6]))
         (do x15 <- Prelude.return x6
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x15]))
               (case x15 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- do x7 <- Prelude.return x2
                                           x8 <- Prelude.return x4
                                           DM.constructorHook
                                             (DI.DebugInfo (DI.SrcID "List" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                             (Prelude.return (Curry.DebugModule.Prelude.Cons x7 x8))
                                  x10 <- Prelude.return x5
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Tuple2 x9 x10))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x13 <- Prelude.return x4
                                  x14 <- do x11 <- Prelude.return x2
                                            x12 <- Prelude.return x5
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "List" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x11, DI.genTerm x12]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Cons x11 x12))
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x13, DI.genTerm x14]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Tuple2 x13 x14))))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "List" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x15])))
                           (strict__case_12 x1 x2 x4 x5)
                           x15)))
term_strict__case_12 x1 = DI.Term "_case_12" (DI.SrcID "List" 0) x1
strict__case_15 x1
  = DM.eval
      (DM.funcDeclHook "_case_15"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x6 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x6]))
               (case x6 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Nil)))
                    Curry.DebugModule.Prelude.Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- Prelude.return x3
                                  x5 <- Prelude.return x2
                                  DM.funcCallHook "_case_14"
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                    (strict__case_14 x4 x5)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "List" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x6])))
                           strict__case_15
                           x6)))
term_strict__case_15 x1 = DI.Term "_case_15" (DI.SrcID "List" 0) x1
strict__case_14 x3 x2
  = DM.eval
      (DM.funcDeclHook "_case_14"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x2]))
         (do x18 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x18]))
               (case x18 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x3
                                  DM.funcCallHook "transpose"
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6]))
                                    (strict_transpose x6)))
                    Curry.DebugModule.Prelude.Cons x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x16 <- do x9 <- Prelude.return x4
                                            x10 <- do x7 <- Prelude.return
                                                              (PC.partCall1
                                                                 (Curry.DebugModule.Prelude.term_strict_head
                                                                    [])
                                                                 Curry.DebugModule.Prelude.strict_head)
                                                      x8 <- Prelude.return x3
                                                      DM.funcCallHook "map"
                                                        (DI.DebugInfo (DI.SrcID "List" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x7, DI.genTerm x8]))
                                                        (Curry.DebugModule.Prelude.strict_map x7 x8)
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "List" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x9, DI.genTerm x10]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Cons x9 x10))
                                  x17 <- do x15 <- do x13 <- Prelude.return x5
                                                      x14 <- do x11 <- Prelude.return
                                                                         (PC.partCall1
                                                                            (Curry.DebugModule.Prelude.term_strict_tail
                                                                               [])
                                                                            Curry.DebugModule.Prelude.strict_tail)
                                                                x12 <- Prelude.return x3
                                                                DM.funcCallHook "map"
                                                                  (DI.DebugInfo (DI.SrcID "List" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x11,
                                                                         DI.genTerm x12]))
                                                                  (Curry.DebugModule.Prelude.strict_map
                                                                     x11
                                                                     x12)
                                                      DM.constructorHook
                                                        (DI.DebugInfo (DI.SrcID "List" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x13, DI.genTerm x14]))
                                                        (Prelude.return
                                                           (Curry.DebugModule.Prelude.Cons x13 x14))
                                            DM.funcCallHook "transpose"
                                              (DI.DebugInfo (DI.SrcID "List" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x15]))
                                              (strict_transpose x15)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x16, DI.genTerm x17]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x16 x17))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "List" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x18])))
                           (strict__case_14 x3)
                           x18)))
term_strict__case_14 x1 = DI.Term "_case_14" (DI.SrcID "List" 0) x1
strict__case_17 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_17"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x8 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Nil)))
                    Curry.DebugModule.Prelude.Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- Prelude.return x1
                                  x6 <- Prelude.return x3
                                  x7 <- Prelude.return x4
                                  DM.funcCallHook "_case_16"
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
                                    (strict__case_16 x5 x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "List" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_17 x1)
                           x8)))
term_strict__case_17 x1 = DI.Term "_case_17" (DI.SrcID "List" 0) x1
strict__case_16 x1 x3 x4
  = DM.eval
      (DM.funcDeclHook "_case_16"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x3, DI.genTerm x4]))
         (do x17 <- Prelude.return x4
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x17]))
               (case x17 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x3
                                  x8 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                                          (Prelude.return Curry.DebugModule.Prelude.Nil)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x7 x8))))
                    Curry.DebugModule.Prelude.Cons x5 x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x15 <- Prelude.return x3
                                  x16 <- do x13 <- Prelude.return x1
                                            x14 <- do x11 <- Prelude.return x1
                                                      x12 <- do x9 <- Prelude.return x5
                                                                x10 <- Prelude.return x6
                                                                DM.constructorHook
                                                                  (DI.DebugInfo (DI.SrcID "List" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x9,
                                                                         DI.genTerm x10]))
                                                                  (Prelude.return
                                                                     (Curry.DebugModule.Prelude.Cons
                                                                        x9
                                                                        x10))
                                                      DM.funcCallHook "intersperse"
                                                        (DI.DebugInfo (DI.SrcID "List" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x11, DI.genTerm x12]))
                                                        (strict_intersperse x11 x12)
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "List" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x13, DI.genTerm x14]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Cons x13 x14))
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x15, DI.genTerm x16]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x15 x16))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "List" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x17])))
                           (strict__case_16 x1 x3)
                           x17)))
term_strict__case_16 x1 = DI.Term "_case_16" (DI.SrcID "List" 0) x1
strict__case_19 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_19"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x12 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x12]))
               (case x12 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Nil)))
                    Curry.DebugModule.Prelude.Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x2
                                  x9 <- Prelude.return x3
                                  x10 <- Prelude.return x4
                                  x11 <- do x6 <- do x5 <- Prelude.return x3
                                                     DM.funcCallHook "elem"
                                                       (DI.DebugInfo (DI.SrcID "List" 0)
                                                          (DI.DynamicInfo [] [DI.genTerm x5]))
                                                       (Curry.DebugModule.Prelude.strict_elem x5)
                                            x7 <- Prelude.return x2
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "List" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                              (Curry.DebugModule.Prelude.strict_apply x6 x7)
                                  DM.funcCallHook "_case_18"
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x8, DI.genTerm x9, DI.genTerm x10,
                                           DI.genTerm x11]))
                                    (strict__case_18 x8 x9 x10 x11)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "List" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x12])))
                           (strict__case_19 x2)
                           x12)))
term_strict__case_19 x1 = DI.Term "_case_19" (DI.SrcID "List" 0) x1
strict__case_18 x2 x3 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_18"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo []
               [DI.genTerm x2, DI.genTerm x3, DI.genTerm x4, DI.genTerm x5]))
         (do x12 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x12]))
               (case x12 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x3
                                  x9 <- do x6 <- Prelude.return x4
                                           x7 <- Prelude.return x2
                                           DM.funcCallHook "intersect"
                                             (DI.DebugInfo (DI.SrcID "List" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                             (strict_intersect x6 x7)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x8 x9))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- Prelude.return x4
                                  x11 <- Prelude.return x2
                                  DM.funcCallHook "intersect"
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
                                    (strict_intersect x10 x11)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "List" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x12])))
                           (strict__case_18 x2 x3 x4)
                           x12)))
term_strict__case_18 x1 = DI.Term "_case_18" (DI.SrcID "List" 0) x1
strict__case_21 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_21"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x12 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x12]))
               (case x12 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    Curry.DebugModule.Prelude.Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x2
                                  x9 <- Prelude.return x3
                                  x10 <- Prelude.return x4
                                  x11 <- do x6 <- do x5 <- Prelude.return x3
                                                     DM.funcCallHook "elem"
                                                       (DI.DebugInfo (DI.SrcID "List" 0)
                                                          (DI.DynamicInfo [] [DI.genTerm x5]))
                                                       (Curry.DebugModule.Prelude.strict_elem x5)
                                            x7 <- Prelude.return x2
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "List" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                              (Curry.DebugModule.Prelude.strict_apply x6 x7)
                                  DM.funcCallHook "_case_20"
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x8, DI.genTerm x9, DI.genTerm x10,
                                           DI.genTerm x11]))
                                    (strict__case_20 x8 x9 x10 x11)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "List" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x12])))
                           (strict__case_21 x2)
                           x12)))
term_strict__case_21 x1 = DI.Term "_case_21" (DI.SrcID "List" 0) x1
strict__case_20 x2 x3 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_20"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo []
               [DI.genTerm x2, DI.genTerm x3, DI.genTerm x4, DI.genTerm x5]))
         (do x12 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x12]))
               (case x12 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x4
                                  x7 <- Prelude.return x2
                                  DM.funcCallHook "union"
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (strict_union x6 x7)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- Prelude.return x3
                                  x11 <- do x8 <- Prelude.return x4
                                            x9 <- Prelude.return x2
                                            DM.funcCallHook "union"
                                              (DI.DebugInfo (DI.SrcID "List" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                              (strict_union x8 x9)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x10 x11))))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "List" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x12])))
                           (strict__case_20 x2 x3 x4)
                           x12)))
term_strict__case_20 x1 = DI.Term "_case_20" (DI.SrcID "List" 0) x1
strict__case_23 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_23"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x11 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Nil)))
                    Curry.DebugModule.Prelude.Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x1
                                  x8 <- Prelude.return x3
                                  x9 <- Prelude.return x4
                                  x10 <- do x5 <- Prelude.return x1
                                            x6 <- Prelude.return x3
                                            DM.funcCallHook "=="
                                              (DI.DebugInfo (DI.SrcID "List" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                              (Curry.DebugModule.Prelude.op_EqEq x5 x6)
                                  DM.funcCallHook "_case_22"
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x7, DI.genTerm x8, DI.genTerm x9,
                                           DI.genTerm x10]))
                                    (strict__case_22 x7 x8 x9 x10)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "List" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_23 x1)
                           x11)))
term_strict__case_23 x1 = DI.Term "_case_23" (DI.SrcID "List" 0) x1
strict__case_22 x1 x3 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_22"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x3, DI.genTerm x4, DI.genTerm x5]))
         (do x10 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x4))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x3
                                  x9 <- do x6 <- Prelude.return x1
                                           x7 <- Prelude.return x4
                                           DM.funcCallHook "delete"
                                             (DI.DebugInfo (DI.SrcID "List" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                             (strict_delete x6 x7)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x8 x9))))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "List" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_22 x1 x3 x4)
                           x10)))
term_strict__case_22 x1 = DI.Term "_case_22" (DI.SrcID "List" 0) x1
strict__case_24 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_24"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x13 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Nil)))
                    Curry.DebugModule.Prelude.Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- Prelude.return x3
                                  x12 <- do x9 <- Prelude.return x1
                                            x10 <- do x7 <- do x5 <- Prelude.return x1
                                                               x6 <- Prelude.return x3
                                                               Prelude.return
                                                                 (PC.partCall1
                                                                    (x'xterm_strict_nubBy46_35lambda6
                                                                       [DI.genTerm x5,
                                                                        DI.genTerm x6])
                                                                    (x'xstrict_nubBy46_35lambda6 x5
                                                                       x6))
                                                      x8 <- Prelude.return x4
                                                      DM.funcCallHook "filter"
                                                        (DI.DebugInfo (DI.SrcID "List" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x7, DI.genTerm x8]))
                                                        (Curry.DebugModule.Prelude.strict_filter x7
                                                           x8)
                                            DM.funcCallHook "nubBy"
                                              (DI.DebugInfo (DI.SrcID "List" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x9, DI.genTerm x10]))
                                              (strict_nubBy x9 x10)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11, DI.genTerm x12]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x11 x12))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "List" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_24 x1)
                           x13)))
term_strict__case_24 x1 = DI.Term "_case_24" (DI.SrcID "List" 0) x1
strict__case_26 x1 x3 x2
  = DM.eval
      (DM.funcDeclHook "_case_26"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x3, DI.genTerm x2]))
         (do x14 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x14]))
               (case x14 of
                    Curry.DebugModule.Prelude.Tuple2 x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- do x8 <- Prelude.return x1
                                            x9 <- Prelude.return x4
                                            x10 <- Prelude.return x5
                                            x11 <- do x6 <- Prelude.return x1
                                                      x7 <- Prelude.return x4
                                                      DM.funcCallHook "apply"
                                                        (DI.DebugInfo (DI.SrcID "List" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x6, DI.genTerm x7]))
                                                        (Curry.DebugModule.Prelude.strict_apply x6
                                                           x7)
                                            DM.funcCallHook "_case_25"
                                              (DI.DebugInfo (DI.SrcID "List" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x8, DI.genTerm x9, DI.genTerm x10,
                                                     DI.genTerm x11]))
                                              (strict__case_25 x8 x9 x10 x11)
                                  x13 <- Prelude.return x3
                                  DM.funcCallHook "++"
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x12, DI.genTerm x13]))
                                    (Curry.DebugModule.Prelude.op_PlusPlus x12 x13)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "List" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x14])))
                           (strict__case_26 x1 x3)
                           x14)))
term_strict__case_26 x1 = DI.Term "_case_26" (DI.SrcID "List" 0) x1
strict__case_25 x1 x4 x5 x6
  = DM.eval
      (DM.funcDeclHook "_case_25"
         (DI.DebugInfo (DI.SrcID "List" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
         (do x9 <- Prelude.return x6
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "List" 0)
                  (DI.DynamicInfo [] [DI.genTerm x9]))
               (case x9 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x5
                                  x8 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                                          (Prelude.return Curry.DebugModule.Prelude.Nil)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "List" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x7 x8))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "List" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Nil)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "List" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x9])))
                           (strict__case_25 x1 x4 x5)
                           x9)))
term_strict__case_25 x1 = DI.Term "_case_25" (DI.SrcID "List" 0) x1