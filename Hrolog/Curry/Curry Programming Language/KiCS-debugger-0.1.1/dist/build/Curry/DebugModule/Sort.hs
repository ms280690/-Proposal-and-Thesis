{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Curry.DebugModule.Sort where
import qualified Prelude
import qualified Curry.Debugger.DebugMonad as DM
import qualified Curry.Debugger.DebugInfo as DI
import qualified Curry.Debugger.PartCalls as PC
import qualified Data.Generics
import qualified Curry.DebugModule.Char
import qualified Curry.DebugModule.Prelude
 
strict_quickSort ::
                 (DM.DM dm, DI.GenTerm a) =>
                   DM.Func dm a (DM.Func dm a Curry.DebugModule.Prelude.Bool) ->
                     Curry.DebugModule.Prelude.List a ->
                       dm (Curry.DebugModule.Prelude.List a)
strict_quickSort x1 x2
  = DM.eval
      (DM.funcDeclHook "quickSort"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_43"
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_43 x3 x4)))
term_strict_quickSort x1
  = DI.Term "quickSort" (DI.SrcID "Sort" 0) x1
 
x'xstrict_quickSort46split466 ::
                              (DM.DM dm, DI.GenTerm i) =>
                                DM.Func dm i (DM.Func dm i Curry.DebugModule.Prelude.Bool) ->
                                  i ->
                                    Curry.DebugModule.Prelude.List i ->
                                      dm
                                        (Curry.DebugModule.Prelude.Tuple2
                                           (Curry.DebugModule.Prelude.List i)
                                           (Curry.DebugModule.Prelude.List i))
x'xstrict_quickSort46split466 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "quickSort.split.6"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x1
             x5 <- Prelude.return x2
             x6 <- Prelude.return x3
             DM.funcCallHook "_case_42"
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
               (strict__case_42 x4 x5 x6)))
x'xterm_strict_quickSort46split466 x1
  = DI.Term "quickSort.split.6" (DI.SrcID "Sort" 0) x1
 
x'xstrict_quickSort46split46646_35selFP335l ::
                                            (DM.DM dm, DI.GenTerm i) =>
                                              Curry.DebugModule.Prelude.Tuple2
                                                (Curry.DebugModule.Prelude.List i)
                                                (Curry.DebugModule.Prelude.List i)
                                                -> dm (Curry.DebugModule.Prelude.List i)
x'xstrict_quickSort46split46646_35selFP335l x1
  = DM.eval
      (DM.funcDeclHook "quickSort.split.6._#selFP3#l"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_39"
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_39 x2)))
x'xterm_strict_quickSort46split46646_35selFP335l x1
  = DI.Term "quickSort.split.6._#selFP3#l" (DI.SrcID "Sort" 0) x1
 
x'xstrict_quickSort46split46646_35selFP435r ::
                                            (DM.DM dm, DI.GenTerm i) =>
                                              Curry.DebugModule.Prelude.Tuple2
                                                (Curry.DebugModule.Prelude.List i)
                                                (Curry.DebugModule.Prelude.List i)
                                                -> dm (Curry.DebugModule.Prelude.List i)
x'xstrict_quickSort46split46646_35selFP435r x1
  = DM.eval
      (DM.funcDeclHook "quickSort.split.6._#selFP4#r"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_38"
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_38 x2)))
x'xterm_strict_quickSort46split46646_35selFP435r x1
  = DI.Term "quickSort.split.6._#selFP4#r" (DI.SrcID "Sort" 0) x1
 
x'xstrict_quickSort46_35selFP635l ::
                                  (DM.DM dm, DI.GenTerm i) =>
                                    Curry.DebugModule.Prelude.Tuple2
                                      (Curry.DebugModule.Prelude.List i)
                                      (Curry.DebugModule.Prelude.List i)
                                      -> dm (Curry.DebugModule.Prelude.List i)
x'xstrict_quickSort46_35selFP635l x1
  = DM.eval
      (DM.funcDeclHook "quickSort._#selFP6#l"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_37"
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_37 x2)))
x'xterm_strict_quickSort46_35selFP635l x1
  = DI.Term "quickSort._#selFP6#l" (DI.SrcID "Sort" 0) x1
 
x'xstrict_quickSort46_35selFP735r ::
                                  (DM.DM dm, DI.GenTerm i) =>
                                    Curry.DebugModule.Prelude.Tuple2
                                      (Curry.DebugModule.Prelude.List i)
                                      (Curry.DebugModule.Prelude.List i)
                                      -> dm (Curry.DebugModule.Prelude.List i)
x'xstrict_quickSort46_35selFP735r x1
  = DM.eval
      (DM.funcDeclHook "quickSort._#selFP7#r"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_36"
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_36 x2)))
x'xterm_strict_quickSort46_35selFP735r x1
  = DI.Term "quickSort._#selFP7#r" (DI.SrcID "Sort" 0) x1
 
strict_mergeSort ::
                 (DM.DM dm, DI.GenTerm a) =>
                   DM.Func dm a (DM.Func dm a Curry.DebugModule.Prelude.Bool) ->
                     Curry.DebugModule.Prelude.List a ->
                       dm (Curry.DebugModule.Prelude.List a)
strict_mergeSort x1 x2
  = DM.eval
      (DM.funcDeclHook "mergeSort"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- Prelude.return x1
             x6 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.funcCallHook "mergeSort.genRuns.16"
                        (DI.DebugInfo (DI.SrcID "Sort" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (x'xstrict_mergeSort46genRuns4616 x3 x4)
             DM.funcCallHook "mergeSort.mergeLists.16"
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (x'xstrict_mergeSort46mergeLists4616 x5 x6)))
term_strict_mergeSort x1
  = DI.Term "mergeSort" (DI.SrcID "Sort" 0) x1
 
x'xstrict_mergeSort46genRuns4616 ::
                                 (DM.DM dm, DI.GenTerm x79) =>
                                   DM.Func dm x79 (DM.Func dm x79 Curry.DebugModule.Prelude.Bool) ->
                                     Curry.DebugModule.Prelude.List x79 ->
                                       dm
                                         (Curry.DebugModule.Prelude.List
                                            (Curry.DebugModule.Prelude.List x79))
x'xstrict_mergeSort46genRuns4616 x1 x2
  = DM.eval
      (DM.funcDeclHook "mergeSort.genRuns.16"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_35"
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_35 x3 x4)))
x'xterm_strict_mergeSort46genRuns4616 x1
  = DI.Term "mergeSort.genRuns.16" (DI.SrcID "Sort" 0) x1
 
x'xstrict_mergeSort46mergePairs4616 ::
                                    (DM.DM dm, DI.GenTerm x79) =>
                                      DM.Func dm x79 (DM.Func dm x79 Curry.DebugModule.Prelude.Bool)
                                        ->
                                        Curry.DebugModule.Prelude.List
                                          (Curry.DebugModule.Prelude.List x79)
                                          ->
                                          dm
                                            (Curry.DebugModule.Prelude.List
                                               (Curry.DebugModule.Prelude.List x79))
x'xstrict_mergeSort46mergePairs4616 x1 x2
  = DM.eval
      (DM.funcDeclHook "mergeSort.mergePairs.16"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_31"
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_31 x3 x4)))
x'xterm_strict_mergeSort46mergePairs4616 x1
  = DI.Term "mergeSort.mergePairs.16" (DI.SrcID "Sort" 0) x1
 
x'xstrict_mergeSort46mergeLists4616 ::
                                    (DM.DM dm, DI.GenTerm x79) =>
                                      DM.Func dm x79 (DM.Func dm x79 Curry.DebugModule.Prelude.Bool)
                                        ->
                                        Curry.DebugModule.Prelude.List
                                          (Curry.DebugModule.Prelude.List x79)
                                          -> dm (Curry.DebugModule.Prelude.List x79)
x'xstrict_mergeSort46mergeLists4616 x1 x2
  = DM.eval
      (DM.funcDeclHook "mergeSort.mergeLists.16"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_29"
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_29 x3 x4)))
x'xterm_strict_mergeSort46mergeLists4616 x1
  = DI.Term "mergeSort.mergeLists.16" (DI.SrcID "Sort" 0) x1
 
strict_merge ::
             (DM.DM dm, DI.GenTerm a) =>
               DM.Func dm a (DM.Func dm a Curry.DebugModule.Prelude.Bool) ->
                 Curry.DebugModule.Prelude.List a ->
                   Curry.DebugModule.Prelude.List a ->
                     dm (Curry.DebugModule.Prelude.List a)
strict_merge x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "merge"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x1
             x5 <- Prelude.return x3
             x6 <- Prelude.return x2
             DM.funcCallHook "_case_27"
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
               (strict__case_27 x4 x5 x6)))
term_strict_merge x1 = DI.Term "merge" (DI.SrcID "Sort" 0) x1
 
strict_leqList ::
               (DM.DM dm, DI.GenTerm a) =>
                 DM.Func dm a (DM.Func dm a Curry.DebugModule.Prelude.Bool) ->
                   Curry.DebugModule.Prelude.List a ->
                     Curry.DebugModule.Prelude.List a ->
                       dm Curry.DebugModule.Prelude.Bool
strict_leqList x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "leqList"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x1
             x5 <- Prelude.return x3
             x6 <- Prelude.return x2
             DM.funcCallHook "_case_23"
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
               (strict__case_23 x4 x5 x6)))
term_strict_leqList x1 = DI.Term "leqList" (DI.SrcID "Sort" 0) x1
 
strict_cmpList ::
               (DM.DM dm, DI.GenTerm a) =>
                 DM.Func dm a (DM.Func dm a Curry.DebugModule.Prelude.Ordering) ->
                   Curry.DebugModule.Prelude.List a ->
                     Curry.DebugModule.Prelude.List a ->
                       dm Curry.DebugModule.Prelude.Ordering
strict_cmpList x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "cmpList"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x1
             x5 <- Prelude.return x3
             x6 <- Prelude.return x2
             DM.funcCallHook "_case_19"
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
               (strict__case_19 x4 x5 x6)))
term_strict_cmpList x1 = DI.Term "cmpList" (DI.SrcID "Sort" 0) x1
 
strict_leqChar ::
               (DM.DM dm) =>
                 Curry.DebugModule.Prelude.Char ->
                   Curry.DebugModule.Prelude.Char -> dm Curry.DebugModule.Prelude.Bool
strict_leqChar x1 x2
  = DM.eval
      (DM.funcDeclHook "leqChar"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return x1
                      DM.funcCallHook "ord"
                        (DI.DebugInfo (DI.SrcID "Sort" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3]))
                        (Curry.DebugModule.Prelude.strict_ord x3)
             x6 <- do x4 <- Prelude.return x2
                      DM.funcCallHook "ord"
                        (DI.DebugInfo (DI.SrcID "Sort" 0)
                           (DI.DynamicInfo [] [DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.strict_ord x4)
             DM.funcCallHook "<="
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (Curry.DebugModule.Prelude.op_LtEq x5 x6)))
term_strict_leqChar x1 = DI.Term "leqChar" (DI.SrcID "Sort" 0) x1
 
strict_cmpChar ::
               (DM.DM dm) =>
                 Curry.DebugModule.Prelude.Char ->
                   Curry.DebugModule.Prelude.Char ->
                     dm Curry.DebugModule.Prelude.Ordering
strict_cmpChar x1 x2
  = DM.eval
      (DM.funcDeclHook "cmpChar"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- Prelude.return x1
             x6 <- Prelude.return x2
             x7 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.funcCallHook "=="
                        (DI.DebugInfo (DI.SrcID "Sort" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_EqEq x3 x4)
             DM.funcCallHook "_case_14"
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
               (strict__case_14 x5 x6 x7)))
term_strict_cmpChar x1 = DI.Term "cmpChar" (DI.SrcID "Sort" 0) x1
 
strict_leqCharIgnoreCase ::
                         (DM.DM dm) =>
                           Curry.DebugModule.Prelude.Char ->
                             Curry.DebugModule.Prelude.Char -> dm Curry.DebugModule.Prelude.Bool
strict_leqCharIgnoreCase x1 x2
  = DM.eval
      (DM.funcDeclHook "leqCharIgnoreCase"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x7 <- do x4 <- do x3 <- Prelude.return x1
                               DM.funcCallHook "toUpper"
                                 (DI.DebugInfo (DI.SrcID "Sort" 0)
                                    (DI.DynamicInfo [] [DI.genTerm x3]))
                                 (Curry.DebugModule.Char.strict_toUpper x3)
                      DM.funcCallHook "ord"
                        (DI.DebugInfo (DI.SrcID "Sort" 0)
                           (DI.DynamicInfo [] [DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.strict_ord x4)
             x8 <- do x6 <- do x5 <- Prelude.return x2
                               DM.funcCallHook "toUpper"
                                 (DI.DebugInfo (DI.SrcID "Sort" 0)
                                    (DI.DynamicInfo [] [DI.genTerm x5]))
                                 (Curry.DebugModule.Char.strict_toUpper x5)
                      DM.funcCallHook "ord"
                        (DI.DebugInfo (DI.SrcID "Sort" 0)
                           (DI.DynamicInfo [] [DI.genTerm x6]))
                        (Curry.DebugModule.Prelude.strict_ord x6)
             DM.funcCallHook "<="
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
               (Curry.DebugModule.Prelude.op_LtEq x7 x8)))
term_strict_leqCharIgnoreCase x1
  = DI.Term "leqCharIgnoreCase" (DI.SrcID "Sort" 0) x1
 
strict_leqString ::
                 (DM.DM dm) =>
                   dm
                     (DM.Func dm
                        (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                        (DM.Func dm
                           (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                           Curry.DebugModule.Prelude.Bool))
strict_leqString
  = DM.eval
      (DM.funcDeclHook "leqString"
         (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
         (do x0 <- Prelude.return
                     (PC.partCall2 (term_strict_leqChar []) strict_leqChar)
             Prelude.return
               (PC.partCall2 (term_strict_leqList [DI.genTerm x0])
                  (strict_leqList x0))))
 
strict_cmpString ::
                 (DM.DM dm) =>
                   dm
                     (DM.Func dm
                        (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                        (DM.Func dm
                           (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                           Curry.DebugModule.Prelude.Ordering))
strict_cmpString
  = DM.eval
      (DM.funcDeclHook "cmpString"
         (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
         (do x0 <- Prelude.return
                     (PC.partCall2 (term_strict_cmpChar []) strict_cmpChar)
             Prelude.return
               (PC.partCall2 (term_strict_cmpList [DI.genTerm x0])
                  (strict_cmpList x0))))
 
strict_leqStringIgnoreCase ::
                           (DM.DM dm) =>
                             dm
                               (DM.Func dm
                                  (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                                  (DM.Func dm
                                     (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                                     Curry.DebugModule.Prelude.Bool))
strict_leqStringIgnoreCase
  = DM.eval
      (DM.funcDeclHook "leqStringIgnoreCase"
         (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
         (do x0 <- Prelude.return
                     (PC.partCall2 (term_strict_leqCharIgnoreCase [])
                        strict_leqCharIgnoreCase)
             Prelude.return
               (PC.partCall2 (term_strict_leqList [DI.genTerm x0])
                  (strict_leqList x0))))
 
strict_leqLexGerman ::
                    (DM.DM dm) =>
                      Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                        Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                          dm Curry.DebugModule.Prelude.Bool
strict_leqLexGerman x1 x2
  = DM.eval
      (DM.funcDeclHook "leqLexGerman"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_12"
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_12 x3 x4)))
term_strict_leqLexGerman x1
  = DI.Term "leqLexGerman" (DI.SrcID "Sort" 0) x1
 
x'xstrict_leqLexGerman46glex4689 ::
                                 (DM.DM dm) =>
                                   Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
x'xstrict_leqLexGerman46glex4689 x1
  = DM.eval
      (DM.funcDeclHook "leqLexGerman.glex.89"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x10 <- Prelude.return x1
             x11 <- do x8 <- do x3 <- Prelude.return x1
                                x4 <- do x2 <- DM.litHook
                                                 (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                    (DI.DynamicInfo [] []))
                                                 (Prelude.return
                                                    (Curry.DebugModule.Prelude.Char 'A'))
                                         DM.funcCallHook "ord"
                                           (DI.DebugInfo (DI.SrcID "Sort" 0)
                                              (DI.DynamicInfo [] [DI.genTerm x2]))
                                           (Curry.DebugModule.Prelude.strict_ord x2)
                                DM.funcCallHook ">="
                                  (DI.DebugInfo (DI.SrcID "Sort" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                                  (Curry.DebugModule.Prelude.op_GtEq x3 x4)
                       x9 <- do x6 <- Prelude.return x1
                                x7 <- do x5 <- DM.litHook
                                                 (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                    (DI.DynamicInfo [] []))
                                                 (Prelude.return
                                                    (Curry.DebugModule.Prelude.Char 'Z'))
                                         DM.funcCallHook "ord"
                                           (DI.DebugInfo (DI.SrcID "Sort" 0)
                                              (DI.DynamicInfo [] [DI.genTerm x5]))
                                           (Curry.DebugModule.Prelude.strict_ord x5)
                                DM.funcCallHook "<="
                                  (DI.DebugInfo (DI.SrcID "Sort" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                  (Curry.DebugModule.Prelude.op_LtEq x6 x7)
                       DM.funcCallHook "&&"
                         (DI.DebugInfo (DI.SrcID "Sort" 0)
                            (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                         (Curry.DebugModule.Prelude.op_AndAnd x8 x9)
             DM.funcCallHook "_case_8"
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
               (strict__case_8 x10 x11)))
x'xterm_strict_leqLexGerman46glex4689 x1
  = DI.Term "leqLexGerman.glex.89" (DI.SrcID "Sort" 0) x1
strict__case_8 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_8"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x13 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x1
                                  x8 <- do x5 <- do x3 <- DM.litHook
                                                            (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                               (DI.DynamicInfo [] []))
                                                            (Prelude.return
                                                               (Curry.DebugModule.Prelude.Char 'a'))
                                                    DM.funcCallHook "ord"
                                                      (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                         (DI.DynamicInfo [] [DI.genTerm x3]))
                                                      (Curry.DebugModule.Prelude.strict_ord x3)
                                           x6 <- do x4 <- DM.litHook
                                                            (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                               (DI.DynamicInfo [] []))
                                                            (Prelude.return
                                                               (Curry.DebugModule.Prelude.Char 'A'))
                                                    DM.funcCallHook "ord"
                                                      (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                         (DI.DynamicInfo [] [DI.genTerm x4]))
                                                      (Curry.DebugModule.Prelude.strict_ord x4)
                                           DM.funcCallHook "-"
                                             (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                             (Curry.DebugModule.Prelude.op_Minus x5 x6)
                                  DM.funcCallHook "+"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                    (Curry.DebugModule.Prelude.op_Plus x7 x8)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- Prelude.return x1
                                  x12 <- do x9 <- Prelude.return x1
                                            x10 <- DM.litHook
                                                     (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                        (DI.DynamicInfo [] []))
                                                     (Prelude.return
                                                        (Curry.DebugModule.Prelude.Pos
                                                           (Curry.DebugModule.Prelude.O
                                                              (Curry.DebugModule.Prelude.O
                                                                 (Curry.DebugModule.Prelude.I
                                                                    (Curry.DebugModule.Prelude.O
                                                                       (Curry.DebugModule.Prelude.O
                                                                          (Curry.DebugModule.Prelude.I
                                                                             (Curry.DebugModule.Prelude.I
                                                                                Curry.DebugModule.Prelude.IHi)))))))))
                                            DM.funcCallHook "=="
                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x9, DI.genTerm x10]))
                                              (Curry.DebugModule.Prelude.op_EqEq x9 x10)
                                  DM.funcCallHook "_case_7"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11, DI.genTerm x12]))
                                    (strict__case_7 x11 x12)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_8 x1)
                           x13)))
term_strict__case_8 x1 = DI.Term "_case_8" (DI.SrcID "Sort" 0) x1
strict__case_7 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_7"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x8 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x3 <- DM.litHook
                                          (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                          (Prelude.return (Curry.DebugModule.Prelude.Char 'a'))
                                  DM.funcCallHook "ord"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x3]))
                                    (Curry.DebugModule.Prelude.strict_ord x3)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  x7 <- do x4 <- Prelude.return x1
                                           x5 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return
                                                      (Curry.DebugModule.Prelude.Pos
                                                         (Curry.DebugModule.Prelude.O
                                                            (Curry.DebugModule.Prelude.I
                                                               (Curry.DebugModule.Prelude.I
                                                                  (Curry.DebugModule.Prelude.O
                                                                     (Curry.DebugModule.Prelude.I
                                                                        (Curry.DebugModule.Prelude.I
                                                                           (Curry.DebugModule.Prelude.I
                                                                              Curry.DebugModule.Prelude.IHi)))))))))
                                           DM.funcCallHook "=="
                                             (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                             (Curry.DebugModule.Prelude.op_EqEq x4 x5)
                                  DM.funcCallHook "_case_6"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (strict__case_6 x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_7 x1)
                           x8)))
term_strict__case_7 x1 = DI.Term "_case_7" (DI.SrcID "Sort" 0) x1
strict__case_6 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_6"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x8 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x3 <- DM.litHook
                                          (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                          (Prelude.return (Curry.DebugModule.Prelude.Char 'o'))
                                  DM.funcCallHook "ord"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x3]))
                                    (Curry.DebugModule.Prelude.strict_ord x3)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  x7 <- do x4 <- Prelude.return x1
                                           x5 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return
                                                      (Curry.DebugModule.Prelude.Pos
                                                         (Curry.DebugModule.Prelude.O
                                                            (Curry.DebugModule.Prelude.O
                                                               (Curry.DebugModule.Prelude.I
                                                                  (Curry.DebugModule.Prelude.I
                                                                     (Curry.DebugModule.Prelude.I
                                                                        (Curry.DebugModule.Prelude.I
                                                                           (Curry.DebugModule.Prelude.I
                                                                              Curry.DebugModule.Prelude.IHi)))))))))
                                           DM.funcCallHook "=="
                                             (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                             (Curry.DebugModule.Prelude.op_EqEq x4 x5)
                                  DM.funcCallHook "_case_5"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (strict__case_5 x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_6 x1)
                           x8)))
term_strict__case_6 x1 = DI.Term "_case_6" (DI.SrcID "Sort" 0) x1
strict__case_5 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_5"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x8 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x3 <- DM.litHook
                                          (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                          (Prelude.return (Curry.DebugModule.Prelude.Char 'u'))
                                  DM.funcCallHook "ord"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x3]))
                                    (Curry.DebugModule.Prelude.strict_ord x3)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  x7 <- do x4 <- Prelude.return x1
                                           x5 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return
                                                      (Curry.DebugModule.Prelude.Pos
                                                         (Curry.DebugModule.Prelude.O
                                                            (Curry.DebugModule.Prelude.O
                                                               (Curry.DebugModule.Prelude.I
                                                                  (Curry.DebugModule.Prelude.O
                                                                     (Curry.DebugModule.Prelude.O
                                                                        (Curry.DebugModule.Prelude.O
                                                                           (Curry.DebugModule.Prelude.I
                                                                              Curry.DebugModule.Prelude.IHi)))))))))
                                           DM.funcCallHook "=="
                                             (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                             (Curry.DebugModule.Prelude.op_EqEq x4 x5)
                                  DM.funcCallHook "_case_4"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (strict__case_4 x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_5 x1)
                           x8)))
term_strict__case_5 x1 = DI.Term "_case_5" (DI.SrcID "Sort" 0) x1
strict__case_4 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_4"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x8 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x3 <- DM.litHook
                                          (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                          (Prelude.return (Curry.DebugModule.Prelude.Char 'a'))
                                  DM.funcCallHook "ord"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x3]))
                                    (Curry.DebugModule.Prelude.strict_ord x3)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  x7 <- do x4 <- Prelude.return x1
                                           x5 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return
                                                      (Curry.DebugModule.Prelude.Pos
                                                         (Curry.DebugModule.Prelude.O
                                                            (Curry.DebugModule.Prelude.I
                                                               (Curry.DebugModule.Prelude.I
                                                                  (Curry.DebugModule.Prelude.O
                                                                     (Curry.DebugModule.Prelude.I
                                                                        (Curry.DebugModule.Prelude.O
                                                                           (Curry.DebugModule.Prelude.I
                                                                              Curry.DebugModule.Prelude.IHi)))))))))
                                           DM.funcCallHook "=="
                                             (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                             (Curry.DebugModule.Prelude.op_EqEq x4 x5)
                                  DM.funcCallHook "_case_3"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (strict__case_3 x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_4 x1)
                           x8)))
term_strict__case_4 x1 = DI.Term "_case_4" (DI.SrcID "Sort" 0) x1
strict__case_3 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_3"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x8 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x3 <- DM.litHook
                                          (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                          (Prelude.return (Curry.DebugModule.Prelude.Char 'o'))
                                  DM.funcCallHook "ord"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x3]))
                                    (Curry.DebugModule.Prelude.strict_ord x3)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  x7 <- do x4 <- Prelude.return x1
                                           x5 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return
                                                      (Curry.DebugModule.Prelude.Pos
                                                         (Curry.DebugModule.Prelude.O
                                                            (Curry.DebugModule.Prelude.O
                                                               (Curry.DebugModule.Prelude.I
                                                                  (Curry.DebugModule.Prelude.I
                                                                     (Curry.DebugModule.Prelude.I
                                                                        (Curry.DebugModule.Prelude.O
                                                                           (Curry.DebugModule.Prelude.I
                                                                              Curry.DebugModule.Prelude.IHi)))))))))
                                           DM.funcCallHook "=="
                                             (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                             (Curry.DebugModule.Prelude.op_EqEq x4 x5)
                                  DM.funcCallHook "_case_2"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (strict__case_2 x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_3 x1)
                           x8)))
term_strict__case_3 x1 = DI.Term "_case_3" (DI.SrcID "Sort" 0) x1
strict__case_2 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_2"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x8 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x3 <- DM.litHook
                                          (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                          (Prelude.return (Curry.DebugModule.Prelude.Char 'u'))
                                  DM.funcCallHook "ord"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x3]))
                                    (Curry.DebugModule.Prelude.strict_ord x3)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  x7 <- do x4 <- Prelude.return x1
                                           x5 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return
                                                      (Curry.DebugModule.Prelude.Pos
                                                         (Curry.DebugModule.Prelude.I
                                                            (Curry.DebugModule.Prelude.I
                                                               (Curry.DebugModule.Prelude.I
                                                                  (Curry.DebugModule.Prelude.I
                                                                     (Curry.DebugModule.Prelude.I
                                                                        (Curry.DebugModule.Prelude.O
                                                                           (Curry.DebugModule.Prelude.I
                                                                              Curry.DebugModule.Prelude.IHi)))))))))
                                           DM.funcCallHook "=="
                                             (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                             (Curry.DebugModule.Prelude.op_EqEq x4 x5)
                                  DM.funcCallHook "_case_1"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (strict__case_1 x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_2 x1)
                           x8)))
term_strict__case_2 x1 = DI.Term "_case_2" (DI.SrcID "Sort" 0) x1
strict__case_1 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_1"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x6 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x6]))
               (case x6 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x3 <- DM.litHook
                                          (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                          (Prelude.return (Curry.DebugModule.Prelude.Char 's'))
                                  DM.funcCallHook "ord"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x3]))
                                    (Curry.DebugModule.Prelude.strict_ord x3)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- Prelude.return x1
                                  x5 <- DM.funcCallHook "otherwise"
                                          (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                          Curry.DebugModule.Prelude.strict_otherwise
                                  DM.funcCallHook "_case_0"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                    (strict__case_0 x4 x5)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x6])))
                           (strict__case_1 x1)
                           x6)))
term_strict__case_1 x1 = DI.Term "_case_1" (DI.SrcID "Sort" 0) x1
strict__case_0 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_0"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3]))
               (case x3 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x1))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 Curry.DebugModule.Prelude.strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x3])))
                           (strict__case_0 x1)
                           x3)))
term_strict__case_0 x1 = DI.Term "_case_0" (DI.SrcID "Sort" 0) x1
strict__case_12 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_12"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x8 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.True)))
                    Curry.DebugModule.Prelude.Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- Prelude.return x3
                                  x6 <- Prelude.return x4
                                  x7 <- Prelude.return x2
                                  DM.funcCallHook "_case_11"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
                                    (strict__case_11 x5 x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_12 x2)
                           x8)))
term_strict__case_12 x1 = DI.Term "_case_12" (DI.SrcID "Sort" 0) x1
strict__case_11 x3 x4 x2
  = DM.eval
      (DM.funcDeclHook "_case_11"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4, DI.genTerm x2]))
         (do x20 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x20]))
               (case x20 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.False)))
                    Curry.DebugModule.Prelude.Cons x5 x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x7 <- do x10 <- do x9 <- Prelude.return x3
                                                        DM.funcCallHook "ord"
                                                          (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                             (DI.DynamicInfo [] [DI.genTerm x9]))
                                                          (Curry.DebugModule.Prelude.strict_ord x9)
                                              DM.funcCallHook "leqLexGerman.glex.89"
                                                (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                   (DI.DynamicInfo [] [DI.genTerm x10]))
                                                (x'xstrict_leqLexGerman46glex4689 x10)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x8 <- do x12 <- do x11 <- Prelude.return x5
                                                                 DM.funcCallHook "ord"
                                                                   (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                                      (DI.DynamicInfo []
                                                                         [DI.genTerm x11]))
                                                                   (Curry.DebugModule.Prelude.strict_ord
                                                                      x11)
                                                       DM.funcCallHook "leqLexGerman.glex.89"
                                                         (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                            (DI.DynamicInfo [] [DI.genTerm x12]))
                                                         (x'xstrict_leqLexGerman46glex4689 x12)
                                              DM.eval
                                                (do x15 <- Prelude.return x4
                                                    x16 <- Prelude.return x6
                                                    x17 <- Prelude.return x7
                                                    x18 <- Prelude.return x8
                                                    x19 <- do x13 <- Prelude.return x7
                                                              x14 <- Prelude.return x8
                                                              DM.funcCallHook "=="
                                                                (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                                   (DI.DynamicInfo []
                                                                      [DI.genTerm x13,
                                                                       DI.genTerm x14]))
                                                                (Curry.DebugModule.Prelude.op_EqEq
                                                                   x13
                                                                   x14)
                                                    DM.funcCallHook "_case_10"
                                                      (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                         (DI.DynamicInfo []
                                                            [DI.genTerm x15, DI.genTerm x16,
                                                             DI.genTerm x17, DI.genTerm x18,
                                                             DI.genTerm x19]))
                                                      (strict__case_10 x15 x16 x17 x18 x19)))))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x20])))
                           (strict__case_11 x3 x4)
                           x20)))
term_strict__case_11 x1 = DI.Term "_case_11" (DI.SrcID "Sort" 0) x1
strict__case_10 x4 x6 x7 x8 x9
  = DM.eval
      (DM.funcDeclHook "_case_10"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo []
               [DI.genTerm x4, DI.genTerm x6, DI.genTerm x7, DI.genTerm x8,
                DI.genTerm x9]))
         (do x15 <- Prelude.return x9
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x15]))
               (case x15 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- Prelude.return x4
                                  x11 <- Prelude.return x6
                                  DM.funcCallHook "leqLexGerman"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
                                    (strict_leqLexGerman x10 x11)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- Prelude.return x7
                                  x13 <- Prelude.return x8
                                  x14 <- DM.funcCallHook "otherwise"
                                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                           Curry.DebugModule.Prelude.strict_otherwise
                                  DM.funcCallHook "_case_9"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x12, DI.genTerm x13, DI.genTerm x14]))
                                    (strict__case_9 x12 x13 x14)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x15])))
                           (strict__case_10 x4 x6 x7 x8)
                           x15)))
term_strict__case_10 x1 = DI.Term "_case_10" (DI.SrcID "Sort" 0) x1
strict__case_9 x7 x8 x9
  = DM.eval
      (DM.funcDeclHook "_case_9"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8, DI.genTerm x9]))
         (do x12 <- Prelude.return x9
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x12]))
               (case x12 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- Prelude.return x7
                                  x11 <- Prelude.return x8
                                  DM.funcCallHook "<"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
                                    (Curry.DebugModule.Prelude.op_Lt x10 x11)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 Curry.DebugModule.Prelude.strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x12])))
                           (strict__case_9 x7 x8)
                           x12)))
term_strict__case_9 x1 = DI.Term "_case_9" (DI.SrcID "Sort" 0) x1
strict__case_14 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_14"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x11 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.EQ)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x1
                                  x9 <- Prelude.return x2
                                  x10 <- do x6 <- do x4 <- Prelude.return x1
                                                     DM.funcCallHook "ord"
                                                       (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                          (DI.DynamicInfo [] [DI.genTerm x4]))
                                                       (Curry.DebugModule.Prelude.strict_ord x4)
                                            x7 <- do x5 <- Prelude.return x2
                                                     DM.funcCallHook "ord"
                                                       (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                          (DI.DynamicInfo [] [DI.genTerm x5]))
                                                       (Curry.DebugModule.Prelude.strict_ord x5)
                                            DM.funcCallHook "<"
                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                              (Curry.DebugModule.Prelude.op_Lt x6 x7)
                                  DM.funcCallHook "_case_13"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x8, DI.genTerm x9, DI.genTerm x10]))
                                    (strict__case_13 x8 x9 x10)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_14 x1 x2)
                           x11)))
term_strict__case_14 x1 = DI.Term "_case_14" (DI.SrcID "Sort" 0) x1
strict__case_13 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_13"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.LT)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.GT)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           (strict__case_13 x1 x2)
                           x4)))
term_strict__case_13 x1 = DI.Term "_case_13" (DI.SrcID "Sort" 0) x1
strict__case_19 x1 x3 x2
  = DM.eval
      (DM.funcDeclHook "_case_19"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x3, DI.genTerm x2]))
         (do x13 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x3
                                  DM.funcCallHook "_case_18"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8]))
                                    (strict__case_18 x8)))
                    Curry.DebugModule.Prelude.Cons x6 x7
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x1
                                  x10 <- Prelude.return x6
                                  x11 <- Prelude.return x7
                                  x12 <- Prelude.return x3
                                  DM.funcCallHook "_case_17"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x9, DI.genTerm x10, DI.genTerm x11,
                                           DI.genTerm x12]))
                                    (strict__case_17 x9 x10 x11 x12)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_19 x1 x3)
                           x13)))
term_strict__case_19 x1 = DI.Term "_case_19" (DI.SrcID "Sort" 0) x1
strict__case_17 x1 x6 x7 x3
  = DM.eval
      (DM.funcDeclHook "_case_17"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x6, DI.genTerm x7, DI.genTerm x3]))
         (do x22 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x22]))
               (case x22 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.GT)))
                    Curry.DebugModule.Prelude.Cons x8 x9
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x16 <- Prelude.return x1
                                  x17 <- Prelude.return x6
                                  x18 <- Prelude.return x7
                                  x19 <- Prelude.return x8
                                  x20 <- Prelude.return x9
                                  x21 <- do x14 <- do x12 <- do x10 <- Prelude.return x1
                                                                x11 <- Prelude.return x6
                                                                DM.funcCallHook "apply"
                                                                  (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x10,
                                                                         DI.genTerm x11]))
                                                                  (Curry.DebugModule.Prelude.strict_apply
                                                                     x10
                                                                     x11)
                                                      x13 <- Prelude.return x8
                                                      DM.funcCallHook "apply"
                                                        (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x12, DI.genTerm x13]))
                                                        (Curry.DebugModule.Prelude.strict_apply x12
                                                           x13)
                                            x15 <- DM.constructorHook
                                                     (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                        (DI.DynamicInfo [] []))
                                                     (Prelude.return Curry.DebugModule.Prelude.EQ)
                                            DM.funcCallHook "=="
                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x14, DI.genTerm x15]))
                                              (Curry.DebugModule.Prelude.op_EqEq x14 x15)
                                  DM.funcCallHook "_case_16"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x16, DI.genTerm x17, DI.genTerm x18,
                                           DI.genTerm x19, DI.genTerm x20, DI.genTerm x21]))
                                    (strict__case_16 x16 x17 x18 x19 x20 x21)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x22])))
                           (strict__case_17 x1 x6 x7)
                           x22)))
term_strict__case_17 x1 = DI.Term "_case_17" (DI.SrcID "Sort" 0) x1
strict__case_16 x1 x6 x7 x8 x9 x10
  = DM.eval
      (DM.funcDeclHook "_case_16"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x6, DI.genTerm x7, DI.genTerm x8,
                DI.genTerm x9, DI.genTerm x10]))
         (do x18 <- Prelude.return x10
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x18]))
               (case x18 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- Prelude.return x1
                                  x12 <- Prelude.return x7
                                  x13 <- Prelude.return x9
                                  DM.funcCallHook "cmpList"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x11, DI.genTerm x12, DI.genTerm x13]))
                                    (strict_cmpList x11 x12 x13)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x14 <- Prelude.return x1
                                  x15 <- Prelude.return x6
                                  x16 <- Prelude.return x8
                                  x17 <- DM.funcCallHook "otherwise"
                                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                           Curry.DebugModule.Prelude.strict_otherwise
                                  DM.funcCallHook "_case_15"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x14, DI.genTerm x15, DI.genTerm x16,
                                           DI.genTerm x17]))
                                    (strict__case_15 x14 x15 x16 x17)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x18])))
                           (strict__case_16 x1 x6 x7 x8 x9)
                           x18)))
term_strict__case_16 x1 = DI.Term "_case_16" (DI.SrcID "Sort" 0) x1
strict__case_15 x1 x6 x8 x9
  = DM.eval
      (DM.funcDeclHook "_case_15"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x6, DI.genTerm x8, DI.genTerm x9]))
         (do x14 <- Prelude.return x9
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x14]))
               (case x14 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- do x10 <- Prelude.return x1
                                            x11 <- Prelude.return x6
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x10, DI.genTerm x11]))
                                              (Curry.DebugModule.Prelude.strict_apply x10 x11)
                                  x13 <- Prelude.return x8
                                  DM.funcCallHook "apply"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x12, DI.genTerm x13]))
                                    (Curry.DebugModule.Prelude.strict_apply x12 x13)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 Curry.DebugModule.Prelude.strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x14])))
                           (strict__case_15 x1 x6 x8)
                           x14)))
term_strict__case_15 x1 = DI.Term "_case_15" (DI.SrcID "Sort" 0) x1
strict__case_18 x3
  = DM.eval
      (DM.funcDeclHook "_case_18"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x3]))
         (do x6 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x6]))
               (case x6 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.EQ)))
                    Curry.DebugModule.Prelude.Cons x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.LT)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x6])))
                           strict__case_18
                           x6)))
term_strict__case_18 x1 = DI.Term "_case_18" (DI.SrcID "Sort" 0) x1
strict__case_23 x1 x3 x2
  = DM.eval
      (DM.funcDeclHook "_case_23"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x3, DI.genTerm x2]))
         (do x10 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.True)))
                    Curry.DebugModule.Prelude.Cons x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  x7 <- Prelude.return x4
                                  x8 <- Prelude.return x5
                                  x9 <- Prelude.return x3
                                  DM.funcCallHook "_case_22"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x6, DI.genTerm x7, DI.genTerm x8,
                                           DI.genTerm x9]))
                                    (strict__case_22 x6 x7 x8 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_23 x1 x3)
                           x10)))
term_strict__case_23 x1 = DI.Term "_case_23" (DI.SrcID "Sort" 0) x1
strict__case_22 x1 x4 x5 x3
  = DM.eval
      (DM.funcDeclHook "_case_22"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x4, DI.genTerm x5, DI.genTerm x3]))
         (do x16 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x16]))
               (case x16 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.False)))
                    Curry.DebugModule.Prelude.Cons x6 x7
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- Prelude.return x1
                                  x11 <- Prelude.return x4
                                  x12 <- Prelude.return x5
                                  x13 <- Prelude.return x6
                                  x14 <- Prelude.return x7
                                  x15 <- do x8 <- Prelude.return x4
                                            x9 <- Prelude.return x6
                                            DM.funcCallHook "=="
                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                              (Curry.DebugModule.Prelude.op_EqEq x8 x9)
                                  DM.funcCallHook "_case_21"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x10, DI.genTerm x11, DI.genTerm x12,
                                           DI.genTerm x13, DI.genTerm x14, DI.genTerm x15]))
                                    (strict__case_21 x10 x11 x12 x13 x14 x15)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x16])))
                           (strict__case_22 x1 x4 x5)
                           x16)))
term_strict__case_22 x1 = DI.Term "_case_22" (DI.SrcID "Sort" 0) x1
strict__case_21 x1 x4 x5 x6 x7 x8
  = DM.eval
      (DM.funcDeclHook "_case_21"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x4, DI.genTerm x5, DI.genTerm x6,
                DI.genTerm x7, DI.genTerm x8]))
         (do x16 <- Prelude.return x8
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x16]))
               (case x16 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x1
                                  x10 <- Prelude.return x5
                                  x11 <- Prelude.return x7
                                  DM.funcCallHook "leqList"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x9, DI.genTerm x10, DI.genTerm x11]))
                                    (strict_leqList x9 x10 x11)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- Prelude.return x1
                                  x13 <- Prelude.return x4
                                  x14 <- Prelude.return x6
                                  x15 <- DM.funcCallHook "otherwise"
                                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                           Curry.DebugModule.Prelude.strict_otherwise
                                  DM.funcCallHook "_case_20"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x12, DI.genTerm x13, DI.genTerm x14,
                                           DI.genTerm x15]))
                                    (strict__case_20 x12 x13 x14 x15)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x16])))
                           (strict__case_21 x1 x4 x5 x6 x7)
                           x16)))
term_strict__case_21 x1 = DI.Term "_case_21" (DI.SrcID "Sort" 0) x1
strict__case_20 x1 x4 x6 x7
  = DM.eval
      (DM.funcDeclHook "_case_20"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x4, DI.genTerm x6, DI.genTerm x7]))
         (do x12 <- Prelude.return x7
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x12]))
               (case x12 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- do x8 <- Prelude.return x1
                                            x9 <- Prelude.return x4
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                              (Curry.DebugModule.Prelude.strict_apply x8 x9)
                                  x11 <- Prelude.return x6
                                  DM.funcCallHook "apply"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
                                    (Curry.DebugModule.Prelude.strict_apply x10 x11)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 Curry.DebugModule.Prelude.strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x12])))
                           (strict__case_20 x1 x4 x6)
                           x12)))
term_strict__case_20 x1 = DI.Term "_case_20" (DI.SrcID "Sort" 0) x1
strict__case_27 x1 x3 x2
  = DM.eval
      (DM.funcDeclHook "_case_27"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x3, DI.genTerm x2]))
         (do x10 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    Curry.DebugModule.Prelude.Cons x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  x7 <- Prelude.return x4
                                  x8 <- Prelude.return x5
                                  x9 <- Prelude.return x3
                                  DM.funcCallHook "_case_26"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x6, DI.genTerm x7, DI.genTerm x8,
                                           DI.genTerm x9]))
                                    (strict__case_26 x6 x7 x8 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_27 x1 x3)
                           x10)))
term_strict__case_27 x1 = DI.Term "_case_27" (DI.SrcID "Sort" 0) x1
strict__case_26 x1 x4 x5 x3
  = DM.eval
      (DM.funcDeclHook "_case_26"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x4, DI.genTerm x5, DI.genTerm x3]))
         (do x20 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x20]))
               (case x20 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x4
                                  x9 <- Prelude.return x5
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x8 x9))))
                    Curry.DebugModule.Prelude.Cons x6 x7
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x14 <- Prelude.return x1
                                  x15 <- Prelude.return x4
                                  x16 <- Prelude.return x5
                                  x17 <- Prelude.return x6
                                  x18 <- Prelude.return x7
                                  x19 <- do x12 <- do x10 <- Prelude.return x1
                                                      x11 <- Prelude.return x4
                                                      DM.funcCallHook "apply"
                                                        (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x10, DI.genTerm x11]))
                                                        (Curry.DebugModule.Prelude.strict_apply x10
                                                           x11)
                                            x13 <- Prelude.return x6
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x12, DI.genTerm x13]))
                                              (Curry.DebugModule.Prelude.strict_apply x12 x13)
                                  DM.funcCallHook "_case_25"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x14, DI.genTerm x15, DI.genTerm x16,
                                           DI.genTerm x17, DI.genTerm x18, DI.genTerm x19]))
                                    (strict__case_25 x14 x15 x16 x17 x18 x19)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x20])))
                           (strict__case_26 x1 x4 x5)
                           x20)))
term_strict__case_26 x1 = DI.Term "_case_26" (DI.SrcID "Sort" 0) x1
strict__case_25 x1 x4 x5 x6 x7 x8
  = DM.eval
      (DM.funcDeclHook "_case_25"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x4, DI.genTerm x5, DI.genTerm x6,
                DI.genTerm x7, DI.genTerm x8]))
         (do x22 <- Prelude.return x8
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x22]))
               (case x22 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x14 <- Prelude.return x4
                                  x15 <- do x11 <- Prelude.return x1
                                            x12 <- Prelude.return x5
                                            x13 <- do x9 <- Prelude.return x6
                                                      x10 <- Prelude.return x7
                                                      DM.constructorHook
                                                        (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x9, DI.genTerm x10]))
                                                        (Prelude.return
                                                           (Curry.DebugModule.Prelude.Cons x9 x10))
                                            DM.funcCallHook "merge"
                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x11, DI.genTerm x12,
                                                     DI.genTerm x13]))
                                              (strict_merge x11 x12 x13)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x14, DI.genTerm x15]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x14 x15))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x16 <- Prelude.return x1
                                  x17 <- Prelude.return x4
                                  x18 <- Prelude.return x5
                                  x19 <- Prelude.return x6
                                  x20 <- Prelude.return x7
                                  x21 <- DM.funcCallHook "otherwise"
                                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                           Curry.DebugModule.Prelude.strict_otherwise
                                  DM.funcCallHook "_case_24"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x16, DI.genTerm x17, DI.genTerm x18,
                                           DI.genTerm x19, DI.genTerm x20, DI.genTerm x21]))
                                    (strict__case_24 x16 x17 x18 x19 x20 x21)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x22])))
                           (strict__case_25 x1 x4 x5 x6 x7)
                           x22)))
term_strict__case_25 x1 = DI.Term "_case_25" (DI.SrcID "Sort" 0) x1
strict__case_24 x1 x4 x5 x6 x7 x8
  = DM.eval
      (DM.funcDeclHook "_case_24"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x4, DI.genTerm x5, DI.genTerm x6,
                DI.genTerm x7, DI.genTerm x8]))
         (do x16 <- Prelude.return x8
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x16]))
               (case x16 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x14 <- Prelude.return x6
                                  x15 <- do x11 <- Prelude.return x1
                                            x12 <- do x9 <- Prelude.return x4
                                                      x10 <- Prelude.return x5
                                                      DM.constructorHook
                                                        (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x9, DI.genTerm x10]))
                                                        (Prelude.return
                                                           (Curry.DebugModule.Prelude.Cons x9 x10))
                                            x13 <- Prelude.return x7
                                            DM.funcCallHook "merge"
                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x11, DI.genTerm x12,
                                                     DI.genTerm x13]))
                                              (strict_merge x11 x12 x13)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x14, DI.genTerm x15]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x14 x15))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 Curry.DebugModule.Prelude.strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x16])))
                           (strict__case_24 x1 x4 x5 x6 x7)
                           x16)))
term_strict__case_24 x1 = DI.Term "_case_24" (DI.SrcID "Sort" 0) x1
strict__case_29 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_29"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x8 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Nil)))
                    Curry.DebugModule.Prelude.Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- Prelude.return x1
                                  x6 <- Prelude.return x3
                                  x7 <- Prelude.return x4
                                  DM.funcCallHook "_case_28"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
                                    (strict__case_28 x5 x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_29 x1)
                           x8)))
term_strict__case_29 x1 = DI.Term "_case_29" (DI.SrcID "Sort" 0) x1
strict__case_28 x1 x3 x4
  = DM.eval
      (DM.funcDeclHook "_case_28"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x3, DI.genTerm x4]))
         (do x16 <- Prelude.return x4
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x16]))
               (case x16 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    Curry.DebugModule.Prelude.Cons x5 x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x14 <- Prelude.return x1
                                  x15 <- do x12 <- do x7 <- Prelude.return x1
                                                      x8 <- Prelude.return x3
                                                      x9 <- Prelude.return x5
                                                      DM.funcCallHook "merge"
                                                        (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x7, DI.genTerm x8,
                                                               DI.genTerm x9]))
                                                        (strict_merge x7 x8 x9)
                                            x13 <- do x10 <- Prelude.return x1
                                                      x11 <- Prelude.return x6
                                                      DM.funcCallHook "mergeSort.mergePairs.16"
                                                        (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x10, DI.genTerm x11]))
                                                        (x'xstrict_mergeSort46mergePairs4616 x10
                                                           x11)
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x12, DI.genTerm x13]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Cons x12 x13))
                                  DM.funcCallHook "mergeSort.mergeLists.16"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x14, DI.genTerm x15]))
                                    (x'xstrict_mergeSort46mergeLists4616 x14 x15)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x16])))
                           (strict__case_28 x1 x3)
                           x16)))
term_strict__case_28 x1 = DI.Term "_case_28" (DI.SrcID "Sort" 0) x1
strict__case_31 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_31"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x8 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Nil)))
                    Curry.DebugModule.Prelude.Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- Prelude.return x1
                                  x6 <- Prelude.return x3
                                  x7 <- Prelude.return x4
                                  DM.funcCallHook "_case_30"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
                                    (strict__case_30 x5 x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_31 x1)
                           x8)))
term_strict__case_31 x1 = DI.Term "_case_31" (DI.SrcID "Sort" 0) x1
strict__case_30 x1 x3 x4
  = DM.eval
      (DM.funcDeclHook "_case_30"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x3, DI.genTerm x4]))
         (do x16 <- Prelude.return x4
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x16]))
               (case x16 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x3
                                  x8 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                          (Prelude.return Curry.DebugModule.Prelude.Nil)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x7 x8))))
                    Curry.DebugModule.Prelude.Cons x5 x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x14 <- do x9 <- Prelude.return x1
                                            x10 <- Prelude.return x3
                                            x11 <- Prelude.return x5
                                            DM.funcCallHook "merge"
                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x9, DI.genTerm x10,
                                                     DI.genTerm x11]))
                                              (strict_merge x9 x10 x11)
                                  x15 <- do x12 <- Prelude.return x1
                                            x13 <- Prelude.return x6
                                            DM.funcCallHook "mergeSort.mergePairs.16"
                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x12, DI.genTerm x13]))
                                              (x'xstrict_mergeSort46mergePairs4616 x12 x13)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x14, DI.genTerm x15]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x14 x15))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x16])))
                           (strict__case_30 x1 x3)
                           x16)))
term_strict__case_30 x1 = DI.Term "_case_30" (DI.SrcID "Sort" 0) x1
strict__case_35 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_35"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x8 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Nil)))
                    Curry.DebugModule.Prelude.Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- Prelude.return x1
                                  x6 <- Prelude.return x3
                                  x7 <- Prelude.return x4
                                  DM.funcCallHook "_case_34"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
                                    (strict__case_34 x5 x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_35 x1)
                           x8)))
term_strict__case_35 x1 = DI.Term "_case_35" (DI.SrcID "Sort" 0) x1
strict__case_34 x1 x3 x4
  = DM.eval
      (DM.funcDeclHook "_case_34"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x3, DI.genTerm x4]))
         (do x20 <- Prelude.return x4
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x20]))
               (case x20 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- do x7 <- Prelude.return x3
                                           x8 <- DM.constructorHook
                                                   (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return Curry.DebugModule.Prelude.Nil)
                                           DM.constructorHook
                                             (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                             (Prelude.return (Curry.DebugModule.Prelude.Cons x7 x8))
                                  x10 <- DM.constructorHook
                                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                           (Prelude.return Curry.DebugModule.Prelude.Nil)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x9 x10))))
                    Curry.DebugModule.Prelude.Cons x5 x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x15 <- Prelude.return x1
                                  x16 <- Prelude.return x3
                                  x17 <- Prelude.return x5
                                  x18 <- Prelude.return x6
                                  x19 <- do x13 <- do x11 <- Prelude.return x1
                                                      x12 <- Prelude.return x3
                                                      DM.funcCallHook "apply"
                                                        (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x11, DI.genTerm x12]))
                                                        (Curry.DebugModule.Prelude.strict_apply x11
                                                           x12)
                                            x14 <- Prelude.return x5
                                            DM.funcCallHook "apply"
                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x13, DI.genTerm x14]))
                                              (Curry.DebugModule.Prelude.strict_apply x13 x14)
                                  DM.funcCallHook "_case_33"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x15, DI.genTerm x16, DI.genTerm x17,
                                           DI.genTerm x18, DI.genTerm x19]))
                                    (strict__case_33 x15 x16 x17 x18 x19)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x20])))
                           (strict__case_34 x1 x3)
                           x20)))
term_strict__case_34 x1 = DI.Term "_case_34" (DI.SrcID "Sort" 0) x1
strict__case_33 x1 x3 x5 x6 x7
  = DM.eval
      (DM.funcDeclHook "_case_33"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x3, DI.genTerm x5, DI.genTerm x6,
                DI.genTerm x7]))
         (do x21 <- Prelude.return x7
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x21]))
               (case x21 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x14 <- do x10 <- Prelude.return x3
                                            x11 <- do x8 <- Prelude.return x5
                                                      x9 <- DM.constructorHook
                                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                                 (DI.DynamicInfo [] []))
                                                              (Prelude.return
                                                                 Curry.DebugModule.Prelude.Nil)
                                                      DM.constructorHook
                                                        (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x8, DI.genTerm x9]))
                                                        (Prelude.return
                                                           (Curry.DebugModule.Prelude.Cons x8 x9))
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x10, DI.genTerm x11]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Cons x10 x11))
                                  x15 <- do x12 <- Prelude.return x1
                                            x13 <- Prelude.return x6
                                            DM.funcCallHook "mergeSort.genRuns.16"
                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x12, DI.genTerm x13]))
                                              (x'xstrict_mergeSort46genRuns4616 x12 x13)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x14, DI.genTerm x15]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x14 x15))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x16 <- Prelude.return x1
                                  x17 <- Prelude.return x3
                                  x18 <- Prelude.return x5
                                  x19 <- Prelude.return x6
                                  x20 <- DM.funcCallHook "otherwise"
                                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                           Curry.DebugModule.Prelude.strict_otherwise
                                  DM.funcCallHook "_case_32"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x16, DI.genTerm x17, DI.genTerm x18,
                                           DI.genTerm x19, DI.genTerm x20]))
                                    (strict__case_32 x16 x17 x18 x19 x20)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x21])))
                           (strict__case_33 x1 x3 x5 x6)
                           x21)))
term_strict__case_33 x1 = DI.Term "_case_33" (DI.SrcID "Sort" 0) x1
strict__case_32 x1 x3 x5 x6 x7
  = DM.eval
      (DM.funcDeclHook "_case_32"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x3, DI.genTerm x5, DI.genTerm x6,
                DI.genTerm x7]))
         (do x16 <- Prelude.return x7
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x16]))
               (case x16 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x14 <- do x10 <- Prelude.return x5
                                            x11 <- do x8 <- Prelude.return x3
                                                      x9 <- DM.constructorHook
                                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                                 (DI.DynamicInfo [] []))
                                                              (Prelude.return
                                                                 Curry.DebugModule.Prelude.Nil)
                                                      DM.constructorHook
                                                        (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x8, DI.genTerm x9]))
                                                        (Prelude.return
                                                           (Curry.DebugModule.Prelude.Cons x8 x9))
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x10, DI.genTerm x11]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Cons x10 x11))
                                  x15 <- do x12 <- Prelude.return x1
                                            x13 <- Prelude.return x6
                                            DM.funcCallHook "mergeSort.genRuns.16"
                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x12, DI.genTerm x13]))
                                              (x'xstrict_mergeSort46genRuns4616 x12 x13)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x14, DI.genTerm x15]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x14 x15))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 Curry.DebugModule.Prelude.strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x16])))
                           (strict__case_32 x1 x3 x5 x6)
                           x16)))
term_strict__case_32 x1 = DI.Term "_case_32" (DI.SrcID "Sort" 0) x1
strict__case_36 x1
  = DM.eval
      (DM.funcDeclHook "_case_36"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_36
                           x4)))
term_strict__case_36 x1 = DI.Term "_case_36" (DI.SrcID "Sort" 0) x1
strict__case_37 x1
  = DM.eval
      (DM.funcDeclHook "_case_37"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_37
                           x4)))
term_strict__case_37 x1 = DI.Term "_case_37" (DI.SrcID "Sort" 0) x1
strict__case_38 x1
  = DM.eval
      (DM.funcDeclHook "_case_38"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_38
                           x4)))
term_strict__case_38 x1 = DI.Term "_case_38" (DI.SrcID "Sort" 0) x1
strict__case_39 x1
  = DM.eval
      (DM.funcDeclHook "_case_39"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_39
                           x4)))
term_strict__case_39 x1 = DI.Term "_case_39" (DI.SrcID "Sort" 0) x1
strict__case_42 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_42"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x26 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x26]))
               (case x26 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                          (Prelude.return Curry.DebugModule.Prelude.Nil)
                                  x10 <- DM.constructorHook
                                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                           (Prelude.return Curry.DebugModule.Prelude.Nil)
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Tuple2 x9 x10))))
                    Curry.DebugModule.Prelude.Cons x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x6 <- do x11 <- Prelude.return x1
                                              x12 <- Prelude.return x2
                                              x13 <- Prelude.return x5
                                              DM.funcCallHook "quickSort.split.6"
                                                (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                   (DI.DynamicInfo []
                                                      [DI.genTerm x11, DI.genTerm x12,
                                                       DI.genTerm x13]))
                                                (x'xstrict_quickSort46split466 x11 x12 x13)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x7 <- do x14 <- Prelude.return x6
                                                       DM.funcCallHook
                                                         "quickSort.split.6._#selFP3#l"
                                                         (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                            (DI.DynamicInfo [] [DI.genTerm x14]))
                                                         (x'xstrict_quickSort46split46646_35selFP335l
                                                            x14)
                                              DM.eval
                                                (DM.letHook
                                                   (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                                      (DI.DynamicInfo [] []))
                                                   (do x8 <- do x15 <- Prelude.return x6
                                                                DM.funcCallHook
                                                                  "quickSort.split.6._#selFP4#r"
                                                                  (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x15]))
                                                                  (x'xstrict_quickSort46split46646_35selFP435r
                                                                     x15)
                                                       DM.eval
                                                         (do x20 <- Prelude.return x1
                                                             x21 <- Prelude.return x2
                                                             x22 <- Prelude.return x4
                                                             x23 <- Prelude.return x7
                                                             x24 <- Prelude.return x8
                                                             x25 <- do x18 <- do x16 <- Prelude.return
                                                                                          x1
                                                                                 x17 <- Prelude.return
                                                                                          x4
                                                                                 DM.funcCallHook
                                                                                   "apply"
                                                                                   (DI.DebugInfo
                                                                                      (DI.SrcID
                                                                                         "Sort"
                                                                                         0)
                                                                                      (DI.DynamicInfo
                                                                                         []
                                                                                         [DI.genTerm
                                                                                            x16,
                                                                                          DI.genTerm
                                                                                            x17]))
                                                                                   (Curry.DebugModule.Prelude.strict_apply
                                                                                      x16
                                                                                      x17)
                                                                       x19 <- Prelude.return x2
                                                                       DM.funcCallHook "apply"
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Sort" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x18,
                                                                                DI.genTerm x19]))
                                                                         (Curry.DebugModule.Prelude.strict_apply
                                                                            x18
                                                                            x19)
                                                             DM.funcCallHook "_case_41"
                                                               (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                                  (DI.DynamicInfo []
                                                                     [DI.genTerm x20,
                                                                      DI.genTerm x21,
                                                                      DI.genTerm x22,
                                                                      DI.genTerm x23,
                                                                      DI.genTerm x24,
                                                                      DI.genTerm x25]))
                                                               (strict__case_41 x20 x21 x22 x23 x24
                                                                  x25)))))))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x26])))
                           (strict__case_42 x1 x2)
                           x26)))
term_strict__case_42 x1 = DI.Term "_case_42" (DI.SrcID "Sort" 0) x1
strict__case_41 x1 x2 x4 x7 x8 x9
  = DM.eval
      (DM.funcDeclHook "_case_41"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x4, DI.genTerm x7,
                DI.genTerm x8, DI.genTerm x9]))
         (do x18 <- Prelude.return x9
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x18]))
               (case x18 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- do x10 <- Prelude.return x4
                                            x11 <- Prelude.return x7
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x10, DI.genTerm x11]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Cons x10 x11))
                                  x13 <- Prelude.return x8
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x12, DI.genTerm x13]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Tuple2 x12 x13))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x14 <- Prelude.return x4
                                  x15 <- Prelude.return x7
                                  x16 <- Prelude.return x8
                                  x17 <- DM.funcCallHook "otherwise"
                                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                           Curry.DebugModule.Prelude.strict_otherwise
                                  DM.funcCallHook "_case_40"
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x14, DI.genTerm x15, DI.genTerm x16,
                                           DI.genTerm x17]))
                                    (strict__case_40 x14 x15 x16 x17)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x18])))
                           (strict__case_41 x1 x2 x4 x7 x8)
                           x18)))
term_strict__case_41 x1 = DI.Term "_case_41" (DI.SrcID "Sort" 0) x1
strict__case_40 x4 x7 x8 x9
  = DM.eval
      (DM.funcDeclHook "_case_40"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo []
               [DI.genTerm x4, DI.genTerm x7, DI.genTerm x8, DI.genTerm x9]))
         (do x14 <- Prelude.return x9
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" 0)
                  (DI.DynamicInfo [] [DI.genTerm x14]))
               (case x14 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- Prelude.return x7
                                  x13 <- do x10 <- Prelude.return x4
                                            x11 <- Prelude.return x8
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x10, DI.genTerm x11]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Cons x10 x11))
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Sort" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x12, DI.genTerm x13]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Tuple2 x12 x13))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 Curry.DebugModule.Prelude.strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Sort" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x14])))
                           (strict__case_40 x4 x7 x8)
                           x14)))
term_strict__case_40 x1 = DI.Term "_case_40" (DI.SrcID "Sort" 0) x1
strict__case_43 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_43"
         (DI.DebugInfo (DI.SrcID "Sort" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x21 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x21]))
               (case x21 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Nil)))
                    Curry.DebugModule.Prelude.Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Sort" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x5 <- do x8 <- Prelude.return x1
                                              x9 <- Prelude.return x3
                                              x10 <- Prelude.return x4
                                              DM.funcCallHook "quickSort.split.6"
                                                (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                   (DI.DynamicInfo []
                                                      [DI.genTerm x8, DI.genTerm x9,
                                                       DI.genTerm x10]))
                                                (x'xstrict_quickSort46split466 x8 x9 x10)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x6 <- do x11 <- Prelude.return x5
                                                       DM.funcCallHook "quickSort._#selFP6#l"
                                                         (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                            (DI.DynamicInfo [] [DI.genTerm x11]))
                                                         (x'xstrict_quickSort46_35selFP635l x11)
                                              DM.eval
                                                (DM.letHook
                                                   (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                                      (DI.DynamicInfo [] []))
                                                   (do x7 <- do x12 <- Prelude.return x5
                                                                DM.funcCallHook
                                                                  "quickSort._#selFP7#r"
                                                                  (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x12]))
                                                                  (x'xstrict_quickSort46_35selFP735r
                                                                     x12)
                                                       DM.eval
                                                         (do x19 <- do x13 <- Prelude.return x1
                                                                       x14 <- Prelude.return x6
                                                                       DM.funcCallHook "quickSort"
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Sort" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x13,
                                                                                DI.genTerm x14]))
                                                                         (strict_quickSort x13 x14)
                                                             x20 <- do x17 <- Prelude.return x3
                                                                       x18 <- do x15 <- Prelude.return
                                                                                          x1
                                                                                 x16 <- Prelude.return
                                                                                          x7
                                                                                 DM.funcCallHook
                                                                                   "quickSort"
                                                                                   (DI.DebugInfo
                                                                                      (DI.SrcID
                                                                                         "Sort"
                                                                                         0)
                                                                                      (DI.DynamicInfo
                                                                                         []
                                                                                         [DI.genTerm
                                                                                            x15,
                                                                                          DI.genTerm
                                                                                            x16]))
                                                                                   (strict_quickSort
                                                                                      x15
                                                                                      x16)
                                                                       DM.constructorHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Sort" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x17,
                                                                                DI.genTerm x18]))
                                                                         (Prelude.return
                                                                            (Curry.DebugModule.Prelude.Cons
                                                                               x17
                                                                               x18))
                                                             DM.funcCallHook "++"
                                                               (DI.DebugInfo (DI.SrcID "Sort" 0)
                                                                  (DI.DynamicInfo []
                                                                     [DI.genTerm x19,
                                                                      DI.genTerm x20]))
                                                               (Curry.DebugModule.Prelude.op_PlusPlus
                                                                  x19
                                                                  x20)))))))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Sort" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x21])))
                           (strict__case_43 x1)
                           x21)))
term_strict__case_43 x1 = DI.Term "_case_43" (DI.SrcID "Sort" 0) x1