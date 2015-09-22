{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Curry.DebugModule.RandomExternal where
import qualified Prelude
import qualified Curry.Debugger.DebugMonad as DM
import qualified Curry.Debugger.DebugInfo as DI
import qualified Curry.Debugger.PartCalls as PC
import qualified Data.Generics
import qualified Curry.DebugModule.Prelude
 
strict_prim_split ::
                  (DM.DM dm) =>
                    Curry.DebugModule.Prelude.Int ->
                      dm
                        (Curry.DebugModule.Prelude.Tuple2 Curry.DebugModule.Prelude.Int
                           Curry.DebugModule.Prelude.Int)
strict_prim_split x0
  = hook_strict_prim_split x0 (Prelude.error "not implemented")
 
strict_prim_nextInt ::
                    (DM.DM dm) =>
                      Curry.DebugModule.Prelude.Int ->
                        dm (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Int)
strict_prim_nextInt x0
  = hook_strict_prim_nextInt x0 (Prelude.error "not implemented")
 
strict_prim_nextIntRange ::
                         (DM.DM dm) =>
                           Curry.DebugModule.Prelude.Int ->
                             Curry.DebugModule.Prelude.Int ->
                               dm (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Int)
strict_prim_nextIntRange x0 x1
  = hook_strict_prim_nextIntRange x0 x1
      (Prelude.error "not implemented")
 
strict_getRandomSeed ::
                     (DM.DM dm) =>
                       dm (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Int)
strict_getRandomSeed
  = hook_strict_getRandomSeed (Prelude.error "not implemented")
 
strict_split ::
             (DM.DM dm) =>
               Curry.DebugModule.Prelude.Int ->
                 dm (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Int)
strict_split x1
  = DM.eval
      (DM.funcDeclHook "split"
         (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (DM.letHook
            (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
            (do x2 <- do x5 <- Prelude.return
                                 (PC.partCall1 (term_strict_prim_split []) strict_prim_split)
                         x6 <- Prelude.return x1
                         DM.funcCallHook "$##"
                           (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
                              (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                           (Curry.DebugModule.Prelude.op_DollarRhombRhomb x5 x6)
                DM.eval
                  (DM.letHook
                     (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                     (do x3 <- do x7 <- Prelude.return x2
                                  DM.funcCallHook "split._#selFP3#s1"
                                    (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7]))
                                    (x'xstrict_split46_35selFP335s1 x7)
                         DM.eval
                           (DM.letHook
                              (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                              (do x4 <- do x8 <- Prelude.return x2
                                           DM.funcCallHook "split._#selFP4#s2"
                                             (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x8]))
                                             (x'xstrict_split46_35selFP435s2 x8)
                                  DM.eval
                                    (do x10 <- Prelude.return x3
                                        x11 <- do x9 <- Prelude.return x4
                                                  DM.funcCallHook "split"
                                                    (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
                                                       (DI.DynamicInfo [] [DI.genTerm x9]))
                                                    (strict_split x9)
                                        DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
                                             (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
                                          (Prelude.return
                                             (Curry.DebugModule.Prelude.Cons x10 x11))))))))))
term_strict_split x1
  = DI.Term "split" (DI.SrcID "RandomExternal" 0) x1
 
x'xstrict_split46_35selFP335s1 ::
                               (DM.DM dm) =>
                                 Curry.DebugModule.Prelude.Tuple2 Curry.DebugModule.Prelude.Int
                                   Curry.DebugModule.Prelude.Int
                                   -> dm Curry.DebugModule.Prelude.Int
x'xstrict_split46_35selFP335s1 x1
  = DM.eval
      (DM.funcDeclHook "split._#selFP3#s1"
         (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_1"
               (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_1 x2)))
x'xterm_strict_split46_35selFP335s1 x1
  = DI.Term "split._#selFP3#s1" (DI.SrcID "RandomExternal" 0) x1
 
x'xstrict_split46_35selFP435s2 ::
                               (DM.DM dm) =>
                                 Curry.DebugModule.Prelude.Tuple2 Curry.DebugModule.Prelude.Int
                                   Curry.DebugModule.Prelude.Int
                                   -> dm Curry.DebugModule.Prelude.Int
x'xstrict_split46_35selFP435s2 x1
  = DM.eval
      (DM.funcDeclHook "split._#selFP4#s2"
         (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_0"
               (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_0 x2)))
x'xterm_strict_split46_35selFP435s2 x1
  = DI.Term "split._#selFP4#s2" (DI.SrcID "RandomExternal" 0) x1
 
strict_nextInt ::
               (DM.DM dm) =>
                 Curry.DebugModule.Prelude.Int ->
                   dm (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Int)
strict_nextInt x1
  = DM.eval
      (DM.funcDeclHook "nextInt"
         (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_nextInt []) strict_prim_nextInt)
             x3 <- Prelude.return x1
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhombRhomb x2 x3)))
term_strict_nextInt x1
  = DI.Term "nextInt" (DI.SrcID "RandomExternal" 0) x1
 
strict_nextIntRange ::
                    (DM.DM dm) =>
                      Curry.DebugModule.Prelude.Int ->
                        Curry.DebugModule.Prelude.Int ->
                          dm (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Int)
strict_nextIntRange x1 x2
  = DM.eval
      (DM.funcDeclHook "nextIntRange"
         (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return
                              (PC.partCall2 (term_strict_prim_nextIntRange [])
                                 strict_prim_nextIntRange)
                      x4 <- Prelude.return x1
                      DM.funcCallHook "$##"
                        (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_DollarRhombRhomb x3 x4)
             x6 <- Prelude.return x2
             DM.funcCallHook "$##"
               (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (Curry.DebugModule.Prelude.op_DollarRhombRhomb x5 x6)))
term_strict_nextIntRange x1
  = DI.Term "nextIntRange" (DI.SrcID "RandomExternal" 0) x1
 
strict_nextBoolean ::
                   (DM.DM dm) =>
                     Curry.DebugModule.Prelude.Int ->
                       dm (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Bool)
strict_nextBoolean x1
  = DM.eval
      (DM.funcDeclHook "nextBoolean"
         (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x5 <- do x2 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "RandomExternal" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Zero)
                      Prelude.return
                        (PC.partCall1
                           (Curry.DebugModule.Prelude.term_op_EqEq [DI.genTerm x2])
                           (Curry.DebugModule.Prelude.op_EqEq x2))
             x6 <- do x3 <- Prelude.return x1
                      x4 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "RandomExternal" 0) (DI.DynamicInfo [] []))
                              (Prelude.return
                                 (Curry.DebugModule.Prelude.Pos Curry.DebugModule.Prelude.IHi))
                      DM.funcCallHook "nextIntRange"
                        (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (strict_nextIntRange x3 x4)
             DM.funcCallHook "map"
               (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (Curry.DebugModule.Prelude.strict_map x5 x6)))
term_strict_nextBoolean x1
  = DI.Term "nextBoolean" (DI.SrcID "RandomExternal" 0) x1
strict__case_0 x1
  = DM.eval
      (DM.funcDeclHook "_case_0"
         (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "RandomExternal" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "RandomExternal" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "RandomExternal" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_0
                           x4)))
term_strict__case_0 x1
  = DI.Term "_case_0" (DI.SrcID "RandomExternal" 0) x1
strict__case_1 x1
  = DM.eval
      (DM.funcDeclHook "_case_1"
         (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "RandomExternal" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "RandomExternal" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "RandomExternal" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_1
                           x4)))
term_strict__case_1 x1
  = DI.Term "_case_1" (DI.SrcID "RandomExternal" 0) x1
hook_strict_prim_split x1 value
  = DM.eval
      (DM.funcDeclHook "prim_split"
         (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_split x1
  = DI.Term "prim_split" (DI.SrcID "RandomExternal" 0) x1
hook_strict_prim_nextInt x1 value
  = DM.eval
      (DM.funcDeclHook "prim_nextInt"
         (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_nextInt x1
  = DI.Term "prim_nextInt" (DI.SrcID "RandomExternal" 0) x1
hook_strict_prim_nextIntRange x1 x2 value
  = DM.eval
      (DM.funcDeclHook "prim_nextIntRange"
         (DI.DebugInfo (DI.SrcID "RandomExternal" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_strict_prim_nextIntRange x1
  = DI.Term "prim_nextIntRange" (DI.SrcID "RandomExternal" 0) x1
hook_strict_getRandomSeed value
  = DM.eval
      (DM.funcDeclHook "getRandomSeed"
         (DI.DebugInfo (DI.SrcID "RandomExternal" 0) (DI.DynamicInfo [] []))
         value)
term_strict_getRandomSeed x1
  = DI.Term "getRandomSeed" (DI.SrcID "RandomExternal" 0) x1