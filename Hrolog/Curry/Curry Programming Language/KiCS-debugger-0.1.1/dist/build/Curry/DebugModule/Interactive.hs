{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Curry.DebugModule.Interactive where
import qualified Prelude
import qualified Curry.Debugger.DebugMonad as DM
import qualified Curry.Debugger.DebugInfo as DI
import qualified Curry.Debugger.PartCalls as PC
import qualified Data.Generics
import qualified Curry.DebugModule.Prelude
 
strict_printTerm ::
                 (DM.DM dm, DI.GenTerm a) =>
                   a ->
                     dm (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Unit)
strict_printTerm x0
  = hook_strict_printTerm x0 (Prelude.error "not implemented")
 
strict_interactiveSols ::
                       (DM.DM dm, DI.GenTerm a) =>
                         Curry.DebugModule.Prelude.List a ->
                           dm (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Unit)
strict_interactiveSols x1
  = DM.eval
      (DM.funcDeclHook "interactiveSols"
         (DI.DebugInfo (DI.SrcID "Interactive" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_3"
               (DI.DebugInfo (DI.SrcID "Interactive" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_3 x2)))
term_strict_interactiveSols x1
  = DI.Term "interactiveSols" (DI.SrcID "Interactive" 0) x1
 
x'xstrict_interactiveSols46_35lambda2 ::
                                      (DM.DM dm, DI.GenTerm f) =>
                                        Curry.DebugModule.Prelude.List f ->
                                          Curry.DebugModule.Prelude.List
                                            Curry.DebugModule.Prelude.Char
                                            ->
                                            dm
                                              (Curry.DebugModule.Prelude.IO dm
                                                 Curry.DebugModule.Prelude.Unit)
x'xstrict_interactiveSols46_35lambda2 x1 x2
  = DM.eval
      (DM.funcDeclHook "interactiveSols._#lambda2"
         (DI.DebugInfo (DI.SrcID "Interactive" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_2"
               (DI.DebugInfo (DI.SrcID "Interactive" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_2 x3 x4)))
x'xterm_strict_interactiveSols46_35lambda2 x1
  = DI.Term "interactiveSols._#lambda2" (DI.SrcID "Interactive" 0) x1
 
strict_printIO ::
               (DM.DM dm, DI.GenTerm a) =>
                 Curry.DebugModule.Prelude.IO dm a ->
                   dm (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Unit)
strict_printIO x1
  = DM.eval
      (DM.funcDeclHook "printIO"
         (DI.DebugInfo (DI.SrcID "Interactive" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             x3 <- Prelude.return
                     (PC.partCall1 (x'xterm_strict_printIO46_35lambda4 [])
                        x'xstrict_printIO46_35lambda4)
             DM.funcCallHook ">>="
               (DI.DebugInfo (DI.SrcID "Interactive" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_GtGtEq x2 x3)))
term_strict_printIO x1
  = DI.Term "printIO" (DI.SrcID "Interactive" 0) x1
 
x'xstrict_printIO46_35lambda4 ::
                              (DM.DM dm, DI.GenTerm z) =>
                                z ->
                                  dm
                                    (Curry.DebugModule.Prelude.IO dm Curry.DebugModule.Prelude.Unit)
x'xstrict_printIO46_35lambda4 x1
  = DM.eval
      (DM.funcDeclHook "printIO._#lambda4"
         (DI.DebugInfo (DI.SrcID "Interactive" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x13 <- do x10 <- do x8 <- DM.litHook
                                         (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                            (DI.DynamicInfo [] []))
                                         (Prelude.return (Curry.DebugModule.Prelude.Char 'I'))
                                 x9 <- do x6 <- DM.litHook
                                                  (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                                     (DI.DynamicInfo [] []))
                                                  (Prelude.return
                                                     (Curry.DebugModule.Prelude.Char 'O'))
                                          x7 <- do x4 <- DM.litHook
                                                           (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                                              (DI.DynamicInfo [] []))
                                                           (Prelude.return
                                                              (Curry.DebugModule.Prelude.Char ':'))
                                                   x5 <- do x2 <- DM.litHook
                                                                    (DI.DebugInfo
                                                                       (DI.SrcID "Interactive" 0)
                                                                       (DI.DynamicInfo [] []))
                                                                    (Prelude.return
                                                                       (Curry.DebugModule.Prelude.Char
                                                                          ' '))
                                                            x3 <- DM.constructorHook
                                                                    (DI.DebugInfo
                                                                       (DI.SrcID "Interactive" 0)
                                                                       (DI.DynamicInfo [] []))
                                                                    (Prelude.return
                                                                       Curry.DebugModule.Prelude.Nil)
                                                            DM.constructorHook
                                                              (DI.DebugInfo
                                                                 (DI.SrcID "Interactive" 0)
                                                                 (DI.DynamicInfo []
                                                                    [DI.genTerm x2, DI.genTerm x3]))
                                                              (Prelude.return
                                                                 (Curry.DebugModule.Prelude.Cons x2
                                                                    x3))
                                                   DM.constructorHook
                                                     (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                                        (DI.DynamicInfo []
                                                           [DI.genTerm x4, DI.genTerm x5]))
                                                     (Prelude.return
                                                        (Curry.DebugModule.Prelude.Cons x4 x5))
                                          DM.constructorHook
                                            (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                               (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                            (Prelude.return (Curry.DebugModule.Prelude.Cons x6 x7))
                                 DM.constructorHook
                                   (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                      (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                   (Prelude.return (Curry.DebugModule.Prelude.Cons x8 x9))
                       DM.funcCallHook "putStr"
                         (DI.DebugInfo (DI.SrcID "Interactive" 0)
                            (DI.DynamicInfo [] [DI.genTerm x10]))
                         (Curry.DebugModule.Prelude.strict_putStr x10)
             x14 <- do x11 <- Prelude.return
                                (PC.partCall1 (term_strict_printTerm []) strict_printTerm)
                       x12 <- Prelude.return x1
                       DM.funcCallHook "$!!"
                         (DI.DebugInfo (DI.SrcID "Interactive" 0)
                            (DI.DynamicInfo [] [DI.genTerm x11, DI.genTerm x12]))
                         (Curry.DebugModule.Prelude.op_DollarEMarkEMark x11 x12)
             DM.funcCallHook ">>"
               (DI.DebugInfo (DI.SrcID "Interactive" 0)
                  (DI.DynamicInfo [] [DI.genTerm x13, DI.genTerm x14]))
               (Curry.DebugModule.Prelude.op_GtGt x13 x14)))
x'xterm_strict_printIO46_35lambda4 x1
  = DI.Term "printIO._#lambda4" (DI.SrcID "Interactive" 0) x1
strict__case_2 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_2"
         (DI.DebugInfo (DI.SrcID "Interactive" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x11 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Interactive" 0)
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    Curry.DebugModule.Prelude.Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Interactive" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- Prelude.return x1
                                  x8 <- Prelude.return x3
                                  x9 <- do x5 <- Prelude.return x3
                                           x6 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return
                                                      (Curry.DebugModule.Prelude.Char 'n'))
                                           DM.funcCallHook "=="
                                             (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                             (Curry.DebugModule.Prelude.op_EqEq x5 x6)
                                  DM.funcCallHook "_case_1"
                                    (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x7, DI.genTerm x8, DI.genTerm x9]))
                                    (strict__case_1 x7 x8 x9)))
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Interactive" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- Prelude.return x1
                                  DM.funcCallHook "interactiveSols"
                                    (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10]))
                                    (strict_interactiveSols x10)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_2 x1)
                           x11)))
term_strict__case_2 x1
  = DI.Term "_case_2" (DI.SrcID "Interactive" 0) x1
strict__case_1 x1 x3 x4
  = DM.eval
      (DM.funcDeclHook "_case_1"
         (DI.DebugInfo (DI.SrcID "Interactive" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x3, DI.genTerm x4]))
         (do x11 <- Prelude.return x4
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Interactive" 0)
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Interactive" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Curry.DebugModule.Prelude.Unit)
                                  DM.funcCallHook "return"
                                    (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5]))
                                    (Curry.DebugModule.Prelude.strict_return x5)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Interactive" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x1
                                  x9 <- Prelude.return x3
                                  x10 <- do x6 <- Prelude.return x3
                                            x7 <- DM.litHook
                                                    (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                                       (DI.DynamicInfo [] []))
                                                    (Prelude.return
                                                       (Curry.DebugModule.Prelude.Char 'N'))
                                            DM.funcCallHook "=="
                                              (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                              (Curry.DebugModule.Prelude.op_EqEq x6 x7)
                                  DM.funcCallHook "_case_0"
                                    (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x8, DI.genTerm x9, DI.genTerm x10]))
                                    (strict__case_0 x8 x9 x10)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_1 x1 x3)
                           x11)))
term_strict__case_1 x1
  = DI.Term "_case_1" (DI.SrcID "Interactive" 0) x1
strict__case_0 x1 x3 x4
  = DM.eval
      (DM.funcDeclHook "_case_0"
         (DI.DebugInfo (DI.SrcID "Interactive" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x3, DI.genTerm x4]))
         (do x7 <- Prelude.return x4
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Interactive" 0)
                  (DI.DynamicInfo [] [DI.genTerm x7]))
               (case x7 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Interactive" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- DM.constructorHook
                                          (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Curry.DebugModule.Prelude.Unit)
                                  DM.funcCallHook "return"
                                    (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5]))
                                    (Curry.DebugModule.Prelude.strict_return x5)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Interactive" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  DM.funcCallHook "interactiveSols"
                                    (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6]))
                                    (strict_interactiveSols x6)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x7])))
                           (strict__case_0 x1 x3)
                           x7)))
term_strict__case_0 x1
  = DI.Term "_case_0" (DI.SrcID "Interactive" 0) x1
strict__case_3 x1
  = DM.eval
      (DM.funcDeclHook "_case_3"
         (DI.DebugInfo (DI.SrcID "Interactive" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x58 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Interactive" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x58]))
               (case x58 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Interactive" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x38 <- do x36 <- DM.litHook
                                                     (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                                        (DI.DynamicInfo [] []))
                                                     (Prelude.return
                                                        (Curry.DebugModule.Prelude.Char 'N'))
                                            x37 <- do x34 <- DM.litHook
                                                               (DI.DebugInfo
                                                                  (DI.SrcID "Interactive" 0)
                                                                  (DI.DynamicInfo [] []))
                                                               (Prelude.return
                                                                  (Curry.DebugModule.Prelude.Char
                                                                     'o'))
                                                      x35 <- do x32 <- DM.litHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Interactive"
                                                                               0)
                                                                            (DI.DynamicInfo [] []))
                                                                         (Prelude.return
                                                                            (Curry.DebugModule.Prelude.Char
                                                                               ' '))
                                                                x33 <- do x30 <- DM.litHook
                                                                                   (DI.DebugInfo
                                                                                      (DI.SrcID
                                                                                         "Interactive"
                                                                                         0)
                                                                                      (DI.DynamicInfo
                                                                                         []
                                                                                         []))
                                                                                   (Prelude.return
                                                                                      (Curry.DebugModule.Prelude.Char
                                                                                         'm'))
                                                                          x31 <- do x28 <- DM.litHook
                                                                                             (DI.DebugInfo
                                                                                                (DI.SrcID
                                                                                                   "Interactive"
                                                                                                   0)
                                                                                                (DI.DynamicInfo
                                                                                                   []
                                                                                                   []))
                                                                                             (Prelude.return
                                                                                                (Curry.DebugModule.Prelude.Char
                                                                                                   'o'))
                                                                                    x29 <- do x26 <- DM.litHook
                                                                                                       (DI.DebugInfo
                                                                                                          (DI.SrcID
                                                                                                             "Interactive"
                                                                                                             0)
                                                                                                          (DI.DynamicInfo
                                                                                                             []
                                                                                                             []))
                                                                                                       (Prelude.return
                                                                                                          (Curry.DebugModule.Prelude.Char
                                                                                                             'r'))
                                                                                              x27 <- do x24 <- DM.litHook
                                                                                                                 (DI.DebugInfo
                                                                                                                    (DI.SrcID
                                                                                                                       "Interactive"
                                                                                                                       0)
                                                                                                                    (DI.DynamicInfo
                                                                                                                       []
                                                                                                                       []))
                                                                                                                 (Prelude.return
                                                                                                                    (Curry.DebugModule.Prelude.Char
                                                                                                                       'e'))
                                                                                                        x25 <- do x22 <- DM.litHook
                                                                                                                           (DI.DebugInfo
                                                                                                                              (DI.SrcID
                                                                                                                                 "Interactive"
                                                                                                                                 0)
                                                                                                                              (DI.DynamicInfo
                                                                                                                                 []
                                                                                                                                 []))
                                                                                                                           (Prelude.return
                                                                                                                              (Curry.DebugModule.Prelude.Char
                                                                                                                                 ' '))
                                                                                                                  x23 <- do x20 <- DM.litHook
                                                                                                                                     (DI.DebugInfo
                                                                                                                                        (DI.SrcID
                                                                                                                                           "Interactive"
                                                                                                                                           0)
                                                                                                                                        (DI.DynamicInfo
                                                                                                                                           []
                                                                                                                                           []))
                                                                                                                                     (Prelude.return
                                                                                                                                        (Curry.DebugModule.Prelude.Char
                                                                                                                                           'S'))
                                                                                                                            x21 <- do x18 <- DM.litHook
                                                                                                                                               (DI.DebugInfo
                                                                                                                                                  (DI.SrcID
                                                                                                                                                     "Interactive"
                                                                                                                                                     0)
                                                                                                                                                  (DI.DynamicInfo
                                                                                                                                                     []
                                                                                                                                                     []))
                                                                                                                                               (Prelude.return
                                                                                                                                                  (Curry.DebugModule.Prelude.Char
                                                                                                                                                     'o'))
                                                                                                                                      x19 <- do x16 <- DM.litHook
                                                                                                                                                         (DI.DebugInfo
                                                                                                                                                            (DI.SrcID
                                                                                                                                                               "Interactive"
                                                                                                                                                               0)
                                                                                                                                                            (DI.DynamicInfo
                                                                                                                                                               []
                                                                                                                                                               []))
                                                                                                                                                         (Prelude.return
                                                                                                                                                            (Curry.DebugModule.Prelude.Char
                                                                                                                                                               'l'))
                                                                                                                                                x17 <- do x14 <- DM.litHook
                                                                                                                                                                   (DI.DebugInfo
                                                                                                                                                                      (DI.SrcID
                                                                                                                                                                         "Interactive"
                                                                                                                                                                         0)
                                                                                                                                                                      (DI.DynamicInfo
                                                                                                                                                                         []
                                                                                                                                                                         []))
                                                                                                                                                                   (Prelude.return
                                                                                                                                                                      (Curry.DebugModule.Prelude.Char
                                                                                                                                                                         'u'))
                                                                                                                                                          x15 <- do x12 <- DM.litHook
                                                                                                                                                                             (DI.DebugInfo
                                                                                                                                                                                (DI.SrcID
                                                                                                                                                                                   "Interactive"
                                                                                                                                                                                   0)
                                                                                                                                                                                (DI.DynamicInfo
                                                                                                                                                                                   []
                                                                                                                                                                                   []))
                                                                                                                                                                             (Prelude.return
                                                                                                                                                                                (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                   't'))
                                                                                                                                                                    x13 <- do x10 <- DM.litHook
                                                                                                                                                                                       (DI.DebugInfo
                                                                                                                                                                                          (DI.SrcID
                                                                                                                                                                                             "Interactive"
                                                                                                                                                                                             0)
                                                                                                                                                                                          (DI.DynamicInfo
                                                                                                                                                                                             []
                                                                                                                                                                                             []))
                                                                                                                                                                                       (Prelude.return
                                                                                                                                                                                          (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                             'i'))
                                                                                                                                                                              x11 <- do x8 <- DM.litHook
                                                                                                                                                                                                (DI.DebugInfo
                                                                                                                                                                                                   (DI.SrcID
                                                                                                                                                                                                      "Interactive"
                                                                                                                                                                                                      0)
                                                                                                                                                                                                   (DI.DynamicInfo
                                                                                                                                                                                                      []
                                                                                                                                                                                                      []))
                                                                                                                                                                                                (Prelude.return
                                                                                                                                                                                                   (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                      'o'))
                                                                                                                                                                                        x9 <- do x6 <- DM.litHook
                                                                                                                                                                                                         (DI.DebugInfo
                                                                                                                                                                                                            (DI.SrcID
                                                                                                                                                                                                               "Interactive"
                                                                                                                                                                                                               0)
                                                                                                                                                                                                            (DI.DynamicInfo
                                                                                                                                                                                                               []
                                                                                                                                                                                                               []))
                                                                                                                                                                                                         (Prelude.return
                                                                                                                                                                                                            (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                               'n'))
                                                                                                                                                                                                 x7 <- do x4 <- DM.litHook
                                                                                                                                                                                                                  (DI.DebugInfo
                                                                                                                                                                                                                     (DI.SrcID
                                                                                                                                                                                                                        "Interactive"
                                                                                                                                                                                                                        0)
                                                                                                                                                                                                                     (DI.DynamicInfo
                                                                                                                                                                                                                        []
                                                                                                                                                                                                                        []))
                                                                                                                                                                                                                  (Prelude.return
                                                                                                                                                                                                                     (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                        's'))
                                                                                                                                                                                                          x5 <- DM.constructorHook
                                                                                                                                                                                                                  (DI.DebugInfo
                                                                                                                                                                                                                     (DI.SrcID
                                                                                                                                                                                                                        "Interactive"
                                                                                                                                                                                                                        0)
                                                                                                                                                                                                                     (DI.DynamicInfo
                                                                                                                                                                                                                        []
                                                                                                                                                                                                                        []))
                                                                                                                                                                                                                  (Prelude.return
                                                                                                                                                                                                                     Curry.DebugModule.Prelude.Nil)
                                                                                                                                                                                                          DM.constructorHook
                                                                                                                                                                                                            (DI.DebugInfo
                                                                                                                                                                                                               (DI.SrcID
                                                                                                                                                                                                                  "Interactive"
                                                                                                                                                                                                                  0)
                                                                                                                                                                                                               (DI.DynamicInfo
                                                                                                                                                                                                                  []
                                                                                                                                                                                                                  [DI.genTerm
                                                                                                                                                                                                                     x4,
                                                                                                                                                                                                                   DI.genTerm
                                                                                                                                                                                                                     x5]))
                                                                                                                                                                                                            (Prelude.return
                                                                                                                                                                                                               (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                  x4
                                                                                                                                                                                                                  x5))
                                                                                                                                                                                                 DM.constructorHook
                                                                                                                                                                                                   (DI.DebugInfo
                                                                                                                                                                                                      (DI.SrcID
                                                                                                                                                                                                         "Interactive"
                                                                                                                                                                                                         0)
                                                                                                                                                                                                      (DI.DynamicInfo
                                                                                                                                                                                                         []
                                                                                                                                                                                                         [DI.genTerm
                                                                                                                                                                                                            x6,
                                                                                                                                                                                                          DI.genTerm
                                                                                                                                                                                                            x7]))
                                                                                                                                                                                                   (Prelude.return
                                                                                                                                                                                                      (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                         x6
                                                                                                                                                                                                         x7))
                                                                                                                                                                                        DM.constructorHook
                                                                                                                                                                                          (DI.DebugInfo
                                                                                                                                                                                             (DI.SrcID
                                                                                                                                                                                                "Interactive"
                                                                                                                                                                                                0)
                                                                                                                                                                                             (DI.DynamicInfo
                                                                                                                                                                                                []
                                                                                                                                                                                                [DI.genTerm
                                                                                                                                                                                                   x8,
                                                                                                                                                                                                 DI.genTerm
                                                                                                                                                                                                   x9]))
                                                                                                                                                                                          (Prelude.return
                                                                                                                                                                                             (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                x8
                                                                                                                                                                                                x9))
                                                                                                                                                                              DM.constructorHook
                                                                                                                                                                                (DI.DebugInfo
                                                                                                                                                                                   (DI.SrcID
                                                                                                                                                                                      "Interactive"
                                                                                                                                                                                      0)
                                                                                                                                                                                   (DI.DynamicInfo
                                                                                                                                                                                      []
                                                                                                                                                                                      [DI.genTerm
                                                                                                                                                                                         x10,
                                                                                                                                                                                       DI.genTerm
                                                                                                                                                                                         x11]))
                                                                                                                                                                                (Prelude.return
                                                                                                                                                                                   (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                      x10
                                                                                                                                                                                      x11))
                                                                                                                                                                    DM.constructorHook
                                                                                                                                                                      (DI.DebugInfo
                                                                                                                                                                         (DI.SrcID
                                                                                                                                                                            "Interactive"
                                                                                                                                                                            0)
                                                                                                                                                                         (DI.DynamicInfo
                                                                                                                                                                            []
                                                                                                                                                                            [DI.genTerm
                                                                                                                                                                               x12,
                                                                                                                                                                             DI.genTerm
                                                                                                                                                                               x13]))
                                                                                                                                                                      (Prelude.return
                                                                                                                                                                         (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                            x12
                                                                                                                                                                            x13))
                                                                                                                                                          DM.constructorHook
                                                                                                                                                            (DI.DebugInfo
                                                                                                                                                               (DI.SrcID
                                                                                                                                                                  "Interactive"
                                                                                                                                                                  0)
                                                                                                                                                               (DI.DynamicInfo
                                                                                                                                                                  []
                                                                                                                                                                  [DI.genTerm
                                                                                                                                                                     x14,
                                                                                                                                                                   DI.genTerm
                                                                                                                                                                     x15]))
                                                                                                                                                            (Prelude.return
                                                                                                                                                               (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                  x14
                                                                                                                                                                  x15))
                                                                                                                                                DM.constructorHook
                                                                                                                                                  (DI.DebugInfo
                                                                                                                                                     (DI.SrcID
                                                                                                                                                        "Interactive"
                                                                                                                                                        0)
                                                                                                                                                     (DI.DynamicInfo
                                                                                                                                                        []
                                                                                                                                                        [DI.genTerm
                                                                                                                                                           x16,
                                                                                                                                                         DI.genTerm
                                                                                                                                                           x17]))
                                                                                                                                                  (Prelude.return
                                                                                                                                                     (Curry.DebugModule.Prelude.Cons
                                                                                                                                                        x16
                                                                                                                                                        x17))
                                                                                                                                      DM.constructorHook
                                                                                                                                        (DI.DebugInfo
                                                                                                                                           (DI.SrcID
                                                                                                                                              "Interactive"
                                                                                                                                              0)
                                                                                                                                           (DI.DynamicInfo
                                                                                                                                              []
                                                                                                                                              [DI.genTerm
                                                                                                                                                 x18,
                                                                                                                                               DI.genTerm
                                                                                                                                                 x19]))
                                                                                                                                        (Prelude.return
                                                                                                                                           (Curry.DebugModule.Prelude.Cons
                                                                                                                                              x18
                                                                                                                                              x19))
                                                                                                                            DM.constructorHook
                                                                                                                              (DI.DebugInfo
                                                                                                                                 (DI.SrcID
                                                                                                                                    "Interactive"
                                                                                                                                    0)
                                                                                                                                 (DI.DynamicInfo
                                                                                                                                    []
                                                                                                                                    [DI.genTerm
                                                                                                                                       x20,
                                                                                                                                     DI.genTerm
                                                                                                                                       x21]))
                                                                                                                              (Prelude.return
                                                                                                                                 (Curry.DebugModule.Prelude.Cons
                                                                                                                                    x20
                                                                                                                                    x21))
                                                                                                                  DM.constructorHook
                                                                                                                    (DI.DebugInfo
                                                                                                                       (DI.SrcID
                                                                                                                          "Interactive"
                                                                                                                          0)
                                                                                                                       (DI.DynamicInfo
                                                                                                                          []
                                                                                                                          [DI.genTerm
                                                                                                                             x22,
                                                                                                                           DI.genTerm
                                                                                                                             x23]))
                                                                                                                    (Prelude.return
                                                                                                                       (Curry.DebugModule.Prelude.Cons
                                                                                                                          x22
                                                                                                                          x23))
                                                                                                        DM.constructorHook
                                                                                                          (DI.DebugInfo
                                                                                                             (DI.SrcID
                                                                                                                "Interactive"
                                                                                                                0)
                                                                                                             (DI.DynamicInfo
                                                                                                                []
                                                                                                                [DI.genTerm
                                                                                                                   x24,
                                                                                                                 DI.genTerm
                                                                                                                   x25]))
                                                                                                          (Prelude.return
                                                                                                             (Curry.DebugModule.Prelude.Cons
                                                                                                                x24
                                                                                                                x25))
                                                                                              DM.constructorHook
                                                                                                (DI.DebugInfo
                                                                                                   (DI.SrcID
                                                                                                      "Interactive"
                                                                                                      0)
                                                                                                   (DI.DynamicInfo
                                                                                                      []
                                                                                                      [DI.genTerm
                                                                                                         x26,
                                                                                                       DI.genTerm
                                                                                                         x27]))
                                                                                                (Prelude.return
                                                                                                   (Curry.DebugModule.Prelude.Cons
                                                                                                      x26
                                                                                                      x27))
                                                                                    DM.constructorHook
                                                                                      (DI.DebugInfo
                                                                                         (DI.SrcID
                                                                                            "Interactive"
                                                                                            0)
                                                                                         (DI.DynamicInfo
                                                                                            []
                                                                                            [DI.genTerm
                                                                                               x28,
                                                                                             DI.genTerm
                                                                                               x29]))
                                                                                      (Prelude.return
                                                                                         (Curry.DebugModule.Prelude.Cons
                                                                                            x28
                                                                                            x29))
                                                                          DM.constructorHook
                                                                            (DI.DebugInfo
                                                                               (DI.SrcID
                                                                                  "Interactive"
                                                                                  0)
                                                                               (DI.DynamicInfo []
                                                                                  [DI.genTerm x30,
                                                                                   DI.genTerm x31]))
                                                                            (Prelude.return
                                                                               (Curry.DebugModule.Prelude.Cons
                                                                                  x30
                                                                                  x31))
                                                                DM.constructorHook
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "Interactive" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x32,
                                                                         DI.genTerm x33]))
                                                                  (Prelude.return
                                                                     (Curry.DebugModule.Prelude.Cons
                                                                        x32
                                                                        x33))
                                                      DM.constructorHook
                                                        (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x34, DI.genTerm x35]))
                                                        (Prelude.return
                                                           (Curry.DebugModule.Prelude.Cons x34 x35))
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x36, DI.genTerm x37]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Cons x36 x37))
                                  DM.funcCallHook "putStrLn"
                                    (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x38]))
                                    (Curry.DebugModule.Prelude.strict_putStrLn x38)))
                    Curry.DebugModule.Prelude.Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Interactive" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x56 <- do x39 <- Prelude.return x2
                                            DM.funcCallHook "printTerm"
                                              (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x39]))
                                              (strict_printTerm x39)
                                  x57 <- do x54 <- do x50 <- do x48 <- DM.litHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Interactive"
                                                                               0)
                                                                            (DI.DynamicInfo [] []))
                                                                         (Prelude.return
                                                                            (Curry.DebugModule.Prelude.Char
                                                                               'M'))
                                                                x49 <- do x46 <- DM.litHook
                                                                                   (DI.DebugInfo
                                                                                      (DI.SrcID
                                                                                         "Interactive"
                                                                                         0)
                                                                                      (DI.DynamicInfo
                                                                                         []
                                                                                         []))
                                                                                   (Prelude.return
                                                                                      (Curry.DebugModule.Prelude.Char
                                                                                         'o'))
                                                                          x47 <- do x44 <- DM.litHook
                                                                                             (DI.DebugInfo
                                                                                                (DI.SrcID
                                                                                                   "Interactive"
                                                                                                   0)
                                                                                                (DI.DynamicInfo
                                                                                                   []
                                                                                                   []))
                                                                                             (Prelude.return
                                                                                                (Curry.DebugModule.Prelude.Char
                                                                                                   'r'))
                                                                                    x45 <- do x42 <- DM.litHook
                                                                                                       (DI.DebugInfo
                                                                                                          (DI.SrcID
                                                                                                             "Interactive"
                                                                                                             0)
                                                                                                          (DI.DynamicInfo
                                                                                                             []
                                                                                                             []))
                                                                                                       (Prelude.return
                                                                                                          (Curry.DebugModule.Prelude.Char
                                                                                                             'e'))
                                                                                              x43 <- do x40 <- DM.litHook
                                                                                                                 (DI.DebugInfo
                                                                                                                    (DI.SrcID
                                                                                                                       "Interactive"
                                                                                                                       0)
                                                                                                                    (DI.DynamicInfo
                                                                                                                       []
                                                                                                                       []))
                                                                                                                 (Prelude.return
                                                                                                                    (Curry.DebugModule.Prelude.Char
                                                                                                                       '?'))
                                                                                                        x41 <- DM.constructorHook
                                                                                                                 (DI.DebugInfo
                                                                                                                    (DI.SrcID
                                                                                                                       "Interactive"
                                                                                                                       0)
                                                                                                                    (DI.DynamicInfo
                                                                                                                       []
                                                                                                                       []))
                                                                                                                 (Prelude.return
                                                                                                                    Curry.DebugModule.Prelude.Nil)
                                                                                                        DM.constructorHook
                                                                                                          (DI.DebugInfo
                                                                                                             (DI.SrcID
                                                                                                                "Interactive"
                                                                                                                0)
                                                                                                             (DI.DynamicInfo
                                                                                                                []
                                                                                                                [DI.genTerm
                                                                                                                   x40,
                                                                                                                 DI.genTerm
                                                                                                                   x41]))
                                                                                                          (Prelude.return
                                                                                                             (Curry.DebugModule.Prelude.Cons
                                                                                                                x40
                                                                                                                x41))
                                                                                              DM.constructorHook
                                                                                                (DI.DebugInfo
                                                                                                   (DI.SrcID
                                                                                                      "Interactive"
                                                                                                      0)
                                                                                                   (DI.DynamicInfo
                                                                                                      []
                                                                                                      [DI.genTerm
                                                                                                         x42,
                                                                                                       DI.genTerm
                                                                                                         x43]))
                                                                                                (Prelude.return
                                                                                                   (Curry.DebugModule.Prelude.Cons
                                                                                                      x42
                                                                                                      x43))
                                                                                    DM.constructorHook
                                                                                      (DI.DebugInfo
                                                                                         (DI.SrcID
                                                                                            "Interactive"
                                                                                            0)
                                                                                         (DI.DynamicInfo
                                                                                            []
                                                                                            [DI.genTerm
                                                                                               x44,
                                                                                             DI.genTerm
                                                                                               x45]))
                                                                                      (Prelude.return
                                                                                         (Curry.DebugModule.Prelude.Cons
                                                                                            x44
                                                                                            x45))
                                                                          DM.constructorHook
                                                                            (DI.DebugInfo
                                                                               (DI.SrcID
                                                                                  "Interactive"
                                                                                  0)
                                                                               (DI.DynamicInfo []
                                                                                  [DI.genTerm x46,
                                                                                   DI.genTerm x47]))
                                                                            (Prelude.return
                                                                               (Curry.DebugModule.Prelude.Cons
                                                                                  x46
                                                                                  x47))
                                                                DM.constructorHook
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "Interactive" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x48,
                                                                         DI.genTerm x49]))
                                                                  (Prelude.return
                                                                     (Curry.DebugModule.Prelude.Cons
                                                                        x48
                                                                        x49))
                                                      DM.funcCallHook "putStrLn"
                                                        (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                                           (DI.DynamicInfo [] [DI.genTerm x50]))
                                                        (Curry.DebugModule.Prelude.strict_putStrLn
                                                           x50)
                                            x55 <- do x52 <- DM.funcCallHook "getLine"
                                                               (DI.DebugInfo
                                                                  (DI.SrcID "Interactive" 0)
                                                                  (DI.DynamicInfo [] []))
                                                               Curry.DebugModule.Prelude.strict_getLine
                                                      x53 <- do x51 <- Prelude.return x3
                                                                Prelude.return
                                                                  (PC.partCall1
                                                                     (x'xterm_strict_interactiveSols46_35lambda2
                                                                        [DI.genTerm x51])
                                                                     (x'xstrict_interactiveSols46_35lambda2
                                                                        x51))
                                                      DM.funcCallHook ">>="
                                                        (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x52, DI.genTerm x53]))
                                                        (Curry.DebugModule.Prelude.op_GtGtEq x52
                                                           x53)
                                            DM.funcCallHook ">>"
                                              (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x54, DI.genTerm x55]))
                                              (Curry.DebugModule.Prelude.op_GtGt x54 x55)
                                  DM.funcCallHook ">>"
                                    (DI.DebugInfo (DI.SrcID "Interactive" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x56, DI.genTerm x57]))
                                    (Curry.DebugModule.Prelude.op_GtGt x56 x57)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Interactive" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x58])))
                           strict__case_3
                           x58)))
term_strict__case_3 x1
  = DI.Term "_case_3" (DI.SrcID "Interactive" 0) x1
hook_strict_printTerm x1 value
  = DM.eval
      (DM.funcDeclHook "printTerm"
         (DI.DebugInfo (DI.SrcID "Interactive" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_printTerm x1
  = DI.Term "printTerm" (DI.SrcID "Interactive" 0) x1