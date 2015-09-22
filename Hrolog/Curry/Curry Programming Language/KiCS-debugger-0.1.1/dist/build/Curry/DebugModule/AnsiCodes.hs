{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Curry.DebugModule.AnsiCodes where
import qualified Prelude
import qualified Curry.Debugger.DebugMonad as DM
import qualified Curry.Debugger.DebugInfo as DI
import qualified Curry.Debugger.PartCalls as PC
import qualified Data.Generics
import qualified Curry.DebugModule.Char
import qualified Curry.DebugModule.List
import qualified Curry.DebugModule.Prelude
 
strict_esc :: (DM.DM dm) => dm Curry.DebugModule.Prelude.Char
strict_esc
  = DM.eval
      (DM.funcDeclHook "esc"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.I
                              (Curry.DebugModule.Prelude.I
                                 (Curry.DebugModule.Prelude.O
                                    (Curry.DebugModule.Prelude.I Curry.DebugModule.Prelude.IHi))))))
             DM.funcCallHook "chr"
               (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                  (DI.DynamicInfo [] [DI.genTerm x0]))
               (Curry.DebugModule.Prelude.strict_chr x0)))
 
strict_cmd ::
           (DM.DM dm) =>
             Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
               dm (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
strict_cmd x1
  = DM.eval
      (DM.funcDeclHook "cmd"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x6 <- DM.funcCallHook "esc"
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     strict_esc
             x7 <- do x4 <- do x2 <- DM.litHook
                                       (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                          (DI.DynamicInfo [] []))
                                       (Prelude.return (Curry.DebugModule.Prelude.Char '['))
                               x3 <- DM.constructorHook
                                       (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                          (DI.DynamicInfo [] []))
                                       (Prelude.return Curry.DebugModule.Prelude.Nil)
                               DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                    (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                                 (Prelude.return (Curry.DebugModule.Prelude.Cons x2 x3))
                      x5 <- Prelude.return x1
                      DM.funcCallHook "++"
                        (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                           (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                        (Curry.DebugModule.Prelude.op_PlusPlus x4 x5)
             DM.constructorHook
               (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                  (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
               (Prelude.return (Curry.DebugModule.Prelude.Cons x6 x7))))
term_strict_cmd x1 = DI.Term "cmd" (DI.SrcID "AnsiCodes" 0) x1
 
strict_cursorPos ::
                 (DM.DM dm, DI.GenTerm a, DI.GenTerm b) =>
                   a ->
                     b ->
                       dm (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
strict_cursorPos x1 x2
  = DM.eval
      (DM.funcDeclHook "cursorPos"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x15 <- do x13 <- do x3 <- Prelude.return x1
                                 DM.funcCallHook "show"
                                   (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                      (DI.DynamicInfo [] [DI.genTerm x3]))
                                   (Curry.DebugModule.Prelude.strict_show x3)
                       x14 <- do x11 <- do x4 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return
                                                      (Curry.DebugModule.Prelude.Char ';'))
                                           x5 <- DM.constructorHook
                                                   (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return Curry.DebugModule.Prelude.Nil)
                                           DM.constructorHook
                                             (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                             (Prelude.return (Curry.DebugModule.Prelude.Cons x4 x5))
                                 x12 <- do x9 <- do x6 <- Prelude.return x2
                                                    DM.funcCallHook "show"
                                                      (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                         (DI.DynamicInfo [] [DI.genTerm x6]))
                                                      (Curry.DebugModule.Prelude.strict_show x6)
                                           x10 <- do x7 <- DM.litHook
                                                             (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                                (DI.DynamicInfo [] []))
                                                             (Prelude.return
                                                                (Curry.DebugModule.Prelude.Char
                                                                   'H'))
                                                     x8 <- DM.constructorHook
                                                             (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                                (DI.DynamicInfo [] []))
                                                             (Prelude.return
                                                                Curry.DebugModule.Prelude.Nil)
                                                     DM.constructorHook
                                                       (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                          (DI.DynamicInfo []
                                                             [DI.genTerm x7, DI.genTerm x8]))
                                                       (Prelude.return
                                                          (Curry.DebugModule.Prelude.Cons x7 x8))
                                           DM.funcCallHook "++"
                                             (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
                                             (Curry.DebugModule.Prelude.op_PlusPlus x9 x10)
                                 DM.funcCallHook "++"
                                   (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                      (DI.DynamicInfo [] [DI.genTerm x11, DI.genTerm x12]))
                                   (Curry.DebugModule.Prelude.op_PlusPlus x11 x12)
                       DM.funcCallHook "++"
                         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                            (DI.DynamicInfo [] [DI.genTerm x13, DI.genTerm x14]))
                         (Curry.DebugModule.Prelude.op_PlusPlus x13 x14)
             DM.funcCallHook "cmd"
               (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                  (DI.DynamicInfo [] [DI.genTerm x15]))
               (strict_cmd x15)))
term_strict_cursorPos x1
  = DI.Term "cursorPos" (DI.SrcID "AnsiCodes" 0) x1
 
strict_cursorHome ::
                  (DM.DM dm) =>
                    dm (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
strict_cursorHome
  = DM.eval
      (DM.funcDeclHook "cursorHome"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x2 <- do x0 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                              (Prelude.return (Curry.DebugModule.Prelude.Char 'H'))
                      x1 <- DM.constructorHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Nil)
                      DM.constructorHook
                        (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                           (DI.DynamicInfo [] [DI.genTerm x0, DI.genTerm x1]))
                        (Prelude.return (Curry.DebugModule.Prelude.Cons x0 x1))
             DM.funcCallHook "cmd"
               (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict_cmd x2)))
 
strict_moveCursor ::
                  (DM.DM dm) =>
                    Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                      Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                        dm (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
strict_moveCursor x1 x2
  = DM.eval
      (DM.funcDeclHook "moveCursor"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x6 <- do x4 <- do x3 <- Prelude.return x2
                               DM.funcCallHook "show"
                                 (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                    (DI.DynamicInfo [] [DI.genTerm x3]))
                                 (Curry.DebugModule.Prelude.strict_show x3)
                      x5 <- Prelude.return x1
                      DM.funcCallHook "++"
                        (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                           (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                        (Curry.DebugModule.Prelude.op_PlusPlus x4 x5)
             DM.funcCallHook "cmd"
               (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                  (DI.DynamicInfo [] [DI.genTerm x6]))
               (strict_cmd x6)))
term_strict_moveCursor x1
  = DI.Term "moveCursor" (DI.SrcID "AnsiCodes" 0) x1
 
strict_cursorUp ::
                (DM.DM dm) =>
                  dm
                    (DM.Func dm
                       (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                       (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_cursorUp
  = DM.eval
      (DM.funcDeclHook "cursorUp"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x2 <- do x0 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                              (Prelude.return (Curry.DebugModule.Prelude.Char 'A'))
                      x1 <- DM.constructorHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Nil)
                      DM.constructorHook
                        (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                           (DI.DynamicInfo [] [DI.genTerm x0, DI.genTerm x1]))
                        (Prelude.return (Curry.DebugModule.Prelude.Cons x0 x1))
             Prelude.return
               (PC.partCall1 (term_strict_moveCursor [DI.genTerm x2])
                  (strict_moveCursor x2))))
 
strict_cursorDown ::
                  (DM.DM dm) =>
                    dm
                      (DM.Func dm
                         (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                         (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_cursorDown
  = DM.eval
      (DM.funcDeclHook "cursorDown"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x2 <- do x0 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                              (Prelude.return (Curry.DebugModule.Prelude.Char 'B'))
                      x1 <- DM.constructorHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Nil)
                      DM.constructorHook
                        (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                           (DI.DynamicInfo [] [DI.genTerm x0, DI.genTerm x1]))
                        (Prelude.return (Curry.DebugModule.Prelude.Cons x0 x1))
             Prelude.return
               (PC.partCall1 (term_strict_moveCursor [DI.genTerm x2])
                  (strict_moveCursor x2))))
 
strict_cursorFwd ::
                 (DM.DM dm) =>
                   dm
                     (DM.Func dm
                        (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                        (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_cursorFwd
  = DM.eval
      (DM.funcDeclHook "cursorFwd"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x2 <- do x0 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                              (Prelude.return (Curry.DebugModule.Prelude.Char 'C'))
                      x1 <- DM.constructorHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Nil)
                      DM.constructorHook
                        (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                           (DI.DynamicInfo [] [DI.genTerm x0, DI.genTerm x1]))
                        (Prelude.return (Curry.DebugModule.Prelude.Cons x0 x1))
             Prelude.return
               (PC.partCall1 (term_strict_moveCursor [DI.genTerm x2])
                  (strict_moveCursor x2))))
 
strict_cursorBack ::
                  (DM.DM dm) =>
                    dm
                      (DM.Func dm
                         (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                         (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_cursorBack
  = DM.eval
      (DM.funcDeclHook "cursorBack"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x2 <- do x0 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                              (Prelude.return (Curry.DebugModule.Prelude.Char 'D'))
                      x1 <- DM.constructorHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Nil)
                      DM.constructorHook
                        (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                           (DI.DynamicInfo [] [DI.genTerm x0, DI.genTerm x1]))
                        (Prelude.return (Curry.DebugModule.Prelude.Cons x0 x1))
             Prelude.return
               (PC.partCall1 (term_strict_moveCursor [DI.genTerm x2])
                  (strict_moveCursor x2))))
 
strict_saveCursor ::
                  (DM.DM dm) =>
                    dm (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
strict_saveCursor
  = DM.eval
      (DM.funcDeclHook "saveCursor"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x2 <- do x0 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                              (Prelude.return (Curry.DebugModule.Prelude.Char 's'))
                      x1 <- DM.constructorHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Nil)
                      DM.constructorHook
                        (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                           (DI.DynamicInfo [] [DI.genTerm x0, DI.genTerm x1]))
                        (Prelude.return (Curry.DebugModule.Prelude.Cons x0 x1))
             DM.funcCallHook "cmd"
               (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict_cmd x2)))
 
strict_restoreCursor ::
                     (DM.DM dm) =>
                       dm (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
strict_restoreCursor
  = DM.eval
      (DM.funcDeclHook "restoreCursor"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x2 <- do x0 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                              (Prelude.return (Curry.DebugModule.Prelude.Char 'u'))
                      x1 <- DM.constructorHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Nil)
                      DM.constructorHook
                        (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                           (DI.DynamicInfo [] [DI.genTerm x0, DI.genTerm x1]))
                        (Prelude.return (Curry.DebugModule.Prelude.Cons x0 x1))
             DM.funcCallHook "cmd"
               (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict_cmd x2)))
 
strict_clear ::
             (DM.DM dm) =>
               dm (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
strict_clear
  = DM.eval
      (DM.funcDeclHook "clear"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x4 <- do x2 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                              (Prelude.return (Curry.DebugModule.Prelude.Char '2'))
                      x3 <- do x0 <- DM.litHook
                                       (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                          (DI.DynamicInfo [] []))
                                       (Prelude.return (Curry.DebugModule.Prelude.Char 'J'))
                               x1 <- DM.constructorHook
                                       (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                          (DI.DynamicInfo [] []))
                                       (Prelude.return Curry.DebugModule.Prelude.Nil)
                               DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                    (DI.DynamicInfo [] [DI.genTerm x0, DI.genTerm x1]))
                                 (Prelude.return (Curry.DebugModule.Prelude.Cons x0 x1))
                      DM.constructorHook
                        (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                        (Prelude.return (Curry.DebugModule.Prelude.Cons x2 x3))
             DM.funcCallHook "cmd"
               (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (strict_cmd x4)))
 
strict_eraseLine ::
                 (DM.DM dm) =>
                   dm (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
strict_eraseLine
  = DM.eval
      (DM.funcDeclHook "eraseLine"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x2 <- do x0 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                              (Prelude.return (Curry.DebugModule.Prelude.Char 'K'))
                      x1 <- DM.constructorHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Nil)
                      DM.constructorHook
                        (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                           (DI.DynamicInfo [] [DI.genTerm x0, DI.genTerm x1]))
                        (Prelude.return (Curry.DebugModule.Prelude.Cons x0 x1))
             DM.funcCallHook "cmd"
               (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict_cmd x2)))
 
strict_mode ::
            (DM.DM dm, DI.GenTerm a) =>
              a ->
                Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                  dm (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
strict_mode x1 x2
  = DM.eval
      (DM.funcDeclHook "mode"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (DM.letHook
            (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
            (do x3 <- do x8 <- do x6 <- DM.litHook
                                          (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return (Curry.DebugModule.Prelude.Char '0'))
                                  x7 <- do x4 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return
                                                      (Curry.DebugModule.Prelude.Char 'm'))
                                           x5 <- DM.constructorHook
                                                   (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return Curry.DebugModule.Prelude.Nil)
                                           DM.constructorHook
                                             (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                             (Prelude.return (Curry.DebugModule.Prelude.Cons x4 x5))
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Cons x6 x7))
                         DM.funcCallHook "cmd"
                           (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                              (DI.DynamicInfo [] [DI.genTerm x8]))
                           (strict_cmd x8)
                DM.eval
                  (do x24 <- do x22 <- do x9 <- Prelude.return x1
                                          DM.funcCallHook "show"
                                            (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                               (DI.DynamicInfo [] [DI.genTerm x9]))
                                            (Curry.DebugModule.Prelude.strict_show x9)
                                x23 <- do x20 <- do x10 <- DM.litHook
                                                             (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                                (DI.DynamicInfo [] []))
                                                             (Prelude.return
                                                                (Curry.DebugModule.Prelude.Char
                                                                   'm'))
                                                    x11 <- DM.constructorHook
                                                             (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                                (DI.DynamicInfo [] []))
                                                             (Prelude.return
                                                                Curry.DebugModule.Prelude.Nil)
                                                    DM.constructorHook
                                                      (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                         (DI.DynamicInfo []
                                                            [DI.genTerm x10, DI.genTerm x11]))
                                                      (Prelude.return
                                                         (Curry.DebugModule.Prelude.Cons x10 x11))
                                          x21 <- do x18 <- Prelude.return x2
                                                    x19 <- do x15 <- Prelude.return x2
                                                              x16 <- Prelude.return x3
                                                              x17 <- do x13 <- do x12 <- Prelude.return
                                                                                           x3
                                                                                  DM.funcCallHook
                                                                                    "isSuffixOf"
                                                                                    (DI.DebugInfo
                                                                                       (DI.SrcID
                                                                                          "AnsiCodes"
                                                                                          0)
                                                                                       (DI.DynamicInfo
                                                                                          []
                                                                                          [DI.genTerm
                                                                                             x12]))
                                                                                    (strict_isSuffixOf
                                                                                       x12)
                                                                        x14 <- Prelude.return x2
                                                                        DM.funcCallHook "apply"
                                                                          (DI.DebugInfo
                                                                             (DI.SrcID "AnsiCodes"
                                                                                0)
                                                                             (DI.DynamicInfo []
                                                                                [DI.genTerm x13,
                                                                                 DI.genTerm x14]))
                                                                          (Curry.DebugModule.Prelude.strict_apply
                                                                             x13
                                                                             x14)
                                                              DM.funcCallHook "_case_4"
                                                                (DI.DebugInfo
                                                                   (DI.SrcID "AnsiCodes" 0)
                                                                   (DI.DynamicInfo []
                                                                      [DI.genTerm x15,
                                                                       DI.genTerm x16,
                                                                       DI.genTerm x17]))
                                                                (strict__case_4 x15 x16 x17)
                                                    DM.funcCallHook "++"
                                                      (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                         (DI.DynamicInfo []
                                                            [DI.genTerm x18, DI.genTerm x19]))
                                                      (Curry.DebugModule.Prelude.op_PlusPlus x18
                                                         x19)
                                          DM.funcCallHook "++"
                                            (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                               (DI.DynamicInfo [] [DI.genTerm x20, DI.genTerm x21]))
                                            (Curry.DebugModule.Prelude.op_PlusPlus x20 x21)
                                DM.funcCallHook "++"
                                  (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x22, DI.genTerm x23]))
                                  (Curry.DebugModule.Prelude.op_PlusPlus x22 x23)
                      DM.funcCallHook "cmd"
                        (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                           (DI.DynamicInfo [] [DI.genTerm x24]))
                        (strict_cmd x24)))))
term_strict_mode x1 = DI.Term "mode" (DI.SrcID "AnsiCodes" 0) x1
 
strict_isSuffixOf ::
                  (DM.DM dm, DI.GenTerm a) =>
                    Curry.DebugModule.Prelude.List a ->
                      dm
                        (DM.Func dm (Curry.DebugModule.Prelude.List a)
                           Curry.DebugModule.Prelude.Bool)
strict_isSuffixOf x1
  = DM.eval
      (DM.funcDeclHook "isSuffixOf"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x5 <- do x4 <- do x2 <- DM.funcCallHook "reverse"
                                       (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                          (DI.DynamicInfo [] []))
                                       Curry.DebugModule.Prelude.strict_reverse
                               x3 <- Prelude.return x1
                               DM.funcCallHook "apply"
                                 (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                    (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                                 (Curry.DebugModule.Prelude.strict_apply x2 x3)
                      Prelude.return
                        (PC.partCall1
                           (Curry.DebugModule.List.term_strict_isPrefixOf [DI.genTerm x4])
                           (Curry.DebugModule.List.strict_isPrefixOf x4))
             x6 <- DM.funcCallHook "reverse"
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     Curry.DebugModule.Prelude.strict_reverse
             DM.funcCallHook "."
               (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (Curry.DebugModule.Prelude.op_Point x5 x6)))
term_strict_isSuffixOf x1
  = DI.Term "isSuffixOf" (DI.SrcID "AnsiCodes" 0) x1
 
strict_bold ::
            (DM.DM dm) =>
              dm
                (DM.Func dm
                   (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                   (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_bold
  = DM.eval
      (DM.funcDeclHook "bold"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos Curry.DebugModule.Prelude.IHi))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_underline ::
                 (DM.DM dm) =>
                   dm
                     (DM.Func dm
                        (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                        (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_underline
  = DM.eval
      (DM.funcDeclHook "underline"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.O
                              (Curry.DebugModule.Prelude.O Curry.DebugModule.Prelude.IHi))))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_revColors ::
                 (DM.DM dm) =>
                   dm
                     (DM.Func dm
                        (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                        (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_revColors
  = DM.eval
      (DM.funcDeclHook "revColors"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.I
                              (Curry.DebugModule.Prelude.I Curry.DebugModule.Prelude.IHi))))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_concealed ::
                 (DM.DM dm) =>
                   dm
                     (DM.Func dm
                        (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                        (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_concealed
  = DM.eval
      (DM.funcDeclHook "concealed"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.O
                              (Curry.DebugModule.Prelude.O
                                 (Curry.DebugModule.Prelude.O Curry.DebugModule.Prelude.IHi)))))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_black ::
             (DM.DM dm) =>
               dm
                 (DM.Func dm
                    (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                    (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_black
  = DM.eval
      (DM.funcDeclHook "black"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.O
                              (Curry.DebugModule.Prelude.I
                                 (Curry.DebugModule.Prelude.I
                                    (Curry.DebugModule.Prelude.I Curry.DebugModule.Prelude.IHi))))))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_red ::
           (DM.DM dm) =>
             dm
               (DM.Func dm
                  (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                  (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_red
  = DM.eval
      (DM.funcDeclHook "red"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.I
                              (Curry.DebugModule.Prelude.I
                                 (Curry.DebugModule.Prelude.I
                                    (Curry.DebugModule.Prelude.I Curry.DebugModule.Prelude.IHi))))))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_green ::
             (DM.DM dm) =>
               dm
                 (DM.Func dm
                    (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                    (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_green
  = DM.eval
      (DM.funcDeclHook "green"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.O
                              (Curry.DebugModule.Prelude.O
                                 (Curry.DebugModule.Prelude.O
                                    (Curry.DebugModule.Prelude.O
                                       (Curry.DebugModule.Prelude.O
                                          Curry.DebugModule.Prelude.IHi)))))))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_yellow ::
              (DM.DM dm) =>
                dm
                  (DM.Func dm
                     (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                     (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_yellow
  = DM.eval
      (DM.funcDeclHook "yellow"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.I
                              (Curry.DebugModule.Prelude.O
                                 (Curry.DebugModule.Prelude.O
                                    (Curry.DebugModule.Prelude.O
                                       (Curry.DebugModule.Prelude.O
                                          Curry.DebugModule.Prelude.IHi)))))))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_blue ::
            (DM.DM dm) =>
              dm
                (DM.Func dm
                   (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                   (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_blue
  = DM.eval
      (DM.funcDeclHook "blue"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.O
                              (Curry.DebugModule.Prelude.I
                                 (Curry.DebugModule.Prelude.O
                                    (Curry.DebugModule.Prelude.O
                                       (Curry.DebugModule.Prelude.O
                                          Curry.DebugModule.Prelude.IHi)))))))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_magenta ::
               (DM.DM dm) =>
                 dm
                   (DM.Func dm
                      (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                      (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_magenta
  = DM.eval
      (DM.funcDeclHook "magenta"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.I
                              (Curry.DebugModule.Prelude.I
                                 (Curry.DebugModule.Prelude.O
                                    (Curry.DebugModule.Prelude.O
                                       (Curry.DebugModule.Prelude.O
                                          Curry.DebugModule.Prelude.IHi)))))))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_cyan ::
            (DM.DM dm) =>
              dm
                (DM.Func dm
                   (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                   (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_cyan
  = DM.eval
      (DM.funcDeclHook "cyan"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.O
                              (Curry.DebugModule.Prelude.O
                                 (Curry.DebugModule.Prelude.I
                                    (Curry.DebugModule.Prelude.O
                                       (Curry.DebugModule.Prelude.O
                                          Curry.DebugModule.Prelude.IHi)))))))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_white ::
             (DM.DM dm) =>
               dm
                 (DM.Func dm
                    (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                    (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_white
  = DM.eval
      (DM.funcDeclHook "white"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.I
                              (Curry.DebugModule.Prelude.O
                                 (Curry.DebugModule.Prelude.I
                                    (Curry.DebugModule.Prelude.O
                                       (Curry.DebugModule.Prelude.O
                                          Curry.DebugModule.Prelude.IHi)))))))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_bgBlack ::
               (DM.DM dm) =>
                 dm
                   (DM.Func dm
                      (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                      (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_bgBlack
  = DM.eval
      (DM.funcDeclHook "bgBlack"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.O
                              (Curry.DebugModule.Prelude.O
                                 (Curry.DebugModule.Prelude.O
                                    (Curry.DebugModule.Prelude.I
                                       (Curry.DebugModule.Prelude.O
                                          Curry.DebugModule.Prelude.IHi)))))))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_bgRed ::
             (DM.DM dm) =>
               dm
                 (DM.Func dm
                    (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                    (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_bgRed
  = DM.eval
      (DM.funcDeclHook "bgRed"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.I
                              (Curry.DebugModule.Prelude.O
                                 (Curry.DebugModule.Prelude.O
                                    (Curry.DebugModule.Prelude.I
                                       (Curry.DebugModule.Prelude.O
                                          Curry.DebugModule.Prelude.IHi)))))))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_bgGreen ::
               (DM.DM dm) =>
                 dm
                   (DM.Func dm
                      (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                      (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_bgGreen
  = DM.eval
      (DM.funcDeclHook "bgGreen"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.O
                              (Curry.DebugModule.Prelude.I
                                 (Curry.DebugModule.Prelude.O
                                    (Curry.DebugModule.Prelude.I
                                       (Curry.DebugModule.Prelude.O
                                          Curry.DebugModule.Prelude.IHi)))))))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_bgYellow ::
                (DM.DM dm) =>
                  dm
                    (DM.Func dm
                       (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                       (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_bgYellow
  = DM.eval
      (DM.funcDeclHook "bgYellow"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.I
                              (Curry.DebugModule.Prelude.I
                                 (Curry.DebugModule.Prelude.O
                                    (Curry.DebugModule.Prelude.I
                                       (Curry.DebugModule.Prelude.O
                                          Curry.DebugModule.Prelude.IHi)))))))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_bgBlue ::
              (DM.DM dm) =>
                dm
                  (DM.Func dm
                     (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                     (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_bgBlue
  = DM.eval
      (DM.funcDeclHook "bgBlue"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.O
                              (Curry.DebugModule.Prelude.O
                                 (Curry.DebugModule.Prelude.I
                                    (Curry.DebugModule.Prelude.I
                                       (Curry.DebugModule.Prelude.O
                                          Curry.DebugModule.Prelude.IHi)))))))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_bgMagenta ::
                 (DM.DM dm) =>
                   dm
                     (DM.Func dm
                        (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                        (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_bgMagenta
  = DM.eval
      (DM.funcDeclHook "bgMagenta"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.I
                              (Curry.DebugModule.Prelude.O
                                 (Curry.DebugModule.Prelude.I
                                    (Curry.DebugModule.Prelude.I
                                       (Curry.DebugModule.Prelude.O
                                          Curry.DebugModule.Prelude.IHi)))))))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_bgCyan ::
              (DM.DM dm) =>
                dm
                  (DM.Func dm
                     (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                     (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_bgCyan
  = DM.eval
      (DM.funcDeclHook "bgCyan"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.O
                              (Curry.DebugModule.Prelude.I
                                 (Curry.DebugModule.Prelude.I
                                    (Curry.DebugModule.Prelude.I
                                       (Curry.DebugModule.Prelude.O
                                          Curry.DebugModule.Prelude.IHi)))))))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_bgWhite ::
               (DM.DM dm) =>
                 dm
                   (DM.Func dm
                      (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char)
                      (Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char))
strict_bgWhite
  = DM.eval
      (DM.funcDeclHook "bgWhite"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.I
                              (Curry.DebugModule.Prelude.I
                                 (Curry.DebugModule.Prelude.I
                                    (Curry.DebugModule.Prelude.I
                                       (Curry.DebugModule.Prelude.O
                                          Curry.DebugModule.Prelude.IHi)))))))
             Prelude.return
               (PC.partCall1 (term_strict_mode [DI.genTerm x0])
                  (strict_mode x0))))
 
strict_ansiLength ::
                  (DM.DM dm) =>
                    Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                      dm Curry.DebugModule.Prelude.Int
strict_ansiLength x1
  = DM.eval
      (DM.funcDeclHook "ansiLength"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- Prelude.return x1
             x4 <- do x2 <- Prelude.return x1
                      DM.funcCallHook "length"
                        (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2]))
                        (Curry.DebugModule.Prelude.strict_length x2)
             DM.funcCallHook "ansiLength.aux.95"
               (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (x'xstrict_ansiLength46aux4695 x3 x4)))
term_strict_ansiLength x1
  = DI.Term "ansiLength" (DI.SrcID "AnsiCodes" 0) x1
 
x'xstrict_ansiLength46aux4695 ::
                              (DM.DM dm) =>
                                Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                                  Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
x'xstrict_ansiLength46aux4695 x1 x2
  = DM.eval
      (DM.funcDeclHook "ansiLength.aux.95"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_3"
               (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_3 x3 x4)))
x'xterm_strict_ansiLength46aux4695 x1
  = DI.Term "ansiLength.aux.95" (DI.SrcID "AnsiCodes" 0) x1
strict__case_3 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_3"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x16 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "AnsiCodes" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x16]))
               (case x16 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    Curry.DebugModule.Prelude.Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- Prelude.return x2
                                  x13 <- Prelude.return x3
                                  x14 <- Prelude.return x4
                                  x15 <- do x10 <- do x5 <- Prelude.return x3
                                                      x6 <- DM.funcCallHook "esc"
                                                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                                 (DI.DynamicInfo [] []))
                                                              strict_esc
                                                      DM.funcCallHook "=="
                                                        (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x5, DI.genTerm x6]))
                                                        (Curry.DebugModule.Prelude.op_EqEq x5 x6)
                                            x11 <- do x9 <- do x7 <- Prelude.return x4
                                                               x8 <- DM.litHook
                                                                       (DI.DebugInfo
                                                                          (DI.SrcID "AnsiCodes" 0)
                                                                          (DI.DynamicInfo [] []))
                                                                       (Prelude.return
                                                                          (Curry.DebugModule.Prelude.Pos
                                                                             (Curry.DebugModule.Prelude.O
                                                                                Curry.DebugModule.Prelude.IHi)))
                                                               DM.funcCallHook "!!"
                                                                 (DI.DebugInfo
                                                                    (DI.SrcID "AnsiCodes" 0)
                                                                    (DI.DynamicInfo []
                                                                       [DI.genTerm x7,
                                                                        DI.genTerm x8]))
                                                                 (Curry.DebugModule.Prelude.op_EMarkEMark
                                                                    x7
                                                                    x8)
                                                      DM.funcCallHook "isDigit"
                                                        (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                           (DI.DynamicInfo [] [DI.genTerm x9]))
                                                        (Curry.DebugModule.Char.strict_isDigit x9)
                                            DM.funcCallHook "&&"
                                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x10, DI.genTerm x11]))
                                              (Curry.DebugModule.Prelude.op_AndAnd x10 x11)
                                  DM.funcCallHook "_case_2"
                                    (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x12, DI.genTerm x13, DI.genTerm x14,
                                           DI.genTerm x15]))
                                    (strict__case_2 x12 x13 x14 x15)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x16])))
                           (strict__case_3 x2)
                           x16)))
term_strict__case_3 x1
  = DI.Term "_case_3" (DI.SrcID "AnsiCodes" 0) x1
strict__case_2 x2 x3 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_2"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
            (DI.DynamicInfo []
               [DI.genTerm x2, DI.genTerm x3, DI.genTerm x4, DI.genTerm x5]))
         (do x20 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                  (DI.DynamicInfo [] [DI.genTerm x20]))
               (case x20 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- do x9 <- do x8 <- do x7 <- do x6 <- Prelude.return x4
                                                                       DM.funcCallHook "tail"
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "AnsiCodes" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x6]))
                                                                         (Curry.DebugModule.Prelude.strict_tail
                                                                            x6)
                                                              DM.funcCallHook "tail"
                                                                (DI.DebugInfo
                                                                   (DI.SrcID "AnsiCodes" 0)
                                                                   (DI.DynamicInfo []
                                                                      [DI.genTerm x7]))
                                                                (Curry.DebugModule.Prelude.strict_tail
                                                                   x7)
                                                     DM.funcCallHook "tail"
                                                       (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                          (DI.DynamicInfo [] [DI.genTerm x8]))
                                                       (Curry.DebugModule.Prelude.strict_tail x8)
                                            DM.funcCallHook "tail"
                                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x9]))
                                              (Curry.DebugModule.Prelude.strict_tail x9)
                                  x13 <- do x10 <- Prelude.return x2
                                            x11 <- DM.litHook
                                                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                        (DI.DynamicInfo [] []))
                                                     (Prelude.return
                                                        (Curry.DebugModule.Prelude.Pos
                                                           (Curry.DebugModule.Prelude.I
                                                              (Curry.DebugModule.Prelude.O
                                                                 Curry.DebugModule.Prelude.IHi))))
                                            DM.funcCallHook "-"
                                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x10, DI.genTerm x11]))
                                              (Curry.DebugModule.Prelude.op_Minus x10 x11)
                                  DM.funcCallHook "ansiLength.aux.95"
                                    (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x12, DI.genTerm x13]))
                                    (x'xstrict_ansiLength46aux4695 x12 x13)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x16 <- Prelude.return x2
                                  x17 <- Prelude.return x3
                                  x18 <- Prelude.return x4
                                  x19 <- do x14 <- Prelude.return x3
                                            x15 <- DM.funcCallHook "esc"
                                                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                        (DI.DynamicInfo [] []))
                                                     strict_esc
                                            DM.funcCallHook "=="
                                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x14, DI.genTerm x15]))
                                              (Curry.DebugModule.Prelude.op_EqEq x14 x15)
                                  DM.funcCallHook "_case_1"
                                    (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x16, DI.genTerm x17, DI.genTerm x18,
                                           DI.genTerm x19]))
                                    (strict__case_1 x16 x17 x18 x19)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x20])))
                           (strict__case_2 x2 x3 x4)
                           x20)))
term_strict__case_2 x1
  = DI.Term "_case_2" (DI.SrcID "AnsiCodes" 0) x1
strict__case_1 x2 x3 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_1"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
            (DI.DynamicInfo []
               [DI.genTerm x2, DI.genTerm x3, DI.genTerm x4, DI.genTerm x5]))
         (do x16 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                  (DI.DynamicInfo [] [DI.genTerm x16]))
               (case x16 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- do x8 <- do x7 <- do x6 <- Prelude.return x4
                                                              DM.funcCallHook "tail"
                                                                (DI.DebugInfo
                                                                   (DI.SrcID "AnsiCodes" 0)
                                                                   (DI.DynamicInfo []
                                                                      [DI.genTerm x6]))
                                                                (Curry.DebugModule.Prelude.strict_tail
                                                                   x6)
                                                     DM.funcCallHook "tail"
                                                       (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                          (DI.DynamicInfo [] [DI.genTerm x7]))
                                                       (Curry.DebugModule.Prelude.strict_tail x7)
                                            DM.funcCallHook "tail"
                                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x8]))
                                              (Curry.DebugModule.Prelude.strict_tail x8)
                                  x12 <- do x9 <- Prelude.return x2
                                            x10 <- DM.litHook
                                                     (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                        (DI.DynamicInfo [] []))
                                                     (Prelude.return
                                                        (Curry.DebugModule.Prelude.Pos
                                                           (Curry.DebugModule.Prelude.O
                                                              (Curry.DebugModule.Prelude.O
                                                                 Curry.DebugModule.Prelude.IHi))))
                                            DM.funcCallHook "-"
                                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x9, DI.genTerm x10]))
                                              (Curry.DebugModule.Prelude.op_Minus x9 x10)
                                  DM.funcCallHook "ansiLength.aux.95"
                                    (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11, DI.genTerm x12]))
                                    (x'xstrict_ansiLength46aux4695 x11 x12)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x13 <- Prelude.return x2
                                  x14 <- Prelude.return x4
                                  x15 <- DM.funcCallHook "otherwise"
                                           (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                              (DI.DynamicInfo [] []))
                                           Curry.DebugModule.Prelude.strict_otherwise
                                  DM.funcCallHook "_case_0"
                                    (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x13, DI.genTerm x14, DI.genTerm x15]))
                                    (strict__case_0 x13 x14 x15)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x16])))
                           (strict__case_1 x2 x3 x4)
                           x16)))
term_strict__case_1 x1
  = DI.Term "_case_1" (DI.SrcID "AnsiCodes" 0) x1
strict__case_0 x2 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_0"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x4, DI.genTerm x5]))
         (do x8 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x4
                                  x7 <- Prelude.return x2
                                  DM.funcCallHook "ansiLength.aux.95"
                                    (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (x'xstrict_ansiLength46aux4695 x6 x7)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                                 Curry.DebugModule.Prelude.strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_0 x2 x4)
                           x8)))
term_strict__case_0 x1
  = DI.Term "_case_0" (DI.SrcID "AnsiCodes" 0) x1
strict__case_4 x2 x3 x4
  = DM.eval
      (DM.funcDeclHook "_case_4"
         (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3, DI.genTerm x4]))
         (do x5 <- Prelude.return x4
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5]))
               (case x5 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Nil)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "AnsiCodes" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "AnsiCodes" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x5])))
                           (strict__case_4 x2 x3)
                           x5)))
term_strict__case_4 x1
  = DI.Term "_case_4" (DI.SrcID "AnsiCodes" 0) x1