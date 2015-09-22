{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Curry.DebugModule.Dequeue where
import qualified Prelude
import qualified Curry.Debugger.DebugMonad as DM
import qualified Curry.Debugger.DebugInfo as DI
import qualified Curry.Debugger.PartCalls as PC
import qualified Data.Generics
import qualified Curry.DebugModule.Prelude
 
instance (DI.GenTerm a) => DI.GenTerm (Queue a) where
        genTerm (S x1 x2 x3 x4)
          = DI.Term "S" (DI.SrcID "Dequeue" 0)
              [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4]
        genTerm x1 = DM.genericTerm (DI.SrcID "Dequeue" 0) x1
 
data Queue a = QueueFail
             | QueueOr DM.OrRef [Queue a]
             | QueueUnderscore
             | S Curry.DebugModule.Prelude.Int
                 (Curry.DebugModule.Prelude.List a) Curry.DebugModule.Prelude.Int
                 (Curry.DebugModule.Prelude.List a)
             deriving (Data.Generics.Typeable, Data.Generics.Data)
 
strict_empty :: (DM.DM dm, DI.GenTerm a) => dm (Queue a)
strict_empty
  = DM.eval
      (DM.funcDeclHook "empty"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
         (do x0 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                     (Prelude.return Curry.DebugModule.Prelude.Zero)
             x1 <- DM.constructorHook
                     (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                     (Prelude.return Curry.DebugModule.Prelude.Nil)
             x2 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                     (Prelude.return Curry.DebugModule.Prelude.Zero)
             x3 <- DM.constructorHook
                     (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                     (Prelude.return Curry.DebugModule.Prelude.Nil)
             DM.constructorHook
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo []
                     [DI.genTerm x0, DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
               (Prelude.return (S x0 x1 x2 x3))))
 
strict_isEmpty ::
               (DM.DM dm, DI.GenTerm a) =>
                 Queue a -> dm Curry.DebugModule.Prelude.Bool
strict_isEmpty x1
  = DM.eval
      (DM.funcDeclHook "isEmpty"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_25"
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_25 x2)))
term_strict_isEmpty x1
  = DI.Term "isEmpty" (DI.SrcID "Dequeue" 0) x1
 
strict_deqHead :: (DM.DM dm, DI.GenTerm a) => Queue a -> dm a
strict_deqHead x1
  = DM.eval
      (DM.funcDeclHook "deqHead"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_24"
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_24 x2)))
term_strict_deqHead x1
  = DI.Term "deqHead" (DI.SrcID "Dequeue" 0) x1
 
strict_deqLast :: (DM.DM dm, DI.GenTerm a) => Queue a -> dm a
strict_deqLast x1
  = DM.eval
      (DM.funcDeclHook "deqLast"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_22"
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_22 x2)))
term_strict_deqLast x1
  = DI.Term "deqLast" (DI.SrcID "Dequeue" 0) x1
 
strict_cons ::
            (DM.DM dm, DI.GenTerm a) => a -> Queue a -> dm (Queue a)
strict_cons x1 x2
  = DM.eval
      (DM.funcDeclHook "cons"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_20"
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_20 x3 x4)))
term_strict_cons x1 = DI.Term "cons" (DI.SrcID "Dequeue" 0) x1
 
strict_deqTail ::
               (DM.DM dm, DI.GenTerm a) => Queue a -> dm (Queue a)
strict_deqTail x1
  = DM.eval
      (DM.funcDeclHook "deqTail"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_19"
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_19 x2)))
term_strict_deqTail x1
  = DI.Term "deqTail" (DI.SrcID "Dequeue" 0) x1
 
strict_snoc ::
            (DM.DM dm, DI.GenTerm a) => a -> Queue a -> dm (Queue a)
strict_snoc x1 x2
  = DM.eval
      (DM.funcDeclHook "snoc"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x1
             x4 <- Prelude.return x2
             DM.funcCallHook "_case_17"
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_17 x3 x4)))
term_strict_snoc x1 = DI.Term "snoc" (DI.SrcID "Dequeue" 0) x1
 
strict_deqInit ::
               (DM.DM dm, DI.GenTerm a) => Queue a -> dm (Queue a)
strict_deqInit x1
  = DM.eval
      (DM.funcDeclHook "deqInit"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_16"
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_16 x2)))
term_strict_deqInit x1
  = DI.Term "deqInit" (DI.SrcID "Dequeue" 0) x1
 
strict_deqReverse ::
                  (DM.DM dm, DI.GenTerm a) => Queue a -> dm (Queue a)
strict_deqReverse x1
  = DM.eval
      (DM.funcDeclHook "deqReverse"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_14"
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_14 x2)))
term_strict_deqReverse x1
  = DI.Term "deqReverse" (DI.SrcID "Dequeue" 0) x1
 
strict_check ::
             (DM.DM dm, DI.GenTerm a) =>
               Curry.DebugModule.Prelude.Int ->
                 Curry.DebugModule.Prelude.List a ->
                   Curry.DebugModule.Prelude.Int ->
                     Curry.DebugModule.Prelude.List a -> dm (Queue a)
strict_check x1 x2 x3 x4
  = DM.eval
      (DM.funcDeclHook "check"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4]))
         (DM.letHook
            (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
            (do x5 <- do x12 <- Prelude.return x1
                         x13 <- Prelude.return x3
                         DM.funcCallHook "+"
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                              (DI.DynamicInfo [] [DI.genTerm x12, DI.genTerm x13]))
                           (Curry.DebugModule.Prelude.op_Plus x12 x13)
                DM.eval
                  (DM.letHook
                     (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                     (do x6 <- do x14 <- Prelude.return x5
                                  x15 <- DM.litHook
                                           (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                              (DI.DynamicInfo [] []))
                                           (Prelude.return
                                              (Curry.DebugModule.Prelude.Pos
                                                 (Curry.DebugModule.Prelude.O
                                                    Curry.DebugModule.Prelude.IHi)))
                                  DM.funcCallHook "div"
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x14, DI.genTerm x15]))
                                    (Curry.DebugModule.Prelude.strict_div x14 x15)
                         DM.eval
                           (DM.letHook
                              (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                              (do x7 <- do x16 <- Prelude.return x5
                                           x17 <- Prelude.return x6
                                           DM.funcCallHook "-"
                                             (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                (DI.DynamicInfo []
                                                   [DI.genTerm x16, DI.genTerm x17]))
                                             (Curry.DebugModule.Prelude.op_Minus x16 x17)
                                  DM.eval
                                    (DM.letHook
                                       (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                          (DI.DynamicInfo [] []))
                                       (do x8 <- do x18 <- Prelude.return x6
                                                    x19 <- Prelude.return x2
                                                    DM.funcCallHook "splitAt"
                                                      (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                         (DI.DynamicInfo []
                                                            [DI.genTerm x18, DI.genTerm x19]))
                                                      (Curry.DebugModule.Prelude.strict_splitAt x18
                                                         x19)
                                           DM.eval
                                             (DM.letHook
                                                (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                                   (DI.DynamicInfo [] []))
                                                (do x9 <- do x20 <- Prelude.return x8
                                                             DM.funcCallHook "check._#selFP3#f'"
                                                               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                                  (DI.DynamicInfo []
                                                                     [DI.genTerm x20]))
                                                               (x'xstrict_check46_35selFP335f' x20)
                                                    DM.eval
                                                      (DM.letHook
                                                         (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                                            (DI.DynamicInfo [] []))
                                                         (do x10 <- do x21 <- Prelude.return x8
                                                                       DM.funcCallHook
                                                                         "check._#selFP4#rf'"
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Dequeue" 0)
                                                                            (DI.DynamicInfo []
                                                                               [DI.genTerm x21]))
                                                                         (x'xstrict_check46_35selFP435rf'
                                                                            x21)
                                                             DM.eval
                                                               (DM.letHook
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "DummyModule" 42)
                                                                     (DI.DynamicInfo [] []))
                                                                  (do x11 <- do x24 <- Prelude.return
                                                                                         x4
                                                                                x25 <- do x22 <- DM.funcCallHook
                                                                                                   "reverse"
                                                                                                   (DI.DebugInfo
                                                                                                      (DI.SrcID
                                                                                                         "Dequeue"
                                                                                                         0)
                                                                                                      (DI.DynamicInfo
                                                                                                         []
                                                                                                         []))
                                                                                                   Curry.DebugModule.Prelude.strict_reverse
                                                                                          x23 <- Prelude.return
                                                                                                   x10
                                                                                          DM.funcCallHook
                                                                                            "apply"
                                                                                            (DI.DebugInfo
                                                                                               (DI.SrcID
                                                                                                  "Dequeue"
                                                                                                  0)
                                                                                               (DI.DynamicInfo
                                                                                                  []
                                                                                                  [DI.genTerm
                                                                                                     x22,
                                                                                                   DI.genTerm
                                                                                                     x23]))
                                                                                            (Curry.DebugModule.Prelude.strict_apply
                                                                                               x22
                                                                                               x23)
                                                                                DM.funcCallHook "++"
                                                                                  (DI.DebugInfo
                                                                                     (DI.SrcID
                                                                                        "Dequeue"
                                                                                        0)
                                                                                     (DI.DynamicInfo
                                                                                        []
                                                                                        [DI.genTerm
                                                                                           x24,
                                                                                         DI.genTerm
                                                                                           x25]))
                                                                                  (Curry.DebugModule.Prelude.op_PlusPlus
                                                                                     x24
                                                                                     x25)
                                                                      DM.eval
                                                                        (do x32 <- Prelude.return x1
                                                                            x33 <- Prelude.return x2
                                                                            x34 <- Prelude.return x3
                                                                            x35 <- Prelude.return x4
                                                                            x36 <- Prelude.return x6
                                                                            x37 <- Prelude.return x7
                                                                            x38 <- Prelude.return x9
                                                                            x39 <- Prelude.return
                                                                                     x11
                                                                            x40 <- do x30 <- Prelude.return
                                                                                               x1
                                                                                      x31 <- do x28 <- do x26 <- DM.litHook
                                                                                                                   (DI.DebugInfo
                                                                                                                      (DI.SrcID
                                                                                                                         "Dequeue"
                                                                                                                         0)
                                                                                                                      (DI.DynamicInfo
                                                                                                                         []
                                                                                                                         []))
                                                                                                                   (Prelude.return
                                                                                                                      (Curry.DebugModule.Prelude.Pos
                                                                                                                         (Curry.DebugModule.Prelude.I
                                                                                                                            Curry.DebugModule.Prelude.IHi)))
                                                                                                          x27 <- Prelude.return
                                                                                                                   x3
                                                                                                          DM.funcCallHook
                                                                                                            "*"
                                                                                                            (DI.DebugInfo
                                                                                                               (DI.SrcID
                                                                                                                  "Dequeue"
                                                                                                                  0)
                                                                                                               (DI.DynamicInfo
                                                                                                                  []
                                                                                                                  [DI.genTerm
                                                                                                                     x26,
                                                                                                                   DI.genTerm
                                                                                                                     x27]))
                                                                                                            (Curry.DebugModule.Prelude.op_Asterisk
                                                                                                               x26
                                                                                                               x27)
                                                                                                x29 <- DM.litHook
                                                                                                         (DI.DebugInfo
                                                                                                            (DI.SrcID
                                                                                                               "Dequeue"
                                                                                                               0)
                                                                                                            (DI.DynamicInfo
                                                                                                               []
                                                                                                               []))
                                                                                                         (Prelude.return
                                                                                                            (Curry.DebugModule.Prelude.Pos
                                                                                                               Curry.DebugModule.Prelude.IHi))
                                                                                                DM.funcCallHook
                                                                                                  "+"
                                                                                                  (DI.DebugInfo
                                                                                                     (DI.SrcID
                                                                                                        "Dequeue"
                                                                                                        0)
                                                                                                     (DI.DynamicInfo
                                                                                                        []
                                                                                                        [DI.genTerm
                                                                                                           x28,
                                                                                                         DI.genTerm
                                                                                                           x29]))
                                                                                                  (Curry.DebugModule.Prelude.op_Plus
                                                                                                     x28
                                                                                                     x29)
                                                                                      DM.funcCallHook
                                                                                        "<="
                                                                                        (DI.DebugInfo
                                                                                           (DI.SrcID
                                                                                              "Dequeue"
                                                                                              0)
                                                                                           (DI.DynamicInfo
                                                                                              []
                                                                                              [DI.genTerm
                                                                                                 x30,
                                                                                               DI.genTerm
                                                                                                 x31]))
                                                                                        (Curry.DebugModule.Prelude.op_LtEq
                                                                                           x30
                                                                                           x31)
                                                                            DM.funcCallHook
                                                                              "_case_13"
                                                                              (DI.DebugInfo
                                                                                 (DI.SrcID "Dequeue"
                                                                                    0)
                                                                                 (DI.DynamicInfo []
                                                                                    [DI.genTerm x32,
                                                                                     DI.genTerm x33,
                                                                                     DI.genTerm x34,
                                                                                     DI.genTerm x35,
                                                                                     DI.genTerm x36,
                                                                                     DI.genTerm x37,
                                                                                     DI.genTerm x38,
                                                                                     DI.genTerm x39,
                                                                                     DI.genTerm
                                                                                       x40]))
                                                                              (strict__case_13 x32
                                                                                 x33
                                                                                 x34
                                                                                 x35
                                                                                 x36
                                                                                 x37
                                                                                 x38
                                                                                 x39
                                                                                 x40)))))))))))))))))
term_strict_check x1 = DI.Term "check" (DI.SrcID "Dequeue" 0) x1
 
x'xstrict_check46_35selFP335f' ::
                               (DM.DM dm, DI.GenTerm x45) =>
                                 Curry.DebugModule.Prelude.Tuple2
                                   (Curry.DebugModule.Prelude.List x45)
                                   (Curry.DebugModule.Prelude.List x45)
                                   -> dm (Curry.DebugModule.Prelude.List x45)
x'xstrict_check46_35selFP335f' x1
  = DM.eval
      (DM.funcDeclHook "check._#selFP3#f'"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_11"
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_11 x2)))
x'xterm_strict_check46_35selFP335f' x1
  = DI.Term "check._#selFP3#f'" (DI.SrcID "Dequeue" 0) x1
 
x'xstrict_check46_35selFP435rf' ::
                                (DM.DM dm, DI.GenTerm x45) =>
                                  Curry.DebugModule.Prelude.Tuple2
                                    (Curry.DebugModule.Prelude.List x45)
                                    (Curry.DebugModule.Prelude.List x45)
                                    -> dm (Curry.DebugModule.Prelude.List x45)
x'xstrict_check46_35selFP435rf' x1
  = DM.eval
      (DM.funcDeclHook "check._#selFP4#rf'"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_10"
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_10 x2)))
x'xterm_strict_check46_35selFP435rf' x1
  = DI.Term "check._#selFP4#rf'" (DI.SrcID "Dequeue" 0) x1
 
strict_listToDeq ::
                 (DM.DM dm, DI.GenTerm a) =>
                   Curry.DebugModule.Prelude.List a -> dm (Queue a)
strict_listToDeq x1
  = DM.eval
      (DM.funcDeclHook "listToDeq"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- do x2 <- Prelude.return x1
                      DM.funcCallHook "length"
                        (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2]))
                        (Curry.DebugModule.Prelude.strict_length x2)
             x4 <- Prelude.return x1
             x5 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                     (Prelude.return Curry.DebugModule.Prelude.Zero)
             x6 <- DM.constructorHook
                     (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                     (Prelude.return Curry.DebugModule.Prelude.Nil)
             DM.funcCallHook "check"
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo []
                     [DI.genTerm x3, DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
               (strict_check x3 x4 x5 x6)))
term_strict_listToDeq x1
  = DI.Term "listToDeq" (DI.SrcID "Dequeue" 0) x1
 
strict_deqToList ::
                 (DM.DM dm, DI.GenTerm a) =>
                   Queue a -> dm (Curry.DebugModule.Prelude.List a)
strict_deqToList x1
  = DM.eval
      (DM.funcDeclHook "deqToList"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_9"
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_9 x2)))
term_strict_deqToList x1
  = DI.Term "deqToList" (DI.SrcID "Dequeue" 0) x1
 
strict_deqLength ::
                 (DM.DM dm, DI.GenTerm a) =>
                   Queue a -> dm Curry.DebugModule.Prelude.Int
strict_deqLength x1
  = DM.eval
      (DM.funcDeclHook "deqLength"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_8"
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_8 x2)))
term_strict_deqLength x1
  = DI.Term "deqLength" (DI.SrcID "Dequeue" 0) x1
 
strict_rotate ::
              (DM.DM dm, DI.GenTerm a) => Queue a -> dm (Queue a)
strict_rotate x1
  = DM.eval
      (DM.funcDeclHook "rotate"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- do x2 <- Prelude.return x1
                      DM.funcCallHook "deqHead"
                        (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2]))
                        (strict_deqHead x2)
             x5 <- do x3 <- Prelude.return x1
                      DM.funcCallHook "deqTail"
                        (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3]))
                        (strict_deqTail x3)
             DM.funcCallHook "snoc"
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
               (strict_snoc x4 x5)))
term_strict_rotate x1 = DI.Term "rotate" (DI.SrcID "Dequeue" 0) x1
 
strict_matchHead ::
                 (DM.DM dm, DI.GenTerm a) =>
                   Queue a ->
                     dm
                       (Curry.DebugModule.Prelude.Maybe
                          (Curry.DebugModule.Prelude.Tuple2 a (Queue a)))
strict_matchHead x1
  = DM.eval
      (DM.funcDeclHook "matchHead"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_7"
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_7 x2)))
term_strict_matchHead x1
  = DI.Term "matchHead" (DI.SrcID "Dequeue" 0) x1
 
strict_matchLast ::
                 (DM.DM dm, DI.GenTerm a) =>
                   Queue a ->
                     dm
                       (Curry.DebugModule.Prelude.Maybe
                          (Curry.DebugModule.Prelude.Tuple2 a (Queue a)))
strict_matchLast x1
  = DM.eval
      (DM.funcDeclHook "matchLast"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_3"
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_3 x2)))
term_strict_matchLast x1
  = DI.Term "matchLast" (DI.SrcID "Dequeue" 0) x1
strict__case_3 x1
  = DM.eval
      (DM.funcDeclHook "_case_3"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x10 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    S x2 x3 x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x2
                                  x7 <- Prelude.return x3
                                  x8 <- Prelude.return x4
                                  x9 <- Prelude.return x5
                                  DM.funcCallHook "_case_2"
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x6, DI.genTerm x7, DI.genTerm x8,
                                           DI.genTerm x9]))
                                    (strict__case_2 x6 x7 x8 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           strict__case_3
                           x10)))
term_strict__case_3 x1
  = DI.Term "_case_3" (DI.SrcID "Dequeue" 0) x1
strict__case_2 x2 x3 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_2"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo []
               [DI.genTerm x2, DI.genTerm x3, DI.genTerm x4, DI.genTerm x5]))
         (do x20 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x20]))
               (case x20 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- Prelude.return x3
                                  DM.funcCallHook "_case_1"
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10]))
                                    (strict__case_1 x10)))
                    Curry.DebugModule.Prelude.Cons x8 x9
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x19 <- do x17 <- Prelude.return x8
                                            x18 <- do x13 <- Prelude.return x2
                                                      x14 <- Prelude.return x3
                                                      x15 <- do x11 <- Prelude.return x4
                                                                x12 <- DM.litHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Dequeue" 0)
                                                                            (DI.DynamicInfo [] []))
                                                                         (Prelude.return
                                                                            (Curry.DebugModule.Prelude.Pos
                                                                               Curry.DebugModule.Prelude.IHi))
                                                                DM.funcCallHook "-"
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "Dequeue" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x11,
                                                                         DI.genTerm x12]))
                                                                  (Curry.DebugModule.Prelude.op_Minus
                                                                     x11
                                                                     x12)
                                                      x16 <- Prelude.return x9
                                                      DM.funcCallHook "check"
                                                        (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x13, DI.genTerm x14,
                                                               DI.genTerm x15, DI.genTerm x16]))
                                                        (strict_check x13 x14 x15 x16)
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x17, DI.genTerm x18]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Tuple2 x17 x18))
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x19]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Just x19))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x20])))
                           (strict__case_2 x2 x3 x4)
                           x20)))
term_strict__case_2 x1
  = DI.Term "_case_2" (DI.SrcID "Dequeue" 0) x1
strict__case_1 x3
  = DM.eval
      (DM.funcDeclHook "_case_1"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x3]))
         (do x10 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Nothing)))
                    Curry.DebugModule.Prelude.Cons x6 x7
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x6
                                  x9 <- Prelude.return x7
                                  DM.funcCallHook "_case_0"
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (strict__case_0 x8 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           strict__case_1
                           x10)))
term_strict__case_1 x1
  = DI.Term "_case_1" (DI.SrcID "Dequeue" 0) x1
strict__case_0 x6 x7
  = DM.eval
      (DM.funcDeclHook "_case_0"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
         (do x11 <- Prelude.return x7
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- do x8 <- Prelude.return x6
                                            x9 <- DM.funcCallHook "empty"
                                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                       (DI.DynamicInfo [] []))
                                                    strict_empty
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Tuple2 x8 x9))
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Just x10))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_0 x6)
                           x11)))
term_strict__case_0 x1
  = DI.Term "_case_0" (DI.SrcID "Dequeue" 0) x1
strict__case_7 x1
  = DM.eval
      (DM.funcDeclHook "_case_7"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x10 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    S x2 x3 x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x2
                                  x7 <- Prelude.return x4
                                  x8 <- Prelude.return x5
                                  x9 <- Prelude.return x3
                                  DM.funcCallHook "_case_6"
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x6, DI.genTerm x7, DI.genTerm x8,
                                           DI.genTerm x9]))
                                    (strict__case_6 x6 x7 x8 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           strict__case_7
                           x10)))
term_strict__case_7 x1
  = DI.Term "_case_7" (DI.SrcID "Dequeue" 0) x1
strict__case_6 x2 x4 x5 x3
  = DM.eval
      (DM.funcDeclHook "_case_6"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo []
               [DI.genTerm x2, DI.genTerm x4, DI.genTerm x5, DI.genTerm x3]))
         (do x21 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x21]))
               (case x21 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- Prelude.return x5
                                  DM.funcCallHook "_case_5"
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10]))
                                    (strict__case_5 x10)))
                    Curry.DebugModule.Prelude.Cons x8 x9
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x20 <- do x18 <- Prelude.return x8
                                            x19 <- do x17 <- do x13 <- Prelude.return x4
                                                                x14 <- Prelude.return x5
                                                                x15 <- do x11 <- Prelude.return x2
                                                                          x12 <- DM.litHook
                                                                                   (DI.DebugInfo
                                                                                      (DI.SrcID
                                                                                         "Dequeue"
                                                                                         0)
                                                                                      (DI.DynamicInfo
                                                                                         []
                                                                                         []))
                                                                                   (Prelude.return
                                                                                      (Curry.DebugModule.Prelude.Pos
                                                                                         Curry.DebugModule.Prelude.IHi))
                                                                          DM.funcCallHook "-"
                                                                            (DI.DebugInfo
                                                                               (DI.SrcID "Dequeue"
                                                                                  0)
                                                                               (DI.DynamicInfo []
                                                                                  [DI.genTerm x11,
                                                                                   DI.genTerm x12]))
                                                                            (Curry.DebugModule.Prelude.op_Minus
                                                                               x11
                                                                               x12)
                                                                x16 <- Prelude.return x9
                                                                DM.funcCallHook "check"
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "Dequeue" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x13,
                                                                         DI.genTerm x14,
                                                                         DI.genTerm x15,
                                                                         DI.genTerm x16]))
                                                                  (strict_check x13 x14 x15 x16)
                                                      DM.funcCallHook "deqReverse"
                                                        (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                           (DI.DynamicInfo [] [DI.genTerm x17]))
                                                        (strict_deqReverse x17)
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x18, DI.genTerm x19]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Tuple2 x18 x19))
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x20]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Just x20))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x21])))
                           (strict__case_6 x2 x4 x5)
                           x21)))
term_strict__case_6 x1
  = DI.Term "_case_6" (DI.SrcID "Dequeue" 0) x1
strict__case_5 x5
  = DM.eval
      (DM.funcDeclHook "_case_5"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x5]))
         (do x10 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.constructorHook
                                 (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Nothing)))
                    Curry.DebugModule.Prelude.Cons x6 x7
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x6
                                  x9 <- Prelude.return x7
                                  DM.funcCallHook "_case_4"
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (strict__case_4 x8 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           strict__case_5
                           x10)))
term_strict__case_5 x1
  = DI.Term "_case_5" (DI.SrcID "Dequeue" 0) x1
strict__case_4 x6 x7
  = DM.eval
      (DM.funcDeclHook "_case_4"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
         (do x11 <- Prelude.return x7
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- do x8 <- Prelude.return x6
                                            x9 <- DM.funcCallHook "empty"
                                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                       (DI.DynamicInfo [] []))
                                                    strict_empty
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Tuple2 x8 x9))
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10]))
                                    (Prelude.return (Curry.DebugModule.Prelude.Just x10))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_4 x6)
                           x11)))
term_strict__case_4 x1
  = DI.Term "_case_4" (DI.SrcID "Dequeue" 0) x1
strict__case_8 x1
  = DM.eval
      (DM.funcDeclHook "_case_8"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x8 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    S x2 x3 x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x2
                                  x7 <- Prelude.return x4
                                  DM.funcCallHook "+"
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (Curry.DebugModule.Prelude.op_Plus x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           strict__case_8
                           x8)))
term_strict__case_8 x1
  = DI.Term "_case_8" (DI.SrcID "Dequeue" 0) x1
strict__case_9 x1
  = DM.eval
      (DM.funcDeclHook "_case_9"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x10 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    S x2 x3 x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- Prelude.return x3
                                  x9 <- do x6 <- DM.funcCallHook "reverse"
                                                   (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                      (DI.DynamicInfo [] []))
                                                   Curry.DebugModule.Prelude.strict_reverse
                                           x7 <- Prelude.return x5
                                           DM.funcCallHook "apply"
                                             (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                             (Curry.DebugModule.Prelude.strict_apply x6 x7)
                                  DM.funcCallHook "++"
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (Curry.DebugModule.Prelude.op_PlusPlus x8 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           strict__case_9
                           x10)))
term_strict__case_9 x1
  = DI.Term "_case_9" (DI.SrcID "Dequeue" 0) x1
strict__case_10 x1
  = DM.eval
      (DM.funcDeclHook "_case_10"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_10
                           x4)))
term_strict__case_10 x1
  = DI.Term "_case_10" (DI.SrcID "Dequeue" 0) x1
strict__case_11 x1
  = DM.eval
      (DM.funcDeclHook "_case_11"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.Tuple2 x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           strict__case_11
                           x4)))
term_strict__case_11 x1
  = DI.Term "_case_11" (DI.SrcID "Dequeue" 0) x1
strict__case_13 x1 x2 x3 x4 x6 x7 x9 x11 x12
  = DM.eval
      (DM.funcDeclHook "_case_13"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4,
                DI.genTerm x6, DI.genTerm x7, DI.genTerm x9, DI.genTerm x11,
                DI.genTerm x12]))
         (do x22 <- Prelude.return x12
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo [] [DI.genTerm x22]))
               (case x22 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x13 <- Prelude.return x1
                                  x14 <- Prelude.return x2
                                  x15 <- Prelude.return x3
                                  x16 <- Prelude.return x4
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x13, DI.genTerm x14, DI.genTerm x15,
                                           DI.genTerm x16]))
                                    (Prelude.return (S x13 x14 x15 x16))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x17 <- Prelude.return x6
                                  x18 <- Prelude.return x7
                                  x19 <- Prelude.return x9
                                  x20 <- Prelude.return x11
                                  x21 <- DM.funcCallHook "otherwise"
                                           (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                              (DI.DynamicInfo [] []))
                                           Curry.DebugModule.Prelude.strict_otherwise
                                  DM.funcCallHook "_case_12"
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x17, DI.genTerm x18, DI.genTerm x19,
                                           DI.genTerm x20, DI.genTerm x21]))
                                    (strict__case_12 x17 x18 x19 x20 x21)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x22])))
                           (strict__case_13 x1 x2 x3 x4 x6 x7 x9 x11)
                           x22)))
term_strict__case_13 x1
  = DI.Term "_case_13" (DI.SrcID "Dequeue" 0) x1
strict__case_12 x6 x7 x9 x11 x12
  = DM.eval
      (DM.funcDeclHook "_case_12"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo []
               [DI.genTerm x6, DI.genTerm x7, DI.genTerm x9, DI.genTerm x11,
                DI.genTerm x12]))
         (do x17 <- Prelude.return x12
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo [] [DI.genTerm x17]))
               (case x17 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x13 <- Prelude.return x6
                                  x14 <- Prelude.return x9
                                  x15 <- Prelude.return x7
                                  x16 <- Prelude.return x11
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x13, DI.genTerm x14, DI.genTerm x15,
                                           DI.genTerm x16]))
                                    (Prelude.return (S x13 x14 x15 x16))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                                 Curry.DebugModule.Prelude.strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x17])))
                           (strict__case_12 x6 x7 x9 x11)
                           x17)))
term_strict__case_12 x1
  = DI.Term "_case_12" (DI.SrcID "Dequeue" 0) x1
strict__case_14 x1
  = DM.eval
      (DM.funcDeclHook "_case_14"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x10 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    S x2 x3 x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x4
                                  x7 <- Prelude.return x5
                                  x8 <- Prelude.return x2
                                  x9 <- Prelude.return x3
                                  DM.constructorHook
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x6, DI.genTerm x7, DI.genTerm x8,
                                           DI.genTerm x9]))
                                    (Prelude.return (S x6 x7 x8 x9))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           strict__case_14
                           x10)))
term_strict__case_14 x1
  = DI.Term "_case_14" (DI.SrcID "Dequeue" 0) x1
strict__case_16 x1
  = DM.eval
      (DM.funcDeclHook "_case_16"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x10 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    S x2 x3 x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x2
                                  x7 <- Prelude.return x3
                                  x8 <- Prelude.return x4
                                  x9 <- Prelude.return x5
                                  DM.funcCallHook "_case_15"
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x6, DI.genTerm x7, DI.genTerm x8,
                                           DI.genTerm x9]))
                                    (strict__case_15 x6 x7 x8 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           strict__case_16
                           x10)))
term_strict__case_16 x1
  = DI.Term "_case_16" (DI.SrcID "Dequeue" 0) x1
strict__case_15 x2 x3 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_15"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo []
               [DI.genTerm x2, DI.genTerm x3, DI.genTerm x4, DI.genTerm x5]))
         (do x14 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x14]))
               (case x14 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "empty"
                                 (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                                 strict_empty))
                    Curry.DebugModule.Prelude.Cons x6 x7
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- Prelude.return x2
                                  x11 <- Prelude.return x3
                                  x12 <- do x8 <- Prelude.return x4
                                            x9 <- DM.litHook
                                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                       (DI.DynamicInfo [] []))
                                                    (Prelude.return
                                                       (Curry.DebugModule.Prelude.Pos
                                                          Curry.DebugModule.Prelude.IHi))
                                            DM.funcCallHook "-"
                                              (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                              (Curry.DebugModule.Prelude.op_Minus x8 x9)
                                  x13 <- Prelude.return x7
                                  DM.funcCallHook "check"
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x10, DI.genTerm x11, DI.genTerm x12,
                                           DI.genTerm x13]))
                                    (strict_check x10 x11 x12 x13)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x14])))
                           (strict__case_15 x2 x3 x4)
                           x14)))
term_strict__case_15 x1
  = DI.Term "_case_15" (DI.SrcID "Dequeue" 0) x1
strict__case_17 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_17"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x16 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x16]))
               (case x16 of
                    S x3 x4 x5 x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x15 <- do x11 <- do x7 <- Prelude.return x5
                                                      x8 <- DM.litHook
                                                              (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                                 (DI.DynamicInfo [] []))
                                                              (Prelude.return
                                                                 (Curry.DebugModule.Prelude.Pos
                                                                    Curry.DebugModule.Prelude.IHi))
                                                      DM.funcCallHook "+"
                                                        (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x7, DI.genTerm x8]))
                                                        (Curry.DebugModule.Prelude.op_Plus x7 x8)
                                            x12 <- do x9 <- Prelude.return x1
                                                      x10 <- Prelude.return x6
                                                      DM.constructorHook
                                                        (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x9, DI.genTerm x10]))
                                                        (Prelude.return
                                                           (Curry.DebugModule.Prelude.Cons x9 x10))
                                            x13 <- Prelude.return x3
                                            x14 <- Prelude.return x4
                                            DM.funcCallHook "check"
                                              (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x11, DI.genTerm x12, DI.genTerm x13,
                                                     DI.genTerm x14]))
                                              (strict_check x11 x12 x13 x14)
                                  DM.funcCallHook "deqReverse"
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x15]))
                                    (strict_deqReverse x15)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x16])))
                           (strict__case_17 x1)
                           x16)))
term_strict__case_17 x1
  = DI.Term "_case_17" (DI.SrcID "Dequeue" 0) x1
strict__case_19 x1
  = DM.eval
      (DM.funcDeclHook "_case_19"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x10 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    S x2 x3 x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x2
                                  x7 <- Prelude.return x4
                                  x8 <- Prelude.return x5
                                  x9 <- Prelude.return x3
                                  DM.funcCallHook "_case_18"
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x6, DI.genTerm x7, DI.genTerm x8,
                                           DI.genTerm x9]))
                                    (strict__case_18 x6 x7 x8 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           strict__case_19
                           x10)))
term_strict__case_19 x1
  = DI.Term "_case_19" (DI.SrcID "Dequeue" 0) x1
strict__case_18 x2 x4 x5 x3
  = DM.eval
      (DM.funcDeclHook "_case_18"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo []
               [DI.genTerm x2, DI.genTerm x4, DI.genTerm x5, DI.genTerm x3]))
         (do x15 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x15]))
               (case x15 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "empty"
                                 (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                                 strict_empty))
                    Curry.DebugModule.Prelude.Cons x6 x7
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x14 <- do x10 <- Prelude.return x4
                                            x11 <- Prelude.return x5
                                            x12 <- do x8 <- Prelude.return x2
                                                      x9 <- DM.litHook
                                                              (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                                 (DI.DynamicInfo [] []))
                                                              (Prelude.return
                                                                 (Curry.DebugModule.Prelude.Pos
                                                                    Curry.DebugModule.Prelude.IHi))
                                                      DM.funcCallHook "-"
                                                        (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x8, DI.genTerm x9]))
                                                        (Curry.DebugModule.Prelude.op_Minus x8 x9)
                                            x13 <- Prelude.return x7
                                            DM.funcCallHook "check"
                                              (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x10, DI.genTerm x11, DI.genTerm x12,
                                                     DI.genTerm x13]))
                                              (strict_check x10 x11 x12 x13)
                                  DM.funcCallHook "deqReverse"
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x14]))
                                    (strict_deqReverse x14)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x15])))
                           (strict__case_18 x2 x4 x5)
                           x15)))
term_strict__case_18 x1
  = DI.Term "_case_18" (DI.SrcID "Dequeue" 0) x1
strict__case_20 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_20"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x15 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x15]))
               (case x15 of
                    S x3 x4 x5 x6
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- do x7 <- Prelude.return x3
                                            x8 <- DM.litHook
                                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                       (DI.DynamicInfo [] []))
                                                    (Prelude.return
                                                       (Curry.DebugModule.Prelude.Pos
                                                          Curry.DebugModule.Prelude.IHi))
                                            DM.funcCallHook "+"
                                              (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                              (Curry.DebugModule.Prelude.op_Plus x7 x8)
                                  x12 <- do x9 <- Prelude.return x1
                                            x10 <- Prelude.return x4
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x9, DI.genTerm x10]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Cons x9 x10))
                                  x13 <- Prelude.return x5
                                  x14 <- Prelude.return x6
                                  DM.funcCallHook "check"
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x11, DI.genTerm x12, DI.genTerm x13,
                                           DI.genTerm x14]))
                                    (strict_check x11 x12 x13 x14)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x15])))
                           (strict__case_20 x1)
                           x15)))
term_strict__case_20 x1
  = DI.Term "_case_20" (DI.SrcID "Dequeue" 0) x1
strict__case_22 x1
  = DM.eval
      (DM.funcDeclHook "_case_22"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x13 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    S x2 x3 x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- do x8 <- Prelude.return x3
                                            x9 <- Prelude.return x4
                                            x10 <- Prelude.return x5
                                            x11 <- do x6 <- Prelude.return x4
                                                      x7 <- DM.litHook
                                                              (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                                 (DI.DynamicInfo [] []))
                                                              (Prelude.return
                                                                 Curry.DebugModule.Prelude.Zero)
                                                      DM.funcCallHook "=="
                                                        (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x6, DI.genTerm x7]))
                                                        (Curry.DebugModule.Prelude.op_EqEq x6 x7)
                                            DM.funcCallHook "_case_21"
                                              (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x8, DI.genTerm x9, DI.genTerm x10,
                                                     DI.genTerm x11]))
                                              (strict__case_21 x8 x9 x10 x11)
                                  DM.funcCallHook "head"
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x12]))
                                    (Curry.DebugModule.Prelude.strict_head x12)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           strict__case_22
                           x13)))
term_strict__case_22 x1
  = DI.Term "_case_22" (DI.SrcID "Dequeue" 0) x1
strict__case_21 x3 x4 x5 x6
  = DM.eval
      (DM.funcDeclHook "_case_21"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo []
               [DI.genTerm x3, DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
         (do x7 <- Prelude.return x6
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo [] [DI.genTerm x7]))
               (case x7 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x5))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x7])))
                           (strict__case_21 x3 x4 x5)
                           x7)))
term_strict__case_21 x1
  = DI.Term "_case_21" (DI.SrcID "Dequeue" 0) x1
strict__case_24 x1
  = DM.eval
      (DM.funcDeclHook "_case_24"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x13 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    S x2 x3 x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x12 <- do x8 <- Prelude.return x2
                                            x9 <- Prelude.return x3
                                            x10 <- Prelude.return x5
                                            x11 <- do x6 <- Prelude.return x2
                                                      x7 <- DM.litHook
                                                              (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                                 (DI.DynamicInfo [] []))
                                                              (Prelude.return
                                                                 Curry.DebugModule.Prelude.Zero)
                                                      DM.funcCallHook "=="
                                                        (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x6, DI.genTerm x7]))
                                                        (Curry.DebugModule.Prelude.op_EqEq x6 x7)
                                            DM.funcCallHook "_case_23"
                                              (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x8, DI.genTerm x9, DI.genTerm x10,
                                                     DI.genTerm x11]))
                                              (strict__case_23 x8 x9 x10 x11)
                                  DM.funcCallHook "head"
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x12]))
                                    (Curry.DebugModule.Prelude.strict_head x12)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           strict__case_24
                           x13)))
term_strict__case_24 x1
  = DI.Term "_case_24" (DI.SrcID "Dequeue" 0) x1
strict__case_23 x2 x3 x5 x6
  = DM.eval
      (DM.funcDeclHook "_case_23"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo []
               [DI.genTerm x2, DI.genTerm x3, DI.genTerm x5, DI.genTerm x6]))
         (do x7 <- Prelude.return x6
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                  (DI.DynamicInfo [] [DI.genTerm x7]))
               (case x7 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x5))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x3))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x7])))
                           (strict__case_23 x2 x3 x5)
                           x7)))
term_strict__case_23 x1
  = DI.Term "_case_23" (DI.SrcID "Dequeue" 0) x1
strict__case_25 x1
  = DM.eval
      (DM.funcDeclHook "_case_25"
         (DI.DebugInfo (DI.SrcID "Dequeue" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x10 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    S x2 x3 x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Dequeue" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- do x6 <- Prelude.return x2
                                           x7 <- Prelude.return x4
                                           DM.funcCallHook "+"
                                             (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                             (Curry.DebugModule.Prelude.op_Plus x6 x7)
                                  x9 <- DM.litHook
                                          (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return Curry.DebugModule.Prelude.Zero)
                                  DM.funcCallHook "=="
                                    (DI.DebugInfo (DI.SrcID "Dequeue" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                    (Curry.DebugModule.Prelude.op_EqEq x8 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Dequeue" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           strict__case_25
                           x10)))
term_strict__case_25 x1
  = DI.Term "_case_25" (DI.SrcID "Dequeue" 0) x1
term_S x1 = DI.Term "S" (DI.SrcID "Dequeue" 0) x1