{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Curry.DebugModule.Read where
import qualified Prelude
import qualified Curry.Debugger.DebugMonad as DM
import qualified Curry.Debugger.DebugInfo as DI
import qualified Curry.Debugger.PartCalls as PC
import qualified Data.Generics
import qualified Curry.DebugModule.Char
import qualified Curry.DebugModule.Prelude
 
strict_readNat ::
               (DM.DM dm) =>
                 Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                   dm Curry.DebugModule.Prelude.Int
strict_readNat x1
  = DM.eval
      (DM.funcDeclHook "readNat"
         (DI.DebugInfo (DI.SrcID "Read" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- do x2 <- Prelude.return
                              (PC.partCall1 (x'xterm_strict_readNat46_35lambda2 [])
                                 x'xstrict_readNat46_35lambda2)
                      x3 <- Prelude.return x1
                      DM.funcCallHook "dropWhile"
                        (DI.DebugInfo (DI.SrcID "Read" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                        (Curry.DebugModule.Prelude.strict_dropWhile x2 x3)
             x5 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                     (Prelude.return Curry.DebugModule.Prelude.Zero)
             DM.funcCallHook "readNat.readNatPrefix.3"
               (DI.DebugInfo (DI.SrcID "Read" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
               (x'xstrict_readNat46readNatPrefix463 x4 x5)))
term_strict_readNat x1 = DI.Term "readNat" (DI.SrcID "Read" 0) x1
 
x'xstrict_readNat46readNatPrefix463 ::
                                    (DM.DM dm) =>
                                      Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char
                                        ->
                                        Curry.DebugModule.Prelude.Int ->
                                          dm Curry.DebugModule.Prelude.Int
x'xstrict_readNat46readNatPrefix463 x1 x2
  = DM.eval
      (DM.funcDeclHook "readNat.readNatPrefix.3"
         (DI.DebugInfo (DI.SrcID "Read" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_7"
               (DI.DebugInfo (DI.SrcID "Read" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_7 x3 x4)))
x'xterm_strict_readNat46readNatPrefix463 x1
  = DI.Term "readNat.readNatPrefix.3" (DI.SrcID "Read" 0) x1
 
x'xstrict_readNat46_35lambda2 ::
                              (DM.DM dm) =>
                                Curry.DebugModule.Prelude.Char -> dm Curry.DebugModule.Prelude.Bool
x'xstrict_readNat46_35lambda2 x1
  = DM.eval
      (DM.funcDeclHook "readNat._#lambda2"
         (DI.DebugInfo (DI.SrcID "Read" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             x3 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                     (Prelude.return (Curry.DebugModule.Prelude.Char ' '))
             DM.funcCallHook "=="
               (DI.DebugInfo (DI.SrcID "Read" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_EqEq x2 x3)))
x'xterm_strict_readNat46_35lambda2 x1
  = DI.Term "readNat._#lambda2" (DI.SrcID "Read" 0) x1
 
strict_readInt ::
               (DM.DM dm) =>
                 Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                   dm Curry.DebugModule.Prelude.Int
strict_readInt x1
  = DM.eval
      (DM.funcDeclHook "readInt"
         (DI.DebugInfo (DI.SrcID "Read" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- do x2 <- Prelude.return
                              (PC.partCall1 (x'xterm_strict_readInt46_35lambda3 [])
                                 x'xstrict_readInt46_35lambda3)
                      x3 <- Prelude.return x1
                      DM.funcCallHook "dropWhile"
                        (DI.DebugInfo (DI.SrcID "Read" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                        (Curry.DebugModule.Prelude.strict_dropWhile x2 x3)
             DM.funcCallHook "readInt.readIntPrefix.14"
               (DI.DebugInfo (DI.SrcID "Read" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (x'xstrict_readInt46readIntPrefix4614 x4)))
term_strict_readInt x1 = DI.Term "readInt" (DI.SrcID "Read" 0) x1
 
x'xstrict_readInt46readIntPrefix4614 ::
                                     (DM.DM dm) =>
                                       Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char
                                         -> dm Curry.DebugModule.Prelude.Int
x'xstrict_readInt46readIntPrefix4614 x1
  = DM.eval
      (DM.funcDeclHook "readInt.readIntPrefix.14"
         (DI.DebugInfo (DI.SrcID "Read" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_5"
               (DI.DebugInfo (DI.SrcID "Read" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_5 x2)))
x'xterm_strict_readInt46readIntPrefix4614 x1
  = DI.Term "readInt.readIntPrefix.14" (DI.SrcID "Read" 0) x1
 
x'xstrict_readInt46_35lambda3 ::
                              (DM.DM dm) =>
                                Curry.DebugModule.Prelude.Char -> dm Curry.DebugModule.Prelude.Bool
x'xstrict_readInt46_35lambda3 x1
  = DM.eval
      (DM.funcDeclHook "readInt._#lambda3"
         (DI.DebugInfo (DI.SrcID "Read" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             x3 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                     (Prelude.return (Curry.DebugModule.Prelude.Char ' '))
             DM.funcCallHook "=="
               (DI.DebugInfo (DI.SrcID "Read" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_EqEq x2 x3)))
x'xterm_strict_readInt46_35lambda3 x1
  = DI.Term "readInt._#lambda3" (DI.SrcID "Read" 0) x1
 
strict_readHex ::
               (DM.DM dm) =>
                 Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char ->
                   dm Curry.DebugModule.Prelude.Int
strict_readHex x1
  = DM.eval
      (DM.funcDeclHook "readHex"
         (DI.DebugInfo (DI.SrcID "Read" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- do x2 <- Prelude.return
                              (PC.partCall1 (x'xterm_strict_readHex46_35lambda4 [])
                                 x'xstrict_readHex46_35lambda4)
                      x3 <- Prelude.return x1
                      DM.funcCallHook "dropWhile"
                        (DI.DebugInfo (DI.SrcID "Read" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                        (Curry.DebugModule.Prelude.strict_dropWhile x2 x3)
             x5 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                     (Prelude.return Curry.DebugModule.Prelude.Zero)
             DM.funcCallHook "readHex.readHexPrefix.22"
               (DI.DebugInfo (DI.SrcID "Read" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
               (x'xstrict_readHex46readHexPrefix4622 x4 x5)))
term_strict_readHex x1 = DI.Term "readHex" (DI.SrcID "Read" 0) x1
 
x'xstrict_readHex46hex2int4622 ::
                               (DM.DM dm) =>
                                 Curry.DebugModule.Prelude.Char -> dm Curry.DebugModule.Prelude.Int
x'xstrict_readHex46hex2int4622 x1
  = DM.eval
      (DM.funcDeclHook "readHex.hex2int.22"
         (DI.DebugInfo (DI.SrcID "Read" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- Prelude.return x1
             x4 <- do x2 <- Prelude.return x1
                      DM.funcCallHook "isDigit"
                        (DI.DebugInfo (DI.SrcID "Read" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2]))
                        (Curry.DebugModule.Char.strict_isDigit x2)
             DM.funcCallHook "_case_3"
               (DI.DebugInfo (DI.SrcID "Read" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_3 x3 x4)))
x'xterm_strict_readHex46hex2int4622 x1
  = DI.Term "readHex.hex2int.22" (DI.SrcID "Read" 0) x1
 
x'xstrict_readHex46readHexPrefix4622 ::
                                     (DM.DM dm) =>
                                       Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Char
                                         ->
                                         Curry.DebugModule.Prelude.Int ->
                                           dm Curry.DebugModule.Prelude.Int
x'xstrict_readHex46readHexPrefix4622 x1 x2
  = DM.eval
      (DM.funcDeclHook "readHex.readHexPrefix.22"
         (DI.DebugInfo (DI.SrcID "Read" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             x4 <- Prelude.return x1
             DM.funcCallHook "_case_1"
               (DI.DebugInfo (DI.SrcID "Read" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_1 x3 x4)))
x'xterm_strict_readHex46readHexPrefix4622 x1
  = DI.Term "readHex.readHexPrefix.22" (DI.SrcID "Read" 0) x1
 
x'xstrict_readHex46_35lambda4 ::
                              (DM.DM dm) =>
                                Curry.DebugModule.Prelude.Char -> dm Curry.DebugModule.Prelude.Bool
x'xstrict_readHex46_35lambda4 x1
  = DM.eval
      (DM.funcDeclHook "readHex._#lambda4"
         (DI.DebugInfo (DI.SrcID "Read" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             x3 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                     (Prelude.return (Curry.DebugModule.Prelude.Char ' '))
             DM.funcCallHook "=="
               (DI.DebugInfo (DI.SrcID "Read" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_EqEq x2 x3)))
x'xterm_strict_readHex46_35lambda4 x1
  = DI.Term "readHex._#lambda4" (DI.SrcID "Read" 0) x1
strict__case_1 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_1"
         (DI.DebugInfo (DI.SrcID "Read" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x13 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Read" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    Curry.DebugModule.Prelude.Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x5 <- do x6 <- Prelude.return x3
                                              DM.funcCallHook "readHex.hex2int.22"
                                                (DI.DebugInfo (DI.SrcID "Read" 0)
                                                   (DI.DynamicInfo [] [DI.genTerm x6]))
                                                (x'xstrict_readHex46hex2int4622 x6)
                                     DM.eval
                                       (do x9 <- Prelude.return x2
                                           x10 <- Prelude.return x4
                                           x11 <- Prelude.return x5
                                           x12 <- do x7 <- Prelude.return x5
                                                     x8 <- DM.litHook
                                                             (DI.DebugInfo (DI.SrcID "Read" 0)
                                                                (DI.DynamicInfo [] []))
                                                             (Prelude.return
                                                                Curry.DebugModule.Prelude.Zero)
                                                     DM.funcCallHook ">="
                                                       (DI.DebugInfo (DI.SrcID "Read" 0)
                                                          (DI.DynamicInfo []
                                                             [DI.genTerm x7, DI.genTerm x8]))
                                                       (Curry.DebugModule.Prelude.op_GtEq x7 x8)
                                           DM.funcCallHook "_case_0"
                                             (DI.DebugInfo (DI.SrcID "Read" 0)
                                                (DI.DynamicInfo []
                                                   [DI.genTerm x9, DI.genTerm x10, DI.genTerm x11,
                                                    DI.genTerm x12]))
                                             (strict__case_0 x9 x10 x11 x12)))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Read" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_1 x2)
                           x13)))
term_strict__case_1 x1 = DI.Term "_case_1" (DI.SrcID "Read" 0) x1
strict__case_0 x2 x4 x5 x6
  = DM.eval
      (DM.funcDeclHook "_case_0"
         (DI.DebugInfo (DI.SrcID "Read" 0)
            (DI.DynamicInfo []
               [DI.genTerm x2, DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
         (do x13 <- Prelude.return x6
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Read" 0)
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- Prelude.return x4
                                  x12 <- do x9 <- do x7 <- Prelude.return x2
                                                     x8 <- DM.litHook
                                                             (DI.DebugInfo (DI.SrcID "Read" 0)
                                                                (DI.DynamicInfo [] []))
                                                             (Prelude.return
                                                                (Curry.DebugModule.Prelude.Pos
                                                                   (Curry.DebugModule.Prelude.O
                                                                      (Curry.DebugModule.Prelude.O
                                                                         (Curry.DebugModule.Prelude.O
                                                                            (Curry.DebugModule.Prelude.O
                                                                               Curry.DebugModule.Prelude.IHi))))))
                                                     DM.funcCallHook "*"
                                                       (DI.DebugInfo (DI.SrcID "Read" 0)
                                                          (DI.DynamicInfo []
                                                             [DI.genTerm x7, DI.genTerm x8]))
                                                       (Curry.DebugModule.Prelude.op_Asterisk x7 x8)
                                            x10 <- Prelude.return x5
                                            DM.funcCallHook "+"
                                              (DI.DebugInfo (DI.SrcID "Read" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x9, DI.genTerm x10]))
                                              (Curry.DebugModule.Prelude.op_Plus x9 x10)
                                  DM.funcCallHook "readHex.readHexPrefix.22"
                                    (DI.DebugInfo (DI.SrcID "Read" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11, DI.genTerm x12]))
                                    (x'xstrict_readHex46readHexPrefix4622 x11 x12)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Read" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_0 x2 x4 x5)
                           x13)))
term_strict__case_0 x1 = DI.Term "_case_0" (DI.SrcID "Read" 0) x1
strict__case_3 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_3"
         (DI.DebugInfo (DI.SrcID "Read" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x19 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Read" 0)
                  (DI.DynamicInfo [] [DI.genTerm x19]))
               (case x19 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- do x3 <- Prelude.return x1
                                           DM.funcCallHook "ord"
                                             (DI.DebugInfo (DI.SrcID "Read" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x3]))
                                             (Curry.DebugModule.Prelude.strict_ord x3)
                                  x6 <- do x4 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Read" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return
                                                      (Curry.DebugModule.Prelude.Char '0'))
                                           DM.funcCallHook "ord"
                                             (DI.DebugInfo (DI.SrcID "Read" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4]))
                                             (Curry.DebugModule.Prelude.strict_ord x4)
                                  DM.funcCallHook "-"
                                    (DI.DebugInfo (DI.SrcID "Read" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                    (Curry.DebugModule.Prelude.op_Minus x5 x6)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x17 <- Prelude.return x1
                                  x18 <- do x15 <- do x9 <- do x7 <- Prelude.return x1
                                                               DM.funcCallHook "ord"
                                                                 (DI.DebugInfo (DI.SrcID "Read" 0)
                                                                    (DI.DynamicInfo []
                                                                       [DI.genTerm x7]))
                                                                 (Curry.DebugModule.Prelude.strict_ord
                                                                    x7)
                                                      x10 <- do x8 <- DM.litHook
                                                                        (DI.DebugInfo
                                                                           (DI.SrcID "Read" 0)
                                                                           (DI.DynamicInfo [] []))
                                                                        (Prelude.return
                                                                           (Curry.DebugModule.Prelude.Char
                                                                              'A'))
                                                                DM.funcCallHook "ord"
                                                                  (DI.DebugInfo (DI.SrcID "Read" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x8]))
                                                                  (Curry.DebugModule.Prelude.strict_ord
                                                                     x8)
                                                      DM.funcCallHook ">="
                                                        (DI.DebugInfo (DI.SrcID "Read" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x9, DI.genTerm x10]))
                                                        (Curry.DebugModule.Prelude.op_GtEq x9 x10)
                                            x16 <- do x13 <- do x11 <- Prelude.return x1
                                                                DM.funcCallHook "ord"
                                                                  (DI.DebugInfo (DI.SrcID "Read" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x11]))
                                                                  (Curry.DebugModule.Prelude.strict_ord
                                                                     x11)
                                                      x14 <- do x12 <- DM.litHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Read" 0)
                                                                            (DI.DynamicInfo [] []))
                                                                         (Prelude.return
                                                                            (Curry.DebugModule.Prelude.Char
                                                                               'F'))
                                                                DM.funcCallHook "ord"
                                                                  (DI.DebugInfo (DI.SrcID "Read" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x12]))
                                                                  (Curry.DebugModule.Prelude.strict_ord
                                                                     x12)
                                                      DM.funcCallHook "<="
                                                        (DI.DebugInfo (DI.SrcID "Read" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x13, DI.genTerm x14]))
                                                        (Curry.DebugModule.Prelude.op_LtEq x13 x14)
                                            DM.funcCallHook "&&"
                                              (DI.DebugInfo (DI.SrcID "Read" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x15, DI.genTerm x16]))
                                              (Curry.DebugModule.Prelude.op_AndAnd x15 x16)
                                  DM.funcCallHook "_case_2"
                                    (DI.DebugInfo (DI.SrcID "Read" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x17, DI.genTerm x18]))
                                    (strict__case_2 x17 x18)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Read" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x19])))
                           (strict__case_3 x1)
                           x19)))
term_strict__case_3 x1 = DI.Term "_case_3" (DI.SrcID "Read" 0) x1
strict__case_2 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_2"
         (DI.DebugInfo (DI.SrcID "Read" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x10 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Read" 0)
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- do x5 <- do x3 <- Prelude.return x1
                                                    DM.funcCallHook "ord"
                                                      (DI.DebugInfo (DI.SrcID "Read" 0)
                                                         (DI.DynamicInfo [] [DI.genTerm x3]))
                                                      (Curry.DebugModule.Prelude.strict_ord x3)
                                           x6 <- do x4 <- DM.litHook
                                                            (DI.DebugInfo (DI.SrcID "Read" 0)
                                                               (DI.DynamicInfo [] []))
                                                            (Prelude.return
                                                               (Curry.DebugModule.Prelude.Char 'A'))
                                                    DM.funcCallHook "ord"
                                                      (DI.DebugInfo (DI.SrcID "Read" 0)
                                                         (DI.DynamicInfo [] [DI.genTerm x4]))
                                                      (Curry.DebugModule.Prelude.strict_ord x4)
                                           DM.funcCallHook "-"
                                             (DI.DebugInfo (DI.SrcID "Read" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                             (Curry.DebugModule.Prelude.op_Minus x5 x6)
                                  x8 <- DM.litHook
                                          (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                                          (Prelude.return
                                             (Curry.DebugModule.Prelude.Pos
                                                (Curry.DebugModule.Prelude.O
                                                   (Curry.DebugModule.Prelude.I
                                                      (Curry.DebugModule.Prelude.O
                                                         Curry.DebugModule.Prelude.IHi)))))
                                  DM.funcCallHook "+"
                                    (DI.DebugInfo (DI.SrcID "Read" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                    (Curry.DebugModule.Prelude.op_Plus x7 x8)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- DM.litHook
                                          (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                                          (Prelude.return
                                             (Curry.DebugModule.Prelude.Pos
                                                Curry.DebugModule.Prelude.IHi))
                                  DM.funcCallHook "negate"
                                    (DI.DebugInfo (DI.SrcID "Read" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9]))
                                    (Curry.DebugModule.Prelude.strict_negate x9)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Read" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_2 x1)
                           x10)))
term_strict__case_2 x1 = DI.Term "_case_2" (DI.SrcID "Read" 0) x1
strict__case_5 x1
  = DM.eval
      (DM.funcDeclHook "_case_5"
         (DI.DebugInfo (DI.SrcID "Read" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x9 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Read" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x9]))
               (case x9 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.litHook
                                 (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Zero)))
                    Curry.DebugModule.Prelude.Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x2
                                  x7 <- Prelude.return x3
                                  x8 <- do x4 <- Prelude.return x2
                                           x5 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Read" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return
                                                      (Curry.DebugModule.Prelude.Char '-'))
                                           DM.funcCallHook "=="
                                             (DI.DebugInfo (DI.SrcID "Read" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                             (Curry.DebugModule.Prelude.op_EqEq x4 x5)
                                  DM.funcCallHook "_case_4"
                                    (DI.DebugInfo (DI.SrcID "Read" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x6, DI.genTerm x7, DI.genTerm x8]))
                                    (strict__case_4 x6 x7 x8)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Read" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x9])))
                           strict__case_5
                           x9)))
term_strict__case_5 x1 = DI.Term "_case_5" (DI.SrcID "Read" 0) x1
strict__case_4 x2 x3 x4
  = DM.eval
      (DM.funcDeclHook "_case_4"
         (DI.DebugInfo (DI.SrcID "Read" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3, DI.genTerm x4]))
         (do x10 <- Prelude.return x4
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Read" 0)
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- do x5 <- Prelude.return x3
                                           DM.funcCallHook "readNat"
                                             (DI.DebugInfo (DI.SrcID "Read" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5]))
                                             (strict_readNat x5)
                                  DM.funcCallHook "negate"
                                    (DI.DebugInfo (DI.SrcID "Read" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6]))
                                    (Curry.DebugModule.Prelude.strict_negate x6)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- do x7 <- Prelude.return x2
                                           x8 <- Prelude.return x3
                                           DM.constructorHook
                                             (DI.DebugInfo (DI.SrcID "Read" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                             (Prelude.return (Curry.DebugModule.Prelude.Cons x7 x8))
                                  DM.funcCallHook "readNat"
                                    (DI.DebugInfo (DI.SrcID "Read" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9]))
                                    (strict_readNat x9)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Read" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_4 x2 x3)
                           x10)))
term_strict__case_4 x1 = DI.Term "_case_4" (DI.SrcID "Read" 0) x1
strict__case_7 x2 x1
  = DM.eval
      (DM.funcDeclHook "_case_7"
         (DI.DebugInfo (DI.SrcID "Read" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x1]))
         (do x19 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Read" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x19]))
               (case x19 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    Curry.DebugModule.Prelude.Cons x3 x4
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x5 <- do x6 <- Prelude.return x3
                                              DM.funcCallHook "ord"
                                                (DI.DebugInfo (DI.SrcID "Read" 0)
                                                   (DI.DynamicInfo [] [DI.genTerm x6]))
                                                (Curry.DebugModule.Prelude.strict_ord x6)
                                     DM.eval
                                       (do x15 <- Prelude.return x2
                                           x16 <- Prelude.return x4
                                           x17 <- Prelude.return x5
                                           x18 <- do x13 <- do x8 <- Prelude.return x5
                                                               x9 <- do x7 <- DM.litHook
                                                                                (DI.DebugInfo
                                                                                   (DI.SrcID "Read"
                                                                                      0)
                                                                                   (DI.DynamicInfo
                                                                                      []
                                                                                      []))
                                                                                (Prelude.return
                                                                                   (Curry.DebugModule.Prelude.Char
                                                                                      '0'))
                                                                        DM.funcCallHook "ord"
                                                                          (DI.DebugInfo
                                                                             (DI.SrcID "Read" 0)
                                                                             (DI.DynamicInfo []
                                                                                [DI.genTerm x7]))
                                                                          (Curry.DebugModule.Prelude.strict_ord
                                                                             x7)
                                                               DM.funcCallHook ">="
                                                                 (DI.DebugInfo (DI.SrcID "Read" 0)
                                                                    (DI.DynamicInfo []
                                                                       [DI.genTerm x8,
                                                                        DI.genTerm x9]))
                                                                 (Curry.DebugModule.Prelude.op_GtEq
                                                                    x8
                                                                    x9)
                                                     x14 <- do x11 <- Prelude.return x5
                                                               x12 <- do x10 <- DM.litHook
                                                                                  (DI.DebugInfo
                                                                                     (DI.SrcID
                                                                                        "Read"
                                                                                        0)
                                                                                     (DI.DynamicInfo
                                                                                        []
                                                                                        []))
                                                                                  (Prelude.return
                                                                                     (Curry.DebugModule.Prelude.Char
                                                                                        '9'))
                                                                         DM.funcCallHook "ord"
                                                                           (DI.DebugInfo
                                                                              (DI.SrcID "Read" 0)
                                                                              (DI.DynamicInfo []
                                                                                 [DI.genTerm x10]))
                                                                           (Curry.DebugModule.Prelude.strict_ord
                                                                              x10)
                                                               DM.funcCallHook "<="
                                                                 (DI.DebugInfo (DI.SrcID "Read" 0)
                                                                    (DI.DynamicInfo []
                                                                       [DI.genTerm x11,
                                                                        DI.genTerm x12]))
                                                                 (Curry.DebugModule.Prelude.op_LtEq
                                                                    x11
                                                                    x12)
                                                     DM.funcCallHook "&&"
                                                       (DI.DebugInfo (DI.SrcID "Read" 0)
                                                          (DI.DynamicInfo []
                                                             [DI.genTerm x13, DI.genTerm x14]))
                                                       (Curry.DebugModule.Prelude.op_AndAnd x13 x14)
                                           DM.funcCallHook "_case_6"
                                             (DI.DebugInfo (DI.SrcID "Read" 0)
                                                (DI.DynamicInfo []
                                                   [DI.genTerm x15, DI.genTerm x16, DI.genTerm x17,
                                                    DI.genTerm x18]))
                                             (strict__case_6 x15 x16 x17 x18)))))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Read" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x19])))
                           (strict__case_7 x2)
                           x19)))
term_strict__case_7 x1 = DI.Term "_case_7" (DI.SrcID "Read" 0) x1
strict__case_6 x2 x4 x5 x6
  = DM.eval
      (DM.funcDeclHook "_case_6"
         (DI.DebugInfo (DI.SrcID "Read" 0)
            (DI.DynamicInfo []
               [DI.genTerm x2, DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
         (do x16 <- Prelude.return x6
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Read" 0)
                  (DI.DynamicInfo [] [DI.genTerm x16]))
               (case x16 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x14 <- Prelude.return x4
                                  x15 <- do x12 <- do x9 <- do x7 <- Prelude.return x2
                                                               x8 <- DM.litHook
                                                                       (DI.DebugInfo
                                                                          (DI.SrcID "Read" 0)
                                                                          (DI.DynamicInfo [] []))
                                                                       (Prelude.return
                                                                          (Curry.DebugModule.Prelude.Pos
                                                                             (Curry.DebugModule.Prelude.O
                                                                                (Curry.DebugModule.Prelude.I
                                                                                   (Curry.DebugModule.Prelude.O
                                                                                      Curry.DebugModule.Prelude.IHi)))))
                                                               DM.funcCallHook "*"
                                                                 (DI.DebugInfo (DI.SrcID "Read" 0)
                                                                    (DI.DynamicInfo []
                                                                       [DI.genTerm x7,
                                                                        DI.genTerm x8]))
                                                                 (Curry.DebugModule.Prelude.op_Asterisk
                                                                    x7
                                                                    x8)
                                                      x10 <- Prelude.return x5
                                                      DM.funcCallHook "+"
                                                        (DI.DebugInfo (DI.SrcID "Read" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x9, DI.genTerm x10]))
                                                        (Curry.DebugModule.Prelude.op_Plus x9 x10)
                                            x13 <- do x11 <- DM.litHook
                                                               (DI.DebugInfo (DI.SrcID "Read" 0)
                                                                  (DI.DynamicInfo [] []))
                                                               (Prelude.return
                                                                  (Curry.DebugModule.Prelude.Char
                                                                     '0'))
                                                      DM.funcCallHook "ord"
                                                        (DI.DebugInfo (DI.SrcID "Read" 0)
                                                           (DI.DynamicInfo [] [DI.genTerm x11]))
                                                        (Curry.DebugModule.Prelude.strict_ord x11)
                                            DM.funcCallHook "-"
                                              (DI.DebugInfo (DI.SrcID "Read" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x12, DI.genTerm x13]))
                                              (Curry.DebugModule.Prelude.op_Minus x12 x13)
                                  DM.funcCallHook "readNat.readNatPrefix.3"
                                    (DI.DebugInfo (DI.SrcID "Read" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x14, DI.genTerm x15]))
                                    (x'xstrict_readNat46readNatPrefix463 x14 x15)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Read" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Read" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x16])))
                           (strict__case_6 x2 x4 x5)
                           x16)))
term_strict__case_6 x1 = DI.Term "_case_6" (DI.SrcID "Read" 0) x1