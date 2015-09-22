{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Curry.DebugModule.Char where
import qualified Prelude
import qualified Curry.Debugger.DebugMonad as DM
import qualified Curry.Debugger.DebugInfo as DI
import qualified Curry.Debugger.PartCalls as PC
import qualified Data.Generics
import qualified Curry.DebugModule.Prelude
 
strict_isUpper ::
               (DM.DM dm) =>
                 Curry.DebugModule.Prelude.Char -> dm Curry.DebugModule.Prelude.Bool
strict_isUpper x1
  = DM.eval
      (DM.funcDeclHook "isUpper"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x10 <- do x4 <- do x2 <- Prelude.return x1
                                DM.funcCallHook "ord"
                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x2]))
                                  (Curry.DebugModule.Prelude.strict_ord x2)
                       x5 <- do x3 <- DM.litHook
                                        (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                        (Prelude.return (Curry.DebugModule.Prelude.Char 'A'))
                                DM.funcCallHook "ord"
                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x3]))
                                  (Curry.DebugModule.Prelude.strict_ord x3)
                       DM.funcCallHook ">="
                         (DI.DebugInfo (DI.SrcID "Char" 0)
                            (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                         (Curry.DebugModule.Prelude.op_GtEq x4 x5)
             x11 <- do x8 <- do x6 <- Prelude.return x1
                                DM.funcCallHook "ord"
                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x6]))
                                  (Curry.DebugModule.Prelude.strict_ord x6)
                       x9 <- do x7 <- DM.litHook
                                        (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                        (Prelude.return (Curry.DebugModule.Prelude.Char 'Z'))
                                DM.funcCallHook "ord"
                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x7]))
                                  (Curry.DebugModule.Prelude.strict_ord x7)
                       DM.funcCallHook "<="
                         (DI.DebugInfo (DI.SrcID "Char" 0)
                            (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                         (Curry.DebugModule.Prelude.op_LtEq x8 x9)
             DM.funcCallHook "&&"
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
               (Curry.DebugModule.Prelude.op_AndAnd x10 x11)))
term_strict_isUpper x1 = DI.Term "isUpper" (DI.SrcID "Char" 0) x1
 
strict_isLower ::
               (DM.DM dm) =>
                 Curry.DebugModule.Prelude.Char -> dm Curry.DebugModule.Prelude.Bool
strict_isLower x1
  = DM.eval
      (DM.funcDeclHook "isLower"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x10 <- do x4 <- do x2 <- Prelude.return x1
                                DM.funcCallHook "ord"
                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x2]))
                                  (Curry.DebugModule.Prelude.strict_ord x2)
                       x5 <- do x3 <- DM.litHook
                                        (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                        (Prelude.return (Curry.DebugModule.Prelude.Char 'a'))
                                DM.funcCallHook "ord"
                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x3]))
                                  (Curry.DebugModule.Prelude.strict_ord x3)
                       DM.funcCallHook ">="
                         (DI.DebugInfo (DI.SrcID "Char" 0)
                            (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                         (Curry.DebugModule.Prelude.op_GtEq x4 x5)
             x11 <- do x8 <- do x6 <- Prelude.return x1
                                DM.funcCallHook "ord"
                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x6]))
                                  (Curry.DebugModule.Prelude.strict_ord x6)
                       x9 <- do x7 <- DM.litHook
                                        (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                        (Prelude.return (Curry.DebugModule.Prelude.Char 'z'))
                                DM.funcCallHook "ord"
                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x7]))
                                  (Curry.DebugModule.Prelude.strict_ord x7)
                       DM.funcCallHook "<="
                         (DI.DebugInfo (DI.SrcID "Char" 0)
                            (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                         (Curry.DebugModule.Prelude.op_LtEq x8 x9)
             DM.funcCallHook "&&"
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
               (Curry.DebugModule.Prelude.op_AndAnd x10 x11)))
term_strict_isLower x1 = DI.Term "isLower" (DI.SrcID "Char" 0) x1
 
strict_isAlpha ::
               (DM.DM dm) =>
                 Curry.DebugModule.Prelude.Char -> dm Curry.DebugModule.Prelude.Bool
strict_isAlpha x1
  = DM.eval
      (DM.funcDeclHook "isAlpha"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- do x2 <- Prelude.return x1
                      DM.funcCallHook "isUpper"
                        (DI.DebugInfo (DI.SrcID "Char" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2]))
                        (strict_isUpper x2)
             x5 <- do x3 <- Prelude.return x1
                      DM.funcCallHook "isLower"
                        (DI.DebugInfo (DI.SrcID "Char" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3]))
                        (strict_isLower x3)
             DM.funcCallHook "||"
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
               (Curry.DebugModule.Prelude.op_OrOr x4 x5)))
term_strict_isAlpha x1 = DI.Term "isAlpha" (DI.SrcID "Char" 0) x1
 
strict_isDigit ::
               (DM.DM dm) =>
                 Curry.DebugModule.Prelude.Char -> dm Curry.DebugModule.Prelude.Bool
strict_isDigit x1
  = DM.eval
      (DM.funcDeclHook "isDigit"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x10 <- do x4 <- do x2 <- DM.litHook
                                        (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                        (Prelude.return (Curry.DebugModule.Prelude.Char '0'))
                                DM.funcCallHook "ord"
                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x2]))
                                  (Curry.DebugModule.Prelude.strict_ord x2)
                       x5 <- do x3 <- Prelude.return x1
                                DM.funcCallHook "ord"
                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x3]))
                                  (Curry.DebugModule.Prelude.strict_ord x3)
                       DM.funcCallHook "<="
                         (DI.DebugInfo (DI.SrcID "Char" 0)
                            (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                         (Curry.DebugModule.Prelude.op_LtEq x4 x5)
             x11 <- do x8 <- do x6 <- Prelude.return x1
                                DM.funcCallHook "ord"
                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x6]))
                                  (Curry.DebugModule.Prelude.strict_ord x6)
                       x9 <- do x7 <- DM.litHook
                                        (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                        (Prelude.return (Curry.DebugModule.Prelude.Char '9'))
                                DM.funcCallHook "ord"
                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x7]))
                                  (Curry.DebugModule.Prelude.strict_ord x7)
                       DM.funcCallHook "<="
                         (DI.DebugInfo (DI.SrcID "Char" 0)
                            (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                         (Curry.DebugModule.Prelude.op_LtEq x8 x9)
             DM.funcCallHook "&&"
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
               (Curry.DebugModule.Prelude.op_AndAnd x10 x11)))
term_strict_isDigit x1 = DI.Term "isDigit" (DI.SrcID "Char" 0) x1
 
strict_isAlphaNum ::
                  (DM.DM dm) =>
                    Curry.DebugModule.Prelude.Char -> dm Curry.DebugModule.Prelude.Bool
strict_isAlphaNum x1
  = DM.eval
      (DM.funcDeclHook "isAlphaNum"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- do x2 <- Prelude.return x1
                      DM.funcCallHook "isAlpha"
                        (DI.DebugInfo (DI.SrcID "Char" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2]))
                        (strict_isAlpha x2)
             x5 <- do x3 <- Prelude.return x1
                      DM.funcCallHook "isDigit"
                        (DI.DebugInfo (DI.SrcID "Char" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3]))
                        (strict_isDigit x3)
             DM.funcCallHook "||"
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
               (Curry.DebugModule.Prelude.op_OrOr x4 x5)))
term_strict_isAlphaNum x1
  = DI.Term "isAlphaNum" (DI.SrcID "Char" 0) x1
 
strict_isOctDigit ::
                  (DM.DM dm) =>
                    Curry.DebugModule.Prelude.Char -> dm Curry.DebugModule.Prelude.Bool
strict_isOctDigit x1
  = DM.eval
      (DM.funcDeclHook "isOctDigit"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x10 <- do x4 <- do x2 <- Prelude.return x1
                                DM.funcCallHook "ord"
                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x2]))
                                  (Curry.DebugModule.Prelude.strict_ord x2)
                       x5 <- do x3 <- DM.litHook
                                        (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                        (Prelude.return (Curry.DebugModule.Prelude.Char '0'))
                                DM.funcCallHook "ord"
                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x3]))
                                  (Curry.DebugModule.Prelude.strict_ord x3)
                       DM.funcCallHook ">="
                         (DI.DebugInfo (DI.SrcID "Char" 0)
                            (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                         (Curry.DebugModule.Prelude.op_GtEq x4 x5)
             x11 <- do x8 <- do x6 <- Prelude.return x1
                                DM.funcCallHook "ord"
                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x6]))
                                  (Curry.DebugModule.Prelude.strict_ord x6)
                       x9 <- do x7 <- DM.litHook
                                        (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                        (Prelude.return (Curry.DebugModule.Prelude.Char '7'))
                                DM.funcCallHook "ord"
                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x7]))
                                  (Curry.DebugModule.Prelude.strict_ord x7)
                       DM.funcCallHook "<="
                         (DI.DebugInfo (DI.SrcID "Char" 0)
                            (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                         (Curry.DebugModule.Prelude.op_LtEq x8 x9)
             DM.funcCallHook "&&"
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
               (Curry.DebugModule.Prelude.op_AndAnd x10 x11)))
term_strict_isOctDigit x1
  = DI.Term "isOctDigit" (DI.SrcID "Char" 0) x1
 
strict_isHexDigit ::
                  (DM.DM dm) =>
                    Curry.DebugModule.Prelude.Char -> dm Curry.DebugModule.Prelude.Bool
strict_isHexDigit x1
  = DM.eval
      (DM.funcDeclHook "isHexDigit"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x25 <- do x2 <- Prelude.return x1
                       DM.funcCallHook "isDigit"
                         (DI.DebugInfo (DI.SrcID "Char" 0)
                            (DI.DynamicInfo [] [DI.genTerm x2]))
                         (strict_isDigit x2)
             x26 <- do x23 <- do x11 <- do x5 <- do x3 <- Prelude.return x1
                                                    DM.funcCallHook "ord"
                                                      (DI.DebugInfo (DI.SrcID "Char" 0)
                                                         (DI.DynamicInfo [] [DI.genTerm x3]))
                                                      (Curry.DebugModule.Prelude.strict_ord x3)
                                           x6 <- do x4 <- DM.litHook
                                                            (DI.DebugInfo (DI.SrcID "Char" 0)
                                                               (DI.DynamicInfo [] []))
                                                            (Prelude.return
                                                               (Curry.DebugModule.Prelude.Char 'A'))
                                                    DM.funcCallHook "ord"
                                                      (DI.DebugInfo (DI.SrcID "Char" 0)
                                                         (DI.DynamicInfo [] [DI.genTerm x4]))
                                                      (Curry.DebugModule.Prelude.strict_ord x4)
                                           DM.funcCallHook ">="
                                             (DI.DebugInfo (DI.SrcID "Char" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                             (Curry.DebugModule.Prelude.op_GtEq x5 x6)
                                 x12 <- do x9 <- do x7 <- Prelude.return x1
                                                    DM.funcCallHook "ord"
                                                      (DI.DebugInfo (DI.SrcID "Char" 0)
                                                         (DI.DynamicInfo [] [DI.genTerm x7]))
                                                      (Curry.DebugModule.Prelude.strict_ord x7)
                                           x10 <- do x8 <- DM.litHook
                                                             (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                (DI.DynamicInfo [] []))
                                                             (Prelude.return
                                                                (Curry.DebugModule.Prelude.Char
                                                                   'F'))
                                                     DM.funcCallHook "ord"
                                                       (DI.DebugInfo (DI.SrcID "Char" 0)
                                                          (DI.DynamicInfo [] [DI.genTerm x8]))
                                                       (Curry.DebugModule.Prelude.strict_ord x8)
                                           DM.funcCallHook "<="
                                             (DI.DebugInfo (DI.SrcID "Char" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
                                             (Curry.DebugModule.Prelude.op_LtEq x9 x10)
                                 DM.funcCallHook "&&"
                                   (DI.DebugInfo (DI.SrcID "Char" 0)
                                      (DI.DynamicInfo [] [DI.genTerm x11, DI.genTerm x12]))
                                   (Curry.DebugModule.Prelude.op_AndAnd x11 x12)
                       x24 <- do x21 <- do x15 <- do x13 <- Prelude.return x1
                                                     DM.funcCallHook "ord"
                                                       (DI.DebugInfo (DI.SrcID "Char" 0)
                                                          (DI.DynamicInfo [] [DI.genTerm x13]))
                                                       (Curry.DebugModule.Prelude.strict_ord x13)
                                           x16 <- do x14 <- DM.litHook
                                                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                 (DI.DynamicInfo [] []))
                                                              (Prelude.return
                                                                 (Curry.DebugModule.Prelude.Char
                                                                    'a'))
                                                     DM.funcCallHook "ord"
                                                       (DI.DebugInfo (DI.SrcID "Char" 0)
                                                          (DI.DynamicInfo [] [DI.genTerm x14]))
                                                       (Curry.DebugModule.Prelude.strict_ord x14)
                                           DM.funcCallHook ">="
                                             (DI.DebugInfo (DI.SrcID "Char" 0)
                                                (DI.DynamicInfo []
                                                   [DI.genTerm x15, DI.genTerm x16]))
                                             (Curry.DebugModule.Prelude.op_GtEq x15 x16)
                                 x22 <- do x19 <- do x17 <- Prelude.return x1
                                                     DM.funcCallHook "ord"
                                                       (DI.DebugInfo (DI.SrcID "Char" 0)
                                                          (DI.DynamicInfo [] [DI.genTerm x17]))
                                                       (Curry.DebugModule.Prelude.strict_ord x17)
                                           x20 <- do x18 <- DM.litHook
                                                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                 (DI.DynamicInfo [] []))
                                                              (Prelude.return
                                                                 (Curry.DebugModule.Prelude.Char
                                                                    'f'))
                                                     DM.funcCallHook "ord"
                                                       (DI.DebugInfo (DI.SrcID "Char" 0)
                                                          (DI.DynamicInfo [] [DI.genTerm x18]))
                                                       (Curry.DebugModule.Prelude.strict_ord x18)
                                           DM.funcCallHook "<="
                                             (DI.DebugInfo (DI.SrcID "Char" 0)
                                                (DI.DynamicInfo []
                                                   [DI.genTerm x19, DI.genTerm x20]))
                                             (Curry.DebugModule.Prelude.op_LtEq x19 x20)
                                 DM.funcCallHook "&&"
                                   (DI.DebugInfo (DI.SrcID "Char" 0)
                                      (DI.DynamicInfo [] [DI.genTerm x21, DI.genTerm x22]))
                                   (Curry.DebugModule.Prelude.op_AndAnd x21 x22)
                       DM.funcCallHook "||"
                         (DI.DebugInfo (DI.SrcID "Char" 0)
                            (DI.DynamicInfo [] [DI.genTerm x23, DI.genTerm x24]))
                         (Curry.DebugModule.Prelude.op_OrOr x23 x24)
             DM.funcCallHook "||"
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x25, DI.genTerm x26]))
               (Curry.DebugModule.Prelude.op_OrOr x25 x26)))
term_strict_isHexDigit x1
  = DI.Term "isHexDigit" (DI.SrcID "Char" 0) x1
 
strict_isSpace ::
               (DM.DM dm) =>
                 Curry.DebugModule.Prelude.Char -> dm Curry.DebugModule.Prelude.Bool
strict_isSpace x1
  = DM.eval
      (DM.funcDeclHook "isSpace"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x19 <- do x2 <- Prelude.return x1
                       x3 <- DM.litHook
                               (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                               (Prelude.return (Curry.DebugModule.Prelude.Char ' '))
                       DM.funcCallHook "=="
                         (DI.DebugInfo (DI.SrcID "Char" 0)
                            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                         (Curry.DebugModule.Prelude.op_EqEq x2 x3)
             x20 <- do x17 <- do x4 <- Prelude.return x1
                                 x5 <- DM.litHook
                                         (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                         (Prelude.return (Curry.DebugModule.Prelude.Char '\t'))
                                 DM.funcCallHook "=="
                                   (DI.DebugInfo (DI.SrcID "Char" 0)
                                      (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                   (Curry.DebugModule.Prelude.op_EqEq x4 x5)
                       x18 <- do x15 <- do x6 <- Prelude.return x1
                                           x7 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Char" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return
                                                      (Curry.DebugModule.Prelude.Char '\n'))
                                           DM.funcCallHook "=="
                                             (DI.DebugInfo (DI.SrcID "Char" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                             (Curry.DebugModule.Prelude.op_EqEq x6 x7)
                                 x16 <- do x13 <- do x8 <- Prelude.return x1
                                                     x9 <- DM.litHook
                                                             (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                (DI.DynamicInfo [] []))
                                                             (Prelude.return
                                                                (Curry.DebugModule.Prelude.Char
                                                                   '\r'))
                                                     DM.funcCallHook "=="
                                                       (DI.DebugInfo (DI.SrcID "Char" 0)
                                                          (DI.DynamicInfo []
                                                             [DI.genTerm x8, DI.genTerm x9]))
                                                       (Curry.DebugModule.Prelude.op_EqEq x8 x9)
                                           x14 <- do x11 <- do x10 <- Prelude.return x1
                                                               DM.funcCallHook "ord"
                                                                 (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                    (DI.DynamicInfo []
                                                                       [DI.genTerm x10]))
                                                                 (Curry.DebugModule.Prelude.strict_ord
                                                                    x10)
                                                     x12 <- DM.litHook
                                                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                 (DI.DynamicInfo [] []))
                                                              (Prelude.return
                                                                 (Curry.DebugModule.Prelude.Pos
                                                                    (Curry.DebugModule.Prelude.O
                                                                       (Curry.DebugModule.Prelude.O
                                                                          (Curry.DebugModule.Prelude.I
                                                                             Curry.DebugModule.Prelude.IHi)))))
                                                     DM.funcCallHook "=="
                                                       (DI.DebugInfo (DI.SrcID "Char" 0)
                                                          (DI.DynamicInfo []
                                                             [DI.genTerm x11, DI.genTerm x12]))
                                                       (Curry.DebugModule.Prelude.op_EqEq x11 x12)
                                           DM.funcCallHook "||"
                                             (DI.DebugInfo (DI.SrcID "Char" 0)
                                                (DI.DynamicInfo []
                                                   [DI.genTerm x13, DI.genTerm x14]))
                                             (Curry.DebugModule.Prelude.op_OrOr x13 x14)
                                 DM.funcCallHook "||"
                                   (DI.DebugInfo (DI.SrcID "Char" 0)
                                      (DI.DynamicInfo [] [DI.genTerm x15, DI.genTerm x16]))
                                   (Curry.DebugModule.Prelude.op_OrOr x15 x16)
                       DM.funcCallHook "||"
                         (DI.DebugInfo (DI.SrcID "Char" 0)
                            (DI.DynamicInfo [] [DI.genTerm x17, DI.genTerm x18]))
                         (Curry.DebugModule.Prelude.op_OrOr x17 x18)
             DM.funcCallHook "||"
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x19, DI.genTerm x20]))
               (Curry.DebugModule.Prelude.op_OrOr x19 x20)))
term_strict_isSpace x1 = DI.Term "isSpace" (DI.SrcID "Char" 0) x1
 
strict_toUpper ::
               (DM.DM dm) =>
                 Curry.DebugModule.Prelude.Char -> dm Curry.DebugModule.Prelude.Char
strict_toUpper x1
  = DM.eval
      (DM.funcDeclHook "toUpper"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- Prelude.return x1
             x4 <- do x2 <- Prelude.return x1
                      DM.funcCallHook "isLower"
                        (DI.DebugInfo (DI.SrcID "Char" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2]))
                        (strict_isLower x2)
             DM.funcCallHook "_case_10"
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_10 x3 x4)))
term_strict_toUpper x1 = DI.Term "toUpper" (DI.SrcID "Char" 0) x1
 
strict_toLower ::
               (DM.DM dm) =>
                 Curry.DebugModule.Prelude.Char -> dm Curry.DebugModule.Prelude.Char
strict_toLower x1
  = DM.eval
      (DM.funcDeclHook "toLower"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- Prelude.return x1
             x4 <- do x2 <- Prelude.return x1
                      DM.funcCallHook "isUpper"
                        (DI.DebugInfo (DI.SrcID "Char" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2]))
                        (strict_isUpper x2)
             DM.funcCallHook "_case_8"
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_8 x3 x4)))
term_strict_toLower x1 = DI.Term "toLower" (DI.SrcID "Char" 0) x1
 
strict_digitToInt ::
                  (DM.DM dm) =>
                    Curry.DebugModule.Prelude.Char -> dm Curry.DebugModule.Prelude.Int
strict_digitToInt x1
  = DM.eval
      (DM.funcDeclHook "digitToInt"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x3 <- Prelude.return x1
             x4 <- do x2 <- Prelude.return x1
                      DM.funcCallHook "isDigit"
                        (DI.DebugInfo (DI.SrcID "Char" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2]))
                        (strict_isDigit x2)
             DM.funcCallHook "_case_6"
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
               (strict__case_6 x3 x4)))
term_strict_digitToInt x1
  = DI.Term "digitToInt" (DI.SrcID "Char" 0) x1
 
strict_intToDigit ::
                  (DM.DM dm) =>
                    Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Char
strict_intToDigit x1
  = DM.eval
      (DM.funcDeclHook "intToDigit"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x8 <- Prelude.return x1
             x9 <- do x6 <- do x2 <- Prelude.return x1
                               x3 <- DM.litHook
                                       (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                       (Prelude.return Curry.DebugModule.Prelude.Zero)
                               DM.funcCallHook ">="
                                 (DI.DebugInfo (DI.SrcID "Char" 0)
                                    (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                                 (Curry.DebugModule.Prelude.op_GtEq x2 x3)
                      x7 <- do x4 <- Prelude.return x1
                               x5 <- DM.litHook
                                       (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                       (Prelude.return
                                          (Curry.DebugModule.Prelude.Pos
                                             (Curry.DebugModule.Prelude.I
                                                (Curry.DebugModule.Prelude.O
                                                   (Curry.DebugModule.Prelude.O
                                                      Curry.DebugModule.Prelude.IHi)))))
                               DM.funcCallHook "<="
                                 (DI.DebugInfo (DI.SrcID "Char" 0)
                                    (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                 (Curry.DebugModule.Prelude.op_LtEq x4 x5)
                      DM.funcCallHook "&&"
                        (DI.DebugInfo (DI.SrcID "Char" 0)
                           (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                        (Curry.DebugModule.Prelude.op_AndAnd x6 x7)
             DM.funcCallHook "_case_2"
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
               (strict__case_2 x8 x9)))
term_strict_intToDigit x1
  = DI.Term "intToDigit" (DI.SrcID "Char" 0) x1
strict__case_2 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_2"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x15 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x15]))
               (case x15 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- do x4 <- do x3 <- DM.litHook
                                                            (DI.DebugInfo (DI.SrcID "Char" 0)
                                                               (DI.DynamicInfo [] []))
                                                            (Prelude.return
                                                               (Curry.DebugModule.Prelude.Char '0'))
                                                    DM.funcCallHook "ord"
                                                      (DI.DebugInfo (DI.SrcID "Char" 0)
                                                         (DI.DynamicInfo [] [DI.genTerm x3]))
                                                      (Curry.DebugModule.Prelude.strict_ord x3)
                                           x5 <- Prelude.return x1
                                           DM.funcCallHook "+"
                                             (DI.DebugInfo (DI.SrcID "Char" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                             (Curry.DebugModule.Prelude.op_Plus x4 x5)
                                  DM.funcCallHook "chr"
                                    (DI.DebugInfo (DI.SrcID "Char" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6]))
                                    (Curry.DebugModule.Prelude.strict_chr x6)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x13 <- Prelude.return x1
                                  x14 <- do x11 <- do x7 <- Prelude.return x1
                                                      x8 <- DM.litHook
                                                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                 (DI.DynamicInfo [] []))
                                                              (Prelude.return
                                                                 (Curry.DebugModule.Prelude.Pos
                                                                    (Curry.DebugModule.Prelude.O
                                                                       (Curry.DebugModule.Prelude.I
                                                                          (Curry.DebugModule.Prelude.O
                                                                             Curry.DebugModule.Prelude.IHi)))))
                                                      DM.funcCallHook ">="
                                                        (DI.DebugInfo (DI.SrcID "Char" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x7, DI.genTerm x8]))
                                                        (Curry.DebugModule.Prelude.op_GtEq x7 x8)
                                            x12 <- do x9 <- Prelude.return x1
                                                      x10 <- DM.litHook
                                                               (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                  (DI.DynamicInfo [] []))
                                                               (Prelude.return
                                                                  (Curry.DebugModule.Prelude.Pos
                                                                     (Curry.DebugModule.Prelude.I
                                                                        (Curry.DebugModule.Prelude.I
                                                                           (Curry.DebugModule.Prelude.I
                                                                              Curry.DebugModule.Prelude.IHi)))))
                                                      DM.funcCallHook "<="
                                                        (DI.DebugInfo (DI.SrcID "Char" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x9, DI.genTerm x10]))
                                                        (Curry.DebugModule.Prelude.op_LtEq x9 x10)
                                            DM.funcCallHook "&&"
                                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x11, DI.genTerm x12]))
                                              (Curry.DebugModule.Prelude.op_AndAnd x11 x12)
                                  DM.funcCallHook "_case_1"
                                    (DI.DebugInfo (DI.SrcID "Char" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x13, DI.genTerm x14]))
                                    (strict__case_1 x13 x14)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x15])))
                           (strict__case_2 x1)
                           x15)))
term_strict__case_2 x1 = DI.Term "_case_2" (DI.SrcID "Char" 0) x1
strict__case_1 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_1"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x10 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x8 <- do x6 <- do x4 <- do x3 <- DM.litHook
                                                                     (DI.DebugInfo
                                                                        (DI.SrcID "Char" 0)
                                                                        (DI.DynamicInfo [] []))
                                                                     (Prelude.return
                                                                        (Curry.DebugModule.Prelude.Char
                                                                           'A'))
                                                             DM.funcCallHook "ord"
                                                               (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                  (DI.DynamicInfo []
                                                                     [DI.genTerm x3]))
                                                               (Curry.DebugModule.Prelude.strict_ord
                                                                  x3)
                                                    x5 <- Prelude.return x1
                                                    DM.funcCallHook "+"
                                                      (DI.DebugInfo (DI.SrcID "Char" 0)
                                                         (DI.DynamicInfo []
                                                            [DI.genTerm x4, DI.genTerm x5]))
                                                      (Curry.DebugModule.Prelude.op_Plus x4 x5)
                                           x7 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Char" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return
                                                      (Curry.DebugModule.Prelude.Pos
                                                         (Curry.DebugModule.Prelude.O
                                                            (Curry.DebugModule.Prelude.I
                                                               (Curry.DebugModule.Prelude.O
                                                                  Curry.DebugModule.Prelude.IHi)))))
                                           DM.funcCallHook "-"
                                             (DI.DebugInfo (DI.SrcID "Char" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                             (Curry.DebugModule.Prelude.op_Minus x6 x7)
                                  DM.funcCallHook "chr"
                                    (DI.DebugInfo (DI.SrcID "Char" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x8]))
                                    (Curry.DebugModule.Prelude.strict_chr x8)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- DM.funcCallHook "otherwise"
                                          (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                          Curry.DebugModule.Prelude.strict_otherwise
                                  DM.funcCallHook "_case_0"
                                    (DI.DebugInfo (DI.SrcID "Char" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9]))
                                    (strict__case_0 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_1 x1)
                           x10)))
term_strict__case_1 x1 = DI.Term "_case_1" (DI.SrcID "Char" 0) x1
strict__case_0 x1
  = DM.eval
      (DM.funcDeclHook "_case_0"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x89 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x89]))
               (case x89 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x88 <- do x86 <- DM.litHook
                                                     (DI.DebugInfo (DI.SrcID "Char" 0)
                                                        (DI.DynamicInfo [] []))
                                                     (Prelude.return
                                                        (Curry.DebugModule.Prelude.Char 'C'))
                                            x87 <- do x84 <- DM.litHook
                                                               (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                  (DI.DynamicInfo [] []))
                                                               (Prelude.return
                                                                  (Curry.DebugModule.Prelude.Char
                                                                     'h'))
                                                      x85 <- do x82 <- DM.litHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Char" 0)
                                                                            (DI.DynamicInfo [] []))
                                                                         (Prelude.return
                                                                            (Curry.DebugModule.Prelude.Char
                                                                               'a'))
                                                                x83 <- do x80 <- DM.litHook
                                                                                   (DI.DebugInfo
                                                                                      (DI.SrcID
                                                                                         "Char"
                                                                                         0)
                                                                                      (DI.DynamicInfo
                                                                                         []
                                                                                         []))
                                                                                   (Prelude.return
                                                                                      (Curry.DebugModule.Prelude.Char
                                                                                         'r'))
                                                                          x81 <- do x78 <- DM.litHook
                                                                                             (DI.DebugInfo
                                                                                                (DI.SrcID
                                                                                                   "Char"
                                                                                                   0)
                                                                                                (DI.DynamicInfo
                                                                                                   []
                                                                                                   []))
                                                                                             (Prelude.return
                                                                                                (Curry.DebugModule.Prelude.Char
                                                                                                   '.'))
                                                                                    x79 <- do x76 <- DM.litHook
                                                                                                       (DI.DebugInfo
                                                                                                          (DI.SrcID
                                                                                                             "Char"
                                                                                                             0)
                                                                                                          (DI.DynamicInfo
                                                                                                             []
                                                                                                             []))
                                                                                                       (Prelude.return
                                                                                                          (Curry.DebugModule.Prelude.Char
                                                                                                             'i'))
                                                                                              x77 <- do x74 <- DM.litHook
                                                                                                                 (DI.DebugInfo
                                                                                                                    (DI.SrcID
                                                                                                                       "Char"
                                                                                                                       0)
                                                                                                                    (DI.DynamicInfo
                                                                                                                       []
                                                                                                                       []))
                                                                                                                 (Prelude.return
                                                                                                                    (Curry.DebugModule.Prelude.Char
                                                                                                                       'n'))
                                                                                                        x75 <- do x72 <- DM.litHook
                                                                                                                           (DI.DebugInfo
                                                                                                                              (DI.SrcID
                                                                                                                                 "Char"
                                                                                                                                 0)
                                                                                                                              (DI.DynamicInfo
                                                                                                                                 []
                                                                                                                                 []))
                                                                                                                           (Prelude.return
                                                                                                                              (Curry.DebugModule.Prelude.Char
                                                                                                                                 't'))
                                                                                                                  x73 <- do x70 <- DM.litHook
                                                                                                                                     (DI.DebugInfo
                                                                                                                                        (DI.SrcID
                                                                                                                                           "Char"
                                                                                                                                           0)
                                                                                                                                        (DI.DynamicInfo
                                                                                                                                           []
                                                                                                                                           []))
                                                                                                                                     (Prelude.return
                                                                                                                                        (Curry.DebugModule.Prelude.Char
                                                                                                                                           'T'))
                                                                                                                            x71 <- do x68 <- DM.litHook
                                                                                                                                               (DI.DebugInfo
                                                                                                                                                  (DI.SrcID
                                                                                                                                                     "Char"
                                                                                                                                                     0)
                                                                                                                                                  (DI.DynamicInfo
                                                                                                                                                     []
                                                                                                                                                     []))
                                                                                                                                               (Prelude.return
                                                                                                                                                  (Curry.DebugModule.Prelude.Char
                                                                                                                                                     'o'))
                                                                                                                                      x69 <- do x66 <- DM.litHook
                                                                                                                                                         (DI.DebugInfo
                                                                                                                                                            (DI.SrcID
                                                                                                                                                               "Char"
                                                                                                                                                               0)
                                                                                                                                                            (DI.DynamicInfo
                                                                                                                                                               []
                                                                                                                                                               []))
                                                                                                                                                         (Prelude.return
                                                                                                                                                            (Curry.DebugModule.Prelude.Char
                                                                                                                                                               'D'))
                                                                                                                                                x67 <- do x64 <- DM.litHook
                                                                                                                                                                   (DI.DebugInfo
                                                                                                                                                                      (DI.SrcID
                                                                                                                                                                         "Char"
                                                                                                                                                                         0)
                                                                                                                                                                      (DI.DynamicInfo
                                                                                                                                                                         []
                                                                                                                                                                         []))
                                                                                                                                                                   (Prelude.return
                                                                                                                                                                      (Curry.DebugModule.Prelude.Char
                                                                                                                                                                         'i'))
                                                                                                                                                          x65 <- do x62 <- DM.litHook
                                                                                                                                                                             (DI.DebugInfo
                                                                                                                                                                                (DI.SrcID
                                                                                                                                                                                   "Char"
                                                                                                                                                                                   0)
                                                                                                                                                                                (DI.DynamicInfo
                                                                                                                                                                                   []
                                                                                                                                                                                   []))
                                                                                                                                                                             (Prelude.return
                                                                                                                                                                                (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                   'g'))
                                                                                                                                                                    x63 <- do x60 <- DM.litHook
                                                                                                                                                                                       (DI.DebugInfo
                                                                                                                                                                                          (DI.SrcID
                                                                                                                                                                                             "Char"
                                                                                                                                                                                             0)
                                                                                                                                                                                          (DI.DynamicInfo
                                                                                                                                                                                             []
                                                                                                                                                                                             []))
                                                                                                                                                                                       (Prelude.return
                                                                                                                                                                                          (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                             'i'))
                                                                                                                                                                              x61 <- do x58 <- DM.litHook
                                                                                                                                                                                                 (DI.DebugInfo
                                                                                                                                                                                                    (DI.SrcID
                                                                                                                                                                                                       "Char"
                                                                                                                                                                                                       0)
                                                                                                                                                                                                    (DI.DynamicInfo
                                                                                                                                                                                                       []
                                                                                                                                                                                                       []))
                                                                                                                                                                                                 (Prelude.return
                                                                                                                                                                                                    (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                       't'))
                                                                                                                                                                                        x59 <- do x56 <- DM.litHook
                                                                                                                                                                                                           (DI.DebugInfo
                                                                                                                                                                                                              (DI.SrcID
                                                                                                                                                                                                                 "Char"
                                                                                                                                                                                                                 0)
                                                                                                                                                                                                              (DI.DynamicInfo
                                                                                                                                                                                                                 []
                                                                                                                                                                                                                 []))
                                                                                                                                                                                                           (Prelude.return
                                                                                                                                                                                                              (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                 ':'))
                                                                                                                                                                                                  x57 <- do x54 <- DM.litHook
                                                                                                                                                                                                                     (DI.DebugInfo
                                                                                                                                                                                                                        (DI.SrcID
                                                                                                                                                                                                                           "Char"
                                                                                                                                                                                                                           0)
                                                                                                                                                                                                                        (DI.DynamicInfo
                                                                                                                                                                                                                           []
                                                                                                                                                                                                                           []))
                                                                                                                                                                                                                     (Prelude.return
                                                                                                                                                                                                                        (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                           ' '))
                                                                                                                                                                                                            x55 <- do x52 <- DM.litHook
                                                                                                                                                                                                                               (DI.DebugInfo
                                                                                                                                                                                                                                  (DI.SrcID
                                                                                                                                                                                                                                     "Char"
                                                                                                                                                                                                                                     0)
                                                                                                                                                                                                                                  (DI.DynamicInfo
                                                                                                                                                                                                                                     []
                                                                                                                                                                                                                                     []))
                                                                                                                                                                                                                               (Prelude.return
                                                                                                                                                                                                                                  (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                     'a'))
                                                                                                                                                                                                                      x53 <- do x50 <- DM.litHook
                                                                                                                                                                                                                                         (DI.DebugInfo
                                                                                                                                                                                                                                            (DI.SrcID
                                                                                                                                                                                                                                               "Char"
                                                                                                                                                                                                                                               0)
                                                                                                                                                                                                                                            (DI.DynamicInfo
                                                                                                                                                                                                                                               []
                                                                                                                                                                                                                                               []))
                                                                                                                                                                                                                                         (Prelude.return
                                                                                                                                                                                                                                            (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                               'r'))
                                                                                                                                                                                                                                x51 <- do x48 <- DM.litHook
                                                                                                                                                                                                                                                   (DI.DebugInfo
                                                                                                                                                                                                                                                      (DI.SrcID
                                                                                                                                                                                                                                                         "Char"
                                                                                                                                                                                                                                                         0)
                                                                                                                                                                                                                                                      (DI.DynamicInfo
                                                                                                                                                                                                                                                         []
                                                                                                                                                                                                                                                         []))
                                                                                                                                                                                                                                                   (Prelude.return
                                                                                                                                                                                                                                                      (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                         'g'))
                                                                                                                                                                                                                                          x49 <- do x46 <- DM.litHook
                                                                                                                                                                                                                                                             (DI.DebugInfo
                                                                                                                                                                                                                                                                (DI.SrcID
                                                                                                                                                                                                                                                                   "Char"
                                                                                                                                                                                                                                                                   0)
                                                                                                                                                                                                                                                                (DI.DynamicInfo
                                                                                                                                                                                                                                                                   []
                                                                                                                                                                                                                                                                   []))
                                                                                                                                                                                                                                                             (Prelude.return
                                                                                                                                                                                                                                                                (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                   'u'))
                                                                                                                                                                                                                                                    x47 <- do x44 <- DM.litHook
                                                                                                                                                                                                                                                                       (DI.DebugInfo
                                                                                                                                                                                                                                                                          (DI.SrcID
                                                                                                                                                                                                                                                                             "Char"
                                                                                                                                                                                                                                                                             0)
                                                                                                                                                                                                                                                                          (DI.DynamicInfo
                                                                                                                                                                                                                                                                             []
                                                                                                                                                                                                                                                                             []))
                                                                                                                                                                                                                                                                       (Prelude.return
                                                                                                                                                                                                                                                                          (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                             'm'))
                                                                                                                                                                                                                                                              x45 <- do x42 <- DM.litHook
                                                                                                                                                                                                                                                                                 (DI.DebugInfo
                                                                                                                                                                                                                                                                                    (DI.SrcID
                                                                                                                                                                                                                                                                                       "Char"
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
                                                                                                                                                                                                                                                                                                 "Char"
                                                                                                                                                                                                                                                                                                 0)
                                                                                                                                                                                                                                                                                              (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                 []
                                                                                                                                                                                                                                                                                                 []))
                                                                                                                                                                                                                                                                                           (Prelude.return
                                                                                                                                                                                                                                                                                              (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                 'n'))
                                                                                                                                                                                                                                                                                  x41 <- do x38 <- DM.litHook
                                                                                                                                                                                                                                                                                                     (DI.DebugInfo
                                                                                                                                                                                                                                                                                                        (DI.SrcID
                                                                                                                                                                                                                                                                                                           "Char"
                                                                                                                                                                                                                                                                                                           0)
                                                                                                                                                                                                                                                                                                        (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                           []
                                                                                                                                                                                                                                                                                                           []))
                                                                                                                                                                                                                                                                                                     (Prelude.return
                                                                                                                                                                                                                                                                                                        (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                           't'))
                                                                                                                                                                                                                                                                                            x39 <- do x36 <- DM.litHook
                                                                                                                                                                                                                                                                                                               (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                  (DI.SrcID
                                                                                                                                                                                                                                                                                                                     "Char"
                                                                                                                                                                                                                                                                                                                     0)
                                                                                                                                                                                                                                                                                                                  (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                     []
                                                                                                                                                                                                                                                                                                                     []))
                                                                                                                                                                                                                                                                                                               (Prelude.return
                                                                                                                                                                                                                                                                                                                  (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                     ' '))
                                                                                                                                                                                                                                                                                                      x37 <- do x34 <- DM.litHook
                                                                                                                                                                                                                                                                                                                         (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                            (DI.SrcID
                                                                                                                                                                                                                                                                                                                               "Char"
                                                                                                                                                                                                                                                                                                                               0)
                                                                                                                                                                                                                                                                                                                            (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                               []
                                                                                                                                                                                                                                                                                                                               []))
                                                                                                                                                                                                                                                                                                                         (Prelude.return
                                                                                                                                                                                                                                                                                                                            (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                               'n'))
                                                                                                                                                                                                                                                                                                                x35 <- do x32 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                   (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                      (DI.SrcID
                                                                                                                                                                                                                                                                                                                                         "Char"
                                                                                                                                                                                                                                                                                                                                         0)
                                                                                                                                                                                                                                                                                                                                      (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                         []
                                                                                                                                                                                                                                                                                                                                         []))
                                                                                                                                                                                                                                                                                                                                   (Prelude.return
                                                                                                                                                                                                                                                                                                                                      (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                         'o'))
                                                                                                                                                                                                                                                                                                                          x33 <- do x30 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                             (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                   "Char"
                                                                                                                                                                                                                                                                                                                                                   0)
                                                                                                                                                                                                                                                                                                                                                (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                   []
                                                                                                                                                                                                                                                                                                                                                   []))
                                                                                                                                                                                                                                                                                                                                             (Prelude.return
                                                                                                                                                                                                                                                                                                                                                (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                   't'))
                                                                                                                                                                                                                                                                                                                                    x31 <- do x28 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                       (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                          (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                             "Char"
                                                                                                                                                                                                                                                                                                                                                             0)
                                                                                                                                                                                                                                                                                                                                                          (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                             []
                                                                                                                                                                                                                                                                                                                                                             []))
                                                                                                                                                                                                                                                                                                                                                       (Prelude.return
                                                                                                                                                                                                                                                                                                                                                          (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                             ' '))
                                                                                                                                                                                                                                                                                                                                              x29 <- do x26 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                 (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                    (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                       "Char"
                                                                                                                                                                                                                                                                                                                                                                       0)
                                                                                                                                                                                                                                                                                                                                                                    (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                       []
                                                                                                                                                                                                                                                                                                                                                                       []))
                                                                                                                                                                                                                                                                                                                                                                 (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                    (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                       'a'))
                                                                                                                                                                                                                                                                                                                                                        x27 <- do x24 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                           (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                              (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                 "Char"
                                                                                                                                                                                                                                                                                                                                                                                 0)
                                                                                                                                                                                                                                                                                                                                                                              (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                 []
                                                                                                                                                                                                                                                                                                                                                                                 []))
                                                                                                                                                                                                                                                                                                                                                                           (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                              (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                 ' '))
                                                                                                                                                                                                                                                                                                                                                                  x25 <- do x22 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                                     (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                        (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                           "Char"
                                                                                                                                                                                                                                                                                                                                                                                           0)
                                                                                                                                                                                                                                                                                                                                                                                        (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                           []
                                                                                                                                                                                                                                                                                                                                                                                           []))
                                                                                                                                                                                                                                                                                                                                                                                     (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                        (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                           'd'))
                                                                                                                                                                                                                                                                                                                                                                            x23 <- do x20 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                                               (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                  (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                     "Char"
                                                                                                                                                                                                                                                                                                                                                                                                     0)
                                                                                                                                                                                                                                                                                                                                                                                                  (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                     []
                                                                                                                                                                                                                                                                                                                                                                                                     []))
                                                                                                                                                                                                                                                                                                                                                                                               (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                  (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                                     'i'))
                                                                                                                                                                                                                                                                                                                                                                                      x21 <- do x18 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                                                         (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                            (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                               "Char"
                                                                                                                                                                                                                                                                                                                                                                                                               0)
                                                                                                                                                                                                                                                                                                                                                                                                            (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                               []
                                                                                                                                                                                                                                                                                                                                                                                                               []))
                                                                                                                                                                                                                                                                                                                                                                                                         (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                            (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                                               'g'))
                                                                                                                                                                                                                                                                                                                                                                                                x19 <- do x16 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                                                                   (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                                      (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                                         "Char"
                                                                                                                                                                                                                                                                                                                                                                                                                         0)
                                                                                                                                                                                                                                                                                                                                                                                                                      (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                                         []
                                                                                                                                                                                                                                                                                                                                                                                                                         []))
                                                                                                                                                                                                                                                                                                                                                                                                                   (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                                      (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                                                         'i'))
                                                                                                                                                                                                                                                                                                                                                                                                          x17 <- do x14 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                                                                             (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                                                   "Char"
                                                                                                                                                                                                                                                                                                                                                                                                                                   0)
                                                                                                                                                                                                                                                                                                                                                                                                                                (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                   []
                                                                                                                                                                                                                                                                                                                                                                                                                                   []))
                                                                                                                                                                                                                                                                                                                                                                                                                             (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                                                (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                                                                   't'))
                                                                                                                                                                                                                                                                                                                                                                                                                    x15 <- do x12 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                                                                                       (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                          (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                                                             "Char"
                                                                                                                                                                                                                                                                                                                                                                                                                                             0)
                                                                                                                                                                                                                                                                                                                                                                                                                                          (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                             []
                                                                                                                                                                                                                                                                                                                                                                                                                                             []))
                                                                                                                                                                                                                                                                                                                                                                                                                                       (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                                                          (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                                                                             ' '))
                                                                                                                                                                                                                                                                                                                                                                                                                              x13 <- do x10 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                                                                                                 (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                    (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                                                                       "Char"
                                                                                                                                                                                                                                                                                                                                                                                                                                                       0)
                                                                                                                                                                                                                                                                                                                                                                                                                                                    (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                       []
                                                                                                                                                                                                                                                                                                                                                                                                                                                       []))
                                                                                                                                                                                                                                                                                                                                                                                                                                                 (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                                                                    (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                                                                                       'v'))
                                                                                                                                                                                                                                                                                                                                                                                                                                        x11 <- do x8 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                                                                                                          (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                             (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Char"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                0)
                                                                                                                                                                                                                                                                                                                                                                                                                                                             (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                                []
                                                                                                                                                                                                                                                                                                                                                                                                                                                                []))
                                                                                                                                                                                                                                                                                                                                                                                                                                                          (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                                                                             (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                                                                                                'a'))
                                                                                                                                                                                                                                                                                                                                                                                                                                                  x9 <- do x6 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                                                                                                                   (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         "Char"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         0)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         []
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         []))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                   (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         'l'))
                                                                                                                                                                                                                                                                                                                                                                                                                                                           x7 <- do x4 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                                               (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Char"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  0)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                               (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  []
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  []))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                                                                                               (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  'u'))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                    x5 <- do x2 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           "Char"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           0)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           []
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           []))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           'e'))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                             x3 <- DM.constructorHook
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           "Char"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           0)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           []
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           []))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        Curry.DebugModule.Prelude.Nil)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                             DM.constructorHook
                                                                                                                                                                                                                                                                                                                                                                                                                                                                               (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     "Char"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     0)
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     []
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     [DI.genTerm
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        x2,
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      DI.genTerm
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        x3]))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                               (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     x2
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     x3))
                                                                                                                                                                                                                                                                                                                                                                                                                                                                    DM.constructorHook
                                                                                                                                                                                                                                                                                                                                                                                                                                                                      (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                                                                                            "Char"
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
                                                                                                                                                                                                                                                                                                                                                                                                                                                                   "Char"
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
                                                                                                                                                                                                                                                                                                                                                                                                                                                          "Char"
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
                                                                                                                                                                                                                                                                                                                                                                                                                                                "Char"
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
                                                                                                                                                                                                                                                                                                                                                                                                                                      "Char"
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
                                                                                                                                                                                                                                                                                                                                                                                                                            "Char"
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
                                                                                                                                                                                                                                                                                                                                                                                                                  "Char"
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
                                                                                                                                                                                                                                                                                                                                                                                                        "Char"
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
                                                                                                                                                                                                                                                                                                                                                                                              "Char"
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
                                                                                                                                                                                                                                                                                                                                                                                    "Char"
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
                                                                                                                                                                                                                                                                                                                                                                          "Char"
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
                                                                                                                                                                                                                                                                                                                                                                "Char"
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
                                                                                                                                                                                                                                                                                                                                                      "Char"
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
                                                                                                                                                                                                                                                                                                                                            "Char"
                                                                                                                                                                                                                                                                                                                                            0)
                                                                                                                                                                                                                                                                                                                                         (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                            []
                                                                                                                                                                                                                                                                                                                                            [DI.genTerm
                                                                                                                                                                                                                                                                                                                                               x30,
                                                                                                                                                                                                                                                                                                                                             DI.genTerm
                                                                                                                                                                                                                                                                                                                                               x31]))
                                                                                                                                                                                                                                                                                                                                      (Prelude.return
                                                                                                                                                                                                                                                                                                                                         (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                                                                                                            x30
                                                                                                                                                                                                                                                                                                                                            x31))
                                                                                                                                                                                                                                                                                                                          DM.constructorHook
                                                                                                                                                                                                                                                                                                                            (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                               (DI.SrcID
                                                                                                                                                                                                                                                                                                                                  "Char"
                                                                                                                                                                                                                                                                                                                                  0)
                                                                                                                                                                                                                                                                                                                               (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                  []
                                                                                                                                                                                                                                                                                                                                  [DI.genTerm
                                                                                                                                                                                                                                                                                                                                     x32,
                                                                                                                                                                                                                                                                                                                                   DI.genTerm
                                                                                                                                                                                                                                                                                                                                     x33]))
                                                                                                                                                                                                                                                                                                                            (Prelude.return
                                                                                                                                                                                                                                                                                                                               (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                                                                                                  x32
                                                                                                                                                                                                                                                                                                                                  x33))
                                                                                                                                                                                                                                                                                                                DM.constructorHook
                                                                                                                                                                                                                                                                                                                  (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                     (DI.SrcID
                                                                                                                                                                                                                                                                                                                        "Char"
                                                                                                                                                                                                                                                                                                                        0)
                                                                                                                                                                                                                                                                                                                     (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                        []
                                                                                                                                                                                                                                                                                                                        [DI.genTerm
                                                                                                                                                                                                                                                                                                                           x34,
                                                                                                                                                                                                                                                                                                                         DI.genTerm
                                                                                                                                                                                                                                                                                                                           x35]))
                                                                                                                                                                                                                                                                                                                  (Prelude.return
                                                                                                                                                                                                                                                                                                                     (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                                                                                        x34
                                                                                                                                                                                                                                                                                                                        x35))
                                                                                                                                                                                                                                                                                                      DM.constructorHook
                                                                                                                                                                                                                                                                                                        (DI.DebugInfo
                                                                                                                                                                                                                                                                                                           (DI.SrcID
                                                                                                                                                                                                                                                                                                              "Char"
                                                                                                                                                                                                                                                                                                              0)
                                                                                                                                                                                                                                                                                                           (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                              []
                                                                                                                                                                                                                                                                                                              [DI.genTerm
                                                                                                                                                                                                                                                                                                                 x36,
                                                                                                                                                                                                                                                                                                               DI.genTerm
                                                                                                                                                                                                                                                                                                                 x37]))
                                                                                                                                                                                                                                                                                                        (Prelude.return
                                                                                                                                                                                                                                                                                                           (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                                                                              x36
                                                                                                                                                                                                                                                                                                              x37))
                                                                                                                                                                                                                                                                                            DM.constructorHook
                                                                                                                                                                                                                                                                                              (DI.DebugInfo
                                                                                                                                                                                                                                                                                                 (DI.SrcID
                                                                                                                                                                                                                                                                                                    "Char"
                                                                                                                                                                                                                                                                                                    0)
                                                                                                                                                                                                                                                                                                 (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                    []
                                                                                                                                                                                                                                                                                                    [DI.genTerm
                                                                                                                                                                                                                                                                                                       x38,
                                                                                                                                                                                                                                                                                                     DI.genTerm
                                                                                                                                                                                                                                                                                                       x39]))
                                                                                                                                                                                                                                                                                              (Prelude.return
                                                                                                                                                                                                                                                                                                 (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                                                                    x38
                                                                                                                                                                                                                                                                                                    x39))
                                                                                                                                                                                                                                                                                  DM.constructorHook
                                                                                                                                                                                                                                                                                    (DI.DebugInfo
                                                                                                                                                                                                                                                                                       (DI.SrcID
                                                                                                                                                                                                                                                                                          "Char"
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
                                                                                                                                                                                                                                                                                "Char"
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
                                                                                                                                                                                                                                                                      "Char"
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
                                                                                                                                                                                                                                                            "Char"
                                                                                                                                                                                                                                                            0)
                                                                                                                                                                                                                                                         (DI.DynamicInfo
                                                                                                                                                                                                                                                            []
                                                                                                                                                                                                                                                            [DI.genTerm
                                                                                                                                                                                                                                                               x46,
                                                                                                                                                                                                                                                             DI.genTerm
                                                                                                                                                                                                                                                               x47]))
                                                                                                                                                                                                                                                      (Prelude.return
                                                                                                                                                                                                                                                         (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                            x46
                                                                                                                                                                                                                                                            x47))
                                                                                                                                                                                                                                          DM.constructorHook
                                                                                                                                                                                                                                            (DI.DebugInfo
                                                                                                                                                                                                                                               (DI.SrcID
                                                                                                                                                                                                                                                  "Char"
                                                                                                                                                                                                                                                  0)
                                                                                                                                                                                                                                               (DI.DynamicInfo
                                                                                                                                                                                                                                                  []
                                                                                                                                                                                                                                                  [DI.genTerm
                                                                                                                                                                                                                                                     x48,
                                                                                                                                                                                                                                                   DI.genTerm
                                                                                                                                                                                                                                                     x49]))
                                                                                                                                                                                                                                            (Prelude.return
                                                                                                                                                                                                                                               (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                  x48
                                                                                                                                                                                                                                                  x49))
                                                                                                                                                                                                                                DM.constructorHook
                                                                                                                                                                                                                                  (DI.DebugInfo
                                                                                                                                                                                                                                     (DI.SrcID
                                                                                                                                                                                                                                        "Char"
                                                                                                                                                                                                                                        0)
                                                                                                                                                                                                                                     (DI.DynamicInfo
                                                                                                                                                                                                                                        []
                                                                                                                                                                                                                                        [DI.genTerm
                                                                                                                                                                                                                                           x50,
                                                                                                                                                                                                                                         DI.genTerm
                                                                                                                                                                                                                                           x51]))
                                                                                                                                                                                                                                  (Prelude.return
                                                                                                                                                                                                                                     (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                        x50
                                                                                                                                                                                                                                        x51))
                                                                                                                                                                                                                      DM.constructorHook
                                                                                                                                                                                                                        (DI.DebugInfo
                                                                                                                                                                                                                           (DI.SrcID
                                                                                                                                                                                                                              "Char"
                                                                                                                                                                                                                              0)
                                                                                                                                                                                                                           (DI.DynamicInfo
                                                                                                                                                                                                                              []
                                                                                                                                                                                                                              [DI.genTerm
                                                                                                                                                                                                                                 x52,
                                                                                                                                                                                                                               DI.genTerm
                                                                                                                                                                                                                                 x53]))
                                                                                                                                                                                                                        (Prelude.return
                                                                                                                                                                                                                           (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                              x52
                                                                                                                                                                                                                              x53))
                                                                                                                                                                                                            DM.constructorHook
                                                                                                                                                                                                              (DI.DebugInfo
                                                                                                                                                                                                                 (DI.SrcID
                                                                                                                                                                                                                    "Char"
                                                                                                                                                                                                                    0)
                                                                                                                                                                                                                 (DI.DynamicInfo
                                                                                                                                                                                                                    []
                                                                                                                                                                                                                    [DI.genTerm
                                                                                                                                                                                                                       x54,
                                                                                                                                                                                                                     DI.genTerm
                                                                                                                                                                                                                       x55]))
                                                                                                                                                                                                              (Prelude.return
                                                                                                                                                                                                                 (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                    x54
                                                                                                                                                                                                                    x55))
                                                                                                                                                                                                  DM.constructorHook
                                                                                                                                                                                                    (DI.DebugInfo
                                                                                                                                                                                                       (DI.SrcID
                                                                                                                                                                                                          "Char"
                                                                                                                                                                                                          0)
                                                                                                                                                                                                       (DI.DynamicInfo
                                                                                                                                                                                                          []
                                                                                                                                                                                                          [DI.genTerm
                                                                                                                                                                                                             x56,
                                                                                                                                                                                                           DI.genTerm
                                                                                                                                                                                                             x57]))
                                                                                                                                                                                                    (Prelude.return
                                                                                                                                                                                                       (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                          x56
                                                                                                                                                                                                          x57))
                                                                                                                                                                                        DM.constructorHook
                                                                                                                                                                                          (DI.DebugInfo
                                                                                                                                                                                             (DI.SrcID
                                                                                                                                                                                                "Char"
                                                                                                                                                                                                0)
                                                                                                                                                                                             (DI.DynamicInfo
                                                                                                                                                                                                []
                                                                                                                                                                                                [DI.genTerm
                                                                                                                                                                                                   x58,
                                                                                                                                                                                                 DI.genTerm
                                                                                                                                                                                                   x59]))
                                                                                                                                                                                          (Prelude.return
                                                                                                                                                                                             (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                x58
                                                                                                                                                                                                x59))
                                                                                                                                                                              DM.constructorHook
                                                                                                                                                                                (DI.DebugInfo
                                                                                                                                                                                   (DI.SrcID
                                                                                                                                                                                      "Char"
                                                                                                                                                                                      0)
                                                                                                                                                                                   (DI.DynamicInfo
                                                                                                                                                                                      []
                                                                                                                                                                                      [DI.genTerm
                                                                                                                                                                                         x60,
                                                                                                                                                                                       DI.genTerm
                                                                                                                                                                                         x61]))
                                                                                                                                                                                (Prelude.return
                                                                                                                                                                                   (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                      x60
                                                                                                                                                                                      x61))
                                                                                                                                                                    DM.constructorHook
                                                                                                                                                                      (DI.DebugInfo
                                                                                                                                                                         (DI.SrcID
                                                                                                                                                                            "Char"
                                                                                                                                                                            0)
                                                                                                                                                                         (DI.DynamicInfo
                                                                                                                                                                            []
                                                                                                                                                                            [DI.genTerm
                                                                                                                                                                               x62,
                                                                                                                                                                             DI.genTerm
                                                                                                                                                                               x63]))
                                                                                                                                                                      (Prelude.return
                                                                                                                                                                         (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                            x62
                                                                                                                                                                            x63))
                                                                                                                                                          DM.constructorHook
                                                                                                                                                            (DI.DebugInfo
                                                                                                                                                               (DI.SrcID
                                                                                                                                                                  "Char"
                                                                                                                                                                  0)
                                                                                                                                                               (DI.DynamicInfo
                                                                                                                                                                  []
                                                                                                                                                                  [DI.genTerm
                                                                                                                                                                     x64,
                                                                                                                                                                   DI.genTerm
                                                                                                                                                                     x65]))
                                                                                                                                                            (Prelude.return
                                                                                                                                                               (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                  x64
                                                                                                                                                                  x65))
                                                                                                                                                DM.constructorHook
                                                                                                                                                  (DI.DebugInfo
                                                                                                                                                     (DI.SrcID
                                                                                                                                                        "Char"
                                                                                                                                                        0)
                                                                                                                                                     (DI.DynamicInfo
                                                                                                                                                        []
                                                                                                                                                        [DI.genTerm
                                                                                                                                                           x66,
                                                                                                                                                         DI.genTerm
                                                                                                                                                           x67]))
                                                                                                                                                  (Prelude.return
                                                                                                                                                     (Curry.DebugModule.Prelude.Cons
                                                                                                                                                        x66
                                                                                                                                                        x67))
                                                                                                                                      DM.constructorHook
                                                                                                                                        (DI.DebugInfo
                                                                                                                                           (DI.SrcID
                                                                                                                                              "Char"
                                                                                                                                              0)
                                                                                                                                           (DI.DynamicInfo
                                                                                                                                              []
                                                                                                                                              [DI.genTerm
                                                                                                                                                 x68,
                                                                                                                                               DI.genTerm
                                                                                                                                                 x69]))
                                                                                                                                        (Prelude.return
                                                                                                                                           (Curry.DebugModule.Prelude.Cons
                                                                                                                                              x68
                                                                                                                                              x69))
                                                                                                                            DM.constructorHook
                                                                                                                              (DI.DebugInfo
                                                                                                                                 (DI.SrcID
                                                                                                                                    "Char"
                                                                                                                                    0)
                                                                                                                                 (DI.DynamicInfo
                                                                                                                                    []
                                                                                                                                    [DI.genTerm
                                                                                                                                       x70,
                                                                                                                                     DI.genTerm
                                                                                                                                       x71]))
                                                                                                                              (Prelude.return
                                                                                                                                 (Curry.DebugModule.Prelude.Cons
                                                                                                                                    x70
                                                                                                                                    x71))
                                                                                                                  DM.constructorHook
                                                                                                                    (DI.DebugInfo
                                                                                                                       (DI.SrcID
                                                                                                                          "Char"
                                                                                                                          0)
                                                                                                                       (DI.DynamicInfo
                                                                                                                          []
                                                                                                                          [DI.genTerm
                                                                                                                             x72,
                                                                                                                           DI.genTerm
                                                                                                                             x73]))
                                                                                                                    (Prelude.return
                                                                                                                       (Curry.DebugModule.Prelude.Cons
                                                                                                                          x72
                                                                                                                          x73))
                                                                                                        DM.constructorHook
                                                                                                          (DI.DebugInfo
                                                                                                             (DI.SrcID
                                                                                                                "Char"
                                                                                                                0)
                                                                                                             (DI.DynamicInfo
                                                                                                                []
                                                                                                                [DI.genTerm
                                                                                                                   x74,
                                                                                                                 DI.genTerm
                                                                                                                   x75]))
                                                                                                          (Prelude.return
                                                                                                             (Curry.DebugModule.Prelude.Cons
                                                                                                                x74
                                                                                                                x75))
                                                                                              DM.constructorHook
                                                                                                (DI.DebugInfo
                                                                                                   (DI.SrcID
                                                                                                      "Char"
                                                                                                      0)
                                                                                                   (DI.DynamicInfo
                                                                                                      []
                                                                                                      [DI.genTerm
                                                                                                         x76,
                                                                                                       DI.genTerm
                                                                                                         x77]))
                                                                                                (Prelude.return
                                                                                                   (Curry.DebugModule.Prelude.Cons
                                                                                                      x76
                                                                                                      x77))
                                                                                    DM.constructorHook
                                                                                      (DI.DebugInfo
                                                                                         (DI.SrcID
                                                                                            "Char"
                                                                                            0)
                                                                                         (DI.DynamicInfo
                                                                                            []
                                                                                            [DI.genTerm
                                                                                               x78,
                                                                                             DI.genTerm
                                                                                               x79]))
                                                                                      (Prelude.return
                                                                                         (Curry.DebugModule.Prelude.Cons
                                                                                            x78
                                                                                            x79))
                                                                          DM.constructorHook
                                                                            (DI.DebugInfo
                                                                               (DI.SrcID "Char" 0)
                                                                               (DI.DynamicInfo []
                                                                                  [DI.genTerm x80,
                                                                                   DI.genTerm x81]))
                                                                            (Prelude.return
                                                                               (Curry.DebugModule.Prelude.Cons
                                                                                  x80
                                                                                  x81))
                                                                DM.constructorHook
                                                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x82,
                                                                         DI.genTerm x83]))
                                                                  (Prelude.return
                                                                     (Curry.DebugModule.Prelude.Cons
                                                                        x82
                                                                        x83))
                                                      DM.constructorHook
                                                        (DI.DebugInfo (DI.SrcID "Char" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x84, DI.genTerm x85]))
                                                        (Prelude.return
                                                           (Curry.DebugModule.Prelude.Cons x84 x85))
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x86, DI.genTerm x87]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Cons x86 x87))
                                  DM.funcCallHook "error"
                                    (DI.DebugInfo (DI.SrcID "Char" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x88]))
                                    (Curry.DebugModule.Prelude.strict_error x88)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                 Curry.DebugModule.Prelude.strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x89])))
                           strict__case_0
                           x89)))
term_strict__case_0 x1 = DI.Term "_case_0" (DI.SrcID "Char" 0) x1
strict__case_6 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_6"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x19 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x19]))
               (case x19 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- do x3 <- Prelude.return x1
                                           DM.funcCallHook "ord"
                                             (DI.DebugInfo (DI.SrcID "Char" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x3]))
                                             (Curry.DebugModule.Prelude.strict_ord x3)
                                  x6 <- do x4 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Char" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return
                                                      (Curry.DebugModule.Prelude.Char '0'))
                                           DM.funcCallHook "ord"
                                             (DI.DebugInfo (DI.SrcID "Char" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4]))
                                             (Curry.DebugModule.Prelude.strict_ord x4)
                                  DM.funcCallHook "-"
                                    (DI.DebugInfo (DI.SrcID "Char" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                    (Curry.DebugModule.Prelude.op_Minus x5 x6)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x17 <- Prelude.return x1
                                  x18 <- do x15 <- do x9 <- do x7 <- Prelude.return x1
                                                               DM.funcCallHook "ord"
                                                                 (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                    (DI.DynamicInfo []
                                                                       [DI.genTerm x7]))
                                                                 (Curry.DebugModule.Prelude.strict_ord
                                                                    x7)
                                                      x10 <- do x8 <- DM.litHook
                                                                        (DI.DebugInfo
                                                                           (DI.SrcID "Char" 0)
                                                                           (DI.DynamicInfo [] []))
                                                                        (Prelude.return
                                                                           (Curry.DebugModule.Prelude.Char
                                                                              'A'))
                                                                DM.funcCallHook "ord"
                                                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x8]))
                                                                  (Curry.DebugModule.Prelude.strict_ord
                                                                     x8)
                                                      DM.funcCallHook ">="
                                                        (DI.DebugInfo (DI.SrcID "Char" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x9, DI.genTerm x10]))
                                                        (Curry.DebugModule.Prelude.op_GtEq x9 x10)
                                            x16 <- do x13 <- do x11 <- Prelude.return x1
                                                                DM.funcCallHook "ord"
                                                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x11]))
                                                                  (Curry.DebugModule.Prelude.strict_ord
                                                                     x11)
                                                      x14 <- do x12 <- DM.litHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Char" 0)
                                                                            (DI.DynamicInfo [] []))
                                                                         (Prelude.return
                                                                            (Curry.DebugModule.Prelude.Char
                                                                               'F'))
                                                                DM.funcCallHook "ord"
                                                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x12]))
                                                                  (Curry.DebugModule.Prelude.strict_ord
                                                                     x12)
                                                      DM.funcCallHook "<="
                                                        (DI.DebugInfo (DI.SrcID "Char" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x13, DI.genTerm x14]))
                                                        (Curry.DebugModule.Prelude.op_LtEq x13 x14)
                                            DM.funcCallHook "&&"
                                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x15, DI.genTerm x16]))
                                              (Curry.DebugModule.Prelude.op_AndAnd x15 x16)
                                  DM.funcCallHook "_case_5"
                                    (DI.DebugInfo (DI.SrcID "Char" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x17, DI.genTerm x18]))
                                    (strict__case_5 x17 x18)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x19])))
                           (strict__case_6 x1)
                           x19)))
term_strict__case_6 x1 = DI.Term "_case_6" (DI.SrcID "Char" 0) x1
strict__case_5 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_5"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x21 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x21]))
               (case x21 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- do x5 <- do x3 <- Prelude.return x1
                                                    DM.funcCallHook "ord"
                                                      (DI.DebugInfo (DI.SrcID "Char" 0)
                                                         (DI.DynamicInfo [] [DI.genTerm x3]))
                                                      (Curry.DebugModule.Prelude.strict_ord x3)
                                           x6 <- do x4 <- DM.litHook
                                                            (DI.DebugInfo (DI.SrcID "Char" 0)
                                                               (DI.DynamicInfo [] []))
                                                            (Prelude.return
                                                               (Curry.DebugModule.Prelude.Char 'A'))
                                                    DM.funcCallHook "ord"
                                                      (DI.DebugInfo (DI.SrcID "Char" 0)
                                                         (DI.DynamicInfo [] [DI.genTerm x4]))
                                                      (Curry.DebugModule.Prelude.strict_ord x4)
                                           DM.funcCallHook "-"
                                             (DI.DebugInfo (DI.SrcID "Char" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                             (Curry.DebugModule.Prelude.op_Minus x5 x6)
                                  x8 <- DM.litHook
                                          (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                          (Prelude.return
                                             (Curry.DebugModule.Prelude.Pos
                                                (Curry.DebugModule.Prelude.O
                                                   (Curry.DebugModule.Prelude.I
                                                      (Curry.DebugModule.Prelude.O
                                                         Curry.DebugModule.Prelude.IHi)))))
                                  DM.funcCallHook "+"
                                    (DI.DebugInfo (DI.SrcID "Char" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                    (Curry.DebugModule.Prelude.op_Plus x7 x8)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x19 <- Prelude.return x1
                                  x20 <- do x17 <- do x11 <- do x9 <- Prelude.return x1
                                                                DM.funcCallHook "ord"
                                                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x9]))
                                                                  (Curry.DebugModule.Prelude.strict_ord
                                                                     x9)
                                                      x12 <- do x10 <- DM.litHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Char" 0)
                                                                            (DI.DynamicInfo [] []))
                                                                         (Prelude.return
                                                                            (Curry.DebugModule.Prelude.Char
                                                                               'a'))
                                                                DM.funcCallHook "ord"
                                                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x10]))
                                                                  (Curry.DebugModule.Prelude.strict_ord
                                                                     x10)
                                                      DM.funcCallHook ">="
                                                        (DI.DebugInfo (DI.SrcID "Char" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x11, DI.genTerm x12]))
                                                        (Curry.DebugModule.Prelude.op_GtEq x11 x12)
                                            x18 <- do x15 <- do x13 <- Prelude.return x1
                                                                DM.funcCallHook "ord"
                                                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x13]))
                                                                  (Curry.DebugModule.Prelude.strict_ord
                                                                     x13)
                                                      x16 <- do x14 <- DM.litHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Char" 0)
                                                                            (DI.DynamicInfo [] []))
                                                                         (Prelude.return
                                                                            (Curry.DebugModule.Prelude.Char
                                                                               'f'))
                                                                DM.funcCallHook "ord"
                                                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x14]))
                                                                  (Curry.DebugModule.Prelude.strict_ord
                                                                     x14)
                                                      DM.funcCallHook "<="
                                                        (DI.DebugInfo (DI.SrcID "Char" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x15, DI.genTerm x16]))
                                                        (Curry.DebugModule.Prelude.op_LtEq x15 x16)
                                            DM.funcCallHook "&&"
                                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x17, DI.genTerm x18]))
                                              (Curry.DebugModule.Prelude.op_AndAnd x17 x18)
                                  DM.funcCallHook "_case_4"
                                    (DI.DebugInfo (DI.SrcID "Char" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x19, DI.genTerm x20]))
                                    (strict__case_4 x19 x20)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x21])))
                           (strict__case_5 x1)
                           x21)))
term_strict__case_5 x1 = DI.Term "_case_5" (DI.SrcID "Char" 0) x1
strict__case_4 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_4"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x10 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x10]))
               (case x10 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- do x5 <- do x3 <- Prelude.return x1
                                                    DM.funcCallHook "ord"
                                                      (DI.DebugInfo (DI.SrcID "Char" 0)
                                                         (DI.DynamicInfo [] [DI.genTerm x3]))
                                                      (Curry.DebugModule.Prelude.strict_ord x3)
                                           x6 <- do x4 <- DM.litHook
                                                            (DI.DebugInfo (DI.SrcID "Char" 0)
                                                               (DI.DynamicInfo [] []))
                                                            (Prelude.return
                                                               (Curry.DebugModule.Prelude.Char 'a'))
                                                    DM.funcCallHook "ord"
                                                      (DI.DebugInfo (DI.SrcID "Char" 0)
                                                         (DI.DynamicInfo [] [DI.genTerm x4]))
                                                      (Curry.DebugModule.Prelude.strict_ord x4)
                                           DM.funcCallHook "-"
                                             (DI.DebugInfo (DI.SrcID "Char" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                             (Curry.DebugModule.Prelude.op_Minus x5 x6)
                                  x8 <- DM.litHook
                                          (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                          (Prelude.return
                                             (Curry.DebugModule.Prelude.Pos
                                                (Curry.DebugModule.Prelude.O
                                                   (Curry.DebugModule.Prelude.I
                                                      (Curry.DebugModule.Prelude.O
                                                         Curry.DebugModule.Prelude.IHi)))))
                                  DM.funcCallHook "+"
                                    (DI.DebugInfo (DI.SrcID "Char" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                    (Curry.DebugModule.Prelude.op_Plus x7 x8)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- DM.funcCallHook "otherwise"
                                          (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                          Curry.DebugModule.Prelude.strict_otherwise
                                  DM.funcCallHook "_case_3"
                                    (DI.DebugInfo (DI.SrcID "Char" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9]))
                                    (strict__case_3 x9)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x10])))
                           (strict__case_4 x1)
                           x10)))
term_strict__case_4 x1 = DI.Term "_case_4" (DI.SrcID "Char" 0) x1
strict__case_3 x1
  = DM.eval
      (DM.funcDeclHook "_case_3"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x83 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x83]))
               (case x83 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x82 <- do x80 <- DM.litHook
                                                     (DI.DebugInfo (DI.SrcID "Char" 0)
                                                        (DI.DynamicInfo [] []))
                                                     (Prelude.return
                                                        (Curry.DebugModule.Prelude.Char 'C'))
                                            x81 <- do x78 <- DM.litHook
                                                               (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                  (DI.DynamicInfo [] []))
                                                               (Prelude.return
                                                                  (Curry.DebugModule.Prelude.Char
                                                                     'h'))
                                                      x79 <- do x76 <- DM.litHook
                                                                         (DI.DebugInfo
                                                                            (DI.SrcID "Char" 0)
                                                                            (DI.DynamicInfo [] []))
                                                                         (Prelude.return
                                                                            (Curry.DebugModule.Prelude.Char
                                                                               'a'))
                                                                x77 <- do x74 <- DM.litHook
                                                                                   (DI.DebugInfo
                                                                                      (DI.SrcID
                                                                                         "Char"
                                                                                         0)
                                                                                      (DI.DynamicInfo
                                                                                         []
                                                                                         []))
                                                                                   (Prelude.return
                                                                                      (Curry.DebugModule.Prelude.Char
                                                                                         'r'))
                                                                          x75 <- do x72 <- DM.litHook
                                                                                             (DI.DebugInfo
                                                                                                (DI.SrcID
                                                                                                   "Char"
                                                                                                   0)
                                                                                                (DI.DynamicInfo
                                                                                                   []
                                                                                                   []))
                                                                                             (Prelude.return
                                                                                                (Curry.DebugModule.Prelude.Char
                                                                                                   '.'))
                                                                                    x73 <- do x70 <- DM.litHook
                                                                                                       (DI.DebugInfo
                                                                                                          (DI.SrcID
                                                                                                             "Char"
                                                                                                             0)
                                                                                                          (DI.DynamicInfo
                                                                                                             []
                                                                                                             []))
                                                                                                       (Prelude.return
                                                                                                          (Curry.DebugModule.Prelude.Char
                                                                                                             'd'))
                                                                                              x71 <- do x68 <- DM.litHook
                                                                                                                 (DI.DebugInfo
                                                                                                                    (DI.SrcID
                                                                                                                       "Char"
                                                                                                                       0)
                                                                                                                    (DI.DynamicInfo
                                                                                                                       []
                                                                                                                       []))
                                                                                                                 (Prelude.return
                                                                                                                    (Curry.DebugModule.Prelude.Char
                                                                                                                       'i'))
                                                                                                        x69 <- do x66 <- DM.litHook
                                                                                                                           (DI.DebugInfo
                                                                                                                              (DI.SrcID
                                                                                                                                 "Char"
                                                                                                                                 0)
                                                                                                                              (DI.DynamicInfo
                                                                                                                                 []
                                                                                                                                 []))
                                                                                                                           (Prelude.return
                                                                                                                              (Curry.DebugModule.Prelude.Char
                                                                                                                                 'g'))
                                                                                                                  x67 <- do x64 <- DM.litHook
                                                                                                                                     (DI.DebugInfo
                                                                                                                                        (DI.SrcID
                                                                                                                                           "Char"
                                                                                                                                           0)
                                                                                                                                        (DI.DynamicInfo
                                                                                                                                           []
                                                                                                                                           []))
                                                                                                                                     (Prelude.return
                                                                                                                                        (Curry.DebugModule.Prelude.Char
                                                                                                                                           'i'))
                                                                                                                            x65 <- do x62 <- DM.litHook
                                                                                                                                               (DI.DebugInfo
                                                                                                                                                  (DI.SrcID
                                                                                                                                                     "Char"
                                                                                                                                                     0)
                                                                                                                                                  (DI.DynamicInfo
                                                                                                                                                     []
                                                                                                                                                     []))
                                                                                                                                               (Prelude.return
                                                                                                                                                  (Curry.DebugModule.Prelude.Char
                                                                                                                                                     't'))
                                                                                                                                      x63 <- do x60 <- DM.litHook
                                                                                                                                                         (DI.DebugInfo
                                                                                                                                                            (DI.SrcID
                                                                                                                                                               "Char"
                                                                                                                                                               0)
                                                                                                                                                            (DI.DynamicInfo
                                                                                                                                                               []
                                                                                                                                                               []))
                                                                                                                                                         (Prelude.return
                                                                                                                                                            (Curry.DebugModule.Prelude.Char
                                                                                                                                                               'T'))
                                                                                                                                                x61 <- do x58 <- DM.litHook
                                                                                                                                                                   (DI.DebugInfo
                                                                                                                                                                      (DI.SrcID
                                                                                                                                                                         "Char"
                                                                                                                                                                         0)
                                                                                                                                                                      (DI.DynamicInfo
                                                                                                                                                                         []
                                                                                                                                                                         []))
                                                                                                                                                                   (Prelude.return
                                                                                                                                                                      (Curry.DebugModule.Prelude.Char
                                                                                                                                                                         'o'))
                                                                                                                                                          x59 <- do x56 <- DM.litHook
                                                                                                                                                                             (DI.DebugInfo
                                                                                                                                                                                (DI.SrcID
                                                                                                                                                                                   "Char"
                                                                                                                                                                                   0)
                                                                                                                                                                                (DI.DynamicInfo
                                                                                                                                                                                   []
                                                                                                                                                                                   []))
                                                                                                                                                                             (Prelude.return
                                                                                                                                                                                (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                   'I'))
                                                                                                                                                                    x57 <- do x54 <- DM.litHook
                                                                                                                                                                                       (DI.DebugInfo
                                                                                                                                                                                          (DI.SrcID
                                                                                                                                                                                             "Char"
                                                                                                                                                                                             0)
                                                                                                                                                                                          (DI.DynamicInfo
                                                                                                                                                                                             []
                                                                                                                                                                                             []))
                                                                                                                                                                                       (Prelude.return
                                                                                                                                                                                          (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                             'n'))
                                                                                                                                                                              x55 <- do x52 <- DM.litHook
                                                                                                                                                                                                 (DI.DebugInfo
                                                                                                                                                                                                    (DI.SrcID
                                                                                                                                                                                                       "Char"
                                                                                                                                                                                                       0)
                                                                                                                                                                                                    (DI.DynamicInfo
                                                                                                                                                                                                       []
                                                                                                                                                                                                       []))
                                                                                                                                                                                                 (Prelude.return
                                                                                                                                                                                                    (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                       't'))
                                                                                                                                                                                        x53 <- do x50 <- DM.litHook
                                                                                                                                                                                                           (DI.DebugInfo
                                                                                                                                                                                                              (DI.SrcID
                                                                                                                                                                                                                 "Char"
                                                                                                                                                                                                                 0)
                                                                                                                                                                                                              (DI.DynamicInfo
                                                                                                                                                                                                                 []
                                                                                                                                                                                                                 []))
                                                                                                                                                                                                           (Prelude.return
                                                                                                                                                                                                              (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                 ':'))
                                                                                                                                                                                                  x51 <- do x48 <- DM.litHook
                                                                                                                                                                                                                     (DI.DebugInfo
                                                                                                                                                                                                                        (DI.SrcID
                                                                                                                                                                                                                           "Char"
                                                                                                                                                                                                                           0)
                                                                                                                                                                                                                        (DI.DynamicInfo
                                                                                                                                                                                                                           []
                                                                                                                                                                                                                           []))
                                                                                                                                                                                                                     (Prelude.return
                                                                                                                                                                                                                        (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                           ' '))
                                                                                                                                                                                                            x49 <- do x46 <- DM.litHook
                                                                                                                                                                                                                               (DI.DebugInfo
                                                                                                                                                                                                                                  (DI.SrcID
                                                                                                                                                                                                                                     "Char"
                                                                                                                                                                                                                                     0)
                                                                                                                                                                                                                                  (DI.DynamicInfo
                                                                                                                                                                                                                                     []
                                                                                                                                                                                                                                     []))
                                                                                                                                                                                                                               (Prelude.return
                                                                                                                                                                                                                                  (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                     'a'))
                                                                                                                                                                                                                      x47 <- do x44 <- DM.litHook
                                                                                                                                                                                                                                         (DI.DebugInfo
                                                                                                                                                                                                                                            (DI.SrcID
                                                                                                                                                                                                                                               "Char"
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
                                                                                                                                                                                                                                                         "Char"
                                                                                                                                                                                                                                                         0)
                                                                                                                                                                                                                                                      (DI.DynamicInfo
                                                                                                                                                                                                                                                         []
                                                                                                                                                                                                                                                         []))
                                                                                                                                                                                                                                                   (Prelude.return
                                                                                                                                                                                                                                                      (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                         'g'))
                                                                                                                                                                                                                                          x43 <- do x40 <- DM.litHook
                                                                                                                                                                                                                                                             (DI.DebugInfo
                                                                                                                                                                                                                                                                (DI.SrcID
                                                                                                                                                                                                                                                                   "Char"
                                                                                                                                                                                                                                                                   0)
                                                                                                                                                                                                                                                                (DI.DynamicInfo
                                                                                                                                                                                                                                                                   []
                                                                                                                                                                                                                                                                   []))
                                                                                                                                                                                                                                                             (Prelude.return
                                                                                                                                                                                                                                                                (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                   'u'))
                                                                                                                                                                                                                                                    x41 <- do x38 <- DM.litHook
                                                                                                                                                                                                                                                                       (DI.DebugInfo
                                                                                                                                                                                                                                                                          (DI.SrcID
                                                                                                                                                                                                                                                                             "Char"
                                                                                                                                                                                                                                                                             0)
                                                                                                                                                                                                                                                                          (DI.DynamicInfo
                                                                                                                                                                                                                                                                             []
                                                                                                                                                                                                                                                                             []))
                                                                                                                                                                                                                                                                       (Prelude.return
                                                                                                                                                                                                                                                                          (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                             'm'))
                                                                                                                                                                                                                                                              x39 <- do x36 <- DM.litHook
                                                                                                                                                                                                                                                                                 (DI.DebugInfo
                                                                                                                                                                                                                                                                                    (DI.SrcID
                                                                                                                                                                                                                                                                                       "Char"
                                                                                                                                                                                                                                                                                       0)
                                                                                                                                                                                                                                                                                    (DI.DynamicInfo
                                                                                                                                                                                                                                                                                       []
                                                                                                                                                                                                                                                                                       []))
                                                                                                                                                                                                                                                                                 (Prelude.return
                                                                                                                                                                                                                                                                                    (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                       'e'))
                                                                                                                                                                                                                                                                        x37 <- do x34 <- DM.litHook
                                                                                                                                                                                                                                                                                           (DI.DebugInfo
                                                                                                                                                                                                                                                                                              (DI.SrcID
                                                                                                                                                                                                                                                                                                 "Char"
                                                                                                                                                                                                                                                                                                 0)
                                                                                                                                                                                                                                                                                              (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                 []
                                                                                                                                                                                                                                                                                                 []))
                                                                                                                                                                                                                                                                                           (Prelude.return
                                                                                                                                                                                                                                                                                              (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                 'n'))
                                                                                                                                                                                                                                                                                  x35 <- do x32 <- DM.litHook
                                                                                                                                                                                                                                                                                                     (DI.DebugInfo
                                                                                                                                                                                                                                                                                                        (DI.SrcID
                                                                                                                                                                                                                                                                                                           "Char"
                                                                                                                                                                                                                                                                                                           0)
                                                                                                                                                                                                                                                                                                        (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                           []
                                                                                                                                                                                                                                                                                                           []))
                                                                                                                                                                                                                                                                                                     (Prelude.return
                                                                                                                                                                                                                                                                                                        (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                           't'))
                                                                                                                                                                                                                                                                                            x33 <- do x30 <- DM.litHook
                                                                                                                                                                                                                                                                                                               (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                  (DI.SrcID
                                                                                                                                                                                                                                                                                                                     "Char"
                                                                                                                                                                                                                                                                                                                     0)
                                                                                                                                                                                                                                                                                                                  (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                     []
                                                                                                                                                                                                                                                                                                                     []))
                                                                                                                                                                                                                                                                                                               (Prelude.return
                                                                                                                                                                                                                                                                                                                  (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                     ' '))
                                                                                                                                                                                                                                                                                                      x31 <- do x28 <- DM.litHook
                                                                                                                                                                                                                                                                                                                         (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                            (DI.SrcID
                                                                                                                                                                                                                                                                                                                               "Char"
                                                                                                                                                                                                                                                                                                                               0)
                                                                                                                                                                                                                                                                                                                            (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                               []
                                                                                                                                                                                                                                                                                                                               []))
                                                                                                                                                                                                                                                                                                                         (Prelude.return
                                                                                                                                                                                                                                                                                                                            (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                               'i'))
                                                                                                                                                                                                                                                                                                                x29 <- do x26 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                   (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                      (DI.SrcID
                                                                                                                                                                                                                                                                                                                                         "Char"
                                                                                                                                                                                                                                                                                                                                         0)
                                                                                                                                                                                                                                                                                                                                      (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                         []
                                                                                                                                                                                                                                                                                                                                         []))
                                                                                                                                                                                                                                                                                                                                   (Prelude.return
                                                                                                                                                                                                                                                                                                                                      (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                         's'))
                                                                                                                                                                                                                                                                                                                          x27 <- do x24 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                             (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                   "Char"
                                                                                                                                                                                                                                                                                                                                                   0)
                                                                                                                                                                                                                                                                                                                                                (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                   []
                                                                                                                                                                                                                                                                                                                                                   []))
                                                                                                                                                                                                                                                                                                                                             (Prelude.return
                                                                                                                                                                                                                                                                                                                                                (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                   ' '))
                                                                                                                                                                                                                                                                                                                                    x25 <- do x22 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                       (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                          (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                             "Char"
                                                                                                                                                                                                                                                                                                                                                             0)
                                                                                                                                                                                                                                                                                                                                                          (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                             []
                                                                                                                                                                                                                                                                                                                                                             []))
                                                                                                                                                                                                                                                                                                                                                       (Prelude.return
                                                                                                                                                                                                                                                                                                                                                          (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                             'n'))
                                                                                                                                                                                                                                                                                                                                              x23 <- do x20 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                 (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                    (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                       "Char"
                                                                                                                                                                                                                                                                                                                                                                       0)
                                                                                                                                                                                                                                                                                                                                                                    (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                       []
                                                                                                                                                                                                                                                                                                                                                                       []))
                                                                                                                                                                                                                                                                                                                                                                 (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                    (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                       'o'))
                                                                                                                                                                                                                                                                                                                                                        x21 <- do x18 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                           (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                              (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                 "Char"
                                                                                                                                                                                                                                                                                                                                                                                 0)
                                                                                                                                                                                                                                                                                                                                                                              (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                 []
                                                                                                                                                                                                                                                                                                                                                                                 []))
                                                                                                                                                                                                                                                                                                                                                                           (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                              (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                 't'))
                                                                                                                                                                                                                                                                                                                                                                  x19 <- do x16 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                                     (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                        (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                           "Char"
                                                                                                                                                                                                                                                                                                                                                                                           0)
                                                                                                                                                                                                                                                                                                                                                                                        (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                           []
                                                                                                                                                                                                                                                                                                                                                                                           []))
                                                                                                                                                                                                                                                                                                                                                                                     (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                        (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                           ' '))
                                                                                                                                                                                                                                                                                                                                                                            x17 <- do x14 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                                               (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                  (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                     "Char"
                                                                                                                                                                                                                                                                                                                                                                                                     0)
                                                                                                                                                                                                                                                                                                                                                                                                  (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                     []
                                                                                                                                                                                                                                                                                                                                                                                                     []))
                                                                                                                                                                                                                                                                                                                                                                                               (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                  (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                                     'a'))
                                                                                                                                                                                                                                                                                                                                                                                      x15 <- do x12 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                                                         (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                            (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                               "Char"
                                                                                                                                                                                                                                                                                                                                                                                                               0)
                                                                                                                                                                                                                                                                                                                                                                                                            (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                               []
                                                                                                                                                                                                                                                                                                                                                                                                               []))
                                                                                                                                                                                                                                                                                                                                                                                                         (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                            (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                                               ' '))
                                                                                                                                                                                                                                                                                                                                                                                                x13 <- do x10 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                                                                   (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                                      (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                                         "Char"
                                                                                                                                                                                                                                                                                                                                                                                                                         0)
                                                                                                                                                                                                                                                                                                                                                                                                                      (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                                         []
                                                                                                                                                                                                                                                                                                                                                                                                                         []))
                                                                                                                                                                                                                                                                                                                                                                                                                   (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                                      (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                                                         'd'))
                                                                                                                                                                                                                                                                                                                                                                                                          x11 <- do x8 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                                                                            (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                                               (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                                                  "Char"
                                                                                                                                                                                                                                                                                                                                                                                                                                  0)
                                                                                                                                                                                                                                                                                                                                                                                                                               (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                  []
                                                                                                                                                                                                                                                                                                                                                                                                                                  []))
                                                                                                                                                                                                                                                                                                                                                                                                                            (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                                               (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                                                                  'i'))
                                                                                                                                                                                                                                                                                                                                                                                                                    x9 <- do x6 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                                                                                     (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                        (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                                                           "Char"
                                                                                                                                                                                                                                                                                                                                                                                                                                           0)
                                                                                                                                                                                                                                                                                                                                                                                                                                        (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                           []
                                                                                                                                                                                                                                                                                                                                                                                                                                           []))
                                                                                                                                                                                                                                                                                                                                                                                                                                     (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                                                        (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                                                                           'g'))
                                                                                                                                                                                                                                                                                                                                                                                                                             x7 <- do x4 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                                                                                              (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                 (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                                                                    "Char"
                                                                                                                                                                                                                                                                                                                                                                                                                                                    0)
                                                                                                                                                                                                                                                                                                                                                                                                                                                 (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                    []
                                                                                                                                                                                                                                                                                                                                                                                                                                                    []))
                                                                                                                                                                                                                                                                                                                                                                                                                                              (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                                                                 (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                                                                                    'i'))
                                                                                                                                                                                                                                                                                                                                                                                                                                      x5 <- do x2 <- DM.litHook
                                                                                                                                                                                                                                                                                                                                                                                                                                                       (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                          (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Char"
                                                                                                                                                                                                                                                                                                                                                                                                                                                             0)
                                                                                                                                                                                                                                                                                                                                                                                                                                                          (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                             []
                                                                                                                                                                                                                                                                                                                                                                                                                                                             []))
                                                                                                                                                                                                                                                                                                                                                                                                                                                       (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                                                                          (Curry.DebugModule.Prelude.Char
                                                                                                                                                                                                                                                                                                                                                                                                                                                             't'))
                                                                                                                                                                                                                                                                                                                                                                                                                                               x3 <- DM.constructorHook
                                                                                                                                                                                                                                                                                                                                                                                                                                                       (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                          (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                                                                             "Char"
                                                                                                                                                                                                                                                                                                                                                                                                                                                             0)
                                                                                                                                                                                                                                                                                                                                                                                                                                                          (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                             []
                                                                                                                                                                                                                                                                                                                                                                                                                                                             []))
                                                                                                                                                                                                                                                                                                                                                                                                                                                       (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                                                                          Curry.DebugModule.Prelude.Nil)
                                                                                                                                                                                                                                                                                                                                                                                                                                               DM.constructorHook
                                                                                                                                                                                                                                                                                                                                                                                                                                                 (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                    (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                                                                       "Char"
                                                                                                                                                                                                                                                                                                                                                                                                                                                       0)
                                                                                                                                                                                                                                                                                                                                                                                                                                                    (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                                       []
                                                                                                                                                                                                                                                                                                                                                                                                                                                       [DI.genTerm
                                                                                                                                                                                                                                                                                                                                                                                                                                                          x2,
                                                                                                                                                                                                                                                                                                                                                                                                                                                        DI.genTerm
                                                                                                                                                                                                                                                                                                                                                                                                                                                          x3]))
                                                                                                                                                                                                                                                                                                                                                                                                                                                 (Prelude.return
                                                                                                                                                                                                                                                                                                                                                                                                                                                    (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                                                                                                                                                                                                                       x2
                                                                                                                                                                                                                                                                                                                                                                                                                                                       x3))
                                                                                                                                                                                                                                                                                                                                                                                                                                      DM.constructorHook
                                                                                                                                                                                                                                                                                                                                                                                                                                        (DI.DebugInfo
                                                                                                                                                                                                                                                                                                                                                                                                                                           (DI.SrcID
                                                                                                                                                                                                                                                                                                                                                                                                                                              "Char"
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
                                                                                                                                                                                                                                                                                                                                                                                                                                     "Char"
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
                                                                                                                                                                                                                                                                                                                                                                                                                            "Char"
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
                                                                                                                                                                                                                                                                                                                                                                                                                  "Char"
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
                                                                                                                                                                                                                                                                                                                                                                                                        "Char"
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
                                                                                                                                                                                                                                                                                                                                                                                              "Char"
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
                                                                                                                                                                                                                                                                                                                                                                                    "Char"
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
                                                                                                                                                                                                                                                                                                                                                                          "Char"
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
                                                                                                                                                                                                                                                                                                                                                                "Char"
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
                                                                                                                                                                                                                                                                                                                                                      "Char"
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
                                                                                                                                                                                                                                                                                                                                            "Char"
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
                                                                                                                                                                                                                                                                                                                                  "Char"
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
                                                                                                                                                                                                                                                                                                                        "Char"
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
                                                                                                                                                                                                                                                                                                              "Char"
                                                                                                                                                                                                                                                                                                              0)
                                                                                                                                                                                                                                                                                                           (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                              []
                                                                                                                                                                                                                                                                                                              [DI.genTerm
                                                                                                                                                                                                                                                                                                                 x30,
                                                                                                                                                                                                                                                                                                               DI.genTerm
                                                                                                                                                                                                                                                                                                                 x31]))
                                                                                                                                                                                                                                                                                                        (Prelude.return
                                                                                                                                                                                                                                                                                                           (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                                                                              x30
                                                                                                                                                                                                                                                                                                              x31))
                                                                                                                                                                                                                                                                                            DM.constructorHook
                                                                                                                                                                                                                                                                                              (DI.DebugInfo
                                                                                                                                                                                                                                                                                                 (DI.SrcID
                                                                                                                                                                                                                                                                                                    "Char"
                                                                                                                                                                                                                                                                                                    0)
                                                                                                                                                                                                                                                                                                 (DI.DynamicInfo
                                                                                                                                                                                                                                                                                                    []
                                                                                                                                                                                                                                                                                                    [DI.genTerm
                                                                                                                                                                                                                                                                                                       x32,
                                                                                                                                                                                                                                                                                                     DI.genTerm
                                                                                                                                                                                                                                                                                                       x33]))
                                                                                                                                                                                                                                                                                              (Prelude.return
                                                                                                                                                                                                                                                                                                 (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                                                                    x32
                                                                                                                                                                                                                                                                                                    x33))
                                                                                                                                                                                                                                                                                  DM.constructorHook
                                                                                                                                                                                                                                                                                    (DI.DebugInfo
                                                                                                                                                                                                                                                                                       (DI.SrcID
                                                                                                                                                                                                                                                                                          "Char"
                                                                                                                                                                                                                                                                                          0)
                                                                                                                                                                                                                                                                                       (DI.DynamicInfo
                                                                                                                                                                                                                                                                                          []
                                                                                                                                                                                                                                                                                          [DI.genTerm
                                                                                                                                                                                                                                                                                             x34,
                                                                                                                                                                                                                                                                                           DI.genTerm
                                                                                                                                                                                                                                                                                             x35]))
                                                                                                                                                                                                                                                                                    (Prelude.return
                                                                                                                                                                                                                                                                                       (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                                                          x34
                                                                                                                                                                                                                                                                                          x35))
                                                                                                                                                                                                                                                                        DM.constructorHook
                                                                                                                                                                                                                                                                          (DI.DebugInfo
                                                                                                                                                                                                                                                                             (DI.SrcID
                                                                                                                                                                                                                                                                                "Char"
                                                                                                                                                                                                                                                                                0)
                                                                                                                                                                                                                                                                             (DI.DynamicInfo
                                                                                                                                                                                                                                                                                []
                                                                                                                                                                                                                                                                                [DI.genTerm
                                                                                                                                                                                                                                                                                   x36,
                                                                                                                                                                                                                                                                                 DI.genTerm
                                                                                                                                                                                                                                                                                   x37]))
                                                                                                                                                                                                                                                                          (Prelude.return
                                                                                                                                                                                                                                                                             (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                                                x36
                                                                                                                                                                                                                                                                                x37))
                                                                                                                                                                                                                                                              DM.constructorHook
                                                                                                                                                                                                                                                                (DI.DebugInfo
                                                                                                                                                                                                                                                                   (DI.SrcID
                                                                                                                                                                                                                                                                      "Char"
                                                                                                                                                                                                                                                                      0)
                                                                                                                                                                                                                                                                   (DI.DynamicInfo
                                                                                                                                                                                                                                                                      []
                                                                                                                                                                                                                                                                      [DI.genTerm
                                                                                                                                                                                                                                                                         x38,
                                                                                                                                                                                                                                                                       DI.genTerm
                                                                                                                                                                                                                                                                         x39]))
                                                                                                                                                                                                                                                                (Prelude.return
                                                                                                                                                                                                                                                                   (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                                                                      x38
                                                                                                                                                                                                                                                                      x39))
                                                                                                                                                                                                                                                    DM.constructorHook
                                                                                                                                                                                                                                                      (DI.DebugInfo
                                                                                                                                                                                                                                                         (DI.SrcID
                                                                                                                                                                                                                                                            "Char"
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
                                                                                                                                                                                                                                                  "Char"
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
                                                                                                                                                                                                                                        "Char"
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
                                                                                                                                                                                                                              "Char"
                                                                                                                                                                                                                              0)
                                                                                                                                                                                                                           (DI.DynamicInfo
                                                                                                                                                                                                                              []
                                                                                                                                                                                                                              [DI.genTerm
                                                                                                                                                                                                                                 x46,
                                                                                                                                                                                                                               DI.genTerm
                                                                                                                                                                                                                                 x47]))
                                                                                                                                                                                                                        (Prelude.return
                                                                                                                                                                                                                           (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                              x46
                                                                                                                                                                                                                              x47))
                                                                                                                                                                                                            DM.constructorHook
                                                                                                                                                                                                              (DI.DebugInfo
                                                                                                                                                                                                                 (DI.SrcID
                                                                                                                                                                                                                    "Char"
                                                                                                                                                                                                                    0)
                                                                                                                                                                                                                 (DI.DynamicInfo
                                                                                                                                                                                                                    []
                                                                                                                                                                                                                    [DI.genTerm
                                                                                                                                                                                                                       x48,
                                                                                                                                                                                                                     DI.genTerm
                                                                                                                                                                                                                       x49]))
                                                                                                                                                                                                              (Prelude.return
                                                                                                                                                                                                                 (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                                    x48
                                                                                                                                                                                                                    x49))
                                                                                                                                                                                                  DM.constructorHook
                                                                                                                                                                                                    (DI.DebugInfo
                                                                                                                                                                                                       (DI.SrcID
                                                                                                                                                                                                          "Char"
                                                                                                                                                                                                          0)
                                                                                                                                                                                                       (DI.DynamicInfo
                                                                                                                                                                                                          []
                                                                                                                                                                                                          [DI.genTerm
                                                                                                                                                                                                             x50,
                                                                                                                                                                                                           DI.genTerm
                                                                                                                                                                                                             x51]))
                                                                                                                                                                                                    (Prelude.return
                                                                                                                                                                                                       (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                          x50
                                                                                                                                                                                                          x51))
                                                                                                                                                                                        DM.constructorHook
                                                                                                                                                                                          (DI.DebugInfo
                                                                                                                                                                                             (DI.SrcID
                                                                                                                                                                                                "Char"
                                                                                                                                                                                                0)
                                                                                                                                                                                             (DI.DynamicInfo
                                                                                                                                                                                                []
                                                                                                                                                                                                [DI.genTerm
                                                                                                                                                                                                   x52,
                                                                                                                                                                                                 DI.genTerm
                                                                                                                                                                                                   x53]))
                                                                                                                                                                                          (Prelude.return
                                                                                                                                                                                             (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                                x52
                                                                                                                                                                                                x53))
                                                                                                                                                                              DM.constructorHook
                                                                                                                                                                                (DI.DebugInfo
                                                                                                                                                                                   (DI.SrcID
                                                                                                                                                                                      "Char"
                                                                                                                                                                                      0)
                                                                                                                                                                                   (DI.DynamicInfo
                                                                                                                                                                                      []
                                                                                                                                                                                      [DI.genTerm
                                                                                                                                                                                         x54,
                                                                                                                                                                                       DI.genTerm
                                                                                                                                                                                         x55]))
                                                                                                                                                                                (Prelude.return
                                                                                                                                                                                   (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                                      x54
                                                                                                                                                                                      x55))
                                                                                                                                                                    DM.constructorHook
                                                                                                                                                                      (DI.DebugInfo
                                                                                                                                                                         (DI.SrcID
                                                                                                                                                                            "Char"
                                                                                                                                                                            0)
                                                                                                                                                                         (DI.DynamicInfo
                                                                                                                                                                            []
                                                                                                                                                                            [DI.genTerm
                                                                                                                                                                               x56,
                                                                                                                                                                             DI.genTerm
                                                                                                                                                                               x57]))
                                                                                                                                                                      (Prelude.return
                                                                                                                                                                         (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                            x56
                                                                                                                                                                            x57))
                                                                                                                                                          DM.constructorHook
                                                                                                                                                            (DI.DebugInfo
                                                                                                                                                               (DI.SrcID
                                                                                                                                                                  "Char"
                                                                                                                                                                  0)
                                                                                                                                                               (DI.DynamicInfo
                                                                                                                                                                  []
                                                                                                                                                                  [DI.genTerm
                                                                                                                                                                     x58,
                                                                                                                                                                   DI.genTerm
                                                                                                                                                                     x59]))
                                                                                                                                                            (Prelude.return
                                                                                                                                                               (Curry.DebugModule.Prelude.Cons
                                                                                                                                                                  x58
                                                                                                                                                                  x59))
                                                                                                                                                DM.constructorHook
                                                                                                                                                  (DI.DebugInfo
                                                                                                                                                     (DI.SrcID
                                                                                                                                                        "Char"
                                                                                                                                                        0)
                                                                                                                                                     (DI.DynamicInfo
                                                                                                                                                        []
                                                                                                                                                        [DI.genTerm
                                                                                                                                                           x60,
                                                                                                                                                         DI.genTerm
                                                                                                                                                           x61]))
                                                                                                                                                  (Prelude.return
                                                                                                                                                     (Curry.DebugModule.Prelude.Cons
                                                                                                                                                        x60
                                                                                                                                                        x61))
                                                                                                                                      DM.constructorHook
                                                                                                                                        (DI.DebugInfo
                                                                                                                                           (DI.SrcID
                                                                                                                                              "Char"
                                                                                                                                              0)
                                                                                                                                           (DI.DynamicInfo
                                                                                                                                              []
                                                                                                                                              [DI.genTerm
                                                                                                                                                 x62,
                                                                                                                                               DI.genTerm
                                                                                                                                                 x63]))
                                                                                                                                        (Prelude.return
                                                                                                                                           (Curry.DebugModule.Prelude.Cons
                                                                                                                                              x62
                                                                                                                                              x63))
                                                                                                                            DM.constructorHook
                                                                                                                              (DI.DebugInfo
                                                                                                                                 (DI.SrcID
                                                                                                                                    "Char"
                                                                                                                                    0)
                                                                                                                                 (DI.DynamicInfo
                                                                                                                                    []
                                                                                                                                    [DI.genTerm
                                                                                                                                       x64,
                                                                                                                                     DI.genTerm
                                                                                                                                       x65]))
                                                                                                                              (Prelude.return
                                                                                                                                 (Curry.DebugModule.Prelude.Cons
                                                                                                                                    x64
                                                                                                                                    x65))
                                                                                                                  DM.constructorHook
                                                                                                                    (DI.DebugInfo
                                                                                                                       (DI.SrcID
                                                                                                                          "Char"
                                                                                                                          0)
                                                                                                                       (DI.DynamicInfo
                                                                                                                          []
                                                                                                                          [DI.genTerm
                                                                                                                             x66,
                                                                                                                           DI.genTerm
                                                                                                                             x67]))
                                                                                                                    (Prelude.return
                                                                                                                       (Curry.DebugModule.Prelude.Cons
                                                                                                                          x66
                                                                                                                          x67))
                                                                                                        DM.constructorHook
                                                                                                          (DI.DebugInfo
                                                                                                             (DI.SrcID
                                                                                                                "Char"
                                                                                                                0)
                                                                                                             (DI.DynamicInfo
                                                                                                                []
                                                                                                                [DI.genTerm
                                                                                                                   x68,
                                                                                                                 DI.genTerm
                                                                                                                   x69]))
                                                                                                          (Prelude.return
                                                                                                             (Curry.DebugModule.Prelude.Cons
                                                                                                                x68
                                                                                                                x69))
                                                                                              DM.constructorHook
                                                                                                (DI.DebugInfo
                                                                                                   (DI.SrcID
                                                                                                      "Char"
                                                                                                      0)
                                                                                                   (DI.DynamicInfo
                                                                                                      []
                                                                                                      [DI.genTerm
                                                                                                         x70,
                                                                                                       DI.genTerm
                                                                                                         x71]))
                                                                                                (Prelude.return
                                                                                                   (Curry.DebugModule.Prelude.Cons
                                                                                                      x70
                                                                                                      x71))
                                                                                    DM.constructorHook
                                                                                      (DI.DebugInfo
                                                                                         (DI.SrcID
                                                                                            "Char"
                                                                                            0)
                                                                                         (DI.DynamicInfo
                                                                                            []
                                                                                            [DI.genTerm
                                                                                               x72,
                                                                                             DI.genTerm
                                                                                               x73]))
                                                                                      (Prelude.return
                                                                                         (Curry.DebugModule.Prelude.Cons
                                                                                            x72
                                                                                            x73))
                                                                          DM.constructorHook
                                                                            (DI.DebugInfo
                                                                               (DI.SrcID "Char" 0)
                                                                               (DI.DynamicInfo []
                                                                                  [DI.genTerm x74,
                                                                                   DI.genTerm x75]))
                                                                            (Prelude.return
                                                                               (Curry.DebugModule.Prelude.Cons
                                                                                  x74
                                                                                  x75))
                                                                DM.constructorHook
                                                                  (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x76,
                                                                         DI.genTerm x77]))
                                                                  (Prelude.return
                                                                     (Curry.DebugModule.Prelude.Cons
                                                                        x76
                                                                        x77))
                                                      DM.constructorHook
                                                        (DI.DebugInfo (DI.SrcID "Char" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x78, DI.genTerm x79]))
                                                        (Prelude.return
                                                           (Curry.DebugModule.Prelude.Cons x78 x79))
                                            DM.constructorHook
                                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x80, DI.genTerm x81]))
                                              (Prelude.return
                                                 (Curry.DebugModule.Prelude.Cons x80 x81))
                                  DM.funcCallHook "error"
                                    (DI.DebugInfo (DI.SrcID "Char" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x82]))
                                    (Curry.DebugModule.Prelude.strict_error x82)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                 Curry.DebugModule.Prelude.strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x83])))
                           strict__case_3
                           x83)))
term_strict__case_3 x1 = DI.Term "_case_3" (DI.SrcID "Char" 0) x1
strict__case_8 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_8"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x13 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- do x8 <- do x5 <- do x3 <- Prelude.return x1
                                                              DM.funcCallHook "ord"
                                                                (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                   (DI.DynamicInfo []
                                                                      [DI.genTerm x3]))
                                                                (Curry.DebugModule.Prelude.strict_ord
                                                                   x3)
                                                     x6 <- do x4 <- DM.litHook
                                                                      (DI.DebugInfo
                                                                         (DI.SrcID "Char" 0)
                                                                         (DI.DynamicInfo [] []))
                                                                      (Prelude.return
                                                                         (Curry.DebugModule.Prelude.Char
                                                                            'A'))
                                                              DM.funcCallHook "ord"
                                                                (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                   (DI.DynamicInfo []
                                                                      [DI.genTerm x4]))
                                                                (Curry.DebugModule.Prelude.strict_ord
                                                                   x4)
                                                     DM.funcCallHook "-"
                                                       (DI.DebugInfo (DI.SrcID "Char" 0)
                                                          (DI.DynamicInfo []
                                                             [DI.genTerm x5, DI.genTerm x6]))
                                                       (Curry.DebugModule.Prelude.op_Minus x5 x6)
                                            x9 <- do x7 <- DM.litHook
                                                             (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                (DI.DynamicInfo [] []))
                                                             (Prelude.return
                                                                (Curry.DebugModule.Prelude.Char
                                                                   'a'))
                                                     DM.funcCallHook "ord"
                                                       (DI.DebugInfo (DI.SrcID "Char" 0)
                                                          (DI.DynamicInfo [] [DI.genTerm x7]))
                                                       (Curry.DebugModule.Prelude.strict_ord x7)
                                            DM.funcCallHook "+"
                                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                              (Curry.DebugModule.Prelude.op_Plus x8 x9)
                                  DM.funcCallHook "chr"
                                    (DI.DebugInfo (DI.SrcID "Char" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10]))
                                    (Curry.DebugModule.Prelude.strict_chr x10)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- Prelude.return x1
                                  x12 <- DM.funcCallHook "otherwise"
                                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                           Curry.DebugModule.Prelude.strict_otherwise
                                  DM.funcCallHook "_case_7"
                                    (DI.DebugInfo (DI.SrcID "Char" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11, DI.genTerm x12]))
                                    (strict__case_7 x11 x12)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_8 x1)
                           x13)))
term_strict__case_8 x1 = DI.Term "_case_8" (DI.SrcID "Char" 0) x1
strict__case_7 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_7"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3]))
               (case x3 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x1))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                 Curry.DebugModule.Prelude.strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x3])))
                           (strict__case_7 x1)
                           x3)))
term_strict__case_7 x1 = DI.Term "_case_7" (DI.SrcID "Char" 0) x1
strict__case_10 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_10"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x13 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x13]))
               (case x13 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- do x8 <- do x5 <- do x3 <- Prelude.return x1
                                                              DM.funcCallHook "ord"
                                                                (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                   (DI.DynamicInfo []
                                                                      [DI.genTerm x3]))
                                                                (Curry.DebugModule.Prelude.strict_ord
                                                                   x3)
                                                     x6 <- do x4 <- DM.litHook
                                                                      (DI.DebugInfo
                                                                         (DI.SrcID "Char" 0)
                                                                         (DI.DynamicInfo [] []))
                                                                      (Prelude.return
                                                                         (Curry.DebugModule.Prelude.Char
                                                                            'a'))
                                                              DM.funcCallHook "ord"
                                                                (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                   (DI.DynamicInfo []
                                                                      [DI.genTerm x4]))
                                                                (Curry.DebugModule.Prelude.strict_ord
                                                                   x4)
                                                     DM.funcCallHook "-"
                                                       (DI.DebugInfo (DI.SrcID "Char" 0)
                                                          (DI.DynamicInfo []
                                                             [DI.genTerm x5, DI.genTerm x6]))
                                                       (Curry.DebugModule.Prelude.op_Minus x5 x6)
                                            x9 <- do x7 <- DM.litHook
                                                             (DI.DebugInfo (DI.SrcID "Char" 0)
                                                                (DI.DynamicInfo [] []))
                                                             (Prelude.return
                                                                (Curry.DebugModule.Prelude.Char
                                                                   'A'))
                                                     DM.funcCallHook "ord"
                                                       (DI.DebugInfo (DI.SrcID "Char" 0)
                                                          (DI.DynamicInfo [] [DI.genTerm x7]))
                                                       (Curry.DebugModule.Prelude.strict_ord x7)
                                            DM.funcCallHook "+"
                                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                              (Curry.DebugModule.Prelude.op_Plus x8 x9)
                                  DM.funcCallHook "chr"
                                    (DI.DebugInfo (DI.SrcID "Char" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10]))
                                    (Curry.DebugModule.Prelude.strict_chr x10)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x11 <- Prelude.return x1
                                  x12 <- DM.funcCallHook "otherwise"
                                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                           Curry.DebugModule.Prelude.strict_otherwise
                                  DM.funcCallHook "_case_9"
                                    (DI.DebugInfo (DI.SrcID "Char" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x11, DI.genTerm x12]))
                                    (strict__case_9 x11 x12)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x13])))
                           (strict__case_10 x1)
                           x13)))
term_strict__case_10 x1 = DI.Term "_case_10" (DI.SrcID "Char" 0) x1
strict__case_9 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_9"
         (DI.DebugInfo (DI.SrcID "Char" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x3 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Char" 0)
                  (DI.DynamicInfo [] [DI.genTerm x3]))
               (case x3 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x1))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Char" 0) (DI.DynamicInfo [] []))
                                 Curry.DebugModule.Prelude.strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Char" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x3])))
                           (strict__case_9 x1)
                           x3)))
term_strict__case_9 x1 = DI.Term "_case_9" (DI.SrcID "Char" 0) x1