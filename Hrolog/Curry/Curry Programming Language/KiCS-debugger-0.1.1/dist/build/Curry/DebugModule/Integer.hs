{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Curry.DebugModule.Integer where
import qualified Prelude
import qualified Curry.Debugger.DebugMonad as DM
import qualified Curry.Debugger.DebugInfo as DI
import qualified Curry.Debugger.PartCalls as PC
import qualified Data.Generics
import qualified Curry.DebugModule.Prelude
 
strict_pow ::
           (DM.DM dm) =>
             Curry.DebugModule.Prelude.Int ->
               Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
strict_pow x1 x2
  = DM.eval
      (DM.funcDeclHook "pow"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- Prelude.return x1
             x6 <- Prelude.return x2
             x7 <- do x3 <- Prelude.return x2
                      x4 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Zero)
                      DM.funcCallHook ">="
                        (DI.DebugInfo (DI.SrcID "Integer" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_GtEq x3 x4)
             DM.funcCallHook "_case_27"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
               (strict__case_27 x5 x6 x7)))
term_strict_pow x1 = DI.Term "pow" (DI.SrcID "Integer" 0) x1
 
x'xstrict_pow46powaux463 ::
                         (DM.DM dm) =>
                           Curry.DebugModule.Prelude.Int ->
                             Curry.DebugModule.Prelude.Int ->
                               Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
x'xstrict_pow46powaux463 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "pow.powaux.3"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x6 <- Prelude.return x1
             x7 <- Prelude.return x2
             x8 <- Prelude.return x3
             x9 <- do x4 <- Prelude.return x3
                      x5 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Zero)
                      DM.funcCallHook "=="
                        (DI.DebugInfo (DI.SrcID "Integer" 0)
                           (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                        (Curry.DebugModule.Prelude.op_EqEq x4 x5)
             DM.funcCallHook "_case_26"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo []
                     [DI.genTerm x6, DI.genTerm x7, DI.genTerm x8, DI.genTerm x9]))
               (strict__case_26 x6 x7 x8 x9)))
x'xterm_strict_pow46powaux463 x1
  = DI.Term "pow.powaux.3" (DI.SrcID "Integer" 0) x1
 
strict_ilog ::
            (DM.DM dm) =>
              Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
strict_ilog x1
  = DM.eval
      (DM.funcDeclHook "ilog"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             x5 <- do x2 <- Prelude.return x1
                      x3 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Zero)
                      DM.funcCallHook ">"
                        (DI.DebugInfo (DI.SrcID "Integer" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                        (Curry.DebugModule.Prelude.op_Gt x2 x3)
             DM.funcCallHook "_case_24"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
               (strict__case_24 x4 x5)))
term_strict_ilog x1 = DI.Term "ilog" (DI.SrcID "Integer" 0) x1
 
strict_isqrt ::
             (DM.DM dm) =>
               Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
strict_isqrt x1
  = DM.eval
      (DM.funcDeclHook "isqrt"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             x5 <- do x2 <- Prelude.return x1
                      x3 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Zero)
                      DM.funcCallHook ">="
                        (DI.DebugInfo (DI.SrcID "Integer" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                        (Curry.DebugModule.Prelude.op_GtEq x2 x3)
             DM.funcCallHook "_case_22"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
               (strict__case_22 x4 x5)))
term_strict_isqrt x1 = DI.Term "isqrt" (DI.SrcID "Integer" 0) x1
 
x'xstrict_isqrt46aux4621 ::
                         (DM.DM dm) =>
                           Curry.DebugModule.Prelude.Int ->
                             Curry.DebugModule.Prelude.Int ->
                               Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
x'xstrict_isqrt46aux4621 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "isqrt.aux.21"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x8 <- Prelude.return x1
             x9 <- Prelude.return x2
             x10 <- Prelude.return x3
             x11 <- do x6 <- Prelude.return x3
                       x7 <- do x4 <- Prelude.return x2
                                x5 <- DM.litHook
                                        (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                        (Prelude.return
                                           (Curry.DebugModule.Prelude.Pos
                                              Curry.DebugModule.Prelude.IHi))
                                DM.funcCallHook "+"
                                  (DI.DebugInfo (DI.SrcID "Integer" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                  (Curry.DebugModule.Prelude.op_Plus x4 x5)
                       DM.funcCallHook "=="
                         (DI.DebugInfo (DI.SrcID "Integer" 0)
                            (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                         (Curry.DebugModule.Prelude.op_EqEq x6 x7)
             DM.funcCallHook "_case_19"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo []
                     [DI.genTerm x8, DI.genTerm x9, DI.genTerm x10, DI.genTerm x11]))
               (strict__case_19 x8 x9 x10 x11)))
x'xterm_strict_isqrt46aux4621 x1
  = DI.Term "isqrt.aux.21" (DI.SrcID "Integer" 0) x1
 
strict_factorial ::
                 (DM.DM dm) =>
                   Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
strict_factorial x1
  = DM.eval
      (DM.funcDeclHook "factorial"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             x5 <- do x2 <- Prelude.return x1
                      x3 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Zero)
                      DM.funcCallHook ">="
                        (DI.DebugInfo (DI.SrcID "Integer" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                        (Curry.DebugModule.Prelude.op_GtEq x2 x3)
             DM.funcCallHook "_case_17"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
               (strict__case_17 x4 x5)))
term_strict_factorial x1
  = DI.Term "factorial" (DI.SrcID "Integer" 0) x1
 
strict_binomial ::
                (DM.DM dm) =>
                  Curry.DebugModule.Prelude.Int ->
                    Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
strict_binomial x1 x2
  = DM.eval
      (DM.funcDeclHook "binomial"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x9 <- Prelude.return x1
             x10 <- Prelude.return x2
             x11 <- do x7 <- do x3 <- Prelude.return x2
                                x4 <- DM.litHook
                                        (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                        (Prelude.return Curry.DebugModule.Prelude.Zero)
                                DM.funcCallHook ">"
                                  (DI.DebugInfo (DI.SrcID "Integer" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                                  (Curry.DebugModule.Prelude.op_Gt x3 x4)
                       x8 <- do x5 <- Prelude.return x1
                                x6 <- Prelude.return x2
                                DM.funcCallHook ">="
                                  (DI.DebugInfo (DI.SrcID "Integer" 0)
                                     (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                  (Curry.DebugModule.Prelude.op_GtEq x5 x6)
                       DM.funcCallHook "&&"
                         (DI.DebugInfo (DI.SrcID "Integer" 0)
                            (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                         (Curry.DebugModule.Prelude.op_AndAnd x7 x8)
             DM.funcCallHook "_case_15"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo []
                     [DI.genTerm x9, DI.genTerm x10, DI.genTerm x11]))
               (strict__case_15 x9 x10 x11)))
term_strict_binomial x1
  = DI.Term "binomial" (DI.SrcID "Integer" 0) x1
 
x'xstrict_binomial46aux4641 ::
                            (DM.DM dm) =>
                              Curry.DebugModule.Prelude.Int ->
                                Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
x'xstrict_binomial46aux4641 x1 x2
  = DM.eval
      (DM.funcDeclHook "binomial.aux.41"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- Prelude.return x1
             x6 <- Prelude.return x2
             x7 <- do x3 <- Prelude.return x1
                      x4 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Zero)
                      DM.funcCallHook "=="
                        (DI.DebugInfo (DI.SrcID "Integer" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_EqEq x3 x4)
             DM.funcCallHook "_case_14"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
               (strict__case_14 x5 x6 x7)))
x'xterm_strict_binomial46aux4641 x1
  = DI.Term "binomial.aux.41" (DI.SrcID "Integer" 0) x1
 
strict_abs ::
           (DM.DM dm) =>
             Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
strict_abs x1
  = DM.eval
      (DM.funcDeclHook "abs"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- Prelude.return x1
             x5 <- do x2 <- Prelude.return x1
                      x3 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Zero)
                      DM.funcCallHook "<"
                        (DI.DebugInfo (DI.SrcID "Integer" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                        (Curry.DebugModule.Prelude.op_Lt x2 x3)
             DM.funcCallHook "_case_13"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
               (strict__case_13 x4 x5)))
term_strict_abs x1 = DI.Term "abs" (DI.SrcID "Integer" 0) x1
 
strict_max ::
           (DM.DM dm) =>
             Curry.DebugModule.Prelude.Int ->
               Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
strict_max x1 x2
  = DM.eval
      (DM.funcDeclHook "max"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- Prelude.return x1
             x6 <- Prelude.return x2
             x7 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.funcCallHook "<"
                        (DI.DebugInfo (DI.SrcID "Integer" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_Lt x3 x4)
             DM.funcCallHook "_case_12"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
               (strict__case_12 x5 x6 x7)))
term_strict_max x1 = DI.Term "max" (DI.SrcID "Integer" 0) x1
 
strict_min ::
           (DM.DM dm) =>
             Curry.DebugModule.Prelude.Int ->
               Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
strict_min x1 x2
  = DM.eval
      (DM.funcDeclHook "min"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- Prelude.return x1
             x6 <- Prelude.return x2
             x7 <- do x3 <- Prelude.return x1
                      x4 <- Prelude.return x2
                      DM.funcCallHook "<"
                        (DI.DebugInfo (DI.SrcID "Integer" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_Lt x3 x4)
             DM.funcCallHook "_case_11"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
               (strict__case_11 x5 x6 x7)))
term_strict_min x1 = DI.Term "min" (DI.SrcID "Integer" 0) x1
 
strict_max3 ::
            (DM.DM dm) =>
              Curry.DebugModule.Prelude.Int ->
                Curry.DebugModule.Prelude.Int ->
                  Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
strict_max3 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "max3"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x6 <- Prelude.return x1
             x7 <- do x4 <- Prelude.return x2
                      x5 <- Prelude.return x3
                      DM.funcCallHook "max"
                        (DI.DebugInfo (DI.SrcID "Integer" 0)
                           (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                        (strict_max x4 x5)
             DM.funcCallHook "max"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
               (strict_max x6 x7)))
term_strict_max3 x1 = DI.Term "max3" (DI.SrcID "Integer" 0) x1
 
strict_min3 ::
            (DM.DM dm) =>
              Curry.DebugModule.Prelude.Int ->
                Curry.DebugModule.Prelude.Int ->
                  Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
strict_min3 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "min3"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x6 <- Prelude.return x1
             x7 <- do x4 <- Prelude.return x2
                      x5 <- Prelude.return x3
                      DM.funcCallHook "min"
                        (DI.DebugInfo (DI.SrcID "Integer" 0)
                           (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                        (strict_min x4 x5)
             DM.funcCallHook "min"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
               (strict_min x6 x7)))
term_strict_min3 x1 = DI.Term "min3" (DI.SrcID "Integer" 0) x1
 
strict_maxlist ::
               (DM.DM dm) =>
                 Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Int ->
                   dm Curry.DebugModule.Prelude.Int
strict_maxlist x1
  = DM.eval
      (DM.funcDeclHook "maxlist"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_10"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_10 x2)))
term_strict_maxlist x1
  = DI.Term "maxlist" (DI.SrcID "Integer" 0) x1
 
strict_minlist ::
               (DM.DM dm) =>
                 Curry.DebugModule.Prelude.List Curry.DebugModule.Prelude.Int ->
                   dm Curry.DebugModule.Prelude.Int
strict_minlist x1
  = DM.eval
      (DM.funcDeclHook "minlist"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return x1
             DM.funcCallHook "_case_8"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2]))
               (strict__case_8 x2)))
term_strict_minlist x1
  = DI.Term "minlist" (DI.SrcID "Integer" 0) x1
 
strict_bitTrunc ::
                (DM.DM dm) =>
                  Curry.DebugModule.Prelude.Int ->
                    Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
strict_bitTrunc x1 x2
  = DM.eval
      (DM.funcDeclHook "bitTrunc"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x7 <- do x5 <- do x3 <- DM.litHook
                                       (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                       (Prelude.return
                                          (Curry.DebugModule.Prelude.Pos
                                             (Curry.DebugModule.Prelude.O
                                                Curry.DebugModule.Prelude.IHi)))
                               x4 <- Prelude.return x1
                               DM.funcCallHook "pow"
                                 (DI.DebugInfo (DI.SrcID "Integer" 0)
                                    (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                                 (strict_pow x3 x4)
                      x6 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                              (Prelude.return
                                 (Curry.DebugModule.Prelude.Pos Curry.DebugModule.Prelude.IHi))
                      DM.funcCallHook "-"
                        (DI.DebugInfo (DI.SrcID "Integer" 0)
                           (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                        (Curry.DebugModule.Prelude.op_Minus x5 x6)
             x8 <- Prelude.return x2
             DM.funcCallHook "bitAnd"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
               (strict_bitAnd x7 x8)))
term_strict_bitTrunc x1
  = DI.Term "bitTrunc" (DI.SrcID "Integer" 0) x1
 
strict_bitAnd ::
              (DM.DM dm) =>
                Curry.DebugModule.Prelude.Int ->
                  Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
strict_bitAnd x1 x2
  = DM.eval
      (DM.funcDeclHook "bitAnd"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- Prelude.return x1
             x6 <- Prelude.return x2
             x7 <- do x3 <- Prelude.return x2
                      x4 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Zero)
                      DM.funcCallHook "=="
                        (DI.DebugInfo (DI.SrcID "Integer" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_EqEq x3 x4)
             DM.funcCallHook "_case_6"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
               (strict__case_6 x5 x6 x7)))
term_strict_bitAnd x1 = DI.Term "bitAnd" (DI.SrcID "Integer" 0) x1
 
strict_bitOr ::
             (DM.DM dm) =>
               Curry.DebugModule.Prelude.Int ->
                 Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
strict_bitOr x1 x2
  = DM.eval
      (DM.funcDeclHook "bitOr"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- Prelude.return x1
             x6 <- Prelude.return x2
             x7 <- do x3 <- Prelude.return x2
                      x4 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Zero)
                      DM.funcCallHook "=="
                        (DI.DebugInfo (DI.SrcID "Integer" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_EqEq x3 x4)
             DM.funcCallHook "_case_4"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
               (strict__case_4 x5 x6 x7)))
term_strict_bitOr x1 = DI.Term "bitOr" (DI.SrcID "Integer" 0) x1
 
strict_bitNot ::
              (DM.DM dm) =>
                Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
strict_bitNot x1
  = DM.eval
      (DM.funcDeclHook "bitNot"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                     (Prelude.return
                        (Curry.DebugModule.Prelude.Pos
                           (Curry.DebugModule.Prelude.O
                              (Curry.DebugModule.Prelude.O
                                 (Curry.DebugModule.Prelude.O
                                    (Curry.DebugModule.Prelude.O
                                       (Curry.DebugModule.Prelude.O
                                          Curry.DebugModule.Prelude.IHi)))))))
             x3 <- Prelude.return x1
             DM.funcCallHook "bitNot.aux.100"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (x'xstrict_bitNot46aux46100 x2 x3)))
term_strict_bitNot x1 = DI.Term "bitNot" (DI.SrcID "Integer" 0) x1
 
x'xstrict_bitNot46aux46100 ::
                           (DM.DM dm) =>
                             Curry.DebugModule.Prelude.Int ->
                               Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
x'xstrict_bitNot46aux46100 x1 x2
  = DM.eval
      (DM.funcDeclHook "bitNot.aux.100"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- Prelude.return x1
             x6 <- Prelude.return x2
             x7 <- do x3 <- Prelude.return x1
                      x4 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Zero)
                      DM.funcCallHook "=="
                        (DI.DebugInfo (DI.SrcID "Integer" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_EqEq x3 x4)
             DM.funcCallHook "_case_2"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
               (strict__case_2 x5 x6 x7)))
x'xterm_strict_bitNot46aux46100 x1
  = DI.Term "bitNot.aux.100" (DI.SrcID "Integer" 0) x1
 
strict_bitXor ::
              (DM.DM dm) =>
                Curry.DebugModule.Prelude.Int ->
                  Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Int
strict_bitXor x1 x2
  = DM.eval
      (DM.funcDeclHook "bitXor"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- Prelude.return x1
             x6 <- Prelude.return x2
             x7 <- do x3 <- Prelude.return x2
                      x4 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                              (Prelude.return Curry.DebugModule.Prelude.Zero)
                      DM.funcCallHook "=="
                        (DI.DebugInfo (DI.SrcID "Integer" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_EqEq x3 x4)
             DM.funcCallHook "_case_1"
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6, DI.genTerm x7]))
               (strict__case_1 x5 x6 x7)))
term_strict_bitXor x1 = DI.Term "bitXor" (DI.SrcID "Integer" 0) x1
 
strict_even ::
            (DM.DM dm) =>
              Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Bool
strict_even x1
  = DM.eval
      (DM.funcDeclHook "even"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- do x2 <- Prelude.return x1
                      x3 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                              (Prelude.return
                                 (Curry.DebugModule.Prelude.Pos
                                    (Curry.DebugModule.Prelude.O Curry.DebugModule.Prelude.IHi)))
                      DM.funcCallHook "mod"
                        (DI.DebugInfo (DI.SrcID "Integer" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                        (Curry.DebugModule.Prelude.strict_mod x2 x3)
             x5 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                     (Prelude.return Curry.DebugModule.Prelude.Zero)
             DM.funcCallHook "=="
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
               (Curry.DebugModule.Prelude.op_EqEq x4 x5)))
term_strict_even x1 = DI.Term "even" (DI.SrcID "Integer" 0) x1
 
strict_odd ::
           (DM.DM dm) =>
             Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Bool
strict_odd x1
  = DM.eval
      (DM.funcDeclHook "odd"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x4 <- do x2 <- Prelude.return x1
                      x3 <- DM.litHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                              (Prelude.return
                                 (Curry.DebugModule.Prelude.Pos
                                    (Curry.DebugModule.Prelude.O Curry.DebugModule.Prelude.IHi)))
                      DM.funcCallHook "mod"
                        (DI.DebugInfo (DI.SrcID "Integer" 0)
                           (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
                        (Curry.DebugModule.Prelude.strict_mod x2 x3)
             x5 <- DM.litHook
                     (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                     (Prelude.return Curry.DebugModule.Prelude.Zero)
             DM.funcCallHook "/="
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
               (Curry.DebugModule.Prelude.op_SlashEq x4 x5)))
term_strict_odd x1 = DI.Term "odd" (DI.SrcID "Integer" 0) x1
strict__case_1 x1 x2 x5
  = DM.eval
      (DM.funcDeclHook "_case_1"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x5]))
         (do x25 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x25]))
               (case x25 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x1))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x3 <- do x12 <- DM.litHook
                                                       (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                          (DI.DynamicInfo [] []))
                                                       (Prelude.return
                                                          (Curry.DebugModule.Prelude.Pos
                                                             (Curry.DebugModule.Prelude.O
                                                                Curry.DebugModule.Prelude.IHi)))
                                              x13 <- do x10 <- do x6 <- Prelude.return x1
                                                                  x7 <- DM.litHook
                                                                          (DI.DebugInfo
                                                                             (DI.SrcID "Integer" 0)
                                                                             (DI.DynamicInfo [] []))
                                                                          (Prelude.return
                                                                             (Curry.DebugModule.Prelude.Pos
                                                                                (Curry.DebugModule.Prelude.O
                                                                                   Curry.DebugModule.Prelude.IHi)))
                                                                  DM.funcCallHook "div"
                                                                    (DI.DebugInfo
                                                                       (DI.SrcID "Integer" 0)
                                                                       (DI.DynamicInfo []
                                                                          [DI.genTerm x6,
                                                                           DI.genTerm x7]))
                                                                    (Curry.DebugModule.Prelude.strict_div
                                                                       x6
                                                                       x7)
                                                        x11 <- do x8 <- Prelude.return x2
                                                                  x9 <- DM.litHook
                                                                          (DI.DebugInfo
                                                                             (DI.SrcID "Integer" 0)
                                                                             (DI.DynamicInfo [] []))
                                                                          (Prelude.return
                                                                             (Curry.DebugModule.Prelude.Pos
                                                                                (Curry.DebugModule.Prelude.O
                                                                                   Curry.DebugModule.Prelude.IHi)))
                                                                  DM.funcCallHook "div"
                                                                    (DI.DebugInfo
                                                                       (DI.SrcID "Integer" 0)
                                                                       (DI.DynamicInfo []
                                                                          [DI.genTerm x8,
                                                                           DI.genTerm x9]))
                                                                    (Curry.DebugModule.Prelude.strict_div
                                                                       x8
                                                                       x9)
                                                        DM.funcCallHook "bitXor"
                                                          (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                             (DI.DynamicInfo []
                                                                [DI.genTerm x10, DI.genTerm x11]))
                                                          (strict_bitXor x10 x11)
                                              DM.funcCallHook "*"
                                                (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                   (DI.DynamicInfo []
                                                      [DI.genTerm x12, DI.genTerm x13]))
                                                (Curry.DebugModule.Prelude.op_Asterisk x12 x13)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x4 <- do x20 <- Prelude.return x1
                                                       x21 <- Prelude.return x2
                                                       x22 <- do x18 <- do x14 <- Prelude.return x2
                                                                           x15 <- DM.litHook
                                                                                    (DI.DebugInfo
                                                                                       (DI.SrcID
                                                                                          "Integer"
                                                                                          0)
                                                                                       (DI.DynamicInfo
                                                                                          []
                                                                                          []))
                                                                                    (Prelude.return
                                                                                       (Curry.DebugModule.Prelude.Pos
                                                                                          (Curry.DebugModule.Prelude.O
                                                                                             Curry.DebugModule.Prelude.IHi)))
                                                                           DM.funcCallHook "mod"
                                                                             (DI.DebugInfo
                                                                                (DI.SrcID "Integer"
                                                                                   0)
                                                                                (DI.DynamicInfo []
                                                                                   [DI.genTerm x14,
                                                                                    DI.genTerm
                                                                                      x15]))
                                                                             (Curry.DebugModule.Prelude.strict_mod
                                                                                x14
                                                                                x15)
                                                                 x19 <- do x16 <- Prelude.return x1
                                                                           x17 <- DM.litHook
                                                                                    (DI.DebugInfo
                                                                                       (DI.SrcID
                                                                                          "Integer"
                                                                                          0)
                                                                                       (DI.DynamicInfo
                                                                                          []
                                                                                          []))
                                                                                    (Prelude.return
                                                                                       (Curry.DebugModule.Prelude.Pos
                                                                                          (Curry.DebugModule.Prelude.O
                                                                                             Curry.DebugModule.Prelude.IHi)))
                                                                           DM.funcCallHook "mod"
                                                                             (DI.DebugInfo
                                                                                (DI.SrcID "Integer"
                                                                                   0)
                                                                                (DI.DynamicInfo []
                                                                                   [DI.genTerm x16,
                                                                                    DI.genTerm
                                                                                      x17]))
                                                                             (Curry.DebugModule.Prelude.strict_mod
                                                                                x16
                                                                                x17)
                                                                 DM.funcCallHook "=="
                                                                   (DI.DebugInfo
                                                                      (DI.SrcID "Integer" 0)
                                                                      (DI.DynamicInfo []
                                                                         [DI.genTerm x18,
                                                                          DI.genTerm x19]))
                                                                   (Curry.DebugModule.Prelude.op_EqEq
                                                                      x18
                                                                      x19)
                                                       DM.funcCallHook "_case_0"
                                                         (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                            (DI.DynamicInfo []
                                                               [DI.genTerm x20, DI.genTerm x21,
                                                                DI.genTerm x22]))
                                                         (strict__case_0 x20 x21 x22)
                                              DM.eval
                                                (do x23 <- Prelude.return x3
                                                    x24 <- Prelude.return x4
                                                    DM.funcCallHook "+"
                                                      (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                         (DI.DynamicInfo []
                                                            [DI.genTerm x23, DI.genTerm x24]))
                                                      (Curry.DebugModule.Prelude.op_Plus x23
                                                         x24)))))))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x25])))
                           (strict__case_1 x1 x2)
                           x25)))
term_strict__case_1 x1
  = DI.Term "_case_1" (DI.SrcID "Integer" 0) x1
strict__case_0 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_0"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.litHook
                                 (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Zero)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.litHook
                                 (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return
                                    (Curry.DebugModule.Prelude.Pos Curry.DebugModule.Prelude.IHi))))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           (strict__case_0 x1 x2)
                           x4)))
term_strict__case_0 x1
  = DI.Term "_case_0" (DI.SrcID "Integer" 0) x1
strict__case_2 x1 x2 x5
  = DM.eval
      (DM.funcDeclHook "_case_2"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x5]))
         (do x20 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x20]))
               (case x20 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.litHook
                                 (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Zero)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x3 <- do x12 <- DM.litHook
                                                       (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                          (DI.DynamicInfo [] []))
                                                       (Prelude.return
                                                          (Curry.DebugModule.Prelude.Pos
                                                             (Curry.DebugModule.Prelude.O
                                                                Curry.DebugModule.Prelude.IHi)))
                                              x13 <- do x10 <- do x6 <- Prelude.return x1
                                                                  x7 <- DM.litHook
                                                                          (DI.DebugInfo
                                                                             (DI.SrcID "Integer" 0)
                                                                             (DI.DynamicInfo [] []))
                                                                          (Prelude.return
                                                                             (Curry.DebugModule.Prelude.Pos
                                                                                Curry.DebugModule.Prelude.IHi))
                                                                  DM.funcCallHook "-"
                                                                    (DI.DebugInfo
                                                                       (DI.SrcID "Integer" 0)
                                                                       (DI.DynamicInfo []
                                                                          [DI.genTerm x6,
                                                                           DI.genTerm x7]))
                                                                    (Curry.DebugModule.Prelude.op_Minus
                                                                       x6
                                                                       x7)
                                                        x11 <- do x8 <- Prelude.return x2
                                                                  x9 <- DM.litHook
                                                                          (DI.DebugInfo
                                                                             (DI.SrcID "Integer" 0)
                                                                             (DI.DynamicInfo [] []))
                                                                          (Prelude.return
                                                                             (Curry.DebugModule.Prelude.Pos
                                                                                (Curry.DebugModule.Prelude.O
                                                                                   Curry.DebugModule.Prelude.IHi)))
                                                                  DM.funcCallHook "div"
                                                                    (DI.DebugInfo
                                                                       (DI.SrcID "Integer" 0)
                                                                       (DI.DynamicInfo []
                                                                          [DI.genTerm x8,
                                                                           DI.genTerm x9]))
                                                                    (Curry.DebugModule.Prelude.strict_div
                                                                       x8
                                                                       x9)
                                                        DM.funcCallHook "bitNot.aux.100"
                                                          (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                             (DI.DynamicInfo []
                                                                [DI.genTerm x10, DI.genTerm x11]))
                                                          (x'xstrict_bitNot46aux46100 x10 x11)
                                              DM.funcCallHook "*"
                                                (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                   (DI.DynamicInfo []
                                                      [DI.genTerm x12, DI.genTerm x13]))
                                                (Curry.DebugModule.Prelude.op_Asterisk x12 x13)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x4 <- do x16 <- DM.litHook
                                                                (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                                   (DI.DynamicInfo [] []))
                                                                (Prelude.return
                                                                   (Curry.DebugModule.Prelude.Pos
                                                                      Curry.DebugModule.Prelude.IHi))
                                                       x17 <- do x14 <- Prelude.return x2
                                                                 x15 <- DM.litHook
                                                                          (DI.DebugInfo
                                                                             (DI.SrcID "Integer" 0)
                                                                             (DI.DynamicInfo [] []))
                                                                          (Prelude.return
                                                                             (Curry.DebugModule.Prelude.Pos
                                                                                (Curry.DebugModule.Prelude.O
                                                                                   Curry.DebugModule.Prelude.IHi)))
                                                                 DM.funcCallHook "mod"
                                                                   (DI.DebugInfo
                                                                      (DI.SrcID "Integer" 0)
                                                                      (DI.DynamicInfo []
                                                                         [DI.genTerm x14,
                                                                          DI.genTerm x15]))
                                                                   (Curry.DebugModule.Prelude.strict_mod
                                                                      x14
                                                                      x15)
                                                       DM.funcCallHook "-"
                                                         (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                            (DI.DynamicInfo []
                                                               [DI.genTerm x16, DI.genTerm x17]))
                                                         (Curry.DebugModule.Prelude.op_Minus x16
                                                            x17)
                                              DM.eval
                                                (do x18 <- Prelude.return x3
                                                    x19 <- Prelude.return x4
                                                    DM.funcCallHook "+"
                                                      (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                         (DI.DynamicInfo []
                                                            [DI.genTerm x18, DI.genTerm x19]))
                                                      (Curry.DebugModule.Prelude.op_Plus x18
                                                         x19)))))))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x20])))
                           (strict__case_2 x1 x2)
                           x20)))
term_strict__case_2 x1
  = DI.Term "_case_2" (DI.SrcID "Integer" 0) x1
strict__case_4 x1 x2 x5
  = DM.eval
      (DM.funcDeclHook "_case_4"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x5]))
         (do x23 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x23]))
               (case x23 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x1))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x3 <- do x12 <- DM.litHook
                                                       (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                          (DI.DynamicInfo [] []))
                                                       (Prelude.return
                                                          (Curry.DebugModule.Prelude.Pos
                                                             (Curry.DebugModule.Prelude.O
                                                                Curry.DebugModule.Prelude.IHi)))
                                              x13 <- do x10 <- do x6 <- Prelude.return x1
                                                                  x7 <- DM.litHook
                                                                          (DI.DebugInfo
                                                                             (DI.SrcID "Integer" 0)
                                                                             (DI.DynamicInfo [] []))
                                                                          (Prelude.return
                                                                             (Curry.DebugModule.Prelude.Pos
                                                                                (Curry.DebugModule.Prelude.O
                                                                                   Curry.DebugModule.Prelude.IHi)))
                                                                  DM.funcCallHook "div"
                                                                    (DI.DebugInfo
                                                                       (DI.SrcID "Integer" 0)
                                                                       (DI.DynamicInfo []
                                                                          [DI.genTerm x6,
                                                                           DI.genTerm x7]))
                                                                    (Curry.DebugModule.Prelude.strict_div
                                                                       x6
                                                                       x7)
                                                        x11 <- do x8 <- Prelude.return x2
                                                                  x9 <- DM.litHook
                                                                          (DI.DebugInfo
                                                                             (DI.SrcID "Integer" 0)
                                                                             (DI.DynamicInfo [] []))
                                                                          (Prelude.return
                                                                             (Curry.DebugModule.Prelude.Pos
                                                                                (Curry.DebugModule.Prelude.O
                                                                                   Curry.DebugModule.Prelude.IHi)))
                                                                  DM.funcCallHook "div"
                                                                    (DI.DebugInfo
                                                                       (DI.SrcID "Integer" 0)
                                                                       (DI.DynamicInfo []
                                                                          [DI.genTerm x8,
                                                                           DI.genTerm x9]))
                                                                    (Curry.DebugModule.Prelude.strict_div
                                                                       x8
                                                                       x9)
                                                        DM.funcCallHook "bitOr"
                                                          (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                             (DI.DynamicInfo []
                                                                [DI.genTerm x10, DI.genTerm x11]))
                                                          (strict_bitOr x10 x11)
                                              DM.funcCallHook "*"
                                                (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                   (DI.DynamicInfo []
                                                      [DI.genTerm x12, DI.genTerm x13]))
                                                (Curry.DebugModule.Prelude.op_Asterisk x12 x13)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x4 <- do x18 <- Prelude.return x1
                                                       x19 <- Prelude.return x2
                                                       x20 <- do x16 <- do x14 <- Prelude.return x2
                                                                           x15 <- DM.litHook
                                                                                    (DI.DebugInfo
                                                                                       (DI.SrcID
                                                                                          "Integer"
                                                                                          0)
                                                                                       (DI.DynamicInfo
                                                                                          []
                                                                                          []))
                                                                                    (Prelude.return
                                                                                       (Curry.DebugModule.Prelude.Pos
                                                                                          (Curry.DebugModule.Prelude.O
                                                                                             Curry.DebugModule.Prelude.IHi)))
                                                                           DM.funcCallHook "mod"
                                                                             (DI.DebugInfo
                                                                                (DI.SrcID "Integer"
                                                                                   0)
                                                                                (DI.DynamicInfo []
                                                                                   [DI.genTerm x14,
                                                                                    DI.genTerm
                                                                                      x15]))
                                                                             (Curry.DebugModule.Prelude.strict_mod
                                                                                x14
                                                                                x15)
                                                                 x17 <- DM.litHook
                                                                          (DI.DebugInfo
                                                                             (DI.SrcID "Integer" 0)
                                                                             (DI.DynamicInfo [] []))
                                                                          (Prelude.return
                                                                             (Curry.DebugModule.Prelude.Pos
                                                                                Curry.DebugModule.Prelude.IHi))
                                                                 DM.funcCallHook "=="
                                                                   (DI.DebugInfo
                                                                      (DI.SrcID "Integer" 0)
                                                                      (DI.DynamicInfo []
                                                                         [DI.genTerm x16,
                                                                          DI.genTerm x17]))
                                                                   (Curry.DebugModule.Prelude.op_EqEq
                                                                      x16
                                                                      x17)
                                                       DM.funcCallHook "_case_3"
                                                         (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                            (DI.DynamicInfo []
                                                               [DI.genTerm x18, DI.genTerm x19,
                                                                DI.genTerm x20]))
                                                         (strict__case_3 x18 x19 x20)
                                              DM.eval
                                                (do x21 <- Prelude.return x3
                                                    x22 <- Prelude.return x4
                                                    DM.funcCallHook "+"
                                                      (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                         (DI.DynamicInfo []
                                                            [DI.genTerm x21, DI.genTerm x22]))
                                                      (Curry.DebugModule.Prelude.op_Plus x21
                                                         x22)))))))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x23])))
                           (strict__case_4 x1 x2)
                           x23)))
term_strict__case_4 x1
  = DI.Term "_case_4" (DI.SrcID "Integer" 0) x1
strict__case_3 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_3"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x6 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x6]))
               (case x6 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.litHook
                                 (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return
                                    (Curry.DebugModule.Prelude.Pos Curry.DebugModule.Prelude.IHi))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- Prelude.return x1
                                  x5 <- DM.litHook
                                          (DI.DebugInfo (DI.SrcID "Integer" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return
                                             (Curry.DebugModule.Prelude.Pos
                                                (Curry.DebugModule.Prelude.O
                                                   Curry.DebugModule.Prelude.IHi)))
                                  DM.funcCallHook "mod"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                    (Curry.DebugModule.Prelude.strict_mod x4 x5)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x6])))
                           (strict__case_3 x1 x2)
                           x6)))
term_strict__case_3 x1
  = DI.Term "_case_3" (DI.SrcID "Integer" 0) x1
strict__case_6 x1 x2 x5
  = DM.eval
      (DM.funcDeclHook "_case_6"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x5]))
         (do x23 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x23]))
               (case x23 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.litHook
                                 (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Zero)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x3 <- do x12 <- DM.litHook
                                                       (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                          (DI.DynamicInfo [] []))
                                                       (Prelude.return
                                                          (Curry.DebugModule.Prelude.Pos
                                                             (Curry.DebugModule.Prelude.O
                                                                Curry.DebugModule.Prelude.IHi)))
                                              x13 <- do x10 <- do x6 <- Prelude.return x1
                                                                  x7 <- DM.litHook
                                                                          (DI.DebugInfo
                                                                             (DI.SrcID "Integer" 0)
                                                                             (DI.DynamicInfo [] []))
                                                                          (Prelude.return
                                                                             (Curry.DebugModule.Prelude.Pos
                                                                                (Curry.DebugModule.Prelude.O
                                                                                   Curry.DebugModule.Prelude.IHi)))
                                                                  DM.funcCallHook "div"
                                                                    (DI.DebugInfo
                                                                       (DI.SrcID "Integer" 0)
                                                                       (DI.DynamicInfo []
                                                                          [DI.genTerm x6,
                                                                           DI.genTerm x7]))
                                                                    (Curry.DebugModule.Prelude.strict_div
                                                                       x6
                                                                       x7)
                                                        x11 <- do x8 <- Prelude.return x2
                                                                  x9 <- DM.litHook
                                                                          (DI.DebugInfo
                                                                             (DI.SrcID "Integer" 0)
                                                                             (DI.DynamicInfo [] []))
                                                                          (Prelude.return
                                                                             (Curry.DebugModule.Prelude.Pos
                                                                                (Curry.DebugModule.Prelude.O
                                                                                   Curry.DebugModule.Prelude.IHi)))
                                                                  DM.funcCallHook "div"
                                                                    (DI.DebugInfo
                                                                       (DI.SrcID "Integer" 0)
                                                                       (DI.DynamicInfo []
                                                                          [DI.genTerm x8,
                                                                           DI.genTerm x9]))
                                                                    (Curry.DebugModule.Prelude.strict_div
                                                                       x8
                                                                       x9)
                                                        DM.funcCallHook "bitAnd"
                                                          (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                             (DI.DynamicInfo []
                                                                [DI.genTerm x10, DI.genTerm x11]))
                                                          (strict_bitAnd x10 x11)
                                              DM.funcCallHook "*"
                                                (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                   (DI.DynamicInfo []
                                                      [DI.genTerm x12, DI.genTerm x13]))
                                                (Curry.DebugModule.Prelude.op_Asterisk x12 x13)
                                     DM.eval
                                       (DM.letHook
                                          (DI.DebugInfo (DI.SrcID "DummyModule" 42)
                                             (DI.DynamicInfo [] []))
                                          (do x4 <- do x18 <- Prelude.return x1
                                                       x19 <- Prelude.return x2
                                                       x20 <- do x16 <- do x14 <- Prelude.return x2
                                                                           x15 <- DM.litHook
                                                                                    (DI.DebugInfo
                                                                                       (DI.SrcID
                                                                                          "Integer"
                                                                                          0)
                                                                                       (DI.DynamicInfo
                                                                                          []
                                                                                          []))
                                                                                    (Prelude.return
                                                                                       (Curry.DebugModule.Prelude.Pos
                                                                                          (Curry.DebugModule.Prelude.O
                                                                                             Curry.DebugModule.Prelude.IHi)))
                                                                           DM.funcCallHook "mod"
                                                                             (DI.DebugInfo
                                                                                (DI.SrcID "Integer"
                                                                                   0)
                                                                                (DI.DynamicInfo []
                                                                                   [DI.genTerm x14,
                                                                                    DI.genTerm
                                                                                      x15]))
                                                                             (Curry.DebugModule.Prelude.strict_mod
                                                                                x14
                                                                                x15)
                                                                 x17 <- DM.litHook
                                                                          (DI.DebugInfo
                                                                             (DI.SrcID "Integer" 0)
                                                                             (DI.DynamicInfo [] []))
                                                                          (Prelude.return
                                                                             Curry.DebugModule.Prelude.Zero)
                                                                 DM.funcCallHook "=="
                                                                   (DI.DebugInfo
                                                                      (DI.SrcID "Integer" 0)
                                                                      (DI.DynamicInfo []
                                                                         [DI.genTerm x16,
                                                                          DI.genTerm x17]))
                                                                   (Curry.DebugModule.Prelude.op_EqEq
                                                                      x16
                                                                      x17)
                                                       DM.funcCallHook "_case_5"
                                                         (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                            (DI.DynamicInfo []
                                                               [DI.genTerm x18, DI.genTerm x19,
                                                                DI.genTerm x20]))
                                                         (strict__case_5 x18 x19 x20)
                                              DM.eval
                                                (do x21 <- Prelude.return x3
                                                    x22 <- Prelude.return x4
                                                    DM.funcCallHook "+"
                                                      (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                         (DI.DynamicInfo []
                                                            [DI.genTerm x21, DI.genTerm x22]))
                                                      (Curry.DebugModule.Prelude.op_Plus x21
                                                         x22)))))))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x23])))
                           (strict__case_6 x1 x2)
                           x23)))
term_strict__case_6 x1
  = DI.Term "_case_6" (DI.SrcID "Integer" 0) x1
strict__case_5 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_5"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x6 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x6]))
               (case x6 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.litHook
                                 (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Zero)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- Prelude.return x1
                                  x5 <- DM.litHook
                                          (DI.DebugInfo (DI.SrcID "Integer" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return
                                             (Curry.DebugModule.Prelude.Pos
                                                (Curry.DebugModule.Prelude.O
                                                   Curry.DebugModule.Prelude.IHi)))
                                  DM.funcCallHook "mod"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                    (Curry.DebugModule.Prelude.strict_mod x4 x5)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x6])))
                           (strict__case_5 x1 x2)
                           x6)))
term_strict__case_5 x1
  = DI.Term "_case_5" (DI.SrcID "Integer" 0) x1
strict__case_8 x1
  = DM.eval
      (DM.funcDeclHook "_case_8"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x6 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x6]))
               (case x6 of
                    Curry.DebugModule.Prelude.Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- Prelude.return x2
                                  x5 <- Prelude.return x3
                                  DM.funcCallHook "_case_7"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                    (strict__case_7 x4 x5)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Integer" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x6])))
                           strict__case_8
                           x6)))
term_strict__case_8 x1
  = DI.Term "_case_8" (DI.SrcID "Integer" 0) x1
strict__case_7 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_7"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
         (do x11 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    Curry.DebugModule.Prelude.Cons x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x2
                                  x10 <- do x8 <- do x6 <- Prelude.return x4
                                                     x7 <- Prelude.return x5
                                                     DM.constructorHook
                                                       (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                          (DI.DynamicInfo []
                                                             [DI.genTerm x6, DI.genTerm x7]))
                                                       (Prelude.return
                                                          (Curry.DebugModule.Prelude.Cons x6 x7))
                                            DM.funcCallHook "minlist"
                                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x8]))
                                              (strict_minlist x8)
                                  DM.funcCallHook "min"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
                                    (strict_min x9 x10)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Integer" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_7 x2)
                           x11)))
term_strict__case_7 x1
  = DI.Term "_case_7" (DI.SrcID "Integer" 0) x1
strict__case_10 x1
  = DM.eval
      (DM.funcDeclHook "_case_10"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x6 <- Prelude.return x1
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x6]))
               (case x6 of
                    Curry.DebugModule.Prelude.Cons x2 x3
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- Prelude.return x2
                                  x5 <- Prelude.return x3
                                  DM.funcCallHook "_case_9"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                    (strict__case_9 x4 x5)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Integer" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x6])))
                           strict__case_10
                           x6)))
term_strict__case_10 x1
  = DI.Term "_case_10" (DI.SrcID "Integer" 0) x1
strict__case_9 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_9"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
         (do x11 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" (- 1))
                  (DI.DynamicInfo [] [DI.genTerm x11]))
               (case x11 of
                    Curry.DebugModule.Prelude.Nil
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    Curry.DebugModule.Prelude.Cons x4 x5
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x2
                                  x10 <- do x8 <- do x6 <- Prelude.return x4
                                                     x7 <- Prelude.return x5
                                                     DM.constructorHook
                                                       (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                          (DI.DynamicInfo []
                                                             [DI.genTerm x6, DI.genTerm x7]))
                                                       (Prelude.return
                                                          (Curry.DebugModule.Prelude.Cons x6 x7))
                                            DM.funcCallHook "maxlist"
                                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x8]))
                                              (strict_maxlist x8)
                                  DM.funcCallHook "max"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x9, DI.genTerm x10]))
                                    (strict_max x9 x10)))
                    _
                      -> DM.treatCase
                           (DM.nepRulesHook
                              (DI.DebugInfo (DI.SrcID "Integer" (- 1))
                                 (DI.DynamicInfo [] [DI.genTerm x11])))
                           (strict__case_9 x2)
                           x11)))
term_strict__case_9 x1
  = DI.Term "_case_9" (DI.SrcID "Integer" 0) x1
strict__case_11 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_11"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x1))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           (strict__case_11 x1 x2)
                           x4)))
term_strict__case_11 x1
  = DI.Term "_case_11" (DI.SrcID "Integer" 0) x1
strict__case_12 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_12"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x4 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x1))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           (strict__case_12 x1 x2)
                           x4)))
term_strict__case_12 x1
  = DI.Term "_case_12" (DI.SrcID "Integer" 0) x1
strict__case_13 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_13"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x4 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x4]))
               (case x4 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x3 <- Prelude.return x1
                                  DM.funcCallHook "negate"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x3]))
                                    (Curry.DebugModule.Prelude.strict_negate x3)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x1))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x4])))
                           (strict__case_13 x1)
                           x4)))
term_strict__case_13 x1
  = DI.Term "_case_13" (DI.SrcID "Integer" 0) x1
strict__case_14 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_14"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x12 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x12]))
               (case x12 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.litHook
                                 (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return
                                    (Curry.DebugModule.Prelude.Pos Curry.DebugModule.Prelude.IHi))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x10 <- Prelude.return x2
                                  x11 <- do x8 <- do x4 <- Prelude.return x1
                                                     x5 <- DM.litHook
                                                             (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                                (DI.DynamicInfo [] []))
                                                             (Prelude.return
                                                                (Curry.DebugModule.Prelude.Pos
                                                                   Curry.DebugModule.Prelude.IHi))
                                                     DM.funcCallHook "-"
                                                       (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                          (DI.DynamicInfo []
                                                             [DI.genTerm x4, DI.genTerm x5]))
                                                       (Curry.DebugModule.Prelude.op_Minus x4 x5)
                                            x9 <- do x6 <- Prelude.return x2
                                                     x7 <- DM.litHook
                                                             (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                                (DI.DynamicInfo [] []))
                                                             (Prelude.return
                                                                (Curry.DebugModule.Prelude.Pos
                                                                   Curry.DebugModule.Prelude.IHi))
                                                     DM.funcCallHook "-"
                                                       (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                          (DI.DynamicInfo []
                                                             [DI.genTerm x6, DI.genTerm x7]))
                                                       (Curry.DebugModule.Prelude.op_Minus x6 x7)
                                            DM.funcCallHook "binomial.aux.41"
                                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                 (DI.DynamicInfo [] [DI.genTerm x8, DI.genTerm x9]))
                                              (x'xstrict_binomial46aux4641 x8 x9)
                                  DM.funcCallHook "*"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x10, DI.genTerm x11]))
                                    (Curry.DebugModule.Prelude.op_Asterisk x10 x11)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x12])))
                           (strict__case_14 x1 x2)
                           x12)))
term_strict__case_14 x1
  = DI.Term "_case_14" (DI.SrcID "Integer" 0) x1
strict__case_15 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_15"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x9 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x9]))
               (case x9 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x7 <- do x4 <- Prelude.return x2
                                           x5 <- Prelude.return x1
                                           DM.funcCallHook "binomial.aux.41"
                                             (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x4, DI.genTerm x5]))
                                             (x'xstrict_binomial46aux4641 x4 x5)
                                  x8 <- do x6 <- Prelude.return x2
                                           DM.funcCallHook "factorial"
                                             (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x6]))
                                             (strict_factorial x6)
                                  DM.funcCallHook "div"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x7, DI.genTerm x8]))
                                    (Curry.DebugModule.Prelude.strict_div x7 x8)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                 Curry.DebugModule.Prelude.strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x9])))
                           (strict__case_15 x1 x2)
                           x9)))
term_strict__case_15 x1
  = DI.Term "_case_15" (DI.SrcID "Integer" 0) x1
strict__case_17 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_17"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x7 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x7]))
               (case x7 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- Prelude.return x1
                                  x6 <- do x3 <- Prelude.return x1
                                           x4 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return Curry.DebugModule.Prelude.Zero)
                                           DM.funcCallHook "=="
                                             (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                                             (Curry.DebugModule.Prelude.op_EqEq x3 x4)
                                  DM.funcCallHook "_case_16"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                    (strict__case_16 x5 x6)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                 Curry.DebugModule.Prelude.strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x7])))
                           (strict__case_17 x1)
                           x7)))
term_strict__case_17 x1
  = DI.Term "_case_17" (DI.SrcID "Integer" 0) x1
strict__case_16 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_16"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x8 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.litHook
                                 (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return
                                    (Curry.DebugModule.Prelude.Pos Curry.DebugModule.Prelude.IHi))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  x7 <- do x5 <- do x3 <- Prelude.return x1
                                                    x4 <- DM.litHook
                                                            (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                               (DI.DynamicInfo [] []))
                                                            (Prelude.return
                                                               (Curry.DebugModule.Prelude.Pos
                                                                  Curry.DebugModule.Prelude.IHi))
                                                    DM.funcCallHook "-"
                                                      (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                         (DI.DynamicInfo []
                                                            [DI.genTerm x3, DI.genTerm x4]))
                                                      (Curry.DebugModule.Prelude.op_Minus x3 x4)
                                           DM.funcCallHook "factorial"
                                             (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5]))
                                             (strict_factorial x5)
                                  DM.funcCallHook "*"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (Curry.DebugModule.Prelude.op_Asterisk x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_16 x1)
                           x8)))
term_strict__case_16 x1
  = DI.Term "_case_16" (DI.SrcID "Integer" 0) x1
strict__case_19 x1 x2 x3 x5
  = DM.eval
      (DM.funcDeclHook "_case_19"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x5]))
         (do x19 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x19]))
               (case x19 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.letHook
                                 (DI.DebugInfo (DI.SrcID "DummyModule" 42) (DI.DynamicInfo [] []))
                                 (do x4 <- do x8 <- do x6 <- Prelude.return x3
                                                       x7 <- Prelude.return x2
                                                       DM.funcCallHook "+"
                                                         (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                            (DI.DynamicInfo []
                                                               [DI.genTerm x6, DI.genTerm x7]))
                                                         (Curry.DebugModule.Prelude.op_Plus x6 x7)
                                              x9 <- DM.litHook
                                                      (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                         (DI.DynamicInfo [] []))
                                                      (Prelude.return
                                                         (Curry.DebugModule.Prelude.Pos
                                                            (Curry.DebugModule.Prelude.O
                                                               Curry.DebugModule.Prelude.IHi)))
                                              DM.funcCallHook "div"
                                                (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                   (DI.DynamicInfo []
                                                      [DI.genTerm x8, DI.genTerm x9]))
                                                (Curry.DebugModule.Prelude.strict_div x8 x9)
                                     DM.eval
                                       (do x14 <- Prelude.return x1
                                           x15 <- Prelude.return x2
                                           x16 <- Prelude.return x3
                                           x17 <- Prelude.return x4
                                           x18 <- do x12 <- do x10 <- Prelude.return x4
                                                               x11 <- Prelude.return x4
                                                               DM.funcCallHook "*"
                                                                 (DI.DebugInfo
                                                                    (DI.SrcID "Integer" 0)
                                                                    (DI.DynamicInfo []
                                                                       [DI.genTerm x10,
                                                                        DI.genTerm x11]))
                                                                 (Curry.DebugModule.Prelude.op_Asterisk
                                                                    x10
                                                                    x11)
                                                     x13 <- Prelude.return x1
                                                     DM.funcCallHook ">"
                                                       (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                          (DI.DynamicInfo []
                                                             [DI.genTerm x12, DI.genTerm x13]))
                                                       (Curry.DebugModule.Prelude.op_Gt x12 x13)
                                           DM.funcCallHook "_case_18"
                                             (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                (DI.DynamicInfo []
                                                   [DI.genTerm x14, DI.genTerm x15, DI.genTerm x16,
                                                    DI.genTerm x17, DI.genTerm x18]))
                                             (strict__case_18 x14 x15 x16 x17 x18)))))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x19])))
                           (strict__case_19 x1 x2 x3)
                           x19)))
term_strict__case_19 x1
  = DI.Term "_case_19" (DI.SrcID "Integer" 0) x1
strict__case_18 x1 x2 x3 x4 x5
  = DM.eval
      (DM.funcDeclHook "_case_18"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4,
                DI.genTerm x5]))
         (do x12 <- Prelude.return x5
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x12]))
               (case x12 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- Prelude.return x1
                                  x7 <- Prelude.return x2
                                  x8 <- Prelude.return x4
                                  DM.funcCallHook "isqrt.aux.21"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x6, DI.genTerm x7, DI.genTerm x8]))
                                    (x'xstrict_isqrt46aux4621 x6 x7 x8)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x9 <- Prelude.return x1
                                  x10 <- Prelude.return x4
                                  x11 <- Prelude.return x3
                                  DM.funcCallHook "isqrt.aux.21"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x9, DI.genTerm x10, DI.genTerm x11]))
                                    (x'xstrict_isqrt46aux4621 x9 x10 x11)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x12])))
                           (strict__case_18 x1 x2 x3 x4)
                           x12)))
term_strict__case_18 x1
  = DI.Term "_case_18" (DI.SrcID "Integer" 0) x1
strict__case_22 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_22"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x7 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x7]))
               (case x7 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- Prelude.return x1
                                  x6 <- do x3 <- Prelude.return x1
                                           x4 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return Curry.DebugModule.Prelude.Zero)
                                           DM.funcCallHook "=="
                                             (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                                             (Curry.DebugModule.Prelude.op_EqEq x3 x4)
                                  DM.funcCallHook "_case_21"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                    (strict__case_21 x5 x6)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                 Curry.DebugModule.Prelude.strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x7])))
                           (strict__case_22 x1)
                           x7)))
term_strict__case_22 x1
  = DI.Term "_case_22" (DI.SrcID "Integer" 0) x1
strict__case_21 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_21"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x7 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x7]))
               (case x7 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.litHook
                                 (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Zero)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- Prelude.return x1
                                  x6 <- do x3 <- Prelude.return x1
                                           x4 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return
                                                      (Curry.DebugModule.Prelude.Pos
                                                         (Curry.DebugModule.Prelude.O
                                                            (Curry.DebugModule.Prelude.O
                                                               Curry.DebugModule.Prelude.IHi))))
                                           DM.funcCallHook "<"
                                             (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                                             (Curry.DebugModule.Prelude.op_Lt x3 x4)
                                  DM.funcCallHook "_case_20"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                    (strict__case_20 x5 x6)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x7])))
                           (strict__case_21 x1)
                           x7)))
term_strict__case_21 x1
  = DI.Term "_case_21" (DI.SrcID "Integer" 0) x1
strict__case_20 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_20"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x6 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x6]))
               (case x6 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.litHook
                                 (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return
                                    (Curry.DebugModule.Prelude.Pos Curry.DebugModule.Prelude.IHi))))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x3 <- Prelude.return x1
                                  x4 <- DM.litHook
                                          (DI.DebugInfo (DI.SrcID "Integer" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return
                                             (Curry.DebugModule.Prelude.Pos
                                                (Curry.DebugModule.Prelude.O
                                                   Curry.DebugModule.Prelude.IHi)))
                                  x5 <- Prelude.return x1
                                  DM.funcCallHook "isqrt.aux.21"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x3, DI.genTerm x4, DI.genTerm x5]))
                                    (x'xstrict_isqrt46aux4621 x3 x4 x5)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x6])))
                           (strict__case_20 x1)
                           x6)))
term_strict__case_20 x1
  = DI.Term "_case_20" (DI.SrcID "Integer" 0) x1
strict__case_24 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_24"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x7 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x7]))
               (case x7 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x5 <- Prelude.return x1
                                  x6 <- do x3 <- Prelude.return x1
                                           x4 <- DM.litHook
                                                   (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                      (DI.DynamicInfo [] []))
                                                   (Prelude.return
                                                      (Curry.DebugModule.Prelude.Pos
                                                         (Curry.DebugModule.Prelude.O
                                                            (Curry.DebugModule.Prelude.I
                                                               (Curry.DebugModule.Prelude.O
                                                                  Curry.DebugModule.Prelude.IHi)))))
                                           DM.funcCallHook "<"
                                             (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                                             (Curry.DebugModule.Prelude.op_Lt x3 x4)
                                  DM.funcCallHook "_case_23"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
                                    (strict__case_23 x5 x6)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                 Curry.DebugModule.Prelude.strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x7])))
                           (strict__case_24 x1)
                           x7)))
term_strict__case_24 x1
  = DI.Term "_case_24" (DI.SrcID "Integer" 0) x1
strict__case_23 x1 x2
  = DM.eval
      (DM.funcDeclHook "_case_23"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x8 <- Prelude.return x2
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x8]))
               (case x8 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.litHook
                                 (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return Curry.DebugModule.Prelude.Zero)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x6 <- DM.litHook
                                          (DI.DebugInfo (DI.SrcID "Integer" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return
                                             (Curry.DebugModule.Prelude.Pos
                                                Curry.DebugModule.Prelude.IHi))
                                  x7 <- do x5 <- do x3 <- Prelude.return x1
                                                    x4 <- DM.litHook
                                                            (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                               (DI.DynamicInfo [] []))
                                                            (Prelude.return
                                                               (Curry.DebugModule.Prelude.Pos
                                                                  (Curry.DebugModule.Prelude.O
                                                                     (Curry.DebugModule.Prelude.I
                                                                        (Curry.DebugModule.Prelude.O
                                                                           Curry.DebugModule.Prelude.IHi)))))
                                                    DM.funcCallHook "div"
                                                      (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                         (DI.DynamicInfo []
                                                            [DI.genTerm x3, DI.genTerm x4]))
                                                      (Curry.DebugModule.Prelude.strict_div x3 x4)
                                           DM.funcCallHook "ilog"
                                             (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                (DI.DynamicInfo [] [DI.genTerm x5]))
                                             (strict_ilog x5)
                                  DM.funcCallHook "+"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo [] [DI.genTerm x6, DI.genTerm x7]))
                                    (Curry.DebugModule.Prelude.op_Plus x6 x7)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x8])))
                           (strict__case_23 x1)
                           x8)))
term_strict__case_23 x1
  = DI.Term "_case_23" (DI.SrcID "Integer" 0) x1
strict__case_26 x1 x2 x3 x4
  = DM.eval
      (DM.funcDeclHook "_case_26"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo []
               [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3, DI.genTerm x4]))
         (do x21 <- Prelude.return x4
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x21]))
               (case x21 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x1))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x18 <- do x12 <- Prelude.return x1
                                            x13 <- do x9 <- Prelude.return x2
                                                      x10 <- Prelude.return x3
                                                      x11 <- do x7 <- do x5 <- Prelude.return x3
                                                                         x6 <- DM.litHook
                                                                                 (DI.DebugInfo
                                                                                    (DI.SrcID
                                                                                       "Integer"
                                                                                       0)
                                                                                    (DI.DynamicInfo
                                                                                       []
                                                                                       []))
                                                                                 (Prelude.return
                                                                                    (Curry.DebugModule.Prelude.Pos
                                                                                       (Curry.DebugModule.Prelude.O
                                                                                          Curry.DebugModule.Prelude.IHi)))
                                                                         DM.funcCallHook "mod"
                                                                           (DI.DebugInfo
                                                                              (DI.SrcID "Integer" 0)
                                                                              (DI.DynamicInfo []
                                                                                 [DI.genTerm x5,
                                                                                  DI.genTerm x6]))
                                                                           (Curry.DebugModule.Prelude.strict_mod
                                                                              x5
                                                                              x6)
                                                                x8 <- DM.litHook
                                                                        (DI.DebugInfo
                                                                           (DI.SrcID "Integer" 0)
                                                                           (DI.DynamicInfo [] []))
                                                                        (Prelude.return
                                                                           (Curry.DebugModule.Prelude.Pos
                                                                              Curry.DebugModule.Prelude.IHi))
                                                                DM.funcCallHook "=="
                                                                  (DI.DebugInfo
                                                                     (DI.SrcID "Integer" 0)
                                                                     (DI.DynamicInfo []
                                                                        [DI.genTerm x7,
                                                                         DI.genTerm x8]))
                                                                  (Curry.DebugModule.Prelude.op_EqEq
                                                                     x7
                                                                     x8)
                                                      DM.funcCallHook "_case_25"
                                                        (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                           (DI.DynamicInfo []
                                                              [DI.genTerm x9, DI.genTerm x10,
                                                               DI.genTerm x11]))
                                                        (strict__case_25 x9 x10 x11)
                                            DM.funcCallHook "*"
                                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x12, DI.genTerm x13]))
                                              (Curry.DebugModule.Prelude.op_Asterisk x12 x13)
                                  x19 <- do x14 <- Prelude.return x2
                                            x15 <- Prelude.return x2
                                            DM.funcCallHook "*"
                                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x14, DI.genTerm x15]))
                                              (Curry.DebugModule.Prelude.op_Asterisk x14 x15)
                                  x20 <- do x16 <- Prelude.return x3
                                            x17 <- DM.litHook
                                                     (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                        (DI.DynamicInfo [] []))
                                                     (Prelude.return
                                                        (Curry.DebugModule.Prelude.Pos
                                                           (Curry.DebugModule.Prelude.O
                                                              Curry.DebugModule.Prelude.IHi)))
                                            DM.funcCallHook "div"
                                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                                 (DI.DynamicInfo []
                                                    [DI.genTerm x16, DI.genTerm x17]))
                                              (Curry.DebugModule.Prelude.strict_div x16 x17)
                                  DM.funcCallHook "pow.powaux.3"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x18, DI.genTerm x19, DI.genTerm x20]))
                                    (x'xstrict_pow46powaux463 x18 x19 x20)))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x21])))
                           (strict__case_26 x1 x2 x3)
                           x21)))
term_strict__case_26 x1
  = DI.Term "_case_26" (DI.SrcID "Integer" 0) x1
strict__case_25 x2 x3 x4
  = DM.eval
      (DM.funcDeclHook "_case_25"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3, DI.genTerm x4]))
         (do x5 <- Prelude.return x4
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5]))
               (case x5 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval (Prelude.return x2))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.litHook
                                 (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                 (Prelude.return
                                    (Curry.DebugModule.Prelude.Pos Curry.DebugModule.Prelude.IHi))))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x5])))
                           (strict__case_25 x2 x3)
                           x5)))
term_strict__case_25 x1
  = DI.Term "_case_25" (DI.SrcID "Integer" 0) x1
strict__case_27 x1 x2 x3
  = DM.eval
      (DM.funcDeclHook "_case_27"
         (DI.DebugInfo (DI.SrcID "Integer" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2, DI.genTerm x3]))
         (do x7 <- Prelude.return x3
             DM.caseHook
               (DI.DebugInfo (DI.SrcID "Integer" 0)
                  (DI.DynamicInfo [] [DI.genTerm x7]))
               (case x7 of
                    Curry.DebugModule.Prelude.True
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (do x4 <- DM.litHook
                                          (DI.DebugInfo (DI.SrcID "Integer" 0)
                                             (DI.DynamicInfo [] []))
                                          (Prelude.return
                                             (Curry.DebugModule.Prelude.Pos
                                                Curry.DebugModule.Prelude.IHi))
                                  x5 <- Prelude.return x1
                                  x6 <- Prelude.return x2
                                  DM.funcCallHook "pow.powaux.3"
                                    (DI.DebugInfo (DI.SrcID "Integer" 0)
                                       (DI.DynamicInfo []
                                          [DI.genTerm x4, DI.genTerm x5, DI.genTerm x6]))
                                    (x'xstrict_pow46powaux463 x4 x5 x6)))
                    Curry.DebugModule.Prelude.False
                      -> DM.branchHook
                           (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                           (DM.eval
                              (DM.funcCallHook "failed"
                                 (DI.DebugInfo (DI.SrcID "Integer" 0) (DI.DynamicInfo [] []))
                                 Curry.DebugModule.Prelude.strict_failed))
                    _
                      -> DM.treatCase
                           (DM.nepCaseHook
                              (DI.DebugInfo (DI.SrcID "Integer" 0)
                                 (DI.DynamicInfo [] [DI.genTerm x7])))
                           (strict__case_27 x1 x2)
                           x7)))
term_strict__case_27 x1
  = DI.Term "_case_27" (DI.SrcID "Integer" 0) x1