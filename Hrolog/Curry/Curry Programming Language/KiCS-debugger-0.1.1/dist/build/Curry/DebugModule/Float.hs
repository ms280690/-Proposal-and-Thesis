{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Curry.DebugModule.Float where
import qualified Prelude
import qualified Curry.Debugger.DebugMonad as DM
import qualified Curry.Debugger.DebugInfo as DI
import qualified Curry.Debugger.PartCalls as PC
import qualified Data.Generics
import qualified Curry.DebugModule.Prelude
 
strict_prim_Float_plus ::
                       (DM.DM dm) =>
                         Curry.DebugModule.Prelude.Float ->
                           Curry.DebugModule.Prelude.Float ->
                             dm Curry.DebugModule.Prelude.Float
strict_prim_Float_plus x0 x1
  = hook_strict_prim_Float_plus x0 x1
      (Prelude.error "not implemented")
 
strict_prim_Float_minus ::
                        (DM.DM dm) =>
                          Curry.DebugModule.Prelude.Float ->
                            Curry.DebugModule.Prelude.Float ->
                              dm Curry.DebugModule.Prelude.Float
strict_prim_Float_minus x0 x1
  = hook_strict_prim_Float_minus x0 x1
      (Prelude.error "not implemented")
 
strict_prim_Float_times ::
                        (DM.DM dm) =>
                          Curry.DebugModule.Prelude.Float ->
                            Curry.DebugModule.Prelude.Float ->
                              dm Curry.DebugModule.Prelude.Float
strict_prim_Float_times x0 x1
  = hook_strict_prim_Float_times x0 x1
      (Prelude.error "not implemented")
 
strict_prim_Float_divide ::
                         (DM.DM dm) =>
                           Curry.DebugModule.Prelude.Float ->
                             Curry.DebugModule.Prelude.Float ->
                               dm Curry.DebugModule.Prelude.Float
strict_prim_Float_divide x0 x1
  = hook_strict_prim_Float_divide x0 x1
      (Prelude.error "not implemented")
 
strict_prim_Float_lt ::
                     (DM.DM dm) =>
                       Curry.DebugModule.Prelude.Float ->
                         Curry.DebugModule.Prelude.Float ->
                           dm Curry.DebugModule.Prelude.Bool
strict_prim_Float_lt x0 x1
  = hook_strict_prim_Float_lt x0 x1 (Prelude.error "not implemented")
 
strict_prim_Float_gt ::
                     (DM.DM dm) =>
                       Curry.DebugModule.Prelude.Float ->
                         Curry.DebugModule.Prelude.Float ->
                           dm Curry.DebugModule.Prelude.Bool
strict_prim_Float_gt x0 x1
  = hook_strict_prim_Float_gt x0 x1 (Prelude.error "not implemented")
 
strict_prim_Float_leq ::
                      (DM.DM dm) =>
                        Curry.DebugModule.Prelude.Float ->
                          Curry.DebugModule.Prelude.Float ->
                            dm Curry.DebugModule.Prelude.Bool
strict_prim_Float_leq x0 x1
  = hook_strict_prim_Float_leq x0 x1
      (Prelude.error "not implemented")
 
strict_prim_Float_geq ::
                      (DM.DM dm) =>
                        Curry.DebugModule.Prelude.Float ->
                          Curry.DebugModule.Prelude.Float ->
                            dm Curry.DebugModule.Prelude.Bool
strict_prim_Float_geq x0 x1
  = hook_strict_prim_Float_geq x0 x1
      (Prelude.error "not implemented")
 
strict_prim_i2f ::
                (DM.DM dm) =>
                  Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Float
strict_prim_i2f x0
  = hook_strict_prim_i2f x0 (Prelude.error "not implemented")
 
strict_prim_truncate ::
                     (DM.DM dm) =>
                       Curry.DebugModule.Prelude.Float -> dm Curry.DebugModule.Prelude.Int
strict_prim_truncate x0
  = hook_strict_prim_truncate x0 (Prelude.error "not implemented")
 
strict_prim_round ::
                  (DM.DM dm) =>
                    Curry.DebugModule.Prelude.Float -> dm Curry.DebugModule.Prelude.Int
strict_prim_round x0
  = hook_strict_prim_round x0 (Prelude.error "not implemented")
 
strict_prim_sqrt ::
                 (DM.DM dm) =>
                   Curry.DebugModule.Prelude.Float ->
                     dm Curry.DebugModule.Prelude.Float
strict_prim_sqrt x0
  = hook_strict_prim_sqrt x0 (Prelude.error "not implemented")
 
strict_prim_log ::
                (DM.DM dm) =>
                  Curry.DebugModule.Prelude.Float ->
                    dm Curry.DebugModule.Prelude.Float
strict_prim_log x0
  = hook_strict_prim_log x0 (Prelude.error "not implemented")
 
strict_prim_exp ::
                (DM.DM dm) =>
                  Curry.DebugModule.Prelude.Float ->
                    dm Curry.DebugModule.Prelude.Float
strict_prim_exp x0
  = hook_strict_prim_exp x0 (Prelude.error "not implemented")
 
strict_prim_sin ::
                (DM.DM dm) =>
                  Curry.DebugModule.Prelude.Float ->
                    dm Curry.DebugModule.Prelude.Float
strict_prim_sin x0
  = hook_strict_prim_sin x0 (Prelude.error "not implemented")
 
strict_prim_cos ::
                (DM.DM dm) =>
                  Curry.DebugModule.Prelude.Float ->
                    dm Curry.DebugModule.Prelude.Float
strict_prim_cos x0
  = hook_strict_prim_cos x0 (Prelude.error "not implemented")
 
strict_prim_tan ::
                (DM.DM dm) =>
                  Curry.DebugModule.Prelude.Float ->
                    dm Curry.DebugModule.Prelude.Float
strict_prim_tan x0
  = hook_strict_prim_tan x0 (Prelude.error "not implemented")
 
strict_prim_atan ::
                 (DM.DM dm) =>
                   Curry.DebugModule.Prelude.Float ->
                     dm Curry.DebugModule.Prelude.Float
strict_prim_atan x0
  = hook_strict_prim_atan x0 (Prelude.error "not implemented")
 
op_PlusPoint ::
             (DM.DM dm) =>
               Curry.DebugModule.Prelude.Float ->
                 Curry.DebugModule.Prelude.Float ->
                   dm Curry.DebugModule.Prelude.Float
op_PlusPoint x1 x2
  = DM.eval
      (DM.funcDeclHook "+."
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return
                              (PC.partCall2 (term_strict_prim_Float_plus [])
                                 strict_prim_Float_plus)
                      x4 <- Prelude.return x1
                      DM.funcCallHook "$#"
                        (DI.DebugInfo (DI.SrcID "Float" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_DollarRhomb x3 x4)
             x6 <- Prelude.return x2
             DM.funcCallHook "$#"
               (DI.DebugInfo (DI.SrcID "Float" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (Curry.DebugModule.Prelude.op_DollarRhomb x5 x6)))
term_op_PlusPoint x1 = DI.Term "+." (DI.SrcID "Float" 0) x1
 
op_MinusPoint ::
              (DM.DM dm) =>
                Curry.DebugModule.Prelude.Float ->
                  Curry.DebugModule.Prelude.Float ->
                    dm Curry.DebugModule.Prelude.Float
op_MinusPoint x1 x2
  = DM.eval
      (DM.funcDeclHook "-."
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return
                              (PC.partCall2 (term_strict_prim_Float_minus [])
                                 strict_prim_Float_minus)
                      x4 <- Prelude.return x1
                      DM.funcCallHook "$#"
                        (DI.DebugInfo (DI.SrcID "Float" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_DollarRhomb x3 x4)
             x6 <- Prelude.return x2
             DM.funcCallHook "$#"
               (DI.DebugInfo (DI.SrcID "Float" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (Curry.DebugModule.Prelude.op_DollarRhomb x5 x6)))
term_op_MinusPoint x1 = DI.Term "-." (DI.SrcID "Float" 0) x1
 
op_AsteriskPoint ::
                 (DM.DM dm) =>
                   Curry.DebugModule.Prelude.Float ->
                     Curry.DebugModule.Prelude.Float ->
                       dm Curry.DebugModule.Prelude.Float
op_AsteriskPoint x1 x2
  = DM.eval
      (DM.funcDeclHook "*."
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return
                              (PC.partCall2 (term_strict_prim_Float_times [])
                                 strict_prim_Float_times)
                      x4 <- Prelude.return x1
                      DM.funcCallHook "$#"
                        (DI.DebugInfo (DI.SrcID "Float" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_DollarRhomb x3 x4)
             x6 <- Prelude.return x2
             DM.funcCallHook "$#"
               (DI.DebugInfo (DI.SrcID "Float" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (Curry.DebugModule.Prelude.op_DollarRhomb x5 x6)))
term_op_AsteriskPoint x1 = DI.Term "*." (DI.SrcID "Float" 0) x1
 
op_SlashPoint ::
              (DM.DM dm) =>
                Curry.DebugModule.Prelude.Float ->
                  Curry.DebugModule.Prelude.Float ->
                    dm Curry.DebugModule.Prelude.Float
op_SlashPoint x1 x2
  = DM.eval
      (DM.funcDeclHook "/."
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return
                              (PC.partCall2 (term_strict_prim_Float_divide [])
                                 strict_prim_Float_divide)
                      x4 <- Prelude.return x1
                      DM.funcCallHook "$#"
                        (DI.DebugInfo (DI.SrcID "Float" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_DollarRhomb x3 x4)
             x6 <- Prelude.return x2
             DM.funcCallHook "$#"
               (DI.DebugInfo (DI.SrcID "Float" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (Curry.DebugModule.Prelude.op_DollarRhomb x5 x6)))
term_op_SlashPoint x1 = DI.Term "/." (DI.SrcID "Float" 0) x1
 
op_LtPoint ::
           (DM.DM dm) =>
             Curry.DebugModule.Prelude.Float ->
               Curry.DebugModule.Prelude.Float ->
                 dm Curry.DebugModule.Prelude.Bool
op_LtPoint x1 x2
  = DM.eval
      (DM.funcDeclHook "<."
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return
                              (PC.partCall2 (term_strict_prim_Float_lt []) strict_prim_Float_lt)
                      x4 <- Prelude.return x1
                      DM.funcCallHook "$#"
                        (DI.DebugInfo (DI.SrcID "Float" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_DollarRhomb x3 x4)
             x6 <- Prelude.return x2
             DM.funcCallHook "$#"
               (DI.DebugInfo (DI.SrcID "Float" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (Curry.DebugModule.Prelude.op_DollarRhomb x5 x6)))
term_op_LtPoint x1 = DI.Term "<." (DI.SrcID "Float" 0) x1
 
op_GtPoint ::
           (DM.DM dm) =>
             Curry.DebugModule.Prelude.Float ->
               Curry.DebugModule.Prelude.Float ->
                 dm Curry.DebugModule.Prelude.Bool
op_GtPoint x1 x2
  = DM.eval
      (DM.funcDeclHook ">."
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return
                              (PC.partCall2 (term_strict_prim_Float_gt []) strict_prim_Float_gt)
                      x4 <- Prelude.return x1
                      DM.funcCallHook "$#"
                        (DI.DebugInfo (DI.SrcID "Float" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_DollarRhomb x3 x4)
             x6 <- Prelude.return x2
             DM.funcCallHook "$#"
               (DI.DebugInfo (DI.SrcID "Float" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (Curry.DebugModule.Prelude.op_DollarRhomb x5 x6)))
term_op_GtPoint x1 = DI.Term ">." (DI.SrcID "Float" 0) x1
 
op_LtEqPoint ::
             (DM.DM dm) =>
               Curry.DebugModule.Prelude.Float ->
                 Curry.DebugModule.Prelude.Float ->
                   dm Curry.DebugModule.Prelude.Bool
op_LtEqPoint x1 x2
  = DM.eval
      (DM.funcDeclHook "<=."
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return
                              (PC.partCall2 (term_strict_prim_Float_leq [])
                                 strict_prim_Float_leq)
                      x4 <- Prelude.return x1
                      DM.funcCallHook "$#"
                        (DI.DebugInfo (DI.SrcID "Float" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_DollarRhomb x3 x4)
             x6 <- Prelude.return x2
             DM.funcCallHook "$#"
               (DI.DebugInfo (DI.SrcID "Float" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (Curry.DebugModule.Prelude.op_DollarRhomb x5 x6)))
term_op_LtEqPoint x1 = DI.Term "<=." (DI.SrcID "Float" 0) x1
 
op_GtEqPoint ::
             (DM.DM dm) =>
               Curry.DebugModule.Prelude.Float ->
                 Curry.DebugModule.Prelude.Float ->
                   dm Curry.DebugModule.Prelude.Bool
op_GtEqPoint x1 x2
  = DM.eval
      (DM.funcDeclHook ">=."
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         (do x5 <- do x3 <- Prelude.return
                              (PC.partCall2 (term_strict_prim_Float_geq [])
                                 strict_prim_Float_geq)
                      x4 <- Prelude.return x1
                      DM.funcCallHook "$#"
                        (DI.DebugInfo (DI.SrcID "Float" 0)
                           (DI.DynamicInfo [] [DI.genTerm x3, DI.genTerm x4]))
                        (Curry.DebugModule.Prelude.op_DollarRhomb x3 x4)
             x6 <- Prelude.return x2
             DM.funcCallHook "$#"
               (DI.DebugInfo (DI.SrcID "Float" 0)
                  (DI.DynamicInfo [] [DI.genTerm x5, DI.genTerm x6]))
               (Curry.DebugModule.Prelude.op_DollarRhomb x5 x6)))
term_op_GtEqPoint x1 = DI.Term ">=." (DI.SrcID "Float" 0) x1
 
strict_i2f ::
           (DM.DM dm) =>
             Curry.DebugModule.Prelude.Int -> dm Curry.DebugModule.Prelude.Float
strict_i2f x1
  = DM.eval
      (DM.funcDeclHook "i2f"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_i2f []) strict_prim_i2f)
             x3 <- Prelude.return x1
             DM.funcCallHook "$#"
               (DI.DebugInfo (DI.SrcID "Float" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhomb x2 x3)))
term_strict_i2f x1 = DI.Term "i2f" (DI.SrcID "Float" 0) x1
 
strict_truncate ::
                (DM.DM dm) =>
                  Curry.DebugModule.Prelude.Float -> dm Curry.DebugModule.Prelude.Int
strict_truncate x1
  = DM.eval
      (DM.funcDeclHook "truncate"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_truncate []) strict_prim_truncate)
             x3 <- Prelude.return x1
             DM.funcCallHook "$#"
               (DI.DebugInfo (DI.SrcID "Float" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhomb x2 x3)))
term_strict_truncate x1
  = DI.Term "truncate" (DI.SrcID "Float" 0) x1
 
strict_round ::
             (DM.DM dm) =>
               Curry.DebugModule.Prelude.Float -> dm Curry.DebugModule.Prelude.Int
strict_round x1
  = DM.eval
      (DM.funcDeclHook "round"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_round []) strict_prim_round)
             x3 <- Prelude.return x1
             DM.funcCallHook "$#"
               (DI.DebugInfo (DI.SrcID "Float" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhomb x2 x3)))
term_strict_round x1 = DI.Term "round" (DI.SrcID "Float" 0) x1
 
strict_sqrt ::
            (DM.DM dm) =>
              Curry.DebugModule.Prelude.Float ->
                dm Curry.DebugModule.Prelude.Float
strict_sqrt x1
  = DM.eval
      (DM.funcDeclHook "sqrt"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_sqrt []) strict_prim_sqrt)
             x3 <- Prelude.return x1
             DM.funcCallHook "$#"
               (DI.DebugInfo (DI.SrcID "Float" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhomb x2 x3)))
term_strict_sqrt x1 = DI.Term "sqrt" (DI.SrcID "Float" 0) x1
 
strict_log ::
           (DM.DM dm) =>
             Curry.DebugModule.Prelude.Float ->
               dm Curry.DebugModule.Prelude.Float
strict_log x1
  = DM.eval
      (DM.funcDeclHook "log"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_log []) strict_prim_log)
             x3 <- Prelude.return x1
             DM.funcCallHook "$#"
               (DI.DebugInfo (DI.SrcID "Float" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhomb x2 x3)))
term_strict_log x1 = DI.Term "log" (DI.SrcID "Float" 0) x1
 
strict_exp ::
           (DM.DM dm) =>
             Curry.DebugModule.Prelude.Float ->
               dm Curry.DebugModule.Prelude.Float
strict_exp x1
  = DM.eval
      (DM.funcDeclHook "exp"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_exp []) strict_prim_exp)
             x3 <- Prelude.return x1
             DM.funcCallHook "$#"
               (DI.DebugInfo (DI.SrcID "Float" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhomb x2 x3)))
term_strict_exp x1 = DI.Term "exp" (DI.SrcID "Float" 0) x1
 
strict_sin ::
           (DM.DM dm) =>
             Curry.DebugModule.Prelude.Float ->
               dm Curry.DebugModule.Prelude.Float
strict_sin x1
  = DM.eval
      (DM.funcDeclHook "sin"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_sin []) strict_prim_sin)
             x3 <- Prelude.return x1
             DM.funcCallHook "$#"
               (DI.DebugInfo (DI.SrcID "Float" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhomb x2 x3)))
term_strict_sin x1 = DI.Term "sin" (DI.SrcID "Float" 0) x1
 
strict_cos ::
           (DM.DM dm) =>
             Curry.DebugModule.Prelude.Float ->
               dm Curry.DebugModule.Prelude.Float
strict_cos x1
  = DM.eval
      (DM.funcDeclHook "cos"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_cos []) strict_prim_cos)
             x3 <- Prelude.return x1
             DM.funcCallHook "$#"
               (DI.DebugInfo (DI.SrcID "Float" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhomb x2 x3)))
term_strict_cos x1 = DI.Term "cos" (DI.SrcID "Float" 0) x1
 
strict_tan ::
           (DM.DM dm) =>
             Curry.DebugModule.Prelude.Float ->
               dm Curry.DebugModule.Prelude.Float
strict_tan x1
  = DM.eval
      (DM.funcDeclHook "tan"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_tan []) strict_prim_tan)
             x3 <- Prelude.return x1
             DM.funcCallHook "$#"
               (DI.DebugInfo (DI.SrcID "Float" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhomb x2 x3)))
term_strict_tan x1 = DI.Term "tan" (DI.SrcID "Float" 0) x1
 
strict_atan ::
            (DM.DM dm) =>
              Curry.DebugModule.Prelude.Float ->
                dm Curry.DebugModule.Prelude.Float
strict_atan x1
  = DM.eval
      (DM.funcDeclHook "atan"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (do x2 <- Prelude.return
                     (PC.partCall1 (term_strict_prim_atan []) strict_prim_atan)
             x3 <- Prelude.return x1
             DM.funcCallHook "$#"
               (DI.DebugInfo (DI.SrcID "Float" 0)
                  (DI.DynamicInfo [] [DI.genTerm x2, DI.genTerm x3]))
               (Curry.DebugModule.Prelude.op_DollarRhomb x2 x3)))
term_strict_atan x1 = DI.Term "atan" (DI.SrcID "Float" 0) x1
hook_strict_prim_Float_plus x1 x2 value
  = DM.eval
      (DM.funcDeclHook "prim_Float_plus"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_strict_prim_Float_plus x1
  = DI.Term "prim_Float_plus" (DI.SrcID "Float" 0) x1
hook_strict_prim_Float_minus x1 x2 value
  = DM.eval
      (DM.funcDeclHook "prim_Float_minus"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_strict_prim_Float_minus x1
  = DI.Term "prim_Float_minus" (DI.SrcID "Float" 0) x1
hook_strict_prim_Float_times x1 x2 value
  = DM.eval
      (DM.funcDeclHook "prim_Float_times"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_strict_prim_Float_times x1
  = DI.Term "prim_Float_times" (DI.SrcID "Float" 0) x1
hook_strict_prim_Float_divide x1 x2 value
  = DM.eval
      (DM.funcDeclHook "prim_Float_divide"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_strict_prim_Float_divide x1
  = DI.Term "prim_Float_divide" (DI.SrcID "Float" 0) x1
hook_strict_prim_Float_lt x1 x2 value
  = DM.eval
      (DM.funcDeclHook "prim_Float_lt"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_strict_prim_Float_lt x1
  = DI.Term "prim_Float_lt" (DI.SrcID "Float" 0) x1
hook_strict_prim_Float_gt x1 x2 value
  = DM.eval
      (DM.funcDeclHook "prim_Float_gt"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_strict_prim_Float_gt x1
  = DI.Term "prim_Float_gt" (DI.SrcID "Float" 0) x1
hook_strict_prim_Float_leq x1 x2 value
  = DM.eval
      (DM.funcDeclHook "prim_Float_leq"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_strict_prim_Float_leq x1
  = DI.Term "prim_Float_leq" (DI.SrcID "Float" 0) x1
hook_strict_prim_Float_geq x1 x2 value
  = DM.eval
      (DM.funcDeclHook "prim_Float_geq"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1, DI.genTerm x2]))
         value)
term_strict_prim_Float_geq x1
  = DI.Term "prim_Float_geq" (DI.SrcID "Float" 0) x1
hook_strict_prim_i2f x1 value
  = DM.eval
      (DM.funcDeclHook "prim_i2f"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_i2f x1
  = DI.Term "prim_i2f" (DI.SrcID "Float" 0) x1
hook_strict_prim_truncate x1 value
  = DM.eval
      (DM.funcDeclHook "prim_truncate"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_truncate x1
  = DI.Term "prim_truncate" (DI.SrcID "Float" 0) x1
hook_strict_prim_round x1 value
  = DM.eval
      (DM.funcDeclHook "prim_round"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_round x1
  = DI.Term "prim_round" (DI.SrcID "Float" 0) x1
hook_strict_prim_sqrt x1 value
  = DM.eval
      (DM.funcDeclHook "prim_sqrt"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_sqrt x1
  = DI.Term "prim_sqrt" (DI.SrcID "Float" 0) x1
hook_strict_prim_log x1 value
  = DM.eval
      (DM.funcDeclHook "prim_log"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_log x1
  = DI.Term "prim_log" (DI.SrcID "Float" 0) x1
hook_strict_prim_exp x1 value
  = DM.eval
      (DM.funcDeclHook "prim_exp"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_exp x1
  = DI.Term "prim_exp" (DI.SrcID "Float" 0) x1
hook_strict_prim_sin x1 value
  = DM.eval
      (DM.funcDeclHook "prim_sin"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_sin x1
  = DI.Term "prim_sin" (DI.SrcID "Float" 0) x1
hook_strict_prim_cos x1 value
  = DM.eval
      (DM.funcDeclHook "prim_cos"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_cos x1
  = DI.Term "prim_cos" (DI.SrcID "Float" 0) x1
hook_strict_prim_tan x1 value
  = DM.eval
      (DM.funcDeclHook "prim_tan"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_tan x1
  = DI.Term "prim_tan" (DI.SrcID "Float" 0) x1
hook_strict_prim_atan x1 value
  = DM.eval
      (DM.funcDeclHook "prim_atan"
         (DI.DebugInfo (DI.SrcID "Float" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         value)
term_strict_prim_atan x1
  = DI.Term "prim_atan" (DI.SrcID "Float" 0) x1