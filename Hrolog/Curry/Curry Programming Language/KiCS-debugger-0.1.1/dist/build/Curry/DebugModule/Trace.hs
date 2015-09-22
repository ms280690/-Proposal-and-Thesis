{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Curry.DebugModule.Trace where
import qualified Prelude
import qualified Curry.Debugger.DebugMonad as DM
import qualified Curry.Debugger.DebugInfo as DI
import qualified Curry.Debugger.PartCalls as PC
import qualified Data.Generics
import qualified Curry.DebugModule.Prelude
 
strict_trace :: (DM.DM dm, DI.GenTerm a) => a -> dm a
strict_trace x1
  = DM.eval
      (DM.funcDeclHook "trace"
         (DI.DebugInfo (DI.SrcID "Trace" 0)
            (DI.DynamicInfo [] [DI.genTerm x1]))
         (Prelude.return x1))
term_strict_trace x1 = DI.Term "trace" (DI.SrcID "Trace" 0) x1