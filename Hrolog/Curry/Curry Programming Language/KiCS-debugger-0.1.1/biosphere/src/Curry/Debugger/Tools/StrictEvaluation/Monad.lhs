> {-# OPTIONS_GHC -fglasgow-exts #-}
> module Curry.Debugger.Tools.StrictEvaluation.Monad where

> import Control.Monad
> import Control.Monad.Trans
> import Curry.Debugger.DebugMonad
> import Curry.Debugger.ShowTerm

> instance Monad m => DM (DMT a m)

> run exp fname = do
>            putStrLn "starting strict evaluation"
>            debugDMT (printOracle >> exp) fname () >>= printTerm
>            putStrLn "\nstrict evaluation finished"


> printOracle = getOracle >>= liftIO . print
