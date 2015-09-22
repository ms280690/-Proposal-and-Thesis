> {-# LANGUAGE FlexibleInstances #-}

> module Curry.Debugger.Tools.Cycle.Monad where

> import Curry.Debugger.DebugMonad
> import Curry.Debugger.ShowTerm
> import Control.Monad.State

Container for the calls (Function name + Argument terms):

> data TraceElement = TraceElement String [Term]

Call is equal to another, if function names and arguments are identical:

> instance Eq TraceElement where
>   (TraceElement s ts) == (TraceElement s' ts') = s == s' && ts == ts'

> instance MonadIO m => DM (DMT [TraceElement] m) where
             
>   funcCallHook name di call = do
>     let args = currentValues (dynamicInfo di)
>     trace <- getToolState
>     let traceElt = TraceElement name args
>     putToolState (traceElt : trace)
>     (when (elem traceElt trace)
>           (fail $ showCycle name args))
>     (liftIO $ putStrLn (name ++ " " ++ (show $ length trace)))
>     result <- call
>     putToolState trace
>     return result

> showCycle :: String -> [Term] -> String
> showCycle func args =
>   "Cycle: " ++ func ++ " " ++ showedArgs
>     where
>       showedArgs = concatMap (\ t -> showTerm t ++ " ") args

> run :: GenTerm a => DMT [TraceElement] IO a -> String -> IO a
> run exp steps = debugDMT exp steps []

