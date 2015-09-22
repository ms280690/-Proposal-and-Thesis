> {-# LANGUAGE FlexibleInstances #-}

> module Curry.Debugger.Tools.Debug.Monad where

> import Curry.Debugger.DebugMonad
> import Curry.Debugger.ShowTerm
> import Control.Monad.State


> instance MonadIO m => DM (DMT Int m) where

>   hook hookType di call = do
>     let args = currentValues (dynamicInfo di)
>     level <- getToolState
>     oracle <- getOracle
>     let oracleInfo = format 4 (show oracle)
>     liftIO $ putStrLn (oracleInfo ++ (replicate level ' ') ++ showHook hookType args)
>     putToolState (level + 1)
>     result <- call
>     putToolState level
>     return result

> format :: Int -> String -> String
> format n [] | n < 0     = []
>             | otherwise = format (n-1) [] ++ " "
> format n (c:cs)         = c : format (n-1) cs

> showHook :: Hook -> [Term] -> String
> showHook (FuncCallHook name) args = "FuncCallHook " ++ showHookSignature name args
> showHook (FuncDeclHook name) args = "FuncDeclHook " ++ showHookSignature name args
> showHook hook                args = showHookSignature (show hook) args

> showHookSignature :: String -> [Term] -> String
> showHookSignature name args =
>   name ++ " " ++ showedArgs
>     where
>       showedArgs = concatMap (\ t -> showTerm t ++ " ") args

> run :: GenTerm a => DMT Int IO a -> String -> IO a
> run exp steps = do
>            putStrLn $ "starting strict evaluation"
>            result <- debugDMT exp steps 0
>            printTerm result
>            putStrLn "\nstrict evaluation finished"
>            return result

