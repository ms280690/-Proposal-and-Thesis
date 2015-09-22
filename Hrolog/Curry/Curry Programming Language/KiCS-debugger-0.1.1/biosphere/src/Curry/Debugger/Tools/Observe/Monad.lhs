> {-# LANGUAGE FlexibleInstances #-}

> module Curry.Debugger.Tools.Observe.Monad where

> import Curry.Debugger.DebugMonad
> import Curry.Debugger.ShowTerm
> import Control.Monad.State

> instance MonadIO m => DM (DMT [String] m) where
             
>   funcCallHook name di call = do
>     let args = currentValues (dynamicInfo di)
>     funcs <- getToolState
>     result <- call
>     when (elem name funcs) 
>       (liftIO $ putStrLn $ showFuncCall name args $ genTerm result)
>     return result

	>     let rt = genTerm result
	>     case rt of
	>       TermUnderscore _ ->
	>         return result
	>       _                -> do
	>         when (elem name funcs) 
	>            (liftIO $ putStrLn $ showFuncCall name args rt)
	>         return result

> showFuncCall :: String -> [Term] -> Term -> String
> showFuncCall func args result =
>   func ++ " " ++ showedArgs ++ "= " ++ showTerm result
>     where
>       showedArgs = concatMap (\ t -> showTerm t ++ " ") args

> run :: GenTerm a => DMT [String] IO a -> String -> IO a
> run exp steps = do
>   putStrLn "Enter names of functions to observe:"
>   funcs <- getLine
>   result <- debugDMT exp steps (words funcs)
>   printTerm result
>   putStrLn ""
>   return result

  

