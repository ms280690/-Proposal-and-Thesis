> {-# OPTIONS_GHC -fglasgow-exts #-}
> module Curry.Debugger.Tools.Observations.Monad where

> import Control.Monad.Trans
> import Control.Monad
> import Curry.Debugger.DebugMonad

> import Curry.Debugger.ShowTerm

> data ObservationsState = OS { fnames :: [String]
>                             , obs :: [(String, [String])] }

> instance Monad m => DM (DMT ObservationsState m) where

>   funcCallHook fname info ma = do
>     r <- ma
>     st <- getToolState
>     let args = currentValues (dynamicInfo info)
>     when
>       (elem fname $ fnames st)
>       (addObs fname $ showFuncCall fname args $ genTerm r)
>     return r

> showFuncCall :: String -> [Term] -> Term -> String
> showFuncCall func args result =
>   (niceFname func) ++ " " ++ showedArgs ++ "= " ++ showTerm result
>     where
>       showedArgs = concatMap (\ t -> showTerm t ++ " ") args

> niceFname fname =
>   let c = head fname
>   in  if c < 'a' || c > 'z' then
>           '(' : fname ++ ")"
>       else
>           fname

> addObs fname val = modifyToolState $
>                        \st -> st { obs = add fname val $ obs st }
>   where
>     add n v [] = [(n, [v])]
>     add n v ((n', vs) : nvs)
>       | n == n' = (n, v : vs) : nvs
>       | otherwise = (n', vs) : add n v nvs

> observe exp = do
>     r <- exp
>     st <- getToolState
>     return (r, obs st)

> run exp fname = do
>   putStrLn "Enter names of functions to observe:"
>   fns <- getLine
>   (r, os) <- debugDMT (observe exp) fname $ OS { fnames = words fns, obs = [] }
>   putStr "Result: "
>   putStrLn $ showGenTerm r
>   putStrLn ""
>   putStrLn "Observations"
>   putStrLn "============"
>   printObservations os

> printObservations [] = return ()
> printObservations ((_, es) : os) = do
>   mapM_ (liftIO . putStrLn) $ reverse es
>   printObservations os

