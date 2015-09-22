> {-# OPTIONS_GHC -fglasgow-exts #-}
> module Curry.Debugger.Tools.Tracer.Monad where

> import Control.Monad.State
> import Control.Monad.Trans

> import Curry.Debugger.DebugMonad
> import Curry.Debugger.Oracle
> import Curry.Debugger.DebugInfo
> import Curry.Debugger.DebugInfoGoodies
> import Curry.Debugger.ShowTerm

> type TM a = DMT () IO a

> instance DM (DMT () IO) where
>     funcDeclHook name info ma = do
>         result <- ma
>         istr <- liftIO $ isTrace name info
>         when (istr)
>              (liftIO $ do
>                           let label = getName info
>                           putStrLn $ label ++ ":"
>                           putStrLn $ replicate (1 + length label) '='
>                           putStrLn $ showGenTerm result
>                           putStrLn "")
>         return result 

> traceFuncName :: (String, String)
> traceFuncName = ("Trace", "trace")

> isTrace :: String -> DI -> IO Bool
> isTrace fname di = do
>     return $ (getModuleName di, fname) == traceFuncName

> getName :: DI -> String
> getName = showTerm . head . getFuncArgs

> run :: GenTerm a => TM a -> String -> IO a
> run ta sfile = debugDMT ta sfile ()

