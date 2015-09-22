module Main where

import Control.Monad (when)
import System.Cmd
import System.Environment

import Curry.Compiler.Config
import Curry.Compiler.CurryToHaskell
import Curry.Compiler.SafeCalls

-------------------------------
-- the kics compiler
-------------------------------

main :: IO ()
main = do 
  (opts,_) <- getOptions 
  if null (filename opts)
   then usage "no file to compile"
   else do
      safe (startCompilation opts) >>=
        maybe (error "error during compilaton") (\_ -> return ())
      let call = ghcCall opts{filename=inKicsSubdir (maybe "Main.hs" (++".hs") $ binary opts)}
      when (executable opts && make opts)
           (do 
              putStrLn ("compiling executable "++maybe "" id (binary opts)) 
              safe $ put 3 opts call
              system call
              return ())
