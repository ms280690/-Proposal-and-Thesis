{- |
    Module      :  $Header$
    Description :  Main module
    Copyright   :  (c) 2005        Martin Engelke
                       2011 - 2014 Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

    Command line tool for generating Curry representations (e.g. FlatCurry,
    AbstractCurry) for a Curry source file including all imported modules.
-}
module Main (main) where

import Base.Messages
import Files.CymakePath (cymakeGreeting, cymakeVersion)
import Html.CurryHtml   (source2html)

import CurryBuilder (buildCurry)
import CompilerOpts (Options (..), CymakeMode (..), getCompilerOpts, usage)

-- |The command line tool cymake
main :: IO ()
main = getCompilerOpts >>= cymake

-- |Invoke the curry builder w.r.t the command line arguments
cymake :: (String, Options, [String], [String]) -> IO ()
cymake (prog, opts, files, errs)
  | mode == ModeHelp           = printUsage prog
  | mode == ModeVersion        = printVersion
  | mode == ModeNumericVersion = printNumericVersion
  | not $ null errs            = badUsage prog errs
  | null files                 = badUsage prog ["no input files"]
  | mode == ModeHtml           = runEitherCYIO $ mapM_ (source2html opts) files
  | otherwise                  = runEitherCYIO $ mapM_ (buildCurry  opts) files
  where mode = optMode opts

-- |Print the usage information of the command line tool
printUsage :: String -> IO ()
printUsage prog = putStrLn $ usage prog

-- |Print the program version
printVersion :: IO ()
printVersion = putStrLn cymakeGreeting

-- |Print the numeric program version
printNumericVersion :: IO ()
printNumericVersion = putStrLn cymakeVersion

-- |Print errors and abort execution on bad parameters
badUsage :: String -> [String] -> IO ()
badUsage prog errs = do
  putErrsLn $ map (\ err -> prog ++ ": " ++ err) errs
  abortWith ["Try '" ++ prog ++ " --help' for more information"]
