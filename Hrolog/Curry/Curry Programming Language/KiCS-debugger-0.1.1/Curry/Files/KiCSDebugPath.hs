module Curry.Files.KiCSDebugPath 
  (getMkstrict
  ,debugVersion
  ,Curry.Files.KiCSDebugPath.getOracleLibDir
  ,getProphecy
  ) where

import Data.Version
import System.FilePath
import Paths_KiCS_debugger

import Curry.Files.ProphecyPath as O

debugVersion = showVersion version

getMkstrict = do
  execDir <- getBinDir
  return (execDir </> "mkstrict")

getOracleLibDir = do
  o <- O.getOracleLibDir
  d <- getDataDir
  return [o,d </> "Curry" </> "Module"]
  

