module Curry.Files.ProphecyPath 
  (getProphecy
  ,getOracleLibDir
  ,getOracleLibBaseDir
  ,prophecyVersion
  ) where

import Data.Version
import System.FilePath
import Paths_KiCS_prophecy

prophecyVersion = showVersion version

getProphecy = do
  execDir <- getBinDir
  return (execDir </> "prophecy")

getOracleLibBaseDir = getDataDir

getOracleLibDir = do
  dir <- getDataDir
  return (dir </> "Curry" </> "Module")
