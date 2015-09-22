module Curry.Files.KiCSPath 
  (getKiCS
  ,getKiCSLibDir
  ,getKiCSDataDir
  ,kicsVersion
  ) where

import Data.Version
import System.FilePath
import Paths_KiCS

kicsVersion = showVersion version

getKiCS     = do
  kicsDir <- getBinDir
  return (kicsDir </> "kics")

getKiCSLibDir = do
  dir <- getDataDir
  return (dir </> "Curry" </> "Module")

getKiCSDataDir = getDataDir