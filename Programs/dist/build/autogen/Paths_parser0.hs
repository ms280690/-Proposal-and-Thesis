module Paths_parser0 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/mehul/.cabal/bin"
libdir     = "/home/mehul/.cabal/lib/parser0-0.1.0.0/ghc-7.6.3"
datadir    = "/home/mehul/.cabal/share/parser0-0.1.0.0"
libexecdir = "/home/mehul/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "parser0_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "parser0_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "parser0_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "parser0_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
