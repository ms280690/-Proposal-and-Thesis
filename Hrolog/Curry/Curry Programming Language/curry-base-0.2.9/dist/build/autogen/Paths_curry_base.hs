module Paths_curry_base (
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
version = Version {versionBranch = [0,2,9], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/curry-base-0.2.9/ghc-7.6.3"
datadir    = "/usr/local/share/curry-base-0.2.9"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "curry_base_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "curry_base_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "curry_base_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "curry_base_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
