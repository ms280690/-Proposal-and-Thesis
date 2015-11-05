module Paths_Prototype1 (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/solanki/.cabal/bin"
libdir     = "/home/solanki/.cabal/lib/x86_64-linux-ghc-7.8.3/Prototype1-0.1.0.0"
datadir    = "/home/solanki/.cabal/share/x86_64-linux-ghc-7.8.3/Prototype1-0.1.0.0"
libexecdir = "/home/solanki/.cabal/libexec"
sysconfdir = "/home/solanki/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Prototype1_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Prototype1_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Prototype1_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Prototype1_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Prototype1_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
