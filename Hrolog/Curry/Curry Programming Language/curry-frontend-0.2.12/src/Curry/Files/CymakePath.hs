module Curry.Files.CymakePath (getCymake,cymakeVersion) where

import Data.Version
import System.FilePath
import Paths_curry_frontend

-- | Retrieve the version number of cymake
cymakeVersion :: String
cymakeVersion = showVersion version

-- | Retrieve the location of the cymake executable
getCymake :: IO String
getCymake     = do
  cymakeDir <- getBinDir
  return (cymakeDir </> "cymake")
