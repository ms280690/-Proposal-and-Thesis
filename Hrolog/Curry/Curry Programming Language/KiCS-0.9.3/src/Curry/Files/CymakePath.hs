{- |
    Module      :  $Header$
    Description :  File pathes
    Copyright   :  (c) 2011, Björn Peemöller (bjp@informatik.uni-kiel.de)
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  experimental
    Portability :  portable

    This module contains functions to obtain the version number and path
    of the cymake binary.
-}
module Files.CymakePath (getCymake, cymakeGreeting, cymakeVersion) where

import Data.Version (showVersion)
import System.FilePath ((</>))
import Paths_curry_frontend

-- | Show a greeting of the current cymake
cymakeGreeting :: String
cymakeGreeting = "This is cymake, version " ++ cymakeVersion

-- | Retrieve the version number of cymake
cymakeVersion :: String
cymakeVersion = showVersion version

-- | Retrieve the location of the cymake executable
getCymake :: IO String
getCymake = do
  cymakeDir <- getBinDir
  return $ cymakeDir </> "cymake"
