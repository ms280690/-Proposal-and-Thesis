module DebuggerPath (
  getDebugLoadPath,
  getDebugFrontendParams,
  getDebugLibPath) where

import Distribution
import FileGoodies

debuggerLibPath :: IO String
debuggerLibPath external

getDebugLibPath :: IO String
getDebugLibPath = do 
  path <- debuggerLibPath
  return (path ++ [separatorChar])

getDebugLoadPath :: IO [String]
getDebugLoadPath = do
  loadPath <- getLoadPath
  libPath  <- getDebugLibPath
  return (loadPath ++ [libPath])

getDebugFrontendParams :: IO FrontendParams 
getDebugFrontendParams = do
  loadPath <- getDebugLoadPath
  return (setFullPath loadPath defaultParams)