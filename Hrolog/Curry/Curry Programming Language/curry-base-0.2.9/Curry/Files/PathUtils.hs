{-
  $Id: PathUtils.lhs,v 1.5 2003/05/04 16:12:35 wlux Exp $

  Copyright (c) 1999-2003, Wolfgang Lux
  See LICENSE for the full license.
-}

module Curry.Files.PathUtils
  ( -- * Re-exports from 'System.FilePath'
    takeBaseName, dropExtension, takeExtension, takeFileName

    -- * Retrieving curry fiiles
  , lookupModule, lookupFile, lookupInterface, getCurryPath

    -- * Reading and writing modules from files
  , writeModule, readModule, maybeReadModule
  , doesModuleExist, getModuleModTime
  ) where

import Control.Monad (liftM)
import System.FilePath
import System.Directory
import System.Time (ClockTime)

import Curry.Base.Ident
import Curry.Files.Filenames


{- |Search for a given curry module in the given source file paths and
    library paths. Note that the current directory is always searched first.
-}
lookupModule :: [FilePath]          -- ^ list of paths to source files
             -> [FilePath]          -- ^ list of paths to library files
             -> ModuleIdent         -- ^ module identifier
             -> IO (Maybe FilePath) -- ^ the file path if found
lookupModule paths libPaths m =
  lookupFile ("" : paths ++ libPaths) moduleExts fn
  where fn = foldr1 combine (moduleQualifiers m)


{- |Search for an interface file in the import search path using the
    interface extension 'flatIntExt'. Note that the current directory is
    always searched first.
-}
lookupInterface :: [FilePath]          -- ^ list of paths to search in
                -> ModuleIdent         -- ^ module identifier
                -> IO (Maybe FilePath) -- ^ the file path if found
lookupInterface paths m = lookupFile ("" : paths) [flatIntExt] ifn
  where ifn = foldr1 combine (moduleQualifiers m)


-- |Search for a source file name and eventually return its content
lookupFile :: [FilePath]          -- ^ list of file paths to search in
           -> [String]            -- ^ list of possible extensions of the file
           -> String              -- ^ initial file name
           -> IO (Maybe FilePath) -- ^ the file path if found
lookupFile paths exts file = lookupFile' paths' where
  paths' = do
           p <- paths
           e <- exts
           let fn = p `combine` replaceExtension file e
           [fn, ensureCurrySubdir fn]
  lookupFile' []      = return Nothing
  lookupFile' (fn:ps) = do
                        so <- doesFileExist fn
                        if so then return (Just fn) else lookupFile' ps


{- | Search in the given list of paths for the given file name. If the file
     name has no extension then source file extension is assumed. If the file
     name already contains a directory than the paths to search in are
     ignored.
-}
getCurryPath :: [FilePath] -> FilePath -> IO (Maybe FilePath)
getCurryPath paths fn = lookupFile filepaths exts fn where
  filepaths = "" : paths'
  fnext     = takeExtension fn
  exts   | null fnext = sourceExts
         | otherwise  = [fnext]
  paths' | pathSeparator `elem` fn = []
         | otherwise               = paths


-- Writing and reading files

{- | Write the content to a file in the given directory or in the
     'currySubdir' sub-directory if the first parameter is set to 'True'.
-}
writeModule :: Bool     -- ^ should the 'currySubdir' be included in the path?
            -> FilePath -- ^ original path
            -> String   -- ^ file content
            -> IO ()
writeModule inSubdir filename contents = do
  let fn = if inSubdir then ensureCurrySubdir filename else filename
  createDirectoryIfMissing True $ takeDirectory fn
  writeFile fn contents


{- | Read the content from a file in the given directory or in the
     'currySubdir' sub-directory of the given sub-directory.
-}
readModule :: FilePath -> IO String
readModule = onExistingFileDo readFile


{- | Tries to read the specified module and returns either 'Just String' if
     reading was successful or 'Nothing' otherwise.
-}
maybeReadModule :: FilePath -> IO (Maybe String)
maybeReadModule filename =
  catch (liftM Just (readModule filename)) (\ _ -> return Nothing)


{- | Check whether a module exists either in the given directory or in the
     'currySubdir'.
-}
doesModuleExist :: FilePath -> IO Bool
doesModuleExist = onExistingFileDo doesFileExist


-- | Get the modification time of a file
getModuleModTime :: FilePath -> IO (System.Time.ClockTime)
getModuleModTime = onExistingFileDo getModificationTime


-- Helper functions


{- | Ensure that the 'currySubdir' is the last component of the
     directory structure of the given 'FilePath'. If the 'FilePath' already
     contains the 'currySubdir' it remains unchanged.
-}
ensureCurrySubdir :: FilePath -> FilePath
ensureCurrySubdir = ensureSubdir currySubdir


{- | Ensure that the given sub-directory is the last component of the
     directory structure of the given 'FilePath'. If the 'FilePath' already
     contains the sub-directory it remains unchanged.
-}
ensureSubdir :: String   -- ^ sub-directory to add
             -> FilePath -- ^ original 'FilePath'
             -> FilePath -- ^ original 'FilePath'
ensureSubdir subdir file
  = replaceDirectory file
  $ addSub (splitDirectories $ takeDirectory file) subdir where
  addSub :: [String] -> String -> String
  addSub [] sub      = sub
  addSub ds sub
    | last ds == sub = joinPath ds
    | otherwise      = joinPath ds </> sub


{- | Perform an action on a file either in the given directory or else in the
     'currySubdir' sub-directory.
-}
onExistingFileDo :: (FilePath -> IO a) -> FilePath -> IO a
onExistingFileDo act filename = do
  ex <- doesFileExist filename
  if ex then act filename
        else act $ ensureCurrySubdir filename
