{-
  Filename mangling for several intermediate file formats.

  The functions in this module were collected from several
  compiler modules in order to provide a unique accessing
  point for this functionality.

  (c) 2009, Holger Siegel.
-}

module Curry.Files.Filenames
  (
    -- * Special directories
    currySubdir

    -- * Common file name extensions
  , curryExt, lcurryExt, icurryExt
  , flatExt, extFlatExt, flatIntExt, xmlExt
  , acyExt, uacyExt
  , sourceRepExt, oExt, debugExt
  , sourceExts, moduleExts, objectExts

    -- * Functions for computing file names
  , interfName, flatName, extFlatName, flatIntName, xmlName
  , acyName, uacyName
  , sourceRepName, objectName
  ) where

import System.FilePath (replaceExtension)

-- |The hidden subdirectory to hide curry files
currySubdir :: String
currySubdir = ".curry"

-- |Filename extension for non-literate curry files
curryExt :: String
curryExt = ".curry"

-- |Filename extension for literate curry files
lcurryExt :: String
lcurryExt = ".lcurry"

-- |Filename extension for curry interface files
icurryExt :: String
icurryExt = ".icurry"

-- |Filename extension for flat-curry files
flatExt :: String
flatExt = ".fcy"

-- |Filename extension for extended-flat-curry files
extFlatExt :: String
extFlatExt = ".efc"

-- |Filename extension for extended-flat-curry interface files
flatIntExt :: String
flatIntExt = ".fint"

-- |Filename extension for extended-flat-curry xml files
xmlExt :: String
xmlExt = "_flat.xml"

-- |Filename extension for abstract-curry files
acyExt :: String
acyExt = ".acy"

-- |Filename extension for untyped-abstract-curry files
uacyExt :: String
uacyExt = ".uacy"

-- |Filename extension for curry source representation files
sourceRepExt :: String
sourceRepExt = ".cy"

-- |Filename extension for object files
oExt :: String
oExt = ".o"

-- |Filename extension for debug object files
debugExt :: String
debugExt = ".d.o"

-- |Filename extension for curry source files
sourceExts :: [String]
sourceExts = [curryExt, lcurryExt]

-- |Filename extension for curry module files
moduleExts :: [String]
moduleExts = sourceExts ++ [icurryExt]

-- |Filename extension for object files
objectExts :: [String]
objectExts = [oExt]

{- ---------------------------------------------------------------------------
   Computation of file names for a given source file
--------------------------------------------------------------------------- -}

-- |Compute the filename of the interface file for a source file
interfName :: FilePath -> FilePath
interfName = replaceWithExtension icurryExt

-- |Compute the filename of the flat curry file for a source file
flatName :: FilePath -> FilePath
flatName = replaceWithExtension flatExt

-- |Compute the filename of the extended flat curry file for a source file
extFlatName :: FilePath -> FilePath
extFlatName = replaceWithExtension extFlatExt

-- |Compute the filename of the flat curry interface file for a source file
flatIntName :: FilePath -> FilePath
flatIntName = replaceWithExtension flatIntExt

-- |Compute the filename of the flat curry xml file for a source file
xmlName :: FilePath -> FilePath
xmlName = replaceWithExtension xmlExt

-- |Compute the filename of the abstract curry file for a source file
acyName :: FilePath -> FilePath
acyName = replaceWithExtension acyExt

-- |Compute the filename of the untyped abstract curry file for a source file
uacyName :: FilePath -> FilePath
uacyName = replaceWithExtension uacyExt

-- |Compute the filename of the source representation file for a source file
sourceRepName :: FilePath -> FilePath
sourceRepName = replaceWithExtension sourceRepExt

{- |Compute the filename of the object file for a source file.
    If the first parameter is 'True', the debug object file name is returned
-}
objectName :: Bool -> FilePath -> FilePath
objectName debug = replaceWithExtension (if debug then debugExt else oExt)

-- |Replace a filename extension with a new extension
replaceWithExtension :: String -> FilePath -> FilePath
replaceWithExtension = flip replaceExtension
