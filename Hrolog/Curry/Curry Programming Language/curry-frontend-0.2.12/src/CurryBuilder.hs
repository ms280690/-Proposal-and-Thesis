-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--
-- CurryBuilder - Generates Curry representations for a Curry source file
--                including all imported modules.
--
-- September 2005,
-- Martin Engelke (men@informatik.uni-kiel.de)
-- March 2007, extensions by Sebastian Fischer (sebf@informatik.uni-kiel.de)
--
module CurryBuilder (buildCurry, smake) where

import System.Exit
import System.Time
import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import Data.List 
import System.IO

import Curry.Base.Ident

import Modules (compileModule)
import CurryCompilerOpts 
import CurryDeps
import Curry.Files.Filenames
import Curry.Files.PathUtils


flatName' :: Options -> FilePath -> FilePath
flatName' o
    | extendedFlat o = extFlatName
    | otherwise      = flatName

-------------------------------------------------------------------------------

-- Compiles the Curry program 'file' including all imported modules, depending
-- on the options 'options'. The compilation was successful, if the returned
-- list is empty, otherwise it contains error messages.
buildCurry :: Options -> FilePath -> IO ()
buildCurry options file
   = do let paths = importPaths options
	file'          <- getCurryPath paths file
	(cfile, errs1) <- return (maybe ("", [missingModule file])
			                (\f -> (f,[]))
				        file')
	unless (null errs1) (abortWith errs1)
	(deps, errs2) <- genDeps paths cfile
	unless (null errs2) (abortWith errs2)
	makeCurry options deps cfile


-------------------------------------------------------------------------------

makeCurry :: Options -> [(ModuleIdent,Source)] -> FilePath -> IO ()
makeCurry options deps file
   = mapM compile (map snd deps) >> return ()
 where
 compile (Source file' mods)
    | dropExtension file == dropExtension file'
      = do 
           flatIntfExists <- doesModuleExist (flatIntName file')
	   if flatIntfExists && not (force options) && null (dump options)
	    then smake (targetNames file')
                       (file':(catMaybes (map flatInterface mods)))
		       (generateFile file')
		       (skipFile file')
	    else generateFile file'
    | otherwise
      = do 
           flatIntfExists <- doesModuleExist (flatIntName file')
	   if flatIntfExists
            then  smake [flatName' options file'] --[flatName file', flatIntName file']
	                (file':(catMaybes (map flatInterface mods)))
			(compileFile file')
			(skipFile file')
	    else compileFile file'
 compile _ = return ()

 compileFile file
    = do unless (noVerb options) (putStrLn ("compiling " ++ file ++ " ..."))
	 compileModule (compOpts True) file
	 return ()

 skipFile file
    = do unless (noVerb options)
		(putStrLn ("skipping " ++ file ++ " ..."))

 generateFile file
    = do unless (noVerb options) 
		(putStrLn ("generating "  
			   ++ (head (targetNames file))               
			   ++ " ..."))
	 compileModule (compOpts False) file
	 return ()

 targetNames fn         
        | flat options            = [flatName' options fn] -- , flatIntName fn]
		| flatXml options         = [xmlName fn]
		| abstract options        = [acyName fn]
		| untypedAbstract options = [uacyName fn]
		| parseOnly options       = [maybe (sourceRepName fn) id (output options)]
		| otherwise               = [flatName' options fn] -- , flatIntName fn]

 flatInterface mod 
    = case (lookup mod deps) of
        Just (Source file _)  -> Just (flatIntName (dropExtension file))
	Just (Interface file) -> Just (flatIntName (dropExtension file))
	_                     -> Nothing

 compOpts isImport
    | isImport 
      = options 
	   { flat = True,
	     flatXml = False,
	     abstract = False,
	     untypedAbstract = False,
	     parseOnly = False,
	     dump = []
	   }
    | otherwise = options

-------------------------------------------------------------------------------

-- Computes a dependency list for the Curry file 'file' (such a list
-- usualy starts with the prelude and ends with 'file'). The result 
-- is a tuple containing an association list (type [(ModuleIdent,Source)]; 
-- see module "CurryDeps") and a list of error messages.
genDeps :: [FilePath] -> FilePath
	   -> IO ([(ModuleIdent,Source)], [String])
genDeps paths file
   = fmap flattenDeps (deps paths [] Map.empty file)


-------------------------------------------------------------------------------
-- A simple make function

-- smake <destination files>
--       <dependencies> 
--       <io action, if dependencies are newer than destination files>
--       <io action, if destination files are newer than dependencies>
smake :: [FilePath] -> [FilePath] -> IO a -> IO a -> IO a
smake dests deps cmd alt
   = do destTimes <- getDestTimes dests
	depTimes  <- getDepTimes deps
	make destTimes depTimes
 where
 make destTimes depTimes
    | (length destTimes) < (length dests) 
      = catch cmd (\err -> abortWith [show err]) 
    | null depTimes 
      = abortWith ["unknown dependencies"]
    | outOfDate destTimes depTimes
      = catch cmd (\err -> abortWith [show err])
    | otherwise
      = alt

--
getDestTimes :: [FilePath] -> IO [ClockTime]
getDestTimes [] = return []
getDestTimes (file:files)
   = catch (do time  <- getModuleModTime file
	       times <- getDestTimes files
	       return (time:times))
           (const (getDestTimes files))

--
getDepTimes :: [String] -> IO [ClockTime]
getDepTimes [] = return []
getDepTimes (file:files)
   = catch (do time  <- getModuleModTime file
	       times <- getDepTimes files
	       return (time:times))
           (\err -> abortWith [show err])

--
outOfDate :: [ClockTime] -> [ClockTime] -> Bool
outOfDate tgtimes dptimes = or (map (\t -> or (map ((<) t) dptimes)) tgtimes)

-------------------------------------------------------------------------------
-- Error handling

-- Prints an error message on 'stderr'
putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

-- Prints a list of error messages on 'stderr'
putErrsLn :: [String] -> IO ()
putErrsLn = mapM_ putErrLn

-- Prints a list of error messages on 'stderr' and aborts the program
abortWith :: [String] -> IO a
abortWith errs = putErrsLn errs >> exitWith (ExitFailure 1)


-- Error messages

missingModule :: FilePath -> String
missingModule file = "Error: missing module \"" ++ file ++ "\""

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
