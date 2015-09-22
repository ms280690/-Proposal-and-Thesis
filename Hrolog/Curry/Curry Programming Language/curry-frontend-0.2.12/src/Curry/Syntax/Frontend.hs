-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--
-- Frontend - Provides an API for dealing with several kinds of Curry
--            program representations
--
-- December 2005,
-- Martin Engelke (men@informatik.uni-kiel.de)
--
module Curry.Syntax.Frontend (lex, parse, fullParse, typingParse)where

import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.Writer
import Control.Monad.Error
import Prelude hiding (lex)


import Curry.Base.MessageMonad
import Curry.Base.Ident
import Curry.Base.Position

import Curry.Files.Filenames
import Curry.Files.PathUtils

import qualified Curry.Syntax as CS
import Curry.Syntax.Lexer

import Modules
import CurryBuilder
import CurryCompilerOpts

import CurryDeps

import Base(ModuleEnv)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Returns the result of a lexical analysis of the source program 'src'.
-- The result is a list of tuples consisting of a position and a token
-- (see Modules "Position" and "CurryLexer")
lex :: FilePath -> String -> MsgMonad [(Position,Token)]
lex fn src = lexFile (first fn) src False []


-- Returns the result of a syntactical analysis of the source program 'src'.
-- The result is the syntax tree of the program (type 'Module'; see Module
-- "CurrySyntax").
parse :: FilePath -> String -> MsgMonad CS.Module
parse fn src = CS.parseModule True fn src >>= genCurrySyntax fn


-- Returns the syntax tree of the source program 'src' (type 'Module'; see
-- Module "CurrySyntax") after resolving the category (i.e. function,
-- constructor or variable) of an identifier. 'fullParse' always
-- searches for standard Curry libraries in the path defined in the
-- environment variable "PAKCSLIBPATH". Additional search paths can
-- be defined using the argument 'paths'.
fullParse :: [FilePath] -> FilePath -> String -> IO (MsgMonad CS.Module)
fullParse paths fn src = -- liftM msgmonad2result $
                         genFullCurrySyntax simpleCheckModule paths fn (parse fn src)

-- Behaves like 'fullParse', but Returns the syntax tree of the source 
-- program 'src' (type 'Module'; see Module "CurrySyntax") after inferring 
-- the types of identifiers.
typingParse :: [FilePath] -> FilePath -> String -> IO (MsgMonad CS.Module)
typingParse paths fn src = genFullCurrySyntax checkModule paths fn (parse fn src)

{-
-- Compiles the source programm 'src' to an AbstractCurry program.
-- 'fullParse' always searches for standard Curry libraries in the path 
-- defined in the environment variable "PAKCSLIBPATH". Additional search 
-- paths can be defined using the argument 'paths'.
-- Notes: Due to the lack of error handling in the current version of the
-- front end, this function may fail when an error occurs
abstractIO :: [FilePath] -> FilePath -> String -> IO (MsgMonad ACY.CurryProg)
abstractIO paths fn src = genAbstractIO paths fn (parse fn src)

-- Compiles the source program 'src' to a FlatCurry program.
-- 'fullParse' always searches for standard Curry libraries in the path 
-- defined in the environment variable "PAKCSLIBPATH". Additional search 
-- paths can be defined using the argument 'paths'.
-- Note: Due to the lack of error handling in the current version of the
-- front end, this function may fail when an error occurs
flatIO :: [FilePath] -> FilePath -> String -> IO (MsgMonad FCY.Prog)
flatIO paths fn src = genFlatIO paths fn (parse fn src)
-}

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Privates...


opts paths = defaultOpts{ 
                     importPaths = paths,
		     noVerb      = True,
		     noWarn      = True,
		     abstract    = True
		   }


--
genCurrySyntax :: FilePath -> CS.Module -> MsgMonad (CS.Module)
genCurrySyntax fn mod
    = let mod'@(CS.Module mid _ _) = patchModuleId fn (importPrelude fn mod)
      in if isValidModuleId fn mid
	 then return mod'
	 else failWith $ err_invalidModuleName mid


--
genFullCurrySyntax :: (Options -> Base.ModuleEnv -> CS.Module -> IO (t1, t2, t3, CS.Module, t4, [WarnMsg]))
                   -> [FilePath] -> t -> MsgMonad CS.Module -> IO (MsgMonad CS.Module)
genFullCurrySyntax check paths fn m
   = runMsgIO m $ \mod -> do errs <- makeInterfaces paths mod
	                     if null errs
	                       then do mEnv <- loadInterfaces paths mod
		                       (_, _, _, mod', _, msgs') <- check (opts paths) mEnv mod
		                       return (tell msgs' >> return  mod')
	                       else return (failWith (head errs))


{-
genAbstractIO :: [FilePath] -> FilePath -> MsgMonad CS.Module
	      -> IO (MsgMonad ACY.CurryProg)
genAbstractIO paths fn m
   = runMsgIO m $ \mod ->
     do errs <- makeInterfaces paths mod
	if null errs
	   then do mEnv <- loadInterfaces paths mod
		   (tyEnv, tcEnv, _, mod', _, msgs')
		       <- simpleCheckModule (opts paths) mEnv mod
		   return (tell msgs' >> return (genTypedAbstract tyEnv tcEnv mod'))
	   else return (failWith $ head errs)


--
genFlatIO :: [FilePath] -> FilePath -> MsgMonad CS.Module -> IO (MsgMonad FCY.Prog)
genFlatIO paths fn m
   = runMsgIO m $ \ mod -> 
     do errs <- makeInterfaces paths mod
	if null errs then
	   (do mEnv <- loadInterfaces paths mod
	       (tyEnv, tcEnv, aEnv, mod', intf, msgs') <- 
	           checkModule (opts paths) mEnv mod
	       let (il, aEnv', _) 
	              = transModule True True False mEnv tyEnv tcEnv aEnv mod'
	           il' = completeCase mEnv il
	           cEnv = curryEnv mEnv tcEnv intf mod'
	           (prog,msgs'') = genFlatCurry (opts paths) cEnv mEnv 
	                                        tyEnv tcEnv aEnv' il'
               return (tell msgs'' >> tell msgs' >> return prog)
	   )
	   else return (failWith $ head errs)
-}


-------------------------------------------------------------------------------

-- Generates interface files for importes modules, if they don't exist or
-- if they are not up-to-date.
makeInterfaces ::  [FilePath] -> CS.Module -> IO [String]
makeInterfaces paths (CS.Module mid _ decls)
  = do let imports = [preludeMIdent | mid /= preludeMIdent] 
		      ++ [imp | CS.ImportDecl _ imp _ _ _ <- decls]
       (deps, errs) <- fmap flattenDeps (foldM (moduleDeps paths []) Map.empty imports)
       when (null errs) (mapM_ (compile deps . snd) deps)
       return errs
 where
 compile deps (Source file' mods)
    = do smake [flatName file', flatIntName file']
	       (file':mapMaybe (flatInterface deps) mods)
	       (compileModule (opts paths) file')
	       (return Nothing)
	 return ()
 compile _ _ = return ()

 flatInterface deps mod 
    = case (lookup mod deps) of
        Just (Source file _)  -> Just (flatIntName (dropExtension file))
	Just (Interface file) -> Just (flatIntName (dropExtension file))
	_                     -> Nothing


-- Returns 'True', if file name and module name are equal.
isValidModuleId :: FilePath -> ModuleIdent -> Bool
isValidModuleId fn mid
   = last (moduleQualifiers mid) == takeBaseName fn


-------------------------------------------------------------------------------
-- Messages

err_invalidModuleName :: ModuleIdent -> String
err_invalidModuleName mid 
   = "module \"" ++ moduleName mid 
     ++ "\" must be in a file \"" ++ moduleName mid ++ ".curry\""


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
