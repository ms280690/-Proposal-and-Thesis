
% $Id: CurryDeps.lhs,v 1.14 2004/02/09 17:10:05 wlux Exp $
%
% Copyright (c) 2002-2004, Wolfgang Lux
% See LICENSE for the full license.
%
% Modified by Martin Engelke (men@informatik.uni-kiel.de)
% Extended by Sebastian Fischer (sebf@informatik.uni-kiel.de)
\nwfilename{CurryDeps.lhs}
\section{Building Programs}
This module implements the functions to compute the dependency
information between Curry modules. This is used to create Makefile
dependencies and to update programs composed of multiple modules.
\begin{verbatim}

> module CurryDeps(Source(..),
>                  deps, flattenDeps, sourceDeps, moduleDeps
>                 ) where

> import Data.List
> import qualified Data.Map as Map
> import Data.Maybe
> import Control.Monad

> import Curry.Base.Ident
> import Curry.Base.MessageMonad

> import Curry.Files.Filenames
> import Curry.Files.PathUtils

> import Curry.Syntax hiding(Interface(..))

> import SCC


> data Source = Source FilePath [ModuleIdent]
>             | Interface FilePath
>             | Unknown
>             deriving (Eq,Ord,Show)
> type SourceEnv = Map.Map ModuleIdent Source

> deps :: [FilePath] -> [FilePath] -> SourceEnv -> FilePath -> IO SourceEnv
> deps paths libraryPaths mEnv fn
>   | e `elem` sourceExts = sourceDeps paths libraryPaths (mkMIdent [r]) mEnv fn
>   | e == icurryExt = return Map.empty
>   | e `elem` objectExts = targetDeps paths libraryPaths mEnv r
>   | otherwise = targetDeps paths libraryPaths mEnv fn
>   where r = dropExtension fn
>         e = takeExtension fn

> targetDeps :: [FilePath] -> [FilePath] -> SourceEnv -> FilePath
>            -> IO SourceEnv
> targetDeps paths libraryPaths mEnv fn =
>   lookupFile [""] sourceExts fn >>=
>   maybe (return (Map.insert m Unknown mEnv)) (sourceDeps paths libraryPaths m mEnv)
>   where m = mkMIdent [fn]

\end{verbatim}
The following functions are used to lookup files related to a given
module. Source files for targets are looked up in the current
directory only. Two different search paths are used to look up
imported modules, the first is used to find source modules, whereas
the library path is used only for finding matching interface files. As
the compiler does not distinguish these paths, we actually check for
interface files in the source paths as well.

Note that the functions \texttt{buildScript} and \texttt{makeDepend}
already remove all directories that are included in the both search
paths from the library paths in order to avoid scanning such
directories more than twice.
\begin{verbatim}

\end{verbatim}
In order to compute the dependency graph, source files for each module
need to be looked up. When a source module is found, its header is
parsed in order to determine the modules that it imports, and
dependencies for these modules are computed recursively. The prelude
is added implicitly to the list of imported modules except for the
prelude itself. Any errors reported by the parser are ignored.
\begin{verbatim}

> moduleDeps :: [FilePath] -> [FilePath] -> SourceEnv -> ModuleIdent
>            -> IO SourceEnv
> moduleDeps paths libraryPaths mEnv m =
>   case Map.lookup m mEnv of
>     Just _ -> return mEnv
>     Nothing ->
>       do
>         mbFn <- lookupModule paths libraryPaths m
>         case mbFn of
>           Just fn
>             | icurryExt `isSuffixOf` fn ->
>                 return (Map.insert m (Interface fn) mEnv)
>             | otherwise -> sourceDeps paths libraryPaths m mEnv fn
>           Nothing -> return (Map.insert m Unknown mEnv)

> sourceDeps :: [FilePath] -> [FilePath] -> ModuleIdent -> SourceEnv
>            -> FilePath -> IO SourceEnv
> sourceDeps paths libraryPaths m mEnv fn =
>   do
>     s <- readModule fn
>     case fst $ runMsg $ parseHeader fn s of
>       Right (Module m' _ ds) ->
>         let ms = imports m' ds in
>         foldM (moduleDeps paths libraryPaths) (Map.insert m (Source fn ms) mEnv) ms
>       Left _ -> return (Map.insert m (Source fn []) mEnv)

> imports :: ModuleIdent -> [Decl] -> [ModuleIdent]
> imports m ds = nub $
>   [preludeMIdent | m /= preludeMIdent] ++ [m | ImportDecl _ m _ _ _ <- ds]



If we want to compile the program instead of generating Makefile
dependencies the environment has to be sorted topologically. Note
that the dependency graph should not contain any cycles.

> flattenDeps :: SourceEnv -> ([(ModuleIdent,Source)],[String])
> flattenDeps = fdeps . sortDeps
>     where
>     sortDeps :: SourceEnv -> [[(ModuleIdent,Source)]]
>     sortDeps = scc modules imports . Map.toList
>
>     modules (m, _) = [m]
>
>     imports (_,Source _ ms) = ms
>     imports (_,Interface _) = []
>     imports (_,Unknown) = []
>
>     fdeps :: [[(ModuleIdent,Source)]] -> ([(ModuleIdent,Source)],[String])
>     fdeps = foldr checkdep ([], [])
>     
>     checkdep [] (ms', es')  = (ms',es')
>     checkdep [m] (ms', es') = (m:ms',es')
>     checkdep dep (ms', es') = (ms',cyclicError (map fst dep) : es')
>
>     cyclicError :: [ModuleIdent] -> String
>     cyclicError (m:ms) =
>         "Cylic import dependency between modules " ++ show m ++ rest ms
>
>     rest [m] = " and " ++ show m
>     rest ms  = rest' ms
>     rest' [m] = ", and " ++ show m
>     rest' (m:ms) = ", " ++ show m ++ rest' ms
