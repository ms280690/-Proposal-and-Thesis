module Curry.Compiler.KicsSubdir where

import System.Directory
import System.FilePath
import System.Time (ClockTime)
import Control.Monad (when)
import Data.List (intersperse,nubBy)

curDirPath :: FilePath
curDirPath = "."

path :: String -> [String]
path = canonPath . separateBy isPathSeparator 
  where
    canonPath (c:cs) = c:filter (not . null) cs

-- separate a list by separator predicate

separateBy :: (a -> Bool) -> [a] -> [[a]]
separateBy p = sep id 
  where
    sep xs [] = [xs []]
    sep xs (c:cs) = if p c then xs [] : sep id cs
                           else sep (xs . (c:)) cs

unpath :: [String] -> String
unpath = concat . intersperse [pathSeparator]

toPathList :: [String] -> String
toPathList = concat . intersperse [searchPathSeparator]


--When we split a path into its basename and directory we will make
--sure that the basename does not contain any path separators.
 
dirname, basename :: FilePath -> FilePath
dirname  = unpath . init . path
basename = last . path

-- add a subdirectory to a given filename 
-- if it is not already present

inSubdir :: String -> String -> String
inSubdir fn sub = unpath $ add (path fn) 
  where
    add ps@[n] = sub:ps
    add ps@[p,n] | p==sub = ps
    add (p:ps) = p:add ps

withoutSubdir :: String -> String -> String
withoutSubdir fn sub = unpath $ rmv (path fn) 
  where
    rmv [] = []
    rmv [p,n]  | p==sub = [n]
    rmv (p:ps) = p:rmv ps


--The sub directory to hide files in:

currySubdir :: String 
currySubdir = ".curry"

inCurrySubdir :: String -> String
inCurrySubdir = (`inSubdir` currySubdir)

kicsSubdir = "kics"
addKicsSubdir s = unpath [s,currySubdir,kicsSubdir]

pathWithSubdirs :: [FilePath] -> [FilePath]
pathWithSubdirs = concatMap dirWithSubdirs
  where
    dirWithSubdirs dir = [dir,unpath [dir,currySubdir,[pathSeparator]],
                              unpath [dir,currySubdir,kicsSubdir,[pathSeparator]]] 

inKicsSubdir :: String -> String
inKicsSubdir s = inCurrySubdir s `inSubdir` kicsSubdir

inModuleSubdir :: String -> String
inModuleSubdir s = s `inSubdir` "Curry" `inSubdir` "Module"

-- destination file names

destination :: Bool -> Maybe String -> String -> String
destination _     (Just t) _        = t
destination False _        filename = inKicsSubdir filename 
destination True  _        filename = inModuleSubdir (inKicsSubdir filename)

--write a file to curry subdirectory

writeKicsFile :: String -> String -> IO ()
writeKicsFile filename contents = do
  let subdir = dirname filename
  createDirectoryIfMissing True subdir
  writeFile filename contents

-- do things with file in subdir

onExistingFileDo :: (String -> IO a) -> String -> IO a
onExistingFileDo act fn = do
  let filename = fn --(fn `withoutSubdir` kicsSubdir) 
  ex <- doesFileExist filename
  if ex then act filename 
    else do
      let filename' = inCurrySubdir filename
      ex <- doesFileExist filename'
      if ex then act filename' 
        else do
          let filename'' = inKicsSubdir filename
          act filename''

readModule :: String -> IO String
readModule = onExistingFileDo readFile

maybeReadModule :: String -> IO (Maybe String)
maybeReadModule filename = 
  catch (readModule filename >>= return . Just) (\_ -> return Nothing)

doesModuleExist :: String -> IO Bool
doesModuleExist = onExistingFileDo doesFileExist

getModuleModTime :: String -> IO ClockTime
getModuleModTime = onExistingFileDo getModificationTime

findFileInPath :: String -> [String] -> IO [String]
findFileInPath fn path = do
   if any isPathSeparator fn 
     then findFile fn
     else do
       let fs = nubBy equalFilePath $ map (</> fn) path
       founds <- mapM findFile fs
       return (nubBy equalFilePath $ concat founds)

  where
    findFile = onExistingFileDo doesExist
    doesExist fn = do ex <- doesFileExist fn 
                      if ex then return [fn] else return []
