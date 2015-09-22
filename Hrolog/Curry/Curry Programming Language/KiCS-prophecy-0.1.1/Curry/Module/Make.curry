-------------------------------------------------------
-- This module provides basic make functionality for 
-- curry programs. The provided actions traverse the
-- import tree and execute a given action only if 
-- necessary.
-------------------------------------------------------

module Make (
  ModuleName,
  Path,
  FileName,
  make, makeWithFrontendParams, obsolete, unless,

  parseArgs,Parameter,
  quiet,force,modulename,output) where

import FlatCurry
import FlatCurryGoodies (progImports)
import Distribution as D
import FiniteMap
import IOExts
import Sort (leqString)
import FileGoodies 
import Time
import Directory
import System (getArgs)

type ModuleName = String
type Path       = String
type FileName   = String

type TestAct a = Path -> ModuleName -> IO (Maybe a)
type ProgAct a = Path -> [a] -> Prog -> IO a

type Done a = IORef (FM String a)

data Parameter = Parameter Bool Bool (Maybe String) (Maybe String) String

defaults :: Parameter
defaults = Parameter False False Nothing Nothing ""

type Getter a = Parameter -> a
type Setter a = a -> Parameter -> Parameter

quiet :: Getter Bool
quiet (Parameter x _ _  _ _) = x

setQuiet :: Setter Bool
setQuiet x (Parameter _ y z e m) = Parameter x y z e m

force :: Getter Bool
force (Parameter _ x _ _ _) = x

setForce :: Setter Bool
setForce x (Parameter y _ z e m) = Parameter y x z e m

output :: Getter (Maybe String)
output (Parameter _ _ x _ _) = x

setOutput :: Setter (Maybe String)
setOutput x (Parameter y z _ e m) = Parameter y z x e m

main :: Getter (Maybe String)
main (Parameter _ _ _ x _) = x

setMain :: Setter (Maybe String)
setMain x (Parameter y z m _ e) = Parameter y z m x e

modulename :: Getter String
modulename (Parameter _ _ _ _ x) = x

setModulename :: Setter String
setModulename x (Parameter y z m e _) = Parameter y z m e x

parseArgs :: IO Parameter
parseArgs = getArgs >>= return . parse defaults
  where
    parse _ []      = usage
    parse p (x:xs) 
     | x=="-q"      = parse (setQuiet True p) xs
     | x=="--quiet" = parse (setQuiet True p) xs
     | x=="-f"      = parse (setForce True p) xs
     | x=="--force" = parse (setForce True p) xs
     | x=="-o"      = case xs of
                       o:xs' -> parse (setOutput (Just o) p) xs'
     | x=="-m"      = case xs of
                       o:xs' -> parse (setMain (Just o) p) xs'
     | null xs      = setModulename x p
    
    usage = error "usage: <-f/--force> <-q/--quiet> <-o outputdir> modulename"

--- calls act on each imported module transitively
--- if test returns Nothing.
make :: Bool -> ModuleName -> TestAct a -> ProgAct a -> IO ()
make = makeWithFrontendParams defaultParams

makeWithFrontendParams :: FrontendParams -> Bool -> ModuleName -> TestAct a -> ProgAct a -> IO ()
makeWithFrontendParams givenParams qu file test act = do
  let (dir,modu) = splitDirectoryBaseName file
  path <- maybe getLoadPath return (fullPath givenParams)
  let loadPath = if dir=="." then path else dir:path
      params   = setFullPath loadPath givenParams
  mCurryFile  <- lookupFileInPath modu [".curry",".lcurry"] loadPath
  unless (mCurryFile==Nothing)
         (do
           unless qu $ putStrLn "ensuring existence of fcy/fint files..."
           callFrontendWithParams FCY (D.setQuiet True params) modu
           unless qu $ putStrLn "...ensured")
  done <- newIORef (emptyFM (\ x y -> not (leqString x y)))
  workUpDependence loadPath done test act modu
  return ()

workUpDependence :: [String] -> Done a -> TestAct a -> ProgAct a -> ModuleName -> IO a
workUpDependence path done test act modu = do
  fm <- readIORef done
  maybe (process path done test act modu) return (lookupFM fm modu)

process :: [String] -> Done a -> TestAct a -> ProgAct a ->  ModuleName -> IO a
process path done test act modu = do
  fn <- getFileInPath (modu++".fcy") [""] path
  imps <- fastReadImports fn >>= mapIO (workUpDependence path done test act)
  let dir = dirName fn++"/"
  result <- test dir modu >>= 
            maybe (readFlatCurryFile fn >>= act dir imps) return 
  updateIORef done (\fm -> addToFM fm modu result)
  return result

--- a standard test if a given file is newer than a list of other files
--- if other files do not exist, the given file is assumed to be up-to-date
--- on up-to-date files a given action is performed
obsolete :: Bool -> (String -> String -> String) -> [String -> String] 
         -> ([String] -> IO a) -> TestAct a
obsolete qu f fs action dir modu = do
  let fn  = f dir modu
      fns = map ((dir++).($modu)) fs
  ex <- doesFileExist fn
  if ex then do
               t  <- getModificationTime fn
               ns <- mapIO (isNewerThan t) fns
               if or ns
                 then do
                   unless qu $ putStrLn $ "obsolete  : " ++ fn
                   return Nothing
                 else do
                   unless qu $ putStrLn $ "up-to-date: " ++ fn
                   action fns >>= return . Just
        else do unless qu $ putStrLn ("missing   : "++ fn) 
                return Nothing
 where
   isNewerThan t file = do 
     ex <- doesFileExist file
     if not ex then return False else do
      t' <- getModificationTime file
      return (compareClockTime t t'/=GT)

fastReadImports :: FileName -> IO [String]
fastReadImports fn = do 
  cont <- readFile fn
  return (strings (takeWhile (/=']') (dropWhile (/='[') cont)))

strings :: String -> [String]
strings [] = []
strings (c:cs) | c=='"' = case break (=='"') cs of
                            (s,_:rest) -> s : strings rest
               | otherwise = strings cs

updateIORef :: IORef a -> (a -> a) -> IO ()
updateIORef ref f = do
  x <- readIORef ref
  writeIORef ref (f x)

unless :: Bool -> IO () -> IO ()
unless True  _   = return ()
unless False act = act

  

  
  