module Curry.Compiler.Config (
  module Curry.Compiler.Config,
  module Curry.Compiler.KicsSubdir) where

import System.FilePath
import System.Process
import System.Time (ClockTime)
import Data.Char
import System.Environment (getEnvironment,getArgs)
import System.Directory hiding (executable)
import System.Time
import GHC.Paths
import Data.List
import Data.Maybe (isJust)
import Data.Version
import Control.Monad

import Curry.Compiler.SafeCalls
import Curry.FlatCurry.Type (readFlat,Prog)
import Curry.Compiler.Names
import Curry.Compiler.KicsSubdir
import Curry.Files.CymakePath
import Curry.Files.KiCSPath

getOptions :: IO (Options,State)
getOptions = do 
  (opts,state) <- readConfig 
  args <- getArgs
  currypath <- getEnv "CURRYPATH"
  parsed <- parseOptions opts args
  parsedOpts <- either usage return parsed
  let addFiledir = case takeDirectory (filename opts) of "" -> id; dir -> (dir:)
  let newOpts = parsedOpts{userlibpath= addFiledir $
                                             userlibpath parsedOpts 
                                             ++ splitPath currypath}
  return (newOpts,state)    


parseOptions :: Options -> [String] -> IO (Either String Options)
parseOptions opts ("-main":x:xs) = parseOptions (opts{mainFunc=x}) xs
parseOptions opts ("-frontend":x:xs) = parseOptions (opts{frontend=x}) xs
parseOptions opts ("-userlibpath":x:xs) = 
  parseOptions (opts{userlibpath=userlibpath opts ++ splitSearchPath x}) xs
parseOptions opts ("-nouserlibpath":xs) = parseOptions (opts{userlibpath=[]}) xs
parseOptions opts ("-make":xs) = parseOptions (opts{make=True}) xs
parseOptions opts ("-nomake":xs) = parseOptions (opts{make=False}) xs
parseOptions opts ("-executable":x:xs) = parseOptions (opts{binary=Just x}) xs
parseOptions opts ("-noexecutable":xs) = parseOptions (opts{binary=Nothing}) xs
parseOptions opts ("-q":xs) = parseOptions (opts{verbosity=0}) xs
parseOptions opts ("-v":i:xs) = parseOptions (opts{verbosity=read i}) xs
parseOptions opts ("-noforce":xs) = parseOptions (opts{force=False}) xs
parseOptions opts ("-force":xs) = parseOptions (opts{force=True}) xs
parseOptions opts ("-all":"df":xs) = parseOptions (opts{pm=All DF}) xs
parseOptions opts ("-all":"bf":xs) = parseOptions (opts{pm=All BF}) xs
parseOptions opts ("-st":xs) = parseOptions (opts{pm=ST}) xs
parseOptions opts ("-i":"df":xs) = parseOptions (opts{pm=Interactive DF}) xs
parseOptions opts ("-i":"bf":xs) = parseOptions (opts{pm=Interactive BF}) xs
parseOptions opts ("-o":x:xs) = parseOptions (opts{target=Just x}) xs
parseOptions opts ("--no-interfaces":xs) = parseOptions (opts{doNotUseInterface=True}) xs
parseOptions opts@Opts{debugOptions=Right dbg} ("-d":xs) 
  = parseOptions (opts{debugOptions=Right dbg{debugOn=True},doNotUseInterface=True}) xs
parseOptions opts ("-d":xs) = switchDebug True opts (flip parseOptions xs)
parseOptions opts ("--debug":xs) = parseOptions opts ("-d":xs)
parseOptions opts@Opts{debugOptions=Right dbg} ("--debugger":d:xs) 
  = parseOptions opts{debugOptions=Right dbg{debugtool=d}} xs
parseOptions opts ("--debugger":d:xs) = 
  putDebugTool d opts (flip parseOptions xs)
parseOptions opts []    = return $ Right opts
parseOptions opts [x]   = return $ Right (opts{filename=x,mainModule=takeBaseName x})
parseOptions _    (x:_) = return $ Left ("unrecognized option: "++x)

usage problem = do
  putStrLn problem
  putStrLn "Usage: kics [options] filename"
  putStrLn "option         | meaning"
  putStrLn "-main          | name of main function "
  putStrLn "-frontend      | frontend binary"
  putStrLn "-userlibpath   | path to curry libraries"
  putStrLn "-nouserlibpath | only standard curry libraries"
  putStrLn "-make          | chase imported modules"
  putStrLn "-nomake        | do not chase imported modules"
  putStrLn "-executable <n>| create executable binary named <n>"
  putStrLn "-noexecutable  | do not create executable"
  putStrLn "-v <n>         | set verbosity level to n, e.g., -v 3"
  putStrLn "-q             | scarce output"
  putStrLn "-force         | force recompilation"
  putStrLn "-noforce       | do not force recompilation"
  putStrLn "-all df        | print all solutions depth first"
  putStrLn "-all bf        | print all solutions breadth first"
  putStrLn "-st            | print solutions as search tree"
  putStrLn "-i df          | interactively show solutions depth first"
  putStrLn "-i bf          | interactively show solutions breadth first"
  putStrLn "-o             | name of output file"
  putStrLn "-d             | turn on debug mode"
  putStrLn "--debugger <n> | use debug tool <n>"
  error "compilation aborted"



data DebugOptions = 
  DebugOptions{debugVersion::Version,
               debugOn :: Bool,
               oracleTransformation,
               bioTransformation,
               debugtool :: String,
               oracleLibPath :: [String]} deriving Show

data Options = Opts{ filename, mainFunc, mainModule, 
                     frontend, ghcOpts, stdLibDir
                       :: String,
                     target,binary :: Maybe String,
                     userlibpath, done :: [String],
                     verbosity :: Int,
                     make, eval, 
                     force, doNotUseInterface :: Bool, 
                     debugOptions :: Either Bool DebugOptions,
                     extCons,hasData :: Bool,
                     pm :: PresentationMode,
                     extData, extFuncs :: [String],
                     extInsts :: [(String,[ProvidedInstance])],
                     toInclude :: ([String],String)} deriving Show

addFileDirToPath :: String -> [String] -> [String]
addFileDirToPath fn = case takeDirectory fn of "" -> id; dir -> (dir:)

libpath :: Options -> [String]
libpath opts@Opts{userlibpath=up,filename=fn,stdLibDir=std,
                  debugOptions=Right DebugOptions{debugOn=True,oracleLibPath=o}} = 
  addFileDirToPath fn $ up ++ (std:o)
libpath opts@Opts{userlibpath=up,filename=fn,stdLibDir=std} = 
  addFileDirToPath fn $ up ++ [std]  

executable :: Options -> Bool
executable = isJust . binary

debug :: Options -> Bool
debug = either (const False) debugOn . debugOptions

debugOff :: Options -> Options
debugOff opts@Opts{debugOptions=Right dbg} =
  opts{debugOptions=Right dbg{debugOn=False}}

cmdLibpath :: Options -> String
cmdLibpath opts = toPathList (libpath opts)

currentModule :: Options -> String
currentModule opts = strip (filename opts)
  where
   strip s = case break isPathSeparator s of
               (s',[]) -> s'
               (_,_:s')  -> strip s'

hasExtData,hasExtInsts, hasExtFuncs :: Options -> Bool
hasExtData opts = 
  not (null (extData opts)) || any (elem Declaration . snd) (extInsts opts)
hasExtInsts opts = 
  not (null (filter (any (/=Declaration) . snd) (extInsts opts)))
hasExtFuncs opts = not (null (extFuncs opts))

defaultOpts = Opts {filename="", mainFunc= "main", mainModule="Main",
      target = Nothing,
      frontend="cymake",
      stdLibDir = "",
      userlibpath=[],
      ghcOpts=" -fcontext-stack=50 ",
      done=[], 
      make=True, 
      binary=Nothing, 
      verbosity=1,
      eval=True,
      force=False,
      debugOptions = Left False,
      doNotUseInterface=False,
      extCons=False,
      hasData=False,
      pm=Interactive DF,
      extData=[],
      extInsts=[],
      extFuncs=[],
      toInclude=([],"")}

kicsrc home = unpath [home,".kicsrc"]

data ChoiceMode = OrBased | CTC deriving (Eq,Read,Show)

data SearchMode = DF | BF 

instance Show SearchMode where
  show DF  = "depth first"
  show BF  = "breadth first"

data PresentationMode = First SearchMode
                      | All SearchMode 
                      | Interactive SearchMode
                      | ST 

instance Show PresentationMode where
  show (All x) = "all solutions "++show x
  show (Interactive x) = "interactive "++show x
  show (First x) = "first solution "++show x
  show ST  = "search tree"

data State = State {home,rts,cmdLineArgs :: String,
                    files :: [(Bool,String)],
                    time :: Bool} deriving Show

defaultState home = State {home=home,
                           rts=" -H400M ",
                           cmdLineArgs="",
                           files=[],
                           time=False}

readPMode s = readPM (words (map toLower s))
    where
      readPM ("interactive":ws) = Interactive (readSM ws)
      readPM ("all":"solutions":ws) = All (readSM ws)
      readPM ["search","tree"] = ST
      
      readSM ["depth","first"] = DF
      readSM ["breadth","first"] = BF

ghcCall :: Options -> String
ghcCall opts@Opts{filename=fn} = 
  callnorm (ghc
             ++makeGhc (make opts)
             ++" -i"++show (toPathList 
                             (pathWithSubdirs $ libpath opts))++" "
             ++kicsSubdirPathToFile
             ++linkOpts
             ++ghcOpts opts
             ++verboseGhc (verbosity opts >= 2) 
             ++ghcTarget opts
             ++" "++show fn)
      
  where
    linkOpts = ""
     -- | debug opts = linkLib++" -L"++installDir++"/src/lib/ "
     -- | otherwise  = ""
     --linkLib  | eval opts  = " -ldyncoracle "
     --         | otherwise  = " -lcoracle "

    verboseGhc True  = ""
    verboseGhc False = " -v0 "

    ghcTarget Opts{binary=Nothing} = ""
    ghcTarget Opts{binary=Just t} = " -o "++show t

    makeGhc True = " --make "
    makeGhc False = ""

    kicsSubdirPathToFile = case takeDirectory fn of
                             "" -> ""
                             path -> " -i"++show (addKicsSubdir path)++" "


cyCall opts = callnorm $ frontend opts++" -e " ++
                         unwords (map (("-i"++) . show) (libpath opts))

callnorm s = unwords (words s) ++ " "

cymake opts = do
  safeSystem (verbosity opts >= 3) 
                         (cyCall opts ++ show (filename opts)
                             ++ if verbosity opts >= 3 then "" else " 1>/dev/null ")

runCurryCmd :: String -> Options -> Safe IO ()
runCurryCmd cmd opts = do
  let args = ((if force opts then ("-f":) else id) $
              (if verbosity opts < 2 then ("-q":) else id)
              [dropExtension $ filename opts])
  put 5 opts (cmd ++ " " ++ show args)
  safeIO $ do
    hdle <- runProcess cmd
               args
               Nothing
               (Just [("CURRYPATH",cmdLibpath opts)])
               Nothing
               Nothing
               Nothing
    waitForProcess hdle
  return ()

prophecy opts@Opts{debugOptions=Right dbg} = do
  put 5 opts "calling prophecy" 
  runCurryCmd (oracleTransformation dbg) opts

mkstrict opts@Opts{debugOptions=Right dbg} = do
  put 5 opts "calling mkstrict" 
  runCurryCmd (bioTransformation dbg) opts

readConfig = do
   home <- getEnv "HOME"
   catch (readFile (kicsrc home) >>= getConfigs home) 
         (\_ -> getConfigs home "")

writeConfig opts state = do
  home <- getEnv "HOME"
  writeFile (kicsrc home)
    (wLibPath++wPM++wEval++wTime ++wRTS)
  where
    wLibPath  = setting 1 (\o-> toPathList $ tail $ userlibpath o)
    wPM       = setting 2 (show . pm)
    wEval     = setting 3 (show . eval)
    wTime     = inState 4 (show . time)
    wRTS      = inState 5 rts

    setting n f = entry n (f opts)
    inState n f = entry n (f state)
    entry n s   = (configs!!(n-1)) ++ "="++s++"\n\n"


mkTags = [(toPathList . userlibpath),(show . pm)]

getDebugOptions :: Options -> IO (Either Bool DebugOptions)
getDebugOptions opts = do
  put' 5 opts "looking for debugger"
  let dbg = "KiCS-debugger"
  answer <- readProcess ghc_pkg ["list",dbg] ""
  put' 5 opts ("ghc-pkg: "++answer)
  if (not $ isInfixOf dbg answer)
   then put' 5 opts "no debugger installed" >> return (Left True)
   else do
     dir <- getKiCSDataDir
     put' 5 opts "getting debugger config"
     dopts <- readProcess ghc ["-e","main",dir </> "debugOptions.hs"] ""
     let (v,p,m,o) = read dopts
     put' 5 opts ("config is: "++dopts)
     return (Right DebugOptions{debugVersion=read v,
                               debugOn=False,
                               debugtool="Observations",
                               oracleTransformation=p,
                               bioTransformation=m,
                               oracleLibPath=o})

updDebugOpts :: (DebugOptions -> DebugOptions) -> Options -> (Options -> IO a) -> IO a 
updDebugOpts _ opts@Opts{debugOptions=Left True} cont = do
  putStrLn "debugger not installed"
  cont opts
updDebugOpts f opts@Opts{debugOptions=Left False} cont = do
  dopts <- getDebugOptions opts
  case dopts of
    Left _ -> cont opts{debugOptions=dopts}
    Right dbg -> cont opts{debugOptions=Right (f dbg)}
updDebugOpts f opts@Opts{debugOptions=Right dbg} cont =
    cont opts{debugOptions=Right (f dbg)}

switchDebug flag = updDebugOpts (\ dbg -> dbg{debugOn=flag})
putDebugTool tool = updDebugOpts (\ dbg -> dbg{debugtool=tool})


getConfigs home cfgs | cfgs == cfgs = do
  punkt <- getCurrentDirectory
  std   <- getKiCSLibDir
  cymake_call <- getCymake
   
  let readOpts = selOpts (entries cfgs)

      opts = defaultOpts
            {userlibpath = let up = readSetting userlibpath splitPath 1
                             in punkt : up,
             stdLibDir   = std,
             pm          = readSetting pm readPMode 2,
             frontend    = cymake_call,
             eval        = readSetting eval read 3,
             force       = False}
      readSetting f r n = maybe (f defaultOpts) r (readOpts!!(n-1))

      defaultsS = defaultState home
      state = defaultsS
               {time = readSSet time read 4,
                rts  = readSSet rts  id   5}
      readSSet f r n = maybe (f defaultsS) r (readOpts!!(n-1))

  return (opts,state)

entries s = equations (lines s)
  where
    equations [] = []
    equations (x:xs) = case break (=='=') x of
      (l,_:r) -> (l,r):equations xs
      _       -> equations xs

selOpts cfgs = map (selTag cfgs) configs

configs = 
 ["Libraries",
  "PresentationMode",
  "Eval",
  "Time",
  "RunTimeSettings"]

selTag [] _ = Nothing
selTag ((t,v):xs) s = 
  if map toLower t==map toLower s 
    then Just v
    else selTag xs s


paths s = case break (==':') s of
           ("","") -> []
           (w,"") -> [w]
           ("",_:ws) -> paths ws
           (w,_:ws) -> w : paths ws

getModTime fn = safeIO (do 
                   ex<-doesModuleExist fn
                   if ex then getModuleModTime fn else return (TOD 0 0))

safeReadFlat :: Options -> String -> Safe IO Prog
safeReadFlat opts s = do
    fs <- safeIO (findFileInPath s (libpath opts))
    fn <- warning s (cmdLibpath opts) fs
    mprog <- safeIO $ readFlat fn
    maybe (fail $ "file not found: "++fn) return mprog
   
warning fn path [] = fail ("error: file "++fn++" not found in path "++path)
warning _ _  (f:fs) = do
  mapM_ (safeIO . putStrLn) 
        (map (\f' -> "further file found (but ignored) "++f'
                   ++" taking "++f++" instead") fs)
  return f


----------------------------------------------
-- external definitions
----------------------------------------------

-- what is provided by external files

data ProvidedInstance = 
  Declaration | Show | Read | BaseCurry | Curry deriving (Eq,Ord,Read,Show)

data Provided = ForType String (Maybe [ProvidedInstance])
              | ForFunction String 
              | SomeFunctions
              | Pragma String
              deriving (Eq,Read,Show)

-- external specifications have to look like this:
-- fortype <typename> [definition|nodef] instances <instname>*
-- extfunc <funcname>

put' :: Int -> Options -> String -> IO ()
put' i Opts{verbosity=j} s | i>j  = return ()
                           | i<=j = putStrLn s

put :: Int -> Options -> String -> Safe IO ()
put i opts s = safeIO $ put' i opts s

getExternalSpecFileName :: Options -> String -> Safe IO (Maybe FilePath)
getExternalSpecFileName opts p = do
    specs <- safeIO $ findFileInPath 
                        (externalSpecName (p `withoutSubdir` currySubdir)) 
                        (libpath opts)
    if null specs 
      then return Nothing 
      else warning "" "" specs >>= return . Just


readExternalSpec :: Options -> String -> Safe IO Options
readExternalSpec opts p = do
    mspecFile <- getExternalSpecFileName opts p
    case mspecFile of
      Nothing -> return opts 
      Just specFile -> do
        spec <- safeIO (readModule specFile)
        put 5 opts "reading external specification"
        let [(specs,stringToInclude)] = reads spec
            newOpts = foldr insertP 
                            opts{toInclude=([],stringToInclude)} 
                            specs
        safeIO (seq newOpts (return ()))
        put 5 opts "external specification read"
        return newOpts
  where
    insertP SomeFunctions         opts = opts{extFuncs = ""     : extFuncs  opts}
    insertP (ForFunction f)       opts = opts{extFuncs = f      : extFuncs  opts}
    insertP (ForType t Nothing)   opts = opts{extData  = t      : extData   opts}
    insertP (ForType t (Just is)) opts = opts{extInsts = (t,is) : extInsts  opts}
    insertP (Pragma p)            opts = let (ps,inc) = toInclude opts
                                          in opts{toInclude=(p:ps,inc)}
    
getExternalSpecModTime :: Options -> String -> Safe IO ClockTime
getExternalSpecModTime opts p = do
  mspecFile <- getExternalSpecFileName opts p
  case mspecFile of
   Nothing       -> return (TOD 0 0)
   Just specFile -> safeIO $ getModuleModTime specFile


baseName f = case reverse f of
  'y':'r':'r':'u':'c':'.':f'     -> reverse f'
  'y':'r':'r':'u':'c':'l':'.':f' -> reverse f'
  _ -> f

getEnv :: String -> IO String
getEnv s = getEnvironment >>= maybe (return "") return . lookup s
