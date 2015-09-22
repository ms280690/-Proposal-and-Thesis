module Main where

import Data.Maybe
import Data.List
import Data.Char
import System.Cmd
import System.IO
import System.Directory (doesFileExist)
import Control.Monad (unless,when)
import System.FilePath
import System.Console.Readline

import Curry.Files.KiCSPath

import Curry.Compiler.Config
import Curry.Compiler.CurryToHaskell
import Curry.Compiler.SafeCalls
import Curry.Compiler.Names
import Curry.Compiler.ShowFlatCurry

import Curry.FlatCurry.Type
import Curry.FlatCurry.Goodies

allFiles = map snd . files
loadedFiles = map snd . filter fst . files

separate s = concat . intersperse s . filter (not . null)

welcome = 
 ["         _               _           _            _"
 ,"        /\\_\\            /\\ \\       /\\ \\          / /\\"
 ,"       / / /  _         \\ \\ \\     /  \\ \\        / /  \\"
 ,"      / / /  /\\_\\       /\\ \\_\\   / /\\ \\ \\      / / /\\ \\__"
 ,"     / / /__/ / /      / /\\/_/  / / /\\ \\ \\    / / /\\ \\___\\"
 ,"    / /\\_____/ /      / / /    / / /  \\ \\_\\   \\ \\ \\ \\/___/"
 ,"   / /\\_______/      / / /    / / /    \\/_/    \\ \\ \\"
 ,"  / / /\\ \\ \\        / / /    / / /         _    \\ \\ \\  The"
 ," / / /  \\ \\ \\   ___/ / /__  / / /________ /_/\\__/ / /  Kiel"
 ,"/ / /    \\ \\ \\ /\\__\\/_/___\\/ / /_________\\\\ \\/___/ /  Curry"
 ,"\\/_/      \\_\\_\\\\/_________/\\/____________/ \\_____\\/  System"
 ,"","Version "++kicsVersion,""]
 

compileCall CTC     = "kics -make "
compileCall OrBased = "kics -or -make "

compileModule file choiceMode = system (compileCall choiceMode++file)

-------------------------------------
-- read history from file
-------------------------------------

historyFile = "kicsi.hist"

readHistory :: IO ()
readHistory = do 
  exHist <- doesFileExist historyFile
  unless (not exHist) 
         (readFile historyFile >>=  addLineToHistory 1 . lines)
  where
    addLineToHistory _ [] = return ()
    addLineToHistory n (s@(':':_):xs) = addHistory s >> addLineToHistory n xs
    addLineToHistory n (s:xs)         = 
      addHistory ("{-"++show n++"-} "++s) >>
      addLineToHistory (n+1) xs

main = do 
  readHistory
  home <- getEnv "HOME"
  (options,state) <- getOptions 
  mapM_ (safe . put 1 options) welcome
  let files = case filename options of
               "" -> ["Prelude"] 
               fn -> [fn]
  load files state options

interactive state opts = do
  mline <- readline (separate "," (loadedFiles state) ++"> ")
  case mline of
    Just line -> addHistory line >>
                 interactiveMenue (words line) state opts
    Nothing   -> return ()

interactiveMenue [] state opts = interactive state opts
interactiveMenue (cmd:cmds) state opts = 
  case map toLower cmd of
    ":load" -> load cmds state opts 
    ":l"    -> load cmds state opts 
    ":add"  -> load (allFiles state++cmds) state opts 
    ":a"    -> load (allFiles state++cmds) state opts 
    ":set"  -> setMenue cmds state opts
    ":reload" -> load (allFiles state) state opts
    ":r"      -> load (allFiles state) state opts
    ":type" -> getType (unwords cmds) state opts
    ":t"    -> getType (unwords cmds) state opts
    ":quit" -> return ()
    ":q"    -> return ()
    ":help" -> help state opts
    ":h"    -> help state opts
    ":?"    -> help state opts
    ":info" -> info cmds (loadedFiles state) state opts
    ":i"    -> info cmds (loadedFiles state) state opts
    ":save" -> writeConfig opts state >> interactive state opts
    ":s"    -> writeConfig opts state >> interactive state opts
    ':':'!':c -> safe (safeSystem False (unwords (c:cmds))) >> interactive state opts
    ':':_   -> putStrLn "unknown command, type :? for help" >> 
               interactive state opts
    _       -> requestExpr state opts (unwords (cmd:cmds))

setMenue [] state opts = do  
  putStrLn "options"
  putStrLn "-------"
  putStrLn $ "search mode:          " ++ (show (pm opts))
  putStrLn $ "timing:               " ++ onOff (time state)
  putStrLn $ "debug:                " ++ onOff (debug opts) 
                                      ++ either (const "") 
                                                ((" -- "++) . debugtool) 
                                                (debugOptions opts)
  putStrLn $ "evaluation mode:      " ++ evalMode (eval opts)
  putStrLn $ "verbosity level:      " ++ show (verbosity opts)
  putStrLn $ "recompilation:        " ++ if force opts then "always (+f)" 
                                                       else "only if older (-f)"  
  putStrLn "\npaths and commands"
  putStrLn "------------------"
  putStrLn   $ "command line options:    " ++ cmdLineArgs state
  putStrLn   $ "run time settings:       " ++ rts state
  putStrLn   $ "ghc compiler options:    " ++ ghcOpts opts
  putStrLn   $ "paths to user libraries: " 
  mapM_ putPath (tail $ userlibpath opts)
  interactive state opts
   where
    putPath p = putStr "                      " >> putStrLn p

setMenue (opt:vals) state opts = do
  case map (map toLower) (opt:vals) of
   ["depth","first"] -> interactive state (newSm opts DF)
   ["df"] -> interactive state (newSm opts DF)
   ["breadth","first"] -> interactive state (newSm opts BF)
   ["bf"] -> interactive state (newSm opts BF)
   ["all","solutions"] -> interactive state (newPm opts (All DF))
   ["all"] -> interactive state (newPm opts (All DF))
   ["first","solution"] -> interactive state (newPm opts (First DF))
   ["first"]            -> interactive state (newPm opts (First DF))
   ["interactive"] -> interactive state (newPm opts (Interactive DF))
   ["i"] -> interactive state (newPm opts (Interactive DF))
   ["search","tree"] -> interactive state opts{pm=ST}
   ["st"] -> interactive state opts{pm=ST}
   ["path+",_] -> let (thisDir:oldPath)=userlibpath opts
     in interactive state opts{userlibpath=thisDir:head vals:oldPath}
   ["path",_] -> let thisDir = head (userlibpath opts)
     in interactive state opts{userlibpath=[thisDir,head vals]}
   ["verbosity",i] | all isDigit i -> interactive state opts{verbosity=read i}
   ["v",i] | all isDigit i -> interactive state opts{verbosity=read i}
   ("command":_) -> interactive state{cmdLineArgs=unwords vals} opts
   ("cmd":_) -> interactive state{cmdLineArgs=unwords vals} opts
   ("rts":_) -> interactive state{rts=' ':unwords vals++" "} opts
   ("rts+":_)-> interactive state{rts=rts state++' ':unwords vals++" "} opts
   ("ghc":_) -> interactive state opts{ghcOpts=' ':unwords vals++" "}
   ("ghc+":_) -> interactive state 
                   opts{ghcOpts=ghcOpts opts++' ':unwords vals++" "}
   ["debugger",debugTool] -> putDebugTool (head vals) opts
                                          (interactive state) 
   ['+':'+':setting] -> longSetting True  state opts setting
   ['-':'-':setting] -> longSetting False state opts setting
   (('+':s):sets) -> shortSettings True  state opts (concat (s:sets))
   (('-':s):sets) -> shortSettings False state opts (concat (s:sets))
   _ -> putStrLn ("invalid setting. Example \":set breadth first\" to " ++
                  "set search strategy to breadth first") >> 
        interactive state opts

longSetting flag state opts "debug"     = 
  switchDebug flag opts (interactive state) 
longSetting flag state opts "time"      = do
  warn state{time=flag} opts 
  interactive state{time=flag} opts
longSetting flag state opts "eval"      = do
  warn state opts{eval=flag}
  interactive state opts{eval=flag}
longSetting flag state opts "make"      = interactive state opts{make=flag}
longSetting flag state opts "force"     = interactive state opts{force=flag}
longSetting _    state opts _           = putStrLn "invalid setting." >> interactive state opts

shortSettings _    state opts [] = do
  warn state opts
  interactive state opts
shortSettings flag state opts ('t':settings) = do
  putStrLn $ "setting time " ++ onOff flag 
  shortSettings flag state{time=flag} opts settings 
shortSettings flag state opts ('-':settings) = 
  shortSettings False state opts settings 
shortSettings flag state opts ('+':settings) = 
  shortSettings True  state opts settings 
shortSettings flag state opts (c:settings) = do
    o <- newOpts c
    shortSettings flag state o settings 
  where
    newOpts 'd' = putStrLn ("setting debbug " ++ onOff flag) >>
                  switchDebug flag opts return  
    newOpts 'e' = putStrLn ("setting evaluation mode to " ++ evalMode flag) >>
                  return opts{eval=flag}
    newOpts 'm' = putStrLn ("setting make " ++ onOff flag) >>
                  return opts{make=flag}
    newOpts 'f' = putStrLn ("setting recompilation to " ++ forceMode flag) >>
                  return opts{force=flag}
    newOpts c   = putStrLn ("unknown short option: "++show c) >>
                  putStrLn ("  (long options are set with \"++\" and \"--\", e.g.,\ 
                            \ \":set ++time\"") >>
                  return opts

onOff True  = "on"
onOff False = "off" 
evalMode True  = "interpreted (+e)"
evalMode False = "compiled (-e)"
forceMode True  = "always (+f)"
forceMode False = "only if older (-f)"

warn state opts = 
  when (time state && eval opts) 
       (putStrLn "warning: for benchmarking you should use +t together with -e")


help state opts = do
  mapM_ putStrLn  
    [":load              load a (number of) file(s)"
    ,":set <option>      set a KiCSi <option>"
    ,":set               see current KiCSi options"
    ,":reload            reload current files" 
    ,":type <expression> show type of <expression>"
    ,":quit              leave KiCSi"
    ,":help              this message"
    ,":!                 system command"]
  interactive state opts
  
info _ [] state opts = interactive state opts
info x (f:fs) state opts = do 
  safe (do 
	p <- safeReadFlat opts (f++".fint")
	safeIO (putStrLn (showFlatProg p))
        safeIO (putStrLn ""))
  info x fs state opts

newSm opts@Opts{pm=All _} x = opts{pm=All x}
newSm opts@Opts{pm=Interactive _} x = opts{pm=Interactive x}
newSm opts@Opts{pm=ST} x = opts{pm=Interactive x}

newPm opts@Opts{pm=ST} x = opts{pm=x}
newPm opts@Opts{pm=All x}   (Interactive _) = opts{pm=Interactive x}
newPm opts@Opts{pm=Interactive x} (All _)   = opts{pm=All x}
newPm opts@Opts{pm=All x}         (First _) = opts{pm=First x}
newPm opts@Opts{pm=Interactive x} (First _) = opts{pm=First x}
newPm opts@Opts{pm=First x} (Interactive _) = opts{pm=Interactive x}
newPm opts@Opts{pm=First x} (All _)         = opts{pm=All x}
newPm opts _ = opts

getType :: String -> State -> Options -> IO ()
getType expr state opts = do 
   t <- safe $ do
          requestFile <- genReqModule (loadedFiles state) expr
          cymake opts{filename=requestFile}
          p <- safeReadFlat opts reqModuleFile
          let (f:_) = filter ((==mainExpr) . snd . funcName) (progFuncs p)
          return (funcType f)
   maybe (return ()) (putStrLn . showCurryType snd False) t
   interactive state opts
               
load [] state opts = interactive state opts
load xs state opts = do
  done <- startCompilations opts{binary=Nothing} fs
  interactive state{files=map (isLoaded done) (nub fs)} opts
  where
    fs = map baseName xs
    isLoaded done f = (elem f done,f)

                  
toMode _ ["or"]  = OrBased
toMode _ ["ctc"] = CTC
toMode m _ = m

mainExpr = "expression"


requestExpr state opts line = do 
  safe $ do
    let ls = loadedFiles state
        mainMod = if null ls then "Prelude" else head ls
    --safeSystem (verbosity opts >= 5) 
    --           ("rm -f request Request.fcy "++reqMod ++".o ") 
    requestFile <- genReqModule (loadedFiles state) line
    let compileOpts = (opts{binary=Just "request",
                            filename=requestFile,
                            mainFunc=mainExpr,
                            mainModule = mainMod,
                            make=True})  
    startCompilation compileOpts
    let call = timing state (requestCall state opts)
    when (not (eval opts))
         (safeSystem (verbosity opts >= 3) 
                     (ghcCall opts{binary=Just (inKicsSubdir "request"),
                                   filename=inKicsSubdir "request.hs"}))
    when (verbosity opts >= 2 || not (eval opts))
         (put 1 opts ("starting evaluation of "++line))
    safeSystem (verbosity opts >= 3) call
    when (debug opts) $ do
         mkstrict compileOpts{filename=reqModuleName,
                              make=True,
                              userlibpath=inKicsSubdir ".":userlibpath compileOpts}
         genDebugModule opts{mainModule=mainMod} (loadedFiles state) line
         safeSystem (verbosity opts >= 5) 
                    (ghcCall $ debugOff opts{target=Just (inKicsSubdir "debug"),
                                  make=True,ghcOpts=ghcOpts opts++" -O2 ", 
                                  filename=inKicsSubdir debugModuleName})
         safeSystem (verbosity opts >= 5) 
                    (inKicsSubdir "debug")
 
  interactive state opts

-- in ghc 6.10 we cannot combine make with "-e"
-- In order to avoid link errors we somehow need 
-- to start make before calling "-e", but it is not yet clear
-- how to avoid generating a binary.
requestCall state opts@Opts{eval=True} = 
  ghcCall opts{make=False,
               ghcOpts=ghcOpts opts++ " +RTS "++ rts state ++ " -RTS -e main ",
               filename=inKicsSubdir "request.hs"}
requestCall state _ = (inKicsSubdir "request"++" "++cmdLineArgs state++" +RTS "++rts state)

reqModuleName = "Request"
reqModuleFile = replaceExtension (inKicsSubdir reqModuleName) ".fcy"

genReqModule fs line = do
  let filename = destination False Nothing (replaceExtension reqModuleName ".curry")
  safeIO (writeKicsFile filename (imports fs++"\n\n"++mainExpr++" = "++ line))
  return filename
   
  

timing (State{time=True}) s = "time "++s
timing _ s = s


unqualMain s = examine (groupBy (\x y->isExtAlpha x && isExtAlpha y) s)
  where
    examine (_:".":"main":xs) = examine xs
    examine ("main":_) = True
    examine (_:xs) = examine xs
    examine [] = False

isExtAlpha '_' = True
isExtAlpha '\'' = True
isExtAlpha c = isDigit c || isAlpha c

reqMod = modName reqModuleName

imports :: [String] -> String
imports = concatMap ("\nimport "++) 

------------------------------
-- triggering the debug tool
------------------------------

debugModuleName = "debug.hs"
genDebugModule Opts{debugOptions=Right DebugOptions{debugtool=tool},mainModule=mod} fs line = do
  let filename   = destination False Nothing debugModuleName
      modImports = imports $ "Curry.Debugger.DebugMonad":
                          ("Curry.Debugger.Tools."++tool++"."++"Monad"):
                          map mkStrictName ((reqModuleName++" as S"):fs)
      modCont = modImports ++
        "\n\nmain = do\n\
        \  run (S.strict_"++mainExpr++") \""++mod++"\""
  safeIO (writeKicsFile filename modCont)
           
         
 
