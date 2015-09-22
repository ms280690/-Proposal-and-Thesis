module Curry.Compiler.CurryToHaskell where 

import Data.List
import Data.Char
import Data.Maybe
import Control.Monad
import System.FilePath

import Curry.FlatCurry.Type
import Curry.FlatCurry.Goodies hiding (consName)
import qualified Curry.Compiler.FunctionalProg as C
import Curry.Compiler.ShowFunctionalProg
import Curry.Compiler.PreTrans hiding (nub,pre)
import Curry.Compiler.Simplification ( simplifyProg )
import Curry.Compiler.SafeCalls
import Curry.Compiler.Brace
import Curry.Compiler.Config as Config
import Curry.Compiler.Names 
             (modName,dbgModName,funcHsName,externalSpecName,
              elimInfix,funName,functionName,constructorName)
import qualified Curry.Compiler.Names as N

--import Debug.Trace
--trace' x = trace (show x) x

-------------------------------
-- main compilation routine
-------------------------------

-- call this function to start compilation
-- arguments: record of Type Options as defined 
-- in Config.hs 

startCompilations :: Options -> [String] -> IO [String]
startCompilations _ [] = return []
startCompilations opts fs = 
  compilations fs opts{done=[],mainModule=head fs}

compilations ::  [String] -> Options -> IO [String]
compilations [] opts = return (done opts)
compilations (f:fs) opts = 
  safe (startCompilation opts{filename=f}) >>=
  compilations fs . maybe opts id

startCompilation :: Options -> Safe IO Options
startCompilation opts0@Opts{filename=fn,userlibpath=up} = do 
  let (dir,file) = splitFileName fn
      opts=if   null dir 
           then opts0{filename=file} 
           else opts0{filename=file,userlibpath=dir:up}
  newOpts <- callFrontend opts 
  visited <- compile newOpts >>= return . done 
  when (make opts) $ do
    put 2 opts "calling ghc"
    ghcProgram False newOpts (funcHsName (filename newOpts))
  return newOpts{done=visited}

-- compile not only returns the current Options 
-- but also a flag whether no significant changes
-- have been made. A significant change forces
-- recompilation of dependent modules.
compile :: Options -> Safe IO Options
compile opts = do
  newOpts <- getFlatCurryFileName opts
  old <- notUptodate newOpts
  if old || force opts || executable opts 
   -- possible improvement: generate only Main.hs if up-to-date 
   -- (but note that main module transformed differently with executable)
   then process newOpts >>= makeImports 
   else skip    newOpts >>= makeImports 

process :: Options -> Safe IO (String,[String],Options)
process opts0@(Opts{filename=fn}) = do
  prog <- safeReadFlat opts0 (replaceExtension fn ".fcy")
  unless (executable opts0 && fn=="Request")
         (put 1 opts0 ("processing: "++progName prog))
  opts <- readExternalSpec opts0 fn
  unless (null $ extData  opts) 
         (put 5 opts "external data declarations found")
  unless (null $ extInsts opts) 
         (put 5 opts "external instance declarations found")
  unless (null $ extFuncs opts) 
         (put 5 opts "external function declarations found")
  applyFlatTransformations opts prog >>= generateHaskellFiles opts
  return (progName prog,progImports prog,opts0)

-- only read beginning of interface file, return name and list of imports 
skip :: Options -> Safe IO (String,[String],Options)
skip opts = do
    let fname = if doNotUseInterface opts 
                then replaceExtension (filename opts) ".fcy"
                else replaceExtension (filename opts) ".fint"
    fn <- safeIO (findFileInPath fname (libpath opts)) >>=
          warning fname (cmdLibpath opts) 
    cont <- safeIOSeq (readModule fn)
    let [("Prog",rest)] = lex cont
        [(name,rest')]  = reads rest
        [(imps,_)]      = reads rest'
    put 3 opts ("up-to-date: "++name)
    return (name,imps,opts)

makeImports :: (String,[String],Options) -> Safe IO Options
makeImports (name,imps,opts@(Opts{filename=fn})) = do
  if (make opts) 
   then do
    impOpts <- foldCompile imps opts{binary=Nothing,target=Nothing}
    return impOpts{done=name : done impOpts}
   else return opts{done=name : done opts}

---------------------------------------------------------------------------------
-- sub routines of compilation
---------------------------------------------------------------------------------

callFrontend opts@(Opts{filename=givenFile}) = do
  let lib = libpath opts
  foundCurry <- safeIO (findFileInPath (replaceExtension givenFile ".curry") lib)
  foundSources <- if null foundCurry 
                   then safeIO (findFileInPath (replaceExtension givenFile ".lcurry") lib)
                   else return foundCurry
  when (make opts) $ do
    unless (null foundSources) $ do 
      put 2 opts ("calling frontend for " ++ head foundSources)
      if debug opts then prophecy opts else cymake opts
  return (if debug opts then opts{filename=dbgModName givenFile} else opts)

getFlatCurryFileName opts@(Opts{filename=basename}) = do
  let lib = libpath opts
      fn  = replaceExtension basename ".fcy" 
  foundFiles <- safeIO (findFileInPath fn lib)
  foundFile <- warning fn (toPathList lib) foundFiles
  let foundBasename = dropExtensions foundFile
  return (opts{filename=foundBasename})

notUptodate opts@(Opts{filename=foundBasename}) 
  | isPrefixOf (stdLibDir opts) founddir  ||
    either (const False) 
           (any (`isPrefixOf` founddir) . oracleLibPath) 
           (debugOptions opts)
  = return False
  | otherwise = do
      tSource1     <- getModTime (replaceExtension foundBasename ".fcy")
      tSource2     <- getExternalSpecModTime opts foundBasename
      let dest = destination True (target opts) (funcHsName foundBasename)
      tDestination <- getModTime dest
      return (tSource1 > tDestination || tSource2 > tDestination)
  where
   founddir = takeDirectory foundBasename

applyFlatTransformations opts prog = do
  let auxNames = generateAuxNames (progFuncs prog)
      mexprog = if executable opts then addExec auxNames opts prog 
                                   else Left prog
  exprog <- either return fail mexprog 
  let suffix = flip replaceExtension $ if doNotUseInterface opts
               then ".fcy"
               else ".fint"
  interfaces <- mapM (safeReadFlat opts . suffix) (progImports exprog) 
  (globals,locProg) <- safeIOSeq (return (splitGlobals exprog))
  let liftedProg = noCharCase (liftCases True (simplifyProg locProg))
  --disAmb <- disambiguate interfaces ceprog
  unless (null globals) 
         (put 5 opts 
            ("module contains "++show (length globals)
                               ++" global declaration(s)"))
  return (globals,liftedProg,interfaces,auxNames)

generateHaskellFiles opts (globals,prog,interfaces,auxNames) = do
  let typeMapping = makeTypeMap (prog:interfaces)
      modules = addGlobalDefs opts globals $
                  transform typeMapping auxNames opts prog
  put 3 opts "generating Haskell"
  mapM (writeProgram opts) modules
  return (haskellFiles opts (progName prog))

writeProgram opts (filename,unqualified,prog) = do
  put 3 opts ("writing "++ filename)
  let printOpts = defaultPrintOptions{unqual=unqualified,
                                      include=snd (toInclude opts),
                                      pragma=fst (toInclude opts)}
  safeIO (writeKicsFile filename (showProgOpt printOpts prog))
  put 3 opts (filename ++ " written")

ghcProgram skipping opts fn = 
  unless (eval opts && executable opts)  $ do
      found <- safeIO (findFileInPath fn (libpath opts))
      let hsFile = head found
          ghc    = safeSystem (verbosity opts >= 2) 
                     (ghcCall opts{make=True,filename=hsFile,target=Nothing})
          shFile = drop 2 (reverse hsFile)
          oFile  = reverse ('o':shFile)
          hiFile = reverse ('i':'h':shFile)
      unless (null found) $
         if skipping 
           then do
                  ex <- safeIO (mapM doesModuleExist [oFile,hiFile])
                  unless (and ex) ghc
           else ghc

foldCompile :: [String] -> Options -> Safe IO Options
foldCompile [] opts     = return opts
foldCompile (f:fs) opts 
  | elem f (done opts) = foldCompile fs opts
  | otherwise          = compile (opts{filename=f}) >>=
                         foldCompile fs


------------------------------------------------------
-- auxiliary functions
------------------------------------------------------

-- names of all haskell files associated with program
haskellFiles :: Options -> String -> [String]
haskellFiles opts name = [funcHsName name]

------------------------------------------------------
-- basic transformation
------------------------------------------------------
-- for a given module up to three haskell modules are generated:
    -- one for the functions,
    -- one for the data declarations (possibly empty)
    -- one "Main"-module to generate executables, 
    --   if the executable flag is set in the options
-- introduce Modules CallTime/RunTimeChoice 
transform typeMapping aux opts0 (Prog name imports types funcs _)
  = (if executable opts 
     then [(destination False Nothing (fromJust (binary opts)++".hs"),
            False,mainModule)] 
     else []) ++ modules

  where
    opts = opts0{hasData=hasInternalData}
    hasInternalData      = not $ null $ filter (not . isExternalType) types

    modules = [allinclusiveProg]

    -- filename, flag and module definitions
    allinclusiveProg = 
      (destination True (target opts) (funcHsName (filename opts)),
       False,
       allinclusive)


    modul mName mImports mExports mTypes mInsts mFuncs =  
      C.Prog mName mImports mExports mTypes mInsts mFuncs []

    allinclusive   = modul funcName allIImports allIExports dataTypes instances functions

    -- the module names are:
    funcName = modName name

    -- the file names of these modules are:
    funcFileName = funcHsName (filename opts)
 
    -- import lists
    newImports = map modName imports
    allIImports = [curryModule] ++ newImports

    {-
    -- this is the only special prelude treatment:
    instImportName 
      | name=="Prelude" = instName ++ " hiding ("++opsUsedInInstances++")"
      | otherwise       = instName
    opsUsedInInstances = "op_38_38"
    -}

    -- export lists
    allIExports = []
 
    -- the generated types, instances and functions
    dataTypes = map (transTypeDecl opts)
                    (typeSyns++filter isToTransform typeDecls)
    instances =  genInstances BaseCurry baseCurryInstance opts typeDecls
              ++ genInstances Curry     curryInstance     opts typeDecls
              ++ genInstances Show      showInstance      opts typeDecls
              ++ genInstances Read      readInstance      opts typeDecls
    functions = map (transFunc opts typeMapping) funcs

    mainModule = mainMod aux funcName opts
 
    -- information about original module 
    (typeSyns,typeDecls) = partition isTypeSyn $ 
                           filter (\t->  not (elem (snd $ typeName t) (extData opts))) types
    isToTransform t = case lookup (snd $ typeName t) (extInsts opts) of
      Nothing -> True
      Just is -> not (elem Declaration is)


--------------------------------------------------------
-- adding main function for executables
--------------------------------------------------------

generateAuxNames fs = (genNewName "aux1" fns,genNewName "aux2" fns)
  where 
    fns = map (snd . funcName) fs

    genNewName s ts = if elem s ts then genNewName ('a':s) ts else s
    

mainMod (_,aux2) m opts = let aux = (m,snd (funName ("",aux2))) in
  C.Prog "Main" [curryModule,modName "Prelude",m]
     [] [] [] 
     [C.Func (m,"main") public untyped 
        (Just  [C.Rule [] 
          (noguard $ fapp (hasPresym ">>") 
                       [app (setProg opts) (C.String (mainModule opts)),
                        app (C.Symbol (modName "Prelude","curryIOVoid"))
                            (sym aux)]) []])]
     []
  where
    setProg _ = cusym "setProgNameAndOrBased"

addExec (aux1,aux2) opts (Prog m is ts funcs ops) = 
  case lookup (mainFunc opts) lfs of
    Just f@(Func n a vis t (Rule vs e)) 
     | t == ioT unitT -> prog False
       [Func a2 0 vis t (Rule [] (flatApp n []))]
     | isIOType t -> prog True
       [Func a1 0 vis (monomorph t) (Rule [] (flatApp n [])),
        Func a2 0 vis (ioT unitT) (Rule [] (flatApp printIO [calla1 t True]))]
     |  isFuncType t && not (debug opts) -- && not (isFuncType (range t)))
          -> Right (mainFunc opts++" is no constant")
     | debug opts -> prog False
       [Func a1 1 vis (monomorph t) (Rule [0] (flatApp n [Var 0])),
        Func a2 0 vis (ioT unitT) (Rule [] 
          (calla1 t (isFuncType (range t) && 
                     isFuncType (range (range t)) && 
                     isIOType (range (range (range t))))))]
     | otherwise -> prog True
       [Func a1 0 vis (monomorph t) (Rule [] (flatApp n [])),
        Func a2 0 vis (ioT unitT) (Rule [] 
          (flatBind (flatGst (calla1 t True)) (startFunc opts)))]
    _ -> Right (mainFunc opts++" undefined")
  where
    a1 = (m,aux1)
    a2 = (m,aux2)
    calla1 t orc = if debug opts 
                   then Comb FuncCall ("Oracle","oracle"++if orc then "IO" else "") 
                             [Comb (FuncPartCall 1) a1 []]
                   else Comb FuncCall a1 []
    printIO = ("Interactive","printIO")
    lfs = zip (map (snd . funcName) funcs) funcs
  
    startFunc Opts{pm=Interactive DF} = ask ... df 
    startFunc Opts{pm=Interactive BF} = ask ... bf 
    startFunc Opts{pm=All DF}         = pr  ... df
    startFunc Opts{pm=All BF}         = pr  ... bf 
    startFunc Opts{pm=First DF}       = ap_ pr $ hd ... df
    startFunc Opts{pm=First BF}       = ap_ pr $ hd ... bf
    startFunc Opts{pm=ST}             = Comb (FuncPartCall 1) pr []
  
    monomorph (TVar _) = unitT
    monomorph (TCons n args) = TCons n (map monomorph args)
    monomorph (FuncType t1 t2) = FuncType (monomorph t1) (monomorph t2)

    prog addInt fs =  Left (Prog m (if addInt then "Interactive":is else is)
                                 ts (fs++funcs) ops)

ask = ("Interactive","interactiveSols")
df  = ("Prelude","allValuesD")
bf  = ("Prelude","allValuesB")
pr  = ("Interactive","printTerm")
hd  = ("Prelude","head")
f ... g = Comb FuncCall  (addPre ".") 
            [Comb (FuncPartCall 1) f [],Comb (FuncPartCall 1) g []]
ap_ f e = Comb FuncCall  (addPre ".") [Comb (FuncPartCall 1) f [],e]

------------------------------------------------------
-- transformation of type declarations
------------------------------------------------------

-- each type declaration has to derive instances for Show and Read
-- moreover, new constructors for logical variables, ors and fails 
-- have to be added.

transTypeDecl :: Options -> TypeDecl -> C.TypeDecl
transTypeDecl opts (Type name vis vars consdecls) 
  = C.Type (consName opts name) (transvis vis) (map (varName "t") vars) 
           (map (transConsdecls opts) consdecls ++
            newConsDecls (consName opts name) vars)
           []
transTypeDecl opts (TypeSyn name vis vars t) 
  = C.TypeSyn (consName opts name) (transvis vis) (map (varName "t") vars) 
              (transTypeExpr opts t)

transConsdecls :: Options -> ConsDecl -> C.ConsDecl
transConsdecls opts (Cons name arity vis ts) 
  = C.Cons (consName opts name) arity (transvis vis) False 
           (map (transTypeExprF opts) ts)

transTypeExpr, transTypeExprF :: Options -> TypeExpr -> C.TypeExpr
transTypeExpr _ (TVar n) = toTVar n
transTypeExpr opts (FuncType t1 t2) = 
  C.FuncType (transTypeExprF opts t1) (transTypeExpr opts t2)
transTypeExpr opts (TCons name ts) 
  = C.TCons (consName opts name) (map (transTypeExprF opts) ts)

transTypeExprF _ (TVar n) = toTVar n
transTypeExprF opts (FuncType t1 t2) = 
      C.TCons (consName opts{extCons=True} (addPre "Prim"))
        [addStateType (C.FuncType (transTypeExprF opts t1) (transTypeExprF opts t2))]
transTypeExprF opts (TCons name ts) 
 = C.TCons (consName opts name) (map (transTypeExprF opts) ts)

newConsDecls (m,n) vs 
  = [C.Cons (m,n++"Fail") 0 private  False [tExceptions],
     C.Cons (m,n++"Or") 2 private False 
        [tOrRef, tBranches newT]] 
  where
    newT = C.TCons (m,n) (map toTVar vs)


-------------------------------------------
-- generating instances
-------------------------------------------

inst newModName name vars classname =  
  C.Instance (map (\v -> C.TypeClass (cu classname) [toTVar v]) vars) 
             (C.TypeClass (cu classname) 
                          [C.TCons (newModName,name) (map toTVar vars)])


curryInstance opts t@(Type origName vis vars consdecls) 
  = inst newModName name vars curryClass 
         [strEq,eq,propagate,foldCurry,typeName,showFunction True opts t] --toTerm,fromTerm
  where
    (newModName,name) = consName opts origName 

    origMod = fst origName
  
    isPrelude = origMod=="Prelude"

    strEq = C.Func (newModName,"strEq") (transvis vis) untyped 
                  (Just  
                    (map strEqRule consdecls++
                    [C.Rule [_x,toPVar 0,_x]
                           (noguard $ 
                              fapp (cupresym "strEqFail")
                                   [fapp (cupresym "typeName") [toVar 0]]) []]))

    strEqRule (Cons cname arity _ _) =
      rule  [C.PComb (consName opts cname) (map toPVar [1..arity]),
             C.PComb (consName opts cname) (map (toPVar' "y") [1..arity])]
            (noguard $ if arity==0 then (cupresym "strEqSuccess")
                         else foldr1 (\ e es -> fapp (cupresym "concAnd") 
                                                     (addStateArg [e,es]))
                                 (map sEq [1..arity])) []
       where
         sEq i = fapp (cupresym "genStrEq") (addStateArg [toVar i,toVar' "y" i])

    eq = C.Func (newModName,"eq") (transvis vis) untyped 
               (Just  
                       (map eqRule consdecls
                         ++otherwiseExp 3 (concupresym opts "False")))

    eqRule (Cons cname arity _ _) =
      rule  [C.PComb (consName opts cname) (map toPVar [1..arity]),
             C.PComb (consName opts cname) (map (toPVar' "y") [1..arity])]
             (noguard $ if arity==0 then concupresym opts "True"
                         else foldr1 (\ e es -> fapp (funcupresym "&&") (addStateArg [e,es]))
                                (map eqArgs [1..arity])) []
       where
         eqArgs i = fapp (cupresym "genEq") (addStateArg [toVar i,toVar' "y" i])

    propagate = C.Func (newModName,"propagate") (transvis vis) untyped 
                       (Just  (map propRule consdecls))
                       

    propRule (Cons cname arity _ _) =
      C.Rule (addStatePat [C.PVar "f",C.PComb (consName opts cname) 
                                              (map toPVar [1..arity])])
             (noguard $ fapp (sym (consName opts cname))
                             (map propCall [1 .. arity])) []
      where propCall i = fapp (C.Var "f") (addStateArg [toHInt (i-1),toVar i])

    foldCurry = C.Func (newModName,"foldCurry") (transvis vis) untyped 
                       (Just  (map foldRule consdecls))
                       
    foldRule (Cons cname arity _ _) =
      C.Rule (addStatePat [C.PVar "f",C.PVar "c",C.PComb (consName opts cname) 
                                                         (map toPVar [1..arity])])
             (noguard $ foldr appFold (C.Var "c") (map toVar [1 .. arity])) []
       where
         appFold v e = fapp (C.Var "f") (addStateArg [v,e])

    typeName =  C.Func (newModName,"typeName") (transvis vis) untyped 
                  (Just  [C.Rule [_x] 
                                 (noguard $ C.String (snd origName)) []])

    toTerm = C.Func (newModName,"toC_Term") (transvis vis) untyped 
                  (Just  
                    (map toTermRule (zip [1..] consdecls) ++
                    [C.Rule [_x,_x,
                             C.PComb (newModName,name++"FreeVar") [C.PVar "r"]] 
                            (noguard $ app (cupresym "C_Free") 
                                        (app c_int
                                          (app (hasPresym "toInteger")
                                               (C.Var "r")))) []]))

    toTermRule (nr,(Cons cname arity _ _)) =
      C.Rule [C.PVar "mode",C.PVar "store",
              C.PComb (consName opts cname) (map toPVar [1..arity])]             
             (noguard $ fapp (cupresym "C_Data") 
                             [toInt nr,c_string_ origMod (snd cname),
                              dList isPrelude (map su [1..arity])]) []
       where
         su i = fapp (cusym "ctcStore") 
                     [C.Var "mode",app (cusym "toC_Term") (C.Var "mode"),
                      C.Var "store",toVar i]

    fromTerm = C.Func (newModName,"fromC_Term") (transvis vis) untyped 
                  (Just  
                    (concatMap fromTermRule (zip [1..] consdecls) ++
                    [C.Rule [C.PComb (baseType isPrelude "C_Free") 
                               [C.PComb (baseType isPrelude "C_Int") 
                                    [C.PVar "r"]]] 
                            (noguard $ app (sym (newModName,name++"FreeVar"))
                                           (app (hasPresym "fromInteger")
                                               (C.Var "r"))) []]))

    fromTermRule (nr,(Cons cname arity _ _)) =
      [rule "C_Data"     [pnr,_x,pts],
       rule "C_Data"     [pfree,pname,pts]]
       where
         pnr = toPInt opts nr
         pfree = C.PComb (baseType isPrelude "C_IntFreeVar") [_x]
         pname = dpList isPrelude (map (toPChar opts) (snd cname))
         pts = dpList isPrelude (map toPVar [1..arity])
         e = noguard $ fapp (sym (consName opts cname)) 
                            (map (app (cusym "fromC_Term") . toVar) [1..arity])
         rule c args = C.Rule [C.PComb (baseType isPrelude c) args] e []


baseCurryInstance opts (Type origName vis vars consdecls) 
  = inst newModName name vars "BaseCurry" 
       [nf False, nf True, 
      	free "generator" "generator",failed,branching,
      	consKind,
      	exceptions,orRef,branches]
  where
    (newModName,name) = consName opts origName 

    origMod = fst origName
  
    isPrelude = origMod=="Prelude"

    nf gr = C.Func (newModName,if gr then "gnf" else "nf") (transvis vis) untyped 
                  (Just  
            (concatMap (nfrule gr) (filter ((1<=) . consArity) consdecls) ++
             [C.Rule (addStatePat [C.PVar "f",C.PVar "x"])
                     (noguard (fapp (C.Var "f") (addStateArg [C.Var "x"]))) []]))

    nfrule gr (Cons cname arity _ _)
      =  [C.Rule [C.PVar "f",
                  C.PComb (consName opts cname) (map toPVar [1..arity]),
                  C.PVar "state0"]
                 (noguard $ foldr (nflambda gr)
                             (fapp (C.Var "f") 
                                [fapp (sym $ consName opts cname) 
                                        (map (toVar' "v") [1..arity]),
                                 toVar' "state" arity])
                             [1..arity]) []]

    nflambda gr i e = 
      fapp (cusym (if gr then "gnfCTC" else "nfCTC")) 
        [C.Lambda [toPVar' "v" i,toPVar' "state" i] e,toVar i,toVar' "state" (i-1)]

    free s t = C.Func (newModName,s) (transvis vis) untyped 
            (Just [C.Rule [C.PVar "i"] (noguard $ 
             fapp (cusym "withRef") [
             C.Lambda [C.PVar "r"] $
             fapp (sym (orName opts origName)) 
             [fapp (cusym "mkRef") [C.Var "r",maxAr,C.Var "i"],
              list_ (map freeCons consdecls)],
             maxAr]) []])
      where
        maxAr = C.Var (show (foldr max 0 (map consArity consdecls)))
        freeCons (Cons cname arity _ _) = 
          fapp (sym (consName opts cname)) 
               (snd $ foldr addOne (0,[]) (replicate arity (app (cusym t))))
        addOne e (n,es) = 
          (n+1,e (fapp (hasPresym "+") [C.Var "r",toHInt n]):es)
 
    failed = constructor "failed" failName 
    freeVarFunc = constructor "freeVar" freeVarName 
    branching = constructor "branching" orName 
    suspend = constructor "suspend" suspName 


    consKind = C.Func (newModName,"consKind") (transvis vis) untyped 
                  (Just  
                    (map tester [(orName, 2, "Branching"),
                                 (failName, 1, "Failed")] ++
                    [C.Rule [_x]
                           (noguard $ (cusym "Val")) []]))

    tester (namer,arity,nameTest)  = 
       C.Rule [C.PComb (namer opts origName) (take arity (repeat (_x)))]
              (noguard (cusym nameTest)) []

    selector nameSel namer arity number =
       C.Func (newModName,nameSel) (transvis vis) untyped 
         (Just [C.Rule [C.PComb (namer opts origName) 
                          (underscores (number-1)++[C.PVar "x"]++
                           underscores (arity-number))]
                       (noguard (C.Var "x")) []])

    constructor nameConstr namer = 
      C.Func (newModName,nameConstr) (transvis vis) untyped 
         (Just  [C.Rule []
                  (noguard $ sym (namer opts origName)) []])

    exceptions = selector "exceptions" failName 1 1
    freeVarRef = selector "freeVarRef" freeVarName 1 1
                     
    orRef    = selector "orRef" orName 2 1
    branches = selector "branches" orName 2 2

    suspRef  = selector "suspRef" suspName 2 1
    suspCont = selector "suspCont" suspName 2 2


    
---------------------------------------------------------------------------


   
------------------------------------------------------
-- transformation of functions and expressions
------------------------------------------------------

transFunc :: Options -> (QName -> QName) -> FuncDecl -> C.FuncDecl
transFunc opts typeMapping (Func fname arity vis t (Rule lhs rhs))
  = C.Func newFName (transvis vis) 
           (transFType opts arity t) crules
    where
      newFName = funName fname
      f = (modName (fst fname),auxName newFName) 
      trhs = transExpr opts rhs

      crules = case rhs of
        Case ct (Var n) bs -> Just (transBranching ct (break (==n) lhs) 
                                                   opts f typeMapping fname bs)
        Case ct _       bs -> error "case not normalized"
        _                  -> Just [rule (map toPVar lhs) (noguard trhs) []]

      auxName (_,name) = 
        if isInfixOpName name 
          then elimInfix name
          else name

transFunc opts _ (Func (m,fname) arity vis t (External _)) = 
 C.Func (funName (m,fname)) (transvis vis) (transFType opts arity t)
     (Just  [rule (map toPVar [1..arity])  
                 (noguard (fapp (C.Symbol (modName m,fname)) 
                                (addStateArg (map toVar [1..arity])))) []])


transFType :: Options -> Int -> TypeExpr -> Maybe C.TypeExpr
-- the first line is for transformations too lazy to compute correct type
transFType _ _ (TVar (-42)) = Nothing 
transFType opts arity t = Just $
  C.TConstr 
    [C.TypeClass c [toTVar tv] | tv <- nub (allVarsInTypeExpr t),
                                  c <- [(curryModule,"Curry")]]
    (addStateType (transFTypeExpr opts arity t))

transFTypeExpr opts 0 t = transTypeExprF opts t
transFTypeExpr opts (n+1) (FuncType t1 t2)
  = C.FuncType (transTypeExprF opts t1) (transFTypeExpr opts n t2)

transvis x | x==Private = C.Private
           | x==Public  = C.Public

transExpr :: Options -> Expr -> C.Expr
transExpr opts (Var n) = toVar n
transExpr opts (Lit l) = transLit opts l
transExpr opts (Free [] e) = transExpr opts e
transExpr opts (Free (v:vs) e) 
  = app freeCall (C.Lambda [toPVar v] (transExpr opts (Free vs e)))
transExpr opts (Or e1 e2) = fapp orSym (map (transExpr opts) [e1, e2])
transExpr opts (Let vbs e) = 
  C.LetDecl (map locdecl vbs) (transExpr opts e)
  where
    locdecl (v,b) = C.LocalPat (toPVar v) (transExpr opts b) []
transExpr opts (Comb FuncCall fn@("Global","global") args) =
  C.LetDecl [C.LocalPat (C.PVar "st") (hasPresym "Nothing") []] 
            (fapp (C.Symbol (funName fn)) (map (transExpr opts) args))
transExpr opts (Comb combType fname args) 
  = newExpr
  where
    newArgs = map (transExpr opts) args

    call = case combType of 
              ConsCall       -> symApp (consName opts fname) newArgs
              FuncCall       -> symApp (funName fname)       (addStateArg newArgs)
              FuncPartCall i -> symApp (funName fname)       newArgs
              ConsPartCall i -> symApp (consName opts fname) newArgs 

    symApp s xs = fapp (C.Symbol s) xs

    newExpr = case combType of 
                ConsCall       -> call
                FuncCall       -> call
                FuncPartCall i -> pf opts i call
                ConsPartCall i -> pc opts i call
transExpr _ (Case _ _ _) = error "unlifted case"


transLit :: Options -> Literal -> C.Expr
transLit opts (Charc c)  = toChar opts c
transLit opts (Floatc f) = toFloat opts f
transLit opts (Intc i)   = toInt i


transBranching :: CaseType -> ([VarIndex],[VarIndex]) -> Options -> QName -> 
  (QName -> QName) -> QName -> [BranchExpr] -> [C.Rule]
transBranching caseMode vs@(as,v:bs) opts f tm oName branches
  = oldRules++newRules
  where
    oldRules = map (transRule vs opts) branches
    typeName = case (\ (Branch p _) -> p) (head branches) of
      Pattern c _ -> tm c
      LPattern l  -> ("Prelude",case l of {Intc _->"Int";Charc _->"Char"})

    freePat = C.AsPat "x" (C.PComb (freeVarName opts typeName) [C.PVar "ref"])
    orPat   = C.PComb (orName opts typeName) [C.PVar "i",C.PVar "xs"]
    suspPat = C.PComb (suspName opts typeName) [C.PVar "ref",C.PVar "susp"]

    isOracleMod = debug opts && take 11 (fst f)=="CurryOracle" && length (fst f) > 11

    refVar = 1 --if null (as++bs) then error $ "where is the ref?" ++ show f
               --               else last (as++bs)
    applyf b = C.Lambda (addStatePat (if b then [toPVar refVar,C.PVar "x"]
                                           else [C.PVar "x"]))
                      (fapp (sym f) 
                            (addStateArg (map toVar as ++ 
                                          C.Var "x" : map toVar bs)))

    newLhs p e = rule (map toPVar as ++ (p:map toPVar bs)) e []
    newRules = 
           [newLhs orPat
             (noguard ((if isOracleMod
                        then fapp (sym (funName ("CEventOracle","onBranches"))) .
                             (toVar refVar :)
                        else fapp (cusym "mapOr"))
                       (addStateArg [applyf isOracleMod,
                                     C.Var "i",C.Var "xs"])))
           ,newLhs (C.PVar "x")
                   (noguard $ (if isOracleMod then closeRef refVar else id)
                            $ fapp (cusym "patternFail") 
                                  [qname_ oName,C.Var "x"])]


    closeRef i e = fapp (sym $ funName ("CEventOracle","closeRef")) $
                        addStateArg [toVar i,e]

transRule :: ([VarIndex],[VarIndex]) -> Options -> BranchExpr -> C.Rule 
transRule (as,v:bs) opts (Branch (LPattern l@(Charc _)) e) 
  = rule ps (C.GuardedExpr [(guard,transExpr opts e)]) []
  where
    guard = app (cupresym "isC_True")
                (fapp (funcupresym "===") [toVar v,toLit opts l])
    ps    = map toPVar as ++ toPVar v : map toPVar bs
transRule (as,v:bs) opts (Branch (LPattern l) e) 
  = rule ps (noguard (transExpr opts e)) []
  where
    ps  = map toPVar as ++ C.AsPat (xvar v) (toPLit opts l) : map toPVar bs
transRule (as,v:bs) opts (Branch (Pattern name args) e) 
  = rule ps (noguard (transExpr opts e)) []
  where
    ps = map toPVar as ++ (if elem v args then id else C.AsPat (xvar v)) 
                          (C.PComb (consName opts name) (map toPVar args)) 
                        : map toPVar bs


rule ps = C.Rule (addStatePat ps)

transOp (Op name InfixOp p)  = C.Op (funName name) C.InfixOp  p
transOp (Op name InfixlOp p) = C.Op (funName name) C.InfixlOp p
transOp (Op name InfixrOp p) = C.Op (funName name) C.InfixrOp p


----------------------------------------------------------------
-- generating instances for read and show
----------------------------------------------------------------

genInstances _ _ _ [] = []
genInstances cl genFunc opts (t:ts) 
  | maybe False (elem cl) (lookup (snd $ typeName t) (extInsts opts)) 
  = genInstances cl genFunc opts ts
  | otherwise = genFunc opts t : 
                genInstances cl genFunc opts ts

showInstance opts t@(Type origName vis vars consdecls) =
 C.Instance (map (\v -> C.TypeClass (addPre "Show") [toTVar v]) vars)
   (C.TypeClass (addPre "Show") [C.TCons (newModName,name) (map toTVar vars)])
   [showFunction False opts t]
 where
   (newModName,name) = consName opts origName



showFunction showQ opts t@(Type origName vis vars consdecls) 
  | maybe False (elem Show) (lookup (snd $ typeName t) (extInsts opts)) 
  = showsPrec [C.Rule [] (C.SimpleExpr (hasPresym "showsPrec")) []]
  | otherwise = showsPrec (map showsPrecRule consdecls
              ++[showGenerator])
 where
   showParenArg (_,'(':_) = hasPresym "True"
   showParenArg _         = if showQ then hasPresym "True" else lt (C.Var "d") app_prec
   showsPrecName = if showQ then "showQ" else "showsPrec"
   showsPrecSym  = (if showQ then cupresym  
                             else hasPresym) showsPrecName

   identifier (_,"()") = "()"
   identifier (cm,cn)  = if showQ then cm++"."++cn else cn

   opening (_,'(':_) = ""
   opening cmn       = identifier cmn ++ " "

   separator (_,'(':_) = ','
   separator _         = ' '

   showsPrec rs = C.Func (newModName,showsPrecName) 
                         (transvis vis) untyped 
                         (Just rs)

   (newModName,name) = consName opts origName

   showsPrecRule (Cons cname 0 _ []) = 
      C.Rule [_x, C.PComb (consName opts cname) []]
         (C.SimpleExpr 
            (app (hasPresym "showString") (string_ (identifier cname)))) []
   showsPrecRule (Cons cname arity _ args) = 
     C.Rule [C.PVar "d", C.PComb (consName opts cname) (map toPVar [1..arity])]
            (C.SimpleExpr (fapp (hasPresym "showParen") 
                             [showParenArg cname,sym ("","showStr")]))
            [C.LocalFunc (C.Func ("","showStr") (transvis vis) untyped 
                  (Just [C.Rule [] (C.SimpleExpr showStr) []]))]
      where
        showStr = points (app (hasPresym "showString") (string_ (opening cname)):
                          intersperse 
                            (app (hasPresym "showChar") (char_ (separator cname)))
                            (map callShowsPrec [1..arity]))
                             

        callShowsPrec i = fapp showsPrecSym [add_prec cname,toVar i]

        points = foldr1 point 

        point x y = fapp (hasPresym ".") [x,y]


   showTuple = C.Func (newModName,showsPrecName) (transvis vis) untyped 
                  (Just (map showTupleRule consdecls++[showGenerator]))

   showTupleRule (Cons cname arity _ args) = 
     C.Rule [C.PVar "d", C.PComb (consName opts cname) (map toPVar [1..arity])]
            (C.SimpleExpr (app (hasPresym "showString") 
                             (app (hasPresym "show") 
                               (fapp (sym ("",snd cname)) 
                                    (map toVar [1..arity]))))) []

   showGenerator = C.Rule [_x, 
                         C.PComb (newModName,name++"Or") [C.PVar "r",_x]]
                   (C.SimpleExpr 
                       (app (hasPresym "showString") 
                            (cons_ (char_ '_') 
                                   (app (hasPresym "show") 
                                        (app (cusym "deref")
                                             (C.Var "r")))))) []

readInstance :: Config.Options -> TypeDecl -> C.InstanceDecl
readInstance opts (Type origName@(modName,name) vis vars consdecls) =
 C.Instance (map (\v -> C.TypeClass (addPre "Read") [toTVar v]) vars)
   (C.TypeClass (addPre "Read") [C.TCons (newModName,newName) (map toTVar vars)])
   [if isTuple (snd origName) then readTuple else readsPrec]
 where
   c@(newModName,newName) = consName opts origName

   readsPrec = C.Func (newModName,"readsPrec") (transvis vis) untyped 
                  (Just [C.Rule [C.PVar "d",C.PVar "r"] 
                          (C.SimpleExpr (plusplus (map read consdecls))) []])

   plusplus = foldr1 (\x y->fapp (hasPresym "++") [x,y])

   read cons@(Cons _ 0 _ []) = 
     fapp (hasPresym "readParen") [hasPresym "False",lamb cons,C.Var "r"]
   read cons = 
     fapp (hasPresym "readParen") [lt (C.Var "d") app_prec,lamb cons,C.Var "r"]

   lamb (Cons cn@(cmodName,cname) arity _ args) = C.Lambda [C.PVar "r"] 
     (C.ListComp (fapp (sym ("","(,)")) 
                     [fapp (sym newC) 
                           (map toVar [1..arity]),
                      toVar' "r" arity ])
        (C.SPat (pair (C.PVar "_") (toPVar' "r" 0)) 
              (fapp (cusym "readQualified") [string_ cmodName,string_ cname,C.Var "r"]):
         map readArg [1..arity]))

     where
       newC@(newMod,newCName) = consName opts cn
    
   readArg i = C.SPat (pair  (toPVar' "x" i) (toPVar' "r" i))
                      (fapp (hasPresym "readsPrec") 
                           [add_prec ("",""),
                            toVar' "r" (i-1)])

   readTuple = C.Func (newModName,"readsPrec") (transvis vis) untyped 
                  (Just (map readTupleRule consdecls))

   readTupleRule (Cons t@(_,tup) arity _ args) =
     C.Rule [C.PVar "d",C.PVar "r"] 
       (C.SimpleExpr 
          (fapp (hasPresym "map") [sym ("","readTup"),
                                   fapp (hasPresym "readsPrec") 
                                        [C.Var "d",C.Var "r"]])) 
       [C.LocalFunc (C.Func ("","readTup") (transvis vis) untyped 
          (Just [C.Rule [pair (C.PComb ("",tup) (map toPVar [1..arity])) 
                              (C.PVar "s")] 
                   (C.SimpleExpr   
                      (fapp (sym ("","(,)"))
                         [fapp (sym (consName opts t)) (map toVar [1..arity]),
                          C.Var "s"])) []]))]
        
   pair x y = C.PComb ("","(,)") [x,y]


add_prec (_,'(':_) = cusym "zero"
add_prec _         = cusym "eleven"

app_prec = cusym "ten"

lt x y = fapp (hasPresym ">") [x,y]

int i = app (hasPresym "fromInteger") (C.Lit (C.Intc i))


--------------------------
-- naming conventions
--------------------------

consName,freeVarName,failName,orName,suspName :: Options -> QName -> QName
consName opts (m,n) = (modName m,cn)
  where
    cn | extCons opts = n
       | otherwise    = constructorName n
    
freeVarName opts = N.freeVarName . consName opts
failName    opts = N.failName    . consName opts
orName      opts = N.orName      . consName opts
suspName    opts = N.suspName    . consName opts

curryName s = (curryModule,s)
curryTCons = C.TCons . curryName

----------------------------------------
-- treating the additional state argument
----------------------------------------

stateTypeName :: String
stateTypeName = "State"

addStateType :: C.TypeExpr -> C.TypeExpr
addStateType t@(C.TVar _) = C.FuncType (curryTCons stateTypeName []) t
addStateType t@(C.TCons _ _) = C.FuncType (curryTCons stateTypeName []) t
addStateType (C.FuncType t1 t2) = C.FuncType t1 (addStateType t2)

addStatePat :: [C.Pattern] -> [C.Pattern]
addStatePat = (++[C.PVar "st"])

addStateArg :: [C.Expr] -> [C.Expr]
addStateArg = (++[C.Var "st"])

-- global definitions must not have a state argument
addGlobalDefs :: Options -> [FuncDecl] -> [(String,Bool,C.Prog)] -> [(String,Bool,C.Prog)]
addGlobalDefs opts gs (x:xs@(_:_)) = x : addGlobalDefs opts gs xs
addGlobalDefs opts gs [(s,b,prog)] = [(s,b,prog{C.funcDecls=gs'++C.funcDecls prog})]
  where 
    gs' = map transformGlobal gs
    transformGlobal (Func n 0 vis t (Rule [] e)) = 
      C.Func (funName n) (transvis vis) (transFType opts 0 t) 
        (Just [C.Rule [] 
                 (C.SimpleExpr (transExpr opts e)) []])

----------------------------------------------------------------
-- constants and abbreviations for flat, resp. abstract curry
----------------------------------------------------------------

-- prelude symbols
sym = C.Symbol 
prelude   = "Prelude"
addPre    = (,) prelude
hasPresym = sym . addPre
cupresym  = sym . (,) (modName prelude)
funcupresym = sym . funName . addPre
concupresym opts = sym . consName opts . addPre

-- symbols from Curry library
curryModule = "Curry.RunTimeSystem"
curryClass  = "Curry"
cu          = (,) curryModule
cusym       = sym . cu

part opts i e = 
  if i<2
   then primValue opts (C.Lambda (addStatePat [toPVar' "v" 1]) e)
   else primValue opts (C.Lambda [toPVar' "v" i, _x] (part opts (i-1) e))

isPrelude :: Options -> Bool
isPrelude opts = currentModule opts=="Prelude" 

-- partial function call, one argument missing
pf :: Options -> Int -> C.Expr -> C.Expr
pf opts = app . partial opts (fapp (cupresym "pf"))

-- partial constructor call, one argument missing
pc :: Options -> Int -> C.Expr -> C.Expr
pc opts = app . partial opts (fapp (cupresym "pc"))

-- partial application, more than one argument
pa :: Options -> [C.Expr] -> C.Expr
pa opts = fapp (cupresym "pa")

-- function compostition (.)
cp :: Options -> [C.Expr] -> C.Expr
cp opts = fapp (cupresym "cp")

partial :: Options -> ([C.Expr] -> C.Expr) -> Int -> C.Expr
partial opts part n
  = foldr1 (\f g -> cp opts [f,g])
  . map (\ (k,p) -> dotted opts (k-1) (p [])) 
  $ reverse (zip (reverse [1..n]) (part:repeat (pa opts)))

-- add a lot of dots to compose part call functions
dotted :: Options -> Int -> C.Expr -> C.Expr
dotted opts n p
  | n == 0    = p
  | otherwise = dotted opts (n-1) (cp opts [p])

prelPCons opts s = C.PComb (consName opts (addPre s))

pO opts x = prelPCons opts "O"   [x]
pI opts x = prelPCons opts "I"   [x]
pIHi opts = prelPCons opts "IHi" []

p0 opts     = prelPCons opts "Zero" []
pPos opts x = prelPCons opts "Pos" [x]
pNeg opts x = prelPCons opts "Neg" [x]

public = C.Public

isMain (_,fname) = fname=="main"

isFirst (_,fname) = fname=="first"

cunit opts = sym (consName opts{extCons=True} $ addPre "T0")

-- types

tFreeVarRef t = curryTCons "FreeVarRef" [t]

tOrRef = curryTCons "OrRef" []

tExceptions = curryTCons "C_Exceptions" []

tSuspRef = curryTCons "SuspRef" []

tList a = C.TCons (addPre "[]") [a]
c_tList a = curryTCons "List" [a]

tPair a b = C.TCons (addPre "(,)") [a,b]

tMaybe a = C.TCons (addPre "Maybe") [a]

tBranches x = curryTCons "Branches" [x]

tSusp x = curryTCons "SuspCont" [x]

private = C.Private

untyped = Nothing

noguard e = C.SimpleExpr e

freeCall = cusym "freeF"

orSym = cusym "orF"

app a b = C.Apply a b

app2 a b c = app (app a b) c

fapp x xs = foldl C.Apply x xs

flatApp = Comb FuncCall 

flatBind x y = Comb FuncCall (addPre ">>=") [x,y]

flatEq x y = Comb FuncCall (addPre "===") [x,y]

flatGst x = Comb FuncCall (addPre "getSearchTree") [x]

mid = hasPresym "id"

baseType _ s = addPre s

toVar i = C.Var (xvar i)

toVar' s i = C.Var (varName s i)

xvar = varName "x"

varName s i = s++show i

toPVar i = C.PVar (varName "x" i)

toPVar' s i = C.PVar (varName s i)

toTVar i = C.TVar (varName "t" i)

primValue opts v = 
  app (sym $ consName opts{extCons=True} (addPre "PrimValue")) v


toList [] = C.Symbol ("","[]")
toList (x:xs) = app2 (C.Symbol ("",":")) x (toList xs)

toPList [] = C.PComb ("","[]") []
toPList (x:xs) = C.PComb ("",":") [x,toPList xs] 

toPLit opts (Intc i) = toPInt opts i
toPLit opts (Charc c) = toPChar opts c
toPLit opts (Floatc f) = toPFloat opts f

toPInt opts n 
  | n>0  = pPos opts (toPNat opts n)
  | n<0  = pNeg opts (toPNat opts (negate n))
  | n==0 = p0 opts

toPNat opts n 
  | d==0 = pIHi opts
  | m==1 = pI opts (toPNat opts d)
  | m==0 = pO opts (toPNat opts d)
  where
    d = div n 2
    m = mod n 2

toPChar opts c 
  | currentModule opts=="Prelude" = C.PComb (modName "Prelude","C_Char") [C.PLit (C.Charc c)]
  | otherwise = C.PComb (modName "Prelude","C_Char") [C.PLit (C.Charc c)]

toPFloat opts n = primPValue opts (C.PLit (C.Floatc n))

primPValue opts p =  C.PComb (consName opts{extCons=True} (addPre "PrimValue")) [p]

toLit opts (Intc i) = toInt i
toLit opts (Charc c) = toChar opts c
toLit opts (Floatc f) = toFloat opts f

toInt n  = C.Lit (C.Intc (toInteger n))
toHInt n = C.Lit (C.HasIntc (toInteger n))

c_int =  cupresym "C_Int"

toChar opts c = app (sym (consName opts ("Prelude","Char"))) (C.Lit (C.Charc c))
toFloat opts f = primValue opts (C.Lit (C.Floatc f))



otherwiseExp n e = [C.Rule (map C.PVar (take n (repeat "_")))
                           (noguard e) []]

ioT x = TCons ("Prelude","IO") [x]
unitT = TCons ("Prelude","()") []

hasUnit = sym ("","()")

hasBind x y = fapp (hasPresym ">>=") [x,y]
hasReturn x = app (hasPresym "return") x

char_ c = C.Lit (C.Charc c)

list_ [] = nil 
list_ (x:xs) = cons_ x (list_ xs)

cons_ x xs = fapp (sym ("",":")) [x,xs]
nil = sym ("","[]")

string_ n = list_ (map char_ n)

c_char_ c = fapp (cusym "C_Char") [C.Lit (C.Charc c)]

c_list_ [] = c_nil
c_list_ (x:xs) = c_cons_ x (c_list_ xs)

c_cons_ x xs = fapp (cupresym ":<") [x,xs]
c_nil = cupresym "List"

bc_list_ [] = bc_nil
bc_list_ (x:xs) = bc_cons_ x (bc_list_ xs)

dList True  = bc_list_
dList False = c_list_

dpList True  = bc_plist_
dpList False = c_plist_

bc_cons_ x xs = fapp (cupresym ":<") [x,xs]
bc_nil = cupresym "List"

c_string_ "Prelude" n = bc_list_ (map c_char_ n)
c_string_ _         n =  c_list_ (map c_char_ n)

pchar_ c = C.PLit (C.Charc c)

plist_ [] = pnil 
plist_ (x:xs) = pcons_ x (plist_ xs)

pcons_ x xs = C.PComb ("",":") [x,xs]
pnil = C.PComb ("","[]") []

c_plist_ [] = c_pnil 
c_plist_ (x:xs) = c_pcons_ x (c_plist_ xs)

c_pcons_ x xs = C.PComb (addPre ":<") [x,xs]
c_pnil = C.PComb (addPre "List") []

bc_plist_ [] = bc_pnil 
bc_plist_ (x:xs) = bc_pcons_ x (bc_plist_ xs)

bc_pcons_ x xs = C.PComb (addPre ":<") [x,xs]
bc_pnil = C.PComb (addPre "List") []


pstring_ n = plist_ (map pchar_ n)

underscores i = replicate i (_x)

qname_ (m,f) = string_ (m++'.':f)

_x = C.PVar "_"

st = C.Var "st"

