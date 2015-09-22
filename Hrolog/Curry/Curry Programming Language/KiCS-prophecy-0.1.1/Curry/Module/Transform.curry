--- Program transformation to compute the oracle.
---
--- @version July 2007
--- new: employ make for modulewise transformation
---

module Transform where

import System (getArgs)
import Directory 
import Integer ( maxlist )
import List 
import Maybe 
import Distribution (getStdLibDir)
import ReadShowTerm

import FlatCurry
import FlatCurryGoodies

import Wrapper
import Make
import AddWorld (addWorld)
import LiftCases (liftCases)

main :: IO () 
main = do
  putStrLn "this is the curry prophecy generator"
  params <- parseArgs
  libdir <- getStdLibDir 
  maybe done mayCreateDirectory (output params)
  transform libdir (force params) (quiet params) (output params) (modulename params)
  putStrLn "curry prophecy generator finished"

mayCreateDirectory :: String -> IO ()
mayCreateDirectory dir = do 
  ex <- doesDirectoryExist dir
  unless ex (createDirectory dir)

transform :: String -> Bool -> Bool -> Maybe String -> String -> IO ()
transform libdir force quiet outdir mod = make quiet mod tester (writeTrans outdir)
  where
    tester = if force then (\ fn _ -> readTypes [fn] >>= return . Just)
                      else myObsolete
    readTypes fns = do
      prog <- readFile (head fns)
      let typeString = dropWhile (/='[') $ dropWhile (/=']') $ dropWhile (/='[') prog
          types = fst $ head $ readsTerm typeString
      return $ filter hasHOTypeArg types

    obso = obsolete quiet (\ dir -> addFcy . addOrc dir outdir) [addFcy] readTypes 

    myObsolete path modu = do
      ob <- obso path modu
      if isJust ob || isJust outdir || not (isPrefixOf libdir path)
        then return ob
        else do
          unless quiet (putStrLn (modu ++ " is a standard library")) 
          ts <- readTypes [path++addFcy modu]
          return (Just ts)

writeTrans :: Maybe String -> String -> [[TypeDecl]] -> Prog -> IO [TypeDecl]
writeTrans outdir path imps prog = do
      let fn = addOrc path outdir (progName prog)
          (trTypes,trProg) = transProg (concat imps) (liftCases False prog)
          res = show trProg
      putStrLn ("generating "++fn)
      writeFile (fn++".fcy")  res
      writeFile (fn++".fint") res
      return trTypes

transProg :: [TypeDecl] -> Prog -> ([TypeDecl],Prog)
transProg impTypes prog
  = (typesToTransform,
     updProg newModName 
            (\imps -> eventMod:oracle:ioexts:progName prog:imps ++ map newModName imps)
            (map (transType isTrType) . filter (isTrType . typeName))
            (concatMap (transFunc isTrType isTrCons))
            (map (updOpName newModNameQ)) prog')
  where
    prog' = rnmAllVarsInProg (+1) (addWorld prog) -- relies on vars starting from 1
    typesToTransform = fst (fixDependence (withHo++impTypes) noHo) 
    (withHo,noHo)    = partition hasHOTypeArg (progTypes prog)

    fixDependence knowns unknowns = 
      case partition (dependsOnTypes (map typeName knowns)) unknowns of
       ([],indeps)   -> (knowns,indeps)
       (deps,indeps) -> fixDependence (knowns++deps) indeps
    
    dependsOnTypes ts = trType (\_ _ _ -> any (any (containsTypes ts) . consArgs))
                               (\_ _ _ -> containsTypes ts)
    containsTypes ts = trTypeExpr (\_->False) (\qn args -> or (elem qn ts:args)) (||)
    
    isTrType t  = elem t $ map typeName typesToTransform
    isTrCons c = elem c $ map consName $ concatMap typeConsDecls 
                        $ filter (not . isTypeSyn) typesToTransform

hasHOTypeArg :: TypeDecl -> Bool
hasHOTypeArg = trType (\_ _ _ cs -> any isFuncType (concatMap consArgs cs)) 
                      (\_ _ _ _ -> False)

transType :: (QName -> Bool) -> TypeDecl -> TypeDecl
transType isTr t 
  | isTr (typeName t) = updTypeName newModNameQ $
                        updTypeConsDecls (map (updCons newModNameQ id id 
                                                       (map (rType isTr)))) t
  | otherwise = t

when :: (a -> Bool) -> (a -> a) -> a -> a
when p f x = if p x then f x else x

rType :: (QName -> Bool) -> TypeExpr -> TypeExpr
rType _ t@(TVar _) = t
rType isTr (TCons name args) = TCons (when isTr newModNameQ name) (map (rType isTr) args)
rType isTr (FuncType dom ran) = 
  FuncType tRef (FuncType (updQNamesInTypeExpr (when isTr newModNameQ) 
                                               (rType isTr dom)) 
                          (rType isTr ran))

transFunc :: (QName -> Bool) -> Options -> FuncDecl -> [FuncDecl]
transFunc isTr opts func@(Func name arity vis t _)
  | name==("Global","global") || name `elem` implementedInOracle = []
  | isExternal func && isIO typ
  = let call | isComplexIO typ = extIO (safeIOResult (extCall arity))
             | otherwise       = extIO (extCall arity) in
    [Func newName arity vis (transFuncType (arity-1) isTr t) $
     Rule (reverse [1 .. arity]) $ collapse ref call]
  | isExternal func 
    = [Func newName (arity+1) vis (transFuncType arity isTr t) $
       Rule (reverse args) $ collapse ref $ extCall (arity+1)]
  | isGlobal func 
    = if isValue (head (combArgs (funcBody func)))
      then [Func newName (arity+1) vis (transFuncType arity isTr t) $
            Rule (reverse args) $ collapse ref $ extCall (arity+1)]
      else error (fst name ++ '.':snd name ++
                  ": cannot treat global states initialized with \
                  \unevaluated expression\n\
                  \possible solution: do some inlining and evaluating.")
  | otherwise
    = case transExpr opts (funcBody func) nextVar of
       Right (ns,vs,exp) -> [updFunc newModNameQ       
                                     (+1)
                                     id
                                     (transFuncType arity isTr)
                                     (updRule (++[1])
                                              (const ({-if take 6 (snd name) == "_case_"
                                                      then exp
                                                      else-} event ns ref vs exp))
                                              id)
                                     func]
       Left err          -> [Func newName (arity+1) vis (transFuncType arity isTr t) $
			     Rule args $ errorCall $ 
                                  fst newName ++ '.':snd newName ++':':err]
 where
  typ = resultType t
  funcVars = allVarsInFunc func
  nextVar = if null funcVars then 2 else maxlist funcVars + 1
  args = [1..arity+1]
  newName = newModNameQ name

  isIO x = isTCons x && tConsName x==(prelude,"IO")
  isComplexIO (TVar _) = False
  isComplexIO (TCons _ as) = 
                  as/=[TCons (prelude,"()") []] 
               && not (elem name specialIOs)
  isGlobal f = case funcBody f of
                Comb FuncCall g [_,_] -> g==("Global","global") 
                _ -> False

  extCall i = Comb FuncCall name (map Var (reverse (tail [1 .. i])))
  isValue = trExpr (const False) (const True)     (\ x _ xs -> and (isCombTypeConsCall x:xs))
                   (\ bs x -> and (x:map snd bs)) (\ _ x -> x) (&&) (\_ _ _ -> False) (\_ _ -> False)

transFuncType :: Int -> (QName -> Bool) -> TypeExpr -> TypeExpr
transFuncType n isTr t@(TVar v) = 
  if v==(-42) || n/=0 then t else FuncType tRef (rType isTr t)
transFuncType 0 isTr t@(TCons _ _) = FuncType tRef (rType isTr t)
transFuncType n isTr t@(FuncType dom ran)
  | n == 0 = FuncType tRef (rType isTr t)
  | otherwise = FuncType (rType isTr dom) (transFuncType (n-1) isTr ran)

transExpr :: Options -> Expr -> Int -> Either String ([Int],[Int],Expr)
transExpr opts exp n = 
  case trExpr var lit comb leT freE oR casE branch exp opts n [ref] of
    Right (_,ns,vs,e) -> Right (ns,vs [],e)
    Left s -> Left s

  
type Options = QName -> Bool
type T a = Options
        -> Int               -- next unused reference
        -> [Int]             -- references to reuse
        -> Either 
           String            -- Error for non-supported constructs
           (Int              -- next unused reference
           ,[Int]            -- references to reuse
           ,[Int]->[Int]     -- references to be introduced
           ,a)               -- monadic result
           

ret :: a -> T a
ret x _ n ns = Right (n,ns,id,x)

(.>>=) :: T a -> (a -> T b) -> T b
(ta .>>= f) isTr n ns
  = case ta isTr n ns of
      Right (m,ms,vs,a) -> case f a isTr m ms of
                            Right (l,ls,ws,b) -> Right (l,ls,vs . ws,b)
                            Left s -> Left s
      Left s -> Left s

sequence :: [T a] -> T [a]
sequence [] = ret []
sequence (tx:txs) = tx .>>= \x -> sequence txs .>>= \xs -> ret (x:xs)

nextRef :: T Int
nextRef _ n [] = Right (n+1,[],(n:),n)
nextRef _ n (m:ms) = Right (n,ms,id,m)

renameCons :: QName -> T QName
renameCons n isTr = ret (when isTr newModNameQ n) isTr

var :: Int -> T Expr
var x = ret (Var x)

lit :: Literal -> T Expr
lit l = ret (Lit l)

comb :: CombType -> QName -> [T Expr] -> T Expr
comb ct@ConsCall name targs
  = sequence targs .>>= \args  ->
    renameCons name  .>>= \name' ->
    ret (Comb ct name' args)

comb ct@(ConsPartCall m) name targs
  = sequence targs .>>= \args ->
    renameCons name  .>>= \name' ->
    ret (partCons m (Comb ct name' args))

comb ct@FuncCall name targs
  = sequence targs .>>= \args ->
    nextRef .>>= \r ->
    let args' = args++[Var r] in
    if name `elem` implementedInOracle

    then ret (inOraclePartCall (snd name) args') 
    else ret (Comb ct (newModNameQ name) args')
      

comb (FuncPartCall m) name targs
  = sequence targs .>>= \args ->
    ret $ partFunc m $
    if name `elem` implementedInOracle
       then inOraclePartCall (snd name) args
       else pc (newModNameQ name) args
 where
  pc = Comb (FuncPartCall (m+1))

leT :: [(Int,T Expr)] -> T Expr -> T Expr
leT tbs texp
  = sequence (map snd tbs) .>>= \es ->
    nextRef .>>= \r isTr n [] -> case texp isTr n [r] of
      Right (m,ms,fvs,exp) -> let bs = zip (map fst tbs) es
                         in Right (m,[],id,Let bs (event ms r (fvs []) exp))
      Left s -> Left s

freE :: [Int] -> T Expr -> T Expr
freE vs te = sequence (replicate (length vs) freeVar) .>>= \ es ->
             te .>>= \ e ->
             ret (Let (zip vs es) e)
  where
    freeVar = nextRef .>>= (ret . Wrapper.unknown . Var)

oR :: T Expr -> T Expr -> T Expr
oR te1 te2 = te1 .>>= \ e1 -> 
             te2 .>>= \ e2 ->
             ret (FlatCurry.Or e1 e2)

casE :: CaseType -> T Expr -> [Int -> T BranchExpr] -> T Expr
casE ct texp tbs
  = texp .>>= \exp ->
    nextRef .>>= \cr ->
    sequence (map ($cr) tbs) .>>= \bs ->
    ret (Case ct exp bs) 

branch :: Pattern -> T Expr -> Int -> T BranchExpr
branch pat texp cr isTr n []
  = case texp isTr n [cr] of
     Right (m,ms,fvs,exp) -> Right (m,[],id,Branch (updPatCons (when isTr newModNameQ) pat) 
                                                   (event ms cr (fvs []) exp))
     Left s -> Left s
  
    

