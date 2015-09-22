{-# OPTIONS -cpp #-} 
--------------------------------
-- preliminary transformations
--------------------------------
module Curry.Compiler.PreTrans where

import Data.Maybe
import Data.List hiding (nub)

import Curry.FlatCurry.Type
import Curry.FlatCurry.Goodies

#if __GLASGOW_HASKELL__ >= 604
import qualified Data.Map as FM

myFromList = FM.fromList
myLookup = FM.lookup
myMember = FM.member

#else
import qualified Data.FiniteMap as FM

myFromList :: Ord key => [(key, elt)] -> FM.FiniteMap key elt
myFromList = FM.listToFM

myLookup :: Ord key => key -> FM.FiniteMap key elt -> Maybe elt
myLookup = flip FM.lookupFM
myMember = FM.elemFM
#endif

-------------------------------------------------------------------------------
-- some auxiliary functions
-------------------------------------------------------------------------------

transFM p f ps = myFromList (filter (p . snd) (map f (allFuncs ps)))

funcDecls (Prog _ _ _ fs _) = fs

allFuncs ps = concatMap funcDecls ps

--- compute number of arguments by function type 
typeArity :: TypeExpr -> Int
typeArity (TVar _) = 0
typeArity (TCons _ _) = 0
typeArity (FuncType _ t2) = 1+typeArity t2


maxL = foldl max 0 

--- is root type constructor IO?
isIOType :: TypeExpr -> Bool
isIOType = trTypeExpr (const False) (\ q _ -> q==(pre "IO")) (\ _ _ -> False)

------------------------------------------------------------
-- eliminate case on character
------------------------------------------------------------

noCharCase :: Prog -> Prog
noCharCase = updProgFuncs (map (updFuncBody noCCase))

noCCase :: Expr -> Expr
noCCase = trExpr Var Lit Comb Let Free Or noCCaseExpr noCCaseBr

noCCaseExpr :: CaseType -> Expr -> [Expr -> Either (Expr,Expr) BranchExpr] -> Expr
noCCaseExpr ct v bs = 
  either (foldr ifte (Comb FuncCall (pre "failed") [])) (Case ct v) (lrs (map ($ v) bs))
  where
    lrs (Left  x:xs) = Left (x:map (either id (error "PreTrans.noCCaseExpr Right?")) xs)
    lrs (Right x:xs) = Right (x:map (either (error "PreTrans.noCCaseExpr Left?") id) xs)

    ifte (b,e1) e2 = Comb FuncCall (pre "if_then_else") [b,e1,e2]

noCCaseBr :: Pattern -> Expr -> Expr -> Either (Expr,Expr) BranchExpr
noCCaseBr p@(LPattern c@(Charc _)) e v = 
  Left (Comb FuncCall (pre "===") [v,Lit c],e)
noCCaseBr p e _ = Right (Branch p e)

------------------------------------------------------------
-- eliminate nested case expressions
------------------------------------------------------------

--- @param - the program to be transformed
liftCases :: Bool -> Prog -> Prog
liftCases nestedOnly p = 
  let fs  = progFuncs p
      aux = genAuxName (map (snd . funcName) fs)
      (exts,ins) = partition isExternal fs
      (newFsf,_,auxFf) = foldr (liftCasesFunc nestedOnly (progName p) aux) 
                               (id,0,id) 
                               ins
   in updProgFuncs (const (newFsf (auxFf exts))) p

type FuncList = [FuncDecl] -> [FuncDecl]
type Result = (FuncList,Int,FuncList)

liftCasesFunc :: Bool -> String -> String -> FuncDecl -> Result -> Result
liftCasesFunc onlyNested mod aux f (es,i0,ff) = 
  ((updFuncBody (const exp) f:) . es,i',ff . ffe)
  where
    body = funcBody f

    (exp,i',ffe,_) = 
     if onlyNested then (case body of
      Case cm e@(Var v) bs -> 
         let (e',i',ffe,_)    = trans e i0
             (bs',i'',ffbs,_) = 
               fold i' (map (\ (Branch pat be) -> branch pat (trans be)) bs)
          in (Case cm e' bs', i'',ffe . ffbs,[])
      _            -> trans body i0)
     else trans body i0
           
    trans = trExpr var lit comb leT freE or casE branch

    var v i = (Var v,i,id,[v])
    lit l i = (Lit l,i,id,[])
    comb ct n args i = let (args',i',ff,vs) = fold i args
      in (Comb ct n args',i',ff,vs)
    leT bs e i = 
      let (vs,es)  = unzip bs 
          (es',i',ffes,ves) = fold i es
          (e',i'',ffe,ve) = e i'
      in (Let (zip vs es') e',i'', ffes . ffe,
          filter (not . elemOf vs) (ves ++ ve))
    freE vs e i = 
      let (e',i',ff,ve) = e i 
       in (Free vs e',i',ff,filter (not . elemOf vs) ve)
    or e1 e2 i = 
      let ([e1',e2'],i',ff,vs) = fold i [e1,e2]
       in (Or e1' e2',i',ff,vs)
    casE ct e bs i = 
      let (e',i',ffe,ve)     = e i
          (bs',i'',ffbs,vbs) = fold i' bs
          envRes = nub (ve ++ vbs)
          env = case e' of
                  Var v -> delete v envRes
                  _     -> envRes
       in (genFuncCall (snd $ funcName f) mod aux i'' env e',i''+1,
           (genFunc (snd $ funcName f) mod aux i'' env e' ct bs':) . ffe . ffbs,
           envRes)
    branch p e i = 
      let (e',i',ff,ve) = e i
       in (Branch p e',i',ff,removePVars ve p)

fold :: a -> [a -> (c,a,d -> d,[e])] -> ([c],a,d -> d,[e])
fold i = foldr once ([],i,id,[])
  where
    once f (es,j,ff1,vs1) = let (e,k,ff2,vs2) = f j
                             in (e:es,k,ff1 . ff2,vs1++vs2)

genFuncCall :: String -> String -> String -> Int -> [VarIndex] -> Expr -> Expr
genFuncCall f mod aux i env e = 
  Comb FuncCall (mod,f++aux++show i) (map Var env ++ [e])

genFunc :: String -> String -> String -> Int -> [VarIndex] -> Expr ->
           CaseType -> [BranchExpr] -> FuncDecl
genFunc f mod aux i env e ct bs = 
  Func (mod,f++aux++show i) (length env+1) Private (TVar (-42)) $
       Rule (env++[v]) (Case ct (Var v) bs)
  where
    v = case e of 
         Var idx -> idx
         _       -> foldr max 0 env + 1

removePVars :: [Int] -> Pattern -> [Int]
removePVars e = trPattern (\ _ vs -> filter (not . elemOf vs) e) (const e)

genAuxName :: [String] -> String
genAuxName = foldl addUnderscores "_case_"

addUnderscores :: String -> String -> String
addUnderscores n m = if isPrefixOf n m then addUnderscores (n++"_") m else n     

elemOf = flip elem

nub xs = map fst $ FM.toList $ FM.fromList $ zip xs (repeat ())

------------------------------------------------------------
-- elimination of constants
------------------------------------------------------------

externalConstants = map ((,) "Prelude") ["success","failed"] ++
                    map ((,) "IO") ["stdin","stdout","stderr"]

isToElim (Rule _ _) t = typeArity t==0 && t /= TVar (-42)
isToElim (External _) _ = False

mapExp f (Var i) = f (Var i)
mapExp f (Lit l) = f (Lit l)
mapExp f (Comb ct n es) = f (Comb ct n (map (mapExp f) es))
mapExp f (Let vbs e) = let (vs,bs) = unzip vbs in 
  Let (zip vs (map (mapExp f) bs)) (mapExp f e)
mapExp f (Free vs e) = Free vs (mapExp f e)
mapExp f (Or e1 e2) = Or (mapExp f e1) (mapExp f e2)
mapExp f (Case ct e bs) = Case ct (mapExp f e) (map mbr bs)
  where
    mbr (Branch p be) = Branch p (mapExp f be)

elimConsts interfaces p@(Prog pn is ts fs os) = 
  Prog pn is ts (map elimConstsF fs) os
  where
    constsfm = transFM id ftypeArity (p:interfaces)

    ftypeArity (Func mn@(m,n) _ _ t r) = (mn,isToElim r t)

    elimConstsF f@(Func mn@(m,n) a v t (External s)) = f
    elimConstsF (Func n a v t r@(Rule vs e)) 
      | isToElim r t = 
          Func n (a+1) v (FuncType unitType t) 
                 (Rule [maxL (allVars e) + 1] (mapExp elimConstsE e))
      | otherwise = Func n a v t (Rule vs (mapExp elimConstsE e)) 

    elimConstsE e = case e of
      Comb FuncCall fn [] -> if myMember fn constsfm
                               then Comb FuncCall fn [unit]
                               else e
      _ -> e

unit = Comb ConsCall (pre "()") []
unitType = TCons (pre "()") []

pre s = ("Prelude",s)

------------------------------------------------------------
-- typing ambiguous type variables
------------------------------------------------------------

makeTypeMap :: [Prog] -> QName -> QName
makeTypeMap ps s = maybe (errorMsg s) id (myLookup s fm)
  where
    fm = myFromList (concatMap typeMapTypeDecl (concatMap typeDecls ps))
    errorMsg (m,n) = error ("PreTrans.makeTypeMap: cannot find type"++
                            " of constructor "++m++"."++n)

typeMapTypeDecl (TypeSyn _ _ _ _) = []
typeMapTypeDecl (Type typeName _ _ consDecls) = 
  zip (map (\ (Cons name _ _ _) -> name) consDecls) (repeat typeName)

typeDecls (Prog _ _ ts _ _) = ts

------------------------------------------------------------
-- global states 
------------------------------------------------------------

splitGlobals :: Prog -> ([FuncDecl],Prog)
splitGlobals prog  
  | progName prog == "Global" = ([],prog)
  | all okDef toTest = (gs,updProgFuncs (const fs) prog) 
  | otherwise    = error $ "function global not allowed in this context " 
                           ++ show (map funcName (filter (not . okDef) gs))
  where
    (toTest,noGlobal) = partition (containsGlobal . resultType . funcType) 
                                  (progFuncs prog) 

    (gs,fs) = partition isGlobalDecl (progFuncs prog) 

    isGlobal (TCons ("Global","Global") _) = True
    isGlobal _                             = False

    isGlobalDecl f = isGlobal (funcType f) && isGlobalDef (funcBody f)

    containsGlobal (TVar _) = False
    containsGlobal t@(TCons _ args) = isGlobal t || any containsGlobal args
    containsGlobal (FuncType _ _) = False

    isGlobalDef (Comb FuncCall ("Global","global") _) = True
    isGlobalDef _                                     = False

    okDef f 
      | isGlobal (funcType f) && isGlobalDef (funcBody f) = 
        isMonomorph (funcType f)
      | otherwise = noCallToGlobal (funcBody f)

    noCallToGlobal = trExpr (\_->True) (\_->True)
                            (\ _ n args -> n/=("Global","global") 
                                           && and args)
                            (\bs e->and (e:map snd bs)) 
                            (\_ ->id) (&&)
                            (\_ e bs -> and (e:bs)) (\_->id)

isMonomorph :: TypeExpr -> Bool
isMonomorph (TVar _)       = False
isMonomorph (TCons _ xs)   = all isMonomorph xs
isMonomorph (FuncType a b) = all isMonomorph [a,b]

