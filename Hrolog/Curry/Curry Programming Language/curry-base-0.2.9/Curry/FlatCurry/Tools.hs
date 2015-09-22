module Curry.FlatCurry.Tools (

  -- operations on programs:
  progName, progImports, progTypes, progFuncs, progOps,

  updProg, updProgName, updProgImports, updProgTypes, updProgFuncs, updProgOps,

  updProgExps, rnmAllVarsProg, allVarsProg, updQNamesProg,

  rnmProg,

  -- operations on type declarations:
  updQNamesType,allConstructors,consQName, consArity, isTypeSyn, isDataTypeDecl,
  isPublicType, isPublicCons,typeQName,isExternalType,

  -- operations on functions:
  funcName, funcArity, funcVisibility, funcType, funcRule, isPublicFunc,

  updFunc, updFuncName, updFuncArity, updFuncVisibility, updFuncType,
  updFuncRule,

  funcArgs, funcBody, funcRHS, isExternal, isCombFunc,

  updFuncArgs, updFuncBody,

  incVarsFunc, rnmAllVarsFunc, allVarsFunc, updQNamesFunc,

  -- operations on function-rules:
  isRuleExternal, ruleArgs, ruleBody,

  updRule, updRuleArgs, updRuleBody, ruleExtDecl, updRuleExtDecl,

  rnmAllVarsRule, allVarsRule, updQNamesRule,

  -- operations on type-expressions:
  isTypeVar, isFuncType, isTypeCons, typeConsName, argTypes, resultType,
  isIOType,typeArity, allTVars,
  rnmAllVarsTypeExpr, allTypeCons,

  -- operations on expressions:
  isVar, varNr, isLit, isComb, isFree, isOr, isCase, isLet, isGround,
  literal, combType, exprFromFreeDecl, orExps,

  isFuncCall, isPartCall, isConsCall, combFunc, combCons, combArgs,
  missingFuncArgs, hasName, caseBranches,

  rnmAllVars, allVars,

  mapVar, mapLit, mapComb, mapFree, mapOr, mapCase, mapLet,

  -- operations on combination-types
  isCombFuncCall, isCombPartCall, isCombConsCall, missingArgs,

  -- operations on branch-expressions
  branchPattern, branchExpr, isConsPattern,

  updBranch, updBranchPattern, updBranchExpr,

  patCons, patArgs, patLiteral, patExpr, updPatArgs, updPatLiteral,

  rnmAllVarsBranch, allVarsBranch,
  rnmAllVarsPat, allVarsPat,

  -- operations on OpDecls
  opName

  ) where


import Data.Maybe

import Curry.FlatCurry.Type

-- auxiliary functions -------------------------------------------------------

-- infixr 5 -:-

-- (-:-) :: Expr -> Expr -> Expr
-- x -:- xs = Comb ConsCall ("Prelude",":") [x,xs]
--
-- nil :: Expr
-- nil = Comb ConsCall ("Prelude","[]") []
--
-- char_ :: Char -> Expr
-- char_ c = Lit (Charc c)
--
-- int_ :: Integer -> Expr
-- int_ n = Lit (Intc n)
--
-- float_ :: Double -> Expr
-- float_ f = Lit (Floatc f)
--
-- list_ :: [Expr] -> Expr
-- list_ [] = nil
-- list_ (x:xs) = x -:- list_ xs
--
-- string_ :: String -> Expr
-- string_ = list_ . map char_

-- Prog ----------------------------------------------------------------------

updProg :: (String -> String)
        -> ([String] -> [String])
        -> ([TypeDecl] -> [TypeDecl])
        -> ([FuncDecl] -> [FuncDecl])
        -> ([OpDecl] -> [OpDecl])
        -> Prog
        -> Prog
updProg fn fi ft ff fo (Prog name imps types funcs ops)
  = Prog (fn name) (fi imps) (ft types) (ff funcs) (fo ops)

--- get name from program
progName :: Prog -> String
progName (Prog name _ _ _ _) = name

--- update name of program
updProgName :: (String -> String) -> Prog -> Prog
updProgName f = updProg f id id id id

--- get imports from program
progImports :: Prog -> [String]
progImports (Prog _ imps _ _ _) = imps

--- update imports of program
updProgImports :: ([String] -> [String]) -> Prog -> Prog
updProgImports f = updProg id f id id id

--- get type declarations from program
progTypes :: Prog -> [TypeDecl]
progTypes (Prog _ _ types _ _) = types

--- update type declarations of program
updProgTypes :: ([TypeDecl] -> [TypeDecl]) -> Prog -> Prog
updProgTypes f = updProg id id f id id

--- get functions from program
progFuncs :: Prog -> [FuncDecl]
progFuncs (Prog _ _ _ funcs _) = funcs

--- update functions of program
updProgFuncs :: ([FuncDecl] -> [FuncDecl]) -> Prog -> Prog
updProgFuncs f = updProg id id id f id

--- get infix operators from program
progOps :: Prog -> [OpDecl]
progOps (Prog _ _ _ _ ops) = ops

--- update infix operators of program
updProgOps :: ([OpDecl] -> [OpDecl]) -> Prog -> Prog
updProgOps f = updProg id id id id f

--- lift transformation on expressions to program
updProgExps :: (Expr -> Expr) -> Prog -> Prog
updProgExps = updProgFuncs . map . updFuncBody

--- rename programs variables
rnmAllVarsProg :: (Int -> Int) -> Prog -> Prog
rnmAllVarsProg = updProgFuncs . map . rnmAllVarsFunc

--- get all program variables (also from patterns)
allVarsProg :: Prog -> [Int]
allVarsProg = concatMap allVarsFunc . progFuncs

--- update all qualified names in program
updQNamesProg :: (QName -> QName) -> Prog -> Prog
updQNamesProg f
  = updProg id id (map (updQNamesType f)) (map (updQNamesFunc f))
     (map (\ (Op name fix prec) -> Op (f name) fix prec))

rnmProg :: String -> Prog -> Prog
rnmProg name p = updProgName (const name) (updQNamesProg rnm p)
 where
  rnm (modul,n) | modul == progName p = (name,n)
                | otherwise           = (modul,n)


-- TypeDecl ------------------------------------------------------------------

--- select all constructors in a type declaration
allConstructors :: TypeDecl -> [ConsDecl]
allConstructors (TypeSyn _ _ _ _) = []
allConstructors (Type _ _ _ cs) = cs

--- select name of constructor
consQName :: ConsDecl -> QName
consQName (Cons n _ _ _) = n

consArity :: ConsDecl -> Int
consArity (Cons _ a _ _) = a

--- update all qualified names in type declaration
updQNamesType :: (QName -> QName) -> TypeDecl -> TypeDecl
updQNamesType f (Type name vis vars decls)
  = Type (f name) vis vars (map (updQNamesConsDecl f) decls)
updQNamesType f (TypeSyn name vis vars t)
  = TypeSyn (f name) vis vars (updQNamesTypeExpr f t)

--- update all qualified names in constructor declaration
updQNamesConsDecl :: (QName -> QName) -> ConsDecl -> ConsDecl
updQNamesConsDecl f (Cons name arity vis args)
  = Cons (f name) arity vis (map (updQNamesTypeExpr f) args)

isDataTypeDecl :: TypeDecl -> Bool
isDataTypeDecl (TypeSyn _ _ _ _) = False
isDataTypeDecl (Type _ _ _ cs) = not (null cs)

isExternalType :: TypeDecl -> Bool
isExternalType (TypeSyn _ _ _ _) = False
isExternalType (Type _ _ _ cs) = null cs

isTypeSyn :: TypeDecl -> Bool
isTypeSyn (Type _ _ _ _)    = False
isTypeSyn (TypeSyn _ _ _ _) = True

isPublicType :: TypeDecl -> Bool
isPublicType (Type _ vis _ _) = vis==Public
isPublicType (TypeSyn _ vis _ _) = vis==Public

isPublicCons :: ConsDecl -> Bool
isPublicCons (Cons _ _ vis _) = vis==Public

typeQName :: TypeDecl -> QName
typeQName (TypeSyn n _ _ _) = n
typeQName (Type n _ _ _) = n



-- FuncDecl ------------------------------------------------------------------

updFunc :: (QName -> QName)
        -> (Int -> Int)
        -> (Visibility -> Visibility)
        -> (TypeExpr -> TypeExpr)
        -> (Rule -> Rule)
        -> FuncDecl
        -> FuncDecl
updFunc fn fa fv ft fr (Func name arity vis t rule)
  = Func (fn name) (fa arity) (fv vis) (ft t) (fr rule)

--- get name of function
funcName :: FuncDecl -> QName
funcName (Func name _ _ _ _) = name

--- update name of function
updFuncName :: (QName -> QName) -> FuncDecl -> FuncDecl
updFuncName f = updFunc f id id id id

--- get arity of function
funcArity :: FuncDecl -> Int
funcArity (Func _ arity _ _ _) = arity

--- update arity of function
updFuncArity :: (Int -> Int) -> FuncDecl -> FuncDecl
updFuncArity f = updFunc id f id id id

--- get visibility of function
funcVisibility :: FuncDecl -> Visibility
funcVisibility (Func _ _ vis _ _) = vis

--- is function public?
isPublicFunc :: FuncDecl -> Bool
isPublicFunc (Func _ _ vis _ _) = vis == Public

--- update visibility of function
updFuncVisibility :: (Visibility -> Visibility) -> FuncDecl -> FuncDecl
updFuncVisibility f = updFunc id id f id id

--- get type of function
funcType :: FuncDecl -> TypeExpr
funcType (Func _ _ _ t _) = t

--- update type of function
updFuncType :: (TypeExpr -> TypeExpr) -> FuncDecl -> FuncDecl
updFuncType f = updFunc id id id f id

--- get rule of function
funcRule :: FuncDecl -> Rule
funcRule (Func _ _ _ _ rule) = rule

--- update rule of function
updFuncRule :: (Rule -> Rule) -> FuncDecl -> FuncDecl
updFuncRule f = updFunc id id id id f

--- update all qualified names in function
updQNamesFunc :: (QName -> QName) -> FuncDecl -> FuncDecl
updQNamesFunc f = updFunc f id id (updQNamesTypeExpr f) (updQNamesRule f)

-- shortcuts

--- get arguments of function, if not externally defined
funcArgs :: FuncDecl -> Maybe [Int]
funcArgs = ruleArgs . funcRule

--- update arguments of function, if not externally defined
updFuncArgs :: ([Int] -> [Int]) -> FuncDecl -> FuncDecl
updFuncArgs = updFuncRule . updRuleArgs

--- get body of function, if not externally defined
funcBody :: FuncDecl -> Maybe Expr
funcBody = ruleBody . funcRule

--- update body of function, if not externally defined
updFuncBody :: (Expr -> Expr) -> FuncDecl -> FuncDecl
updFuncBody = updFuncRule . updRuleBody

--- get right-hand-sides of function (body without leading case and or nodes)
funcRHS :: FuncDecl -> Maybe [Expr]
funcRHS = maybe Nothing (Just . unwrapCaseOr) . funcBody
 where
  unwrapCaseOr e
    | isCase e
      = concatMap unwrapCaseOr (map branchExpr (caseBranches e))
    | isOr e = concatMap unwrapCaseOr (orExps e)
    | otherwise = [e]

--- is function externally defined?
isExternal :: FuncDecl -> Bool
isExternal = isRuleExternal . funcRule

--- is expression e an application of function f?
--- @*param f - function declaration
--- @*param e - expression
isCombFunc :: FuncDecl -> Expr -> Bool
isCombFunc = hasName . funcName

-- auxiliary functions -------------------------------------------------------

--- increment all variable names in function
incVarsFunc :: Int -> FuncDecl -> FuncDecl
incVarsFunc m = rnmAllVarsFunc (m+)

--- rename all variables in function
rnmAllVarsFunc :: (Int -> Int) -> FuncDecl -> FuncDecl
rnmAllVarsFunc f (Func name arity vis t rule)
  = Func name arity vis t (rnmAllVarsRule f rule)

--- get variable names in a function declaration
allVarsFunc :: FuncDecl -> [Int]
allVarsFunc = allVarsRule . funcRule

-- Rule ----------------------------------------------------------------------

updRule :: ([VarIndex] -> [VarIndex])
        -> (Expr -> Expr)
        -> (String -> String)
        -> Rule
        -> Rule
updRule fa fe _ (Rule args expr) = Rule (fa args) (fe expr)
updRule _ _ f (External s) = External (f s)

--- is rule an external declaration?
isRuleExternal :: Rule -> Bool
isRuleExternal (Rule _ _) = False
isRuleExternal (External _) = True

--- get rules arguments if it's not external
ruleArgs :: Rule -> Maybe [Int]
ruleArgs (Rule args _) = Just args
ruleArgs (External _) = Nothing

--- update rules arguments
updRuleArgs :: ([Int] -> [Int]) -> Rule -> Rule
updRuleArgs f = updRule f id id

--- get rules body if it's not external
ruleBody :: Rule -> Maybe Expr
ruleBody (Rule _ expr) = Just expr
ruleBody (External _)  = Nothing

--- update rules body
updRuleBody :: (Expr -> Expr) -> Rule -> Rule
updRuleBody f = updRule id f id

--- get rules external declaration
ruleExtDecl :: Rule -> Maybe String
ruleExtDecl (Rule _ _ ) = Nothing
ruleExtDecl (External s) = Just s

--- update rules external declaration
updRuleExtDecl :: (String -> String) -> Rule -> Rule
updRuleExtDecl f = updRule id id f

--- update all qualified names in rule
updQNamesRule :: (QName -> QName) -> Rule -> Rule
updQNamesRule = updRuleBody . updQNames

-- auxiliary functions -------------------------------------------------------

--- rename all variables in rule
rnmAllVarsRule :: (Int -> Int) -> Rule -> Rule
rnmAllVarsRule f (Rule args body)
  = Rule (map f args) (rnmAllVars f body)
rnmAllVarsRule _ (External s) = External s

--- get variable names in a functions rule
allVarsRule :: Rule -> [Int]
allVarsRule (Rule args body) = args ++ allVars body
allVarsRule (External _)     = []

-- TypeExpr ------------------------------------------------------------------

--- is type expression a type variable?
isTypeVar :: TypeExpr -> Bool
isTypeVar t = case t of
  TVar _ -> True
  _ -> False

--- is type expression a functional type?
isFuncType :: TypeExpr -> Bool
isFuncType t = case t of
  FuncType _ _ -> True
  _ -> False

--- compute number of arguments by function type
typeArity :: TypeExpr -> Int
typeArity (TVar _) = 0
typeArity (TCons _ _) = 0
typeArity (FuncType _ t2) = 1+typeArity t2

--- is type expression a type constructor?
isTypeCons :: TypeExpr -> Bool
isTypeCons t = case t of
  TCons _ _ -> True
  _ -> False

--- is root type constructor IO?
isIOType :: TypeExpr -> Bool
isIOType t = typeConsName t==Just ("Prelude","IO")

--- get name if type expression is type constructor
typeConsName :: TypeExpr -> Maybe QName
typeConsName t | isTypeCons t = let TCons name _ = t in Just name
               | otherwise = Nothing

--- get argument types from functional type
argTypes :: TypeExpr -> [TypeExpr]
argTypes t = case t of
  FuncType dom ran -> dom : argTypes ran
  _ -> []

--- get result type from (nested) functional type
resultType :: TypeExpr -> TypeExpr
resultType t = case t of
  FuncType _ ran -> resultType ran
  _ -> t

--- rename variables in type declaration
rnmAllVarsTypeExpr :: (Int -> Int) -> TypeExpr -> TypeExpr
rnmAllVarsTypeExpr f (TVar n) = TVar (f n)
rnmAllVarsTypeExpr f (TCons name args)
  = TCons name (map (rnmAllVarsTypeExpr f) args)
rnmAllVarsTypeExpr f (FuncType dom ran)
  = FuncType (rnmAllVarsTypeExpr f dom) (rnmAllVarsTypeExpr f ran)

allTVars :: TypeExpr -> [TVarIndex]
allTVars (TVar n) = [n]
allTVars (TCons _ args) = concatMap allTVars args
allTVars (FuncType t1 t2) = concatMap allTVars [t1,t2]

--- yield the list of all contained type constructors
allTypeCons :: TypeExpr -> [QName]
allTypeCons (TVar _) = []
allTypeCons (TCons name args) = name : concatMap allTypeCons args
allTypeCons (FuncType t1 t2) = allTypeCons t1 ++ allTypeCons t2

--- update all qualified names in type expression
updQNamesTypeExpr :: (QName -> QName) -> TypeExpr -> TypeExpr
updQNamesTypeExpr _ (TVar n) = TVar n
updQNamesTypeExpr f (FuncType dom ran)
  = FuncType (updQNamesTypeExpr f dom) (updQNamesTypeExpr f ran)
updQNamesTypeExpr f (TCons name args)
  = TCons (f name) (map (updQNamesTypeExpr f) args)

-- Expr ----------------------------------------------------------------------

--- is expression a variable?
isVar :: Expr -> Bool
isVar e = case e of
  Var _ -> True
  _ -> False

--- get internal number of variable
varNr :: Expr -> Int
varNr (Var n) = n
varNr _       = error "Curry.FlatCurry.Tools.varNr: no variable"

--- is expression a literal expression?
isLit :: Expr -> Bool
isLit e = case e of
  Lit _ -> True
  _ -> False

--- is expression combined?
isComb :: Expr -> Bool
isComb e = case e of
  Comb _ _ _ -> True
  _ -> False

--- is expression a declaration of free variables?
isFree :: Expr -> Bool
isFree e = case e of
  Free _ _ -> True
  _ -> False

--- is expression an or-expression?
isOr :: Expr -> Bool
isOr e = case e of
  Or _ _ -> True
  _ -> False

--- is expression a case expression?
isCase :: Expr -> Bool
isCase e = case e of
  Case _ _ _ -> True
  _ -> False

--- is expression a let expression?
isLet :: Expr -> Bool
isLet e = case e of
  Let _ _ -> True
  _ -> False

--- is expression fully evaluated?
isGround :: Expr -> Bool
isGround expr
  = case expr of
      Comb ConsCall _ args -> all isGround args
      _ -> isLit expr

--- get literal if expression is literal expression
literal :: Expr -> Maybe Literal
literal e = case e of
  Lit l -> Just l
  _ -> Nothing

--- get combination type if expression is a combined expression
combType :: Expr -> Maybe CombType
combType e = case e of
  Comb ct _ _ -> Just ct
  _ -> Nothing

--- get expression from declaration of free variables
exprFromFreeDecl :: Expr -> Expr
exprFromFreeDecl (Free _ e) = e
exprFromFreeDecl _          = error $ "Curry.FlatCurry.Tools." ++
  "exprFromFreeDecl: no declaration of free variables"

--- get expressions from or-expression
orExps :: Expr -> [Expr]
orExps (Or e1 e2) = [e1,e2]
orExps _          = error "Curry.FlatCurry.Tools.orExps: no or expression"

-- shortcuts

--- is expression a call of a function where all arguments are provided?
isFuncCall :: Expr -> Bool
isFuncCall e = maybe False isCombFuncCall (combType e)

--- is expression a partial call?
isPartCall :: Expr -> Bool
isPartCall e = maybe False isCombPartCall (combType e)

--- is expression a call of a constructor?
isConsCall :: Expr -> Bool
isConsCall e = maybe False isCombConsCall (combType e)

--- get name of function if expression is a (maybe partial) function call
combFunc :: Expr -> Maybe QName
combFunc e
  | isFuncCall e || isPartCall e = let Comb _ name _ = e in Just name
  | otherwise = Nothing

--- get name of constructor if expression is a constructor call
combCons :: Expr -> Maybe QName
combCons e
  | isConsCall e = let Comb _ name _ = e in Just name
  | otherwise = Nothing

--- get arguments if expression is combined
combArgs :: Expr -> Maybe [Expr]
combArgs e | isComb e = let Comb _ _ args = e in Just args
              | otherwise = Nothing

--- get number of missing function arguments if expression is combined
missingFuncArgs :: Expr -> Maybe Int
missingFuncArgs e = combType e >>= Just . missingArgs

--- is expression a combined expression with given name?
hasName :: QName -> Expr -> Bool
hasName name (Comb _ name' _) = name == name'
hasName _    _                = error $ "Curry.FlatCurry.Tools.hasName: " ++
                                        "no combined expression"

--- get branch expressions from case expression
caseBranches :: Expr -> [BranchExpr]
caseBranches (Case _ _ bs) = bs
caseBranches _             = error $ "Curry.FlatCurry.Tools.caseBranches: " ++
                                        "no case expression"

-- auxiliary functions

--- rename all variables (even in patterns) in expression
rnmAllVars :: (Int -> Int) -> Expr -> Expr
rnmAllVars f (Var n) = Var (f n)
rnmAllVars _ (Lit l) = Lit l
rnmAllVars f (Comb ct name args) = Comb ct name (map (rnmAllVars f) args)
rnmAllVars f (Free vs e) = Free (map f vs) (rnmAllVars f e)
rnmAllVars f (Or e1 e2) = Or (rnmAllVars f e1) (rnmAllVars f e2)
rnmAllVars f (Case ct e bs)
  = Case ct (rnmAllVars f e) (map (rnmAllVarsBranch f) bs)
rnmAllVars f (Let bs e)
  = Let (map (\ (n,e') -> (f n,rnmAllVars f e')) bs) (rnmAllVars f e)

--- get all variables (even in patterns) in expression
allVars :: Expr -> [Int]
allVars (Var n) = [n]
allVars (Lit _) = []
allVars (Comb _ _ args) = concatMap allVars args
allVars (Free vs e) = vs ++ allVars e
allVars (Or e1 e2) = allVars e1 ++ allVars e2
allVars (Case _ e bs) = allVars e ++ concatMap allVarsBranch bs
allVars (Let bs e) = concatMap (\ (n,e') -> n:allVars e') bs ++ allVars e

--- map all variables in given expression
mapVar :: (Expr -> Expr) -> Expr -> Expr
mapVar f (Var n) = f (Var n)
mapVar _ (Lit l) = Lit l
mapVar f (Comb ct name args) = Comb ct name (map (mapVar f) args)
mapVar f (Free vs e) = Free vs (mapVar f e)
mapVar f (Or e1 e2) = Or (mapVar f e1) (mapVar f e2)
mapVar f (Case ct e bs)
  = Case ct (mapVar f e) (map (updBranchExpr (mapVar f)) bs)
mapVar f (Let bs e) = Let (map (\ (n,e') -> (n,mapVar f e')) bs) (mapVar f e)

--- map all literals in given expression
mapLit :: (Expr -> Expr) -> Expr -> Expr
mapLit _ (Var n) = Var n
mapLit f (Lit l) = f (Lit l)
mapLit f (Comb ct name args) = Comb ct name (map (mapLit f) args)
mapLit f (Free vs e) = Free vs (mapLit f e)
mapLit f (Or e1 e2) = Or (mapLit f e1) (mapLit f e2)
mapLit f (Case ct e bs)
  = Case ct (mapLit f e) (map (updBranchExpr (mapLit f)) bs)
mapLit f (Let bs e) = Let (map (\ (n,e') -> (n,mapLit f e')) bs) (mapLit f e)

--- map all combined expressions in given expression
mapComb :: (Expr -> Expr) -> Expr -> Expr
mapComb _ (Var n) = Var n
mapComb _ (Lit l) = Lit l
mapComb f (Comb ct name args) = f (Comb ct name (map (mapComb f) args))
mapComb f (Free vs e) = Free vs (mapComb f e)
mapComb f (Or e1 e2) = Or (mapComb f e1) (mapComb f e2)
mapComb f (Case ct e bs)
  = Case ct (mapComb f e) (map (updBranchExpr (mapComb f)) bs)
mapComb f (Let bs e)
  = Let (map (\ (n,e') -> (n,mapComb f e')) bs) (mapComb f e)

--- map all free declarations in given expression
mapFree :: (Expr -> Expr) -> Expr -> Expr
mapFree _ (Var n) = Var n
mapFree _ (Lit l) = Lit l
mapFree f (Comb ct name args) = Comb ct name (map (mapFree f) args)
mapFree f (Free vs e) = f (Free vs (mapFree f e))
mapFree f (Or e1 e2) = Or (mapFree f e1) (mapFree f e2)
mapFree f (Case ct e bs)
  = Case ct (mapFree f e) (map (updBranchExpr (mapFree f)) bs)
mapFree f (Let bs e)
  = Let (map (\ (n,e') -> (n,mapFree f e')) bs) (mapFree f e)

--- map all or expressions in given expression
mapOr :: (Expr -> Expr) -> Expr -> Expr
mapOr _ (Var n) = Var n
mapOr _ (Lit l) = Lit l
mapOr f (Comb ct name args) = Comb ct name (map (mapOr f) args)
mapOr f (Free vs e) = Free vs (mapOr f e)
mapOr f (Or e1 e2) = f (Or (mapOr f e1) (mapOr f e2))
mapOr f (Case ct e bs)
  = Case ct (mapOr f e) (map (updBranchExpr (mapOr f)) bs)
mapOr f (Let bs e) = Let (map (\ (n,e') -> (n,mapOr f e')) bs) (mapOr f e)

--- map all case expressions in given expression
mapCase :: (Expr -> Expr) -> Expr -> Expr
mapCase _ (Var n) = Var n
mapCase _ (Lit l) = Lit l
mapCase f (Comb ct name args) = Comb ct name (map (mapCase f) args)
mapCase f (Free vs e) = Free vs (mapCase f e)
mapCase f (Or e1 e2) = Or (mapCase f e1) (mapCase f e2)
mapCase f (Case ct e bs)
  = f (Case ct (mapCase f e) (map (updBranchExpr (mapCase f)) bs))
mapCase f (Let bs e)
  = Let (map (\ (n,e') -> (n,mapCase f e')) bs) (mapCase f e)

--- map all let expressions in given expression
mapLet :: (Expr -> Expr) -> Expr -> Expr
mapLet _ (Var n) = Var n
mapLet _ (Lit l) = Lit l
mapLet f (Comb ct name args) = Comb ct name (map (mapLet f) args)
mapLet f (Free vs e) = Free vs (mapLet f e)
mapLet f (Or e1 e2) = Or (mapLet f e1) (mapLet f e2)
mapLet f (Case ct e bs)
  = Case ct (mapLet f e) (map (updBranchExpr (mapLet f)) bs)
mapLet f (Let bs e)
  = f (Let (map (\ (n,e') -> (n,mapLet f e')) bs) (mapLet f e))

--- update all qualified names in expression
updQNames :: (QName -> QName) -> Expr -> Expr
updQNames f
  = mapComb (\ (Comb ct name args) -> Comb ct (f name) args)
  . mapCase (\ (Case ct e bs)
              -> Case ct e (map (updBranchPattern (updPatCons f)) bs))

-- CombType ------------------------------------------------------------------

--- is combination type FuncCall?
isCombFuncCall :: CombType -> Bool
isCombFuncCall ct = case ct of
  FuncCall -> True
  _ -> False

--- is combination type PartCall?
isCombPartCall :: CombType -> Bool
isCombPartCall ct = case ct of
  FuncPartCall _ -> True
  ConsPartCall _ -> True
  _ -> False

--- is combination type ConsCall?
isCombConsCall :: CombType -> Bool
isCombConsCall ct = case ct of
  ConsCall -> True
  _ -> False

--- get number of missing args from combination type
missingArgs :: CombType -> Int
missingArgs FuncCall = 0
missingArgs (FuncPartCall n) = n
missingArgs (ConsPartCall n) = n
missingArgs ConsCall = 0  -- ConsCalls need not be fully applied (?)

-- BranchExpr ----------------------------------------------------------------

updBranch :: (Pattern -> Pattern)
          -> (Expr -> Expr)
          -> BranchExpr
          -> BranchExpr
updBranch fp fe (Branch pat expr) = Branch (fp pat) (fe expr)

--- get pattern from branch expression
branchPattern :: BranchExpr -> Pattern
branchPattern (Branch pat _) = pat

--- update pattern of branch expression
updBranchPattern :: (Pattern -> Pattern) -> BranchExpr -> BranchExpr
updBranchPattern f = updBranch f id

--- get expression from branch expression
branchExpr :: BranchExpr -> Expr
branchExpr (Branch _ e) = e

--- update expression of branch expression
updBranchExpr :: (Expr -> Expr) -> BranchExpr -> BranchExpr
updBranchExpr f = updBranch id f

--- is pattern a constructor pattern?
isConsPattern :: Pattern -> Bool
isConsPattern (Pattern _ _) = True
isConsPattern (LPattern _) = False

updPattern :: (QName -> QName)
           -> ([VarIndex] -> [VarIndex])
           -> (Literal -> Literal)
           -> Pattern
           -> Pattern
updPattern fn fa _ (Pattern name args) = Pattern (fn name) (fa args)
updPattern _ _ f (LPattern l) = LPattern (f l)

--- get name if pattern is a constructor pattern
patCons :: Pattern -> Maybe QName
patCons (Pattern name _) = Just name
patCons (LPattern _) = Nothing

--- update constructors name of pattern
updPatCons :: (QName -> QName) -> Pattern -> Pattern
updPatCons f = updPattern f id id

--- get arguments if pattern is a constructor pattern
patArgs :: Pattern -> Maybe [Int]
patArgs (Pattern _ args) = Just args
patArgs (LPattern _) = Nothing

updPatArgs :: ([Int] -> [Int]) -> Pattern -> Pattern
updPatArgs f = updPattern id f id

--- get literal if pattern is a literal pattern
patLiteral :: Pattern -> Maybe Literal
patLiteral (Pattern _ _) = Nothing
patLiteral (LPattern l) = Just l

--- update literal of pattern
updPatLiteral :: (Literal -> Literal) -> Pattern -> Pattern
updPatLiteral f = updPattern id id f

--- build expression from pattern
patExpr :: Pattern -> Expr
patExpr (Pattern name args) = Comb ConsCall name (map Var args)
patExpr (LPattern l) = Lit l

-- auxiliary functions -------------------------------------------------------

--- rename all variables in branch expression
rnmAllVarsBranch :: (Int -> Int) -> BranchExpr -> BranchExpr
rnmAllVarsBranch f (Branch pat e)
  = Branch (rnmAllVarsPat f pat) (rnmAllVars f e)

--- flatten all variables in branch expression
allVarsBranch :: BranchExpr -> [Int]
allVarsBranch (Branch pat e) = allVarsPat pat ++ allVars e

--- rename variables in pattern
rnmAllVarsPat :: (Int -> Int) -> Pattern -> Pattern
rnmAllVarsPat f (Pattern name args) = Pattern name (map f args)
rnmAllVarsPat _ (LPattern l) = LPattern l

--- flatten pattern variables
allVarsPat :: Pattern -> [Int]
allVarsPat = fromMaybe [] . patArgs

-- opDecls ------------------------------

opName :: OpDecl -> QName
opName (Op name _ _) = name