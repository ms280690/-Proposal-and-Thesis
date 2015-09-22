module Curry.Compiler.Simplification (simplifyProg) where


import Prelude hiding ( or,fail,catch )

import Curry.FlatCurry.Type
import Curry.FlatCurry.Goodies hiding ( freeVars )
import qualified Curry.FlatCurry.Goodies as FCG

import Data.List ( sortBy, groupBy, partition )


data Int' = Neg Nat | Zero | Pos Nat
data Nat = IHi | O Nat | I Nat

simplifyProg :: Prog -> Prog
simplifyProg = simplified []

simplified :: [FuncDecl] -> Prog -> Prog
simplified preludeFuncs prog =
  updProgExps (runSimp next rs . evalFamilySimp tExpr opt) prog
 where
  opt = elimSimpleLet `or`
        elimIntLit `or`
        elimFailBranch `or`
        elimCase `or`
        propagate

  next = 1 + maxlist (0:allVarsInProg prog)

  rs = map rule (filter isInlined (preludeFuncs ++ progFuncs prog))
  rule func = (funcName func, funcRule func)

  -- inline only flat constants and if_then_else
  isInlined func =
    not (isExternal func) &&
    (funcName func == (preludeName,if_then_elseName) ||
     isConstant (funcBody func) ||
     isVar (funcBody func))

isConstant :: Expr -> Bool
isConstant exp = isLit exp || (isConsCall exp && null (combArgs exp))


-- elimination of let bindings that occur only once in right-hand side

elimSimpleLet :: Expr -> Simp Expr
elimSimpleLet exp
  | isLet exp && (null keptBs || not (null simpBs))
    = ret (let_ keptBs (replace simpBs e))
  | otherwise = fail
 where
  Let bs e = exp
  (simpBs,keptBs') = partition isSimpleBind bs

  keptBs = map (\ (v,e) -> (v,replace simpBs e)) keptBs'

  freeVarsInBinds = concatMap (freeVars . snd) bs

  isSimpleBind (x,e) =
    isVar e || not (x `elem` freeVarsInBinds) && x `isUniqueIn` exp

isUniqueIn :: VarIndex -> Expr -> Bool
x `isUniqueIn` exp = null xs || null (tail xs)
 where xs = filter (x==) (freeVars exp)


-- elimination of integer literals and patterns

elimIntLit :: Expr -> Simp Expr
elimIntLit exp
  | isLit exp && isIntLit lit = ret (intLitToCons lit)
  | isCase exp && any (isIntPattern . branchPattern) (caseBranches exp)
    = flatCase ct [e] (map nestedBranch bs) fail
  | otherwise = fail
 where
  lit = literal exp
  Case ct e bs = exp

isIntLit :: Literal -> Bool
isIntLit exp = case exp of Intc _ -> True; _ -> False

intLitToCons :: Literal -> Expr
intLitToCons (Intc n) = int_ (intToInt' n)

isIntPattern :: Pattern -> Bool
isIntPattern pat = not (isConsPattern pat) && isIntLit (patLiteral pat)

nestedBranch :: BranchExpr -> ([Expr],Expr)
nestedBranch (Branch pat exp) =
  case patExpr pat of
    Lit (Intc n) -> ([int_ (intToInt' n)], exp)
    pexp -> ([pexp], exp)

-- flattens a case expression.
-- the branches are given as pairs of possibly nested constructor terms
-- and arbitrary right hand sides.
-- multiple arguments of patterns are matched from left to right!
flatCase :: CaseType -> [Expr] -> [([Expr],Expr)] -> Simp Expr -> Simp Expr
flatCase _ [] [] err = err
flatCase _ [] bs@(_:_) _ = ret (foldr1 (?~) (map snd bs))
flatCase ct (e:es) bs err
  | all isVar pats
    = flatCase ct es (map replaceVar bs) err
  | not (null bs) && all isConsCall pats
    = liftSimp (Case ct e) (mapSimp branch groupedBs)
  | otherwise
    = foldr (flatCase ct (e:es)) err (groupBy (lift2 sameKind (head . fst)) bs)
 where
  pats = map (head . fst) bs
  groupedBs = reorderBy (lift2 cmpQName (combName . head . fst)) bs
  sameKind p1 p2 = all isVar [p1,p2] || all isConsCall [p1,p2]

  replaceVar (Var x:ps,rhs) = (ps,Let [(x,e)] rhs)

  branch gbs@((Comb _ name args : _, _) : _) =
    nextVars (length args) .>>= \xs ->
    liftSimp (Branch (Pattern name xs))
      (flatCase ct (map Var xs ++ es) (map extend gbs) err)

  extend (Comb _ _ args : ps, rhs) = (args ++ ps, rhs)

-- elimination of failing branches in case expressions

elimFailBranch :: Expr -> Simp Expr
elimFailBranch exp
  | isCase exp && (null bs || any isFailBranch bs)
    = ret (replaceBranches exp (filter (not . isFailBranch) bs))
  | otherwise = fail
 where
  bs = caseBranches exp

isFailBranch :: BranchExpr -> Bool
isFailBranch = isFailed . branchExpr

isFailed :: Expr -> Bool
isFailed exp = isFuncCall exp && combName exp == (preludeName,failedName)

replaceBranches :: Expr -> [BranchExpr] -> Expr
replaceBranches (Case ct e _) bs
  | null bs   = failed_
  | otherwise = Case ct e bs


-- elimination of case applied to constructor terms

elimCase :: Expr -> Simp Expr
elimCase exp
  | isCase exp && isConsCall scr = match scr (caseBranches exp)
  | otherwise = fail
 where
  scr = caseExpr exp

match :: Expr -> [BranchExpr] -> Simp Expr
match (Comb _ name args) bs
  | null xs = ret failed_
  | otherwise
    = nextVars (length ys) .>>= \zs ->
      ret $ Let (zip zs args) (replace (zip ys (map Var zs)) exp)
 where
  xs = filter ((name==) . patCons . branchPattern) bs
  Branch pat exp : _ = xs
  ys = patArgs pat


-- inlining of functions whose rule is provided

propagate :: Expr -> Simp Expr
propagate exp
  | isFuncCall exp = fetchRule (combName exp) .>>= ret . inline exp
  | otherwise      = fail

inline :: Expr -> Rule -> Expr
inline (Comb _ _ args) (Rule params body) = Let (zip params args) body

-- traversables

tInt :: Traversable Int' Nat
tInt Zero    = noChildren Zero
tInt (Pos n) = ([n], \ [n] -> Pos n)
tInt (Neg n) = ([n], \ [n] -> Neg n)

tNat :: Traversable Nat Nat
tNat IHi   = noChildren IHi
tNat (O n) = ([n], \ [n] -> O n)
tNat (I n) = ([n], \ [n] -> I n)

tExpr :: Traversable Expr Expr
tExpr exp =
  case exp of
    Comb ct name args -> (args, Comb ct name)
    Let bs e -> let (xs,es) = unzip bs in (e:es, \ (e:es) -> Let (zip xs es) e)
    Free xs e -> ([e], \ [e] -> Free xs e)
    Or e1 e2 -> ([e1,e2], \ [e1,e2] -> Or e1 e2)
    Case ct e bs -> let (ps,es) = unzip (map branch bs)
                     in (e:es, \ (e:es) -> Case ct e (zipWith Branch ps es))
    _ -> noChildren exp
 where
  branch (Branch p e) = (p,e)

tBranchExpr :: Traversable BranchExpr Expr
tBranchExpr (Branch pat exp) = ([exp], \ [exp] -> Branch pat exp)

tTypeExpr :: Traversable TypeExpr TypeExpr
tTypeExpr typ =
  case typ of
    FuncType dom ran -> ([dom,ran], \ [dom,ran] -> FuncType dom ran)
    TCons name args -> (args, TCons name)
    _ -> noChildren typ


-- comparison

type Ord' a = a -> a -> Ordering

reorderBy :: Ord' a -> [a] -> [[a]]
reorderBy cmp = groupBy eq . sortBy cmp
 where
  eq x y = cmp x y == EQ

cmpQName :: Ord' QName
cmpQName = cmpPair cmpString cmpString

cmpPair :: Ord' a -> Ord' b -> Ord' (a,b)
cmpPair cmpa cmpb (a1,b1) (a2,b2) = 
  case cmpa a1 a2 of
    EQ -> cmpb b1 b2
    cmp -> cmp


-- creating FlatCurry expressions

let_ bs e = if null bs then e else Let bs e

preludeName = "Prelude"
if_then_elseName = "if_then_else"
failedName = "failed"

failed_ :: Expr
failed_ = Comb FuncCall (preludeName,failedName) []

zero_ = Comb ConsCall (preludeName, "Zero") []
pos_ n = Comb ConsCall (preludeName, "Pos") [n]
neg_ n = Comb ConsCall (preludeName, "Neg") [n]

iHi_ = Comb ConsCall (preludeName, "IHi") []
o_ n = Comb ConsCall (preludeName, "O") [n]
i_ n = Comb ConsCall (preludeName, "I") [n]

x ?~ y = Comb FuncCall (preludeName, "?") [x,y]

int_ :: Int' -> Expr
int_ = foldChildren tInt tNat intExp natExp
 where
  intExp Zero    _   = zero_
  intExp (Pos _) [n] = pos_ n
  intExp (Neg _) [n] = neg_ n

  natExp IHi     _   = iHi_
  natExp (O _)   [n] = o_ n
  natExp (I _)   [n] = i_ n


-- auxiliary functions

lift2 :: (a -> a -> c) -> (b -> a) -> (b -> b -> c)
lift2 op f x y = op (f x) (f y)

stripSuffix :: String -> String -> String
stripSuffix suf str
  | suf `isSuffixOf` str = take (length str - length suf) str
  | otherwise = str

isSuffixOf, isPrefixOf :: Eq a => [a] -> [a] -> Bool
suf `isSuffixOf` l = reverse suf `isPrefixOf` reverse l

[] `isPrefixOf` _ = True
(x:xs) `isPrefixOf` (y:ys) = x==y && xs `isPrefixOf` ys


-- compute free variables of expression

freeVars :: Expr -> [VarIndex]
freeVars = outOfScopeVars []

outOfScopeVars :: [VarIndex] -> Expr -> [VarIndex]
outOfScopeVars scope exp = fold tExpr vars exp scope
 where
  vars exp cs scope =
    case (exp,cs) of
      (Var n,_) -> if n `elem` scope then [] else [n]
      (Let bs _,_) ->
        concatMap ( $ filter (not . (`elem` map fst bs)) scope) cs
      (Free vs _,[e]) -> e (filter (not . (`elem` vs)) scope)
      (Case _ _ bs,e:es) ->
        e scope ++ concat (zipWith (scopeBranch scope) bs es)
      _ -> concatMap ( $ scope) cs

  scopeBranch scope (Branch pat _) e
    | isConsPattern pat = e (filter (not . (`elem` patArgs pat)) scope)
    | otherwise = e scope


-- replace free variables in expression according to environment

type Env   = [(VarIndex,Expr)]

replace :: Env -> Expr -> Expr
replace env exp
  | isVar  exp = fromEnv [] (varNr exp) env 
  | isLet  exp = mapChildren tExpr (replace (removeLetBinds exp env)) exp
  | isFree exp = mapChildren tExpr (replace (remove (FCG.freeVars exp) env)) exp
  | isCase exp = let Case ct e bs = exp
                  in Case ct (replace env e) (map (replaceBranch env) bs)
  | otherwise  = mapChildren tExpr (replace env) exp

fromEnv :: [VarIndex] -> VarIndex -> Env -> Expr
fromEnv is i env = case lookup i env of
  Nothing -> Var i
  Just (Var j) -> if elem j is then Comb FuncCall ("Prelude","failed") [] 
                               else fromEnv (j:is) j env
  Just e  -> replace env e

remove :: [VarIndex] -> Env -> Env
remove xs env = filter (not . (`elem`xs) . fst) env

removeLetBinds :: Expr -> Env -> Env
removeLetBinds = remove . map fst . letBinds

replaceBranch :: Env -> BranchExpr -> BranchExpr
replaceBranch env b =
  mapChildren tBranchExpr (replace (remove (patArgs (branchPattern b)) env)) b

maxlist :: [Int] -> Int
maxlist [n] = n
maxlist (n:m:ns) = max n (maxlist (m:ns))


--- A datatype is <code>Traversable</code> if it defines a function
--- that can decompose a value into a list of children of the same type
--- and recombine new children to a new value of the original type. 
---
type Traversable a b = a -> ([b], [b] -> a)

--- Traversal function for constructors without children.
---
noChildren :: Traversable a b
noChildren x = ([], const x)

--- Yields the children of a value.
---
children :: Traversable a b -> a -> [b]
children tr = fst . tr

--- Replaces the children of a value.
--- 
replaceChildren :: Traversable a b -> a -> [b] -> a
replaceChildren tr = snd . tr

--- Applies the given function to each child of a value.
---
mapChildren :: Traversable a b -> (b -> b) -> a -> a
mapChildren tr f x = replaceChildren tr x (map f (children tr x))

--- Computes a list of the given value, its children, those children, etc.
---
family :: Traversable a a -> a -> [a]
family tr x = familyFL tr x []

--- Computes a list of family members of the children of a value.
--- The value and its children can have different types.
---
childFamilies :: Traversable a b -> Traversable b b -> a -> [b]
childFamilies tra trb x = childFamiliesFL tra trb x [] 

-- implementation of 'family' with functional lists for efficiency reasons

type FunList a = [a] -> [a]

familyFL :: Traversable a a -> a -> FunList a
familyFL tr x xs = x : childFamiliesFL tr tr x xs

childFamiliesFL :: Traversable a b -> Traversable b b -> a -> FunList b
childFamiliesFL tra trb x xs = concatFL (map (familyFL trb) (children tra x)) xs

--- Concatenates a list of functional lists.
---
concatFL :: [FunList a] -> FunList a
concatFL [] ys = ys
concatFL (x:xs) ys = x (concatFL xs ys)

--- Applies the given function to each member of the family of a value.
--- Proceeds bottom-up.
---
mapFamily :: Traversable a a -> (a -> a) -> a -> a
mapFamily tr f = f . mapChildFamilies tr tr f

--- Applies the given function to each member of the families of the children
--- of a value. The value and its children can have different types.
--- Proceeds bottom-up.
---
mapChildFamilies :: Traversable a b -> Traversable b b -> (b -> b) -> a -> a
mapChildFamilies tra trb = mapChildren tra . mapFamily trb

--- Applies the given function to each member of the family of a value 
--- as long as possible. On each member of the family of the result the given
--- function will yield <code>Nothing</code>.
--- Proceeds bottom-up.
---
evalFamily :: Traversable a a -> (a -> Maybe a) -> a -> a
evalFamily tr f = mapFamily tr g
 where g x = maybe x (mapFamily tr g) (f x)

--- Applies the given function to each member of the families of the children
--- of a value as long as possible.
--- Similar to 'evalFamily'.
---
evalChildFamilies :: Traversable a b -> Traversable b b
                  -> (b -> Maybe b) -> a -> a
evalChildFamilies tra trb = mapChildren tra . evalFamily trb

--- Implements a traversal similar to a fold with possible default cases.
---
fold :: Traversable a a -> (a -> [r] -> r) -> a -> r
fold tr f = foldChildren tr tr f f

--- Fold the children and combine the results.
---
foldChildren :: Traversable a b -> Traversable b b
             -> (a -> [rb] -> ra) -> (b -> [rb] -> rb) -> a -> ra
foldChildren tra trb f g a = f a (map (fold trb g) (children tra a))

infixl 1 .>>=, .>>


type Rules = [(QName,Rule)]
type Simp a = VarIndex -> Rules -> Maybe (a,VarIndex)

runSimp :: Int -> Rules -> Simp a -> a
runSimp n rs o =
  maybe (error "Simplification.runSimp: simplification fails") fst (o n rs)

ret :: a -> Simp a
ret x n _ = Just (x,n)

(.>>=) :: Simp a -> (a -> Simp b) -> Simp b
(oa .>>= f) n rs = 
  case oa n rs of
    Nothing -> Nothing
    Just (a,n) -> f a n rs

(.>>) :: Simp b -> Simp a -> Simp a
o .>> oa = o .>>= const oa

liftSimp :: (a -> b) -> Simp a -> Simp b
liftSimp f oa = oa .>>= ret . f

fail :: Simp a
fail _ _ = Nothing

catch :: Simp a -> Simp a -> Simp a
catch o1 o2 n rs = maybe (o2 n rs) Just (o1 n rs) 

or :: (a -> Simp b) -> (a -> Simp b) -> a -> Simp b
or f g a = catch (f a) (g a)

nextVar :: Simp VarIndex
nextVar n _ = Just (n,n+1)

nextVars :: Int -> Simp [VarIndex]
nextVars n = sequenceSimp (replicate n nextVar)

fetchRule :: QName -> Simp Rule
fetchRule name n rs = maybe Nothing defRule (lookup name rs)
 where
  defRule (Rule args body) = 
    let arity = length args
        args' = take arity [n ..]
     in Just (Rule args' (replace (zip args (map Var args')) body)
             ,n+arity)
  defRule (External _) = Nothing

sequenceSimp :: [Simp a] -> Simp [a]
sequenceSimp [] = ret []
sequenceSimp (ox:oxs) = ox .>>= \x -> sequenceSimp oxs .>>= \xs -> ret (x:xs)

mapSimp :: (a -> Simp b) -> [a] -> Simp [b]
mapSimp f = sequenceSimp . map f


replaceChildrenSimp :: Traversable a b -> a -> Simp [b] -> Simp a
replaceChildrenSimp tr = liftSimp . replaceChildren tr

mapChildrenSimp :: Traversable a b -> (b -> Simp b) -> a -> Simp a
mapChildrenSimp tr f a = replaceChildrenSimp tr a (mapSimp f (children tr a))

mapFamilySimp :: Traversable a a -> (a -> Simp a) -> a -> Simp a
mapFamilySimp tr f a = mapChildFamiliesSimp tr tr f a .>>= f

mapChildFamiliesSimp :: Traversable a b -> Traversable b b
                    -> (b -> Simp b) -> a -> Simp a
mapChildFamiliesSimp tra trb = mapChildrenSimp tra . mapFamilySimp trb

evalFamilySimp :: Traversable a a -> (a -> Simp a) -> a -> Simp a
evalFamilySimp tr f = mapFamilySimp tr g
 where g a = catch (f a .>>= mapFamilySimp tr g) (ret a)

evalChildFamiliesSimp :: Traversable a b -> Traversable b b
                     -> (b -> Simp b) -> a -> Simp a
evalChildFamiliesSimp tra trb = mapChildrenSimp tra . evalFamilySimp trb

cmpString :: String -> String -> Ordering
cmpString = compare

intToInt' :: Prelude.Integral a => a -> Int'
intToInt' n = case Prelude.compare n 0 of
 LT -> Neg (intToNat (Prelude.abs n))
 EQ -> Zero
 GT -> Pos (intToNat (Prelude.abs n))

intToNat :: Prelude.Integral a => a -> Nat
intToNat n = case Prelude.mod n 2 of
              1 -> if m Prelude.== 0 then IHi else I (intToNat m)
              0 -> O (intToNat m)
  where m = Prelude.div n 2

