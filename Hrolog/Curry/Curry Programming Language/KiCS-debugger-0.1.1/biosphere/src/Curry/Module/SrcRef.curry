{-

  There is no support for function patterns nor record syntax.

  Issues:
    source references for variables in mathcing pattern declarations 
    are incorrect.

-}

module SrcRef (
  SrcRef,
  InfoTree,
  getInfoTree,nextStaticInfo,infoChildren,

  srcAt,treeN,


  bigTest,testAt,testAddTree,testZip) 

   where

import qualified FlatCurry as FC
import qualified FlatCurryGoodies as FG
import CurrySyntax
import ReadShowTerm
import System
import FiniteMap
import Sort
import List
import Float

import Unsafe (trace)

--------------------------------------------
-- the interface 
--------------------------------------------

type SrcRef = Int
type Eq a = Maybe (Equation a)


data SrcEntity a = SrcDecl (Decl a) 
                 | SrcCon  (Decl a) (ConstrDecl a)
                 | SrcConTerm (Decl a) (Eq a) (ConstrTerm a)
                 | SrcNew  (Decl a) (NewConstrDecl a)
                 | SrcRhs  (Decl a) (Eq a) (Rhs a)
                 | SrcId   (Decl a) Ident
                 | SrcCond (Decl a) (Eq a) (Expression a) (Expression a)
                 | SrcExpr (Decl a) (Eq a) (Expression a)
                 | SrcAlt  (Decl a) (Eq a) (Alt a)

type InfoTree = [AdrTree [SrcRef]]

nextStaticInfo :: InfoTree -> ([SrcRef],InfoTree)
nextStaticInfo [] = error "empty infotree"
nextStaticInfo (Tree x xs:adrs) = (x,xs ++ adrs)

infoChildren :: InfoTree -> [InfoTree]
infoChildren = map (:[]) 

getInfoTree :: FC.Prog -> IO InfoTree
getInfoTree p@(FC.Prog m _ _ _ _) = do
  readCurry m >>= return . zipFlatCy p 

--------------------------------------------
-- implementation 
--------------------------------------------

treeN :: Int -> InfoTree
treeN _ = trees where trees = tr:trees
                      tr = Tree [0] trees 



----------------------------------------------------------
-- the instance to find a source entity at given position
----------------------------------------------------------

srcAt :: Module p -> SrcRef -> Maybe (SrcEntity p)
srcAt m ref = either Just (const Nothing) $
  moduleAt m ref (\ x r -> if r==0 then Left x else Right (r-1))

-------------------------------------------
-- some basic connectors definitions and
-- connectors 
-------------------------------------------

-- an action tells what to do with a source entity
type Action a b p = SrcEntity p -> a -> Either b a
              

-- a source selector decides whether to apply the action 
-- to this part of the source
type Selector a b p = a -> Action a b p -> Either b a

here :: SrcEntity p -> Selector a b p
here se ref h = h se ref

notHere :: Selector a b p
notHere ref _ = Right ref

-- is it left or right?

(.*) :: Selector a b p -> Selector a b p -> Selector a b p
(f .* g) ref h = either Left (flip g h) (f ref h)

-- is it part of this list?
inListAt :: (a -> Selector b c p) -> [a] -> Selector b c p
inListAt _ []     = notHere 
inListAt f (x:xs) = f x .* inListAt f xs

-------------------------------------------
-- traversing the program
-------------------------------------------

-- srcAt for a program and a reference 
-- -> retrieve source entity at address
moduleAt :: Module p -> Selector a b p 
moduleAt (Module _ _ ds) = declsAt ds 

declsAt :: [Decl p] -> Selector a b p
declsAt = inListAt declAt

declAt :: Decl p -> Selector a b p
declAt (ImportDecl _ _ _ _ _) = notHere 
declAt d@(InfixDecl _ _ _ is) = inListAt (here . SrcId d) is 
declAt d@(DataDecl _ _ _ cs)  = 
  here (SrcDecl d) .* inListAt (here . SrcCon d) cs 
declAt d@(NewtypeDecl _ _ _  c) = here (SrcDecl d) .* here (SrcNew d c)
declAt d@(TypeDecl _ _ _ _)  = here (SrcDecl d) 
declAt (TypeSig _ _ _)       = notHere 
declAt (EvalAnnot _ _ _)     = notHere 
declAt d@(FunctionDecl _ _ es) 
  = here (SrcDecl d) .* eqsAt d es
declAt d@(ExternalDecl _ _ _ i _) = here (SrcId d i) 
declAt (FlatExternalDecl _ _) = notHere 
declAt d@(PatternDecl _ t rhs) 
  = here (SrcDecl d) .* conTermAt (d,Nothing) t .* rhsAt (d,Nothing) rhs
declAt d@(ExtraVariables _ is) = inListAt (here . SrcId d) is


locsAt :: [Decl p] -> Selector a b p
locsAt = declsAt

type EqContext p = Decl p
type ExpContext p = (Decl p,Eq p)

eqsAt :: EqContext p -> [Equation p] -> Selector a b p
eqsAt d = inListAt (eqAt d)

eqAt :: EqContext p -> Equation p -> Selector a b p
eqAt d eq@(Equation _ l r) = 
  (lhsAt (d,Just eq) l) .* (rhsAt (d,Just eq) r)

lhsAt :: ExpContext p -> Lhs p -> Selector a b p
lhsAt c (FunLhs _ cs)   = conTermsAt c cs
lhsAt c (OpLhs t1 _ t2) = conTermAt c t1 .* conTermAt c t2
lhsAt c (ApLhs l cs)    = lhsAt c l .* conTermsAt c cs
  
conTermsAt :: ExpContext p -> [ConstrTerm p] -> Selector a b p 
conTermsAt c = inListAt (conTermAt c)

conTermAt :: ExpContext p -> ConstrTerm p -> Selector a b p
conTermAt c@(d,eq) t = case t of
      ParenPattern t'           -> conTermAt c t' 
      LiteralPattern (String s) -> 
        stringAt (SrcConTerm d eq . LiteralPattern) s 
      ListPattern cs  -> listExpAt (conTermAt c) 
                           (here . SrcConTerm d eq . ListPattern)
                           cs 
      _ -> here (SrcConTerm d eq t) .* 
         (case t of
           ConstructorPattern _ cs   -> conTermsAt c cs  
           InfixPattern t1 _ t2 -> (conTermAt c t1 .* conTermAt c t2) 
           TuplePattern cs -> conTermsAt c cs 
           AsPattern _ p   -> conTermAt c p 
           LazyPattern p   -> conTermAt c p
           FunctionPattern _ cs -> conTermsAt c cs 
           InfixFuncPattern t1 _ t2 -> conTermAt c t1 .* conTermAt c t2
           --RecordPattern [Field a (ConstrTerm a)] (Maybe (ConstrTerm a))
           _ -> notHere)


stringAt :: (Literal -> SrcEntity p) -> String -> Selector a b p
stringAt f = listExpAt (litAt f . Char) (here . f . String)

listExpAt :: (e -> Selector a b p) -> ([e] -> Selector a b p) 
          -> [e] -> Selector a b p
listExpAt _ fs []         = fs []
listExpAt f fs xxs@(x:xs) = fs xxs .* f x .* listExpAt f fs xs

litAt :: (Literal -> SrcEntity p) -> Literal -> Selector a b p
litAt f l = case l of
  String s -> stringAt f s 
  _        -> here (f l) 



rhsAt :: ExpContext p -> Rhs p -> Selector a b p
rhsAt c@(d,eq) r@(SimpleRhs _ e ls)  
  = here (SrcRhs d eq r) .* expAt c e .* locsAt ls
rhsAt c@(d,eq) r@(GuardedRhs conds ls) 
  = here (SrcRhs d eq r) .* condsAt c conds .* locsAt ls

condsAt :: ExpContext p -> [CondExpr p] -> Selector a b p
condsAt c = inListAt (condAt c)

condAt :: ExpContext p -> CondExpr p -> Selector a b p
condAt c@(d,eq) (CondExpr _ e1 e2) = 
  here (SrcCond d eq e1 e2) .* expAt c e1 .* expAt c e2

srcExpr :: ExpContext p -> Expression p -> SrcEntity p
srcExpr (d,eq) = SrcExpr d eq

expsAt :: ExpContext p -> [Expression p] -> Selector a b p
expsAt c = inListAt (expAt c)


expAt :: ExpContext p -> Expression p -> Selector a b p
expAt c e@(Variable _)    = here (srcExpr c e) 
expAt c (Literal l)       = litAt (srcExpr c . Literal) l 
expAt t c@(Constructor _) = here (srcExpr t c) 
expAt c (Paren e)         = expAt c e 
expAt c t@(Typed e _)     = here (srcExpr c t) .* expAt c e
expAt c e@(Tuple es)      = here (srcExpr c e) .* expsAt c es
expAt c (List es)         = listExpAt (expAt c) (here . srcExpr c . List) es
expAt c e@(EnumFrom e')   = here (srcExpr c e) .* expAt c e' 
expAt c e@(EnumFromThen e' e'') = 
  here (srcExpr c e) .* expAt c e' .* expAt c e''
expAt c e@(EnumFromTo e1 e2) = 
  here (srcExpr c e) .* expAt c e1 .* expAt c e2
expAt c e@(EnumFromThenTo e1 e2 e3) = 
  here (srcExpr c e) .* expAt c e1 .* expAt c e2 .* expAt c e3
expAt c e@(UnaryMinus _ e1) = here (srcExpr c e) .* expAt c e1
expAt c e@(Apply e1 e2) = here (srcExpr c e) .* expAt c e1 .* expAt c e2
expAt c e@(InfixApply e1 _ e2) = here (srcExpr c e) .* expAt c e1 .* expAt c e2
expAt c e@(LeftSection e1 _) = here (srcExpr c e) .* expAt c e1
expAt c e@(RightSection _ e1) = here (srcExpr c e) .* expAt c e1
expAt c e@(Lambda cs e1) = here (srcExpr c e) .* conTermsAt c cs .* expAt c e1
expAt c e@(IfThenElse e1 e2 e3) = 
  here (srcExpr c e) .* expAt c e1 .* expAt c e2 .* expAt c e3
expAt c (ListCompr e1 stmts) = 
  listExpAt (stmtAt c) final (stmts ++ [StmtExpr e1])
  where
    final [] = notHere
    final xs@(_:_) = here $  srcExpr c $ ListCompr e1 $ init xs
expAt c (Do stmts e1) = doAt c e1 stmts
expAt c e@(Case e1 alts) =
  here (srcExpr c e) .* expAt c e1 .* altsAt c alts
expAt c e@(Let ds e1) = 
  here (srcExpr c e) .* declsAt ds .* expAt c e1
expAt _ (RecordConstr _) = error "record1"
 -- [Field a (Expression a)]
expAt _ (RecordSelection _ _) = error "record2"
 -- (Expression a) Idente
expAt _ (RecordUpdate _ _)= error "record3"
 --[Field a (Expression a)] (Expression a)


doAt :: ExpContext p -> Expression p -> [Statement p] -> Selector a b p
doAt c e1 stmts = listExpAt (stmtAt c) maybeFinalGenerator stmts 
  where
    maybeFinalGenerator []       = expAt c e1
    maybeFinalGenerator xs@(_:_) = here (srcExpr c (Do xs e1))

stmtAt :: ExpContext p -> Statement p -> Selector a b p
stmtAt c (StmtExpr e)   = expAt c e
stmtAt _ (StmtDecl ds)  = declsAt ds
stmtAt c (StmtBind p e) = conTermAt c p .* expAt c e


altsAt :: ExpContext p -> [Alt p] -> Selector a b p
altsAt a = inListAt (altAt a)

altAt :: ExpContext p -> Alt p -> Selector a b p
altAt c a@(Alt _ p rhs) = 
  here (SrcAlt (err "decl") (err "eq") a) .* conTermAt c p .* rhsAt c rhs
  where
    err s = error $ "no context for "++s++"in alternative"

-------------------------------------------
-- Getting the whole SrcRef Information
-------------------------------------------

data AdrTree a = Tree a [AdrTree a]

type Tree = AdrTree (Maybe SrcRef) 

type State = SrcRef 

type Add a = State -> (State,a)

ret :: a -> Add a
ret x s = (s,x)

infixl 1 .>>, .>>=

(.>>=) :: Add a -> (a -> Add b) -> Add b
(f .>>= g) s = case f s of
                (s',x) -> g x s'

(.>>) :: Add _ -> Add b -> Add b
f .>> g = f .>>= const g

nope :: Add Tree
nope = ret (Tree Nothing [])

add :: (SrcRef -> a) -> Add a
add f r = (r+1,f r)

inc :: Int -> Add ()
inc i r = (r+i,())

next :: Add (Maybe Int)
next r = (r+1,Just r)

this :: Add Int
this r = (r,r)

run :: Add a -> a
run f = snd (f 0)


-- srcAdd for a program and a reference 
-- -> retrieve source entity Add address
moduleAdd :: Module Pos -> Add [Tree]
moduleAdd (Module _ _ ds) = declsAdd ds

declsAdd :: [Decl Pos] -> Add [Tree]
declsAdd = inListAdd declAdd

declAdd :: Decl Pos -> Add Tree
declAdd (ImportDecl _ _ _ _ _) = nope
declAdd (InfixDecl _ _ _ is) = 
  inListAdd (const $ next .>>= ret . flip Tree []) is .>>=
  ret . Tree Nothing
declAdd (DataDecl _ _ _ cs)  = 
  next .>>= \ r ->
  inListAdd constrDeclAdd cs .>>= 
  ret . Tree r 
declAdd (NewtypeDecl _ _ _ c) = 
  next .>>= \r ->  
  newConstrDeclAdd c .>>= 
  ret . Tree r . (:[])
declAdd (TypeDecl _ _ _ _)  = 
  next .>>= ret . flip Tree []
declAdd (TypeSig _ _ _) = nope
declAdd (EvalAnnot _ _ _) = nope
declAdd (FunctionDecl _ _ es) =
  next .>>= \r ->  
  eqsAdd es .>>= 
  ret . Tree r
declAdd (ExternalDecl _ _ _ _ _) = 
  next .>>= ret . flip Tree []
declAdd (FlatExternalDecl _ _) = nope 
declAdd (PatternDecl _ t rhs) 
  = next .>>= \r ->
    conTermAdd t .>>= \ t' ->
    rhsAdd rhs .>>= \ r' ->
    ret (Tree r [t',r'])
declAdd (ExtraVariables _ is) = 
  inListAdd (const $ next .>>= ret . flip Tree []) is .>>= 
  ret . Tree Nothing


constrDeclAdd :: ConstrDecl Pos -> Add Tree
constrDeclAdd (ConstrDecl _ _ _ _) = 
  next .>>= ret . flip Tree []
constrDeclAdd (ConOpDecl _ _ _ _ _) = 
  next .>>= ret . flip Tree []

newConstrDeclAdd :: NewConstrDecl Pos -> Add Tree
newConstrDeclAdd = error "adding to new declaration"

locsAdd :: [Decl Pos] -> Add [Tree]
locsAdd = declsAdd

eqsAdd :: [Equation Pos] -> Add [Tree] 
eqsAdd = inListAdd eqAdd

eqAdd :: Equation Pos -> Add Tree
eqAdd (Equation _ l r) = 
  lhsAdd l .>>= \ l' ->
  rhsAdd r .>>= \ r' -> 
  ret (Tree Nothing [l',r'])

lhsAdd :: Lhs Pos -> Add Tree
lhsAdd (FunLhs _ cs)   = conTermsAdd cs .>>= ret . Tree Nothing
lhsAdd (OpLhs t1 _ t2) = 
  conTermAdd t1 .>>= \ t1' -> 
  conTermAdd t2 .>>= \ t2' ->
  ret (Tree Nothing [t1',t2'])
lhsAdd (ApLhs l cs) = 
  lhsAdd l .>>= \ l' -> 
  conTermsAdd cs .>>= 
  ret . Tree Nothing . (l':)
  
conTermAdd :: ConstrTerm Pos -> Add Tree
conTermAdd t = case t of
      ParenPattern t' -> conTermAdd t'
      LiteralPattern l -> litAdd l
      NegativePattern _ l -> litAdd l
      ListPattern cs   -> listExpAdd conTermAdd cs 
      _ -> next .>>= \r->
         (case t of

           ConstructorPattern _ cs -> 
             conTermsAdd cs .>>= ret . Tree r
           InfixPattern t1 _ t2 -> 
             conTermAdd t1 .>>= \ t1' -> 
             conTermAdd t2 .>>= \ t2' ->
             ret (Tree r [t1',t2'])
           TuplePattern cs -> conTermsAdd cs .>>= ret . Tree r
           AsPattern _ p   -> conTermAdd p 
           LazyPattern p   -> conTermAdd p 
           FunctionPattern _ cs -> 
             conTermsAdd cs .>>= ret . Tree r
           InfixFuncPattern t1 _ t2 -> 
             conTermAdd t1 .>>= \ t1' -> 
             conTermAdd t2 .>>= \ t2' ->
             ret (Tree r [t1',t2'])
           VariablePattern _ -> ret (Tree r [])
           _ -> ret (Tree r []))


conTermsAdd :: [ConstrTerm Pos] -> Add [Tree] 
conTermsAdd = inListAdd conTermAdd

stringAdd :: String -> Add Tree
stringAdd = listExpAdd (\_-> next .>>= ret . flip Tree [])

listExpAdd :: (a -> Add Tree) -> [a] -> Add Tree
listExpAdd f = listExpAdd' f (next .>>= ret . flip Tree [])

listExpAdd' :: (a -> Add Tree) -> Add Tree -> [a] -> Add Tree
listExpAdd' _ e []     = e
listExpAdd' f e (x:xs) = next .>>= \ rxxs ->
                        f x  .>>= \ tx ->
                        listExpAdd' f e xs .>>= \ txs -> 
                        ret (Tree rxxs [tx,txs])


litAdd :: Literal -> Add Tree
litAdd l = case l of
  String s -> stringAdd s 
  _        -> next .>>= ret . flip Tree []


rhsAdd :: Rhs Pos -> Add Tree
rhsAdd (SimpleRhs _ e ls) = 
  next .>>= \ r -> 
  expAdd e .>>= \ e' ->
  locsAdd ls .>>= ret . Tree r . (e':)
rhsAdd (GuardedRhs conds ls) =
  next .>>= \ r -> 
  condsAdd conds .>>= \ conds' ->
  locsAdd ls .>>= ret . Tree r . (conds'++)

condsAdd :: [CondExpr Pos] -> Add [Tree]
condsAdd = inListAdd condAdd

condAdd :: CondExpr Pos -> Add Tree
condAdd (CondExpr _ e1 e2) = 
  next .>>= \ r -> 
  expAdd e1 .>>= \ e1' -> 
  expAdd e2 .>>= \ e2' ->
  ret (Tree r [e1',e2'])

expsAdd :: [Expression Pos] -> Add [Tree]
expsAdd = inListAdd expAdd

expAdd :: Expression Pos -> Add Tree
expAdd (Variable _)    = next .>>= ret . flip Tree []
expAdd (Literal l)     = litAdd l 
expAdd (Constructor _) = next .>>= ret . flip Tree []
expAdd (Paren e)         = expAdd e 
expAdd (Typed e _)       = 
  next .>>= \r -> expAdd e .>>= ret . Tree r . (:[])
expAdd (Tuple es)        = 
  next .>>= \ r -> expsAdd es .>>= ret . Tree r
expAdd (List es)         = listExpAdd expAdd es
expAdd (EnumFrom e)      = 
  next .>>= \r -> 
  expAdd e .>>= 
  ret . Tree r . (:[])
expAdd (EnumFromThen e1 e2) = 
  next .>>= \r ->  
  expAdd e1 .>>= \ e1' -> 
  expAdd e2 .>>= \ e2' ->
  ret (Tree r [e1',e2'])
expAdd (EnumFromTo e1 e2) = 
  next .>>= \r ->  
  expAdd e1 .>>= \ e1' -> 
  expAdd e2 .>>= \ e2' ->
  ret (Tree r [e1',e2'])
expAdd (EnumFromThenTo e1 e2 e3) = 
  next .>>= \r ->  
  expAdd e1 .>>= \ e1' -> 
  expAdd e2 .>>= \ e2' ->
  expAdd e3 .>>= \ e3' ->
  ret (Tree r [e1',e2',e3'])
expAdd (UnaryMinus _ e) = 
  next .>>= \r -> 
  expAdd e .>>= 
  ret . Tree r . (:[])
expAdd (Apply e1 e2) = 
  next .>>= \r ->  
  expAdd e1 .>>= \ e1' -> 
  expAdd e2 .>>= \ e2' ->
  ret (Tree r [e1',e2'])
expAdd (InfixApply e1 _ e2) = 
  next .>>= \r ->  
  expAdd e1 .>>= \ e1' -> 
  expAdd e2 .>>= \ e2' ->
  ret (Tree r [e1',e2'])
expAdd (LeftSection e _) = 
  next .>>= \r -> 
  expAdd e .>>= 
  ret . Tree r . (:[])
expAdd (RightSection _ e) = 
  next .>>= \r -> 
  expAdd e .>>= 
  ret . Tree r . (:[])
expAdd (Lambda cs e1) = 
  next .>>= \r -> 
  conTermsAdd cs .>>= \ cs' -> 
  expAdd e1 .>>= \ e' -> 
  ret (Tree r [Tree Nothing cs', Tree Nothing [e']])
expAdd (IfThenElse e1 e2 e3) = 
  next .>>= \r ->  
  expAdd e1 .>>= \ e1' -> 
  expAdd e2 .>>= \ e2' ->
  expAdd e3 .>>= \ e3' ->
  ret (Tree r [e1',e2',e3'])
-- why are listcomp and do different?
-- because they miss a return at the end.
expAdd (ListCompr e1 stmts) = 
  listExpAdd' stmtAdd final stmts
  where
    final = next .>>= \ r ->
            expAdd e1 .>>= \ t ->
            ret (Tree r [t])
expAdd (Do stmts e1) = stmtsAdd stmts e1
  --listExpAdd stmtAdd (stmts ++ [StmtExpr e1])
expAdd (Case e1 alts) =
  next .>>= \r ->
  expAdd e1 .>>= \ e1' -> 
  altsAdd alts .>>= 
  ret . (Tree r) . (e1':)
expAdd (Let ds e1) = 
  next .>>= \ r ->
  declsAdd ds .>>= \ ds' -> 
  expAdd e1 .>>= \ e1' ->
  ret (Tree r (ds'++[e1']))

expAdd (RecordConstr _) = error "add record1"
 -- [Field a (Expression a)]
expAdd (RecordSelection _ _) = error "add record2"
 -- (Expression a) Ident
expAdd (RecordUpdate _ _)= error "add record3"
 --[Field a (Expression a)] (Expression a)


stmtsAdd :: [Statement Pos] -> Expression Pos -> Add Tree
stmtsAdd stmts e = listExpAdd' stmtAdd (expAdd e) stmts

 --stmts ++ [StmtExpr e1])failed --inListAdd stmtAdd

stmtAdd :: Statement Pos -> Add Tree
stmtAdd (StmtExpr e)   = expAdd e 
stmtAdd (StmtDecl ds)  = declsAdd ds .>>= ret . Tree Nothing
stmtAdd (StmtBind p e) = 
  conTermAdd p .>>= \p' -> 
  expAdd e .>>= \ e' ->
  ret (Tree Nothing [p',e'])


altsAdd :: [Alt Pos] -> Add [Tree]
altsAdd = inListAdd altAdd

altAdd :: Alt Pos -> Add Tree
altAdd (Alt _ p rhs) = 
  next .>>= \r ->  
  conTermAdd p .>>= \p'-> 
  rhsAdd rhs .>>= \ r' ->
  ret (Tree r [p',r'])

inListAdd :: (a -> Add b) -> [a] -> Add [b]
inListAdd _ []     = ret []
inListAdd f (x:xs) = f x .>>= \ x' -> 
                     inListAdd f xs .>>= 
                     ret . (x':)



--------------------------------------------
-- making addresses for flatcurry programs
--------------------------------------------


adrMap :: (a -> b) -> AdrTree a -> AdrTree b
adrMap f (Tree x xs) = Tree (f x) (map (adrMap f) xs)
{-
mkExprs :: Int -> StaticInfoExpr
mkExprs i = FC.Comb FC.ConsCall ("DebugInfo","SrcID") 
               [FC.Comb FC.ConsCall ("Prelude","[]") [],
                FC.Lit (FC.Intc i)]

mkAdrs :: [Decl Pos] -> FC.Prog -> [AdrTree Int]
mkAdrs ds (FC.Prog _ _ ts fs _) = 
  map (mkAdrType ds) ts ++ map (mkAdrFun ds) fs

mkAdrType :: [Decl Pos] -> FC.TypeDecl -> AdrTree Int
mkAdrType _ _ = manyZeros


mkAdrFun :: [Decl Pos] -> FC.FuncDecl -> AdrTree Int
mkAdrFun _ _ = manyZeros

manyZeros :: AdrTree Int
manyZeros = Tree 0 (repeat manyZeros)
-}
-----------------------------------------------------
-- phase one: top level declarations with addresses
-----------------------------------------------------

type QN = Either String FC.QName                     

ltQN :: QN -> QN -> Bool
ltQN (Left s) (Left s') = cmpString s s' == LT
ltQN (Left _) (Right _) = True
ltQN (Right _) (Left _) = False
ltQN (Right s) (Right s') = case cmpString (snd s) (snd s') of
              LT -> True
              EQ -> cmpString (fst s) (fst s') == LT
              _  -> False

type Tree' = AdrTree [SrcRef]
type TopMap = FM QN (Decl Pos,Tree)

mkMap :: Module Pos -> TopMap
mkMap m@(Module _ _ ds) = 
  foldr addDecl (emptyFM ltQN) (zip ds trees)
  where
    trees = run (moduleAdd m)

    addDecl x fm = case fst x of 
        DataDecl _ (Ident s _) _ _   -> addToFM fm (Left s) x
        TypeDecl _ (Ident s _) _ _   -> addToFM fm (Left s) x
        FunctionDecl _ (Ident s _) _ -> addToFM fm (Left s) x
        _                            -> fm

lookupTM :: String -> FC.QName -> TopMap -> Maybe (Decl Pos,Tree)
lookupTM m mn fm = 
  lookupFM fm (if m==fst mn then Left (snd mn) else Right mn)

zipFlatCy :: FC.Prog -> Module Pos -> [Tree']
zipFlatCy (FC.Prog p _ ts fs _) m  
  = typeRefs ++ funcRefs
  where
    qnmap = mkMap m 
    typeRefs     = map (zipType qnmap p . FG.typeName) ts
    (zippedFuncs,lifts) = (unzip' $ map (zipFunc qnmap p) fs) defOpts
    funcRefs = zipLiftFuncs lifts fs zippedFuncs 


tot :: Tree -> Tree'
tot = adrMap (maybe [] (:[]))

zipType :: TopMap -> String -> FC.QName -> Tree'
zipType fm m mn = 
  maybe (error $ "data decl for type " ++ show mn ++ " not found.")
        (tot . snd)
        (lookupTM m mn fm)
 
zipFunc :: TopMap -> String -> FC.FuncDecl -> ZR (Maybe Tree')
zipFunc fm m f opts = maybe (Nothing,[])
                       (\ d -> (Just $^ zipBody f d) opts)
                       (lookupTM m mn fm)
  where
    mn = FG.funcName f

zipBody :: FC.FuncDecl -> (Decl Pos,Tree) -> ZR Tree'
zipBody f (FunctionDecl _ _ eqs,Tree (Just i) ts) = 
  Tree [i] $^ caseCascade (FG.funcArgs f) (FG.funcBody f) prts
  where
    prts = zipWith patRhsTree eqs ts

    patRhsTree :: Equation Pos -> Tree -> (([ConstrTerm Pos],Tree),(Rhs Pos,Tree))
    patRhsTree (Equation _ l r) (Tree _ [tl,tr]) = ((cterms l,tl),(r,tr))

    cterms :: Lhs Pos -> [ConstrTerm Pos]
    cterms (FunLhs _ cs)    = cs
    cterms (OpLhs  c1 _ c2) = [c1,c2]
    cterms (ApLhs  l cs)    = cterms l ++ cs

caseCascade :: [FC.VarIndex] -> FC.Expr 
            -> [(([ConstrTerm Pos],Tree),(Rhs Pos,Tree))] 
            -> ZipResult
caseCascade = caseCascade' 0 True

caseCascade' :: Int -> Bool -> [FC.VarIndex] -> FC.Expr 
            -> [(([ConstrTerm Pos],Tree),(Rhs Pos,Tree))] 
            -> ZipResult
caseCascade' ref b vs e prts = case e of
  FC.Case FC.Flex (FC.Var v) bs -> 
    case break (v==) vs of
      (vs1,_:vs2) -> let idx=length vs1 in
                     zipBranches ref idx vs1 vs2 
                        bs
                        (groupBy (grp idx) (sortBy (srt idx bs) prts))
      _ -> error $ "var not found"
  FC.Case FC.Rigid (FC.Var v) bs -> 
    if b then res else caseCascade' ref b vs (FC.Case FC.Flex (FC.Var v) bs) prts
  _ -> res
        
 where
    fidx ct = maybe (error "fidx") id . findIndex (flatConsName ct==)

    srt i bs ((cts1,_),(_,_)) ((cts2,_),(_,_)) = 
      fidx (cts1!!i) bsns <= fidx (cts2!!i) bsns
      where
        bsns = map (snd . FG.patCons . FG.branchPattern) bs

    grp i ((cts1,_),(_,_)) ((cts2,_),(_,_)) = 
      flatConsName (cts1!!i) == flatConsName (cts2!!i)

    res = case prts of
        [((pvs,Tree Nothing _),(r,t))] -> 
          if all isVarPat pvs then zipRhs e r t
                              else error $ "non-variable lhs left: " ++ show pvs
        [((_,t),_)] -> error $ "unexpexted tree in case cascade: "++show t
        (_:_:_) -> error "more than one right hand side found"

flatConsName :: ConstrTerm _ -> String
flatConsName (LiteralPattern l) = case l of
  String []    -> "[]"
  String (_:_) -> ":"
  _ -> error "name of lit"
flatConsName (ConstructorPattern qi _) = identName qi
flatConsName (TuplePattern xs) = '(':replicate (length xs - 1) ','++")"
flatConsName (ListPattern []) = "[]"
flatConsName (ListPattern (_:_)) = ":"
flatConsName (InfixPattern _ qi _) = identName qi
flatConsName (ParenPattern p) = flatConsName p


identName :: QualIdent -> String
identName (UnqualIdent (Ident s _)) = s
identName (QualIdent _ (Ident s _)) = s

opName :: InfixOp -> String
opName iop = identName (opIdent iop)

opIdent :: InfixOp -> QualIdent
opIdent (InfixOp i)     = i
opIdent (InfixConstr i) = i

conArgs :: ConstrTerm p -> [ConstrTerm p]
conArgs (ConstructorPattern _ cs) = cs
conArgs (TuplePattern cs) = cs
conArgs (VariablePattern _) = error "unexpected pattern variable"
conArgs (ParenPattern p) = conArgs p
conArgs (LiteralPattern l) = case l of
  String [] -> []
  String (c:cs) -> [LiteralPattern (Char c),LiteralPattern (String cs)]
  _ -> []
conArgs (NegativePattern _ _) = []
conArgs (ListPattern []) = []
conArgs (ListPattern (x:xs)) = [x,ListPattern xs]
conArgs (InfixPattern x _ y) = [x,y]
conArgs (AsPattern _ c) = conArgs c

isVarPat :: ConstrTerm _ -> Bool
isVarPat x = case x of VariablePattern _ -> True; _ -> False

zipBranches :: Int -> Int -> [FC.VarIndex] -> [FC.VarIndex] -> [FC.BranchExpr] 
            -> [[(([ConstrTerm Pos],Tree),(Rhs Pos,Tree))]] 
            -> ZipResult
zipBranches ref idx vs1 vs2 bs prtss = unzip' $ zipWith' zipper bs prtss
  where
    zipWith' _ []       []     = []
    zipWith' f (x:xs)   (y:ys) = f x y:zipWith' f xs ys
    zipWith' _ xs@(_:_) []     = map allRef xs

    zipper :: FC.BranchExpr -> [(([ConstrTerm Pos],Tree),(Rhs Pos,Tree))] 
           -> ZR Tree'
    zipper (FC.Branch p e) prts = Tree is $^ ts
      where
        modPrt ((cts,Tree _ trees),(r,rt)) = case splitAt idx cts of
          (cts1,ct:cts2) -> case splitAt idx trees of
            (ts1,Tree (Just i) ts':ts2) -> 
               (i,((cts1++conArgs ct++cts2,Tree Nothing (ts1++ts'++ts2)),(r,rt)))
            _ -> error "wrong tree in zipBranches"
          _ -> error "wrong index in zipBranches"
        (is,prts')  = unzip (map modPrt prts)
        ts = caseCascade (vs1++vs++vs2) e prts'
        vs = case p of FC.Pattern _ args -> args; FC.LPattern _ -> []

    allRef :: FC.BranchExpr -> ZR Tree' 
    allRef (FC.Branch _ e) = zir (t [FG.trExpr v l c le f o ca br e])
     where
       t   = Tree [ref]
       v _ = t []
       l = error "allRef: literal"
       c = error "allRef: comb"
       le = error "allRef: let"
       f = error "allRef: free"
       o = error "allRef: or"
       ca _ _ ts = t ts
       br _ tr = tr
    
       



zipRhs :: FC.Expr -> Rhs Pos -> Tree -> ZipResult
zipRhs (FC.Let vbs e) (SimpleRhs _ e' locs@(_:_)) (Tree (Just i) (t:ts))
  = (:[]) $^ Tree [i] $^ zipBinds vbs e locs e' (ts++[t])
zipRhs e   (SimpleRhs _ e' []) (Tree _ ts)  = 
  case ts of
   [t] -> zipExp e e' t
   _ -> error $ "unexpected tree for right hand side: " ++ show ts
zipRhs exp (GuardedRhs grs []) (Tree _ tgrs) 
  = zipGuards guardErr grs exp tgrs
zipRhs (FC.Let vbs e) (GuardedRhs grs locs@(_:_)) (Tree (Just i) ts)
  = (:[]) $^ Tree [i] $^
    zipBinds' (const $ zipGuards guardErr grs) vbs e locs (tlocs++tgrs)
  where
    (tgrs,tlocs) = splitAt (length grs) ts

-- = (:[]) $^ Tree [i] $^ zipBinds vbs e locs e' (ts++[t])

guardErr :: Int
guardErr = (error "empty guard sequence")
  

zipGuards :: Int -> [CondExpr Pos] -> FC.Expr -> [Tree] -> ZipResult
zipGuards i [] e []
     | e==FC.Comb FC.FuncCall (prelude,"failed") [] = zir [Tree [i] []]
     | otherwise = error "guard mismatch"
zipGuards _ (CondExpr _ g' r':xs) e (Tree (Just i) [tg,tr]:txs) =
      case e of 
        FC.Comb FC.FuncCall (m,n) [g,r] -> 
         if   m==prelude && n=="cond" && null xs
         then (:[]) $^ Tree [i] $^ zipExp g g' tg +++ zipExp r r' tr
         else error "wrong construction of predicate guard"
        FC.Case _ g [FC.Branch (FC.Pattern pt []) r,
                     FC.Branch (FC.Pattern pf []) e'] -> 
          if   pt==(prelude,"True") && pf==(prelude,"False")
          then (:[]) $^ Tree [i]
                     $^ (zipExp g g' tg +++)
                     $  (Tree [i] []:)
                     $^ (zipExp r r' tr +++)
                     $  (Tree [i] []:)
                     $^ zipGuards i xs e' txs
          else error "wrong construction of Boolean guard"
        _ -> error "unexpected guarded right hand side"
      


type ZipResult = ZR [Tree']
type ZR a = Options -> (a,Collected)
type Collected = [(String,(Lifted,Tree))]

data Lifted = LiftedExp (Expression Pos)
            | LiftedComplexPat (ConstrTerm Pos) Int
            | LiftedBind (ConstrTerm Pos) (Expression Pos)

type Options = [(Ident,(Expression Pos,Tree))]

defOpts :: Options
defOpts = []

infixr 5 +++
infixr 0 $^

($^) :: (a -> b) -> ZR a -> ZR b
f $^ g = \ opts -> case g opts of
                     (ts,x) -> (f ts,x)

zir :: a -> ZR a
zir x _ = (x,[])

concats :: [ZR [a]] -> ZR [a]
concats xs = \ opts -> case unzip (map ($opts) xs) of 
  (as,bs) -> (concat as,concat bs)

(+++) :: ZR [a] -> ZR [a] -> ZR [a]
(f +++ g) opts = let (xs,xs') = f opts 
                     (ys,ys') = g opts 
                  in (xs++ys,xs'++ys')

unzip' :: [ZR a] -> ZR [a]
unzip' []      _    = ([],[])
unzip' (f:xys) opts = let (x,ys) = f opts in case unzip' xys opts of (as,bs) -> (x:as,ys++bs)


zipExp :: FC.Expr -> Expression Pos -> Tree -> ZipResult
zipExp (FC.Var _) (Variable _) t opts = zir [tot t] opts
zipExp (FC.Lit _) (Literal _)  t opts = zir [tot t] opts
zipExp e@(FC.Lit _) (Let ds exp) (Tree _ ts) opts 
  = addBinds (zipExp e exp) ds ts opts
zipExp e@(FC.Lit _) (Variable (UnqualIdent v)) (Tree _ []) opts 
  = case lookup v opts of
      Just (exp,t) -> zipExp e exp t opts
zipExp e'@(FC.Comb _ (m,n) args) e t opts
  | isLet e = case e of
     Let ds exp -> case t of
       Tree _ trees -> addBinds (zipExp e' exp) ds trees opts

  | isStatement e = case decompStatement e of
      (sts,end) -> zipSts e' sts end t opts

  | isTupleName n && not (isApply e) = case e of 
      Tuple args' -> case t of
        Tree (Just i) trees -> 
          ((:[]) $^ Tree [i] $^ concats (zipWith3 zipExp args args' trees)) opts
      _ -> error $ "tuple expected instead of " ++ show e

  | n=="[]" = case e of List [] -> case t of
                          Tree (Just i) [] -> zir [Tree [i] []] opts
                        Literal (String "") -> case t of
                          Tree (Just i) [] -> zir [Tree [i] []] opts

  | n==":" && not (isApply e) = case e of 
     List (x:xs) -> case t of
       Tree (Just i) [tx,txs] -> case args of
         [ex,exs] ->    ((:[]) 
                     $^ Tree [i] 
                     $^ (zipExp ex x tx +++
                         zipExp exs (List xs) txs)) opts
     Literal (String (_:xs)) -> case t of
       Tree (Just i) [tx,txs] -> case args of
         [_,exs] -> ((:[]) $^ Tree [i] $^ (tot tx:) 
                           $^ zipExp exs (Literal (String xs)) txs) opts
     InfixApply _ _ _ -> case te of 
       Tree (Just i) [] -> ((:[]) $^ Tree [i] $^ const targs) opts
     _ -> error $ "unexpected expression for (:): "++show e
  
  | m==prelude && n=="apply" 
  = case e of 
     Apply e1 e2 -> case t of
      Tree (Just i) [te1,te2] -> case args of
         [e1',e2'] -> ((:[]) $^ Tree [i] $^ zipExp e1' e1 te1 
                                        +++ zipExp e2' e2 te2) opts 
         
  | isVariable e = case e of
      Variable (UnqualIdent v) -> case lookup v opts of
        Just (exp,tree) -> zipExp e' exp tree opts
        _ -> varFunCall 
      _ -> varFunCall

  | otherwise = case te of 
     Tree (Just i) [] -> ((:[]) $^ Tree [i] $^ const targs) opts
     Tree (Just i) _ -> case fun of
       Lambda _ _ -> 
        ((:[]) $^ Tree [i] $^ 
          const (targs',(n,(LiftedExp fun,te)):funs)) opts
       _ -> error $ "unexpected source in zipExp: "++show (fun,e',e,t,te)
     _ -> error $ "unexpected tree in zipExp: " ++ show (e',e,t,te)
  where
    (res,rts) = decompApp e t 
    fun:es = reverse res
    te:ts = reverse rts
    targs@(targs',funs) = concats (zipWith3 zipExp args es ts) opts
    varFunCall = case te of Tree (Just i) [] -> 
                              ((:[]) $^ Tree [i] $^ const targs) opts

zipExp (FC.Let vbs e') e t opts = case e of
  Let ds body -> case t of
    Tree (Just i) ts -> ((:[]) $^ Tree [i] $^ zipBinds vbs e' ds body ts) opts

zipExp e@(FC.Case FC.Rigid _ _) e' t opts 
  = case decompStatement e' of
      (sts,end) -> zipSts e sts end t opts
      {-case e' of
      ListCompr res gs -> case t of
        Tree (Just r) [tres,tgs] -> ((:[]) $^ zir (Tree [r] [])) opts
      _ -> error $ "rigid case: " ++ show e'-}

zipBinds' :: ([(FC.VarIndex,FC.Expr)] -> FC.Expr -> [Tree] -> ZipResult) 
         -> [(FC.VarIndex,FC.Expr)] -> FC.Expr
         -> [Decl Pos] -> [Tree] -> ZipResult
zipBinds' cont vbs e []    ts = cont vbs e ts
zipBinds' _    []  _ (_:_) _  = error "bind mismatch"
zipBinds' cont bvbs@((_,b):vbs) e 
         (PatternDecl _ p (SimpleRhs _ b' []):ds) 
         (Tree _ [tp,tr]:ts)
  | isVarPat p || null (conArgs p)
  = zipBindBody b b' tr vbs
  | otherwise 
  = case span isSelCall bvbs of
      (sels,(_,e2):vbs') -> makeSels sels p tp +++
                            zipBindBody e2 b' tr vbs'
      res -> error $ "wrong span: " ++ show res

  where
    isSelCall (_,exp) = FG.isComb exp && isSelName (snd $ FG.combName exp)
    zipBindBody e1 e2 t0 xs = case t0 of
      Tree _ [t] -> 
        zipExp e1 e2 t +++ zipBinds' cont xs e ds ts
      _ -> error $ "unexpected tree in zipBind: " ++ show t0

makeSels :: [(FC.VarIndex,FC.Expr)] -> ConstrTerm Pos -> Tree 
         -> ZipResult
makeSels vbs pattern tp@(Tree _ argTrees) _ = 
  (map (\i -> Tree [i] []) varRefs,
   map (\ (t,n) -> (n,(LiftedComplexPat pattern t,tp)))
       (zip varRefs varNames))
 where
   varRefs = match (map varNameFromSel varNames) (conArgs pattern) argTrees

   varNames = map (snd . FG.combName . snd) vbs

   match :: [String] -> [ConstrTerm Pos] -> [Tree] -> [Int]
   match [] _ _ = []
   match vvs@(v:vs) (p:ps) (Tree (Just i) tps:ts)
     = case p of 
         VariablePattern (Ident pv _) -> if pv==v then i:match vs ps ts  
                                                  else match vvs ps ts
         _ -> match vvs (conArgs p ++ ps) (tps ++ ts)
   match (v:_) [] (_:_) = error $ "no patterns left to match " ++ v
   match (v:_) (_:_) [] = error $ "no trees left to match " ++ v
   match (v:_) []    [] = error $ "nothing left to match " ++ v



zipBinds :: [(FC.VarIndex,FC.Expr)] -> FC.Expr
         -> [Decl Pos] -> Expression Pos -> [Tree] -> ZipResult
zipBinds vbs e ds e' = zipBinds' moreLets vbs e ds
  where
    moreLets []         exp [t] = zipExp exp e' t
    moreLets vbs'@(_:_) exp [Tree _ ts] = case e' of
      Let ds' e'' -> zipBinds vbs' exp ds' e'' ts
      _ -> error $ "unexpected expression when looking for let: " ++  show e'

addBinds :: (Tree -> ZipResult) -> [Decl Pos] -> [Tree] -> ZipResult
addBinds cont [] [t] = cont t
addBinds cont (PatternDecl _ (VariablePattern v) (SimpleRhs _ b []):ds) 
              (Tree _        [_,                 Tree _      [t]]  :ts)
  = \opts -> addBinds cont ds ts ((v,(b,t)):opts)

isTupleName :: String -> Bool
isTupleName s = case s of
  '(':cs -> dropWhile (','==) cs == ")"
  _ -> False


isApply :: Expression _ -> Bool
isApply e = case e of
  Apply _ _        -> True
  _             -> False

isLet :: Expression _ -> Bool
isLet e = case e of
  Let _ _       -> True
  _             -> False

isVariable :: Expression _ -> Bool
isVariable e = case e of
  Variable _    -> True
  _             -> False

isStatement :: Expression _ -> Bool
isStatement e = case e of
  Do _ _        -> True
  ListCompr _ _ -> True
  _             -> False


decompStatement :: Expression p -> ([Statement p],Expression p)
decompStatement (Do xs x)        = (xs,x)
decompStatement (ListCompr x xs) = (xs,x)

decompApp :: Expression p -> Tree -> ([Expression p],[Tree])
decompApp e t = case e of 
  Paren e' -> decompApp e' t
  Apply e1 e2 -> case t of
   Tree _ [te1,te2] -> let (es,ts) =  decompApp e1 te1
                        in (e2:es,te2:ts) 
   _ -> error $ "wrong tree for apply: " ++ show t
  InfixApply e1 qi e2 -> case t of
   Tree i [te1,te2] -> ([e2,e1,Variable (opIdent qi)],[te2,te1,Tree i []])
  _           -> ([e],[t])


zipSts :: FC.Expr -> [Statement Pos] -> Expression Pos -> Tree 
       -> ZipResult
zipSts e  [] e' t = case e of
  (FC.Comb FC.ConsCall (_,":") [e'',_]) -> 
    case t of
     Tree (Just r) [t'] -> (\ [te'] -> [Tree [r] [te',Tree [r] []]]) $^ 
                           zipExp e'' e' t'
  _ ->  zipExp e e' t
zipSts e (StmtExpr g':gs) e' (Tree (Just r) [tg,te]) 
  = zipMBind e g' r tg (zipStsRest gs e' (error "do not touch") te)
zipSts e (StmtBind p g':gs) e' (Tree (Just r) [(Tree _ [tp,tg]),te])
  = zipMBind e g' r tg (zipStsRest gs e' (p,r) (Tree (Just r) [tp,te]))

zipMBind (FC.Comb _ (m,n) [g,e]) g' r tg f opts 
  | m==prelude && elem n [">>",">>="]
  = ((:[]) $^ Tree [r] $^ zipExp g g' tg +++ f e True) opts
  | n=="++" 
  = case zipMBind g g' r tg f opts of
      (t,ls) -> ([Tree [r] (t++[Tree [r] []])],ls)


{-trace (show (g,e,g',r,tg)) $ case f g False opts of
      ([te'],ls) -> ([Tree [r] [te',Tree [r] []]],ls)-}
zipMBind (FC.Comb FC.FuncCall (m,n) [e,_,g]) g' r tg f opts 
  | m==prelude && n=="foldr" 
  = case zipExp g g' tg opts of 
      ([tg'],ls) -> case f e False opts of
         ([te'],ls') -> 
           ([Tree [r] [te',Tree [r] [],tg']],ls++ls')
zipMBind (FC.Case FC.Rigid g [FC.Branch _ e,_]) g' r tg f opts
  = case zipExp g g' tg opts of 
      ([tg'],ls) -> case f e False opts of
         ([te'],ls') -> 
           ([Tree [r] [tg',Tree [r] [],te',Tree [r] [],Tree [r] []]],ls++ls')

zipStsRest :: [Statement Pos] -> Expression Pos -> (ConstrTerm Pos,Int)
           -> Tree -> FC.Expr -> Bool -> ZipResult
zipStsRest gs e' (p,r) te exp@(FC.Comb _ (_,n) _) b
  | isLambdaName n 
  = const ([Tree [r] []], [(n,(LiftedBind p e,te))])
  | otherwise 
  = zipSts exp gs e' te
  where
    e = (if b then flip Do else ListCompr) e' gs 
zipStsRest gs e' _ te e@(FC.Case _ _ _) _
  = zipSts e gs e' te



{-
case genExp exp of
     Left (g,e) -> case e of
      FC.Comb (FC.FuncPartCall 1) (_,n) [] -> 
        if isLambdaName n  -- condition may be deleted later
        then 
          
                               --zipSts e gs e' {-(Lambda (error "nopos") p e')-} te
        else error $ "lambda name expected: " ++ n
      _ -> error $ "empty lambda expected: " ++ show e
  
-}
{-
--Tree _ [tp,tg]
genExp :: FC.Expr -> Either (FC.Expr,FC.Expr)(FC.Expr,FC.Expr)
genExp  = Left (g,e)
genExp (FC.Comb FC.FuncCall (_,">>=") [g,e]) = Left (g,e)
genExp  = Right (e',e) 
-}


zipLiftFuncs :: Collected -> [FC.FuncDecl] -> [Maybe Tree'] -> [Tree']
zipLiftFuncs _ [] [] = []
zipLiftFuncs lifts (_:fs) (Just t:zippedFuncs) 
  = t:zipLiftFuncs lifts fs zippedFuncs
zipLiftFuncs lifts (f:fs) (Nothing:zippedFuncs) 
  = case lookup (snd $ FG.funcName f) lifts of
      Just (LiftedExp e,t) -> case zipLiftFunc f e t defOpts of
        (t',ls) -> t':zipLiftFuncs (ls++lifts) fs zippedFuncs
      Just (LiftedComplexPat p i,t) -> fst (zipSelector f p i t defOpts):
        zipLiftFuncs lifts fs zippedFuncs
      Just (LiftedBind p e,t) -> 
        case zipLiftedBind f p e t defOpts of
          (t',ls) -> t':zipLiftFuncs (ls++lifts) fs zippedFuncs
      Nothing -> error $ "no source for function " ++ (snd $ FG.funcName f)


zipLiftFunc :: FC.FuncDecl -> Expression Pos -> Tree -> ZR Tree'
zipLiftFunc f (Lambda ps e) (Tree (Just i) [l,r]) =  
  Tree [i] $^ caseCascade (FG.funcArgs f) (FG.funcBody f) 
                [((ps,l),
                  (SimpleRhs (error "look at position") e [],r))]
  --zipExp (FG.funcBody f) e t

zipSelector :: FC.FuncDecl -> ConstrTerm Pos -> Int -> Tree -> ZR Tree'
zipSelector f pat i tpat --(Tree (Just tp) ts)
  = Tree [i] $^ caseCascade (FG.funcArgs f) (FG.funcBody f) 
                  [(([pat],Tree Nothing [tpat]),
                   (SimpleRhs (error "look at position") e [],
                    Tree Nothing [Tree (Just i) []]))]

  -- = zir (Tree [i] [Tree [tp] [Tree [tv] []]])
  where
    e = Variable (UnqualIdent (Ident (varNameFromSel $ snd $ FG.funcName f) 0))
    --Tree (Just tv) _ = error $ "zipSel" ++ show (ts,i) --ts!!i


zipLiftedBind :: FC.FuncDecl -> ConstrTerm Pos -> Expression Pos 
              -> Tree -> ZR Tree'
zipLiftedBind f p e (Tree (Just r) [tp,tg]) 
  = Tree [r] $^ caseCascade' r False (FG.funcArgs f) (FG.funcBody f) 
                [(([p],Tree Nothing [tp]),
                  (SimpleRhs (error "look at position") e [],Tree Nothing [tg]))]

isLambdaName :: String -> Bool
isLambdaName s = case break (=='.') s of
   (_,_:s') -> take 8 s' == "_#lambda" || isLambdaName s'
   _        -> False

fpSelExt :: String
fpSelExt = "_#selFP"

isSelName :: String -> Bool
isSelName s = case break (=='.') s of
   (_,_:s') -> isPrefixOf fpSelExt s' || isSelName s'
   _        -> False

varNameFromSel :: String -> String
varNameFromSel s = case break (=='.') s of
   (_,_:s') -> if isPrefixOf fpSelExt s' 
               then tail $ dropWhile (/='#') $ drop (length fpSelExt) s'
               else varNameFromSel s'
   _        -> error "no var name in selector"

prelude :: String
prelude = "Prelude"


---------------------------------------------------
-- getting the parameter from SrcEntity for tests
---------------------------------------------------
{-
param :: SrcEntity a -> Maybe a
param (SrcDecl d) = paramDecl d
param (SrcCon  _ c) = paramConDecl c
param (SrcConTerm _ _ t) = paramConTerm t
param (SrcNew _ n) = paramNewDecl n
param (SrcId _ _) = Nothing
param (SrcExpr _ _ e) = paramExp e
param (SrcAlt _ _ a) = paramAlt a

paramDecl :: Decl a -> Maybe a
paramDecl (InfixDecl x _ _ _)      = Just x
paramDecl (DataDecl x _ _ _)       = Just x
paramDecl (NewtypeDecl x _ _ _)    = Just x
paramDecl (TypeDecl x _ _ _)       = Just x
paramDecl (FunctionDecl x _ _ )    = Just x
paramDecl (ExternalDecl x _ _ _ _) = Just x
paramDecl (PatternDecl x _ _)      = Just x
paramDecl (ExtraVariables x _)     = Just x

paramConDecl :: ConstrDecl a -> Maybe a
paramConDecl (ConstrDecl x _ _ _)  = Just x
paramConDecl (ConOpDecl x _ _ _ _) = Just x

paramConTerm :: ConstrTerm a -> Maybe a
paramConTerm _ = Nothing

paramNewDecl :: NewConstrDecl a -> Maybe a
paramNewDecl (NewConstrDecl x _ _ _) = Just x

paramEq :: Equation a -> Maybe a
paramEq (Equation x _ _) = Just x

paramExp :: Expression a -> Maybe a
paramExp (Variable _) = Nothing
paramExp (Literal _)  = Nothing
paramExp (Constructor _) = Nothing
paramExp (Typed _ _) = Nothing
paramExp (Tuple _) = Nothing

paramAlt :: Alt a -> Maybe a
paramAlt (Alt x _ _) = Just x
-}
-----------------------------------------------------
-- testing
-----------------------------------------------------

mainTest :: (FC.Prog -> Module Pos -> a) -> String -> IO ()
mainTest f fn = do
    p <- FC.readFlatCurry fn
    m <- readCurry fn
    print (f p m)
{-
testWithRef :: IO ()
testWithRef = do
    [fn] <- getArgs
    m <- readCurry fn
    let test (d,r) = srcAt m r==Just (SrcDecl d)
    print $ and $ map test (zipWithRef m)
-}    

testAt :: Int -> String -> IO ()
testAt n = mainTest (\ _ m -> maybe "Nothing" mainInfo $ srcAt m n)

testZip :: String -> IO ()
testZip = mainTest zipFlatCy

{-
testWithRef2 :: String -> IO ()
testWithRef2 = mainTest (\ _ -> take 10 . zipWithRef)
-}

maxRef :: Tree -> SrcRef 
maxRef = last . refs
  where
    refs (Tree (Just i) ts) = i:concatMap refs ts
    refs (Tree Nothing  ts) = concatMap refs ts
     
    last []  = error "empty module"
    last [x] = x
    last (_:xs@(_:_)) = last xs



testAdd :: String -> IO ()
testAdd = mainTest t
  where
    t _ m = r `seq` maybe False
                  (\_-> maybe True bad (srcAt m (r+1))) 
                  (srcAt m r)
      where r = maxRef (Tree Nothing $ run (moduleAdd m))
    bad _ = False

testAddTree :: String -> IO ()
testAddTree = mainTest (\_ m -> run (moduleAdd m))

testMaxRef :: String -> IO ()
testMaxRef = mainTest (\_ m -> maxRef $ Tree Nothing $ run (moduleAdd m))

testSrcRef :: FC.Prog -> Module Pos -> Bool
testSrcRef p@(FC.Prog _ _ ts fs _) m = 
  case splitAt (length ts) trees of
    (tts,fts) -> and (zipWith' testType ts tts++zipWith' testFunc fs fts)
  where
    trees = zipFlatCy p m

    testType t (Tree [i] xs) =  case srcAt m i of
      Just (SrcDecl d) -> case d of 
        DataDecl _ (Ident s _) _ _ -> 
          snd (FG.typeName t)===s  && 
          and (zipWith' testCons (FG.typeConsDecls t) xs)
        TypeDecl _ (Ident s _) _ _ -> snd (FG.typeName t)===s  
      _ -> error "test type"

    testCons c (Tree [i] []) = case srcAt m i of
      Just (SrcCon _ c') -> consDeclName c' === snd (FG.consName c)

    testFunc f (Tree [i] xs) = case srcAt m i of
      Just (SrcDecl (FunctionDecl _ (Ident s _) _)) -> 
        s===snd (FG.funcName f) && testExp (FG.funcBody f) xs
      Just (SrcExpr _ _ (Lambda _ _)) -> 
        is "no Lambda name" isLambdaName (snd $ FG.funcName f) && 
        testExp (FG.funcBody f) xs
      Just (SrcConTerm (PatternDecl _ _ _) Nothing (VariablePattern (Ident s _))) -> 
        is "no selector name" isSelName (snd $ FG.funcName f) &&
        s===varNameFromSel (snd $ FG.funcName f) &&
        testExp (FG.funcBody f) xs
      Just (SrcExpr _ _ (Do _ _)) ->
        is "no Lambda name" isLambdaName (snd $ FG.funcName f) && 
        testExp (FG.funcBody f) xs
      Just (SrcExpr _ _ (ListCompr _ _)) ->
        is "no Lambda name" isLambdaName (snd $ FG.funcName f) && 
        testExp (FG.funcBody f) xs
      se -> error $ "wrong source for func decl: " ++ show' se
    --testFunc f (Tree [] xs)  = error $ show f
  
    testExp (FC.Var _) xs = case xs of
        [Tree [i] []] -> case srcAt m i of
         Just (SrcExpr _ _ (Variable _)) -> True
         Just (SrcConTerm _ _ (VariablePattern _)) -> True
         Just (SrcExpr _ _ (ListCompr _ _)) -> True
         se -> error $ "wrong source for var: " ++ show' se
        _ -> error $ "wrong tree for var: " ++ show xs
    testExp (FC.Case FC.Flex e bs) xs = case e of
      FC.Var _ -> and (zipWith' testPats bs xs)
      _ -> error "unexpected flexible case"
    testExp (FC.Case FC.Rigid e bs) xs = case xs of
     (Tree [i] (te:tbs):_) ->
      case srcAt m i of
        Just (SrcCond  _ _ _ _) -> case bs of
          [FC.Branch pt r,FC.Branch pf grs] -> case tbs of 
            Tree [i'] []:tr:Tree [i''] []:tgrs -> 
              snd (FG.patCons pt)==="True"  &&
              snd (FG.patCons pf)==="False" && 
              i===i' && i===i'' && 
              testExp e [te] && testExp r [tr] && testExp grs tgrs
            _ -> error "0"
          _ -> error "1"
        Just (SrcExpr _ _ (ListCompr _ _)) -> case bs of
          [FC.Branch pt r,FC.Branch pf nil] -> 
            case tbs of 
             [Tree [i'] [],tr,Tree [i''] [],Tree [i'''] []] -> 
              snd (FG.patCons pt)==="True"  &&
              snd (FG.patCons pf)==="False" && 
              snd (FG.combName nil)==="[]" && 
              i===i' && i===i'' && i===i''' &&
              testExp e [te] && testExp r [tr] 
             _ -> error "2"
          _ -> error "3"
        Just (SrcConTerm _ _ (ConstructorPattern _ _)) ->
          case e of
            FC.Var _ -> and (zipWith' testPats bs xs)
            _ -> error "unexpected rigid case"
        se -> error $ "unexpected source for rigid case: " ++ show' se
     _ -> error $ "wrong trees for rigid case: " ++ show xs

      {-FC.Var _ -> if   length xs === length bs
                  then and (zipWith' testPats bs xs)
                  else case xs of 
            (te:tbs) -> testExp e [te] && and (zipWith' testPats bs tbs)

      _ -> case xs of 
            (te:tbs) -> testExp e [te] && and (zipWith' testPats bs tbs)-}

    testExp (FC.Comb FC.FuncCall (_,n) args) xs = case xs of
      [Tree [i] tas] ->
        case srcAt m i of
          Just (SrcExpr _ _ (Variable qi)) -> 
            identName qi===n && 
            and (zipWith' testExp args (map (:[]) tas))
          Just (SrcExpr _ _ (InfixApply _ qi _)) -> 
            opName qi===n && 
            and (zipWith' testExp args (map (:[]) tas))
          Just (SrcExpr _ _ (Do (StmtBind _ _:_) _)) -> 
            is "no bind func nor lambda" 
               (\n' -> elem n' [">>",">>="] || isLambdaName n') n && 
            and (zipWith' testExp args (map (:[]) tas))
          Just (SrcExpr _ _ (Do _ _)) -> 
            is "no bind func" (flip elem [">>",">>="]) n && 
            and (zipWith' testExp args (map (:[]) tas))
          Just (SrcExpr _ _ (ListCompr _ (StmtBind _ _:_))) -> 
            case args of 
             [e,nil,g] -> case tas of
               [Tree [i'] [],Tree [i''] [],tg] ->
                 i===i' && i===i'' &&
                 n==="foldr" &&
                 (snd (FG.combName nil) === "[]") &&
                 is "no lambda name" isLambdaName (snd (FG.combName e)) && 
                 testExp g [tg]
               _ -> error $ "wrong tree: " ++ show (tas,args)
             [fold,g] -> case tas of
               [tg, Tree [i'] []] ->
                 i===i' &&
                 n==="++" &&
                 testExp fold [Tree [i'] []] &&
                 testExp g [tg]
               _ -> error $ "mist: " ++ show tas
             _ -> error $ "wrong args for desugared statement bind: " ++ show args
          Just (SrcExpr _ _ (Lambda _ _)) -> 
            is "no lambda name" isLambdaName n && 
            and (zipWith' testExp args (map (:[]) tas))

          Just (SrcCond _ _ _ _) -> 
            (n=="cond" && and (zipWith' testExp args (map (:[]) tas))) ||
            (n=="failed" && 
             is "has arguments" null args && 
             is "tree not empty" null tas) || 
            (error $ "guard mismatch: "++n)
          Just (SrcConTerm (PatternDecl _ _ _) Nothing _) 
            | null tas -> True
          Just (SrcExpr _ _ (Apply _ _)) ->
            n==="apply" && and (zipWith' testExp args (map (:[]) tas))
          Just src -> error $ 
            "wrong source for call of func "++n++": "++show src ++ " at " ++ show i
          Nothing -> error $ "Nothing for func " ++ n ++ " at " ++ show i
      _ -> error $ "wrong tree for fun comb: " ++ n
    testExp (FC.Comb FC.ConsCall (_,n) args) xs 
     | isTupleName n = case xs of
         [Tree [i] tas] -> case srcAt m i of
            Just (SrcExpr _ _ e) -> case e of
              Tuple _ -> and (zipWith' testExp args (map (:[]) tas))
              Constructor qi -> identName qi===n && 
                and (zipWith' testExp args (map (:[]) tas))
              _ -> error "6"
            _ -> error "test tuple"
         _ -> error $ "wrong tree for tuple"
     | otherwise = 
       case xs of
      [Tree [i] tas] ->
        case srcAt m i of
          Just (SrcExpr _ _ (Constructor qi)) -> 
            identName qi===n && 
            and (zipWith' testExp args (map (:[]) tas))
          Just (SrcExpr _ _ (List l)) -> case l of
            [] -> n==="[]"
            _  -> n===":" && and (zipWith' testExp args (map (:[]) tas))
          Just (SrcExpr _ _ (Literal (String l))) -> case l of
            [] -> n==="[]"
            _  -> n===":" && case args of
               [FC.Lit (FC.Charc _),exs] -> case tas of
                 [_,txs] -> testExp exs [txs]
                 _ -> error $ "wrong tree for string: " ++ show tas
               _ -> error "wrong string expr for source"
          Just (SrcExpr _ _ (InfixApply _ iop _)) -> 
            opName iop===n && 
            and (zipWith' testExp args (map (:[]) tas))
          Just (SrcExpr _ _ (ListCompr _ [])) -> case tas of
            [te,Tree [i'] []] ->
              n===":" && 
              i===i' &&
              testExp (head args) [te]
            _ -> error $ "wrong trees for list compr: " ++ show tas
          se -> error $ "wrong source for call of constructor "++n
                      ++": "++show' se ++ " at " ++ show i
      _ -> error $ "wrong trees for call of constructor "++n
                      ++": "++ show xs

    testExp (FC.Comb (FC.FuncPartCall _) mn args) xs 
      = testExp (FC.Comb FC.FuncCall mn args) xs 

    testExp (FC.Comb (FC.ConsPartCall _) mn args) xs 
      = testExp (FC.Comb FC.ConsCall mn args) xs 

    testExp (FC.Lit l) xs = case xs of
      [Tree [i] []] -> case srcAt m i of
        Just (SrcExpr _ _ (Literal l')) -> testLit 1 l l'
        _ -> error $ "wrong source for literal" 
      _ -> error $ "wrong tree for literal: " ++ show xs

    testExp (FC.Let vbs e) xs = case xs of
      [Tree [i] xs'] -> case srcAt m i of
        Just (SrcExpr _ _ (Let _ _)) -> testBinds vbs e xs'
        Just (SrcRhs  _ _ (SimpleRhs _ _ (_:_))) -> testBinds vbs e xs'
        Just (SrcRhs  _ _ (GuardedRhs _ (_:_)))  -> testBinds vbs e xs'
        _ -> error "17"
      _ -> error $ "unexpected tree for test let: " ++ show xs

    

    testBinds [] e t = testExp e t
    testBinds ((_,b):vbs) e (t:xs) = testExp b [t] && testBinds vbs e xs
 
    testPats b (Tree rs xs) = 
      and (testExp (FG.branchExpr b) xs:map (testPat b) rs)

    testPat b i = case srcAt m i of
      Just (SrcConTerm _ _ ct) -> case FG.branchPattern b of
        FC.Pattern (_,n) _ -> n===flatConsName ct
        FC.LPattern l -> case ct of
                        LiteralPattern l'    -> testLit 1    l l'
                        NegativePattern _ l' -> testLit (-1) l l'
      Just (SrcExpr _ _ (ListCompr _ _)) -> True
      se -> error $ "wrong source for pattern: " ++ show' se 

    testLit _ (FC.Charc x)  (Char x')  = x===x'
    testLit i (FC.Intc x)   (Int _ x') = x===(i*x')
    testLit i (FC.Floatc x) (Float x') = if i<0 then x===((0.0-.1.0) *. x') else x===x'

    zipWith' _ [] [] = []
    zipWith' _ xs@(_:_) [] = error $ "zipWith' left list not empty: " ++ show xs
    zipWith' _ [] (_:_) = error "zipWith' right"
    zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

    x===y = if x==y then True 
            else error $ "mismatch: " ++show x ++ " | " ++ show y

    is msg pred s = if pred s then True
                    else error $ msg ++ " |  " ++ show s

    

show' :: Maybe (SrcEntity Pos) -> String
show' = maybe "Nothing" mainInfo --show 


  
consDeclName :: ConstrDecl _ -> String
consDeclName (ConstrDecl _ _ (Ident s _) _)  = s
consDeclName (ConOpDecl _ _ _ (Ident s _) _) = s

bigTest :: String -> IO ()
bigTest = mainTest testSrcRef

mainInfo :: SrcEntity _ -> String
mainInfo (SrcDecl x) = show x
mainInfo (SrcCon _ x) = show x
mainInfo (SrcConTerm _ _ x) = show x
mainInfo (SrcNew _ x) = show x
mainInfo (SrcRhs _ _ x) = show x
mainInfo (SrcId _ x) = show x
mainInfo (SrcCond _ _ x y) = "Cond"++show (x,y)
mainInfo (SrcExpr _ _ x) = show x
mainInfo (SrcAlt _ _ x) = show x

init :: [a] -> [a]
init [_] = []
init (x:xs@(_:_)) = x:init xs


