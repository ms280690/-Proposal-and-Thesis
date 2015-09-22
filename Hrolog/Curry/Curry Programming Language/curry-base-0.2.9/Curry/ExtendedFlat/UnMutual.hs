{-# LANGUAGE DoRec #-}

{-
  Turns mutually recursive declarations into a single recursive
  declaration, of a tuple value, trying to minimize the number
  of the tuple. This is an implementation of the algorithm described in
  http://www.informatik.uni-kiel.de/~mh/lehre/diplomarbeiten/siegel.pdf

  (c) 2009, Holger Siegel.
-}
module Curry.ExtendedFlat.UnMutual(unMutualProg) where

import Data.Graph
-- import Data.Function(on)
import Data.Maybe
import Data.List
import Control.Monad.State

import Curry.Base.Position(noRef)
import Curry.ExtendedFlat.Type
import Curry.ExtendedFlat.Goodies
import Curry.ExtendedFlat.MonadicGoodies


type Bind = (VarIndex, Expr)    -- (name, value)

newtype UnMutualState = UnMutualState { localCounter :: Int }


type UnMutualMonad = State UnMutualState


unMutualProg :: Prog -> Prog
unMutualProg p = evalState (updProgFuncsM
                            (\fdecl -> do
                               modify (\st -> st { localCounter = (maximum . map idxOf . allVarsInFunc) fdecl})
                               updFuncLetsM rmMutualRecursion fdecl)
                            p) (UnMutualState 1000)

rmMutualRecursion :: [Bind] -> Expr -> UnMutualMonad Expr
rmMutualRecursion bs body
    | allWhnf bs || length bs <= 1
        = return (Let bs body)
    | otherwise
        = do
          rec (body', bound, fbs) <- partitionBinds (fvs body) sccs (body, mkTuple fbs, [])
          mkSingleLet body' bound fbs
    where fvsGraph    = depGraph bs
          sccs        = sortSccs fvsGraph


mkSingleLet :: Expr -> Expr -> [VarIndex] -> UnMutualMonad Expr
mkSingleLet e2 e1 [v]
      = return (Let [(v, e1)] e2)
mkSingleLet body bound fbs
    = do recname <- newLocalName (Just fbsType)
         bound' <- mkFbSelectors recname bound
         body' <- mkFbSelectors recname body

         return (Let [(recname, bound')] body')
    where
      fbsType = TCons (mkQName tuplecon) (map (fromJust . typeofVar) fbs)
      tuplecon =  ("Prelude", "(" ++ replicate (length fbs -1 ) ',' ++ ")")
      mkFbSelectors recname b  = foldM (mkSelector recname)b fbs
      mkSelector recname b v   = nonrecLet v (mkSel (Var recname) v fbs) b


-- Some self-explaining helper functions:


nonrecLet :: VarIndex -> Expr -> Expr -> UnMutualMonad Expr
nonrecLet x e1 e2
    | x `elem` allVars e1
        = do vi <- newLocalName (typeofVar x)
             let e2' = subst x (Var vi) e2
             return (Let [(vi,e1)] e2')
    | otherwise = return (Let [(x,e1)] e2)


mkTuple :: [VarIndex] -> Expr
mkTuple [e]  = Var e
mkTuple es   = Comb ConsCall (mkTupleConstr es) $ map Var es


mkTupleConstr :: [a] -> QName
mkTupleConstr arity = curry mkQName "Prelude" ("(" ++ replicate (length arity-1) ',' ++ ")")

mkSel :: Expr -> VarIndex -> [VarIndex] -> Expr
mkSel e v vs = Case noRef Rigid e  [Branch pat (Var v)]
    where  pat   = Pattern tcon vs
           tcon  = mkTupleConstr vs


allWhnf :: [Bind] -> Bool
allWhnf = all (whnf . snd)

{-
The type |FvsNode| stands for a single node in a dependency graph.
It contains the binding, i.e. the identifier and the right hand side, as well
as a list of the identifiers the right hand side refers to.

Function |depGraph| turns a list of bindings into a dependency graph.

Function |sortSccs| calculates a list of strongly connected components
with the help of the library function |stronglyConnCompR|.
In contrast to the list of SCCs returned from this function,
the list of SCCs returned by |sortSccs| is in reversed order.
This is required, beacuase we start to process nested
declarations at the innermost binding.
-}

type FvsNode = (Bind, VarIndex, [VarIndex])

depGraph :: [Bind] -> [FvsNode]
depGraph = map (\(x, e) -> ((x, e), x, fvs e))


sortSccs :: [FvsNode] -> [SCC FvsNode]
sortSccs = reverse . stronglyConnCompR


{-
Function |partitionBinds| takes the following arguments: A list of identifier that occur
in the body of the declaration, a sorted list of strongy connected components,
a 3-tuple consising of the body of the declaration, a tuple expression that contains the
feedback variables, and the list of identifiers that are already added to the feedback set.
It returns an updated version of that 3-tuple, in which the body expression is 'surrounded'
by declarations of identifiers that the body refers to, the tuple expression is 'surrounded'
by declarations that are needed to define the feedback vriables, and the set of feedback
identifiers is the complete feedback set:
-}
partitionBinds :: [VarIndex] -> [SCC FvsNode]
               -> (Expr, Expr, [VarIndex])
               -> UnMutualMonad (Expr, Expr, [VarIndex])

-- When there is no binding left in a strongly connected component,
-- then move to the next SCC:
partitionBinds pull  (CyclicSCC []:ds) part
    = partitionBinds pull ds part

{- If the next SCC is cyclic, then pick the best candidate for the feedback set
and remove it from the SCC. The rest of the SCC breaks into smaller SCCs that are sorted
and added to the remaining list of SCCs. The selected candidate is added to the feedback set,
and its declaration is added to the tuple expression: -}
partitionBinds pull (CyclicSCC d:ds) (body, bound, fbs)
    = let (b@(v,e), d')  = pickFbNode pull d
          sccs      = sortSccs d' ++ ds
      in do l <- nonrecLet v e bound
            partitionBinds pull sccs (body, l, fst b:fbs)

-- If the next SCC is acyclic, then it is not added to the feedback set. Instead,
-- its declaration is added to the tuple expression. Depending on whether it
-- is needed in the body expression, its declaration is also  added to the body expression:
partitionBinds pull  (AcyclicSCC ((x,e),_,r):ds) (body, bound, fbs)
    = do l <- nonrecLet x e bound
         (body', pull') <- if x `elem` pull
                           then do l' <- nonrecLet x e body
                                   return (l', r `union` pull)
                           else return (body, pull)
         partitionBinds pull' ds (body', l, fbs)

-- When there are no more declarations to be processed, the 3-tuple is returned as
-- result:
partitionBinds _pull [] part
    = return part



-- Function |pickFbNode| picks the best candidate from a SCC. Irs choice depends
-- not only on the SCC, but also on whether the candidate is referred to by the body expression:

pickFbNode :: [VarIndex] -> [FvsNode] -> (Bind, [FvsNode])
pickFbNode pull defs = (b, d)
    where
    ds         = [x | (_, x, _) <- defs]
    (b, y, _)  = maximumBy (compare `on` weight pull ds) defs
    d          = [ n | n@(_, x, _) <- defs, x /= y]

-- not in ghc 6.8.2:
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on (.*.) f x y = f x .*. f y

{-
Function |weight| estimates the usefulness of adding an identifier to the feedback set.
It uses the fact, that tuples are sorted in exicographic order by default. An identifier is
rated on whether it
\begin{enumerate}
        \item has a recursive reference to itself,
        \item has a high number of references to other identifiers in the same SCC, or
        \item is referred to by the body expression.
\end{enumerate}
-}

weight :: [VarIndex] -> [VarIndex] -> FvsNode -> (Bool, Int, Bool)
weight pull defs (_,x,fv) = (recursive, length incoming, pulled)
    where  recursive  = x `elem` fv
           incoming   = fv `intersect` defs
           pulled     = x `elem` pull



newLocalName :: Maybe TypeExpr -> UnMutualMonad VarIndex
newLocalName t
    = do st <- get
         let counter = 1 + localCounter st
         put st { localCounter = counter  }
         return (VarIndex t counter)


subst :: VarIndex -> Expr -> Expr -> Expr
subst v x = po
    where po e@(Var v')
              | v==v'  = x
              | otherwise = e
          po e@(Lit _)
              = e
          po (Comb t n es)
              = Comb t n (map po es)
          po e@(Free vs e')
              | v `elem` vs = e
              | otherwise   = Free vs (po e')
          po e@(Let bs e')
              | lookup v bs == Nothing
              = Let (map poBind bs) (po e')
              | otherwise = e
          po (Or l r) = Or (po l) (po r)
          po (Case p t e bs) = Case p t (po e) (map poBranch bs)
          poBind  (w, rhs) = (w, po rhs)
          poBranch e@(Branch p rhs)
              | v `elem` trPattern (\_ args -> args) (const []) p
              = e
              | otherwise
              = Branch p (po rhs)



