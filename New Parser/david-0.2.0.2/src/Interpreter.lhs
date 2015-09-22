> {-# LANGUAGE TemplateHaskell, QuasiQuotes,
>              ViewPatterns,
>              GeneralizedNewtypeDeriving,
>              FlexibleInstances, FlexibleContexts,
>              UndecidableInstances, IncoherentInstances #-}
> 
> module Interpreter
>    ( resolve, resolve_
>    , MonadTrace(..), withTrace
>    , MonadGraphGen(..), runNoGraphT

>    , xTerm, prFalse, builtins,Stack, Branch, Path, root,  Trace(..), trace_
>    )
> where
> import Control.Monad.Reader
> import Control.Monad.Writer
> import Control.Monad.State
> import Control.Monad.Error
> import Data.Maybe (isJust)
> import Data.Generics (everywhere, mkT)
> import Control.Applicative ((<$>),(<*>),(<$),(<*), Applicative(..))
> import Data.Char (chr)
> import Data.List (sort, nub)
> 
> import Prelude
>
> import Syntax
> import Unifier
> import Database
> import Quote
> import Variables 
>
> xTerm :: String -> [Term] -> Term
> xTerm str terms = ($ terms) . Struct . Atom $ str

xTerm "world" [Struct (Atom "hello") []]
world(hello)

> 
> prFalse :: Term 
> prFalse = xTerm "false" []
> 
> builtins :: [Clause]
> builtins =
>    [ Clause (xTerm "="   [var "X", var "X"]) []
>    , Clause (xTerm "\\=" [var "X", var "X"]) [cut, prFalse]
>    , Clause (xTerm "\\=" [var "X", var "Y"]) []
>    , Clause [tm|not(A)|] [[tm|A|], cut, prFalse]
>    , Clause [tm|not(A)|] []

    The interpretation of both not and \\+ is faulty.  Consider
    not((!, fail)).  This should succeed, but the cut will cut the
    branch for not, and then the fail will back up into the caller.

>    , Clause (xTerm "\\+" [var "A"]) [var "A", cut, prFalse]
>    , Clause (xTerm "\\+" [var "A"]) []
>    , Clause (xTerm "true" []) []
>    , Clause [tm| ','(A, B)|] [tms| A, B|]
>    , Clause [tm|';'(A,_)|] [tms| A|]
>    , Clause [tm|';'(_,B)|] [tms| B|]
>    , ClauseFn (xTerm "is"  [var "L", var "R"]) is
>    , ClauseFn (xTerm "<"   [var "N", var "M"]) (binaryIntegerPredicate (<))
>    , ClauseFn (xTerm ">"   [var "N", var "M"]) (binaryIntegerPredicate (>))
>    , ClauseFn (xTerm "=<"  [var "N", var "M"]) (binaryIntegerPredicate (<=))
>    , ClauseFn (xTerm ">="  [var "N", var "M"]) (binaryIntegerPredicate (>=))
>    , ClauseFn (xTerm "=:=" [var "N", var "M"]) (binaryIntegerPredicate (==))
>    , ClauseFn (xTerm "@<" [var "T1", var "T2"]) (binaryPredicate (<))
>    , ClauseFn (xTerm "@>" [var "T1", var "T2"]) (binaryPredicate (>))
>    , ClauseFn (xTerm "@=<"[var "T1", var "T2"]) (binaryPredicate (<=))
>    , ClauseFn (xTerm "@>="[var "T1", var "T2"]) (binaryPredicate (>=))
>    , ClauseFn (xTerm "==" [var "T1", var "T2"]) (binaryPredicate (==))
>    , ClauseFn [tm|sort(Input,Output)|] (function sort_pl)
>    , Clause [tm|member(X,'.'(X, _))|] []
>    , Clause [tm|member(X, '.'(_,Xs))|] [[tm|member(X, Xs)|]]
>    , ClauseFn (xTerm "=.." [var "Term", var "List"]) univ
>    , ClauseFn (xTerm "atom" [var "T"]) atom
>    , ClauseFn (xTerm "char_code" [var "Atom", var "Code"]) char_code
>    , Clause [tm|phrase(RuleName, InputList)|]
>             [tms| phrase(RuleName,InputList,[])|]
>    , Clause [tm| phrase(Rule,InputList,Rest)|]
>                [tms|'=..'(Rule,L), append(L, [InputList,Rest], L1),
>                     '=..'(Goal,L1), Goal|]
>    , Clause (xTerm "append" [xTerm "[]" [], var "YS", var "YS"]) []
>    , Clause (xTerm "append" [xTerm "." [var "X", var "XS"], var "YS", xTerm "." [var "X", var "XSYS"]]) [xTerm "append" [var "XS", var "YS", var "XSYS"]]
>    ]
>  where
>    binaryIntegerPredicate :: (Integer -> Integer -> Bool) -> ([Term] -> [Goal])
>    binaryIntegerPredicate p [eval->Just n, eval->Just m] | n `p` m = []
>    binaryIntegerPredicate p _ = [prFalse]
> 
>    binaryPredicate :: (Term -> Term -> Bool) -> ([Term] -> [Goal])
>    binaryPredicate p [t1, t2] | t1 `p` t2 = []
>    binaryPredicate p _ = [prFalse]
> 
>    is [t, eval->Just n] = [xTerm "=" [t, xTerm (show n) []]]
>    is _                 = [prFalse]
> 
>    eval (PInteger n)                      = return n :: Maybe Integer
>    eval (Struct (Operator "+") [t1, t2])  = (+) <$> eval t1 <*> eval t2
>    eval (Struct (Operator "*") [t1, t2])  = (*) <$> eval t1 <*> eval t2
>    eval (Struct (Operator "-") [t1, t2])  = (-) <$> eval t1 <*> eval t2
>    eval (Struct (Atom "mod") [t1, t2])    = mod <$> eval t1 <*> eval t2
>    eval (Struct (Atom "div") [t1, t2])    = div <$> eval t1 <*> eval t2
>    eval (Struct (Operator "-") [t])       = negate <$> eval t
>    eval _                                 = mzero
> 
>    univ [Struct a ts, list] = [xTerm "=" [xTerm "." [Struct a [], foldr cons nil ts], list]]
>    univ [term, Struct (Operator ".") [Struct a [], t]]
>           = [xTerm "=" [term, Struct a (foldr_pl (:) [] t)]]
>    univ _                                          = [prFalse]
> 
>    atom [Struct _ []] = []
>    atom _             = [prFalse]
> 
>    char_code [Struct (Atom [c]) [], t] = [xTerm "=" [PInteger . toInteger . fromEnum $ c, t]]
>    char_code [t, PInteger n]           = [xTerm "=" [t, xTerm [chr $ fromInteger n] []]]
>    char_code _                         = [prFalse]
> 
>    function :: (Term -> Term) -> ([Term] -> [Goal])
>    function f [input, output] = [xTerm "=" [output, f input]]
> 
>    sort_pl = foldr cons nil . nub . sort . foldr_pl (:) []
> 
> class Monad m => MonadTrace m where
>    trace :: String -> m ()
> instance MonadTrace (Trace IO) where
>    trace = Trace . putStrLn
> instance MonadTrace IO where
>    trace _ = return ()
> instance MonadTrace (Either err) where
>    trace _ = return ()
> instance (MonadTrace m, MonadTrans t, Monad (t m)) => MonadTrace (t m) where
>    trace x = lift (trace x)
> 
> 
> newtype Trace m a = Trace { withTrace :: m a }  deriving (Functor, Monad, MonadError e)
> trace_ :: (Show a, MonadTrace m) => [Char] -> a -> m ()
> trace_ label x = trace (label++":\t"++show x)
> 
> 
> class Monad m => MonadGraphGen m where
>    createConnections :: Branch -> [Branch] -> [Branch] -> m ()
>    markSolution :: Unifier -> m ()
>    markCutBranches :: Stack -> m ()
> 
> instance MonadGraphGen m => MonadGraphGen (ReaderT r m) where
>    createConnections x y z = lift (createConnections x y z)
>    markSolution = lift . markSolution
>    markCutBranches = lift . markCutBranches
> 
> 
> newtype NoGraphT m a = NoGraphT {runNoGraphT :: m a} deriving (Monad, Functor, MonadFix, MonadPlus, Applicative, MonadError e)
> instance MonadTrans NoGraphT where
>    lift = NoGraphT
> 
> instance Monad m => MonadGraphGen (NoGraphT m) where
>    createConnections _ _ _ = NoGraphT $ return ()
>    markSolution      _      = NoGraphT $ return ()
>    markCutBranches   _      = NoGraphT $ return ()
> 
> 
> type Stack = [(Branch, [Branch])]
> type Branch = (Path, Unifier, [Goal])
> type Path = [Integer] -- Used for generating graph output
> root = [] :: Path
> 
> resolve :: (Functor m, MonadTrace m, Error e, MonadError e m) => [Clause] -> [Goal] -> m [Unifier]
> resolve program goals = runNoGraphT (resolve_ program goals)
> 

resolve [Clause (Struct (Atom "hello") [Struct (Atom "a") []]) []] [Struct (Atom "hello") [Var $ VariableName 0 "X"]]
[[(X,a)]]


> resolve_ :: (Functor m, MonadTrace m, Error e, MonadError e m, MonadGraphGen m) => [Clause] -> [Goal] -> m [Unifier]
> -- Yield all unifiers that resolve <goal> using the clauses from <program>.
> resolve_ program goals = map cleanup <$>
>     runReaderT (resolve' 1 (root, [], goals) [])
>                (createDB (builtins ++ program) (map Atom ["false","fail"]))
>     -- NOTE Is it a good idea to "hardcode" the builtins like this?
>   where
>       cleanup = filter ((\(VariableName i _) -> i == 0) . fst)
>
>       whenPredicateIsUnknown sig action = do
>           b <- asks (hasPredicate sig)
>           unless b action

    resolve' currently takes
        a depth;
        a triple of path, (global) unifier, list of goals;
        and a stack (essential list of of the above triples)

The depth is only used to ensure fresh variables on unification.

    here is a new plan:
        (1) drop the depth argument
        (2) replace the lists of goals with a more complex structure,
            which consists of a list of triples, where each triple
            contains: 
            (a) a list of remaining sub-goals for the current goal,
            (b) a unification dictiionary giving values for the
                bound variables in the current clause.
            (c) the cut point at the time the head matched.  This is 
                value like item (3) below.
        (3) a backtracking stack.  Each frame consists of a value like
            in (2), together with the state of global memory, a
            unification dictionary, a list of subgoals, and a pointer to
            a backtracking stack.

            Backtracking causes the list in (2) to be replaced by the
            list in the backtrack top, reset global memory to the state
            in the backtrack top, then adds a new triple to the goal
            list consisting of
                (a) the subgoals from the backtrack top
                (b) the unification dictionary from the backtrack top
                (c) the backtrack pointer from the backtrack top
            It then pops the backtracking stack and calls resolve'.


>       --resolve' :: Int -> Unifier -> [Goal] -> Stack -> m [Unifier]
>       resolve' depth (path, usf, []) stack = do
>          trace "=== yield solution ==="
>          trace_ "Depth" depth
>          trace_ "Unif." usf
> 
>          markSolution usf
> 
>          (cleanup usf:) <$> backtrack depth stack
>       resolve' depth (path, usf, Cut n:gs) stack = do
>          trace "=== resolve' (Cut) ==="
>          trace_ "Depth"   depth
>          trace_ "Unif."   usf
>          trace_ "Goals"   (Cut n:gs)
>          mapM_ (trace_ "Stack") stack
> 
>          createConnections (path, usf, Cut n:gs) [(1:path,[],[])] [(1:path, usf, gs)]
> 
>          markCutBranches (take n stack)
>          resolve' depth (1:path, usf, gs) (drop n stack)

    in the new plan, cut works by setting the backtrack stack to the value
    of cut pointer in the top frame of the goals.


>       resolve' depth (path, usf, goals@(nextGoal@(Struct (Operator "=") [l,r]):gs)) stack = do
>          -- This special case is here to avoid introducing unnecessary
>          -- variables that occur when applying "X=X." as a rule.
>          trace "=== resolve' (=/2) ==="
>          trace_ "Depth"   depth
>          trace_ "Unif."   usf
>          trace_ "Goals"   goals
>          mapM_ (trace_ "Stack") stack
> 
>          let bs = [ (1:path,u,[]) | u <- unify l r ]
>          let branches = do
>              (p,u,[]) <- bs
>              let u'  = usf +++ u
>                  gs' = map (apply u') gs
>                  gs'' = everywhere (mkT shiftCut) gs'
>              return (p, u', gs'')
> 
>          createConnections (path, usf, nextGoal:gs) bs branches
> 
>          choose depth (path,usf,gs) branches stack
> 
>         where
>          shiftCut (Cut n) = Cut (succ n)
>          shiftCut t       = t

    The above case needs some thought in terms of how to handle in terms
    of unification.  In the current thoughts about unification there is
    an assumption that unification always happens in a caller-callee
    context.  This is also a clear context where stack-stack references
    would be helpful.

>       resolve' depth (path, usf, nextGoal:gs) stack = do
>          trace "=== resolve' ==="
>          trace_ "Depth"   depth
>          trace_ "Unif."   usf
>          trace_ "Goals"   (nextGoal:gs)
>          mapM_ (trace_ "Stack") stack
>          let sig = signature nextGoal
>          whenPredicateIsUnknown sig $ do
>             throwError $ strMsg $ "Unknown predicate: " ++ show sig
>          bs <- getProtoBranches -- Branch generation happens in two phases so visualizers can pick what to display.
>          let branches = do
>                (p, u, newGoals) <- bs
>                let u' = usf +++ u
>                let gs'  = map (apply u') $ newGoals ++ gs
>                let gs'' = everywhere (mkT shiftCut) gs'
>                return (p, u', gs'')
> 
>          createConnections (path, usf, nextGoal:gs) bs branches
> 
>          choose depth (path,usf,gs) branches stack
>        where
>          getProtoBranches = do
>             clauses <- asks (getClauses nextGoal)

                we need a reader to get at Code here.

>             return $ do
>                (i,clause) <- zip [1..] $ renameVars clauses

                      new model: no need for renewing variables

>                u <- unify (apply usf nextGoal) (lhs clause)
>                return (i:path, u, rhs clause (map snd u))

                      what to do about Haskell functions?

> 
> 
>          shiftCut (Cut n) = Cut (succ n)
>          shiftCut t       = t
> 
>          renameVars = everywhere $ mkT $ \(VariableName _ v) -> VariableName depth v
> 




>       {-==================================================-}
>       {- data base clauses                                -}
>       {-==================================================-}
>       resolve' depth (path, usf, goals@(Struct (Atom "asserta") [fact]:gs)) stack = do
>          trace "=== resolve' (asserta/1) ==="
>          trace_ "Depth"   depth
>          trace_ "Unif."   usf
>          trace_ "Goals"   goals
>          mapM_ (trace_ "Stack") stack
> 
>          createConnections (path, usf, goals) [(1:path,[],[])] [(1:path, usf, gs)]
> 
>          local (asserta fact) $ resolve' depth (1:path, usf, gs) stack
>       resolve' depth (path, usf, goals@(Struct (Atom "assertz") [fact]:gs)) stack = do
>          trace "=== resolve' (assertz/1) ==="
>          trace_ "Depth"   depth
>          trace_ "Unif."   usf
>          trace_ "Goals"   goals
>          mapM_ (trace_ "Stack") stack
> 
>          createConnections (path, usf, goals) [(1:path,[],[])] [(1:path, usf, gs)]
> 
>          local (assertz fact) $ resolve' depth (1:path, usf, gs) stack
>       resolve' depth (path, usf, goals@(Struct (Atom "retract") [t]:gs)) stack = do
>          trace "=== resolve' (retract/1) ==="
>          trace_ "Depth"   depth
>          trace_ "Unif."   usf
>          trace_ "Goals"   goals
>          mapM_ (trace_ "Stack") stack
> 
>          createConnections (path, usf, goals) [(1:path,[],[])] [(1:path, usf, gs)]
> 
>          clauses <- asks (getClauses t)
>          case [ t' | Clause t' [] <- clauses, isJust (unify t t') ] of
>             []       -> return (fail "retract/1")
>             (fact:_) -> local (abolish fact) $ resolve' depth (1:path, usf, gs) stack


>       choose depth _           []              stack = backtrack depth stack
>       choose depth (path,u,gs) ((path',u',gs'):alts) stack = do
>          trace "=== choose ==="
>          trace_ "Depth"   depth
>          trace_ "Unif."   u
>          trace_ "Goals"   gs
>          mapM_ (trace_ "Alt.") ((path',u',gs'):alts)
>          mapM_ (trace_ "Stack") stack
>          resolve' (succ depth) (path',u',gs') (((path,u,gs),alts) : stack)
> 
>       backtrack _     [] = do
>          trace "=== give up ==="
>          return (fail "Goal cannot be resolved!")
>       backtrack depth (((path,u,gs),alts):stack) = do
>          trace "=== backtrack ==="
>          choose (pred depth) (path,u,gs) alts stack
