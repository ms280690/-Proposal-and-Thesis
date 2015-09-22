
% $Id: Lift.lhs,v 1.23 2004/02/13 14:02:54 wlux Exp $
%
% Copyright (c) 2001-2003, Wolfgang Lux
% See LICENSE for the full license.
%
\nwfilename{Lift.lhs}
\section{Lifting Declarations}
After desugaring and simplifying the code, the compiler lifts all
local function declarations to the top-level keeping only local
variable declarations. The algorithm used here is similar to
Johnsson's~\cite{Johnsson87:Thesis} (see also chapter 6
of~\cite{PeytonJonesLester92:Book}). It consists of two phases, first
we abstract each local function declaration, adding its free variables
as initial parameters and update all calls to take these variables
into account. Then all local function declarations are collected and
lifted to the top-level.
\begin{verbatim}

> module Lift(lift) where

> import Control.Monad
> import qualified Control.Monad.State as S
> import Data.List
> import qualified Data.Map as Map
> import qualified Data.Set as Set

> import Curry.Syntax
> import Curry.Syntax.Utils
> import Types
> import Curry.Base.Ident
> import Base
> import TopEnv
> import SCC

> lift :: ValueEnv -> EvalEnv -> Module -> (Module,ValueEnv,EvalEnv)
> lift tyEnv evEnv (Module m es ds) =
>   (Module m es (concatMap liftFunDecl ds'),tyEnv',evEnv')
>   where (ds',tyEnv',evEnv') =
>           S.evalState (S.evalStateT (abstractModule m ds) tyEnv) evEnv

\end{verbatim}
\paragraph{Abstraction}
Besides adding the free variables to every (local) function, the
abstraction pass also has to update the type environment in order to
reflect the new types of the expanded functions. As usual we use a
state monad transformer in order to pass the type environment
through. The environment constructed in the abstraction phase maps
each local function declaration onto its replacement expression,
i.e. the function applied to its free variables.
\begin{verbatim}

> type AbstractState a = S.StateT ValueEnv (S.State EvalEnv) a
> type AbstractEnv = Map.Map Ident Expression

> abstractModule :: ModuleIdent -> [Decl]
>                -> AbstractState ([Decl],ValueEnv,EvalEnv)
> abstractModule m ds =
>   do
>     ds' <- mapM (abstractDecl m "" [] Map.empty) ds
>     tyEnv' <- S.get
>     evEnv' <- S.lift S.get
>     return (ds',tyEnv',evEnv')

> abstractDecl :: ModuleIdent -> String -> [Ident] -> AbstractEnv -> Decl
>              -> AbstractState Decl
> abstractDecl m _ lvs env (FunctionDecl p f eqs) =
>   liftM (FunctionDecl p f) (mapM (abstractEquation m lvs env) eqs)
> abstractDecl m pre lvs env (PatternDecl p t rhs) =
>   liftM (PatternDecl p t) (abstractRhs m pre lvs env rhs)
> abstractDecl _ _ _ _ d = return d

> abstractEquation :: ModuleIdent -> [Ident] -> AbstractEnv -> Equation
>                  -> AbstractState Equation
> abstractEquation m lvs env (Equation p lhs@(FunLhs f ts) rhs) =
>   liftM (Equation p lhs)
>         (abstractRhs m (name f ++ ".") (lvs ++ bv ts) env rhs)

> abstractRhs :: ModuleIdent -> String -> [Ident] -> AbstractEnv -> Rhs
>             -> AbstractState Rhs
> abstractRhs m pre lvs env (SimpleRhs p e _) =
>   liftM (flip (SimpleRhs p) []) (abstractExpr m pre lvs env e)

\end{verbatim}
Within a declaration group we have to split the list of declarations
into the function and value declarations. Only the function
declarations are affected by the abstraction algorithm; the value
declarations are left unchanged except for abstracting their right
hand sides.

The abstraction of a recursive declaration group is complicated by the
fact that not all functions need to call each in a recursive
declaration group. E.g., in the following example neither g nor h
call each other.
\begin{verbatim}
  f = g True
    where x = f 1
          f z = y + z
          y = g False
          g z = if z then x else 0
\end{verbatim}
Because of this fact, f and g can be abstracted separately by adding
only \texttt{y} to \texttt{f} and \texttt{x} to \texttt{g}. On the
other hand, in the following example
\begin{verbatim}
  f x y = g 4
    where g p = h p + x
          h q = k + y + q
          k = g x
\end{verbatim}
the local function \texttt{g} uses \texttt{h}, so the free variables
of \texttt{h} have to be added to \texttt{g} as well. However, because
\texttt{h} does not call \texttt{g} it is sufficient to add only
\texttt{k} and \texttt{y} (and not \texttt{x}) to its definition. We
handle this by computing the dependency graph between the functions
and splitting this graph into its strongly connected components. Each
component is then processed separately, adding the free variables in
the group to its functions.

We have to be careful with local declarations within desugared case
expressions. If some of the cases have guards, e.g.,
\begin{verbatim}
  case e of
    x | x < 1 -> 1
    x -> let double y = y * y in double x
\end{verbatim}
the desugarer at present may duplicate code. While there is no problem
with local variable declaration being duplicated, we must avoid to
lift local function declarations more than once. Therefore
\texttt{abstractFunDecls} transforms only those function declarations
that have not been lifted and discards the other declarations. Note
that it is easy to check whether a function has been lifted by
checking whether an entry for its untransformed name is still present
in the type environment.
\begin{verbatim}

> abstractDeclGroup :: ModuleIdent -> String -> [Ident] -> AbstractEnv
>                   -> [Decl] -> Expression -> AbstractState Expression
> abstractDeclGroup m pre lvs env ds e =
>   abstractFunDecls m pre (lvs ++ bv vds) env (scc bv (qfv m) fds) vds e
>   where (fds,vds) = partition isFunDecl ds

> abstractFunDecls :: ModuleIdent -> String -> [Ident] -> AbstractEnv
>                  -> [[Decl]] -> [Decl] -> Expression
>                  -> AbstractState Expression
> abstractFunDecls m pre lvs env [] vds e =
>   do
>     vds' <- mapM (abstractDecl m pre lvs env) vds
>     e' <- abstractExpr m pre lvs env e
>     return (Let vds' e')
> abstractFunDecls m pre lvs env (fds:fdss) vds e =
>   do
>     fs' <- liftM (\tyEnv -> filter (not . isLifted tyEnv) fs) S.get
>     S.modify (abstractFunTypes m pre fvs fs')
>     S.lift (S.modify (abstractFunAnnots m pre fs'))
>     fds' <- mapM (abstractFunDecl m pre fvs lvs env')
>                  [d | d <- fds, any (`elem` fs') (bv d)]
>     e' <- abstractFunDecls m pre lvs env' fdss vds e
>     return (Let fds' e')
>   where fs = bv fds
>         fvs = filter (`elem` lvs) (Set.toList fvsRhs)
>         env' = foldr (bindF (map mkVar fvs)) env fs
>         fvsRhs = Set.unions
>           [Set.fromList (maybe [v] (qfv m) (Map.lookup v env)) | v <- qfv m fds]
>         bindF fvs f = Map.insert f (apply (mkFun m pre f) fvs)
>         isLifted tyEnv f = null (lookupValue f tyEnv)

> abstractFunTypes :: ModuleIdent -> String -> [Ident] -> [Ident]
>                  -> ValueEnv -> ValueEnv
> abstractFunTypes m pre fvs fs tyEnv = foldr abstractFunType tyEnv fs
>   where tys = map (varType tyEnv) fvs
>         abstractFunType f tyEnv =
>           qualBindFun m (liftIdent pre f)
>                         (foldr TypeArrow (varType tyEnv f) tys)
>                         (unbindFun f tyEnv)

> abstractFunAnnots :: ModuleIdent -> String -> [Ident] -> EvalEnv -> EvalEnv
> abstractFunAnnots m pre fs evEnv = foldr abstractFunAnnot evEnv fs
>   where abstractFunAnnot f evEnv =
>           case Map.lookup f evEnv of
>             Just ev -> Map.insert (liftIdent pre f) ev (Map.delete f evEnv)
>             Nothing -> evEnv

> abstractFunDecl :: ModuleIdent -> String -> [Ident] -> [Ident]
>                 -> AbstractEnv -> Decl -> AbstractState Decl
> abstractFunDecl m pre fvs lvs env (FunctionDecl p f eqs) =
>   abstractDecl m pre lvs env (FunctionDecl p f' (map (addVars f') eqs))
>   where f' = liftIdent pre f
>         addVars f (Equation p (FunLhs _ ts) rhs) =
>           Equation p (FunLhs f (map VariablePattern fvs ++ ts)) rhs
> abstractFunDecl m pre _ lvs env (ExternalDecl p cc ie f ty) =
>   return (ExternalDecl p cc ie (liftIdent pre f) ty)

> abstractExpr :: ModuleIdent -> String -> [Ident] -> AbstractEnv
>              -> Expression -> AbstractState Expression
> abstractExpr _ _ _ _ (Literal l) = return (Literal l)
> abstractExpr m pre lvs env (Variable v)
>   | isQualified v = return (Variable v)
>   | otherwise = maybe (return (Variable v)) (abstractExpr m pre lvs env)
>                       (Map.lookup (unqualify v) env)
> abstractExpr _ _ _ _ (Constructor c) = return (Constructor c)
> abstractExpr m pre lvs env (Apply e1 e2) =
>   do
>     e1' <- abstractExpr m pre lvs env e1
>     e2' <- abstractExpr m pre lvs env e2
>     return (Apply e1' e2')
> abstractExpr m pre lvs env (Let ds e) = abstractDeclGroup m pre lvs env ds e
> abstractExpr m pre lvs env (Case r e alts) =
>   do
>     e' <- abstractExpr m pre lvs env e
>     alts' <- mapM (abstractAlt m pre lvs env) alts
>     return (Case r e' alts')
> abstractExpr m _ _ _ _ = internalError "abstractExpr"

> abstractAlt :: ModuleIdent -> String -> [Ident] -> AbstractEnv -> Alt
>             -> AbstractState Alt
> abstractAlt m pre lvs env (Alt p t rhs) =
>   liftM (Alt p t) (abstractRhs m pre (lvs ++ bv t) env rhs)

\end{verbatim}
\paragraph{Lifting}
After the abstraction pass, all local function declarations are lifted
to the top-level.
\begin{verbatim}

> liftFunDecl :: Decl -> [Decl]
> liftFunDecl (FunctionDecl p f eqs) = (FunctionDecl p f eqs' : concat dss')
>   where (eqs',dss') = unzip (map liftEquation eqs)
> liftFunDecl d = [d]

> liftVarDecl :: Decl -> (Decl,[Decl])
> liftVarDecl (PatternDecl p t rhs) = (PatternDecl p t rhs',ds')
>   where (rhs',ds') = liftRhs rhs
> liftVarDecl (ExtraVariables p vs) = (ExtraVariables p vs,[])

> liftEquation :: Equation -> (Equation,[Decl])
> liftEquation (Equation p lhs rhs) = (Equation p lhs rhs',ds')
>   where (rhs',ds') = liftRhs rhs

> liftRhs :: Rhs -> (Rhs,[Decl])
> liftRhs (SimpleRhs p e _) = (SimpleRhs p e' [],ds')
>   where (e',ds') = liftExpr e

> liftDeclGroup :: [Decl] -> ([Decl],[Decl])
> liftDeclGroup ds = (vds',concat (map liftFunDecl fds ++ dss'))
>   where (fds,vds) = partition isFunDecl ds
>         (vds',dss') = unzip (map liftVarDecl vds)

> liftExpr :: Expression -> (Expression,[Decl])
> liftExpr (Literal l) = (Literal l,[])
> liftExpr (Variable v) = (Variable v,[])
> liftExpr (Constructor c) = (Constructor c,[])
> liftExpr (Apply e1 e2) = (Apply e1' e2',ds' ++ ds'')
>   where (e1',ds') = liftExpr e1
>         (e2',ds'') = liftExpr e2
> liftExpr (Let ds e) = (mkLet ds' e',ds'' ++ ds''')
>   where (ds',ds'') = liftDeclGroup ds
>         (e',ds''') = liftExpr e
>         mkLet ds e = if null ds then e else Let ds e
> liftExpr (Case r e alts) = (Case r e' alts',concat (ds':dss'))
>   where (e',ds') = liftExpr e
>         (alts',dss') = unzip (map liftAlt alts)
> liftExpr _ = internalError "liftExpr"

> liftAlt :: Alt -> (Alt,[Decl])
> liftAlt (Alt p t rhs) = (Alt p t rhs',ds')
>   where (rhs',ds') = liftRhs rhs


\end{verbatim}
\paragraph{Auxiliary definitions}
\begin{verbatim}

> isFunDecl :: Decl -> Bool
> isFunDecl (FunctionDecl _ _ _) = True
> isFunDecl (ExternalDecl _ _ _ _ _) = True
> isFunDecl _ = False

> mkFun :: ModuleIdent -> String -> Ident -> Expression
> mkFun m pre f = Variable (qualifyWith m (liftIdent pre f))

> mkVar :: Ident -> Expression
> mkVar v = Variable (qualify v)

> apply :: Expression -> [Expression] -> Expression
> apply = foldl Apply

> qualBindFun :: ModuleIdent -> Ident -> Type -> ValueEnv -> ValueEnv
> qualBindFun m f ty 
>   = qualBindTopEnv "Lift.qualBindFun" f' (Value f' (polyType ty))
>   where f' = qualifyWith m f

> unbindFun :: Ident -> ValueEnv -> ValueEnv
> unbindFun = unbindTopEnv

> varType :: ValueEnv -> Ident -> Type
> varType tyEnv v =
>   case lookupValue v tyEnv of
>     [Value _ (ForAll _ ty)] -> ty
>     _ -> internalError ("varType " ++ show v)

> liftIdent :: String -> Ident -> Ident
> liftIdent prefix x =
>     renameIdent (mkIdent (prefix ++ (show x))) (uniqueId x)
>    --renameIdent (mkIdent (prefix ++ name x ++ show (uniqueId x))) (uniqueId x)

\end{verbatim}
