
% $Id: Typing.lhs,v 1.7 2004/02/12 19:13:12 wlux Exp $
%
% Copyright (c) 2003-2006, Wolfgang Lux
% See LICENSE for the full license.
%
\nwfilename{Typing.lhs}
\section{Computing the Type of Curry Expressions}
\begin{verbatim}

> module Typing(Typeable(..)) where

> import Data.Maybe
> import Control.Monad
> import Control.Monad.State as S

> import Curry.Base.Ident
> import Curry.Syntax
> import Curry.Syntax.Utils

> import Types
> import Base
> import TypeSubst
> import TopEnv
> import Utils


\end{verbatim}
During the transformation of Curry source code into the intermediate
language, the compiler has to recompute the types of expressions. This
is simpler than type checking because the types of all variables are
known. Yet, the compiler still must handle functions and constructors
with polymorphic types and instantiate their type schemes using fresh
type variables. Since all types computed by \texttt{typeOf} are
monomorphic, we can use type variables with non-negative offsets for
the instantiation of type schemes here without risk of name conflicts.
Using non-negative offsets also makes it easy to distinguish these
fresh variables from free type variables introduce during type
inference, which must be regarded as constants here.

However, using non-negative offsets for fresh type variables gives
rise to two problems when those types are entered back into the type
environment, e.g., while introducing auxiliary variables during
desugaring. The first is that those type variables now appear to be
universally quantified variables, but with indices greater than the
number of quantified type variables.\footnote{To be precise, this can
  happen only for auxiliary variables, which have monomorphic types,
  whereas auxiliary functions will be assigned polymorphic types and
  these type variables will be properly quantified. However, in this
  case the assigned types may be too general.} This results in an
internal error (``Prelude.!!: index too large'') whenever such a type
is instantiated. The second problem is that there may be inadvertent
name captures because \texttt{computeType} always uses indices
starting at 0 for the fresh type variables. In order to avoid these
problems, \texttt{computeType} renames all type variables with
non-negative offsets after the final type has been computed, using
negative indices below the one with the smallest value occurring in
the type environment. Computing the minimum index of all type
variables in the type environment seems prohibitively inefficient.
However, recall that, thanks to laziness, the minimum is computed only
when the final type contains any type variables with non-negative
indices. This happens, for instance, 36 times while compiling the
prelude (for 159 evaluated applications of \texttt{typeOf}) and only
twice while compiling the standard \texttt{IO} module (for 21
applications of \texttt{typeOf}).\footnote{These numbers were obtained
  for version 0.9.9.}

A careful reader will note that inadvertent name captures are still
possible if one computes the types of two or more auxiliary variables
before actually entering their types into the environment. Therefore,
make sure that you enter the types of these auxiliary variables
immediately into the type environment, unless you are sure that those
types cannot contain fresh type variables. One such case are the free
variables of a goal.

\ToDo{In the long run, this module should be made obsolete by adding
attributes to the abstract syntax tree -- e.g., along the lines of
Chap.~6 in~\cite{PeytonJonesLester92:Book} -- and returning an
abstract syntax tree attributed with type information together with
the type environment from type inference. This also would allow
getting rid of the identifiers in the representation of integer
literals, which are used in order to implement overloading of
integer constants.}

\ToDo{When computing the type of an expression with a type signature
make use of the annotation instead of recomputing its type. In order
to do this, we must either ensure that the types are properly
qualified and expanded or we need access to the type constructor
environment.}
\begin{verbatim}

> type TyState a = S.StateT TypeSubst (S.State Int) a

> run :: TyState a -> ValueEnv -> a
> run m tyEnv = S.evalState (S.evalStateT m idSubst) 0

> class Typeable a where
>   typeOf :: ValueEnv -> a -> Type

> instance Typeable Ident where
>   typeOf = computeType identType

> instance Typeable ConstrTerm where
>   typeOf = computeType argType

> instance Typeable Expression where
>   typeOf = computeType exprType

> instance Typeable Rhs where
>   typeOf = computeType rhsType

> computeType f tyEnv x = normalize (run doComputeType tyEnv)
>   where doComputeType =
>           do
>             ty <- f tyEnv x
>             theta <- S.get
>             return (fixTypeVars tyEnv (subst theta ty))

> fixTypeVars :: ValueEnv -> Type -> Type
> fixTypeVars tyEnv ty = subst (foldr2 bindSubst idSubst tvs tvs') ty
>   where tvs = filter (>= 0) (typeVars ty)
>         tvs' = map TypeVariable [n - 1,n - 2 ..]
>         n = minimum (0 : concatMap typeVars tys)
>         tys = [ty | (_,Value _ (ForAll _ ty)) <- localBindings tyEnv]

> identType :: ValueEnv -> Ident -> TyState Type
> identType tyEnv x = instUniv (varType x tyEnv)

> litType :: ValueEnv -> Literal -> TyState Type
> litType _ (Char _ _)    = return charType
> litType tyEnv (Int v _) = identType tyEnv v
> litType _ (Float _ _)   = return floatType
> litType _ (String _ _)  = return stringType

> argType :: ValueEnv -> ConstrTerm -> TyState Type
> argType tyEnv (LiteralPattern l) = litType tyEnv l
> argType tyEnv (NegativePattern _ l) = litType tyEnv l
> argType tyEnv (VariablePattern v) = identType tyEnv v
> argType tyEnv (ConstructorPattern c ts) =
>   do
>     ty <- instUnivExist (constrType c tyEnv)
>     tys <- mapM (argType tyEnv) ts
>     unifyList (init (flatten ty)) tys
>     return (last (flatten ty))
>   where flatten (TypeArrow ty1 ty2) = ty1 : flatten ty2
>         flatten ty = [ty]
> argType tyEnv (InfixPattern t1 op t2) =
>   argType tyEnv (ConstructorPattern op [t1,t2])
> argType tyEnv (ParenPattern t) = argType tyEnv t
> argType tyEnv (TuplePattern _ ts)
>   | null ts = return unitType
>   | otherwise = liftM tupleType $ mapM (argType tyEnv) ts
> argType tyEnv (ListPattern _ ts) = freshTypeVar >>= flip elemType ts
>   where elemType ty [] = return (listType ty)
>         elemType ty (t:ts) =
>           argType tyEnv t >>= unify ty >> elemType ty ts
> argType tyEnv (AsPattern v _) = argType tyEnv (VariablePattern v)
> argType tyEnv (LazyPattern _ t) = argType tyEnv t
> argType tyEnv (FunctionPattern f ts) =
>   do 
>     ty <- instUniv (funType f tyEnv)
>     tys <- mapM (argType tyEnv) ts
>     unifyList (init (flatten ty)) tys
>     return (last (flatten ty))
>   where flatten (TypeArrow ty1 ty2) = ty1 : flatten ty2
>         flatten ty = [ty]
> argType tyEnv (InfixFuncPattern t1 op t2) =
>   argType tyEnv (FunctionPattern op [t1,t2])
> argType tyEnv (RecordPattern fs r)
>   | isJust r =
>     do
>       tys <- mapM (fieldPattType tyEnv) fs
>       rty <- argType tyEnv (fromJust r)
>       (TypeVariable i) <- freshTypeVar
>       unify rty (TypeRecord tys (Just i))
>       return rty
>   | otherwise =
>     do
>       tys <- mapM (fieldPattType tyEnv) fs
>       return (TypeRecord tys Nothing)

> fieldPattType :: ValueEnv -> Field ConstrTerm -> TyState (Ident,Type)
> fieldPattType tyEnv (Field _ l t) =
>   do
>     lty <- instUniv (labelType l tyEnv)
>     ty <- argType tyEnv t
>     unify lty ty
>     return (l,lty)

> exprType :: ValueEnv -> Expression -> TyState Type
> exprType tyEnv (Literal l) = litType tyEnv l
> exprType tyEnv (Variable v) = instUniv (funType v tyEnv)
> exprType tyEnv (Constructor c) = instUnivExist (constrType c tyEnv)
> exprType tyEnv (Typed e _) = exprType tyEnv e
> exprType tyEnv (Paren e) = exprType tyEnv e
> exprType tyEnv (Tuple _ es)
>   | null es = return unitType
>   | otherwise = liftM tupleType $ mapM (exprType tyEnv) es
> exprType tyEnv (List _ es) = freshTypeVar >>= flip elemType es
>   where elemType ty [] = return (listType ty)
>         elemType ty (e:es) =
>           exprType tyEnv e >>= unify ty >> elemType ty es
> exprType tyEnv (ListCompr _ e _) = liftM listType $ exprType tyEnv e
> exprType tyEnv (EnumFrom _) = return (listType intType)
> exprType tyEnv (EnumFromThen _ _) = return (listType intType)
> exprType tyEnv (EnumFromTo _ _) = return (listType intType)
> exprType tyEnv (EnumFromThenTo _ _ _) = return (listType intType)
> exprType tyEnv (UnaryMinus _ e) = exprType tyEnv e
> exprType tyEnv (Apply e1 e2) =
>   do
>     (ty1,ty2) <- exprType tyEnv e1 >>= unifyArrow
>     exprType tyEnv e2 >>= unify ty1
>     return ty2
> exprType tyEnv (InfixApply e1 op e2) =
>   do
>     (ty1,ty2,ty3) <- exprType tyEnv (infixOp op) >>= unifyArrow2
>     exprType tyEnv e1 >>= unify ty1
>     exprType tyEnv e2 >>= unify ty2
>     return ty3
> exprType tyEnv (LeftSection e op) =
>   do
>     (ty1,ty2,ty3) <- exprType tyEnv (infixOp op) >>= unifyArrow2
>     exprType tyEnv e >>= unify ty1
>     return (TypeArrow ty2 ty3)
> exprType tyEnv (RightSection op e) =
>   do
>     (ty1,ty2,ty3) <- exprType tyEnv (infixOp op) >>= unifyArrow2
>     exprType tyEnv e >>= unify ty2
>     return (TypeArrow ty1 ty3)
> exprType tyEnv (Lambda _ args e) =
>   do
>     tys <- mapM (argType tyEnv) args
>     ty <- exprType tyEnv e
>     return (foldr TypeArrow ty tys)
> exprType tyEnv (Let _ e) = exprType tyEnv e
> exprType tyEnv (Do _ e) = exprType tyEnv e
> exprType tyEnv (IfThenElse _ e1 e2 e3) =
>   do
>     exprType tyEnv e1 >>= unify boolType
>     ty2 <- exprType tyEnv e2
>     ty3 <- exprType tyEnv e3
>     unify ty2 ty3
>     return ty3
> exprType tyEnv (Case _ _ alts) = freshTypeVar >>= flip altType alts
>   where altType ty [] = return ty
>         altType ty (Alt _ _ rhs:alts) =
>           rhsType tyEnv rhs >>= unify ty >> altType ty alts
> exprType tyEnv (RecordConstr fs) =
>   do 
>     tys <- mapM (fieldExprType tyEnv) fs
>     return (TypeRecord tys Nothing)
> exprType tyEnv (RecordSelection r l) =
>   do 
>     lty <- instUniv (labelType l tyEnv)
>     rty <- exprType tyEnv r
>     (TypeVariable i) <- freshTypeVar
>     unify rty (TypeRecord [(l,lty)] (Just i))
>     return lty
> exprType tyEnv (RecordUpdate fs r) =
>   do
>     tys <- mapM (fieldExprType tyEnv) fs
>     rty <- exprType tyEnv r
>     (TypeVariable i) <- freshTypeVar
>     unify rty (TypeRecord tys (Just i))
>     return rty

> rhsType :: ValueEnv -> Rhs -> TyState Type
> rhsType tyEnv (SimpleRhs _ e _) = exprType tyEnv e
> rhsType tyEnv (GuardedRhs es _) = freshTypeVar >>= flip condExprType es
>   where condExprType ty [] = return ty
>         condExprType ty (CondExpr _ _ e:es) =
>           exprType tyEnv e >>= unify ty >> condExprType ty es

> fieldExprType :: ValueEnv -> Field Expression -> TyState (Ident,Type)
> fieldExprType tyEnv (Field _ l e) =
>   do
>     lty <- instUniv (labelType l tyEnv)
>     ty <- exprType tyEnv e
>     unify lty ty
>     return (l,lty)

\end{verbatim}
In order to avoid name conflicts with non-generalized type variables
in a type we instantiate quantified type variables using non-negative
offsets here.
\begin{verbatim}

> freshTypeVar :: TyState Type
> freshTypeVar = liftM TypeVariable $ S.lift (S.modify succ >> S.get)

> instType :: Int -> Type -> TyState Type
> instType n ty =
>   do
>     tys <- sequence (replicate n freshTypeVar)
>     return (expandAliasType tys ty)

> instUniv :: TypeScheme -> TyState Type
> instUniv (ForAll n ty) = instType n ty

> instUnivExist :: ExistTypeScheme -> TyState Type
> instUnivExist (ForAllExist n n' ty) = instType (n + n') ty

\end{verbatim}
When unifying two types, the non-generalized variables, i.e.,
variables with negative offsets, must not be substituted. Otherwise,
the unification algorithm is identical to the one used by the type
checker.
\begin{verbatim}

> unify :: Type -> Type -> TyState ()
> unify ty1 ty2 =
>   S.modify (\theta -> unifyTypes (subst theta ty1) (subst theta ty2) theta)

> unifyList :: [Type] -> [Type] -> TyState ()
> unifyList tys1 tys2 = sequence_ (zipWith unify tys1 tys2)

> unifyArrow :: Type -> TyState (Type,Type)
> unifyArrow ty =
>   do
>     theta <- S.get
>     case subst theta ty of
>       TypeVariable tv
>         | tv >= 0 ->
>             do
>               ty1 <- freshTypeVar
>               ty2 <- freshTypeVar
>               S.modify (bindVar tv (TypeArrow ty1 ty2))
>               return (ty1,ty2)
>       TypeArrow ty1 ty2 -> return (ty1,ty2)
>       ty' -> internalError ("unifyArrow (" ++ show ty' ++ ")")

> unifyArrow2 :: Type -> TyState (Type,Type,Type)
> unifyArrow2 ty =
>   do
>     (ty1,ty2) <- unifyArrow ty
>     (ty21,ty22) <- unifyArrow ty2
>     return (ty1,ty21,ty22)

> unifyTypes :: Type -> Type -> TypeSubst -> TypeSubst
> unifyTypes (TypeVariable tv1) (TypeVariable tv2) theta
>   | tv1 == tv2 = theta
> unifyTypes (TypeVariable tv) ty theta
>   | tv >= 0 = bindVar tv ty theta
> unifyTypes ty (TypeVariable tv) theta
>   | tv >= 0 = bindVar tv ty theta
> unifyTypes (TypeConstructor tc1 tys1) (TypeConstructor tc2 tys2) theta
>   | tc1 == tc2 = foldr2 unifyTypes theta tys1 tys2
> unifyTypes (TypeConstrained tys1 tv1) (TypeConstrained tys2 tv2) theta
>   | tv1 == tv2 = theta
> unifyTypes (TypeArrow ty11 ty12) (TypeArrow ty21 ty22) theta =
>   unifyTypes ty11 ty21 (unifyTypes ty12 ty22 theta)
> unifyTypes (TypeSkolem k1) (TypeSkolem k2) theta
>   | k1 == k2 = theta
> unifyTypes (TypeRecord fs1 Nothing) (TypeRecord fs2 Nothing) theta
>   | length fs1 == length fs2 = foldr (unifyTypedLabels fs1) theta fs2
> unifyTypes tr1@(TypeRecord fs1 Nothing) (TypeRecord fs2 (Just a2)) theta =
>   unifyTypes (TypeVariable a2)
>              tr1
>              (foldr (unifyTypedLabels fs1) theta fs2)
> unifyTypes tr1@(TypeRecord _ (Just _)) tr2@(TypeRecord _ Nothing) theta =
>   unifyTypes tr2 tr1 theta
> unifyTypes (TypeRecord fs1 (Just a1)) (TypeRecord fs2 (Just a2)) theta =
>   unifyTypes (TypeVariable a1)
>              (TypeVariable a2)
>              (foldr (unifyTypedLabels fs1) theta fs2)
> unifyTypes ty1 ty2 _ =
>   internalError ("unify: (" ++ show ty1 ++ ") (" ++ show ty2 ++ ")")

> unifyTypedLabels :: [(Ident,Type)] -> (Ident,Type) -> TypeSubst -> TypeSubst
> unifyTypedLabels fs1 (l,ty) theta =
>   maybe theta (\ty1 -> unifyTypes ty1 ty theta) (lookup l fs1)

\end{verbatim}
The functions \texttt{constrType}, \texttt{varType}, and
\texttt{funType} are used for computing the type of constructors,
pattern variables, and variables.

\ToDo{These functions should be shared with the type checker.}
\begin{verbatim}

> constrType :: QualIdent -> ValueEnv -> ExistTypeScheme
> constrType c tyEnv =
>   case qualLookupValue c tyEnv of
>     [DataConstructor _ sigma] -> sigma
>     [NewtypeConstructor _ sigma] -> sigma
>     _ -> internalError ("constrType " ++ show c)

> varType :: Ident -> ValueEnv -> TypeScheme
> varType v tyEnv =
>   case lookupValue v tyEnv of
>     [Value _ sigma] -> sigma
>     _ -> internalError ("varType " ++ show v)

> funType :: QualIdent -> ValueEnv -> TypeScheme
> funType f tyEnv =
>   case qualLookupValue f tyEnv of
>     [Value _ sigma] -> sigma
>     _ -> internalError ("funType " ++ show f)

> labelType :: Ident -> ValueEnv -> TypeScheme
> labelType l tyEnv =
>   case lookupValue l tyEnv of
>     [Label _ _ sigma] -> sigma
>     _ -> internalError ("labelType " ++ show l)

\end{verbatim}
