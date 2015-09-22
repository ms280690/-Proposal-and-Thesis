% $Id: Types.lhs,v 1.11 2004/02/08 22:14:02 wlux Exp $
%
% Copyright (c) 2002, Wolfgang Lux
% See LICENSE for the full license.
%
% Modified by Martin Engelke (men@informatik.uni-kiel.de)
%
\nwfilename{Types.lhs}
\section{Types}
This module modules provides the definitions for the internal 
representation of types in the compiler.
\begin{verbatim}

> module Types where

> import Data.List
> import Data.Maybe

> import Curry.Base.Ident

\end{verbatim}
A type is either a type variable, an application of a type constructor
to a list of arguments, or an arrow type. The \texttt{TypeConstrained}
case is used for representing type variables that are restricted to a
particular set of types. At present, this is used for typing guard
expressions, which are restricted to be either of type \texttt{Bool}
or of type \texttt{Success}, and integer literals, which are
restricted to types \texttt{Int} and \texttt{Float}. If the type is
not restricted it defaults to the first type from the constraint list.
The case \texttt{TypeSkolem} is used for handling skolem types, which
result from the use of existentially quantified data constructors.

Type variables are represented with deBruijn style indices. Universally
quantified type variables are assigned indices in the order of their
occurrence in the type from left to right. This leads to a canonical
representation of types where $\alpha$-equivalence of two types
coincides with equality of the representation.

Note that even though \texttt{TypeConstrained} variables use indices
as well, these variables must never be quantified.
\begin{verbatim}

> data Type =
>     TypeConstructor QualIdent [Type]
>   | TypeVariable Int
>   | TypeConstrained [Type] Int
>   | TypeArrow Type Type
>   | TypeSkolem Int
>   | TypeRecord [(Ident,Type)] (Maybe Int)
>   deriving (Show, Eq)

\end{verbatim}
The function \texttt{isArrowType} checks whether a type is a function
type $t_1 \rightarrow t_2 \rightarrow \dots \rightarrow t_n$ . The
function \texttt{arrowArity} computes the arity $n$ of a function type
and \texttt{arrowBase} returns the type $t_n$.
\begin{verbatim}

> isArrowType :: Type -> Bool
> isArrowType (TypeArrow _ _) = True
> isArrowType _ = False

> arrowArity :: Type -> Int
> arrowArity (TypeArrow _ ty) = 1 + arrowArity ty
> arrowArity _ = 0

> arrowArgs :: Type -> [Type]
> arrowArgs (TypeArrow ty1 ty2) = ty1 : arrowArgs ty2
> arrowArgs ty = []

> arrowBase :: Type -> Type
> arrowBase (TypeArrow _ ty) = arrowBase ty
> arrowBase ty = ty

\end{verbatim}
The functions \texttt{typeVars}, \texttt{typeConstrs},
\texttt{typeSkolems} return a list of all type variables, type
constructors, or skolems occurring in a type $t$, respectively. Note
that \texttt{TypeConstrained} variables are not included in the set of
type variables because they cannot be generalized.
\begin{verbatim}

> typeVars :: Type -> [Int]
> typeVars ty = vars ty []
>   where vars (TypeConstructor _ tys) tvs = foldr vars tvs tys
>         vars (TypeVariable tv) tvs = tv : tvs
>         vars (TypeConstrained _ _) tvs = tvs
>         vars (TypeArrow ty1 ty2) tvs = vars ty1 (vars ty2 tvs)
>         vars (TypeSkolem _) tvs = tvs
>         vars (TypeRecord fs rtv) tvs =
>             foldr vars (maybe tvs (: tvs) rtv) (map snd fs)

> typeConstrs :: Type -> [QualIdent]
> typeConstrs ty = types ty []
>   where types (TypeConstructor tc tys) tcs = tc : foldr types tcs tys
>         types (TypeVariable _) tcs = tcs
>         types (TypeConstrained _ _) tcs = tcs
>         types (TypeArrow ty1 ty2) tcs = types ty1 (types ty2 tcs)
>         types (TypeSkolem _) tcs = tcs
>         types (TypeRecord fs _) tcs =
>             foldr types tcs (map snd fs)

> typeSkolems :: Type -> [Int]
> typeSkolems ty = skolems ty []
>   where skolems (TypeConstructor _ tys) sks = foldr skolems sks tys
>         skolems (TypeVariable _) sks = sks
>         skolems (TypeConstrained _ _) sks = sks
>         skolems (TypeArrow ty1 ty2) sks = skolems ty1 (skolems ty2 sks)
>         skolems (TypeSkolem k) sks = k : sks
>         skolems (TypeRecord fs _) sks =
>             foldr skolems sks (map snd fs)

> equTypes :: Type -> Type -> Bool
> equTypes t1 t2 = fst (equ [] t1 t2)
>  where 
>  equ is (TypeConstructor qid1 ts1) (TypeConstructor qid2 ts2)
>     | qid1 == qid2 = equs is ts1 ts2
>     | otherwise    = (False, is)
>  equ is (TypeVariable i1) (TypeVariable i2)
>     = maybe (True, (i1,i2):is) 
>             (\ i2' -> (i2 == i2', is))
>             (lookup i1 is)
>  equ is (TypeConstrained ts1 i1) (TypeConstrained ts2 i2)
>     = let (res, is') = equs is ts1 ts2
>       in  maybe (res, (i1,i2):is')
>                 (\ i2' -> (res && i2 == i2', is'))
>                 (lookup i1 is')
>  equ is (TypeArrow tf1 tt1) (TypeArrow tf2 tt2)
>     = let (res1, is1) = equ is tf1 tf2
>           (res2, is2) = equ is1 tt1 tt2
>       in  (res1 && res2, is2)
>  equ is (TypeSkolem i1) (TypeSkolem i2)
>     = maybe (True, (i1,i2):is)
>             (\ i2' -> (i2 == i2', is))
>             (lookup i1 is)
>  equ is (TypeRecord fs1 r1) (TypeRecord fs2 r2)
>     | isJust r1 && isJust r2
>       = let (res1, is1) = equ is (TypeVariable (fromJust r1))
>		                   (TypeVariable (fromJust r2))
>             (res2, is2) = equRecords is1 fs1 fs2
>         in  (res1 && res2, is2)
>     | isNothing r1 && isNothing r2 = equRecords is fs1 fs2
>     | otherwise = (False, is)
>  equ is _ _ = (False, is)
>	
>  equRecords is fs1 fs2 | length fs1 == length fs2 = equrec is fs1 fs2
>		         | otherwise = (False, is)
>    where
>    equrec is [] fs2 = (True, is)
>    equrec is ((l,t):fs1) fs2
>       = let (res1, is1) = maybe (False,is) (equ is t) (lookup l fs2)
>             (res2, is2) = equrec is1 fs1 fs2
>         in  (res1 && res2, is2)
>
>  equs is [] [] = (True, is)
>  equs is (t1:ts1) (t2:ts2)
>     = let (res1, is1) = equ is t1 t2
>           (res2, is2) = equs is1 ts1 ts2
>       in  (res1 && res2, is2)

\end{verbatim}
We support two kinds of quantifications of types here, universally
quantified type schemes $\forall\overline{\alpha} .
\tau(\overline{\alpha})$ and universally and existentially quantified
type schemes $\forall\overline{\alpha} \exists\overline{\eta} .
\tau(\overline{\alpha},\overline{\eta})$.  In both, quantified type
variables are assigned ascending indices starting from 0. Therefore it
is sufficient to record the numbers of quantified type variables in
the \texttt{ForAll} and \texttt{ForAllExist} constructors. In case of
the latter, the first of the two numbers is the number of universally
quantified variables and the second the number of existentially
quantified variables.
\begin{verbatim}

> data TypeScheme = ForAll Int Type deriving (Show, Eq)
> data ExistTypeScheme = ForAllExist Int Int Type deriving (Show, Eq)

\end{verbatim}
The functions \texttt{monoType} and \texttt{polyType} translate a type
$\tau$ into a monomorphic type scheme $\forall.\tau$ and a polymorphic
type scheme $\forall\overline{\alpha}.\tau$ where $\overline{\alpha} =
\textrm{fv}(\tau)$, respectively. \texttt{polyType} assumes that all
universally quantified variables in the type are assigned indices
starting with 0 and does not renumber the variables.
\begin{verbatim}

> monoType, polyType :: Type -> TypeScheme
> monoType ty = ForAll 0 ty
> polyType ty = ForAll (maximum (-1 : typeVars ty) + 1) ty

\end{verbatim}
There are a few predefined types:
\begin{verbatim}

> unitType,boolType,charType,intType,floatType,stringType,successType :: Type
> unitType = primType unitId []
> boolType = primType boolId []
> charType = primType charId []
> intType = primType intId []
> floatType = primType floatId []
> stringType = listType charType
> successType = primType successId []

> listType,ioType :: Type -> Type
> listType ty = primType listId [ty]
> ioType ty = primType ioId [ty]

> tupleType :: [Type] -> Type
> tupleType tys = primType (tupleId (length tys)) tys

> primType :: Ident -> [Type] -> Type
> primType = TypeConstructor . qualifyWith preludeMIdent

> typeVar :: Int -> Type
> typeVar = TypeVariable

\end{verbatim}



> qualifyType :: ModuleIdent -> Type -> Type
> qualifyType m (TypeConstructor tc tys)
>   | isTupleId tc' = tupleType tys'
>   | tc' == unitId && n == 0 = unitType
>   | tc' == listId && n == 1 = listType (head tys')
>   | otherwise = TypeConstructor (qualQualify m tc) tys'
>   where n = length tys'
>         tc' = unqualify tc
>         tys' = map (qualifyType m) tys
> qualifyType _ (TypeVariable tv) = TypeVariable tv
> qualifyType m (TypeConstrained tys tv) =
>   TypeConstrained (map (qualifyType m) tys) tv
> qualifyType m (TypeArrow ty1 ty2) =
>   TypeArrow (qualifyType m ty1) (qualifyType m ty2)
> qualifyType _ (TypeSkolem k) = TypeSkolem k
> qualifyType m (TypeRecord fs rty) =
>   TypeRecord (map (\ (l,ty) -> (l, qualifyType m ty)) fs) rty


> unqualifyType :: ModuleIdent -> Type -> Type
> unqualifyType m (TypeConstructor tc tys) =
>   TypeConstructor (qualUnqualify m tc) (map (unqualifyType m) tys)
> unqualifyType _ (TypeVariable tv) = TypeVariable tv
> unqualifyType m (TypeConstrained tys tv) =
>   TypeConstrained (map (unqualifyType m) tys) tv
> unqualifyType m (TypeArrow ty1 ty2) =
>   TypeArrow (unqualifyType m ty1) (unqualifyType m ty2)
> unqualifyType m (TypeSkolem k) = TypeSkolem k
> unqualifyType m (TypeRecord fs rty) =
>   TypeRecord (map (\ (l,ty) -> (l, unqualifyType m ty)) fs) rty

