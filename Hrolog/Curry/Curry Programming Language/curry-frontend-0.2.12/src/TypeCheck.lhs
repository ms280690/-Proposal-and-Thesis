
% $Id: TypeCheck.lhs,v 1.90 2004/11/06 18:34:07 wlux Exp $
%
% Copyright (c) 1999-2004, Wolfgang Lux
% See LICENSE for the full license.
%
% Modified by Martin Engelke (men@informatik.uni-kiel.de)
%
\nwfilename{TypeCheck.lhs}
\section{Type Checking Curry Programs}
This module implements the type checker of the Curry compiler. The
type checker is invoked after the syntactic correctness of the program
has been verified. Local variables have been renamed already. Thus the
compiler can maintain a flat type environment (which is necessary in
order to pass the type information to later phases of the compiler).
The type checker now checks the correct typing of all expressions and
also verifies that the type signatures given by the user match the
inferred types. The type checker uses algorithm
W~\cite{DamasMilner82:Principal} for inferring the types of
unannotated declarations, but allows for polymorphic recursion when a
type annotation is present.
\begin{verbatim}

> module TypeCheck(typeCheck) where

> import Text.PrettyPrint.HughesPJ
> import Control.Monad.State as S
> import Data.List
> import Data.Maybe
> import qualified Data.Map as Map
> import qualified Data.Set as Set

> import Curry.Base.Position
> import Curry.Base.Ident
> import Curry.Syntax
> import Curry.Syntax.Pretty
> import Curry.Syntax.Utils


> import Base
> import Types
> import TopEnv
> import SCC
> import TypeSubst
> import Utils

> infixl 5 $-$

> ($-$) :: Doc -> Doc -> Doc
> x $-$ y = x $$ space $$ y

\end{verbatim}
Type checking proceeds as follows. First, the type constructor
environment is initialized by adding all types defined in the current
module. Next, the types of all data constructors and field labels
are entered into the type environment and then a type inference 
for all function and value definitions is performed. 
The type checker returns the resulting type
constructor and type environments.
\begin{verbatim}

> typeCheck :: ModuleIdent -> TCEnv -> ValueEnv -> [Decl] -> (TCEnv,ValueEnv)
> typeCheck m tcEnv tyEnv ds =
>   run (tcDecls m tcEnv' Map.empty vds >>
>        S.lift S.get >>= \theta -> S.get >>= \tyEnv' ->
>        return (tcEnv',subst theta tyEnv'))
>       (bindLabels m tcEnv' (bindConstrs m tcEnv' tyEnv))
>   where (tds,vds) = partition isTypeDecl ds
>         tcEnv' = bindTypes m tds tcEnv

\end{verbatim}

The type checker makes use of nested state monads in order to
maintain the type environment, the current substitution, and a counter
which is used for generating fresh type variables.
\begin{verbatim}

> type TcState a = S.StateT ValueEnv (S.StateT TypeSubst (S.State Int)) a

> run :: TcState a -> ValueEnv -> a
> run m tyEnv = S.evalState (S.evalStateT (S.evalStateT m tyEnv) idSubst) 0

\end{verbatim}
\paragraph{Defining Types}
Before type checking starts, the types defined in the local module
have to be entered into the type constructor environment. All type
synonyms occurring in the definitions are fully expanded and all type
constructors are qualified with the name of the module in which they
are defined. This is possible because Curry does not allow (mutually)
recursive type synonyms. In order to simplify the expansion of type
synonyms, the compiler first performs a dependency analysis on the
type definitions. This also makes it easy to identify (mutually)
recursive synonyms.

Note that \texttt{bindTC} is passed the \emph{final} type constructor
environment in order to handle the expansion of type synonyms. This
does not lead to a termination problem because \texttt{sortTypeDecls}
already has checked that there are no recursive type synonyms.

We have to be careful with existentially quantified type variables for
data constructors. An existentially quantified type variable may
shadow a universally quantified variable from the left hand side of
the type declaration. In order to avoid wrong indices being assigned
to these variables, we replace all shadowed variables in the left hand
side by \texttt{anonId} before passing them to \texttt{expandMonoType}
and \texttt{expandMonoTypes}, respectively.
\begin{verbatim}

> bindTypes :: ModuleIdent -> [Decl] -> TCEnv -> TCEnv
> bindTypes m ds tcEnv = tcEnv'
>   where tcEnv' = foldr (bindTC m tcEnv') tcEnv (sortTypeDecls m ds)

> bindTC :: ModuleIdent -> TCEnv -> Decl -> TCEnv -> TCEnv
> bindTC m tcEnv (DataDecl _ tc tvs cs) =
>   bindTypeInfo DataType m tc tvs (map (Just . mkData) cs)
>   where mkData (ConstrDecl _ evs c tys) = Data c (length evs) tys'
>           where tys' = expandMonoTypes m tcEnv (cleanTVars tvs evs) tys
>         mkData (ConOpDecl _ evs ty1 op ty2) = Data op (length evs) tys'
>           where tys' = expandMonoTypes m tcEnv (cleanTVars tvs evs) [ty1,ty2]
> bindTC m tcEnv (NewtypeDecl _ tc tvs (NewConstrDecl _ evs c ty)) =
>   bindTypeInfo RenamingType m tc tvs (Data c (length evs) ty')
>   where ty' = expandMonoType m tcEnv (cleanTVars tvs evs) ty
> bindTC m tcEnv (TypeDecl _ tc tvs ty) =
>   bindTypeInfo AliasType m tc tvs (expandMonoType m tcEnv tvs ty)
> bindTC _ _ _ = id

> cleanTVars :: [Ident] -> [Ident] -> [Ident]
> cleanTVars tvs evs = [if tv `elem` evs then anonId else tv | tv <- tvs]

> sortTypeDecls :: ModuleIdent -> [Decl] -> [Decl]
> sortTypeDecls m = map (typeDecl m) . scc bound free
>   where bound (DataDecl _ tc _ _) = [tc]
>         bound (NewtypeDecl _ tc _ _) = [tc]
>         bound (TypeDecl _ tc _ _) = [tc]
>         free (DataDecl _ _ _ _) = []
>         free (NewtypeDecl _ _ _ _) = []
>         free (TypeDecl _ _ _ ty) = ft m ty []

> typeDecl :: ModuleIdent -> [Decl] -> Decl
> typeDecl _ [] = internalError "typeDecl"
> typeDecl _ [d@(DataDecl _ _ _ _)] = d
> typeDecl _ [d@(NewtypeDecl _ _ _ _)] = d
> typeDecl m [d@(TypeDecl p tc _ ty)]
>   | tc `elem` ft m ty [] = errorAt' (recursiveTypes [tc])
>   | otherwise = d
> typeDecl _ (TypeDecl p tc _ _ : ds) =
>   errorAt' (recursiveTypes (tc : [tc' | TypeDecl _ tc' _ _ <- ds]))

> ft :: ModuleIdent -> TypeExpr -> [Ident] -> [Ident]
> ft m (ConstructorType tc tys) tcs =
>   maybe id (:) (localIdent m tc) (foldr (ft m) tcs tys)
> ft _ (VariableType _) tcs = tcs
> ft m (TupleType tys) tcs = foldr (ft m) tcs tys
> ft m (ListType ty) tcs = ft m ty tcs
> ft m (ArrowType ty1 ty2) tcs = ft m ty1 $ ft m ty2 $ tcs
> ft m (RecordType fs rty) tcs = 
>   foldr (ft m) (maybe tcs (\ty -> ft m ty tcs) rty) (map snd fs)

\end{verbatim}
\paragraph{Defining Data Constructors}
In the next step, the types of all data constructors are entered into
the type environment using the information just entered into the type
constructor environment. Thus, we can be sure that all type variables
have been properly renamed and all type synonyms are already expanded.
\begin{verbatim}

> bindConstrs :: ModuleIdent -> TCEnv -> ValueEnv -> ValueEnv
> bindConstrs m tcEnv tyEnv =
>   foldr (bindData . snd) tyEnv (localBindings tcEnv)
>   where bindData (DataType tc n cs) tyEnv =
>           foldr (bindConstr m n (constrType tc n)) tyEnv (catMaybes cs)
>         bindData (RenamingType tc n (Data c n' ty)) tyEnv =
>           bindGlobalInfo NewtypeConstructor m c
>                          (ForAllExist n n' (TypeArrow ty (constrType tc n)))
>                          tyEnv
>         bindData (AliasType _ _ _) tyEnv = tyEnv
>         bindConstr m n ty (Data c n' tys) =
>           bindGlobalInfo DataConstructor m c
>                          (ForAllExist n n' (foldr TypeArrow ty tys))
>         constrType tc n = TypeConstructor tc (map TypeVariable [0..n-1])

\end{verbatim}
\paragraph{Defining Field Labels}
Records can only be declared as type aliases. So currently there is
nothing more to do than entering all typed record fields (labels) 
which occur in record types on the right-hand-side of type aliases 
into the type environment. Since we use the type constructor environment
again, we can be sure that all type variables
have been properly renamed and all type synonyms are already expanded.
\begin{verbatim}

> bindLabels :: ModuleIdent -> TCEnv -> ValueEnv -> ValueEnv
> bindLabels m tcEnv tyEnv =
>   foldr (bindFieldLabels . snd) tyEnv (localBindings tcEnv)
>   where bindFieldLabels (AliasType r _ (TypeRecord fs _)) tyEnv =
>           foldr (bindField r) tyEnv fs
>	  bindFieldLabels _ tyEnv = tyEnv
>	  
>         bindField r (l,ty) tyEnv =
>           case (lookupValue l tyEnv) of
>             [] -> bindLabel l r (polyType ty) tyEnv 
>             _  -> tyEnv

\end{verbatim}
\paragraph{Type Signatures}
The type checker collects type signatures in a flat environment. All
anonymous variables occurring in a signature are replaced by fresh
names. However, the type is not expanded so that the signature is
available for use in the error message that is printed when the
inferred type is less general than the signature.
\begin{verbatim}

> type SigEnv = Map.Map Ident TypeExpr

> bindTypeSig :: Ident -> TypeExpr -> SigEnv -> SigEnv
> bindTypeSig = Map.insert

> bindTypeSigs :: Decl -> SigEnv -> SigEnv
> bindTypeSigs (TypeSig _ vs ty) env =
>   foldr (flip bindTypeSig (nameSigType ty)) env vs 
> bindTypeSigs _ env = env

> lookupTypeSig :: Ident -> SigEnv -> Maybe TypeExpr
> lookupTypeSig = Map.lookup

> qualLookupTypeSig :: ModuleIdent -> QualIdent -> SigEnv -> Maybe TypeExpr
> qualLookupTypeSig m f sigs = localIdent m f >>= flip lookupTypeSig sigs

> nameSigType :: TypeExpr -> TypeExpr
> nameSigType ty = fst (nameType ty (filter (`notElem` fv ty) nameSupply))

> nameTypes :: [TypeExpr] -> [Ident] -> ([TypeExpr],[Ident])
> nameTypes (ty:tys) tvs = (ty':tys',tvs'')
>   where (ty',tvs') = nameType ty tvs
>         (tys',tvs'') = nameTypes tys tvs'
> nameTypes [] tvs = ([],tvs)

> nameType :: TypeExpr -> [Ident] -> (TypeExpr,[Ident])
> nameType (ConstructorType tc tys) tvs = (ConstructorType tc tys',tvs')
>   where (tys',tvs') = nameTypes tys tvs
> nameType (VariableType tv) (tv':tvs)
>   | tv == anonId = (VariableType tv',tvs)
>   | otherwise = (VariableType tv,tv':tvs)
> nameType (TupleType tys) tvs = (TupleType tys',tvs')
>   where (tys',tvs') = nameTypes tys tvs
> nameType (ListType ty) tvs = (ListType ty',tvs')
>   where (ty',tvs') = nameType ty tvs
> nameType (ArrowType ty1 ty2) tvs = (ArrowType ty1' ty2',tvs'')
>   where (ty1',tvs') = nameType ty1 tvs
>         (ty2',tvs'') = nameType ty2 tvs'
> nameType (RecordType fs rty) tvs = 
>   (RecordType (zip ls tys') (listToMaybe rty'), tvs)
>   where (ls, tys) = unzip fs
>         (tys', _) = nameTypes tys tvs
>         (rty', _) = nameTypes (maybeToList rty) tvs
        
\end{verbatim}
\paragraph{Type Inference}
Before type checking a group of declarations, a dependency analysis is
performed and the declaration group is eventually transformed into
nested declaration groups which are checked separately. Within each
declaration group, first the left hand sides of all declarations are
typed. Next, the right hand sides of the declarations are typed in the
extended type environment. Finally, the types for the left and right
hand sides are unified and the types of all defined functions are
generalized. The generalization step will also check that the type
signatures given by the user match the inferred types.

Argument and result types of foreign functions using the
\texttt{ccall} calling convention are restricted to the basic types
\texttt{Bool}, \texttt{Char}, \texttt{Int}, and \texttt{Float}. In
addition, \texttt{IO}~$t$ is a legitimate result type when $t$ is
either one of the basic types or \texttt{()}.

\ToDo{Extend the set of legitimate types to match the types admitted
  by the Haskell Foreign Function Interface
  Addendum.~\cite{Chakravarty03:FFI}}
\begin{verbatim}

> tcDecls :: ModuleIdent -> TCEnv -> SigEnv -> [Decl] -> TcState ()
> tcDecls m tcEnv sigs ds =
>   mapM_ (tcDeclGroup m tcEnv (foldr bindTypeSigs sigs ods))
>         (scc bv (qfv m) vds)
>   where (vds,ods) = partition isValueDecl ds

> tcDeclGroup :: ModuleIdent -> TCEnv -> SigEnv -> [Decl] -> TcState ()
> --tcDeclGroup m tcEnv _ [ForeignDecl p cc _ f ty] =
> --  tcForeignFunct m tcEnv p cc f ty
> tcDeclGroup m tcEnv _ [ExternalDecl _ _ _ f ty] =
>   tcExternalFunct m tcEnv f ty
> tcDeclGroup m tcEnv sigs [FlatExternalDecl _ fs] =
>   mapM_ (tcFlatExternalFunct m tcEnv sigs) fs
> tcDeclGroup m tcEnv sigs [ExtraVariables _ vs] =
>   mapM_ (tcExtraVar m tcEnv sigs ) vs
> tcDeclGroup m tcEnv sigs ds =
>   do
>     tyEnv0 <- S.get
>     tysLhs <- mapM (tcDeclLhs m tcEnv sigs) ds
>     tysRhs <- mapM (tcDeclRhs m tcEnv tyEnv0 sigs) ds
>     sequence_ (zipWith3 (unifyDecl m) ds tysLhs tysRhs)
>     theta <- S.lift S.get
>     mapM_ (genDecl m tcEnv sigs (fvEnv (subst theta tyEnv0)) theta) ds

> --tcForeignFunct :: ModuleIdent -> TCEnv -> Position -> CallConv -> Ident
> --               -> TypeExpr -> TcState ()
> --tcForeignFunct m tcEnv p cc f ty =
> --  S.modify (bindFun m f (checkForeignType cc (expandPolyType tcEnv ty)))
> --  where checkForeignType CallConvPrimitive ty = ty
> --        checkForeignType CallConvCCall (ForAll n ty) =
> --          ForAll n (checkCCallType ty)
> --        checkCCallType (TypeArrow ty1 ty2)
> --          | isCArgType ty1 = TypeArrow ty1 (checkCCallType ty2)
> --          | otherwise = errorAt p (invalidCType "argument" m ty1)
> --        checkCCallType ty
> --          | isCResultType ty = ty
> --          | otherwise = errorAt p (invalidCType "result" m ty)
> --        isCArgType (TypeConstructor tc []) = tc `elem` basicTypeId
> --        isCArgType _ = False
> --        isCResultType (TypeConstructor tc []) = tc `elem` basicTypeId
> --        isCResultType (TypeConstructor tc [ty]) =
> --          tc == qIOId && (ty == unitType || isCArgType ty)
> --        isCResultType _ = False
> --        basicTypeId = [qBoolId,qCharId,qIntId,qFloatId]

> tcExternalFunct :: ModuleIdent -> TCEnv -> Ident -> TypeExpr -> TcState ()
> tcExternalFunct m tcEnv  f ty =
>   S.modify (bindFun m f (expandPolyType m tcEnv ty))

> tcFlatExternalFunct :: ModuleIdent -> TCEnv -> SigEnv -> Ident -> TcState ()
> tcFlatExternalFunct m tcEnv sigs f =
>   typeOf f tcEnv sigs >>= S.modify . bindFun m f
>   where typeOf f tcEnv sigs =
>           case lookupTypeSig f sigs of
>             Just ty -> return (expandPolyType m tcEnv ty)
>             Nothing -> internalError "tcFlatExternalFunct"

> tcExtraVar :: ModuleIdent -> TCEnv -> SigEnv -> Ident
>            -> TcState ()
> tcExtraVar m tcEnv sigs v =
>   typeOf v tcEnv sigs >>= S.modify . bindFun m v . monoType
>   where typeOf v tcEnv sigs =
>           case lookupTypeSig v sigs of
>             Just ty
>               | n == 0 -> return ty'
>               | otherwise -> errorAt' (polymorphicFreeVar v)
>               where ForAll n ty' = expandPolyType m tcEnv ty
>             Nothing -> freshTypeVar

> tcDeclLhs :: ModuleIdent -> TCEnv -> SigEnv -> Decl -> TcState Type
> tcDeclLhs m tcEnv sigs (FunctionDecl p f _) =
>   tcConstrTerm m tcEnv sigs p (VariablePattern f)
> tcDeclLhs m tcEnv sigs (PatternDecl p t _) = tcConstrTerm m tcEnv sigs p t

> tcDeclRhs :: ModuleIdent -> TCEnv -> ValueEnv -> SigEnv -> Decl
>           -> TcState Type
> tcDeclRhs m tcEnv tyEnv0 sigs (FunctionDecl _ f (eq:eqs)) =
>   tcEquation m tcEnv tyEnv0 sigs eq >>= flip tcEqns eqs
>   where tcEqns ty [] = return ty
>         tcEqns ty (eq@(Equation p _ _):eqs) =
>           tcEquation m tcEnv tyEnv0 sigs eq >>=
>           unify p "equation" (ppDecl (FunctionDecl p f [eq])) m ty >>
>           tcEqns ty eqs
> tcDeclRhs m tcEnv tyEnv0 sigs (PatternDecl _ _ rhs) =
>   tcRhs m tcEnv tyEnv0 sigs rhs

> unifyDecl :: ModuleIdent -> Decl -> Type -> Type -> TcState ()
> unifyDecl m (FunctionDecl p f _) =
>   unify p "function binding" (text "Function:" <+> ppIdent f) m
> unifyDecl m (PatternDecl p t _) =
>   unify p "pattern binding" (ppConstrTerm 0 t) m

\end{verbatim}
In Curry we cannot generalize the types of let-bound variables because
they can refer to logic variables. Without this monomorphism
restriction unsound code like
\begin{verbatim}
bug = x =:= 1 & x =:= 'a'
  where x :: a
        x = fresh
fresh :: a
fresh = x where x free
\end{verbatim}
could be written. Note that \texttt{fresh} has the polymorphic type
$\forall\alpha.\alpha$. This is correct because \texttt{fresh} is a
function and therefore returns a different variable at each
invocation.

The code in \texttt{genVar} below also verifies that the inferred type
for a variable or function matches the type declared in a type
signature. As the declared type is already used for assigning an initial
type to a variable when it is used, the inferred type can only be more
specific. Therefore, if the inferred type does not match the type
signature the declared type must be too general.
\begin{verbatim}

> genDecl :: ModuleIdent -> TCEnv -> SigEnv -> Set.Set Int -> TypeSubst -> Decl
>         -> TcState ()
> genDecl m tcEnv sigs lvs theta (FunctionDecl _ f _) =
>   S.modify (genVar True m tcEnv sigs lvs theta f)
> genDecl m tcEnv sigs lvs theta (PatternDecl p t _) =
>   mapM_ (S.modify . genVar False m tcEnv sigs lvs theta ) (bv t)

> genVar :: Bool -> ModuleIdent -> TCEnv -> SigEnv -> Set.Set Int -> TypeSubst
>        -> Ident -> ValueEnv -> ValueEnv
> genVar poly m tcEnv sigs lvs theta v tyEnv =
>   case lookupTypeSig v sigs of
>     Just sigTy
>       | cmpTypes sigma (expandPolyType m tcEnv sigTy) -> tyEnv'
>       | otherwise -> errorAt (positionOfIdent v) 
>                              (typeSigTooGeneral m what sigTy sigma)
>     Nothing -> tyEnv'
>   where what = text (if poly then "Function:" else "Variable:") <+> ppIdent v
>         tyEnv' = rebindFun m v sigma tyEnv
>         sigma = genType poly (subst theta (varType v tyEnv))
>         genType poly (ForAll n ty)
>           | n > 0 = internalError ("genVar: " ++ showLine (positionOfIdent v) ++ 
>                                    show v ++ " :: " ++ show ty)
>           | poly = gen lvs ty
>           | otherwise = monoType ty
>         cmpTypes (ForAll _ t1) (ForAll _ t2) = equTypes t1 t2

> tcEquation :: ModuleIdent -> TCEnv -> ValueEnv -> SigEnv -> Equation
>            -> TcState Type
> tcEquation m tcEnv tyEnv0 sigs (Equation p lhs rhs) =
>   do
>     tys <- mapM (tcConstrTerm m tcEnv sigs p) ts
>     ty <- tcRhs m tcEnv tyEnv0 sigs rhs
>     checkSkolems p m (text "Function: " <+> ppIdent f) tyEnv0
>                  (foldr TypeArrow ty tys)
>   where (f,ts) = flatLhs lhs

> tcLiteral :: ModuleIdent -> Literal -> TcState Type
> tcLiteral _ (Char _ _) = return charType
> tcLiteral m (Int v _)  = --return intType
>   do
>     ty <- freshConstrained [intType,floatType]
>     S.modify (bindFun m v (monoType ty))
>     return ty
> tcLiteral _ (Float _ _) = return floatType
> tcLiteral _ (String _ _) = return stringType

> tcConstrTerm :: ModuleIdent -> TCEnv -> SigEnv -> Position -> ConstrTerm
>              -> TcState Type
> tcConstrTerm m tcEnv sigs _ (LiteralPattern l) = tcLiteral m l
> tcConstrTerm m tcEnv sigs _ (NegativePattern _ l) = tcLiteral m l
> tcConstrTerm m tcEnv sigs _ (VariablePattern v) =
>   do 
>     ty <- case lookupTypeSig v sigs of
>             Just t -> inst (expandPolyType m tcEnv t)
>             Nothing -> freshTypeVar
>     S.modify (bindFun m v (monoType ty))
>     return ty
>   
> tcConstrTerm m tcEnv sigs p t@(ConstructorPattern c ts) =
>   do
>     tyEnv <- S.get
>     ty <- skol (constrType m c tyEnv)
>     unifyArgs (ppConstrTerm 0 t) ts ty
>   where unifyArgs _ [] ty = return ty
>         unifyArgs doc (t:ts) (TypeArrow ty1 ty2) =
>           tcConstrTerm m tcEnv sigs p t >>=
>           unify p "pattern" (doc $-$ text "Term:" <+> ppConstrTerm 0 t)
>                 m ty1 >>
>           unifyArgs doc ts ty2
>         unifyArgs _ _ _ = internalError "tcConstrTerm"
> tcConstrTerm m tcEnv sigs p t@(InfixPattern t1 op t2) =
>   do
>     tyEnv <- S.get
>     ty <- skol (constrType m op tyEnv)
>     unifyArgs (ppConstrTerm 0 t) [t1,t2] ty
>   where unifyArgs _ [] ty = return ty
>         unifyArgs doc (t:ts) (TypeArrow ty1 ty2) =
>           tcConstrTerm m tcEnv sigs p t >>=
>           unify p "pattern" (doc $-$ text "Term:" <+> ppConstrTerm 0 t)
>                 m ty1 >>
>           unifyArgs doc ts ty2
>         unifyArgs _ _ _ = internalError "tcConstrTerm"
> tcConstrTerm m tcEnv sigs p (ParenPattern t) = tcConstrTerm m tcEnv sigs p t
> tcConstrTerm m tcEnv sigs p (TuplePattern _ ts)
>  | null ts = return unitType
>  | otherwise = liftM tupleType $ mapM (tcConstrTerm m tcEnv sigs p) ts
> tcConstrTerm m tcEnv sigs p t@(ListPattern _ ts) =
>   freshTypeVar >>= flip (tcElems (ppConstrTerm 0 t)) ts
>   where tcElems _ ty [] = return (listType ty)
>         tcElems doc ty (t:ts) =
>           tcConstrTerm m tcEnv sigs p t >>=
>           unify p "pattern" (doc $-$ text "Term:" <+> ppConstrTerm 0 t)
>                 m ty >>
>           tcElems doc ty ts
> tcConstrTerm m tcEnv sigs p t@(AsPattern v t') =
>   do
>     ty1 <- tcConstrTerm m tcEnv sigs p (VariablePattern v)
>     ty2 <- tcConstrTerm m tcEnv sigs p t'
>     unify p "pattern" (ppConstrTerm 0 t) m ty1 ty2
>     return ty1
> tcConstrTerm m tcEnv sigs p (LazyPattern _ t) = tcConstrTerm m tcEnv sigs p t
> tcConstrTerm m tcEnv sigs p t@(FunctionPattern f ts) =
>   do
>     tyEnv <- S.get
>     ty <- inst (funType m f tyEnv) --skol (constrType m c tyEnv)
>     unifyArgs (ppConstrTerm 0 t) ts ty
>   where unifyArgs _ [] ty = return ty
>         unifyArgs doc (t:ts) ty@(TypeVariable _) =
>           do (alpha,beta) <- tcArrow p "function pattern" doc m ty
>	       ty' <- tcConstrTermFP m tcEnv sigs p t
>	       unify p "function pattern"
>	             (doc $-$ text "Term:" <+> ppConstrTerm 0 t)
>	             m ty' alpha
>	       unifyArgs doc ts beta
>         unifyArgs doc (t:ts) (TypeArrow ty1 ty2) =
>           tcConstrTermFP m tcEnv sigs p t >>=
>           unify p "function pattern" 
>	          (doc $-$ text "Term:" <+> ppConstrTerm 0 t)
>                 m ty1 >>
>           unifyArgs doc ts ty2
>         unifyArgs _ _ ty = internalError ("tcConstrTerm: " ++ show ty)
> tcConstrTerm m tcEnv sigs p t@(InfixFuncPattern t1 op t2) =
>   tcConstrTerm m tcEnv sigs p (FunctionPattern op [t1,t2])
> tcConstrTerm m tcEnv sigs p r@(RecordPattern fs rt)
>   | isJust rt =
>     do
>       ty <- tcConstrTerm m tcEnv sigs p (fromJust rt)
>       fts <- mapM (tcFieldPatt (tcConstrTerm m tcEnv sigs) m) fs
>       alpha <- freshVar id
>	let rty = TypeRecord fts (Just alpha)
>	unify p "record pattern" (ppConstrTerm 0 r) m ty rty
>       return rty
>   | otherwise =
>     do
>       fts <- mapM (tcFieldPatt (tcConstrTerm m tcEnv sigs) m) fs
>       return (TypeRecord fts Nothing)

\end{verbatim}
In contrast to usual patterns, the type checking routine for arguments of 
function patterns \texttt{tcConstrTermFP} differs from \texttt{tcConstrTerm}
because of possibly multiple occurrences of variables.
\begin{verbatim}

> tcConstrTermFP :: ModuleIdent -> TCEnv -> SigEnv -> Position -> ConstrTerm
>                   -> TcState Type
> tcConstrTermFP m tcEnv sigs p (LiteralPattern l) = tcLiteral m l
> tcConstrTermFP m tcEnv sigs p (NegativePattern _ l) = tcLiteral m l
> tcConstrTermFP m tcEnv sigs p (VariablePattern v) =
>   do
>     ty <- maybe freshTypeVar 
>                 (inst . expandPolyType m tcEnv) 
>                 (lookupTypeSig v sigs)
>     tyEnv <- S.get
>     ty' <- maybe (S.modify (bindFun m v (monoType ty)) >> return ty)
>                  (\ (ForAll _ t) -> return t)
>	           (sureVarType v tyEnv)
>     return ty' 
> tcConstrTermFP m tcEnv sigs p t@(ConstructorPattern c ts) =
>   do
>     tyEnv <- S.get
>     ty <- skol (constrType m c tyEnv)
>     unifyArgs (ppConstrTerm 0 t) ts ty
>   where unifyArgs _ [] ty = return ty
>         unifyArgs doc (t:ts) (TypeArrow ty1 ty2) =
>           tcConstrTermFP m tcEnv sigs p t >>=
>           unify p "pattern" (doc $-$ text "Term:" <+> ppConstrTerm 0 t)
>                 m ty1 >>
>           unifyArgs doc ts ty2
>         unifyArgs _ _ _ = internalError "tcConstrTermFP"
> tcConstrTermFP m tcEnv sigs p t@(InfixPattern t1 op t2) =
>   do
>     tyEnv <- S.get
>     ty <- skol (constrType m op tyEnv)
>     unifyArgs (ppConstrTerm 0 t) [t1,t2] ty
>   where unifyArgs _ [] ty = return ty
>         unifyArgs doc (t:ts) (TypeArrow ty1 ty2) =
>           tcConstrTermFP m tcEnv sigs p t >>=
>           unify p "pattern" (doc $-$ text "Term:" <+> ppConstrTerm 0 t)
>                 m ty1 >>
>           unifyArgs doc ts ty2
>         unifyArgs _ _ _ = internalError "tcConstrTermFP"
> tcConstrTermFP m tcEnv sigs p (ParenPattern t) = tcConstrTermFP m tcEnv sigs p t
> tcConstrTermFP m tcEnv sigs p (TuplePattern _ ts)
>  | null ts = return unitType
>  | otherwise = liftM tupleType $ mapM (tcConstrTermFP m tcEnv sigs p) ts
> tcConstrTermFP m tcEnv sigs p t@(ListPattern _ ts) =
>   freshTypeVar >>= flip (tcElems (ppConstrTerm 0 t)) ts
>   where tcElems _ ty [] = return (listType ty)
>         tcElems doc ty (t:ts) =
>           tcConstrTermFP m tcEnv sigs p t >>=
>           unify p "pattern" (doc $-$ text "Term:" <+> ppConstrTerm 0 t)
>                 m ty >>
>           tcElems doc ty ts
> tcConstrTermFP m tcEnv sigs p t@(AsPattern v t') =
>   do
>     ty1 <- tcConstrTermFP m tcEnv sigs p (VariablePattern v)
>     ty2 <- tcConstrTermFP m tcEnv sigs p t'
>     unify p "pattern" (ppConstrTerm 0 t) m ty1 ty2
>     return ty1
> tcConstrTermFP m tcEnv sigs p (LazyPattern _ t) = tcConstrTermFP m tcEnv sigs p t
> tcConstrTermFP m tcEnv sigs p t@(FunctionPattern f ts) =
>   do
>     tyEnv <- S.get
>     ty <- inst (funType m f tyEnv) --skol (constrType m c tyEnv)
>     unifyArgs (ppConstrTerm 0 t) ts ty
>   where unifyArgs _ [] ty = return ty
>         unifyArgs doc (t:ts) ty@(TypeVariable _) =
>           do (alpha,beta) <- tcArrow p "function pattern" doc m ty
>	       ty' <- tcConstrTermFP m tcEnv sigs p t
>	       unify p "function pattern"
>	             (doc $-$ text "Term:" <+> ppConstrTerm 0 t)
>	             m ty' alpha
>	       unifyArgs doc ts beta
>         unifyArgs doc (t:ts) (TypeArrow ty1 ty2) =
>           tcConstrTermFP m tcEnv sigs p t >>=
>           unify p "pattern" (doc $-$ text "Term:" <+> ppConstrTerm 0 t)
>                 m ty1 >>
>           unifyArgs doc ts ty2
>         unifyArgs _ _ _ = internalError "tcConstrTermFP"
> tcConstrTermFP m tcEnv sigs p t@(InfixFuncPattern t1 op t2) =
>   tcConstrTermFP m tcEnv sigs p (FunctionPattern op [t1,t2])
> tcConstrTermFP m tcEnv sigs p r@(RecordPattern fs rt)
>   | isJust rt =
>     do
>       ty <- tcConstrTermFP m tcEnv sigs p (fromJust rt)
>       fts <- mapM (tcFieldPatt (tcConstrTermFP m tcEnv sigs) m) fs
>       alpha <- freshVar id
>	let rty = TypeRecord fts (Just alpha)
>	unify p "record pattern" (ppConstrTerm 0 r) m ty rty
>       return rty
>   | otherwise =
>     do
>       fts <- mapM (tcFieldPatt (tcConstrTermFP m tcEnv sigs) m) fs
>       return (TypeRecord fts Nothing)

> tcFieldPatt :: (Position -> ConstrTerm -> TcState Type) -> ModuleIdent
>             -> Field ConstrTerm -> TcState (Ident,Type)
> tcFieldPatt tcPatt m f@(Field _ l t) =
>   do
>     tyEnv <- S.get
>     let p = positionOfIdent l
>     lty <- maybe (freshTypeVar
>	             >>= (\lty' ->
>		           S.modify
>		             (bindLabel l (qualifyWith m (mkIdent "#Rec"))
>		                        (polyType lty'))
>		           >> return lty'))
>	           (\ (ForAll _ lty') -> return lty')
>	           (sureLabelType l tyEnv)
>     ty <- tcPatt p t
>     unify p "record" (text "Field:" <+> ppFieldPatt f) m lty ty
>     return (l,ty)

> tcRhs :: ModuleIdent -> TCEnv -> ValueEnv -> SigEnv -> Rhs -> TcState Type
> tcRhs m tcEnv tyEnv0 sigs (SimpleRhs p e ds) =
>   do
>     tcDecls m tcEnv sigs ds
>     ty <- tcExpr m tcEnv sigs p e
>     checkSkolems p m (text "Expression:" <+> ppExpr 0 e) tyEnv0 ty
> tcRhs m tcEnv tyEnv0 sigs (GuardedRhs es ds) =
>   do
>     tcDecls m tcEnv sigs ds
>     tcCondExprs m tcEnv tyEnv0 sigs es

> tcCondExprs :: ModuleIdent -> TCEnv -> ValueEnv -> SigEnv -> [CondExpr]
>             -> TcState Type
> tcCondExprs m tcEnv tyEnv0 sigs es =
>   do
>     gty <- if length es > 1 then return boolType
>                             else freshConstrained [successType,boolType]
>     ty <- freshTypeVar
>     tcCondExprs' gty ty es
>   where tcCondExprs' gty ty [] = return ty
>         tcCondExprs' gty ty (e:es) =
>           tcCondExpr gty ty e >> tcCondExprs' gty ty es
>         tcCondExpr gty ty (CondExpr p g e) =
>           tcExpr m tcEnv sigs p g >>=
>           unify p "guard" (ppExpr 0 g) m gty >>
>           tcExpr m tcEnv sigs p e >>=
>           checkSkolems p m (text "Expression:" <+> ppExpr 0 e) tyEnv0 >>=
>           unify p "guarded expression" (ppExpr 0 e) m ty

> tcExpr :: ModuleIdent -> TCEnv -> SigEnv -> Position -> Expression
>        -> TcState Type
> tcExpr m _ _ _ (Literal l) = tcLiteral m l
> tcExpr m tcEnv sigs p (Variable v) =
>   case qualLookupTypeSig m v sigs of
>     Just ty -> inst (expandPolyType m tcEnv ty)
>     Nothing -> S.get >>= inst . funType m v
> tcExpr m tcEnv sigs p (Constructor c) = S.get >>= instExist . constrType m c
> tcExpr m tcEnv sigs p (Typed e sig) =
>   do
>     tyEnv0 <- S.get
>     ty <- tcExpr m tcEnv sigs p e
>     inst sigma' >>=
>       flip (unify p "explicitly typed expression" (ppExpr 0 e) m) ty
>     theta <- S.lift S.get
>     let sigma = gen (fvEnv (subst theta tyEnv0)) (subst theta ty)
>     unless (sigma == sigma')
>       (errorAt p (typeSigTooGeneral m (text "Expression:" <+> ppExpr 0 e)
>                  sig' sigma))
>     return ty
>   where sig' = nameSigType sig
>         sigma' = expandPolyType m tcEnv sig'
> tcExpr m tcEnv sigs p (Paren e) = tcExpr m tcEnv sigs p e
> tcExpr m tcEnv sigs p (Tuple _ es)
>   | null es = return unitType
>   | otherwise = liftM tupleType $ mapM (tcExpr m tcEnv sigs p) es
> tcExpr m tcEnv sigs p e@(List _ es) = freshTypeVar >>= tcElems (ppExpr 0 e) es
>   where tcElems _ [] ty = return (listType ty)
>         tcElems doc (e:es) ty =
>           tcExpr m tcEnv sigs p e >>=
>           unify p "expression" (doc $-$ text "Term:" <+> ppExpr 0 e)
>                 m ty >>
>           tcElems doc es ty
> tcExpr m tcEnv sigs p (ListCompr _ e qs) =
>   do
>     tyEnv0 <- S.get
>     mapM_ (tcQual m tcEnv sigs p) qs
>     ty <- tcExpr m tcEnv sigs p e
>     checkSkolems p m (text "Expression:" <+> ppExpr 0 e) tyEnv0 (listType ty)
> tcExpr m tcEnv sigs p e@(EnumFrom e1) =
>   do
>     ty1 <- tcExpr m tcEnv sigs p e1
>     unify p "arithmetic sequence"
>           (ppExpr 0 e $-$ text "Term:" <+> ppExpr 0 e1) m intType ty1
>     return (listType intType)
> tcExpr m tcEnv sigs p e@(EnumFromThen e1 e2) =
>   do
>     ty1 <- tcExpr m tcEnv sigs p e1
>     ty2 <- tcExpr m tcEnv sigs p e2
>     unify p "arithmetic sequence"
>           (ppExpr 0 e $-$ text "Term:" <+> ppExpr 0 e1) m intType ty1
>     unify p "arithmetic sequence"
>           (ppExpr 0 e $-$ text "Term:" <+> ppExpr 0 e2) m intType ty2
>     return (listType intType)
> tcExpr m tcEnv sigs p e@(EnumFromTo e1 e2) =
>   do
>     ty1 <- tcExpr m tcEnv sigs p e1
>     ty2 <- tcExpr m tcEnv sigs p e2
>     unify p "arithmetic sequence"
>           (ppExpr 0 e $-$ text "Term:" <+> ppExpr 0 e1) m intType ty1
>     unify p "arithmetic sequence"
>           (ppExpr 0 e $-$ text "Term:" <+> ppExpr 0 e2) m intType ty2
>     return (listType intType)
> tcExpr m tcEnv sigs p e@(EnumFromThenTo e1 e2 e3) =
>   do
>     ty1 <- tcExpr m tcEnv sigs p e1
>     ty2 <- tcExpr m tcEnv sigs p e2
>     ty3 <- tcExpr m tcEnv sigs p e3
>     unify p "arithmetic sequence"
>           (ppExpr 0 e $-$ text "Term:" <+> ppExpr 0 e1) m intType ty1
>     unify p "arithmetic sequence"
>           (ppExpr 0 e $-$ text "Term:" <+> ppExpr 0 e2) m intType ty2
>     unify p "arithmetic sequence"
>           (ppExpr 0 e $-$ text "Term:" <+> ppExpr 0 e3) m intType ty3
>     return (listType intType)
> tcExpr m tcEnv sigs p e@(UnaryMinus op e1) =
>   do
>     opTy <- opType op
>     ty1 <- tcExpr m tcEnv sigs p e1
>     unify p "unary negation" (ppExpr 0 e $-$ text "Term:" <+> ppExpr 0 e1)
>           m opTy ty1
>     return ty1
>   where opType op
>           | op == minusId = freshConstrained [intType,floatType]
>           | op == fminusId = return floatType
>           | otherwise = internalError ("tcExpr unary " ++ name op)
> tcExpr m tcEnv sigs p e@(Apply e1 e2) =
>   do
>     ty1 <- tcExpr m tcEnv sigs p e1
>     ty2 <- tcExpr m tcEnv sigs p e2
>     (alpha,beta) <-
>       tcArrow p "application" (ppExpr 0 e $-$ text "Term:" <+> ppExpr 0 e1)
>               m ty1
>     unify p "application" (ppExpr 0 e $-$ text "Term:" <+> ppExpr 0 e2)
>           m alpha ty2
>     return beta
> tcExpr m tcEnv sigs p e@(InfixApply e1 op e2) =
>   do
>     opTy <- tcExpr m tcEnv sigs p (infixOp op)
>     ty1 <- tcExpr m tcEnv sigs p e1
>     ty2 <- tcExpr m tcEnv sigs p e2
>     (alpha,beta,gamma) <-
>       tcBinary p "infix application"
>                (ppExpr 0 e $-$ text "Operator:" <+> ppOp op) m opTy
>     unify p "infix application" (ppExpr 0 e $-$ text "Term:" <+> ppExpr 0 e1)
>           m alpha ty1
>     unify p "infix application" (ppExpr 0 e $-$ text "Term:" <+> ppExpr 0 e2)
>           m beta ty2
>     return gamma
> tcExpr m tcEnv sigs p e@(LeftSection e1 op) =
>   do
>     opTy <- tcExpr m tcEnv sigs p (infixOp op)
>     ty1 <- tcExpr m tcEnv sigs p e1
>     (alpha,beta) <-
>       tcArrow p "left section" (ppExpr 0 e $-$ text "Operator:" <+> ppOp op)
>               m opTy
>     unify p "left section" (ppExpr 0 e $-$ text "Term:" <+> ppExpr 0 e1)
>           m alpha ty1
>     return beta
> tcExpr m tcEnv sigs p e@(RightSection op e1) =
>   do
>     opTy <- tcExpr m tcEnv sigs p (infixOp op)
>     ty1 <- tcExpr m tcEnv sigs p e1
>     (alpha,beta,gamma) <-
>       tcBinary p "right section"
>                (ppExpr 0 e $-$ text "Operator:" <+> ppOp op) m opTy
>     unify p "right section" (ppExpr 0 e $-$ text "Term:" <+> ppExpr 0 e1)
>           m beta ty1
>     return (TypeArrow alpha gamma)
> tcExpr m tcEnv sigs p exp@(Lambda r ts e) =
>   do
>     tyEnv0 <- S.get
>     tys <- mapM (tcConstrTerm m tcEnv sigs p) ts
>     ty <- tcExpr m tcEnv sigs p e
>     checkSkolems p m (text "Expression:" <+> ppExpr 0 exp) tyEnv0
>                  (foldr TypeArrow ty tys)
> tcExpr m tcEnv sigs p (Let ds e) =
>   do
>     tyEnv0 <- S.get
>     theta <- S.lift S.get
>     tcDecls m tcEnv sigs ds
>     ty <- tcExpr m tcEnv sigs p e
>     checkSkolems p m (text "Expression:" <+> ppExpr 0 e) tyEnv0 ty
> tcExpr m tcEnv sigs p (Do sts e) =
>   do
>     tyEnv0 <- S.get
>     mapM_ (tcStmt m tcEnv sigs p) sts
>     alpha <- freshTypeVar
>     ty <- tcExpr m tcEnv sigs p e
>     unify p "statement" (ppExpr 0 e) m (ioType alpha) ty
>     checkSkolems p m (text "Expression:" <+> ppExpr 0 e) tyEnv0 ty
> tcExpr m tcEnv sigs p e@(IfThenElse _ e1 e2 e3) =
>   do
>     ty1 <- tcExpr m tcEnv sigs p e1
>     unify p "expression" (ppExpr 0 e $-$ text "Term:" <+> ppExpr 0 e1)
>           m boolType ty1
>     ty2 <- tcExpr m tcEnv sigs p e2
>     ty3 <- tcExpr m tcEnv sigs p e3
>     unify p "expression" (ppExpr 0 e $-$ text "Term:" <+> ppExpr 0 e3)
>           m ty2 ty3
>     return ty3
> tcExpr m tcEnv sigs p (Case _ e alts) =
>   do
>     tyEnv0 <- S.get
>     ty <- tcExpr m tcEnv sigs p e
>     alpha <- freshTypeVar
>     tcAlts tyEnv0 ty alpha alts
>   where tcAlts tyEnv0 _ ty [] = return ty
>         tcAlts tyEnv0 ty1 ty2 (alt:alts) =
>           tcAlt (ppAlt alt) tyEnv0 ty1 ty2 alt >> tcAlts tyEnv0 ty1 ty2 alts
>         tcAlt doc tyEnv0 ty1 ty2 (Alt p t rhs) =
>           tcConstrTerm m tcEnv sigs p t >>=
>           unify p "case pattern" (doc $-$ text "Term:" <+> ppConstrTerm 0 t)
>                 m ty1 >>
>           tcRhs m tcEnv tyEnv0 sigs rhs >>=
>           unify p "case branch" doc m ty2
> tcExpr m tcEnv sigs p (RecordConstr fs) =
>   do 
>     fts <- mapM (tcFieldExpr m tcEnv sigs equals) fs
>     --when (1 == length fs)
>     --     (error (show fs ++ "\n" ++ show fts))
>     return (TypeRecord fts Nothing)
> tcExpr m tcEnv sigs p r@(RecordSelection e l) =
>   do
>     ty <- tcExpr m tcEnv sigs p e
>     tyEnv <- S.get
>     lty <- maybe (freshTypeVar 
>	             >>= (\lty' -> 
>		           S.modify 
>		             (bindLabel l (qualifyWith m (mkIdent "#Rec"))
>		                        (monoType lty'))
>	                   >> return lty'))
>                  (\ (ForAll _ lty') -> return lty')
>	           (sureLabelType l tyEnv)
>     alpha <- freshVar id
>     let rty = TypeRecord [(l,lty)] (Just alpha)
>     unify p "record selection" (ppExpr 0 r) m ty rty
>     return lty
> tcExpr m tcEnv sigs p r@(RecordUpdate fs e) =
>   do
>     ty <- tcExpr m tcEnv sigs p e
>     fts <- mapM (tcFieldExpr m tcEnv sigs (text ":=")) fs
>     alpha <- freshVar id
>     let rty = TypeRecord fts (Just alpha)
>     unify p "record update" (ppExpr 0 r) m ty rty
>     return ty

> tcQual :: ModuleIdent -> TCEnv -> SigEnv -> Position -> Statement
>        -> TcState ()
> tcQual m tcEnv sigs p (StmtExpr _ e) =
>   do
>     ty <- tcExpr m tcEnv sigs p e
>     unify p "guard" (ppExpr 0 e) m boolType ty
> tcQual m tcEnv sigs p q@(StmtBind _ t e) =
>   do
>     ty1 <- tcConstrTerm m tcEnv sigs p t
>     ty2 <- tcExpr m tcEnv sigs p e
>     unify p "generator" (ppStmt q $-$ text "Term:" <+> ppExpr 0 e)
>           m (listType ty1) ty2
> tcQual m tcEnv sigs p (StmtDecl ds) = tcDecls m tcEnv sigs ds

> tcStmt :: ModuleIdent -> TCEnv -> SigEnv -> Position -> Statement
>        -> TcState ()
> tcStmt m tcEnv sigs p (StmtExpr _ e) =
>   do
>     alpha <- freshTypeVar
>     ty <- tcExpr m tcEnv sigs p e
>     unify p "statement" (ppExpr 0 e) m (ioType alpha) ty
> tcStmt m tcEnv sigs p st@(StmtBind _ t e) =
>   do
>     ty1 <- tcConstrTerm m tcEnv sigs p t
>     ty2 <- tcExpr m tcEnv sigs p e
>     unify p "statement" (ppStmt st $-$ text "Term:" <+> ppExpr 0 e)
>           m (ioType ty1) ty2
> tcStmt m tcEnv sigs p (StmtDecl ds) = tcDecls m tcEnv sigs ds

> tcFieldExpr :: ModuleIdent -> TCEnv -> SigEnv -> Doc -> Field Expression
>	      -> TcState (Ident,Type)
> tcFieldExpr m tcEnv sigs comb f@(Field _ l e) =
>   do
>     tyEnv <- S.get
>     let p = positionOfIdent l
>     lty <- maybe (freshTypeVar 
>	             >>= (\lty' -> 
>		           S.modify 
>		             (bindLabel l (qualifyWith m (mkIdent "#Rec"))
>		                          (monoType lty'))
>	                   >> return lty'))
>                  inst
>	           (sureLabelType l tyEnv)
>     ty <- tcExpr m tcEnv sigs p e
>     unify p "record" (text "Field:" <+> ppFieldExpr comb f) m lty ty
>     return (l,ty)

\end{verbatim}
The function \texttt{tcArrow} checks that its argument can be used as
an arrow type $\alpha\rightarrow\beta$ and returns the pair
$(\alpha,\beta)$. Similarly, the function \texttt{tcBinary} checks
that its argument can be used as an arrow type
$\alpha\rightarrow\beta\rightarrow\gamma$ and returns the triple
$(\alpha,\beta,\gamma)$.
\begin{verbatim}

> tcArrow :: Position -> String -> Doc -> ModuleIdent -> Type
>         -> TcState (Type,Type)
> tcArrow p what doc m ty =
>   do
>     theta <- S.lift S.get
>     unaryArrow (subst theta ty)
>   where unaryArrow (TypeArrow ty1 ty2) = return (ty1,ty2)
>         unaryArrow (TypeVariable tv) =
>           do
>             alpha <- freshTypeVar
>             beta <- freshTypeVar
>             S.lift (S.modify (bindVar tv (TypeArrow alpha beta)))
>             return (alpha,beta)
>         unaryArrow ty = errorAt p (nonFunctionType what doc m ty)

> tcBinary :: Position -> String -> Doc -> ModuleIdent -> Type
>          -> TcState (Type,Type,Type)
> tcBinary p what doc m ty = tcArrow p what doc m ty >>= uncurry binaryArrow
>   where binaryArrow ty1 (TypeArrow ty2 ty3) = return (ty1,ty2,ty3)
>         binaryArrow ty1 (TypeVariable tv) =
>           do
>             beta <- freshTypeVar
>             gamma <- freshTypeVar
>             S.lift (S.modify (bindVar tv (TypeArrow beta gamma)))
>             return (ty1,beta,gamma)
>         binaryArrow ty1 ty2 =
>           errorAt p (nonBinaryOp what doc m (TypeArrow ty1 ty2))

\end{verbatim}
\paragraph{Unification}
The unification uses Robinson's algorithm (cf., e.g., Chap.~9
of~\cite{PeytonJones87:Book}).
\begin{verbatim}

> unify :: Position -> String -> Doc -> ModuleIdent -> Type -> Type
>       -> TcState ()
> unify p what doc m ty1 ty2 =
>   S.lift $
>   do
>     theta <- S.get
>     let ty1' = subst theta ty1
>     let ty2' = subst theta ty2
>     either (errorAt p . typeMismatch what doc m ty1' ty2')
>            (S.modify . compose)
>            (unifyTypes m ty1' ty2')

> unifyTypes :: ModuleIdent -> Type -> Type -> Either Doc TypeSubst
> unifyTypes _ (TypeVariable tv1) (TypeVariable tv2)
>   | tv1 == tv2 = Right idSubst
>   | otherwise = Right (bindSubst tv1 (TypeVariable tv2) idSubst)
> unifyTypes m (TypeVariable tv) ty
>   | tv `elem` typeVars ty = Left (recursiveType m tv ty)
>   | otherwise = Right (bindSubst tv ty idSubst)
> unifyTypes m ty (TypeVariable tv)
>   | tv `elem` typeVars ty = Left (recursiveType m tv ty)
>   | otherwise = Right (bindSubst tv ty idSubst)
> unifyTypes _ (TypeConstrained tys1 tv1) (TypeConstrained tys2 tv2)
>   | tv1 == tv2 = Right idSubst
>   | tys1 == tys2 = Right (bindSubst tv1 (TypeConstrained tys2 tv2) idSubst)
> unifyTypes m (TypeConstrained tys tv) ty =
>   foldr (choose . unifyTypes m ty) (Left (incompatibleTypes m ty (head tys)))
>         tys
>   where choose (Left _) theta' = theta'
>         choose (Right theta) _ = Right (bindSubst tv ty theta)
> unifyTypes m ty (TypeConstrained tys tv) =
>   foldr (choose . unifyTypes m ty) (Left (incompatibleTypes m ty (head tys)))
>         tys
>   where choose (Left _) theta' = theta'
>         choose (Right theta) _ = Right (bindSubst tv ty theta)
> unifyTypes m (TypeConstructor tc1 tys1) (TypeConstructor tc2 tys2)
>   | tc1 == tc2 = unifyTypeLists m tys1 tys2
> unifyTypes m (TypeArrow ty11 ty12) (TypeArrow ty21 ty22) =
>   unifyTypeLists m [ty11,ty12] [ty21,ty22]
> unifyTypes _ (TypeSkolem k1) (TypeSkolem k2)
>   | k1 == k2 = Right idSubst
> unifyTypes m (TypeRecord fs1 Nothing) tr2@(TypeRecord fs2 Nothing)
>   | length fs1 == length fs2 = unifyTypedLabels m fs1 tr2
> unifyTypes m tr1@(TypeRecord fs1 Nothing) tr2@(TypeRecord fs2 (Just a2)) =
>   either Left
>          (\res -> either Left 
>	                   (Right . compose res) 
>                          (unifyTypes m (TypeVariable a2) tr1))
>          (unifyTypedLabels m fs2 tr1)
> unifyTypes m tr1@(TypeRecord _ (Just _)) tr2@(TypeRecord _ Nothing) =
>   unifyTypes m tr2 tr1
> unifyTypes m (TypeRecord fs1 (Just a1)) tr2@(TypeRecord fs2 (Just a2)) =
>   let (fs1', rs1, rs2) = splitFields fs1 fs2
>   in  either 
>         Left
>         (\res -> 
>           either 
>             Left 
>	      (\res' -> Right (compose res res'))
>	      (unifyTypeLists m [TypeVariable a1,
>			         TypeRecord (fs1 ++ rs2) Nothing]
>	                        [TypeVariable a2,
>			         TypeRecord (fs2 ++ rs1) Nothing]))
>         (unifyTypedLabels m fs1' tr2)
>   where
>   splitFields fs1 fs2 = split' [] [] fs2 fs1
>   split' fs1' rs1 rs2 [] = (fs1',rs1,rs2)
>   split' fs1' rs1 rs2 ((l,ty):fs1) =
>     maybe (split' fs1' ((l,ty):rs1) rs2 fs1)
>           (const (split' ((l,ty):fs1') rs1 (remove l rs2) fs1))
>           (lookup l rs2)
> unifyTypes m ty1 ty2 = Left (incompatibleTypes m ty1 ty2)

> unifyTypeLists :: ModuleIdent -> [Type] -> [Type] -> Either Doc TypeSubst
> unifyTypeLists _ [] _ = Right idSubst
> unifyTypeLists _ _ [] = Right idSubst
> unifyTypeLists m (ty1:tys1) (ty2:tys2) =
>   either Left (unifyTypesTheta m ty1 ty2) (unifyTypeLists m tys1 tys2)
>   where unifyTypesTheta m ty1 ty2 theta =
>           either Left (Right . flip compose theta)
>                  (unifyTypes m (subst theta ty1) (subst theta ty2))

> unifyTypedLabels :: ModuleIdent -> [(Ident,Type)] -> Type 
>	           -> Either Doc TypeSubst
> unifyTypedLabels m [] (TypeRecord _ _) = Right idSubst
> unifyTypedLabels m ((l,ty):fs1) tr@(TypeRecord fs2 _) =
>   either Left
>          (\r -> 
>            maybe (Left (missingLabel m l tr))
>                  (\ty' -> 
>		     either (const (Left (incompatibleLabelTypes m l ty ty')))
>	                    (Right . flip compose r)
>	                    (unifyTypes m ty ty'))
>                  (lookup l fs2))
>          (unifyTypedLabels m fs1 tr)
> unifyTypedLabels _ _ _ = internalError "unifyTypedLabels"

\end{verbatim}
For each declaration group, the type checker has to ensure that no
skolem type escapes its scope.
\begin{verbatim}

> checkSkolems :: Position -> ModuleIdent -> Doc -> ValueEnv -> Type
>              -> TcState Type
> checkSkolems p m what tyEnv ty =
>   do
>     theta <- S.lift S.get
>     let ty' = subst theta ty
>         fs = fsEnv (subst theta tyEnv)
>     unless (all (`Set.member` fs) (typeSkolems ty'))
>            (errorAt p (skolemEscapingScope m what ty'))
>     --error (show ty ++ " ## " ++ show (subst theta ty))
>     return ty'

\end{verbatim}
\paragraph{Instantiation and Generalization}
We use negative offsets for fresh type variables.
\begin{verbatim}

> fresh :: (Int -> a) -> TcState a
> fresh f = liftM f (S.lift (S.lift (S.modify succ >> S.get)))

> freshVar :: (Int -> a) -> TcState a
> freshVar f = fresh (\n -> f (- n - 1))

> freshTypeVar :: TcState Type
> freshTypeVar = freshVar TypeVariable

> freshConstrained :: [Type] -> TcState Type
> freshConstrained tys = freshVar (TypeConstrained tys)

> freshSkolem :: TcState Type
> freshSkolem = fresh TypeSkolem

> inst :: TypeScheme -> TcState Type
> inst (ForAll n ty) =
>   do
>     tys <- replicateM n freshTypeVar
>     return (expandAliasType tys ty)

> instExist :: ExistTypeScheme -> TcState Type
> instExist (ForAllExist n n' ty) =
>   do
>     tys <- replicateM (n + n') freshTypeVar
>     return (expandAliasType tys ty)

> skol :: ExistTypeScheme -> TcState Type
> skol (ForAllExist n n' ty) =
>   do
>     tys <- replicateM n freshTypeVar
>     tys' <- replicateM n' freshSkolem
>     return (expandAliasType (tys ++ tys') ty)

> gen :: Set.Set Int -> Type -> TypeScheme
> gen gvs ty =
>   ForAll (length tvs) (subst (foldr2 bindSubst idSubst tvs tvs') ty)
>   where tvs = [tv | tv <- nub (typeVars ty), tv `Set.notMember` gvs]
>         tvs' = map TypeVariable [0..]

\end{verbatim}
\paragraph{Auxiliary Functions}
The functions \texttt{constrType}, \texttt{varType}, and
\texttt{funType} are used to retrieve the type of constructors,
pattern variables, and variables in expressions, respectively, from
the type environment. Because the syntactical correctness has already
been verified by the syntax checker, none of these functions should
fail.

Note that \texttt{varType} can handle ambiguous identifiers and
returns the first available type. This function is used for looking up
the type of an identifier on the left hand side of a rule where it
unambiguously refers to the local definition.
\begin{verbatim}

> constrType :: ModuleIdent -> QualIdent -> ValueEnv -> ExistTypeScheme
> constrType m c tyEnv =
>   case qualLookupValue c tyEnv of
>     [DataConstructor _ sigma] -> sigma
>     [NewtypeConstructor _ sigma] -> sigma
>     _ -> case (qualLookupValue (qualQualify m c) tyEnv) of
>            [DataConstructor _ sigma] -> sigma
>            [NewtypeConstructor _ sigma] -> sigma
>            _ -> internalError ("constrType " ++ show c)

> varType :: Ident -> ValueEnv -> TypeScheme
> varType v tyEnv =
>   case lookupValue v tyEnv of
>     Value _ sigma : _ -> sigma
>     _ -> internalError ("varType " ++ show v)

> sureVarType :: Ident -> ValueEnv -> Maybe TypeScheme
> sureVarType v tyEnv =
>   case lookupValue v tyEnv of
>     Value _ sigma : _ -> Just sigma
>     _ -> Nothing

> funType :: ModuleIdent -> QualIdent -> ValueEnv -> TypeScheme
> funType m f tyEnv =
>   case (qualLookupValue f tyEnv) of
>     [Value _ sigma] -> sigma
>     vs -> case (qualLookupValue (qualQualify m f) tyEnv) of
>             [Value _ sigma] -> sigma
>             _ -> internalError ("funType " ++ show f)

> sureLabelType :: Ident -> ValueEnv -> Maybe TypeScheme
> sureLabelType l tyEnv =
>   case lookupValue l tyEnv of
>     Label _ _ sigma : _ -> Just sigma
>     _ -> Nothing


\end{verbatim}
The function \texttt{expandType} expands all type synonyms in a type
and also qualifies all type constructors with the name of the module
in which the type was defined.
\begin{verbatim}

> expandMonoType :: ModuleIdent -> TCEnv -> [Ident] -> TypeExpr -> Type
> expandMonoType m tcEnv tvs ty = expandType m tcEnv (toType tvs ty)

> expandMonoTypes :: ModuleIdent -> TCEnv -> [Ident] -> [TypeExpr] -> [Type]
> expandMonoTypes m tcEnv tvs tys = map (expandType m tcEnv) (toTypes tvs tys)

> expandPolyType :: ModuleIdent -> TCEnv -> TypeExpr -> TypeScheme
> expandPolyType m tcEnv ty = 
>     polyType $ normalize $ expandMonoType m tcEnv [] ty

> expandType :: ModuleIdent -> TCEnv -> Type -> Type
> expandType m tcEnv (TypeConstructor tc tys) =
>   case qualLookupTC tc tcEnv of
>     [DataType tc' _ _] -> TypeConstructor tc' tys'
>     [RenamingType tc' _ _] -> TypeConstructor tc' tys'
>     [AliasType _ _ ty] -> expandAliasType tys' ty
>     _ -> case (qualLookupTC (qualQualify m tc) tcEnv) of
>            [DataType tc' _ _] -> TypeConstructor tc' tys'
>            [RenamingType tc' _ _] -> TypeConstructor tc' tys'
>            [AliasType _ _ ty] -> expandAliasType tys' ty
>            _ -> internalError ("expandType " ++ show tc)
>   where tys' = map (expandType m tcEnv) tys
> expandType _ _ (TypeVariable tv) = TypeVariable tv
> expandType _ _ (TypeConstrained tys tv) = TypeConstrained tys tv
> expandType m tcEnv (TypeArrow ty1 ty2) =
>   TypeArrow (expandType m tcEnv ty1) (expandType m tcEnv ty2)
> expandType _ tcEnv (TypeSkolem k) = TypeSkolem k
> expandType m tcEnv (TypeRecord fs rv) =
>   TypeRecord (map (\ (l,ty) -> (l, expandType m tcEnv ty)) fs) rv

\end{verbatim}
The functions \texttt{fvEnv} and \texttt{fsEnv} compute the set of
free type variables and free skolems of a type environment,
respectively. We ignore the types of data constructors here because we
know that they are closed.
\begin{verbatim}

> fvEnv :: ValueEnv -> Set.Set Int
> fvEnv tyEnv =
>   Set.fromList [tv | ty <- localTypes tyEnv, tv <- typeVars ty, tv < 0]

> fsEnv :: ValueEnv -> Set.Set Int
> fsEnv tyEnv = Set.unions (map (Set.fromList . typeSkolems) (localTypes tyEnv))

> localTypes :: ValueEnv -> [Type]
> localTypes tyEnv = [ty | (_,Value _ (ForAll _ ty)) <- localBindings tyEnv]

\end{verbatim}
Miscellaneous functions.
\begin{verbatim}

> remove :: Eq a => a -> [(a,b)] -> [(a,b)]
> remove _ [] = []
> remove k ((k',e):kes) | k == k'   = kes
>		        | otherwise = (k',e):(remove k kes) 

\end{verbatim}
Error functions.
\begin{verbatim}

> recursiveTypes :: [Ident] -> (Position,String)
> recursiveTypes [tc] = 
>     (positionOfIdent tc,
>      "Recursive synonym type " ++ name tc)
> recursiveTypes (tc:tcs) =
>  (positionOfIdent tc,
>   "Recursive synonym types " ++ name tc ++ types "" tcs)
>   where types comma [tc] = comma ++ " and " ++ name tc ++
>                            showLine (positionOfIdent tc) 
>         types _ (tc:tcs) = ", " ++ name tc ++ 
>                            showLine (positionOfIdent tc) ++ 
>                            types "," tcs

> polymorphicFreeVar :: Ident -> (Position,String)
> polymorphicFreeVar v =
>  (positionOfIdent v,
>   "Free variable " ++ name v ++ " has a polymorphic type")

> typeSigTooGeneral :: ModuleIdent -> Doc -> TypeExpr -> TypeScheme -> String
> typeSigTooGeneral m what ty sigma = show $
>   vcat [text "Type signature too general", what,
>         text "Inferred type:" <+> ppTypeScheme m sigma,
>         text "Type signature:" <+> ppTypeExpr 0 ty]

> nonFunctionType :: String -> Doc -> ModuleIdent -> Type -> String
> nonFunctionType what doc m ty = show $
>   vcat [text "Type error in" <+> text what, doc,
>         text "Type:" <+> ppType m ty,
>         text "Cannot be applied"]

> nonBinaryOp :: String -> Doc -> ModuleIdent -> Type -> String
> nonBinaryOp what doc m ty = show $
>   vcat [text "Type error in" <+> text what, doc,
>         text "Type:" <+> ppType m ty,
>         text "Cannot be used as binary operator"]

> typeMismatch :: String -> Doc -> ModuleIdent -> Type -> Type -> Doc -> String
> typeMismatch what doc m ty1 ty2 reason = show $
>   vcat [text "Type error in" <+> text what, doc,
>         text "Inferred type:" <+> ppType m ty2,
>         text "Expected type:" <+> ppType m ty1,
>         reason]

> skolemEscapingScope :: ModuleIdent -> Doc -> Type -> String
> skolemEscapingScope m what ty = show $
>   vcat [text "Existential type escapes out of its scope", what,
>         text "Type:" <+> ppType m ty]

> recursiveType :: ModuleIdent -> Int -> Type -> Doc
> recursiveType m tv ty = incompatibleTypes m (TypeVariable tv) ty

> missingLabel :: ModuleIdent -> Ident -> Type -> Doc
> missingLabel m l rty =
>   sep [text "Missing field for label" <+> ppIdent l,
>        text "in the record type" <+> ppType m rty]

> incompatibleTypes :: ModuleIdent -> Type -> Type -> Doc
> incompatibleTypes m ty1 ty2 =
>   sep [text "Types" <+> ppType m ty1,
>        nest 2 (text "and" <+> ppType m ty2),
>        text "are incompatible"]

> incompatibleLabelTypes :: ModuleIdent -> Ident -> Type -> Type -> Doc
> incompatibleLabelTypes m l ty1 ty2 =
>   sep [text "Labeled types" <+> ppIdent l <> text "::" <> ppType m ty1,
>        nest 10 (text "and" <+> ppIdent l <> text "::" <> ppType m ty2),
>        text "are incompatible"]

\end{verbatim}


\end{verbatim}
The following functions implement pretty-printing for types.
\begin{verbatim}

> ppType :: ModuleIdent -> Type -> Doc
> ppType m = ppTypeExpr 0 . fromQualType m

> ppTypeScheme :: ModuleIdent -> TypeScheme -> Doc
> ppTypeScheme m (ForAll _ ty) = ppType m ty
