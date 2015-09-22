
% $Id: KindCheck.lhs,v 1.33 2004/02/13 19:24:04 wlux Exp $
%
% Copyright (c) 1999-2004, Wolfgang Lux
% See LICENSE for the full license.
%
% Modified by Martin Engelke (men@informatik.uni-kiel.de)
%
\nwfilename{KindCheck.lhs}
\section{Checking Type Definitions}
After the source file has been parsed and all modules have been
imported, the compiler first performs kind checking on all type
definitions and signatures. Because Curry currently does not support
type classes, kind checking is rather trivial. All types must be of
first order kind ($\star$), i.e., all type constructor applications
must be saturated.

During kind checking, this module will also disambiguate nullary
constructors and type variables which -- in contrast to Haskell -- is
not possible on purely syntactic criteria. In addition it is checked
that all type constructors and type variables occurring on the right
hand side of a type declaration are actually defined and no identifier
is defined more than once.
\begin{verbatim}

> module KindCheck(kindCheck) where

> import Data.Maybe

> import Curry.Syntax
> import Curry.Syntax.Utils(isTypeDecl)
> import Curry.Base.Position
> import Curry.Base.Ident
> import Base hiding (bindArity)
> import TopEnv

\end{verbatim}
In order to check type constructor applications, the compiler
maintains an environment containing the kind information for all type
constructors. The function \texttt{kindCheck} first initializes this
environment by filtering out the arity of each type constructor from
the imported type environment. Next, the arities of all locally
defined type constructors are inserted into the environment, and,
finally, the declarations are checked within this environment.
\begin{verbatim}

> kindCheck :: ModuleIdent -> TCEnv -> [Decl] -> [Decl]
> kindCheck m tcEnv ds =
>   case findDouble (map tconstr ds') of
>     Nothing -> map (checkDecl m kEnv) ds
>     Just tc -> errorAt' (duplicateType tc)
>   where ds' = filter isTypeDecl ds
>         kEnv = foldr (bindArity m) (fmap tcArity tcEnv) ds'

\end{verbatim}
The kind environment only needs to record the arity of each type constructor.
\begin{verbatim}

> type KindEnv = TopEnv Int

> bindArity :: ModuleIdent -> Decl -> KindEnv -> KindEnv
> bindArity m (DataDecl _ tc tvs _) = bindArity' m  tc tvs
> bindArity m (NewtypeDecl _ tc tvs _) = bindArity' m  tc tvs
> bindArity m (TypeDecl _ tc tvs _) = bindArity' m  tc tvs
> bindArity _ _ = id

> bindArity' :: ModuleIdent -> Ident -> [Ident]
>            -> KindEnv -> KindEnv
> bindArity' m tc tvs 
>   = bindTopEnv "KindCheck.bindArity'" tc n 
>                . qualBindTopEnv "KindCheck.bindArity'" (qualifyWith m tc) n
>   where n = length tvs

> lookupKind :: Ident -> KindEnv -> [Int]
> lookupKind = lookupTopEnv

> qualLookupKind :: QualIdent -> KindEnv -> [Int]
> qualLookupKind = qualLookupTopEnv

\end{verbatim}
When type declarations are checked, the compiler will allow anonymous
type variables on the left hand side of the declaration, but not on
the right hand side. Function and pattern declarations must be
traversed because they can contain local type signatures.
\begin{verbatim}

> checkDecl :: ModuleIdent -> KindEnv -> Decl -> Decl
> checkDecl m kEnv (DataDecl p tc tvs cs) =
>   DataDecl p tc tvs' (map (checkConstrDecl m kEnv tvs') cs)
>   where tvs' = checkTypeLhs kEnv tvs
> checkDecl m kEnv (NewtypeDecl p tc tvs nc) =
>   NewtypeDecl p tc tvs' (checkNewConstrDecl m kEnv tvs' nc)
>   where tvs' = checkTypeLhs kEnv tvs
> checkDecl m kEnv (TypeDecl p tc tvs ty) =
>   TypeDecl p tc tvs' (checkClosedType m kEnv tvs' ty)
>   where tvs' = checkTypeLhs kEnv tvs
> checkDecl m kEnv (TypeSig p vs ty) =
>   TypeSig p vs (checkType m kEnv ty)
> checkDecl m kEnv (FunctionDecl p f eqs) =
>   FunctionDecl p f (map (checkEquation m kEnv) eqs)
> checkDecl m kEnv (PatternDecl p t rhs) =
>   PatternDecl p t (checkRhs m kEnv rhs)
> checkDecl m kEnv (ExternalDecl p cc ie f ty) =
>   ExternalDecl p cc ie f (checkType m kEnv ty)
> checkDecl _ _ d = d

> checkTypeLhs :: KindEnv -> [Ident] -> [Ident]
> checkTypeLhs kEnv (tv:tvs)
>   | tv == anonId = tv : checkTypeLhs kEnv tvs
>   | isTypeConstr tv = errorAt' (noVariable tv)
>   | tv `elem` tvs = errorAt' (nonLinear tv)
>   | otherwise = tv : checkTypeLhs kEnv tvs
>   where isTypeConstr tv = not (null (lookupKind tv kEnv))
> checkTypeLhs _ [] = []

> checkConstrDecl :: ModuleIdent -> KindEnv -> [Ident] -> ConstrDecl -> ConstrDecl
> checkConstrDecl m kEnv tvs (ConstrDecl p evs c tys) =
>   ConstrDecl p evs' c (map (checkClosedType m kEnv tvs') tys)
>   where evs' = checkTypeLhs kEnv evs
>         tvs' = evs' ++ tvs
> checkConstrDecl m kEnv tvs (ConOpDecl p evs ty1 op ty2) =
>   ConOpDecl p evs' (checkClosedType m kEnv tvs' ty1) op
>             (checkClosedType m kEnv tvs' ty2)
>   where evs' = checkTypeLhs kEnv evs
>         tvs' = evs' ++ tvs

> checkNewConstrDecl :: ModuleIdent -> KindEnv -> [Ident] -> NewConstrDecl 
>	     -> NewConstrDecl
> checkNewConstrDecl m kEnv tvs (NewConstrDecl p evs c ty) =
>   NewConstrDecl p evs' c (checkClosedType m kEnv tvs' ty)
>   where evs' = checkTypeLhs kEnv evs
>         tvs' = evs' ++ tvs

\end{verbatim}
Checking expressions is rather straight forward. The compiler must
only traverse the structure of expressions in order to find local
declaration groups.
\begin{verbatim}

> checkEquation :: ModuleIdent -> KindEnv -> Equation -> Equation
> checkEquation m kEnv (Equation p lhs rhs) = 
>     Equation p lhs (checkRhs m kEnv rhs)

> checkRhs :: ModuleIdent -> KindEnv -> Rhs -> Rhs
> checkRhs m kEnv (SimpleRhs p e ds) =
>   SimpleRhs p (checkExpr m kEnv e) (map (checkDecl m kEnv) ds)
> checkRhs m kEnv (GuardedRhs es ds) =
>   GuardedRhs (map (checkCondExpr m kEnv) es) (map (checkDecl m kEnv) ds)

> checkCondExpr :: ModuleIdent -> KindEnv -> CondExpr -> CondExpr
> checkCondExpr m kEnv (CondExpr p g e) =
>   CondExpr p (checkExpr m kEnv g) (checkExpr m kEnv e)

> checkExpr :: ModuleIdent -> KindEnv -> Expression -> Expression
> checkExpr _ _ (Literal l) = Literal l
> checkExpr _ _ (Variable v) = Variable v
> checkExpr _ _ (Constructor c) = Constructor c
> checkExpr m kEnv (Paren e) = Paren (checkExpr m kEnv e)
> checkExpr m kEnv (Typed e ty) =
>   Typed (checkExpr m kEnv e) (checkType m kEnv ty)
> checkExpr m kEnv (Tuple p es) = Tuple p (map (checkExpr m kEnv ) es)
> checkExpr m kEnv (List p es) = List p (map (checkExpr m kEnv ) es)
> checkExpr m kEnv (ListCompr p e qs) =
>   ListCompr p (checkExpr m kEnv e) (map (checkStmt m kEnv ) qs)
> checkExpr m kEnv  (EnumFrom e) = EnumFrom (checkExpr m kEnv  e)
> checkExpr m kEnv  (EnumFromThen e1 e2) =
>   EnumFromThen (checkExpr m kEnv  e1) (checkExpr m kEnv  e2)
> checkExpr m kEnv  (EnumFromTo e1 e2) =
>   EnumFromTo (checkExpr m kEnv  e1) (checkExpr m kEnv  e2)
> checkExpr m kEnv  (EnumFromThenTo e1 e2 e3) =
>   EnumFromThenTo (checkExpr m kEnv  e1) (checkExpr m kEnv  e2)
>                  (checkExpr m kEnv  e3)
> checkExpr m kEnv  (UnaryMinus op e) = UnaryMinus op (checkExpr m kEnv  e)
> checkExpr m kEnv  (Apply e1 e2) =
>   Apply (checkExpr m kEnv  e1) (checkExpr m kEnv  e2)
> checkExpr m kEnv  (InfixApply e1 op e2) =
>   InfixApply (checkExpr m kEnv  e1) op (checkExpr m kEnv  e2)
> checkExpr m kEnv  (LeftSection e op) = LeftSection (checkExpr m kEnv  e) op
> checkExpr m kEnv  (RightSection op e) = RightSection op (checkExpr m kEnv  e)
> checkExpr m kEnv  (Lambda r ts e) = Lambda r ts (checkExpr m kEnv  e)
> checkExpr m kEnv  (Let ds e) =
>   Let (map (checkDecl m kEnv) ds) (checkExpr m kEnv  e)
> checkExpr m kEnv  (Do sts e) =
>   Do (map (checkStmt m kEnv ) sts) (checkExpr m kEnv  e)
> checkExpr m kEnv  (IfThenElse r e1 e2 e3) =
>   IfThenElse r (checkExpr m kEnv  e1) (checkExpr m kEnv  e2)
>              (checkExpr m kEnv  e3)
> checkExpr m kEnv  (Case r e alts) =
>   Case r (checkExpr m kEnv  e) (map (checkAlt m kEnv) alts)
> checkExpr m kEnv  (RecordConstr fs) =
>   RecordConstr (map (checkFieldExpr m kEnv) fs)
> checkExpr m kEnv  (RecordSelection e l) =
>   RecordSelection (checkExpr m kEnv  e) l
> checkExpr m kEnv  (RecordUpdate fs e) =
>   RecordUpdate (map (checkFieldExpr m kEnv) fs) (checkExpr m kEnv  e)

> checkStmt :: ModuleIdent -> KindEnv -> Statement -> Statement
> checkStmt m kEnv  (StmtExpr p e) = StmtExpr p (checkExpr m kEnv  e)
> checkStmt m kEnv  (StmtBind p t e) = StmtBind p t (checkExpr m kEnv  e)
> checkStmt m kEnv  (StmtDecl ds) = StmtDecl (map (checkDecl m kEnv) ds)

> checkAlt :: ModuleIdent -> KindEnv -> Alt -> Alt
> checkAlt m kEnv (Alt p t rhs) = Alt p t (checkRhs m kEnv rhs)

> checkFieldExpr :: ModuleIdent -> KindEnv -> Field Expression
>	            -> Field Expression
> checkFieldExpr m kEnv (Field p l e) = Field p l (checkExpr m kEnv e)

\end{verbatim}
The parser cannot distinguish unqualified nullary type constructors
and type variables. Therefore, if the compiler finds an unbound
identifier in a position where a type variable is admissible, it will
interpret the identifier as such.
\begin{verbatim}

> checkClosedType :: ModuleIdent -> KindEnv -> [Ident] -> TypeExpr 
>	  -> TypeExpr
> checkClosedType m kEnv tvs ty = checkClosed tvs (checkType m kEnv  ty)

> checkType :: ModuleIdent -> KindEnv -> TypeExpr -> TypeExpr
> checkType m kEnv (ConstructorType tc tys) =
>   case qualLookupKind tc kEnv of
>     []
>       | not (isQualified tc) && null tys -> VariableType (unqualify tc)
>       | otherwise -> errorAt' (undefinedType tc)
>     [n]
>       | n == n' -> ConstructorType tc (map (checkType m kEnv ) tys)
>       | otherwise -> errorAt' (wrongArity tc n n')
>     _ -> case (qualLookupKind (qualQualify m tc) kEnv) of
>            [n] 
>               | n == n' -> ConstructorType tc (map (checkType m kEnv ) tys)
>               | otherwise -> errorAt' (wrongArity tc n n')
>            _ -> errorAt' (ambiguousType tc)
>  where n' = length tys 
> checkType m kEnv  (VariableType tv)
>   | tv == anonId = VariableType tv
>   | otherwise = checkType m kEnv  (ConstructorType (qualify tv) [])
> checkType m kEnv  (TupleType tys) =
>   TupleType (map (checkType m kEnv ) tys)
> checkType m kEnv  (ListType ty) =
>   ListType (checkType m kEnv  ty)
> checkType m kEnv  (ArrowType ty1 ty2) =
>   ArrowType (checkType m kEnv  ty1) (checkType m kEnv  ty2)
> checkType m kEnv  (RecordType fs r) =
>   RecordType (map (\ (ls,ty) -> (ls, checkType m kEnv  ty)) fs)
>	       (maybe Nothing (Just . checkType m kEnv ) r)

> checkClosed :: [Ident] -> TypeExpr -> TypeExpr
> checkClosed tvs (ConstructorType tc tys) =
>   ConstructorType tc (map (checkClosed tvs) tys)
> checkClosed tvs (VariableType tv)
>   | tv == anonId || tv `notElem` tvs = errorAt' (unboundVariable tv)
>   | otherwise = VariableType tv
> checkClosed tvs (TupleType tys) =
>   TupleType (map (checkClosed tvs) tys)
> checkClosed tvs (ListType ty) =
>   ListType (checkClosed tvs ty)
> checkClosed tvs (ArrowType ty1 ty2) =
>   ArrowType (checkClosed tvs ty1) (checkClosed tvs ty2)
> checkClosed tvs (RecordType fs r) =
>   RecordType (map (\ (ls,ty) -> (ls, checkClosed tvs ty)) fs)
>	       (maybe Nothing (Just . checkClosed tvs) r)
>       

\end{verbatim}
Auxiliary definitions
\begin{verbatim}

> tconstr :: Decl -> Ident
> tconstr (DataDecl p tc _ _) = tc
> tconstr (NewtypeDecl p tc _ _) = tc
> tconstr (TypeDecl p tc _ _) = tc
> tconstr _ = internalError "tconstr"

\end{verbatim}
Error messages:
\begin{verbatim}

> undefinedType :: QualIdent -> (Position,String)
> undefinedType tc = 
>     (positionOfQualIdent tc,
>      "Undefined type " ++ qualName tc)

> ambiguousType :: QualIdent -> (Position,String)
> ambiguousType tc = 
>     (positionOfQualIdent tc,
>      "Ambiguous type " ++ qualName tc)

> duplicateType :: Ident -> (Position,String)
> duplicateType tc = 
>     (positionOfIdent tc,
>      "More than one definition for type " ++ name tc)

> nonLinear :: Ident -> (Position,String)
> nonLinear tv =
>  (positionOfIdent tv,      
>   "Type variable " ++ name tv ++
>   " occurs more than once on left hand side of type declaration")

> noVariable :: Ident -> (Position,String)
> noVariable tv =
>  (positionOfIdent tv,      
>   "Type constructor " ++ name tv ++
>   " used in left hand side of type declaration")

> wrongArity :: QualIdent -> Int -> Int -> (Position,String)
> wrongArity tc arity argc =
>  (positionOfQualIdent tc,      
>   "Type constructor " ++ qualName tc ++ " expects " ++ arguments arity ++
>   " but is applied to " ++ show argc)
>   where arguments 0 = "no arguments"
>         arguments 1 = "1 argument"
>         arguments n = show n ++ " arguments"

> unboundVariable :: Ident -> (Position,String)
> unboundVariable tv = 
>     (positionOfIdent tv,
>      "Unbound type variable " ++ name tv)

\end{verbatim}
