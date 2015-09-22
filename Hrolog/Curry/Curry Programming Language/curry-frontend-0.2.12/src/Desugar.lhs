% $Id: Desugar.lhs,v 1.42 2004/02/15 22:10:32 wlux Exp $
%
% Copyright (c) 2001-2004, Wolfgang Lux
% See LICENSE for the full license.
%
% Modified by Martin Engelke (men@informatik.uni-kiel.de)
%
\nwfilename{Desugar.lhs}
\section{Desugaring Curry Expressions}
The desugaring pass removes all syntactic sugar from the module. In
particular, the output of the desugarer will have the following
properties.
\begin{itemize}
\item All function definitions are $\eta$-expanded.\\
  {\em Note:} Since this version is used as a frontend for PAKCS, the 
  $\eta$-expansion had been disabled.
\item No guarded right hand sides occur in equations, pattern
  declarations, and case alternatives. In addition, the declaration
  lists of the right hand sides are empty; local declarations are
  transformed into let expressions.
\item Patterns in equations and case alternatives are composed only of
  \begin{itemize}
  \item literals,
  \item variables,
  \item constructor applications, and
  \item as patterns.
  \end{itemize}
\item Expressions are composed only of
  \begin{itemize}
  \item literals,
  \item variables,
  \item constructors,
  \item (binary) applications,
  \item let expressions, and
  \item case expressions.
  \end{itemize}
\item Applications $N\:x$ in patterns and expressions, where $N$ is a
  newtype constructor, are replaced by a $x$. Note that neither the
  newtype declaration itself nor partial applications of newtype
  constructors are changed.\footnote{It were possible to replace
  partial applications of newtype constructor by \texttt{prelude.id}.
  However, our solution yields a more accurate output when the result
  of a computation includes partial applications.}
\item Function patterns are replaced by variables and are integrated
  in a guarded right hand side using the \texttt{=:<=} operator
\item Records, which currently must be declared using the keyword
  \texttt{type}, are transformed into data types with one constructor.
  Record construction and pattern matching are represented using the
  record constructor. Selection and update are represented using selector
  and update functions which are generated for each record declaration.
  The record constructor must be entered into the type environment as well
  as the selector functions and the update functions. 
\end{itemize}

\ToDo{Use a different representation for the restricted code instead
of using the syntax tree from \texttt{CurrySyntax}.}

\textbf{As we are going to insert references to real prelude entities,
all names must be properly qualified before calling this module.}
\begin{verbatim}

> module Desugar(desugar) where

> import Data.Maybe
> import Control.Arrow(second)
> import Control.Monad.State as S
> import Data.List

> import Curry.Base.Position
> import Curry.Base.Ident
> import Curry.Syntax.Utils
> import Curry.Syntax

> import Types
> import Base
> import Typing
> import Utils



posE = undefined

\end{verbatim}
New identifiers may be introduced while desugaring pattern
declarations, case and $\lambda$-expressions, and list comprehensions.
As usual, we use a state monad transformer for generating unique
names. In addition, the state is also used for passing through the
type environment, which must be augmented with the types of these new
variables.
\begin{verbatim}

> type DesugarState a = S.StateT ValueEnv (S.State Int) a

> run :: DesugarState a -> ValueEnv -> a
> run m tyEnv = S.evalState (S.evalStateT m tyEnv) 1

\end{verbatim}
The desugaring phase keeps only the type, function, and value
declarations of the module. In the current version record declarations
are transformed into data types. The remaining type declarations are
not desugared and cannot occur in local declaration groups.
They are filtered out separately.

In order to use records within other modules, the export specification
of the module has to be extended with the selector and update functions of
all exported labels.

Actually, the transformation is slightly more general than necessary
as it allows value declarations at the top-level of a module.
\begin{verbatim}

> desugar :: ValueEnv -> TCEnv -> Module -> (Module,ValueEnv)
> desugar tyEnv tcEnv (Module m es ds) = (Module m es ds',tyEnv')
>   where (ds',tyEnv') = run (desugarModule m tcEnv ds) tyEnv

> desugarModule :: ModuleIdent -> TCEnv -> [Decl] 
>	        -> DesugarState ([Decl],ValueEnv)
> desugarModule m tcEnv ds = 
>   do
>     dss <- mapM (desugarRecordDecl m tcEnv) ds
>     let ds' = concat dss
>     ds'' <- desugarDeclGroup m tcEnv ds'
>     tyEnv' <- S.get
>     return (filter isTypeDecl ds' ++ ds'', tyEnv')

\end{verbatim}


Within a declaration group, all type signatures and evaluation
annotations are discarded. First, the patterns occurring in the left
hand sides are desugared. Due to lazy patterns this may add further
declarations to the group that must be desugared as well.
\begin{verbatim}

> desugarDeclGroup :: ModuleIdent -> TCEnv -> [Decl] -> DesugarState [Decl]
> desugarDeclGroup m tcEnv ds =
>   do
>     dss' <- mapM (desugarDeclLhs m tcEnv) (filter isValueDecl ds)
>     mapM (desugarDeclRhs m tcEnv) (concat dss')

> desugarDeclLhs :: ModuleIdent -> TCEnv -> Decl -> DesugarState [Decl]
> desugarDeclLhs m tcEnv (PatternDecl p t rhs) =
>   do
>     (ds',t') <- desugarTerm m tcEnv p [] t
>     dss' <- mapM (desugarDeclLhs m tcEnv) ds'
>     return (PatternDecl p t' rhs : concat dss')
> desugarDeclLhs m tcEnv (FlatExternalDecl p fs) =
>   do
>     tyEnv <- S.get
>     return (map (externalDecl tyEnv p) fs)
>   where externalDecl tyEnv p f =
>           ExternalDecl p CallConvPrimitive (Just (name f)) f
>                        (fromType (typeOf tyEnv (Variable (qual f))))
>         qual f
>           | unRenameIdent f == f = qualifyWith m f
>           | otherwise = qualify f
> desugarDeclLhs _ _ d = return [d]

\end{verbatim}
After desugaring its right hand side, each equation is $\eta$-expanded
by adding as many variables as necessary to the argument list and
applying the right hand side to those variables ({\em Note:} $\eta$-expansion
is disabled in the version for PAKCS).
Furthermore every occurrence of a record type within the type of a function
is simplified to the corresponding type constructor from the record
declaration. This is possible because currently records must not be empty
and a record label belongs to only one record declaration.
\begin{verbatim}

> desugarDeclRhs :: ModuleIdent -> TCEnv -> Decl -> DesugarState Decl
> desugarDeclRhs m tcEnv (FunctionDecl p f eqs) =
>   do
>     tyEnv <- S.get
>     let ty =  (flip typeOf (Variable (qual f))) tyEnv
>     liftM (FunctionDecl p f) 
>	    (mapM (desugarEquation m tcEnv (arrowArgs ty)) eqs)
>   where qual f
>           | unRenameIdent f == f = qualifyWith m f
>           | otherwise = qualify f
> desugarDeclRhs _ tcEnv (ExternalDecl p cc ie f ty) =
>   return (ExternalDecl p cc (ie `mplus` Just (name f)) f ty)
> desugarDeclRhs m tcEnv (PatternDecl p t rhs) =
>   liftM (PatternDecl p t) (desugarRhs m tcEnv p rhs)
> desugarDeclRhs _ tcEnv (ExtraVariables p vs) = return (ExtraVariables p vs)

> desugarEquation :: ModuleIdent -> TCEnv -> [Type] -> Equation 
>	          -> DesugarState Equation
> desugarEquation m tcEnv tys (Equation p lhs rhs) =
>   do
>     (ds',ts') <- mapAccumM (desugarTerm m tcEnv p) [] ts
>     rhs' <- desugarRhs m tcEnv p (addDecls ds' rhs)
>     (ts'', rhs'') <- desugarFunctionPatterns m p ts' rhs'
>     return (Equation p (FunLhs f ts'') rhs'')
>   where (f,ts) = flatLhs lhs


\end{verbatim}
The transformation of patterns is straight forward except for lazy
patterns. A lazy pattern \texttt{\~}$t$ is replaced by a fresh
variable $v$ and a new local declaration $t$~\texttt{=}~$v$ in the
scope of the pattern. In addition, as-patterns $v$\texttt{@}$t$ where
$t$ is a variable or an as-pattern are replaced by $t$ in combination
with a local declaration for $v$.
\begin{verbatim}

> desugarLiteral :: Literal -> DesugarState (Either Literal ([SrcRef],[Literal]))
> desugarLiteral (Char p c) = return (Left (Char p c))
> desugarLiteral (Int v i)  = liftM (Left . fixType) S.get
>   where 
>    fixType tyEnv
>      | typeOf tyEnv v == floatType 
>          = Float (srcRefOf $ positionOfIdent v) (fromIntegral i)
>      | otherwise = Int v i
> desugarLiteral (Float p f) = return (Left (Float p f))
> desugarLiteral (String (SrcRef [i]) cs) 
>   = return (Right (consRefs i cs,zipWith (Char . SrcRef . (:[])) [i,i+2..] cs))
>   where consRefs r []     = [SrcRef [r]]
>         consRefs r (_:xs) = let r'=r+2 in r' `seq` (SrcRef [r']:consRefs r' xs)
> desugarLiteral (String is _) = error $ "internal error desugarLiteral; "++
>                                        "wrong source ref for string: "  ++ show is

> desugarList :: [SrcRef] -> (SrcRef -> b -> b -> b) -> (SrcRef -> b) -> [b] -> b
> desugarList pos cons nil xs = snd (foldr cons' nil' xs)
>   where rNil:rCs = reverse pos 
>         nil'                = (rCs,nil rNil)
>         cons' t (rC:rCs,ts) = (rCs,cons rC t ts)

> desugarTerm :: ModuleIdent -> TCEnv -> Position -> [Decl] -> ConstrTerm
>             -> DesugarState ([Decl],ConstrTerm)
> desugarTerm m tcEnv p ds (LiteralPattern l) =
>   desugarLiteral l >>=
>   either (return . (,) ds . LiteralPattern)
>          (\ (pos,ls) -> desugarTerm m tcEnv p ds $ ListPattern pos $ map LiteralPattern ls)
> desugarTerm m tcEnv p ds (NegativePattern _ l) =
>   desugarTerm m tcEnv p ds (LiteralPattern (negateLiteral l))
>   where negateLiteral (Int v i) = Int v (-i)
>         negateLiteral (Float p f) = Float p (-f)
>         negateLiteral _ = internalError "negateLiteral"
> desugarTerm _ _ _ ds (VariablePattern v) = return (ds,VariablePattern v)
> desugarTerm m tcEnv p ds (ConstructorPattern c [t]) =
>   do
>     tyEnv <- S.get
>     liftM (if isNewtypeConstr tyEnv c then id else second (constrPat c))
>           (desugarTerm m tcEnv p ds t)
>   where constrPat c t = ConstructorPattern c [t]
> desugarTerm m tcEnv p ds (ConstructorPattern c ts) =
>   liftM (second (ConstructorPattern c)) (mapAccumM (desugarTerm m tcEnv p) ds ts)
> desugarTerm m tcEnv p ds (InfixPattern t1 op t2) =
>   desugarTerm m tcEnv p ds (ConstructorPattern op [t1,t2])
> desugarTerm m tcEnv p ds (ParenPattern t) = desugarTerm m tcEnv p ds t
> desugarTerm m tcEnv p ds (TuplePattern pos ts) =
>   desugarTerm m tcEnv p ds (ConstructorPattern (tupleConstr ts) ts)
>   where tupleConstr ts = addRef pos $ 
>                          if null ts then qUnitId else qTupleId (length ts)
> desugarTerm m tcEnv p ds (ListPattern pos ts) =
>   liftM (second (desugarList pos cons nil)) (mapAccumM (desugarTerm m tcEnv p) ds ts)
>   where nil  p' = ConstructorPattern (addRef p' qNilId) []
>         cons p' t ts = ConstructorPattern (addRef p' qConsId) [t,ts]

> desugarTerm m tcEnv p ds (AsPattern v t) =
>   liftM (desugarAs p v) (desugarTerm m tcEnv p ds t)
> desugarTerm m tcEnv p ds (LazyPattern pos t) = desugarLazy pos m p ds t
> desugarTerm m tcEnv p ds (FunctionPattern f ts) =
>   liftM (second (FunctionPattern f)) (mapAccumM (desugarTerm m tcEnv p) ds ts)
> desugarTerm m tcEnv p ds (InfixFuncPattern t1 f t2) =
>   desugarTerm m tcEnv p ds (FunctionPattern f [t1,t2])
> desugarTerm m tcEnv p ds (RecordPattern fs _)
>   | null fs = internalError "desugarTerm: empty record"
>   | otherwise =
>     do tyEnv <- S.get 
>	 case (lookupValue (fieldLabel (head fs)) tyEnv) of
>          [Label _ r _] -> 
>            desugarRecordPattern m tcEnv p ds (map field2Tuple fs) r
>          _ -> internalError "desugarTerm: no label"

> desugarAs :: Position -> Ident -> ([Decl],ConstrTerm) -> ([Decl],ConstrTerm)
> desugarAs p v (ds,t) =
>  case t of
>    VariablePattern v' -> (varDecl p v (mkVar v') : ds,t)
>    AsPattern v' _ -> (varDecl p v (mkVar v') : ds,t)
>    _ -> (ds,AsPattern v t)

> desugarLazy :: SrcRef -> ModuleIdent -> Position -> [Decl] -> ConstrTerm
>             -> DesugarState ([Decl],ConstrTerm)
> desugarLazy pos m p ds t =
>   case t of
>     VariablePattern _ -> return (ds,t)
>     ParenPattern t' -> desugarLazy pos m p ds t'
>     AsPattern v t' -> liftM (desugarAs p v) (desugarLazy pos m p ds t')
>     LazyPattern pos t' -> desugarLazy pos m p ds t'
>     _ ->
>       do
>         v0 <- S.get >>= freshIdent m "_#lazy" . monoType . flip typeOf t
>         let v' = addPositionIdent (AST pos) v0
>         return (patDecl p{astRef=pos} t (mkVar v') : ds,VariablePattern v')


\end{verbatim}
A list of boolean guards is expanded into a nested if-then-else
expression, whereas a constraint guard is replaced by a case
expression. Note that if the guard type is \texttt{Success} only a
single guard is allowed for each equation.\footnote{This change was
introduced in version 0.8 of the Curry report.} We check for the
type \texttt{Bool} of the guard because the guard's type defaults to
\texttt{Success} if it is not restricted by the guard expression.
\begin{verbatim}

> desugarRhs :: ModuleIdent -> TCEnv -> Position -> Rhs -> DesugarState Rhs
> desugarRhs m tcEnv p rhs =
>   do
>     tyEnv <- S.get
>     e' <- desugarExpr m tcEnv p (expandRhs tyEnv prelFailed rhs)
>     return (SimpleRhs p e' [])

> expandRhs :: ValueEnv -> Expression -> Rhs -> Expression
> expandRhs tyEnv _ (SimpleRhs _ e ds) = Let ds e
> expandRhs tyEnv e0 (GuardedRhs es ds) = Let ds (expandGuards tyEnv e0 es)

> expandGuards :: ValueEnv -> Expression -> [CondExpr] -> Expression
> expandGuards tyEnv e0 es
>   | booleanGuards tyEnv es = foldr mkIfThenElse e0 es
>   | otherwise = mkCond es
>   where mkIfThenElse (CondExpr p g e) = IfThenElse (srcRefOf p) g e
>         mkCond [CondExpr p g e] = Apply (Apply prelCond g) e

> booleanGuards :: ValueEnv -> [CondExpr] -> Bool
> booleanGuards _ [] = False
> booleanGuards tyEnv (CondExpr _ g _ : es) =
>   not (null es) || typeOf tyEnv g == boolType

> desugarExpr :: ModuleIdent -> TCEnv -> Position -> Expression
>             -> DesugarState Expression
> desugarExpr m tcEnv p (Literal l) =
>   desugarLiteral l >>=
>   either (return . Literal) (\ (pos,ls) -> desugarExpr m tcEnv p $ List pos $ map Literal ls)
> desugarExpr _ _ _ (Variable v) = return (Variable v)
> desugarExpr _ _ _ (Constructor c) = return (Constructor c)
> desugarExpr m tcEnv p (Paren e) = desugarExpr m tcEnv p e
> desugarExpr m tcEnv p (Typed e _) = desugarExpr m tcEnv p e
> desugarExpr m tcEnv p (Tuple pos es) =
>   liftM (apply (Constructor (tupleConstr es))) 
>         (mapM (desugarExpr m tcEnv p) es)
>   where tupleConstr es = addRef pos $ if null es then qUnitId else qTupleId (length es)
> desugarExpr m tcEnv p (List pos es) =
>   liftM (desugarList pos cons nil) (mapM (desugarExpr m tcEnv p) es)
>   where nil p'  = Constructor (addRef p' qNilId)
>         cons p' = Apply . Apply (Constructor $ addRef p' qConsId)
> desugarExpr m tcEnv p (ListCompr pos e []) = desugarExpr m tcEnv p (List [pos,pos] [e])
> desugarExpr m tcEnv p (ListCompr r e (q:qs)) = 
>   desugarQual m tcEnv p q (ListCompr r e qs)
> desugarExpr m tcEnv p (EnumFrom e) = 
>   liftM (Apply prelEnumFrom) (desugarExpr m tcEnv p e)
> desugarExpr m tcEnv p (EnumFromThen e1 e2) =
>   liftM (apply prelEnumFromThen) (mapM (desugarExpr m tcEnv p) [e1,e2])
> desugarExpr m tcEnv p (EnumFromTo e1 e2) =
>   liftM (apply prelEnumFromTo) (mapM (desugarExpr m tcEnv p) [e1,e2])
> desugarExpr m tcEnv p (EnumFromThenTo e1 e2 e3) =
>   liftM (apply prelEnumFromThenTo) (mapM (desugarExpr m tcEnv p) [e1,e2,e3])
> desugarExpr m tcEnv p (UnaryMinus op e) =
>   do
>     tyEnv <- S.get
>     liftM (Apply (unaryMinus op (typeOf tyEnv e))) (desugarExpr m tcEnv p e)
>   where unaryMinus op ty
>           | op == minusId =
>               if ty == floatType then prelNegateFloat else prelNegate
>           | op == fminusId = prelNegateFloat
>           | otherwise = internalError "unaryMinus"
> desugarExpr m tcEnv p (Apply (Constructor c) e) =
>   do
>     tyEnv <- S.get
>     liftM (if isNewtypeConstr tyEnv c then id else (Apply (Constructor c)))
>           (desugarExpr m tcEnv p e)
> desugarExpr m tcEnv p (Apply e1 e2) =
>   do
>     e1' <- desugarExpr m tcEnv p e1
>     e2' <- desugarExpr m tcEnv p e2
>     return (Apply e1' e2')
> desugarExpr m tcEnv p (InfixApply e1 op e2) =
>   do
>     op' <- desugarExpr m tcEnv p (infixOp op)
>     e1' <- desugarExpr m tcEnv p e1
>     e2' <- desugarExpr m tcEnv p e2
>     return (Apply (Apply op' e1') e2')
> desugarExpr m tcEnv p (LeftSection e op) =
>   do
>     op' <- desugarExpr m tcEnv p (infixOp op)
>     e' <- desugarExpr m tcEnv p e
>     return (Apply op' e')
> desugarExpr m tcEnv p (RightSection op e) =
>   do
>     op' <- desugarExpr m tcEnv p (infixOp op)
>     e' <- desugarExpr m tcEnv p e
>     return (Apply (Apply prelFlip op') e')
> desugarExpr m tcEnv p exp@(Lambda r ts e) =
>   do
>     f <- S.get >>=
>          freshIdent m "_#lambda" . polyType . flip typeOf exp
>     desugarExpr m tcEnv p (Let [funDecl (AST r) f ts e] (mkVar f))
> desugarExpr m tcEnv p (Let ds e) =
>   do
>     ds' <- desugarDeclGroup m tcEnv ds
>     e' <- desugarExpr m tcEnv p e
>     return (if null ds' then e' else Let ds' e')
> desugarExpr m tcEnv p (Do sts e) = 
>   desugarExpr m tcEnv p (foldr desugarStmt e sts)
>   where desugarStmt (StmtExpr r e) e' = apply (prelBind_ r) [e,e']
>         desugarStmt (StmtBind r t e) e' = apply (prelBind r) [e,Lambda r [t] e']
>         desugarStmt (StmtDecl ds) e' = Let ds e'
> desugarExpr m tcEnv p (IfThenElse r e1 e2 e3) =
>   do
>     e1' <- desugarExpr m tcEnv p e1
>     e2' <- desugarExpr m tcEnv p e2
>     e3' <- desugarExpr m tcEnv p e3
>     return (Case r e1' [caseAlt p truePattern e2',caseAlt p falsePattern e3'])
> desugarExpr m tcEnv p (Case r e alts)
>   | null alts = return prelFailed
>   | otherwise =
>       do
>         e' <- desugarExpr m tcEnv p e
>         v <- S.get >>= freshIdent m "_#case" . monoType . flip typeOf e
>         alts' <- mapM (desugarAltLhs m tcEnv) alts
>         tyEnv <- S.get
>         alts'' <- mapM (desugarAltRhs m tcEnv)
>                        (map (expandAlt tyEnv v) (init (tails alts')))
>         return (mkCase m v e' alts'')
>   where mkCase m v e alts
>           | v `elem` qfv m alts = Let [varDecl p v e] (Case r (mkVar v) alts)
>           | otherwise = Case r e alts
> desugarExpr m tcEnv p (RecordConstr fs)
>   | null fs = internalError "desugarExpr: empty record construction"
>   | otherwise =
>       do let l = fieldLabel (head fs)
>	       fs' = map field2Tuple fs
>          tyEnv <- S.get
>	   case (lookupValue l tyEnv) of
>            [Label l' r _] -> desugarRecordConstr m tcEnv p r fs'
>            _  -> internalError "desugarExpr: illegal record construction"
> desugarExpr m tcEnv p (RecordSelection e l) =
>   do tyEnv <- S.get
>      case (lookupValue l tyEnv) of
>        [Label _ r _] -> desugarRecordSelection m tcEnv p r l e
>        _ -> internalError "desugarExpr: illegal record selection"
> desugarExpr m tcEnv p (RecordUpdate fs rexpr)
>   | null fs = internalError "desugarExpr: empty record update"
>   | otherwise =
>       do let l = fieldLabel (head fs)
>	       fs' = map field2Tuple fs
>          tyEnv <- S.get
>	   case (lookupValue l tyEnv) of
>            [Label _ r _] -> desugarRecordUpdate m tcEnv p r rexpr fs'
>            _  -> internalError "desugarExpr: illegal record update"

desugarExpr _ _ _ x = internalError $ "desugarExpr: unexpected expression " ++ show x

\end{verbatim}
If an alternative in a case expression has boolean guards and all of
these guards return \texttt{False}, the enclosing case expression does
not fail but continues to match the remaining alternatives against the
selector expression. In order to implement this semantics, which is
compatible with Haskell, we expand an alternative with boolean guards
such that it evaluates a case expression with the remaining cases that
are compatible with the matched pattern when the guards fail.
\begin{verbatim}

> desugarAltLhs :: ModuleIdent -> TCEnv -> Alt -> DesugarState Alt
> desugarAltLhs m tcEnv (Alt p t rhs) =
>   do
>     (ds',t') <- desugarTerm m tcEnv p [] t
>     return (Alt p t' (addDecls ds' rhs))

> desugarAltRhs :: ModuleIdent -> TCEnv -> Alt -> DesugarState Alt
> desugarAltRhs m tcEnv (Alt p t rhs) = 
>   liftM (Alt p t) (desugarRhs m tcEnv p rhs)

> expandAlt :: ValueEnv -> Ident -> [Alt] -> Alt
> expandAlt tyEnv v (Alt p t rhs : alts) = caseAlt p t (expandRhs tyEnv e0 rhs)
>   where e0 = Case (srcRefOf p) (mkVar v) 
>                   (filter (isCompatible t . altPattern) alts)
>         altPattern (Alt _ t _) = t

> isCompatible :: ConstrTerm -> ConstrTerm -> Bool
> isCompatible (VariablePattern _) _ = True
> isCompatible _ (VariablePattern _) = True
> isCompatible (AsPattern _ t1) t2 = isCompatible t1 t2
> isCompatible t1 (AsPattern _ t2) = isCompatible t1 t2
> isCompatible (ConstructorPattern c1 ts1) (ConstructorPattern c2 ts2) =
>   and ((c1 == c2) : zipWith isCompatible ts1 ts2)
> isCompatible (LiteralPattern l1) (LiteralPattern l2) = canon l1 == canon l2
>   where canon (Int _ i) = Int anonId i
>         canon l = l

\end{verbatim}
The frontend provides several extensions of the Curry functionality, which
have to be desugared as well. This part transforms the following extensions:
\begin{itemize}
\item runction patterns
\item records
\end{itemize}
\begin{verbatim}

> desugarFunctionPatterns :: ModuleIdent -> Position -> [ConstrTerm] -> Rhs
>	                     -> DesugarState ([ConstrTerm], Rhs)
> desugarFunctionPatterns m p ts rhs = 
>   do (ts', its) <- elimFunctionPattern m p ts
>      rhs' <- genFunctionPatternExpr m p its rhs
>      return (ts', rhs')

> desugarRecordDecl :: ModuleIdent -> TCEnv -> Decl -> DesugarState [Decl]
> desugarRecordDecl m tcEnv (TypeDecl p r vs (RecordType fss _)) =
>   case (qualLookupTC r' tcEnv) of
>     [AliasType _ n (TypeRecord fs' _)] ->
>       do tyEnv <- S.get
>	   let tys = concatMap (\ (ls,ty) -> replicate (length ls) ty) fss
>	       --tys' = map (elimRecordTypes tyEnv) tys
>	       rdecl = DataDecl p r vs [ConstrDecl p [] r tys]
>	       rty' = TypeConstructor r' (map TypeVariable [0 .. n-1])
>              rcts' = ForAllExist 0 n (foldr TypeArrow rty' (map snd fs'))
>	   rfuncs <- mapM (genRecordFuncs m tcEnv p r' rty' (map fst fs')) fs'
>	   S.modify (bindGlobalInfo DataConstructor m r rcts')
>          return (rdecl:(concat rfuncs))
>     _ -> internalError "desugarRecordDecl: no record"
>   where r' = qualifyWith m r
> desugarRecordDecl _ _ decl = return [decl]

> desugarRecordPattern :: ModuleIdent -> TCEnv -> Position -> [Decl]
>		       -> [(Ident,ConstrTerm)] -> QualIdent
>		       -> DesugarState ([Decl],ConstrTerm)
> desugarRecordPattern m tcEnv p ds fs r =
>   case (qualLookupTC r tcEnv) of
>     [AliasType _ _ (TypeRecord fs' _)] ->
>       do let ts = map (\ (l,_) 
>		         -> fromMaybe (VariablePattern anonId)
>		                      (lookup l fs))
>		        fs'
>	   desugarTerm m tcEnv p ds (ConstructorPattern r ts)

> desugarRecordConstr :: ModuleIdent -> TCEnv -> Position -> QualIdent 
>	              -> [(Ident,Expression)] -> DesugarState Expression
> desugarRecordConstr m tcEnv p r fs =
>   case (qualLookupTC r tcEnv) of
>     [AliasType _ _ (TypeRecord fs' _)] ->
>       do let cts = map (\ (l,_) -> 
>	                  fromMaybe (internalError "desugarRecordConstr")
>		                    (lookup l fs)) fs'
>	   desugarExpr m tcEnv p (foldl Apply (Constructor r) cts)
>     _ -> internalError "desugarRecordConstr: wrong type"

> desugarRecordSelection :: ModuleIdent -> TCEnv -> Position -> QualIdent 
>		         -> Ident -> Expression -> DesugarState Expression
> desugarRecordSelection m tcEnv p r l e =
>   desugarExpr m tcEnv p (Apply (Variable (qualRecSelectorId m r l)) e)

> desugarRecordUpdate :: ModuleIdent -> TCEnv -> Position -> QualIdent
>	              -> Expression -> [(Ident,Expression)] 
>	              -> DesugarState Expression
> desugarRecordUpdate m tcEnv p r rexpr fs =
>   desugarExpr m tcEnv p (foldl (genRecordUpdate m r) rexpr fs)
>   where
>   genRecordUpdate m r rexpr (l,e) =
>     Apply (Apply (Variable (qualRecUpdateId m r l)) rexpr) e

> elimFunctionPattern :: ModuleIdent -> Position -> [ConstrTerm]
>		         -> DesugarState ([ConstrTerm], [(Ident,ConstrTerm)])
> elimFunctionPattern m p [] = return ([],[])
> elimFunctionPattern m p (t:ts)
>    | containsFunctionPattern t
>      = do tyEnv <- S.get
>	    ident <- freshIdent m "_#funpatt" (monoType (typeOf tyEnv t))
>	    (ts',its') <- elimFunctionPattern m p ts
>           return ((VariablePattern ident):ts', (ident,t):its')
>    | otherwise
>      = do (ts', its') <- elimFunctionPattern m p ts
>	    return (t:ts', its')

> containsFunctionPattern :: ConstrTerm -> Bool
> containsFunctionPattern (ConstructorPattern _ ts)
>    = any containsFunctionPattern ts
> containsFunctionPattern (InfixPattern t1 _ t2)
>    = any containsFunctionPattern [t1,t2]
> containsFunctionPattern (ParenPattern t)
>    = containsFunctionPattern t
> containsFunctionPattern (TuplePattern _ ts)
>    = any containsFunctionPattern ts
> containsFunctionPattern (ListPattern _ ts)
>    = any containsFunctionPattern ts
> containsFunctionPattern (AsPattern _ t)
>    = containsFunctionPattern t
> containsFunctionPattern (LazyPattern _ t)
>    = containsFunctionPattern t
> containsFunctionPattern (FunctionPattern _ _) = True
> containsFunctionPattern (InfixFuncPattern _ _ _) = True
> containsFunctionPattern _ = False

> genFunctionPatternExpr :: ModuleIdent -> Position -> [(Ident, ConstrTerm)]
>		            -> Rhs -> DesugarState Rhs
> genFunctionPatternExpr m _ its rhs@(SimpleRhs p expr decls)
>    | null its = return rhs
>    | otherwise
>      = let ies = map (\ (i,t) -> (i, constrTerm2Expr t)) its
>	     fpexprs = map (\ (ident, expr) 
>		            -> Apply (Apply prelFuncPattEqu expr) 
>		                     (Variable (qualify ident)))
>	                   ies
>	     fpexpr =  foldl (\e1 e2 -> Apply (Apply prelConstrConj e1) e2)
>	                     (head fpexprs) 
>		             (tail fpexprs)
>	     freevars = foldl getConstrTermVars [] (map snd its)
>            rhsexpr = Let [ExtraVariables p freevars]
>		           (Apply (Apply prelCond fpexpr) expr)
>        in  return (SimpleRhs p rhsexpr decls)  
> genFunctionPatternExpr _ _ _ rhs
>    = internalError "genFunctionPatternExpr: unexpected right-hand-side"

> constrTerm2Expr :: ConstrTerm -> Expression
> constrTerm2Expr (LiteralPattern lit)
>    = Literal lit
> constrTerm2Expr (VariablePattern ident)
>    = Variable (qualify ident)
> constrTerm2Expr (ConstructorPattern qident cts)
>    = foldl (\e1 e2 -> Apply e1 e2) 
>            (Constructor qident) 
>            (map constrTerm2Expr cts)
> constrTerm2Expr (FunctionPattern qident cts)
>    = foldl (\e1 e2 -> Apply e1 e2) 
>            (Variable qident) 
>            (map constrTerm2Expr cts)
> constrTerm2Expr _
>    = internalError "constrTerm2Expr: unexpected constructor term"

> getConstrTermVars :: [Ident] -> ConstrTerm -> [Ident]
> getConstrTermVars ids (VariablePattern ident)
>    | elem ident ids = ids
>    | otherwise      = ident:ids
> getConstrTermVars ids (ConstructorPattern _ cts)
>    = foldl getConstrTermVars ids cts
> getConstrTermVars ids (InfixPattern c1 qid c2)
>    = getConstrTermVars ids (ConstructorPattern qid [c1,c2])
> getConstrTermVars ids (ParenPattern c)
>    = getConstrTermVars ids c
> getConstrTermVars ids (TuplePattern _ cts)
>    = foldl getConstrTermVars ids cts
> getConstrTermVars ids (ListPattern _ cts)
>    = foldl getConstrTermVars ids cts
> getConstrTermVars ids (AsPattern _ c)
>    = getConstrTermVars ids c
> getConstrTermVars ids (LazyPattern _ c)
>    = getConstrTermVars ids c
> getConstrTermVars ids (FunctionPattern _ cts)
>    = foldl getConstrTermVars ids cts
> getConstrTermVars ids (InfixFuncPattern c1 qid c2)
>    = getConstrTermVars ids (FunctionPattern qid [c1,c2])
> getConstrTermVars ids _
>    = ids

> genRecordFuncs :: ModuleIdent -> TCEnv -> Position -> QualIdent -> Type 
>	         -> [Ident] -> (Ident, Type) -> DesugarState [Decl]
> genRecordFuncs m tcEnv p r rty ls (l,ty) =
>   case (qualLookupTC r tcEnv) of
>     [AliasType _ n (TypeRecord fs _)] ->
>       do let (selId, selFunc) = genSelectorFunc m p r ls l
>              (updId, updFunc) = genUpdateFunc m p r ls l
>	       selType = polyType (TypeArrow rty ty)
>	       updType = polyType (TypeArrow rty (TypeArrow ty rty))
>	   S.modify (bindFun m selId selType . bindFun m updId updType)
>	   return [selFunc,updFunc]
>     _ -> internalError "genRecordFuncs: wrong type"

> genSelectorFunc :: ModuleIdent -> Position -> QualIdent -> [Ident] -> Ident
>	          -> (Ident, Decl)
> genSelectorFunc m p r ls l =
>   let selId = recSelectorId r l
>       cpatt = ConstructorPattern r (map VariablePattern ls)
>	selLhs = FunLhs selId [cpatt]
>	selRhs = SimpleRhs p (Variable (qualify l)) []
>   in  (selId, FunctionDecl p selId [Equation p selLhs selRhs])

> genUpdateFunc :: ModuleIdent -> Position -> QualIdent -> [Ident] -> Ident
>	        -> (Ident, Decl)
> genUpdateFunc m p r ls l =
>   let updId = recUpdateId r l
>	ls' = replaceIdent l anonId ls
>	cpatt1 = ConstructorPattern r (map VariablePattern ls')
>       cpatt2 = VariablePattern l
>	cexpr = foldl Apply 
>	              (Constructor r)
>	              (map (Variable . qualify) ls) 
>	updLhs = FunLhs updId [cpatt1, cpatt2]
>	updRhs = SimpleRhs p cexpr []
>   in  (updId, FunctionDecl p updId [Equation p updLhs updRhs])

> replaceIdent :: Ident -> Ident -> [Ident] -> [Ident]
> replaceIdent _ _ [] = []
> replaceIdent what with (id:ids)
>   | what == id = with:ids
>   | otherwise  = id:(replaceIdent what with ids)

\end{verbatim}
In general, a list comprehension of the form
\texttt{[}$e$~\texttt{|}~$t$~\texttt{<-}~$l$\texttt{,}~\emph{qs}\texttt{]}
is transformed into an expression \texttt{foldr}~$f$~\texttt{[]}~$l$ where $f$
is a new function defined as
\begin{quote}
  \begin{tabbing}
    $f$ $x$ \emph{xs} \texttt{=} \\
    \quad \= \texttt{case} $x$ \texttt{of} \\
          \> \quad \= $t$ \texttt{->} \texttt{[}$e$ \texttt{|} \emph{qs}\texttt{]} \texttt{++} \emph{xs} \\
          \>       \> \texttt{\_} \texttt{->} \emph{xs}
  \end{tabbing}
\end{quote}
Note that this translation evaluates the elements of $l$ rigidly,
whereas the translation given in the Curry report is flexible.
However, it does not seem very useful to have the comprehension
generate instances of $t$ which do not contribute to the list.

Actually, we generate slightly better code in a few special cases.
When $t$ is a plain variable, the \texttt{case} expression degenerates
into a let-binding and the auxiliary function thus becomes an alias
for \texttt{(++)}. Instead of \texttt{foldr~(++)} we use the
equivalent prelude function \texttt{concatMap}. In addition, if the
remaining list comprehension in the body of the auxiliary function has
no qualifiers -- i.e., if it is equivalent to \texttt{[$e$]} -- we
avoid the construction of the singleton list by calling \texttt{(:)}
instead of \texttt{(++)} and \texttt{map} in place of
\texttt{concatMap}, respectively. -}
\begin{verbatim}

> desugarQual :: ModuleIdent -> TCEnv -> Position -> Statement -> Expression
>      -> DesugarState Expression
> desugarQual m tcEnv p (StmtExpr pos b) e = 
>   desugarExpr m tcEnv p (IfThenElse pos b e (List [pos] []))
> desugarQual m tcEnv p (StmtBind refBind t l) e
>   | isVarPattern t = desugarExpr m tcEnv p (qualExpr t e l)
>   | otherwise =
>       do
>         tyEnv <- S.get
>         v0 <- freshIdent m "_#var" (monoType (typeOf tyEnv t))
>         l0 <- freshIdent m "_#var" (monoType (typeOf tyEnv e))
>         let v  = addRefId refBind v0
>             l' = addRefId refBind l0
>         desugarExpr m tcEnv p (apply (prelFoldr refBind) 
>                                      [foldFunct v l' e,List [refBind] [],l])
>   where 
>     qualExpr v (ListCompr _ e []) l 
>       = apply (prelMap refBind) [Lambda refBind [v] e,l]
>     qualExpr v e l = apply (prelConcatMap refBind) [Lambda refBind [v] e,l]

>     foldFunct v l e =
>           Lambda refBind (map VariablePattern [v,l])
>             (Case refBind (mkVar v)
>                   [caseAlt {-refBind-} p t (append e (mkVar l)),
>                    caseAlt {-refBind-} p (VariablePattern v) (mkVar l)])
>
>     append (ListCompr _ e []) l = apply (Constructor $ addRef refBind $ qConsId) [e,l]
>     append e l = apply (prelAppend refBind) [e,l]
>
> desugarQual m tcEnv p (StmtDecl ds) e = desugarExpr m tcEnv p (Let ds e)

\end{verbatim}
Generation of fresh names
\begin{verbatim}

> freshIdent :: ModuleIdent -> String -> TypeScheme -> DesugarState Ident
> freshIdent m prefix ty =
>   do
>     x <- liftM (mkName prefix) (S.lift (S.modify succ >> S.get))
>     S.modify (bindFun m x ty)
>     return x
>   where mkName pre n = mkIdent (pre ++ show n)

\end{verbatim}
Prelude entities
\begin{verbatim}

> prelBind = prel ">>="
> prelBind_ = prel ">>"
> prelFlip = Variable $ preludeIdent "flip"
> prelEnumFrom = Variable $ preludeIdent "enumFrom"
> prelEnumFromTo = Variable $ preludeIdent "enumFromTo"
> prelEnumFromThen = Variable $ preludeIdent "enumFromThen"
> prelEnumFromThenTo = Variable $ preludeIdent "enumFromThenTo"
> prelFailed = Variable $ preludeIdent "failed"
> prelMap r = Variable $ addRef r $ preludeIdent "map"
> prelFoldr = prel "foldr"
> prelAppend = prel "++"
> prelConcatMap = prel "concatMap"
> prelNegate = Variable $ preludeIdent "negate"
> prelNegateFloat = Variable $ preludeIdent "negateFloat"
> prelCond = Variable $ preludeIdent "cond"
> prelFuncPattEqu = Variable $ preludeIdent "=:<="
> prelConstrConj = Variable $ preludeIdent "&"

> prel s r = Variable (addRef r (preludeIdent s))

> truePattern = ConstructorPattern qTrueId []
> falsePattern = ConstructorPattern qFalseId []


> preludeIdent :: String -> QualIdent
> preludeIdent = qualifyWith preludeMIdent . mkIdent

\end{verbatim}
Auxiliary definitions
\begin{verbatim}

> isNewtypeConstr :: ValueEnv -> QualIdent -> Bool
> isNewtypeConstr tyEnv c =
>   case qualLookupValue c tyEnv of
>     [DataConstructor _ _] -> False
>     [NewtypeConstructor _ _] -> True
>     _ -> internalError ("isNewtypeConstr " ++ show c) --internalError "isNewtypeConstr"

> isVarPattern :: ConstrTerm -> Bool
> isVarPattern (VariablePattern _) = True
> isVarPattern (ParenPattern t) = isVarPattern t
> isVarPattern (AsPattern _ t) = isVarPattern t
> isVarPattern (LazyPattern _ _) = True
> isVarPattern _ = False

> funDecl :: Position -> Ident -> [ConstrTerm] -> Expression -> Decl
> funDecl p f ts e =
>   FunctionDecl p f [Equation p (FunLhs f ts) (SimpleRhs p e [])]

> patDecl :: Position -> ConstrTerm -> Expression -> Decl
> patDecl p t e = PatternDecl p t (SimpleRhs p e [])

> varDecl :: Position -> Ident -> Expression -> Decl
> varDecl p = patDecl p . VariablePattern

> addDecls :: [Decl] -> Rhs -> Rhs
> addDecls ds (SimpleRhs p e ds') = SimpleRhs p e (ds ++ ds')
> addDecls ds (GuardedRhs es ds') = GuardedRhs es (ds ++ ds')

> caseAlt :: Position -> ConstrTerm -> Expression -> Alt
> caseAlt p t e = Alt p t (SimpleRhs p e [])

> apply :: Expression -> [Expression] -> Expression
> apply = foldl Apply

> mkVar :: Ident -> Expression
> mkVar = Variable . qualify


\end{verbatim}
