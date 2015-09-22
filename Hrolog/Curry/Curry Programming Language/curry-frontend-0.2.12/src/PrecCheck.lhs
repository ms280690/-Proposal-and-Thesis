
% $Id: PrecCheck.lhs,v 1.21 2004/02/15 22:10:34 wlux Exp $
%
% Copyright (c) 2001-2004, Wolfgang Lux
% See LICENSE for the full license.
%
% Modified by Martin Engelke (men@informatik.uni-kiel.de)
%
\nwfilename{PrecCheck.lhs}
\section{Checking Precedences of Infix Operators}
The parser does not know the relative precedences of infix operators
and therefore parses them as if they all associate to the right and
have the same precedence. After performing the definition checks,
the compiler is going to process the infix applications in the module
and rearrange infix applications according to the relative precedences
of the operators involved.
\begin{verbatim}

> module PrecCheck(precCheck) where

> import Data.List

> import Curry.Base.Position
> import Curry.Base.Ident
> import Curry.Syntax
> import Curry.Syntax.Utils

> import Base

\end{verbatim}
For each declaration group, including the module-level, the compiler
first checks that its fixity declarations contain no duplicates and
that there is a corresponding value or constructor declaration in that
group. The fixity declarations are then used for extending the
imported precedence environment.
\begin{verbatim}

> bindPrecs :: ModuleIdent -> [Decl] -> PEnv -> PEnv
> bindPrecs m ds pEnv =
>   case findDouble ops of
>     Nothing ->
>       case [ op | op <- ops, op `notElem` bvs] of
>         [] -> foldr bindPrec pEnv fixDs
>         op : _ -> errorAt' (undefinedOperator op)
>     Just op -> errorAt' (duplicatePrecedence op)
>   where (fixDs,nonFixDs) = partition isInfixDecl ds
>         bvs = concatMap boundValues nonFixDs
>         ops = [ op | InfixDecl p _ _ ops <- fixDs, op <- ops]
>         bindPrec (InfixDecl _ fix pr ops) pEnv
>           | p == defaultP = pEnv
>           | otherwise = foldr (flip (bindP m) p) pEnv ops
>           where p = OpPrec fix pr

> boundValues :: Decl -> [Ident]
> boundValues (DataDecl _ _ _ cs) = map constr cs
>   where constr (ConstrDecl _ _ c _) = c
>         constr (ConOpDecl _ _ _ op _) = op
> boundValues (NewtypeDecl _ _ _ (NewConstrDecl _ _ c _)) = [c]
> boundValues (FunctionDecl _ f _) = [f]
> boundValues (ExternalDecl _ _ _ f _) = [f]
> boundValues (FlatExternalDecl _ fs) = fs
> boundValues (PatternDecl _ t _) = bv t
> boundValues (ExtraVariables _ vs) = vs
> boundValues _ = []

\end{verbatim}
With the help of the precedence environment, the compiler checks all
infix applications and sections in the program. This pass will modify
the parse tree such that for a nested infix application the operator
with the lowest precedence becomes the root and that two adjacent
operators with the same precedence will not have conflicting
associativities. Note that the top-level precedence environment has to
be returned because it is needed for constructing the module's
interface.
\begin{verbatim}

> precCheck :: ModuleIdent -> PEnv -> [Decl] -> (PEnv,[Decl])
> precCheck = checkDecls

> checkDecls :: ModuleIdent -> PEnv -> [Decl] -> (PEnv,[Decl])
> checkDecls m pEnv ds = pEnv' `seq` (pEnv',ds')
>   where pEnv' = bindPrecs m ds pEnv
>         ds' = map (checkDecl m pEnv') ds

> checkDecl :: ModuleIdent -> PEnv -> Decl -> Decl
> checkDecl m pEnv (FunctionDecl p f eqs) =
>   FunctionDecl p f (map (checkEqn m pEnv) eqs)
> checkDecl m pEnv (PatternDecl p t rhs) =
>   PatternDecl p (checkConstrTerm pEnv t) (checkRhs m pEnv rhs)
> checkDecl _ _ d = d

> checkEqn :: ModuleIdent -> PEnv -> Equation -> Equation
> checkEqn m pEnv (Equation p lhs rhs) =
>   Equation p (checkLhs pEnv lhs) (checkRhs m pEnv rhs)

> checkLhs :: PEnv -> Lhs -> Lhs
> checkLhs pEnv (FunLhs f ts) = FunLhs f (map (checkConstrTerm pEnv) ts)
> checkLhs pEnv (OpLhs t1 op t2) = t1' `seq` t2' `seq` OpLhs t1' op t2'
>   where t1' = checkOpL pEnv op (checkConstrTerm pEnv t1)
>         t2' = checkOpR pEnv op (checkConstrTerm pEnv t2)
> checkLhs pEnv (ApLhs lhs ts) =
>   ApLhs (checkLhs pEnv lhs) (map (checkConstrTerm pEnv) ts)

> checkConstrTerm :: PEnv -> ConstrTerm -> ConstrTerm
> checkConstrTerm _ (LiteralPattern l) = LiteralPattern l
> checkConstrTerm _ (NegativePattern op l) = NegativePattern op l
> checkConstrTerm _ (VariablePattern v) = VariablePattern v
> checkConstrTerm pEnv (ConstructorPattern c ts) =
>   ConstructorPattern c (map (checkConstrTerm pEnv) ts)
> checkConstrTerm pEnv (InfixPattern t1 op t2) =
>   fixPrecT pEnv InfixPattern
>	 (checkConstrTerm pEnv t1) op (checkConstrTerm pEnv t2)
> checkConstrTerm pEnv (ParenPattern t) =
>   ParenPattern (checkConstrTerm pEnv t)
> checkConstrTerm pEnv (TuplePattern p ts) =
>   TuplePattern p (map (checkConstrTerm pEnv) ts)
> checkConstrTerm pEnv (ListPattern p ts) =
>   ListPattern p (map (checkConstrTerm pEnv) ts)
> checkConstrTerm pEnv (AsPattern v t) =
>   AsPattern v (checkConstrTerm pEnv t)
> checkConstrTerm pEnv (LazyPattern p t) =
>   LazyPattern p (checkConstrTerm pEnv t)
> checkConstrTerm pEnv (FunctionPattern f ts) =
>   FunctionPattern f (map (checkConstrTerm pEnv) ts)
> checkConstrTerm pEnv (InfixFuncPattern t1 op t2) =
>   fixPrecT pEnv InfixFuncPattern 
>	 (checkConstrTerm pEnv t1) op (checkConstrTerm pEnv t2)
> checkConstrTerm pEnv (RecordPattern fs r) =
>   RecordPattern (map (checkFieldPattern pEnv) fs)
>	          (maybe Nothing (Just . checkConstrTerm pEnv) r)

> checkFieldPattern :: PEnv -> Field ConstrTerm -> Field ConstrTerm
> checkFieldPattern pEnv (Field p label patt) =
>     Field p label (checkConstrTerm pEnv patt)

> checkRhs :: ModuleIdent -> PEnv -> Rhs -> Rhs
> checkRhs m pEnv (SimpleRhs p e ds) = SimpleRhs p (checkExpr m pEnv' e) ds'
>   where (pEnv',ds') = checkDecls m pEnv ds
> checkRhs m pEnv (GuardedRhs es ds) =
>   GuardedRhs (map (checkCondExpr m pEnv') es) ds'
>   where (pEnv',ds') = checkDecls m pEnv ds

> checkCondExpr :: ModuleIdent -> PEnv -> CondExpr -> CondExpr
> checkCondExpr m pEnv (CondExpr p g e) =
>   CondExpr p (checkExpr m pEnv g) (checkExpr m pEnv e)

> checkExpr :: ModuleIdent -> PEnv -> Expression -> Expression
> checkExpr _ _ (Literal l) = Literal l
> checkExpr _ _ (Variable v) = Variable v
> checkExpr _ _ (Constructor c) = Constructor c
> checkExpr m pEnv (Paren e) = Paren (checkExpr m  pEnv e)
> checkExpr m pEnv (Typed e ty) = Typed (checkExpr m  pEnv e) ty
> checkExpr m pEnv (Tuple p es) = Tuple p (map (checkExpr m  pEnv) es)
> checkExpr m pEnv (List p es) = List p (map (checkExpr m  pEnv) es)
> checkExpr m pEnv (ListCompr p e qs) = ListCompr p (checkExpr m  pEnv' e) qs'
>   where (pEnv',qs') = mapAccumL (checkStmt m ) pEnv qs
> checkExpr m pEnv (EnumFrom e) = EnumFrom (checkExpr m pEnv e)
> checkExpr m pEnv (EnumFromThen e1 e2) =
>   EnumFromThen (checkExpr m pEnv e1) (checkExpr m pEnv e2)
> checkExpr m pEnv (EnumFromTo e1 e2) =
>   EnumFromTo (checkExpr m pEnv e1) (checkExpr m pEnv e2)
> checkExpr m pEnv (EnumFromThenTo e1 e2 e3) =
>   EnumFromThenTo (checkExpr m pEnv e1)
>                  (checkExpr m pEnv e2)
>                  (checkExpr m pEnv e3)
> checkExpr m pEnv (UnaryMinus op e) = UnaryMinus op (checkExpr m pEnv e)
> checkExpr m pEnv (Apply e1 e2) =
>   Apply (checkExpr m pEnv e1) (checkExpr m pEnv e2)
> checkExpr m pEnv (InfixApply e1 op e2) =
>   fixPrec pEnv (checkExpr m pEnv e1) op (checkExpr m pEnv e2)
> checkExpr m pEnv (LeftSection e op) =
>   checkLSection pEnv op (checkExpr m pEnv e)
> checkExpr m pEnv (RightSection op e) =
>   checkRSection pEnv op (checkExpr m pEnv e)
> checkExpr m pEnv (Lambda r ts e) =
>   Lambda r (map (checkConstrTerm pEnv) ts) (checkExpr m pEnv e)
> checkExpr m pEnv (Let ds e) = Let ds' (checkExpr m pEnv' e)
>   where (pEnv',ds') = checkDecls m pEnv ds
> checkExpr m pEnv (Do sts e) = Do sts' (checkExpr m pEnv' e)
>   where (pEnv',sts') = mapAccumL (checkStmt m ) pEnv sts
> checkExpr m pEnv (IfThenElse r e1 e2 e3) =
>   IfThenElse r (checkExpr m pEnv e1)
>              (checkExpr m pEnv e2)
>              (checkExpr m pEnv e3)
> checkExpr m pEnv (Case r e alts) =
>   Case r (checkExpr m pEnv e) (map (checkAlt m pEnv) alts)
> checkExpr m pEnv (RecordConstr fs) =
>   RecordConstr (map (checkFieldExpr m pEnv) fs)
> checkExpr m pEnv (RecordSelection e label) =
>   RecordSelection (checkExpr m pEnv e) label
> checkExpr m pEnv (RecordUpdate fs e) =
>   RecordUpdate (map (checkFieldExpr m pEnv) fs) (checkExpr m pEnv e)

> checkFieldExpr :: ModuleIdent -> PEnv -> Field Expression -> Field Expression
> checkFieldExpr m pEnv (Field p label e) =
>   Field p label (checkExpr m  pEnv e)

> checkStmt :: ModuleIdent -> PEnv -> Statement -> (PEnv,Statement)
> checkStmt m pEnv (StmtExpr p e) = (pEnv,StmtExpr p (checkExpr m pEnv e))
> checkStmt m pEnv (StmtDecl ds) = pEnv' `seq` (pEnv',StmtDecl ds')
>   where (pEnv',ds') = checkDecls m pEnv ds
> checkStmt m pEnv (StmtBind p t e) =
>   (pEnv,StmtBind p (checkConstrTerm pEnv t) (checkExpr m pEnv e))

> checkAlt :: ModuleIdent -> PEnv -> Alt -> Alt
> checkAlt m pEnv (Alt p t rhs) =
>   Alt p (checkConstrTerm pEnv t) (checkRhs m pEnv rhs)

\end{verbatim}
The functions \texttt{fixPrec}, \texttt{fixUPrec}, and
\texttt{fixRPrec} check the relative precedences of adjacent infix
operators in nested infix applications and unary negations. The
expressions will be reordered such that the infix operator with the
lowest precedence becomes the root of the expression. \emph{The
functions rely on the fact that the parser constructs infix
applications in a right-associative fashion}, i.e., the left argument
of an infix application will never be an infix application. In
addition, a unary negation will never have an infix application as
its argument.

The function \texttt{fixPrec} checks whether the left argument of an
infix application is a unary negation and eventually reorders the
expression if the precedence of the infix operator is higher than that
of the negation. This will be done with the help of the function
\texttt{fixUPrec}. In any case, the function \texttt{fixRPrec} is used
for fixing the precedence of the infix operator and that of its right
argument. Note that both arguments already have been checked before
\texttt{fixPrec} is called.
\begin{verbatim}

> fixPrec :: PEnv -> Expression -> InfixOp -> Expression
>         -> Expression
> fixPrec pEnv (UnaryMinus uop e1) op e2
>   | pr < 6 || pr == 6 && fix == InfixL =
>       fixRPrec pEnv (UnaryMinus uop e1) op e2
>   | pr > 6 = fixUPrec pEnv uop e1 op e2
>   | otherwise = errorAt' $ ambiguousParse "unary" (qualify uop) (opName op)
>   where OpPrec fix pr = opPrec op pEnv
> fixPrec pEnv e1 op e2 = fixRPrec pEnv e1 op e2

> fixUPrec :: PEnv -> Ident -> Expression -> InfixOp -> Expression
>          -> Expression
> fixUPrec pEnv uop  _ op (UnaryMinus _ _) =
>   errorAt' $ ambiguousParse "operator" (opName op) (qualify uop)
> fixUPrec pEnv uop e1 op1 (InfixApply e2 op2 e3)
>   | pr2 < 6 || pr2 == 6 && fix2 == InfixL =
>       InfixApply (fixUPrec pEnv uop e1 op1 e2) op2 e3
>   | pr2 > 6 = UnaryMinus uop (fixRPrec pEnv e1 op1 (InfixApply e2 op2 e3))
>   | otherwise = errorAt' $ ambiguousParse "unary" (qualify uop) (opName op2)
>   where OpPrec fix2 pr2 = opPrec op2 pEnv
> fixUPrec _ uop e1 op e2 = UnaryMinus uop (InfixApply e1 op e2)

> fixRPrec :: PEnv -> Expression -> InfixOp -> Expression
>          -> Expression
> fixRPrec pEnv e1 op (UnaryMinus uop e2)
>   | pr < 6 = InfixApply e1 op (UnaryMinus uop e2)
>   | otherwise =
>       errorAt' $ ambiguousParse "operator" (opName op) (qualify uop)
>   where OpPrec _ pr = opPrec op pEnv
> fixRPrec pEnv e1 op1 (InfixApply e2 op2 e3)
>   | pr1 < pr2 || pr1 == pr2 && fix1 == InfixR && fix2 == InfixR =
>       InfixApply e1 op1 (InfixApply e2 op2 e3)
>   | pr1 > pr2 || pr1 == pr2 && fix1 == InfixL && fix2 == InfixL =
>       InfixApply (fixPrec pEnv e1 op1 e2) op2 e3
>   | otherwise =
>       errorAt' $ ambiguousParse "operator" (opName op1) (opName op2)
>   where OpPrec fix1 pr1 = opPrec op1 pEnv
>         OpPrec fix2 pr2 = opPrec op2 pEnv
> fixRPrec _ e1 op e2 = InfixApply e1 op e2

\end{verbatim}
The functions \texttt{checkLSection} and \texttt{checkRSection} are
used for handling the precedences inside left and right sections.
These functions only need to check that an infix operator occurring in
the section has either a higher precedence than the section operator
or both operators have the same precedence and are both left
associative for a left section and right associative for a right
section, respectively.
\begin{verbatim}

> checkLSection :: PEnv -> InfixOp -> Expression -> Expression
> checkLSection pEnv op e@(UnaryMinus uop _)
>   | pr < 6 || pr == 6 && fix == InfixL = LeftSection e op
>   | otherwise = errorAt' $ ambiguousParse "unary" (qualify uop) (opName op)
>   where OpPrec fix pr = opPrec op pEnv
> checkLSection pEnv op1 e@(InfixApply _ op2 _)
>   | pr1 < pr2 || pr1 == pr2 && fix1 == InfixL && fix2 == InfixL =
>       LeftSection e op1
>   | otherwise =
>       errorAt' $ ambiguousParse "operator" (opName op1) (opName op2)
>   where OpPrec fix1 pr1 = opPrec op1 pEnv
>         OpPrec fix2 pr2 = opPrec op2 pEnv
> checkLSection _ op e = LeftSection e op

> checkRSection :: PEnv -> InfixOp -> Expression -> Expression
> checkRSection pEnv op e@(UnaryMinus uop _)
>   | pr < 6 = RightSection op e
>   | otherwise = errorAt' $ ambiguousParse "unary" (qualify uop) (opName op)
>   where OpPrec _ pr = opPrec op pEnv
> checkRSection pEnv op1 e@(InfixApply _ op2 _)
>   | pr1 < pr2 || pr1 == pr2 && fix1 == InfixR && fix2 == InfixR =
>       RightSection op1 e
>   | otherwise =
>       errorAt' $ ambiguousParse "operator" (opName op1) (opName op2)
>   where OpPrec fix1 pr1 = opPrec op1 pEnv
>         OpPrec fix2 pr2 = opPrec op2 pEnv
> checkRSection _ op e = RightSection op e

\end{verbatim}
The functions \texttt{fixPrecT} and \texttt{fixRPrecT} check the
relative precedences of adjacent infix operators in patterns. The
patterns will be reordered such that the infix operator with the
lowest precedence becomes the root of the term. \emph{The functions
rely on the fact that the parser constructs infix patterns in a
right-associative fashion}, i.e., the left argument of an infix pattern
will never be an infix pattern. The functions also check whether the
left and right arguments of an infix pattern are negative literals. In
this case, the negation must bind more tightly than the operator for
the pattern to be accepted.
\begin{verbatim}

> fixPrecT ::  PEnv 
>             -> (ConstrTerm -> QualIdent -> ConstrTerm -> ConstrTerm)
>	      -> ConstrTerm -> QualIdent -> ConstrTerm  
>             -> ConstrTerm
> fixPrecT pEnv infixpatt t1@(NegativePattern uop l) op t2
>   | pr < 6 || pr == 6 && fix == InfixL 
>     = fixRPrecT pEnv infixpatt t1 op t2
>   | otherwise 
>     = errorAt' $ invalidParse "unary" uop op
>   where OpPrec fix pr = prec op pEnv
> fixPrecT pEnv infixpatt t1 op t2 
>   = fixRPrecT pEnv infixpatt t1 op t2

> fixRPrecT :: PEnv 
>              -> (ConstrTerm -> QualIdent -> ConstrTerm -> ConstrTerm)
>              -> ConstrTerm  -> QualIdent -> ConstrTerm
>              -> ConstrTerm
> fixRPrecT pEnv infixpatt t1 op t2@(NegativePattern uop l)
>   | pr < 6    = infixpatt t1 op t2
>   | otherwise = errorAt' $ invalidParse "unary" uop op
>   where OpPrec _ pr = prec op pEnv
> fixRPrecT pEnv infixpatt t1 op1 (InfixPattern t2 op2 t3)
>   | pr1 < pr2 || pr1 == pr2 && fix1 == InfixR && fix2 == InfixR
>     = infixpatt t1 op1 (InfixPattern t2 op2 t3)
>   | pr1 > pr2 || pr1 == pr2 && fix1 == InfixL && fix2 == InfixL
>     = InfixPattern (fixPrecT pEnv infixpatt t1 op1 t2) op2 t3
>   | otherwise 
>     = errorAt' $ ambiguousParse "operator" op1 op2
>   where OpPrec fix1 pr1 = prec op1 pEnv
>         OpPrec fix2 pr2 = prec op2 pEnv
> fixRPrecT pEnv infixpatt t1 op1 (InfixFuncPattern t2 op2 t3)
>   | pr1 < pr2 || pr1 == pr2 && fix1 == InfixR && fix2 == InfixR
>     = infixpatt t1 op1 (InfixFuncPattern t2 op2 t3)
>   | pr1 > pr2 || pr1 == pr2 && fix1 == InfixL && fix2 == InfixL
>     = InfixFuncPattern (fixPrecT pEnv infixpatt t1 op1 t2) op2 t3
>   | otherwise 
>     = errorAt' $ ambiguousParse "operator" op1 op2
>   where OpPrec fix1 pr1 = prec op1 pEnv
>         OpPrec fix2 pr2 = prec op2 pEnv
> fixRPrecT _ infixpatt t1 op t2 = infixpatt t1 op t2

> {-fixPrecT :: Position -> PEnv -> ConstrTerm -> QualIdent -> ConstrTerm
>          -> ConstrTerm
> fixPrecT p pEnv t1@(NegativePattern uop l) op t2
>   | pr < 6 || pr == 6 && fix == InfixL = fixRPrecT p pEnv t1 op t2
>   | otherwise = errorAt p $ invalidParse "unary" uop op
>   where OpPrec fix pr = prec op pEnv
> fixPrecT p pEnv t1 op t2 = fixRPrecT p pEnv t1 op t2-}

> {-fixRPrecT :: Position -> PEnv -> ConstrTerm -> QualIdent -> ConstrTerm
>           -> ConstrTerm
> fixRPrecT p pEnv t1 op t2@(NegativePattern uop l)
>   | pr < 6 = InfixPattern t1 op t2
>   | otherwise = errorAt p $ invalidParse "unary" uop op
>   where OpPrec _ pr = prec op pEnv
> fixRPrecT p pEnv t1 op1 (InfixPattern t2 op2 t3)
>   | pr1 < pr2 || pr1 == pr2 && fix1 == InfixR && fix2 == InfixR =
>       InfixPattern t1 op1 (InfixPattern t2 op2 t3)
>   | pr1 > pr2 || pr1 == pr2 && fix1 == InfixL && fix2 == InfixL =
>       InfixPattern (fixPrecT p pEnv t1 op1 t2) op2 t3
>   | otherwise = errorAt p $ ambiguousParse "operator" op1 op2
>   where OpPrec fix1 pr1 = prec op1 pEnv
>         OpPrec fix2 pr2 = prec op2 pEnv
> fixRPrecT _ _ t1 op t2 = InfixPattern t1 op t2-}

\end{verbatim}
The functions \texttt{checkOpL} and \texttt{checkOpR} check the left
and right arguments of an operator declaration. If they are infix
patterns they must bind more tightly than the operator, otherwise the
left-hand side of the declaration is invalid.
\begin{verbatim}

> checkOpL :: PEnv -> Ident -> ConstrTerm -> ConstrTerm
> checkOpL pEnv op t@(NegativePattern uop l)
>   | pr < 6 || pr == 6 && fix == InfixL = t
>   | otherwise = errorAt' $ invalidParse "unary" uop (qualify op)
>   where OpPrec fix pr = prec (qualify op) pEnv
> checkOpL pEnv op1 t@(InfixPattern _ op2 _)
>   | pr1 < pr2 || pr1 == pr2 && fix1 == InfixL && fix2 == InfixL = t
>   | otherwise = errorAt' $ invalidParse "operator" op1 op2
>   where OpPrec fix1 pr1 = prec (qualify op1) pEnv
>         OpPrec fix2 pr2 = prec op2 pEnv
> checkOpL _ _ t = t

> checkOpR :: PEnv -> Ident -> ConstrTerm -> ConstrTerm
> checkOpR pEnv op t@(NegativePattern uop l)
>   | pr < 6 = t
>   | otherwise = errorAt' $ invalidParse "unary" uop (qualify op)
>   where OpPrec _ pr = prec (qualify op) pEnv
> checkOpR pEnv op1 t@(InfixPattern _ op2 _)
>   | pr1 < pr2 || pr1 == pr2 && fix1 == InfixR && fix2 == InfixR = t
>   | otherwise = errorAt' $ invalidParse "operator" op1 op2
>   where OpPrec fix1 pr1 = prec (qualify op1) pEnv
>         OpPrec fix2 pr2 = prec op2 pEnv
> checkOpR _ _ t = t

\end{verbatim}
The functions \texttt{opPrec} and \texttt{prec} return the fixity and
operator precedence of an entity. Even though precedence checking is
performed after the renaming phase, we have to be prepared to see
ambiguous identifiers here. This may happen while checking the root of
an operator definition that shadows an imported definition.
\begin{verbatim}

> opPrec :: InfixOp -> PEnv -> OpPrec
> opPrec op = prec (opName op)

> prec :: QualIdent -> PEnv -> OpPrec
> prec op env =
>   case qualLookupP op env of
>     [] -> defaultP
>     PrecInfo _ p : _ -> p

\end{verbatim}
Error messages.
\begin{verbatim}

> undefinedOperator :: Ident -> (Position,String)
> undefinedOperator op = 
>  (positionOfIdent op,
>   "no definition for " ++ name op ++ " in this scope")

> duplicatePrecedence :: Ident -> (Position,String)
> duplicatePrecedence op = 
>  (positionOfIdent op,
>   "More than one fixity declaration for " ++ name op)

> invalidParse :: String -> Ident -> QualIdent -> (Position,String)
> invalidParse what op1 op2 =
>  (positionOfIdent op1,
>   "Invalid use of " ++ what ++ " " ++ name op1 ++ " with " ++ qualName op2 ++
>   (showLine $ positionOfQualIdent op2))

> ambiguousParse :: String -> QualIdent -> QualIdent -> (Position,String)
> ambiguousParse what op1 op2 =
>  (positionOfQualIdent op1,
>   "Ambiguous use of " ++ what ++ " " ++ qualName op1 ++
>   " with " ++ qualName op2 ++ (showLine $ positionOfQualIdent op2))

\end{verbatim}
