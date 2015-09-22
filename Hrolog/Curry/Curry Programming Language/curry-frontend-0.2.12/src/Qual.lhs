
% $Id: Qual.lhs,v 1.18 2004/02/15 22:10:36 wlux Exp $
%
% Copyright (c) 2001-2004, Wolfgang Lux
% See LICENSE for the full license.
%
% Modified by Martin Engelke (men@informatik.uni-kiel.de)
%
\nwfilename{Qual.lhs}
\section{Proper Qualification}
After checking the module and before starting the translation into the
intermediate language, the compiler properly qualifies all
constructors and (global) functions occurring in a pattern or
expression such that their module prefix matches the module of their
definition. This is done also for functions and constructors declared
in the current module. Only functions and variables declared in local
declarations groups as well as function arguments remain unchanged.

\em{Note:} The modified version also qualifies type constructors
\begin{verbatim}

> module Qual(qual) where

> import Curry.Base.Ident
> import Curry.Syntax

> import Base
> import TopEnv

> qual :: ModuleIdent -> ValueEnv -> [Decl] -> [Decl]
> qual m tyEnv ds = map (qualDecl m tyEnv) ds

> qualDecl :: ModuleIdent -> ValueEnv -> Decl -> Decl
> qualDecl m tyEnv (FunctionDecl p f eqs) =
>   FunctionDecl p f (map (qualEqn m tyEnv) eqs)
> qualDecl m tyEnv (PatternDecl p t rhs) =
>   PatternDecl p (qualTerm m tyEnv t) (qualRhs m tyEnv rhs)
> qualDecl _ _ d = d

> qualEqn :: ModuleIdent -> ValueEnv -> Equation -> Equation
> qualEqn m tyEnv (Equation p lhs rhs) =
>   Equation p (qualLhs m tyEnv lhs) (qualRhs m tyEnv rhs)

> qualLhs :: ModuleIdent -> ValueEnv -> Lhs -> Lhs
> qualLhs m tyEnv (FunLhs f ts) = FunLhs f (map (qualTerm m tyEnv) ts)
> qualLhs m tyEnv (OpLhs t1 op t2) =
>   OpLhs (qualTerm m tyEnv t1) op (qualTerm m tyEnv t2)
> qualLhs m tyEnv (ApLhs lhs ts) =
>   ApLhs (qualLhs m tyEnv lhs) (map (qualTerm m tyEnv) ts)

> qualTerm :: ModuleIdent -> ValueEnv -> ConstrTerm -> ConstrTerm
> qualTerm _ _ (LiteralPattern l) = LiteralPattern l
> qualTerm _ _ (NegativePattern op l) = NegativePattern op l
> qualTerm _ _ (VariablePattern v) = VariablePattern v
> qualTerm m tyEnv (ConstructorPattern c ts) =
>   ConstructorPattern (qualIdent m tyEnv c) (map (qualTerm m tyEnv) ts)
> qualTerm m tyEnv (InfixPattern t1 op t2) =
>   InfixPattern (qualTerm m tyEnv t1) 
>                (qualIdent m tyEnv op) 
>                (qualTerm m tyEnv t2)
> qualTerm m tyEnv (ParenPattern t) = ParenPattern (qualTerm m tyEnv t)
> qualTerm m tyEnv (TuplePattern p ts) = TuplePattern p (map (qualTerm m tyEnv) ts)
> qualTerm m tyEnv (ListPattern p ts) = ListPattern p (map (qualTerm m tyEnv) ts)
> qualTerm m tyEnv (AsPattern v t) = AsPattern v (qualTerm m tyEnv t)
> qualTerm m tyEnv (LazyPattern p t) = LazyPattern p (qualTerm m tyEnv t)
> qualTerm m tyEnv (FunctionPattern f ts) =
>   FunctionPattern (qualIdent m tyEnv f) (map (qualTerm m tyEnv) ts)
> qualTerm m tyEnv (InfixFuncPattern t1 op t2) =
>   InfixFuncPattern (qualTerm m tyEnv t1) 
>		     (qualIdent m tyEnv op) 
>	             (qualTerm m tyEnv t2)
> qualTerm m tyEnv (RecordPattern fs rt) =
>   RecordPattern (map (qualFieldPattern m tyEnv) fs)
>	          (maybe Nothing (Just . qualTerm m tyEnv) rt)

> qualFieldPattern :: ModuleIdent -> ValueEnv -> Field ConstrTerm
>	           -> Field ConstrTerm
> qualFieldPattern m tyEnv (Field p l t) = Field p l (qualTerm m tyEnv t)

> qualRhs :: ModuleIdent -> ValueEnv -> Rhs -> Rhs
> qualRhs m tyEnv (SimpleRhs p e ds) =
>   SimpleRhs p (qualExpr m tyEnv e) (map (qualDecl m tyEnv) ds) 
> qualRhs m tyEnv (GuardedRhs es ds) =
>   GuardedRhs (map (qualCondExpr m tyEnv) es) (map (qualDecl m tyEnv) ds)

> qualCondExpr :: ModuleIdent -> ValueEnv -> CondExpr -> CondExpr
> qualCondExpr m tyEnv (CondExpr p g e) =
>   CondExpr p (qualExpr m tyEnv g) (qualExpr m tyEnv e)

> qualExpr :: ModuleIdent -> ValueEnv -> Expression -> Expression
> qualExpr _ _ (Literal l) = Literal l
> qualExpr m tyEnv (Variable v) = Variable (qualIdent m tyEnv v)
> qualExpr m tyEnv (Constructor c) = Constructor (qualIdent m tyEnv c)
> qualExpr m tyEnv (Paren e) = Paren (qualExpr m tyEnv e)
> qualExpr m tyEnv (Typed e ty) = Typed (qualExpr m tyEnv e) ty
> qualExpr m tyEnv (Tuple p es) = Tuple p (map (qualExpr m tyEnv) es)
> qualExpr m tyEnv (List p es) = List p (map (qualExpr m tyEnv) es)
> qualExpr m tyEnv (ListCompr p e qs) =
>   ListCompr p (qualExpr m tyEnv e) (map (qualStmt m tyEnv) qs)
> qualExpr m tyEnv (EnumFrom e) = EnumFrom (qualExpr m tyEnv e)
> qualExpr m tyEnv (EnumFromThen e1 e2) =
>   EnumFromThen (qualExpr m tyEnv e1) (qualExpr m tyEnv e2)
> qualExpr m tyEnv (EnumFromTo e1 e2) =
>   EnumFromTo (qualExpr m tyEnv e1) (qualExpr m tyEnv e2)
> qualExpr m tyEnv (EnumFromThenTo e1 e2 e3) =
>   EnumFromThenTo (qualExpr m tyEnv e1) 
>                  (qualExpr m tyEnv e2) 
>                  (qualExpr m tyEnv e3)
> qualExpr m tyEnv (UnaryMinus op e) = UnaryMinus op (qualExpr m tyEnv e)
> qualExpr m tyEnv (Apply e1 e2) = 
>   Apply (qualExpr m tyEnv e1) (qualExpr m tyEnv e2)
> qualExpr m tyEnv (InfixApply e1 op e2) =
>   InfixApply (qualExpr m tyEnv e1) (qualOp m tyEnv op) (qualExpr m tyEnv e2)
> qualExpr m tyEnv (LeftSection e op) =
>   LeftSection (qualExpr m tyEnv e) (qualOp m tyEnv op)
> qualExpr m tyEnv (RightSection op e) =
>   RightSection (qualOp m tyEnv op) (qualExpr m tyEnv e)
> qualExpr m tyEnv (Lambda r ts e) =
>   Lambda r (map (qualTerm m tyEnv) ts) (qualExpr m tyEnv e)
> qualExpr m tyEnv (Let ds e) = 
>   Let (map (qualDecl m tyEnv) ds) (qualExpr m tyEnv e)
> qualExpr m tyEnv (Do sts e) = 
>   Do (map (qualStmt m tyEnv) sts) (qualExpr m tyEnv e)
> qualExpr m tyEnv (IfThenElse r e1 e2 e3) =
>   IfThenElse r (qualExpr m tyEnv e1) 
>              (qualExpr m tyEnv e2) 
>              (qualExpr m tyEnv e3)
> qualExpr m tyEnv (Case r e alts) =
>   Case r (qualExpr m tyEnv e) (map (qualAlt m tyEnv) alts)
> qualExpr m tyEnv (RecordConstr fs) =
>   RecordConstr (map (qualFieldExpr m tyEnv) fs)
> qualExpr m tyEnv (RecordSelection e l) =
>   RecordSelection (qualExpr m tyEnv e) l
> qualExpr m tyEnv (RecordUpdate fs e) =
>   RecordUpdate (map (qualFieldExpr m tyEnv) fs) (qualExpr m tyEnv e)

> qualStmt :: ModuleIdent -> ValueEnv -> Statement -> Statement
> qualStmt m tyEnv (StmtExpr p e) = StmtExpr p (qualExpr m tyEnv e)
> qualStmt m tyEnv (StmtBind p t e) =
>   StmtBind p (qualTerm m tyEnv t) (qualExpr m tyEnv e)
> qualStmt m tyEnv (StmtDecl ds) = StmtDecl (map (qualDecl m tyEnv) ds)

> qualAlt :: ModuleIdent -> ValueEnv -> Alt -> Alt
> qualAlt m tyEnv (Alt p t rhs) = 
>   Alt p (qualTerm m tyEnv t) (qualRhs m tyEnv rhs)

> qualFieldExpr :: ModuleIdent -> ValueEnv -> Field Expression
>	        -> Field Expression
> qualFieldExpr m tyEnv (Field p l e) = Field p l (qualExpr m tyEnv e)

> qualOp :: ModuleIdent -> ValueEnv -> InfixOp -> InfixOp
> qualOp m tyEnv (InfixOp op) = InfixOp (qualIdent m tyEnv op)
> qualOp m tyEnv (InfixConstr op) = InfixConstr (qualIdent m tyEnv op)

> qualIdent :: ModuleIdent -> ValueEnv -> QualIdent -> QualIdent
> qualIdent m tyEnv x
>   | not (isQualified x) && uniqueId (unqualify x) /= 0 = x
>   | otherwise =
>       case (qualLookupValue x tyEnv) of
>         [y] -> origName y
>         vs  -> case (qualLookupValue (qualQualify m x) tyEnv) of
>                  [y] -> origName y
>                  _ -> qualQualify m x -- internalError ("qualIdent: " ++ show x)

\end{verbatim}
