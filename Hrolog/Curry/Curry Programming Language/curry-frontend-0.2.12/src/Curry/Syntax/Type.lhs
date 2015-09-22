> {-# LANGUAGE DeriveDataTypeable #-}

% $Id: CurrySyntax.lhs,v 1.43 2004/02/15 22:10:31 wlux Exp $
%
% Copyright (c) 1999-2004, Wolfgang Lux
% See LICENSE for the full license.
%
% Modified by Martin Engelke (men@informatik.uni-kiel.de)
%
\nwfilename{CurrySyntax.lhs}
\section{The Parse Tree}
This module provides the necessary data structures to maintain the
parsed representation of a Curry program.

\em{Note:} this modified version uses haskell type \texttt{Integer}
instead of \texttt{Int} for representing integer values. This allows
an unlimited range of integer constants in Curry programs.
\begin{verbatim}

> module Curry.Syntax.Type where

> import Curry.Base.Ident
> import Curry.Base.Position
> import Data.Generics
> import Control.Monad.State

\end{verbatim}
\paragraph{Modules}
\begin{verbatim}

> data Module = Module ModuleIdent (Maybe ExportSpec) [Decl] 
>  deriving (Eq,Show,Read,Typeable,Data)

> data ExportSpec = Exporting Position [Export] deriving (Eq,Show,Read,Typeable,Data)
> data Export =
>     Export         QualIdent                  -- f/T
>   | ExportTypeWith QualIdent [Ident]          -- T(C1,...,Cn)
>   | ExportTypeAll  QualIdent                  -- T(..)
>   | ExportModule   ModuleIdent
>   deriving (Eq,Show,Read,Typeable,Data)

\end{verbatim}
\paragraph{Module declarations}
\begin{verbatim}

> data ImportSpec =
>     Importing Position [Import]
>   | Hiding Position [Import]
>   deriving (Eq,Show,Read,Typeable,Data)
> data Import =
>     Import         Ident            -- f/T
>   | ImportTypeWith Ident [Ident]    -- T(C1,...,Cn)
>   | ImportTypeAll  Ident            -- T(..)
>   deriving (Eq,Show,Read,Typeable,Data)

> data Decl =
>     ImportDecl Position ModuleIdent Qualified (Maybe ModuleIdent)
>                (Maybe ImportSpec)
>   | InfixDecl Position Infix Integer [Ident]
>   | DataDecl Position Ident [Ident] [ConstrDecl]
>   | NewtypeDecl Position Ident [Ident] NewConstrDecl
>   | TypeDecl Position Ident [Ident] TypeExpr
>   | TypeSig Position [Ident] TypeExpr
>   | EvalAnnot Position [Ident] EvalAnnotation
>   | FunctionDecl Position Ident [Equation]
>   | ExternalDecl Position CallConv (Maybe String) Ident TypeExpr
>   | FlatExternalDecl Position [Ident]
>   | PatternDecl Position ConstrTerm Rhs
>   | ExtraVariables Position [Ident]
>   deriving (Eq,Show,Read,Typeable,Data)

> data ConstrDecl =
>     ConstrDecl Position [Ident] Ident [TypeExpr]
>   | ConOpDecl Position [Ident] TypeExpr Ident TypeExpr
>   deriving (Eq,Show,Read,Typeable,Data)
> data NewConstrDecl =
>   NewConstrDecl Position [Ident] Ident TypeExpr
>   deriving (Eq,Show,Read,Typeable,Data)

> type Qualified = Bool
> data Infix = InfixL | InfixR | Infix deriving (Eq,Show,Read,Typeable,Data)
> data EvalAnnotation = EvalRigid | EvalChoice deriving (Eq,Show,Read,Typeable,Data)
> data CallConv = CallConvPrimitive | CallConvCCall deriving (Eq,Show,Read,Typeable,Data)

\end{verbatim}
\paragraph{Module interfaces}
Interface declarations are restricted to type declarations and signatures. 
Note that an interface function declaration additionaly contains the 
function arity (= number of parameters) in order to generate
correct FlatCurry function applications.
\begin{verbatim}

> data Interface = Interface ModuleIdent [IDecl] deriving (Eq,Show,Read,Typeable,Data)

> data IDecl =
>     IImportDecl Position ModuleIdent
>   | IInfixDecl Position Infix Integer QualIdent
>   | HidingDataDecl Position Ident [Ident] 
>   | IDataDecl Position QualIdent [Ident] [Maybe ConstrDecl]
>   | INewtypeDecl Position QualIdent [Ident] NewConstrDecl
>   | ITypeDecl Position QualIdent [Ident] TypeExpr
>   | IFunctionDecl Position QualIdent Int TypeExpr
>   deriving (Eq,Show,Read,Typeable,Data)

\end{verbatim}
\paragraph{Types}
\begin{verbatim}

> data TypeExpr =
>     ConstructorType QualIdent [TypeExpr]
>   | VariableType Ident
>   | TupleType [TypeExpr]
>   | ListType TypeExpr
>   | ArrowType TypeExpr TypeExpr
>   | RecordType [([Ident],TypeExpr)] (Maybe TypeExpr) 
>     -- {l1 :: t1,...,ln :: tn | r}
>   deriving (Eq,Show,Read,Typeable,Data)

\end{verbatim}
\paragraph{Functions}
\begin{verbatim}

> data Equation = Equation Position Lhs Rhs deriving (Eq,Show,Read,Typeable,Data)
> data Lhs =
>     FunLhs Ident [ConstrTerm]
>   | OpLhs ConstrTerm Ident ConstrTerm
>   | ApLhs Lhs [ConstrTerm]
>   deriving (Eq,Show,Read,Typeable,Data)
> data Rhs =
>     SimpleRhs Position Expression [Decl]
>   | GuardedRhs [CondExpr] [Decl]
>   deriving (Eq,Show,Read,Typeable,Data)
> data CondExpr = CondExpr Position Expression Expression deriving (Eq,Show,Read,Typeable,Data)

> flatLhs :: Lhs -> (Ident,[ConstrTerm])
> flatLhs lhs = flat lhs []
>   where flat (FunLhs f ts) ts' = (f,ts ++ ts')
>         flat (OpLhs t1 op t2) ts = (op,t1:t2:ts)
>         flat (ApLhs lhs ts) ts' = flat lhs (ts ++ ts')

\end{verbatim}
\paragraph{Literals} The \texttt{Ident} argument of an \texttt{Int}
literal is used for supporting ad-hoc polymorphism on integer
numbers. An integer literal can be used either as an integer number or
as a floating-point number depending on its context. The compiler uses
the identifier of the \texttt{Int} literal for maintaining its type.
\begin{verbatim}

> data Literal =
>     Char SrcRef Char                         -- should be Int to handle Unicode
>   | Int Ident Integer
>   | Float SrcRef Double
>   | String SrcRef String                     -- should be [Int] to handle Unicode
>   deriving (Eq,Show,Read,Typeable,Data)

> mk' :: ([SrcRef] -> a) -> a
> mk' = ($[])

> mk :: (SrcRef -> a) -> a
> mk = ($noRef)

> mkInt :: Integer -> Literal
> mkInt i = mk (\r -> Int (addPositionIdent (AST  r) anonId) i) 

\end{verbatim}
\paragraph{Patterns}
\begin{verbatim}

> data ConstrTerm =
>     LiteralPattern Literal
>   | NegativePattern Ident Literal
>   | VariablePattern Ident
>   | ConstructorPattern QualIdent [ConstrTerm]
>   | InfixPattern ConstrTerm QualIdent ConstrTerm
>   | ParenPattern ConstrTerm
>   | TuplePattern SrcRef [ConstrTerm]
>   | ListPattern [SrcRef] [ConstrTerm]
>   | AsPattern Ident ConstrTerm
>   | LazyPattern SrcRef ConstrTerm
>   | FunctionPattern QualIdent [ConstrTerm]
>   | InfixFuncPattern ConstrTerm QualIdent ConstrTerm
>   | RecordPattern [Field ConstrTerm] (Maybe ConstrTerm)  
>         -- {l1 = p1, ..., ln = pn}  oder {l1 = p1, ..., ln = pn | p}
>   deriving (Eq,Show,Read,Typeable,Data)

\end{verbatim}
\paragraph{Expressions}
\begin{verbatim}

> data Expression =
>     Literal Literal
>   | Variable QualIdent
>   | Constructor QualIdent
>   | Paren Expression
>   | Typed Expression TypeExpr
>   | Tuple SrcRef [Expression]
>   | List [SrcRef] [Expression]
>   | ListCompr SrcRef Expression [Statement] -- the ref corresponds to the main list  
>   | EnumFrom Expression
>   | EnumFromThen Expression Expression
>   | EnumFromTo Expression Expression
>   | EnumFromThenTo Expression Expression Expression
>   | UnaryMinus Ident Expression
>   | Apply Expression Expression
>   | InfixApply Expression InfixOp Expression
>   | LeftSection Expression InfixOp
>   | RightSection InfixOp Expression
>   | Lambda SrcRef [ConstrTerm] Expression
>   | Let [Decl] Expression
>   | Do [Statement] Expression
>   | IfThenElse SrcRef Expression Expression Expression
>   | Case SrcRef Expression [Alt]
>   | RecordConstr [Field Expression]            -- {l1 = e1,...,ln = en}
>   | RecordSelection Expression Ident           -- e -> l
>   | RecordUpdate [Field Expression] Expression -- {l1 := e1,...,ln := en | e}
>   deriving (Eq,Show,Read,Typeable,Data)

> data InfixOp = InfixOp QualIdent | InfixConstr QualIdent deriving (Eq,Show,Read,Typeable,Data)

> data Statement =
>     StmtExpr SrcRef Expression
>   | StmtDecl [Decl]
>   | StmtBind SrcRef ConstrTerm Expression
>   deriving (Eq,Show,Read,Typeable,Data)

> data Alt = Alt Position ConstrTerm Rhs deriving (Eq,Show,Read,Typeable,Data)

> data Field a = Field Position Ident a deriving (Eq, Show,Read,Typeable,Data)

> fieldLabel :: Field a -> Ident
> fieldLabel (Field _ l _) = l

> fieldTerm :: Field a -> a
> fieldTerm (Field _ _ t) = t

> field2Tuple :: Field a -> (Ident,a)
> field2Tuple (Field _ l t) = (l,t)

> opName :: InfixOp -> QualIdent
> opName (InfixOp op) = op
> opName (InfixConstr c) = c

\end{verbatim}

> instance SrcRefOf ConstrTerm where
>   srcRefOf (LiteralPattern l) = srcRefOf l
>   srcRefOf (NegativePattern i _) = srcRefOf i
>   srcRefOf (VariablePattern i) = srcRefOf i
>   srcRefOf (ConstructorPattern i _) = srcRefOf i
>   srcRefOf (InfixPattern _ i _) = srcRefOf i
>   srcRefOf (ParenPattern c) = srcRefOf c
>   srcRefOf (TuplePattern s _) = s
>   srcRefOf (ListPattern s _) = error "list pattern has several source refs"
>   srcRefOf (AsPattern i _) = srcRefOf i
>   srcRefOf (LazyPattern s _) = s
>   srcRefOf (FunctionPattern i _) = srcRefOf i
>   srcRefOf (InfixFuncPattern _ i _) = srcRefOf i

> instance SrcRefOf Literal where
>   srcRefOf (Char s _)   = s
>   srcRefOf (Int i _)    = srcRefOf i
>   srcRefOf (Float s _)  = s
>   srcRefOf (String s _) = s

---------------------------
-- add source references
---------------------------

> type M a = a -> State Int a
> 
> addSrcRefs :: Module -> Module
> addSrcRefs x = evalState (addRef x) 0
>   where 
>     addRef :: Data a' => M a' 
>     addRef = down `extM` addRefPos   
>                   `extM` addRefSrc   
>                   `extM` addRefIdent
>                   `extM` addRefListPat
>                   `extM` addRefListExp
>       where
>         down :: Data a' => M a'
>         down = gmapM addRef
> 
>         addRefPos :: M [SrcRef]
>         addRefPos _ = liftM (:[]) next
> 
>         addRefSrc :: M SrcRef
>         addRefSrc _ = next
> 
>         addRefIdent :: M Ident
>         addRefIdent ident = liftM (flip addRefId ident) next
>
>         addRefListPat :: M ConstrTerm
>         addRefListPat (ListPattern _ ts) = do
>           liftM (uncurry ListPattern) (addRefList ts)
>         addRefListPat ct = gmapM addRef ct
>   
>         addRefListExp :: M Expression
>         addRefListExp (List _ ts) = do
>           liftM (uncurry List) (addRefList ts)
>         addRefListExp ct = gmapM addRef ct
>   
>         addRefList :: Data a' => [a'] -> State Int ([SrcRef],[a'])
>         addRefList ts = do
>           i <- next
>           let add t = do t' <- addRef t;j <- next; return (j,t')
>           ists <- sequence (map add ts)
>           let (is,ts') = unzip ists
>           return (i:is,ts')
>         
>         next :: State Int SrcRef
>         next = do
>           i <- get
>           put $! i+1
>           return (SrcRef [i])
