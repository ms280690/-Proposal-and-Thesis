> {-# LANGUAGE DeriveDataTypeable #-}

% $Id: IL.lhs,v 1.18 2003/10/28 05:43:38 wlux Exp $
%
% Copyright (c) 1999-2003 Wolfgang Lux
% See LICENSE for the full license.
%
% Modified by Martin Engelke (men@informatik.uni-kiel.de)
%
\nwfilename{IL.lhs}
\section{The intermediate language}
The module \texttt{IL} defines the intermediate language which will be
compiled into abstract machine code. The intermediate language removes
a lot of syntactic sugar from the Curry source language.  Top-level
declarations are restricted to data type and function definitions. A
newtype definition serves mainly as a hint to the backend that it must
provide an auxiliary function for partial applications of the
constructor. \textbf{Newtype constructors must not occur in patterns
and may be used in expressions only as partial applications.}

Type declarations use a de-Bruijn indexing scheme (starting at 0) for
type variables. In the type of a function, all type variables are
numbered in the order of their occurence from left to right, i.e., a
type \texttt{(Int -> b) -> (a,b) -> c -> (a,c)} is translated into the
type (using integer numbers to denote the type variables)
\texttt{(Int -> 0) -> (1,0) -> 2 -> (1,2)}.

Pattern matching in an equation is handled via flexible and rigid
\texttt{Case} expressions. Overlapping rules are translated with the
help of \texttt{Or} expressions. The intermediate language has three
kinds of binding expressions, \texttt{Exist} expressions introduce a
new logical variable, \texttt{Let} expression support a single
non-recursive variable binding, and \texttt{Letrec} expressions
introduce multiple variables with recursive initializer expressions.
The intermediate language explicitly distinguishes (local) variables
and (global) functions in expressions.

\em{Note:} this modified version uses haskell type \texttt{Integer}
instead of \texttt{Int} for representing integer values. This provides
an unlimited range of integer constants in Curry programs.
\begin{verbatim}

> module IL.Type where

> import Data.Generics
> import Curry.Base.Ident
> import Curry.Base.Position (SrcRef(..))

> data Module = Module ModuleIdent [ModuleIdent] [Decl] deriving (Eq,Show)

> data Decl = 
>     DataDecl QualIdent Int [ConstrDecl [Type]]
>   | NewtypeDecl QualIdent Int (ConstrDecl Type)
>   | FunctionDecl QualIdent [Ident] Type Expression
>   | ExternalDecl QualIdent CallConv String Type
>   deriving (Eq,Show)

> data ConstrDecl a = ConstrDecl QualIdent a deriving (Eq,Show)
> data CallConv = Primitive | CCall deriving (Eq,Show)

> data Type =
>     TypeConstructor QualIdent [Type]
>   | TypeVariable Int
>   | TypeArrow Type Type
>   deriving (Eq,Show, Typeable, Data)

> data Literal = Char SrcRef Char | Int SrcRef Integer | Float SrcRef Double deriving (Eq,Show)

> data ConstrTerm =
>   -- literal patterns
>     LiteralPattern Literal
>   -- constructors
>   | ConstructorPattern QualIdent [Ident]
>   -- default
>   | VariablePattern Ident
>   deriving (Eq,Show)

> data Expression =
>   -- literal constants
>     Literal Literal
>   -- variables, functions, constructors
>   | Variable Ident | Function QualIdent Int | Constructor QualIdent Int
>   -- applications
>   | Apply Expression Expression
>   -- case expressions
>   | Case SrcRef Eval Expression [Alt]
>   -- non-determinisismic or
>   | Or Expression Expression
>   -- binding forms
>   | Exist Ident Expression
>   | Let Binding Expression
>   | Letrec [Binding] Expression
>   deriving (Eq,Show)

> data Eval = Rigid | Flex deriving (Eq,Show)
> data Alt = Alt ConstrTerm Expression deriving (Eq,Show)
> data Binding = Binding Ident Expression deriving (Eq,Show)

\end{verbatim}

> instance SrcRefOf ConstrTerm where
>   srcRefOf (LiteralPattern l) = srcRefOf l
>   srcRefOf (ConstructorPattern i _) = srcRefOf i
>   srcRefOf (VariablePattern i) = srcRefOf i


> instance SrcRefOf Literal where
>   srcRefOf (Char s _)   = s
>   srcRefOf (Int s _)    = s
>   srcRefOf (Float s _)  = s  


