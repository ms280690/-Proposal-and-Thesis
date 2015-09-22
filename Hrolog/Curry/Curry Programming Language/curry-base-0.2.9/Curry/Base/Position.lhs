> {-# LANGUAGE DeriveDataTypeable #-}

% -*- LaTeX -*-
% $Id: Position.lhs,v 1.2 2000/10/08 09:55:43 lux Exp $
%
% $Log: Position.lhs,v $
% Revision 1.2  2000/10/08 09:55:43  lux
% Column numbers now start at 1. If the column number is less than 1 it
% will not be shown.
%
% Revision 1.1  2000/07/23 11:03:37  lux
% Positions now implemented in a separate module.
%
%
\nwfilename{Position.lhs}
\section{Positions}
A source file position consists of a filename, a line number, and a
column number. A tab stop is assumed at every eighth column.
\begin{verbatim}

> module Curry.Base.Position where
> import Data.Generics

> -- | A pointer to the origin
> newtype SrcRef = SrcRef [Int] deriving (Data,Typeable)

-- the instances for standard classes or such that SrcRefs are invisible

> instance Show SrcRef where show _ = ""
> instance Read SrcRef where readsPrec _ s = [(noRef,s)]
> instance Eq SrcRef   where _ == _ = True
> instance Ord SrcRef  where compare _ _ = EQ

> -- | The empty source code reference
> noRef :: SrcRef
> noRef = SrcRef []

> -- | Increment a source code reference by a given number
> incSrcRef :: SrcRef -> Int -> SrcRef
> incSrcRef (SrcRef [i]) j = SrcRef [i+j]
> incSrcRef is           _ = error $
>    "internal error; increment source ref: " ++ show is

> -- | Source code positions
> data Position
>   -- | Normal source code position
>   = Position
>     { file   :: FilePath -- ^ 'FilePath' of the source file
>     , line   :: Int      -- ^ line number, beginning at 1
>     , column :: Int      -- ^ column number, beginning at 1
>     , astRef :: SrcRef   -- ^ reference to the abstract syntax tree
>     }
>   -- | Position in the abstract syntax tree
>   | AST
>     { astRef :: SrcRef -- ^ reference to the abstract syntax tree
>     }
>   -- | no position
>   | NoPos
>     deriving (Eq, Ord,Data,Typeable)

> -- | Increment the position in the abstract syntax tree
> incPosition :: Position -> Int -> Position
> incPosition NoPos _ = NoPos
> incPosition p     j = p { astRef = incSrcRef (astRef p) j }

> instance Read Position where
>   readsPrec p s =
>     [ (Position{file="",line=i,column=j,astRef=noRef},s') |
>       ((i,j),s') <- readsPrec p s]

> instance Show Position where
>   showsPrec _ Position{file=fn,line=l,column=c} =
>     (if null fn then id else shows fn . showString ", ") .
>     showString "line " . shows l .
>     (if c > 0 then showChar '.' . shows c else id)
>   showsPrec _ AST{} = id
>   showsPrec _ NoPos = id

> -- | Number of spaces for a tabulator
> tabWidth :: Int
> tabWidth = 8

> -- | Absolute first position of a file
> first :: FilePath -> Position
> first fn = Position fn 1 1 noRef

> -- | Increment a position by a number of columns
> incr :: Position -> Int -> Position
> incr p@Position{column=c} n = p{column=c + n}
> incr p _ = p

> -- | Next position to the right
> next :: Position -> Position
> next = flip incr 1

> -- | First position after the next tabulator
> tab :: Position -> Position
> tab p@Position{column=c} = p{column=c + tabWidth - (c - 1) `mod` tabWidth}
> tab p = p

> -- | First position of the next line
> nl :: Position -> Position
> nl p@Position{line=l} = p{line=l + 1, column=1}
> nl p = p

> -- | Show the line and column of the 'Position'
> showLine :: Position -> String
> showLine NoPos = ""
> showLine AST{} = ""
> showLine Position{line=l,column=c}
>   = "(line " ++ show l ++ "." ++ show c ++ ") "

> -- | Type class for data type containing source code references
> class SrcRefOf a where
>   -- | Retrieve all 'SrcRef's
>   srcRefsOf :: a -> [SrcRef]
>   srcRefsOf = (:[]) . srcRefOf
>   -- | Retrieve the first 'SrcRef'
>   srcRefOf :: a -> SrcRef
>   srcRefOf = head . srcRefsOf

> instance SrcRefOf Position where
>     srcRefOf NoPos = noRef
>     srcRefOf x     = astRef x

\end{verbatim}