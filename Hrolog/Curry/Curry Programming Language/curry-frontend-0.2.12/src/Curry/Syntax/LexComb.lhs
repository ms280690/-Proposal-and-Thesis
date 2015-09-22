% -*- LaTeX -*-
% $Id: LexComb.lhs,v 1.16 2004/01/20 16:44:14 wlux Exp $
%
% Copyright (c) 1999-2004, Wolfgang Lux
% See LICENSE for the full license.
%
\nwfilename{LexComb.lhs}
\section{Lexing combinators}
The module \texttt{LexComb} provides the basic types and combinators
to implement the lexers. The combinators use continuation passing code
in a monadic style. The first argument of the continuation function is
the current position, and the second is the string to be parsed. The third
argument is a flag which signals the lexer that it is lexing the
beginning of a line and therefore has to check for layout tokens. The
fourth argument is a stack of indentations that is used to handle
nested layout groups.
\begin{verbatim}

> module Curry.Syntax.LexComb where

> import Data.Char

> import Curry.Base.MessageMonad
> import Curry.Base.Position

> infixl 1 `thenP`, `thenP_`

> type Indent = Int
> type Context = [Indent]
> type P a = Position -> String -> Bool -> Context -> MsgMonad a

> parse :: P a -> FilePath -> String -> MsgMonad a
> parse p fn s = p (first fn) s False []

\end{verbatim}
Monad functions for the lexer.
\begin{verbatim}

> returnP :: a -> P a
> returnP x _ _ _ _ = return x

> thenP :: P a -> (a -> P b) -> P b
> thenP lex k pos s bol ctxt = lex pos s bol ctxt >>= \x -> k x pos s bol ctxt

> thenP_ :: P a -> P b -> P b
> p1 `thenP_` p2 = p1 `thenP` \_ -> p2

> failP :: Position -> String -> P a
> failP pos msg _ _ _ _ = failWith (parseError pos msg)

> closeP0 :: P a -> P (P a)
> closeP0 lex pos s bol ctxt = return (\_ _ _ _ -> lex pos s bol ctxt)

> closeP1 :: (a -> P b) -> P (a -> P b)
> closeP1 f pos s bol ctxt = return (\x _ _ _ _ -> f x pos s bol ctxt)

> parseError :: Position -> String -> String
> parseError p what = "\n" ++ show p ++ ": " ++ what

\end{verbatim}
Combinators that handle layout.
\begin{verbatim}

> pushContext :: Int -> P a -> P a
> pushContext col cont pos s bol ctxt = cont pos s bol (col:ctxt)

> popContext :: P a -> P a
> popContext cont pos s bol (_:ctxt) = cont pos s bol ctxt
> popContext cont pos s bol [] =
>    error "parse error: popping layout from empty context stack. \
>          \Perhaps you have inserted too many '}'?"

\end{verbatim}
Conversions from strings into numbers.
\begin{verbatim}

> convertSignedIntegral :: Num a => a -> String -> a
> convertSignedIntegral b ('+':s) = convertIntegral b s
> convertSignedIntegral b ('-':s) = - convertIntegral b s
> convertSignedIntegral b s = convertIntegral b s

> convertIntegral :: Num a => a -> String -> a
> convertIntegral b = foldl op 0
>   where m `op` n | isDigit n = b * m + fromIntegral (ord n - ord0)
>                  | isUpper n = b * m + fromIntegral (ord n - ordA)
>                  | otherwise = b * m + fromIntegral (ord n - orda)
>         ord0 = ord '0'
>         ordA = ord 'A' - 10
>         orda = ord 'a' - 10

> convertSignedFloating :: Fractional a => String -> String -> Int -> a
> convertSignedFloating ('+':m) f e = convertFloating m f e
> convertSignedFloating ('-':m) f e = - convertFloating m f e
> convertSignedFloating m f e = convertFloating m f e

> convertFloating :: Fractional a => String -> String -> Int -> a
> convertFloating m f e
>   | e' == 0 = m'
>   | e' > 0  = m' * 10^e'
>   | otherwise = m' / 10^(-e')
>   where m' = convertIntegral 10 (m ++ f)
>         e' = e - length f

\end{verbatim}
