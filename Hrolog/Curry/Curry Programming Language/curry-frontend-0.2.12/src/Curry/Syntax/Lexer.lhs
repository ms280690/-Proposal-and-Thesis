
% $Id: CurryLexer.lhs,v 1.40 2004/03/04 22:39:12 wlux Exp $
%
% Copyright (c) 1999-2004, Wolfgang Lux
% See LICENSE for the full license.
%
% Modified by Martin Engelke (men@informatik.uni-kiel.de)
%
\nwfilename{CurryLexer.lhs}
\section{A Lexer for Curry}
In this section a lexer for Curry is implemented.
\begin{verbatim}
 
> module Curry.Syntax.Lexer (lexFile,lexer, Token (..), Category(..), Attributes(..)) where

> import Data.Char 
> import Data.List
> import qualified Data.Map as Map

> import Curry.Syntax.LexComb
> import Curry.Base.Position



\end{verbatim}
\paragraph{Tokens} Note that the equality and ordering instances of
\texttt{Token} disregard the attributes.
\begin{verbatim}

> data Token = Token Category Attributes

> instance Eq Token where
>   Token t1 _ == Token t2 _ = t1 == t2
> instance Ord Token where
>   Token t1 _ `compare` Token t2 _ = t1 `compare` t2

> data Category =
>   -- literals
>     CharTok | IntTok | FloatTok | IntegerTok | StringTok
>   -- identifiers
>   | Id | QId | Sym | QSym
>   -- punctuation symbols
>   | LeftParen | RightParen | Semicolon | LeftBrace | RightBrace
>   | LeftBracket | RightBracket | Comma | Underscore | Backquote
>   -- turn off layout (inserted by bbr)
>   | LeftBraceSemicolon
>   -- virtual punctation (inserted by layout)
>   | VSemicolon | VRightBrace
>   -- reserved identifiers
>   | KW_case | KW_choice | KW_data | KW_do | KW_else | KW_eval | KW_external
>   | KW_free | KW_if | KW_import | KW_in | KW_infix | KW_infixl | KW_infixr
>   | KW_let | KW_module | KW_newtype | KW_of | KW_rigid | KW_then | KW_type
>   | KW_where
>   -- reserved operators
>   | At | Colon | DotDot | DoubleColon | Equals | Backslash | Bar
>   | LeftArrow | RightArrow | Tilde | Binds
>   -- special identifiers
>   | Id_as | Id_ccall | Id_forall | Id_hiding | Id_interface | Id_primitive
>   | Id_qualified
>   -- special operators
>   | Sym_Dot | Sym_Minus | Sym_MinusDot
>   -- end-of-file token
>   | EOF
>   -- comments (only for full lexer) inserted by men & bbr
>   | LineComment | NestedComment 
>   deriving (Eq,Ord)

\end{verbatim}
There are different kinds of attributes associated with the tokens.
Most attributes simply save the string corresponding to the token.
However, for qualified identifiers, we also record the list of module
qualifiers. The values corresponding to a literal token are properly
converted already. To simplify the creation and extraction of
attribute values we make use of records.
\begin{verbatim}

> data Attributes =
>     NoAttributes
>   | CharAttributes{ cval :: Char, original :: String}
>   | IntAttributes{ ival :: Int , original :: String}
>   | FloatAttributes{ fval :: Double, original :: String}
>   | IntegerAttributes{ intval :: Integer, original :: String}
>   | StringAttributes{ sval :: String, original :: String}
>   | IdentAttributes{ modul :: [String], sval :: String}

> instance Show Attributes where
>   showsPrec _ NoAttributes = showChar '_'
>   showsPrec _ (CharAttributes cval _) = shows cval
>   showsPrec _ (IntAttributes ival _) = shows ival
>   showsPrec _ (FloatAttributes fval _) = shows fval
>   showsPrec _ (IntegerAttributes intval _) = shows intval
>   showsPrec _ (StringAttributes sval _) = shows sval
>   showsPrec _ (IdentAttributes mIdent ident) =
>     showString ("`" ++ concat (intersperse "." (mIdent ++ [ident])) ++ "'")

\end{verbatim}
The following functions can be used to construct tokens with
specific attributes.
\begin{verbatim}

> tok :: Category -> Token
> tok t = Token t NoAttributes

> idTok :: Category -> [String] -> String -> Token
> idTok t mIdent ident = Token t IdentAttributes{ modul = mIdent, sval = ident }

> charTok :: Char -> String -> Token
> charTok c o = Token CharTok CharAttributes{ cval = c, original = o }

> intTok :: Int -> String -> Token
> intTok base digits =
>   Token IntTok IntAttributes{ ival = convertIntegral base digits,
>                               original = digits}

> floatTok :: String -> String -> Int -> String -> Token
> floatTok mant frac exp rest =
>   Token FloatTok FloatAttributes{ fval = convertFloating mant frac exp, 
>                                   original = mant++"."++frac++rest}
 
> integerTok :: Integer -> String -> Token
> integerTok base digits =
>   Token IntegerTok
>         IntegerAttributes{intval = (convertIntegral base digits) :: Integer,
>                           original = digits}

> stringTok :: String -> String -> Token
> stringTok cs o = Token StringTok StringAttributes{ sval = cs, original = o }

> lineCommentTok :: String -> Token
> lineCommentTok s = Token LineComment StringAttributes{ sval = s, original = s}

> nestedCommentTok :: String -> Token
> nestedCommentTok s = Token NestedComment StringAttributes{ sval = s, original = s }

\end{verbatim}
The \texttt{Show} instance of \texttt{Token} is designed to display
all tokens in their source representation.
\begin{verbatim}

> instance Show Token where
>   showsPrec _ (Token Id a) = showString "identifier " . shows a
>   showsPrec _ (Token QId a) = showString "qualified identifier " . shows a
>   showsPrec _ (Token Sym a) = showString "operator " . shows a
>   showsPrec _ (Token QSym a) = showString "qualified operator " . shows a
>   showsPrec _ (Token IntTok a) = showString "integer " . shows a
>   showsPrec _ (Token FloatTok a) = showString "float " . shows a
>   showsPrec _ (Token CharTok a) = showString "character " . shows a
>   showsPrec _ (Token IntegerTok a) = showString "integer " . shows a
>   showsPrec _ (Token StringTok a) = showString "string " . shows a
>   showsPrec _ (Token LeftParen _) = showString "`('"
>   showsPrec _ (Token RightParen _) = showString "`)'"
>   showsPrec _ (Token Semicolon _) = showString "`;'"
>   showsPrec _ (Token LeftBrace _) = showString "`{'"
>   showsPrec _ (Token RightBrace _) = showString "`}'"
>   showsPrec _ (Token LeftBracket _) = showString "`['"
>   showsPrec _ (Token RightBracket _) = showString "`]'"
>   showsPrec _ (Token Comma _) = showString "`,'"
>   showsPrec _ (Token Underscore _) = showString "`_'"
>   showsPrec _ (Token Backquote _) = showString "``'"
>   showsPrec _ (Token VSemicolon _) =
>     showString "`;' (inserted due to layout)"
>   showsPrec _ (Token VRightBrace _) =
>     showString "`}' (inserted due to layout)"
>   showsPrec _ (Token At _) = showString "`@'"
>   showsPrec _ (Token Colon _) = showString "`:'"
>   showsPrec _ (Token DotDot _) = showString "`..'"
>   showsPrec _ (Token DoubleColon _) = showString "`::'"
>   showsPrec _ (Token Equals _) = showString "`='"
>   showsPrec _ (Token Backslash _) = showString "`\\'"
>   showsPrec _ (Token Bar _) = showString "`|'"
>   showsPrec _ (Token LeftArrow _) = showString "`<-'"
>   showsPrec _ (Token RightArrow _) = showString "`->'"
>   showsPrec _ (Token Tilde _) = showString "`~'"
>   showsPrec _ (Token Binds _) = showString "`:='"
>   showsPrec _ (Token Sym_Dot _) = showString "operator `.'"
>   showsPrec _ (Token Sym_Minus _) = showString "operator `-'"
>   showsPrec _ (Token Sym_MinusDot _) = showString "operator `-.'"
>   showsPrec _ (Token KW_case _) = showString "`case'"
>   showsPrec _ (Token KW_choice _) = showString "`choice'"
>   showsPrec _ (Token KW_data _) = showString "`data'"
>   showsPrec _ (Token KW_do _) = showString "`do'"
>   showsPrec _ (Token KW_else _) = showString "`else'"
>   showsPrec _ (Token KW_eval _) = showString "`eval'"
>   showsPrec _ (Token KW_external _) = showString "`external'"
>   showsPrec _ (Token KW_free _) = showString "`free'"
>   showsPrec _ (Token KW_if _) = showString "`if'"
>   showsPrec _ (Token KW_import _) = showString "`import'"
>   showsPrec _ (Token KW_in _) = showString "`in'"
>   showsPrec _ (Token KW_infix _) = showString "`infix'"
>   showsPrec _ (Token KW_infixl _) = showString "`infixl'"
>   showsPrec _ (Token KW_infixr _) = showString "`infixr'"
>   showsPrec _ (Token KW_let _) = showString "`let'"
>   showsPrec _ (Token KW_module _) = showString "`module'"
>   showsPrec _ (Token KW_newtype _) = showString "`newtype'"
>   showsPrec _ (Token KW_of _) = showString "`of'"
>   showsPrec _ (Token KW_rigid _) = showString "`rigid'"
>   showsPrec _ (Token KW_then _) = showString "`then'"
>   showsPrec _ (Token KW_type _) = showString "`type'"
>   showsPrec _ (Token KW_where _) = showString "`where'"
>   showsPrec _ (Token Id_as _) = showString "identifier `as'"
>   showsPrec _ (Token Id_ccall _) = showString "identifier `ccall'"
>   showsPrec _ (Token Id_forall _) = showString "identifier `forall'"
>   showsPrec _ (Token Id_hiding _) = showString "identifier `hiding'"
>   showsPrec _ (Token Id_interface _) = showString "identifier `interface'"
>   showsPrec _ (Token Id_primitive _) = showString "identifier `primitive'"
>   showsPrec _ (Token Id_qualified _) = showString "identifier `qualified'"
>   showsPrec _ (Token EOF _) = showString "<end-of-file>"
>   showsPrec _ (Token LineComment a) = shows a
>   showsPrec _ (Token NestedComment a) = shows a

\end{verbatim}
Tables for reserved operators and identifiers
\begin{verbatim}

> reserved_ops, reserved_and_special_ops :: Map.Map String Category
> reserved_ops = Map.fromList [
>     ("@",  At),
>     ("::", DoubleColon),
>     ("..", DotDot),
>     ("=",  Equals),
>     ("\\", Backslash),
>     ("|",  Bar),
>     ("<-", LeftArrow),
>     ("->", RightArrow),
>     ("~",  Tilde),
>     (":=", Binds)
>   ]
> reserved_and_special_ops = foldr (uncurry Map.insert) reserved_ops [
>     (":",  Colon),
>     (".",  Sym_Dot),
>     ("-",  Sym_Minus),
>     ("-.", Sym_MinusDot)
>   ]

> reserved_ids, reserved_and_special_ids :: Map.Map String Category
> reserved_ids = Map.fromList [
>     ("case",     KW_case),
>     ("choice",   KW_choice),
>     ("data",     KW_data),
>     ("do",       KW_do),
>     ("else",     KW_else),
>     ("eval",     KW_eval),
>     ("external", KW_external),
>     ("free",     KW_free),
>     ("if",       KW_if),
>     ("import",   KW_import),
>     ("in",       KW_in),
>     ("infix",    KW_infix),
>     ("infixl",   KW_infixl),
>     ("infixr",   KW_infixr),
>     ("let",      KW_let),
>     ("module",   KW_module),
>     ("newtype",  KW_newtype),
>     ("of",       KW_of),
>     ("rigid",    KW_rigid),
>     ("then",     KW_then),
>     ("type",     KW_type),
>     ("where",    KW_where)
>   ]
> reserved_and_special_ids = foldr (uncurry Map.insert) reserved_ids [
>     ("as",        Id_as),
>     ("ccall",     Id_ccall),
>     ("forall",    Id_forall),
>     ("hiding",    Id_hiding),
>     ("interface", Id_interface),
>     ("primitive", Id_primitive),
>     ("qualified", Id_qualified)
>   ]

\end{verbatim}
Character classes
\begin{verbatim}

> isIdent, isSym, isOctit, isHexit :: Char -> Bool
> isIdent c = isAlphaNum c || c `elem` "'_"
> isSym c = c `elem` "~!@#$%^&*+-=<>:?./|\\"
> isOctit c = c >= '0' && c <= '7'
> isHexit c = isDigit c || c >= 'A' && c <= 'F' || c >= 'a' && c <= 'f'

inserted for full lexing (men&bbr)

> isLineComment, isNestedComment :: String -> Bool
> isLineComment ('-':'-':_) = True
> isLineComment _ = False
> isNestedComment ('{':'-':s) = True
> isNestedComment _ = False


\end{verbatim}
Lexing functions
\begin{verbatim}

> type SuccessP a = Position -> Token -> P a
> type FailP a = Position -> String -> P a

> lexFile :: P [(Position,Token)]
> lexFile = fullLexer tokens failP
>   where tokens p t@(Token c _)
>           | c == EOF = returnP [(p,t)]
>           | otherwise = lexFile `thenP` returnP . ((p,t):)

> lexer :: SuccessP a -> FailP a -> P a
> lexer success fail = skipBlanks
>   where -- skipBlanks moves past whitespace and comments
>         skipBlanks p [] bol = success p (tok EOF) p [] bol
>         skipBlanks p ('\t':s) bol = skipBlanks (tab p) s bol
>         skipBlanks p ('\n':s) bol = skipBlanks (nl p) s True
>         skipBlanks p ('-':'-':s) bol =
>           skipBlanks (nl p) (tail' (dropWhile (/= '\n') s)) True
>         skipBlanks p ('{':'-':s) bol =
>           nestedComment p skipBlanks fail (incr p 2) s bol
>         skipBlanks p (c:s) bol
>           | isSpace c = skipBlanks (next p) s bol
>           | otherwise =
>               (if bol then lexBOL else lexToken) success fail p (c:s) bol
>         tail' [] = []
>         tail' (_:tl) = tl

> fullLexer :: SuccessP a -> FailP a -> P a
> fullLexer success fail = skipBlanks
>   where -- skipBlanks moves past whitespace 
>         skipBlanks p [] bol = success p (tok EOF) p [] bol
>         skipBlanks p ('\t':s) bol = skipBlanks (tab p) s bol
>         skipBlanks p ('\n':s) bol = skipBlanks (nl p) s True
>         skipBlanks p s@('-':'-':_) bol = lexLineComment success p s bol
>         skipBlanks p s@('{':'-':_) bol =
>           lexNestedComment 0 id p success fail p s bol
>         skipBlanks p (c:s) bol
>           | isSpace c = skipBlanks (next p) s bol
>           | otherwise =
>               (if bol then lexBOL else lexToken) success fail p (c:s) bol
>         tail' [] = []
>         tail' (_:tl) = tl

> lexLineComment :: SuccessP a -> P a
> lexLineComment success p s = case break (=='\n') s of
>   (comment,rest) -> success p (lineCommentTok comment) (incr p (length comment)) rest
 
> lexNestedComment :: Int -> (String -> String) -> 
>                     Position -> SuccessP a -> FailP a -> P a
> lexNestedComment 1 comment p0 success fail p ('-':'}':s) = 
>   success p0 (nestedCommentTok (comment "-}") ) (incr p 2) s 
> lexNestedComment n comment p0 success fail p ('{':'-':s) = 
>   lexNestedComment (n+1) (comment . ("{-"++)) p0 success fail (incr p 2) s
> lexNestedComment n comment p0 success fail p ('-':'}':s) = 
>   lexNestedComment (n-1) (comment . ("-}"++)) p0 success fail (incr p 2) s
> lexNestedComment n comment p0 success fail p (c@'\t':s) = 
>   lexNestedComment n (comment . (c:)) p0 success fail (tab p) s
> lexNestedComment n comment p0 success fail p (c@'\n':s) = 
>   lexNestedComment n (comment . (c:)) p0 success fail (nl p) s
> lexNestedComment n comment p0 success fail p (c:s) = 
>   lexNestedComment n (comment . (c:)) p0 success fail (next p) s
> lexNestedComment n comment p0 success fail p "" = 
>   fail p0 "Unterminated nested comment" p []

> nestedComment :: Position -> P a -> FailP a -> P a
> nestedComment p0 success fail p ('-':'}':s) = success (incr p 2) s
> nestedComment p0 success fail p ('{':'-':s) =
>   nestedComment p (nestedComment p0 success fail) fail (incr p 2) s
> nestedComment p0 success fail p ('\t':s) =
>   nestedComment p0 success fail (tab p) s
> nestedComment p0 success fail p ('\n':s) =
>   nestedComment p0 success fail (nl p) s
> nestedComment p0 success fail p (_:s) =
>   nestedComment p0 success fail (next p) s
> nestedComment p0 success fail p [] =
>   fail p0 "Unterminated nested comment at end-of-file" p []


> lexBOL :: SuccessP a -> FailP a -> P a
> lexBOL success fail p s _ [] = lexToken success fail p s False []
> lexBOL success fail p s _ ctxt@(n:rest)
>   | col < n = success p (tok VRightBrace) p s True rest
>   | col == n = success p (tok VSemicolon) p s False ctxt
>   | otherwise = lexToken success fail p s False ctxt
>   where col = column p

> lexToken :: SuccessP a -> FailP a -> P a
> lexToken success fail p [] = success p (tok EOF) p []
> lexToken success fail p (c:s)
>   | c == '(' = token LeftParen
>   | c == ')' = token RightParen
>   | c == ',' = token Comma
>   | c == ';' = token Semicolon
>   | c == '[' = token LeftBracket
>   | c == ']' = token RightBracket
>   | c == '_' = token Underscore
>   | c == '`' = token Backquote
>   | c == '{' = lexLeftBrace (token LeftBrace) (next p) (success p) s 
>   | c == '}' = \bol -> token RightBrace bol . drop 1
>   | c == '\'' = lexChar p success fail (next p) s
>   | c == '\"' = lexString p success fail (next p) s
>   | isAlpha c = lexIdent (success p) p (c:s)
>   | isSym c = lexSym (success p) p (c:s)
>   | isDigit c = lexNumber (success p) p (c:s)
>   | otherwise = fail p ("Illegal character " ++ show c) p s
>   where token t = success p (tok t) (next p) s

> lexIdent :: (Token -> P a) -> P a
> lexIdent cont p s =
>   maybe (lexOptQual cont (token Id) [ident]) (cont . token)
>         (Map.lookup ident reserved_and_special_ids)
>         (incr p (length ident)) rest
>   where (ident,rest) = span isIdent s
>         token t = idTok t [] ident

> lexSym :: (Token -> P a) -> P a
> lexSym cont p s =
>   cont (idTok (maybe Sym id (Map.lookup sym reserved_and_special_ops)) [] sym)
>        (incr p (length sym)) rest
>   where (sym,rest) = span isSym s

> lexLeftBrace leftBrace _ _       []    = leftBrace
> lexLeftBrace leftBrace p cont (c:s) 
>   | c==';'    = cont (tok LeftBraceSemicolon) (next p) s
>   | otherwise = leftBrace

\end{verbatim}
{\em Note:} the function \texttt{lexOptQual} has been extended to provide
the qualified use of the Prelude list operators and tuples.
\begin{verbatim}

> lexOptQual :: (Token -> P a) -> Token -> [String] -> P a
> lexOptQual cont token mIdent p ('.':c:s)
>   | isAlpha c = lexQualIdent cont identCont mIdent (next p) (c:s)
>   | isSym c = lexQualSym cont identCont mIdent (next p) (c:s)
>   | c=='(' || c=='[' 
>     = lexQualPreludeSym cont token identCont mIdent (next p) (c:s)
>  where identCont _ _ = cont token p ('.':c:s)
> lexOptQual cont token mIdent p s = cont token p s

> lexQualIdent :: (Token -> P a) -> P a -> [String] -> P a
> lexQualIdent cont identCont mIdent p s =
>   maybe (lexOptQual cont (idTok QId mIdent ident) (mIdent ++ [ident]))
>         (const identCont)
>         (Map.lookup ident reserved_ids)
>         (incr p (length ident)) rest
>   where (ident,rest) = span isIdent s

> lexQualSym :: (Token -> P a) -> P a -> [String] -> P a
> lexQualSym cont identCont mIdent p s =
>   maybe (cont (idTok QSym mIdent sym)) (const identCont)
>         (Map.lookup sym reserved_ops)
>         (incr p (length sym)) rest
>   where (sym,rest) = span isSym s


> lexQualPreludeSym :: (Token -> P a) -> Token -> P a -> [String] -> P a
> lexQualPreludeSym cont _ identCont mIdent p ('[':']':rest) =
>   cont (idTok QId mIdent "[]") (incr p 2) rest
> lexQualPreludeSym cont _ identCont mIdent p ('(':rest)
>   | not (null rest') && head rest'==')' 
>   = cont (idTok QId mIdent ('(':tup++")")) (incr p (length tup+2)) (tail rest')
>   where (tup,rest') = span (==',') rest
> lexQualPreludeSym cont token _ _ p s =  cont token p s


\end{verbatim}
{\em Note:} since Curry allows an unlimited range of integer numbers,
read numbers must be converted to Haskell type \texttt{Integer}.
\begin{verbatim}

> lexNumber :: (Token -> P a) -> P a
> lexNumber cont p ('0':c:s)
>   | c `elem` "oO" = lexOctal cont nullCont (incr p 2) s
>   | c `elem` "xX" = lexHexadecimal cont nullCont (incr p 2) s
>   where nullCont _ _ = cont (intTok 10 "0") (next p) (c:s)
> lexNumber cont p s
>     = lexOptFraction cont (integerTok 10 digits) digits
>                      (incr p (length digits)) rest
>   where (digits,rest) = span isDigit s
>         num           = (read digits) :: Integer

> lexOctal :: (Token -> P a) -> P a -> P a
> lexOctal cont nullCont p s
>   | null digits = nullCont undefined undefined
>   | otherwise = cont (integerTok 8 digits) (incr p (length digits)) rest
>   where (digits,rest) = span isOctit s

> lexHexadecimal :: (Token -> P a) -> P a -> P a
> lexHexadecimal cont nullCont p s
>   | null digits = nullCont undefined undefined
>   | otherwise = cont (integerTok 16 digits) (incr p (length digits)) rest
>   where (digits,rest) = span isHexit s

> lexOptFraction :: (Token -> P a) -> Token -> String -> P a
> lexOptFraction cont _ mant p ('.':c:s)
>   | isDigit c = lexOptExponent cont (floatTok mant frac 0 "") mant frac
>                                (incr p (length frac+1)) rest
>   where (frac,rest) = span isDigit (c:s)
> lexOptFraction cont token mant p (c:s)
>   | c `elem` "eE" = lexSignedExponent cont intCont mant "" [c] (next p) s
>   where intCont _ _ = cont token p (c:s)
> lexOptFraction cont token _ p s = cont token p s

> lexOptExponent :: (Token -> P a) -> Token -> String -> String -> P a
> lexOptExponent cont token mant frac p (c:s)
>   | c `elem` "eE" = lexSignedExponent cont floatCont mant frac [c] (next p) s
>   where floatCont _ _ = cont token p (c:s)
> lexOptExponent cont token mant frac p s = cont token p s

> lexSignedExponent :: (Token -> P a) -> P a -> String -> String -> String -> P a
> lexSignedExponent cont floatCont mant frac e p ('+':c:s)
>   | isDigit c = lexExponent cont mant frac (e++"+") id (next p) (c:s)
> lexSignedExponent cont floatCont mant frac e p ('-':c:s)
>   | isDigit c = lexExponent cont mant frac (e++"-") negate (next p) (c:s)
> lexSignedExponent cont floatCont mant frac e p (c:s)
>   | isDigit c = lexExponent cont mant frac e id p (c:s)
> lexSignedExponent cont floatCont mant frac e p s = floatCont p s

> lexExponent :: (Token -> P a) -> String -> String -> String -> (Int -> Int) -> P a
> lexExponent cont mant frac e expSign p s =
>   cont (floatTok mant frac exp (e++digits)) (incr p (length digits)) rest
>   where (digits,rest) = span isDigit s
>         exp = expSign (convertIntegral 10 digits)

> lexChar :: Position -> SuccessP a -> FailP a -> P a
> lexChar p0 success fail p [] = fail p0 "Illegal character constant" p []
> lexChar p0 success fail p (c:s)
>   | c == '\\' = lexEscape p (lexCharEnd p0 success fail) fail (next p) s
>   | c == '\n' = fail p0 "Illegal character constant" p (c:s)
>   | c == '\t' = lexCharEnd p0 success fail c "\t" (tab p) s
>   | otherwise = lexCharEnd p0 success fail c [c] (next p) s

> lexCharEnd :: Position -> SuccessP a -> FailP a -> Char -> String -> P a
> lexCharEnd p0 success fail c o p ('\'':s) = success p0 (charTok c o) (next p) s
> lexCharEnd p0 success fail c o p s =
>   fail p0 "Improperly terminated character constant" p s

> lexString :: Position -> SuccessP a -> FailP a -> P a
> lexString p0 success fail = lexStringRest p0 success fail "" id

> lexStringRest :: Position -> SuccessP a -> FailP a -> String -> (String -> String) -> P a
> lexStringRest p0 success fail s0 so p [] = 
>   fail p0 "Improperly terminated string constant" p []
> lexStringRest p0 success fail s0 so p (c:s)
>   | c == '\\' =
>       lexStringEscape p (lexStringRest p0 success fail) fail s0 so (next p) s
>   | c == '\"' = success p0 (stringTok (reverse s0) (so "")) (next p) s
>   | c == '\n' = fail p0 "Improperly terminated string constant" p []
>   | c == '\t' = lexStringRest p0 success fail (c:s0) (so . (c:)) (tab p) s
>   | otherwise = lexStringRest p0 success fail (c:s0) (so . (c:)) (next p) s

> lexStringEscape ::  Position -> (String -> (String -> String) -> P a) -> FailP a -> 
>                                  String -> (String -> String) -> P a
> lexStringEscape p0 success fail s0 so p [] = lexEscape p0 undefined fail p []
> lexStringEscape p0 success fail s0 so p (c:s)
>   | c == '&' = success s0 (so . ("\\&"++)) (next p) s
>   | isSpace c = lexStringGap (success s0) fail so p (c:s)
>   | otherwise = lexEscape p0 (\ c' s' -> success (c':s0) (so . (s'++))) fail p (c:s)

> lexStringGap :: ((String -> String) -> P a) -> FailP a -> (String -> String) -> P a
> lexStringGap success fail so p [] = fail p "End of file in string gap" p []
> lexStringGap success fail so p (c:s)
>   | c == '\\' = success (so . (c:)) (next p) s
>   | c == '\t' = lexStringGap success fail (so . (c:)) (tab p) s
>   | c == '\n' = lexStringGap success fail (so . (c:)) (nl p) s
>   | isSpace c = lexStringGap success fail (so . (c:)) (next p) s
>   | otherwise = fail p ("Illegal character in string gap " ++ show c) p s

> lexEscape :: Position -> (Char -> String -> P a) -> FailP a -> P a
> lexEscape p0 success fail p ('a':s) = success '\a' "\\a" (next p) s
> lexEscape p0 success fail p ('b':s) = success '\b' "\\b" (next p) s
> lexEscape p0 success fail p ('f':s) = success '\f' "\\f" (next p) s
> lexEscape p0 success fail p ('n':s) = success '\n' "\\n" (next p) s
> lexEscape p0 success fail p ('r':s) = success '\r' "\\r" (next p) s
> lexEscape p0 success fail p ('t':s) = success '\t' "\\t" (next p) s
> lexEscape p0 success fail p ('v':s) = success '\v' "\\v" (next p) s
> lexEscape p0 success fail p ('\\':s) = success '\\' "\\\\" (next p) s
> lexEscape p0 success fail p ('"':s) = success '\"' "\\\"" (next p) s
> lexEscape p0 success fail p ('\'':s) = success '\'' "\\\'" (next p) s
> lexEscape p0 success fail p ('^':c:s)
>   | isUpper c || c `elem` "@[\\]^_" =
>       success (chr (ord c `mod` 32)) ("\\^"++[c]) (incr p 2) s
> lexEscape p0 success fail p ('o':c:s)
>   | isOctit c = numEscape p0 success fail 8 isOctit ("\\o"++) (next p) (c:s)
> lexEscape p0 success fail p ('x':c:s)
>   | isHexit c = numEscape p0 success fail 16 isHexit ("\\x"++) (next p) (c:s)
> lexEscape p0 success fail p (c:s)
>   | isDigit c = numEscape p0 success fail 10 isDigit ("\\"++) p (c:s)
> lexEscape p0 success fail p s = asciiEscape p0 success fail p s

> asciiEscape :: Position -> (Char -> String -> P a) -> FailP a -> P a
> asciiEscape p0 success fail p ('N':'U':'L':s) = success '\NUL' "\\NUL" (incr p 3) s
> asciiEscape p0 success fail p ('S':'O':'H':s) = success '\SOH' "\\SOH" (incr p 3) s
> asciiEscape p0 success fail p ('S':'T':'X':s) = success '\STX' "\\STX" (incr p 3) s
> asciiEscape p0 success fail p ('E':'T':'X':s) = success '\ETX' "\\ETX" (incr p 3) s
> asciiEscape p0 success fail p ('E':'O':'T':s) = success '\EOT' "\\EOT" (incr p 3) s
> asciiEscape p0 success fail p ('E':'N':'Q':s) = success '\ENQ' "\\ENQ" (incr p 3) s
> asciiEscape p0 success fail p ('A':'C':'K':s) = success '\ACK' "\\ACK" (incr p 3) s 
> asciiEscape p0 success fail p ('B':'E':'L':s) = success '\BEL' "\\BEL" (incr p 3) s
> asciiEscape p0 success fail p ('B':'S':s) = success '\BS' "\\BS" (incr p 2) s
> asciiEscape p0 success fail p ('H':'T':s) = success '\HT' "\\HT" (incr p 2) s
> asciiEscape p0 success fail p ('L':'F':s) = success '\LF' "\\LF" (incr p 2) s
> asciiEscape p0 success fail p ('V':'T':s) = success '\VT' "\\VT" (incr p 2) s
> asciiEscape p0 success fail p ('F':'F':s) = success '\FF' "\\FF" (incr p 2) s
> asciiEscape p0 success fail p ('C':'R':s) = success '\CR' "\\CR" (incr p 2) s
> asciiEscape p0 success fail p ('S':'O':s) = success '\SO' "\\SO" (incr p 2) s
> asciiEscape p0 success fail p ('S':'I':s) = success '\SI' "\\SI" (incr p 2) s
> asciiEscape p0 success fail p ('D':'L':'E':s) = success '\DLE' "\\DLE" (incr p 3) s 
> asciiEscape p0 success fail p ('D':'C':'1':s) = success '\DC1' "\\DC1" (incr p 3) s
> asciiEscape p0 success fail p ('D':'C':'2':s) = success '\DC2' "\\DC2" (incr p 3) s
> asciiEscape p0 success fail p ('D':'C':'3':s) = success '\DC3' "\\DC3" (incr p 3) s
> asciiEscape p0 success fail p ('D':'C':'4':s) = success '\DC4' "\\DC4" (incr p 3) s
> asciiEscape p0 success fail p ('N':'A':'K':s) = success '\NAK' "\\NAK" (incr p 3) s
> asciiEscape p0 success fail p ('S':'Y':'N':s) = success '\SYN' "\\SYN" (incr p 3) s
> asciiEscape p0 success fail p ('E':'T':'B':s) = success '\ETB' "\\ETB" (incr p 3) s
> asciiEscape p0 success fail p ('C':'A':'N':s) = success '\CAN' "\\CAN" (incr p 3) s 
> asciiEscape p0 success fail p ('E':'M':s) = success '\EM' "\\EM" (incr p 2) s
> asciiEscape p0 success fail p ('S':'U':'B':s) = success '\SUB' "\\SUB" (incr p 3) s
> asciiEscape p0 success fail p ('E':'S':'C':s) = success '\ESC' "\\ESC" (incr p 3) s
> asciiEscape p0 success fail p ('F':'S':s) = success '\FS' "\\FS" (incr p 2) s
> asciiEscape p0 success fail p ('G':'S':s) = success '\GS' "\\GS" (incr p 2) s
> asciiEscape p0 success fail p ('R':'S':s) = success '\RS' "\\RS" (incr p 2) s
> asciiEscape p0 success fail p ('U':'S':s) = success '\US' "\\US" (incr p 2) s
> asciiEscape p0 success fail p ('S':'P':s) = success '\SP' "\\SP" (incr p 2) s
> asciiEscape p0 success fail p ('D':'E':'L':s) = success '\DEL' "\\DEL" (incr p 3) s
> asciiEscape p0 success fail p s = fail p0 "Illegal escape sequence" p s

> numEscape :: Position -> (Char -> String -> P a) -> FailP a -> Int
>           -> (Char -> Bool) -> (String -> String) -> P a
> numEscape p0 success fail b isDigit so p s
>   | n >= min && n <= max = success (chr n) (so digits) (incr p (length digits)) rest
>   | otherwise = fail p0 "Numeric escape out-of-range" p s
>   where (digits,rest) = span isDigit s
>         n = convertIntegral b digits
>         min = ord minBound
>         max = ord maxBound

\end{verbatim}
