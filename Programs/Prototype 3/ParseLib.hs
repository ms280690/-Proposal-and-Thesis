{-----------------------------------------------------------------------------

                 A LIBRARY OF MONADIC PARSER COMBINATORS

                              29th July 1996
                           Revised, October 1996
                       Revised again, November 1998

                 Graham Hutton               Erik Meijer
            University of Nottingham    University of Utrecht

This Haskell 98 script defines a library of parser combinators, and is taken
from sections 1-6 of our article "Monadic Parser Combinators".  Some changes
to the library have been made in the move from Gofer to Haskell:

   * Do notation is used in place of monad comprehension notation;

   * The parser datatype is defined using "newtype", to avoid the overhead
     of tagging and untagging parsers with the P constructor.

-----------------------------------------------------------------------------}

module ParseLib
   (Parser, item, papply, (+++), sat, many, many1, sepby, sepby1, chainl,
    chainl1, chainr, chainr1, ops, bracket, char, digit, lower, upper,
    letter, alphanum, string, ident, nat, int, spaces, comment, junk,
    parse, token, natural, integer, symbol, identifier, module Monad) where

import Char
import Monad


infixr 5 +++

--- The parser monad ---------------------------------------------------------

newtype Parser a   = P (String -> [(a,String)])

instance Functor Parser where
   -- map         :: (a -> b) -> (Parser a -> Parser b)
   fmap f (P p)    = P (\inp -> [(f v, out) | (v,out) <- p inp])

instance Monad Parser where
   -- return      :: a -> Parser a
   return v        = P (\inp -> [(v,inp)])

   -- >>=         :: Parser a -> (a -> Parser b) -> Parser b
   (P p) >>= f     = P (\inp -> concat [papply (f v) out | (v,out) <- p inp])

instance MonadPlus Parser where
   -- mzero            :: Parser a
   mzero                = P (\inp -> [])

   -- mplus            :: Parser a -> Parser a -> Parser a
   (P p) `mplus` (P q)  = P (\inp -> (p inp ++ q inp))

--- Other primitive parser combinators ---------------------------------------

item              :: Parser Char
item               = P (\inp -> case inp of
                                   []     -> []
                                   (x:xs) -> [(x,xs)])
{--
papply (item) "c"
[('c',"")]

papply (item) "1"
[('1',"")]

papply (item) "hello"
[('h',"ello")]

--}


force             :: Parser a -> Parser a
force (P p)        = P (\inp -> let x = p inp in
                                (fst (head x), snd (head x)) : tail x)
{--
papply (force item) "h1ello"
[('h',"1ello")]


--}


first             :: Parser a -> Parser a
first (P p)        = P (\inp -> case p inp of
                                   []     -> []
                                   (x:xs) -> [x])
{--
papply (first item) "h1ello"
[('h',"1ello")]

--}

papply            :: Parser a -> String -> [(a,String)]
papply (P p) inp   = p inp

--- Derived combinators ------------------------------------------------------

(+++)             :: Parser a -> Parser a -> Parser a
p +++ q            = first (p `mplus` q)
{--
papply ((+++) lower upper) "h1ello"
[('h',"1ello")]

papply ((+++) lower upper) "H1ello"
[('H',"1ello")]

papply ((+++) upper lower) "h1ello"
[('h',"1ello")]

papply ((+++) upper lower) "H1ello"
[('H',"1ello")]

--}


sat               :: (Char -> Bool) -> Parser Char
sat p              = do {x <- item; if p x then return x else mzero}
{--
papply (sat isAlpha) "1"
[]

papply (sat isAlpha) "c"
[('c',"")]

papply (sat isAlpha) "H"
[('H',"")]

papply (sat isAlpha) "Hell"
[('H',"ell")]

papply (sat isAlpha) "Hell0"
[('H',"ell0")]

papply (sat isAlphaNum) "Hell0"
[('H',"ell0")]

papply (sat isAlphaNum) "1Hell0"
[('1',"Hell0")]
--}



many              :: Parser a -> Parser [a]
many p             = force (many1 p +++ return [])
{--
papply (many lower) "hello"
[("hello","")]

 papply (many lower) "1hello"
[("","1hello")]

papply (many lower) "hel1lo"
[("hel","1lo")]
--}



many1             :: Parser a -> Parser [a]
many1 p            = do {x <- p; xs <- many p; return (x:xs)}

sepby             :: Parser a -> Parser b -> Parser [a]
p `sepby` sep      = (p `sepby1` sep) +++ return []
{--
papply (lower `sepby` digit) "h1e2l3l4o5"
[("hello","5")]

papply (lower `sepby` (char '-')) "h-e-l-l-o-"
[("hello","-")]
--}



sepby1            :: Parser a -> Parser b -> Parser [a]
p `sepby1` sep     = do {x <- p; xs <- many (do {sep; p}); return (x:xs)}

chainl            :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op v      = (p `chainl1` op) +++ return v
{--

--}


chainl1           :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op     = do {x <- p; rest x}
                     where
                        rest x = do {f <- op; y <- p; rest (f x y)}
                                 +++ return x

chainr            :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainr p op v      = (p `chainr1` op) +++ return v

chainr1           :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainr1` op     = do {x <- p; rest x}
                     where
                        rest x = do {f <- op; y <- p `chainr1` op; return (f x y)}
                                 +++ return x

ops               :: [(Parser a, b)] -> Parser b
ops xs             = foldr1 (+++) [do {p; return op} | (p,op) <- xs]
{--

--}

bracket           :: Parser a -> Parser b -> Parser c -> Parser b
bracket open p close = do {open; x <- p; close; return x}
{--
papply (bracket lower digit upper) "l1H"
[('1',"")]

papply (bracket (char '(') digit (char ')')) "(1)"
[('1',"")]

papply (bracket (char '(') (many digit) (char ')')) "(111)"
[("111","")]
--}


--- Useful parsers -----------------------------------------------------------

char              :: Char -> Parser Char
char x             = sat (\y -> x == y)
{--
papply (char 'c') "chello"
[('c',"hello")]

papply (char 'c') "dhello"
[]
--}

digit             :: Parser Char
digit              = sat isDigit
{--
papply (digit) "1234"
[('1',"234")]

papply (many digit) "1234"
[("1234","")]
--}

lower             :: Parser Char
lower              = sat isLower

upper             :: Parser Char
upper              = sat isUpper

letter            :: Parser Char
letter             = sat isAlpha
{--
papply (many letter) "helloH"
[("helloH","")]

--}

alphanum          :: Parser Char
alphanum           = sat isAlphaNum
{--
 papply (many alphanum) "hell13123oH"
[("hell13123oH","")]

--}

string            :: String -> Parser String
string ""          = return ""
string (x:xs)      = do {char x; string xs; return (x:xs)}
{--
papply (string "hello") "hello"
[("hello","")]

papply (string "hello") "hello1"
[("hello","1")]

papply (string "hello") "1hello1"
[]

--}


ident             :: Parser String
ident              = do {x <- lower; xs <- many alphanum; return (x:xs)}
{--
papply (string "hello") "1hello1"
[]

papply (string "hello") "hello1"
[("hello","1")]
--}

nat               :: Parser Int
nat                = do {x <- digit; return (digitToInt x)} `chainl1` return op
                     where
                        m `op` n = 10*m + n
{--
papply (nat) "1hello1"
[(1,"hello1")]

papply (nat) "123hello1"
[(123,"hello1")]
--}


int               :: Parser Int
int                = do {char '-'; n <- nat; return (-n)} +++ nat
{--
papply (int) "123hello1"
[(123,"hello1")]

papply (int) "-123hello1"
[(-123,"hello1")]
--}

--- Lexical combinators ------------------------------------------------------

spaces            :: Parser ()
spaces             = do {many1 (sat isSpace); return ()}
{--
papply (spaces) " -123hello1"
[((),"-123hello1")]

papply (spaces) " -123 hello1"
[((),"-123 hello1")]

--}


comment           :: Parser ()
comment            = do {string "--"; many (sat (\x -> x /= '\n')); return ()}
{--
papply (comment) "--comment"
[((),"")]

papply (comment) "1--comment"
[]
--}

junk              :: Parser ()
junk               = do {many (spaces +++ comment); return ()}
{--
papply (junk) " --comment"
[((),"")]

papply (junk) "--comment"
[((),"")]
--}

parse             :: Parser a -> Parser a
parse p            = do {junk; p}
{--
papply (parse digit) " --comment\n 1"
[('1',"")]

--}

token             :: Parser a -> Parser a
token p            = do {v <- p; junk; return v}
{--
papply (token digit) "1"
[('1',"")]
--}

--- Token parsers ------------------------------------------------------------

natural           :: Parser Int
natural            = token nat

integer           :: Parser Int
integer            = token int

symbol            :: String -> Parser String
symbol xs          = token (string xs)

identifier        :: [String] -> Parser String
identifier ks      = token (do {x <- ident; if not (elem x ks) then return x
                                                               else mzero})

------------------------------------------------------------------------------
