module Parser
   ( consult, consultString, parseQuery
   , program, whitespace, comment, clause, terms, term, bottom, vname
   ) where

import Text.Parsec
import Text.Parsec.Expr hiding (Assoc(..))
import qualified Text.Parsec.Expr as Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Control.Applicative ((<$>),(<*>),(<$),(<*))
import Data.Functor.Identity  -- Added later was not there as a result of which the signatures of certain parser functions  
                              -- were throwing errors


import Syntax

{--
Like consult in Prolog.
If the program is parsed correctly, then each predicate is added to the list of results.  
--}
consult :: FilePath -> IO (Either ParseError Program)
consult = fmap consultString . readFile
{--
consult "/home/mehul/Dropbox/PrologPrograms/shoeStore.pl"

Right [

"append([], X, X)",
"append([H1|T1], L2, [H1|T2]) :- append(T1, L2, T2)",

"right(X, Y, L) :- append(_, [X,Y|_], L)",

"len([], 0)",
"len([_|T], N) :- len(T, X), N is X+1",

"start(S) :- 
len(S, 4), 
S=[[Shoe1,Store1],[Shoe2,Store2],[Shoe3,Store3],[Shoe4,Store4]], 
member(Store1, [ffs,hhs,tsps,ts]), 
member(Store2, [ffs,hhs,tsps,ts]), 
member(Store3, [ffs,hhs,tsps,ts]), 
member(Store4, [ffs,hhs,tsps,ts]), 
member(Shoe1, [ee,ff,pp,ss]), 
member(Shoe2, [ee,ff,pp,ss]), 
member(Shoe3, [ee,ff,pp,ss]), 
member(Shoe4, [ee,ff,pp,ss]), 
not(Store1=Store2), 
not(Store1=Store3), 
not(Store1=Store4), 
not(Store2=Store3), 
not(Store2=Store4), 
not(Store3=Store4), 
not(Shoe1=Shoe2), 
not(Shoe1=Shoe3), 
not(Shoe1=Shoe4), 
not(Shoe2=Shoe3), 
not(Shoe2=Shoe4), 
not(Shoe3=Shoe4), 
member([ff,hhs], S), 
not(right([pp,_], [_,ts], S)), S=[_,[_,ffs],_,_], 
S=[[_,tsps],_,_,[ss,_]]"]
--}


consultString :: String -> Either ParseError Program
consultString = parse (whitespace >> program <* eof) "(input)"
{--
consultString "hello."
Right ["hello"]
--}

parseQuery :: String -> Either ParseError [Term]
parseQuery = parse (whitespace >> terms <* eof) "(query)"
{--
parseQuery "hello(X)"
Right [hello(X)]
--}

program :: ParsecT String () Data.Functor.Identity.Identity [Clause]
program = many (clause <* char '.' <* whitespace)

whitespace :: ParsecT String () Data.Functor.Identity.Identity ()
whitespace = skipMany (comment <|> skip space <?> "")

comment :: ParsecT String () Data.Functor.Identity.Identity ()
comment = skip $ choice
   [ string "/*" >> (manyTill anyChar $ try $ string "*/")
   , char '%' >> (manyTill anyChar $ try $ skip newline <|> eof)
   ]

skip :: ParsecT String () Data.Functor.Identity.Identity a -> ParsecT String () Data.Functor.Identity.Identity ()
skip = (>> return ())

clause :: ParsecT String () Data.Functor.Identity.Identity Clause
clause = do t <- struct <* whitespace
            dcg t <|> normal t
   where
      normal t = do
            ts <- option [] $ do string ":-" <* whitespace
                                 terms
            return (Clause t ts)

      dcg t = do
            string "-->" <* whitespace
            ts <- terms
            return (translate (t,ts))

      translate ((Struct a ts), rhs) =
         let lhs' = Struct a (arguments ts (head vars) (last vars))
             vars = map (var.("d_"++).(a++).show) [0..length rhs] -- We explicitly choose otherwise invalid variable names
             rhs' = zipWith3 translate' rhs vars (tail vars)
         in Clause lhs' rhs'

      translate' t s s0 | isList t   = Struct "=" [ s, foldr_pl cons s0 t ]     -- Terminal
      translate' t@(Struct "{}" ts) s s0 = foldr and (Struct "=" [ s, s0 ]) ts  -- Braced terms
      translate' (Struct a ts)  s s0 = Struct a (arguments ts s s0)             -- Non-Terminal

      and x y = Struct "," [x,y]


isList :: Term -> Bool
isList (Struct "." [_,_]) = True
isList (Struct "[]" [])   = True
isList _                  = False

terms :: ParsecT String () Identity [Term]
terms = sepBy1 termWithoutConjunction (charWs ',')

term :: ParsecT String () Identity Term
term = term' False

termWithoutConjunction :: ParsecT String () Identity Term
termWithoutConjunction = term' True

term' :: Bool -> ParsecT String () Identity Term
term' ignoreConjunction = buildExpressionParser (reverse $ map (map toParser) $ hierarchy ignoreConjunction) (bottom <* whitespace)

bottom :: ParsecT String () Identity Term
bottom = variable
      <|> struct
      <|> list
      <|> stringLiteral
      <|> cut <$ char '!'
      <|> Struct "{}" <$> between (charWs '{') (char '}') terms
      <|> between (charWs '(') (char ')') term

toParser :: Syntax.Operator -> Parsec.Operator String u Identity Term
toParser (PrefixOp name)      = Prefix (reservedOp name >> return (\t -> Struct name [t]))
toParser (InfixOp assoc name) = Infix  (reservedOp name >> return (\t1 t2 -> Struct name [t1, t2]))
                                       (case assoc of AssocLeft  -> Parsec.AssocLeft
                                                      AssocRight -> Parsec.AssocRight)

reservedOp :: String -> ParsecT String u Identity ()
reservedOp = P.reservedOp $ P.makeTokenParser $ emptyDef
   { P.opStart = oneOf ";,<=>\\i*+m@"
   , P.opLetter = oneOf "=.:<+"
   , P.reservedOpNames = operatorNames
   , P.caseSensitive = True
   }

charWs :: Char -> ParsecT String () Identity Char
charWs c = char c <* whitespace

operatorNames :: [[Char]]
operatorNames = [ ";", ",", "<", "=..", "=:=", "=<", "=", ">=", ">", "\\=", "is", "*", "+", "-", "\\", "mod", "div", "\\+" ]

variable :: ParsecT String u Identity Term
variable = (Wildcard <$ try (char '_' <* notFollowedBy (alphaNum <|> char '_')))
       <|> Var <$> vname
       <?> "variable"

vname :: ParsecT String u Identity VariableName
vname = VariableName 0 <$> ((:) <$> upper    <*> many  (alphaNum <|> char '_') <|>
                            (:) <$> char '_' <*> many1 (alphaNum <|> char '_'))

atom :: ParsecT String u Identity [Char]
atom = (:) <$> lower <*> many (alphaNum <|> char '_')
   <|> many1 digit
   <|> choice (map string operatorNames)
   <|> many1 (oneOf "#$&*+/.<=>\\^~")
   <|> between (char '\'') (char '\'') (many (noneOf "'"))
   <?> "atom"

struct :: ParsecT String () Identity Term
struct = do a <- atom
            ts <- option [] $ between (charWs '(') (char ')') terms
            return (Struct a ts)

list :: ParsecT String () Identity Term
list = between (charWs '[') (char ']') $
         flip (foldr cons) <$> option []  terms
                           <*> option nil (charWs '|' >> term)

stringLiteral :: ParsecT String u Identity Term
stringLiteral = foldr cons nil . map representChar <$> between (char '"') (char '"') (try (many (noneOf "\"")))

representChar :: Enum a => a -> Term
representChar c = Struct (show (fromEnum c)) [] -- This is the classical Prolog representation of chars as code points.
--representChar c = Struct [c] [] -- This is the more natural representation as one-character atoms.
--representChar c = Struct "char" [Struct (show (fromEnum c)) []] -- This is a representation as tagged code points.
--toChar :: Term -> Maybe Char
--toChar (Struct "char" [Struct (toEnum . read->c) []]) = Just c
--toChar _                                              = Nothing

