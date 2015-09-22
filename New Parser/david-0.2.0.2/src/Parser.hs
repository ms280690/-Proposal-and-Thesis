{-# Language FlexibleContexts #-}

module Parser
   {-
   ( consult, consultString, parseQuery
   , program, whitespace, comment, clause, terms, term, vname
   )
   -}
   where

import Data.List(foldl',intercalate)
import Data.Char(chr,ord,isSpace,isOctDigit)
import Text.Parsec
import Control.Applicative ((<$>),(<*>),(<$),(<*),(*>))
import Control.Monad.Identity (Identity)

import Syntax
import PrettyPrint

consult :: FilePath -> IO (Either ParseError Program)
consult = fmap consultString . readFile

consultString :: String -> Either ParseError Program
consultString = parse (whitespace >> program <* eof) "(input)"

parseQuery :: String -> Either ParseError [Term]
parseQuery = parse (whitespace >> terms <* eof) "(query)"
{--
parseQuery "hello(X)"
Right [hello(X)]
--}

--parserTest :: (Show a) =>
--              ParsecT String () Identity a -> String -> Either ParseError a
--parserTest
--  :: Stream s Char =>
--     ParsecT s () a
--     -> s -> Either ParseError a
parserTest parser = parse (whitespace >> parser <* eof) "(test)"

{-========================================================================-}
  -- top-level grammar parsers
{-========================================================================-}

program ::  Stream s m Char => ParsecT s u m Program
program = many (sentence <* char '.' <* whitespace)
{--
parserTest program "hello(a)."
Right hello(a).

--}

sentence ::  Stream s m Char => ParsecT s u m Sentence
sentence
  =   Query   <$> (string "?-" *> whitespace *> body)
  <|> Command <$> (string ":-" *> whitespace *> body)
  <|> C       <$> clause
{--
parserTest sentence "?-hello(X)."
Right ?- [<{< hello(X),. >}>].

parserTest sentence "hello(X):-world(X)."
Right hello(X) :- <{< world(X),. >}>.
--}


body ::  Stream s m Char => ParsecT s u m Body
body = terms

clause ::  Stream s m Char => ParsecT s u m Clause
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

      translate ((Struct a ts), rhs1) =
         let lhs' = Struct a (arguments ts (head vars) (last vars))
             vars = map (var.("d_"++).(atomString a++).show) [0..length rhs1] -- We explicitly choose otherwise invalid variable names
             rhs' = zipWith3 translate' rhs1 vars (tail vars)
         in Clause lhs' rhs'

      eql a b = Struct (Operator "=") [a,b]
      translate' t s s0 | isList t   =  s `eql` foldr_pl cons s0 t     -- Terminal
      translate' (Struct (Operator "{}") ts) s s0 = foldr and (s `eql` s0) ts  -- Braced terms
      translate' (Struct a ts)  s s0 = Struct a (arguments ts s s0)             -- Non-Terminal

      and x y = Struct (Operator ",") [x,y]
{--
parserTest clause "hello(X):-world(X)."
Right hello(X) :- <{< world(X),. >}>


--}


struct ::  Stream s m Char => ParsecT s u m Term
struct = Struct <$> atom <*> option [] functorArgs
{--
parserTest struct "hello(X)"
Right hello(X)
--}

nonAtomStruct ::  Stream s m Char => ParsecT s u m Term
nonAtomStruct = do
  -- ensure that alternatives can find the atom if not followed by '('
  atom1 <- try $ atom <* lookAhead (char '(')
  Struct atom1 <$> functorArgs
{--


--}

list ::  Stream s m Char => ParsecT s u m Term
list = between (charWs '[') (char ']') $
         flip (foldr cons) <$> option []  terms
                           <*> option nil (charWs '|' >> term)
{--
parserTest list "[1,2,3,4]"
Right .(1,.(2,.(3,.(4,[]))))

parserTest list "[1,2,3,4.1]"
Right .(1,.(2,.(3,.(4.1,[]))))
--}

bracedList ::  Stream s m Char => ParsecT s u m Term
bracedList = between (charWs '{') (char '}') $
         Struct (Operator "{}") <$> terms

{--
parserTest bracedList "{1,2,3,4.1, hello(X),'abcd'}"
Right {}(1,2,3,4.1,hello(X),abcd)

--}

nonAtomConstant :: Stream s m Char => ParsecT s u m Term
nonAtomConstant
   =   PFloat           <$> pNan
   <|> PFloat           <$> pInf
   <|> PFloat           <$> try pFloat -- we need the try so we can get the next case
   <|> PInteger         <$> pInteger   -- this must come after float
   <|> PString          <$> pString

{--
parserTest nonAtomConstant "1"
Right 1

parserTest nonAtomConstant "1.2"
Right 1.2
--}

-- | gTerms should allow for comma etc as operators
gTerms ::  Stream s m Char => ParsecT s u m Term
gTerms = do
  let comma = FIOperator . Operator $ ","
  xxs <- sepBy1 (sepBy1 flatItem whitespace) (charWs ',')
  return $! Flat $! intercalate [comma] $ xxs
{--
parserTest gTerms "+"
Right <{< + >}>

parserTest gTerms "hello"
Right <{< hello >}>

--}


terms ::  Stream s m Char => ParsecT s u m [Term]
terms = sepBy1 term (charWs ',')
{--
parserTest terms "hello(a,b,c), world(d,e,f)"
Right [hello(a,b,c),world(d,e,f)]

parserTest terms "Hello(a,b,c), World(d,e,f)"
Right [<{< Hello,([a,,,b,,,c]) >}>,<{< World,([d,,,e,,,f]) >}>]

--}


term :: Stream s m Char => ParsecT s u m Term
term = do
  x1 <- flatItem
  xs <- many $ try $ (skipMany space >> flatItem)
  return $! xFlat (x1:xs)
  where
      xFlat [FITerm t] = t
      xFlat [FIOperator t] = Struct t []
      xFlat x = Flat x
{--
parserTest term "hello(a,b,c)"
Right hello(a,b,c)

parserTest term "Hello(a,b,c)"
Right <{< Hello,([a,,,b,,,c]) >}>


--}


flatItem :: Stream s m Char => ParsecT s u m FlatItem
flatItem
    =   xBracket <$> between (charWs '(') (char ')') gTerms
    <|> FIOperator <$> try (atom <* notFollowedBy (char '(')) -- ')'
    <|> FITerm <$> term2
    where
      -- drop redundant brackets ... should we do this at this level?
      xBracket (Flat [FIOperator atoM]) = FITerm $ Struct atoM []
      -- above.  Bracketed operators lose their magic.
      xBracket (Flat [item]) = item
      -- above.  other single things in brackets can lose the brackets.
      xBracket (Flat items)  = Bracket items
      xBracket other         = FITerm other
      -- above.  likely impossible as gTerms returns a Flat ...
{--
parserTest flatItem "+"
Right +

parserTest flatItem "-"
Right -

flatItem  "(Hello(a,b,c), World(d,e,f))"
Right ([Hello,([a,,,b,,,c]),,,World,([d,,,e,,,f])]
--}

variable :: Stream s m Char => ParsecT s u m Term
variable
  = (Wildcard <$ try (char '_' <* notFollowedBy (alphaNum <|> char '_')))
  <|> Var <$> vname
  <?> "variable"
{--
parserTest variable "X_32569"
Right X_32569

parserTest variable "_32569"
Right _32569

parserTest variable "x"
Left "(test)" (line 1, column 1):
unexpected "x"
expecting whitespace or variable
--}

atom :: Stream s m Char => ParsecT s u m Atom
atom = Operator         <$> many1 (oneOf "#$&*+-../:<=>?@\\^`~")
   <|> Operator         <$> (try . choice . map string)
                            [ ";", "is", "mod", "div" ]
   <|> Atom <$> pAtom  -- non operator atoms
                       -- we should probably distinguish
                       -- 'a' and a at this point, but we don't
   <?> "atom"
{--
parserTest atom "+"
Right +

parserTest atom "++"
Right ++

--}


functorArgs ::  Stream s m Char => ParsecT s u m [Term]
functorArgs = between (charWs '(') (char ')') terms
{--
parserTest functorArgs "(hello,world)"
Right [hello,world]

--}


term2 :: Stream s m Char => ParsecT s u m Term
term2 = variable
      <|> nonAtomStruct
      -- <|> struct
      -- atom structs are captured by flatItem and converted
      -- back to structs on unflattening provided that they
      -- do not have an operator definition.
      <|> list
      <|> bracedList
      <|> nonAtomConstant
      <|> cut <$ char '!'
      -- we don't need below as flatItem handles it
      <|> between (charWs '(') (char ')') gTerms
{--
parserTest term2 "!"
Right !


--}





{-========================================================================-}
  -- Combinators
{-========================================================================-}

skip ::  ParsecT s u m a ->  ParsecT s u m ()
skip = (>> return ())

charWs ::  Stream s m Char => Char -> ParsecT s u m Char
charWs c = char c <* whitespace

{-========================================================================-}
  -- Tokens and their subcomponents
{-========================================================================-}

whitespace ::  Stream s m Char => ParsecT s u m ()
whitespace = skipMany (comment <|> skip space <?> "whitespace")

comment ::  Stream s m Char => ParsecT s u m ()
comment = skip $ choice
   [ string "/*" >> (manyTill anyChar $ try $ string "*/")
   , char '%' >> (manyTill anyChar $ try $ skip newline <|> eof)
   ]

vname :: Stream s m Char => ParsecT s u m VariableName
vname = VariableName 0 <$>
        (    (:) <$> upper    <*> many  (alphaNum <|> char '_')
         <|> (:) <$> char '_' <*> many1 (alphaNum <|> char '_'))

pAtom :: Stream s m Char => ParsecT s u m String
pAtom = (:) <$> lower <*> many (alphaNum <|> char '_')
 <|> sQuote *> many qItem <* sQuote


pString :: Stream s m Char => ParsecT s u m String
pString = dQuote *> many sItem <* dQuote


qItem :: Stream s m Char => ParsecT s u m Char
qItem = noneOf "'\\" <|> (const '\'' <$> try (string "''")) <|> escapeChar

sItem :: Stream s m Char => ParsecT s u m Char
sItem = noneOf [dQ,escC] <|> (const dQ <$> string [dQ,dQ]) <|> escapeChar where
  dQ = '"'
  escC = '\\'

sQuote :: Stream s m Char => ParsecT s u m Char
sQuote = char '\''

dQuote :: Stream s m Char => ParsecT s u m Char
dQuote = char '"'

escapeChar :: Stream s m Char => ParsecT s u m Char
escapeChar = (char '\\' *>) $ do
  c <- anyChar
  case c of
    'a'     -> return $! chr 7
    'b'     -> return $! chr 8
    'c'     -> spaces *> qItem
    'd'     -> return $! chr 127
    'e'     -> return $! chr 27
    'f'     -> return $! chr 12
    'n'     -> return $! chr 10
    'r'     -> return $! chr 13
    'v'     -> return $! chr 11
    '^'     -> (const (chr 127) <$> char '?')
               <|> (chr . (subtract (ord 'A')) . ord) <$> upper
               <|> (chr . (subtract (ord 'a')) . ord) <$> lower
    other   
      | isSpace other   -> qItem
      | isOctDigit other   -> do
            c2 <- maybe id (:) <$> optionMaybe octDigit
            c3 <- maybe id (:) <$> optionMaybe octDigit
            return $! read . ("'\\o"++) . (c:) . c2 . c3 $ "'"
      | otherwise       -> return other

pInteger :: Stream s m Char => ParsecT s u m Integer
pInteger = 
      toInteger . ord <$> (try (string "0'") *> qItem)
  <|> do
        b <- try (pBase <* lookAhead (char '\''))
        _ <- char '\''
        m <- pMantissa b
        return $! make b m
  <|> read <$> many1 digit
  where
    pBase = do
      d1 <- oneOf "123456789"
      d2 <- case d1 of
        '1'     -> optionMaybe digit
        '2'     -> optionMaybe digit
        '3'     -> optionMaybe (oneOf "0123456")
        _other  -> return Nothing
      return $! d1 : maybe [] (:[]) d2
    pMantissa b = many1 . satisfy $ okDigit (read b)
    okDigit base c = 0 <= d && d < base where d = digitValue c
    make b = foldl' (addDigit (read b)) 0 . map (toInteger . digitValue)
    digitValue d
      | '0' <= d && d <= '9'            = ord d - ord '0'
      | 'a' <= d && d <= 'z'            = 10 + ord d - ord 'a'
      | 'A' <= d && d <= 'Z'            = 10 + ord d - ord 'A'
      | otherwise                       = error $ "Program logic (\"" ++ [d]  ++"\"\
            \ is not a digit)"
    addDigit base a b = a*base+b


pNan :: Stream s m Char => ParsecT s u m Double
pNan = const (0/0) <$> optional (oneOf "+-") <* string "nan"
pInf :: Stream s m Char => ParsecT s u m Double
pInf =  (*(1/0)) . sign1 <$> ((option '+' (oneOf "+-")) <* string "inf") where
  sign1 :: Char -> Double
  sign1 '+' = 1.0
  sign1 '-' = -1.0


pFloat :: Stream s m Char => ParsecT s u m Double
pFloat = do
  d1 <- many1 digit
  d2 <- char '.' *> many1 digit
  d3 <- option 1.0 pExponent
  return $! d3 * (read (d1++"."++d2))

pExponent :: Stream s m Char => ParsecT s u m Double
pExponent = do
  _  <- oneOf "eE"
  s1 <- option "" . count 1 . oneOf $ "+-"
  d1 <- many1 digit
  return $! read ("1E"++s1++d1)



{-========================================================================-}
  -- miscellanious
{-========================================================================-}

isList (Struct (Operator ".") [_,_]) = True
isList (Struct (Atom "[]") [])       = True
isList _                             = False


-- | This is the classical Prolog representation of chars as code points.
representChar :: Char -> Term  
representChar c = (`Struct`[]) . Atom $ [c]  

{-
-- This is the more natural representation as one-character atoms.
representChar :: Char -> Term  
representChar c = apply1 charFunctor . PInteger . toInteger . fromEnum $  c where
  apply1 x y = Struct x [y]
  charFunctor = Atom "char"
-}

-- | Conversion for the classical Prolog representation of chars.
toChar :: Term -> Maybe Char
toChar (Struct (Atom [c]) [] ) = Just c
toChar _                       = Nothing

{-
-- | Conversion for the more natural Prolog representation of chars.
toChar :: Term -> Maybe Char
toChar t
  | Struct a [b] <-t,
    Atom "char" <- a,
    PInteger x <-  b    = Just (chr x)
  | otherwise           = Nothing

-}
