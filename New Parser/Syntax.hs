{-# LANGUAGE DeriveDataTypeable, ViewPatterns, ScopedTypeVariables #-}
module Syntax
   ( Term(..), var, cut
   , Clause(..), rhs
   , Atom(..), atomString
   , FlatItem(..)
   , VariableName(..), Goal, Program
   , Body
   , Sentence(..)
   , cons, nil, foldr_pl
   , arguments -- FIXME Should not be exposed
   , Operator(..), Assoc(..)
   )
where

import Data.Generics (Data(..), Typeable(..))


type Program = [Sentence]
type Body    = [Goal]

data Sentence
  = Query   Body
  | Command Body
  | C Clause
  deriving (Data, Typeable)

data Clause = Clause { lhs :: Term, rhs_ :: [Goal] }
            | ClauseFn { lhs :: Term, fn :: [Term] -> [Goal] }
      deriving (Data, Typeable)

rhs :: Clause -> [Term] -> [Goal]
rhs (Clause   _ rhs') = const rhs'
rhs (ClauseFn _ fn' ) = fn'

data Term = Struct Atom [Term]
          | Var VariableName
          | Wildcard
          | PString   !String
          | PInteger  !Integer
          | PFloat    !Double
          | Flat [FlatItem]
          | Cut Int
      deriving (Eq, Data, Typeable)


var :: String -> Term
var = Var . VariableName 0

{--
var "A"
A
--}

cut :: Term 
cut = Cut 0

data FlatItem = Bracket [FlatItem]
              | FITerm Term
              | FIOperator Atom
      deriving (Eq, Data, Typeable)

      

data VariableName = VariableName Int String
      deriving (Eq, Data, Typeable, Ord)

data Atom         = Atom      !String
                  | Operator  !String
      deriving (Eq, Ord, Data, Typeable)

atomString :: Atom -> String
atomString (Atom s) = s
atomString (Operator s) = s
                    
{--
atomString (Operator "hello")
"hello"

atomString (Atom "hello")
"hello"
--}


type Goal         = Term

instance Ord Term where
   (<=) = wildcards <=! variables <=! numbers <=!
          atoms <=! compound_terms <=! error "incomparable"

infixr 4 <=!
(<=!) :: Ord a => (t -> Maybe a) -> (t -> t -> Bool) -> t -> t -> Bool
(q <=! _) (q->Just l) (q->Just r) = l <= r
(q <=! _) (q->Just _) _ = True
(q <=! _) _ (q->Just _) = False
(_ <=! c) x y = c x y

wildcards :: Term -> Maybe ()
wildcards Wildcard = Just ()
wildcards _        = Nothing

variables :: Term -> Maybe VariableName
variables (Var v) = Just v
variables _       = Nothing

numbers :: Term -> Maybe Double
numbers (PInteger n) = Just (fromInteger n)
numbers (PFloat n)   = Just n
numbers _            = Nothing

atoms :: Term -> Maybe [Atom]
atoms (Struct a []) = Just [a]
atoms _             = Nothing

compound_terms :: Term -> Maybe (Int,Atom,[Term])
compound_terms (Struct a ts) = Just (length ts, a, ts)
compound_terms _             = Nothing




hierarchy :: Bool -> [[Operator]]
hierarchy ignoreConjunction =
   --[ [ InfixOp NonAssoc "-->", InfixOp NonAssoc ":-" ]
   [ [ infixR ";" ] ] ++
   (if ignoreConjunction then [] else [ [ infixR "," ] ])  ++
   [ [ prefix "\\+" ]
   , map infixL ["<", "=..", "=:=", "=<", "=", ">=", ">", "\\=", "is", "==", "@<", "@=<", "@>=", "@>"]
   , map infixL ["+", "-", "\\"]
   , [ infixL "*"]
   , [ infixL "mod" ]
   , [ prefix "-" ]
   , [ prefix "$" ] -- used for quasi quotation
   ]
 where
   prefix = PrefixOp
   infixL = InfixOp AssocLeft
   infixR = InfixOp AssocRight

operatorTable :: [(String, (Int,Operator))]
operatorTable = concat $ zipWith (map . g) [1..] $ hierarchy False
 where g p op@(InfixOp _ name) = (name,(p,op))
       g p op@(PrefixOp name)  = (name,(p,op))

foldr_pl f k (Struct (Atom ".") [h,t]) = f h (foldr_pl f k t)
foldr_pl _ k (Struct (Atom "[]") [])   = k

cons t1 t2 = Struct (Atom ".")  [t1,t2]
nil        = Struct (Atom "[]") []

data Operator = PrefixOp String
              | InfixOp Assoc String
data Assoc = AssocLeft
           | AssocRight


arguments ts xs ds = ts ++ [ xs, ds ]
-- arguments ts xs ds = [ xs \\ ds ] ++ ts
