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

-- Some other hidden 
   , wildcards, numbers, variables, atoms, compound_terms, hierarchy, operatorTable

   )
where

import Data.Generics (Data(..), Typeable(..))


type Program = [Sentence] 
type Body    = [Goal]
{--

--}

data Sentence
  = Query   Body
  | Command Body
  | C Clause
  deriving (Data, Typeable)
{--
:t C $ Clause (Struct (Atom "a") [Var $ VariableName 0 "A"]) [(Struct (Atom "b") [Var $ VariableName 0 "B"]), (Struct (Atom "c") [Var $ VariableName 0 "C"])]

a(A) :- b(B), c(C).
--}


data Clause = Clause { lhs :: Term, rhs_ :: [Goal] }
            | ClauseFn { lhs :: Term, fn :: [Term] -> [Goal] }
      deriving (Data, Typeable)

{--

Clause (Struct (Atom "a") [Var $ VariableName 0 "A"]) [(Struct (Atom "b") [Var $ VariableName 0 "B"]), (Struct (Atom "c") [Var $ VariableName 0 "C"])]
a(A) :- b(B), c(C)

--}

rhs :: Clause -> [Term] -> [Goal]
rhs (Clause   _ rhs') = const rhs'
rhs (ClauseFn _ fn' ) = fn'
{--
rhs (Clause (Struct (Atom "a") [Var $ VariableName 0 "A"]) [(Struct (Atom "b") [Var $ VariableName 0 "B"]), (Struct (Atom "c") [Var $ VariableName 0 "C"])]) [Struct (Atom "a") [Var $ VariableName 0 "A"]]
[b(B),c(C)]
--}

data Term = Struct Atom [Term]
          | Var VariableName
          | Wildcard
          | PString   !String
          | PInteger  !Integer
          | PFloat    !Double
          | Flat [FlatItem]
          | Cut Int
      deriving (Eq, Data, Typeable)
{--
Struct (Atom "Hello") []
Hello

Struct (Atom "Hello") [Var $ VariableName 0 "X"]
Hello(X)

Wildcard 
_

Var (VariableName 0 "X")
X

Var (VariableName 1 "X")
X#1

PString "Hello"
"Hello"

PInteger 1
1

PFloat 1.2
1.2

:t Flat [Bracket [FIOperator (Operator "+"), FITerm (Struct (Atom "hello") [Var $ VariableName 0 "X"])]]
Flat [Bracket [FIOperator (Operator "+"), FITerm (Struct (Atom "hello") [Var $ VariableName 0 "X"])]]
  :: Term

Cut 1
!

Cut 10000
!
--}

var :: String -> Term
var = Var . VariableName 0
{--
var "X"
X
--}      

cut :: Term 
cut = Cut 0
{--
cut
!
--}

data FlatItem = Bracket [FlatItem]
              | FITerm Term
              | FIOperator Atom
      deriving (Eq, Data, Typeable)
{--
FITerm (Struct (Atom "Hello") [])
Hello

FITerm (Struct (Atom "hello") [Var $ VariableName 0 "X"])
hello(X)

FIOperator (Operator "+")
+

Bracket [FIOperator (Operator "+"), FITerm (Struct (Atom "hello") [Var $ VariableName 0 "X"])]
([+,hello(X)])

--}


data VariableName = VariableName Int String
      deriving (Eq, Data, Typeable, Ord)
{--
VariableName 0 "X"
X
--}

data Atom         = Atom      !String
                  | Operator  !String
      deriving (Eq, Ord, Data, Typeable)
{--
show (Atom "Hello")
"Hello"

show (Operator "+")
"+"
--}

atomString :: Atom -> String
atomString (Atom s) = s
atomString (Operator s) = s
{--
atomString (Atom "Hello")
"Hello"

atomString  (Operator "+")
"+"
--}
                    

type Goal         = Term
{--
:t (Struct (Atom "hello") []) :: Goal
(Struct (Atom "hello") []) :: Goal :: Goal
--}


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
{--
wildcards (Wildcard)
Just ()

wildcards (Cut 0)
Nothing
--}

variables :: Term -> Maybe VariableName
variables (Var v) = Just v
variables _       = Nothing
{--
variables (Var (VariableName 0 "X"))
Just X

variables (Struct (Atom "Hello") [])
Nothing

--}


numbers :: Term -> Maybe Double
numbers (PInteger n) = Just (fromInteger n)
numbers (PFloat n)   = Just n
numbers _            = Nothing
{--
numbers (PInteger 5)
Just 5.0

numbers (PFloat  5)
Just 5.0
--}

atoms :: Term -> Maybe [Atom]
atoms (Struct a []) = Just [a]
atoms _             = Nothing
{--
atoms (Struct (Atom "Hello") [])
Just [Hello]
--}

compound_terms :: Term -> Maybe (Int,Atom,[Term])
compound_terms (Struct a ts) = Just (length ts, a, ts)
compound_terms _             = Nothing
{--
compound_terms (Struct (Atom "hello") [(Var $ VariableName 0 "X"), (Struct (Atom "world") [])])
Just (2,hello,[X,world])
--}

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
{--

--}

operatorTable :: [(String, (Int,Operator))]
operatorTable = concat $ zipWith (map . g) [1..] $ hierarchy False
 where g p op@(InfixOp _ name) = (name,(p,op))
       g p op@(PrefixOp name)  = (name,(p,op))
{--

--}       

foldr_pl :: (Term -> t -> t) -> t -> Term -> t
foldr_pl f k (Struct (Atom ".") [h,t]) = f h (foldr_pl f k t)
foldr_pl _ k (Struct (Atom "[]") [])   = k
{--

--}

cons :: Term -> Term -> Term
cons t1 t2 = Struct (Atom ".")  [t1,t2]
{--
cons (Struct (Atom "hello") []) (Struct (Atom "world") [])
.(hello,world)
--}

nil :: Term
nil        = Struct (Atom "[]") []
{--
nil
[]
--}

data Operator = PrefixOp String
              | InfixOp Assoc String
data Assoc = AssocLeft
           | AssocRight
{--
:t (PrefixOp "+") 
(PrefixOp "+") :: Operator

:t (InfixOp AssocLeft  "+") 
(InfixOp AssocLeft  "+") :: Operator
--}

arguments :: [a] -> a -> a -> [a]
arguments ts xs ds = ts ++ [ xs, ds ]
-- arguments ts xs ds = [ xs \\ ds ] ++ ts
{--
arguments [1,2] 1 2
[1,2,1,2]
--}