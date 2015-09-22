{-# LANGUAGE DeriveDataTypeable, ViewPatterns, ScopedTypeVariables #-}
module Syntax
   ( Term(..), var, cut
   , Clause(..), rhs
   , VariableName(..), Atom, Goal, Program
   , cons, nil, foldr_pl
   , arguments -- FIXME Should not be exposed
   , hierarchy
   , Operator(..), Assoc(..)
   )
where

import Data.Generics (Data(..), Typeable(..))
import Data.List (intercalate)
import Data.Char (isLetter)

----------------------------------------------------------------------
----------------------------------------------------------------------
import Data.Traversable as T
import Data.Foldable
import Data.Functor
import Control.Unification as U
import Control.Applicative as A

{--
Intercalate Examples

:t intercalate
intercalate :: [a] -> [[a]] -> [a]

intercalate [1..5] [[6..10], [11..15], [16..20]]
[6,7,8,9,10,1,2,3,4,5,11,12,13,14,15,1,2,3,4,5,16,17,18,19,20]

intercalate [1..5] [[6..10], [11..15]]
[6,7,8,9,10,1,2,3,4,5,11,12,13,14,15]
--}

{--
A Prolog Term can be an 

Atom  
Struct "hello" []
hello
Struct "hello" [Struct "a" []]
hello(a)

Variable
Var (VariableName 125 "X")
X#125

Wildcard (Don't Care)
_

Cut
Cut 0
!
Cut 4
!

--}

data Term = Struct Atom [Term]
          | Var VariableName
          | Wildcard -- Don't cares 
          | Cut Int
      deriving (Eq, Data, Typeable)
var = Var . VariableName 0
cut = Cut 0

{--

Clause
Clause (Struct "hello" [Struct "a" []]) ([Struct "world" []])
"hello(a) :- world"


Clausefn
????

--}

data Clause = Clause { lhs :: Term, rhs_ :: [Goal] }
            | ClauseFn { lhs :: Term, fn :: [Term] -> [Goal] }
      deriving (Data, Typeable)

rhs :: Clause -> [Term] -> [Goal]      
rhs (Clause   _ rhs) = const rhs
rhs (ClauseFn _ fn ) = fn

data VariableName = VariableName Int String
      deriving (Eq, Data, Typeable, Ord)

type Atom         = String
type Goal         = Term
type Program      = [Clause]

--Precedence, less than or equal to.
instance Ord Term where
   (<=) = wildcards <=! variables <=! atoms <=! compound_terms <=! error "incomparable"

-- Uses the auxiliary functions below 

(<=!) :: Ord a => (t -> Maybe a) -> (t -> t -> Bool) -> t -> t -> Bool
infixr 4 <=!
(q <=! _) (q->Just l) (q->Just r) = l <= r 
(q <=! _) (q->Just _) _ = True 
(q <=! _) _ (q->Just _) = False 
(_ <=! c) x y = c x y 

{--
The following functions take terms and convert them into Maybes
--}

wildcards :: Term -> Maybe ()
wildcards Wildcard = Just ()
wildcards _        = Nothing

variables :: Term -> Maybe VariableName
variables (Var v) = Just v
variables _       = Nothing

numbers :: Term -> Maybe Integer
numbers (Struct (reads->[(n :: Integer,"")]) []) = Just n
numbers _                                        = Nothing

atoms :: Term -> Maybe [Atom]
atoms (Struct a []) = Just [a]
atoms _             = Nothing

compound_terms :: Term -> Maybe (Int, Atom, [Term])
compound_terms (Struct a ts) = Just (length ts, a, ts)
compound_terms _             = Nothing

-- Printing stuff
instance Show Term where
   show = prettyPrint False 0

prettyPrint :: Bool -> Int -> Term -> [Char]
prettyPrint True _ t@(Struct "," [_,_]) = "(" ++ prettyPrint False 0 t ++  ")"

prettyPrint f n (Struct (flip lookup operatorTable->Just (p,InfixOp assoc name)) [l,r]) =
   parensIf (n >= p) $ prettyPrint f n_l l ++ spaced name ++ prettyPrint f n_r r
     where (n_l,n_r) = case assoc of
                           AssocLeft  -> (p-1, p)
                           AssocRight -> (p, p-1)

prettyPrint f n (Struct (flip lookup operatorTable->Just (p,PrefixOp name)) [r]) =
   parensIf (n >= p) $ name ++ prettyPrint f (p {- Non-associative -}) r

prettyPrint _ _ t@(Struct "." [_,_]) =
   let (ts,rest) = g [] t in
      --case guard (isNil rest) >> sequence (map toChar ts) of
      --   Just str -> prettyPrint str
      --   Nothing  ->
            "[" ++ intercalate "," (map (prettyPrint True 0) ts) ++ (if isNil rest then "" else "|" ++ (prettyPrint True 0) rest) ++  "]"
   where g ts (Struct "." [h,t]) = g (h:ts) t
         g ts t = (reverse ts, t)
         isNil (Struct "[]" []) = True
         isNil _                = False

prettyPrint _ _ (Struct a [])   = a
prettyPrint _ _ (Struct a ts)   = a ++ "(" ++ intercalate ", " (map (prettyPrint True 0) ts) ++ ")"
prettyPrint _ _ (Var v)         = show v
prettyPrint _ _ Wildcard        = "_"
prettyPrint _ _ (Cut _)         = "!"
--prettyPrint _ _ ((==cut)->True) = "!"
--prettyPrint _ _ (Cut n)         = "!^" ++ show n


spaced s = let h = head s
               l = last s
           in spaceIf (isLetter h) ++ s ++ spaceIf (isLetter l || ',' == l)

spaceIf True  = " "
spaceIf False = ""

parensIf :: Bool -> String -> String
parensIf True  s = "(" ++ s ++")"
parensIf False s = s


operatorTable :: [(String, (Int,Operator))]
operatorTable = Prelude.concat $ zipWith (map . g) [1..] $ hierarchy False
 where g p op@(InfixOp _ name) = (name,(p,op))
       g p op@(PrefixOp name)  = (name,(p,op))

instance Show VariableName where
   show (VariableName 0 v) = v
   show (VariableName i v) = v ++ "#" ++  show i

instance Show Clause where
   show (Clause   lhs [] ) = show $ show lhs
   show (Clause   lhs rhs) = show $ show lhs ++ " :- " ++ intercalate ", " (map show rhs)
   show (ClauseFn lhs _  ) = show $ show lhs ++ " :- " ++ "<Haskell function>"



foldr_pl :: (Term -> t -> t) -> t -> Term -> t
foldr_pl f k (Struct "." [h,t]) = f h (foldr_pl f k t)
foldr_pl _ k (Struct "[]" [])   = k

cons t1 t2 = Struct "."  [t1,t2]
nil        = Struct "[]" []

data Operator = PrefixOp String
              | InfixOp Assoc String
data Assoc = AssocLeft
           | AssocRight

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


--infix 6 \\
--x \\ y = Struct "\\" [x,y]

arguments ts xs ds = ts ++ [ xs, ds ]
-- arguments ts xs ds = [ xs \\ ds ] ++ ts


{--
Data Type Genric Programming / Generic Programming is a way of defining functions to work on Structures of Data Types 
rather than Data Types themselves. 

Thus a single function can be designed to work on a number of Data Types

--}




--------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------



