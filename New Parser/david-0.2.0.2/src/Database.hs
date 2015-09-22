module Database
   ( createDB
   , hasPredicate
   , getClauses
   , asserta
   , assertz
   , abolish
   , Signature(), signature
   )
where

import Data.Map (Map)
import qualified Data.Map as Map

import Syntax
import PrettyPrint()


data Signature = Signature Atom Int deriving (Ord, Eq)
instance Show Signature where
   show (Signature name arity) = show name ++ "/" ++ show arity

{--
(Signature (Atom "Hello") 5)
Hello/5

Signature (Operator "+") 2
+/2
--}   

signature :: Term -> Signature
signature (Struct name ts) = Signature name (length ts)
signature (Cut _)          = Signature (Operator "!") 0
{--
signature (Struct (Operator "+") [(Struct (Atom "a")[]), (Struct (Atom "b") [])])
+/2

signature (Struct (Atom "Hello") [])
Hello/0
--}

newtype Database = DB (Map Signature [Clause])
{--
:t 
createDB [Clause (Struct (Atom "a") [Var $ VariableName 0 "A"]) [(Struct (Atom "b") [Var $ VariableName 0 "B"]), 
(Struct (Atom "c") [Var $ VariableName 0 "C"])]] [(Atom "hello")]

createDB [Clause (Struct (Atom "a") [Var $ VariableName 0 "A"]) [(Struct (Atom "b") [Var $ VariableName 0 "B"]),
(Struct (Atom "c") [Var $ VariableName 0 "C"])]] [(Atom "hello")]
  :: Database

--}

hasPredicate :: Signature -> Database -> Bool
hasPredicate sig (DB index) = Map.member sig index
{--
hasPredicate 
(signature (Struct (Atom "a") [Var $ VariableName 0 "A"])) $ 
createDB [Clause (Struct (Atom "a") [Var $ VariableName 0 "A"]) [(Struct (Atom "b") [Var $ VariableName 0 "B"]), 
(Struct (Atom "c") [Var $ VariableName 0 "C"])]] [(Atom "hello")]

True

hasPredicate 
(signature (Struct (Atom "d") [Var $ VariableName 0 "D"])) $ 
createDB [Clause (Struct (Atom "a") [Var $ VariableName 0 "A"]) [(Struct (Atom "b") [Var $ VariableName 0 "B"]), 
(Struct (Atom "c") [Var $ VariableName 0 "C"])]] [(Atom "hello")]

False
--}


createDB :: [Clause] -> [Atom] -> Database
createDB clauses emptyPredicates = DB $
   foldr (\clause -> Map.insertWith' (++) (signature (lhs clause)) [clause])
         (Map.fromList [ (signature (Struct name []), []) | name <- emptyPredicates ])
         clauses

getClauses :: Term -> Database -> [Clause]
getClauses term (DB index) = maybe [] id $ Map.lookup (signature term) index
{--
getClauses (Struct (Atom "a") [Var $ VariableName 0 "A"]) (createDB [Clause (Struct (Atom "a") [Var $ VariableName 0 "A"]) [(Struct (Atom "b") [Var $ VariableName 0 "B"]),(Struct (Atom "c") [Var $ VariableName 0 "C"])]] [(Atom "hello")])
[a(A) :- b(B), c(C)]

getClauses (Struct (Atom "d") [Var $ VariableName 0 "A"]) (createDB [Clause (Struct (Atom "a") [Var $ VariableName 0 "A"]) [(Struct (Atom "b") [Var $ VariableName 0 "B"]),(Struct (Atom "c") [Var $ VariableName 0 "C"])]] [(Atom "hello")])
[]
--}

asserta, assertz, abolish :: Term -> Database -> Database
asserta fact (DB index) = DB $ Map.insertWith (++)        (signature fact) [Clause fact []] index
assertz fact (DB index) = DB $ Map.insertWith (flip (++)) (signature fact) [Clause fact []] index
abolish fact (DB index) = DB $ Map.adjust deleteFact (signature fact) index
   where deleteFact (Clause t []:cs) | t == fact = cs
         deleteFact (_          :cs)             = cs
         deleteFact []                           = []
{--
asserta (Struct (Atom "hello") []) (createDB [Clause (Struct (Atom "a") [Var $ VariableName 0 "A"]) [(Struct (Atom "b") [Var $ VariableName 0 "B"]),(Struct (Atom "c") [Var $ VariableName 0 "C"])]] [(Atom "hello")])

--}