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


data Signature = Signature Atom Int deriving (Ord, Eq)
instance Show Signature where
   show (Signature name arity) = name ++ "/" ++ show arity

{--
Takes a Term and returns its signature, which is of the above forms.
--}
signature :: Term -> Signature
signature (Struct name ts) = Signature name (length ts)
{--
signature (Struct "hello" []) 
hello/0

signature (Struct "hello" [Var (VariableName 0 "X"), Var (VariableName 0 "Y")]) 
hello/2
--}

newtype Database = DB (Map Signature [Clause])

{--
Checks the database for predicates with the given signature
--}
hasPredicate :: Signature -> Database -> Bool
hasPredicate sig (DB index) = Map.member sig index

{--
hasPredicate (Signature "hello" 1) x
True

hasPredicate (Signature "hello" 2) x
False

hasPredicate (Signature "hello" 0) x
True
--}

{--
It is basically a foldr operation

f--> (\clause -> Map.insertWith' (++) (signature (lhs clause)) [clause])
Take a clause and its lhs 
Create a signature which is the key and along with it add it to the map using insertWith'

acc--> (Map.fromList [ (signature (Struct name []), []) | name <- emptyPredicates ])
Take the second argument which is a list of atoms, create terms out of each of them and then converts the formed 
list into a Map

list --> clauses
The list of clauses  

--}
createDB :: [Clause] -> [Atom] -> Database
createDB clauses emptyPredicates = DB $
   foldr (\clause -> Map.insertWith' (++) (signature (lhs clause)) [clause])
         (Map.fromList [ (signature (Struct name []), []) | name <- emptyPredicates ])
         clauses
{--
let x = createDB [Clause (Struct "hello" [Struct "a" []]) [Struct "world" []]] ["hello"]   
--}


getClauses :: Term -> Database -> [Clause]
getClauses term (DB index) = maybe [] id $ Map.lookup (signature term) index
{--
getClauses (Struct "hello" [Struct "a" []]) x
["hello(a) :- world"]
--}

{--
assert is a meta-predicate that adds its single argument, which may be a fact or a rule, to the Prolog database. 
The idea is that you construct or learn a new fact or rule in the course of executing your Prolog program, and you want to 
add it to the database. asserta ensures that the added fact/rule is added before any other facts or rules with the same functor), 
while assertz adds the fact after any other rules or facts with the same functor. When more than one rule/fact with the same
functor is present in the database, they are tried in the order that they appear in the database, hence the need for asserta 
and assertz. You would use asserta in the common case where the new fact is supposed to save the effort involved in checking the
fact using rules and other facts. You might use assertz, for example, if you were trying to construct a queue data structure 
(where items are added to the end of the queue.
--}

asserta :: Term -> Database -> Database
asserta fact (DB index) = DB $ Map.insertWith (++)        (signature fact) [Clause fact []] index

assertz :: Term -> Database -> Database
assertz fact (DB index) = DB $ Map.insertWith (flip (++)) (signature fact) [Clause fact []] index

abolish :: Term -> Database -> Database
abolish fact (DB index) = DB $ Map.adjust deleteFact (signature fact) index
   where deleteFact (Clause t []:cs) | t == fact = cs
         deleteFact (_          :cs)             = cs
         deleteFact []                           = []

