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

signature :: Term -> Signature
signature (Struct name ts) = Signature name (length ts)
signature (Cut _)          = Signature (Operator "!") 0


newtype Database = DB (Map Signature [Clause])

hasPredicate :: Signature -> Database -> Bool
hasPredicate sig (DB index) = Map.member sig index

createDB :: [Clause] -> [Atom] -> Database
createDB clauses emptyPredicates = DB $
   foldr (\clause -> Map.insertWith' (++) (signature (lhs clause)) [clause])
         (Map.fromList [ (signature (Struct name []), []) | name <- emptyPredicates ])
         clauses

getClauses :: Term -> Database -> [Clause]
getClauses term (DB index) = maybe [] id $ Map.lookup (signature term) index


asserta, assertz, abolish :: Term -> Database -> Database
asserta fact (DB index) = DB $ Map.insertWith (++)        (signature fact) [Clause fact []] index
assertz fact (DB index) = DB $ Map.insertWith (flip (++)) (signature fact) [Clause fact []] index
abolish fact (DB index) = DB $ Map.adjust deleteFact (signature fact) index
   where deleteFact (Clause t []:cs) | t == fact = cs
         deleteFact (_          :cs)             = cs
         deleteFact []                           = []
