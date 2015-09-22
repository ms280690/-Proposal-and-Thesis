module PatchPrelude where


import Curry.ExtendedFlat.Type


-- the prelude has to be extended by data declarations for list and tuples

prelude = "Prelude"

patchPreludeFCY :: Prog -> Prog
patchPreludeFCY (Prog name imports types funcs ops)
   | name == prelude
     = Prog name [] (prelude_types_fcy ++ types) funcs ops
   | otherwise
     = Prog name imports types funcs ops

prelude_types_fcy :: [TypeDecl]
prelude_types_fcy =
  let unit = mkQName (prelude,"()")
      nil  = mkQName (prelude,"[]") in
  [Type unit Public [] [(Cons unit 0 Public [])],
   Type nil Public [0] 
        [Cons nil 0 Public [],
         Cons (mkQName (prelude,":")) 2 Public 
              [TVar 0, TCons nil [TVar 0]]]] ++
  map tupleType [2..maxTupleArity]

tupleType ar = 
  let tuplecons = mkQName (prelude,"("++take (ar-1) (repeat ',')++")") in
  Type tuplecons Public [0..ar-1]
       [Cons tuplecons ar Public (map TVar [0..ar-1])]

-- Maximal arity of tuples:
maxTupleArity = 15


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

