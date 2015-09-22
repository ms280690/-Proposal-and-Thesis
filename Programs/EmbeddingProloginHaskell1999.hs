

-- Mehul Solanki


-- Embedding Prolog in Haskell 1999 (Functional reading of Logic programs) by Mike Spivey and Silvija Seres.

-- This program is an attempt to replicate the ideas proposed in the above document into a Haskell program.

-- The data types in a logic program are the predicates and the answers it produces.


type Predicate = Answer -> [Answer]

type Answer = (Subst, Int)

type Subst = (Term, Term)

type Term = String


-- The paper proposes a minimalistic	 extension to the host language Haskell in order to replicate Prolog like capabilities.
-- There are fouor operators that have been introduced, namely, &, ||, exists and unify.


-- The logical and (&) operator.

--logicAnd :: Predicate -> Predicate -> Predicate
--p `logicAnd` q = concat (map (q . p))  

--logicAnd :: Predicate -> Predicate -> 
logicAnd p q x = concat (map (q . p) x)

-- The logical or (||) operator
(||) :: Predicate -> Predicate -> Predicate
(p || q) x = p x ++ q x

-- True predicate
true :: Predicate
true x = [x]

-- False Predicate
false :: Predicate
false x = []

-- INCOMPLETE
unify :: Subst -> (Term, Term) -> [Subst]
unify s (t, u) = []

(unifyWrapper) :: (Term, Term) -> Predicate
(t `unifyWrapper` u) (s,n) = [(s1, n) | s1 <- unify s (t, u)]

exists :: (Term -> Predicate) -> Predicate
exists p (s, n) = p (makevar n) (s, n + 1)

solve :: Predicate -> Stream String
solve p = map print (p ([],0))


