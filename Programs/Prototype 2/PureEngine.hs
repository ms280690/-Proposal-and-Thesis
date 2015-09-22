-- The Pure Prolog inference engine (using explicit prooftrees)
-- Mark P. Jones November 1990, modified for Gofer 20th July 1991,
-- and for Hugs 1.3 June 1996.
--
-- Suitable for use with Hugs 98.
--

module PureEngine( version, prove ) where

import Prolog
import Subst
import Interact
import Data.List(nub)

version = "tree based" 

--- Calculation of solutions:

-- Each node in a prooftree corresponds to:
-- either: a solution to the current goal, represented by Done s, where s
--         is the required substitution
-- or:     a choice between a number of subtrees ts, each corresponding to a
--         proof of a subgoal of the current goal, represented by Choice ts.
--         The proof tree corresponding to an unsolvable goal is Choice [] 

data Prooftree = Done Subst  |  Choice [Prooftree]

-- prooftree uses the rules of Prolog to construct a suitable proof tree for
--           a specified goal
prooftree   :: Database -> Int -> Subst -> [Term] -> Prooftree
prooftree db = pt
 where pt           :: Int -> Subst -> [Term] -> Prooftree
       pt n s []     = Done s 													-- Rule 1
       pt n s (g:gs) = Choice [ pt (n+1) (u@@s) (map (app u) (tp++gs))
                              | (tm:-tp)<-renClauses db n g, u<-unify g tm ]	-- Rule 2
{--
pt 1 nullSubst [] = Done (nullSubst)

pt n s (g:gs)

renClauses :- Rename variables in a clause, the parameters are the database, an int (X_1 will become X_2 if 2 is passed) and a goal term 
			(head of list) resulting in a clause.

unify :- take the head of the list and and match with head of clause from renClauses to get a list of substitutions.

app :- function for applying (Subst) to (Terms)
the new list is formed by replacing the cluase head with its body and applying the unifier to get a new list of terms

so the new parameters for pt are

(n+1) (the old substitution + the new one from unify) (the list formed after applying the unifier to the (body of head goal + rest of the list)) 


Working of a small example

The database,
(foldl addClause emptyDb [((:-) (Struct "hello" []) []), ((:-) (Struct "hello" [Struct "world" []]) []), ((:-) (Struct "hello" []) [Struct "world" []]), ((:-) (Struct "hello" [Var (1,"X")]) [])])
hello.
hello(world).
hello:-world.
hello(X_1).

The other parameters are 1 nullSubst(as mentioned in the prove function).

For the list of goals, [(Struct "hello" []), (Struct "hello" [(Struct "world" [])]), (Struct "hello" [Var (0, "X")])]

1. [Struct "hello" []] :: [Term]

* Rule 1 does not apply

* Rule 2 does apply,

(tm:- tp) <- renClauses db 1 (Struct "hello" [])

tm ==> "hello , hello(world) , hello , hello(X_1) , "
tp ==> "[] , [] , [world] , [] , "









--}



-- DFS Function
-- search performs a depth-first search of a proof tree, producing the list
-- of solution substitutions as they are encountered.
search              :: Prooftree -> [Subst] 
search (Done s)      = [s]
search (Choice pts)  = [ s | pt <- pts, s <- search pt ]


prove    :: Database -> [Term] -> [Subst]
prove db  = search . prooftree db 1 nullSubst

--- End of PureEngine.hs



----------------------- TESTING FUNCTIONS ------------------------------------------


myTest1 db n g = do
	(tm :- tp) <- renClauses db n g
	u<-unify g tm
	(show tp) ++ " , "
	show "\n"


-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------


