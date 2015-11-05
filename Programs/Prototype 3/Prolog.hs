
-- Representation of Prolog Terms, Clauses and Databases
-- Mark P. Jones November 1990, modified for Gofer 20th July 1991,
-- and for Hugs 1.3 June 1996.
--
-- Suitable for use with Hugs 98.
--

-- module Prolog
--	( Id, Term(..), Clause(..), Database
--	, varsIn, renClauses, addClause, emptyDb, termlist, clause
--	) where

module Prolog where

import Data.List
import CombParse
import Data.Char

infix 6 :-

--- Prolog Terms:

type Id       = (Int,String)
{--
:t (1, "Var1") :: Id
(1,"Var1") :: Id
--}

type Atom     = String
{--
:t "hello" :: Atom
"hello" :: Atom
--}

data Term     = Var Id | Struct Atom [Term]
{--
:t (Var (1, "var1"))
Var (1,"var1") :: Term

(Var (1, "var1"))
var1_1

:t (Struct "hello" [(Var (1, "var"))])
Struct "hello" [Var (1,"var")] :: Term

(Struct "hello" [(Var (1, "var"))])
hello(var_1)
--}

data Clause   = Term :- [Term]
{--
(:-) (Struct "hello" []) [(Struct "world" [])]
hello:-world.

--}

data Database = Db [(Atom,[Clause])]
{--
Db [("my",[(:-) (Struct "hello" []) [(Struct "world" [])]])]
hello:-world.

--}

instance Eq Term where
    Var v       == Var w       =  v==w
    Struct a ts == Struct b ss =  a==b && ts==ss
    _           == _           =  False
{--
Var (1, "var1") == Var (2, "var2")
False

Struct "hello" [] == Struct "world" []
False

--}


--- Determine the list of variables in a term:

varsIn              :: Term -> [Id]
varsIn (Var i)       = [i]
varsIn (Struct i ts) = (nub . Prelude.concat . Prelude.map varsIn) ts --nub removes duplicate elements from a list
{--
varsIn (Struct "hello" [Struct "world" [Var (1,"var1")]])
[(1,"var1")]
--}


renameVars                  :: Int -> Term -> Term
renameVars lev (Var (n,s))   = Var (lev,s)
renameVars lev (Struct s ts) = Struct s (Prelude.map (renameVars lev) ts)
{--
renameVars 1 (Var (0, "var"))
var_1

--}


--- Functions for manipulating databases (as an abstract datatype)

emptyDb      :: Database
emptyDb       = Db []
{--
emptyDb
-- Empty Database --
--}

renClauses                  :: Database -> Int -> Term -> [Clause]
renClauses db n (Var _)      = []
renClauses db n (Struct a _) = [ r tm:-Prelude.map r tp | (tm:-tp)<-clausesFor a db ]
                               where r = renameVars n
{--
Rename the variables in the clauses of a database,
renClauses (foldl addClause emptyDb [((:-) (Struct "hello" []) []), ((:-) (Struct "hello" [Struct "world" []]) []), ((:-) (Struct "hello" []) [Struct "world" []]), ((:-) (Struct "hello" [Var (1,"X")]) [])]) 2 (Struct "hello" [(Var (1, "X"))])
[hello.,hello(world).,hello:-world.,hello(X_2).]

The database originally had the following clauses,
hello.
hello(world).
hello:-world.
hello(X_1).

To rename the variable in the last clause for hello which is X_1

the following parameters are passed,
2 (Struct "hello" [(Var (1, "X"))])
--}

clausesFor           :: Atom -> Database -> [Clause]
clausesFor a (Db rss) = case dropWhile (\(n,rs) -> n<a) rss of
                         []         -> []
                         ((n,rs):_) -> if a==n then rs else []
{--
Finding all clauses for a particular head in a database,
clausesFor "hello" $ foldl addClause emptyDb [((:-) (Struct "hello" []) []), ((:-) (Struct "hello" [Struct "world" []]) []), ((:-) (Struct "hello" []) [Struct "world" []])]
[hello.,hello(world).,hello:-world.] 
      
--}


addClause :: Database -> Clause -> Database
addClause (Db rss) r@(Struct a _ :- _)
           = Db (update rss)
             where update []            = [(a,[r])]
                   update (h@(n,rs):rss')
                          | n==a        = (n,rs++[r]) : rss'
		          | n<a         = h : update rss'
                          | otherwise   = (a,[r]) : h : rss'
{--
addClause x ((:-) (Struct "hello" [(Struct "world" [])]) [])
hello(world).

addClause x ((:-) (Struct "hello" []) [])
hello.  

Adding multiple clauses to a database,

foldl addClause emptyDb [((:-) (Struct "hello" []) []), ((:-) (Struct "hello" [Struct "world" []]) []), ((:-) (Struct "hello" []) [Struct "world" []])]
hello.
hello(world).
hello:-world.
--}



--- Output functions (defined as instances of Show):

instance Show Term where
  showsPrec p (Var (n,s))
              | n==0        = showString s
              | otherwise   = showString s . showChar '_' . shows n
  showsPrec p (Struct a []) = showString a
  showsPrec p (Struct a ts) = showString a . showChar '('
                                           . showWithSep "," ts
                                           . showChar ')'

instance Show Clause where
   showsPrec p (t:-[]) = shows t . showChar '.'
   showsPrec p (t:-gs) = shows t . showString ":-"
                                 . showWithSep "," gs
                                 . showChar '.'

instance Show Database where
    showsPrec p (Db [])  = showString "-- Empty Database --\n"
    showsPrec p (Db rss) = foldr1 (\u v-> u . showChar '\n' . v)
                                  [ showWithTerm "\n" rs | (i,rs)<-rss ]

--- Local functions for use in defining instances of Show:

showWithSep          :: Show a => String -> [a] -> ShowS
showWithSep s [x]     = shows x
showWithSep s (x:xs)  = shows x . showString s . showWithSep s xs
{--

--}


showWithTerm         :: Show a => String -> [a] -> ShowS
showWithTerm s xs     = foldr1 (.) [shows x . showString s | x<-xs]

--- String parsing functions for Terms and Clauses:
--- Local definitions:

letter       :: Parser Char
letter        = sat (\c->isAlpha c || isDigit c || c `elem` ":;+=-*&%$#@?/.~!")

variable     :: Parser Term
variable      = sat isUpper `pseq` many letter `pam` makeVar
                where makeVar (initial,rest) = Var (0,(initial:rest))

struct       :: Parser Term
struct        = many letter `pseq` (sptok "(" `pseq` termlist `pseq` sptok ")"
                                       `pam` (\(o,(ts,c))->ts)
                                  `orelse`
                                   okay [])
                `pam` (\(name,terms)->Struct name terms)

--- Exports:

term         :: Parser Term
term          = sp (variable `orelse` struct)

termlist     :: Parser [Term]
termlist      = listOf term (sptok ",")

clause       :: Parser Clause
clause        = sp struct `pseq` (sptok ":-" `pseq` listOf term (sptok ",")
                                 `pam` (\(from,body)->body)
                                `orelse` okay [])
                          `pseq` sptok "."
                     `pam` (\(head,(goals,dot))->head:-goals)

--- End of Prolog.hs

