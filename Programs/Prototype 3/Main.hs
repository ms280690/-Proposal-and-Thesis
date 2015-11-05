-- Prolog interpreter top level module
-- Mark P. Jones November 1990, modified for Gofer 20th July 1991,
-- and for Hugs 1.3 June 1996.
--
-- Suitable for use with Hugs 98.
--

module Main where

import CombParse
import Prolog
import Interact
import Subst
import PureEngine
import Data.List(nub)

import System.IO.Error
--- Command structure and parsing:


import CustomSyntax

data Command = Fact Clause | Query [Term] | Show | Error | Quit | NoChange

command :: Parser Command
command  = just (sptok "bye" `orelse` sptok "quit") `pam` (\quit->Quit)
               `orelse`
           just (okay NoChange)
               `orelse`
           just (sptok "??") `pam` (\show->Show)
               `orelse`
           just clause `pam` Fact
               `orelse`
           just (sptok "?-" `pseq` termlist) `pam` (\(q,ts)->Query ts)
               `orelse`
           okay Error

--- Main program read-solve-print loop:

signOn           :: String
signOn            = "Mini Prolog Version 1.5g (" ++ version ++ ")\n\n"
{--
 signOn
"Mini Prolog Version 1.5g (tree based)\n\n" :: String
(780 reductions, 1132 cells)

--}


main             :: IO ()
main              = do putStr signOn
                       putStr ("Reading " ++ stdlib)
		       clauses <- readLibrary stdlib
                       interpreter clauses

readLibrary :: [Char] -> IO [Clause]
readLibrary lib   = do is <- readFile lib
                       let parse   = map clause (lines is)
                           clauses = [ r | ((r,""):_) <- parse ]
                           reading = ['.'| c <- clauses] ++ " done\n"
                       putStr reading
		       return clauses
		    `catchIOError` \err ->
                    do putStr "...not found\n"
                       return []
{--
do{readLibrary "stdlib.txt"}
........ done
 :: IO [Clause]
--}

stdlib           :: String  
stdlib            = "stdlib.txt"

interpreter      :: [Clause] -> IO ()
interpreter lib   = do is <- getContents
                       putStr (loop startDb is)
                    where startDb = foldl addClause emptyDb lib
{--
interpreter [((:-) (Struct "hello" []) [(Struct "world" [])]), ((:-) (Struct "hello" []) [])]
> ??
??
hello:-world.
hello.
> ?-hello
?-hello
yes.
> okay
okay
I don't understand
> bye
bye
Thank you and goodbye
it :: ()
(0.16 secs, 2413396 bytes)
--}


loop             :: Database -> String -> String
loop db           = readLine "> " (exec db . fst . head . command)
{--
loop (Db [("my",[(:-) (Struct "hello" []) [(Struct "world" [])]])]) "hello."
"> hello.\n> *** Exception: Prelude.tail: empty list

loop (Db [("my",[(:-) (Struct "hello" []) [(Struct "world" [])]])]) "bye"
"> bye\nThank you and goodbye\n"

loop (Db [("my",[(:-) (Struct "hello" []) [(Struct "world" [])]])]) "?? "
"> ?? \nhello:-world.\n> *** Exception: Prelude.tail: empty list


--}

exec             :: Database -> Command -> String -> String
exec db (Fact r)  = loop (addClause db r)
exec db (Query q) = demonstrate db q
exec db Show      = writeStr (show db)                 (loop db)
exec db Error     = writeStr "I don't understand\n"    (loop db)
exec db Quit      = writeStr "Thank you and goodbye\n" end
exec db NoChange  = loop db
{--

--}


--- Handle printing of solutions etc...

solution      :: [Id] -> Subst -> [String]
solution vs s  = [ show (Var i) ++ " = " ++ show v
                                | (i,v) <- [ (i,s i) | i<-vs ], v /= Var i ]
{--
solution [(0, "Var")] nullSubst
[]
it :: [String]

solution [(1, "Var")] nullSubst
[]
it :: [String]

solution [(1, "Var")] substFunction
["Var_1 = Var_2"]
it :: [String]
--}


demonstrate     :: Database -> [Term] -> Interact
demonstrate db q = printOut (map (solution vs) (prove db q))
 where vs               = (nub . concat . map varsIn) q -- nub removes duplicate elements from the list
       printOut []      = writeStr "no.\n"     (loop db)
       printOut ([]:bs) = writeStr "yes.\n"    (loop db)
       printOut (b:bs)  = writeStr (doLines b) (nextReqd bs)
       doLines          = foldr1 (\xs ys -> xs ++ "\n" ++ ys)
       nextReqd bs      = writeStr " "
                          (readChar end
                           (\c-> if c==';' then writeStr ";\n" (printOut bs)
                                           else writeStr "\n"  (loop db)))

--- End of Main.hs
