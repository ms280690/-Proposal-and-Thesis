{-
  compile with
    ghc --make -o PropLogic Main.hs
  and run with
    ./PropLogic ....
-}
module Main where

-- imports

  import System (getArgs)
  import qualified TextDisplay as D
  import qualified Costack as C
  import PropLogic

-- main function

  briefHelpMessage = D.correctTextFrame
   [ "Syntax of the PropLogic command:                                                      ",
     "   PropLogic                    -- print this help                                    ",
     "   PropLogic help               -- print a more detailed help (with more options)     ",
     "   PropLogic pdnf PROPFORM      -- the Prime Disjunctive Normal Form of PROPFORM      ",
     "   PropLogic pcnf PROPFORM      -- the Prime Conjunctive Normal Form of PROPFORM      ",
     "   PropLogic spdnf PROPFORM     -- the Simplified Prime Disjunctive Normal Form       ",
     "   PropLogic scdnf PROPFORM     -- the Simplified Prime Conjunctive Normal Form       ",
     "   PropLogic xpdnf PROPFORM     -- the Extended/Indexed Prime Disjunctive Normal Form ",
     "   PropLogic xpcnf PROPFORM     -- the Extended/Indexed Prime Conjunctive Normal Form ",
     "where in each case, PROPFORM is a propositional formula in fancy notation and         ",
     "wrapped in double quotes \"...\"." ]

  fullHelpMessage = D.correctTextFrame
   [ "+---------------------------------------------------------------------------+",
     "|                (Fancy) Syntax of Propositional Formulas:                  |",
     "+---------------------+-----------------------------+-----------------------+",
     "| w                   | for every atom w            | (atomic formula)      |",
     "| false               |                             | (zero or false value) |",
     "| true                |                             | (unit or true value)  |",
     "| -p                  | for every formula p         | (negation)            |",
     "| [p1 * ... * pN]     | for N>=0 formulas p1,...,pN | (conjunction)         |",
     "| [p1 + ... + pN]     | for N>=0 formulas p1,...,pN | (disjunction)         |",
     "| [p1 -> ... -> pN]   | for N>=0 formulas p1,...,pN | (subjunction)         |",
     "| [p1 <-> ... <-> pN] | for N>=0 formulas p1,...,pN | (equijunction)        |",
     "+---------------------+-----------------------------+-----------------------+",
     "| where an atom w is any non-empty sequence of letters and digits, except   |",
     "| the keywords 'true' and 'false'.                                          |",
     "+---------------------------------------------------------------------------|",
     "",
     "Example formulas are:",
     "",
     "  [-x + [z <-> true] + -x + 23 + -y + -[x -> y] + [z * -7]]",
     "",
     "  [[-x + y] <-> [x -> y] <-> -[x * -y] <-> [false -> x -> y -> true]]",
     "",
     "+----------------------------------------------------------------------+",
     "|                Definition of I-Forms                                 |",
     "+----------------------------------------------------------------------+",
     "| An I-Form is a list [L1,...,Ln] of n I-Lines. Each I-Line is a list  |",
     "| [I1,...,Im] of integers, so that their absolute values are in strict |",
     "| ascending order, i.e. abs(I1) < ... < abs(Im).                       |",
     "+----------------------------------------------------------------------+",
     "",
     "Example I-Forms are:",
     "",
     "  [[-2,4,6],[1,3,5,7],[-1,2,-3,4,-5,6],[],[-3],[3],[1,2,3,4,5,6,7]]",
     "",
     "  [[]]",
     "",
     "+------------------------------------------------------------------------------------+",
     "|                         Syntax of the PropLogic command                            |",
     "+---------------------------+--------------------------------------------------------+",
     "| PropLogic                 | print a brief overview of important command options    |",
     "| PropLogic help            | print a more detailed help (namely this one)lp         |",
     "| PropLogic pdnf PROPFORM   | the Prime Disjunctive Normal Form of PROPFORM          |",
     "| PropLogic pcnf PROPFORM   | the Prime Conjunctive Normal Form of PROPFORM          |",
     "| PropLogic spdnf PROPFORM  | the Simplified Prime Disjunctive Normal Form           |",
     "| PropLogic scdnf PROPFORM  | the Simplified Prime Conjunctive Normal Form           |",
     "| PropLogic xpdnf PROPFORM  | the Extended/Indexed Prime Disjunctive Normal Form     |",
     "| PropLogic xpcnf PROPFORM  | the Extended/Indexed Prime Conjunctive Normal Form     |",
     "| PropLogic primForm IFORM  | the Prime Normal Form of the given IFORM               |",
     "| PropLogic testing X Y Z N | performs test runs of Prime Form with randomly         |",
     "|                           | generated I-Forms. The four integer parameters are:    |",
     "|                           |   X is the number of (different) atoms of the I-Form   |",
     "|                           |   Y is the length of the randomly generated I-Form     |",
     "|                           |   Z is the average length of the I-Lines in the I-Form |",
     "|                           |   N is the number of test runs.                        |",
     "+---------------------------+--------------------------------------------------------+",
     "| where                                                                              |",
     "|   PROPFORM is a propositional formula (see below) surrounded by double quotes,     |",
     "|   IFORM is an I-Form (see below) surrounded by double quotes.                      |",
     "+------------------------------------------------------------------------------------+",
     "",
     "Legal example calls of the PropLogic command are:",
     "",
     "  PropLogic spdnf \"[-x + [z <-> true] + -x + 23 + -y + -[x -> y] + [z * -7]]\" ",
     "",
     "  PropLogic primForm \"[[-2,3],[1,2,3],[1,2,-3],[-1,4],[-2,4]]\"",
     "",
     "  PropLogic testing 5 20 4 100",
     "",
     "More help and other material:",
     "  http://www.bucephalus.org/PropLogic",
     "" ]

  main :: IO ()
  main = do args <- getArgs
            let act = case args of
                        []                      -> D.printTextFrame briefHelpMessage
                        ["help"]                -> D.printTextFrame fullHelpMessage
                        ["pdnf", form]          -> pdnf form
                        ["pcnf", form]          -> pcnf form
                        ["spdnf", form]         -> spdnf form
                        ["spcnf", form]         -> spcnf form
                        ["xpdnf", form]         -> xpdnf form
                        ["xpcnf", form]         -> xpcnf form
                        ["primForm", intLL]     -> print (prim intLL)
                        ["testing", x, y, z, n] -> do t <- verboseRandomPrimeTesting (read x, read y, read z) (read n)
                                                      print t
                        otherwise               -> do print "Unknown arguments for PropLogic!"
                                                      D.printTextFrame briefHelpMessage
            act
            return ()
    where prim intLL =  let intLL' = (read intLL) :: [[Int]]
                            iform = iForm intLL'
                            pform = primForm iform
                            intLL'' = map C.toList (C.toList pform)
                        in intLL''

