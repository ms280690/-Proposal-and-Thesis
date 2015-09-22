% Oracle.lhs
% Andreas Baldeau
% April 13, 2009

Overview
========

> module Curry.Debugger.Oracle (
>     Oracle,
>     emptyOracle,
>     allTrue,
>     pop,
>     push,
>     readStepsFile,
>     parseSteps,
>     readExtFile
> ) where

Representation
==============

> import qualified Curry.Debugger.BoolStack as BS

`Oracle`
--------

`Oracle` is just a simple `BoolStack`. 

> type Oracle = BS.BoolStack

> emptyOracle :: Oracle
> emptyOracle = BS.emptyBoolStack

> allTrue :: Oracle
> allTrue = BS.allTrue

Functions
=========

Accessing the `Oracle`
----------------------

These are simply aliases for the corressponding `BoolStack` functions.

> pop :: Oracle -> (Oracle, Bool)
> pop = BS.pop

> push :: Oracle -> Bool -> Oracle
> push = BS.push

Parsing prophecy files
----------------------

`readStepsFile` reads the specified (without `.steps`) prophecy file and returns
the represented `Oracle`.

> readStepsFile :: String -> IO Oracle
> readStepsFile modName = do
>     contents <- readFile $ modName ++ ".steps"
>     return $ parseSteps contents

`parseSteps` parses the content of a prophecy file and returns the represented
`Oracle`.

> parseSteps :: String -> Oracle
> parseSteps steps =
>     case reads steps :: [(Oracle, String)] of
>         [(o, "")] -> o
>         value     -> error $ "Parsing steps failed: "
>                           ++ show value

Virtual I/O
-----------

Reads the specified external file for given module name and returns the splitted list of inputs.

> readExtFile :: String -> IO [String]
> readExtFile modName = do
>     contents <- readFile $ modName ++ ".steps"
>     return $ split contents

Splits the given contents of an .ext file.

> split :: String -> [String]
> split s = case break (=='\n') s of
>            ([],[])    -> []
>            (n,_:vres) -> let (v,res) = splitAt (read n) vres in  
>                          v:split res
