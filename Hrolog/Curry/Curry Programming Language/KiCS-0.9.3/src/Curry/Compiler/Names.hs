module Curry.Compiler.Names where

import Data.Char
import Data.List
import System.FilePath

import Curry.Compiler.ShowFunctionalProg (isTuple,isInfixOpName)

---------------------------------------------------------------------------
-- generating names to avoid clashes with Haskell
---------------------------------------------------------------------------
-- constructor names

preludeConstructorName "()" = "T0"
preludeConstructorName "[]" = "List"
preludeConstructorName ":"  = ":<"
preludeConstructorName n 
  | isTuple n = "T"++show (1+length (takeWhile (==',') (tail n)))
  | otherwise = 'C':'_':n

constructorName = preludeConstructorName

consName (m,n) = 
  case m of
   ""        -> ("",preludeConstructorName n)
   _         -> (modName m,constructorName n)


{-
extConsName exts (m,n) = case m of
   "Prelude" -> (datamod m,preludeConstructorName n)
   ""        -> ("",preludeConstructorName n)
   _         -> (datamod m,constructorName n)
  where
    datamod = if elem (m,n) exts then extDataModName else dataModName
-}

functionName n | isInfixOpName n = elimInfix n 
               | otherwise = 'c':'_':n

funName (m,n) = (modName m,functionName n)

elimInfix name = "op_"++concat (intersperse "_" (map (show . ord) name))

-----------------------------------------
-- naming conventions for new objects
-----------------------------------------
-- module names

insertName :: String -> FilePath -> FilePath
insertName s xs = replaceFileName xs (s++takeFileName xs)

moduleName s =
  if isLower (head (takeBaseName s)) then insertName "C_" s else s

modName s = insertName "Curry.Module." (moduleName s)
funcHsName s = replaceExtension (moduleName s) ".hs"
externalSpecName s = replaceExtension s ".hs.include"

dbgMName  = "Oracle"
dbgModName  = insertName dbgMName
strictPrefix = "Curry.DebugModule."
mkStrictName = insertName strictPrefix


-- names for new constructors
--addPrefix s _ (p@"Prelude","Int")   = (instModName p,"C_Int"++s)
--addPrefix s _ (p@"Prelude","Float") = (instModName p,"Prim"++s)
addPrefix s (m,n) = (m,n++s)

freeVarName = addPrefix "FreeVar"
failName    = addPrefix "Fail"
orName      = addPrefix "Or"
suspName    = addPrefix "Susp"


