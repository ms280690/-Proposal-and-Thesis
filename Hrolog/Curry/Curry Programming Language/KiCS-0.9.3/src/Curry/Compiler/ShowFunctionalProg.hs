{-# LANGUAGE PatternGuards #-}
-- uses pattern guards to recognize strings and lists
------------------------------------------------------------------------------
--- A pretty printer for AbstractHaskell, adapted from AbstractCurryPrinter
---
--- This library defines a function "showProg" that shows
--- an AbstractCurry program in standard Curry syntax.
---
--- @author Martin Engelke, Bernd Brassel, Michael Hanus, Sebastian Fischer
--- @version May 2007
-- in November 2004: 
-- - added filter for type variables (to print <var0> as var0, like in Prelude)
-- - prettyprint list patterns
-- in July 2005:
-- - added options to most functions
-- - print qualified symbol when necessary (local functions missing)
-- in May 2007:
-- - prettier representation of Curry and Haskell Strings
------------------------------------------------------------------------------
module Curry.Compiler.ShowFunctionalProg(
  showProg,showProgOpt,PrintOptions(..),defaultPrintOptions,
                            showTypeDecls,
                            showTypeDecl,
                            showTypeExpr,
                            showFuncDecl,
                            showExpr,showPattern,
                            isInfixOpName,isTuple) where

import Data.List
import Data.Char (isDigit,ord)
import Data.Maybe (isJust)
import Control.Monad (ap)

import Curry.Compiler.FunctionalProg
import Curry.Compiler.Brace

-------------------------------------------------------------------------------
-- Functions to print an AbstractCurry program in standard Curry syntax
-------------------------------------------------------------------------------

data PrintOptions = PrintOpt { unqual :: Bool,
                               sep :: String,
                               include :: String,
                               pragma  :: [String] }

defaultPrintOptions :: PrintOptions
defaultPrintOptions = PrintOpt False "" "" []

--- Shows an AbstractCurry program in standard Curry syntax.
showProg :: Prog -> String
showProg = showProgOpt defaultPrintOptions

showProgOpt :: PrintOptions -> Prog -> String
showProgOpt opts p@(Prog m imports exports typedecls insdecls funcdecls opdecls)
   = showPragma (["OPTIONS -cpp -O0 "
                      ,"LANGUAGE RankNTypes,\
                               \ ScopedTypeVariables,\
                               \ MultiParamTypeClasses,\
                               \ FlexibleInstances,\
                               \ TypeSynonymInstances"] ++ pragma opts)
     ++ "module "++m++showExports opts m exports ++" where\n\n"
     ++ showImports imports
     ++ "\n\n-- begin included\n\n" 
     ++ include opts 
     ++ "\n\n-- end included\n\n"
     ++ showOpDecls opdecls
     ++ showTypeDecls opts typedecls
     ++ showInsDecls opts insdecls
     ++ separate "\n\n" (map (showFuncDeclOpt opts) funcdecls)
     ++ "\n"


-----------------------------------------
-- pragma declaration
-----------------------------------------

showPragma :: [String] -> String
showPragma = concatMap (\s -> "{-# " ++ s ++ " #-}\n\n")

-----------------------------------------
-- export declaration
-----------------------------------------

showExports :: PrintOptions -> String -> [String] -> String
showExports _ m exports = brace " (" ")" ", " (("module "++m):exports)

-----------------------------------------
-- import declaration
-----------------------------------------

showImports :: [String] -> String
showImports imports = brace "" "\n\n" "\n" (map ("import "++) imports)
    
-----------------------------------------
-- infix operators
-----------------------------------------

showOpDecls :: [OpDecl] -> String
showOpDecls opdecls = brace "" "\n\n" "\n" (map showOpDecl opdecls)

showOpDecl :: OpDecl -> String
showOpDecl (Op (_,name) fixity precedence)
   = separate " " [showFixity fixity,show precedence,'`':showIdentifier name++"`"]

showFixity :: Fixity -> String
showFixity InfixOp  = "infix"
showFixity InfixlOp = "infixl"
showFixity InfixrOp = "infixr"

--------------------------------------------------
-- type declarations, instances, type classes
--------------------------------------------------

--- Shows a list of AbstractCurry type declarations in standard Curry syntax.
showTypeDecls :: PrintOptions -> [TypeDecl] -> String
showTypeDecls opts typedecls =  
  brace "" "\n\n" "\n\n" (map (showTypeDecl opts) typedecls)

--- Shows an AbstractCurry type declaration in standard Curry syntax.
showTypeDecl :: PrintOptions -> TypeDecl -> String
showTypeDecl opts t = 
  decl ++ showIdentifier (snd (typeName t)) ++ 
  brace " " "" " " (map (showTypeExpr opts False . TVar) (typeVars t)) ++ " = "++
  (case t of
    TypeSyn{typeExpr=e} -> showTypeExpr opts False e
    Type{consDecls=cs} -> separate "\n  | " (map (showConsDecl opts) cs) ++
                          brace "\n  deriving (" ")" "," (derive t))
  where
    decl = case t of {TypeSyn{} -> "type "; Type{} -> "data "} 

showConsDecl :: PrintOptions -> ConsDecl -> String
showConsDecl opts c 
   = separate (if strictArgs c then " !" else " ") 
              (showIdentifier (snd (consName c)) : 
               map (showTypeExpr opts True) (consArgs c))

showInsDecls :: PrintOptions -> [InstanceDecl] -> String
showInsDecls opts is = brace "" "\n\n" "\n\n" (map (showInsDecl opts) is)

showInsDecl :: PrintOptions -> InstanceDecl -> String
showInsDecl opts (Instance tcs tc fs) 
  = "instance " 
  ++ showTypeConstr opts tcs
  ++ showTypeClass opts tc 
  ++ brace " where\n  " "\n\n" "  " (map (showFuncDeclOpt (opts{sep="  "})) fs)

showTypeConstr opts tcs = brace "(" ") => " "," (map (showTypeClass opts) tcs)

showTypeClass opts (TypeClass qn ts) 
  = snd qn ++ brace " " "" " " (map (showTypeExpr opts True) ts)

--- Shows an AbstractCurry type expression in standard Curry syntax.
--- If the first argument is True, the type expression is enclosed
--- in brackets.
showTypeExpr :: PrintOptions -> Bool -> TypeExpr -> String
showTypeExpr _ _ (TVar name) = showIdentifier name
showTypeExpr opts nested (FuncType domain range) =
   (if nested then brace "(" ")" else separate) " -> "
   [showTypeExpr opts (case domain of {FuncType _ _ -> False; _ -> True}) domain,
    showTypeExpr opts False range]
showTypeExpr opts nested (TCons (mod,name) typelist) = 
   (if nested && not (null typelist) then brace "(" ")" else separate) ""
   [showTypeCons opts mod name typelist]
showTypeExpr opts nested (TConstr tcs t) = 
   (if nested then brace "(" ")" else separate) ""
   [showTypeConstr opts tcs ++ showTypeExpr opts False t]

showTypeCons :: PrintOptions -> String -> String -> [TypeExpr] -> String
showTypeCons opts mod name ts = 
  showSymbol opts (mod,name) ++ 
  brace " " "" " " (map (showTypeExpr opts True) ts)



------------------------------------------
-- function declarations
------------------------------------------

--- Shows an AbstractCurry function declaration in standard Curry syntax.
showFuncDecl = showFuncDeclOpt defaultPrintOptions

showFuncDeclOpt :: PrintOptions -> FuncDecl -> String
showFuncDeclOpt opts f = 
  maybe "" (\t->fname ++" :: "++ (showTypeExpr opts False t) ++ "\n") 
           (funcType f) ++
  maybe (fname ++ " external") 
        (brace (fname++" ") "\n\n" ("\n"++sep opts++fname++" ") . 
        map (showRule opts)) (funcBody f)
  where
    fname = showIdentifier (snd (funcName f))

showRule :: PrintOptions -> Rule -> String
showRule opts (Rule ps r ls) 
  = separate " " (map (showPatternOpt opts) ps) ++
    showRhs opts r ++
    brace "\n   where\n    " "" "\n    " (map (showLocalDecl opts) ls)

showRhs :: PrintOptions -> Rhs -> String
showRhs opts (SimpleExpr e) = " = "++showExprOpt opts e
showRhs opts (GuardedExpr gs) = brace "\n  " "" "\n  " (map (showGuard opts) gs)

showGuard :: PrintOptions -> (Expr,Expr) -> String
showGuard opts (g,r) = "  | " ++ showExprOpt opts g ++ " = " ++ showExprOpt opts r

showLocalDecl :: PrintOptions -> LocalDecl -> String
showLocalDecl opts (LocalFunc funcdecl) = showFuncDeclOpt (opts{sep="    "}) funcdecl
showLocalDecl opts (LocalPat pattern expr ls) =
   showPatternOpt opts pattern ++ " = " ++ showExprOpt opts expr ++
   brace "\n   where\n    " "" "\n    " (map (showLocalDecl opts) ls)

---------------------------------------
-- symbols, expresssions, identifiers
---------------------------------------

-- Remove characters '<' and '>' from identifiers sind these characters
-- are sometimes introduced in new identifiers generated by the front end (for sections)
-- also eliminate non standard characters.

showIdentifier :: String -> String
showIdentifier "[]" = "[]"
showIdentifier "_" = "_"
showIdentifier name 
  | isInfixOpName name = "("++name++")"
  | isTuple name = name
  | otherwise = let newName = normChars name in
     if head newName=='\'' then "c_"++newName else newName
  where
   normChars [] = []
   normChars (c@'_':cs) = c:normChars cs
   normChars (c:cs) 
     | (co >= na && co <= nz) = c:normChars cs
     | (co >= nA && co <= nZ) = c:normChars cs
     | (co >= n0 && co <= n9) = c:normChars cs
     | otherwise = '\'':show co++normChars cs
     where
       co = ord c
       na = 97
       nz = 122
       nA = 65
       nZ = 90
       n0 = 48
       n9 = 57

--- Shows an AbstractCurry expression in standard Curry syntax.
showExpr = showExprOpt defaultPrintOptions

showExprOpt :: PrintOptions -> Expr -> String
showExprOpt _ (Var name) = showIdentifier name
showExprOpt _ (Lit lit) = showLiteral lit
showExprOpt opts (Symbol name) = showSymbol opts name
showExprOpt opts exp@(Apply func arg)
  | Just cs <- expAsCurryString   exp = fromCurryString cs
  | Just cl <- expAsCurryList     exp = fromCurryList cl
  | Just hs <- expAsHaskellString exp = fromHaskellString hs
  | Just hl <- expAsHaskellList   exp = fromHaskellList hl
  | otherwise = showExprOpt opts func ++ brace "(" ")" "" [showExprOpt opts arg]
 where
  -- string or list is non-empty (the empty string is parsed as empty list)
  fromCurryString s = "(fromHaskellString " ++ show s++ ")"

  fromCurryList es
    = "(fromHaskellList ["
   ++ concat (intersperse "," (map (showExprOpt opts) es)) ++ "])"

  fromHaskellString s = show s -- quotation marks and quoted special chars

  fromHaskellList es
    = "[" ++ concat (intersperse "," (map (showExprOpt opts) es)) ++ "]"

showExprOpt opts (Lambda patts expr) = showLambda opts patts expr
showExprOpt opts (LetDecl localdecls expr)
   = brace "let {" "} in " "; " (map (showLocalDecl opts) localdecls) ++
     showExprOpt opts expr
showExprOpt opts (DoExpr stmts)
   = brace "do\n    " "\n  " "\n    " (map (showStatement opts) stmts)
showExprOpt opts (ListComp expr stmts)
   =    brace "[" "]" " | " 
          [showExprOpt opts expr,separate ", " (map (showStatement opts) stmts)]
showExprOpt opts (Case expr branches)
   = brace ("case " ++ showExprOpt opts expr ++ " of\n") "\n" "\n  "
       (map (showBranchExpr opts) branches)
showExprOpt _ (String s) = '"':s++"\"" --"

showSymbol :: PrintOptions -> QName -> String
showSymbol _ ("",symName) = showIdentifier symName
showSymbol opts (m,symName) 
  | isInfixOpName symName = brace "(" ")" "" [m++"."++symName]
  | not (unqual opts) || isExternalModule = m++"."++showIdentifier symName
  | otherwise = showIdentifier symName
  where
    isExternalModule
      = case m of {('E':'x':'t':'e':'r':'n':'a':'l':_) -> True;_->False}

showLambda opts patts expr = 
  brace "\\ " " -> " " " (map (showPatternOpt opts) patts) ++
  showExprOpt opts expr


showStatement :: PrintOptions -> Statement -> String
showStatement opts (SExpr expr) = showExprOpt opts expr
showStatement opts (SPat pattern expr)
   = showPatternOpt opts pattern ++ " <- " ++ showExprOpt opts expr
showStatement opts (SLet localdecls)
   =  brace "let " " in \n  " "\n    " (map (showLocalDecl opts) localdecls)

-- try to transform expression into a non-empty Curry string
expAsCurryString :: Expr -> Maybe String
expAsCurryString (Symbol ("CurryPrelude","List")) = Just ""
expAsCurryString (Apply (Apply (Symbol ("CurryPrelude",":<"))
                          (Apply (Symbol ("CurryPrelude","C_Char"))
                                 (Lit (Charc c))))
                   cs)
  = Just (c:) `ap` expAsCurryString cs
expAsCurryString _ = Nothing

-- try to transform expression into a Curry list
expAsCurryList :: Expr -> Maybe [Expr]
expAsCurryList (Symbol ("CurryPrelude","List")) = Just []
expAsCurryList (Apply (Apply (Symbol ("CurryPrelude",":<")) x) xs)
  = Just (x:) `ap` expAsCurryList xs
expAsCurryList _ = Nothing

-- try to transform expression into a non-empty Haskell string
expAsHaskellString :: Expr -> Maybe String
expAsHaskellString (Symbol ("","[]")) = Just ""
expAsHaskellString (Apply (Apply (Symbol ("",":")) (Lit (Charc c))) cs)
  = Just (c:) `ap` expAsHaskellString cs
expAsHaskellString _ = Nothing

-- try to transform expression into a Haskell list
expAsHaskellList :: Expr -> Maybe [Expr]
expAsHaskellList (Symbol ("","[]")) = Just []
expAsHaskellList (Apply (Apply (Symbol ("",":")) x) xs)
  = Just (x:) `ap` expAsHaskellList xs
expAsHaskellList _ = Nothing

-------------------------------------------------------
-- patterns
-------------------------------------------------------

showPattern :: Pattern -> String
showPattern = showPatternOpt defaultPrintOptions

showPatternOpt :: PrintOptions -> Pattern -> String
showPatternOpt _ (PVar name) = showIdentifier name
showPatternOpt _ (PLit lit) = showLiteral lit
showPatternOpt opts (PComb name []) = showSymbol opts name 
showPatternOpt opts (PComb sym ps)
   = brace "(" ")" " " (showSymbol opts sym:map (showPatternOpt opts) ps)
showPatternOpt opts (AsPat v p) = 
  showPatternOpt opts (PVar v)++"@"++showPatternOpt opts p

showBranchExpr :: PrintOptions -> BranchExpr -> String
showBranchExpr opts (Branch pattern expr)
   = showPatternOpt opts pattern ++ " -> " ++ showExprOpt opts expr

showLiteral :: Literal -> String
showLiteral (HasIntc i) = '(':show i++"::Int)"
showLiteral (Intc i) = '(':show i++"::C_Int)"
showLiteral (Floatc f) = '(':show f++"::Float)"
showLiteral (Charc c) = "'"++showCharc c++"'"

showCharc :: Char -> String
showCharc c = case c of 
   '\n' -> "\\n"
   '\t' -> "\\t"
   '\r' -> "\\r"
   '\\' -> "\\\\"
   '\"' -> "\\\""
   '\'' -> "\\'"
   _ -> [c]

-------------------------------------------------------------------------------
--- tests for various properties of AbstractCurry constructs
-------------------------------------------------------------------------------

isInfixOpName :: String -> Bool
isInfixOpName = all (`elem` infixIDs)

isCFuncType t = case t of
                  FuncType _ _ -> True
                  _ -> False

isTuple [] = False
isTuple (c:cs) = c=='(' && dropWhile (==',') cs == ")"

------------------------------------------------------------------------------
--- constants used by AbstractCurryPrinter
------------------------------------------------------------------------------

infixIDs :: String
infixIDs =  "~!@#$%^&*+-=<>?./|\\:"
