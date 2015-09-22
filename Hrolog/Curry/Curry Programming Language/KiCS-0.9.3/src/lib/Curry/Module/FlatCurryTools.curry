------------------------------------------------------------------------------
--- Some tools to support meta-programming in Curry based on FlatCurry.
---
--- This library contains
--- <UL>
--- <LI> a show function for a string representation of FlatCurry programs
---   (function "showFlatProg")
---
--- <LI> a function for showing FlatCurry expressions in (almost) Curry syntax
---   (function "showCurryExpr")
--- </UL>
---
--- Note that the previously contained function "writeFLC"
--- is no longer supported. Use Flat2Fcy.writeFCY instead
--- and change file suffix into ".fcy"!
---
--- @author Michael Hanus
--- @version August 2005
------------------------------------------------------------------------------

module FlatCurryTools(showFlatProg,showFlatType,showFlatFunc,
                      showCurryType,showCurryExpr,showCurryId,showCurryVar)
   where

import FlatCurry
import List
import Char

--- Shows a FlatCurry program term as a string (with some pretty printing).
showFlatProg :: Prog -> String
showFlatProg (Prog modname imports types funcs ops) =
     " (Prog " ++ show modname
     ++ (if imports==[] then "\n  []" else
         "\n  [" ++ showFlatListElems show imports ++ "]")
     ++ (if types==[] then "\n  []" else
         "\n  [" ++ showFlatListElems showFlatType types ++ "\n ]")
     ++ "\n  [" ++ showFlatListElems showFlatFunc funcs ++ "\n  ]"
     ++ "\n " ++ showFlatList showFlatOp ops
     ++ "\n )\n"

showFlatVisibility Public  = " Public "
showFlatVisibility Private = " Private "

showFlatFixity InfixOp = " InfixOp "
showFlatFixity InfixlOp = " InfixlOp "
showFlatFixity InfixrOp = " InfixrOp "

showFlatOp (Op name fix prec) =
 "(Op " ++ show name ++ showFlatFixity fix ++ show prec ++ ")"

showFlatType :: TypeDecl -> String
showFlatType (Type name vis tpars consdecls) =
  "\n  (Type " ++ show name ++ showFlatVisibility vis
               ++ showFlatList show tpars
               ++ showFlatList showFlatCons consdecls ++ ")"
showFlatType (TypeSyn name vis tpars texp) =
  "\n  (TypeSyn " ++ show name ++ showFlatVisibility vis
                  ++ showFlatList show tpars
                  ++ showFlatTypeExpr texp ++ ")"

showFlatCons (Cons cname arity vis types) =
  "(Cons " ++ show cname ++ " " ++ show arity
           ++ showFlatVisibility vis
           ++ showFlatList showFlatTypeExpr types ++ ")"

showFlatFunc :: FuncDecl -> String
showFlatFunc (Func name arity vis ftype rl) =
  "\n  (Func " ++ show name ++ " " ++ show arity ++ " "
               ++ showFlatVisibility vis ++
  "\n        " ++ showFlatTypeExpr ftype ++
  "\n       " ++ showFlatRule rl ++ ")"

showFlatRule (Rule params expr) =
  " (Rule " ++ showFlatList show params
            ++ showFlatExpr expr ++ ")"
showFlatRule (External name) =
  " (External " ++ show name ++ ")"

showFlatTypeExpr (FuncType t1 t2) =
  "(FuncType " ++ showFlatTypeExpr t1 ++ " " ++ showFlatTypeExpr t2 ++ ")"
showFlatTypeExpr (TCons tc ts) =
  "(TCons " ++ show tc
            ++ showFlatList showFlatTypeExpr ts ++ ")"
showFlatTypeExpr (TVar n) = "(TVar " ++ show n ++ ")"


showFlatCombType FuncCall = "FuncCall"
showFlatCombType ConsCall = "ConsCall"
showFlatCombType (FuncPartCall n) = "(FuncPartCall " ++ show n ++ ")"
showFlatCombType (ConsPartCall n) = "(ConsPartCall " ++ show n ++ ")"

showFlatExpr (Var n) = "(Var " ++ show n ++ ")"
showFlatExpr (Lit l) = "(Lit " ++ showFlatLit l ++ ")"
showFlatExpr (Comb ctype cf es) =
  "(Comb " ++ showFlatCombType ctype ++ " "
           ++ show cf ++ showFlatList showFlatExpr es ++ ")"
showFlatExpr (Let bindings exp) =
  "(Let " ++ showFlatList showFlatBinding bindings ++ showFlatExpr exp ++ ")"
 where 
   showFlatBinding :: (Int,Expr) -> String 
   showFlatBinding (x,e) = "("++show x++","++showFlatExpr e++")"
showFlatExpr (Free xs e) =
  "(Free " ++ showFlatList show xs ++ showFlatExpr e ++ ")"
showFlatExpr (Or e1 e2) =
  "(Or " ++ showFlatExpr e1 ++ " " ++ showFlatExpr e2 ++ ")"
showFlatExpr (Case Rigid e bs) =
  "(Case Rigid " ++ showFlatExpr e ++ showFlatList showFlatBranch bs ++ ")"
showFlatExpr (Case Flex e bs) =
  "(Case Flex " ++ showFlatExpr e ++ showFlatList showFlatBranch bs ++ ")"

showFlatLit (Intc   i) = "(Intc " ++ show i ++ ")"
showFlatLit (Floatc f) = "(Floatc " ++ show f ++ ")"
showFlatLit (Charc  c) =
 if ord c >= 32  &&  ord c < 127
 then "(Charc '" ++ [c] ++ "')"
 else "(Charc (chr " ++ show (ord c) ++ "))"

showFlatBranch (Branch p e) = "(Branch " ++ showFlatPattern p
                                         ++ showFlatExpr e ++ ")"

showFlatPattern (Pattern qn xs) =
      "(Pattern " ++ show qn
                  ++ showFlatList show xs ++ ")"
showFlatPattern (LPattern lit) = "(LPattern " ++ showFlatLit lit ++ ")"


-- format a finite list of elements:
showFlatList :: (a->String) -> [a] -> String
showFlatList format elems = " [" ++ showFlatListElems format elems ++ "] "

showFlatListElems :: (a->String) -> [a] -> String
showFlatListElems format elems = concat (intersperse "," (map format elems))


------------------------------------------------------------------------------
--- Shows a FlatCurry type in Curry syntax.
---
--- @param trans - a translation function from qualified type names
---                to external type names
--- @param nested - True iff brackets must be written around complex types
--- @param texpr - the FlatCurry type expression to be formatted
--- @return the String representation of the formatted type expression

showCurryType :: ((String,String) -> String) -> Bool -> TypeExpr -> String

showCurryType _ _ (TVar i) = if i<5 then [chr (97+i)] else 't':show i
showCurryType tf nested (FuncType t1 t2) =
  showBracketsIf nested
    (showCurryType tf (isFuncType t1) t1 ++ " -> " ++
     showCurryType tf False t2)
showCurryType tf nested (TCons tc ts)
 | ts==[]  = tf tc
 | tc==("prelude","[]") && (head ts == TCons ("prelude","Char") [])
   = "String"
 | tc==("prelude","[]")
  = "[" ++ showCurryType tf False (head ts) ++ "]" -- list type
 | take 2 (snd tc) == "(,"                         -- tuple type
  = "(" ++ concat (intersperse "," (map (showCurryType tf False) ts)) ++ ")"
 | otherwise
  = showBracketsIf nested
    (tf tc ++ concatMap (\t->' ':showCurryType tf True t) ts)

isFuncType (TVar _)       = False
isFuncType (FuncType _ _) = True
isFuncType (TCons _ _)  = False


------------------------------------------------------------------------------
--- Shows a FlatCurry expressions in (almost) Curry syntax.
---
--- @param trans - a translation function from qualified functions names
---                to external function names
--- @param nested - True iff brackets must be written around complex terms
--- @param indent - the indentation used in  case expressions and if-then-else
--- @param expr - the FlatCurry expression to be formatted
--- @return the String representation of the formatted expression

showCurryExpr :: ((String,String) -> String) -> Bool -> Int -> Expr -> String

showCurryExpr _ _ _ (Var n) = showCurryVar n

showCurryExpr _ _ _ (Lit l) = showCurryLit l

showCurryExpr tf _ _ (Comb _ cf []) = showCurryId (tf cf)
showCurryExpr tf nested b (Comb _ cf [e]) =
  showBracketsIf nested (showCurryId (tf cf) ++ " "
                            ++ showCurryExpr tf True b e)
showCurryExpr tf nested b (Comb ct cf [e1,e2])
 | cf==("prelude","apply")
  = showBracketsIf nested
       (showCurryExpr tf True b e1 ++ " " ++ showCurryExpr tf True b e2)
 | isAlpha (head (snd cf))
  = showBracketsIf nested
    (tf cf ++" "++ showCurryElems (showCurryExpr tf True b) [e1,e2])
 | isFiniteList (Comb ct cf [e1,e2])
  = if isStringConstant (Comb ct cf [e1,e2])
    then "\"" ++ showCurryStringConstant (Comb ct cf [e1,e2]) ++ "\""
    else "[" ++
         concat (intersperse "," (showCurryFiniteList tf b (Comb ct cf [e1,e2])))
         ++ "]"
 | snd cf == "(,)" -- pair constructor?
  = "(" ++ showCurryExpr tf False b e1 ++ "," ++
           showCurryExpr tf False b e2 ++ ")"
 | otherwise
  = showBracketsIf nested
              (showCurryExpr tf True b e1 ++ " " ++ tf cf ++ " " ++
               showCurryExpr tf True b e2 )
showCurryExpr tf nested b (Comb _ cf (e1:e2:e3:es))
 | cf==("prelude","if_then_else") && es==[]
  = showBracketsIf nested
        ("\n" ++
         sceBlanks b ++ " if "   ++ showCurryExpr tf False (b+2) e1 ++ "\n" ++
         sceBlanks b ++ " then " ++ showCurryExpr tf False (b+2) e2 ++ "\n" ++
         sceBlanks b ++ " else " ++ showCurryExpr tf False (b+2) e3)
 | take 2 (snd cf) == "(,"  -- tuple constructor?
  = "(" ++
    concat (intersperse "," (map (showCurryExpr tf False b) (e1:e2:e3:es)))
        ++ ")"
 | otherwise
  = showBracketsIf nested
       (showCurryId (tf cf) ++ " "
        ++ showCurryElems (showCurryExpr tf True b) (e1:e2:e3:es))

showCurryExpr tf nested b (Let bindings exp) =
  showBracketsIf nested
    ("\n"++sceBlanks b++"let " ++ concat (intersperse ("\n    "++sceBlanks b)
     (map (\ (x,e)->showCurryVar x ++" = "++showCurryExpr tf False (b+4) e) bindings)) ++
     ("\n"++sceBlanks b++" in ") ++ showCurryExpr tf False (b+4) exp)

showCurryExpr tf nested b (Free [] e) = showCurryExpr tf nested b e

showCurryExpr tf nested b (Free (x:xs) e) =
  showBracketsIf nested
    ("let " ++ concat (intersperse "," (map showCurryVar (x:xs))) ++
     " free in " ++ showCurryExpr tf False b e)

showCurryExpr tf nested b (Or e1 e2) =
  showBracketsIf nested
    (showCurryExpr tf True b e1 ++ " ? " ++ showCurryExpr tf True b e2)

showCurryExpr tf nested b (Case ctype e cs) =
  showBracketsIf nested
    ((if ctype==Rigid then "case " else "fcase ") ++
     showCurryExpr tf True b e ++ " of\n " ++
     showCurryElems (showCurryCase tf (b+2)) cs ++ sceBlanks b)


showCurryVar i = "v" ++ show i

--- Shows an identifier in Curry form. Thus, operators are enclosed in brackets.
showCurryId name | isAlpha (head name) = name
                 | name == "[]"        = name
                 | otherwise           = ('(':name)++")"

showCurryLit (Intc   i) = show i
showCurryLit (Floatc f) = show f
showCurryLit (Charc  c) = show c

showCurryCase tf b (Branch (Pattern l vs) e) =
  sceBlanks b ++ showPattern (tf l) vs
              ++ " -> " ++ showCurryExpr tf False b e ++ "\n"
 where
   showPattern c [] = c
   showPattern c [x] = c ++ " " ++ showCurryVar x
   showPattern c [x1,x2] =
     if isAlpha (head c)
     then c ++ " " ++ showCurryVar x1 ++ " " ++ showCurryVar x2
     else if c=="(,)" -- pair constructor?
          then "(" ++ showCurryVar x1 ++ "," ++ showCurryVar x2 ++ ")"
          else showCurryVar x1 ++ " " ++ c ++ " " ++ showCurryVar x2
   showPattern c (x1:x2:x3:xs) =
     if take 2 c == "(,"  -- tuple constructor?
     then "("++ concat (intersperse "," (map showCurryVar (x1:x2:x3:xs))) ++")"
     else c ++ " " ++ showCurryElems showCurryVar (x1:x2:x3:xs)

showCurryCase tf b (Branch (LPattern l) e) =
  sceBlanks b ++ showCurryLit l ++ " "
              ++ " -> " ++ showCurryExpr tf False b e ++ "\n"

showCurryFiniteList _ _ (Comb _ ("prelude","[]") []) = []
showCurryFiniteList tf b (Comb _ ("prelude",":") [e1,e2]) =
  showCurryExpr tf False b e1 : showCurryFiniteList tf b e2

-- show a string constant
showCurryStringConstant (Comb _ ("prelude","[]") []) = []
showCurryStringConstant (Comb _ ("prelude",":") [e1,e2]) =
   showCharExpr e1 ++ showCurryStringConstant e2

showCharExpr (Lit (Charc c))
  | c=='"'  = "\\\""
  | c=='\'' = "\\\'"
  | c=='\n' = "\\n"
  | o < 32 || o > 126 =
    ['\\',chr(o `div` 100 + 48), chr(((o `mod` 100) `div` 10 + 48)),chr(o `mod` 10 + 48)]
  | otherwise = [c]
 where
   o = ord c

showCurryElems :: (a->String) -> [a] -> String
showCurryElems format elems =
   concat (intersperse " " (map format elems))

showBracketsIf nested s = if nested then '(' : s ++ ")" else s

sceBlanks b = take b (repeat ' ')

-- Is the expression a finite list (with an empty list at the end)?
isFiniteList :: Expr -> Bool
isFiniteList (Var _) = False
isFiniteList (Lit _) = False
isFiniteList (Comb _ name args)
  | name==("prelude","[]") && args==[] = True
  | name==("prelude",":") && length args == 2 = isFiniteList (args!!1)
  | otherwise = False
isFiniteList (Let _ _) = False
isFiniteList (Free _ _) = False
isFiniteList (Or _ _) = False
isFiniteList (Case _ _ _) = False

-- Is the expression a string constant?
isStringConstant :: Expr -> Bool
isStringConstant e = case e of
  Comb _ name args -> (name==("prelude","[]") && null args) ||
                      (name==("prelude",":") && length args == 2 &&
                       isCharConstant (head args) && isStringConstant (args!!1))
  _                -> False

-- Is the expression a character constant?
isCharConstant :: Expr -> Bool
isCharConstant e = case e of
  Lit (Charc _) -> True
  _             -> False


------------------------------------------------------------------------------