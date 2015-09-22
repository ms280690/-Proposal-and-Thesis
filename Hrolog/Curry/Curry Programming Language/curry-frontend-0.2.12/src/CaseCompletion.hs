-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--
-- CaseCompletion - expands case branches with missing constructors
--
-- The MMC translates case expressions into the intermediate language
-- representation (IL) without completing them (i.e. without generating
-- case branches for missing contructors). Because they are necessary for
-- the PAKCS back end this module expands all case expressions accordingly.
--
-- May 2005,
-- Martin Engelke, (men@informatik.uni-kiel.de)
-- 
module CaseCompletion (completeCase) where

import Data.Maybe

import Curry.Base.Position (SrcRef)
import Curry.Base.Ident
import qualified Curry.Syntax

import Base (ModuleEnv, lookupModule)
import IL.Type
import OldScopeEnv -- as ScopeEnv
import IL.Scope



-------------------------------------------------------------------------------

-- Completes case expressions by adding branches for missing constructors.
-- The module environment 'menv' is needed to compute these constructors.
--
-- Call:
--      completeCase <module environment>
--                   <IL module>
--
completeCase :: ModuleEnv -> Module -> Module
completeCase menv mod = let (mod', _) = visitModule menv mod in mod'


-------------------------------------------------------------------------------
-- The following functions run through an IL term searching for
-- case expressions

--
visitModule :: ModuleEnv -> Module -> (Module, [Message])
visitModule menv (Module mident imports decls)
   = ((Module mident (insertUnique preludeMIdent imports) decls'), msgs')
 where
   (decls', msgs') = visitList (visitDecl (Module mident imports decls) menv)
		               insertDeclScope
			       []
			       (getModuleScope (Module mident imports decls))
			       decls


--
visitDecl :: Module -> ModuleEnv -> [Message] -> ScopeEnv -> Decl
	     -> (Decl, [Message])
visitDecl mod menv msgs senv (DataDecl qident arity cdecls)
   = ((DataDecl qident arity cdecls), msgs)

visitDecl mod menv msgs senv (NewtypeDecl qident arity cdecl)
   = ((NewtypeDecl qident arity cdecl), msgs)

visitDecl mod menv msgs senv (FunctionDecl qident params typeexpr expr)
   = ((FunctionDecl qident params typeexpr expr'), msgs)
 where
   (expr', _, _) = visitExpr mod menv msgs (insertExprScope senv expr) expr

visitDecl mod menv msgs senv (ExternalDecl qident cconv name typeexpr)
   = ((ExternalDecl qident cconv name typeexpr), msgs)


--
visitExpr :: Module -> ModuleEnv -> [Message] -> ScopeEnv -> Expression 
	     -> (Expression, [Message],ScopeEnv)
visitExpr mod menv msgs senv (Literal lit) 
   = ((Literal lit), msgs, senv)

visitExpr mod menv msgs senv (Variable ident) 
   = ((Variable ident), msgs, senv)

visitExpr mod menv msgs senv (Function qident arity) 
   = ((Function qident arity), msgs, senv)

visitExpr mod menv msgs senv (Constructor qident arity)
   = ((Constructor qident arity), msgs, senv)

visitExpr mod menv msgs senv (Apply expr1 expr2)
   = ((Apply expr1' expr2'), msgs2, senv2)
 where
   (expr1', msgs1, senv1) = visitExpr mod menv msgs (insertExprScope senv expr1) expr1
   (expr2', msgs2, senv2) = visitExpr mod menv msgs1 (insertExprScope senv1 expr2) expr2

visitExpr mod menv msgs senv (Case r evalannot expr alts)
   | null altsR
     = intError "visitExpr" "empty alternative list"
   | evalannot == Flex   -- pattern matching causes flexible case expressions
     = (Case r evalannot expr' altsR, msgs, senv1)
   | isConstrAlt altR
     = (expr2, msgs3, senv3)
   | isLitAlt altR
     = (completeLitAlts r evalannot expr' altsR, msgs3, senv2)
   | isVarAlt altR
     = (completeVarAlts expr' altsR, msgs3, senv2)
   | otherwise 
     = intError "visitExpr" "illegal alternative list"
 where
   altR           = head altsR
   (expr', _, senv1) = visitExpr mod menv msgs (insertExprScope senv expr) expr
   (alts', _, senv2) = visitListWithEnv (visitAlt mod menv) insertAltScope msgs senv1 alts
   (altsR, msgs3) = removeRedundantAlts msgs alts'
   (expr2, senv3) = completeConsAlts r mod menv senv2 evalannot expr' altsR

visitExpr mod menv msgs senv (Or expr1 expr2)
   = ((Or expr1' expr2'), msgs2, senv3)
 where
   (expr1', msgs1, senv2) = visitExpr mod menv msgs (insertExprScope senv expr1) expr1
   (expr2', msgs2, senv3) = visitExpr mod menv msgs1 (insertExprScope senv2 expr2) expr2

visitExpr mod menv msgs senv (Exist ident expr)
   = ((Exist ident expr'), msgs', senv2)
 where
   (expr', msgs', senv2) = visitExpr mod menv msgs (insertExprScope senv expr) expr

visitExpr mod menv msgs senv (Let bind expr)
   = ((Let bind' expr'), msgs2, senv3)
 where
   (expr', _, senv2) = visitExpr mod menv msgs (insertExprScope senv expr) expr
   (bind', msgs2, senv3) = visitBinding mod menv msgs (insertBindingScope senv2 bind) bind

visitExpr mod menv msgs senv (Letrec binds expr)
   = ((Letrec binds' expr'), msgs2, senv3)
 where
   (expr', msgs1, senv2)  = visitExpr mod menv msgs (insertExprScope senv expr) expr
   (binds', msgs2, senv3) = visitListWithEnv (visitBinding mod menv)
		               const
			       msgs1
			       (foldl insertBindingScope senv2 binds)
			       binds


--
visitAlt :: Module -> ModuleEnv -> [Message] -> ScopeEnv -> Alt 
	    -> (Alt, [Message], ScopeEnv)
visitAlt mod menv msgs senv (Alt pattern expr)
   = ((Alt pattern expr'), msgs', senv2)
 where
   (expr', msgs', senv2) = visitExpr mod menv msgs (insertExprScope senv expr) expr


--
visitBinding :: Module -> ModuleEnv -> [Message] -> ScopeEnv -> Binding 
	        -> (Binding, [Message], ScopeEnv)
visitBinding mod menv msgs senv (Binding ident expr)
   = ((Binding ident expr'), msgs', senv2)
 where
   (expr', msgs', senv2) = visitExpr mod menv msgs (insertExprScope senv expr) expr


--
visitList :: ([Message] -> ScopeEnv -> a -> (a, [Message]))
	     -> (ScopeEnv -> a -> ScopeEnv)
	     -> [Message] -> ScopeEnv -> [a]
	     -> ([a], [Message])
visitList visitTerm insertScope msgs senv []
   = ([], msgs)
visitList visitTerm insertScope msgs senv (term:terms)
   = ((term':terms'), msgs2)
 where
   (term', msgs1)  = visitTerm msgs (insertScope senv term) term
   (terms', msgs2) = visitList visitTerm insertScope msgs1 senv terms

visitListWithEnv :: ([Message] -> ScopeEnv -> a -> (a, [Message], ScopeEnv))
	     -> (ScopeEnv -> a -> ScopeEnv)
	     -> [Message] -> ScopeEnv -> [a]
	     -> ([a], [Message], ScopeEnv)
visitListWithEnv visitTerm insertScope msgs senv []
   = ([], msgs, senv)
visitListWithEnv visitTerm insertScope msgs senv (term:terms)
   = ((term':terms'), msgs2, senv3)
 where
   (term', msgs1, senv2)  = visitTerm msgs (insertScope senv term) term
   (terms', msgs2, senv3) = visitListWithEnv visitTerm insertScope msgs1 senv2 terms

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Functions for completing case alternatives

-- Completes a case alternative list which branches via constructor patterns
-- by adding alternatives of the form
--
--      comp_pattern -> default_expr
--
-- where "comp_pattern" is a complementary constructor pattern and
-- "default_expr" is the expression from the first alternative containing
-- a variable pattern. If there is no such alternative the defualt expression
-- is set to the prelude function 'failed'.
--
-- This funtions uses a scope environment ('ScopeEnv') to generate fresh
-- variables for the arguments of the new constructors.
--
completeConsAlts :: SrcRef -> Module -> ModuleEnv -> ScopeEnv 
		    -> Eval -> Expression -> [Alt]
		    -> (Expression, ScopeEnv)
completeConsAlts r mod menv senv evalannot expr alts
   = (Case r evalannot expr (alts1 ++ alts2), senv2)
 where
   (Alt varpatt defaultexpr) = getDefaultAlt alts
   (VariablePattern varid)   = varpatt
   alts1       = filter isConstrAlt alts
   constrs     = (map p_getConsAltIdent alts1)
   cconsinfos  = getComplConstrs mod menv constrs
   (cconstrs,senv2) = 
                 foldr p_genConstrTerm
                       ([],senv) 
                       cconsinfos
   alts2       = map (\cconstr -> 
		      (Alt cconstr 
		        (replaceVar varid (cterm2expr cconstr) defaultexpr))) 
		     cconstrs

   p_getConsAltIdent (Alt (ConstructorPattern qident _) _) = qident

   p_genConstrTerm (qident, arity) (cconstrs,senv3) =
       let args = OldScopeEnv.genIdentList arity "x" senv3
           senv4 = foldr OldScopeEnv.insertIdent senv3 args
       in (ConstructorPattern qident args : cconstrs, senv4)


-- If the alternatives branches via literal pattern complementary
-- constructor list cannot be generated because it would become infinite.
-- So the function 'completeLitAlts' transforms case expressions like
--      case <cexpr> of
--        <lit_1> -> <expr_1>
--        <lit_2> -> <expr_2>
--                    :
--        <lit_n> -> <expr_n>
--       [<var>   -> <default_expr>]
-- to 
--      case (<cexpr> == <lit_1>) of
--        True  -> <expr_1>
--        False -> case (<cexpr> == <lit_2>) of
--                   True  -> <expr_2>
--                   False -> case ...
--                                  :
--                               -> case (<cexpr> == <lit_n>) of
--                                    True  -> <expr_n>
--                                    False -> <default_expr>
--
completeLitAlts :: SrcRef -> Eval -> Expression -> [Alt] -> Expression
completeLitAlts r evalannot expr [] = failedExpr
completeLitAlts r evalannot expr (alt:alts)
   | isLitAlt alt 
     = (Case r evalannot 
	     (eqExpr expr (p_makeLitExpr alt))
	     [(Alt truePatt  (getAltExpr alt)),
	      (Alt falsePatt (completeLitAlts r evalannot expr alts))])
   | otherwise
     = case alt of
         Alt (VariablePattern v) expr'
	   -> replaceVar v expr expr'
	 _ -> intError "completeLitAlts" "illegal alternative"
 where
   p_makeLitExpr alt
      = case (getAltPatt alt) of
	  LiteralPattern lit -> Literal lit
	  _                  -> intError "completeLitAlts" 
				         "literal pattern expected"


-- For the unusual case of having only one alternative containing a variable
-- pattern it is necessary to tranform it to a 'let' term because FlatCurry
-- does not support variable patterns in case alternatives. So the
-- case expression
--      case <ce> of 
--        x -> <expr>
-- is transformed ot
--      let x = <ce> in <expr>
completeVarAlts :: Expression -> [Alt] -> Expression
completeVarAlts expr [] = failedExpr
completeVarAlts expr (alt:_)
   = (Let (Binding (p_getVarIdent alt) expr) (getAltExpr alt))
 where
   p_getVarIdent alt
      = case (getAltPatt alt) of
	  VariablePattern ident -> ident
	  _                     -> intError "completeVarAlts" 
				            "variable pattern expected"


-------------------------------------------------------------------------------
-- The function 'removeRedundantAlts' removes case branches which are
-- either idle (i.e. they will never be reached) or multiply declared.
-- Note: unlike the PAKCS frontend MCC does not support warnings. So
-- there will be no messages if alternatives have been removed.
 
removeRedundantAlts :: [Message] -> [Alt] -> ([Alt], [Message])
removeRedundantAlts msgs alts
   = let
         (alts1, msgs1) = removeIdleAlts msgs alts
	 (alts2, msgs2) = removeMultipleAlts msgs1 alts1
     in
         (alts2, msgs2)


-- An alternative is idle if it occurs anywehere behind another alternative 
-- which contains a variable pattern. Example:
--    case x of
--      (y:ys) -> e1
--      z      -> e2
--      []     -> e3
-- Here all alternatives behind (z  -> e2) are idle and will be removed.
removeIdleAlts :: [Message] -> [Alt] -> ([Alt], [Message])
removeIdleAlts msgs alts 
   | null alts2 = (alts1, msgs)
   | otherwise  = (alts1, msgs)
 where
   (alts1, alts2) = splitAfter isVarAlt alts


-- An alternative occures multiply if at least two alternatives
-- use the same pattern. Example:
--    case x of
--      []     -> e1
--      (y:ys) -> e2
--      []     -> e3
-- Here the last alternative occures multiply because its pattern is already
-- used in the first alternative. All multiple alternatives will be
-- removed except for the first occurrence.
removeMultipleAlts :: [Message] -> [Alt] -> ([Alt], [Message])
removeMultipleAlts msgs alts
   = p_remove msgs [] alts
 where
   p_remove msgs altsR []     = ((reverse altsR), msgs)
   p_remove msgs altsR (alt:alts)
      | p_containsAlt alt altsR = p_remove msgs altsR alts
      | otherwise               = p_remove msgs (alt:altsR) alts

   p_containsAlt alt alts = any (p_eqAlt alt) alts

   p_eqAlt (Alt (LiteralPattern lit1) _) alt2
      = case alt2 of
	  (Alt (LiteralPattern lit2) _) -> lit1 == lit2
	  _                             -> False
   p_eqAlt (Alt (ConstructorPattern qident1 _) _) alt2
      = case alt2 of
	  (Alt (ConstructorPattern qident2 _) _) -> qident1 == qident2
	  _                                      -> False
   p_eqAlt (Alt (VariablePattern _) _) alt2
      = case alt2 of
	  (Alt (VariablePattern _) _) -> True
	  _                           -> False


-------------------------------------------------------------------------------
-- Some functions for testing and extracting terms from case alternatives

--
isVarAlt :: Alt -> Bool
isVarAlt alt = case (getAltPatt alt) of
	         VariablePattern _ -> True
		 _                 -> False

--
isConstrAlt :: Alt -> Bool
isConstrAlt alt = case (getAltPatt alt) of
		    ConstructorPattern _ _ -> True
		    _                      -> False

--
isLitAlt :: Alt -> Bool
isLitAlt alt = case (getAltPatt alt) of
	         LiteralPattern _ -> True
		 _                -> False


--
getAltExpr :: Alt -> Expression
getAltExpr (Alt _ expr) = expr


--
getAltPatt :: Alt -> ConstrTerm
getAltPatt (Alt cterm _) = cterm


-- Note: the newly generated variable 'x!' is just a dummy and will never
-- occur in the transformed program
getDefaultAlt :: [Alt] -> Alt
getDefaultAlt alts 
   = fromMaybe (Alt (VariablePattern (mkIdent "x!")) failedExpr)
               (find isVarAlt alts)


-------------------------------------------------------------------------------
-- This part of the module contains functions for replacing variables
-- with expressions. This is necessary in the case of having a default 
-- alternative like
--      v -> <expr>
-- where the variable v occurs in the default expression <expr>. When
-- building additional alternatives for this default expression the variable
-- must be replaced with the newly generated constructors.

-- Call:
--      replaceVar <variable id>
--                 <replace-with expression>
--                 <replace-in expression>
--
replaceVar :: Ident -> Expression -> Expression -> Expression
replaceVar ident expr (Variable ident')
   | ident == ident' = expr
   | otherwise       = Variable ident'
replaceVar ident expr (Apply expr1 expr2)
   = Apply (replaceVar ident expr expr1) (replaceVar ident expr expr2)
replaceVar ident expr (Case r eval expr' alts)
   = Case r eval 
          (replaceVar ident expr expr') 
	  (map (replaceVarInAlt ident expr) alts)
replaceVar ident expr (Or expr1 expr2)
   = Or (replaceVar ident expr expr1) (replaceVar ident expr expr2)
replaceVar ident expr (Exist ident' expr')
   | ident == ident' = Exist ident' expr'
   | otherwise       = Exist ident' (replaceVar ident expr expr')
replaceVar ident expr (Let binding expr')
   | varOccursInBinding ident binding
     = Let binding expr'
   | otherwise
     = Let (replaceVarInBinding ident expr binding) 
	   (replaceVar ident expr expr')
replaceVar ident expr (Letrec bindings expr')
   | any (varOccursInBinding ident) bindings
     = Letrec bindings expr'
   | otherwise
     = Letrec (map (replaceVarInBinding ident expr) bindings)
              (replaceVar ident expr expr')
replaceVar _ _ expr'
   = expr'


--
replaceVarInAlt :: Ident -> Expression -> Alt -> Alt
replaceVarInAlt ident expr (Alt patt expr')
   | varOccursInPattern ident patt 
     = Alt patt expr'
   | otherwise 
     = Alt patt (replaceVar ident expr expr')


--
replaceVarInBinding :: Ident -> Expression -> Binding -> Binding
replaceVarInBinding ident expr (Binding ident' expr')
   | ident == ident' = Binding ident' expr'
   | otherwise       = Binding ident' (replaceVar ident expr expr')


--
varOccursInPattern :: Ident -> ConstrTerm -> Bool
varOccursInPattern ident (VariablePattern ident')
   = ident == ident'
varOccursInPattern ident (ConstructorPattern _ idents)
   = elem ident idents
varOccursInPattern _ _
   = False


--
varOccursInBinding :: Ident -> Binding -> Bool
varOccursInBinding ident (Binding ident' _)
   = ident == ident'


-------------------------------------------------------------------------------
-- The following functions generate several IL expressions and patterns

--
failedExpr :: Expression
failedExpr = Function (qualifyWith preludeMIdent (mkIdent "failed")) 0

--
eqExpr :: Expression -> Expression -> Expression
eqExpr e1 e2 = Apply
	         (Apply 
		   (Function (qualifyWith preludeMIdent (mkIdent "==")) 2)
		   e1)
		 e2


--
truePatt :: ConstrTerm
truePatt = ConstructorPattern qTrueId []

--
falsePatt :: ConstrTerm
falsePatt = ConstructorPattern qFalseId []


--
cterm2expr :: ConstrTerm -> Expression
cterm2expr (LiteralPattern lit) = Literal lit
cterm2expr (ConstructorPattern qident args)
   = p_genApplic (Constructor qident (length args)) args
 where
   p_genApplic expr []     = expr
   p_genApplic expr (v:vs) = p_genApplic (Apply expr (Variable v)) vs
cterm2expr (VariablePattern ident) = Variable ident



-------------------------------------------------------------------------------
-- The folowing functions compute the missing constructors for generating
-- new case alternatives

-- Computes the complementary constructors for a list of constructors. All
-- specified constructors must have the same type.
-- This functions uses the module environment 'menv' which contains all known
-- constructors, except for those which are declared in the module and
-- except for the list constructors.
--
-- Call:
--      getComplConstr <IL module>
--                     <module environment>
--                     <list of (qualified) constructor ids>
--
getComplConstrs :: Module -> ModuleEnv -> [QualIdent] -> [(QualIdent, Int)]
getComplConstrs (Module mid _ decls) menv constrs
   | null constrs 
     = intError "getComplConstrs" "empty constructor list"
   | cons == qNilId || cons == qConsId
     = getCC constrs [(qNilId, 0), (qConsId, 2)]
   | mid' == mid
     = getCCFromDecls mid constrs decls
   | otherwise
     = maybe [] -- error ...
             (getCCFromIDecls mid' constrs) 
	     (lookupModule mid' menv)
 where
   cons = head constrs

   mid' = fromMaybe mid (qualidMod cons)


-- Find complementary constructors within the declarations of the
-- current module
getCCFromDecls :: ModuleIdent -> [QualIdent] -> [Decl] -> [(QualIdent, Int)]
getCCFromDecls _ constrs decls
   = let
         cdecls = maybe [] -- error ...
		        p_extractConstrDecls
			(find (p_declaresConstr (head constrs)) decls)
	 cinfos = map p_getConstrDeclInfo cdecls
     in
         getCC constrs cinfos
 where
   p_declaresConstr qident decl
      = case decl of
	  DataDecl _ _ cdecls   -> any (p_isConstrDecl qident) cdecls
	  NewtypeDecl _ _ cdecl -> p_isConstrDecl qident cdecl
	  _                     -> False

   p_isConstrDecl qident (ConstrDecl qid _) = qident == qid

   p_extractConstrDecls decl
      = case decl of
	  DataDecl _ _ cdecls   -> cdecls
	  _                     -> []

   p_getConstrDeclInfo (ConstrDecl qident types) = (qident, length types)


-- Find complementary constructors within the module environment
getCCFromIDecls :: ModuleIdent -> [QualIdent] -> [Curry.Syntax.IDecl] 
		   -> [(QualIdent, Int)]
getCCFromIDecls mident constrs idecls
   = let
         cdecls = maybe [] -- error ...
		        p_extractIConstrDecls
		        (find (p_declaresIConstr (head constrs)) idecls)
	 cinfos = map (p_getIConstrDeclInfo mident) cdecls
     in
         getCC constrs cinfos
 where
   p_declaresIConstr qident idecl
      = case idecl of
	  Curry.Syntax.IDataDecl _ _ _ cdecls
	      -> any (p_isIConstrDecl qident) 
		     (map fromJust (filter isJust cdecls))
	  Curry.Syntax.INewtypeDecl _ _ _ ncdecl 
	      -> p_isINewConstrDecl qident ncdecl
	  _   -> False

   p_isIConstrDecl qident (Curry.Syntax.ConstrDecl _ _ ident _)
      = (unqualify qident) == ident
   p_isIConstrDecl qident (Curry.Syntax.ConOpDecl _ _ _ ident _)
      = (unqualify qident) == ident

   p_isINewConstrDecl qident (Curry.Syntax.NewConstrDecl _ _ ident _)
      = (unqualify qident) == ident

   p_extractIConstrDecls idecl
      = case idecl of
	  Curry.Syntax.IDataDecl _ _ _ cdecls 
	      -> map fromJust (filter isJust cdecls)
	  _   -> []

   p_getIConstrDeclInfo mid (Curry.Syntax.ConstrDecl _ _ ident types)
      = (qualifyWith mid ident, length types)
   p_getIConstrDeclInfo mid (Curry.Syntax.ConOpDecl _ _ _ ident _)
      = (qualifyWith mid ident, 2)


-- Compute complementary constructors
getCC :: [QualIdent] -> [(QualIdent, Int)] -> [(QualIdent, Int)]
getCC _ [] = []
getCC constrs ((qident,arity):cis)
   | any ((==) qident) constrs = getCC constrs cis
   | otherwise                 = (qident,arity):(getCC constrs cis)


-------------------------------------------------------------------------------
-- Message handling
-- Not in use in this version, but intended for further versions

type Message = String


-------------------------------------------------------------------------------
-- Miscellaneous

-- Splits a list behind the first element which satify 'cond'
splitAfter :: (a -> Bool) -> [a] -> ([a], [a])
splitAfter cond xs = p_splitAfter cond [] xs
 where
   p_splitAfter c fs []     = ((reverse fs),[])
   p_splitAfter c fs (l:ls) | c l       = ((reverse (l:fs)), ls)
			    | otherwise = p_splitAfter c (l:fs) ls


-- Returns the first element which satisfy 'cond'. The returned element is
-- embedded in a 'Maybe' term
find :: (a -> Bool) -> [a] -> Maybe a
find _    []     = Nothing
find cond (x:xs) | cond x    = Just x
		 | otherwise = find cond xs


-- Prefixes an element to a list if it does not already exit within the
-- list
insertUnique :: Eq a => a -> [a] -> [a]
insertUnique x xs | elem x xs = xs
		  | otherwise = x:xs


-- Raises an internal error
intError :: String -> String -> a
intError fun msg = error ("CaseCompletion." ++ fun ++ " - " ++ msg)


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
