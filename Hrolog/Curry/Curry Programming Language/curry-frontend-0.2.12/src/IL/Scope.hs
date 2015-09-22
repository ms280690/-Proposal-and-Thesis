module IL.Scope (getModuleScope,
		insertDeclScope, insertConstrDeclScope,
		insertCallConvScope, insertTypeScope,
		insertLiteralScope, insertConstrTermScope,
		insertExprScope, insertAltScope,
		insertBindingScope) where

import Curry.Base.Ident

import IL.Type
import OldScopeEnv as ScopeEnv


-------------------------------------------------------------------------------

--
getModuleScope :: Module -> ScopeEnv
getModuleScope (Module _ _ decls) = foldl insertDecl newScopeEnv decls


--
insertDeclScope :: ScopeEnv -> Decl -> ScopeEnv
insertDeclScope env (DataDecl _ _ _) = env
insertDeclScope env (NewtypeDecl _ _ _) = env
insertDeclScope env (FunctionDecl _ params _ _)
   = foldr ScopeEnv.insertIdent (ScopeEnv.beginScope env) params
insertDeclScope env (ExternalDecl _ _ _ _) = env


--
insertConstrDeclScope :: ScopeEnv -> ConstrDecl [Type] -> ScopeEnv
insertConstrDeclScope env _ = env


--
insertCallConvScope :: ScopeEnv -> CallConv -> ScopeEnv
insertCallConvScope env _ = env


--
insertTypeScope :: ScopeEnv -> Type -> ScopeEnv
insertTypeScope env _ = env


--
insertLiteralScope :: ScopeEnv -> Literal -> ScopeEnv
insertLiteralScope env _ = env


--
insertConstrTermScope :: ScopeEnv -> ConstrTerm -> ScopeEnv
insertConstrTermScope env _ = env


--
insertExprScope :: ScopeEnv -> Expression -> ScopeEnv
insertExprScope env (Literal _) = env
insertExprScope env (Variable _) = env
insertExprScope env (Function _ _) = env
insertExprScope env (Constructor _ _) = env
insertExprScope env (Apply _ _) = env
insertExprScope env (Case _ _ _ _) = env
insertExprScope env (Or _ _) = env
insertExprScope env (Exist ident _)
   = ScopeEnv.insertIdent ident (ScopeEnv.beginScope env)
insertExprScope env (Let bind _)
   = insertBinding (beginScope env) bind
insertExprScope env (Letrec binds _)
   = foldl insertBinding (beginScope env) binds


--
insertAltScope :: ScopeEnv -> Alt -> ScopeEnv
insertAltScope env (Alt cterm _)
   = insertConstrTerm (ScopeEnv.beginScope env) cterm


--
insertBindingScope :: ScopeEnv -> Binding -> ScopeEnv
insertBindingScope env _ = env


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

--
insertDecl :: ScopeEnv -> Decl -> ScopeEnv
insertDecl env (DataDecl qident _ cdecls)
   = foldl insertConstrDecl
	 (ScopeEnv.insertIdent (unqualify qident) env)
	 cdecls

insertDecl env (NewtypeDecl qident _ cdecl)
   = insertConstrDecl (ScopeEnv.insertIdent (unqualify qident) env) cdecl

insertDecl env (FunctionDecl qident _ _ _)
   = ScopeEnv.insertIdent (unqualify qident) env

insertDecl env (ExternalDecl qident _ _ _)
   = ScopeEnv.insertIdent (unqualify qident) env


--
insertConstrDecl :: ScopeEnv -> ConstrDecl a -> ScopeEnv
insertConstrDecl env (ConstrDecl qident _)
   = ScopeEnv.insertIdent (unqualify qident) env


--
insertConstrTerm :: ScopeEnv -> ConstrTerm -> ScopeEnv
insertConstrTerm env (LiteralPattern _) = env
insertConstrTerm env (ConstructorPattern _ params)
   = foldr ScopeEnv.insertIdent env params
insertConstrTerm env (VariablePattern ident)
   = ScopeEnv.insertIdent ident env


--
insertBinding :: ScopeEnv -> Binding -> ScopeEnv
insertBinding env (Binding ident _) = ScopeEnv.insertIdent ident env


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
