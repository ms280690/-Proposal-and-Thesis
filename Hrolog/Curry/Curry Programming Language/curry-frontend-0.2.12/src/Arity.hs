-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--
-- Arity - provides functions for expanding the arity environment 'ArityEnv'
--         (see Module "Base")
--
-- September 2005,
-- Martin Engelke (men@informatik.uni-kiel.de)
--
module Arity (bindArities) where

import Curry.Base.Ident
import Curry.Syntax

import Base(ArityEnv, bindArity)


-------------------------------------------------------------------------------

-- Expands the arity envorinment with (global / local) function arities and
-- constructor arities
bindArities :: ArityEnv -> Module -> ArityEnv
bindArities aEnv (Module mid _ decls)
   = foldl (visitDecl mid) aEnv decls


-------------------------------------------------------------------------------

visitDecl :: ModuleIdent -> ArityEnv -> Decl -> ArityEnv
visitDecl mid aEnv (DataDecl _ _ _ cdecls)
   = foldl (visitConstrDecl mid) aEnv cdecls
visitDecl mid aEnv (ExternalDecl _ _ _ id texpr)
   = bindArity mid id (typeArity texpr) aEnv
visitDecl mid aEnv (FunctionDecl _ id equs)
   = let (Equation _ lhs rhs) = head equs
     in  visitRhs mid (visitLhs mid id aEnv lhs) rhs
visitDecl _ aEnv _ = aEnv


visitConstrDecl :: ModuleIdent -> ArityEnv -> ConstrDecl -> ArityEnv
visitConstrDecl mid aEnv (ConstrDecl _ _ id texprs)
   = bindArity mid id (length texprs) aEnv
visitConstrDecl mid aEnv (ConOpDecl _ _ _ id _)
   = bindArity mid id 2 aEnv


visitLhs :: ModuleIdent -> Ident -> ArityEnv -> Lhs -> ArityEnv
visitLhs mid _ aEnv (FunLhs id params)
   = bindArity mid id (length params) aEnv
visitLhs mid id aEnv (OpLhs _ _ _)
   = bindArity mid id 2 aEnv
visitLhs _ _ aEnv _ = aEnv


visitRhs :: ModuleIdent -> ArityEnv -> Rhs -> ArityEnv
visitRhs mid aEnv (SimpleRhs _ expr decls)
   = foldl (visitDecl mid) (visitExpression mid aEnv expr) decls
visitRhs mid aEnv (GuardedRhs cexprs decls)
   = foldl (visitDecl mid) (foldl (visitCondExpr mid) aEnv cexprs) decls


visitCondExpr :: ModuleIdent -> ArityEnv -> CondExpr -> ArityEnv
visitCondExpr mid aEnv (CondExpr _ cond expr)
   = visitExpression mid (visitExpression mid aEnv expr) cond


visitExpression :: ModuleIdent -> ArityEnv -> Expression -> ArityEnv
visitExpression mid aEnv (Paren expr)
   = visitExpression mid aEnv expr
visitExpression mid aEnv (Typed expr _)
   = visitExpression mid aEnv expr
visitExpression mid aEnv (Tuple _ exprs)
   = foldl (visitExpression mid) aEnv exprs
visitExpression mid aEnv (List _ exprs)
   = foldl (visitExpression mid) aEnv exprs
visitExpression mid aEnv (ListCompr _ expr stmts)
   = foldl (visitStatement mid) (visitExpression mid aEnv expr) stmts
visitExpression mid aEnv (EnumFrom expr)
   = visitExpression mid aEnv expr
visitExpression mid aEnv (EnumFromThen expr1 expr2)
   = foldl (visitExpression mid) aEnv [expr1,expr2]
visitExpression mid aEnv (EnumFromTo expr1 expr2)
   = foldl (visitExpression mid) aEnv [expr1,expr2]
visitExpression mid aEnv (EnumFromThenTo expr1 expr2 expr3)
   = foldl (visitExpression mid) aEnv [expr1,expr2,expr3]
visitExpression mid aEnv (UnaryMinus _ expr)
   = visitExpression mid aEnv expr
visitExpression mid aEnv (Apply expr1 expr2)
   = foldl (visitExpression mid) aEnv [expr1,expr2]
visitExpression mid aEnv (InfixApply expr1 _ expr2)
   = foldl (visitExpression mid) aEnv [expr1,expr2]
visitExpression mid aEnv (LeftSection expr _)
   = visitExpression mid aEnv expr
visitExpression mid aEnv (RightSection _ expr)
   = visitExpression mid aEnv expr
visitExpression mid aEnv (Lambda _ _ expr)
   = visitExpression mid aEnv expr
visitExpression mid aEnv (Let decls expr)
   = foldl (visitDecl mid) (visitExpression mid aEnv expr) decls
visitExpression mid aEnv (Do stmts expr)
   = foldl (visitStatement mid) (visitExpression mid aEnv expr) stmts
visitExpression mid aEnv (IfThenElse _ expr1 expr2 expr3)
   = foldl (visitExpression mid) aEnv [expr1,expr2,expr3]
visitExpression mid aEnv (Case _ expr alts)
   = visitExpression mid (foldl (visitAlt mid) aEnv alts) expr
visitExpression _ aEnv _ = aEnv


visitStatement :: ModuleIdent -> ArityEnv -> Statement -> ArityEnv
visitStatement mid aEnv (StmtExpr _ expr)
   = visitExpression mid aEnv expr
visitStatement mid aEnv (StmtDecl decls)
   = foldl (visitDecl mid) aEnv decls
visitStatement mid aEnv (StmtBind _ _ expr)
   = visitExpression mid aEnv expr


visitAlt :: ModuleIdent -> ArityEnv -> Alt -> ArityEnv
visitAlt mid aEnv (Alt _ _ rhs)
   = visitRhs mid aEnv rhs



-------------------------------------------------------------------------------

-- Computes the function arity using a type expression
typeArity :: TypeExpr -> Int
typeArity (ArrowType _ t2) = 1 + typeArity t2
typeArity _                = 0


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
