module Curry.Syntax.Utils(Expr, fv, qfv,
                          QuantExpr, bv,

                          isEvalAnnot, isTypeSig,
                          infixOp,
                          isTypeDecl, isValueDecl,
                          isInfixDecl,
                          isRecordDecl, isImportDecl) where

import qualified Data.Set as Set


import Curry.Base.Ident 
import Curry.Syntax.Type

{-
  Free and bound variables
  
  The compiler needs to compute the sets of free and bound variables for
  various different entities. We will devote three type classes to that
  purpose. The \texttt{QualExpr} class is expected to take into account
  that it is possible to use a qualified name to refer to a function
  defined in the current module and therefore \emph{M.x} and $x$, where
  $M$ is the current module name, should be considered the same name.
  However note that this is correct only after renaming all local
  definitions as \emph{M.x} always denotes an entity defined at the
  top-level.
  
  The \texttt{Decl} instance of \texttt{QualExpr} returns all free
  variables on the right hand side, regardless of whether they are bound
  on the left hand side. This is more convenient as declarations are
  usually processed in a declaration group where the set of free
  variables cannot be computed independently for each declaration. Also
  note that the operator in a unary minus expression is not a free
  variable. This operator always refers to a global function from the
  prelude.
-}

class Expr e where
  fv :: e -> [Ident]
class QualExpr e where
  qfv :: ModuleIdent -> e -> [Ident]
class QuantExpr e where
  bv :: e -> [Ident]

instance Expr e => Expr [e] where
  fv = concat . map fv
instance QualExpr e => QualExpr [e] where
  qfv m = concat . map (qfv m)
instance QuantExpr e => QuantExpr [e] where
  bv = concat . map bv

instance QualExpr Decl where
  qfv m (FunctionDecl _ _ eqs) = qfv m eqs
  qfv m (PatternDecl _ _ rhs) = qfv m rhs
  qfv _ _ = []

instance QuantExpr Decl where
  bv (TypeSig _ vs _) = vs
  bv (EvalAnnot _ fs _) = fs
  bv (FunctionDecl _ f _) = [f]
  bv (ExternalDecl _ _ _ f _) = [f]
  bv (FlatExternalDecl _ fs) = fs
  bv (PatternDecl _ t _) = bv t
  bv (ExtraVariables _ vs) = vs
  bv _ = []

instance QualExpr Equation where
  qfv m (Equation _ lhs rhs) = filterBv lhs (qfv m lhs ++ qfv m rhs)

instance QuantExpr Lhs where
  bv = bv . snd . flatLhs

instance QualExpr Lhs where
  qfv m lhs = qfv m (snd (flatLhs lhs))

instance QualExpr Rhs where
  qfv m (SimpleRhs _ e ds) = filterBv ds (qfv m e ++ qfv m ds)
  qfv m (GuardedRhs es ds) = filterBv ds (qfv m es ++ qfv m ds)

instance QualExpr CondExpr where
  qfv m (CondExpr _ g e) = qfv m g ++ qfv m e

instance QualExpr Expression where
  qfv _ (Literal _) = []
  qfv m (Variable v) = maybe [] return (localIdent m v)
  qfv _ (Constructor _) = []
  qfv m (Paren e) = qfv m e
  qfv m (Typed e _) = qfv m e
  qfv m (Tuple _ es) = qfv m es
  qfv m (List _ es) = qfv m es
  qfv m (ListCompr _ e qs) = foldr (qfvStmt m) (qfv m e) qs
  qfv m (EnumFrom e) = qfv m e
  qfv m (EnumFromThen e1 e2) = qfv m e1 ++ qfv m e2
  qfv m (EnumFromTo e1 e2) = qfv m e1 ++ qfv m e2
  qfv m (EnumFromThenTo e1 e2 e3) = qfv m e1 ++ qfv m e2 ++ qfv m e3
  qfv m (UnaryMinus _ e) = qfv m e
  qfv m (Apply e1 e2) = qfv m e1 ++ qfv m e2
  qfv m (InfixApply e1 op e2) = qfv m op ++ qfv m e1 ++ qfv m e2
  qfv m (LeftSection e op) = qfv m op ++ qfv m e
  qfv m (RightSection op e) = qfv m op ++ qfv m e
  qfv m (Lambda _ ts e) = filterBv ts (qfv m e)
  qfv m (Let ds e) = filterBv ds (qfv m ds ++ qfv m e)
  qfv m (Do sts e) = foldr (qfvStmt m) (qfv m e) sts
  qfv m (IfThenElse _ e1 e2 e3) = qfv m e1 ++ qfv m e2 ++ qfv m e3
  qfv m (Case _ e alts) = qfv m e ++ qfv m alts
  qfv m (RecordConstr fs) = qfv m fs
  qfv m (RecordSelection e _) = qfv m e
  qfv m (RecordUpdate fs e) = qfv m e ++ qfv m fs

qfvStmt :: ModuleIdent -> Statement -> [Ident] -> [Ident]
qfvStmt m st fvs = qfv m st ++ filterBv st fvs

instance QualExpr Statement where
  qfv m (StmtExpr _ e) = qfv m e
  qfv m (StmtDecl ds) = filterBv ds (qfv m ds)
  qfv m (StmtBind _ t e) = qfv m e

instance QualExpr Alt where
  qfv m (Alt _ t rhs) = filterBv t (qfv m rhs)

instance QuantExpr a => QuantExpr (Field a) where
  bv (Field _ _ t) = bv t

instance QualExpr a => QualExpr (Field a) where
  qfv m (Field _ _ t) = qfv m t

instance QuantExpr Statement where
  bv (StmtExpr _ e) = []
  bv (StmtBind _ t e) = bv t
  bv (StmtDecl ds) = bv ds

instance QualExpr InfixOp where
  qfv m (InfixOp op) = qfv m (Variable op)
  qfv _ (InfixConstr _) = []

instance QuantExpr ConstrTerm where
  bv (LiteralPattern _) = []
  bv (NegativePattern _ _) = []
  bv (VariablePattern v) = [v]
  bv (ConstructorPattern c ts) = bv ts
  bv (InfixPattern t1 op t2) = bv t1 ++ bv t2
  bv (ParenPattern t) = bv t
  bv (TuplePattern _ ts) = bv ts
  bv (ListPattern _ ts) = bv ts
  bv (AsPattern v t) = v : bv t
  bv (LazyPattern _ t) = bv t
  bv (FunctionPattern f ts) = bvFuncPatt (FunctionPattern f ts)
  bv (InfixFuncPattern t1 op t2) = bvFuncPatt (InfixFuncPattern t1 op t2)
  bv (RecordPattern fs r) = (maybe [] bv r) ++ bv fs

instance QualExpr ConstrTerm where
  qfv _ (LiteralPattern _) = []
  qfv _ (NegativePattern _ _) = []
  qfv _ (VariablePattern _) = []
  qfv m (ConstructorPattern _ ts) = qfv m ts
  qfv m (InfixPattern t1 _ t2) = qfv m [t1,t2]
  qfv m (ParenPattern t) = qfv m t
  qfv m (TuplePattern _ ts) = qfv m ts
  qfv m (ListPattern _ ts) = qfv m ts
  qfv m (AsPattern _ ts) = qfv m ts
  qfv m (LazyPattern _ t) = qfv m t
  qfv m (FunctionPattern f ts) 
    = (maybe [] return (localIdent m f)) ++ qfv m ts
  qfv m (InfixFuncPattern t1 op t2) 
    = (maybe [] return (localIdent m op)) ++ qfv m [t1,t2]
  qfv m (RecordPattern fs r) = (maybe [] (qfv m) r) ++ qfv m fs

instance Expr TypeExpr where
  fv (ConstructorType _ tys) = fv tys
  fv (VariableType tv)
    | tv == anonId = []
    | otherwise = [tv]
  fv (TupleType tys) = fv tys
  fv (ListType ty) = fv ty
  fv (ArrowType ty1 ty2) = fv ty1 ++ fv ty2
  fv (RecordType fs rty) = (maybe [] fv rty) ++ fv (map snd fs)

filterBv :: QuantExpr e => e -> [Ident] -> [Ident]
filterBv e = filter (`Set.notMember` Set.fromList (bv e))

{-
  Since multiple variable occurrences are allowed in function patterns,
  it is necessary to compute the list of bound variables in a different way:
  Each variable occuring in the function pattern will be unique in the result
  list.
 -}

bvFuncPatt :: ConstrTerm -> [Ident]
bvFuncPatt = bvfp []
 where
 bvfp bvs (LiteralPattern _) = bvs
 bvfp bvs (NegativePattern _ _) = bvs
 bvfp bvs (VariablePattern v)
    | elem v bvs = bvs
    | otherwise  = v:bvs
 bvfp bvs (ConstructorPattern c ts) = foldl bvfp bvs ts
 bvfp bvs (InfixPattern t1 op t2) = foldl bvfp bvs [t1,t2]
 bvfp bvs (ParenPattern t) = bvfp bvs t
 bvfp bvs (TuplePattern _ ts) = foldl bvfp bvs ts
 bvfp bvs (ListPattern _ ts) = foldl bvfp bvs ts
 bvfp bvs (AsPattern v t)
    | elem v bvs = bvfp bvs t
    | otherwise  = bvfp (v:bvs) t
 bvfp bvs (LazyPattern _ t) = bvfp bvs t
 bvfp bvs (FunctionPattern f ts) = foldl bvfp bvs ts
 bvfp bvs (InfixFuncPattern t1 op t2) = foldl bvfp bvs [t1, t2]
 bvfp bvs (RecordPattern fs r)
    = foldl bvfp (maybe bvs (bvfp bvs) r) (map fieldTerm fs)



{-
  Here is a list of predicates identifying various kinds of
  declarations.
-}

isImportDecl, isInfixDecl, isTypeDecl :: Decl -> Bool
isTypeSig, isEvalAnnot, isValueDecl :: Decl -> Bool

isImportDecl (ImportDecl _ _ _ _ _) = True
isImportDecl _ = False

isInfixDecl (InfixDecl _ _ _ _) = True
isInfixDecl _ = False

isTypeDecl (DataDecl _ _ _ _) = True
isTypeDecl (NewtypeDecl _ _ _ _) = True
isTypeDecl (TypeDecl _ _ _ _) = True
isTypeDecl _ = False

isTypeSig (TypeSig _ _ _) = True
isTypeSig (ExternalDecl _ _ _ _ _) = True
isTypeSig _ = False

isEvalAnnot (EvalAnnot _ _ _) = True
isEvalAnnot _ = False

isValueDecl (FunctionDecl _ _ _) = True
isValueDecl (ExternalDecl _ _ _ _ _) = True
isValueDecl (FlatExternalDecl _ _) = True
isValueDecl (PatternDecl _ _ _) = True
isValueDecl (ExtraVariables _ _) = True
isValueDecl _ = False

isRecordDecl (TypeDecl _ _ _ (RecordType _ _)) = True
isRecordDecl _ = False


{-
  The function \texttt{infixOp} converts an infix operator into an
  expression.
-}

infixOp :: InfixOp -> Expression
infixOp (InfixOp op) = Variable op
infixOp (InfixConstr op) = Constructor op
