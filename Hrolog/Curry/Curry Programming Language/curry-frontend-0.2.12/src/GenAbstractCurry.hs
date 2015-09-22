-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--
-- GenAbstractCurry - Generates an AbstractCurry program term
--                    (type 'CurryProg')
--
-- July 2005,
-- Martin Engelke (men@informatik.uni-kiel.de)
--
module GenAbstractCurry (genTypedAbstract, 
			 genUntypedAbstract) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List
import Data.Char

import Curry.Syntax
import Curry.Syntax.Utils
import Curry.AbstractCurry

import Base
import Types
import Curry.Base.Ident
import Curry.Base.Position
import TopEnv


-------------------------------------------------------------------------------

-- Generates standard (type infered) AbstractCurry code from a CurrySyntax
-- module. The function needs the type environment 'tyEnv' to determin the
-- infered function types.
genTypedAbstract :: ValueEnv -> TCEnv -> Module -> CurryProg
genTypedAbstract tyEnv tcEnv mod
   = genAbstract (genAbstractEnv TypedAcy tyEnv tcEnv mod) mod


-- Generates untyped AbstractCurry code from a CurrySyntax module. The type
-- signature takes place in every function type annotation, if it exists, 
-- otherwise the dummy type "Prelude.untyped" is used.
genUntypedAbstract :: ValueEnv -> TCEnv -> Module -> CurryProg
genUntypedAbstract tyEnv tcEnv mod
   = genAbstract (genAbstractEnv UntypedAcy tyEnv tcEnv mod) mod


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- Private...

-- Generates an AbstractCurry program term from the syntax tree
genAbstract :: AbstractEnv -> Module -> CurryProg
genAbstract env (Module mid exp decls)
   = let partitions = foldl partitionDecl emptyPartitions decls
         modname    = moduleName mid 
	 (imps, _)  
	     = mapfoldl genImportDecl env (reverse (importDecls partitions))
	 (types, _) 
	     = mapfoldl genTypeDecl env (reverse (typeDecls partitions))
	 (_, funcs) 
	     = Map.mapAccumWithKey (genFuncDecl False) 
	                env 
			(funcDecls partitions)
	 (ops, _)   
	     = mapfoldl genOpDecl env (reverse (opDecls partitions))
     in  CurryProg modname imps types (Map.elems funcs) ops


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- The following types and functions can be used to spread a list of
-- CurrySyntax declarations into four parts: a list of imports, a list of
-- type declarations (data types and type synonyms), a table of function
-- declarations and a list of fixity declarations.


-- Inserts a CurrySyntax top level declaration into a partition.
-- Note: declarations are collected in reverse order.
partitionDecl :: Partitions -> Decl -> Partitions
partitionDecl partitions (TypeSig pos ids typeexpr)
   = partitionFuncDecls (\id -> TypeSig pos [id] typeexpr) partitions ids
partitionDecl partitions (EvalAnnot pos ids annot)
   = partitionFuncDecls (\id -> EvalAnnot pos [id] annot) partitions ids
partitionDecl partitions (FunctionDecl pos id equs)
   = partitionFuncDecls (const (FunctionDecl pos id equs)) partitions [id]
partitionDecl partitions (ExternalDecl pos conv name id typeexpr)
   = partitionFuncDecls (const (ExternalDecl pos conv name id typeexpr))
                     partitions
		     [id]
partitionDecl partitions (FlatExternalDecl pos ids)
   = partitionFuncDecls (\id -> FlatExternalDecl pos [id]) partitions ids
partitionDecl partitions (InfixDecl pos fix prec idents)
   = partitions {opDecls = map (\id -> (InfixDecl pos fix prec [id])) idents
		           ++ opDecls partitions }
partitionDecl partitions decl
   = case decl of
       ImportDecl _ _ _ _ _ 
         -> partitions {importDecls = decl: importDecls partitions }
       DataDecl _ _ _ _     
         -> partitions {typeDecls = decl : typeDecls partitions }
       TypeDecl _ _ _ _     
         -> partitions {typeDecls = decl : typeDecls partitions }
       _ -> partitions


--
partitionFuncDecls :: (Ident -> Decl) -> Partitions -> [Ident] -> Partitions
partitionFuncDecls genDecl partitions ids
   = partitions {funcDecls = foldl partitionFuncDecl (funcDecls partitions) ids}
 where
   partitionFuncDecl funcs' id
      = Map.insert id (genDecl id : fromMaybe [] (Map.lookup id funcs')) funcs'


-- Data type for representing partitions of CurrySyntax declarations
-- (according to the definition of the AbstractCurry program
-- representation; type 'CurryProg').
-- Since a complete function declaration usually consist of more than one
-- declaration (e.g. rules, type signature etc.), it is necessary 
-- to collect them within an association list
data Partitions = Partitions {importDecls :: [Decl],
			      typeDecls   :: [Decl],
			      funcDecls   :: Map.Map Ident [Decl],
			      opDecls     :: [Decl]
			     } deriving Show

-- Generates initial partitions.
emptyPartitions = Partitions {importDecls = [],
			      typeDecls   = [],
			      funcDecls   = Map.empty,
			      opDecls     = []
			     } 


-------------------------------------------------------------------------------
-- The following functions convert CurrySyntax terms to AbstractCurry
-- terms.

--
genImportDecl :: AbstractEnv -> Decl -> (String, AbstractEnv)
genImportDecl env (ImportDecl _ mid _ _ _) = (moduleName mid, env)


--
genTypeDecl :: AbstractEnv -> Decl -> (CTypeDecl, AbstractEnv)
genTypeDecl env (DataDecl _ ident params cdecls)
   = let (idxs, env1)    = mapfoldl genTVarIndex env params
	 (cdecls', env2) = mapfoldl genConsDecl env1 cdecls
     in  (CType (genQName True env2 (qualifyWith (moduleId env) ident))
	        (genVisibility env2 ident)
	        (zip idxs (map name params))
	        cdecls',
	  resetScope env2)
genTypeDecl env (TypeDecl _ ident params typeexpr)
   = let (idxs, env1)      = mapfoldl genTVarIndex env params
	 (typeexpr', env2) = genTypeExpr env1 typeexpr
     in  (CTypeSyn (genQName True env2 (qualifyWith (moduleId env) ident))
	           (genVisibility env2 ident)
	           (zip idxs (map name params))
	           typeexpr',
	  resetScope env2)
genTypeDecl env (NewtypeDecl pos ident _ _)
   = errorAt pos "'newtype' declarations are not supported in AbstractCurry"
genTypeDecl env _
   = internalError "unexpected declaration"


--
genConsDecl :: AbstractEnv -> ConstrDecl -> (CConsDecl, AbstractEnv)
genConsDecl env (ConstrDecl _ _ ident params)
   = let (params', env') = mapfoldl genTypeExpr env params
     in  (CCons (genQName False env' (qualifyWith (moduleId env) ident))
	        (length params)
	        (genVisibility env' ident)
	        params',
	  env')
genConsDecl env (ConOpDecl pos ids ltype ident rtype)
   = genConsDecl env (ConstrDecl pos ids ident [ltype, rtype])


--
genTypeExpr :: AbstractEnv -> TypeExpr -> (CTypeExpr, AbstractEnv)
genTypeExpr env (ConstructorType qident targs)
   = let (targs', env') = mapfoldl genTypeExpr env targs
     in  (CTCons (genQName True env' qident) targs', env')
genTypeExpr env (VariableType ident)
   | isJust midx = (CTVar (fromJust midx, name ident), env)
   | otherwise   = (CTVar (idx, name ident), env')
 where
   midx        = getTVarIndex env ident
   (idx, env') = genTVarIndex env ident
genTypeExpr env (TupleType targs)
   | len > 1   = genTypeExpr env (ConstructorType (qTupleId len) targs)
   | len == 0  = genTypeExpr env (ConstructorType qUnitId targs)
   | len == 1  = genTypeExpr env (head targs)
 where len = length targs
genTypeExpr env (ListType typeexpr)
   = genTypeExpr env (ConstructorType qListId [typeexpr])
genTypeExpr env (ArrowType texpr1 texpr2)
   = let (texpr1', env1) = genTypeExpr env texpr1
	 (texpr2', env2) = genTypeExpr env1 texpr2
     in  (CFuncType texpr1' texpr2', env2)
genTypeExpr env (RecordType fss mr)
   = let fs = concatMap (\ (ls,typeexpr) -> map (\l -> (l,typeexpr)) ls) fss
         (ls,ts) = unzip fs
         (ts',env1) = mapfoldl genTypeExpr env ts
         ls' = map name ls
     in case mr of
           Nothing
             -> (CRecordType (zip ls' ts') Nothing, env1)
           Just tvar@(VariableType _)
             -> let (CTVar iname, env2) = genTypeExpr env1 tvar
                in  (CRecordType (zip ls' ts') (Just iname), env2)
           (Just r@(RecordType _ _))
             -> let (CRecordType fields rbase, env2) = genTypeExpr env1 r
		    fields' = foldr (uncurry insertEntry) 
				    fields
			            (zip ls' ts')
		in  (CRecordType fields' rbase, env2)
           _ -> internalError "illegal record base"


-- NOTE: every infix declaration must declare exactly one operator.
genOpDecl :: AbstractEnv -> Decl -> (COpDecl, AbstractEnv)
genOpDecl env (InfixDecl _ fix prec [ident])
   = (COp (genQName False env (qualifyWith (moduleId env) ident))
          (genFixity fix)
          (fromInteger prec),
      env)


--
genFixity :: Infix -> CFixity
genFixity InfixL = CInfixlOp
genFixity InfixR = CInfixrOp
genFixity Infix  = CInfixOp


-- Generate an AbstractCurry function declaration from a list of CurrySyntax
-- function declarations.
-- NOTES: 
--   - every declaration in 'decls' must declare exactly one function.
--   - since infered types are internally represented in flat style,
--     all type variables are renamed with generated symbols when
--     generating typed AbstractCurry.
genFuncDecl :: Bool -> AbstractEnv -> Ident -> [Decl] -> (AbstractEnv, CFuncDecl)
genFuncDecl isLocal env ident decls
   | not (null decls)
     = let name          = genQName False env (qualify ident)
	   visibility    = genVisibility env ident
           evalannot     = maybe CFlex 
	                         (\ (EvalAnnot _ _ ea) -> genEvalAnnot ea)
				 (find isEvalAnnot decls)
           (mtype, env1) = maybe (Nothing, env) 
                                 (\ (t, env') -> (Just t, env'))
				 (genFuncType env decls)
	   (rules, env2) = maybe ([], env1)
			         (\ (FunctionDecl _ _ equs)
				  -> mapfoldl genRule env1 equs)
				 (find isFunctionDecl decls)
           mexternal     = fmap genExternal (find isExternal decls)
	   arity         = compArity mtype rules
           typeexpr      = fromMaybe (CTCons ("Prelude","untyped") []) mtype
           rule          = compRule evalannot rules mexternal
           env3          = if isLocal then env1 else resetScope env2
       in  (env3, CFunc name arity visibility typeexpr rule)
   | otherwise
     = internalError ("missing declaration for function \""
		      ++ show ident ++ "\"")
 where
   genFuncType env decls
      | acytype == UntypedAcy
	= fmap (genTypeSig env) (find isTypeSig decls)
      | acytype == TypedAcy
	= fmap (genTypeExpr env) mftype
      | otherwise 
	= Nothing
    where 
    acytype = acyType env
    mftype  | isLocal   
	      = lookupType ident (typeEnv env)
	    | otherwise 
	      = qualLookupType (qualifyWith (moduleId env) ident)
	                       (typeEnv env)

   genTypeSig env (TypeSig _ _ ts)          = genTypeExpr env ts
   genTypeSig env (ExternalDecl _ _ _ _ ts) = genTypeExpr env ts

   genExternal (ExternalDecl _ _ mname ident _)
      = CExternal (fromMaybe (name ident) mname)
   genExternal (FlatExternalDecl _ [ident])
      = CExternal (name ident)
   genExternal _
      = internalError "illegal external declaration occured"

   compArity mtypeexpr rules
      | not (null rules)
        = let (CRule patts _ _) = head rules in length patts
      | otherwise
        = maybe (internalError ("unable to compute arity for function \""
				++ show ident ++ "\""))
	        compArityFromType
		mtypeexpr

   compArityFromType (CTVar _)        = 0
   compArityFromType (CFuncType _ t2) = 1 + compArityFromType t2
   compArityFromType (CTCons _ _)     = 0

   compRule evalannot rules mexternal
      | not (null rules) = CRules evalannot rules
      | otherwise
	= fromMaybe (internalError ("missing rule for function \""
				    ++ show ident ++ "\""))
	            mexternal


--
genRule :: AbstractEnv -> Equation -> (CRule, AbstractEnv)
genRule env (Equation pos lhs rhs)
   = let (patts, env1)  = mapfoldl (genPattern pos)
			           (beginScope env) 
				   (simplifyLhs lhs)
	 (locals, env2) = genLocalDecls env1 (simplifyRhsLocals rhs)
	 (crhss, env3)  = mapfoldl (genCrhs pos) env2 (simplifyRhsExpr rhs)
     in  (CRule patts crhss locals, endScope env3)


--
genCrhs :: Position -> AbstractEnv -> (Expression, Expression) 
           -> ((CExpr, CExpr), AbstractEnv)
genCrhs pos env (cond, expr)
   = let (cond', env1) = genExpr pos env cond
	 (expr', env2) = genExpr pos env1 expr
     in  ((cond', expr'), env2)


-- NOTE: guarded expressions and 'where' declarations in local pattern
-- declarations are not supported in PAKCS
genLocalDecls :: AbstractEnv -> [Decl] -> ([CLocalDecl], AbstractEnv)
genLocalDecls env decls
   = genLocals (foldl genLocalIndex env decls)
               (funcDecls (foldl partitionDecl emptyPartitions decls))
	       decls
 where
   genLocalIndex env (PatternDecl _ constr _)
      = genLocalPatternIndex env constr
   genLocalIndex env (ExtraVariables _ idents)
      = let (_, env') = mapfoldl genVarIndex env idents
	in  env'
   genLocalIndex env _
       = env

   genLocalPatternIndex env (VariablePattern ident)
      = snd (genVarIndex env ident)
   genLocalPatternIndex env (ConstructorPattern _ args)
      = foldl genLocalPatternIndex env args
   genLocalPatternIndex env (InfixPattern c1 _ c2)
      = foldl genLocalPatternIndex env [c1,c2]
   genLocalPatternIndex env (ParenPattern c)
      = genLocalPatternIndex env c
   genLocalPatternIndex env (TuplePattern _ args)
      = foldl genLocalPatternIndex env args
   genLocalPatternIndex env (ListPattern _ args)
      = foldl genLocalPatternIndex env args
   genLocalPatternIndex env (AsPattern ident c)
      = genLocalPatternIndex (snd (genVarIndex env ident)) c
   genLocalPatternIndex env (LazyPattern _ c)
      = genLocalPatternIndex env c
   genLocalPatternIndex env (RecordPattern fields mc)
      = let env' = foldl genLocalPatternIndex env (map fieldTerm fields)
        in  maybe env' (genLocalPatternIndex env') mc
   genLocalPatternIndex env _
      = env

   -- The association list 'fdecls' is necessary because function
   -- rules may not be together in the declaration list
   genLocals :: AbstractEnv -> Map.Map Ident [Decl] -> [Decl] 
	        -> ([CLocalDecl], AbstractEnv)
   genLocals env _ [] = ([], env)
   genLocals env fdecls ((FunctionDecl _ ident _):decls)
      = let (funcdecl, env1) = genLocalFuncDecl (beginScope env) fdecls ident
	    (locals, env2)   = genLocals (endScope env1) fdecls decls
        in  (funcdecl:locals, env2)
   genLocals env fdecls ((ExternalDecl _ _ _ ident _):decls)
      = let (funcdecl, env1) = genLocalFuncDecl (beginScope env) fdecls ident
	    (locals, env2)   = genLocals (endScope env1) fdecls decls
        in  (funcdecl:locals, env2)
   genLocals env fdecls ((FlatExternalDecl pos idents):decls)
      | null idents = genLocals env fdecls decls
      | otherwise 
        = let (funcdecl, env1) 
		= genLocalFuncDecl (beginScope env) fdecls (head idents)
	      (locals, env2) 
		= genLocals (endScope env1)
		            fdecls 
			    (FlatExternalDecl pos (tail idents):decls)
          in  (funcdecl:locals, env2)
   genLocals env fdecls (PatternDecl pos constr rhs : decls)
      = let (patt, env1)    = genLocalPattern pos env constr
	    (plocals, env2) = genLocalDecls (beginScope env1) 
			                    (simplifyRhsLocals rhs)
	    (expr, env3)    = genLocalPattRhs pos env2 (simplifyRhsExpr rhs)
	    (locals, env4)  = genLocals (endScope env3) fdecls decls
	in  (CLocalPat patt expr plocals:locals, env4)
   genLocals env fdecls ((ExtraVariables pos idents):decls)
      | null idents  = genLocals env fdecls decls
      | otherwise
        = let ident  = head idents
	      idx    = fromMaybe 
		         (internalError ("cannot find index"
					 ++ " for free variable \""
					 ++ show ident ++ "\""))
		         (getVarIndex env ident)
	      decls' = ExtraVariables pos (tail idents) : decls
	      (locals, env') = genLocals env fdecls decls'
          in (CLocalVar (idx, name ident) : locals, env')
   genLocals env fdecls ((TypeSig _ _ _):decls)
      = genLocals env fdecls decls
   genLocals _ _ decl = internalError ("unexpected local declaration: \n"
				       ++ show (head decl))

   genLocalFuncDecl :: AbstractEnv -> Map.Map Ident [Decl] -> Ident 
		       -> (CLocalDecl, AbstractEnv)
   genLocalFuncDecl env fdecls ident
      = let fdecl = fromMaybe 
		      (internalError ("missing declaration" 
				      ++ " for local function \""
				      ++ show ident ++ "\""))
		      (Map.lookup ident fdecls)
	    (_, funcdecl) = genFuncDecl True env ident fdecl
        in  (CLocalFunc funcdecl, env)

   genLocalPattern pos env (LiteralPattern lit)
      = case lit of
       String _ cs 
         -> genLocalPattern pos env 
                 (ListPattern [] (map (LiteralPattern . Char noRef) cs))
       _ -> (CPLit (genLiteral lit), env)
   genLocalPattern pos env (VariablePattern ident)
      = let idx = fromMaybe 
		     (internalError ("cannot find index"
				    ++ " for pattern variable \""
				    ++ show ident ++ "\""))
		     (getVarIndex env ident)   
        in  (CPVar (idx, name ident), env)
   genLocalPattern pos env (ConstructorPattern qident args)
      = let (args', env') = mapfoldl (genLocalPattern pos) env args
	in (CPComb (genQName False env qident) args', env')
   genLocalPattern pos env (InfixPattern larg qident rarg)
      = genLocalPattern pos env (ConstructorPattern qident [larg, rarg])
   genLocalPattern pos env (ParenPattern patt)
      = genLocalPattern pos env patt
   genLocalPattern pos env (TuplePattern _ args)
      | len > 1  
        = genLocalPattern pos env (ConstructorPattern (qTupleId len) args)
      | len == 1
	= genLocalPattern pos env (head args)
      | len == 0
	= genLocalPattern pos env (ConstructorPattern qUnitId [])
    where len = length args
   genLocalPattern pos env (ListPattern _ args)
      = genLocalPattern pos env 
	  (foldr (\p1 p2 -> ConstructorPattern qConsId [p1,p2])
	   (ConstructorPattern qNilId [])
	   args)
   genLocalPattern pos _ (NegativePattern _ _)
      = errorAt pos "negative patterns are not supported in AbstractCurry"
   genLocalPattern pos env (AsPattern ident cterm)
      = let (patt, env1) = genLocalPattern pos env cterm
	    idx          = fromMaybe 
			      (internalError ("cannot find index"
					      ++ " for alias variable \""
					      ++ show ident ++ "\""))
			      (getVarIndex env1 ident)
        in  (CPAs (idx, name ident) patt, env1)
   genLocalPattern pos env (LazyPattern _ cterm)
      = let (patt, env') = genLocalPattern pos env cterm
        in  (CPLazy patt, env')
   genLocalPattern pos env (RecordPattern fields mr)
      = let (fields', env1) = mapfoldl (genField genLocalPattern) env fields
	    (mr', env2)
		= maybe (Nothing, env1)
		        (applyFst Just . genLocalPattern pos env1)
			mr
	in  (CPRecord fields' mr', env2)

   genLocalPattRhs pos env [(Variable qSuccessFunId, expr)]
      = genExpr pos env expr
   genLocalPattRhs pos _ _
      = errorAt pos ("guarded expressions in pattern declarations"
		     ++ " are not supported in AbstractCurry")


--
genExpr :: Position -> AbstractEnv -> Expression -> (CExpr, AbstractEnv)
genExpr pos env (Literal lit)
   = case lit of
       String _ cs -> genExpr pos env (List [] (map (Literal . Char noRef) cs))
       _           -> (CLit (genLiteral lit), env)
genExpr _ env (Variable qident)
   | isJust midx          = (CVar (fromJust midx, name ident), env)
   | qident == qSuccessId = (CSymbol (genQName False env qSuccessFunId), env)
   | otherwise            = (CSymbol (genQName False env qident), env)
 where
   ident = unqualify qident
   midx  = getVarIndex env ident
genExpr _ env (Constructor qident)
   = (CSymbol (genQName False env qident), env)
genExpr pos env (Paren expr)
   = genExpr pos env expr
genExpr pos env (Typed expr _)
   = genExpr pos env expr
genExpr pos env (Tuple _ args)
   | len > 1
     = genExpr pos env (foldl Apply (Variable (qTupleId (length args))) args)
   | len == 1
     = genExpr pos env (head args)
   | len == 0
     = genExpr pos env (Variable qUnitId)
 where len = length args
genExpr pos env (List _ args)
   = let cons = Constructor qConsId
	 nil  = Constructor qNilId
     in  genExpr pos env (foldr (Apply . Apply cons) nil args)
genExpr pos env (ListCompr _ expr stmts)
   = let (stmts', env1) = mapfoldl (genStatement pos) (beginScope env) stmts
	 (expr', env2)  = genExpr pos env1 expr
     in  (CListComp expr' stmts', endScope env2)
genExpr pos env (EnumFrom expr)
   = genExpr pos env (Apply (Variable qEnumFromId) expr)
genExpr pos env (EnumFromThen expr1 expr2)
   = genExpr pos env (Apply (Apply (Variable qEnumFromThenId) expr1) expr2)
genExpr pos env (EnumFromTo expr1 expr2)
   = genExpr pos env (Apply (Apply (Variable qEnumFromToId) expr1) expr2)
genExpr pos env (EnumFromThenTo expr1 expr2 expr3)
   = genExpr pos env (Apply (Apply (Apply (Variable qEnumFromThenToId) 
				    expr1) expr2) expr3)
genExpr pos env (UnaryMinus _ expr)
   = genExpr pos env (Apply (Variable qNegateId) expr)
genExpr pos env (Apply expr1 expr2)
   = let (expr1', env1) = genExpr pos env expr1
	 (expr2', env2) = genExpr pos env1 expr2
     in  (CApply expr1' expr2', env2)
genExpr pos env (InfixApply expr1 op expr2)
   = genExpr pos env (Apply (Apply (opToExpr op) expr1) expr2)
genExpr pos env (LeftSection expr op)
   = let ident  = freshVar env "x"
	 patt   = VariablePattern ident
	 var    = Variable (qualify ident)
	 applic = Apply (Apply (opToExpr op) expr) var 
     in  genExpr pos env (Lambda noRef [patt] applic)
genExpr pos env (RightSection op expr)
   = let ident  = freshVar env "x"
	 patt   = VariablePattern ident
	 var    = Variable (qualify ident)
	 applic = Apply (Apply (opToExpr op) var) expr 
     in  genExpr pos env (Lambda noRef [patt] applic)
genExpr pos env (Lambda _ params expr)
   = let (params', env1) = mapfoldl (genPattern pos) (beginScope env) params
	 (expr', env2)   = genExpr pos env1 expr
     in  (CLambda params' expr', endScope env2)
genExpr pos env (Let decls expr)
   = let (decls', env1) = genLocalDecls (beginScope env) decls
	 (expr', env2)  = genExpr pos env1 expr
     in  (CLetDecl decls' expr', endScope env2)
genExpr pos env (Do stmts expr)
   = let (stmts', env1) = mapfoldl (genStatement pos) (beginScope env) stmts
	 (expr', env2)  = genExpr pos env1 expr
     in  (CDoExpr (stmts' ++ [CSExpr expr']), endScope env2)
genExpr pos env (IfThenElse _ expr1 expr2 expr3)
   = genExpr pos env (Apply (Apply (Apply (Variable qIfThenElseId)
				    expr1) expr2) expr3)
genExpr pos env (Case _ expr alts)
   = let (expr', env1) = genExpr pos env expr
	 (alts', env2) = mapfoldl genBranchExpr env1 alts
     in  (CCase expr' alts', env2)
genExpr pos env (RecordConstr fields)
   = let (fields', env1) = mapfoldl (genField genExpr) env fields
     in  (CRecConstr fields', env1)
genExpr pos env (RecordSelection expr label)
   = let (expr', env1) = genExpr pos env expr
     in  (CRecSelect expr' (name label), env1)
genExpr pos env (RecordUpdate fields expr)
   = let (fields', env1) = mapfoldl (genField genExpr) env fields
         (expr', env2)   = genExpr pos env1 expr
     in  (CRecUpdate fields' expr', env2)


--
genStatement :: Position -> AbstractEnv -> Statement 
	        -> (CStatement, AbstractEnv)
genStatement pos env (StmtExpr _ expr)
   = let (expr', env') = genExpr pos env expr
     in  (CSExpr expr', env')
genStatement _ env (StmtDecl decls)
   = let (decls', env') = genLocalDecls env decls
     in  (CSLet decls', env')
genStatement pos env (StmtBind _ patt expr)
   = let (expr', env1) = genExpr pos env expr
	 (patt', env2) = genPattern pos env1 patt
     in  (CSPat patt' expr', env2)


-- NOTE: guarded expressions and local declarations in case branches
-- are not supported in PAKCS
genBranchExpr :: AbstractEnv -> Alt -> (CBranchExpr, AbstractEnv)
genBranchExpr env (Alt pos patt rhs)
   = let (patt', env1) = genPattern pos (beginScope env) patt
	 (expr', env2) = genBranchRhs pos env1 (simplifyRhsExpr rhs)
     in  (CBranch patt' expr', endScope env2)
 where
   genBranchRhs pos env [(Variable qSuccessFunId, expr)]
      = genExpr pos env expr
   genBranchRhs pos _ _
      = errorAt pos ("guarded expressions in case alternatives"
		     ++ " are not supported in AbstractCurry")


--
genPattern :: Position -> AbstractEnv -> ConstrTerm -> (CPattern, AbstractEnv)
genPattern pos env (LiteralPattern lit)
   = case lit of
       String _ cs 
         -> genPattern pos env (ListPattern [] (map (LiteralPattern . Char noRef) cs))
       _ -> (CPLit (genLiteral lit), env)
genPattern _ env (VariablePattern ident)
   = let (idx, env') = genVarIndex env ident
     in  (CPVar (idx, name ident), env')
genPattern pos env (ConstructorPattern qident args)
   = let (args', env') = mapfoldl (genPattern pos) env args
     in  (CPComb (genQName False env qident) args', env')
genPattern pos env (InfixPattern larg qident rarg)
   = genPattern pos env (ConstructorPattern qident [larg, rarg])
genPattern pos env (ParenPattern patt)
   = genPattern pos env patt
genPattern pos env (TuplePattern _ args)
   | len > 1
     = genPattern pos env (ConstructorPattern (qTupleId len) args)
   | len == 1
     = genPattern pos env (head args)
   | len == 0
     = genPattern pos env (ConstructorPattern qUnitId [])
 where len = length args
genPattern pos env (ListPattern _ args)
   = genPattern pos env (foldr (\x1 x2 -> ConstructorPattern qConsId [x1, x2]) 
		         (ConstructorPattern qNilId []) 
		         args)
genPattern pos _ (NegativePattern _ _)
   = errorAt pos "negative patterns are not supported in AbstractCurry"
genPattern pos env (AsPattern ident cterm)
   = let (patt, env1) = genPattern pos env cterm
	 (idx, env2) = genVarIndex env1 ident
     in  (CPAs (idx, name ident) patt, env2)
genPattern pos env (LazyPattern _ cterm)
   = let (patt, env') = genPattern pos env cterm
     in  (CPLazy patt, env')
genPattern pos env (FunctionPattern qident cterms)
   = let (patts, env') = mapfoldl (genPattern pos) env cterms
     in  (CPFuncComb (genQName False env qident) patts, env')
genPattern pos env (InfixFuncPattern cterm1 qident cterm2)
   = genPattern pos env (FunctionPattern qident [cterm1, cterm2])
genPattern pos env (RecordPattern fields mr)
   = let (fields', env1) = mapfoldl (genField genPattern) env fields
         (mr', env2)     = maybe (Nothing, env1)
                                 (applyFst Just . genPattern pos env1)
				 mr
     in  (CPRecord fields' mr', env2)


--
genField :: (Position -> AbstractEnv -> a -> (b, AbstractEnv))
	 -> AbstractEnv -> Field a -> (CField b, AbstractEnv)
genField genTerm env (Field pos label term)
   = let (term',env1) = genTerm pos env term
     in  ((name label, term'), env1)

--
genLiteral :: Literal -> CLiteral
genLiteral (Char _ c)  = CCharc c
genLiteral (Int _ i)   = CIntc i
genLiteral (Float _ f) = CFloatc f
genLiteral _           = internalError "unsupported literal"


-- Notes: 
-- - Some prelude identifiers are not quialified. The first check ensures
--   that they get a correct qualifier.
-- - The test for unqualified identifiers is necessary to qualify
--   them correctly in the untyped AbstractCurry representation.
genQName :: Bool -> AbstractEnv -> QualIdent -> QName
genQName isTypeCons env qident
   | isPreludeSymbol qident
     = genQualName (qualQualify preludeMIdent qident)
   | not (isQualified qident)
     = genQualName (getQualIdent (unqualify qident))
   | otherwise
     = genQualName qident
 where
  genQualName qid
     = let (mmid, id) = (qualidMod qid, qualidId qid)
	   mid = maybe (moduleId env)
		       (\mid' -> fromMaybe mid' (Map.lookup mid' (imports env)))
		       mmid
       in  (moduleName mid, name id)

  getQualIdent id
     | isTypeCons = case (lookupTC id (tconsEnv env)) of
		      --[DataType qid _ _] -> qid
		      --[RenamingType qid _ _] -> qid
		      --[AliasType qid _ _] -> qid
		      [info] -> origName info
		      _ ->  qualifyWith (moduleId env) id
     | otherwise  = case (lookupValue id (typeEnv env)) of
		      --[DataConstructor qid _] -> qid
		      --[NewtypeConstructor qid _] -> qid
		      --[Value qid _] -> qid
		      [info] -> origName info
		      _ -> qualifyWith (moduleId env) id
		      


--
genVisibility :: AbstractEnv -> Ident -> CVisibility
genVisibility env ident
   | isExported env ident = Public
   | otherwise            = Private


--
genEvalAnnot :: EvalAnnotation -> CEvalAnnot
genEvalAnnot EvalRigid  = CRigid
genEvalAnnot EvalChoice = CChoice


-------------------------------------------------------------------------------
-- This part defines an environment containing all necessary information
-- for generating the AbstractCurry representation of a CurrySyntax term.

-- Data type for representing an AbstractCurry generator environment.
--
--    moduleName  - name of the module
--    typeEnv     - table of all known types
--    exports     - table of all exported symbols from the module
--    imports     - table of import aliases
--    varIndex    - index counter for generating variable indices
--    tvarIndex   - index counter for generating type variable indices
--    varScope    - stack of variable tables
--    tvarScope   - stack of type variable tables
--    acyType     - type of AbstractCurry code to be generated
data AbstractEnv = AbstractEnv {moduleId   :: ModuleIdent,
				typeEnv    :: ValueEnv,
				tconsEnv   :: TCEnv,
				exports    :: Set.Set Ident,
				imports    :: Map.Map ModuleIdent ModuleIdent,
				varIndex   :: Int,
				tvarIndex  :: Int,
				varScope   :: [Map.Map Ident Int],
				tvarScope  :: [Map.Map Ident Int],
                                acyType    :: AbstractType
			       } deriving Show

-- Data type representing the type of AbstractCurry code to be generated
-- (typed infered or untyped (i.e. type signated))
data AbstractType = TypedAcy | UntypedAcy deriving (Eq, Show)


-- Initializes the AbstractCurry generator environment.
genAbstractEnv :: AbstractType -> ValueEnv -> TCEnv -> Module -> AbstractEnv
genAbstractEnv absType tyEnv tcEnv (Module mid exps decls)
   = AbstractEnv 
       {moduleId     = mid,
	typeEnv      = tyEnv,
	tconsEnv     = tcEnv,
	exports      = foldl (buildExportTable mid decls) Set.empty exps',
	imports      = foldl buildImportTable Map.empty decls,
	varIndex     = 0,
	tvarIndex    = 0,
	varScope     = [Map.empty],
	tvarScope    = [Map.empty],
        acyType      = absType
       }
 where
   exps' = maybe (buildExports mid decls) (\ (Exporting _ es) -> es) exps


-- Generates a list of exports for all specified top level declarations
buildExports :: ModuleIdent -> [Decl] -> [Export]
buildExports _ [] = []
buildExports mid (DataDecl _ ident _ _:ds) 
   = ExportTypeAll (qualifyWith mid ident) : buildExports mid ds
buildExports mid ((NewtypeDecl _ ident _ _):ds)
   = ExportTypeAll (qualifyWith mid ident) : buildExports mid ds
buildExports mid ((TypeDecl _ ident _ _):ds)
   = Export (qualifyWith mid ident) : buildExports mid ds
buildExports mid ((FunctionDecl _ ident _):ds)
   = Export (qualifyWith mid ident) : buildExports mid ds
buildExports mid (ExternalDecl _ _ _ ident _ : ds)
   = Export (qualifyWith mid ident) : buildExports mid ds
buildExports mid (FlatExternalDecl _ idents : ds)
   = map (Export . qualifyWith mid) idents ++ buildExports mid ds
buildExports mid (_:ds) = buildExports mid ds


-- Builds a table containing all exported (i.e. public) identifiers
-- from a module.
buildExportTable :: ModuleIdent -> [Decl] -> Set.Set Ident -> Export 
                 -> Set.Set Ident
buildExportTable mid _ exptab (Export qident)
   | isJust (localIdent mid qident)
     = insertExportedIdent exptab (unqualify qident)
   | otherwise = exptab
buildExportTable mid _ exptab (ExportTypeWith qident ids)
   | isJust (localIdent mid qident)
     = foldl insertExportedIdent 
             (insertExportedIdent exptab (unqualify qident))
             ids
   | otherwise  = exptab
buildExportTable mid decls exptab (ExportTypeAll qident)
   | isJust ident'
     = foldl insertExportedIdent
             (insertExportedIdent exptab ident)
             (maybe [] getConstrIdents (find (isDataDeclOf ident) decls))
   | otherwise = exptab
 where 
   ident' = localIdent mid qident
   ident  = fromJust ident'
buildExportTable _ _ exptab (ExportModule _) = exptab

--
insertExportedIdent :: Set.Set Ident -> Ident -> Set.Set Ident
insertExportedIdent env ident = Set.insert ident env

--
getConstrIdents :: Decl -> [Ident]
getConstrIdents (DataDecl _ _ _ constrs)
   = map getConstrIdent constrs
 where
   getConstrIdent (ConstrDecl _ _ ident _)  = ident
   getConstrIdent (ConOpDecl _ _ _ ident _) = ident


-- Builds a table for dereferencing import aliases
buildImportTable :: Map.Map ModuleIdent ModuleIdent -> Decl
		    -> Map.Map ModuleIdent ModuleIdent
buildImportTable env (ImportDecl _ mid _ malias _)
   = Map.insert (fromMaybe mid malias) mid env
buildImportTable env _ = env


-- Checks whether an identifier is exported or not.
isExported :: AbstractEnv -> Ident -> Bool
isExported env ident = Set.member ident (exports env)


-- Generates an unique index for the  variable 'ident' and inserts it
-- into the  variable table of the current scope.
genVarIndex :: AbstractEnv -> Ident -> (Int, AbstractEnv)
genVarIndex env ident 
   = let idx   = varIndex env
         vtabs = varScope env
	 vtab  = head vtabs --if null vtabs then Map.empty else head vtabs
     in  (idx, env {varIndex = idx + 1,
		    varScope = Map.insert ident idx vtab : sureTail vtabs})

-- Generates an unique index for the type variable 'ident' and inserts it
-- into the type variable table of the current scope.
genTVarIndex :: AbstractEnv -> Ident -> (Int, AbstractEnv)
genTVarIndex env ident
   = let idx   = tvarIndex env
         vtabs = tvarScope env
	 vtab  = head vtabs --if null vtabs then Map.empty else head vtabs
     in  (idx, env {tvarIndex = idx + 1,
		    tvarScope = Map.insert ident idx vtab : sureTail vtabs })


-- Looks up the unique index for the variable 'ident' in the
-- variable table of the current scope.
getVarIndex :: AbstractEnv -> Ident -> Maybe Int
getVarIndex env ident = Map.lookup ident (head (varScope env))

-- Looks up the unique index for the type variable 'ident' in the type
-- variable table of the current scope.
getTVarIndex :: AbstractEnv -> Ident -> Maybe Int
getTVarIndex env ident = Map.lookup ident (head (tvarScope env))


-- Generates an indentifier which doesn't occur in the variable table
-- of the current scope.
freshVar :: AbstractEnv -> String -> Ident
freshVar env name = genFreshVar env name 0
 where
   genFreshVar env name idx
      | isJust (getVarIndex env ident)
         = genFreshVar env name (idx + 1)
      | otherwise 
         = ident
    where ident = mkIdent (name ++ show idx)

-- Sets the index counter back to zero and deletes all stack entries.
resetScope :: AbstractEnv -> AbstractEnv
resetScope env = env {varIndex  = 0,
		      tvarIndex = 0,
		      varScope  = [Map.empty],
		      tvarScope = [Map.empty]}

-- Starts a new scope, i.e. copies and pushes the variable table of the current 
-- scope onto the top of the stack
beginScope :: AbstractEnv -> AbstractEnv
beginScope env = env {varScope  = head vs :vs,
		      tvarScope = head tvs :tvs }
 where
 vs  = varScope env
 tvs = tvarScope env

-- End the current scope, i.e. pops and deletes the variable table of the
-- current scope from the top of the stack.
endScope :: AbstractEnv -> AbstractEnv
endScope env = env {varScope  = if oneElement vs then vs else tail vs,
		    tvarScope = if oneElement tvs then tvs else tail tvs}
 where
 vs  = varScope env
 tvs = tvarScope env


-------------------------------------------------------------------------------
-- Miscellaneous...

-- Some identifiers...
qEnumFromId       = qualifyWith preludeMIdent (mkIdent "enumFrom")
qEnumFromThenId   = qualifyWith preludeMIdent (mkIdent "enumFromThen")
qEnumFromToId     = qualifyWith preludeMIdent (mkIdent "enumFromTo")
qEnumFromThenToId = qualifyWith preludeMIdent (mkIdent "enumFromThenTo")
qNegateId         = qualifyWith preludeMIdent (mkIdent "negate")
qIfThenElseId     = qualifyWith preludeMIdent (mkIdent "if_then_else")
qSuccessFunId     = qualifyWith preludeMIdent (mkIdent "success")


-- The following functions check whether a declaration is of a certain kind
isFunctionDecl :: Decl -> Bool
isFunctionDecl (FunctionDecl _ _ _) = True
isFunctionDecl _                    = False

isExternal :: Decl -> Bool
isExternal (ExternalDecl _ _ _ _ _) = True
isExternal (FlatExternalDecl _ _)   = True
isExternal _                        = False


-- Checks, whether a declaration is the data declaration of 'ident'.
isDataDeclOf :: Ident -> Decl -> Bool
isDataDeclOf ident (DataDecl _ ident' _ _) 
   = ident == ident'
isDataDeclOf _ _  
   = False


-- Checks, whether a symbol is defined in the Prelude.
isPreludeSymbol :: QualIdent -> Bool
isPreludeSymbol qident
   = let (mmid, ident) = (qualidMod qident, qualidId qident)
     in  (isJust mmid && preludeMIdent == fromJust mmid)
         || elem ident [unitId, listId, nilId, consId]
	 || isTupleId ident


-- Converts an infix operator to an expression
opToExpr :: InfixOp -> Expression
opToExpr (InfixOp qident)     = Variable qident
opToExpr (InfixConstr qident) = Constructor qident


-- Looks up the type of a qualified symbol in the type environment and
-- converts it to a CurrySyntax type term.
qualLookupType :: QualIdent -> ValueEnv -> Maybe TypeExpr
qualLookupType qident tyEnv
   = case (qualLookupValue qident tyEnv) of
       [Value _ ts] -> (\ (ForAll _ ty) -> Just (fromType ty)) ts
       _            -> Nothing

-- Looks up the type of a symbol in the type environment and
-- converts it to a CurrySyntax type term.
lookupType :: Ident -> ValueEnv -> Maybe TypeExpr
lookupType ident tyEnv
   = case (lookupValue ident tyEnv) of
       [Value _ ts] -> (\ (ForAll _ ty) -> Just (fromType ty)) ts
       _            -> Nothing


-- The following functions transform left-hand-side and right-hand-side terms
-- for a better handling
simplifyLhs :: Lhs -> [ConstrTerm]
simplifyLhs = snd . flatLhs

simplifyRhsExpr :: Rhs -> [(Expression, Expression)]
simplifyRhsExpr (SimpleRhs _ expr _) 
   = [(Variable qSuccessId, expr)]
simplifyRhsExpr (GuardedRhs crhs _)  
   = map (\ (CondExpr _ cond expr) -> (cond, expr)) crhs

simplifyRhsLocals :: Rhs -> [Decl]
simplifyRhsLocals (SimpleRhs _ _ locals) = locals
simplifyRhsLocals (GuardedRhs _ locals)  = locals


-- FIXME This mapfold is a twisted mapAccumL
-- A combination of 'map' and 'foldl'. It maps a function to a list
-- from left to right while updating the argument 'e' continously.
mapfoldl :: (a -> b -> (c,a)) -> a -> [b] -> ([c], a)
mapfoldl _ e []     = ([], e)
mapfoldl f e (x:xs) = let (x', e')   = f e x
                          (xs', e'') = mapfoldl f e' xs
                      in  (x':xs', e'')

-- Inserts an element under a key into an association list
insertEntry :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
insertEntry k e [] = [(k,e)]
insertEntry k e ((x,y):xys)
   | k == x    = (k,e):xys
   | otherwise = (x,y) : insertEntry k e xys


-- Returns the list without the first element. If the list is empty, an
-- empty list will be returned.
sureTail :: [a] -> [a]
sureTail []     = []
sureTail (_:xs) = xs


-- Returns 'True', if a list contains exactly one element
oneElement :: [a] -> Bool
oneElement [_] = True
oneElement _   = False


-- Applies 'f' on the first value in a tuple
applyFst :: (a -> c) -> (a,b) -> (c,b)
applyFst f (x,y) = (f x, y)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
