-------------------------------------------------------------------------------
--
-- WarnCheck - Searches for potentially irregular code and generates
--             warning messages
--                
-- February 2006,
-- Martin Engelke (men@informatik.uni-kiel.de)
--
module WarnCheck (warnCheck) where

import Control.Monad.State
import qualified Data.Map as Map
import Data.List

import Curry.Base.Ident
import Curry.Base.Position
import Curry.Base.MessageMonad
import Curry.Syntax

import Base (ValueEnv, ValueInfo(..), qualLookupValue)
import TopEnv
import qualified ScopeEnv
import ScopeEnv (ScopeEnv)


-------------------------------------------------------------------------------

-- Data type for representing the current state of generating warnings.
-- The monadic representation of the state allows the usage of monadic 
-- syntax (do expression) for dealing easier and safer with its
-- contents.

type CheckState = State CState

data CState = CState {messages  :: [WarnMsg],
		      scope     :: ScopeEnv QualIdent IdInfo,
		      values    :: ValueEnv,
		      moduleId  :: ModuleIdent }

-- Runs a 'CheckState' action and returns the list of messages
run ::  CheckState a -> [WarnMsg]
run f
   = reverse (messages (execState f emptyState))

emptyState :: CState
emptyState = CState {messages  = [],
		     scope     = ScopeEnv.new,
		     values    = emptyTopEnv,
		     moduleId  = mkMIdent []
		    }

-------------------------------------------------------------------------------

-- Find potentially incorrect code in a Curry program and generate
-- the following warnings for:
--    - unreferenced variables
--    - shadowing variables
--    - idle case alternatives
--    - overlapping case alternatives
--    - function rules which are not together
warnCheck :: ModuleIdent -> ValueEnv -> [Decl] -> [Decl] -> [WarnMsg]
warnCheck mid vals imports decls
   = run (do addImportedValues vals
	     addModuleId mid
	     checkImports imports
	     foldM' insertDecl decls
	     foldM' (checkDecl mid) decls
             checkDeclOccurrences decls
	 )


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

--
checkDecl :: ModuleIdent -> Decl -> CheckState ()
checkDecl mid (DataDecl pos ident params cdecls)
   = do beginScope
	foldM' insertTypeVar params
	foldM' (checkConstrDecl mid) cdecls
	params' <- filterM isUnrefTypeVar params
	when (not (null params')) 
	     (foldM' genWarning' (map unrefTypeVar params'))
	endScope
checkDecl mid (TypeDecl _ ident params texpr)
   = do beginScope
	foldM' insertTypeVar params
	checkTypeExpr mid texpr
	params' <- filterM isUnrefTypeVar params
	when (not (null params'))
	     (foldM' genWarning'  (map unrefTypeVar params'))
	endScope
checkDecl mid (FunctionDecl pos ident equs)
   = do beginScope
	foldM' (checkEquation mid) equs
	c <- isConsId ident
	idents' <- returnUnrefVars
	when (not (c || null idents')) 
             (foldM' genWarning' (map unrefVar idents'))
	endScope
checkDecl mid (PatternDecl _ cterm rhs)
   = do checkConstrTerm mid cterm
	checkRhs mid rhs
checkDecl _ _ = return ()

-- Checks locally declared identifiers (i.e. functions and logic variables)
-- for shadowing
checkLocalDecl :: Decl -> CheckState ()
checkLocalDecl (FunctionDecl pos ident _)
   = do s <- isShadowingVar ident
	when s (genWarning' (shadowingVar ident))
checkLocalDecl (ExtraVariables pos idents)
   = do idents' <- filterM isShadowingVar idents
	when (not (null idents'))
	     (foldM' genWarning' (map shadowingVar idents'))
checkLocalDecl (PatternDecl _ constrTerm _)
   = checkConstrTerm (mkMIdent []) constrTerm
checkLocalDecl _ = return ()

--
checkConstrDecl :: ModuleIdent -> ConstrDecl -> CheckState ()
checkConstrDecl mid (ConstrDecl _ _ ident texprs)
   = do visitId ident
	foldM' (checkTypeExpr mid) texprs
checkConstrDecl mid (ConOpDecl _ _ texpr1 ident texpr2)
   = do visitId ident
	checkTypeExpr mid texpr1
	checkTypeExpr mid texpr2


checkTypeExpr :: ModuleIdent -> TypeExpr -> CheckState ()
checkTypeExpr mid (ConstructorType qid texprs)
   = do maybe (return ()) visitTypeId (localIdent mid qid)
	foldM' (checkTypeExpr mid ) texprs
checkTypeExpr mid  (VariableType ident)
   = visitTypeId ident
checkTypeExpr mid  (TupleType texprs)
   = foldM' (checkTypeExpr mid ) texprs
checkTypeExpr mid  (ListType texpr)
   = checkTypeExpr mid  texpr
checkTypeExpr mid  (ArrowType texpr1 texpr2)
   = do checkTypeExpr mid  texpr1
	checkTypeExpr mid  texpr2
checkTypeExpr mid  (RecordType fields restr)
   = do foldM' (checkTypeExpr mid ) (map snd fields)
	maybe (return ()) (checkTypeExpr mid ) restr

--
checkEquation :: ModuleIdent -> Equation -> CheckState ()
checkEquation mid (Equation _ lhs rhs)
   = do checkLhs mid lhs
	checkRhs mid rhs

--
checkLhs :: ModuleIdent -> Lhs -> CheckState ()
checkLhs mid (FunLhs ident cterms)
   = do visitId ident
	foldM' (checkConstrTerm mid) cterms
	foldM' (insertConstrTerm False) cterms
checkLhs mid (OpLhs cterm1 ident cterm2)
   = checkLhs mid (FunLhs ident [cterm1, cterm2])
checkLhs mid (ApLhs lhs cterms)
   = do checkLhs mid lhs
	foldM' (checkConstrTerm mid ) cterms
	foldM' (insertConstrTerm False) cterms

--
checkRhs :: ModuleIdent -> Rhs -> CheckState ()
checkRhs mid (SimpleRhs _ expr decls)
   = do beginScope  -- function arguments can be overwritten by local decls
	foldM' checkLocalDecl decls
	foldM' insertDecl decls
	foldM' (checkDecl mid) decls
	checkDeclOccurrences decls
	checkExpression mid expr
	idents' <- returnUnrefVars
	when (not (null idents'))
	     (foldM' genWarning' (map unrefVar idents'))
	endScope
checkRhs mid (GuardedRhs cexprs decls)
   = do beginScope
	foldM' checkLocalDecl decls
	foldM' insertDecl decls
	foldM' (checkDecl mid) decls
	checkDeclOccurrences decls
	foldM' (checkCondExpr mid) cexprs
	idents' <- returnUnrefVars
	when (not (null idents'))
	     (foldM' genWarning' (map unrefVar idents'))
	endScope


--
checkCondExpr :: ModuleIdent -> CondExpr -> CheckState ()
checkCondExpr mid (CondExpr _ cond expr)
   = do checkExpression mid cond
	checkExpression mid expr

-- 
checkConstrTerm :: ModuleIdent -> ConstrTerm -> CheckState ()
checkConstrTerm mid (VariablePattern ident)
   = do s <- isShadowingVar ident
	when s (genWarning' (shadowingVar ident))
checkConstrTerm mid (ConstructorPattern _ cterms)
   = foldM' (checkConstrTerm mid ) cterms
checkConstrTerm mid (InfixPattern cterm1 qident cterm2)
   = checkConstrTerm mid (ConstructorPattern qident [cterm1, cterm2])
checkConstrTerm mid (ParenPattern cterm)
   = checkConstrTerm mid cterm
checkConstrTerm mid (TuplePattern _ cterms)
   = foldM' (checkConstrTerm mid ) cterms
checkConstrTerm mid (ListPattern _ cterms)
   = foldM' (checkConstrTerm mid ) cterms
checkConstrTerm mid (AsPattern ident cterm)
   = do s <- isShadowingVar ident
	when s (genWarning' (shadowingVar ident))
	checkConstrTerm mid cterm
checkConstrTerm mid (LazyPattern _ cterm)
   = checkConstrTerm mid cterm
checkConstrTerm mid (FunctionPattern _ cterms)
   = foldM' (checkConstrTerm mid ) cterms
checkConstrTerm mid  (InfixFuncPattern cterm1 qident cterm2)
   = checkConstrTerm mid  (FunctionPattern qident [cterm1, cterm2])
checkConstrTerm mid  (RecordPattern fields restr)
   = do foldM' (checkFieldPattern mid) fields
	maybe (return ()) (checkConstrTerm mid ) restr
checkConstrTerm _ _ = return ()

--
checkExpression :: ModuleIdent -> Expression -> CheckState ()
checkExpression mid (Variable qident)
   = maybe (return ()) visitId (localIdent mid qident)
checkExpression mid (Paren expr)
   = checkExpression mid expr
checkExpression mid (Typed expr _)
   = checkExpression mid expr
checkExpression mid (Tuple _ exprs)
   = foldM' (checkExpression mid ) exprs
checkExpression mid (List _ exprs)
   = foldM' (checkExpression mid ) exprs
checkExpression mid (ListCompr _ expr stmts)
   = do beginScope
	foldM' (checkStatement mid ) stmts
	checkExpression mid expr
	idents' <- returnUnrefVars
	when (not (null idents'))
	     (foldM' genWarning' (map unrefVar idents'))
	endScope
checkExpression mid  (EnumFrom expr)
   = checkExpression mid  expr
checkExpression mid  (EnumFromThen expr1 expr2)
   = foldM' (checkExpression mid ) [expr1, expr2]
checkExpression mid  (EnumFromTo expr1 expr2)
   = foldM' (checkExpression mid ) [expr1, expr2]
checkExpression mid  (EnumFromThenTo expr1 expr2 expr3)
   = foldM' (checkExpression mid ) [expr1, expr2, expr3]
checkExpression mid  (UnaryMinus _ expr)
   = checkExpression mid  expr
checkExpression mid  (Apply expr1 expr2)
   = foldM' (checkExpression mid ) [expr1, expr2]
checkExpression mid  (InfixApply expr1 op expr2)
   = do maybe (return ()) (visitId) (localIdent mid (opName op))
	foldM' (checkExpression mid ) [expr1, expr2]
checkExpression mid  (LeftSection expr _)
   = checkExpression mid  expr
checkExpression mid  (RightSection _ expr)
   = checkExpression mid  expr
checkExpression mid  (Lambda _ cterms expr)
   = do beginScope
	foldM' (checkConstrTerm mid ) cterms
	foldM' (insertConstrTerm False) cterms
	checkExpression mid expr
	idents' <- returnUnrefVars
	when (not (null idents'))
	     (foldM' genWarning' (map unrefVar idents'))
	endScope
checkExpression mid  (Let decls expr)
   = do beginScope
	foldM' checkLocalDecl decls
	foldM' insertDecl decls
	foldM' (checkDecl mid) decls
	checkDeclOccurrences decls
	checkExpression mid  expr
	idents' <- returnUnrefVars
	when (not (null idents'))
	     (foldM' genWarning' (map unrefVar idents'))
	endScope
checkExpression mid  (Do stmts expr)
   = do beginScope
	foldM' (checkStatement mid ) stmts
	checkExpression mid  expr
	idents' <- returnUnrefVars
	when (not (null idents'))
	     (foldM' genWarning' (map unrefVar idents'))
	endScope
checkExpression mid  (IfThenElse _ expr1 expr2 expr3)
   = foldM' (checkExpression mid ) [expr1, expr2, expr3]
checkExpression mid  (Case _ expr alts)
   = do checkExpression mid  expr
	foldM' (checkAlt mid) alts
	checkCaseAlternatives mid alts
checkExpression mid (RecordConstr fields)
   = foldM' (checkFieldExpression mid) fields
checkExpression mid (RecordSelection expr ident)
   = checkExpression mid expr -- Hier auch "visitId ident" ?
checkExpression mid (RecordUpdate fields expr)
   = do foldM' (checkFieldExpression mid) fields
	checkExpression mid expr
checkExpression _ _  = return ()

--
checkStatement :: ModuleIdent -> Statement -> CheckState ()
checkStatement mid (StmtExpr _ expr)
   = checkExpression mid expr
checkStatement mid (StmtDecl decls)
   = do foldM' checkLocalDecl decls
	foldM' insertDecl decls
	foldM' (checkDecl mid) decls
	checkDeclOccurrences decls
checkStatement mid (StmtBind _ cterm expr)
   = do checkConstrTerm mid cterm
	insertConstrTerm False cterm
	checkExpression mid expr

--
checkAlt :: ModuleIdent -> Alt -> CheckState ()
checkAlt mid (Alt pos cterm rhs)
   = do beginScope 
	checkConstrTerm mid  cterm
	insertConstrTerm False cterm
	checkRhs mid rhs
	idents' <-  returnUnrefVars
	when (not (null idents'))
	     (foldM' genWarning' (map unrefVar idents'))
	endScope

--
checkFieldExpression :: ModuleIdent -> Field Expression -> CheckState ()
checkFieldExpression mid (Field _ ident expr)
   = checkExpression mid expr -- Hier auch "visitId ident" ?

--
checkFieldPattern :: ModuleIdent -> Field ConstrTerm -> CheckState ()
checkFieldPattern mid (Field _ ident cterm)
   = checkConstrTerm mid  cterm

-- Check for idle and overlapping case alternatives
checkCaseAlternatives :: ModuleIdent -> [Alt] -> CheckState ()
checkCaseAlternatives mid alts
   = do checkIdleAlts mid alts
	checkOverlappingAlts mid alts

--
-- FIXME this looks buggy: is alts' required to be non-null or not? (hsi)
checkIdleAlts :: ModuleIdent -> [Alt] -> CheckState ()
checkIdleAlts mid alts
   = do alts' <- dropUnless' isVarAlt alts
	let idles = tail_ [] alts'
	    (Alt pos _ _) = head idles
	unless (null idles) (genWarning pos idleCaseAlts)
 where
 isVarAlt (Alt _ (VariablePattern id) _) 
    = isVarId id
 isVarAlt (Alt _ (ParenPattern (VariablePattern id)) _) 
    = isVarId id
 isVarAlt (Alt _ (AsPattern _ (VariablePattern id)) _)
    = isVarId id
 isVarAlt _ = return False

--
checkOverlappingAlts :: ModuleIdent -> [Alt] -> CheckState ()
checkOverlappingAlts mid [] = return ()
checkOverlappingAlts mid (alt:alts)
   = do (altsr, alts') <- partition' (equalAlts alt) alts
        mapM_ (\ (Alt pos _ _) -> genWarning pos overlappingCaseAlt) altsr
	checkOverlappingAlts mid alts'
 where
 equalAlts (Alt _ cterm1 _) (Alt _ cterm2 _) = equalConstrTerms cterm1 cterm2

 equalConstrTerms (LiteralPattern l1) (LiteralPattern l2)
    = return (l1 == l2)
 equalConstrTerms (NegativePattern id1 l1) (NegativePattern id2 l2) 
    = return (id1 == id2 && l1 == l2)
 equalConstrTerms (VariablePattern id1) (VariablePattern id2)
    = do p <- isConsId id1 
	 return (p && id1 == id2)
 equalConstrTerms (ConstructorPattern qid1 cs1)
		  (ConstructorPattern qid2 cs2)
    = if qid1 == qid2
      then all' (\ (c1,c2) -> equalConstrTerms c1 c2) (zip cs1 cs2)
      else return False
 equalConstrTerms (InfixPattern lcs1 qid1 rcs1)
		  (InfixPattern lcs2 qid2 rcs2)
    = equalConstrTerms (ConstructorPattern qid1 [lcs1, rcs1])
                       (ConstructorPattern qid2 [lcs2, rcs2])
 equalConstrTerms (ParenPattern cterm1) (ParenPattern cterm2)
    = equalConstrTerms cterm1 cterm2
 equalConstrTerms (TuplePattern _ cs1) (TuplePattern _ cs2)
    = equalConstrTerms (ConstructorPattern (qTupleId 2) cs1)
                       (ConstructorPattern (qTupleId 2) cs2)
 equalConstrTerms (ListPattern _ cs1) (ListPattern _ cs2)
    = cmpListM equalConstrTerms cs1 cs2
 equalConstrTerms (AsPattern id1 cterm1) (AsPattern id2 cterm2)
    = equalConstrTerms cterm1 cterm2
 equalConstrTerms (LazyPattern _ cterm1) (LazyPattern _ cterm2)
    = equalConstrTerms cterm1 cterm2
 equalConstrTerms _ _ = return False


-- Find function rules which are not together
checkDeclOccurrences :: [Decl] -> CheckState ()
checkDeclOccurrences decls = checkDO (mkIdent "") Map.empty decls
 where
 checkDO prevId env [] = return ()
 checkDO prevId env ((FunctionDecl pos ident _):decls)
    = do c <- isConsId ident
	 if not (c || prevId == ident)
          then (maybe (checkDO ident (Map.insert ident pos env) decls)
	              (\pos' -> genWarning' (rulesNotTogether ident pos')
		                >> checkDO ident env decls)
	              (Map.lookup ident env))
	  else checkDO ident env decls
 checkDO _ env (_:decls) 
    = checkDO (mkIdent "") env decls


-- check import declarations for multiply imported modules
checkImports :: [Decl] -> CheckState ()
checkImports imps = checkImps Map.empty imps
 where
 checkImps env [] = return ()
 checkImps env ((ImportDecl pos mid _ _ spec):imps)
    | mid /= preludeMIdent
      = maybe (checkImps (Map.insert mid (fromImpSpec spec) env) imps)
              (\ishs -> checkImpSpec env pos mid ishs spec
	                >>= (\env' -> checkImps env' imps))
	      (Map.lookup mid env)
    | otherwise
      = checkImps env imps
 checkImps env (_:imps) = checkImps env imps

 checkImpSpec env pos mid (is,hs) Nothing
    = genWarning' (multiplyImportedModule mid) >> return env
 checkImpSpec env pos mid (is,hs) (Just (Importing _ is'))
    | null is && any (\i' -> notElem i' hs) is'
      = do genWarning' (multiplyImportedModule mid)
	   return (Map.insert mid (is',hs) env)
    | null iis
      = return (Map.insert mid (is' ++ is,hs) env)
    | otherwise
      = do foldM' genWarning'
		  (map ((multiplyImportedSymbol mid) . impName) iis)
	   return (Map.insert mid (unionBy cmpImport is' is,hs) env)
  where iis = intersectBy cmpImport is' is
 checkImpSpec env pos mid (is,hs) (Just (Hiding _ hs'))
    | null ihs
      = return (Map.insert mid (is,hs' ++ hs) env)
    | otherwise
      = do foldM' genWarning' 
		  (map ((multiplyHiddenSymbol mid) . impName) ihs)
	   return (Map.insert mid (is,unionBy cmpImport hs' hs) env)
  where ihs = intersectBy cmpImport hs' hs

 cmpImport (ImportTypeWith id1 cs1) (ImportTypeWith id2 cs2)
    = id1 == id2 && null (intersect cs1 cs2)
 cmpImport i1 i2 = (impName i1) == (impName i2)

 impName (Import id)           = id
 impName (ImportTypeAll id)    = id
 impName (ImportTypeWith id _) = id

 fromImpSpec Nothing                 = ([],[])
 fromImpSpec (Just (Importing _ is)) = (is,[])
 fromImpSpec (Just (Hiding _ hs))    = ([],hs)


-------------------------------------------------------------------------------
-- For detecting unreferenced variables, the following functions updates the 
-- current check state by adding identifiers occuring in declaration left hand 
-- sides.

--
insertDecl :: Decl -> CheckState ()
insertDecl (DataDecl _ ident _ cdecls)
   = do insertTypeConsId ident
	foldM' insertConstrDecl cdecls
insertDecl (TypeDecl _ ident _ texpr)
   = do insertTypeConsId ident
	insertTypeExpr texpr
insertDecl (FunctionDecl _ ident _)
   = do c <- isConsId ident
	unless c (insertVar ident)
insertDecl (ExternalDecl _ _ _ ident _)
   = insertVar ident
insertDecl (FlatExternalDecl _ idents)
   = foldM' insertVar idents
insertDecl (PatternDecl _ cterm _)
   = insertConstrTerm False cterm
insertDecl (ExtraVariables _ idents)
   = foldM' insertVar idents
insertDecl _ = return ()

--
insertTypeExpr :: TypeExpr -> CheckState ()
insertTypeExpr (VariableType _) = return ()
insertTypeExpr (ConstructorType _ texprs)
   = foldM' insertTypeExpr texprs
insertTypeExpr (TupleType texprs)
   = foldM' insertTypeExpr texprs
insertTypeExpr (ListType texpr)
   = insertTypeExpr texpr
insertTypeExpr (ArrowType texpr1 texpr2)
   = foldM' insertTypeExpr [texpr1,texpr2]
insertTypeExpr (RecordType fields restr)
   = do --foldM' insertVar (concatMap fst fields)
	maybe (return ()) insertTypeExpr restr

--
insertConstrDecl :: ConstrDecl -> CheckState ()
insertConstrDecl (ConstrDecl _ _ ident _)
   = insertConsId ident
insertConstrDecl (ConOpDecl _ _ _ ident _)
   = insertConsId ident

-- Notes: 
--    - 'fp' indicates whether 'checkConstrTerm' deals with the arguments
--      of a function pattern or not.
--    - Since function patterns are not recognized before syntax check, it is
--      necessary to determine, whether a constructor pattern represents a
--      constructor or a function. 
insertConstrTerm :: Bool -> ConstrTerm -> CheckState ()
insertConstrTerm fp (VariablePattern ident)
   | fp        = do c <- isConsId ident
		    v <- isVarId ident
		    unless c (if (name ident) /= "_" && v
			         then visitId ident
			         else insertVar ident)
   | otherwise = do c <- isConsId ident
	            unless c (insertVar ident)
insertConstrTerm fp (ConstructorPattern qident cterms)
   = do c <- isQualConsId qident
	if c then foldM' (insertConstrTerm fp) cterms
	     else foldM' (insertConstrTerm True) cterms
insertConstrTerm fp (InfixPattern cterm1 qident cterm2)
   = insertConstrTerm fp (ConstructorPattern qident [cterm1, cterm2])
insertConstrTerm fp (ParenPattern cterm)
   = insertConstrTerm fp cterm
insertConstrTerm fp (TuplePattern _ cterms)
   = foldM' (insertConstrTerm fp) cterms
insertConstrTerm fp (ListPattern _ cterms)
   = foldM' (insertConstrTerm fp) cterms
insertConstrTerm fp (AsPattern ident cterm)
   = do insertVar ident
	insertConstrTerm fp cterm
insertConstrTerm fp (LazyPattern _ cterm)
   = insertConstrTerm fp cterm
insertConstrTerm _ (FunctionPattern _ cterms)
   = foldM' (insertConstrTerm True) cterms
insertConstrTerm _ (InfixFuncPattern cterm1 qident cterm2)
   = insertConstrTerm True (FunctionPattern qident [cterm1, cterm2])
insertConstrTerm fp (RecordPattern fields restr)
   = do foldM' (insertFieldPattern fp) fields
	maybe (return ()) (insertConstrTerm fp) restr
insertConstrTerm _ _ = return ()

--
insertFieldPattern :: Bool -> Field ConstrTerm -> CheckState ()
insertFieldPattern fp (Field _ _ cterm)
   = insertConstrTerm fp cterm

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

-- Data type for distinguishing identifiers as either (type) constructors or
-- (type) variables (including functions).
-- The Boolean flag in 'VarInfo' is used to mark variables when they are used 
-- within expressions.
data IdInfo = ConsInfo | VarInfo Bool deriving Show

--
isVariable :: IdInfo -> Bool
isVariable (VarInfo _) = True
isVariable _           = False

--
isConstructor :: IdInfo -> Bool
isConstructor ConsInfo = True
isConstructor _        = False

--
variableVisited :: IdInfo -> Bool
variableVisited (VarInfo v) = v
variableVisited _           = True

--
visitVariable :: IdInfo -> IdInfo
visitVariable info = case info of
		       VarInfo _ -> VarInfo True
		       _         -> info


--
modifyScope :: (ScopeEnv QualIdent IdInfo -> ScopeEnv QualIdent IdInfo)
	       -> CState -> CState
modifyScope f state = state{ scope = f (scope state) }


--
genWarning :: Position -> String -> CheckState ()
genWarning pos msg
   = modify (\state -> state{ messages = warnMsg:(messages state) })
 where warnMsg = WarnMsg (Just pos) msg
 
genWarning' :: (Position, String) -> CheckState ()
genWarning' (pos, msg)
   = modify (\state -> state{ messages = warnMsg:(messages state) })
    where warnMsg = WarnMsg (Just pos) msg 

--
insertVar :: Ident -> CheckState ()
insertVar id 
   | isAnnonId id = return ()
   | otherwise
     = modify 
         (\state -> modifyScope 
	              (ScopeEnv.insert (commonId id) (VarInfo False)) state)

--
insertTypeVar :: Ident -> CheckState ()
insertTypeVar id
   | isAnnonId id = return ()
   | otherwise    
     = modify 
         (\state -> modifyScope 
	              (ScopeEnv.insert (typeId id) (VarInfo False)) state)

--
insertConsId :: Ident -> CheckState ()
insertConsId id
   = modify 
       (\state -> modifyScope (ScopeEnv.insert (commonId id) ConsInfo) state)

--
insertTypeConsId :: Ident -> CheckState ()
insertTypeConsId id
   = modify 
       (\state -> modifyScope (ScopeEnv.insert (typeId id) ConsInfo) state)

--
isVarId :: Ident -> CheckState Bool
isVarId id
   = gets (\state -> isVar state (commonId id))

--
isConsId :: Ident -> CheckState Bool
isConsId id 
   = gets (\state -> isCons state (qualify id))

--
isQualConsId :: QualIdent -> CheckState Bool
isQualConsId qid
   = gets (\state -> isCons state qid)

--
isShadowingVar :: Ident -> CheckState Bool
isShadowingVar id 
   = gets (\state -> isShadowing state (commonId id))

--
visitId :: Ident -> CheckState ()
visitId id 
   = modify 
       (\state -> modifyScope 
	            (ScopeEnv.modify visitVariable (commonId id)) state)

--
visitTypeId :: Ident -> CheckState ()
visitTypeId id 
   = modify 
       (\state -> modifyScope 
	            (ScopeEnv.modify visitVariable (typeId id)) state)

--
isUnrefTypeVar :: Ident -> CheckState Bool
isUnrefTypeVar id
   = gets (\state -> isUnref state (typeId id))

--
returnUnrefVars :: CheckState [Ident]
returnUnrefVars 
   = gets (\state -> 
	   	    let ids    = map fst (ScopeEnv.toLevelList (scope state))
                        unrefs = filter (isUnref state) ids
	            in  map unqualify unrefs )

--
addModuleId :: ModuleIdent -> CheckState ()
addModuleId mid = modify (\state -> state{ moduleId = mid })

--

withScope :: CheckState a -> CheckState ()
withScope m = beginScope >> m >> endScope

--
beginScope :: CheckState ()
beginScope = modify (\state -> modifyScope ScopeEnv.beginScope state)

--
endScope :: CheckState ()
endScope = modify (\state -> modifyScope ScopeEnv.endScopeUp state)


-- Adds the content of a value environment to the state
addImportedValues :: ValueEnv -> CheckState ()
addImportedValues vals = modify (\state -> state{ values = vals })

--
foldM' :: (a -> CheckState ()) -> [a] -> CheckState ()
foldM' f [] = return ()
foldM' f (x:xs) = f x >> foldM' f xs

--
dropUnless' :: (a -> CheckState Bool) -> [a] -> CheckState [a]
dropUnless' mpred [] = return []
dropUnless' mpred (x:xs)
   = do p <- mpred x
	if p then return (x:xs) else dropUnless' mpred xs

--
partition' :: (a -> CheckState Bool) -> [a] -> CheckState ([a],[a])
partition' mpred xs = part mpred [] [] xs
 where
 part mpred ts fs [] = return (reverse ts, reverse fs)
 part mpred ts fs (x:xs)
   = do p <- mpred x
	if p then part mpred (x:ts) fs xs
	     else part mpred ts (x:fs) xs

--
all' :: (a -> CheckState Bool) -> [a] -> CheckState Bool
all' mpred [] = return True
all' mpred (x:xs)
   = do p <- mpred x
	if p then all' mpred xs else return False



-------------------------------------------------------------------------------

--
isShadowing :: CState -> QualIdent -> Bool
isShadowing state qid
   = let sc = scope state
     in  maybe False isVariable (ScopeEnv.lookup qid sc)
	 && ScopeEnv.level qid sc < ScopeEnv.currentLevel sc

--
isUnref :: CState -> QualIdent -> Bool
isUnref state qid 
   = let sc = scope state
     in  maybe False (not . variableVisited) (ScopeEnv.lookup qid sc)
         && ScopeEnv.level qid sc == ScopeEnv.currentLevel sc

--
isVar :: CState -> QualIdent -> Bool
isVar state qid = maybe (isAnnonId (unqualify qid)) 
	           isVariable 
		   (ScopeEnv.lookup qid (scope state))

--
isCons :: CState -> QualIdent -> Bool
isCons state qid = maybe (isImportedCons state qid)
		         isConstructor
			 (ScopeEnv.lookup qid (scope state))
 where
 isImportedCons state qid
    = case (qualLookupValue qid (values state)) of
        (DataConstructor _ _):_    -> True
        (NewtypeConstructor _ _):_ -> True
        _                          -> False


--
isAnnonId :: Ident -> Bool
isAnnonId id = (name id) == "_"


-- Since type identifiers and normal identifiers (e.g. functions, variables
-- or constructors) don't share the same namespace, it is necessary
-- to distinguish them in the scope environment of the check state.
-- For this reason type identifiers are annotated with 1 and normal
-- identifiers are annotated with 0.
--
commonId :: Ident -> QualIdent
commonId id = qualify (unRenameIdent id)

--
typeId :: Ident -> QualIdent
typeId id = qualify (renameIdent id 1)


-------------------------------------------------------------------------------
-- Warnings...

unrefTypeVar :: Ident -> (Position, String)
unrefTypeVar id = 
  (positionOfIdent id,
   "unreferenced type variable \"" ++ show id ++ "\"")

unrefVar :: Ident -> (Position, String)
unrefVar id = 
  (positionOfIdent id,
   "unused declaration of variable \"" ++ show id ++ "\"")

shadowingVar :: Ident -> (Position, String)
shadowingVar id = 
  (positionOfIdent id,
   "shadowing symbol \"" ++ show id ++ "\"")

idleCaseAlts :: String
idleCaseAlts = "idle case alternative(s)"

overlappingCaseAlt :: String
overlappingCaseAlt = "redundant overlapping case alternative"

rulesNotTogether :: Ident -> Position -> (Position, String)
rulesNotTogether id pos
  = (positionOfIdent id,
     "rules for function \"" ++ show id ++ "\" "    
     ++ "are not together "
     ++ "(first occurrence at " 
     ++ show (line pos) ++ "." ++ show (column pos) ++ ")")

multiplyImportedModule :: ModuleIdent -> (Position, String)
multiplyImportedModule mid 
  = (positionOfModuleIdent mid,
     "module \"" ++ show mid ++ "\" was imported more than once")

multiplyImportedSymbol :: ModuleIdent -> Ident -> (Position, String)
multiplyImportedSymbol mid ident
  = (positionOfIdent ident,
     "symbol \"" ++ show ident ++ "\" was imported from module \""
     ++ show mid ++ "\" more than once")

multiplyHiddenSymbol :: ModuleIdent -> Ident -> (Position, String)
multiplyHiddenSymbol mid ident
  = (positionOfIdent ident,
     "symbol \"" ++ show ident ++ "\" from module \"" ++ show mid
     ++ "\" was hidden more than once")


-------------------------------------------------------------------------------
-- Miscellaneous

-- safer versions of 'tail' and 'head'
tail_ :: [a] -> [a] -> [a]
tail_ alt []     = alt
tail_ _   (_:xs) = xs


--
cmpListM :: Monad m => (a -> a -> m Bool) -> [a] -> [a] -> m Bool
cmpListM cmpM []     []     = return True
cmpListM cmpM (x:xs) (y:ys) = do c <- cmpM x y
				 if c then cmpListM cmpM xs ys 
				      else return False
cmpListM cmpM _      _      = return False


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------