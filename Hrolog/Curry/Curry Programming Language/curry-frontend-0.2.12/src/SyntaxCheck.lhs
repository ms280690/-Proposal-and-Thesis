
% $Id: SyntaxCheck.lhs,v 1.53 2004/02/15 22:10:37 wlux Exp $
%
% Copyright (c) 1999-2004, Wolfgang Lux
% See LICENSE for the full license.
%
% Modified by Martin Engelke (men@informatik.uni-kiel.de)
%
\nwfilename{SyntaxCheck.lhs}
\section{Syntax Checks}
After the type declarations have been checked, the compiler performs a
syntax check on the remaining declarations. This check disambiguates
nullary data constructors and variables which -- in contrast to
Haskell -- is not possible on purely syntactic criteria. In addition,
this pass checks for undefined as well as ambiguous variables and
constructors. In order to allow lifting of local definitions in
later phases, all local variables are renamed by adding a unique
key.\footnote{Actually, all variables defined in the same scope share
the same key.} Finally, all (adjacent) equations of a function are
merged into a single definition.
\begin{verbatim}

> module SyntaxCheck(syntaxCheck) where

> import Data.Maybe
> import Data.List
> import qualified Data.Map as Map
> import Control.Monad.State as S

> import Curry.Syntax
> import Curry.Syntax.Utils
> import Types
> import Curry.Base.Position
> import Curry.Base.Ident
> import Base
> import NestEnv
> import Utils

\end{verbatim}
The syntax checking proceeds as follows. First, the compiler extracts
information about all imported values and data constructors from the
imported (type) environments. Next, the data constructors defined in
the current module are entered into this environment. After this
all record labels are entered into the environment too. If a record
identifier is already assigned to a constructor, then an error will be
generated. Finally, all
declarations are checked within the resulting environment. In
addition, this process will also rename the local variables.
\begin{verbatim}

> syntaxCheck :: Bool -> ModuleIdent -> ImportEnv -> ArityEnv -> ValueEnv -> TCEnv -> [Decl] -> [Decl]
> syntaxCheck withExt m iEnv aEnv tyEnv tcEnv ds =
>   case findDouble (concatMap constrs tds) of
>     --Nothing -> tds ++ run (checkModule withExt m env vds)
>     Nothing -> map (checkTypeDecl withExt m) tds
>	        ++ run (checkModule withExt m env2 vds)
>     Just c -> errorAt' (duplicateData c)
>   where (tds,vds) = partition isTypeDecl ds
>	  (rs, tds') = partition isRecordDecl tds
>         env1 = foldr (bindTypes m) -- (bindConstrs m) 
>	               (globalEnv (fmap (renameInfo tcEnv iEnv aEnv) tyEnv)) 
>	               tds'
>	  env2 = foldr (bindTypes m) env1 rs

\end{verbatim}
A global state transformer is used for generating fresh integer keys
by which the variables get renamed.
\begin{verbatim}

> type RenameState a = S.State Int a

> run :: RenameState a -> a
> run m = S.evalState m (globalKey + 1)

> newId :: RenameState Int
> newId = S.modify succ >> S.get

\end{verbatim}
\ToDo{Probably the state transformer should use an \texttt{Integer} 
counter.}

A nested environment is used for recording information about the data
constructors and variables in the module. For every data constructor
its arity is saved. This is used for checking that all constructor
applications in patterns are saturated. For local variables the
environment records the new name of the variable after renaming.
Global variables are recorded with qualified identifiers in order
to distinguish multiply declared entities.

Currently records must explicitly be declared together with their labels.
When constructing or updating a record, it is necessary to compute 
all its labels using just one of them. Thus for each label 
the record identifier and all its labels are entered into the environment

\em{Note:} the function \texttt{qualLookupVar} has been extended to
allow the usage of the qualified list constructor \texttt{(prelude.:)}.
\begin{verbatim}

> type RenameEnv = NestEnv RenameInfo
> data RenameInfo = Constr Int 
>                 | GlobalVar Int QualIdent 
>                 | LocalVar Int Ident
>	          | RecordLabel QualIdent [Ident]
>	    deriving (Eq,Show)

> globalKey :: Int
> globalKey = uniqueId (mkIdent "")

> renameInfo :: TCEnv -> ImportEnv -> ArityEnv -> ValueInfo -> RenameInfo
> renameInfo tcEnv iEnv aEnv (DataConstructor _ (ForAllExist _ _ ty)) 
>    = Constr (arrowArity ty)
> renameInfo tcEnv iEnv aEnv (NewtypeConstructor _ _) 
>    = Constr 1
> renameInfo tcEnv iEnv aEnv (Value qid _)
>    = let (mmid, id) = (qualidMod qid, qualidId qid)
>          qid' = maybe qid 
>	                (\mid -> maybe qid 
>		                       (\mid' -> qualifyWith mid' id)
>				       (lookupAlias mid iEnv))
>		        mmid
>      in case (lookupArity id aEnv) of
>	    [ArityInfo _ arity] -> GlobalVar arity qid
>           rs -> case (qualLookupArity qid' aEnv) of
>	            [ArityInfo _ arity] -> GlobalVar arity qid
>	            _ -> maybe (internalError "renameInfo: missing arity")
>	                       (\ (ArityInfo _ arity) -> GlobalVar arity qid)
>		               (find (\ (ArityInfo qid'' _) 
>			              -> qid'' == qid) rs)
> renameInfo tcEnv iEnv aEnv (Label l r _)
>    = case (qualLookupTC r tcEnv) of
>        [AliasType _ _ (TypeRecord fs _)] ->
>          RecordLabel r (map fst fs)
>        _ -> internalError "renameInfo: no record"

\end{verbatim}
Since record types are currently translated into data types, it is
necessary to ensure that all identifiers for records and constructors
are different. Furthermore it is not allowed to declare a label more
than once.
\begin{verbatim}

> bindTypes :: ModuleIdent -> Decl -> RenameEnv -> RenameEnv
> bindTypes m (DataDecl _ tc _ cs) env = foldr (bindConstr m) env cs
> bindTypes m (NewtypeDecl _ tc _ nc) env = bindNewConstr m nc env
> bindTypes m (TypeDecl _ t _ (RecordType fs r)) env =
>    -- - | isJust r = internalError "bindTypes: illegal record declaration"
>    -- - | null fs = errorAt (positionOfIdent t) emptyRecord
>    -- - | otherwise =
>      case (qualLookupVar (qualifyWith m t) env) of
>        [] -> foldr (bindRecordLabel m t (concatMap fst fs)) env fs
>        rs | any isConstr rs -> errorAt' (illegalRecordId t)
>           | otherwise
>             -> foldr (bindRecordLabel m t (concatMap fst fs)) env fs
> bindTypes _ _ env = env

> bindRecordLabel :: ModuleIdent -> Ident -> [Ident] 
>	             -> ([Ident],TypeExpr) -> RenameEnv -> RenameEnv
> bindRecordLabel m t labels (ls,_) env = 
>     foldr (\l -> case (lookupVar l env) of
>                    [] -> bindGlobal m l
>                             (RecordLabel (qualifyWith m t) labels)
>                    _  -> errorAt' (duplicateDefinition l)
>	    ) env ls

> --bindConstrs :: ModuleIdent -> Decl -> RenameEnv -> RenameEnv
> --bindConstrs m (DataDecl _ tc _ cs) env = foldr (bindConstr m) env cs
> --bindConstrs m (NewtypeDecl _ tc _ nc) env = bindNewConstr m nc env
> --bindConstrs _ _ env = env

> bindConstr :: ModuleIdent -> ConstrDecl -> RenameEnv -> RenameEnv
> bindConstr m (ConstrDecl _ _ c tys) = bindGlobal m c (Constr (length tys))
> bindConstr m (ConOpDecl _ _ _ op _) = bindGlobal m op (Constr 2)

> bindNewConstr :: ModuleIdent -> NewConstrDecl -> RenameEnv -> RenameEnv
> bindNewConstr m (NewConstrDecl _ _ c _) = bindGlobal m c (Constr 1)

> bindFuncDecl :: ModuleIdent -> Decl -> RenameEnv -> RenameEnv
> bindFuncDecl m (FunctionDecl _ id equs) env
>    | null equs = internalError "bindFuncDecl: missing equations"
>    | otherwise = let (_,ts) = getFlatLhs (head equs)
>		   in  bindGlobal m 
>	                          id 
>			          (GlobalVar (length ts) (qualifyWith m id))
>	                          env
> bindFuncDecl m (ExternalDecl _ _ _ id texpr) env
>    = bindGlobal m id (GlobalVar (typeArity texpr) (qualifyWith m id)) env
> bindFuncDecl m (TypeSig _ ids texpr) env
>    = foldr bindTS env (map (qualifyWith m) ids)
>  where
>  bindTS qid env 
>     | null (qualLookupVar qid env)
>       = bindGlobal m (unqualify qid) (GlobalVar (typeArity texpr) qid) env
>     | otherwise
>       = env
> bindFuncDecl _ _ env = env

> bindVarDecl :: Decl -> RenameEnv -> RenameEnv
> bindVarDecl (FunctionDecl _ id equs) env
>    | null equs 
>      = internalError "bindFuncDecl: missing equations"
>    | otherwise 
>      = let (_,ts) = getFlatLhs (head equs)
>	 in  bindLocal (unRenameIdent id) (LocalVar (length ts) id) env
> bindVarDecl (PatternDecl p t _) env
>    = foldr bindVar env (bv t)
> bindVarDecl (ExtraVariables p vs) env
>    = foldr bindVar env vs 
> bindVarDecl _ env = env

> bindVar :: Ident -> RenameEnv -> RenameEnv
> bindVar v env
>   | v' == anonId = env
>   | otherwise = bindLocal v' (LocalVar 0 v) env
>   where v' = unRenameIdent v

> bindGlobal :: ModuleIdent -> Ident -> RenameInfo -> RenameEnv -> RenameEnv
> bindGlobal m c r = bindNestEnv c r . qualBindNestEnv (qualifyWith m c) r

> bindLocal :: Ident -> RenameInfo -> RenameEnv -> RenameEnv
> bindLocal f r = bindNestEnv f r

> lookupVar :: Ident -> RenameEnv -> [RenameInfo]
> lookupVar v env = lookupNestEnv v env ++! lookupTupleConstr v

> qualLookupVar :: QualIdent -> RenameEnv -> [RenameInfo]
> qualLookupVar v env =
>   qualLookupNestEnv v env
>   ++! qualLookupListCons v env
>   ++! lookupTupleConstr (unqualify v)

> qualLookupListCons :: QualIdent -> RenameEnv -> [RenameInfo]
> qualLookupListCons v env
>    | (isJust mmid) && ((fromJust mmid) == preludeMIdent) && (ident == consId)
>       = qualLookupNestEnv (qualify ident) env
>    | otherwise = []
>  where (mmid, ident) = (qualidMod v, qualidId v)

> lookupTupleConstr :: Ident -> [RenameInfo]
> lookupTupleConstr v
>   | isTupleId v = [Constr (tupleArity v)]
>   | otherwise = []

\end{verbatim}
When a module is checked, the global declaration group is checked. The
resulting renaming environment can be discarded. The same is true for
a goal. Note that all declarations in the goal must be considered as
local declarations.
\begin{verbatim}

> checkModule :: Bool -> ModuleIdent -> RenameEnv -> [Decl] -> RenameState [Decl]
> checkModule withExt m env ds = liftM snd (checkTopDecls withExt m env ds)

> checkTopDecls :: Bool -> ModuleIdent -> RenameEnv -> [Decl]
>               -> RenameState (RenameEnv,[Decl])
> checkTopDecls withExt m env ds = 
>   checkDeclGroup (bindFuncDecl m) withExt m globalKey env ds

> checkTypeDecl :: Bool -> ModuleIdent -> Decl -> Decl
> checkTypeDecl withExt m d@(TypeDecl p r tvs (RecordType fs rty))
>   | not withExt = errorAt (positionOfIdent r) noRecordExt
>   | isJust rty = internalError "checkTypeDecl - illegal record type"
>   | null fs = errorAt (positionOfIdent r) emptyRecord
>   | otherwise = TypeDecl p r tvs (RecordType fs Nothing)
> checkTypeDecl _ _ d = d

\end{verbatim}
Each declaration group opens a new scope and uses a distinct key
for renaming the variables in this scope. In a declaration group,
first the left hand sides of all declarations are checked, next the
compiler checks that there is a definition for every type signature
and evaluation annotation in this group. Finally, the right hand sides
are checked and adjacent equations for the same function are merged
into a single definition.

The function \texttt{checkDeclLhs} also handles the case where a
pattern declaration is recognized as a function declaration by the
parser. This happens, e.g., for the declaration \verb|where Just x = y|
because the parser cannot distinguish nullary constructors and
functions. Note that pattern declarations are not allowed on the
top-level.
\begin{verbatim}

> checkLocalDecls :: Bool -> ModuleIdent -> RenameEnv -> [Decl] 
>                  -> RenameState (RenameEnv,[Decl])
> checkLocalDecls withExt m env ds =
>   newId >>= \k -> checkDeclGroup bindVarDecl withExt m k (nestEnv env) ds

> checkDeclGroup :: (Decl -> RenameEnv -> RenameEnv) -> Bool -> ModuleIdent
>                 -> Int -> RenameEnv -> [Decl] 
>                 -> RenameState (RenameEnv,[Decl])
> checkDeclGroup bindDecl withExt m k env ds =
>   mapM (checkDeclLhs withExt k m env) ds' >>=
>   checkDecls bindDecl withExt m env . joinEquations
>  where ds' = sortFuncDecls ds

> checkDeclLhs :: Bool -> Int -> ModuleIdent -> RenameEnv -> Decl -> RenameState Decl
> checkDeclLhs withExt k _ _ (InfixDecl p fix pr ops) =
>   return (InfixDecl p fix pr (map (flip renameIdent k) ops))
> checkDeclLhs withExt k _ env (TypeSig p vs ty) =
>   return (TypeSig p (map (checkVar "type signature" k env) vs) ty)
> checkDeclLhs withExt k _ env (EvalAnnot p fs ev) =
>   return (EvalAnnot p (map (checkVar "evaluation annotation" k env) fs) ev)
> checkDeclLhs withExt k m env (FunctionDecl p _ eqs) = 
>   checkEquationLhs withExt k m env p eqs
> checkDeclLhs withExt k _ env (ExternalDecl p cc ie f ty) =
>   return (ExternalDecl p cc ie (checkVar "external declaration" k env f) ty)
> checkDeclLhs withExt k _ env (FlatExternalDecl p fs) =
>   return (FlatExternalDecl p
>             (map (checkVar "external declaration" k env) fs))
> checkDeclLhs withExt k m env (PatternDecl p t rhs) =
>   do
>     t' <- checkConstrTerm withExt k p m env t
>     return (PatternDecl p t' rhs)
> checkDeclLhs withExt k _ env (ExtraVariables p vs) =
>   return (ExtraVariables p
>             (map (checkVar "free variables declaration" k env) vs))
> checkDeclLhs _ _ _ _ d = return d

> checkEquationLhs :: Bool -> Int -> ModuleIdent -> RenameEnv -> Position 
>	           -> [Equation] -> RenameState Decl
> checkEquationLhs withExt k m env p [Equation p' lhs rhs] =
>   either (return . funDecl) (checkDeclLhs withExt k m env . patDecl)
>          (checkEqLhs m k env p' lhs)
>   where funDecl (f,lhs) = FunctionDecl p f [Equation p' lhs rhs]
>         patDecl t
>           | k == globalKey = errorAt p noToplevelPattern
>           | otherwise = PatternDecl p' t rhs
> checkEquationLhs _ _ _ _ _ _ = internalError "checkEquationLhs"

> checkEqLhs :: ModuleIdent -> Int -> RenameEnv -> Position -> Lhs
>            -> Either (Ident,Lhs) ConstrTerm
> checkEqLhs m k env _ (FunLhs f ts)
>   | isDataConstr f env
>     = if k /= globalKey
>       then Right (ConstructorPattern (qualify f) ts)
>       else if null (qualLookupVar (qualifyWith m f) env)
>            then Left (f',FunLhs f' ts)
>	     else errorAt (positionOfIdent f) noToplevelPattern
>   | otherwise = Left (f',FunLhs f' ts)
>   where f' = renameIdent f k
> checkEqLhs m k env p (OpLhs t1 op t2)
>   | isDataConstr op env 
>     = if k /= globalKey
>       then checkOpLhs k env (infixPattern t1 (qualify op)) t2
>       else if null (qualLookupVar (qualifyWith m op) env)
>            then Left (op',OpLhs t1 op' t2)
>	     else errorAt p noToplevelPattern
>   | otherwise = Left (op',OpLhs t1 op' t2)
>   where op' = renameIdent op k
>         infixPattern (InfixPattern t1 op1 t2) op2 t3 =
>           InfixPattern t1 op1 (infixPattern t2 op2 t3)
>         infixPattern t1 op t2 = InfixPattern t1 op t2
> checkEqLhs m k env p (ApLhs lhs ts) =
>   case checkEqLhs m k env p lhs of
>     Left (f',lhs') -> Left (f',ApLhs lhs' ts)
>     Right _ -> errorAt' $ nonVariable "curried definition" f
>   where (f,_) = flatLhs lhs

> checkOpLhs :: Int -> RenameEnv -> (ConstrTerm -> ConstrTerm) -> ConstrTerm
>            -> Either (Ident,Lhs) ConstrTerm
> checkOpLhs k env f (InfixPattern t1 op t2)
>   | isJust m || isDataConstr op' env =
>       checkOpLhs k env (f . InfixPattern t1 op) t2
>   | otherwise = Left (op'',OpLhs (f t1) op'' t2)
>   where (m,op') = (qualidMod op, qualidId op)
>         op'' = renameIdent op' k
> checkOpLhs _ _ f t = Right (f t)

> checkVar :: String -> Int -> RenameEnv -> Ident -> Ident
> checkVar what k env v 
>   | False && isDataConstr v env = errorAt' (nonVariable what v)---------------
>   | otherwise = renameIdent v k


> checkDecls :: (Decl -> RenameEnv -> RenameEnv) -> Bool -> ModuleIdent
>	        -> RenameEnv -> [Decl] -> RenameState (RenameEnv,[Decl])
> checkDecls bindDecl withExt m env ds = 
>   case findDouble bvs of
>     Nothing ->
>       case findDouble tys of
>         Nothing ->
>           case findDouble evs of
>             Nothing ->
>               case filter (`notElem` tys) fs' of
>                 [] -> liftM ((,) env') 
>		              (mapM (checkDeclRhs withExt bvs m env'') ds)
>                 f : _ -> errorAt' (noTypeSig f)
>             Just v -> errorAt' (duplicateEvalAnnot v)
>         Just v -> errorAt' (duplicateTypeSig v)
>     Just v -> errorAt' (duplicateDefinition v)
>   where vds = filter isValueDecl ds
>	  tds = filter isTypeSig ds
>         bvs = concat (map vars vds)
>         tys = concat (map vars tds)
>         evs = concat (map vars (filter isEvalAnnot ds))
>         fs' = [f | FlatExternalDecl _ fs <- ds, f <- fs]
>         env' = foldr bindDecl env vds
>         env'' = foldr bindDecl env' tds

> checkDeclRhs :: Bool -> [Ident] -> ModuleIdent -> RenameEnv -> Decl 
>              -> RenameState Decl
> checkDeclRhs withExt bvs _ _ (TypeSig p vs ty) =
>   return (TypeSig p (map (checkLocalVar bvs ) vs) ty)
> checkDeclRhs withExt bvs _ _ (EvalAnnot p vs ev) =
>   return (EvalAnnot p (map (checkLocalVar bvs ) vs) ev)
> checkDeclRhs withExt _ m env (FunctionDecl p f eqs) =
>   liftM (FunctionDecl p f) (mapM (checkEquation withExt m env) eqs)
> checkDeclRhs withExt _ m env (PatternDecl p t rhs) =
>   liftM (PatternDecl p t) (checkRhs withExt m env rhs)
> checkDeclRhs _ _ _ _ d = return d

> checkLocalVar :: [Ident] -> Ident -> Ident
> checkLocalVar bvs v
>   | v `elem` bvs = v
>   | otherwise = errorAt' (noBody v)

> joinEquations :: [Decl] -> [Decl]
> joinEquations [] = []
> joinEquations (FunctionDecl p f eqs : FunctionDecl p' f' [eq] : ds)
>   | f == f' =
>       if arity (head eqs) == arity eq then
>         joinEquations (FunctionDecl p f (eqs ++ [eq]) : ds)
>       else
>         errorAt' (differentArity f)
>   where arity (Equation _ lhs _) = length $ snd $ flatLhs lhs
> joinEquations (d : ds) = d : joinEquations ds

> checkEquation :: Bool -> ModuleIdent -> RenameEnv -> Equation -> RenameState Equation
> checkEquation withExt m env (Equation p lhs rhs) =
>   do
>     (env',lhs') <- checkLhs withExt p m env lhs
>     rhs' <- checkRhs withExt m env' rhs
>     return (Equation p lhs' rhs')

> checkLhs :: Bool -> Position -> ModuleIdent -> RenameEnv -> Lhs 
>             -> RenameState (RenameEnv,Lhs)
> checkLhs withExt p m env lhs =
>   newId >>= \k ->
>   checkLhsTerm withExt k p m env lhs >>=
>   return . checkConstrTerms withExt (nestEnv env)

> checkLhsTerm :: Bool -> Int -> Position -> ModuleIdent -> RenameEnv -> Lhs 
>                 -> RenameState Lhs
> checkLhsTerm withExt k p m env (FunLhs f ts) =
>   do
>     ts' <- mapM (checkConstrTerm withExt k p m env) ts
>     return (FunLhs f ts')
> checkLhsTerm withExt k p m env (OpLhs t1 op t2) =
>   let wrongCalls = concatMap (checkParenConstrTerm (Just (qualify op)))
>                               [t1,t2] in
>   if not (null wrongCalls)
>     then errorAt (positionOfIdent op) 
>                  (infixWithoutParens wrongCalls)
>     else  do
>       t1' <- checkConstrTerm withExt k p m env t1
>       t2' <- checkConstrTerm withExt k p m env t2 
>       return (OpLhs t1' op t2')
>
> checkLhsTerm withExt k p m env (ApLhs lhs ts) =
>   do
>     lhs' <- checkLhsTerm withExt k p m env lhs
>     ts' <- mapM (checkConstrTerm withExt k p m env) ts
>     return (ApLhs lhs' ts')

> checkArgs :: Bool -> Position -> ModuleIdent -> RenameEnv -> [ConstrTerm]
>           -> RenameState (RenameEnv,[ConstrTerm])
> checkArgs withExt p m env ts =
>   newId >>= \k ->
>   mapM (checkConstrTerm withExt k p m env) ts >>=
>   return . checkConstrTerms withExt (nestEnv env)

> checkConstrTerms :: QuantExpr t => Bool -> RenameEnv -> t
>                  -> (RenameEnv,t)
> checkConstrTerms withExt env ts =
>   case findDouble bvs of
>     Nothing -> (foldr bindVar env bvs,ts)
>     Just v -> errorAt' (duplicateVariable v)
>   where bvs = bv ts

> checkConstrTerm :: Bool -> Int -> Position -> ModuleIdent -> RenameEnv
>	             -> ConstrTerm -> RenameState ConstrTerm
> checkConstrTerm _ _ _ _ _ (LiteralPattern l) =
>   liftM LiteralPattern (renameLiteral l)
> checkConstrTerm _ _ _ _ _ (NegativePattern op l) =
>   liftM (NegativePattern op) (renameLiteral l)
> checkConstrTerm withExt k p m env (VariablePattern v)
>   | v == anonId 
>     = liftM (VariablePattern . renameIdent anonId) newId
>   | otherwise 
>     = checkConstrTerm withExt k p m env (ConstructorPattern (qualify v) [])
> checkConstrTerm withExt k p m env (ConstructorPattern c ts) =
>   case qualLookupVar c env of
>     [Constr n]
>       | n == n' ->
>           liftM (ConstructorPattern c) 
>	          (mapM (checkConstrTerm withExt k p m env) ts)
>       | otherwise -> errorAt' (wrongArity c n n')
>       where n' = length ts
>     [r]
>       | null ts && not (isQualified c) ->
>	    return (VariablePattern (renameIdent (varIdent r) k))
>       | withExt ->
>           do ts' <- mapM (checkConstrTerm withExt k p m env) ts
>	       if n' > n
>	          then let (ts1,ts2) = splitAt n ts'
>	               in  return (genFuncPattAppl 
>			             (FunctionPattern (qualVarIdent r) 
>				                      ts1) 
>	                             ts2)
>	          else return (FunctionPattern (qualVarIdent r) ts')
>       | otherwise -> errorAt (positionOfQualIdent c) noFuncPattExt  
>	where n = arity r
>	      n' = length ts
>     rs -> case (qualLookupVar (qualQualify m c) env) of
>             []
>               | null ts && not (isQualified c) ->
>	            return (VariablePattern (renameIdent (unqualify c) k))
>	        | null rs -> errorAt' (undefinedData c)
>		| otherwise -> errorAt' (ambiguousData c)
>             [Constr n]
>               | n == n' ->
>                   liftM (ConstructorPattern (qualQualify m c)) 
>                         (mapM (checkConstrTerm withExt k p m env) ts)
>               | otherwise -> errorAt' (wrongArity c n n')
>               where n' = length ts
>	      [r]
>	        | null ts && not (isQualified c) ->
>                   return (VariablePattern (renameIdent (varIdent r) k))
>               | withExt ->
>	            do ts' <- mapM (checkConstrTerm withExt k p m env) ts
>	               if n' > n
>	                  then let (ts1,ts2) = splitAt n ts'
>	                       in  return 
>			             (genFuncPattAppl 
>			                (FunctionPattern (qualVarIdent r) ts1) 
>	                                ts2)
>	                  else return (FunctionPattern (qualVarIdent r) ts')
>	        | otherwise -> errorAt (positionOfQualIdent c) noFuncPattExt
>               where n = arity r
>		      n' = length ts
>             _ -> errorAt' (ambiguousData c)
> checkConstrTerm withExt k p m env (InfixPattern t1 op t2) =
>   case (qualLookupVar op env) of
>     [Constr n]
>       | n == 2 ->
>           do t1' <- checkConstrTerm withExt k p m env t1
>	       t2' <- checkConstrTerm withExt k p m env t2
>              return (InfixPattern t1' op t2') 
>       | otherwise -> errorAt' (wrongArity op n 2)
>     [r]
>       | withExt ->
>           do t1' <- checkConstrTerm withExt k p m env t1
>	       t2' <- checkConstrTerm withExt k p m env t2
>              return (InfixFuncPattern t1' op t2')
>       | otherwise -> errorAt p noFuncPattExt    
>     rs -> case (qualLookupVar (qualQualify m op) env) of
>             [] | null rs -> errorAt' (undefinedData op)
>                | otherwise -> errorAt' (ambiguousData op)
>             [Constr n]
>               | n == 2 ->
>                   do t1' <- checkConstrTerm withExt k p m env t1
>	               t2' <- checkConstrTerm withExt k p m env t2
>                      return (InfixPattern t1' (qualQualify m op) t2') 
>               | otherwise -> errorAt' (wrongArity op n 2)
>	      [r]
>               | withExt ->
>	            do t1' <- checkConstrTerm withExt k p m env t1
>	               t2' <- checkConstrTerm withExt k p m env t2
>		       return (InfixFuncPattern t1' (qualQualify m op) t2')
>	        | otherwise -> errorAt p noFuncPattExt
>             _ -> errorAt' (ambiguousData op)
> checkConstrTerm withExt k p m env (ParenPattern t) =
>   liftM ParenPattern (checkConstrTerm withExt k p m env t)
> checkConstrTerm withExt k p m env (TuplePattern pos ts) =
>   liftM (TuplePattern pos) (mapM (checkConstrTerm withExt k p m env) ts)
> checkConstrTerm withExt k p m env (ListPattern pos ts) =
>   liftM (ListPattern pos) (mapM (checkConstrTerm withExt k p m env) ts)
> checkConstrTerm withExt k p m env (AsPattern v t) =
>   liftM (AsPattern (checkVar "@ pattern" k env v))
>         (checkConstrTerm withExt k p m env t)
> checkConstrTerm withExt k p m env (LazyPattern pos t) =
>   liftM (LazyPattern pos) (checkConstrTerm withExt k p m env t)
> checkConstrTerm withExt k p m env (RecordPattern fs t)
>   | not withExt = errorAt p noRecordExt
>   | not (null fs) =
>     let (Field _ label _) = head fs
>     in  case (lookupVar label env) of
>           [] -> errorAt' (undefinedLabel label)
>           [RecordLabel r ls]
>             | not (null duplicates) ->
>	        errorAt' (duplicateLabel (head duplicates))
>	      | isNothing t && not (null missings) ->
>	        errorAt (positionOfIdent label) 
>                       (missingLabel (head missings) r "record pattern")
>             | maybe True ((==) (VariablePattern anonId)) t ->
>	        do fs' <- mapM (checkFieldPatt withExt k m r env) fs
>	           t'  <- maybe (return Nothing)
>	                        (\t' -> checkConstrTerm withExt k p m env t'
>			                >>= return . Just)
>			        t
>	           return (RecordPattern fs' t')
>	      | otherwise -> errorAt p illegalRecordPatt
>            where ls' = map fieldLabel fs
>                  duplicates = maybeToList (dup ls')
>		   missings = ls \\ ls'
>	    [_] -> errorAt' (notALabel label)
>	    _ -> errorAt' (duplicateDefinition label)
>   | otherwise = errorAt p emptyRecord

> checkFieldPatt :: Bool -> Int -> ModuleIdent -> QualIdent -> RenameEnv
>	            -> Field ConstrTerm -> RenameState (Field ConstrTerm)
> checkFieldPatt withExt k m r env (Field p l t)
>    = case (lookupVar l env) of
>        [] -> errorAt' (undefinedLabel l)
>        [RecordLabel r' _]
>          | r == r' -> do t' <- checkConstrTerm withExt k 
>                                   (positionOfIdent l) m env t
>		           return (Field p l t')
>          | otherwise -> errorAt' (illegalLabel l r)
>        [_] -> errorAt' (notALabel l)
>	 _ -> errorAt' (duplicateDefinition l)

> checkRhs :: Bool -> ModuleIdent -> RenameEnv -> Rhs -> RenameState Rhs
> checkRhs withExt m env (SimpleRhs p e ds) =
>   do
>     (env',ds') <- checkLocalDecls withExt m env ds
>     e' <- checkExpr withExt p m env' e
>     return (SimpleRhs p e' ds')
> checkRhs withExt m env (GuardedRhs es ds) =
>   do
>     (env',ds') <- checkLocalDecls withExt m env ds
>     es' <- mapM (checkCondExpr withExt m env') es
>     return (GuardedRhs es' ds')

> checkCondExpr :: Bool -> ModuleIdent -> RenameEnv -> CondExpr -> RenameState CondExpr
> checkCondExpr withExt m env (CondExpr p g e) =
>   do
>     g' <- checkExpr withExt p m env g
>     e' <- checkExpr withExt p m env e
>     return (CondExpr p g' e')

> checkExpr :: Bool -> Position -> ModuleIdent -> RenameEnv -> Expression 
>           -> RenameState Expression
> checkExpr _ _ _ _ (Literal l) = liftM Literal (renameLiteral l)
> checkExpr withExt _ m env (Variable v) =
>   case (qualLookupVar v env) of
>     [] ->  errorAt' (undefinedVariable v)
>     [Constr _] -> return (Constructor v)
>     [GlobalVar _ _] -> return (Variable v)
>     [LocalVar _ v'] -> return (Variable (qualify v'))
>     rs -> case (qualLookupVar (qualQualify m v) env) of
>             [] -> errorAt' (ambiguousIdent rs v)
>             [Constr _] -> return (Constructor v)
>             [GlobalVar _ _] -> return (Variable v)
>             [LocalVar _ v'] -> return (Variable (qualify v'))
>             rs' -> errorAt' (ambiguousIdent rs' v)
> checkExpr withExt p m env (Constructor c) = 
>   checkExpr withExt p m env (Variable c)
> checkExpr withExt p m env (Paren e) = 
>   liftM Paren (checkExpr withExt p m env e)
> checkExpr withExt p m env (Typed e ty) = 
>   liftM (flip Typed ty) (checkExpr withExt p m env e)
> checkExpr withExt p m env (Tuple pos es) = 
>   liftM (Tuple pos) (mapM (checkExpr withExt p m env) es)
> checkExpr withExt p m env (List pos es) = 
>   liftM (List pos) (mapM (checkExpr withExt p m env) es)
> checkExpr withExt p m env (ListCompr pos e qs) =
>   do
>     (env',qs') <- mapAccumM (checkStatement withExt p m) env qs
>     e' <- checkExpr withExt p m env' e
>     return (ListCompr pos e' qs')
> checkExpr withExt p m env (EnumFrom e) = 
>   liftM EnumFrom (checkExpr withExt p m env e)
> checkExpr withExt p m env (EnumFromThen e1 e2) =
>   do
>     e1' <- checkExpr withExt p m env e1
>     e2' <- checkExpr withExt p m env e2
>     return (EnumFromThen e1' e2')
> checkExpr withExt p m env (EnumFromTo e1 e2) =
>   do
>     e1' <- checkExpr withExt p m env e1
>     e2' <- checkExpr withExt p m env e2
>     return (EnumFromTo e1' e2')
> checkExpr withExt p m env (EnumFromThenTo e1 e2 e3) =
>   do
>     e1' <- checkExpr withExt p m env e1
>     e2' <- checkExpr withExt p m env e2
>     e3' <- checkExpr withExt p m env e3
>     return (EnumFromThenTo e1' e2' e3')
> checkExpr withExt p m env (UnaryMinus op e) = 
>   liftM (UnaryMinus op) (checkExpr withExt p m env e)
> checkExpr withExt p m env (Apply e1 e2) =
>   do
>     e1' <- checkExpr withExt p m env e1
>     e2' <- checkExpr withExt p m env e2
>     return (Apply e1' e2')
> checkExpr withExt p m env (InfixApply e1 op e2) =
>   do
>     e1' <- checkExpr withExt p m env e1
>     e2' <- checkExpr withExt p m env e2
>     return (InfixApply e1' (checkOp m env op) e2')
> checkExpr withExt p m env (LeftSection e op) =
>   liftM (flip LeftSection (checkOp m env op)) (checkExpr withExt p m env e)
> checkExpr withExt p m env (RightSection op e) =
>   liftM (RightSection (checkOp m env op)) (checkExpr withExt p m env e)
> checkExpr withExt p m env (Lambda r ts e) =
>   do
>     (env',ts') <- checkArgs withExt p m env ts
>     e' <- checkExpr withExt p m env' e
>     return (Lambda r ts' e')
> checkExpr withExt p m env (Let ds e) =
>   do
>     (env',ds') <- checkLocalDecls withExt m env ds
>     e' <- checkExpr withExt p m env' e
>     return (Let ds' e')
> checkExpr withExt p m env (Do sts e) =
>   do
>     (env',sts') <- mapAccumM (checkStatement withExt p m) env sts
>     e' <- checkExpr withExt p m env' e
>     return (Do sts' e')
> checkExpr withExt p m env (IfThenElse r e1 e2 e3) =
>   do
>     e1' <- checkExpr withExt p m env e1
>     e2' <- checkExpr withExt p m env e2
>     e3' <- checkExpr withExt p m env e3
>     return (IfThenElse r e1' e2' e3')
> checkExpr withExt p m env (Case r e alts) =
>   do
>     e' <- checkExpr withExt p m env e
>     alts' <- mapM (checkAlt withExt m env) alts
>     return (Case r e' alts')
> checkExpr withExt p m env (RecordConstr fs)
>   | not withExt = errorAt p noRecordExt
>   | not (null fs) = 
>     let (Field _ label _) = head fs
>     in  case (lookupVar label env) of
>           [] -> errorAt' (undefinedLabel label)
>	    [RecordLabel r ls]
>              | not (null duplicates) ->
>                errorAt' (duplicateLabel (head duplicates))
>              | not (null missings) ->
>	         errorAt (positionOfIdent label) 
>                        (missingLabel (head missings) r "record construction")
>	       | otherwise ->
>	         do fs' <- mapM (checkFieldExpr withExt m r env) fs
>	            return (RecordConstr fs')
>	      where ls' = map fieldLabel fs
>	            duplicates = maybeToList (dup ls')
>		    missings = ls \\ ls'
>           [_] -> errorAt' (notALabel label)
>	    _ -> errorAt' (duplicateDefinition label)
>   | otherwise = errorAt p emptyRecord
> checkExpr withExt p m env (RecordSelection e l)
>   | not withExt = errorAt p noRecordExt
>   | otherwise =
>     case (lookupVar l env) of
>       [] -> errorAt' (undefinedLabel l)
>       [RecordLabel r ls] ->
>         do e' <- checkExpr withExt p m env e
>            return (RecordSelection e' l)
>       [_] -> errorAt' (notALabel l)
>       _ -> errorAt' (duplicateDefinition l)
> checkExpr withExt p m env (RecordUpdate fs e)
>   | not withExt = errorAt p noRecordExt
>   | not (null fs) =
>     let (Field _ label _) = head fs
>     in  case (lookupVar label env) of
>           [] -> errorAt' (undefinedLabel label)
>	    [RecordLabel r ls]
>             | not (null duplicates) ->
>	        errorAt' (duplicateLabel (head duplicates))
>	      | otherwise ->
>	        do fs' <- mapM (checkFieldExpr withExt m r env) fs
>	           e' <- checkExpr withExt (positionOfIdent label) m env e
>	           return (RecordUpdate fs' e')
>	      where duplicates = maybeToList (dup (map fieldLabel fs))
>	    [_] -> errorAt' (notALabel label)
>	    _ -> errorAt' (duplicateDefinition label)
>   | otherwise = errorAt p emptyRecord

> checkStatement :: Bool -> Position -> ModuleIdent -> RenameEnv -> Statement
>                -> RenameState (RenameEnv,Statement)
> checkStatement withExt p m env (StmtExpr pos e) =
>   do
>     e' <- checkExpr withExt p m env e
>     return (env,StmtExpr pos e')
> checkStatement withExt p m env (StmtBind pos t e) =
>   do
>     e' <- checkExpr withExt p m env e
>     (env',[t']) <- checkArgs withExt p m env [t]
>     return (env',StmtBind pos t' e')
> checkStatement withExt _ m env (StmtDecl ds) =
>   do
>     (env',ds') <- checkLocalDecls withExt m env ds
>     return (env',StmtDecl ds')

> checkAlt :: Bool -> ModuleIdent -> RenameEnv -> Alt -> RenameState Alt
> checkAlt withExt m env (Alt p t rhs) =
>   do
>     (env',[t']) <- checkArgs withExt p m env [t]
>     rhs' <- checkRhs withExt m env' rhs
>     return (Alt p t' rhs')

> checkFieldExpr :: Bool -> ModuleIdent -> QualIdent -> RenameEnv 
>	            -> Field Expression -> RenameState (Field Expression)
> checkFieldExpr withExt m r env (Field p l e)
>    = case (lookupVar l env) of
>        [] -> errorAt' (undefinedLabel l)
>        [RecordLabel r' _]
>          | r == r' -> do e' <- checkExpr withExt (positionOfIdent l) m env e
>		           return (Field p l e')
>          | otherwise -> errorAt' (illegalLabel l r)
>        [_] -> errorAt' (notALabel l)
>	 _ -> errorAt' (duplicateDefinition l)


> checkOp :: ModuleIdent -> RenameEnv -> InfixOp -> InfixOp
> checkOp m env op =
>   case (qualLookupVar v env) of
>     [] -> errorAt' (undefinedVariable v)
>     [Constr _] -> InfixConstr v
>     [GlobalVar _ _] -> InfixOp v
>     [LocalVar _ v'] -> InfixOp (qualify v')
>     rs -> case (qualLookupVar (qualQualify m v) env) of
>             [] -> errorAt' (ambiguousIdent rs v)
>             [Constr _] -> InfixConstr v
>             [GlobalVar _ _] -> InfixOp v
>             [LocalVar _ v'] -> InfixOp (qualify v')
>             rs' -> errorAt' (ambiguousIdent rs' v)
>   where v = opName op

\end{verbatim}
Auxiliary definitions.
\begin{verbatim}

> constrs :: Decl -> [Ident]
> constrs (DataDecl _ _ _ cs) = map constr cs
>   where constr (ConstrDecl _ _ c _) = c
>         constr (ConOpDecl _ _ _ op _) = op
> constrs (NewtypeDecl _ _ _ (NewConstrDecl _ _ c _)) = [c]
> constrs _ = []

> vars :: Decl -> [Ident]
> vars (TypeSig p fs _) = fs
> vars (EvalAnnot p fs _) = fs
> vars (FunctionDecl p f _) = [f]
> vars (ExternalDecl p _ _ f _) = [f]
> vars (FlatExternalDecl p fs) = fs
> vars (PatternDecl p t _) = (bv t)
> vars (ExtraVariables p vs) = vs
> vars _ = []

> renameLiteral :: Literal -> RenameState Literal
> renameLiteral (Int v i) = liftM (flip Int i . renameIdent v) newId
> renameLiteral l = return l


Since the compiler expects all rules of the same function to be together,
it is necessary to sort the list of declarations.

> sortFuncDecls :: [Decl] -> [Decl]
> sortFuncDecls decls = sortFD Map.empty [] decls
>  where
>  sortFD env res [] = reverse res
>  sortFD env res (decl:decls)
>     = case decl of
>	  FunctionDecl _ ident _
>	     | isJust (Map.lookup ident env)
>	       -> sortFD env (insertBy cmpFuncDecl decl res) decls
>	     | otherwise
>              -> sortFD (Map.insert ident () env) (decl:res) decls
>	  _    -> sortFD env (decl:res) decls

> cmpFuncDecl :: Decl -> Decl -> Ordering
> cmpFuncDecl (FunctionDecl _ id1 _) (FunctionDecl _ id2 _)
>    | id1 == id2 = EQ
>    | otherwise  = GT
> cmpFuncDecl decl1 decl2 = GT

cmpPos :: Position -> Position -> Ordering
cmpPos p1 p2 | lp1 < lp2  = LT
             | lp1 == lp2 = EQ
             | otherwise  = GT
 where lp1 = line p1
       lp2 = line p2


\end{verbatim}
Due to the lack of a capitalization convention in Curry, it is
possible that an identifier may ambiguously refer to a data
constructor and a function provided that both are imported from some
other module. When checking whether an identifier denotes a
constructor there are two options with regard to ambiguous
identifiers:
\begin{enumerate}
\item Handle the identifier as a data constructor if at least one of
  the imported names is a data constructor.
\item Handle the identifier as a data constructor only if all imported
  entities are data constructors.
\end{enumerate}
We choose the first possibility here because in the second case a
redefinition of a constructor can magically become possible if a
function with the same name is imported. It seems better to warn
the user about the fact that the identifier is ambiguous.
\begin{verbatim}

> isDataConstr :: Ident -> RenameEnv -> Bool
> isDataConstr v = any isConstr . lookupVar v . globalEnv . toplevelEnv

> isConstr :: RenameInfo -> Bool
> isConstr (Constr _) = True
> isConstr (GlobalVar _ _) = False
> isConstr (LocalVar _ _) = False
> isConstr (RecordLabel _ _) = False

> varIdent :: RenameInfo -> Ident
> varIdent (GlobalVar _ v) = unqualify v
> varIdent (LocalVar _ v) =  v
> varIdent _ = internalError "not a variable"

> qualVarIdent :: RenameInfo -> QualIdent
> qualVarIdent (GlobalVar _ v) = v
> qualVarIdent (LocalVar _ v) = qualify v
> qualVarIdent _ = internalError "not a qualified variable"

> arity :: RenameInfo -> Int
> arity (Constr n) = n
> arity (GlobalVar n _) = n
> arity (LocalVar n _) = n
> arity (RecordLabel _ ls) = length ls

\end{verbatim}
Unlike expressions, constructor terms have no possibility to represent
over-applications in function patterns. Therefore it is necessary to
transform them to nested
function patterns using the prelude function \texttt{apply}. E.g. the
the function pattern \texttt{(id id 10)} is transformed to
\texttt{(apply (id id) 10)}
\begin{verbatim}

> genFuncPattAppl :: ConstrTerm -> [ConstrTerm] -> ConstrTerm
> genFuncPattAppl term [] = term
> genFuncPattAppl term (t:ts) 
>    = FunctionPattern qApplyId [genFuncPattAppl term ts, t]
>  where
>  qApplyId = qualifyWith preludeMIdent (mkIdent "apply")

\end{verbatim}
Miscellaneous functions.
\begin{verbatim}

> typeArity :: TypeExpr -> Int
> typeArity (ArrowType _ t2) = 1 + typeArity t2
> typeArity _                = 0

> getFlatLhs :: Equation -> (Ident,[ConstrTerm])
> getFlatLhs (Equation  _ lhs _) = flatLhs lhs

> dup :: Eq a => [a] -> Maybe a
> dup [] = Nothing
> dup (x:xs) | elem x xs = Just x
>            | otherwise = dup xs

\end{verbatim}
Error messages.
\begin{verbatim}

> undefinedVariable :: QualIdent -> (Position,String)
> undefinedVariable v = 
>     (positionOfQualIdent v,
>      qualName v ++ " is undefined")

> undefinedData :: QualIdent -> (Position,String)
> undefinedData c =  
>     (positionOfQualIdent c,
>      "Undefined data constructor " ++ qualName c)

> undefinedLabel :: Ident -> (Position,String)
> undefinedLabel l =   
>     (positionOfIdent l,
>      "Undefined record label `" ++ name l ++ "`")

> ambiguousIdent :: [RenameInfo] -> QualIdent -> (Position,String)
> ambiguousIdent rs
>   | any isConstr rs = ambiguousData
>   | otherwise = ambiguousVariable

> ambiguousVariable :: QualIdent -> (Position,String)
> ambiguousVariable v =  
>     (positionOfQualIdent v,
>      "Ambiguous variable " ++ qualName v)

> ambiguousData :: QualIdent -> (Position,String)
> ambiguousData c =  
>     (positionOfQualIdent c,
>      "Ambiguous data constructor " ++ qualName c)

> duplicateDefinition :: Ident -> (Position,String)
> duplicateDefinition v =
>     (positionOfIdent v,
>      "More than one definition for `" ++ name v ++ "`")

> duplicateVariable :: Ident -> (Position,String)
> duplicateVariable v = 
>     (positionOfIdent v,
>      name v ++ " occurs more than once in pattern")

> duplicateData :: Ident -> (Position,String)
> duplicateData c = 
>     (positionOfIdent c,
>      "More than one definition for data constructor `"
>	            ++ name c ++ "`")

> duplicateTypeSig :: Ident -> (Position,String)
> duplicateTypeSig v =  
>     (positionOfIdent v,
>      "More than one type signature for `" ++ name v ++ "`")

> duplicateEvalAnnot :: Ident -> (Position,String)
> duplicateEvalAnnot v =   
>     (positionOfIdent v,
>      "More than one eval annotation for `" ++ name v ++ "`")

> duplicateLabel :: Ident -> (Position,String)
> duplicateLabel l =   
>     (positionOfIdent l,
>      "Multiple occurrence of record label `" ++ name l ++ "`")

> missingLabel :: Ident -> QualIdent -> String -> String 
> missingLabel l r what = 
>     "Missing label `" ++ name l 
>     ++ "` in the " ++ what ++ " of `" 
>     ++ name (unqualify r) ++ "`" --qualName r


> illegalLabel :: Ident -> QualIdent -> (Position,String)
> illegalLabel l r =   
>     (positionOfIdent l,
>      "Label `" ++ name l ++ "` is not defined in record `" 
>	     ++ name (unqualify r) ++ "`") --qualName r

> illegalRecordId :: Ident -> (Position,String)
> illegalRecordId r = 
>    (positionOfIdent r,
>     "Record identifier `" ++ name r 
>	      ++ "` already assigned to a data constructor")

> nonVariable :: String -> Ident -> (Position,String)
> nonVariable what c = 
>  (positionOfIdent c,     
>   "Data constructor `" ++ name c ++ "` in left hand side of " ++ what)

> noBody :: Ident -> (Position,String)
> noBody v = 
>  (positionOfIdent v,     
>   "No body for `" ++ name v ++ "`")

> noTypeSig :: Ident -> (Position,String)
> noTypeSig f = 
>  (positionOfIdent f,     
>   "No type signature for external function `" ++ name f ++ "`")

> noToplevelPattern :: String
> noToplevelPattern = "Pattern declaration not allowed at top-level"

> notALabel :: Ident -> (Position,String)
> notALabel l =  
>  (positionOfIdent l,     
>   "`" ++ name l ++ "` is not a record label")

> emptyRecord :: String
> emptyRecord = "empty records are not allowed"

> differentArity :: Ident -> (Position,String)
> differentArity f =   
>  (positionOfIdent f,     
>   "Equations for `" ++ name f ++ "` have different arities")

> wrongArity :: QualIdent -> Int -> Int -> (Position,String)
> wrongArity c arity argc =  
>  (positionOfQualIdent c,    
>   "Data constructor " ++ qualName c ++ " expects " ++ arguments arity ++
>   " but is applied to " ++ show argc)
>   where arguments 0 = "no arguments"
>         arguments 1 = "1 argument"
>         arguments n = show n ++ " arguments"

> illegalRecordPatt :: String
> illegalRecordPatt = "Expexting `_` after `|` in the record pattern"

> noFuncPattExt :: String
> noFuncPattExt = "function patterns are not supported in standard curry"
>	        ++ extMessage

> noRecordExt :: String
> noRecordExt = "records are not supported in standard curry"
>             ++ extMessage

> extMessage :: String
> extMessage = "\n(Use flag -e to enable extended curry)"

> infixWithoutParens :: [(QualIdent,QualIdent)] -> String
> infixWithoutParens calls =
>     "Missing parens in infix patterns: \n" ++
>     unlines (map (\(q1,q2) -> show q1 ++ " " ++ 
>                               showLine (positionOfQualIdent q1) ++ 
>                               "calls " ++ show q2 ++ " " ++ 
>                               showLine (positionOfQualIdent q2)) calls)

\end{verbatim}

checkParen 
@param Aufrufende InfixFunktion
@param ConstrTerm
@return Liste mit fehlerhaften Funktionsaufrufen
\begin{verbatim}

> checkParenConstrTerm :: (Maybe QualIdent) -> ConstrTerm -> [(QualIdent,QualIdent)]
> checkParenConstrTerm _ (LiteralPattern _) = []
> checkParenConstrTerm _ (NegativePattern _ _) = []
> checkParenConstrTerm _ (VariablePattern _) = []
> checkParenConstrTerm _ (ConstructorPattern qualIdent constrTerms) =
>     concatMap (checkParenConstrTerm Nothing) constrTerms
> checkParenConstrTerm mCaller (InfixPattern constrTerm1 qualIdent constrTerm2) =
>     maybe [] (\c -> [(c,qualIdent)]) mCaller ++
>     checkParenConstrTerm Nothing constrTerm1 ++
>     checkParenConstrTerm Nothing constrTerm2
> checkParenConstrTerm _ (ParenPattern constrTerm) =
>     checkParenConstrTerm Nothing constrTerm
> checkParenConstrTerm _ (TuplePattern _ constrTerms) =
>     concatMap (checkParenConstrTerm Nothing) constrTerms
> checkParenConstrTerm _ (ListPattern _ constrTerms) =
>     concatMap (checkParenConstrTerm Nothing) constrTerms
> checkParenConstrTerm mCaller (AsPattern _ constrTerm) =
>     checkParenConstrTerm mCaller constrTerm
> checkParenConstrTerm mCaller (LazyPattern _ constrTerm) =
>     checkParenConstrTerm mCaller constrTerm
> checkParenConstrTerm _ (FunctionPattern _ constrTerms) =
>     concatMap (checkParenConstrTerm Nothing) constrTerms
> checkParenConstrTerm mCaller (InfixFuncPattern constrTerm1 qualIdent constrTerm2) =
>     maybe [] (\c -> [(c,qualIdent)]) mCaller ++
>     checkParenConstrTerm Nothing constrTerm1 ++
>     checkParenConstrTerm Nothing constrTerm2
> checkParenConstrTerm _ (RecordPattern fieldConstrTerms mConstrTerm) =  
>     maybe [] (checkParenConstrTerm Nothing) mConstrTerm ++
>     concatMap (\(Field _ _ constrTerm) -> checkParenConstrTerm Nothing constrTerm) 
>               fieldConstrTerms

\end{verbatim}
