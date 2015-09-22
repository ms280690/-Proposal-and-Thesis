
% $Id: ILTrans.lhs,v 1.86 2004/02/13 19:23:58 wlux Exp $
%
% Copyright (c) 1999-2003, Wolfgang Lux
% See LICENSE for the full license.
%
% Modified by Martin Engelke (men@informatik.uni-kiel.de)
%
\nwfilename{ILTrans.lhs}
\section{Translating Curry into the Intermediate Language}
After desugaring and lifting have been performed, the source code is
translated into the intermediate language. Besides translating from
source terms and expressions into intermediate language terms and
expressions this phase in particular has to implement the pattern
matching algorithm for equations and case expressions.

Because of name conflicts between the source and intermediate language
data structures, we can use only a qualified import for the
\texttt{IL} module.
\begin{verbatim}

> module IL.CurryToIL(ilTrans,ilTransIntf, translType) where

> import Data.Maybe
> import Data.List
> import qualified Data.Set as Set
> import qualified Data.Map as Map

> import Curry.Base.Position
> import Curry.Base.Ident
> import Curry.Syntax
> import Curry.Syntax.Utils

> import Types
> import Base
> import qualified IL.Type as IL
> import Utils



\end{verbatim}
\paragraph{Modules}
At the top-level, the compiler has to translate data type, newtype,
function, and external declarations. When translating a data type or
newtype declaration, we ignore the types in the declaration and lookup
the types of the constructors in the type environment instead because
these types are already fully expanded, i.e., they do not include any
alias types.
\begin{verbatim}

> ilTrans :: Bool -> ValueEnv -> TCEnv -> EvalEnv -> Module -> IL.Module
> ilTrans flat tyEnv tcEnv evEnv (Module m _ ds) = 
>   IL.Module m (imports m ds') ds'
>   where ds' = concatMap (translGlobalDecl flat m tyEnv tcEnv evEnv) ds

> translGlobalDecl :: Bool -> ModuleIdent -> ValueEnv -> TCEnv -> EvalEnv
>                  -> Decl -> [IL.Decl]
> translGlobalDecl _ m tyEnv tcEnv _ (DataDecl _ tc tvs cs) =
>   [translData m tyEnv tcEnv tc tvs cs]
> translGlobalDecl _ m tyEnv tcEnv _ (NewtypeDecl _ tc tvs nc) =
>   [translNewtype m tyEnv tcEnv tc tvs nc]
> translGlobalDecl flat m tyEnv tcEnv evEnv (FunctionDecl pos f eqs) =
>   [translFunction pos flat m tyEnv tcEnv evEnv f eqs]
> translGlobalDecl _ m tyEnv tcEnv _ (ExternalDecl _ cc ie f _) =
>   [translExternal m tyEnv tcEnv f cc (fromJust ie)]
> translGlobalDecl _ _ _ _ _ _ = []

> translData :: ModuleIdent -> ValueEnv -> TCEnv -> Ident -> [Ident] -> [ConstrDecl]
>            -> IL.Decl
> translData m tyEnv tcEnv tc tvs cs =
>   IL.DataDecl (qualifyWith m tc) (length tvs)
>               (map (translConstrDecl m tyEnv tcEnv) cs)

> translNewtype :: ModuleIdent -> ValueEnv -> TCEnv -> Ident -> [Ident] 
>	        -> NewConstrDecl -> IL.Decl
> translNewtype m tyEnv tcEnv tc tvs (NewConstrDecl _ _ c _) =
>   IL.NewtypeDecl (qualifyWith m tc) (length tvs)
>                  (IL.ConstrDecl c' (translType' m tyEnv tcEnv ty))
>                  -- (IL.ConstrDecl c' (translType ty))
>   where c' = qualifyWith m c
>         TypeArrow ty _ = constrType tyEnv c'

> translConstrDecl :: ModuleIdent -> ValueEnv -> TCEnv -> ConstrDecl
>                  -> IL.ConstrDecl [IL.Type]
> translConstrDecl m tyEnv tcEnv d =
>   IL.ConstrDecl c' (map (translType' m tyEnv tcEnv)
>	                  (arrowArgs (constrType tyEnv c')))
>   -- IL.ConstrDecl c' (map translType (arrowArgs (constrType tyEnv c')))
>   where c' = qualifyWith m (constr d)
>         constr (ConstrDecl _ _ c _) = c
>         constr (ConOpDecl _ _ _ op _) = op

> translExternal :: ModuleIdent -> ValueEnv -> TCEnv -> Ident -> CallConv
>                -> String -> IL.Decl
> translExternal m tyEnv tcEnv f cc ie =
>   IL.ExternalDecl f' (callConv cc) ie 
>                   (translType' m tyEnv tcEnv (varType tyEnv f'))
>   -- IL.ExternalDecl f' (callConv cc) ie (translType (varType tyEnv f'))
>   where f' = qualifyWith m f
>         callConv CallConvPrimitive = IL.Primitive
>         callConv CallConvCCall = IL.CCall

\end{verbatim}
\paragraph{Interfaces}
In order to generate code, the compiler also needs to know the tags
and arities of all imported data constructors. For that reason we
compile the data type declarations of all interfaces into the
intermediate language, too. In this case we do not lookup the
types in the environment because the types in the interfaces are
already fully expanded. Note that we do not translate data types
which are imported into the interface from some other module.
\begin{verbatim}

> ilTransIntf :: ValueEnv -> TCEnv -> Interface -> [IL.Decl]
> ilTransIntf tyEnv tcEnv (Interface m ds) = 
>   foldr (translIntfDecl m tyEnv tcEnv) [] ds

> translIntfDecl :: ModuleIdent -> ValueEnv -> TCEnv -> IDecl -> [IL.Decl] 
>	         -> [IL.Decl]
> translIntfDecl m tyEnv tcEnv (IDataDecl _ tc tvs cs) ds
>   | not (isQualified tc) = 
>     translIntfData m tyEnv tcEnv (unqualify tc) tvs cs : ds
> translIntfDecl _ _ _ _ ds = ds

> translIntfData :: ModuleIdent -> ValueEnv -> TCEnv -> Ident -> [Ident] 
>	         -> [Maybe ConstrDecl] -> IL.Decl
> translIntfData m tyEnv tcEnv tc tvs cs =
>   IL.DataDecl (qualifyWith m tc) (length tvs)
>               (map (maybe hiddenConstr 
>	                    (translIntfConstrDecl m tyEnv tcEnv tvs)) cs)
>   where hiddenConstr = IL.ConstrDecl qAnonId []
>         qAnonId = qualify anonId

> translIntfConstrDecl :: ModuleIdent -> ValueEnv -> TCEnv -> [Ident] 
>                      -> ConstrDecl -> IL.ConstrDecl [IL.Type]
> translIntfConstrDecl m tyEnv tcEnv tvs (ConstrDecl _ _ c tys) =
>   IL.ConstrDecl (qualifyWith m c) (map (translType' m tyEnv tcEnv)
>			                 (toQualTypes m tvs tys))
>   -- IL.ConstrDecl (qualifyWith m c) (map translType (toQualTypes m tvs tys))
> translIntfConstrDecl m tyEnv tcEnv tvs (ConOpDecl _ _ ty1 op ty2) =
>   IL.ConstrDecl (qualifyWith m op)
>                 (map (translType' m tyEnv tcEnv)
>	               (toQualTypes m tvs [ty1,ty2]))
>   -- IL.ConstrDecl (qualifyWith m op)
>   --              (map translType (toQualTypes m tvs [ty1,ty2]))

\end{verbatim}
\paragraph{Types}
The type representation in the intermediate language is the same as
the internal representation except that it does not support
constrained type variables and skolem types. The former are fixed and
the later are replaced by fresh type constructors.

Due to possible occurrence of record types, it is necessary to transform
them back into their corresponding type constructors.
\begin{verbatim}

> translType' :: ModuleIdent -> ValueEnv -> TCEnv -> Type -> IL.Type
> translType' m tyEnv tcEnv ty =
>   translType (elimRecordTypes m tyEnv tcEnv (maximum (0:(typeVars ty))) ty)

> translType :: Type -> IL.Type
> translType (TypeConstructor tc tys) =
>   IL.TypeConstructor tc (map translType tys)
> translType (TypeVariable tv) = IL.TypeVariable tv
> translType (TypeConstrained tys _) = translType (head tys)
> translType (TypeArrow ty1 ty2) =
>   IL.TypeArrow (translType ty1) (translType ty2)
> translType (TypeSkolem k) =
>   IL.TypeConstructor (qualify (mkIdent ("_" ++ show k))) []

> elimRecordTypes :: ModuleIdent -> ValueEnv -> TCEnv -> Int -> Type -> Type
> elimRecordTypes m tyEnv tcEnv n (TypeConstructor t tys) =
>   TypeConstructor t (map (elimRecordTypes m tyEnv tcEnv n) tys)
> elimRecordTypes m tyEnv tcEnv n (TypeVariable v) =
>   TypeVariable v
> elimRecordTypes m tyEnv tcEnv n (TypeConstrained tys v) =
>   TypeConstrained (map (elimRecordTypes m tyEnv tcEnv n) tys) v
> elimRecordTypes m tyEnv tcEnv n (TypeArrow t1 t2) =
>   TypeArrow (elimRecordTypes m tyEnv tcEnv n t1)
>             (elimRecordTypes m tyEnv tcEnv n t2)
> elimRecordTypes m tyEnv tcEnv n (TypeSkolem v) =
>   TypeSkolem v
> elimRecordTypes m tyEnv tcEnv n (TypeRecord fs _)
>   | null fs = internalError "elimRecordTypes: empty record type"
>   | otherwise =
>     case (lookupValue (fst (head fs)) tyEnv) of
>       [Label _ r _] ->
>         case (qualLookupTC r tcEnv) of
>           [AliasType _ n' (TypeRecord fs' _)] ->
>	      let is = [0 .. n'-1]
>                 vs = foldl (matchTypeVars fs)
>			     Map.empty
>			     fs'
>		  tys = map (\i -> maybe (TypeVariable (i+n))
>			                 (elimRecordTypes m tyEnv tcEnv n)
>		                         (Map.lookup i vs))
>		            is 
>	      in  TypeConstructor r tys
>	    _ -> internalError "elimRecordTypes: no record type"
>       _ -> internalError "elimRecordTypes: no label"

> matchTypeVars :: [(Ident,Type)] -> Map.Map Int Type -> (Ident,Type) 
>	           -> Map.Map Int Type
> matchTypeVars fs vs (l,ty) =
>   maybe vs (match vs ty) (lookup l fs)
>   where
>   match vs (TypeVariable i) ty' = Map.insert i ty' vs
>   match vs (TypeConstructor _ tys) (TypeConstructor _ tys') =
>     matchList vs tys tys'
>   match vs (TypeConstrained tys _) (TypeConstrained tys' _) =
>     matchList vs tys tys'
>   match vs (TypeArrow ty1 ty2) (TypeArrow ty1' ty2') =
>     matchList vs [ty1,ty2] [ty1',ty2']
>   match vs (TypeSkolem _) (TypeSkolem _) = vs
>   match vs (TypeRecord fs _) (TypeRecord fs' _) =
>     foldl (matchTypeVars fs') vs fs
>   match vs ty ty' = 
>     internalError ("matchTypeVars: " ++ show ty ++ "\n" ++ show ty')
>
>   matchList vs tys tys' = 
>     foldl (\vs' (ty,ty') -> match vs' ty ty') vs (zip tys tys')

\end{verbatim}
\paragraph{Functions}
Each function in the program is translated into a function of the
intermediate language. The arguments of the function are renamed such
that all variables occurring in the same position (in different
equations) have the same name. This is necessary in order to
facilitate the translation of pattern matching into a \texttt{case}
expression. We use the following simple convention here: The top-level
arguments of the function are named from left to right \texttt{\_1},
\texttt{\_2}, and so on. The names of nested arguments are constructed
by appending \texttt{\_1}, \texttt{\_2}, etc. from left to right to
the name that were assigned to a variable occurring at the position of
the constructor term.

Some special care is needed for the selector functions introduced by
the compiler in place of pattern bindings. In order to generate the
code for updating all pattern variables, the equality of names between
the pattern variables in the first argument of the selector function
and their repeated occurrences in the remaining arguments must be
preserved. This means that the second and following arguments of a
selector function have to be renamed according to the name mapping
computed for its first argument.

If an evaluation annotation is available for a function, it determines
the evaluation mode of the case expression. Otherwise, the function
uses flexible matching.
\begin{verbatim}

> type RenameEnv = Map.Map Ident Ident

> translFunction :: Position -> Bool -> ModuleIdent -> ValueEnv -> TCEnv
>       -> EvalEnv -> Ident -> [Equation] -> IL.Decl
> translFunction pos flat m tyEnv tcEnv evEnv f eqs =
>   -- - | f == mkIdent "fun" = error (show (translType' m tyEnv tcEnv ty))
>   -- - | otherwise = 
>     IL.FunctionDecl f' vs (translType' m tyEnv tcEnv ty) expr
>    -- = IL.FunctionDecl f' vs (translType ty)
>    --                  (match ev vs (map (translEquation tyEnv vs vs'') eqs))
>   where f'  = qualifyWith m f
>         ty  = varType tyEnv f'
>         -- ty' = elimRecordType m tyEnv tcEnv (maximum (0:(typeVars ty))) ty
>         ev' = Map.lookup f evEnv
>         ev  = maybe (defaultMode ty) evalMode ev'
>         vs  = if not flat && isFpSelectorId f then translArgs eqs vs' else vs'
>         (vs',vs'') = splitAt (equationArity (head eqs)) 
>                              (argNames (mkIdent ""))
>         expr | ev' == Just EvalChoice
>                = IL.Apply 
>                    (IL.Function 
>                       (qualifyWith preludeMIdent (mkIdent "commit"))
>                       1)
>                    (match (srcRefOf pos) IL.Rigid vs 
>                       (map (translEquation tyEnv vs vs'') eqs))
>              | otherwise
>                =  match (srcRefOf pos) ev vs (map (translEquation tyEnv vs vs'') eqs)
>         ---
>         -- (vs',vs'') = splitAt (arrowArity ty) (argNames (mkIdent ""))

> evalMode :: EvalAnnotation -> IL.Eval
> evalMode EvalRigid = IL.Rigid
> evalMode EvalChoice = error "eval choice is not yet supported"

> defaultMode :: Type -> IL.Eval
> defaultMode _ = IL.Flex
>
> --defaultMode ty = if isIO (arrowBase ty) then IL.Rigid else IL.Flex
> --  where TypeConstructor qIOId _ = ioType undefined
> --        isIO (TypeConstructor tc [_]) = tc == qIOId
> --        isIO _ = False

> translArgs :: [Equation] -> [Ident] -> [Ident]
> translArgs [Equation _ (FunLhs _ (t:ts)) _] (v:_) =
>   v : map (translArg (bindRenameEnv v t Map.empty)) ts
>   where translArg env (VariablePattern v) = fromJust (Map.lookup v env)

> translEquation :: ValueEnv -> [Ident] -> [Ident] -> Equation
>                -> ([NestedTerm],IL.Expression)
> translEquation tyEnv vs vs' (Equation _ (FunLhs _ ts) rhs) =
>   (zipWith translTerm vs ts,
>    translRhs tyEnv vs' (foldr2 bindRenameEnv Map.empty vs ts) rhs)

> translRhs :: ValueEnv -> [Ident] -> RenameEnv -> Rhs -> IL.Expression
> translRhs tyEnv vs env (SimpleRhs _ e _) = translExpr tyEnv vs env e


> equationArity :: Equation -> Int
> equationArity (Equation _ lhs _) = p_equArity lhs
>  where
>    p_equArity (FunLhs _ ts) = length ts
>    p_equArity (OpLhs _ _ _) = 2
>    p_equArity _             = error "ILTrans - illegal equation"


\end{verbatim}
\paragraph{Pattern Matching}
The pattern matching code searches for the left-most inductive
argument position in the left hand sides of all rules defining an
equation. An inductive position is a position where all rules have a
constructor rooted term. If such a position is found, a \texttt{case}
expression is generated for the argument at that position. The
matching code is then computed recursively for all of the alternatives
independently. If no inductive position is found, the algorithm looks
for the left-most demanded argument position, i.e., a position where
at least one of the rules has a constructor rooted term. If such a
position is found, an \texttt{or} expression is generated with those
cases that have a variable at the argument position in one branch and
all other rules in the other branch. If there is no demanded position,
the pattern matching is finished and the compiler translates the right
hand sides of the remaining rules, eventually combining them using
\texttt{or} expressions.

Actually, the algorithm below combines the search for inductive and
demanded positions. The function \texttt{match} scans the argument
lists for the left-most demanded position. If this turns out to be
also an inductive position, the function \texttt{matchInductive} is
called in order to generate a \texttt{case} expression. Otherwise, the
function \texttt{optMatch} is called that tries to find an inductive
position in the remaining arguments. If one is found,
\texttt{matchInductive} is called, otherwise the function
\texttt{optMatch} uses the demanded argument position found by
\texttt{match}.
\begin{verbatim}

> data NestedTerm = NestedTerm IL.ConstrTerm [NestedTerm] deriving Show

> pattern (NestedTerm t _) = t
> arguments (NestedTerm _ ts) = ts

> translLiteral :: Literal -> IL.Literal
> translLiteral (Char p c) = IL.Char p c
> translLiteral (Int id i) = IL.Int (srcRefOf (positionOfIdent id)) i
> translLiteral (Float p f) = IL.Float p f
> translLiteral _ = internalError "translLiteral"

> translTerm :: Ident -> ConstrTerm -> NestedTerm
> translTerm _ (LiteralPattern l) =
>   NestedTerm (IL.LiteralPattern (translLiteral l)) []
> translTerm v (VariablePattern _) = NestedTerm (IL.VariablePattern v) []
> translTerm v (ConstructorPattern c ts) =
>   NestedTerm (IL.ConstructorPattern c (take (length ts) vs))
>              (zipWith translTerm vs ts)
>   where vs = argNames v
> translTerm v (AsPattern _ t) = translTerm v t
> translTerm _ _ = internalError "translTerm"

> bindRenameEnv :: Ident -> ConstrTerm -> RenameEnv -> RenameEnv
> bindRenameEnv _ (LiteralPattern _) env = env
> bindRenameEnv v (VariablePattern v') env = Map.insert v' v env
> bindRenameEnv v (ConstructorPattern _ ts) env =
>   foldr2 bindRenameEnv env (argNames v) ts
> bindRenameEnv v (AsPattern v' t) env = Map.insert v' v (bindRenameEnv v t env)
> bindRenameEnv _ _ env = internalError "bindRenameEnv"

> argNames :: Ident -> [Ident]
> argNames v = [mkIdent (prefix ++ show i) | i <- [1..]]
>   where prefix = name v ++ "_"

> type Match = ([NestedTerm],IL.Expression)
> type Match' = ([NestedTerm] -> [NestedTerm],[NestedTerm],IL.Expression)

> isDefaultPattern :: IL.ConstrTerm -> Bool
> isDefaultPattern (IL.VariablePattern _) = True
> isDefaultPattern _ = False

> isDefaultMatch :: (IL.ConstrTerm,a) -> Bool
> isDefaultMatch = isDefaultPattern . fst

> match :: SrcRef -> IL.Eval -> [Ident] -> [Match] -> IL.Expression
> match _   ev [] alts = foldl1 IL.Or (map snd alts)
> match pos ev (v:vs) alts
>   | null vars = e1
>   | null nonVars = e2
>   | otherwise = optMatch pos ev (IL.Or e1 e2) (v:) vs (map skipArg alts)
>   where (vars,nonVars) = partition isDefaultMatch (map tagAlt alts)
>         e1 = matchInductive pos ev id v vs nonVars
>         e2 = match pos ev vs (map snd vars)
>         tagAlt (t:ts,e) = (pattern t,(arguments t ++ ts,e))
>         skipArg (t:ts,e) = ((t:),ts,e)

> optMatch :: SrcRef -> IL.Eval -> IL.Expression -> ([Ident] -> [Ident]) 
>    -> [Ident] ->[Match'] -> IL.Expression
> optMatch _ ev e prefix [] alts = e
> optMatch pos ev e prefix (v:vs) alts
>   | null vars = matchInductive pos ev prefix v vs nonVars
>   | otherwise = optMatch pos ev e (prefix . (v:)) vs (map skipArg alts)
>   where (vars,nonVars) = partition isDefaultMatch (map tagAlt alts)
>         tagAlt (prefix,t:ts,e) = (pattern t,(prefix (arguments t ++ ts),e))
>         skipArg (prefix,t:ts,e) = (prefix . (t:),ts,e)

> matchInductive :: SrcRef -> IL.Eval -> ([Ident] -> [Ident]) -> Ident 
>    -> [Ident] ->[(IL.ConstrTerm,Match)] -> IL.Expression
> matchInductive pos ev prefix v vs alts =
>   IL.Case pos ev (IL.Variable v) (matchAlts ev prefix vs alts)

> matchAlts :: IL.Eval -> ([Ident] -> [Ident]) -> [Ident] ->
>     [(IL.ConstrTerm,Match)] -> [IL.Alt]
> matchAlts ev prefix vs [] = []
> matchAlts ev prefix vs ((t,alt):alts) =
>   IL.Alt t (match (srcRefOf t) 
>                   ev (prefix (vars t ++ vs)) (alt : map snd same)) :
>   matchAlts ev prefix vs others
>   where (same,others) = partition ((t ==) . fst) alts 
>         vars (IL.ConstructorPattern _ vs) = vs
>         vars _ = []

\end{verbatim}
Matching in a \texttt{case}-expression works a little bit differently.
In this case, the alternatives are matched from the first to the last
alternative and the first matching alternative is chosen. All
remaining alternatives are discarded.

\ToDo{The case matching algorithm should use type information in order
to detect total matches and immediately discard all alternatives which
cannot be reached.}
\begin{verbatim}

> caseMatch :: SrcRef -> ([Ident] -> [Ident]) -> [Ident] -> [Match'] 
>    -> IL.Expression
> caseMatch _ prefix [] alts = thd3 (head alts)
> caseMatch r prefix (v:vs) alts
>   | isDefaultMatch (head alts') =
>       caseMatch r (prefix . (v:)) vs (map skipArg alts)
>   | otherwise =
>       IL.Case r IL.Rigid (IL.Variable v) (caseMatchAlts prefix vs alts')
>   where alts' = map tagAlt alts
>         tagAlt (prefix,t:ts,e) = (pattern t,(prefix,arguments t ++ ts,e))
>         skipArg (prefix,t:ts,e) = (prefix . (t:),ts,e)

> caseMatchAlts ::
>     ([Ident] -> [Ident]) -> [Ident] -> [(IL.ConstrTerm,Match')] -> [IL.Alt]
> caseMatchAlts prefix vs alts = map caseAlt (ts ++ ts')
>   where (ts',ts) = partition isDefaultPattern (nub (map fst alts))
>         caseAlt t =
>           IL.Alt t (caseMatch (srcRefOf t) id (prefix (vars t ++ vs))
>                               (matchingCases t alts))
>         matchingCases t =
>           map (joinArgs (vars t)) . filter (matches t . fst)
>         matches t t' = t == t' || isDefaultPattern t'
>         joinArgs vs (IL.VariablePattern _,(prefix,ts,e)) =
>            (id,prefix (map varPattern vs ++ ts),e)
>         joinArgs _ (_,(prefix,ts,e)) = (id,prefix ts,e)
>         varPattern v = NestedTerm (IL.VariablePattern v) []
>         vars (IL.ConstructorPattern _ vs) = vs
>         vars _ = []

\end{verbatim}
\paragraph{Expressions}
Note that the case matching algorithm assumes that the matched
expression is accessible through a variable. The translation of case
expressions therefore introduces a let binding for the scrutinized
expression and immediately throws it away after the matching -- except
if the matching algorithm has decided to use that variable in the
right hand sides of the case expression. This may happen, for
instance, if one of the alternatives contains an \texttt{@}-pattern.
\begin{verbatim}

> translExpr :: ValueEnv -> [Ident] -> RenameEnv -> Expression -> IL.Expression
> translExpr _ _ _ (Literal l) = IL.Literal (translLiteral l)
> translExpr tyEnv _ env (Variable v) =
>   case lookupVar v env of
>     Just v' -> IL.Variable v'
>     Nothing -> IL.Function v (arrowArity (varType tyEnv v))
>   where lookupVar v env
>           | isQualified v = Nothing
>           | otherwise = Map.lookup (unqualify v) env
> translExpr tyEnv _ _ (Constructor c) =
>   IL.Constructor c (arrowArity (constrType tyEnv c))
> translExpr tyEnv vs env (Apply e1 e2) =
>   IL.Apply (translExpr tyEnv vs env e1) (translExpr tyEnv vs env e2)
> translExpr tyEnv vs env (Let ds e) =
>   case ds of
>     [ExtraVariables _ vs] -> foldr IL.Exist e' vs
>     [d] | all (`notElem` bv d) (qfv emptyMIdent d) ->
>       IL.Let (translBinding env' d) e'
>     _ -> IL.Letrec (map (translBinding env') ds) e'
>   where e' = translExpr tyEnv vs env' e
>         env' = foldr2 Map.insert env bvs bvs
>         bvs = bv ds
>         translBinding env (PatternDecl _ (VariablePattern v) rhs) =
>           IL.Binding v (translRhs tyEnv vs env rhs)
>         translBinding env p = error $ "unexpected binding: "++show p
> translExpr tyEnv ~(v:vs) env (Case r e alts) =
>   case caseMatch r id [v] (map (translAlt v) alts) of
>     IL.Case r mode (IL.Variable v') alts'
>       | v == v' && v `notElem` fv alts' -> IL.Case r mode e' alts'
>     e''
>       | v `elem` fv e'' -> IL.Let (IL.Binding v e') e''
>       | otherwise -> e''
>   where e' = translExpr tyEnv vs env e
>         translAlt v (Alt _ t rhs) =
>           (id,
>            [translTerm v t],
>            translRhs tyEnv vs (bindRenameEnv v t env) rhs)
> translExpr _ _ _ _ = internalError "translExpr"

> instance Expr IL.Expression where
>   fv (IL.Variable v) = [v]
>   fv (IL.Apply e1 e2) = fv e1 ++ fv e2
>   fv (IL.Case _ _ e alts) = fv e ++ fv alts
>   fv (IL.Or e1 e2) = fv e1 ++ fv e2
>   fv (IL.Exist v e) = filter (/= v) (fv e)
>   fv (IL.Let (IL.Binding v e1) e2) = fv e1 ++ filter (/= v) (fv e2)
>   fv (IL.Letrec bds e) = filter (`notElem` vs) (fv es ++ fv e)
>     where (vs,es) = unzip [(v,e) | IL.Binding v e <- bds]
>   fv _ = []

> instance Expr IL.Alt where
>   fv (IL.Alt (IL.ConstructorPattern _ vs) e) = filter (`notElem` vs) (fv e)
>   fv (IL.Alt (IL.VariablePattern v) e) = filter (v /=) (fv e)
>   fv (IL.Alt _ e) = fv e

\end{verbatim}
\paragraph{Auxiliary Definitions}
The functions \texttt{varType} and \texttt{constrType} return the type
of variables and constructors, respectively. The quantifiers are
stripped from the types.
\begin{verbatim}

> varType :: ValueEnv -> QualIdent -> Type
> varType tyEnv f =
>   case qualLookupValue f tyEnv of
>     [Value _ (ForAll _ ty)] -> ty
>     _ -> internalError ("varType: " ++ show f)

> constrType :: ValueEnv -> QualIdent -> Type
> constrType tyEnv c =
>   case qualLookupValue c tyEnv of
>     [DataConstructor _ (ForAllExist _ _ ty)] -> ty
>     [NewtypeConstructor _ (ForAllExist _ _ ty)] -> ty
>     _ -> internalError ("constrType: " ++ show c)

\end{verbatim}
The list of import declarations in the intermediate language code is
determined by collecting all module qualifiers used in the current
module.
\begin{verbatim}

> imports :: ModuleIdent -> [IL.Decl] -> [ModuleIdent]
> imports m = Set.toList . Set.delete m . Set.fromList . foldr modulesDecl []

> modulesDecl :: IL.Decl -> [ModuleIdent] -> [ModuleIdent]
> modulesDecl (IL.DataDecl _ _ cs) ms = foldr modulesConstrDecl ms cs
>   where modulesConstrDecl (IL.ConstrDecl _ tys) ms = foldr modulesType ms tys
> modulesDecl (IL.NewtypeDecl _ _ (IL.ConstrDecl _ ty)) ms = modulesType ty ms
> modulesDecl (IL.FunctionDecl _ _ ty e) ms = modulesType ty (modulesExpr e ms)
> modulesDecl (IL.ExternalDecl _ _ _ ty) ms = modulesType ty ms

> modulesType :: IL.Type -> [ModuleIdent] -> [ModuleIdent]
> modulesType (IL.TypeConstructor tc tys) ms =
>   modules tc (foldr modulesType ms tys)
> modulesType (IL.TypeVariable _) ms = ms
> modulesType (IL.TypeArrow ty1 ty2) ms = modulesType ty1 (modulesType ty2 ms)

> modulesExpr :: IL.Expression -> [ModuleIdent] -> [ModuleIdent]
> modulesExpr (IL.Function f _) ms = modules f ms
> modulesExpr (IL.Constructor c _) ms = modules c ms
> modulesExpr (IL.Apply e1 e2) ms = modulesExpr e1 (modulesExpr e2 ms)
> modulesExpr (IL.Case _ _ e as) ms = modulesExpr e (foldr modulesAlt ms as)
>   where modulesAlt (IL.Alt t e) ms = modulesConstrTerm t (modulesExpr e ms)
>         modulesConstrTerm (IL.ConstructorPattern c _) ms = modules c ms
>         modulesConstrTerm _ ms = ms
> modulesExpr (IL.Or e1 e2) ms = modulesExpr e1 (modulesExpr e2 ms)
> modulesExpr (IL.Exist _ e) ms = modulesExpr e ms
> modulesExpr (IL.Let b e) ms = modulesBinding b (modulesExpr e ms)
> modulesExpr (IL.Letrec bs e) ms = foldr modulesBinding (modulesExpr e ms) bs
> modulesExpr _ ms = ms

> modulesBinding :: IL.Binding -> [ModuleIdent] -> [ModuleIdent]
> modulesBinding (IL.Binding _ e) = modulesExpr e

> modules :: QualIdent -> [ModuleIdent] -> [ModuleIdent]
> modules x ms = maybe ms (: ms) (qualidMod x)

\end{verbatim}

