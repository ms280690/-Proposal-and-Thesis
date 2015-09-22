% $Id: Base.lhs,v 1.77 2004/02/15 22:10:25 wlux Exp $
%
% Copyright (c) 1999-2004, Wolfgang Lux
% See LICENSE for the full license.
%
% Modified by Martin Engelke (men@informatik.uni-kiel.de)
%
\nwfilename{Base.lhs}
\section{Common Definitions for the Compiler}

The module Base implements the anti-pattern 'God-object'.
By providing definitions for various unrelated phases of the
compiler, it irrevocably turns the module structure into spaghetti.
(hsi, 2009)

\begin{verbatim}

> module Base where

> import Data.List
> import Control.Monad
> import Data.Maybe
> import qualified Data.Map as Map

> import Curry.Base.Ident 
> import Curry.Base.Position
> import Types
> import qualified Curry.Syntax as CS
> import Curry.Syntax.Utils
> import TopEnv
> import Utils



\end{verbatim}
\paragraph{Types}
The functions \texttt{toType}, \texttt{toTypes}, and \texttt{fromType}
convert Curry type expressions into types and vice versa. The
functions \texttt{qualifyType} and \texttt{unqualifyType} add and
remove module qualifiers in a type, respectively.

When Curry type expression are converted with \texttt{toType} or
\texttt{toTypes}, type variables are assigned ascending indices in the
order of their occurrence. It is possible to pass a list of additional
type variables to both functions which are assigned indices before
those variables occurring in the type. This allows preserving the
order of type variables in the left hand side of a type declaration.
\begin{verbatim}

> toQualType :: ModuleIdent -> [Ident] -> CS.TypeExpr -> Type
> toQualType m tvs = qualifyType m . toType tvs

> toQualTypes :: ModuleIdent -> [Ident] -> [CS.TypeExpr] -> [Type]
> toQualTypes m tvs = map (qualifyType m) . toTypes tvs

> toType :: [Ident] -> CS.TypeExpr -> Type
> toType tvs ty = toType' (Map.fromList (zip (tvs ++ tvs') [0..])) ty
>   where tvs' = [tv | tv <- nub (fv ty), tv `notElem` tvs]

> toTypes :: [Ident] -> [CS.TypeExpr] -> [Type]
> toTypes tvs tys = map (toType' (Map.fromList (zip (tvs ++ tvs') [0..]))) tys
>   where tvs' = [tv | tv <- nub (concatMap fv tys), tv `notElem` tvs]

> toType' :: Map.Map Ident Int -> CS.TypeExpr -> Type
> toType' tvs (CS.ConstructorType tc tys) =
>   TypeConstructor tc (map (toType' tvs) tys)
> toType' tvs (CS.VariableType tv) =
>   maybe (internalError ("toType " ++ show tv)) TypeVariable (Map.lookup tv tvs)
> toType' tvs (CS.TupleType tys)
>   | null tys = TypeConstructor (qualify unitId) []
>   | otherwise = TypeConstructor (qualify (tupleId (length tys'))) tys'
>   where tys' = map (toType' tvs) tys
> toType' tvs (CS.ListType ty) = TypeConstructor (qualify listId) [toType' tvs ty]
> toType' tvs (CS.ArrowType ty1 ty2) =
>   TypeArrow (toType' tvs ty1) (toType' tvs ty2)
> toType' tvs (CS.RecordType fs rty) =
>   TypeRecord (concatMap (\ (ls,ty) -> map (\l -> (l, toType' tvs ty)) ls) fs)
>              (maybe Nothing 
>	              (\ty -> case toType' tvs ty of
>	                        TypeVariable tv -> Just tv 
>	                        _ -> internalError ("toType " ++ show ty))
>	              rty)

> fromQualType :: ModuleIdent -> Type -> CS.TypeExpr
> fromQualType m = fromType . unqualifyType m

> fromType :: Type -> CS.TypeExpr
> fromType (TypeConstructor tc tys)
>   | isTupleId c = CS.TupleType tys'
>   | c == listId && length tys == 1 = CS.ListType (head tys')
>   | c == unitId && null tys = CS.TupleType []
>   | otherwise = CS.ConstructorType tc tys'
>   where c = unqualify tc
>         tys' = map fromType tys
> fromType (TypeVariable tv) =
>   CS.VariableType (if tv >= 0 then nameSupply !! tv
>                            else mkIdent ('_' : show (-tv)))
> fromType (TypeConstrained tys _) = fromType (head tys)
> fromType (TypeArrow ty1 ty2) = CS.ArrowType (fromType ty1) (fromType ty2)
> fromType (TypeSkolem k) = CS.VariableType (mkIdent ("_?" ++ show k))
> fromType (TypeRecord fs rty) = 
>   CS.RecordType (map (\ (l,ty) -> ([l], fromType ty)) fs)
>              (maybe Nothing (Just . fromType . TypeVariable) rty)



\end{verbatim}
\paragraph{Interfaces}
The compiler maintains a global environment holding all (directly or
indirectly) imported interfaces.

The function \texttt{bindFlatInterface} transforms FlatInterface
information (type \texttt{FlatCurry.Prog} to MCC interface declarations
(type \texttt{CurrySyntax.IDecl}. This is necessary to process
FlatInterfaces instead of ".icurry" files when using MCC as frontend
for PAKCS.
\begin{verbatim}

> type ModuleEnv = Map.Map ModuleIdent [CS.IDecl]

> lookupModule :: ModuleIdent -> ModuleEnv -> Maybe [CS.IDecl]
> lookupModule = Map.lookup


\end{verbatim}
\paragraph{Type constructors}
For all defined types the compiler must maintain kind information. At
present, Curry does not support type classes. Therefore its type
language is first order and the only information that must be recorded
is the arity of each type. For algebraic data types and renaming types
the compiler also records all data constructors belonging to that
type, for alias types the type expression to be expanded is saved. In
order to manage the import and export of types, the names of the
original definitions are also recorded. On import two types are
considered equal if their original names match.

The information for a data constructor comprises the number of
existentially quantified type variables and the list of the argument
types. Note that renaming type constructors have only one type
argument.

Importing and exporting algebraic data types and renaming types is
complicated by the fact that the constructors of the type may be
(partially) hidden in the interface. This facilitates the definition
of abstract data types. An abstract type is always represented as a
data type without constructors in the interface regardless of whether
it is defined as a data type or as a renaming type. When only some
constructors of a data type are hidden, those constructors are
replaced by underscores in the interface. Furthermore, if the
right-most constructors of a data type are hidden, they are not
exported at all in order to make the interface more stable against
changes which are private to the module.
\begin{verbatim}

> data TypeInfo = DataType QualIdent Int [Maybe (Data [Type])]
>               | RenamingType QualIdent Int (Data Type)
>               | AliasType QualIdent Int Type
>               deriving Show

> data Data a = Data Ident Int a deriving Show

> instance Entity TypeInfo where
>   origName (DataType tc _ _) = tc
>   origName (RenamingType tc _ _) = tc
>   origName (AliasType tc _ _) = tc
>   merge (DataType tc n cs) (DataType tc' _ cs')
>     | tc == tc' = Just (DataType tc n (mergeData cs cs'))
>     where mergeData ds [] = ds
>           mergeData [] ds = ds
>           mergeData (d:ds) (d':ds') = d `mplus` d' : mergeData ds ds'
>   merge (DataType tc n _) (RenamingType tc' _ nc)
>     | tc == tc' = Just (RenamingType tc n nc)
>   merge (RenamingType tc n nc) (DataType tc' _ _)
>     | tc == tc' = Just (RenamingType tc n nc)
>   merge (RenamingType tc n nc) (RenamingType tc' _ _)
>     | tc == tc' = Just (RenamingType tc n nc)
>   merge (AliasType tc n ty) (AliasType tc' _ _)
>     | tc == tc' = Just (AliasType tc n ty)
>   merge _ _ = Nothing

> tcArity :: TypeInfo -> Int
> tcArity (DataType _ n _) = n
> tcArity (RenamingType _ n _) = n
> tcArity (AliasType _ n _) = n

\end{verbatim}
Types can only be defined on the top-level; no nested environments are
needed for them. Tuple types must be handled as a special case because
there is an infinite number of potential tuple types making it
impossible to insert them into the environment in advance.
\begin{verbatim}

> type TCEnv = TopEnv TypeInfo

> bindTypeInfo :: (QualIdent -> Int -> a -> TypeInfo) -> ModuleIdent
>              -> Ident -> [Ident] -> a -> TCEnv -> TCEnv
> bindTypeInfo f m tc tvs x 
>   = bindTopEnv "Base.bindTypeInfo" tc t 
>     . qualBindTopEnv "Base.bindTypeInfo" tc' t
>   where tc' = qualifyWith m tc
>         t = f tc' (length tvs) x

> lookupTC :: Ident -> TCEnv -> [TypeInfo]
> lookupTC tc tcEnv = lookupTopEnv tc tcEnv ++! lookupTupleTC tc

> qualLookupTC :: QualIdent -> TCEnv -> [TypeInfo]
> qualLookupTC tc tcEnv =
>   qualLookupTopEnv tc tcEnv ++! lookupTupleTC (unqualify tc)

> lookupTupleTC :: Ident -> [TypeInfo]
> lookupTupleTC tc
>   | isTupleId tc = [tupleTCs !! (tupleArity tc - 2)]
>   | otherwise = []

> tupleTCs :: [TypeInfo]
> tupleTCs = map typeInfo tupleData
>   where typeInfo (Data c _ tys) =
>           DataType (qualifyWith preludeMIdent c) (length tys)
>                    [Just (Data c 0 tys)]

> tupleData :: [Data [Type]]
> tupleData = [Data (tupleId n) 0 (take n tvs) | n <- [2..]]
>   where tvs = map typeVar [0..]

\end{verbatim}
\paragraph{Function and constructor types}
In order to test the type correctness of a module, the compiler needs
to determine the type of every data constructor, function,
variable, record and label in the module. 
For the purpose of type checking there is no
need for distinguishing between variables and functions. For all objects
their original names and their types are saved. Functions also
contain arity information. Labels currently contain the name of their
defining record. On import two values
are considered equal if their original names match.
\begin{verbatim}

> data ValueInfo = DataConstructor QualIdent ExistTypeScheme
>                | NewtypeConstructor QualIdent ExistTypeScheme
>                | Value QualIdent TypeScheme
>	         | Label QualIdent QualIdent TypeScheme
>	           -- Label <label> <record name> <type>
>                deriving Show

> instance Entity ValueInfo where
>   origName (DataConstructor origName _) = origName
>   origName (NewtypeConstructor origName _) = origName
>   origName (Value origName _) = origName
>   origName (Label origName _ _) = origName
>   
>   merge (Label l r ty) (Label l' r' ty')
>     | l == l' && r == r' = Just (Label l r ty)
>     | otherwise = Nothing
>   merge x y
>     | origName x == origName y = Just x
>     | otherwise = Nothing


\end{verbatim}
Even though value declarations may be nested, the compiler uses only
flat environments for saving type information. This is possible
because all identifiers are renamed by the compiler. Here we need
special cases for handling tuple constructors.

\em{Note:} the function \texttt{qualLookupValue} has been extended to
allow the usage of the qualified list constructor \texttt{(Prelude.:)}.
\begin{verbatim}

> type ValueEnv = TopEnv ValueInfo

> bindGlobalInfo :: (QualIdent -> a -> ValueInfo) -> ModuleIdent -> Ident -> a
>                -> ValueEnv -> ValueEnv
> bindGlobalInfo f m c ty 
>   = bindTopEnv "Base.bindGlobalInfo" c v 
>     . qualBindTopEnv "Base.bindGlobalInfo" c' v
>   where c' = qualifyWith m c
>         v = f c' ty

> bindFun :: ModuleIdent -> Ident -> TypeScheme -> ValueEnv -> ValueEnv
> bindFun m f ty tyEnv
>   | uniqueId f == 0 
>     = bindTopEnv "Base.bindFun" f v (qualBindTopEnv "Base.bindFun" f' v tyEnv)
>   | otherwise = bindTopEnv "Base.bindFun" f v tyEnv
>   where f' = qualifyWith m f
>         v = Value f' ty

> rebindFun :: ModuleIdent -> Ident -> TypeScheme -> ValueEnv -> ValueEnv
> rebindFun m f ty
>   | uniqueId f == 0 = rebindTopEnv f v . qualRebindTopEnv f' v
>   | otherwise = rebindTopEnv f v
>   where f' = qualifyWith m f
>         v = Value f' ty

> bindLabel :: Ident -> QualIdent -> TypeScheme -> ValueEnv -> ValueEnv
> bindLabel l r ty tyEnv = bindTopEnv "Base.bindLabel" l v tyEnv
>   where v  = Label (qualify l) r ty

> lookupValue :: Ident -> ValueEnv -> [ValueInfo]
> lookupValue x tyEnv = lookupTopEnv x tyEnv ++! lookupTuple x

> qualLookupValue :: QualIdent -> ValueEnv -> [ValueInfo]
> qualLookupValue x tyEnv =
>   qualLookupTopEnv x tyEnv 
>   ++! qualLookupCons x tyEnv
>   ++! lookupTuple (unqualify x)

> qualLookupCons :: QualIdent -> ValueEnv -> [ValueInfo]
> qualLookupCons x tyEnv
>    | maybe False ((==) preludeMIdent) mmid && id == consId
>       = qualLookupTopEnv (qualify id) tyEnv
>    | otherwise = []
>  where (mmid, id) = (qualidMod x, qualidId x)

> lookupTuple :: Ident -> [ValueInfo]
> lookupTuple c
>   | isTupleId c = [tupleDCs !! (tupleArity c - 2)]
>   | otherwise = []

> tupleDCs :: [ValueInfo]
> tupleDCs = map dataInfo tupleTCs
>   where dataInfo (DataType tc tvs [Just (Data c _ tys)]) =
>           DataConstructor (qualUnqualify preludeMIdent tc)
>                           (ForAllExist (length tys) 0
>                                        (foldr TypeArrow (tupleType tys) tys))

\end{verbatim}
\paragraph{Arity}
In order to generate correct FlatCurry application it is necessary
to define the number of arguments as the arity value (instead of
using the arity computed from the type). For this reason the compiler
needs a table containing the information for all known functions
and constructors. 
\begin{verbatim}

> type ArityEnv = TopEnv ArityInfo

> data ArityInfo = ArityInfo QualIdent Int deriving Show

> instance Entity ArityInfo where
>   origName (ArityInfo origName _) = origName

> bindArity :: ModuleIdent -> Ident -> Int -> ArityEnv -> ArityEnv
> bindArity mid id arity aEnv
>    | uniqueId id == 0 
>      = bindTopEnv "Base.bindArity" id arityInfo 
>                   (qualBindTopEnv "Base.bindArity" qid arityInfo aEnv)
>    | otherwise        
>      = bindTopEnv "Base.bindArity" id arityInfo aEnv
>  where
>  qid = qualifyWith mid id
>  arityInfo = ArityInfo qid arity

> lookupArity :: Ident -> ArityEnv -> [ArityInfo]
> lookupArity id aEnv = lookupTopEnv id aEnv ++! lookupTupleArity id

> qualLookupArity :: QualIdent -> ArityEnv -> [ArityInfo]
> qualLookupArity qid aEnv = qualLookupTopEnv qid aEnv
>		             ++! qualLookupConsArity qid aEnv
>			     ++! lookupTupleArity (unqualify qid)

> qualLookupConsArity :: QualIdent -> ArityEnv -> [ArityInfo]
> qualLookupConsArity qid aEnv
>    | maybe False ((==) preludeMIdent) mmid && id == consId
>      = qualLookupTopEnv (qualify id) aEnv
>    | otherwise
>      = []
>  where (mmid, id) = (qualidMod qid, qualidId qid)

> lookupTupleArity :: Ident -> [ArityInfo]
> lookupTupleArity id 
>    | isTupleId id 
>      = [ArityInfo (qualifyWith preludeMIdent id) (tupleArity id)]
>    | otherwise
>      = []

\end{verbatim}
\paragraph{Module alias}
\begin{verbatim}

> type ImportEnv = Map.Map ModuleIdent ModuleIdent

> bindAlias :: CS.Decl -> ImportEnv -> ImportEnv
> bindAlias (CS.ImportDecl _ mid _ mmid _)
>    = Map.insert mid (fromMaybe mid mmid)

> lookupAlias :: ModuleIdent -> ImportEnv -> Maybe ModuleIdent
> lookupAlias = Map.lookup

> sureLookupAlias :: ModuleIdent -> ImportEnv -> ModuleIdent
> sureLookupAlias m = fromMaybe m . lookupAlias m


\end{verbatim}
\paragraph{Operator precedences}
In order to parse infix expressions correctly, the compiler must know
the precedence and fixity of each operator. Operator precedences are
associated with entities and will be checked after renaming was
applied. Nevertheless, we need to save precedences for ambiguous names
in order to handle them correctly while computing the exported
interface of a module.

If no fixity is assigned to an operator, it will be given the default
precedence 9 and assumed to be a left-associative operator.

\em{Note:} this modified version uses Haskell type \texttt{Integer}
for representing the precedence. This change had to be done due to the
introduction of unlimited integer constants in the parser / lexer.
\begin{verbatim}

> data OpPrec = OpPrec CS.Infix Integer deriving Eq

> instance Show OpPrec where
>   showsPrec _ (OpPrec fix p) = showString (assoc fix) . shows p
>     where assoc CS.InfixL = "left "
>           assoc CS.InfixR = "right "
>           assoc CS.Infix  = "non-assoc "

> defaultP :: OpPrec
> defaultP = OpPrec CS.InfixL 9

\end{verbatim}
The lookup functions for the environment which maintains the operator
precedences are simpler than for the type and value environments
because they do not need to handle tuple constructors.
\begin{verbatim}

> data PrecInfo = PrecInfo QualIdent OpPrec deriving (Eq,Show)

> instance Entity PrecInfo where
>   origName (PrecInfo op _) = op

> type PEnv = TopEnv PrecInfo

> bindP :: ModuleIdent -> Ident -> OpPrec -> PEnv -> PEnv
> bindP m op p
>   | uniqueId op == 0 
>     = bindTopEnv "Base.bindP" op info . qualBindTopEnv "Base.bindP" op' info
>   | otherwise = bindTopEnv "Base.bindP" op info
>   where op' = qualifyWith m op
>         info = PrecInfo op' p

> lookupP :: Ident -> PEnv -> [PrecInfo]
> lookupP = lookupTopEnv

> qualLookupP :: QualIdent -> PEnv -> [PrecInfo]
> qualLookupP = qualLookupTopEnv

\end{verbatim}
\paragraph{Evaluation modes}
The compiler has to collect the evaluation annotations for a program
in an environment. As these annotations affect only local declarations,
a flat environment mapping unqualified names onto annotations is
sufficient.
\begin{verbatim}

> type EvalEnv = Map.Map Ident CS.EvalAnnotation


\end{verbatim}
\paragraph{Predefined types}
The list and unit data types must be predefined because their
definitions
\begin{verbatim}
data () = ()
data [] a = [] | a : [a]
\end{verbatim}
are not allowed by Curry's syntax. The corresponding types
are available in the environments \texttt{initTCEnv} and
\texttt{initDCEnv}. In addition, the precedence of the (infix) list
constructor is available in the environment \texttt{initPEnv}.

Note that only the unqualified names are predefined. This is correct,
because neither \texttt{Prelude.()} nor \texttt{Prelude.[]} are valid
identifiers.
\begin{verbatim}

> initPEnv :: PEnv
> initPEnv =
>   predefTopEnv qConsId (PrecInfo qConsId (OpPrec CS.InfixR 5)) emptyTopEnv

> initTCEnv :: TCEnv
> initTCEnv = foldr (uncurry predefTC) emptyTopEnv predefTypes
>   where predefTC (TypeConstructor tc tys) =
>           predefTopEnv (qualify (unqualify tc)) .
>             DataType tc (length tys) . map Just

> initDCEnv :: ValueEnv
> initDCEnv =
>   foldr (uncurry predefDC) emptyTopEnv
>         [(c,constrType (polyType ty) n' tys)
>         | (ty,cs) <- predefTypes, Data c n' tys <- cs]
>   where predefDC c ty = predefTopEnv c' (DataConstructor c' ty)
>           where c' = qualify c
>         constrType (ForAll n ty) n' = ForAllExist n n' . foldr TypeArrow ty

> initAEnv :: ArityEnv
> initAEnv
>    = foldr bindPredefArity emptyTopEnv (concatMap snd predefTypes)
>  where
>  bindPredefArity (Data id _ ts)
>     = bindArity preludeMIdent id (length ts)

> initIEnv :: ImportEnv
> initIEnv = Map.empty

> predefTypes :: [(Type,[Data [Type]])]
> predefTypes =
>   let a = typeVar 0 in [
>     (unitType,   [Data unitId 0 []]),
>     (listType a, [Data nilId 0 [],Data consId 0 [a,listType a]])
>   ]


\end{verbatim}

\paragraph{Miscellany}
Error handling
\begin{verbatim}

> errorAt :: Position -> String -> a
> errorAt p msg = error ("\n" ++ show p ++ ": " ++ msg)

> errorAt' :: (Position,String) -> a
> errorAt' = uncurry errorAt

> internalError :: String -> a
> internalError what = error ("internal error: " ++ what)

\end{verbatim}
Name supply for the generation of (type) variable names.
\begin{verbatim}

> nameSupply :: [Ident]
> nameSupply = map mkIdent [c:showNum i | i <- [0..], c <- ['a'..'z']]
>   where showNum 0 = ""
>         showNum n = show n

\end{verbatim}
\ToDo{The \texttt{nameSupply} should respect the current case mode, 
i.e., use upper case for variables in Prolog mode.}

\end{verbatim}
The function \texttt{findDouble} checks whether a list of entities is
linear, i.e., if every entity in the list occurs only once. If it is
non-linear, the first offending object is returned.
\begin{verbatim}

> findDouble :: Eq a => [a] -> Maybe a
> findDouble (x:xs)
>   | x `elem` xs = Just x
>   | otherwise = findDouble xs
> findDouble [] = Nothing

\end{verbatim}



