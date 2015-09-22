
% $Id: Modules.lhs,v 1.84 2004/02/10 17:46:07 wlux Exp $
%
% Copyright (c) 1999-2004, Wolfgang Lux
% See LICENSE for the full license.
%
% Modified by Martin Engelke (men@informatik.uni-kiel.de)
% March 2007, extensions by Sebastian Fischer (sebf@informatik.uni-kiel.de)
%
\nwfilename{Modules.lhs}
\section{Modules}
This module controls the compilation of modules.

Since this version is only used as a frontend for PAKCS, some of the following 
import declarations are commented out
\begin{verbatim}

> module Modules(compileModule,
>                importPrelude, patchModuleId,
>	         loadInterfaces, transModule,
>	         simpleCheckModule, checkModule
>	        ) where

> import Text.PrettyPrint.HughesPJ
> import Data.List
> import qualified Data.Map as Map
> import System.IO
> import Data.Maybe
> import Control.Monad

> import Curry.Base.MessageMonad
> import Curry.Base.Position as P
> import Curry.Base.Ident

> import Curry.Files.Filenames
> import Curry.Files.PathUtils

> import Curry.Syntax
> import Curry.Syntax.Utils(isImportDecl)
> import Curry.Syntax.Pretty(ppModule,ppIDecl)
> import Curry.Syntax.ShowModule(showModule)

> import Curry.ExtendedFlat.Type
> import qualified Curry.ExtendedFlat.Type as EF 

> import qualified IL.Type as IL
> import IL.CurryToIL(ilTrans)
> import qualified IL.Pretty(ppModule)
> import IL.XML(xmlModule)

> import Base
> import Types
> import KindCheck(kindCheck)
> import SyntaxCheck(syntaxCheck)
> import PrecCheck(precCheck)
> import TypeCheck(typeCheck)
> import WarnCheck(warnCheck)
> import Arity
> import Imports(importInterface,importInterfaceIntf,importUnifyData)
> import Exports(expandInterface,exportInterface)
> import Eval(evalEnv)
> import Qual(qual)
> import Desugar(desugar)
> import Simplify(simplify)
> import Lift(lift)

> import GenFlatCurry (genFlatCurry,genFlatInterface)
> import qualified Curry.AbstractCurry as AC
> import GenAbstractCurry
> import InterfaceCheck
> import CurryEnv

> import CurryCompilerOpts(Options(..),Dump(..))
> import CaseCompletion


> import TypeSubst
> import TopEnv


\end{verbatim}
The function \texttt{compileModule} is the main entry-point of this
module for compiling a Curry source module. Depending on the command
line options it will emit either C code or FlatCurry code (standard 
or in XML
representation) or AbtractCurry code (typed, untyped or with type
signatures) for the module. Usually the first step is to
check the module. Then the code is translated into the intermediate
language. If necessary, this phase will also update the module's
interface file. The resulting code then is either written out (in
FlatCurry or XML format) or translated further into C code.
The untyped  AbstractCurry representation is written
out directly after parsing and simple checking the source file. 
The typed AbstractCurry code is written out after checking the module.

The compiler automatically loads the prelude when compiling any
module, except for the prelude itself, by adding an appropriate import
declaration to the module. 

Since this modified version of the Muenster Curry Compiler is used
as a frontend for PAKCS, all functions for evaluating goals and generating C 
code are obsolete and commented out.
\begin{verbatim}

> compileModule :: Options -> FilePath -> IO (Maybe FilePath)
> compileModule opts fn =
>   do
>     mod <- liftM (importPrelude fn . ok . parseModule likeFlat fn) (readModule fn)
>     let m = patchModuleId fn mod
>     checkModuleId fn m
>     mEnv <- loadInterfaces (importPaths opts) m
>     if uacy || src
>        then 
>          do (tyEnv, tcEnv, aEnv, m', intf, _) <- simpleCheckModule opts mEnv m
>             if uacy then genAbstract opts fn tyEnv tcEnv m'
>                     else do
>                       let outputFile = maybe (sourceRepName fn)
>                                              id 
>                                              (output opts)
>                           outputMod = showModule m'
>                       writeModule (writeToSubdir opts) outputFile outputMod
>                       return Nothing
>        else
>          do -- checkModule checks types, and then transModule introduces new
>             -- functions (by lambda lifting in 'desugar'). Consequence: The
>             -- type of the newly introduced functions are not inferred (hsi)
>             (tyEnv, tcEnv, aEnv, m', intf, _) <- checkModule opts mEnv m
>             let (il,aEnv',dumps) = transModule fcy False False 
>			                         mEnv tyEnv tcEnv aEnv m'
>             mapM_ (doDump opts) dumps
>	      genCode opts fn mEnv tyEnv tcEnv aEnv' intf m' il
>   where acy      = abstract opts
>         uacy     = untypedAbstract opts
>         fcy      = flat opts
>         xml      = flatXml opts
>         src      = parseOnly opts
>         likeFlat = fcy || xml || acy || uacy || src
>	  
>         genCode opts fn mEnv tyEnv tcEnv aEnv intf m il
>            | fcy || xml = genFlat opts fn mEnv tyEnv tcEnv aEnv intf m il
>            | acy        = genAbstract opts fn tyEnv tcEnv m
>            | otherwise  = return Nothing

> loadInterfaces :: [FilePath] -> Module -> IO ModuleEnv
> loadInterfaces paths (Module m _ ds) =
>   foldM (loadInterface paths [m]) Map.empty
>         [(p,m) | ImportDecl p m _ _ _ <- ds]

> checkModuleId :: Monad m => FilePath -> Module -> m ()
> checkModuleId fn (Module mid _ _)
>    | last (moduleQualifiers mid) == takeBaseName fn
>      = return ()
>    | otherwise
>      = error ("module \"" ++ moduleName mid 
>	        ++ "\" must be in a file \"" ++ moduleName mid
>	        ++ ".curry\"")

> simpleCheckModule :: Options -> ModuleEnv -> Module 
>	    -> IO (ValueEnv,TCEnv,ArityEnv,Module,Interface,[WarnMsg])
> simpleCheckModule opts mEnv (Module m es ds) =
>   do unless (noWarn opts) (printMessages msgs)
>      return (tyEnv'', tcEnv, aEnv'', modul, intf, msgs)
>   where (impDs,topDs) = partition isImportDecl ds
>         iEnv = foldr bindAlias initIEnv impDs
>         (pEnv,tcEnv,tyEnv,aEnv) = importModules mEnv impDs
>         msgs = warnCheck m tyEnv impDs topDs
>	  withExt = withExtensions opts
>         (pEnv',topDs') = precCheck m pEnv 
>		           $ syntaxCheck withExt m iEnv aEnv tyEnv tcEnv
>			   $ kindCheck m tcEnv topDs
>         ds' = impDs ++ qual m tyEnv topDs'
>         modul = (Module m es ds') --expandInterface (Module m es ds') tcEnv tyEnv
>         (_,tcEnv'',tyEnv'',aEnv'') 
>            = qualifyEnv mEnv pEnv' tcEnv tyEnv aEnv
>         intf = exportInterface modul pEnv' tcEnv'' tyEnv''

> checkModule :: Options -> ModuleEnv -> Module 
>      -> IO (ValueEnv,TCEnv,ArityEnv,Module,Interface,[WarnMsg])
> checkModule opts mEnv (Module m es ds) =
>   do unless (noWarn opts) (printMessages msgs)
>      when (m == mkMIdent ["field114..."])
>           (error (show es))
>      return (tyEnv''', tcEnv', aEnv'', modul, intf, msgs)
>   where (impDs,topDs) = partition isImportDecl ds
>         iEnv = foldr bindAlias initIEnv impDs
>         (pEnv,tcEnvI,tyEnvI,aEnv) = importModules mEnv impDs
>         tcEnv = if withExtensions opts
>	             then fmap (expandRecordTC tcEnvI) tcEnvI
>		     else tcEnvI
>         lEnv = importLabels mEnv impDs
>	  tyEnvL = addImportedLabels m lEnv tyEnvI
>	  tyEnv = if withExtensions opts
>	             then fmap (expandRecordTypes tcEnv) tyEnvL
>		     else tyEnvI
>         msgs = warnCheck m tyEnv impDs topDs
>	  withExt = withExtensions opts
>         -- fre: replaced the argument aEnv by aEnv'' in the
>         --      expression below. This fixed a bug that occured
>         --      when one imported a module qualified that
>         --      exported a function from another module.
>         --      However, there is now a cyclic dependecy 
>         --      but tests didn't show any problems.
>         (pEnv',topDs') = precCheck m pEnv 
>		           $ syntaxCheck withExt m iEnv aEnv'' tyEnv tcEnv
>			   $ kindCheck m tcEnv topDs
>         (tcEnv',tyEnv') = typeCheck m tcEnv tyEnv topDs'
>         ds' = impDs ++ qual m tyEnv' topDs'
>         modul = expandInterface (Module m es ds') tcEnv' tyEnv'
>         (pEnv'',tcEnv'',tyEnv'',aEnv'') 
>            = qualifyEnv mEnv pEnv' tcEnv' tyEnv' aEnv
>         tyEnvL' = addImportedLabels m lEnv tyEnv''
>	  tyEnv''' = if withExtensions opts
>	                then fmap (expandRecordTypes tcEnv'') tyEnvL'
>		        else tyEnv''
>         --tyEnv''' = addImportedLabels m lEnv tyEnv''
>         intf = exportInterface modul pEnv'' tcEnv'' tyEnv'''

> transModule :: Bool -> Bool -> Bool -> ModuleEnv -> ValueEnv -> TCEnv
>      -> ArityEnv -> Module -> (IL.Module,ArityEnv,[(Dump,Doc)])
> transModule flat debug trusted mEnv tyEnv tcEnv aEnv (Module m es ds) =
>     (il',aEnv',dumps)
>   where topDs = filter (not . isImportDecl) ds
>         evEnv = evalEnv topDs
>         (desugared,tyEnv') = desugar tyEnv tcEnv (Module m es topDs)
>         (simplified,tyEnv'') = simplify flat tyEnv' evEnv desugared
>         (lifted,tyEnv''',evEnv') = lift tyEnv'' evEnv simplified
>         aEnv' = bindArities aEnv lifted
>         il = ilTrans flat tyEnv''' tcEnv evEnv' lifted
>         il' = completeCase mEnv il
>         dumps = [(DumpRenamed,ppModule (Module m es ds)),
>	           (DumpTypes,ppTypes m (localBindings tyEnv)),
>	           (DumpDesugared,ppModule desugared),
>                  (DumpSimplified,ppModule simplified),
>                  (DumpLifted,ppModule lifted),
>                  (DumpIL,IL.Pretty.ppModule il),
>	           (DumpCase,IL.Pretty.ppModule il')
>	          ]

> qualifyEnv :: ModuleEnv -> PEnv -> TCEnv -> ValueEnv -> ArityEnv
>     -> (PEnv,TCEnv,ValueEnv,ArityEnv)
> qualifyEnv mEnv pEnv tcEnv tyEnv aEnv =
>   (foldr bindQual pEnv' (localBindings pEnv),
>    foldr bindQual tcEnv' (localBindings tcEnv),
>    foldr bindGlobal tyEnv' (localBindings tyEnv),
>    foldr bindQual aEnv' (localBindings aEnv))
>   where (pEnv',tcEnv',tyEnv',aEnv') =
>           foldl importInterface initEnvs (Map.toList mEnv)
>         importInterface (pEnv,tcEnv,tyEnv,aEnv) (m,ds) =
>           importInterfaceIntf (Interface m ds) pEnv tcEnv tyEnv aEnv
>         bindQual (_,y) = qualBindTopEnv "Modules.qualifyEnv" (origName y) y
>         bindGlobal (x,y)
>           | uniqueId x == 0 = bindQual (x,y)
>           | otherwise = bindTopEnv "Modules.qualifyEnv" x y

> writeXML :: Bool -> Maybe FilePath -> FilePath -> CurryEnv -> IL.Module -> IO ()
> writeXML sub tfn sfn cEnv il = writeModule sub ofn (showln code)
>   where ofn  = fromMaybe (xmlName sfn) tfn
>         code = (xmlModule cEnv il)

> writeFlat :: Options -> Maybe FilePath -> FilePath -> CurryEnv -> ModuleEnv 
>              -> ValueEnv -> TCEnv -> ArityEnv -> IL.Module -> IO Prog
> writeFlat opts tfn sfn cEnv mEnv tyEnv tcEnv aEnv il
>   = writeFlatFile opts (genFlatCurry opts cEnv mEnv tyEnv tcEnv aEnv il)
>                        (fromMaybe (flatName sfn) tfn)

> writeFlatFile :: Options -> (Prog, [WarnMsg]) -> String -> IO Prog
> writeFlatFile opts@Options{extendedFlat=ext,writeToSubdir=sub} (res,msgs) fname = do
>         unless (noWarn opts) (printMessages msgs)
>	  if ext then writeExtendedFlat sub fname res
>                else writeFlatCurry sub fname res
>         return res


> writeTypedAbs :: Bool -> Maybe FilePath -> FilePath -> ValueEnv -> TCEnv -> Module
>	           -> IO ()
> writeTypedAbs sub tfn sfn tyEnv tcEnv mod
>    = AC.writeCurry sub fname (genTypedAbstract tyEnv tcEnv mod)
>  where fname = fromMaybe (acyName sfn) tfn

> writeUntypedAbs :: Bool -> Maybe FilePath -> FilePath -> ValueEnv -> TCEnv  
>	             -> Module -> IO ()
> writeUntypedAbs sub tfn sfn tyEnv tcEnv mod
>    = AC.writeCurry sub fname (genUntypedAbstract tyEnv tcEnv mod)
>  where fname = fromMaybe (uacyName sfn) tfn

> showln :: Show a => a -> String
> showln x = shows x "\n"

\end{verbatim}

The function \texttt{importModules} brings the declarations of all
imported modules into scope for the current module.
\begin{verbatim}

> importModules :: ModuleEnv -> [Decl] -> (PEnv,TCEnv,ValueEnv,ArityEnv)
> importModules mEnv ds = (pEnv,importUnifyData tcEnv,tyEnv,aEnv)
>   where (pEnv,tcEnv,tyEnv,aEnv) = foldl importModule initEnvs ds
>         importModule (pEnv,tcEnv,tyEnv,aEnv) (ImportDecl p m q asM is) =
>           case Map.lookup m mEnv of
>             Just ds -> importInterface p (fromMaybe m asM) q is
>                                        (Interface m ds) pEnv tcEnv tyEnv aEnv
>             Nothing -> internalError "importModule"
>         importModule (pEnv,tcEnv,tyEnv,aEnv) _ = (pEnv,tcEnv,tyEnv,aEnv)

> initEnvs :: (PEnv,TCEnv,ValueEnv,ArityEnv)
> initEnvs = (initPEnv,initTCEnv,initDCEnv,initAEnv)

\end{verbatim}
Unlike unsual identifiers like in functions, types etc. identifiers
of labels are always represented unqualified within the whole context
of compilation. Since the common type environment (type \texttt{ValueEnv})
has some problems with handling imported unqualified identifiers, it is 
necessary to add the type information for labels seperately. For this reason
the function \texttt{importLabels} generates an environment containing
all imported labels and the function \texttt{addImportedLabels} adds this
content to a type environment.
\begin{verbatim}

> importLabels :: ModuleEnv -> [Decl] -> LabelEnv
> importLabels mEnv ds = foldl importLabelTypes Map.empty ds
>   where
>   importLabelTypes lEnv (ImportDecl p m _ asM is) =
>     case (Map.lookup m mEnv) of
>       Just ds' -> foldl (importLabelType p (fromMaybe m asM) is) lEnv ds'
>       Nothing -> internalError "importLabels"
>   importLabelTypes lEnv _ = lEnv
>		      
>   importLabelType p m is lEnv (ITypeDecl _ r _ (RecordType fs _)) =
>     foldl (insertLabelType p m r' (getImportSpec r' is)) lEnv fs
>     where r' = qualifyWith m (fromRecordExtId (unqualify r))
>   importLabelType _ _ _ lEnv _ = lEnv
>			   
>   insertLabelType p m r (Just (ImportTypeAll _)) lEnv ([l],ty) =
>     bindLabelType l r (toType [] ty) lEnv
>   insertLabelType p m r (Just (ImportTypeWith _ ls)) lEnv ([l],ty)
>     | l `elem` ls = bindLabelType l r (toType [] ty) lEnv
>     | otherwise   = lEnv
>   insertLabelType _ _ _ _ lEnv _ = lEnv
>			     
>   getImportSpec r (Just (Importing _ is')) =
>     find (isImported (unqualify r)) is'
>   getImportSpec r Nothing = Just (ImportTypeAll (unqualify r))
>   getImportSpec r _ = Nothing
>		
>   isImported r (Import r') = r == r'
>   isImported r (ImportTypeWith r' _) = r == r'
>   isImported r (ImportTypeAll r') = r == r'

> addImportedLabels :: ModuleIdent -> LabelEnv -> ValueEnv -> ValueEnv
> addImportedLabels m lEnv tyEnv = 
>   foldr addLabelType tyEnv (concatMap snd (Map.toList lEnv))
>   where
>   addLabelType (LabelType l r ty) tyEnv = 
>     let m' = fromMaybe m (qualidMod r)
>     in  importTopEnv m' l 
>                      (Label (qualify l) (qualQualify m' r) (polyType ty)) 
>	               tyEnv

\end{verbatim}
Fully expand all (imported) record types within the type constructor 
environment and the type environment.
Note: the record types for the current module are expanded within the
type check.
\begin{verbatim}

> expandRecordTC :: TCEnv -> TypeInfo -> TypeInfo
> expandRecordTC tcEnv (DataType qid n args) =
>   DataType qid n (map (maybe Nothing (Just . (expandData tcEnv))) args)
> expandRecordTC tcEnv (RenamingType qid n (Data id m ty)) =
>   RenamingType qid n (Data id m (expandRecords tcEnv ty))
> expandRecordTC tcEnv (AliasType qid n ty) =
>   AliasType qid n (expandRecords tcEnv ty)

> expandData :: TCEnv -> Data [Type] -> Data [Type]
> expandData tcEnv (Data id n tys) =
>   Data id n (map (expandRecords tcEnv) tys)

> expandRecordTypes :: TCEnv -> ValueInfo -> ValueInfo
> expandRecordTypes tcEnv (DataConstructor qid (ForAllExist n m ty)) =
>   DataConstructor qid (ForAllExist n m (expandRecords tcEnv ty))
> expandRecordTypes tcEnv (NewtypeConstructor qid (ForAllExist n m ty)) =
>   NewtypeConstructor qid (ForAllExist n m (expandRecords tcEnv ty))
> expandRecordTypes tcEnv (Value qid (ForAll n ty)) =
>   Value qid (ForAll n (expandRecords tcEnv ty))
> expandRecordTypes tcEnv (Label qid r (ForAll n ty)) =
>   Label qid r (ForAll n (expandRecords tcEnv ty))

> expandRecords :: TCEnv -> Type -> Type
> expandRecords tcEnv (TypeConstructor qid tys) =
>   case (qualLookupTC qid tcEnv) of
>     [AliasType _ _ rty@(TypeRecord _ _)]
>       -> expandRecords tcEnv 
>            (expandAliasType (map (expandRecords tcEnv) tys) rty)
>     _ -> TypeConstructor qid (map (expandRecords tcEnv) tys)
> expandRecords tcEnv (TypeConstrained tys v) =
>   TypeConstrained (map (expandRecords tcEnv) tys) v
> expandRecords tcEnv (TypeArrow ty1 ty2) =
>   TypeArrow (expandRecords tcEnv ty1) (expandRecords tcEnv ty2)
> expandRecords tcEnv (TypeRecord fs rv) =
>   TypeRecord (map (\ (l,ty) -> (l,expandRecords tcEnv ty)) fs) rv
> expandRecords _ ty = ty

\end{verbatim}
An implicit import of the prelude is added to the declarations of
every module, except for the prelude itself. If no explicit import for
the prelude is present, the prelude is imported unqualified, otherwise
only a qualified import is added.
\begin{verbatim}

> importPrelude :: FilePath -> Module -> Module
> importPrelude fn (Module m es ds) =
>   Module m es (if m == preludeMIdent then ds else ds')
>   where ids = [decl | decl@(ImportDecl _ _ _ _ _) <- ds]
>         ds' = ImportDecl (P.first fn) preludeMIdent
>                          (preludeMIdent `elem` map importedModule ids)
>                          Nothing Nothing : ds
>         importedModule (ImportDecl _ m q asM is) = fromMaybe m asM

\end{verbatim}
If an import declaration for a module is found, the compiler first
checks whether an import for the module is already pending. In this
case the module imports are cyclic which is not allowed in Curry. The
compilation will therefore be aborted. Next, the compiler checks
whether the module has been imported already. If so, nothing needs to
be done, otherwise the interface will be searched in the import paths
and compiled.
\begin{verbatim}

> loadInterface :: [FilePath] -> [ModuleIdent] -> ModuleEnv ->
>     (Position,ModuleIdent) -> IO ModuleEnv
> loadInterface paths ctxt mEnv (p,m)
>   | m `elem` ctxt = errorAt p (cyclicImport m (takeWhile (/= m) ctxt))
>   | isLoaded m mEnv = return mEnv
>   | otherwise =
>       lookupInterface paths m >>=
>       maybe (errorAt p (interfaceNotFound m))
>             (compileInterface paths ctxt mEnv m)
>   where isLoaded m mEnv = maybe False (const True) (Map.lookup m mEnv)

\end{verbatim}
After reading an interface, all imported interfaces are recursively
loaded and entered into the interface's environment. There is no need
to check FlatCurry-Interfaces, since these files contain automaticaly
generated FlatCurry terms (type \texttt{Prog}).
\begin{verbatim}

> compileInterface :: [FilePath] -> [ModuleIdent] -> ModuleEnv -> ModuleIdent
>                  -> FilePath -> IO ModuleEnv
> compileInterface paths ctxt mEnv m fn =
>   do
>     mintf <- readFlatInterface fn
>     let intf = fromMaybe (errorAt (P.first fn) (interfaceNotFound m)) mintf
>         (Prog mod _ _ _ _) = intf
>         m' = mkMIdent [mod]
>     unless (m' == m) (errorAt (P.first fn) (wrongInterface m m'))
>     mEnv' <- loadFlatInterfaces paths ctxt mEnv intf
>     return (bindFlatInterface intf mEnv')

> --loadIntfInterfaces :: [FilePath] -> [ModuleIdent] -> ModuleEnv -> Interface
> --                   -> IO ModuleEnv
> --loadIntfInterfaces paths ctxt mEnv (Interface m ds) =
> --  foldM (loadInterface paths (m:ctxt)) mEnv [(p,m) | IImportDecl p m <- ds]


> loadFlatInterfaces :: [FilePath] -> [ModuleIdent] -> ModuleEnv -> Prog
>                    -> IO ModuleEnv
> loadFlatInterfaces paths ctxt mEnv (Prog m is _ _ _) =
>   foldM (loadInterface paths ((mkMIdent [m]):ctxt)) 
>         mEnv 
>         (map (\i -> (p, mkMIdent [i])) is)
>  where p = P.first m


Interface files are updated by the Curry builder when necessary.
(see module \texttt{CurryBuilder}).

\end{verbatim}
The \texttt{doDump} function writes the selected information to the
standard output.
\begin{verbatim}

> doDump :: Options -> (Dump,Doc) -> IO ()
> doDump opts (d,x) =
>   when (d `elem` dump opts)
>        (print (text hd $$ text (replicate (length hd) '=') $$ x))
>   where hd = dumpHeader d

> dumpHeader :: Dump -> String
> dumpHeader DumpRenamed = "Module after renaming"
> dumpHeader DumpTypes = "Types"
> dumpHeader DumpDesugared = "Source code after desugaring"
> dumpHeader DumpSimplified = "Source code after simplification"
> dumpHeader DumpLifted = "Source code after lifting"
> dumpHeader DumpIL = "Intermediate code"
> dumpHeader DumpCase = "Intermediate code after case simplification"


\end{verbatim}
The functions \texttt{genFlat} and \texttt{genAbstract} generate
flat and abstract curry representations depending on the specified option.
If the interface of a modified Curry module did not change, the corresponding 
file name will be returned within the result of \texttt{genFlat} (depending
on the compiler flag "force") and other modules importing this module won't
be dependent on it any longer.
\begin{verbatim}

> genFlat :: Options -> FilePath -> ModuleEnv -> ValueEnv -> TCEnv -> ArityEnv 
>            -> Interface -> Module -> IL.Module -> IO (Maybe FilePath)
> genFlat opts fname mEnv tyEnv tcEnv aEnv intf mod il
>   | flat opts
>     = do writeFlat opts Nothing fname cEnv mEnv tyEnv tcEnv aEnv il
>          let (flatInterface,intMsgs) = genFlatInterface opts cEnv mEnv tyEnv tcEnv aEnv il
>          if force opts
>            then 
>              do writeInterface flatInterface intMsgs
>                 return Nothing
>            else 
>               do mfint <- readFlatInterface fintName
>                  let flatIntf = fromMaybe emptyIntf mfint
>                  if mfint == mfint  -- necessary to close the file 'fintName'
>                        && not (interfaceCheck flatIntf flatInterface)
>                     then 
>                        do writeInterface flatInterface intMsgs
>                           return Nothing
>                     else return Nothing
>   | flatXml opts
>     = writeXML (writeToSubdir opts) (output opts) fname cEnv il >> 
>       return Nothing
>   | otherwise
>     = internalError "@Modules.genFlat: illegal option"
>  where
>    fintName = flatIntName fname
>    cEnv = curryEnv mEnv tcEnv intf mod
>    emptyIntf = Prog "" [] [] [] []
>    writeInterface intf msgs = do
>          unless (noWarn opts) (printMessages msgs)
>          writeFlatCurry (writeToSubdir opts) fintName intf


> genAbstract :: Options -> FilePath  -> ValueEnv -> TCEnv -> Module 
>                -> IO (Maybe FilePath)
> genAbstract opts@Options{writeToSubdir=sub} fname tyEnv tcEnv mod
>    | abstract opts
>      = do writeTypedAbs sub Nothing fname tyEnv tcEnv mod 
>           return Nothing
>    | untypedAbstract opts
>      = do writeUntypedAbs sub Nothing fname tyEnv tcEnv mod
>           return Nothing
>    | otherwise
>      = internalError "@Modules.genAbstract: illegal option"

> printMessages :: [WarnMsg] -> IO ()
> printMessages []   = return ()
> printMessages msgs = hPutStrLn stderr $ unlines $ map showWarning msgs

\end{verbatim}
The function \texttt{ppTypes} is used for pretty-printing the types
from the type environment.
\begin{verbatim}

> ppTypes :: ModuleIdent -> [(Ident,ValueInfo)] -> Doc
> ppTypes m = vcat . map (ppIDecl . mkDecl) . filter (isValue . snd)
>   where mkDecl (v,Value _ (ForAll _ ty)) =
>           IFunctionDecl undefined (qualify v) (arrowArity ty) 
>		      (fromQualType m ty)
>         isValue (DataConstructor _ _) = False
>         isValue (NewtypeConstructor _ _) = False
>         isValue (Value _ _) = True
>         isValue (Label _ _ _) = False


\end{verbatim}
A module which doesn't contain a \texttt{module ... where} declaration
obtains its filename as module identifier (unlike the definition in
Haskell and original MCC where a module obtains \texttt{main}).
\begin{verbatim}

> patchModuleId :: FilePath -> Module -> Module
> patchModuleId fn (Module mid mexports decls)
>    | (moduleName mid) == "main"
>      = Module (mkMIdent [takeBaseName fn]) mexports decls
>    | otherwise
>      = Module mid mexports decls


\end{verbatim}
Error functions.
\begin{verbatim}

> interfaceNotFound :: ModuleIdent -> String
> interfaceNotFound m = "Interface for module " ++ moduleName m ++ " not found"

> cyclicImport :: ModuleIdent -> [ModuleIdent] -> String
> cyclicImport m [] = "Recursive import for module " ++ moduleName m
> cyclicImport m ms =
>   "Cyclic import dependency between modules " ++ moduleName m ++
>     modules "" ms
>   where modules comma [m] = comma ++ " and " ++ moduleName m
>         modules _ (m:ms) = ", " ++ moduleName m ++ modules "," ms

> wrongInterface :: ModuleIdent -> ModuleIdent -> String
> wrongInterface m m' =
>   "Expected interface for " ++ show m ++ " but found " ++ show m'

\end{verbatim}




> bindFlatInterface :: Prog -> ModuleEnv -> ModuleEnv
> bindFlatInterface (Prog m imps ts fs os)
>    = Map.insert (mkMIdent [m])
>      ((map genIImportDecl imps)
>       ++ (map genITypeDecl ts')
>       ++ (map genIFuncDecl fs)
>       ++ (map genIOpDecl os))
>  where
>  genIImportDecl :: String -> IDecl
>  genIImportDecl imp = IImportDecl pos (mkMIdent [imp])
>
>  genITypeDecl :: TypeDecl -> IDecl
>  genITypeDecl (Type qn _ is cs)
>     | recordExt `isPrefixOf` localName qn
>       = ITypeDecl pos
>                   (genQualIdent qn)
>	            (map (genVarIndexIdent "a") is)
>	            (RecordType (map genLabeledType cs) Nothing)
>     | otherwise
>       = IDataDecl pos 
>                   (genQualIdent qn) 
>                   (map (genVarIndexIdent "a") is) 
>                   (map (Just . genConstrDecl) cs)
>  genITypeDecl (TypeSyn qn _ is t)
>     = ITypeDecl pos
>                 (genQualIdent qn)
>                 (map (genVarIndexIdent "a") is)
>                 (genTypeExpr t)
>
>  genIFuncDecl :: FuncDecl -> IDecl
>  genIFuncDecl (Func qn a _ t _) 
>     = IFunctionDecl pos (genQualIdent qn) a (genTypeExpr t)
>
>  genIOpDecl :: OpDecl -> IDecl
>  genIOpDecl (Op qn f p) = IInfixDecl pos (genInfix f) p  (genQualIdent qn)
>
>  genConstrDecl :: ConsDecl -> ConstrDecl
>  genConstrDecl (Cons qn _ _ ts)
>     = ConstrDecl pos [] (mkIdent (localName qn)) (map genTypeExpr ts)
>
>  genLabeledType :: EF.ConsDecl -> ([Ident],Curry.Syntax.TypeExpr)
>  genLabeledType (Cons qn _ _ [t])
>     = ([renameLabel (fromLabelExtId (mkIdent $ localName qn))], genTypeExpr t)
>
>  genTypeExpr :: EF.TypeExpr -> Curry.Syntax.TypeExpr
>  genTypeExpr (TVar i)
>     = VariableType (genVarIndexIdent "a" i)
>  genTypeExpr (FuncType t1 t2) 
>     = ArrowType (genTypeExpr t1) (genTypeExpr t2)
>  genTypeExpr (TCons qn ts) 
>     = ConstructorType (genQualIdent qn) (map genTypeExpr ts)
>
>  genInfix :: EF.Fixity -> Infix
>  genInfix EF.InfixOp  = Infix
>  genInfix EF.InfixlOp = InfixL
>  genInfix EF.InfixrOp = InfixR
>
>  genQualIdent :: EF.QName -> QualIdent
>  genQualIdent EF.QName{modName=mod,localName=name} = 
>    qualifyWith (mkMIdent [mod]) (mkIdent name)
>
>  genVarIndexIdent :: String -> Int -> Ident
>  genVarIndexIdent v i = mkIdent (v ++ show i)
>
>  isSpecialPreludeType :: TypeDecl -> Bool
>  isSpecialPreludeType (Type EF.QName{modName=mod,localName=name} _ _ _) 
>     = (name == "[]" || name == "()") && mod == "Prelude"
>  isSpecialPreludeType _ = False
>
>  pos = P.first m
>  ts' = filter (not . isSpecialPreludeType) ts




\end{verbatim}
The label environment is used to store information of labels.
Unlike unsual identifiers like in functions, types etc. identifiers
of labels are always represented unqualified. Since the common type 
environment (type \texttt{ValueEnv}) has some problems with handling 
imported unqualified identifiers, it is necessary to process the type 
information for labels seperately.
\begin{verbatim}

> data LabelInfo = LabelType Ident QualIdent Type deriving Show

> type LabelEnv = Map.Map Ident [LabelInfo]

> bindLabelType :: Ident -> QualIdent -> Type -> LabelEnv -> LabelEnv
> bindLabelType l r ty = Map.insertWith (++) l [LabelType l r ty]

