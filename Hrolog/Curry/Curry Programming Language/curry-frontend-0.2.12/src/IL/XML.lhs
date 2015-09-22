
% $Id: ILxml.lhs,v 1.0 2001/06/19 12:19:18 rafa Exp $
%
% $Log: ILxml.lhs,v $
%
% Revision 1.1  2001/06/19 12:19:18  rafa
% Pretty printer in XML for the intermediate language added.
%
%
% Modified by Martin Engelke (men@informatik.uni-kiel.de)
%
\nwfilename{ILxml.lhs}
\section{A pretty printer in XML for the intermediate language}
This module implements just another pretty printer, this time in XML and for
the intermediate language. It was mainly adapted from the Curry pretty
printer (see sect.~\ref{sec:CurryPP}), which in turn is based on Simon
Marlow's pretty printer for Haskell. The format of the output intends to be
similar to that of Flat-Curry XML representation.
\begin{verbatim}

> module IL.XML(module IL.XML) where

> import Text.PrettyPrint.HughesPJ
> import Data.Maybe

> import Curry.Base.Ident
> import qualified Curry.Syntax as CS

> import IL.Type
> import CurryEnv



> -- identation level
> level::Int
> level = 3

> xmlModule :: CurryEnv -> Module -> Doc
> xmlModule cEnv m = text "<prog>" $$ nest level (xmlBody cEnv m) 
>	                           $$ text "</prog>"

> xmlBody :: CurryEnv -> Module -> Doc
> xmlBody cEnv (Module name imports decls) =
>                   xmlElement "module"      xmlModuleDecl      moduleDecl   $$
>                   xmlElement "import"      xmlImportDecl      importDecl   $$
>                   xmlElement "types"       xmlTypeDecl        typeDecl     $$
>                   xmlElement "functions"   xmlFunctionDecl    functionDecl $$
>                   xmlElement "operators"   xmlOperatorDecl    operatorDecl $$
>                   xmlElement "translation" xmlTranslationDecl translationDecl
>               where
>                 moduleDecl      = [name]
>                 importDecl      = imports
>                 operatorDecl    = infixDecls cEnv
>                 translationDecl = foldl (qualIDeclId (moduleId cEnv))
>			                  [] 
>				          (interface cEnv)
>                 (functionDecl,typeDecl) = splitDecls decls

> -- =========================================================================

> xmlModuleDecl :: ModuleIdent -> Doc
> xmlModuleDecl name = xmlModuleIdent name

> -- =========================================================================

> xmlImportDecl :: ModuleIdent -> Doc
> xmlImportDecl name = xmlElement "module" xmlModuleDecl  [name]


> -- =========================================================================
> --            T Y P E S
> -- =========================================================================

> xmlTypeDecl :: Decl -> Doc
> xmlTypeDecl (DataDecl tc arity cs) =
>   beginType                                  $$
>   nest level (xmlTypeParams arity)           $$
>   xmlLines xmlConstructor cs                 $$
>   endType
>  where
>   beginType = text "<type name=\"" <> (xmlQualIdent tc) <> text "\">"
>   endType   = text "</type>"

> xmlTypeParams :: Int -> Doc
> xmlTypeParams n = xmlElement "params" xmlTypeVar [0..(n-1)]

> xmlConstructor :: ConstrDecl [Type] -> Doc
> xmlConstructor (ConstrDecl ident []) = xmlConstructorBegin ident 0
> xmlConstructor (ConstrDecl ident l)  =
>   xmlConstructorBegin ident (length l) $$
>   xmlLines xmlType l $$
>   xmlConstructorEnd
>  where
>   xmlConstructorEnd = text "</cons>"

> xmlConstructorBegin :: QualIdent -> Int -> Doc
> xmlConstructorBegin ident n = xmlHeadingWithArity "cons" ident n (n==0)

> xmlHeadingWithArity :: String -> QualIdent -> Int -> Bool -> Doc
> xmlHeadingWithArity tagName ident n single =
>   if single
>   then prefix<>text "/>"
>   else prefix<> text ">"
>   where
>     prefix = text ("<"++tagName++" name=\"") <> name <> text "\" " <> arity
>     arity  = text "arity=\"" <> xmlInt n <> text "\""
>     name   = xmlQualIdent ident


> xmlType :: Type -> Doc
> xmlType (TypeConstructor ident []) = xmlTypeConsBegin ident True
> xmlType (TypeConstructor ident l)  = xmlTypeConsBegin ident False $$
>                                      xmlLines xmlType l           $$
>                                      xmlTypeConsEnd
>                                      where
>                                        xmlTypeConsEnd = text "</tcons>"

> xmlType (TypeVariable n) = xmlTypeVar n
> xmlType (TypeArrow  a b) = xmlTypeFun a b

> xmlTypeConsBegin :: QualIdent -> Bool -> Doc
> xmlTypeConsBegin ident single =
>   if single
>   then prefix <> text "/>"
>   else prefix <> text ">"
>   where
>     name   = xmlQualIdent ident
>     prefix = text "<tcons name=\"" <> name <> text "\""

> xmlTypeVar :: Int -> Doc
> xmlTypeVar n = text "<tvar>"<> xmlInt n <> text "</tvar>"

> xmlTypeFun :: Type -> Type -> Doc
> xmlTypeFun a b =  xmlElement "functype" xmlType  [a,b]


> -- =========================================================================
> --            F U N C T I O N S
> -- =========================================================================

> xmlFunctionDecl :: Decl -> Doc
> xmlFunctionDecl (NewtypeDecl tc arity (ConstrDecl ident ty)) =
>   xmlFunctionDecl (FunctionDecl ident [arg] ftype (Variable arg))
>   where
>    arg = mkIdent "_1"
>    ftype = TypeArrow ty (TypeConstructor tc (map TypeVariable [0..arity-1]))

> xmlFunctionDecl (FunctionDecl ident largs fType expr) =
>    heading $$ nest level (xmlRule largs expr) $$ end
>  where
>    heading = xmlBeginFunction ident (length largs) fType
>    end     = text "</func>"

> xmlFunctionDecl (ExternalDecl ident callConv internalName fType) =
>    heading $$ external $$ end
>  where
>    heading  = xmlBeginFunction ident (xmlFunctionArity fType) fType
>    external = text ("<external>"
>                     ++ xmlFormat internalName
>                     ++ "</external>")
>    end      = text "</func>"

> xmlBeginFunction :: QualIdent -> Int -> Type -> Doc
> xmlBeginFunction ident n fType =
>    heading $$ typeDecls
>    where
>      heading   = xmlHeadingWithArity "func" ident n False
>      typeDecls = nest level (xmlType fType)

> xmlEndFunction ::  Doc
> xmlEndFunction  = text "</func>"

> xmlFunctionArity :: Type -> Int
> xmlFunctionArity (TypeConstructor ident l) = 0
> xmlFunctionArity (TypeVariable n)          = 0
> xmlFunctionArity (TypeArrow  a b)          = 1 + (xmlFunctionArity b)

> xmlRule :: [Ident] -> Expression -> Doc
> xmlRule lArgs e = text "<rule>"               $$
>                   nest level (xmlLhs lArgs)   $$
>                   nest level (xmlRhs lArgs e) $$
>                   text "</rule>"

> xmlLhs :: [Ident] -> Doc
> xmlLhs l  = xmlElement "lhs" xmlVar [0..((length l)-1)]

> xmlRhs :: [Ident] -> Expression -> Doc
> xmlRhs l e = text "<rhs>"  $$ nest level rhs $$ text "</rhs>"
>              where
>                varDicc    = xmlBuildDicc l
>                (rhs, _) = xmlExpr varDicc e

> -- =========================================================================

> -- =========================================================================
> --            E X P R E S S I O N S
> -- =========================================================================

> xmlExpr :: [(Int,Ident)] -> Expression -> (Doc,[(Int,Ident)])
> xmlExpr d (Literal lit)  = (xmlLiteral (xmlLit lit),d)
> xmlExpr d (Variable ident)  = xmlExprVar d ident
> xmlExpr d (Function ident arity)    = (xmlSingleApp ident arity True,d)
> xmlExpr d (Constructor ident arity) = (xmlSingleApp ident arity False,d)
> xmlExpr d exp@(Apply e1 e2)         = xmlApply  d exp (xmlAppArgs exp)
> xmlExpr d (Case _ eval expr alt)      = xmlCase   d eval expr alt
> xmlExpr d (Or expr1 expr2)          = xmlOr     d expr1 expr2
> xmlExpr d (Exist ident expr)        = xmlFree   d ident expr
> xmlExpr d (Let binding expr)        = xmlLet    d binding expr
> xmlExpr d (Letrec lBinding expr)    = xmlLetrec d lBinding expr
>   --error "Recursive let bindings not supported in FlatCurry"

> -- =========================================================================

> xmlSingleApp :: QualIdent -> Int -> Bool -> Doc
> xmlSingleApp ident arity isFunction =
>    if arity>0
>    then xmlCombHeading identDoc (text "PartCall") True
>    else xmlCombHeading identDoc (text totalApp) True
>    where
>       identDoc = xmlQualIdent ident
>       totalApp = if isFunction then "FuncCall" else "ConsCall"


> xmlCombHeading :: Doc -> Doc -> Bool -> Doc
> xmlCombHeading name cType single =
>     if single
>     then prefix <> text " />"
>     else prefix <> text ">"
>     where
>       prefix = text "<comb type=\""<>cType<>text "\" name=\""<>name<>text "\""

> -- =========================================================================

> xmlExprVar :: [(Int,Ident)] -> Ident -> (Doc,[(Int,Ident)])
> xmlExprVar d ident =
>    if isNew
>    then (xmlVar newVar, (newVar,ident):d)
>    else (xmlVar var, d)
>    where
>       var    = xmlLookUp ident d
>       isNew  = var == -1
>       newVar = xmlNewVar d

> -- =========================================================================


> xmlApply :: [(Int,Ident)] -> Expression -> (Expression,[Expression]) ->
>              (Doc,[(Int,Ident)])

> xmlApply d exp ((Function ident arity),lExp) =
>   xmlApplyFunctor d ident arity lExp True

> xmlApply d exp ((Constructor ident arity),lExp) =
>   xmlApplyFunctor d ident arity lExp False

> xmlApply d (Apply expr1 expr2) e' =
>   (text "<apply>" $$ nest level e1 $$ nest level e2 $$ text "</apply>", d2)
>     where
>        (e1,d1) = xmlExpr d  expr1
>        (e2,d2) = xmlExpr d1 expr2

> xmlApplyFunctor ::[(Int,Ident)] -> QualIdent -> Int -> [Expression] ->
>                     Bool -> (Doc,[(Int,Ident)])
> xmlApplyFunctor d ident arity lArgs isFunction =
>    xmlCombApply d (xmlQualIdent ident)  (text cTypeS) n lArgs
>    where
>       n     = length (lArgs)
>       cTypeS = if n==arity
>               then if isFunction
>                    then "FuncCall"
>                    else "ConsCall"
>               else "PartCall"


> xmlCombApply :: [(Int,Ident)] -> Doc -> Doc -> Int ->
>                                 [Expression] -> (Doc,[(Int,Ident)])
> xmlCombApply d name cType 0 lArgs =
>    (xmlCombHeading name cType True,d)
> xmlCombApply d name cType n lArgs =
>    (xmlCombHeading name cType False $$ xmlLines id lDocs$$ text "</comb>", d1)
>    where
>      (lDocs,d1) = xmlMapDicc d xmlExpr lArgs


> xmlAppArgs :: Expression -> (Expression,[Expression])
> xmlAppArgs (Apply e1 e2) = (e,lArgs++[e2])
>                            where
>                                (e,lArgs) = (xmlAppArgs e1)
> xmlAppArgs e             = (e,[])
> -- =========================================================================


> -- =========================================================================

> xmlCase :: [(Int,Ident)] -> Eval -> Expression -> [Alt] -> (Doc,[(Int,Ident)])
> xmlCase d eval expr lAlt =
>   (heading $$ nest level e1 $$ xmlLines id lDocs$$ end,d2)
>   where
>     sEval      = if eval==Rigid then "\"Rigid\"" else "\"Flex\""
>     heading    = text "<case type=" <> text sEval <> text ">"
>     end        = text "</case>"
>     (e1,_)    = xmlExpr d expr
>     (lDocs,d2) = xmlMapDicc d xmlBranch  lAlt

> xmlOr :: [(Int,Ident)] -> Expression -> Expression -> (Doc,[(Int,Ident)])
> xmlOr d  expr1 expr2 =
>    (text "<or>" $$ nest level e1 $$ nest level e2 $$  text "</or>",d2)
>    where
>      (e1,d1) = xmlExpr d expr1
>      (e2,d2) = xmlExpr d1 expr2


> xmlBranch :: [(Int,Ident)] -> Alt -> (Doc,[(Int,Ident)])
> xmlBranch d (Alt pattern expr) =
>    (text "<branch>" $$ nest level e1 $$ nest level e2 $$ text "</branch>",d2)
>    where
>      (e1,d1) = xmlPattern d pattern
>      (e2,d2) = xmlExpr d1 expr


> xmlPattern :: [(Int,Ident)] -> ConstrTerm -> (Doc,[(Int,Ident)])
> xmlPattern d (LiteralPattern lit) = (xmlLitPattern (xmlLit lit),d)
> xmlPattern d (ConstructorPattern ident lArgs) = xmlConsPattern d ident  lArgs
> xmlPattern d (VariablePattern _) = error "Variable patterns not allowed in Flat Curry"

> xmlConsPattern :: [(Int,Ident)] -> QualIdent -> [Ident] -> (Doc,[(Int,Ident)])
> xmlConsPattern d ident lArgs =
>    (heading $$ xmlLines id lDocs $$ end,d2)
>    where
>      heading    = text "<pattern name=\""<> (xmlQualIdent ident) <>
>                   text "\"" <> endh
>      endh       = if (length lArgs)>0 then text ">" else text "/>"
>      end        = if (length lArgs)>0 then text "</pattern>" else empty
>      (lDocs,d2) = xmlMapDicc d xmlExprVar lArgs

> -- =========================================================================


> xmlFree :: [(Int,Ident)] -> Ident -> Expression -> (Doc,[(Int,Ident)])
> xmlFree d ident exp =
>  (text "<freevars>" $$ nest level v $$ nest level e $$ text "</freevars>",d2)
>                    where
>                       (v,d1) = xmlExprVar d  ident
>                       (e,d2) = xmlExpr d1 exp


> -- =========================================================================

> xmlLet :: [(Int,Ident)] -> Binding -> Expression -> (Doc,[(Int,Ident)])
> xmlLet d binding exp =
>   (text "<let>" $$ nest level b $$ nest level e $$ text "</let>", d2)
>   where
>    (b,d1) = xmlBinding d binding
>    (e,d2) = xmlExpr d1 exp

> xmlBinding :: [(Int,Ident)] -> Binding -> (Doc,[(Int,Ident)])
> xmlBinding d  (Binding ident exp) =
>    (text "<binding>" $$ nest level v $$ nest level e $$ text "</binding>",d2)
>    where
>       (v,_)  = xmlExprVar d ident
>       (e,d2) = xmlExpr d exp

> -- =========================================================================

> xmlLetrec :: [(Int,Ident)] -> [Binding] -> Expression -> (Doc,[(Int,Ident)])
> xmlLetrec d lB exp =
>   (text "<letrec>" $$ xmlLines id b $$ nest level e $$ text "</letrec>",d2)
>   where
>     (b,d1) = xmlMapDicc d xmlBinding lB
>     (e,d2) = xmlExpr d1 exp

> -- =========================================================================


> -- =========================================================================
> --            A U X I L I A R Y  F U N C T I O N S
> -- =========================================================================

> splitDecls :: [Decl] -> ([Decl],[Decl])
> splitDecls []     = ([],[])
> splitDecls (x:xs) = case x of
>                      DataDecl     _ _ _   -> (functionDecl,x:typeDecl)
>                      NewtypeDecl  _ _ _   -> (x:functionDecl,typeDecl)
>                      FunctionDecl _ _ _ _ -> (x:functionDecl,typeDecl)
>                      ExternalDecl _ _ _ _   -> (x:functionDecl,typeDecl)
>                   where
>                       (functionDecl,typeDecl) = splitDecls xs




> xmlElement :: Eq a => String -> (a -> Doc) -> [a] -> Doc
> xmlElement name f []     = text ("<"++name++" />")
> xmlElement name f lDecls = beginElement $$ xmlLines f lDecls $$ endElement
>                            where
>                                beginElement = text ("<"++name++">")
>                                endElement   = text ("</"++name++">")
>

> xmlLines :: (a -> Doc) -> [a] -> Doc
> xmlLines f = (nest level).vcat.(map f)


> xmlMapDicc::[(Int,Ident)] -> ([(Int,Ident)] -> a -> (Doc,[(Int,Ident)])) ->
>              [a] -> ([Doc],[(Int,Ident)])
> xmlMapDicc d f lArgs = foldl newArg ([],d) lArgs
>                             where
>                               newArg (l,d)  e = (l++[v'],d')
>                                                 where (v',d') = f d e
>


> -- The dictionary identifies var names with integers
> -- it will be ordered starting at the greatest integer
> xmlBuildDicc :: [Ident] -> [(Int,Ident)]
> xmlBuildDicc l = reverse (zip [0..((length l)-1)] l)

> -- looks for a ident in the dictorionary. If it appears returns its
> -- associated value. Otherwise, -1 is returned
> xmlLookUp :: Ident -> [(Int,Ident)] -> Int
> xmlLookUp ident []          = -1
> xmlLookUp ident ((n,name):xs) = if ident==name
>                                 then n
>                                 else xmlLookUp ident xs

> -- generates a integer corresponding to a new var
> xmlNewVar :: [(Int,Ident)] -> Int
> xmlNewVar []             = 0
> xmlNewVar ((n,ident):xs) = n+1

> xmlVar :: Int -> Doc
> xmlVar n = text "<var>" <> xmlInt n <> text "</var>"

> xmlLiteral :: Doc -> Doc
> xmlLiteral d =   text "<lit>" $$ nest level d $$ text "</lit>"

> xmlLitPattern :: Doc -> Doc
> xmlLitPattern d =   text "<lpattern>" $$ nest level d $$ text "</lpattern>"


> xmlLit :: Literal -> Doc
> xmlLit (Char _ c) = text "<charc>" <>  xmlInt (fromEnum c) <> text "</charc>"
> xmlLit (Int _ n) = text "<intc>" <>  xmlInteger n <> text "</intc>"
> xmlLit (Float _ n) = text "<floatc>" <>  xmlFloat n <> text "</floatc>"

> xmlOperatorDecl :: CS.IDecl -> Doc
> xmlOperatorDecl (CS.IInfixDecl _ fixity prec qident) =
>     text "<op fixity=\"" <> xmlFixity fixity 
>     <> text "\" prec=\"" <> xmlInteger prec <> text "\">"
>     <> xmlIdent (unqualify qident)
>     <> text "</op>"

> xmlFixity :: CS.Infix -> Doc
> xmlFixity CS.InfixL = text "InfixlOp"
> xmlFixity CS.InfixR = text "InfixrOp"
> xmlFixity CS.Infix  = text "InfixOp"


> xmlTranslationDecl :: QualIdent -> Doc
> xmlTranslationDecl expId =
>       text "<trans>" 
>    $$ nest level (   text "<name>"    <> xmlIdent (unqualify expId) <> text "</name>"
>                   $$ text "<intname>" <> xmlQualIdent expId         <> text "</intname>")
>    $$ text "</trans>"


> xmlIdent :: Ident -> Doc
> xmlIdent ident = text (xmlFormat (name ident))

> xmlInt :: Int -> Doc
> xmlInt n = text (show n)

> xmlInteger :: Integer -> Doc
> xmlInteger n = text (show n)

> xmlFloat :: Double -> Doc
> xmlFloat n = text (show n)

> xmlQualIdent :: QualIdent -> Doc
> xmlQualIdent ident = text (xmlFormat (qualName ident))

> xmlModuleIdent:: ModuleIdent -> Doc
> xmlModuleIdent name = text (xmlFormat (moduleName name))

> xmlFormat :: String -> String
> xmlFormat []       = []
> xmlFormat ('>':xs) = "&gt;"++xmlFormat xs
> xmlFormat ('<':xs) = "&lt;"++xmlFormat xs
> xmlFormat ('&':xs) = "&amp;"++xmlFormat xs
> xmlFormat (x:xs)   = x:(xmlFormat xs)

> -- =========================================================================

> qualIDeclId :: ModuleIdent -> [QualIdent] -> CS.IDecl -> [QualIdent]
> qualIDeclId mid qids (CS.IDataDecl _ qid _ mcdecls)
>    = foldl (qualConstrDeclId mid) (qid:qids) (catMaybes mcdecls)
> qualIDeclId mid qids (CS.INewtypeDecl _ qid _ ncdecl)
>    = qualNewConstrDeclId mid (qid:qids) ncdecl
> qualIDeclId mid qids (CS.ITypeDecl _ qid _ _)
>    = qid:qids
> qualIDeclId mid qids (CS.IFunctionDecl _ qid _ _)
>    = qid:qids
> qualIDeclId mid qids _ = qids

> qualConstrDeclId :: ModuleIdent -> [QualIdent] -> CS.ConstrDecl 
>	              -> [QualIdent]
> qualConstrDeclId mid qids (CS.ConstrDecl _ _ id _)
>    = (qualifyWith mid id):qids
> qualConstrDeclId mid qids (CS.ConOpDecl _ _ _ id _)
>    = (qualifyWith mid id):qids

> qualNewConstrDeclId :: ModuleIdent -> [QualIdent] -> CS.NewConstrDecl 
>	                 -> [QualIdent]
> qualNewConstrDeclId mid qids (CS.NewConstrDecl _ _ id _)
>    = (qualifyWith mid id):qids


\end{verbatim}
