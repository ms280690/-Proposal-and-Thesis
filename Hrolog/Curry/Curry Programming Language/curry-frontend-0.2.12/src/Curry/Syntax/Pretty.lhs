
% $Id: CurryPP.lhs,v 1.50 2004/02/15 22:10:27 wlux Exp $
%
% Copyright (c) 1999-2004, Wolfgang Lux
% See LICENSE for the full license.
%
% Modified by Martin Engelke (men@informatik.uni-kiel.de)
%
\nwfilename{CurryPP.lhs}
\section{A Pretty Printer for Curry}\label{sec:CurryPP}
This module implements a pretty printer for Curry expressions. It was
derived from the Haskell pretty printer provided in Simon Marlow's
Haskell parser.
\begin{verbatim}

> module Curry.Syntax.Pretty where

> import Text.PrettyPrint.HughesPJ

> import Curry.Base.Ident
> import Curry.Syntax.Type


\end{verbatim}
Pretty print a module
\begin{verbatim}

> ppModule :: Module -> Doc
> ppModule (Module m es ds) = ppModuleHeader m es $$ ppBlock ds

\end{verbatim}
Module header
\begin{verbatim}

> ppModuleHeader :: ModuleIdent -> Maybe ExportSpec -> Doc
> ppModuleHeader m es =
>   text "module" <+> ppMIdent m <+> maybePP ppExportSpec es <+> text "where"

> ppExportSpec :: ExportSpec -> Doc
> ppExportSpec (Exporting _ es) = parenList (map ppExport es)

> ppExport :: Export -> Doc
> ppExport (Export x) = ppQIdent x
> ppExport (ExportTypeWith tc cs) = ppQIdent tc <> parenList (map ppIdent cs)
> ppExport (ExportTypeAll tc) = ppQIdent tc <> text "(..)"
> ppExport (ExportModule m) = text "module" <+> ppMIdent m

\end{verbatim}
Declarations
\begin{verbatim}

> ppBlock :: [Decl] -> Doc
> ppBlock = vcat . map ppDecl

> ppDecl :: Decl -> Doc
> ppDecl (ImportDecl _ m q asM is) =
>   text "import" <+> ppQualified q <+> ppMIdent m <+> maybePP ppAs asM
>                 <+> maybePP ppImportSpec is
>   where ppQualified q = if q then text "qualified" else empty
>         ppAs m = text "as" <+> ppMIdent m
> ppDecl (InfixDecl _ fix p ops) = ppPrec fix p <+> list (map ppInfixOp ops)
> ppDecl (DataDecl _ tc tvs cs) =
>   sep (ppTypeDeclLhs "data" tc tvs :
>        map indent (zipWith (<+>) (equals : repeat vbar) (map ppConstr cs)))
> ppDecl (NewtypeDecl _ tc tvs nc) =
>   sep [ppTypeDeclLhs "newtype" tc tvs <+> equals,indent (ppNewConstr nc)]
> ppDecl (TypeDecl _ tc tvs ty) =
>   sep [ppTypeDeclLhs "type" tc tvs <+> equals,indent (ppTypeExpr 0 ty)]
> ppDecl (TypeSig _ fs ty) =
>   list (map ppIdent fs) <+> text "::" <+> ppTypeExpr 0 ty
> ppDecl (EvalAnnot _ fs ev) =
>   list (map ppIdent fs) <+> text "eval" <+> ppEval ev
>   where ppEval EvalRigid = text "rigid"
>         ppEval EvalChoice = text "choice"
> ppDecl (FunctionDecl _ _ eqs) = vcat (map ppEquation eqs)
> ppDecl (ExternalDecl p cc impent f ty) =
>   sep [text "external" <+> ppCallConv cc <+> maybePP (text . show) impent,
>        indent (ppDecl (TypeSig p [f] ty))]
>   where ppCallConv CallConvPrimitive = text "primitive"
>         ppCallConv CallConvCCall = text "ccall"
> ppDecl (FlatExternalDecl _ fs) = list (map ppIdent fs) <+> text "external"
> ppDecl (PatternDecl _ t rhs) = ppRule (ppConstrTerm 0 t) equals rhs
> ppDecl (ExtraVariables _ vs) = list (map ppIdent vs) <+> text "free"

> ppImportSpec :: ImportSpec -> Doc
> ppImportSpec (Importing _ is) = parenList (map ppImport is)
> ppImportSpec (Hiding _ is) = text "hiding" <+> parenList (map ppImport is)

> ppImport :: Import -> Doc
> ppImport (Import x) = ppIdent x
> ppImport (ImportTypeWith tc cs) = ppIdent tc <> parenList (map ppIdent cs)
> ppImport (ImportTypeAll tc) = ppIdent tc <> text "(..)"

> ppPrec :: Infix -> Integer -> Doc
> ppPrec fix p = ppAssoc fix <+> ppPrio p
>   where ppAssoc InfixL = text "infixl"
>         ppAssoc InfixR = text "infixr"
>         ppAssoc Infix = text "infix"
>         ppPrio p = if p < 0 then empty else integer p

> ppTypeDeclLhs :: String -> Ident -> [Ident] -> Doc
> ppTypeDeclLhs kw tc tvs = text kw <+> ppIdent tc <+> hsep (map ppIdent tvs)

> ppConstr :: ConstrDecl -> Doc
> ppConstr (ConstrDecl _ tvs c tys) =
>   sep [ppExistVars tvs,ppIdent c <+> fsep (map (ppTypeExpr 2) tys)]
> ppConstr (ConOpDecl _ tvs ty1 op ty2) =
>   sep [ppExistVars tvs,ppTypeExpr 1 ty1,ppInfixOp op <+> ppTypeExpr 1 ty2]

> ppNewConstr :: NewConstrDecl -> Doc
> ppNewConstr (NewConstrDecl _ tvs c ty) =
>   sep [ppExistVars tvs,ppIdent c <+> ppTypeExpr 2 ty]

> ppExistVars :: [Ident] -> Doc
> ppExistVars tvs
>   | null tvs = empty
>   | otherwise = text "forall" <+> hsep (map ppIdent tvs) <+> char '.'

> ppEquation :: Equation -> Doc
> ppEquation (Equation _ lhs rhs) = ppRule (ppLhs lhs) equals rhs

> ppLhs :: Lhs -> Doc
> ppLhs (FunLhs f ts) = ppIdent f <+> fsep (map (ppConstrTerm 2) ts)
> ppLhs (OpLhs t1 f t2) =
>   ppConstrTerm 1 t1 <+> ppInfixOp f <+> ppConstrTerm 1 t2
> ppLhs (ApLhs lhs ts) = parens (ppLhs lhs) <+> fsep (map (ppConstrTerm 2) ts)

> ppRule :: Doc -> Doc -> Rhs -> Doc
> ppRule lhs eq (SimpleRhs _ e ds) =
>   sep [lhs <+> eq,indent (ppExpr 0 e)] $$ ppLocalDefs ds
> ppRule lhs eq (GuardedRhs es ds) =
>   sep [lhs,indent (vcat (map (ppCondExpr eq) es))] $$ ppLocalDefs ds

> ppLocalDefs :: [Decl] -> Doc
> ppLocalDefs ds
>   | null ds = empty
>   | otherwise = indent (text "where" <+> ppBlock ds)

\end{verbatim}
Interfaces
\begin{verbatim}

> ppInterface :: Interface -> Doc
> ppInterface (Interface m ds) =
>   text "interface" <+> ppMIdent m <+> text "where" <+> lbrace
>     $$ vcat (punctuate semi (map ppIDecl ds)) $$ rbrace

> ppIDecl :: IDecl -> Doc
> ppIDecl (IImportDecl _ m) = text "import" <+> ppMIdent m
> ppIDecl (IInfixDecl _ fix p op) = ppPrec fix p <+> ppQInfixOp op
> ppIDecl (HidingDataDecl _ tc tvs) =
>   text "hiding" <+> ppITypeDeclLhs "data" (qualify tc) tvs
> ppIDecl (IDataDecl _ tc tvs cs) =
>   sep (ppITypeDeclLhs "data" tc tvs :
>        map indent (zipWith (<+>) (equals : repeat vbar) (map ppIConstr cs)))
>   where ppIConstr = maybe (char '_') ppConstr
> ppIDecl (INewtypeDecl _ tc tvs nc) =
>   sep [ppITypeDeclLhs "newtype" tc tvs <+> equals,indent (ppNewConstr nc)]
> ppIDecl (ITypeDecl _ tc tvs ty) =
>   sep [ppITypeDeclLhs "type" tc tvs <+> equals,indent (ppTypeExpr 0 ty)]
> ppIDecl (IFunctionDecl _ f _ ty) = ppQIdent f <+> text "::" <+> ppTypeExpr 0 ty

> ppITypeDeclLhs :: String -> QualIdent -> [Ident] -> Doc
> ppITypeDeclLhs kw tc tvs = text kw <+> ppQIdent tc <+> hsep (map ppIdent tvs)

\end{verbatim}
Types
\begin{verbatim}

> ppTypeExpr :: Int -> TypeExpr -> Doc
> ppTypeExpr p (ConstructorType tc tys) =
>   parenExp (p > 1 && not (null tys))
>            (ppQIdent tc <+> fsep (map (ppTypeExpr 2) tys))
> ppTypeExpr _ (VariableType tv) = ppIdent tv
> ppTypeExpr _ (TupleType tys) = parenList (map (ppTypeExpr 0) tys)
> ppTypeExpr _ (ListType ty) = brackets (ppTypeExpr 0 ty)
> ppTypeExpr p (ArrowType ty1 ty2) =
>   parenExp (p > 0) (fsep (ppArrowType (ArrowType ty1 ty2)))
>   where ppArrowType (ArrowType ty1 ty2) =
>           ppTypeExpr 1 ty1 <+> rarrow : ppArrowType ty2
>         ppArrowType ty = [ppTypeExpr 0 ty]
> ppTypeExpr p (RecordType fs rty) = 
>   braces (list (map ppTypedField fs) 
>           <> maybe empty (\ty -> space <> char '|' <+> ppTypeExpr 0 ty) rty)
>   where
>   ppTypedField (ls,ty) = 
>     list (map ppIdent ls) <> text "::" <> ppTypeExpr 0 ty



\end{verbatim}
Literals
\begin{verbatim}

> ppLiteral :: Literal -> Doc
> ppLiteral (Char _ c)   = text (show c)
> ppLiteral (Int _ i)    = integer i
> ppLiteral (Float _ f)  = double f
> ppLiteral (String _ s) = text (show s)

\end{verbatim}
Patterns
\begin{verbatim}

> ppConstrTerm :: Int -> ConstrTerm -> Doc
> ppConstrTerm p (LiteralPattern l) =
>   parenExp (p > 1 && isNegative l) (ppLiteral l)
>   where isNegative (Char _ _)   = False
>         isNegative (Int _ i)    = i < 0
>         isNegative (Float _ f)  = f < 0.0
>         isNegative (String _ _) = False
> ppConstrTerm p (NegativePattern op l) =
>   parenExp (p > 1) (ppInfixOp op <> ppLiteral l)
> ppConstrTerm _ (VariablePattern v) = ppIdent v
> ppConstrTerm p (ConstructorPattern c ts) =
>   parenExp (p > 1 && not (null ts))
>            (ppQIdent c <+> fsep (map (ppConstrTerm 2) ts))
> ppConstrTerm p (InfixPattern t1 c t2) =
>   parenExp (p > 0)
>            (sep [ppConstrTerm 1 t1 <+> ppQInfixOp c,
>                  indent (ppConstrTerm 0 t2)])
> ppConstrTerm _ (ParenPattern t) = parens (ppConstrTerm 0 t)
> ppConstrTerm _ (TuplePattern _ ts) = parenList (map (ppConstrTerm 0) ts)
> ppConstrTerm _ (ListPattern _ ts) = bracketList (map (ppConstrTerm 0) ts)
> ppConstrTerm _ (AsPattern v t) = ppIdent v <> char '@' <> ppConstrTerm 2 t
> ppConstrTerm _ (LazyPattern _ t) = char '~' <> ppConstrTerm 2 t
> ppConstrTerm p (FunctionPattern f ts) =
>   parenExp (p > 1 && not (null ts))
>            (ppQIdent f <+> fsep (map (ppConstrTerm 2) ts))
> ppConstrTerm p (InfixFuncPattern t1 f t2) =
>   parenExp (p > 0)
>            (sep [ppConstrTerm 1 t1 <+> ppQInfixOp f,
>                  indent (ppConstrTerm 0 t2)])
> ppConstrTerm p (RecordPattern fs rt) =
>   braces (list (map ppFieldPatt fs)
>          <> (maybe empty (\t -> space <> char '|' <+> ppConstrTerm 0 t) rt))

> ppFieldPatt :: Field ConstrTerm -> Doc
> ppFieldPatt (Field _ l t) = ppIdent l <> equals <> ppConstrTerm 0 t

\end{verbatim}
Expressions
\begin{verbatim}

> ppCondExpr :: Doc -> CondExpr -> Doc
> ppCondExpr eq (CondExpr _ g e) =
>   vbar <+> sep [ppExpr 0 g <+> eq,indent (ppExpr 0 e)]

> ppExpr :: Int -> Expression -> Doc
> ppExpr _ (Literal l) = ppLiteral l
> ppExpr _ (Variable v) = ppQIdent v
> ppExpr _ (Constructor c) = ppQIdent c
> ppExpr _ (Paren e) = parens (ppExpr 0 e)
> ppExpr p (Typed e ty) =
>   parenExp (p > 0) (ppExpr 0 e <+> text "::" <+> ppTypeExpr 0 ty)
> ppExpr _ (Tuple _ es) = parenList (map (ppExpr 0) es)
> ppExpr _ (List _ es) = bracketList (map (ppExpr 0) es)
> ppExpr _ (ListCompr _ e qs) =
>   brackets (ppExpr 0 e <+> vbar <+> list (map ppStmt qs))
> ppExpr _ (EnumFrom e) = brackets (ppExpr 0 e <+> text "..")
> ppExpr _ (EnumFromThen e1 e2) =
>   brackets (ppExpr 0 e1 <> comma <+> ppExpr 0 e2 <+> text "..")
> ppExpr _ (EnumFromTo e1 e2) =
>   brackets (ppExpr 0 e1 <+> text ".." <+> ppExpr 0 e2)
> ppExpr _ (EnumFromThenTo e1 e2 e3) =
>   brackets (ppExpr 0 e1 <> comma <+> ppExpr 0 e2
>               <+> text ".." <+> ppExpr 0 e3)
> ppExpr p (UnaryMinus op e) = parenExp (p > 1) (ppInfixOp op <> ppExpr 1 e)
> ppExpr p (Apply e1 e2) =
>   parenExp (p > 1) (sep [ppExpr 1 e1,indent (ppExpr 2 e2)])
> ppExpr p (InfixApply e1 op e2) =
>   parenExp (p > 0) (sep [ppExpr 1 e1 <+> ppQInfixOp (opName op),
>                          indent (ppExpr 1 e2)])
> ppExpr _ (LeftSection e op) = parens (ppExpr 1 e <+> ppQInfixOp (opName op))
> ppExpr _ (RightSection op e) = parens (ppQInfixOp (opName op) <+> ppExpr 1 e)
> ppExpr p (Lambda _ t e) =
>   parenExp (p > 0)
>            (sep [backsl <> fsep (map (ppConstrTerm 2) t) <+> rarrow,
>                  indent (ppExpr 0 e)])
> ppExpr p (Let ds e) =
>   parenExp (p > 0)
>            (sep [text "let" <+> ppBlock ds <+> text "in",ppExpr 0 e])
> ppExpr p (Do sts e) =
>   parenExp (p > 0) (text "do" <+> (vcat (map ppStmt sts) $$ ppExpr 0 e))
> ppExpr p (IfThenElse _ e1 e2 e3) =
>   parenExp (p > 0)
>            (text "if" <+>
>             sep [ppExpr 0 e1,
>                  text "then" <+> ppExpr 0 e2,
>                  text "else" <+> ppExpr 0 e3])
> ppExpr p (Case _ e alts) =
>   parenExp (p > 0)
>            (text "case" <+> ppExpr 0 e <+> text "of" $$
>             indent (vcat (map ppAlt alts)))
> ppExpr p (RecordConstr fs) =
>   braces (list (map (ppFieldExpr equals) fs))
> ppExpr p (RecordSelection e l) =
>   parenExp (p > 0)
>            (ppExpr 1 e <+> text "->" <+> ppIdent l)
> ppExpr p (RecordUpdate fs e) =
>   braces (list (map (ppFieldExpr (text ":=")) fs)
>          <+> char '|' <+> ppExpr 0 e)

> ppStmt :: Statement -> Doc
> ppStmt (StmtExpr _ e) = ppExpr 0 e
> ppStmt (StmtBind _ t e) = sep [ppConstrTerm 0 t <+> larrow,indent (ppExpr 0 e)]
> ppStmt (StmtDecl ds) = text "let" <+> ppBlock ds

> ppAlt :: Alt -> Doc
> ppAlt (Alt _ t rhs) = ppRule (ppConstrTerm 0 t) rarrow rhs

> ppFieldExpr :: Doc -> Field Expression -> Doc
> ppFieldExpr comb (Field _ l e) = ppIdent l <> comb <> ppExpr 0 e

> ppOp :: InfixOp -> Doc
> ppOp (InfixOp op) = ppQInfixOp op
> ppOp (InfixConstr op) = ppQInfixOp op

\end{verbatim}

Names
\begin{verbatim}

> ppIdent :: Ident -> Doc
> ppIdent x = parenExp (isInfixOp x) (text (name x))

> ppQIdent :: QualIdent -> Doc
> ppQIdent x = parenExp (isQInfixOp x) (text (qualName x))

> ppInfixOp :: Ident -> Doc
> ppInfixOp x = backQuoteExp (not (isInfixOp x)) (text (name x))

> ppQInfixOp :: QualIdent -> Doc
> ppQInfixOp x = backQuoteExp (not (isQInfixOp x)) (text (qualName x))

> ppMIdent :: ModuleIdent -> Doc
> ppMIdent m = text (moduleName m)

\end{verbatim}
Print printing utilities
\begin{verbatim}

> indent :: Doc -> Doc
> indent = nest 2

> maybePP :: (a -> Doc) -> Maybe a -> Doc
> maybePP pp = maybe empty pp

> parenExp :: Bool -> Doc -> Doc
> parenExp b doc = if b then parens doc else doc

> backQuoteExp :: Bool -> Doc -> Doc
> backQuoteExp b doc = if b then backQuote <> doc <> backQuote else doc

> list, parenList, bracketList, braceList :: [Doc] -> Doc
> list = fsep . punctuate comma
> parenList = parens . list
> bracketList = brackets . list
> braceList = braces . list

> backQuote,backsl,vbar,rarrow,larrow :: Doc
> backQuote = char '`'
> backsl = char '\\'
> vbar = char '|'
> rarrow = text "->"
> larrow = text "<-"

\end{verbatim}
