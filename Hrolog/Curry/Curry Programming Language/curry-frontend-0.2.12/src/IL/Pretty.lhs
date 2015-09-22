% $Id: ILPP.lhs,v 1.22 2003/10/28 05:43:43 wlux Exp $
%
% Copyright (c) 1999-2003 Wolfgang Lux
% See LICENSE for the full license.
%
% Modified by Martin Engelke (men@informatik.uni-kiel.de)
%
\nwfilename{ILPP.lhs}
\section{A pretty printer for the intermediate language}
This module implements just another pretty printer, this time for the
intermediate language. It was mainly adapted from the Curry pretty
printer (see sect.~\ref{sec:CurryPP}) which, in turn, is based on Simon
Marlow's pretty printer for Haskell.
\begin{verbatim}

> module IL.Pretty(ppModule)  where
> 
> import Text.PrettyPrint.HughesPJ

> import Curry.Base.Ident
> import IL.Type

> default(Int,Double)

> dataIndent = 2
> bodyIndent = 2
> exprIndent = 2
> caseIndent = 2
> altIndent = 2

> ppModule :: Module -> Doc
> ppModule (Module m is ds) =
>   vcat (text "module" <+> text (show m) <+> text "where" :
>         map ppImport is ++ map ppDecl ds)

> ppImport :: ModuleIdent -> Doc
> ppImport m = text "import" <+> text (show m)

> ppDecl :: Decl -> Doc
> ppDecl (DataDecl tc n cs) =
>   sep (text "data" <+> ppTypeLhs tc n :
>        map (nest dataIndent)
>            (zipWith (<+>) (equals : repeat (char '|')) (map ppConstr cs)))
> ppDecl (NewtypeDecl tc n (ConstrDecl c ty)) =
>   sep [text "newtype" <+> ppTypeLhs tc n <+> equals,
>        nest dataIndent (ppConstr (ConstrDecl c [ty]))]
> ppDecl (FunctionDecl f vs ty exp) =
>   ppTypeSig f ty $$
>   sep [ppQIdent f <+> hsep (map ppIdent vs) <+> equals,
>        nest bodyIndent (ppExpr 0 exp)]
> ppDecl (ExternalDecl f cc ie ty) =
>   sep [text "external" <+> ppCallConv cc <+> text (show ie),
>        nest bodyIndent (ppTypeSig f ty)]
>   where ppCallConv Primitive = text "primitive"
>         ppCallConv CCall = text "ccall"

> ppTypeLhs :: QualIdent -> Int -> Doc
> ppTypeLhs tc n = ppQIdent tc <+> hsep (map text (take n typeVars))

> ppConstr :: ConstrDecl [Type] -> Doc
> ppConstr (ConstrDecl c tys) = ppQIdent c <+> fsep (map (ppType 2) tys)

> ppTypeSig :: QualIdent -> Type -> Doc
> ppTypeSig f ty = ppQIdent f <+> text "::" <+> ppType 0 ty

> ppType :: Int -> Type -> Doc
> ppType p (TypeConstructor tc tys)
>   | isQTupleId tc = parens (fsep (punctuate comma (map (ppType 0) tys)))
>   | unqualify tc == nilId = brackets (ppType 0 (head tys))
>   | otherwise =
>       ppParen (p > 1 && not (null tys))
>               (ppQIdent tc <+> fsep (map (ppType 2) tys))
> ppType _ (TypeVariable n)
>   | n >= 0 = text (typeVars !! n)
>   | otherwise = text ('_':show (-n))
> ppType p (TypeArrow ty1 ty2) =
>   ppParen (p > 0) (fsep (ppArrow (TypeArrow ty1 ty2)))
>   where ppArrow (TypeArrow ty1 ty2) =
>           ppType 1 ty1 <+> text "->" : ppArrow ty2
>         ppArrow ty = [ppType 0 ty]

> ppBinding :: Binding -> Doc
> ppBinding (Binding v exp) =
>   sep [ppIdent v <+> equals,nest bodyIndent (ppExpr 0 exp)]

> ppAlt :: Alt -> Doc
> ppAlt (Alt pat exp) =
>   sep [ppConstrTerm pat <+> text "->",nest altIndent (ppExpr 0 exp)]

> ppLiteral :: Literal -> Doc
> ppLiteral (Char _ c) = text (show c)
> ppLiteral (Int _ i) = integer i
> ppLiteral (Float _ f) = double f

> ppConstrTerm :: ConstrTerm -> Doc
> ppConstrTerm (LiteralPattern l) = ppLiteral l
> ppConstrTerm (ConstructorPattern c [v1,v2])
>   | isQInfixOp c = ppIdent v1 <+> ppQInfixOp c <+> ppIdent v2
> ppConstrTerm (ConstructorPattern c vs)
>   | isQTupleId c = parens (fsep (punctuate comma (map ppIdent vs)))
>   | otherwise = ppQIdent c <+> fsep (map ppIdent vs)
> ppConstrTerm (VariablePattern v) = ppIdent v

> ppExpr :: Int -> Expression -> Doc
> ppExpr p (Literal l) = ppLiteral l
> ppExpr p (Variable v) = ppIdent v
> ppExpr p (Function f _) = ppQIdent f
> ppExpr p (Constructor c _) = ppQIdent c
> ppExpr p (Apply (Apply (Function f _) e1) e2)
>   | isQInfixOp f = ppInfixApp p e1 f e2
> ppExpr p (Apply (Apply (Constructor c _) e1) e2)
>   | isQInfixOp c = ppInfixApp p e1 c e2
> ppExpr p (Apply e1 e2) =
>   ppParen (p > 2) (sep [ppExpr 2 e1,nest exprIndent (ppExpr 3 e2)])
> ppExpr p (Case _ ev e alts) =
>   ppParen (p > 0)
>           (text "case" <+> ppEval ev <+> ppExpr 0 e <+> text "of" $$
>            nest caseIndent (vcat (map ppAlt alts)))
>   where ppEval Rigid = text "rigid"
>         ppEval Flex = text "flex"
> ppExpr p (Or e1 e2) =
>   ppParen (p > 0) (sep [ppExpr 0 e1,char '|' <+> ppExpr 0 e2])
> ppExpr p (Exist v e) =
>   ppParen (p > 0)
>           (sep [text "let" <+> ppIdent v <+> text "free" <+> text "in",
>                 ppExpr 0 e])
> ppExpr p (Let b e) =
>   ppParen (p > 0) (sep [text "let" <+> ppBinding b <+> text "in",ppExpr 0 e])
> ppExpr p (Letrec bs e) =
>   ppParen (p > 0)
>           (sep [text "letrec" <+> vcat (map ppBinding bs) <+> text "in",
>                 ppExpr 0 e])

> ppInfixApp :: Int -> Expression -> QualIdent -> Expression -> Doc
> ppInfixApp p e1 op e2 =
>   ppParen (p > 1)
>           (sep [ppExpr 2 e1 <+> ppQInfixOp op,nest exprIndent (ppExpr 2 e2)])

> ppIdent :: Ident -> Doc
> ppIdent ident
>   | isInfixOp ident = parens (ppName ident)
>   | otherwise = ppName ident

> ppQIdent :: QualIdent -> Doc
> ppQIdent ident
>   | isQInfixOp ident = parens (ppQual ident)
>   | otherwise = ppQual ident

> ppQInfixOp :: QualIdent -> Doc
> ppQInfixOp op
>   | isQInfixOp op = ppQual op
>   | otherwise = char '`' <> ppQual op <> char '`'

> ppName :: Ident -> Doc
> ppName x = text (name x)

> ppQual :: QualIdent -> Doc
> ppQual x = text (qualName x)

> typeVars :: [String]
> typeVars = [mkTypeVar c i | i <- [0..], c <- ['a' .. 'z']]
>   where mkTypeVar c i = c : if i == 0 then [] else show i

> ppParen :: Bool -> Doc -> Doc
> ppParen p = if p then parens else id

\end{verbatim}
