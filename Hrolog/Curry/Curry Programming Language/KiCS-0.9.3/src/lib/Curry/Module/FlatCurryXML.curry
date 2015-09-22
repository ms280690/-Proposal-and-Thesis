------------------------------------------------------------------------------
--- This library contains functions to convert FlatCurry programs
--- into corresponding XML expressions and vice versa.
--- This can be used to store Curry programs in a way independent
--- from PAKCS or to use the PAKCS back end by other systems.
---
--- The library provides the following functions:
--- <PRE>
--- "flatCurry2XmlFile": transform a FlatCurry program into XML file
--- "flatCurry2Xml":     transform a FlatCurry program into XML expression
--- "xmlFile2FlatCurry": read an XML file and return the FlatCurry program
--- "xml2FlatCurry":     transform an XML expression into a FlatCurry program
--- </PRE>
---
--- @author Michael Hanus
--- @version August 2005
------------------------------------------------------------------------------

module FlatCurryXML(flatCurry2XmlFile,flatCurry2Xml,
                    xmlFile2FlatCurry,xml2FlatCurry)  where

import FlatCurry
import XML
import Read

-- URL for the FlatCurry DTD:
flatCurryDtd = "http://www.informatik.uni-kiel.de/~curry/flatcurrynew.dtd"

------------------------------------------------------------------------------
-- Transforming FlatCurry programs into corresponding XML terms:
------------------------------------------------------------------------------

--- Transforms a FlatCurry program term into a corresponding XML file.
flatCurry2XmlFile :: Prog -> String -> IO ()
flatCurry2XmlFile flatprog filename = writeFile filename
  (showXmlDocWithParams [DtdUrl flatCurryDtd] (flatCurry2Xml flatprog))

--- Transforms a FlatCurry program term into a corresponding XML expression.
flatCurry2Xml :: Prog -> XmlExp
flatCurry2Xml (Prog modname imports types funcs ops) =
  xml "prog"
   [xml "module" [xtxt modname],
    xml "import" (map (\s->xml "module" [xtxt s]) imports),
    xml "types" (map xmlShowType types),
    xml "functions" (map xmlShowFunc funcs),
    xml "operators" (map xmlShowOp ops)]

qname2xmlattrs (mod,name) = [("module",mod),("name",name)]

xmlShowVisibity Public  = [("visibility","public")]
xmlShowVisibity Private = [("visibility","private")]

xmlShowType (Type name vis tpars consdecls) =
  XElem "type" (qname2xmlattrs name ++ xmlShowVisibity vis)
        ([xml "params" (map xmlShowTVar tpars)] ++ map xmlShowCons consdecls)
xmlShowType (TypeSyn name vis tpars texp) =
  XElem "typesyn" (qname2xmlattrs name ++ xmlShowVisibity vis)
        [xml "params" (map xmlShowTVar tpars),xmlShowTypeExpr texp]

xmlShowCons (Cons cname arity vis types) =
  XElem "cons" (qname2xmlattrs cname ++ [("arity",show arity)] ++ xmlShowVisibity vis)
        (map xmlShowTypeExpr types)

xmlShowTypeExpr (FuncType t1 t2) =
  xml "functype" [xmlShowTypeExpr t1,xmlShowTypeExpr t2]
xmlShowTypeExpr (TCons tc ts) =
  XElem "tcons" (qname2xmlattrs tc) (map xmlShowTypeExpr ts)
xmlShowTypeExpr (TVar n) = xmlShowTVar n

xmlShowTVar i = xml "tvar" [xtxt (show i)]

xmlShowFunc (Func name arity vis ftype rl) =
  XElem "func" (qname2xmlattrs name ++ [("arity",show arity)] ++ xmlShowVisibity vis)
        [xmlShowTypeExpr ftype,xmlShowRule rl]

xmlShowRule (Rule params expr) =
  xml "rule" [xml "lhs" (map xmlShowVar params),
              xml "rhs" [xmlShowExpr expr]]
xmlShowRule (External name) = xml "external" [xtxt name]

xmlShowVar i = xml "var" [xtxt (show i)]

xmlShowExpr (Var n) = xmlShowVar n
xmlShowExpr (Lit l) = xml "lit" [xmlShowLit l]
xmlShowExpr (Comb FuncCall cf es) =
  XElem "funccall" (qname2xmlattrs cf) (map xmlShowExpr es)
xmlShowExpr (Comb ConsCall cf es) =
  XElem "conscall" (qname2xmlattrs cf) (map xmlShowExpr es)
xmlShowExpr (Comb (FuncPartCall n) cf es) =
  XElem "funcpartcall" (qname2xmlattrs cf ++ [("missing",show n)]) (map xmlShowExpr es)
xmlShowExpr (Comb (ConsPartCall n) cf es) =
  XElem "conspartcall" (qname2xmlattrs cf ++ [("missing",show n)]) (map xmlShowExpr es)
xmlShowExpr (Free xs e) =
  xml "free" [xml "freevars" (map xmlShowVar xs), xmlShowExpr e]
xmlShowExpr (Or e1 e2) =
  xml "or" [xmlShowExpr e1,xmlShowExpr e2]
xmlShowExpr (Case ctype e cs) =
  XElem (if ctype==Flex then "fcase" else "case") []
        ([xmlShowExpr e] ++ map xmlShowBranch cs)
xmlShowExpr (Let bindings expr) =
  xml "letrec" (map (\(i,e)->xml "binding" [xmlShowVar i, xmlShowExpr e]) bindings
                ++ [xmlShowExpr expr])

xmlShowLit (Intc   i) = xml "intc"   [xtxt (show i)]
xmlShowLit (Floatc f) = xml "floatc" [xtxt (show f)]
xmlShowLit (Charc  c) = xml "charc"  [xtxt (show (ord c))]

xmlShowBranch (Branch (Pattern cons xs) e) =
  xml "branch" [XElem "pattern" (qname2xmlattrs cons) (map xmlShowVar xs),
                xmlShowExpr e]
xmlShowBranch (Branch (LPattern lit) e) =
  xml "branch" [xml "lpattern" [xmlShowLit lit], xmlShowExpr e]

xmlShowOp (Op name fix prec) =
  XElem "op" (qname2xmlattrs name ++ [("fixity",show fix),("prec",show prec)]) []


------------------------------------------------------------------------------
-- Transforming XML terms into corresponding FlatCurry programs:
------------------------------------------------------------------------------

--- Reads an XML file with a FlatCurry program and returns
--- the FlatCurry program.
xmlFile2FlatCurry :: String -> IO Prog
xmlFile2FlatCurry filename =
  do xexp <- readXmlFile filename
     return (xml2FlatCurry xexp)

--- Transforms an XML term into a FlatCurry program.
xml2FlatCurry :: XmlExp -> Prog
xml2FlatCurry
 (XElem "prog" []
   [XElem "module"      [] xmodname,
    XElem "import"      [] ximports,
    XElem "types"       [] xtypes,
    XElem "functions"   [] xfunctions,
    XElem "operators"   [] xoperators]) =
  Prog (textOfXml xmodname)
       (map (\(XElem "module" [] xim) -> textOfXml xim) ximports)
       (map flatx2typedecl xtypes)
       (map (\(XElem "func" [("module",mod),("name",fname),("arity",farity),xvis]
                            [xftype,xfbody])
             -> Func (mod,fname) (readNat farity) (xvis2vis xvis) (flatx2texp xftype)
                     (flatx2FunBody xfbody))
            xfunctions)
       (map (\(XElem "op"
                     [("module",mod),("name",name),("fixity",xfix),("prec",xprec)] [])
             -> Op (mod,name) (flatx2Fixity xfix) (readNat xprec))
            xoperators)

flatx2typedecl (XElem "type" [("module",tmod),("name",tname),xtvis]
                      (XElem "params" [] xtvars : xconstructors)) =
  Type (tmod,tname) (xvis2vis xtvis)
       (map (\ (XElem "tvar" [] xtvar) -> readNat (textOfXml xtvar)) xtvars)
       (map (\ (XElem "cons" [("module",mod),("name",xcn),("arity",xar),xvis] xtexps)
              -> Cons (mod,xcn) (readNat xar) (xvis2vis xvis) (map flatx2texp xtexps))
            xconstructors)
flatx2typedecl (XElem "typesyn" [("module",tmod),("name",tname),xtvis]
                      [XElem "params" [] xtvars, xtexp]) =
  TypeSyn (tmod,tname) (xvis2vis xtvis)
          (map (\ (XElem "tvar" [] xtvar) -> readNat (textOfXml xtvar)) xtvars)
          (flatx2texp xtexp)

flatx2FunBody (XElem "external" [] xename) = External (textOfXml xename)
flatx2FunBody (XElem "rule" [] [XElem "lhs" [] xvars,
                                XElem "rhs" [] [xrhs]]) =
   Rule (map flatx2var xvars) (flatx2exp xrhs)

flatx2var :: XmlExp -> VarIndex
flatx2var (XElem "var" [] xvar) = readNat (textOfXml xvar)

flatx2exp :: XmlExp -> Expr
flatx2exp (XElem "var" [] xvar) = Var (readNat (textOfXml xvar))
flatx2exp (XElem "lit" [] [xlit]) = Lit (flatx2lit xlit)
flatx2exp (XElem "funccall" [("module",mod),("name",name)] xexps) =
  Comb FuncCall (mod,name) (map flatx2exp xexps)
flatx2exp (XElem "conscall" [("module",mod),("name",name)] xexps) =
  Comb ConsCall (mod,name) (map flatx2exp xexps)
flatx2exp (XElem "funcpartcall" [("module",mod),("name",name),("missing",nmiss)] xexps) =
  Comb (FuncPartCall (readNat nmiss)) (mod,name) (map flatx2exp xexps)
flatx2exp (XElem "conspartcall" [("module",mod),("name",name),("missing",nmiss)] xexps) =
  Comb (ConsPartCall (readNat nmiss)) (mod,name) (map flatx2exp xexps)
flatx2exp (XElem "free" [] [XElem "freevars" [] xvars, xexp]) =
  Free (map flatx2var xvars) (flatx2exp xexp)
flatx2exp (XElem "or" [] [xexp1,xexp2]) =
  Or (flatx2exp xexp1) (flatx2exp xexp2)
flatx2exp (XElem "case" [] (xexp : xbranches)) =
  Case Rigid (flatx2exp xexp) (map flatx2branch xbranches)
flatx2exp (XElem "fcase" [] (xexp : xbranches)) =
  Case Flex (flatx2exp xexp) (map flatx2branch xbranches)
flatx2exp (XElem "let" [] xbindings) =
  let (bindings,exp) = flatx2let xbindings
   in Let bindings exp
flatx2exp (XElem "letrec" [] xbindings) =
  let (bindings,exp) = flatx2let xbindings
   in Let bindings exp

flatx2let [xexp] = ([],flatx2exp xexp)
flatx2let (XElem "binding" [] [XElem "var" [] xvar, xexp] : xb:xbs) =
  let (bindings,exp) = flatx2let (xb:xbs)
   in ((readNat (textOfXml xvar), flatx2exp xexp) : bindings, exp)

flatx2branch (XElem "branch" [] [XElem "pattern" [("module",mod),("name",cons)] xvars,xexp]) =
  Branch (Pattern (mod,cons) (map flatx2var xvars)) (flatx2exp xexp)
flatx2branch (XElem "branch" [] [XElem "lpattern" [] [xlit], xexp]) =
  Branch (LPattern (flatx2lit xlit)) (flatx2exp xexp)
flatx2branch (XElem "branch" [] [XElem "hpattern" _ _,_]) =
  error "Higher-order patterns not supported in this version of FlatCurry!"

flatx2lit :: XmlExp -> Literal
flatx2lit (XElem "intc" [] xintc) = Intc (readNat (textOfXml xintc))
flatx2lit (XElem "floatc" [] _) =
            error "Reading of floats not yet implemented!"
flatx2lit (XElem "charc" [] xintc) = Charc (chr (readNat (textOfXml xintc)))

flatx2texp :: XmlExp -> TypeExpr
flatx2texp (XElem "tvar" [] xtvar) = TVar (readNat (textOfXml xtvar))
flatx2texp (XElem "functype" [] [xtexp1,xtexp2]) =
   FuncType (flatx2texp xtexp1) (flatx2texp xtexp2)
flatx2texp (XElem "tcons" [("module",mod),("name",tcname)] xtexps) =
   TCons (mod,tcname) (map flatx2texp xtexps)

xvis2vis ("visibility","public")  = Public
xvis2vis ("visibility","private") = Private

flatx2Fixity "InfixOp"  = InfixOp
flatx2Fixity "InfixlOp" = InfixlOp
flatx2Fixity "InfixrOp" = InfixrOp


------------------------------------------------------------------------------
