Module "Read"
(Just (Exporting (8,12) [(Export (QualIdent Nothing (Ident "readNat" 0))),(Export (QualIdent Nothing (Ident "readInt" 0))),(Export (QualIdent Nothing (Ident "readHex" 0)))]))
[(ImportDecl (1,1) "Prelude" False Nothing Nothing)
,(ImportDecl (10,1) "Char" False Nothing Nothing)
,(TypeSig (15,1) [(Ident "readNat" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (ConstructorType (QualIdent Nothing (Ident "Int" 0)) [])))
,(FunctionDecl (16,1) (Ident "readNat" 0) [(Equation (16,1) (FunLhs (Ident "readNat" 0) [(VariablePattern (Ident "l" 2))]) (SimpleRhs (16,13) (Apply (Apply (Variable (QualIdent Nothing (Ident "readNatPrefix" 3))) (Paren (Apply (Apply (Variable (QualIdent (Just "Prelude") (Ident "dropWhile" 0))) (Paren (Lambda [(VariablePattern (Ident "c" 11))] (InfixApply (Variable (QualIdent Nothing (Ident "c" 11))) (InfixOp (QualIdent (Just "Prelude") (Ident "==" 0))) (Literal (Char ' ')))))) (Variable (QualIdent Nothing (Ident "l" 2)))))) (Literal (Int (Ident "_" 12) 0))) [(FunctionDecl (18,3) (Ident "readNatPrefix" 3) [(Equation (18,3) (FunLhs (Ident "readNatPrefix" 3) [(ListPattern []),(VariablePattern (Ident "n" 4))]) (SimpleRhs (18,24) (Variable (QualIdent Nothing (Ident "n" 4))) [])),(Equation (19,3) (FunLhs (Ident "readNatPrefix" 3) [(ParenPattern (InfixPattern (VariablePattern (Ident "c" 6)) (QualIdent Nothing (Ident ":" 0)) (VariablePattern (Ident "cs" 6)))),(VariablePattern (Ident "n" 6))]) (SimpleRhs (20,4) (Let [(PatternDecl (20,8) (VariablePattern (Ident "oc" 8)) (SimpleRhs (20,13) (Apply (Variable (QualIdent (Just "Prelude") (Ident "ord" 0))) (Variable (QualIdent Nothing (Ident "c" 6)))) []))] (IfThenElse (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "oc" 8))) (InfixOp (QualIdent (Just "Prelude") (Ident ">=" 0))) (Apply (Variable (QualIdent (Just "Prelude") (Ident "ord" 0))) (Literal (Char '0')))) (InfixOp (QualIdent (Just "Prelude") (Ident "&&" 0))) (InfixApply (Variable (QualIdent Nothing (Ident "oc" 8))) (InfixOp (QualIdent (Just "Prelude") (Ident "<=" 0))) (Apply (Variable (QualIdent (Just "Prelude") (Ident "ord" 0))) (Literal (Char '9'))))) (Apply (Apply (Variable (QualIdent Nothing (Ident "readNatPrefix" 3))) (Variable (QualIdent Nothing (Ident "cs" 6)))) (Paren (InfixApply (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "n" 6))) (InfixOp (QualIdent (Just "Prelude") (Ident "*" 0))) (Literal (Int (Ident "_" 10) 10))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (Variable (QualIdent Nothing (Ident "oc" 8)))) (InfixOp (QualIdent (Just "Prelude") (Ident "-" 0))) (Paren (Apply (Variable (QualIdent (Just "Prelude") (Ident "ord" 0))) (Literal (Char '0'))))))) (Variable (QualIdent Nothing (Ident "n" 6))))) []))])]))])
,(TypeSig (28,1) [(Ident "readInt" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (ConstructorType (QualIdent Nothing (Ident "Int" 0)) [])))
,(FunctionDecl (29,1) (Ident "readInt" 0) [(Equation (29,1) (FunLhs (Ident "readInt" 0) [(VariablePattern (Ident "l" 13))]) (SimpleRhs (29,13) (Apply (Variable (QualIdent Nothing (Ident "readIntPrefix" 14))) (Paren (Apply (Apply (Variable (QualIdent (Just "Prelude") (Ident "dropWhile" 0))) (Paren (Lambda [(VariablePattern (Ident "c" 20))] (InfixApply (Variable (QualIdent Nothing (Ident "c" 20))) (InfixOp (QualIdent (Just "Prelude") (Ident "==" 0))) (Literal (Char ' ')))))) (Variable (QualIdent Nothing (Ident "l" 13)))))) [(FunctionDecl (31,3) (Ident "readIntPrefix" 14) [(Equation (31,3) (FunLhs (Ident "readIntPrefix" 14) [(ListPattern [])]) (SimpleRhs (31,26) (Literal (Int (Ident "_" 17) 0)) [])),(Equation (32,3) (FunLhs (Ident "readIntPrefix" 14) [(ParenPattern (InfixPattern (VariablePattern (Ident "c" 18)) (QualIdent Nothing (Ident ":" 0)) (VariablePattern (Ident "cs" 18))))]) (SimpleRhs (32,26) (IfThenElse (InfixApply (Variable (QualIdent Nothing (Ident "c" 18))) (InfixOp (QualIdent (Just "Prelude") (Ident "==" 0))) (Literal (Char '-'))) (UnaryMinus (Ident "-" 0) (Paren (Apply (Variable (QualIdent (Just "Read") (Ident "readNat" 0))) (Variable (QualIdent Nothing (Ident "cs" 18)))))) (Apply (Variable (QualIdent (Just "Read") (Ident "readNat" 0))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "c" 18))) (InfixConstr (QualIdent Nothing (Ident ":" 0))) (Variable (QualIdent Nothing (Ident "cs" 18))))))) []))])]))])
,(TypeSig (38,1) [(Ident "readHex" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (ConstructorType (QualIdent Nothing (Ident "Int" 0)) [])))
,(FunctionDecl (39,1) (Ident "readHex" 0) [(Equation (39,1) (FunLhs (Ident "readHex" 0) [(VariablePattern (Ident "l" 21))]) (SimpleRhs (39,13) (Apply (Apply (Variable (QualIdent Nothing (Ident "readHexPrefix" 22))) (Paren (Apply (Apply (Variable (QualIdent (Just "Prelude") (Ident "dropWhile" 0))) (Paren (Lambda [(VariablePattern (Ident "c" 35))] (InfixApply (Variable (QualIdent Nothing (Ident "c" 35))) (InfixOp (QualIdent (Just "Prelude") (Ident "==" 0))) (Literal (Char ' ')))))) (Variable (QualIdent Nothing (Ident "l" 21)))))) (Literal (Int (Ident "_" 36) 0))) [(FunctionDecl (41,3) (Ident "readHexPrefix" 22) [(Equation (41,3) (FunLhs (Ident "readHexPrefix" 22) [(ListPattern []),(VariablePattern (Ident "n" 23))]) (SimpleRhs (41,24) (Variable (QualIdent Nothing (Ident "n" 23))) [])),(Equation (42,3) (FunLhs (Ident "readHexPrefix" 22) [(ParenPattern (InfixPattern (VariablePattern (Ident "c" 25)) (QualIdent Nothing (Ident ":" 0)) (VariablePattern (Ident "cs" 25)))),(VariablePattern (Ident "n" 25))]) (SimpleRhs (43,4) (Let [(PatternDecl (43,8) (VariablePattern (Ident "cv" 27)) (SimpleRhs (43,13) (Apply (Variable (QualIdent Nothing (Ident "hex2int" 22))) (Variable (QualIdent Nothing (Ident "c" 25)))) []))] (IfThenElse (InfixApply (Variable (QualIdent Nothing (Ident "cv" 27))) (InfixOp (QualIdent (Just "Prelude") (Ident ">=" 0))) (Literal (Int (Ident "_" 29) 0))) (Apply (Apply (Variable (QualIdent Nothing (Ident "readHexPrefix" 22))) (Variable (QualIdent Nothing (Ident "cs" 25)))) (Paren (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "n" 25))) (InfixOp (QualIdent (Just "Prelude") (Ident "*" 0))) (Literal (Int (Ident "_" 30) 16))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (Variable (QualIdent Nothing (Ident "cv" 27)))))) (Variable (QualIdent Nothing (Ident "n" 25))))) []))]),(FunctionDecl (47,3) (Ident "hex2int" 22) [(Equation (47,3) (FunLhs (Ident "hex2int" 22) [(VariablePattern (Ident "c" 31))]) (SimpleRhs (47,15) (IfThenElse (Apply (Variable (QualIdent (Just "Char") (Ident "isDigit" 0))) (Variable (QualIdent Nothing (Ident "c" 31)))) (InfixApply (Apply (Variable (QualIdent (Just "Prelude") (Ident "ord" 0))) (Variable (QualIdent Nothing (Ident "c" 31)))) (InfixOp (QualIdent (Just "Prelude") (Ident "-" 0))) (Apply (Variable (QualIdent (Just "Prelude") (Ident "ord" 0))) (Literal (Char '0')))) (IfThenElse (InfixApply (InfixApply (Apply (Variable (QualIdent (Just "Prelude") (Ident "ord" 0))) (Variable (QualIdent Nothing (Ident "c" 31)))) (InfixOp (QualIdent (Just "Prelude") (Ident ">=" 0))) (Apply (Variable (QualIdent (Just "Prelude") (Ident "ord" 0))) (Literal (Char 'A')))) (InfixOp (QualIdent (Just "Prelude") (Ident "&&" 0))) (InfixApply (Apply (Variable (QualIdent (Just "Prelude") (Ident "ord" 0))) (Variable (QualIdent Nothing (Ident "c" 31)))) (InfixOp (QualIdent (Just "Prelude") (Ident "<=" 0))) (Apply (Variable (QualIdent (Just "Prelude") (Ident "ord" 0))) (Literal (Char 'F'))))) (InfixApply (InfixApply (Apply (Variable (QualIdent (Just "Prelude") (Ident "ord" 0))) (Variable (QualIdent Nothing (Ident "c" 31)))) (InfixOp (QualIdent (Just "Prelude") (Ident "-" 0))) (Apply (Variable (QualIdent (Just "Prelude") (Ident "ord" 0))) (Literal (Char 'A')))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (Literal (Int (Ident "_" 33) 10))) (UnaryMinus (Ident "-" 0) (Literal (Int (Ident "_" 34) 1))))) []))])]))])
]