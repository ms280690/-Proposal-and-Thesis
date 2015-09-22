Module "ReadShowTerm"
Nothing
[(ImportDecl (1,1) "Prelude" False Nothing Nothing)
,(ImportDecl (13,1) "Char" False Nothing (Just (Importing (13,12) [(Import (Ident "isSpace" 0))])))
,(TypeSig (22,1) [(Ident "showTerm" 0)] (ArrowType (VariableType (Ident "_" 0)) (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])))
,(FunctionDecl (23,1) (Ident "showTerm" 0) [(Equation (23,1) (FunLhs (Ident "showTerm" 0) [(VariablePattern (Ident "x" 2))]) (SimpleRhs (23,14) (InfixApply (Variable (QualIdent (Just "ReadShowTerm") (Ident "prim_showTerm" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "$##" 0))) (Variable (QualIdent Nothing (Ident "x" 2)))) []))])
,(TypeSig (25,1) [(Ident "prim_showTerm" 0)] (ArrowType (VariableType (Ident "_" 0)) (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])))
,(FlatExternalDecl (26,1) [(Ident "prim_showTerm" 0)])
,(TypeSig (34,1) [(Ident "showQTerm" 0)] (ArrowType (VariableType (Ident "_" 0)) (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])))
,(FunctionDecl (35,1) (Ident "showQTerm" 0) [(Equation (35,1) (FunLhs (Ident "showQTerm" 0) [(VariablePattern (Ident "x" 4))]) (SimpleRhs (35,15) (InfixApply (Variable (QualIdent (Just "ReadShowTerm") (Ident "prim_showQTerm" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "$##" 0))) (Variable (QualIdent Nothing (Ident "x" 4)))) []))])
,(TypeSig (37,1) [(Ident "prim_showQTerm" 0)] (ArrowType (VariableType (Ident "_" 0)) (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])))
,(FlatExternalDecl (38,1) [(Ident "prim_showQTerm" 0)])
,(TypeSig (48,1) [(Ident "readsUnqualifiedTerm" 0)] (ArrowType (ListType (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])) (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (ListType (TupleType [(VariableType (Ident "_" 0)),(ConstructorType (QualIdent Nothing (Ident "String" 0)) [])])))))
,(FunctionDecl (49,1) (Ident "readsUnqualifiedTerm" 0) [(Equation (49,1) (FunLhs (Ident "readsUnqualifiedTerm" 0) [(ListPattern []),(VariablePattern (Ident "_" 7))]) (SimpleRhs (50,3) (Apply (Variable (QualIdent (Just "Prelude") (Ident "error" 0))) (Literal (String "ReadShowTerm.readsUnqualifiedTerm: list of module prefixes is empty"))) [])),(Equation (51,1) (FunLhs (Ident "readsUnqualifiedTerm" 0) [(ParenPattern (InfixPattern (VariablePattern (Ident "prefix" 9)) (QualIdent Nothing (Ident ":" 0)) (VariablePattern (Ident "prefixes" 9)))),(VariablePattern (Ident "s" 9))]) (SimpleRhs (52,3) (Apply (Apply (Variable (QualIdent (Just "ReadShowTerm") (Ident "readsUnqualifiedTermWithPrefixes" 0))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "prefix" 9))) (InfixConstr (QualIdent Nothing (Ident ":" 0))) (Variable (QualIdent Nothing (Ident "prefixes" 9)))))) (Variable (QualIdent Nothing (Ident "s" 9)))) []))])
,(TypeSig (54,1) [(Ident "readsUnqualifiedTermWithPrefixes" 0)] (ArrowType (ListType (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])) (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (ListType (TupleType [(VariableType (Ident "_" 0)),(ConstructorType (QualIdent Nothing (Ident "String" 0)) [])])))))
,(FunctionDecl (55,1) (Ident "readsUnqualifiedTermWithPrefixes" 0) [(Equation (55,1) (FunLhs (Ident "readsUnqualifiedTermWithPrefixes" 0) [(VariablePattern (Ident "prefixes" 11)),(VariablePattern (Ident "s" 11))]) (SimpleRhs (56,3) (InfixApply (Paren (InfixApply (Variable (QualIdent (Just "ReadShowTerm") (Ident "prim_readsUnqualifiedTerm" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "$##" 0))) (Variable (QualIdent Nothing (Ident "prefixes" 11))))) (InfixOp (QualIdent (Just "Prelude") (Ident "$##" 0))) (Variable (QualIdent Nothing (Ident "s" 11)))) []))])
,(TypeSig (58,1) [(Ident "prim_readsUnqualifiedTerm" 0)] (ArrowType (ListType (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])) (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (ListType (TupleType [(VariableType (Ident "_" 0)),(ConstructorType (QualIdent Nothing (Ident "String" 0)) [])])))))
,(FlatExternalDecl (59,1) [(Ident "prim_readsUnqualifiedTerm" 0)])
,(TypeSig (70,1) [(Ident "readUnqualifiedTerm" 0)] (ArrowType (ListType (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])) (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (VariableType (Ident "_" 0)))))
,(FunctionDecl (71,1) (Ident "readUnqualifiedTerm" 0) [(Equation (71,1) (FunLhs (Ident "readUnqualifiedTerm" 0) [(VariablePattern (Ident "prefixes" 13)),(VariablePattern (Ident "s" 13))]) (SimpleRhs (71,34) (Case (Apply (Apply (Variable (QualIdent (Just "ReadShowTerm") (Ident "readsUnqualifiedTerm" 0))) (Variable (QualIdent Nothing (Ident "prefixes" 13)))) (Variable (QualIdent Nothing (Ident "s" 13)))) [(Alt (72,3) (ListPattern [(TuplePattern [(VariablePattern (Ident "term" 15)),(VariablePattern (Ident "tail" 15))])]) (SimpleRhs (73,9) (IfThenElse (Apply (Apply (Variable (QualIdent (Just "Prelude") (Ident "all" 0))) (Variable (QualIdent (Just "Char") (Ident "isSpace" 0)))) (Variable (QualIdent Nothing (Ident "tail" 15)))) (Variable (QualIdent Nothing (Ident "term" 15))) (Apply (Variable (QualIdent (Just "Prelude") (Ident "error" 0))) (Paren (InfixApply (Literal (String "ReadShowTerm.readUnqualifiedTerm: no parse, unmatched string after term: ")) (InfixOp (QualIdent (Just "Prelude") (Ident "++" 0))) (Variable (QualIdent Nothing (Ident "tail" 15))))))) [])),(Alt (75,3) (ListPattern []) (SimpleRhs (75,10) (Apply (Variable (QualIdent (Just "Prelude") (Ident "error" 0))) (Literal (String "ReadShowTerm.readUnqualifiedTerm: no parse"))) [])),(Alt (76,3) (VariablePattern (Ident "_" 20)) (SimpleRhs (76,10) (Apply (Variable (QualIdent (Just "Prelude") (Ident "error" 0))) (Literal (String "ReadShowTerm.readUnqualifiedTerm: ambiguous parse"))) []))]) []))])
,(TypeSig (80,1) [(Ident "readsTerm" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (ListType (TupleType [(VariableType (Ident "_" 0)),(ConstructorType (QualIdent Nothing (Ident "String" 0)) [])]))))
,(FunctionDecl (81,1) (Ident "readsTerm" 0) [(Equation (81,1) (FunLhs (Ident "readsTerm" 0) [(VariablePattern (Ident "s" 22))]) (SimpleRhs (81,15) (InfixApply (Apply (Variable (QualIdent (Just "ReadShowTerm") (Ident "prim_readsUnqualifiedTerm" 0))) (List [])) (InfixOp (QualIdent (Just "Prelude") (Ident "$##" 0))) (Variable (QualIdent Nothing (Ident "s" 22)))) []))])
,(TypeSig (85,1) [(Ident "readTerm" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (VariableType (Ident "_" 0))))
,(FunctionDecl (86,1) (Ident "readTerm" 0) [(Equation (86,1) (FunLhs (Ident "readTerm" 0) [(VariablePattern (Ident "s" 24))]) (SimpleRhs (86,14) (Case (Variable (QualIdent Nothing (Ident "result" 25))) [(Alt (87,3) (ListPattern [(TuplePattern [(VariablePattern (Ident "term" 27)),(VariablePattern (Ident "tail" 27))])]) (SimpleRhs (88,9) (IfThenElse (Apply (Apply (Variable (QualIdent (Just "Prelude") (Ident "all" 0))) (Variable (QualIdent (Just "Char") (Ident "isSpace" 0)))) (Variable (QualIdent Nothing (Ident "tail" 27)))) (Variable (QualIdent Nothing (Ident "term" 27))) (Apply (Variable (QualIdent (Just "Prelude") (Ident "error" 0))) (Paren (InfixApply (Literal (String "ReadShowTerm.readTerm: no parse, unmatched string after term: ")) (InfixOp (QualIdent (Just "Prelude") (Ident "++" 0))) (Variable (QualIdent Nothing (Ident "tail" 27))))))) [])),(Alt (90,3) (ListPattern []) (SimpleRhs (90,10) (InfixApply (Variable (QualIdent (Just "Prelude") (Ident "error" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "$" 0))) (InfixApply (Literal (String "ReadShowTerm.readTerm: no parse of string beginning with ")) (InfixOp (QualIdent (Just "Prelude") (Ident "++" 0))) (Apply (Apply (Variable (QualIdent (Just "Prelude") (Ident "take" 0))) (Literal (Int (Ident "_" 31) 20))) (Variable (QualIdent Nothing (Ident "s" 24)))))) [])),(Alt (92,3) (VariablePattern (Ident "_" 33)) (SimpleRhs (92,10) (Apply (Variable (QualIdent (Just "Prelude") (Ident "error" 0))) (Literal (String "ReadShowTerm.readTerm: ambiguous parse"))) []))]) [(PatternDecl (93,8) (VariablePattern (Ident "result" 25)) (SimpleRhs (93,17) (InfixApply (Apply (Variable (QualIdent (Just "ReadShowTerm") (Ident "prim_readsUnqualifiedTerm" 0))) (List [])) (InfixOp (QualIdent (Just "Prelude") (Ident "$##" 0))) (Variable (QualIdent Nothing (Ident "s" 24)))) []))]))])
,(TypeSig (100,1) [(Ident "readsQTerm" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (ListType (TupleType [(VariableType (Ident "_" 0)),(ConstructorType (QualIdent Nothing (Ident "String" 0)) [])]))))
,(FunctionDecl (101,1) (Ident "readsQTerm" 0) [(Equation (101,1) (FunLhs (Ident "readsQTerm" 0) [(VariablePattern (Ident "s" 35))]) (SimpleRhs (101,16) (InfixApply (Variable (QualIdent (Just "ReadShowTerm") (Ident "prim_readsQTerm" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "$##" 0))) (Variable (QualIdent Nothing (Ident "s" 35)))) []))])
,(TypeSig (103,1) [(Ident "prim_readsQTerm" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (ListType (TupleType [(VariableType (Ident "_" 0)),(ConstructorType (QualIdent Nothing (Ident "String" 0)) [])]))))
,(FlatExternalDecl (104,1) [(Ident "prim_readsQTerm" 0)])
,(TypeSig (109,1) [(Ident "readQTerm" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (VariableType (Ident "_" 0))))
,(FunctionDecl (110,1) (Ident "readQTerm" 0) [(Equation (110,1) (FunLhs (Ident "readQTerm" 0) [(VariablePattern (Ident "s" 37))]) (SimpleRhs (110,15) (Case (Variable (QualIdent Nothing (Ident "result" 38))) [(Alt (111,3) (ListPattern [(TuplePattern [(VariablePattern (Ident "term" 40)),(VariablePattern (Ident "tail" 40))])]) (SimpleRhs (111,20) (IfThenElse (Apply (Apply (Variable (QualIdent (Just "Prelude") (Ident "all" 0))) (Variable (QualIdent (Just "Char") (Ident "isSpace" 0)))) (Variable (QualIdent Nothing (Ident "tail" 40)))) (Variable (QualIdent Nothing (Ident "term" 40))) (Apply (Variable (QualIdent (Just "Prelude") (Ident "error" 0))) (Literal (String "ReadShowTerm.readQTerm: no parse")))) [])),(Alt (113,3) (ListPattern []) (SimpleRhs (113,10) (Apply (Variable (QualIdent (Just "Prelude") (Ident "error" 0))) (Literal (String "ReadShowTerm.readQTerm: no parse"))) [])),(Alt (114,3) (VariablePattern (Ident "_" 45)) (SimpleRhs (114,10) (Apply (Variable (QualIdent (Just "Prelude") (Ident "error" 0))) (Literal (String "ReadShowTerm.readQTerm: ambiguous parse"))) []))]) [(PatternDecl (115,8) (VariablePattern (Ident "result" 38)) (SimpleRhs (115,17) (Apply (Variable (QualIdent (Just "ReadShowTerm") (Ident "readsQTerm" 0))) (Variable (QualIdent Nothing (Ident "s" 37)))) []))]))])
,(TypeSig (121,1) [(Ident "readQTermFile" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (ConstructorType (QualIdent Nothing (Ident "IO" 0)) [(VariableType (Ident "_" 0))])))
,(FunctionDecl (122,1) (Ident "readQTermFile" 0) [(Equation (122,1) (FunLhs (Ident "readQTermFile" 0) [(VariablePattern (Ident "file" 47))]) (SimpleRhs (122,22) (InfixApply (Apply (Variable (QualIdent (Just "Prelude") (Ident "readFile" 0))) (Variable (QualIdent Nothing (Ident "file" 47)))) (InfixOp (QualIdent (Just "Prelude") (Ident ">>=" 0))) (InfixApply (Variable (QualIdent (Just "Prelude") (Ident "return" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "." 0))) (Variable (QualIdent (Just "ReadShowTerm") (Ident "readQTerm" 0))))) []))])
,(TypeSig (127,1) [(Ident "readQTermListFile" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (ConstructorType (QualIdent Nothing (Ident "IO" 0)) [(ListType (VariableType (Ident "_" 0)))])))
,(FunctionDecl (128,1) (Ident "readQTermListFile" 0) [(Equation (128,1) (FunLhs (Ident "readQTermListFile" 0) [(VariablePattern (Ident "file" 49))]) (SimpleRhs (128,26) (InfixApply (Apply (Variable (QualIdent (Just "Prelude") (Ident "readFile" 0))) (Variable (QualIdent Nothing (Ident "file" 49)))) (InfixOp (QualIdent (Just "Prelude") (Ident ">>=" 0))) (InfixApply (Variable (QualIdent (Just "Prelude") (Ident "return" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "." 0))) (InfixApply (Apply (Variable (QualIdent (Just "Prelude") (Ident "map" 0))) (Variable (QualIdent (Just "ReadShowTerm") (Ident "readQTerm" 0)))) (InfixOp (QualIdent (Just "Prelude") (Ident "." 0))) (Variable (QualIdent (Just "Prelude") (Ident "lines" 0)))))) []))])
,(TypeSig (134,1) [(Ident "writeQTermFile" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (ArrowType (VariableType (Ident "_" 0)) (ConstructorType (QualIdent Nothing (Ident "IO" 0)) [(TupleType [])]))))
,(FunctionDecl (135,1) (Ident "writeQTermFile" 0) [(Equation (135,1) (FunLhs (Ident "writeQTermFile" 0) [(VariablePattern (Ident "filename" 51)),(VariablePattern (Ident "term" 51))]) (SimpleRhs (135,32) (Apply (Apply (Variable (QualIdent (Just "Prelude") (Ident "writeFile" 0))) (Variable (QualIdent Nothing (Ident "filename" 51)))) (Paren (Apply (Variable (QualIdent (Just "ReadShowTerm") (Ident "showQTerm" 0))) (Variable (QualIdent Nothing (Ident "term" 51)))))) []))])
,(TypeSig (143,1) [(Ident "writeQTermListFile" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (ArrowType (ListType (VariableType (Ident "_" 0))) (ConstructorType (QualIdent Nothing (Ident "IO" 0)) [(TupleType [])]))))
,(FunctionDecl (144,1) (Ident "writeQTermListFile" 0) [(Equation (144,1) (FunLhs (Ident "writeQTermListFile" 0) [(VariablePattern (Ident "filename" 53)),(VariablePattern (Ident "terms" 53))]) (SimpleRhs (145,5) (Apply (Apply (Variable (QualIdent (Just "Prelude") (Ident "writeFile" 0))) (Variable (QualIdent Nothing (Ident "filename" 53)))) (Paren (Apply (Variable (QualIdent (Just "Prelude") (Ident "unlines" 0))) (Paren (Apply (Apply (Variable (QualIdent (Just "Prelude") (Ident "map" 0))) (Variable (QualIdent (Just "ReadShowTerm") (Ident "showQTerm" 0)))) (Variable (QualIdent Nothing (Ident "terms" 53)))))))) []))])
]