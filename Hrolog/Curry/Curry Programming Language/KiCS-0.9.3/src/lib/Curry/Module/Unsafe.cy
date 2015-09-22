Module "Unsafe"
(Just (Exporting (10,15) [(Export (QualIdent Nothing (Ident "unsafePerformIO" 0))),(Export (QualIdent Nothing (Ident "trace" 0))),(Export (QualIdent Nothing (Ident "spawnConstraint" 0))),(Export (QualIdent Nothing (Ident "isVar" 0))),(Export (QualIdent Nothing (Ident "identicalVar" 0))),(Export (QualIdent Nothing (Ident "showAnyTerm" 0))),(Export (QualIdent Nothing (Ident "showAnyQTerm" 0))),(Export (QualIdent Nothing (Ident "showAnyExpression" 0))),(Export (QualIdent Nothing (Ident "showAnyQExpression" 0))),(Export (QualIdent Nothing (Ident "readsAnyUnqualifiedTerm" 0))),(Export (QualIdent Nothing (Ident "readAnyUnqualifiedTerm" 0))),(Export (QualIdent Nothing (Ident "readsAnyQTerm" 0))),(Export (QualIdent Nothing (Ident "readAnyQTerm" 0))),(Export (QualIdent Nothing (Ident "generateChoice" 0))),(Export (QualIdent Nothing (Ident "OrRef" 0))),(Export (QualIdent Nothing (Ident "try" 0))),(Export (QualIdent Nothing (Ident "orsWithOrRef" 0))),(Export (QualIdent Nothing (Ident "nrOfChoices" 0)))]))
[(ImportDecl (1,1) "Prelude" False Nothing Nothing)
,(ImportDecl (18,1) "Char" False Nothing (Just (Importing (18,12) [(Import (Ident "isSpace" 0))])))
,(ImportDecl (19,1) "Meta" False Nothing Nothing)
,(TypeSig (22,1) [(Ident "unsafePerformIO" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "IO" 0)) [(VariableType (Ident "a" 0))]) (VariableType (Ident "a" 0))))
,(FunctionDecl (23,1) (Ident "unsafePerformIO" 0) [(Equation (23,1) (FunLhs (Ident "unsafePerformIO" 0) [(VariablePattern (Ident "act" 2))]) (SimpleRhs (23,23) (InfixApply (Variable (QualIdent (Just "Unsafe") (Ident "prim_unsafePerformIO" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "$#" 0))) (Variable (QualIdent Nothing (Ident "act" 2)))) []))])
,(TypeSig (25,1) [(Ident "prim_unsafePerformIO" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "IO" 0)) [(VariableType (Ident "a" 0))]) (VariableType (Ident "a" 0))))
,(FlatExternalDecl (26,1) [(Ident "prim_unsafePerformIO" 0)])
,(TypeSig (30,1) [(Ident "trace" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (ArrowType (VariableType (Ident "a" 0)) (VariableType (Ident "a" 0)))))
,(FunctionDecl (31,1) (Ident "trace" 0) [(Equation (31,1) (FunLhs (Ident "trace" 0) [(VariablePattern (Ident "s" 4)),(VariablePattern (Ident "x" 4))]) (SimpleRhs (31,13) (Apply (Variable (QualIdent (Just "Unsafe") (Ident "unsafePerformIO" 0))) (Paren (InfixApply (Apply (Variable (QualIdent (Just "Prelude") (Ident "putStr" 0))) (Variable (QualIdent Nothing (Ident "s" 4)))) (InfixOp (QualIdent (Just "Prelude") (Ident ">>" 0))) (Apply (Variable (QualIdent (Just "Prelude") (Ident "return" 0))) (Variable (QualIdent Nothing (Ident "x" 4))))))) []))])
,(TypeSig (44,1) [(Ident "spawnConstraint" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "Success" 0)) []) (ArrowType (VariableType (Ident "a" 0)) (VariableType (Ident "a" 0)))))
,(FlatExternalDecl (45,1) [(Ident "spawnConstraint" 0)])
,(TypeSig (49,1) [(Ident "isVar" 0)] (ArrowType (VariableType (Ident "_" 0)) (ConstructorType (QualIdent Nothing (Ident "Bool" 0)) [])))
,(FunctionDecl (50,1) (Ident "isVar" 0) [(Equation (50,1) (FunLhs (Ident "isVar" 0) [(VariablePattern (Ident "v" 6))]) (SimpleRhs (50,11) (Apply (Variable (QualIdent (Just "Unsafe") (Ident "unsafePerformIO" 0))) (Paren (InfixApply (Apply (Variable (QualIdent (Just "Meta") (Ident "isFree" 0))) (Variable (QualIdent Nothing (Ident "v" 6)))) (InfixOp (QualIdent (Just "Prelude") (Ident ">>=" 0))) (InfixApply (Variable (QualIdent (Just "Prelude") (Ident "return" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "." 0))) (Apply (Apply (Variable (QualIdent (Just "Prelude") (Ident "either" 0))) (Paren (Apply (Variable (QualIdent (Just "Prelude") (Ident "const" 0))) (Constructor (QualIdent (Just "Prelude") (Ident "True" 0)))))) (Paren (Apply (Variable (QualIdent (Just "Prelude") (Ident "const" 0))) (Constructor (QualIdent (Just "Prelude") (Ident "False" 0)))))))))) []))])
,(TypeSig (53,1) [(Ident "prim_isVar" 0)] (ArrowType (VariableType (Ident "_" 0)) (ConstructorType (QualIdent Nothing (Ident "Bool" 0)) [])))
,(FlatExternalDecl (54,1) [(Ident "prim_isVar" 0)])
,(TypeSig (62,1) [(Ident "identicalVar" 0)] (ArrowType (VariableType (Ident "a" 0)) (ArrowType (VariableType (Ident "a" 0)) (ConstructorType (QualIdent Nothing (Ident "Bool" 0)) []))))
,(FunctionDecl (63,1) (Ident "identicalVar" 0) [(Equation (63,1) (FunLhs (Ident "identicalVar" 0) [(VariablePattern (Ident "x" 8)),(VariablePattern (Ident "y" 8))]) (SimpleRhs (63,20) (InfixApply (Paren (InfixApply (Variable (QualIdent (Just "Unsafe") (Ident "prim_identicalVar" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "$!" 0))) (Variable (QualIdent Nothing (Ident "x" 8))))) (InfixOp (QualIdent (Just "Prelude") (Ident "$!" 0))) (Variable (QualIdent Nothing (Ident "y" 8)))) []))])
,(TypeSig (66,1) [(Ident "prim_identicalVar" 0)] (ArrowType (VariableType (Ident "a" 0)) (ArrowType (VariableType (Ident "a" 0)) (ConstructorType (QualIdent Nothing (Ident "Bool" 0)) []))))
,(FlatExternalDecl (67,1) [(Ident "prim_identicalVar" 0)])
,(TypeSig (78,1) [(Ident "showAnyTerm" 0)] (ArrowType (VariableType (Ident "_" 0)) (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])))
,(FunctionDecl (79,1) (Ident "showAnyTerm" 0) [(Equation (79,1) (FunLhs (Ident "showAnyTerm" 0) [(VariablePattern (Ident "x" 10))]) (SimpleRhs (79,17) (InfixApply (Variable (QualIdent (Just "Unsafe") (Ident "prim_showAnyTerm" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "$!!" 0))) (Variable (QualIdent Nothing (Ident "x" 10)))) []))])
,(TypeSig (81,1) [(Ident "prim_showAnyTerm" 0)] (ArrowType (VariableType (Ident "_" 0)) (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])))
,(FlatExternalDecl (82,1) [(Ident "prim_showAnyTerm" 0)])
,(TypeSig (92,1) [(Ident "showAnyQTerm" 0)] (ArrowType (VariableType (Ident "_" 0)) (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])))
,(FunctionDecl (93,1) (Ident "showAnyQTerm" 0) [(Equation (93,1) (FunLhs (Ident "showAnyQTerm" 0) [(VariablePattern (Ident "x" 12))]) (SimpleRhs (93,18) (InfixApply (Variable (QualIdent (Just "Unsafe") (Ident "prim_showAnyQTerm" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "$!!" 0))) (Variable (QualIdent Nothing (Ident "x" 12)))) []))])
,(TypeSig (95,1) [(Ident "prim_showAnyQTerm" 0)] (ArrowType (VariableType (Ident "_" 0)) (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])))
,(FlatExternalDecl (96,1) [(Ident "prim_showAnyQTerm" 0)])
,(TypeSig (105,1) [(Ident "readsAnyUnqualifiedTerm" 0)] (ArrowType (ListType (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])) (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (ListType (TupleType [(VariableType (Ident "_" 0)),(ConstructorType (QualIdent Nothing (Ident "String" 0)) [])])))))
,(FunctionDecl (106,1) (Ident "readsAnyUnqualifiedTerm" 0) [(Equation (106,1) (FunLhs (Ident "readsAnyUnqualifiedTerm" 0) [(ListPattern []),(VariablePattern (Ident "_" 15))]) (SimpleRhs (107,3) (Apply (Variable (QualIdent (Just "Prelude") (Ident "error" 0))) (Literal (String "ReadShowTerm.readsAnyUnqualifiedTerm: list of module prefixes is empty"))) [])),(Equation (108,1) (FunLhs (Ident "readsAnyUnqualifiedTerm" 0) [(ParenPattern (InfixPattern (VariablePattern (Ident "prefix" 17)) (QualIdent Nothing (Ident ":" 0)) (VariablePattern (Ident "prefixes" 17)))),(VariablePattern (Ident "s" 17))]) (SimpleRhs (109,3) (Apply (Apply (Variable (QualIdent (Just "Unsafe") (Ident "readsAnyUnqualifiedTermWithPrefixes" 0))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "prefix" 17))) (InfixConstr (QualIdent Nothing (Ident ":" 0))) (Variable (QualIdent Nothing (Ident "prefixes" 17)))))) (Variable (QualIdent Nothing (Ident "s" 17)))) []))])
,(TypeSig (111,1) [(Ident "readsAnyUnqualifiedTermWithPrefixes" 0)] (ArrowType (ListType (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])) (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (ListType (TupleType [(VariableType (Ident "_" 0)),(ConstructorType (QualIdent Nothing (Ident "String" 0)) [])])))))
,(FunctionDecl (112,1) (Ident "readsAnyUnqualifiedTermWithPrefixes" 0) [(Equation (112,1) (FunLhs (Ident "readsAnyUnqualifiedTermWithPrefixes" 0) [(VariablePattern (Ident "prefixes" 19)),(VariablePattern (Ident "s" 19))]) (SimpleRhs (113,3) (InfixApply (Paren (InfixApply (Variable (QualIdent (Just "Unsafe") (Ident "prim_readsAnyUnqualifiedTerm" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "$##" 0))) (Variable (QualIdent Nothing (Ident "prefixes" 19))))) (InfixOp (QualIdent (Just "Prelude") (Ident "$##" 0))) (Variable (QualIdent Nothing (Ident "s" 19)))) []))])
,(TypeSig (115,1) [(Ident "prim_readsAnyUnqualifiedTerm" 0)] (ArrowType (ListType (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])) (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (ListType (TupleType [(VariableType (Ident "_" 0)),(ConstructorType (QualIdent Nothing (Ident "String" 0)) [])])))))
,(FlatExternalDecl (116,1) [(Ident "prim_readsAnyUnqualifiedTerm" 0)])
,(TypeSig (123,1) [(Ident "readAnyUnqualifiedTerm" 0)] (ArrowType (ListType (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])) (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (VariableType (Ident "_" 0)))))
,(FunctionDecl (124,1) (Ident "readAnyUnqualifiedTerm" 0) [(Equation (124,1) (FunLhs (Ident "readAnyUnqualifiedTerm" 0) [(VariablePattern (Ident "prefixes" 21)),(VariablePattern (Ident "s" 21))]) (SimpleRhs (124,37) (Case (Variable (QualIdent Nothing (Ident "result" 22))) [(Alt (125,3) (ListPattern [(TuplePattern [(VariablePattern (Ident "term" 24)),(VariablePattern (Ident "tail" 24))])]) (SimpleRhs (126,9) (IfThenElse (Apply (Apply (Variable (QualIdent (Just "Prelude") (Ident "all" 0))) (Variable (QualIdent (Just "Char") (Ident "isSpace" 0)))) (Variable (QualIdent Nothing (Ident "tail" 24)))) (Variable (QualIdent Nothing (Ident "term" 24))) (Apply (Variable (QualIdent (Just "Prelude") (Ident "error" 0))) (Paren (InfixApply (Literal (String "Unsafe.readAnyUnqualifiedTerm: no parse, unmatched string after term: ")) (InfixOp (QualIdent (Just "Prelude") (Ident "++" 0))) (Variable (QualIdent Nothing (Ident "tail" 24))))))) [])),(Alt (128,3) (ListPattern []) (SimpleRhs (128,10) (Apply (Variable (QualIdent (Just "Prelude") (Ident "error" 0))) (Literal (String "Unsafe.readAnyUnqualifiedTerm: no parse"))) [])),(Alt (129,3) (VariablePattern (Ident "_" 29)) (SimpleRhs (129,10) (Apply (Variable (QualIdent (Just "Prelude") (Ident "error" 0))) (Literal (String "Unsafe.readAnyUnqualifiedTerm: ambiguous parse"))) []))]) [(PatternDecl (130,8) (VariablePattern (Ident "result" 22)) (SimpleRhs (130,17) (Apply (Apply (Variable (QualIdent (Just "Unsafe") (Ident "readsAnyUnqualifiedTerm" 0))) (Variable (QualIdent Nothing (Ident "prefixes" 21)))) (Variable (QualIdent Nothing (Ident "s" 21)))) []))]))])
,(TypeSig (138,1) [(Ident "readsAnyQTerm" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (ListType (TupleType [(VariableType (Ident "_" 0)),(ConstructorType (QualIdent Nothing (Ident "String" 0)) [])]))))
,(FunctionDecl (139,1) (Ident "readsAnyQTerm" 0) [(Equation (139,1) (FunLhs (Ident "readsAnyQTerm" 0) [(VariablePattern (Ident "s" 31))]) (SimpleRhs (139,19) (InfixApply (Variable (QualIdent (Just "Unsafe") (Ident "prim_readsAnyQTerm" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "$##" 0))) (Variable (QualIdent Nothing (Ident "s" 31)))) []))])
,(TypeSig (141,1) [(Ident "prim_readsAnyQTerm" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (ListType (TupleType [(VariableType (Ident "_" 0)),(ConstructorType (QualIdent Nothing (Ident "String" 0)) [])]))))
,(FlatExternalDecl (142,1) [(Ident "prim_readsAnyQTerm" 0)])
,(TypeSig (148,1) [(Ident "readAnyQTerm" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "String" 0)) []) (VariableType (Ident "_" 0))))
,(FunctionDecl (149,1) (Ident "readAnyQTerm" 0) [(Equation (149,1) (FunLhs (Ident "readAnyQTerm" 0) [(VariablePattern (Ident "s" 33))]) (SimpleRhs (149,18) (Case (Variable (QualIdent Nothing (Ident "result" 34))) [(Alt (150,3) (ListPattern [(TuplePattern [(VariablePattern (Ident "term" 36)),(VariablePattern (Ident "tail" 36))])]) (SimpleRhs (150,20) (IfThenElse (Apply (Apply (Variable (QualIdent (Just "Prelude") (Ident "all" 0))) (Variable (QualIdent (Just "Char") (Ident "isSpace" 0)))) (Variable (QualIdent Nothing (Ident "tail" 36)))) (Variable (QualIdent Nothing (Ident "term" 36))) (Apply (Variable (QualIdent (Just "Prelude") (Ident "error" 0))) (Literal (String "Unsafe.readAnyQTerm: no parse")))) [])),(Alt (152,3) (ListPattern []) (SimpleRhs (152,10) (Apply (Variable (QualIdent (Just "Prelude") (Ident "error" 0))) (Literal (String "Unsafe.readAnyQTerm: no parse"))) [])),(Alt (153,3) (VariablePattern (Ident "_" 41)) (SimpleRhs (153,10) (Apply (Variable (QualIdent (Just "Prelude") (Ident "error" 0))) (Literal (String "Unsafe.readAnyQTerm: ambiguous parse"))) []))]) [(PatternDecl (154,8) (VariablePattern (Ident "result" 34)) (SimpleRhs (154,17) (Apply (Variable (QualIdent (Just "Unsafe") (Ident "readsAnyQTerm" 0))) (Variable (QualIdent Nothing (Ident "s" 33)))) []))]))])
,(TypeSig (161,1) [(Ident "showAnyExpression" 0)] (ArrowType (VariableType (Ident "_" 0)) (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])))
,(FlatExternalDecl (162,1) [(Ident "showAnyExpression" 0)])
,(TypeSig (168,1) [(Ident "showAnyQExpression" 0)] (ArrowType (VariableType (Ident "_" 0)) (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])))
,(FlatExternalDecl (169,1) [(Ident "showAnyQExpression" 0)])
,(TypeSig (171,1) [(Ident "try" 0)] (ArrowType (VariableType (Ident "a" 0)) (ConstructorType (QualIdent Nothing (Ident "Either" 0)) [(VariableType (Ident "a" 0)),(TupleType [(ConstructorType (QualIdent Nothing (Ident "OrRef" 0)) []),(ListType (VariableType (Ident "a" 0)))])])))
,(FlatExternalDecl (172,1) [(Ident "try" 0)])
,(TypeSig (174,1) [(Ident "orsWithOrRef" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "OrRef" 0)) []) (ArrowType (ListType (VariableType (Ident "a" 0))) (VariableType (Ident "a" 0)))))
,(FlatExternalDecl (175,1) [(Ident "orsWithOrRef" 0)])
,(TypeSig (177,1) [(Ident "generateChoice" 0)] (ArrowType (ListType (VariableType (Ident "a" 0))) (VariableType (Ident "a" 0))))
,(FlatExternalDecl (178,1) [(Ident "generateChoice" 0)])
,(TypeSig (180,1) [(Ident "nrOfChoices" 0)] (ConstructorType (QualIdent Nothing (Ident "IO" 0)) [(ConstructorType (QualIdent Nothing (Ident "Int" 0)) [])]))
,(FlatExternalDecl (181,1) [(Ident "nrOfChoices" 0)])
]