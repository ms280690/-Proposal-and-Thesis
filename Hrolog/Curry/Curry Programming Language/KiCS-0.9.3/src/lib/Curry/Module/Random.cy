Module "Random"
(Just (Exporting (19,14) [(Export (QualIdent Nothing (Ident "nextInt" 0))),(Export (QualIdent Nothing (Ident "nextIntRange" 0))),(Export (QualIdent Nothing (Ident "nextBoolean" 0))),(Export (QualIdent Nothing (Ident "getRandomSeed" 0)))]))
[(ImportDecl (1,1) "Prelude" False Nothing Nothing)
,(ImportDecl (21,1) "Time" False Nothing Nothing)
,(ImportDecl (22,1) "System" False Nothing (Just (Importing (22,14) [(Import (Ident "getCPUTime" 0))])))
,(FunctionDecl (30,1) (Ident "multiplier" 0) [(Equation (30,1) (FunLhs (Ident "multiplier" 0) []) (SimpleRhs (30,14) (Literal (Int (Ident "_" 4) 25214903917)) []))])
,(FunctionDecl (31,1) (Ident "addend" 0) [(Equation (31,1) (FunLhs (Ident "addend" 0) []) (SimpleRhs (31,14) (Literal (Int (Ident "_" 7) 11)) []))])
,(FunctionDecl (32,1) (Ident "powermask" 0) [(Equation (32,1) (FunLhs (Ident "powermask" 0) []) (SimpleRhs (32,14) (Literal (Int (Ident "_" 10) 48)) []))])
,(FunctionDecl (33,1) (Ident "mask" 0) [(Equation (33,1) (FunLhs (Ident "mask" 0) []) (SimpleRhs (33,14) (Literal (Int (Ident "_" 13) 281474976710656)) []))])
,(FunctionDecl (34,1) (Ident "intsize" 0) [(Equation (34,1) (FunLhs (Ident "intsize" 0) []) (SimpleRhs (34,14) (Literal (Int (Ident "_" 16) 32)) []))])
,(FunctionDecl (35,1) (Ident "intspan" 0) [(Equation (35,1) (FunLhs (Ident "intspan" 0) []) (SimpleRhs (35,14) (Literal (Int (Ident "_" 19) 4294967296)) []))])
,(FunctionDecl (36,1) (Ident "intlimit" 0) [(Equation (36,1) (FunLhs (Ident "intlimit" 0) []) (SimpleRhs (36,14) (Literal (Int (Ident "_" 22) 2147483648)) []))])
,(FunctionDecl (40,1) (Ident "sequence" 0) [(Equation (40,1) (FunLhs (Ident "sequence" 0) [(VariablePattern (Ident "seed" 23))]) (SimpleRhs (40,17) (InfixApply (Variable (QualIdent Nothing (Ident "next" 24))) (InfixConstr (QualIdent Nothing (Ident ":" 0))) (Apply (Variable (QualIdent (Just "Random") (Ident "sequence" 0))) (Variable (QualIdent Nothing (Ident "next" 24))))) [(PatternDecl (41,11) (VariablePattern (Ident "next" 24)) (SimpleRhs (41,18) (Apply (Variable (QualIdent (Just "Random") (Ident "nextseed" 0))) (Variable (QualIdent Nothing (Ident "seed" 23)))) []))]))])
,(FunctionDecl (45,1) (Ident "nextseed" 0) [(Equation (45,1) (FunLhs (Ident "nextseed" 0) [(VariablePattern (Ident "seed" 26))]) (SimpleRhs (45,17) (InfixApply (Paren (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "seed" 26))) (InfixOp (QualIdent (Just "Prelude") (Ident "*" 0))) (Variable (QualIdent (Just "Random") (Ident "multiplier" 0)))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (Variable (QualIdent (Just "Random") (Ident "addend" 0))))) (InfixOp (QualIdent (Just "Prelude") (Ident "mod" 0))) (Variable (QualIdent (Just "Random") (Ident "mask" 0)))) []))])
,(FunctionDecl (47,1) (Ident "xor" 0) [(Equation (47,1) (FunLhs (Ident "xor" 0) [(VariablePattern (Ident "x" 28)),(VariablePattern (Ident "y" 28))]) (SimpleRhs (47,11) (IfThenElse (InfixApply (Paren (InfixApply (Variable (QualIdent Nothing (Ident "x" 28))) (InfixOp (QualIdent (Just "Prelude") (Ident "==" 0))) (Literal (Int (Ident "_" 38) 0)))) (InfixOp (QualIdent (Just "Prelude") (Ident "&&" 0))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "y" 28))) (InfixOp (QualIdent (Just "Prelude") (Ident "==" 0))) (Literal (Int (Ident "_" 39) 0))))) (Literal (Int (Ident "_" 40) 0)) (InfixApply (Variable (QualIdent Nothing (Ident "lastBit" 29))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (InfixApply (Literal (Int (Ident "_" 41) 2)) (InfixOp (QualIdent (Just "Prelude") (Ident "*" 0))) (Variable (QualIdent Nothing (Ident "restBits" 29)))))) [(PatternDecl (48,11) (VariablePattern (Ident "lastBit" 29)) (SimpleRhs (48,22) (IfThenElse (InfixApply (Paren (InfixApply (Variable (QualIdent Nothing (Ident "x" 28))) (InfixOp (QualIdent (Just "Prelude") (Ident "mod" 0))) (Literal (Int (Ident "_" 31) 2)))) (InfixOp (QualIdent (Just "Prelude") (Ident "==" 0))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "y" 28))) (InfixOp (QualIdent (Just "Prelude") (Ident "mod" 0))) (Literal (Int (Ident "_" 32) 2))))) (Literal (Int (Ident "_" 33) 0)) (Literal (Int (Ident "_" 34) 1))) [])),(PatternDecl (49,11) (VariablePattern (Ident "restBits" 29)) (SimpleRhs (49,22) (Apply (Apply (Variable (QualIdent (Just "Random") (Ident "xor" 0))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "x" 28))) (InfixOp (QualIdent (Just "Prelude") (Ident "div" 0))) (Literal (Int (Ident "_" 36) 2))))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "y" 28))) (InfixOp (QualIdent (Just "Prelude") (Ident "div" 0))) (Literal (Int (Ident "_" 37) 2))))) []))]))])
,(FunctionDecl (51,1) (Ident "power" 0) [(Equation (51,1) (FunLhs (Ident "power" 0) [(VariablePattern (Ident "base" 42)),(VariablePattern (Ident "exp" 42))]) (SimpleRhs (51,18) (Apply (Apply (Apply (Variable (QualIdent Nothing (Ident "binary" 43))) (Literal (Int (Ident "_" 51) 1))) (Variable (QualIdent Nothing (Ident "base" 42)))) (Variable (QualIdent Nothing (Ident "exp" 42)))) [(FunctionDecl (52,11) (Ident "binary" 43) [(Equation (52,11) (FunLhs (Ident "binary" 43) [(VariablePattern (Ident "x" 44)),(VariablePattern (Ident "b" 44)),(VariablePattern (Ident "e" 44))]) (SimpleRhs (53,17) (IfThenElse (Paren (InfixApply (Variable (QualIdent Nothing (Ident "e" 44))) (InfixOp (QualIdent (Just "Prelude") (Ident "==" 0))) (Literal (Int (Ident "_" 46) 0)))) (Variable (QualIdent Nothing (Ident "x" 44))) (Apply (Apply (Apply (Variable (QualIdent Nothing (Ident "binary" 43))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "x" 44))) (InfixOp (QualIdent (Just "Prelude") (Ident "*" 0))) (IfThenElse (Paren (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "e" 44))) (InfixOp (QualIdent (Just "Prelude") (Ident "mod" 0))) (Literal (Int (Ident "_" 47) 2))) (InfixOp (QualIdent (Just "Prelude") (Ident "==" 0))) (Literal (Int (Ident "_" 48) 1)))) (Variable (QualIdent Nothing (Ident "b" 44))) (Literal (Int (Ident "_" 49) 1)))))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "b" 44))) (InfixOp (QualIdent (Just "Prelude") (Ident "*" 0))) (Variable (QualIdent Nothing (Ident "b" 44)))))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "e" 44))) (InfixOp (QualIdent (Just "Prelude") (Ident "div" 0))) (Literal (Int (Ident "_" 50) 2)))))) []))])]))])
,(FunctionDecl (58,1) (Ident "nextIntBits" 0) [(Equation (58,1) (FunLhs (Ident "nextIntBits" 0) [(VariablePattern (Ident "seed" 52)),(VariablePattern (Ident "bits" 52))]) (SimpleRhs (58,25) (Apply (Apply (Variable (QualIdent (Just "Prelude") (Ident "map" 0))) (Variable (QualIdent Nothing (Ident "adjust" 53)))) (Variable (QualIdent Nothing (Ident "list" 53)))) [(PatternDecl (59,11) (VariablePattern (Ident "init" 53)) (SimpleRhs (59,18) (InfixApply (Paren (Apply (Apply (Variable (QualIdent (Just "Random") (Ident "xor" 0))) (Variable (QualIdent Nothing (Ident "seed" 52)))) (Variable (QualIdent (Just "Random") (Ident "multiplier" 0))))) (InfixOp (QualIdent (Just "Prelude") (Ident "mod" 0))) (Variable (QualIdent (Just "Random") (Ident "mask" 0)))) [])),(PatternDecl (60,11) (VariablePattern (Ident "list" 53)) (SimpleRhs (60,18) (Apply (Variable (QualIdent (Just "Random") (Ident "sequence" 0))) (Variable (QualIdent Nothing (Ident "init" 53)))) [])),(PatternDecl (61,11) (VariablePattern (Ident "shift" 53)) (SimpleRhs (61,19) (Apply (Apply (Variable (QualIdent (Just "Random") (Ident "power" 0))) (Literal (Int (Ident "_" 57) 2))) (Paren (InfixApply (Variable (QualIdent (Just "Random") (Ident "powermask" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "-" 0))) (Variable (QualIdent Nothing (Ident "bits" 52)))))) [])),(FunctionDecl (62,11) (Ident "adjust" 53) [(Equation (62,11) (FunLhs (Ident "adjust" 53) [(VariablePattern (Ident "x" 58))]) (SimpleRhs (62,22) (IfThenElse (InfixApply (Variable (QualIdent Nothing (Ident "arg" 59))) (InfixOp (QualIdent (Just "Prelude") (Ident ">" 0))) (Variable (QualIdent (Just "Random") (Ident "intlimit" 0)))) (InfixApply (Variable (QualIdent Nothing (Ident "arg" 59))) (InfixOp (QualIdent (Just "Prelude") (Ident "-" 0))) (Variable (QualIdent (Just "Random") (Ident "intspan" 0)))) (Variable (QualIdent Nothing (Ident "arg" 59)))) [(PatternDecl (64,21) (VariablePattern (Ident "arg" 59)) (SimpleRhs (64,27) (InfixApply (Paren (InfixApply (Variable (QualIdent Nothing (Ident "x" 58))) (InfixOp (QualIdent (Just "Prelude") (Ident "div" 0))) (Variable (QualIdent Nothing (Ident "shift" 53))))) (InfixOp (QualIdent (Just "Prelude") (Ident "mod" 0))) (Variable (QualIdent (Just "Random") (Ident "intspan" 0)))) []))]))])]))])
,(TypeSig (77,1) [(Ident "nextInt" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "Int" 0)) []) (ListType (ConstructorType (QualIdent Nothing (Ident "Int" 0)) []))))
,(FunctionDecl (78,1) (Ident "nextInt" 0) [(Equation (78,1) (FunLhs (Ident "nextInt" 0) [(VariablePattern (Ident "seed" 61))]) (SimpleRhs (78,16) (Apply (Apply (Variable (QualIdent (Just "Random") (Ident "nextIntBits" 0))) (Variable (QualIdent Nothing (Ident "seed" 61)))) (Variable (QualIdent (Just "Random") (Ident "intsize" 0)))) []))])
,(TypeSig (89,1) [(Ident "nextIntRange" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "Int" 0)) []) (ArrowType (ConstructorType (QualIdent Nothing (Ident "Int" 0)) []) (ListType (ConstructorType (QualIdent Nothing (Ident "Int" 0)) [])))))
,(FunctionDecl (90,1) (Ident "nextIntRange" 0) [(Equation (90,1) (FunLhs (Ident "nextIntRange" 0) [(VariablePattern (Ident "seed" 63)),(VariablePattern (Ident "n" 63))]) (GuardedRhs [(CondExpr (90,21) (InfixApply (Variable (QualIdent Nothing (Ident "n" 63))) (InfixOp (QualIdent (Just "Prelude") (Ident ">" 0))) (Literal (Int (Ident "_" 82) 0))) (IfThenElse (Apply (Variable (QualIdent Nothing (Ident "power_of_2" 64))) (Variable (QualIdent Nothing (Ident "n" 63)))) (Apply (Apply (Variable (QualIdent (Just "Prelude") (Ident "map" 0))) (Variable (QualIdent Nothing (Ident "adjust_a" 64)))) (Variable (QualIdent Nothing (Ident "seq" 64)))) (Apply (Apply (Variable (QualIdent (Just "Prelude") (Ident "map" 0))) (Variable (QualIdent Nothing (Ident "adjust_b" 64)))) (Paren (Apply (Apply (Variable (QualIdent (Just "Prelude") (Ident "filter" 0))) (Variable (QualIdent Nothing (Ident "adjust_c" 64)))) (Variable (QualIdent Nothing (Ident "seq" 64))))))))] [(PatternDecl (93,11) (VariablePattern (Ident "seq" 64)) (SimpleRhs (93,17) (Apply (Apply (Variable (QualIdent (Just "Random") (Ident "nextIntBits" 0))) (Variable (QualIdent Nothing (Ident "seed" 63)))) (Paren (InfixApply (Variable (QualIdent (Just "Random") (Ident "intsize" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "-" 0))) (Literal (Int (Ident "_" 66) 1))))) [])),(FunctionDecl (94,11) (Ident "adjust_a" 64) [(Equation (94,11) (FunLhs (Ident "adjust_a" 64) [(VariablePattern (Ident "x" 67))]) (SimpleRhs (94,24) (InfixApply (Paren (InfixApply (Variable (QualIdent Nothing (Ident "n" 63))) (InfixOp (QualIdent (Just "Prelude") (Ident "*" 0))) (Variable (QualIdent Nothing (Ident "x" 67))))) (InfixOp (QualIdent (Just "Prelude") (Ident "div" 0))) (Variable (QualIdent (Just "Random") (Ident "intlimit" 0)))) []))]),(FunctionDecl (95,11) (Ident "adjust_b" 64) [(Equation (95,11) (FunLhs (Ident "adjust_b" 64) [(VariablePattern (Ident "x" 69))]) (SimpleRhs (95,24) (InfixApply (Variable (QualIdent Nothing (Ident "x" 69))) (InfixOp (QualIdent (Just "Prelude") (Ident "mod" 0))) (Variable (QualIdent Nothing (Ident "n" 63)))) []))]),(FunctionDecl (96,11) (Ident "adjust_c" 64) [(Equation (96,11) (FunLhs (Ident "adjust_c" 64) [(VariablePattern (Ident "x" 71))]) (SimpleRhs (96,24) (InfixApply (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "x" 71))) (InfixOp (QualIdent (Just "Prelude") (Ident "-" 0))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "x" 71))) (InfixOp (QualIdent (Just "Prelude") (Ident "mod" 0))) (Variable (QualIdent Nothing (Ident "n" 63)))))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "n" 63))) (InfixOp (QualIdent (Just "Prelude") (Ident "-" 0))) (Literal (Int (Ident "_" 73) 1))))) (InfixOp (QualIdent (Just "Prelude") (Ident ">=" 0))) (Literal (Int (Ident "_" 74) 0))) []))]),(FunctionDecl (97,11) (Ident "power_of_2" 64) [(Equation (97,11) (FunLhs (Ident "power_of_2" 64) [(VariablePattern (Ident "k" 75))]) (SimpleRhs (97,26) (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "k" 75))) (InfixOp (QualIdent (Just "Prelude") (Ident "==" 0))) (Literal (Int (Ident "_" 77) 2))) (InfixOp (QualIdent (Just "Prelude") (Ident "||" 0))) (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "k" 75))) (InfixOp (QualIdent (Just "Prelude") (Ident ">" 0))) (Literal (Int (Ident "_" 78) 2))) (InfixOp (QualIdent (Just "Prelude") (Ident "&&" 0))) (InfixApply (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "k" 75))) (InfixOp (QualIdent (Just "Prelude") (Ident "mod" 0))) (Literal (Int (Ident "_" 79) 2))) (InfixOp (QualIdent (Just "Prelude") (Ident "==" 0))) (Literal (Int (Ident "_" 80) 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "&&" 0))) (Apply (Variable (QualIdent Nothing (Ident "power_of_2" 64))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "k" 75))) (InfixOp (QualIdent (Just "Prelude") (Ident "div" 0))) (Literal (Int (Ident "_" 81) 2)))))))) []))])]))])
,(TypeSig (106,1) [(Ident "nextBoolean" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "Int" 0)) []) (ListType (ConstructorType (QualIdent Nothing (Ident "Bool" 0)) []))))
,(FunctionDecl (107,1) (Ident "nextBoolean" 0) [(Equation (107,1) (FunLhs (Ident "nextBoolean" 0) [(VariablePattern (Ident "seed" 83))]) (SimpleRhs (107,20) (Apply (Apply (Variable (QualIdent (Just "Prelude") (Ident "map" 0))) (RightSection (InfixOp (QualIdent (Just "Prelude") (Ident "/=" 0))) (Literal (Int (Ident "_" 85) 0)))) (Paren (Apply (Apply (Variable (QualIdent (Just "Random") (Ident "nextIntBits" 0))) (Variable (QualIdent Nothing (Ident "seed" 83)))) (Literal (Int (Ident "_" 86) 1))))) []))])
,(TypeSig (114,1) [(Ident "getRandomSeed" 0)] (ConstructorType (QualIdent Nothing (Ident "IO" 0)) [(ConstructorType (QualIdent Nothing (Ident "Int" 0)) [])]))
,(FunctionDecl (115,1) (Ident "getRandomSeed" 0) [(Equation (115,1) (FunLhs (Ident "getRandomSeed" 0) []) (SimpleRhs (116,3) (InfixApply (Variable (QualIdent (Just "Time") (Ident "getClockTime" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident ">>=" 0))) (Lambda [(VariablePattern (Ident "time" 89))] (InfixApply (Variable (QualIdent (Just "System") (Ident "getCPUTime" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident ">>=" 0))) (Lambda [(VariablePattern (Ident "msecs" 90))] (Let [(PatternDecl (118,7) (ParenPattern (ConstructorPattern (QualIdent (Just "Time") (Ident "CalendarTime" 0)) [(VariablePattern (Ident "y" 91)),(VariablePattern (Ident "mo" 91)),(VariablePattern (Ident "d" 91)),(VariablePattern (Ident "h" 91)),(VariablePattern (Ident "m" 91)),(VariablePattern (Ident "s" 91)),(VariablePattern (Ident "_" 92))])) (SimpleRhs (118,39) (Apply (Variable (QualIdent (Just "Time") (Ident "toUTCTime" 0))) (Variable (QualIdent Nothing (Ident "time" 89)))) []))] (Apply (Variable (QualIdent (Just "Prelude") (Ident "return" 0))) (Paren (InfixApply (Paren (InfixApply (InfixApply (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "y" 91))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (Variable (QualIdent Nothing (Ident "mo" 91)))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (Variable (QualIdent Nothing (Ident "d" 91)))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (Variable (QualIdent Nothing (Ident "h" 91)))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "m" 91))) (InfixOp (QualIdent (Just "Prelude") (Ident "*" 0))) (Variable (QualIdent Nothing (Ident "s" 91)))) (InfixOp (QualIdent (Just "Prelude") (Ident "*" 0))) (Variable (QualIdent Nothing (Ident "msecs" 90)))))) (InfixOp (QualIdent (Just "Prelude") (Ident "mod" 0))) (Variable (QualIdent (Just "Random") (Ident "mask" 0))))))))))) []))])
]