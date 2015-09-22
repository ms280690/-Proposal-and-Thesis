Module "Time"
(Just (Exporting (8,12) [(Export (QualIdent Nothing (Ident "ClockTime" 0))),(ExportTypeAll (QualIdent Nothing (Ident "CalendarTime" 0))),(Export (QualIdent Nothing (Ident "ctYear" 0))),(Export (QualIdent Nothing (Ident "ctMonth" 0))),(Export (QualIdent Nothing (Ident "ctDay" 0))),(Export (QualIdent Nothing (Ident "ctHour" 0))),(Export (QualIdent Nothing (Ident "ctMin" 0))),(Export (QualIdent Nothing (Ident "ctSec" 0))),(Export (QualIdent Nothing (Ident "ctTZ" 0))),(Export (QualIdent Nothing (Ident "getClockTime" 0))),(Export (QualIdent Nothing (Ident "getLocalTime" 0))),(Export (QualIdent Nothing (Ident "toUTCTime" 0))),(Export (QualIdent Nothing (Ident "toClockTime" 0))),(Export (QualIdent Nothing (Ident "toCalendarTime" 0))),(Export (QualIdent Nothing (Ident "clockTimeToInt" 0))),(Export (QualIdent Nothing (Ident "calendarTimeToString" 0))),(Export (QualIdent Nothing (Ident "toDayString" 0))),(Export (QualIdent Nothing (Ident "toTimeString" 0))),(Export (QualIdent Nothing (Ident "addSeconds" 0))),(Export (QualIdent Nothing (Ident "addMinutes" 0))),(Export (QualIdent Nothing (Ident "addHours" 0))),(Export (QualIdent Nothing (Ident "addDays" 0))),(Export (QualIdent Nothing (Ident "addMonths" 0))),(Export (QualIdent Nothing (Ident "addYears" 0))),(Export (QualIdent Nothing (Ident "daysOfMonth" 0))),(Export (QualIdent Nothing (Ident "validDate" 0))),(Export (QualIdent Nothing (Ident "compareCalendarTime" 0))),(Export (QualIdent Nothing (Ident "compareClockTime" 0))),(Export (QualIdent Nothing (Ident "compareDate" 0)))]))
[(ImportDecl (1,1) "Prelude" False Nothing Nothing)
,(DataDecl (18,1) (Ident "ClockTime" 0) [] [(ConstrDecl (18,18) [] (Ident "CTime" 0) [(ConstructorType (QualIdent Nothing (Ident "Int" 0)) [])])])
,(DataDecl (24,1) (Ident "CalendarTime" 0) [] [(ConstrDecl (24,21) [] (Ident "CalendarTime" 0) [(ConstructorType (QualIdent Nothing (Ident "Int" 0)) []),(ConstructorType (QualIdent Nothing (Ident "Int" 0)) []),(ConstructorType (QualIdent Nothing (Ident "Int" 0)) []),(ConstructorType (QualIdent Nothing (Ident "Int" 0)) []),(ConstructorType (QualIdent Nothing (Ident "Int" 0)) []),(ConstructorType (QualIdent Nothing (Ident "Int" 0)) []),(ConstructorType (QualIdent Nothing (Ident "Int" 0)) [])])])
,(TypeSig (27,1) [(Ident "ctYear" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "Int" 0)) [])))
,(FunctionDecl (28,1) (Ident "ctYear" 0) [(Equation (28,1) (FunLhs (Ident "ctYear" 0) [(ParenPattern (ConstructorPattern (QualIdent (Just "Time") (Ident "CalendarTime" 0)) [(VariablePattern (Ident "y" 2)),(VariablePattern (Ident "_" 3)),(VariablePattern (Ident "_" 4)),(VariablePattern (Ident "_" 5)),(VariablePattern (Ident "_" 6)),(VariablePattern (Ident "_" 7)),(VariablePattern (Ident "_" 8))]))]) (SimpleRhs (28,39) (Variable (QualIdent Nothing (Ident "y" 2))) []))])
,(TypeSig (31,1) [(Ident "ctMonth" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "Int" 0)) [])))
,(FunctionDecl (32,1) (Ident "ctMonth" 0) [(Equation (32,1) (FunLhs (Ident "ctMonth" 0) [(ParenPattern (ConstructorPattern (QualIdent (Just "Time") (Ident "CalendarTime" 0)) [(VariablePattern (Ident "_" 11)),(VariablePattern (Ident "m" 10)),(VariablePattern (Ident "_" 12)),(VariablePattern (Ident "_" 13)),(VariablePattern (Ident "_" 14)),(VariablePattern (Ident "_" 15)),(VariablePattern (Ident "_" 16))]))]) (SimpleRhs (32,40) (Variable (QualIdent Nothing (Ident "m" 10))) []))])
,(TypeSig (35,1) [(Ident "ctDay" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "Int" 0)) [])))
,(FunctionDecl (36,1) (Ident "ctDay" 0) [(Equation (36,1) (FunLhs (Ident "ctDay" 0) [(ParenPattern (ConstructorPattern (QualIdent (Just "Time") (Ident "CalendarTime" 0)) [(VariablePattern (Ident "_" 19)),(VariablePattern (Ident "_" 20)),(VariablePattern (Ident "d" 18)),(VariablePattern (Ident "_" 21)),(VariablePattern (Ident "_" 22)),(VariablePattern (Ident "_" 23)),(VariablePattern (Ident "_" 24))]))]) (SimpleRhs (36,38) (Variable (QualIdent Nothing (Ident "d" 18))) []))])
,(TypeSig (39,1) [(Ident "ctHour" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "Int" 0)) [])))
,(FunctionDecl (40,1) (Ident "ctHour" 0) [(Equation (40,1) (FunLhs (Ident "ctHour" 0) [(ParenPattern (ConstructorPattern (QualIdent (Just "Time") (Ident "CalendarTime" 0)) [(VariablePattern (Ident "_" 27)),(VariablePattern (Ident "_" 28)),(VariablePattern (Ident "_" 29)),(VariablePattern (Ident "h" 26)),(VariablePattern (Ident "_" 30)),(VariablePattern (Ident "_" 31)),(VariablePattern (Ident "_" 32))]))]) (SimpleRhs (40,39) (Variable (QualIdent Nothing (Ident "h" 26))) []))])
,(TypeSig (43,1) [(Ident "ctMin" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "Int" 0)) [])))
,(FunctionDecl (44,1) (Ident "ctMin" 0) [(Equation (44,1) (FunLhs (Ident "ctMin" 0) [(ParenPattern (ConstructorPattern (QualIdent (Just "Time") (Ident "CalendarTime" 0)) [(VariablePattern (Ident "_" 35)),(VariablePattern (Ident "_" 36)),(VariablePattern (Ident "_" 37)),(VariablePattern (Ident "_" 38)),(VariablePattern (Ident "m" 34)),(VariablePattern (Ident "_" 39)),(VariablePattern (Ident "_" 40))]))]) (SimpleRhs (44,38) (Variable (QualIdent Nothing (Ident "m" 34))) []))])
,(TypeSig (47,1) [(Ident "ctSec" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "Int" 0)) [])))
,(FunctionDecl (48,1) (Ident "ctSec" 0) [(Equation (48,1) (FunLhs (Ident "ctSec" 0) [(ParenPattern (ConstructorPattern (QualIdent (Just "Time") (Ident "CalendarTime" 0)) [(VariablePattern (Ident "_" 43)),(VariablePattern (Ident "_" 44)),(VariablePattern (Ident "_" 45)),(VariablePattern (Ident "_" 46)),(VariablePattern (Ident "_" 47)),(VariablePattern (Ident "s" 42)),(VariablePattern (Ident "_" 48))]))]) (SimpleRhs (48,38) (Variable (QualIdent Nothing (Ident "s" 42))) []))])
,(TypeSig (52,1) [(Ident "ctTZ" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "Int" 0)) [])))
,(FunctionDecl (53,1) (Ident "ctTZ" 0) [(Equation (53,1) (FunLhs (Ident "ctTZ" 0) [(ParenPattern (ConstructorPattern (QualIdent (Just "Time") (Ident "CalendarTime" 0)) [(VariablePattern (Ident "_" 51)),(VariablePattern (Ident "_" 52)),(VariablePattern (Ident "_" 53)),(VariablePattern (Ident "_" 54)),(VariablePattern (Ident "_" 55)),(VariablePattern (Ident "_" 56)),(VariablePattern (Ident "tz" 50))]))]) (SimpleRhs (53,38) (Variable (QualIdent Nothing (Ident "tz" 50))) []))])
,(TypeSig (57,1) [(Ident "getClockTime" 0)] (ConstructorType (QualIdent Nothing (Ident "IO" 0)) [(ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) [])]))
,(FlatExternalDecl (58,1) [(Ident "getClockTime" 0)])
,(TypeSig (61,1) [(Ident "getLocalTime" 0)] (ConstructorType (QualIdent Nothing (Ident "IO" 0)) [(ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) [])]))
,(FunctionDecl (62,1) (Ident "getLocalTime" 0) [(Equation (62,1) (FunLhs (Ident "getLocalTime" 0) []) (SimpleRhs (62,16) (Do [(StmtBind (VariablePattern (Ident "ctime" 60)) (Variable (QualIdent (Just "Time") (Ident "getClockTime" 0))))] (Apply (Variable (QualIdent (Just "Time") (Ident "toCalendarTime" 0))) (Variable (QualIdent Nothing (Ident "ctime" 60))))) []))])
,(TypeSig (69,1) [(Ident "clockTimeToInt" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "Int" 0)) [])))
,(FunctionDecl (70,1) (Ident "clockTimeToInt" 0) [(Equation (70,1) (FunLhs (Ident "clockTimeToInt" 0) [(ParenPattern (ConstructorPattern (QualIdent (Just "Time") (Ident "CTime" 0)) [(VariablePattern (Ident "i" 61))]))]) (SimpleRhs (70,28) (Variable (QualIdent Nothing (Ident "i" 61))) []))])
,(TypeSig (75,1) [(Ident "toCalendarTime" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "IO" 0)) [(ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) [])])))
,(FunctionDecl (76,1) (Ident "toCalendarTime" 0) [(Equation (76,1) (FunLhs (Ident "toCalendarTime" 0) [(VariablePattern (Ident "ctime" 63))]) (SimpleRhs (76,24) (InfixApply (Variable (QualIdent (Just "Time") (Ident "prim_toCalendarTime" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "$##" 0))) (Variable (QualIdent Nothing (Ident "ctime" 63)))) []))])
,(TypeSig (78,1) [(Ident "prim_toCalendarTime" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "IO" 0)) [(ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) [])])))
,(FlatExternalDecl (79,1) [(Ident "prim_toCalendarTime" 0)])
,(TypeSig (83,1) [(Ident "toUTCTime" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) [])))
,(FunctionDecl (84,1) (Ident "toUTCTime" 0) [(Equation (84,1) (FunLhs (Ident "toUTCTime" 0) [(VariablePattern (Ident "ctime" 65))]) (SimpleRhs (84,19) (InfixApply (Variable (QualIdent (Just "Time") (Ident "prim_toUTCTime" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "$##" 0))) (Variable (QualIdent Nothing (Ident "ctime" 65)))) []))])
,(TypeSig (86,1) [(Ident "prim_toUTCTime" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) [])))
,(FlatExternalDecl (87,1) [(Ident "prim_toUTCTime" 0)])
,(TypeSig (90,1) [(Ident "toClockTime" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) [])))
,(FunctionDecl (91,1) (Ident "toClockTime" 0) [(Equation (91,1) (FunLhs (Ident "toClockTime" 0) [(VariablePattern (Ident "d" 67))]) (SimpleRhs (91,17) (InfixApply (Variable (QualIdent (Just "Time") (Ident "prim_toClockTime" 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "$##" 0))) (Variable (QualIdent Nothing (Ident "d" 67)))) []))])
,(TypeSig (93,1) [(Ident "prim_toClockTime" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) [])))
,(FlatExternalDecl (94,1) [(Ident "prim_toClockTime" 0)])
,(TypeSig (97,1) [(Ident "calendarTimeToString" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])))
,(FunctionDecl (98,1) (Ident "calendarTimeToString" 0) [(Equation (98,1) (FunLhs (Ident "calendarTimeToString" 0) [(AsPattern (Ident "ctime" 69) (ParenPattern (ConstructorPattern (QualIdent (Just "Time") (Ident "CalendarTime" 0)) [(VariablePattern (Ident "y" 69)),(VariablePattern (Ident "mo" 69)),(VariablePattern (Ident "d" 69)),(VariablePattern (Ident "_" 70)),(VariablePattern (Ident "_" 71)),(VariablePattern (Ident "_" 72)),(VariablePattern (Ident "_" 73))])))]) (SimpleRhs (99,5) (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "shortMonths" 74))) (InfixOp (QualIdent (Just "Prelude") (Ident "!!" 0))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "mo" 69))) (InfixOp (QualIdent (Just "Prelude") (Ident "-" 0))) (Literal (Int (Ident "_" 76) 1))))) (InfixOp (QualIdent (Just "Prelude") (Ident "++" 0))) (InfixApply (Literal (String " ")) (InfixOp (QualIdent (Just "Prelude") (Ident "++" 0))) (InfixApply (Apply (Variable (QualIdent (Just "Prelude") (Ident "show" 0))) (Variable (QualIdent Nothing (Ident "d" 69)))) (InfixOp (QualIdent (Just "Prelude") (Ident "++" 0))) (InfixApply (Literal (String " ")) (InfixOp (QualIdent (Just "Prelude") (Ident "++" 0))) (InfixApply (Apply (Variable (QualIdent (Just "Time") (Ident "toTimeString" 0))) (Variable (QualIdent Nothing (Ident "ctime" 69)))) (InfixOp (QualIdent (Just "Prelude") (Ident "++" 0))) (InfixApply (Literal (String " ")) (InfixOp (QualIdent (Just "Prelude") (Ident "++" 0))) (Apply (Variable (QualIdent (Just "Prelude") (Ident "show" 0))) (Variable (QualIdent Nothing (Ident "y" 69)))))))))) [(PatternDecl (101,9) (VariablePattern (Ident "shortMonths" 74)) (SimpleRhs (101,23) (List [(Literal (String "Jan")),(Literal (String "Feb")),(Literal (String "Mar")),(Literal (String "Apr")),(Literal (String "May")),(Literal (String "Jun")),(Literal (String "Jul")),(Literal (String "Aug")),(Literal (String "Sep")),(Literal (String "Oct")),(Literal (String "Nov")),(Literal (String "Dec"))]) []))]))])
,(TypeSig (106,1) [(Ident "toDayString" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])))
,(FunctionDecl (107,1) (Ident "toDayString" 0) [(Equation (107,1) (FunLhs (Ident "toDayString" 0) [(ParenPattern (ConstructorPattern (QualIdent (Just "Time") (Ident "CalendarTime" 0)) [(VariablePattern (Ident "y" 77)),(VariablePattern (Ident "mo" 77)),(VariablePattern (Ident "d" 77)),(VariablePattern (Ident "_" 78)),(VariablePattern (Ident "_" 79)),(VariablePattern (Ident "_" 80)),(VariablePattern (Ident "_" 81))]))]) (SimpleRhs (108,5) (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "longMonths" 82))) (InfixOp (QualIdent (Just "Prelude") (Ident "!!" 0))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "mo" 77))) (InfixOp (QualIdent (Just "Prelude") (Ident "-" 0))) (Literal (Int (Ident "_" 84) 1))))) (InfixOp (QualIdent (Just "Prelude") (Ident "++" 0))) (InfixApply (Literal (String " ")) (InfixOp (QualIdent (Just "Prelude") (Ident "++" 0))) (InfixApply (Apply (Variable (QualIdent (Just "Prelude") (Ident "show" 0))) (Variable (QualIdent Nothing (Ident "d" 77)))) (InfixOp (QualIdent (Just "Prelude") (Ident "++" 0))) (InfixApply (Literal (String ", ")) (InfixOp (QualIdent (Just "Prelude") (Ident "++" 0))) (Apply (Variable (QualIdent (Just "Prelude") (Ident "show" 0))) (Variable (QualIdent Nothing (Ident "y" 77)))))))) [(PatternDecl (109,9) (VariablePattern (Ident "longMonths" 82)) (SimpleRhs (109,22) (List [(Literal (String "January")),(Literal (String "February")),(Literal (String "March")),(Literal (String "April")),(Literal (String "May")),(Literal (String "June")),(Literal (String "July")),(Literal (String "August")),(Literal (String "September")),(Literal (String "October")),(Literal (String "November")),(Literal (String "December"))]) []))]))])
,(TypeSig (113,1) [(Ident "toTimeString" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "String" 0)) [])))
,(FunctionDecl (114,1) (Ident "toTimeString" 0) [(Equation (114,1) (FunLhs (Ident "toTimeString" 0) [(ParenPattern (ConstructorPattern (QualIdent (Just "Time") (Ident "CalendarTime" 0)) [(VariablePattern (Ident "_" 86)),(VariablePattern (Ident "_" 87)),(VariablePattern (Ident "_" 88)),(VariablePattern (Ident "h" 85)),(VariablePattern (Ident "mi" 85)),(VariablePattern (Ident "s" 85)),(VariablePattern (Ident "_" 89))]))]) (SimpleRhs (115,4) (InfixApply (Apply (Variable (QualIdent Nothing (Ident "digit2" 90))) (Variable (QualIdent Nothing (Ident "h" 85)))) (InfixOp (QualIdent (Just "Prelude") (Ident "++" 0))) (InfixApply (Literal (String ":")) (InfixOp (QualIdent (Just "Prelude") (Ident "++" 0))) (InfixApply (Apply (Variable (QualIdent Nothing (Ident "digit2" 90))) (Variable (QualIdent Nothing (Ident "mi" 85)))) (InfixOp (QualIdent (Just "Prelude") (Ident "++" 0))) (InfixApply (Literal (String ":")) (InfixOp (QualIdent (Just "Prelude") (Ident "++" 0))) (Apply (Variable (QualIdent Nothing (Ident "digit2" 90))) (Variable (QualIdent Nothing (Ident "s" 85)))))))) [(FunctionDecl (116,9) (Ident "digit2" 90) [(Equation (116,9) (FunLhs (Ident "digit2" 90) [(VariablePattern (Ident "n" 91))]) (SimpleRhs (116,20) (IfThenElse (InfixApply (Variable (QualIdent Nothing (Ident "n" 91))) (InfixOp (QualIdent (Just "Prelude") (Ident "<" 0))) (Literal (Int (Ident "_" 93) 10))) (List [(Literal (Char '0')),(Apply (Variable (QualIdent (Just "Prelude") (Ident "chr" 0))) (Paren (InfixApply (Apply (Variable (QualIdent (Just "Prelude") (Ident "ord" 0))) (Literal (Char '0'))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (Variable (QualIdent Nothing (Ident "n" 91))))))]) (Apply (Variable (QualIdent (Just "Prelude") (Ident "show" 0))) (Variable (QualIdent Nothing (Ident "n" 91))))) []))])]))])
,(TypeSig (120,1) [(Ident "addSeconds" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "Int" 0)) []) (ArrowType (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) []))))
,(FunctionDecl (121,1) (Ident "addSeconds" 0) [(Equation (121,1) (FunLhs (Ident "addSeconds" 0) [(VariablePattern (Ident "n" 94)),(ParenPattern (ConstructorPattern (QualIdent (Just "Time") (Ident "CTime" 0)) [(VariablePattern (Ident "ctime" 94))]))]) (SimpleRhs (121,30) (Apply (Constructor (QualIdent (Just "Time") (Ident "CTime" 0))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "ctime" 94))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (Variable (QualIdent Nothing (Ident "n" 94)))))) []))])
,(TypeSig (124,1) [(Ident "addMinutes" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "Int" 0)) []) (ArrowType (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) []))))
,(FunctionDecl (125,1) (Ident "addMinutes" 0) [(Equation (125,1) (FunLhs (Ident "addMinutes" 0) [(VariablePattern (Ident "n" 96)),(ParenPattern (ConstructorPattern (QualIdent (Just "Time") (Ident "CTime" 0)) [(VariablePattern (Ident "ctime" 96))]))]) (SimpleRhs (125,30) (Apply (Constructor (QualIdent (Just "Time") (Ident "CTime" 0))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "ctime" 96))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "n" 96))) (InfixOp (QualIdent (Just "Prelude") (Ident "*" 0))) (Literal (Int (Ident "_" 98) 60))))))) []))])
,(TypeSig (128,1) [(Ident "addHours" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "Int" 0)) []) (ArrowType (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) []))))
,(FunctionDecl (129,1) (Ident "addHours" 0) [(Equation (129,1) (FunLhs (Ident "addHours" 0) [(VariablePattern (Ident "n" 99)),(ParenPattern (ConstructorPattern (QualIdent (Just "Time") (Ident "CTime" 0)) [(VariablePattern (Ident "ctime" 99))]))]) (SimpleRhs (129,28) (Apply (Constructor (QualIdent (Just "Time") (Ident "CTime" 0))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "ctime" 99))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "n" 99))) (InfixOp (QualIdent (Just "Prelude") (Ident "*" 0))) (Literal (Int (Ident "_" 101) 3600))))))) []))])
,(TypeSig (132,1) [(Ident "addDays" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "Int" 0)) []) (ArrowType (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) []))))
,(FunctionDecl (133,1) (Ident "addDays" 0) [(Equation (133,1) (FunLhs (Ident "addDays" 0) [(VariablePattern (Ident "n" 102)),(ParenPattern (ConstructorPattern (QualIdent (Just "Time") (Ident "CTime" 0)) [(VariablePattern (Ident "ctime" 102))]))]) (SimpleRhs (133,27) (Apply (Constructor (QualIdent (Just "Time") (Ident "CTime" 0))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "ctime" 102))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "n" 102))) (InfixOp (QualIdent (Just "Prelude") (Ident "*" 0))) (Literal (Int (Ident "_" 104) 86400))))))) []))])
,(TypeSig (136,1) [(Ident "addMonths" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "Int" 0)) []) (ArrowType (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) []))))
,(FunctionDecl (137,1) (Ident "addMonths" 0) [(Equation (137,1) (FunLhs (Ident "addMonths" 0) [(VariablePattern (Ident "n" 105)),(VariablePattern (Ident "ctime" 105))]) (SimpleRhs (138,2) (Let [(PatternDecl (138,6) (ConstructorPattern (QualIdent (Just "Time") (Ident "CalendarTime" 0)) [(VariablePattern (Ident "y" 107)),(VariablePattern (Ident "mo" 107)),(VariablePattern (Ident "d" 107)),(VariablePattern (Ident "h" 107)),(VariablePattern (Ident "mi" 107)),(VariablePattern (Ident "s" 107)),(VariablePattern (Ident "tz" 107))]) (SimpleRhs (138,38) (Apply (Variable (QualIdent (Just "Time") (Ident "toUTCTime" 0))) (Variable (QualIdent Nothing (Ident "ctime" 105)))) [])),(PatternDecl (139,6) (VariablePattern (Ident "nmo" 107)) (SimpleRhs (139,12) (InfixApply (InfixApply (Paren (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "mo" 107))) (InfixOp (QualIdent (Just "Prelude") (Ident "-" 0))) (Literal (Int (Ident "_" 110) 1))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (Variable (QualIdent Nothing (Ident "n" 105))))) (InfixOp (QualIdent (Just "Prelude") (Ident "mod" 0))) (Literal (Int (Ident "_" 111) 12))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (Literal (Int (Ident "_" 112) 1))) []))] (IfThenElse (InfixApply (Variable (QualIdent Nothing (Ident "nmo" 107))) (InfixOp (QualIdent (Just "Prelude") (Ident ">" 0))) (Literal (Int (Ident "_" 113) 0))) (Apply (Apply (Variable (QualIdent (Just "Time") (Ident "addYears" 0))) (Paren (InfixApply (Paren (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "mo" 107))) (InfixOp (QualIdent (Just "Prelude") (Ident "-" 0))) (Literal (Int (Ident "_" 114) 1))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (Variable (QualIdent Nothing (Ident "n" 105))))) (InfixOp (QualIdent (Just "Prelude") (Ident "div" 0))) (Literal (Int (Ident "_" 115) 12))))) (Paren (Apply (Variable (QualIdent (Just "Time") (Ident "toClockTime" 0))) (Paren (Apply (Apply (Apply (Apply (Apply (Apply (Apply (Constructor (QualIdent (Just "Time") (Ident "CalendarTime" 0))) (Variable (QualIdent Nothing (Ident "y" 107)))) (Variable (QualIdent Nothing (Ident "nmo" 107)))) (Variable (QualIdent Nothing (Ident "d" 107)))) (Variable (QualIdent Nothing (Ident "h" 107)))) (Variable (QualIdent Nothing (Ident "mi" 107)))) (Variable (QualIdent Nothing (Ident "s" 107)))) (Variable (QualIdent Nothing (Ident "tz" 107)))))))) (Apply (Apply (Variable (QualIdent (Just "Time") (Ident "addYears" 0))) (Paren (InfixApply (InfixApply (Paren (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "mo" 107))) (InfixOp (QualIdent (Just "Prelude") (Ident "-" 0))) (Literal (Int (Ident "_" 116) 1))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (Variable (QualIdent Nothing (Ident "n" 105))))) (InfixOp (QualIdent (Just "Prelude") (Ident "div" 0))) (Literal (Int (Ident "_" 117) 12))) (InfixOp (QualIdent (Just "Prelude") (Ident "-" 0))) (Literal (Int (Ident "_" 118) 1))))) (Paren (Apply (Variable (QualIdent (Just "Time") (Ident "toClockTime" 0))) (Paren (Apply (Apply (Apply (Apply (Apply (Apply (Apply (Constructor (QualIdent (Just "Time") (Ident "CalendarTime" 0))) (Variable (QualIdent Nothing (Ident "y" 107)))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "nmo" 107))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (Literal (Int (Ident "_" 119) 12))))) (Variable (QualIdent Nothing (Ident "d" 107)))) (Variable (QualIdent Nothing (Ident "h" 107)))) (Variable (QualIdent Nothing (Ident "mi" 107)))) (Variable (QualIdent Nothing (Ident "s" 107)))) (Variable (QualIdent Nothing (Ident "tz" 107)))))))))) []))])
,(TypeSig (148,1) [(Ident "addYears" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "Int" 0)) []) (ArrowType (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) []))))
,(FunctionDecl (149,1) (Ident "addYears" 0) [(Equation (149,1) (FunLhs (Ident "addYears" 0) [(VariablePattern (Ident "n" 120)),(VariablePattern (Ident "ctime" 120))]) (SimpleRhs (149,20) (IfThenElse (InfixApply (Variable (QualIdent Nothing (Ident "n" 120))) (InfixOp (QualIdent (Just "Prelude") (Ident "==" 0))) (Literal (Int (Ident "_" 122) 0))) (Variable (QualIdent Nothing (Ident "ctime" 120))) (Let [(PatternDecl (150,7) (ConstructorPattern (QualIdent (Just "Time") (Ident "CalendarTime" 0)) [(VariablePattern (Ident "y" 123)),(VariablePattern (Ident "mo" 123)),(VariablePattern (Ident "d" 123)),(VariablePattern (Ident "h" 123)),(VariablePattern (Ident "mi" 123)),(VariablePattern (Ident "s" 123)),(VariablePattern (Ident "tz" 123))]) (SimpleRhs (150,39) (Apply (Variable (QualIdent (Just "Time") (Ident "toUTCTime" 0))) (Variable (QualIdent Nothing (Ident "ctime" 120)))) []))] (Apply (Variable (QualIdent (Just "Time") (Ident "toClockTime" 0))) (Paren (Apply (Apply (Apply (Apply (Apply (Apply (Apply (Constructor (QualIdent (Just "Time") (Ident "CalendarTime" 0))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "y" 123))) (InfixOp (QualIdent (Just "Prelude") (Ident "+" 0))) (Variable (QualIdent Nothing (Ident "n" 120)))))) (Variable (QualIdent Nothing (Ident "mo" 123)))) (Variable (QualIdent Nothing (Ident "d" 123)))) (Variable (QualIdent Nothing (Ident "h" 123)))) (Variable (QualIdent Nothing (Ident "mi" 123)))) (Variable (QualIdent Nothing (Ident "s" 123)))) (Variable (QualIdent Nothing (Ident "tz" 123)))))))) []))])
,(TypeSig (154,1) [(Ident "daysOfMonth" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "Int" 0)) []) (ArrowType (ConstructorType (QualIdent Nothing (Ident "Int" 0)) []) (ConstructorType (QualIdent Nothing (Ident "Int" 0)) []))))
,(FunctionDecl (155,1) (Ident "daysOfMonth" 0) [(Equation (155,1) (FunLhs (Ident "daysOfMonth" 0) [(VariablePattern (Ident "mo" 125)),(VariablePattern (Ident "yr" 125))]) (SimpleRhs (156,3) (IfThenElse (InfixApply (Variable (QualIdent Nothing (Ident "mo" 125))) (InfixOp (QualIdent (Just "Prelude") (Ident "/=" 0))) (Literal (Int (Ident "_" 127) 2))) (InfixApply (List [(Literal (Int (Ident "_" 128) 31)),(Literal (Int (Ident "_" 129) 28)),(Literal (Int (Ident "_" 130) 31)),(Literal (Int (Ident "_" 131) 30)),(Literal (Int (Ident "_" 132) 31)),(Literal (Int (Ident "_" 133) 30)),(Literal (Int (Ident "_" 134) 31)),(Literal (Int (Ident "_" 135) 31)),(Literal (Int (Ident "_" 136) 30)),(Literal (Int (Ident "_" 137) 31)),(Literal (Int (Ident "_" 138) 30)),(Literal (Int (Ident "_" 139) 31))]) (InfixOp (QualIdent (Just "Prelude") (Ident "!!" 0))) (Paren (InfixApply (Variable (QualIdent Nothing (Ident "mo" 125))) (InfixOp (QualIdent (Just "Prelude") (Ident "-" 0))) (Literal (Int (Ident "_" 140) 1))))) (IfThenElse (InfixApply (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "yr" 125))) (InfixOp (QualIdent (Just "Prelude") (Ident "mod" 0))) (Literal (Int (Ident "_" 141) 4))) (InfixOp (QualIdent (Just "Prelude") (Ident "==" 0))) (Literal (Int (Ident "_" 142) 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "&&" 0))) (Paren (InfixApply (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "yr" 125))) (InfixOp (QualIdent (Just "Prelude") (Ident "mod" 0))) (Literal (Int (Ident "_" 143) 100))) (InfixOp (QualIdent (Just "Prelude") (Ident "/=" 0))) (Literal (Int (Ident "_" 144) 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "||" 0))) (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "yr" 125))) (InfixOp (QualIdent (Just "Prelude") (Ident "mod" 0))) (Literal (Int (Ident "_" 145) 400))) (InfixOp (QualIdent (Just "Prelude") (Ident "==" 0))) (Literal (Int (Ident "_" 146) 0)))))) (Literal (Int (Ident "_" 147) 29)) (Literal (Int (Ident "_" 148) 28)))) []))])
,(TypeSig (163,1) [(Ident "validDate" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "Int" 0)) []) (ArrowType (ConstructorType (QualIdent Nothing (Ident "Int" 0)) []) (ArrowType (ConstructorType (QualIdent Nothing (Ident "Int" 0)) []) (ConstructorType (QualIdent Nothing (Ident "Bool" 0)) [])))))
,(FunctionDecl (164,1) (Ident "validDate" 0) [(Equation (164,1) (FunLhs (Ident "validDate" 0) [(VariablePattern (Ident "y" 149)),(VariablePattern (Ident "m" 149)),(VariablePattern (Ident "d" 149))]) (SimpleRhs (164,19) (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "m" 149))) (InfixOp (QualIdent (Just "Prelude") (Ident ">" 0))) (Literal (Int (Ident "_" 151) 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "&&" 0))) (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "m" 149))) (InfixOp (QualIdent (Just "Prelude") (Ident "<" 0))) (Literal (Int (Ident "_" 152) 13))) (InfixOp (QualIdent (Just "Prelude") (Ident "&&" 0))) (InfixApply (InfixApply (Variable (QualIdent Nothing (Ident "d" 149))) (InfixOp (QualIdent (Just "Prelude") (Ident ">" 0))) (Literal (Int (Ident "_" 153) 0))) (InfixOp (QualIdent (Just "Prelude") (Ident "&&" 0))) (InfixApply (Variable (QualIdent Nothing (Ident "d" 149))) (InfixOp (QualIdent (Just "Prelude") (Ident "<=" 0))) (Apply (Apply (Variable (QualIdent (Just "Time") (Ident "daysOfMonth" 0))) (Variable (QualIdent Nothing (Ident "m" 149)))) (Variable (QualIdent Nothing (Ident "y" 149)))))))) []))])
,(TypeSig (167,1) [(Ident "compareDate" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) []) (ArrowType (ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "Ordering" 0)) []))))
,(FunctionDecl (168,1) (Ident "compareDate" 0) [(Equation (168,1) (FunLhs (Ident "compareDate" 0) []) (SimpleRhs (168,15) (Variable (QualIdent (Just "Time") (Ident "compareCalendarTime" 0))) []))])
,(TypeSig (171,1) [(Ident "compareCalendarTime" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) []) (ArrowType (ConstructorType (QualIdent Nothing (Ident "CalendarTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "Ordering" 0)) []))))
,(FunctionDecl (172,1) (Ident "compareCalendarTime" 0) [(Equation (172,1) (FunLhs (Ident "compareCalendarTime" 0) [(VariablePattern (Ident "ct1" 156)),(VariablePattern (Ident "ct2" 156))]) (SimpleRhs (173,3) (Apply (Apply (Variable (QualIdent (Just "Time") (Ident "compareClockTime" 0))) (Paren (Apply (Variable (QualIdent (Just "Time") (Ident "toClockTime" 0))) (Variable (QualIdent Nothing (Ident "ct1" 156)))))) (Paren (Apply (Variable (QualIdent (Just "Time") (Ident "toClockTime" 0))) (Variable (QualIdent Nothing (Ident "ct2" 156)))))) []))])
,(TypeSig (176,1) [(Ident "compareClockTime" 0)] (ArrowType (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) []) (ArrowType (ConstructorType (QualIdent Nothing (Ident "ClockTime" 0)) []) (ConstructorType (QualIdent Nothing (Ident "Ordering" 0)) []))))
,(FunctionDecl (177,1) (Ident "compareClockTime" 0) [(Equation (177,1) (FunLhs (Ident "compareClockTime" 0) [(ParenPattern (ConstructorPattern (QualIdent (Just "Time") (Ident "CTime" 0)) [(VariablePattern (Ident "time1" 158))])),(ParenPattern (ConstructorPattern (QualIdent (Just "Time") (Ident "CTime" 0)) [(VariablePattern (Ident "time2" 158))]))]) (GuardedRhs [(CondExpr (178,2) (InfixApply (Variable (QualIdent Nothing (Ident "time1" 158))) (InfixOp (QualIdent (Just "Prelude") (Ident "<" 0))) (Variable (QualIdent Nothing (Ident "time2" 158)))) (Constructor (QualIdent (Just "Prelude") (Ident "LT" 0)))),(CondExpr (179,2) (InfixApply (Variable (QualIdent Nothing (Ident "time1" 158))) (InfixOp (QualIdent (Just "Prelude") (Ident ">" 0))) (Variable (QualIdent Nothing (Ident "time2" 158)))) (Constructor (QualIdent (Just "Prelude") (Ident "GT" 0)))),(CondExpr (180,2) (Variable (QualIdent (Just "Prelude") (Ident "otherwise" 0))) (Constructor (QualIdent (Just "Prelude") (Ident "EQ" 0))))] []))])
]
