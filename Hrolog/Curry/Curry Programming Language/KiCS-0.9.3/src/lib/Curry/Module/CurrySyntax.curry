--- This module provides the necessary data structures to maintain the
--- parsed representation of a Curry program employed by the Curry frontend.
---
--- The representation differs from the one of the frontend in two respects:
--- 1. module names are repesented as strings and
--- 2. positions are replaced by polymorphic labels which are positions
---    without filename by default.
---
--- @author Wolfgang Lux, Martin Engelke, Sebastian Fischer
---
module CurrySyntax (

  Module(..), Pos, Position(..), Ident(..), QualIdent(..), ModuleIdent,

  ExportSpec(..), Export(..), ImportSpec(..), Import(..),

  Decl(..), ConstrDecl(..), NewConstrDecl(..),

  Infix(..), EvalAnnotation(..), CallConv(..),

  TypeExpr(..), Equation(..), Lhs(..), Rhs(..), CondExpr(..),

  Literal(..), ConstrTerm(..), Expression(..),

  InfixOp(..), Statement(..), Alt(..), Field(..),

  readCurry, readCurryWithParseOptions, readCurryFile, 

  writeCurryModule, writeCurryFile

  ) where

import Directory ( doesFileExist )
import ReadShowTerm ( readUnqualifiedTerm, showTerm )
import Distribution
  ( callFrontendWithParams , FrontendTarget(..)
  , FrontendParams, defaultParams, setQuiet
  , lookupFileInLoadPath )
import FileGoodies ( baseName )

--- Source positions: <code>(line,column)</code>
---
type Pos = (Int,Int)

--- The original type of source positions used by the frontend:
--- @cons Position <code>filepath line column</code>
---
data Position = Position String Int Int

--- An identifier is represented by its name and an additional number
--- to avoid name clashes. If the number is <code>0</code> the identifier 
--- does not need to be renamed: 
--- @cons Ident <code>name id</code>
---
data Ident = Ident String Int

--- Qualified identifiers have an attached module name, unqualified have not:
--- @cons UnqualIdent <code>ident</code>
--- @cons QualIdent <code>moduleIdent ident</code>
--- 
data QualIdent = UnqualIdent Ident | QualIdent ModuleIdent Ident

--- Module identifiers
---
type ModuleIdent = String

--- A module consists of a name, an optional export specification and a list of
--- declarations. 
--- The type variable can be used to label some parts of the module.
--- Initially, a module is labeld with positions of type <code>Pos</code>.
--- @cons Module <code>moduleIdent optExportSpec decls</code>
---
data Module a = Module ModuleIdent (Maybe (ExportSpec a)) [Decl a]

--- An export specification consists of a label and a list of exports:
--- @cons ExportSpec: <code>(ExportSpec lab exports)</code>
---
data ExportSpec a = Exporting a [Export]

--- There are different kinds of exports:
--- @cons Export <code>qident</code>: a qualified identifier
--- @cons ExportTypeWith <code>type idents</code>: some identifiers of a type
--- @cons ExportTypeAll <code>type</code>: all identifiers of a type
--- @cons ExportModule <code>moduleIdent</code>: a complete module
---
data Export
  = Export QualIdent
  | ExportTypeWith QualIdent [Ident]
  | ExportTypeAll QualIdent
  | ExportModule ModuleIdent

--- An import specification either imports or hides names from other modules:
--- @cons Importing <code>lab imports</code>
--- @cons Hiding <code>lab imports</code>
---
data ImportSpec a
  = Importing a [Import]
  | Hiding a [Import]

--- There are different kinds of imports:
--- @cons Import <code>ident</code>: an identifier of the imported/hidden module
--- @cons ImportTypeWith <code>type idents</code>: some identifiers of a type
--- from the imported/hidden module
--- @cons ImportTypeAll <code>type</code>: all identifiers of a type from the
--- imported/hidden module
---
data Import
  = Import Ident
  | ImportTypeWith Ident [Ident]
  | ImportTypeAll Ident

--- There are different kinds of declarations:
--- @cons ImportDecl <code>lab moduleIdent quali optAlias optImportSpec</code>:
--- An import declaration consists of a label, a module identifier, a flag
--- that determines whether the import is qualified, an optional alias and
--- an optional import specification.
--- @cons InfixDecl <code>lab assoc prec idents</code>:
--- An infix declaration consists of a label, an associativity declaration,
--- a precedence and a list of infix identifiers.
--- @cons DataDecl <code>lab name params consDecls</code>:
--- A data declaration consists of a label, the name of the datatype,
--- the names of the type parameters and a list of constructor declarations.
--- @cons NewtypeDecl <code>lab name params newConsDecl</code>:
--- A newtype declaration consists of a label, the name of the datatype,
--- the names of the type parameters and a "newconstructor declaration".
--- @cons TypeDecl <code>lab name params type</code>:
--- A type synonym declaration consists of a label, the name of the synonym,
--- the names of the type parameters and a type expression.
--- @cons TypeSig <code>lab idents type</code>:
--- A type signature consists of a label, a list of identifiers and the
--- specified type of these idetifiers.
--- @cons EvalAnnot <code>lab idents evalMode</code>:
--- An evaluation annotation consists of a label, a list of identifiers and
--- the specified evaluation mode of these identifiers.
--- @cons FunctionDecl <code>lab name eqs</code>:
--- A function declaration consists of a label, the name of the function
--- an a list of defining equations.
--- @cons ExternalDecl <code>lab callConvention optSpec name</code>:
--- The declaration of an external function consists of a label, a calling
--- convention, an optional specification and the name of the function.
--- @cons FlatExternalDecl <code>lab idents</code>:
--- A flat external declaration consists of a label and a list of
--- identifiers.
--- @cons PatternDecl <code>lab pat rhs</code>:
--- A pattern declaration consists of a label, a pattern and a
--- right-hand-side.
--- @cons ExtraVariables <code>lab idents</code>:
--- A declaration of free variables consists of a label and the list of
--- introduced names.
---
data Decl a
  = ImportDecl a ModuleIdent Bool (Maybe ModuleIdent) (Maybe (ImportSpec a))
  | InfixDecl a Infix Int [Ident]
  | DataDecl a Ident [Ident] [ConstrDecl a]
  | NewtypeDecl a Ident [Ident] (NewConstrDecl a)  -- omit?
  | TypeDecl a Ident [Ident] TypeExpr
  | TypeSig a [Ident] TypeExpr
  | EvalAnnot a [Ident] EvalAnnotation  -- deprecated?
  | FunctionDecl a Ident [Equation a]
  | ExternalDecl a CallConv (Maybe String) Ident TypeExpr
  | FlatExternalDecl a [Ident]  -- purpose?
  | PatternDecl a (ConstrTerm a) (Rhs a)
  | ExtraVariables a [Ident]

--- Constructors can be defined in prefix or in infix notation:
--- @cons ConstrDecl <code>lab idents name argTypes</code>
--- @cons ConOpDecl <code>lab idents ltype name rtype</code>
---
data ConstrDecl a  -- purpose of [Ident]?
  = ConstrDecl a [Ident] Ident [TypeExpr]
  | ConOpDecl a [Ident] TypeExpr Ident TypeExpr

--- A "newconstructor declaration" consists of a label, a list of identifiers,
--- the name of the new constructor and its argument type:
--- @cons NewConstrDecl <code>lab idents name type</code>
---
data NewConstrDecl a = NewConstrDecl a [Ident] Ident TypeExpr

--- An infix operator is either left-, right- or not associative.
---
data Infix = InfixL | InfixR | Infix

--- The evaluation mode of a function is either rigid or flexible.
---
data EvalAnnotation = EvalRigid | EvalChoice

--- There are two different kinds of calling conventions for external functions.
---
data CallConv = CallConvPrimitive | CallConvCCall

--- There are different types of type expressions:
--- @cons ConstructorType <code>name paramTypes</code>:
--- A constructed type consists of the name of the type constructor and the
--- list of parameter types.
--- @cons VariableType <code>name</code>:
--- A type variable is represented by its name.
--- @cons TupleType <code>paramTypes</code>:
--- A tuple type is represented by its parameter types.
--- @cons ListType <code>type</code>:
--- A list type is represented by the wrapped element type.
--- @cons ArrowType <code>dom ran</code>:
--- A functional type is represented by its domain and range types.
--- @cons RecordType <code>fieldTypes optType</code>:
--- A record type is represented by a list of type declarations for its
--- fields and an optional remaining type. A field type declaration specifies
--- the type of multiple field identifiers.
---
data TypeExpr
  = ConstructorType QualIdent [TypeExpr]
  | VariableType Ident
  | TupleType [TypeExpr]
  | ListType TypeExpr
  | ArrowType TypeExpr TypeExpr
  | RecordType [([Ident],TypeExpr)] (Maybe TypeExpr)

--- A defining equation consists of a label and the left- and right-hand-side
--- of the equation.
--- @cons Equation <code>lab lhs rhs</code>
---
data Equation a = Equation a (Lhs a) (Rhs a)

--- There are different kinds of left-hand-sides in equations:
--- @cons FunLhs <code>name patterns</code>:
--- The left-hand-side of a function rule consists of the name of the
--- function and a list of patterns.
--- @cons OpLhs <code>lpat opname rpat</code>:
--- The left-hand-side of an infix operator rule consists of the left
--- argument, the name and the right argument of the operator.
--- @cons ApLhs <code>lhs patterns</code>:
--- A left-hand-side can also be the application of another left-hand-side
--- to additional arguments. This is sometimes useful for operator declarations
--- like <pre>(f . g) x = f (g x)</pre>
---
data Lhs a
  = FunLhs Ident [ConstrTerm a]
  | OpLhs (ConstrTerm a) Ident (ConstrTerm a)
  | ApLhs (Lhs a) [ConstrTerm a]

--- The right-hand-side of a defining equation may be quarded and can contain
--- local declarations:
--- @cons SimpleRhs <code>lab body localDecls</code>
--- @cons GuardedRhs <code>condEqs localDecls</code>
---
data Rhs a
  = SimpleRhs a (Expression a) [Decl a]
  | GuardedRhs [CondExpr a] [Decl a]

--- A conditional equation consists of a label, a condition and the conditional
--- right-hand-side:
--- @cons CondExpr <code>lab cond expr</code>
---
data CondExpr a = CondExpr a (Expression a) (Expression a)

--- The are character, integer, float and string literals. <br/>
--- The Ident argument of an Int literal is used for supporting 
--- ad-hoc polymorphism on integer numbers. 
--- An integer literal can be used either as an integer number or
--- as a floating-point number depending on its context. The compiler uses
--- the identifier of the Int literal for maintaining its type.
---
data Literal
  = Char Char
  | Int Ident Int
  | Float Float
  | String String

--- There are different kinds of patterns:
--- @cons LiteralPattern <code>literal</code>:
--- literal patterns declare the literal to be matched.
--- @cons NegativePattern <code>ident literal</code>:
--- negative patterns consist of an identifier and a literal.
--- @cons VariablePattern <code>name</code>:
--- a named variable pattern
--- @cons ConstructorPattern <code>name argPats</code>:
--- A constructor pattern consists of the name of the matched constructor
--- and a list of argument patterns.
--- @cons InfixPattern <code>lpat opname rpat</code>:
--- An infix pattern consists of the left argument pattern, the
--- name of the infix constructor and the right argument pattern.
--- @cons ParenPattern <code>pat</code>:
--- A pattern enclosed in brackets.
--- @cons TuplePattern <code>argPats</code>:
--- A tuple pattern references a list of argument patterns.
--- @cons ListPattern <code>argPats</code>:
--- A list pattern references a list of argument patterns.
--- @cons AsPattern <code>name pat</code>:
--- An as pattern introduces a name for the expression matched by the
--- enclosed pattern.
--- @cons LazyPattern <code>pat</code>:
--- A lazy pattern is syntactic sugar for a pattern variable that is
--- matched in a local declaration.
--- @cons FunctionPattern <code>name argPats</code>:
--- A function pattern consists of the name of the pattern function and
--- a list of argument patterns.
--- @cons InfixFuncPattern <code>lpat op rpat</code>
--- An infix function pattern consists of the left argument pattern, the 
--- name of the infix operator and the right argument pattern.
--- @cons RecordPattern <code>fieldPats optPat</code>:
--- A record pattern consists of a list of field patterns and an optional
--- pattern for the remaining record.
---
data ConstrTerm a
  = LiteralPattern Literal
  | NegativePattern Ident Literal  -- purpose? e.g. f (-3) = 4
  | VariablePattern Ident
  | ConstructorPattern QualIdent [ConstrTerm a]
  | InfixPattern (ConstrTerm a) QualIdent (ConstrTerm a)
  | ParenPattern (ConstrTerm a)
  | TuplePattern [ConstrTerm a]
  | ListPattern [ConstrTerm a]
  | AsPattern Ident (ConstrTerm a)
  | LazyPattern (ConstrTerm a)  -- omit?
  | FunctionPattern QualIdent [ConstrTerm a]
  | InfixFuncPattern (ConstrTerm a) QualIdent (ConstrTerm a)
  | RecordPattern [Field a (ConstrTerm a)] (Maybe (ConstrTerm a))

--- There are different kinds of expressions:
--- @cons Literal <code>literal</code>
--- @cons Variable <code>name</code>
--- @cons Constructor <code>name</code>
--- @cons Paren <code>exp</code>:  bracketed expression
--- @cons Typed <code>exp type</code>:
--- expression with an annotated type (<code>exp :: type</code>)
--- @cons Tuple <code>exps</code>
--- @cons List  <code>exps</code>
--- @cons ListCompr <code>exp stmts</code>:
--- <code>[ exp | stmts ]</code>
--- @cons EnumFrom <code>start</code>: <code>[start..]</code>
--- @cons EnumFromThen <code>start next</code>: <code>[start,next..]</code>
--- @cons EnumFromTo <code>start stop</code>: <code>[start..stop]</code>
--- @cons EnumFromThenTo <code>start next stop</code>:
--- <code>[start,next..stop]</code>
--- @cons UnaryMinus <code>ident exp</code>
--- @cons Apply <code>exp1 exp2</code>
--- @cons InfixApply <code>lexp op rexp</code>
--- @cons LeftSection <code>exp op</code>:
--- infix operator partially applied to left argument
--- @cons RightSection <code>op exp</code>:
--- infix operator partially applied to right argument.
--- @cons Lambda <code>pats exp</code>:
--- lambda abstraction with multiple arguments and pattern matching
--- @cons Let <code>decls exp</code>
--- @cons Do <code>stmts exp</code>:
--- do expression with multiple statements and a final expression
--- @cons IfThenElse <code>cond texp fexp</code>
--- @cons Case <code>exp alts</code>
--- case distinction with matched expression and list of alternatives:
--- @cons RecordConstr <code>fieldDecls</code>:
--- <code>{ key1 = value1, key2 = value2 }</code>
--- @cons RecordSelection <code>exp field</code>: <code>exp -> field</code>
--- @cons RecordUpdate <code>fieldDecls exp</code>:
--- <code>{ key := value | exp }</code>
---
data Expression a
  = Literal Literal
  | Variable QualIdent
  | Constructor QualIdent
  | Paren (Expression a)
  | Typed (Expression a) TypeExpr
  | Tuple [(Expression a)]
  | List [(Expression a)]
  | ListCompr (Expression a) [Statement a]
  | EnumFrom (Expression a)
  | EnumFromThen (Expression a) (Expression a)
  | EnumFromTo (Expression a) (Expression a)
  | EnumFromThenTo (Expression a) (Expression a) (Expression a)
  | UnaryMinus Ident (Expression a)  -- purpose of Ident? type Int/Float?
  | Apply (Expression a) (Expression a)
  | InfixApply (Expression a) InfixOp (Expression a)
  | LeftSection (Expression a) InfixOp
  | RightSection InfixOp (Expression a)
  | Lambda [ConstrTerm a] (Expression a)
  | Let [Decl a] (Expression a)
  | Do [Statement a] (Expression a)
  | IfThenElse (Expression a) (Expression a) (Expression a)
  | Case (Expression a) [Alt a]
  | RecordConstr [Field a (Expression a)]
  | RecordSelection (Expression a) Ident
  | RecordUpdate [Field a (Expression a)] (Expression a)

--- Infix operators can be function and constructor symbols.
---
data InfixOp = InfixOp QualIdent | InfixConstr QualIdent

--- There are different kinds of statements used in list comprehensions and
--- do expressions:
--- @cons StmtExpr <code>exp</code>
--- @cons StmtDecl <code>decls</code>: local declarations
--- @cons StmtBind <code>pat rhs</code>: statement bind with pattern matching
---
data Statement a
  = StmtExpr (Expression a)
  | StmtDecl [Decl a]
  | StmtBind (ConstrTerm a) (Expression a)

--- A case alternative consists of a label, a pattern and a right-hand-side.
--- @cons Alt <code>lab pat rhs</code>
---
data Alt a = Alt a (ConstrTerm a) (Rhs a)

--- A field consists of a label, a field name and either a pattern or an 
--- expression.
--- @cons Field <code>lab name pat</code>, <code>Field lab name exp</code>
---
data Field a b = Field a Ident b


--- Parses a Curry program into its source representation.
--- The argument is the name of a module or the name of its source file.
--- If necessary, a current .cy file is generated in the same place where the
--- .curry or .lcurry file is found.
--- The result is a representation of a module labeled with positions.
---
readCurry :: String -> IO (Module Pos)
readCurry name = readCurryWithParseOptions name (setQuiet True defaultParams)

--- Parses a Curry program into its source representation using the specified
--- parameters to the frontend.
--- The argument is the name of a module or the name of its source file.
--- If necessary, a current .cy file is generated in the same place where the
--- .curry or .lcurry file is found.
--- The result is a representation of a module labeled with positions.
---
readCurryWithParseOptions :: String -> FrontendParams -> IO (Module Pos)
readCurryWithParseOptions name options = do
  parseCurry ".curry" `orElseDo` parseCurry ".lcurry"
  readCurryFile progname
 where
  progname = stripSuffix name
  parseCurry suffix
    = absoluteFileName (progname++suffix) >>= maybe (return Nothing) parse
  parse _ = callFrontendWithParams CY options progname >> return (Just ())

--- Parses a Curry program into its source representation.
--- The argument is the name of a module or the name of its source file.
--- The .cy file is neither generated if it does not exist nor updated if there
--- is a more recent source file!
---
readCurryFile :: String -> IO (Module Pos)
readCurryFile name = do
  filename <- absoluteFileName (progname++".cy")
  maybe (error $ progname ++ ".cy not found") read filename
 where
  progname = stripSuffix name
  read filename = do
    filecontents <- readFile filename
    return (readUnqualifiedTerm ["CurrySyntax","Prelude"] filecontents)

--- Writes the source representation of a Curry module labeled with positions
--- into a file MOD.cy where MOD is the name of the module.
---
writeCurryModule :: Module Pos -> IO ()
writeCurryModule m@(Module name _ _) = writeCurryFile (name++".cy") m

--- Writes the source representation of a Curry module labeled with positions
--- into a file with the given name.
---
writeCurryFile :: String -> Module Pos -> IO ()
writeCurryFile file m = writeFile file (showTerm m)


-- private auxiliary functions ------------------------------------------------

absoluteFileName :: String -> IO (Maybe String)
absoluteFileName name
  | name == baseName name = lookupFileInLoadPath name
  | otherwise = do
    exists <- doesFileExist name
    return (if exists then Just name else Nothing)

orElseDo :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
orElseDo ioma ioa = ioma >>= maybe ioa (return . Just)

stripSuffix :: String -> String
stripSuffix = takeWhile (/='.')

