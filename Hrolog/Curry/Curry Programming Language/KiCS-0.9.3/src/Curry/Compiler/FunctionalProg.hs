------------------------------------------------------------------------------
--- Library to support meta-programming in Curry.
---
--- This library contains a definition for representing Haskell programs
--- in Curry (type "CurryProg") and an I/O action to read Curry programs and
--- transform them into this abstract representation (function "readCurry").
---
--- Note this defines a slightly new format for AbstractCurry
--- in comparison to the first proposal of 2003.
---
--- The Difference to AbstractCurry for now is only the deriving construct.
---
--- Assumption: an abstract Curry program is stored in file prog.acy
---             and translated with the parser by "parsecurry -acy prog".
---
--- @author Michael Hanus, Bernd Braﬂel
--- @version August 2005
------------------------------------------------------------------------------

module Curry.Compiler.FunctionalProg where

------------------------------------------------------------------------------
-- Definition of data types for representing abstract Curry programs:
-- ==================================================================

--- Data type for representing a Curry module in the intermediate form.
--- A value of this data type has the form
--- <CODE>
---  (CProg modname imports typedecls functions opdecls)
--- </CODE>
--- where modname: name of this module,
---       imports: list of modules names that are imported,
---       typedecls, opdecls, functions: see below

data Prog = Prog { progName :: String,
                   imports,exports ::[String],
                   typeDecls :: [TypeDecl],
                   instanceDecls :: [InstanceDecl],
                   funcDecls :: [FuncDecl],
                   opDecls :: [OpDecl] } deriving (Show,Eq,Read)

emptyProg = Prog "" [] [] [] [] [] []


--- The data type for representing qualified names.
--- In AbstractCurry all names are qualified to avoid name clashes.
--- The first component is the module name and the second component the
--- unqualified name as it occurs in the source program.
type QName = (String,String)


-- Data type to specify the visibility of various entities.

data Visibility = Public    -- exported entity
                | Private   -- private entity
                  deriving (Show,Eq,Read)


--- The data type for representing type variables.
--- They are represented by (i,n) where i is a type variable index
--- which is unique inside a function and n is a name (if possible,
--- the name written in the source program).
type VarName = String

--- Data type for representing definitions of algebraic data types
--- and type synonyms.
--- <PRE>
--- A data type definition of the form
---
--- data t x1...xn = ...| c t1....tkc |...
---
--- is represented by the Curry term
---
--- (CType t v [i1,...,in] [...(CCons c kc v [t1,...,tkc])...])
---
--- where each ij is the index of the type variable xj
---
--- Note: the type variable indices are unique inside each type declaration
---       and are usually numbered from 0
---
--- Thus, a data type declaration consists of the name of the data type,
--- a list of type parameters and a list of constructor declarations.
--- </PRE>

data TypeDecl = Type { 
                         typeName :: QName,
                         typeVis  :: Visibility,
                         typeVars :: [VarName],
                         consDecls :: [ConsDecl],
                         derive ::  [String]}
              | TypeSyn 
                       { typeName :: QName,
                         typeVis  :: Visibility,
                         typeVars :: [VarName],
                         typeExpr :: TypeExpr}
                 deriving (Show,Eq,Read)

--- For a type declaration the membership to certain classes can be derived in 
--- Haskell.

data TypeClass = TypeClass { className :: QName, 
                             classArgs :: [TypeExpr]} deriving (Show,Eq,Read)

data InstanceDecl = Instance {
                      constraint :: [TypeClass],
                      instanciated :: TypeClass,
                      instanceFunc :: [FuncDecl]} deriving (Show,Eq,Read)

--- A constructor declaration consists of the name and arity of the
--- constructor and a list of the argument types of the constructor.

data ConsDecl = Cons { consName :: QName,
                       consArity :: Int, 
                       consVis :: Visibility,
                       strictArgs :: Bool,
                       consArgs :: [TypeExpr]} deriving (Show,Eq,Read)


--- Data type for type expressions.
--- A type expression is either a type variable, a function type,
--- or a type constructor application.
---
--- Note: the names of the predefined type constructors are
---       "Int", "Float", "Bool", "Char", "IO", "Success",
---       "()" (unit type), "(,...,)" (tuple types), "[]" (list type)

data TypeExpr =
    TVar VarName               -- type variable
  | FuncType TypeExpr TypeExpr  -- function type t1->t2
  | TCons QName [TypeExpr]       -- type constructor application
                                   -- (CTCons (module,name) arguments)
  | TConstr [TypeClass] TypeExpr
                   deriving (Show,Eq,Read)


--- Data type for operator declarations.
--- An operator declaration "fix p n" in Curry corresponds to the
--- AbstractCurry term (COp n fix p).

data OpDecl = Op QName Fixity Int deriving (Show,Eq,Read)

data Fixity = InfixOp   -- non-associative infix operator
            | InfixlOp  -- left-associative infix operator
            | InfixrOp  -- right-associative infix operator
                   deriving (Show,Eq,Read)


--- Data types for representing object variables.
--- Object variables occurring in expressions are represented by (Var i)
--- where i is a variable index.

--- Data type for representing function declarations.
--- <PRE>
--- A function declaration in FlatCurry is a term of the form
---
---  (CFunc name arity visibility type (CRules eval [CRule rule1,...,rulek]))
---
--- and represents the function "name" with definition
---
---   name :: type
---   rule1
---   ...
---   rulek
---
--- Note: the variable indices are unique inside each rule
---
--- External functions are represented as (CFunc name arity type (CExternal s))
--- where s is the external name associated to this function.
---
--- Thus, a function declaration consists of the name, arity, type, and
--- a list of rules.
--- </PRE>

data FuncDecl = Func { funcName :: QName,
                       funcVis :: Visibility,
                       funcType :: Maybe TypeExpr,
                       funcBody ::  Maybe [Rule]} deriving (Show,Eq,Read)


--- A rule is either a list of formal parameters together with an expression
--- (i.e., a rule in flat form), a list of general program rules with
--- an evaluation annotation, or it is externally defined

--- The most general form of a rule. It consists of a list of patterns
--- (left-hand side), a list of guards ("success" if not present in the
--- source text) with their corresponding right-hand sides, and
--- a list of local declarations.
data Rule = Rule { patterns :: [Pattern],
                   rhs :: Rhs,
                   locDecls :: [LocalDecl]}
                   deriving (Show,Eq,Read)

data Rhs = SimpleExpr Expr | GuardedExpr [(Expr,Expr)] deriving (Show,Eq,Read)


--- Data type for representing local (let/where) declarations
data LocalDecl =
     LocalFunc FuncDecl                 -- local function declaration
   | LocalPat  Pattern Expr [LocalDecl] -- local pattern declaration
                   deriving (Show,Eq,Read)

--- Data type for representing Curry expressions.

data Expr =
   Var      VarName              -- variable (unique index / name)
 | Lit      Literal               -- literal (Integer/Float/Char constant)
 | Symbol   QName                  -- a defined symbol with module and name
 | Apply    Expr Expr            -- application (e1 e2)
 | Lambda   [Pattern] Expr       -- lambda abstraction
 | LetDecl  [LocalDecl] Expr     -- local let declarations
 | DoExpr   [Statement]           -- do expression
 | ListComp Expr [Statement]     -- list comprehension
 | Case     Expr [BranchExpr]    -- case expression
 | String   String 
 deriving (Show,Eq,Read)

--- Data type for representing statements in do expressions and
--- list comprehensions.

data Statement = SExpr Expr         -- an expression (I/O action or boolean)
               | SPat Pattern Expr -- a pattern definition
               | SLet [LocalDecl]   -- a local let declaration
               deriving (Show,Eq,Read)

--- Data type for representing pattern expressions.

data Pattern =
   PVar VarName         -- pattern variable (unique index / name)
 | PLit Literal          -- literal (Integer/Float/Char constant)
 | PComb QName [Pattern] -- application (m.c e1 ... en) of n-ary
                           -- constructor m.c (CPComb (m,c) [e1,...,en])
 | AsPat VarName Pattern
                   deriving (Show,Eq,Read)

--- Data type for representing branches in case expressions.

data BranchExpr = Branch Pattern Expr
                   deriving (Show,Eq,Read)

--- Data type for representing literals occurring in an expression.
--- It is either an integer, a float, or a character constant.

data Literal = Intc   Integer
             | HasIntc Integer
             | Floatc Double
             | Charc  Char
                   deriving (Show,Eq,Read)

