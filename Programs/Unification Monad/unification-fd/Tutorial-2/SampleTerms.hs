-- <http://phaneron.rickmurphy.org/2012/08/sample-terms-for-practical-type-inference-for-arbitrary-rank-types/>

-- Section 1 - Implicitly and Explicitly Typed Terms and Types

-- Example 1 - Literal
-- 0 -- 0 :: (forall. Int)

-- Example 2 - Implicitly Typed Term
-- \x. x -- (\x. x) :: (forall a. a -> a) - I combinator

-- Example 3 - Type Constant (Rho Type?) in Typed Abstraction
-- \(x::a). x -- (\(x::a). x) :: (forall. a -> a) -- Confirm the a denotes a type constant, or rho type?

-- Example 4 - Another Type Constant in Typed Abstraction
-- \(x::a). 0 -- (\(x::a). 0) :: (forall. a -> Int)

-- Example 5 - Yet Another
-- \(x::a). \(y::b). y -- (\(x::a). \(y::b). y) :: (forall. a -> b -> b)

-- Example 6 - And Another
-- \(x::Bool). \(y::Bool). y -- (\(x::Bool). \(y::Bool). y) :: (forall. Bool -> Bool -> Bool)

-- Example 7 - Equivalence of Typed Abstraction and Typed Annotation
-- \(x::Int). x -- (\(x::Int). x) :: (forall. Int -> Int) -- typed abstraction
-- \x. x :: Int -- (\x. x :: Int) :: (forall. Int -> Int) -- typed annotation

-- Example 8 - Typed Abstraction Annotated with (Rho Type ?)
-- \(x::(forall. a -> a)). x -- (\(x::(forall. a -> a)). x) :: (forall. (forall. a -> a) -> a -> a) -- Although the universal quantifiers seem to be nested, there's still no type variable to range over the types, so this is still rank 0.

-- Section 2

-- Examples 1 and 3 - 8 are all rank 0 types. Rank 0 types are monotypes. Note that in each rank 0 type the universal quantifier is not followed by a type variable before the dot (forall.). Examples 9 - 10 are rank 1 types. Rank 1 terms are polytypes. Rank 1 types have one or more type variables left of the dot (forall a b.). Examples 11 - x are rank 2 types. Rank 2 terms annotate

-- Example 9 - Type Variable Rank 1
-- \x. \y. x -- (\x. \y. x) :: (forall a b. b -> a -> b) -- K combinator

-- Example 10 - Another Rank 1
-- \x. \y. \z. x z (y z) -- (\x. \y. \z. x z (y z)) :: (forall a b c. (b -> a -> c) -> (b -> a) -> b -> c) -- S combinator

-- Example 11 - Universal Quantifier Annotation on Typed Abstraction, Rank 2
-- \(x::(forall a. a)). 0 -- (\(x::(forall a. a)). 0) :: (forall. (forall a. a) -> Int) -- note there's no quantifier to range over the monotype Int (Is what Andreas said correct ?). The explicit forall annotation on the bound and binding variable x causes the program to infer a Rank 2 polytype as indicated by the "-> Int" following the (forall a. a), while noting the absence of a type variable following the outer forall printed by the program, correct?

-- Example 12 - Another Universal Quantifier Annotation on Typed Abstraction, Rank 2
-- \(x::(forall a. a -> a)). x -- (\(x::(forall a. a -> a)). x) :: (forall b. (forall a. a -> a) -> b -> b) -- Also Rank 2, the universal quantifier on type variable b ranges over the type variable a as well as the polytype bs.

-- Example 13 - Yet Another Universal Quantifier Annotation on Typed Abstraction, Rank 2
-- (\(f::(forall a. a -> a)). \(x::(forall b. b)). f (f x)) :: (forall c. (forall a. a -> a) -> (forall b. b) -> c) -- This is still rank 2. Rank 3 requires another level of nesting.

-- Section 3 - Alpha, Beta and Eta reductions. Interesting examples of abstraction, application, lets and subsumption.

-- Example 14 - Alpha reduction: (\X -> E) => (\Y -> E) [X := Y] , if Y not free in E. There are no free variables. Bound variables are renamed.
-- \x. \x. x -- (\x. \x. x) :: (forall a b. a -> b -> b)

-- Example 15 - Omega term not allowed
-- (\x. x x) (\x. x x ) -- Occurs check for '$1' in: $1 -> $2

-- Example 16 - Beta Reduction: (\V -> E) F => E [V := F] The application of the rand F to the rator (\V -> E) reduces to E where V is replaced by F.
-- (\x. x) (\x. x) -- ((\x. x) (\x. x)) :: (forall a. a -> a) -- degenerate case

-- Example 17 - More Examples of Explicit and implicitly Typed Beta Reduction
-- (\x. x) (\x. \y. y) -- ((\x. x) (\x. \y. y)) :: (forall a b. a -> b -> b) -- reduces to rand
-- (\(x::(forall a. a -> a)). x) (\x. x) -- ((\(x::(forall a. a -> a)). x) (\x. x)) :: (forall a. a -> a) -- Rank 2
-- ((\x. x) :: (forall a. a -> a)) (\(x::Int). x) -- (((\x. x) :: (forall a. a -> a)) (\(x::Int). x)) :: (forall. Int -> Int)
-- ((\(x::(forall a. a -> a)). x) (\x. x) -- (((\(x::(forall a. a -> a)). x) (\x. x)) :: (forall a. a -> a)) :: (forall a. a -> a)

-- Example 18 - Eta Reduction: (\X -> F X) => F, if X not free in F
-- \x. (\y. y) x -- (\x. (\y. y) x) :: (forall a. a -> a) -- degenerate case

-- Example 19 - More Examples of Eta Reduction with Explicit Type Annotations
-- \(x::forall a. a -> a). (\(y::forall b. b -> b). y) x -- (\(x::(forall a. a -> a)). (\(y::(forall b. b -> b)). y) x) :: (forall b. (forall a. a -> a) -> b -> b) -- Still Rank 2
-- \(x::Bool). (\y. y) 0 -- (\(x::Bool). (\y. y) 0) :: (forall. Bool -> Int)
-- \x. (\(y::Bool). y) 0 -- (\x. (\(y::Bool). y) 0) -- Cannot unify types: Int Bool
-- \(x::(forall a. a -> a)). (\y. y) x -- (\(x::(forall a. a -> a)). (\y. y) x) :: (forall b. (forall a. a -> a) -> b -> b)

-- Example 20 - Implicitly Typed Let with Equivalent Application: let V = F in E <=> (\V -> E') F
-- let x = 0 in x -- (let { x = 0 } in x) :: (forall. Int) -- (\x. x) 0 -- Rank 0

-- Example 21 - Another Implicitly Typed Let with Equivalent App
-- let x = \y. y in x -- (let { x = \y. y } in x) :: (forall a. a -> a) -- (\x. x) (\y. y) -- Rank 1

-- Example 22 - Explicitly Typed Let with Equivalent App
-- let x = 0 in x :: Int -- (let { x = 0 } in x :: Int) :: (forall. Int) -- (\(x::Int). x) 0 -- Rank 0

-- Example 23 - Explicitly Typed Let with Equivalent App
-- let x = \(y::forall a. a -> a) . y in x -- (let { x = \(y::(forall a. a -> a)). y } in x) :: (forall b. (forall a. a -> a) -> b -> b) -- (\(x::(forall a. a -> a)). x) - Rank 2

-- Example 24 - Explicitly Typed Let, Not Equivalent to Application
-- let x = \(y::Int). y in \(x::(forall a. a -> a)). x -- (let { x = \(y::Int). y } in \(x::(forall a. a -> a)). x) :: (forall b. (forall a. a -> a) -> b -> b) -- Rank 2
-- (\(x::(forall a. a -> a)). x) (\(y::Int). y) -- Cannot unify types: a 1 Int

-- let x = \(x::forall a. a -> a). x in \(y::Int). y -- (let { x = \(x::(forall a. a -> a)). x } in \(y::Int). y) :: (forall. Int -> Int) -- (\(y::Int). y) (\(x::(forall a. a -> a)). x) -- Cannot unify types: Int $0 -> $1

-- Example 25 - Explicitly Typed Let, Not Equivalent to Application

-- let x = \(y::forall a. a -> a). y in \x. x -- (let { x = \(y::(forall a. a -> a)). y } in \x. x) :: (forall a. a -> a) -- ((\(x::(forall a. a -> a)). x) (\y. y)) :: (forall a. a -> a) -- Rank 1

-- let x = \y. y in \(x::forall a. a -> a). x -- (let { x = \y. y } in \(x::(forall a. a -> a)). x)  :: (forall b. (forall a. a -> a) -> b -> b) -- Rank 2 -- (\y. y) (\(x::forall a. a -> a). x) -- Subsumption check failed: $1 is not as polymorphic as (forall a. a -> a)

-- Restatement of Subsumption
-- the type of rand must be at least as (more) polymorphic as the rator, known as subsumption
-- contravariantly, s1 in the rator must be at least as polymorphic as s2 in the rand
-- if s1 is more polymorphic than s2, then t1 = (s1 -> Int) is less polymorphic than t2 = (s2 -> Int)

-- Example 26 - Explicitly Typed Rand Subsumes Rator
-- t1 = (\x. 0) :: (forall a. a -> Int) -- s1 = a
-- t2 = (\(x::Int). 0) :: (forall. Int -> Int) -- s2 = Int
-- s1 is more polymorphic than s2, therefore t1 is less polymorphic than, or subsumed by t2 and the following application succeeds
-- (\x. 0) (\(x::Int).x) -- ((((\x. 0) :: (forall a. a -> Int)) ((\(x::Int). 0) :: (forall. Int -> Int))) :: (forall. Int)) - Rank 0
-- (\(x::Int). 0)(\x. 0) -- Switch terms and Cannot unify types Int $4 -> $5

-- Example 27 - Explicitly Typed Rand Subsumes Rator, Rank Difference
-- (\(x::(forall a. a -> a)). x) (\x. x) -- (((\(x::(forall a. a -> a)). x) (\x. x)) :: (forall a. a -> a)) :: (forall a. a -> a) -- Rand at least as polymorphic as rator (s1 in rator is rank1, though rator is rank 2 and rand is rank 1)
-- (\x. x) (\(x::(forall a. a -> a)). x) -- Although rand is rank 2, Subsumption check failed: $1 is not as polymorphic as (forall a. a -> a)

-- Section 4 - Type Unification Errors

-- Example 30 - Typed Application, Explicitly Divergent Types
-- (\(x::Bool). x) (\x. 0) -- (\(x::Bool). x) ((\x. 0) :: (forall a. a -> Int)) -- Cannot unify types: Bool $2 -> $3
-- (\x. 0) (\(x::Bool). x) -- ((\(x::(forall a. a -> a)). x) (\x. x) (\x. 0) (\(x::Bool). x)) :: (forall. Int) -- Rank 0 and works, but the following does not, explain?
-- (\(x::Int). 0)(\(x::Bool). x) -- Cannot unify types: Int $0 -> $1

-- Example 31 - Typed Application, Implicitly Divergent Types
-- (\(x::Bool). 0)(\x. x) -- Cannot unify types: Bool $0 -> $1
-- (\x. x)(\(x::Bool). 0) -- ((\(x::(forall a. a -> a)). x) (\x. x) (\x. x) (\(x::Bool). 0)) :: (forall. Bool -> Int)

-- Example 31 - Typed Application, Implicitly Divergent Types
-- (\(x::a). x) (\x. 0) -- Panic! Unexpected types in unification: a $0 -> $1
-- (\x. 0)(\(x::a). x) -- Panic! Unexpected types in unification: $1 a

-- Example 32 - Typed Application, Explicitly Divergent Types
-- (\(x::Int). x)(\(x::(forall a. a -> a)). x) -- Cannot unify types: Int $0 -> $1
-- (\(x::(forall a. a -> a)). (\(y::Int). y) x) -- Cannot unify types: Int $1 -> $2 ***

-- Example 33 - Typed Application, Type Constant
-- (\(x::a). (\(y::a). y) x) -- Panic! Unexpected types in unification: a a
-- (\x. (\(y::a). y) x) -- Panic! Unexpected types in unification: $0 a
-- (\(x::a). (\y. y) x) -- Panic! Unexpected types in unification: a $0

-- Section 5 - List terms and the explicitly quantified principle types derived from the terms. Using application and beta reduction show the principle type of terms. One can also use ghci to evaluate these terms.

-- Example 34 - Deriving Various Implicit Terms and Their Principle Types

-- deriving various terms with Int
-- (\x. 0) :: (forall a. a -> Int) -- equivalently in ghci :t  (\x -> 0) :: Num a => t -> a
-- (\x. x 0) :: (forall a. (Int -> a) -> a)
-- (\x. x) :: (forall a. a -> a)

-- (\x. \y. x) :: (forall a b. b -> a -> b)
-- (\x. \y. y) :: (forall a b. a -> b -> b)
-- (\x. \y. x y) :: (forall a b. (a -> b) -> a -> b)
-- (\x. \y. y x) :: (forall a b. a -> (a -> b) -> b)
-- (\x. \y. x (y x)) :: (forall a b. (a -> b) -> ((a -> b) -> a) -> b)
-- (\x. \y. y (x y)) :: (forall a b. ((a -> b) -> a) -> (a -> b) -> b)

-- (\x. \y. x 0) :: (forall a b. (Int -> b) -> a -> b)
-- (\x. \y. y 0) :: (forall a b. a -> (Int -> b) -> b)
-- (\x. \y. x y 0) :: (forall a b. (a -> Int -> b) -> a -> b)
-- (\x. \y. y x 0) :: (forall a b. a -> (a -> Int -> b) -> b)
-- (\x. \y. y (x 0)) :: (forall a b. (Int -> a) -> (a -> b) -> b))
-- (\x. \y. x (y 0)) :: (forall a b. (a -> b) -> (Int -> a) -> b)

-- \x. \y. x x 0 -- Occurs check for '$2' in: $2 -> $3
-- \x. \y. y y x 0 -- Occurs check for '$2' in: $2 -> $3

-- (\x. \y. y (y 0)) :: (forall a. a -> (Int -> Int) -> Int)
-- (\x. \y. x (x 0)) :: (forall a. (Int -> Int) -> a -> Int)

-- (\x. \y. y (x (x 0))) :: (forall a. (Int -> Int) -> (Int -> a) -> a)
-- (\x. \y. x (y (y 0))) :: (forall a. (Int -> a) -> (Int -> Int) -> a)

-- note type variable dropped
-- (\x. \y. y (x (y (y 0)))) :: (forall. (Int -> Int) -> (Int -> Int) -> Int)
-- (\x. \y. x (y (x (x 0)))) :: (forall. (Int -> Int) -> (Int -> Int) -> Int)

-- (\x. \y. \z. 0) :: (forall a b c. a -> b -> c -> Int)

-- application
-- ((\x. \y. x) (\x. x)) :: (forall a b. a -> b -> b)
-- ((\x. \y. y) (\x. x)) :: (forall a. a -> a)

-- ((\x. \y. y) (\x. \y. x)) :: (forall a. a -> a)

-- ((\x. \y. x) (\x. \y. x)) :: (forall a b c. a -> c -> b -> c)
-- ((\x. \y. x) (\x. \y. y)) :: (forall a b c. a -> b -> c -> c)

-- ((\x. \y. y x) (\x. \y. y x)) :: (forall a b c. ((a -> (a -> b) -> b) -> c) -> c)

-- Beta Reduction: (\V -> E) F => E [V := F] The application of the rand F to the rator (\V -> E) reduces to E where V is replaced by F.
-- a few sample terms

-- #1
-- (\x. \y. x) (\x. x) => substitute
-- (\x. \y. (\x. x))   => reduce
-- \y. \x. x           => alpha rename
-- (\x. \y. y) :: (forall a b. a -> b -> b)

-- #2
-- ((\x. x) (\x. \y. y x)) => substitute
-- (\x. (\x. \y. y x))     => reduce
-- (\x. \y. y x) :: (forall a b. a -> (a -> b) -> b)

-- #3
-- (\x. \y. x y) (\x. x)      => substitute
-- \x. \y. ((\x. x) (\y. y))  => reduce
-- (\x. x)  :: (forall a. a -> a)

-- #4
-- (\x. \y. x y) (\x. \y. y x)           => substitute
-- \x. \y. ((\x. \y. y x) (\x. \y. y x)) => reduce
-- (\x. \y. y x) (\x. \y. y x)           => ? not normal form?
-- (\x. \y. y x) :: (forall a b. a -> (a -> b) -> b)

-- Section 6 - Adding Explicit Quantifiers Results in a Change of Principle Type

-- Example 35 - Type Variables with Arguments Reversed Plus New Quantifier (C)

-- (\x. \y. x) :: (forall a b. b -> a -> b)
-- (\(x::(forall a. a)). \(y::(forall b. b)). x) :: (forall c. (forall a. a) -> (forall b. b) -> c)

-- Example 36 - Function Argument Missing

-- (\x. \y. y x) :: (forall a b. a -> (a -> b) -> b)
-- (\(x::(forall a. a -> a)). \(y::(forall b. b -> b)). y x) :: (forall c. (forall a. a -> a) -> (forall b. b -> b) -> c -> c)
-- </pre>
-- <p>Haskell Terms and Types</p>
-- <pre>
{-# LANGUAGE RankNTypes, DatatypeContexts, ExistentialQuantification #-}
-- http://en.wikibooks.org/wiki/Haskell/Existentially_quantified_types
-- A Direct Algorithm for Type Inference in the Rank 2 Fragment of the Second-Order Lambda-Calculus
-- Practical Type Inference For Arbitrary Rank Types
-- http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.46.4938&#038;rep=rep1&#038;type=pdf
-- http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.95.2624&#038;rep=rep1&#038;type=pdf
-- http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.22.2695&#038;rep=rep1&#038;type=pdf

-- can ghci evaluate explicitly typed terms
-- terms are admissible whose explicitly quantified types are admissible
-- explicit quantifiers and principle types

module Main where

-- Section 1, basics

-- Example 1 - Literal

e1 :: Int -- implicitly quantified, rank 0
e1 = 0t

e1' :: forall. Int -- explicitly quantified, rank 0
e1' = let
      g :: Int -> Int -- implicitly quantified, rank 0
      g x = x
      in g 0

-- Example 2 - Explicitly Quantified, Monomorphic Type

e2 :: forall. Int -> Int -- explicitly quantified, rank 0
e2 x = x

-- e2' :: forall. a -> a -- Main.hs:45:22: Not in scope: type variable `a', ? no type constants
-- e2' x = x

-- Example 3 - Explicitly and Implicitly Quantified Polytype

e3 :: a -> a -- implicitly quantified, rank 1
e3 x = x

e3' :: forall a. a -> a -- explicitly quantified, rank 1
e3' x = x -- e3 0 0 it :: Integer

-- Example 4 - Explicitly Quantified Polytype, rank 2

e4 :: (forall a. a -> a) -> Int -- explicitly quantified, rank 2
e4 x =  x 0

-- type constructors, implicit quantifier, rank 2

-- | No explicit universal quantifier
data T a = TC0 a | TC1 Int | TC2 (a -> a) | TC3 (Int -> Int)

-- :kind! T -- T :: * -> *
-- :t TC0 :: a -> T a
-- :t TC1 -- TC1 :: Int -> T a
-- :t TC2 -- TC2 :: (a -> a) -> T a
-- :t TC3 -- TC3 :: (Int -> Int) -> T a

-- the expressions below are not meant to be printed
-- evaluate the expressions and get the message to implement show
-- check the types of each using :t on the type constructor
-- check the kind using :kind! on the type

-- Example 6 - Monomorphic Type Variable Allows Any Monomorphic Type
e6 :: T Int -- notice the type is NOT (T a)
e6 = TC0 0

e6' :: T Bool -- same as above
e6' = TC0 True

e6'' :: T (a -> a)
e6'' = TC0 ((\x y -> x (x y)) (\y -> y))

-- Example 7 - Monommorphic Type Restriction (Int)
e7 :: T a -- notice the type IS (T a)
e7 = TC1 0

-- e7' = TC1 True -- Couldn't match expected type `Int' with actual type `Bool'

-- Example 8 - Monomorphic Function on Type Variables (a -> a), Function Evaluation
iid :: Int -> Int
iid x = x

bid :: Bool -> Bool
bid x = x

e8 :: T Int -- although the type is TC2 :: (a -> a) -> T a, the type system infers T Int
e8 = TC2 iid

e8' :: T Bool -- same
e8' = TC2 bid

-- e8'' = TC2 0 -- No instance for (Num (a0 -> a0))

-- Example 9 - Monomorphic Function on Type Restriction (Int -> Int), Function Evaluation
e9 :: T a -- although the type constructor is TC3 :: (Int -> Int) -> T a, the type system infers T a
e9 = TC3 iid

e9' :: T a -- more general types are allowed (id)
e9' = TC3 id

-- e9'' = TC3 bid -- Couldn't match expected type `Int' with actual type `Bool' Expected type: Int -> Int Actual type: Bool -> Bool

-- e9''' = TC3 ((\x y -> x (x y))) -- The lambda expression `\ x y -> x (x y)' has two arguments, but its type `Int -> Int' has only one

-- Example 10 - Existential (Union) Type

-- It would appear that U has one type constructor with an explicitly quantified type: UC1, however the universal quantifier does not appear in either of these existentially quantified types. Because there is no universal quantifier these would be considered rank 0 types and the type variable in UC1 would be inhabited by monomorphic types.

data U = forall a. UC1 a | forall. UC2 Int

-- :kind! U :: *
-- :t UC1 :: a -> U1 -- note no universal quantifier
-- t: UC2 :: Int -> U1 -- note no universal quantifier, no type variable

e10 :: [U] -- existential type
e10 = (UC1 0) : [UC1 True]

-- e10' = UC2 True -- Couldn't match expected type `Int' with actual type `Bool'

-- Example 11 Explicitly Quantified (Intersection, Identity and Multiparam) Types, Rank 2

-- These explicitly quantified type constructors are all rank 2.

data U1 = UC1' (forall a. a) | UC2' (forall a. a -> a) | UC3' (forall a b. a -> b -> b)

-- :kind! U1 :: *
-- :t UC1' :: (forall a. a) -> U1
-- :t UC2' :: (forall a. a -> a) -> U1
-- :t UC3' :: (forall a b. a -> b -> b) -> U1

e11 :: U1
e11 = UC1' undefined -- only undefined is admissable under this type because it is the intersection of all types

e11' :: U1 -- only matching polytypic type variables are accepted, as below monomorphic types are rejected by the type system
e11' = UC2' id

e11'' :: U1 -- and terms that reduce to the principle type are admissible
e11'' = UC2' ((\x y -> x y) (\x -> x))

-- e11'' application and beta reduction
-- (\x. \y. x y) (\x. x)      => substitute
-- \x. \y. ((\x. x) (\y. y))  => reduce
-- (\x. x)  :: (forall a. a -> a)

-- e11''' = UC2' bid -- Couldn't match type `a' with `Bool' `a' is a rigid type variable bound by a type expected by the context: a -> a

-- e11'''' = UC2' 0 -- No instance for (Num (a -> a)) arising from the literal `0'

e11''''' :: U1 -- matching polyptypic types are accepted
e11''''' = UC3' (\x y -> y) -- :: (forall a b. a -> b -> b)

-- e11'''''' = UC3' (\x y -> x) -- nonmatching are rejected - Couldn't match type `a' with `b' `a' is a rigid type variable bound by a type expected by the context: a -> b -> b at Main.hs:159:12   `b' is a rigid type variable bound by a type expected by the context: a -> b -> b at Main.hs:159:12

-- e11'''''' = UC3' ((\x -> x) (\x y -> y x)) -- Couldn't match type `b' with `a -> b' `b' is a rigid type variable bound by a type expected by the context: a -> b -> b at Main.hs:165:8

-- e11'''''' application and beta reduction explains this mismatch
-- ((\x. x) (\x. \y. y x)) => substitute
-- (\x. (\x. \y. y x))     => reduce
-- (\x. \y. y x) :: (forall a b. a -> (a -> b) -> b)

-- Example 12 - Covariant (?) and Contravariant Explicitly Quantified Type Constructors

-- UC4 is a rank 2 type constructor noting that the right parenthese around forall b. b -> b are removed when checking its type. UC4' is a rank 3 type constructor, noting the type checker perserves the parentheses around (forall b. b -> b).

data U4 = UC4 (forall a. a -> (forall b. b -> b)) | UC4' (forall a. (forall b. b -> b) -> a)

-- :kind! U4 :: *
-- :t UC4 :: UC4 (forall a. a -> forall b. b -> b) -> U4
-- :t UC4' :: (forall a. (forall b. b -> b) -> a) -> U4

e12 :: U4
e12 = UC4 (\x y -> y) -- matches parameters

e12' :: U4
e12' = UC4 ((\x y -> x) (\x -> x)) -- reduction

-- beta reduction on term in type constructor
-- (\x. \y. x) (\x. x) => substitute
-- (\x. \y. (\x. x))   => reduce
-- \y. \x. x           => alpha rename
-- (\x. \y. y) :: (forall a b. a -> b -> b)

-- what are the implicit terms that are admissible by the type
-- what is the method for determining
-- given a term what is are the principle type(s) among choices of the explicit quantifiers
-- what is a term of rank n that is admissible by the principle type
e12'' :: U4
e12'' = UC4' undefined -- see section 6 in Tc monad

-- Example 13

-- The first type constructor is rank 2, the second, third and fourth type constructors are rank 3. The implicit terms are admissible when their explicitly quantified principle type is admissible. See section 5 and 6 in Tc monad.

data U5 = UC5 (forall a. a -> Int) | UC5' ((forall a. a -> a) -> Int) | UC5'' ((forall a. a -> a) -> (forall b. b -> b) -> Int) | UC5''' (forall b. (forall a. a -> a) -> b -> b)

-- :kind! U5 :: *
-- :t UC5 :: (forall a. a -> Int) -> U5
-- :t UC5' :: ((forall a. a -> a) -> Int) -> U5
-- :t UC5'' :: ((forall a. a -> a) -> (forall b. b -> b) -> Int) -> U5
-- :t UC5''' :: (forall b. (forall a. a -> a) -> b -> b) -> U5

-- terms are admissible whose explicitly quantified types are admissible
-- (\x -> 0) :: Num a => t -> a
-- (\x. 0) :: (forall a. a -> Int)
e13 :: U5
e13 = UC5 (\x -> 0)

-- (\x -> x 0) :: Num a => (a -> t) -> t
-- explicitly quantify x with forall a. a -> a
-- (\(x::(forall a. a -> a)). x 0) :: (forall. (forall a. a -> a) -> Int)
e13' :: U5
e13' = UC5' (\x -> x 0)

-- (\x. \y. y (x 0)) :: (forall a b. (Int -> a) -> (a -> b) -> b)
-- explicitly quantify x with forall a. a -> a and y with forall b. b -> b
-- (\(x::(forall a. a -> a)). \(y::(forall b. b -> b)). y (x 0)) :: (forall. (forall a. a -> a) -> (forall b. b -> b) -> Int)
e13'' :: U5
e13'' = UC5'' (\x y -> y (x 0))

-- By inspection the implicitly typed term (\x. \y. y) infers the principle type (forall a b. a -> b -> b), so that term is admissible in UC5''' as would it be with (forall b. (forall a. a) -> b -> b) where the universally quantified type variable a has been reduced from a function.

e13''' :: U5
e13''' = UC5''' (\x y -> y) -- the term (\x -> x) is also admissible here ...

-- Example 14 - Contravariant Type Constructors, Rank 3 and Rank 5

data U6 = UC6 (forall b. (forall a. a -> b) -> b) | UC6' ((forall c. (forall b. (forall a. a -> a) -> b) -> c) -> Int)
-- :kind! U6 :: *
-- :t UC6 :: (forall b. (forall a. a -> b) -> b) -> U6
-- :t UC6' :: ((forall c. (forall b. (forall a. a -> a) -> b) -> c) -> Int) -> U6

-- inspection should be replaced by a system in which we infer the admissible principle type from the implicitly typed terms. See section 6 in the accompanying Tc Monad.
