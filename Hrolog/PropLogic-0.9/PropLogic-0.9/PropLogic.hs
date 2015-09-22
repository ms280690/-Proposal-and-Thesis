{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

{- |
  A powerful system for /propositional logic/.
  Defines an abstract concept of a /propositional algebra/ and provides both default and fast instances.
  Emphasizes the use of /(canonic) normalizations/ in general and so-called /Prime Normal Forms/ in particular.

  <http://www.bucephalus.org/PropLogic/>

  is the homepage with additional information for a variety of users, including short and thorough introductions to the
  use of "PropLogic" and the mathematical background of the whole design.
-}

module PropLogic (

  -- * Start
  -- | The whole package is very modular in itself, according to the different perspectives we take to approach the subject
  -- (as explained below). It is possible to be more specific in an application and to import only certain parts. But the
  -- easiest and recommended way is the import of just this "PropLogic" module, which is just the summary of its main parts.
  --
  -- This package uses certain extensions to the Haskell 98 standard.
  -- In particular, the /propositional algebra/ concept is implemented as a two-parameter type class 'PropAlg'.
  -- The package is developed with GHC (the Glasgow Haskell Compiler) and that works fine.
  -- Hugs works fine as well, but it requires to be started with @hugs -98@.

  -- * The composition and modules

  -- ** Introduction
  -- | /Propositional logic/ (also called /sentential logic/) is a very standard and relatively uncomplicated system in itself.
  --
  -- The (binary) /boolean value algebra/ is one of the simplest structures at all and a standard in almost every programming language.
  -- (Recall, that it is in Haskell given by the type @Bool@, made of @True@ and @False@, the
  -- order @<=@ and the boolean junctors @not@, @&&@ and @||@.)
  -- A /propositional algebra/ is then essentially defined as the extension of this boolean value algebra with free variables,
  -- commonly called /atoms/.
  --
  -- But however dense an abstract characterization of a propositional algebra may be, most traditional introductions are more complex and
  -- start with a syntax, i.e. some choice of what /propositional formulas/ should cover, a more or less diverse semantics, together
  -- with some calculus for semantically sound and complete manipulation of formulas.

  -- ** The four main modules
  -- | In this Haskell version of propositional logic, one design principle is the separation of the different aspects in four
  -- main modules.
  --
  -- >
  -- >
  -- >                     PropLogicCore
  -- >                    /             \
  -- >                   /               \
  -- >                  /                 \
  -- >                 /                   \
  -- >       DefaultPropLogic            FastPropLogic
  -- >                \                    /
  -- >                 \                  /
  -- >                  \                /
  -- >                   \              /
  -- >                     PropLogicTest
  -- >

  -- ** The abstract /core/ concepts

  module PropLogicCore,

  -- | This module comprises the abstract definition of two core concepts of propositional logic:
  --
  -- * The data type @('PropForm' a)@ of /propositional formulas/, based on a given /atom/ type @a@.
  --
  -- * The two-parameter type class @('PropAlg' a p)@ of a /propositional algebra/, where @a@ is the /atom/ type and @p@ the type of
  --   /propositions/. Operations of such a structure include a decision if two propositions are 'equivalent', if a given proposition is
  --  'satisfiable', a converter 'toPropForm' and the inverse 'fromPropForm', which turns a propositional formula into a proposition.
  --
  -- "PropLogicCore" provides the interface for the toolbox most users would need in most real circumstances.
  -- The actual 'PropAlg' instances are implemented in the following two modules.

  -- ** Propositional logic the /default/ style and the descriptive introduction of (canonic) normalizations

  module DefaultPropLogic,

  -- | "DefaultPropLogic" on the other hand provides one (actually more than one) instance of 'PropAlg', along with a whole range of
  -- additional functionalities. For example, it introduces the concepts of /valuators/ and a /truth tables/.
  -- The title /default/ indicates the very straight forward, intuitive, and naive approach.
  -- For example, the implementation for the satisfiability
  -- function applies the naive test, if the truth table has at least one /true/ in the result column.
  -- In fact, in "DefaultPropLogic", the semantics of propositional algebra is thoroughly reconstructed and that makes this
  -- module suitable as an interactive tutorial for propositional logic itself and all the concepts of the abstract level.
  -- (There is an according introduction available online.)

  -- | The "DefaultPropLogic" introduces something else, namely a non-traditional idea of what the standard operations are.
  -- We don't bother with the introduction of a certain rule system or calculus and we don't reconstruct propositional logic as
  -- a proof system.
  -- Our approach is based on /(canonic) normalizations/ instead.
  -- In particular, we use special kinds of /DNF/s and /CNF/s (/disjunctive/ or /conjunctive normal forms/), namely
  -- /prime normal forms/, and all traditional problems come down to a single normalization.
  -- For example, a formula &#966; is satisfiable iff the prime DNF is not the empty disjunction,
  -- i.e. iff the call of @primeDNF &#966;@ returns another result than @(&#8744;)@.
  -- All traditional problems can be solved with one single canonization such as 'primeDNF' or 'primeCNF', respectively.

  -- ** Instances of propositional algebras with /fast/ implementations

  module FastPropLogic,

  -- | Prime Normal Forms are indeed distinguished and reasonably compact semantic canonizations of propositional formulas.
  -- However, for all cases with more than just a trivial amount of atoms involved, the default implementations are not feasible,
  -- because of the exponential cost explosing of these default methods.
  -- In "FastPropLogic" we provide a complete new implementation, based on a /fast/ system of algorithms.
  -- Main goal is the construction of fast instances of the 'PropAlg' type class.

  -- ** Additional functions

  module PropLogicTest,

  -- | The independent implementations of the default and the fast approach allows us to use one as a test for the other, because
  -- they should produce the same results. To do that systematically, "PropLogicTest" has random formula generators and test for
  -- correctness.
  -- Another goal is the investigation of the actual performance of the /fast/ methods.
  -- Finally, this module contains some useful functions which use functions from both "DefaultPropLogic" and "FastPropLogic".

  -- ** Hidden modules
  -- | "TextDisplay" implements some methods for the two-dimensional treatment of plain text output. This is a useful tool, for
  -- example for the display of truth tables in interpreter sessions. It provides the 'display' function which can be applied to
  -- all the main concepts (formulas, truth tables, normal forms), for more intuitive and graphical layout than calling 'print'.
  --
  -- "Olist" has methods for treating ordered lists as finite sets.
  --
  -- "Costack" is the implementation of a /concatenable stack/, i.e. it re-defines the standard Haskell functions
  -- @(:)@, @(null)@, @head@, @tail@, and @(++)@. The concatenation @(++)@ is not a primitive function in Haskell, and a /costack/
  -- is a data structure, where all these functions are primitive.
  -- The "Costack" module is only used in the implementations of the "FastPropLogic" functions.

  -- * Propositional algebras
  -- | As mentioned already, the two core concepts in the implementation of our version of propositional logic are
  -- /propositional formulas/, given as the data type @'PropForm' a@, and the two-parameter type class @'PropAlg' a p@
  -- for the abstract notion of a /propositional algebra/.
  -- These two notions are described in more detail in the "PropLogicCore" module.

  -- ** The different instances
  -- | The actual implementation of 'PropAlg' instances is done in two other modules and in two different ways:
  --
  --   (I) In the "DefaultPropLogic" module, we implement three different propositional algebras in a standard, intuitive and straight
  --   forward way.
  --
  --   (1) @(PropAlg a ('PropForm' a))@, is the standard propositional algebra, where the propositions are actually the propositional
  --   formulas.
  --
  --   (2) @(PropAlg a ('TruthTable' a))@ is a less common, but nevertheless very intuitive propositional algebra, which is based on
  --   /truth tables/.
  --
  --   (3) @(PropAlg 'Void' 'Bool')@ instantiates the predefined algebra @Bool@ of boolean values as a propositional algebra, the
  --   one with the empty atom type @Void@. Actually, all other instances of propositional algebras are equally powerful in the
  --   sense that they can all be based on an arbitrary type @a@ of atoms. But the propositional algebra on @Bool@ is just the
  --   trivial algebra with the empty atom type and we neglect it in the comparison of the different instances below.
  --
  --   (II) In the "FastPropLogic" module defines a small variety of propositional algebras where the propositions are certain kinds of
  --   /normal forms/.
  --
  --   (1) @(PropAlg a ('XPDNF' a))@ is the algebra, where each proposition is an /indexed prime disjunctive normal form/.
  --   One distinctive feature of these forms is that they are /canonic/ in the sense that two propositions are equivalent if and only
  --   if they are equal.
  --
  --   (2) @(PropAlg a ('XPCNF' a))@ is the /dual/ version of the previous algebra, elements are /indexed prime conjunctive normal forms/.
  --
  --   (3) @(PropAlg a ('MixForm' a))@ is a variation and mix of the previous two algebras. The normal forms here may be either
  --   /conjunctive/ or /disjunctive normal forms/, they are not necessarily /prime/ normal forms, but may only be /pairwise minimal/.
  --   So in this algebra, the propositions are no longer canonic. But the advantage is that all the junctions, i.e. the boolean
  --   operations such as negation, conjunction etc. are of polynomial complexity. And in applications, where predominanty junctions
  --   are used, this implementation of a propositional algebra is faster than the previous two.

  -- ** Feasibility and performance of the fast implementations
  -- | The default instances of 'PropAlg' (i.e. on formulas and truth tables) are not feasible for other than small problems, and
  -- that is why we implemented some fast instances as well.
  -- One example for a cost explosing is the call of 'satisfiable' applied to a formula &#966;. If &#966; has say @n@ different atoms,
  -- then there are @2^n@ different valuations of these atoms and in the worst case, we need to test for all these valuations, if
  -- they make &#966; true or false. /Not feasible/ means /exponential in the worst input cases/.
  -- Not all functions of the algebra on formulas are not feasible in this sense.
  -- But compare to the default instances, the fast instances are called /fast/, because none of their functions is exponential.
  --
  -- In fact, we cannot claim really, that this is actually true. We don't have a proof for the non-exponentiality of the fast functions.
  -- The only criterion for the feasibility of our implementations is empirical.
  -- For more detail and empirical data about the performance of the involved functions, we refer to the home page.

) where

  import TextDisplay
  import Olist
  import Costack
  import PropLogicCore
  import DefaultPropLogic
  import FastPropLogic
  import PropLogicTest

