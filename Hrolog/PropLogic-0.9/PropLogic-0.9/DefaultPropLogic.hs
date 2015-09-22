{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

{- |
  This module implements the basic operations of propositional logic in a very /default/ way, i.e. the definitions and implementations
  are very intuitive and the whole module can be seen as a reconstruction and tutorial of propositional logic itself.
  However, some of these implementations are not feasible for other than very small input, because the intuitive algorithms are
  sometimes too ineffective.

  Next to some syntactical tools, we provide a common reconstruction of the semantics with an emphasis on /truth tables/.
  As a result, we obtain two default models of a propositional algebra, namely
  @PropAlg a (PropForm a)@ the propositional algebra on propositional formulas 'PropForm' and
  @PropAlg a (TruthTable a)@ the algebra on the so-called /truth tables/ 'TruthTable' (each one on a linearly ordered atom type @a@).
  Additionally, we also instantiate the predefined boolean value algebra on 'Bool' as a trivial, because atomless propositional algebra.

  Another important concept is the /normalization/. We introduce a whole range of /normalizers/ and /canonizers/ of
  propositional formulas.
-}

module DefaultPropLogic (

  -- * Syntactic operations

  -- ** Formula Deconstructors

  juncDeg,
  -- | returns a /junctor degree/ @0,1,...,,7@, in case the formulas is created with @A,F,T,N,CJ,DJ,SJ,EJ@, respectively.
  -- For example,
  --
  -- > > juncDeg (A 'x')
  -- > 0
  -- >
  -- > > juncDeg (CJ [A 'x', F])
  -- > 4

  juncArgs,
  -- | returns the list of arguments of the outer junctor of the given formula, e.g.
  --
  -- > > juncArgs (SJ [A 'x', F, CJ [A 'y', T]])
  -- > [A 'x',F,CJ [A 'y',T]]
  -- >
  -- > > juncArgs F
  -- > []
  -- >
  -- > > juncArgs (N T)
  -- > [T]
  -- >
  -- > > juncArgs (A 'x')
  -- > [A 'x']

  juncCons,
  -- | is the junction constructor, which is defined so that for every formula @p@ holds
  --
  -- > juncCons (juncDeg p) (juncArgs p) == p
  --
  -- For example,
  --
  -- > > juncCons 5 [juncCons 0 [A 'x'], juncCons 1 [], juncCons 3 [juncCons 2 []]]
  -- > DJ [A 'x',F,N T]

  -- ** Size

  atomSize,
  -- | counts the number of atoms (including multiple occurrences), i.e. each occurrence of an @A@.
  --
  -- > atomSize ( CJ [DJ [A "x", N (A "y")], DJ [A "x", A "z"], T] )  ==  4
  --

  juncSize,
  -- | counts the number of bit values and junctions, i.e. each occurrence of @F@, @T@, @N@, @CJ@, @DJ@, @SJ@, @EJ@
  --
  -- >  juncSize ( CJ [DJ [A "x", N (A "y")], DJ [A "x", A "z"], T] )  ==  5
  --

  size,
  -- | The overall size of a formula, where
  --
  -- > (size f) = (atomSize f) + (juncSize f)
  --
  -- For example
  --
  -- >  size ( CJ [DJ [A "x", N (A "y")], DJ [A "x", A "z"], T] )  ==  9

  -- ** Linear syntactic order on formulas
  -- | .....................................CONTINUEHERE ....................................................

  -- * Semantics
  -- | A denotational semantics for propositional formulas is defined by the basic idea, that the /truth table/ of a formula
  -- defines its meaning. Formally speaking, if @A = {a1, ..., aN}@ is the atom set of a given formula &#966;, then each /valuator/,
  -- i.e. each function @&#969; :: A -> Bool@ turns &#966; into an either @True@ or @False@ value (namely @boolApply &#966; &#969;@).
  -- The summary of these @2^N@ valuator-value pairs is formalized by a function @(A -> Bool) -> Bool@, and that is what we call
  -- the /truth table/ of &#966; (namely @truthTable &#966;@).
  --
  -- For example, if &#966; is @a &#8596; b@, then the atom set is @{a,b}@. And if @tt = truthTable &#966;@, then we can
  -- @display tt@ and obtain
  --
  -- > +---+---+---+
  -- > | a | b |   |
  -- > +---+---+---+
  -- > | 0 | 0 | 1 |
  -- > +---+---+---+
  -- > | 0 | 1 | 0 |
  -- > +---+---+---+
  -- > | 1 | 0 | 0 |
  -- > +---+---+---+
  -- > | 1 | 1 | 1 |
  -- > +---+---+---+
  --
  -- Implementing this idea in Haskell is not that straight forward however, because Haskell doesn't allow type constructions like
  -- @(atoms &#966;) -> Bool@ for valuators and @((atoms &#966;) -> Bool) -> Bool@ for truth tables. So we need to make some adaptions.

  -- ** Valuators and formulas as functions
  LiteralPair,
  Valuator,
  -- | Ideally, a valuator is a finite function @{a1,...,aN} -> Bool@ and we can represent it as a list of atom-value pairs
  -- @[(a1,b1),...,(aN,bN)]@. We disambiguate and increase the efficiency of this representation, when we demand that @a1 < ... < aN@,
  -- according to a given order on the atoms. But declaring @Valuator a@ as @Olist (a, Bool)@ only makes @(a1, b1) < ... < (aN, bN)@.
  -- For example, @[('x',False),('x',True)]@ is a @Valuator Char@, but obviously not a /correct/ one, as we call it.

  correctValuator,
  -- | If the given list of literal pairs makes a correct valuator, then this argument is returned as it is. Otherwise, an error
  -- occurs. For example,
  --
  -- > > correctValuator [('x', False), ('y', True)]
  -- > [('x',False),('y',True)]
  -- >
  -- > > correctValuator [('x', False), ('x', True)]
  -- > *** Exception: correctValuator: multiple occurring atoms
  -- >
  -- > > correctValuator [('y', False), ('x', True)]
  -- > *** Exception: correctValuator: atoms are not in order

  valuate,
  -- | @valuate &#969; &#966;@ takes the formula &#966; and replaces its atoms by boolean values according to &#969;. For example,
  --
  -- > > valuate [('x', True), ('y', False)] (EJ [A 'x', N (A 'x'), A 'y', A 'z', A 'y'])
  -- > EJ [T,N T,F,A 'z',F]

  boolEval,
  -- | @boolEval &#969;@ evaluates a nullatomic formula &#969; to a boolean value according to the common semantics for the
  -- junctions @N@, @DJ@, ..., but causes an error when &#969; is not nullatomic. For example,
  --
  -- > > boolEval (DJ [T, F])
  -- > True
  -- >
  -- > > boolEval (CJ [T, F])
  -- > False
  -- >
  -- > > boolEval (EJ [T,T])
  -- > True
  -- >
  -- > > boolEval (CJ [T, SJ [F, F]])
  -- > True
  -- >
  -- > > boolEval (CJ [T, A 'x'])
  -- > -- error ...
  --
  -- (Note, that we also provide a generalization 'eval' of 'boolEval' below.)

  boolApply,
  -- | combines 'valuate' and 'boolEval', i.e.
  --
  -- > boolApply p v = boolEval (valuate v p)
  --
  -- For example,
  --
  -- > > boolApply (EJ [A 'x', A 'y']) [('x', True), ('y', True)]
  -- > True
  -- >
  -- > > boolApply (EJ [A 'x', A 'y']) [('x', True), ('y', False)]
  -- > False
  -- >
  -- > > boolApply (EJ [A 'x', A 'y']) [('x', True)]
  -- > -- error
  --
  -- (Note, that we also provide a generalization 'apply' of 'boolApply' below.)

  allValuators,
  -- | For example
  --
  -- > > allValuators ['y', 'x']
  -- > [[('x',False),('y',False)],[('x',False),('y',True)],[('x',True),('y',False)],[('x',True),('y',True)]]

  zeroValuators,
  -- | @zeroValuators &#966;@ returns all valuators @&#969;@ (on the atom set of @&#966;@) with @boolApply &#966; &#969; = False@.
  -- For example,
  --
  -- > > zeroValuators (CJ [A 'x', A 'y'])
  -- > [[('x',False),('y',False)],[('x',False),('y',True)],[('x',True),('y',False)]]

  unitValuators,
  -- | @unitValuators &#966;@ returns all valuators @&#969;@ (on the atom set of @&#966;@) with @boolApply &#966; &#969; = Truee@.
  -- For example,
  --
  -- > > unitValuators (CJ [A 'x', A 'y'])
  -- > [[('x',True),('y',True)]]

  -- ** Truth tables

  TruthTable,
  -- | A truth table is thus a triple @(atomList, optForm, tableBody)@, where @atomList@ is the ordered list of atoms, @optForm@ is
  -- the optional formula that is a representation for this table, and @tableBody@ holds the rows of the table.
  -- For example, if the table @tt :: TruthTable String@ is 'display'ed by
  --
  -- > +---+---+---+
  -- > | x | y |   |
  -- > +---+---+---+
  -- > | 0 | 0 | 1 |
  -- > +---+---+---+
  -- > | 0 | 1 | 0 |
  -- > +---+---+---+
  -- > | 1 | 0 | 0 |
  -- > +---+---+---+
  -- > | 1 | 1 | 1 |
  -- > +---+---+---+
  --
  -- then @tt@ has the formal representation as
  --
  -- > (["x","y"],Nothing,[([False,False],True),([False,True],False),([True,False],False),([True,True],True)])
  --
  -- The optional formula @Maybe (PropForm String)@ is @Nothing@ in this case, and this makes it a /plain/ table.
  -- But it is common and often convenient to write the formula that represents this table in the head row as well.
  -- Such a truth table is e.g. given by
  --
  -- > (["x","y"],Just (EJ [A "x",A "y"]),[([False,False],True),([False,True],False),([True,False],False),([True,True],True)])
  --
  -- And when we 'display' it, we obtain
  --
  -- > +---+---+-----------+
  -- > | x | y | [x <-> y] |
  -- > +---+---+-----------+
  -- > | 0 | 0 |     1     |
  -- > +---+---+-----------+
  -- > | 0 | 1 |     0     |
  -- > +---+---+-----------+
  -- > | 1 | 0 |     0     |
  -- > +---+---+-----------+
  -- > | 1 | 1 |     1     |
  -- > +---+---+-----------+

  correctTruthTable,
  -- | checks if the given argument makes indeed a proper truth table. If so, the argument is returned unaltered. Otherwise, an
  -- according error message is returned.

  truthTable,
  -- | returns the truth table of the given formula, with the formula itself included in the header. For example,
  --
  -- > > truthTable (EJ [A "abc", F])
  -- > (["abc"],Just (EJ [A "abc",F]),[([False],True),([True],False)])
  -- >
  -- > > display it
  -- > +-----+-----------------+
  -- > | abc | [abc <-> false] |
  -- > +-----+-----------------+
  -- > |  0  |        1        |
  -- > +-----+-----------------+
  -- > |  1  |        0        |
  -- > +-----+-----------------+

  plainTruthTable,
  -- | as @truthTable@, but this time without the formula included. For the same example, this is
  --
  -- > > plainTruthTable (EJ [A "abc", F])
  -- > (["abc"],Nothing,[([False],True),([True],False)])
  -- >
  -- > > display it
  -- > +-----+---+
  -- > | abc |   |
  -- > +-----+---+
  -- > |  0  | 1 |
  -- > +-----+---+
  -- > |  1  | 0 |
  -- > +-----+---+

  truthTableBy,
  -- | By default, the truth table of a formula &#966;, i.e. the right result column of the table is constructed with
  -- @boolApply &#966; &#969;@ for the valuator @&#969;@ in each of the rows. Now @truthTableBy@ is a generalization of this
  -- construction and allows other functions of type @Valuator a -> Bool@ for this process, as well.

  -- ** Multiple truth tables

  -- | It is common and efficient, for example when comparing several formulas for equivalence, to write these formulas in just
  -- one table and add a result row for each formula. For example, the two formulas @&#172;(x &#8594; y)@ and @x &#8743; &#172; y@
  -- are equivalent and we can verify that by comparing the two result columns in their tables. We call
  --
  -- > > multiTruthTable [N (SJ [A 'x', A 'y']), CJ [A 'x', N (A 'y')]]
  -- > ("xy",[N (SJ [A 'x',A 'y']),CJ [A 'x',N (A 'y')]],
  -- >       [([False,False],[False,False]),([False,True],[False,False]),([True,False],[True,True]),([True,True],[False,False])])
  -- >
  -- > > display it
  -- > +-------------------------+
  -- > | MultiTruthTable         |
  -- > |  1. -[x -> y]           |
  -- > |  2. [x * -y]            |
  -- > | +---+---+------+------+ |
  -- > | | x | y |  1.  |  2.  | |
  -- > | +---+---+------+------+ |
  -- > | | 0 | 0 |  0   |  0   | |
  -- > | +---+---+------+------+ |
  -- > | | 0 | 1 |  0   |  0   | |
  -- > | +---+---+------+------+ |
  -- > | | 1 | 0 |  1   |  1   | |
  -- > | +---+---+------+------+ |
  -- > | | 1 | 1 |  0   |  0   | |
  -- > | +---+---+------+------+ |
  -- > +-------------------------+

  MultiTruthTable,
  -- | Similar to the type definition of 'TruthTable' as a triple @(atoms, formulaList, tableBody)@.

  correctMultiTruthTable,
  -- | returns the argument unchanged, if this is a well-formed 'MultiTruthTable' (with correct number of rows and columns etc.),
  -- and returns an error, otherwise.

  multiTruthTable,
  -- | returns the common 'MultiTruthTable' of a formula list, where the atom list is the union of the atoms of all the formulas.

  -- ** Mutual converters between syntax and semantics

  -- | With 'truthTable' we turned a 'PropForm' into a 'TruthTable'. But we can also turn a 'TruthTable' into a 'PropForm'.
  -- However, this process is not unique anymore, because there are (infinitely) many formulas to represent each truth table.
  -- Nevertheless, there are two standard ways to do so:
  --
  --  (1) Each row with a result 1 (i.e. each /unit valuator/) is turned into a /Normal Literal Conjunction/ or 'NLC' and
  --      the disjunction of them is a /Disjunctive Normal Form/ or 'DNF', which represents the table.
  --
  --  (2) Each row with a result 0 (i.e. each /zero valuator/) is turned into a /Normal Literal Disjunction/ or 'NLD' and
  --      the conjunction of them is a /Conjunctive Normal Form/ or 'CNF', which represents the table, too.
  --
  -- For example, if the 'TruthTable' is say
  --
  -- > > let tt = truthTable (EJ [A 'x', A 'y'])
  -- > > > display tt
  -- > +---+---+-----------+
  -- > | x | y | [x <-> y] |
  -- > +---+---+-----------+
  -- > | 0 | 0 |     1     |
  -- > +---+---+-----------+
  -- > | 0 | 1 |     0     |
  -- > +---+---+-----------+
  -- > | 1 | 0 |     0     |
  -- > +---+---+-----------+
  -- > | 1 | 1 |     1     |
  -- > +---+---+-----------+
  --
  -- then
  --
  -- > > display (truthTableToDNF tt)
  -- > [[-x * -y] + [x * y]]
  -- >
  -- > > display (truthTableToCNF tt)
  -- > [[x + -y] * [-x + y]]
  --
  -- The following functions provides the whole range of transformations for valuators, valuator lists, truth tables, and the
  -- according normal forms.

  valuatorToNLC,
  -- | For example,
  --
  -- > > valuatorToNLC [('x', True), ('y', False), ('z', True)]
  -- > CJ [A 'x',N (A 'y'),A 'z']

  valuatorToNLD,
  -- | For example,
  --
  -- > > valuatorToNLD [('x', True), ('y', False), ('z', True)]
  -- > DJ [N (A 'x'),A 'y',N (A 'z')]
  --
  -- Note, that the literal values appear inverted, e.g. @('x', True)@ becomes @N (A 'x')@.

  valuatorListToCNF,
  -- | turns each valuator of the given list into its 'NLD' and returns their overall conjunction.

  valuatorListToDNF,
  -- | turns each valuator of the given list into its 'NLC' and returns their overall disjunction.

  nlcToValuator,
  -- | Inverse operation of 'valuatorToNLC'. Undefined, if the argument is not a 'NLC'.
  -- For example
  --
  -- > > nlcToValuator (CJ [A 'x', N (A 'y')])
  -- > [('x',True),('y',False)]

  nldToValuator,
  -- | Inverse operation of 'valuatorToNLD'. Undefined, if the argument is not a 'NLD'.
  -- For example
  --
  -- > > nldToValuator (DJ [A 'x', N (A 'y')])
  -- > [('x',False),('y',True)]

  cnfToValuatorList,
  -- | Inverse operation of 'valuatorListToCNF', undefined if the argument is not a 'CNF'.

  dnfToValuatorList,
  -- | Inverse operation of 'valuatorListToDNF', undefined if the argument is not a 'DNF'.

  truthTableZeroValuators,
  -- | extracts all valuators that have a @1@ (i.e. @True@) in their result column.

  truthTableUnitValuators,
  -- | extracts all valuators that have a @0@ (i.e. @False@) in their result column.

  truthTableToDNF,
  -- | as explained above. In fact, @truthTableToDNF = valuatorListToDNF . truthTableUnitValuators@.
  -- Actually, the result is not only a 'DNF', but even a 'NaturalDNF', as described below.

  truthTableToCNF,
  -- | the dual version of 'truthTableToDNF'. Again, the result is even a 'NaturalCNF'.

  -- * Normal Forms

  -- ** Normalizations and Canonizations in general

  -- *** Definition

  -- | Let @S@ be any type (or set) and @~@ an /equivalence relation/ on @S@. A function @nf :: S -> S@ is then said to be
  --
  --   * a /normalizer/ for @~@, when @x ~ y@ iff @(nf x) ~ (nf y)@, for all @x, y :: S@.
  --
  --   * a /canonic normalizer/ or /canonizer/ for @~@, if it is a normalizer and @x ~ y@ implies @(nf x) == (nf y)@,
  --     for all @x, y :: S@.
  --
  -- If the normalizer is not a trivial one (like @id :: S -> S@), then the images @(nf x)@ for @x :: S@ alltogether are a proper
  -- subset of @S@ itself, called /normal forms/. And they are /canonic normal forms/, if they are the result of a
  -- canonizer, i.e. if no two different normal forms are equivalent.

  -- *** Normalizations in Propositional Algebras

  -- | In a propositional algebra @'PropAlg' a p@ we have three equivalence relations on the type @p@ of propositions:
  --
  --   * 'equivalent', the /semantic equivalence/ relation
  --
  --   * 'equiatomic', the /equiatomic/ or /atomic equivalence/ relation
  --
  --   * 'biequivalent', the /biequivalence/ or /theory-algebraic equivalence/ relation
  --
  --  Accordingly, we call a normalizer (or canonizer) @nf :: p -> p@ a
  --
  --   * /semantic/ normalizer, if it is a normalizer for 'equivalent',
  --
  --   * /atomic/ normalizer, if it is a normalizer for 'equiatomic'
  --
  --   * /theory-algebraic/ normalizer or /bi-normalizer/, if it is a normalizer for 'biequivalent'
  --
  -- Traditionally, a normalizer is always a semantic normalizer, and all the normalizers we further introduce in this module are
  -- of this kind. The notion of an atomic normalizer is actually quite superfluous, because it is not really relevant in practice.
  -- But in our concept of a propositional algebra 'PropAlg', two propositions are considered equal (from this abstract point of view),
  -- if they are biequivalent, i.e. if they are equivalent and also have the same atoms.

  -- *** Default semantic normalizations

  -- | In this default approach to normalizations, we only introduce important and more or less common examples and don't built
  -- propositional algebras on them. Most of the normalizers are semantic normalizer.
  --
  -- We also hope to increase the understanding by introducing the normalizer not as a function @nf :: PropForm a -> PropForm a@,
  -- but as @nf :: PropForm a -> NF a@, where @type NF a = PropForm a@ stands for the normal subset of formulas.

  -- ** Ordered Propositional Formulas

  -- | Next to the /semantic/ and /atomic/ order relations, given by `subvalent` and `subatomic`, respectively, there is
  -- also a /formal/ order '<=' defined on formulas.
  -- Different to the semantic and atomic order, this formal order is not only a partial, but even a linear order relation.
  -- In Haskell, these linear order structures are realized as instances of the 'Ord' type class, i.e. we can 'compare'
  -- formulas and check for '<', '<=', '=='  between formulas etc, given that the atom type itself is ordered.
  -- The actual implementation of the formal order is irrelevant here.
  -- The only thing of importance here is that fact that we can use it and that way we can remove ambiguities and
  -- increase the effectiveness of some operations. In other words, it induces a normalization as follows.
  --
  -- (Note however, that we did not simply use @deriving@ to implement it, because we adopted the common definition
  -- of literals 'LitForm', and that would have compromised an understanding of 'NLC's and 'NLD's as ordered formulas.
  -- For example, we need @N (A 10) < A 20@ to be true.)
  --
  -- Recall from the axioms of propositional algebras, that conjunctions and disjunctions are both idempotent and commutative,
  -- i.e. we can remove multiple occurring components and arbitrarily change the component order in each conjunction and
  -- disjunction. The result is still (bi-) equivalent.
  -- For example, @CJ [A 'z', A 'x', A 'z', A 'y', A 'z']@ is (bi-)equivalent to @CJ [A 'x', A 'y', A 'z']@.

  OrdPropForm,
  -- | A propositional formula @&#966;@ is said to be /ordered/, if the components @&#966;1, &#966;2..., &#966;N@ of every
  -- conjunction @CJ [&#966;1, &#966;2..., &#966;N]@ and every disjunction @DJ [&#966;1, &#966;2..., &#966;N]@ occurring in
  -- @&#966;@ are strictly ordered in the sense that  @&#966;1 < &#966;2 < ... < &#966;N@.

  isOrdPropForm,
  -- | is @True@ iff the argument is an ordered propositional formula.

  ordPropForm,
  -- | turns each formula into a (bi-)equivalent ordered form. For example,
  --
  -- > > ordPropForm (DJ [A 'x', A 'x', A 'z', A 'y', A 'x'])
  -- > DJ [A 'x',A 'y',A 'z']
  --
  -- > > ordPropForm (CJ [A 'x', T, N (A 'x'), T, F, A 'x'])
  -- > CJ [A 'x',F,T,N (A 'x')]                                -- because A 'x' < F < T < N (A 'x')

  -- ** Evaluated Normal Forms

  -- | It is common to avoid unary conjunctions @CJ [&#966;]@ and disjunctions @DJ [&#966;]@, both are (bi-)equivalent to @&#966;@
  -- itself. And a nullary @CJ []@ is usually replaced by @T@, and @DJ []@ by @F@.
  -- Nullary and unary sub- and equijunctions are less common, but they are also easily replaceable by their equivalent form @T@.
  --
  -- Another easy simplification of formulas is the involvement of @F@ or @T@ as subformulas. For example, @N F@ is obviously
  -- (bi-)equivalent to @T@, @CJ [&#966;, T, &#968;]@ is (bi-)equivalent to @CJ [&#966;, &#968;]@ and  @CJ [&#966;, F, &#968;]@
  -- is equivalent to  @T@. Similar rules hold for the other junctions (although a little more complicated in case of @SJ@ and @EJ@).
  --
  -- The replacement of these mentioned subformulas is quite an easy and effective (semantic) normalization and we call it
  -- /evaluation/ 'eval', because it is in a certain way a generalization of the /boolean evaluation/ 'boolEval'.

  EvalNF,
  -- | A formula is called an /Evaluated Normal Form/ iff it is either @F@ or @T@ or a formula that neither has bit values
  -- nor trivial junctions.
  -- By /having bit values/ we mean that it contains @F@s or @T@s as subformulas.
  -- By /having trivial junctions/ we mean nullary or unary junctions with @CJ@, @DJ@, @SJ@, or @EJ@ as subformulas.

  isEvalNF,
  -- | checks, if the given formula is indeed an Evaluated Normal Form. For example,
  --
  -- > > isEvalNF ( T )
  -- > True
  -- > > isEvalNF ( CJ [T, A 'x'] )
  -- > False                               -- because the formula has a bit value T
  -- > > isEvalNF ( CJ [] )
  -- > False                               -- because the formula has a trivial junction, namely the nullary conjunction
  -- > > isEvalNF ( SJ [A 'x'] )
  -- > False                               -- because it has a trivial junction, namely an unary subjunction
  -- > > isEvalNF ( CJ [A 'x', N (A 'y)] )
  -- > True                                -- has neither bit values nor trivial junctions

  -- *** Generalized evaluation and application for formulas: @eval@ and @apply@

  eval,
  -- | 'eval' takes a given formula and returns an equivalent Evaluated Normal Form.
  -- This way, it is a generalization of 'boolEval', which was only defined for nullatomic formulas.
  -- More precisely, if @&#966;@ is a nullatomic formula, then @boolEval &#966;@ is @True@ (or @False@) iff @bool &#966;@ is
  -- @T@ (or @F@, respectively).
  -- For example
  --
  -- > > eval (N F)
  -- > T
  -- >
  -- > > eval (DJ [A 'x', F, A 'y'])
  -- > DJ [A 'x',A 'y']
  -- >
  -- > > eval (DJ [A 'x', T, A 'y'])
  -- > T
  -- >
  -- > > eval (EJ [A 'x', T])
  -- > A 'x'
  -- >
  -- > > eval (EJ [A 'x', F])
  -- > N (A 'x')
  -- >
  -- > > eval (EJ [A 'x', A 'y'])
  -- > EJ [A 'x',A 'y']
  --
  -- 'eval' is  not as powerful as e.g. 'valid', i.e. it does not return 'T' for every valid input formula. For example,
  --
  -- > > eval (DJ [A 'x', N (A 'x')])
  -- > DJ [A 'x', N (A 'x')]
  --
  -- The advantage is that 'eval' is only of linear time complexity (at least if the input formula does not contain subjunctions
  -- or equijunctions; in which case the effort is somewhat greater).
  --
  -- Because the resulting formula is always equivalent to the argument formula, 'eval' is an example of a /(semantic) normalizer/
  -- (see 'EvalNF' below).

  apply,
  -- | As 'eval' is a generalization of 'boolEval', 'apply' is a generalization of 'boolApply', defined by
  -- @apply &#966; &#969; = eval (valuate &#969; &#966;)@.
  -- For example,
  --
  -- > > apply (DJ [A 'x', A 'y', A 'z']) [('y', True)]
  -- > T
  --
  -- > > apply (DJ [A 'x', A 'y', A 'z']) [('y', False)]
  -- > DJ [A 'x',A 'z']

  -- ** Literal Form(ula)s

  -- | A /literal/ is an atom with an assigned boolean value. The type for the semantic version of a literal was earlier introduced
  -- as the 'LiteralPair'. The formal version is the /literal formula/ 'LitForm', which is either an atomic formula @A x@ or a
  -- negated atomic formula @N (A x)@.
  -- This is the common definition of a literal in logic, although more close to the original idea would be the form @EJ [A x, T]@
  -- instead of @A x@ and @EJ [A x, F]@ instead of @N (A x)@. But it is common in the literature to use the
  -- shorter and (bi-)equivalent versions @A x@ and @N (A x)@, instead.
  --
  -- Of course, the literal formulas do not induce a normalization, most formulas don't have an equivalent literal formula.
  -- But literal formulas are important building blocks of the most important normalizations in propositional logic.

  LitForm,
  -- | is the type synonym for all formulas @A x@ or @N (A x)@.

  isLitForm,
  -- | checks if a formula is indeed a literal form.

  litFormAtom,
  -- | returns the atom of a literal form, e.g.
  --
  -- > > litFormAtom (A 'x')
  -- > 'x'
  --
  -- > > litFormAtom (N (A 'x'))
  -- > 'x'

  litFormValue,
  -- | returns the boolean value of a literal form, e.g.
  --
  -- > > litFormValue (A 'x')
  -- > True
  --
  -- > > litFormValue (N (A 'x'))
  -- > False

  -- ** Negation Normal Forms
  NegNormForm,
  -- | A /Negation Normal Form/ is either a literal form, or a conjunction or a disjunction of negation normal forms.
  -- This subset of propositional formulas is a normal subset in the sense that each formula has a (bi-)equivalent
  -- negation normal form.

  isNegNormForm,
  -- | checks if a given propositional formula is in negation normal form.

  negNormForm,
  -- | is the according normalizer, i.e. it returns an equivalent negation normal form.
  -- For example,
  --
  -- > >  negNormForm (N (CJ [N (SJ [A 'x', A 'y']), N T, F]))
  -- > DJ [CJ [DJ [N (A 'x'),A 'y']],CJ [],CJ []]
  -- >
  -- > > negNormForm (N (CJ [N (DJ [A 'x']), N (DJ [N (A 'x')])]))
  -- > DJ [DJ [A 'x'],DJ [N (A 'x')]]
  --
  -- This conversion is based on four laws for the removal of @T@, @F@, @SJ@ and @EJ@:
  --
  -- @T &#8660; CJ[]@
  --
  -- @F &#8660; DJ[]@
  --
  -- @[&#966;1 &#8594; &#966;2 &#8594; ... &#8594; &#966;N] &#8660; [[-&#966;1 + &#966;2] * [-&#966;2 + &#966;3] * ...]@
  --
  -- @[&#966;1 &#8596; &#966;2 &#8596; ... &#8596; &#966;N]  &#8660; [[&#966;1 * &#966;2 * ... * &#966;N] + [-&#966;1 * -&#966;2 * ... * -&#966;N]]@
  --
  -- and seven laws to remove negations other than negations of atomic formulas:
  --
  -- @-T &#8660; DJ[]@
  --
  -- @-F &#8660; CJ[]@
  --
  -- @--&#966;  &#8660; &#966;@
  --
  -- @-[&#966;1 * &#966;2 * ... * &#966;N]  &#8660; [-&#966;1 + -&#966;2 + ... + -&#966;N]@
  --
  -- @-[&#966;1 + &#966;2 + ... + &#966;N]  &#8660; [-&#966;1 * -&#966;2 * ... * -&#966;N]@
  --
  -- @-[&#966;1 &#8594; &#966;2 &#8594; ... &#8594; &#966;N]  &#8660; [[&#966;1 * -&#966;2] + [&#966;2 * -&#966;3] + ...]@
  --
  -- @-[&#966;1 &#8596; &#966;2 &#8596; ... &#8596; &#966;N]  &#8660; [[&#966;1 + &#966;2 + ... + &#966;N] * [-&#966;1 + -&#966;2 + ... + -&#966;N]]@
  --
  -- Note, that each each formula does have an biequivalent negation normal form, but our normalizer only returns equivalent
  -- forms in general. For example,
  --
  -- > > negNormForm (SJ [A 'x'])
  -- > CJ []                                    -- i.e. the atom 'x' is lost and this result is only equivalent

  -- ** NLCs and NLDs, CNFs and DNFs

  -- | Probably the most important and best known normal forms are the /Conjunctive Normal Forms/ or /CNFs/ and the
  -- /Disjunctive Normal Forms/ or /DNFs/. These forms are mutually /dual/, as it is called.
  --
  -- Let us consider DNFs. Usually, a DNF is defined as a disjunction of conjunctions of literals. An example DNF would then be
  --
  -- > DJ [CJ [A 'x', A 'y'], CJ [N (A 'y'), A 'y'], CJ [A 'z', N (A 'x')]]
  --
  -- But our definition of a DNF is more restrictive, because we demand each literal conjunction @CJ [&#955;_1, ..., &#955;_n]@
  -- to be a /normal literal conjunction/ in the sense that the literal atoms must be strictly ordered, i.e.
  -- @'litFormAtom' &#955;_1 < ... < 'litFormAtom' &#955;_n@.
  -- And this is obviously not the case for the second and third of the three literal conjunctions of the given example.
  --
  -- Every literal conjunction can easily be converted into a NLC, unless it contains a complementary pair of literal, such as the
  -- @CJ [N (A 'y'), A 'y']@. In that case, it is equivalent to @F@, and as a component of the disjunction, it may be removed
  -- alltogether without changing the semantics.
  -- So the example formula in a proper DNF version would be
  --
  -- > DJ [CJ [A 'x', A 'y'], CJ [A 'x', N (A 'z')]]
  --
  -- Note, that NLCs, NLDs, DNFs and CNFs are further specializations of Negation Normal Forms, so it is intuitive to introduce them
  -- as subtypes via the @type@ synonym.

  NLC,
  -- | A /normal literal conjunction/ or /NLC/ is a conjunction @CJ [&#955;_1, ..., &#955;_n]@ with
  -- @&#955;_1,...,&#955;_n :: 'LitForm' a@ and @'litFormAtom' &#955;_1 < ... < 'litFormAtom' &#955;_n@.

  isNLC,
  -- | checks if the formula is indeed a NLC. For example,
  --
  -- > > isNLC (CJ [A 'a', N (A 'b'), A 'c'])
  -- > True

  NLD,
  -- | A /normal literal disjunction/ or /NLD/ is a disjunction @DJ [&#955;_1, ..., &#955;_n]@ with
  -- @&#955;_1,...,&#955;_n :: 'LitForm' a@ and @'litFormAtom' &#955;_1 < ... < 'litFormAtom' &#955;_n@.

  isNLD,
  -- | checks if the formula is a NLD.

  CNF,
  -- | A /Conjunctive Normal Form/ or /CNF/ is a conjunction @CJ [&#948;_1,...,&#948;_n]@, of NLDs @&#948;_1,...,&#948;_n@.

  isCNF,
  -- | checks if the argument is a CNF. For example,
  --
  -- > > isCNF (CJ [DJ [A 'x', A 'y'], DJ [N (A 'x'), A 'z']])
  -- > True

  DNF,
  -- | A /Disjunctive Normal Form/ or /DNF/ is a disjunction @DJ [&#947;_1,...,&#947;_n]@ of NLCs @&#947;_1,...,&#947;_n@.

  isDNF,
  -- | checks if the argument is a DNF.

  -- ** Natural DNFs and CNFs
  -- | Recall, that 'truthTable' takes a formula and returns a 'TruthTable', and that we can use 'truthTableToDNF' to convert
  -- this into a formula, again. Combining the two steps @truthTableToDNF.truthTable@ defines a normalization of formulas into
  -- DNFs we call this the /natural DNF/ of the formula.
  --
  -- Actually, it is more common to call this the /canonic DNF/, but this title is not correct in a strict understanding of
  -- our canonizer notion.
  -- For example, the two formulas @DJ [A 'x', T]@ and @DJ [A 'y', T]@ are equivalent, but their natural DNFs are different, so
  -- 'naturalDNF' is not a semantic canonizer.
  -- By the way, iIt is not an atomic canonizer either, because e.g. @DJ [A 'x', F]@ and its natural DNF @DJ[]@ don't have
  -- the same atoms.
  --
  -- A formal characterization of all the result DNFs of 'naturalDNF' says that: a 'DNF' is a 'NaturalDNF' iff all its component
  -- NLCs are of the same length (i.e. they have the same atom set).

  NaturalDNF,
  NaturalCNF,
  isNaturalDNF,
  isNaturalCNF,
  naturalDNF,
  naturalCNF,

  -- ** Prime DNFs and CNFs

  -- | Natural DNFs are not really efficient to use, except for trivial cases, because a formula of @n@ atoms comprises up to
  -- @2^n@ NLCs, each one containing @n@ literals.
  -- In an attempt to define shorter and more efficient versions of DNFs and CNFs, we come up with two different approaches:
  -- one is the /prime/, the other one is the /minimal/ normal form. The properties of these forms are very important for
  -- our whole design. Some of them might be surprising, e.g. that prime and minimal forms are only sometimes identical, and
  -- that prime forms do make (semantic) canonizations and minimal forms do not.
  -- We first define and generate these prime forms here, minimal forms are introduced further below.

  -- *** Formal definition

  -- **** Prime Disjunctive Normal Forms

  -- | Given an arbitrary 'PropForm' @&#966;@, a 'DNF' @&#916; = DJ [&#947;_1,...,&#947;_n]@ and any 'NLC'
  -- @&#947; = CJ [&#955;_1,...,&#955;_k]@, all on the same atom type. We say that
  --
  --  (1) @&#947;@ is a /factor/ of @&#916;@, if @&#947; &#8658; &#916;@, i.e. if @&#947@ is 'subvalent' to @&#916;@.
  --      (Note, that each of the components @&#947;_1,...,&#947;_n@ of @&#916;@ is a factor of @&#916;@.)
  --
  --  (2) @&#947;@ is a /prime factor/ of @&#916;@, if it is a factor and there is not different factor @&#947;'@ of @&#916;@
  --      such that @&#947; &#8658; &#947;' &#8658; &#916;@.
  --      In other words, we cannot delete any of the literals @&#955;_1,...,&#955;_k@ in @&#947;@ without violating the
  --      subvalence @&#947; &#8658; &#916;@.
  --
  --  (3) @&#916;@ is a /Prime Disjunctive Normal Form/ or /PDNF/, if the @&#947;_1,...,&#947;_n@ are exactly all the
  --      prime factors of @&#916;@.
  --      To futher remove disambiguities, we also demand that @&#916;@ is /ordered/, as earlier defined, which means that
  --      @&#947;_1 < ... < &#947;_n@.
  --
  --  (4) @&#916;@ is /the (ordered) PDNF/ of @&#966;@ iff @&#916;@ is an (ordered) PDNF equivalent to @&#916;@.
  --
  -- It is easy to proof that every formula @&#916;@ has one and only one equivalent PDNF.
  -- So this does induce a semantic /canonization/ of propositional formulas.
  -- The canonization is not a bi-canonization, however, because the normal form is not always equiatomic.
  -- For example, @DJ [CJ []]@ is the PDNF of @EJ [A 5, A 5]@, but the atom @5@ is lost.

  -- **** Prime Conjunctive Normal Forms

  -- | This is /dual/ to the previous definition, i.e. given a formula @&#966;@, a 'CNF' @&#915; = [&#948;_1,...,&#948;_n]@ and
  -- a 'NLD' @&#948; = DJ [&#955;_1,...,&#955_k]@, then
  --
  --   (1) @&#948;@ is a /cofactor/ of @&#915;@, if @&#915; &#8658; &#948;@.
  --
  --   (2) @&#948;@ is a /prime cofactor/ of @&#915;@, if it is a cofactor and we cannot delete any of the literals
  --       @&#955;_1,...,&#955;_k@ in @&#948;@ without violating the subvalence @&#915; &#8658; &#948;@.
  --
  --   (3) @&#915;@ is a /Prime Conjunctive Normal Form/ or /PCNF/, if the @&#948;_1,...,&#948;@ are exactly the set of all its
  --       prime cofactors, and if these cofactors are ordered.
  --
  --   (4) @&#915;@ is /the (ordered) PCNF/ of @&#966;@ iff @&#915;@ is an (ordered) PCNF equivalent to @&#916;@.
  --
  -- Again, each formula has a unique equivalent PCNF.

  -- *** The Haskell types and functions

  PDNF,
  -- | is the type synonym for /Prime Disjunctive Normal Forms/ or /PDNF/s.

  PCNF,
  -- | is the type synonym for /Prime Conjunctive Normal Forms/ or /PCNF/s.

  primeDNF,
  -- | is the PDNF canonizer, i.e. it turns each given formula into its unique PDNF. For example,
  --
  -- > > primeDNF (EJ [A 'x', A 'y'])
  -- > DJ [CJ [N (A 'x'),N (A 'y')],CJ [A 'x',A 'y']]
  -- >
  -- > > display it             -- "it" is the previous value in a ghci session, here the PDNF
  -- > [[-x * -y] + [x * y]]
  -- >
  -- > > primeDNF T
  -- > DJ [CJ []]               -- all valid/tautological formulas have this same PDNF
  --
  -- > > primeDNF F
  -- > DJ []                    -- all contradictory formulas have this same PDNF

  primeCNF,
  -- | is the PCNF canonizer, i.e. the result is the unique PCNF of the given formula. For example,
  --
  -- > > primeCNF (EJ [A 'x', A 'y'])
  -- > CJ [DJ [N (A 'x'),A 'y'],DJ [A 'x',N (A 'y')]]
  -- >
  -- > > display it              -- means: display the previous formulas
  -- > [[-x + y] * [x + -y]]
  -- >
  -- > > primeCNF (EJ [A 'x', A 'x'])
  -- > CJ []                     -- which is the same PCNF for all tautological formulas

  -- *** The /default/ and the /fast/ generation of Prime Normal Forms

  -- Although Prime (Disjunctive/Conjunctive) Normal Forms are generally much more compact than Natural DNFs (or Natural CNFs),
  -- the naive or /default/ method to construct them is of exponential time order.
  --
  -- ..................CONTINUEHERE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!...............................

  validates,
  -- | We say that a valuator @v@ /validates/ a formula @p@, iff the @p@-application on @v@ is valid. In other words,
  --
  -- > (validates v p) == valid (apply p v)

  falsifies,
  -- | We say that a valuator @v@ /falsifies/ a formula @p@, iff the @p@-application on @v@ is unsatisfiable. In other words,
  --
  -- > (falsifies v p) == not (satisfiable (apply p v))

  directSubvaluators,
  -- | For example,
  --
  -- > directSubvaluators [(2,True),(4,False),(6,True)] ==
  -- >  [[(2,True),(4,False)],[(2,True),(6,True)],[(4,False),(6,True)]]

  allDirectSubvalidators,
  allDirectSubfalsifiers,
  primeValuators,
  coprimeValuators,

  -- *** Reference to the fast generation of Prime Normal Forms
  -- | Note, that the PropLogic package also provides functions 'pdnf' and 'pcnf' that do exactly the same as 'primeDNF' and
  -- 'primeCNF'.
  -- .... CONTINUEHERE .....

  -- ** Minimal DNFs and CNFs
  MDNF,
  MCNF,
  minimalDNFs,
  minimalCNFs,

  -- ** Simplified DNFs and CNFs
  SimpleDNF, SimpleCNF,
  simpleDNF, simpleCNF,

  -- * Propositional algebras

  -- | ..... CONTINUEHERE .....

  -- ** @PropAlg a (PropForm a)@, the Propositional Formula Algebra

  -- | ..... CONTINUEHERE .....

  ext',

  -- ** @PropAlg a (PropForm a)@, the Truth Table Algebra

  -- | ..... CONTINUEHERE .....

  -- ** @PropAlg Void Bool@, the Boolean Value Algebra

  -- | Recall, that the /boolean/ or /bit value algebra/ is a predefined, integrated of Haskell, comprising:
  --
  -- >   False, True :: Bool
  -- >   not :: Bool -> Bool
  -- >   (&&), (||), (<), (<=), (==), (/=), (=>), (>) :: Bool -> Bool -> Bool
  -- >   and, or :: [Bool] -> Bool
  -- >   compare :: Bool -> Bool -> Ordering, -- where data Ordering = LT | EQ | GT
  -- >   all, any :: (a -> Bool) -> [a] -> Bool
  --
  -- ..... CONTINUEHERE .....

) where ---------------------------------------------------------------------------------------------------------------

-- import

  import qualified List        as L
  import qualified Olist       as O
  import qualified TextDisplay as D
  import PropLogicCore

-- formula deconstructors

  juncDeg :: PropForm a -> Int
  juncDeg (A _)  = 0
  juncDeg F      = 1
  juncDeg T      = 2
  juncDeg (N _)  = 3
  juncDeg (CJ _) = 4
  juncDeg (DJ _) = 5
  juncDeg (SJ _) = 6
  juncDeg (EJ _) = 7

  juncArgs :: PropForm a -> [PropForm a]
  juncArgs (A x)   = [A x]
  juncArgs F       = []
  juncArgs T       = []
  juncArgs (N p)   = [p]
  juncArgs (CJ pL) = pL
  juncArgs (DJ pL) = pL
  juncArgs (SJ pL) = pL
  juncArgs (EJ pL) = pL

  juncCons :: Int -> [PropForm a] -> PropForm a
  juncCons 0 [A x] = A x
  juncCons 1 []    = F
  juncCons 2 []    = T
  juncCons 3 [p]   = N p
  juncCons 4 pL    = CJ pL
  juncCons 5 pL    = DJ pL
  juncCons 6 pL    = SJ pL
  juncCons 7 pL    = EJ pL

-- size

  atomSize :: PropForm a -> Int
  atomSize (A a) = 1
  atomSize F = 0
  atomSize T = 0
  atomSize (N p) = atomSize p
  atomSize (CJ ps) = sum (map atomSize ps)
  atomSize (DJ ps) = sum (map atomSize ps)
  atomSize (SJ ps) = sum (map atomSize ps)
  atomSize (EJ ps) = sum (map atomSize ps)

  juncSize :: PropForm a -> Int
  juncSize (A a) = 0
  juncSize F = 1
  juncSize T = 1
  juncSize (N p) = 1 + (juncSize p)
  juncSize (CJ ps) = 1 + sum (map juncSize ps)
  juncSize (DJ ps) = 1 + sum (map juncSize ps)
  juncSize (SJ ps) = 1 + sum (map juncSize ps)
  juncSize (EJ ps) = 1 + sum (map juncSize ps)

  size :: PropForm a -> Int
  size f = (atomSize f) + (juncSize f)

-- valuators and truth tables

  type LiteralPair a = (a,Bool)

  type Valuator a = O.Olist (LiteralPair a)

  type TruthTable a = (O.Olist a, Maybe (PropForm a), [([Bool],Bool)])

  type MultiTruthTable a = (O.Olist a, [PropForm a], [([Bool], [Bool])])

-- type correctness checking

  correctValuator :: Ord a => [LiteralPair a] -> Valuator a
  correctValuator [] = []
  correctValuator [(a,b)] = [(a,b)]
  correctValuator ((a1,b1):(a2,b2):pL) = case compare a1 a2 of
    LT -> (a1,b1) : (correctValuator ((a2,b2):pL))
    EQ -> error "correctValuator: multiple occurring atoms"
    GT -> error "correctValuator: atoms are not in order"

  correctTruthTable :: Ord a => ([a], Maybe (PropForm a), [([Bool],Bool)]) -> TruthTable a
  correctTruthTable (atomL, p, rowL) =
    if O.isOlist atomL
    then if dualPower (length atomL) == length rowL
         then if Prelude.all (\ (bL,b) -> (length bL) == (length atomL)) rowL
              then if O.isOlist (fst (unzip rowL))
                   then if O.included (atomList p) atomL
                        then (atomL, p, rowL)
                        else error "correctTruthTable -- not all atoms of the formula are in the given atom list"
                   else error "correctTruthTable -- valuators are not in strict order"
              else error "correctTruthTable -- some rows are not well-formed"
         else error "correctTruthTable -- wrong number of rows"
    else error "correctTruthTable -- atom list is not in strict order"
    where dualPower n = if n == 0
                        then 1
                        else 2 * (dualPower (n - 1))
          atomList Nothing = []
          atomList (Just p)  = atoms p

  correctMultiTruthTable :: Ord a => ([a], [PropForm a], [([Bool], [Bool])]) -> MultiTruthTable a
  correctMultiTruthTable (atomL, pL, rowL) =
    if O.isOlist atomL
    then if dualPower (length atomL) == length rowL
         then if Prelude.all (\ (bL,rL) -> ((length bL)==(length atomL) && (length rL)==(length pL))) rowL
              then if O.isOlist (fst (unzip rowL))
                   then if Prelude.all (\ p -> O.included (atoms p) atomL) pL
                        then (atomL, pL, rowL)
                        else error "correctMultiTruthTable -- some atoms in the formulas are not in the atom list"
                   else error "correctMultiTruthTable -- valuators are not in strict order"
              else error "correctMultiTruthTable -- some rows are not well-formed"
         else error "correctMultiTruthTable -- wrong number of rows"
    else error "correctMultiTruthTable -- atom list is not in strict order"
    where dualPower n = if n == 0
                        then 1
                        else 2 * (dualPower (n - 1))

-- Display instances

  instance D.Display a => D.Display (LiteralPair a) where
    textFrame (a,b) = D.textFrame [(a,b)]

  instance D.Display a => D.Display (Valuator a) where
    textFrame litL = D.textFrameBox (D.plainMerge (D.normalTextFrameTable (map litRow litL)))
      where boolTextFrame b = if b then ["1"] else ["0"]
            litRow (a,b) = [D.textFrame a, [" = "], boolTextFrame b]

  instance (Ord a, D.Display a) => D.Display [Valuator a] where
    textFrame vL = textFrame
      where atomL = O.olist (concat (map (fst . unzip) vL))
            headRow = map D.textFrame atomL
            bodyRow _ [] = []
            bodyRow [] (a':atomL) = [""] : (bodyRow [] atomL)
            bodyRow ((a,b):litL) (a':atomL) = case compare a a' of
              EQ -> (D.textFrame b) : (bodyRow litL atomL)
              GT -> [] : (bodyRow ((a,b):litL) atomL)
            textFrameTable = headRow : (map (\ valuator -> (bodyRow valuator atomL)) vL)
            textFrame = D.gridMerge (D.normalTextFrameTable textFrameTable)

  instance D.Display a => D.Display (TruthTable a) where
    textFrame (atomL, maybeForm, pairL) = textFrame
      where formTF = case maybeForm of
              Nothing   -> [""]
              Just form -> D.textFrame form
            headRow = (map D.textFrame atomL) ++ [formTF]
            bodyRow (boolL,b) = (map D.textFrame boolL) ++ [D.textFrame b]
            textFrameTable = [headRow] ++ (map bodyRow pairL)
            textFrame = D.gridMerge (D.normalTextFrameTable textFrameTable)

  instance D.Display a => D.Display (MultiTruthTable a) where
    textFrame (atomL, formL, pairL) = textFrame'
      where ordinalList = map (\ n -> [" " ++ (show n) ++ ". "]) (take (length formL) [1..]) :: [D.TextFrame]
            formBlock = D.plainMerge (D.centerAlign (D.leftAlign (L.transpose [ordinalList, map D.textFrame formL])))
            headRow = (map D.textFrame atomL) ++ ordinalList
            bodyRow (bL1, bL2) = (map D.textFrame bL1) ++ (map D.textFrame bL2)
            textFrameTable = [headRow] ++ (map bodyRow pairL)
            tableBlock = D.gridMerge (D.normalTextFrameTable textFrameTable)
            textFrame = D.correctTextFrame (["MultiTruthTable"] ++ formBlock ++ tableBlock)
            textFrame' = D.textFrameBox textFrame

-- literal normal forms and semantic type conversion

  valuatorToNLC :: Valuator a -> NLC a
  valuatorToNLC pairL = CJ (map litForm pairL)
    where litForm (x,True)  = A x
          litForm (x,False) = N (A x)

  valuatorToNLD :: Valuator a -> NLD a
  -- Note, that the literals are inverted!
  valuatorToNLD pairL = DJ (map litForm pairL)
    where litForm (x,True)  = N (A x)
          litForm (x,False) = A x

  valuatorListToCNF :: [Valuator a] -> CNF a
  valuatorListToCNF vL = CJ (map valuatorToNLD vL)

  valuatorListToDNF :: [Valuator a] -> DNF a
  valuatorListToDNF vL = DJ (map valuatorToNLC vL)

  nlcToValuator :: NLC a -> Valuator a
  nlcToValuator (CJ litL) = map litPair litL
    where litPair (N (A a)) = (a,False)
          litPair (A a)     = (a,True)

  nldToValuator :: NLD a -> Valuator a
  nldToValuator (DJ litL) = map litPair litL
    where litPair (N (A a)) = (a,True)  -- The literal values are inverted!
          litPair (A a)     = (a,False) -- The literal values are inverted!

  cnfToValuatorList :: CNF a -> [Valuator a]
  cnfToValuatorList (CJ pL) = map nldToValuator pL

  dnfToValuatorList :: DNF a -> [Valuator a]
  dnfToValuatorList (DJ pL) = map nlcToValuator pL

  truthTableZeroValuators :: TruthTable a -> [Valuator a]
  truthTableZeroValuators (atomL, _, pairL) =
    map (\ (bitL, b) -> zip atomL bitL) (filter (\ (bitL,b) -> (not b)) pairL)

  truthTableUnitValuators :: TruthTable a -> [Valuator a]
  truthTableUnitValuators (atomL, _, pairL) =
    map (\ (bitL, b) -> zip atomL bitL) (filter (\ (bitL,b) -> b) pairL)

-- valuators

  allValuators :: Ord a => [a] -> O.Olist (Valuator a)
  allValuators aL = iter (O.olist aL)
    where iter [] = [[]]
          iter (a:aL) = let vL = iter aL
                        in (map (\ v -> (a,False):v) vL) ++ (map (\ v -> (a,True):v) vL)

  unitValuators :: Ord a => PropForm a -> O.Olist (Valuator a)
  unitValuators p = filter (boolApply p) (allValuators (atoms p))

  zeroValuators :: Ord a => PropForm a -> O.Olist (Valuator a)
  zeroValuators p = filter (not . (boolApply p)) (allValuators (atoms p))

-- basic propositional semantics

  valuate :: Ord a => Valuator a -> PropForm a -> PropForm a
  valuate v (A x) = case (L.lookup x v) of
                      Nothing    -> (A x)
                      Just False -> F
                      Just True  -> T
  valuate v F = F
  valuate v T = T
  valuate v (N p)= N (valuate v p)
  valuate v (CJ pL) = CJ (map (valuate v) pL)
  valuate v (DJ pL) = DJ (map (valuate v) pL)
  valuate v (SJ pL) = SJ (map (valuate v) pL)
  valuate v (EJ pL) = EJ (map (valuate v) pL)

  boolEval :: PropForm a -> Bool
  boolEval p =  let pairwise rel [] = True
                    pairwise rel [x] = True
                    pairwise rel (x:y:zL) = (rel x y) && (pairwise rel (y:zL))
                in case p of
                    (A a)   -> error "boolEval: only defined for nullatomic formulas"
                    F       -> False
                    T       -> True
                    (N p)   -> not (boolEval p)
                    (CJ pL) -> and (map boolEval pL)
                    (DJ pL) -> or (map boolEval pL)
                    (SJ pL) -> pairwise (<=) (map boolEval pL)
                    (EJ pL) -> pairwise (==) (map boolEval pL)

  boolApply :: Ord a => PropForm a -> Valuator a -> Bool
  boolApply form valuator = boolEval (valuate valuator form)

-- truth tables and propositional formulas

  truthTable :: Ord a => PropForm a -> TruthTable a
  truthTable p = theTable
    where atomL = atoms p
          vL = allValuators atomL
          makeRow v = (snd (unzip v), boolApply p v)
          theTable = (atomL, Just p, map makeRow vL)

  plainTruthTable :: Ord a => PropForm a -> TruthTable a
  plainTruthTable p = theTable
    where atomL = atoms p
          vL = allValuators atomL
          makeRow v = (snd (unzip v), boolApply p v)
          theTable = (atomL, Nothing, map makeRow vL)

  truthTableBy :: Ord a => PropForm a -> [a] -> (Valuator a -> Bool) -> TruthTable a
  truthTableBy p aL boolFun = theTable
    where atomL = O.olist aL
          vL = allValuators atomL
          makeRow v = (snd (unzip v), boolFun v)
          theTable = (atomL, Nothing, map makeRow vL)

  multiTruthTable :: Ord a => [PropForm a] -> MultiTruthTable a
  multiTruthTable pL = theTable
    where atomL = O.unionList (map atoms pL)
          vL = allValuators atomL
          makeRow v = (snd (unzip v), map (\ p -> boolApply p v) pL)
          theTable = (atomL, pL, map makeRow vL)

  truthTableToDNF :: TruthTable a -> NaturalDNF a
  truthTableToDNF = valuatorListToDNF . truthTableUnitValuators

  truthTableToCNF :: TruthTable a -> NaturalCNF a
  truthTableToCNF = valuatorListToCNF . truthTableZeroValuators

-- auxiliary atom functions

  isRedundantAtom :: (Ord a) => a -> PropForm a -> Bool
  isRedundantAtom a p = equivalent (valuate [(a,False)] p) (valuate [(a,True)] p)

  infRedTruthTable :: (Ord a) => PropForm a -> [a] -> TruthTable a
  infRedTruthTable p aL = truthTableBy p aL isValidator
    where isValidator v = valid (valuate v p)

  supRedTruthTable :: (Ord a) => PropForm a -> [a] -> TruthTable a
  supRedTruthTable p aL = truthTableBy p aL isSatisfier
    where isSatisfier v = satisfiable (valuate v p)

-------------------------------------------- NORMAL FORMS ------------------------------------------------------------

-- The syntactic order on formulas and ordered normal forms

  instance Ord a => Ord (PropForm a) where
    compare (A x)     (A y)     = compare x y
    compare (N (A x)) (N (A y)) = compare x y
    compare (N (A x)) (A y)     = case compare x y of
                                    LT -> LT
                                    GT -> GT
                                    EQ -> LT
    compare (A x)     (N (A y)) = case compare x y of
                                    LT -> LT
                                    GT -> GT
                                    EQ -> GT
    compare p         q         = case compare (juncDeg p) (juncDeg q) of
                                    LT -> LT
                                    GT -> GT
                                    EQ -> lexCompare (juncArgs p) (juncArgs q)
      where lexCompare []     []     = EQ
            lexCompare []     _      = LT
            lexCompare _      []     = GT
            lexCompare (p:pL) (q:qL) = case compare p q of
                                         EQ -> lexCompare pL qL
                                         LT -> LT
                                         GT -> GT

  type OrdPropForm a = PropForm a

  isOrdPropForm :: Ord a => PropForm a -> Bool
  isOrdPropForm (A a)   = True
  isOrdPropForm F       = True
  isOrdPropForm T       = True
  isOrdPropForm (N p)   = isOrdPropForm p
  isOrdPropForm (CJ pL) = (all isOrdPropForm pL) && O.isOlist pL
  isOrdPropForm (DJ pL) = (all isOrdPropForm pL) && O.isOlist pL
  isOrdPropForm (SJ pL) = (all isOrdPropForm pL)
  isOrdPropForm (EJ pL) = (all isOrdPropForm pL)

  ordPropForm :: Ord a => PropForm a -> OrdPropForm a
  ordPropForm (A a)   = (A a)
  ordPropForm F       = F
  ordPropForm T       = T
  ordPropForm (N p)   = (N p)
  ordPropForm (CJ pL) = CJ (O.olist (map ordPropForm pL))
  ordPropForm (DJ pL) = DJ (O.olist (map ordPropForm pL))
  ordPropForm (SJ pL) = SJ (map ordPropForm pL)
  ordPropForm (EJ pL) = EJ (map ordPropForm pL)

-- Eval Norm Form

  type EvalNF a = PropForm a

  hasTrivialJuncs :: PropForm a -> Bool
  hasTrivialJuncs (A _) = False
  hasTrivialJuncs F = False
  hasTrivialJuncs T = False
  hasTrivialJuncs (N p) = hasTrivialJuncs p
  hasTrivialJuncs (CJ pL) = ((length pL) < 2) || or (map hasTrivialJuncs pL)
  hasTrivialJuncs (DJ pL) = ((length pL) < 2) || or (map hasTrivialJuncs pL)
  hasTrivialJuncs (SJ pL) = ((length pL) < 2) || or (map hasTrivialJuncs pL)
  hasTrivialJuncs (EJ pL) = ((length pL) < 2) || or (map hasTrivialJuncs pL)

  hasBitValues :: PropForm a -> Bool
  hasBitValues (A _) = False
  hasBitValues F = True
  hasBitValues T = True
  hasBitValues (N p) = hasBitValues p
  hasBitValues (CJ pL) = or (map hasBitValues pL)
  hasBitValues (DJ pL) = or (map hasBitValues pL)
  hasBitValues (SJ pL) = or (map hasBitValues pL)
  hasBitValues (EJ pL) = or (map hasBitValues pL)

  isEvalNF :: PropForm a -> Bool
  isEvalNF F = True
  isEvalNF T = True
  isEvalNF p = not (hasTrivialJuncs p) && not (hasBitValues p)

-- generalized evaluation and application

  eval :: PropForm a -> EvalNF a
  eval (A a)   = (A a)
  eval F       = F
  eval T       = T
  eval (N p)   = case (eval p) of
                   F -> T
                   T -> F
                   q -> (N q)
  eval (CJ ps) = cjEval([],ps)
  eval (DJ ps) = djEval([],ps)
  eval (SJ ps) = sjEval([],ps)
  eval (EJ ps) = ejEval([],ps)

  cjEval :: ([PropForm a],[PropForm a]) -> PropForm a
  cjEval([],[])   = T
  cjEval([p],[])  = p
  cjEval(ps,[])   = (CJ ps)
  cjEval(ps,q:qs) = case (eval q) of
                      F -> F
                      T -> cjEval(ps, qs)
                      r -> cjEval(ps ++ [r], qs)

  djEval :: ([PropForm a],[PropForm a]) -> PropForm a
  djEval([],[])   = F
  djEval([p],[])  = p
  djEval(ps,[])   = (DJ ps)
  djEval(ps,q:qs) = case (eval q) of
                      F -> djEval(ps, qs)
                      T -> T
                      r -> djEval(ps ++ [r], qs)

  sjEval :: ([PropForm a],[PropForm a]) -> PropForm a
  sjEval([],[])   = T
  sjEval([p],[])  = T
  sjEval(ps,[])   = (SJ ps)
  sjEval(ps,q:qs) = case (eval q) of
                     F -> cjEval(map N ps, [SJ qs])
                     T -> cjEval([SJ ps], qs)
                     r -> sjEval(ps++[r], qs)

  ejEval :: ([PropForm a],[PropForm a]) -> PropForm a
  ejEval([],[]) = T
  ejEval([p],[]) = T
  ejEval(ps,[]) = (EJ ps)
  ejEval(ps,q:qs) = case (eval q) of
                      F -> cjEval(map N ps, map N qs)
                      T -> cjEval(ps, qs)
                      r -> ejEval(ps ++ [r], qs)

  apply :: Ord a => PropForm a -> Valuator a -> EvalNF a
  apply form valuator = eval (valuate valuator form)

-- Literal form(ula)s

  type LitForm a = PropForm a

  isLitForm :: PropForm a -> Bool
  isLitForm p = case p of
    A a     -> True
    N (A a) -> True
    _       -> False

  litFormAtom :: LitForm a -> a
  litFormAtom (A a)     = a
  litFormAtom (N (A a)) = a
  litFormAtom _         = error "litFormAtom -- not a LitForm "

  litFormValue :: LitForm a -> Bool
  litFormValue (A a)     = True
  litFormValue (N (A a)) = False
  litFormValue _         = error "litFormAtom -- not a LitForm "

-- Negation Normal Forms

  type NegNormForm a = PropForm a

  isNegNormForm :: PropForm a -> Bool
  isNegNormForm (A _)     = True
  isNegNormForm F         = True
  isNegNormForm T         = True
  isNegNormForm (N (A _)) = True
  isNegNormForm (N _)     = False
  isNegNormForm (CJ pL)   = Prelude.all isNegNormForm pL
  isNegNormForm (DJ pL)   = Prelude.all isNegNormForm pL
  isNegNormForm (SJ _)    = False
  isNegNormForm (EJ _)    = False

  negNormForm :: PropForm a -> NegNormForm a
  negNormForm (A a) = (A a)
  negNormForm F = DJ []
  negNormForm T = CJ []
  negNormForm (CJ pL) = CJ (map negNormForm pL)
  negNormForm (DJ pL) = DJ (map negNormForm pL)
  negNormForm (SJ pL) = let iter [] = []
                            iter [p] = []
                            iter (p1:p2:pL) = (DJ [negNormForm (N p1), negNormForm p2]) : iter (p2:pL)
                        in CJ (iter pL)
  negNormForm (EJ pL) = DJ [CJ (map negNormForm pL), CJ (map (negNormForm . N) pL)]
  negNormForm (N (A a)) = N (A a)
  negNormForm (N F) = CJ []
  negNormForm (N T) = DJ []
  negNormForm (N (N p)) = negNormForm p
  negNormForm (N (CJ pL)) = DJ (map (negNormForm . N) pL)
  negNormForm (N (DJ pL)) = CJ (map (negNormForm . N) pL)
  negNormForm (N (SJ pL)) = let iter [] = []
                                iter [p] = []
                                iter (p1:p2:pL) = (CJ [negNormForm p1, negNormForm (N p2)]) : iter (p2:pL)
                            in DJ (iter pL)
  negNormForm (N (EJ pL)) = CJ [DJ (map negNormForm pL), DJ (map (negNormForm . N) pL)]

-- literal forms, NLCs, NLDs, DNFs and CNFs

  type NLC a = NegNormForm a

  type NLD a = NegNormForm a

  type CNF a = NegNormForm a

  type DNF a = NegNormForm a

  isNLC :: Ord a => PropForm a -> Bool
  isNLC (CJ pL) = (all isLitForm pL) && O.isOlist (map litFormAtom pL)
  isNLC _ = False

  isNLD :: Ord a => PropForm a -> Bool
  isNLD (DJ pL) = (all isLitForm pL) && O.isOlist (map litFormAtom pL)
  isNLD _ = False

  isCNF :: Ord a => PropForm a -> Bool
  isCNF (CJ pL) = all isNLD pL
  isCNF _ = False

  isDNF :: Ord a => PropForm a -> Bool
  isDNF (DJ pL) = all isNLC pL
  isDNF _ = False

-- Natural DNFs and CNFs

  type NaturalCNF a = CNF a

  type NaturalDNF a = DNF a

  isNaturalDNF :: Ord a => PropForm a -> Bool
  isNaturalDNF (DJ pL) = isDNF (DJ pL) && (Prelude.all fullAtomSet pL)
    where fullAtomSet q = O.equal (atoms (DJ pL)) (atoms q)
  isNaturalDNF _ = False

  isNaturalCNF :: Ord a => PropForm a -> Bool
  isNaturalCNF (CJ pL) = isCNF (CJ pL) && (Prelude.all fullAtomSet pL)
    where fullAtomSet q = O.equal (atoms (CJ pL)) (atoms q)
  isNaturalCNF _ = False

  naturalDNF :: Ord a => PropForm a -> NaturalDNF a
  naturalDNF = truthTableToDNF . truthTable

  naturalCNF :: Ord a => PropForm a -> NaturalCNF a
  naturalCNF = truthTableToCNF . truthTable

-- Prime DNFs and CNFs

  type PDNF a  = DNF a

  type PCNF a  = CNF a

  validates :: Ord a => Valuator a -> PropForm a -> Bool
  validates v p = valid (eval (valuate v p))

  falsifies :: Ord a => Valuator a -> PropForm a -> Bool
  falsifies v p = not (satisfiable (eval (valuate v p)))

  directSubvaluators :: Valuator a -> [Valuator a]
  directSubvaluators v = iter([],v)
    where iter(_,[]) = []
          iter(u,p:w) = iter(u ++ [p],w) ++ [u ++ w]

  allDirectSubvalidators :: Ord a => Valuator a -> PropForm a -> [Valuator a]
  allDirectSubvalidators v p = filter (\ v -> (validates v p)) (directSubvaluators v)

  allDirectSubfalsifiers :: Ord a => Valuator a -> PropForm a -> [Valuator a]
  allDirectSubfalsifiers v p = filter (\ v -> (falsifies v p)) (directSubvaluators v)

  primeValuators :: Ord a => PropForm a -> [Valuator a]
  primeValuators p = iter([],[],unitValuators p)
    where iter(primes,[],[]) = reverse primes
          iter(primes,validators,[]) = iter(primes,[],L.nub validators)
          iter(primes,validators,v:vL) = case (allDirectSubvalidators v p) of
            []  -> iter(v:primes,validators,vL)
            wL  -> iter(primes,validators ++ wL,vL)

  coprimeValuators :: Ord a => PropForm a -> [Valuator a]
  coprimeValuators p = iter([],[],zeroValuators p)
    where iter(coprimes,[],[]) = reverse coprimes
          iter(coprimes,falsifiers,[]) = iter(coprimes,[],L.nub falsifiers)
          iter(coprimes,falsifiers,v:vL) = case (allDirectSubfalsifiers v p) of
            [] -> iter(v:coprimes,falsifiers,vL)
            wL -> iter(coprimes,falsifiers ++ wL,vL)

  primeDNF :: Ord a => PropForm a -> PDNF a
  primeDNF = ordPropForm . valuatorListToDNF . primeValuators

  primeCNF :: Ord a => PropForm a -> PCNF a
  primeCNF = ordPropForm . valuatorListToCNF . coprimeValuators

-- Minimal DNFs and CNFs

  type MDNF a = DNF a

  type MCNF a = CNF a

  equivDirectSubDNFs :: Ord a => DNF a -> [DNF a]
  equivDirectSubDNFs (DJ nlcL) = iter nlcL []
    where iter [] nlcL' = []
          iter (nlc:nlcL) nlcL' = if subvalent nlc (DJ (nlcL ++ nlcL'))
                                  then (DJ (nlcL ++ nlcL')) : (iter nlcL (nlc : nlcL'))
                                  else iter nlcL (nlc : nlcL')

  minimalEquivalentIncludedDNFs :: Ord a => DNF a -> [DNF a]
  minimalEquivalentIncludedDNFs dnf = nextStep [] [] [dnf]
    where nextStep finalDnfs []       []            = O.olist finalDnfs
          nextStep finalDnfs nextDnfs []         = nextStep finalDnfs [] (O.olist nextDnfs)
          nextStep finalDnfs nextDnfs (dnf:dnfL) = case equivDirectSubDNFs dnf of
                                                     []    -> nextStep (dnf:finalDnfs) nextDnfs dnfL
                                                     dnfL' -> nextStep finalDnfs (nextDnfs ++ dnfL') dnfL

  equivDirectSubCNFs :: Ord a => CNF a -> [CNF a]
  equivDirectSubCNFs (CJ nldL) = iter nldL []
    where iter [] nldL' = []
          iter (nld:nldL) nldL' = if subvalent (CJ (nldL ++ nldL')) nld
                                  then (CJ (nldL ++ nldL')) : (iter nldL (nld : nldL'))
                                  else iter nldL (nld : nldL')

  minimalEquivalentIncludedCNFs :: Ord a => CNF a -> [CNF a]
  minimalEquivalentIncludedCNFs cnf = nextStep [] [] [cnf]
    where nextStep finalCnfs []       []         = O.olist finalCnfs
          nextStep finalCnfs nextCnfs []         = nextStep finalCnfs [] (O.olist nextCnfs)
          nextStep finalCnfs nextCnfs (cnf:cnfL) = case equivDirectSubCNFs cnf of
                                                    []    -> nextStep (cnf:finalCnfs) nextCnfs cnfL
                                                    cnfL' -> nextStep finalCnfs (nextCnfs ++ cnfL') cnfL

  minimalDNFs :: Ord a => PropForm a -> [MDNF a]
  minimalDNFs = minimalEquivalentIncludedDNFs . primeDNF

  minimalCNFs :: Ord a => PropForm a -> [MCNF a]
  minimalCNFs = minimalEquivalentIncludedCNFs . primeCNF

-- Simplified DNFs and CNFs

  type SimpleDNF a = PropForm a

  type SimpleCNF a = PropForm a

  simpleDNF :: Ord a => DNF a -> SimpleDNF a
  simpleDNF p = if isDNF p
                then eval p
                else error "simpleDNF -- argument is not a DNF"

  simpleCNF :: Ord a => CNF a -> SimpleCNF a
  simpleCNF p = if isCNF p
                then eval p
                else error "simpleCNF -- argument is not a CNF"

-------------------------------- PROPOSITIONAL ALGEBRAS ---------------------------------------------------------------

-- the propositional algebra of propositional formulas

  instance Ord a => PropAlg a (PropForm a) where
    at = A
    false = F
    true = T
    neg = N
    conj = CJ
    disj = DJ
    subj = SJ
    equij = EJ
    valid p = and resultColumn
      where (atomL, maybeProp, pairL) = truthTable p
            resultColumn = snd (unzip pairL)
    satisfiable p = or resultColumn
      where (atomL, maybeProp, pairL) = truthTable p
            resultColumn = snd (unzip pairL)
    subvalent p1 p2 = valid (SJ [p1,p2])
    equivalent p1 p2 = valid (EJ [p1,p2])
    covalent p1 p2 = satisfiable (CJ [p1,p2])
    disvalent p1 p2 = not (satisfiable (CJ [p1,p2]))
    properSubvalent p1 p2 = (subvalent p1 p2) && not (equivalent p1 p2)
    properDisvalent p1 p2 = (disvalent p1 p2) && satisfiable p1 && satisfiable p2
    atoms (A a) = [a]
    atoms F = []
    atoms T = []
    atoms (N p) = atoms p
    atoms (CJ pL) = O.unionList (map atoms pL)
    atoms (DJ pL) = O.unionList (map atoms pL)
    atoms (SJ pL) = O.unionList (map atoms pL)
    atoms (EJ pL) = O.unionList (map atoms pL)
    redAtoms p = filter (\ a -> isRedundantAtom a p) (atoms p)
    irrAtoms p = filter (\ a -> not (isRedundantAtom a p)) (atoms p)
    ext p aL = if null aL
               then p
               else CJ [p, DJ (T : (map A aL))]
    infRed p aL =
      let cnf = truthTableToCNF (infRedTruthTable p aL)
      in if cnf == CJ []
         then DJ (T : (map A aL))
         else cnf
    supRed p aL =
      let dnf = truthTableToDNF (supRedTruthTable p aL)
      in if dnf == DJ []
         then CJ (F : (map A aL))
         else dnf
    infElim p aL = infRed p (O.difference (atoms p) (O.olist aL))
    supElim p aL = supRed p (O.difference (atoms p) (O.olist aL))
    toPropForm = id
    fromPropForm = id

-- alternative version of the extension

  ext' :: PropForm a -> O.Olist a -> PropForm a
  ext' p aL = if null aL
              then p
              else DJ [p, CJ (F : (map A aL))]

-- the propositional algebra of truth tables

  instance Ord a => PropAlg a (TruthTable a) where
    at x = plainTruthTable (A x)
    false = plainTruthTable F
    true = plainTruthTable T
    neg (atomL, _, pairL) = (atomL, Nothing, pairL')
      where pairL' = map (\ (bL,b) -> (bL, not b)) pairL
    conj ttL = plainTruthTable (conj (map toPropForm ttL))
    disj ttL = plainTruthTable (disj (map toPropForm ttL))
    subj ttL = plainTruthTable (subj (map toPropForm ttL))
    equij ttL = plainTruthTable (equij (map toPropForm ttL))
    valid (atomL, _, pairL) = Prelude.and (snd (unzip pairL))
    satisfiable (atomL, _, pairL) = Prelude.or (snd (unzip pairL))
    subvalent tt1 tt2 = subvalent (toPropForm tt1) (toPropForm tt2)
    equivalent tt1 tt2 = equivalent (toPropForm tt1) (toPropForm tt2)
    covalent tt1 tt2 = covalent (toPropForm tt1) (toPropForm tt2)
    disvalent tt1 tt2 = disvalent (toPropForm tt1) (toPropForm tt2)
    properSubvalent tt1 tt2 = properSubvalent (toPropForm tt1) (toPropForm tt2)
    properDisvalent tt1 tt2 = properDisvalent (toPropForm tt1) (toPropForm tt2)
    atoms (atomL, _, _) = atomL
    redAtoms tt = redAtoms (toPropForm tt)
    irrAtoms tt = irrAtoms (toPropForm tt)
    ext tt aL = truthTable (ext (toPropForm tt) aL)
    infRed tt aL = truthTable (infRed (toPropForm tt) aL)
    supRed tt aL = truthTable (supRed (toPropForm tt) aL)
    infElim tt aL = truthTable (infElim (toPropForm tt) aL)
    supElim tt aL = truthTable (supElim (toPropForm tt) aL)
    toPropForm = truthTableToDNF
    fromPropForm = truthTable

-- The propositional algebra of boolean values

  newtype Void = Void Void
    deriving (Show, Read, Eq, Ord)

  instance PropAlg Void Bool where
    at _ = undefined
    false = False
    true = True
    neg = not
    conj = and
    disj = or
    subj bL = case bL of
                []         -> True
                [b]        -> True
                (b1:b2:bL) -> (b1 <= b2) && subj (b2:bL)
    equij bL = case bL of
                []         -> True
                [b]        -> True
                (b1:b2:bL) -> (b1 == b2) && equij (b2:bL)
    valid = id
    satisfiable = id
    subvalent = (<=)
    equivalent = (==)
    covalent = (&&)
    disvalent = (/=)
    properSubvalent = (<)
    properDisvalent _ _ = False
    atoms _ = []
    redAtoms _ = []
    irrAtoms  _ = []
    ext b _ = b
    infRed b _ = b
    supRed b _ = b
    infElim b _ = b
    supElim b _ = b
    toPropForm b = if b then T else F
    fromPropForm p = if nullatomic p
                     then boolEval p
                     else error "fromPropForm -- formula is not nullatomic"
