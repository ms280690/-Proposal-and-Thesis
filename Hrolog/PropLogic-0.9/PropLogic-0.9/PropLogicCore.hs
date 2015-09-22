{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

{- |
  This module comprises the abstract definition of two core concepts of propositional logic:

    * The data type @('PropForm' a)@ of /propositional formulas/, based on a given /atom/ type @a@.

    * The two-parameter type class @('PropAlg' a p)@ of a /propositional algebra/, where @a@ is the /atom/ type and @p@ the type of
    /propositions/. Operations of such a structure include a decision if two propositions are 'equivalent', if a given proposition is
    'satisfiable', a converter 'toPropForm' and the inverse 'fromPropForm', which turns a propositional formula into a proposition.
-}

module PropLogicCore (

  -- * Propositional formulas
  PropForm (..),

  -- | A typical example of a propositional formula &#966; in standard mathematical notation is given by
  --
  -- @&#172;(rain &#8743; snow) &#8743; (wet &#8596; (rain &#8744; snow)) &#8743; (rain &#8594; hot) &#8743; (snow &#8594; &#172; hot)@
  --
  -- The primitive elements @hot@, @rain@, @snow@ and @wet@ are the /atoms/ of &#966;.
  -- In Haskell, we define propositional formulas as members of the data type (@PropForm a@), where the type parameter @a@ is the
  -- chosen atom type. A suitable choice for our example would be the atom type @String@ and &#966; becomes a member of
  -- @PropForm String@ type, namely
  --
  -- > CJ [N (CJ [A "rain", A "snow"]), EJ [A "wet", DJ [A "rain", A "snow"]], SJ [A "rain", A "hot"], SJ [A "snow", N (A "hot")]]
  --
  -- This Haskell version is more tedious and we introduce a third notation for nicer output by making @PropForm@ an instance of the
  -- 'Display' type class. A call of @'display' &#966;@ then returns
  --
  -- > [-[rain * snow] * [wet <-> [rain + snow]] * [rain -> hot] * [snow -> -hot]]
  --
  -- The following overview compares the different representations:
  --
  -- >   Haskell            displayed as              kind of formula
  -- >   --------------------------------------------------------------------
  -- >   A x                x   (without quotes)      atomic formula
  -- >   F                  false                     the boolean zero value
  -- >   T                  true                      the boolean unit value
  -- >   N p                -p                        negation
  -- >   CJ [p1,...,pN]     [p1 * ... * pN]           conjunction
  -- >   DJ [p1,...,pN]     [p1 + ... + pN]           disjunction
  -- >   SJ [p1,...,pN]     [p1 -> ... -> pN]         subjunction
  -- >   EJ [p1,...,pN]     [p1 <-> ... <-> pN]       equijunction
  --
  -- Note, that the negation is unary, as usual, but the last four constructors are all multiary junctions, i.e. the list @[p1,...,pN]@
  -- may have any number @N@ of arguments, including @N=0@ and @N=1@.
  --
  -- @PropForm a@ is an instance of @Eq@ and @Ord@, two formulas can be compared for linear order with @<@ or @compare@ and
  -- @PropForm a@ alltogther is linearly ordered, provided that @a@ itself is.
  -- But note, that this order is a pure formal expression order does neither reflect the atomical quasi-order structure
  -- (induced by the @subatomic@ relation; see below) nor the semantical quasi-order structure (induced by @subvalent@).
  -- So this is not the order that reflects the idea of propositional logic.
  -- But we do use it however for the sorting and order of formulas to reduce ambiguities and increase
  -- the efficiency of algorithmes on certain normal forms.
  -- In "DefaultPropLogic" we introduce the normal forms 'OrdPropForm' and the normalizer 'ordPropForm'.
  --
  -- @PropForm a@ is also an instance of @'Read'@ and @'Show'@, so String conversion (and displaying results in the interpreter) are
  -- well defined. For example
  --
  -- > show (CJ [A 3, N (A 7), A 4])  ==  "CJ [A 3,N (A 7),A 4]"
  --
  -- Note, that reading a formula, e.g.
  --
  -- > read "SJ [A 3, A 4, T]"
  --
  -- issues a complaint due to the ambiguity of the atom type. But that can be fixed, e.g. by stating the type explicitely,
  -- as in
  --
  -- > (read "SJ [A 3, A 4, T]") :: PropForm Integer
  --

  -- ** Parsing propositional formulas on string atoms
  stringToProp,

  -- | ... CONTINUEHERE ....

  -- * Propositional algebras
  PropAlg (..),

  -- | @PropAlg a p@ is a structure, made of
  --
  -- @a@ is the /atom/ type
  --
  -- @p@ is the type of /propositions/
  --
  -- @'at' :: a -> p@ is the /atomic proposition/ constructor, similar to the constructor 'A' for atomic formulas.
  --
  -- Similar to the definition of 'PropForm', we have the same set of boolean junctors on propositions:
  -- @'false', 'true' :: p@, @'neg' :: p-> p@ and @'conj', 'disj', 'subj', 'equij' :: [p] -> p@
  --
  -- There the set of
  -- ......................................................................

) where ---------------------------------------------------------------------------------------------------------------

-- imports

  import qualified List        as L
  import qualified TextDisplay as D
  import qualified Olist       as O

-- the PropForm data type

  data PropForm a
    = A a
    | F
    | T
    | N (PropForm a)
    | CJ [PropForm a]
    | DJ [PropForm a]
    | SJ [PropForm a]
    | EJ [PropForm a]
    deriving (Show, Read, Eq)

-- the PropAlg class

  class Ord a => PropAlg a p | p -> a where
  -- the signature
    -- atomic proposition constructor
    at :: a -> p
    -- boolean junctors
    false :: p
    true :: p
    neg :: p -> p
    conj :: [p] -> p
    disj :: [p] -> p
    subj :: [p] -> p
    equij :: [p] -> p
    -- semantic properties and relations
    valid :: p -> Bool
    satisfiable :: p -> Bool
    contradictory :: p -> Bool
    subvalent :: p -> p -> Bool
    equivalent :: p -> p -> Bool
    covalent :: p -> p -> Bool
    disvalent :: p -> p -> Bool
    properSubvalent :: p -> p -> Bool
    properDisvalent :: p -> p -> Bool
    -- atom sets
    atoms :: p -> O.Olist a
    redAtoms :: p -> O.Olist a
    irrAtoms :: p -> O.Olist a
    -- atomic properties and relations
    nullatomic :: p -> Bool
    subatomic :: p -> p -> Bool
    equiatomic :: p -> p -> Bool
    coatomic :: p -> p -> Bool
    disatomic :: p -> p -> Bool
    properSubatomic :: p -> p -> Bool
    properDisatomic :: p -> p -> Bool
    -- atomic modifiers
    ext :: p -> [a] -> p
    infRed :: p -> [a] -> p
    supRed :: p -> [a] -> p
    infElim :: p -> [a] -> p
    supElim :: p -> [a] -> p
    -- biequivalence
    biequivalent :: p -> p -> Bool
    -- meta properties
    pointwise :: (p -> Bool) -> [p] -> Bool
    pairwise :: (p -> p -> Bool) -> [p] -> Bool
    -- formula conversions
    toPropForm :: p -> PropForm a
    fromPropForm :: PropForm a -> p
  -- some implementations
    pointwise = all
    pairwise property pL =
       case pL of
         []     -> True
         [x]    -> True
         (x:xL) -> (all (property x) xL)  && (pairwise property xL)
    contradictory p = not (satisfiable p)
    satisfiable p = not (contradictory p)
    covalent p1 p2 = not (disvalent p1 p2)
    disvalent p1 p2 = not (covalent p1 p2)
    nullatomic p = O.isEmpty (atoms p)
    subatomic p1 p2 = O.included (atoms p1) (atoms p2)
    equiatomic p1 p2 = O.equal (atoms p1) (atoms p2)
    coatomic p1 p2 = not (disatomic p1 p2)
    disatomic p1 p2 = O.disjunct (atoms p1) (atoms p2)
    properSubatomic p1 p2 = O.properlyIncluded (atoms p1) (atoms p2)
    properDisatomic p1 p2 = O.disjunct aL1 aL2 && not (O.isEmpty aL1) && not (O.isEmpty aL2)
      where aL1 = atoms p1
            aL2 = atoms p2
    biequivalent p1 p2 = equiatomic p1 p2 && equivalent p1 p2

-- displaying formulas

  instance D.Display a => D.Display (PropForm a) where
    textFrame form =
      let falseSymbol = "false"
          trueSymbol  = "true"
          negSymbol   = "-"
          conjSymbol  = "*"
          disjSymbol  = "+"
          subjSymbol  = "->"
          equijSymbol = "<->"
          multiJuncTextFrame symb pL = case pL of
            []  -> ["[" ++ symb ++ "]"]
            [p] -> D.textFrameBracket (D.plainMerge (D.normalTextFrameTable
                     [[[symb ++ " "], D.textFrame p]]))
            pL  -> D.textFrameBracket (D.plainMerge (D.normalTextFrameTable
                     [(L.intersperse [" " ++ symb ++ " "] (map D.textFrame pL))]))
      in case form of
        A x    -> D.textFrame x
        F      -> [falseSymbol]
        T      -> [trueSymbol]
        N p    -> D.plainMerge (D.normalTextFrameTable [[[negSymbol], D.textFrame p]])
        CJ pL -> multiJuncTextFrame conjSymbol pL
        DJ pL -> multiJuncTextFrame disjSymbol pL
        SJ pL -> multiJuncTextFrame subjSymbol pL
        EJ pL -> multiJuncTextFrame equijSymbol pL

-- parsing formulas on string atoms

  type Parser a = String -> [(a, String)]

  parseProp :: Parser (PropForm String)
  parseProp = parseAtom `plus` parseFalse `plus` parseTrue `plus` parseNeg `plus`
              parseConj `plus` parseDisj  `plus` parseSubj `plus` parseEquij
    where
      -- eight functions of type Parser (PropForm String)
      parseAtom = \ inp -> [ (A out',inp') | (out',inp') <- (skipWhite atomToken) inp ]
      parseFalse = \ inp -> [ (F, inp') | (out', inp') <- (skipWhite falseToken) inp ]
      parseTrue = \ inp -> [ (T, inp') | (out', inp') <- (skipWhite trueToken) inp ]
      parseNeg = \ inp -> [ (N out2, inp2) | (out1, inp1) <- (skipWhite negToken) inp,
                                             (out2, inp2) <- (skipWhite parseProp) inp1 ]
      parseConj = parseJunction CJ conjToken
      parseDisj = parseJunction DJ disjToken
      parseSubj = parseJunction SJ subjToken
      parseEquij = parseJunction EJ equijToken
      -- auxiliary function
      parseJunction :: ([PropForm String] -> PropForm String) -> Parser String -> Parser (PropForm String)
      parseJunction junc token = \ inp ->
        [ (junc [], i3) | (o1,i1) <- (skipWhite leftToken) inp,
                          (o2,i2) <- (skipWhite token) i1,
                          (o3,i3) <- (skipWhite rightToken) i2 ]
        ++
        [ (junc [o3], i4) | (o1,i1) <- (skipWhite leftToken) inp,
                            (o2,i2) <- (skipWhite token) i1,
                            (o3,i3) <- (skipWhite parseProp) i2,
                            (o4,i4) <- (skipWhite rightToken) i3 ]
        ++
        [ (junc (o2:o4:o5), i6) | (o1,i1) <- (skipWhite leftToken) inp,
                                  (o2,i2) <- (skipWhite parseProp) i1,
                                  (o3,i3) <- (skipWhite token) i2,
                                  (o4,i4) <- (skipWhite parseProp) i3,
                                  (o5,i5) <- tokenPropSeq i4,
                                  (o6,i6) <- (skipWhite rightToken) i5]
          where tokenPropSeq :: Parser [PropForm String]
                tokenPropSeq = \ inp -> case (skipWhite token) inp of
                  []         -> [([],inp)]
                  [(t,inp')] -> [ (o1:o2, i2) | (o1,i1) <- (skipWhite parseProp) inp',
                                                (o2,i2) <- (skipWhite tokenPropSeq) i1 ]
      -- the token parser of type Parser String
      atomToken = \ inp -> [ (out',inp') | (out',inp') <- (grab1 isAlphanum) inp,
                                           out' /= "false", out' /= "true" ]
      leftToken  = string "["         :: Parser String
      rightToken = string "]"         :: Parser String
      falseToken = string "false"     :: Parser String
      trueToken  = string "true"      :: Parser String
      negToken   = string "-"         :: Parser String
      conjToken  = string "*"         :: Parser String
      disjToken  = string "+"         :: Parser String
      subjToken  = string "->"        :: Parser String
      equijToken = string "<->"       :: Parser String
      -- the parser combinators
      string :: String -> Parser String
      string s = \ inp -> let (s1,s2,s3) = iter (s,inp,[])
                          -- iter(s,inp,[]) = (s1,s2,s3) so that
                          -- s1++s2=s and s1++s3=inp with s1 of maximal length
                          in if null s2
                             then [(s1,s3)]
                             else []
        where iter ([],y,z) = (reverse z,[],y)
              iter (x,[],z) = (reverse z,x,[])
              iter (x:xL,y:yL,z) = if x == y
                                   then iter (xL, yL, x:z)
                                   else (reverse z, x:xL, y:yL)
      grab :: (Char -> Bool) -> Parser String
      grab chf = \ inp -> iter ("",inp)
        where iter (xL, [])   = [(reverse xL, [])]
              iter (xL, y:yL) = if chf y
                                then iter (y:xL, yL)
                                else [(reverse xL, y:yL)]
      grab1 :: (Char -> Bool) -> Parser String
      grab1 chf = \ inp -> filter (\ (x,y) -> not (null x)) (grab chf inp)
      plus :: Parser a -> Parser a -> Parser a
      p `plus` q = \ inp -> (p inp ++ q inp)
      -- some character predicates of type Char -> Bool
      isDigit c = '0' <= c && c <= '9'
      isLower c = 'a' <= c && c <= 'z'
      isUpper c = 'A' <= c && c <= 'Z'
      isLetter c = isLower c || isUpper c
      isAlphanum c = isDigit c || isLetter c
      isWhite c = c == ' '   -- space char
               || c == '\n'  -- newline
               || c == '\t'  -- horizontal tab
               || c == '\v'  -- vertical tab
               || c == '\f'  -- form feed
               || c == '\r'  -- carriage return
      -- white space
      whiteSpace :: Parser String
      whiteSpace = grab isWhite
      skipWhite :: Parser a -> Parser a
      skipWhite p = \ inp -> [ (out1, inp1) | (out0, inp0) <- whiteSpace inp, (out1, inp1) <- p inp0 ]

  stringToProp :: String -> PropForm String
  stringToProp inp = case parseProp inp of
    []            -> error ("stringToProp -- input is not a proper fancy formula:\n" ++ inp)
    [(form,inp')] -> form
    _             -> error ("stringToProp -- Fatal error! Input doesn't parse unambiguously:\n" ++ inp)

