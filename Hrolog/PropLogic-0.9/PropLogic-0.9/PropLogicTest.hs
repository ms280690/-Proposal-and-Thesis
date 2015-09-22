module PropLogicTest (

  -- * Convenient abbreviations and combinations of often used functions

  -- ** Various prime form generations
  pdnf', pcnf',
  spdnf', spcnf',
  xpdnf', xpcnf',

  -- ** Various prime form generations with combined parsing and display
  pdnf, pcnf,
  spdnf, spcnf,
  xpdnf, xpcnf,

  -- * Random generation

  -- ** Auxiliary random functions (probably obsolete)

  randomListMember,
  -- | For example,
  --
  -- > > randomListMember ['a','b','c','d']
  -- > 'b'
  -- > > randomListMember ['a','b','c','d']
  -- > 'd'
  -- > > randomListMember ['a','b','c','d']
  -- > 'c'

  randomChoice,
  -- | Removes one random member from a given list and returns this member and the remaining list. For example,
  --
  -- > > randomChoice ["a", "b", "c"]
  -- > ("c",["a","b"])
  -- > > randomChoice ["a", "b", "c"]
  -- > ("b",["a","c"])
  -- > > randomChoice ["a", "b", "c"]
  -- > ("a",["b","c"])
  -- > > randomChoice ["a", "b", "c"]
  -- > ("a",["b","c"])

  shuffle,
  -- | For example,
  --
  -- > > shuffle [1,2,3,2,1]
  -- > [2,1,3,1,2]
  -- > > shuffle [1,2,3,2,1]
  -- > [1,2,3,2,1]
  -- > > shuffle [1,2,3,2,1]
  -- > [2,1,2,3,1]
  -- > > shuffle [1,2,3,2,1]
  -- > [2,3,1,1,2]
  -- > > shuffle [1,2,3,2,1]
  -- > [2,1,1,3,2]
  -- > > shuffle [1,2,3,2,1]
  -- > [1,2,1,3,2]

  randomSublist,
  -- | @randomSublist xL@ deletes a random number of members and returns the result. For example,
  --
  -- > > randomSublist [1,2,3,4,5,6,7,7,7,8,8,8]
  -- > [4]
  -- > > randomSublist [1,2,3,4,5,6,7,7,7,8,8,8]
  -- > [3,4]
  -- > > randomSublist [1,2,3,4,5,6,7,7,7,8,8,8]
  -- > [2,3,4,5,6,7,7,7,8,8]
  -- > > randomSublist [1,2,3,4,5,6,7,7,7,8,8,8]
  -- > [1,2,5,7,8]
  -- > > randomSublist [1,2,3,4,5,6,7,7,7,8,8,8]
  -- > [4,5,8,8]
  -- > > randomSublist [1,2,3,4,5,6,7,7,7,8,8,8]
  -- > [1,2,3,5,6,7,7,8,8]

  nRandomRIO,
  -- | @nRandomRIO n (lower,upper)@ returns an list of n random integers between lower and upper.
  -- For example,
  --
  -- > > nRandomRIO 10 (1,3)
  -- > [2,3,3,2,2,3,3,3,3,1]
  -- > > nRandomRIO 10 (1,3)
  -- > [3,2,1,2,2,3,3,1,1,1]
  -- > > nRandomRIO 10 (1,3)
  -- > [1,3,2,1,2,1,2,2,1,2]

  weightedRandomMember,
  -- | @weightedRandomMember@ takes a list @[(x1,n1),...,(xM,nM)]@ of @(a,Int)@ pairs and selects one of the @x1,...,xM@.
  -- The second integer value @ni@ in each pair @(xi,ni)@ represents the relative likelihood for the choice of @xi@ compare to the
  -- other values. In the following example and on the long run, @a@ is chosen 10 times more often than @b@, and @c@ isn't chosen at all.
  --
  -- > > weightedRandomMember [("a",10),("b",1),("c",0)]
  -- > "a"
  -- > > weightedRandomMember [("a",10),("b",1),("c",0)]
  -- > "a"
  -- > > weightedRandomMember [("a",10),("b",1),("c",0)]
  -- > "a"
  -- > > weightedRandomMember [("a",10),("b",1),("c",0)]
  -- > "a"
  -- > > weightedRandomMember [("a",10),("b",1),("c",0)]
  -- > "a"
  -- > > weightedRandomMember [("a",10),("b",1),("c",0)]
  -- > "a"
  -- > > weightedRandomMember [("a",10),("b",1),("c",0)]
  -- > "a"
  -- > > weightedRandomMember [("a",10),("b",1),("c",0)]
  -- > "b"
  -- > > weightedRandomMember [("a",10),("b",1),("c",0)]
  -- > "a"

  appleBasketDistribution,
  -- | @appleBasketDistribution n k@ distributes @n@ apples into @k@ baskets in the sense that the result is a random integer list
  -- of length @k@, where the sum of its components is @n@.
  --
  -- > > appleBasketDistribution 10 4
  -- > [4,2,2,2]
  -- > > appleBasketDistribution 10 4
  -- > [2,2,3,3]
  -- > > appleBasketDistribution 10 4
  -- > [3,2,1,4]
  -- > > appleBasketDistribution 10 4
  -- > [0,7,0,3]

  -- ** Random I-Forms and X-Forms

  averageLineLength,
  randomILine,
  randomIForm,
  randomXForm,

  -- ** Random DNFs and CNFs

  randomDNF,
  randomCNF,
  randomCharDNF, randomCharCNF,
  randomIntDNF, randomIntCNF,

  -- ** Random propositional formulas in general

  -- *** Size parameter

  SizeTriple,

  sizeTriple,

  -- *** Random formulas

  JunctorSymbol(..),

  JunctorWeighting,

  defaultJunctorWeighting,

  weightedRandomPropForm,

  randomPropForm,
  -- | @randomPropForm aL (aSize,jSize)@ returns a randomly generated propositional formula @p@ such that
  --
  -- >     atoms p    == aL
  -- >     atomSize p == aSize
  -- >     juncSize p == jSize
  --
  -- If @atomSize@ is smaller than the length of @aL@, an error message is returned. For example,
  --
  -- > > randomPropForm ["x", "y"] (3,3)
  -- > EJ [A "y",T,A "y",A "x",DJ []]
  -- > > randomPropForm ["x", "y"] (3,3)
  -- > EJ [SJ [A "y",A "y"],DJ [A "x"]]
  -- > > randomPropForm ["x", "y"] (3,3)
  -- > EJ [EJ [],A "x",CJ [A "y"],A "x"]
  -- > > randomPropForm ['x','y','z'] (1,1)
  -- > *** Exception: randomPropForm -- atom size smaller than ordered atom list

  randomCharProp,
  -- | @randomCharProp (aCard,aSize,jSize)@ is @randomPropForm aL (aSize,jSize)@, where @aL@ is the ordered list of the first @aCard@
  -- small characters @'a', 'b', 'c', ...@. If the atom cardinality is undefined, i.e. if not @0<=aCard<=26@, or if it exceeds @aSize@,
  -- an error message is returned.
  -- For example,
  --
  -- > > randomCharProp (3,5,5)
  -- > SJ [A 'c',EJ [A 'a',A 'c'],F,T,DJ [A 'b'],A 'c']
  -- > > randomCharProp (3,5,5)
  -- > SJ [A 'a',A 'c',DJ [T,CJ [A 'b',A 'a',A 'b'],T]]
  -- > > randomCharProp (3,5,5)
  -- > DJ [F,A 'b',SJ [F],A 'c',DJ [A 'a'],A 'c',A 'a']

  randomIntProp,

  -- * Testing

  -- ** Testing a propositional algebra

  -- *** Axioms of propositional algebras
  axiom_reflexivity_of_subvalence,
  axiom_transitivity_of_subvalence,
  axiom_criterion_for_equivalence,
  -- | CONTINUEHERE ..............................................................!!

  -- *** The summarized test function for a propositional algebra
  test_prop_alg,

  -- ** Tests for "DefaultPropLogic"

  -- *** Testing the normalizations

  -- | ...continuehere...............................

  -- *** Testing the default propositional algebras

  -- ** Tests for "FastPropLogic"

  -- *** Testing of the M- and P-procedure
  -- | continuehere ............................

  -- *** Correctness of the Prime Normal Form constructions
  -- | continuehere ...........................

  -- ** Test for the total package
  total_test,

  -- * Profiling - first version

  -- ** Measures

  Msec,
  CanonPerformance,
  Verbose,

  -- ** correctness tests (move these correctness tests)--!!!!!!!!!!1

  pnfCorrect,
  pnfCorrectRepeat,

  -- ** performance tests

  pnfPerform,
  pnfPerformRandom,
  pnfPerformRepeat,

  -- * Profiling - second version
  Seconds,
  verboseRandomPrimeTest,
  verboseRandomPrimeTesting,
  meanValue,
  standDeviation,
  normSeconds,

) where ---------------------------------------------------------------------------------------------------------------

  import Char (chr)
  import Ratio
  import Random (randomIO, randomRIO)
  import Control.Monad
  import CPUTime (getCPUTime)
  import qualified Time  as T
  import qualified Olist as O
  import qualified TextDisplay as D
  import qualified Costack as C
  import PropLogicCore
  import DefaultPropLogic
  import FastPropLogic

-- convenient abbreviations

  pdnf' :: Ord a => PropForm a -> PDNF a
  pdnf' = fromXPDNF . toXPDNF

  pcnf' :: Ord a => PropForm a -> PCNF a
  pcnf' = fromXPCNF . toXPCNF

  spdnf' :: Ord a => PropForm a -> SimpleDNF a
  spdnf' = simpleDNF . pdnf'

  spcnf' :: Ord a => PropForm a -> SimpleCNF a
  spcnf' = simpleCNF . pcnf'

  xpdnf' :: Ord a => PropForm a -> XPDNF a               -- this should go to the FastPropLogic module!!!!!!!!!!!!!!!!
  xpdnf' = toXPDNF

  xpcnf' :: Ord a => PropForm a -> XPCNF a               -- this should go to the FastPropLogic module!!!!!!!!!!!!!!!!
  xpcnf' = toXPCNF

  pdnf :: String -> IO ()
  pdnf = D.display . pdnf' . stringToProp

  pcnf :: String -> IO ()
  pcnf = D.display . pcnf' . stringToProp

  spdnf :: String -> IO ()
  spdnf = D.display . spdnf' . stringToProp

  spcnf :: String -> IO ()
  spcnf = D.display . spcnf' . stringToProp

  xpdnf :: String -> IO ()
  xpdnf = D.display . xpdnf' . stringToProp

  xpcnf :: String -> IO ()
  xpcnf = D.display . xpcnf' . stringToProp

-- auxiliary random functions

  randomListMember :: [a] -> IO a
  randomListMember xL = do i <- randomRIO (0, length xL - 1)
                           return (xL !! i)

  randomChoice :: [a] -> IO (a, [a])
  randomChoice [] = error "randomChoice -- empty list"
  randomChoice xL = do i <- randomRIO (0, length xL -1)
                       let (yL, z:zL) = splitAt i xL
                       return (z, yL ++ zL)

  nRandomListMember :: Int -> [a] -> IO [a]
  nRandomListMember n xL = mapM randomListMember (replicate n xL)

  randomInsert :: a -> [a] -> IO [a]
  randomInsert x xL = do i <- randomRIO (0, length xL)
                         let (yL,zL) = splitAt i xL
                         return (yL ++ [x] ++ zL)

  shuffle :: [a] -> IO [a]
  shuffle xL = if null xL
               then return []
               else do (y,yL) <- randomChoice xL
                       zL <- shuffle yL
                       return (y:zL)

  randomSublist :: [a] -> IO [a]
  randomSublist xL = do let n = length xL -1
                        k <- randomRIO (0, n)
                        iL <- shuffle [1..n]
                        let jL = O.olist (take k iL)
                        let yL = filtering xL jL 1
                        return yL
    where filtering xL [] _ = []
          filtering (x:xL) (j:jL) i = if j == i
                                      then x : (filtering xL jL (i + 1))
                                      else filtering xL (j:jL) (i + 1)

  nRandomRIO :: Int -> (Int,Int) -> IO [Int]
  nRandomRIO n (lower,upper) = sequence (replicate n (randomRIO (lower,upper)))

  weightedRandomMember :: [(a,Int)] -> IO a
  weightedRandomMember pairL = do i <- randomRIO (1, sum (map snd pairL))
                                  return (select pairL i)
    where select ((x,k):pL) i = if i <= k
                                then x
                                else select pL (i - k)

  appleBasketDistribution :: Int -> Int -> IO [Int]
  appleBasketDistribution apples baskets =
    if apples < 0 || baskets < 0
    then error "appleBasketDistribution -- negative arguments"
    else if baskets == 0
         then if apples == 0
              then return []
              else error "appleBasketDistribution -- zero baskets"
         else do basketIndexList <- nRandomRIO apples (0, baskets - 1)
                 return (map (\ i -> count i basketIndexList) (Prelude.take baskets (enumFrom 0)))
    where count x yL = length (filter (\ y -> y == x) yL)

-- random I-Forms and X-Forms

  averageLineLength :: IForm -> Float
  averageLineLength iform = (fromIntegral (truncate ((v / l) * 100))) / 100
    where v = fromIntegral (volume iform) :: Float
          l = fromIntegral (C.length iform) :: Float

  randomILine :: Int -> Int -> IO ILine
  randomILine atomNum averLen =
    do intLL <- mapM singleAtom [1..atomNum]
       let intL = concat intLL
       return (iLine intL)
    where singleAtom :: IAtom -> IO [ILit]
          singleAtom atom = do i <- randomRIO (1, atomNum)
                               b <- randomIO :: IO Bool
                               let lit = if i <= averLen
                                         then if b then [atom] else [negLit atom]
                                         else []
                               return lit

  randomIForm :: Int -> Int -> Int -> IO IForm

--  randomIForm atomNum formLen averLineLen =
--    do lineL <- sequence (replicate formLen (randomILine atomNum averLineLen))
--       return (C.fromList lineL)

  randomIForm atomNum formLen averLineLen
    | atomNum < 0 = error "randomIForm -- negative number of atoms"
    | formLen < 0 = error "randomIForm -- negative I-Form length"
    | averLineLen < 0 = error "randomIForm -- negative average I-Line length"
    | atomNum < averLineLen = error "randomIForm -- atom number smaller than average I-Line length"
    | otherwise   =  do lineL <- sequence (replicate formLen (randomILine atomNum averLineLen))
                        return (C.fromList lineL)

  randomXForm :: Ord a => [a] -> Int -> Int -> IO (XForm a)
  randomXForm atomL formLen averLineLen =
    do let ordAtomL = O.olist atomL
       let atomNum = length ordAtomL
       iform <- randomIForm atomNum formLen averLineLen
       return (ordAtomL, iform)

-- random DNFs and CNFs

  randomDNF :: Ord a => [a] -> Int -> Int -> IO (DNF a)
  randomDNF atomL formLen averLineLen = do xform <- randomXForm atomL formLen averLineLen
                                           return (xDNF xform)

  randomCNF :: Ord a => [a] -> Int -> Int -> IO (CNF a)
  randomCNF atomL formLen averLineLen = do xform <- randomXForm atomL formLen averLineLen
                                           return (xCNF xform)

  randomCharDNF :: Int -> IO (DNF Char)
  randomCharDNF atomNum = randomDNF (take atomNum ['a'..'z']) (2 * atomNum) atomNum

  randomCharCNF :: Int -> IO (CNF Char)
  randomCharCNF atomNum = randomCNF (take atomNum ['a'..'z']) (2 * atomNum) atomNum

  randomIntDNF :: Int -> IO (DNF Int)
  randomIntDNF atomNum = randomDNF (take atomNum [1..]) (2 * atomNum) atomNum

  randomIntCNF :: Int -> IO (CNF Int)
  randomIntCNF atomNum = randomCNF (take atomNum [1..]) (2 * atomNum) atomNum

-- size parameters

  type SizeTriple = (Int,Int,Int) -- = (length atoms, atomSize, juncSize)

  sizeTriple :: Ord a => PropForm a -> SizeTriple
  sizeTriple p = (Prelude.length (atoms p), atomSize p, juncSize p)

-- random formula generation with weighted junctors

  data JunctorSymbol = T_ | F_ | N_ | CJ_ | DJ_ | SJ_ | EJ_
    deriving (Eq, Ord, Show, Read)

  type JunctorWeighting = [(JunctorSymbol, Int)]

  defaultJunctorWeighting :: JunctorWeighting
  defaultJunctorWeighting = [(N_,15), (F_,1), (T_,1), (CJ_,5), (DJ_,5), (SJ_,1), (EJ_,1)]

  weightedRandomPropForm :: JunctorWeighting -> O.Olist a -> (Int, Int) -> IO (PropForm a)
  weightedRandomPropForm weighting ordAtomL (aSize,jSize)
    | aSize < 0 || jSize < 0            = error "weightedRandomPropForm -- negative size"
    | aSize == 0 && jSize == 0          = error "weightedRandomPropForm -- zero atom and junctor size"
    | aSize > 0 && length ordAtomL == 0 = error "weightedRandomPropForm -- no atoms available"
    | aSize < length ordAtomL           = error "weightedRandomPropForm -- atom size smaller than ordered atom list"
    | otherwise = do aL <- nRandomListMember (aSize - (length ordAtomL)) ordAtomL
                     aL <- shuffle (aL ++ ordAtomL)
                     p <- iter jSize (map A aL)
                     return p
    where iter :: Int -> [PropForm a] -> IO (PropForm a)
          iter jSize pL | jSize == 0 = if length pL == 1
                                       then return (head pL)
                                       else error "weightedRandomPropForm -- junctor size"
                        | jSize == 1 = do x <- if length pL == 0
                                               then randomJunc [CJ_, DJ_, SJ_, EJ_, F_, T_]
                                               else randomJunc [CJ_, DJ_, SJ_, EJ_]
                                          p <- case x of
                                                  F_  -> return F
                                                  T_  -> return T
                                                  CJ_ -> return (CJ pL)
                                                  DJ_ -> return (DJ pL)
                                                  SJ_ -> return (SJ pL)
                                                  EJ_ -> return (EJ pL)
                                          return p
                        | jSize >= 2 = do x <- if length pL == 0
                                               then randomJunc [F_, T_, CJ_, DJ_, SJ_, EJ_]
                                               else randomJunc [F_, T_, CJ_, DJ_, SJ_, EJ_, N_]
                                          qL <- case x of
                                                  F_ -> do let pL' = F : pL
                                                           pL'' <- shuffle pL'
                                                           return pL'
                                                  T_ -> do let pL' = T : pL
                                                           pL'' <- shuffle pL'
                                                           return pL'
                                                  N_ -> do (p',pL') <- randomChoice pL
                                                           pL'' <- shuffle ((N p') : pL')
                                                           return pL''
                                                  _  -> do i <- randomRIO (0, length pL - 1)
                                                           let (pL1, pL2) = splitAt i pL
                                                           let q = case x of
                                                                     CJ_ -> (CJ pL1)
                                                                     DJ_ -> (DJ pL1)
                                                                     SJ_ -> (SJ pL1)
                                                                     EJ_ -> (EJ pL1)
                                                           pL' <- shuffle (q : pL2)
                                                           return pL'
                                          p <- iter (jSize - 1) qL
                                          return p
          randomJunc :: [JunctorSymbol] -> IO JunctorSymbol
          randomJunc selection = weightedRandomMember (select selection weighting)
             where select [] _ = []
                   select (x:xL) pL = (select1 x pL) : (select xL pL)
                   select1 x ((y,n):pL) = if x == y
                                          then (y,n)
                                          else select1 x pL

  randomPropForm :: O.Olist a -> (Int,Int) -> IO (PropForm a)
  randomPropForm ordAtomL (aSize,jSize)
    | aSize < 0 || jSize < 0            = error "randomPropForm -- negative size"
    | aSize == 0 && jSize == 0          = error "randomPropForm -- zero atom and junctor size"
    | aSize > 0 && length ordAtomL == 0 = error "randomPropForm -- no atoms available"
    | aSize < length ordAtomL           = error "randomPropForm -- atom size smaller than ordered atom list"
    | otherwise = do aL <- nRandomListMember (aSize - (length ordAtomL)) ordAtomL
                     aL <- shuffle (aL ++ ordAtomL)
                     p  <- weightedRandomPropForm defaultJunctorWeighting aL (aSize,jSize)
                     return p

  randomCharProp :: SizeTriple -> IO (PropForm Char)
  randomCharProp (aCard,aSize,jSize) =
    if aCard < 0 || aCard > 26
    then error ("randomCharProp -- " ++ (show aCard) ++ " exceeds the number of available 26 characters")
    else do let aL = take aCard charList
            p <- randomPropForm aL (aSize,jSize)
            return p
    where charList = map chr [97..122] -- all small characters 'a', ..., 'z'

  randomIntProp :: SizeTriple -> IO (PropForm Int)
  randomIntProp (aCard,aSize,jSize) = do let aL = Prelude.take aCard [1..]
                                         p <- randomPropForm aL (aSize,jSize)
                                         return p

-- axioms of propositional algebras

  -- the conversion axioms

{--
  axiom_atom_conversion :: PropAlg a p => a -> Bool
  axiom_atom_conversion a = undefined

  -- ambiguous constraint error: -----!!!!!!!!!!!!!
  -- axiom_zero_conversion :: PropAlg a p => Bool
  -- axiom_zero_conversion = fromPropForm F `biequivalent` false

  -- ambiguous constraint error: -----!!!!!!!!!!!!!
  -- axiom_unit_conversion :: PropAlg a p => Bool
  -- axiom_unit_conversion = fromPropForm T `biequivalent` true

  axiom_neg_conversion :: PropAlg a p => PropForm a -> Bool
  axiom_neg_conversion p = fromPropForm (N p) `biequivalent` neg (fromPropForm p)

  ...............etc......................
-}

  -- semantic axioms

  axiom_reflexivity_of_subvalence :: PropAlg a p => p -> Bool
  axiom_reflexivity_of_subvalence p = p `subvalent` p

  axiom_transitivity_of_subvalence :: PropAlg a p => (p,p,p) -> Bool
  axiom_transitivity_of_subvalence (p1,p2,p3) = ((p1 `subvalent` p2) && (p2 `subvalent` p3)) `subvalent` (p1 `subvalent` p3)

  axiom_criterion_for_equivalence :: PropAlg a p => (p,p) -> Bool
  axiom_criterion_for_equivalence (p1,p2) = (p1 `equivalent` p2) `equivalent` ((p1 `subvalent` p2) && (p2 `subvalent` p1))

  axiom_least_element :: PropAlg a p => p -> Bool
  axiom_least_element p = false `subvalent` p

  axiom_greatest_element :: PropAlg a p => p -> Bool
  axiom_greatest_element p = p `subvalent` true

  axiom_neutral_disjunction_element :: PropAlg a p => p -> Bool
  axiom_neutral_disjunction_element p = disj [false, p] `equivalent` p

  axiom_neutral_conjunction_element :: PropAlg a p => p -> Bool
  axiom_neutral_conjunction_element p = conj [true, p] `equivalent` p

  axiom_conjunctive_complement :: PropAlg a p => p -> Bool
  axiom_conjunctive_complement p = conj [p, neg p] `equivalent` false

  axiom_disjunctive_complement :: PropAlg a p => p -> Bool
  axiom_disjunctive_complement p = disj [p, neg p] `equivalent` true

  axiom_conjunctive_idempotency :: PropAlg a p => p -> Bool
  axiom_conjunctive_idempotency p = conj [p, p] `equivalent` p

  axiom_disjunctive_idempotency :: PropAlg a p => p -> Bool
  axiom_disjunctive_idempotency p = disj [p, p] `equivalent` p

  -- CONTINUEHERE ----------------!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

-- test functions for propositional algebras

  test_prop_alg :: (Show a, Show p, PropAlg a p) => IO a -> IO p -> Int -> IO ()
  test_prop_alg randAtom randProp number =
    do -- generation of random arguments
       atomL <- replicateM number randAtom
       propL <- replicateM number randProp
       propL' <- replicateM number randProp
       propL'' <- replicateM number randProp
       let propPairL = zip propL propL'
       let propTripleL = zip3 propL propL' propL''
       -- test the single axioms
       test_axiom "reflexivity of the subvalence relation"  axiom_reflexivity_of_subvalence propL
       test_axiom "transitivity of the subvalence relation" axiom_transitivity_of_subvalence propTripleL
       test_axiom "criterion for equivalence"               axiom_criterion_for_equivalence propPairL
       -- CONTINUEHERE ...........................
       -- finish
       print "Test for propositional algebra finished successfully."
    where test_axiom :: Show a => String -> (a -> Bool) -> [a] -> IO ()
          test_axiom name axiom [] = print ("test for " ++ name ++ " passed " ++ show number ++ " times")
          test_axiom name axiom (arg:argL) = if axiom arg
                                             then test_axiom name axiom argL
                                             else error (name ++ " doesn't hold for " ++ "\n" ++ show arg)

-- test for the total package

  total_test :: Int -> IO ()
  total_test = undefined

-- measures and test parameters

  type Msec = Integer -- milliseconds

  timeDiffToMsec :: T.TimeDiff -> Msec
  timeDiffToMsec td =
    if T.tdMonth td /= 0 || T.tdYear td /= 0
    then error "timeDiffToMsec -- cannot deal with non-zero months or years"
    else (div (T.tdPicosec td) 1000000000) +
         (toInteger (T.tdSec td)  * 1000) +
         (toInteger (T.tdMin td)  * 1000 * 60) +
         (toInteger (T.tdHour td) * 1000 * 60 * 60) +
         (toInteger (T.tdDay td)  * 1000 * 60 * 60 * 24)

  diffTimes :: T.ClockTime -> T.ClockTime -> Msec
  diffTimes t1 t2 = timeDiffToMsec (T.diffClockTimes t1 t2)

  type CanonPerformance = (SizeTriple,(Msec,SizeTriple),(Msec,SizeTriple)) -- (pre-size, (D-time, D-size), (C-time, C-size))

  type Verbose = Bool  -- set True means that the according function produces additional information while in progress

-- correctness tests

  pnfCorrect :: (D.Display a, Show a, Ord a) => Verbose -> PropForm a -> IO (Either (PropForm a) CanonPerformance)
  pnfCorrect verb p =
    do return (ifVerbose (do { print "The input formula:" ; D.display p }))
       t1 <- T.getClockTime
       let fastPdnf = pdnf' $! p
       t2 <- T.getClockTime
       let fastPdnfTime = diffTimes t2 t1
       return (ifVerbose (do { print ("The fast PDNF (computed in " ++ (show fastPdnfTime) ++ " milliseconds):") ; D.display fastPdnf }))
       t1 <- T.getClockTime
       let defaultPdnf = primeDNF $! p
       t2 <- T.getClockTime
       let defaultPdnfTime = diffTimes t2 t1
       return (ifVerbose (do { print ("The default PDNF (computed in " ++ (show defaultPdnfTime) ++ " milliseconds):") ; D.display defaultPdnf }))
       t1 <- T.getClockTime
       let fastPcnf = pcnf' $! p
       t2 <- T.getClockTime
       let fastPcnfTime = diffTimes t2 t1
       return (ifVerbose (do { print ("The fast PCNF (computed in " ++ (show fastPcnfTime) ++ " milliseconds):") ; D.display fastPcnf }))
       t1 <- T.getClockTime
       let defaultPcnf = primeCNF $! p
       t2 <- T.getClockTime
       let defaultPcnfTime = diffTimes t2 t1
       return (ifVerbose (do { print ("The default PCNF (computed in " ++ (show defaultPcnfTime) ++ " milliseconds):") ; D.display defaultPcnf }))
       let result = if defaultPdnf == fastPdnf && defaultPcnf == fastPcnf
                    then Right (sizeTriple p, (fastPdnfTime, sizeTriple fastPdnf), (fastPcnfTime, sizeTriple fastPcnf))
                    else Left p
       return (ifVerbose (do { print "The overall result is:" ; print result }))
       return result
    where ifVerbose act = if verb then act else (putStr "")

  pnfCorrectRepeat :: Verbose -> (Int, Int, Int) -> IO (PropForm Char)
  pnfCorrectRepeat verb (aCard, aSize, jSize) =
    do p <- randomCharProp (aCard, aSize, jSize)
       r <- pnfCorrect verb p
       q <- case r of
              Left p' -> return p'
              Right _ -> do print r
                            pnfCorrectRepeat verb (aCard, aSize, jSize)
       return q

-- performance tests

  pnfPerform :: (D.Display a, Show a, Ord a) => PropForm a -> IO CanonPerformance
  pnfPerform p =
    do let formSize = sizeTriple p
       t1 <- T.getClockTime
       let pdnfForm = pdnf' $! p
       let pdnfSize = sizeTriple $! pdnfForm
       t2 <- T.getClockTime
       let pdnfTime = diffTimes t2 t1
       t1 <- T.getClockTime
       let pcnfForm = pcnf' $! p
       let pcnfSize = sizeTriple $! pcnfForm
       t2 <- T.getClockTime
       let pcnfTime = diffTimes t2 t1
       return (formSize, (pdnfTime, pdnfSize), (pcnfTime, pcnfSize))

  pnfPerformRandom :: SizeTriple -> IO CanonPerformance
  pnfPerformRandom (aCard, aSize, jSize) =
    do p <- randomIntProp (aCard, aSize, jSize)
       r <- pnfPerform p
       return r

  pnfPerformRepeat :: SizeTriple -> IO ()
  pnfPerformRepeat st = do cp <- pnfPerformRandom st
                           print cp
                           x <- pnfPerformRepeat st
                           return x

---------------------------------------------------------------------------------------------------
-- new stuff ----------- move this into the right places ------------------------------------------
---------------------------------------------------------------------------------------------------

  onePrimFormTest :: IForm -> ()
  onePrimFormTest iform = let q = primForm iform
                              b1 = primeDNF (iDNF iform) == iDNF q
                              b2 = primeCNF (iCNF iform) == iCNF q
                          in if b1 && b2
                             then ()
                             else error ("onePrimFormTest dicovered an error for \n" ++ (show iform))

  primFormTesting :: Int -> Int -> Int -> IO ()
  primFormTesting atomNumber formLength averLineLen=
     do iform <- randomIForm atomNumber formLength averLineLen
        let b = onePrimFormTest iform
        putStr "."
        t <- primFormTesting atomNumber formLength averLineLen
        return t

  randomPrimForms :: Int -> Int -> Int -> IO ()
  randomPrimForms atomNumber formLen averLineLen =
     do iform <- randomIForm atomNumber formLen averLineLen
        let p = primForm iform
        let q = primForm iform
        putStr (show (formLength iform) ++ ":" ++ show (formLength p) ++ ":" ++ show (formLength q) ++ " ")
        t <- randomPrimForms atomNumber formLen averLineLen
        return t

  xprimTest :: ILine -> ILine -> Bool
  xprimTest line1 line2 = (xprim line1 line2) == (xprim' line1 line2)

  pairPrimTest :: ILine -> ILine -> Bool
  pairPrimTest line1 line2 = (pdnf == iDNF iform) && (pcnf == iCNF iform)
    where pdnf = primeDNF (DJ [iNLC line1, iNLC line2])
          pcnf = primeCNF (CJ [iNLD line1, iNLD line2])
          iform = orderForm (pairPrim line1 line2)

  xprimTesting :: Int -> Int -> IO ()
  xprimTesting atomNumber averLen =
    do line1 <- randomILine atomNumber averLen
       line2 <- randomILine atomNumber averLen
       let b = xprimTest line1 line2
       let s = if b then "." else error ("xprim and xprim' produce different results for "
                                       ++ (show line1) ++ " and " ++ (show line2))
       putStr s
       let b = pairPrimTest line1 line2
       let s = if b then "." else error ("pairPrim and primeDNF or primeCNF produce different PNFs for"
                                         ++ (show line1) ++ " and " ++ (show line2))
       putStr s
       t <- xprimTesting atomNumber averLen
       return t

  m2formTesting :: Int -> Int -> Int -> IO ()
  m2formTesting atomNumber formLen averLineLen =
     do iform <- randomIForm atomNumber formLen averLineLen
        let m = m2form iform
        putStr (show (formLength iform) ++ ":" ++ show (formLength m) ++ "  ")
        t <- m2formTesting atomNumber formLen averLineLen
        return t

  verbosePProcedure :: IForm -> IO ()
  verbosePProcedure iform = iter iform 0
    where iter iform step = do putStrLn ("step: " ++ (show step) ++
                                         "; atoms: " ++ (show (length (formIndices iform))) ++
                                         "; length: " ++ (show (formLength iform)) ++
                                         "; volume: " ++ (show (volume iform)) ++
                                         ": averageLineLength: " ++ (show (averageLineLength iform)))
                               let mform1 = m2form iform
                               let mform2 = orderForm mform1
                               r <- if mform2 == iform
                                    then putStrLn "DONE"
                                    else iter mform2 (step + 1)
                               return r

  pProcedureTesting :: Int -> Int -> Int -> IO ()
  pProcedureTesting atomNumber formLen averLineLen =
     do p <- randomIForm atomNumber formLen averLineLen
        verbosePProcedure p
        t <- pProcedureTesting atomNumber formLen averLineLen
        return t

-- Profiling - second version

-- testing the prime generation of IForms with random input IForms

  type Seconds = Double

  verboseRandomPrimeTest :: (Int,Int,Int) -> IO (Int,Int,Int,Seconds)
  -- ^ @randomPrimeTest (atomNum, formLen, averLineLen) = (atomNum', formLen', vol', secs')@
  verboseRandomPrimeTest (atomNum, formLen, averLineLen) =
     do iform <- randomIForm atomNum formLen averLineLen
        putStrLn "*** The random IForm is: ***"
        print iform
        putStrLn "*** Its prime IForm is: ***"
        t1 <- getCPUTime
        let pform = primForm $! iform
        print pform
        t2 <- getCPUTime
        putStrLn "*** Result of this test ***"
        putStr "Properties of the random IForm: "
        iformDiagnosis iform
        putStr "Properties of its prime IForm:  "
        iformDiagnosis pform
        let t = (fromIntegral (t2 - t1)) / 1000000000000 :: Seconds
        putStrLn ("Time to produce the prime IForm: " ++ (show t) ++ " seconds.")
        return (length (formIndices pform), formLength pform, volume pform, t)
     where iformDiagnosis :: IForm -> IO ()
           iformDiagnosis iform =  putStrLn ( "atomNumber = " ++ (show (length (formIndices iform))) ++
                                              "; formLength = " ++ (show (formLength iform)) ++
                                              "; volume = " ++ (show (volume iform)) ++
                                              "; averageLineLength = " ++ (show (averageLineLength iform)) )

  verboseRandomPrimeTesting :: (Int,Int,Int) -> Int -> IO (Int,Int,Seconds,Seconds,Seconds)
  -- ^ @verboseRandomPrimeTesting (atomNum, formLen, averLineLen) numberOfTests = (maxFormLen, averFormLen, maxTime, averTime, standDev)@
  verboseRandomPrimeTesting (atomNum, formLen, averLineLen) numberOfTests = iter [] numberOfTests
    where iter :: [(Int,Int,Int,Seconds)] -> Int -> IO (Int,Int,Seconds,Seconds,Seconds)
          iter qL n | n == 0    = do let (maxFormLen, averFormLen, maxTime, averTime, standDev) = evaluate qL
                                     D.printTextFrame $ D.textFrameBox $ D.correctTextFrame
                                      [ "All " ++ (show numberOfTests) ++ " tests have been performed: ",
                                        "  Parameters for the random IForms were:",
                                        "    atomNumber = " ++ (show atomNum),
                                        "    formLength = " ++ (show formLen),
                                        "    averageLineLength = " ++ (show averLineLen),
                                        "  Result of the test series:",
                                        "    numberOfTests = " ++ (show numberOfTests),
                                        "    maxFormLength = " ++ (show maxFormLen),
                                        "    averageFormLength = " ++ (show averFormLen),
                                        "    maxTime = " ++ (show maxTime) ++ " seconds",
                                        "    averageTime = " ++ (show averTime) ++ " seconds",
                                        "    standardTimeDeviation = " ++ (show standDev) ++ " seconds" ]
                                     return (maxFormLen, averFormLen, maxTime, averTime, standDev)
                    | otherwise = do putStrLn ("*** Test number " ++ (show (numberOfTests - n + 1)) ++
                                               " of " ++ (show numberOfTests) ++ ": ***")
                                     q <- verboseRandomPrimeTest (atomNum, formLen, averLineLen)
                                     iter (q:qL) (n - 1)
          evaluate :: [(Int,Int,Int,Seconds)] -> (Int,Int,Seconds,Seconds,Seconds)
          evaluate qL = let (formLenL, timeL) = unzip (map (\ (atomNum, formLen, vol, secs) -> (formLen, secs)) qL)
                        in ( maximum formLenL,
                             (sum formLenL) `div` (length formLenL),
                             maximum timeL,
                             meanValue timeL,
                             standDeviation timeL )

  meanValue :: [Seconds] -> Seconds
  meanValue timeL = (sum timeL) / (fromIntegral (length timeL))

  standDeviation :: [Seconds] -> Seconds
  standDeviation timeL = sqrt ( (sum (map (\ t -> (t - m)^2) timeL)) / (fromIntegral (length timeL)) )
    where m = meanValue timeL

  normSeconds :: Seconds -> Seconds
  normSeconds s = (fromIntegral (round (s * 1000))) / 1000


