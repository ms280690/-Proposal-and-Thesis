--- Lightweight, automated, specification-based testing of Curry programs.
--- Currently, EasyCheck is only supported by the Kiel Curry System (KiCS).
--- See 
--- <a href="http://www-ps.informatik.uni-kiel.de/currywiki/tools/easycheck"
--- >here</a> for a tutorial introduction to EasyCheck.
---
--- @author Jan Christiansen and Sebastian Fischer
---
--- @version January 2008
---
module EasyCheck (

  -- test specification
  Prop, (==>), for, forValues,

  test, is, isAlways, isEventually, prop, uniquely, always, eventually, 
  failing, successful, deterministic, (-=-), (#), (<~>), (~>), (<~),

  isSameSet, isSubsetOf,

  -- test annotations
  label, trivial, classify, collect, collectAs, 

  -- test functions
  easyCheck, easyCheck1, easyCheck2, easyCheck3, easyCheck4, easyCheck5,
  verboseCheck, verboseCheck1, verboseCheck2, verboseCheck3, verboseCheck4,
  verboseCheck5,

  -- useful for other tools accessing EasyCheck
  valuesOf, Result(..), result,

  -- useful auxiliary functions
  diagonal

  ) where

import List             ( nub, group, intersperse, (\\) )
import Sort             ( leqList, leqString, mergeSort )
import Meta             ( searchTree )
import Integer          ( abs )
import Read             ( readNat )
import RandomExternal   ( split, nextInt )

infix  4 `isSameSet`, `isSubsetOf`
infix  1 `is`, `isAlways`, `isEventually`, -=-, #, <~>, ~>, <~, `trivial`
infixr 0 ==>


data Test = Test Result [String] [String]

data Result = Undef | Ok | Falsified [String] | Ambigious [Bool] [String]

type Prop = [Test]

notest :: Test
notest = Test Undef [] []

result :: Test -> Result
result (Test r _ _) = r

setResult :: Result -> Test -> Test
setResult res (Test _ s a) = Test res a s

args, stamp :: Test -> [String]
args  (Test _ a _) = a
stamp (Test _ _ s) = s

updArgs, updStamp :: ([String] -> [String]) -> Test -> Test
updArgs  upd (Test r a s) = Test r (upd a) s
updStamp upd (Test r a s) = Test r a (upd s)

-- Test Specification

--- Specify a property as predicate on the list of nondeterministic results.
test :: a -> ([a] -> Bool) -> Prop
test x f = [setResult res notest]
 where
  xs  = valuesOf x
  res = case valuesOf (f xs) of
          [True]  -> Ok
          [False] -> Falsified (map show xs)
          bs      -> Ambigious bs (map show xs)

--- Specify a property as predicate that must hold for
--- the unique result, all results or any result.
is, isAlways, isEventually :: a -> (a -> Bool) -> Prop
is x f = test x (\xs -> case xs of [y] -> f y; _ -> False)
isAlways x  = test x . all
isEventually x = test x . any

--- List boolean value to property.
prop, uniquely, always, eventually :: Bool -> Prop
prop       = uniquely
uniquely   = (`is`id)
always     = (`isAlways`id)
eventually = (`isEventually`id)

--- Properties on the number of results.
failing, successful, deterministic :: _ -> Prop
failing x = test x null
successful x = test x (not . null)
deterministic x = x `is` const True

--- Specify the number of different results.
(#) :: _ -> Int -> Prop
x # n = test x ((n==) . length . nub)

--- Deterministic equality.
--- False if any argument is non-deterministic or fails.
(-=-) :: a -> a -> Prop
x -=- y = (x,y) `is` uncurry (==)

--- Three kinds of "Nondeterministic equality".
(<~>), (~>), (<~) :: a -> a -> Prop
x <~> y = test x (isSameSet (valuesOf y))
x  ~> y = test x (isSubsetOf (valuesOf y))
x <~  y = test x (`isSubsetOf` (valuesOf y))

isSameSet, isSubsetOf, subset :: [a] -> [a] -> Bool
xs `isSameSet` ys = xs' `subset` ys' && ys' `subset` xs'
 where xs' = nub xs; ys' = nub ys
xs `isSubsetOf` ys = nub xs `subset` ys
xs `subset` ys = null (xs\\ys)

--- Implication operator to reject invalid test input.
(==>) :: Bool -> Prop -> Prop
True  ==> p = p
False ==> _ = [notest]

forAll :: (b -> Prop) -> a -> (a -> b) -> Prop
forAll c x f = forAllValues c (valuesOf x) f

forAllValues :: (b -> Prop) -> [a] -> (a -> b) -> Prop
forAllValues c xs f
  = diagonal [[ updArgs (show y:) t | t <- c (f y) ] | y <- xs ]

--- Specify a custom input generator for a property.
for :: a -> (a -> Prop) -> Prop
for = forAll id

forValues :: [a] -> (a -> Prop) -> Prop
forValues = forAllValues id

-- Test Annotations

--- Add a label to a property that is shown in the test summary.
label :: String -> Prop -> Prop
label = map . updStamp . (:)

--- Add a label to a property conditionally.
classify :: Bool -> String -> Prop -> Prop
classify True  name = label name
classify False _    = id

--- Add the label 'trivial' to a property conditionally.
trivial :: Bool -> Prop -> Prop
trivial = (`classify`"trivial")

--- Add a string representation of given value as a label to a property. 
collect :: a -> Prop -> Prop
collect = label . show

--- Like 'collect' but with discriminating prefix.
collectAs :: String -> a -> Prop -> Prop
collectAs name = label . ((name++": ")++) . show

-- Test Functions

data Config = Config Int Int (Int -> [String] -> String)

maxTest, maxFail :: Config -> Int
maxTest (Config n _ _) = n
maxFail (Config _ n _) = n

every :: Config -> Int -> [String] -> String
every (Config _ _ f) = f

setEvery :: (Int -> [String] -> String) -> Config -> Config
setEvery f (Config n m _) = Config n m f

easy :: Config
easy = Config 100 1000
        (\n _ -> let s = ' ':show (n+1) in s ++ [ chr 8 | _ <- s ])

verbose :: Config
verbose = setEvery (\n xs -> show n ++ ":\n" ++ unlines xs) easy

--- Test a property and print a summary.
easyCheck, verboseCheck :: Prop -> IO ()
easyCheck    = check easy
verboseCheck = check verbose

suc :: (a -> Prop) -> (b -> a) -> Prop
suc n = forAll n unknown

easyCheck1 :: (_ -> Prop) -> IO ()
easyCheck1 = easyCheck . suc id

easyCheck2 :: (_ -> _ -> Prop) -> IO ()
easyCheck2 = easyCheck . suc (suc id)

easyCheck3 :: (_ -> _ -> _ -> Prop) -> IO ()
easyCheck3 = easyCheck . suc (suc (suc id))

easyCheck4 :: (_ -> _ -> _ -> _ -> Prop) -> IO ()
easyCheck4 = easyCheck . suc (suc (suc (suc id)))

easyCheck5 :: (_ -> _ -> _ -> _ -> _ -> Prop) -> IO ()
easyCheck5 = easyCheck . suc (suc (suc (suc (suc id))))

verboseCheck1 :: (_ -> Prop) -> IO ()
verboseCheck1 = verboseCheck . suc id

verboseCheck2 :: (_ -> _ -> Prop) -> IO ()
verboseCheck2 = verboseCheck . suc (suc id)

verboseCheck3 :: (_ -> _ -> _ -> Prop) -> IO ()
verboseCheck3 = verboseCheck . suc (suc (suc id))

verboseCheck4 :: (_ -> _ -> _ -> _ -> Prop) -> IO ()
verboseCheck4 = verboseCheck . suc (suc (suc (suc id)))

verboseCheck5 :: (_ -> _ -> _ -> _ -> _ -> Prop) -> IO ()
verboseCheck5 = verboseCheck . suc (suc (suc (suc (suc id))))


check :: Config -> Prop -> IO ()
check config p = do
  isOrBased <- evalModeIsOrBased
  if isOrBased then tests config p 0 0 []
   else putStrLn $ unlines
    ["","EasyCheck must be run without suspending computations.",""
    ,"Please",""
    ," - type ':set or' at the command prompt in kicsi or"
    ," - put 'ChoiceMode=OrBased' in your ~/.kicsrc file",""
    ,"to enable this mode for this session or for all sessions respectively."]

tests :: Config -> [Test] -> Int -> Int -> [[String]] -> IO ()
tests _ [] ntest _ stamps = done "Passed" ntest stamps
tests config (t:ts) ntest nfail stamps
  | ntest == maxTest config = done "OK, passed" ntest stamps
  | nfail == maxFail config = done "Arguments exhausted after" ntest stamps
  | otherwise = do
      putStr (every config ntest (args t))
      case result t of
        Undef -> tests config ts ntest (nfail+1) stamps
        Ok    -> tests config ts (ntest+1) nfail (stamp t:stamps)
        Falsified results -> putStr $
          "Falsified by " ++ nth (ntest+1) ++ " test" ++
          (if null (args t) then "." else ".\nArguments:") ++ "\n" ++
          unlines (args t) ++
          if null results then "no result\n"
           else "Results:\n" ++ unlines results
        Ambigious bs results -> putStr $
          "Ambigious property yields " ++ show bs ++ " for " ++ 
          nth (ntest+1) ++ " test" ++
          (if null (args t) then "." else ".\nArguments:") ++ "\n" ++
          unlines (args t) ++
          if null results then "no result\n"
           else "Results:\n" ++ unlines results

nth :: Int -> String
nth n = case n of 1 -> "first"; 2 -> "second"; 3 -> "third"; _ -> show n++ "th"

done :: String -> Int -> [[String]] -> IO ()
done mesg ntest stamps = do
  putStr $ mesg ++ " " ++ show ntest ++ " test"
        ++ (if ntest >= 2 then "s" else "") ++ table
 where
  table = display
        . map entry
        . reverse
        . mergeSort (leqPair (<=) (leqList leqString))
        . map pairLength
        . group
        . mergeSort (leqList leqString)
        . filter (not . null)
        $ stamps

  display []         = ".\n"
  display [x]        = " - " ++ x ++ ".\n"
  display xs@(_:_:_) = ".\n" ++ unlines (map (++".") xs)

  pairLength xss@(xs:_) = (length xss,xs)

  entry (n,xs) = percentage n ntest ++ " " ++ concat (intersperse ", " xs)

  percentage n m = let s = show ((100*n)`div`m)
                    in replicate (3-length s) ' ' ++ s ++ "%"

-- Auxiliary Functions

leqPair :: (a -> a -> Bool) -> (b -> b -> Bool) -> ((a,b) -> (a,b) -> Bool)
leqPair leqa leqb (x1,y1) (x2,y2)
  | x1 == x2  = leqb y1 y2
  | otherwise = leqa x1 x2

leList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
leList _  []     []     = False
leList _  []     (_:_)  = True
leList _  (_:_)  []     = False
leList le (x:xs) (y:ys) = le x y || x == y && leList le xs ys

valuesOf :: a -> [a]
valuesOf =
  rndLevelDiagFlat 3 2008 .
  searchTree . (id$##)

--- randomized diagonalization of levels. 
---
--- @param rs random seeds
--- @param t search tree
--- @return enumeration of values in given search tree
---
rndLevelDiag :: Int -> SearchTree a -> [a]
rndLevelDiag rnd t =
  [ x | Value x <- diagonal (rndLevels rnd [t]) ]

rndLevels :: Int -> [SearchTree a] -> [[SearchTree a]]
rndLevels rnd ts =
  if null ts then []
   else ts : rndLevels r (concat (zipWith shuffle rs [ us | Choice us <- ts ]))
 where
  r:rs = split rnd

--- randomized diagonalization of levels with flatening. 

rndLevelDiagFlat :: Int -> Int -> SearchTree a -> [a]
rndLevelDiagFlat d rnd t = 
  concat $ transpose (zipWith rndLevelDiag rs (flatRep d [t]))
 where
  rs = split rnd

flat :: SearchTree a -> [SearchTree a]
flat t = case t of
           Value _ -> [t]
           Choice ts -> ts
           _ -> []

flatRep :: Int -> [SearchTree a] -> [SearchTree a]
flatRep n ts | n==0      = ts
             | otherwise = flatRep (n-1) (concatMap flat ts)

-- auxiliary functions

--- list diagonalization. 
--- Fairly merges (possibly infinite) list of (possibly infinite) lists.
---
--- @param ls lists of lists
--- @return fair enumeration of all elements of inner lists of given lists
---
diagonal :: [[a]] -> [a]
diagonal = concat . foldr diags []
 where
  diags []     ys = ys
  diags (x:xs) ys = [x] : merge xs ys

  merge []       ys     = ys
  merge xs@(_:_) []     = map (:[]) xs
  merge (x:xs)   (y:ys) = (x:y) : merge xs ys


--- Computes a random permutation of the given list.
---
--- @param rnd random seed
--- @param l lists to shuffle
--- @return shuffled list
---
shuffle :: Int -> [a] -> [a]
shuffle rnd l = shuffleWithLen (nextInt rnd) (length l) l

shuffleWithLen :: [Int] -> Int -> [a] -> [a]
shuffleWithLen (r:rs) len xs
  | len == 0  = []
  | otherwise = z : shuffleWithLen rs (len-1) (ys++zs)
 where
  (ys,z:zs) = splitAt (abs r `mod` len) xs


transpose :: [[a]] -> [[a]]
transpose [] = []
transpose ([] : xss) = transpose xss
transpose ((x:xs) : xss)
  = (x : [h | (h:_) <- xss]) : transpose (xs : [t | (_:t) <- xss])

evalModeIsOrBased :: IO Bool
evalModeIsOrBased = getSearchTree (id$#unknown) >>= return . (==Value ())
