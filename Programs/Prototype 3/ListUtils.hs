module ListUtils(
	sums, products,
	subsequences,
	permutations
	) where

sums, products     :: Num a => [a] -> [a]
sums                = scanl (+) 0
products            = scanl (*) 1

{--
sums [1..5] => scanl (op) v => scanl (+) v => [v+1, v+1+2, v+1+2+3, ...... ]

Similarly fpr products,

sums [1..5]
[0,1,3,6,10,15]

products [1..5]
[1,1,2,6,24,120]
--}

-- subsequences xs returns the list of all subsequences of xs.
-- e.g., subsequences "abc" == ["","c","b","bc","a","ac","ab","abc"]
subsequences           :: [a] -> [[a]]
subsequences []         = [[]]
subsequences (x:xs)     = subsequences xs ++ map (x:) (subsequences xs)

-- permutations xs returns the list of all permutations of xs.
-- e.g., permutations "abc" == ["abc","bac","bca","acb","cab","cba"]
permutations           :: [a] -> [[a]]
permutations []         = [[]]
permutations (x:xs)     = [zs | ys <- permutations xs, zs <- interleave x ys ]
  where interleave         :: a -> [a] -> [[a]]
        interleave x []     = [[x]]
        interleave x (y:ys) = [x:y:ys] ++ map (y:) (interleave x ys)

