module Costack (
  Costack(),
  fromList, toList,
  cons, cocons,
  Costack.head, Costack.tail,
  isEmpty, empty,
  singleton, append, Costack.concat, merge,
  Costack.length,
  Costack.map, Costack.foldr,
  Costack.take, Costack.filter,
  Costack.sort, Costack.strictSort, Costack.sorted, Costack.strictSorted,
) where ---------------------------------------------------------------------------------------------------------------

  import qualified List as L

  data Costack a = COSTACK [a]
    deriving (Show, Read, Eq)

  fromList :: [a] -> Costack a
  fromList l = COSTACK l

  toList :: Costack a -> [a]
  toList (COSTACK l)= l

  cons :: a -> Costack a -> Costack a
  cons x (COSTACK yL) = COSTACK (x:yL)

  cocons :: Costack a -> a -> Costack a
  cocons (COSTACK xL) y = COSTACK (xL ++ [y])

  head :: Costack a -> a
  head (COSTACK []) = error "Costack.head -- empty costack"
  head (COSTACK (x:xL)) = x

  tail :: Costack a -> Costack a
  tail (COSTACK []) = error "Costack.tail -- empty costack"
  tail (COSTACK (x:xL)) = COSTACK xL

  isEmpty :: Costack a -> Bool
  isEmpty (COSTACK l) = null l

  empty :: Costack a
  empty = COSTACK []

  singleton :: a -> Costack a
  singleton x = COSTACK [x]

  append :: Costack a -> Costack a -> Costack a
  append (COSTACK xL) (COSTACK yL) = COSTACK (xL ++ yL)

  concat :: [Costack a] -> Costack a
  concat cL = COSTACK (Prelude.concat (Prelude.map toList cL))

  merge :: Costack (Costack a) -> Costack a
  merge c = COSTACK (Prelude.concat (Prelude.map toList (toList c)))

  length :: Costack a -> Int
  length (COSTACK xL) = Prelude.length xL

  map :: (a -> b) -> Costack a -> Costack b
  map f (COSTACK xL) = COSTACK (Prelude.map f xL)

  foldr :: (a -> b -> b) -> b -> Costack a -> b
  foldr f x (COSTACK []) = x
  foldr f x (COSTACK (y:yL)) = Costack.foldr f (f y x) (COSTACK yL)

  take :: Int -> Costack a -> Costack a
  take n (COSTACK xL) = COSTACK (Prelude.take n xL)

  filter :: (a -> Bool) -> Costack a -> Costack a
  filter p (COSTACK xL) = COSTACK (Prelude.filter p xL)

  sort :: Ord a => Costack a -> Costack a
  sort (COSTACK xL) = COSTACK (L.sort xL)

  sorted :: Ord a => Costack a -> Bool
  sorted (COSTACK xL) = iter xL
    where iter [] = True
          iter [x] = True
          iter (x:y:zL) = x < y && iter (y:zL)

  strictSort :: Ord a => Costack a -> Costack a
  strictSort (COSTACK xL) = COSTACK (L.nub (L.sort xL))

  strictSorted :: Ord a => Costack a -> Bool
  strictSorted (COSTACK xL) = iter xL
    where iter [] = True
          iter [x] = True
          iter (x:y:zL) = x <= y && iter (y:zL)
