{- |
  An 'Olist' is an /ordered list/. The main function of this module is the implementation of the finite subset structure of a given type @a@. Finite sets are represented as ordered lists and the basic set functions and relations like 'union', 'intersection', 'included' etc. are provided.
-}

module Olist (

  -- * The @Olist@ type

  Olist,

  -- * The construction and test for ordered lists

  olist,
  -- | For example,
  --
  -- > > olist ["b", "c", "b", "a", "c", "a"]
  -- > ["a","b","c"]
  --
  -- As the example shows, multiple occuring members are deleted.
  --
  -- (The implementation builts the ordered list with component-wise 'insert' and that is not an optimal sorting algorithm in general.
  -- But it is optimal, when the argument is an already ordered list. We use 'olist' only as a an additional safety-mechanism for
  -- functions like @ext :: p -> [a] -> p@, where in most cases the second argument will usually be an @Olist a@ value, anyway.)

  isOlist,
  -- | Checks if the given argument is ordered.

  -- * The empty list

  empty,
  -- | Returns the empty list @[]@, i.e.
  --
  -- > > Olist.empty
  -- > []
  --
  -- (Note, that there is also an @empty@ function in the @Costack@ module.)

  isEmpty,
  -- | Checks on emptiness; i.e. it is the same as the Haskell native @null@.
  --
  -- > > isEmpty [1,2,3]
  -- > False

  -- * Singular operations on ordered lists

  member,
  -- | For example,
  --
  -- > > member 7 [3,5,7,9]
  -- > True
  -- > > 4 `member` [2,3,4,5]
  -- > True

  insert,
  -- | For example,
  --
  -- > > insert 7 [3,5,9,11]
  -- > [3,5,7,9,11]
  -- > > insert 7 [3,5,7,9,11]
  -- > [3,5,7,9,11]

  delete,
  -- | For example,
  --
  -- > > delete 7 [3,5,7,9,11]
  -- > [3,5,9,11]
  -- > > delete 7 [3,5,9,11]
  -- > [3,5,9,11]

  -- * The common binary operations on sets

  -- | These functions all assume, that the arguments are actually 'Olist' values. Otherwise, the function doesn't terminate with an
  -- error, it just produces unintended results.

  included,
  -- | Implementation of &#8838; on (finite) sets. For example,
  --
  -- > > included "acd" "abcd"        -- recall, that "acd" is the list ['a', 'c', 'd']
  -- > True
  -- > > [2,4] `included` [1,2,3,4,5]
  -- > True

  properlyIncluded,
  -- | Implementation of the strict version &#8834; of &#8838;, i.e. the first argument must be included, but different to the second one.

  disjunct,
  -- | Two finite sets, i.e. two ordered lists are /disjunct/ iff they do not have a common member. For example
  --
  -- > > disjunct "acd" "bef"
  -- > True
  -- > > "abc" `disjunct` "bef"
  -- > False
  -- > > [] `disjunct` [1,2,3]
  -- > True

  properlyDisjunct,
  -- | Two finite sets are /properly disjunct/ iff they are disjunct and none of them is empty.
  --
  -- > > [] `properlyDisjunct` [1,2,3]
  -- > False

  equal,
  -- | The equality of two finite sets; actually it is just another name for @(==)@ on ordered lists.

  union,
  -- | The implementation of &#8746;, for example
  --
  -- > > [1,2,4,5] `union` [1,3,5,7]
  -- > [1,2,3,4,5,7]

  intersection,
  -- | The implementation of &#8745;, for example
  --
  -- > > [1,2,4,5] `intersection` [1,3,5,7]
  -- > [1,5]

  difference,
  -- | Implementation of the difference operator &#92; on sets. For example,
  --
  -- > > [1,2,4,5] `difference` [1,3,5,7]
  -- > [2,4]

  opposition,
  -- | The /opposition/ or /symmetric difference/ @S@&#8711;@T@ of two sets @S@, @T@ is defined as @(S&#92;T)&#8746;(T&#92;S)@. For example,
  --
  -- > > [1,2,4,5] `opposition` [1,3,5,7]
  -- > [2,3,4,7]

  unionList,
  -- | Returns the union of a list of ordered lists. For example,
  --
  -- > > unionList [[1,3,5], [1,2,3], [1,5,9]]
  -- > [1,2,3,5,9]
  -- > > unionList []
  -- > []

  intersectionList,
  -- | Returns the intersection of a list of ordered lists. The result is undefined for the empty list.
  --
  -- > > intersectionList [[1,3,5], [1,2,3], [1,5,9]]
  -- > [1]
  -- > > intersectionList []
  -- > *** Exception: Olist.intersectionList: not defined for empty list argument

) where ---------------------------------------------------------------------------------------------------------------

-- the type of ordered lists

  type Olist a = [a]

-- order list construction

  olist :: Ord a => [a] -> Olist a
  olist [] = []
  olist (a:aL) = insert a (olist aL)

  isOlist :: Ord a => [a] -> Bool
  isOlist [] = True
  isOlist [a] = True
  isOlist (a:b:cL) = (a < b) && isOlist (b:cL)

-- empty list

  empty :: Ord a => Olist a
  empty = []

  isEmpty :: Ord a => Olist a -> Bool
  isEmpty = null

-- singular operations

  member :: Ord a => a -> Olist a -> Bool
  member a [] = False
  member a (b:bL) = case compare a b of
    LT -> False
    EQ -> True
    GT -> member a bL

  insert :: Ord a => a -> Olist a -> Olist a
  insert a [] = [a]
  insert a (b:bL) = case compare a b of
    LT -> a:b:bL
    EQ -> b:bL
    GT -> b : (insert a bL)

  delete :: Ord a => a -> Olist a -> Olist a
  delete a [] = []
  delete a (b:bL) = case compare a b of
    LT -> b:bL
    EQ -> bL
    GT -> b : (delete a bL)

-- binary operations

  included :: Ord a => Olist a -> Olist a -> Bool
  included [] bL = True
  included aL [] = False
  included (a:aL) (b:bL) = case compare a b of
    LT -> False
    EQ -> included aL bL
    GT -> included (a:aL) bL

  properlyIncluded :: Ord a => Olist a -> Olist a -> Bool
  properlyIncluded [] [] = False
  properlyIncluded [] bL = True
  properlyIncluded aL [] = False
  properlyIncluded (a:aL) (b:bL) = case compare a b of
    LT -> False
    EQ -> properlyIncluded aL bL
    GT -> included (a:aL) bL

  disjunct :: Ord a => Olist a -> Olist a -> Bool
  disjunct [] bL = True
  disjunct bL [] = True
  disjunct (a:aL) (b:bL) = case compare a b of
    LT -> disjunct aL (b:bL)
    EQ -> False
    GT -> disjunct (a:aL) bL

  properlyDisjunct :: Ord a => Olist a -> Olist a -> Bool
  properlyDisjunct [] bL = False
  properlyDisjunct bL [] = False
  properlyDisjunct (a:aL) (b:bL) = case compare a b of
    LT -> disjunct aL (b:bL)
    EQ -> False
    GT -> disjunct (a:aL) bL

  equal :: Ord a => Olist a -> Olist a -> Bool
  equal = (==)

  union :: Ord a => Olist a -> Olist a -> Olist a
  union [] bL = bL
  union aL [] = aL
  union (a:aL) (b:bL) = case compare a b of
    LT -> a : (union aL (b:bL))
    EQ -> a : (union aL bL)
    GT -> b : (union (a:aL) bL)

  intersection :: Ord a => Olist a -> Olist a -> Olist a
  intersection [] bL = []
  intersection aL [] = []
  intersection (a:aL) (b:bL) = case compare a b of
    LT -> intersection aL (b:bL)
    EQ -> a : (intersection aL bL)
    GT -> intersection (a:aL) bL

  difference :: Ord a => Olist a -> Olist a -> Olist a
  difference [] bL = []
  difference aL [] = aL
  difference (a:aL) (b:bL) = case compare a b of
    LT -> a : (difference aL (b:bL))
    EQ -> difference aL bL
    GT -> difference (a:aL) bL

  opposition :: Ord a => Olist a -> Olist a -> Olist a
  opposition [] bL = bL
  opposition aL [] = aL
  opposition (a:aL) (b:bL) = case compare a b of
    LT -> a : (opposition aL (b:bL))
    EQ -> opposition aL bL
    GT -> b : (opposition (a:aL) bL)

  unionList :: Ord a => [Olist a] -> Olist a
  unionList = foldl union []

  intersectionList :: Ord a => [Olist a] -> Olist a
  intersectionList [] = error "Olist.intersectionList: not defined for empty list argument"
  intersectionList aL = foldl1 intersection aL
