-----------------------------------------------------------------------------
-- Number.hs:	Fixed width integers with overflow detection
--
-- This library defines a numeric datatype of fixed width integers
-- (whatever Int supplies).  But, unlike Int, overflows are detected and
-- cause a run-time error.  Covers all classes upto and including Bounded
-- and Ix.  A fairly messy hack, but it works (most of the time :-) ...
--
-- Suitable for use with Hugs 98
-----------------------------------------------------------------------------

module Number( 
	Number,
	-- instance Eq       Number,
	-- instance Ord      Number,
	-- instance Show     Number,
	-- instance Enum     Number,
	-- instance Num      Number,
	-- instance Bounded  Number,
	-- instance Real     Number,
	-- instance Ix       Number,
	-- instance Integral Number,
	) where

import Ix(Ix(..))

default (Number,Int,Float)

type Number = Int
  in numEq           :: Number -> Number -> Bool,
     numCmp          :: Number -> Number -> Ordering,
     numShowsPrec    :: Int -> Number -> ShowS,
     numEnumFrom     :: Number -> [Number],
     numEnumFromThen :: Number -> Number -> [Number],
     numAdd          :: Number -> Number -> Number,
     numSub          :: Number -> Number -> Number,
     numMul          :: Number -> Number -> Number,
     numNeg          :: Number -> Number,
     numFromInt      :: Int -> Number,
     numToInt        :: Number -> Int,
     numFromInteger  :: Integer -> Number,
     numToInteger    :: Number -> Integer,
     numMax          :: Number,
     numMin          :: Number,
     numSignum       :: Number -> Number,
     numToRat        :: Number -> Rational,
     numQrm          :: Number -> Number -> (Number, Number),
     numRange        :: (Number, Number) -> [Number],
     numIndex        :: (Number, Number) -> Number -> Int,
     numInRange      :: (Number, Number) -> Number -> Bool

numEq           = (==)
{--
numEq 1 2
False

numEq 1 1
True

--}

numCmp          = compare
{--
numCmp 1 1
EQ

numCmp 1 0
GT

numCmp 1 2
LT
--}

numShowsPrec    = showsPrec
{--
numShowsPrec 1 2 ""
"2"

--}

numEnumFrom     = enumFrom
{--

numEnumFrom n => [n..]

--}

numEnumFromThen = enumFromThen
{--

enumFromThen a b => [a,b, ...]

--}

numFromInt x    = x
{--
numFromInt 6
6

--}

numToInt x      = x
{--

numToInt 6
6

--}

numFromInteger  = fromInteger
{--
numToInt 6
6

--}

numToInteger    = toInteger
{--
numToInt 6
6

--}

numMax          = maxBound
{--
numMax
2147483647

--}

numMin          = minBound
{--
numMin
-2147483648

--}

numSignum       = signum
{--
Sign of a number,

2 * numSignum (-3)
-2

--}

numToRat        = toRational
{--
numToRat 23
23 % 1

--}

numQrm          = quotRem
{--
numQrm 23 6
(3,5)

--}

numRange        = range
{--
 numRange (2,10)
[2,3,4,5,6,7,8,9,10]

--}

numIndex        = index
{--
numIndex (2,10) 5
3

index of 5 is 3

--}

numInRange      = inRange
{--
numInRange (2,10) 5
True

numInRange (2,10) 50
False
--}


numAdd x y = if xsgn/=ysgn || xsgn==rsgn then r else error "add overflow!"
             where xsgn = x>=0
                   ysgn = y>=0
                   rsgn = r>=0
                   r    = x + y
{--
numAdd 1 (-2)
-1

--}


numSub x y = if xsgn==ysgn || ysgn/=rsgn then r else error "sub overflow!"
             where xsgn = x>=0
                   ysgn = y>=0
                   rsgn = r>=0
                   r    = x - y
{--
numSub 1 (-2)
3


--}


numMul x y = if y==0 || (r `div` y == x) then r else error "mult overflow!"
             where r = x * y
{--
numMul 1 (-2)
-2

--}


numNeg x   = if x>=0 || r>=0 then r else error "negate overflow!"
             where r = negate x
{--
numNeg (-2)
2

--}


instance Eq Number where
  (==)   = numEq

instance Ord Number where
  compare = numCmp

instance Show Number where
  showsPrec = numShowsPrec


instance Enum Number where
  toEnum       = numFromInt
  fromEnum     = numToInt
  enumFrom     = numEnumFrom
  enumFromThen = numEnumFromThen

instance Num Number where
  (+)         = numAdd
  (-)         = numSub
  (*)         = numMul
  negate      = numNeg
  fromInt     = numFromInt
  fromInteger = numFromInteger
  abs x       = if x<0 then negate x else x
  signum      = numSignum


instance Bounded Number where
  minBound    = numMin
  maxBound    = numMax

instance Real Number where
  toRational  = numToRat

instance Ix Number where
  range   = numRange
  index   = numIndex
  inRange = numInRange


instance Integral Number where
  quotRem   = numQrm
  toInteger = numToInteger

--------------------------------------------------------------------------------

