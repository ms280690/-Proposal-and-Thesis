----------------------------------------------------------------------------
--- The standard prelude of Curry.
--- All top-level functions defined in this module
--- are always available in any Curry program.
----------------------------------------------------------------------------

module Prelude where
-- Lines beginning with "--++" are part of the prelude
-- but cannot parsed by the compiler

-- Infix operator declarations:

infixl 9 !!
infixr 9 .
infixl 7 *, *^, `div`, `mod`
infixl 6 +, +^, -, -^
-- infixr 5 :                          -- declared together with list
infixr 5 ++
infix  4 =:=, ==, ===, /=, <, >, <=, >=, =:<=, <^, <=^, >^, >=^
infix  4  `elem`, `notElem`
infixr 3 &&
infixr 2 ||
infixl 1 >>, >>=
infixr 0 $, $!, $!!, $#, $##, `seq`, &, &>, ?

-- external base types for numbers and characters
data Float
data Char 
type String = [Char]

-- Some standard combinators:

--- Function composition.
(.)   :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f (g x)

--- Identity function.
id              :: a -> a
id x            = x

--- Constant function.
const           :: a -> _ -> a
const x _       = x

--- Converts an uncurried function to a curried function.
curry           :: ((a,b) -> c) -> a -> b -> c
curry f a b     =  f (a,b)

--- Converts an curried function to a function on pairs.
uncurry         :: (a -> b -> c) -> (a,b) -> c
uncurry f (a,b) = f a b

--- (flip f) is identical to f but with the order of arguments reversed.
flip            :: (a -> b -> c) -> b -> a -> c
flip  f x y     = f y x

--- Repeats application of a function until a predicate holds.
until          :: (a -> Bool) -> (a -> a) -> a -> a
until p f x     = if p x then x else until p f (f x)

--- Right-associative application.
($)             :: (a -> b) -> a -> b
f $ x           = f x

--- Right-associative application with strict evaluation of its argument
--- to head normal form.
($!)    :: (a -> b) -> a -> b
($!) external

--- Right-associative application with strict evaluation of its argument
--- to normal form.
($!!)    :: (a -> b) -> a -> b
($!!) external

--- Right-associative application with strict evaluation of its argument
--- to head normal form. 
--- Suspends until the result is bound to a non-variable term.
($#) :: (a->b) -> a -> b
($#) external

--- Right-associative application with strict evaluation of its argument
--- to ground normal form. 
--- Suspends until the result is bound to a non-variable term.
($##) :: (a->b) -> a -> b
($##) external

--- Evaluates the argument to spine form and returns it.
--- Suspends until the result is bound to a non-variable spine.
ensureSpine :: [a] -> [a] 
ensureSpine = (ensureList $#)
 where ensureList []     = []
       ensureList (x:xs) = x : ensureSpine xs

--- Evaluates the first argument to head normal form (which could also
--- be a free variable) and returns the second argument.
seq     :: _ -> a -> a
seq x y = (const y) $! x



--- Aborts the execution with an error message.
error :: String -> _
error s = prim_error $## s

prim_error    :: String -> _
prim_error external

--- A non-reducible polymorphic function.
--- It is useful to express a failure in a search branch of the execution.
failed :: _ 
failed external

-- Boolean values
data Bool = False | True 

--- Sequential conjunction on Booleans.
(&&)            :: Bool -> Bool -> Bool
True  && x      = x
False && _      = False
 

--- Sequential disjunction on Booleans.
(||)            :: Bool -> Bool -> Bool
True  || _      = True
False || x      = x
 

--- Negation on Booleans.
not             :: Bool -> Bool
not True        = False
not False       = True

--- Useful name for the last condition in a sequence of conditional equations.
otherwise       :: Bool
otherwise       = True


--- The standard conditional. It suspends if the condition is a free variable.
if_then_else           :: Bool -> a -> a -> a
if_then_else b t f = case b of True  -> t
                               False -> f


--- Ordering type. Useful as a result of comparison functions.
data Ordering = LT | EQ | GT

isLT LT = True
isLT GT = False
isLT EQ = False

isGT LT = False
isGT GT = True
isGT EQ = False

isEQ LT = False
isEQ GT = False
isEQ EQ = True

{-compare :: Int -> Int -> Ordering
compare x y | (prim_Int_le $# x) $# y = LT
            | (prim_Int_le $# y) $# x = GT
            | otherwise = EQ -}

compare :: Int -> Int -> Ordering
compare Zero    Zero    = EQ
compare Zero    (Pos _) = LT
compare Zero    (Neg _) = GT
compare (Pos _) Zero    = GT
compare (Pos x) (Pos y) = cmpNat x y
compare (Pos _) (Neg _) = GT
compare (Neg _) Zero    = LT
compare (Neg _) (Pos _) = LT
compare (Neg x) (Neg y) = cmpNat y x

{-
compare x y = compareData [toNumData x] [toNumData y]
  where
    compareData :: [NumData] -> [NumData] -> Ordering
    compareData [] []    = EQ
    compareData (NumData xi xts:xs) (NumData yi yts:ys)
      | int_le xi yi = LT
      | int_le yi xi = GT
      | otherwise    = compareData (xts++xs) (yts++ys)

-}
--- Less-than on ground data terms.
(<)  :: Int -> Int -> Bool  
x < y = compare x y == LT
          
--- Greater-than on ground data terms.
(>)  :: Int -> Int -> Bool  
x > y = compare x y == GT

--- Less-or-equal on ground data terms.
(<=)  ::  Int -> Int -> Bool 
x <= y = compare x y /= GT

--- Greater-or-equal on ground data terms.
(>=) ::  Int -> Int -> Bool
x >= y = compare x y /= LT

--- Maximum of ground data terms.
max  ::  Int -> Int -> Int  
max x y = case compare x y of 
  LT -> y
  _  -> x

---  Minimum of ground data terms
min  :: Int -> Int -> Int
min  x y = case compare x y of 
  GT -> y
  _  -> x
         
--- Equality on finite ground data terms.
(==)            :: a -> a -> Bool
(==) external

--- Disequality.
(/=)            :: a -> a -> Bool
x /= y          = not (x==y)


-- Pairs

--++ data (a,b) = (a,b)

--- Selects the first component of a pair.
fst             :: (a,_) -> a
fst (x,_)       = x

--- Selects the second component of a pair.
snd             :: (_,b) -> b
snd (_,y)       = y


-- Unit type
--++ data () = ()

-- Lists

--++ data [a] = [] | a : [a]

--- Computes the first element of a list.
head            :: [a] -> a
head (x:_)      = x

--- Computes the remaining elements of a list.
tail            :: [a] -> [a]
tail (_:xs)     = xs

--- Is a list empty?
null            :: [_] -> Bool
null []         = True
null (_:_)      = False

--- Concatenates two lists.
--- Since it is flexible, it could be also used to split a list
--- into two sublists etc.
(++)            :: [a] -> [a] -> [a]
[]     ++ ys    = ys
(x:xs) ++ ys    = x : xs++ys

--- Computes the length of a list.
length          :: [_] -> Int
length []       = 0
length (_:xs)   = 1 + length xs

--- List index (subscript) operator, head has index 0.
(!!)            :: [a] -> Int -> a
(x:xs) !! n | n==0      = x
            | n>0       = xs !! (n-1)

--- Map a function on all elements of a list.
map             :: (a->b) -> [a] -> [b]
map _ []        = []
map f (x:xs)    = f x : map f xs

--- Accumulates all list elements by applying a binary operator from
--- left to right. Thus,
--- <CODE>foldl f z [x1,x2,...,xn] = (...((z `f` x1) `f` x2) ...) `f` xn</CODE>
foldl            :: (a -> b -> a) -> a -> [b] -> a
foldl _ z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

--- Accumulates a non-empty list from left to right.
foldl1           :: (a -> a -> a) -> [a] -> a
foldl1 f (x:xs)  = foldl f x xs

--- Accumulates all list elements by applying a binary operator from
--- right to left. Thus,
--- <CODE>foldr f z [x1,x2,...,xn] = (x1 `f` (x2 `f` ... (xn `f` z)...))</CODE>
foldr            :: (a->b->b) -> b -> [a] -> b
foldr _ z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

--- Accumulates a non-empty list from right to left:
foldr1              :: (a -> a -> a) -> [a] -> a
foldr1 _ [x]        = x
foldr1 f (x1:x2:xs) = f x1 (foldr1 f (x2:xs))

--- Filters all elements satisfying a given predicate in a list.
filter            :: (a -> Bool) -> [a] -> [a]
filter _ []       = []
filter p (x:xs)   = if p x then x : filter p xs
                           else filter p xs

--- Joins two lists into one list of pairs. If one input list is shorter than
--- the other, the additional elements of the longer list are discarded.
zip               :: [a] -> [b] -> [(a,b)]
zip []     _      = []
zip (_:_)  []     = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

--- Joins three lists into one list of triples. If one input list is shorter
--- than the other, the additional elements of the longer lists are discarded.
zip3                      :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3 []     _      _      = []
zip3 (_:_)  []     _      = []
zip3 (_:_)  (_:_)  []     = []
zip3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3 xs ys zs

--- Joins two lists into one list by applying a combination function to
--- corresponding pairs of elements. Thus <CODE>zip = zipWith (,)</CODE>
zipWith                 :: (a->b->c) -> [a] -> [b] -> [c]
zipWith _ []     _      = []
zipWith _ (_:_)  []     = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

--- Joins three lists into one list by applying a combination function to
--- corresponding triples of elements. Thus <CODE>zip3 = zipWith3 (,,)</CODE>
zipWith3                        :: (a->b->c->d) -> [a] -> [b] -> [c] -> [d]
zipWith3 _ []     _      _      = []
zipWith3 _ (_:_)  []     _      = []
zipWith3 _ (_:_)  (_:_)  []     = []
zipWith3 f (x:xs) (y:ys) (z:zs) = f x y z : zipWith3 f xs ys zs

--- Transforms a list of pairs into a pair of lists.
unzip               :: [(a,b)] -> ([a],[b])
unzip []            = ([],[])
unzip ((x,y):ps)    = (x:xs,y:ys) where (xs,ys) = unzip ps

--- Transforms a list of triples into a triple of lists.
unzip3              :: [(a,b,c)] -> ([a],[b],[c])
unzip3 []           = ([],[],[])
unzip3 ((x,y,z):ts) = (x:xs,y:ys,z:zs) where (xs,ys,zs) = unzip3 ts

--- Concatenates a list of lists into one list.
concat            :: [[a]] -> [a]
concat l          = foldr (++) [] l

--- Maps a function from elements to lists and merges the result into one list.
concatMap         :: (a -> [b]) -> [a] -> [b]
concatMap f       = concat . map f

--- Infinite list of repeated applications of a function f to an element x.
--- Thus, <CODE>iterate f x = [x, f x, f (f x),...]</CODE>
iterate           :: (a -> a) -> a -> [a]
iterate f x       = x : iterate f (f x)

--- Infinite list where all elements have the same value.
--- Thus, <CODE>repeat x = [x, x, x,...]</CODE>
repeat            :: a -> [a]
repeat x          = x : repeat x

--- List of length n where all elements have the same value.
replicate         :: Int -> a -> [a]
replicate n x     = take n (repeat x)

--- Returns prefix of length n.
take              :: Int -> [a] -> [a]
take (Neg _)   _      = []
take Zero      _      = []
take (Pos _)   []     = []
take (Pos n) (x:xs) = x : take (Pos n-1) xs

--- Returns suffix without first n elements.
drop              :: Int -> [a] -> [a]
drop n l          = if n<=0 then l else dropp n l
   where dropp _ []     = []
         dropp m (_:xs) = drop (m-1) xs

--- (splitAt n xs) is equivalent to (take n xs, drop n xs)
splitAt           :: Int -> [a] -> ([a],[a])
splitAt n l       = if n<=0 then ([],l) else splitAtp n l
   where splitAtp _ []     = ([],[])
         splitAtp m (x:xs) = let (ys,zs) = splitAt (m-1) xs in (x:ys,zs)

--- Returns longest prefix with elements satisfying a predicate.
takeWhile          :: (a -> Bool) -> [a] -> [a]
takeWhile _ []     = []
takeWhile p (x:xs) = if p x then x : takeWhile p xs else []

--- Returns suffix without takeWhile prefix.
dropWhile          :: (a -> Bool) -> [a] -> [a]
dropWhile _ []     = []
dropWhile p (x:xs) = if p x then dropWhile p xs else x:xs

--- (span p xs) is equivalent to (takeWhile p xs, dropWhile p xs)
span               :: (a -> Bool) -> [a] -> ([a],[a])
span _ []          = ([],[])
span p (x:xs)
       | p x       = let (ys,zs) = span p xs in (x:ys, zs)
       | otherwise = ([],x:xs)

--- (break p xs) is equivalent to (takeWhile (not.p) xs, dropWhile (not.p) xs).
--- Thus, it breaks a list at the first occurrence of an element satisfying p.
break              :: (a -> Bool) -> [a] -> ([a],[a])
break p            = span (not . p)

--- Breaks a string into a list of lines where a line is terminated at a
--- newline character. The resulting lines do not contain newline characters.
lines        :: String -> [String]
lines []     = []
lines (x:xs) = let (l,xs_l) = splitline (x:xs) in l : lines xs_l
 where splitline []     = ([],[])
       splitline (c:cs) = if c=='\n'
                          then ([],cs)
                          else let (ds,es) = splitline cs in (c:ds,es)

--- Concatenates a list of strings with terminating newlines.
unlines    :: [String] -> String
unlines ls = concatMap (++"\n") ls

--- Breaks a string into a list of words where the words are delimited by
--- white spaces.
words      :: String -> [String]
words s    = let s1 = dropWhile isSpace s
              in if s1=="" then []
                           else let (w,s2) = break isSpace s1
                                 in w : words s2
 where
   isSpace c = c == ' '  || c == '\t' || c == '\n' || c == '\r'

--- Concatenates a list of strings with a blank between two strings.
unwords    :: [String] -> String
unwords ws = if ws==[] then []
                       else foldr1 (\w s -> w ++ ' ':s) ws

--- Reverses the order of all elements in a list.
reverse    :: [a] -> [a]
reverse    = foldl (flip (:)) []

--- Computes the conjunction of a Boolean list.
and        :: [Bool] -> Bool
and        = foldr (&&) True

--- Computes the disjunction of a Boolean list.
or         :: [Bool] -> Bool
or         = foldr (||) False

--- Is there an element in a list satisfying a given predicate?
any        :: (a -> Bool) -> [a] -> Bool
any p      = or . map p

--- Is a given predicate satisfied by all elements in a list?
all        :: (a -> Bool) -> [a] -> Bool
all p      = and . map p

--- Element of a list?
elem       :: a -> [a] -> Bool
elem x     = any (x==)

--- Not element of a list?
notElem    :: a -> [a] -> Bool
notElem x  = all (x/=)

--- Looks up a key in an association list.
lookup            :: a -> [(a,b)] -> Maybe b
lookup _ []       = Nothing
lookup k ((x,y):xys)
      | k==x      = Just y
      | otherwise = lookup k xys

--- Generates an infinite sequence of ascending integers.
enumFrom               :: Int -> [Int]                   -- [n..]
enumFrom n             = n : enumFrom (n+1)

--- Generates an infinite sequence of integers with a particular in/decrement.
enumFromThen           :: Int -> Int -> [Int]            -- [n1,n2..]
enumFromThen n1 n2     = iterate ((n2-n1)+) n1

--- Generates a sequence of ascending integers.
enumFromTo             :: Int -> Int -> [Int]            -- [n..m]
enumFromTo n m         = if n>m then [] else n : enumFromTo (n+1) m

--- Generates a sequence of integers with a particular in/decrement.
enumFromThenTo         :: Int -> Int -> Int -> [Int]     -- [n1,n2..m]
enumFromThenTo n1 n2 m = takeWhile p (enumFromThen n1 n2)
                         where p x | n2 >= n1  = (x <= m)
                                   | otherwise = (x >= m)


--- Converts a character into its ASCII value.
ord :: Char -> Int
ord c = prim_ord $## c

prim_ord :: Char -> Int
prim_ord external

--- Converts an ASCII value into a character.
chr :: Int -> Char
chr i = prim_chr $## i

prim_chr :: Int -> Char
prim_chr external

-- Natural numbers

data Nat = IHi | O Nat | I Nat

succ :: Nat -> Nat
succ (O bs) = I bs
succ (I bs) = O (succ bs)
succ IHi = O IHi

(+^) :: Nat -> Nat -> Nat
O x +^ O y = O (x +^ y)
O x +^ I y = I (x +^ y)
O x +^ IHi = I x
I x +^ O y = I (x +^ y)
I x +^ I y = O (succ x +^ y)
I x +^ IHi = O (succ x)
IHi +^ y   = succ y

cmpNat :: Nat -> Nat -> Ordering
cmpNat IHi IHi   = EQ
cmpNat IHi (O _) = LT
cmpNat IHi (I _) = LT
cmpNat (O _) IHi = GT
cmpNat (I _) IHi = GT
cmpNat (O x) (O y) = cmpNat x y
cmpNat (I x) (I y) = cmpNat x y
cmpNat (O x) (I y) = cmpNatLT x y
cmpNat (I x) (O y) = cmpNatGT x y

cmpNatLT :: Nat -> Nat -> Ordering
cmpNatLT IHi _     = LT
cmpNatLT (O _) IHi = GT
cmpNatLT (I _) IHi = GT
cmpNatLT (O x) (O y) = cmpNatLT x y
cmpNatLT (I x) (I y) = cmpNatLT x y
cmpNatLT (O x) (I y) = cmpNatLT x y
cmpNatLT (I x) (O y) = cmpNatGT x y

cmpNatGT :: Nat -> Nat -> Ordering
cmpNatGT _   IHi   = GT
cmpNatGT IHi (O _) = LT
cmpNatGT IHi (I _) = LT
cmpNatGT (O x) (O y) = cmpNatGT x y
cmpNatGT (I x) (I y) = cmpNatGT x y
cmpNatGT (O x) (I y) = cmpNatLT x y
cmpNatGT (I x) (O y) = cmpNatGT x y

(<^), (>^), (<=^), (>=^) :: Nat -> Nat -> Bool

x <^  y = isLT (cmpNat x y)
x >^  y = isGT (cmpNat x y)
x <=^ y = not (isGT (cmpNat x y))
x >=^ y = not (isLT (cmpNat x y))

(*^) :: Nat -> Nat -> Nat
IHi *^ y = y
I x *^ y = O (y *^ x) +^ y
O x *^ y = O (x *^ y)

pred :: Nat -> Nat
pred (O IHi)     = IHi
pred (O x@(O _)) = I (pred x)
pred (O (I x))   = I (O x) 
pred (I x)       = O x

-- Integers

data Int = Neg Nat | Zero | Pos Nat

-- basic operations (+1), (-1), (*2)

inc, dec, mult2 :: Int -> Int

inc Zero = Pos IHi
inc (Pos n) = Pos (succ n)
inc (Neg IHi) = Zero
inc (Neg (O n)) = Neg (pred (O n))
inc (Neg (I n)) = Neg (O n)

dec Zero = Neg IHi
dec (Neg n) = Neg (succ n)
dec (Pos IHi) = Zero
dec (Pos (O n)) = Pos (pred (O n))
dec (Pos (I n)) = Pos (O n)

mult2 (Pos n) = Pos (O n)
mult2 Zero    = Zero
mult2 (Neg n) = Neg (O n)

(-^) :: Nat -> Nat -> Int
IHi -^ y   = inc (Neg y)             -- 1-n = 1+(-n)
O x -^ IHi = Pos (pred (O x))    
O x -^ O y = mult2 (x -^ y)
O x -^ I y = dec (mult2 (x -^ y))
I x -^ IHi = Pos (O x)
I x -^ O y = inc (mult2 (x -^ y))    -- 2*n+1 - 2*m = 1+2*(n-m)
I x -^ I y = mult2 (x -^ y)          -- 2*n+1 - (2*m+1) = 2*(n-m)

div2 :: Nat -> Nat
div2 (O x) = x
div2 (I x) = x

mod2 :: Nat -> Int
mod2 IHi   = Pos IHi
mod2 (O _) = Zero
mod2 (I _) = Pos IHi
 
divmodNat :: Nat -> Nat -> (Int,Int)
divmodNat x y 
  | y==IHi    = (Pos x,Zero) 
  | otherwise = case cmpNat x y of
    EQ -> (Pos IHi,Zero)
    LT -> (Zero, Pos x)
    GT -> case divmodNat (div2 x) y of
      (Zero,_)      -> (Pos IHi,x -^ y)
      (Pos d,Zero)  -> (Pos (O d),mod2 x)
      (Pos d,Pos m) -> case divmodNat (shift x m) y of
        (Zero,m')   -> (Pos (O d),m')
        (Pos d',m') -> (Pos (O d +^ d'),m')
  where
    shift (O _) n = O n
    shift (I _) n = I n


--- Adds two integers.
(+)   :: Int -> Int -> Int
Pos x + Pos y = Pos (x +^ y)
Neg x + Neg y = Neg (x +^ y)
Pos x + Neg y = x -^ y
Neg x + Pos y = y -^ x
Zero  + x     = x
x@(Pos _) + Zero = x
x@(Neg _) + Zero = x

--- Subtracts two integers.
(-)   :: Int -> Int -> Int
x - Neg y = x + Pos y
x - Pos y = x + Neg y
x - Zero  = x

--- Multiplies two integers.
(*)   :: Int -> Int -> Int
Pos x * Pos y = Pos (x *^ y)
Pos x * Neg y = Neg (x *^ y)
Neg x * Neg y = Pos (x *^ y)
Neg x * Pos y = Neg (x *^ y)
Zero  * _     = Zero
Pos _ * Zero  = Zero
Neg _ * Zero  = Zero

--- Integer division. The value is the integer quotient of its arguments
--- and always truncated towards zero.
--- Thus, the value of <code>13 `div` 5</code> is <code>2</code>,
--- and the value of <code>-15 `div` 4</code> is <code>-3</code>.
--- Integer remainder. The value is the remainder of the integer division and
--- it obeys the rule <code>x `mod` y = x - y * (x `div` y)</code>.
--- Thus, the value of <code>13 `mod` 5</code> is <code>3</code>,
--- and the value of <code>-15 `mod` 4</code> is <code>-3</code>.

divmod   :: Int -> Int -> (Int,Int)
divmod Zero    _       = (Zero,Zero)
divmod (Pos _) Zero    = error "division by 0"
divmod (Pos x) (Pos y) = divmodNat x y
divmod (Pos x) (Neg y) = let (d,m) = divmodNat x y in (negate d,m)
divmod (Neg _) Zero    = error "division by 0"
divmod (Neg x) (Pos y) = let (d,m) = divmodNat x y in (negate d,negate m)
divmod (Neg x) (Neg y) = let (d,m) = divmodNat x y in (d,negate m)

div,mod :: Int -> Int -> Int
x `div` y = fst (divmod x y)

x `mod` y = snd (divmod x y)

--- Unary minus. Usually written as "- e".
negate :: Int -> Int
negate Zero    = Zero
negate (Pos x) = Neg x
negate (Neg x) = Pos x

--- Unary minus on Floats. Usually written as "-e".
negateFloat :: Float -> Float
negateFloat x = prim_negateFloat $# x

prim_negateFloat :: Float -> Float
prim_negateFloat external

-- Constraints
data Success = Success

--- The always satisfiable constraint.
success :: Success
success = Success

--- The equational constraint.
--- (e1 =:= e2) is satisfiable if both sides e1 and e2 can be
--- reduced to a unifiable data term (i.e., a term without defined
--- function symbols).
(=:=)   :: a -> a -> Success
x =:= y | x===y = success

(===)   :: a -> a -> Bool
(===) external


--- Concurrent conjunction on constraints.
--- An expression like (c1 & c2) is evaluated by evaluating
--- the constraints c1 and c2 in a concurrent manner.
(&)     :: Success -> Success -> Success
(&) external

--- Constrained expression.
--- An expression like (c &> e) is evaluated by first solving
--- constraint c and then evaluating e.
(&>)          :: Success -> a -> a
c &> x | c = x

--andBreadth :: [Bool] -> Bool
--andBreadth external

-- Maybe type

data Maybe a = Nothing | Just a 

maybe              :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing  = n
maybe _ f (Just x) = f x


-- Either type

data Either a b = Left a | Right b

either               :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left x)  = f x
either _ g (Right x) = g x


-- Monadic IO

data IO _  -- conceptually: World -> (a,World)

--- Sequential composition of actions.
--- @param a - An action
--- @param fa - A function from a value into an action
--- @return An action that first performs a (yielding result r)
---         and then performs (fa r)
(>>=)             :: IO a -> (a -> IO b) -> IO b
(>>=) external

--- The empty action that directly returns its argument.
return            :: a -> IO a
return external

--- Sequential composition of actions.
--- @param a1 - An action
--- @param a2 - An action
--- @return An action that first performs a1 and then a2
(>>)              :: IO _ -> IO b        -> IO b
a >> b            = a >>= const b

--- The empty action that returns nothing.
done              :: IO ()
done              = return ()

--- An action that puts its character argument on standard output.
putChar           :: Char -> IO ()
putChar c = prim_putChar $## c

prim_putChar           :: Char -> IO ()
prim_putChar external

--- An action that reads a character from standard output and returns it.
getChar           :: IO Char
getChar external

--- An action that (lazily) reads a file and returns its contents.
readFile :: String -> IO String
readFile s = prim_readFile $## s

prim_readFile          :: String -> IO String
prim_readFile external

--- An action that writes a file.
--- @param filename - The name of the file to be written.
--- @param contents - The contents to be written to the file.
writeFile         :: String -> String -> IO ()
writeFile fn s = (prim_writeFile $## fn) $## s

prim_writeFile         :: String -> String -> IO ()
prim_writeFile external

--- An action that appends a string to a file.
--- It behaves like writeFile if the file does not exist.
--- @param filename - The name of the file to be written.
--- @param contents - The contents to be appended to the file.
appendFile        :: String -> String -> IO ()
appendFile fn s = (prim_appendFile  $## fn) $## s

prim_appendFile         :: String -> String -> IO ()
prim_appendFile external

--- Catches a possible failure during the execution of an I/O action.
--- <CODE>(catchFail act err)</CODE>:
--- apply action <CODE>act</CODE> and, if it fails,
--- apply action <CODE>err</CODE>
catchFail         :: IO a -> IO a -> IO a
catchFail external

--- Action to print a string on stdout.
putStr            :: String -> IO ()
putStr []         = done
putStr (c:cs)     = putChar c >> putStr cs
 
--- Action to print a string with a newline on stdout.
putStrLn          :: String -> IO ()
putStrLn cs       = putStr cs >> putChar '\n'

--- Action to read a line from stdin.
getLine           :: IO String
getLine           = do c <- getChar
                       if c=='\n' then return []
                                  else do cs <- getLine
                                          return (c:cs)

--- Converts an arbitrary term into an external string representation.
show    :: _ -> String
show s = prim_show $## s

prim_show    :: _ -> String
prim_show external

--- Converts a term into a string and prints it.
print   :: _ -> IO ()
print t = putStrLn (show t)

--- Solves a constraint as an I/O action.
--- Note: the constraint should be always solvable in a deterministic way
doSolve :: Success -> IO ()
doSolve constraint | constraint = done


-- IO monad auxiliary functions:

--- Executes a sequence of I/O actions and collects all results in a list.
sequenceIO       :: [IO a] -> IO [a]
sequenceIO []     = return []
sequenceIO (c:cs) = do x  <- c
                       xs <- sequenceIO cs
                       return (x:xs)

--- Executes a sequence of I/O actions and ignores the results.
sequenceIO_        :: [IO _] -> IO ()
sequenceIO_         = foldr (>>) done

--- Maps an I/O action function on a list of elements.
--- The results of all I/O actions are collected in a list.
mapIO             :: (a -> IO b) -> [a] -> IO [b]
mapIO f            = sequenceIO . map f

--- Maps an I/O action function on a list of elements.
--- The results of all I/O actions are ignored.
mapIO_            :: (a -> IO _) -> [a] -> IO ()
mapIO_ f           = sequenceIO_ . map f


----------------------------------------------------------------
-- Non-determinism:

--- Non-deterministic choice <EM>par excellence</EM>.
--- The value of <EM>x ? y</EM> is either <EM>x</EM> or <EM>y</EM>.
--- @param x - The right argument.
--- @param y - The left argument.
--- @return either <EM>x</EM> or <EM>y</EM> non-deterministically.
(?)   :: a -> a -> a
x ? _ = x
_ ? y = y

----------------------------------------------------------------
-- Encapsulated search:
----------------------------------------------------------------

--- Search trees represent the search space of evaluating a given 
--- term. For example, the search tree corresponding to 
--- the evaluation of
--- (0?1) + (0?1) 
--- is Or [Or [Value 0,Value 1],Or [Value 1,Value 2]]
--- whereas the one corresponding to 
--- let x=0?1 in x+x 
--- is Or [Value 0,Value 2]. 
data SearchTree a 
  = Fail
  | Value a 
  | Choice [SearchTree a] 
  | Suspend 
 
--- Basic search control operator, providing the searchtree lazily with 
--- respect to Or branches. The argument of the Value constructor is 
--- always evaluated to full normal form. This guarantees that it is 
--- really a value and does neither induce another fail nor a 
--- branching.
getSearchTree :: a -> IO (SearchTree a)
getSearchTree external

{-
--- Basic operation for generic programming
--- type Data represents arbitrary data types
--- with the strings containing the names of 
--- constructors

data Data = Data String [Data]

--- toData residuates on free variables
--- example: toData (Just False) = Data "Just" [Data "False"]

toData :: a -> Data
toData external

--- E.g. fromData (Data unknown "Nothing") = (Data 1 unknown) = Nothing 
--- (modulo variable instanciation)
fromData :: Data -> a
fromData external

--- num data might make 
data NumData = NumData Int [NumData]

--- toNumData residuates on free variables
toNumData :: a -> NumData
toNumData external

--- toData/fromData are enough for basic generics like 
--- show, read, compare and (==).
--- See compare above for a simple example.
--- Other operations can be found in Module Generic
-}
 
--- depth first search
allValuesD :: SearchTree a -> [a]
allValuesD (Value x) = [x]
allValuesD Fail      = []
allValuesD Suspend   = []
allValuesD (Choice xs)   = concatMap allValuesD xs

--- breadth first search
allValuesB :: SearchTree a -> [a]
allValuesB st = unfoldOrs [st]
  where
    partition (Value x) y = let (vs,ors) = y in (x:vs,ors)
    partition (Choice xs)   y = let (vs,ors) = y in (vs,xs++ors)
    partition Fail      y = y
    partition Suspend   y = y

    unfoldOrs [] = []
    unfoldOrs (x:xs) = let (vals,ors) = foldr partition ([],[]) (x:xs) in
      vals ++ unfoldOrs ors

--- Inject operator which adds the application of the unary
--- procedure p to the search variable to the search goal
--- taken from Oz. p x comes before g x to enable a test+generate
--- form in a sequential implementation.
inject  :: (a->Success) -> (a->Success) -> (a->Success) 
inject g p = \x -> p x & g x

--- Identity function used by the partial evaluator
--- to mark expressions to be partially evaluated.
PEVAL   :: a -> a
PEVAL x = x

-- Only for internal use:
-- Represenation of higher-order applications in FlatCurry.
apply :: (a->b) -> a -> b
apply external


-- Only for internal use:
-- Representation of conditional rules in FlatCurry.
cond :: Success -> a -> a
cond external

unknown :: a
unknown = let x free in x

-- the end


--- Non-strict equational constraint. Experimental.
(=:<=) :: a -> a -> Success
(=:<=) external
