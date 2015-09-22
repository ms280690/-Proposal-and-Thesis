module Meta 
  (isFree, 

   headNormalFormIO, hnfIO,nfIO,ghnfIO,gnfIO,

   searchTree, 
   RichSearchTree(..), --Suspension,
   getRichSearchTree, richSearchTree, 
   st, richST,

   cover, ors, OrRef,

   parallelSearch,

   Exception(..), throw,

   allValuesI

  ) where


--- Evaluate the given term to head normal form and
--- return this head normal form as either 
--- a) Left  if the hnf is a free variable
--- b) Right if the hnf is a value
isFree :: a -> IO (Either a a)
isFree x = headNormalFormIO prim_isFree x

prim_isFree :: a -> IO (Either a a)
prim_isFree external

--- Evaluate the given term to head normal form and
--- call the given continuation with this head 
--- normal form.
headNormalFormIO :: (a -> IO b) -> a -> IO b
headNormalFormIO external

--- The declarative form of encapsulated search:
--- It is both strong and referentially transparent
--- in the sense that the result only depends on the
--- given value and not on the state of bindings
--- or non-deterministic branchings of the remaining 
--- computation.
searchTree :: a -> SearchTree a
searchTree external

hnfIO, nfIO, gnfIO, ghnfIO :: a -> IO a
gnfIO  external
ghnfIO external
nfIO   external
hnfIO  external


--- A SearchTree with more information than the Prelude one.

data RichSearchTree a 
  = RichFail Exception
  | RichValue a 
  | RichChoice OrRef [RichSearchTree a] 
  | RichSuspend 

data Exception 
  = ErrorCall String
  | PatternMatchFail String
  | AssertionFailed String
  | IOException String
  | PreludeFailed

data OrRef 


--- external representation of the non-determinism context
--- of a value 
--data Context 
--type Suspension _ = () --not yet implemented, might be Context -> RichSearchTree a

getRichSearchTree :: a -> IO (RichSearchTree a)
getRichSearchTree external

richSearchTree :: a -> RichSearchTree a
richSearchTree external

-- parallel search
parallelSearch :: a -> IO [a]
parallelSearch external

--- cover safes an expression from being drawn into a search tree
cover :: a -> a
cover external

--- an encapsulation to head normal form only
st :: a -> SearchTree a
st external

--- an encapsulation to head normal form only
richST :: a -> RichSearchTree a
richST external

--- a question mark on lists
ors :: [a] -> a
ors external



-- Exception handling

throw :: Exception -> a
throw e = prim_throw $## e

prim_throw :: Exception -> a
prim_throw external


-- Interleaved search

data Seq a = Nil | Cons a (Seq a) | Continued (Seq a)

list :: Seq a -> [a]
list Nil = []
list (Cons x xs) = x : list xs
list (Continued xs) = list xs

interleave :: Seq a -> Seq a -> Seq a
interleave Nil              ys             = Continued ys
interleave (Cons x xs)      ys             = Cons x (interleave ys xs)
interleave xs@(Continued _) Nil            = xs
interleave (Continued xs)   (Cons x ys)    = Cons x (interleave xs ys)
interleave (Continued xs)   (Continued ys) = Continued (interleave xs ys)

seq :: SearchTree a -> Seq a
seq Fail        = Nil
seq (Value x)   = Cons x Nil
seq (Choice ts) = foldr1 interleave (Nil : map seq (filter isValOrChoice ts))
seq Suspend     = Nil

isValOrChoice :: SearchTree _ -> Bool
isValOrChoice Fail       = False
isValOrChoice (Value _)  = True
isValOrChoice (Choice _) = True
isValOrChoice Suspend    = False

allValuesI :: SearchTree a -> [a]
allValuesI = list . seq
