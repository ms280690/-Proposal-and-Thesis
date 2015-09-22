module Curry.RunTimeSystem.BaseCurry (
  module Curry.RunTimeSystem.BaseCurry, 
  module Curry.RunTimeSystem.Store) where

import Curry.RunTimeSystem.Store
import Data.IORef
import System.IO.Unsafe

--trace' s x = unsafePerformIO (putStrLn s >> return x) 

-- On the top level io monad of each program we manage a store.
-- Because there is unsafe io and because some operations on
-- stores start out without one, a state might be without store.
type State = Store

-- curry data terms are classified into ConsKinds
data ConsKind = Val | Branching | Failed deriving (Show,Eq)

-- computations of (head) normal forms might residuate or not.
type HNFMode = Bool

type Branches a = [a]

data Exception 
  = ErrorCall String
  | PatternMatchFail String
  | AssertionFailed String
  | PreludeFailed
  | IOException String deriving Eq

type C_Exceptions = Exception

type Result  a = State -> a
type Result' a = Store -> a

----------------------------------------------------------------
-- the BaseCurry class
----------------------------------------------------------------

class BaseCurry a where
  -- computations of normal forms
  nf   :: BaseCurry b => (a -> Result b) -> a -> Result b
  gnf  :: BaseCurry b => (a -> Result b) -> a -> Result b

  -- constructors
  generator :: Int -> a
  failed    :: C_Exceptions          -> a
  branching :: OrRef   -> Branches a -> a

  -- category of given constructor
  consKind :: a -> ConsKind

  -- selectors
  exceptions :: a -> C_Exceptions
  orRef      :: a -> OrRef
  branches   :: a -> Branches a

------------------------------------------------------------------
-- implementation of call-time choice
------------------------------------------------------------------

-- This function controls all kinds of evaluations to (head) normal forms
-- IMPORTANT: if you change anything here, also update ExternalPrelude.prim_do
ctcStore :: (BaseCurry a,BaseCurry b) => HNFMode -> (a -> Result b) -> a -> Result b
ctcStore mode cont x state = 
  case consKind x of
   Val       -> cont x state
   Failed    -> addException err x
   Branching -> let ref = orRef x 
                    bs  = branches x 
                 in manipulateStore
                      (failed (curryError "ctcStore"))
                      contCTC
                      (\ ref' contSt -> if   mode || not (isGenerator ref)
                                        then lift contCTC (narrowOrRef ref) bs contSt
                                        else cont (branching ref' bs) state)
                      
                      ( \ ref' x' state' -> branching ref' [contCTC x' state'])
                      ref bs state                      
  where
    contCTC = ctcStore mode cont
    err = curryError ("Prelude."++if mode then "$#" else "$!")

mapOr :: BaseCurry b => (a -> Result b) -> OrRef -> Branches a -> Result b
mapOr cont ref bs = manipulateStore
    (failed (curryError "mapOr"))
    cont
    (\ _ -> lift cont (narrowOrRef ref) bs)
    (\ ref x st -> branching ref [cont x st])
    ref bs

lift :: BaseCurry b => (a -> Result b) -> OrRef -> Branches a 
                    -> (Int -> State)  -> b
lift cont ref bs contSt = 
  branching ref (zipWith (\ x i -> cont x (contSt i)) bs [0..])

addException :: (BaseCurry a,BaseCurry b) => Exception -> a -> b
addException _ x = failed (exceptions x)

curryError :: String -> Exception
curryError = ErrorCall 


